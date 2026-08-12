#!/usr/bin/env python3
"""Fixture tests for revision-v2 orchestration safety contracts."""

from __future__ import annotations

import argparse
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import tempfile
import time
from types import SimpleNamespace
import unittest
from unittest import mock


MODULE_PATH = Path(__file__).with_name("orchestrate_revision_jobs.py")
SPEC = importlib.util.spec_from_file_location("revision_orchestrator", MODULE_PATH)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError(f"Cannot import {MODULE_PATH}")
ORCHESTRATOR = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(ORCHESTRATOR)


def write_json(path: Path, value: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, indent=2) + "\n", encoding="utf-8")


def file_hash(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


class OrchestratorFixtureTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory()
        self.repo_root = Path(self.temporary.name)
        self.registry = ORCHESTRATOR.Registry(
            MODULE_PATH.with_name("revision_jobs.json"),
            repo_root=self.repo_root,
        )
        self.job_id = "integrate_manuscript"

    def tearDown(self) -> None:
        self.temporary.cleanup()

    def prospective(self) -> dict:
        job = self.registry.get(self.job_id)
        return {
            "status": "planned",
            "dry_run": True,
            "job_id": self.job_id,
            "config_hash": self.registry.config_hash(self.job_id),
            "command": self.registry.command(self.job_id),
            "parameters": {"configured": job["parameters"]},
            "seed": {"origin": 20260715, "rule": "fixture"},
            "required_inputs": {"upstream": "fixture"},
            "expected_outputs": [
                "checkpoint_results.csv",
                "results.csv",
                "run_manifest.json",
                "_SUCCESS.json",
            ],
            "software_versions": {"R": "fixture"},
            "expected_counts": {"expected_units": 1},
        }

    def write_valid_completion(self) -> None:
        output_dir = self.registry.output_dir(self.job_id)
        write_json(
            self.registry.artifact(self.job_id, "dry_run_manifest"),
            {
                "status": "planned",
                "dry_run": True,
                "expected_counts": {"expected_units": 1},
            },
        )
        write_json(
            self.registry.artifact(self.job_id, "prospective_manifest"),
            self.prospective(),
        )
        csv_text = "unit_key,status,stage\nintegrate,ok,integration\n"
        checkpoint = self.registry.artifact(self.job_id, "checkpoint_file")
        results = output_dir / "results.csv"
        checkpoint.write_text(csv_text, encoding="utf-8")
        results.write_text(csv_text, encoding="utf-8")
        run_manifest = self.registry.artifact(self.job_id, "run_manifest")
        write_json(
            run_manifest,
            {
                "job_id": self.job_id,
                "config_hash": self.registry.config_hash(self.job_id),
                "command": self.registry.command(self.job_id),
                "status": "complete",
                "workers": self.registry.get(self.job_id)["workers"],
                "job_timeout_seconds": self.registry.get(self.job_id)["contract"][
                    "job_timeout_seconds"
                ],
                "dry_run": False,
                "software_versions": {"R": "fixture"},
            },
        )
        hashes = {
            "checkpoint_results.csv": file_hash(checkpoint),
            "results.csv": file_hash(results),
            "run_manifest.json": file_hash(run_manifest),
        }
        write_json(
            self.registry.artifact(self.job_id, "success_marker"),
            {
                "job_id": self.job_id,
                "config_hash": self.registry.config_hash(self.job_id),
                "expected_units": 1,
                "observed_unique_units": 1,
                "prospective_manifest_valid": True,
                "run_manifest_valid": True,
                "output_hashes": hashes,
            },
        )

    def status(self) -> dict:
        processes = {job_id: [] for job_id in self.registry.jobs}
        return ORCHESTRATOR.job_status(
            self.registry,
            self.job_id,
            sessions=set(),
            processes=processes,
        )

    def test_prospective_requires_planning_evidence(self) -> None:
        prospective = self.prospective()
        prospective["status"] = "running"
        prospective["dry_run"] = False
        prospective["parameters"] = {}
        prospective["seed"] = {}
        prospective["required_inputs"] = {}
        prospective["expected_outputs"] = []
        prospective["software_versions"] = {}
        write_json(
            self.registry.artifact(self.job_id, "prospective_manifest"),
            prospective,
        )
        errors = ORCHESTRATOR.validate_prospective_manifest(self.registry, self.job_id)
        joined = "\n".join(errors)
        for expected in (
            "prospective status",
            "prospective dry_run",
            "parameters missing or empty",
            "seed missing or empty",
            "required_inputs missing or empty",
            "expected_outputs missing or empty",
            "software_versions missing or empty",
        ):
            self.assertIn(expected, joined)

    def test_complete_requires_real_manifests_counts_and_hashes(self) -> None:
        self.write_valid_completion()
        self.assertEqual(ORCHESTRATOR.validate_success_marker(self.registry, self.job_id), [])
        status = self.status()
        self.assertEqual(status["state"], "COMPLETE")
        self.assertEqual(status["unique_units"], 1)

        marker_path = self.registry.artifact(self.job_id, "success_marker")
        marker = ORCHESTRATOR.read_json(marker_path)
        assert marker is not None
        marker["unexpected"] = "forbidden"
        write_json(marker_path, marker)
        errors = ORCHESTRATOR.validate_success_marker(self.registry, self.job_id)
        self.assertTrue(any("success marker schema mismatch" in error for error in errors))
        marker.pop("unexpected")
        write_json(marker_path, marker)

        run_manifest = self.registry.artifact(self.job_id, "run_manifest")
        value = ORCHESTRATOR.read_json(run_manifest)
        assert value is not None
        value["command"] = ["Rscript", "wrong.R"]
        write_json(run_manifest, value)
        marker = ORCHESTRATOR.read_json(marker_path)
        assert marker is not None
        marker["output_hashes"]["run_manifest.json"] = file_hash(run_manifest)
        write_json(marker_path, marker)
        errors = ORCHESTRATOR.validate_success_marker(self.registry, self.job_id)
        self.assertTrue(any("run manifest command" in error for error in errors))
        self.assertEqual(self.status()["state"], "FAILED")

    def test_duplicate_units_and_failure_marker_prevent_complete(self) -> None:
        self.write_valid_completion()
        checkpoint = self.registry.artifact(self.job_id, "checkpoint_file")
        checkpoint.write_text(
            "unit_key,status,stage\nintegrate,ok,integration\nintegrate,ok,integration\n",
            encoding="utf-8",
        )
        marker_path = self.registry.artifact(self.job_id, "success_marker")
        marker = ORCHESTRATOR.read_json(marker_path)
        assert marker is not None
        marker["output_hashes"]["checkpoint_results.csv"] = file_hash(checkpoint)
        write_json(marker_path, marker)
        errors = ORCHESTRATOR.validate_success_marker(self.registry, self.job_id)
        self.assertTrue(any("duplicate unit_key" in error for error in errors))

        self.write_valid_completion()
        self.registry.artifact(self.job_id, "failure_marker").write_text(
            "failure evidence\n", encoding="utf-8"
        )
        self.assertEqual(self.status()["state"], "FAILED")

    def test_validate_rejects_new_non_manifest_dry_run_artifact(self) -> None:
        args = argparse.Namespace(
            job=self.job_id,
            job_positional=None,
            structural_only=False,
        )

        def fake_run(*_args, **_kwargs):
            output_dir = self.registry.output_dir(self.job_id)
            output_dir.mkdir(parents=True, exist_ok=True)
            (output_dir / "results.csv").write_text("unit_key\nforbidden\n", encoding="utf-8")
            return SimpleNamespace(returncode=0)

        source = self.repo_root / "frame_scores.csv"
        source.write_text("clip,frame_index,anger\na,1,0.5\n", encoding="utf-8")
        with mock.patch.dict(
            os.environ,
            {"REVISION_EMPIRICAL_FRAME_SCORES": str(source)},
        ):
            with mock.patch.object(ORCHESTRATOR.subprocess, "run", side_effect=fake_run):
                with self.assertRaisesRegex(
                    ORCHESTRATOR.OrchestratorError,
                    "dry-run created forbidden artifacts: results.csv",
                ):
                    ORCHESTRATOR.command_validate(self.registry, args)

    def test_validate_requires_external_input_dependency_before_hashing(self) -> None:
        args = argparse.Namespace(
            job="aggregate_results",
            job_positional=None,
            structural_only=False,
        )
        environment_name = "REVISION_EMPIRICAL_FRAME_SCORES"
        with mock.patch.dict(os.environ, {environment_name: ""}):
            with mock.patch.object(ORCHESTRATOR.subprocess, "run") as run:
                with self.assertRaisesRegex(
                    ORCHESTRATOR.OrchestratorError,
                    f"set {environment_name}",
                ):
                    ORCHESTRATOR.command_validate(self.registry, args)
                run.assert_not_called()

            structural_args = argparse.Namespace(
                job="aggregate_results",
                job_positional=None,
                structural_only=True,
            )
            self.assertEqual(
                ORCHESTRATOR.command_validate(self.registry, structural_args),
                0,
            )

    def test_resume_wrapper_appends_to_log(self) -> None:
        wrapper = ORCHESTRATOR.shell_wrapper(self.registry, self.job_id, resume=True)
        self.assertIn("tee -a", wrapper)
        self.assertIn("timeout --signal=TERM --kill-after=60s 7200s", wrapper)
        self.assertIn("REVISION_JOB_RESUME=1", wrapper)

    def test_plan_exposes_resolved_job_contract(self) -> None:
        record = ORCHESTRATOR.plan_record(self.registry, "t25_gimme")
        self.assertEqual(record["parameters"]["design_range_status"], "out_of_range_short_record")
        self.assertEqual(record["job_timeout_seconds"], 604800)
        self.assertEqual(record["per_unit_timeout_seconds"], 900)
        self.assertEqual(record["budget_seconds"], 900)
        self.assertEqual(record["worker_limit"], 24)
        self.assertEqual(record["expected_work"]["expected_units"], 64800)
        self.assertTrue(record["paths"]["checkpoint"].endswith("checkpoint_results.csv"))
        self.assertTrue(record["paths"]["run_log"].endswith("run.log"))
        self.assertIn("t100_reuse_audit", record["required_dependencies"])
        self.assertFalse(record["resume_policy"]["automatic"])
        environment = self.registry.job_environment("t25_gimme", resume=False)
        self.assertEqual(environment["REVISION_JOB_TIMEOUT_SECONDS"], "604800")
        self.assertEqual(environment["REVISION_JOB_PER_UNIT_TIMEOUT_SECONDS"], "900")
        self.assertEqual(environment["REVISION_JOB_BUDGET_SECONDS"], "900")
        self.assertEqual(environment["REVISION_GIMME_TIMEOUT_SEC"], "900")
        self.assertEqual(
            self.registry.get("empirical_cv")["parameters"]["frozen_dynega_ncores"],
            1,
        )
        self.assertEqual(
            self.registry.get("empirical_cv")["parameters"]["outer_parallel_backend"],
            "psock",
        )
        self.assertEqual(
            self.registry.get("rebuild_figures_tables")["parameters"][
                "publication_schema_revision"
            ],
            2,
        )

    def test_empirical_parallel_contract_rejects_internal_multicore(self) -> None:
        manifest = json.loads(
            MODULE_PATH.with_name("revision_jobs.json").read_text(encoding="utf-8")
        )
        empirical = next(job for job in manifest["jobs"] if job["id"] == "empirical_cv")
        empirical["parameters"]["frozen_dynega_ncores"] = 2
        errors = ORCHESTRATOR.validate_manifest(manifest)
        self.assertIn("empirical_cv: frozen_dynega_ncores must equal 1", errors)

    def test_empirical_parallel_contract_rejects_fork_backend(self) -> None:
        manifest = json.loads(
            MODULE_PATH.with_name("revision_jobs.json").read_text(encoding="utf-8")
        )
        empirical = next(job for job in manifest["jobs"] if job["id"] == "empirical_cv")
        empirical["parameters"]["outer_parallel_backend"] = "fork"
        errors = ORCHESTRATOR.validate_manifest(manifest)
        self.assertIn("empirical_cv: outer_parallel_backend must equal psock", errors)

    def test_rebuild_contract_rejects_stale_publication_schema(self) -> None:
        manifest = json.loads(
            MODULE_PATH.with_name("revision_jobs.json").read_text(encoding="utf-8")
        )
        rebuild = next(
            job for job in manifest["jobs"] if job["id"] == "rebuild_figures_tables"
        )
        rebuild["parameters"]["publication_schema_revision"] = 1
        errors = ORCHESTRATOR.validate_manifest(manifest)
        self.assertIn(
            "rebuild_figures_tables: publication_schema_revision must equal 2",
            errors,
        )

    def test_empirical_external_input_is_required_hashed_and_exported(self) -> None:
        environment_name = "REVISION_EMPIRICAL_FRAME_SCORES"
        source = self.repo_root / "frame_scores.csv"
        source.write_text("clip,frame_index,anger\na,1,0.5\n", encoding="utf-8")

        with mock.patch.dict(os.environ, {environment_name: ""}):
            registry = ORCHESTRATOR.Registry(
                MODULE_PATH.with_name("revision_jobs.json"),
                repo_root=self.repo_root,
            )
            with self.assertRaisesRegex(
                ORCHESTRATOR.OrchestratorError,
                f"set {environment_name}",
            ):
                registry.job_environment("empirical_cv", resume=False)
            dry_environment = registry.job_environment(
                "empirical_cv",
                resume=False,
                require_external_inputs=False,
            )
            self.assertNotIn(environment_name, dry_environment)

        with mock.patch.dict(os.environ, {environment_name: str(source)}):
            first_registry = ORCHESTRATOR.Registry(
                MODULE_PATH.with_name("revision_jobs.json"),
                repo_root=self.repo_root,
            )
            first_hash = first_registry.config_hash("empirical_cv")
            environment = first_registry.job_environment("empirical_cv", resume=False)
            self.assertEqual(environment[environment_name], str(source.resolve()))
            self.assertIn(
                f"export {environment_name}={source.resolve()}",
                ORCHESTRATOR.shell_wrapper(first_registry, "empirical_cv", resume=False),
            )
            resolved = ORCHESTRATOR.resolved_required_inputs(
                first_registry,
                "empirical_cv",
            )["resolved_external_artifacts"]
            self.assertEqual(resolved[0]["sha256"], file_hash(source))

            first_registry.persist_external_inputs("empirical_cv")
            with mock.patch.dict(os.environ, {environment_name: ""}):
                receipt_registry = ORCHESTRATOR.Registry(
                    MODULE_PATH.with_name("revision_jobs.json"),
                    repo_root=self.repo_root,
                )
                self.assertEqual(
                    receipt_registry.config_hash("empirical_cv"),
                    first_hash,
                )
                self.assertEqual(
                    receipt_registry.job_environment("empirical_cv", resume=False)[
                        environment_name
                    ],
                    str(source.resolve()),
                )

            source.write_text("clip,frame_index,anger\na,1,0.6\n", encoding="utf-8")
            second_registry = ORCHESTRATOR.Registry(
                MODULE_PATH.with_name("revision_jobs.json"),
                repo_root=self.repo_root,
            )
            self.assertNotEqual(first_hash, second_registry.config_hash("empirical_cv"))

    def test_external_input_hash_namespace_is_stable_through_downstream_jobs(self) -> None:
        environment_name = "REVISION_EMPIRICAL_FRAME_SCORES"
        source = self.repo_root / "frame_scores.csv"
        source.write_text("clip,frame_index,anger\na,1,0.5\n", encoding="utf-8")
        downstream_jobs = (
            "empirical_cv",
            "aggregate_results",
            "rebuild_figures_tables",
            "integrate_manuscript",
        )

        with mock.patch.dict(os.environ, {environment_name: str(source)}):
            validation_registry = ORCHESTRATOR.Registry(
                MODULE_PATH.with_name("revision_jobs.json"),
                repo_root=self.repo_root,
            )
            validation_registry.require_validation_external_inputs(
                ["integrate_manuscript"]
            )
            validation_hashes = {
                job_id: validation_registry.config_hash(job_id)
                for job_id in downstream_jobs
            }
            empirical_job = validation_registry.get("empirical_cv")
            expected_counts = {
                key: value
                for key, value in empirical_job.items()
                if key.startswith("expected_")
            }
            write_json(
                validation_registry.artifact("empirical_cv", "dry_run_manifest"),
                {
                    "status": "planned",
                    "dry_run": True,
                    "expected_counts": expected_counts,
                },
            )
            write_json(
                validation_registry.artifact("empirical_cv", "prospective_manifest"),
                {
                    "status": "planned",
                    "dry_run": True,
                    "job_id": "empirical_cv",
                    "config_hash": validation_registry.config_hash("empirical_cv"),
                    "command": validation_registry.command("empirical_cv"),
                    "parameters": {"configured": empirical_job["parameters"]},
                    "seed": {"origin": 20260715, "rule": "fixture"},
                    "required_inputs": {"frame_scores": str(source)},
                    "expected_outputs": ["checkpoint_results.csv"],
                    "software_versions": {"R": "fixture"},
                    "expected_counts": expected_counts,
                },
            )
            validation_registry.persist_external_inputs("empirical_cv")

        with mock.patch.dict(os.environ, {environment_name: ""}):
            launch_registry = ORCHESTRATOR.Registry(
                MODULE_PATH.with_name("revision_jobs.json"),
                repo_root=self.repo_root,
            )
            launch_hashes = {
                job_id: launch_registry.config_hash(job_id)
                for job_id in downstream_jobs
            }
            processes = {job_id: [] for job_id in launch_registry.jobs}
            empirical_status = ORCHESTRATOR.job_status(
                launch_registry,
                "empirical_cv",
                sessions=set(),
                processes=processes,
            )
            self.assertEqual(empirical_status["state"], "READY")
            ORCHESTRATOR.verify_launch_state(
                launch_registry,
                "empirical_cv",
                resume=False,
                statuses={"empirical_cv": empirical_status},
            )
        self.assertEqual(launch_hashes, validation_hashes)

        source.write_text("clip,frame_index,anger\na,1,0.6\n", encoding="utf-8")
        with mock.patch.dict(os.environ, {environment_name: str(source)}):
            changed_registry = ORCHESTRATOR.Registry(
                MODULE_PATH.with_name("revision_jobs.json"),
                repo_root=self.repo_root,
            )
            changed_status = ORCHESTRATOR.job_status(
                changed_registry,
                "empirical_cv",
                sessions=set(),
                processes={job_id: [] for job_id in changed_registry.jobs},
            )
        self.assertNotEqual(
            changed_status["config_hash"],
            validation_hashes["empirical_cv"],
        )
        self.assertEqual(changed_status["state"], "PLANNED")

    def test_reuse_or_fallback_alternatives_are_exclusive(self) -> None:
        audit_path = self.registry.output_dir("t100_reuse_audit") / "reuse_audit.json"
        write_json(
            audit_path,
            {
                "methods": {
                    "graphicalvar": {"reusable": True, "reason": "verified fixture"},
                    "gimme": {"reusable": False, "reason": "fixture mismatch"},
                }
            },
        )
        states = {
            job_id: {
                "state": "COMPLETE",
                "artifact_issues": [],
                "blocking_dependencies": [],
                "conditional_active": None,
                "conditional_reason": None,
            }
            for job_id in self.registry.jobs
        }
        graph_fallback = "t100_graphicalvar_fallback"
        gimme_fallback = "t100_gimme_fallback"
        states[graph_fallback]["state"] = "READY"
        states[gimme_fallback]["state"] = "READY"
        ORCHESTRATOR.apply_conditional_reuse_states(self.registry, states)
        self.assertFalse(states[graph_fallback]["conditional_active"])
        self.assertEqual(states[graph_fallback]["state"], "BLOCKED")
        self.assertTrue(states[gimme_fallback]["conditional_active"])
        self.assertEqual(states[gimme_fallback]["state"], "READY")

        blockers = ORCHESTRATOR.dependency_blockers(
            self.registry,
            "aggregate_results",
            states,
        )
        self.assertIn("gimme fallback incomplete: t100_gimme_fallback", blockers)
        self.assertFalse(any("graphicalvar fallback" in value for value in blockers))
        states[gimme_fallback]["state"] = "COMPLETE"
        self.assertEqual(
            ORCHESTRATOR.dependency_blockers(
                self.registry,
                "aggregate_results",
                states,
            ),
            [],
        )

    def test_capacity_guards_enforce_one_heavy_job_and_24_workers(self) -> None:
        statuses = {
            job_id: {
                "job_id": job_id,
                "heavy": job["heavy"],
                "workers": job["workers"],
                "session_active": False,
                "process_active": False,
            }
            for job_id, job in self.registry.jobs.items()
        }
        statuses["empirical_cv"]["process_active"] = True
        with self.assertRaisesRegex(
            ORCHESTRATOR.OrchestratorError,
            "maximum one heavy job",
        ):
            ORCHESTRATOR.verify_capacity(
                self.registry,
                "t100_dynega",
                statuses,
                sessions=set(),
            )

        statuses["empirical_cv"]["process_active"] = False
        statuses["t25_graphicalvar"]["process_active"] = True
        with self.assertRaisesRegex(
            ORCHESTRATOR.OrchestratorError,
            r"24 active \+ 1 requested > 24",
        ):
            ORCHESTRATOR.verify_capacity(
                self.registry,
                "t100_reuse_audit",
                statuses,
                sessions=set(),
            )

    def test_eta_needs_two_progressing_snapshots_and_stall_uses_age(self) -> None:
        snapshot = {
            "event": "status",
            "recorded_at": "2026-07-15T12:10:00+00:00",
            "job_id": "empirical_cv",
            "config_hash": "fixture",
            "state": "RUNNING",
            "progress": 20,
            "expected_units": 100,
            "recent_rate_per_hour": None,
            "eta_seconds": None,
            "eta_at": None,
        }
        no_history = ORCHESTRATOR.add_eta(dict(snapshot), [])
        self.assertIsNone(no_history["eta_seconds"])
        history = [
            {
                "event": "status",
                "recorded_at": "2026-07-15T12:00:00+00:00",
                "job_id": "empirical_cv",
                "config_hash": "fixture",
                "progress": 10,
            }
        ]
        with_history = ORCHESTRATOR.add_eta(dict(snapshot), history)
        self.assertEqual(with_history["recent_rate_per_hour"], 60.0)
        self.assertEqual(with_history["eta_seconds"], 4800)

        lock_path = self.registry.lock_path("empirical_cv")
        lock_path.parent.mkdir(parents=True, exist_ok=True)
        lock_path.write_text("{}\n", encoding="utf-8")
        stale_time = time.time() - 1900
        os.utime(lock_path, (stale_time, stale_time))
        processes = {job_id: [] for job_id in self.registry.jobs}
        status = ORCHESTRATOR.job_status(
            self.registry,
            "empirical_cv",
            sessions={self.registry.session_name("empirical_cv")},
            processes=processes,
        )
        self.assertEqual(status["state"], "STALLED")

    def test_manual_integration_gate_refuses_without_receipt(self) -> None:
        states = {
            job_id: {"state": "COMPLETE"}
            for job_id in self.registry.jobs
        }
        states[self.job_id] = {
            "state": "READY",
            "session_active": False,
            "process_active": False,
            "session": self.registry.session_name(self.job_id),
        }
        receipt = ORCHESTRATOR.readiness_receipt_path(self.registry, self.job_id)
        assert receipt is not None
        self.assertFalse(receipt.exists())
        with self.assertRaisesRegex(
            ORCHESTRATOR.OrchestratorError,
            "manual review gate failed: missing or invalid manual readiness receipt",
        ):
            ORCHESTRATOR.verify_launch_state(
                self.registry,
                self.job_id,
                resume=False,
                statuses=states,
            )
        self.assertFalse(receipt.exists())

    def test_manual_receipt_hashes_both_current_dependency_markers(self) -> None:
        dependency_hashes: dict[str, str] = {}
        for dependency in ("aggregate_results", "rebuild_figures_tables"):
            marker = self.registry.artifact(dependency, "success_marker")
            write_json(marker, {"dependency": dependency, "fixture": True})
            dependency_hashes[dependency] = file_hash(marker)
        receipt = ORCHESTRATOR.readiness_receipt_path(self.registry, self.job_id)
        assert receipt is not None
        write_json(
            receipt,
            {
                "job_id": self.job_id,
                "config_hash": self.registry.config_hash(self.job_id),
                "status": "approved",
                "reviewer": "fixture-reviewer",
                "reviewed_at": "2026-07-15T12:00:00+00:00",
                "reviewed_dependency_hashes": dependency_hashes,
            },
        )
        self.assertEqual(
            ORCHESTRATOR.validate_manual_readiness_receipt(self.registry, self.job_id),
            [],
        )

        aggregate_marker = self.registry.artifact("aggregate_results", "success_marker")
        write_json(aggregate_marker, {"dependency": "aggregate_results", "fixture": "changed"})
        errors = ORCHESTRATOR.validate_manual_readiness_receipt(
            self.registry,
            self.job_id,
        )
        self.assertTrue(any("do not match current" in error for error in errors))


if __name__ == "__main__":
    unittest.main()
