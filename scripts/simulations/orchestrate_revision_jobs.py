#!/usr/bin/env python3
"""Safely inspect and explicitly launch the revision-v2 job graph."""

from __future__ import annotations

import argparse
import csv
import datetime as dt
import hashlib
import json
import os
from pathlib import Path
import shlex
import shutil
import subprocess
import time
from typing import Any, Iterable, Sequence


SCRIPT_PATH = Path(__file__).resolve()
REPO_ROOT = SCRIPT_PATH.parents[2]
DEFAULT_CONFIG = SCRIPT_PATH.with_name("revision_jobs.json")
AUTHORIZED_ACK = "LONG-RUNS-AUTHORIZED"
OUTPUT_ROOT = "simulation_outputs/revision_v2"
REQUIRED_JOB_IDS = (
    "empirical_cv",
    "t100_reuse_audit",
    "t100_dynega",
    "t100_static_oracle",
    "t100_graphicalvar_fallback",
    "t100_gimme_fallback",
    "t25_dynega",
    "t25_graphicalvar",
    "t25_gimme",
    "t25_static_oracle",
    "challenge_dynega",
    "challenge_graphicalvar",
    "challenge_static_oracle",
    "aggregate_results",
    "rebuild_figures_tables",
    "integrate_manuscript",
)
EXTERNAL_INPUT_ENVIRONMENTS = {
    "empirical_cv": ("REVISION_EMPIRICAL_FRAME_SCORES",),
}


class OrchestratorError(RuntimeError):
    """Raised for a safe, user-facing orchestration failure."""


def utc_now() -> dt.datetime:
    return dt.datetime.now(dt.timezone.utc)


def isoformat(value: dt.datetime | None = None) -> str:
    return (value or utc_now()).isoformat(timespec="seconds")


def read_json(path: Path) -> dict[str, Any] | None:
    if not path.is_file():
        return None
    try:
        with path.open(encoding="utf-8") as handle:
            value = json.load(handle)
    except (OSError, json.JSONDecodeError):
        return None
    return value if isinstance(value, dict) else None


def parse_timestamp(value: Any) -> dt.datetime | None:
    if not isinstance(value, str):
        return None
    try:
        parsed = dt.datetime.fromisoformat(value.replace("Z", "+00:00"))
    except ValueError:
        return None
    if parsed.tzinfo is None:
        return parsed.replace(tzinfo=dt.timezone.utc)
    return parsed.astimezone(dt.timezone.utc)


def locked_command(job_id: str) -> list[str]:
    return [
        "Rscript",
        "scripts/simulations/run_revision_job.R",
        "--job",
        job_id,
        "--config",
        "scripts/simulations/revision_jobs.json",
    ]


def find_cycles(graph: dict[str, list[str]]) -> list[str]:
    visiting: set[str] = set()
    visited: set[str] = set()
    stack: list[str] = []

    def visit(node: str) -> list[str] | None:
        if node in visiting:
            start = stack.index(node)
            return stack[start:] + [node]
        if node in visited:
            return None
        visiting.add(node)
        stack.append(node)
        for dependency in graph[node]:
            cycle = visit(dependency)
            if cycle:
                return cycle
        stack.pop()
        visiting.remove(node)
        visited.add(node)
        return None

    for job_id in graph:
        cycle = visit(job_id)
        if cycle:
            return cycle
    return []


def validate_manifest(manifest: dict[str, Any]) -> list[str]:
    errors: list[str] = []
    defaults = manifest.get("defaults")
    jobs = manifest.get("jobs")
    if not isinstance(defaults, dict):
        return ["defaults must be an object"]
    if not isinstance(jobs, list):
        return ["jobs must be an array"]
    if manifest.get("acknowledgement") != AUTHORIZED_ACK:
        errors.append(f"acknowledgement must be {AUTHORIZED_ACK!r}")
    if defaults.get("output_root") != OUTPUT_ROOT:
        errors.append(f"defaults.output_root must be {OUTPUT_ROOT!r}")
    if defaults.get("max_workers") != 24:
        errors.append("defaults.max_workers must equal 24")
    if defaults.get("max_heavy_jobs") != 1:
        errors.append("defaults.max_heavy_jobs must equal 1")
    errors.extend(validate_contract_defaults(defaults))

    job_map: dict[str, dict[str, Any]] = {}
    for index, value in enumerate(jobs):
        if not isinstance(value, dict):
            errors.append(f"jobs[{index}] must be an object")
            continue
        job_id = value.get("id")
        if not isinstance(job_id, str) or not job_id:
            errors.append(f"jobs[{index}].id must be a non-empty string")
            continue
        if job_id in job_map:
            errors.append(f"duplicate job id: {job_id}")
        job_map[job_id] = value

    if set(job_map) != set(REQUIRED_JOB_IDS):
        missing = sorted(set(REQUIRED_JOB_IDS) - set(job_map))
        extra = sorted(set(job_map) - set(REQUIRED_JOB_IDS))
        if missing:
            errors.append(f"missing required job ids: {', '.join(missing)}")
        if extra:
            errors.append(f"unexpected job ids: {', '.join(extra)}")

    max_workers = defaults.get("max_workers", 24)
    graph: dict[str, list[str]] = {}
    for job_id, job in job_map.items():
        errors.extend(validate_job(job_id, job, job_map, max_workers))
        required = job.get("required_dependencies", [])
        optional = job.get("optional_dependencies", [])
        graph[job_id] = list(required) + list(optional)
    if graph and not errors:
        cycle = find_cycles(graph)
        if cycle:
            errors.append(f"dependency cycle: {' -> '.join(cycle)}")
    return errors


def validate_contract_defaults(defaults: dict[str, Any]) -> list[str]:
    contract = defaults.get("job_contract")
    if not isinstance(contract, dict):
        return ["defaults.job_contract must be an object"]
    errors: list[str] = []
    required_keys = {
        "seed_policy",
        "config_hash_policy",
        "path_templates",
        "tmux_name_template",
        "resume_policy",
        "final_validation_conditions",
    }
    missing = sorted(required_keys - set(contract))
    if missing:
        errors.append(f"defaults.job_contract missing: {', '.join(missing)}")
    seed = contract.get("seed_policy")
    if not isinstance(seed, dict) or seed.get("origin") != 20260715:
        errors.append("defaults.job_contract.seed_policy must define origin 20260715")
    elif (
        "pair_key" not in str(seed.get("derivation"))
        or seed.get("method_independent_pair_keys") is not True
        or seed.get("matched_horizons_share_pair_keys") is not True
        or seed.get("challenge_variants_share_pair_keys") is not True
    ):
        errors.append("defaults.job_contract.seed_policy must define paired common-random seeds")
    hash_policy = contract.get("config_hash_policy")
    if not isinstance(hash_policy, dict):
        errors.append("defaults.job_contract.config_hash_policy must be an object")
    elif (
        hash_policy.get("algorithm") != "sha256"
        or hash_policy.get("length") != defaults.get("config_hash_length")
        or not isinstance(hash_policy.get("inputs"), list)
        or not hash_policy["inputs"]
    ):
        errors.append("defaults.job_contract.config_hash_policy is inconsistent with defaults")
    path_templates = contract.get("path_templates")
    expected_templates = {
        "output_dir": f"{OUTPUT_ROOT}/{{job_id}}/{{config_hash}}",
        "checkpoint": f"{{output_dir}}/{defaults.get('checkpoint_file')}",
        "dry_run_manifest": f"{{output_dir}}/{defaults.get('dry_run_manifest')}",
        "prospective_manifest": f"{{output_dir}}/{defaults.get('prospective_manifest')}",
        "run_manifest": f"{{output_dir}}/{defaults.get('run_manifest')}",
        "run_log": f"{{output_dir}}/{defaults.get('run_log')}",
        "success_marker": f"{{output_dir}}/{defaults.get('success_marker')}",
        "failure_marker": f"{{output_dir}}/{defaults.get('failure_marker')}",
        "exit_code": f"{{output_dir}}/{defaults.get('exit_code_file')}",
    }
    if path_templates != expected_templates:
        errors.append("defaults.job_contract.path_templates is inconsistent with defaults")
    tmux_template = contract.get("tmux_name_template")
    expected_tmux = f"{defaults.get('tmux_prefix')}-{{job_id}}-{{config_hash_prefix8}}"
    if tmux_template != expected_tmux:
        errors.append("defaults.job_contract.tmux_name_template is inconsistent with defaults")
    resume = contract.get("resume_policy")
    expected_resume = {
        "automatic": False,
        "explicit_resume_command_required": True,
        "checkpoint_first": True,
        "fresh_launch_refuses_existing_checkpoint": True,
        "append_run_log": True,
    }
    if resume != expected_resume:
        errors.append("defaults.job_contract.resume_policy is inconsistent with safeguards")
    conditions = contract.get("final_validation_conditions")
    if (
        not isinstance(conditions, list)
        or not conditions
        or not all(isinstance(value, str) and value for value in conditions)
    ):
        errors.append("defaults.job_contract.final_validation_conditions must be non-empty")
    return errors


def validate_job(
    job_id: str,
    job: dict[str, Any],
    job_map: dict[str, dict[str, Any]],
    max_workers: Any,
) -> list[str]:
    errors: list[str] = []
    workers = job.get("workers")
    if not isinstance(workers, int) or isinstance(workers, bool):
        errors.append(f"{job_id}: workers must be an integer")
    elif not isinstance(max_workers, int) or not 1 <= workers <= max_workers:
        errors.append(f"{job_id}: workers must be between 1 and {max_workers}")
    if not isinstance(job.get("heavy"), bool):
        errors.append(f"{job_id}: heavy must be boolean")
    if not isinstance(job.get("stage"), int):
        errors.append(f"{job_id}: stage must be an integer")
    errors.extend(validate_job_contract(job_id, job))
    errors.extend(validate_manual_review_config(job_id, job, job_map))
    if job.get("command") != locked_command(job_id):
        errors.append(f"{job_id}: command differs from the locked dispatcher command")
    expected_counts = {
        key: value for key, value in job.items() if key.startswith("expected_")
    }
    for key, value in expected_counts.items():
        if not isinstance(value, int) or isinstance(value, bool) or value <= 0:
            errors.append(f"{job_id}: {key} must be a positive integer")
    if "expected_readouts" in expected_counts:
        if expected_counts.get("expected_units") != expected_counts["expected_readouts"]:
            errors.append(f"{job_id}: expected_units must equal expected_readouts")
    if job_id == "empirical_cv":
        empirical_total = expected_counts.get("expected_train_units", 0) + expected_counts.get(
            "expected_heldout_units", 0
        )
        if expected_counts.get("expected_units") != empirical_total:
            errors.append("empirical_cv: expected_units must equal train plus heldout units")

    required = job.get("required_dependencies")
    optional = job.get("optional_dependencies")
    if not isinstance(required, list) or not all(isinstance(x, str) for x in required):
        errors.append(f"{job_id}: required_dependencies must be a string array")
        required = []
    if not isinstance(optional, list) or not all(isinstance(x, str) for x in optional):
        errors.append(f"{job_id}: optional_dependencies must be a string array")
        optional = []
    if len(required) != len(set(required)) or len(optional) != len(set(optional)):
        errors.append(f"{job_id}: dependency arrays must not contain duplicates")
    overlap = set(required) & set(optional)
    if overlap:
        errors.append(f"{job_id}: dependencies cannot be required and optional: {sorted(overlap)}")
    for dependency in [*required, *optional]:
        if dependency == job_id:
            errors.append(f"{job_id}: a job cannot depend on itself")
        elif dependency not in job_map:
            errors.append(f"{job_id}: unknown dependency {dependency}")
    conditional = job.get("conditional")
    if conditional is not None:
        required_keys = {"audit_job", "artifact", "method", "run_when_reusable"}
        if not isinstance(conditional, dict) or set(conditional) != required_keys:
            errors.append(f"{job_id}: conditional must contain exactly {sorted(required_keys)}")
        elif conditional["audit_job"] not in job_map:
            errors.append(f"{job_id}: conditional audit_job is unknown")
        elif not isinstance(conditional["run_when_reusable"], bool):
            errors.append(f"{job_id}: conditional run_when_reusable must be boolean")
    alternatives = job.get("input_alternatives", [])
    if not isinstance(alternatives, list):
        errors.append(f"{job_id}: input_alternatives must be an array")
    else:
        required_keys = {"method", "audit_job", "artifact", "fallback_job"}
        for alternative in alternatives:
            if not isinstance(alternative, dict) or set(alternative) != required_keys:
                errors.append(
                    f"{job_id}: each input alternative must contain exactly {sorted(required_keys)}"
                )
                continue
            for dependency_key in ("audit_job", "fallback_job"):
                if alternative[dependency_key] not in job_map:
                    errors.append(
                        f"{job_id}: input alternative {dependency_key} is unknown"
                    )
    return errors


def validate_job_contract(job_id: str, job: dict[str, Any]) -> list[str]:
    contract = job.get("contract")
    required_keys = {
        "inherits",
        "resource_class",
        "worker_limit",
        "job_timeout_seconds",
        "per_unit_timeout_seconds",
        "budget_seconds",
        "external_required_inputs",
    }
    if not isinstance(contract, dict) or set(contract) != required_keys:
        return [f"{job_id}: contract must contain exactly {sorted(required_keys)}"]
    errors: list[str] = []
    if contract["inherits"] != "defaults.job_contract":
        errors.append(f"{job_id}: contract must inherit defaults.job_contract")
    expected_class = "heavy" if job.get("heavy") else "light"
    if contract["resource_class"] != expected_class:
        errors.append(f"{job_id}: contract resource_class must equal {expected_class}")
    if contract["worker_limit"] != job.get("workers"):
        errors.append(f"{job_id}: contract worker_limit must equal workers")
    for key in ("job_timeout_seconds", "per_unit_timeout_seconds", "budget_seconds"):
        value = contract[key]
        if key == "job_timeout_seconds" and (
            not isinstance(value, int) or isinstance(value, bool) or value <= 0
        ):
            errors.append(f"{job_id}: contract job_timeout_seconds must be a positive integer")
        elif key != "job_timeout_seconds" and value is not None and (
            not isinstance(value, int) or isinstance(value, bool) or value <= 0
        ):
            errors.append(f"{job_id}: contract {key} must be null or a positive integer")
    inputs = contract["external_required_inputs"]
    if not isinstance(inputs, list) or not all(
        isinstance(value, str) and value for value in inputs
    ):
        errors.append(f"{job_id}: contract external_required_inputs must be a string array")
    elif len(
        [value for value in inputs if value.startswith("required_at_execution:")]
    ) != len(EXTERNAL_INPUT_ENVIRONMENTS.get(job_id, ())):
        errors.append(
            f"{job_id}: each external required input must map to an environment variable"
        )
    parameters = job.get("parameters")
    if not isinstance(parameters, dict) or not parameters:
        errors.append(f"{job_id}: parameters must be a non-empty object")
    elif job_id == "empirical_cv":
        if parameters.get("frozen_dynega_ncores") != 1:
            errors.append("empirical_cv: frozen_dynega_ncores must equal 1")
        if parameters.get("outer_parallel_backend") != "psock":
            errors.append("empirical_cv: outer_parallel_backend must equal psock")
    elif job_id == "t25_gimme":
        if parameters.get("design_range_status") != "out_of_range_short_record":
            errors.append("t25_gimme: design_range_status must equal out_of_range_short_record")
        if (
            contract["per_unit_timeout_seconds"] != 900
            or contract["budget_seconds"] != 900
        ):
            errors.append(
                "t25_gimme: per_unit_timeout_seconds and budget_seconds must both equal 900"
            )
    elif job_id == "rebuild_figures_tables":
        if parameters.get("publication_schema_revision") != 2:
            errors.append(
                "rebuild_figures_tables: publication_schema_revision must equal 2"
            )
    return errors


def validate_manual_review_config(
    job_id: str,
    job: dict[str, Any],
    job_map: dict[str, dict[str, Any]],
) -> list[str]:
    required = job.get("manual_review_before_launch", False)
    specification = job.get("manual_readiness_receipt")
    if not isinstance(required, bool):
        return [f"{job_id}: manual_review_before_launch must be boolean"]
    if not required:
        if specification is not None:
            return [f"{job_id}: manual_readiness_receipt requires manual review"]
        return []
    required_keys = {"filename", "required_status", "reviewed_dependencies"}
    if not isinstance(specification, dict) or set(specification) != required_keys:
        return [
            f"{job_id}: manual_readiness_receipt must contain exactly {sorted(required_keys)}"
        ]
    filename = specification["filename"]
    if (
        not isinstance(filename, str)
        or not filename
        or Path(filename).name != filename
    ):
        return [f"{job_id}: readiness receipt filename must be a safe basename"]
    if specification["required_status"] != "approved":
        return [f"{job_id}: readiness receipt required_status must equal approved"]
    dependencies = specification["reviewed_dependencies"]
    if (
        not isinstance(dependencies, list)
        or not dependencies
        or not all(isinstance(value, str) and value in job_map for value in dependencies)
        or len(dependencies) != len(set(dependencies))
        or job_id in dependencies
    ):
        return [f"{job_id}: readiness receipt reviewed_dependencies are invalid"]
    if job_id == "integrate_manuscript" and dependencies != [
        "aggregate_results",
        "rebuild_figures_tables",
    ]:
        return [
            "integrate_manuscript: readiness receipt must review aggregate_results "
            "and rebuild_figures_tables"
        ]
    return []


class Registry:
    def __init__(self, config_path: Path, repo_root: Path = REPO_ROOT) -> None:
        self.repo_root = repo_root.resolve()
        self.config_path = config_path.resolve()
        try:
            with self.config_path.open(encoding="utf-8") as handle:
                self.manifest = json.load(handle)
        except OSError as error:
            raise OrchestratorError(f"cannot read config: {self.config_path}: {error}") from error
        except json.JSONDecodeError as error:
            raise OrchestratorError(f"invalid JSON config: {error}") from error
        if not isinstance(self.manifest, dict):
            raise OrchestratorError("manifest root must be an object")
        errors = validate_manifest(self.manifest)
        if errors:
            raise OrchestratorError("invalid manifest:\n- " + "\n- ".join(errors))
        self.defaults = self.manifest["defaults"]
        self.jobs = {job["id"]: job for job in self.manifest["jobs"]}
        self._hashes: dict[str, str] = {}

    def ordered_job_ids(self) -> list[str]:
        return sorted(
            self.jobs,
            key=lambda job_id: (self.jobs[job_id]["stage"], REQUIRED_JOB_IDS.index(job_id)),
        )

    def get(self, job_id: str) -> dict[str, Any]:
        try:
            return self.jobs[job_id]
        except KeyError as error:
            raise OrchestratorError(f"unknown job: {job_id}") from error

    def config_hash(self, job_id: str) -> str:
        if job_id in self._hashes:
            return self._hashes[job_id]
        job = self.get(job_id)
        dependency_ids = [
            *job.get("required_dependencies", []),
            *job.get("optional_dependencies", []),
        ]
        payload = {
            "schema_version": self.manifest.get("schema_version"),
            "revision": self.manifest.get("revision"),
            "resolved_job_contract": self.defaults["job_contract"],
            "job": {
                key: value
                for key, value in job.items()
                if key not in {"description", "stage"}
            },
            "dependency_hashes": {
                dependency: self.config_hash(dependency) for dependency in dependency_ids
            },
        }
        external_inputs = self.external_inputs(job_id, require=False)
        if external_inputs:
            payload["external_input_hashes"] = {
                environment: sha256_file(path)
                for environment, path in sorted(external_inputs.items())
            }
        encoded = json.dumps(payload, sort_keys=True, separators=(",", ":")).encode()
        length = int(self.defaults["config_hash_length"])
        value = hashlib.sha256(encoded).hexdigest()[:length]
        self._hashes[job_id] = value
        return value

    def output_dir(self, job_id: str) -> Path:
        return (
            self.repo_root
            / self.defaults["output_root"]
            / job_id
            / self.config_hash(job_id)
        )

    def display_path(self, path: Path) -> str:
        try:
            return str(path.relative_to(self.repo_root))
        except ValueError:
            return str(path)

    def artifact(self, job_id: str, default_key: str) -> Path:
        return self.output_dir(job_id) / self.defaults[default_key]

    def session_name(self, job_id: str) -> str:
        prefix = self.defaults["tmux_prefix"]
        return f"{prefix}-{job_id}-{self.config_hash(job_id)[:8]}"

    def lock_path(self, job_id: str) -> Path:
        root = self.repo_root / self.defaults["lock_root"]
        return root / f"{job_id}-{self.config_hash(job_id)}.lock.json"

    def command(self, job_id: str, dry_run: bool = False) -> list[str]:
        command = list(self.get(job_id)["command"])
        if dry_run:
            command.append("--dry-run")
        return command

    def external_input_receipt_path(self, job_id: str) -> Path:
        return (
            self.repo_root
            / self.defaults["output_root"]
            / ".external_inputs"
            / f"{job_id}.json"
        )

    def external_inputs_from_receipt(
        self,
        job_id: str,
        environment_names: tuple[str, ...],
    ) -> dict[str, Path]:
        receipt_path = self.external_input_receipt_path(job_id)
        receipt = read_json(receipt_path)
        if receipt is None:
            return {}
        if receipt.get("job_id") != job_id:
            raise OrchestratorError(f"external input receipt job mismatch: {receipt_path}")
        entries = receipt.get("external_inputs")
        if not isinstance(entries, dict) or set(entries) != set(environment_names):
            raise OrchestratorError(f"external input receipt schema mismatch: {receipt_path}")
        resolved: dict[str, Path] = {}
        for environment, entry in entries.items():
            if not isinstance(entry, dict) or set(entry) != {"path", "sha256"}:
                raise OrchestratorError(f"external input receipt schema mismatch: {receipt_path}")
            path_value = entry.get("path")
            expected_hash = entry.get("sha256")
            if not isinstance(path_value, str) or not isinstance(expected_hash, str):
                raise OrchestratorError(f"external input receipt schema mismatch: {receipt_path}")
            path = Path(path_value)
            if not path.is_file():
                raise OrchestratorError(f"external input does not exist: {path}")
            if sha256_file(path) != expected_hash:
                raise OrchestratorError(f"external input hash changed: {path}")
            resolved[environment] = path
        return resolved

    def external_inputs(self, job_id: str, require: bool) -> dict[str, Path]:
        specifications = [
            value
            for value in self.get(job_id)["contract"]["external_required_inputs"]
            if value.startswith("required_at_execution:")
        ]
        environment_names = EXTERNAL_INPUT_ENVIRONMENTS.get(job_id, ())
        configured = {
            environment: os.environ.get(environment, "").strip()
            for environment in environment_names
        }
        if environment_names and not any(configured.values()):
            received = self.external_inputs_from_receipt(job_id, environment_names)
            if received:
                return received
        resolved: dict[str, Path] = {}
        for environment, specification in zip(environment_names, specifications):
            value = configured[environment]
            if not value:
                if require:
                    raise OrchestratorError(
                        f"set {environment} to {specification.removeprefix('required_at_execution:')}"
                    )
                continue
            path = Path(value).expanduser()
            if not path.is_absolute():
                path = self.repo_root / path
            path = path.resolve()
            if not path.is_file():
                raise OrchestratorError(f"external input does not exist: {path}")
            resolved[environment] = path
        return resolved

    def require_validation_external_inputs(self, job_ids: Iterable[str]) -> None:
        visited: set[str] = set()

        def visit(job_id: str) -> None:
            if job_id in visited:
                return
            visited.add(job_id)
            job = self.get(job_id)
            for dependency in [
                *job.get("required_dependencies", []),
                *job.get("optional_dependencies", []),
            ]:
                visit(dependency)
            if job_id in EXTERNAL_INPUT_ENVIRONMENTS:
                self.external_inputs(job_id, require=True)

        for job_id in job_ids:
            visit(job_id)
        self._hashes.clear()

    def persist_external_inputs(self, job_id: str) -> None:
        if job_id not in EXTERNAL_INPUT_ENVIRONMENTS:
            return
        resolved = self.external_inputs(job_id, require=True)
        receipt_path = self.external_input_receipt_path(job_id)
        receipt_path.parent.mkdir(parents=True, exist_ok=True)
        payload = {
            "job_id": job_id,
            "external_inputs": {
                environment: {
                    "path": str(path),
                    "sha256": sha256_file(path),
                }
                for environment, path in sorted(resolved.items())
            },
        }
        temporary = receipt_path.with_name(f".{receipt_path.name}.{os.getpid()}.tmp")
        try:
            with temporary.open("w", encoding="utf-8") as handle:
                json.dump(payload, handle, sort_keys=True)
                handle.write("\n")
            os.replace(temporary, receipt_path)
        finally:
            temporary.unlink(missing_ok=True)

    def job_environment(
        self,
        job_id: str,
        resume: bool,
        require_external_inputs: bool = True,
    ) -> dict[str, str]:
        job = self.get(job_id)
        contract = job["contract"]
        inputs = {
            dependency: self.display_path(self.output_dir(dependency))
            for dependency in [
                *job.get("required_dependencies", []),
                *job.get("optional_dependencies", []),
            ]
        }
        environment = {
            "REVISION_JOB_ID": job_id,
            "REVISION_JOB_OUTPUT_DIR": str(self.output_dir(job_id)),
            "REVISION_JOB_CONFIG_HASH": self.config_hash(job_id),
            "REVISION_JOB_WORKERS": str(job["workers"]),
            "REVISION_JOB_TIMEOUT_SECONDS": str(contract["job_timeout_seconds"]),
            "REVISION_JOB_RESUME": "1" if resume else "0",
            "REVISION_JOB_INPUTS_JSON": json.dumps(inputs, sort_keys=True),
        }
        method = job["parameters"].get("method")
        per_unit_timeout = contract["per_unit_timeout_seconds"]
        if per_unit_timeout is not None:
            environment["REVISION_JOB_PER_UNIT_TIMEOUT_SECONDS"] = str(per_unit_timeout)
        if contract["budget_seconds"] is not None:
            environment["REVISION_JOB_BUDGET_SECONDS"] = str(contract["budget_seconds"])
        if per_unit_timeout is not None and method == "graphicalvar":
            environment["REVISION_GRAPHICALVAR_TIMEOUT_SEC"] = str(per_unit_timeout)
        if per_unit_timeout is not None and method == "gimme":
            environment["REVISION_GIMME_TIMEOUT_SEC"] = str(per_unit_timeout)
        environment.update(
            {
                name: str(path)
                for name, path in self.external_inputs(
                    job_id,
                    require=require_external_inputs,
                ).items()
            }
        )
        return environment


def tmux_sessions() -> set[str]:
    if shutil.which("tmux") is None:
        return set()
    result = subprocess.run(
        ["tmux", "list-sessions", "-F", "#{session_name}"],
        check=False,
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        return set()
    return {line.strip() for line in result.stdout.splitlines() if line.strip()}


def revision_processes(job_ids: Iterable[str]) -> dict[str, list[dict[str, Any]]]:
    evidence = {job_id: [] for job_id in job_ids}
    result = subprocess.run(
        ["ps", "-eo", "pid=,etimes=,args="],
        check=False,
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        return evidence
    for line in result.stdout.splitlines():
        parts = line.strip().split(None, 2)
        if len(parts) != 3:
            continue
        pid_text, elapsed_text, command = parts
        try:
            tokens = shlex.split(command)
        except ValueError:
            continue
        for index, token in enumerate(tokens[:-3]):
            if Path(token).name != "Rscript":
                continue
            if Path(tokens[index + 1]).name != "run_revision_job.R":
                continue
            tail = tokens[index + 2 :]
            if "--job" not in tail:
                continue
            job_index = tail.index("--job")
            if job_index + 1 >= len(tail):
                continue
            job_id = tail[job_index + 1]
            if job_id in evidence:
                evidence[job_id].append(
                    {
                        "pid": int(pid_text),
                        "elapsed_seconds": int(elapsed_text),
                        "command": command[:300],
                    }
                )
            break
    return evidence


def checkpoint_summary(path: Path) -> dict[str, Any]:
    summary: dict[str, Any] = {
        "rows": 0,
        "status_counts": {},
        "stage_counts": {},
        "current_stage": None,
        "unique_units": None,
        "unique_panels": None,
        "unique_representations": None,
        "unique_method_panels": None,
        "duplicate_units": None,
        "age_seconds": None,
    }
    if not path.is_file():
        return summary
    counts: dict[str, int] = {}
    stage_counts: dict[str, int] = {}
    unique_units: set[str] = set()
    unique_panels: set[str] = set()
    unique_representations: set[str] = set()
    unique_method_panels: set[tuple[str, str]] = set()
    try:
        with path.open(newline="", encoding="utf-8-sig") as handle:
            reader = csv.DictReader(handle)
            fieldnames = set(reader.fieldnames or [])
            for row in reader:
                summary["rows"] += 1
                status = row.get("status") or row.get("Status")
                if status:
                    counts[status] = counts.get(status, 0) + 1
                stage = row.get("stage") or row.get("Stage")
                if stage:
                    stage_counts[stage] = stage_counts.get(stage, 0) + 1
                    summary["current_stage"] = stage
                unit_key = row.get("unit_key")
                if unit_key:
                    unique_units.add(unit_key)
                panel_key = next(
                    (
                        row.get(candidate)
                        for candidate in (
                            "panel_key",
                            "base_panel_key",
                            "panel_id",
                            "condition_key",
                            "simulation_id",
                        )
                        if candidate in fieldnames and row.get(candidate)
                    ),
                    None,
                )
                if panel_key:
                    unique_panels.add(panel_key)
                representation_key = row.get("representation_key")
                if representation_key:
                    unique_representations.add(representation_key)
                method_id = row.get("method_id") or row.get("method")
                method_panel_key = representation_key or panel_key
                if method_id and method_panel_key:
                    unique_method_panels.add((method_panel_key, method_id))
    except (OSError, csv.Error) as error:
        summary["read_error"] = str(error)
    summary["status_counts"] = dict(sorted(counts.items()))
    summary["stage_counts"] = dict(sorted(stage_counts.items()))
    if unique_units:
        summary["unique_units"] = len(unique_units)
        summary["duplicate_units"] = summary["rows"] - len(unique_units)
    if unique_panels:
        summary["unique_panels"] = len(unique_panels)
    if unique_representations:
        summary["unique_representations"] = len(unique_representations)
    if unique_method_panels:
        summary["unique_method_panels"] = len(unique_method_panels)
    try:
        summary["age_seconds"] = max(0, round(time.time() - path.stat().st_mtime))
    except OSError as error:
        summary["age_error"] = str(error)
    return summary


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def validate_run_manifest(registry: Registry, job_id: str) -> list[str]:
    path = registry.artifact(job_id, "run_manifest")
    manifest = read_json(path)
    if manifest is None:
        return [f"missing or invalid {registry.display_path(path)}"]
    errors: list[str] = []
    contract = registry.get(job_id)["contract"]
    exact_fields = {
        "job_id": job_id,
        "config_hash": registry.config_hash(job_id),
        "command": registry.command(job_id),
        "status": "complete",
        "workers": registry.get(job_id)["workers"],
        "job_timeout_seconds": contract["job_timeout_seconds"],
        "dry_run": False,
    }
    if contract["per_unit_timeout_seconds"] is not None:
        exact_fields["per_unit_timeout_seconds"] = contract["per_unit_timeout_seconds"]
    if contract["budget_seconds"] is not None:
        exact_fields["budget_seconds"] = contract["budget_seconds"]
    for key, required in exact_fields.items():
        if manifest.get(key) != required:
            errors.append(f"run manifest {key}={manifest.get(key)!r}; expected {required!r}")
    versions = manifest.get("software_versions")
    if not isinstance(versions, dict) or not versions:
        errors.append("run manifest software_versions must be a non-empty object")
    return errors


def validate_csv_completion_counts(
    registry: Registry,
    job_id: str,
    path: Path,
) -> list[str]:
    summary = checkpoint_summary(path)
    label = registry.display_path(path)
    errors: list[str] = []
    if summary.get("read_error"):
        return [f"invalid completion CSV {label}: {summary['read_error']}"]
    expected = expected_units(registry, job_id)
    if summary["unique_units"] is None:
        errors.append(f"completion CSV lacks non-empty unit_key values: {label}")
    elif summary["unique_units"] != expected:
        errors.append(
            f"completion CSV unique_units={summary['unique_units']!r}; expected {expected!r}: {label}"
        )
    if summary["duplicate_units"]:
        errors.append(
            f"completion CSV has {summary['duplicate_units']} duplicate unit_key rows: {label}"
        )

    job = registry.get(job_id)
    count_fields = {
        "expected_panels": "unique_panels",
        "expected_base_panels": "unique_panels",
        "expected_representations": "unique_representations",
        "expected_method_panels": "unique_method_panels",
    }
    for configured_key, observed_key in count_fields.items():
        configured = job.get(configured_key)
        if configured is None:
            continue
        observed = summary[observed_key]
        if observed != configured:
            errors.append(
                f"completion CSV {observed_key}={observed!r}; "
                f"{configured_key}={configured!r}: {label}"
            )
    return errors


def validate_success_marker(registry: Registry, job_id: str) -> list[str]:
    marker_path = registry.artifact(job_id, "success_marker")
    marker = read_json(marker_path)
    if marker is None:
        return [f"missing or invalid {registry.display_path(marker_path)}"]
    errors: list[str] = []
    expected = expected_units(registry, job_id)
    exact_fields = {
        "job_id": job_id,
        "config_hash": registry.config_hash(job_id),
        "expected_units": expected,
        "observed_unique_units": expected,
        "prospective_manifest_valid": True,
        "run_manifest_valid": True,
    }
    expected_keys = {*exact_fields, "output_hashes"}
    if set(marker) != expected_keys:
        missing = sorted(expected_keys - set(marker))
        extra = sorted(set(marker) - expected_keys)
        details: list[str] = []
        if missing:
            details.append("missing " + ", ".join(missing))
        if extra:
            details.append("unexpected " + ", ".join(extra))
        errors.append("success marker schema mismatch: " + "; ".join(details))
    for key, required in exact_fields.items():
        if marker.get(key) != required:
            errors.append(f"success marker {key}={marker.get(key)!r}; expected {required!r}")

    output_dir = registry.output_dir(job_id).resolve()
    errors.extend(validate_prospective_manifest(registry, job_id))
    errors.extend(validate_run_manifest(registry, job_id))
    output_hashes = marker.get("output_hashes")
    if not isinstance(output_hashes, dict) or not output_hashes:
        errors.append("success marker output_hashes must be a non-empty map")
        return errors
    hashed_paths: set[str] = set()
    for relative_name, expected_hash in output_hashes.items():
        if not isinstance(relative_name, str) or not isinstance(expected_hash, str):
            errors.append("success marker output_hashes entries must map strings to strings")
            continue
        candidate = (output_dir / relative_name).resolve()
        if not candidate.is_relative_to(output_dir):
            errors.append(f"hashed output escapes job directory: {relative_name}")
            continue
        if not candidate.is_file():
            errors.append(f"missing hashed output: {relative_name}")
            continue
        hashed_paths.add(relative_name)
        actual_hash = sha256_file(candidate)
        if actual_hash.lower() != expected_hash.lower():
            errors.append(f"output hash mismatch: {relative_name}")

    prospective = read_json(registry.artifact(job_id, "prospective_manifest")) or {}
    expected_outputs = prospective.get("expected_outputs")
    if isinstance(expected_outputs, list):
        success_name = registry.defaults["success_marker"]
        required_hashes = {
            name for name in expected_outputs if isinstance(name, str) and name != success_name
        }
        missing_hashes = sorted(required_hashes - hashed_paths)
        if missing_hashes:
            errors.append(
                "success marker omits hashes for expected outputs: " + ", ".join(missing_hashes)
            )

        checkpoint_name = registry.defaults["checkpoint_file"]
        if checkpoint_name in expected_outputs:
            checkpoint_path = output_dir / checkpoint_name
            if not checkpoint_path.is_file():
                errors.append(f"missing expected checkpoint: {checkpoint_name}")
            else:
                errors.extend(validate_csv_completion_counts(registry, job_id, checkpoint_path))
        results_name = "results.csv"
        if results_name in expected_outputs:
            results_path = output_dir / results_name
            if not results_path.is_file():
                errors.append(f"missing expected final results: {results_name}")
            else:
                errors.extend(validate_csv_completion_counts(registry, job_id, results_path))
    return errors


def expected_units(registry: Registry, job_id: str) -> int | None:
    configured = registry.get(job_id).get("expected_units")
    if isinstance(configured, int) and not isinstance(configured, bool) and configured > 0:
        return configured
    dry_run = read_json(registry.artifact(job_id, "dry_run_manifest")) or {}
    for key in ("expected_units", "expected_rows", "total_units", "total_rows"):
        value = dry_run.get(key)
        if isinstance(value, int) and not isinstance(value, bool) and value > 0:
            return value
    return None


def read_exit_code(path: Path) -> int | None:
    try:
        return int(path.read_text(encoding="utf-8").strip())
    except (OSError, ValueError):
        return None


def path_age_seconds(path: Path) -> int | None:
    try:
        return max(0, round(time.time() - path.stat().st_mtime))
    except OSError:
        return None


def job_status(
    registry: Registry,
    job_id: str,
    sessions: set[str] | None = None,
    processes: dict[str, list[dict[str, Any]]] | None = None,
) -> dict[str, Any]:
    sessions = sessions if sessions is not None else tmux_sessions()
    processes = processes if processes is not None else revision_processes(registry.jobs)
    checkpoint_path = registry.artifact(job_id, "checkpoint_file")
    checkpoint = checkpoint_summary(checkpoint_path)
    output_dir = registry.output_dir(job_id)
    success_path = registry.artifact(job_id, "success_marker")
    success = success_path.is_file()
    failure = registry.artifact(job_id, "failure_marker").is_file()
    dry_run = registry.artifact(job_id, "dry_run_manifest").is_file()
    exit_code = read_exit_code(registry.artifact(job_id, "exit_code_file"))
    session = registry.session_name(job_id)
    process_evidence = processes.get(job_id, [])
    active = session in sessions or bool(process_evidence)
    locked = registry.lock_path(job_id).is_file()
    job = registry.get(job_id)
    stall_after = int(job.get("stall_after_seconds", registry.defaults["stall_after_seconds"]))
    activity_age = checkpoint["age_seconds"]
    if activity_age is None and locked:
        activity_age = path_age_seconds(registry.lock_path(job_id))
    if activity_age is None and process_evidence:
        activity_age = max(item["elapsed_seconds"] for item in process_evidence)
    stalled = active and activity_age is not None and activity_age > stall_after

    artifact_issues: list[str] = []
    if checkpoint.get("read_error"):
        artifact_issues.append(f"invalid checkpoint: {checkpoint['read_error']}")
    if checkpoint.get("age_error"):
        artifact_issues.append(f"cannot stat checkpoint: {checkpoint['age_error']}")
    success_errors = validate_success_marker(registry, job_id) if success else []
    if success_errors:
        artifact_issues.extend(success_errors)
    dry_run_errors = compare_dry_run_counts(registry, job_id) if dry_run else []
    if dry_run_errors:
        artifact_issues.extend(dry_run_errors)
    if not dry_run:
        artifact_issues.append(
            f"missing {registry.display_path(registry.artifact(job_id, 'dry_run_manifest'))}"
        )
    prospective_errors = validate_prospective_manifest(registry, job_id) if dry_run else []
    if prospective_errors:
        artifact_issues.extend(prospective_errors)
    run_manifest_path = registry.artifact(job_id, "run_manifest")
    if (active or locked or checkpoint["rows"] > 0 or exit_code is not None) and not run_manifest_path.is_file():
        artifact_issues.append(f"missing {registry.display_path(run_manifest_path)}")
    if failure:
        artifact_issues.append(
            f"failure marker present: {registry.display_path(registry.artifact(job_id, 'failure_marker'))}"
        )
    if active and not checkpoint_path.is_file():
        artifact_issues.append(f"checkpoint not created yet: {registry.display_path(checkpoint_path)}")

    if stalled:
        state = "STALLED"
    elif active:
        state = "RUNNING"
    elif failure or success_errors or (exit_code is not None and exit_code != 0):
        state = "FAILED"
    elif success:
        state = "COMPLETE"
    elif exit_code == 0:
        artifact_issues.append("process exited zero without valid completion evidence")
        state = "FAILED"
    elif locked or checkpoint["rows"] > 0:
        state = "PARTIAL"
    elif dry_run and not dry_run_errors and not prospective_errors:
        state = "READY"
    elif dry_run:
        state = "BLOCKED"
    else:
        state = "PLANNED"

    total = expected_units(registry, job_id)
    progress = int(checkpoint["rows"])
    if state == "COMPLETE" and total is not None and progress == 0:
        progress = total
    percent = None
    if total:
        percent = round(min(progress / total, 1.0) * 100, 2)
    return {
        "event": "status",
        "recorded_at": isoformat(),
        "job_id": job_id,
        "config_hash": registry.config_hash(job_id),
        "state": state,
        "dag_stage": job["stage"],
        "current_stage": checkpoint["current_stage"] or job["parameters"]["analysis"],
        "heavy": job["heavy"],
        "workers": job["workers"],
        "conditional": job.get("conditional"),
        "conditional_active": None,
        "conditional_reason": None,
        "required_dependencies": list(job["required_dependencies"]),
        "blocking_dependencies": [],
        "session": session,
        "session_active": session in sessions,
        "process_active": bool(process_evidence),
        "process_evidence": process_evidence,
        "lock_present": locked,
        "output_dir": registry.display_path(output_dir),
        "checkpoint": registry.display_path(checkpoint_path),
        "checkpoint_age_seconds": checkpoint["age_seconds"],
        "last_activity_age_seconds": activity_age,
        "stall_after_seconds": stall_after,
        "progress": progress,
        "unique_units": checkpoint["unique_units"],
        "unique_panels": checkpoint["unique_panels"],
        "unique_representations": checkpoint["unique_representations"],
        "unique_method_panels": checkpoint["unique_method_panels"],
        "duplicate_units": checkpoint["duplicate_units"],
        "expected_units": total,
        "progress_percent": percent,
        "status_counts": checkpoint["status_counts"],
        "stage_counts": checkpoint["stage_counts"],
        "exit_code": exit_code,
        "artifact_issues": artifact_issues,
        "recent_rate_per_hour": None,
        "eta_seconds": None,
        "eta_at": None,
    }


def load_history(path: Path) -> list[dict[str, Any]]:
    if not path.is_file():
        return []
    records: list[dict[str, Any]] = []
    try:
        with path.open(encoding="utf-8") as handle:
            for line in handle:
                try:
                    value = json.loads(line)
                except json.JSONDecodeError:
                    continue
                if isinstance(value, dict):
                    records.append(value)
    except OSError:
        return []
    return records


def add_eta(
    snapshot: dict[str, Any],
    history: Iterable[dict[str, Any]],
) -> dict[str, Any]:
    total = snapshot.get("expected_units")
    current = snapshot.get("progress")
    if (
        snapshot.get("state") != "RUNNING"
        or not isinstance(total, int)
        or not isinstance(current, int)
        or current >= total
    ):
        return snapshot
    candidates = [
        record
        for record in history
        if record.get("event") == "status"
        and record.get("job_id") == snapshot["job_id"]
        and record.get("config_hash") == snapshot["config_hash"]
        and isinstance(record.get("progress"), int)
        and parse_timestamp(record.get("recorded_at")) is not None
    ]
    candidates.append(snapshot)
    candidates.sort(key=lambda record: parse_timestamp(record["recorded_at"]) or utc_now())

    progressing: list[dict[str, Any]] = []
    for record in candidates[-50:]:
        if not progressing or record["progress"] > progressing[-1]["progress"]:
            progressing.append(record)
    if len(progressing) < 2:
        return snapshot
    first = progressing[-2]
    last = progressing[-1]
    first_time = parse_timestamp(first["recorded_at"])
    last_time = parse_timestamp(last["recorded_at"])
    if first_time is None or last_time is None:
        return snapshot
    elapsed = (last_time - first_time).total_seconds()
    completed = last["progress"] - first["progress"]
    if elapsed <= 0 or completed <= 0:
        return snapshot
    rate = completed / elapsed
    remaining_seconds = max(0, round((total - current) / rate))
    snapshot["recent_rate_per_hour"] = round(rate * 3600, 3)
    snapshot["eta_seconds"] = remaining_seconds
    snapshot["eta_at"] = isoformat(utc_now() + dt.timedelta(seconds=remaining_seconds))
    return snapshot


def append_history(path: Path, record: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as handle:
        handle.write(json.dumps(record, sort_keys=True) + "\n")


def history_path(registry: Registry) -> Path:
    return registry.repo_root / registry.defaults["status_history"]


def select_job_ids(registry: Registry, selector: str | None) -> list[str]:
    if selector in (None, "all"):
        return registry.ordered_job_ids()
    registry.get(selector)
    return [selector]


def requested_job(args: argparse.Namespace, allow_all: bool) -> str:
    positional = getattr(args, "job_positional", None)
    option = getattr(args, "job", None)
    if positional and option and positional != option:
        raise OrchestratorError(
            f"conflicting job selectors: positional {positional!r} and --job {option!r}"
        )
    selected = option or positional
    if selected:
        return selected
    if allow_all:
        return "all"
    raise OrchestratorError("--job JOB_ID is required")


def format_table(rows: Sequence[Sequence[Any]], headers: Sequence[str]) -> str:
    values = [["" if value is None else str(value) for value in row] for row in rows]
    widths = [len(header) for header in headers]
    for row in values:
        for index, value in enumerate(row):
            widths[index] = max(widths[index], len(value))
    header_line = "  ".join(header.ljust(widths[index]) for index, header in enumerate(headers))
    divider = "  ".join("-" * width for width in widths)
    body = [
        "  ".join(value.ljust(widths[index]) for index, value in enumerate(row))
        for row in values
    ]
    return "\n".join([header_line, divider, *body])


def expected_work(job: dict[str, Any]) -> dict[str, int]:
    return {
        key: value
        for key, value in job.items()
        if key.startswith("expected_") and isinstance(value, int) and not isinstance(value, bool)
    }


def resolved_job_paths(registry: Registry, job_id: str) -> dict[str, str]:
    return {
        "output_dir": registry.display_path(registry.output_dir(job_id)),
        "checkpoint": registry.display_path(registry.artifact(job_id, "checkpoint_file")),
        "dry_run_manifest": registry.display_path(
            registry.artifact(job_id, "dry_run_manifest")
        ),
        "prospective_manifest": registry.display_path(
            registry.artifact(job_id, "prospective_manifest")
        ),
        "run_manifest": registry.display_path(registry.artifact(job_id, "run_manifest")),
        "run_log": registry.display_path(registry.artifact(job_id, "run_log")),
        "success_marker": registry.display_path(registry.artifact(job_id, "success_marker")),
        "failure_marker": registry.display_path(registry.artifact(job_id, "failure_marker")),
        "exit_code": registry.display_path(registry.artifact(job_id, "exit_code_file")),
        "lock": registry.display_path(registry.lock_path(job_id)),
        "status_history": registry.defaults["status_history"],
    }


def resolved_required_inputs(registry: Registry, job_id: str) -> dict[str, Any]:
    job = registry.get(job_id)
    required = set(job["required_dependencies"])
    dependencies = [
        {
            "job_id": dependency,
            "required": dependency in required,
            "output_dir": registry.display_path(registry.output_dir(dependency)),
        }
        for dependency in [
            *job["required_dependencies"],
            *job["optional_dependencies"],
        ]
    ]
    return {
        "dependency_outputs": dependencies,
        "external_artifacts": list(job["contract"]["external_required_inputs"]),
        "resolved_external_artifacts": [
            {
                "environment": environment,
                "path": registry.display_path(path),
                "sha256": sha256_file(path),
            }
            for environment, path in sorted(
                registry.external_inputs(job_id, require=False).items()
            )
        ],
    }


def readiness_receipt_path(registry: Registry, job_id: str) -> Path | None:
    specification = registry.get(job_id).get("manual_readiness_receipt")
    if not isinstance(specification, dict):
        return None
    return registry.output_dir(job_id) / specification["filename"]


def readiness_receipt_contract(registry: Registry, job_id: str) -> dict[str, Any]:
    job = registry.get(job_id)
    path = readiness_receipt_path(registry, job_id)
    specification = job.get("manual_readiness_receipt")
    reviewed_dependencies = (
        list(specification["reviewed_dependencies"])
        if isinstance(specification, dict)
        else []
    )
    return {
        "required": bool(job.get("manual_review_before_launch")),
        "path": registry.display_path(path) if path is not None else None,
        "required_fields": [
            "job_id",
            "config_hash",
            "status",
            "reviewer",
            "reviewed_at",
            "reviewed_dependency_hashes",
        ] if path is not None else [],
        "required_status": (
            specification["required_status"]
            if path is not None
            else None
        ),
        "reviewed_dependencies": reviewed_dependencies,
        "dependency_hash_source": "SHA-256 of each dependency's current _SUCCESS.json",
    }


def plan_record(registry: Registry, job_id: str) -> dict[str, Any]:
    job = registry.get(job_id)
    inherited = registry.defaults["job_contract"]
    contract = job["contract"]
    return {
        "stage": job["stage"],
        "job_id": job_id,
        "description": job["description"],
        "class": contract["resource_class"],
        "resource_class": contract["resource_class"],
        "workers": job["workers"],
        "worker_limit": contract["worker_limit"],
        "job_timeout_seconds": contract["job_timeout_seconds"],
        "per_unit_timeout_seconds": contract["per_unit_timeout_seconds"],
        "budget_seconds": contract["budget_seconds"],
        "parameters": job["parameters"],
        "expected_work": expected_work(job),
        "required_dependencies": list(job["required_dependencies"]),
        "optional_dependencies": list(job["optional_dependencies"]),
        "required_inputs": resolved_required_inputs(registry, job_id),
        "conditional": job.get("conditional"),
        "input_alternatives": list(job.get("input_alternatives", [])),
        "seed_policy": inherited["seed_policy"],
        "config_hash_policy": inherited["config_hash_policy"],
        "config_hash": registry.config_hash(job_id),
        "paths": resolved_job_paths(registry, job_id),
        "tmux_session": registry.session_name(job_id),
        "resume_policy": inherited["resume_policy"],
        "manual_review": readiness_receipt_contract(registry, job_id),
        "final_validation_conditions": inherited["final_validation_conditions"],
        "command": registry.command(job_id),
    }


def expected_work_label(work: dict[str, int]) -> str:
    ordered = sorted(work, key=lambda key: (key != "expected_units", key))
    return ",".join(f"{key.removeprefix('expected_')}={work[key]}" for key in ordered)


def command_plan(registry: Registry, args: argparse.Namespace) -> int:
    job_ids = select_job_ids(registry, requested_job(args, allow_all=True))
    records = [plan_record(registry, job_id) for job_id in job_ids]
    if args.json:
        print(json.dumps(records, indent=2))
        return 0
    rows = [
        [
            record["stage"],
            record["job_id"],
            record["resource_class"],
            record["workers"],
            record["job_timeout_seconds"],
            record["per_unit_timeout_seconds"] or "-",
            record["budget_seconds"] or "-",
            expected_work_label(record["expected_work"]),
            ",".join(record["required_dependencies"]) or "-",
            ",".join(record["optional_dependencies"]) or "-",
            "explicit/checkpoint",
            record["config_hash"],
            record["tmux_session"],
            record["paths"]["output_dir"],
        ]
        for record in records
    ]
    headers = (
        "stage",
        "job",
        "class",
        "workers",
        "job timeout",
        "unit timeout",
        "budget",
        "expected work",
        "requires",
        "optional",
        "resume",
        "hash",
        "tmux",
        "output",
    )
    print(format_table(rows, headers))
    return 0


def command_validate(registry: Registry, args: argparse.Namespace) -> int:
    job_ids = select_job_ids(registry, requested_job(args, allow_all=True))
    print(f"Manifest valid: {len(registry.jobs)} jobs; max workers 24; max heavy jobs 1.")
    if args.structural_only:
        return 0
    registry.require_validation_external_inputs(job_ids)
    failures: list[str] = []
    for job_id in job_ids:
        command = registry.command(job_id, dry_run=True)
        output_dir = registry.output_dir(job_id)
        before_files = {
            path.relative_to(output_dir).as_posix()
            for path in output_dir.rglob("*")
            if path.is_file()
        } if output_dir.is_dir() else set()
        env = os.environ.copy()
        env.update(
            registry.job_environment(
                job_id,
                resume=False,
                require_external_inputs=True,
            )
        )
        env["REVISION_JOB_VALIDATE"] = "1"
        print(f"[{job_id}] {shlex.join(command)}", flush=True)
        result = subprocess.run(command, cwd=registry.repo_root, env=env, check=False)
        if result.returncode != 0:
            failures.append(f"{job_id} (exit {result.returncode})")
            continue
        after_files = {
            path.relative_to(output_dir).as_posix()
            for path in output_dir.rglob("*")
            if path.is_file()
        } if output_dir.is_dir() else set()
        allowed_new = {
            registry.defaults["dry_run_manifest"],
            registry.defaults["prospective_manifest"],
        }
        forbidden_new = sorted((after_files - before_files) - allowed_new)
        if forbidden_new:
            failures.append(
                f"{job_id} (dry-run created forbidden artifacts: {', '.join(forbidden_new)})"
            )
            continue
        failures.extend(compare_dry_run_counts(registry, job_id))
        failures.extend(validate_prospective_manifest(registry, job_id))
    if failures:
        raise OrchestratorError("dry-run validation failed: " + ", ".join(failures))
    return 0


def compare_dry_run_counts(registry: Registry, job_id: str) -> list[str]:
    path = registry.artifact(job_id, "dry_run_manifest")
    dry_run = read_json(path)
    if dry_run is None:
        return [f"{job_id} (missing {registry.display_path(path)})"]
    nested_counts: dict[str, Any] = {}
    for container_name in ("counts", "expected_counts"):
        container = dry_run.get(container_name)
        if isinstance(container, dict):
            nested_counts.update(container)
    mismatches: list[str] = []
    expected = {
        key: value
        for key, value in registry.get(job_id).items()
        if key.startswith("expected_")
    }
    for key, configured in expected.items():
        actual = dry_run.get(key, nested_counts.get(key))
        if actual != configured:
            mismatches.append(
                f"{job_id} ({key}: config={configured!r}, dry-run={actual!r})"
            )
    return mismatches


def validate_prospective_manifest(registry: Registry, job_id: str) -> list[str]:
    path = registry.artifact(job_id, "prospective_manifest")
    prospective = read_json(path)
    if prospective is None:
        return [f"{job_id} (missing or invalid {registry.display_path(path)})"]
    mismatches: list[str] = []
    exact = {
        "status": "planned",
        "dry_run": True,
        "job_id": job_id,
        "config_hash": registry.config_hash(job_id),
        "command": registry.command(job_id),
    }
    for key, configured in exact.items():
        if prospective.get(key) != configured:
            mismatches.append(
                f"{job_id} (prospective {key}: config={configured!r}, artifact={prospective.get(key)!r})"
            )
    required_objects = ("parameters", "seed", "required_inputs", "software_versions")
    for key in required_objects:
        value = prospective.get(key)
        if not isinstance(value, dict) or not value:
            mismatches.append(f"{job_id} (prospective {key} missing or empty)")
    expected_outputs = prospective.get("expected_outputs")
    if (
        not isinstance(expected_outputs, list)
        or not expected_outputs
        or not all(isinstance(value, str) and value for value in expected_outputs)
    ):
        mismatches.append(f"{job_id} (prospective expected_outputs missing or empty)")
    else:
        unsafe = [
            value
            for value in expected_outputs
            if Path(value).is_absolute() or ".." in Path(value).parts
        ]
        if unsafe:
            mismatches.append(
                f"{job_id} (prospective expected_outputs contain unsafe paths: {unsafe!r})"
            )
    counts = prospective.get("expected_counts")
    if not isinstance(counts, dict):
        mismatches.append(f"{job_id} (prospective expected_counts missing or invalid)")
        return mismatches
    expected = {
        key: value
        for key, value in registry.get(job_id).items()
        if key.startswith("expected_")
    }
    for key, configured in expected.items():
        if counts.get(key) != configured:
            mismatches.append(
                f"{job_id} (prospective {key}: config={configured!r}, artifact={counts.get(key)!r})"
            )
    return mismatches


def reuse_decision(
    registry: Registry,
    specification: dict[str, Any],
) -> tuple[bool | None, str, str | None]:
    audit_job = specification["audit_job"]
    path = registry.output_dir(audit_job) / specification["artifact"]
    audit = read_json(path)
    if audit is None:
        return None, "", f"missing or invalid {registry.display_path(path)}"
    methods = audit.get("methods")
    method = specification["method"]
    entry = methods.get(method) if isinstance(methods, dict) else None
    if not isinstance(entry, dict) or not isinstance(entry.get("reusable"), bool):
        return None, "", f"reuse audit lacks methods.{method}.reusable boolean"
    reason = entry.get("reason")
    return entry["reusable"], reason if isinstance(reason, str) else "", None


def dependency_blockers(
    registry: Registry,
    job_id: str,
    states: dict[str, dict[str, Any]],
) -> list[str]:
    job = registry.get(job_id)
    blockers = [
        dependency
        for dependency in job["required_dependencies"]
        if states[dependency]["state"] != "COMPLETE"
    ]
    conditional = job.get("conditional")
    if isinstance(conditional, dict) and states[job_id].get("conditional_active") is not True:
        blockers.append("conditional reuse decision does not require fallback")
    for alternative in job.get("input_alternatives", []):
        audit_job = alternative["audit_job"]
        if states[audit_job]["state"] != "COMPLETE":
            blockers.append(f"{alternative['method']} reuse audit incomplete")
            continue
        reusable, _, issue = reuse_decision(registry, alternative)
        if issue:
            blockers.append(issue)
        elif reusable is False and states[alternative["fallback_job"]]["state"] != "COMPLETE":
            blockers.append(
                f"{alternative['method']} fallback incomplete: {alternative['fallback_job']}"
            )
    return blockers


def apply_conditional_reuse_states(
    registry: Registry,
    states: dict[str, dict[str, Any]],
) -> None:
    for job_id, status in states.items():
        conditional = registry.get(job_id).get("conditional")
        if not isinstance(conditional, dict):
            continue
        audit_job = conditional["audit_job"]
        status["conditional_active"] = None
        status["conditional_reason"] = "reuse audit pending"
        if states[audit_job]["state"] != "COMPLETE":
            if status["state"] not in {"RUNNING", "COMPLETE"}:
                status["state"] = "BLOCKED"
            status["artifact_issues"].append("reuse audit must complete before fallback decision")
            status["blocking_dependencies"] = [audit_job]
            continue
        reusable, reason, issue = reuse_decision(registry, conditional)
        if issue:
            if status["state"] not in {"RUNNING", "COMPLETE"}:
                status["state"] = "BLOCKED"
            status["artifact_issues"].append(issue)
            status["conditional_reason"] = issue
            status["blocking_dependencies"] = [issue]
            continue
        active = reusable == conditional.get("run_when_reusable", False)
        status["conditional_active"] = active
        status["conditional_reason"] = reason
        if not active and status["state"] != "COMPLETE":
            status["state"] = "BLOCKED"
            status["artifact_issues"].append(
                "verified reusable artifact; fallback is not required"
                + (f": {reason}" if reason else "")
            )
            status["blocking_dependencies"] = [
                "conditional reuse decision does not require fallback"
            ]


def collect_statuses(
    registry: Registry,
    job_ids: Sequence[str],
    memory_history: Sequence[dict[str, Any]] | None = None,
) -> list[dict[str, Any]]:
    sessions = tmux_sessions()
    processes = revision_processes(registry.jobs)
    persisted = load_history(history_path(registry))
    combined_history = [*persisted, *(memory_history or [])]
    all_statuses = {
        job_id: add_eta(
            job_status(registry, job_id, sessions, processes),
            combined_history,
        )
        for job_id in registry.ordered_job_ids()
    }
    apply_conditional_reuse_states(registry, all_statuses)
    for job_id, status in all_statuses.items():
        if status["state"] not in {"PLANNED", "READY"}:
            continue
        blockers = dependency_blockers(registry, job_id, all_statuses)
        status["blocking_dependencies"] = blockers
        if blockers:
            status["state"] = "BLOCKED"
            status["artifact_issues"].append(
                "incomplete dependencies: " + ", ".join(blockers)
            )
    return [all_statuses[job_id] for job_id in job_ids]


def print_statuses(statuses: Sequence[dict[str, Any]], as_json: bool) -> None:
    if as_json:
        print(json.dumps(list(statuses), indent=2, sort_keys=True))
        return
    rows = []
    for status in statuses:
        progress = str(status["progress"])
        if status["expected_units"] is not None:
            progress = f"{progress}/{status['expected_units']}"
        status_counts = ",".join(
            f"{key}:{value}" for key, value in status["status_counts"].items()
        ) or "-"
        evidence: list[str] = []
        if status["session_active"]:
            evidence.append("tmux")
        if status["process_active"]:
            pids = ",".join(str(item["pid"]) for item in status["process_evidence"])
            evidence.append(f"proc:{pids}")
        eta = "-"
        if status["eta_seconds"] is not None:
            eta = f"{status['eta_seconds']}s"
        rate = "-"
        if status["recent_rate_per_hour"] is not None:
            rate = f"{status['recent_rate_per_hour']}/h"
        rows.append(
            [
                status["job_id"],
                status["state"],
                status["current_stage"],
                progress,
                status["unique_panels"],
                status_counts,
                status["checkpoint_age_seconds"],
                rate,
                eta,
                "+".join(evidence) or "-",
                "; ".join(status["artifact_issues"]) or "-",
            ]
        )
    print(
        format_table(
            rows,
            (
                "job",
                "state",
                "stage",
                "progress",
                "unique",
                "status counts",
                "age(s)",
                "rate",
                "eta",
                "evidence",
                "missing/invalid artifacts",
            ),
        )
    )


def command_status(registry: Registry, args: argparse.Namespace) -> int:
    statuses = collect_statuses(
        registry,
        select_job_ids(registry, requested_job(args, allow_all=True)),
    )
    if not args.no_record:
        for status in statuses:
            append_history(history_path(registry), status)
    print_statuses(statuses, args.json)
    return 0


def command_watch(registry: Registry, args: argparse.Namespace) -> int:
    job_ids = select_job_ids(registry, requested_job(args, allow_all=True))
    memory_history: list[dict[str, Any]] = []
    iteration = 0
    try:
        while args.iterations == 0 or iteration < args.iterations:
            statuses = collect_statuses(registry, job_ids, memory_history)
            print(f"snapshot {isoformat()}")
            print_statuses(statuses, args.json)
            memory_history.extend(statuses)
            if not args.no_record:
                for status in statuses:
                    append_history(history_path(registry), status)
            iteration += 1
            if args.iterations != 0 and iteration >= args.iterations:
                break
            time.sleep(args.interval)
    except KeyboardInterrupt:
        return 130
    return 0


def dependencies_complete(
    registry: Registry,
    job_id: str,
    states: dict[str, dict[str, Any]],
) -> tuple[bool, list[str]]:
    missing = dependency_blockers(registry, job_id, states)
    return not missing, missing


def command_next(registry: Registry, args: argparse.Namespace) -> int:
    statuses = collect_statuses(registry, registry.ordered_job_ids())
    states = {status["job_id"]: status for status in statuses}
    active_heavy = any(
        status["state"] in {"RUNNING", "STALLED"} and status["heavy"]
        for status in statuses
    )
    active_workers = sum(
        status["workers"]
        for status in statuses
        if status["session_active"] or status["process_active"]
    )
    rows: list[list[str]] = []
    for job_id in registry.ordered_job_ids():
        job = registry.get(job_id)
        status = states[job_id]
        if status["state"] not in {"PLANNED", "READY"}:
            continue
        if job.get("conditional") and status.get("conditional_active") is not True:
            continue
        ready, missing = dependencies_complete(registry, job_id, states)
        if not ready:
            continue
        blocked_reasons: list[str] = []
        if job["heavy"] and active_heavy:
            blocked_reasons.append("heavy job active")
        if active_workers + job["workers"] > registry.defaults["max_workers"]:
            blocked_reasons.append("worker cap")
        blocked = ", ".join(blocked_reasons) or "-"
        action = "validate" if status["state"] == "PLANNED" else "launch"
        if (
            job.get("manual_review_before_launch")
            and action == "launch"
            and validate_manual_readiness_receipt(registry, job_id)
        ):
            action = "review then launch"
        condition = status.get("conditional_reason") or "-"
        rows.append([job_id, action, blocked, condition])
    if rows:
        print(format_table(rows, ("job", "action", "blocked", "condition")))
    else:
        print("No dependency-ready jobs. The orchestrator never advances the DAG automatically.")
    return 0


def shell_wrapper(registry: Registry, job_id: str, resume: bool) -> str:
    environment = registry.job_environment(job_id, resume)
    exports = "\n".join(
        f"export {key}={shlex.quote(value)}" for key, value in environment.items()
    )
    run_log = registry.artifact(job_id, "run_log")
    exit_code = registry.artifact(job_id, "exit_code_file")
    lock_path = registry.lock_path(job_id)
    timeout_seconds = registry.get(job_id)["contract"]["job_timeout_seconds"]
    command = shlex.join(
        [
            "timeout",
            "--signal=TERM",
            "--kill-after=60s",
            f"{timeout_seconds}s",
            *registry.command(job_id),
        ]
    )
    return "\n".join(
        [
            "set -o pipefail",
            f"cd {shlex.quote(str(registry.repo_root))}",
            exports,
            f"{command} 2>&1 | tee -a {shlex.quote(str(run_log))}",
            "rc=${PIPESTATUS[0]}",
            f"printf '%s\\n' \"$rc\" > {shlex.quote(str(exit_code))}",
            f"rm -f {shlex.quote(str(lock_path))}",
            "exit \"$rc\"",
        ]
    )


def tmux_launch_command(registry: Registry, job_id: str, resume: bool) -> list[str]:
    wrapper = shell_wrapper(registry, job_id, resume)
    shell_command = f"bash -lc {shlex.quote(wrapper)}"
    return [
        "tmux",
        "new-session",
        "-d",
        "-s",
        registry.session_name(job_id),
        shell_command,
    ]


def command_print_launch(registry: Registry, args: argparse.Namespace) -> int:
    job_id = requested_job(args, allow_all=False)
    registry.get(job_id)
    subcommand = "resume" if args.resume else "launch"
    config = registry.display_path(registry.config_path)
    invocation = [
        "python3",
        registry.display_path(SCRIPT_PATH),
        "--config",
        config,
        subcommand,
        "--job",
        job_id,
        "--execute",
        "--ack-long-running",
        AUTHORIZED_ACK,
    ]
    print(f"job: {job_id}")
    print(f"config hash: {registry.config_hash(job_id)}")
    print(f"output: {registry.display_path(registry.output_dir(job_id))}")
    print(f"tmux session: {registry.session_name(job_id)}")
    print(f"dispatcher: {shlex.join(registry.command(job_id))}")
    if registry.get(job_id).get("manual_review_before_launch"):
        receipt_path = readiness_receipt_path(registry, job_id)
        errors = validate_manual_readiness_receipt(registry, job_id)
        receipt_display = registry.display_path(receipt_path) if receipt_path else "not configured"
        receipt_state = "valid" if not errors else "required before launch"
        print(f"manual review receipt: {receipt_display} ({receipt_state})")
    print(f"authorized invocation: {shlex.join(invocation)}")
    print(f"tmux preview: {shlex.join(tmux_launch_command(registry, job_id, args.resume))}")
    return 0


def verify_authorization(args: argparse.Namespace) -> None:
    if not args.execute:
        raise OrchestratorError("refusing to launch without --execute")
    if args.ack != AUTHORIZED_ACK:
        raise OrchestratorError(f"--ack-long-running must exactly equal {AUTHORIZED_ACK}")


def validate_manual_readiness_receipt(registry: Registry, job_id: str) -> list[str]:
    job = registry.get(job_id)
    if not job.get("manual_review_before_launch"):
        return []
    path = readiness_receipt_path(registry, job_id)
    if path is None:
        return ["manual readiness receipt path is not configured"]
    receipt = read_json(path)
    if receipt is None:
        return [f"missing or invalid manual readiness receipt: {registry.display_path(path)}"]
    required_keys = {
        "job_id",
        "config_hash",
        "status",
        "reviewer",
        "reviewed_at",
        "reviewed_dependency_hashes",
    }
    errors: list[str] = []
    if set(receipt) != required_keys:
        errors.append("manual readiness receipt has an invalid schema")
    specification = job["manual_readiness_receipt"]
    exact_fields = {
        "job_id": job_id,
        "config_hash": registry.config_hash(job_id),
        "status": specification["required_status"],
    }
    for key, required in exact_fields.items():
        if receipt.get(key) != required:
            errors.append(f"manual readiness receipt {key} does not match the locked plan")
    current_hashes: dict[str, str] = {}
    for dependency in specification["reviewed_dependencies"]:
        marker = registry.artifact(dependency, "success_marker")
        if not marker.is_file():
            errors.append(
                "manual readiness receipt dependency marker is missing: "
                f"{registry.display_path(marker)}"
            )
            continue
        current_hashes[dependency] = sha256_file(marker)
    if receipt.get("reviewed_dependency_hashes") != current_hashes:
        errors.append(
            "manual readiness receipt reviewed_dependency_hashes do not match current "
            "dependency success markers"
        )
    reviewer = receipt.get("reviewer")
    if not isinstance(reviewer, str) or not reviewer.strip():
        errors.append("manual readiness receipt reviewer must be non-empty")
    if parse_timestamp(receipt.get("reviewed_at")) is None:
        errors.append("manual readiness receipt reviewed_at must be an ISO-8601 timestamp")
    return errors


def verify_launch_state(
    registry: Registry,
    job_id: str,
    resume: bool,
    statuses: dict[str, dict[str, Any]],
) -> None:
    status = statuses[job_id]
    registry.external_inputs(job_id, require=True)
    if status["state"] == "COMPLETE":
        raise OrchestratorError(f"{job_id} is already complete")
    if status["session_active"] or status["process_active"]:
        raise OrchestratorError(f"{job_id} is already running in {status['session']}")
    if resume:
        if status["state"] not in {"PARTIAL", "FAILED", "STALLED"}:
            raise OrchestratorError(f"{job_id} has no interrupted run to resume; use launch")
    elif status["state"] != "READY":
        raise OrchestratorError(
            f"{job_id} is {status['state']}; validate it first or use resume for partial work"
        )
    ready, missing = dependencies_complete(registry, job_id, statuses)
    if not ready:
        raise OrchestratorError(f"required dependencies are incomplete: {', '.join(missing)}")
    readiness_errors = validate_manual_readiness_receipt(registry, job_id)
    if readiness_errors:
        raise OrchestratorError("manual review gate failed: " + "; ".join(readiness_errors))


def verify_capacity(
    registry: Registry,
    job_id: str,
    statuses: dict[str, dict[str, Any]],
    sessions: set[str],
) -> None:
    job = registry.get(job_id)
    prefix = f"{registry.defaults['tmux_prefix']}-{job_id}-"
    duplicates = sorted(session for session in sessions if session.startswith(prefix))
    if duplicates:
        raise OrchestratorError(f"duplicate tmux session for {job_id}: {', '.join(duplicates)}")
    if job["heavy"]:
        active = [
            status["job_id"]
            for status in statuses.values()
            if status["heavy"] and (status["session_active"] or status["process_active"])
        ]
        if active:
            raise OrchestratorError(
                "maximum one heavy job is enforced; currently active: " + ", ".join(active)
            )
    active_workers = sum(
        status["workers"]
        for status in statuses.values()
        if status["session_active"] or status["process_active"]
    )
    if active_workers + job["workers"] > registry.defaults["max_workers"]:
        raise OrchestratorError(
            "aggregate worker cap exceeded: "
            f"{active_workers} active + {job['workers']} requested > {registry.defaults['max_workers']}"
        )


def acquire_lock(registry: Registry, job_id: str, resume: bool) -> None:
    lock_path = registry.lock_path(job_id)
    lock_path.parent.mkdir(parents=True, exist_ok=True)
    if resume and lock_path.exists():
        lock_path.unlink()
    payload = {
        "job_id": job_id,
        "config_hash": registry.config_hash(job_id),
        "session": registry.session_name(job_id),
        "created_at": isoformat(),
        "resume": resume,
    }
    try:
        descriptor = os.open(lock_path, os.O_CREAT | os.O_EXCL | os.O_WRONLY, 0o600)
    except FileExistsError as error:
        raise OrchestratorError(f"lock already exists: {registry.display_path(lock_path)}") from error
    with os.fdopen(descriptor, "w", encoding="utf-8") as handle:
        json.dump(payload, handle, sort_keys=True)
        handle.write("\n")


def command_launch(registry: Registry, args: argparse.Namespace, resume: bool) -> int:
    verify_authorization(args)
    job_id = requested_job(args, allow_all=False)
    registry.get(job_id)
    sessions = tmux_sessions()
    statuses_list = collect_statuses(registry, registry.ordered_job_ids())
    statuses = {status["job_id"]: status for status in statuses_list}
    verify_launch_state(registry, job_id, resume, statuses)
    verify_capacity(registry, job_id, statuses, sessions)
    registry.persist_external_inputs(job_id)
    if shutil.which("tmux") is None:
        raise OrchestratorError("tmux is required for launch and resume")
    if shutil.which("timeout") is None:
        raise OrchestratorError("GNU timeout is required to enforce the job timeout")

    output_dir = registry.output_dir(job_id)
    output_dir.mkdir(parents=True, exist_ok=True)
    acquire_lock(registry, job_id, resume)
    command = tmux_launch_command(registry, job_id, resume)
    result = subprocess.run(command, cwd=registry.repo_root, check=False, capture_output=True, text=True)
    if result.returncode != 0:
        registry.lock_path(job_id).unlink(missing_ok=True)
        detail = result.stderr.strip() or result.stdout.strip() or f"exit {result.returncode}"
        raise OrchestratorError(f"tmux launch failed: {detail}")

    event = {
        "event": "resume" if resume else "launch",
        "recorded_at": isoformat(),
        "job_id": job_id,
        "config_hash": registry.config_hash(job_id),
        "session": registry.session_name(job_id),
        "output_dir": registry.display_path(output_dir),
        "workers": registry.get(job_id)["workers"],
    }
    append_history(history_path(registry), event)
    print(f"Started {job_id} in tmux session {registry.session_name(job_id)}.")
    print(f"Output: {registry.display_path(output_dir)}")
    print("No downstream job will start automatically.")
    return 0


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--config", type=Path, default=DEFAULT_CONFIG)
    subparsers = parser.add_subparsers(dest="command", required=True)

    plan = subparsers.add_parser(
        "plan",
        help="Print the immutable job graph, contracts, and configuration hashes.",
    )
    plan.add_argument("job_positional", nargs="?")
    plan.add_argument("--job")
    plan.add_argument("--json", action="store_true")

    validate = subparsers.add_parser("validate", help="Validate the manifest and dispatcher dry runs.")
    validate.add_argument("job_positional", nargs="?")
    validate.add_argument("--job")
    validate.add_argument("--structural-only", action="store_true")

    status = subparsers.add_parser("status", help="Read checkpoint-first status without launching work.")
    status.add_argument("job_positional", nargs="?")
    status.add_argument("--job")
    status.add_argument("--json", action="store_true")
    status.add_argument("--no-record", action="store_true", help="Do not append this snapshot to history.")

    watch = subparsers.add_parser("watch", help="Poll checkpoint-first status; never advance the DAG.")
    watch.add_argument("job_positional", nargs="?")
    watch.add_argument("--job")
    watch.add_argument(
        "--interval",
        type=float,
        default=300.0,
        help="Polling interval in seconds (default: 300).",
    )
    watch.add_argument("--iterations", type=int, default=0, help="Zero watches until interrupted.")
    watch.add_argument("--json", action="store_true")
    watch.add_argument("--no-record", action="store_true", help="Do not append snapshots to history.")

    next_command = subparsers.add_parser("next", help="List dependency-ready jobs without launching.")

    print_launch = subparsers.add_parser("print-launch", help="Print the guarded launch command.")
    print_launch.add_argument("job_positional", nargs="?")
    print_launch.add_argument("--job")
    print_launch.add_argument("--resume", action="store_true")

    for name in ("launch", "resume"):
        launch = subparsers.add_parser(name, help=f"Explicitly {name} one job in tmux.")
        launch.add_argument("job_positional", nargs="?")
        launch.add_argument("--job")
        launch.add_argument("--execute", action="store_true")
        launch.add_argument("--ack-long-running", "--ack", dest="ack")
    return parser


def dispatch(registry: Registry, args: argparse.Namespace) -> int:
    commands = {
        "plan": command_plan,
        "validate": command_validate,
        "status": command_status,
        "watch": command_watch,
        "next": command_next,
        "print-launch": command_print_launch,
    }
    if args.command == "launch":
        return command_launch(registry, args, resume=False)
    if args.command == "resume":
        return command_launch(registry, args, resume=True)
    return commands[args.command](registry, args)


def main(argv: Sequence[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    if args.command == "watch":
        if args.interval <= 0:
            parser.error("--interval must be positive")
        if args.iterations < 0:
            parser.error("--iterations must be zero or positive")
    try:
        registry = Registry(args.config)
        return dispatch(registry, args)
    except OrchestratorError as error:
        parser.exit(2, f"error: {error}\n")


if __name__ == "__main__":
    raise SystemExit(main())
