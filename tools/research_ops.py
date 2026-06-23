#!/usr/bin/env python3
"""Small project-editable research provenance helper."""

from __future__ import annotations

import argparse
import datetime as dt
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Any

try:
    import yaml
except ImportError:  # pragma: no cover - exercised only in minimal envs
    yaml = None


DEFAULT_RESEARCH_YAML = """project: null
paths:
  manuscript: manuscript
  figures: figures
  tables: tables
  research: research
  runs: research/runs
  reports: research/reports
  log: research/log.jsonl
  evidence: research/evidence.jsonl
  claims: research/claim-ledger.yaml
  references: manuscript/references.bib
execution:
  python: python3
  r: Rscript
  require_clean_git_for_final: false
paper:
  engine: quarto
  main: manuscript/paper.qmd
checks:
  require_run_manifest_for_result_claims: true
  require_evidence_for_literature_claims: true
  fail_on_stale_figures: true
  fail_on_untracked_generated_outputs: true
human_checkpoints:
  - change_hypothesis
  - change_construct
  - change_outcome
  - change_exclusion_rule
  - add_causal_language
  - infer_sensitive_identity
"""

NETWORK_CONFIG = """edges:
  path: null
  source_col: source
  target_col: target
  time_col: null
  weight_col: null
  directed: true
nodes:
  path: null
  id_col: id
  actor_type_col: null
analysis:
  expected_window_start: null
  expected_window_end: null
  allowed_actor_types: []
  known_bad_dates: []
  duplicate_policy: document
report:
  output: research/reports/network-audit-{date}.md
"""

MEASUREMENT_CONFIG = """scores:
  path: null
  variables: []
  group_col: null
  text_col: null
measurement:
  construct_names: []
  threshold_cols: []
  model_version_cols: []
  prompt_version_cols: []
  allowed_score_min: null
  allowed_score_max: null
report:
  output: research/reports/measurement-audit-{date}.md
"""

DISTILLATION_CONFIG = """workflow_name: null
reusable_outcome: null
variable_inputs: []
invariant_steps: []
project_specific_assumptions: []
guardrails_from_failed_attempts: []
human_checkpoints: []
redactions_required: []
proposed_skill_change: null
tests: []
file_count_impact:
  added: 0
  modified: 0
"""


def require_yaml() -> None:
    if yaml is None:
        raise SystemExit("PyYAML is required for YAML-backed research operations.")


def now_iso() -> str:
    return dt.datetime.now(dt.timezone.utc).isoformat(timespec="seconds").replace("+00:00", "Z")


def today() -> str:
    return dt.date.today().isoformat()


def slugify(value: str) -> str:
    slug = re.sub(r"[^a-zA-Z0-9]+", "-", value.strip().lower()).strip("-")
    return slug or "analysis"


def project_root(args: argparse.Namespace) -> Path:
    return Path(args.project_root).expanduser().resolve()


def load_config(root: Path) -> dict[str, Any]:
    require_yaml()
    path = root / "research.yaml"
    if not path.exists():
        return yaml.safe_load(DEFAULT_RESEARCH_YAML)
    data = yaml.safe_load(path.read_text()) or {}
    default = yaml.safe_load(DEFAULT_RESEARCH_YAML)
    return merge_dicts(default, data)


def merge_dicts(base: dict[str, Any], override: dict[str, Any]) -> dict[str, Any]:
    merged = dict(base)
    for key, value in override.items():
        if isinstance(value, dict) and isinstance(merged.get(key), dict):
            merged[key] = merge_dicts(merged[key], value)
        else:
            merged[key] = value
    return merged


def cfg_path(config: dict[str, Any], key: str) -> str:
    return str(config.get("paths", {}).get(key, ""))


def resolve(root: Path, value: str | Path) -> Path:
    path = Path(value).expanduser()
    return path if path.is_absolute() else root / path


def rel(root: Path, path: Path) -> str:
    try:
        return path.resolve().relative_to(root).as_posix()
    except ValueError:
        return path.as_posix()


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def read_jsonl_ids(path: Path) -> set[str]:
    ids: set[str] = set()
    if not path.exists():
        return ids
    for line in path.read_text().splitlines():
        if not line.strip():
            continue
        try:
            record = json.loads(line)
        except json.JSONDecodeError:
            continue
        if isinstance(record, dict) and record.get("id"):
            ids.add(str(record["id"]))
    return ids


def git_value(root: Path, args: list[str]) -> str | None:
    try:
        result = subprocess.run(
            ["git", *args],
            cwd=root,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            check=True,
        )
    except (OSError, subprocess.CalledProcessError):
        return None
    return result.stdout.strip()


def git_dirty(root: Path) -> bool | None:
    status = git_value(root, ["status", "--short"])
    if status is None:
        return None
    return bool(status)


def next_id(path: Path, prefix: str) -> str:
    count = 1
    if path.exists():
        count += sum(1 for line in path.read_text().splitlines() if line.strip())
    return f"{prefix}-{today().replace('-', '')}-{count:03d}"


def parse_key_value(values: list[str]) -> dict[str, Any]:
    parsed: dict[str, Any] = {}
    for item in values:
        if "=" not in item:
            raise SystemExit(f"Expected KEY=VALUE, got {item!r}")
        key, raw = item.split("=", 1)
        try:
            parsed[key] = json.loads(raw)
        except json.JSONDecodeError:
            parsed[key] = raw
    return parsed


def artifact_record(root: Path, value: str, role: str, command: str | None = None) -> dict[str, Any]:
    path = resolve(root, value)
    record: dict[str, Any] = {
        "path": rel(root, path),
        "hash": None,
        "type": path.suffix.lstrip(".") or "file",
        "role": role,
        "generating_command": command,
    }
    if path.exists() and path.is_file():
        record["hash"] = sha256_file(path)
    else:
        record["missing"] = True
    return record


def write_json(path: Path, data: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(data, indent=2, sort_keys=True) + "\n")


def append_jsonl(path: Path, record: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as handle:
        handle.write(json.dumps(record, sort_keys=True) + "\n")


def cmd_init_project(args: argparse.Namespace) -> int:
    root = project_root(args)
    root.mkdir(parents=True, exist_ok=True)
    research_yaml = root / "research.yaml"
    if not research_yaml.exists() or args.force:
        research_yaml.write_text(DEFAULT_RESEARCH_YAML.replace("project: null", f"project: {root.name}"))

    config = load_config(root)
    for key in ("research", "runs", "reports"):
        value = cfg_path(config, key)
        if value:
            resolve(root, value).mkdir(parents=True, exist_ok=True)
    (root / "research" / "skill-config").mkdir(parents=True, exist_ok=True)
    (root / "tools").mkdir(parents=True, exist_ok=True)

    project_tool = root / "tools" / "research_ops.py"
    source = Path(__file__).resolve()
    if source != project_tool.resolve() and (not project_tool.exists() or args.force):
        shutil.copy2(source, project_tool)

    templates = {
        root / "research" / "skill-config" / "network-audit.yaml": NETWORK_CONFIG,
        root / "research" / "skill-config" / "measurement-audit.yaml": MEASUREMENT_CONFIG,
        root / "research" / "skill-config" / "distillation.yaml": DISTILLATION_CONFIG,
    }
    for path, content in templates.items():
        if not path.exists() or args.force:
            path.write_text(content)

    print(f"Initialized research project at {root}")
    print("Editable project helper: tools/research_ops.py")
    return 0


def cmd_hash(args: argparse.Namespace) -> int:
    root = project_root(args)
    records = []
    for value in args.paths:
        path = resolve(root, value)
        if not path.exists() or not path.is_file():
            records.append({"path": value, "error": "missing-or-not-file"})
            continue
        records.append({"path": rel(root, path), "sha256": sha256_file(path)})
    if args.json:
        print(json.dumps(records, indent=2, sort_keys=True))
    else:
        for record in records:
            if "sha256" in record:
                print(f"{record['sha256']}  {record['path']}")
            else:
                print(f"ERROR {record['path']}: {record['error']}")
    return 1 if any("error" in record for record in records) else 0


def cmd_log_analysis(args: argparse.Namespace) -> int:
    root = project_root(args)
    config = load_config(root)
    log_path = resolve(root, cfg_path(config, "log"))
    record = {
        "id": args.id or next_id(log_path, "log"),
        "timestamp": now_iso(),
        "title": args.title,
        "task_type": args.task_type,
        "status": args.status,
        "inputs": args.input,
        "commands_or_files": args.command,
        "summary": args.summary,
        "next_action": args.next_action,
        "links": {
            "run_ids": args.run_id,
            "evidence_ids": args.evidence_id,
            "claim_ids": args.claim_id,
            "artifacts": args.artifact,
            "reports": args.report,
        },
    }
    append_jsonl(log_path, record)
    print(f"Appended {record['id']} to {rel(root, log_path)}")
    return 0


def cmd_add_evidence(args: argparse.Namespace) -> int:
    root = project_root(args)
    config = load_config(root)
    evidence_path = resolve(root, cfg_path(config, "evidence"))
    record = {
        "id": args.id or next_id(evidence_path, "ev"),
        "source_type": args.source_type,
        "citation_key": args.citation_key,
        "doi_or_url": args.doi_or_url,
        "retrieved_at": args.retrieved_at or now_iso(),
        "locator": args.locator,
        "evidence_status": args.evidence_status,
        "claim_supported": args.claim_supported,
        "paraphrase": args.paraphrase,
        "limitations": args.limitations,
    }
    append_jsonl(evidence_path, record)
    print(f"Appended {record['id']} to {rel(root, evidence_path)}")
    return 0


def unique_run_id(root: Path, runs_dir: Path, title: str) -> str:
    base = f"run-{today().replace('-', '')}-{slugify(title)}"
    index = 1
    while (runs_dir / f"{base}-{index:03d}" / "run-manifest.json").exists():
        index += 1
    return f"{base}-{index:03d}"


def cmd_capture_run(args: argparse.Namespace) -> int:
    root = project_root(args)
    config = load_config(root)
    runs_dir = resolve(root, cfg_path(config, "runs"))
    run_id = args.run_id or unique_run_id(root, runs_dir, args.title)
    run_dir = runs_dir / run_id
    command_for_outputs = args.command[-1] if args.command else None
    manifest = {
        "run_id": run_id,
        "experiment_id": args.experiment_id,
        "started_at": args.started_at or now_iso(),
        "finished_at": args.finished_at or now_iso(),
        "status": args.status,
        "git_commit": git_value(root, ["rev-parse", "HEAD"]),
        "git_dirty": git_dirty(root),
        "commands": args.command,
        "parameters": parse_key_value(args.param),
        "seeds": args.seed,
        "input_artifacts": [artifact_record(root, value, "input") for value in args.input],
        "output_artifacts": [
            artifact_record(root, value, "output", command_for_outputs) for value in args.output
        ],
        "environment": {
            "python": sys.version.split()[0],
            "cwd": str(root),
        },
        "software_versions": {},
        "checks": args.check,
    }
    write_json(run_dir / "run-manifest.json", manifest)
    print(f"Wrote {rel(root, run_dir / 'run-manifest.json')}")
    return 0


def validate_manifest(root: Path, manifest_path: Path) -> tuple[list[str], list[str]]:
    errors: list[str] = []
    warnings: list[str] = []
    data = json.loads(manifest_path.read_text())
    required = ["run_id", "status", "commands", "input_artifacts", "output_artifacts"]
    for key in required:
        if key not in data:
            errors.append(f"missing required field: {key}")
    if data.get("status") not in {"success", "failed", "partial"}:
        errors.append("status must be success, failed, or partial")
    for group in ("input_artifacts", "output_artifacts"):
        for artifact in data.get(group, []):
            path_value = artifact.get("path")
            expected = artifact.get("hash")
            if not path_value:
                errors.append(f"{group} entry missing path")
                continue
            path = resolve(root, path_value)
            if not path.exists():
                errors.append(f"artifact missing: {path_value}")
                continue
            if expected:
                actual = sha256_file(path)
                if actual != expected:
                    errors.append(f"artifact hash mismatch: {path_value}")
            else:
                warnings.append(f"artifact has no recorded hash: {path_value}")
    return errors, warnings


def cmd_validate_run(args: argparse.Namespace) -> int:
    root = project_root(args)
    manifest = resolve(root, args.manifest)
    errors, warnings = validate_manifest(root, manifest)
    for warning in warnings:
        print(f"WARNING: {warning}")
    for error in errors:
        print(f"ERROR: {error}")
    if errors:
        return 1
    print(f"Run manifest valid: {rel(root, manifest)}")
    return 0


def load_claims(path: Path) -> list[dict[str, Any]]:
    require_yaml()
    if not path.exists():
        return []
    data = yaml.safe_load(path.read_text()) or []
    if not isinstance(data, list):
        raise SystemExit(f"{path} must contain a YAML list")
    return data


def run_ids(root: Path, config: dict[str, Any]) -> set[str]:
    runs = resolve(root, cfg_path(config, "runs"))
    found: set[str] = set()
    if not runs.exists():
        return found
    for manifest in runs.glob("*/run-manifest.json"):
        try:
            found.add(json.loads(manifest.read_text()).get("run_id") or manifest.parent.name)
        except json.JSONDecodeError:
            continue
    return found


def run_manifests(root: Path, config: dict[str, Any]) -> list[Path]:
    runs = resolve(root, cfg_path(config, "runs"))
    if not runs.exists():
        return []
    return sorted(runs.glob("*/run-manifest.json"))


def validate_all_runs(root: Path, config: dict[str, Any]) -> tuple[list[str], list[str]]:
    errors: list[str] = []
    warnings: list[str] = []
    manifests = run_manifests(root, config)
    if not manifests:
        errors.append(f"no run manifests found under {cfg_path(config, 'runs')}")
        return errors, warnings
    for manifest in manifests:
        manifest_errors, manifest_warnings = validate_manifest(root, manifest)
        manifest_label = rel(root, manifest)
        errors.extend(f"{manifest_label}: {error}" for error in manifest_errors)
        warnings.extend(f"{manifest_label}: {warning}" for warning in manifest_warnings)
    return errors, warnings


def untracked_paths(root: Path, paths: list[str]) -> list[str]:
    existing = [value for value in paths if value and resolve(root, value).exists()]
    if not existing:
        return []
    output = git_value(root, ["ls-files", "--others", "--exclude-standard", "--", *existing])
    if output is None:
        return []
    return sorted(line for line in output.splitlines() if line.strip())


def check_untracked_generated_outputs(root: Path, config: dict[str, Any]) -> list[str]:
    paths = config.get("paths", {})
    package_paths = [
        paths.get("figures", ""),
        paths.get("tables", ""),
        paths.get("research", ""),
        "research.yaml",
        "tools/research_ops.py",
        "README.md",
        "scripts/README.md",
        "REPRODUCIBILITY.md",
    ]
    untracked = untracked_paths(root, [str(path) for path in package_paths])
    return [f"untracked package artifact: {path}" for path in untracked]


def check_claims(root: Path, allow_missing: bool = False) -> tuple[list[str], list[str]]:
    config = load_config(root)
    claims_path = resolve(root, cfg_path(config, "claims"))
    if not claims_path.exists():
        if allow_missing:
            return [], [f"claim ledger missing: {rel(root, claims_path)}"]
        return [f"claim ledger missing: {rel(root, claims_path)}"], []
    claims = load_claims(claims_path)
    evidence_ids = read_jsonl_ids(resolve(root, cfg_path(config, "evidence")))
    known_runs = run_ids(root, config)
    errors: list[str] = []
    warnings: list[str] = []
    checks = config.get("checks", {})
    for index, claim in enumerate(claims, start=1):
        if not isinstance(claim, dict):
            errors.append(f"claim {index} is not a mapping")
            continue
        claim_id = str(claim.get("id") or f"claim at index {index}")
        if not claim.get("statement"):
            errors.append(f"{claim_id}: missing statement")
        status = str(claim.get("status") or "")
        if status not in {"supported", "needs-evidence", "too-strong", "retired"}:
            errors.append(f"{claim_id}: invalid status")
        claim_type = str(claim.get("claim_type") or "").lower()
        if status == "retired":
            continue
        if claim_type == "empirical" and checks.get("require_run_manifest_for_result_claims", True):
            if not claim.get("run_ids"):
                errors.append(f"{claim_id}: empirical claim has no run_ids")
            if not claim.get("artifact_selectors"):
                errors.append(f"{claim_id}: empirical claim has no artifact_selectors")
        if claim_type == "literature" and checks.get("require_evidence_for_literature_claims", True):
            if not claim.get("evidence_ids") and not claim.get("citations"):
                errors.append(f"{claim_id}: literature claim has no evidence_ids or citations")
        for evidence_id in claim.get("evidence_ids") or []:
            if evidence_ids and evidence_id not in evidence_ids:
                errors.append(f"{claim_id}: unknown evidence_id {evidence_id}")
        for run_id in claim.get("run_ids") or []:
            if run_id not in known_runs:
                errors.append(f"{claim_id}: unknown run_id {run_id}")
        for selector in claim.get("artifact_selectors") or []:
            path_value = selector.get("path") if isinstance(selector, dict) else selector
            if path_value and not resolve(root, path_value).exists():
                warnings.append(f"{claim_id}: artifact selector path not found: {path_value}")
    return errors, warnings


def cmd_check_claims(args: argparse.Namespace) -> int:
    root = project_root(args)
    errors, warnings = check_claims(root, args.allow_missing)
    for warning in warnings:
        print(f"WARNING: {warning}")
    for error in errors:
        print(f"ERROR: {error}")
    if errors:
        return 1
    print("Claim ledger checks passed")
    return 0


def bib_keys(path: Path) -> set[str]:
    if not path.exists():
        return set()
    return set(re.findall(r"@\w+\s*\{\s*([^,\s]+)", path.read_text(errors="ignore")))


def cited_keys(path: Path) -> set[str]:
    if not path.exists():
        return set()
    text = path.read_text(errors="ignore")
    crossref_prefixes = ("fig-", "tbl-", "eq-", "sec-", "app-", "lst-")
    keys = set(re.findall(r"(?<![\w.%-])@([A-Za-z0-9_:.+-]+)", text))
    return {key for key in keys if not key.startswith(crossref_prefixes)}


def cmd_check_paper(args: argparse.Namespace) -> int:
    root = project_root(args)
    config = load_config(root)
    errors, warnings = check_claims(root, allow_missing=args.allow_missing_claims)
    checks = config.get("checks", {})
    if checks.get("fail_on_stale_figures", True):
        run_errors, run_warnings = validate_all_runs(root, config)
        errors.extend(run_errors)
        warnings.extend(run_warnings)
    if checks.get("fail_on_untracked_generated_outputs", True):
        errors.extend(check_untracked_generated_outputs(root, config))
    paper_main = resolve(root, config.get("paper", {}).get("main", "manuscript/paper.qmd"))
    refs = resolve(root, cfg_path(config, "references"))
    if paper_main.exists() and refs.exists():
        missing = sorted(cited_keys(paper_main) - bib_keys(refs))
        for key in missing:
            errors.append(f"citation key missing from bibliography: {key}")
    elif paper_main.exists() and not refs.exists():
        warnings.append(f"bibliography missing: {rel(root, refs)}")
    else:
        warnings.append(f"paper main missing: {rel(root, paper_main)}")
    for warning in warnings:
        print(f"WARNING: {warning}")
    for error in errors:
        print(f"ERROR: {error}")
    if errors:
        return 1
    print("Paper checks passed")
    return 0


def cmd_test_skills(args: argparse.Namespace) -> int:
    with tempfile.TemporaryDirectory(prefix="research-skill-test-") as temp:
        root = Path(temp) / "project"
        subprocess.run(
            [sys.executable, __file__, "init-project", "--project-root", str(root)],
            check=True,
            stdout=subprocess.PIPE,
            text=True,
        )
        (root / "data").mkdir()
        (root / "figures").mkdir()
        (root / "data" / "input.csv").write_text("id,value\n1,2\n")
        (root / "figures" / "result.csv").write_text("metric,value\nmean,2\n")
        subprocess.run(
            [
                sys.executable,
                __file__,
                "capture-run",
                "--project-root",
                str(root),
                "--title",
                "toy analysis",
                "--command",
                "python analysis.py",
                "--input",
                "data/input.csv",
                "--output",
                "figures/result.csv",
                "--status",
                "success",
            ],
            check=True,
            stdout=subprocess.PIPE,
            text=True,
        )
        subprocess.run(
            [
                sys.executable,
                __file__,
                "add-evidence",
                "--project-root",
                str(root),
                "--source-type",
                "paper",
                "--citation-key",
                "example2026",
                "--evidence-status",
                "metadata-only",
                "--paraphrase",
                "Toy evidence record.",
            ],
            check=True,
            stdout=subprocess.PIPE,
            text=True,
        )
        evidence_id = json.loads((root / "research" / "evidence.jsonl").read_text().splitlines()[0])["id"]
        manifest = next((root / "research" / "runs").glob("*/run-manifest.json"))
        run_id = json.loads(manifest.read_text())["run_id"]
        claims = [
            {
                "id": "claim-001",
                "location": "results",
                "statement": "The toy output reports a mean value.",
                "claim_type": "empirical",
                "status": "supported",
                "run_ids": [run_id],
                "evidence_ids": [],
                "artifact_selectors": ["figures/result.csv"],
                "citations": [],
                "scope": "descriptive",
                "caveats": [],
            },
            {
                "id": "claim-002",
                "location": "background",
                "statement": "A toy source is cited.",
                "claim_type": "literature",
                "status": "supported",
                "run_ids": [],
                "evidence_ids": [evidence_id],
                "artifact_selectors": [],
                "citations": ["example2026"],
                "scope": "background",
                "caveats": [],
            },
        ]
        require_yaml()
        (root / "research" / "claim-ledger.yaml").write_text(yaml.safe_dump(claims, sort_keys=False))
        result = subprocess.run(
            [sys.executable, __file__, "check-claims", "--project-root", str(root)],
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
        )
        if result.returncode != 0:
            print(result.stdout)
            return result.returncode
    print("Research skill self-test passed")
    return 0


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)

    init = sub.add_parser("init-project", help="create editable project research files")
    init.add_argument("--project-root", default=".")
    init.add_argument("--force", action="store_true")
    init.set_defaults(func=cmd_init_project)

    hash_cmd = sub.add_parser("hash", help="compute sha256 hashes")
    hash_cmd.add_argument("paths", nargs="+")
    hash_cmd.add_argument("--project-root", default=".")
    hash_cmd.add_argument("--json", action="store_true")
    hash_cmd.set_defaults(func=cmd_hash)

    log = sub.add_parser("log-analysis", help="append a research log entry")
    log.add_argument("--project-root", default=".")
    log.add_argument("--id")
    log.add_argument("--title", required=True)
    log.add_argument("--task-type", default="exploratory")
    log.add_argument("--status", default="done", choices=["open", "done", "blocked", "abandoned"])
    log.add_argument("--input", action="append", default=[])
    log.add_argument("--command", action="append", default=[])
    log.add_argument("--summary", required=True)
    log.add_argument("--next-action")
    log.add_argument("--run-id", action="append", default=[])
    log.add_argument("--evidence-id", action="append", default=[])
    log.add_argument("--claim-id", action="append", default=[])
    log.add_argument("--artifact", action="append", default=[])
    log.add_argument("--report", action="append", default=[])
    log.set_defaults(func=cmd_log_analysis)

    evidence = sub.add_parser("add-evidence", help="append evidence JSONL")
    evidence.add_argument("--project-root", default=".")
    evidence.add_argument("--id")
    evidence.add_argument("--source-type", required=True)
    evidence.add_argument("--citation-key")
    evidence.add_argument("--doi-or-url")
    evidence.add_argument("--retrieved-at")
    evidence.add_argument("--locator")
    evidence.add_argument("--evidence-status", default="unverified")
    evidence.add_argument("--claim-supported")
    evidence.add_argument("--paraphrase")
    evidence.add_argument("--limitations")
    evidence.set_defaults(func=cmd_add_evidence)

    capture = sub.add_parser("capture-run", help="write a run manifest")
    capture.add_argument("--project-root", default=".")
    capture.add_argument("--title", default="analysis")
    capture.add_argument("--run-id")
    capture.add_argument("--experiment-id")
    capture.add_argument("--started-at")
    capture.add_argument("--finished-at")
    capture.add_argument("--status", default="success", choices=["success", "failed", "partial"])
    capture.add_argument("--command", action="append", default=[])
    capture.add_argument("--param", action="append", default=[])
    capture.add_argument("--seed", action="append", default=[])
    capture.add_argument("--input", action="append", default=[])
    capture.add_argument("--output", action="append", default=[])
    capture.add_argument("--check", action="append", default=[])
    capture.set_defaults(func=cmd_capture_run)

    validate = sub.add_parser("validate-run", help="validate a run manifest")
    validate.add_argument("manifest")
    validate.add_argument("--project-root", default=".")
    validate.set_defaults(func=cmd_validate_run)

    claims = sub.add_parser("check-claims", help="check claim-ledger.yaml")
    claims.add_argument("--project-root", default=".")
    claims.add_argument("--allow-missing", action="store_true")
    claims.set_defaults(func=cmd_check_claims)

    paper = sub.add_parser("check-paper", help="check claims and basic citation consistency")
    paper.add_argument("--project-root", default=".")
    paper.add_argument("--allow-missing-claims", action="store_true")
    paper.set_defaults(func=cmd_check_paper)

    test = sub.add_parser("test-skills", help="run a toy workflow test")
    test.set_defaults(func=cmd_test_skills)

    return parser


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    return int(args.func(args))


if __name__ == "__main__":
    raise SystemExit(main())
