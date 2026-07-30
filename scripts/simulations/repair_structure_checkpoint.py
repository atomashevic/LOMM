#!/usr/bin/env python3
"""Repair structure-job rows serialized with the error-readout column order."""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import os
import re
import shutil
import stat
import tempfile
from collections import Counter, defaultdict
from pathlib import Path


VALID_STATUSES = {"ok", "error", "timeout"}
ERROR_FIRST_SUFFIX = (
    "status",
    "error_message",
    "structure_fit_status",
    "structure_hash",
    "structure_fit_runtime_sec",
    "readout_runtime_sec",
    "runtime_sec",
    "completed_within_900",
    "n_dim_hat",
    "n_dim_exact",
    "assignment_accuracy",
    "adjusted_rand_index",
    "variation_information",
    "true_membership",
    "estimated_membership",
    "score_cor_common",
    "score_nrmse_common",
    "n_embed_best",
    "tefi_best",
    "native_expand_n_dim",
    "native_expand_assignment_accuracy",
    "native_expand_status",
    "native_expand_error_message",
    "correlation_louvain_095_n_dim",
    "correlation_louvain_095_assignment_accuracy",
    "correlation_louvain_095_status",
    "correlation_louvain_095_error_message",
)


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def read_checkpoint(path: Path) -> tuple[list[str], list[list[str]]]:
    with path.open(newline="", encoding="utf-8") as handle:
        reader = csv.reader(handle)
        header = next(reader)
        rows = list(reader)
    if not header or any(len(row) != len(header) for row in rows):
        raise ValueError("Checkpoint has an empty header or inconsistent row widths.")
    if len(header) != len(set(header)):
        raise ValueError("Checkpoint header contains duplicate columns.")
    return header, rows


def error_first_order(header: list[str]) -> list[str]:
    try:
        suffix_start = header.index("structure_fit_status")
    except ValueError as error:
        raise ValueError("Checkpoint omits structure_fit_status.") from error
    order = header[:suffix_start] + list(ERROR_FIRST_SUFFIX)
    if len(order) != len(header) or set(order) != set(header):
        raise ValueError("Checkpoint schema does not match the known structure-row schema.")
    return order


def validate_structure_contract(header: list[str], rows: list[list[str]]) -> None:
    index = {name: position for position, name in enumerate(header)}
    unit_keys = [row[index["unit_key"]] for row in rows]
    if any(not key for key in unit_keys) or len(unit_keys) != len(set(unit_keys)):
        raise ValueError("Repaired checkpoint has empty or duplicate unit keys.")
    groups: dict[str, list[list[str]]] = defaultdict(list)
    for row in rows:
        status = row[index["status"]]
        if status not in VALID_STATUSES:
            raise ValueError(f"Invalid repaired status: {status!r}.")
        groups[row[index["structure_fit_key"]]].append(row)
    expected_algorithms = {"louvain", "leiden", "walktrap"}
    for fit_key, group in groups.items():
        algorithms = {row[index["community_algorithm"]] for row in group}
        statuses = {row[index["structure_fit_status"]] for row in group}
        hashes = {
            row[index["structure_hash"]]
            for row in group
            if row[index["structure_hash"]]
        }
        if len(group) != 3 or algorithms != expected_algorithms or len(statuses) != 1:
            raise ValueError(f"Invalid structure-fit group: {fit_key}.")
        fit_status = next(iter(statuses))
        if fit_status not in VALID_STATUSES:
            raise ValueError(f"Invalid structure-fit status for {fit_key}.")
        if fit_status == "ok":
            if len(hashes) != 1 or not re.fullmatch(r"[0-9a-f]{64}", next(iter(hashes))):
                raise ValueError(f"Invalid structure hash for {fit_key}.")
        elif hashes:
            raise ValueError(f"Failed structure fit retained a hash for {fit_key}.")


def repair_rows(
    header: list[str], rows: list[list[str]]
) -> tuple[list[list[str]], list[int]]:
    canonical = {name: position for position, name in enumerate(header)}
    source_order = error_first_order(header)
    affected = [
        position
        for position, row in enumerate(rows)
        if row[canonical["status"]] not in VALID_STATUSES
    ]
    repaired = list(rows)
    for position in affected:
        source = dict(zip(source_order, rows[position], strict=True))
        repaired[position] = [source[name] for name in header]
    validate_structure_contract(header, repaired)
    return repaired, affected


def write_checkpoint(path: Path, header: list[str], rows: list[list[str]]) -> None:
    original_mode = stat.S_IMODE(path.stat().st_mode)
    descriptor, temporary_name = tempfile.mkstemp(
        prefix=f"{path.name}.", suffix=".tmp", dir=path.parent
    )
    try:
        with os.fdopen(descriptor, "w", newline="", encoding="utf-8") as handle:
            writer = csv.writer(handle, lineterminator="\n", quoting=csv.QUOTE_ALL)
            writer.writerow(header)
            writer.writerows(rows)
            handle.flush()
            os.fsync(handle.fileno())
        os.chmod(temporary_name, original_mode)
        os.replace(temporary_name, path)
    finally:
        if os.path.exists(temporary_name):
            os.unlink(temporary_name)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--checkpoint", type=Path, required=True)
    parser.add_argument("--expected-sha256", required=True)
    parser.add_argument("--expected-affected-rows", type=int, required=True)
    parser.add_argument("--expected-affected-panels", type=int, required=True)
    parser.add_argument("--backup", type=Path, required=True)
    parser.add_argument("--receipt", type=Path, required=True)
    parser.add_argument("--execute", action="store_true")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    original_hash = sha256(args.checkpoint)
    if original_hash != args.expected_sha256:
        raise ValueError("Checkpoint SHA-256 differs from the authorized failed artifact.")
    header, rows = read_checkpoint(args.checkpoint)
    repaired, affected = repair_rows(header, rows)
    panel_index = header.index("panel_key")
    panels = Counter(rows[position][panel_index] for position in affected)
    if len(affected) != args.expected_affected_rows:
        raise ValueError("Affected-row count differs from the authorized repair scope.")
    if len(panels) != args.expected_affected_panels or set(panels.values()) != {6}:
        raise ValueError("Affected panels do not have the expected six-row batches.")
    summary = {
        "status": "validated" if not args.execute else "repaired",
        "checkpoint": str(args.checkpoint),
        "original_sha256": original_hash,
        "row_count": len(rows),
        "affected_rows": len(affected),
        "affected_panels": len(panels),
        "affected_panel_keys": sorted(panels),
        "status_counts_after_repair": dict(
            sorted(Counter(row[header.index("status")] for row in repaired).items())
        ),
    }
    if args.execute:
        if args.backup.exists():
            if sha256(args.backup) != original_hash:
                raise ValueError("Existing backup differs from the failed checkpoint.")
        else:
            shutil.copy2(args.checkpoint, args.backup)
        write_checkpoint(args.checkpoint, header, repaired)
        summary["repaired_sha256"] = sha256(args.checkpoint)
        args.receipt.parent.mkdir(parents=True, exist_ok=True)
        args.receipt.write_text(json.dumps(summary, indent=2, sort_keys=True) + "\n")
    print(json.dumps(summary, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
