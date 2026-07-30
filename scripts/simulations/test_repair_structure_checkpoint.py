#!/usr/bin/env python3

import csv
import importlib.util
import os
import tempfile
import unittest
from pathlib import Path


SCRIPT = Path(__file__).with_name("repair_structure_checkpoint.py")
SPEC = importlib.util.spec_from_file_location("repair_structure_checkpoint", SCRIPT)
REPAIR = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(REPAIR)


CANONICAL_SUFFIX = (
    "structure_fit_status",
    "structure_hash",
    "structure_fit_runtime_sec",
    "native_expand_n_dim",
    "native_expand_assignment_accuracy",
    "native_expand_status",
    "native_expand_error_message",
    "correlation_louvain_095_n_dim",
    "correlation_louvain_095_assignment_accuracy",
    "correlation_louvain_095_status",
    "correlation_louvain_095_error_message",
    "readout_runtime_sec",
    "runtime_sec",
    "status",
    "error_message",
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
)


class RepairStructureCheckpointTests(unittest.TestCase):
    def test_error_first_batch_is_remapped_and_validated(self):
        header = [
            "unit_key",
            "panel_key",
            "structure_fit_key",
            "community_algorithm",
            *CANONICAL_SUFFIX,
        ]
        source_order = REPAIR.error_first_order(header)
        rows = []
        algorithms = ("louvain", "leiden", "walktrap")
        for method, fit_status in (("static", "error"), ("oracle", "ok")):
            for algorithm in algorithms:
                values = {name: "" for name in header}
                values.update(
                    unit_key=f"panel|{method}|{algorithm}",
                    panel_key="panel",
                    structure_fit_key=f"panel|{method}",
                    community_algorithm=algorithm,
                    structure_fit_status=fit_status,
                    structure_hash="a" * 64 if fit_status == "ok" else "",
                    structure_fit_runtime_sec="1.2",
                    runtime_sec="1.3",
                    status=fit_status,
                    error_message=(
                        "need finite 'xlim' values" if fit_status == "error" else ""
                    ),
                    completed_within_900="TRUE" if fit_status == "ok" else "FALSE",
                    true_membership="Y1=1;Y2=1",
                    estimated_membership="Y1=1;Y2=1" if fit_status == "ok" else "",
                )
                rows.append([values[name] for name in source_order])
        repaired, affected = REPAIR.repair_rows(header, rows)
        self.assertEqual(len(affected), 6)
        status_index = header.index("status")
        self.assertEqual(
            [row[status_index] for row in repaired],
            ["error"] * 3 + ["ok"] * 3,
        )

    def test_atomic_write_preserves_permissions(self):
        with tempfile.TemporaryDirectory() as directory:
            checkpoint = Path(directory) / "checkpoint.csv"
            checkpoint.write_text('"unit_key","status"\n"a","ok"\n')
            os.chmod(checkpoint, 0o664)
            REPAIR.write_checkpoint(
                checkpoint,
                ["unit_key", "status"],
                [["b", "error"]],
            )
            self.assertEqual(checkpoint.stat().st_mode & 0o777, 0o664)
            with checkpoint.open(newline="") as handle:
                self.assertEqual(list(csv.reader(handle))[1], ["b", "error"])


if __name__ == "__main__":
    unittest.main()
