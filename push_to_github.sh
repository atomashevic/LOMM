#!/bin/bash
# Usage: unzip lomm_rr_v2_push_package.zip at the ROOT of your LOMM clone
#        (so this script and paper/ land next to the repo's own paper/),
#        then run:  bash push_to_github.sh
set -euo pipefail

BRANCH="rr-v2-math-revision"
MSG="Apply post-review remaining-issue fixes R1-R6 to RR_V2"

git checkout "$BRANCH"


git add paper/Paper_emotion_dynamics_HGolino_RR_V2.qmd \
        paper/RR_V2_deliverables/RR_to_RR_V2.unified.diff \
        paper/RR_V2_deliverables/RR_V2_change_log.md \
        paper/RR_V2_deliverables/RR_V2_implementation_audit.md \
        paper/RR_V2_deliverables/audit_numeric.md \
        paper/RR_V2_deliverables/lomm_audit.py

# Byte-verify the manuscript blob matches the validated SHA
EXPECTED="8295916c2cdd8392da0a7b935aba31c79738242a"
ACTUAL=$(git ls-files -s paper/Paper_emotion_dynamics_HGolino_RR_V2.qmd | awk '{print $2}')
[ "$ACTUAL" = "$EXPECTED" ] || { echo "BLOB MISMATCH: $ACTUAL != $EXPECTED"; exit 1; }

git commit -m "$MSG"
git push origin "$BRANCH"
echo "Pushed. Open the PR to main at: https://github.com/atomashevic/LOMM/compare/main...$BRANCH"
