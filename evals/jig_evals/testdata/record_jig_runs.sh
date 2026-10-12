#!/bin/bash
# Re-records testdata/jig-runs/<scenario>/ by running a real jig binary against
# scripted_harness.py. Usage: record_jig_runs.sh <path to jig binary>
set -euo pipefail
jig_binary="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
testdata="$(cd "$(dirname "$0")" && pwd)"
scratch="$(mktemp -d /tmp/jig-record.XXXXXX)"
cp "$testdata/scripted_harness.py" "$scratch/scripted_harness.py"

for scenario in fix-on-second-iteration escalate-on-reproduce invalid-handoff unknown-cost retries-exhausted; do
  workspace="$scratch/$scenario"
  mkdir -p "$workspace"
  cd "$workspace"
  git init -q
  git -c user.name=eval -c user.email=eval@example.invalid commit -q --allow-empty -m "Initial commit"
  "$jig_binary" init --harness custom > /dev/null
  cat > .jig/config.yaml <<CONFIG
harness: ["python3", "$scratch/scripted_harness.py"]
tiers:
  mechanical: ["python3", "$scratch/scripted_harness.py", "--mechanical"]
CONFIG
  exit_code=0
  JIG_STUB_SCENARIO="$scenario" JIG_STUB_STATE_DIR="$scratch/state-$scenario" \
    "$jig_binary" run bugfix --task "Listing 25 items at 10 per page shows only 2 pages." \
    > /dev/null 2>&1 || exit_code=$?
  target="$testdata/jig-runs/$scenario"
  rm -f "$target"/*.json "$target"/metering.jsonl "$target"/jig-exit-code
  mkdir -p "$target"
  cp .jig/runs/*.json .jig/runs/metering.jsonl "$target/"
  echo "$exit_code" > "$target/jig-exit-code"
done
echo "recorded into $testdata/jig-runs"
