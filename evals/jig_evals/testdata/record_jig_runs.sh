#!/bin/bash
# Re-records testdata/jig-runs/<scenario>/ with a real jig binary and scripted_harness.py.
set -euo pipefail
jig_binary="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
testdata="$(cd "$(dirname "$0")" && pwd)"
scratch="$(mktemp -d /tmp/jig-record.XXXXXX)"
cp "$testdata/scripted_harness.py" "$scratch/scripted_harness.py"

for scenario in fix-on-second-iteration escalate-on-reproduce invalid-handoff unknown-cost \
  retries-exhausted timeout-then-recover timeout-exhausted killed-mid-step; do
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
timeout:
  default: 3s
  grace: 1s
CONFIG
  export JIG_STUB_SCENARIO="$scenario" JIG_STUB_STATE_DIRECTORY="$scratch/state-$scenario"
  target="$testdata/jig-runs/$scenario"
  rm -rf "$target"
  mkdir -p "$target"
  if [ "$scenario" = killed-mid-step ]; then
    # Stands in for Harbor's agent timeout: jig dies mid-step and writes no exit code.
    "$jig_binary" run bugfix --task "Listing 25 items at 10 per page shows only 2 pages." > /dev/null 2>&1 &
    jig_pid=$!
    sleep 2
    kill -9 "$jig_pid"
    wait "$jig_pid" 2> /dev/null || true
    pkill -9 -f "$scratch/scripted_harness.py" || true
  else
    exit_code=0
    "$jig_binary" run bugfix --task "Listing 25 items at 10 per page shows only 2 pages." \
      > /dev/null 2>&1 || exit_code=$?
    echo "$exit_code" > "$target/jig-exit-code"
  fi
  cp .jig/runs/*.json .jig/runs/metering.jsonl "$target/"
done
echo "recorded into $testdata/jig-runs"
