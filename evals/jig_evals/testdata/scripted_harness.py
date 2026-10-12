"""Stand-in harness: answers each jig step per JIG_STUB_SCENARIO, counting calls in JIG_STUB_STATE_DIRECTORY."""

import json
import os
import re
import sys
import time
from pathlib import Path

HANDOFF_TEMPLATE = "Done.\n\n```handoff\nstatus: {status}\nsummary: {summary}\n```\n"


def step_outcome(scenario: str, skill: str, call_number: int) -> str | None:
    """Return the handoff status for this call, or None for no handoff at all."""
    if scenario == "escalate-on-reproduce" and skill == "reproduce-issue":
        return "escalate"
    if scenario == "invalid-handoff" and skill == "write-failing-test":
        return None
    if scenario == "fix-on-second-iteration" and skill == "run-tests":
        return "fail" if call_number == 1 else "pass"
    if scenario == "retries-exhausted" and skill == "run-tests":
        return "fail"
    return "pass"


def main() -> None:
    prompt = sys.argv[-1]
    skill_match = re.search(r"Workflow: \S+ - step \d+ of \d+ \(([a-z-]+)\)", prompt)
    skill = skill_match.group(1) if skill_match else "unknown"
    scenario = os.environ["JIG_STUB_SCENARIO"]
    state_directory = Path(os.environ["JIG_STUB_STATE_DIRECTORY"])
    state_directory.mkdir(parents=True, exist_ok=True)
    counter_path = state_directory / f"{skill}.count"
    call_number = int(counter_path.read_text()) + 1 if counter_path.exists() else 1
    counter_path.write_text(str(call_number))

    if skill == "implement-fix" and (
        scenario == "timeout-exhausted"
        or (scenario == "timeout-then-recover" and call_number == 1)
        or scenario == "killed-mid-step"
    ):
        time.sleep(60)

    status = step_outcome(scenario, skill, call_number)
    text = (
        "I could not finish this step."
        if status is None
        else HANDOFF_TEMPLATE.format(status=status, summary=f"{skill} call {call_number}")
    )
    reports_cost = not (scenario == "unknown-cost" and skill == "run-tests")
    if not reports_cost:
        print(text)
        return
    print(
        json.dumps(
            {
                "type": "result",
                "result": text,
                "session_id": f"stub-{skill}-{call_number}",
                "total_cost_usd": 0.0125 if skill in ("run-tests", "open-pr") else 0.25,
                "usage": {
                    "input_tokens": 1000,
                    "cache_creation_input_tokens": 200,
                    "cache_read_input_tokens": 3000,
                    "output_tokens": 400,
                },
            }
        )
    )


if __name__ == "__main__":
    main()
