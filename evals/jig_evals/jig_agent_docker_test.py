"""Harbor runs JigAgent in Docker with a stub harness; opt in with JIG_EVALS_LINUX_JIG_BINARY."""

import json
import os
import subprocess
from pathlib import Path

import pytest

from jig_evals.eval_report import load_trial_outcomes
from jig_evals.eval_suite import JIG_AGENT_IMPORT_PATH, harbor_cli_path

LINUX_JIG_BINARY = os.environ.get("JIG_EVALS_LINUX_JIG_BINARY")
TASK_PATH = Path(__file__).parent.parent / "datasets" / "bugfix" / "pagination-last-page"

pytestmark = pytest.mark.skipif(
    LINUX_JIG_BINARY is None, reason="set JIG_EVALS_LINUX_JIG_BINARY to run the Docker test"
)


def stub_harness(handoff_status: str) -> list[str]:
    script = f"printf '%s\\n' '```handoff' 'status: {handoff_status}' 'summary: stub' '```'"
    return ["sh", "-c", script, "stub"]


def stub_agent(handoff_status: str) -> dict:
    return {
        "import_path": JIG_AGENT_IMPORT_PATH,
        "kwargs": {
            "jig_binary_path": LINUX_JIG_BINARY,
            "install_harness": False,
            "harness": stub_harness(handoff_status),
        },
    }


def test_paused_and_completed_runs_return_normally_and_keep_the_run_record(tmp_path):
    job_config = {
        "job_name": "stub-harness",
        "jobs_dir": str(tmp_path),
        "n_concurrent_trials": 2,
        "agents": [stub_agent("escalate"), stub_agent("pass")],
        "tasks": [{"path": str(TASK_PATH)}],
    }
    config_path = tmp_path / "job.json"
    config_path.write_text(json.dumps(job_config))
    subprocess.run([harbor_cli_path(), "run", "-c", config_path, "-y", "-q"], check=True, timeout=1200)

    outcomes = load_trial_outcomes(tmp_path / "stub-harness")
    statuses = sorted(outcome.jig_run.status for outcome in outcomes)
    assert statuses == ["completed", "paused"]
    for outcome in outcomes:
        assert outcome.exception_type is None
        assert outcome.reward == 0.0
        assert not outcome.solved_unattended
    paused = next(outcome for outcome in outcomes if outcome.jig_run.status == "paused")
    assert paused.jig_run.exit_code == 2
    assert paused.jig_run.escalation_cause == "agent-escalated"
