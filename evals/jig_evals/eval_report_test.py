import json
import shutil
from pathlib import Path

import pytest

from jig_evals.eval_report import build_eval_report, load_trial_outcomes, main

RECORDED_RUNS = Path(__file__).parent / "testdata" / "jig-runs"
MODEL = "anthropic/claude-sonnet-4-6"


def write_trial(
    job_directory: Path,
    trial_name: str,
    agent_name: str,
    reward: float | None,
    recorded_run: str | None = None,
    cost_usd: float | None = None,
    exception_type: str | None = None,
) -> None:
    """Write a trial directory in the shape Harbor 0.24 writes it."""
    trial_directory = job_directory / trial_name
    (trial_directory / "agent").mkdir(parents=True)
    kwargs = {"jig_binary_path": "/host/jig"} if agent_name == "jig" else {}
    result = {
        "trial_name": trial_name,
        "task_name": "jig/" + trial_name.split("__")[0],
        "agent_info": {"name": agent_name, "version": "1"},
        "config": {"agent": {"name": None, "model_name": MODEL, "kwargs": kwargs}},
        "agent_result": {"cost_usd": cost_usd},
        "verifier_result": None if reward is None else {"rewards": {"reward": reward}},
        "exception_info": None
        if exception_type is None
        else {"exception_type": exception_type, "exception_message": "", "exception_traceback": "", "occurred_at": "2026-10-12T00:00:00Z"},
    }
    (trial_directory / "result.json").write_text(json.dumps(result))
    if recorded_run is not None:
        shutil.copytree(RECORDED_RUNS / recorded_run, trial_directory / "agent" / "jig-runs")


@pytest.fixture
def job_directory(tmp_path: Path) -> Path:
    job = tmp_path / "job"
    job.mkdir()
    (job / "result.json").write_text(json.dumps({"id": "job-level result, not a trial"}))
    write_trial(job, "pagination-last-page__a", "jig", 1.0, "fix-on-second-iteration")
    write_trial(job, "pagination-last-page__b", "jig", 0.0, "escalate-on-reproduce")
    write_trial(job, "settings-merge__a", "jig", 1.0, "retries-exhausted")
    write_trial(job, "settings-merge__b", "jig", 0.0, "invalid-handoff")
    write_trial(job, "duration-parse__a", "jig", 1.0, "unknown-cost")
    write_trial(job, "duration-parse__b", "claude-code", 1.0, cost_usd=0.5)
    write_trial(job, "settings-merge__c", "claude-code", 0.0, cost_usd=0.75)
    write_trial(job, "invoice-rounding__a", "claude-code", None, exception_type="AgentTimeoutError")
    return job


def reports_by_agent(job_directory: Path) -> dict:
    reports = build_eval_report(load_trial_outcomes(job_directory))
    return {report.arm.split(" ")[0]: report for report in reports}


def test_arms_are_split_by_agent_and_host_paths_stay_out_of_the_label(job_directory):
    reports = build_eval_report(load_trial_outcomes(job_directory))
    assert [report.arm for report in reports] == [f"claude-code ({MODEL})", f"jig ({MODEL})"]


def test_a_solved_but_paused_run_is_not_an_unattended_solve(job_directory):
    jig = reports_by_agent(job_directory)["jig"]
    assert (jig.trials, jig.solved, jig.solved_unattended) == (5, 3, 2)


def test_jig_cost_metrics_count_failed_trials_and_keep_unknown_cost_visible(job_directory):
    jig = reports_by_agent(job_directory)["jig"]
    known_costs = [1.0375, 0.25, 1.2875, 0.5, 0.7625]
    assert jig.mean_cost_per_trial_usd == pytest.approx(sum(known_costs) / 5)
    assert jig.cost_per_solved_task_usd == pytest.approx(sum(known_costs) / 3)
    assert jig.unknown_cost_events == 1
    assert jig.cost_by_tier_usd == pytest.approx({"default": 3.75, "mechanical": 0.0875})


def test_jig_workflow_metrics(job_directory):
    jig = reports_by_agent(job_directory)["jig"]
    assert jig.mean_retry_iterations == pytest.approx((2 + 0 + 3 + 0 + 1) / 5)
    assert jig.escalations == 2
    assert jig.escalations_by_cause == {"agent-escalated": 1, "failure-escalated": 1}
    assert (jig.invalid_handoff_steps, jig.steps, jig.trials_with_invalid_handoff) == (1, 23, 1)


def test_baseline_timeout_is_unsolved_and_costless(job_directory):
    baseline = reports_by_agent(job_directory)["claude-code"]
    assert (baseline.solved, baseline.solved_unattended, baseline.timeouts) == (1, 1, 1)
    assert baseline.trials_without_cost == 1
    assert baseline.cost_per_solved_task_usd == pytest.approx(1.25)
    assert not baseline.is_jig


def test_rendered_report_names_every_metric(job_directory, capsys):
    assert main([str(job_directory)]) == 0
    output = capsys.readouterr().out
    for metric in (
        "solve rate",
        "unattended solve rate",
        "cost per trial",
        "cost per solved task",
        "cost per tier",
        "retry iterations",
        "escalation rate",
        "invalid-handoff rate",
    ):
        assert metric in output
    assert "60.0% (3/5)" in output


def test_empty_job_directory_is_an_error(tmp_path, capsys):
    assert main([str(tmp_path)]) == 1
    assert "no trials found" in capsys.readouterr().err
