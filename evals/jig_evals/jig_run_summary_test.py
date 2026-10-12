import json
from pathlib import Path

import pytest

from jig_evals.jig_run_summary import summarize_jig_run

RECORDED_RUNS = Path(__file__).parent / "testdata" / "jig-runs"


def test_completed_run_counts_each_retry_iteration_and_splits_cost_by_tier():
    summary = summarize_jig_run(RECORDED_RUNS / "fix-on-second-iteration")
    assert summary.status == "completed"
    assert summary.exit_code == 0
    assert summary.retry_iterations == 2
    assert summary.step_count == 7
    assert not summary.escalated
    assert summary.escalation_cause is None
    assert summary.cost_by_tier == pytest.approx({"default": 1.0, "mechanical": 0.0375})
    assert summary.cost_usd == pytest.approx(1.0375)
    assert summary.unknown_cost_events == 0
    assert (summary.input_tokens, summary.cache_tokens, summary.output_tokens) == (
        7 * 4200,
        7 * 3000,
        7 * 400,
    )


def test_agent_escalation_pauses_the_run():
    summary = summarize_jig_run(RECORDED_RUNS / "escalate-on-reproduce")
    assert summary.status == "paused"
    assert summary.exit_code == 2
    assert summary.escalated
    assert summary.escalation_cause == "agent-escalated"
    assert summary.retry_iterations == 0


def test_exhausted_retries_escalate_as_a_failure():
    summary = summarize_jig_run(RECORDED_RUNS / "retries-exhausted")
    assert summary.escalated
    assert summary.escalation_cause == "failure-escalated"
    assert summary.retry_iterations == 3


def test_invalid_handoff_fails_the_run():
    summary = summarize_jig_run(RECORDED_RUNS / "invalid-handoff")
    assert summary.status == "failed"
    assert summary.exit_code == 1
    assert summary.invalid_handoff_steps == 1
    assert not summary.escalated


def test_one_unknown_cost_makes_the_run_cost_unknown_but_keeps_known_parts():
    summary = summarize_jig_run(RECORDED_RUNS / "unknown-cost")
    assert summary.cost_usd is None
    assert summary.unknown_cost_events == 1
    assert summary.known_cost_usd == pytest.approx(0.7625)
    assert summary.cost_by_tier == pytest.approx({"default": 0.75, "mechanical": 0.0125})


def test_missing_directory_summarizes_as_no_run(tmp_path):
    summary = summarize_jig_run(tmp_path / "jig-runs")
    assert summary.status is None
    assert summary.exit_code is None
    assert summary.cost_usd is None
    assert summary.step_count == 0


def test_a_timeout_the_retry_group_recovers_from_still_completes():
    summary = summarize_jig_run(RECORDED_RUNS / "timeout-then-recover")
    assert summary.status == "completed"
    assert summary.timeout_steps == 1
    assert not summary.ended_on_timeout
    assert summary.retry_iterations == 2
    assert summary.cost_usd is None
    assert summary.unknown_cost_events == 1


def test_timeouts_in_every_iteration_end_the_run_on_a_timeout():
    summary = summarize_jig_run(RECORDED_RUNS / "timeout-exhausted")
    assert summary.status == "paused"
    assert summary.exit_code == 2
    assert summary.ended_on_timeout
    assert summary.escalation_cause == "failure-escalated"
    assert (summary.timeout_steps, summary.retry_iterations) == (3, 3)


def test_a_run_killed_mid_step_has_an_unmetered_attempt_and_no_exit_code():
    summary = summarize_jig_run(RECORDED_RUNS / "killed-mid-step")
    assert summary.status == "running"
    assert summary.exit_code is None
    assert summary.interrupted_attempts == 1
    assert summary.known_cost_usd == pytest.approx(0.5)
    assert summary.cost_usd is None
    assert not summary.completed


def test_newest_run_record_wins_and_metering_of_other_runs_is_ignored(tmp_path):
    for run_id, started_at in (("old", "2026-10-11T00:00:00Z"), ("new", "2026-10-12T00:00:00Z")):
        (tmp_path / f"{run_id}.json").write_text(
            json.dumps({"id": run_id, "status": "completed", "steps": [], "started_at": started_at})
        )
    (tmp_path / "metering.jsonl").write_text(
        '{"run_id": "old", "cost_usd": 5.0}\n{"run_id": "new", "cost_usd": 1.5}\nnot json\n'
    )
    summary = summarize_jig_run(tmp_path)
    assert summary.run_id == "new"
    assert summary.cost_usd == pytest.approx(1.5)


def test_newest_run_record_wins_with_and_without_milliseconds(tmp_path):
    for run_id, started_at in (("whole-second", "2026-10-12T15:27:11Z"), ("later", "2026-10-12T15:27:11.250Z")):
        (tmp_path / f"{run_id}.json").write_text(
            json.dumps({"id": run_id, "status": "completed", "steps": [], "started_at": started_at})
        )
    assert summarize_jig_run(tmp_path).run_id == "later"
