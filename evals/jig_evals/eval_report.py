"""Report jig eval metrics from a Harbor job directory.

Usage: python -m jig_evals.eval_report <job directory> [--json]

Each agent configuration in the job is one arm (for example jig and the
plain claude-code baseline on the same tasks).
"""

import argparse
import json
import statistics
import sys
from dataclasses import dataclass, field
from pathlib import Path

from jig_evals.jig_agent import JIG_RUNS_LOG_NAME
from jig_evals.jig_run_summary import JigRunSummary, summarize_jig_run

# Host-specific paths say nothing about the arm, so they stay out of its label.
ARM_LABEL_IGNORED_KWARGS = {"jig_binary_path"}


@dataclass(frozen=True)
class TrialOutcome:
    """One Harbor trial: the verifier's reward plus what the agent reported."""

    trial_name: str
    task_name: str
    arm: str
    reward: float | None
    exception_type: str | None
    cost_usd: float | None
    jig_run: JigRunSummary | None

    @property
    def solved(self) -> bool:
        return self.reward is not None and self.reward >= 1.0

    @property
    def finished_unattended(self) -> bool:
        """jig: the run completed. Other agents: the agent ended without an error."""
        if self.jig_run is not None:
            return self.jig_run.completed and self.exception_type is None
        return self.exception_type is None

    @property
    def solved_unattended(self) -> bool:
        return self.solved and self.finished_unattended

    @property
    def timed_out(self) -> bool:
        harbor_timeout = self.exception_type is not None and "Timeout" in self.exception_type
        return harbor_timeout or (self.jig_run is not None and self.jig_run.timeout_steps > 0)

    @property
    def known_cost_usd(self) -> float | None:
        """Cost of the parts that reported one; None when nothing reported a cost."""
        if self.jig_run is not None:
            return self.jig_run.known_cost_usd if self.jig_run.cost_by_tier else None
        return self.cost_usd


def arm_label(agent_name: str, model_name: str | None, kwargs: dict) -> str:
    """Name an arm by agent, model, and the options that change its behavior."""
    label = agent_name + (f" ({model_name})" if model_name else "")
    options = [
        f"{key}={json.dumps(value) if not isinstance(value, str) else value}"
        for key, value in sorted(kwargs.items())
        if key not in ARM_LABEL_IGNORED_KWARGS
    ]
    return label + (" " + " ".join(options) if options else "")


def load_trial_outcome(trial_directory: Path) -> TrialOutcome:
    """Read one trial directory written by Harbor."""
    result = json.loads((trial_directory / "result.json").read_text())
    agent_config = (result.get("config") or {}).get("agent") or {}
    agent_name = (result.get("agent_info") or {}).get("name") or agent_config.get("name") or "?"
    rewards = (result.get("verifier_result") or {}).get("rewards") or {}
    exception_info = result.get("exception_info") or {}
    agent_result = result.get("agent_result") or {}
    jig_runs_directory = trial_directory / "agent" / JIG_RUNS_LOG_NAME
    return TrialOutcome(
        trial_name=result["trial_name"],
        task_name=result["task_name"],
        arm=arm_label(agent_name, agent_config.get("model_name"), agent_config.get("kwargs") or {}),
        reward=rewards.get("reward"),
        exception_type=exception_info.get("exception_type"),
        cost_usd=agent_result.get("cost_usd"),
        jig_run=summarize_jig_run(jig_runs_directory) if agent_name == "jig" else None,
    )


def load_trial_outcomes(job_directory: Path) -> list[TrialOutcome]:
    """Read every trial of a Harbor job directory, in trial name order."""
    trial_directories = sorted(
        path.parent
        for path in job_directory.glob("*/result.json")
        if "trial_name" in json.loads(path.read_text())
    )
    return [load_trial_outcome(directory) for directory in trial_directories]


@dataclass
class ArmReport:
    """The metrics of one arm; rates are fractions of the arm's trials."""

    arm: str
    trials: int
    solved: int
    solved_unattended: int
    mean_cost_per_trial_usd: float | None
    trials_without_cost: int
    cost_per_solved_task_usd: float | None
    timeouts: int
    is_jig: bool
    cost_by_tier_usd: dict[str, float] = field(default_factory=dict)
    unknown_cost_events: int = 0
    mean_retry_iterations: float | None = None
    escalations: int = 0
    escalations_by_cause: dict[str, int] = field(default_factory=dict)
    steps: int = 0
    invalid_handoff_steps: int = 0
    trials_with_invalid_handoff: int = 0
    solved_by_task: dict[str, tuple[int, int]] = field(default_factory=dict)


def build_arm_report(arm: str, outcomes: list[TrialOutcome]) -> ArmReport:
    """Compute one arm's metrics; a cost per solved task counts failed trials' cost too."""
    known_costs = [outcome.known_cost_usd for outcome in outcomes if outcome.known_cost_usd is not None]
    solved = sum(outcome.solved for outcome in outcomes)
    jig_runs = [outcome.jig_run for outcome in outcomes if outcome.jig_run is not None]
    solved_by_task: dict[str, tuple[int, int]] = {}
    for outcome in outcomes:
        task_solved, task_trials = solved_by_task.get(outcome.task_name, (0, 0))
        solved_by_task[outcome.task_name] = (task_solved + outcome.solved, task_trials + 1)

    report = ArmReport(
        arm=arm,
        trials=len(outcomes),
        solved=solved,
        solved_unattended=sum(outcome.solved_unattended for outcome in outcomes),
        mean_cost_per_trial_usd=statistics.fmean(known_costs) if known_costs else None,
        trials_without_cost=len(outcomes) - len(known_costs),
        cost_per_solved_task_usd=sum(known_costs) / solved if solved and known_costs else None,
        timeouts=sum(outcome.timed_out for outcome in outcomes),
        is_jig=bool(jig_runs),
        solved_by_task=dict(sorted(solved_by_task.items())),
    )
    if not jig_runs:
        return report
    for run in jig_runs:
        for tier, cost in run.cost_by_tier.items():
            report.cost_by_tier_usd[tier] = report.cost_by_tier_usd.get(tier, 0.0) + cost
        if run.escalation_cause is not None:
            report.escalations += 1
            report.escalations_by_cause[run.escalation_cause] = (
                report.escalations_by_cause.get(run.escalation_cause, 0) + 1
            )
    report.unknown_cost_events = sum(run.unknown_cost_events for run in jig_runs)
    report.mean_retry_iterations = statistics.fmean(run.retry_iterations for run in jig_runs)
    report.steps = sum(run.step_count for run in jig_runs)
    report.invalid_handoff_steps = sum(run.invalid_handoff_steps for run in jig_runs)
    report.trials_with_invalid_handoff = sum(run.invalid_handoff_steps > 0 for run in jig_runs)
    return report


def build_eval_report(outcomes: list[TrialOutcome]) -> list[ArmReport]:
    """Group trials by arm, sorted by arm label."""
    arms: dict[str, list[TrialOutcome]] = {}
    for outcome in outcomes:
        arms.setdefault(outcome.arm, []).append(outcome)
    return [build_arm_report(arm, arms[arm]) for arm in sorted(arms)]


def format_rate(count: int, total: int) -> str:
    percent = 100.0 * count / total if total else 0.0
    return f"{percent:.1f}% ({count}/{total})"


def format_usd(value: float | None) -> str:
    return "n/a" if value is None else f"${value:.4f}"


def render_arm_report(report: ArmReport) -> str:
    """Render one arm as aligned plain-text lines."""
    cost_note = f" ({report.trials_without_cost} of {report.trials} trials reported no cost)" if report.trials_without_cost else ""
    rows = [
        ("trials", str(report.trials)),
        ("solve rate", format_rate(report.solved, report.trials)),
        ("unattended solve rate", format_rate(report.solved_unattended, report.trials)),
        ("cost per trial", f"{format_usd(report.mean_cost_per_trial_usd)}{cost_note}"),
        ("cost per solved task", format_usd(report.cost_per_solved_task_usd)),
        ("timeouts", str(report.timeouts)),
    ]
    if report.is_jig:
        tiers = ", ".join(f"{tier} {format_usd(cost)}" for tier, cost in sorted(report.cost_by_tier_usd.items()))
        unknown = f" (+{report.unknown_cost_events} unknown-cost steps)" if report.unknown_cost_events else ""
        causes = ", ".join(f"{cause} {count}" for cause, count in sorted(report.escalations_by_cause.items()))
        rows += [
            ("cost per tier", (tiers or "n/a") + unknown),
            ("retry iterations", f"{report.mean_retry_iterations:.2f} per trial"),
            ("escalation rate", format_rate(report.escalations, report.trials) + (f": {causes}" if causes else "")),
            (
                "invalid-handoff rate",
                f"{format_rate(report.invalid_handoff_steps, report.steps)} of steps, "
                f"in {report.trials_with_invalid_handoff} of {report.trials} trials",
            ),
        ]
    rows.append(("solved per task", ""))
    rows += [
        (f"  {task}", f"{task_solved}/{task_trials}")
        for task, (task_solved, task_trials) in report.solved_by_task.items()
    ]
    width = max(len(name) for name, _ in rows)
    return "\n".join([report.arm] + [f"  {name.ljust(width)}  {value}" for name, value in rows])


def render_eval_report(reports: list[ArmReport]) -> str:
    return "\n\n".join(render_arm_report(report) for report in reports) + "\n"


def main(arguments: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Report jig eval metrics from a Harbor job directory.")
    parser.add_argument("job_directory", type=Path)
    parser.add_argument("--json", action="store_true", help="print the metrics as JSON")
    options = parser.parse_args(arguments)
    outcomes = load_trial_outcomes(options.job_directory)
    if not outcomes:
        print(f"eval report: no trials found in {options.job_directory}", file=sys.stderr)
        return 1
    reports = build_eval_report(outcomes)
    if options.json:
        print(json.dumps([report.__dict__ for report in reports], indent=2))
    else:
        print(render_eval_report(reports), end="")
    return 0


if __name__ == "__main__":
    sys.exit(main())
