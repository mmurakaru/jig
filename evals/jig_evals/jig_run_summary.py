"""Summarize one jig run from its run record and metering ledger.

The input is a copy of `.jig/runs/` plus the `jig-exit-code` file the
adapter writes next to it. Both the Harbor adapter and the report use this,
so a metric has one definition.
"""

import json
from dataclasses import asdict, dataclass, field
from pathlib import Path

JIG_EXIT_CODE_FILE = "jig-exit-code"
METERING_LEDGER_FILE = "metering.jsonl"

# The `until: pass` step of the bugfix retry group: one record per iteration.
RETRY_ITERATION_SKILL = "run-tests"

DEFAULT_TIER_NAME = "default"


@dataclass(frozen=True)
class JigRunSummary:
    """What one jig run did; cost_usd is None when any step's cost is unknown."""

    run_id: str | None
    status: str | None
    exit_code: int | None
    escalation_cause: str | None
    retry_iterations: int
    step_count: int
    invalid_handoff_steps: int
    timeout_steps: int
    cost_usd: float | None
    known_cost_usd: float
    unknown_cost_events: int
    cost_by_tier: dict[str, float] = field(default_factory=dict)
    input_tokens: int | None = None
    cache_tokens: int | None = None
    output_tokens: int | None = None
    steps: list[dict] = field(default_factory=list)

    @property
    def escalated(self) -> bool:
        """A paused run waits for a human, which an unattended eval never has."""
        return self.status == "paused"

    @property
    def completed(self) -> bool:
        return self.status == "completed"

    def to_metadata(self) -> dict:
        """Plain JSON-safe dict for Harbor's AgentContext.metadata."""
        return {**asdict(self), "escalated": self.escalated}


def read_jig_exit_code(runs_directory: Path) -> int | None:
    """Return the exit code the adapter recorded, or None when jig never exited."""
    path = runs_directory / JIG_EXIT_CODE_FILE
    try:
        return int(path.read_text().strip())
    except (FileNotFoundError, ValueError):
        return None


def load_latest_run_record(runs_directory: Path) -> dict | None:
    """Return the newest run record in the directory, or None if there is none."""
    records = []
    for path in runs_directory.glob("*.json"):
        try:
            record = json.loads(path.read_text())
        except (OSError, json.JSONDecodeError):
            continue
        if isinstance(record, dict) and "id" in record and "steps" in record:
            records.append(record)
    if not records:
        return None
    return max(records, key=lambda record: record.get("started_at", ""))


def load_metering_events(runs_directory: Path, run_id: str | None) -> list[dict]:
    """Return the run's metering events; torn or foreign lines are skipped."""
    path = runs_directory / METERING_LEDGER_FILE
    if not path.exists():
        return []
    events = []
    for line in path.read_text().splitlines():
        try:
            event = json.loads(line)
        except json.JSONDecodeError:
            continue
        if isinstance(event, dict) and (run_id is None or event.get("run_id") == run_id):
            events.append(event)
    return events


def classify_escalation_cause(record: dict) -> str | None:
    """Name why a paused run paused: the agent asked, or jig escalated a failure."""
    if record.get("status") != "paused":
        return None
    steps = record.get("steps") or []
    if not steps:
        return "other"
    last_outcome = steps[-1].get("outcome")
    if last_outcome == "escalate":
        return "agent-escalated"
    if last_outcome in ("fail", "invalid-handoff"):
        return "failure-escalated"
    return "other"


def sum_token_usage(events: list[dict]) -> tuple[int | None, int | None, int | None]:
    """Return (input including cache, cache read, output) tokens over all events."""
    usages = [event["usage"] for event in events if isinstance(event.get("usage"), dict)]
    if not usages:
        return None, None, None
    input_tokens = sum(
        usage.get("input_tokens", 0)
        + usage.get("cache_creation_input_tokens", 0)
        + usage.get("cache_read_input_tokens", 0)
        for usage in usages
    )
    cache_tokens = sum(usage.get("cache_read_input_tokens", 0) for usage in usages)
    output_tokens = sum(usage.get("output_tokens", 0) for usage in usages)
    return input_tokens, cache_tokens, output_tokens


def summarize_jig_run(runs_directory: Path) -> JigRunSummary:
    """Summarize the newest run in a copied `.jig/runs/` directory."""
    record = load_latest_run_record(runs_directory) or {}
    run_id = record.get("id")
    steps = record.get("steps") or []
    events = load_metering_events(runs_directory, run_id)

    known_cost_usd = 0.0
    unknown_cost_events = 0
    cost_by_tier: dict[str, float] = {}
    for event in events:
        cost = event.get("cost_usd")
        if isinstance(cost, (int, float)):
            known_cost_usd += cost
            tier = event.get("tier") or DEFAULT_TIER_NAME
            cost_by_tier[tier] = cost_by_tier.get(tier, 0.0) + cost
        else:
            unknown_cost_events += 1
    input_tokens, cache_tokens, output_tokens = sum_token_usage(events)

    return JigRunSummary(
        run_id=run_id,
        status=record.get("status"),
        exit_code=read_jig_exit_code(runs_directory),
        escalation_cause=classify_escalation_cause(record),
        retry_iterations=sum(1 for step in steps if step.get("skill") == RETRY_ITERATION_SKILL),
        step_count=len(steps),
        invalid_handoff_steps=sum(1 for step in steps if step.get("outcome") == "invalid-handoff"),
        timeout_steps=sum(1 for step in steps if step.get("outcome") == "timeout"),
        cost_usd=known_cost_usd if events and unknown_cost_events == 0 else None,
        known_cost_usd=known_cost_usd,
        unknown_cost_events=unknown_cost_events,
        cost_by_tier=cost_by_tier,
        input_tokens=input_tokens,
        cache_tokens=cache_tokens,
        output_tokens=output_tokens,
        steps=[
            {
                "skill": step.get("skill"),
                "tier": step.get("tier"),
                "outcome": step.get("outcome"),
                "cost_usd": step.get("cost_usd"),
            }
            for step in steps
        ],
    )
