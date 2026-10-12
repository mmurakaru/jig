"""Summarize one jig run from `.jig/runs/` and the adapter's `jig-exit-code` file."""

import json
import re
from dataclasses import asdict, dataclass, field
from datetime import UTC, datetime
from enum import StrEnum
from pathlib import Path

JIG_EXIT_CODE_FILE = "jig-exit-code"
METERING_LEDGER_FILE = "metering.jsonl"

# The skills of the bugfix retry group; their step keys carry the iteration.
RETRY_GROUP_SKILLS = ("implement-fix", "run-tests")
STEP_KEY_ITERATION_PATTERN = re.compile(r"/e(\d+)/(?:.*/)?r(\d+)/s\d+$")

DEFAULT_TIER_NAME = "default"


class EscalationCause(StrEnum):
    """Why a paused run paused."""

    AGENT_ESCALATED = "agent-escalated"
    FAILURE_ESCALATED = "failure-escalated"
    OTHER = "other"


@dataclass(frozen=True)
class JigRunSummary:
    """What one jig run did; cost_usd is None when any step's cost is unknown."""

    run_id: str | None
    status: str | None
    exit_code: int | None
    escalation_cause: EscalationCause | None
    retry_iterations: int
    step_count: int
    invalid_handoff_steps: int
    timeout_steps: int
    interrupted_attempts: int
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

    @property
    def ended_on_timeout(self) -> bool:
        """The run stopped short with a timed-out step last; a timeout it recovered from does not count."""
        return not self.completed and bool(self.steps) and self.steps[-1]["outcome"] == "timeout"

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
    return max(records, key=lambda record: parse_run_timestamp(record.get("started_at")))


def parse_run_timestamp(text: str | None) -> datetime:
    """Parse a run record timestamp, with or without milliseconds; unreadable sorts first."""
    try:
        return datetime.fromisoformat(text) if text else datetime.min.replace(tzinfo=UTC)
    except ValueError:
        return datetime.min.replace(tzinfo=UTC)


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


def classify_escalation_cause(record: dict) -> EscalationCause | None:
    """Name why a paused run paused: the agent asked, or jig escalated a failure."""
    if record.get("status") != "paused":
        return None
    steps = record.get("steps") or []
    if not steps:
        return EscalationCause.OTHER
    last_outcome = steps[-1].get("outcome")
    if last_outcome == "escalate":
        return EscalationCause.AGENT_ESCALATED
    if last_outcome in ("fail", "invalid-handoff", "timeout"):
        return EscalationCause.FAILURE_ESCALATED
    return EscalationCause.OTHER


def count_retry_iterations(steps: list[dict]) -> int:
    """Count distinct retry iterations from step keys; a timed-out fix leaves no run-tests record."""
    iterations = set()
    for step in steps:
        if step.get("skill") not in RETRY_GROUP_SKILLS:
            continue
        match = STEP_KEY_ITERATION_PATTERN.search((step.get("attempt") or {}).get("step_key", ""))
        if match:
            iterations.add(match.groups())
    return len(iterations)


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
    attempts_in_flight = record.get("attempts_in_flight") or {}
    interrupted_attempts = len(record.get("interrupted_attempts") or []) + len(attempts_in_flight)

    return JigRunSummary(
        run_id=run_id,
        status=record.get("status"),
        exit_code=read_jig_exit_code(runs_directory),
        escalation_cause=classify_escalation_cause(record),
        retry_iterations=count_retry_iterations(steps),
        step_count=len(steps),
        invalid_handoff_steps=sum(1 for step in steps if step.get("outcome") == "invalid-handoff"),
        timeout_steps=sum(1 for step in steps if step.get("outcome") == "timeout"),
        interrupted_attempts=interrupted_attempts,
        # An attempt still in flight was cut off before jig could meter it.
        cost_usd=known_cost_usd if events and unknown_cost_events == 0 and not attempts_in_flight else None,
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
