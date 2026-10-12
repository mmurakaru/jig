"""Parse short duration strings such as 90s, 5m or 1h30m."""

import re

SECONDS_PER_UNIT = {"h": 3600, "m": 60, "s": 1}
COMPONENT_PATTERN = re.compile(r"(\d+)([hms])")


def parse_duration(text: str) -> int:
    """Return the duration in seconds; raise ValueError on anything else."""
    if not text or COMPONENT_PATTERN.sub("", text):
        raise ValueError(f"not a duration: {text!r}")
    seconds = 0
    for amount, unit in COMPONENT_PATTERN.findall(text):
        seconds = int(amount) * SECONDS_PER_UNIT[unit]
    return seconds
