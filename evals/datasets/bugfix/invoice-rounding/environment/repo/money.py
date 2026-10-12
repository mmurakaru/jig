"""Convert decimal amounts to whole cents."""

from decimal import Decimal


def to_cents(amount: Decimal) -> int:
    """Return the amount in whole cents, rounding half a cent up."""
    return int(amount * 100)


def format_cents(cents: int) -> str:
    """Render whole cents as a two-decimal amount, for example 1234 as 12.34."""
    return f"{cents // 100}.{cents % 100:02d}"
