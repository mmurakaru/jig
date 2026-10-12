"""Invoice totals."""

from decimal import Decimal

from money import format_cents, to_cents


def line_total_cents(quantity: int, unit_price: str) -> int:
    """Return the cents for one invoice line; unit_price is a decimal string."""
    return to_cents(quantity * Decimal(unit_price))


def invoice_total(lines: list[tuple[int, str]]) -> str:
    """Return the formatted total of (quantity, unit_price) lines."""
    return format_cents(sum(line_total_cents(quantity, price) for quantity, price in lines))
