from decimal import Decimal

from invoice import invoice_total, line_total_cents
from money import to_cents


def test_half_cent_line_rounds_up():
    assert line_total_cents(3, "0.335") == 101


def test_invoice_with_half_cent_line():
    assert invoice_total([(3, "0.335"), (1, "2.00")]) == "3.01"


def test_below_half_cent_rounds_down():
    assert to_cents(Decimal("0.104")) == 10


def test_whole_cents_are_unchanged():
    assert to_cents(Decimal("19.99")) == 1999
