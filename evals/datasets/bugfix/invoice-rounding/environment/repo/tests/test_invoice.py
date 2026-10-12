from invoice import invoice_total, line_total_cents


def test_whole_cent_line():
    assert line_total_cents(2, "1.25") == 250


def test_total_of_several_lines():
    assert invoice_total([(1, "10.00"), (3, "0.50")]) == "11.50"


def test_empty_invoice():
    assert invoice_total([]) == "0.00"
