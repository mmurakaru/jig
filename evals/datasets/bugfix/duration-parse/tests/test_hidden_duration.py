import pytest

from duration import parse_duration


def test_hours_and_minutes():
    assert parse_duration("1h30m") == 5400


def test_all_three_units():
    assert parse_duration("1h1m1s") == 3661


def test_single_unit_still_works():
    assert parse_duration("45s") == 45


def test_trailing_garbage_is_rejected():
    with pytest.raises(ValueError):
        parse_duration("1h30x")
