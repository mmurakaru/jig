import pytest

from duration import parse_duration


def test_seconds():
    assert parse_duration("90s") == 90


def test_minutes():
    assert parse_duration("5m") == 300


def test_hours():
    assert parse_duration("2h") == 7200


def test_rejects_unknown_unit():
    with pytest.raises(ValueError):
        parse_duration("3d")


def test_rejects_empty_text():
    with pytest.raises(ValueError):
        parse_duration("")
