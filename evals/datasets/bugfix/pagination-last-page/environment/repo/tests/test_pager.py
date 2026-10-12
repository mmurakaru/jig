import pytest

from pager import page, page_count


def test_page_count_for_full_pages():
    assert page_count(20, 10) == 2


def test_page_count_rejects_zero_per_page():
    with pytest.raises(ValueError):
        page_count(5, 0)


def test_first_page():
    assert page(list(range(20)), 1, 10) == list(range(10))


def test_page_zero_does_not_exist():
    with pytest.raises(IndexError):
        page(list(range(20)), 0, 10)
