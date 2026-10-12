import pytest

from pager import page, page_count


def test_partial_last_page_is_counted():
    assert page_count(25, 10) == 3


def test_partial_last_page_holds_the_remaining_items():
    assert page(list(range(25)), 3, 10) == [20, 21, 22, 23, 24]


def test_no_items_means_no_pages():
    assert page_count(0, 10) == 0


def test_page_after_the_last_does_not_exist():
    with pytest.raises(IndexError):
        page(list(range(25)), 4, 10)
