"""Split a list of items into numbered pages."""


def page_count(total_items: int, per_page: int) -> int:
    """Return how many pages hold total_items at per_page items each."""
    if per_page < 1:
        raise ValueError("per_page must be at least 1")
    return total_items // per_page


def page(items: list, number: int, per_page: int) -> list:
    """Return the items on page `number`, counting pages from 1."""
    if number < 1 or number > page_count(len(items), per_page):
        raise IndexError(f"page {number} does not exist")
    start = (number - 1) * per_page
    return items[start : start + per_page]
