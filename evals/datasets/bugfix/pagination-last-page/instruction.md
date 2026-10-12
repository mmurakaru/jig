Bug: the last page of a listing is missing.

With 25 items at 10 per page, `page_count` reports 2 pages, and asking for page 3 raises `IndexError`.
The last 5 items cannot be reached.
A listing with a partial last page must count that page and return its items.

The code is in `pager.py`; the tests run with `python -m pytest`.
