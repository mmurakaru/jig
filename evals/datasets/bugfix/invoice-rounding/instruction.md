Bug: invoice totals are sometimes one cent short.

An invoice with 3 items at 0.335 each shows a line total of 1.00, but the expected total is 1.01.
Amounts of half a cent or more must round up to the next cent; smaller fractions round down.

The invoice code is in `invoice.py`; the tests run with `python -m pytest`.
