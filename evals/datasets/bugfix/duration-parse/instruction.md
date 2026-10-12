Bug: durations with more than one unit are parsed wrong.

`parse_duration("1h30m")` returns 1800, but 1 hour and 30 minutes is 5400 seconds.
Single-unit durations such as `90s` are correct.
Every component of a duration must count toward the total.

The parser is in `duration.py`; the tests run with `python -m pytest`.
