Bug: a nested user setting wipes out the other defaults in its section.

With the user setting `{"editor": {"wrap": true}}`, the effective `editor` section is `{"wrap": true}`.
The default `tab_width` of 4 is lost; the expected section is `{"tab_width": 4, "wrap": true}`.
Nested sections must merge key by key at any depth, lists must still be replaced, and merging must never change the defaults or the user settings.

The code is in `settings.py`; the tests run with `python -m pytest`.
