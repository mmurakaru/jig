#!/bin/bash
set -euo pipefail
cd /app
git apply <<'PATCH'
--- a/duration.py
+++ b/duration.py
@@ -12,5 +12,5 @@ def parse_duration(text: str) -> int:
         raise ValueError(f"not a duration: {text!r}")
     seconds = 0
     for amount, unit in COMPONENT_PATTERN.findall(text):
-        seconds = int(amount) * SECONDS_PER_UNIT[unit]
+        seconds += int(amount) * SECONDS_PER_UNIT[unit]
     return seconds
PATCH
