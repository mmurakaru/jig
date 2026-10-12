#!/bin/bash
set -euo pipefail
cd /app
git apply <<'PATCH'
--- a/money.py
+++ b/money.py
@@ -1,11 +1,11 @@
 """Convert decimal amounts to whole cents."""

-from decimal import Decimal
+from decimal import ROUND_HALF_UP, Decimal


 def to_cents(amount: Decimal) -> int:
     """Return the amount in whole cents, rounding half a cent up."""
-    return int(amount * 100)
+    return int((amount * 100).quantize(Decimal(1), rounding=ROUND_HALF_UP))


 def format_cents(cents: int) -> str:
PATCH
