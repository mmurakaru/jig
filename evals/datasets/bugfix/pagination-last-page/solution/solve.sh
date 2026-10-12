#!/bin/bash
set -euo pipefail
cd /app
git apply <<'PATCH'
--- a/pager.py
+++ b/pager.py
@@ -5,7 +5,7 @@ def page_count(total_items: int, per_page: int) -> int:
     """Return how many pages hold total_items at per_page items each."""
     if per_page < 1:
         raise ValueError("per_page must be at least 1")
-    return total_items // per_page
+    return -(-total_items // per_page)


 def page(items: list, number: int, per_page: int) -> list:
PATCH
