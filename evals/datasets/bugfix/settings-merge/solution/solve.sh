#!/bin/bash
set -euo pipefail
cd /app
git apply <<'PATCH'
--- a/settings.py
+++ b/settings.py
@@ -1,5 +1,7 @@
 """Layer user settings over default settings."""

+import copy
+
 DEFAULT_SETTINGS = {
     "theme": "light",
     "editor": {"tab_width": 4, "wrap": False},
@@ -13,7 +15,13 @@ def merge_settings(base: dict, override: dict) -> dict:
     Nested mappings merge key by key; any other override value (lists too)
     replaces the base value.
     """
-    return {**base, **override}
+    merged = copy.deepcopy(base)
+    for key, value in override.items():
+        if isinstance(value, dict) and isinstance(merged.get(key), dict):
+            merged[key] = merge_settings(merged[key], value)
+        else:
+            merged[key] = copy.deepcopy(value)
+    return merged


 def effective_settings(user_settings: dict) -> dict:
PATCH
