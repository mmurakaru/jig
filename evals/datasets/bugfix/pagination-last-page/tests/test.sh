#!/bin/bash
# Reward 1 only when the hidden tests and the visible suite both pass.
cd /app
export PYTHONPATH=/app
if python -m pytest -q -p no:cacheprovider --rootdir=/app /tests/test_hidden_*.py /app/tests; then
  echo 1 > /logs/verifier/reward.txt
else
  echo 0 > /logs/verifier/reward.txt
fi
