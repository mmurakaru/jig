# Implement the fix

Using the previous handoff (the failing test, or the last test run's
failures), change the code so the bug is fixed. Keep the change minimal and
in the style of the surrounding code. Do not weaken or delete the test.

If a previous attempt failed, the handoff tells you why - revise the approach
instead of repeating it.

End your reply with a handoff block:

    ```handoff
    status: pass
    artifacts:
      - path/of/changed/files
    summary: |
      What was changed and why this fixes the root cause.
    ```

Report `status: pass` when the fix is in place, `status: escalate` when the
fix requires a decision only a human can make.
