# Run the tests

Run the repository's test suite (or, at minimum, the test added for this
task). Report honestly - do not rationalize failures away.

End your reply with a handoff block:

    ```handoff
    status: pass
    summary: |
      What was run and the outcome. On failure: which tests failed and the
      relevant error output, so the next attempt knows what to fix.
    ```

Report `status: pass` only when the tests pass. Report `status: fail` when
any test fails - the workflow loops back to the fix step with your summary.
