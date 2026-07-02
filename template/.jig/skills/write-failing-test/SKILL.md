# Write a failing test

Using the previous handoff's reproduction notes, add a test that captures the
bug: it must fail on the current code for the same reason the reproduction
failed, and it must be a test that should pass once the bug is fixed. Run it
to confirm it fails. Do not fix the bug itself.

End your reply with a handoff block:

    ```handoff
    status: pass
    artifacts:
      - path/to/the-new-test
    summary: |
      Which test was added, how to run it, and what it asserts.
    ```

Report `status: pass` when the new test fails for the right reason,
`status: fail` when you could not express the bug as a test.
