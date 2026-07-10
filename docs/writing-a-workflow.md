# Writing a workflow

A workflow binds an ordered list of skills to one task type. It lives in
`.jig/workflows/<name>.yaml`, is validated by `jig validate <name>`, and is
deliberately boring: the schema routes on step outcomes and never evaluates
expressions.

## The whole schema

```yaml
name: bugfix                     # letters, digits, '.', '_', '-'
steps:
  - skill: reproduce-issue       # a plain step
    on_fail: escalate            # optional: escalate | abort
  - skill: write-failing-test
    with:                        # optional: literal inputs, string -> string
      spec: docs/specs/login.md  # rendered into the prompt verbatim
  - retry:                       # a bounded self-correction loop
      max_iterations: 3          # required, positive integer
      on_exhausted: escalate     # required: escalate | abort
      steps:
        - skill: implement-fix
        - skill: run-tests
          until: pass            # passing this step completes the group
  - forEach:                     # bounded fan-out over a data file
      items: specs/ports.tsv     # .tsv with a header row, or a .json array of objects
      as: port                   # binds {{ port.<column> }} in this block's with: values
      steps:                     # same shapes as the top level: steps and retry groups
        - retry:
            max_iterations: 3
            on_exhausted: escalate
            steps:
              - skill: implement-port
                with:
                  spec: "{{ port.spec }}"
              - skill: verify-port
                until: pass
  - skill: open-pr
```

That is everything. Unknown keys are rejected; retry blocks and forEach
blocks cannot nest themselves; forEach may contain retry, never the
reverse; `until` is only legal inside a retry; `with` values are literal
strings - jig never evaluates, inlines, or resolves them;
`{{ <as>.<column> }}` is pure substitution, exists only in a forEach body's
`with:` values, and is the only interpolation jig will ever do.

## Semantics

- Steps run in order. Each step receives the previous step's handoff in its
  prompt.
- A step's `with` map renders into its prompt as a "Step inputs" section,
  keys in file order. It is how a workflow binds repo facts (a spec path, a
  data file) so the skill itself stays reusable across repositories.
- A step's `fail`: with no `on_fail`, the run fails; `on_fail: escalate`
  pauses it for a human; `on_fail: abort` ends it as aborted.
- A step's `escalate` always pauses the run. Resume with
  `jig run --resume <run-id> [--guidance "..."]`.
- Inside a retry block, any failing step fails the *iteration*, and its
  handoff threads into the next attempt - that is the self-correction loop.
  The group completes when its `until: pass` step passes (or when all steps
  pass). Exhausting `max_iterations` triggers `on_exhausted`.
- A forEach block runs its body once per item, in order. Items come from a
  checked-in file: TSV with a header row (no quoting - values cannot
  contain tabs or newlines) or a JSON array of objects with string values.
  The first column is the item's *key* - its identity in position lines,
  `jig status`, and step records - and must be non-empty and unique.
- forEach items are resolved once, when the entry starts, and snapshotted
  into the run record: editing the items file while a run is paused has no
  effect on that run. A pause resumes at the same item and body step;
  `--skip` completes the current item's current step and automation
  continues with the rest. An empty items file is an error.

## Where intelligence lives

If you find yourself wanting conditionals, variables, or branching: put that
decision *inside a skill* and report the outcome via the handoff status. The
workflow routes on outcomes; the agent decides within a step. Schema
additions require a recorded design decision - the freeze is the feature.

Recorded decision: `with` and `forEach` were added as *data*, not logic.
`with` binds repo facts so skills stay pure; `forEach` is bounded fan-out
over a checked-in list. There are still no expressions, conditionals, or
branches in the schema - routing stays on handoff outcomes, decisions stay
inside skills.
