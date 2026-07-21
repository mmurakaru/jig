# Writing a workflow

A workflow binds an ordered list of skills to one task type. It is
validated by `jig validate <name>` and is deliberately boring: the schema
routes on step outcomes and never evaluates expressions.

It lives in one of two places:

- **Flat**: `.jig/workflows/<name>.yaml` - a single file with an inline
  `context:`.
- **Module directory** (preferred when it has inputs):
  `.jig/workflows/<name>/workflow.yaml`, co-locating everything it needs:

  ```
  .jig/workflows/css-module-migration/
    workflow.yaml        # the config
    AGENTS.md            # constant context, auto-loaded (no `context:` key)
    items.tsv            # forEach data, referenced locally as `items.tsv`
    guides/PORTING.md    # reference a skill reads via `with:`
  ```

  A sibling `AGENTS.md` becomes the workflow's context automatically -
  setting both it and an inline `context:` is an error. `forEach items:`
  resolves relative to the workflow's directory, so it stays local.

## The whole schema

```yaml
name: bugfix                     # letters, digits, '.', '_', '-'
context: |                       # optional: constant framing for every step
  What this workflow always does and the invariants it holds.
steps:
  - skill: reproduce-issue       # an agent step (runs the harness)
    on_fail: escalate            # optional: escalate | abort
  - command: "make lint"         # a command step: runs a shell command,
    on_fail: abort               # no agent. exit 0 = pass, nonzero = fail
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
          tier: mechanical       # optional: the cost tier this step runs on
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
- A step runs either a `skill` (an agent) or a `command` (a shell command),
  never both. A command step is for deterministic, judgment-free work: its
  exit status is the outcome (0 = pass, nonzero = fail), it runs no model
  (recorded at $0), and on failure its output threads onward so a retry can
  act on it. It takes `on_fail`/`until` like any step, and interpolates
  `{{ item.* }}` in its command inside a forEach. Reserve agent steps for
  the parts that need reasoning; let commands do capture, diff, lint, etc.
- The workflow's `context`, if set, renders as a constant preamble in every
  step's prompt (every step, every forEach item) - it never varies and
  never threads. Use it for the invariants the whole workflow holds; use
  `with` for per-step facts and the `--task` for the run's specific goal.
- A step's `with` map renders into its prompt as a "Step inputs" section,
  keys in file order. It is how a workflow binds repo facts (a spec path, a
  data file) so the skill itself stays reusable across repositories.
- A step's `fail`: with no `on_fail`, the run fails; `on_fail: escalate`
  pauses it for a human; `on_fail: abort` ends it as aborted.
- A step's `escalate` always pauses the run. Resume with
  `jig run --resume <run-id> [--guidance "..."]`.
- A skill step's `tier` names the cost tier it runs on; `config.yaml` maps
  each tier to a concrete harness command (`tiers:`), so the workflow
  stays portable while the economics stay a local deployment concern.
  Only the moments that need judgment - diagnosis, design, tradeoffs -
  need the default (frontier) harness; mechanical steps like running
  tests or opening a PR run fine on a cheaper tier. Precedence: the
  step's `tier` wins over the skill's own frontmatter `tier`, which wins
  over the default harness. A tier the local config does not map falls
  back to the default harness; `jig validate` warns about it. A command
  step runs no harness, so `tier` is rejected there.
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
  effect on that run. The items file may be produced by an earlier step -
  it is validated at the forEach entry (before the fan-out spends), so an
  absent file is not an up-front error; a present one is still checked up
  front as an early tripwire. A pause resumes at the same item and body step;
  `--skip` completes the current item's current step and automation
  continues with the rest. An empty items file is an error.
- Items are isolated: each item starts fresh, with no handoff from the
  previous item (or the step before the forEach). Threading within an item
  - step-to-step and retry - is unchanged. Per-item context comes from
  `with:`, not from an upstream handoff.

## Where intelligence lives

If you find yourself wanting conditionals, variables, or branching: put that
decision *inside a skill* and report the outcome via the handoff status. The
workflow routes on outcomes; the agent decides within a step. Schema
additions require a recorded design decision - the freeze is the feature.

Recorded decision: `context`, `with`, and `forEach` were added as *data*,
not logic. `context` is the workflow's constant framing; `with` binds
per-step repo facts so skills stay pure; `forEach` is bounded fan-out over a
checked-in list. There are still no expressions, conditionals, or branches
in the schema - routing stays on handoff outcomes, decisions stay inside
skills.
