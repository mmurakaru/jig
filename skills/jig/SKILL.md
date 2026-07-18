---
name: jig
description: >-
  Operate jig, the workflow runner: turn a ticket into a supervised jig run,
  scaffold .jig/ into a repository, or author jig workflows and skills. Use
  when the user mentions jig, names a jig workflow (e.g. "run bugfix for
  issue #42"), or asks to set up jig or write a workflow or skill for it.
  Do not trigger on generic bug-fix or feature requests that don't mention
  jig or a workflow.
---

# jig

jig executes declarative workflows by spawning one headless agent per step.
Your job is to **operate the binary and supervise the run** - never execute
a workflow's YAML yourself. The run record, metering, resume, and pause
semantics exist only through the binary; bypassing it forfeits all of them.

Scaffolding a new repo -> [references/scaffolding.md](references/scaffolding.md).
Writing workflows or step-skills -> [references/authoring.md](references/authoring.md).

## Preconditions

1. `cd "$(git rev-parse --show-toplevel)"` - jig only looks at the cwd.
2. `.jig/` missing? Offer to scaffold: see scaffolding.md.
3. `jig` not on PATH? Offer to install the release binary for the platform
   (or `dune build` inside the jig repo itself). If declined, stop -
   following the workflow manually is not a fallback.

## From ticket to run

1. **Fetch the ticket** with whatever tool is available (`gh issue view`,
   Jira/Linear MCP, or the user's pasted text - any source works).
2. **Condense it into a task**: one paragraph with reproduction hints,
   acceptance criteria, constraints. Prefix the origin reference so the run
   record stays traceable, e.g. `GitHub issue #42: <title> - <condensed>`.
3. **Pick the workflow**: if the user named one, use it. Otherwise run
   `jig list workflows`, infer the fit from the ticket's nature, and
   **confirm your pick with the user before launching** - a run spends money.
4. **Validate, then launch detached**:

   ```sh
   jig validate <workflow>
   jig run <workflow> --task "<condensed task>" --detach
   ```

   Capture the run id from the `detached: <run-id>` line. Foreground
   `jig run` is fine for workflows you expect to finish in a minute or two.
   Use `--isolated` whenever another run (or the user) may touch the same
   working tree.

## Supervise

Poll on a sensible cadence (~30-60s; the log at `.jig/runs/<run-id>.log`
shows live step output):

```sh
jig status <run-id> --json
```

React to `status`:

- `running` - keep polling.
- `completed` - report: per-step outcomes, total cost, the PR link if a
  step opened one, and the run record path.
- `paused` - a step escalated. Triage the last handoff (below).
- `failed` / `aborted` - report the failing step's handoff and stderr tail;
  suggest the fix or a `--resume` if the cause is transient.

## Triage a paused run

Read the last handoff from `jig status <run-id>` output. Then:

- **Answer it yourself** only if the answer is derivable from the repo, the
  ticket, or this conversation - then
  `jig run --resume <run-id> --guidance "<decision + reasoning>"` and tell
  the user what you decided (it is auditable in the run record).
- **Surface it verbatim** if it is destructive, irreversible, credential-
  shaped, product taste, or host-state-owning (e.g. starting the user's dev
  server). Offer the hand-takeover: `jig attach <run-id>` opens the paused
  step's session interactively - that command is the user's, never yours.
- A human finished the step manually? `jig run --resume <run-id> --skip
  --guidance "<what was done>"`.

Preflight failures (a workflow's first command step probing the
environment) pause the same way: fix what is safely mechanical (install
deps, create a directory), resume; leave the rest to the user.

## Report

Always end with: run id, outcome, total cost, artifacts (PR link, changed
paths from the last handoff), and the run record path (`.jig/runs/<id>.json`).
While a run is still executing, also mention `jig watch <run-id>` - it
attaches the live pipeline view in the user's terminal, and quitting it
never affects the run.
