# jig

A minimal, agnostic runner for AI-driven development workflows.
Declarative workflow format + portable runner with four swappable ports - the primitive, not the product.

jig does not implement an agent. Each step of a workflow is a headless
invocation of the agent harness you already use (`claude -p`, `codex exec`,
any CLI that takes a prompt); jig owns the ordering, the retries, the
escalation to humans, the run history, and the cost log.

## Quickstart

1. Download a binary from the releases page (or `dune build` from source).
2. In your repository:

   ```sh
   jig init --harness claude --skill-paths ~/.claude/skills
   ```

   Scaffolds `.jig/` from the set embedded in the binary: two reference
   workflows (`bugfix`, `feature-development`), their skills, and a working
   harness preset (`claude` or `codex`; bare `jig init` writes a commented
   template). `--skill-paths` points workflows at an existing skill
   library.

3. Point `.jig/config.yaml` at your harness, then:

   ```sh
   jig validate bugfix
   jig run bugfix --task "users can't reset their password when ..."
   jig status <run-id>
   ```

A run walks the workflow step by step. Each step's agent ends with a
**handoff** - status plus a note for the next agent - and jig threads it
into the next step's prompt. Failed steps inside a `retry` block loop with
their failure notes until the tests pass or the budget exhausts; anything
marked `escalate` pauses the run for you:

```sh
jig run --resume <run-id> --guidance "the fix belongs in the parser, not the lexer"
```

## The verbs

```
jig init                                    # scaffold .jig/ from the embedded starter set
jig run <workflow> --task "<description>"   # execute a workflow against a task
jig run --resume <run-id> [--guidance "…"]  # continue a paused run
jig status <run-id> [--json]                # inspect a run
jig list workflows [--json]                 # discover what is runnable
jig validate <workflow>                     # lint before running
```

## Layout

```
.jig/
  config.yaml          # harness command (+ optional sandbox wrapper)
  skills/<name>/SKILL.md
  workflows/<name>.yaml
runs/                  # one JSON record per run + metering.jsonl
```

Everything is a file: skills and workflows are versioned with the code they
operate on, run records are plain JSON a GUI could be built against, and the
binary has no other state.

## Docs

- [Writing a skill](docs/writing-a-skill.md)
- [Writing a workflow](docs/writing-a-workflow.md)
- [Implementing a port](docs/implementing-a-port.md)

## Design

Four ports (Executor, ModelProvider, Metering, Store) behind module
signatures with boring local defaults: subprocess, config lookup, JSONL,
filesystem. Isolation is Executor configuration (`--isolated` runs in a git
worktree per run; an optional config `wrapper` prepends an OS sandbox to
every invocation). The workflow schema is deliberately frozen at ordered
steps + `on_fail` + bounded `retry` - intelligence lives in skills, not in
YAML.
