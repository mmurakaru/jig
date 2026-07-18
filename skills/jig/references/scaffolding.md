# Scaffolding a repository for jig

## Init

```sh
jig init --harness claude --skill-paths ~/.claude/skills
```

- `--harness claude|codex` writes a known-good config for that harness;
  `custom` (default) writes a commented template the user must edit.
- `--skill-paths DIR` (repeatable) points workflows at external skill
  libraries; repo-local `.jig/skills/` always wins on name clashes.
- `jig init` refuses to touch an existing `.jig/` - it bootstraps, never
  merges.

The scaffold ships two workflows (`bugfix`, `feature-development`) and the
eight step-skills they reference.

## config.yaml essentials

- `harness:` (required) - the headless agent command, one list item per
  argument. The step prompt is appended as the final argument, so never end
  with a flag expecting a value. Structured JSON output enables cost
  metering.
- `wrapper:` - prepended to every harness invocation (OS sandbox).
- `attach:` / `attach_headless:` - interactive and headless session-reopen
  commands; `{session_id}` is substituted per step.
- `notify:` - run on every run stop; `{run_id}` and `{status}` substituted.

## Prove the wiring before the first real run

`jig validate <workflow>` checks structure, not the harness. After
scaffolding, run a throwaway workflow once so a config typo surfaces on a
run that costs nothing:

```yaml
# .jig/workflows/smoke.yaml
name: smoke
steps:
  - command: "echo jig-wiring-ok"
```

```sh
jig run smoke --task "smoke test"
```

A completed run proves: binary works, run records write, command steps
execute. To also prove harness invocation and handoff parsing, add a
one-line skill step and confirm the step's outcome is `pass`. Delete
`smoke.yaml` afterwards or keep it as the repo's health check.

## Environment prerequisites

Workflows that need a live environment (dev server, database, creds)
declare it as their first step - see the preflight convention in
[authoring.md](authoring.md). If a run pauses on a preflight failure, the
fix belongs to whoever owns that state: install-like gaps are yours,
host-state and credentials are the user's.
