# Authoring jig workflows and step-skills

These contracts travel with this skill because consuming repos carry only
`.jig/`, not jig's own documentation.

## Workflows - `.jig/workflows/<name>.yaml`

```yaml
name: bugfix
context: |            # optional constant framing for every step
  ...
steps:
  - command: "./scripts/preflight.sh"   # shell step: exit 0 = pass, $0 cost
    on_fail: escalate                   # escalate | abort (default: fail run)
  - skill: reproduce-issue              # one headless agent invocation
    with:                               # literal string inputs, never evaluated
      area: "auth"
  - retry:                              # bounded self-correction loop
      max_iterations: 3
      on_exhausted: escalate            # escalate | abort
      steps:
        - skill: implement-fix
        - command: "npm test"           # deterministic gate
          until: pass                   # completes the retry group early
  - forEach:                            # bounded fan-out over checked-in data
      items: files.tsv                  # tsv/json next to the workflow
      as: file
      steps:
        - skill: review
          with: { path: "{{ file.path }}" }   # the only interpolation jig does
```

Constraints the validator enforces: unknown keys rejected; retry and
forEach never nest themselves; `until` only inside retry; `with` values are
literals. A workflow directory (`.jig/workflows/<name>/workflow.yaml`) may
co-locate `AGENTS.md` (auto-loaded as context) and the forEach items file.
Always finish with `jig validate <name>`.

Branching belongs inside skills, reported through handoff status - the
schema routes on outcomes and never evaluates expressions.

## Step-skills - `.jig/skills/<name>/SKILL.md`

Flat Markdown, **no frontmatter** - the body reaches the step agent
byte-for-byte (this is a different artifact from harness skills like the
one you are reading). Shape that works:

```markdown
# Reproduce the issue

<what to do, one capability, written for an agent with no other context>

Done when <checkable criterion>: pass = <...>; fail = <...>;
escalate = <what only a human can answer>.
```

Give the fail path as many words as the success path - the failure summary
is what the retry loop reads to do better.

## Verification: never let the agent grade its own homework

- Prefer a `command:` step as the gate after any agent step whose success
  is checkable by machine (`until: pass` on `npm test`, not on a skill's
  self-reported handoff).
- The verifier runs in the tree the agent just edited. Gate on checks the
  agent cannot trivially rewrite: the pre-existing test suite, lint, a
  build - not a test file the previous step created. Treat "the agent
  modified the tests and they now pass" as a review flag.

## Preflight convention

A workflow that needs a live environment opens with:

```yaml
steps:
  - command: "./scripts/preflight.sh"
    on_fail: escalate
```

The script probes everything the run assumes (server health endpoint, deps
installed, creds set) and exits non-zero with a clear message. Failure
pauses the run before any model spend, with the script's output in the
handoff; after fixing, `jig run --resume <id>` re-probes. Share one script
across the repo's workflows.
