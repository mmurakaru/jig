# Writing a skill

A skill is one step's instructions: a plain markdown `SKILL.md` in a folder
named for the skill. jig resolves the name repo-first - `.jig/skills/<name>/`
- then through each directory in the config's `skill_paths`, in order. A repo
shadows an external skill by defining one with the same name. The file's
content reaches the harness byte-for-byte.

## Frontmatter

A leading YAML frontmatter block (between `---` lines) is runner metadata,
not instructions - it is stripped before the body reaches the harness. jig
reads one key:

```markdown
---
tier: mechanical
---
# Run the tests
...
```

`tier` names the cost tier the skill runs on by default; the workflow step
can override it, and `config.yaml` maps tier names to harness commands. A
skill that is mechanical by nature (running tests, capturing output)
declares that here once, instead of in every workflow that uses it. Other
frontmatter keys (a `name:` or `description:` from harness skill
ecosystems) are ignored. A file with no leading `---` has no frontmatter
and passes byte-for-byte.

## The prompt

For each step, jig composes one prompt for the harness:

```
Context:                   (only when the workflow sets `context:`)
<the workflow's constant framing>

Field guide (repo knowledge accumulated by earlier runs):
<.jig/FIELDGUIDE.md>       (only when the file exists and has content)

Task: <the --task description>

Step inputs (bound by the workflow; treat each value as a literal):
<key>: <value>             (only when the workflow binds `with:`)

Human guidance:            (only on the first step after a resume)
<the --guidance text>

Previous handoff:          (from the second step on)
status: ...
<previous step's summary>

<the SKILL.md content>

Protocol: end your reply with a fenced handoff block ...
```

The protocol section is jig's own: it instructs the agent to close its reply
with a handoff block (`status: pass | fail | escalate`, artifact paths, a
summary written for the next agent). A step whose output has no parseable
handoff is recorded as `invalid-handoff` and stops the run - silence is not
success.

The skill's part of the contract is meaning, not mechanics: say what the
statuses mean *for this step* - its completion criterion.

## The field guide

`.jig/FIELDGUIDE.md` is repo knowledge that agents accumulate across runs:
build quirks, hidden dependencies, commands that must run first. When the
file exists, jig reads it from the run's workspace before every skill step
and renders it into the prompt, and the protocol invites the agent to
append durable, non-obvious learnings - one short factual line each. An
append mid-run reaches the very next step. The file's existence is the
opt-in: delete it and jig says nothing about field guides. It is a plain
repo file - versioned, reviewable in PRs, and prunable like any other
document; in an isolated run the worktree's copy travels with the run's
branch. Curate it: entries that restate the code or docs are prompt noise
in every future step.

## Style

- One capability per skill. If the instructions need sections for two
  different jobs, split the skill.
- End on a checkable completion criterion: "done when the failure is
  observable on demand", not "investigate the issue". A vague criterion
  invites the agent to declare victory early.
- Make the criterion exhaustive where it matters: "every statement in the
  spec implemented and covered", not "implement the spec".
- Cut anything the agent does by default; every surviving line should
  change behavior. Prefer one strong word (red, verbatim, minimal) over a
  restated sentence.
- The failure path earns as many words as the success path: say what the
  summary must contain on fail, because it is all the next attempt gets.
- Repo-local skills are versioned with the repository they operate on -
  evolve them in the same PRs as the code. Workflows that depend on
  `skill_paths` libraries fail validation loudly on machines without them.
