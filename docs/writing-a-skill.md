# Writing a skill

A skill is an atomic capability: a folder under `.jig/skills/<name>/`
containing a `SKILL.md`. It knows nothing about workflows, models, or
sandboxes - it is instructions for one step of work.

```
.jig/skills/run-tests/SKILL.md
```

## What jig does with it

For each step, jig builds a prompt and hands it to your configured harness
as the final command argument:

```
Task: <the --task description>

Human guidance:            (only on the first step after a resume)
<the --guidance text>

Previous handoff:          (from the second step on)
status: ...
<previous step's summary>

<your SKILL.md content>
```

## The handoff contract is jig's job, not yours

The prompt envelope ends with jig's protocol section instructing the agent
to close its reply with a fenced handoff block (`status: pass | fail |
escalate`, artifact paths, a summary for the next agent). Your skill file
never restates it - any plain markdown file, including one you already use
elsewhere, works as a skill byte-for-byte. A step that exits without a
parseable handoff is recorded as `invalid-handoff` and stops the run -
silence is not success.

What your skill SHOULD say is what the statuses *mean for this step*: its
completion criterion.

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
- Skills are versioned with the repository they operate on - evolve them in
  the same PRs as the code.
