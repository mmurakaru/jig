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

## The one obligation: end with a handoff

Every skill must instruct the agent to end its reply with a fenced handoff
block. The handoff is how the run continues - jig parses the last such block
in the step's output:

    ```handoff
    status: pass | fail | escalate
    artifacts:
      - paths/the/step/produced
    summary: |
      Prose written for the NEXT agent: what happened, what to look at,
      what to do differently.
    ```

Rules that make handoffs useful:

- `status` is required. `pass` continues the workflow, `fail` stops it (or
  retries, inside a retry block), `escalate` pauses the run for a human.
- Reference artifacts by path; never paste file contents into the summary -
  the workspace already has them.
- Write the summary for its reader: the next step's agent, or the human who
  gets the escalation.
- A step that exits without a parseable handoff is recorded as
  `invalid-handoff` and stops the run - silence is not success.

## Style

- One capability per skill. If the instructions need sections for two
  different jobs, split the skill.
- Say what "done" means and what honest failure looks like. Agents follow
  incentives you write down.
- Skills are versioned with the repository they operate on - evolve them in
  the same PRs as the code.
