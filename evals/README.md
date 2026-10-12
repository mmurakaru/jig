# jig evals

The evals measure whether a jig workflow solves real tasks, and at what cost.
They run under [Harbor](https://github.com/harbor-framework/harbor) (pinned to `harbor==0.24.0`), outside the jig binary.

Each trial is one agent on one task in its own Docker container.
A task is a tiny repository with one seeded bug, a hidden test, and a reference fix.
Two arms run on the same tasks and the same model:

- `jig`: the `JigAgent` adapter runs `jig run bugfix --task <instruction>` unattended.
- `claude-code (baseline)`: Harbor's built-in Claude Code agent gets the same instruction in one session.

## Run it

You need Docker, [uv](https://docs.astral.sh/uv/), and `ANTHROPIC_API_KEY` in the environment.
From the repository root:

```sh
uv run --project evals jig-evals --model anthropic/claude-sonnet-4-6
```

The command runs both arms on every task, then prints the report.
Job results go to `evals/jobs/<job name>/`; `uv run --project evals harbor view evals/jobs` browses them.
Trial directories hold prompts, code, and harness output, so treat them as sensitive.

Without a model, `--smoke` runs Harbor's oracle agent (applies the reference fix, must score 1) and its no-op agent (changes nothing, must score 0) on every task:

```sh
uv run --project evals jig-evals --smoke
```

Other options:

- `--jig-binary <path>` uploads a Linux x86-64 jig binary, for example one built from this checkout on Linux.
  Without it, the adapter downloads `jig-linux-x86_64` from the newest release (`--jig-release <tag>` picks one).
- `--mechanical-model <model>` maps jig's `mechanical` tier to a second model, to compare tiers.
- `--no-baseline` skips the claude-code arm.
- `--task <name>` (repeatable, globs allowed) runs a subset; `--attempts <k>` runs each task k times per arm.

To print the report again for an existing job: `uv run --project evals python -m jig_evals.eval_report evals/jobs/<job name>` (`--json` for machine output).

The `evals` GitHub Actions workflow runs the same command on manual dispatch only.
It builds the Linux binary from the checkout and reads the `ANTHROPIC_API_KEY` repository secret.

## The report

The report has one block per arm.
A trial is solved when the verifier writes reward 1: the hidden tests and the visible suite both pass.

- `solve rate`: solved trials over all trials.
- `unattended solve rate`: solved trials whose run also finished on its own.
  For jig, the run status must be `completed`: a paused run waits for a human that an unattended eval never has, so it does not count, even when the verifier scores the workspace as fixed.
  For the baseline, the agent must end without an error or a timeout.
- `cost per trial`: mean USD over trials that reported a cost.
  A jig run reports the sum of its steps' metered cost.
  Trials with no cost at all are counted next to it, never treated as free.
- `cost per solved task`: total cost of all trials, failed ones included, over solved trials.
  A cheap run that fails is not a saving.
- `timeouts`: trials stopped by Harbor's agent timeout, or jig runs with a step that timed out; neither counts as solved unattended.
- `solved per task`: solved over attempts for each task.

jig arms also show:

- `cost per tier`: metered cost by tier (`default` is the default harness).
  Steps whose harness reported no cost are counted as unknown-cost steps.
- `retry iterations`: mean number of `run-tests` step records per trial, which is one per iteration of the bugfix retry group.
- `escalation rate`: trials whose run paused.
  `agent-escalated` means a step handed off `status: escalate`; `failure-escalated` means jig escalated a failure (`on_fail: escalate`, or the retry group ran out of iterations).
- `invalid-handoff rate`: steps whose output had no valid handoff block, over all steps, and how many trials had one.

## How the adapter works

`jig_evals/jig_agent.py` is a Harbor installed agent.
During setup it installs the jig binary and the Claude Code CLI in the container.
For the run it scaffolds `.jig/` with `jig init`, writes a `.jig/config.yaml` whose harness is `claude -p` in `bypassPermissions` mode (the container is the sandbox), and runs the workflow.
`.jig/runs/` is a link into the trial's agent log directory, so the run record and `metering.jsonl` survive a timeout.
jig exits 1 on a failed run and 2 on a paused run; the adapter writes that code to `jig-runs/jig-exit-code` and returns normally, so Harbor scores the workspace and never retries a real outcome.
After the trial it puts cost and tokens into Harbor's agent context and the run summary into its metadata.

Adapter options go through `--ak name=value` when you call `harbor run` directly: `workflow`, `jig_binary_path`, `jig_release`, `harness` (a JSON list), `mechanical_model`, and `install_harness`.

## The dataset

`datasets/bugfix/` holds four Python tasks in Harbor's task format (`instruction.md`, `task.toml`, `environment/`, `tests/`, `solution/`):

- `pagination-last-page`: a single-file off-by-one; the control task.
- `duration-parse`: a parser keeps only the last component of `1h30m`.
- `invoice-rounding`: the symptom shows in `invoice.py`, the cause is in `money.py`.
- `settings-merge`: a shallow merge drops nested defaults; a fix that changes its input in place breaks a visible test.

The image holds the repository as a git repository with a first commit, so the `open-pr` step can commit locally.
The hidden tests live in `tests/` and reach the container only after the agent finishes.

## Tests

```sh
cd evals && uv run pytest
```

The tests use run records that a real jig binary wrote against `jig_evals/testdata/scripted_harness.py` (`testdata/record_jig_runs.sh` records them again).
Set `JIG_EVALS_LINUX_JIG_BINARY=<Linux jig binary>` to also run Harbor end to end in Docker with a stub harness: one paused and one completed run, no model.
