# Tier audit of the reference workflows

## Question

Audit the embedded `bugfix` and `feature-development` workflows and their skills against the finding from recent multi-agent cost research that only decomposition, design decisions, and tradeoff moments need frontier-model intelligence, while execution against explicit instructions can run on cheaper models.
For each step, classify it as judgment (frontier tier) or mechanical (cheap tier) and give the rationale.

## Method

Each workflow step maps to one skill under `template/skills/`.
The classification key is whether the step's "done when" bar can be met by following an explicit handoff, or whether meeting it requires the model to form a hypothesis, resolve an ambiguity, or choose between approaches.
Steps that read handoffs and act on them mechanically go to the cheap tier.
Steps whose output the whole run is built on, and where a wrong-but-confident answer is expensive to unwind, go to the frontier tier.

## bugfix workflow

| Step | Proposed tier | Rationale |
| --- | --- | --- |
| reproduce-issue | frontier | The step has to explore an unfamiliar repository, decide what command or minimal script exposes the fault, and name where the fault likely lives. That is diagnosis, not execution: there is no explicit instruction to follow, only a symptom to reason back from. A weak model here produces a shallow or wrong "where it lives", and every later step inherits that mistake. |
| write-failing-test | frontier | Turning a reproduction into a test that goes red for the bug's reason requires understanding the causal mechanism, not just re-running the reproduction. The handoff describes the failure, but expressing it as a single assertion that is red now and green after the fix is a modeling judgment. A weak model writes a test that is red for the wrong reason, which sends the whole retry loop chasing a phantom. This is the lightest of the frontier steps and the first candidate to revisit if the reproduce-issue handoff is made richer. |
| implement-fix | frontier | The step chooses a root-cause fix, keeps it minimal, and revises its approach from the previous attempt's failure. Choosing between fixes and diagnosing why the last attempt failed is exactly the tradeoff reasoning the research reserves for frontier models. A weak model patches the symptom or repeats a failed approach, burning retry iterations. |
| run-tests | cheap | The instruction is fully explicit: run the suite, report results verbatim, do not rationalize a failure away. There is no decision to make and no synthesis to perform. A cheap model is arguably safer here, because the failure mode of a stronger model is over-interpreting or explaining away a red result, which this step explicitly forbids. |
| open-pr | cheap | Committing on a branch and opening a PR is a fixed procedure, and the body is assembled from prior handoffs that already name the problem, root cause, and change. The only judgment is light narrative synthesis of inputs that already exist. A weak model produces a blander PR body but does not corrupt the change or the verification. |

## feature-development workflow

| Step | Proposed tier | Rationale |
| --- | --- | --- |
| clarify-requirements | frontier | The step reads a ticket and code, then writes the requirement as testable statements with explicit scope and non-regression boundaries, escalating the moment two readings survive. This is decomposition and ambiguity resolution, the highest-value judgment in the whole workflow, and its output is the spec every later step implements against. A weak model writes a vague or wrong spec and the feature is built confidently in the wrong direction. |
| implement-feature | frontier | Building the spec test-first, matching surrounding style, reusing existing helpers, and revising from a prior failed attempt are all design decisions made against a codebase, not execution of explicit steps. A weak model reimplements what already exists, misreads the spec, or repeats a failed approach. |
| run-tests | cheap | Same as the bugfix run-tests step: an explicit run-and-report instruction with no decision and an anti-rationalization rule that favors a plainer model. |
| capture-evidence | cheap | Demonstrating the feature end-to-end and saving artifacts follows a recipe: run it the way a user would, capture the transcript or screenshot. The one judgment is choosing the user-facing path that matters, which is well constrained once the spec and implementation exist. A weak model may pick a less representative path but does not affect correctness of the code. |
| open-pr | cheap | Identical to the bugfix open-pr step: fixed procedure, body assembled from existing handoffs, only light narrative synthesis at stake. |

## Summary

Frontier tier: reproduce-issue, write-failing-test, implement-fix (bugfix); clarify-requirements, implement-feature (feature-development).
Cheap tier: run-tests, open-pr (both workflows); capture-evidence (feature-development).
The split lines up with the research: the frontier steps are the ones that diagnose, decompose, or choose an approach, and the cheap steps execute against instructions that a prior step or the skill itself already made explicit.

## Open questions

- write-failing-test sits on the boundary. Its tier depends on how much diagnosis the reproduce-issue handoff carries. If reproduction hands over the exact causal mechanism, the test becomes closer to mechanical transcription and could drop to a mid or cheap tier. Worth testing empirically rather than fixing by assertion.
- The two tiers may be too coarse. Several steps (write-failing-test, open-pr, capture-evidence) read as a middle band rather than a clean frontier-or-cheap split. A three-tier model may map the work more honestly.
- The retry loops wrap a frontier step (implement-fix, implement-feature) and a cheap step (run-tests) together. It is worth confirming the tier is assigned per step inside the loop, not per loop, so retries do not force the cheap step onto an expensive model.
- The on_fail and on_exhausted escalations route to a human. Whether an escalation should first retry on a higher tier before reaching a human is a separate routing decision this audit does not settle.
- Tier assignment is static here. A step that usually runs cheap (open-pr) but hits an unexpected conflict may need to escalate its own tier. Dynamic per-run escalation is out of scope for this audit.
