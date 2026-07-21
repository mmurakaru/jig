# Model selection across harness CLIs

Wayfinder research for [issue #107](https://github.com/mmurakaru/jig/issues/107), feeding the map at [issue #100](https://github.com/mmurakaru/jig/issues/100).

## Question

How is model selection expressed in the headless-agent CLIs jig targets, and are the tier profiles shipped in `jig init` presets correct?
Flag names and model identifiers were verified against `--help` output from the installed CLI and against official docs.
Pre-trained knowledge was not trusted for any flag or model name.

## jig's invocation contract

jig appends the step prompt as the final positional argument of the `harness` command.
Two rules follow from `template/config.yaml`:
the harness list must not end with a flag that takes multiple values, or that flag swallows the prompt;
structured JSON output is what lets jig meter cost and tokens.
Each CLI below is scored against those two rules.

## Claude Code

Verified against the installed CLI (`claude --version` reports `2.1.215 (Claude Code)`) via `claude --help`, plus a live `claude -p --output-format json` run.

Model selection uses `--model <model>`.
The help text states the value is either an alias for the latest model (`fable`, `opus`, `sonnet`) or a model's full name (for example `claude-fable-5`).
`--fallback-model <model>` accepts a comma-separated list of models to try when the primary is overloaded or unavailable, and only works with `--print`.

Current full model identifiers (from the harness environment's model list):
`claude-opus-4-8`, `claude-sonnet-5`, `claude-haiku-4-5-20251001`, `claude-fable-5`.
A live run confirmed `claude-fable-5` and `claude-haiku-4-5-20251001` appear verbatim as slugs in the CLI's own `modelUsage` output.

Structured output uses `--output-format <format>` with choices `text` (default), `json` (single result object), and `stream-json`.
The `json` output is a single object rich with metering fields.
A real run returned `total_cost_usd`, a `usage` object (`input_tokens`, `output_tokens`, `cache_creation_input_tokens`, `cache_read_input_tokens`), and a `modelUsage` map keyed by model slug where each entry carries `inputTokens`, `outputTokens`, `costUSD`, `contextWindow`, and `maxOutputTokens`.
This is the only surveyed CLI that reports cost directly in USD.

Invocation-contract fit: the prompt is a trailing positional argument, so it appends cleanly.
The shipped `template/config.yaml` ends the harness list with `--permission-mode acceptEdits`, a single-value flag, which is safe.
The `--disallowedTools "EnterWorktree,ExitWorktree"` entry passes a single comma-separated value, so it also does not swallow the prompt.
Gotcha: `--allowedTools`/`--disallowedTools` are declared as multi-value (`<tools...>`), so passing them space-separated as the last flag would consume the prompt; jig sidesteps this by using the comma-separated single-token form and placing the single-value `--permission-mode` last.

## OpenAI Codex CLI

Verified against official docs at developers.openai.com/codex (served via learn.chatgpt.com).
Codex is not installed locally, so `--help` could not be captured.

The non-interactive entry point is `codex exec`.
The prompt is passed as the final positional argument, for example `codex exec "summarize the repository structure"`.
Alternatively stdin supplies the prompt with the `-` sentinel, for example `cat prompt.txt | codex exec -`.

Model selection uses `--model` (short form `-m`), for example `codex exec -m gpt-5.6-sol "..."`.
It can also be set in `~/.codex/config.toml` with `model = "..."`, overridden generically with `-c/--config model=...`, or grouped into `[profiles.NAME]` blocks selected with `--profile NAME`.
Explicit launch flags take precedence over config and profile defaults.

Current model identifiers (from the Codex models doc):
recommended are `gpt-5.6-sol` (flagship, and the primary default under the standard Power setting), `gpt-5.6-terra` (balanced), and `gpt-5.6-luna` (fast/affordable);
also available are `gpt-5.5`, `gpt-5.4`, `gpt-5.4-mini`, and the text-only research preview `gpt-5.3-codex-spark`.
`gpt-5.2` and `gpt-5.3-codex` are deprecated when signing in with ChatGPT.

Structured output uses `--json`, which turns stdout into a JSON Lines (JSONL) stream of events rather than a single object.
`-o/--output-last-message <path>` writes only the final message to a file.
Token usage appears in the `turn.completed` event, whose `usage` object carries `input_tokens`, `cached_input_tokens`, `output_tokens`, and `reasoning_output_tokens`.
Codex reports tokens but not a dollar cost, so jig would compute cost from token counts and its own price table.

Invocation-contract fit: prompt-as-final-positional works natively.
Gotcha for metering: unlike Claude's single `json` object, Codex `--json` is JSONL, so jig must scan the stream for the `turn.completed` event instead of parsing one result object.

## Gemini CLI

Verified against the official Gemini CLI docs (geminicli.com and the google-gemini/gemini-cli repo).
Gemini CLI is not installed locally, so `--help` could not be captured.

Headless mode is triggered by a non-TTY environment or by supplying `-p/--prompt`.
The prompt can be given via `-p/--prompt` or as a positional argument (`gemini "query"`);
the reference notes `--prompt` text is appended to stdin input when both are present.

Model selection uses `--model` (short form `-m`).
Documented aliases are `auto` (default; resolves to `gemini-2.5-pro` or `gemini-3-pro-preview`), `pro`, `flash` (`gemini-2.5-flash`), and `flash-lite` (`gemini-2.5-flash-lite`).
Documented concrete names include `gemini-3-pro-preview`, `gemini-3-flash-preview`, `gemini-2.5-pro`, `gemini-2.5-flash`, and `gemini-2.5-flash-lite`.

Structured output uses `--output-format` (short form `-o`) with choices `text` (default), `json`, and `stream-json`.
The `json` format returns a single object containing the response plus a `stats` field with token usage and API latency metrics.
`stream-json` emits newline-delimited events (`init`, `message`, `tool_use`, `tool_result`, `error`, `result`), and its final `result` event carries aggregated stats and per-model token usage breakdowns.
Usage is reported in tokens; a dollar cost is not documented.

Invocation-contract fit: prompt-as-final-positional works.
Gotcha: `-o` means `--output-format` here, whereas in Codex `-o` means `--output-last-message` - do not carry a short flag across presets.
Because the prompt appends to stdin input, passing it positionally (jig's contract) is cleaner than mixing stdin and a trailing `-p` value.

## Cross-CLI summary

| CLI | Model flag | Structured output | Cost in output | Prompt as final positional |
| --- | --- | --- | --- | --- |
| Claude Code | `--model` (alias or full slug) | `--output-format json` (single object) | Yes, `total_cost_usd` + per-model `costUSD` | Yes |
| Codex | `--model`/`-m`, config `model`, `--profile` | `--json` (JSONL events) | No, tokens only (`turn.completed.usage`) | Yes |
| Gemini | `--model`/`-m` (aliases + slugs) | `--output-format json`/`stream-json` | No, tokens only (`stats`) | Yes |

All three accept the prompt as a trailing positional argument, so all three fit jig's invocation contract.
Only Claude Code emits a dollar cost directly;
for Codex and Gemini, jig meters from token counts.

## Sources

- Claude Code: `claude --help` and a live `claude -p --output-format json` run, CLI version 2.1.215 (installed locally).
- Claude model identifiers: harness environment model list (Opus 4.8 `claude-opus-4-8`, Sonnet 5 `claude-sonnet-5`, Haiku 4.5 `claude-haiku-4-5-20251001`, Fable 5 `claude-fable-5`); `claude-fable-5` and `claude-haiku-4-5-20251001` also confirmed verbatim in the live `modelUsage` output.
- Codex non-interactive mode and `--json`/`-o`: https://learn.chatgpt.com/docs/non-interactive-mode
- Codex model flag, config key, `-c/--config`, and profiles: https://learn.chatgpt.com/docs/config-file/config-reference.md
- Codex model identifiers: https://learn.chatgpt.com/docs/models
- Gemini CLI headless mode and output formats: https://geminicli.com/docs/cli/headless/
- Gemini CLI flags (`-m/--model`, `-p/--prompt`, `--output-format`): https://geminicli.com/docs/cli/cli-reference/
- Gemini CLI model aliases and slugs: https://github.com/google-gemini/gemini-cli/blob/main/docs/cli/model.md

Confidence: high for Claude Code (verified against the installed binary) and for the Codex/Gemini flag names and model slugs quoted from official docs;
medium for whether every listed Codex/Gemini model is enabled for a given account, since availability is entitlement-dependent and was not exercised against a live key.
