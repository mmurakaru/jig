"""Harbor installed agent that runs a jig workflow inside the task container.

Select it with `--agent jig_evals.jig_agent:JigAgent`. The agent installs a
Linux jig binary and the Claude Code CLI, scaffolds `.jig/` in the task's
working directory, runs `jig run <workflow> --task <instruction>`, and keeps
the run record and metering ledger under the trial's agent logs.
"""

import asyncio
import json
import shlex
import urllib.request
from pathlib import Path
from typing import override

from harbor.agents.installed.base import BaseInstalledAgent
from harbor.agents.options import InstalledAgentOptions
from harbor.environments.base import BaseEnvironment
from harbor.models.agent.context import AgentContext
from pydantic import Field

from jig_evals.jig_run_summary import JIG_EXIT_CODE_FILE, summarize_jig_run

JIG_REPOSITORY = "mmurakaru/jig"
JIG_LINUX_ASSET = "jig-linux-x86_64"
JIG_INSTALL_PATH = "/usr/local/bin/jig"
JIG_RELEASE_CACHE = Path.home() / ".cache" / "jig-evals"

# Kept in the agent log directory, so the record survives a Harbor timeout.
JIG_RUNS_LOG_NAME = "jig-runs"
JIG_OUTPUT_LOG_NAME = "jig-output.txt"

CLAUDE_CODE_BOOTSTRAP_URL = "https://downloads.claude.ai/claude-code-releases/bootstrap.sh"
HARNESS_AUTH_ENV_NAMES = ("ANTHROPIC_API_KEY", "CLAUDE_CODE_OAUTH_TOKEN", "ANTHROPIC_BASE_URL")


class JigAgentOptions(InstalledAgentOptions):
    """Options passed with `--ak name=value`."""

    workflow: str = Field(default="bugfix", description="jig workflow to run.")
    jig_binary_path: str | None = Field(
        default=None, description="Host path to a Linux x86-64 jig binary; skips the download."
    )
    jig_release: str = Field(
        default="latest", description="Release tag to download, or latest (pre-releases count)."
    )
    harness: list[str] | None = Field(
        default=None, description="Harness argv for .jig/config.yaml; defaults to Claude Code."
    )
    mechanical_model: str | None = Field(
        default=None, description="Model for the mechanical tier; unset keeps one harness."
    )
    install_harness: bool = Field(default=True, description="Install the Claude Code CLI.")


def claude_harness_command(model: str | None) -> list[str]:
    """Return the headless Claude Code argv; the container is the sandbox, so permissions are bypassed."""
    model_flags = ["--model", model] if model else []
    return [
        "claude",
        "-p",
        *model_flags,
        "--output-format",
        "json",
        "--disallowedTools",
        "EnterWorktree,ExitWorktree",
        "--permission-mode",
        "bypassPermissions",
    ]


def render_jig_eval_config(harness: list[str], mechanical_harness: list[str] | None) -> str:
    """Return a .jig/config.yaml; JSON flow sequences are valid YAML."""
    lines = [f"harness: {json.dumps(harness)}"]
    if mechanical_harness is not None:
        lines += ["tiers:", f"  mechanical: {json.dumps(mechanical_harness)}"]
    return "\n".join(lines) + "\n"


def jig_run_command(workflow: str, agent_logs_directory: str) -> str:
    """Return the shell command that runs jig and always exits 0.

    jig exits 1 (failed) or 2 (paused) on a real outcome; Harbor would treat
    that as an agent error and might retry it, so the exit code goes to a file.
    The task text comes from $JIG_EVAL_TASK.
    """
    runs_log = shlex.quote(f"{agent_logs_directory}/{JIG_RUNS_LOG_NAME}")
    output_log = shlex.quote(f"{agent_logs_directory}/{JIG_OUTPUT_LOG_NAME}")
    exit_code_file = shlex.quote(f"{agent_logs_directory}/{JIG_RUNS_LOG_NAME}/{JIG_EXIT_CODE_FILE}")
    return (
        f"mkdir -p {runs_log} && rm -rf .jig/runs && ln -s {runs_log} .jig/runs; "
        f'jig run {shlex.quote(workflow)} --task "$JIG_EVAL_TASK" > {output_log} 2>&1; '
        f"echo $? > {exit_code_file}"
    )


def resolve_latest_jig_release() -> str:
    """Return the newest jig release tag on GitHub, pre-releases included."""
    url = f"https://api.github.com/repos/{JIG_REPOSITORY}/releases?per_page=1"
    with urllib.request.urlopen(url, timeout=30) as response:
        releases = json.load(response)
    if not releases:
        raise RuntimeError(f"jig release lookup found no releases at {url}")
    return releases[0]["tag_name"]


def download_jig_release_binary(tag: str) -> Path:
    """Download the Linux jig binary of a release into the host cache once."""
    target = JIG_RELEASE_CACHE / tag / JIG_LINUX_ASSET
    if target.exists():
        return target
    target.parent.mkdir(parents=True, exist_ok=True)
    url = f"https://github.com/{JIG_REPOSITORY}/releases/download/{tag}/{JIG_LINUX_ASSET}"
    partial = target.with_suffix(".partial")
    urllib.request.urlretrieve(url, partial)
    partial.rename(target)
    return target


class JigAgent(BaseInstalledAgent):
    """Runs `jig run <workflow>` unattended; a paused or failed run is a result, not an error."""

    options_model = JigAgentOptions
    options: JigAgentOptions

    @staticmethod
    @override
    def name() -> str:
        return "jig"

    async def resolve_jig_binary(self) -> Path:
        """Return the host path of the jig binary to upload, downloading it if needed."""
        if self.options.jig_binary_path is not None:
            self._version = "local"
            return Path(self.options.jig_binary_path).expanduser().resolve()
        tag = self.options.jig_release
        if tag == "latest":
            tag = await asyncio.to_thread(resolve_latest_jig_release)
        self._version = tag
        return await asyncio.to_thread(download_jig_release_binary, tag)

    @override
    async def install(self, environment: BaseEnvironment) -> None:
        await self.ensure_system_dependencies(environment, ("curl", "bash", "ca_certificates", "git"))
        jig_binary = await self.resolve_jig_binary()
        await environment.upload_file(jig_binary, JIG_INSTALL_PATH)
        await self.exec_as_root(environment, f"chmod 755 {JIG_INSTALL_PATH} && jig --version")
        if self.options.install_harness:
            await self.exec_as_agent(
                environment,
                f"set -euo pipefail; curl -fsSL {CLAUDE_CODE_BOOTSTRAP_URL} | bash && "
                'export PATH="$HOME/.local/bin:$PATH" && claude --version',
            )

    def harness_environment(self, instruction: str) -> dict[str, str]:
        """Return the env for jig and its harness: auth from the host, never from task files."""
        environment_variables = {
            name: value for name in HARNESS_AUTH_ENV_NAMES if (value := self._get_env(name))
        }
        environment_variables["IS_SANDBOX"] = "1"
        environment_variables["CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC"] = "1"
        environment_variables["JIG_EVAL_TASK"] = instruction
        return environment_variables

    def eval_config_text(self) -> str:
        """Return the .jig/config.yaml for this trial's options and model."""
        harness = self.options.harness or claude_harness_command(self._parsed_model_name)
        mechanical_harness = (
            claude_harness_command(self.options.mechanical_model)
            if self.options.mechanical_model
            else None
        )
        return render_jig_eval_config(harness, mechanical_harness)

    @override
    async def run(self, instruction: str, environment: BaseEnvironment, context: AgentContext) -> None:
        agent_logs_directory = self.environment_logs_dir.as_posix()
        working_directory = await self.exec_as_agent(environment, "jig init --harness claude && pwd")
        await self._upload_config_text(
            environment,
            content=self.eval_config_text(),
            remote_path=f"{working_directory.stdout.strip().splitlines()[-1]}/.jig/config.yaml",
            filename="config.yaml",
        )
        await self.exec_as_agent(
            environment,
            'export PATH="$HOME/.local/bin:$PATH"; '
            + jig_run_command(self.options.workflow, agent_logs_directory),
            env=self.harness_environment(self.render_instruction(instruction)),
        )

    @override
    def populate_context_post_run(self, context: AgentContext) -> None:
        summary = summarize_jig_run(self.logs_dir / JIG_RUNS_LOG_NAME)
        context.cost_usd = summary.cost_usd
        context.n_input_tokens = summary.input_tokens
        context.n_cache_tokens = summary.cache_tokens
        context.n_output_tokens = summary.output_tokens
        context.metadata = {**(context.metadata or {}), "jig": summary.to_metadata()}
