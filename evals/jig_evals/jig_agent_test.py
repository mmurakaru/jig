import subprocess
from pathlib import Path

import pytest
import yaml

from jig_evals.jig_agent import JIG_OUTPUT_LOG_NAME, JIG_RUNS_LOG_NAME, JigAgent, jig_run_command
from jig_evals.jig_run_summary import summarize_jig_run


@pytest.mark.parametrize("jig_exit_code", [0, 1, 2])
def test_run_command_exits_zero_and_records_the_jig_exit_code(tmp_path, jig_exit_code):
    fake_bin_directory = tmp_path / "bin"
    fake_bin_directory.mkdir()
    fake_jig = fake_bin_directory / "jig"
    fake_jig.write_text(
        "#!/bin/sh\n"
        'printf \'{"id": "run-1", "status": "paused", "steps": []}\' > .jig/runs/run-1.json\n'
        'echo "task was: $JIG_EVAL_TASK"\n'
        f"exit {jig_exit_code}\n"
    )
    fake_jig.chmod(0o755)
    workspace = tmp_path / "workspace"
    (workspace / ".jig" / "runs").mkdir(parents=True)
    logs = tmp_path / "logs"

    completed = subprocess.run(
        ["bash", "-c", "set -o pipefail; " + jig_run_command("bugfix", str(logs))],
        cwd=workspace,
        env={"PATH": f"{fake_bin_directory}:/usr/bin:/bin", "JIG_EVAL_TASK": "fix 'the' bug"},
    )

    assert completed.returncode == 0
    summary = summarize_jig_run(logs / JIG_RUNS_LOG_NAME)
    assert summary.exit_code == jig_exit_code
    assert summary.run_id == "run-1"
    assert (logs / JIG_OUTPUT_LOG_NAME).read_text() == "task was: fix 'the' bug\n"


def make_agent(tmp_path: Path, **options) -> JigAgent:
    return JigAgent(logs_dir=tmp_path, model_name="anthropic/claude-sonnet-4-6", **options)


def test_eval_config_runs_claude_headless_on_the_trial_model(tmp_path):
    config = yaml.safe_load(make_agent(tmp_path).eval_config_text())
    harness = config["harness"]
    assert harness[:2] == ["claude", "-p"]
    assert harness[harness.index("--model") + 1] == "claude-sonnet-4-6"
    # jig appends the prompt last, so the list must not end on a multi-value flag.
    assert harness[-2:] == ["--permission-mode", "bypassPermissions"]
    assert "tiers" not in config


def test_mechanical_model_adds_a_mechanical_tier(tmp_path):
    config = yaml.safe_load(make_agent(tmp_path, mechanical_model="claude-haiku-4-5").eval_config_text())
    mechanical = config["tiers"]["mechanical"]
    assert mechanical[mechanical.index("--model") + 1] == "claude-haiku-4-5"


def test_custom_harness_replaces_claude(tmp_path):
    config = yaml.safe_load(make_agent(tmp_path, harness=["sh", "-c", "echo ok"]).eval_config_text())
    assert config["harness"] == ["sh", "-c", "echo ok"]


def test_harness_environment_passes_auth_from_the_host_only(tmp_path, monkeypatch):
    monkeypatch.setenv("ANTHROPIC_API_KEY", "test-key")
    monkeypatch.delenv("CLAUDE_CODE_OAUTH_TOKEN", raising=False)
    monkeypatch.setenv("UNRELATED_SECRET", "nope")
    environment = make_agent(tmp_path).harness_environment("the task")
    assert environment["ANTHROPIC_API_KEY"] == "test-key"
    assert "CLAUDE_CODE_OAUTH_TOKEN" not in environment
    assert "UNRELATED_SECRET" not in environment
    assert environment["JIG_EVAL_TASK"] == "the task"
    assert environment["IS_SANDBOX"] == "1"


def test_unknown_option_is_rejected(tmp_path):
    with pytest.raises(ValueError, match="Unknown option .no_such_option."):
        make_agent(tmp_path, no_such_option=True)
