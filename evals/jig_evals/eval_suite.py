"""Run the jig eval suite with Harbor and print the report."""

import argparse
import json
import subprocess
import sys
import tempfile
from datetime import datetime
from pathlib import Path

from jig_evals.eval_report import build_eval_report, load_trial_outcomes, render_eval_report

EVALS_DIRECTORY = Path(__file__).resolve().parent.parent
BUGFIX_DATASET = EVALS_DIRECTORY / "datasets" / "bugfix"
DEFAULT_JOBS_DIRECTORY = EVALS_DIRECTORY / "jobs"
JIG_AGENT_IMPORT_PATH = "jig_evals.jig_agent:JigAgent"
BASELINE_AGENT_NAME = "claude-code"


def build_agent_configs(options: argparse.Namespace) -> list[dict]:
    """Return the Harbor agent entries: one per arm, all on the same model."""
    if options.smoke:
        return [{"name": "oracle"}, {"name": "nop"}]
    jig_kwargs: dict = {}
    if options.jig_binary is not None:
        jig_kwargs["jig_binary_path"] = str(Path(options.jig_binary).resolve())
    else:
        jig_kwargs["jig_release"] = options.jig_release
    if options.mechanical_model is not None:
        jig_kwargs["mechanical_model"] = options.mechanical_model
    agents = [{"import_path": JIG_AGENT_IMPORT_PATH, "model_name": options.model, "kwargs": jig_kwargs}]
    if not options.no_baseline:
        agents.append({"name": BASELINE_AGENT_NAME, "model_name": options.model})
    return agents


def build_job_config(options: argparse.Namespace) -> dict:
    """Return a Harbor JobConfig; retries stay at 0 so a failed run is never paid for twice."""
    dataset: dict = {"path": str(BUGFIX_DATASET)}
    if options.task:
        dataset["task_names"] = options.task
    return {
        "job_name": options.job_name,
        "jobs_dir": str(Path(options.jobs_dir).resolve()),
        "n_attempts": options.attempts,
        "n_concurrent_trials": options.concurrency,
        "retry": {"max_retries": 0},
        "agents": build_agent_configs(options),
        "datasets": [dataset],
    }


def parse_suite_arguments(arguments: list[str] | None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--smoke", action="store_true", help="oracle and no-op agents only; no model")
    parser.add_argument("--model", help="model for jig and the baseline, e.g. anthropic/claude-sonnet-4-6")
    parser.add_argument("--jig-binary", help="Linux x86-64 jig binary; default: download a release")
    parser.add_argument("--jig-release", default="latest", help="release tag to download (default: latest)")
    parser.add_argument("--mechanical-model", help="model for jig's mechanical tier")
    parser.add_argument("--no-baseline", action="store_true", help="skip the plain claude-code arm")
    parser.add_argument("--task", action="append", help="task name or glob to include (repeatable)")
    parser.add_argument("--attempts", type=int, default=1, help="trials per task and arm (default: 1)")
    parser.add_argument("--concurrency", type=int, default=2, help="trials at once (default: 2)")
    parser.add_argument("--jobs-dir", default=str(DEFAULT_JOBS_DIRECTORY))
    parser.add_argument("--job-name", default=datetime.now().strftime("%Y-%m-%d__%H-%M-%S"))
    options = parser.parse_args(arguments)
    if not options.smoke and options.model is None:
        parser.error("--model is required unless --smoke is given")
    return options


def main(arguments: list[str] | None = None) -> int:
    options = parse_suite_arguments(arguments)
    job_config = build_job_config(options)
    harbor = Path(sys.executable).parent / "harbor"
    with tempfile.NamedTemporaryFile("w", suffix=".json", delete=False) as config_file:
        json.dump(job_config, config_file)
    harbor_exit_code = subprocess.run([str(harbor), "run", "-c", config_file.name, "-y", "-q"]).returncode
    Path(config_file.name).unlink()

    job_directory = Path(job_config["jobs_dir"]) / options.job_name
    outcomes = load_trial_outcomes(job_directory) if job_directory.exists() else []
    if not outcomes:
        print(f"eval suite: no trials found in {job_directory}", file=sys.stderr)
        return harbor_exit_code or 1
    print(f"\njig eval report - {job_directory}\n")
    print(render_eval_report(build_eval_report(outcomes)), end="")
    return harbor_exit_code


if __name__ == "__main__":
    sys.exit(main())
