import pytest

from jig_evals.eval_suite import build_job_config, parse_suite_arguments


def test_model_run_pairs_jig_with_the_baseline_on_the_same_model():
    config = build_job_config(parse_suite_arguments(["--model", "anthropic/claude-sonnet-4-6"]))
    agents = config["agents"]
    assert [agent.get("name") or agent["import_path"] for agent in agents] == [
        "jig_evals.jig_agent:JigAgent",
        "claude-code",
    ]
    assert {agent["model_name"] for agent in agents} == {"anthropic/claude-sonnet-4-6"}
    assert agents[0]["kwargs"] == {"jig_release": "latest"}
    assert config["retry"] == {"max_retries": 0}
    assert config["datasets"][0]["path"].endswith("datasets/bugfix")


def test_local_binary_and_mechanical_model_reach_the_jig_arm_only(tmp_path):
    binary = tmp_path / "jig-linux-x86_64"
    options = parse_suite_arguments(
        ["--model", "m", "--jig-binary", str(binary), "--mechanical-model", "cheap", "--no-baseline"]
    )
    agents = build_job_config(options)["agents"]
    assert len(agents) == 1
    assert agents[0]["kwargs"] == {"jig_binary_path": str(binary.resolve()), "mechanical_model": "cheap"}


def test_smoke_run_needs_no_model_and_uses_oracle_and_no_op():
    config = build_job_config(parse_suite_arguments(["--smoke", "--task", "duration-parse"]))
    assert config["agents"] == [{"name": "oracle"}, {"name": "nop"}]
    assert config["datasets"][0]["task_names"] == ["duration-parse"]


def test_model_is_required_without_smoke():
    with pytest.raises(SystemExit):
        parse_suite_arguments([])
