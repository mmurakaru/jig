import copy

from settings import DEFAULT_SETTINGS, effective_settings, merge_settings


def test_nested_setting_keeps_its_siblings():
    editor = effective_settings({"editor": {"wrap": True}})["editor"]
    assert editor == {"tab_width": 4, "wrap": True}


def test_merging_twice_leaves_defaults_unchanged():
    defaults_before = copy.deepcopy(DEFAULT_SETTINGS)
    effective_settings({"editor": {"tab_width": 2}})
    effective_settings({"editor": {"wrap": True}})
    assert DEFAULT_SETTINGS == defaults_before


def test_deeply_nested_merge():
    base = {"a": {"b": {"c": 1, "d": 2}}}
    merged = merge_settings(base, {"a": {"b": {"d": 3}}})
    assert merged == {"a": {"b": {"c": 1, "d": 3}}}
    assert base == {"a": {"b": {"c": 1, "d": 2}}}


def test_result_does_not_share_nested_mappings_with_base():
    base = {"editor": {"tab_width": 4}}
    merged = merge_settings(base, {"theme": "dark"})
    merged["editor"]["tab_width"] = 8
    assert base["editor"]["tab_width"] == 4
