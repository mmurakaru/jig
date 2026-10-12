import copy

from settings import DEFAULT_SETTINGS, effective_settings, merge_settings


def test_top_level_value_is_replaced():
    assert effective_settings({"theme": "dark"})["theme"] == "dark"


def test_lists_are_replaced_not_joined():
    assert effective_settings({"plugins": ["lint"]})["plugins"] == ["lint"]


def test_no_user_settings_gives_defaults():
    assert effective_settings({}) == DEFAULT_SETTINGS


def test_inputs_are_not_changed():
    base = {"editor": {"tab_width": 4}, "theme": "light"}
    override = {"editor": {"wrap": True}}
    base_before = copy.deepcopy(base)
    override_before = copy.deepcopy(override)
    merge_settings(base, override)
    assert base == base_before
    assert override == override_before
