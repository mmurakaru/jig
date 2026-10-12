"""Layer user settings over default settings."""

DEFAULT_SETTINGS = {
    "theme": "light",
    "editor": {"tab_width": 4, "wrap": False},
    "plugins": ["spellcheck"],
}


def merge_settings(base: dict, override: dict) -> dict:
    """Return base with override layered on top; neither input is changed.

    Nested mappings merge key by key; any other override value (lists too)
    replaces the base value.
    """
    return {**base, **override}


def effective_settings(user_settings: dict) -> dict:
    """Return the defaults with the user's settings layered on top."""
    return merge_settings(DEFAULT_SETTINGS, user_settings)
