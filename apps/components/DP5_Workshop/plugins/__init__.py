#this belongs in apps/components/DP5_Workshop/plugins/__init__.py - Version: 1
# X-Seti - October 20 2026 - IMG Factory 1.6 - DP5 Plugin Loader.

"""
DP5 Plugin Loader - finds plugin modules exposing PLUGIN_ACTIONS.
"""

##Methods list -
# list_plugins

import importlib
import pkgutil


def list_plugins(): #vers 1
    """Return [(PLUGIN_NAME, PLUGIN_ACTIONS)] for every plugin module."""
    out = []
    for info in sorted(pkgutil.iter_modules(__path__), key=lambda m: m.name):
        mod = importlib.import_module(f"{__name__}.{info.name}")
        actions = getattr(mod, 'PLUGIN_ACTIONS', None)
        if actions:
            out.append((getattr(mod, 'PLUGIN_NAME', info.name), actions))
    return out
