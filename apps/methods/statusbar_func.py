#this belongs in apps/methods/statusbar_func.py - Version: 1
# X-Seti - October 20 2026 - IMG Factory 1.6 - Workshop Status Bar Option

"""
Workshop Status Bar Option - per workshop show/hide, saved in workshop_ui.json.
register_statusbar() after building the bar; add_statusbar_checkbox() in Settings.
"""

##Methods list -
# _load
# _save
# add_statusbar_checkbox
# apply_statusbar
# register_statusbar
# set_statusbar_shown
# statusbar_shown

import json

from PyQt6.QtWidgets import QCheckBox

from apps.methods.img_factory_settings import get_user_config_dir


def _load(): #vers 1
    """All workshops' UI options."""
    path = get_user_config_dir() / 'workshop_ui.json'
    if path.is_file():
        with open(path, 'r', encoding='utf-8') as f:
            return json.load(f)
    return {}


def _save(data): #vers 1
    """Write all workshops' UI options."""
    path = get_user_config_dir() / 'workshop_ui.json'
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, 'w', encoding='utf-8') as f:
        json.dump(data, f, indent=2)


def statusbar_shown(ws): #vers 1
    """Saved status bar state for this workshop (default shown)."""
    return bool(_load().get(type(ws).__name__, {}).get('show_statusbar', True))


def set_statusbar_shown(ws, on): #vers 1
    """Save and apply the status bar state."""
    data = _load()
    data.setdefault(type(ws).__name__, {})['show_statusbar'] = bool(on)
    _save(data)
    apply_statusbar(ws)


def apply_statusbar(ws): #vers 1
    """Show or hide the registered bar; docked_hide bars stay hidden docked."""
    bar = getattr(ws, '_status_widget', None)
    if bar is None:
        return
    docked = not getattr(ws, 'standalone_mode', True)
    bar.setVisible(statusbar_shown(ws) and not (ws._status_docked_hide and docked))


def register_statusbar(ws, widget, docked_hide=False): #vers 1
    """Remember the workshop's status bar widget and apply the saved state."""
    ws._status_widget = widget
    ws._status_docked_hide = docked_hide
    apply_statusbar(ws)


def add_statusbar_checkbox(ws, layout): #vers 1
    """'Show status bar' checkbox for a Settings dialog; applies at once."""
    chk = QCheckBox("Show status bar")
    chk.setChecked(statusbar_shown(ws))
    chk.toggled.connect(lambda on: set_statusbar_shown(ws, on))
    (layout.addRow if hasattr(layout, 'addRow') else layout.addWidget)(chk)
    return chk
