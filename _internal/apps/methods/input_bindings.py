#this belongs in apps/methods/input_bindings.py - Version: 1
# X-Seti - October 28 2026 - IMG Factory 1.6 - Key and mouse bindings

"""
Per-tool key shortcuts, 3D view mouse buttons and camera keys.
Stored in <repo or exe folder>/buttons/<tool>.json.
"""

##Methods list -
# bindings_dir
# hotkey_label
# mouse_remap
#
##class InputBindingsMixin: -
# _binding_actions
# _binding_name
# _bindings_apply
# _bindings_load
# _bindings_path
# _bindings_save
# _bindings_viewports
# open_bindings_editor

import json
import sys
from pathlib import Path

from PyQt6.QtCore import Qt
from PyQt6.QtGui import QKeySequence, QShortcut
from PyQt6.QtWidgets import QAbstractButton, QToolBar

BUTTON_NAMES = {'left': Qt.MouseButton.LeftButton, 'right': Qt.MouseButton.RightButton,
                'middle': Qt.MouseButton.MiddleButton}
MOUSE_ROLES = {'select': "Select / drag", 'orbit': "Rotate view (click: menu)", 'pan': "Pan view (click: cycle)"}
MOUSE_DEFAULTS = {'select': 'left', 'orbit': 'right', 'pan': 'middle'}
ROLE_BUTTON = {'select': Qt.MouseButton.LeftButton, 'orbit': Qt.MouseButton.RightButton,
               'pan': Qt.MouseButton.MiddleButton}     # button the viewport code expects

HOTKEY_LABELS = {'open': "Open", 'save': "Save", 'force_save': "Force Save", 'save_as': "Save As",
                 'close': "Close", 'undo': "Undo", 'redo': "Redo", 'redo2': "Redo (alt)",
                 'copy': "Copy", 'paste': "Paste", 'delete': "Delete", 'duplicate': "Duplicate",
                 'rename': "Rename", 'import': "Import", 'export': "Export", 'export_all': "Export All",
                 'refresh': "Refresh", 'properties': "Properties", 'settings': "Settings",
                 'select_all': "Select All", 'invert': "Invert Selection", 'find': "Find",
                 'help': "Help", 'fit': "Fit View"}


def bindings_dir() -> Path: #vers 1
    """buttons/ beside the exe, else in the repo root."""
    if getattr(sys, 'frozen', False):
        return Path(sys.executable).parent / 'buttons'
    return Path(__file__).resolve().parents[2] / 'buttons'


def hotkey_label(attr: str) -> str: #vers 1
    """hotkey_save_as -> 'Save As'."""
    key = attr[len('hotkey_'):] if attr.startswith('hotkey_') else attr
    return HOTKEY_LABELS.get(key, key.replace('_', ' ').title())


def mouse_remap(widget): #vers 1
    """{physical button: logical button} from the owning tool, or None."""
    w = widget
    while w is not None:
        m = getattr(w, '_bindings_mouse_remap', None)
        if m is not None:
            return m or None
        w = w.parentWidget()
    return None


class InputBindingsMixin:
    """Load / apply / edit buttons/<tool>.json. Applied once the tool is built."""

    def _binding_name(self) -> str: #vers 1
        """File stem: the tool's module name (model_workshop, hex_workshop ...)."""
        return getattr(self, '_bindings_name', None) or type(self).__module__.rsplit('.', 1)[-1]

    def _bindings_path(self) -> Path: #vers 1
        return bindings_dir() / f"{self._binding_name()}.json"

    def _bindings_load(self) -> dict: #vers 1
        p = self._bindings_path()
        if not p.exists():
            return {}
        try:
            return json.loads(p.read_text())
        except Exception as e:
            print(f"[bindings] {p.name}: {e}")
            return {}

    def _binding_actions(self) -> list: #vers 1
        """[(id, label, target)]: QShortcut attributes, then toolbar / ribbon buttons."""
        out = [(a, hotkey_label(a), o) for a, o in sorted(vars(self).items()) if isinstance(o, QShortcut)]
        seen = set()
        for tb in self.findChildren(QToolBar):              # every ribbon / toolbar in the tool
            for b in tb.findChildren(QAbstractButton):
                tip = b.toolTip().strip()
                if not tip or b.objectName() == 'qt_toolbar_ext_button':
                    continue
                bid = f"ribbon:{tb.objectName() or tb.windowTitle()}/{tip}"
                if bid in seen:
                    continue
                seen.add(bid)
                out.append((bid, f"{tb.windowTitle() or tb.objectName()}: {tip}", b))
        return out

    def _bindings_viewports(self) -> list: #vers 1
        """3D views (DFFViewport) inside this tool; none if never imported."""
        mod = sys.modules.get('apps.methods.dff_viewport')
        return self.findChildren(mod.DFFViewport) if mod else []

    def _bindings_apply(self, data=None): #vers 1
        """Apply keys, mouse buttons and camera keys from data (default: the json)."""
        data = self._bindings_load() if data is None else data
        actions = self._binding_actions()
        if not hasattr(self, '_bindings_defaults'):          # first run: keys as built
            self._bindings_defaults = {i: (t.key().toString() if isinstance(t, QShortcut) else '')
                                       for i, _l, t in actions}
        ribbon_sc = getattr(self, '_bindings_ribbon_sc', None)
        if ribbon_sc is None:
            ribbon_sc = self._bindings_ribbon_sc = {}
        keys = data.get('keys', {})
        for bid, _label, target in actions:
            if bid not in keys:
                continue
            seq = QKeySequence(keys[bid])
            if isinstance(target, QShortcut):
                target.setKey(seq)
                continue
            sc = ribbon_sc.get(bid)
            if sc is None:
                sc = ribbon_sc[bid] = QShortcut(self)
                sc.setContext(Qt.ShortcutContext.WidgetWithChildrenShortcut)
                sc.activated.connect(lambda b=target: b.isEnabled() and b.click())
            sc.setKey(seq)
        roles = dict(MOUSE_DEFAULTS, **data.get('mouse', {}))
        remap = {BUTTON_NAMES[roles[r]]: ROLE_BUTTON[r] for r in MOUSE_ROLES}
        self._bindings_mouse_remap = {p: l for p, l in remap.items() if p != l}
        if data.get('camera'):
            for vp in self._bindings_viewports():
                vp.set_key_bindings(data['camera'])

    def _bindings_save(self, data: dict): #vers 1
        """Write the json and apply it."""
        p = self._bindings_path()
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(json.dumps(data, indent=2))
        self._bindings_apply(data)
        if hasattr(self, '_set_status'):
            self._set_status(f"Bindings saved: buttons/{p.name}")

    def open_bindings_editor(self): #vers 1
        """Key & Mouse Bindings dialog."""
        from apps.methods.input_bindings_dialog import InputBindingsDialog
        InputBindingsDialog(self, parent=self).exec()
