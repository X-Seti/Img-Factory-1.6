#this belongs in apps/components/DP5_Workshop/plugins/__init__.py - Version: 3
# X-Seti - October 26 2026 - IMG Factory 1.6 - DP5 Plugin Loader.

"""
DP5 Plugin Loader - finds plugin modules exposing PLUGIN_NAME and
PLUGIN_ACTIONS [(label, fn) or (None, None) for a divider]. Built-in
plugins live here; user plugins are .py files in the dp5_plugins folder
(settings folder, beside the exe in a built program). Each action gets a
PluginHost - the only DP5 calls a plugin should use. A plugin that fails
to import is reported, the others still load.
"""

##Methods list -
# list_plugins
# user_plugin_dir
#
##class PluginHost: -
# __init__
# active_label
# active_tab
# image
# new_tab
# set_image
# status
# tab_image
# tabs

import importlib
import importlib.util
import pkgutil

from apps.methods.img_factory_settings import get_user_config_dir


class PluginHost: #vers 1
    """What a plugin may do with DP5: read / write the canvas, open tabs, show status."""

    def __init__(self, ws): #vers 1
        self.ws = ws
        self.widget = ws                                 # parent for plugin dialogs

    def image(self): #vers 1
        """(rgba bytearray copy, w, h) of the active canvas, or None."""
        c = self.ws.dp5_canvas
        return (bytearray(c.rgba), c.tex_w, c.tex_h) if c else None

    def set_image(self, rgba, w=None, h=None, undo=True): #vers 1
        """Replace the active canvas (new size allowed); undoable."""
        ws, c = self.ws, self.ws.dp5_canvas
        if not c:
            return
        w, h = w or c.tex_w, h or c.tex_h
        if len(rgba) != w * h * 4:
            raise ValueError(f"Plugin image is {len(rgba)} bytes, {w} x {h} needs {w * h * 4}")
        if undo:
            ws._push_undo()
        resized = (w, h) != (c.tex_w, c.tex_h)
        ws._canvas_width, ws._canvas_height = w, h
        c.tex_w, c.tex_h = w, h
        c.rgba = bytearray(rgba)
        c.update()
        if resized:
            ws._fit_canvas_to_viewport()

    def new_tab(self, rgba, w, h, label=None): #vers 1
        """Open rgba as a new canvas tab (current tab kept)."""
        self.ws._canvas_to_new_tab(rgba, w, h, label)

    def tabs(self) -> list: #vers 1
        """Labels of the canvas tabs."""
        self.ws._save_current_canvas_tab()
        return [t['label'] for t in self.ws._canvas_tabs]

    def tab_image(self, idx: int): #vers 1
        """(rgba, w, h) of tab idx (live canvas when it is the active tab)."""
        ws = self.ws
        if idx == ws._active_canvas_tab_idx:
            return self.image()
        st = ws._canvas_tabs[idx]['state']
        return bytearray(st['rgba']), st['tex_w'], st['tex_h']

    def active_tab(self) -> int: #vers 1
        """Index of the active canvas tab."""
        return max(0, self.ws._active_canvas_tab_idx)

    def active_label(self) -> str: #vers 1
        ws = self.ws
        tabs = ws._canvas_tabs
        return tabs[ws._active_canvas_tab_idx]['label'] if 0 <= ws._active_canvas_tab_idx < len(tabs) else '1'

    def status(self, msg: str): #vers 1
        self.ws._set_status(msg)


def user_plugin_dir(): #vers 1
    """Folder for user plugins (made on first use)."""
    d = get_user_config_dir() / 'dp5_plugins'
    d.mkdir(parents=True, exist_ok=True)
    return d


def list_plugins(): #vers 2
    """([(PLUGIN_NAME, PLUGIN_ACTIONS)], [(module, error)]) - built-in, then user plugins."""
    mods, errors = [], []
    for info in sorted(pkgutil.iter_modules(__path__), key=lambda m: m.name):
        try:
            mods.append((info.name, importlib.import_module(f"{__name__}.{info.name}")))
        except Exception as ex:                          # one broken plugin must not stop DP5
            errors.append((info.name, f"{type(ex).__name__}: {ex}"))
    for path in sorted(user_plugin_dir().glob('*.py')):
        try:
            spec = importlib.util.spec_from_file_location(f"dp5_user_plugin_{path.stem}", path)
            mod = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(mod)
            mods.append((path.stem, mod))
        except Exception as ex:
            errors.append((path.name, f"{type(ex).__name__}: {ex}"))
    out = []
    for name, mod in mods:
        actions = getattr(mod, 'PLUGIN_ACTIONS', None)
        if actions:
            out.append((getattr(mod, 'PLUGIN_NAME', name), actions))
    return out, errors
