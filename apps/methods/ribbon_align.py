#this belongs in apps/methods/ribbon_align.py - Version: 2
# X-Seti - November 09 2026 - IMG Factory 1.6 - Ribbon button alignment

"""
Ribbon buttons packed to the start, centre or end of their ribbon:
left / right when horizontal, top / bottom when vertical. Saved per
workshop and ribbon in ~/.config/imgfactory/ribbon_align.json and
re-applied whenever a ribbon is shown.
"""

##Methods list -
# add_align_menu
# install_ribbon_align
# _key
# _load
# _path
# _remove_filter
# set_ribbon_align
#
##class _AlignFilter: -
# eventFilter

import json

from PyQt6.QtCore import QEvent, QObject, Qt
from PyQt6.QtWidgets import QApplication, QSizePolicy, QToolBar, QWidget

from apps.methods.img_factory_settings import get_user_config_dir

_MODES = ('start', 'centre', 'end')
_filter = None


def _path(): #vers 1
    return get_user_config_dir() / 'ribbon_align.json'


def _load() -> dict: #vers 1
    try:
        return json.loads(_path().read_text())
    except (OSError, ValueError):
        return {}


def _key(tb) -> str: #vers 1
    """Workshop class / ribbon name, so same-named ribbons stay apart."""
    w = tb.parentWidget()
    while w is not None and type(w).__module__.startswith('PyQt6'):
        w = w.parentWidget()
    return f"{type(w).__name__ if w is not None else 'app'}/{tb.objectName() or tb.windowTitle()}"


def set_ribbon_align(tb, mode: str, save: bool = True) -> None: #vers 1
    """Pack tb's buttons: 'start', 'centre' or 'end' (spacers absorb the rest)."""
    for act in getattr(tb, '_align_spacers', []):
        tb.removeAction(act)
    tb._align_spacers = []

    def spacer():
        w = QWidget()
        w.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        w.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents)
        return w
    acts = tb.actions()
    if mode in ('end', 'centre') and acts:
        tb._align_spacers.append(tb.insertWidget(acts[0], spacer()))
    if mode == 'centre':
        tb._align_spacers.append(tb.addWidget(spacer()))
    tb._align_mode = mode
    if save:
        data = _load()
        if mode == 'start':
            data.pop(_key(tb), None)
        else:
            data[_key(tb)] = mode
        _path().parent.mkdir(parents=True, exist_ok=True)
        _path().write_text(json.dumps(data, indent=2))


def add_align_menu(menu, tb) -> None: #vers 1
    """'Align Buttons' submenu for tb, labelled by its orientation."""
    horiz = tb.orientation() == Qt.Orientation.Horizontal
    labels = ("Left", "Centre", "Right") if horiz else ("Top", "Centre", "Bottom")
    sub = menu.addMenu("Align Buttons")
    cur = getattr(tb, '_align_mode', 'start')
    for mode, label in zip(_MODES, labels):
        a = sub.addAction(label)
        a.setCheckable(True)
        a.setChecked(cur == mode)
        a.triggered.connect(lambda _c, m=mode: set_ribbon_align(tb, m))


class _AlignFilter(QObject): #vers 1
    """Ribbon shown: put back its saved alignment (also after a rebuild)."""

    def eventFilter(self, obj, ev): #vers 1
        if ev.type() == QEvent.Type.Show and isinstance(obj, QToolBar):
            mode = _load().get(_key(obj))
            live = [a for a in getattr(obj, '_align_spacers', []) if a in obj.actions()]
            if mode and (getattr(obj, '_align_mode', None) != mode or not live):
                set_ribbon_align(obj, mode, save=False)
        return False


def _remove_filter() -> None: #vers 1
    """App quitting: drop the filter before Qt destroys widgets."""
    global _filter
    app = QApplication.instance()
    if app is not None and _filter is not None:
        app.removeEventFilter(_filter)
    _filter = None


def install_ribbon_align() -> None: #vers 2
    """Install the filter once per application."""
    global _filter
    app = QApplication.instance()
    if app is not None and _filter is None:
        _filter = _AlignFilter(app)
        app.installEventFilter(_filter)
        app.aboutToQuit.connect(_remove_filter)          # no Python calls during teardown
