#this belongs in apps/components/DP5_Workshop/depends/history_widget.py - Version: 1
# X-Seti - October 27 2026 - IMG Factory 1.6 - DP5 History dock.

"""
History dock: the undo stack as a thumbnail list (oldest first), the
current state, then redo states (greyed). Clicking a row undoes or redoes
until that state is current. owner._history_changed() refreshes it.
"""

##Methods list -
# _on_clicked
# _thumb
# create_history_dock
# refresh_history

from PyQt6.QtCore import QSize, Qt
from PyQt6.QtGui import QColor, QIcon, QImage, QPixmap
from PyQt6.QtWidgets import QFrame, QLabel, QListWidget, QListWidgetItem, QVBoxLayout

THUMB = 40
_ROLE = Qt.ItemDataRole.UserRole


def _thumb(cache: dict, rgba, w: int, h: int) -> QIcon: #vers 1
    """Cached icon for one stack entry (keyed by the bytes object)."""
    key = id(rgba)
    hit = cache.get(key)
    if hit and hit[0] is rgba:
        return hit[1]
    img = QImage(bytes(rgba), w, h, w * 4, QImage.Format.Format_RGBA8888)
    pm = QPixmap.fromImage(img.scaled(THUMB, THUMB, Qt.AspectRatioMode.KeepAspectRatio,
                                      Qt.TransformationMode.FastTransformation))
    icon = QIcon(pm)
    cache[key] = (rgba, icon)
    return icon


def create_history_dock(owner): #vers 1
    from apps.components.DP5_Workshop.depends.dock_helper import make_dock
    panel = QFrame()
    panel.setObjectName("dp5_history_panel")
    lay = QVBoxLayout(panel)
    lay.setContentsMargins(4, 4, 4, 4)
    lay.setSpacing(3)
    lw = QListWidget()
    lw.setIconSize(QSize(THUMB, THUMB))
    lw.itemClicked.connect(lambda it: _on_clicked(owner, it))
    lay.addWidget(lw, 1)
    hint = QLabel("Click a step to go back / forward to it")
    hint.setWordWrap(True)
    lay.addWidget(hint)
    owner._history_lw = lw
    owner._history_cache = {}
    dock = make_dock(owner, "History", panel)
    dock.visibilityChanged.connect(lambda on: on and refresh_history(owner))
    owner._history_dock = dock
    return dock


def refresh_history(owner): #vers 1
    """Rebuild the list from owner's undo / redo stacks."""
    lw = getattr(owner, '_history_lw', None)
    c = owner.dp5_canvas
    if lw is None or not c or not owner._history_dock.isVisible():
        return
    cache = owner._history_cache
    lw.clear()
    undo = list(owner._undo_stack)
    redo = list(owner._redo_stack)[::-1]                 # next redo first
    live = set()
    for i, (rgba, w, h, _pa) in enumerate(undo):
        it = QListWidgetItem(_thumb(cache, rgba, w, h), f"Step {i + 1}  {w}x{h}")
        it.setData(_ROLE, ('undo', len(undo) - i))
        lw.addItem(it)
        live.add(id(rgba))
    cur = QListWidgetItem(QIcon(QPixmap.fromImage(QImage(bytes(c.rgba), c.tex_w, c.tex_h, c.tex_w * 4,
                                                         QImage.Format.Format_RGBA8888).scaled(
        THUMB, THUMB, Qt.AspectRatioMode.KeepAspectRatio))), f"Current  {c.tex_w}x{c.tex_h}")
    f = cur.font()
    f.setBold(True)
    cur.setFont(f)
    cur.setData(_ROLE, ('cur', 0))
    lw.addItem(cur)
    for j, (rgba, w, h, _pa) in enumerate(redo):
        it = QListWidgetItem(_thumb(cache, rgba, w, h), f"Redo {j + 1}  {w}x{h}")
        it.setForeground(QColor(128, 128, 128))
        it.setData(_ROLE, ('redo', j + 1))
        lw.addItem(it)
        live.add(id(rgba))
    for k in [k for k in cache if k not in live]:        # drop thumbnails of discarded states
        del cache[k]
    lw.setCurrentItem(cur)
    lw.scrollToItem(cur)


def _on_clicked(owner, item): #vers 1
    kind, n = item.data(_ROLE)
    for _ in range(n):
        if kind == 'undo':
            owner._undo_canvas()
        elif kind == 'redo':
            owner._redo_canvas()
