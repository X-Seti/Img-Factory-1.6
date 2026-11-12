#this belongs in apps/methods/edge_cursor.py - Version: 3
# X-Seti - November 09 2026 - IMG Factory 1.6 - Resize cursor reset

"""
Frameless windows set a resize cursor on themselves near their edges;
children inherit it, so moving from an edge straight onto a child left
the arrow stuck as a resize arrow. One app-wide filter clears it when
the mouse enters any child.
"""

##Methods list -
# install_edge_cursor_reset
# _remove_filter
#
##class _EdgeCursorReset: -
# eventFilter

from PyQt6.QtCore import QEvent, QObject, Qt
from PyQt6.QtWidgets import QApplication, QWidget

_RESIZE = {Qt.CursorShape.SizeVerCursor, Qt.CursorShape.SizeHorCursor,
           Qt.CursorShape.SizeFDiagCursor, Qt.CursorShape.SizeBDiagCursor}
_filter = None


class _EdgeCursorReset(QObject): #vers 1
    """Entering a child: resize cursors set on its parents are cleared."""

    def eventFilter(self, obj, ev): #vers 1
        if ev.type() == QEvent.Type.Enter and isinstance(obj, QWidget) and not obj.isWindow():
            p = obj.parentWidget()                         # workshop or its window set the shape
            while p is not None:
                if p.testAttribute(Qt.WidgetAttribute.WA_SetCursor) and p.cursor().shape() in _RESIZE:
                    p.unsetCursor()
                if p.isWindow():
                    break
                p = p.parentWidget()
        return False


def _remove_filter() -> None: #vers 1
    """App quitting: drop filter, free windows before sip's exit sweep."""
    global _filter
    from PyQt6 import sip
    app = QApplication.instance()
    if app is not None and _filter is not None:
        app.removeEventFilter(_filter)
    _filter = None
    for w in QApplication.topLevelWidgets():         # stale wrappers segfaulted sip at exit
        if not sip.isdeleted(w):
            sip.delete(w)


def install_edge_cursor_reset() -> None: #vers 3
    """Install the filter once per application (plus ribbon alignment)."""
    global _filter
    from apps.methods.ribbon_align import install_ribbon_align
    install_ribbon_align()
    app = QApplication.instance()
    if app is not None and _filter is None:
        _filter = _EdgeCursorReset(app)
        app.installEventFilter(_filter)
        app.aboutToQuit.connect(_remove_filter)          # no Python calls during teardown
