#this belongs in apps/methods/short_path.py - Version: 1
# X-Seti - November 16 2026 - IMG Factory 1.6 - Short path line edit

"""
ShortPathEdit - a QLineEdit that shows a long path shortened ('.../PC/GTASA-PC')
until it is clicked; text() always returns the full path.
"""

##Methods list -
# shorten_path

##class ShortPathEdit: -
# __init__
# _render
# focusInEvent
# focusOutEvent
# setText
# text

from PyQt6.QtWidgets import QLineEdit


def shorten_path(path: str, keep: int = 2) -> str: #vers 1
    """'/a/b/c/d/e' -> '.../d/e' (last keep folders)."""
    p = path.replace('\\', '/').rstrip('/')
    parts = [x for x in p.split('/') if x]
    if len(parts) <= keep + 1:
        return path
    return '.../' + '/'.join(parts[-keep:])


class ShortPathEdit(QLineEdit): #vers 1
    """Shows a short path until focused; text() is always the full path."""

    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self._full = ''
        self._short_on = True

    def text(self) -> str: #vers 1
        """Full path, whether shown short or in full."""
        return self._full if self._short_on else super().text()

    def setText(self, t: str): #vers 1
        """Set the full path; display follows the current state."""
        self._full = t or ''
        self._render()

    def _render(self): #vers 1
        """Show the short or full form without signalling a text change."""
        shown = shorten_path(self._full) if self._short_on else self._full
        self.blockSignals(True)
        super().setText(shown)
        self.blockSignals(False)
        self.setToolTip(self._full)

    def focusInEvent(self, ev): #vers 1
        """Click / focus: show the full path for editing."""
        if self._short_on:
            self._short_on = False
            self._render()
        super().focusInEvent(ev)

    def focusOutEvent(self, ev): #vers 1
        """Leaving the field: keep what was typed and shorten again."""
        if not self._short_on:
            self._full = super().text()
            self._short_on = True
            self._render()
        super().focusOutEvent(ev)
