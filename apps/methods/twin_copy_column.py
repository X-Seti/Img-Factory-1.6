#this belongs in apps/methods/twin_copy_column.py - Version: 1
# X-Seti - November 16 2026 - IMG Factory 1.6 - Twin panel copy column

"""
CopyColumn - buttons between the twin Dir List panels: copy, copy back, Mirror, Clone, Open, Eject.
"""

##Methods list -

##class CopyColumn: -
# __init__
# _apply
# dry_run
# eventFilter
# set_horizontal
# showEvent

from PyQt6.QtCore import pyqtSignal, QEvent
from PyQt6.QtWidgets import QBoxLayout, QPushButton, QWidget

_TIGHT = 720   # splitter width below which labels shrink to arrows only


class CopyColumn(QWidget): #vers 1
    """Positional buttons: forward goes first panel to second panel."""
    copy_fwd = pyqtSignal()
    copy_back = pyqtSignal()
    mirror_fwd = pyqtSignal()
    clone_fwd = pyqtSignal()
    open_item = pyqtSignal()
    eject_item = pyqtSignal()

    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self._horiz = True
        self._tight = False
        self._lay = QBoxLayout(QBoxLayout.Direction.TopToBottom, self)
        self._lay.setContentsMargins(2, 2, 2, 2)
        self._lay.setSpacing(4)
        specs = [("fwd", "Copy selected to the next panel", self.copy_fwd),
                 ("back", "Copy selected to the previous panel", self.copy_back),
                 ("mirror", "Mirror: update missing or older files in the next panel; extras go to Trash or are deleted", self.mirror_fwd),
                 ("clone", "Clone: back up this whole folder; the next panel becomes an exact copy", self.clone_fwd),
                 ("dry", "Dry run: Mirror and Clone only list what they would do", None),
                 ("open", "Open the selected archive or mount the selected ISO / IMG", self.open_item),
                 ("eject", "Eject: unmount opened images and remove extracted archives", self.eject_item)]
        self._btns = {}
        self._lay.addStretch()
        for key, tip, sig in specs:
            b = QPushButton()
            b.setToolTip(tip)
            if sig is not None:
                b.clicked.connect(sig)
            else:
                b.setCheckable(True)
            self._btns[key] = b
            self._lay.addWidget(b)
        self._lay.addStretch()
        self._apply()

    def dry_run(self) -> bool: #vers 1
        """True while the Dry run button is down."""
        return self._btns["dry"].isChecked()

    def set_horizontal(self, horiz: bool): #vers 1
        """True when panels sit side by side (buttons stack in a column)."""
        self._horiz = horiz
        self._lay.setDirection(QBoxLayout.Direction.TopToBottom if horiz
                               else QBoxLayout.Direction.LeftToRight)
        self._apply()

    def _apply(self): #vers 1
        fwd, back = ("-->", "<--") if self._horiz else ("v", "^")
        full = {"fwd": fwd, "back": back, "mirror": "Mirror " + fwd, "clone": "Clone " + fwd,
                "dry": "Dry run", "open": "Open", "eject": "Eject"}
        tight = {"fwd": fwd, "back": back, "mirror": "M" + fwd, "clone": "C" + fwd,
                "dry": "Dry", "open": "Open", "eject": "Eject"}
        txt = tight if self._tight else full
        w = 54 if self._tight else 92
        for k, b in self._btns.items():
            b.setText(txt[k])
            b.setFixedWidth(w) if self._horiz else b.setMinimumWidth(w)
            b.setMaximumWidth(w if self._horiz else 16777215)
            b.setFixedHeight(26)
        if self._horiz:
            self.setFixedWidth(w + 4)
            self.setMaximumHeight(16777215)
        else:
            self.setMaximumWidth(16777215)
            self.setFixedHeight(34)

    def showEvent(self, ev): #vers 1
        """Watch the parent splitter width for the tight-space rule."""
        p = self.parentWidget()
        if p is not None:
            p.installEventFilter(self)
            self._check(p.width())
        super().showEvent(ev)

    def eventFilter(self, obj, ev): #vers 1
        if ev.type() == QEvent.Type.Resize:
            self._check(ev.size().width())
        return False

    def _check(self, width: int): #vers 1
        tight = width < _TIGHT
        if tight != self._tight:
            self._tight = tight
            self._apply()
