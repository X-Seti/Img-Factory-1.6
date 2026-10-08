#this belongs in apps/gui/radar_align_dialog.py - Version: 1
# X-Seti - October 08 2026 - IMG Factory 1.6 - Radar/water alignment window

"""
Nudge radar and water layers per game; values saved by the owner.
"""

##Methods list -
# __init__
# _arrow_box
# _nudge
# _refresh
# _reset
# _step

from PyQt6.QtWidgets import (QCheckBox, QComboBox, QDialog, QGridLayout,
                             QGroupBox, QHBoxLayout, QLabel, QPushButton,
                             QVBoxLayout)


class RadarAlignDialog(QDialog):
    """Non-modal: arrows move radar or water by the chosen step."""

    STEPS = (1, 10, 50, 100)

    def __init__(self, owner, parent=None): #vers 1
        super().__init__(parent)
        self._owner = owner
        self.setWindowTitle("Radar / Water Alignment")
        self.setModal(False)
        lay = QVBoxLayout(self)
        self._root_lbl = QLabel("")
        self._root_lbl.setWordWrap(True)
        lay.addWidget(self._root_lbl)
        row = QHBoxLayout()
        row.addWidget(QLabel("Step:"))
        self._step_combo = QComboBox()
        for s in self.STEPS:
            self._step_combo.addItem(f"{s} units", s)
        self._step_combo.setCurrentIndex(1)
        row.addWidget(self._step_combo)
        row.addStretch()
        lay.addLayout(row)
        self._labels = {}
        for layer, title in (('radar', "Radar tiles"), ('water', "Water")):
            lay.addWidget(self._arrow_box(layer, title))
        self._fix_chk = QCheckBox("Water fix (VC -400 X shift)")
        self._fix_chk.setToolTip("Off for LCS ports, LC, SA. Saved per game.")
        self._fix_chk.toggled.connect(lambda on: self._owner._set_water_fix(on))
        lay.addWidget(self._fix_chk)
        close = QPushButton("Close")
        close.clicked.connect(self.close)
        lay.addWidget(close)
        self._refresh()

    def _arrow_box(self, layer, title): #vers 1
        """Up/down/left/right/reset grid for one layer."""
        box = QGroupBox(title)
        g = QGridLayout(box)
        for text, r, c, dx, dy in (("Up", 0, 1, 0, 1), ("Left", 1, 0, -1, 0),
                                   ("Right", 1, 2, 1, 0), ("Down", 2, 1, 0, -1)):
            b = QPushButton(text)
            b.setAutoRepeat(True)
            b.clicked.connect(lambda _c=False, l=layer, x=dx, y=dy: self._nudge(l, x, y))
            g.addWidget(b, r, c)
        reset = QPushButton("Reset")
        reset.clicked.connect(lambda _c=False, l=layer: self._reset(l))
        g.addWidget(reset, 1, 1)
        lbl = QLabel("")
        g.addWidget(lbl, 3, 0, 1, 3)
        self._labels[layer] = lbl
        return box

    def _step(self): #vers 1
        """Current nudge step in world units."""
        return float(self._step_combo.currentData() or 10)

    def _nudge(self, layer, dx, dy): #vers 1
        """Move one layer by step in the given direction."""
        x, y = self._owner._radar_align_offset(layer)
        s = self._step()
        self._owner._set_radar_align_offset(layer, x + dx * s, y + dy * s)
        self._refresh()

    def _reset(self, layer): #vers 1
        """Layer back to zero offset."""
        self._owner._set_radar_align_offset(layer, 0.0, 0.0)
        self._refresh()

    def _refresh(self): #vers 1
        """Labels and fix checkbox from the owner's saved values."""
        path = self._owner._radar_settings_path()
        self._root_lbl.setText(f"Saved to: {path}" if path else "Load a game first - nothing to save to.")
        for layer, lbl in self._labels.items():
            x, y = self._owner._radar_align_offset(layer)
            lbl.setText(f"Offset X {x:+.0f}  Y {y:+.0f}")
        self._fix_chk.blockSignals(True)
        self._fix_chk.setChecked(self._owner._water_fix_active())
        self._fix_chk.blockSignals(False)
