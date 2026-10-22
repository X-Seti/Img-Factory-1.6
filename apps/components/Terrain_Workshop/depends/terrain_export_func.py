#this belongs in apps/components/Terrain_Workshop/depends/terrain_export_func.py - Version: 2
# X-Seti - October 22 2026 - IMG Factory 1.6 - Terrain Workshop export

"""
Export dock: game, name prefix, first model ID, draw distance, output folder
and optional IMG; writes DFF / COL / TXD per tile plus IDE and IPL.
"""

##class TerrainExportMixin: -
# _create_tw_export_dock
# _tw_export
# _tw_pick_img
# _tw_pick_out

import os

from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import (
    QComboBox, QDockWidget, QDoubleSpinBox, QFileDialog, QFormLayout, QHBoxLayout, QLineEdit,
    QMessageBox, QPushButton, QSpinBox, QWidget)


class TerrainExportMixin: #vers 1
    """Export dock for TerrainWorkshop."""

    def _create_tw_export_dock(self): #vers 3
        """Game, names, IDs, folders and the Export button."""
        from apps.methods.ui_sizes import size
        panel = QWidget()
        form = QFormLayout(panel)
        form.setContentsMargins(6, 4, 6, 4)
        form.setVerticalSpacing(2)
        form.setHorizontalSpacing(4)
        form.setLabelAlignment(Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter)
        self._tw_game = QComboBox()
        for label, key in (("San Andreas", 'sa'), ("Vice City", 'vc'), ("GTA III", 'gta3')):
            self._tw_game.addItem(label, key)
        self._tw_prefix = QLineEdit("myland")
        self._tw_prefix.setMaxLength(12)
        self._tw_prefix.setToolTip("Model / TXD / IDE / IPL name start (tiles: prefix_x_y)")
        self._tw_id = QSpinBox()
        self._tw_id.setRange(1, 65000)
        self._tw_id.setValue(18631)
        self._tw_id.setToolTip("First free model ID in the game's IDEs")
        self._tw_id.setFixedWidth(size('number_width', self))
        self._tw_dist = QDoubleSpinBox()
        self._tw_dist.setRange(50, 3000)
        self._tw_dist.setValue(299)
        self._tw_dist.setSuffix(" m")
        self._tw_dist.setFixedWidth(size('number_width', self))
        self._tw_dist.setToolTip("Draw distance of each tile")
        self._tw_out = QLineEdit()
        self._tw_out.setPlaceholderText("Output folder")
        self._tw_img = QLineEdit()
        self._tw_img.setPlaceholderText("Optional: also add to this IMG")
        tb = self._tw_tool_button
        for label, edit, picker in (("Folder", self._tw_out, self._tw_pick_out),
                                    ("IMG", self._tw_img, self._tw_pick_img)):
            row = QHBoxLayout()
            row.setSpacing(2)
            row.addWidget(edit, 1)
            row.addWidget(tb('folder_icon', f"Choose the {label.lower()}", picker))
            form.addRow(label, row)
        form.insertRow(0, "Game", self._tw_game)
        form.insertRow(1, "Name", self._tw_prefix)
        form.insertRow(2, "First ID", self._tw_id)
        form.insertRow(3, "Draw distance", self._tw_dist)
        go = QPushButton("Export terrain")
        go.setFixedHeight(size('action_button', self))
        go.setToolTip("DFF + COL per tile, one TXD, IDE and IPL (and the IMG entries)")
        go.clicked.connect(self._tw_export)
        form.addRow(go)
        dock = QDockWidget("Export", self)
        dock.setObjectName("TerrainExport")
        dock.setWidget(panel)
        self._tw_export_dock = dock
        return dock

    def _tw_pick_out(self): #vers 1
        d = QFileDialog.getExistingDirectory(self, "Export folder", self._tw_out.text())
        if d:
            self._tw_out.setText(d)

    def _tw_pick_img(self): #vers 1
        p, _ = QFileDialog.getOpenFileName(self, "Add to IMG", self._tw_img.text(), "IMG archives (*.img)")
        if p:
            self._tw_img.setText(p)

    def _tw_export(self): #vers 1
        """Write the game files; report what was made."""
        from apps.methods.terrain_export import export_terrain
        t = self._tw_terrain
        out = self._tw_out.text().strip()
        if t is None:
            QMessageBox.information(self, "Export terrain", "Create or open a terrain first.")
            return
        if not out:
            self._tw_pick_out()
            out = self._tw_out.text().strip()
            if not out:
                return
        try:
            res = export_terrain(t, self._tw_library, out, self._tw_game.currentData(),
                                 self._tw_prefix.text(), self._tw_id.value(), self._tw_texture,
                                 self._tw_img.text().strip(), self._tw_dist.value(),
                                 lambda n, total: self._set_status(f"Exporting tile {n} / {total}"))
        except Exception as e:
            QMessageBox.critical(self, "Export terrain", str(e))
            return
        a, b = res['ids']
        msg = (f"{res['tiles']} tiles exported (IDs {a}-{b}) to {out}.\n"
               f"Add the IDE and IPL to the game's .dat, and the models folder to an IMG"
               + (" (done)." if self._tw_img.text().strip() else "."))
        self._set_status(f"Terrain exported: {res['tiles']} tiles")
        QMessageBox.information(self, "Export terrain", msg)
