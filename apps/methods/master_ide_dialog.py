#this belongs in apps/methods/master_ide_dialog.py - Version: 1

##Methods list -
# MasterIDEDialog
# show_master_ide

"""master_ide_dialog.py - the real UI for master_ide.py's own step-1
merge (Sep 5 2026, per Keith: "This needs to show the IDE file or all
the IDE files in a single view... even the ability to create a
master file"). Read-only for now (Keith's own approved build order -
moving/renaming/removing with real ID cascading into IPL/2DFX comes
in a later step, once this foundation and the backup system are both
proven). Shows every merged object grouped by its own real section,
sorted by ID (Keith's own real default order), with genuine ID
collisions highlighted in a theme-aware tint - the same real red-tint
approach already used and confirmed elsewhere in Asset Checker."""

import os
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QTableWidget, QTableWidgetItem,
    QPushButton, QFileDialog, QMessageBox,
)
from PyQt6.QtCore import Qt

from apps.methods.master_ide import load_master_ide, write_master_ide


class MasterIDEDialog(QDialog): #vers 1
    def __init__(self, parent, result, source_paths): #vers 1
        super().__init__(parent)
        self.result = result
        self.source_paths = source_paths
        self.setWindowTitle("Master IDE")
        self.resize(820, 560)
        self._build_ui()
        self._populate()

    def _build_ui(self): #vers 1
        lay = QVBoxLayout(self)

        top = QHBoxLayout()
        names = ", ".join(os.path.basename(p) for p in self.result.source_files)
        top.addWidget(QLabel(f"Merged: {names} ({self.result.total_objects} object(s))"))
        top.addStretch()
        lay.addLayout(top)

        if self.result.collisions:
            warn = QLabel(
                f"WARNING: {len(self.result.collisions)} real ID collision(s) found - "
                f"see highlighted rows below. Resolve these before using this as a real "
                f"combined file.")
            warn.setStyleSheet("font-weight: bold;")
            lay.addWidget(warn)

        if self.result.errors:
            for err in self.result.errors:
                lay.addWidget(QLabel(f"Error: {err}"))

        self.table = QTableWidget()
        self.table.setColumnCount(5)
        self.table.setHorizontalHeaderLabels(["Section", "ID", "Model", "TXD", "Source IDE"])
        self.table.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
        self.table.horizontalHeader().setStretchLastSection(True)
        lay.addWidget(self.table, 1)

        btn_row = QHBoxLayout()
        save_btn = QPushButton("Save as Master IDE...")
        save_btn.clicked.connect(self._on_save)
        btn_row.addWidget(save_btn)
        btn_row.addStretch()
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.accept)
        btn_row.addWidget(close_btn)
        lay.addLayout(btn_row)

    def _populate(self): #vers 1
        base = self.palette().color(self.palette().currentColorGroup(),
                                     self.palette().ColorRole.Base)
        from PyQt6.QtGui import QColor
        collision_tint = QColor(
            min(255, base.red() + 40), max(0, base.green() - 25), max(0, base.blue() - 25))
        collision_ids = {c.model_id for c in self.result.collisions}

        rows = []
        for section, objs in self.result.objects_by_section.items():
            for obj in objs:
                rows.append((section, obj.model_id, obj.model_name,
                             obj.txd_name, os.path.basename(obj.source_ide)))

        self.table.setRowCount(len(rows))
        for row, (section, model_id, model_name, txd_name, source_ide) in enumerate(rows):
            has_collision = model_id in collision_ids
            for col, val in enumerate([section, str(model_id), model_name, txd_name, source_ide]):
                item = QTableWidgetItem(val)
                item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
                if has_collision:
                    item.setBackground(collision_tint)
                self.table.setItem(row, col, item)

    def _on_save(self): #vers 1
        if self.result.collisions:
            reply = QMessageBox.warning(
                self, "Real ID Collisions Found",
                f"{len(self.result.collisions)} model ID(s) are used by more than one "
                f"different model across the merged files. Saving now will keep both "
                f"colliding entries as-is (nothing is renumbered by this step). "
                f"Continue anyway?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
            if reply != QMessageBox.StandardButton.Yes:
                return

        path, _ = QFileDialog.getSaveFileName(self, "Save Master IDE", "", "IDE Files (*.ide)")
        if not path:
            return
        if write_master_ide(self.result, path):
            QMessageBox.information(self, "Saved", f"Master IDE saved to:\n{path}")
        else:
            QMessageBox.warning(self, "Save Failed", "Could not write the master IDE file.")


def show_master_ide(main_window, ide_paths, game: str = None): #vers 1
    """Entry point - loads and merges the given real .ide files and
    shows the result. ide_paths can be a single path or a list."""
    if isinstance(ide_paths, str):
        ide_paths = [ide_paths]
    result = load_master_ide(ide_paths, game=game)
    dlg = MasterIDEDialog(main_window, result, ide_paths)
    dlg.exec()
