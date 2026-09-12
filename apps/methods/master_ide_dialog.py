#this belongs in apps/methods/master_ide_dialog.py - Version: 2

##Methods list -
# MasterIDEDialog
# show_master_ide
# show_master_ide_from_dat

"""master_ide_dialog.py - the real UI for master_ide.py's own step-1 merge (Sep 5 2026)"""

import os
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QTableWidget, QTableWidgetItem,
    QPushButton, QFileDialog, QMessageBox,
)
from PyQt6.QtCore import Qt

from apps.methods.master_ide import (
    load_master_ide, write_master_ide, collect_ide_paths_from_dat)


class MasterIDEDialog(QDialog): #vers 2
    def __init__(self, parent, result, source_paths, game=None): #vers 2
        super().__init__(parent)
        self.result = result
        self.source_paths = source_paths
        self.game = game
        self.setWindowTitle("Master IDE")
        self.resize(820, 560)
        self._build_ui()
        self._refresh_top()
        self._populate()

    def _build_ui(self): #vers 2
        self._lay = QVBoxLayout(self)
        lay = self._lay

        self._top = QHBoxLayout()
        lay.addLayout(self._top)
        self._warn_lbl = None
        self._error_lbls = []

        self.table = QTableWidget()
        self.table.setColumnCount(5)
        self.table.setHorizontalHeaderLabels(["Section", "ID", "Model", "TXD", "Source IDE"])
        self.table.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
        self.table.horizontalHeader().setStretchLastSection(True)
        lay.addWidget(self.table, 1)

        btn_row = QHBoxLayout()
        load_dat_btn = QPushButton("Load from .dat...")
        load_dat_btn.clicked.connect(self._on_load_from_dat)
        btn_row.addWidget(load_dat_btn)
        save_btn = QPushButton("Save as Master IDE...")
        save_btn.clicked.connect(self._on_save)
        btn_row.addWidget(save_btn)
        btn_row.addStretch()
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.accept)
        btn_row.addWidget(close_btn)
        lay.addLayout(btn_row)

    def _refresh_top(self): #vers 1
        """Rebuild the header info/warning/error labels - called on\n        init and again after a Load from .dat swaps self.result."""
        while self._top.count():
            item = self._top.takeAt(0)
            if item.widget():
                item.widget().deleteLater()
        names = ", ".join(os.path.basename(p) for p in self.result.source_files)
        self._top.addWidget(QLabel(f"Merged: {names} ({self.result.total_objects} object(s))"))
        self._top.addStretch()

        for lbl in getattr(self, '_extra_lbls', []):
            self._lay.removeWidget(lbl)
            lbl.deleteLater()
        self._extra_lbls = []
        if self.result.collisions:
            warn = QLabel(
                f"WARNING: {len(self.result.collisions)} real ID collision(s) found - "
                f"see highlighted rows below. Resolve these before using this as a real "
                f"combined file.")
            warn.setStyleSheet("font-weight: bold;")
            self._lay.insertWidget(1, warn)
            self._extra_lbls.append(warn)
        for err in self.result.errors:
            lbl = QLabel(f"Error: {err}")
            self._lay.insertWidget(1, lbl)
            self._extra_lbls.append(lbl)

    def _on_load_from_dat(self): #vers 1
        path, _ = QFileDialog.getOpenFileName(
            self, "Select GTA .dat file", "",
            "GTA DAT files (gta3.dat gta_vc.dat gta.dat gta_sol.dat gtasol.dat);;All files (*.dat)")
        if not path:
            return
        ide_paths, game = collect_ide_paths_from_dat(path)
        if not ide_paths:
            QMessageBox.warning(self, "Master IDE",
                f"No IDE files found from:\n{path}")
            return
        self.result = load_master_ide(ide_paths, game=game)
        self.source_paths = ide_paths
        self.game = game
        self._refresh_top()
        self._populate()

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
                self, " ID Collisions Found",
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


def show_master_ide(main_window, ide_paths, game: str = None): #vers 2
    """Entry point - loads and merges the given real .ide files and
    shows the result. ide_paths can be a single path or a list."""
    if isinstance(ide_paths, str):
        ide_paths = [ide_paths]
    result = load_master_ide(ide_paths, game=game)
    dlg = MasterIDEDialog(main_window, result, ide_paths, game=game)
    dlg.exec()


def show_master_ide_from_dat(main_window, dat_path: str): #vers 1
    """Entry point - resolves every real IDE a game's .dat loads
    (default.dat + main dat), merges, and shows the result."""
    ide_paths, game = collect_ide_paths_from_dat(dat_path)
    if not ide_paths:
        QMessageBox.warning(main_window, "Master IDE",
            f"No IDE files found from:\n{dat_path}")
        return
    show_master_ide(main_window, ide_paths, game=game)
