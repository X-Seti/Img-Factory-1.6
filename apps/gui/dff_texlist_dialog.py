#this belongs in apps/gui/dff_texlist_dialog.py - Version: 1
# X-Seti - March 2026 - IMG Factory 1.6 - DFF Texture List Dialog
"""
DFF Texture List Dialog
Shows texture names from a DFF with TXD existence checks.
Includes option to build a TXD from a folder of PNG/BMP/TGA images.
"""

import os
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QTableWidget, QTableWidgetItem,
    QPushButton, QLabel, QFileDialog, QHeaderView, QAbstractItemView,
    QMessageBox
)
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QColor

from apps.core.theme_utils import apply_dialog_theme

##Methods list -
# show_dff_texlist_dialog
# DFFTexListDialog.__init__
# DFFTexListDialog._build_ui
# DFFTexListDialog._populate
# DFFTexListDialog._build_txd_from_folder
# DFFTexListDialog._open_txd_workshop


class DFFTexListDialog(QDialog): #vers 1
    """Dialog showing texture names from a DFF with TXD status checks."""

    def __init__(self, parent, dff_name: str, report: dict, img_entries=None,
                 main_window=None):
        super().__init__(parent)
        self.dff_name    = dff_name
        self.report      = report        # from dff_texlist.get_dff_texture_report()
        self.img_entries = img_entries
        self.main_window = main_window
        self.setWindowTitle(f"Texture List — {dff_name}")
        self.setMinimumSize(640, 400)
        self._build_ui()
        self._populate()
        apply_dialog_theme(self)

    def _build_ui(self): #vers 1
        layout = QVBoxLayout(self)

        # Summary label
        tex_count = len(self.report.get('textures', []))
        self._summary = QLabel(f"{tex_count} texture(s) referenced in {self.dff_name}")
        layout.addWidget(self._summary)

        # Table: Name | In IMG | On Disk
        self.table = QTableWidget()
        self.table.setColumnCount(3)
        self.table.setHorizontalHeaderLabels(["Texture Name", "In IMG", "On Disk"])
        self.table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        self.table.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        self.table.setAlternatingRowColors(True)
        hdr = self.table.horizontalHeader()
        hdr.setSectionResizeMode(0, QHeaderView.ResizeMode.Stretch)
        hdr.setSectionResizeMode(1, QHeaderView.ResizeMode.ResizeToContents)
        hdr.setSectionResizeMode(2, QHeaderView.ResizeMode.ResizeToContents)
        layout.addWidget(self.table)

        # Buttons
        btn_row = QHBoxLayout()

        self.build_txd_btn = QPushButton("Build TXD from folder...")
        self.build_txd_btn.setToolTip(
            "Pick a folder — matching PNG/BMP/TGA files are imported into a new TXD")
        self.build_txd_btn.clicked.connect(self._build_txd_from_folder)
        btn_row.addWidget(self.build_txd_btn)

        btn_row.addStretch()

        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.accept)
        btn_row.addWidget(close_btn)
        layout.addLayout(btn_row)

    def _populate(self): #vers 1
        textures = self.report.get('textures', [])
        in_img   = self.report.get('in_img',   {})
        on_disk  = self.report.get('on_disk',  {})

        self.table.setRowCount(len(textures))
        green = QColor(0, 160, 0)
        red   = QColor(180, 0, 0)
        grey  = QColor(120, 120, 120)

        for row, name in enumerate(textures):
            self.table.setItem(row, 0, QTableWidgetItem(name))

            img_found  = in_img.get(name, False)
            disk_found = on_disk.get(name)

            img_item = QTableWidgetItem("Yes" if img_found else "No")
            img_item.setForeground(green if img_found else red)
            img_item.setTextAlignment(Qt.AlignmentFlag.AlignCenter)
            self.table.setItem(row, 1, img_item)

            if disk_found:
                disk_item = QTableWidgetItem(os.path.basename(disk_found))
                disk_item.setForeground(green)
                disk_item.setToolTip(disk_found)
            else:
                disk_item = QTableWidgetItem("Not found")
                disk_item.setForeground(grey)
            disk_item.setTextAlignment(Qt.AlignmentFlag.AlignCenter)
            self.table.setItem(row, 2, disk_item)

        # Update summary counts
        found_img  = sum(1 for v in in_img.values() if v)
        found_disk = sum(1 for v in on_disk.values() if v)
        self._summary.setText(
            f"{len(textures)} texture(s) in {self.dff_name} — "
            f"{found_img} TXD in IMG, {found_disk} found on disk")

    def _build_txd_from_folder(self): #vers 1
        """Let user pick a folder, find matching images, open TXD Workshop to build."""
        folder = QFileDialog.getExistingDirectory(
            self, "Pick folder containing PNG/BMP/TGA images")
        if not folder:
            return

        textures = self.report.get('textures', [])
        EXTS = ('.png', '.bmp', '.tga', '.jpg', '.jpeg', '.dds')

        # Find matching files
        matched = {}
        for name in textures:
            for ext in EXTS:
                path = os.path.join(folder, name + ext)
                if os.path.isfile(path):
                    matched[name] = path
                    break

        if not matched:
            QMessageBox.information(self, "Build TXD",
                f"No matching image files found in:\n{folder}\n\n"
                f"Expected filenames like: {textures[0]}.png" if textures else "No textures.")
            return

        missing = [n for n in textures if n not in matched]
        msg = f"Found {len(matched)} of {len(textures)} matching image(s)."
        if missing:
            msg += f"\n\nMissing ({len(missing)}):\n" + "\n".join(f"  {n}" for n in missing[:10])
        msg += "\n\nOpen TXD Workshop to build the TXD?"

        if QMessageBox.question(self, "Build TXD", msg) != QMessageBox.StandardButton.Yes:
            return

        self._open_txd_workshop(matched)

    def _open_txd_workshop(self, image_map: dict): #vers 1
        """Open TXD Workshop and queue the image_map for import."""
        try:
            from apps.components.Txd_Editor.txd_workshop import open_txd_workshop
            w = open_txd_workshop(self.main_window)
            # Pass the image map so TXD Workshop can auto-import
            if w and hasattr(w, 'import_images_as_textures'):
                w.import_images_as_textures(image_map)
            elif w:
                # Fallback: log the files
                names = "\n".join(f"  {k}: {v}" for k, v in image_map.items())
                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(
                        f"TXD Workshop opened — import these images:\n{names}")
        except Exception as e:
            QMessageBox.warning(self, "TXD Workshop",
                f"Could not open TXD Workshop:\n{e}")


def show_dff_texlist_dialog(main_window, dff_name: str, dff_data: bytes): #vers 1
    """Extract textures from dff_data and show the dialog.
    main_window: IMG Factory main window (for IMG entries + game root).
    """
    from apps.core.dff_texlist import get_dff_texture_report

    img_entries  = []
    search_dirs  = []

    if main_window:
        img = getattr(main_window, 'current_img', None)
        if img and hasattr(img, 'entries'):
            img_entries = img.entries
        game_root = getattr(main_window, 'game_root', '')
        if game_root:
            search_dirs = [
                game_root,
                os.path.join(game_root, 'models'),
                os.path.join(game_root, 'models', 'txd'),
            ]

    report = get_dff_texture_report(dff_data, img_entries, search_dirs)

    dlg = DFFTexListDialog(main_window, dff_name, report,
                           img_entries=img_entries, main_window=main_window)
    dlg.exec()
