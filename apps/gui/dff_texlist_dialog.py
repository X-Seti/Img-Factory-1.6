#this belongs in apps/gui/dff_texlist_dialog.py - Version: 3
# X-Seti - March 2026 - IMG Factory 1.6 - DFF Texture List Dialog
"""
DFF Texture List Dialog
Shows texture names from a DFF with TXD existence checks.
Includes option to build a TXD from a folder of PNG/BMP/TGA images.
MissingTXDDialog: batch scanner for all DFFs in the loaded IMG.
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
# show_missing_txd_dialog
# DFFTexListDialog.__init__
# DFFTexListDialog._build_ui
# DFFTexListDialog._populate
# DFFTexListDialog._find_missing_txds
# DFFTexListDialog._build_txd_from_folder
# DFFTexListDialog._open_txd_workshop
# MissingTXDDialog.__init__
# MissingTXDDialog._build_ui
# MissingTXDDialog._populate
# MissingTXDDialog._show_full
# MissingTXDDialog._create_txds_from_folder

LARGE_IMG_THRESHOLD = 200   # rows above this trigger the large-IMG prompt


class DFFTexListDialog(QDialog): #vers 1
    """Dialog showing texture names from a DFF with TXD status checks."""

    def __init__(self, parent, dff_name: str, report: dict,
                 img_entries=None, main_window=None):
        super().__init__(parent)
        self.dff_name    = dff_name
        self.report      = report
        self.img_entries = img_entries
        self.main_window = main_window
        self.setWindowTitle(f"Texture List — {dff_name}")
        self.setMinimumSize(640, 400)
        self._build_ui()
        self._populate()
        apply_dialog_theme(self)

    def _build_ui(self): #vers 1
        layout = QVBoxLayout(self)

        # IDE TXD label — from DAT Browser xref
        ide_txd = self.report.get('ide_txd_name')
        if ide_txd:
            from apps.core.dff_texlist import ide_txd_in_img
            in_img = ide_txd_in_img(ide_txd, self.img_entries)
            status = "found in IMG" if in_img else "NOT in IMG"
            color  = "#00a000" if in_img else "#b00000"
            ide_label = QLabel(
                f"IDE TXD: <b>{ide_txd}.txd</b> — "
                f"<span style='color:{color}'>{status}</span>")
            ide_label.setTextFormat(Qt.TextFormat.RichText)
            layout.addWidget(ide_label)

        # Summary label
        tex_count = len(self.report.get('textures', []))
        self._summary = QLabel(f"{tex_count} texture(s) referenced in {self.dff_name}")
        layout.addWidget(self._summary)

        # Table: Name | In IMG | On Disk
        self.table = QTableWidget()
        self.table.setColumnCount(3)
        self.table.setHorizontalHeaderLabels(["Texture Name", "In DFF Textures", "On Disk"])
        self.table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        self.table.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        self.table.setAlternatingRowColors(True)
        hdr = self.table.horizontalHeader()
        hdr.setSectionResizeMode(0, QHeaderView.ResizeMode.Stretch)
        hdr.setSectionResizeMode(1, QHeaderView.ResizeMode.ResizeToContents)
        hdr.setSectionResizeMode(2, QHeaderView.ResizeMode.ResizeToContents)
        layout.addWidget(self.table)

        btn_row = QHBoxLayout()

        self.build_txd_btn = QPushButton("Build TXD from folder...")
        self.build_txd_btn.setToolTip(
            "Pick a folder — matching PNG/BMP/TGA files are imported into a new TXD")
        self.build_txd_btn.clicked.connect(self._build_txd_from_folder)
        btn_row.addWidget(self.build_txd_btn)

        find_btn = QPushButton("Find Missing TXDs")
        find_btn.setToolTip("Scan all DFFs in the IMG for missing TXD files")
        find_btn.clicked.connect(self._find_missing_txds)
        btn_row.addWidget(find_btn)

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
        green = self._get_ui_color('success') if hasattr(self,'_get_ui_color') else QColor(0,160,0)
        red   = self._get_ui_color('error') if hasattr(self,'_get_ui_color') else QColor(180,0,0)
        grey  = self._get_ui_color('viewport_text')

        for row, name in enumerate(textures):
            self.table.setItem(row, 0, QTableWidgetItem(name))
            img_found  = in_img.get(name, False)
            img_item   = QTableWidgetItem("Yes" if img_found else "No")
            img_item.setForeground(green if img_found else red)
            img_item.setTextAlignment(Qt.AlignmentFlag.AlignCenter)
            self.table.setItem(row, 1, img_item)
            if on_disk.get(name):
                disk_item = QTableWidgetItem(os.path.basename(on_disk[name]))
                disk_item.setForeground(green)
                disk_item.setToolTip(on_disk[name])
            else:
                disk_item = QTableWidgetItem("Not found")
                disk_item.setForeground(grey)
            disk_item.setTextAlignment(Qt.AlignmentFlag.AlignCenter)
            self.table.setItem(row, 2, disk_item)

        found_img  = sum(1 for v in in_img.values() if v)
        found_disk = sum(1 for v in on_disk.values() if v)
        self._summary.setText(
            f"{len(textures)} texture(s) in {self.dff_name} — "
            f"{found_img} TXD in IMG, {found_disk} found on disk")

        # Highlight Build TXD button if IDE TXD is missing from IMG
        ide_txd = self.report.get('ide_txd_name')
        if ide_txd:
            from apps.core.dff_texlist import ide_txd_in_img
            ide_in_img = ide_txd_in_img(ide_txd, self.img_entries)
            if not ide_in_img:
                self.build_txd_btn.setStyleSheet(
                    "background-color: #8b0000; color: white; font-weight: bold;")
                self.build_txd_btn.setText("Create Missing TXD from folder...")

    def _find_missing_txds(self): #vers 1
        """Open the Missing TXD batch scanner dialog."""
        if not self.main_window:
            return
        dlg = MissingTXDDialog(self, self.main_window)
        dlg.exec()

    def _build_txd_from_folder(self): #vers 1
        """Let user pick a folder, find matching images, open TXD Workshop to build."""
        folder = QFileDialog.getExistingDirectory(
            self, "Pick folder containing PNG/BMP/TGA images")
        if not folder:
            return
        textures = self.report.get('textures', [])
        EXTS = ('.png', '.bmp', '.tga', '.jpg', '.jpeg', '.dds')
        matched = {}
        for name in textures:
            for ext in EXTS:
                path = os.path.join(folder, name + ext)
                if os.path.isfile(path):
                    matched[name] = path
                    break
        if not matched:
            QMessageBox.information(self, "Build TXD",
                "No matching image files found in:\n" + folder)
            return
        missing = [n for n in textures if n not in matched]
        msg = f"Found {len(matched)} of {len(textures)} matching image(s)."
        if missing:
            msg += "\n\nMissing (" + str(len(missing)) + "):\n" + \
                   "\n".join("  " + n for n in missing[:10])
        msg += "\n\nOpen TXD Workshop to build the TXD?"
        if QMessageBox.question(self, "Build TXD", msg) != QMessageBox.StandardButton.Yes:
            return
        self._open_txd_workshop(matched)

    def _open_txd_workshop(self, image_map: dict): #vers 1
        """Open TXD Workshop and queue the image_map for import."""
        try:
            from apps.components.Txd_Editor.txd_workshop import open_txd_workshop
            w = open_txd_workshop(self.main_window)
            if w and hasattr(w, 'import_images_as_textures'):
                w.import_images_as_textures(image_map)
            elif w and self.main_window and hasattr(self.main_window, 'log_message'):
                names = "\n".join(f"  {k}: {v}" for k, v in image_map.items())
                self.main_window.log_message(
                    "TXD Workshop opened — import these images:\n" + names)
        except Exception as e:
            QMessageBox.warning(self, "TXD Workshop",
                "Could not open TXD Workshop:\n" + str(e))


class MissingTXDDialog(QDialog): #vers 1
    """Batch scanner — shows all DFFs in the IMG with missing IDE-declared TXDs.
    For large IMGs (200+ problem entries) prompts before loading all rows.
    """

    def __init__(self, parent, main_window):
        super().__init__(parent)
        self.main_window = main_window
        self._results    = []
        self._show_all   = False
        self.setWindowTitle("Missing TXD Scanner")
        self.setMinimumSize(760, 500)
        self._build_ui()
        self._populate()
        apply_dialog_theme(self)

    def _build_ui(self): #vers 1
        layout = QVBoxLayout(self)

        self._summary = QLabel("Scanning…")
        layout.addWidget(self._summary)

        # Large-IMG warning bar — hidden by default
        self._large_bar = QHBoxLayout()
        self._large_label = QLabel("")
        self._large_label.setStyleSheet("color: #e0a000; font-weight: bold;")
        self._large_bar.addWidget(self._large_label)
        self._show_all_btn = QPushButton("Show all")
        self._show_all_btn.setVisible(False)
        self._show_all_btn.clicked.connect(self._show_full)
        self._large_bar.addWidget(self._show_all_btn)
        self._large_bar.addStretch()
        layout.addLayout(self._large_bar)

        self.table = QTableWidget()
        self.table.setColumnCount(4)
        self.table.setHorizontalHeaderLabels(
            ["DFF", "IDE TXD", "In IMG", "Textures in DFF"])
        self.table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        self.table.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        self.table.setAlternatingRowColors(True)
        hdr = self.table.horizontalHeader()
        hdr.setSectionResizeMode(0, QHeaderView.ResizeMode.Stretch)
        hdr.setSectionResizeMode(1, QHeaderView.ResizeMode.ResizeToContents)
        hdr.setSectionResizeMode(2, QHeaderView.ResizeMode.ResizeToContents)
        hdr.setSectionResizeMode(3, QHeaderView.ResizeMode.Stretch)
        layout.addWidget(self.table)

        btn_row = QHBoxLayout()
        create_btn = QPushButton("Create TXDs from folder...")
        create_btn.setToolTip(
            "Pick a folder of images — builds a TXD for each missing entry found")
        create_btn.clicked.connect(self._create_txds_from_folder)
        btn_row.addWidget(create_btn)
        btn_row.addStretch()
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.accept)
        btn_row.addWidget(close_btn)
        layout.addLayout(btn_row)

    def _populate(self): #vers 1
        from apps.core.dff_texlist import find_missing_txds
        self._results = find_missing_txds(self.main_window)

        missing = [r for r in self._results if r['ide_txd'] and r['in_img'] is False]
        no_ide  = [r for r in self._results if r['ide_txd'] is None]
        ok      = [r for r in self._results if r['in_img'] is True]

        self._summary.setText(
            f"{len(self._results)} DFFs scanned — "
            f"{len(missing)} missing TXD, "
            f"{len(no_ide)} no IDE entry, "
            f"{len(ok)} OK")

        show = missing + no_ide

        # Large IMG prompt — ask before filling hundreds of rows
        if len(show) > LARGE_IMG_THRESHOLD and not self._show_all:
            self._large_label.setText(
                f"Large result: {len(show)} entries. "
                f"Showing first {LARGE_IMG_THRESHOLD}.")
            self._show_all_btn.setText(f"Show all {len(show)}")
            self._show_all_btn.setVisible(True)
            show = show[:LARGE_IMG_THRESHOLD]
        else:
            self._large_label.setText("")
            self._show_all_btn.setVisible(False)

        self._fill_table(show)

    def _show_full(self): #vers 1
        """Remove the row cap and repopulate with all results."""
        self._show_all = True
        self._show_all_btn.setVisible(False)
        self._large_label.setText("")
        missing = [r for r in self._results if r['ide_txd'] and r['in_img'] is False]
        no_ide  = [r for r in self._results if r['ide_txd'] is None]
        self._fill_table(missing + no_ide)

    def _fill_table(self, rows: list): #vers 1
        green = self._get_ui_color('success') if hasattr(self,'_get_ui_color') else QColor(0,160,0)
        red   = self._get_ui_color('error') if hasattr(self,'_get_ui_color') else QColor(180,0,0)
        grey  = self._get_ui_color('viewport_text')
        self.table.setRowCount(len(rows))
        for row, r in enumerate(rows):
            self.table.setItem(row, 0, QTableWidgetItem(r['dff']))
            txd_item = QTableWidgetItem(
                r['ide_txd'] + '.txd' if r['ide_txd'] else '—')
            txd_item.setForeground(grey if not r['ide_txd'] else red)
            self.table.setItem(row, 1, txd_item)
            if r['in_img'] is None:
                s = QTableWidgetItem("No IDE")
                s.setForeground(grey)
            elif r['in_img']:
                s = QTableWidgetItem("Yes")
                s.setForeground(green)
            else:
                s = QTableWidgetItem("Missing")
                s.setForeground(red)
            s.setTextAlignment(Qt.AlignmentFlag.AlignCenter)
            self.table.setItem(row, 2, s)
            tex_list = ", ".join(r['textures_in_dff'][:4])
            if len(r['textures_in_dff']) > 4:
                tex_list += " +" + str(len(r['textures_in_dff']) - 4) + " more"
            self.table.setItem(row, 3, QTableWidgetItem(tex_list or "—"))

    def _create_txds_from_folder(self): #vers 1
        """Pick a folder, for each missing-TXD row try to build from matching images."""
        missing = [r for r in self._results
                   if r['ide_txd'] and r['in_img'] is False and r['textures_in_dff']]
        if not missing:
            QMessageBox.information(self, "Create TXDs",
                "No missing TXDs with known texture names found.")
            return
        folder = QFileDialog.getExistingDirectory(
            self, "Pick folder containing PNG/BMP/TGA images")
        if not folder:
            return
        EXTS = ('.png', '.bmp', '.tga', '.jpg', '.jpeg', '.dds')
        built = 0
        skipped = []
        for r in missing:
            matched = {}
            for name in r['textures_in_dff']:
                for ext in EXTS:
                    path = os.path.join(folder, name + ext)
                    if os.path.isfile(path):
                        matched[name] = path
                        break
            if matched:
                try:
                    from apps.components.Txd_Editor.txd_workshop import open_txd_workshop
                    w = open_txd_workshop(self.main_window)
                    if w and hasattr(w, 'import_images_as_textures'):
                        w.import_images_as_textures(matched)
                    built += 1
                except Exception:
                    skipped.append(r['dff'])
            else:
                skipped.append(r['dff'])
        msg = "Opened TXD Workshop for " + str(built) + " model(s)."
        if skipped:
            msg += "\nSkipped " + str(len(skipped)) + " (no matching images found)."
        QMessageBox.information(self, "Create TXDs", msg)


def show_missing_txd_dialog(main_window): #vers 1
    """Open the Missing TXD batch scanner directly from a menu."""
    dlg = MissingTXDDialog(main_window, main_window)
    dlg.exec()


def show_dff_texlist_dialog(main_window, dff_name: str, dff_data: bytes,
                            ide_txd_name: str = None): #vers 2
    """Extract textures from dff_data and show the dialog.
    ide_txd_name: TXD name from IDE cross-reference (DAT Browser xref).
    """
    from apps.core.dff_texlist import get_dff_texture_report

    img_entries = []
    search_dirs = []

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
    report['ide_txd_name'] = ide_txd_name

    dlg = DFFTexListDialog(main_window, dff_name, report,
                           img_entries=img_entries, main_window=main_window)
    dlg.exec()
