#!/usr/bin/env python3
#this belongs in apps/components/Disc_Workshop/disc_workshop.py - Version: 2
# X-Seti - October 23 2026 - IMG Factory 1.6 - Disc Workshop
# Built on GUIWorkshop base (same as Capture / Retro GTA Workshop)

"""
Disc Workshop - CD / DVD images (CloneCD .img, raw .bin, .iso): file tree,
extract files or the whole disc, replace / grow / add files (sector EDC /
ECC rewritten), save (backup first) or save as a copy. Map and style files
open in Retro GTA Workshop.
"""

##Methods list -
# open_disc_workshop
#
##class DiscWorkshop: -
# __init__
# _add_file
# _build_ribbons
# _extract_all
# _extract_selected
# _fill_tree
# _item_opened
# _open_file
# _replace_selected
# _save_as
# _save_file
# _selected_entries
# closeEvent
# setup_ui

import os
import sys
import tempfile
from pathlib import Path

_root = Path(__file__).resolve().parents[3]       # project root for standalone runs
if str(_root) not in sys.path:
    sys.path.insert(0, str(_root))

from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import (QApplication, QFileDialog, QLabel, QMessageBox, QTreeWidget, QTreeWidgetItem,
                             QVBoxLayout, QWidget)

from apps.methods.cd_image import CDImage, is_cd_image
from apps.methods.gui_workshop import GUIWorkshop
from apps.methods.imgfactory_svg_icons import SVGIconFactory
from apps.methods.ribbon_system import RibbonMixin

App_name = "Disc Workshop"
Build = "Build 1"
KIND_LABEL = {'data': 'Data', 'form2': 'Video / XA (raw 2336)', 'audio': 'CD audio (.wav)', 'dir': 'Folder'}
RETRO_EXTS = ('.gmp', '.sty', '.cmp', '.g24')


class DiscWorkshop(RibbonMixin, GUIWorkshop): #vers 1

    App_name        = App_name
    App_build       = Build
    App_author      = "X-Seti"
    App_year        = "2026"
    App_description = ("CD / DVD disc images: browse, extract and replace files.\n"
                       "CloneCD .img, raw .bin (PS1 Mode 2) and .iso.")
    App_icon        = "disc_app_icon"
    config_key      = "disc_workshop"
    WS_DEFAULTS     = {"last_folder": ""}
    DROP_EXTS       = ('.img', '.bin', '.iso')
    _ribbon_name    = "disc_workshop"
    _RIBBON_LAYOUT_VERSION = 1

    def __init__(self, parent=None, main_window=None): #vers 1
        self._disc = None
        super().__init__(parent, main_window)
        self.ribbon_restore_state()

    def setup_ui(self): #vers 1
        """Titlebar / ribbons + file tree / status bar."""
        ml = QVBoxLayout(self)
        ml.setContentsMargins(*self.get_content_margins())
        ml.setSpacing(self.setspacing)
        ml.addWidget(self._create_toolbar())
        page = QWidget()
        lay = QVBoxLayout(page)
        lay.setContentsMargins(4, 4, 4, 4)
        self._info = QLabel("Open a disc image (.img / .bin / .iso)")
        self._tree = QTreeWidget()
        self._tree.setHeaderLabels(["File", "Size", "Sector", "Type"])
        self._tree.setSelectionMode(QTreeWidget.SelectionMode.ExtendedSelection)
        self._tree.itemDoubleClicked.connect(lambda it, _c: self._item_opened(it))
        lay.addWidget(self._info)
        lay.addWidget(self._tree, 1)
        ml.addWidget(self.ribbon_wrap(page), 1)
        self._build_ribbons()
        self._status_widget = self._create_status_bar()
        ml.addWidget(self._status_widget)
        self._status_widget.setVisible(self.WS.get("show_statusbar", True))

    def _build_ribbons(self): #vers 2
        B = self.ribbon_button
        tb = self.ribbon_toolbar("Disc")
        B(tb, "open_icon", "Open a disc image", lambda: self._open_file(None))
        B(tb, "save_icon", "Save changes into the image (backup first)", self._save_file)
        B(tb, "save_all_icon", "Save as a new image", self._save_as)
        tb = self.ribbon_toolbar("Files")
        B(tb, "export_icon", "Extract the selected files", self._extract_selected)
        B(tb, "folder_icon", "Extract the whole disc", self._extract_all)
        B(tb, "import_icon", "Replace the selected file (moves after the image end when bigger)",
          self._replace_selected)
        B(tb, "add_icon", "Add a file to the selected folder (or the root)", self._add_file)

    def _open_file(self, path=None): #vers 1
        """Open a disc image and list its files."""
        if not path:
            path, _ = QFileDialog.getOpenFileName(self, "Open disc image", self.WS.get("last_folder", ""),
                                                  "Disc images (*.img *.bin *.iso);;All files (*)")
            if not path:
                return
        if not is_cd_image(path):
            QMessageBox.warning(self, App_name, "Not a CD / DVD image with an ISO 9660 file system.")
            return
        try:
            self._disc = CDImage(path)
        except (OSError, ValueError) as ex:
            QMessageBox.warning(self, App_name, str(ex))
            return
        self.WS.set("last_folder", os.path.dirname(path))
        self.WS.save()
        self._fill_tree()

    def _fill_tree(self): #vers 1
        """File tree with sizes, start sector and kind."""
        d = self._disc
        self._tree.clear()
        nodes = {'': self._tree.invisibleRootItem()}
        for e in d.entries():
            parent, _s, name = e['path'].rpartition('/')
            it = QTreeWidgetItem([name, '' if e['dir'] else f"{e['size']:,}", str(e['lba']),
                                  KIND_LABEL.get(e['kind'], e['kind'])])
            it.setData(0, Qt.ItemDataRole.UserRole, e)
            nodes.get(parent, nodes['']).addChild(it)
            nodes[e['path']] = it
        self._tree.expandAll()
        for c in range(4):
            self._tree.resizeColumnToContents(c)
        sector = "raw 2352 (Mode %d)" % d.mode if d.raw else "ISO 2048"
        self._info.setText(f"{os.path.basename(d.path)}   volume: {d.volume}   {d.system}   sectors: {sector}   "
                           f"{sum(not e['dir'] for e in d.entries())} files")
        if hasattr(self, 'title_label'):
            self.title_label.setText(f"{App_name} - {os.path.basename(d.path)}")

    def _selected_entries(self) -> list: #vers 1
        return [it.data(0, Qt.ItemDataRole.UserRole) for it in self._tree.selectedItems()
                if not it.data(0, Qt.ItemDataRole.UserRole)['dir']]

    def _extract_selected(self): #vers 1
        """Selected files into a folder."""
        sel = self._selected_entries()
        if not sel:
            self._set_status("Select files first")
            return
        out = QFileDialog.getExistingDirectory(self, "Extract to folder", self.WS.get("last_folder", ""))
        if not out:
            return
        for e in sel:
            name = e['path'].split('/')[-1]
            with open(os.path.join(out, name), 'wb') as f:
                f.write(self._disc.read(e))
        self._set_status(f"Extracted {len(sel)} file(s) to {out}")

    def _extract_all(self): #vers 1
        """Whole disc tree into a folder."""
        if self._disc is None:
            return
        out = QFileDialog.getExistingDirectory(self, "Extract whole disc to folder", self.WS.get("last_folder", ""))
        if not out:
            return
        target = os.path.join(out, os.path.splitext(os.path.basename(self._disc.path))[0] + "_files")
        QApplication.setOverrideCursor(Qt.CursorShape.WaitCursor)
        try:
            n = self._disc.extract_all(target)
        finally:
            QApplication.restoreOverrideCursor()
        self._set_status(f"Extracted {n} files to {target}")

    def _replace_selected(self): #vers 2
        """Replace one file (in place, or moved after the image end); written on Save."""
        sel = self._selected_entries()
        if len(sel) != 1:
            self._set_status("Select one file to replace")
            return
        e = sel[0]
        path, _ = QFileDialog.getOpenFileName(self, f"Replace {e['path']}", self.WS.get("last_folder", ""))
        if not path:
            return
        try:
            with open(path, 'rb') as f:
                self._disc.grow(e, f.read())
        except (OSError, ValueError) as ex:
            QMessageBox.warning(self, "Replace", str(ex))
            return
        self._fill_tree()
        self._set_status(f"{e['path']} replaced - Save to write the image")

    def _add_file(self): #vers 1
        """Add a file into the selected folder (root if none); written on Save."""
        if self._disc is None:
            return
        sel = [it.data(0, Qt.ItemDataRole.UserRole) for it in self._tree.selectedItems()]
        folder = ''
        if sel:
            e = sel[0]
            folder = e['path'] if e['dir'] else e['path'].rpartition('/')[0]
        path, _ = QFileDialog.getOpenFileName(self, f"Add file to /{folder}", self.WS.get("last_folder", ""))
        if not path:
            return
        name = os.path.basename(path).upper()
        try:
            with open(path, 'rb') as f:
                self._disc.add_file(folder, name, f.read())
        except (OSError, ValueError) as ex:
            QMessageBox.warning(self, "Add file", str(ex))
            return
        self._fill_tree()
        self._set_status(f"{name} added to /{folder} - Save to write the image")

    def _save_file(self): #vers 1
        """Changed sectors into the image (image backed up first)."""
        from apps.methods.file_backup import backup_file
        if self._disc is None or not self._disc._dirty:
            self._set_status("Nothing to save")
            return
        if backup_file(self._disc.path) is None:
            QMessageBox.critical(self, App_name, "Backup failed - image not written.")
            return
        n = len(self._disc._dirty)
        self._disc.save()
        self._set_status(f"Saved {n} sectors into {self._disc.path}")

    def _save_as(self): #vers 1
        """Copy of the image with the changes."""
        if self._disc is None:
            return
        path, _ = QFileDialog.getSaveFileName(self, "Save disc image as", self._disc.path,
                                              "Disc images (*.img *.bin *.iso)")
        if not path or path == self._disc.path:
            return
        self._disc.save(path)
        self._set_status(f"Saved a copy: {path}")

    def _item_opened(self, item): #vers 1
        """Double-click: GTA1 / GTA2 maps and styles open in Retro GTA Workshop."""
        e = item.data(0, Qt.ItemDataRole.UserRole)
        if e['dir'] or not e['path'].lower().endswith(RETRO_EXTS):
            return
        tmp = os.path.join(tempfile.mkdtemp(prefix='disc_'), e['path'].split('/')[-1])
        with open(tmp, 'wb') as f:
            f.write(self._disc.read(e))
        mw = self.main_window
        if mw is not None and hasattr(mw, 'open_retro_gta_workshop_docked'):
            mw.open_retro_gta_workshop_docked(tmp)
        else:
            from apps.components.Retro_GTA_Workshop.retro_gta_workshop import open_retro_gta_workshop
            self._retro = open_retro_gta_workshop(None, tmp)

    def closeEvent(self, ev): #vers 1
        """Ask before losing replaced files."""
        if self._disc is not None and self._disc._dirty and QMessageBox.question(
                self, App_name, "Close without saving the replaced files?") != QMessageBox.StandardButton.Yes:
            ev.ignore()
            return
        super().closeEvent(ev)


def open_disc_workshop(main_window=None, file_path=None): #vers 1
    """Open docked in an IMG Factory tab, or standalone."""
    mw = main_window
    if mw is not None and hasattr(mw, 'main_tab_widget'):
        c = QWidget()
        lay = QVBoxLayout(c)
        lay.setContentsMargins(0, 0, 0, 0)
        w = DiscWorkshop(parent=c, main_window=mw)
        lay.addWidget(w)
        tw = mw.main_tab_widget
        tw.setCurrentIndex(tw.addTab(c, SVGIconFactory.disc_app_icon(20),
                                     os.path.basename(file_path) if file_path else App_name))
        if hasattr(mw, '_ensure_tab_area_visible'):
            mw._ensure_tab_area_visible()
    else:
        w = DiscWorkshop(main_window=main_window)
        w.resize(1100, 760)
        w.show()
    if file_path:
        w._open_file(file_path)
    return w


if __name__ == "__main__":
    app = QApplication(sys.argv)
    app.setWindowIcon(SVGIconFactory.disc_app_icon(64))
    win = open_disc_workshop(None, sys.argv[1] if len(sys.argv) > 1 else None)
    sys.exit(app.exec())
