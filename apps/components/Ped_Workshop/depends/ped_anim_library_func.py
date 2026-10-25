#this belongs in apps/components/Ped_Workshop/depends/ped_anim_library_func.py - Version: 2
# X-Seti - October 23 2026 - IMG Factory 1.6 - Ped Workshop anim search and batch export

"""
Search every anim in the game (ped.ifp, anim folder IFPs, anim.img) in one
list (IV .wad too), and batch export chosen anims to one IFP or one IFP per anim.
"""

##class PedAnimLibraryMixin: -
# _anim_batch_export
# _anim_game_index
# _anim_game_search
# _anim_ifp_sources
# _anim_open_found

import copy
import os

from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import (QAbstractItemView, QDialog, QDialogButtonBox, QFileDialog, QHBoxLayout,
                             QLabel, QLineEdit, QListWidget, QListWidgetItem, QMessageBox, QPushButton, QRadioButton,
                             QTreeWidget, QTreeWidgetItem, QVBoxLayout)

from apps.methods.ifp_format import IFPFile, parse_ifp, save_ifp


class PedAnimLibraryMixin: #vers 1
    """Game-wide anim search and batch export for PedWorkshop."""

    def _anim_ifp_sources(self) -> list: #vers 2
        """IFP sources: combo entries plus loose IFPs beside them."""
        out = [self._ifp_source.itemData(i) for i in range(self._ifp_source.count())]
        folders = {os.path.dirname(p) for p in out if p and '.img:' not in p.lower()}
        for d in sorted(folders):
            for n in sorted(os.listdir(d)):
                p = os.path.join(d, n)
                if n.lower().endswith(('.ifp', '.wad')) and p not in out:
                    out.append(p)
        return [p for p in out if p]

    def _anim_game_index(self) -> list: #vers 2
        """(source, ifp name, anim name, duration) for every anim; cached."""
        sources = self._anim_ifp_sources()
        cache = getattr(self, '_anim_index_cache', None)
        if cache and cache[0] == sources:
            return cache[1]
        from apps.methods.img_core_classes import IMGFile
        rows, archives = [], {}
        for src in sources:
            img_path, sep, entry = src.rpartition(':')
            try:
                if sep and img_path.lower().endswith('.img'):
                    arc = archives.get(img_path)
                    if arc is None:
                        arc = archives[img_path] = IMGFile(img_path)
                        arc.open()
                    e = next(x for x in arc.entries if x.name.lower() == entry.lower())
                    data = arc.read_entry_data(e)
                else:
                    with open(src, 'rb') as fh:
                        data = fh.read()
                label = os.path.splitext(os.path.basename(entry if sep else src))[0]
                if src.lower().endswith('.wad'):                    # GTA IV: header names only
                    from apps.methods.iv_anim import wad_anim_list
                    rows += [(src, label, n, d) for n, d in wad_anim_list(data)]
                    continue
                f = parse_ifp(data)
            except Exception as ex:                                # unreadable IFP: listed in status
                self._set_status(f"Anim search: skipped {os.path.basename(src)} ({ex})")
                continue
            rows += [(src, f.name, a.name, a.duration()) for a in f.anims]
        self._anim_index_cache = (sources, rows)
        return rows

    def _anim_game_search(self): #vers 1
        """Dialog: search all game anims; open picks IFP and anim."""
        if not self._ifp_source.count():
            QMessageBox.information(self, "Search all anims", "Open a game folder first.")
            return
        self._set_status("Indexing game anims...")
        rows = self._anim_game_index()
        dlg = QDialog(self)
        dlg.setWindowTitle(f"Search all anims ({len(rows)})")
        dlg.resize(560, 480)
        lay = QVBoxLayout(dlg)
        edit = QLineEdit()
        edit.setPlaceholderText("Anim or IFP name, e.g. walk, fight, swim")
        tree = QTreeWidget()
        tree.setHeaderLabels(["Anim", "IFP", "Length"])
        tree.setRootIsDecorated(False)
        tree.setSortingEnabled(True)
        for src, ifp, name, dur in rows:
            it = QTreeWidgetItem([name, ifp, f"{dur:.2f} s"])
            it.setData(0, Qt.ItemDataRole.UserRole, (src, name))
            it.setToolTip(1, src)
            tree.addTopLevelItem(it)
        tree.sortByColumn(0, Qt.SortOrder.AscendingOrder)
        tree.resizeColumnToContents(0)
        count = QLabel()

        def filt(text):
            t, shown = text.lower(), 0
            for i in range(tree.topLevelItemCount()):
                it = tree.topLevelItem(i)
                hide = bool(t) and t not in it.text(0).lower() and t not in it.text(1).lower()
                it.setHidden(hide)
                shown += not hide
            count.setText(f"{shown} of {len(rows)} anims")

        def open_item(it=None):
            it = it or tree.currentItem()
            if it is not None:
                self._anim_open_found(*it.data(0, Qt.ItemDataRole.UserRole))
                dlg.accept()
        edit.textChanged.connect(filt)
        tree.itemDoubleClicked.connect(lambda it, _c: open_item(it))
        btns = QDialogButtonBox(QDialogButtonBox.StandardButton.Open | QDialogButtonBox.StandardButton.Close)
        btns.accepted.connect(open_item)
        btns.rejected.connect(dlg.reject)
        lay.addWidget(edit)
        lay.addWidget(tree, 1)
        row = QHBoxLayout()
        row.addWidget(count, 1)
        row.addWidget(btns)
        lay.addLayout(row)
        filt('')
        edit.setFocus()
        self._set_status(f"{len(rows)} anims in {len(self._anim_ifp_sources())} IFPs")
        dlg.exec()

    def _anim_open_found(self, src: str, anim: str): #vers 1
        """Open the IFP (combo synced) and select the anim."""
        if self._ifp is None or self._ifp.path != src:
            i = self._ifp_source.findData(src)
            if i < 0:
                self._ifp_source.addItem(os.path.basename(src), src)
                i = self._ifp_source.count() - 1
            self._ifp_source.setCurrentIndex(i)
            self._anim_open_ifp(src)
        self._anim_search.clear()
        items = self._anim_list.findItems(anim, Qt.MatchFlag.MatchFixedString)
        if items:
            self._anim_list.setCurrentItem(items[0])
            self._anim_list.scrollToItem(items[0])

    def _anim_batch_export(self): #vers 2
        """Export chosen anims of the open IFP: one IFP, or one per anim."""
        if self._ifp is None or not self._ifp.anims:
            QMessageBox.information(self, "Batch export", "Open an IFP first.")
            return
        if self._ifp.version == 'WAD':
            QMessageBox.information(self, "Batch export", "GTA IV .wad anims play only - export is not done yet.")
            return
        dlg = QDialog(self)
        dlg.setWindowTitle(f"Batch export - {self._ifp.name}")
        dlg.resize(380, 460)
        lay = QVBoxLayout(dlg)
        lst = QListWidget()
        lst.setSelectionMode(QAbstractItemView.SelectionMode.ExtendedSelection)
        picked = {it.text() for it in self._anim_list.selectedItems()}
        for a in self._ifp.anims:
            it = QListWidgetItem(a.name)
            it.setFlags(it.flags() | Qt.ItemFlag.ItemIsUserCheckable)
            it.setCheckState(Qt.CheckState.Checked if a.name in picked else Qt.CheckState.Unchecked)
            it.setData(Qt.ItemDataRole.UserRole, a)
            lst.addItem(it)
        edit = QLineEdit()
        edit.setPlaceholderText("Filter, then tick all shown")

        def filt(text):
            for i in range(lst.count()):
                lst.item(i).setHidden(bool(text) and text.lower() not in lst.item(i).text().lower())

        def tick(state):
            for i in range(lst.count()):
                if not lst.item(i).isHidden():
                    lst.item(i).setCheckState(state)
        edit.textChanged.connect(filt)
        row = QHBoxLayout()
        row.addWidget(edit, 1)
        for label, state in (("All", Qt.CheckState.Checked), ("None", Qt.CheckState.Unchecked)):
            btn = QPushButton(label)
            btn.clicked.connect(lambda _c=False, s=state: tick(s))
            row.addWidget(btn)
        one = QRadioButton("One IFP with all ticked anims")
        each = QRadioButton("One IFP per anim (folder)")
        one.setChecked(True)
        btns = QDialogButtonBox(QDialogButtonBox.StandardButton.Save | QDialogButtonBox.StandardButton.Cancel)
        btns.accepted.connect(dlg.accept)
        btns.rejected.connect(dlg.reject)
        lay.addLayout(row)
        lay.addWidget(lst, 1)
        lay.addWidget(one)
        lay.addWidget(each)
        lay.addWidget(btns)
        if dlg.exec() != QDialog.DialogCode.Accepted:
            return
        anims = [lst.item(i).data(Qt.ItemDataRole.UserRole) for i in range(lst.count())
                 if lst.item(i).checkState() == Qt.CheckState.Checked]
        if not anims:
            return
        start = os.path.dirname(self._ifp.path.rpartition(':')[0] or self._ifp.path) or ''
        ver = self._ifp.version
        if one.isChecked():
            path, _ = QFileDialog.getSaveFileName(self, "Export anims", os.path.join(start, f"{self._ifp.name}_export.ifp"),
                                                  "IFP animations (*.ifp)")
            if not path:
                return
            name = os.path.splitext(os.path.basename(path))[0][:23]
            jobs = [(path, IFPFile(ver, name, [copy.deepcopy(a) for a in anims]))]
        else:
            folder = QFileDialog.getExistingDirectory(self, "Export anims to folder", start)
            if not folder:
                return
            jobs = [(os.path.join(folder, f"{a.name}.ifp"), IFPFile(ver, a.name[:23], [copy.deepcopy(a)]))
                    for a in anims]
        try:
            for path, f in jobs:
                save_ifp(f, path)
        except Exception as ex:
            QMessageBox.warning(self, "Batch export", str(ex))
            return
        self._set_status(f"Exported {len(anims)} anims to {len(jobs)} IFP file(s)")
