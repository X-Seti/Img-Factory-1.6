#this belongs in apps/components/Cutscene_Workshop/depends/cutscene_cast_func.py - Version: 4
# X-Seti - November 06 2026 - IMG Factory 1.6 - Cutscene Workshop cast

"""
Cast panel: every model of the cutscene with its animation. Swap a model
for another ped (or a model file) in the preview, add / remove .cut models
(SA, IV), export a model as it is or converted for in-game use (cutscene
ped to a game skeleton, cutscene vehicle onto a donor car; IV cutscene
peds are game peds already), open it in the Ped / Vehicle / Model
Workshop, and write the swaps into the game IMG.
"""

##Methods list -
#
##class CutsceneCastMixin: -
# _cast_add
# _cast_apply
# _cast_clear
# _cast_current
# _cast_export
# _cast_export_game
# _cast_fill
# _cast_for_game
# _cast_import
# _cast_iv
# _cast_model_files
# _cast_kind
# _cast_map_dialog
# _cast_model_names
# _cast_pick_model
# _cast_refresh
# _cast_remove
# _cast_rows
# _cast_section_changed
# _cast_send
# _cast_swap
# _create_cast_panel
# _cs_cast_fetch
# _cs_game_file

import importlib.util
import os
import re

from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import (QComboBox, QDialog, QDialogButtonBox, QFileDialog, QFormLayout, QGridLayout,
                             QHeaderView, QInputDialog, QLabel, QLineEdit, QListWidget, QMessageBox,
                             QPushButton, QScrollArea, QTableWidget, QTableWidgetItem, QVBoxLayout, QWidget)

CAST_COLS = ["Model", "Animation", "Kind", "Found in", "Stand-in"]
MODEL_EXTS = ('.dff', '.txd')                         # III / VC / SA
IV_EXTS = ('.wdd', '.wft', '.wtd', '.wbs', '.wdr')     # GTA IV ped / fragment / textures / bounds / prop
WORKSHOPS = {'ped': ('apps.components.Ped_Workshop.ped_workshop', "Ped Workshop"),
             'vehicle': ('apps.components.Vehicle_Workshop.vehicle_workshop', "Vehicle Workshop"),
             'object': ('apps.components.Model_Editor.model_workshop', "Model Workshop")}


class CutsceneCastMixin: #vers 3
    """Cast of the current cutscene: swap, add, remove, import, export, send."""

    def _create_cast_panel(self): #vers 1
        """Cast table over the action buttons."""
        w = QWidget()
        lay = QVBoxLayout(w)
        lay.setContentsMargins(2, 2, 2, 2)

        self._cast_table = QTableWidget(0, len(CAST_COLS))
        self._cast_table.setHorizontalHeaderLabels(CAST_COLS)
        self._cast_table.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
        self._cast_table.setSelectionBehavior(QTableWidget.SelectionBehavior.SelectRows)
        self._cast_table.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.ResizeToContents)
        self._cast_table.doubleClicked.connect(lambda *_: self._cast_swap())
        lay.addWidget(self._cast_table, 1)

        grid = QGridLayout()

        # 1. Swap Button
        self._btn_swap = QPushButton("Swap")
        self._btn_swap.setToolTip("Play this part with another model (preview)")
        self._btn_swap.clicked.connect(self._cast_swap)
        # Example for individual styling/sizing:
        self._btn_swap.setStyleSheet("background-color: #333; color: white;")
        self._btn_swap.setFixedSize(100, 30)
        grid.addWidget(self._btn_swap, 0, 0)

        # 2. Clear Button
        self._btn_clear = QPushButton("Clear")
        self._btn_clear.setToolTip("Back to the original model")
        self._btn_clear.clicked.connect(self._cast_clear)
        grid.addWidget(self._btn_clear, 0, 1)

        # 3. Add Button
        self._btn_add = QPushButton("Add")
        self._btn_add.setToolTip("Add a model to the .cut (SA)")
        self._btn_add.clicked.connect(self._cast_add)
        grid.addWidget(self._btn_add, 0, 2)

        # 4. Remove Button
        self._btn_remove = QPushButton("Remove")
        self._btn_remove.setToolTip("Remove the model from the .cut (SA)")
        self._btn_remove.clicked.connect(self._cast_remove)
        grid.addWidget(self._btn_remove, 1, 0)

        # 5. Import Button
        self._btn_import = QPushButton("Import")
        self._btn_import.setToolTip("Stand-in from a DFF / TXD file")
        self._btn_import.clicked.connect(self._cast_import)
        grid.addWidget(self._btn_import, 1, 1)

        # 6. Export Button
        self._btn_export = QPushButton("Export")
        self._btn_export.setToolTip("Save the DFF / TXD as they are")
        self._btn_export.clicked.connect(self._cast_export)
        grid.addWidget(self._btn_export, 1, 2)

        # 7. For Game Button
        self._btn_export_game = QPushButton("For game")
        self._btn_export_game.setToolTip("Save converted for in-game use")
        self._btn_export_game.clicked.connect(self._cast_export_game)
        grid.addWidget(self._btn_export_game, 2, 0)

        # 8. Send To Button
        self._btn_send = QPushButton("Send to")
        self._btn_send.setToolTip("Open in the Ped / Vehicle / Model Workshop")
        self._btn_send.clicked.connect(self._cast_send)
        grid.addWidget(self._btn_send, 2, 1)

        # 9. Write Button
        self._btn_apply = QPushButton("Write")
        self._btn_apply.setToolTip("Write the stand-ins into the game IMG")
        self._btn_apply.clicked.connect(self._cast_apply)
        grid.addWidget(self._btn_apply, 2, 2)

        lay.addLayout(grid)
        self._cast_swaps, self._cast_kinds = {}, {}
        return w


    def _cast_iv(self) -> bool: #vers 1
        """True for a GTA IV cutscene."""
        return bool(getattr(self, '_cs_iv', False))


    def _cast_model_files(self, name: str) -> dict: #vers 1
        """{ext: bytes} of a model's files (stand-in when swapped)."""
        sw = self._cast_swaps.get(name.lower())
        if sw is not None:
            return dict(sw['files'])
        out = {}
        for ext in (IV_EXTS if self._cast_iv() else MODEL_EXTS):
            data = self._cs_fetch(name, ext)
            if data is not None:
                out[ext] = data
        return out


    def _cast_rows(self) -> list: #vers 2
        """[(model, animation, row key or None)] like the preview builds them (IV: first section)."""
        if self._cast_iv():
            from apps.methods.cutscene_files import iv_cut_parts
            parts = iv_cut_parts(self._cs_sections)
            return [(m, a, i) for i, m, a, _f in (parts[0]['models'] if parts else [])]
        rows = dict(getattr(self, '_cs_sections', [])).get('model', [])
        out = []
        for i, row in enumerate(rows):
            parts = [p.strip() for p in row.split(',')]
            if len(parts) >= 2 and parts[1]:
                out.append((parts[1], parts[2] if len(parts) > 2 and parts[2] else parts[1], i))
        if not out and getattr(self, '_cs_cur', None):              # III / VC: the IFP's animations
            from apps.methods.ifp_format import parse_ifp
            raw = self._cs_fetch(self._cs_cur, '.ifp')
            out = [(a.name, a.name, None) for a in (parse_ifp(raw).anims if raw else [])]
        return out


    def _cast_kind(self, name: str) -> str: #vers 3
        """ped / vehicle / object / missing from the model's frames (IV: its file types)."""
        if name.lower() in self._cast_kinds:
            return self._cast_kinds[name.lower()]
        if self._cast_iv():
            f = self._cast_model_files(name)
            ped = '.wft' in f and ('.wdd' in f or name.lower() == 'player')      # Niko: parts in playerped.rpf
            kind = ('ped' if ped else 'vehicle' if '.wft' in f else
                    'object' if '.wdr' in f else 'missing')
            self._cast_kinds[name.lower()] = kind
            return kind
        raw = self._cs_fetch(name, '.dff')
        kind = 'missing'
        if raw is not None:
            from apps.methods.dff_skin import read_skeleton
            try:
                sk = read_skeleton(raw)
                names = [f['name'].strip().lower() for f in sk.frames]
            except (ValueError, IndexError, KeyError):
                names, sk = [], None
            if sk is not None and (sk.skins and len(names) > 16 or 'swaist' in names):
                kind = 'ped'
            elif any(n in ('chassis', 'chassis_hi') or n.endswith(('_ok', 'body')) or 'whl' in n or
                     n.startswith('wheel') for n in names):
                kind = 'vehicle'
            else:
                kind = 'object'
        self._cast_kinds[name.lower()] = kind
        return kind


    def _cast_fill(self): #vers 2
        """Table of the current cutscene's cast."""
        t = self._cast_table
        t.setRowCount(0)
        for model, anim, _i in self._cast_rows():
            hit = next((h for h in (self._cs_locate(model, e) for e in
                                    (('.wdd', '.wdr', '.wft') if self._cast_iv() else ('.dff',))) if h), None)
            where = ('-' if hit is None else os.path.basename(hit[1]) if hit[0] == 'file'
                     else os.path.basename(hit[0].file_path))
            sw = self._cast_swaps.get(model.lower())
            r = t.rowCount()
            t.insertRow(r)
            for c, v in enumerate((model, anim, self._cast_kind(model), where, sw['label'] if sw else '')):
                t.setItem(r, c, QTableWidgetItem(v))


    def _cast_current(self): #vers 1
        """(model, animation, .cut row) of the selected cast row, or None."""
        r = self._cast_table.currentRow()
        rows = self._cast_rows()
        if not (0 <= r < len(rows)):
            QMessageBox.information(self, self.App_name, "Pick a model in the Cast list")
            return None
        return rows[r]


    def _cs_cast_fetch(self, name: str, ext: str): #vers 2
        """Preview fetch: a stand-in's bytes for a swapped model, else the game's."""
        sw = getattr(self, '_cast_swaps', {}).get(name.lower())
        if sw is not None:
            return sw['files'].get(ext)
        return self._cs_fetch(name, ext)


    def _cast_model_names(self) -> list: #vers 2
        """Every model name in the game's model IMGs (DFF; IV .wdd / .wdr / .wft)."""
        names = set()
        exts = ('.wdd', '.wdr', '.wft') if self._cast_iv() else ('.dff',)
        for _path, arc in self._cs_model_arcs():
            names.update(e.name[:-4] for e in arc.entries if e.name.lower().endswith(exts))
        return sorted(names, key=str.lower)


    def _cast_pick_model(self, title: str, start: str = '') -> str: #vers 1
        """Filterable list of the game's models; '' when cancelled."""
        names = self._cast_model_names()
        dlg = QDialog(self)
        dlg.setWindowTitle(title)
        lay = QVBoxLayout(dlg)
        flt = QLineEdit(start)
        flt.setPlaceholderText("Filter models")
        lst = QListWidget()

        def fill(text=''):
            lst.clear()
            lst.addItems([n for n in names if text.lower() in n.lower()][:3000])
        flt.textChanged.connect(fill)
        fill(start)
        lst.doubleClicked.connect(dlg.accept)
        lay.addWidget(flt)
        lay.addWidget(lst, 1)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept)
        bb.rejected.connect(dlg.reject)
        lay.addWidget(bb)
        #dlg.resize(360, 480)
        if dlg.exec() != QDialog.DialogCode.Accepted or lst.currentItem() is None:
            return ''
        return lst.currentItem().text()

    def _cast_swap(self): #vers 2
        """Stand-in from the game's models for the selected part (preview)."""
        cur = self._cast_current()
        if cur is None:
            return
        pick = self._cast_pick_model(f"Stand-in for {cur[0]}")
        if not pick:
            return
        files = {e: d for e in (IV_EXTS if self._cast_iv() else MODEL_EXTS)
                 for d in [self._cs_fetch(pick, e)] if d is not None}
        if not files:
            return
        self._cast_swaps[cur[0].lower()] = {'label': pick, 'files': files}
        self._cast_refresh()

    def _cast_import(self): #vers 2
        """Stand-in from model files (files of the same name beside it come too)."""
        cur = self._cast_current()
        if cur is None:
            return
        exts = IV_EXTS if self._cast_iv() else MODEL_EXTS
        path, _ = QFileDialog.getOpenFileName(self, f"Model for {cur[0]}", "",
                                              "Models (" + " ".join('*' + e for e in exts) + ")")
        if not path:
            return
        stem = os.path.splitext(path)[0]
        files = {e: open(stem + e, 'rb').read() for e in exts if os.path.isfile(stem + e)}
        if not self._cast_iv() and '.txd' not in files:
            txd_path, _ = QFileDialog.getOpenFileName(self, "Its textures (Cancel: none)", os.path.dirname(path),
                                                      "Textures (*.txd)")
            if txd_path:
                files['.txd'] = open(txd_path, 'rb').read()
        self._cast_swaps[cur[0].lower()] = {'label': os.path.basename(stem), 'files': files}
        self._cast_refresh()

    def _cast_clear(self): #vers 1
        """Drop the selected part's stand-in."""
        cur = self._cast_current()
        if cur is not None and self._cast_swaps.pop(cur[0].lower(), None) is not None:
            self._cast_refresh()

    def _cast_refresh(self): #vers 1
        """Table and preview again after a cast change."""
        row = self._cast_table.currentRow()
        self._cast_fill()
        self._cast_table.selectRow(row)
        self._pv_for = None                                 # rebuild the preview scene
        self._cs_preview_load()

    def _cast_add(self): #vers 2
        """New model in the .cut, played by an existing animation (SA: IFP anim; IV: a cast member's)."""
        if self._cast_iv():
            rows = self._cast_rows()
            if not rows:
                return
            model = self._cast_pick_model("Model to add")
            if not model:
                return
            src, ok = QInputDialog.getItem(self, self.App_name, f"Play {model} with the animation of:",
                                           [r[0] for r in rows], 0, False)
            if not ok:
                return
            from apps.methods.cutscene_files import iv_cut_parts
            parts = iv_cut_parts(self._cs_sections)
            k = 0
            for i, (tag, srows) in enumerate(self._cs_sections):
                if tag != 'models' or k >= len(parts):
                    continue
                hit = next((m for m in parts[k]['models'] if m[1] == src), None)
                if hit is not None:
                    idx = max([int(m[0]) for m in parts[k]['models'] if m[0].isdigit()] + [-1]) + 1
                    srows.append(f"{idx} {model} {hit[2]}" + (f" {hit[3]}" if hit[3] else ""))
                k += 1
            self._cast_section_changed(next(i for i, (t, _r) in enumerate(self._cs_sections) if t == 'models'))
            return
        secs = getattr(self, '_cs_sections', [])
        idx = next((i for i, (n, _r) in enumerate(secs) if n == 'model'), None)
        if idx is None:
            QMessageBox.information(self, self.App_name, "III / VC cutscenes keep their cast in main.scm; "
                                    "swap a model instead")
            return
        from apps.methods.ifp_format import parse_ifp
        raw = self._cs_fetch(self._cs_cur, '.ifp')
        anims = [a.name for a in (parse_ifp(raw).anims if raw else [])]
        if not anims:
            return
        model = self._cast_pick_model("Model to add")
        if not model:
            return
        anim, ok = QInputDialog.getItem(self, self.App_name, f"Animation for {model}:", anims, 0, False)
        if not ok:
            return
        secs[idx][1].append(f"1,{model},{anim}")
        self._cast_section_changed(idx)


    def _cast_remove(self): #vers 2
        """Take the selected model out of the .cut (IV: every section)."""
        cur = self._cast_current()
        if cur is None:
            return
        if cur[2] is None:
            QMessageBox.information(self, self.App_name, "III / VC cutscenes keep their cast in main.scm")
            return
        if QMessageBox.question(self, self.App_name, f"Remove {cur[0]} from {self._cs_cur}.cut?") \
                != QMessageBox.StandardButton.Yes:
            return
        if self._cast_iv():
            first = None
            for i, (tag, rows) in enumerate(self._cs_sections):
                if tag == 'models':
                    rows[:] = [r for r in rows if len(r.split()) < 2 or r.split()[1] != cur[0]]
                    first = i if first is None else first
            idx = first
        else:
            idx = next(i for i, (n, _r) in enumerate(self._cs_sections) if n == 'model')
            del self._cs_sections[idx][1][cur[2]]
        self._cast_swaps.pop(cur[0].lower(), None)
        self._cast_section_changed(idx)


    def _cast_section_changed(self, idx: int): #vers 2
        """Model section edited: script panel, text, cast and preview follow; unsaved."""
        self._cs_loading = True
        for i, (tag, rows) in enumerate(self._cs_sections):
            self._cs_sec_combo.setItemText(i, f"{tag}  ({len(rows)})")
        self._cs_text.setPlainText(self._cs_cut_text().replace('\r\n', '\n'))
        self._cs_loading = False
        if self._cs_sec_combo.currentIndex() == idx:
            self._cs_show_section(idx)
        self._cs_dirty = True
        self._cast_refresh()


    def _cast_for_game(self, model: str, kind: str): #vers 2
        """({ext: bytes}, note, save name) of a model ready for in-game use; None when cancelled."""
        files = self._cast_model_files(model)
        if self._cast_iv():
            if not files:
                raise ValueError(f"{model}: no model files found")
            line = next((l for l in self._cs_game_file('data', 'peds.ide').splitlines()
                         if l.split(',')[0].strip().lower() == model.lower()), '')
            note = "GTA IV: cutscene peds are game models already" + (f"\n\npeds.ide:\n{line}" if line else "")
            return files, note, model
        dff, txd = files.get('.dff'), files.get('.txd')
        if dff is None:
            raise ValueError(f"{model}.dff not found")
        if kind == 'ped':
            from apps.methods.ped_cs_convert import convert_cutscene_ped, cutscene_ped_game
            from apps.methods.dff_skin import read_skeleton
            sk = read_skeleton(dff)
            name = re.sub(r'^cs_?', '', model) or model
            if not sk.skins:                                         # III: cutscenes use game peds
                return files, "III ped: already in the game format", model
            if not any(f['node_id'] >= 5000 for f in sk.frames):
                return files, "Already on the game skeleton", name
            out, note = convert_cutscene_ped(dff, cutscene_ped_game(dff))
            return {'.dff': out, '.txd': txd}, note, name
        if kind == 'vehicle':
            return self._cast_map_dialog(model, dff, txd)
        return files, "Object: saved as it is", model


    def _cast_map_dialog(self, model: str, dff: bytes, txd): #vers 2
        """Donor car and part map for a cutscene vehicle; ({ext: bytes}, note, donor) or None."""
        from apps.methods import veh_cs_convert as vc
        guess = re.sub(r'^cs_?|\d+$', '', model.lower())
        donor = self._cast_pick_model(f"Donor in-game vehicle for {model}", guess)
        if not donor:
            return None
        d_dff, d_txd = self._cs_fetch(donor, '.dff'), self._cs_fetch(donor, '.txd')
        cs_parts, d_parts = vc.part_names(dff), vc.part_names(d_dff)
        pmap = vc.guess_part_map(cs_parts, d_parts)
        dlg = QDialog(self)
        dlg.setWindowTitle(f"{model} onto {donor}")
        outer = QVBoxLayout(dlg)
        outer.addWidget(QLabel("Cutscene part for each donor part ('-' keeps the donor's mesh):"))
        area = QScrollArea()
        area.setWidgetResizable(True)
        inner = QWidget()
        form = QFormLayout(inner)
        combos = {}
        for d in d_parts:
            cb = QComboBox()
            cb.addItems(['-'] + cs_parts)
            cb.setCurrentText(pmap.get(d, '-'))
            form.addRow(d, cb)
            combos[d] = cb
        area.setWidget(inner)
        outer.addWidget(area, 1)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept)
        bb.rejected.connect(dlg.reject)
        outer.addWidget(bb)
        #dlg.resize(420, 520)
        if dlg.exec() != QDialog.DialogCode.Accepted:
            return None
        pmap = {d: cb.currentText() for d, cb in combos.items() if cb.currentText() != '-'}
        out, note, _kept = vc.convert_cutscene_vehicle(dff, d_dff, pmap)
        from apps.methods.rw_chunks import texture_names
        tex = vc.merge_txd(txd, d_txd, texture_names(out)) if txd else d_txd
        ide = ''
        data = self._cs_game_file('data', 'vehicles.ide') or self._cs_game_file('data', 'default.ide')
        if data:
            line = vc.find_ide_line(data, donor)
            ide = f"\n\nIDE (cars) of {donor}:\n{line}" if line else ''
        return {'.dff': out, '.txd': tex}, f"Donor {donor}: {note}{ide}", donor

    def _cs_game_file(self, folder: str, name: str) -> str: #vers 2
        """Text of <game>/<folder>/<name> beside the cutscenes, or ''."""
        base = os.path.dirname(self._cs_img.file_path) if self._cs_img is not None else self._cs_dir
        root = os.path.dirname(base or '')
        if self._cast_iv():                                          # IV: <game>/common/data
            root = os.path.join(os.path.dirname(root), 'common')
        d = next((os.path.join(root, x) for x in (os.listdir(root) if os.path.isdir(root) else [])
                  if x.lower() == folder), None)
        f = next((os.path.join(d, x) for x in os.listdir(d) if x.lower() == name), None) if d else None
        if not f:
            return ''
        with open(f, errors='replace') as fh:
            return fh.read()

    def _cast_export(self): #vers 2
        """Save the selected model's (or stand-in's) files unchanged."""
        cur = self._cast_current()
        if cur is None:
            return
        folder = QFileDialog.getExistingDirectory(self, f"Export {cur[0]} to")
        if not folder:
            return
        done = []
        for ext, data in self._cast_model_files(cur[0]).items():
            with open(os.path.join(folder, cur[0] + ext), 'wb') as f:
                f.write(data)
            done.append(cur[0] + ext)
        self._set_status(f"Exported {', '.join(done)} to {folder}")

    def _cast_export_game(self, folder: str = '') -> list: #vers 2
        """Save the selected model converted for in-game use; paths written."""
        cur = self._cast_current()
        if cur is None:
            return []
        kind = self._cast_kind(cur[0])
        try:
            res = self._cast_for_game(cur[0], kind)
        except ValueError as e:
            QMessageBox.warning(self, self.App_name, str(e))
            return []
        if res is None:
            return []
        files, note, name = res
        if not folder:
            folder = QFileDialog.getExistingDirectory(self, f"Save {cur[0]} for the game to")
            if not folder:
                return []
        name, ok = QInputDialog.getText(self, self.App_name, "Save as (model name):", text=name)
        if not ok or not name.strip():
            return []
        paths = []
        for ext, data in files.items():
            if data:
                p = os.path.join(folder, name.strip() + ext)
                with open(p, 'wb') as f:
                    f.write(data)
                paths.append(p)
        QMessageBox.information(self, self.App_name, f"Saved {', '.join(os.path.basename(p) for p in paths)}"
                                f"\n\n{note}")
        return paths

    def _cast_send(self): #vers 3
        """Open the converted model in its workshop (inside IMG Factory)."""
        cur = self._cast_current()
        if cur is None:
            return
        kind = self._cast_kind(cur[0])
        mod, title = WORKSHOPS.get('object' if self._cast_iv() else kind, WORKSHOPS['object'])
        if importlib.util.find_spec(mod) is None:
            QMessageBox.information(self, self.App_name, f"{title} isn't in this install - use For game, "
                                    "then open the files in it")
            return
        import tempfile
        paths = self._cast_export_game(tempfile.mkdtemp(prefix='cutscene_'))   # name asked there
        dff = next((p for p in paths if p.endswith(('.dff', '.wdr', '.wft', '.wdd'))), None)
        if dff is None:
            self._set_status(f"{cur[0]}: not sent (conversion cancelled or no donor)")
            return
        if self._cast_iv():
            kind = 'object'                                          # IV models open in Model Workshop
            dff = next((p for p in paths if p.endswith(('.wdr', '.wft'))), dff)
        mw = getattr(self, 'main_window', None)
        if kind == 'ped':
            from apps.components.Ped_Workshop.ped_workshop import open_ped_workshop
            ws = open_ped_workshop(mw)
            self._sent_ws = ws                                       # kept so the window stays open
            ws.open_dff_file(dff)
            txd = next((p for p in paths if p.endswith('.txd')), None)
            if txd:
                with open(txd, 'rb') as f:
                    ws._load_txd_file_from_data(f.read(), os.path.basename(txd))
        elif kind == 'vehicle':
            from apps.components.Vehicle_Workshop.vehicle_workshop import open_vehicle_workshop
            self._sent_ws = open_vehicle_workshop(mw, dff)          # kept so the window stays open
        else:
            from apps.components.Model_Editor.model_workshop import open_model_workshop
            self._sent_ws = open_model_workshop(mw, dff)
        self._set_status(f"{cur[0]}: sent to the {title}")

    def _cast_apply(self): #vers 4
        """Write each stand-in into the IMG holding the original (originals backed up)."""
        if not self._cast_swaps:
            QMessageBox.information(self, self.App_name, "No stand-ins to write")
            return
        jobs, skipped = [], []
        for model, sw in self._cast_swaps.items():
            hit = next((h for h in (self._cs_locate(model, e) for e in sw['files']) if h), None)
            if hit is None or hit[0] == 'file' or (self._cast_iv() and model == 'player'):   # csplay, Niko
                skipped.append(model)
                continue
            jobs.append((model, sw, hit[0]))
        if not jobs:
            QMessageBox.information(self, self.App_name, "Nothing to write: " + ", ".join(skipped) +
                                    " not in a game IMG (the game builds it elsewhere)")
            return
        shared = [m for m, _s, a in jobs if not a.file_path.lower().endswith(('cutscene.img', 'cutsprops.img'))]
        msg = "\n".join(f"{m} <- {s['label']}  ({os.path.basename(a.file_path)})" for m, s, a in jobs)
        if skipped:
            msg += "\n\nNot written (not in a game IMG): " + ", ".join(skipped)
        if shared:
            msg += ("\n\nThese are also the in-game models: " + ", ".join(shared) +
                    " will change outside cutscenes too.")
        if QMessageBox.question(self, self.App_name, f"Write into the game?\n\n{msg}\n\n"
                                "Originals are copied to cutscene_backup/ beside the IMG first.") \
                != QMessageBox.StandardButton.Yes:
            return
        for model, sw, arc in jobs:
            bak = os.path.join(os.path.dirname(arc.file_path), 'cutscene_backup')
            os.makedirs(bak, exist_ok=True)
            for ext in sw['files']:
                e = next((x for x in arc.entries if x.name.lower() == model + ext), None)
                if e is not None and not os.path.exists(os.path.join(bak, e.name)):
                    with open(os.path.join(bak, e.name), 'wb') as f:
                        f.write(arc.read_entry_data(e))
            for ext, data in sw['files'].items():
                if data:
                    e = next((x for x in arc.entries if x.name.lower() == model + ext), None)
                    arc.add_entry(e.name if e else model + ext, data)
        self._cast_kinds.clear()
        self._set_status(f"Wrote {len(jobs)} stand-ins; originals in cutscene_backup/")
