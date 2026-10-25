#this belongs in apps/components/Ped_Workshop/depends/ped_iv_func.py - Version: 2
# X-Seti - October 23 2026 - IMG Factory 1.6 - Ped Workshop GTA IV component peds

"""
GTA IV peds from componentpeds.img: the .wdd components (head, uppr, lowr,
suse, hair ...) are put together into one model with the .wtd textures,
skinned to the .wft skeleton so .wad anims play.
Components tab: pick each slot's drawable and texture variation.
"""

##class PedIVMixin: -
# _create_iv_tab
# _iv_animator
# _iv_build
# _iv_entry_bytes
# _iv_load_ped
# _iv_save
# _iv_slot_changed

import os
import re
import tempfile

from PyQt6.QtWidgets import QComboBox, QFormLayout, QHBoxLayout, QLabel, QMessageBox, QPushButton, QWidget

from apps.methods.dff_classes import Atomic, DFFModel, Frame
from apps.methods.ped_files import IV_SLOTS, iv_component_names, read_ped_variations


class PedIVMixin: #vers 1
    """GTA IV component peds for PedWorkshop."""

    def _create_iv_tab(self): #vers 2
        """Components tab (GTA IV): one drawable + texture combo row per slot."""
        page = QWidget()
        self._iv_form = QFormLayout(page)
        self._iv_form.setContentsMargins(4, 4, 4, 4)
        self._iv_info = QLabel("GTA IV: pick a ped in the Peds tab")
        save = QPushButton("Save to componentpeds.img")
        save.setToolTip("Write changed meshes (.wdd) and textures (.wtd) back (entry backups first)")
        save.clicked.connect(lambda _c=False: self._iv_save())
        top = QHBoxLayout()
        top.addWidget(self._iv_info, 1)
        top.addWidget(save)
        self._iv_form.addRow(top)
        self._iv_rows = {}                                  # slot -> (geometry combo, texture combo)
        self._iv_wdd = None
        return page

    def _iv_animator(self, model): #vers 1
        """Skeleton from the ped .wft, skins for the meshes: bones and .wad anims play."""
        from apps.methods.iv_anim import iv_skeleton, iv_skins
        wft = self._iv_entry_bytes(self._iv_ped + '.wft')
        if wft is None:
            self._animator = None
            self._bone_list.clear()
            self._set_status(f"Ped: {self._iv_ped}.wft not in componentpeds.img - no skeleton")
            return
        sk = iv_skeleton(wft)
        iv_skins(sk, model.geometries)
        self._init_animator(None, sk)

    def _iv_entry_bytes(self, name: str): #vers 1
        """Bytes of an entry in componentpeds.img, or None."""
        path = self._ped_paths.get('components')
        if not path:
            return None
        arc = getattr(self, '_iv_img', None)
        if arc is None or arc.file_path != path:
            from apps.methods.img_core_classes import IMGFile
            arc = IMGFile(path)
            if not arc.open():
                return None
            self._iv_img = arc
        e = next((x for x in arc.entries if x.name.lower() == name.lower()), None)
        return arc.read_entry_data(e) if e is not None else None

    def _iv_load_ped(self, ped: str): #vers 2
        """Read a ped's .wdd / .wtd, fill the Components tab, show it."""
        from apps.methods.wdr_model import wdd_hashes
        wdd = self._iv_entry_bytes(ped + '.wdd')
        if wdd is None:
            self._set_status(f"Ped: {ped}.wdd not in componentpeds.img")
            return
        wtd = self._iv_entry_bytes(ped + '.wtd')
        self._iv_ped, self._iv_wdd = ped, wdd
        names = iv_component_names(wdd_hashes(wdd))
        self._iv_hash_of = {n: h for h, n in names.items()}
        self._iv_tex_names, self._iv_wtd_path, self._iv_wtd = [], None, wtd
        self._iv_tex_orig = {}
        if wtd is not None:
            from apps.methods.xtd_textures import parse_iv_wtd
            texs = parse_iv_wtd(wtd)
            self._iv_tex_names = [t.get('name', '').lower() for t in texs]
            self._iv_tex_orig = {t.get('name', '').lower(): bytes(t['rgba_data']) for t in texs}
            path = os.path.join(tempfile.mkdtemp(prefix='ped_'), ped + '.wtd')
            with open(path, 'wb') as f:
                f.write(wtd)
            self._iv_wtd_path = path
        var_path = self._ped_paths.get('variations')
        default = {}
        if var_path:
            for slot, geo in read_ped_variations(var_path).get(ped.lower(), []):
                default.setdefault(slot, geo)
        while self._iv_form.rowCount() > 1:                 # rebuild slot rows
            self._iv_form.removeRow(1)
        self._iv_rows = {}
        for slot in IV_SLOTS:
            comps = sorted(n for n in names.values() if n.startswith(slot + '_'))
            if not comps:
                continue
            geo = QComboBox()
            geo.addItem("none", '')
            for n in comps:
                geo.addItem(n, n)
            want = next((n for n in comps if int(n[5:8]) == default.get(slot, -1)), comps[0])
            geo.setCurrentIndex(geo.findData(want))
            tex = QComboBox()
            tex.setToolTip("Texture variation")
            row = QHBoxLayout()
            row.addWidget(geo, 1)
            row.addWidget(tex)
            self._iv_rows[slot] = (geo, tex)
            self._iv_slot_changed(slot, build=False)
            geo.currentIndexChanged.connect(lambda _i, s=slot: self._iv_slot_changed(s))
            tex.currentIndexChanged.connect(lambda _i: self._iv_build())
            self._iv_form.addRow(slot, row)
        self._iv_info.setText(f"{ped}: {len(names)} components, {len(self._iv_tex_names)} textures")
        self._iv_build(textures=True)

    def _iv_save(self): #vers 1
        """Changed meshes into <ped>.wdd and changed textures into <ped>.wtd in componentpeds.img."""
        from apps.methods.path_nodes import write_back
        from apps.methods.wdr_model import write_wdr_model
        from apps.methods.xtd_textures import parse_iv_wtd, write_iv_wtd
        img = self._ped_paths.get('components')
        model = getattr(self, '_current_dff_model', None)
        if not img or getattr(self, '_iv_wdd', None) is None or model is None:
            self._set_status("Save IV ped: no GTA IV ped open")
            return
        done = []
        try:
            wdd, report = write_wdr_model(self._iv_wdd, model, wdd=True)
            if wdd is not self._iv_wdd:
                write_back(f"{img}:{self._iv_ped}.wdd", lambda _old: wdd)
                self._iv_wdd = wdd
                done.append(f"{self._iv_ped}.wdd ({len([r for r in report if r.startswith('geometry')])} meshes)")
            if self._iv_wtd is not None:
                cur = {t.get('name', '').lower(): t for t in (getattr(self, '_mod_textures', None) or [])}
                edits = []
                for t in parse_iv_wtd(self._iv_wtd):
                    n = t.get('name', '').lower()
                    c = cur.get(n)
                    changed = c is not None and c.get('rgba_data') and bytes(c['rgba_data']) != self._iv_tex_orig.get(n)
                    edits.append(c if changed else None)
                if any(e is not None for e in edits):
                    wtd = write_iv_wtd(self._iv_wtd, edits)
                    write_back(f"{img}:{self._iv_ped}.wtd", lambda _old: wtd)
                    self._iv_wtd = wtd
                    self._iv_tex_orig.update({e['name'].lower(): bytes(e['rgba_data']) for e in edits if e})
                    done.append(f"{self._iv_ped}.wtd ({sum(e is not None for e in edits)} textures)")
        except Exception as ex:
            QMessageBox.warning(self, "Save IV ped", str(ex))
            return
        self._iv_img = None                                 # reopen: entries moved
        self._set_status("Saved " + ", ".join(done) if done else "Save IV ped: nothing changed")

    def _iv_slot_changed(self, slot: str, build: bool = True): #vers 1
        """Slot drawable picked: list its texture variations (a, b ... / race)."""
        geo, tex = self._iv_rows[slot]
        n = geo.currentData() or ''
        tex.blockSignals(True)
        tex.clear()
        if n:
            pre = f"{slot}_diff_{n[5:8]}_"
            for t in sorted(x for x in self._iv_tex_names if x.startswith(pre)):
                tex.addItem(t[len(pre):], t)
        tex.setEnabled(tex.count() > 1)
        tex.blockSignals(False)
        if build:
            self._iv_build()

    def _iv_build(self, textures: bool = False): #vers 2
        """Put the picked components together and show them (textures: load the WTD)."""
        from apps.components.Model_Editor.model_workshop import ModelWorkshop
        from apps.methods.wdr_model import parse_wdd
        model = DFFModel(source_path=self._iv_ped + '.wdd')
        for slot, (geo, tex) in self._iv_rows.items():
            n = geo.currentData()
            if not n:
                continue
            m = parse_wdd(self._iv_wdd, self._iv_hash_of[n], n)
            pick = tex.currentData()
            for g in (m.geometries if m else []):
                for mat in g.materials:
                    if pick and re.match(rf"{slot}_diff_\d{{3}}_", (mat.texture_name or '').lower()):
                        mat.texture_name = pick                # chosen variation
                model.geometries.append(g)
        if not model.geometries:
            self._set_status(f"Ped: {self._iv_ped} - no components picked")
            return
        model.frames = [Frame(name=self._iv_ped)]            # one root frame: all meshes drawn
        model.atomics = [Atomic(0, i) for i in range(len(model.geometries))]
        ModelWorkshop._show_dff(self, os.path.join(tempfile.gettempdir(), self._iv_ped + '.wdd'), model)
        self._iv_animator(model)
        if textures and getattr(self, '_iv_wtd_path', None):
            self._load_txd_file(self._iv_wtd_path)
            if self._animator is not None:
                self._anim_render()                           # posed mesh with the textures
        self._set_status(f"Ped: {self._iv_ped} (GTA IV, {len(model.geometries)} meshes)")
