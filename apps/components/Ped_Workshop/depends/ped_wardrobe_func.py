#this belongs in apps/components/Ped_Workshop/depends/ped_wardrobe_func.py - Version: 1
# X-Seti - November 02 2026 - IMG Factory 1.6 - Ped Workshop CJ wardrobe

"""
CJ wardrobe (SA): pick each clothes component from shopping.dat items,
clothes.dat rules applied, parts from models/player.img merged into one
textured DFF + TXD, shown in the ped preview; Export saves the pair.
"""

##Methods list -
#
##class PedWardrobeMixin: -
# _cj_build
# _cj_export
# _cj_player_bytes
# _cj_wardrobe

import os

from PyQt6.QtWidgets import (QCheckBox, QComboBox, QDialog, QFormLayout, QHBoxLayout, QMessageBox,
                             QPushButton, QVBoxLayout)

_COMPS = ('head', 'torso', 'legs', 'feet', 'hat', 'glasses', 'necklace', 'watch', 'special')


class PedWardrobeMixin: #vers 1
    """CJ outfit builder for San Andreas."""

    def _cj_player_bytes(self, name: str): #vers 1
        """Entry bytes from the game's models/player.img, or None."""
        root = getattr(self, '_ped_root', '') or ''
        arc = getattr(self, '_cj_img', None)
        if arc is None:
            models = next((os.path.join(root, d) for d in (os.listdir(root) if os.path.isdir(root) else [])
                           if d.lower() == 'models'), None)
            hit = next((f for f in os.listdir(models) if f.lower() == 'player.img'), None) if models else None
            if not hit:
                return None
            from apps.methods.img_core_classes import IMGFile
            arc = IMGFile(os.path.join(models, hit))
            if not arc.open():
                return None
            self._cj_img = arc
        e = next((x for x in arc.entries if x.name.lower() == name.lower()), None)
        return arc.read_entry_data(e) if e is not None else None

    def _cj_wardrobe(self): #vers 1
        """Wardrobe dialog: one list per component, Apply shows CJ, Export saves DFF + TXD."""
        from apps.methods.cj_player import DEFAULT_OUTFIT, parse_clothes_dat, parse_shopping_clothes
        data = getattr(self, '_ped_paths', {}).get('data_files') or {}
        root = getattr(self, '_ped_root', '') or ''
        if self._cj_player_bytes('head.dff') is None:
            QMessageBox.information(self, "CJ wardrobe", "Open a San Andreas game folder first (needs models/player.img).")
            return

        def read(name):
            p = next((v for k, v in data.items() if k.lower() == name), None) if isinstance(data, dict) else None
            p = p or os.path.join(root, 'data', name)
            if not os.path.isfile(p):
                return ''
            with open(p, encoding='latin-1') as f:
                return f.read()

        self._cj_rules = parse_clothes_dat(read('clothes.dat'))
        items = parse_shopping_clothes(read('shopping.dat'))
        dlg = QDialog(self)
        dlg.setWindowTitle("CJ wardrobe")
        lay = QVBoxLayout(dlg)
        form = QFormLayout()
        self._cj_combos = {}
        for comp in _COMPS:
            cb = QComboBox()
            cb.addItem("(none)", ('', ''))
            dm = DEFAULT_OUTFIT.get(comp)
            if dm:
                cb.addItem(f"default: {dm[0]} / {dm[1]}", dm)
            for it in sorted((i for i in items if i['component'] == comp), key=lambda i: i['model']):
                cb.addItem(f"{it['model']} / {it['texture']}  ({it['name']}, ${it['price']})",
                           (it['model'], it['texture']))
            cb.setCurrentIndex(1 if dm else 0)
            form.addRow(comp.capitalize() + ":", cb)
            self._cj_combos[comp] = cb
        lay.addLayout(form)
        self._cj_cut = QCheckBox("Cutscene parts (clothes.dat CUTS)")
        lay.addWidget(self._cj_cut)
        row = QHBoxLayout()
        for text, slot in (("Apply", self._cj_build), ("Export DFF + TXD...", self._cj_export), ("Close", dlg.close)):
            b = QPushButton(text)
            b.clicked.connect(slot)
            row.addWidget(b)
        lay.addLayout(row)
        dlg.resize(560, 420)
        dlg.show()
        self._cj_dialog = dlg

    def _cj_build(self, show: bool = True): #vers 1
        """Resolve the outfit, merge the parts; show it in the preview. Returns (dff, txd, parts)."""
        import tempfile
        from apps.methods.cj_player import DEFAULT_OUTFIT, merge_parts, merge_txds, resolve_outfit, texture_part
        from apps.methods.txd_parser import parse_txd
        outfit = {c: cb.currentData() for c, cb in self._cj_combos.items() if cb.currentData() and cb.currentData()[0]}
        outfit['hands'] = DEFAULT_OUTFIT['hands']
        parts, dffs, txds, missing = resolve_outfit(outfit, self._cj_rules, self._cj_cut.isChecked()), [], [], []
        for comp, model, tex in parts:
            raw = self._cj_player_bytes(model + '.dff')
            if raw is None:
                missing.append(model)
                continue
            t = self._cj_player_bytes(tex + '.txd') if tex else None
            names = [x['name'] for x in parse_txd(t)] if t else []
            dffs.append(texture_part(raw, names[0]) if names else raw)
            if t:
                txds.append(t)
        if not dffs:
            QMessageBox.warning(self, "CJ wardrobe", "No parts found in player.img")
            return None
        dff, txd = merge_parts(dffs), (merge_txds(txds) if txds else None)
        if show:
            path = os.path.join(tempfile.mkdtemp(prefix='cj_'), 'player.dff')
            with open(path, 'wb') as f:
                f.write(dff)
            self._original_dff_name = 'player.dff'
            self.open_dff_file(path)
            if txd:
                self._load_txd_file_from_data(txd, 'player.txd')
            self._set_status("CJ: " + ", ".join(m for _c, m, _t in parts)
                             + (f"   (missing: {', '.join(missing)})" if missing else ""))
        return dff, txd, parts

    def _cj_export(self): #vers 1
        """Save the current outfit as one DFF and one TXD."""
        from PyQt6.QtWidgets import QFileDialog
        built = self._cj_build(show=False)
        if not built:
            return
        dff, txd, _parts = built
        dst, _ = QFileDialog.getSaveFileName(self, "Export CJ outfit", "player.dff", "DFF (*.dff)")
        if not dst:
            return
        from apps.methods.file_backup import safe_write_bytes
        safe_write_bytes(dst, dff)
        if txd:
            safe_write_bytes(os.path.splitext(dst)[0] + '.txd', txd)
        self._set_status(f"Exported {os.path.basename(dst)}" + (" + .txd" if txd else ""))
