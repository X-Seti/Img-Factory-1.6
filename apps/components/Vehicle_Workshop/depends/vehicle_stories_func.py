#this belongs in apps/components/Vehicle_Workshop/depends/vehicle_stories_func.py - Version: 1
# X-Seti - November 07 2026 - IMG Factory 1.6 - Vehicle Workshop LCS/VCS extract

"""
Extract LCS / VCS (PS2 / PSP) vehicles: pick the game's GTA3PS2.IMG, tick
cars, choose GTA III / VC / SA and a folder; each car is written as a DFF
and TXD (apps/methods/stories_vehicle.py). One car opens in the workshop.
"""

##class VehicleStoriesMixin: -
# _stories_extract
# _stories_list

import os

from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import (QComboBox, QDialog, QDialogButtonBox, QFileDialog, QHBoxLayout, QLabel,
                             QLineEdit, QListWidget, QListWidgetItem, QMessageBox, QPushButton,
                             QVBoxLayout)

_GAMES = (("GTA Vice City", 'vc'), ("GTA III", 'iii'), ("GTA San Andreas", 'sa'))


class VehicleStoriesMixin: #vers 1
    """LCS / VCS vehicle extraction for Vehicle Workshop."""

    def _stories_list(self, img_path: str): #vers 1
        """(IMGFile, dtz bytes, [(model, txd)] cars) for a Stories GTA3PS2.IMG; raises ValueError."""
        from apps.methods.img_core_classes import IMGFile
        from apps.methods.stories_dtz import StoriesDTZ, dtz_paths, find_stories_root
        lay = dtz_paths(find_stories_root(img_path) or os.path.dirname(img_path))
        if not lay:
            raise ValueError("GAME.DTZ not found beside this IMG - pick the LCS / VCS GTA3PS2.IMG")
        game, plat, dtz, _img = lay
        z = StoriesDTZ(dtz, game, plat)
        arc = IMGFile(img_path)
        if not arc.open():
            raise ValueError(f"{os.path.basename(img_path)}: not readable")
        names = {e.name.lower() for e in arc.entries}
        cars = sorted((o['name'], o.get('txd') or o['name']) for o in z.objects()
                      if o.get('section') == 'cars' and o['name'].lower() + '.mdl' in names)
        return arc, z.data, cars

    def _stories_extract(self): #vers 1
        """Dialog: Stories IMG, cars, target game, folder; writes DFF + TXD per car."""
        from apps.methods.stories_vehicle import export_vehicle
        dlg = QDialog(self)
        dlg.setWindowTitle("Extract LCS / VCS vehicles")
        dlg.resize(460, 560)
        lay = QVBoxLayout(dlg)
        row = QHBoxLayout()
        src = QLineEdit(self.WS.get('stories_img', '') if hasattr(self, 'WS') else '')
        src.setPlaceholderText("LCS / VCS GTA3PS2.IMG (GAME.DTZ next to it)")
        pick = QPushButton("Browse")
        row.addWidget(src, 1)
        row.addWidget(pick)
        lay.addLayout(row)
        cars = QListWidget()
        lay.addWidget(cars, 1)
        info = QLabel("")
        lay.addWidget(info)
        opts = QHBoxLayout()
        game = QComboBox()
        for label, key in _GAMES:
            game.addItem(label, key)
        opts.addWidget(QLabel("For"))
        opts.addWidget(game)
        allb = QPushButton("Tick all")
        opts.addStretch(1)
        opts.addWidget(allb)
        lay.addLayout(opts)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Cancel)
        go = bb.addButton("Extract to folder...", QDialogButtonBox.ButtonRole.AcceptRole)
        lay.addWidget(bb)
        state = {}

        def _load(path):  #vers 1
            cars.clear()
            try:
                state['arc'], state['dtz'], found = self._stories_list(path)
            except (ValueError, OSError) as e:
                info.setText(str(e))
                return
            for model, txd in found:
                it = QListWidgetItem(model)
                it.setData(Qt.ItemDataRole.UserRole, txd)
                it.setFlags(it.flags() | Qt.ItemFlag.ItemIsUserCheckable)
                it.setCheckState(Qt.CheckState.Unchecked)
                cars.addItem(it)
            info.setText(f"{len(found)} vehicles")
            if hasattr(self, 'WS'):
                self.WS.set('stories_img', path)
                self.WS.save()

        def _browse():  #vers 1
            p, _ = QFileDialog.getOpenFileName(dlg, "LCS / VCS GTA3PS2.IMG", src.text(), "IMG (*.img *.IMG)")
            if p:
                src.setText(p)
                _load(p)

        pick.clicked.connect(_browse)
        allb.clicked.connect(lambda: [cars.item(i).setCheckState(Qt.CheckState.Checked)
                                      for i in range(cars.count())])
        bb.rejected.connect(dlg.reject)
        go.clicked.connect(dlg.accept)
        if src.text() and os.path.isfile(src.text()):
            _load(src.text())
        if dlg.exec() != QDialog.DialogCode.Accepted or 'arc' not in state:
            return
        picked = [(cars.item(i).text(), cars.item(i).data(Qt.ItemDataRole.UserRole))
                  for i in range(cars.count()) if cars.item(i).checkState() == Qt.CheckState.Checked]
        if not picked:
            return
        out = QFileDialog.getExistingDirectory(self, "Write DFF / TXD to", getattr(self, "_last_dir", ""))
        if not out:
            return
        arc, dtz = state['arc'], state['dtz']
        ents = {e.name.lower(): e for e in arc.entries}
        tex_ext = '.xtx' if any(n.endswith('.xtx') for n in ents) else '.chk'
        report = []
        for model, txd in picked:
            mdl = arc.read_entry_data(ents[model.lower() + '.mdl'])
            te = ents.get(txd.lower() + tex_ext)
            try:
                report += export_vehicle(mdl, arc.read_entry_data(te) if te else b'', dtz, model, out,
                                         game.currentData())
            except (ValueError, OSError) as e:
                report.append(f"{model}: {e}")
        if len(picked) == 1 and os.path.isfile(os.path.join(out, picked[0][0] + '.dff')):
            self.load_dff(os.path.join(out, picked[0][0] + '.dff'))
            if os.path.isfile(os.path.join(out, picked[0][0] + '.txd')):
                self.load_txd(os.path.join(out, picked[0][0] + '.txd'))
        QMessageBox.information(self, "LCS / VCS vehicles", "\n".join(report[:40]))
