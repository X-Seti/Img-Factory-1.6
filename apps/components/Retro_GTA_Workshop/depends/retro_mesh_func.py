#this belongs in apps/components/Retro_GTA_Workshop/depends/retro_mesh_func.py - Version: 2
# X-Seti - October 24 2026 - IMG Factory 1.6 - Retro GTA Workshop 3D export

"""
File ribbon: Export 3D - the open map as OBJ (whole map) plus sector
DFF + COL models, IDE / IPL placements and a TXD for Map Workshop.
"""

##Methods list -
#
##class RetroMeshMixin: -
# _export_3d

import os

from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import QFileDialog, QInputDialog, QMessageBox, QProgressDialog

from apps.methods.retro_mesh import export_map_3d


class RetroMeshMixin: #vers 1
    """3D export of the Retro GTA Workshop."""

    def _export_3d(self): #vers 2
        """Pick folder, DFF version, scale and area; write the map's 3D files."""
        from apps.methods.model_convert import TARGETS
        if self._map is None or self._style is None:
            self._set_status("Open a map (and its style) first")
            return
        folder = QFileDialog.getExistingDirectory(self, "Export map as 3D (OBJ, DFF + COL) to folder",
                                                  self._data_dir or '')
        if not folder:
            return
        dffs = [lab for lab, (kind, _v) in TARGETS.items() if kind == 'dff']
        label, ok = QInputDialog.getItem(self, "Export 3D", "Sector DFF version:", dffs, 0, False)
        if not ok:
            return
        scale, ok = QInputDialog.getDouble(self, "Export 3D", "Units per block:", 4.0, 0.5, 64.0, 1)
        if not ok:
            return
        v = self._view
        vis = (max(0, int(-v.ox // v.scale)), max(0, int(-v.oy // v.scale)),
               min(255, int((v.width() - v.ox) // v.scale)), min(255, int((v.height() - v.oy) // v.scale)))
        text, ok = QInputDialog.getText(self, "Export 3D", "Blocks x0, y0, x1, y1 (default: the visible area):",
                                        text=", ".join(map(str, vis)))
        if not ok:
            return
        try:
            area = tuple(max(0, min(255, int(p))) for p in text.replace(' ', '').split(','))
            if len(area) != 4 or area[0] > area[2] or area[1] > area[3]:
                raise ValueError
        except ValueError:
            QMessageBox.warning(self, self.App_name, "Area must be x0, y0, x1, y1 (0-255)")
            return
        stem = os.path.splitext(os.path.basename(self._map.path))[0].lower()
        out = os.path.join(folder, stem + "_3d")
        dlg = QProgressDialog("Building sectors...", "Cancel", 0, 256, self)
        dlg.setWindowModality(Qt.WindowModality.WindowModal)

        def progress(k, n):
            dlg.setMaximum(n)
            dlg.setValue(k)
            return not dlg.wasCanceled()
        try:
            report = export_map_3d(self._map, self._style, out, stem, scale, label, progress=progress, area=area)
        except (OSError, ValueError, InterruptedError) as ex:
            dlg.close()
            QMessageBox.warning(self, self.App_name, str(ex))
            return
        dlg.close()
        self._set_status(f"3D export to {out}: " + "; ".join(report))
