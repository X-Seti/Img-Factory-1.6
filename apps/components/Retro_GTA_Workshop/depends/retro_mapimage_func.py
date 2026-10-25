#this belongs in apps/components/Retro_GTA_Workshop/depends/retro_mapimage_func.py - Version: 2
# X-Seti - October 24 2026 - IMG Factory 1.6 - Retro GTA Workshop map PNG

"""
Map right-click menu: snapshot of the view, the whole map (or the visible
area) as one PNG, and an edited PNG imported back as lids.
"""

##Methods list -
#
##class RetroMapImageMixin: -
# _map_area_px
# _map_export_png
# _map_import_png
# _map_menu
# _map_snapshot

import os

import numpy as np
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QImage
from PyQt6.QtWidgets import QApplication, QFileDialog, QInputDialog, QMenu, QMessageBox

from apps.methods.retro_mapimage import import_map_image, map_image
from apps.methods.retro_palette import png_rgba


class RetroMapImageMixin: #vers 1
    """Map snapshot / PNG export / PNG import."""

    def _map_menu(self, pos): #vers 1
        menu = QMenu(self)
        menu.addAction("Snapshot view to PNG...", self._map_snapshot)
        menu.addAction("Export map to one PNG...", self._map_export_png)
        menu.addAction("Import map PNG (lids)...", self._map_import_png)
        menu.exec(pos)

    def _map_snapshot(self): #vers 1
        path, _ = QFileDialog.getSaveFileName(self, "Snapshot", os.path.join(self._data_dir or '', "snapshot.png"),
                                              "PNG images (*.png)")
        if path and self._view.grab().save(path, 'PNG'):
            self._set_status(f"Snapshot saved: {path}")

    def _map_area_px(self, title: str): #vers 1
        """(area, px per block) asked: whole map or the visible area; 8 / 16 / 32 / 64 px."""
        v = self._view
        vis = (max(0, int(-v.ox // v.scale)), max(0, int(-v.oy // v.scale)),
               min(255, int((v.width() - v.ox) // v.scale)), min(255, int((v.height() - v.oy) // v.scale)))
        which, ok = QInputDialog.getItem(self, title, "Area:", ["Whole map", f"Visible area {vis}"], 0, False)
        if not ok:
            return None
        area = (0, 0, 255, 255) if which == "Whole map" else vis
        cells = (area[2] - area[0] + 1) * (area[3] - area[1] + 1)
        sizes = [p for p in (8, 16, 32, 64) if cells * p * p <= 16384 * 16384]
        px, ok = QInputDialog.getItem(self, title, "Pixels per block:", [str(p) for p in sizes],
                                      min(1, len(sizes) - 1), False)
        return (area, int(px)) if ok else None

    def _map_export_png(self): #vers 1
        """The map's lids as one PNG (Copy combined map tiles)."""
        if self._map is None or self._style is None:
            return
        got = self._map_area_px("Export map PNG")
        if not got:
            return
        area, px = got
        stem = os.path.splitext(os.path.basename(self._map.path))[0]
        path, _ = QFileDialog.getSaveFileName(self, "Export map PNG",
                                              os.path.join(self._data_dir or '', f"{stem}_{px}px.png"), "PNG images (*.png)")
        if not path:
            return
        QApplication.setOverrideCursor(Qt.CursorShape.WaitCursor)
        try:
            a = np.ascontiguousarray(map_image(self._map, self._style, px, area))
            ok = QImage(a.data, a.shape[1], a.shape[0], a.shape[1] * 3, QImage.Format.Format_RGB888).save(path, 'PNG')
        finally:
            QApplication.restoreOverrideCursor()
        self._set_status(f"Map PNG {a.shape[1]} x {a.shape[0]} saved: {path}" if ok else "PNG not written")

    def _map_import_png(self): #vers 2
        """An edited map PNG back: changed blocks get the closest lid tile (rotation / flip)."""
        if self._map is None or self._style is None:
            return
        path, _ = QFileDialog.getOpenFileName(self, "Import map PNG", self._data_dir or '', "PNG images (*.png)")
        if not path:
            return
        got = self._map_area_px("Import map PNG - area and size it was exported with")
        if not got:
            return
        area, px = got
        try:
            a = png_rgba(path)
        except ValueError as ex:
            QMessageBox.warning(self, self.App_name, str(ex))
            return
        QApplication.setOverrideCursor(Qt.CursorShape.WaitCursor)
        try:
            n = import_map_image(self._map, self._style, a, px, area)
        except ValueError as ex:
            QApplication.restoreOverrideCursor()
            QMessageBox.warning(self, self.App_name, str(ex))
            return
        QApplication.restoreOverrideCursor()
        if n:
            self._map_dirty = True
            self._view.set_data(self._map, self._style)
            self._update_title()
        self._set_status(f"Map PNG imported: {n} blocks changed")
