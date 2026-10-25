#this belongs in apps/components/Retro_GTA_Workshop/depends/retro_port_func.py - Version: 1
# X-Seti - October 24 2026 - IMG Factory 1.6 - Retro GTA Workshop map port

"""
Port blocks of another map (PC <-> PS1, same game) into the open map,
tiles matched to the open style.
"""

##Methods list -
#
##class RetroPortMixin: -
# _port_from_map

import os
import struct

from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import QApplication, QFileDialog, QMessageBox

from apps.methods.retro_port import port_blocks


class RetroPortMixin: #vers 1
    """Edit ribbon: Port blocks from map."""

    def _port_from_map(self): #vers 1
        """Pick a source map; its blocks replace this map's (tiles matched)."""
        if self._map is None or self._style is None:
            self._set_status("Open the target map (and its style) first")
            return
        path, _ = QFileDialog.getOpenFileName(self, "Port blocks from map", self._data_dir or '',
                                              "Maps (*.cmp *.vsr *.gmp);;All files (*)")
        if not path:
            return
        try:
            src = self._read_map(path)
            sty_path = self._style_for(path)
            if not sty_path:
                raise ValueError(f"No style found beside {os.path.basename(path)}")
            src_style = self._read_style(sty_path, src)
        except (OSError, ValueError, KeyError, struct.error) as ex:
            QMessageBox.warning(self, self.App_name, str(ex))
            return
        if QMessageBox.question(
                self, self.App_name,
                f"Replace every block of {os.path.basename(self._map.path)} with the blocks of "
                f"{os.path.basename(path)} ({src.GAME}), tiles matched to "
                f"{os.path.basename(self._style.path)}?\nRoutes, zones and other data stay. "
                f"Not undoable - reopen the map without saving to go back.") != QMessageBox.StandardButton.Yes:
            return
        QApplication.setOverrideCursor(Qt.CursorShape.WaitCursor)
        try:
            n = port_blocks(src, src_style, self._map, self._style)
        except ValueError as ex:
            QMessageBox.warning(self, self.App_name, str(ex))
            return
        finally:
            QApplication.restoreOverrideCursor()
        self._map_dirty = True
        self._block_map_opened()
        self._view.set_data(self._map, self._style)
        self._update_title()
        self._set_status(f"Ported {n['blocks']} blocks from {os.path.basename(path)}: "
                         f"{n['side tiles']} side, {n['lid tiles']} lid tiles matched")
