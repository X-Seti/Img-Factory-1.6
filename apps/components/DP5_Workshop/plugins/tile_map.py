#this belongs in apps/components/DP5_Workshop/plugins/tile_map.py - Version: 1
# X-Seti - October 27 2026 - IMG Factory 1.6 - DP5 Tile Map Editor Plugin.

"""
Tile Map editor: cut a tile set from any canvas tab (tile size, margin,
spacing), then stamp tiles onto a map grid. Left click places the picked
tile (Fill mode floods same-tile areas), right click erases, Pick mode
copies a cell's tile. Maps save / load as CSV (-1 = empty) and render to
a new tab or the active canvas.
"""

##Methods list -
# run
#
##class MapView: -
# __init__
# _cell_at
# _fill
# _put
# mouseMoveEvent
# mousePressEvent
# paintEvent
# set_zoom
#
##class TileMapDialog: -
# __init__
# _build_ui
# _cut_tiles
# _load_csv
# _render
# _resize_map
# _save_csv
# _to_canvas
# _to_tab
# mode

import csv

import numpy as np
from PyQt6.QtCore import QSize, Qt
from PyQt6.QtGui import QColor, QIcon, QImage, QPainter, QPen, QPixmap
from PyQt6.QtWidgets import (QButtonGroup, QComboBox, QDialog, QFileDialog, QFormLayout, QHBoxLayout, QLabel,
                             QListWidget, QListWidgetItem, QMessageBox, QPushButton, QScrollArea, QSpinBox,
                             QVBoxLayout, QWidget)

PLUGIN_NAME = "Tile Map"


class MapView(QWidget):
    """Map grid: cells hold tile indices (-1 empty)."""

    def __init__(self, dlg): #vers 1
        super().__init__()
        self._dlg = dlg
        self._zoom = 2
        self.setMouseTracking(True)
        self._hover = None

    def set_zoom(self, z: int): #vers 1
        self._zoom = z
        d = self._dlg
        self.setFixedSize(d.cols * d.tw * z + 1, d.rows * d.th * z + 1)
        self.update()

    def _cell_at(self, pos): #vers 1
        d = self._dlg
        cx, cy = pos.x() // (d.tw * self._zoom), pos.y() // (d.th * self._zoom)
        return (cx, cy) if 0 <= cx < d.cols and 0 <= cy < d.rows else None

    def _put(self, cell, value: int): #vers 1
        d = self._dlg
        cx, cy = cell
        if d.map[cy, cx] != value:
            d.map[cy, cx] = value
            self.update()

    def _fill(self, cell, value: int): #vers 1
        """Flood the 4-connected area of the clicked cell's tile."""
        d = self._dlg
        target = d.map[cell[1], cell[0]]
        if target == value:
            return
        stack = [cell]
        while stack:
            x, y = stack.pop()
            if 0 <= x < d.cols and 0 <= y < d.rows and d.map[y, x] == target:
                d.map[y, x] = value
                stack += [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        self.update()

    def mousePressEvent(self, e): #vers 1
        cell = self._cell_at(e.position().toPoint())
        if not cell:
            return
        d = self._dlg
        right = e.button() == Qt.MouseButton.RightButton
        mode = d.mode()
        if mode == 'pick':
            v = int(d.map[cell[1], cell[0]])
            if v >= 0:
                d.picker.setCurrentRow(v)
        elif mode == 'fill':
            self._fill(cell, -1 if right else d.picker.currentRow())
        else:
            self._put(cell, -1 if right else d.picker.currentRow())

    def mouseMoveEvent(self, e): #vers 1
        cell = self._cell_at(e.position().toPoint())
        if cell != self._hover:
            self._hover = cell
            self.update()
        d = self._dlg
        if cell and d.mode() == 'place':
            if e.buttons() & Qt.MouseButton.LeftButton:
                self._put(cell, d.picker.currentRow())
            elif e.buttons() & Qt.MouseButton.RightButton:
                self._put(cell, -1)

    def paintEvent(self, _): #vers 1
        d = self._dlg
        p = QPainter(self)
        z, tw, th = self._zoom, d.tw, d.th
        p.fillRect(self.rect(), QColor(40, 40, 40))
        for cy in range(d.rows):
            for cx in range(d.cols):
                v = int(d.map[cy, cx])
                if 0 <= v < len(d.tiles):
                    p.drawImage(cx * tw * z, cy * th * z,
                                d.tiles[v].scaled(tw * z, th * z, Qt.AspectRatioMode.IgnoreAspectRatio,
                                                  Qt.TransformationMode.FastTransformation))
        p.setPen(QPen(QColor(90, 90, 90), 1))
        for cx in range(d.cols + 1):
            p.drawLine(cx * tw * z, 0, cx * tw * z, d.rows * th * z)
        for cy in range(d.rows + 1):
            p.drawLine(0, cy * th * z, d.cols * tw * z, cy * th * z)
        if self._hover:
            p.setPen(QPen(QColor(0, 200, 255), 2))
            p.drawRect(self._hover[0] * tw * z, self._hover[1] * th * z, tw * z, th * z)


class TileMapDialog(QDialog):
    """Tile set picker + map grid; non-modal."""

    def __init__(self, host): #vers 1
        super().__init__(host.widget)
        self.host = host
        self.setWindowTitle("Tile Map Editor")
        self.resize(1000, 680)
        self.tw = self.th = 16
        self.cols, self.rows = 32, 20
        self.map = np.full((self.rows, self.cols), -1, np.int32)
        self.tiles = []
        self._build_ui()

    def _build_ui(self): #vers 1
        lay = QHBoxLayout(self)
        left = QVBoxLayout()
        form = QFormLayout()
        self.src = QComboBox()
        self.src.addItems(self.host.tabs())
        self.src.setCurrentIndex(self.host.active_tab())
        form.addRow("Tile set tab:", self.src)
        self.tw_spin, self.th_spin, self.margin, self.spacing = QSpinBox(), QSpinBox(), QSpinBox(), QSpinBox()
        for s, v in ((self.tw_spin, 16), (self.th_spin, 16), (self.margin, 0), (self.spacing, 0)):
            s.setRange(0 if s in (self.margin, self.spacing) else 1, 512)
            s.setValue(v)
        form.addRow("Tile width:", self.tw_spin)
        form.addRow("Tile height:", self.th_spin)
        form.addRow("Margin:", self.margin)
        form.addRow("Spacing:", self.spacing)
        left.addLayout(form)
        cut = QPushButton("Cut tile set")
        cut.setToolTip("Slice the chosen tab into tiles (map is kept)")
        cut.clicked.connect(self._cut_tiles)
        left.addWidget(cut)
        self.picker = QListWidget()
        self.picker.setViewMode(QListWidget.ViewMode.IconMode)
        self.picker.setIconSize(QSize(32, 32))
        self.picker.setMovement(QListWidget.Movement.Static)
        self.picker.setResizeMode(QListWidget.ResizeMode.Adjust)
        self.picker.setMinimumWidth(220)
        left.addWidget(QLabel("Tiles (click to pick):"))
        left.addWidget(self.picker, 1)
        lay.addLayout(left)

        right = QVBoxLayout()
        top = QHBoxLayout()
        self.cols_spin, self.rows_spin = QSpinBox(), QSpinBox()
        for s, v in ((self.cols_spin, self.cols), (self.rows_spin, self.rows)):
            s.setRange(1, 1024)
            s.setValue(v)
        top.addWidget(QLabel("Map"))
        top.addWidget(self.cols_spin)
        top.addWidget(QLabel("x"))
        top.addWidget(self.rows_spin)
        size_btn = QPushButton("Resize map")
        size_btn.clicked.connect(self._resize_map)
        top.addWidget(size_btn)
        top.addSpacing(12)
        self.modes = QButtonGroup(self)
        for i, (name, tip) in enumerate((("Place", "Left place, right erase (drag to paint)"),
                                         ("Fill", "Flood fill the clicked area"),
                                         ("Pick", "Pick the clicked cell's tile"))):
            b = QPushButton(name)
            b.setCheckable(True)
            b.setToolTip(tip)
            b.setChecked(i == 0)
            self.modes.addButton(b, i)
            top.addWidget(b)
        top.addSpacing(12)
        top.addWidget(QLabel("Zoom"))
        self.zoom = QSpinBox()
        self.zoom.setRange(1, 8)
        self.zoom.setValue(2)
        top.addWidget(self.zoom)
        top.addStretch()
        right.addLayout(top)
        self.view = MapView(self)
        sa = QScrollArea()
        sa.setWidget(self.view)
        right.addWidget(sa, 1)
        self.zoom.valueChanged.connect(self.view.set_zoom)
        bottom = QHBoxLayout()
        for text, fn in (("Load map…", self._load_csv), ("Save map…", self._save_csv),
                         ("Render to new tab", self._to_tab), ("Render to canvas", self._to_canvas)):
            b = QPushButton(text)
            b.clicked.connect(fn)
            bottom.addWidget(b)
        bottom.addStretch()
        close = QPushButton("Close")
        close.clicked.connect(self.close)
        bottom.addWidget(close)
        right.addLayout(bottom)
        lay.addLayout(right, 1)
        self._cut_tiles()

    def mode(self) -> str: #vers 1
        return ('place', 'fill', 'pick')[self.modes.checkedId()]

    def _cut_tiles(self): #vers 1
        """Tiles from the chosen tab, left to right, top to bottom."""
        rgba, w, h = self.host.tab_image(self.src.currentIndex())
        tw, th, m, s = self.tw_spin.value(), self.th_spin.value(), self.margin.value(), self.spacing.value()
        a = np.frombuffer(bytes(rgba), np.uint8).reshape(h, w, 4)
        self.tiles = []
        self.picker.clear()
        y = m
        while y + th <= h:
            x = m
            while x + tw <= w:
                t = np.ascontiguousarray(a[y:y + th, x:x + tw])
                img = QImage(t.tobytes(), tw, th, tw * 4, QImage.Format.Format_RGBA8888).copy()
                self.tiles.append(img)
                it = QListWidgetItem(QIcon(QPixmap.fromImage(img.scaled(32, 32, Qt.AspectRatioMode.KeepAspectRatio))),
                                     str(len(self.tiles) - 1))
                self.picker.addItem(it)
                x += tw + s
            y += th + s
        self.tw, self.th = tw, th
        if self.tiles:
            self.picker.setCurrentRow(0)
        self.map[self.map >= len(self.tiles)] = -1
        self.view.set_zoom(self.zoom.value())

    def _resize_map(self): #vers 1
        """New size keeps the overlapping cells."""
        c, r = self.cols_spin.value(), self.rows_spin.value()
        new = np.full((r, c), -1, np.int32)
        rr, cc = min(r, self.rows), min(c, self.cols)
        new[:rr, :cc] = self.map[:rr, :cc]
        self.map, self.cols, self.rows = new, c, r
        self.view.set_zoom(self.zoom.value())

    def _render(self) -> bytearray: #vers 1
        """Map as RGBA (cols*tw x rows*th), empty cells transparent."""
        out = QImage(self.cols * self.tw, self.rows * self.th, QImage.Format.Format_RGBA8888)
        out.fill(Qt.GlobalColor.transparent)
        p = QPainter(out)
        p.setCompositionMode(QPainter.CompositionMode.CompositionMode_Source)
        for cy in range(self.rows):
            for cx in range(self.cols):
                v = int(self.map[cy, cx])
                if 0 <= v < len(self.tiles):
                    p.drawImage(cx * self.tw, cy * self.th, self.tiles[v])
        p.end()
        return bytearray(out.constBits().asstring(out.sizeInBytes()))

    def _to_tab(self): #vers 1
        self.host.new_tab(self._render(), self.cols * self.tw, self.rows * self.th, "map")
        self.host.status(f"Map {self.cols} x {self.rows} rendered to a new tab")

    def _to_canvas(self): #vers 1
        self.host.set_image(self._render(), self.cols * self.tw, self.rows * self.th)
        self.host.status(f"Map {self.cols} x {self.rows} rendered to the canvas")

    def _save_csv(self): #vers 1
        path, _ = QFileDialog.getSaveFileName(self, "Save tile map", "map.csv", "CSV (*.csv)")
        if not path:
            return
        with open(path, 'w', newline='') as f:
            csv.writer(f).writerows(self.map.tolist())
        self.host.status(f"Map saved: {path}")

    def _load_csv(self): #vers 1
        path, _ = QFileDialog.getOpenFileName(self, "Load tile map", "", "CSV (*.csv)")
        if not path:
            return
        with open(path, newline='') as f:
            rows = [[int(v) for v in r] for r in csv.reader(f) if r]
        if not rows or len({len(r) for r in rows}) != 1:
            QMessageBox.warning(self, "Load tile map", "Not a rectangular tile map CSV")
            return
        self.map = np.array(rows, np.int32)
        self.rows, self.cols = self.map.shape
        self.cols_spin.setValue(self.cols)
        self.rows_spin.setValue(self.rows)
        self.view.set_zoom(self.zoom.value())


def run(host): #vers 1
    """Open the editor (one per DP5 window)."""
    if not host.image():
        return
    dlg = TileMapDialog(host)
    host.widget._tile_map_dialog = dlg                  # keep it alive
    dlg.show()


PLUGIN_ACTIONS = [("Tile map editor…", run)]
