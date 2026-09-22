#!/usr/bin/env python3
#this belongs in apps/components/Path_Workshop/path_workshop.py - Version: 22
# X-Seti - September 2026 - IMG Factory 1.6 - Path Workshop
# Radar Workshop (tiles + ribbons) with a Paths tab: path files are plotted
# in world coordinates over the radar tiles or a large background image.
# Formats: apps/methods/path_formats.py (flight/spath/train text, IPL 'path'
# section, SA nodes*.dat). Every save backs the file up first.

##Methods list -
# PathMapCanvas
# PathWorkshop
# open_path_workshop

import os
import sys
from pathlib import Path

_root = Path(__file__).resolve().parents[3]
if str(_root) not in sys.path:
    sys.path.insert(0, str(_root))

from PyQt6.QtCore import Qt, QPointF, QRectF, pyqtSignal
from PyQt6.QtGui import QColor, QImage, QPainter, QPen, QBrush
from PyQt6.QtWidgets import (
    QApplication, QCheckBox, QDialog, QDialogButtonBox, QDoubleSpinBox,
    QFileDialog, QFormLayout, QHBoxLayout, QLabel, QListWidget, QListWidgetItem,
    QMessageBox, QPushButton, QSplitter, QVBoxLayout, QWidget)

from apps.components.Radar_Editor.radar_workshop import RadarWorkshop
from apps.methods.path_formats import load_path_layer

App_name  = "Path Workshop"
App_build = "Sep 2026"
Build     = "Build 2"

_LAYER_COLORS = ["#ff5252", "#40c4ff", "#ffd740", "#69f0ae", "#e040fb",
                 "#ff9100", "#18ffff", "#b2ff59"]


class PathMapCanvas(QWidget): #vers 1
    """World-coordinate plot: optional background image mapped onto
    bounds (min_x, max_x, min_y, max_y), path layers on top. Wheel zooms
    at the cursor, right/middle drag pans, left click selects the nearest
    node and dragging moves it (release = one undo step)."""

    node_selected = pyqtSignal(int, int)      # layer index, node index (-1 none)
    node_moved    = pyqtSignal(int, int)      # live while dragging
    edit_started  = pyqtSignal(int)           # layer index, before a drag changes it
    edit_finished = pyqtSignal(int)

    HIT_PX = 9

    def __init__(self, parent=None):
        super().__init__(parent)
        self.layers = []
        self.bg: QImage = None
        self.bounds = (-3000.0, 3000.0, -3000.0, 3000.0)
        self._zoom, self._pan = 1.0, QPointF(0, 0)
        self._sel = (-1, -1)
        self._drag = None
        self._pan_from = None
        self._fitted = False
        self.setMouseTracking(True)
        self.setMinimumSize(320, 240)
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)

    # -- coordinate mapping (world y is up, screen y is down)
    def _base_scale(self):
        x0, x1, y0, y1 = self.bounds
        return min(self.width() / max(1e-6, x1 - x0), self.height() / max(1e-6, y1 - y0))

    def w2s(self, x, y):
        x0, x1, y0, y1 = self.bounds
        s = self._base_scale() * self._zoom
        return QPointF((x - x0) * s + self._pan.x(), (y1 - y) * s + self._pan.y())

    def s2w(self, p):
        x0, x1, y0, y1 = self.bounds
        s = self._base_scale() * self._zoom
        return (p.x() - self._pan.x()) / s + x0, y1 - (p.y() - self._pan.y()) / s

    def set_bounds(self, b):
        self.bounds = tuple(float(v) for v in b)
        self.fit()

    def set_background(self, img):
        self.bg = img
        self.update()

    def fit(self):
        self._zoom, self._pan = 1.0, QPointF(0, 0)
        self.update()

    def fit_to_layers(self):
        pts = [p for l in self.layers if l.visible for p in l.points]
        if not pts:
            return self.fit()
        xs, ys = [p[0] for p in pts], [p[1] for p in pts]
        x0, x1, y0, y1 = min(xs), max(xs), min(ys), max(ys)
        pad = max(20.0, (x1 - x0) * 0.05, (y1 - y0) * 0.05)
        self._zoom = 1.0
        base = self._base_scale()
        s = min(self.width() / (x1 - x0 + 2 * pad), self.height() / (y1 - y0 + 2 * pad))
        self._zoom = s / base
        bx0, bx1, by0, by1 = self.bounds
        self._pan = QPointF(self.width() / 2 - ((x0 + x1) / 2 - bx0) * s,
                            self.height() / 2 - (by1 - (y0 + y1) / 2) * s)
        self.update()

    def set_selection(self, li, ni):
        self._sel = (li, ni)
        self.update()

    def paintEvent(self, ev):
        p = QPainter(self)
        p.fillRect(self.rect(), self.palette().color(self.backgroundRole()).darker(160))
        x0, x1, y0, y1 = self.bounds
        if self.bg is not None:
            # Draw only the visible part: a huge destination rect (deep zoom)
            # is silently dropped by Qt, so clip source and target to the view.
            tl, br = self.w2s(x0, y1), self.w2s(x1, y0)
            full = QRectF(tl, br)
            vis = full.intersected(QRectF(self.rect()))
            if vis.width() > 0 and vis.height() > 0 and full.width() > 0:
                kx, ky = self.bg.width() / full.width(), self.bg.height() / full.height()
                src = QRectF((vis.left() - full.left()) * kx, (vis.top() - full.top()) * ky,
                             vis.width() * kx, vis.height() * ky)
                p.drawImage(vis, self.bg, src)
        else:
            p.setPen(QPen(QColor(90, 90, 100), 1))
            step = 500.0
            gx = x0 - (x0 % step)
            while gx <= x1:
                a, b = self.w2s(gx, y0), self.w2s(gx, y1)
                p.drawLine(a, b)
                gx += step
            gy = y0 - (y0 % step)
            while gy <= y1:
                a, b = self.w2s(x0, gy), self.w2s(x1, gy)
                p.drawLine(a, b)
                gy += step
        for li, layer in enumerate(self.layers):
            if not layer.visible:
                continue
            col = QColor(_LAYER_COLORS[li % len(_LAYER_COLORS)])
            pts = layer.points
            p.setPen(QPen(col, 1.4))
            for a, b in layer.edges:
                p.drawLine(self.w2s(pts[a][0], pts[a][1]), self.w2s(pts[b][0], pts[b][1]))
            p.setPen(Qt.PenStyle.NoPen)
            p.setBrush(QBrush(col))
            r = 3.0 if len(pts) < 5000 else 1.5
            for pt in pts:
                p.drawEllipse(self.w2s(pt[0], pt[1]), r, r)
        li, ni = self._sel
        if 0 <= li < len(self.layers) and 0 <= ni < len(self.layers[li].points):
            pt = self.layers[li].points[ni]
            p.setPen(QPen(QColor("white"), 2))
            p.setBrush(Qt.BrushStyle.NoBrush)
            p.drawEllipse(self.w2s(pt[0], pt[1]), 7, 7)
        p.end()

    def _nearest(self, pos):
        best, hit = self.HIT_PX ** 2, (-1, -1)
        for li, layer in enumerate(self.layers):
            if not layer.visible:
                continue
            for ni, pt in enumerate(layer.points):
                q = self.w2s(pt[0], pt[1])
                d = (q.x() - pos.x()) ** 2 + (q.y() - pos.y()) ** 2
                if d < best:
                    best, hit = d, (li, ni)
        return hit

    def mousePressEvent(self, ev):
        if ev.button() == Qt.MouseButton.LeftButton:
            hit = self._nearest(ev.position())
            self._sel = hit
            self.node_selected.emit(*hit)
            if hit[0] >= 0:
                self.edit_started.emit(hit[0])
                self._drag = hit
            self.update()
        elif ev.button() in (Qt.MouseButton.RightButton, Qt.MouseButton.MiddleButton):
            self._pan_from = ev.position()

    def mouseMoveEvent(self, ev):
        if self._drag and ev.buttons() & Qt.MouseButton.LeftButton:
            li, ni = self._drag
            x, y = self.s2w(ev.position())
            self.layers[li].move(ni, round(x, 3), round(y, 3))
            self.node_moved.emit(li, ni)
            self.update()
        elif self._pan_from is not None:
            d = ev.position() - self._pan_from
            self._pan += d
            self._pan_from = ev.position()
            self.update()

    def mouseReleaseEvent(self, ev):
        if self._drag and ev.button() == Qt.MouseButton.LeftButton:
            self.edit_finished.emit(self._drag[0])
            self._drag = None
        self._pan_from = None

    def wheelEvent(self, ev):
        f = 1.25 if ev.angleDelta().y() > 0 else 0.8
        pos = ev.position()
        wx, wy = self.s2w(pos)
        self._zoom = max(0.2, min(200.0, self._zoom * f))
        q = self.w2s(wx, wy)
        self._pan += pos - q
        self.update()


class _BoundsDialog(QDialog): #vers 1
    def __init__(self, parent, bounds):
        super().__init__(parent)
        self.setWindowTitle("Background world bounds")
        fl = QFormLayout(self)
        self.sp = []
        for label, v in zip(("Min X (west)", "Max X (east)", "Min Y (south)", "Max Y (north)"), bounds):
            s = QDoubleSpinBox()
            s.setRange(-100000, 100000)
            s.setDecimals(1)
            s.setValue(v)
            fl.addRow(label, s)
            self.sp.append(s)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(self.accept)
        bb.rejected.connect(self.reject)
        fl.addRow(bb)

    def values(self):
        return tuple(s.value() for s in self.sp)


class PathWorkshop(RadarWorkshop): #vers 2
    """Radar Workshop + a Paths tab: plot, select, move, add/delete nodes."""

    App_name       = "Path Workshop"
    config_key     = "path_workshop"
    _ribbon_name   = "path_workshop"
    # Bump when the set of ribbons changes (1 = Radar ribbons + Paths ribbon)
    _RIBBON_LAYOUT_VERSION = 1

    def __init__(self, parent=None, main_window=None): #vers 2
        self._path_undo, self._path_redo = [], []
        self._bg_is_radar = False
        super().__init__(parent, main_window)
        self.setWindowTitle(App_name)

    # -- UI
    def setup_ui(self): #vers 2
        super().setup_ui()
        tab = QWidget()
        lay = QHBoxLayout(tab)
        lay.setContentsMargins(0, 0, 0, 0)
        sp = QSplitter(Qt.Orientation.Horizontal)
        left = QWidget()
        ll = QVBoxLayout(left)
        ll.setContentsMargins(4, 4, 4, 4)
        ll.addWidget(QLabel("Path files"))
        self._layer_list = QListWidget()
        self._layer_list.itemChanged.connect(self._on_layer_toggled)
        ll.addWidget(self._layer_list, 1)
        self._node_lbl = QLabel("No node selected")
        ll.addWidget(self._node_lbl)
        self._spx, self._spy, self._spz = (QDoubleSpinBox() for _ in range(3))
        for sp_, pre in ((self._spx, "X "), (self._spy, "Y "), (self._spz, "Z ")):
            sp_.setRange(-100000, 100000)
            sp_.setDecimals(3)
            sp_.setPrefix(pre)
            sp_.setEnabled(False)
            ll.addWidget(sp_)
        self._apply_btn = QPushButton("Apply position")
        self._apply_btn.setEnabled(False)
        self._apply_btn.clicked.connect(self._apply_node_spins)
        ll.addWidget(self._apply_btn)
        self._path_status = QLabel("")
        self._path_status.setWordWrap(True)
        ll.addWidget(self._path_status)
        sp.addWidget(left)
        self._path_canvas = PathMapCanvas()
        self._path_canvas.node_selected.connect(self._on_node_selected)
        self._path_canvas.node_moved.connect(self._on_node_moved)
        self._path_canvas.edit_started.connect(self._push_undo)
        self._path_canvas.edit_finished.connect(lambda li: self._update_state())
        sp.addWidget(self._path_canvas)
        sp.setSizes([230, 900])
        lay.addWidget(sp)
        self._view_tabs.addTab(tab, "Paths")
        self._paths_tab_index = self._view_tabs.count() - 1
        self._view_tabs.tabBar().setTabButton(
            self._paths_tab_index, self._view_tabs.tabBar().ButtonPosition.RightSide, None)
        self._path_canvas.set_bounds(self.get_world_bounds())
        if hasattr(self, '_title_lbl'):
            self._title_lbl.setText(f"{App_name} - {Build}")

    def _build_ribbons(self): #vers 2
        super()._build_ribbons()
        self._ribbon_mw.addToolBarBreak()
        tb = self.ribbon_toolbar("Paths")
        B = self.ribbon_button
        B(tb, "open_icon",   "Open path file(s)...", self._open_paths)
        self._paths_save_btn = B(tb, "save_icon", "Save path files (backup first)", self._save_paths, enabled=False)
        B(tb, "saveas_icon", "Save selected path file as...", self._save_path_as)
        B(tb, "trash_icon",  "Close selected path file", self._close_layer)
        tb.addSeparator()
        B(tb, "undo_icon", "Undo path edit", self._undo_path)
        B(tb, "redo_icon", "Redo path edit", self._redo_path)
        tb.addSeparator()
        B(tb, "add_icon",   "Add waypoint after selected (text waypoint files)", self._add_node)
        B(tb, "trash_icon", "Delete selected waypoint (text waypoint files)", self._delete_node)
        tb.addSeparator()
        B(tb, "map_icon", "Use loaded radar tiles as background", self._use_radar_bg, text="Tiles")
        B(tb, "import_icon", "Load a background image (PNG/JPG)...", self._load_bg_image)
        B(tb, "settings_icon", "Set background world bounds...", self._set_bounds)
        B(tb, "fit_grid_icon", "Fit view to path data", lambda: self._path_canvas.fit_to_layers())
        B(tb, "zoom_out_icon", "Show the whole background / map bounds", lambda: self._path_canvas.fit())

    # -- helpers
    def _layers(self):
        return self._path_canvas.layers

    def _cur_layer(self):
        li = self._layer_list.currentRow()
        if li < 0 and self._path_canvas._sel[0] >= 0:
            li = self._path_canvas._sel[0]
        return li if 0 <= li < len(self._layers()) else -1

    def _refresh_layer_list(self):
        self._layer_list.blockSignals(True)
        self._layer_list.clear()
        for l in self._layers():
            it = QListWidgetItem(f"{'* ' if l.dirty else ''}{l.name}  ({len(l.points)})")
            it.setFlags(it.flags() | Qt.ItemFlag.ItemIsUserCheckable)
            it.setCheckState(Qt.CheckState.Checked if l.visible else Qt.CheckState.Unchecked)
            self._layer_list.addItem(it)
        self._layer_list.blockSignals(False)

    def _update_state(self):
        dirty = any(l.dirty for l in self._layers())
        self._paths_save_btn.setEnabled(dirty)
        self._refresh_layer_list()
        self._path_canvas.update()

    def _on_layer_toggled(self, item):
        li = self._layer_list.row(item)
        if 0 <= li < len(self._layers()):
            self._layers()[li].visible = item.checkState() == Qt.CheckState.Checked
            self._path_canvas.update()

    # -- files
    def _open_paths(self): #vers 1
        files, _ = QFileDialog.getOpenFileNames(
            self, "Open path file(s)", "",
            "Path files (nodes*.dat train*.dat tracks*.dat flight*.dat spath*.dat *.ipl *.dat);;All files (*)")
        for f in files:
            self.load_path_file(f)
        if files:
            self._path_canvas.fit_to_layers()
            self._view_tabs.setCurrentIndex(self._paths_tab_index)

    def load_path_file(self, path: str) -> bool: #vers 1
        try:
            layer = load_path_layer(path)
        except Exception as e:
            QMessageBox.warning(self, App_name, f"Could not read {os.path.basename(path)}:\n{e}")
            return False
        self._layers().append(layer)
        self._update_state()
        self._path_status.setText(f"Loaded {layer.name}: {len(layer.points)} nodes")
        return True

    def _write_layer(self, layer, path) -> bool: #vers 1
        from apps.methods.file_backup import backup_file, note_change
        try:
            if os.path.exists(path):
                note_change(f"Save path {os.path.basename(path)}")
                if backup_file(path) is None:
                    QMessageBox.warning(self, App_name, "Backup failed - file not overwritten.")
                    return False
            layer.save(path)
            return True
        except Exception as e:
            QMessageBox.critical(self, App_name, f"Save failed for {os.path.basename(path)}:\n{e}")
            return False

    def _save_paths(self): #vers 1
        saved = [l.name for l in self._layers() if l.dirty and self._write_layer(l, l.path)]
        self._update_state()
        if saved:
            self._set_status("Saved " + ", ".join(saved))

    def _save_path_as(self): #vers 1
        li = self._cur_layer()
        if li < 0:
            return
        layer = self._layers()[li]
        path, _ = QFileDialog.getSaveFileName(self, "Save path file as", layer.path)
        if path and self._write_layer(layer, path):
            self._update_state()
            self._set_status(f"Saved {os.path.basename(path)}")

    def _close_layer(self): #vers 1
        li = self._cur_layer()
        if li < 0:
            return
        l = self._layers()[li]
        if l.dirty and QMessageBox.question(
                self, App_name, f"{l.name} has unsaved changes. Close anyway?") != QMessageBox.StandardButton.Yes:
            return
        del self._layers()[li]
        self._path_canvas.set_selection(-1, -1)
        self._update_state()

    # -- background
    def _use_radar_bg(self): #vers 1
        img = self.get_composite_image(4096)
        if img is None:
            QMessageBox.information(self, App_name, "Load radar tiles first (File ribbon: Load radar IMG).")
            return
        self._path_canvas.set_background(img)
        self._path_canvas.set_bounds(self.get_world_bounds())
        self._bg_is_radar = True

    def _load_bg_image(self): #vers 1
        path, _ = QFileDialog.getOpenFileName(
            self, "Background image", "", "Images (*.png *.jpg *.jpeg *.bmp);;All files (*)")
        if not path:
            return
        img = QImage(path)
        if img.isNull():
            QMessageBox.warning(self, App_name, "Could not read that image.")
            return
        self._path_canvas.set_background(img)
        self._bg_is_radar = False
        self._set_bounds()

    def _set_bounds(self): #vers 1
        dlg = _BoundsDialog(self, self._path_canvas.bounds)
        if dlg.exec() == QDialog.DialogCode.Accepted:
            x0, x1, y0, y1 = dlg.values()
            if x1 > x0 and y1 > y0:
                self._path_canvas.set_bounds((x0, x1, y0, y1))

    # -- editing
    def _push_undo(self, li): #vers 1
        if 0 <= li < len(self._layers()):
            self._path_undo.append((self._layers()[li], self._layers()[li].snapshot()))
            del self._path_undo[:-50]
            self._path_redo.clear()

    def _undo_path(self): #vers 1
        if not self._path_undo:
            return
        layer, snap = self._path_undo.pop()
        self._path_redo.append((layer, layer.snapshot()))
        layer.restore(snap)
        self._update_state()

    def _redo_path(self): #vers 1
        if not self._path_redo:
            return
        layer, snap = self._path_redo.pop()
        self._path_undo.append((layer, layer.snapshot()))
        layer.restore(snap)
        self._update_state()

    def _on_node_selected(self, li, ni): #vers 1
        on = li >= 0
        for s in (self._spx, self._spy, self._spz, self._apply_btn):
            s.setEnabled(on)
        if on:
            self._layer_list.setCurrentRow(li)
            self._fill_spins(li, ni)
        else:
            self._node_lbl.setText("No node selected")

    def _fill_spins(self, li, ni):
        p = self._layers()[li].points[ni]
        self._node_lbl.setText(f"{self._layers()[li].name}  node {ni}")
        for s, v in ((self._spx, p[0]), (self._spy, p[1]), (self._spz, p[2])):
            s.blockSignals(True)
            s.setValue(v)
            s.blockSignals(False)

    def _on_node_moved(self, li, ni): #vers 1
        self._fill_spins(li, ni)

    def _apply_node_spins(self): #vers 1
        li, ni = self._path_canvas._sel
        if li < 0:
            return
        self._push_undo(li)
        self._layers()[li].move(ni, self._spx.value(), self._spy.value(), self._spz.value())
        self._update_state()

    def _add_node(self): #vers 1
        li, ni = self._path_canvas._sel
        if li < 0 or not self._layers()[li].can_add:
            QMessageBox.information(self, App_name,
                "Select a node in a flight / spath / train file first "
                "(IPL paths and nodes.dat link nodes by index, so they can't grow).")
            return
        layer = self._layers()[li]
        self._push_undo(li)
        x, y, z, *_ = layer.points[ni]
        new = layer.add(x + 20.0, y + 20.0, z, after=ni)
        self._path_canvas.set_selection(li, new)
        self._on_node_selected(li, new)
        self._update_state()

    def _delete_node(self): #vers 1
        li, ni = self._path_canvas._sel
        if li < 0 or not self._layers()[li].can_delete or len(self._layers()[li].points) <= 1:
            QMessageBox.information(self, App_name,
                "Only nodes of flight / spath / train files can be deleted.")
            return
        self._push_undo(li)
        self._layers()[li].delete(ni)
        self._path_canvas.set_selection(-1, -1)
        self._on_node_selected(-1, -1)
        self._update_state()

    # -- menus / close
    def _build_menus_into_qmenu(self, pm): #vers 2
        super()._build_menus_into_qmenu(pm)
        pm.addSeparator()
        m = pm.addMenu("Paths")
        m.addAction("Open Path File(s)...", self._open_paths)
        m.addAction("Save Path Files", self._save_paths)

    def closeEvent(self, event): #vers 1
        if self.standalone_mode and any(l.dirty for l in self._layers()):
            r = QMessageBox.question(
                self, App_name, "Save changed path files before closing?",
                QMessageBox.StandardButton.Save | QMessageBox.StandardButton.Discard
                | QMessageBox.StandardButton.Cancel)
            if r == QMessageBox.StandardButton.Cancel:
                event.ignore()
                return
            if r == QMessageBox.StandardButton.Save:
                self._save_paths()
                if any(l.dirty for l in self._layers()):
                    event.ignore()
                    return
        super().closeEvent(event)


def open_path_workshop(main_window=None, path: str = None): #vers 2
    app = QApplication.instance() or QApplication(sys.argv)
    w = PathWorkshop(main_window=main_window)
    w.resize(1280, 860)
    w.show()
    if path:
        w.load_path_file(path)
    return w


if __name__ == "__main__":
    app = QApplication(sys.argv)
    w = PathWorkshop()
    w.resize(1280, 860)
    w.show()
    for a in sys.argv[1:]:
        if os.path.isfile(a):
            w.load_path_file(a)
    sys.exit(app.exec())
