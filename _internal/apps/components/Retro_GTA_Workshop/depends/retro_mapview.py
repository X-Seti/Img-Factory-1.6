#this belongs in apps/components/Retro_GTA_Workshop/depends/retro_mapview.py - Version: 10
# X-Seti - October 23 2026 - IMG Factory 1.6 - Retro GTA Workshop top-down map view

"""
Top-down GTA1 / GTA2 map view: lids of every level stacked (or one level) drawn from
the style tiles. Zoomed out it shows a cached overview image (8 px a
block); zoomed in it draws the visible blocks at full tile detail.
Wheel zooms, right / middle drag pans, left click selects or paints;
dragging a marker / zone (select tool) moves it.
"""

##class MapView: -
# __init__
# _cell_at
# _cell_rgb
# _item_at
# _lid_image
# _lids
# _paint_drag
# _paint_markers
# _overview
# _paint_ground
# _thumb
# center_on
# clear_cache
# fit
# markers_at
# mouseMoveEvent
# mousePressEvent
# mouseReleaseEvent
# paintEvent
# refresh_cell
# set_data
# set_level
# set_markers
# set_overlay
# set_tool
# wheelEvent

import numpy as np

from PyQt6.QtCore import QPointF, QRectF, Qt, pyqtSignal
from PyQt6.QtGui import QColor, QFont, QImage, QPainter, QPen, QPixmap
from PyQt6.QtWidgets import QWidget

from apps.methods.gta2_map import SIZE, face_flags, face_tile   # GTA1 maps share size / word layout

THUMB = 8                         # overview pixels per block
GROUND_TINT = {'road': QColor(90, 90, 255, 70), 'pavement': QColor(255, 220, 60, 70),
               'field': QColor(60, 220, 60, 70), 'water': QColor(40, 160, 255, 90),
               'building': QColor(255, 80, 80, 70)}
MARKER_COLOUR = {'player': QColor(255, 255, 255), 'phone': QColor(255, 200, 0), 'trigger': QColor(255, 60, 200),
                 'door': QColor(160, 110, 60), 'ped': QColor(60, 220, 60), 'car': QColor(60, 160, 255),
                 'light': QColor(255, 255, 140), 'area': QColor(255, 120, 40), 'object': QColor(200, 200, 200)}


class MapView(QWidget): #vers 1
    """Zoomable top-down map; signals the clicked / painted / hovered cell."""

    cell_clicked = pyqtSignal(int, int)
    cell_painted = pyqtSignal(int, int)
    cell_hovered = pyqtSignal(int, int)
    menu_requested = pyqtSignal(object)                 # global QPoint of a right-click (no drag)
    item_picked = pyqtSignal(str, int)                  # Data tab kind, row
    item_moved = pyqtSignal(str, int, float, float)     # kind, row, dx, dy in blocks

    def __init__(self, parent=None): #vers 5
        super().__init__(parent)
        self.setMouseTracking(True)
        self.setMinimumSize(300, 300)
        self.map = self.style = None
        self.scale = 8.0                       # screen pixels per block
        self.ox = self.oy = 0.0                 # screen position of block (0, 0)
        self.level = -1                         # -1 top lid, else that z
        self.tool = 'select'
        self.show_grid = True
        self.show_zones = False
        self.show_ground = False                # ground types + arrows overlay
        self.show_missions = True
        self.markers = []                       # mission / script markers
        self.show_data = True
        self.overlay = []                       # lights / map objects (Data tab)
        self.selected = None
        self._pan = None
        self._drag = None                       # dragged marker / zone
        self._pix = {}                          # (tile, rot, flip) -> QPixmap 64
        self._thumbs = {}                       # tile -> 8x8x3 array
        self._ov = None                         # overview QImage
        self._ov_arr = None

    def set_data(self, gmap, style): #vers 1
        """New map / style: caches dropped, view fitted."""
        self.map, self.style = gmap, style
        self._pix.clear()
        self._thumbs.clear()
        self._ov = None
        self.fit()

    def set_markers(self, markers: list): #vers 1
        self.markers = markers
        self.update()

    def markers_at(self, x: int, y: int) -> list: #vers 2
        """Markers in cell x, y."""
        shown = (self.markers if self.show_missions else []) + (self.overlay if self.show_data else [])
        return [m for m in shown if int(m['x']) == x and int(m['y']) == y]

    def _item_at(self, pos): #vers 1
        """(kind, row, x, y, w, h, centred) of the marker / zone under pos, else None."""
        s = self.scale
        hit = max(6.0, min(6.0, s / 6) + 3)
        for kind, shown in (('Mission objects', self.markers if self.show_missions else []),
                            ('', self.overlay if self.show_data and s >= 6 else [])):
            for i, m in enumerate(shown):
                k, row = (kind, i) if kind else m.get('src', ('', -1))
                if not k:
                    continue
                if abs(self.ox + m['x'] * s - pos.x()) <= hit and abs(self.oy + m['y'] * s - pos.y()) <= hit:
                    return (k, row, m['x'], m['y'], m['w'], m['h'], True)
        if self.show_zones and self.map is not None:
            bx, by = (pos.x() - self.ox) / s, (pos.y() - self.oy) / s
            for i, zn in enumerate(self.map.zones()):              # smallest first in GTA2
                if zn['x'] <= bx < zn['x'] + zn['w'] and zn['y'] <= by < zn['y'] + zn['h']:
                    return ('Zones', i, zn['x'], zn['y'], zn['w'], zn['h'], False)
        return None

    def _paint_drag(self, p, s): #vers 1
        """Dragged marker / zone drawn at its new place."""
        d = self._drag
        if d is None or not d['moved']:
            return
        _k, _i, x, y, w, h, centred = d['item']
        x, y = x + d['dx'], y + d['dy']
        cx, cy = self.ox + x * s, self.oy + y * s
        p.setPen(QPen(QColor(255, 80, 80), 2, Qt.PenStyle.DashLine))
        p.setBrush(Qt.BrushStyle.NoBrush)
        if centred:
            if w or h:
                p.drawRect(QRectF(cx - w * s / 2, cy - h * s / 2, w * s, h * s))
            p.drawEllipse(QPointF(cx, cy), 7, 7)
        else:
            p.drawRect(QRectF(cx, cy, w * s, h * s))

    def set_overlay(self, markers: list): #vers 1
        self.overlay = markers
        self.update()

    def _paint_markers(self, p, s, markers: list): #vers 2
        """Markers: dots (areas as boxes), type names when zoomed in."""
        p.setFont(QFont("Arial", 7))
        r = max(2.0, min(6.0, s / 6))
        for m in markers:
            c = MARKER_COLOUR.get(m['kind'], MARKER_COLOUR['object'])
            cx, cy = self.ox + m['x'] * s, self.oy + m['y'] * s
            if m['w'] or m['h']:
                p.setPen(QPen(c, 1, Qt.PenStyle.DashLine))
                p.setBrush(Qt.BrushStyle.NoBrush)
                p.drawRect(QRectF(cx - m['w'] * s / 2, cy - m['h'] * s / 2, m['w'] * s, m['h'] * s))
            p.setPen(QPen(QColor(0, 0, 0), 1))
            p.setBrush(c)
            p.drawEllipse(QPointF(cx, cy), r, r)
            if s >= 24:
                p.setPen(c)
                p.drawText(QPointF(cx + r + 2, cy + 3), m['label'])
        p.setBrush(Qt.BrushStyle.NoBrush)

    def clear_cache(self): #vers 1
        """Tile images changed: rebuild them and the overview."""
        self._pix.clear()
        self._thumbs.clear()
        self._ov = None
        self.update()

    def set_level(self, level: int): #vers 1
        self.level = level
        self._ov = None
        self.update()

    def set_tool(self, tool: str): #vers 1
        self.tool = tool
        self.setCursor(Qt.CursorShape.CrossCursor if tool == 'paint' else Qt.CursorShape.ArrowCursor)

    def _lids(self, x: int, y: int) -> list: #vers 1
        """Lid words to draw at a cell, bottom first (Top: every level, else that level)."""
        col, m = self.map.column(x, y), self.map
        zs = sorted(col) if self.level < 0 else ([self.level] if self.level in col else [])
        out = []
        for z in zs:
            w = m.face_word(m.blocks[col[z]], 'lid')
            if face_tile(w):
                out.append(w)
        return out

    def _thumb(self, lid: int) -> np.ndarray: #vers 5
        """8x8 RGBA of a lid (rotation / flip applied, alpha kept)."""
        key = lid & 0xF3FF
        a = self._thumbs.get(key)
        if a is None:
            t = self.style.face_rgba('lid', face_tile(lid))
            st = t.shape[0] // THUMB                          # 64 px or PS1 32 px tiles
            a = t[st // 2::st, st // 2::st]
            f = face_flags(lid)
            a = np.rot90(a, -f['rotation'])                  # rotate clockwise, then flip
            if f['flip']:
                a = a[:, ::-1]
            a = np.ascontiguousarray(a)
            self._thumbs[key] = a
        return a

    def _cell_rgb(self, x: int, y: int) -> np.ndarray: #vers 1
        """8x8 RGB of a cell: lids stacked bottom to top, transparent pixels show the one below."""
        cell = np.zeros((THUMB, THUMB, 3), np.uint8)
        for lid in self._lids(x, y):
            t = self._thumb(lid)
            m = t[:, :, 3] > 0
            cell[m] = t[:, :, :3][m]
        return cell

    def _overview(self) -> QImage: #vers 2
        """Whole map at 8 px a block (cached)."""
        if self._ov is None:
            arr = np.zeros((SIZE * THUMB, SIZE * THUMB, 3), np.uint8)
            for y in range(SIZE):
                for x in range(SIZE):
                    arr[y * THUMB:(y + 1) * THUMB, x * THUMB:(x + 1) * THUMB] = self._cell_rgb(x, y)
            self._ov_arr = arr
            self._ov = QImage(arr.data, arr.shape[1], arr.shape[0], arr.shape[1] * 3,
                              QImage.Format.Format_RGB888)
        return self._ov

    def refresh_cell(self, x: int, y: int): #vers 2
        """Redraw one block after an edit (overview patched in place)."""
        if self._ov is not None:
            self._ov_arr[y * THUMB:(y + 1) * THUMB, x * THUMB:(x + 1) * THUMB] = self._cell_rgb(x, y)
            self._ov = QImage(self._ov_arr.data, self._ov_arr.shape[1], self._ov_arr.shape[0],
                              self._ov_arr.shape[1] * 3, QImage.Format.Format_RGB888)
        self.update()

    def _lid_image(self, lid: int) -> QPixmap: #vers 4
        """Full tile (64 px, PS1 32 px) with the lid's rotation / flip."""
        key = lid & 0xF3FF
        pm = self._pix.get(key)
        if pm is None:
            a = self.style.face_rgba('lid', face_tile(lid))
            f = face_flags(lid)
            a = np.rot90(a, -f['rotation'])                  # rotate clockwise, then flip
            if f['flip']:
                a = a[:, ::-1]
            a = np.ascontiguousarray(a)
            h, w = a.shape[:2]
            img = QImage(a.data, w, h, w * 4, QImage.Format.Format_RGBA8888).copy()
            pm = self._pix[key] = QPixmap.fromImage(img)
        return pm

    def fit(self): #vers 1
        """Whole map in view."""
        s = min(self.width(), self.height()) / SIZE
        self.scale = max(1.0, s)
        self.ox = (self.width() - SIZE * self.scale) / 2
        self.oy = (self.height() - SIZE * self.scale) / 2
        self.update()

    def center_on(self, x: int, y: int, scale: float = 32.0): #vers 1
        self.scale = scale
        self.ox = self.width() / 2 - (x + 0.5) * scale
        self.oy = self.height() / 2 - (y + 0.5) * scale
        self.update()

    def _cell_at(self, pos) -> tuple: #vers 1
        x = int((pos.x() - self.ox) // self.scale)
        y = int((pos.y() - self.oy) // self.scale)
        return (x, y) if 0 <= x < SIZE and 0 <= y < SIZE else (None, None)

    def paintEvent(self, _ev): #vers 7
        from apps.methods.ui_color import get_ui_color
        p = QPainter(self)
        owner = self.window()                              # workshop / IMG Factory: has app_settings
        p.fillRect(self.rect(), get_ui_color(owner, 'viewport_bg'))
        if self.map is None or self.style is None:
            p.setPen(get_ui_color(owner, 'viewport_text'))
            p.drawText(self.rect(), Qt.AlignmentFlag.AlignCenter, "Open a GTA1 / GTA2 game folder or a map")
            return
        s = self.scale
        if s <= 16:                                       # overview image, scaled
            p.setRenderHint(QPainter.RenderHint.SmoothPixmapTransform, s < THUMB)
            p.drawImage(QRectF(self.ox, self.oy, SIZE * s, SIZE * s), self._overview())
        else:
            x0, y0 = max(0, int(-self.ox // s)), max(0, int(-self.oy // s))
            x1 = min(SIZE, int((self.width() - self.ox) // s) + 1)
            y1 = min(SIZE, int((self.height() - self.oy) // s) + 1)
            for y in range(y0, y1):
                for x in range(x0, x1):
                    for lid in self._lids(x, y):              # lower lids show through
                        pm = self._lid_image(lid)
                        p.drawPixmap(QRectF(self.ox + x * s, self.oy + y * s, s, s), pm, QRectF(pm.rect()))
        if self.show_grid and s >= 12:
            p.setPen(QPen(QColor(255, 255, 255, 40), 1))
            x0, y0 = max(0, int(-self.ox // s)), max(0, int(-self.oy // s))
            x1 = min(SIZE, int((self.width() - self.ox) // s) + 1)
            y1 = min(SIZE, int((self.height() - self.oy) // s) + 1)
            for x in range(x0, x1 + 1):
                p.drawLine(QPointF(self.ox + x * s, self.oy + y0 * s), QPointF(self.ox + x * s, self.oy + y1 * s))
            for y in range(y0, y1 + 1):
                p.drawLine(QPointF(self.ox + x0 * s, self.oy + y * s), QPointF(self.ox + x1 * s, self.oy + y * s))
        if self.show_ground:
            self._paint_ground(p, s)
        if self.show_zones:
            p.setFont(QFont("Arial", 8))
            for zn in self.map.zones():
                r = QRectF(self.ox + zn['x'] * s, self.oy + zn['y'] * s, zn['w'] * s, zn['h'] * s)
                p.setPen(QPen(QColor(255, 200, 40, 200), 1))
                p.drawRect(r)
                if s >= 6:
                    p.drawText(r.adjusted(2, 1, 0, 0), Qt.AlignmentFlag.AlignLeft, zn['name'])
        if self.show_missions and self.markers:
            self._paint_markers(p, s, self.markers)
        if self.show_data and self.overlay and s >= 6:          # lights / objects: zoomed in only
            self._paint_markers(p, s, self.overlay)
        if self.selected is not None:
            x, y = self.selected
            p.setPen(QPen(QColor(255, 80, 80), 2))
            p.drawRect(QRectF(self.ox + x * s, self.oy + y * s, s, s))
        self._paint_drag(p, s)
        p.end()

    def _paint_ground(self, p, s): #vers 1
        """Ground type tint per block; arrows (car paths) when zoomed in."""
        x0, y0 = max(0, int(-self.ox // s)), max(0, int(-self.oy // s))
        x1 = min(SIZE, int((self.width() - self.ox) // s) + 1)
        y1 = min(SIZE, int((self.height() - self.oy) // s) + 1)
        m = self.map
        p.setPen(Qt.PenStyle.NoPen)
        for y in range(y0, y1):
            for x in range(x0, x1):
                _z, b = m.top_block(x, y)
                if b is None:
                    continue
                g = m.GROUND[m.ground_of(b)]
                c = GROUND_TINT.get(g)
                r = QRectF(self.ox + x * s, self.oy + y * s, s, s)
                if c is not None:
                    p.fillRect(r, c)
                if s >= 16:
                    for dx, dy, red in m.arrow_dirs(b):
                        cx, cy = r.center().x(), r.center().y()
                        p.setPen(QPen(QColor(230, 50, 50) if red else QColor(60, 230, 60), max(1.5, s / 20)))
                        tip = QPointF(cx + dx * s * 0.35, cy + dy * s * 0.35)
                        p.drawLine(QPointF(cx - dx * s * 0.25, cy - dy * s * 0.25), tip)
                        p.drawLine(tip, QPointF(tip.x() - dx * s * 0.12 - dy * s * 0.1,
                                                tip.y() - dy * s * 0.12 + dx * s * 0.1))
                        p.drawLine(tip, QPointF(tip.x() - dx * s * 0.12 + dy * s * 0.1,
                                                tip.y() - dy * s * 0.12 - dx * s * 0.1))
                        p.setPen(Qt.PenStyle.NoPen)

    def wheelEvent(self, ev): #vers 1
        f = 1.25 if ev.angleDelta().y() > 0 else 0.8
        new = min(128.0, max(1.0, self.scale * f))
        pos = ev.position()
        self.ox = pos.x() - (pos.x() - self.ox) * new / self.scale
        self.oy = pos.y() - (pos.y() - self.oy) * new / self.scale
        self.scale = new
        self.update()

    def mousePressEvent(self, ev): #vers 3
        if ev.button() in (Qt.MouseButton.RightButton, Qt.MouseButton.MiddleButton):
            self._pan = ev.position()
            self._press = (ev.button(), ev.position())
            return
        x, y = self._cell_at(ev.position())
        if x is None or self.map is None:
            return
        if self.tool == 'paint':
            self.cell_painted.emit(x, y)
        else:
            item = self._item_at(ev.position())
            if item is not None:                          # drag a marker / zone to move it
                self._drag = {'item': item, 'at': ev.position(), 'dx': 0.0, 'dy': 0.0, 'moved': False}
                self.item_picked.emit(item[0], item[1])
            self.selected = (x, y)
            self.cell_clicked.emit(x, y)
            self.update()

    def mouseMoveEvent(self, ev): #vers 3
        if self._drag is not None and ev.buttons() & Qt.MouseButton.LeftButton:
            d = ev.position() - self._drag['at']
            self._drag['moved'] = self._drag['moved'] or d.manhattanLength() >= 4
            self._drag['dx'], self._drag['dy'] = d.x() / self.scale, d.y() / self.scale
            self.update()
            return
        if self._pan is not None:
            d = ev.position() - self._pan
            self.ox += d.x()
            self.oy += d.y()
            self._pan = ev.position()
            self.update()
            return
        x, y = self._cell_at(ev.position())
        if x is None or self.map is None:              # nothing loaded yet
            return
        self.cell_hovered.emit(x, y)
        if self.tool == 'paint' and ev.buttons() & Qt.MouseButton.LeftButton:
            self.cell_painted.emit(x, y)

    def mouseReleaseEvent(self, ev): #vers 3
        """End a pan / drag (moved item signalled); a right-click without dragging asks for the menu."""
        self._pan = None
        d, self._drag = self._drag, None
        if d is not None and d['moved'] and ev.button() == Qt.MouseButton.LeftButton:
            self.item_moved.emit(d['item'][0], d['item'][1], d['dx'], d['dy'])
            self.update()
        press = getattr(self, '_press', None)
        self._press = None
        if press and press[0] == Qt.MouseButton.RightButton == ev.button() and \
                (ev.position() - press[1]).manhattanLength() < 4:
            self.menu_requested.emit(ev.globalPosition().toPoint())
