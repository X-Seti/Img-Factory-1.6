#!/usr/bin/env python3
# radar_workshop.py — Build 1
# X-Seti - Apr11 2026 - Radar Workshop
# Standalone radar tile editor built on DP5Canvas engine
# Supports: VC/VCS, LC/LCS, SA (8x8 = 64 tiles), SOL (36x36 = 1296 tiles), Custom grid
# Format: one DXT1 128x128 TXD per radar tile, inside a GTA IMG v1/v2 archive

import os
import re
import struct
import sys
from pathlib import Path
from typing import List, Optional, Tuple, Dict

from PyQt6.QtCore import Qt, QPoint, QRect, QSize, pyqtSignal
from PyQt6.QtGui import (QColor, QIcon, QImage, QPainter, QPen,
                          QPixmap, QBrush, QFont)
from PyQt6.QtWidgets import (
    QApplication, QWidget, QDialog, QVBoxLayout, QHBoxLayout,
    QSplitter, QLabel, QPushButton, QComboBox, QSpinBox,
    QScrollArea, QFileDialog, QMessageBox, QStatusBar,
    QGroupBox, QCheckBox, QProgressDialog, QFrame,
    QGridLayout, QSizePolicy, QTabWidget, QToolBar,
    QToolButton, QMenu, QLineEdit
)
from PyQt6.QtGui import QAction

App_name = "Radar Workshop"
Build    = "1"

# ─────────────────────────────────────────────────────────────
# Game presets — (cols, rows, name_pattern, display_name)
# name_pattern: callable(idx) -> texture name inside the TXD,
#               and also the search pattern for IMG entries
# ─────────────────────────────────────────────────────────────
def _radar_name_sa(idx):   return f"RADAR{idx:02d}"
def _radar_name_sol(idx):  return f"radar{idx:04d}"

GAME_PRESETS = {
    "SA":  {"cols": 8,  "rows": 8,  "count": 64,   "name_fn": _radar_name_sa,
            "img_pattern": r"^radar\d{2}\.txd$|^RADAR\d{2}\.txd$",
            "label": "GTA San Andreas"},
    "VC":  {"cols": 8,  "rows": 8,  "count": 64,   "name_fn": _radar_name_sa,
            "img_pattern": r"^radar\d{2}\.txd$|^RADAR\d{2}\.txd$",
            "label": "GTA Vice City"},
    "VCS": {"cols": 8,  "rows": 8,  "count": 64,   "name_fn": _radar_name_sa,
            "img_pattern": r"^radar\d{2}\.txd$|^RADAR\d{2}\.txd$",
            "label": "GTA Vice City Stories"},
    "LC":  {"cols": 8,  "rows": 8,  "count": 64,   "name_fn": _radar_name_sa,
            "img_pattern": r"^radar\d{2}\.txd$|^RADAR\d{2}\.txd$",
            "label": "GTA Liberty City (III)"},
    "LCS": {"cols": 8,  "rows": 8,  "count": 64,   "name_fn": _radar_name_sa,
            "img_pattern": r"^radar\d{2}\.txd$|^RADAR\d{2}\.txd$",
            "label": "GTA Liberty City Stories"},
    "SOL": {"cols": 36, "rows": 36, "count": 1296,  "name_fn": _radar_name_sol,
            "img_pattern": r"^radar\d{4}\.txd$",
            "label": "GTA State of Liberty"},
    "Custom": {"cols": 8, "rows": 8, "count": 64,   "name_fn": _radar_name_sa,
               "img_pattern": r"^radar",
               "label": "Custom Grid"},
}

TILE_W = TILE_H = 128   # All radar tiles are 128x128 DXT1

# ─────────────────────────────────────────────────────────────
# DXT1 decoder (no external deps)
# ─────────────────────────────────────────────────────────────
def decode_dxt1(data: bytes, width: int, height: int) -> bytes:
    out = bytearray(width * height * 4)
    bx  = (width  + 3) // 4
    by  = (height + 3) // 4
    pos = 0
    for block_y in range(by):
        for block_x in range(bx):
            if pos + 8 > len(data):
                break
            c0 = struct.unpack_from('<H', data, pos)[0]
            c1 = struct.unpack_from('<H', data, pos+2)[0]
            lut = struct.unpack_from('<I', data, pos+4)[0]
            pos += 8

            def unpack565(v):
                return ((v>>11)&0x1F)*255//31, ((v>>5)&0x3F)*255//63, (v&0x1F)*255//31

            r0,g0,b0 = unpack565(c0)
            r1,g1,b1 = unpack565(c1)
            if c0 > c1:
                pal = [(r0,g0,b0,255),(r1,g1,b1,255),
                       ((2*r0+r1)//3,(2*g0+g1)//3,(2*b0+b1)//3,255),
                       ((r0+2*r1)//3,(g0+2*g1)//3,(b0+2*b1)//3,255)]
            else:
                pal = [(r0,g0,b0,255),(r1,g1,b1,255),
                       ((r0+r1)//2,(g0+g1)//2,(b0+b1)//2,255),
                       (0,0,0,0)]

            for py in range(4):
                for px in range(4):
                    ix = block_x*4+px
                    iy = block_y*4+py
                    if ix < width and iy < height:
                        ci = (lut >> ((py*4+px)*2)) & 3
                        o  = (iy*width+ix)*4
                        out[o:o+4] = pal[ci]
    return bytes(out)


def encode_dxt1(rgba: bytes, width: int, height: int) -> bytes:
    """Simple DXT1 encoder — per-block colour quantise (no dithering)."""
    from PIL import Image
    img = Image.frombytes('RGBA', (width, height), rgba).convert('RGB')
    bx  = (width  + 3) // 4
    by  = (height + 3) // 4
    out = bytearray()
    px  = img.load()

    def pack565(r,g,b):
        return ((r>>3)<<11)|((g>>2)<<5)|(b>>3)

    for block_y in range(by):
        for block_x in range(bx):
            pixels = []
            for py in range(4):
                for pxx in range(4):
                    ix = block_x*4+pxx
                    iy = block_y*4+py
                    if ix < width and iy < height:
                        pixels.append(px[ix,iy])
                    else:
                        pixels.append((0,0,0))
            if not pixels:
                out += b'\x00'*8
                continue
            rs = [p[0] for p in pixels]
            gs = [p[1] for p in pixels]
            bs = [p[2] for p in pixels]
            c0 = (max(rs),max(gs),max(bs))
            c1 = (min(rs),min(gs),min(bs))
            v0 = pack565(*c0)
            v1 = pack565(*c1)
            if v0 < v1:
                v0,v1 = v1,v0
                c0,c1 = c1,c0
            pal = [c0, c1,
                   tuple((2*a+b)//3 for a,b in zip(c0,c1)),
                   tuple((a+2*b)//3 for a,b in zip(c0,c1))]
            lut = 0
            for i,p in enumerate(pixels):
                best = min(range(4), key=lambda k:
                           sum((p[j]-pal[k][j])**2 for j in range(3)))
                lut |= best << (i*2)
            out += struct.pack('<HHI', v0, v1, lut)
    return bytes(out)


# ─────────────────────────────────────────────────────────────
# RW TXD reader — single-texture radar TXD (D3D8/PC, DXT1 128x128)
# ─────────────────────────────────────────────────────────────
class RadarTxdReader:
    """Reads one radar TXD and returns (rgba:bytes, w, h, tex_name)."""

    @staticmethod
    def read(data: bytes) -> Tuple[bytes, int, int, str]:
        if len(data) < 28:
            raise ValueError("TXD too small")
        # Walk sections: 0x16=TXD container, 0x15=Texture Native
        pos = 12  # skip outer header
        while pos + 12 <= len(data):
            sec_type = struct.unpack_from('<I', data, pos)[0]
            sec_size = struct.unpack_from('<I', data, pos+4)[0]
            if sec_type == 0x15:   # Texture Native
                th = pos + 24      # skip section header (12) + inner struct header (12)
                tex_name  = data[th+8:th+40].rstrip(b'\x00').decode('latin1','replace')
                width     = struct.unpack_from('<H', data, th+80)[0]
                height    = struct.unpack_from('<H', data, th+82)[0]
                comp      = struct.unpack_from('<B', data, th+87)[0]
                data_size = struct.unpack_from('<I', data, th+88)[0]
                px_off    = th + 92
                px_data   = data[px_off:px_off+data_size]
                if comp == 1:   # DXT1
                    rgba = decode_dxt1(px_data, width, height)
                else:
                    # Fallback: treat as raw RGBA/ARGB
                    rgba = bytes(px_data[:width*height*4])
                return rgba, width, height, tex_name
            pos += 12 + sec_size
            if sec_size == 0:
                break
        raise ValueError("No Texture Native section found")

    @staticmethod
    def write(rgba: bytes, width: int, height: int, tex_name: str,
              rw_ver: int = 0x1003FFFF) -> bytes:
        """Pack RGBA back into a minimal single-texture DXT1 TXD."""
        dxt = encode_dxt1(rgba, width, height)
        data_size = len(dxt)

        name_bytes  = tex_name.encode('latin1')[:31].ljust(32, b'\x00')
        alpha_bytes = b'\x00' * 32

        # Native texture struct (D3D8 PC DXT1)
        native = bytearray()
        native += struct.pack('<I', 8)            # platform D3D8
        native += struct.pack('<I', 0x00000000)   # filter flags (match original radar TXDs)
        native += name_bytes
        native += alpha_bytes
        native += struct.pack('<I', 0x00000200)   # raster format (DXT1)
        native += b'DXT1'                         # 4-cc
        native += struct.pack('<HH', width, height)
        native += struct.pack('<BBBB', 16, 1, 4, 1)  # depth mips raster comp
        native += struct.pack('<I', data_size)
        native += dxt

        def rw_section(type_id, body):
            hdr = struct.pack('<III', type_id, len(body), rw_ver)
            return hdr + body

        tex_struct_body = struct.pack('<HH', 1, 0)      # tex_count, device_id (inside container struct)
        inner_struct    = rw_section(0x01, bytes(native))
        ext_section     = rw_section(0x03, b'')
        tex_native_body = inner_struct + ext_section
        tex_native      = rw_section(0x15, tex_native_body)

        container_struct = rw_section(0x01, tex_struct_body)
        container_body   = container_struct + tex_native + rw_section(0x03, b'')
        container        = rw_section(0x16, container_body)

        # Pad to 2048-byte sector boundary
        padded = container + b'\x00' * ((-len(container)) % 2048)
        return padded


# ─────────────────────────────────────────────────────────────
# IMG v1 reader (Radartex.img / Radartex.dir style)
# ─────────────────────────────────────────────────────────────
class ImgReader:
    """Read GTA IMG v1 (.img + .dir) or v2 (self-contained) archives."""

    def __init__(self, img_path: str):
        self.img_path = img_path
        self.entries: List[Dict] = []
        self._img_data: Optional[bytes] = None
        self._load()

    def _load(self):
        img_path = Path(self.img_path)
        dir_path = img_path.with_suffix('.dir')

        if dir_path.exists():
            # IMG v1: separate .dir
            dir_data = dir_path.read_bytes()
            for i in range(0, len(dir_data), 32):
                chunk = dir_data[i:i+32]
                if len(chunk) < 32:
                    break
                offset, size = struct.unpack_from('<II', chunk, 0)
                name = chunk[8:].rstrip(b'\x00').decode('latin1', 'replace').strip('\x00')
                if name:
                    self.entries.append({
                        'name':   name,
                        'offset': offset * 2048,
                        'size':   size   * 2048,
                        'idx':    len(self.entries),
                    })
            self._img_data = img_path.read_bytes()
        else:
            # IMG v2: self-contained, 4-byte magic + count + entries
            data = img_path.read_bytes()
            if data[:4] != b'VER2':
                raise ValueError("Not a valid IMG file (no .dir and not VER2)")
            count = struct.unpack_from('<I', data, 4)[0]
            for i in range(count):
                base = 8 + i*32
                offset, size = struct.unpack_from('<II', data, base)
                name = data[base+8:base+32].rstrip(b'\x00').decode('latin1','replace')
                if name:
                    self.entries.append({
                        'name':   name,
                        'offset': offset * 2048,
                        'size':   size   * 2048,
                        'idx':    i,
                    })
            self._img_data = data

    def get_entry_data(self, entry: Dict) -> bytes:
        return self._img_data[entry['offset']:entry['offset']+entry['size']]

    def find_radar_entries(self, pattern: str) -> List[Dict]:
        rx = re.compile(pattern, re.IGNORECASE)
        return [e for e in self.entries if rx.match(e['name'])]

    def write_entry(self, entry: Dict, new_data: bytes) -> bytes:
        """Return a new copy of the img bytes with one entry replaced.
        Pads new_data to original entry size (assumes it fits — DXT1 tiles are fixed size)."""
        img = bytearray(self._img_data)
        off = entry['offset']
        sz  = entry['size']
        padded = new_data + b'\x00' * max(0, sz - len(new_data))
        img[off:off+sz] = padded[:sz]
        return bytes(img)


# ─────────────────────────────────────────────────────────────
# Tile thumbnail widget
# ─────────────────────────────────────────────────────────────
class TileThumbnail(QLabel):
    """64x64 clickable tile thumbnail with index label and dirty badge."""

    clicked = pyqtSignal(int)  # tile index

    THUMB = 64

    def __init__(self, idx: int, parent=None):
        super().__init__(parent)
        self.tile_idx = idx
        self.setFixedSize(self.THUMB + 2, self.THUMB + 16)
        self.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.setCursor(Qt.CursorShape.PointingHandCursor)
        self._dirty     = False
        self._selected  = False
        self._pixmap    = None
        self._set_blank()

    def _set_blank(self):
        pm = QPixmap(self.THUMB, self.THUMB)
        pm.fill(QColor(40, 40, 40))
        self._pixmap = pm
        self._repaint_label()

    def set_rgba(self, rgba: bytes, w: int, h: int):
        img = QImage(rgba, w, h, w*4, QImage.Format.Format_RGBA8888)
        pm  = QPixmap.fromImage(img).scaled(
            self.THUMB, self.THUMB,
            Qt.AspectRatioMode.KeepAspectRatio,
            Qt.TransformationMode.SmoothTransformation)
        self._pixmap = pm
        self._repaint_label()

    def set_dirty(self, d: bool):
        self._dirty = d
        self._repaint_label()

    def set_selected(self, s: bool):
        self._selected = s
        self._repaint_label()

    def _repaint_label(self):
        canvas = QPixmap(self.THUMB + 2, self.THUMB + 16)
        canvas.fill(Qt.GlobalColor.transparent)
        p = QPainter(canvas)

        # Border
        border = QColor(0, 180, 255) if self._selected else QColor(60, 60, 60)
        p.setPen(QPen(border, 2))
        p.drawRect(1, 1, self.THUMB, self.THUMB)

        # Tile image
        if self._pixmap:
            p.drawPixmap(2, 2, self._pixmap)

        # Dirty badge (red triangle top-right)
        if self._dirty:
            p.setPen(Qt.PenStyle.NoPen)
            p.setBrush(QBrush(QColor(220, 50, 50)))
            p.drawPolygon(*[QPoint(self.THUMB-8+2, 2),
                            QPoint(self.THUMB+2, 2),
                            QPoint(self.THUMB+2, 10)])

        # Index label
        p.setPen(QColor(180, 180, 180))
        p.setFont(QFont("monospace", 7))
        p.drawText(QRect(0, self.THUMB+2, self.THUMB+2, 14),
                   Qt.AlignmentFlag.AlignCenter,
                   str(self.tile_idx))
        p.end()
        self.setPixmap(canvas)

    def mousePressEvent(self, ev):
        if ev.button() == Qt.MouseButton.LeftButton:
            self.clicked.emit(self.tile_idx)


# ─────────────────────────────────────────────────────────────
# Tile grid panel (scrollable)
# ─────────────────────────────────────────────────────────────
class TileGridPanel(QScrollArea):
    tile_selected = pyqtSignal(int)

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWidgetResizable(True)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self._thumbs: List[TileThumbnail] = []
        self._selected_idx = -1
        self._container = QWidget()
        self._grid = QGridLayout(self._container)
        self._grid.setSpacing(2)
        self._grid.setContentsMargins(4, 4, 4, 4)
        self.setWidget(self._container)

    def setup_grid(self, count: int, cols: int):
        # Clear existing
        for t in self._thumbs:
            t.deleteLater()
        self._thumbs = []
        self._selected_idx = -1

        for idx in range(count):
            t = TileThumbnail(idx)
            t.clicked.connect(self._on_thumb_clicked)
            self._thumbs.append(t)
            self._grid.addWidget(t, idx // cols, idx % cols)

        # Fit scroll area width to grid
        thumb_w = TileThumbnail.THUMB + 4
        self.setMinimumWidth(min(cols, 12) * thumb_w + 20)

    def set_tile_rgba(self, idx: int, rgba: bytes, w: int, h: int):
        if 0 <= idx < len(self._thumbs):
            self._thumbs[idx].set_rgba(rgba, w, h)

    def set_tile_dirty(self, idx: int, dirty: bool):
        if 0 <= idx < len(self._thumbs):
            self._thumbs[idx].set_dirty(dirty)

    def select_tile(self, idx: int):
        if 0 <= self._selected_idx < len(self._thumbs):
            self._thumbs[self._selected_idx].set_selected(False)
        self._selected_idx = idx
        if 0 <= idx < len(self._thumbs):
            self._thumbs[idx].set_selected(True)
            self.ensureWidgetVisible(self._thumbs[idx])

    def _on_thumb_clicked(self, idx: int):
        self.select_tile(idx)
        self.tile_selected.emit(idx)


# ─────────────────────────────────────────────────────────────
# Minimap preview (assembled radar)
# ─────────────────────────────────────────────────────────────
class MinimapWidget(QLabel):
    """Composites all loaded tiles into a scaled assembled radar view."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.setStyleSheet("background:#111; border:1px solid #444;")
        self.setMinimumSize(200, 200)
        self._cols   = 8
        self._count  = 64
        self._tiles: Dict[int, QPixmap] = {}

    def setup(self, cols: int, count: int):
        self._cols  = cols
        self._count = count
        self._tiles = {}
        self._redraw()

    def set_tile(self, idx: int, rgba: bytes, w: int, h: int):
        img = QImage(rgba, w, h, w*4, QImage.Format.Format_RGBA8888)
        self._tiles[idx] = QPixmap.fromImage(img)
        self._redraw()

    def _redraw(self):
        rows  = (self._count + self._cols - 1) // self._cols
        total_w = self._cols * TILE_W
        total_h = rows       * TILE_H
        full = QPixmap(total_w, total_h)
        full.fill(QColor(20, 20, 20))
        p = QPainter(full)
        for idx, pm in self._tiles.items():
            col = idx % self._cols
            row = idx // self._cols
            p.drawPixmap(col*TILE_W, row*TILE_H, pm)
        p.end()

        avail = min(self.width() or 400, self.height() or 400, 512)
        scaled = full.scaled(avail, avail,
                             Qt.AspectRatioMode.KeepAspectRatio,
                             Qt.TransformationMode.SmoothTransformation)
        self.setPixmap(scaled)


# ─────────────────────────────────────────────────────────────
# Main Radar Workshop window
# ─────────────────────────────────────────────────────────────
class RadarWorkshop(QWidget):
    """
    Radar tile editor — own window, DP5Canvas embedded as paint engine.
    """

    workshop_closed = pyqtSignal()

    def __init__(self, parent=None, main_window=None):
        super().__init__(parent)
        self.main_window    = main_window
        self.setWindowTitle(f"{App_name}  [Build {Build}]")
        self.setWindowFlags(Qt.WindowType.Window)
        self.resize(1380, 820)

        # State
        self._img_reader:     Optional[ImgReader] = None
        self._img_path:       str = ""
        self._game_preset:    dict = GAME_PRESETS["SA"]
        self._current_idx:    int  = -1
        self._tile_rgba:      Dict[int, bytes] = {}   # idx → raw RGBA bytes
        self._tile_entries:   List[dict] = []          # ordered IMG entries
        self._dirty_tiles:    set = set()
        self._canvas          = None
        self._canvas_rgba:    bytearray = bytearray(TILE_W * TILE_H * 4)

        self._build_ui()

    # ─── UI construction ──────────────────────────────────────

    def _build_ui(self):
        root = QVBoxLayout(self)
        root.setContentsMargins(0, 0, 0, 0)
        root.setSpacing(0)

        root.addWidget(self._build_toolbar())

        splitter = QSplitter(Qt.Orientation.Horizontal)
        splitter.setChildrenCollapsible(False)

        # Left: tile grid
        self._tile_grid = TileGridPanel()
        self._tile_grid.tile_selected.connect(self._on_tile_selected)
        splitter.addWidget(self._tile_grid)

        # Centre: canvas + palette
        splitter.addWidget(self._build_canvas_panel())

        # Right: info + minimap tabs
        splitter.addWidget(self._build_right_panel())

        splitter.setSizes([220, 750, 260])
        root.addWidget(splitter, 1)

        # Status bar
        self._status = QStatusBar()
        self._status.setMaximumHeight(22)
        root.addWidget(self._status)
        self._set_status("No IMG loaded")

        # Initialise grid for default game
        self._apply_game_preset("SA")

    def _build_toolbar(self) -> QToolBar:
        tb = QToolBar()
        tb.setMovable(False)
        tb.setIconSize(QSize(18, 18))

        # Game selector
        tb.addWidget(QLabel("  Game: "))
        self._game_combo = QComboBox()
        self._game_combo.addItems(list(GAME_PRESETS.keys()))
        self._game_combo.setCurrentText("SA")
        self._game_combo.currentTextChanged.connect(self._on_game_changed)
        tb.addWidget(self._game_combo)

        # Custom grid spinboxes (hidden unless Custom selected)
        tb.addWidget(QLabel("  Grid: "))
        self._cols_spin = QSpinBox()
        self._cols_spin.setRange(1, 100)
        self._cols_spin.setValue(8)
        self._cols_spin.setPrefix("W ")
        self._cols_spin.setEnabled(False)
        self._cols_spin.valueChanged.connect(self._on_custom_grid_changed)
        tb.addWidget(self._cols_spin)

        self._rows_spin = QSpinBox()
        self._rows_spin.setRange(1, 100)
        self._rows_spin.setValue(8)
        self._rows_spin.setPrefix("H ")
        self._rows_spin.setEnabled(False)
        self._rows_spin.valueChanged.connect(self._on_custom_grid_changed)
        tb.addWidget(self._rows_spin)

        tb.addSeparator()

        # File ops
        load_btn = QPushButton("Load IMG…")
        load_btn.clicked.connect(self._load_img)
        tb.addWidget(load_btn)

        self._save_btn = QPushButton("Save IMG…")
        self._save_btn.clicked.connect(self._save_img)
        self._save_btn.setEnabled(False)
        tb.addWidget(self._save_btn)

        tb.addSeparator()

        export_btn = QPushButton("Export Sheet…")
        export_btn.setToolTip("Export all tiles as a single assembled PNG sheet")
        export_btn.clicked.connect(self._export_sheet)
        tb.addWidget(export_btn)

        import_btn = QPushButton("Import Sheet…")
        import_btn.setToolTip("Import a PNG sheet and slice back into tiles")
        import_btn.clicked.connect(self._import_sheet)
        tb.addWidget(import_btn)

        tb.addSeparator()

        # Tile ops
        self._push_btn = QPushButton("Push to Tile")
        self._push_btn.setToolTip("Write canvas back to current tile")
        self._push_btn.clicked.connect(self._push_canvas_to_tile)
        self._push_btn.setEnabled(False)
        tb.addWidget(self._push_btn)

        self._revert_btn = QPushButton("Revert Tile")
        self._revert_btn.setToolTip("Reload tile from original IMG data")
        self._revert_btn.clicked.connect(self._revert_tile)
        self._revert_btn.setEnabled(False)
        tb.addWidget(self._revert_btn)

        return tb

    def _build_canvas_panel(self) -> QWidget:
        panel = QWidget()
        vl    = QVBoxLayout(panel)
        vl.setContentsMargins(2, 2, 2, 2)
        vl.setSpacing(2)

        # Tile name label
        self._tile_label = QLabel("No tile selected")
        self._tile_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._tile_label.setStyleSheet("font-weight:bold; color:#aaa; padding:2px;")
        vl.addWidget(self._tile_label)

        # Canvas scroll area
        self._canvas_scroll = QScrollArea()
        self._canvas_scroll.setWidgetResizable(False)
        self._canvas_scroll.setStyleSheet("background:#1a1a1a;")
        vl.addWidget(self._canvas_scroll, 1)

        # Palette strip (borrowing PaletteGrid from dp5_workshop)
        try:
            from apps.components.DP5_Workshop.dp5_workshop import (
                PaletteGrid, FGBGSwatch)
            hl = QHBoxLayout()

            self._fgbg  = FGBGSwatch()
            self._palette = PaletteGrid(cols=16, cell=14)
            self._palette.color_picked.connect(self._on_palette_color)

            hl.addWidget(self._fgbg)
            hl.addWidget(self._palette, 1)
            vl.addLayout(hl)
        except Exception as e:
            vl.addWidget(QLabel(f"Palette unavailable: {e}"))
            self._fgbg    = None
            self._palette = None

        # Zoom controls
        hl_zoom = QHBoxLayout()
        hl_zoom.addStretch()
        for label, z in [("1×",1),("2×",2),("4×",4),("8×",8),("Fit",0)]:
            btn = QPushButton(label)
            btn.setFixedWidth(36)
            btn.clicked.connect(lambda _, zz=z: self._set_zoom(zz))
            hl_zoom.addWidget(btn)
        vl.addLayout(hl_zoom)

        return panel

    def _build_right_panel(self) -> QWidget:
        tabs = QTabWidget()

        # ── Tile Info tab ──
        info_w = QWidget()
        il = QVBoxLayout(info_w)

        self._info_name  = QLabel("—")
        self._info_size  = QLabel("—")
        self._info_fmt   = QLabel("—")
        self._info_dirty = QLabel("—")
        self._info_idx   = QLabel("—")

        for lbl, widget in [("Name:", self._info_name),
                             ("Size:", self._info_size),
                             ("Format:", self._info_fmt),
                             ("Tile:", self._info_idx),
                             ("Status:", self._info_dirty)]:
            row = QHBoxLayout()
            l   = QLabel(lbl)
            l.setFixedWidth(55)
            l.setStyleSheet("color:#888;")
            row.addWidget(l)
            row.addWidget(widget)
            il.addLayout(row)

        il.addSpacing(8)

        dirty_lbl = QLabel("Dirty tiles:")
        dirty_lbl.setStyleSheet("color:#888;")
        self._dirty_count = QLabel("0")
        row = QHBoxLayout()
        row.addWidget(dirty_lbl); row.addWidget(self._dirty_count)
        il.addLayout(row)

        il.addStretch()
        tabs.addTab(info_w, "Tile Info")

        # ── Map Preview tab ──
        self._minimap = MinimapWidget()
        tabs.addTab(self._minimap, "Map Preview")

        return tabs

    # ─── Game / grid setup ────────────────────────────────────

    def _on_game_changed(self, game: str):
        custom = (game == "Custom")
        self._cols_spin.setEnabled(custom)
        self._rows_spin.setEnabled(custom)
        self._apply_game_preset(game)

    def _on_custom_grid_changed(self):
        if self._game_combo.currentText() == "Custom":
            cols  = self._cols_spin.value()
            rows  = self._rows_spin.value()
            count = cols * rows
            GAME_PRESETS["Custom"]["cols"]  = cols
            GAME_PRESETS["Custom"]["rows"]  = rows
            GAME_PRESETS["Custom"]["count"] = count
            self._apply_game_preset("Custom")

    def _apply_game_preset(self, game: str):
        self._game_preset = GAME_PRESETS[game]
        cols  = self._game_preset["cols"]
        count = self._game_preset["count"]
        self._tile_grid.setup_grid(count, cols)
        self._minimap.setup(cols, count)
        self._tile_rgba   = {}
        self._dirty_tiles = set()
        self._current_idx = -1
        self._push_btn.setEnabled(False)
        self._revert_btn.setEnabled(False)
        self._tile_label.setText("No tile selected")
        self._update_info(None)
        self._set_status(f"{self._game_preset['label']} — {count} tiles  ({cols}×{self._game_preset['rows']})")

    # ─── IMG loading ──────────────────────────────────────────

    def _load_img(self):
        path, _ = QFileDialog.getOpenFileName(
            self, "Open Radar IMG",
            "", "IMG Archives (*.img);;All Files (*)")
        if not path:
            return
        try:
            self._img_reader = ImgReader(path)
            self._img_path   = path
        except Exception as e:
            QMessageBox.critical(self, "Load Error", str(e))
            return

        pattern = self._game_preset["img_pattern"]
        entries = self._img_reader.find_radar_entries(pattern)

        if not entries:
            QMessageBox.warning(self, "No Radar Tiles",
                f"No radar tile TXDs found in {Path(path).name}.\n"
                f"Pattern: {pattern}")
            return

        # Sort by name so tile order is deterministic
        entries.sort(key=lambda e: e['name'].lower())
        self._tile_entries = entries

        # Auto-detect grid if count matches a known preset
        count = len(entries)
        self._autodetect_grid(count)

        # Load all tiles with progress
        prog = QProgressDialog("Loading radar tiles…", "Cancel", 0, len(entries), self)
        prog.setWindowModality(Qt.WindowModality.WindowModal)
        prog.show()

        for i, entry in enumerate(entries):
            prog.setValue(i)
            QApplication.processEvents()
            if prog.wasCanceled():
                break
            try:
                txd_data = self._img_reader.get_entry_data(entry)
                rgba, w, h, tex_name = RadarTxdReader.read(txd_data)
                self._tile_rgba[i] = rgba
                self._tile_grid.set_tile_rgba(i, rgba, w, h)
                self._minimap.set_tile(i, rgba, w, h)
            except Exception as e:
                print(f"  WARN tile {i} ({entry['name']}): {e}", file=sys.stderr)

        prog.setValue(len(entries))
        self._dirty_tiles = set()
        self._save_btn.setEnabled(True)
        self._set_status(f"Loaded {len(entries)} tiles from {Path(path).name}")

    def _autodetect_grid(self, count: int):
        """Switch game preset if count matches a known pattern."""
        for game, preset in GAME_PRESETS.items():
            if game == "Custom":
                continue
            if preset["count"] == count:
                self._game_combo.blockSignals(True)
                self._game_combo.setCurrentText(game)
                self._game_combo.blockSignals(False)
                self._game_preset = preset
                cols = preset["cols"]
                self._tile_grid.setup_grid(count, cols)
                self._minimap.setup(cols, count)
                return
        # Unknown count — use custom grid (square root approximation)
        import math
        cols = max(1, round(math.sqrt(count)))
        rows = (count + cols - 1) // cols
        GAME_PRESETS["Custom"]["cols"]  = cols
        GAME_PRESETS["Custom"]["rows"]  = rows
        GAME_PRESETS["Custom"]["count"] = count
        self._game_combo.blockSignals(True)
        self._game_combo.setCurrentText("Custom")
        self._game_combo.blockSignals(False)
        self._cols_spin.setValue(cols)
        self._rows_spin.setValue(rows)
        self._game_preset = GAME_PRESETS["Custom"]
        self._tile_grid.setup_grid(count, cols)
        self._minimap.setup(cols, count)

    # ─── Tile selection → canvas ──────────────────────────────

    def _on_tile_selected(self, idx: int):
        # Push any unsaved canvas edits back before switching
        if self._current_idx >= 0 and self._canvas is not None:
            self._tile_rgba[self._current_idx] = bytes(self._canvas_rgba)

        self._current_idx = idx
        self._tile_grid.select_tile(idx)

        rgba = self._tile_rgba.get(idx)
        if rgba is None:
            self._tile_label.setText(f"Tile {idx} — not loaded")
            return

        name = (self._tile_entries[idx]['name'] if idx < len(self._tile_entries)
                else self._game_preset["name_fn"](idx))
        self._tile_label.setText(f"Tile {idx}  —  {name}")

        # Load into canvas
        self._canvas_rgba = bytearray(rgba)
        self._rebuild_canvas()

        self._push_btn.setEnabled(True)
        self._revert_btn.setEnabled(idx < len(self._tile_entries))

        dirty = idx in self._dirty_tiles
        self._update_info(idx, name, dirty)
        self._set_status(f"Tile {idx}  |  {name}  |  {TILE_W}×{TILE_H}  DXT1"
                         + ("  [modified]" if dirty else ""))

    def _rebuild_canvas(self):
        """Create or replace the DP5Canvas with current tile RGBA."""
        try:
            from apps.components.DP5_Workshop.dp5_workshop import DP5Canvas
        except Exception as e:
            self._canvas_scroll.setWidget(QLabel(f"DP5Canvas unavailable: {e}"))
            return

        self._canvas = DP5Canvas(TILE_W, TILE_H, self._canvas_rgba)
        self._canvas.zoom = 4
        self._canvas.show_grid = True
        self._canvas.pixel_changed.connect(self._on_canvas_pixel_changed)

        # Wire colour from FGBGSwatch
        if self._fgbg:
            self._canvas.color = self._fgbg.fg
            self._fgbg.fg_changed.connect(lambda c: setattr(self._canvas, 'color', c))

        self._canvas_scroll.setWidget(self._canvas)
        self._canvas_scroll.setWidget(self._canvas)
        self._canvas.update()

    def _on_canvas_pixel_changed(self, x: int, y: int):
        """Mark tile dirty when canvas is edited."""
        if self._current_idx >= 0:
            self._dirty_tiles.add(self._current_idx)
            self._tile_grid.set_tile_dirty(self._current_idx, True)
            self._dirty_count.setText(str(len(self._dirty_tiles)))
            self._info_dirty.setText("Modified ●")
            self._info_dirty.setStyleSheet("color:#f66;")

    def _on_palette_color(self, color: QColor):
        if self._canvas:
            self._canvas.color = color
        if self._fgbg:
            self._fgbg.set_fg(color)

    # ─── Tile push / revert ───────────────────────────────────

    def _push_canvas_to_tile(self):
        idx = self._current_idx
        if idx < 0 or self._canvas is None:
            return
        rgba = bytes(self._canvas_rgba)
        self._tile_rgba[idx] = rgba
        self._dirty_tiles.add(idx)
        self._tile_grid.set_tile_rgba(idx, rgba, TILE_W, TILE_H)
        self._tile_grid.set_tile_dirty(idx, True)
        self._minimap.set_tile(idx, rgba, TILE_W, TILE_H)
        self._dirty_count.setText(str(len(self._dirty_tiles)))
        self._set_status(f"Tile {idx} pushed  |  {len(self._dirty_tiles)} tile(s) modified")

    def _revert_tile(self):
        idx = self._current_idx
        if idx < 0 or idx >= len(self._tile_entries):
            return
        try:
            txd_data = self._img_reader.get_entry_data(self._tile_entries[idx])
            rgba, w, h, _ = RadarTxdReader.read(txd_data)
            self._tile_rgba[idx] = rgba
            self._canvas_rgba = bytearray(rgba)
            self._dirty_tiles.discard(idx)
            self._tile_grid.set_tile_rgba(idx, rgba, w, h)
            self._tile_grid.set_tile_dirty(idx, False)
            self._minimap.set_tile(idx, rgba, w, h)
            self._rebuild_canvas()
            self._dirty_count.setText(str(len(self._dirty_tiles)))
            self._update_info(idx, self._tile_entries[idx]['name'], False)
        except Exception as e:
            QMessageBox.warning(self, "Revert Error", str(e))

    # ─── Save IMG ─────────────────────────────────────────────

    def _save_img(self):
        if not self._img_reader or not self._dirty_tiles:
            QMessageBox.information(self, "Nothing to Save", "No tiles have been modified.")
            return

        path, _ = QFileDialog.getSaveFileName(
            self, "Save Radar IMG",
            self._img_path, "IMG Archives (*.img);;All Files (*)")
        if not path:
            return

        try:
            img_data = self._img_reader._img_data   # working copy
            for idx in sorted(self._dirty_tiles):
                if idx >= len(self._tile_entries):
                    continue
                entry = self._tile_entries[idx]
                rgba  = self._tile_rgba.get(idx)
                if rgba is None:
                    continue
                tex_name = entry['name'].replace('.txd','')
                new_txd  = RadarTxdReader.write(rgba, TILE_W, TILE_H, tex_name)
                # Patch into img_data
                off = entry['offset']
                sz  = entry['size']
                padded = new_txd + b'\x00' * max(0, sz - len(new_txd))
                img_data = img_data[:off] + padded[:sz] + img_data[off+sz:]

            Path(path).write_bytes(img_data)

            # If saving alongside original .dir, copy it too
            src_dir = Path(self._img_path).with_suffix('.dir')
            dst_dir = Path(path).with_suffix('.dir')
            if src_dir.exists() and path != self._img_path:
                import shutil
                shutil.copy2(src_dir, dst_dir)

            self._dirty_tiles = set()
            for i in range(len(self._tile_entries)):
                self._tile_grid.set_tile_dirty(i, False)
            self._dirty_count.setText("0")
            self._set_status(f"Saved to {Path(path).name}")

        except Exception as e:
            QMessageBox.critical(self, "Save Error", str(e))

    # ─── Export / Import sheet ────────────────────────────────

    def _export_sheet(self):
        if not self._tile_rgba:
            QMessageBox.information(self, "Nothing to Export", "Load an IMG first.")
            return
        path, _ = QFileDialog.getSaveFileName(
            self, "Export Radar Sheet", "radar_sheet.png",
            "PNG Images (*.png)")
        if not path:
            return

        cols  = self._game_preset["cols"]
        count = len(self._tile_rgba)
        rows  = (count + cols - 1) // cols

        from PIL import Image
        sheet = Image.new("RGBA", (cols*TILE_W, rows*TILE_H), (0,0,0,0))
        for idx, rgba in self._tile_rgba.items():
            if len(rgba) == TILE_W*TILE_H*4:
                tile = Image.frombytes("RGBA", (TILE_W, TILE_H), rgba)
                col = idx % cols
                row = idx // cols
                sheet.paste(tile, (col*TILE_W, row*TILE_H))

        sheet.save(path)
        self._set_status(f"Exported {count} tiles to {Path(path).name}")

    def _import_sheet(self):
        path, _ = QFileDialog.getOpenFileName(
            self, "Import Radar Sheet", "",
            "PNG Images (*.png);;All Images (*.png *.bmp *.tga)")
        if not path:
            return

        from PIL import Image
        sheet = Image.open(path).convert("RGBA")
        cols  = self._game_preset["cols"]
        tile_w = sheet.width  // cols
        rows  = sheet.height  // tile_w   # assume square tiles
        count = min(cols * rows, self._game_preset["count"])

        changed = 0
        for idx in range(count):
            col = idx % cols
            row = idx // cols
            tile = sheet.crop((col*tile_w, row*tile_w,
                               (col+1)*tile_w, (row+1)*tile_w))
            if tile_w != TILE_W:
                tile = tile.resize((TILE_W, TILE_H), Image.LANCZOS)
            rgba = tile.tobytes()
            self._tile_rgba[idx] = rgba
            self._dirty_tiles.add(idx)
            self._tile_grid.set_tile_rgba(idx, rgba, TILE_W, TILE_H)
            self._tile_grid.set_tile_dirty(idx, True)
            self._minimap.set_tile(idx, rgba, TILE_W, TILE_H)
            changed += 1

        self._dirty_count.setText(str(len(self._dirty_tiles)))
        self._set_status(f"Imported {changed} tiles from sheet  —  {len(self._dirty_tiles)} dirty")

    # ─── Zoom ─────────────────────────────────────────────────

    def _set_zoom(self, z: int):
        if self._canvas is None:
            return
        if z == 0:
            # Fit: choose zoom so canvas fills scroll area
            avail_w = self._canvas_scroll.viewport().width()
            avail_h = self._canvas_scroll.viewport().height()
            z = max(1, min(avail_w // TILE_W, avail_h // TILE_H))
        self._canvas.zoom = z
        self._canvas.update()

    # ─── Info / status ────────────────────────────────────────

    def _update_info(self, idx, name=None, dirty=False):
        if idx is None:
            for w in [self._info_name, self._info_size, self._info_fmt,
                      self._info_idx, self._info_dirty]:
                w.setText("—")
                w.setStyleSheet("")
            return
        self._info_idx.setText(str(idx))
        self._info_name.setText(name or "—")
        self._info_size.setText(f"{TILE_W} × {TILE_H} px")
        self._info_fmt.setText("DXT1 (compressed)")
        if dirty:
            self._info_dirty.setText("Modified ●")
            self._info_dirty.setStyleSheet("color:#f66;")
        else:
            self._info_dirty.setText("Clean")
            self._info_dirty.setStyleSheet("color:#6f6;")

    def _set_status(self, msg: str):
        self._status.showMessage(msg)

    def closeEvent(self, ev):
        if self._dirty_tiles:
            r = QMessageBox.question(
                self, "Unsaved Changes",
                f"{len(self._dirty_tiles)} tile(s) have unsaved changes.\nClose anyway?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
            if r != QMessageBox.StandardButton.Yes:
                ev.ignore()
                return
        self.workshop_closed.emit()
        ev.accept()


# ─────────────────────────────────────────────────────────────
# Launcher
# ─────────────────────────────────────────────────────────────
def open_radar_workshop(main_window=None) -> RadarWorkshop:
    try:
        w = RadarWorkshop(None, main_window)
        try:
            from apps.methods.imgfactory_svg_icons import get_radar_icon
            w.setWindowIcon(get_radar_icon(64))
        except Exception:
            pass
        w.show()
        return w
    except Exception as e:
        if main_window:
            QMessageBox.critical(main_window, App_name + " Error", str(e))
        return None


__all__ = ['RadarWorkshop', 'open_radar_workshop']


# ─────────────────────────────────────────────────────────────
# Standalone entry point
# ─────────────────────────────────────────────────────────────
if __name__ == "__main__":
    import traceback
    print(f"{App_name} Build {Build} starting…")
    try:
        app = QApplication(sys.argv)
        app.setApplicationName(App_name)
        app.setOrganizationName("X-Seti")
        w = RadarWorkshop()
        w.show()
        sys.exit(app.exec())
    except Exception as e:
        print(f"ERROR: {e}")
        traceback.print_exc()
        sys.exit(1)
