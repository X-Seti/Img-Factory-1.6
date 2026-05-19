#!/usr/bin/env python3
#this belongs in apps/components/Hex_Editor/hex_workshop.py - Version: 2
# X-Seti - May18 2026 - IMG Factory 1.6 - Hex Workshop
"""
Hex Workshop - Standalone hex editor with 3-panel display.
Based on Hex_Editor_Panel.py three-panel layout.
Uses gui_workshop.py base (local copy — do not import from Tmp_Template).

##Methods list -
# HexViewWidget.__init__
# HexViewWidget._build_panels
# HexViewWidget.load_data
# HexViewWidget.load_file
# HexViewWidget.display_hex
# HexViewWidget.goto_offset
# HexViewWidget.show_hex_context_menu
# HexViewWidget.copy_selected
# HexViewWidget.paste_hex
# StructureView.__init__
# StructureView.parse_dff
# StructureView.show_context_menu
# StructureView.goto_offset
# HexWorkshop.__init__
# HexWorkshop.setup_ui
# HexWorkshop._build_left_panel
# HexWorkshop._build_centre_panel
# HexWorkshop._build_right_panel
# HexWorkshop._open_file
# HexWorkshop._save_file
# HexWorkshop.load_file
# HexWorkshop._goto_offset
# HexWorkshop._build_menus_into_qmenu
# show_hex_editor_for_file
# show_hex_editor_for_entry
# open_hex_workshop
"""

import sys, os, struct, binascii, tempfile
from pathlib import Path
from typing import Optional

_root = Path(__file__).resolve().parents[3]
if str(_root) not in sys.path: sys.path.insert(0, str(_root))

from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QSplitter, QLabel, QLineEdit,
    QTextEdit, QTableWidget, QTableWidgetItem, QHeaderView, QScrollArea,
    QPushButton, QFileDialog, QMessageBox, QApplication, QFrame,
    QMenu, QAbstractItemView, QToolBar, QSizePolicy
)
from PyQt6.QtCore import Qt, QMimeData, pyqtSignal
from PyQt6.QtGui import QFont, QColor, QClipboard, QKeySequence, QContextMenuEvent

try:
    from apps.gui.tool_menu_mixin import ToolMenuMixin
except ImportError:
    class ToolMenuMixin:
        def _build_menus_into_qmenu(self, pm): pass

try:
    from PyQt6.QtGui import QAction
except ImportError:
    from PyQt6.QtWidgets import QAction

# GUIWorkshop inlined below

App_name   = "Hex Workshop"
App_build  = "Build 1"
config_key = "hex_workshop"


# ─────────────────────────────────────────────────────────────────────────────
# 3-Panel Hex View
# ─────────────────────────────────────────────────────────────────────────────

class HexViewWidget(QWidget):  #vers 1
    """3-panel hex display: Address | Hex values | ASCII"""
    offset_selected = pyqtSignal(int)   # emitted when user clicks a row

    BYTES_PER_ROW = 16

    def __init__(self, parent=None):  #vers 1
        super().__init__(parent)
        self._data: bytes = b''
        self._build_panels()

    def _build_panels(self):  #vers 1
        layout = QHBoxLayout(self)
        layout.setContentsMargins(0,0,0,0); layout.setSpacing(0)
        sp = QSplitter(Qt.Orientation.Horizontal)

        mono = QFont("Courier New", 10)

        # Panel 1 — Address
        self.addr_panel = QTextEdit()
        self.addr_panel.setReadOnly(True); self.addr_panel.setFont(mono)
        self.addr_panel.setMaximumWidth(90)
        self.addr_panel.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.addr_panel.setStyleSheet("background:#1e1e2e; color:#888; border:none;")

        # Panel 2 — Hex
        self.hex_panel = QTextEdit()
        self.hex_panel.setReadOnly(True); self.hex_panel.setFont(mono)
        self.hex_panel.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.hex_panel.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.hex_panel.customContextMenuRequested.connect(self.show_hex_context_menu)
        self.hex_panel.setStyleSheet("background:#1e1e2e; color:#d4d4d4; border:none;")

        # Panel 3 — ASCII
        self.ascii_panel = QTextEdit()
        self.ascii_panel.setReadOnly(True); self.ascii_panel.setFont(mono)
        self.ascii_panel.setMaximumWidth(160)
        self.ascii_panel.setStyleSheet("background:#1e1e2e; color:#9cdcfe; border:none;")

        # Sync scrolling
        def _sync_scroll(val):
            self.addr_panel.verticalScrollBar().setValue(val)
            self.ascii_panel.verticalScrollBar().setValue(val)
        self.hex_panel.verticalScrollBar().valueChanged.connect(_sync_scroll)

        sp.addWidget(self.addr_panel)
        sp.addWidget(self.hex_panel)
        sp.addWidget(self.ascii_panel)
        sp.setSizes([90, 500, 160])
        layout.addWidget(sp)

    def load_data(self, data: bytes):  #vers 1
        self._data = data
        self.display_hex()

    def load_file(self, path: str) -> bool:  #vers 1
        try:
            with open(path, 'rb') as f:
                self._data = f.read()
            self.display_hex()
            return True
        except Exception as ex:
            print(f"HexViewWidget.load_file: {ex}"); return False

    def display_hex(self):  #vers 1
        addr_lines = []; hex_lines = []; ascii_lines = []
        data = self._data
        bpr  = self.BYTES_PER_ROW
        for i in range(0, len(data), bpr):
            chunk = data[i:i+bpr]
            addr_lines.append(f"{i:08X}:")
            hex_part = ' '.join(f"{b:02X}" for b in chunk)
            hex_part += '   ' * (bpr - len(chunk))   # pad short last row
            hex_lines.append(hex_part)
            ascii_part = ''.join(chr(b) if 32 <= b < 127 else '.' for b in chunk)
            ascii_lines.append(ascii_part)
        self.addr_panel.setPlainText('\n'.join(addr_lines))
        self.hex_panel.setPlainText('\n'.join(hex_lines))
        self.ascii_panel.setPlainText('\n'.join(ascii_lines))

    def goto_offset(self, offset: int):  #vers 1
        row = offset // self.BYTES_PER_ROW
        sb  = self.hex_panel.verticalScrollBar()
        line_h = self.hex_panel.fontMetrics().lineSpacing()
        sb.setValue(row * line_h)

    def show_hex_context_menu(self, pos):  #vers 1
        menu = QMenu(self)
        menu.addAction("Copy selected", self.copy_selected)
        menu.addAction("Paste hex",     self.paste_hex)
        menu.exec(self.hex_panel.mapToGlobal(pos))

    def copy_selected(self):  #vers 1
        text = self.hex_panel.textCursor().selectedText()
        QApplication.clipboard().setText(text)

    def paste_hex(self):  #vers 1
        clip = QApplication.clipboard().text().strip().replace(' ','')
        try:
            new_bytes = bytes.fromhex(clip)
            cursor = self.hex_panel.textCursor()
            row    = cursor.blockNumber()
            offset = row * self.BYTES_PER_ROW
            data   = bytearray(self._data)
            data[offset:offset+len(new_bytes)] = new_bytes
            self._data = bytes(data)
            self.display_hex()
        except Exception: pass


# ─────────────────────────────────────────────────────────────────────────────
# Structure / parse view (right panel)
# ─────────────────────────────────────────────────────────────────────────────

class StructureView(QTableWidget):  #vers 1
    goto_requested = pyqtSignal(int)

    COLS = ["Offset", "Size", "Type", "Value", "Description"]

    def __init__(self, parent=None):  #vers 1
        super().__init__(0, len(self.COLS), parent)
        self.setHorizontalHeaderLabels(self.COLS)
        self.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.ResizeToContents)
        self.horizontalHeader().setStretchLastSection(True)
        self.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        self.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        self.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.customContextMenuRequested.connect(self.show_context_menu)
        self.setFont(QFont("Courier New", 9))

    def parse_dff(self, data: bytes):  #vers 1
        """Parse RenderWare binary stream chunks."""
        self.setRowCount(0)
        if len(data) < 12: return
        pos = 0
        RW_TYPES = {
            0x01: "Struct",       0x02: "String",
            0x03: "Extension",    0x0E: "Texture",
            0x0F: "Material",     0x10: "Material List",
            0x11: "Frame List",   0x12: "Geometry",
            0x1A: "Clump",        0x1B: "Atomic",
            0x15: "Texture Native",
        }
        try:
            while pos + 12 <= len(data):
                chunk_type = struct.unpack_from('<I', data, pos)[0]
                chunk_size = struct.unpack_from('<I', data, pos+4)[0]
                chunk_ver  = struct.unpack_from('<I', data, pos+8)[0]
                type_name  = RW_TYPES.get(chunk_type, f"0x{chunk_type:04X}")
                row = self.rowCount(); self.insertRow(row)
                for col, val in enumerate([
                    f"0x{pos:08X}", str(chunk_size),
                    f"0x{chunk_type:04X}", f"0x{chunk_ver:08X}", type_name
                ]):
                    item = QTableWidgetItem(val)
                    item.setData(Qt.ItemDataRole.UserRole, pos)
                    self.setItem(row, col, item)
                pos += 12 + chunk_size
                if chunk_size == 0: break
        except Exception: pass

    def show_context_menu(self, pos):  #vers 1
        item = self.itemAt(pos)
        if not item: return
        offset = item.data(Qt.ItemDataRole.UserRole)
        if offset is None: return
        menu = QMenu(self)
        menu.addAction(f"Go to offset 0x{offset:08X}",
                       lambda: self.goto_requested.emit(offset))
        menu.addAction("Copy row", lambda: QApplication.clipboard().setText(
            '  '.join(self.item(item.row(), c).text() for c in range(self.columnCount()))))
        menu.exec(self.viewport().mapToGlobal(pos))

    def goto_offset(self, offset: int):  #vers 1
        for row in range(self.rowCount()):
            it = self.item(row, 0)
            if it and int(it.text(), 16) == offset:
                self.selectRow(row)
                self.scrollToItem(it)
                break


# ─────────────────────────────────────────────────────────────────────────────
# Main Workshop
# ─────────────────────────────────────────────────────────────────────────────


class WorkshopSettings:
    """Per-app JSON settings.  Stored at ~/.config/imgfactory/{config_key}.json
    Same pattern as RADSettings / WATSettings across all workshops.
    """
    MAX_RECENT = 10

    DEFAULTS = {
        # Window geometry
        "window_x": -1,  "window_y": -1,
        "window_w": 1400, "window_h": 800,
        # Toolbar / menu
        "show_menubar":            False,
        "menu_style":              "dropdown",  # "dropdown" | "topbar"
        "menu_bar_font_size":      9,
        "menu_bar_height":         22,
        "menu_dropdown_font_size": 9,
        # Status bar
        "show_statusbar":          True,
        # Fonts
        "font_title_family":  "Arial",        "font_title_size":   14,
        "font_panel_family":  "Arial",        "font_panel_size":   10,
        "font_button_family": "Arial",        "font_button_size":  10,
        "font_info_family":   "Courier New",  "font_info_size":     9,
        # Display
        "button_display_mode": "both",         # "both"|"icons"|"text"
        "sidebar_width":       82,
        # Recent files
        "recent_files": [],
    }

    def __init__(self, config_key: str = "gui_workshop"):
        cfg = Path.home() / ".config" / "imgfactory"
        cfg.mkdir(parents=True, exist_ok=True)
        self._path = cfg / f"{config_key}.json"
        self._data = dict(self.DEFAULTS)
        self._load()

    def _load(self):
        try:
            if self._path.exists():
                self._data.update(
                    {k: v for k, v in json.loads(self._path.read_text()).items()
                     if k in self.DEFAULTS})
        except Exception:
            pass

    def save(self):
        try: self._path.write_text(json.dumps(self._data, indent=2))
        except Exception: pass

    def get(self, key, default=None):
        return self._data.get(
            key, default if default is not None else self.DEFAULTS.get(key))

    def set(self, key, value):
        if key in self.DEFAULTS:
            self._data[key] = value

    def add_recent(self, path: str):
        r = [p for p in self._data.get("recent_files", []) if p != str(path)]
        r.insert(0, str(path))
        self._data["recent_files"] = r[:self.MAX_RECENT]
        self.save()

    def get_recent(self) -> list:
        return [p for p in self._data.get("recent_files", [])
                if Path(p).exists()]


#    _CornerOverlay                                                             

class _CornerOverlay(QWidget):
    """Transparent overlay that draws accent-coloured resize triangles.
    Shared by all GUIWorkshop subclasses — do not modify.
    """
    SIZE = 20

    def __init__(self, parent):
        super().__init__(parent)
        for attr in [Qt.WidgetAttribute.WA_TransparentForMouseEvents,
                     Qt.WidgetAttribute.WA_NoSystemBackground,
                     Qt.WidgetAttribute.WA_TranslucentBackground,
                     Qt.WidgetAttribute.WA_AlwaysStackOnTop]:
            self.setAttribute(attr, True)
        self.setWindowFlags(Qt.WindowType.Widget)
        self._hover_corner = None
        self._app_settings = getattr(parent, "app_settings", None)
        self.setGeometry(0, 0, parent.width(), parent.height())
        self._update_mask()


    def _get_ui_color(self, key): #vers 1
        """Return theme-aware QColor. No hardcoded colors - everything via app_settings."""
        from PyQt6.QtGui import QColor
        try:
            app_settings = getattr(self, 'app_settings', None) or \
                getattr(getattr(self, 'main_window', None), 'app_settings', None)
            if app_settings and hasattr(app_settings, 'get_ui_color'):
                return app_settings.get_ui_color(key)
        except Exception:
            pass
        pal = self.palette()
        if key == 'viewport_bg':
            return pal.color(pal.ColorRole.Base)
        if key == 'viewport_text':
            return pal.color(pal.ColorRole.PlaceholderText)
        if key == 'border':
            return pal.color(pal.ColorRole.Mid)
        return pal.color(pal.ColorRole.WindowText)

    def _update_mask(self):
        from PyQt6.QtGui import QRegion
        s = self.SIZE; w, h = self.width(), self.height()
        region = QRegion()
        for pts in [
            [QPoint(0,0),   QPoint(s,0),   QPoint(0,s)],
            [QPoint(w,0),   QPoint(w-s,0), QPoint(w,s)],
            [QPoint(0,h),   QPoint(s,h),   QPoint(0,h-s)],
            [QPoint(w,h),   QPoint(w-s,h), QPoint(w,h-s)],
        ]:
            region = region.united(QRegion(QPolygon(pts)))
        self.setMask(region)

    def update_state(self, hover_corner, app_settings):
        self._hover_corner = hover_corner
        self._app_settings = app_settings
        self.update()

    def setGeometry(self, *a):
        super().setGeometry(*a); self._update_mask()

    def resizeEvent(self, ev):
        super().resizeEvent(ev); self._update_mask()

    def paintEvent(self, ev):
        s = self.SIZE
        try:
            accent = QColor(
                self._app_settings.get_theme_colors()
                    .get("accent_primary", "#4682FF"))
        except Exception:
            accent = self._get_ui_color('accent_primary') if hasattr(self,'_get_ui_color') else QColor(70,130,255)
        accent.setAlpha(200)
        hc = QColor(accent); hc.setAlpha(255)
        w, h = self.width(), self.height()
        corners = {
            "top-left":     [(0,0),   (s,0),   (0,s)],
            "top-right":    [(w,0),   (w-s,0), (w,s)],
            "bottom-left":  [(0,h),   (s,h),   (0,h-s)],
            "bottom-right": [(w,h),   (w-s,h), (w,h-s)],
        }
        p = QPainter(self)
        for name, pts in corners.items():
            p.setBrush(hc if name == self._hover_corner else accent)
            p.setPen(Qt.PenStyle.NoPen)
            p.drawPolygon(QPolygon([QPoint(x, y) for x, y in pts]))
        p.end()


#
# SECTION 2 — Toolbar: Menu, Settings UI, Info [i], Cog [⚙]
#
#
# Toolbar layout (left → right):
#   [Menu] [Settings]  <stretch>  <Title>  <stretch>
#   [Open] [Save] [Export] [Import]  [Undo]  [ℹ]  [⚙]  [_] [⬜] [✕]
#
# [Menu]     → _on_menu_btn_clicked → _show_dropdown_menu (or toggle topbar)
# [Settings] → _show_workshop_settings (Fonts / Display / Menu / About tabs)
# [ℹ  Info]  → _show_about (reads App_author, App_year, App_description)
# [⚙  Cog]  → _launch_theme_settings (global AppSettings SettingsDialog)
# [_ ⬜ ✕]  → minimize / maximize / close  (standalone only)


class _ToolbarMixin:
    """Toolbar + Settings dialog + theme methods.
    Mixed into GUIWorkshop — not used standalone.
    """

    #    Toolbar creation                                                       

    def _create_toolbar(self):
        self.toolbar = QFrame()
        self.toolbar.setFrameStyle(QFrame.Shape.StyledPanel)
        self.toolbar.setFixedHeight(self.toolbarheight)
        self.toolbar.setObjectName("titlebar")
        self.toolbar.installEventFilter(self)
        self.toolbar.setMouseTracking(True)
        self.titlebar = self.toolbar   # alias for drag detection

        lo = QHBoxLayout(self.toolbar)
        lo.setContentsMargins(5, 4, 5, 4)
        lo.setSpacing(4)
        ic = self._get_icon_color()

        # Helper: create a fixed-size icon button
        def _ibtn(icon_fn, tip, slot):
            b = QPushButton()
            try:
                b.setIcon(getattr(SVGIconFactory, icon_fn)(20, ic))
                b.setIconSize(QSize(20, 20))
            except Exception:
                pass
            b.setFixedSize(35, 35)
            b.setToolTip(tip)
            b.clicked.connect(slot)
            return b

        #    Left: [Menu] [Settings]                                        
        self.menu_btn = QPushButton("Menu")
        self.menu_btn.setFont(self.button_font)
        self.menu_btn.setMinimumHeight(28)
        self.menu_btn.setMaximumHeight(35)
        self.menu_btn.setToolTip(
            "Show menu (dropdown or top bar — set in Settings)")
        self.menu_btn.clicked.connect(self._on_menu_btn_clicked)
        lo.addWidget(self.menu_btn)

        self.settings_btn = QPushButton()
        try:
            self.settings_btn.setIcon(SVGIconFactory.settings_icon(20, ic))
            self.settings_btn.setIconSize(QSize(20, 20))
        except Exception:
            pass
        self.settings_btn.setText(" Settings")
        self.settings_btn.setFont(self.button_font)
        self.settings_btn.setMinimumHeight(28)
        self.settings_btn.setMaximumHeight(35)
        self.settings_btn.setToolTip(
            "Workshop settings — Fonts, Display, Menu, About")
        self.settings_btn.clicked.connect(self._show_workshop_settings)
        lo.addWidget(self.settings_btn)

        lo.addSpacing(4)
        lo.addStretch()

        #    Centre: title                                                  
        self.title_label = QLabel(self.App_name)
        self.title_label.setFont(self.title_font)
        self.title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.title_label.setObjectName("title_label")
        lo.addWidget(self.title_label)

        lo.addStretch()
        lo.addSpacing(4)

        #    Right: action buttons                                          
        self.open_btn   = _ibtn("open_icon",   "Open  Ctrl+O",  self._open_file)
        self.save_btn   = _ibtn("save_icon",   "Save  Ctrl+S",  self._save_file)
        self.export_btn = _ibtn("export_icon", "Export",        self._export_file)
        self.import_btn = _ibtn("import_icon", "Import",        self._import_file)
        self.save_btn.setEnabled(False)
        for b in (self.open_btn, self.save_btn,
                  self.export_btn, self.import_btn):
            lo.addWidget(b)

        lo.addSpacing(6)

        self.undo_btn = _ibtn("undo_icon", "Undo  Ctrl+Z", self._undo)
        lo.addWidget(self.undo_btn)

        lo.addSpacing(4)

        # [ℹ] Info — About this workshop
        self.info_btn = _ibtn("info_icon", "About / Info", self._show_about)
        lo.addWidget(self.info_btn)

        # [⚙] Cog — Global AppSettings theme dialog
        self.properties_btn = _ibtn(
            "properties_icon",
            "Global Theme Settings  (AppSettings)",
            self._launch_theme_settings)
        lo.addWidget(self.properties_btn)

        lo.addSpacing(4)

        # [_] [⬜] [✕] — Window controls (standalone only)
        if self.standalone_mode:
            self.minimize_btn = _ibtn("minimize_icon", "Minimise",
                                      self.showMinimized)
            self.maximize_btn = _ibtn("maximize_icon", "Maximise / Restore",
                                      self._toggle_maximize)
            self.close_btn    = _ibtn("close_icon",    "Close",
                                      self.close)
            for b in (self.minimize_btn, self.maximize_btn, self.close_btn):
                lo.addWidget(b)
        else:
            self.dock_btn = QPushButton("D")
            self.dock_btn.setFixedSize(35, 35)
            self.dock_btn.setToolTip("Dock / Undock")
            self.dock_btn.clicked.connect(self.toggle_dock_mode)
            lo.addWidget(self.dock_btn)

        return self.toolbar

    #    Menu button handler                                                    

    def _on_menu_btn_clicked(self):
        """[Menu] button — dropdown or toggle top bar per settings."""
        if self.WS.get("menu_style", "dropdown") == "dropdown":
            self._show_dropdown_menu()
        else:
            on = not self.WS.get("show_menubar", False)
            self.WS.set("show_menubar", on); self.WS.save()
            if hasattr(self, "_menu_bar_container"):
                self._menu_bar_container.setVisible(on)

    def _show_dropdown_menu(self):
        """Pop up the workshop menus as a QMenu below the [Menu] button."""
        menu = QMenu(self)
        self._build_menus_into_qmenu(menu)
        btn = getattr(self, "menu_btn", None)
        pos = btn.mapToGlobal(btn.rect().bottomLeft()) if btn else self.cursor().pos()
        menu.exec(pos)

    def _show_popup_menu(self):   # compat alias
        self._show_dropdown_menu()

    #    [ℹ] Info — About dialog                                                

    def _show_about(self):
        """[ℹ] button — show About / Info for this workshop."""
        author = getattr(self, "App_author",      __author__)
        year   = getattr(self, "App_year",        __year__)
        desc   = getattr(self, "App_description", "")
        QMessageBox.information(self, f"About {self.App_name}",
            f"{self.App_name}   {self.App_build}\n\n"
            + (f"{desc}\n\n" if desc else "")
            + f"Copyright \u00a9 {year}  {author}\n"
              f"Part of IMG Factory 1.6 — a GTA modding toolkit.")

    #    [⚙] Cog — Global AppSettings theme dialog                             

    def _launch_theme_settings(self):
        """[⚙] Cog — opens the global AppSettings / SettingsDialog.
        Identical pattern to radar_workshop._launch_theme_settings.
        """
        try:
            if not APPSETTINGS_AVAILABLE:
                QMessageBox.information(self, "Theme",
                    "AppSettings not available in this environment.")
                return
            if not self.app_settings:
                self.app_settings = AppSettings()
            dialog = SettingsDialog(self.app_settings, self)
            dialog.themeChanged.connect(lambda _: self._apply_theme())
            if dialog.exec():
                self._apply_theme()
                self._refresh_icons()
        except Exception as e:
            QMessageBox.warning(self, "Theme Error",
                f"Could not open theme settings:\n{e}")

    #    [Settings] — Workshop-local settings dialog                            

    def _show_workshop_settings(self):
        """[Settings] button — workshop-local settings.
        Tabs: Fonts / Display / Menu / About
        Reads/writes WorkshopSettings (per-app JSON).
        """
        dlg = QDialog(self)
        dlg.setWindowTitle(f"{self.App_name} — Settings")
        dlg.setMinimumSize(520, 460)
        try:
            from apps.core.theme_utils import apply_dialog_theme
            apply_dialog_theme(dlg, self.main_window)
        except Exception:
            pass

        lo  = QVBoxLayout(dlg)
        tabs = QTabWidget()
        ws  = self.WS

        #    Tab 1: Fonts                                                   
        ft  = QWidget(); fl = QVBoxLayout(ft)

        def _font_row(label, fam_key, sz_key, def_fam, def_sz, mn=7, mx=32):
            grp = QGroupBox(label); row = QHBoxLayout(grp)
            fc = QFontComboBox()
            fc.setCurrentFont(__import__("PyQt6.QtGui", fromlist=["QFont"])
                              .QFont(ws.get(fam_key, def_fam)))
            sc = QSpinBox(); sc.setRange(mn, mx)
            sc.setValue(ws.get(sz_key, def_sz))
            sc.setSuffix(" pt"); sc.setFixedWidth(75)
            row.addWidget(fc); row.addWidget(sc)
            fl.addWidget(grp)
            return fc, sc

        fc_tit, sc_tit = _font_row("Title Font",
            "font_title_family",  "font_title_size",  "Arial",       14, 10, 32)
        fc_pan, sc_pan = _font_row("Panel / Header Font",
            "font_panel_family",  "font_panel_size",  "Arial",       10)
        fc_btn, sc_btn = _font_row("Button Font",
            "font_button_family", "font_button_size", "Arial",       10)
        fc_inf, sc_inf = _font_row("Info Bar Font",
            "font_info_family",   "font_info_size",   "Courier New",  9)
        fl.addStretch()
        tabs.addTab(ft, "Fonts")

        #    Tab 2: Display                                                 
        dt = QWidget(); dl = QVBoxLayout(dt)

        bm_grp = QGroupBox("Button Display Mode"); bm_lo = QVBoxLayout(bm_grp)
        bm_cb  = QComboBox()
        bm_cb.addItems(["Icons + Text", "Icons Only", "Text Only"])
        bm_cb.setCurrentIndex(
            {"both":0,"icons":1,"text":2}.get(ws.get("button_display_mode","both"),0))
        bm_lo.addWidget(bm_cb)
        bm_lo.addWidget(QLabel("Restart required to change button mode.",
                               styleSheet="color:#888;font-style:italic;"))
        dl.addWidget(bm_grp)

        sb_grp = QGroupBox("Status Bar"); sb_lo = QVBoxLayout(sb_grp)
        sb_chk = QCheckBox("Show status bar at bottom")
        sb_chk.setChecked(bool(ws.get("show_statusbar", True)))
        sb_lo.addWidget(sb_chk)
        dl.addWidget(sb_grp)

        sw_grp = QGroupBox("Sidebar"); sw_lo = QVBoxLayout(sw_grp)
        from PyQt6.QtWidgets import QFormLayout
        sw_form = QFormLayout()
        sw_spin = QSpinBox(); sw_spin.setRange(60,200)
        sw_spin.setValue(ws.get("sidebar_width", 82)); sw_spin.setSuffix(" px")
        sw_form.addRow("Sidebar width:", sw_spin)
        sw_lo.addLayout(sw_form)
        dl.addWidget(sw_grp)

        dl.addStretch()
        tabs.addTab(dt, "Display")

        #    Tab 3: Menu                                                    
        mt = QWidget(); ml = QVBoxLayout(mt)

        ms_grp = QGroupBox("Menu Style"); ms_lo = QVBoxLayout(ms_grp)
        ms_cb  = QComboBox()
        ms_cb.addItems(["Dropdown  ☰  (default)", "Top menu bar"])
        ms_cb.setCurrentIndex(
            0 if ws.get("menu_style","dropdown") == "dropdown" else 1)
        ms_lo.addWidget(ms_cb)
        ms_lo.addWidget(QLabel("Restart required to switch menu style.",
                               styleSheet="color:#888;font-style:italic;"))
        ml.addWidget(ms_grp)

        mf_grp = QGroupBox("Menu Font Size"); mf_lo = QFormLayout(mf_grp)
        mf_dd  = QSpinBox(); mf_dd.setRange(7,16)
        mf_dd.setValue(ws.get("menu_dropdown_font_size",9)); mf_dd.setSuffix(" pt")
        mf_lo.addRow("Dropdown font:", mf_dd)
        mf_bh  = QSpinBox(); mf_bh.setRange(18,40)
        mf_bh.setValue(ws.get("menu_bar_height",22)); mf_bh.setSuffix(" px")
        mf_lo.addRow("Bar height:", mf_bh)
        ml.addWidget(mf_grp)
        ml.addStretch()
        tabs.addTab(mt, "Menu")

        #    Tab 4: About                                                   
        at  = QWidget(); al = QVBoxLayout(at)
        atx = QTextEdit(); atx.setReadOnly(True)
        author = getattr(self, "App_author",      __author__)
        year   = getattr(self, "App_year",        __year__)
        desc   = getattr(self, "App_description", "GUIWorkshop — IMG Factory 1.6")
        atx.setHtml(
            f"<h2>{self.App_name}</h2>"
            f"<p><b>Build:</b> {self.App_build}</p>"
            f"<p>{desc}</p>"
            f"<hr>"
            f"<p>Copyright &copy; {year} <b>{author}</b></p>"
            f"<p>Part of <b>IMG Factory 1.6</b> — a GTA modding toolkit.</p>"
            f"<p style='color:#888;'>Not affiliated with Rockstar Games "
            f"or Take-Two Interactive.</p>")
        al.addWidget(atx)
        tabs.addTab(at, "About")

        #    Dialog buttons                                                 
        lo.addWidget(tabs)
        btns = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok |
            QDialogButtonBox.StandardButton.Cancel)
        btns.accepted.connect(dlg.accept)
        btns.rejected.connect(dlg.reject)
        lo.addWidget(btns)

        if dlg.exec() != QDialog.DialogCode.Accepted:
            return

        #    Save                                                          
        ws.set("font_title_family",        fc_tit.currentFont().family())
        ws.set("font_title_size",          sc_tit.value())
        ws.set("font_panel_family",        fc_pan.currentFont().family())
        ws.set("font_panel_size",          sc_pan.value())
        ws.set("font_button_family",       fc_btn.currentFont().family())
        ws.set("font_button_size",         sc_btn.value())
        ws.set("font_info_family",         fc_inf.currentFont().family())
        ws.set("font_info_size",           sc_inf.value())
        ws.set("button_display_mode",      ["both","icons","text"][bm_cb.currentIndex()])
        ws.set("show_statusbar",           sb_chk.isChecked())
        ws.set("sidebar_width",            sw_spin.value())
        ws.set("menu_style",               "dropdown" if ms_cb.currentIndex()==0 else "topbar")
        ws.set("menu_dropdown_font_size",  mf_dd.value())
        ws.set("menu_bar_height",          mf_bh.value())
        ws.save()

        # Live-apply without restart where possible
        self._load_fonts_from_settings()
        if hasattr(self, "title_label"):
            self.title_label.setFont(self.title_font)
        if hasattr(self, "_status_widget"):
            self._status_widget.setVisible(ws.get("show_statusbar", True))
        if hasattr(self, "_sidebar_frame"):
            self._sidebar_frame.setFixedWidth(ws.get("sidebar_width", 82))
        self._set_status("Settings saved.")

    #    Theme helpers                                                          

    def _get_icon_color(self) -> str:
        """Returns text_primary from current theme."""
        if APPSETTINGS_AVAILABLE and self.app_settings:
            try:
                return self.app_settings.get_theme_colors().get(
                    "text_primary", "#e0e0e0")
            except Exception:
                pass
        bg = self.palette().window().color()
        return "#e0e0e0" if bg.lightness() < 128 else "#202020"

    def _get_accent_color(self) -> str:
        """Returns accent_primary from current theme."""
        if APPSETTINGS_AVAILABLE and self.app_settings:
            try:
                return self.app_settings.get_theme_colors().get(
                    "accent_primary", "#4682FF")
            except Exception:
                pass
        return "#4682FF"

    def _apply_theme(self):
        """Apply QSS from AppSettings."""
        if self.app_settings:
            try:
                qss = self.app_settings.get_stylesheet()
                if qss: self.setStyleSheet(qss)
            except Exception:
                pass

    def _refresh_icons(self):
        """Called on theme change — re-apply theme and rebuild toolbar icons."""
        self._apply_theme()
        if hasattr(self, "_corner_overlay"):
            self._corner_overlay.update_state(self.hover_corner, self.app_settings)
        ic = self._get_icon_color()
        for btn_name, icon_fn in {
            "open_btn":       "open_icon",
            "save_btn":       "save_icon",
            "export_btn":     "export_icon",
            "import_btn":     "import_icon",
            "undo_btn":       "undo_icon",
            "info_btn":       "info_icon",
            "properties_btn": "properties_icon",
            "settings_btn":   "settings_icon",
        }.items():
            btn = getattr(self, btn_name, None)
            if btn:
                try: btn.setIcon(getattr(SVGIconFactory, icon_fn)(20, ic))
                except Exception: pass
        if self.standalone_mode:
            for btn_name, icon_fn in {
                "minimize_btn": "minimize_icon",
                "maximize_btn": "maximize_icon",
                "close_btn":    "close_icon",
            }.items():
                btn = getattr(self, btn_name, None)
                if btn:
                    try: btn.setIcon(getattr(SVGIconFactory, icon_fn)(20, ic))
                    except Exception: pass


#
# SECTION 3 — Layout: setup_ui, left panel, centre panel, right panel, status
#

class _LayoutMixin:
    """Panel creation and layout.
    Mixed into GUIWorkshop — not used standalone.
    Override any _create_* method in your subclass to replace that panel.
    """

    def setup_ui(self):
        """Main layout: toolbar / three-panel splitter / status bar."""
        ml = QVBoxLayout(self)
        ml.setContentsMargins(*self.get_content_margins())
        ml.setSpacing(self.setspacing)

        ml.addWidget(self._create_toolbar())

        sp = QSplitter(Qt.Orientation.Horizontal)
        sp.addWidget(self._create_left_panel())
        sp.addWidget(self._create_centre_panel())
        # Right button bar disabled — not needed for all workshops
        # sp.addWidget(self._create_right_panel())
        sp.setStretchFactor(0, 1)
        sp.setStretchFactor(1, 5)
        sp.setSizes([200, 950])
        self._main_splitter = sp
        ml.addWidget(sp)

        self._status_widget = self._create_status_bar()
        ml.addWidget(self._status_widget)
        self._status_widget.setVisible(self.WS.get("show_statusbar", True))

    def _create_left_panel(self):
        """Left panel — list + Add/Remove + info label.
        Override to replace with your own content.
        """
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        ll = QVBoxLayout(panel)
        ll.setContentsMargins(*self.get_panel_margins())

        hdr = QLabel("Items")
        hdr.setAlignment(Qt.AlignmentFlag.AlignCenter)
        hdr.setFont(self.panel_font)
        hdr.setStyleSheet("font-weight:bold; padding:2px;")
        ll.addWidget(hdr)

        self._item_list = QListWidget()
        self._item_list.setAlternatingRowColors(True)
        self._item_list.currentRowChanged.connect(
            self._on_list_selection_changed)
        ll.addWidget(self._item_list)

        br = QHBoxLayout()
        self._add_item_btn = QPushButton("+ Add")
        self._del_item_btn = QPushButton("− Remove")
        self._add_item_btn.clicked.connect(self._on_add_item)
        self._del_item_btn.clicked.connect(self._on_remove_item)
        br.addWidget(self._add_item_btn)
        br.addWidget(self._del_item_btn)
        ll.addLayout(br)

        sep = QFrame(); sep.setFrameShape(QFrame.Shape.HLine)
        ll.addWidget(sep)

        self._info_lbl = QLabel("No file loaded")
        self._info_lbl.setFont(self.infobar_font)
        ll.addWidget(self._info_lbl)

        return panel

    def _create_centre_panel(self):
        """Centre panel — tab view with placeholder tabs.
        Override to replace with your own canvas/tabs.
        """
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        cl = QVBoxLayout(panel)
        cl.setContentsMargins(0, 0, 0, 0)
        cl.setSpacing(0)

        self._view_tabs = QTabWidget()
        self._view_tabs.setDocumentMode(True)
        self._view_tabs.currentChanged.connect(self._on_tab_changed)

        for label in ("View A", "View B"):
            tab = QWidget()
            tl  = QVBoxLayout(tab)
            tl.addWidget(QLabel(
                f"[ {label} — override _create_centre_panel() ]",
                alignment=Qt.AlignmentFlag.AlignCenter))
            self._view_tabs.addTab(tab, label)

        cl.addWidget(self._view_tabs)
        return panel

    def _create_right_panel(self):
        """Right panel — sidebar with 2-col tool button grid.
        Override _populate_sidebar() to change tool buttons.
        """
        sidebar = QFrame()
        sidebar.setFrameStyle(QFrame.Shape.StyledPanel)
        sidebar.setFixedWidth(self.WS.get("sidebar_width", 82))
        sl = QVBoxLayout(sidebar)
        sl.setContentsMargins(2, 4, 2, 4)
        sl.setSpacing(2)
        self._sidebar_layout = sl
        self._sidebar_frame  = sidebar
        self._draw_btns      = {}

        self._populate_sidebar()

        sl.addStretch(0)
        return sidebar

    def _populate_sidebar(self):
        """Build the 2-col icon grid in the right sidebar.
        Override to change or extend the tool set.
        """
        sl = self._sidebar_layout
        ic = self._get_icon_color()
        BTN = 36

        def _nb(icon_fn, tip, slot, checkable=False):
            b = QToolButton(); b.setFixedSize(BTN, BTN)
            try: b.setIcon(getattr(SVGIconFactory, icon_fn)(20, ic))
            except Exception: b.setText(tip[:2])
            b.setToolTip(tip); b.setCheckable(checkable)
            b.clicked.connect(slot); return b

        def _row(*btns):
            row = QHBoxLayout()
            row.setSpacing(2); row.setContentsMargins(0, 0, 0, 0)
            for b in btns: row.addWidget(b)
            if len(btns) == 1: row.addStretch()
            sl.addLayout(row)

        def _sep():
            s = QFrame(); s.setFrameShape(QFrame.Shape.HLine)
            sl.addSpacing(2); sl.addWidget(s); sl.addSpacing(2)

        def _tool(icon_fn, tip, name):
            b = _nb(icon_fn, tip,
                    lambda checked=False, t=name: self._set_active_tool(t),
                    checkable=True)
            self._draw_btns[name] = b; return b

        # Row 1-2: View controls
        _row(_nb("zoom_in_icon",  "Zoom in  (+)",      lambda: self._zoom(1.25)),
             _nb("zoom_out_icon", "Zoom out  (-)",     lambda: self._zoom(0.8)))
        _row(_nb("fit_grid_icon", "Fit  Ctrl+0",       self._fit),
             _nb("locate_icon",   "Jump to selection", self._jump))
        _sep()

        # Rows 3-6: Draw tools (2 per row)
        _row(_tool("paint_icon",     "Pencil (P)",           "pencil"),
             _tool("fill_icon",      "Flood fill (F)",       "fill"))
        _row(_tool("line_icon",      "Line (L)",             "line"),
             _tool("rect_icon",      "Rect outline (R)",     "rect"))
        _row(_tool("rect_fill_icon", "Filled rect (Shift+R)","rect_fill"),
             _tool("dropper_icon",   "Colour picker (K)",    "picker"))
        _row(_tool("scissors_icon",  "Cut (X)",              "cut"),
             _tool("paste_brush_icon","Paste (V)",           "paste"))
        _row(_tool("zoom_in_icon",   "Zoom tool (Z)",        "zoom"),
             _nb("search_icon",      "Open in editor tab",
                 lambda: self._on_toolbar_action("edit")))
        _sep()

        # Rows 7-8: Transform tools
        _row(_nb("rotate_cw_icon",  "Rotate +90°",
                 lambda: self._on_toolbar_action("rotate_cw")),
             _nb("rotate_ccw_icon", "Rotate -90°",
                 lambda: self._on_toolbar_action("rotate_ccw")))
        _row(_nb("flip_horz_icon",  "Flip Horizontal",
                 lambda: self._on_toolbar_action("flip_h")),
             _nb("flip_vert_icon",  "Flip Vertical",
                 lambda: self._on_toolbar_action("flip_v")))

        if "pencil" in self._draw_btns:
            self._draw_btns["pencil"].setChecked(True)
            self._active_tool = "pencil"

    def _create_status_bar(self):
        """Status bar — single line at bottom, toggleable via settings."""
        self._status_bar = QLabel(
            f"Ready  |  {self.App_name}  {self.App_build}")
        self._status_bar.setFixedHeight(self.statusheight)
        self._status_bar.setFont(self.infobar_font)
        self._status_bar.setStyleSheet("padding:2px 6px;")
        return self._status_bar

    def _set_status(self, msg: str):
        if hasattr(self, "_status_bar"):
            self._status_bar.setText(msg)


#
# SECTION 4 — Logic stubs
# These are the methods your subclass overrides with actual app logic.
# Everything above this line is pure UI — do not put app logic there.
#

class _LogicStubsMixin:
    """Stub methods for subclass override.
    All return immediately or show a 'not implemented' status message.
    Replace these with your actual file format, drawing, and undo logic.
    """

    #    ToolMenuMixin protocol                                                 
    def get_menu_title(self) -> str:
        return self.App_name

    def _build_menus_into_qmenu(self, pm):
        """Override to populate File / Edit / View menus for your app."""
        fm = pm.addMenu("File")
        fm.addAction("Open…  Ctrl+O",  self._open_file)
        fm.addAction("Save…  Ctrl+S",  self._save_file)
        fm.addSeparator()
        fm.addAction("Export…",        self._export_file)
        fm.addAction("Import…",        self._import_file)
        fm.addSeparator()
        recent = self.WS.get_recent()
        if recent:
            rm = fm.addMenu("Recent Files")
            for rp in recent:
                act = rm.addAction(Path(rp).name); act.setToolTip(rp)
                act.triggered.connect(
                    lambda checked=False, p=rp: self._open_file(p))
            rm.addSeparator()
            rm.addAction("Clear Recent", self._clear_recent)
        em = pm.addMenu("Edit")
        em.addAction("Undo  Ctrl+Z",   self._undo)
        em.addAction("Redo  Ctrl+Y",   self._redo)
        vm = pm.addMenu("View")
        vm.addAction("Zoom In  +",     lambda: self._zoom(1.25))
        vm.addAction("Zoom Out  -",    lambda: self._zoom(0.8))
        vm.addAction("Fit  Ctrl+0",    self._fit)
        vm.addSeparator()
        vm.addAction("About " + self.App_name, self._show_about)

    #    File operations                                                        
    def _open_file(self, path=None):   pass   # override: load your format
    def _save_file(self):              pass   # override: save your format
    def _export_file(self):            pass   # override: export
    def _import_file(self):            pass   # override: import
    def _clear_recent(self):
        self.WS._data["recent_files"] = []; self.WS.save()
        self._set_status("Recent files cleared")

    #    Edit operations                                                        
    def _undo(self):         self._set_status("Undo — override in subclass")
    def _redo(self):         self._set_status("Redo — override in subclass")
    def _copy_item(self):    pass   # override: copy selection
    def _paste_item(self):   pass   # override: paste clipboard

    #    View operations                                                        
    def _zoom(self, factor: float): pass   # override: zoom your canvas
    def _fit(self):                 pass   # override: fit view
    def _jump(self):                pass   # override: jump to selection

    #    Panel callbacks                                                        
    def _on_list_selection_changed(self, row: int): pass
    def _on_tab_changed(self, idx: int):            pass
    def _on_add_item(self):
        self._item_list.addItem(
            QListWidgetItem(f"Item {self._item_list.count()}"))
    def _on_remove_item(self):
        row = self._item_list.currentRow()
        if row >= 0: self._item_list.takeItem(row)

    #    Toolbar actions                                                        
    def _on_toolbar_action(self, action: str): pass  # rotate/flip/edit etc.

    #    Tool management                                                        
    def _set_active_tool(self, tool: str):
        self._active_tool = tool
        for name, btn in self._draw_btns.items():
            btn.setChecked(name == tool)


#
# GUIWorkshop — assembles all four sections
#

class GUIWorkshop(_ToolbarMixin, _LayoutMixin, _LogicStubsMixin,
                  ToolMenuMixin, QWidget):
    """Reusable workshop base.  Subclass this, override App_name/config_key
    and the stubs in Section 4.  All chrome, theme, settings, and window
    management are inherited from the four sections above.
    """

    #    Subclass identity — OVERRIDE ALL OF THESE                              
    App_name        = "Workshop"
    App_build       = "Build 1"
    App_author      = "X-Seti"
    App_year        = "2026"
    App_description = "GUIWorkshop base template — IMG Factory 1.6"
    config_key      = "gui_workshop"

    #    Signals                                                                
    workshop_closed = pyqtSignal()
    window_closed   = pyqtSignal()

    #    Init                                                                   
    def __init__(self, parent=None, main_window=None):
        super().__init__(parent)
        self.main_window     = main_window
        self.standalone_mode = (main_window is None)
        self.is_docked       = not self.standalone_mode
        self.dock_widget     = None

        # Fonts (loaded from settings below)
        self.title_font   = QFont("Arial", 14)
        self.panel_font   = QFont("Arial", 10)
        self.button_font  = QFont("Arial", 10)
        self.infobar_font = QFont("Courier New", 9)
        self.button_display_mode = "both"

        # Margins / spacing (consistent across all workshops)
        self.contmergina = 1; self.contmerginb = 1
        self.contmerginc = 1; self.contmergind = 1; self.setspacing = 2
        self.panelmergina = 5; self.panelmerginb = 5
        self.panelmerginc = 5; self.panelmergind = 5
        self.toolbarheight = 50; self.statusheight = 22

        # Window chrome state
        self.dragging         = False; self.drag_position    = None
        self.resizing         = False; self.resize_corner    = None
        self.initial_geometry = None;  self.corner_size      = 20
        self.hover_corner     = None

        # AppSettings (global theme)
        if main_window and hasattr(main_window, "app_settings"):
            self.app_settings = main_window.app_settings
        elif APPSETTINGS_AVAILABLE:
            try:    self.app_settings = AppSettings()
            except Exception: self.app_settings = None
        else:
            self.app_settings = None

        if self.app_settings and hasattr(self.app_settings, "theme_changed"):
            self.app_settings.theme_changed.connect(self._refresh_icons)

        # Per-app settings
        self.WS = WorkshopSettings(self.config_key)
        if self.standalone_mode:
            self.resize(max(800, self.WS.get("window_w", 1400)),
                        max(500, self.WS.get("window_h",  800)))
            wx, wy = self.WS.get("window_x", -1), self.WS.get("window_y", -1)
            if wx >= 0 and wy >= 0: self.move(wx, wy)

        self._load_fonts_from_settings()
        self.icon_factory = SVGIconFactory()
        self.setWindowTitle(self.App_name)
        self.setMinimumSize(800, 500)
        self._active_tool = "pencil"
        self._draw_btns   = {}

        if self.standalone_mode:
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint)
        else:
            self.setWindowFlags(Qt.WindowType.Widget)

        if parent:
            p = parent.pos(); self.move(p.x() + 50, p.y() + 80)

        if not getattr(self, '_defer_setup_ui', False):
            self.setup_ui()
        self._setup_shortcuts()
        self._apply_theme()

    def _load_fonts_from_settings(self):
        ws = self.WS
        self.title_font   = QFont(ws.get("font_title_family",  "Arial"),
                                  ws.get("font_title_size",     14))
        self.panel_font   = QFont(ws.get("font_panel_family",  "Arial"),
                                  ws.get("font_panel_size",     10))
        self.button_font  = QFont(ws.get("font_button_family", "Arial"),
                                  ws.get("font_button_size",    10))
        self.infobar_font = QFont(ws.get("font_info_family",   "Courier New"),
                                  ws.get("font_info_size",       9))
        self.button_display_mode = ws.get("button_display_mode", "both")

    def get_content_margins(self):
        return (self.contmergina, self.contmerginb,
                self.contmerginc, self.contmergind)

    def get_panel_margins(self):
        return (self.panelmergina, self.panelmerginb,
                self.panelmerginc, self.panelmergind)

    def _setup_shortcuts(self):
        for key, fn in [("Ctrl+O", self._open_file), ("Ctrl+S", self._save_file),
                        ("Ctrl+Z", self._undo), ("Ctrl+Y", self._redo),
                        ("Ctrl+Shift+Z", self._redo), ("Ctrl+0", self._fit),
                        ("Ctrl+C", self._copy_item), ("Ctrl+V", self._paste_item)]:
            QShortcut(QKeySequence(key), self).activated.connect(fn)
        for key, tool in [("P","pencil"),("F","fill"),("L","line"),("R","rect"),
                          ("K","picker"),("Z","zoom"),("X","cut"),("V","paste")]:
            QShortcut(QKeySequence(key), self).activated.connect(
                lambda t=tool: self._set_active_tool(t))
        QShortcut(QKeySequence("Shift+R"), self).activated.connect(
            lambda: self._set_active_tool("rect_fill"))

    #    Window chrome                                                          
    def showEvent(self, ev):
        super().showEvent(ev)
        if not hasattr(self, "_corner_overlay"):
            self._corner_overlay = _CornerOverlay(self)
            self._corner_overlay.update_state(None, self.app_settings)
        self._corner_overlay.setGeometry(0, 0, self.width(), self.height())
        self._corner_overlay.raise_()
        self._corner_overlay.show()

    def resizeEvent(self, ev):
        super().resizeEvent(ev)
        if hasattr(self, "_corner_overlay"):
            self._corner_overlay.setGeometry(0, 0, self.width(), self.height())

    def _get_resize_corner(self, pos):
        s = self.corner_size; x, y = pos.x(), pos.y()
        w, h = self.width(), self.height()
        if x < s and y < s:    return "top-left"
        if x > w-s and y < s:  return "top-right"
        if x < s and y > h-s:  return "bottom-left"
        if x > w-s and y > h-s: return "bottom-right"
        return None

    def _update_cursor(self, corner):
        c = {"top-left": Qt.CursorShape.SizeFDiagCursor,
             "top-right": Qt.CursorShape.SizeBDiagCursor,
             "bottom-left": Qt.CursorShape.SizeBDiagCursor,
             "bottom-right": Qt.CursorShape.SizeFDiagCursor}
        self.setCursor(c.get(corner, Qt.CursorShape.ArrowCursor))

    def _handle_corner_resize(self, global_pos):
        if not self.resize_corner or not self.drag_position: return
        delta = global_pos - self.drag_position
        g = self.initial_geometry; dx, dy = delta.x(), delta.y()
        if "right"  in self.resize_corner: self.resize(max(800,g.width()+dx), self.height())
        if "bottom" in self.resize_corner: self.resize(self.width(), max(500,g.height()+dy))
        if "left"   in self.resize_corner:
            self.setGeometry(g.x()+dx, g.y(), max(800,g.width()-dx), g.height())
        if "top"    in self.resize_corner:
            self.setGeometry(g.x(), g.y()+dy, g.width(), max(500,g.height()-dy))

    def mousePressEvent(self, ev):
        if ev.button() != Qt.MouseButton.LeftButton:
            super().mousePressEvent(ev); return
        self.resize_corner = self._get_resize_corner(ev.pos())
        if self.resize_corner:
            self.resizing = True
            self.drag_position = ev.globalPosition().toPoint()
            self.initial_geometry = self.geometry()
            ev.accept(); return
        if (hasattr(self, "titlebar") and
                self.titlebar.geometry().contains(ev.pos())):
            handle = self.windowHandle()
            if handle: handle.startSystemMove()
            ev.accept(); return
        super().mousePressEvent(ev)

    def mouseMoveEvent(self, ev):
        if ev.buttons() == Qt.MouseButton.LeftButton:
            if self.resizing and self.resize_corner:
                self._handle_corner_resize(ev.globalPosition().toPoint())
                ev.accept(); return
        else:
            corner = self._get_resize_corner(ev.pos())
            if corner != self.hover_corner:
                self.hover_corner = corner
                if hasattr(self, "_corner_overlay"):
                    self._corner_overlay.update_state(corner, self.app_settings)
            self._update_cursor(corner)
        super().mouseMoveEvent(ev)

    def mouseReleaseEvent(self, ev):
        self.dragging = False; self.resizing = False; self.resize_corner = None
        self.setCursor(Qt.CursorShape.ArrowCursor); ev.accept()

    def _toggle_maximize(self):
        if self.isMaximized(): self.showNormal()
        else: self.showMaximized()

    def toggle_dock_mode(self): pass  # override if dock support needed

    def closeEvent(self, ev):
        if self.standalone_mode:
            g = self.geometry()
            self.WS.set("window_x", g.x()); self.WS.set("window_y", g.y())
            self.WS.set("window_w", g.width()); self.WS.set("window_h", g.height())
            self.WS.save()
        self.workshop_closed.emit(); self.window_closed.emit()
        super().closeEvent(ev)


#                                                                              
# Standalone launcher
#                                                                              

if __name__ == "__main__":
    import traceback
    print("GUIWorkshop template — standalone demo")
    try:
        app = QApplication(sys.argv)
        w = GUIWorkshop()
        w.setWindowTitle("GUIWorkshop — Template Demo")
        w.resize(1300, 800)
        w.show()
        sys.exit(app.exec())
    except Exception as e:
        print(f"ERROR: {e}"); traceback.print_exc(); sys.exit(1)

class HexWorkshop(GUIWorkshop):  #vers 1
    App_name   = App_name
    App_build  = App_build
    App_auth   = "X-Seti"
    config_key = config_key

    def __init__(self, parent=None, main_window=None):  #vers 1
        self._defer_setup_ui  = True
        super().__init__(parent)
        self.main_window      = main_window
        self._file_path: Optional[str] = None
        self._data:      bytes         = b''
        self._modified   = False
        self.setup_ui()
        self._set_status("Open a file to begin (File → Open…)")

    def setup_ui(self):  #vers 2
        super().setup_ui()

    def _create_centre_panel(self):  #vers 1
        panel = QFrame(); panel.setFrameStyle(QFrame.Shape.StyledPanel)
        cl = QVBoxLayout(panel); cl.setContentsMargins(0,0,0,0)
        # Goto offset bar
        goto_widget = QWidget()
        gh = QHBoxLayout(goto_widget); gh.setContentsMargins(4,2,4,2)
        gh.addWidget(QLabel("Go to offset:"))
        self._goto_input = QLineEdit(); self._goto_input.setPlaceholderText("0x0000")
        self._goto_input.setMaximumWidth(110); self._goto_input.setFixedHeight(22)
        self._goto_input.returnPressed.connect(self._goto_offset)
        gh.addWidget(self._goto_input)
        gh.addStretch()
        self._file_size_lbl = QLabel(""); gh.addWidget(self._file_size_lbl)
        cl.addWidget(goto_widget)
        self._hex_view = HexViewWidget()
        cl.addWidget(self._hex_view, 1)
        return panel

    def _build_left_panel(self, parent):  #vers 1
        """Left: file info + offset search"""
        panel = QFrame(parent); panel.setFrameStyle(QFrame.Shape.StyledPanel)
        ll = QVBoxLayout(panel); ll.setContentsMargins(4,4,4,4); ll.setSpacing(4)
        ll.addWidget(QLabel("File Info"))
        self._info_lbl = QLabel("No file loaded")
        self._info_lbl.setFont(QFont("Courier New", 9))
        self._info_lbl.setWordWrap(True)
        ll.addWidget(self._info_lbl)
        ll.addStretch()
        return panel

    def _build_centre_panel(self, parent):  #vers 1
        """Centre: 3-panel hex view"""
        panel = QFrame(parent); panel.setFrameStyle(QFrame.Shape.StyledPanel)
        cl = QVBoxLayout(panel); cl.setContentsMargins(0,0,0,0)
        self._hex_view = HexViewWidget()
        cl.addWidget(self._hex_view, 1)
        return panel

    def _build_right_panel(self, parent=None):  #vers 1
        """Right: structure / parse view"""
        panel = QFrame(); panel.setFrameStyle(QFrame.Shape.StyledPanel)
        rl = QVBoxLayout(panel); rl.setContentsMargins(0,0,0,0)
        self._struct_view = StructureView()
        self._struct_view.goto_requested.connect(self._hex_view.goto_offset)
        rl.addWidget(self._struct_view, 1)
        return panel

    def load_file(self, path: str):  #vers 1
        try:
            with open(path, 'rb') as f:
                self._data = f.read()
            self._file_path = path
            self._hex_view.load_data(self._data)
            # Auto-parse DFF/TXD/COL structures
            ext = os.path.splitext(path)[1].lower()
            if ext in ('.dff', '.txd'):
                self._struct_view.parse_dff(self._data)
            self._info_lbl.setText(
                f"File: {os.path.basename(path)}\n"
                f"Size: {len(self._data):,} bytes\n"
                f"Path: {path}")
            self.setWindowTitle(f"Hex Workshop — {os.path.basename(path)}")
            self._set_status(f"Loaded: {os.path.basename(path)}  ({len(self._data):,} bytes)")
            if hasattr(self, '_file_size_lbl'):
                self._file_size_lbl.setText(f"{len(self._data):,} bytes")
            self._modified = False
        except Exception as ex:
            QMessageBox.critical(self, "Error", str(ex))

    def _open_file(self, path: str = None):  #vers 1
        if path is None:
            path, _ = QFileDialog.getOpenFileName(
                self, "Open File", "",
                "All GTA files (*.dff *.txd *.col *.img *.dat *.ipl *.ide *.bin);;"
                "All files (*)")
        if path:
            self.load_file(path)

    def _save_file(self):  #vers 1
        if not self._file_path:
            path, _ = QFileDialog.getSaveFileName(self, "Save As", "", "All files (*)")
            if not path: return
            self._file_path = path
        try:
            with open(self._file_path, 'wb') as f:
                f.write(self._hex_view._data)
            self._set_status(f"Saved: {os.path.basename(self._file_path)}")
            self._modified = False
        except Exception as ex:
            QMessageBox.critical(self, "Save Error", str(ex))

    def _goto_offset(self):  #vers 1
        text = self._goto_input.text().strip()
        try:
            offset = int(text, 16) if text.startswith('0x') else int(text)
            self._hex_view.goto_offset(offset)
            self._struct_view.goto_offset(offset)
        except ValueError:
            self._set_status(f"Invalid offset: {text}")

    def _build_menus_into_qmenu(self, pm):  #vers 1
        fm = pm.addMenu("File")
        fm.addAction("Open…",    self._open_file)
        fm.addAction("Save",     self._save_file)
        fm.addSeparator()
        fm.addAction("Close",    self.close)
        vm = pm.addMenu("View")
        vm.addAction("Go to offset…", lambda: self._goto_input.setFocus())


# ─────────────────────────────────────────────────────────────────────────────
# Backward compat helpers (used by imgfactory + right_click_actions)
# ─────────────────────────────────────────────────────────────────────────────

def show_hex_editor_for_file(main_window, file_path, entry_info=None):  #vers 1
    w = HexWorkshop(main_window=main_window)
    w.resize(1200, 800); w.show()
    w.load_file(file_path)
    return w

def show_hex_editor_for_entry(main_window, row, entry_info):  #vers 1
    """Open hex editor for an IMG entry (extracts to temp file)."""
    try:
        img = getattr(main_window, 'current_img', None)
        if not img: return None
        entry = img.entries[row] if hasattr(img, 'entries') else None
        if not entry: return None
        data = img.read_entry(entry) if hasattr(img, 'read_entry') else b''
        with tempfile.NamedTemporaryFile(
                suffix=os.path.splitext(entry.name)[1],
                prefix=os.path.splitext(entry.name)[0]+'_',
                delete=False) as tf:
            tf.write(data); tmp_path = tf.name
        w = HexWorkshop(main_window=main_window)
        w.resize(1200, 800); w.show()
        w.load_file(tmp_path)
        return w
    except Exception as ex:
        print(f"show_hex_editor_for_entry: {ex}"); return None

def open_hex_workshop(main_window=None, file_path=None):  #vers 1
    app = QApplication.instance() or QApplication(sys.argv)
    w = HexWorkshop(main_window=main_window)
    w.resize(1200, 800); w.show()
    if file_path: w.load_file(file_path)
    return w


if __name__ == "__main__":
    app = QApplication(sys.argv)
    path = sys.argv[1] if len(sys.argv) > 1 else None
    w = open_hex_workshop(file_path=path)
    sys.exit(app.exec())


# ── GUIWorkshop (inlined) ────────────────────────────────────────────────────
# GUIWorkshop — TEMPLATE ONLY. Copy into your workshop, do not import.
#
# ┌─────────────────────────────────────────────────────────────────┐
# │ !! WARNING — DO NOT IMPORT THIS FILE INTO YOUR WORKSHOP !!      │
# │                                                                 │
# │ WRONG:  from apps.components.Tmp_Template.gui_workshop import   │
# │         GUIWorkshop                                             │
# │                                                                 │
# │ RIGHT:  Copy this file into your workshop folder and rename it  │
# │         e.g. apps/components/My_Workshop/my_workshop.py         │
# │         Then edit your copy in place.                           │
# │                                                                 │
# │ Each workshop MUST be standalone and self-contained.            │
# │ Importing this file creates a hard dependency that breaks       │
# │ when the template changes, causes setup_ui() timing issues,     │
# │ and makes workshops impossible to run independently.            │
# └─────────────────────────────────────────────────────────────────┘
#
# HOW TO CREATE A NEW WORKSHOP:
# 1. Copy Tmp_Template/ to apps/components/My_Workshop/
# 2. Rename temp_workshop.py → my_workshop.py
# 3. Edit the copy — change App_name, config_key, override stubs
# 4. Never import from Tmp_Template again
#
# ┌                                                                 ┐
# │ SECTION 1 │ GUI Core — imports, WorkshopSettings, _CornerOverlay│
# │ SECTION 2 │ Toolbar — Menu, Settings UI, Info [i], Cog [⚙]     │
# │ SECTION 3 │ Layout  — setup_ui, left, centre, right, statusbar  │
# │ SECTION 4 │ Logic   — stubs to override in your subclass        │
# └                                                                 ┘
#
# If your workshop needs state before setup_ui() runs, use this pattern:
#   def __init__(self, ...):
#       self._defer_setup_ui = True   # stops auto-call in __init__
#       super().__init__(...)          # base state initialised
#       # ... set up your own state here ...
#       self.setup_ui()               # call manually when ready
#           self.setup_ui()              # call manually when ready

import sys, json
from pathlib import Path

from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QSplitter, QLabel, QToolButton,
    QPushButton, QFrame, QSizePolicy, QListWidget, QListWidgetItem,
    QFileDialog, QMessageBox, QTabWidget, QDialog, QApplication,
    QSpinBox, QGroupBox, QComboBox, QCheckBox, QFontComboBox,
    QScrollArea, QMenu, QDialogButtonBox, QTextEdit
)
from PyQt6.QtGui import (
    QColor, QPainter, QPen, QFont, QIcon, QKeySequence,
    QShortcut, QPolygon
)
from PyQt6.QtCore import Qt, QSize, QPoint, pyqtSignal


#
# SECTION 1 — GUI Core
# Imports, optional deps, WorkshopSettings, _CornerOverlay
#

APPSETTINGS_AVAILABLE = False
try:
    from apps.utils.app_settings_system import AppSettings, SettingsDialog
    APPSETTINGS_AVAILABLE = True
except ImportError:
    AppSettings = SettingsDialog = None



try:
    from apps.methods.imgfactory_svg_icons import SVGIconFactory
except ImportError:
    class SVGIconFactory:
        @staticmethod
        def _s(sz=20, c=None): return QIcon()
        open_icon = save_icon = export_icon = import_icon = delete_icon = \
        undo_icon = info_icon = properties_icon = minimize_icon = \
        maximize_icon = close_icon = settings_icon = search_icon = \
        zoom_in_icon = zoom_out_icon = fit_grid_icon = locate_icon = \
        paint_icon = fill_icon = dropper_icon = line_icon = rect_icon = \
        rect_fill_icon = scissors_icon = paste_brush_icon = \
        rotate_cw_icon = rotate_ccw_icon = flip_horz_icon = \
        flip_vert_icon = folder_icon = staticmethod(_s)



# Module-level identity defaults (override via class attributes in subclass)
__author__  = "X-Seti"
__year__    = "2026"


#    WorkshopSettings                                                          

