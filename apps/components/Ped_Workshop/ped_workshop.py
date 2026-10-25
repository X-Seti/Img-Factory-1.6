#!/usr/bin/env python3
#this belongs in apps/components/Ped_Workshop/ped_workshop.py - Version: 20
# X-Seti - October 21 2026 - IMG Factory 1.6 - Ped Workshop

"""
Ped Workshop - Model Workshop with a Peds dock (peds IDE, ped groups,
ped types) and an Animation dock (IFP playback, stick-man bones,
stop-motion keyframe editing) for GTA III, VC and SA.
"""

##Methods list -
# open_ped_workshop
#
##class PedWorkshop: -
# _cycle_ped_view
# _default_ped_docks
# _extra_settings_tabs
# _load_ped_prefs
# _ped_dirty_items
# _ped_grid_cfg
# _ped_grid_custom
# _ped_open_menu
# _ped_save_all
# _ribbon_config_path
# _ped_resize_textures
# _ped_set_grid
# _save_ped_prefs
# _show_dff
# _titlebar_title
# closeEvent
# setup_ui

import os
import sys

if sys.platform.startswith('linux'):           # NVIDIA on Wayland: X11 / GLX backend
    os.environ['QT_QPA_PLATFORM'] = 'xcb'
    os.environ['QSG_RHI_BACKEND'] = 'opengl'
_root = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
if _root not in sys.path:                      # standalone: make 'apps' importable
    sys.path.insert(0, _root)

from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import QApplication

from apps.components.Model_Editor.model_workshop import ModelWorkshop
from apps.components.Ped_Workshop.depends.ped_anim_func import PedAnimMixin
from apps.components.Ped_Workshop.depends.ped_anim_library_func import PedAnimLibraryMixin
from apps.components.Ped_Workshop.depends.ped_data_func import PedDataMixin
from apps.components.Ped_Workshop.depends.ped_iv_func import PedIVMixin
from apps.components.Ped_Workshop.depends.ped_pad_func import PedPadMixin
from apps.components.Ped_Workshop.depends.ped_skin_func import PedSkinMixin
from apps.methods.img_factory_settings import get_user_config_dir

App_name = "Ped Workshop"
GRID_PATTERNS = {'squares': ('lines', "Squares"), 'hex': ('honeycomb', "Hex"),     # key: (viewport type, label)
                 'solid': ('squares', "Solid"), 'checker': ('checker', "Checkerboard")}


class PedWorkshop(PedDataMixin, PedIVMixin, PedAnimMixin, PedAnimLibraryMixin, PedPadMixin, PedSkinMixin, ModelWorkshop): #vers 6
    """Model Workshop plus ped data and animation docks."""
    _OUTER_LAYOUT_VERSION = 13           # own dock layout (Peds / Animation / Bones)

    def _ribbon_config_path(self): #vers 1
        """Own ribbon and dock layout file."""
        return get_user_config_dir() / 'ped_workshop.json'

    def _default_ped_docks(self, docks): #vers 2
        """No saved layout: Peds 42 %, Animation 30 %, Bones 28 % of the right column."""
        import json
        try:
            data = json.loads(self._ribbon_config_path().read_text())
            if data.get('outer_layout_version') == self._OUTER_LAYOUT_VERSION:
                return
        except Exception:
            pass
        h = max(400, self._outer_mw.height())
        self._outer_mw.resizeDocks(list(docks), [int(h * 0.42), int(h * 0.3), int(h * 0.28)],
                                   Qt.Orientation.Vertical)

    _PED_VIEWS = (("Front", 180), ("Right", 270), ("Back", 0), ("Left", 90))   # (name, yaw)

    def _cycle_ped_view(self): #vers 1
        """Ped view button: Front, Right, Back, Left in turn (level camera)."""
        self._ped_view = (getattr(self, '_ped_view', -1) + 1) % len(self._PED_VIEWS)
        name, yaw = self._PED_VIEWS[self._ped_view]
        vp = self.preview_widget
        vp._yaw, vp._pitch = yaw, 90
        vp.update()
        nxt = self._PED_VIEWS[(self._ped_view + 1) % len(self._PED_VIEWS)][0]
        self._ped_view_act.setToolTip(f"Ped view: {name} (click for {nxt})")
        self._set_status(f"View: {name}")

    def _ped_resize_textures(self, px: int): #vers 1
        """Scale the loaded ped textures down so the longest side is px (smooth)."""
        from PyQt6.QtCore import Qt as _Qt
        from PyQt6.QtGui import QImage
        texs = getattr(self, '_mod_textures', None) or []
        done = 0
        for t in texs:
            w, h = t.get('width', 0), t.get('height', 0)
            if not t.get('rgba_data') or max(w, h) <= px:
                continue
            f = px / max(w, h)
            nw, nh = max(1, int(w * f)), max(1, int(h * f))
            img = QImage(bytes(t['rgba_data']), w, h, w * 4, QImage.Format.Format_RGBA8888)
            img = img.scaled(nw, nh, _Qt.AspectRatioMode.IgnoreAspectRatio,
                             _Qt.TransformationMode.SmoothTransformation).convertToFormat(QImage.Format.Format_RGBA8888)
            t['rgba_data'] = bytes(img.constBits().asstring(img.sizeInBytes()))
            t['width'], t['height'] = nw, nh
            t['compressed_data'], t['mipmap_levels'] = b'', []     # re-encoded on save
            t.pop('dxt_levels', None)
            if t.get('format') in ('PAL8', 'PAL4'):
                t['format'] = 'DXT1'
            done += 1
        if not done:
            self._set_status(f"Textures: nothing larger than {px} px")
            return
        self._populate_texture_list()
        vp = self.preview_widget
        vp._upload_textures(texs)
        vp.update()
        self._set_status(f"Textures: {done} scaled to {px} px - Save TXD to keep")

    def _ped_set_grid(self, kind: str, path: str = ''): #vers 2
        """Grid style preset or 'custom' (Settings Grid tab); saved in the ped prefs."""
        from PyQt6.QtWidgets import QFileDialog
        vp = self.preview_widget
        if kind == 'custom':
            self._ped_grid_custom(self._ped_grid_cfg())
            self._save_ped_prefs(grid=dict(self._ped_grid_cfg(), kind='custom', path=''))
            self._set_status("Grid: custom")
            return
        if kind == 'texture' and not path:
            path, _ = QFileDialog.getOpenFileName(self, "Grid texture", '',
                                                  "Images (*.png *.jpg *.jpeg *.bmp *.tga)")
            if not path:
                return
        styles = {'lines': ('lines', (40, 40, 60), (76, 76, 102)),
                  'holodeck': ('lines', (0, 0, 0), (255, 200, 40)),
                  'checker': ('checker', (60, 60, 72), (118, 118, 132)),
                  'honeycomb': ('honeycomb', (40, 40, 60), (76, 76, 102)),
                  'texture': ('squares', (40, 40, 60), (76, 76, 102)),
                  'none': ('none', (40, 40, 60), (76, 76, 102))}
        gtype, fill, line = styles.get(kind, styles['lines'])
        vp.set_grid_colors(fill, line)
        vp.set_squares_fill('texture' if kind == 'texture' else 'color', path=path or None, tile_size=2.0)
        vp.set_squares_fill_alpha(0.12)
        vp.set_grid_show_lines(True)
        vp.set_grid_line_size(4)
        vp.set_grid_type(gtype)
        prev = self._load_ped_prefs().get('grid') or {}
        keep = {k: prev[k] for k in ('pattern', 'fill', 'line', 'width') if k in prev}
        self._save_ped_prefs(grid=dict(keep, kind=kind, path=path))   # custom values kept
        self._set_status(f"Grid: {kind}")

    def _ped_grid_cfg(self) -> dict: #vers 1
        """Saved custom grid: pattern, fill, line colour, width."""
        cfg = {'pattern': 'squares', 'fill': [40, 40, 60], 'line': [76, 76, 102], 'width': 1}
        saved = self._load_ped_prefs().get('grid') or {}
        cfg.update({k: saved[k] for k in cfg if k in saved})
        return cfg

    def _ped_grid_custom(self, cfg: dict): #vers 1
        """Apply custom grid pattern, colours and thickness to the viewport."""
        vp = self.preview_widget
        pat = cfg.get('pattern', 'squares')
        vp.set_grid_colors(tuple(cfg['fill']), tuple(cfg['line']))
        vp.set_squares_fill('color')
        vp.set_grid_line_size(int(cfg.get('width', 1)))
        vp.set_squares_fill_alpha(0.9 if pat == 'solid' else 0.12)
        vp.set_grid_show_lines(pat != 'solid')
        vp.set_grid_type(GRID_PATTERNS.get(pat, ('lines', ''))[0])

    def _extra_settings_tabs(self, tabs): #vers 1
        """Settings Grid tab: pattern, line / fill colour, thickness (live)."""
        from PyQt6.QtGui import QColor
        from PyQt6.QtWidgets import QColorDialog, QComboBox, QFormLayout, QPushButton, QSpinBox, QWidget
        cfg = self._ped_grid_cfg()
        page = QWidget()
        form = QFormLayout(page)
        combo = QComboBox()
        for key, (_t, label) in GRID_PATTERNS.items():
            combo.addItem(label, key)
        combo.setCurrentIndex(max(0, combo.findData(cfg['pattern'])))
        spin = QSpinBox()
        spin.setRange(1, 10)
        spin.setSuffix(" px")
        spin.setValue(int(cfg['width']))

        def apply():
            cfg['pattern'], cfg['width'] = combo.currentData(), spin.value()
            self._ped_grid_custom(cfg)
            self._save_ped_prefs(grid=dict(cfg, kind='custom', path=''))

        def colour_btn(key, title):
            btn = QPushButton()
            btn.setToolTip(f"Grid {title.lower()} colour")

            def paint():
                r, g, b = cfg[key]
                btn.setText(f"#{r:02X}{g:02X}{b:02X}")
                btn.setStyleSheet(f"background: rgb({r},{g},{b}); color: {'#000' if r + g + b > 380 else '#FFF'};")

            def pick():
                c = QColorDialog.getColor(QColor(*cfg[key]), self, f"Grid {title}")
                if c.isValid():
                    cfg[key] = [c.red(), c.green(), c.blue()]
                    paint()
                    apply()
            paint()
            btn.clicked.connect(pick)
            return btn

        combo.currentIndexChanged.connect(apply)
        spin.valueChanged.connect(apply)
        form.addRow("Pattern:", combo)
        form.addRow("Line colour:", colour_btn('line', "Line"))
        form.addRow("Fill colour:", colour_btn('fill', "Fill"))
        form.addRow("Thickness:", spin)
        tabs.addTab(page, "Grid")

    def _titlebar_title(self): #vers 1
        return App_name

    def _load_ped_prefs(self) -> dict: #vers 1
        import json
        try:
            return json.loads(self._ribbon_config_path().read_text()).get('ped_prefs', {})
        except Exception:
            return {}

    def _save_ped_prefs(self, **values): #vers 1
        """Merge values into ped_prefs of ped_workshop.json."""
        import json
        path = self._ribbon_config_path()
        try:
            data = json.loads(path.read_text()) if path.exists() else {}
        except Exception:
            data = {}
        data.setdefault('ped_prefs', {}).update(values)
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(data, indent=2))

    def _ped_open_menu(self): #vers 1
        """Open: game folder (remembered), IFP, or a single DFF / TXD."""
        from PyQt6.QtWidgets import QMenu
        from PyQt6.QtGui import QCursor
        m = QMenu(self)
        m.addAction("Game folder...", lambda: self._open_ped_root(''))
        recent = self._load_ped_prefs().get('recent_roots', [])
        if recent:
            sub = m.addMenu("Recent game folders")
            for r in recent:
                sub.addAction(r, lambda p=r: self._open_ped_root(p))
        m.addSeparator()
        m.addAction("IFP animation file...", lambda: self._anim_open_ifp(''))
        m.addAction("Model / TXD file...", self._open_file)
        m.exec(QCursor.pos())

    def _ped_dirty_items(self) -> list: #vers 2
        """Names of unsaved items."""
        out = []
        if getattr(self, '_ped_ide_dirty', False):
            out.append("peds IDE")
        if getattr(self, '_ped_file_shown', None) and self._ped_file_edit.document().isModified():
            out.append(os.path.basename(self._ped_file_shown))
        if getattr(self, '_ifp_dirty', False):
            out.append(f"{self._ifp.name}.ifp" if self._ifp else "IFP")
        if getattr(self, '_skin_dirty', False):
            out.append("skin weights")
        return out

    def _ped_save_all(self): #vers 2
        """Save every changed item: peds IDE, data file, IFP."""
        items = self._ped_dirty_items()
        if not items:
            self._set_status("Ped Workshop: nothing to save")
            return
        if getattr(self, '_ped_ide_dirty', False):
            self._save_ped_ide()
        if getattr(self, '_ped_file_shown', None) and self._ped_file_edit.document().isModified():
            self._save_ped_text(self._ped_file_shown)
        if getattr(self, '_ifp_dirty', False):
            self._anim_save_ifp()
        if getattr(self, '_skin_dirty', False):
            self._skin_save()
        self._set_status("Saved: " + ", ".join(items))

    def closeEvent(self, event): #vers 1
        """Ask before losing unsaved ped edits."""
        from PyQt6.QtWidgets import QMessageBox
        items = self._ped_dirty_items()
        if items:
            r = QMessageBox.question(self, App_name, "Save changes to " + ", ".join(items) + "?",
                                     QMessageBox.StandardButton.Save | QMessageBox.StandardButton.Discard
                                     | QMessageBox.StandardButton.Cancel)
            if r == QMessageBox.StandardButton.Cancel:
                event.ignore()
                return
            if r == QMessageBox.StandardButton.Save:
                self._ped_save_all()
        super().closeEvent(event)

    def _show_dff(self, file_path, model): #vers 2
        """Any DFF opened (Open, IMG, tabs): also set up bones / anims / skinning."""
        super()._show_dff(file_path, model)
        if file_path and os.path.isfile(file_path):
            with open(file_path, 'rb') as f:
                raw = f.read()
            self._ped_dff_src = (file_path, raw)               # skin weights save back here
            self._init_animator(raw)
            self._skin_refresh()

    def setup_ui(self): #vers 17
        """Model Workshop layout, then Peds, Animation and Bones docks on the right."""
        super().setup_ui()
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        self.setWindowIcon(SVGIconFactory.ped_app_icon(32))       # same icon as Intro / right pane
        mw = self._outer_mw
        docks = (self._create_peds_dock(), self._create_anim_dock(), self._create_bones_dock())
        for dock, title in zip(docks, ("Peds", "Animation", "Bones")):
            dock.setMinimumWidth(280)
            self._make_dock_collapsible(dock, title)
            mw.addDockWidget(Qt.DockWidgetArea.RightDockWidgetArea, dock)
        mw.splitDockWidget(docks[0], docks[1], Qt.Orientation.Vertical)
        mw.splitDockWidget(docks[1], docks[2], Qt.Orientation.Vertical)
        mw.resizeDocks(list(docks), [420, 300, 280], Qt.Orientation.Vertical)
        from PyQt6.QtCore import QTimer
        QTimer.singleShot(700, lambda d=docks: self._default_ped_docks(d))
        outer = getattr(self, '_outer_docks', None)
        if isinstance(outer, list):
            outer.extend(docks)
        for btn, slot, tip in ((getattr(self, 'open_btn', None), self._ped_open_menu,
                                "Open a game folder, IFP or model"),
                               (getattr(self, 'save_btn', None), self._ped_save_all,
                                "Save all changed ped files (backup first)")):
            if btn is not None:
                btn.clicked.disconnect()
                btn.clicked.connect(slot)
                btn.setToolTip(tip)
        from apps.methods.dock_panels import add_title_widget
        from PyQt6.QtWidgets import QAbstractSpinBox, QComboBox, QLineEdit, QSizePolicy
        tb = self._anim_tool_button
        self._ped_root_label.setStyleSheet("padding-left: 6px;")
        self._ped_search.setMinimumWidth(70)
        for w in (self._ped_root_label,          # Peds | folder | folder, open | filter | save, save all | x
                  tb('folder_icon', "Game folder: GTA III, VC, SA or IV install", lambda: self._open_ped_root('')),
                  tb('open_icon', "Open a game folder, IFP or model", self._ped_open_menu),
                  self._ped_search,
                  tb('save_icon', "Save the peds IDE (changed rows, backup first)", self._save_ped_ide),
                  tb('save_all_icon', "Save all changed ped files: peds IDE, data file, IFP",
                     self._ped_save_all)):
            add_title_widget(docks[0], w)
        lbl_lay = docks[0].titleBarWidget().layout()
        lbl_lay.setStretchFactor(self._ped_root_label, 1)
        docks[1]._title_label.hide()                         # Animation: IFP combo is the title
        for w in self._anim_title_widgets:
            add_title_widget(docks[1], w)
        docks[1].titleBarWidget().layout().setStretchFactor(self._anim_search, 1)
        self._ifp_source.setMaximumWidth(110)
        for w in self._bones_title_widgets:                  # Bones: view toggles in the title
            add_title_widget(docks[2], w)
        for dock in docks:                                   # no row taller than 26 px
            for w in dock.findChildren((QLineEdit, QComboBox, QAbstractSpinBox)):
                w.setFixedHeight(24)
            for w in dock.findChildren(QAbstractSpinBox):    # text on the row's centre line
                w.setStyleSheet("padding: 0px 4px;")
            for w in dock.findChildren(QComboBox):           # narrow docks keep their icons
                w.setSizeAdjustPolicy(QComboBox.SizeAdjustPolicy.AdjustToMinimumContentsLengthWithIcon)
                w.setMinimumContentsLength(6)
        self._ped_table.verticalHeader().setDefaultSectionSize(22)
        for w in self._rot_spins + self._pos_spins:            # pose fields shrink with the dock
            w.setMinimumWidth(44)
            w.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Fixed)
        from PyQt6.QtWidgets import QToolBar
        from PyQt6.QtGui import QAction
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        nav = self._inner_mw.findChild(QToolBar, "Navigation")
        self._ped_view_act = QAction(SVGIconFactory.ped_view_icon(20, self._get_icon_color()), "Ped view", self)
        self._ped_view_act.setToolTip("Ped view: Front, Right, Back, Left in turn")
        self._ped_view_act.triggered.connect(self._cycle_ped_view)
        if nav is not None:
            nav.addAction(self._ped_view_act)
        from PyQt6.QtWidgets import QToolButton
        view_btn = QToolButton()                             # also in the Bones title, always visible
        view_btn.setDefaultAction(self._ped_view_act)
        view_btn.setAutoRaise(True)
        add_title_widget(docks[2], view_btn, 1)
        quad_act = getattr(self, '_quad_view_act', None)
        if quad_act is not None:                             # 4-pane: Top / Front / Side / Perspective
            quad_btn = QToolButton()
            quad_btn.setDefaultAction(quad_act)
            quad_btn.setAutoRaise(True)
            quad_btn.setToolTip("4-pane view on / off (Top, Front, Side, Perspective)")
            add_title_widget(docks[2], quad_btn, 2)
        from PyQt6.QtWidgets import QMenu
        grid_btn = QToolButton()                             # grid style menu
        grid_btn.setIcon(SVGIconFactory.grid_icon(16, self._get_icon_color()))
        grid_btn.setToolTip("Grid style: lines, holodeck, checkerboard, honeycomb, texture")
        grid_btn.setPopupMode(QToolButton.ToolButtonPopupMode.InstantPopup)
        grid_btn.setAutoRaise(True)
        menu = QMenu(grid_btn)
        for key, label in (('lines', "Lines"), ('holodeck', "Holodeck (yellow)"), ('checker', "Checkerboard"),
                           ('honeycomb', "Honeycomb"), ('texture', "Texture..."), ('custom', "Custom (Settings > Grid)"),
                           ('none', "No grid")):
            menu.addAction(label, lambda k=key: self._ped_set_grid(k))
        grid_btn.setMenu(menu)
        add_title_widget(docks[2], grid_btn, 3)
        add_title_widget(docks[2], self._anim_tool_button(        # skinning window
            'paint_icon', "Skinning: bone weights (heat map, paint) and texture / UV", self._skin_window), 4)
        size_btn = QToolButton()                             # texture size: render down
        size_btn.setIcon(SVGIconFactory.dp_resize_icon(16, self._get_icon_color()))
        size_btn.setToolTip("Texture size: scale the ped textures down (longest side)")
        size_btn.setPopupMode(QToolButton.ToolButtonPopupMode.InstantPopup)
        size_btn.setAutoRaise(True)
        smenu = QMenu(size_btn)
        for px in (512, 256, 128):
            smenu.addAction(f"{px} px", lambda p=px: self._ped_resize_textures(p))
        size_btn.setMenu(smenu)
        tex_dock = getattr(self, '_texture_dock', None)
        if tex_dock is not None and tex_dock.titleBarWidget() is not None:
            add_title_widget(tex_dock, size_btn)
        saved = self._load_ped_prefs().get('grid')
        if saved:
            self._ped_set_grid(saved.get('kind', 'lines'), saved.get('path', ''))
        self._ped_view_act.setToolTip("Ped view: Front, Right, Back, Left in turn\n"
                                      "(right-drag to orbit, middle-drag to pan)")
        self.setWindowTitle(App_name)


def open_ped_workshop(main_window=None, game_root: str = '', file_path: str = ''): #vers 2
    """Open docked in IMG Factory (tab) or standalone; file_path picks a data file / IFP."""
    if main_window is not None and hasattr(main_window, 'main_tab_widget'):
        from PyQt6.QtWidgets import QVBoxLayout, QWidget
        container = QWidget()
        lay = QVBoxLayout(container)
        lay.setContentsMargins(0, 0, 0, 0)
        ws = PedWorkshop(container, main_window)
        ws.setWindowFlags(Qt.WindowType.Widget)
        lay.addWidget(ws)
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        idx = main_window.main_tab_widget.addTab(container, SVGIconFactory.ped_app_icon(20), App_name)
        main_window.main_tab_widget.setCurrentIndex(idx)
    else:
        ws = PedWorkshop(main_window=main_window)       # frameless: custom title bar only
        ws.setWindowTitle(App_name)
        ws.resize(1400, 900)
    ws.show()
    if not game_root and main_window is None:
        game_root = (ws._load_ped_prefs().get('recent_roots') or [''])[0]
    if game_root and os.path.isdir(game_root):
        ws._open_ped_root(game_root)
        if file_path:
            ws._open_ped_file(file_path)
    return ws


if __name__ == "__main__":
    app = QApplication(sys.argv)
    w = open_ped_workshop(None, sys.argv[1] if len(sys.argv) > 1 else '')
    sys.exit(app.exec())
