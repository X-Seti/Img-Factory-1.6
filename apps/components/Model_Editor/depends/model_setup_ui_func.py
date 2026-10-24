#this belongs in apps/components/Model_Editor/depends/model_setup_ui_func.py - Version: 19
# X-Seti - October 12 2026 - IMG Factory 1.6 - Model Workshop UI setup

"""
Toolbars, ribbons, panels, docks and toolbar state for ModelWorkshop.
"""

##class ModelSetupUIMixin(TitlebarMixin): -
# _apply_icon_scale
# _build_model_ide_toolbar
# _build_model_name_toolbar
# _build_toolbars
# _create_frame_hierarchy_panel
# _create_left_panel
# _create_models_table_panel
# _create_paint_bar
# _create_right_panel
# _enable_dff_toolbar
# _find_all_paint_btns
# _info_ribbon_menu
# _menu_btn_popup
# _move_info_ribbon
# _on_menu_btn_clicked
# _rebuild_toolbars
# _refresh_icons
# _restore_toolbar_state
# _ribbon_config_path
# _save_toolbar_state
# _set_col_buttons_enabled
# _switch_icon_set
# _sync_middle_btn_row_visibility
# _titlebar_actions
# _titlebar_title
# _toggle_mid_btn_row_collapsed
# _toggle_tex_btn_row_collapsed
# _toolbar_context_menu
# _update_tex_btn_compact
# _update_transform_text_panel_visibility
# _wire_col_buttons
# _wire_dff_buttons
# _wrap_middle_panel_with_own_dock_areas

from PyQt6.QtCore import QSize, Qt
from PyQt6.QtGui import QAction
from PyQt6.QtWidgets import QAbstractItemView, QApplication, QComboBox, QFrame, QHBoxLayout, QLabel, QLineEdit, QListWidget, QMenu, QPushButton, QTableWidget, QVBoxLayout, QWidget
from apps.methods.dff_viewport import DFFViewport
from apps.methods.img_factory_settings import get_user_config_dir
from apps.methods.imgfactory_svg_icons import SVGIconFactory
from apps.methods.titlebar_func import TitlebarMixin
from apps.components.Model_Editor.depends.model_list_func import _ModelListDelegate

# Build information
App_name = "Model Workshop"
App_build = "193"

VIEWPORT_AVAILABLE = True
DEBUG_STANDALONE = False
APPSETTINGS_AVAILABLE = True

class ModelSetupUIMixin(TitlebarMixin): #vers 2
    """Toolbars, ribbons, panels, docks and toolbar state for ModelWorkshop."""



    def _refresh_icons(self): #vers 3
        """Refresh all button icons after theme change — picks up current text_primary colour."""
        SVGIconFactory.clear_cache()
        c = self._get_icon_color()
        SVGIconFactory.set_theme_color(c)

        # Map: attribute name → icon factory method
        _icon_map = [
            # Toolbar
            ('settings_btn',       'settings_icon'),
            ('open_btn',           'open_icon'),
            ('open_dff_btn',       'open_icon'),
            ('open_txd_btn',       'open_icon'),
            ('tex_load_btn',       'open_icon'),
            ('tex_browse_btn',     'folder_icon'),
            ('tex_pass_btn',       'export_icon'),
            ('tex_dp5_btn',        'dp5_workshop_icon'),
            ('tex_save_btn',       'save_icon'),
            ('save_btn',           'save_icon'),
            ('saveall_btn',        'saveas_icon'),
            ('export_all_btn',     'package_icon'),
            ('undo_btn',           'undo_icon'),
            ('info_btn',           'info_icon'),
            ('minimize_btn',       'minimize_icon'),
            ('maximize_btn',       'maximize_icon'),
            ('close_btn',          'close_icon'),
            ('open_img_btn',       'folder_icon'),
            ('from_img_btn',       'open_icon'),
            # Middle panel mini-toolbar
            ('open_col_btn',       'open_icon'),
            ('save_col_btn',       'save_icon'),
            ('export_col_btn',     'package_icon'),
            ('undo_col_btn',       'undo_icon'),
            # Transform icon panel
            ('flip_vert_btn',      'flip_vert_icon'),
            ('flip_horz_btn',      'flip_horz_icon'),
            ('rotate_cw_btn',      'rotate_cw_icon'),
            ('rotate_ccw_btn',     'rotate_ccw_icon'),
            ('analyze_btn',        'analyze_icon'),
            ('copy_btn',           'copy_icon'),
            ('paste_btn',          'paste_icon'),
            ('create_surface_btn', 'add_icon'),
            ('delete_surface_btn', 'remove_icon'),
            ('import_btn',         'import_icon'),
            ('export_btn',         'export_icon'),
            # Right panel toolbar buttons
            ('switch_btn',         'flip_vert_icon'),
            ('convert_btn',        'convert_icon'),
            ('properties_btn',     'settings_icon'),
        ]
        for attr, method in _icon_map:
            btn = getattr(self, attr, None)
            if btn is None:
                continue
            fn = getattr(self.icon_factory, method, None)
            if fn is None:
                continue
            try:
                btn.setIcon(fn(color=c))
            except TypeError:
                try:
                    btn.setIcon(fn())
                except Exception:
                    pass


# - Settings Reusable

        # Sync mini toolbar visibility with current dock state
        self._sync_middle_btn_row_visibility()
        self._apply_custom_icons()

    def _sync_middle_btn_row_visibility(self): #vers 3
        """Show/hide the mini toolbar's action icons (Open/Save/Import/
        Export/Undo/3D View) based on dock state AND collapse state; the
        row is hidden when standalone ([=] sits in the Models title)."""
        if not hasattr(self, '_middle_btn_row'):
            return
        docked_only = self.is_docked and not self.standalone_mode
        collapsed = getattr(self, '_mid_btn_row_collapsed', False)
        show = docked_only and not collapsed
        self._middle_btn_row.setVisible(docked_only)        # standalone: no empty row
        for attr in ('open_col_btn', 'save_col_btn', 'import_btn',
                     'export_col_btn', 'undo_col_btn', '_mini_3d_btn'):
            b = getattr(self, attr, None)
            if b:
                b.setVisible(show)

    def _toggle_mid_btn_row_collapsed(self): #vers 1
        """Collapse/expand the mini toolbar's action icons via the -/+
        button, independent of the icon-only/icon+text adaptive display."""
        self._mid_btn_row_collapsed = not getattr(self, '_mid_btn_row_collapsed', False)
        self.mid_btn_collapse_btn.setText("+" if self._mid_btn_row_collapsed else "−")
        self._sync_middle_btn_row_visibility()

    def _on_menu_btn_clicked(self): #vers 1
        from PyQt6.QtWidgets import QMenu
        menu = QMenu(self)
        if hasattr(self, '_build_menus_into_qmenu'):
            self._build_menus_into_qmenu(menu)
        btn = getattr(self, 'menu_btn', None)
        if btn: menu.exec(btn.mapToGlobal(btn.rect().bottomLeft()))




    def _update_transform_text_panel_visibility(self): #vers 3
        """DockableToolbar version — toolbars manage their own visibility."""
        btr = getattr(self, '_bottom_text_row', None)
        bir = getattr(self, '_bottom_icon_row', None)
        if btr or bir:
            rp = getattr(self, '_right_panel_ref', None)
            ref_w = rp.width() if rp else self.width()
            try:
                from apps.methods.imgfactory_ui_settings import get_collapse_threshold
                threshold = get_collapse_threshold(getattr(self, 'main_window', None))
            except Exception:
                threshold = 550
            wide = ref_w >= threshold
            if btr: btr.setVisible(wide)
            if bir: bir.setVisible(not wide)

    def _update_tex_btn_compact(self): #vers 2
        """Icon-only when texture panel is narrow — delegates to shared helper."""
        panel = getattr(self, '_tex_panel', None)
        if not panel or not panel.isVisible():
            return
        try:
            from apps.methods.imgfactory_ui_settings import apply_compact_buttons
            meta = [(getattr(self, attr, None), label)
                    for attr, label, _ in getattr(self, '_tex_btns_meta', [])]
            apply_compact_buttons(meta, panel.width())
        except Exception:
            pass

    def _toggle_tex_btn_row_collapsed(self): #vers 1
        """Collapse/expand the texture panel's Load/Texlist/WShop/Save
        buttons via the -/+ button, independent of the icon-only/icon+text
        adaptive display."""
        self._tex_btn_row_collapsed = not getattr(self, '_tex_btn_row_collapsed', False)
        self.tex_btn_collapse_btn.setText("+" if self._tex_btn_row_collapsed else "−")
        show = not self._tex_btn_row_collapsed
        for attr, _label, _icon in getattr(self, '_tex_btns_meta', []):
            b = getattr(self, attr, None)
            if b:
                b.setVisible(show)


# - Panel Setup

    def _menu_btn_popup(self): #vers 1
        """Menu button calls _on_menu_btn_clicked."""
        return False

    def _titlebar_actions(self, layout): #vers 1
        """Model titlebar: IMG pickers, Open, Save, Import, Export, Undo."""
        icon_color = self._get_icon_color()
        _ICO_SZ = self._titlebar_icon_size()
        # Only show "Open IMG" button if NOT standalone
        if not self.standalone_mode:
            self.open_img_btn = QPushButton("OpenIMG")
            self.open_img_btn.setFont(self.button_font)
            self.open_img_btn.setIcon(self.icon_factory.folder_icon(color=icon_color))
            self.open_img_btn.setIconSize(QSize(_ICO_SZ, _ICO_SZ))
            self.open_img_btn.clicked.connect(self.open_img_archive)
            self.open_img_btn.setToolTip("Open an IMG archive and browse its DFF/COL entries")
            layout.addWidget(self.open_img_btn)

            # "From IMG" — pick a COL entry from the currently loaded IMG in IMG Factory
            self.from_img_btn = QPushButton("From IMG")
            self.from_img_btn.setFont(self.button_font)
            self.from_img_btn.setIcon(self.icon_factory.open_icon(color=icon_color))
            self.from_img_btn.setIconSize(QSize(_ICO_SZ, _ICO_SZ))
            self.from_img_btn.clicked.connect(self._pick_col_from_current_img)
            self.from_img_btn.setToolTip("Pick a DFF/COL entry from the currently loaded IMG")
            layout.addWidget(self.from_img_btn)

        # Open button
        self.open_btn = QPushButton()
        self.open_btn.setFont(self.button_font)
        self.open_btn.setIcon(self.icon_factory.open_icon(color=icon_color))
        self.open_btn.setText("Open")
        self.open_btn.setIconSize(QSize(20, 20))
        self.open_btn.setShortcut("Ctrl+O")
        if self.button_display_mode == 'icons':
            self.open_btn.setFixedSize(40, 40)
        self.open_btn.setToolTip("Open DFF or COL model file (Ctrl+O)")
        self.open_btn.clicked.connect(self._open_file)
        layout.addWidget(self.open_btn)

        # Open DFF (standalone — skips COL filter)
        self.open_dff_btn = QPushButton()
        self.open_dff_btn.setFont(self.button_font)
        self.open_dff_btn.setIcon(self.icon_factory.open_icon(color=icon_color))
        self.open_dff_btn.setText("DFF/ TXD")
        self.open_dff_btn.setIconSize(QSize(20, 20))
        self.open_dff_btn.setToolTip("Open a DFF model file directly (Ctrl+D)")
        self.open_dff_btn.setShortcut("Ctrl+D")
        self.open_dff_btn.clicked.connect(self._open_dff_standalone)

        self.load_txd_btn = QPushButton("TXD")
        self.load_txd_btn.setFont(self.button_font)
        self.load_txd_btn.setIcon(self.icon_factory.open_icon(color=icon_color))
        self.load_txd_btn.setIconSize(QSize(16, 16))
        self.load_txd_btn.setFixedHeight(26)
        self.load_txd_btn.setMinimumWidth(80)
        self.load_txd_btn.setToolTip("Open TXD — uses IDE link if available, else browse")
        self.load_txd_btn.clicked.connect(self._open_txd_smart)

        # GL Model Viewer button
        self.gl_viewer_btn = QPushButton()
        self.gl_viewer_btn.setFont(self.button_font)
        self.gl_viewer_btn.setText("3D View")
        self.gl_viewer_btn.setToolTip("Open GL Model Viewer (hardware 3D)")
        self.gl_viewer_btn.clicked.connect(self._open_gl_viewer)

        # Open TXD button — loads TXD for the current DFF, or browses for one
        self.open_txd_btn = QPushButton()
        self.open_txd_btn.setFont(self.button_font)
        self.open_txd_btn.setIcon(self.icon_factory.open_icon(color=icon_color))
        self.open_txd_btn.setText("TXD")
        self.open_txd_btn.setIconSize(QSize(20, 20))
        self.open_txd_btn.setToolTip(
            "Open TXD for current DFF model (Ctrl+T)\n"
            "If IDE link is known, auto-finds TXD from open IMGs.\n"
            "Hold Shift to always browse for a file.")
        self.open_txd_btn.setShortcut("Ctrl+T")
        self.open_txd_btn.clicked.connect(self._open_txd_combined)

        # Save button
        self.save_btn = QPushButton()
        self.save_btn.setFont(self.button_font)
        self.save_btn.setIcon(self.icon_factory.save_icon(color=icon_color))
        self.save_btn.setText("Save")
        self.save_btn.setIconSize(QSize(20, 20))
        self.save_btn.setShortcut("Ctrl+S")
        if self.button_display_mode == 'icons':
            self.save_btn.setFixedSize(40, 40)
        self.save_btn.setEnabled(True)
        self.save_btn.setToolTip("Export/save model (Ctrl+S)")
        self.save_btn.clicked.connect(self._save_file)
        layout.addWidget(self.save_btn)

        # Save button
        self.saveall_btn = QPushButton()
        self.saveall_btn.setFont(self.button_font)
        self.saveall_btn.setIcon(self.icon_factory.saveas_icon(color=icon_color))
        self.saveall_btn.setText("Save All")
        self.saveall_btn.setIconSize(QSize(20, 20))
        self.saveall_btn.setShortcut("Ctrl+S")
        if self.button_display_mode == 'icons':
            self.saveall_btn.setFixedSize(40, 40)
        self.saveall_btn.setEnabled(True)
        self.saveall_btn.setToolTip("Export/save model (Ctrl+S)")
        self.saveall_btn.clicked.connect(self._saveall_file)
        #layout.addWidget(self.saveall_btn)

        # Import button (title bar) — same _import_model callback the
        # docked middle-panel mini toolbar already uses successfully.
        self.tb_import_btn = QPushButton()
        self.tb_import_btn.setFont(self.button_font)
        self.tb_import_btn.setIcon(
            self.icon_factory.import_icon(color=icon_color)
            if hasattr(self.icon_factory, 'import_icon')
            else self.icon_factory.open_icon(color=icon_color))
        self.tb_import_btn.setText("Import")
        self.tb_import_btn.setIconSize(QSize(20, 20))
        if self.button_display_mode == 'icons':
            self.tb_import_btn.setFixedSize(40, 40)
        self.tb_import_btn.setToolTip("Import — MDL, OBJ, FBX and other formats")
        self.tb_import_btn.clicked.connect(self._import_model)
        layout.addWidget(self.tb_import_btn)

        # Export button (title bar) — same _export_model_menu callback the
        # docked middle-panel mini toolbar already uses successfully.
        # (Previously wired to export_all(), which is COL-specific and the
        # wrong callback for a general model export button - fixed.)
        self.export_ojs_btn = QPushButton("Export")
        self.export_ojs_btn.setFont(self.button_font)
        self.export_ojs_btn.setIcon(self.icon_factory.multi_export_icon(color=icon_color)
            if hasattr(self.icon_factory, 'multi_export_icon')
            else self.icon_factory.package_icon(color=icon_color))
        self.export_ojs_btn.setIconSize(QSize(20, 20))
        if self.button_display_mode == 'icons':
            self.export_ojs_btn.setFixedSize(40, 40)
        self.export_ojs_btn.setToolTip("Export — OBJ, COL, 3DS, FBX and other formats")
        self.export_ojs_btn.clicked.connect(self._export_model_menu)
        self.export_ojs_btn.setEnabled(True)
        layout.addWidget(self.export_ojs_btn)

        self.export_tex_btn = QPushButton("Extract Tex")
        self.export_tex_btn.setFont(self.button_font)
        self.export_tex_btn.setIcon(self.icon_factory.package_icon(color=icon_color))
        self.export_tex_btn.setIconSize(QSize(20, 20))
        self.export_tex_btn.setToolTip("Export textures as png, tga, dss or other formats")
        self.export_tex_btn.clicked.connect(self.export_all)
        self.export_tex_btn.setEnabled(True)



        self.undo_btn = QPushButton()
        self.undo_btn.setFont(self.button_font)
        self.undo_btn.setIcon(self.icon_factory.undo_icon(color=icon_color))
        self.undo_btn.setText("")
        self.undo_btn.setIconSize(QSize(20, 20))
        self.undo_btn.clicked.connect(self._undo_last_action)
        self.undo_btn.setEnabled(True)
        self.undo_btn.setToolTip("Undo last change")
        layout.addWidget(self.undo_btn)

        # Register title bar buttons for adaptive icon-only display when
        # the window/toolbar is narrow (same apply_compact_buttons helper
        # the middle panel's mini toolbar and texture panel already use).
        self._titlebar_compact_btns = [
            (self.open_btn,       "Open"),
            (self.save_btn,       "Save"),
            (self.tb_import_btn,  "Import"),
            (self.export_ojs_btn, "Export"),
            (self.undo_btn,       ""),
        ]

    def _titlebar_title(self): #vers 1
        """Centre title."""
        return App_name




    def _enable_dff_toolbar(self, dff_mode: bool): #vers 3
        """Switch left toolbar between DFF mode and COL mode.
        DFF mode: enables paint, material, render-select, prelighting, flip, rotate, analyze, copy, delete, duplicate.
        COL mode: enables COL-specific buttons, disables DFF ones."""
        # Buttons only relevant for COL — hide entirely in DFF mode
        col_only = [
            'create_surface_btn',
        ]
        # Buttons that work in BOTH modes (flip/rotate/analyze/copy/paste/delete/duplicate)
        shared_btns = [
            'flip_vert_btn', 'flip_horz_btn', 'rotate_cw_btn', 'rotate_ccw_btn',
            'analyze_btn', 'copy_btn', 'paste_btn', 'delete_surface_btn',
            'duplicate_surface_btn',
        ]
        # Buttons shared but context-switches
        dff_btns = ['paint_btn', 'surface_type_btn']

        for attr in col_only:
            b = getattr(self, attr, None)
            if b:
                b.setEnabled(not dff_mode)
                b.setVisible(not dff_mode)

        for attr in shared_btns:
            b = getattr(self, attr, None)
            if b:
                b.setEnabled(True)
                b.setVisible(True)

        for attr in dff_btns:
            b = getattr(self, attr, None)
            if b: b.setEnabled(dff_mode)

        # Rewire shared buttons for correct mode
        if dff_mode:
            self._wire_dff_buttons()
        else:
            self._wire_col_buttons()

        # DFF-only toolbar buttons (backface, front-paint, primitive, etc.)
        # V/E/F/P select buttons are gated separately by
        # _update_select_mode_availability (need an actual model, not just
        # DFF-vs-COL mode) so they're excluded from this blanket pass.
        _select_btn_attrs = {'_sel_vert_btn', '_sel_edge_btn',
                              '_sel_face_btn', '_sel_poly_btn'}
        for btn in getattr(self, '_dff_only_toolbar_btns', []):
            is_select_btn = any(btn is getattr(self, a, None) for a in _select_btn_attrs)
            btn.setVisible(dff_mode)
            if not is_select_btn:
                btn.setEnabled(dff_mode)
        self._update_select_mode_availability()

        # Prelighting row — shown in DFF mode
        for attr in ('prelight_apply_btn', 'prelight_setup_btn'):
            b = getattr(self, attr, None)
            if b:
                b.setEnabled(dff_mode)

        # Bottom info_format label
        lbl = getattr(self, 'info_format', None)
        if lbl:
            lbl.setText("Prelight: " if dff_mode else "Format: ")

        # Update tooltips for DFF context
        if dff_mode:
            if hasattr(self, 'paint_btn'):
                self.paint_btn.setToolTip(
                    "Open Material List — assign textures to DFF geometry")
            if hasattr(self, 'surface_type_btn'):
                self.surface_type_btn.setToolTip(
                    "Material Editor — view materials, textures, IDE info, swap TXD")
            if hasattr(self, 'flip_vert_btn'):
                self.flip_vert_btn.setToolTip("Flip DFF geometry on Y axis")
            if hasattr(self, 'flip_horz_btn'):
                self.flip_horz_btn.setToolTip("Flip DFF geometry on X axis")
            if hasattr(self, 'rotate_cw_btn'):
                self.rotate_cw_btn.setToolTip("Rotate DFF geometry 90° clockwise (Z axis)")
            if hasattr(self, 'rotate_ccw_btn'):
                self.rotate_ccw_btn.setToolTip("Rotate DFF geometry 90° counter-clockwise (Z axis)")
            if hasattr(self, 'analyze_btn'):
                self.analyze_btn.setToolTip("Analyze DFF — verts, tris, materials, frames")
            if hasattr(self, 'copy_btn'):
                self.copy_btn.setToolTip("Copy selected geometry to clipboard")
            if hasattr(self, 'paste_btn'):
                self.paste_btn.setToolTip("Paste geometry from clipboard")
            if hasattr(self, 'delete_surface_btn'):
                self.delete_surface_btn.setToolTip("Delete selected geometry from DFF")
            if hasattr(self, 'duplicate_surface_btn'):
                self.duplicate_surface_btn.setToolTip("Duplicate selected geometry")

    # - Render / selection mode

    def _wire_col_buttons(self): #vers 1
        """Reconnect shared toolbar buttons to COL-mode handlers."""
        pairs = [
            ('flip_vert_btn',       lambda: self.preview_widget and self.preview_widget.flip_vertical()),
            ('flip_horz_btn',       lambda: self.preview_widget and self.preview_widget.flip_horizontal()),
            ('rotate_cw_btn',       lambda: self.preview_widget and self.preview_widget.rotate_cw()),
            ('rotate_ccw_btn',      lambda: self.preview_widget and self.preview_widget.rotate_ccw()),
            ('analyze_btn',         self._analyze_collision),
            ('copy_btn',            self._copy_surface),
            ('paste_btn',           self._paste_surface),
            ('delete_surface_btn',  self._delete_surface),
            ('duplicate_surface_btn', self._duplicate_surface),
        ]
        for attr, fn in pairs:
            b = getattr(self, attr, None)
            if b:
                try: b.clicked.disconnect()
                except Exception: pass
                b.clicked.connect(fn)

    def _wire_dff_buttons(self): #vers 1
        """Connect shared toolbar buttons to DFF-mode handlers."""
        pairs = [
            ('flip_vert_btn',       self._dff_flip_y),
            ('flip_horz_btn',       self._dff_flip_x),
            ('rotate_cw_btn',       self._dff_rotate_cw),
            ('rotate_ccw_btn',      self._dff_rotate_ccw),
            ('analyze_btn',         self._dff_analyze),
            ('copy_btn',            self._dff_copy_geometry),
            ('paste_btn',           self._dff_paste_geometry),
            ('delete_surface_btn',  self._dff_delete_geometry),
            ('duplicate_surface_btn', self._dff_duplicate_geometry),
        ]
        for attr, fn in pairs:
            b = getattr(self, attr, None)
            if b:
                try: b.clicked.disconnect()
                except Exception: pass
                b.clicked.connect(fn)


    def _create_left_panel(self): #vers 8
        """Create left panel - COL file list (only in IMG Factory mode)"""
        # In standalone mode, don't create this panel
        if self.standalone_mode:
            self.col_list_widget = None  # Explicitly set to None
            return None

        if not self.main_window:
            # Standalone mode - return None to hide this panel
            return None

        # Only create panel in IMG Factory mode
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(200)
        panel.setMaximumWidth(300)

        layout = QVBoxLayout(panel)
        layout.setContentsMargins(5, 5, 5, 5)

        # Header label + search button: shown in the Files dock title bar
        self._left_panel_header = QLabel("No files loaded")
        self._left_panel_header.setFont(self.panel_font)

        self._model_search_btn = QPushButton()
        self._model_search_btn.setFixedSize(20, 18)
        self._model_search_btn.setToolTip("Search model files")
        try:
            from apps.methods.imgfactory_svg_icons import SVGIconFactory as _SVG
            self._model_search_btn.setIcon(_SVG.search_icon(16))
            self._model_search_btn.setIconSize(QSize(16, 16))
        except Exception:
            self._model_search_btn.setText("?")
        self._model_search_btn.clicked.connect(self._show_model_search)

        # Search box (hidden by default)
        self._model_search_box = QLineEdit()
        self._model_search_box.setPlaceholderText("Search model files...")
        self._model_search_box.setVisible(False)
        self._model_search_box.textChanged.connect(self._filter_model_list)
        layout.addWidget(self._model_search_box)

        self.col_list_widget = QListWidget()
        self.col_list_widget.setAlternatingRowColors(True)
        self.col_list_widget.itemClicked.connect(self._on_col_selected)
        self.col_list_widget.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.col_list_widget.customContextMenuRequested.connect(self._on_model_list_menu)
        layout.addWidget(self.col_list_widget)
        return panel


    def _create_models_table_panel(self): #vers 2
        """Create the Models table panel - mini toolbar + view toggle +
        model table only. Frame Hierarchy and Textures are now separate
        top-level dock widgets (see _create_frame_hierarchy_panel,
        _create_texture_panel), not stacked into this same panel -
        every section can be independently dragged/floated/docked now,
        same as Files already could."""
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(250)

        layout = QVBoxLayout(panel)
        layout.setContentsMargins(5, 5, 5, 5)
        layout.setSpacing(4)
        self._middle_panel_layout = layout

        # - Mini toolbar row: Open / Save / Import / Export / Undo / 3D View
        # (docked mode only) + the view-toggle button (always visible).
        # Previously the toggle had its own dedicated row above this one,
        # wasting a full row of height on just one small button - merged
        # into a single row instead.
        icon_color = self._get_icon_color()
        self._middle_btn_row = QFrame()
        btn_layout = QHBoxLayout(self._middle_btn_row)
        btn_layout.setContentsMargins(0, 0, 0, 0)
        btn_layout.setSpacing(3)

        # Collapse/expand toggle - hides the action icons below to reclaim
        # space, independent of the icon-only/icon+text compacting.
        self._mid_btn_row_collapsed = False
        self.mid_btn_collapse_btn = QPushButton("−")
        self.mid_btn_collapse_btn.setFixedSize(18, 28)
        self.mid_btn_collapse_btn.setToolTip("Collapse/expand this toolbar")
        self.mid_btn_collapse_btn.clicked.connect(self._toggle_mid_btn_row_collapsed)
        btn_layout.addWidget(self.mid_btn_collapse_btn)

        def _icon_btn(icon, tooltip, slot, enabled=True):  #vers 2
            b = QPushButton()
            b.setIcon(icon)
            b.setIconSize(QSize(20, 20))
            b.setFixedHeight(28)
            b.setMinimumWidth(28)
            b.setToolTip(tooltip)
            b.clicked.connect(slot)
            b.setEnabled(enabled)
            return b

        # Load (DFF + TXD + COL multi-select)
        self.open_col_btn = _icon_btn(
            self.icon_factory.open_icon(color=icon_color),
            "Load — DFF model, TXD textures, COL collision (multi-select)",
            self._open_dff_standalone)
        btn_layout.addWidget(self.open_col_btn)

        # Save
        self.save_col_btn = _icon_btn(
            self.icon_factory.save_icon(color=icon_color),
            "Save current file",
            self._save_file)
        btn_layout.addWidget(self.save_col_btn)

        # Import (other formats)
        self.import_btn = _icon_btn(
            self.icon_factory.import_icon(color=icon_color)
                if hasattr(self.icon_factory, 'import_icon')
                else self.icon_factory.open_icon(color=icon_color),
            "Import — MDL, OBJ, FBX and other formats",
            self._import_model)
        btn_layout.addWidget(self.import_btn)

        # Export (multiple formats — geometry + COL)
        self.export_col_btn = _icon_btn(
            self.icon_factory.multi_export_icon(color=icon_color),
            "Export — OBJ, COL, 3DS, FBX and other formats",
            self._export_model_menu)
        btn_layout.addWidget(self.export_col_btn)

        # Undo
        self.undo_col_btn = _icon_btn(
            self.icon_factory.undo_icon(color=icon_color),
            "Undo last change",
            self._undo_last_action)
        btn_layout.addWidget(self.undo_col_btn)

        # 3D View
        self._mini_3d_btn = _icon_btn(
            self.icon_factory.viewport_icon(color=icon_color),
            "Open GL Model Viewer",
            self._open_gl_viewer)
        btn_layout.addWidget(self._mini_3d_btn)

        btn_layout.addStretch()

        # View-toggle button - always visible regardless of dock state,
        # unlike the action icons above it.
        self._col_view_mode = 'detail'   # start in compact thumbnail view
        self.col_view_toggle_btn = QPushButton("[=]")
        self.col_view_toggle_btn.setFont(self.button_font)
        self.col_view_toggle_btn.setFixedWidth(32)
        self.col_view_toggle_btn.setFixedHeight(22)
        self.col_view_toggle_btn.setToolTip(
            "Toggle view: compact list ↔ full details table")
        self.col_view_toggle_btn.clicked.connect(self._toggle_col_view)
        btn_layout.addWidget(self.col_view_toggle_btn)

        layout.addWidget(self._middle_btn_row)
        self._middle_btn_row.setVisible(True)   # row itself always shown -
        # the toggle button lives here now, only the action icons below
        # are conditionally hidden when not docked.
        self._sync_middle_btn_row_visibility()
        # Register for adaptive compact display
        self._mid_compact_btns = [
            (getattr(self, 'open_col_btn',  None), "Open"),
            (getattr(self, 'save_col_btn',  None), "Save"),
            (getattr(self, 'import_btn',    None), "Import"),
            (getattr(self, 'export_col_btn',None), "Export"),
            (getattr(self, 'undo_col_btn',  None), "Undo"),
        ]

        # Hook the row's own resize event directly - self.resizeEvent()
        # only fires when the whole ModelWorkshop widget's outer bounding
        # box changes, but dragging the Models/viewport dock divider only
        # changes this row's own width (the outer tab doesn't resize at
        # all), so relying solely on self.resizeEvent() left the compact
        # check stuck at whatever it computed on first layout - possibly
        # wrong, and never re-evaluated afterwards.
        _orig_mid_resize = self._middle_btn_row.resizeEvent
        def _mid_btn_row_resize(event, _orig=_orig_mid_resize):  #vers 1
            _orig(event)
            try:
                from apps.methods.imgfactory_ui_settings import apply_compact_buttons
                apply_compact_buttons(self._mid_compact_btns,
                                      self._middle_btn_row.width(),
                                      compact_threshold=320)
            except Exception:
                pass
        self._middle_btn_row.resizeEvent = _mid_btn_row_resize

        # - Model table (detail view)
        self.collision_list = QTableWidget()

        class _GuiLayout:
            def __init__(self, table):  #vers 1
                self.table = table
        self.gui_layout = _GuiLayout(self.collision_list)

        self.collision_list.setColumnCount(8)
        self.collision_list.setHorizontalHeaderLabels([
            "Name", "Format", "RW Version", "Size",
            "Verts", "Tris", "Materials", "UV Layers"])
        self.collision_list.setSelectionBehavior(
            QAbstractItemView.SelectionBehavior.SelectRows)
        self.collision_list.setSelectionMode(
            QAbstractItemView.SelectionMode.ExtendedSelection)
        self.collision_list.setAlternatingRowColors(True)
        self.collision_list.itemSelectionChanged.connect(self._on_collision_selected)
        self.collision_list.horizontalHeader().setStretchLastSection(True)
        self.collision_list.setContextMenuPolicy(
            Qt.ContextMenuPolicy.CustomContextMenu)
        self.collision_list.customContextMenuRequested.connect(
            self._show_collision_context_menu)
        self.collision_list.setVisible(False)  # hidden at startup — compact view is default
        layout.addWidget(self.collision_list)

        # - Compact list (thumbnail + name/version/counts, single row)
        self.mod_compact_list = QTableWidget()
        self.mod_compact_list.setColumnCount(2)
        self.mod_compact_list.setHorizontalHeaderLabels(["Preview", "Details"])
        self.mod_compact_list.horizontalHeader().setStretchLastSection(True)
        self.mod_compact_list.setSelectionBehavior(
            QAbstractItemView.SelectionBehavior.SelectRows)
        self.mod_compact_list.setSelectionMode(
            QAbstractItemView.SelectionMode.ExtendedSelection)
        self.mod_compact_list.setAlternatingRowColors(True)
        self.mod_compact_list.setIconSize(QSize(64, 64))
        self.mod_compact_list.itemSelectionChanged.connect(
            self._on_compact_col_selected)
        self.mod_compact_list.setContextMenuPolicy(
            Qt.ContextMenuPolicy.CustomContextMenu)
        self.mod_compact_list.customContextMenuRequested.connect(
            self._show_collision_context_menu)
        self.mod_compact_list.setVisible(True)    # start in compact view
        self.mod_compact_list.setRowCount(0)      # populated on first file load
        self.mod_compact_list.setWordWrap(True)
        self.mod_compact_list.setItemDelegate(_ModelListDelegate(self.mod_compact_list))
        layout.addWidget(self.mod_compact_list)

        # Model Name / IDE-TXD ribbons - built here but docked into the
        # middle panel's own nested QMainWindow below, not added to this
        # layout directly. Split into two so they can be moved
        # independently of each other.
        self._info_ribbons = {
            'name': self._build_model_name_toolbar(),
            'ide':  self._build_model_ide_toolbar(icon_color),
        }
        self._info_ribbon_location = {'name': 'middle', 'ide': 'middle'}

        return self._wrap_middle_panel_with_own_dock_areas(panel)

    def _create_frame_hierarchy_panel(self): #vers 2
        """Create the Frame Hierarchy panel on its own - previously stacked
        inside the same panel as the model table/textures, now a separate
        top-level dock widget so it can be independently dragged/floated.
        No internal header row - the dock's own title bar already shows
        'Frame Hierarchy', and collapsing is now done by double-clicking
        that title bar (see DockPanelsMixin._make_dock_collapsible) instead of a per-panel
        toggle button."""
        from PyQt6.QtWidgets import QTreeWidget
        self._frame_tree_panel = QFrame()
        self._frame_tree_panel.setFrameStyle(QFrame.Shape.StyledPanel)
        ft_lay = QVBoxLayout(self._frame_tree_panel)
        ft_lay.setContentsMargins(2, 2, 2, 2)
        ft_lay.setSpacing(2)

        self._frame_tree = QTreeWidget()
        self._frame_tree.setHeaderLabels(["Frame", "Parent", "Position"])
        self._frame_tree.setAlternatingRowColors(True)
        self._frame_tree.setFont(self.panel_font)
        self._frame_tree.itemClicked.connect(self._on_frame_tree_clicked)
        ft_lay.addWidget(self._frame_tree)

        return self._frame_tree_panel


    def _wrap_middle_panel_with_own_dock_areas(self, content_panel): #vers 4
        """Give the middle panel its own nested QMainWindow (same pattern
        the right panel already uses for the viewport), so the Name and
        IDE/TXD ribbons have real Left/Right toolbar dock areas specifically
        flanking the middle panel's own content - not just the option of
        living inside it as a plain widget, or all the way over on the
        far-right ribbon stack. Both default to the bottom (closest to
        their old combined position), but can be dragged to either side
        independently within this mini QMainWindow, or moved to the main
        right panel via the existing right-click action."""
        from PyQt6.QtWidgets import QMainWindow
        middle_mw = QMainWindow()
        middle_mw.setWindowFlags(Qt.WindowType.Widget)
        middle_mw.setDockOptions(
            QMainWindow.DockOption.AllowNestedDocks |
            QMainWindow.DockOption.AllowTabbedDocks)
        # Same explicit separator styling as outer_mw - prevents the
        # docked-mode theme-cascade issue that zeroed out separator
        # visibility from affecting this nested QMainWindow too.
        middle_mw.setStyleSheet(
            "QMainWindow::separator { "
            "background: palette(mid); width: 5px; height: 5px; } "
            "QMainWindow::separator:hover { background: palette(highlight); }")
        middle_mw.setCentralWidget(content_panel)
        middle_mw.addToolBar(Qt.ToolBarArea.BottomToolBarArea, self._info_ribbons['name'])
        # Explicit break so IDE always sits on its own row below Name,
        # instead of sharing one row whenever there's enough width.
        middle_mw.addToolBarBreak(Qt.ToolBarArea.BottomToolBarArea)
        middle_mw.addToolBar(Qt.ToolBarArea.BottomToolBarArea, self._info_ribbons['ide'])
        self._middle_mw = middle_mw
        return middle_mw

    def _build_model_name_toolbar(self): #vers 2
        """Build the 'Model Name: Click to edit...' ribbon on its own,
        split out from the combined Model Info ribbon so it can be
        independently docked/moved from the IDE/TXD one."""
        from PyQt6.QtWidgets import QToolBar
        tb = QToolBar("Model Name")
        tb.setObjectName("Model Name")
        tb.setIconSize(QSize(18, 18))
        tb.setMovable(True)
        tb.setFloatable(True)
        tb.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        tb.customContextMenuRequested.connect(
            lambda pos, t=tb, key='name': self._info_ribbon_menu(t, pos, key))

        name_label = QLabel("Model Name:")
        name_label.setFont(self.panel_font)
        tb.addWidget(name_label)

        self.info_name = QLineEdit()
        self.info_name.setText("Click to edit...")
        self.info_name.setFont(self.panel_font)
        self.info_name.setReadOnly(True)
        self.info_name.setMinimumWidth(120)
        self.info_name.setStyleSheet("padding: 2px; border: 1px solid palette(mid);")
        self.info_name.mousePressEvent = lambda e: self._enable_name_edit(e, False)
        tb.addWidget(self.info_name)

        return tb

    def _build_model_ide_toolbar(self, icon_color): #vers 2
        """Build the 'IDE: -- ID: -- TXD: --' ribbon on its own, split out
        from the combined Model Info ribbon so it can be independently
        docked/moved from the Name one."""
        from PyQt6.QtWidgets import QToolBar
        from PyQt6.QtGui import QAction
        tb = QToolBar("Model IDE / TXD")
        tb.setObjectName("Model IDE / TXD")
        tb.setIconSize(QSize(18, 18))
        tb.setMovable(True)
        tb.setFloatable(True)
        tb.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        tb.customContextMenuRequested.connect(
            lambda pos, t=tb, key='ide': self._info_ribbon_menu(t, pos, key))

        ide_lbl = QLabel("IDE:")
        ide_lbl.setFont(self.panel_font)
        tb.addWidget(ide_lbl)
        self.info_ide_section = QLabel("—")
        self.info_ide_section.setFont(self.panel_font)
        tb.addWidget(self.info_ide_section)
        self.info_model_id = QLabel("ID: —")
        self.info_model_id.setFont(self.panel_font)
        tb.addWidget(self.info_model_id)
        txd_lbl = QLabel("TXD:")
        txd_lbl.setFont(self.panel_font)
        tb.addWidget(txd_lbl)
        self.info_txd_name = QLabel("—")
        tb.addWidget(self.info_txd_name)

        self.find_in_ide_btn = QAction(
            self.icon_factory.database_icon(color=icon_color), "Find in IDE", tb)
        self.find_in_ide_btn.setToolTip("Look up model in DAT Browser IDE entries")
        self.find_in_ide_btn.triggered.connect(self._find_in_ide)
        tb.addAction(self.find_in_ide_btn)

        return tb

    def _info_ribbon_menu(self, tb, pos, key): #vers 1
        """Right-click menu on the Model Name / IDE-TXD ribbons - lets the
        user move either one independently between the middle panel
        (default) and the right panel's ribbon stack. A plain QToolBar
        can't natively drag-and-dock between two separate QMainWindows,
        so this is a deliberate action instead."""
        from PyQt6.QtWidgets import QMenu
        menu = QMenu(self)
        if self._info_ribbon_location.get(key, 'middle') == 'middle':
            menu.addAction("Move to Right Panel Ribbons",
                            lambda: self._move_info_ribbon(key, 'right'))
        else:
            menu.addAction("Move to Middle Panel",
                            lambda: self._move_info_ribbon(key, 'middle'))
        menu.exec(tb.mapToGlobal(pos))

    def _move_info_ribbon(self, key: str, location: str): #vers 3
        """Reparent one of the Name/IDE-TXD toolbars (identified by key,
        'name' or 'ide') between the middle panel's own nested QMainWindow
        and the right panel's QMainWindow - dockable/floatable in both
        places. Persists the choice per-ribbon to model_workshop.json."""
        tb = self._info_ribbons.get(key)
        if tb is None or location == self._info_ribbon_location.get(key):
            return
        if location == 'right':
            mid_mw = getattr(self, '_middle_mw', None)
            if mid_mw:
                mid_mw.removeToolBar(tb)
            tb.setParent(None)
            mw = getattr(self, '_inner_mw', None)
            if mw:
                mw.addToolBar(Qt.ToolBarArea.RightToolBarArea, tb)
                tb.setVisible(True)
        else:
            mw = getattr(self, '_inner_mw', None)
            if mw:
                mw.removeToolBar(tb)
            tb.setParent(None)
            mid_mw = getattr(self, '_middle_mw', None)
            if mid_mw:
                mid_mw.addToolBar(Qt.ToolBarArea.BottomToolBarArea, tb)
                tb.setVisible(True)
        self._info_ribbon_location[key] = location
        try:
            import json
            from pathlib import Path
            path = self._ribbon_config_path()
            try:
                data = json.loads(path.read_text())
            except Exception:
                data = {}
            locations = data.get('info_ribbon_locations', {})
            locations[key] = location
            data['info_ribbon_locations'] = locations
            path.write_text(json.dumps(data, indent=2))
        except Exception:
            pass
        self._set_status(f"Model Info ribbon moved to {location} panel")

    def _create_right_panel(self): #vers 15
        """Right panel using QMainWindow + QToolBar for native docking.
        QMainWindow handles toolbar placement, row stacking, floating, and
        save/restore natively — same system Gwenview/KDE apps use."""
        icon_color = self._get_icon_color()

        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(200)
        self._right_panel_ref = panel
        outer_layout = QVBoxLayout(panel)
        outer_layout.setContentsMargins(4, 4, 4, 4)
        outer_layout.setSpacing(3)

        # Inner QMainWindow — owns the viewport as central widget and all
        # QToolBars. Embedded as a plain widget (no window chrome).
        from PyQt6.QtWidgets import QMainWindow
        inner_mw = QMainWindow()
        inner_mw.setWindowFlags(Qt.WindowType.Widget)
        inner_mw.setDockOptions(
            QMainWindow.DockOption.AllowNestedDocks |
            QMainWindow.DockOption.AllowTabbedDocks)
        # Same explicit separator styling as outer_mw/middle_mw - guards
        # against the same docked-mode theme-cascade issue, in case any
        # dock widgets get added here in future.
        inner_mw.setStyleSheet(
            "QMainWindow::separator { "
            "background: palette(mid); width: 5px; height: 5px; } "
            "QMainWindow::separator:hover { background: palette(highlight); }")
        self._inner_mw = inner_mw

        # Central widget — the 3D viewport, wrapped in a stack so the
        # 4-Pane view (Top/Front/Side/Perspective) can be swapped in without
        # disturbing QMainWindow's central-widget ownership.
        from PyQt6.QtWidgets import QStackedWidget
        self.preview_widget = DFFViewport()
        self.preview_widget._workshop_ref = self
        self._viewport_stack = QStackedWidget()
        self._viewport_stack.addWidget(self.preview_widget)   # index 0: single view
        inner_mw.setCentralWidget(self._viewport_stack)
        self._gl_viewport  = self.preview_widget
        self._qp_viewport  = self.preview_widget
        self._gl_mode      = True

        self._create_paint_bar()

        # Build all toolbars and add to the inner QMainWindow
        self._build_toolbars(inner_mw, icon_color)
        self._apply_custom_icons()

        outer_layout.addWidget(inner_mw, stretch=1)

        # Restore saved toolbar state (positions, rows, floating)
        from PyQt6.QtCore import QTimer as _QT
        _QT.singleShot(400, self._restore_toolbar_state)

        # Save on close
        self.window_closed.connect(self._save_toolbar_state)
        self.window_closed.connect(self._save_quad_layout)

        return panel

    def _build_toolbars(self, mw: 'QMainWindow', icon_color: str): #vers 17
        """Build all QToolBar instances using QAction.
        Icon set resolved once — 'default' uses SVGIconFactory with currentColor,
        '3dsmax' uses MaxIconSet with hardcoded Max palette."""
        from PyQt6.QtWidgets import QToolBar
        icon_size = QSize(20, 20)
        pw = self.preview_widget
        self._ribbon_actions = []

        # Resolve icon set once for all buttons
        _icon_set = self._get_icon_set()
        if _icon_set == '3dsmax':
            from apps.methods.max_icon_set import MaxIconSet as _IS
        else:
            _IS = None

        def _icon(default_fn, max_name: str = None):  #vers 1
            """Return correct icon function for active icon set."""
            if _IS and max_name and hasattr(_IS, max_name):
                return getattr(_IS, max_name)
            return default_fn

        def _tb(name, area=Qt.ToolBarArea.TopToolBarArea):  #vers 1
            tb = QToolBar(name, mw)
            tb.setObjectName(name)
            tb.setIconSize(icon_size)
            tb.setMovable(True)
            tb.setFloatable(True)
            tb.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
            tb.customContextMenuRequested.connect(
                lambda pos, t=tb: self._toolbar_context_menu(t, pos))
            mw.addToolBar(area, tb)
            return tb

        def _act(tb, name, icon_fn, callback=None, checkable=False,
                 checked=False, attr=None):  #vers 1
            """Create a QAction, add to toolbar, register in _ribbon_actions."""
            try:
                icon = icon_fn(color=icon_color)
            except Exception:
                icon = self.icon_factory.settings_icon(color=icon_color)
            act = QAction(icon, name, mw)
            act.setToolTip(name)
            act.setCheckable(checkable)
            if checkable:
                act.setChecked(checked)
            if callback:
                if checkable:
                    act.toggled.connect(callback)
                else:
                    act.triggered.connect(callback)
            tb.addAction(act)
            self._ribbon_actions.append({
                'action':    act,
                'toolbar':   tb,
                'name':      name,
                'icon_fn':   icon_fn,
                'checkable': checkable,
            })
            if attr:
                setattr(self, attr, act)
            return act

        # - Ribbon 1: Selection
        tb_sel = _tb("Selection")
        from PyQt6.QtGui import QAction, QActionGroup
        from PyQt6.QtWidgets import QLabel as _QL

        # V/E/F/P as exclusive QActions in an action group
        sel_group = QActionGroup(mw)
        sel_group.setExclusive(True)

        def _sel_act(attr, name, mode, icon_fn_name, max_name=None):  #vers 1
            act = QAction(name, mw)
            try:
                fn = _icon(getattr(self.icon_factory, icon_fn_name), max_name or icon_fn_name)
                act.setIcon(fn(color=icon_color))
            except Exception:
                pass
            act.setToolTip(name)
            act.setCheckable(True)
            act.setEnabled(False)
            act.triggered.connect(lambda _=False, m=mode: self._set_select_mode(m))
            sel_group.addAction(act)
            tb_sel.addAction(act)
            self._ribbon_actions.append({
                'action': act, 'toolbar': tb_sel, 'name': name,
                'icon_fn': getattr(self.icon_factory, icon_fn_name, None),
                'checkable': True,
            })
            setattr(self, attr, act)
            return act

        _sel_act('_sel_obj_act',  'Object Select (finish editing)', 'object', 'dp_select_icon')
        self._sel_obj_act.setEnabled(True)
        _sel_act('_sel_vert_act', 'Vertex Select', 'vertex', 'vertex_select_icon', 'vertex_select_icon')
        _sel_act('_sel_edge_act', 'Edge Select',   'edge',   'edge_select_icon',   'edge_select_icon')
        _sel_act('_sel_face_act', 'Face Select',   'face',   'face_select_icon',   'face_select_icon')
        _sel_act('_sel_poly_act', 'Polygon Select','poly',   'poly_select_icon',   'poly_select_icon')
        self._sel_obj_act.setChecked(True)          # viewport starts in object mode
        self._select_mode_group = sel_group

        # Selection count label — QToolBar widget (labels can't be QAction)
        self._sel_count_label = _QL("")
        self._sel_count_label.setToolTip("Selection count")
        self._sel_count_label.setStyleSheet(
            "color: palette(mid); font-size: 11px; padding: 0 4px;")
        _cnt_act = tb_sel.addWidget(self._sel_count_label)
        from apps.methods.imgfactory_svg_icons import get_select_all_icon
        _cnt_act.setIcon(get_select_all_icon(20, icon_color))   # Ribbon Manager row icon
        _cnt_act.setText("Selection count")

        tb_sel.addSeparator()

        _act(tb_sel, "Backface Culling",
             _icon(self.icon_factory.backface_icon, 'backface_cull_icon'),
             lambda v: self._toggle_backface_cull(),
             checkable=True, attr='_backface_cull_act')

        _act(tb_sel, "Front-only Paint",
             self.icon_factory.view_icon,
             lambda v: self._toggle_front_only_paint(),
             checkable=True, attr='_front_paint_act')

        # - Ribbon 2: Snap Targets

        from apps.methods.max_svg_icons import MaxSVGIcons
        tb_snap = _tb("Snap Targets")

        def _snap_act(attr, target, name, icon_fn_name, max_name=None):  #vers 1
            # Resolve icon — Max set overrides MaxSVGIcons snap icons
            if _IS and max_name and hasattr(_IS, max_name):
                try:
                    icon = getattr(_IS, max_name)(size=20)
                except Exception as _e:
                    print(f"[max snap icon] {max_name}: {_e}")
                    icon = self.icon_factory.settings_icon(color=icon_color)
            else:
                try:
                    icon = getattr(MaxSVGIcons, icon_fn_name)(size=20, color=icon_color)
                except Exception as _e:
                    print(f"[snap icon] {icon_fn_name}: {_e}")
                    icon = self.icon_factory.settings_icon(color=icon_color)
            act = QAction(icon, name, mw)
            act.setToolTip(name)
            act.setCheckable(True)
            def _on(checked, t=target, a=act):  #vers 1
                vp = getattr(self, 'preview_widget', None)
                if vp and hasattr(vp, '_snap_targets'):
                    vp.toggle_snap_target(t)
                    a.setChecked(vp._snap_targets.get(t, False))
            act.toggled.connect(_on)
            tb_snap.addAction(act)
            _fn = getattr(MaxSVGIcons, icon_fn_name, None)
            self._ribbon_actions.append({
                'action': act, 'toolbar': tb_snap, 'name': name,
                'icon_fn': (lambda sz=20, c=icon_color, fn=_fn:
                            fn(size=sz, color=c)) if _fn else None,
                'checkable': True,
            })
            setattr(self, attr, act)
            return act

        _snap_act('_snap_grid_act',     'grid',     'Snap: Grid Points', 'snap_grid_icon',     'snap_grid_icon')
        _snap_act('_snap_pivot_act',    'pivot',    'Snap: Pivot',       'snap_pivot_icon',    'snap_pivot_icon')
        _snap_act('_snap_vertex_act',   'vertex',   'Snap: Vertex',      'snap_vertex_icon',   'snap_vertex_icon')
        _snap_act('_snap_endpoint_act', 'endpoint', 'Snap: Endpoint',    'snap_endpoint_icon', 'snap_endpoint_icon')
        _snap_act('_snap_midpoint_act', 'midpoint', 'Snap: Midpoint',    'snap_midpoint_icon', 'snap_midpoint_icon')
        _snap_act('_snap_edge_act',     'edge',     'Snap: Edge',        'snap_edge_icon',     'snap_edge_icon')
        _snap_act('_snap_face_act',     'face',     'Snap: Face',        'snap_face_icon',     'snap_face_icon')
        tb_snap.addSeparator()
        _act(tb_snap, "Axis Constraints",
             _icon(lambda color=icon_color: MaxSVGIcons.snap_axis_constraint_icon(
                 size=20, color=color), 'snap_axis_icon'),
             checkable=True, attr='_snap_axis_act')
        _act(tb_snap, "Angle Snap",
             _icon(lambda color=icon_color: MaxSVGIcons.snap_angle_icon(
                 size=20, color=color), 'snap_angle_icon'),
             checkable=True, attr='_snap_angle_act')
        _act(tb_snap, "Percent Snap",
             _icon(lambda color=icon_color: MaxSVGIcons.snap_percent_icon(
                 size=20, color=color), 'snap_percent_icon'),
             checkable=True, attr='_snap_percent_act')

        # - Ribbon 3: Edit Geometry
        tb_geo = _tb("Edit Geometry")
        _act(tb_geo, "Edit Model (mesh edit, Face mode)", self.icon_factory.edit_model_icon,
             callback=lambda *_: self._edit_model(), attr='_edit_model_act')
        self._edit_model_act.setEnabled(False)
        _act(tb_geo, "Create Primitive",
             _icon(lambda color=icon_color: MaxSVGIcons.create_primitive_icon(
                 size=20, color=color), 'create_primitive_icon'),
             callback=self._create_primitive_dialog, attr='_prim_act')
        _act(tb_geo, "Extrude Faces",
             _icon(lambda color=icon_color: MaxSVGIcons.extrude_icon(
                 size=20, color=color), 'extrude_icon'),
             callback=self._extrude_dialog, attr='_extrude_act')
        tb_geo.addSeparator()
        _act(tb_geo, "Mirror",
             _icon(lambda color=icon_color: MaxSVGIcons.mirror_icon(
                 size=20, color=color), 'mirror_icon'),
             callback=self._mirror_dialog, attr='_mirror_act')
        _act(tb_geo, "Align",
             _icon(lambda color=icon_color: MaxSVGIcons.align_icon(
                 size=20, color=color), 'align_icon'),
             callback=self._align_dialog, attr='_align_act')
        _act(tb_geo, "Mesh Edit",
             _icon(self.icon_factory.mesh_icon, 'mesh_icon'),
             callback=lambda *_: self._mesh_menu(), attr='_mesh_act')
        self._mesh_act.setToolTip("Mesh edit tools (also right-click the viewport in Vertex/Face mode)")

        # - Mesh ribbons (Oct 12 2026): COL Workshop edit tools on DFF geometry
        from apps.methods.imgfactory_svg_icons import get_select_all_icon, get_select_inverse_icon, list_icon
        IF = self.icon_factory
        mw.addToolBarBreak(Qt.ToolBarArea.TopToolBarArea)      # mesh ribbons on their own row
        for group, entries in (
            ("Mesh Select", [
                ("Select All Vertices",        lambda color=None: get_select_all_icon(20, color),
                 lambda *_: self._mesh_select_verts('all')),
                ("Select No Vertices",         IF.select_none_icon,     lambda *_: self._mesh_select_verts('none')),
                ("Invert Vertex Selection",    lambda color=None: get_select_inverse_icon(20, color),
                 lambda *_: self._mesh_select_verts('invert')),
                ("Vertices of Selected Faces", IF.vertex_select_icon,   lambda *_: self._mesh_select_verts('from_faces')),
                ("Faces of Selected Vertices", IF.select_inside_icon,   lambda *_: self._mesh_verts_to_faces()),
                ("Select by Material",         IF.select_material_icon, lambda *_: self._mesh_select_material()),
                ("Hide Selected Faces",        IF.hide_faces_icon,      lambda *_: self._mesh_hide_selected()),
                ("Unhide All Faces",           IF.unhide_faces_icon,    lambda *_: self._mesh_unhide_all()),
                ("Circle Region Select",       IF.region_circle_icon,   self._mesh_region_circle, '_mesh_circle_act'),
                ("Window Face Select",         IF.region_window_icon,   self._mesh_region_window, '_mesh_window_act'),
                ("Show Vertices",              IF.show_vertices_icon,   self._mesh_show_vertices, '_mesh_showv_act'),
                ("Selection Lock [Space]",     IF.selection_lock_icon,  self._mesh_toggle_lock, '_mesh_lock_act'),
                ("Game Controller",            IF.controller_icon,      self._mesh_toggle_gamepad, '_mesh_gamepad_act'),
            ]),
            ("Mesh Edit", [
                ("Position / Move...",         IF.vertex_position_icon, lambda *_: self._mesh_move_dialog()),
                ("Rotate...",                  IF.rotate_cw_icon,       lambda *_: self._mesh_rotate_dialog()),
                ("Scale...",                   IF.bounds_icon,          lambda *_: self._mesh_scale_dialog()),
                ("Mirror...",                  IF.mirror_icon,          lambda *_: self._mirror_dialog()),
                ("Centre to Origin",           IF.snap_to_center_icon,  lambda *_: self._mesh_centre_origin()),
                ("Detach Selected Faces",      IF.detach_faces_icon,    lambda *_: self._mesh_detach()),
                ("Detached Parts...",          lambda color=None: list_icon(20, color),
                 lambda *_: self._mesh_detached_dialog()),
                ("Delete Selected Faces",      IF.delete_face_icon,     lambda *_: self._mesh_delete_faces()),
                ("Weld Selected Vertices",     IF.converge_to_center_icon, lambda *_: self._mesh_weld()),
                ("Create Face from 3 Vertices", IF.create_face_icon,    lambda *_: self._mesh_add_face()),
                ("Delete Selected Vertices",   IF.delete_vertex_icon,   lambda *_: self._mesh_delete_vertices()),
                ("Split Selected Faces",       IF.split_faces_icon,     lambda *_: self._mesh_split()),
                ("Fill Hole",                  IF.fill_icon,            lambda *_: self._mesh_fill_hole()),
                ("UV Map...",                  IF.uv_map_icon,          lambda *_: self._mesh_uv_dialog()),
                ("Face Move: Keep Connected",  IF.faces_connected_icon,
                 lambda on: on and self._mesh_set_face_mode('connected'), '_mesh_fm_connected_act'),
                ("Face Move: Detach",          IF.detach_faces_icon,
                 lambda on: on and self._mesh_set_face_mode('detach'), '_mesh_fm_detach_act'),
                ("Face Move: Extrude",         IF.faces_extrude_icon,
                 lambda on: on and self._mesh_set_face_mode('extrude'), '_mesh_fm_extrude_act'),
            ]),
            ("Mesh Optimise", [
                ("Clean (keep UV seams)",      IF.filter_icon,          lambda *_: self._mesh_optimise('clean')),
                ("Merge Flat Areas",           IF.merge_files_icon,     lambda *_: self._mesh_optimise('merge')),
                ("Decimate...",                IF.split_faces_icon,     lambda *_: self._mesh_optimise('decimate')),
                ("Delete Isolated Vertices",   IF.isolated_verts_icon,  lambda *_: self._mesh_optimise('isolated')),
            ]),
            ("Mesh Geometry", [
                ("Selection to New Geometry",  IF.new_icon,             lambda *_: self._mesh_selection_to_geometry()),
                ("Save Selection as DFF...",   IF.save_selection_icon,  lambda *_: self._mesh_selection_to_file()),
                ("Copy as LOD...",             IF.copy_lod_icon,        lambda *_: self._mesh_copy_as_lod()),
                ("Optimum Bounds",             IF.optimum_bounds_icon,  lambda *_: self._mesh_optimum_bounds()),
            ]),
            ("Mesh Lighting", [
                ("Generate Lighting...",       IF.lighting_icon,        lambda *_: self._mesh_lighting()),
                ("Light View",                 IF.light_view_icon,      self._mesh_light_view, '_mesh_light_act'),
            ])):
            if group == "Mesh Geometry":
                mw.addToolBarBreak(Qt.ToolBarArea.TopToolBarArea)
            tb_mesh = _tb(group)
            for name, icon_fn, cb, *attr in entries:          # attr given: checkable toggle
                _act(tb_mesh, name, icon_fn, callback=cb, checkable=bool(attr), attr=attr[0] if attr else None)
        self.preview_widget._lock_key_callback = self._mesh_lock_act.toggle
        from PyQt6.QtGui import QActionGroup
        fm_group = QActionGroup(self)
        fm_group.setExclusive(True)
        for m in ('connected', 'detach', 'extrude'):
            fm_group.addAction(getattr(self, f'_mesh_fm_{m}_act'))
        self._mesh_fm_connected_act.blockSignals(True)
        self._mesh_fm_connected_act.setChecked(True)
        self._mesh_fm_connected_act.blockSignals(False)
        from apps.methods.gamepad_input import gamepad_saved
        if gamepad_saved('model_workshop.json'):
            self._mesh_gamepad_act.setChecked(True)

        # - Ribbon 4: Navigation
        tb_nav = _tb("Navigation", Qt.ToolBarArea.RightToolBarArea)
        _act(tb_nav, "Zoom In",       _icon(self.icon_factory.zoom_in_icon,   'zoom_in_icon'),   pw.zoom_in)
        _act(tb_nav, "Zoom Out",      _icon(self.icon_factory.zoom_out_icon,  'zoom_out_icon'),  pw.zoom_out)
        _act(tb_nav, "Reset View",    _icon(self.icon_factory.reset_view_icon,'reset_view_icon'),pw.reset_view)
        _act(tb_nav, "Fit to Window", _icon(self.icon_factory.fit_icon,       'fit_view_icon'),  pw.fit_to_window)
        tb_nav.addSeparator()
        for label, yaw, pitch, default_fn, max_name in [
            ("View XY",  0,   0,  self.icon_factory.view_xy_icon,  'view_xy_icon'),
            ("View XZ",  0,  90,  self.icon_factory.view_xz_icon,  'view_xz_icon'),
            ("View YZ",  90,  0,  self.icon_factory.view_yz_icon,  'view_yz_icon'),
            ("View Iso", 30, 20,  self.icon_factory.view_iso_icon, 'view_iso_icon'),
        ]:
            def _set_v(checked=False, y=yaw, p=pitch):  #vers 1
                pw._yaw = y; pw._pitch = p; pw.update()
            _act(tb_nav, label, _icon(default_fn, max_name), _set_v)
        tb_nav.addSeparator()
        _act(tb_nav, "4-Pane View",
             _icon(self.icon_factory.quad_view_icon, 'quad_view_icon'),
             self._toggle_quad_view, checkable=True, attr='_quad_view_act')

        # - Ribbon 5: Render
        tb_rend = _tb("Render", Qt.ToolBarArea.RightToolBarArea)
        _act(tb_rend, "Render Settings",
             _icon(self.icon_factory.render_settings_icon, 'render_settings_icon'),
             self._open_render_settings_dialog)
        _act(tb_rend, "Toggle Mesh",
             _icon(self.icon_factory.mesh_icon, 'toggle_mesh_icon'),
             lambda v: pw.set_show_mesh(v),
             checkable=True, checked=True, attr='_view_mesh_act')
        _act(tb_rend, "Toggle Backface",
             _icon(self.icon_factory.toggle_backface_icon, 'toggle_backface_icon'),
             lambda v: pw.set_backface(v),
             checkable=True, attr='_backface_act')
        _act(tb_rend, "Cycle Render Style",
             _icon(self.icon_factory.render_style_icon, 'render_style_icon'),
             self._cycle_view_render_style)
        tb_rend.addSeparator()
        _act(tb_rend, "Toggle Shading",
             _icon(self.icon_factory.shading_sphere_icon, 'shading_icon'),
             lambda v: self._toggle_viewport_shading(v),
             checkable=True, checked=True, attr='_shading_act')
        _act(tb_rend, "Light Setup",
             _icon(self.icon_factory.light_icon, 'light_setup_icon'),
             self._open_light_setup_dialog, attr='_light_setup_act')
        tb_rend.addSeparator()
        _act(tb_rend, "Open GL Model Viewer (hardware 3D)",
             _icon(self.icon_factory.viewport_icon, 'gl_viewer_icon'),
             self._open_gl_viewer, attr='_gl_viewer_act')

        # Store toolbar refs
        self._tb_selection = tb_sel
        self._tb_snap      = tb_snap
        self._tb_geometry  = tb_geo
        self._tb_nav       = tb_nav
        self._tb_render    = tb_rend

        # DFF-only actions — disabled until a model is loaded
        self._dff_only_actions = [
            self._sel_vert_act, self._sel_edge_act,
            self._sel_face_act, self._sel_poly_act,
            self._backface_cull_act, self._front_paint_act,
            self._prim_act, self._extrude_act,
            self._shading_act, self._light_setup_act,
        ]
        # Legacy compat for code that checks _dff_only_toolbar_btns
        self._dff_only_toolbar_btns = []

        # Compatibility attrs for code using old QPushButton names
        self._sel_vert_btn   = None
        self._sel_edge_btn   = None
        self._sel_face_btn   = None
        self._sel_poly_btn   = None

    def _toolbar_context_menu(self, toolbar, pos): #vers 4
        """Right-click context menu on any toolbar."""
        from PyQt6.QtWidgets import QMenu
        menu = QMenu(self)

        # Icon Set submenu
        icon_set_menu = menu.addMenu("Icon Set")
        current = self._get_icon_set()
        for label, key in [("Default (theme-aware)", 'default'),
                            ("3ds Max style",         '3dsmax')]:
            act = icon_set_menu.addAction(label)
            act.setCheckable(True)
            act.setChecked(current == key)
            act.triggered.connect(lambda _, k=key: self._switch_icon_set(k))

        # Icon Size submenu
        size_menu = menu.addMenu("Icon Size")
        from PyQt6.QtWidgets import QSlider, QWidgetAction
        slider = QSlider(Qt.Orientation.Horizontal)
        slider.setRange(14, 40)
        slider.setSingleStep(2)
        try:
            import json
            from pathlib import Path
            data = json.loads((self._ribbon_config_path()).read_text())
            slider.setValue(data.get('icon_scale', 20))
        except Exception:
            slider.setValue(20)
        slider.valueChanged.connect(self._apply_icon_scale)
        wa = QWidgetAction(menu)
        wa.setDefaultWidget(slider)
        size_menu.addAction(wa)

        menu.addSeparator()
        menu.addAction("Ribbon Manager...", self.open_ribbon_manager)
        menu.addSeparator()
        from PyQt6.QtWidgets import QToolBar as _QTB
        menu.addAction("Lock All Toolbars",
            lambda: [tb.setMovable(False)
                     for tb in self._inner_mw.findChildren(_QTB)])
        menu.addAction("Unlock All Toolbars",
            lambda: [tb.setMovable(True)
                     for tb in self._inner_mw.findChildren(_QTB)])
        menu.exec(toolbar.mapToGlobal(pos))

    def _switch_icon_set(self, key: str): #vers 3
        """Save icon set choice and rebuild toolbars to apply it."""
        try:
            import json
            from pathlib import Path
            path = self._ribbon_config_path()
            try:
                data = json.loads(path.read_text())
            except Exception:
                data = {}
            data['icon_set'] = key
            path.write_text(json.dumps(data, indent=2))
        except Exception as _e:
            print(f"[icon_set] save failed: {_e}")
        # Rebuild toolbars live
        self._rebuild_toolbars()

    def _rebuild_toolbars(self): #vers 1
        """Remove all existing toolbars and rebuild with the current icon set."""
        mw = getattr(self, '_inner_mw', None)
        if mw is None:
            return
        from PyQt6.QtWidgets import QToolBar
        for tb in list(mw.findChildren(QToolBar)):
            mw.removeToolBar(tb)
            tb.deleteLater()
        self._ribbon_actions = []
        icon_color = self._get_icon_color()
        self._build_toolbars(mw, icon_color)
        self._apply_custom_icons()
        self._set_status(f"Icon set applied")

    def _apply_icon_scale(self, px: int): #vers 3
        """Apply icon size to all toolbars live and persist it."""
        mw = getattr(self, '_inner_mw', None)
        if mw:
            from PyQt6.QtWidgets import QToolBar
            from PyQt6.QtCore import QSize
            for tb in mw.findChildren(QToolBar):
                tb.setIconSize(QSize(px, px))
        try:
            import json
            from pathlib import Path
            path = self._ribbon_config_path()
            try:
                data = json.loads(path.read_text())
            except Exception:
                data = {}
            data['icon_scale'] = px
            path.write_text(json.dumps(data, indent=2))
        except Exception:
            pass

    def _ribbon_config_path(self): #vers 1
        """Ribbon layout, presets and custom icons file."""
        return get_user_config_dir() / 'model_workshop.json'

    def _save_toolbar_state(self): #vers 4
        """Save QMainWindow toolbar state to model_workshop.json."""
        mw = getattr(self, '_inner_mw', None)
        if mw is None:
            return
        try:
            import json
            from pathlib import Path
            path = self._ribbon_config_path()
            try:
                data = json.loads(path.read_text())
            except Exception:
                data = {}
            data['toolbar_state'] = mw.saveState(self._RIBBON_LAYOUT_VERSION).toHex().data().decode()
            data['toolbar_state_version'] = self._RIBBON_LAYOUT_VERSION
            path.write_text(json.dumps(data, indent=2))
            self._set_status("Ribbon config saved")
            main_wnd = getattr(self, 'main_window', None)
            if main_wnd and hasattr(main_wnd, 'log_message'):
                main_wnd.log_message("Model Workshop: Ribbon config saved")
        except Exception as _e:
            print(f"[ModelWorkshop] _save_toolbar_state error: {_e}")

    def _restore_toolbar_state(self): #vers 8
        """Restore QMainWindow toolbar state from model_workshop.json.
        Uses an explicit layout version - bumped whenever ribbons are
        added/removed/renamed - so a stale save from an older ribbon
        layout is cleanly rejected instead of silently failing to
        restore (Qt's own toolbar-name hashing does this invisibly and
        without any way to detect success/failure)."""
        mw = getattr(self, '_inner_mw', None)
        if mw is None:
            return
        try:
            import json
            from pathlib import Path
            from PyQt6.QtCore import QByteArray
            path = self._ribbon_config_path()
            if not path.exists():
                return
            data = json.loads(path.read_text())
            if 'icon_scale' in data:                      # saved ribbon icon size
                from PyQt6.QtCore import QSize
                from PyQt6.QtWidgets import QToolBar
                for tb in mw.findChildren(QToolBar):
                    tb.setIconSize(QSize(data['icon_scale'], data['icon_scale']))
            state_hex = data.get('toolbar_state')
            saved_version = data.get('toolbar_state_version')
            if state_hex and saved_version == self._RIBBON_LAYOUT_VERSION:
                ok = mw.restoreState(QByteArray.fromHex(state_hex.encode()),
                                      self._RIBBON_LAYOUT_VERSION)
                if ok:
                    self._set_status("Ribbon config loaded")
                    main_wnd = getattr(self, 'main_window', None)
                    if main_wnd and hasattr(main_wnd, 'log_message'):
                        main_wnd.log_message("Model Workshop: Ribbon config loaded")
                else:
                    print("[ModelWorkshop] _restore_toolbar_state: restoreState() returned False")
            elif state_hex:
                print(f"[ModelWorkshop] Saved ribbon layout is from an older version "
                      f"({saved_version} != {self._RIBBON_LAYOUT_VERSION}) - skipping, "
                      f"will save fresh on next change.")
        except Exception as _e:
            print(f"[ModelWorkshop] _restore_toolbar_state error: {_e}")
        finally:
            # Safety net: restoreState() can leave a ribbon fully hidden
            # (e.g. saved mid-drag, floating off-screen, squeezed out) with
            # no user-facing 'closed' state for these ribbons to have meant
            # intentionally - so force every one of them visible no matter
            # what happened above. Only position/floating/row should be
            # affected by the saved state, never full visibility.
            for tb in (getattr(self, '_tb_selection', None),
                       getattr(self, '_tb_snap', None),
                       getattr(self, '_tb_geometry', None),
                       getattr(self, '_tb_nav', None),
                       getattr(self, '_tb_render', None)):
                if tb is not None:
                    tb.setVisible(True)
                    tb.toggleViewAction().setChecked(True)

    def _create_paint_bar(self): #vers 3
        """Floating paint bar — QWidget child of preview_widget, sits at top of viewport.
        Called once from _create_right_panel after preview_widget is created."""
        vp = self.preview_widget
        ic = self._get_icon_color()

        bar = QWidget(vp)
        bar.setObjectName("paint_bar")
        bar.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
        bar.setStyleSheet(
            "QWidget#paint_bar { background:palette(base); border-bottom:2px solid #ff8c00; }"
            "QLabel  { color:#ddd; background:transparent; }"
            "QComboBox { background:palette(base); color:palette(windowText); border:1px solid palette(mid); }"
            "QPushButton { background:palette(base); color:palette(windowText); border:1px solid palette(mid); border-radius:3px; }"
            "QPushButton:hover   { background:#353548; }"
            "QPushButton:checked { background:#ff8c00; color:#000; border:1px solid #ff8c00; }"
        )
        bar.setFixedHeight(34)

        lay = QHBoxLayout(bar)
        lay.setContentsMargins(6, 3, 6, 3)
        lay.setSpacing(4)

        lay.addWidget(QLabel("Mat:"))

        self.paint_swatch = QLabel()
        self.paint_swatch.setFixedSize(16, 16)
        self.paint_swatch.setStyleSheet(
            "background:#808080; border:1px solid palette(mid); border-radius:2px;")
        lay.addWidget(self.paint_swatch)

        self.paint_mat_combo = QComboBox()
        self.paint_mat_combo.setFixedHeight(26)
        self.paint_mat_combo.setMinimumWidth(160)
        self.paint_mat_combo.setMaximumWidth(260)
        lay.addWidget(self.paint_mat_combo)

        lay.addSpacing(4)

        def _tbtn(attr, icon_fn, tip, tool):  #vers 1
            b = QPushButton()
            try:
                b.setIcon(getattr(self.icon_factory, icon_fn)(color=ic))
            except Exception:
                b.setText(tool[0].upper())
            b.setIconSize(QSize(16, 16))
            b.setFixedSize(28, 28)
            b.setToolTip(tip)
            b.setCheckable(True)
            b.clicked.connect(lambda *_: self._set_paint_tool(tool))
            setattr(self, attr, b)
            lay.addWidget(b)

        _tbtn('tool_paint_btn',   'paint_icon',   'Paint faces',   'paint')
        _tbtn('tool_dropper_btn', 'dropper_icon', 'Dropper',       'dropper')
        _tbtn('tool_fill_btn',    'fill_icon',    'Flood fill',    'fill')
        if self.tool_paint_btn:
            self.tool_paint_btn.setChecked(True)

        self.paint_undo_btn = QPushButton()
        try:
            self.paint_undo_btn.setIcon(self.icon_factory.undo_paint_icon(color=ic))
        except Exception:
            self.paint_undo_btn.setText("↩")
        self.paint_undo_btn.setIconSize(QSize(16, 16))
        self.paint_undo_btn.setFixedSize(28, 28)
        self.paint_undo_btn.setToolTip("Undo last paint op")
        self.paint_undo_btn.setEnabled(False)
        self.paint_undo_btn.clicked.connect(self._undo_last_action)
        lay.addWidget(self.paint_undo_btn)

        lay.addStretch()

        self.paint_exit_btn = QPushButton("✕")
        self.paint_exit_btn.setFixedSize(28, 28)
        self.paint_exit_btn.setToolTip("Exit paint mode")
        self.paint_exit_btn.setStyleSheet(
            "color:#ff6b35; font-weight:bold; background:palette(base); border:1px solid palette(mid); border-radius:3px;")
        self.paint_exit_btn.clicked.connect(self._exit_paint_mode)
        lay.addWidget(self.paint_exit_btn)

        self.paint_toolbar = bar
        bar.setGeometry(0, 0, vp.width(), 34)
        bar.hide()

        # Reposition bar when viewport resizes
        _orig = vp.resizeEvent
        def _on_vp_resize(event, _o=_orig, _bar=bar, _vp=vp):  #vers 1
            _o(event)
            if _bar.isVisible():
                _bar.setGeometry(0, 0, _vp.width(), 34)
                _bar.raise_()
        vp.resizeEvent = _on_vp_resize


    def _find_all_paint_btns(self): #vers 1
        """Return all paint buttons from both icon and text panels."""
        btns = []
        b = getattr(self, 'paint_btn', None)
        if b: btns.append(b)
        # Walk icon panel for any other paint_btn that was overwritten
        ip = getattr(self, '_transform_icon_panel_ref', None)
        if ip:
            from PyQt6.QtWidgets import QPushButton
            for child in ip.findChildren(QPushButton):
                if child.toolTip() and 'paint' in child.toolTip().lower()                    and child not in btns:
                    btns.append(child)
        return btns

    def _set_col_buttons_enabled(self, enabled: bool): #vers 1
        """Enable/disable all transform buttons in BOTH icon and text panels.
        The text panel overwrites self.X refs, so when the icon panel is visible
        (narrow mode) those refs point to hidden buttons. Walk the icon panel too.
        """
        col_btn_attrs = [
            'flip_vert_btn', 'flip_horz_btn', 'rotate_cw_btn', 'rotate_ccw_btn',
            'analyze_btn', 'copy_btn', 'delete_surface_btn', 'duplicate_surface_btn',
            'paint_btn', 'surface_type_btn', 'surface_edit_btn', 'build_from_txd_btn',
            'show_shadow_btn', 'create_shadow_btn', 'remove_shadow_btn',
            'compress_btn', 'uncompress_btn', 'switch_btn', 'convert_btn',
        ]
        for attr in col_btn_attrs:
            btn = getattr(self, attr, None)
            if btn is not None:
                btn.setEnabled(enabled)
        icon_panel = getattr(self, '_transform_icon_panel_ref', None)
        if icon_panel:
            from PyQt6.QtWidgets import QPushButton
            for btn in icon_panel.findChildren(QPushButton):
                btn.setEnabled(enabled)
