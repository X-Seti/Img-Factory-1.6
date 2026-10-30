#this belongs in apps/components/Map_Editor/depends/map_widgets_func.py - Version: 5
# X-Seti - October 14 2026 - IMG Factory 1.6 - Map Workshop widgets

"""
Settings store, helper widgets, dialogs and table models used by Map Workshop.
"""

##Methods list -
# euler_degrees_to_quat
# _model_workshop_config_dir
# quat_to_euler_degrees
# _write_ipl_lines

##class _DFFGeometryAdapter: -
# face_count
# __init__
# materials
# __repr__
# vertex_count

##class _ModelListDelegate: -
# paint
# sizeHint

##class MapSettings: -
# get
# __init__
# _load
# save
# _save_now
# set
# shared

##class _MapWaterDepth: -
# _cache
# __init__
# _offset
# selected_z
# set_selected_z
# z_range
# z_values

##class _ViewportGauges: -
# eventFilter
# __init__
# _menu
# _place
# refresh
# set_side
# update

##class _MapOverlayToggleButton: -
# _apply_style
# __init__
# isChecked
# mousePressEvent
# set_display_style
# set_middle_click_menu_available
# set_shown

##class _WaterLevelsDialog: -
# __init__
# _apply

##class _WaterQuadDialog: -
# _apply
# __init__
# show_quad

##class _PathGroupEditDialog: -
# _apply
# _delete_selected_row
# __init__

##class _InstanceEditPanel: -
# _add_nudge_section
# _add_section
# _clear_layout_recursive
# __init__
# _make_standard_button
# _on_2dfx_button_clicked
# _on_add_texture
# _on_apply_clicked
# _on_close_clicked
# _on_del_texture
# _on_export_one_texture
# _on_identity_context_menu
# _on_position_nudged
# _on_rename_texture
# _on_replace_texture
# _on_rotation_nudged
# _on_save_clicked
# _on_scale_nudged
# _on_set_scale_zero_clicked
# _on_tobj_button_clicked
# _on_undo_clicked
# _populate_identity_section
# _populate_texture_list_widget
# _reflow_nudge_rows
# _refresh_position_spins
# _refresh_rotation_spins
# _refresh_scale_spins
# resizeEvent
# _save_all_textures_to_folder
# _save_raw_txd_file
# _set_nav_visible
# show_for_instance
# _show_tex_names_dialog
# _show_texture_thumbnail_strip
# _show_texture_zoom_view
# _texture_name_exists

##class _FilteredLoaderStub: -
# get_object
# __init__

##class _ObjectBrowserModel: -
# columnCount
# data
# flags
# headerData
# __init__
# _is_generic
# mimeData
# mimeTypes
# object_at
# _recompute
# rowCount
# set_mode
# set_model_cache
# set_objects
# set_search
# toggle_favourite

##class _InstanceTableModel: -
# columnCount
# data
# headerData
# __init__
# instance_at
# rowCount

##class _SizeAdaptiveStackedWidget: -
# minimumSizeHint
# sizeHint

##class _IplLoadDialog: -
# closeEvent
# file_done
# file_failed
# file_skipped
# file_started
# finish
# __init__
# message
# model_row
# _on_btn
# _pump
# set_file_progress
# set_model
# show_summary
# start_file
# _tick

import json
import math
import os
from pathlib import Path

from PyQt6.QtCore import QAbstractTableModel, QObject, QSize, QTimer, Qt, pyqtSignal
from PyQt6.QtGui import QBrush, QColor, QIcon, QImage, QPixmap
from PyQt6.QtWidgets import (
    QAbstractItemView, QApplication, QCheckBox, QComboBox, QDialog, QDoubleSpinBox,
    QFileDialog, QGridLayout, QGroupBox, QHBoxLayout, QHeaderView, QInputDialog, QLabel,
    QLineEdit, QListWidget, QListWidgetItem, QMenu, QMessageBox, QProgressBar, QPushButton,
    QScrollArea, QSizePolicy, QSpinBox, QStackedWidget, QStyledItemDelegate, QTableWidget,
    QTableWidgetItem, QToolButton, QVBoxLayout, QWidget)

from apps.methods.img_factory_settings import get_user_config_dir


# Temporary 3D viewport placeholder

VIEWPORT_AVAILABLE = True

DEBUG_STANDALONE = False

# Import AppSettings
try:
    APPSETTINGS_AVAILABLE = True
except ImportError:
    APPSETTINGS_AVAILABLE = False
    print("Warning: AppSettings not available")


App_name = "Map Workshop"
App_build = "193"


# Model Workshop icon available: SVGIconFactory.model_workshop_icon()
# Use for: DFF edit button in main toolbar, Model Workshop tab icon.
# - DFF → Viewport adapter

def quat_to_euler_degrees(x, y, z, w): #vers 2
    """Convert a quaternion to (roll, pitch, yaw) euler angles in
    degrees - standard formula, round-trip verified against
    euler_degrees_to_quat. Used to present an IPLInstance's rotation
    (stored as a quaternion) as editable X/Y/Z degree values.

    Normalizes its input first (Sep 5 2026  own real,
    confirmed IPL line showing a raw, non-unit (1,1,1,1) rotation) -
    this formula assumes a unit quaternion; feeding it a non-
    normalized one produces mathematically wrong angles, not just an
    approximation.

    Also now special-cases gimbal lock (|pitch| == 90 degrees) with
    the standard fallback formula (Sep 5 2026,  "the
    Rotation line is wrong. I'm wondering where its picking up
    x146.3 y90 z146.3, doing a search with ds '146.3' its picking up
    some of the path files") - at gimbal lock, roll and yaw are
    mathematically underdetermined by a plain atan2 on the raw off-
    diagonal terms (only their combination is meaningful, not their
    individual split), so a tiny floating-point difference in the
    input can swing the extracted roll/yaw between wildly different-
    looking values even though the actual 3D rotation barely changes.
    That's what produced the confusing 146.3 - not data leaking from
    path files (a red herring from my own search; that specific
    number is a common byproduct of this exact instability, not
    evidence of real cross-contamination). The standard fallback
    (roll=0, yaw absorbs the combined twist) gives a stable, sensible
    result instead."""
    mag = math.sqrt(x*x + y*y + z*z + w*w)
    if mag > 1e-9:
        x, y, z, w = x/mag, y/mag, z/mag, w/mag
    sinr_cosp = 2 * (w * x + y * z)
    cosr_cosp = 1 - 2 * (x * x + y * y)
    sinp = 2 * (w * y - z * x)
    siny_cosp = 2 * (w * z + x * y)
    cosy_cosp = 1 - 2 * (y * y + z * z)
    if sinp >= 1.0 - 1e-7:
        pitch = math.pi / 2
        roll = 0.0
        yaw = -2 * math.atan2(x, w)
    elif sinp <= -1.0 + 1e-7:
        pitch = -math.pi / 2
        roll = 0.0
        yaw = 2 * math.atan2(x, w)
    else:
        roll = math.atan2(sinr_cosp, cosr_cosp)
        pitch = math.asin(sinp)
        yaw = math.atan2(siny_cosp, cosy_cosp)
    return math.degrees(roll), math.degrees(pitch), math.degrees(yaw)


def euler_degrees_to_quat(roll_deg, pitch_deg, yaw_deg): #vers 1
    """Convert (roll, pitch, yaw) euler angles in degrees back to a
    quaternion (x, y, z, w) - inverse of quat_to_euler_degrees."""
    roll, pitch, yaw = (math.radians(roll_deg), math.radians(pitch_deg),
                       math.radians(yaw_deg))
    cr, sr = math.cos(roll * 0.5), math.sin(roll * 0.5)
    cp, sp = math.cos(pitch * 0.5), math.sin(pitch * 0.5)
    cy, sy = math.cos(yaw * 0.5), math.sin(yaw * 0.5)
    w = cr * cp * cy + sr * sp * sy
    x = sr * cp * cy - cr * sp * sy
    y = cr * sp * cy + sr * cp * sy
    z = cr * cp * sy - sr * sp * cy
    return x, y, z, w


# Model Workshop icon available: SVGIconFactory.model_workshop_icon()
# Use for: DFF edit button in main toolbar, Model Workshop tab icon.
# - DFF → Viewport adapter

def _write_ipl_lines(path: str, lines: list) -> None: #vers 1
    """Write text lines back to an IPL/DAT the safe way: journal + timestamped backup
    of the existing file (raises if the backup fails), atomic write, latin-1 so no byte
    is dropped, and the file's own line ending (new lines get CRLF in a CRLF file)."""
    from apps.methods.file_backup import safe_write_bytes
    crlf = any(l.endswith('\r\n') for l in lines)
    eol = '\r\n' if crlf else '\n'
    out = [(l.rstrip('\r\n') + eol) if l.endswith('\n') else l for l in lines]
    safe_write_bytes(path, ''.join(out).encode('latin-1', errors='replace'),
                     f"Map Workshop: save {os.path.basename(path)}")


class _DFFGeometryAdapter:
    """Adapts a DFF Geometry for use with DFFViewport.

    The viewport expects model.vertices (objects with .x .y .z) and
    model.faces (objects with .vertex_indices or .a .b .c).
    DFF Geometry has .vertices (Vector3) and .triangles (Triangle with .v1 .v2 .v3).
    Vertices are transformed to world space using the frame hierarchy.
    """

    class _FaceAdapter:
        """Wraps a DFF Triangle so the viewport can read it as a COL face."""
        __slots__ = ('vertex_indices', 'material', 'a', 'b', 'c')
        def __init__(self, tri):  #vers 1
            self.vertex_indices = (tri.v1, tri.v2, tri.v3)
            self.a = tri.v1
            self.b = tri.v2
            self.c = tri.v3
            self.material = tri.material_id



    def __init__(self, geometry, geometry_index: int = 0, dff_model=None, atomic=None): #vers 2
        self._geometry = geometry
        self.faces     = [self._FaceAdapter(t) for t in geometry.triangles]
        self.spheres   = []
        self.boxes     = []
        self.name      = f"geometry_{geometry_index}"

        # Use raw local-space vertices — world transform not needed for single-geom preview
        self.vertices = list(geometry.vertices)

    @property
    def vertex_count(self):   return len(self.vertices)  #vers 1
    @property
    def face_count(self):     return len(self.faces)  #vers 1

    @property
    def materials(self): #vers 2
        """Expose DFF geometry materials for use by the viewport renderer."""
        return getattr(self._geometry, 'materials', [])

    def __repr__(self): #vers 1
        return f"<_DFFGeometryAdapter {self.name} {self.vertex_count}v {self.face_count}f>"


class _ModelListDelegate(QStyledItemDelegate):
    """Word-wrapping delegate for the COL compact list Details column."""
    def paint(self, painter, option, index): #vers 1
        if index.column() != 1:
            super().paint(painter, option, index)
            return
        from PyQt6.QtWidgets import QStyle, QApplication
        from PyQt6.QtCore import Qt
        # Background
        QApplication.style().drawPrimitive(
            QStyle.PrimitiveElement.PE_PanelItemViewItem, option, painter)
        text = index.data(Qt.ItemDataRole.DisplayRole) or ''
        painter.save()
        painter.setClipRect(option.rect)
        r = option.rect.adjusted(4, 4, -4, -4)
        painter.setPen(option.palette.text().color())
        painter.setFont(option.font)
        painter.drawText(r, Qt.TextFlag.TextWordWrap | Qt.AlignmentFlag.AlignTop, text)
        painter.restore()


    def sizeHint(self, option, index): #vers 1
        if index.column() != 1:
            return super().sizeHint(option, index)
        from PyQt6.QtCore import Qt, QSize
        text = index.data(Qt.ItemDataRole.DisplayRole) or ''
        fm = option.fontMetrics
        # Measure the text in a column ~160px wide
        w = 160
        r = fm.boundingRect(0, 0, w, 9999,
            Qt.TextFlag.TextWordWrap | Qt.AlignmentFlag.AlignTop, text)
        return QSize(w, max(72, r.height() + 12))






def _model_workshop_config_dir() -> Path: #vers 3
    """The one, shared, correct location for every Map/Model Workshop
    config file in this app - a dedicated config/ subfolder alongside
    this app's own depends/ folder (Aug 20 2026)"""
    cfg_dir = Path(__file__).resolve().parent.parent / 'config'   # Map_Editor/config
    try:
        cfg_dir.mkdir(parents=True, exist_ok=True)
        # Confirm it's actually writable, not just that mkdir didn't
        # raise (mkdir with exist_ok=True succeeds even against a
        # directory that exists but denies write access to us).
        probe = cfg_dir / '.write_test'
        probe.write_text('')
        probe.unlink()
        return cfg_dir
    except Exception as e:
        print(f"[Map/Model Workshop config] App folder not writable "
              f"({e}), falling back to ~/.config/imgfactory")
        fallback = get_user_config_dir()
        fallback.mkdir(parents=True, exist_ok=True)
        return fallback


class MapSettings(QObject):
    """
    Lightweight JSON settings for DP5 Workshop.
    Stored alongside this module itself (see _model_workshop_config_
    dir's own docstring - app-folder-relative for standalone
    deployments, not ~/.config), falling back to ~/.config/imgfactory
    only if that isn't writable.
    Completely separate from the global AppSettings/theme system.

    Now a QObject (Aug 16 2026)
    """

    DEFAULTS = {
        'show_bitmap_list':  False,    # left panel visible
        # Persistent default fill colour for new canvases - replaces the
        # New Canvas dialog's hardcoded Grey(128,128,128) with a
        # user-adjustable colour, set via the Canvas swatch in
        # Brush & Colors.
        'canvas_fill_color': '#808080',
        # Widget manager - enable/disable each dock independently of its
        # current show/hide state (this controls whether the dock exists
        # at all / is offered in the UI, not just its visibility).
        'widget_bitmaps_enabled':      True,
        'widget_brushcolors_enabled':  True,
        'widget_imagepalette_enabled': True,
        'widget_userpalette_enabled':  True,
        'tool_icon_size':    24,       # tool button pixel size (20–64)
        'tool_icon_color':   'color',  # 'color' | 'white' | 'dark'
        'tool_columns':      3,        # 3, 4, 5, or 6
        'hidden_tools':      [],       # list of tool_ids to hide
        'img_pal_cols':      16,       # image palette columns
        'img_pal_rows':      16,       # image palette max visible rows
        'user_pal_cols':     16,       # user palette columns
        'user_pal_rows':     16,       # user palette max visible rows
        'default_zoom':      4,        # startup zoom level
        'undo_levels':       32,

        'recent_dat_files':  [],
        # Auto-load the most recently used game world on startup (Aug
        # 20 2026  own explicit "option 2" choice: "build
        # auto-restore last world on startup, so preload can fire
        # without a manual reload each time") - default on since he
        # chose it, but toggleable since it's a real startup-behaviour
        # change some setups might not want (a heavy world load every
        # time the tool opens).
        'auto_load_last_world': True,
        # Auto-dismiss the load summary dialog (Aug 20 2026, per
        #  : "that dat window, countdown from 10, then
        # automatically press ok, also show the preloaded files in
        # that dialog window") - off by default, since a popup
        # closing itself is a bigger behaviour change to opt into
        # than auto-loading the world itself.
        'auto_dismiss_summary_dialog': False,
        'auto_dismiss_summary_seconds': 10,

        'lod_draw_dist_threshold': 300.0,

        'lod_test_circle_radius': 300.0,

        'path_line_color': (255, 0, 0),

        'path_node_color': (255, 204, 0),
        'path_line_thickness': 1.2,
        'path_node_size': 3.5,
        'track_line_thickness': 1.5,
        'airtrain_color': (230, 153, 51),
        'airtrain_line_thickness': 1.2,
        'grid_spacing': 5,
        'grid_fixed_step': 200,
        'grid_cell_count': 24,
        'show_radar_tex_layer': False,
        'radar_tex_folder': '',
        'grid_scale_mode': 'zoom_relative',
        'skybox_path': '',
        'timecyc_path': '',
        'sky_gradient_flipped': False,
        'water_display_style': 'fill',
        'water_texture_path': '',
        'water_tile_size': 256,
        'water_hide_outside_map': False,
        'preload_saved_files': [],
        # Water2's own style toggle (Aug 20 2026,  "I like
        # the blue, so we can keep it, or have an option to use the
        # water texture, either from the game or the tex/ file from
        # img factory") - off by default (my own stated
        # preference), independent of whether a texture happens to be
        # preloaded, so switching back and forth doesn't need re-
        # preloading each time.
        'water2_use_texture': False,
        # Real, manually-chosen custom water texture path (Aug 20
        # 2026,  "the option settings path for using other
        # water textures") - empty means "no custom override, try the
        # loaded game's own particle.txd first, then this app's own
        # tex/ folder asset if preloaded via that route instead".
        'water2_custom_texture_path': '',
        # Height/transparency adjustments (Aug 20 2026, .per:
        # "The water needs to be moved up and have transparency
        # settings, but I'm not sure by how much").
        'water2_height_offset': 0.0,
        'water2_alpha': 0.45,
        # Viewport gauge overlay: 'left', 'right' or 'off'
        'viewport_gauge_side': 'right',
        # X/Y offsets (Aug 20 2026,  "6 squares offset on
        # the larger grid, or 14 on the smaller grid") - matches
        # water_workshop.py's own "World coordinate offset" X/Y/Z
        # feature; per-file, not assumed to be the same for every VC
        # install.
        'water2_x_offset': 0.0,
        'water2_y_offset': 0.0,
        # VC-only gate (Aug 20 2026,  "offset should only be
        # for VC, so we need a toggle to effect VC waterpro.dat only")
        'water2_offset_vc_only': True,
        # Radar/water fix mode: 'vc' VC only, 'all', 'off'
        'water2_offset_mode': '',

        # IPL Controls display style (Aug 20 2026,  "we
        # could add a toggle in settings, Show IPL Controls = as
        # [Buttons] or ribbon icons") - off (buttons/text) by default,
        # since not every overlay toggle button has real icon artwork
        # yet.
        'ipl_controls_icon_only': False,

        # Viewport camera state (Aug 20 2026,  "remember the
        # zoom settings, and view location when app is closed") - None
        # means "never saved yet, use the viewport's own built-in
        # default" rather than a real 0.0/wrong guessed default.
        'viewport_dist': None,
        'viewport_pan_x': None,
        'viewport_pan_y': None,
        'viewport_yaw': None,
        'viewport_pitch': None,

        # Camera angle when centring on a selected instance (Sep 5
        # 2026,  "when selecting a model in map workshop,
        # can we change the camera view, have a setting, view from 0,
        # +200 so we dont view the model from the bottom, we see it
        # from the top instead") - off by default since it's a new
        # behaviour change to opt into; _center_on_instance leaves the
        # camera's current yaw/pitch alone otherwise, which can leave
        # it looking up from underneath the model. When on, selecting
        # an instance also resets pitch to a from-above angle and
        # dist to focus_from_above_dist.
        'focus_from_above': False,
        'focus_from_above_dist': 200.0,

        # IPL Object Editor always-on-top (Sep 5 2026,  "the
        # IPL file editor should stay on top, with a settings toggle
        # option") - off by default, matching every other new opt-in
        # behaviour toggle added this session.
        'ipl_editor_always_on_top': False,

        # IPL stems to parse as VC layout (Sep 5 2026,  "LC,
        # MLL, VC are still in VC format... a loading toggle to adjust
        # ipl loading patterns") - comma-separated, no extension, e.g.
        # "lc,mll,vc" - empty by default (no change to normal loading).
        'vc_layout_ipl_stems': '',

        # distinct from paths' red.
        'cull_box_color': (255, 217, 51),

        'zone_box_color': (77, 179, 255),
        # Zone box render style (Aug 16 2026)
        'zone_render_style': 'ghosted',
        # Occlusion zone box colour in the 3D view (Aug 16 2026)
        'occl_box_color': (255, 102, 204),
        # Train track line colour (Aug 17 2026)
        # loosely evoking real rail.
        'track_color': (191, 191, 204),
        # Zoom to mouse cursor (Aug 18 2026)
        'zoom_to_cursor': False,
        # Axis-colored box faces (Aug 18 2026)
        'box_axis_colors': False,
        # Unique colour per box (Aug 19 2026)
        'box_unique_colors': False,
        # No-clip box resizing (Aug 19 2026)
        'no_clip_boxes': False,
        # Auto-highlight on hover (Aug 19 2026)
        'auto_highlight_hover': False,
        # Dock/splitter layout (Aug 19 2026)
        'workshop_dock_state': '',
        # Viewport camera keybindings (Aug 16 2026)
        'viewport_key_bindings': {},
        # Load text IPL + its binary stream set together (Aug 1 2026)
        'load_text_plus_binary_ipl_set': True,
        # Preload IMG archives to OS disk cache on DAT load (Aug 16 2026)
        'preload_img_on_dat_load': False,
        'show_load_options_dialog': True,
        # Verbose per-model loading dialog (Aug 1 2026)
        'show_verbose_loading_dialog': False,
        # Parallel model/texture parsing (Sep 25 2026); 0 = auto (cores - 2)
        'load_workers': 0,
        # Texture downscale (Aug 1 2026)
        'texture_downscale_enabled': False,
        'texture_downscale_threshold': 512,
        'texture_downscale_target': 256,
        # Per-format texture limits (Oct 12 2026); RW uses texture_downscale_*
        'tex_limit_iv_on': False, 'tex_limit_iv_over': 256, 'tex_limit_iv_to': 128,
        'tex_limit_stories_on': False, 'tex_limit_stories_over': 512, 'tex_limit_stories_to': 256,
        'tex_limit_nif_on': False, 'tex_limit_nif_over': 512, 'tex_limit_nif_to': 256,
        'tex_keep_dxt': True,   # DXT kept compressed, GPU decodes
        'default_width':     320,
        'default_height':    200,
        'retro_palette':     'Amiga AGA WB',
        'show_pixel_grid':   True,
        'grid_color':        '#808080',  # pixel grid colour (hex)
        'platform_mode':     'none',   # 'none'|'c64'|'c64m'|'spectrum'|'msx'|'cpc'|'atari_st'|'amiga'
        'show_cell_grid':    False,    # show platform cell boundaries
        'show_statusbar':    True,     # show bottom status bar
        'show_paint_canvas': False,    # forked-in paint canvas - hidden by default, toggleable
        'favourite_objects': [],       # list of favourited model_ids, Object Browser panel
        # Per-view-mode pan direction inversion (World View: Top/Side/
        # Front/3D each have a different camera orientation, so the
        # same raw mouse delta needs different sign handling per mode
        # to feel consistenT.
        'viewport_pan_invert': {
            'Top':   {'x': False, 'y': False},
            'Side':  {'x': False, 'y': False},
            'Front': {'x': False, 'y': False},
            '3D':    {'x': False, 'y': False},
        },
        # Mouse button assignment for World View pane interaction -
        # which button pans vs rotates (rotate only applies to
        # unlocked/3D panes; locked ortho panes only ever pan).
        'viewport_pan_button': 'middle',     # 'middle' | 'left' | 'right'
        'viewport_rotate_button': 'right',   # 'middle' | 'left' | 'right'
        # Persisted display order for IPL Sections rows - a list of
        # ipl_name strings; any names not in this list (a different
        # world/mod was loaded) get appended alphabetically after the
        # known ones, rather than the order being lost entirely.
        'ipl_sections_order': [],
        # Persisted column widths (user-resized), keyed by panel -
        # restored on next open rather than resetting to defaults.
        'ipl_sections_column_widths': [],
        'object_browser_column_widths': [],
        'object_browser_hidden_columns': [],
        'ui_font_size':      10,       # toolbar/button font size
        'canvas_mode':       'free',   # 'free'|'platform'|'texture'|'icon'
        'show_anim_strip':   False,    # show animation timeline strip
        'anim_fps':          12,       # default animation FPS
        'zoom_to_fit_resize': False,
        'show_menubar':       False,     # hidden by default — enable in Settings > Menu
        'menu_style':         'dropdown', # 'topbar' | 'dropdown'
        'menu_bar_font_size':  9,         # topbar menubar font size (pt)
        'menu_bar_height':     22,        # topbar menubar height (px)
        'menu_dropdown_font_size': 9,     # dropdown menu item font size (pt)
        'outer_layout_state':   '',            # QMainWindow.saveState() hex - dock/ribbon layout
        'outer_layout_version': 0,             # must match _OUTER_LAYOUT_VERSION to restore
        # Icon editor
        'char_editor_docked':  False,  # _CharFontEditor dock state
        'sprite_editor_docked': False, # _SpriteEditor dock state
        'sprite_editor_folder': '',    # sprite source folder
        'char_editor_folder':   '',    # bitmap font folder
        'svg_browser_docked':  False,  # SVGIconBrowser dock state
        'icon_editor_docked':  False,  # True = snapped to overlay, False = floating
        'icon_editor_x':       -1,     # last window X (-1 = auto)
        'icon_editor_y':       -1,     # last window Y
        'icon_editor_out_fmt': 'PNG',  # last export format
        'icon_editor_alpha':   True,   # colour 0 = alpha
        'icon_editor_alpha_r': 0,      # alpha colour R
        'icon_editor_alpha_g': 0,      # alpha colour G
        'icon_editor_alpha_b': 0,      # alpha colour B
        'icon_editor_amiga_pal':'AGA Workbench (WB3.9)',
        'icon_editor_folder':   '',   # last source icon folder
        # Marching ants (animated selection outline)
        'marching_ants_enabled': True,
        'marching_ants_fg':      '#000000',  # foreground colour (hex)
        'marching_ants_bg':      '#ffffff',  # background colour (hex)
        'marching_ants_style':   'dashes',   # 'dashes' | 'dots'
        'marching_ants_speed':   150,        # animation step interval, ms
        # Ribbon appearance - applies to every ribbon (Tools, Image Ops,
        # any added later), independently for whichever orientation the
        # ribbon is currently docked at (vertical = left/right dock area,
        # horizontal = top/bottom), re-applied live when a ribbon is
        # dragged between them.
        'ribbon_icon_size_vert':  22,
        'ribbon_icon_size_horz':  22,
        'ribbon_padding_vert':    3,
        'ribbon_padding_horz':    3,
        'ribbon_opacity':         100,   # 0-100, 100 = fully opaque
        # Internal padding between the icon and the button's own edge -
        # distinct from ribbon_padding_* above, which is the gap BETWEEN
        # buttons. QToolButton has its own built-in padding from the Qt
        # style even when the toolbar's inter-item spacing is 0.
        'ribbon_button_padding_vert': 2,
        'ribbon_button_padding_horz': 2,
        # Ribbon Manager - persisted tool_id -> ribbon_name assignment,
        # letting tools be moved freely between ribbons (including new
        # custom ribbons the user creates). Empty dict means "use each
        # tool's default_ribbon from _RIBBON_TOOL_REGISTRY".
        'ribbon_tool_assignment': {},
        # Ribbon Manager - explicit display order for all reassignable
        # tools, updated by drag-reordering within a ribbon in the
        # manager dialog. Empty list means "use each tool's natural
        # order from _RIBBON_TOOL_REGISTRY".
        'ribbon_tool_order': [],
        # Fonts/Display/Preview settings (Aug 20 2026)
        'default_font_family': 'Fira Sans Condensed', 'default_font_size': 14,
        'title_font_family':   'Arial', 'title_font_size':   14,
        'panel_font_family':   'Arial', 'panel_font_size':   10,
        'button_font_family':  'Arial', 'button_font_size':  10,
        'infobar_font_family': 'Courier New', 'infobar_font_size': 9,
        'button_display_mode': 'both',
        'zoom_level':          1.0,
        'background_mode':     'solid',
        'checkerboard_size':   16,
        'overlay_opacity':     50,
        # Radar tile generation (Aug 20 2026)
        'radar_tiles_output_dir': '',
        'radar_tiles_pack_txd':   True,
        'radar_tiles_copy_to_assists': True,
        #  bug fixed (Aug 20 2026)
        'radar_tiles_show_grid':  False,
        # Save points - snapshots of unsaved map edits (Sep 23 2026)
        'savepoints_enabled':      False,
        'savepoints_interval_min': 10,
        'savepoints_keep':         20,
        # Map Checks limits (None = built-in default per game) (Sep 23 2026)
        'limit_gta3_model_infos': None, 'limit_gta3_buildings': None,
        'limit_vc_model_infos':   None, 'limit_vc_buildings':   None,
        'limit_sa_model_infos':   None, 'limit_sa_buildings':   None,
        'limit_sol_model_infos':  None, 'limit_sol_buildings':  None,
        # Camera bookmarks per world: {world key: {slot: [pan_x, pan_y, dist, yaw, pitch, name]}}
        'camera_bookmarks': {},
        # Game controller (Sep 24 2026)
        'gamepad_enabled':  False,
        'gamepad_deadzone': 0.15,
        # Prelight bake (Sep 24 2026)
        'prelight_ambient': (90, 90, 100), 'prelight_diffuse': (200, 190, 170),
        'prelight_azimuth': 135.0, 'prelight_elevation': 45.0, 'prelight_strength': 1.0,
        'prelight_shadows': True, 'prelight_shadow_dark': 0.6, 'prelight_shadow_res': 2048,
        # The real "other grid options" this same comment block above
        # already flagged as coming later (Aug 20 2026)
        'grid_type': 'lines',
        'grid_bg_color': (51, 128, 230),
        'squares_fill_mode': 'color',
        'squares_texture_path': '',
        'squares_tile_size': 256,
        'squares_texture_alpha': 0.5,
        'grid_show_lines': True,
        'grid_hide_over_radar_tiles': False,
        'grid_line_color': (76, 76, 102),
        'grid_line_size': 4,
    }

    _instance = None

    @classmethod
    def shared(cls): #vers 1
        """Singleton access; overriding __new__ on QObject segfaults (PyQt 6.11)."""
        if cls._instance is None:
            cls._instance = cls()
        return cls._instance

    def __init__(self): #vers 5
        super().__init__()
        # App-folder-relative, not ~/.config (Aug 20 2026)
        self._path = _model_workshop_config_dir() / 'map_workshop.json'
        self._data = dict(self.DEFAULTS)
        self._load()
        # Debounced auto-save (Aug 16 2026)
        self._save_timer = QTimer(self)
        self._save_timer.setSingleShot(True)
        self._save_timer.timeout.connect(self._save_now)

    def _load(self): #vers 2
        """Load previously-saved settings from disk (Aug 20 2026)"""
        try:
            if self._path.exists():
                loaded = json.loads(self._path.read_text())
                self._data.update({k: v for k, v in loaded.items()
                                   if k in self.DEFAULTS})
        except Exception as e:
            print(f"[MapSettings] Failed to load {self._path}: {e} "
                  f"- falling back to defaults for every setting.")

    def save(self): #vers 2
        """Force an immediate write, bypassing the debounce - kept as
        the explicit public API existing call sites already use
        (still harmless/correct to call, just redundant now that
        set() schedules its own save automatically), and useful
        anywhere an immediate guaranteed write matters more than
        debounce delay (e.g. right before an app close)."""
        if self._save_timer.isActive():
            self._save_timer.stop()
        self._save_now()

    def _save_now(self): #vers 2
        """Write the current settings to disk (Aug 20 2026)"""
        try:
            tmp_path = self._path.with_suffix('.json.tmp')
            tmp_path.write_text(json.dumps(self._data, indent=2))
            os.replace(tmp_path, self._path)
        except Exception as e:
            print(f"[MapSettings] Failed to save {self._path}: {e}")

    def get(self, key, default=None): #vers 1
        return self._data.get(key, default if default is not None
                              else self.DEFAULTS.get(key))

    def set(self, key, value): #vers 2
        if key in self.DEFAULTS:
            self._data[key] = value
            self._save_timer.start(800)




class _MapWaterDepth: #vers 2
    """Depth gauge source: water cell heights plus the Z offset; dragging sets the offset."""

    def __init__(self, ws): #vers 1
        self._ws = ws

    def _cache(self): #vers 2
        """(distinct heights, range, sea level), recomputed only when the cell list changes."""
        from apps.methods.ui_color import depth_range
        vp = getattr(self._ws, 'preview_widget', None)
        cells = getattr(vp, '_water2_cells', None) or []
        key = (id(cells), len(cells))
        if getattr(self, '_key', None) != key:
            zs = [round(c[4], 2) for c in cells]
            lo, hi = depth_range(zs)
            sea = max(set(zs), key=zs.count) if zs else 0.0
            self._key, self._vals = key, sorted(set(zs))
            self._rng, self._sea = (lo - 25.0, hi + 25.0), sea   # room to drag the offset
        return self._vals, self._rng, self._sea

    def _offset(self): #vers 1
        vp = getattr(self._ws, 'preview_widget', None)
        return float(getattr(vp, '_water2_height_offset', 0.0) or 0.0)

    def z_values(self): #vers 3
        off = self._offset()
        return [z + off for z in self._cache()[0]]

    def z_range(self): #vers 3
        return self._cache()[1]

    def selected_z(self): #vers 2
        """Sea level (most common height) with the Z offset; None without water."""
        vals, _rng, sea = self._cache()
        return sea + self._offset() if vals else None

    def set_selected_z(self, z, start=False): #vers 2
        """Drag: Z offset = z - sea level (saved to the map settings)."""
        off = round(z - self._cache()[2], 2)
        vp = getattr(self._ws, 'preview_widget', None)
        if vp is not None and hasattr(vp, 'set_water2_height_offset'):
            vp.set_water2_height_offset(off)
        self._ws.map_settings.set('water2_height_offset', off)
        self._ws._write_water_offsets({'z': off})      # per game, data/water_offset.dat
        self._ws._set_status(f"Water Z offset {off:+.2f}")
        for g in (getattr(self._ws, '_water_gauge', None), getattr(self._ws, '_vp_gauges', None)):
            if g is not None:
                g.update()


class _ViewportGauges(QWidget): #vers 1
    """Viewport overlay, middle left or right: water depth (drag = Z offset)
    when water is shown, else the radar height colours when the radar layer is on."""

    WIDTH = 26

    def __init__(self, ws, vp): #vers 1
        super().__init__(vp)
        from PyQt6.QtWidgets import QVBoxLayout
        from apps.components.Water_Editor.water_workshop import WaterZGauge
        from apps.components.Radar_Editor.radar_workshop import _HeightGauge
        self._ws, self._vp = ws, vp
        lay = QVBoxLayout(self)
        lay.setContentsMargins(0, 0, 0, 0)
        self.water = WaterZGauge(_MapWaterDepth(ws))
        self.height_gauge = _HeightGauge()
        self.height_gauge.setToolTip("Radar height colours: forest (high) to deep water (low)")
        lay.addWidget(self.water)
        lay.addWidget(self.height_gauge)
        self.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.customContextMenuRequested.connect(self._menu)
        for w in (self.water, self.height_gauge):
            w.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
            w.customContextMenuRequested.connect(lambda pos, w=w: self._menu(w.mapTo(self, pos)))
        vp.installEventFilter(self)
        self.refresh()

    def eventFilter(self, obj, ev): #vers 1
        from PyQt6.QtCore import QEvent
        if obj is self._vp and ev.type() == QEvent.Type.Resize:
            self._place()
        return False

    def refresh(self): #vers 1
        """Pick water / radar / hidden from what the viewport shows."""
        vp, side = self._vp, self._ws.map_settings.get('viewport_gauge_side') or 'right'
        water = bool(getattr(vp, 'show_water', False) and getattr(vp, '_water2_cells', None))
        radar = bool(getattr(vp, '_show_radar_tex_layer', False))
        self.water.setVisible(water)
        self.height_gauge.setVisible(radar and not water)
        self.setVisible(side != 'off' and (water or radar))
        self._place()
        self.update()

    def _place(self): #vers 1
        vp = self._vp
        h = max(120, vp.height() // 2)
        side = self._ws.map_settings.get('viewport_gauge_side') or 'right'
        x = 8 if side == 'left' else vp.width() - self.WIDTH - 8
        self.setGeometry(x, (vp.height() - h) // 2, self.WIDTH, h)
        self.raise_()

    def update(self): #vers 1
        super().update()
        self.water.update()

    def _menu(self, pos): #vers 1
        """Right-click: move left / right, or hide."""
        from PyQt6.QtWidgets import QMenu
        menu = QMenu(self)
        for label, side in (("Gauge Left", 'left'), ("Gauge Right", 'right'), ("Hide Gauge", 'off')):
            menu.addAction(label, lambda s=side: self.set_side(s))
        menu.exec(self.mapToGlobal(pos))

    def set_side(self, side): #vers 1
        self._ws.map_settings.set('viewport_gauge_side', side)
        self.refresh()


class _MapOverlayToggleButton(QToolButton): #vers 2
    """One compact button replacing a pair of checkboxes (show/hide +
    edit mode) for a single map overlay type (Aug 18 2026).

    Optional icon support added (Aug 20 2026,  "we could
    add a toggle in settings, Show IPL Controls = as [Buttons] or
    ribbon icons") - label is always kept as this button's own real
    text (used for its tooltip and QMessageBox-style status text
    regardless of display mode), icon is optional and only actually
    shown once set_display_style(icon_only=True) is called - a
    button given no real icon yet just keeps showing its own text
    even in icon-only mode, rather than rendering blank."""
    show_toggled = pyqtSignal(bool)
    edit_toggled = pyqtSignal(bool)
    middle_clicked = pyqtSignal()

    def __init__(self, label, supports_edit=False, icon=None, parent=None): #vers 3
        super().__init__(parent)
        self._label = label
        self.setText(label)
        if icon is not None:
            self.setIcon(icon)
        self.setFixedHeight(20)
        self._shown = False
        self._editing = False
        self._supports_edit = supports_edit
        self._has_middle_menu = False
        tip = f"Left-click: show/hide {label}."
        if supports_edit:
            tip += f"\nRight-click: toggle edit mode for {label}."
        self.setToolTip(tip)
        self._apply_style()

    def set_middle_click_menu_available(self, available: bool): #vers 1
        """Mark whether this button has a real middle-click menu
        connected (Aug 21 2026,  "add these to the svg icon
        zon button with a middle-click, and move those functions
        over. Same with cull, occl, auzo, grge") - only updates this
        button's own real tooltip text; map_workshop.py itself still
        owns building/showing the actual real menu via middle_
        clicked, this widget only ever emits that one real signal."""
        self._has_middle_menu = available
        tip = f"Left-click: show/hide {self._label}."
        if self._supports_edit:
            tip += f"\nRight-click: toggle edit mode for {self._label}."
        if available:
            tip += f"\nMiddle-click: add/delete/save/corner coords for {self._label}."
        self.setToolTip(tip)

    def set_display_style(self, icon_only: bool): #vers 2
        """Switch between this button's own real text label and its
        real icon (Aug 20 2026,  "Show IPL Controls = as
        [Buttons] or ribbon icons") - falls back to text-only when
        icon_only is requested but no real icon was ever actually set
        for this button, rather than rendering a blank button.

         fix (Aug 20 2026,  "the svg icons, need to be
        square like the others in the ribbons... its only the new
        ribbon buttons that are a different size, they need to be the
        same size as the existing buttons") - __init__ only ever fixed
        this button's own real height (20px), leaving its own real
        width to size itself to whatever text label happened to be
        set, even once switched to icon-only mode - that stale, text-
        sized width is exactly why these looked visibly wider/non-
        square next to the other, real native QAction-based ribbon
        buttons (which have no text reservation at all). Now fixes
        both width and height to a real, square 28px (this app's own
        real 20px icon size plus standard Qt toolbar padding) in
        icon-only mode, and releases back to auto/flexible width for
        text mode, where a square size would clip the label."""
        if icon_only and not self.icon().isNull():
            self.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonIconOnly)
            self.setFixedSize(28, 28)
        else:
            self.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextOnly)
            self.setMinimumWidth(0)
            self.setMaximumWidth(16777215)
            self.setFixedHeight(20)

    def mousePressEvent(self, event): #vers 2
        if event.button() == Qt.MouseButton.LeftButton:
            self._shown = not self._shown
            self._apply_style()
            self.show_toggled.emit(self._shown)
        elif event.button() == Qt.MouseButton.RightButton:
            if self._supports_edit:
                self._editing = not self._editing
                self._apply_style()
                self.edit_toggled.emit(self._editing)
            else:
                mw = self.window()
                if mw is not None and hasattr(mw, '_set_status'):
                    mw._set_status(f"No edit mode available for {self.text()} yet")
        elif event.button() == Qt.MouseButton.MiddleButton:
            #  fix (Aug 21 2026,  "the new buttons,
            # +zon, -zon, and save zon... add these to the svg icon
            # zon button with a middle-click, and move those
            # functions over") - just emits; map_workshop.py owns
            # building/showing the actual real menu (add/delete/save/
            # corner coords) via this same real signal, same widget-
            # emits/caller-builds pattern every other button-driven
            # menu in this file already uses.
            if self._has_middle_menu:
                self.middle_clicked.emit()
        # Deliberately not calling super().mousePressEvent() - this
        # widget fully owns its own click behaviour rather than also
        # triggering QToolButton's own default checked/pressed
        # handling, which would fight with the manual state above.

    def set_shown(self, shown: bool, emit: bool = True): #vers 1
        """Programmatic state change (e.g. one button's own handler
        turning another button on, the way "Edit Paths" used to force
        "Show Paths" on) - emits show_toggled by default, matching
        QCheckBox.setChecked's own real behaviour (existing callers
        like _on_edit_paths_toggled rely on that signal firing to
        also trigger the actual viewport update, not just a visual
        state change)."""
        self._shown = shown
        self._apply_style()
        if emit:
            self.show_toggled.emit(self._shown)

    def isChecked(self) -> bool: #vers 1
        """QCheckBox-compatible accessor (Aug 18 2026)"""
        return self._shown



    def _apply_style(self): #vers 2
        # Slightly tightened padding (Aug 20 2026)
        bg = "rgba(120, 170, 255, 90)" if self._shown else "transparent"
        border = "2px dashed #ffaa00" if self._editing else "1px solid rgba(255,255,255,40)"
        self.setStyleSheet(
            f"QToolButton {{ background-color: {bg}; border: {border}; "
            f"border-radius: 3px; padding: 1px 4px; }}")


class _WaterLevelsDialog(QDialog): #vers 1
    """Non-modal editor for waterpro level heights (VC waterpro.dat, LCS/VCS GAME.DTZ)."""

    def __init__(self, workshop, heights, source, parent=None): #vers 1
        super().__init__(parent)
        self.ws = workshop
        self.setWindowTitle("Water Levels")
        self.setModal(False)
        lay = QVBoxLayout(self)
        lay.addWidget(QLabel(f"{len(heights)} water level(s) in {source}\n"
                             f"Each grid cell uses one level; changing a height moves all its cells."))
        self.table = QTableWidget(len(heights), 1)
        self.table.setHorizontalHeaderLabels(["Height (Z)"])
        self.spins = []
        for r, h in enumerate(heights):
            sp = QDoubleSpinBox(); sp.setRange(-2000, 2000); sp.setDecimals(3); sp.setValue(h)
            self.table.setCellWidget(r, 0, sp)
            self.spins.append(sp)
        self.table.horizontalHeader().setStretchLastSection(True)
        lay.addWidget(self.table)
        btns = QHBoxLayout()
        for text, slot in (("Apply", self._apply), ("Save", workshop._save_water_levels)):
            b = QPushButton(text); b.clicked.connect(lambda _c=False, f=slot: f()); btns.addWidget(b)
        lay.addLayout(btns)

    def _apply(self): #vers 1
        """Push heights to the loaded waterpro and the view."""
        self.ws._apply_water_levels([sp.value() for sp in self.spins])


class _WaterQuadDialog(QDialog): #vers 1
    """Non-modal editor for one water.dat quad: corner X/Y/Z, undo, save."""

    def __init__(self, workshop, parent=None): #vers 1
        super().__init__(parent)
        self.ws = workshop
        self.index = -1
        self.setWindowTitle("Water Quad")
        self.setModal(False)
        lay = QVBoxLayout(self)
        self.title = QLabel("Click a water quad in the view")
        lay.addWidget(self.title)
        self.grid = QGridLayout()
        for c, h in enumerate(("", "X", "Y", "Z")):
            self.grid.addWidget(QLabel(h), 0, c)
        self.spins = []
        for r in range(4):
            self.grid.addWidget(QLabel(f"Corner {r + 1}"), r + 1, 0)
            row = []
            for c, (lo, hi) in enumerate(((-9000, 9000), (-9000, 9000), (-2000, 2000))):
                sp = QDoubleSpinBox(); sp.setRange(lo, hi); sp.setDecimals(3)
                self.grid.addWidget(sp, r + 1, c + 1)
                row.append(sp)
            self.spins.append(row)
        lay.addLayout(self.grid)
        btns = QHBoxLayout()
        for text, slot in (("Apply", self._apply), ("Undo", workshop._undo_water_edit),
                           ("Save water.dat", workshop._save_water_dat),
                           ("Alignment...", workshop._show_radar_align_dialog)):
            b = QPushButton(text); b.clicked.connect(lambda _c=False, f=slot: f()); btns.addWidget(b)
        lay.addLayout(btns)

    def show_quad(self, index, quad): #vers 1
        """Fill the fields from one quad (triangles leave corner 4 off)."""
        self.index = index
        n = len(quad["corners"])
        self.title.setText(f"Water quad {index}  ({'triangle' if n == 3 else 'quad'})")
        for r, row in enumerate(self.spins):
            on = r < n
            for c, sp in enumerate(row):
                sp.setEnabled(on)
                sp.blockSignals(True)
                if on:
                    cr = quad["corners"][r]
                    sp.setValue((cr["x"], cr["y"], cr["f"][0])[c])
                sp.blockSignals(False)

    def _apply(self): #vers 1
        """Send the typed corner values to the workshop."""
        if self.index < 0:
            return
        vals = [tuple(sp.value() for sp in row) for row in self.spins if row[0].isEnabled()]
        self.ws._set_water_quad(self.index, vals)


class _PathGroupEditDialog(QDialog): #vers 1
    """Path Group Editor - add/move/delete/connect nodes within one
    path group (Aug 16 2026)"""

    def __init__(self, group, workshop, parent=None): #vers 2
        super().__init__(parent)
        self.group = group
        self.workshop = workshop
        self.setWindowTitle(
            f"Path Group Editor — Group ({group.header_a}, {group.header_b}) — {group.source_ipl}")
        self.setMinimumSize(820, 440)
        lay = QVBoxLayout(self)

        lay.addWidget(QLabel(
            "Type: 0=Null (ignored/padding), 1=External (links to another "
            "group by exact position match), 2=Internal (links within this "
            "group). Next: which row (0-11) in THIS group this node's own "
            "link points to, or -1 for none."))

        header_row = QHBoxLayout()
        header_row.addWidget(QLabel("Group Header (A, B):"))
        self._header_a_spin = QSpinBox()
        self._header_a_spin.setRange(-1, 999999)
        self._header_a_spin.setValue(group.header_a)
        self._header_b_spin = QSpinBox()
        self._header_b_spin.setRange(-1, 999999)
        self._header_b_spin.setValue(group.header_b)
        header_row.addWidget(self._header_a_spin)
        header_row.addWidget(self._header_b_spin)
        header_row.addStretch()
        lay.addLayout(header_row)

        headers = ["Slot", "Type", "Next", "X", "Y", "Z", "Median",
                   "Left", "Right", "Flag1", "Flag2", "Flag3"]
        self._table = QTableWidget(12, len(headers))
        self._table.setHorizontalHeaderLabels(headers)
        self._table.verticalHeader().setVisible(False)

        nodes = list(group.nodes)[:12]
        from apps.methods.gta_dat_parser import PathNode
        while len(nodes) < 12:
            nodes.append(PathNode(node_type=0, next_id=-1, x=0.0, y=0.0, z=0.0,
                                  median=0.0, left=0, right=0,
                                  flag1=0, flag2=0, flag3=0))

        self._type_combos  = []
        self._next_spins   = []
        self._pos_spins    = []   # [(x,y,z,median), ...] per row
        self._left_spins   = []
        self._right_spins  = []
        self._flag_spins   = []   # [(f1,f2,f3), ...] per row

        for i, node in enumerate(nodes):
            slot_item = QTableWidgetItem(str(i))
            slot_item.setFlags(slot_item.flags() & ~Qt.ItemFlag.ItemIsEditable)
            self._table.setItem(i, 0, slot_item)

            type_combo = QComboBox()
            type_combo.addItems(["0 – Null", "1 – External", "2 – Internal"])
            type_combo.setCurrentIndex(min(max(node.node_type, 0), 2))
            self._table.setCellWidget(i, 1, type_combo)
            self._type_combos.append(type_combo)

            next_spin = QSpinBox()
            next_spin.setRange(-1, 11)
            next_spin.setValue(node.next_id)
            next_spin.setToolTip("Row index (0-11) this node links to, or -1 for none")
            self._table.setCellWidget(i, 2, next_spin)
            self._next_spins.append(next_spin)

            pos_row = []
            for col, val in ((3, node.x), (4, node.y), (5, node.z), (6, node.median)):
                spin = QDoubleSpinBox()
                spin.setRange(-999999.0, 999999.0)
                spin.setDecimals(2)
                spin.setValue(val)
                self._table.setCellWidget(i, col, spin)
                pos_row.append(spin)
            self._pos_spins.append(pos_row)

            left_spin = QSpinBox(); left_spin.setRange(0, 255); left_spin.setValue(node.left)
            self._table.setCellWidget(i, 7, left_spin)
            self._left_spins.append(left_spin)
            right_spin = QSpinBox(); right_spin.setRange(0, 255); right_spin.setValue(node.right)
            self._table.setCellWidget(i, 8, right_spin)
            self._right_spins.append(right_spin)

            flag_row = []
            for col, val in ((9, node.flag1), (10, node.flag2), (11, node.flag3)):
                spin = QDoubleSpinBox() if col == 11 else QSpinBox()   # flag3 fractional
                spin.setRange(0, 255)
                if col == 11:
                    spin.setDecimals(3)
                spin.setValue(val)
                self._table.setCellWidget(i, col, spin)
                flag_row.append(spin)
            self._flag_spins.append(flag_row)

        self._table.resizeColumnsToContents()
        lay.addWidget(self._table)

        btn_row = QHBoxLayout()
        delete_btn = QPushButton("Delete Selected Row (set to Null)")
        delete_btn.setToolTip(
            "Resets the currently selected row back to a Null node -\n"
            "Type 0, Next -1, all position/flag fields zeroed, matching\n"
            "a real null-padding line in the file.")
        delete_btn.clicked.connect(self._delete_selected_row)
        btn_row.addWidget(delete_btn)
        btn_row.addStretch()
        apply_btn = QPushButton("Apply")
        apply_btn.setToolTip(
            "Updates this path group in memory - the 3D viewport and\n"
            "IPL File Display table refresh immediately. Does not write\n"
            "to the .ipl file on disk (not implemented yet).")
        apply_btn.clicked.connect(self._apply)
        btn_row.addWidget(apply_btn)
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.close)
        btn_row.addWidget(close_btn)
        lay.addLayout(btn_row)

    def _delete_selected_row(self): #vers 1
        row = self._table.currentRow()
        if row < 0:
            return
        self._type_combos[row].setCurrentIndex(0)
        self._next_spins[row].setValue(-1)
        for spin in self._pos_spins[row]:
            spin.setValue(0.0)
        self._left_spins[row].setValue(0)
        self._right_spins[row].setValue(0)
        for spin in self._flag_spins[row]:
            spin.setValue(0)

    def _apply(self): #vers 3
        from apps.methods.gta_dat_parser import PathNode
        old = list(self.group.nodes)
        self.group.header_a = self._header_a_spin.value()
        self.group.header_b = self._header_b_spin.value()
        new_nodes = []
        for i in range(12):
            x, y, z, median = (s.value() for s in self._pos_spins[i])
            new_nodes.append(PathNode(
                node_type=self._type_combos[i].currentIndex(),
                next_id=self._next_spins[i].value(),
                x=x, y=y, z=z, median=median,
                left=self._left_spins[i].value(),
                right=self._right_spins[i].value(),
                flag1=self._flag_spins[i][0].value(),
                flag2=self._flag_spins[i][1].value(),
                flag3=self._flag_spins[i][2].value(),
                cross=getattr(old[i], 'cross', 0) if i < len(old) else 0,   # keep crossroad
                extra=getattr(old[i], 'extra', ()) if i < len(old) else (),
            ))
        self.group.nodes = new_nodes
        if hasattr(self.workshop, '_refresh_path_visualization'):
            self.workshop._refresh_path_visualization()
        if hasattr(self.workshop, '_refresh_ipl_inst_file_panel'):
            self.workshop._refresh_ipl_inst_file_panel()
        if hasattr(self.workshop, '_set_status'):
            self.workshop._set_status(
                f"Path group ({self.group.header_a}, {self.group.header_b}) "
                f"updated in memory - not yet saved to file")


class _InstanceEditPanel(QWidget):
    """Non-modal, persistent object info/edit panel - shown in the top-
    left corner."""

    _POS_SMALL_STEP = 0.5
    _POS_LARGE_STEP = 10.0
    _ROT_SMALL_STEP = 1.0
    _ROT_LARGE_STEP = 15.0
    _SCALE_SMALL_STEP = 0.1
    _SCALE_LARGE_STEP = 1.0

    def __init__(self, workshop, parent=None): #vers 4
        super().__init__(parent)
        self._workshop = workshop
        self._inst = None
        self._current_ide_obj = None
        self._loader = None
        self._nudge_wide = None   # current reflow state, None forces first layout
        self.setWindowTitle("[IPL object editor]")
        self.setMinimumWidth(560)
        self.setMinimumHeight(320)
        self._lay = QVBoxLayout(self)
        self._lay.setContentsMargins(4, 4, 4, 4)
        self._lay.setSpacing(3)
        self._identity_box = self._add_section("Identity")
        self._identity_box.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self._identity_box.customContextMenuRequested.connect(self._on_identity_context_menu)
        #TODO Show Interior; 0 only when it exists
        #TODO Show LOD index -1 only when we're working with SA format files.
        #TODO Add the ability to read and write binary IPLs from the SA gta3.img
        self._nav_row = QHBoxLayout()
        self._nav_label = QLabel("")
        self._nav_prev_btn = QPushButton("< Prev")
        self._nav_next_btn = QPushButton("Next >")
        self._nav_prev_btn.setFixedHeight(18)
        self._nav_next_btn.setFixedHeight(18)
        self._nav_prev_btn.clicked.connect(lambda: self._workshop._cycle_model_instance(-1))
        self._nav_next_btn.clicked.connect(lambda: self._workshop._cycle_model_instance(1))
        self._nav_row.addWidget(self._nav_prev_btn)
        self._nav_row.addWidget(self._nav_label)
        self._nav_row.addWidget(self._nav_next_btn)
        self._lay.addLayout(self._nav_row)
        self._set_nav_visible(False)
        #TODO the values in the x, y and z boxes need to be visble.
        #TODO thinking about adding < << Value >> > arrow back with keyboard shortcuts, need compact << and >> svg icons.
        self._pos_box, self._pos_spins, self._pos_grid, self._pos_rows = \
            self._add_nudge_section("Position", self._POS_SMALL_STEP, self._POS_LARGE_STEP, self._on_position_nudged)
        self._rot_box, self._rot_spins, self._rot_grid, self._rot_rows = \
            self._add_nudge_section("Rotation (degrees)", self._ROT_SMALL_STEP, self._ROT_LARGE_STEP, self._on_rotation_nudged)
        self._scale_box, self._scale_spins, self._scale_grid, self._scale_rows = \
            self._add_nudge_section("Scale", self._SCALE_SMALL_STEP, self._SCALE_LARGE_STEP, self._on_scale_nudged)
        from apps.methods.imgfactory_svg_icons import (
            get_checkmark_icon, get_undo_icon, get_close_icon, get_save_icon)
        #TODO need to write all stubs and show process of objects being moved in the viewport,
        icon_color = self._workshop._get_icon_color()

        effects_row = QHBoxLayout()
        self._2dfx_btn = self._make_standard_button("2DFX (0)")
        self._tobj_btn = self._make_standard_button("TOBJ (0)")
        self._zero_btn = self._make_standard_button("Set Scaling to 0", tooltip=
            "Sets this instance's scale to (0,0,0) - matches how some\n"
            "converted IPLs (e.g. GTA SA data converted to VC's format)\n"
            "represent 'no real scale data' rather than the normal (1,1,1)\n"
            "unit scale. See the Ignore Scaling option in IPL Controls\n"
            "for treating this the other way around, without editing.")
        self._2dfx_btn.clicked.connect(self._on_2dfx_button_clicked)
        self._tobj_btn.clicked.connect(self._on_tobj_button_clicked)
        self._zero_btn.clicked.connect(self._on_set_scale_zero_clicked)
        effects_row.addWidget(self._2dfx_btn)
        effects_row.addWidget(self._tobj_btn)
        effects_row.addWidget(self._zero_btn)
        #objs Exists. TODO tobj shown below, needs adding with a [timecyc] button in ipl section, 1 minute per second [Editable] to show the game world cycling, night and day cycle with 2dfx objects lighting up. showing the map as if it was in the game.
        #end
        #tobj
        #10976, drivingsch_SFS, drivingschool_sfse, 150, 0, 0, 23
        #11394, hubgrgbeams_SFSe, newhubgrg1_sfse, 100, 4, 7, 17
        #11410, nightlights01_SFSE, nightlights_sfse, 550, 4, 20, 6
        #11411, nightlights02_SFSE, nightlights_sfse, 550, 4, 20, 6
        #11412, nightlights03_SFSE, nightlights_sfse, 550, 4, 20, 6
        #end
        #path TODO a path button is needed to show paths. showing all ped, vehicle, airplace, train paths as plotted lines and nods.
        #end
        #2dfx TODO 2dfx button exists, but needs functionality, reading and displaying 2dfx lighting effects around models, being able to edit these.
        #end
        #anim TODO anim button needed for GTASA, need to look into how this works.
        #11417 ,xenonsign2_SFSe ,xenon_sfse ,SFSe ,150 ,0

        self._lay.addLayout(effects_row)
        self._current_effects = []
        self._current_tobjs = []

        bottom_row = QHBoxLayout()
        apply_btn = self._make_standard_button("Apply", get_checkmark_icon(18, icon_color), tooltip=
            "Apply the edited IPL and IDE lines above (undoable).\n"
            "Position/Rotation/Scale nudges already apply live.")
        undo_btn = self._make_standard_button("Undo", get_undo_icon(18, icon_color))
        close_btn = self._make_standard_button("Close", get_close_icon(18, icon_color))
        save_btn = self._make_standard_button("Save", get_save_icon(18, icon_color), tooltip=
            "Write this object's IPL and IDE changes to disk (backup first).")
        apply_btn.clicked.connect(self._on_apply_clicked)
        undo_btn.clicked.connect(self._on_undo_clicked)
        close_btn.clicked.connect(self._on_close_clicked)
        save_btn.clicked.connect(self._on_save_clicked)
        bottom_row.addWidget(apply_btn)
        bottom_row.addWidget(undo_btn)
        bottom_row.addWidget(close_btn)
        bottom_row.addWidget(save_btn)
        self._lay.addLayout(bottom_row)
        #self._lay.addStretch()

        self._reflow_nudge_rows(wide=True)

    def resizeEvent(self, event): #vers 1
        """Wrap the nudge rows' arrow buttons onto a second line when
        the panel is too narrow for everything on one row, rather than
        clipping/squeezing them."""
        super().resizeEvent(event)
        wide = self.width() >= 260
        if wide != self._nudge_wide:
            self._reflow_nudge_rows(wide)

    def _reflow_nudge_rows(self, wide): #vers 3
        """Rebuild both nudge sections' grids for the given width mode.

        Compact mode (Aug 1 2026)"""
        for grid, rows_info in ((self._pos_grid, self._pos_rows),
                                (self._rot_grid, self._rot_rows),
                                (self._scale_grid, self._scale_rows)):
            while grid.count():
                grid.takeAt(0)
            if wide:
                for i, (label, btn_ll, btn_l, spin, btn_r, btn_rr) in enumerate(rows_info):
                    c = i * 6
                    btn_ll.setVisible(True)
                    btn_rr.setVisible(True)
                    grid.addWidget(label, 0, c)
                    grid.addWidget(btn_ll, 0, c + 1)
                    grid.addWidget(btn_l, 0, c + 2)
                    grid.addWidget(spin, 0, c + 3)
                    grid.addWidget(btn_r, 0, c + 4)
                    grid.addWidget(btn_rr, 0, c + 5)
            else:
                for i, (label, btn_ll, btn_l, spin, btn_r, btn_rr) in enumerate(rows_info):
                    btn_ll.setVisible(True)
                    btn_rr.setVisible(True)
                    r = i * 2
                    grid.addWidget(label, r, 0)
                    grid.addWidget(spin, r, 1, 1, 4)
                    grid.addWidget(btn_ll, r + 1, 0)
                    grid.addWidget(btn_l, r + 1, 1)
                    grid.addWidget(btn_r, r + 1, 2)
                    grid.addWidget(btn_rr, r + 1, 3)
        self._nudge_wide = wide

    def _add_section(self, title): #vers 3
        box = QGroupBox(title)
        box.setSizePolicy(QSizePolicy.Policy.Preferred, QSizePolicy.Policy.Fixed)
        lay = QVBoxLayout(box)
        lay.setContentsMargins(6, 4, 6, 4)
        lay.setSpacing(2)
        self._lay.addWidget(box)
        return box

    _COMPACT_BUTTON_STYLE = "padding: 0px 4px; margin: 0px; border-width: 1px;"

    def _make_standard_button(self, text, icon=None, tooltip=None): #vers 1
        """Build a button matching the exact standard used by the IPL
        Sections row (Open/Close/New/Delete in the IPL tab)"""
        btn = QPushButton(icon, text) if icon is not None else QPushButton(text)
        if icon is not None:
            btn.setIconSize(QSize(18, 18))
        btn.setFixedHeight(18)
        btn.setStyleSheet(self._COMPACT_BUTTON_STYLE)
        if tooltip:
            btn.setToolTip(tooltip)
        return btn


    def _clear_layout_recursive(self, layout): #vers 1
        """Remove and delete every item in a layout, recursing into
        any nested layouts too."""
        while layout.count():
            item = layout.takeAt(0)
            w = item.widget()
            if w is not None:
                w.hide()
                w.deleteLater()
                continue
            nested = item.layout()
            if nested is not None:
                self._clear_layout_recursive(nested)

    def _populate_identity_section(self, ipl_line, ide_line, txd_name, interior, lod_index): #vers 5
        """Identity section: raw IPL line, raw IDE line, and a 3rd row
        with the TXD's real status - one of three messages depending
        on what actually happened when looking it up (Aug 1 2026)"""
        box = self._identity_box
        lay = box.layout()
        self._clear_layout_recursive(lay)

        from apps.methods.imgfactory_svg_icons import get_view_icon
        icon_color = self._workshop._get_icon_color()

        ipl_row = QHBoxLayout()
        ipl_show_btn = self._make_standard_button("", get_view_icon(18, icon_color))
        ipl_show_btn.setFixedWidth(24)
        ipl_show_btn.setToolTip("Jump to this instance's line in the IPL editor")
        ipl_show_btn.clicked.connect(
            lambda: self._workshop._jump_to_ipl_line(self._inst.source_ipl, self._inst.line_no))
        ipl_row.addWidget(ipl_show_btn)
        self._ipl_line_edit = QLineEdit(ipl_line)
        self._ipl_line_edit.setToolTip(
            "id, name, interior, pos x/y/z, scale x/y/z, rot x/y/z/w\nEdit and press Apply")
        self._ipl_line_edit.returnPressed.connect(self._on_apply_clicked)
        ipl_row.addWidget(self._ipl_line_edit, 1)
        lay.addLayout(ipl_row)

        ide_row = QHBoxLayout()
        ide_show_btn = self._make_standard_button("", get_view_icon(18, icon_color))
        ide_show_btn.setFixedWidth(24)
        ide_show_btn.setToolTip("Jump to this object's line in the IDE editor")
        ide_show_btn.setEnabled(self._current_ide_obj is not None)
        if self._current_ide_obj is not None:
            ide_show_btn.clicked.connect(
                lambda: self._workshop._jump_to_ide_line(
                    self._current_ide_obj.source_ide, self._current_ide_obj.line_no))
        ide_row.addWidget(ide_show_btn)
        obj = self._current_ide_obj
        raw = self._workshop._ide_raw_line(obj) if obj is not None else None
        self._ide_line_edit = QLineEdit(raw.rstrip('\r\n') if raw is not None else ide_line)
        self._ide_line_edit.setEnabled(raw is not None)
        self._ide_line_edit.setToolTip(
            "The IDE line exactly as in the file (any section) - edit and press Apply.\n"
            "The model ID must stay the same.")
        self._ide_line_edit.returnPressed.connect(self._on_apply_clicked)
        ide_row.addWidget(self._ide_line_edit, 1)
        if obj is not None:
            src = QLabel(f"{obj.source_ide} (line {obj.line_no})")
            ide_row.addWidget(src)
        lay.addLayout(ide_row)

        row = QHBoxLayout()
        if not txd_name:
            row.addWidget(QLabel("(TXD unknown - no IDE match)"))
        else:
            textures, source, status = self._workshop._get_txd_textures(txd_name)
            if status == 'loaded':
                status_lbl = QLabel(f"{txd_name}.txd is loaded")
                if source:
                    status_lbl.setToolTip(f"Found in: {source}")
                row.addWidget(status_lbl)
                from apps.methods.imgfactory_svg_icons import get_view_icon
                show_btn = self._make_standard_button(
                    "Show", get_view_icon(18, self._workshop._get_icon_color()))
                show_btn.clicked.connect(
                    lambda: self._workshop._show_textures_for_instance(self._inst))
                row.addWidget(show_btn)
            elif status == 'failed':
                status_lbl = QLabel(f"{txd_name}.txd exists but can not be loaded")
                if source:
                    status_lbl.setToolTip(f"Found in: {source} (but failed to parse)")
                row.addWidget(status_lbl)
            else:
                row.addWidget(QLabel(f"{txd_name}.txd is missing from gta3.img"))
        row.addStretch()
        row.addWidget(QLabel(f"Interior: {interior}   LOD index: {lod_index}"))
        lay.addLayout(row)

    def _on_identity_context_menu(self, pos): #vers 1
        """Right-click menu on the Identity section - Aug 1 2026)"""
        if self._inst is None:
            return
        menu = QMenu(self)
        names_action = menu.addAction("Show tex names")
        textures_action = menu.addAction("Show Textures")
        chosen = menu.exec(self._identity_box.mapToGlobal(pos))
        if chosen is names_action:
            self._show_tex_names_dialog()
        elif chosen is textures_action:
            self._show_texture_thumbnail_strip()

    def _show_tex_names_dialog(self): #vers 1
        """RW-Analyze-style texture list: every texture name the
        model's own geometry materials reference (Req), cross-
        referenced against what's actually in the TXD (Incl) - Req
        without Incl means the model needs a texture the TXD doesn't
        have; Incl without Req means the TXD has a texture the model
        doesn't use; both means it's fully satisfied (R&I)."""
        model_cache = getattr(self._workshop, '_model_cache', None)
        inst = self._inst
        if model_cache is None or inst is None:
            return
        dff_model = model_cache.get_geometry(inst.model_name)
        required = []   # (texture_name, alpha_name)
        seen_tex_names = set()
        if dff_model is not None:
            for g in getattr(dff_model, 'geometries', []):
                for m in getattr(g, 'materials', []):
                    # Dedupe (Aug 1 2026)
                    key = m.texture_name.lower() if m.texture_name else ""
                    if m.texture_name and key not in seen_tex_names:
                        seen_tex_names.add(key)
                        required.append((m.texture_name, m.texture_mask))
        loader = getattr(self._workshop, '_world_loader', None)
        obj = loader.get_object(inst.model_id) if loader else None
        txd_name = obj.txd_name if obj else ""
        textures, _source, _status = (self._workshop._get_txd_textures(txd_name)
                                      if txd_name else (None, None, 'missing'))
        included = set((textures or {}).keys())

        table = QTableWidget(0, 3)
        table.setHorizontalHeaderLabels(["Texture Name", "Alpha Name", "Req/Incl"])
        table.setRowCount(len(required))
        for i, (tex_name, alpha_name) in enumerate(required):
            table.setItem(i, 0, QTableWidgetItem(tex_name))
            table.setItem(i, 1, QTableWidgetItem(alpha_name))
            status = "R&I" if tex_name.lower() in included else "Req"
            table.setItem(i, 2, QTableWidgetItem(status))
        table.resizeColumnsToContents()

        dlg = QDialog(self)
        dlg.setWindowTitle(f"Texture List for {inst.model_name}")
        lay = QVBoxLayout(dlg)
        legend = QLabel("Req = required by model geometry   Incl = included in TXD   R&I = both")
        legend.setStyleSheet("color: palette(mid);")
        lay.addWidget(legend)
        lay.addWidget(table)
        dlg.resize(420, 260)
        dlg.exec()

    def _show_texture_thumbnail_strip(self): #vers 5
        """Texture editor dialog - resizable grid of texture
        thumbnails with Add/Del/Export/Replace/Rename/Apply/Save
        actions (Sep 5 2026  own texture-editor button
        request), plus the existing Save as TXD/Save as Single
        Textures export options. Double-click opens a big, resizable
        close-up view of one texture (Sep 5 2026,  "i like
        to see things close up").

        HONEST LIMITATION: Add/Del/Replace/Rename only edit the in-
        memory copy of the textures dict this dialog holds - there is
        no real write-back-to-disk for TXD files anywhere in this
        project yet (same limitation already documented for IPL/IDE/
        DAT/IMG editing in the Editing Panel dock). Apply/Save keep
        those in-memory edits for this dialog's own session, so Save
        as TXD/Save as Single Textures export what you've actually
        changed - they do not write anything back into the original
        .img archive."""
        inst = self._inst
        if inst is None:
            return
        loader = getattr(self._workshop, '_world_loader', None)
        obj = loader.get_object(inst.model_id) if loader else None
        txd_name = obj.txd_name if obj else ""
        textures, _source, status = (self._workshop._get_txd_textures(txd_name)
                                     if txd_name else (None, None, 'missing'))
        if not textures:
            QMessageBox.information(self, "Textures",
                f"{txd_name}.txd is {status}" if txd_name else "(no TXD)")
            return
        # Our own mutable copy - edits here don't touch model_cache's
        # cached dict unless the user explicitly exports them out.
        textures = dict(textures)

        from apps.components.Map_Editor.depends.overlay_icons import OverlayIcons

        dlg = QDialog(self)
        dlg.setWindowTitle(f"Textures - {txd_name}.txd" if txd_name else "Textures")
        dlg.resize(720, 520)
        dlg.setMinimumSize(420, 320)
        outer = QVBoxLayout(dlg)

        list_widget = QListWidget()

        def _selected_tex():
            item = list_widget.currentItem()
            return item.data(Qt.ItemDataRole.UserRole) if item else None

        # Action row (Sep 5 2026,  "when adding new buttons,
        # if space is limited, revert to SVG icons") - icon-only,
        # tooltip carries the label.
        action_row = QHBoxLayout()

        def _mkbtn(icon_fn, tooltip, handler):
            btn = QToolButton()
            btn.setIcon(icon_fn(20))
            btn.setToolTip(tooltip)
            btn.clicked.connect(handler)
            return btn

        add_btn = _mkbtn(OverlayIcons.add_texture_icon, "Add Texture...",
            lambda: self._on_add_texture(dlg, textures, list_widget))
        del_btn = _mkbtn(OverlayIcons.del_texture_icon, "Delete Texture",
            lambda: self._on_del_texture(dlg, textures, list_widget))
        export_btn = _mkbtn(OverlayIcons.export_texture_icon, "Export Texture...",
            lambda: self._on_export_one_texture(dlg, _selected_tex()))
        replace_btn = _mkbtn(OverlayIcons.replace_texture_icon, "Replace Texture...",
            lambda: self._on_replace_texture(dlg, textures, list_widget))
        rename_btn = _mkbtn(OverlayIcons.rename_texture_icon, "Rename Texture...",
            lambda: self._on_rename_texture(dlg, textures, list_widget))
        apply_btn = _mkbtn(OverlayIcons.apply_texture_icon,
            "Apply - keep these edits for this session", lambda: None)
        save_btn = _mkbtn(OverlayIcons.save_texture_icon,
            "Save - keep these edits, then close", lambda: dlg.accept())
        for b in (add_btn, del_btn, export_btn, replace_btn, rename_btn):
            action_row.addWidget(b)
        action_row.addSpacing(12)
        action_row.addWidget(apply_btn)
        action_row.addWidget(save_btn)
        action_row.addStretch()
        save_txd_btn = QPushButton("Save as TXD...")
        save_txd_btn.clicked.connect(lambda: self._save_raw_txd_file(txd_name))
        action_row.addWidget(save_txd_btn)
        save_folder_btn = QPushButton("Save as Single Textures...")
        save_folder_btn.clicked.connect(
            lambda: self._save_all_textures_to_folder(textures, txd_name))
        action_row.addWidget(save_folder_btn)
        outer.addLayout(action_row)

        # Thumbnail grid (Sep 5 2026,  "have the ability to
        # select the listed textures; show as a large, resizable
        # option window. i like to see things close up.") -
        # QListWidget IconMode gives real resizing/scrolling/selection
        # for free, replacing the old fixed QHBoxLayout-of-QLabels strip.
        list_widget.setViewMode(QListWidget.ViewMode.IconMode)
        list_widget.setIconSize(QSize(48, 48))
        list_widget.setResizeMode(QListWidget.ResizeMode.Adjust)
        list_widget.setSpacing(10)
        list_widget.setMovement(QListWidget.Movement.Static)
        self._populate_texture_list_widget(list_widget, textures)
        list_widget.itemDoubleClicked.connect(
            lambda item: self._show_texture_zoom_view(
                dlg, item.data(Qt.ItemDataRole.UserRole)))
        outer.addWidget(list_widget, 1)

        dlg.exec()

    def _populate_texture_list_widget(self, list_widget, textures): #vers 1
        """(Re)build a QListWidget's items from a textures dict,
        keyed by lowercase name - shared by the initial build and by
        Add/Del/Rename/Replace's own refresh after editing."""
        list_widget.clear()
        for tex in textures.values():
            pixmap = self._workshop._create_texture_thumbnail(
                tex.get('rgba_data'), tex.get('width', 0), tex.get('height', 0))
            item = QListWidgetItem(tex.get('name', ''))
            if pixmap is not None:
                item.setIcon(QIcon(pixmap))
            item.setData(Qt.ItemDataRole.UserRole, tex)
            list_widget.addItem(item)

    def _show_texture_zoom_view(self, parent, tex): #vers 2
        """Big, resizable close-up view of one texture (Sep 5 2026,
         "show as a large, resizable option window. i like
        to see things close up.") - shown at native resolution (or
        upscaled if tiny) inside a scroll area, so the window is
        genuinely resizable and the image can be inspected at real
        pixel detail rather than force-fitted and softened."""
        if tex is None:
            return
        rgba = tex.get('rgba_data')
        w, h = tex.get('width', 0), tex.get('height', 0)
        if not (rgba and w > 0 and h > 0):
            return
        qi = QImage(bytes(rgba), w, h, w * 4, QImage.Format.Format_RGBA8888)
        pixmap = QPixmap.fromImage(qi)
        scale = 4 if max(w, h) <= 64 else (2 if max(w, h) <= 128 else 1)
        if scale > 1:
            pixmap = pixmap.scaled(w * scale, h * scale,
                Qt.AspectRatioMode.KeepAspectRatio, Qt.TransformationMode.FastTransformation)

        dlg = QDialog(parent)
        dlg.setWindowTitle(tex.get('name', 'Texture'))
        dlg.resize(min(pixmap.width() + 40, 900), min(pixmap.height() + 80, 900))
        lay = QVBoxLayout(dlg)
        scroll = QScrollArea()
        scroll.setWidgetResizable(False)
        img_lbl = QLabel()
        img_lbl.setPixmap(pixmap)
        scroll.setWidget(img_lbl)
        lay.addWidget(scroll, 1)
        info = QLabel(f"{tex.get('name','')} — {w}x{h} — {tex.get('format','')}"
                      + (f" (shown at {scale}x)" if scale > 1 else ""))
        info.setAlignment(Qt.AlignmentFlag.AlignCenter)
        lay.addWidget(info)
        dlg.exec()

    def _texture_name_exists(self, textures, name, exclude_name=None): #vers 1
        """True if name (case-insensitive) already exists among
        textures - checking every currently-loaded texture name in
        this TXD,  "check with the database that the
        texture I add or rename... doesn't already exist". Since a
        texture's separate normal/alpha companion (e.g. GTA's own
        "namea" convention) is itself just another regular named
        entry in the same TXD, checking every existing name here
        already covers those too - no separate suffix-specific logic
        needed."""
        name_lower = name.strip().lower()
        if not name_lower:
            return True
        exclude_lower = exclude_name.strip().lower() if exclude_name else None
        for tex in textures.values():
            existing = tex.get('name', '').lower()
            if existing == exclude_lower:
                continue
            if existing == name_lower:
                return True
        return False

    def _on_add_texture(self, parent, textures, list_widget): #vers 1
        """Add Texture - load an image file, ask for a name, check it
        doesn't already exist in this TXD, add it to the in-memory
        set (Sep 5 2026  own texture-editor button
        request)."""
        path, _ = QFileDialog.getOpenFileName(
            parent, "Add Texture", os.path.expanduser('~'),
            "Images (*.png *.bmp *.jpg *.jpeg *.tga)")
        if not path:
            return
        qi = QImage(path)
        if qi.isNull():
            QMessageBox.warning(parent, "Add Texture", f"Couldn't read image:\n{path}")
            return
        default_name = os.path.splitext(os.path.basename(path))[0]
        name, ok = QInputDialog.getText(parent, "Add Texture", "Texture name:",
                                         text=default_name)
        if not ok or not name.strip():
            return
        if self._texture_name_exists(textures, name):
            QMessageBox.warning(parent, "Add Texture",
                f'A texture named "{name.strip()}" already exists in this TXD.')
            return
        qi = qi.convertToFormat(QImage.Format.Format_RGBA8888)
        rgba = bytes(qi.bits().asstring(qi.width() * qi.height() * 4))
        tex = {'name': name.strip(), 'width': qi.width(), 'height': qi.height(),
               'rgba_data': rgba, 'format': 'RGBA32'}
        textures[name.strip().lower()] = tex
        self._populate_texture_list_widget(list_widget, textures)

    def _on_del_texture(self, parent, textures, list_widget): #vers 1
        """Delete Texture - remove the selected texture from the in-
        memory set, after confirming."""
        item = list_widget.currentItem()
        if item is None:
            return
        tex = item.data(Qt.ItemDataRole.UserRole)
        name = tex.get('name', '')
        if QMessageBox.question(
                parent, "Delete Texture", f'Delete "{name}" from this TXD?',
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
                ) != QMessageBox.StandardButton.Yes:
            return
        textures.pop(name.lower(), None)
        self._populate_texture_list_widget(list_widget, textures)

    def _on_export_one_texture(self, parent, tex): #vers 1
        """Export Texture - save just the currently-selected texture
        as a full-resolution PNG."""
        if tex is None:
            QMessageBox.information(parent, "Export Texture", "Select a texture first.")
            return
        name = tex.get('name', '') or 'texture'
        rgba, w, h = tex.get('rgba_data'), tex.get('width', 0), tex.get('height', 0)
        if not (rgba and w > 0 and h > 0):
            return
        start_dir = getattr(self._workshop, '_texlist_folder', '') or os.path.expanduser('~')
        path, _ = QFileDialog.getSaveFileName(
            parent, "Export Texture", os.path.join(start_dir, name + '.png'),
            "PNG files (*.png)")
        if not path:
            return
        qi = QImage(bytes(rgba), w, h, w * 4, QImage.Format.Format_RGBA8888)
        if not qi.save(path):
            QMessageBox.warning(parent, "Export Texture", "Failed to save.")

    def _on_replace_texture(self, parent, textures, list_widget): #vers 1
        """Replace Texture - swap the selected texture's own image
        data for a new file's, keeping its existing name."""
        item = list_widget.currentItem()
        if item is None:
            return
        tex = item.data(Qt.ItemDataRole.UserRole)
        name = tex.get('name', '')
        path, _ = QFileDialog.getOpenFileName(
            parent, "Replace Texture", os.path.expanduser('~'),
            "Images (*.png *.bmp *.jpg *.jpeg *.tga)")
        if not path:
            return
        qi = QImage(path)
        if qi.isNull():
            QMessageBox.warning(parent, "Replace Texture", f"Couldn't read image:\n{path}")
            return
        qi = qi.convertToFormat(QImage.Format.Format_RGBA8888)
        rgba = bytes(qi.bits().asstring(qi.width() * qi.height() * 4))
        tex['width'], tex['height'], tex['rgba_data'] = qi.width(), qi.height(), rgba
        tex['format'] = 'RGBA32'
        textures[name.lower()] = tex
        self._populate_texture_list_widget(list_widget, textures)

    def _on_rename_texture(self, parent, textures, list_widget): #vers 1
        """Rename Texture - check the new name doesn't already exist
        in this TXD before renaming."""
        item = list_widget.currentItem()
        if item is None:
            return
        tex = item.data(Qt.ItemDataRole.UserRole)
        old_name = tex.get('name', '')
        new_name, ok = QInputDialog.getText(
            parent, "Rename Texture", "New name:", text=old_name)
        if not ok or not new_name.strip() or new_name.strip() == old_name:
            return
        if self._texture_name_exists(textures, new_name, exclude_name=old_name):
            QMessageBox.warning(parent, "Rename Texture",
                f'A texture named "{new_name.strip()}" already exists in this TXD.')
            return
        textures.pop(old_name.lower(), None)
        tex['name'] = new_name.strip()
        textures[new_name.strip().lower()] = tex
        self._populate_texture_list_widget(list_widget, textures)

    def _save_raw_txd_file(self, txd_name): #vers 1
        """Export the original, unmodified .txd container bytes for
        txd_name (Sep 5 2026,  "we also need to extract the
        .txd file aswell, not just the textures") - straight from the
        IMG archive, no decode/re-encode, so whatever the alpha/
        compression/format story turns out to be, this file is
        exactly what the game itself has."""
        model_cache = getattr(self._workshop, '_model_cache', None)
        raw = model_cache.get_raw_txd(txd_name) if model_cache else None
        if not raw:
            QMessageBox.warning(self, "Save .txd", f"Couldn't read {txd_name}.txd's raw data.")
            return
        start_dir = getattr(self._workshop, '_texlist_folder', '') or os.path.expanduser('~')
        path, _ = QFileDialog.getSaveFileName(
            self, "Save .txd", os.path.join(start_dir, txd_name + '.txd'), "TXD files (*.txd)")
        if not path:
            return
        try:
            with open(path, 'wb') as f:
                f.write(raw)
            QMessageBox.information(self, "Save .txd", f"Saved {txd_name}.txd to {path}")
        except OSError as e:
            QMessageBox.warning(self, "Save .txd", f"Failed to save: {e}")

    def _save_all_textures_to_folder(self, textures, txd_name): #vers 1
        """Export every texture in `textures` (the same dict passed to
        _show_texture_thumbnail_strip) as a full-resolution PNG into a
        folder the user picks. Defaults to the workshop's own
        Texlist folder if one is set, same convention already used
        for texture import/export elsewhere in this file."""
        start_dir = getattr(self._workshop, '_texlist_folder', '') or os.path.expanduser('~')
        folder = QFileDialog.getExistingDirectory(self, "Save Textures To Folder", start_dir)
        if not folder:
            return
        saved, failed = 0, 0
        for tex in textures.values():
            name = tex.get('name', '') or 'texture'
            rgba = tex.get('rgba_data', b'')
            w, h = tex.get('width', 0), tex.get('height', 0)
            if not (rgba and w > 0 and h > 0):
                failed += 1
                continue
            try:
                qi = QImage(bytes(rgba), w, h, w * 4, QImage.Format.Format_RGBA8888).copy()
                if qi.save(os.path.join(folder, name + '.png')):
                    saved += 1
                else:
                    failed += 1
            except Exception:
                failed += 1
        msg = f"Saved {saved} texture(s) to {folder}"
        if failed:
            msg += f" ({failed} failed)"
        QMessageBox.information(self, "Save Textures", msg)

    def _add_nudge_section(self, title, small_step, large_step, on_nudge): #vers 2
        """One label + << < [value] > >> row per axis (X/Y/Z), using
        real chevron icons rather than text, laid out in a QGridLayout
        so _reflow_nudge_rows can wrap the arrow buttons onto a second
        row when the panel is too narrow to fit everything on one
        line (rather than clipping/squeezing, which a plain QHBoxLayout
        would do with no wrapping at all)."""
        box = QGroupBox(title)
        box.setSizePolicy(QSizePolicy.Policy.Preferred, QSizePolicy.Policy.Fixed)
        grid = QGridLayout(box)
        grid.setContentsMargins(6, 4, 6, 4)
        grid.setSpacing(3)
        wf = self._workshop
        icon_sz = 14
        icon_color = wf._get_icon_color()
        icons = {
            'll': wf._render_variant_icon('chevron_left2', None, icon_sz, icon_color, has_menu=False),
            'l':  wf._render_variant_icon('chevron_left',  None, icon_sz, icon_color, has_menu=False),
            'r':  wf._render_variant_icon('chevron_right', None, icon_sz, icon_color, has_menu=False),
            'rr': wf._render_variant_icon('chevron_right2',None, icon_sz, icon_color, has_menu=False),
        }
        spins = {}
        rows_info = []   # per-axis widget refs, for _reflow_nudge_rows
        for axis in ('x', 'y', 'z'):
            label = QLabel(axis.upper() + ":")
            from PyQt6.QtWidgets import QAbstractSpinBox
            btn_ll = QPushButton(); btn_ll.setIcon(icons['ll']); btn_ll.setFixedWidth(28)
            btn_l  = QPushButton(); btn_l.setIcon(icons['l']);   btn_l.setFixedWidth(22)
            spin = QDoubleSpinBox()
            spin.setRange(-100000.0, 100000.0)
            spin.setDecimals(2)
            spin.setButtonSymbols(QAbstractSpinBox.ButtonSymbols.NoButtons)
            spin.setFixedWidth(74)
            btn_r  = QPushButton(); btn_r.setIcon(icons['r']);   btn_r.setFixedWidth(22)
            btn_rr = QPushButton(); btn_rr.setIcon(icons['rr']); btn_rr.setFixedWidth(28)
            # 22px uniform height (Aug 1 2026)
            for _btn in (btn_ll, btn_l, spin, btn_r, btn_rr):
                _btn.setFixedHeight(24)
            btn_ll.setToolTip(f"-{large_step:g}")
            btn_l.setToolTip(f"-{small_step:g}")
            btn_r.setToolTip(f"+{small_step:g}")
            btn_rr.setToolTip(f"+{large_step:g}")
            btn_ll.clicked.connect(lambda _=False, a=axis: on_nudge(a, -large_step))
            btn_l.clicked.connect(lambda _=False, a=axis: on_nudge(a, -small_step))
            btn_r.clicked.connect(lambda _=False, a=axis: on_nudge(a, small_step))
            btn_rr.clicked.connect(lambda _=False, a=axis: on_nudge(a, large_step))
            spin.editingFinished.connect(
                lambda a=axis, s=spin: on_nudge(a, None, absolute=s.value()))
            spins[axis] = spin
            rows_info.append((label, btn_ll, btn_l, spin, btn_r, btn_rr))
        self._lay.addWidget(box)
        return box, spins, grid, rows_info
        return box, spins

    def _set_nav_visible(self, visible): #vers 1
        self._nav_label.setVisible(visible)
        self._nav_prev_btn.setVisible(visible)
        self._nav_next_btn.setVisible(visible)

    def show_for_instance(self, inst, loader, nav_info=None, model_cache=None): #vers 4
        """Refresh every section for a (possibly new) instance - called
        both when first opening the panel and whenever the selection
        changes (Instance List, or the merged Object Browser), so the
        same panel stays open and up to date rather than needing to be
        reopened. nav_info, if given, is (current_index, total_count)
        for a model with multiple placements - shows Prev/Next
        cycling,"""
        self._inst = inst
        self._loader = loader
        if nav_info is not None:
            idx, total = nav_info
            self._set_nav_visible(total > 1)
            self._nav_label.setText(f"Instance {idx + 1} of {total}")
        else:
            self._set_nav_visible(False)
        obj = loader.get_object(inst.model_id) if loader else None
        effects = loader.get_2dfx_for_model(inst.model_id) if loader else []
        tobjs = loader.get_tobj_for_model(inst.model_id) if loader else []

        self.setWindowTitle(
            f"[IPL object editor] ID {inst.model_id} | {inst.model_name} | {inst.source_ipl}")
        ipl_line = (f"{inst.model_id}, {inst.model_name}, {inst.interior}, "
                   f"{inst.pos_x:.10g}, {inst.pos_y:.10g}, {inst.pos_z:.10g}, "
                   f"{inst.scale_x:.10g}, {inst.scale_y:.10g}, {inst.scale_z:.10g}, "
                   f"{inst.rot_x:.10g}, {inst.rot_y:.10g}, {inst.rot_z:.10g}, {inst.rot_w:.10g}")
        if obj is not None:
            ide_fields = [str(obj.model_id), obj.model_name, obj.txd_name]
            ide_fields += [str(v) for v in obj.extra.values()]
            ide_line = ", ".join(ide_fields)
            txd_name = obj.txd_name
        else:
            ide_line = "(no matching IDE entry found)"
            txd_name = ""
        self._current_ide_obj = obj
        self._populate_identity_section(ipl_line, ide_line, txd_name, inst.interior, inst.lod_index)

        self._refresh_position_spins()
        self._refresh_rotation_spins()
        self._refresh_scale_spins()

        self._current_effects = effects
        self._current_tobjs = tobjs
        self._2dfx_btn.setText(f"2DFX ({len(effects)})")
        self._tobj_btn.setText(f"TOBJ ({len(tobjs)})")

    def _refresh_position_spins(self): #vers 1
        inst = self._inst
        for axis, spin in self._pos_spins.items():
            spin.blockSignals(True)
            spin.setValue(getattr(inst, f"pos_{axis}"))
            spin.blockSignals(False)

    def _refresh_rotation_spins(self): #vers 2
        """Shows the *effective* rotation (matching what the viewport
        actually renders), not necessarily the raw stored quaternion
        verbatim - see ModelWorkshop._effective_rotation/_conjugate_
        rotation_for_game for why (Aug 1 2026)"""
        inst = self._inst
        ex, ey, ez, ew = self._workshop._conjugate_rotation_for_game(
            inst.rot_x, inst.rot_y, inst.rot_z, inst.rot_w)
        roll, pitch, yaw = quat_to_euler_degrees(ex, ey, ez, ew)
        for axis, val in zip(('x', 'y', 'z'), (roll, pitch, yaw)):
            spin = self._rot_spins[axis]
            spin.blockSignals(True)
            spin.setValue(val)
            spin.blockSignals(False)

    def _on_position_nudged(self, axis, delta, absolute=None): #vers 2
        if self._inst is None:
            return
        attr = f"pos_{axis}"
        inst = self._inst
        old_val = getattr(inst, attr)
        new_val = absolute if absolute is not None else old_val + delta
        setattr(inst, attr, new_val)
        self._refresh_position_spins()
        self._workshop._on_instance_edited(inst)
        # Undo support (Aug 18 2026)
        def _undo():
            setattr(inst, attr, old_val)
            if self._inst is inst:
                self._refresh_position_spins()
            self._workshop._on_instance_edited(inst)
        def _redo():
            setattr(inst, attr, new_val)
            if self._inst is inst:
                self._refresh_position_spins()
            self._workshop._on_instance_edited(inst)
        model_name = getattr(inst, 'model_name', '?')
        self._workshop._push_map_undo(_undo, _redo, f"Move {model_name} ({axis.upper()})")

    def _on_rotation_nudged(self, axis, delta, absolute=None): #vers 3
        """Edits the *effective* rotation shown in the spin boxes, then
        converts back through the same conjugate (its own inverse) to
        get what should actually be stored in inst.rot_x/y/z/w - see
        _refresh_rotation_spins."""
        if self._inst is None:
            return
        inst = self._inst
        old_quat = (inst.rot_x, inst.rot_y, inst.rot_z, inst.rot_w)
        ex, ey, ez, ew = self._workshop._conjugate_rotation_for_game(*old_quat)
        roll, pitch, yaw = quat_to_euler_degrees(ex, ey, ez, ew)
        current = {'x': roll, 'y': pitch, 'z': yaw}
        current[axis] = absolute if absolute is not None else current[axis] + delta
        nx, ny, nz, nw = euler_degrees_to_quat(current['x'], current['y'], current['z'])
        x, y, z, w = self._workshop._conjugate_rotation_for_game(nx, ny, nz, nw)
        new_quat = (x, y, z, w)
        inst.rot_x, inst.rot_y, inst.rot_z, inst.rot_w = new_quat
        self._refresh_rotation_spins()
        self._workshop._on_instance_edited(inst)
        # Undo support - stores the raw quaternion directly (what's
        # actually persisted on the instance), not the euler angles
        # shown in the UI, so restoring it can't accumulate rounding
        # drift from repeatedly converting through euler and back.
        def _undo():
            inst.rot_x, inst.rot_y, inst.rot_z, inst.rot_w = old_quat
            if self._inst is inst:
                self._refresh_rotation_spins()
            self._workshop._on_instance_edited(inst)
        def _redo():
            inst.rot_x, inst.rot_y, inst.rot_z, inst.rot_w = new_quat
            if self._inst is inst:
                self._refresh_rotation_spins()
            self._workshop._on_instance_edited(inst)
        model_name = getattr(inst, 'model_name', '?')
        self._workshop._push_map_undo(_undo, _redo, f"Rotate {model_name} ({axis.upper()})")

    def _on_scale_nudged(self, axis, delta, absolute=None): #vers 2
        if self._inst is None:
            return
        attr = f"scale_{axis}"
        inst = self._inst
        old_val = getattr(inst, attr)
        new_val = absolute if absolute is not None else old_val + delta
        setattr(inst, attr, new_val)
        self._refresh_scale_spins()
        self._workshop._on_instance_edited(inst)
        def _undo():
            setattr(inst, attr, old_val)
            if self._inst is inst:
                self._refresh_scale_spins()
            self._workshop._on_instance_edited(inst)
        def _redo():
            setattr(inst, attr, new_val)
            if self._inst is inst:
                self._refresh_scale_spins()
            self._workshop._on_instance_edited(inst)
        model_name = getattr(inst, 'model_name', '?')
        self._workshop._push_map_undo(_undo, _redo, f"Scale {model_name} ({axis.upper()})")

    def _refresh_scale_spins(self): #vers 1
        inst = self._inst
        for axis, spin in self._scale_spins.items():
            spin.blockSignals(True)
            spin.setValue(getattr(inst, f"scale_{axis}"))
            spin.blockSignals(False)

    def _on_set_scale_zero_clicked(self): #vers 2
        """Set this instance's scale to (0,0,0)"""
        if self._inst is None:
            return
        inst = self._inst
        old_scale = (inst.scale_x, inst.scale_y, inst.scale_z)
        inst.scale_x = inst.scale_y = inst.scale_z = 0.0
        self._refresh_scale_spins()
        self._workshop._on_instance_edited(inst)
        def _undo():
            inst.scale_x, inst.scale_y, inst.scale_z = old_scale
            if self._inst is inst:
                self._refresh_scale_spins()
            self._workshop._on_instance_edited(inst)
        def _redo():
            inst.scale_x = inst.scale_y = inst.scale_z = 0.0
            if self._inst is inst:
                self._refresh_scale_spins()
            self._workshop._on_instance_edited(inst)
        model_name = getattr(inst, 'model_name', '?')
        self._workshop._push_map_undo(_undo, _redo, f"Zero scale on {model_name}")

    def _on_2dfx_button_clicked(self): #vers 1
        """Show this instance's 2DFX effects in a popup - replaces the
        earlier always-visible text section (Aug 1 2026)"""
        if not self._current_effects:
            QMessageBox.information(self, "2DFX Effects", "(none)")
            return
        lines = [f"#{i+1}: {e.obj_type} (line {e.line_no}, {e.source_ide})"
                for i, e in enumerate(self._current_effects)]
        QMessageBox.information(self, "2DFX Effects", "\n".join(lines))

    def _on_tobj_button_clicked(self): #vers 2
        """Show this instance's TOBJ (timed object) variants in a
        popup - replaces the earlier always-visible text section.

        Now shows each variant's time_on/time_off range too (Aug 1
        2026)"""
        if not self._current_tobjs:
            QMessageBox.information(self, "TOBJ (Timed Object) Variants", "(none)")
            return
        lines = []
        for t in self._current_tobjs:
            time_on = t.extra.get('time_on')
            time_off = t.extra.get('time_off')
            time_part = f"  -  visible {time_on:02d}:00-{time_off:02d}:00" \
                if time_on is not None and time_off is not None else ""
            lines.append(f"{t.model_name} (ID {t.model_id}, {t.source_ide} line {t.line_no}){time_part}")
        QMessageBox.information(self, "TOBJ (Timed Object) Variants", "\n".join(lines))

    def _on_close_clicked(self): #vers 2
        self.hide()

    def _on_apply_clicked(self): #vers 3
        """Parse the edited IPL / IDE lines and apply them (undoable)."""
        inst = self._inst
        if inst is None:
            return
        ws = self._workshop
        try:
            p = [x.strip() for x in self._ipl_line_edit.text().split(',')]
            if len(p) != 13:
                raise ValueError(f"IPL line needs 13 fields, got {len(p)}")
            new_inst = {'model_id': int(p[0]), 'model_name': p[1], 'interior': int(p[2]),
                        'pos_x': float(p[3]), 'pos_y': float(p[4]), 'pos_z': float(p[5]),
                        'scale_x': float(p[6]), 'scale_y': float(p[7]), 'scale_z': float(p[8]),
                        'rot_x': float(p[9]), 'rot_y': float(p[10]), 'rot_z': float(p[11]),
                        'rot_w': float(p[12])}
        except ValueError as e:
            QMessageBox.warning(self, "Apply", f"IPL line: {e}")
            return
        obj = self._current_ide_obj
        new_raw = None
        if obj is not None and self._ide_line_edit.isEnabled():
            cur = (ws._ide_raw_line(obj) or '').rstrip('\r\n')
            if self._ide_line_edit.text() != cur:
                new_raw = self._ide_line_edit.text()
                err = ws._check_ide_raw(obj, new_raw)
                if err:
                    QMessageBox.warning(self, "Apply", f"IDE line: {err}")
                    return
        ws._apply_instance_fields(inst, new_inst)
        if new_raw is not None:
            ws._apply_ide_raw(obj, new_raw)
        self._refresh_position_spins()
        self._refresh_rotation_spins()
        self._refresh_scale_spins()

    def _on_undo_clicked(self): #vers 2
        """Undo/redo for mapping changes (Aug 18 2026)"""
        if QApplication.keyboardModifiers() & Qt.KeyboardModifier.ShiftModifier:
            self._workshop._map_redo()
        else:
            self._workshop._map_undo()

    def _on_save_clicked(self): #vers 3
        """Save this object's IPL (if changed) and its IDE line (if edited)."""
        inst = self._inst
        if inst is None:
            return
        ws = self._workshop
        ws._refresh_dirty_ipls()
        saved = []
        if inst.source_ipl in ws._dirty_ipls:
            if not ws._save_ipl_in_place(inst.source_ipl):
                return
            saved.append(inst.source_ipl)
        obj = self._current_ide_obj
        if obj is not None and obj.model_id in ws._ide_pending:
            if not ws._save_pending_ide([ws._ide_pending[obj.model_id]]):
                return
            saved.append(obj.source_ide)
        ws._set_status("Saved " + ", ".join(saved) if saved else "Nothing to save for this object")


class _FilteredLoaderStub:
    """Minimal loader-shaped wrapper so _populate_instance_list (which
    expects .instances and .get_object()) can be fed a filtered subset
    of instances (from the IPL Sections panel's Show/Hide toggles)
    without needing a second, real GTAWorldLoader - object definitions
    don't change when filtering by IPL, only which instances are
    visible, so get_object() just delegates to the original loader."""

    def __init__(self, instances, real_loader):
        self.instances = instances
        self._real_loader = real_loader

    def get_object(self, model_id):
        if self._real_loader is None:
            return None
        return self._real_loader.get_object(model_id)


class _ObjectBrowserModel(QAbstractTableModel):
    """Backs the Object Browser QTableView - shows the loaded object
    catalog (IDEObject definitions from GTAWorldLoader.objects), each
    row's instance count (how many placements in the currently loaded
    world use that model) and favourite status. Supports four view
    modes and live search filtering, applied in set_filter()/
    set_search() by recomputing self._rows (the currently visible,
    sorted/filtered subset) rather than the model touching the full
    object catalog on every repaint."""

    _HEADERS = ["★", "ID", "Model", "TXD", "Instances", "Size"]

    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self._all_objects: list = []       # every IDEObject, set once per load
        self._instance_counts: dict = {}   # model_id -> instance count
        self._favourites: set = set()      # favourited model_ids
        self._mode = 'all'                 # 'all' | 'most_used' | 'favourites' | 'generic'
        self._search = ''
        self._rows: list = []              # currently visible IDEObjects
        self._model_cache = None           # ModelCache, set via set_model_cache()

    def set_model_cache(self, model_cache): #vers 1
        """Set the ModelCache used to look up each object's real
        width/height (from its actual loaded geometry, if available -
        shows nothing for a model that hasn't been loaded yet, per the
        lazy-loading design, rather than blocking to load it just to
        show a size)."""
        self._model_cache = model_cache

    def set_objects(self, objects, instance_counts, favourites): #vers 1
        self._all_objects = objects
        self._instance_counts = instance_counts
        self._favourites = set(favourites)
        self._recompute()

    def set_mode(self, mode): #vers 1
        self._mode = mode
        self._recompute()

    def set_search(self, text): #vers 1
        self._search = text.lower().strip()
        self._recompute()

    def toggle_favourite(self, model_id): #vers 1
        if model_id in self._favourites:
            self._favourites.discard(model_id)
        else:
            self._favourites.add(model_id)
        self._recompute()
        return sorted(self._favourites)

    def _is_generic(self, obj): #vers 1
        """Heuristic for 'generic' objects - matches the real naming
        convention seen in GTA3/VC/SA .dat files and IDE data (e.g.
        MODELS\\GENERIC\\WHEELS.DFF, GENERIC.TXD): model or TXD name
        containing 'generic'."""
        return 'generic' in obj.model_name.lower() or 'generic' in obj.txd_name.lower()

    def _recompute(self): #vers 1
        self.beginResetModel()
        rows = self._all_objects
        if self._mode == 'favourites':
            rows = [o for o in rows if o.model_id in self._favourites]
        elif self._mode == 'generic':
            rows = [o for o in rows if self._is_generic(o)]
        if self._search:
            rows = [o for o in rows if self._search in o.model_name.lower()
                    or self._search in o.txd_name.lower()]
        if self._mode == 'most_used':
            rows = sorted(rows, key=lambda o: self._instance_counts.get(o.model_id, 0),
                          reverse=True)
        else:
            rows = sorted(rows, key=lambda o: o.model_name.lower())
        self._rows = rows
        self.endResetModel()

    def rowCount(self, parent=None): #vers 1
        return len(self._rows)

    def columnCount(self, parent=None): #vers 1
        return len(self._HEADERS)

    def headerData(self, section, orientation, role=Qt.ItemDataRole.DisplayRole): #vers 1
        if role != Qt.ItemDataRole.DisplayRole:
            return None
        if orientation == Qt.Orientation.Horizontal:
            return self._HEADERS[section]
        return str(section + 1)

    def data(self, index, role=Qt.ItemDataRole.DisplayRole): #vers 2
        if not index.isValid():
            return None
        obj = self._rows[index.row()]
        col = index.column()
        if role == Qt.ItemDataRole.DisplayRole:
            if col == 0:
                return "★" if obj.model_id in self._favourites else ""
            if col == 1:
                return str(obj.model_id)
            if col == 2:
                return obj.model_name
            if col == 3:
                return obj.txd_name
            if col == 4:
                return str(self._instance_counts.get(obj.model_id, 0))
            if col == 5:
                if self._model_cache is not None:
                    dims = self._model_cache.get_dimensions(obj.model_name)
                    if dims is not None:
                        width, depth, height = dims
                        return f"{width:.1f} × {height:.1f}"
                return ""
        return None

    def object_at(self, row): #vers 1
        if 0 <= row < len(self._rows):
            return self._rows[row]
        return None

    def flags(self, index): #vers 1
        """Rows can be dragged into the viewport to place the model."""
        f = super().flags(index)
        return f | Qt.ItemFlag.ItemIsDragEnabled if index.isValid() else f

    def mimeTypes(self): #vers 1
        return ['application/x-imgfactory-model-id']

    def mimeData(self, indexes): #vers 1
        """Model ID of the dragged row for DFFViewport.dropEvent."""
        from PyQt6.QtCore import QMimeData
        md = QMimeData()
        obj = self.object_at(indexes[0].row()) if indexes else None
        if obj is not None:
            md.setData('application/x-imgfactory-model-id', str(obj.model_id).encode())
        return md


class _InstanceTableModel(QAbstractTableModel):
    """Backs the Instance List QTableView - resolves/formats each row's
    display data on demand via data(), rather than eagerly building a
    QTableWidgetItem per cell for every instance up front (the old
    QTableWidget approach, timed at 2.6s of UI freeze for 51,711
    instances - two thirds of which was resolving every single
    instance's TXD name immediately regardless of whether that row
    would ever actually be scrolled into view).

    Just ID + Model columns - TXD/Position/Interior/Source IPL are
    still available (via the object detail panel opened from a row),
    just not shown as default columns any more."""

    _HEADERS = ["ID", "Model"]

    def __init__(self, instances, loader, parent=None, changed_fn=None): #vers 3
        super().__init__(parent)
        self._instances = instances
        self._loader = loader
        self._txd_cache = {}   # model_id -> resolved TXD name, filled lazily
        self._changed_fn = changed_fn   # inst -> True if edited since last save

    def rowCount(self, parent=None): #vers 1
        return len(self._instances)

    def columnCount(self, parent=None): #vers 1
        return len(self._HEADERS)

    def headerData(self, section, orientation, role=Qt.ItemDataRole.DisplayRole): #vers 1
        if role != Qt.ItemDataRole.DisplayRole:
            return None
        if orientation == Qt.Orientation.Horizontal:
            return self._HEADERS[section]
        return str(section + 1)


    def data(self, index, role=Qt.ItemDataRole.DisplayRole): #vers 3
        if not index.isValid():
            return None
        if role == Qt.ItemDataRole.ForegroundRole:
            if self._changed_fn is not None and self._changed_fn(self._instances[index.row()]):
                return QBrush(QColor(230, 140, 30))
            return None
        if role != Qt.ItemDataRole.DisplayRole:
            return None
        inst = self._instances[index.row()]
        col = index.column()
        if col == 0:
            return str(inst.model_id)
        if col == 1:
            return inst.model_name
        return None

    def instance_at(self, row): #vers 1
        """Look up the raw IPLInstance for a given row - used by the
        row-selection handler to centre the world-view cameras."""
        if 0 <= row < len(self._instances):
            return self._instances[row]
        return None


class _SizeAdaptiveStackedWidget(QStackedWidget):
    """QStackedWidget defaults to sizing itself (sizeHint/minimumSizeHint)
    to the LARGEST of its pages, regardless of which one is currently
    visible - so a stack holding Object Browser's own table plus the
    merged IDE/IPL/DAT/IMG tabs was reporting the IMG tab's much wider
    content (~509px) as its minimum even while showing the object
    table (~72px), permanently locking Object Browser's width no
    matter how compact everything else in the dock became."""
    def sizeHint(self): #vers 1
        cw = self.currentWidget()
        return cw.sizeHint() if cw is not None else super().sizeHint()

    def minimumSizeHint(self): #vers 1
        cw = self.currentWidget()
        return cw.minimumSizeHint() if cw is not None else super().minimumSizeHint()


class _IplLoadDialog(QDialog):
    """One IPL load dialog for single or multiple files: file bar, per-model rows, timed close."""

    CLOSE_AFTER = 5     # seconds before auto close once finished

    def __init__(self, parent=None, total_files=1): #vers 2
        super().__init__(parent)
        import time as _time
        self._time = _time
        self.setWindowTitle("Loading IPL")
        self.resize(600, 440)
        self.cancelled = False
        self._finished = False
        self._total_files = max(1, total_files)
        self._countdown = self.CLOSE_AFTER
        self._last_pump = 0.0
        lay = QVBoxLayout(self)
        top = QHBoxLayout()
        self._file_lbl = QLabel("")
        f = self._file_lbl.font(); f.setBold(True); self._file_lbl.setFont(f)
        self._bar = QProgressBar(); self._bar.setRange(0, 100)
        self._files_lbl = QLabel("")
        top.addWidget(self._file_lbl); top.addWidget(self._bar, 1); top.addWidget(self._files_lbl)
        lay.addLayout(top)
        self._table = QTableWidget(0, 3)
        self._table.setHorizontalHeaderLabels(["Model", "Progress", "Textures"])
        self._table.verticalHeader().setVisible(False)
        self._table.verticalHeader().setDefaultSectionSize(20)
        self._table.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        self._table.setSelectionMode(QAbstractItemView.SelectionMode.NoSelection)
        hh = self._table.horizontalHeader()
        hh.setSectionResizeMode(0, QHeaderView.ResizeMode.Stretch)
        for col, width in ((1, 70), (2, 170)):   # fixed: no relayout per row
            hh.setSectionResizeMode(col, QHeaderView.ResizeMode.Fixed)
            self._table.setColumnWidth(col, width)
        lay.addWidget(self._table, 1)
        self._msgs = QListWidget(); self._msgs.setMaximumHeight(90); self._msgs.setVisible(False)
        lay.addWidget(self._msgs)
        bottom = QHBoxLayout()
        self._keep = QCheckBox("Keep open"); self._keep.setVisible(False)
        self._btn = QPushButton("Cancel")
        self._btn.clicked.connect(self._on_btn)
        bottom.addWidget(self._keep); bottom.addStretch(); bottom.addWidget(self._btn)
        lay.addLayout(bottom)
        self._timer = QTimer(self); self._timer.timeout.connect(self._tick)
        self.show(); self.raise_()
        for _ in range(3):              # let the window map and paint first
            QApplication.processEvents()

    def _pump(self, force=False): #vers 2
        now = self._time.monotonic()
        if force or now - self._last_pump >= 0.1:
            self._table.scrollToBottom()
            QApplication.processEvents()
            self._last_pump = now

    def start_file(self, name, idx=0): #vers 1
        """New IPL: header, Files i/N, empty model list."""
        self.setWindowTitle(f"Loading {name}")
        self._file_lbl.setText(f"Loading {name}")
        self._files_lbl.setText(f"Files {idx + 1}/{self._total_files}")
        self._bar.setValue(0)
        self._table.setRowCount(0)
        self._pump(True)

    def set_file_progress(self, done, total): #vers 1
        self._bar.setValue(int(done * 100 / total) if total else 100)
        self._pump()

    def model_row(self, model_name): #vers 2
        """Add a 'Parsing x.dff' row, return its index."""
        r = self._table.rowCount()
        self._table.insertRow(r)
        self._table.setItem(r, 0, QTableWidgetItem(f"Parsing {model_name}.dff"))
        self._table.setItem(r, 1, QTableWidgetItem("0%"))
        self._table.setItem(r, 2, QTableWidgetItem(""))
        self._pump()
        return r

    def set_model(self, row, pct, textures=None): #vers 1
        self._table.item(row, 1).setText(f"{pct}%")
        if textures is not None:
            self._table.item(row, 2).setText(textures)
        self._pump()

    def message(self, text): #vers 1
        """Warnings, missing files, linked streams."""
        self._msgs.setVisible(True)
        self._msgs.addItem(text)
        self._msgs.scrollToBottom()
        self._pump()

    def finish(self, summary): #vers 1
        """Done: full bar, summary, timed close unless Keep open."""
        self._finished = True
        self._file_lbl.setText(summary)
        self._bar.setValue(100)
        self._keep.setVisible(True)
        self._btn.setText(f"Close ({self._countdown})")
        self._timer.start(1000)
        self._pump(True)

    def file_started(self, path): #vers 1
        """IMG batch report: archive started (one-window world load)."""
        self._files_lbl.setText(os.path.basename(path))
        self._pump()

    def file_done(self, path, img_file): #vers 1
        """IMG batch report: archive loaded."""
        n = len(getattr(img_file, 'entries', []) or [])
        self.message(f"{os.path.basename(path)} - {n:,} entries")

    def file_failed(self, path, message): #vers 1
        """IMG batch report: archive failed, reason logged."""
        first = (message or "unknown error").strip().splitlines()[0]
        self.message(f"{os.path.basename(path)} - FAILED: {first}")

    def file_skipped(self, path, reason): #vers 1
        """IMG batch report: archive skipped."""
        self.message(f"{os.path.basename(path)} - skipped: {reason}")

    def show_summary(self, title, summary_text, preloaded_files, seconds=None): #vers 1
        """World summary in this window; timed close only when seconds set."""
        self.setWindowTitle(title)
        self._table.setVisible(False)
        self._msgs.setMaximumHeight(16777215)
        self._msgs.setVisible(True)
        for line in summary_text.splitlines():
            if line.strip():
                self._msgs.addItem(line)
        if preloaded_files:
            self._msgs.addItem("Preloaded:")
            self._msgs.addItems([f"    {f}" for f in preloaded_files])
        if seconds:
            self._countdown = int(seconds)
        self.finish(title)
        if not seconds:
            self._keep.setChecked(True)
            self._tick()

    def _tick(self): #vers 1
        if self._keep.isChecked():
            self._timer.stop()
            self._btn.setText("Close")
            return
        self._countdown -= 1
        if self._countdown <= 0:
            self._timer.stop()
            self.close()
            return
        self._btn.setText(f"Close ({self._countdown})")

    def _on_btn(self): #vers 1
        if self._finished:
            self.close()
            return
        self.cancelled = True
        self._btn.setEnabled(False)
        self._btn.setText("Cancelling...")

    def closeEvent(self, event): #vers 1
        if not self._finished:
            self.cancelled = True
        self._timer.stop()
        super().closeEvent(event)


#  diagnostic marker (Aug 20 2026) - see __init__'s own docstring
