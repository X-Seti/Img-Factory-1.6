#!/usr/bin/env python3
# apps/components/Map_Editor/map_workshop.py - Version: 1 (Build 62)
# X-Seti - Jul 28 2026 - Map Workshop - Img Factory 1.6 GTA map/IPL editor.
#
# Forked from DP5_Workshop/dp5_workshop.py (v91, build 418) as the UI
# foundation - the ribbon system (Plotting/Shapes/Annotate-style ribbons,
# Ribbon Manager with move/save/load presets, right-click ribbon context
# menu), the docking/main-window shell, and the settings dialog framework
# are all being reused and built on for the map editor. The paint-specific
# canvas, brush tools, and drawing logic are being stripped out and
# replaced with map-editing content (IPL/IDE placement data, an OpenGL 3D
# viewport, and other GTA-map-specific render elements) incrementally.
#
# Original DP5_Workshop merge history (kept for context, no longer
# accurate to this file's actual contents):
#   map_workshop.py   (v1 container skeleton)
#   dp5_functions.py  (MapCanvas, DP5PaletteBar, DP5PaintEditor logic)
#   color_pal_presets.py (ColorPalPresetsMixin — retro palette presets)
#   map_workshop_concept.py (dual palette concept)
#   dp5_paint_clone.py (tool system reference)
#
# Layout (DPaint5-faithful):
#   Left:   bitmap list panel (import / export / delete)
#   Centre: menubar + zoomable scrollable MapCanvas
#   Right:  2-col tool gadget bar (SVG icons) + brush size slider +
#           FG/BG swatches + IMAGE palette strip + USER palette (retro presets)


import os, sys, random, json
import math


def quat_to_euler_degrees(x, y, z, w): #vers 1
    """Convert a quaternion to (roll, pitch, yaw) euler angles in
    degrees - standard formula, round-trip verified against
    euler_degrees_to_quat. Used to present an IPLInstance's rotation
    (stored as a quaternion) as editable X/Y/Z degree values."""
    sinr_cosp = 2 * (w * x + y * z)
    cosr_cosp = 1 - 2 * (x * x + y * y)
    roll = math.atan2(sinr_cosp, cosr_cosp)
    sinp = 2 * (w * y - z * x)
    pitch = math.copysign(math.pi / 2, sinp) if abs(sinp) >= 1 else math.asin(sinp)
    siny_cosp = 2 * (w * z + x * y)
    cosy_cosp = 1 - 2 * (y * y + z * z)
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
from collections import deque
from pathlib import Path
from typing import Optional, List, Tuple

os.environ['QT_QPA_PLATFORM'] = 'xcb'
os.environ['QSG_RHI_BACKEND'] = 'opengl'

current_dir  = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(os.path.dirname(os.path.dirname(current_dir)))
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

from PyQt6.QtWidgets import (
    QApplication, QWidget, QVBoxLayout, QHBoxLayout, QSplitter,
    QListWidget, QListWidgetItem, QLabel, QPushButton, QFrame, QRadioButton,
    QLineEdit, QMessageBox, QGroupBox, QComboBox,
    QSpinBox, QTabWidget, QScrollArea, QCheckBox, QDialog,
    QFormLayout, QFontComboBox, QSlider, QSizePolicy,
    QAbstractItemView, QMenu, QMenuBar, QStatusBar,
    QFileDialog, QColorDialog, QGridLayout, QInputDialog, QDockWidget,
    QTableWidget, QTableWidgetItem, QHeaderView, QDoubleSpinBox, QProgressDialog
)
from PyQt6.QtCore import Qt, QPoint, QPointF, QRect, pyqtSignal, QSize, QTimer, QAbstractTableModel
from PyQt6.QtGui import (
    QImage, QPixmap, QPainter, QColor, QCursor, QAction,
    QMouseEvent, QWheelEvent, QFont, QIcon, QPen, QBrush,
    QPainterPath, QKeySequence
)

#TODO search and remove dead code from DP5.

##Methods list -
# MapSettings.__init__
# MapSettings._load
# MapSettings.get
# MapSettings.save
# MapSettings.set
# MapSettingsDialog.__init__
# MapSettingsDialog._accept
##Methods list - tags below are a draft aid, not a
# guarantee - CHECK-CHAIN and LIKELY-* entries were not each traced
# through their actual callers the way VERIFIED-LIVE/VERIFIED-DEAD
# entries were (see recent commits for that process) - verify before
# deleting anything tagged anything other than VERIFIED-DEAD.
#
# MapWorkshop.__init__  [FRAMEWORK] Qt override / entry point, always live
# MapWorkshop._activate_stamp_mode  [VERIFIED-LIVE] traced actual caller this session
# MapWorkshop._apply_menu_bar_style  [VERIFIED-LIVE] traced actual caller this session
# MapWorkshop._apply_mode_to_canvas  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._apply_ribbon_style  [VERIFIED-LIVE] traced actual caller this session
# MapWorkshop._apply_selection_rotation  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._apply_theme  [VERIFIED-LIVE] traced actual caller this session
# MapWorkshop._build_canvas_menus  [VERIFIED-LIVE] traced actual caller this session
# MapWorkshop._build_menus_into_qmenu  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._build_ribbons_from_assignment  [VERIFIED-LIVE] traced actual caller this session
# MapWorkshop._build_tool_ribbon  [VERIFIED-LIVE] traced actual caller this session
# MapWorkshop._capture_canvas_tab_state  [VERIFIED-LIVE] traced actual caller this session
# MapWorkshop._copy_selection  [VERIFIED-LIVE] traced actual caller this session
# MapWorkshop._create_annotate_ribbon  [VERIFIED-DEAD] traced this session - zero live callers
# MapWorkshop._create_canvas_tabs_ribbon  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._create_centre_panel  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._create_image_ops_ribbon  [VERIFIED-DEAD] traced this session - zero live callers
# MapWorkshop._create_tool_settings_ribbon  [VERIFIED-DEAD] traced this session - zero live callers
# MapWorkshop._create_toolbar  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._cut_selection  [LIKELY-LIVE] 3 references - probably live, not individually confirmed
# MapWorkshop._deselect  [CHECK-CHAIN] 2 reference(s) - verify caller is itself live before removing
# MapWorkshop._dither_bayer_canvas  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._dither_checker_canvas  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._dither_floyd_steinberg  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._dp5_blur  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._dp5_edge_detect  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._dp5_emboss  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._dp5_sharpen  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._duplicate_last_stamp  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._fill_canvas  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._fit_canvas_to_viewport  [LIKELY-LIVE] 11 references - probably live, not individually confirmed
# MapWorkshop._fit_img_pal_height  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._flip_h  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._flip_v  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._get_icon_color  [VERIFIED-LIVE] traced actual caller this session
# MapWorkshop._get_resize_corner  [CHECK-CHAIN] 2 reference(s) - verify caller is itself live before removing
# MapWorkshop._get_resize_direction  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._get_ribbon_assignment  [VERIFIED-LIVE] traced actual caller this session
# MapWorkshop._get_ribbon_tile_bg  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._get_tool_menu_style  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._get_user_palette_rgb  [LIKELY-LIVE] 5 references - probably live, not individually confirmed
# MapWorkshop._group_palette  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._handle_corner_resize  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._handle_resize  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._is_on_draggable_area  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._launch_theme_settings  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._limit_cell_colours  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._make_dock_collapsible  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._make_dropdown_tool_button  [LIKELY-LIVE] 5 references - probably live, not individually confirmed
# MapWorkshop._mirror_h  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._mirror_v  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._nearest_in_palette  [CHECK-CHAIN] 2 reference(s) - verify caller is itself live before removing
# MapWorkshop._nearest_zx_colour  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._on_bg_changed  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._on_canvas_changed  [VERIFIED-DEAD] traced this session - zero live callers
# MapWorkshop._on_fg_changed  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._on_image_palette_color  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._on_menu_btn_clicked  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._on_ts_font_size_changed  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._on_ts_size_changed  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._on_ts_strength_changed  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._on_user_palette_color  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._open_char_editor  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._open_dp5_colour_adjust  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._open_dp5_seamless  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._open_dp5_snow  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._open_icon_browser  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._open_icon_editor  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._open_ribbon_manager  [VERIFIED-LIVE] traced actual caller this session
# MapWorkshop._open_sprite_editor  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._open_zoom_lens  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._paint_variant_shape  [VERIFIED-LIVE] traced actual caller this session
# MapWorkshop._paste_selection  [VERIFIED-LIVE] traced actual caller this session
# MapWorkshop._pick_sticker  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._pick_ts_font  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._pil_transform  [LIKELY-LIVE] 7 references - probably live, not individually confirmed
# MapWorkshop._place_text_at  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._push_color_history  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._push_undo  [LIKELY-LIVE] 20 references - probably live, not individually confirmed
# MapWorkshop._rebuild_right_panel  [CHECK-CHAIN] 2 reference(s) - verify caller is itself live before removing
# MapWorkshop._rebuild_tool_ribbons  [LIKELY-LIVE] 6 references - probably live, not individually confirmed
# MapWorkshop._redo_canvas  [CHECK-CHAIN] 2 reference(s) - verify caller is itself live before removing
# MapWorkshop._refresh_canvas_tabs_ribbon  [CHECK-CHAIN] 2 reference(s) - verify caller is itself live before removing
# MapWorkshop._refresh_corner_overlay  [CHECK-CHAIN] 2 reference(s) - verify caller is itself live before removing
# MapWorkshop._refresh_icons  [CHECK-CHAIN] 2 reference(s) - verify caller is itself live before removing
# MapWorkshop._refresh_tool_settings_ribbon  [CHECK-CHAIN] 2 reference(s) - verify caller is itself live before removing
# MapWorkshop._render_as_ansi  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._render_as_ascii  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._render_as_petscii  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._render_as_teletext  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._render_variant_icon  [VERIFIED-LIVE] traced actual caller this session
# MapWorkshop._restore_canvas_tab_state  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._restore_outer_layout  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._ribbon_context_menu  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._ribbon_presets_dir  [CHECK-CHAIN] 2 reference(s) - verify caller is itself live before removing
# MapWorkshop._rotate_180  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._rotate_90_ccw  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._rotate_90_cw  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._rotate_arbitrary  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._rotate_selection_dialog  [VERIFIED-LIVE] traced actual caller this session
# MapWorkshop._save_current_canvas_tab  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._scale_canvas  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._select_all  [CHECK-CHAIN] 2 reference(s) - verify caller is itself live before removing
# MapWorkshop._select_tool  [LIKELY-LIVE] 13 references - probably live, not individually confirmed
# MapWorkshop._set_brush_size  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._set_opacity  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._set_platform  [CHECK-CHAIN] 2 reference(s) - verify caller is itself live before removing
# MapWorkshop._set_polygon_sides  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._set_ribbon_assignment  [VERIFIED-LIVE] traced actual caller this session
# MapWorkshop._set_show_grid  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._set_snap_grid  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._set_status  [VERIFIED-LIVE] traced actual caller this session
# MapWorkshop._set_zoom  [LIKELY-LIVE] 8 references - probably live, not individually confirmed
# MapWorkshop._set_zoom_mode  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._setup_corner_overlay  [LIKELY-LIVE] 4 references - probably live, not individually confirmed
# MapWorkshop._show_active_zoom_sensitivity_menu  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._show_dropdown_menu  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._show_sticker_picker  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._show_tool_settings_menu  [CHECK-CHAIN] 2 reference(s) - verify caller is itself live before removing
# MapWorkshop._show_workshop_settings  [VERIFIED-LIVE] traced actual caller this session
# MapWorkshop._snap_canvas_to_user_palette  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._snap_canvas_to_user_palette_dither  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._snap_cell_to_palette  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._snap_image_to_platform_palette  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._snap_image_to_user_palette  [CHECK-CHAIN] 2 reference(s) - verify caller is itself live before removing
# MapWorkshop._switch_canvas_tab  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._sync_brush_thumb  [LIKELY-LIVE] 3 references - probably live, not individually confirmed
# MapWorkshop._toggle_active_zoom  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._toggle_brush_manager  [VERIFIED-DEAD] traced this session - zero live callers
# MapWorkshop._toggle_cell_grid  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._toggle_clash_visualiser  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._toggle_colour_constraints  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._toggle_dither_mode  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._toggle_dock_floating  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._toggle_maximize  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._toggle_menubar  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._toggle_onion_skin  [VERIFIED-LIVE] traced actual caller this session
# MapWorkshop._toggle_statusbar  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._toggle_symmetry_mode  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._undo_canvas  [LIKELY-LIVE] 3 references - probably live, not individually confirmed
# MapWorkshop._update_color_swatches  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._update_cursor  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._update_mode_buttons  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._update_status  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._update_transform_text_panel_visibility  [LIKELY-DEAD-DP5] zero self.-references found
# MapWorkshop._update_zoom_label  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop._zoom_mode_menu  [CHECK-CHAIN] 1 reference(s) - verify caller is itself live before removing
# MapWorkshop.closeEvent  [FRAMEWORK] Qt override / entry point, always live
# MapWorkshop.get_menu_title  [FRAMEWORK] Qt override / entry point, always live
# MapWorkshop.keyPressEvent  [FRAMEWORK] Qt override / entry point, always live
# MapWorkshop.mouseMoveEvent  [FRAMEWORK] Qt override / entry point, always live
# MapWorkshop.mousePressEvent  [FRAMEWORK] Qt override / entry point, always live
# MapWorkshop.mouseReleaseEvent  [FRAMEWORK] Qt override / entry point, always live
# MapWorkshop.paintEvent  [FRAMEWORK] Qt override / entry point, always live
# MapWorkshop.resizeEvent  [FRAMEWORK] Qt override / entry point, always live
# MapWorkshop.set_menu_orientation  [FRAMEWORK] Qt override / entry point, always live
# MapWorkshop.setup_ui  [FRAMEWORK] Qt override / entry point, always live
# MapWorkshop.showEvent  [FRAMEWORK] Qt override / entry point, always live
# MapWorkshop._9bit_to_rgb  [ALREADY REMOVED]
# MapWorkshop._add_canvas_tab  [ALREADY REMOVED]
# MapWorkshop._adjust  [ALREADY REMOVED]
# MapWorkshop._anim_add_frame  [ALREADY REMOVED]
# MapWorkshop._anim_del_frame  [ALREADY REMOVED]
# MapWorkshop._anim_dup_frame  [ALREADY REMOVED]
# MapWorkshop._anim_export_gif  [ALREADY REMOVED]
# MapWorkshop._anim_export_png_seq  [ALREADY REMOVED]
# MapWorkshop._anim_first  [ALREADY REMOVED]
# MapWorkshop._anim_highlight_thumb  [ALREADY REMOVED]
# MapWorkshop._anim_init_frames  [ALREADY REMOVED]
# MapWorkshop._anim_last  [ALREADY REMOVED]
# MapWorkshop._anim_load_frame  [ALREADY REMOVED]
# MapWorkshop._anim_next  [ALREADY REMOVED]
# MapWorkshop._anim_prev  [ALREADY REMOVED]
# MapWorkshop._anim_refresh_thumbs  [ALREADY REMOVED]
# MapWorkshop._anim_save_current_frame  [ALREADY REMOVED]
# MapWorkshop._anim_tick  [ALREADY REMOVED]
# MapWorkshop._anim_toggle_play  [ALREADY REMOVED]
# MapWorkshop._anim_update_label  [ALREADY REMOVED]
# MapWorkshop._apply_atari_st_constraint  [ALREADY REMOVED]
# MapWorkshop._apply_bit_depth  [ALREADY REMOVED]
# MapWorkshop._apply_cell_constraint  [ALREADY REMOVED]
# MapWorkshop._apply_generic_constraint  [ALREADY REMOVED]
# MapWorkshop._apply_ham_constraint  [ALREADY REMOVED]
# MapWorkshop._apply_msx_constraint  [ALREADY REMOVED]
# MapWorkshop._apply_palette0_alpha  [ALREADY REMOVED]
# MapWorkshop._apply_pending_constraint  [ALREADY REMOVED]
# MapWorkshop._apply_spectrum_clash  [ALREADY REMOVED]
# MapWorkshop._apply_zx8x_dither  [ALREADY REMOVED]
# MapWorkshop._background_to_alpha  [ALREADY REMOVED]
# MapWorkshop._background_to_alpha_dialog  [ALREADY REMOVED]
# MapWorkshop._batch_convert_icons  [ALREADY REMOVED]
# MapWorkshop._batch_convert_textures  [ALREADY REMOVED]
# MapWorkshop._blend_palette_colors  [ALREADY REMOVED]
# MapWorkshop._build_load_menu  [ALREADY REMOVED]
# MapWorkshop._canvas_to_256colour_indexed  [ALREADY REMOVED]
# MapWorkshop._center_view_on  [ALREADY REMOVED]
# MapWorkshop._clear_brush  [ALREADY REMOVED]
# MapWorkshop._clear_canvas  [ALREADY REMOVED]
# MapWorkshop._convert_canvas_to_platform  [ALREADY REMOVED]
# MapWorkshop._create_anim_strip  [ALREADY REMOVED]
# MapWorkshop._create_docked_bar  [ALREADY REMOVED]
# MapWorkshop._crop_to_selection  [ALREADY REMOVED]
# MapWorkshop._decode_amiga_info  [ALREADY REMOVED]
# MapWorkshop._decode_iff_ilbm  [ALREADY REMOVED]
# MapWorkshop._decode_newicon_im1  [ALREADY REMOVED]
# MapWorkshop._delete_bitmap  [ALREADY REMOVED]
# MapWorkshop._export_amiga_icon  [ALREADY REMOVED]
# MapWorkshop._export_art_studio  [ALREADY REMOVED]
# MapWorkshop._export_bitmap  [ALREADY REMOVED]
# MapWorkshop._export_c64mprg  [ALREADY REMOVED]
# MapWorkshop._export_c64prg  [ALREADY REMOVED]
# MapWorkshop._export_dds  [ALREADY REMOVED]
# MapWorkshop._export_icns  [ALREADY REMOVED]
# MapWorkshop._export_ico  [ALREADY REMOVED]
# MapWorkshop._export_iff  [ALREADY REMOVED]
# MapWorkshop._export_iff_ham  [ALREADY REMOVED]
# MapWorkshop._export_koala  [ALREADY REMOVED]
# MapWorkshop._export_msxcom  [ALREADY REMOVED]
# MapWorkshop._export_nex  [ALREADY REMOVED]
# MapWorkshop._export_nxi  [ALREADY REMOVED]
# MapWorkshop._export_pal  [ALREADY REMOVED]
# MapWorkshop._export_pcx  [ALREADY REMOVED]
# MapWorkshop._export_pi1  [ALREADY REMOVED]
# MapWorkshop._export_plus4prg  [ALREADY REMOVED]
# MapWorkshop._export_sc2  [ALREADY REMOVED]
# MapWorkshop._export_scr  [ALREADY REMOVED]
# MapWorkshop._export_svg_icon  [ALREADY REMOVED]
# MapWorkshop._export_tap  [ALREADY REMOVED]
# MapWorkshop._export_texture_bmp  [ALREADY REMOVED]
# MapWorkshop._export_texture_png  [ALREADY REMOVED]
# MapWorkshop._export_tga  [ALREADY REMOVED]
# MapWorkshop._export_vicprg  [ALREADY REMOVED]
# MapWorkshop._get_canvas_pil  [ALREADY REMOVED]
# MapWorkshop._iff_find_chunk  [ALREADY REMOVED]
# MapWorkshop._iff_unpack_body  [ALREADY REMOVED]
# MapWorkshop._import_amiga_info  [ALREADY REMOVED]
# MapWorkshop._import_art_studio  [ALREADY REMOVED]
# MapWorkshop._import_bitmap  [ALREADY REMOVED]
# MapWorkshop._import_bitmap_path  [ALREADY REMOVED]
# MapWorkshop._import_bitmap_snap_canvas_size  [ALREADY REMOVED]
# MapWorkshop._import_bitmap_snap_canvas_size_dither  [ALREADY REMOVED]
# MapWorkshop._import_bitmap_snap_dither  [ALREADY REMOVED]
# MapWorkshop._import_bitmap_snap_user_pal  [ALREADY REMOVED]
# MapWorkshop._import_dds  [ALREADY REMOVED]
# MapWorkshop._import_dropped_file  [ALREADY REMOVED]
# MapWorkshop._import_gif  [ALREADY REMOVED]
# MapWorkshop._import_icns  [ALREADY REMOVED]
# MapWorkshop._import_ico  [ALREADY REMOVED]
# MapWorkshop._import_iff  [ALREADY REMOVED]
# MapWorkshop._import_koala  [ALREADY REMOVED]
# MapWorkshop._import_nxi  [ALREADY REMOVED]
# MapWorkshop._import_pal  [ALREADY REMOVED]
# MapWorkshop._import_pcx  [ALREADY REMOVED]
# MapWorkshop._import_pi1  [ALREADY REMOVED]
# MapWorkshop._import_psd  [ALREADY REMOVED]
# MapWorkshop._import_sc2  [ALREADY REMOVED]
# MapWorkshop._import_scr  [ALREADY REMOVED]
# MapWorkshop._import_svg  [ALREADY REMOVED]
# MapWorkshop._import_tga  [ALREADY REMOVED]
# MapWorkshop._import_tiff  [ALREADY REMOVED]
# MapWorkshop._invert  [ALREADY REMOVED]
# MapWorkshop._is_importable_ext  [ALREADY REMOVED]
# MapWorkshop._load_btn_context_menu  [ALREADY REMOVED]
# MapWorkshop._load_rgba  [ALREADY REMOVED]
# MapWorkshop._make_checkerboard  [ALREADY REMOVED]
# MapWorkshop._new_btn_context_menu  [ALREADY REMOVED]
# MapWorkshop._on_bitmap_selected  [ALREADY REMOVED]
# MapWorkshop._on_brush_mgr_selected  [ALREADY REMOVED]
# MapWorkshop._on_splitter_moved  [ALREADY REMOVED]
# MapWorkshop._resize_canvas_dialog  [ALREADY REMOVED]
# MapWorkshop._rgb_to_9bit  [ALREADY REMOVED]
# MapWorkshop._set_canvas_mode  [ALREADY REMOVED]
# MapWorkshop._show_load_menu  [ALREADY REMOVED]
# MapWorkshop._show_load_menu_at  [ALREADY REMOVED]
# MapWorkshop._toggle_anim_strip  [ALREADY REMOVED]
# MapWorkshop._write_amiga_info  [ALREADY REMOVED]
# MapWorkshop._write_icns  [ALREADY REMOVED]
# MapWorkshop.dragEnterEvent  [ALREADY REMOVED]
# MapWorkshop.dragMoveEvent  [ALREADY REMOVED]
# MapWorkshop.dropEvent  [ALREADY REMOVED]

App_name = "Map Workshop"
App_build = "July 28 26 (Build 380) Vers 62"
DEBUG_STANDALONE = False

# - Tool IDs
TOOL_PENCIL        = 'pencil'
TOOL_ERASER        = 'eraser'
TOOL_FILL          = 'fill'
TOOL_SPRAY         = 'spray'
TOOL_LINE          = 'line'
TOOL_CURVE         = 'curve'
TOOL_RECT          = 'rect'
TOOL_FILLED_RECT   = 'filled_rect'
TOOL_CIRCLE        = 'circle'
TOOL_FILLED_CIRCLE = 'filled_circle'
TOOL_TRIANGLE      = 'triangle'
TOOL_FILLED_TRIANGLE = 'filled_triangle'
TOOL_POLYGON       = 'polygon'
TOOL_FILLED_POLYGON = 'filled_polygon'
TOOL_STAR          = 'star'
TOOL_FILLED_STAR   = 'filled_star'
TOOL_LASSO         = 'lasso'
TOOL_FILLED_LASSO  = 'filled_lasso'   # right-click fill toggle
TOOL_PICKER        = 'picker'
TOOL_SELECT        = 'select'
TOOL_SELECT_COPY   = 'select_copy'
TOOL_MOVE          = 'move'
TOOL_ZOOM          = 'zoom'
TOOL_TEXT          = 'text'
TOOL_STAMP         = 'stamp'          # stamp/paste brush from buffer
TOOL_CROP          = 'crop'           # crop canvas to selection
TOOL_RESIZE        = 'resize'         # resize canvas
TOOL_DITHER        = 'dither'         # dither brush
TOOL_SYMMETRY      = 'symmetry'       # symmetry/mirror drawing
TOOL_BLUR_BRUSH    = 'blur_brush'     # blur brush (gaussian soften under cursor)
TOOL_SMUDGE        = 'smudge'        # smudge/blend pixels under cursor
TOOL_LIGHTEN       = 'lighten'       # dodge — lighten pixels under cursor
TOOL_DARKEN        = 'darken'        # burn  — darken pixels under cursor
TOOL_ARROW         = 'arrow'         # annotation arrow (line + arrowhead)
TOOL_HIGHLIGHTER   = 'highlighter'   # semi-transparent marker, alpha-blends
TOOL_SHARPEN       = 'sharpen'       # unsharp-mask brush (opposite of blur)
TOOL_STICKER       = 'sticker'       # stamp an emoji glyph onto the canvas
TOOL_DOUBLE_ARROW  = 'double_arrow'  # arrowheads at both ends
TOOL_MARKER_RECT   = 'marker_rect'   # highlighter-style rectangle outline
TOOL_CULL_BOXES    = 'cull_boxes'    # map editor: toggle IPL cull zone box display
TOOL_MARKER_ELLIPSE= 'marker_ellipse'# highlighter-style ellipse outline
TOOL_NUMBER        = 'number'        # auto-incrementing numbered badge
TOOL_PIXELATE      = 'pixelate'      # mosaic/pixelation brush
TOOL_SPRAYCAN      = 'spraycan'      # heavy/messy spray, distinct from the finer Airbrush (TOOL_SPRAY)
TOOL_DOT           = 'dot'           # plain filled dot stamp, no digit (Number group variant)
TOOL_BULLET        = 'bullet'        # small fixed-size bullet-point marker (Number group variant)
TOOL_ALPHA_BRUSH   = 'alpha_brush'   # paint transparency directly - left erases to alpha 0, right restores to alpha 255

# - Try importing shared infrastructure
try:
    from apps.gui.tool_menu_mixin import ToolMenuMixin as _ToolMenuMixin
except Exception:
    class _ToolMenuMixin:
        pass

try:
    from apps.methods.imgfactory_svg_icons import SVGIconFactory
    ICONS_AVAILABLE = True
except ImportError:
    ICONS_AVAILABLE = False
    class SVGIconFactory:
        def __getattr__(self, name):  #vers 1
            return lambda *a, **k: QIcon()
        @staticmethod
        def clear_cache(): pass  #vers 1
        @staticmethod
        def settings_icon(size=20, color='#ffffff'): return QIcon()  #vers 1
        @staticmethod
        def properties_icon(size=20, color='#ffffff'): return QIcon()  #vers 1
        @staticmethod
        def minimize_icon(size=20, color='#ffffff'): return QIcon()  #vers 1
        @staticmethod
        def maximize_icon(size=20, color='#ffffff'): return QIcon()  #vers 1
        @staticmethod
        def close_icon(size=20, color='#ffffff'): return QIcon()  #vers 1
        @staticmethod
        def ai_icon(size=20, color='#ffffff'): return QIcon()  #vers 1

try:
    from apps.utils.app_settings_system import AppSettings, SettingsDialog
    APPSETTINGS_AVAILABLE = True
except ImportError:
    APPSETTINGS_AVAILABLE = False
    AppSettings   = None
    SettingsDialog = None


# - Tool icon renderer — Photoshop-style white silhouettes on dark tile
def _load_tool_icon(shape: str, size: int = 42, active: bool = False,
                    tile_bg: str = '', icon_col: str = '') -> QIcon:  #vers 4
    """
    Load tool icon — checks in order:
    1. apps/icons/{shape}.svg or .png  (shared icons folder)
    2. This module's own icons/ subfolder (Map_Editor/icons/) for local overrides
    3. _make_tool_icon SVG/QPainter renderer  (built-in fallback)
    """
    import os
    # 1. Shared apps/icons/ folder
    try:
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        icon = SVGIconFactory._load_from_file(shape, size, icon_col or None)
        if icon is not None:
            return icon
    except Exception:
        pass
    # 2. This module's own icons/ subfolder (__file__ already resolves to
    # Map_Editor/map_workshop.py, so this was already correct - just a
    # misleadingly-worded comment, not an actual wrong path)
    local_dir = os.path.join(os.path.dirname(__file__), 'icons')
    for ext in ('svg', 'png'):
        fpath = os.path.join(local_dir, f'{shape}.{ext}')
        if os.path.isfile(fpath):
            pix = QPixmap(fpath).scaled(
                size, size,
                Qt.AspectRatioMode.KeepAspectRatio,
                Qt.TransformationMode.SmoothTransformation)
            return QIcon(pix)
    # 3. Built-in renderer
    return _make_tool_icon(shape, size, active, tile_bg=tile_bg, icon_col=icon_col)



#  MapSettings — per-tool settings (JSON, separate from global AppSettings)
class MapSettings:
    """
    Lightweight JSON settings for DP5 Workshop.
    Stored at ~/.config/imgfactory/map_workshop.json
    Completely separate from the global AppSettings/theme system.
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
        # to feel consistent - Keith reported Top view's left/right
        # was inverted and other views' up/down was inverted; exposed
        # here as user-adjustable rather than a single hardcoded guess,
        # since without a real GPU to test against directly, giving
        # direct control is safer than a blind sign-flip.
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
    }

    def __init__(self): #vers 1
        cfg_dir = Path.home() / '.config' / 'imgfactory'
        cfg_dir.mkdir(parents=True, exist_ok=True)
        self._path = cfg_dir / 'map_workshop.json'
        self._data = dict(self.DEFAULTS)
        self._load()


    def _load(self): #vers 1
        try:
            if self._path.exists():
                loaded = json.loads(self._path.read_text())
                self._data.update({k: v for k, v in loaded.items()
                                   if k in self.DEFAULTS})
        except Exception:
            pass

    def save(self): #vers 1
        try:
            self._path.write_text(json.dumps(self._data, indent=2))
        except Exception:
            pass

    def get(self, key, default=None): #vers 1
        return self._data.get(key, default if default is not None
                              else self.DEFAULTS.get(key))

    def set(self, key, value): #vers 1
        if key in self.DEFAULTS:
            self._data[key] = value


class MapSettingsDialog(QDialog):
    """Settings dialog for Map Workshop — does NOT touch global AppSettings."""

    def __init__(self, map_settings: MapSettings, parent=None): #vers 4
        super().__init__(parent)
        self.s = map_settings
        self._workshop = parent   # MapWorkshop instance - gives access to _WIDGET_REGISTRY
        self.setWindowTitle(App_name + " - Settings")
        self.setMinimumWidth(380)
        self.setModal(True)

        root = QVBoxLayout(self)
        tabs = QTabWidget()

        # - Canvas tab
        canvas_tab = QWidget()
        cl = QFormLayout(canvas_tab)
        cl.setSpacing(8)

        self._w_spin = QSpinBox(); self._w_spin.setRange(8, 4096)
        self._w_spin.setValue(self.s.get('default_width'))
        cl.addRow("Default width:", self._w_spin)

        self._h_spin = QSpinBox(); self._h_spin.setRange(8, 4096)
        self._h_spin.setValue(self.s.get('default_height'))
        cl.addRow("Default height:", self._h_spin)

        self._zoom_spin = QSpinBox(); self._zoom_spin.setRange(1, 64)
        self._zoom_spin.setValue(self.s.get('default_zoom'))
        cl.addRow("Default zoom:", self._zoom_spin)

        self._undo_spin = QSpinBox(); self._undo_spin.setRange(4, 128)
        self._undo_spin.setValue(self.s.get('undo_levels'))
        cl.addRow("Undo levels:", self._undo_spin)

        tabs.addTab(canvas_tab, "Canvas")

        # - Ribbons tab (icon size / padding per orientation, opacity)
        ribbons_tab = QWidget()
        rl = QFormLayout(ribbons_tab)

        ribbon_mgr_btn = QPushButton("Ribbon Manager…")
        ribbon_mgr_btn.setToolTip("Move tools between ribbons, save/load layout presets")
        if self._workshop is not None:
            ribbon_mgr_btn.clicked.connect(self._workshop._open_ribbon_manager)
        rl.addRow(ribbon_mgr_btn)
        rl.addRow(QLabel(""))

        rl.addRow(QLabel("—  Vertical  (docked left/right)  —"))
        self._ribbon_icon_vert_spin = QSpinBox()
        self._ribbon_icon_vert_spin.setRange(12, 64)
        self._ribbon_icon_vert_spin.setValue(self.s.get('ribbon_icon_size_vert'))
        rl.addRow("Icon size:", self._ribbon_icon_vert_spin)
        self._ribbon_pad_vert_spin = QSpinBox()
        self._ribbon_pad_vert_spin.setRange(0, 20)
        self._ribbon_pad_vert_spin.setValue(self.s.get('ribbon_padding_vert'))
        self._ribbon_pad_vert_spin.setToolTip("Gap between buttons")
        rl.addRow("Button spacing:", self._ribbon_pad_vert_spin)
        self._ribbon_btn_pad_vert_spin = QSpinBox()
        self._ribbon_btn_pad_vert_spin.setRange(0, 20)
        self._ribbon_btn_pad_vert_spin.setValue(self.s.get('ribbon_button_padding_vert'))
        self._ribbon_btn_pad_vert_spin.setToolTip("Gap between the icon and the button's own edge")
        rl.addRow("Button edge padding:", self._ribbon_btn_pad_vert_spin)

        rl.addRow(QLabel("—  Horizontal  (docked top/bottom)  —"))
        self._ribbon_icon_horz_spin = QSpinBox()
        self._ribbon_icon_horz_spin.setRange(12, 64)
        self._ribbon_icon_horz_spin.setValue(self.s.get('ribbon_icon_size_horz'))
        rl.addRow("Icon size:", self._ribbon_icon_horz_spin)
        self._ribbon_pad_horz_spin = QSpinBox()
        self._ribbon_pad_horz_spin.setRange(0, 20)
        self._ribbon_pad_horz_spin.setValue(self.s.get('ribbon_padding_horz'))
        self._ribbon_pad_horz_spin.setToolTip("Gap between buttons")
        rl.addRow("Button spacing:", self._ribbon_pad_horz_spin)
        self._ribbon_btn_pad_horz_spin = QSpinBox()
        self._ribbon_btn_pad_horz_spin.setRange(0, 20)
        self._ribbon_btn_pad_horz_spin.setValue(self.s.get('ribbon_button_padding_horz'))
        self._ribbon_btn_pad_horz_spin.setToolTip("Gap between the icon and the button's own edge")
        rl.addRow("Button edge padding:", self._ribbon_btn_pad_horz_spin)

        rl.addRow(QLabel("—  Appearance  —"))
        self._ribbon_opacity_spin = QSpinBox()
        self._ribbon_opacity_spin.setRange(10, 100)
        self._ribbon_opacity_spin.setSuffix(" %")
        self._ribbon_opacity_spin.setValue(self.s.get('ribbon_opacity'))
        self._ribbon_opacity_spin.setToolTip("Ribbon background translucency - "
                                             "lower = more see-through")
        rl.addRow("Opacity:", self._ribbon_opacity_spin)

        tabs.addTab(ribbons_tab, "Ribbons")

        # - Interface tab
        ui_tab = QWidget()
        ul = QFormLayout(ui_tab)
        ul.setSpacing(8)

        self._bitmap_chk = QCheckBox()
        self._bitmap_chk.setChecked(self.s.get('show_bitmap_list'))
        ul.addRow("Show bitmap list panel:", self._bitmap_chk)

        self._statusbar_chk = QCheckBox()
        self._statusbar_chk.setChecked(self.s.get('show_statusbar'))
        ul.addRow("Show status bar:", self._statusbar_chk)

        self._font_size_spin = QSpinBox(); self._font_size_spin.setRange(7, 18)
        self._font_size_spin.setValue(self.s.get('ui_font_size'))
        ul.addRow("UI font size:", self._font_size_spin)

        self._icon_size_spin = QSpinBox(); self._icon_size_spin.setRange(16, 64)
        self._icon_size_spin.setValue(self.s.get('tool_icon_size'))
        ul.addRow("Tool icon size (px):", self._icon_size_spin)

        self._icon_color_combo = QComboBox()
        self._icon_color_combo.addItems(['color', 'white', 'dark'])
        idx = {'color': 0, 'white': 1, 'dark': 2}.get(
            self.s.get('tool_icon_color'), 0)
        self._icon_color_combo.setCurrentIndex(idx)
        ul.addRow("Tool icon colour:", self._icon_color_combo)

        self._cols_combo = QComboBox()
        self._cols_combo.addItems(['3 columns', '4 columns', '5 columns', '6 columns'])
        col_idx = {3: 0, 4: 1, 5: 2, 6: 3}.get(self.s.get('tool_columns'), 0)
        self._cols_combo.setCurrentIndex(col_idx)
        ul.addRow("Gadget columns:", self._cols_combo)

        tabs.addTab(ui_tab, "Interface")

        # - Widgets tab (enable/disable each dock) - generated from
        # MapWorkshop._WIDGET_REGISTRY rather than one hardcoded checkbox
        # per widget, so adding a new widget to the registry (e.g. an
        # alternative colour-widget implementation) automatically gets a
        # toggle here too, with no dialog code changes needed.
        widgets_tab = QWidget()
        wl = QFormLayout(widgets_tab)
        wl.setSpacing(8)

        self._widget_chks = {}   # key -> QCheckBox, for generic save logic
        registry = getattr(self._workshop, '_WIDGET_REGISTRY', [])
        for entry in registry:
            # Entries with enabled_setting=None (Bitmaps) have their
            # visibility combined with another setting elsewhere, but
            # still get a checkbox here using the conventional settings
            # key, which already exists.
            setting_key = entry.get('enabled_setting') or f"widget_{entry['key']}_enabled"
            chk = QCheckBox()
            chk.setChecked(self.s.get(setting_key))
            wl.addRow(f"{entry.get('label', entry['key'])}:", chk)
            self._widget_chks[entry['key']] = chk

        tabs.addTab(widgets_tab, "Widgets")

        # - Gadgets tab
        gadgets_tab = QWidget()
        gl = QVBoxLayout(gadgets_tab)
        gl.setSpacing(4)
        gl.addWidget(QLabel("Click to toggle tool visibility (highlighted = visible):"))
        hidden = self.s.get('hidden_tools') or []
        icon_sz = self.s.get('tool_icon_size')
        btn_sz  = max(26, icon_sz + 2)  # min 26px so labels stay readable
        self._gadget_chks = {}
        TOOL_LABELS = [
            ('pencil','Pencil'), ('eraser','Eraser'), ('fill','Fill'),
            ('spray','Spray'), ('picker','Picker'), ('curve','Curve'),
            ('line','Line'), ('rect','Rectangle'), ('circle','Circle'),
            ('triangle','Triangle'), ('polygon','Polygon'), ('star','Star'),
            ('select','Select'), ('lasso','Lasso'), ('zoom','Zoom'),
            ('text','Text'), ('crop','Crop'), ('resize','Resize'),
            ('dither','Dither'), ('symmetry','Symmetry'),
        ]
        grid_w = QWidget()
        grid_l = QGridLayout(grid_w)
        grid_l.setSpacing(4)
        grid_l.setContentsMargins(0, 0, 0, 0)
        cols = 4
        for idx, (tool_id, label) in enumerate(TOOL_LABELS):
            btn = QPushButton()
            btn.setCheckable(True)
            btn.setChecked(tool_id not in hidden)
            btn.setFixedSize(btn_sz, btn_sz + 14)
            btn.setToolTip(label)
            # Use parent workshop's icon colour if available
            _ws  = parent if hasattr(parent, '_get_icon_color') else None
            _col = _ws._get_icon_color() if _ws else ''
            _tbg = ''
            if _ws and _ws.app_settings:
                _tc = _ws.app_settings.get_theme_colors() or {}
                _tbg = _tc.get('gadgetbar_bg', _tc.get('toolbar_bg', ''))
            ico = _load_tool_icon(tool_id, icon_sz, tile_bg=_tbg, icon_col=_col)
            btn.setIcon(ico)
            btn.setIconSize(QSize(icon_sz, icon_sz))
            lbl_short = label[:6]
            btn.setText(lbl_short)
            # Theme-aware stylesheet — no hardcoded colours
            acc = '#4a8a4a'
            if _ws and _ws.app_settings:
                _tc2 = _ws.app_settings.get_theme_colors() or {}
                acc  = _tc2.get('accent_primary', acc)
            btn.setStyleSheet(
                f"QPushButton {{ font-size: 8px; color: palette(mid); "
                f"background: palette(base); border: 1px solid palette(mid); "
                f"padding-top: 2px; }} "
                f"QPushButton:checked {{ background: {acc}; "
                f"border: 1px solid palette(highlight); }}"
            )
            btn.setLayoutDirection(Qt.LayoutDirection.LeftToRight)
            grid_l.addWidget(btn, idx // cols, idx % cols)
            self._gadget_chks[tool_id] = btn
        gl.addWidget(grid_w)
        gl.addStretch()

        # - Menu tab  
        menu_tab = QWidget()
        ml = QFormLayout(menu_tab)
        ml.setSpacing(8)

        self._menu_style_combo = QComboBox()
        self._menu_style_combo.addItems(['topbar', 'dropdown'])
        self._menu_style_combo.setCurrentText(self.s.get('menu_style'))
        ml.addRow("Menu orientation:", self._menu_style_combo)

        self._menu_bar_height_spin = QSpinBox()
        self._menu_bar_height_spin.setRange(16, 40)
        self._menu_bar_height_spin.setValue(self.s.get('menu_bar_height'))
        self._menu_bar_height_spin.setSuffix(" px")
        ml.addRow("Topbar height:", self._menu_bar_height_spin)

        self._menu_bar_font_spin = QSpinBox()
        self._menu_bar_font_spin.setRange(7, 16)
        self._menu_bar_font_spin.setValue(self.s.get('menu_bar_font_size'))
        self._menu_bar_font_spin.setSuffix(" pt")
        ml.addRow("Topbar font size:", self._menu_bar_font_spin)

        self._menu_dropdown_font_spin = QSpinBox()
        self._menu_dropdown_font_spin.setRange(7, 16)
        self._menu_dropdown_font_spin.setValue(self.s.get('menu_dropdown_font_size'))
        self._menu_dropdown_font_spin.setSuffix(" pt")
        ml.addRow("Dropdown font size:", self._menu_dropdown_font_spin)

        tabs.addTab(menu_tab, "Menu")

        tabs.addTab(gadgets_tab, "Gadgets")

        # - Viewport tab (World View movement settings)
        viewport_tab = QWidget()
        vp_lay = QVBoxLayout(viewport_tab)
        vp_form = QFormLayout()
        vp_form.setSpacing(8)

        self._pan_button_combo = QComboBox()
        self._pan_button_combo.addItems(["left", "middle", "right"])
        self._pan_button_combo.setCurrentText(self.s.get('viewport_pan_button'))
        vp_form.addRow("Pan button:", self._pan_button_combo)

        self._rotate_button_combo = QComboBox()
        self._rotate_button_combo.addItems(["left", "middle", "right"])
        self._rotate_button_combo.setCurrentText(self.s.get('viewport_rotate_button'))
        vp_form.addRow("Rotate button (3D only):", self._rotate_button_combo)
        vp_lay.addLayout(vp_form)

        vp_lay.addWidget(QLabel(
            "Invert pan direction per viewport - Top/Side/Front/3D each\n"
            "have a different camera orientation, so correct this per\n"
            "mode if panning feels backwards in one but not another."))

        self._pan_invert_checks = {}
        invert_cfg = self.s.get('viewport_pan_invert') or {}
        for mode in ("Top", "Side", "Front", "3D"):
            box = QGroupBox(mode)
            box_lay = QHBoxLayout(box)
            axis_cfg = invert_cfg.get(mode, {'x': False, 'y': False})
            chk_x = QCheckBox("Invert X"); chk_x.setChecked(axis_cfg.get('x', False))
            chk_y = QCheckBox("Invert Y"); chk_y.setChecked(axis_cfg.get('y', False))
            box_lay.addWidget(chk_x); box_lay.addWidget(chk_y)
            vp_lay.addWidget(box)
            self._pan_invert_checks[mode] = (chk_x, chk_y)
        vp_lay.addStretch()
        tabs.addTab(viewport_tab, "Viewport")

        root.addWidget(tabs)

        # OK / Cancel
        btns = QHBoxLayout()
        btns.addStretch()
        ok_btn = QPushButton("OK")
        ok_btn.setDefault(True)
        ok_btn.clicked.connect(self._accept)
        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(self.reject)
        btns.addWidget(ok_btn); btns.addWidget(cancel_btn)
        root.addLayout(btns)

    def _accept(self): #vers 1
        self.s.set('default_width',    self._w_spin.value())
        self.s.set('default_height',   self._h_spin.value())
        self.s.set('default_zoom',     self._zoom_spin.value())
        self.s.set('undo_levels',      self._undo_spin.value())
        self.s.set('show_pixel_grid',  self._grid_chk.isChecked())
        self.s.set('zoom_to_fit_resize', self._fit_resize_chk.isChecked())
        self.s.set('menu_style',              self._menu_style_combo.currentText())
        self.s.set('menu_bar_height',         self._menu_bar_height_spin.value())
        self.s.set('menu_bar_font_size',      self._menu_bar_font_spin.value())
        self.s.set('menu_dropdown_font_size', self._menu_dropdown_font_spin.value())
        self.s.set('img_pal_cols',       self._img_pal_cols_spin.value())
        self.s.set('img_pal_rows',       self._img_pal_rows_spin.value())
        self.s.set('user_pal_cols',      self._user_pal_cols_spin.value())
        self.s.set('user_pal_rows',      self._user_pal_rows_spin.value())
        self.s.set('platform_mode',      self._platform_combo.currentText())
        self.s.set('show_cell_grid',     self._cell_grid_chk.isChecked())
        self.s.set('grid_color',         self._grid_color_btn._chosen)
        self.s.set('marching_ants_enabled', self._ants_chk.isChecked())
        self.s.set('marching_ants_style',   self._ants_style_combo.currentText())
        self.s.set('marching_ants_fg',      self._ants_fg_btn._chosen)
        self.s.set('marching_ants_bg',      self._ants_bg_btn._chosen)
        self.s.set('marching_ants_speed',   self._ants_speed_spin.value())
        self.s.set('ribbon_icon_size_vert', self._ribbon_icon_vert_spin.value())
        self.s.set('ribbon_padding_vert',   self._ribbon_pad_vert_spin.value())
        self.s.set('ribbon_icon_size_horz', self._ribbon_icon_horz_spin.value())
        self.s.set('ribbon_padding_horz',   self._ribbon_pad_horz_spin.value())
        self.s.set('ribbon_button_padding_vert', self._ribbon_btn_pad_vert_spin.value())
        self.s.set('ribbon_button_padding_horz', self._ribbon_btn_pad_horz_spin.value())
        self.s.set('ribbon_opacity',        self._ribbon_opacity_spin.value())
        self.s.set('show_bitmap_list', self._bitmap_chk.isChecked())
        registry = getattr(self._workshop, '_WIDGET_REGISTRY', [])
        for entry in registry:
            setting_key = entry.get('enabled_setting') or f"widget_{entry['key']}_enabled"
            chk = self._widget_chks.get(entry['key'])
            if chk is not None:
                self.s.set(setting_key, chk.isChecked())
        self.s.set('show_statusbar',   self._statusbar_chk.isChecked())
        self.s.set('ui_font_size',     self._font_size_spin.value())
        self.s.set('tool_icon_size',   self._icon_size_spin.value())
        self.s.set('tool_icon_color',  self._icon_color_combo.currentText())
        self.s.set('tool_columns',     [3, 4, 5, 6][self._cols_combo.currentIndex()])
        hidden = [tid for tid, chk in self._gadget_chks.items() if not chk.isChecked()]
        self.s.set('hidden_tools',     hidden)

        self.s.set('viewport_pan_button',    self._pan_button_combo.currentText())
        self.s.set('viewport_rotate_button', self._rotate_button_combo.currentText())
        invert_cfg = {}
        for mode, (chk_x, chk_y) in self._pan_invert_checks.items():
            invert_cfg[mode] = {'x': chk_x.isChecked(), 'y': chk_y.isChecked()}
        self.s.set('viewport_pan_invert', invert_cfg)

        self.s.save()
        # Re-apply immediately to any already-open World View panes, so
        # the change takes effect without needing to reopen/restart.
        if self._workshop is not None:
            for pane in getattr(self._workshop, '_world_panes', []):
                self._workshop._apply_viewport_movement_settings(pane, pane._view_label)
        self.accept()



class _CornerOverlay(QWidget):
    """Transparent overlay that draws corner resize triangles on top of all children.
    Uses setMask() so only the triangle pixels exist — fully transparent elsewhere.
    WA_AlwaysStackOnTop keeps it above all sibling widgets on Wayland/KDE."""

    SIZE = 20   # triangle leg size in pixels

    def __init__(self, parent): #vers 3
        super().__init__(parent)
        self.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents, True)
        self.setAttribute(Qt.WidgetAttribute.WA_NoSystemBackground, True)
        self.setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground, True)
        self.setAttribute(Qt.WidgetAttribute.WA_AlwaysStackOnTop, True)
        self.setWindowFlags(Qt.WindowType.Widget)
        self._hover_corner = None
        self._app_settings = None
        self.setGeometry(0, 0, parent.width(), parent.height())
        self._update_mask()

    def _update_mask(self): #vers 1
        """Create a mask covering only the four corner triangles."""
        from PyQt6.QtGui import QRegion, QPolygon
        from PyQt6.QtCore import QPoint
        s = self.SIZE
        w, h = self.width(), self.height()
        region = QRegion()
        for pts in [
            [QPoint(0,0),    QPoint(s,0),    QPoint(0,s)],     # top-left
            [QPoint(w,0),    QPoint(w-s,0),  QPoint(w,s)],     # top-right
            [QPoint(0,h),    QPoint(s,h),    QPoint(0,h-s)],   # bottom-left
            [QPoint(w,h),    QPoint(w-s,h),  QPoint(w,h-s)],   # bottom-right
        ]:
            region = region.united(QRegion(QPolygon(pts)))
        self.setMask(region)

    def update_state(self, hover_corner, app_settings): #vers 1
        self._hover_corner = hover_corner
        self._app_settings = app_settings
        self.update()

    def setGeometry(self, *args): #vers 1
        super().setGeometry(*args)
        self._update_mask()

    def resizeEvent(self, event): #vers 1
        super().resizeEvent(event)
        self._update_mask()

    def paintEvent(self, event): #vers 2
        s = self.SIZE
        _p = self.palette()
        _accent_fallback = _p.color(_p.ColorRole.Highlight)
        if self._app_settings:
            try:
                colors = self._app_settings.get_theme_colors()
                accent = QColor(colors.get('accent_primary', _accent_fallback.name()))
            except Exception:
                accent = _accent_fallback
        else:
            accent = _accent_fallback
        accent.setAlpha(200)
        hover_c = QColor(accent); hover_c.setAlpha(255)
        w, h = self.width(), self.height()
        corners = {
            'top-left':     [(0,0),  (s,0),   (0,s)],
            'top-right':    [(w,0),  (w-s,0), (w,s)],
            'bottom-left':  [(0,h),  (s,h),   (0,h-s)],
            'bottom-right': [(w,h),  (w-s,h), (w,h-s)],
        }
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        for name, pts in corners.items():
            path = QPainterPath()
            path.moveTo(*pts[0]); path.lineTo(*pts[1]); path.lineTo(*pts[2])
            path.closeSubpath()
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(QBrush(hover_c if self._hover_corner == name else accent))
            painter.drawPath(path)
        painter.end()


class _InstanceEditPanel(QWidget):
    """Non-modal, persistent object info/edit panel - shown in the top-
    left corner (per Keith's request for a 'pop-out dialog or embedded
    window' rather than a blocking modal dialog). Shows Identity/IDE/
    IPL/2DFX/TOBJ info like the earlier modal version, plus live
    position and rotation nudge controls (small/large step buttons
    either side of a directly-editable spinbox per axis) that actually
    modify the underlying IPLInstance in memory and refresh the World
    View/Instance List to match.

    Rotation is stored on IPLInstance as a quaternion but edited here
    as X/Y/Z degrees (quat_to_euler_degrees/euler_degrees_to_quat) -
    standard, round-trip-verified conversion, not GTA-format-specific
    guessing."""

    _POS_SMALL_STEP = 0.5
    _POS_LARGE_STEP = 10.0
    _ROT_SMALL_STEP = 1.0
    _ROT_LARGE_STEP = 15.0

    def __init__(self, workshop, parent=None): #vers 2
        super().__init__(parent, Qt.WindowType.Tool)
        self._workshop = workshop
        self._inst = None
        self._loader = None
        self._nudge_wide = None   # current reflow state, None forces first layout
        self.setWindowTitle("Object Info")
        self.setMinimumWidth(180)

        self._lay = QVBoxLayout(self)
        self._identity_box = self._add_section("Identity")
        self._nav_row = QHBoxLayout()
        self._nav_label = QLabel("")
        self._nav_prev_btn = QPushButton("< Prev")
        self._nav_next_btn = QPushButton("Next >")
        self._nav_prev_btn.clicked.connect(lambda: self._workshop._cycle_model_instance(-1))
        self._nav_next_btn.clicked.connect(lambda: self._workshop._cycle_model_instance(1))
        self._nav_row.addWidget(self._nav_prev_btn)
        self._nav_row.addWidget(self._nav_label)
        self._nav_row.addWidget(self._nav_next_btn)
        self._lay.addLayout(self._nav_row)
        self._set_nav_visible(False)
        self._ide_box = self._add_section("IDE Info")
        self._pos_box, self._pos_spins, self._pos_grid, self._pos_rows = \
            self._add_nudge_section("Position", self._POS_SMALL_STEP,
                                    self._POS_LARGE_STEP, self._on_position_nudged)
        self._rot_box, self._rot_spins, self._rot_grid, self._rot_rows = \
            self._add_nudge_section("Rotation (degrees)", self._ROT_SMALL_STEP,
                                    self._ROT_LARGE_STEP, self._on_rotation_nudged)
        self._meta_box = self._add_section("Placement Info")
        self._2dfx_box = self._add_section("2DFX Effects")
        self._tobj_box = self._add_section("TOBJ (Timed Object) Variants")

        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.hide)
        self._lay.addWidget(close_btn)

        self._reflow_nudge_rows(wide=True)

    def resizeEvent(self, event): #vers 1
        """Wrap the nudge rows' arrow buttons onto a second line when
        the panel is too narrow for everything on one row, rather than
        clipping/squeezing them (per Keith's request - 'wrapping is
        needed depending on the popup dialog width')."""
        super().resizeEvent(event)
        wide = self.width() >= 260
        if wide != self._nudge_wide:
            self._reflow_nudge_rows(wide)

    def _reflow_nudge_rows(self, wide): #vers 1
        """Rebuild both nudge sections' grids for the given width mode -
        wide: label, <<, <, value, >, >> all in one row per axis.
        narrow: label+value on one row, the 4 arrow buttons on the row
        beneath, per axis."""
        for grid, rows_info in ((self._pos_grid, self._pos_rows),
                                (self._rot_grid, self._rot_rows)):
            # Clear all positions (widgets stay alive, just get re-added)
            while grid.count():
                grid.takeAt(0)
            for i, (label, btn_ll, btn_l, spin, btn_r, btn_rr) in enumerate(rows_info):
                if wide:
                    r = i
                    grid.addWidget(label, r, 0)
                    grid.addWidget(btn_ll, r, 1)
                    grid.addWidget(btn_l, r, 2)
                    grid.addWidget(spin, r, 3)
                    grid.addWidget(btn_r, r, 4)
                    grid.addWidget(btn_rr, r, 5)
                else:
                    r = i * 2
                    grid.addWidget(label, r, 0)
                    grid.addWidget(spin, r, 1, 1, 4)
                    grid.addWidget(btn_ll, r + 1, 0)
                    grid.addWidget(btn_l, r + 1, 1)
                    grid.addWidget(btn_r, r + 1, 2)
                    grid.addWidget(btn_rr, r + 1, 3)
        self._nudge_wide = wide

    def _add_section(self, title): #vers 1
        box = QGroupBox(title)
        QVBoxLayout(box)
        self._lay.addWidget(box)
        return box

    def _set_section_lines(self, box, lines): #vers 1
        lay = box.layout()
        while lay.count():
            item = lay.takeAt(0)
            w = item.widget()
            if w: w.deleteLater()
        if lines:
            for line in lines:
                lay.addWidget(QLabel(line))
        else:
            empty = QLabel("(none)")
            empty.setStyleSheet("color: palette(mid);")
            lay.addWidget(empty)

    def _add_nudge_section(self, title, small_step, large_step, on_nudge): #vers 2
        """One label + << < [value] > >> row per axis (X/Y/Z), using
        real chevron icons rather than text, laid out in a QGridLayout
        so _reflow_nudge_rows can wrap the arrow buttons onto a second
        row when the panel is too narrow to fit everything on one
        line (rather than clipping/squeezing, which a plain QHBoxLayout
        would do with no wrapping at all)."""
        box = QGroupBox(title)
        grid = QGridLayout(box)
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
            btn_ll = QPushButton(); btn_ll.setIcon(icons['ll']); btn_ll.setFixedWidth(28)
            btn_l  = QPushButton(); btn_l.setIcon(icons['l']);   btn_l.setFixedWidth(22)
            spin = QDoubleSpinBox()
            spin.setRange(-100000.0, 100000.0)
            spin.setDecimals(2)
            spin.setFixedWidth(80)
            btn_r  = QPushButton(); btn_r.setIcon(icons['r']);   btn_r.setFixedWidth(22)
            btn_rr = QPushButton(); btn_rr.setIcon(icons['rr']); btn_rr.setFixedWidth(28)
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

    def show_for_instance(self, inst, loader, nav_info=None, model_cache=None): #vers 3
        """Refresh every section for a (possibly new) instance - called
        both when first opening the panel and whenever the selection
        changes (Instance List, or the merged Object Browser), so the
        same panel stays open and up to date rather than needing to be
        reopened. nav_info, if given, is (current_index, total_count)
        for a model with multiple placements - shows Prev/Next
        cycling, per Keith's request that clicking a merged Object
        Browser row (one row per model, not per placement) still be
        able to reach every instance of that model. model_cache, if
        given, is used to show the model's real width/height (from its
        actual loaded geometry, per Keith's request) alongside its
        name - blank if that model's geometry isn't loaded yet, not an
        error, matching the same lazy-loading fallback used elsewhere."""
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

        size_suffix = ""
        if model_cache is not None:
            dims = model_cache.get_dimensions(inst.model_name)
            if dims is not None:
                width, depth, height = dims
                size_suffix = f"  ({width:.1f} × {height:.1f})"

        self.setWindowTitle(f"Object Info - {inst.model_name} (ID {inst.model_id})")
        self._set_section_lines(self._identity_box, [
            f"ID: {inst.model_id}",
            f"Name: {inst.model_name}{size_suffix}",
            f"Texture (TXD): {obj.txd_name if obj else '(unresolved - no IDE match)'}",
        ])
        if obj:
            ide_lines = [f"Type: {obj.obj_type}   Section: {obj.section}",
                        f"Source: {obj.source_ide}  (line {obj.line_no})"]
            ide_lines += [f"{k}: {v}" for k, v in obj.extra.items()]
            self._set_section_lines(self._ide_box, ide_lines)
        else:
            self._set_section_lines(self._ide_box, None)

        self._refresh_position_spins()
        self._refresh_rotation_spins()

        self._set_section_lines(self._meta_box, [
            f"Interior: {inst.interior}   LOD index: {inst.lod_index}",
            f"Source IPL: {inst.source_ipl}  (line {inst.line_no})",
        ])
        self._set_section_lines(self._2dfx_box, [
            f"#{i+1}: {e.obj_type} (line {e.line_no}, {e.source_ide})"
            for i, e in enumerate(effects)] or None)
        self._set_section_lines(self._tobj_box, [
            f"{t.model_name} (ID {t.model_id}, {t.source_ide} line {t.line_no})"
            for t in tobjs] or None)

    def _refresh_position_spins(self): #vers 1
        inst = self._inst
        for axis, spin in self._pos_spins.items():
            spin.blockSignals(True)
            spin.setValue(getattr(inst, f"pos_{axis}"))
            spin.blockSignals(False)

    def _refresh_rotation_spins(self): #vers 1
        inst = self._inst
        roll, pitch, yaw = quat_to_euler_degrees(
            inst.rot_x, inst.rot_y, inst.rot_z, inst.rot_w)
        for axis, val in zip(('x', 'y', 'z'), (roll, pitch, yaw)):
            spin = self._rot_spins[axis]
            spin.blockSignals(True)
            spin.setValue(val)
            spin.blockSignals(False)

    def _on_position_nudged(self, axis, delta, absolute=None): #vers 1
        if self._inst is None:
            return
        attr = f"pos_{axis}"
        new_val = absolute if absolute is not None else getattr(self._inst, attr) + delta
        setattr(self._inst, attr, new_val)
        self._refresh_position_spins()
        self._workshop._on_instance_edited(self._inst)

    def _on_rotation_nudged(self, axis, delta, absolute=None): #vers 1
        if self._inst is None:
            return
        roll, pitch, yaw = quat_to_euler_degrees(
            self._inst.rot_x, self._inst.rot_y, self._inst.rot_z, self._inst.rot_w)
        current = {'x': roll, 'y': pitch, 'z': yaw}
        current[axis] = absolute if absolute is not None else current[axis] + delta
        x, y, z, w = euler_degrees_to_quat(current['x'], current['y'], current['z'])
        self._inst.rot_x, self._inst.rot_y, self._inst.rot_z, self._inst.rot_w = x, y, z, w
        self._refresh_rotation_spins()
        self._workshop._on_instance_edited(self._inst)


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

    def __init__(self, instances, loader, parent=None): #vers 2
        super().__init__(parent)
        self._instances = instances
        self._loader = loader
        self._txd_cache = {}   # model_id -> resolved TXD name, filled lazily

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

    def _resolve_txd(self, inst): #vers 1
        cached = self._txd_cache.get(inst.model_id)
        if cached is not None:
            return cached
        obj = self._loader.get_object(inst.model_id)
        name = obj.txd_name if obj else ""
        self._txd_cache[inst.model_id] = name
        return name

    def data(self, index, role=Qt.ItemDataRole.DisplayRole): #vers 2
        if role != Qt.ItemDataRole.DisplayRole or not index.isValid():
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


class MapWorkshop(_ToolMenuMixin, QWidget):
    """Deluxe Paint 5 inspired bitmap editor — standalone + embeddable."""

    workshop_closed = pyqtSignal()
    window_closed   = pyqtSignal()

    # Bump whenever the set of ribbons/dock widgets changes (added/
    # removed/renamed) so a saved layout from an older structure is
    # cleanly rejected instead of Qt silently failing to restore it.
    # History: 1 = Tools/Image Ops ribbons + Bitmaps/Brush & Colors/
    # Image Palette/User Palette dock widgets (initial ribbon rebuild).
    # 2 = Control Panel dock added, IPL Inst File dock added, standalone
    # Editing Panel dock added then merged into Object Browser (IDE/
    # IPL/DAT/IMG tabs) - none of these bumped the version at the time,
    # so a layout saved under any of those structures could pass this
    # check but restore into a mismatched/broken arrangement, since the
    # actual set of docks has changed underneath it. Bumped now so any
    # such stale saved layout is cleanly rejected instead, falling back
    # to the default arrangement (which correctly shows every dock).
    _OUTER_LAYOUT_VERSION = 3

    # Shared compact sizing for Object Browser and all four tabs merged
    # into it (IDE/IPL/DAT/IMG) - per Keith's report that the new tabs'
    # buttons/header cells were taller than Object Browser's own,
    # everything should use these same values.
    _COMPACT_BUTTON_H = 18
    _COMPACT_ICON_SIZE = 18

    # Widget registry - each entry describes one dock widget module under
    # depends/. setup_ui() loops over this instead of hardcoding each
    # dock's creation, so adding a new widget, removing one, or swapping
    # one for an alternative implementation only means editing this list.
    # 'enabled_setting': the map_settings key controlling visibility, or
    # None if visibility is handled separately (Bitmaps combines its
    # enabled_setting with the older show_bitmap_list preference right
    # after this loop runs, rather than a single setting here).
    _WIDGET_REGISTRY = [
        # Empty for now - the four paint-specific docks that used to be
        # here (Bitmaps, Brush & Colors, Image Palette, User Palette)
        # don't apply to a map editor. Map-specific docks (object list,
        # favourites, category tree) will be added here as they're built.
    ]

    #    Init                                                                   

    def __init__(self, parent=None, main_window=None): #vers 1
        super().__init__(parent)

        self.main_window     = main_window
        self.standalone_mode = (main_window is None)
        self.is_docked       = not self.standalone_mode
        self.setAcceptDrops(True)   # drag image files anywhere on the workshop to load them

        # DP5-specific settings (JSON, separate from global theme)
        self.map_settings = MapSettings()

        # Fonts — size from settings
        fs = self.map_settings.get('ui_font_size')
        self.title_font  = QFont("Arial", fs + 4)
        self.panel_font  = QFont("Arial", fs)
        self.button_font = QFont("Arial", fs)
        self.fonthsize   = max(7, fs - 1)

        # Window chrome
        self.use_system_titlebar  = False
        self.window_always_on_top = False
        self.dragging             = False
        self.drag_position        = None
        self.resizing             = False
        self.resize_corner        = None
        self.corner_size          = 20
        self.hover_corner         = None

        # Canvas state — initial values from map_settings
        self._canvas_width  = self.map_settings.get('default_width')
        self._enforce_constraints = False
        self._canvas_mode   = 'free'
        self._mode_locked   = False
        self._mode_btns     = {}
        # Animation
        self._current_frame = 0
        self._canvas_height = self.map_settings.get('default_height')
        self._canvas_zoom   = self.map_settings.get('default_zoom')
        self._undo_stack    = deque(maxlen=self.map_settings.get('undo_levels'))
        self._redo_stack    = deque(maxlen=self.map_settings.get('undo_levels'))
        self.map_canvas     = None   # set by _create_centre_panel

        # Multi-canvas tabs - each entry is a full state snapshot (image
        # data, dimensions, undo/redo stacks) saved when switching away
        # from it. map_canvas/the-per-workshop undo/redo stacks always
        # hold whichever tab is currently active; there's no separate
        # MapCanvas widget per tab, since that attribute is referenced
        # throughout this file - switching tabs saves the outgoing
        # state into its slot and loads the incoming tab's saved state
        # into the same canvas widget/attributes instead.
        self._canvas_tabs = []
        self._active_canvas_tab_idx = 0


        # Duplicate (U) - position of the last stamped/duplicated copy,
        # so repeated duplicates step diagonally rather than stacking
        # exactly on top of each other.
        self._last_stamp_pos = None

        # Shared default text font/size - read by the Tool Settings
        # ribbon's Font button and by _TextToolCornerPanel when first
        # created, so both stay in sync rather than each having their
        # own hardcoded Arial/16 default.
        self._default_text_font_family = "Arial"
        self._default_text_font_size = 16

        # AppSettings (global theme only)
        if main_window and hasattr(main_window, 'app_settings'):
            self.app_settings = main_window.app_settings
        elif APPSETTINGS_AVAILABLE:
            try:
                self.app_settings = AppSettings()
            except Exception:
                self.app_settings = None
        else:
            self.app_settings = None

        if self.app_settings and hasattr(self.app_settings, 'theme_changed'):
            # Was connected to _refresh_icons only - _apply_theme() (which
            # calls _refresh_icons() itself at the end, plus re-styles the
            # ribbon/separator/dock title bars/splitter) was never actually
            # triggered by a live theme switch, only at initial startup and
            # from DP5's own theme dialog. That's why none of tonight's
            # theme-colour fixes appeared to change anything when switching
            # themes through the embedding app's own settings.
            self.app_settings.theme_changed.connect(self._apply_theme)

        # Icon factory
        self.icon_factory = SVGIconFactory()

        self.setWindowTitle(App_name)
        self.resize(1400, 800)
        self.setMinimumSize(900, 560)

        # Window icon — shows in taskbar, alt-tab, and title bar
        try:
            from apps.methods.imgfactory_svg_icons import get_map_workshop_icon
            self.setWindowIcon(get_map_workshop_icon(64))
        except Exception:
            pass

        if self.standalone_mode:
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint)
        else:
            self.setWindowFlags(Qt.WindowType.Widget)

        if parent:
            p = parent.pos()
            self.move(p.x() + 50, p.y() + 80)

        self.setup_ui()
        self._apply_theme()

    #    UI construction                                                        

    def setup_ui(self): #vers 1
        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(1, 1, 1, 1)
        main_layout.setSpacing(0)   # no gaps — each widget manages its own margin

        toolbar = self._create_toolbar()
        self._workshop_toolbar = toolbar
        if self.standalone_mode:
            # Standalone: toolbar is the titlebar/drag handle — always visible
            main_layout.addWidget(toolbar)
            toolbar.setVisible(True)
        # Docked: don't add toolbar to layout at all — avoids any ghost height

        # Internal QMenuBar — built always so menus exist for both modes.
        # Standalone: shown as topbar or hidden per settings.
        # Docked: hidden — a single dropdown button provides access instead.
        from PyQt6.QtWidgets import QHBoxLayout as _QHL
        self._menu_bar_container = QWidget(self)
        self._menu_bar_container.setObjectName("dp5_menu_bar_container")
        _chl = _QHL(self._menu_bar_container)
        _chl.setContentsMargins(0, 0, 0, 0)
        _chl.setSpacing(0)

        mb = QMenuBar(self._menu_bar_container)
        self._menu_bar = mb
        self._build_canvas_menus(mb)
        self._apply_menu_bar_style()
        _chl.addWidget(mb)

        if self.standalone_mode:
            show_mb = (self.map_settings.get('show_menubar', False) and
                       self.map_settings.get('menu_style', 'dropdown') == 'topbar')
            if show_mb:
                self._menu_bar_container.setMinimumHeight(0)
                self._menu_bar_container.setMaximumHeight(16777215)
                self._menu_bar_container.setVisible(True)
            else:
                self._menu_bar_container.setVisible(False)
                self._menu_bar_container.setFixedHeight(0)
            main_layout.addWidget(self._menu_bar_container)
        else:
            # Docked: hide the QMenuBar container entirely.
            # A slim dropdown button row gives full menu access instead.
            self._menu_bar_container.setVisible(False)
            self._menu_bar_container.setFixedHeight(0)
            # Don't add _menu_bar_container to layout — keeps zero height.

            # Titlebar [DP5] button is the menu entry point when docked
            self._register_titlebar_tool_btn()

        # QMainWindow + QDockWidget + QToolBar, matching the pattern used
        # for Model Workshop: canvas is the central widget (always visible,
        # never accidentally closed/floated away), Tools and Image Ops are
        # QToolBar ribbons (movable/floatable/collapsible), and Bitmaps/
        # Brush & Colors/Image Palette/User Palette are independent
        # QDockWidgets - every section can be dragged, floated, tabbed
        # with any other, or collapsed via double-clicking its title bar.
        from PyQt6.QtWidgets import QDockWidget, QMainWindow
        outer_mw = QMainWindow()
        outer_mw.setWindowFlags(Qt.WindowType.Widget)
        outer_mw.setDockOptions(
            QMainWindow.DockOption.AllowNestedDocks |
            QMainWindow.DockOption.AllowTabbedDocks)
        # Explicit separator styling - a nested QMainWindow embedded inside
        # IMG Factory's own tab widget inherits its theme stylesheet
        # cascading down, which can zero out the dock separator's
        # visibility entirely (found and fixed for Model Workshop). A
        # locally-set stylesheet takes precedence over anything inherited.
        themecol = self.app_settings.get_theme_colors()
        hexval = themecol.get('panel_bg')
        accentval = themecol.get('accent_primary')
        sep_bg_rule = f"background: {accentval}; " if accentval else ""

        outer_mw.setStyleSheet(
            f"QMainWindow {{ background: {hexval}; }} "
            f"QMainWindow::separator {{ {sep_bg_rule}"
            "width: 1px; height: 1px; } "
            "QMainWindow::separator:hover { background: palette(highlight); }")
        self._outer_mw = outer_mw
        # Per Keith: "unable to dock objects browser to the top left,
        # or left" - Qt's default for dockNestingEnabled is False,
        # which prevents dragging a dock into an area another dock
        # already occupies (e.g. Control Panel on the left) unless
        # nesting is explicitly enabled.
        outer_mw.setDockNestingEnabled(True)

        centre = self._create_centre_panel()
        outer_mw.setCentralWidget(centre)
        if hasattr(self, '_status_bar'):
            outer_mw.setStatusBar(self._status_bar)

        # Register the initial canvas as tab 1
        if self.map_canvas and not self._canvas_tabs:
            self._canvas_tabs.append(
                {'label': '1', 'state': self._capture_canvas_tab_state()})
            self._active_canvas_tab_idx = 0

        # World viewport dock - Top/Side/3D triple-pane, per Keith's
        # request ("would be interesting to also add viewports, side
        # view, top view, 3d view"). Same camera/pane-lock architecture
        # as Model Workshop's existing 4-pane DFFViewport quad, applied
        # here to MapViewport (world instance markers) instead.
        world_dock = self._create_world_viewport_dock()
        if world_dock is not None:
            outer_mw.addDockWidget(Qt.DockWidgetArea.RightDockWidgetArea, world_dock)

        # Object Browser dock - search + filter (All/Most Used/
        # Favourites/Generic) over the loaded object catalog, one row
        # per model (not per placement) - merged with what used to be
        # a separate Instance List dock, per Keith's request. Selecting
        # a row centres the camera on that model's first placement
        # (with Prev/Next cycling shown in the edit panel if it has
        # more than one), right-click for Add/Remove Favourites.
        object_browser_dock = self._create_object_browser_dock()
        outer_mw.addDockWidget(Qt.DockWidgetArea.RightDockWidgetArea, object_browser_dock)
        if world_dock is not None:
            outer_mw.splitDockWidget(world_dock, object_browser_dock, Qt.Orientation.Vertical)

        # IPL Inst File dock - takes the old IPL Sections dock's former
        # physical location (stacked below Object Browser, which now
        # also hosts the merged IDE/IPL/DAT/IMG tabs directly - per
        # Keith's request to merge the former standalone Editing Panel
        # dock in rather than keep it separate) - shows the real raw
        # content of whichever IPL is currently selected in the [IPL]
        # tab.
        ipl_inst_file_dock = self._create_ipl_inst_file_panel()
        outer_mw.addDockWidget(Qt.DockWidgetArea.RightDockWidgetArea, ipl_inst_file_dock)
        outer_mw.splitDockWidget(object_browser_dock, ipl_inst_file_dock, Qt.Orientation.Vertical)

        # Control Panel dock - replicates MooMapper's own "Hide/Show
        # Control Panel" area (see _create_control_panel_dock's own
        # docstring for what's real vs stubbed). Stacked below rather
        # than tabbed - MooMapper itself shows this as its own separate
        # area, and this panel has enough content to be awkward
        # competing for tab space with Object Browser's table.
        control_panel_dock = self._create_control_panel_dock()
        outer_mw.addDockWidget(Qt.DockWidgetArea.RightDockWidgetArea, control_panel_dock)
        outer_mw.splitDockWidget(ipl_inst_file_dock, control_panel_dock, Qt.Orientation.Vertical)

        # Widget registry - each entry is a self-contained dock module
        # under depends/. Adding, removing, or swapping a widget for an
        # alternative implementation only means editing this list, not
        # touching the loop below or any other part of setup_ui().
        for entry in self._WIDGET_REGISTRY:
            module = __import__(entry['module'], fromlist=[entry['create_fn']])
            create_fn = getattr(module, entry['create_fn'])
            dock = create_fn(self)
            outer_mw.addDockWidget(entry['area'], dock)
            if entry['enabled_setting']:
                dock.setVisible(self.map_settings.get(entry['enabled_setting']))

        # Tools and Image Ops are ribbons (QToolBar), not dock widgets -
        # linear rows of actions, not panels with complex widget content.
        # Plotting/Shapes (and any custom ribbons from the Ribbon
        # Manager) are built together, data-driven from the current
        # tool->ribbon assignment.
        # Per Keith: "script out the rest of the dp5 icons and buttons" -
        # Image Ops (Colour Adjustments/Seamless/Snow/Zoom Lens/Icon
        # Browser/Icon Editor), Annotate (Arrow/Marker/Text/Number/Blur/
        # Stickers), and Tool Settings (Pen Size/Strength - for tools
        # that no longer exist since Plotting/Shapes were emptied) are
        # all fully paint/image-editing specific with nothing left to
        # do in a map editor - no longer created/added. The method
        # definitions are left in place, unused, for safety/
        # reversibility rather than a riskier full removal.
        dynamic_ribbons = self._build_ribbons_from_assignment()
        canvas_tabs_ribbon = self._create_canvas_tabs_ribbon()
        panels_ribbon = self._create_panels_ribbon()
        for tb in dynamic_ribbons.values():
            outer_mw.addToolBar(Qt.ToolBarArea.LeftToolBarArea, tb)
        outer_mw.addToolBar(Qt.ToolBarArea.TopToolBarArea, canvas_tabs_ribbon)
        outer_mw.addToolBar(Qt.ToolBarArea.TopToolBarArea, panels_ribbon)

        # (All four paint-specific docks that used to be here - Bitmaps,
        # Brush & Colors, Image Palette, User Palette - have been removed;
        # the widget registry above is empty for now, ready for
        # map-specific docks to be added.)

        main_layout.addWidget(outer_mw)

        # Restore saved ribbon/dock layout, if any
        QTimer.singleShot(0, self._restore_outer_layout)

        # Save the layout reliably, not just on this widget's own
        # closeEvent (which likely never fires at all when embedded as
        # a tab - see _save_outer_layout's docstring): once when the
        # whole application quits, and debounced whenever any of the
        # map-specific docks actually change location/float state, so
        # rearranging docks gets saved even if the app is never
        # cleanly quit afterward.
        try:
            from PyQt6.QtWidgets import QApplication as _QApp
            app_inst = _QApp.instance()
            if app_inst is not None:
                app_inst.aboutToQuit.connect(self._save_outer_layout)
        except Exception:
            pass

        self._layout_save_timer = QTimer(self)
        self._layout_save_timer.setSingleShot(True)
        self._layout_save_timer.timeout.connect(self._save_outer_layout)
        for dock in (getattr(self, '_world_view_dock', None),
                     getattr(self, '_instance_list_dock', None),
                     getattr(self, '_object_browser_dock', None),
                     getattr(self, '_ipl_inst_file_dock', None),
                     getattr(self, '_control_panel_dock', None)):
            if dock is None:
                continue
            dock.dockLocationChanged.connect(
                lambda _area: self._layout_save_timer.start(1000))
            dock.topLevelChanged.connect(
                lambda _floating: self._layout_save_timer.start(1000))

        # Periodic safety net - dockLocationChanged only fires when a
        # dock's AREA changes (left<->right), not when it's restacked
        # within the same area via splitter rearrangement (e.g.
        # dragging Object Browser to sit under Instance List, both
        # staying in the right dock area - exactly the scenario that
        # wasn't being saved at all). A cheap periodic save while the
        # workshop is open guarantees rearrangements get persisted soon
        # regardless of which specific Qt signal would or wouldn't fire.
        self._layout_periodic_timer = QTimer(self)
        self._layout_periodic_timer.timeout.connect(self._save_outer_layout)
        self._layout_periodic_timer.start(15000)

        self._set_status(f"Canvas: {self._canvas_width}×{self._canvas_height}")

        # Initial tool
        QTimer.singleShot(0, lambda: self._select_tool(TOOL_PENCIL))

        # Corner resize overlay
        if self.standalone_mode:
            QTimer.singleShot(0, self._setup_corner_overlay)

    # - Toolbar (standalone titlebar)

    def _restore_outer_layout(self): #vers 1
        """Restore outer_mw's dock/ribbon layout, saved on last close.
        Version-checked so a stale save from an older ribbon/dock
        structure is cleanly rejected instead of Qt silently failing to
        restore anything, with a force-visible safety net afterwards -
        restoreState() can leave a dock fully hidden with no user-facing
        way to have meant that intentionally."""
        mw = getattr(self, '_outer_mw', None)
        if mw is None:
            return
        try:
            from PyQt6.QtCore import QByteArray
            state_hex = self.map_settings.get('outer_layout_state')
            saved_version = self.map_settings.get('outer_layout_version')
            if state_hex and saved_version == self._OUTER_LAYOUT_VERSION:
                mw.restoreState(QByteArray.fromHex(state_hex.encode()),
                                 self._OUTER_LAYOUT_VERSION)
        except Exception as _e:
            print(f"[MapWorkshop] _restore_outer_layout error: {_e}")
        finally:
            for dock in (getattr(self, '_bitmaps_dock', None),
                         getattr(self, '_brush_colors_dock', None),
                         getattr(self, '_img_palette_dock', None),
                         getattr(self, '_user_palette_dock', None)):
                if dock is not None:
                    dock.setVisible(True)
                    dock.toggleViewAction().setChecked(True)

    def _toggle_dock_floating(self, dock): #vers 1
        """Toggle a dock's floating state, explicitly positioning/sizing
        and raising the resulting floating window. Plain setFloating()
        alone can leave a dock with a custom title bar at an odd,
        overlapping, or effectively invisible position/size."""
        was_floating = dock.isFloating()
        dock.setFloating(not was_floating)
        if not was_floating:
            if dock.width() < 200 or dock.height() < 150:
                dock.resize(320, 400)
            anchor = self.mapToGlobal(self.rect().center())
            dock.move(anchor.x() - dock.width() // 2,
                      anchor.y() - dock.height() // 2)
            dock.show()
            dock.raise_()
            dock.activateWindow()

    def _make_dock_collapsible(self, dock, title): #vers 2
        """Custom title bar for a QDockWidget: double-click anywhere on the
        title (label or empty bar area) to collapse the dock down to just
        this title bar, hiding its content; double-click again to restore.
        Keeps float/close buttons so nothing is lost versus Qt's native
        title bar - only mouseDoubleClickEvent is overridden, so single-
        click-and-drag (moving the dock) still works exactly as before."""
        from PyQt6.QtWidgets import QWidget as _QW, QToolButton as _QTB

        bar = _QW()
        bar.setObjectName("dp5_dock_titlebar")
        # This is a plain QWidget, not a QFrame, so apply_panel_effects
        # (which only walks QFrame/QGroupBox) never touches it - it was
        # fully transparent, letting whatever's behind the dock bleed
        # through and create a visible seam against the content panel
        # below. Give it its own explicit background matching the same
        # theme colour the content panel uses, so there's no seam.
        if self.app_settings and hasattr(self.app_settings, 'get_theme_colors'):
            tc = self.app_settings.get_theme_colors()
            hexval = tc.get('panel_bg') or tc.get('bg_primary')
            if hexval:
                bar.setStyleSheet(f"QWidget#dp5_dock_titlebar {{ background: {hexval}; }}")
        lay = QHBoxLayout(bar)
        lay.setContentsMargins(6, 2, 2, 2)
        lay.setSpacing(2)

        lbl = QLabel(title)
        lbl.setFont(self.button_font)
        lay.addWidget(lbl)
        lay.addStretch()

        float_btn = _QTB()
        float_btn.setText("⧉")
        float_btn.setToolTip("Float/dock")
        float_btn.setFixedSize(20, 20)
        float_btn.setAutoRaise(True)
        float_btn.clicked.connect(lambda: self._toggle_dock_floating(dock))
        lay.addWidget(float_btn)

        close_btn = _QTB()
        close_btn.setText("×")
        close_btn.setToolTip("Close (use the View menu or another dock's "
                              "right-click menu to bring it back)")
        close_btn.setFixedSize(20, 20)
        close_btn.setAutoRaise(True)
        close_btn.clicked.connect(dock.close)
        lay.addWidget(close_btn)

        def _dbl_click(event, d=dock):  #vers 1
            content = d.widget()
            if content:
                content.setVisible(not content.isVisible())
        bar.mouseDoubleClickEvent = _dbl_click
        lbl.mouseDoubleClickEvent = _dbl_click

        dock.setTitleBarWidget(bar)


    def _create_toolbar(self): #vers 2
        # Read sizes from app_settings so they match Global App System Settings
        try:
            from apps.utils.app_settings_system import get_titlebar_sizes as _gts
            _as = getattr(self, 'app_settings', None) or getattr(
                  getattr(self, 'main_window', None), 'app_settings', None)
            _sz = _gts(_as)
            _TB_H    = _sz['tb_height']
            _BTN_SZ  = _sz['btn_size']
            _ICO_SZ  = _sz['icon_size']
            _BTN_H   = _sz['btn_height']
        except Exception:
            _TB_H, _BTN_SZ, _ICO_SZ, _BTN_H = 32, 32, 20, 24
        # titlebar and toolbar are the SAME widget — avoids a floating 45px ghost
        # that was rendering at (0,0) and creating blank space above the canvas.
        self.toolbar = QFrame()
        self.toolbar.setFrameStyle(QFrame.Shape.NoFrame)
        self.toolbar.setMaximumHeight(_TB_H + 10)
        self.toolbar.setObjectName("titlebar")
        self.toolbar.installEventFilter(self)
        self.toolbar.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents, False)
        self.toolbar.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
        self.toolbar.setMouseTracking(True)
        self.titlebar = self.toolbar   # alias — drag detection uses self.titlebar
        # gadgetbar_bg applied via QFrame#titlebar rule in global stylesheet

        layout = QHBoxLayout(self.toolbar)
        layout.setContentsMargins(5, 5, 5, 5)
        layout.setSpacing(4)

        icon_color = self._get_icon_color()

        # -_tb helper — icon-only button (no text label), tooltip carries
        # the label. Per Keith's spec: standalone titlebar buttons are
        # SVG icons only, not icon+text.
        def _tb(text, tip, slot, icon_fn=None):  #vers 2
            btn = QPushButton()
            btn.setToolTip(tip)
            btn.setMinimumHeight(28)
            btn.setMaximumHeight(28)
            btn.setFixedWidth(32)
            if icon_fn:
                try:
                    btn.setIcon(icon_fn(18, icon_color))
                    btn.setIconSize(QSize(18, 18))
                except Exception:
                    pass
            if slot: btn.clicked.connect(slot)
            layout.addWidget(btn)
            return btn

        # - Cog / Settings (standalone only)
        self.menu_toggle_btn = QPushButton()
        self.menu_toggle_btn.setIcon(SVGIconFactory.hamburger_menu_icon(20, icon_color))
        self.menu_toggle_btn.setIconSize(QSize(_ICO_SZ, _ICO_SZ))
        self.menu_toggle_btn.setToolTip("Show menu (topbar or dropdown — set in Settings)")
        self.menu_toggle_btn.setMinimumHeight(28)
        self.menu_toggle_btn.setMaximumHeight(28)
        self.menu_toggle_btn.setFixedWidth(32)
        self.menu_toggle_btn.clicked.connect(self._on_menu_btn_clicked)
        self.menu_toggle_btn.setVisible(self.standalone_mode)
        layout.addWidget(self.menu_toggle_btn)

        self.settings_btn = QPushButton()
        self.settings_btn.setIcon(SVGIconFactory.settings_icon(20, icon_color))
        self.settings_btn.setIconSize(QSize(_ICO_SZ, _ICO_SZ))
        self.settings_btn.setFixedWidth(32)
        self.settings_btn.clicked.connect(self._show_workshop_settings)
        self.settings_btn.setToolTip(App_name + " Settings")
        self.settings_btn.setVisible(self.standalone_mode)
        layout.addWidget(self.settings_btn)

        layout.addStretch()

        # - Title with Map Workshop icon
        title_row = QHBoxLayout()
        title_row.setSpacing(6)
        title_icon_lbl = QLabel()
        try:
            from apps.methods.imgfactory_svg_icons import get_map_workshop_icon
            pix = get_map_workshop_icon(22, icon_color).pixmap(22, 22)
            title_icon_lbl.setPixmap(pix)
        except Exception:
            if ICONS_AVAILABLE:
                pix = SVGIconFactory.folder_icon(20, icon_color).pixmap(20, 20)
                title_icon_lbl.setPixmap(pix)
        title_row.addWidget(title_icon_lbl)
        self.title_label = QLabel(App_name)
        self.title_label.setFont(self.title_font)
        self.title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        title_row.addWidget(self.title_label)
        layout.addLayout(title_row)

        layout.addStretch()

        # - [Load][Save][Add][Del][Rename][Undo] after title - per
        # Keith's exact spec. Dropped New/Clear/Brushes entirely (paint-
        # tool leftovers that don't belong in a map editor's titlebar).
        self.tb_load_btn = _tb("Load", "Load a GTA game - click for\n"
                              "Load Game Folder… / Load Game DAT File…",
                              self._show_map_load_menu,
                              SVGIconFactory.open_icon)
        self.tb_save_btn = _tb("Save", "Not yet available - writing changes\n"
                               "(Add/Delete/Rename, position/rotation edits)\n"
                               "back to disk isn't built yet; they're\n"
                               "in-memory only for the current session",
                               None,
                               SVGIconFactory.save_icon)
        self.tb_save_btn.setEnabled(False)
        from apps.methods.imgfactory_svg_icons import get_add_icon, get_trash_icon, get_rename_icon
        self.tb_add_btn = _tb("Add", "Add Instance Here - place another copy of the\n"
                              "selected model at the origin",
                              lambda: self._on_object_browser_add_clicked(),
                              lambda sz, col: get_add_icon(sz, col))
        self.tb_del_btn = _tb("Del", "Delete All Instances of the selected model",
                              lambda: self._on_object_browser_delete_clicked(),
                              lambda sz, col: get_trash_icon(sz, col))
        self.tb_rename_btn = _tb("Rename", "Rename the selected object",
                                 lambda: self._on_object_browser_rename_clicked(),
                                 lambda sz, col: get_rename_icon(sz, col))
        self.tb_undo_btn   = _tb("Undo",   "Undo last action  (Ctrl+Z)",
                                  self._undo_canvas,
                                  SVGIconFactory.undo_icon)

        self.properties_btn = QPushButton()
        self.properties_btn.setIcon(SVGIconFactory.properties_icon(20, icon_color))
        self.properties_btn.setIconSize(QSize(18, 18))
        self.properties_btn.setFixedWidth(32)
        self.properties_btn.setMinimumHeight(28)
        self.properties_btn.setMaximumHeight(28)
        self.properties_btn.setToolTip("Theme Settings")
        self.properties_btn.clicked.connect(self._launch_theme_settings)
        layout.addWidget(self.properties_btn)

        if self.standalone_mode:
            for attr, icon_method, slot, tip in [
                ('minimize_btn', 'minimize_icon', self.showMinimized,    "Minimize"),
                ('maximize_btn', 'maximize_icon', self._toggle_maximize, "Maximize"),
                ('close_btn',    'close_icon',    self.close,            "Close"),
            ]:
                btn = QPushButton()
                btn.setIcon(getattr(SVGIconFactory, icon_method)(18, icon_color))
                btn.setIconSize(QSize(18, 18))
                btn.setFixedWidth(36)
                btn.setMinimumHeight(28); btn.setMaximumHeight(28)
                btn.clicked.connect(slot)
                btn.setToolTip(tip)
                setattr(self, attr, btn)
                layout.addWidget(btn)

        return self.toolbar

    #    Docked compact action bar                                             

    def _create_centre_panel(self): #vers 3
        panel = QWidget()
        self._centre_panel = panel
        layout = QVBoxLayout(panel)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)


        # Status bar - built here (needs canvas context) but NOT added to
        # this panel's own layout. It becomes outer_mw's native status
        # bar instead (see setup_ui) - QMainWindow's built-in status bar
        # always spans the full window width, whereas embedding it in
        # this panel meant it only got whatever width was left after the
        # right-side dock widgets (Brush & Colors etc.) took their share,
        # squeezing/covering its right-aligned permanent widgets.
        self._status_bar = QStatusBar()
        self._status_bar.setSizeGripEnabled(False)
        self._status_bar.setFixedHeight(22)
        self._status_bar.setVisible(self.map_settings.get('show_statusbar'))
        # Permanent right-side info labels
        self._status_size_lbl  = QLabel("320×256")
        self._status_depth_lbl = QLabel("RGBA32")
        self._status_size_lbl.setStyleSheet("padding: 0 6px; color: palette(mid);")
        self._status_depth_lbl.setStyleSheet("padding: 0 6px; color: palette(mid);")
        self._status_bar.addPermanentWidget(self._status_depth_lbl)
        self._status_bar.addPermanentWidget(self._status_size_lbl)

        # Initial collapse state - matches the same logic
        # _toggle_paint_canvas applies dynamically. Canvas is hidden by
        # default, so without this the central widget area reserves
        # space for nothing, leaving a visible empty gap (IPL Sections
        # used to fill this same space as a fallback before it became
        # its own dock).
        if not self.map_settings.get('show_paint_canvas'):
            panel.setMaximumSize(0, 0)

        return panel

    def _build_canvas_menus(self, mb): #vers 2
        """Populate a QMenuBar (topbar) or QMenu (docked/dropdown) with all DP5 menus.
        Both share the same addMenu()/addAction() API so one method serves all cases."""
        # File
        fm = mb.addMenu("File")
        fm.addAction("Load Game Folder…", self._load_game_folder)
        fm.addAction("Load Game DAT File…", self._load_game_dat_file)
        fm.addSeparator()
        # Import submenu — all supported formats

        # Edit
        em = mb.addMenu("Edit")
        em.addAction("Undo\tCtrl+Z",       self._undo_canvas)
        em.addAction("Redo\tCtrl+Y",       self._redo_canvas)
        em.addSeparator()
        em.addAction("Cut\tCtrl+X",        self._cut_selection)
        em.addAction("Copy\tCtrl+C",       self._copy_selection)
        em.addAction("Paste\tCtrl+V",      self._paste_selection)
        em.addSeparator()
        em.addAction("Select All\tCtrl+A", self._select_all)
        em.addAction("Deselect\tEsc",      self._deselect)
        em.addSeparator()
        em.addAction("Rotate Selection…",  self._rotate_selection_dialog)
        # Picture

        vm = mb.addMenu("View")
        vm.addAction("Zoom in  Ctrl++",  lambda: self._set_zoom(
            self._canvas_zoom * 1.25 if self._canvas_zoom < 1
            else min(64, self._canvas_zoom + 1)))
        vm.addAction("Zoom out  Ctrl+-", lambda: self._set_zoom(
            max(0.05, self._canvas_zoom * 0.8 if self._canvas_zoom <= 1
            else self._canvas_zoom - 1)))
        vm.addSeparator()
        for z in (0.1, 0.25, 0.5, 1, 2, 4, 8, 16):
            lbl = f"{int(z)}×" if z >= 1 else f"{z}×"
            vm.addAction(lbl, lambda _, zz=z: self._set_zoom(zz))
        ga = vm.addAction("Pixel grid")
        ga.setCheckable(True)
        ga.setChecked(self.map_settings.get('show_pixel_grid'))
        ga.triggered.connect(self._set_show_grid)
        cg = vm.addAction("Cell grid (platform)")
        cg.setCheckable(True); cg.setChecked(False)
        cg.triggered.connect(self._toggle_cell_grid)
        sb = vm.addAction("Status bar")
        sb.setCheckable(True); sb.setChecked(self.map_settings.get('show_statusbar'))
        sb.triggered.connect(self._toggle_statusbar)

        os_act = vm.addAction("Onion skin")
        os_act.setCheckable(True); os_act.setChecked(False)
        os_act.triggered.connect(self._toggle_onion_skin)
        vm.addSeparator()
        clash_act = vm.addAction("Colour clash visualiser")
        clash_act.setCheckable(True); clash_act.setChecked(False)
        clash_act.triggered.connect(self._toggle_clash_visualiser)
        self._clash_act = clash_act
        vm.addSeparator()

        # Canvas mode

        # Tools menu (Renamed variable to 'tm' to avoid shadowing File 'fm')
        tm = mb.addMenu("Tools")

        # ... later in the _pm helper ...

        def _pm(label, items):  #vers 1
            sub = plm.addMenu(label)
            for name, mode in items:
                # Use 'checked' as a throwaway variable to catch the signal's boolean
                sub.addAction(name, lambda checked, m=mode: self._set_platform(m))

        # Platform menu
        plm = mb.addMenu("Platform")
        plm.addSeparator()

    #    Right panel: gadget bar + palettes                                     

    def get_menu_title(self) -> str: #vers 2
        """Short label for imgfactory titlebar button."""
        return "MAP"

    def _get_tool_menu_style(self) -> str: #vers 1
        """Read menu_style from map_settings."""
        return self.map_settings.get('menu_style', 'dropdown')

    def _build_menus_into_qmenu(self, parent_menu): #vers 4
        """Build all DP5 menus into parent_menu (QMenu or QMenuBar)."""
        self._build_canvas_menus(parent_menu)

    def _apply_menu_bar_style(self): #vers 3
        """Apply font size, height and colours to the topbar menubar.
        Uses explicit colours so it stays readable regardless of app theme.
        """
        mb = getattr(self, '_menu_bar', None)
        if not mb:
            return
        bar_h  = self.map_settings.get('menu_bar_height', 22)
        bar_fs = self.map_settings.get('menu_bar_font_size', 9)
        dd_fs  = self.map_settings.get('menu_dropdown_font_size', 9)

        # Get theme colours if available, otherwise use sensible defaults
        bg   = '#2b2b2b'
        fg   = '#e0e0e0'
        sel  = '#1976d2'
        selfg = '#ffffff'
        border = '#555555'
        try:
            app_settings = getattr(self, 'app_settings', None)
            if not app_settings and self.main_window:
                app_settings = getattr(self.main_window, 'app_settings', None)
            if app_settings:
                tc = app_settings.get_theme_colors() or {}
                bg    = tc.get('bg_primary',   bg)
                fg    = tc.get('text_primary',  fg)
                sel   = tc.get('accent',        sel)
                border = tc.get('border',       border)
        except Exception:
            pass

        # Height controlled by container — NOT by stylesheet min/max-height
        # (stylesheet height properties override Qt layout and prevent the bar showing)
        mb.setStyleSheet(f"""
            QMenuBar {{
                background-color: {bg};
                color: {fg};
                border-bottom: 1px solid {border};
                font-size: {bar_fs}pt;
            }}
            QMenuBar::item {{
                background-color: transparent;
                padding: 2px 6px;
            }}
            QMenuBar::item:selected {{
                background-color: {sel};
                color: {selfg};
            }}
            QMenu {{
                background-color: {bg};
                color: {fg};
                border: 1px solid {border};
                font-size: {dd_fs}pt;
            }}
            QMenu::item:selected {{
                background-color: {sel};
                color: {selfg};
            }}
        """)

        # Size the container based on settings, not isVisible() 
        # (isVisible() is False during __init__ even if widget will be shown)
        c = getattr(self, '_menu_bar_container', None)
        if c:
            show = (self.map_settings.get('show_menubar', False) and
                    self.map_settings.get('menu_style', 'dropdown') == 'topbar')
            if show:
                c.setMinimumHeight(0)
                c.setMaximumHeight(bar_h)
            # Don't touch height here if hiding — setup_ui and set_menu_orientation handle that


    def set_menu_orientation(self, style: str): #vers 5
        """Switch DP5 menu between 'topbar' (internal) and 'dropdown' (host menubar).
        When docked the internal bar is always suppressed regardless of style —
        imgfactory's top bar owns the menus via ToolMenuMixin injection.
        """
        self.map_settings.set('menu_style', style)
        self.map_settings.set('show_menubar', style == 'topbar')

        container = getattr(self, '_menu_bar_container', None) or getattr(self, '_menu_bar', None)
        if container:
            if style == 'topbar' and self.standalone_mode:
                # Only show internal bar in standalone mode
                container.setMinimumHeight(0)
                container.setMaximumHeight(16777215)
                container.setVisible(True)
                container.updateGeometry()
                self._apply_menu_bar_style()
            else:
                # Docked always hides internal bar; dropdown mode always hides it too
                container.setVisible(False)
                container.setMinimumHeight(0)
                container.setMaximumHeight(0)
                container.setFixedHeight(0)

        # Notify imgfactory to inject/remove tool menu when docked
        if not self.standalone_mode:
            mw = getattr(self, 'main_window', None)
            if mw and hasattr(mw, 'menu_bar_system'):
                if style == 'dropdown':
                    mw.menu_bar_system._inject_tool_menu(self)
                else:
                    mw.menu_bar_system._remove_tool_menu()


    # Ribbon Manager: master registry of every reassignable tool -
    # (tool_id, icon_shape, tooltip, default_ribbon_name). Replaces the
    # old hardcoded TOOL_ORDER/SHAPE_ORDER lists - _get_ribbon_assignment()
    # reads/writes which ribbon each tool_id currently lives in (starting
    # from these defaults), and _build_ribbons_from_assignment() builds
    # whichever ribbons that assignment calls for.
    _RIBBON_TOOL_REGISTRY = [
        # Paint tools removed per Keith's request ("lets remove the
        # plotting, shapes, like you suggest") - this registry drove
        # the Plotting/Shapes ribbons' contents (Pencil/Eraser/Fill/
        # Line/Rectangle/etc, all inherited from the DP5 fork this
        # editor started from). The registry mechanism itself (Ribbon
        # Manager, drag-reordering, per-tool ribbon assignment,
        # persistence) is untouched and still generic for any future
        # single-selection tool. Empty for now - LOD display mode and
        # Cull Boxes are added directly in _build_ribbons_from_assignment
        # instead (like the existing Active Zoom button), since neither
        # fits this registry's "one mutually-exclusive drawing tool
        # active at a time" model - LOD is a 3-way dropdown, Cull Boxes
        # is an independent show/hide toggle, not exclusive with
        # anything else.
    ]

    def _get_ribbon_assignment(self): #vers 1
        """Return the current tool_id -> ribbon_name assignment, reading
        from saved settings if present and falling back to each tool's
        default_ribbon from _RIBBON_TOOL_REGISTRY otherwise. Always
        returns an assignment for every registered tool, even if the
        saved settings only cover a subset (e.g. after a new tool was
        added since the setting was last saved)."""
        saved = self.map_settings.get('ribbon_tool_assignment') or {}
        assignment = {}
        for tool_id, _shape, _tip, default_ribbon in self._RIBBON_TOOL_REGISTRY:
            assignment[tool_id] = saved.get(tool_id, default_ribbon)
        return assignment

    def _set_ribbon_assignment(self, assignment: dict): #vers 1
        """Persist a full tool_id -> ribbon_name assignment to settings."""
        self.map_settings.set('ribbon_tool_assignment', dict(assignment))

    def _build_tool_ribbon(self, tb, tool_order, icon_sz, icon_color, tile_bg): #vers 1
        """Shared button-creation loop for a ribbon of persistent-tool
        buttons, including the right-click zoom-mode/settings menus -
        used by both the Shapes ribbon and the Plotting ribbon (the
        former combined Tools ribbon) so this logic isn't duplicated
        between them. All buttons register into the single shared
        self._tool_btns dict regardless of which ribbon they're on, so
        _select_tool's existing checked-state sync keeps working across
        both ribbons transparently."""
        from PyQt6.QtGui import QAction
        for tool_id, shape, tip in tool_order:
            ico = _load_tool_icon(shape, icon_sz, active=False,
                                  tile_bg=tile_bg, icon_col=icon_color)
            act = QAction(ico, tip, tb)
            act.setCheckable(True)
            act.setToolTip(tip)
            act.triggered.connect(lambda _, t=tool_id: self._select_tool(t))
            tb.addAction(act)
            self._tool_btns[tool_id] = act

            # Zoom keeps its right-click mode menu - needs the real
            # QToolButton Qt made for this action, QAction itself has no
            # mouse-event API.
            if tool_id == TOOL_ZOOM:
                w = tb.widgetForAction(act)
                if w is not None:
                    w.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
                    w.customContextMenuRequested.connect(self._zoom_mode_menu)
                    act.setToolTip("Zoom — left-click to zoom in\nRight-click to select zoom mode")

            # Brush-parameter tools get a right-click settings menu
            # (brush size plus intensity/strength/density) - the "brush
            # settings" chunk Keith asked for.
            elif tool_id in (TOOL_BLUR_BRUSH, TOOL_SMUDGE, TOOL_LINE,
                             TOOL_ERASER, TOOL_SPRAY, TOOL_SPRAYCAN,
                             TOOL_LIGHTEN, TOOL_DARKEN):
                w = tb.widgetForAction(act)
                if w is not None:
                    w.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
                    w.customContextMenuRequested.connect(
                        lambda pos, t=tool_id: self._show_tool_settings_menu(pos, t))
                    act.setToolTip(tip + "\nRight-click for settings")

    def _get_ribbon_tile_bg(self): #vers 1
        """Shared theme-colour lookup for a ribbon's icon tile
        background - used by both Shapes and Plotting ribbon builders."""
        _tile_bg = ''
        try:
            if self.app_settings:
                _tc = self.app_settings.get_theme_colors() or {}
                _tile_bg = _tc.get('gadgetbar_bg',
                               _tc.get('toolbar_bg',
                                   _tc.get('bg_secondary', '')))
        except Exception:
            pass
        return _tile_bg

    def _build_ribbons_from_assignment(self): #vers 1
        """Build every ribbon called for by the current tool->ribbon
        assignment (_get_ribbon_assignment) - replaces the old hardcoded
        _create_shapes_ribbon/_create_tools_ribbon pair. Returns a dict
        of {ribbon_name: QToolBar}. self._dynamic_ribbons keeps this
        same dict for later rebuilds (Ribbon Manager applying a new
        assignment or preset). Active Zoom always attaches to whichever
        ribbon TOOL_ZOOM currently lives in, since they're closely
        related (both zoom-adjacent)."""
        from PyQt6.QtWidgets import QToolBar
        from PyQt6.QtGui import QAction
        icon_color = self._get_icon_color()
        icon_sz = self.map_settings.get('tool_icon_size')
        _tile_bg = self._get_ribbon_tile_bg()
        hidden_tools = self.map_settings.get('hidden_tools') or []
        assignment = self._get_ribbon_assignment()

        # Group registry entries by their currently-assigned ribbon,
        # respecting a saved display order if one exists (from drag-
        # reordering in the Ribbon Manager), otherwise each tool's
        # natural registry order
        registry_by_id = {t: (t, s, tip, d) for t, s, tip, d in self._RIBBON_TOOL_REGISTRY}
        saved_order = self.map_settings.get('ribbon_tool_order') or []
        ordered_ids = [t for t in saved_order if t in registry_by_id]
        ordered_ids += [t for t, _s, _tip, _d in self._RIBBON_TOOL_REGISTRY
                        if t not in ordered_ids]

        by_ribbon = {}
        for tool_id in ordered_ids:
            if tool_id in hidden_tools:
                continue
            _t, shape, tip, default_ribbon = registry_by_id[tool_id]
            ribbon_name = assignment.get(tool_id, default_ribbon)
            by_ribbon.setdefault(ribbon_name, []).append((tool_id, shape, tip))

        self._tool_btns = {}
        self._tool_icon_sz = icon_sz
        ribbons = {}

        for ribbon_name, tool_order in by_ribbon.items():
            tb = QToolBar(ribbon_name)
            tb.setObjectName(ribbon_name)
            tb.setIconSize(QSize(icon_sz, icon_sz))
            tb.setMovable(True)
            tb.setFloatable(True)
            self._build_tool_ribbon(tb, tool_order, icon_sz, icon_color, _tile_bg)
            ribbons[ribbon_name] = tb

        # 'Plotting' ribbon now holds map-editing tools instead of paint
        # tools, per Keith's request - the registry above is empty, so
        # explicitly create it here (it won't appear in by_ribbon with
        # nothing registered to it) as a home for these.
        if 'Plotting' not in ribbons:
            tb = QToolBar('Plotting')
            tb.setObjectName('Plotting')
            tb.setIconSize(QSize(icon_sz, icon_sz))
            tb.setMovable(True)
            tb.setFloatable(True)
            ribbons['Plotting'] = tb
        map_tools_ribbon = ribbons['Plotting']

        # LOD display mode dropdown - moved here from the Panels ribbon,
        # since it's a map-editing tool, not a panel-visibility toggle.
        # Normal (default) / LOD / Both. Only meaningful for SA/SOL
        # worlds (GTA3/VC have no lod_index concept - see resolve_lod_
        # pairs), but harmless to show regardless since it's a no-op
        # when there are no resolved pairs.
        from PyQt6.QtWidgets import QToolButton
        from PyQt6.QtGui import QActionGroup
        lod_icon = self._render_variant_icon('lod_toggle', None, icon_sz,
                                             icon_color, has_menu=True)
        lod_btn = QToolButton()
        lod_btn.setIcon(lod_icon)
        lod_btn.setText("LOD")
        lod_btn.setToolTip("Global LOD display mode for paired objects\n"
                           "(a specific object can still be overridden\n"
                           "individually via Object Browser)")
        lod_btn.setPopupMode(QToolButton.ToolButtonPopupMode.InstantPopup)
        lod_menu = QMenu(lod_btn)
        lod_group = QActionGroup(lod_btn)
        lod_group.setExclusive(True)
        for mode, label in (('normal', "Normal"), ('lod', "LOD"), ('both', "Both")):
            act = lod_menu.addAction(label)
            act.setCheckable(True)
            act.setChecked(mode == 'normal')
            lod_group.addAction(act)
            act.triggered.connect(lambda checked=False, m=mode: self._set_lod_display_mode(m))
        lod_btn.setMenu(lod_menu)
        map_tools_ribbon.addWidget(lod_btn)
        self._lod_mode_button = lod_btn
        map_tools_ribbon.addSeparator()

        # Cull Boxes toggle - real, working feature (not a stub): shows/
        # hides wireframe boxes for every loaded IPL cull zone in World
        # View. An independent show/hide toggle, not a mutually-
        # exclusive "active drawing tool" - added directly rather than
        # through the tool registry above, which doesn't fit this shape.
        cull_icon = self._render_variant_icon('cull_boxes', None, icon_sz,
                                              icon_color, has_menu=False)
        cull_act = QAction(cull_icon, "Show Cull Boxes", map_tools_ribbon)
        cull_act.setCheckable(True)
        cull_act.setToolTip("Show/hide wireframe boxes for every loaded\n"
                            "IPL cull zone")
        cull_act.triggered.connect(self._toggle_cull_boxes)
        map_tools_ribbon.addAction(cull_act)
        self._cull_boxes_act = cull_act
        map_tools_ribbon.addSeparator()

        # Render Mode dropdown - Solid (default) / Semi / Wireframe -
        # only affects instances with real loaded geometry (see
        # MapViewport._draw_instance_mesh); instances falling back to
        # point rendering are unaffected regardless of this setting.
        render_mode_icon = self._render_variant_icon('cull_boxes', None, icon_sz,
                                                      icon_color, has_menu=True)
        render_mode_btn = QToolButton()
        render_mode_btn.setIcon(render_mode_icon)
        render_mode_btn.setText("Render")
        render_mode_btn.setToolTip("Render mode for instances with real loaded\n"
                                   "geometry - Solid / Semi(-transparent) / Wireframe")
        render_mode_btn.setPopupMode(QToolButton.ToolButtonPopupMode.InstantPopup)
        render_mode_menu = QMenu(render_mode_btn)
        render_mode_group = QActionGroup(render_mode_btn)
        render_mode_group.setExclusive(True)
        for mode, label in (('solid', "Solid"), ('semi', "Semi"), ('wireframe', "Wireframe")):
            act = render_mode_menu.addAction(label)
            act.setCheckable(True)
            act.setChecked(mode == 'solid')
            render_mode_group.addAction(act)
            act.triggered.connect(lambda checked=False, m=mode: self._set_render_mode(m))
        render_mode_btn.setMenu(render_mode_menu)
        map_tools_ribbon.addWidget(render_mode_btn)
        self._render_mode_button = render_mode_btn

        # Active Zoom - toggle button for follow-cursor high-zoom mode,
        # attached to whichever ribbon Zoom itself ended up in
        zoom_ribbon_name = assignment.get(TOOL_ZOOM, 'Plotting')
        tb = ribbons.get(zoom_ribbon_name)
        if tb is not None:
            _az_icon = self._render_variant_icon('active_zoom', None, icon_sz,
                                                 icon_color, has_menu=False)
            az_act = QAction(_az_icon, '', tb)
            az_act.setToolTip("Active Zoom - toggle high zoom that follows\n"
                              "the cursor, for detail/pixel work\n"
                              "Right-click for sensitivity")
            az_act.setCheckable(True)
            az_act.triggered.connect(self._toggle_active_zoom)
            tb.addAction(az_act)
            self._active_zoom_btn = az_act
            az_btn_w = tb.widgetForAction(az_act)
            if az_btn_w is not None:
                az_btn_w.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
                az_btn_w.customContextMenuRequested.connect(
                    self._show_active_zoom_sensitivity_menu)

        for tb in ribbons.values():
            self._apply_ribbon_style(tb)
            tb.orientationChanged.connect(lambda _o, t=tb: self._apply_ribbon_style(t))

        self._dynamic_ribbons = ribbons
        # Back-compat aliases - a couple of places still refer to these
        # two ribbons by name directly
        self._tools_ribbon  = ribbons.get('Plotting')
        self._shapes_ribbon = ribbons.get('Shapes')
        return ribbons

    def _rebuild_tool_ribbons(self): #vers 1
        """Tear down and rebuild every dynamic tool ribbon from the
        current assignment - called by the Ribbon Manager whenever the
        user moves a tool to a different ribbon, resizes icons, or
        loads a preset. Removes the old QToolBars from the main window
        first (Qt doesn't like two toolbars with the same objectName
        coexisting) before re-adding the freshly built ones."""
        outer_mw = getattr(self, '_outer_mw', None)
        if outer_mw is None:
            return
        old_ribbons = getattr(self, '_dynamic_ribbons', {})
        for tb in old_ribbons.values():
            outer_mw.removeToolBar(tb)
            tb.deleteLater()

        new_ribbons = self._build_ribbons_from_assignment()
        for tb in new_ribbons.values():
            outer_mw.addToolBar(Qt.ToolBarArea.LeftToolBarArea, tb)
            tb.show()

        # Re-select the current tool so the new buttons' checked state
        # matches, and refresh the always-visible settings ribbon
        if self.map_canvas:
            self._select_tool(self.map_canvas.tool, from_button_click=False)

    def _ribbon_presets_dir(self): #vers 1
        """Folder where Ribbon Manager presets are saved/loaded from."""
        d = Path.home() / '.config' / 'imgfactory' / 'map_ribbon_presets'
        d.mkdir(parents=True, exist_ok=True)
        return d

    def _open_ribbon_manager(self): #vers 2
        """Ribbon Manager dialog - two-pane layout matching Model
        Workshop's richer style: left pane lists ribbons (with an icon
        preview of their first tool), right pane shows the selected
        ribbon's tools with icons and names, drag-reorderable within the
        ribbon. Explicit +New Ribbon/Delete buttons, a Move-to-ribbon
        control, icon size slider, and Save/Load Preset. Changes apply
        live as you go (each move/reorder immediately rebuilds the real
        ribbons) - Cancel reverts to a snapshot taken when the dialog
        opened; OK just keeps whatever's already been applied."""
        from PyQt6.QtWidgets import (QListWidget, QListWidgetItem, QSplitter,
            QAbstractItemView, QSlider, QDialogButtonBox)

        snapshot_assignment = dict(self._get_ribbon_assignment())
        snapshot_order = list(self.map_settings.get('ribbon_tool_order') or [])
        snapshot_icon_horz = self.map_settings.get('ribbon_icon_size_horz')
        snapshot_icon_vert = self.map_settings.get('ribbon_icon_size_vert')
        registry_by_id = {t: (t, s, tip, d) for t, s, tip, d in self._RIBBON_TOOL_REGISTRY}
        icon_color = self._get_icon_color()
        icon_sz_preview = 20

        def _tool_name(tool_id):
            tip = registry_by_id[tool_id][2]
            return tip.split(' — ')[0].split(' (')[0].strip()

        def _tool_icon(tool_id):
            shape = registry_by_id[tool_id][1]
            return _load_tool_icon(shape, icon_sz_preview, active=False,
                                   tile_bg='', icon_col=icon_color)

        dlg = QDialog(self)
        dlg.setWindowTitle("Ribbon Manager")
        dlg.setMinimumSize(660, 440)
        outer = QVBoxLayout(dlg)

        # New/Delete ribbon row
        tb_row = QHBoxLayout()
        new_btn = QPushButton("+ New Ribbon")
        del_btn = QPushButton("Delete")
        save_preset_btn = QPushButton("Save Preset…")
        load_preset_btn = QPushButton("Load Preset…")
        for b in (new_btn, del_btn, save_preset_btn, load_preset_btn):
            tb_row.addWidget(b)
        tb_row.addStretch()
        outer.addLayout(tb_row)

        # Icon size row
        size_row = QHBoxLayout()
        size_row.addWidget(QLabel("Ribbon Icon Size:"))
        size_slider = QSlider(Qt.Orientation.Horizontal)
        size_slider.setRange(12, 64)
        size_slider.setSingleStep(2)
        size_slider.setValue(max(self.map_settings.get('ribbon_icon_size_horz'),
                                 self.map_settings.get('ribbon_icon_size_vert')))
        size_label = QLabel(f"{size_slider.value()}px")
        size_label.setMinimumWidth(36)
        size_row.addWidget(size_slider, 1)
        size_row.addWidget(size_label)
        outer.addLayout(size_row)

        splitter = QSplitter(Qt.Orientation.Horizontal)
        outer.addWidget(splitter, 1)

        left = QWidget()
        ll = QVBoxLayout(left); ll.setSpacing(4)
        ll.addWidget(QLabel("Ribbons"))
        ribbon_list = QListWidget()
        ll.addWidget(ribbon_list)
        splitter.addWidget(left)

        right = QWidget()
        rl = QVBoxLayout(right); rl.setSpacing(4)
        tool_label = QLabel("Select a ribbon")
        rl.addWidget(tool_label)
        tool_list = QListWidget()
        tool_list.setDragDropMode(QAbstractItemView.DragDropMode.InternalMove)
        tool_list.setDefaultDropAction(Qt.DropAction.MoveAction)
        tool_list.setIconSize(QSize(icon_sz_preview, icon_sz_preview))
        rl.addWidget(tool_list)

        move_row = QHBoxLayout()
        move_row.addWidget(QLabel("Move selected to:"))
        move_combo = QComboBox()
        move_row.addWidget(move_combo, 1)
        move_btn = QPushButton("Move →")
        move_row.addWidget(move_btn)
        rl.addLayout(move_row)
        splitter.addWidget(right)
        splitter.setSizes([200, 460])

        btns = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok |
                                QDialogButtonBox.StandardButton.Cancel)
        outer.addWidget(btns)

        state = {'selected_ribbon': None}

        def _current_assignment():
            return self._get_ribbon_assignment()

        def _refresh_ribbon_list():
            ribbon_list.blockSignals(True)
            prev = state['selected_ribbon']
            ribbon_list.clear()
            move_combo.clear()
            assignment = _current_assignment()
            by_ribbon = {}
            for tool_id in assignment:
                by_ribbon.setdefault(assignment[tool_id], []).append(tool_id)
            for name in sorted(by_ribbon.keys()):
                item = QListWidgetItem(name)
                tools_here = by_ribbon[name]
                if tools_here:
                    item.setIcon(_tool_icon(tools_here[0]))
                ribbon_list.addItem(item)
                move_combo.addItem(name)
            ribbon_list.blockSignals(False)
            # Reselect the previously-selected ribbon if it still exists
            if prev is not None:
                matches = ribbon_list.findItems(prev, Qt.MatchFlag.MatchExactly)
                if matches:
                    ribbon_list.setCurrentItem(matches[0])
                    return
            if ribbon_list.count():
                ribbon_list.setCurrentRow(0)

        def _refresh_tool_list():
            tool_list.blockSignals(True)
            tool_list.clear()
            name = state['selected_ribbon']
            if not name:
                tool_list.blockSignals(False)
                return
            tool_label.setText(f"{name} — tools")
            assignment = _current_assignment()
            order = self.map_settings.get('ribbon_tool_order') or []
            ordered_ids = [t for t in order if t in registry_by_id]
            ordered_ids += [t for t in registry_by_id if t not in ordered_ids]
            for tool_id in ordered_ids:
                if assignment.get(tool_id) != name:
                    continue
                item = QListWidgetItem(_tool_icon(tool_id), _tool_name(tool_id))
                item.setData(Qt.ItemDataRole.UserRole, tool_id)
                tool_list.addItem(item)
            tool_list.blockSignals(False)

        def _on_ribbon_selected(row):
            item = ribbon_list.item(row)
            state['selected_ribbon'] = item.text() if item else None
            _refresh_tool_list()
        ribbon_list.currentRowChanged.connect(_on_ribbon_selected)

        def _on_tools_reordered():
            # Rebuild ribbon_tool_order from the current combined order:
            # this ribbon's tools in their new order, all other tools
            # keeping their existing relative order
            name = state['selected_ribbon']
            if not name:
                return
            new_local_order = []
            for i in range(tool_list.count()):
                tid = tool_list.item(i).data(Qt.ItemDataRole.UserRole)
                if tid:
                    new_local_order.append(tid)
            old_order = self.map_settings.get('ribbon_tool_order') or []
            old_order = [t for t in old_order if t in registry_by_id]
            old_order += [t for t in registry_by_id if t not in old_order]
            merged = []
            it = iter(new_local_order)
            for tid in old_order:
                if tid in new_local_order:
                    merged.append(next(it))
                else:
                    merged.append(tid)
            self.map_settings.set('ribbon_tool_order', merged)
            self.map_settings.save()
            self._rebuild_tool_ribbons()
        tool_list.model().rowsMoved.connect(_on_tools_reordered)

        def _new_ribbon():
            name, ok = QInputDialog.getText(dlg, "New Ribbon", "Ribbon name:")
            if not ok or not name.strip():
                return
            name = name.strip()
            # An empty ribbon has no tools yet - it'll only actually
            # appear once a tool is moved into it, since ribbons here
            # are derived from the assignment rather than being
            # independent objects. Remember the intent so the list
            # shows it immediately as a hint of where to move things.
            state['pending_new_ribbon'] = name
            item = QListWidgetItem(name)
            ribbon_list.addItem(item)
            move_combo.addItem(name)
            ribbon_list.setCurrentItem(item)
            self._set_status(f"New ribbon '{name}' created - move a tool into it")
        new_btn.clicked.connect(_new_ribbon)

        def _delete_ribbon():
            name = state['selected_ribbon']
            if not name:
                return
            assignment = _current_assignment()
            affected = [t for t, r in assignment.items() if r == name]
            if affected:
                ans = QMessageBox.question(
                    dlg, "Delete Ribbon",
                    f"'{name}' has {len(affected)} tool(s).\n"
                    "They will move to Plotting.\nContinue?",
                    QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.Cancel)
                if ans != QMessageBox.StandardButton.Yes:
                    return
                for t in affected:
                    assignment[t] = 'Plotting'
                self._set_ribbon_assignment(assignment)
                self.map_settings.save()
                self._rebuild_tool_ribbons()
            state['selected_ribbon'] = None
            _refresh_ribbon_list()
        del_btn.clicked.connect(_delete_ribbon)

        def _move_selected():
            item = tool_list.currentItem()
            if not item:
                return
            tool_id = item.data(Qt.ItemDataRole.UserRole)
            target = move_combo.currentText().strip()
            if not tool_id or not target or target == state['selected_ribbon']:
                return
            assignment = _current_assignment()
            assignment[tool_id] = target
            self._set_ribbon_assignment(assignment)
            self.map_settings.save()
            self._rebuild_tool_ribbons()
            _refresh_ribbon_list()
        move_btn.clicked.connect(_move_selected)

        def _on_size_changed(px):
            size_label.setText(f"{px}px")
            self.map_settings.set('ribbon_icon_size_horz', px)
            self.map_settings.set('ribbon_icon_size_vert', px)
            self.map_settings.save()
            self._rebuild_tool_ribbons()
        size_slider.valueChanged.connect(_on_size_changed)

        def _save_preset():
            name, ok = QInputDialog.getText(dlg, "Save Preset", "Preset name:")
            if not ok or not name.strip():
                return
            data = {
                'assignment': _current_assignment(),
                'order': self.map_settings.get('ribbon_tool_order') or [],
                'icon_size': size_slider.value(),
            }
            path = self._ribbon_presets_dir() / f"{name.strip()}.json"
            try:
                path.write_text(json.dumps(data, indent=2))
                self._set_status(f"Saved ribbon preset '{name.strip()}'")
            except Exception as e:
                QMessageBox.warning(dlg, "Save Preset Error", str(e))
        save_preset_btn.clicked.connect(_save_preset)

        def _load_preset():
            presets_dir = self._ribbon_presets_dir()
            files = sorted(p.stem for p in presets_dir.glob('*.json'))
            if not files:
                QMessageBox.information(dlg, "Load Preset", "No saved presets found.")
                return
            name, ok = QInputDialog.getItem(dlg, "Load Preset", "Preset:", files, 0, False)
            if not ok:
                return
            path = presets_dir / f"{name}.json"
            try:
                data = json.loads(path.read_text())
            except Exception as e:
                QMessageBox.warning(dlg, "Load Preset Error", str(e))
                return
            self._set_ribbon_assignment(data.get('assignment', {}))
            self.map_settings.set('ribbon_tool_order', data.get('order', []))
            loaded_size = data.get('icon_size')
            if loaded_size:
                self.map_settings.set('ribbon_icon_size_horz', loaded_size)
                self.map_settings.set('ribbon_icon_size_vert', loaded_size)
                size_slider.setValue(loaded_size)
            self.map_settings.save()
            self._rebuild_tool_ribbons()
            state['selected_ribbon'] = None
            _refresh_ribbon_list()
            self._set_status(f"Loaded ribbon preset '{name}'")
        load_preset_btn.clicked.connect(_load_preset)

        def _on_accept():
            dlg.accept()

        def _on_cancel():
            self._set_ribbon_assignment(snapshot_assignment)
            self.map_settings.set('ribbon_tool_order', snapshot_order)
            self.map_settings.set('ribbon_icon_size_horz', snapshot_icon_horz)
            self.map_settings.set('ribbon_icon_size_vert', snapshot_icon_vert)
            self.map_settings.save()
            self._rebuild_tool_ribbons()
            dlg.reject()
        btns.accepted.connect(_on_accept)
        btns.rejected.connect(_on_cancel)

        _refresh_ribbon_list()
        self._ribbon_manager_dlg = dlg
        dlg.exec()

    def _apply_ribbon_style(self, toolbar): #vers 2
        """Apply icon size / padding / opacity to a ribbon, using whichever
        of the vertical or horizontal settings match its current
        orientation. Called on creation and again whenever the ribbon is
        dragged to a dock area of the other orientation (orientationChanged),
        so settings stay correct no matter where the user puts it."""
        vertical = (toolbar.orientation() == Qt.Orientation.Vertical)
        icon_sz = self.map_settings.get(
            'ribbon_icon_size_vert' if vertical else 'ribbon_icon_size_horz')
        padding = self.map_settings.get(
            'ribbon_padding_vert' if vertical else 'ribbon_padding_horz')
        btn_padding = self.map_settings.get(
            'ribbon_button_padding_vert' if vertical else 'ribbon_button_padding_horz')
        opacity = self.map_settings.get('ribbon_opacity')

        toolbar.setIconSize(QSize(icon_sz, icon_sz))
        toolbar.layout().setSpacing(max(0, int(padding)))

        alpha = max(0, min(255, round(opacity / 100 * 255)))
        base_col = None
        if self.app_settings and hasattr(self.app_settings, 'get_theme_colors'):
            tc = self.app_settings.get_theme_colors()
            #hexval = tc.get('panel_bg')
            hexval = tc.get('bg_primary')
            #hexval = tc.get('bg_secondary')
            if hexval:
                base_col = QColor(hexval)

        btn_rule = f"QToolButton {{ padding: {max(0, int(btn_padding))}px; }}"
        if base_col is not None:
            toolbar.setStyleSheet(
                f"QToolBar {{ background: rgba({base_col.red()}, {base_col.green()}, "
                f"{base_col.blue()}, {alpha}); "
                f"spacing: {max(0, int(padding))}px; }} " + btn_rule)
        else:
            toolbar.setStyleSheet(
                f"QToolBar {{ spacing: {max(0, int(padding))}px; }} " + btn_rule)

        # Right-click anywhere on the ribbon's empty background opens a
        # menu with Ribbon Manager access (per Keith's request) - only
        # wire this once per toolbar, since _apply_ribbon_style also
        # re-runs on every orientationChanged
        if not getattr(toolbar, '_ribbon_ctx_menu_wired', False):
            toolbar.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
            toolbar.customContextMenuRequested.connect(
                lambda pos, t=toolbar: self._ribbon_context_menu(t, pos))
            toolbar._ribbon_ctx_menu_wired = True

    def _ribbon_context_menu(self, toolbar, pos): #vers 2
        """Right-click context menu on any ribbon's empty background -
        gives access to the Ribbon Manager without needing to dig
        through Settings, plus quick lock/unlock for all ribbons at
        once. Also lists every dock with a checkable show/hide action
        (dock.toggleViewAction()) - per Keith's report that Object
        Browser disappeared entirely with no way to bring it back;
        this is the safety net for that failure mode, regardless of
        what caused the dock to end up hidden in the first place."""
        from PyQt6.QtWidgets import QMenu, QToolBar, QDockWidget
        menu = QMenu(self)
        menu.addAction("Ribbon Manager…", self._open_ribbon_manager)
        menu.addSeparator()
        outer_mw = getattr(self, '_outer_mw', None)
        if outer_mw is not None:
            menu.addAction("Lock All Ribbons",
                lambda: [tb.setMovable(False) for tb in outer_mw.findChildren(QToolBar)])
            menu.addAction("Unlock All Ribbons",
                lambda: [tb.setMovable(True) for tb in outer_mw.findChildren(QToolBar)])
            docks = outer_mw.findChildren(QDockWidget)
            if docks:
                menu.addSeparator()
                docks_menu = menu.addMenu("Docks")
                for dock in docks:
                    docks_menu.addAction(f"Show {dock.windowTitle()}",
                        lambda checked=False, d=dock: (d.show(), d.raise_()))
        menu.exec(toolbar.mapToGlobal(pos))

    def _create_image_ops_ribbon(self): #vers 1
        """Image Ops ribbon - Colour Adjustments/Seamless/Snow/Zoom Lens/
        Icon Browser/Icon Editor, converted from plain icon buttons to a
        QToolBar of QActions."""
        from PyQt6.QtWidgets import QToolBar
        from PyQt6.QtGui import QAction
        icon_color = self._get_icon_color()
        _render_sz = max(self.map_settings.get('ribbon_icon_size_vert'),
                        self.map_settings.get('ribbon_icon_size_horz'))

        tb = QToolBar("Image Ops")
        tb.setObjectName("Image Ops")
        tb.setMovable(True)
        tb.setFloatable(True)

        def _op_act(icon_method, tip, slot):  #vers 2
            try:
                ico = getattr(SVGIconFactory, icon_method)(_render_sz, icon_color)
            except Exception:
                ico = QIcon()
            act = QAction(ico, tip, tb)
            act.setToolTip(tip)
            act.triggered.connect(slot)
            tb.addAction(act)
            return act

        self._apply_ribbon_style(tb)
        tb.orientationChanged.connect(lambda _o, t=tb: self._apply_ribbon_style(t))
        return tb

    def _create_panels_ribbon(self): #vers 1
        """Panel/canvas show-hide toggles, in their own dedicated ribbon
        - deliberately NOT the Canvas Tabs ribbon, since
        _refresh_canvas_tabs_ribbon() calls ribbon.clear() every time a
        canvas tab is added/removed, which would wipe these out (this
        was a real, previously-unnoticed bug: the canvas toggle added in
        an earlier session lived in that ribbon and was being silently
        cleared the same way)."""
        from PyQt6.QtWidgets import QToolBar
        from PyQt6.QtGui import QAction

        tb = QToolBar("Panels")
        tb.setObjectName("Panels")
        tb.setMovable(True)
        tb.setFloatable(True)

        icon_color = self._get_icon_color()
        icon_sz = self.map_settings.get('ribbon_icon_size_horz')

        # Per Keith: "Show world view + Show object browser buttons are
        # pointless since the panels already exist" - removed. Both
        # docks remain closeable/reopenable via their own [x] and
        # right-click title bar menu, the standard Qt dock convention,
        # without needing dedicated ribbon buttons for it.

        # LOD display mode dropdown moved to the map-tools ribbon (see
        # _build_ribbons_from_assignment) - it's a map-editing tool,
        # not a panel-visibility toggle, so it belongs there instead.

        self._panels_ribbon = tb
        self._apply_ribbon_style(tb)
        tb.orientationChanged.connect(lambda _o, t=tb: self._apply_ribbon_style(t))
        return tb

    def _create_canvas_tabs_ribbon(self): #vers 2
        """Canvas tabs ribbon - one numbered button per open canvas
        document (New Canvas creates a new one), positioned top-right
        above the right-side dock widgets. Click a number to switch to
        that canvas; the whole state (image data, undo/redo) is saved/
        restored via _switch_canvas_tab().

        Only builds the numbered tab buttons - _refresh_canvas_tabs_
        ribbon() calls ribbon.clear() every time tabs change, so nothing
        else can safely live in this toolbar (see _create_panels_ribbon,
        which is where the canvas/panel toggles live instead - a
        previous version of this method put them here, which meant they
        were being silently wiped out on every tab change)."""
        from PyQt6.QtWidgets import QToolBar

        tb = QToolBar("Canvas Tabs")
        tb.setObjectName("Canvas Tabs")
        tb.setMovable(True)
        tb.setFloatable(True)

        self._canvas_tabs_ribbon = tb
        self._canvas_tab_btns = []
        self._apply_ribbon_style(tb)
        tb.orientationChanged.connect(lambda _o, t=tb: self._apply_ribbon_style(t))
        self._refresh_canvas_tabs_ribbon()
        return tb

    def _create_tool_settings_ribbon(self): #vers 1
        """Tool Settings ribbon - always-visible, top-right (per Keith's
        request), showing the CURRENTLY SELECTED tool's Size (GIMP-style
        plain numeric field, label changes per tool - 'Pen Size',
        'Line Size', 'Eraser Size', etc), Strength/Intensity where that
        tool has one, or Font name + size for the Text tool family.
        Updates live via _refresh_tool_settings_ribbon(), called from
        _select_tool on every switch. The right-click popup menus
        (_show_tool_settings_menu) still exist as a secondary path, but
        this ribbon is the primary always-visible one Keith asked for."""
        from PyQt6.QtWidgets import QToolBar, QWidgetAction, QDoubleSpinBox

        tb = QToolBar("Tool Settings")
        tb.setObjectName("Tool Settings")
        tb.setMovable(True)
        tb.setFloatable(True)

        # Row height tracks the same global setting the other ribbons'
        # icon sizing already responds to (Settings > Ribbons > Icon
        # size, horizontal) - previously hardcoded to 22, disconnected
        # from that setting entirely. _refresh_tool_settings_ribbon_size()
        # re-applies this if the setting changes later.
        _ROW_H = self.map_settings.get('ribbon_icon_size_horz')

        container = QWidget()
        row = QHBoxLayout(container)
        row.setContentsMargins(6, 1, 6, 1)
        row.setSpacing(6)

        self._ts_size_label = QLabel("Size:")
        self._ts_size_label.setFixedHeight(_ROW_H)
        self._ts_size_spin = QSpinBox()
        self._ts_size_spin.setRange(1, 200)
        self._ts_size_spin.setFixedWidth(55)
        self._ts_size_spin.setFixedHeight(_ROW_H)
        self._ts_size_spin.valueChanged.connect(self._on_ts_size_changed)

        self._ts_strength_label = QLabel("Strength:")
        self._ts_strength_label.setFixedHeight(_ROW_H)
        self._ts_strength_spin = QDoubleSpinBox()
        self._ts_strength_spin.setFixedWidth(65)
        self._ts_strength_spin.setFixedHeight(_ROW_H)
        self._ts_strength_spin.valueChanged.connect(self._on_ts_strength_changed)

        self._ts_font_btn = QPushButton(self._default_text_font_family)
        self._ts_font_btn.setFixedHeight(_ROW_H)
        self._ts_font_btn.setToolTip("Click to choose the text font")
        self._ts_font_btn.clicked.connect(self._pick_ts_font)
        self._ts_font_size_spin = QSpinBox()
        self._ts_font_size_spin.setRange(4, 200)
        self._ts_font_size_spin.setValue(self._default_text_font_size)
        self._ts_font_size_spin.setFixedWidth(55)
        self._ts_font_size_spin.setFixedHeight(_ROW_H)
        self._ts_font_size_spin.valueChanged.connect(self._on_ts_font_size_changed)
        self._ts_font_label = QLabel("Font:")
        self._ts_font_label.setFixedHeight(_ROW_H)
        self._ts_font_size_label = QLabel("Size:")
        self._ts_font_size_label.setFixedHeight(_ROW_H)

        row.addWidget(self._ts_size_label)
        row.addWidget(self._ts_size_spin)
        row.addWidget(self._ts_strength_label)
        row.addWidget(self._ts_strength_spin)
        row.addWidget(self._ts_font_label)
        row.addWidget(self._ts_font_btn)
        row.addWidget(self._ts_font_size_label)
        row.addWidget(self._ts_font_size_spin)
        container.setFixedHeight(_ROW_H + 4)

        wa = QWidgetAction(tb)
        wa.setDefaultWidget(container)
        tb.addAction(wa)

        self._tool_settings_ribbon = tb
        self._apply_ribbon_style(tb)
        self._refresh_tool_settings_ribbon()
        return tb

    def _refresh_tool_settings_ribbon(self): #vers 1
        """Update the Tool Settings ribbon's visible fields and values
        for whichever tool is currently active. Called from
        _select_tool on every tool switch."""
        if not hasattr(self, '_ts_size_spin'):
            return   # ribbon not built yet
        c = self.map_canvas
        tool = c.tool if c else None
        is_text = tool in self._TEXT_TOOLS

        # Font fields - text tools only
        for wdg in (self._ts_font_label, self._ts_font_btn,
                    self._ts_font_size_label, self._ts_font_size_spin):
            wdg.setVisible(is_text)
        if is_text:
            self._ts_font_btn.setText(self._default_text_font_family)
            self._ts_font_size_spin.blockSignals(True)
            self._ts_font_size_spin.setValue(self._default_text_font_size)
            self._ts_font_size_spin.blockSignals(False)

        # Size field - hidden for tools with no meaningful size, plain
        # numeric GIMP-style field otherwise with a per-tool label
        show_size = (not is_text) and (tool not in self._TOOL_NO_SIZE) and c is not None
        self._ts_size_label.setVisible(show_size)
        self._ts_size_spin.setVisible(show_size)
        if show_size:
            self._ts_size_label.setText(self._TOOL_SIZE_LABELS.get(tool, 'Size:'))
            self._ts_size_spin.blockSignals(True)
            self._ts_size_spin.setValue(c.brush_size)
            self._ts_size_spin.blockSignals(False)

        # Strength field - only for tools with a mapped attribute
        strength_info = self._TOOL_STRENGTH_MAP.get(tool)
        show_strength = (not is_text) and strength_info is not None and c is not None
        self._ts_strength_label.setVisible(show_strength)
        self._ts_strength_spin.setVisible(show_strength)
        if show_strength:
            attr, label, minv, maxv, is_float, step = strength_info
            self._ts_strength_label.setText(label)
            self._ts_strength_spin.blockSignals(True)
            self._ts_strength_spin.setDecimals(2 if is_float else 0)
            self._ts_strength_spin.setSingleStep(step)
            self._ts_strength_spin.setRange(minv, maxv)
            self._ts_strength_spin.setValue(getattr(c, attr))
            self._ts_strength_spin.blockSignals(False)

    def _on_ts_size_changed(self, v: int): #vers 1
        if self.map_canvas:
            self.map_canvas.brush_size = v

    def _on_ts_strength_changed(self, v): #vers 1
        c = self.map_canvas
        if not c: return
        info = self._TOOL_STRENGTH_MAP.get(c.tool)
        if info:
            attr = info[0]
            setattr(c, attr, v)

    def _pick_ts_font(self): #vers 1
        """Font button clicked - open a font picker, matching Keith's
        'font type, when you click on the font shown' request."""
        from PyQt6.QtGui import QFontDialog
        current = QFont(self._default_text_font_family)
        font, ok = QFontDialog.getFont(current, self, "Choose Text Font")
        if ok:
            self._default_text_font_family = font.family()
            self._ts_font_btn.setText(font.family())
            panel = getattr(self, '_text_corner_panel', None)
            if panel:
                panel.font_combo.setCurrentFont(font)

    def _on_ts_font_size_changed(self, v: int): #vers 1
        self._default_text_font_size = v
        panel = getattr(self, '_text_corner_panel', None)
        if panel:
            panel.size_spin.setValue(v)

    def _paint_variant_shape(self, p, kind, size, color): #vers 2
        """Draw a simple, reliable shape for an Annotate-ribbon variant
        that has no existing SVG icon - avoids depending on the system
        font having specific Unicode glyphs (arrows, shape symbols,
        circled digits), which isn't guaranteed and was the cause of
        blank/invisible buttons."""
        from PyQt6.QtGui import QPolygon
        qc = QColor(color)
        m = max(2, int(size * 0.15))
        pen_w = max(1, size // 10)
        p.setPen(QPen(qc, pen_w))
        p.setBrush(Qt.BrushStyle.NoBrush)

        if kind == 'arrow':
            p.drawLine(m, size - m, size - m, m)
            ah = size * 0.3
            p.setPen(Qt.PenStyle.NoPen); p.setBrush(qc)
            p.drawPolygon(QPolygon([
                QPoint(size - m, m), QPoint(int(size - m - ah), m),
                QPoint(size - m, int(m + ah))]))
        elif kind == 'double_arrow':
            cy = size // 2
            p.drawLine(m, cy, size - m, cy)
            ah = size * 0.22
            p.setPen(Qt.PenStyle.NoPen); p.setBrush(qc)
            p.drawPolygon(QPolygon([
                QPoint(size - m, cy), QPoint(int(size - m - ah), int(cy - ah)),
                QPoint(int(size - m - ah), int(cy + ah))]))
            p.drawPolygon(QPolygon([
                QPoint(m, cy), QPoint(int(m + ah), int(cy - ah)),
                QPoint(int(m + ah), int(cy + ah))]))
        elif kind == 'marker_pen':
            p.setPen(Qt.PenStyle.NoPen); p.setBrush(qc)
            p.drawRect(m, int(size * 0.4), size - 2*m, int(size * 0.2))
        elif kind == 'marker_rect':
            p.setPen(Qt.PenStyle.NoPen); p.setBrush(qc)
            p.drawRect(m, m, size - 2*m, size - 2*m)
        elif kind == 'marker_ellipse':
            p.setPen(Qt.PenStyle.NoPen); p.setBrush(qc)
            p.drawEllipse(m, m, size - 2*m, size - 2*m)
        elif kind in ('text_pointer', 'text_arrow'):
            font = QFont(); font.setPixelSize(int(size * 0.5)); font.setBold(True)
            p.setFont(font); p.setPen(qc)
            p.drawText(0, 0, int(size * 0.55), size,
                       Qt.AlignmentFlag.AlignCenter, "A")
            if kind == 'text_pointer':
                p.drawLine(int(size*0.55), int(size*0.7), size - m, int(size*0.7))
            else:
                p.drawLine(int(size*0.55), int(size*0.65), size - m - 3, m + 3)
                p.setPen(Qt.PenStyle.NoPen); p.setBrush(qc)
                ah = size * 0.16
                p.drawPolygon(QPolygon([
                    QPoint(size - m, m), QPoint(int(size - m - ah), m),
                    QPoint(size - m, int(m + ah))]))
        elif kind in ('number', 'number_pointer', 'number_arrow'):
            d = size - 2*m
            p.setPen(Qt.PenStyle.NoPen); p.setBrush(qc)
            p.drawEllipse(m, m, d, d)
            font = QFont(); font.setPixelSize(int(size * 0.45)); font.setBold(True)
            p.setFont(font); p.setPen(QColor('#ffffff'))
            p.drawText(m, m, d, d, Qt.AlignmentFlag.AlignCenter, "1")
        elif kind == 'pixelate':
            cell = size // 4
            for row in range(4):
                for col in range(4):
                    shade = qc.lighter(130) if (row+col) % 2 == 0 else qc.darker(120)
                    p.setPen(Qt.PenStyle.NoPen); p.setBrush(shade)
                    p.drawRect(col*cell, row*cell, cell, cell)
        elif kind == 'sharpen':
            p.setPen(Qt.PenStyle.NoPen); p.setBrush(qc)
            p.drawPolygon(QPolygon([
                QPoint(size//2, m), QPoint(size - m, size//2),
                QPoint(size//2, size - m), QPoint(m, size//2)]))
        elif kind == 'duplicate':
            p.setPen(QPen(qc, pen_w)); p.setBrush(Qt.BrushStyle.NoBrush)
            off = int(size * 0.18)
            sq = size - m - off
            p.drawRect(m, m, sq - m, sq - m)
            p.drawRect(m + off, m + off, sq - m, sq - m)
        elif kind == 'dot':
            p.setPen(Qt.PenStyle.NoPen); p.setBrush(qc)
            d = int(size * 0.6)
            off = (size - d) // 2
            p.drawEllipse(off, off, d, d)
        elif kind == 'bullet':
            p.setPen(Qt.PenStyle.NoPen); p.setBrush(qc)
            d = int(size * 0.35)
            off = (size - d) // 2
            p.drawEllipse(off, off, d, d)
        elif kind == 'active_zoom':
            p.setPen(QPen(qc, pen_w)); p.setBrush(Qt.BrushStyle.NoBrush)
            lens_d = int(size * 0.55)
            p.drawEllipse(m, m, lens_d, lens_d)
            p.drawLine(m + int(lens_d*0.85), m + int(lens_d*0.85), size - m, size - m)
            # Crosshair inside the lens to suggest cursor-follow
            cx, cy = m + lens_d // 2, m + lens_d // 2
            p.drawLine(cx - lens_d//4, cy, cx + lens_d//4, cy)
            p.drawLine(cx, cy - lens_d//4, cx, cy + lens_d//4)
        elif kind == 'canvas_toggle':
            p.setPen(QPen(qc, pen_w)); p.setBrush(Qt.BrushStyle.NoBrush)
            p.drawRect(m, m + 2, size - 2*m, size - 2*m - 4)
            p.drawLine(m, size - m - 2, size - m, m + 2)
        elif kind == 'lod_toggle':
            p.setPen(QPen(qc, pen_w)); p.setBrush(Qt.BrushStyle.NoBrush)
            big = int(size * 0.65)
            small = int(size * 0.4)
            p.drawRect(m, m, big, big)
            p.drawRect(size - m - small, size - m - small, small, small)
        elif kind in ('eye_visible', 'eye_hidden'):
            p.setPen(QPen(qc, pen_w)); p.setBrush(Qt.BrushStyle.NoBrush)
            cx, cy = size // 2, size // 2
            eye_w = size - 2*m
            eye_h = int(eye_w * 0.55)
            rect_x, rect_y = m, cy - eye_h // 2
            # Eye outline as an ellipse (approximated with drawArc-style
            # via drawEllipse, simplest reliable cross-Qt-version option)
            p.drawEllipse(rect_x, rect_y, eye_w, eye_h)
            pupil_d = max(2, int(eye_h * 0.5))
            p.setBrush(QBrush(qc))
            p.drawEllipse(cx - pupil_d//2, cy - pupil_d//2, pupil_d, pupil_d)
            if kind == 'eye_hidden':
                p.setBrush(Qt.BrushStyle.NoBrush)
                p.drawLine(m, m, size - m, size - m)
        elif kind == 'cull_boxes':
            p.setPen(QPen(qc, pen_w)); p.setBrush(Qt.BrushStyle.NoBrush)
            front = int(size * 0.6)
            offset = int(size * 0.22)
            fx, fy = m, size - m - front
            p.drawRect(fx, fy, front, front)
            bx, by = fx + offset, fy - offset
            p.drawRect(bx, by, front, front)
            for dx, dy in ((0, 0), (front, 0), (0, front), (front, front)):
                p.drawLine(fx + dx, fy + dy, bx + dx, by + dy)
        elif kind == 'list_all':
            p.setPen(QPen(qc, pen_w)); p.setBrush(Qt.BrushStyle.NoBrush)
            step = (size - 2*m) / 3.0
            for i in range(3):
                y = int(m + step * i + step/2)
                p.drawLine(m, y, size - m, y)
        elif kind == 'bars_most_used':
            p.setPen(QPen(qc, pen_w)); p.setBrush(QBrush(qc))
            bar_w = max(2, (size - 2*m) // 4)
            gap = max(1, bar_w // 2)
            heights = (0.4, 0.7, 1.0)
            x = m
            for h in heights:
                bar_h = int((size - 2*m) * h)
                p.drawRect(x, size - m - bar_h, bar_w, bar_h)
                x += bar_w + gap
        elif kind == 'star_filled':
            from PyQt6.QtGui import QPolygonF
            p.setPen(QPen(qc, pen_w)); p.setBrush(QBrush(qc))
            cx, cy = size / 2.0, size / 2.0
            outer_r, inner_r = (size - 2*m) / 2.0, (size - 2*m) / 4.5
            pts = []
            for i in range(10):
                ang = -math.pi/2 + i * math.pi/5
                r = outer_r if i % 2 == 0 else inner_r
                pts.append(QPointF(cx + r*math.cos(ang), cy + r*math.sin(ang)))
            p.drawPolygon(QPolygonF(pts))
        elif kind == 'box_generic':
            p.setPen(QPen(qc, pen_w)); p.setBrush(Qt.BrushStyle.NoBrush)
            box = size - 2*m
            p.drawRect(m, m, box, box)
            p.drawLine(m, m + box//3, size - m, m + box//3)
        elif kind == 'tab_ide':
            from PyQt6.QtGui import QPolygonF
            p.setPen(QPen(qc, pen_w)); p.setBrush(Qt.BrushStyle.NoBrush)
            fold = int(size * 0.28)
            pts = [QPointF(m, m), QPointF(size - m - fold, m), QPointF(size - m, m + fold),
                   QPointF(size - m, size - m), QPointF(m, size - m)]
            p.drawPolygon(QPolygonF(pts))
            p.drawLine(size - m - fold, m, size - m - fold, m + fold)
            p.drawLine(size - m - fold, m + fold, size - m, m + fold)
            step = (size - 2*m) / 3.5
            for i in range(2):
                y = int(m + fold + step * (i + 1))
                p.drawLine(m + 2, y, size - m - 2, y)
        elif kind == 'tab_ipl':
            p.setPen(QPen(qc, pen_w)); p.setBrush(Qt.BrushStyle.NoBrush)
            cx = size / 2.0
            top = m
            tip_y = size - m
            r = (size - 2*m) * 0.32
            cy = top + r + 1
            p.drawEllipse(QPointF(cx, cy), r, r)
            from PyQt6.QtGui import QPolygonF
            pts = [QPointF(cx - r * 0.6, cy + r * 0.6), QPointF(cx, tip_y),
                   QPointF(cx + r * 0.6, cy + r * 0.6)]
            p.drawPolygon(QPolygonF(pts))
            p.setBrush(QBrush(qc))
            p.drawEllipse(QPointF(cx, cy), r * 0.35, r * 0.35)
        elif kind == 'tab_dat':
            p.setPen(QPen(qc, pen_w)); p.setBrush(Qt.BrushStyle.NoBrush)
            step = (size - 2*m) / 2.5
            knob_x = (0.3, 0.65, 0.4)
            for i in range(3):
                y = int(m + step * i + step * 0.5)
                p.drawLine(m, y, size - m, y)
                kx = int(m + (size - 2*m) * knob_x[i])
                p.setBrush(QBrush(qc))
                p.drawEllipse(QPointF(kx, y), 2, 2)
                p.setBrush(Qt.BrushStyle.NoBrush)
        elif kind == 'tab_img':
            p.setPen(QPen(qc, pen_w)); p.setBrush(Qt.BrushStyle.NoBrush)
            band_h = (size - 2*m) / 3.2
            for i in range(3):
                y = int(m + i * (band_h + 2))
                p.drawRect(m, y, size - 2*m, int(band_h))
        elif kind in ('chevron_left', 'chevron_left2', 'chevron_right', 'chevron_right2'):
            p.setPen(QPen(qc, max(2, pen_w))); p.setBrush(Qt.BrushStyle.NoBrush)
            cy = size // 2
            half_h = (size - 2*m) // 2
            is_right = kind.startswith('chevron_right')
            is_double = kind.endswith('2')

            def _draw_chevron(cx): #vers 1
                if is_right:
                    p.drawLine(cx - half_h//2, cy - half_h, cx + half_h//2, cy)
                    p.drawLine(cx + half_h//2, cy, cx - half_h//2, cy + half_h)
                else:
                    p.drawLine(cx + half_h//2, cy - half_h, cx - half_h//2, cy)
                    p.drawLine(cx - half_h//2, cy, cx + half_h//2, cy + half_h)

            if is_double:
                offset = max(2, size // 5)
                _draw_chevron(size//2 - offset)
                _draw_chevron(size//2 + offset)
            else:
                _draw_chevron(size // 2)

    def _render_variant_icon(self, icon_kind, icon_method, size, icon_color,
                              has_menu: bool = False) -> QIcon: #vers 2
        """Render one square icon for a dropdown-button variant: the
        existing SVG icon if icon_method is given, otherwise a shape
        drawn via _paint_variant_shape for icon_kind. If has_menu, bakes
        a small triangle into the top-right corner instead of relying
        on Qt's separate style-drawn menu-indicator, which reads as a
        detached arrow rather than part of the icon."""
        from PyQt6.QtGui import QPolygon
        px = QPixmap(size, size)
        px.fill(Qt.GlobalColor.transparent)
        p = QPainter(px)
        p.setRenderHint(QPainter.RenderHint.Antialiasing)

        if icon_method:
            try:
                ico = getattr(SVGIconFactory, icon_method)(size, icon_color)
                ico.paint(p, 0, 0, size, size)
            except Exception:
                pass
        elif icon_kind:
            self._paint_variant_shape(p, icon_kind, size, icon_color)

        if has_menu:
            p.setPen(Qt.PenStyle.NoPen)
            p.setBrush(QColor(icon_color))
            a = max(5, size // 3)
            p.drawPolygon(QPolygon([
                QPoint(size - a, 0), QPoint(size, 0), QPoint(size, a)]))
        p.end()
        return QIcon(px)

    def _make_dropdown_tool_button(self, tb, variants, extra_menu_items=None): #vers 3
        """Build a button whose icon always shows a small triangle baked
        into its own top-right corner to indicate a menu is available -
        replacing Qt's separate style-drawn menu-indicator arrow, which
        reads as a detached, floating arrow rather than part of the
        icon (and, before this, often had nothing visible next to it
        since most variants had no icon at all). Single click activates
        whichever variant is currently 'active' for this button; click-
        and-hold reveals the other variants via DelayedPopup.
        variants: list of (icon_kind_or_none, icon_method_or_none, tip,
        tool_id) tuples - icon_kind selects a shape from
        _paint_variant_shape when there's no SVG icon_method. The first
        entry is the initial default. Picking a different variant from
        the dropdown makes IT the new default (updates the button's own
        icon/tooltip and records it in self._annotate_tool_btns so
        _select_tool's checked-state sync still finds it under
        whichever tool_id is currently active).
        extra_menu_items: optional list of (label, callback) tuples
        appended to the dropdown after the variants (e.g. 'Reset
        numbering')."""
        from PyQt6.QtWidgets import QToolButton
        icon_color = self._get_icon_color()
        _render_sz = max(self.map_settings.get('ribbon_icon_size_vert'),
                        self.map_settings.get('ribbon_icon_size_horz'))

        btn = QToolButton(tb)
        btn.setPopupMode(QToolButton.ToolButtonPopupMode.DelayedPopup)
        btn.setCheckable(True)
        # Hide Qt's own style-drawn menu-indicator - the dropdown cue
        # is baked into the icon pixmap itself instead (see
        # _render_variant_icon's has_menu triangle).
        btn.setStyleSheet("QToolButton::menu-indicator { image: none; width: 0px; }")

        kind0, icon0, tip0, tool0 = variants[0]
        btn.setIcon(self._render_variant_icon(kind0, icon0, _render_sz, icon_color,
                                               has_menu=True))
        btn.setToolTip(tip0)
        state = {'tool_id': tool0}

        def _activate(tool_id, kind, icon_method, tip):
            state['tool_id'] = tool_id
            btn.setIcon(self._render_variant_icon(kind, icon_method, _render_sz,
                                                   icon_color, has_menu=True))
            btn.setToolTip(tip)
            self._annotate_tool_btns[tool_id] = btn
            self._select_tool(tool_id)

        # Clicking the main button re-selects whatever tool_id is
        # currently active for it (updated by _activate when a
        # different variant is picked from the dropdown menu below)
        btn.clicked.connect(lambda: self._select_tool(state['tool_id']))

        menu = QMenu(btn)
        for kind, icon_method, tip, tool_id in variants:
            menu_icon = self._render_variant_icon(kind, icon_method, _render_sz,
                                                   icon_color, has_menu=False)
            act = menu.addAction(menu_icon, tip)
            act.triggered.connect(
                lambda _, tid=tool_id, k=kind, im=icon_method, t=tip: _activate(tid, k, im, t))
        if extra_menu_items:
            menu.addSeparator()
            for label, callback in extra_menu_items:
                menu.addAction(label).triggered.connect(callback)
        btn.setMenu(menu)

        self._annotate_tool_btns[tool0] = btn
        tb.addWidget(btn)
        return btn

    def _create_annotate_ribbon(self): #vers 3
        """Annotate ribbon - split-button dropdown groups matching the
        reference screenshots:
          Line group:   Arrow (default) / Double Arrow / Line
          Marker group: Marker Pen (default) / Marker Rectangle / Marker Ellipse
          Text group:   Text (default) / Text Pointer / Text Arrow
          Number group: Number (default) / Number Pointer / Number Arrow
                        (+ Reset numbering)
          Blur group:   Blur (default) / Pixelate
        Plus standalone Sharpen, Stickers (picker dialog), and
        Duplicate (U)."""
        from PyQt6.QtWidgets import QToolBar
        from PyQt6.QtGui import QAction
        icon_color = self._get_icon_color()
        _render_sz = max(self.map_settings.get('ribbon_icon_size_vert'),
                        self.map_settings.get('ribbon_icon_size_horz'))

        tb = QToolBar("Annotate")
        tb.setObjectName("Annotate")
        tb.setMovable(True)
        tb.setFloatable(True)
        self._annotate_tool_btns = {}

        # Line group
        self._make_dropdown_tool_button(tb, [
            ('arrow', None, 'Arrow (annotation)', TOOL_ARROW),
            ('double_arrow', None, 'Double Arrow', TOOL_DOUBLE_ARROW),
            (None, 'dp_line_icon', 'Line',      TOOL_LINE),
        ])

        # Marker group
        self._make_dropdown_tool_button(tb, [
            ('marker_pen', None, 'Marker Pen (semi-transparent highlighter)', TOOL_HIGHLIGHTER),
            ('marker_rect', None, 'Marker Rectangle', TOOL_MARKER_RECT),
            ('marker_ellipse', None, 'Marker Ellipse', TOOL_MARKER_ELLIPSE),
        ])

        # Text group - Text Pointer/Text Arrow removed per Keith
        # (buggy leader-line/arrow variants), plain Text only now
        self._make_dropdown_tool_button(tb, [
            (None, 'dp_text_icon', 'Text',      TOOL_TEXT),
        ])

        # Number group (+ Dots/Bullet Points variants, + reset numbering)
        # Number Pointer/Number Arrow removed per Keith - didn't work right.
        self._make_dropdown_tool_button(tb, [
            ('number', None, 'Number',           TOOL_NUMBER),
            ('dot', None, 'Dots (plain marker, no number)', TOOL_DOT),
            ('bullet', None, 'Bullet Point',     TOOL_BULLET),
        ], extra_menu_items=[
            ('Reset numbering to 1', lambda: setattr(self, '_next_annotation_number', 1)),
        ])

        # Blur/Smudge/Pixelate group
        self._make_dropdown_tool_button(tb, [
            (None, 'dp_blur_brush_icon', 'Blur brush', TOOL_BLUR_BRUSH),
            (None, 'dp_smudge_icon',    'Smudge',      TOOL_SMUDGE),
            ('pixelate', None, 'Pixelate',       TOOL_PIXELATE),
        ])

        # Standalone Sharpen
        sharpen_icon = self._render_variant_icon('sharpen', None, _render_sz,
                                                  icon_color, has_menu=False)
        sharpen_act = QAction(sharpen_icon, '', tb)
        sharpen_act.setToolTip('Sharpen brush\nRight-click for settings')
        sharpen_act.setCheckable(True)
        sharpen_act.triggered.connect(lambda: self._select_tool(TOOL_SHARPEN))
        tb.addAction(sharpen_act)
        self._annotate_tool_btns[TOOL_SHARPEN] = sharpen_act
        sharpen_btn_w = tb.widgetForAction(sharpen_act)
        if sharpen_btn_w is not None:
            sharpen_btn_w.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
            sharpen_btn_w.customContextMenuRequested.connect(
                lambda pos: self._show_tool_settings_menu(pos, TOOL_SHARPEN))

        # Standalone Stickers (opens the image picker dialog)
        _stickers_preview = _list_stickers()
        if _stickers_preview:
            _rgba, _sw, _sh = _load_sticker_rgba(_stickers_preview[0])
            _simg = QImage(bytes(_rgba), _sw, _sh, _sw * 4, QImage.Format.Format_RGBA8888)
            _spix = QPixmap.fromImage(_simg).scaled(
                _render_sz, _render_sz, Qt.AspectRatioMode.KeepAspectRatio,
                Qt.TransformationMode.SmoothTransformation)
            sticker_btn = QAction(QIcon(_spix), '', tb)
        else:
            sticker_btn = QAction('\U0001F600', tb)
        sticker_btn.setToolTip('Stickers - click to choose one from apps/emojis/')
        tb.addAction(sticker_btn)
        sticker_btn.triggered.connect(
            lambda: self._show_sticker_picker(tb.widgetForAction(sticker_btn)))

        # Standalone Duplicate (U) - duplicates the current stamp/copy
        # buffer immediately, offset slightly, matching the reference
        # tool's keyboard-shortcut duplicate action. Note: 'U' is
        # already bound to TOOL_SMUDGE in this app's own key scheme, so
        # no keyboard shortcut is bound here to avoid the conflict.
        dup_icon = self._render_variant_icon('duplicate', None, _render_sz,
                                              icon_color, has_menu=False)
        dup_act = QAction(dup_icon, '', tb)
        dup_act.setToolTip('Duplicate')
        dup_act.triggered.connect(self._duplicate_last_stamp)
        tb.addAction(dup_act)

        self._annotate_ribbon = tb
        self._apply_ribbon_style(tb)
        tb.orientationChanged.connect(lambda _o, t=tb: self._apply_ribbon_style(t))
        return tb



    def _show_sticker_picker(self, btn): #vers 3
        """Show a scrollable grid of sticker thumbnails loaded from
        apps/emojis/ - picking one sets it as the current sticker and
        switches to the Sticker tool."""
        from PyQt6.QtWidgets import QToolButton
        stickers = _list_stickers()
        if not stickers:
            QMessageBox.information(self, "Stickers",
                "No sticker images found in apps/emojis/.")
            return

        dlg = QDialog(self)
        dlg.setWindowTitle(f"Choose a Sticker ({len(stickers)} available)")
        dlg.setMinimumSize(420, 360)
        root = QVBoxLayout(dlg)

        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        grid_widget = QWidget()
        grid = QGridLayout(grid_widget)
        grid.setSpacing(2)

        cols = 12
        for idx, filename in enumerate(stickers):
            rgba, w, h = _load_sticker_rgba(filename)
            thumb_btn = QToolButton()
            thumb_btn.setFixedSize(28, 28)
            thumb_btn.setToolTip(filename)
            if rgba is not None:
                img = QImage(bytes(rgba), w, h, w * 4, QImage.Format.Format_RGBA8888)
                pix = QPixmap.fromImage(img).scaled(
                    24, 24, Qt.AspectRatioMode.KeepAspectRatio,
                    Qt.TransformationMode.SmoothTransformation)
                thumb_btn.setIcon(QIcon(pix))
                thumb_btn.setIconSize(QSize(24, 24))
            thumb_btn.clicked.connect(
                lambda _, f=filename: self._pick_sticker(f, dlg))
            grid.addWidget(thumb_btn, idx // cols, idx % cols)

        scroll.setWidget(grid_widget)
        root.addWidget(scroll)
        dlg.exec()

    def _pick_sticker(self, filename: str, dlg=None): #vers 2
        self._current_sticker = filename
        self._select_tool(TOOL_STICKER, from_button_click=False)
        if dlg is not None:
            dlg.accept()

    def _select_tool(self, tool_id: str, from_button_click: bool = True): #vers 4
        """Select a tool. Outline/filled shape variants and Select/Select
        Copy are now separate explicit tool IDs (own icons), so no fill-
        state remapping is needed - tool_id is used as-is."""
        # Crop and resize are immediate actions, not persistent tools
        if tool_id == TOOL_CROP:
            self._crop_to_selection()
            return
        if tool_id == TOOL_RESIZE:
            self._resize_canvas_dialog()
            return
        # Dither and symmetry are toggles
        if tool_id == TOOL_DITHER:
            self._toggle_dither_mode()
            return
        if tool_id == TOOL_SYMMETRY:
            self._toggle_symmetry_mode()
            return

        # Toggle-off: clicking the already-active tool's button again
        # deselects it, reverting to Pencil (the neutral default tool)
        # rather than leaving it "stuck" selected. Only applies to
        # actual button clicks - internal callers re-selecting the
        # current tool just to refresh icons/state (from_button_click=
        # False) should not trigger this.
        if from_button_click and self.map_canvas and self.map_canvas.tool == tool_id:
            if tool_id == TOOL_PENCIL:
                return   # already on the fallback tool, nothing to toggle to
            tool_id = TOOL_PENCIL

        actual_tool = tool_id

        if self.map_canvas:
            c = self.map_canvas
            # Per-tool brush size: remember the outgoing tool's value,
            # restore the incoming tool's own remembered value (default
            # 1 for a tool switched to for the first time).
            if c.tool != actual_tool:
                c._tool_brush_sizes[c.tool] = c.brush_size
                c.brush_size = c._tool_brush_sizes.get(actual_tool, 1)
            # Clear selection marching-ants when leaving SELECT tool
            if self.map_canvas.tool in (TOOL_SELECT, TOOL_SELECT_COPY, 'lasso') and \
               actual_tool not in (TOOL_SELECT, TOOL_SELECT_COPY, 'lasso'):
                self.map_canvas._sel_active    = False
                self.map_canvas._selection_rect = None
                self.map_canvas._sel_floating   = False
                self.map_canvas.update()
            self.map_canvas.tool = actual_tool
            self.map_canvas._curve_phase = None
            self.map_canvas._curve_p0 = None
            self.map_canvas._curve_p1 = None
            self.map_canvas._curve_control = None
            self.map_canvas._curve_dragging_warp = False
            self.map_canvas._poly_pts  = []
            # Set cursor for zoom mode
            if tool_id == TOOL_ZOOM:
                zm = getattr(self.map_canvas, '_zoom_mode', 'in')
                if zm == 'box':
                    self.map_canvas.setCursor(Qt.CursorShape.CrossCursor)
                elif zm == 'out':
                    self.map_canvas.setCursor(Qt.CursorShape.SizeAllCursor)
                else:
                    self.map_canvas.setCursor(Qt.CursorShape.ArrowCursor)
            # When switching to TOOL_MOVE with a buffer, auto-float it
            if tool_id == TOOL_MOVE:
                c = self.map_canvas
                if c._sel_buffer and c._sel_buf_w > 0 and not c._sel_floating:
                    if not c._sel_float_pos:
                        if c._sel_active and c._selection_rect:
                            c._sel_float_pos = (c._selection_rect.x(),
                                                c._selection_rect.y())
                        else:
                            c._sel_float_pos = (
                                max(0, c.tex_w//2 - c._sel_buf_w//2),
                                max(0, c.tex_h//2 - c._sel_buf_h//2))
                    c._sel_floating = True
                    c.update()

        icon_sz = self.map_settings.get('tool_icon_size')
        icon_color = self._get_icon_color()
        _tile_bg = ''
        _active_tile_bg = ''
        try:
            if self.app_settings:
                _tc = self.app_settings.get_theme_colors() or {}
                _tile_bg = _tc.get('gadgetbar_bg',
                               _tc.get('toolbar_bg',
                                   _tc.get('bg_secondary', '')))
                _active_tile_bg = _tc.get('accent_primary', _tile_bg)
        except Exception:
            pass

        # Shape key map — includes all variants
        _shape_map = {
            TOOL_PENCIL: 'pencil', TOOL_ERASER: 'eraser', TOOL_FILL: 'fill',
            TOOL_SPRAY: 'spray', TOOL_PICKER: 'picker', TOOL_CURVE: 'curve',
            TOOL_LINE: 'line',
            TOOL_RECT:     'rect',     TOOL_FILLED_RECT:     'filled_rect',
            TOOL_CIRCLE:   'circle',   TOOL_FILLED_CIRCLE:   'filled_circle',
            TOOL_TRIANGLE: 'triangle', TOOL_FILLED_TRIANGLE: 'filled_triangle',
            TOOL_POLYGON:  'polygon',  TOOL_FILLED_POLYGON:  'filled_polygon',
            TOOL_STAR:     'star',     TOOL_FILLED_STAR:     'filled_star',
            TOOL_SELECT: 'select', TOOL_SELECT_COPY: 'select_copy', TOOL_LASSO: 'lasso',
            TOOL_FILLED_LASSO: 'filled_lasso',
            TOOL_MOVE: 'move', TOOL_ZOOM: 'zoom', TOOL_TEXT: 'text',
            TOOL_STAMP: 'stamp',
        }

        for tid, btn in getattr(self, '_tool_btns', {}).items():
            is_active = (tid == tool_id)
            btn.setChecked(is_active)
            shape_key = _shape_map.get(tid, tid)
            btn.setIcon(_make_tool_icon(
                shape_key, icon_sz, active=is_active,
                tile_bg=(_active_tile_bg if is_active else _tile_bg),
                icon_col=icon_color))

        # Sync Annotate ribbon's tool buttons too (Line/Curve/Blur appear
        # in both ribbons as separate QAction instances)
        for tid, act in getattr(self, '_annotate_tool_btns', {}).items():
            act.setChecked(tid == tool_id)

        # Sync brush thumbnail active border
        if hasattr(self, '_brush_thumb'):
            self._brush_thumb.set_active(tool_id == TOOL_STAMP)

        self._refresh_tool_settings_ribbon()


    def _set_brush_size(self, v: int):  #vers 2
        if self.map_canvas:
            self.map_canvas.brush_size = v
        if hasattr(self, '_size_val_lbl'):
            self._size_val_lbl.setText(str(v))


    def _toggle_dither_mode(self): #vers 2
        """Cycle dither: off → checker → bayer → off."""
        cycle = {'off': 'checker', 'checker': 'bayer', 'bayer': 'off'}
        self._dither_mode = cycle[self._dither_mode]
        if self.map_canvas:
            self.map_canvas.dither_mode = self._dither_mode
        btn = self._tool_btns.get(TOOL_DITHER)
        if btn:
            labels = {'off':'Dither','checker':'Dthr ⊞','bayer':'Dthr ▦'}
            btn.setChecked(self._dither_mode != 'off')
            btn.setToolTip(f"Dither: {self._dither_mode} — click to cycle")
        self._set_status(f"Dither: {self._dither_mode}")


    def _toggle_symmetry_mode(self): #vers 2
        """Cycle symmetry: off → H → V → quad → off."""
        cycle = {'off': 'H', 'H': 'V', 'V': 'quad', 'quad': 'off'}
        self._symmetry_mode = cycle[self._symmetry_mode]
        if self.map_canvas:
            self.map_canvas.symmetry_mode = self._symmetry_mode
        btn = self._tool_btns.get(TOOL_SYMMETRY)
        if btn:
            labels = {'off': 'Sym', 'H': 'Sym H', 'V': 'Sym V', 'quad': 'Sym X'}
            btn.setChecked(self._symmetry_mode != 'off')
            btn.setToolTip(f"Symmetry: {self._symmetry_mode.upper()} — click to cycle")
        self._set_status(f"Symmetry: {self._symmetry_mode.upper()}")

    def _set_opacity(self, v: int): #vers 1
        if self.map_canvas:
            self.map_canvas.opacity = v / 100.0
        if hasattr(self, '_opacity_val_lbl'):
            self._opacity_val_lbl.setText(f"{v}%")

    def _on_fg_changed(self, c: QColor): #vers 2
        if self.map_canvas:
            self.map_canvas.color = c
        self.pal_bar.set_selection_by_color(c)
        self._push_color_history(c)

    def _push_color_history(self, c: QColor): #vers 1
        hex_c = c.name()
        if self._color_history and self._color_history[0] == hex_c:
            return
        if hex_c in self._color_history:
            self._color_history.remove(hex_c)
        self._color_history.insert(0, hex_c)
        self._color_history = self._color_history[:12]
        for i, btn in enumerate(self._color_hist_btns):
            if i < len(self._color_history):
                col = self._color_history[i]
                border = 'palette(mid)'
                if self.app_settings and hasattr(self.app_settings, 'get_theme_colors'):
                    tc = self.app_settings.get_theme_colors()
                    border = tc.get('border') or border
                btn.setStyleSheet(f"background:{col}; border:1px solid {border};")
                btn.setEnabled(True)
                btn.setToolTip(col)
                btn.clicked.disconnect() if btn.receivers(btn.clicked) > 0 else None
                btn.clicked.connect(lambda _, hc=col: self._fgbg_swatch.set_fg(QColor(hc)))
            else:
                from apps.components.DP5_Workshop.depends.brushcolors_widget import (
                    _style_empty_history_slot)
                _style_empty_history_slot(self, btn)
                btn.setEnabled(False)

    def _on_bg_changed(self, c: QColor):  #vers 2
        if self.map_canvas:
            self.map_canvas.bg_color = c



    def _on_menu_btn_clicked(self): #vers 3
        style = self.map_settings.get('menu_style')
        if style == 'dropdown':
            self._show_dropdown_menu()
        else:
            on = not self.map_settings.get('show_menubar')
            self.map_settings.set('show_menubar', on)
            self.map_settings.save()
            c = getattr(self, '_menu_bar_container', self._menu_bar if hasattr(self, '_menu_bar') else None)
            if c:
                c.setMinimumHeight(0)
                c.setMaximumHeight(16777215 if on else 0)
                c.setVisible(on)


    def _show_dropdown_menu(self): #vers 2
        """Pop up the canvas menus as a single QMenu dropdown — standalone safe."""
        from PyQt6.QtWidgets import QMenu
        menu = QMenu(self)
        try:
            self._build_menus_into_qmenu(menu)
        except Exception as _e:
            menu.addAction(f"Menu error: {_e}").setEnabled(False)
        btn = getattr(self, 'menu_toggle_btn', None)
        if btn:
            menu.exec(btn.mapToGlobal(btn.rect().bottomLeft()))
        else:
            from PyQt6.QtGui import QCursor
            menu.exec(QCursor.pos())


    def _toggle_menubar(self, on: bool): #vers 3
        self.map_settings.set('show_menubar', on)
        self.map_settings.save()
        c = getattr(self, '_menu_bar_container', self._menu_bar if hasattr(self, '_menu_bar') else None)
        if c:
            c.setMinimumHeight(0)
            c.setMaximumHeight(16777215 if on else 0)
            c.setVisible(on)


    def _set_snap_grid(self, on: bool): #vers 1
        if self.map_canvas:
            self.map_canvas.snap_grid = on


    def _set_show_grid(self, on: bool): #vers 2
        if self.map_canvas:
            self.map_canvas.show_grid = bool(on)
            self.map_canvas.update()
        self.map_settings.set('show_pixel_grid', bool(on))
        self.map_settings.save()
        if hasattr(self, '_grid_chk2'):
            self._grid_chk2.setChecked(bool(on))

    # Platform cell sizes: (cell_w, cell_h, max_colours_per_cell)
    _PLATFORM_CELLS = {
        'none':        (1,  1,   256),
        'amiga':       (8,  1,   32),
        'amiga_ecs':   (8,  1,   64),
        'amiga_aga':   (8,  1,   256),
        'amiga_ham':   (1,  1,   4096),
        'amiga_ham8':  (1,  1,   16777216),
        'amiga_rtg':   (1,  1,   256),
        'amiga_ntsc':  (8,  1,   32),    # OCS NTSC 320×200
        'amiga_hi':    (16, 1,   32),    # OCS HiRes 640×256
        'amiga_lace':  (8,  1,   32),    # OCS PAL interlace 320×512
        'amiga_ecs_hi':(16, 1,   64),    # ECS HiRes 640×256
        'amiga_aga_hi':(16, 1,   256),   # AGA HiRes 640×256
        'amiga_rtg_800':  (1,1,  256),   # RTG 800×600
        'amiga_rtg_1024': (1,1,  256),   # RTG 1024×768
        'amiga_rtg_pal':  (1,1,  256),   # RTG 720×576 PAL
        'amiga_rtg_ntsc': (1,1,  256),   # RTG 720×480 NTSC
        'amiga_overscan_pal':  (8,1,  32),   # native chipset overscan 720×576 PAL
        'amiga_overscan_ntsc': (8,1,  32),   # native chipset overscan 720×480 NTSC
        'rtg_1024_640':  (1,1, 16777216),
        'rtg_1280_720':  (1,1, 16777216),
        'rtg_1280_800':  (1,1, 16777216),
        'rtg_1280_1024': (1,1, 16777216),
        'rtg_1366_768':  (1,1, 16777216),
        'rtg_1440_900':  (1,1, 16777216),
        'rtg_1600_900':  (1,1, 16777216),
        'rtg_1680_1050': (1,1, 16777216),
        'rtg_1920_1080': (1,1, 16777216),
        'rtg_1920_1200': (1,1, 16777216),
        'rtg_2560_1440': (1,1, 16777216),
        'rtg_3840_2160': (1,1, 16777216),
        'c64':         (8,  8,   2),
        'c64m':        (4,  8,   4),
        'spectrum':    (8,  8,   2),
        'spectrum128': (8,  8,   2),   # same display as 48K
        'zx80':        (8,  8,   2),
        'zx81':        (8,  8,   2),
        'specnext':    (1,  1,   256), # Layer 2 free pixel mode
        'specnext_ul': (8,  8,   2),   # Next classic ULA mode
        'timex':       (8,  8,   2),   # TS2068 standard mode
        'timex_hi':    (1,  1,   2),   # TS2068 HiRes 512×192 B&W
        'pentagon':    (8,  8,   2),   # same as Spectrum
        'jupiter':     (8,  8,   2),   # Jupiter Ace B&W
        'msx':         (8,  8,   2),
        'msx2':        (8,  8,   16),
        'cpc':         (4,  8,   4),
        'cpc1':        (8,  8,   2),
        'cpc_plus':    (8,  8,   16),
        'pcw':         (8,  8,   2),
        'nc':          (8,  8,   4),
        'atari_2600':  (2,  1,   4),
        'atari_st':    (16, 1,   16),
        'atari_st_med':(8,  1,   4),
        'atari_st_hi': (1,  1,   2),
        'atari_ste':   (16, 1,   16),
        'atari_ste_med':(8, 1,   4),
        'atari_ste_hi': (1, 1,   2),
        'atari_800':   (2,  1,   4),
        'atari_800_lo':  (4,  1,   4),
        'atari_5200':  (2,  1,   4),
        'atari_5200_lo': (4,  1,   4),
        'atari_7800':  (2,  1,   4),
        'atari_7800_lo': (4,  1,   4),
        'atari_lynx':  (1,  1,   16),
        'atari_falcon':(1,  1,   65536),
        'atari_jaguar':(1,  1,   16777216),
        'plus4':       (8,  8,   2),
        'plus4m':      (4,  8,   4),
        'vic20':       (8,  8,   2),
        'nimbus':      (4,  4,   16),
        'nimbus_hi':   (4,  4,   16),   # Nimbus 640×250
        'nes':         (8,  8,   4),    # NES 256×240, 4col/8×8 tile
        'snes':        (8,  8,   16),   # SNES 256×224, 16col/8×8 tile
        'game_boy':    (8,  8,   4),    # GB 160×144, 4 shades
        'game_boy_pocket':(8,8,  4),
        'game_boy_color': (8,8,  4),    # GBC 160×144
        'game_boy_advance':(8,8, 4),    # GBA 240×160
        'sg1000':      (8,  8,   16),   # SG-1000 256×192
        'master_sys':  (8,  8,   16),   # Master System 256×192
        'mega_drive':  (8,  8,   16),   # Mega Drive 320×224
        'game_gear':   (8,  8,   16),   # Game Gear 160×144
        'pc_engine':   (8,  8,   16),   # PC Engine 256×240
        # BBC Micro / Electron (6845 CRTC + ULA) - MODE 1 is the common
        # default graphics mode; 0 and 2 are the other bitmap modes.
        'bbc0':        (8,  8,   2),    # MODE 0 640×256 mono
        'bbc1':        (8,  8,   4),    # MODE 1 320×256 4col (default)
        'bbc2':        (8,  8,   16),   # MODE 2 160×256 16col
        'electron0':   (8,  8,   2),    # same modes, software ULA
        'electron1':   (8,  8,   4),
        'electron2':   (8,  8,   16),
        # Acorn Archimedes (VIDC) - MODE 12 was the classic desktop
        # 16-colour mode; later RISC OS used higher-res 256-colour modes.
        'archimedes12':  (1,  1,  16),   # MODE 12 640×256 16col
        'archimedes_hi': (1,  1,  256),  # MODE 21-ish 640×480 256col
        # Tandy CoCo 1/2 and Dragon (shared Motorola 6847 VDG) - RG6
        # (256×192 2col) and CG2 (128×192 4col) are the two common
        # graphics modes.
        'coco12_hi':   (8,  8,   2),    # RG6 256×192 mono
        'coco12_lo':   (8,  8,   4),    # CG2 128×192 4col (default)
        'dragon_hi':   (8,  8,   2),
        'dragon_lo':   (8,  8,   4),
        # CoCo 3 (GIME) - much more flexible; 320×192 16col was the
        # common "high colour" mode, 640×225 the mono hi-res mode.
        'coco3':       (1,  1,   16),   # 320×192 16col (default)
        'coco3_hi':    (1,  1,   2),    # 640×225 mono
        # SAM Coupé - MODE 1 (Spectrum-compatible attribute clash,
        # 256×192 16col) is the common default; MODE 4 is the higher-
        # res 512×192 16col per-pixel mode.
        'samcoupe1':   (8,  8,   16),   # MODE 1 256×192 (default)
        'samcoupe4':   (1,  1,   16),   # MODE 4 512×192
        # Apple II - Lo-Res (chunky 40×48 blocks) and Hi-Res (280×192,
        # 6 NTSC artifact colours) are the two classic graphics modes.
        'apple2_lo':   (1,  1,   16),   # Lo-Res 40×48
        'apple2_hi':   (1,  1,   6),    # Hi-Res 280×192
        # Sinclair QL (ZX8301 "Master Chip") - MODE 4 (512×256 4col) and
        # MODE 8 (256×256 8col).
        'ql_4c':       (1,  1,   4),    # MODE 4 512×256 (default)
        'ql_8c':       (1,  1,   8),    # MODE 8 256×256
        # ULA Plus - same 256×192 resolution as standard Spectrum, just
        # a larger 64-colour palette via attribute remapping.
        'ula_plus':    (8,  8,   16),
        # Atari 2600 PAL - taller frame than NTSC (more active scanlines).
        'atari_2600_pal': (2, 1, 4),
    }

    # Platform native resolution, keyed the same as _PLATFORM_CELLS -
    # single source of truth, used by _set_platform and the drag-and-drop
    # auto-convert-to-platform feature.
    _PLATFORM_RES = {
        'c64':         (320, 200), 'c64m':        (160, 200),
        'spectrum':    (256, 192), 'spectrum128': (256, 192),
        'zx80':        (256, 192), 'zx81':        (256, 192),
        'specnext':    (320, 256), 'specnext_ul': (256, 192),
        'timex':       (256, 192), 'timex_hi':    (512, 192),
        'pentagon':    (256, 192), 'jupiter':     (256,  192),
        'msx':         (256, 192), 'msx2':        (256, 212),
        'cpc':         (160, 200), 'cpc1':        (320, 200),
        'cpc_plus':    (320, 200), 'pcw':         (720, 256),
        'nc':          (480, 128),
        'atari_2600':  (160, 192),  # NTSC standard kernel
        'atari_st':    (320, 200),  # Low res, 16col
        'atari_st_med': (640, 200),  # Medium res, 4col
        'atari_st_hi':  (640, 400),  # High res, mono (needs mono monitor)
        'atari_ste':   (320, 200),  # Low res, 16col (4096-col palette)
        'atari_ste_med': (640, 200), # Medium res, 4col
        'atari_ste_hi':  (640, 400), # High res, mono
        'atari_800':   (320, 192),
        'atari_800_lo':  (160, 192),  # ANTIC GR.7-style 4col mode
        'atari_5200':  (320, 192), 'atari_5200_lo': (160, 192),
        'atari_7800':  (160, 240), 'atari_7800_lo': (160, 192),  # 160×240 NTSC most common
        'atari_lynx':  (160, 102), 'atari_falcon': (320, 200), 'atari_falcon_hi': (640, 480),
        'atari_jaguar': (320, 240),
        'amiga':       (320, 256), 'amiga_ntsc':  (320, 200),
        'amiga_hi':    (640, 256), 'amiga_lace':  (320, 512),
        'amiga_ecs':   (320, 256), 'amiga_ecs_hi':(640, 256),
        'amiga_aga_hi':(640, 256),
        # Native chipset overscan (extends the normal display area using
        # the same OCS/ECS/AGA hardware) - distinct from the RTG entries
        # below, which are a separate Zorro graphics card, not the native
        # chipset.
        'amiga_overscan_pal':  (720, 576),
        'amiga_overscan_ntsc': (720, 480),
        'amiga_rtg_800':  (800, 600), 'amiga_rtg_1024': (1024, 768),
        'amiga_rtg_pal':  (720, 576), 'amiga_rtg_ntsc': (720, 480),
        # Modern / RTG - generic standard display resolutions, not tied
        # to any specific retro machine (truecolor, no fixed palette).
        'rtg_1024_640':  (1024, 640),
        'rtg_1280_720':  (1280, 720),   # 720p
        'rtg_1280_800':  (1280, 800),
        'rtg_1280_1024': (1280, 1024),
        'rtg_1366_768':  (1366, 768),
        'rtg_1440_900':  (1440, 900),
        'rtg_1600_900':  (1600, 900),
        'rtg_1680_1050': (1680, 1050),
        'rtg_1920_1080': (1920, 1080),  # 1080p
        'rtg_1920_1200': (1920, 1200),
        'rtg_2560_1440': (2560, 1440),  # 1440p
        'rtg_3840_2160': (3840, 2160),  # 4K UHD
        'amiga_aga':   (320, 256), 'amiga_ham':   (320, 256),
        'amiga_ham8':  (320, 256), 'amiga_rtg':   (640, 480),
        'plus4':       (320, 200), 'plus4m':      (160, 200),  # TED multicolor, like C64m
        'vic20':       (176, 184),
        'nes':         (256, 240), 'snes':        (256, 224),
        'game_boy':    (160, 144), 'game_boy_pocket': (160, 144),
        'game_boy_color': (160, 144), 'game_boy_advance': (240, 160),
        'nimbus':      (320, 250), 'nimbus_hi':   (640, 250),
        'sg1000':      (256, 192), 'master_sys':  (256, 192),
        'mega_drive':  (320, 224), 'game_gear':   (160, 144),
        'pc_engine':   (256, 240),
        'bbc0':      (640, 256), 'bbc1':      (320, 256), 'bbc2':      (160, 256),
        'electron0': (640, 256), 'electron1': (320, 256), 'electron2': (160, 256),
        'archimedes12': (640, 256), 'archimedes_hi': (640, 480),
        'coco12_hi': (256, 192), 'coco12_lo': (128, 192),
        'dragon_hi': (256, 192), 'dragon_lo': (128, 192),
        'coco3':     (320, 192), 'coco3_hi':  (640, 225),
        'samcoupe1': (256, 192), 'samcoupe4': (512, 192),
        'apple2_lo': (40, 48),   'apple2_hi': (280, 192),
        'ql_4c':     (512, 256), 'ql_8c':     (256, 256),
        'ula_plus':  (256, 192),
        'atari_2600_pal': (160, 228),
    }

    # Pixel aspect ratio (width-per-height) for modes whose pixel grid
    # isn't square - e.g. C64 multicolor packs 160 double-wide pixels
    # across the same physical screen width as hi-res's 320 pixels, so
    # each multicolor pixel renders twice as wide as it is tall. Modes
    # not listed here default to 1.0 (square). This only corrects the
    # horizontal stretch for modes that trade horizontal resolution for
    # colour depth at the same vertical resolution - interlace modes
    # (which instead double vertical resolution, e.g. amiga_lace) aren't
    # covered by this and default to 1.0.
    _PLATFORM_PIXEL_ASPECT = {
        'c64m': 2.0,
        'plus4m': 2.0,
        'cpc': 2.0,          # CPC Mode 0 160×200 vs Mode 1 'cpc1' 320×200
        'bbc0': 0.5, 'bbc2': 2.0,             # vs bbc1 (320×256) baseline
        'electron0': 0.5, 'electron2': 2.0,   # vs electron1 baseline
        'atari_800_lo': 2.0, 'atari_5200_lo': 2.0, 'atari_7800_lo': 2.0,
        'amiga_hi': 0.5, 'amiga_ecs_hi': 0.5, 'amiga_aga_hi': 0.5,
        'samcoupe4': 0.5,    # vs samcoupe1 (256×192) baseline
        'ql_8c': 2.0,        # vs ql_4c (512×256) baseline
    }

    # Modern/RTG resolutions each get 16-bit and 24-bit variants alongside
    # their base (32-bit) code, generated here rather than hand-typing 24
    # more dict entries - _PLATFORM_BIT_DEPTH maps a mode code to the
    # _bit_depth_combo index it should apply (0=32-bit RGBA, 1=24-bit RGB,
    # 2=16-bit R5G6B5), read by _set_platform.
    _PLATFORM_BIT_DEPTH = {}
    for _base in ('rtg_1024_640', 'rtg_1280_720', 'rtg_1280_800',
                   'rtg_1280_1024', 'rtg_1366_768', 'rtg_1440_900',
                   'rtg_1600_900', 'rtg_1680_1050', 'rtg_1920_1080',
                   'rtg_1920_1200', 'rtg_2560_1440', 'rtg_3840_2160'):
        _PLATFORM_BIT_DEPTH[_base] = 0   # base code = 32-bit
        _w, _h = _PLATFORM_RES[_base]
        _cw, _ch, _ = _PLATFORM_CELLS[_base]
        _PLATFORM_RES[_base + '_24']   = (_w, _h)
        _PLATFORM_CELLS[_base + '_24'] = (_cw, _ch, 16777216)
        _PLATFORM_BIT_DEPTH[_base + '_24'] = 1
        _PLATFORM_RES[_base + '_16']   = (_w, _h)
        _PLATFORM_CELLS[_base + '_16'] = (_cw, _ch, 65536)
        _PLATFORM_BIT_DEPTH[_base + '_16'] = 2
    del _base, _w, _h, _cw, _ch


    def _set_platform(self, mode: str): #vers 5
        """Set platform mode — cell grid, auto-load palette, resize canvas, fit zoom."""
        self._platform_mode = mode
        cw, ch, _ = self._PLATFORM_CELLS.get(mode, (1,1,256))
        if self.map_canvas:
            self.map_canvas.cell_w = cw
            self.map_canvas.cell_h = ch
            if mode != 'none':
                self.map_canvas.show_cell_grid = True
            self.map_canvas.pixel_aspect_x = self._PLATFORM_PIXEL_ASPECT.get(mode, 1.0)
            self.map_canvas.update()
        self.map_settings.set('platform_mode', mode)

        _pal_map = {
            'c64': 'C64', 'c64m': 'C64',
            'spectrum': 'ZX Spectrum', 'spectrum128': 'ZX Spectrum 128K',
            'specnext': 'ZX Spectrum Next', 'specnext_ul': 'ZX Spectrum',
            'zx80': 'ZX80', 'zx81': 'ZX81',
            'timex': 'Timex TS2068', 'timex_hi': 'Timex HiRes',
            'pentagon': 'Pentagon', 'jupiter': 'Jupiter Ace',
            'msx': 'MSX1', 'msx2': 'MSX2',
            'cpc': 'Amstrad CPC', 'cpc1': 'Amstrad CPC',
            'cpc_plus': 'Amstrad CPC+',
            'pcw':  'Amstrad PCW',
            'nc':   'Amstrad NC100/200',
            'atari_2600': 'Atari 2600 NTSC',
            'atari_st': 'Atari ST', 'atari_st_med': 'Atari ST', 'atari_st_hi': 'Atari ST',
            'atari_ste': 'Atari STe', 'atari_ste_med': 'Atari STe', 'atari_ste_hi': 'Atari STe',
            'atari_800': 'Atari 800 GTIA', 'atari_800_lo': 'Atari 800 GTIA',
            'atari_5200': 'Atari 5200', 'atari_5200_lo': 'Atari 5200',
            'atari_7800': 'Atari 7800', 'atari_7800_lo': 'Atari 7800',
            'atari_lynx': 'Atari Lynx',
            'atari_falcon': 'Atari Falcon',
            'atari_jaguar': 'Atari Jaguar',
            'amiga': 'Amiga OCS', 'amiga_ntsc': 'Amiga OCS',
            'amiga_overscan_pal': 'Amiga OCS', 'amiga_overscan_ntsc': 'Amiga OCS',
            'amiga_hi': 'Amiga OCS', 'amiga_lace': 'Amiga OCS',
            'amiga_ecs': 'Amiga ECS', 'amiga_ecs_hi': 'Amiga ECS',
            'amiga_aga_hi': 'Amiga AGA',
            'amiga_rtg_800': 'Amiga AGA WB', 'amiga_rtg_1024': 'Amiga AGA WB',
            'amiga_rtg_pal': 'Amiga AGA WB', 'amiga_rtg_ntsc': 'Amiga AGA WB',
            'rtg_1024_640': 'Amiga AGA WB', 'rtg_1280_720': 'Amiga AGA WB',
            'rtg_1280_800': 'Amiga AGA WB', 'rtg_1280_1024': 'Amiga AGA WB',
            'rtg_1366_768': 'Amiga AGA WB', 'rtg_1440_900': 'Amiga AGA WB',
            'rtg_1600_900': 'Amiga AGA WB', 'rtg_1680_1050': 'Amiga AGA WB',
            'rtg_1920_1080': 'Amiga AGA WB', 'rtg_1920_1200': 'Amiga AGA WB',
            'rtg_2560_1440': 'Amiga AGA WB', 'rtg_3840_2160': 'Amiga AGA WB',
            'amiga_aga': 'Amiga AGA',
            'amiga_ham': 'Amiga OCS',
            'amiga_ham8': 'Amiga AGA',
            'amiga_rtg': 'Amiga AGA WB',
            'plus4': 'Plus/4', 'plus4m': 'Plus/4', 'vic20': 'VIC-20',
            'nimbus': 'RM Nimbus',
            'bbc0': 'BBC Micro', 'bbc1': 'BBC Micro', 'bbc2': 'BBC Micro',
            'electron0': 'Acorn Electron', 'electron1': 'Acorn Electron',
            'electron2': 'Acorn Electron',
            'archimedes12': 'Acorn Archimedes', 'archimedes_hi': 'Acorn Archimedes',
            'coco12_hi': 'CoCo 1/2', 'coco12_lo': 'CoCo 1/2',
            'coco3': 'CoCo 3', 'coco3_hi': 'CoCo 3',
            'dragon_hi': 'Dragon 32/64', 'dragon_lo': 'Dragon 32/64',
            'samcoupe1': 'SAM Coupé', 'samcoupe4': 'SAM Coupé',
            'apple2_lo': 'Apple II Lo-Res', 'apple2_hi': 'Apple II Hi-Res',
            'ql_4c': 'Sinclair QL', 'ql_8c': 'Sinclair QL',
            'ula_plus': 'ULA Plus',
            'atari_2600_pal': 'Atari 2600 PAL',
        }
        # Depth-suffixed Modern/RTG codes (rtg_X_16/_24) share their base
        # code's palette entry - avoids duplicating 24 more _pal_map lines.
        _pal_lookup_mode = mode[:-3] if mode.endswith(('_16', '_24')) else mode
        if _pal_lookup_mode in _pal_map:
            self._apply_retro_palette(_pal_map[_pal_lookup_mode])

        # Apply bit depth for Modern/RTG modes (32/24/16-bit variants)
        if mode in self._PLATFORM_BIT_DEPTH:
            self._canvas_bit_depth = self._PLATFORM_BIT_DEPTH[mode]
            if hasattr(self, '_bit_depth_combo'):
                self._bit_depth_combo.setCurrentIndex(self._canvas_bit_depth)

        # Resize canvas to platform native resolution
        _plat_res = self._PLATFORM_RES
        if mode in _plat_res and self.map_canvas:
            pw, ph = _plat_res[mode]
            if (pw, ph) != (self._canvas_width, self._canvas_height):
                from PIL import Image
                src_w, src_h = self.map_canvas.tex_w, self.map_canvas.tex_h
                src_bytes = bytes(self.map_canvas.rgba)
                expected = src_w * src_h * 4
                if len(src_bytes) != expected:
                    self._set_status(
                        f"Canvas size mismatch detected ({src_w}×{src_h} "
                        f"expected {expected}B, got {len(src_bytes)}B) - "
                        f"resize skipped, please report this")
                    return
                img = Image.frombytes('RGBA', (src_w, src_h), src_bytes)
                img = img.resize((pw, ph), Image.LANCZOS)
                self._canvas_width  = pw
                self._canvas_height = ph
                self.map_canvas.tex_w = pw
                self.map_canvas.tex_h = ph
                self.map_canvas.rgba  = bytearray(img.tobytes())
                self.map_canvas.update()

        if self.map_canvas and mode != 'none':
            self._fit_canvas_to_viewport()
        self._set_status(
            f"Platform: {mode.upper()}  {self._canvas_width}×{self._canvas_height}  cell {cw}×{ch}")


    def _update_mode_buttons(self): #vers 1
        """Sync toolbar mode button checked states."""
        for m, btn in self._mode_btns.items():
            btn.setChecked(m == self._canvas_mode)


    def _apply_mode_to_canvas(self, mode: str): #vers 1
        """Convert current canvas to match mode constraints."""
        if not self.map_canvas: return
        from PIL import Image
        rgba = bytes(self.map_canvas.rgba)
        w, h = self._canvas_width, self._canvas_height
        img = Image.frombytes('RGBA', (w, h), rgba)

        if mode == 'platform':
            plat = self._platform_mode
            plat_res = {
                'c64':      (320, 200), 'c64m':     (160, 200),
                'spectrum': (256, 192), 'zx80': (256, 192), 'zx81': (256, 192), 'specnext': (320, 256),
                'msx':      (256, 192), 'cpc':      (160, 200),
                'cpc1':     (320, 200), 'atari_st': (320, 200),
                'amiga':    (320, 256), 'amiga_aga':(320, 256),
                'plus4':    (320, 200), 'vic20':    (176, 184),
            }
            if plat in plat_res:
                pw, ph = plat_res[plat]
                img = img.resize((pw, ph), Image.LANCZOS)
                self._canvas_width  = pw
                self._canvas_height = ph
            # Snap to platform palette then apply cell constraints
            img = self._snap_image_to_platform_palette(img)
            self._canvas_bit_depth = 3

        elif mode == 'texture':
            # Snap to nearest power-of-2 size
            import math
            pw = 2 ** round(math.log2(max(w, 1)))
            ph = 2 ** round(math.log2(max(h, 1)))
            if pw != w or ph != h:
                img = img.resize((pw, ph), Image.LANCZOS)
                self._canvas_width  = pw
                self._canvas_height = ph
            # Apply current bit depth
            depth = self._canvas_bit_depth
            if depth == 2:   # 16-bit
                rgb = img.convert('RGB'); px = rgb.tobytes()
                buf = bytearray(len(px))
                for i in range(0, len(px), 3):
                    buf[i]  = (px[i]   >> 3) << 3
                    buf[i+1]= (px[i+1] >> 2) << 2
                    buf[i+2]= (px[i+2] >> 3) << 3
                img = Image.frombytes('RGB',(pw,ph),bytes(buf)).convert('RGBA')
            elif depth == 3: # 8-bit
                img = img.convert('RGB').quantize(colors=256).convert('RGB').convert('RGBA')

        elif mode == 'icon':
            # Snap to nearest standard icon size
            ICON_SIZES = [16, 32, 48, 64, 128, 256]
            best = min(ICON_SIZES, key=lambda s: abs(s - max(w, h)))
            if best != w or best != h:
                img = img.resize((best, best), Image.LANCZOS)
                self._canvas_width  = best
                self._canvas_height = best

        self.map_canvas.tex_w = self._canvas_width
        self.map_canvas.tex_h = self._canvas_height
        self.map_canvas.rgba  = bytearray(img.tobytes())
        self.map_canvas.update()
        self._fit_canvas_to_viewport()


    def _toggle_cell_grid(self): #vers 1
        if not self.map_canvas: return
        self.map_canvas.show_cell_grid = not self.map_canvas.show_cell_grid
        self.map_canvas.update()


    def _toggle_statusbar(self, on: bool): #vers 1
        self.map_settings.set('show_statusbar', on)
        self.map_settings.save()
        if hasattr(self, '_status_bar'):
            self._status_bar.setVisible(on)


    def _zoom_mode_menu(self, pos): #vers 1
        """Right-click context menu on zoom gadget — select zoom mode."""
        menu = QMenu(self)
        modes = [
            ('in',  '🔍  Zoom In      (click to zoom in 2×)'),
            ('out', '🔎  Zoom Out     (click to zoom out ½×)'),
            ('box', '⬚   Box Zoom     (drag to zoom to selection)'),
            ('fit', '⊡   Zoom to Fit  (click to fit canvas in view)'),
        ]
        current = getattr(self.map_canvas, '_zoom_mode', 'in') if self.map_canvas else 'in'
        for mode_id, label in modes:
            a = menu.addAction(label)
            a.setCheckable(True)
            a.setChecked(mode_id == current)
            a.triggered.connect(lambda _, m=mode_id: self._set_zoom_mode(m))
        w = self.sender()   # the actual QToolButton (customContextMenuRequested
                             # sender) - _tool_btns now holds QActions, which
                             # have no mapToGlobal().
        menu.exec(w.mapToGlobal(pos) if w else self.cursor().pos())


    def _set_zoom_mode(self, mode: str): #vers 1
        """Set the zoom tool sub-mode and update tooltip."""
        if self.map_canvas:
            self.map_canvas._zoom_mode = mode
        labels = {'in':'Zoom In','out':'Zoom Out','box':'Box Zoom','fit':'Fit'}
        tips = {
            'in':  'Zoom In — click to zoom in 2×\nRight-click to change mode',
            'out': 'Zoom Out — click to zoom out ½×\nRight-click to change mode',
            'box': 'Box Zoom — drag a rectangle to zoom to it\nRight-click to change mode',
            'fit': 'Zoom to Fit — click to fit canvas in view\nRight-click to change mode',
        }
        btn = self._tool_btns.get(TOOL_ZOOM)
        if btn:
            btn.setToolTip(tips[mode])
        self._set_status(f"Zoom mode: {labels[mode]}")
        # Also activate the zoom tool
        self._select_tool(TOOL_ZOOM, from_button_click=False)

    def _show_tool_settings_menu(self, pos, tool_id): #vers 1
        """Right-click context menu on a tool's ribbon button - shows
        adjustable spinboxes for that tool's per-tool parameters
        (brush size plus whichever intensity/strength/density setting
        applies). Follows the same context-menu-on-toolbutton pattern
        as _zoom_mode_menu. Covers Blur/Smudge/Line/Eraser/Airbrush/
        Spraycan/Lighten/Darken/Sharpen - the brush settings Keith
        asked for."""
        from PyQt6.QtWidgets import QWidgetAction, QDoubleSpinBox
        c = self.map_canvas
        if not c:
            return
        menu = QMenu(self)

        def _add_spin_row(label, get_fn, set_fn, minv, maxv,
                          is_float=False, step=1, decimals=2):
            row_w = QWidget()
            row = QHBoxLayout(row_w)
            row.setContentsMargins(8, 3, 8, 3)
            row.addWidget(QLabel(label))
            if is_float:
                spin = QDoubleSpinBox()
                spin.setSingleStep(step)
                spin.setDecimals(decimals)
            else:
                spin = QSpinBox()
            spin.setRange(minv, maxv)
            spin.setValue(get_fn())
            spin.setFixedWidth(70)
            spin.valueChanged.connect(set_fn)
            row.addWidget(spin)
            wa = QWidgetAction(menu)
            wa.setDefaultWidget(row_w)
            menu.addAction(wa)

        # Brush size applies to nearly all of these (radius, line
        # thickness, eraser size)
        _add_spin_row("Brush Size:", lambda: c.brush_size,
                     lambda v: setattr(c, 'brush_size', v), 1, 50)

        if tool_id == TOOL_BLUR_BRUSH:
            _add_spin_row("Intensity:", lambda: c.blur_intensity,
                         lambda v: setattr(c, 'blur_intensity', v), 1, 10)
        elif tool_id == TOOL_SMUDGE:
            _add_spin_row("Strength:", lambda: c.smudge_strength,
                         lambda v: setattr(c, 'smudge_strength', v),
                         0.1, 0.9, is_float=True, step=0.05)
        elif tool_id in (TOOL_LIGHTEN, TOOL_DARKEN):
            _add_spin_row("Strength:", lambda: c.dodge_burn_amount,
                         lambda v: setattr(c, 'dodge_burn_amount', v), 5, 100)
        elif tool_id == TOOL_SHARPEN:
            _add_spin_row("Amount:", lambda: c.sharpen_amount,
                         lambda v: setattr(c, 'sharpen_amount', v),
                         0.1, 3.0, is_float=True, step=0.1)
        elif tool_id == TOOL_SPRAY:
            _add_spin_row("Density:", lambda: c.spray_density,
                         lambda v: setattr(c, 'spray_density', v), 1, 30)
        elif tool_id == TOOL_SPRAYCAN:
            _add_spin_row("Density:", lambda: c.spraycan_density,
                         lambda v: setattr(c, 'spraycan_density', v), 1, 30)
            _add_spin_row("Particle Size:", lambda: c.spraycan_particle_size,
                         lambda v: setattr(c, 'spraycan_particle_size', v), 1, 8)
            _add_spin_row("Hardness:", lambda: c.spraycan_hardness,
                         lambda v: setattr(c, 'spraycan_hardness', v), 5, 100)

        w = self.sender()
        menu.exec(w.mapToGlobal(pos) if w else self.cursor().pos())


    #    Colour Clash Visualiser

    def _toggle_clash_visualiser(self, on: bool): #vers 1
        """Toggle ZX Spectrum colour clash overlay — red = more than 2 colours in 8×8 cell."""
        if self.map_canvas:
            self.map_canvas._show_clash = on
            self.map_canvas.update()
        if on:
            self._set_status("Clash visualiser ON — red cells have >2 colours")
        else:
            self._set_status("Clash visualiser OFF")


    #    Character / Font Editor

    def _open_char_editor(self): #vers 2
        """Toggle character/font editor floating panel."""
        if not hasattr(self, "_char_editor_panel") or \
                not self._char_editor_panel.isVisible():
            self._char_editor_panel = _CharFontEditor(self)
            # Position to right of DP5 window
            pos = self.mapToGlobal(self.rect().topRight())
            self._char_editor_panel.move(pos.x() + 6, pos.y())
            self._char_editor_panel.show()
            self._char_editor_panel.raise_()
        else:
            self._char_editor_panel.hide()


    #    Sprite Editor

    def _open_sprite_editor(self): #vers 2
        """Toggle sprite editor floating panel."""
        if not hasattr(self, "_sprite_editor_panel") or \
                not self._sprite_editor_panel.isVisible():
            self._sprite_editor_panel = _SpriteEditor(self)
            pos = self.mapToGlobal(self.rect().topRight())
            self._sprite_editor_panel.move(pos.x() + 6, pos.y() + 60)
            self._sprite_editor_panel.show()
            self._sprite_editor_panel.raise_()
        else:
            self._sprite_editor_panel.hide()


    def _open_icon_editor(self): #vers 1
        """Toggle icon editor floating panel."""
        if not hasattr(self, "_icon_editor_panel") or \
                not self._icon_editor_panel.isVisible():
            self._icon_editor_panel = _IconEditor(self)
            pos = self.mapToGlobal(self.rect().topRight())
            self._icon_editor_panel.move(pos.x() + 6, pos.y() + 120)
            self._icon_editor_panel.show()
            self._icon_editor_panel.raise_()
        else:
            self._icon_editor_panel.hide()

    def _toggle_onion_skin(self, on: bool): #vers 1
        if self.map_canvas:
            self.map_canvas.onion_skin = on
            self.map_canvas.onion_rgba = (
                bytearray(self._frames[max(0, self._current_frame-1)])
                if on and len(self._frames) > 1 else None)
            self.map_canvas.update()

    def _toggle_colour_constraints(self): #vers 1
        """Toggle enforcement of per-cell colour limits for current platform."""
        self._enforce_constraints = not getattr(self, '_enforce_constraints', False)
        self._set_status(f"Colour constraints: {'ON' if self._enforce_constraints else 'OFF'}")

    def _place_text_at(self, tx: int, ty: int): #vers 5
        """Show inline text input overlay on canvas at click position,
        plus (if not already shown) a corner panel with font/size/
        colour controls and a Close button that ends the writing
        session."""
        if not self.map_canvas: return
        ed = self
        z = self.map_canvas.zoom
        zx = z * getattr(self.map_canvas, 'pixel_aspect_x', 1.0)

        # Create floating text entry if not already visible
        if hasattr(self, '_text_overlay') and self._text_overlay and \
                self._text_overlay.isVisible():
            self._text_overlay.close()

        # Reuse the corner panel across multiple text placements rather
        # than rebuilding it each time - its font/size/colour settings
        # persist for the next piece of text too.
        if not hasattr(self, '_text_corner_panel') or self._text_corner_panel is None:
            panel = _TextToolCornerPanel(self)
            panel.closed.connect(lambda: panel.hide())
            self._text_corner_panel = panel
        panel = self._text_corner_panel

        overlay = _CanvasTextOverlay(self, tx, ty, z, self.map_canvas, panel)
        self._text_overlay = overlay
        # Position the overlay widget over the canvas at the click point.
        # _CanvasTextOverlay sets Qt.WindowType.Tool, making it a real
        # top-level window even though it has a parent - its .move()
        # therefore expects GLOBAL screen coordinates, not coordinates
        # relative to self (the main window). canvas_widget.mapTo(self,
        # local_point) only gets us as far as self's own local space, so
        # that has to go through self.mapToGlobal() too before it's a
        # valid position to pass to a top-level window's move(). This
        # was the actual bug: passing a self-local point straight to
        # move() only looked correct when the main window happened to
        # sit at screen (0,0) - anywhere else, the overlay appeared
        # offset by exactly the main window's own screen position.
        canvas_widget = self.map_canvas
        px = int(tx * zx)
        py = int(ty * z)
        local_point = canvas_widget.mapTo(self, QPoint(px, py))
        overlay.move(self.mapToGlobal(local_point))
        overlay.show()
        overlay.activateWindow()
        overlay._edit.setFocus()

        # Position the corner panel at the top-right of the canvas
        # viewport, not at the text click position - stays put across
        # multiple text placements so the controls don't jump around.
        # Same global-coordinate fix applies here (also Qt.WindowType.Tool).
        sa = getattr(self, '_canvas_scroll', None)
        viewport = sa.viewport() if sa else canvas_widget
        vp_top_right_local = viewport.mapTo(self, QPoint(viewport.width(), 0))
        vp_top_right = self.mapToGlobal(vp_top_right_local)
        panel.adjustSize()
        panel.move(vp_top_right.x() - panel.width() - 8, vp_top_right.y() + 8)
        panel.show()
        panel.raise_()

    def _toggle_brush_manager(self): #vers 1
        """Show/hide the brush manager as a floating panel."""
        if not hasattr(self, '_brush_mgr_panel'):
            self._brush_mgr_panel = BrushManager(self)
            self._brush_mgr_panel.setWindowFlags(
                Qt.WindowType.Tool | Qt.WindowType.WindowStaysOnTopHint)
            self._brush_mgr_panel.brush_selected.connect(self._on_brush_mgr_selected)

        if self._brush_mgr_panel.isVisible():
            self._brush_mgr_panel.hide()
        else:
            # Position near toolbar
            pos = self.mapToGlobal(self.toolbar.pos())
            self._brush_mgr_panel.move(pos.x(), pos.y() + self.toolbar.height() + 4)
            self._brush_mgr_panel.resize(220, 320)
            self._brush_mgr_panel.show()
            self._brush_mgr_panel.raise_()

    def _activate_stamp_mode(self): #vers 1
        """Switch to stamp tool so user clicks anywhere to place the buffer."""
        if not self.map_canvas or not self.map_canvas._sel_buffer:
            return
        self._select_tool(TOOL_STAMP, from_button_click=False)
        self._set_status("Stamp mode — click to place, press Esc to exit")

    def _sync_brush_thumb(self): #vers 1
        """Update the brush thumbnail from the current copy buffer."""
        if not hasattr(self, '_brush_thumb') or not self.map_canvas:
            return
        c = self.map_canvas
        self._brush_thumb.set_buffer(c._sel_buffer, c._sel_buf_w, c._sel_buf_h)

    def _cut_selection(self): #vers 1
        if not self.map_canvas: return
        self._push_undo()
        self.map_canvas.cut_selection()
        self._sync_brush_thumb()
        self._set_status("Selection cut — click Brush thumbnail to stamp")

    def _copy_selection(self): #vers 1
        if not self.map_canvas: return
        self.map_canvas.copy_selection()
        self._sync_brush_thumb()
        self._set_status("Selection copied — click Brush thumbnail to stamp")

    def _paste_selection(self): #vers 1
        """Paste: activate stamp mode so user clicks to place."""
        if not self.map_canvas: return
        c = self.map_canvas
        if c._sel_buffer and c._sel_buf_w:
            self._activate_stamp_mode()
            self._set_status("Click anywhere to stamp — Esc to exit stamp mode")
        else:
            self._set_status("Nothing to paste")

    def _select_all(self):  #vers 1
        if not self.map_canvas: return
        self.map_canvas._selection_rect = QRect(0, 0,
                                                self._canvas_width,
                                                self._canvas_height)
        self.map_canvas._sel_active = True
        self.map_canvas.update()
        self._select_tool(TOOL_SELECT, from_button_click=False)
        self._set_status("All selected")

    def _rotate_selection_dialog(self): #vers 2
        """Rotate the active selection — inline overlay panel."""
        if not self.map_canvas: return
        c = self.map_canvas
        if not (c._sel_active and c._selection_rect):
            self._set_status("No selection to rotate — use Select tool first")
            return
        if not c._sel_buffer:
            c.copy_selection()
        if not c._sel_buffer:
            return

        from PyQt6.QtWidgets import (QWidget, QVBoxLayout, QHBoxLayout,
            QPushButton, QDoubleSpinBox, QLabel, QSlider, QCheckBox)

        ctrl = QWidget()
        cl = QVBoxLayout(ctrl); cl.setContentsMargins(0,0,0,0); cl.setSpacing(4)

        cl.addWidget(QLabel("Rotation angle (°):"))
        deg_spin = QDoubleSpinBox()
        deg_spin.setRange(-359.9, 359.9); deg_spin.setDecimals(1)
        deg_spin.setSingleStep(1.0); deg_spin.setValue(0.0); deg_spin.setSuffix("°")
        cl.addWidget(deg_spin)

        sl = QSlider(Qt.Orientation.Horizontal)
        sl.setRange(-180, 180); sl.setValue(0)
        sl.valueChanged.connect(lambda v: deg_spin.setValue(float(v)))
        deg_spin.valueChanged.connect(lambda v: sl.setValue(int(v)))
        cl.addWidget(sl)

        preset_row = QHBoxLayout()
        for label, angle in [("−90°",-90),("−45°",-45),("+45°",45),("+90°",90),("180°",180)]:
            b = QPushButton(label); b.setFixedHeight(24)
            b.clicked.connect(lambda _=False, a=angle: deg_spin.setValue(float(a)))
            preset_row.addWidget(b)
        cl.addLayout(preset_row)

        expand_cb = QCheckBox("Expand to fit")
        expand_cb.setChecked(True)
        cl.addWidget(expand_cb)

        parent_vp = self._canvas_scroll.viewport() if hasattr(self, '_canvas_scroll') else self

        def _apply():  #vers 1
            angle = deg_spin.value()
            if angle != 0.0:
                self._apply_selection_rotation(angle, expand=expand_cb.isChecked())
                self._set_status(f"Selection rotated {angle}°")

        self._ToolOverlay(parent_vp, self, "Rotate Selection",
                          ctrl, apply_fn=_apply, generate_fn=None)

    def _apply_selection_rotation(self, angle: float, expand: bool = True): #vers 1
        """Rotate the selection buffer by angle degrees (CCW positive).
        Updates _sel_buffer and re-floats the selection."""
        c = self.map_canvas
        if not c._sel_buffer or c._sel_buf_w <= 0:
            return
        try:
            from PIL import Image
            w, h = c._sel_buf_w, c._sel_buf_h
            img = Image.frombytes('RGBA', (w, h), bytes(c._sel_buffer))
            # PIL rotate: positive = CCW; we want CW for consistency with _rotate_90_cw
            rotated = img.rotate(-angle, expand=expand,
                                 resample=Image.Resampling.BILINEAR,
                                 fillcolor=(0, 0, 0, 0))
            nw, nh = rotated.size
            c._sel_buffer = bytearray(rotated.tobytes())
            c._sel_buf_w  = nw
            c._sel_buf_h  = nh
            # Float the selection at its original position
            if c._selection_rect and not c._sel_floating:
                ox = c._selection_rect.x() + (c._selection_rect.width()  - nw) // 2
                oy = c._selection_rect.y() + (c._selection_rect.height() - nh) // 2
                c._sel_float_pos = (max(0, ox), max(0, oy))
            c._sel_floating = True
            c._sel_active   = True
            c._selection_rect = None   # float replaces rect
            c.update()
            self._set_status(f"Selection rotated {angle:+.1f}°  —  stamp with TOOL_MOVE or ✓")
        except ImportError:
            self._set_status("PIL (Pillow) not available — install with: pip install Pillow")
        except Exception as e:
            self._set_status(f"Rotate error: {e}")


    def _deselect(self): #vers 1
        if not self.map_canvas: return
        c = self.map_canvas
        if c._sel_floating:
            c._stamp_selection(keep_floating=False)
        c._sel_active        = False
        c._sel_floating      = False
        c._sel_drag_start    = None
        c._selection_rect    = None
        c.update()

    def _set_polygon_sides(self): #vers 1
        if not self.map_canvas: return
        n, ok = QInputDialog.getInt(self, "Polygon", "Number of sides:", 6, 3, 32)
        if ok:
            self.map_canvas._polygon_sides = n
            self._set_status(f"Polygon: {n} sides")


    #    Canvas signal callbacks

    def _on_canvas_changed(self, x: int, y: int): #vers 1
        if self.map_canvas:
            self._update_status(x, y, self.map_canvas.get_pixel(x, y))
            if self._enforce_constraints and self._platform_mode != 'none':
                # Debounce — only apply constraint after mouse settles
                if not hasattr(self, '_constraint_timer'):
                    from PyQt6.QtCore import QTimer
                    self._constraint_timer = QTimer()
                    self._constraint_timer.setSingleShot(True)
                    self._constraint_timer.timeout.connect(self._apply_pending_constraint)
                self._constraint_pending = (x, y)
                self._constraint_timer.start(80)  # 80ms debounce

    def _nearest_in_palette(self, r: int, g: int, b: int, palette: list) -> tuple: #vers 1
        return min(palette, key=lambda c:(c[0]-r)**2+(c[1]-g)**2+(c[2]-b)**2)


    def _nearest_zx_colour(self, r: int, g: int, b: int) -> tuple: #vers 1
        return self._nearest_in_palette(r, g, b, self._ZX_PALETTE)


    def _snap_cell_to_palette(self, cx, cy, cw, ch, w, h, palette): #vers 1
        """Snap all pixels in cell to nearest colour from given palette."""
        for dy in range(ch):
            for dx in range(cw):
                tx, ty = cx+dx, cy+dy
                if not (0 <= tx < w and 0 <= ty < h): continue
                i = (ty*w+tx)*4
                r,g,b = self.map_canvas.rgba[i:i+3]
                best = self._nearest_in_palette(r, g, b, palette)
                self.map_canvas.rgba[i:i+3] = list(best)


    def _limit_cell_colours(self, cx, cy, cw, ch, w, h, max_c): #vers 1
        """After palette snap, enforce max_c colours per cell."""
        colours = {}
        for dy in range(ch):
            for dx in range(cw):
                tx, ty = cx+dx, cy+dy
                if not (0 <= tx < w and 0 <= ty < h): continue
                i = (ty*w+tx)*4
                key = tuple(self.map_canvas.rgba[i:i+3])
                colours[key] = colours.get(key, 0) + 1
        if len(colours) <= max_c: return
        kept = sorted(colours, key=lambda k: -colours[k])[:max_c]
        for dy in range(ch):
            for dx in range(cw):
                tx, ty = cx+dx, cy+dy
                if not (0 <= tx < w and 0 <= ty < h): continue
                i = (ty*w+tx)*4
                key = tuple(self.map_canvas.rgba[i:i+3])
                if key not in kept:
                    best = min(kept, key=lambda k:(k[0]-key[0])**2+(k[1]-key[1])**2+(k[2]-key[2])**2)
                    self.map_canvas.rgba[i:i+3] = list(best)


    def _get_user_palette_rgb(self): #vers 1
        """Return current user palette as list of (r,g,b) tuples, or None if empty."""
        if not hasattr(self, '_user_pal_grid'): return None
        colors = getattr(self._user_pal_grid, '_colors', [])
        if not colors: return None
        return [(c.red(), c.green(), c.blue()) for c in colors if c.isValid()]


    def _snap_image_to_user_palette(self, img): #vers 1
        """Snap every pixel in a PIL RGBA image to nearest colour in the user palette."""
        from PIL import Image
        palette = self._get_user_palette_rgb()
        if not palette:
            QMessageBox.warning(self, "Snap to User Palette",
                                "No user palette loaded. Load a palette first.")
            return img
        n = min(len(palette), 256)
        pal_img = Image.new('P', (1, 1))
        flat = []
        for r, g, b in palette[:n]:
            flat += [r, g, b]
        flat += [0] * (768 - len(flat))
        pal_img.putpalette(flat)
        return img.convert('RGB').quantize(palette=pal_img, dither=0).convert('RGB').convert('RGBA')


    #    Render As

    def _render_as_ascii(self): #vers 1
        """Convert canvas to ASCII art — map brightness to characters, render back to canvas."""
        if not self.map_canvas: return
        from PIL import Image, ImageDraw, ImageFont

        # ASCII ramp — dark to light
        RAMP = ' .\'`^",:;Il!i><~+_-?][}{1)(|/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$'

        cols, ok = QInputDialog.getInt(self, "ASCII Art", "Characters wide:", 80, 10, 320)
        if not ok: return

        self._push_undo()
        from PIL import Image
        src = Image.frombytes('RGBA',(self._canvas_width,self._canvas_height),
                              bytes(self.map_canvas.rgba)).convert('L')

        char_w, char_h = 6, 10   # monospace character size in pixels
        rows = int(cols * (self._canvas_height / self._canvas_width) * (char_w / char_h))
        rows = max(4, rows)

        # Resize to character grid
        small = src.resize((cols, rows), Image.LANCZOS)
        pixels = list(small.getdata())

        # Map brightness to character
        chars = []
        for px in pixels:
            idx = int(px / 255 * (len(RAMP)-1))
            chars.append(RAMP[idx])

        # Render back to canvas as pixel art
        cell_pw = self._canvas_width  // cols
        cell_ph = self._canvas_height // rows
        cell_pw = max(1, cell_pw); cell_ph = max(1, cell_ph)

        out_w = cols * cell_pw
        out_h = rows * cell_ph
        out = Image.new('RGB', (out_w, out_h), (20, 20, 20))
        draw = ImageDraw.Draw(out)

        try:
            font = ImageFont.truetype("/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf", cell_ph)
        except Exception:
            font = ImageFont.load_default()

        for i, ch in enumerate(chars):
            col = i % cols; row = i // cols
            x = col * cell_pw; y = row * cell_ph
            # Brightness-based grey
            brightness = int(list(small.getdata())[i])
            grey = (brightness, brightness, brightness)
            draw.text((x, y), ch, fill=grey, font=font)

        out_rgba = out.convert('RGBA')
        self._canvas_width  = out_w
        self._canvas_height = out_h
        self.map_canvas.tex_w = out_w
        self.map_canvas.tex_h = out_h
        self.map_canvas.rgba  = bytearray(out_rgba.tobytes())
        self.map_canvas.update()
        self._fit_canvas_to_viewport()
        self._set_status(f"ASCII art: {cols}×{rows} chars → {out_w}×{out_h}px")

        # Offer text export
        if QMessageBox.question(self, "Export ASCII", "Export as text file?",
            QMessageBox.StandardButton.Yes|QMessageBox.StandardButton.No) == QMessageBox.StandardButton.Yes:
            path, _ = QFileDialog.getSaveFileName(self,"Save ASCII","art.txt","Text (*.txt)")
            if path:
                lines = [''.join(chars[r*cols:(r+1)*cols]) for r in range(rows)]
                open(path,'w').write('\n'.join(lines))


    def _render_as_ansi(self): #vers 1
        """Convert canvas to ANSI art using block chars █▌▐▄▀ with 16 ANSI colours."""
        if not self.map_canvas: return
        from PIL import Image

        cols, ok = QInputDialog.getInt(self, "ANSI Art", "Characters wide:", 80, 10, 320)
        if not ok: return

        self._push_undo()

        # ANSI 16-colour palette (standard terminal)
        ANSI_PAL = [
            (0,0,0),(170,0,0),(0,170,0),(170,170,0),
            (0,0,170),(170,0,170),(0,170,170),(170,170,170),
            (85,85,85),(255,85,85),(85,255,85),(255,255,85),
            (85,85,255),(255,85,255),(85,255,255),(255,255,255),
        ]

        # Block characters — each char cell is split top/bottom half
        # Use upper-half block '▀' with fg=top colour, bg=bottom colour
        # This gives 2 vertical pixels per character row

        char_w = 8; char_h = 8
        rows = max(4, int(cols * (self._canvas_height / self._canvas_width) * (char_w / char_h) * 2))

        src = Image.frombytes('RGBA',(self._canvas_width,self._canvas_height),
                              bytes(self.map_canvas.rgba)).convert('RGB')
        # Resize to cols × rows (each row = half-block = 2 pixel rows)
        small = src.resize((cols, rows), Image.LANCZOS)
        pixels = list(small.getdata())

        def nearest_ansi(r,g,b):  #vers 1
            return min(range(16), key=lambda i:(ANSI_PAL[i][0]-r)**2+(ANSI_PAL[i][1]-g)**2+(ANSI_PAL[i][2]-b)**2)

        # Build ANSI escape sequence string
        ansi_lines = []
        char_rows = rows // 2
        ansi_out = []
        for row in range(char_rows):
            line = ''
            ansi_row = []
            for col in range(cols):
                top = pixels[(row*2)*cols+col]
                bot = pixels[(row*2+1)*cols+col] if (row*2+1)*cols+col < len(pixels) else top
                fg = nearest_ansi(*top)
                bg = nearest_ansi(*bot)
                ansi_row.append((fg, bg))
                # ANSI escape: \033[38;5;{fg}m\033[48;5;{bg}m▀
                line += f'\033[38;5;{fg}m\033[48;5;{bg}m▀'
            line += '\033[0m'
            ansi_lines.append(line)
            ansi_out.append(ansi_row)

        # Render to canvas
        from PIL import ImageDraw, ImageFont
        cell_ph = max(4, self._canvas_height // char_rows)
        cell_pw = max(4, self._canvas_width  // cols)
        out_w = cols * cell_pw
        out_h = char_rows * cell_ph
        out = Image.new('RGB', (out_w, out_h), (0,0,0))

        for row, row_data in enumerate(ansi_out):
            for col, (fg, bg) in enumerate(row_data):
                x = col * cell_pw; y = row * cell_ph
                top_c = ANSI_PAL[fg]; bot_c = ANSI_PAL[bg]
                # Top half
                for py in range(cell_ph//2):
                    for px in range(cell_pw):
                        out.putpixel((x+px, y+py), top_c)
                # Bottom half
                for py in range(cell_ph//2, cell_ph):
                    for px in range(cell_pw):
                        out.putpixel((x+px, y+py), bot_c)

        out_rgba = out.convert('RGBA')
        self._canvas_width  = out_w
        self._canvas_height = out_h
        self.map_canvas.tex_w = out_w
        self.map_canvas.tex_h = out_h
        self.map_canvas.rgba  = bytearray(out_rgba.tobytes())
        self.map_canvas.update()
        self._fit_canvas_to_viewport()
        self._set_status(f"ANSI art: {cols}×{char_rows} chars → {out_w}×{out_h}px")

        if QMessageBox.question(self, "Export ANSI", "Export as .ans file?",
            QMessageBox.StandardButton.Yes|QMessageBox.StandardButton.No) == QMessageBox.StandardButton.Yes:
            path, _ = QFileDialog.getSaveFileName(self,"Save ANSI","art.ans","ANSI (*.ans)")
            if path:
                open(path,'w').write('\n'.join(ansi_lines)+'\n')


    def _render_as_petscii(self): #vers 1
        """Convert canvas to PETSCII block art using C64 16-colour palette and block chars."""
        if not self.map_canvas: return
        from PIL import Image

        cols, ok = QInputDialog.getInt(self, "PETSCII", "Characters wide (max 40):", 40, 10, 80)
        if not ok: return

        self._push_undo()

        C64_PAL = [
            (0,0,0),(255,255,255),(136,0,0),(170,255,238),
            (204,68,204),(0,204,85),(0,0,170),(238,238,119),
            (221,136,85),(102,68,0),(255,119,119),(51,51,51),
            (119,119,119),(170,255,102),(0,136,255),(187,187,187),
        ]

        # PETSCII block chars rendered as 2×2 sub-cell pixel blocks
        # Each char cell = 8×8px, split into 4 quadrants: TL TR BL BR
        # Characters: space=0000, ▘=1000, ▝=0100, ▀=1100,
        #             ▖=0010, ▌=1010, ▞=0110, ▛=1110,
        #             ▗=0001, ▚=1001, ▐=0101, ▜=1101,
        #             ▄=0011, ▙=1011, ▟=0111, █=1111

        BLOCKS = [' ','▘','▝','▀','▖','▌','▞','▛','▗','▚','▐','▜','▄','▙','▟','█']

        char_w = 8; char_h = 8
        rows = max(4, int(cols * (self._canvas_height / self._canvas_width)))

        src = Image.frombytes('RGBA',(self._canvas_width,self._canvas_height),
                              bytes(self.map_canvas.rgba)).convert('RGB')
        # Resize to cols*2 × rows*2 (2×2 sub-pixels per char)
        small = src.resize((cols*2, rows*2), Image.LANCZOS)
        pixels = list(small.getdata())
        sw = cols*2

        def nearest_c64(r,g,b):  #vers 1
            return min(range(16), key=lambda i:(C64_PAL[i][0]-r)**2+(C64_PAL[i][1]-g)**2+(C64_PAL[i][2]-b)**2)

        # For each char cell, pick dominant fg/bg from 4 sub-pixels
        cell_data = []  # (fg_idx, bg_idx, block_bits)
        for row in range(rows):
            for col in range(cols):
                # 4 sub-pixels: TL, TR, BL, BR
                quads = [
                    pixels[(row*2)*sw   + col*2],
                    pixels[(row*2)*sw   + col*2+1],
                    pixels[(row*2+1)*sw + col*2],
                    pixels[(row*2+1)*sw + col*2+1],
                ]
                idxs = [nearest_c64(*q) for q in quads]
                # Find 2 most common colours
                from collections import Counter
                counts = Counter(idxs)
                top2 = [c for c,_ in counts.most_common(2)]
                fg = top2[0]; bg = top2[1] if len(top2)>1 else fg
                # Assign bits: 1=fg, 0=bg for TL,TR,BL,BR
                bits = sum((1<<(3-i)) for i,idx in enumerate(idxs) if idx==fg)
                cell_data.append((fg, bg, bits))

        # Render to canvas
        cell_pw = char_w; cell_ph = char_h
        out_w = cols * cell_pw; out_h = rows * cell_ph
        out = Image.new('RGB', (out_w, out_h), (0,0,0))
        px_data = out.load()

        for row in range(rows):
            for col in range(cols):
                fg, bg, bits = cell_data[row*cols+col]
                fc = C64_PAL[fg]; bc = C64_PAL[bg]
                x0 = col*cell_pw; y0 = row*cell_ph
                hw = cell_pw//2; hh = cell_ph//2
                sub = [(bits>>3)&1,(bits>>2)&1,(bits>>1)&1,bits&1]
                quadrants = [(0,0,hw,hh),(hw,0,cell_pw,hh),(0,hh,hw,cell_ph),(hw,hh,cell_pw,cell_ph)]
                for (x1,y1,x2,y2),on in zip(quadrants,sub):
                    c = fc if on else bc
                    for py in range(y1,y2):
                        for px in range(x1,x2):
                            px_data[x0+px, y0+py] = c

        out_rgba = out.convert('RGBA')
        self._canvas_width  = out_w; self._canvas_height = out_h
        self.map_canvas.tex_w = out_w; self.map_canvas.tex_h = out_h
        self.map_canvas.rgba  = bytearray(out_rgba.tobytes())
        self.map_canvas.update()
        self._fit_canvas_to_viewport()
        self._set_status(f"PETSCII: {cols}×{rows} chars → {out_w}×{out_h}px")

        if QMessageBox.question(self, "Export PETSCII", "Export as C64 PRG?",
            QMessageBox.StandardButton.Yes|QMessageBox.StandardButton.No) == QMessageBox.StandardButton.Yes:
            path, _ = QFileDialog.getSaveFileName(self,"Save PETSCII PRG","petscii.prg","PRG (*.prg)")
            if path:
                # Screen RAM + colour RAM format (screen codes + colour)
                screen = bytearray(1000); colour = bytearray(1000)
                for i,(fg,bg,bits) in enumerate(cell_data[:1000]):
                    screen[i] = bits  # approximate PETSCII screen code
                    colour[i] = fg & 0xF
                # BASIC stub + screen data at $0400
                prg = b'\x01\x08' + bytearray(14) + b'\x00\x04' + screen + b'\xD8\x07' + colour
                open(path,'wb').write(prg)


    def _render_as_teletext(self): #vers 1
        """Convert canvas to Teletext mosaic block art (2×3 sub-blocks per cell, 8 colours)."""
        if not self.map_canvas: return
        from PIL import Image

        cols, ok = QInputDialog.getInt(self, "Teletext", "Characters wide (40=standard):", 40, 10, 80)
        if not ok: return

        self._push_undo()

        # Teletext 8 colours (RGB combinations)
        TT_PAL = [
            (0,0,0),(255,0,0),(0,255,0),(255,255,0),
            (0,0,255),(255,0,255),(0,255,255),(255,255,255),
        ]

        def nearest_tt(r,g,b):  #vers 1
            return min(range(8), key=lambda i:(TT_PAL[i][0]-r)**2+(TT_PAL[i][1]-g)**2+(TT_PAL[i][2]-b)**2)

        # Teletext mosaic: each char cell = 2×3 grid of sub-blocks
        # Cell size: 6×10 pixels (or 12×20 for 2x rendering)
        # 6 bits → 64 possible mosaic characters
        # Sub-block layout:
        #  [0][1]
        #  [2][3]
        #  [4][5]
        rows = max(4, int(cols * (self._canvas_height / self._canvas_width) * (2/3)))

        src = Image.frombytes('RGBA',(self._canvas_width,self._canvas_height),
                              bytes(self.map_canvas.rgba)).convert('RGB')
        small = src.resize((cols*2, rows*3), Image.LANCZOS)
        pixels = list(small.getdata())
        sw = cols*2

        cell_data = []  # (fg_idx, bits6)
        for row in range(rows):
            for col in range(cols):
                # 6 sub-pixels (2 wide × 3 tall)
                subs = [
                    pixels[(row*3+sr)*sw + col*2+sc]
                    for sr in range(3) for sc in range(2)
                ]
                idxs = [nearest_tt(*s) for s in subs]
                from collections import Counter
                counts = Counter(idxs)
                fg = counts.most_common(1)[0][0]
                # Bit = 1 if matches fg colour, 0 = background (black)
                bits = sum((1<<i) for i,idx in enumerate(idxs) if idx==fg)
                cell_data.append((fg, bits))

        # Render — cell pixel size
        cell_pw = 12; cell_ph = 20  # 2× for readability
        out_w = cols * cell_pw; out_h = rows * cell_ph
        out = Image.new('RGB', (out_w, out_h), (0,0,0))
        px_data = out.load()

        for row in range(rows):
            for col in range(cols):
                fg, bits = cell_data[row*cols+col]
                fc = TT_PAL[fg]; bc = (0,0,0)
                x0 = col*cell_pw; y0 = row*cell_ph
                bw = cell_pw//2; bh = cell_ph//3
                for si in range(6):
                    sr = si//2; sc = si%2
                    on = (bits>>si)&1
                    c = fc if on else bc
                    for py in range(bh):
                        for px in range(bw):
                            px_data[x0+sc*bw+px, y0+sr*bh+py] = c

        out_rgba = out.convert('RGBA')
        self._canvas_width  = out_w; self._canvas_height = out_h
        self.map_canvas.tex_w = out_w; self.map_canvas.tex_h = out_h
        self.map_canvas.rgba  = bytearray(out_rgba.tobytes())
        self.map_canvas.update()
        self._fit_canvas_to_viewport()
        self._set_status(f"Teletext: {cols}×{rows} chars → {out_w}×{out_h}px")

        if QMessageBox.question(self, "Export Teletext", "Export as .tti file?",
            QMessageBox.StandardButton.Yes|QMessageBox.StandardButton.No) == QMessageBox.StandardButton.Yes:
            path, _ = QFileDialog.getSaveFileName(self,"Save Teletext","page.tti","TTI (*.tti)")
            if path:
                # TTI format: simple text with colour codes
                lines = ['OL,100']  # page header
                TT_CODES = ['\\0','\\1','\\2','\\3','\\4','\\5','\\6','\\7']
                for row in range(rows):
                    row_cells = cell_data[row*cols:(row+1)*cols]
                    # Build teletext line with colour changes
                    line_str = ''
                    cur_fg = -1
                    for fg, bits in row_cells:
                        if fg != cur_fg:
                            line_str += TT_CODES[fg]
                            cur_fg = fg
                        # Map 6-bit mosaic to Unicode block char
                        mosaic_char = chr(0x23A0 + bits) if bits > 0 else ' '
                        line_str += mosaic_char
                    lines.append(f'OL,{100+row+1},{line_str}')
                open(path,'w',encoding='utf-8').write('\n'.join(lines)+'\n')


    def _snap_canvas_to_user_palette(self): #vers 3
        """Hard-snap entire canvas to current user palette (no dither)."""
        if not self.map_canvas: return
        palette = self._get_user_palette_rgb()
        if not palette:
            QMessageBox.warning(self, "Snap to Palette", "No user palette loaded.")
            return
        self._push_undo()
        from PIL import Image
        img = Image.frombytes('RGBA', (self._canvas_width, self._canvas_height),
                              bytes(self.map_canvas.rgba))
        snapped = self._snap_image_to_user_palette(img)
        self.map_canvas.rgba = bytearray(snapped.tobytes())
        self.map_canvas.update()
        self._set_status(f"Snapped to palette: {len(palette)} colours")


    def _snap_canvas_to_user_palette_dither(self): #vers 1
        """Snap canvas to user palette with dither — asks which method."""
        palette = self._get_user_palette_rgb()
        if not palette:
            QMessageBox.warning(self, "Snap + Dither", "No user palette loaded.")
            return
        # Quick pick via input dialog
        method, ok = QInputDialog.getItem(
            self, "Dither Method", "Choose dither:",
            ["Floyd-Steinberg", "Bayer 4×4", "Checkerboard"], 0, False)
        if not ok: return
        mode_map = {"Floyd-Steinberg": "floyd", "Bayer 4×4": "bayer", "Checkerboard": "checker"}
        mode = mode_map[method]
        self._push_undo()
        old_mode = getattr(self, '_pal_dither_mode', 'off')
        self._pal_dither_mode = mode
        from PIL import Image
        img = Image.frombytes('RGBA', (self._canvas_width, self._canvas_height),
                              bytes(self.map_canvas.rgba))
        snapped = self._apply_user_palette_dither(img)
        self._pal_dither_mode = old_mode
        self.map_canvas.rgba = bytearray(snapped.tobytes())
        self.map_canvas.update()
        self._set_status(f"Snapped ({mode}): {len(palette)} colours")
        """Snap entire canvas to current user palette, with optional dithering."""
        if not self.map_canvas: return
        palette = self._get_user_palette_rgb()
        if not palette:
            QMessageBox.warning(self, "Snap to User Palette", "No user palette loaded.")
            return
        self._push_undo()
        from PIL import Image
        img = Image.frombytes('RGBA', (self._canvas_width, self._canvas_height),
                              bytes(self.map_canvas.rgba))
        mode = getattr(self, '_pal_dither_mode', 'off')
        if mode != 'off':
            snapped = self._apply_user_palette_dither(img)
        else:
            snapped = self._snap_image_to_user_palette(img)
        self.map_canvas.rgba = bytearray(snapped.tobytes())
        self.map_canvas.update()
        self._set_status(f"Snapped to user palette ({len(palette)} colours, dither:{mode})")


    def _snap_image_to_platform_palette(self, img): #vers 1
        """Snap every pixel in a PIL RGBA image to the nearest platform palette colour.
        Returns the modified image. Used when loading an image in platform mode."""
        mode = self._platform_mode
        # Get the platform palette as a flat list of (r,g,b) tuples
        pal_map = {
            'c64':       self._C64_PALETTE,
            'c64m':      self._C64_PALETTE,
            'spectrum': self._ZX_PALETTE,
            'zx80':        'threshold_bw',
            'zx81':        'bayer_bw',
            'timex_hi':    'threshold_bw',  # HiRes mode is B&W
            'jupiter':     'threshold_bw',  # Jupiter Ace is B&W
            'specnext':  None,   # 256 colour — no snap needed
            'msx':       self._MSX_PALETTE,
            'cpc':       self._CPC_PALETTE,
            'cpc1':      self._CPC_PALETTE,
            'atari_st':  self._ATARI_ST_PALETTE,
            'atari_800': self._ATARI_800_PALETTE,
            'amiga':     [(0,0,0),(255,255,255),(170,0,0),(85,255,255),
                          (170,0,170),(85,255,85),(0,0,170),(255,255,85),
                          (170,85,0),(85,85,0),(255,119,119),(85,85,85),
                          (119,119,119),(170,255,170),(85,136,255),(170,170,170),
                          (0,0,0),(17,17,17),(34,34,34),(51,51,51),
                          (68,68,68),(85,85,85),(102,102,102),(119,119,119),
                          (136,136,136),(153,153,153),(170,170,170),(187,187,187),
                          (204,204,204),(221,221,221),(238,238,238),(255,255,255)],
            'amiga_aga': 'user',   # 256 colour — snap to user palette if loaded
            'amiga_ham': None,   # handled by HAM constraint
            'amiga_ham8':None,
            'amiga_rtg': None,
            'plus4':     self._C64_PALETTE,
            'vic20':     self._C64_PALETTE,
        }
        palette = pal_map.get(mode)
        if palette is None:
            return img   # no snap for full-colour modes
        if palette == 'user':
            # Use current user palette
            user_pal = self._get_user_palette_rgb()
            if not user_pal: return img
            palette = user_pal
        if palette == 'bayer_bw':
            from PIL import Image as PILImage
            BAYER = [[0,8,2,10],[12,4,14,6],[3,11,1,9],[15,7,13,5]]
            rgb = img.convert('RGB')
            w2, h2 = rgb.size
            out = PILImage.new('RGB', (w2, h2))
            px_in = rgb.load(); px_out = out.load()
            for y in range(h2):
                for x in range(w2):
                    r, g, b = px_in[x, y]
                    lum = int(0.299*r + 0.587*g + 0.114*b)
                    thresh = int(BAYER[y%4][x%4] / 16.0 * 255)
                    v = 255 if lum > thresh else 0
                    px_out[x, y] = (v, v, v)
            return out.convert('RGBA')
        if palette == 'threshold_bw':
            # ZX80: hard threshold — average per 8×8 block then B&W
            from PIL import Image as PILImage
            rgb = img.convert('L')  # greyscale
            out = PILImage.new('L', rgb.size)
            px = rgb.load(); po = out.load()
            w2, h2 = rgb.size
            for y in range(h2):
                for x in range(w2):
                    po[x, y] = 255 if px[x, y] >= 128 else 0
            return out.convert('RGBA')

        from PIL import Image
        rgb = img.convert('RGB')
        px = list(rgb.getdata())
        w, h = rgb.size

        # Build a PIL palette image for fast quantization to platform colours
        n = len(palette)
        pal_img = Image.new('P', (1, 1))
        flat = []
        for r,g,b in palette:
            flat += [r,g,b]
        flat += [0] * (768 - len(flat))
        pal_img.putpalette(flat)

        # Quantize to platform palette (nearest colour, no dither)
        snapped = rgb.quantize(palette=pal_img, dither=0).convert('RGB').convert('RGBA')
        return snapped


    def _update_status(self, x: int, y: int, colour: QColor): #vers 2
        zoom = self._canvas_zoom
        tool = getattr(self.map_canvas, 'tool', '?') if self.map_canvas else '?'
        self._set_status(
            f"Canvas: {self._canvas_width}×{self._canvas_height}  |  "
            f"Pos: {x},{y}  |  "
            f"RGBA({colour.red()},{colour.green()},{colour.blue()},{colour.alpha()})  |  "
            f"Zoom: {zoom}×  |  Tool: {tool}")


    def _update_zoom_label(self): #vers 1
        if self.map_canvas:
            self._canvas_zoom = self.map_canvas.zoom
        if hasattr(self, '_zoom_lbl'):
            z = self._canvas_zoom
            self._zoom_lbl.setText(f"{int(z)}×" if z >= 1 else f"{z:.2f}×")

    #    Canvas operations                                                      


    def _push_undo(self):  #vers 3
        if self.map_canvas: #vers 1
            self._undo_stack.append(
                (bytes(self.map_canvas.rgba), self._canvas_width, self._canvas_height,
                 self.map_canvas.pixel_aspect_x))
            self._redo_stack.clear()


    def _undo_canvas(self): #vers 3
        if self.map_canvas and self._undo_stack:
            self._redo_stack.append(
                (bytes(self.map_canvas.rgba), self._canvas_width, self._canvas_height,
                 self.map_canvas.pixel_aspect_x))
            rgba, w, h, pa = self._undo_stack.pop()
            size_changed = (w, h) != (self._canvas_width, self._canvas_height) or \
                           pa != self.map_canvas.pixel_aspect_x
            self._canvas_width, self._canvas_height = w, h
            self.map_canvas.tex_w, self.map_canvas.tex_h = w, h
            self.map_canvas.pixel_aspect_x = pa
            self.map_canvas.rgba[:] = rgba
            self.map_canvas.update()
            if size_changed:
                self._fit_canvas_to_viewport()
            self._set_status(f"Undo  ({w}×{h})")


    def _redo_canvas(self): #vers 3
        if self.map_canvas and self._redo_stack:
            self._undo_stack.append(
                (bytes(self.map_canvas.rgba), self._canvas_width, self._canvas_height,
                 self.map_canvas.pixel_aspect_x))
            rgba, w, h, pa = self._redo_stack.pop()
            size_changed = (w, h) != (self._canvas_width, self._canvas_height) or \
                           pa != self.map_canvas.pixel_aspect_x
            self._canvas_width, self._canvas_height = w, h
            self.map_canvas.tex_w, self.map_canvas.tex_h = w, h
            self.map_canvas.pixel_aspect_x = pa
            self.map_canvas.rgba[:] = rgba
            self.map_canvas.update()
            if size_changed:
                self._fit_canvas_to_viewport()
            self._set_status(f"Redo  ({w}×{h})")


    #    Multi-canvas tabs                                                       

    def _capture_canvas_tab_state(self): #vers 1
        """Snapshot everything needed to fully restore the currently
        active canvas later: image data, dimensions, pixel aspect, and
        this workshop's undo/redo stacks (which are shared/global, not
        per-canvas-widget, so they must travel with the tab)."""
        if not self.map_canvas:
            return None
        return {
            'rgba': bytearray(self.map_canvas.rgba),
            'tex_w': self.map_canvas.tex_w,
            'tex_h': self.map_canvas.tex_h,
            'pixel_aspect_x': self.map_canvas.pixel_aspect_x,
            'canvas_width': self._canvas_width,
            'canvas_height': self._canvas_height,
            'undo_stack': deque(self._undo_stack, maxlen=self._undo_stack.maxlen),
            'redo_stack': deque(self._redo_stack, maxlen=self._redo_stack.maxlen),
        }

    def _restore_canvas_tab_state(self, state): #vers 1
        """Apply a previously captured tab state back onto the active
        canvas widget/workshop attributes. Also clears transient,
        tab-independent UI state (selection, marching ants, in-progress
        curve/polygon points) since none of that carries meaning across
        a different image - matches how switching documents in most
        paint programs drops any in-progress interaction."""
        if not self.map_canvas or state is None:
            return
        c = self.map_canvas
        c.tex_w, c.tex_h = state['tex_w'], state['tex_h']
        c.pixel_aspect_x = state['pixel_aspect_x']
        c.rgba = bytearray(state['rgba'])
        self._canvas_width  = state['canvas_width']
        self._canvas_height = state['canvas_height']
        self._undo_stack = deque(state['undo_stack'], maxlen=self._undo_stack.maxlen)
        self._redo_stack = deque(state['redo_stack'], maxlen=self._redo_stack.maxlen)

        # Clear transient state that doesn't carry across documents
        c._sel_active = False
        c._selection_rect = None
        c._sel_floating = False
        c._lasso_pts = []
        c._curve_phase = None
        c._curve_p0 = None
        c._curve_p1 = None
        c._curve_control = None
        c._curve_dragging_warp = False
        c._poly_pts = []
        c._drawing = False
        c._last_pt = None
        c._preview_start = c._preview_end = None

        c.update()
        self._fit_canvas_to_viewport()
        if hasattr(self, '_status_size_lbl'):
            self._status_size_lbl.setText(f"{c.tex_w}×{c.tex_h}")

    def _switch_canvas_tab(self, idx: int): #vers 1
        """Switch to canvas tab idx - saves the current tab's state into
        its own slot first, then loads the target tab's saved state."""
        if idx == self._active_canvas_tab_idx:
            return
        if not (0 <= idx < len(self._canvas_tabs)):
            return
        current_state = self._capture_canvas_tab_state()
        if current_state is not None and 0 <= self._active_canvas_tab_idx < len(self._canvas_tabs):
            self._canvas_tabs[self._active_canvas_tab_idx]['state'] = current_state
        self._active_canvas_tab_idx = idx
        self._restore_canvas_tab_state(self._canvas_tabs[idx]['state'])
        self._refresh_canvas_tabs_ribbon()

    def _save_current_canvas_tab(self): #vers 1
        """Call this BEFORE overwriting map_canvas's contents (New
        Canvas, Load, etc.) so the outgoing tab's state is captured
        correctly - capturing it any later would see the new data
        instead of what's being replaced. Handles the very first call
        (no tabs registered yet) by registering the current canvas as
        tab 1 first."""
        state = self._capture_canvas_tab_state()
        if state is None:
            return
        if not self._canvas_tabs:
            self._canvas_tabs.append({'label': '1', 'state': state})
            self._active_canvas_tab_idx = 0
        else:
            self._canvas_tabs[self._active_canvas_tab_idx]['state'] = state

    def _refresh_canvas_tabs_ribbon(self): #vers 1
        """Rebuild the numbered tab buttons to match self._canvas_tabs,
        with the active tab's button checked."""
        ribbon = getattr(self, '_canvas_tabs_ribbon', None)
        if ribbon is None:
            return
        ribbon.clear()
        self._canvas_tab_btns = []
        for i, tab in enumerate(self._canvas_tabs):
            act = QAction(tab['label'], ribbon)
            act.setCheckable(True)
            act.setChecked(i == self._active_canvas_tab_idx)
            act.triggered.connect(lambda _, idx=i: self._switch_canvas_tab(idx))
            ribbon.addAction(act)
            self._canvas_tab_btns.append(act)

    def _fill_canvas(self): #vers 1
        if not self.map_canvas: return
        self._push_undo()
        c = self.map_canvas.color
        for i in range(self._canvas_width * self._canvas_height):
            self.map_canvas.rgba[i*4:i*4+4] = [c.red(),c.green(),c.blue(),c.alpha()]
        self.map_canvas.update()


    def _fit_canvas_to_viewport(self): #vers 2
        if not self.map_canvas: return
        sa = getattr(self, '_canvas_scroll', None)
        vw = sa.viewport().width()  if sa else self.width()
        vh = sa.viewport().height() if sa else self.height()
        w = max(1, self.map_canvas.tex_w)
        h = max(1, self.map_canvas.tex_h)
        pa = getattr(self.map_canvas, 'pixel_aspect_x', 1.0)
        fit_z = min(vw / (w * pa), vh / h)
        for snap in (16, 8, 4, 2, 1):
            if fit_z >= snap:
                fit_z = snap; break
        self._set_zoom(max(0.05, fit_z))


    def resizeEvent(self, event): #vers 2
        super().resizeEvent(event)
        if self.map_settings.get('zoom_to_fit_resize'):
            self._fit_canvas_to_viewport()
        self._refresh_corner_overlay()


    def _set_zoom(self, z, anchor_widget_pos=None): #vers 1
        """
        Set zoom level.  anchor_widget_pos: QPoint in scroll-area viewport
        coordinates to keep fixed.  If None, anchors to viewport centre.
        """
        if not self.map_canvas: return
        old_z = max(0.01, self.map_canvas.zoom)
        z     = max(0.05, min(64.0, float(z)))
        self.map_canvas.zoom = z
        self._canvas_zoom    = z
        self._update_zoom_label()

        # Resize canvas widget to match new zoom
        zx = z * getattr(self.map_canvas, 'pixel_aspect_x', 1.0)
        new_w = max(200, int(self.map_canvas.tex_w * zx))
        new_h = max(200, int(self.map_canvas.tex_h * z))
        self.map_canvas.resize(new_w, new_h)
        self.map_canvas.updateGeometry()

        # Scroll to keep anchor point fixed
        sa = getattr(self, '_canvas_scroll', None)
        if sa:
            hb = sa.horizontalScrollBar()
            vb = sa.verticalScrollBar()
            if anchor_widget_pos is None:
                # Default anchor: current viewport centre
                vp = sa.viewport()
                anchor_widget_pos = QPoint(vp.width() // 2, vp.height() // 2)
            # Current scroll position + anchor gives the tex-space point
            old_sx = hb.value() + anchor_widget_pos.x()
            old_sy = vb.value() + anchor_widget_pos.y()
            # New scroll position that keeps the same tex point under anchor
            ratio  = z / old_z
            new_sx = int(old_sx * ratio) - anchor_widget_pos.x()
            new_sy = int(old_sy * ratio) - anchor_widget_pos.y()
            hb.setValue(max(0, new_sx))
            vb.setValue(max(0, new_sy))

        self.map_canvas.update()

    def _toggle_active_zoom(self): #vers 1
        """Active Zoom toggle - jumps to a high zoom level and enables
        follow-cursor viewport panning (see _center_view_on), so detail/
        pixel work can happen directly in the zoomed main view rather
        than the separate read-only Zoom Lens corner preview. Toggling
        off restores the zoom level from before it was enabled."""
        c = self.map_canvas
        if not c:
            return
        if not c._active_zoom_follow:
            self._active_zoom_prev_level = c.zoom
            c._active_zoom_follow = True
            c._active_zoom_last_center = None
            self._set_zoom(8.0)
            self._set_status("Active Zoom on - viewport follows cursor")
        else:
            c._active_zoom_follow = False
            prev = getattr(self, '_active_zoom_prev_level', None)
            if prev:
                self._set_zoom(prev)
            self._set_status("Active Zoom off")
        btn = getattr(self, '_active_zoom_btn', None)
        if btn:
            btn.setChecked(c._active_zoom_follow)

    def _show_active_zoom_sensitivity_menu(self, pos): #vers 1
        """Right-click on the Active Zoom button - adjust how far the
        cursor must move (in texture pixels) before the viewport
        recenters again. Lower values recenter more eagerly (more
        precise tracking, more scroll/repaint work); higher values
        recenter less often (smoother, less laggy, slightly less
        tightly centred)."""
        from PyQt6.QtWidgets import QWidgetAction
        c = self.map_canvas
        if not c:
            return
        menu = QMenu(self)
        row_w = QWidget()
        row = QHBoxLayout(row_w)
        row.setContentsMargins(8, 3, 8, 3)
        row.addWidget(QLabel("Sensitivity:"))
        spin = QSpinBox()
        spin.setRange(1, 50)
        spin.setValue(c._active_zoom_sensitivity)
        spin.setToolTip("Lower = tracks the cursor more tightly (may lag).\n"
                        "Higher = smoother panning, less precise centring.")
        spin.setFixedWidth(60)
        spin.valueChanged.connect(lambda v: setattr(c, '_active_zoom_sensitivity', v))
        row.addWidget(spin)
        wa = QWidgetAction(menu)
        wa.setDefaultWidget(row_w)
        menu.addAction(wa)
        w = self.sender()
        menu.exec(w.mapToGlobal(pos) if w else self.cursor().pos())

    def _flip_h(self):   self._mirror_h()   # legacy alias  #vers 1
    def _flip_v(self):   self._mirror_v()   # legacy alias  #vers 1


    def _dither_floyd_steinberg(self): #vers 1
        """Apply Floyd-Steinberg error-diffusion dither to canvas (reduces to 16 colours)."""
        if not self.map_canvas: return
        from PIL import Image
        n, ok = QInputDialog.getInt(self, "Floyd-Steinberg Dither",
                                    "Colours to reduce to:", 16, 2, 256)
        if not ok: return
        self._push_undo()
        img = Image.frombytes('RGBA',(self._canvas_width,self._canvas_height),
                               bytes(self.map_canvas.rgba)).convert('RGB')
        # PIL quantize with dither=1 = Floyd-Steinberg
        q = img.quantize(colors=n, dither=1).convert('RGB').convert('RGBA')
        self.map_canvas.rgba = bytearray(q.tobytes())
        self.map_canvas.update()
        self._set_status(f"Floyd-Steinberg dither → {n} colours")


    def _dither_bayer_canvas(self): #vers 1
        """Apply 4×4 Bayer ordered dither to canvas."""
        if not self.map_canvas: return
        from PIL import Image
        n, ok = QInputDialog.getInt(self, "Bayer Dither",
                                    "Colours to reduce to:", 16, 2, 256)
        if not ok: return
        self._push_undo()
        BAYER4 = [[0,8,2,10],[12,4,14,6],[3,11,1,9],[15,7,13,5]]
        img = Image.frombytes('RGBA',(self._canvas_width,self._canvas_height),
                               bytes(self.map_canvas.rgba)).convert('RGB')
        # Quantize first to get palette, then apply bayer threshold
        q_pal = img.quantize(colors=n, dither=0)
        pal_flat = q_pal.getpalette()
        pal = [(pal_flat[i*3],pal_flat[i*3+1],pal_flat[i*3+2]) for i in range(n)]
        px = list(img.getdata())
        w,h = self._canvas_width, self._canvas_height
        out = bytearray(w*h*4)
        for y in range(h):
            for x in range(w):
                r,g,b = px[y*w+x]
                t = BAYER4[y%4][x%4]/16.0
                # Shift pixel value by threshold amount
                sr = min(255,int(r + (t-0.5)*32))
                sg = min(255,int(g + (t-0.5)*32))
                sb = min(255,int(b + (t-0.5)*32))
                best = min(pal, key=lambda c: (c[0]-sr)**2+(c[1]-sg)**2+(c[2]-sb)**2)
                i = (y*w+x)*4
                out[i:i+4] = [best[0],best[1],best[2],255]
        self.map_canvas.rgba = out
        self.map_canvas.update()
        self._set_status(f"Bayer 4×4 dither → {n} colours")


    def _dither_checker_canvas(self): #vers 1
        """Apply checkerboard FG/BG dither to entire canvas."""
        if not self.map_canvas: return
        self._push_undo()
        fg = self.map_canvas.color
        bg = self._fgbg_swatch._bg if hasattr(self,'_fgbg_swatch') else QColor(0,0,0,255)
        w,h = self._canvas_width, self._canvas_height
        out = bytearray(self.map_canvas.rgba)
        for y in range(h):
            for x in range(w):
                if (x+y)%2==0:
                    i=(y*w+x)*4
                    out[i:i+4]=[bg.red(),bg.green(),bg.blue(),bg.alpha()]
        self.map_canvas.rgba = out
        self.map_canvas.update()
        self._set_status("Checkerboard dither applied")


    def _pil_transform(self, fn): #vers 1
        """Apply a PIL Image transform to the canvas."""
        if not self.map_canvas: return
        self._push_undo()
        from PIL import Image
        img = Image.frombytes('RGBA',
                              (self._canvas_width, self._canvas_height),
                              bytes(self.map_canvas.rgba))
        img2 = fn(img)
        # Update canvas dimensions if they changed (scale/rotate 90)
        w2, h2 = img2.size
        self._canvas_width  = w2
        self._canvas_height = h2
        new_rgba = bytearray(img2.tobytes())
        self.map_canvas.tex_w = w2
        self.map_canvas.tex_h = h2
        self.map_canvas.rgba  = new_rgba
        self.map_canvas.update()
        self._set_status(f"Canvas: {w2}×{h2}")


    def _rotate_90_cw(self): #vers 1
        from PIL import Image
        self._pil_transform(lambda i: i.transpose(Image.Transpose.ROTATE_270))


    def _rotate_90_ccw(self): #vers 1
        from PIL import Image
        self._pil_transform(lambda i: i.transpose(Image.Transpose.ROTATE_90))


    def _rotate_180(self): #vers 1
        from PIL import Image
        self._pil_transform(lambda i: i.transpose(Image.Transpose.ROTATE_180))


    def _rotate_arbitrary(self): #vers 1
        deg, ok = QInputDialog.getInt(self, "Rotate", "Degrees (clockwise):",
                                      45, -359, 359)
        if not ok: return
        from PIL import Image
        self._pil_transform(lambda i: i.rotate(-deg, expand=True,
                                               resample=Image.Resampling.BILINEAR))

    def _mirror_h(self): #vers 1
        from PIL import Image
        self._pil_transform(lambda i: i.transpose(Image.Transpose.FLIP_LEFT_RIGHT))


    def _mirror_v(self): #vers 1
        from PIL import Image
        self._pil_transform(lambda i: i.transpose(Image.Transpose.FLIP_TOP_BOTTOM))


    def _scale_canvas(self): #vers 2
        """Scale canvas — inline overlay panel."""
        if not self.map_canvas: return

        from PyQt6.QtWidgets import (QWidget, QVBoxLayout, QHBoxLayout,
            QGridLayout, QFormLayout, QSpinBox, QLabel, QPushButton, QComboBox)

        ctrl = QWidget()
        cl = QVBoxLayout(ctrl); cl.setContentsMargins(0,0,0,0); cl.setSpacing(3)

        form = QFormLayout()
        w_spin = QSpinBox(); w_spin.setRange(8, 8192); w_spin.setValue(self._canvas_width)
        h_spin = QSpinBox(); h_spin.setRange(8, 8192); h_spin.setValue(self._canvas_height)
        form.addRow("Width:", w_spin)
        form.addRow("Height:", h_spin)
        cl.addLayout(form)

        cl.addWidget(QLabel("Presets:"))
        grid = QGridLayout(); grid.setSpacing(2)
        for i, (lbl, pw, ph) in enumerate([
            ("32×32",32,32),("64×64",64,64),("128×128",128,128),("256×256",256,256),
            ("512×512",512,512),("1024×1024",1024,1024),("320×200",320,200),("640×480",640,480),
        ]):
            b = QPushButton(lbl); b.setFixedHeight(22)
            b.clicked.connect(lambda _=False, pw=pw, ph=ph:
                              (w_spin.setValue(pw), h_spin.setValue(ph)))
            grid.addWidget(b, i // 4, i % 4)
        cl.addLayout(grid)

        resamp_combo = QComboBox()
        resamp_combo.addItems(["Nearest", "Bilinear", "Bicubic", "Lanczos"])
        rf = QFormLayout(); rf.addRow("Resample:", resamp_combo)
        cl.addLayout(rf)

        parent_vp = self._canvas_scroll.viewport() if hasattr(self, '_canvas_scroll') else self

        def _apply():  #vers 1
            from PIL import Image
            methods = [Image.Resampling.NEAREST, Image.Resampling.BILINEAR,
                       Image.Resampling.BICUBIC, Image.Resampling.LANCZOS]
            nw, nh = w_spin.value(), h_spin.value()
            self._pil_transform(lambda i, nw=nw, nh=nh, m=methods[resamp_combo.currentIndex()]:
                                i.resize((nw, nh), m))

        self._ToolOverlay(parent_vp, self, "Scale Canvas",
                          ctrl, apply_fn=_apply, generate_fn=None)

    def _load_game_folder(self, preset_root: str = None): #vers 2
        """Load a GTA game's world data (DAT -> IDE -> IPL, full engine-
        order two-phase load) via the existing GTAWorldLoader - this is
        the Map Editor's actual data layer, already handling multi-game
        detection (GTA3/VC/SA/SOL) and object/instance cross-referencing;
        nothing new needed here beyond wiring it into the UI.

        preset_root: when given (e.g. passed in from Dat Browser, which
        already has a game root loaded), skip the folder picker and load
        directly - the same underlying load either way, just two ways to
        reach it, matching how Model/TXD/COL Workshop can be opened either
        via an explicit path or by picking up whatever's already open."""
        from PyQt6.QtWidgets import QFileDialog
        from apps.methods.gta_dat_parser import detect_game, GTAWorldLoader

        if preset_root:
            folder = preset_root
        else:
            folder = QFileDialog.getExistingDirectory(self, "Select GTA game folder")
            if not folder:
                return

        game = detect_game(folder)
        if not game:
            QMessageBox.warning(self, "Load Game Folder",
                f"Couldn't detect a supported GTA game in:\n{folder}\n\n"
                "Expected a 'data' folder containing gta3.dat, gta_vc.dat, "
                "or gta.dat.")
            return

        loader = GTAWorldLoader(game)
        loader.lazy_ipl_loading = True   # per Keith: don't load/scan any
                                         # IPL's content (or its models'
                                         # geometry/textures) until the
                                         # user actually asks for that
                                         # specific IPL, not the whole
                                         # world eagerly at load time
        ok = loader.load(folder)
        self._game_root = folder
        self._apply_loaded_world(loader, game, ok, "Load Game Folder")

    def _show_map_load_menu(self): #vers 1
        """Menu shown from the titlebar's Load button - the same two
        load paths already in the File menu (Load Game Folder / Load
        Game DAT File), surfaced here since Keith wants Load on the
        titlebar to actually load a map, not the paint canvas's old
        image-load menu."""
        menu = QMenu(self)
        menu.addAction("Load Game Folder…", self._load_game_folder)
        menu.addAction("Load Game DAT File…", self._load_game_dat_file)
        menu.exec(self.tb_load_btn.mapToGlobal(
            self.tb_load_btn.rect().bottomLeft()))

    def _load_game_dat_file(self, preset_dat_path: str = None): #vers 1
        """Load a GTA game's world data starting from one specific .dat
        file, rather than a whole game folder - per Keith's request
        that the standalone flow ask for the actual gta_xx.dat/
        default.dat file path directly. Also the entry point for the
        DAT Browser tree's 'Load with Map Workshop' right-click on a
        specific .dat entry, via preset_dat_path.

        The game is detected purely from the .dat file's own basename
        (detect_game_from_dat_filename) rather than scanning a folder -
        deliberately doesn't accept an ambiguous bare 'default.dat'
        (shared across gta3/vc/sa), since there'd be no way to tell
        which game it belongs to from the filename alone; the user
        picks the actual main .dat (gta3.dat, gta_vc.dat, gta.dat, or
        gta_sol.dat)."""
        from PyQt6.QtWidgets import QFileDialog
        from apps.methods.gta_dat_parser import detect_game_from_dat_filename, GTAWorldLoader

        if preset_dat_path:
            dat_path = preset_dat_path
        else:
            dat_path, _ = QFileDialog.getOpenFileName(
                self, "Select GTA .dat file", "",
                "GTA DAT files (gta3.dat gta_vc.dat gta.dat gta_sol.dat gtasol.dat gta_quick.dat);;All files (*.dat)")
            if not dat_path:
                return

        game = detect_game_from_dat_filename(dat_path)
        if not game:
            QMessageBox.warning(self, "Load Game DAT File",
                f"Couldn't identify the game from:\n{dat_path}\n\n"
                "Expected the game's main .dat file (gta3.dat, gta_vc.dat, "
                "gta.dat, or gta_sol.dat) - not default.dat, which is "
                "shared across games and can't be identified by name alone.")
            return

        game_root = os.path.normpath(os.path.join(os.path.dirname(dat_path), ".."))
        loader = GTAWorldLoader(game)
        loader.lazy_ipl_loading = True
        ok = loader.load_from_dat(dat_path, game_root)
        self._game_root = game_root
        self._apply_loaded_world(loader, game, ok, "Load Game DAT File")

    def _apply_loaded_world(self, loader, game, ok, source_desc): #vers 1
        """Shared post-load handling for both _load_game_folder and
        _load_game_dat_file - status message, populating the World View
        panes/Instance List/IPL Sections panel, and the summary/error
        dialog. Factored out so both loading paths (folder-based, or a
        specific .dat file) share exactly the same result handling."""
        self._world_loader = loader
        self._loaded_dat_path = getattr(loader.main_dat, 'dat_path', '') if hasattr(loader, 'main_dat') else ''

        if not ok:
            QMessageBox.warning(self, source_desc,
                f"Load failed:\n" + "\n".join(loader.stats.errors[:10]))
            return

        self._set_status(
            f"Loaded {game.upper()} world: {len(loader.objects)} objects, "
            f"{len(loader.instances)} instances, {loader.stats.ipl_files} IPL files")
        self._lod_pairs = loader.resolve_lod_pairs()
        self._lod_overrides = {}
        self._lod_display_mode = 'normal'

        model_cache = getattr(self, '_model_cache', None)
        if model_cache is None:
            from apps.components.Map_Editor.depends.model_cache import ModelCache
            model_cache = ModelCache()
            self._model_cache = model_cache
        model_cache.index_img_files(loader.get_img_paths())
        # Per Keith: model/texture scanning should happen when a
        # specific IPL is actually loaded, not for the whole world at
        # startup - with lazy_ipl_loading enabled above, loader.
        # instances is empty at this point anyway (nothing parsed
        # yet), so pre-loading here would be a no-op regardless. The
        # real pre-load now happens per-IPL in _on_ipl_section_cell_
        # clicked, scoped to just that IPL's newly-loaded instances.

        visible = self._apply_lod_filter(loader.instances)
        for pane in getattr(self, '_world_panes', []):
            pane.set_instances(visible)
            pane.set_model_cache(model_cache)
            pane.set_cull_boxes(loader.culls, getattr(self, '_cull_boxes_act', None) and
                                self._cull_boxes_act.isChecked())
        self._populate_instance_list(_FilteredLoaderStub(visible, loader))
        self._populate_ipl_sections(loader)
        self._populate_object_browser(loader)
        self._refresh_ide_tab(loader)
        self._refresh_dat_tab()
        self._refresh_img_tab(loader)
        self._refresh_ipl_inst_file_panel()
        QMessageBox.information(self, source_desc, loader.get_summary())

        # Per Keith: "we could have load from .dat file with no IPL
        # files selected... Or load with options. the options show all
        # ipl files [x] boxes and model [x] textures [x] and
        # [continue]." Only offered when lazy loading actually
        # discovered something to choose from.
        if getattr(loader, 'lazy_ipl_loading', False) and loader.available_ipls:
            selection = self._show_load_options_dialog(loader)
            if selection is not None:
                stems, load_models, load_textures = selection
                if stems:
                    self._load_selected_ipls_with_log(
                        loader, model_cache, stems, load_models, load_textures)

    def _load_selected_ipls_with_log(self, loader, model_cache, stems, load_models, load_textures): #vers 1
        """Load a batch of specific IPLs (from the Load Options dialog),
        showing a scrolling log dialog matching Keith's exact requested
        format: "loading <name>" followed by each newly-added instance's
        line as it's added, then a final per-IPL result line - "loaded
        <name> - no errors" or "<name> - <model>.dff/<txd>.txd missing
        from img file" when a referenced model/texture genuinely isn't
        in any indexed archive at all (as opposed to a parse error in
        the IPL's own text, which _write_ipl_error_log/the per-IPL
        issue count already covers separately).

        load_models/load_textures gate whether ModelCache.get_geometry/
        get_textures are called at all for this batch - unchecking
        either skips that half of pre-loading entirely for everything
        loaded here (their own per-instance rendering fallback to
        point/dot rendering is unaffected either way, this only
        controls whether pre-loading happens now)."""
        from PyQt6.QtWidgets import QTextEdit

        dlg = QDialog(self)
        dlg.setWindowTitle("Loading IPL Files")
        dlg.resize(560, 420)
        lay = QVBoxLayout(dlg)
        log = QTextEdit()
        log.setReadOnly(True)
        font = log.font(); font.setFamily("monospace"); log.setFont(font)
        lay.addWidget(log)
        cancel_btn = QPushButton("Cancel")
        btn_row = QHBoxLayout()
        btn_row.addStretch()
        btn_row.addWidget(cancel_btn)
        lay.addLayout(btn_row)

        cancelled = {'flag': False}
        cancel_btn.clicked.connect(lambda: cancelled.update(flag=True))
        dlg.show()

        def _append(line): #vers 1
            log.append(line)
            QApplication.processEvents()

        any_loaded = False
        for stem in stems:
            if cancelled['flag']:
                _append("Cancelled - remaining IPLs not loaded")
                break
            entry = loader.available_ipls.get(stem)
            display_name = os.path.basename(entry.abs_path) if entry else stem
            _append(f"loading {display_name}")

            before_count = len(loader.instances)
            result = loader.load_ipl_by_name(stem)
            if not result.success:
                _append(f"{display_name} - failed to load"
                       + (f": {result.errors[0]}" if result.errors else ""))
                continue
            any_loaded = True

            new_instances = loader.instances[before_count:]
            for i, inst in enumerate(new_instances):
                _append(f"{inst.model_id}, {inst.model_name}, {inst.interior}, "
                        f"{inst.pos_x}, {inst.pos_y}, {inst.pos_z}, "
                        f"{inst.scale_x}, {inst.scale_y}, {inst.scale_z}, "
                        f"{inst.rot_x}, {inst.rot_y}, {inst.rot_z}, {inst.rot_w}")
                if cancelled['flag']:
                    break

            # Missing-model/texture detection - genuinely absent from
            # any indexed archive, not a parse error in the IPL itself
            missing = []
            seen_models = set()
            for inst in new_instances:
                if inst.model_name in seen_models:
                    continue
                seen_models.add(inst.model_name)
                obj = loader.get_object(inst.model_id)
                txd_name = obj.txd_name if obj else ""
                if load_models and not model_cache.is_dff_indexed(inst.model_name):
                    missing.append(f"{inst.model_name}.dff")
                if load_textures and txd_name and not model_cache.is_txd_indexed(txd_name):
                    missing.append(f"{txd_name}.txd")
                if load_models:
                    model_cache.get_geometry(inst.model_name)
                if load_textures and txd_name:
                    model_cache.get_textures(txd_name)

            problem_count = result.error_count + result.warning_count
            if missing:
                _append(f"{display_name} loaded - "
                        + "/ ".join(missing) + " missing from img file")
                self._write_ipl_error_log(result)
            elif problem_count:
                log_path = self._write_ipl_error_log(result)
                _append(f"{display_name} loaded - {problem_count} issue(s) found"
                        + (f", check {os.path.basename(log_path)} added to the maps folder"
                           if log_path else ""))
            else:
                _append(f"{display_name} loaded - no errors")

        if any_loaded:
            self._all_instances = list(loader.instances)
            self._populate_object_browser(loader)
            visible = self._apply_lod_filter(loader.instances)
            for pane in getattr(self, '_world_panes', []):
                pane.set_instances(visible)

        cancel_btn.setText("Close")
        cancel_btn.clicked.disconnect()
        cancel_btn.clicked.connect(dlg.accept)

    def _show_load_options_dialog(self, loader): #vers 1
        """Shown right after a world's IPL list is discovered (but
        before any of their content is actually loaded) - lets the
        user choose "load from .dat file" (stay purely lazy, nothing
        loaded now, browse/load individual IPLs later via their eye
        icons - the existing behaviour) or "load with options" (pick
        specific IPLs, and whether to load their models/textures, right
        now). Returns (selected_stems, load_models, load_textures), or
        None if the user picked "load from .dat file" / cancelled."""
        dlg = QDialog(self)
        dlg.setWindowTitle("Load Options")
        dlg.setMinimumWidth(420)
        lay = QVBoxLayout(dlg)

        dat_only_radio = QRadioButton(
            "Load from .dat file - IPL data is discovered but not loaded; "
            "no models or textures loaded into the scene\n"
            "(load individual IPLs later via their eye icon in IPL Sections)")
        dat_only_radio.setChecked(True)
        with_options_radio = QRadioButton("Load with options:")
        lay.addWidget(dat_only_radio)
        lay.addWidget(with_options_radio)

        options_box = QGroupBox()
        themecol_opt = self.app_settings.get_theme_colors()
        panel_bg_opt = themecol_opt.get('panel_bg')
        if panel_bg_opt:
            options_box.setStyleSheet(f"QGroupBox {{ background: {panel_bg_opt}; }}")
        options_lay = QVBoxLayout(options_box)
        ipl_list = QListWidget()
        ipl_list.setSelectionMode(QAbstractItemView.SelectionMode.NoSelection)
        stem_by_display = {}
        for stem, entry in sorted(loader.available_ipls.items(),
                                  key=lambda kv: os.path.basename(kv[1].abs_path).lower()):
            display_name = os.path.basename(entry.abs_path)
            item = QListWidgetItem(display_name)
            item.setFlags(item.flags() | Qt.ItemFlag.ItemIsUserCheckable)
            item.setCheckState(Qt.CheckState.Unchecked)
            ipl_list.addItem(item)
            stem_by_display[display_name] = stem
        options_lay.addWidget(ipl_list)

        select_row = QHBoxLayout()
        select_all_btn = QPushButton("Select All")
        select_none_btn = QPushButton("Select None")
        select_row.addWidget(select_all_btn)
        select_row.addWidget(select_none_btn)
        select_row.addStretch()
        options_lay.addLayout(select_row)

        def _set_all_checked(checked): #vers 1
            state = Qt.CheckState.Checked if checked else Qt.CheckState.Unchecked
            for i in range(ipl_list.count()):
                ipl_list.item(i).setCheckState(state)
        select_all_btn.clicked.connect(lambda: _set_all_checked(True))
        select_none_btn.clicked.connect(lambda: _set_all_checked(False))

        models_chk = QCheckBox("Model")
        models_chk.setChecked(True)
        textures_chk = QCheckBox("Textures")
        textures_chk.setChecked(True)
        mt_row = QHBoxLayout()
        mt_row.addWidget(models_chk)
        mt_row.addWidget(textures_chk)
        mt_row.addStretch()
        options_lay.addLayout(mt_row)
        lay.addWidget(options_box)

        def _sync_options_enabled(): #vers 1
            options_box.setEnabled(with_options_radio.isChecked())
        dat_only_radio.toggled.connect(_sync_options_enabled)
        with_options_radio.toggled.connect(_sync_options_enabled)
        _sync_options_enabled()

        btn_row = QHBoxLayout()
        btn_row.addStretch()
        continue_btn = QPushButton("Continue")
        continue_btn.setDefault(True)
        btn_row.addWidget(continue_btn)
        lay.addLayout(btn_row)

        result = {}
        def _on_continue(): #vers 1
            if with_options_radio.isChecked():
                selected_stems = [
                    stem_by_display[ipl_list.item(i).text()]
                    for i in range(ipl_list.count())
                    if ipl_list.item(i).checkState() == Qt.CheckState.Checked]
                result['value'] = (selected_stems, models_chk.isChecked(),
                                   textures_chk.isChecked())
            else:
                result['value'] = ([], True, True)   # dat-only: nothing to load
            dlg.accept()
        continue_btn.clicked.connect(_on_continue)

        dlg.exec()
        return result.get('value')

    def _set_render_mode(self, mode): #vers 1
        """Set the Solid/Semi/Wireframe render mode across all World
        View panes - only affects instances with real loaded geometry,
        per MapViewport.set_render_mode's own docstring."""
        for pane in getattr(self, '_world_panes', []):
            pane.set_render_mode(mode)

    def _preload_world_assets(self, loader, model_cache, instances=None, title=None): #vers 2
        """Eagerly load+parse (and cache) geometry and textures for
        every distinct model referenced by the given instances (or
        every loaded instance, if none given), with a progress dialog
        showing what's currently being processed.

        Originally ran across the whole world at load time - per
        Keith's follow-up report ("very slow scanning... it should be
        showing model/texture when loading the selected .ipl, not at
        map mapper startup"), this is now called scoped to one
        specific IPL's newly-loaded instances instead (see
        _ensure_ipl_loaded), matching MooMapper's own model of not
        touching an IPL's content - or its models' geometry/textures -
        until the user actually asks for that specific IPL.
        QProgressDialog processes Qt events internally on setValue(),
        so the UI stays responsive throughout rather than a single
        long blocking call."""
        instances = instances if instances is not None else loader.instances
        # Deduplicate by model_name - many instances share one model,
        # no need to load it more than once.
        seen_models = {}
        for inst in instances:
            if inst.model_name not in seen_models:
                obj = loader.get_object(inst.model_id)
                txd_name = obj.txd_name if obj else ""
                seen_models[inst.model_name] = (txd_name, inst.source_ipl)
        if not seen_models:
            return

        items = list(seen_models.items())
        progress = QProgressDialog(
            title or "Loading assets…", "Cancel", 0, len(items), self)
        progress.setWindowTitle("Loading Meshes and Textures")
        progress.setMinimumDuration(500)   # don't flash up for fast loads
        progress.setWindowModality(Qt.WindowModality.WindowModal)

        loaded_txds = set()
        for i, (model_name, (txd_name, source_ipl)) in enumerate(items):
            if progress.wasCanceled():
                self._set_status(
                    f"Asset loading cancelled after {i} of {len(items)} models "
                    f"- remaining models will still load on demand while browsing")
                break
            progress.setLabelText(
                f"Model: {model_name}\nTexture: {txd_name or '(none)'}\nIPL: {source_ipl}")
            progress.setValue(i)
            model_cache.get_geometry(model_name)
            if txd_name and txd_name not in loaded_txds:
                model_cache.get_textures(txd_name)
                loaded_txds.add(txd_name)
        progress.setValue(len(items))

    def _toggle_cull_boxes(self, checked): #vers 1
        """Show/hide wireframe cull zone boxes across all World View
        panes - real, working feature using GTAWorldLoader.culls (see
        MapViewport._draw_cull_boxes for the honest caveat on the
        field-interpretation assumption)."""
        loader = getattr(self, '_world_loader', None)
        culls = loader.culls if loader is not None else []
        for pane in getattr(self, '_world_panes', []):
            pane.set_cull_boxes(culls, checked)

    def _create_control_panel_dock(self): #vers 1
        """Control Panel dock - replicates MooMapper's "Hide/Show
        Control Panel" layout (Position/Move There, Time, Enable
        Textures/Wireframe/Alpha Blending/First Person/Background Map
        checkboxes, Zoom/Reset View, background colour, a Visible
        Files-style summary, and the Dragging Controls legend), per
        Keith's request to replicate the shape of all these functions
        first so there's a solid base to build on later - several are
        wired to real, working functionality already in this project
        (Position/Move There, Reset View, Zoom, Wireframe Mode via the
        existing render-mode dropdown, background colour); the rest
        are clearly marked STUB below and don't yet do anything -
        they're placeholders for functionality this project doesn't
        have yet (time-of-day simulation, first-person navigation,
        a reference background map image, texture toggling separate
        from render mode)."""
        panel = QWidget()
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(6, 6, 6, 6)

        # - Position (X, Y, Z) + Move There
        pos_box = QGroupBox("Position (X, Y, Z)")
        from PyQt6.QtWidgets import QAbstractSpinBox
        themecol_pos = self.app_settings.get_theme_colors()
        panel_bg_pos = themecol_pos.get('panel_bg')
        if panel_bg_pos:
            pos_box.setStyleSheet(f"QGroupBox {{ background: {panel_bg_pos}; }}")
        pos_lay = QHBoxLayout(pos_box)
        self._cp_pos_x = QDoubleSpinBox(); self._cp_pos_x.setRange(-100000, 100000)
        self._cp_pos_y = QDoubleSpinBox(); self._cp_pos_y.setRange(-100000, 100000)
        self._cp_pos_z = QDoubleSpinBox(); self._cp_pos_z.setRange(-100000, 100000)
        move_there_btn = QPushButton("Move There")
        move_there_btn.setFixedHeight(self._COMPACT_BUTTON_H + 6)
        move_there_btn.setToolTip("Centre all World View panes' cameras on this\n"
                                  "typed position - the direct type-in alternative\n"
                                  "to nudging/clicking an object")
        move_there_btn.clicked.connect(self._on_move_there_clicked)
        for spin in (self._cp_pos_x, self._cp_pos_y, self._cp_pos_z):
            spin.setDecimals(2)
            # Without this, Qt reserves worst-case width for the full
            # ±100000 range (over 500px combined for all three) - per
            # Keith's report that this was locking the whole panel's
            # width. Still fully usable for extreme values via typing/
            # scrolling, just doesn't force display of every possible
            # digit at once.
            spin.setMinimumWidth(60)
            spin.setButtonSymbols(QAbstractSpinBox.ButtonSymbols.UpDownArrows)
            spin.setFixedHeight(self._COMPACT_BUTTON_H + 6)   # match Move There's height
            pos_lay.addWidget(spin)
        pos_lay.addWidget(move_there_btn)
        lay.addWidget(pos_box)

        # - Time (STUB - no day/night simulation exists yet)
        time_row = QHBoxLayout()
        time_row.addWidget(QLabel("Time:"))
        self._cp_time_combo = QComboBox()
        self._cp_time_combo.addItems([f"{h:02d}:00" for h in range(24)])
        self._cp_time_combo.setCurrentText("09:00")
        self._cp_time_combo.setToolTip("STUB - no time-of-day simulation built yet\n"
                                       "(would matter for TOBJ/timed object rendering)")
        time_row.addWidget(self._cp_time_combo)
        time_row.addStretch()
        lay.addLayout(time_row)

        # - Checkboxes: Enable Textures / Wireframe Mode / Alpha Blending /
        #   First Person / Background Map
        self._cp_enable_textures_chk = QCheckBox("Enable Textures")
        self._cp_enable_textures_chk.setChecked(True)
        self._cp_enable_textures_chk.setToolTip("STUB - texture binding for real mesh\n"
                                                "rendering isn't wired up yet")
        lay.addWidget(self._cp_enable_textures_chk)

        self._cp_wireframe_chk = QCheckBox("Wireframe Mode")
        self._cp_wireframe_chk.setToolTip("Same as choosing Wireframe in the\n"
                                          "Render Mode dropdown (Plotting ribbon)")
        self._cp_wireframe_chk.toggled.connect(self._on_control_panel_wireframe_toggled)
        lay.addWidget(self._cp_wireframe_chk)

        self._cp_alpha_chk = QCheckBox("Alpha Blending")
        self._cp_alpha_chk.setChecked(True)
        self._cp_alpha_chk.setToolTip("STUB - not yet connected to anything")
        lay.addWidget(self._cp_alpha_chk)

        self._cp_first_person_chk = QCheckBox("First Person")
        self._cp_first_person_chk.setToolTip("STUB - no first-person navigation mode\n"
                                             "built yet, only orbit/pan camera controls")
        lay.addWidget(self._cp_first_person_chk)

        self._cp_background_map_chk = QCheckBox("Background Map")
        self._cp_background_map_chk.setToolTip("STUB - no reference background map\n"
                                                "image support built yet")
        lay.addWidget(self._cp_background_map_chk)

        # - Zoom +/- and Reset View
        zoom_row = QHBoxLayout()
        zoom_row.addWidget(QLabel("Zoom:"))
        zoom_minus_btn = QPushButton("-"); zoom_minus_btn.setFixedWidth(28)
        zoom_minus_btn.clicked.connect(lambda: self._on_control_panel_zoom(1.15))
        zoom_plus_btn = QPushButton("+"); zoom_plus_btn.setFixedWidth(28)
        zoom_plus_btn.clicked.connect(lambda: self._on_control_panel_zoom(0.85))
        zoom_row.addWidget(zoom_minus_btn)
        zoom_row.addWidget(zoom_plus_btn)
        zoom_row.addStretch()
        lay.addLayout(zoom_row)

        reset_view_btn = QPushButton("Reset View")
        reset_view_btn.setToolTip("Reset yaw/pitch/pan and re-fit the camera to\n"
                                  "the currently loaded instances, for every pane")
        reset_view_btn.clicked.connect(self._on_control_panel_reset_view)
        lay.addWidget(reset_view_btn)

        # - Background colour
        bg_row = QHBoxLayout()
        bg_row.addWidget(QLabel("Background:"))
        self._cp_bg_combo = QComboBox()
        self._cp_bg_combo.addItems(["Default", "Black", "White", "Dark Grey"])
        self._cp_bg_combo.currentTextChanged.connect(self._on_control_panel_bg_changed)
        bg_row.addWidget(self._cp_bg_combo)
        lay.addLayout(bg_row)

        # - "Normal Mode" dropdown (STUB - MooMapper shows this but its
        #   exact purpose isn't clear from the reference screenshot alone)
        mode_row = QHBoxLayout()
        mode_row.addWidget(QLabel("Mode:"))
        self._cp_mode_combo = QComboBox()
        self._cp_mode_combo.addItems(["Normal Mode"])
        self._cp_mode_combo.setToolTip("STUB - MooMapper shows a mode dropdown here,\n"
                                       "but its exact purpose isn't confirmed yet")
        mode_row.addWidget(self._cp_mode_combo)
        mode_row.addStretch()
        lay.addLayout(mode_row)

        # - Dragging Controls legend - matches the actual current
        #   MapViewport controls (configure_movement/mouseMoveEvent),
        #   not a stub - this reflects real, working behaviour.
        controls_box = QGroupBox("Dragging Controls")
        themecol = self.app_settings.get_theme_colors()
        panel_bg = themecol.get('panel_bg')
        if panel_bg:
            controls_box.setStyleSheet(
                f"QGroupBox {{ background: {panel_bg}; }} "
                f"QGroupBox QLabel {{ background: {panel_bg}; }}")
        controls_lay = QVBoxLayout(controls_box)
        for line in (
            "Middle Btn: Move Camera (pan)",
            "Right Btn: Rotate Camera (3D pane only)",
            "Left Btn (click, no drag): Select Object & Zoom In",
            "Mouse Wheel: Zoom",
        ):
            controls_lay.addWidget(QLabel(line))
        note = QLabel("(button assignment is configurable in Settings)")
        note.setStyleSheet("color: palette(mid);")
        controls_lay.addWidget(note)
        lay.addWidget(controls_box)

        lay.addStretch()

        dock = QDockWidget("Control Panel", self)
        dock.setObjectName("Control Panel")
        dock.setWidget(panel)
        dock.setFeatures(QDockWidget.DockWidgetFeature.DockWidgetMovable |
                        QDockWidget.DockWidgetFeature.DockWidgetFloatable)
        self._control_panel_dock = dock
        return dock

    def _on_move_there_clicked(self): #vers 1
        x, y, z = self._cp_pos_x.value(), self._cp_pos_y.value(), self._cp_pos_z.value()
        for pane in getattr(self, '_world_panes', []):
            pane._pan_x = -x
            pane._pan_y = -y
            pane.update()

    def _on_control_panel_wireframe_toggled(self, checked): #vers 1
        """Wireframe Mode checkbox - mirrors the Render Mode dropdown
        rather than being a separate, independent control, so the two
        can't disagree with each other."""
        self._set_render_mode('wireframe' if checked else 'solid')
        lod_btn = getattr(self, '_render_mode_button', None)
        if lod_btn is not None:
            menu = lod_btn.menu()
            for act in menu.actions():
                act.setChecked(act.text() == ("Wireframe" if checked else "Solid"))

    def _on_control_panel_zoom(self, factor): #vers 1
        for pane in getattr(self, '_world_panes', []):
            pane._dist = max(0.1, min(50000.0, pane._dist * factor))
            if pane._projection == 'ortho':
                try:
                    pane.resizeGL(pane.width(), pane.height())
                except Exception:
                    pass
            pane.update()

    def _on_control_panel_reset_view(self): #vers 1
        for pane in getattr(self, '_world_panes', []):
            pane.reset_view()

    def _on_control_panel_bg_changed(self, text): #vers 1
        colors = {
            "Default": None,
            "Black": (0, 0, 0),
            "White": (255, 255, 255),
            "Dark Grey": (40, 40, 40),
        }
        rgb = colors.get(text)
        for pane in getattr(self, '_world_panes', []):
            pane.set_bg_color_override(rgb)

    def _create_ipl_tab(self): #vers 1
        """[IPL] tab content, matching MooMapper's own "Item Placement"
        tab - the former standalone IPL Sections dock's table moves
        here unchanged, plus an Open/Close/New/Delete button row and
        INST/CULL/ZONE/PATH radio buttons at the bottom (only INST/
        CULL/ZONE are real - PATH isn't parsed anywhere in this
        project yet, honestly stubbed rather than guessed at without
        real sample data to verify against, the same caution applied
        to the VC/GTA3 inst-format work earlier).

        Column order is eye-icon first, then name (per Keith's
        request) - previously name then icon."""
        panel = QWidget()
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(1, 1, 1, 1)
        from apps.methods.imgfactory_svg_icons import get_remove_icon, get_file_icon, get_add_icon, get_close_icon
        from PyQt6.QtWidgets import QButtonGroup
        title_row = QHBoxLayout()
        label = QLabel("IPL Sections")
        label.setStyleSheet("font-weight: bold;")
        title_row.addWidget(label)
        sm_buttonheight = 20
        #_COMPACT_BUTTON_H = 18 #TODO; does not show the right size?
        #_COMPACT_ICON_SIZE = 18 TODO; Text less then min 18 and max 20, the text buttons get corrupted.

        icon_color = self._get_icon_color()
        open_btn = QPushButton(get_add_icon(sm_buttonheight, icon_color), "Open")
        open_btn.setToolTip("Load the selected IPL's content on demand -\n"
                            "same as clicking its eye icon to show it")
        open_btn.setIconSize(QSize(18, 18))
        open_btn.setMinimumHeight(18); open_btn.setMaximumHeight(28)
        open_btn.setMinimumWidth(40)
        open_btn.clicked.connect(self._on_ipl_tab_open_clicked)
        #open_btn.setFixedHeight(16)
        close_btn = QPushButton(get_close_icon(sm_buttonheight, icon_color), "Close")
        close_btn.setToolTip("Hide the selected IPL - same as clicking\n"
                             "its eye icon to hide it (its data stays\n"
                             "loaded, just not shown)")
        close_btn.setIconSize(QSize(18, 18))
        close_btn.setMinimumHeight(18); close_btn.setMaximumHeight(28)
        close_btn.setMinimumWidth(40)
        close_btn.clicked.connect(self._on_ipl_tab_close_clicked)
        #close_btn.setFixedHeight(16) #TODO; need new icon.
        new_btn = QPushButton(get_file_icon(sm_buttonheight, icon_color), "New")
        new_btn.setToolTip("STUB - creating a brand new, empty IPL file\n"
                           "on disk isn't built yet")
        new_btn.setIconSize(QSize(18, 18))
        new_btn.setMinimumHeight(18); new_btn.setMaximumHeight(28)
        new_btn.setMinimumWidth(40)
        new_btn.setEnabled(False)
        #new_btn.setFixedHeight(16) #TODO; need delete icon.
        delete_btn = QPushButton(get_remove_icon(sm_buttonheight, icon_color), "Delete")
        delete_btn.setToolTip("STUB - deleting an IPL file from disk isn't\n"
                              "built yet (no write-back infrastructure exists\n"
                              "for any file type in Map Workshop yet)")
        delete_btn.setIconSize(QSize(18, 18))
        delete_btn.setMinimumHeight(18); delete_btn.setMaximumHeight(28)
        delete_btn.setMinimumWidth(40)
        delete_btn.setEnabled(False)
        #delete_btn.setFixedHeight(16)
        for b in (open_btn, close_btn, new_btn, delete_btn):
            title_row.addWidget(b)
        title_row.addStretch()
        lay.addLayout(title_row)

        table = QTableWidget(0, 2)
        table.setHorizontalHeaderLabels(["", "IPL File"])
        header = table.horizontalHeader()
        header.setSectionResizeMode(0, QHeaderView.ResizeMode.Fixed)
        header.setSectionResizeMode(1, QHeaderView.ResizeMode.Interactive)
        header.setStretchLastSection(False)
        table.setColumnWidth(0, 18)
        saved_widths = self.map_settings.get('ipl_sections_column_widths') or []
        if len(saved_widths) >= 2:
            table.setColumnWidth(1, saved_widths[1])
        else:
            header.setSectionResizeMode(1, QHeaderView.ResizeMode.Stretch)
        header.sectionResized.connect(self._on_ipl_sections_column_resized)
        self._apply_compact_table_style(table)
        table.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
        table.setShowGrid(False)
        table.setToolTip("Toggle which IPL files' placements are shown in World View")
        table.cellClicked.connect(self._on_ipl_section_cell_clicked)
        table.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        table.customContextMenuRequested.connect(self._on_ipl_sections_context_menu)
        lay.addWidget(table)
        self._ipl_sections_table = table

        placeholder = QLabel("No world loaded yet")
        #placeholder.setStyleSheet("color: palette(mid);")
        lay.addWidget(placeholder)
        self._ipl_sections_placeholder = placeholder
        placeholder.setVisible(True)
        table.setVisible(False)

        # INST/CULL/ZONE/PATH data-type selector - switches which kind
        # of IPL content the "IPL Inst File" panel shows for the
        # currently selected IPL. INST/CULL/ZONE are real (all three
        # are already parsed - GTAWorldLoader.instances/culls/zones);
        # PATH is an honest stub, disabled with a tooltip explaining
        # why, rather than a guess at an unverified format.
        themecol = self.app_settings.get_theme_colors()
        panel_bg = themecol.get('panel_bg')
        type_box = QGroupBox()
        if panel_bg:
            type_box.setStyleSheet(f"QGroupBox {{ background: {panel_bg}; }}")
        type_lay = QVBoxLayout(type_box)
        self._ipl_type_group = QButtonGroup(panel)
        self._ipl_type_group.setExclusive(True)
        type_specs = [
            ('inst', "INST - Item Instances", True),
            ('cull', "CULL - Object Culling", True),
            ('zone', "ZONE - Map Zones", True),
            ('path', "PATH - Pedestrian / Vehicle Paths", False),
        ]
        for key, text, enabled in type_specs:
            radio = QRadioButton(text)
            radio.setChecked(key == 'inst')
            radio.setEnabled(enabled)
            if not enabled:
                radio.setToolTip("STUB - path node data isn't parsed anywhere\n"
                                 "in this project yet, no real sample data has\n"
                                 "been verified against yet to build this on")
            radio.toggled.connect(
                lambda checked, k=key: self._on_ipl_data_type_changed(k) if checked else None)
            self._ipl_type_group.addButton(radio)
            type_lay.addWidget(radio)
        lay.addWidget(type_box)
        self._ipl_data_type = 'inst'
        return panel

    def _populate_ipl_sections(self, loader): #vers 6
        """Fill the IPL Sections panel from a completed load - one row
        per IPL, in the user's saved display order (ipl_sections_order)
        if any, with new/unrecognised names (a different world/mod)
        appended alphabetically after the known ones rather than losing
        the saved order entirely.

        With lazy_ipl_loading enabled (the default for Map Workshop's
        own load paths), every IPL the .dat(s) reference is listed
        immediately from loader.available_ipls - matching MooMapper's
        own behaviour of listing every IPL path upfront without loading
        any of their content - rather than only showing IPLs that
        already have instances loaded. Falls back to the older
        instances-based derivation for any caller that doesn't use lazy
        loading."""
        table = getattr(self, '_ipl_sections_table', None)
        placeholder = getattr(self, '_ipl_sections_placeholder', None)
        if table is None:
            return

        self._all_instances = list(loader.instances)

        icon_color = self._get_icon_color()
        icon_sz = 16
        self._eye_open_icon = self._render_variant_icon('eye_visible', None, icon_sz,
                                                         icon_color, has_menu=False)
        self._eye_closed_icon = self._render_variant_icon('eye_hidden', None, icon_sz,
                                                           icon_color, has_menu=False)

        # display name (as it'll appear in the table / match inst.source_ipl
        # once loaded) -> lowercase stem (the key load_ipl_by_name expects)
        self._ipl_display_to_stem = {}
        if getattr(loader, 'lazy_ipl_loading', False):
            for stem, entry in loader.available_ipls.items():
                display_name = os.path.basename(entry.abs_path)
                self._ipl_display_to_stem[display_name] = stem
            current_names = set(self._ipl_display_to_stem.keys())
            # Nothing is actually loaded yet - every IPL starts hidden/
            # unloaded, matching MooMapper's own default state, rather
            # than showing everything as already visible.
            self._hidden_ipls = set(current_names)
        else:
            current_names = {inst.source_ipl for inst in loader.instances}
            self._hidden_ipls = set()

        saved_order = self.map_settings.get('ipl_sections_order') or []
        ordered = [n for n in saved_order if n in current_names]
        remaining = sorted(current_names - set(ordered))
        self._ipl_display_order = ordered + remaining

        self._rebuild_ipl_sections_rows()

        if placeholder is not None:
            placeholder.setVisible(False)
        table.setVisible(True)

    def _rebuild_ipl_sections_rows(self): #vers 1
        """(Re)build every row from self._ipl_display_order - shared by
        the initial populate and by _move_ipl_section, so reordering
        doesn't duplicate the row-construction logic."""
        table = self._ipl_sections_table
        table.setRowCount(len(self._ipl_display_order))
        hidden = getattr(self, '_hidden_ipls', set())
        for row, ipl_name in enumerate(self._ipl_display_order):
            is_hidden = ipl_name in hidden
            eye_item = QTableWidgetItem()
            eye_item.setIcon(self._eye_closed_icon if is_hidden else self._eye_open_icon)
            eye_item.setTextAlignment(Qt.AlignmentFlag.AlignCenter)
            eye_item.setToolTip(f"Show {ipl_name}" if is_hidden else f"Hide {ipl_name}")
            eye_item.setData(Qt.ItemDataRole.UserRole, ipl_name)
            table.setItem(row, 0, eye_item)
            name_item = QTableWidgetItem(ipl_name)
            self._style_ipl_name_item(name_item, is_hidden)
            table.setItem(row, 1, name_item)

    def _move_ipl_section(self, ipl_name, direction): #vers 1
        """Move one IPL section up (-1) or down (+1) in the display
        order, persisting the new order so it survives reloads."""
        order = getattr(self, '_ipl_display_order', None)
        if not order or ipl_name not in order:
            return
        idx = order.index(ipl_name)
        new_idx = idx + direction
        if not (0 <= new_idx < len(order)):
            return  # already at that end
        order[idx], order[new_idx] = order[new_idx], order[idx]
        self._rebuild_ipl_sections_rows()
        self.map_settings.set('ipl_sections_order', order)
        self.map_settings.save()

    def _on_ipl_sections_context_menu(self, pos): #vers 1
        """Right-click a row for Move Up/Down - explicit menu actions
        rather than drag-and-drop, since QTableWidget's built-in
        InternalMove drag-drop is a known source of subtle bugs with
        multi-column rows (data can end up split across the wrong
        rows/columns) - explicit actions are simple and reliable."""
        table = self._ipl_sections_table
        index = table.indexAt(pos)
        if not index.isValid():
            return
        item = table.item(index.row(), 0)
        if item is None:
            return
        ipl_name = item.data(Qt.ItemDataRole.UserRole)
        order = getattr(self, '_ipl_display_order', [])
        idx = order.index(ipl_name) if ipl_name in order else -1

        menu = QMenu(table)
        is_hidden = ipl_name in getattr(self, '_hidden_ipls', set())
        vis_act = menu.addAction("Show" if is_hidden else "Hide")
        vis_act.triggered.connect(
            lambda checked=False, r=index.row(): self._on_ipl_section_cell_clicked(r, 0))
        menu.addSeparator()
        up_act = menu.addAction("Move Up")
        up_act.setEnabled(idx > 0)
        up_act.triggered.connect(lambda checked=False, n=ipl_name: self._move_ipl_section(n, -1))
        down_act = menu.addAction("Move Down")
        down_act.setEnabled(0 <= idx < len(order) - 1)
        down_act.triggered.connect(lambda checked=False, n=ipl_name: self._move_ipl_section(n, 1))
        menu.exec(table.viewport().mapToGlobal(pos))

    def _on_ipl_sections_column_resized(self, logical_index, old_size, new_size): #vers 1
        """Persist the user's column widths for the IPL Sections table
        whenever they resize a column, so it doesn't reset to defaults
        on next open."""
        table = getattr(self, '_ipl_sections_table', None)
        if table is None:
            return
        widths = [table.columnWidth(c) for c in range(table.columnCount())]
        self.map_settings.set('ipl_sections_column_widths', widths)
        self.map_settings.save()

    def _on_object_browser_column_resized(self, logical_index, old_size, new_size): #vers 1
        """Persist the user's column widths for Object Browser whenever
        they resize a column, so it doesn't reset to defaults on next
        open. Column 2 (Model) is captured too even though it stays
        Stretch-managed and isn't restored on the next load - harmless
        to store, simpler than special-casing it out."""
        view = getattr(self, '_object_browser_view', None)
        if view is None:
            return
        widths = [view.columnWidth(c) for c in range(view.model().columnCount())]
        self.map_settings.set('object_browser_column_widths', widths)
        self.map_settings.save()

    def _on_object_browser_header_context_menu(self, pos): #vers 1
        """Right-click the Object Browser header to hide/show individual
        columns - per Keith's request. A checked item means the column
        is currently visible; unchecking hides it. Persisted to
        object_browser_hidden_columns so it survives to the next
        session."""
        view = getattr(self, '_object_browser_view', None)
        if view is None:
            return
        model = view.model()
        header = view.horizontalHeader()
        menu = QMenu(header)
        for col in range(model.columnCount()):
            label = model.headerData(col, Qt.Orientation.Horizontal) or f"Column {col}"
            act = menu.addAction(str(label))
            act.setCheckable(True)
            act.setChecked(not view.isColumnHidden(col))
            act.triggered.connect(
                lambda checked, c=col: self._on_object_browser_column_visibility_toggled(c, checked))
        menu.exec(header.mapToGlobal(pos))

    def _on_object_browser_column_visibility_toggled(self, col, visible): #vers 2
        view = getattr(self, '_object_browser_view', None)
        if view is None:
            return
        view.setColumnHidden(col, not visible)
        hidden = [c for c in range(view.model().columnCount()) if view.isColumnHidden(c)]
        self.map_settings.set('object_browser_hidden_columns', hidden)
        self.map_settings.save()
        self._recompute_object_browser_model_width()

    def _recompute_object_browser_model_width(self): #vers 1
        """Explicitly set the Model column (col 2) to fill whatever
        space is left after every other VISIBLE column, replacing
        QHeaderView.ResizeMode.Stretch - confirmed by direct
        measurement that Stretch mode wasn't reliably recalculating
        after columns were hidden/shown (Keith's report: the Model
        column widened correctly when he hid TXD/Instances/Size
        interactively, but reset to narrow after a reload - Stretch's
        automatic recalculation isn't dependable enough to build this
        on). Called on initial setup, on any column visibility toggle,
        and on the view's own resize (see eventFilter)."""
        view = getattr(self, '_object_browser_view', None)
        if view is None:
            return
        model = view.model()
        if model is None:
            return
        other_width = sum(view.columnWidth(c) for c in range(model.columnCount())
                          if c != 2 and not view.isColumnHidden(c))
        available = view.viewport().width()
        model_width = max(80, available - other_width)
        view.setColumnWidth(2, model_width)

    def _on_ipl_tab_open_clicked(self): #vers 1
        """Open button - loads the currently selected IPL's content on
        demand, same as clicking its eye icon to show it."""
        table = getattr(self, '_ipl_sections_table', None)
        if table is None or table.currentRow() < 0:
            return
        self._on_ipl_section_cell_clicked(table.currentRow(), 0)

    def _on_ipl_tab_close_clicked(self): #vers 1
        """Close button - hides the currently selected IPL, same as
        clicking its eye icon to hide it. Its data stays loaded (not
        re-parsed if reopened), only its instances stop being shown."""
        table = getattr(self, '_ipl_sections_table', None)
        if table is None or table.currentRow() < 0:
            return
        row = table.currentRow()
        item = table.item(row, 0)
        if item is None:
            return
        ipl_name = item.data(Qt.ItemDataRole.UserRole)
        if ipl_name not in getattr(self, '_hidden_ipls', set()):
            self._on_ipl_section_cell_clicked(row, 0)

    def _on_ipl_data_type_changed(self, data_type): #vers 1
        """INST/CULL/ZONE/PATH radio changed - updates which kind of
        data the IPL Inst File panel shows for the currently selected
        IPL. Only called for the real, enabled options (INST/CULL/
        ZONE) - PATH is disabled at the widget level since it isn't
        parsed anywhere in this project yet."""
        self._ipl_data_type = data_type
        self._refresh_ipl_inst_file_panel()

    def _create_ipl_inst_file_panel(self): #vers 1
        """New panel replacing the old standalone IPL Sections dock's
        physical location - shows the real, raw text content of
        whichever IPL is currently selected in the [IPL] tab, filtered
        to whichever data type (INST/CULL/ZONE) is selected there.
        Updates live as different IPL files are clicked, per Keith's
        request ("each ipl file we press changes the contents in the
        IPL inst file"). Re-reads the actual file from disk each time
        rather than reconstructing from parsed IPLInstance data, so
        it's always faithful to the real file (comments, exact
        formatting, sections not otherwise surfaced anywhere)."""
        panel = QWidget()
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(6, 6, 6, 6)
        from PyQt6.QtWidgets import QTextEdit
        text = QTextEdit()
        text.setReadOnly(True)
        font = text.font(); font.setFamily("monospace"); text.setFont(font)
        text.setPlaceholderText("Select an IPL file in the IPL tab to preview its contents here")
        lay.addWidget(text)
        self._ipl_inst_file_text = text

        dock = QDockWidget("IPL Inst File", self)
        dock.setObjectName("IPL Inst File")
        dock.setWidget(panel)
        dock.setFeatures(QDockWidget.DockWidgetFeature.DockWidgetMovable |
                        QDockWidget.DockWidgetFeature.DockWidgetFloatable)
        self._ipl_inst_file_dock = dock
        return dock

    def _refresh_ipl_inst_file_panel(self): #vers 1
        """Re-read and display the currently selected IPL's raw file
        content, filtered to the currently selected data type (INST/
        CULL/ZONE - PATH is stubbed, never reachable here since its
        radio button is disabled)."""
        text_widget = getattr(self, '_ipl_inst_file_text', None)
        table = getattr(self, '_ipl_sections_table', None)
        loader = getattr(self, '_world_loader', None)
        if text_widget is None or table is None or loader is None:
            return
        row = table.currentRow()
        if row < 0:
            text_widget.clear()
            return
        item = table.item(row, 0)
        if item is None:
            text_widget.clear()
            return
        display_name = item.data(Qt.ItemDataRole.UserRole)
        stem = getattr(self, '_ipl_display_to_stem', {}).get(display_name)
        entry = loader.available_ipls.get(stem) if stem else None
        if entry is None or not entry.exists:
            text_widget.setPlainText(f"({display_name} - file not found on disk)")
            return
        try:
            with open(entry.abs_path, 'r', encoding='ascii', errors='ignore') as f:
                raw_text = f.read()
        except Exception as e:
            text_widget.setPlainText(f"(could not read {display_name}: {e})")
            return

        data_type = getattr(self, '_ipl_data_type', 'inst')
        section_text = self._extract_ipl_section_text(raw_text, data_type)
        text_widget.setPlainText(section_text if section_text is not None else raw_text)

    def _extract_ipl_section_text(self, raw_text, section_name): #vers 1
        """Extract just one named section's lines (between the section
        keyword and its "end") from a raw IPL file's text - returns
        None if that section isn't present at all (falls back to
        showing the whole file), rather than an empty/misleading
        result."""
        lines = raw_text.splitlines()
        out = []
        in_section = False
        found = False
        for raw in lines:
            line = raw.split("#")[0].strip()
            low = line.lower()
            if not in_section and low == section_name:
                in_section = True
                found = True
                out.append(raw)
                continue
            if in_section:
                out.append(raw)
                if low == "end":
                    in_section = False
        return "\n".join(out) if found else None

    def _apply_compact_table_style(self, table): #vers 1
        """Apply the same compact row/header height Object Browser
        uses (font-metric-based: text height + 2px, not a hardcoded
        guess) to a table in one of the merged tabs - per Keith's
        report that the new tabs' header cells were taller than Object
        Browser's own. Reused instead of duplicating this logic in
        every tab's own creation method."""
        from PyQt6.QtGui import QFontMetrics
        row_h = QFontMetrics(table.font()).height() + 2
        table.verticalHeader().setVisible(False)
        table.verticalHeader().setMinimumSectionSize(row_h)
        table.verticalHeader().setDefaultSectionSize(row_h)
        header = table.horizontalHeader()
        header.setMinimumSectionSize(16)
        header.setFixedHeight(QFontMetrics(header.font()).height() + 2)

    def _create_ide_tab(self): #vers 2
        """[IDE] tab content, matching MooMapper's own "Object
        Definition" tab - lists every IDE file the .dat references
        (real data, drawn from GTAWorldLoader.load_log, which already
        tracks every IDE path encountered during loading), paths
        shortened relative to the game root. Clicking a row previews
        that file's real content below, mirroring the IPL tab's
        click-to-preview behaviour (per Keith's request that IDE files
        work "like the ipl links"). Edit/Save are honest stubs - no
        write-back infrastructure exists for any file type yet."""
        panel = QWidget()
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(1, 1, 1, 1)
        from apps.methods.imgfactory_svg_icons import get_save_icon, get_edit_icon
        from PyQt6.QtWidgets import QButtonGroup
        title_row = QHBoxLayout()
        sm_buttonheight = 20

        icon_color = self._get_icon_color()

        label = QLabel("IDE Definitions")
        label.setStyleSheet("font-weight: bold;")
        title_row.addWidget(label)
        #edit_btn.setStyleSheet("font-weight: bold;")
        edit_btn = QPushButton(get_edit_icon(sm_buttonheight, icon_color), "Edit")
        edit_btn.setToolTip("STUB - no IDE editing built yet")
        edit_btn.setIconSize(QSize(18, 18))
        edit_btn.setMinimumHeight(18); edit_btn.setMaximumHeight(28)
        edit_btn.setEnabled(False)
        #edit_btn.setFixedHeight(self._COMPACT_BUTTON_H)
        #save_btn.setStyleSheet("font-weight: bold;")
        save_btn = QPushButton(get_save_icon(sm_buttonheight, icon_color), "Save")
        save_btn.setToolTip("STUB - no write-back to disk exists for any\n"
                            "file type in Map Workshop yet")
        save_btn.setIconSize(QSize(18, 18))
        save_btn.setMinimumHeight(18); save_btn.setMaximumHeight(28)
        save_btn.setEnabled(False)
        #save_btn.setFixedHeight(self._COMPACT_BUTTON_H)
        title_row.addWidget(edit_btn)
        title_row.addWidget(save_btn)
        title_row.addStretch()
        lay.addLayout(title_row)
        table = QTableWidget(0, 2)
        table.setHorizontalHeaderLabels(["Ind.", "Filename"])
        table.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
        table.horizontalHeader().setSectionResizeMode(1, QHeaderView.ResizeMode.Stretch)
        table.cellClicked.connect(self._on_ide_tab_row_clicked)
        self._apply_compact_table_style(table)
        lay.addWidget(table)
        self._ide_tab_table = table
        from PyQt6.QtWidgets import QTextEdit
        preview = QTextEdit()
        preview.setReadOnly(True)
        font = preview.font(); font.setFamily("monospace"); preview.setFont(font)
        preview.setPlaceholderText("Click an IDE file above to preview its contents here")
        lay.addWidget(preview)
        self._ide_tab_preview = preview
        return panel

    def _refresh_ide_tab(self, loader): #vers 2
        """Populate the IDE file list - paths shortened relative to the
        game root folder (per Keith: "start from the game root folder
        to shorten the paths"), matching how IPL Sections already
        shows short names rather than full absolute paths."""
        table = getattr(self, '_ide_tab_table', None)
        if table is None:
            return
        game_root = getattr(self, '_game_root', None)
        ide_paths = [abs_path for phase, entry_type, abs_path, ok in loader.load_log
                    if entry_type == "IDE" and ok]
        table.setRowCount(len(ide_paths))
        self._ide_tab_paths = {}
        for i, path in enumerate(ide_paths):
            display_path = path
            if game_root:
                try:
                    display_path = os.path.relpath(path, game_root)
                except Exception:
                    display_path = path
            table.setItem(i, 0, QTableWidgetItem(str(i)))
            table.setItem(i, 1, QTableWidgetItem(display_path))
            self._ide_tab_paths[i] = path

    def _on_ide_tab_row_clicked(self, row, col): #vers 1
        """Preview the clicked IDE file's real raw text content - per
        Keith's request that IDE files work like IPL files (click to
        preview), mirroring _refresh_ipl_inst_file_panel's approach:
        re-read directly from disk each time rather than reconstructed
        from parsed data."""
        text_widget = getattr(self, '_ide_tab_preview', None)
        path = getattr(self, '_ide_tab_paths', {}).get(row)
        if text_widget is None or path is None:
            return
        try:
            with open(path, 'r', encoding='ascii', errors='ignore') as f:
                text_widget.setPlainText(f.read())
        except Exception as e:
            text_widget.setPlainText(f"(could not read {path}: {e})")

    def _create_dat_tab(self): #vers 1
        """[DAT] tab content, matching MooMapper's own "DAT Editor" -
        shows the real raw text of the loaded .dat file (comments,
        directive order, exactly as it appears on disk), re-read
        directly rather than reconstructed from parsed data. Edit/Save
        are honest stubs, same reasoning as the IDE tab."""
        panel = QWidget()
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(6, 6, 6, 6)
        title_row = QHBoxLayout()
        from apps.methods.imgfactory_svg_icons import get_save_icon, get_edit_icon
        from PyQt6.QtWidgets import QButtonGroup

        sm_buttonheight = 20

        icon_color = self._get_icon_color()

        label = QLabel("Dat Editor")
        label.setStyleSheet("font-weight: bold;")
        title_row.addWidget(label)

        edit_btn = QPushButton(get_edit_icon(sm_buttonheight, icon_color), "Edit")
        edit_btn.setToolTip("STUB - no .dat editing built yet")
        edit_btn.setIconSize(QSize(18, 18))
        edit_btn.setMinimumHeight(18); edit_btn.setMaximumHeight(28)
        edit_btn.setEnabled(False)
        #edit_btn.setFixedHeight(16)
        save_btn = QPushButton(get_save_icon(sm_buttonheight, icon_color), "Save")
        save_btn.setToolTip("STUB - no write-back to disk exists for any\n"
                            "file type in Map Workshop yet")
        save_btn.setIconSize(QSize(18, 18))
        save_btn.setMinimumHeight(18); save_btn.setMaximumHeight(28)
        save_btn.setEnabled(False)
        #save_btn.setFixedHeight(16)
        title_row.addWidget(edit_btn)
        title_row.addWidget(save_btn)
        title_row.addStretch()
        lay.addLayout(title_row)
        from PyQt6.QtWidgets import QTextEdit
        text = QTextEdit()
        text.setReadOnly(True)
        font = text.font(); font.setFamily("monospace"); text.setFont(font)
        text.setPlaceholderText("Load a game folder/DAT file to see its contents here")
        lay.addWidget(text)
        self._dat_tab_text = text
        return panel

    def _refresh_dat_tab(self): #vers 1
        text_widget = getattr(self, '_dat_tab_text', None)
        dat_path = getattr(self, '_loaded_dat_path', None)
        if text_widget is None:
            return
        if not dat_path:
            text_widget.clear()
            return
        try:
            with open(dat_path, 'r', encoding='ascii', errors='ignore') as f:
                text_widget.setPlainText(f.read())
        except Exception as e:
            text_widget.setPlainText(f"(could not read {dat_path}: {e})")

    def _create_img_tab(self): #vers 1
        """[IMG] tab content, matching MooMapper's own "IMG Archive"
        tab - numbered sub-tabs (one per loaded IMG archive), each
        showing that archive's real entry list (name/type/size) via
        the already-built ModelCache index. Extract/Add/Del/Rename are
        honest stubs - these are real file-writing operations, and no
        write-back infrastructure exists for any file type in Map
        Workshop yet, IMG included."""
        panel = QWidget()
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(1, 1, 1, 1)
        from apps.methods.imgfactory_svg_icons import get_rebuild_icon, get_rename_icon, get_remove_icon, get_add_icon, get_dump_icon

        title_row = QHBoxLayout()
        sm_buttonheight = 20
        icon_color = self._get_icon_color()

        label = QLabel("IMG File")
        label.setStyleSheet("font-weight: bold;")
        title_row.addWidget(label)

        ext_btn = QPushButton(get_dump_icon(sm_buttonheight, icon_color), "Extract")
        ext_btn.setToolTip("STUB - no .dat editing built yet")
        ext_btn.setIconSize(QSize(18, 18))
        ext_btn.setMinimumHeight(18); ext_btn.setMaximumHeight(28)
        ext_btn.setMinimumWidth(40)
        ext_btn.setEnabled(False)
        ext_btn.setFixedHeight(16)
        add_btn = QPushButton(get_add_icon(sm_buttonheight, icon_color), "Add")
        add_btn.setToolTip("STUB - no .dat editing built yet")
        add_btn.setIconSize(QSize(18, 18))
        add_btn.setMinimumHeight(18); add_btn.setMaximumHeight(28)
        add_btn.setMinimumWidth(40)
        add_btn.setEnabled(False)
        add_btn.setFixedHeight(16)
        del_btn = QPushButton(get_remove_icon(sm_buttonheight, icon_color), "Del")
        del_btn.setToolTip("STUB - no .dat editing built yet")
        del_btn.setIconSize(QSize(18, 18))
        del_btn.setMinimumHeight(18); del_btn.setMaximumHeight(28)
        del_btn.setMinimumWidth(40)
        del_btn.setEnabled(False)
        del_btn.setFixedHeight(16)
        ren_btn = QPushButton(get_rename_icon(sm_buttonheight, icon_color), "Rename")
        ren_btn.setToolTip("STUB - no .dat editing built yet")
        ren_btn.setIconSize(QSize(18, 18))
        ren_btn.setMinimumHeight(18); ren_btn.setMaximumHeight(28)
        ren_btn.setMinimumWidth(40)
        ren_btn.setEnabled(False)
        ren_btn.setFixedHeight(16)
        save_btn = QPushButton(get_rebuild_icon(sm_buttonheight, icon_color), "Rebuild")
        save_btn.setToolTip("STUB - no write-back to disk exists for any\n"
                            "file type in Map Workshop yet")
        save_btn.setIconSize(QSize(18, 18))
        save_btn.setMinimumHeight(18); save_btn.setMaximumHeight(28)
        save_btn.setMinimumWidth(40)
        save_btn.setEnabled(False)
        save_btn.setFixedHeight(16)
        for b in (ext_btn, add_btn, del_btn, ren_btn, save_btn):
            title_row.addWidget(b)
        title_row.addStretch()
        lay.addLayout(title_row)

        img_tabs = QTabWidget()
        img_tabs.setTabPosition(QTabWidget.TabPosition.North)
        from PyQt6.QtGui import QFontMetrics
        tab_font = img_tabs.font()
        img_tabs.tabBar().setStyleSheet(
            f"QTabBar::tab {{ height: {QFontMetrics(tab_font).height() + 6}px; padding: 2px 8px; }}")
        lay.addWidget(img_tabs)
        self._img_tab_tabs = img_tabs
        return panel

    def _refresh_img_tab(self, loader): #vers 1
        """Rebuild the numbered IMG sub-tabs from the loaded world's
        actual IMG archives (loader.get_img_paths()) - one table per
        archive, listing every entry's real name/type/size read
        directly from the archive itself."""
        img_tabs = getattr(self, '_img_tab_tabs', None)
        if img_tabs is None:
            return
        img_tabs.clear()
        from apps.methods.img_core_classes import IMGFile
        for i, img_path in enumerate(loader.get_img_paths(), 1):
            table = QTableWidget(0, 4)
            table.setHorizontalHeaderLabels(["Ind.", "Filename", "Type", "Size"])
            table.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
            table.horizontalHeader().setSectionResizeMode(1, QHeaderView.ResizeMode.Stretch)
            self._apply_compact_table_style(table)
            try:
                img = IMGFile(img_path)
                if img.open():
                    table.setRowCount(len(img.entries))
                    for row, entry in enumerate(img.entries):
                        table.setItem(row, 0, QTableWidgetItem(str(row)))
                        table.setItem(row, 1, QTableWidgetItem(entry.name))
                        table.setItem(row, 2, QTableWidgetItem(entry.extension.upper() + " File"
                                                                if entry.extension else "File"))
                        size_kb = max(1, entry.size // 1024)
                        table.setItem(row, 3, QTableWidgetItem(f"{size_kb} kB"))
            except Exception:
                pass
            img_tabs.addTab(table, str(i))

    def _create_editing_panel_dock(self): #vers 1
        """New tabbed "Editing Panel" dock - IDE/IPL/DAT/IMG, matching
        MooMapper's own tabbed IMG Archive/Object Definition/Item
        Placement structure - replaces the previous standalone IPL
        Sections dock (its content now lives in the [IPL] tab, moved
        rather than duplicated). [IPL] is the only fully real,
        interactive tab so far; [IDE] and [DAT] show real data
        (IDE file list, raw .dat text) but can't edit/save yet;
        [IMG] shows real per-archive entry lists but can't
        Extract/Add/Del/Rename yet - none of this project has any
        write-back-to-disk infrastructure yet, for any file type."""
        icon_color = self._get_icon_color()
        tabs = QTabWidget()
        tabs.addTab(self._create_ide_tab(),
                   self._render_variant_icon('tab_ide', None, 24, icon_color, has_menu=False), "IDE")
        tabs.addTab(self._create_ipl_tab(),
                   self._render_variant_icon('tab_ipl', None, 24, icon_color, has_menu=False), "IPL")
        tabs.addTab(self._create_dat_tab(),
                   self._render_variant_icon('tab_dat', None, 24, icon_color, has_menu=False), "DAT")
        tabs.addTab(self._create_img_tab(),
                   self._render_variant_icon('tab_img', None, 24, icon_color, has_menu=False), "IMG")
        tabs.setCurrentIndex(1)   # [IPL] is the main working tab so far

        dock = QDockWidget("Editing Panel", self)
        dock.setObjectName("Editing Panel")
        dock.setWidget(tabs)
        dock.setFeatures(QDockWidget.DockWidgetFeature.DockWidgetMovable |
                        QDockWidget.DockWidgetFeature.DockWidgetFloatable)
        self._editing_panel_dock = dock
        self._editing_panel_tabs = tabs
        return dock

    def _on_ipl_section_cell_clicked(self, row, col): #vers 4
        """Clicking the eye-icon cell (col 0) toggles that IPL's
        visibility - plain item click rather than a button, so there's
        no button widget/chrome to size or pad. Clicking anywhere else
        on the row just selects it, which is enough to refresh the IPL
        Inst File panel below (per Keith's request that pressing any
        IPL file updates that panel's content, independent of
        toggling visibility).

        With lazy IPL loading, toggling to visible for the first time
        also triggers the actual on-demand load of that IPL's content
        (_ensure_ipl_loaded) - matching MooMapper's model of not
        touching an IPL until the user asks for it."""
        table = self._ipl_sections_table
        item = table.item(row, 0)
        if item is None:
            return
        if col == 0:
            ipl_name = item.data(Qt.ItemDataRole.UserRole)
            hidden = ipl_name in getattr(self, '_hidden_ipls', set())
            new_hidden = not hidden
            if not new_hidden:
                self._ensure_ipl_loaded(ipl_name)
            item.setIcon(self._eye_closed_icon if new_hidden else self._eye_open_icon)
            item.setToolTip(f"Show {ipl_name}" if new_hidden else f"Hide {ipl_name}")
            name_item = table.item(row, 1)
            if name_item is not None:
                self._style_ipl_name_item(name_item, new_hidden)
            self._toggle_ipl_section(ipl_name, new_hidden)
        self._refresh_ipl_inst_file_panel()

    def _ensure_ipl_loaded(self, display_name): #vers 2
        """Actually load one IPL's content on demand, the first time
        it's toggled visible - parses its instances (GTAWorldLoader.
        load_ipl_by_name), refreshes self._all_instances/Object Browser
        with the newly-added data, and pre-loads (with a progress
        dialog scoped to just this IPL's models, not the whole world)
        the geometry/textures those new instances reference. A no-op
        if this IPL was already loaded (or lazy loading isn't active
        at all, e.g. a non-Map-Workshop caller).

        Reports per-IPL success/error status matching Keith's requested
        format ("path/airport.ipl loaded - no errors" / "path/
        airportN.ipl loaded - N errors found, check log added to the
        maps folder") - writes an actual .log file alongside the IPL
        itself when there are errors, rather than only showing a
        transient status message."""
        loader = getattr(self, '_world_loader', None)
        if loader is None or not getattr(loader, 'lazy_ipl_loading', False):
            return
        stem = getattr(self, '_ipl_display_to_stem', {}).get(display_name)
        if stem is None or stem in loader.loaded_ipls:
            return   # not a lazily-tracked IPL, or already loaded

        before_count = len(loader.instances)
        result = loader.load_ipl_by_name(stem)
        if not result.success:
            self._set_status(f"{display_name} failed to load"
                             + (f" - {result.errors[0]}" if result.errors else ""))
            return

        problem_count = result.error_count + result.warning_count
        if problem_count:
            log_path = self._write_ipl_error_log(result)
            self._set_status(
                f"{display_name} loaded - {problem_count} issue(s) found"
                + (f", check {os.path.basename(log_path)} added to the maps folder"
                   if log_path else ""))
        else:
            self._set_status(f"{display_name} loaded - no errors "
                             f"({result.instance_count} instances)")

        new_instances = loader.instances[before_count:]

        self._all_instances = list(loader.instances)
        self._populate_object_browser(loader)

        model_cache = getattr(self, '_model_cache', None)
        if model_cache is not None and new_instances:
            self._preload_world_assets(
                loader, model_cache, instances=new_instances,
                title=f"Loading {display_name}…")

    def _write_ipl_error_log(self, result): #vers 1
        """Write a plain-text log of one IPL's parse errors/warnings
        alongside the IPL file itself (same folder), per Keith's
        request ("check log added to the maps folder"). Returns the
        written path, or None if writing failed (in which case the
        errors are still visible via the status message and the
        loader's own accumulated stats - this is a convenience, not the
        only place the information lives)."""
        if not result.abs_path:
            return None
        try:
            folder = os.path.dirname(result.abs_path)
            base = os.path.splitext(os.path.basename(result.abs_path))[0]
            log_path = os.path.join(folder, f"{base}_load_errors.log")
            with open(log_path, 'w', encoding='utf-8') as f:
                f.write(f"Load errors for {result.abs_path}\n")
                f.write(f"{result.error_count} error(s), {result.warning_count} warning(s)\n\n")
                if result.errors:
                    f.write("Errors:\n")
                    for e in result.errors:
                        f.write(f"  - {e}\n")
                if result.warnings:
                    f.write("\nWarnings:\n")
                    for w in result.warnings:
                        f.write(f"  - {w}\n")
            return log_path
        except Exception:
            return None

    def _style_ipl_name_item(self, name_item, hidden): #vers 3
        """Grey out a disabled/hidden IPL's name text, per Keith's
        request, so a hidden entry is visually distinct at a glance,
        not just via its eye icon.

        Doesn't rely on QPalette's Disabled colour group - confirmed
        that resolves incorrectly here (this app's dark theme is
        applied via QSS stylesheet, which leaves the underlying
        QPalette's Active and Disabled Text roles both reporting white -
        a known Qt quirk where stylesheet-driven theming doesn't keep
        the palette object itself in sync). Instead blends the active
        text colour 50% toward the window background colour
        programmatically, which dims correctly under any theme."""
        pal = self.palette()
        text_color = pal.color(pal.ColorGroup.Active, pal.ColorRole.Text)
        if hidden:
            bg_color = pal.color(pal.ColorGroup.Active, pal.ColorRole.Window)
            color = QColor(
                (text_color.red()   + bg_color.red())   // 2,
                (text_color.green() + bg_color.green()) // 2,
                (text_color.blue()  + bg_color.blue())  // 2)
        else:
            color = text_color
        name_item.setForeground(QBrush(color))

    def _toggle_ipl_section(self, ipl_name, hidden): #vers 1
        """Show/hide all instances from one IPL file - recomputes the
        visible instance subset from the full loaded set and re-feeds
        it to the world-view panes and Instance List, without needing
        to reload from disk."""
        if hidden:
            self._hidden_ipls.add(ipl_name)
        else:
            self._hidden_ipls.discard(ipl_name)
        self._apply_ipl_visibility_filter()

    def _apply_ipl_visibility_filter(self): #vers 2
        """Recompute which instances are currently visible: every
        loaded instance whose source_ipl isn't in self._hidden_ipls,
        then layer LOD show/hide on top (global toggle + any per-
        instance overrides) - push the final result to the world-view
        panes and Instance List."""
        all_inst = getattr(self, '_all_instances', None)
        if all_inst is None:
            return
        hidden = getattr(self, '_hidden_ipls', set())
        visible = [i for i in all_inst if i.source_ipl not in hidden] if hidden else all_inst
        visible = self._apply_lod_filter(visible)
        for pane in getattr(self, '_world_panes', []):
            pane.set_instances(visible)
        loader_stub = _FilteredLoaderStub(visible, getattr(self, '_world_loader', None))
        self._populate_instance_list(loader_stub)

    def _apply_lod_filter(self, instances): #vers 2
        """Given an already-IPL-filtered instance list, decide for each
        LOD-paired primary instance which version(s) to keep - per-
        instance override (self._lod_overrides, keyed by id(primary
        instance), one of 'normal'/'lod'/'both'/None) takes precedence
        over the global mode (self._lod_display_mode). Instances with
        no LOD pair at all pass through unchanged."""
        pairs = getattr(self, '_lod_pairs', None)
        if not pairs:
            return instances
        overrides = getattr(self, '_lod_overrides', {})
        global_mode = getattr(self, '_lod_display_mode', 'normal')
        paired_target_ids = {id(v) for v in pairs.values()}
        result = []
        for inst in instances:
            iid = id(inst)
            if iid in pairs:
                mode = overrides.get(iid) or global_mode
                if mode == 'lod':
                    result.append(pairs[iid])
                elif mode == 'both':
                    result.append(inst)
                    result.append(pairs[iid])
                else:  # 'normal'
                    result.append(inst)
            elif iid in paired_target_ids:
                # This instance IS someone's LOD target - it's only
                # included via its primary above (in 'lod'/'both' mode),
                # to avoid duplicates when both members of a pair pass
                # the IPL filter.
                continue
            else:
                result.append(inst)
        return result

    def _set_lod_display_mode(self, mode): #vers 1
        """Global LOD display mode - 'normal' (default), 'lod', or
        'both'. Per-instance overrides (set via the Instance List)
        still take precedence over this for any instance they cover."""
        self._lod_display_mode = mode
        self._apply_ipl_visibility_filter()

    def _create_object_browser_dock(self): #vers 2
        """Object Browser dock - Add/Delete/Rename icon row, search +
        filter (All/Most Used/Favourites/Generic) over the loaded
        object catalog. Double-click a row to toggle its favourite
        status (persisted to map_settings)."""
        from PyQt6.QtWidgets import QTableView, QLineEdit, QPushButton, QButtonGroup, QToolButton, QStackedWidget
        from apps.methods.imgfactory_svg_icons import get_add_icon, get_trash_icon, get_rename_icon

        OBJECT_BROWSER_ICON_SIZE = 18
        OBJECT_BROWSER_BUTTON_W = 18
        OBJECT_BROWSER_BUTTON_H = 18
        panel = QWidget()

        main_layout = QHBoxLayout(panel)
        main_layout.setContentsMargins(4, 4, 4, 4)

        # Per Keith: "I think there is a hidden splitter in the middle
        # between both panes, I cant resize the objects browser
        # window" - there was: a QSplitter with an empty, entirely
        # unused right_panel (nothing was ever added to its layout)
        # taking up half the space and interfering with resizing.
        # Removed entirely - left_panel's content goes straight into
        # main_layout now.
        left_panel = QWidget()
        left_layout = QVBoxLayout(left_panel)
        left_layout.setContentsMargins(4, 4, 4, 4)
        main_layout.addWidget(left_panel)

        # Compact single-row layout: search field (stretching) + mode
        # icon buttons (All/Most Used/Favourites/Generic) + Add/Del/
        # Rename icons, replacing what was previously three separate
        # rows (search, then Add/Del/Rename, then text-label mode
        # buttons) - per Keith's TODOs asking for exactly this.
        icon_color = self._get_icon_color()

        top_row_widget = QWidget()
        top_row = QHBoxLayout(top_row_widget)
        top_row.setContentsMargins(0, 0, 0, 0)

        group = QButtonGroup(panel)
        group.setExclusive(True)
        self._object_mode_buttons = {}

        # IMG/DAT/IDE/IPL - compact colored-text buttons (the label
        # text itself IS the icon, no separate graphic - per Keith's
        # request to save space) for the tabs merged in from the old
        # standalone Editing Panel dock. Order matches Keith's request:
        # IMG, DAT, IDE, IPL, then All/Most Used/Favourites/Generic.
        tab_colors = {'img': '#c060e0', 'dat': '#e0a030', 'ide': '#4090e0', 'ipl': '#40b060'}
        tab_labels = {'img': "IMG", 'dat': "DAT", 'ide': "IDE", 'ipl': "IPL"}
        self._object_browser_tab_buttons = {}
        for tab_key in ('img', 'dat', 'ide', 'ipl'):
            btn = QToolButton()
            btn.setText(tab_labels[tab_key])
            btn.setStyleSheet(f"QToolButton {{ color: {tab_colors[tab_key]}; font-weight: bold; }}")
            btn.setToolTip(f"{tab_labels[tab_key]} tab")
            btn.setCheckable(True)
            btn.setFixedHeight(OBJECT_BROWSER_BUTTON_H)
            btn.clicked.connect(lambda checked, k=tab_key: self._on_object_browser_tab_changed(k))
            group.addButton(btn)
            top_row.addWidget(btn)
            self._object_browser_tab_buttons[tab_key] = btn

        mode_icon_shapes = {'all': 'list_all', 'most_used': 'bars_most_used',
                            'favourites': 'star_filled', 'generic': 'box_generic'}
        mode_labels = {'all': "All", 'most_used': "Most Used",
                      'favourites': "Favourites", 'generic': "Generic"}
        self._object_mode_button_text_widths = {}
        for mode in ('all', 'most_used', 'favourites', 'generic'):
            icon = self._render_variant_icon(mode_icon_shapes[mode], None,
                                             OBJECT_BROWSER_ICON_SIZE, icon_color,
                                             has_menu=False)
            btn = QToolButton()
            btn.setIcon(icon)
            btn.setText(mode_labels[mode])
            btn.setToolTip(mode_labels[mode])
            btn.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonIconOnly)
            btn.setCheckable(True)
            btn.setChecked(mode == 'all')
            btn.setFixedHeight(OBJECT_BROWSER_BUTTON_H)
            btn.setMinimumWidth(OBJECT_BROWSER_ICON_SIZE + 8)   # icon-only size floor - without
                                                                # this the icon+text minimumSizeHint
                                                                # (~80-100px per button) forces the
                                                                # whole dock's minimum width way up,
                                                                # blocking narrower resizing entirely
            btn.clicked.connect(lambda checked, m=mode: self._on_object_mode_changed(m))
            group.addButton(btn)
            top_row.addWidget(btn)
            self._object_mode_buttons[mode] = btn
            # Estimated width needed in icon+text mode, computed directly
            # from font metrics rather than by actually switching the
            # button's style to measure it (avoids flicker/cascading
            # resize events - see _update_mode_button_style).
            from PyQt6.QtGui import QFontMetrics
            text_w = QFontMetrics(btn.font()).horizontalAdvance(mode_labels[mode])
            self._object_mode_button_text_widths[mode] = OBJECT_BROWSER_ICON_SIZE + text_w + 16

        # Add/Delete/Rename icon row - previously these actions only
        # existed as right-click context menu entries with no visible
        # affordance at all (Keith couldn't find them in a screenshot -
        # rightly so, since nothing hinted they existed). Real, visible
        # SVG icon buttons, operating on whichever row is currently
        # selected; disabled entirely when nothing is selected.
        action_row_widget = QWidget()
        action_row = QHBoxLayout(action_row_widget)
        action_row.setContentsMargins(0, 0, 0, 0)
        self._ob_add_btn = QPushButton(get_add_icon(OBJECT_BROWSER_ICON_SIZE, icon_color), "")
        self._ob_add_btn.setToolTip("Add Instance Here - place another copy of the\n"
                                    "selected model at the origin")
        self._ob_add_btn.clicked.connect(self._on_object_browser_add_clicked)
        self._ob_del_btn = QPushButton(get_trash_icon(18, icon_color), "")
        self._ob_del_btn.setToolTip("Delete All Instances of the selected model")
        self._ob_del_btn.clicked.connect(self._on_object_browser_delete_clicked)
        self._ob_rename_btn = QPushButton(get_rename_icon(18, icon_color), "")
        self._ob_rename_btn.setToolTip("Rename the selected object")
        self._ob_rename_btn.clicked.connect(self._on_object_browser_rename_clicked)
        for btn in (self._ob_add_btn, self._ob_del_btn, self._ob_rename_btn):
            btn.setFixedSize(OBJECT_BROWSER_BUTTON_W, OBJECT_BROWSER_BUTTON_H)
            btn.setEnabled(False)
            action_row.addWidget(btn)
        # Only shown when docked - in standalone mode the titlebar's own
        # Add/Del/Rename icons cover this, so showing both would be
        # redundant (per Keith's explicit request). The mode buttons
        # above stay visible either way - they aren't duplicated
        # anywhere else, unlike Add/Del/Rename.
        action_row_widget.setVisible(not self.standalone_mode)
        self._ob_action_row_widget = action_row_widget
        top_row.addWidget(action_row_widget)

        self._ob_top_row_widget = top_row_widget
        top_row_widget.installEventFilter(self)
        QTimer.singleShot(0, self._update_mode_button_style)
        left_layout.addWidget(top_row_widget)

        view = QTableView()
        from PyQt6.QtGui import QFontMetrics
        OBJECT_BROWSER_ROW_HEIGHT = QFontMetrics(view.font()).height() + 2   # text height + 2px
        view.verticalHeader().setMinimumSectionSize(OBJECT_BROWSER_ROW_HEIGHT)
        view.verticalHeader().setDefaultSectionSize(
            OBJECT_BROWSER_ROW_HEIGHT
        )
        header = view.horizontalHeader()
        header.setFixedHeight(QFontMetrics(header.font()).height() + 2)
        header.setMinimumSectionSize(16)   # Qt's own default was silently clamping the narrow ★/ID widths upward
        header.setSectionResizeMode(0, QHeaderView.ResizeMode.Interactive)
        header.setSectionResizeMode(1, QHeaderView.ResizeMode.Interactive)
        # Model (col 2) used to be QHeaderView.ResizeMode.Stretch, but
        # that didn't reliably recalculate after hiding/showing other
        # columns (confirmed by direct measurement, not just assumed) -
        # explicit, directly-computed width instead (see
        # _recompute_object_browser_model_width), recalculated on
        # setup, column visibility changes, and view resize.
        header.setSectionResizeMode(2, QHeaderView.ResizeMode.Interactive)
        header.sectionResized.connect(self._on_object_browser_column_resized)
        header.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        header.customContextMenuRequested.connect(self._on_object_browser_header_context_menu)
        view.verticalHeader().setVisible(False)
        view.setEditTriggers(QTableView.EditTrigger.NoEditTriggers)
        view.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        view.doubleClicked.connect(self._on_object_row_double_clicked)
        view.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        view.customContextMenuRequested.connect(self._on_object_browser_context_menu)
        view.installEventFilter(self)
        model = _ObjectBrowserModel()
        view.setModel(model)
        # Narrow defaults for ★/ID (per Keith: "Fav * cell needs to be
        # narrow, 5px of the ID, Model should fit the name") - just
        # enough to show a star glyph and a typical 4-5 digit model ID,
        # freeing the rest of the row width for Model. Saved widths (if
        # any) override these below.
        view.setColumnWidth(0, 20)
        view.setColumnWidth(1, 45)
        saved_widths = self.map_settings.get('object_browser_column_widths') or []
        for col, width in enumerate(saved_widths):
            if col < model.columnCount() and col != 2:   # 2 (Model) is computed, not restored
                header.setSectionResizeMode(col, QHeaderView.ResizeMode.Interactive)
                view.setColumnWidth(col, width)
        hidden_cols = self.map_settings.get('object_browser_hidden_columns') or []
        for col in hidden_cols:
            if col < model.columnCount():
                view.setColumnHidden(col, True)
        QTimer.singleShot(0, self._recompute_object_browser_model_width)
        sel_model = view.selectionModel()
        if sel_model is not None:
            sel_model.currentRowChanged.connect(
                lambda cur, prev: self._on_object_row_selected(cur.row()))
        self._object_browser_view = view
        self._object_browser_model = model

        # Stacked content area - page 0 is the Object Browser table
        # itself (All/Most Used/Favourites/Generic modes all share this
        # one page, just filtering its rows); pages 1-4 are the tabs
        # merged in from the former standalone Editing Panel dock.
        content_stack = QStackedWidget()
        content_stack.addWidget(view)
        content_stack.addWidget(self._create_ide_tab())
        content_stack.addWidget(self._create_ipl_tab())
        content_stack.addWidget(self._create_dat_tab())
        content_stack.addWidget(self._create_img_tab())
        left_layout.addWidget(content_stack)
        self._object_browser_content_stack = content_stack

        dock = QDockWidget("Object Browser", self)
        dock.setObjectName("Object Browser")
        dock.setWidget(panel)
        dock.setFeatures(QDockWidget.DockWidgetFeature.DockWidgetMovable |
                        QDockWidget.DockWidgetFeature.DockWidgetFloatable)

        title_bar = QWidget()
        title_lay = QHBoxLayout(title_bar)
        title_lay.setContentsMargins(6, 2, 6, 2)
        title_label = QLabel("Object Browser")
        title_lay.addWidget(title_label)
        search = QLineEdit()
        search.setPlaceholderText("Search objects…")
        search.setMinimumWidth(40)
        search.textChanged.connect(self._on_object_search_changed)
        title_lay.addWidget(search, 1)
        self._object_search_edit = search
        dock.setTitleBarWidget(title_bar)

        self._object_browser_dock = dock
        return dock

    def _populate_object_browser(self, loader): #vers 1
        """Fill the Object Browser from a completed load - instance
        counts per model (for Most Used) computed once here, not
        recomputed on every filter change."""
        model = getattr(self, '_object_browser_model', None)
        if model is None:
            return
        model.set_model_cache(getattr(self, '_model_cache', None))
        from collections import Counter
        counts = Counter(inst.model_id for inst in loader.instances)
        favourites = self.map_settings.get('favourite_objects') or []
        model.set_objects(list(loader.objects.values()), counts, favourites)

    def _on_object_search_changed(self, text): #vers 1
        model = getattr(self, '_object_browser_model', None)
        if model is not None:
            model.set_search(text)

    def _on_object_mode_changed(self, mode): #vers 2
        model = getattr(self, '_object_browser_model', None)
        if model is not None:
            model.set_mode(mode)
        stack = getattr(self, '_object_browser_content_stack', None)
        if stack is not None:
            stack.setCurrentIndex(0)   # table page - IMG/DAT/IDE/IPL buttons show pages 1-4

    def _on_object_browser_tab_changed(self, tab_key): #vers 1
        """IMG/DAT/IDE/IPL button clicked - switches the shared content
        stack to that tab's page instead of the Object Browser table,
        per Keith's request to merge the former standalone Editing
        Panel dock's tabs in here."""
        stack = getattr(self, '_object_browser_content_stack', None)
        if stack is None:
            return
        page_index = {'ide': 1, 'ipl': 2, 'dat': 3, 'img': 4}.get(tab_key)
        if page_index is not None:
            stack.setCurrentIndex(page_index)

    def _on_object_row_selected(self, row): #vers 2
        """Selecting a model row in the (now merged) Object Browser
        centres the camera + shows the edit panel on that model's
        FIRST placement, if it has any - with Prev/Next cycling stored
        for models with multiple instances. Models with zero instances
        (defined in the IDE but never placed) show identity/IDE/2DFX/
        TOBJ info in the panel without any placement-specific data,
        since there's no instance to show it for. Also enables/disables
        the Add/Delete/Rename icon row based on selection state."""
        if row < 0:
            self._selected_object_model_id = None
            self._update_object_browser_action_buttons()
            return
        model = self._object_browser_model
        obj = model.object_at(row)
        if obj is None:
            self._selected_object_model_id = None
            self._update_object_browser_action_buttons()
            return
        self._selected_object_model_id = obj.model_id
        self._update_object_browser_action_buttons()
        loader = getattr(self, '_world_loader', None)
        instances = loader.get_instances_for_model(obj.model_id) if loader else []
        if not instances:
            self._current_model_instances = []
            self._current_instance_index = 0
            return
        self._current_model_instances = instances
        self._current_instance_index = 0
        self._center_on_instance(instances[0], nav_info=(0, len(instances)))

    def _update_object_browser_action_buttons(self): #vers 1
        """Enable/disable the Add/Delete/Rename icon buttons to match
        the current selection - Add and Rename work regardless of
        instance count, Delete additionally needs at least one
        instance to remove."""
        mid = getattr(self, '_selected_object_model_id', None)
        has_selection = mid is not None
        self._ob_add_btn.setEnabled(has_selection)
        self._ob_rename_btn.setEnabled(has_selection)
        instance_count = 0
        if has_selection:
            model = self._object_browser_model
            instance_count = model._instance_counts.get(mid, 0)
        self._ob_del_btn.setEnabled(has_selection and instance_count > 0)

    def _on_object_browser_add_clicked(self): #vers 1
        mid = getattr(self, '_selected_object_model_id', None)
        if mid is not None:
            self._add_instance_of_model(mid)
            self._update_object_browser_action_buttons()

    def _on_object_browser_delete_clicked(self): #vers 1
        mid = getattr(self, '_selected_object_model_id', None)
        if mid is not None:
            self._delete_all_instances_of_model(mid)
            self._update_object_browser_action_buttons()

    def _on_object_browser_rename_clicked(self): #vers 1
        mid = getattr(self, '_selected_object_model_id', None)
        if mid is not None:
            self._rename_object(mid)

    def _cycle_model_instance(self, direction): #vers 1
        """Prev (-1) / Next (+1) through the currently-selected model's
        placements, wrapping around at either end."""
        instances = getattr(self, '_current_model_instances', [])
        if not instances:
            return
        idx = (getattr(self, '_current_instance_index', 0) + direction) % len(instances)
        self._current_instance_index = idx
        self._center_on_instance(instances[idx], nav_info=(idx, len(instances)))

    def _on_object_browser_context_menu(self, pos): #vers 2
        """Right-click a row for Favourites, Rename, Add Instance, and
        Delete All Instances - the Add/Del/Rename functions Keith asked
        to continue. These are in-memory operations for now (mutating
        self._all_instances and the loader's own instances list, kept
        consistent with each other) - writing changes back to the
        actual IPL/IDE files on disk isn't built yet, tracked
        separately in TODO.md."""
        view = self._object_browser_view
        index = view.indexAt(pos)
        if not index.isValid():
            return
        model = self._object_browser_model
        obj = model.object_at(index.row())
        if obj is None:
            return

        menu = QMenu(view)
        is_fav = obj.model_id in model._favourites
        fav_act = menu.addAction("Remove from Favourites" if is_fav else "Add to Favourites")
        fav_act.triggered.connect(lambda checked=False, mid=obj.model_id:
                                  self._toggle_instance_favourite(mid))
        menu.addSeparator()
        rename_act = menu.addAction("Rename Object…")
        rename_act.triggered.connect(lambda checked=False, mid=obj.model_id:
                                     self._rename_object(mid))
        add_act = menu.addAction("Add Instance Here…")
        add_act.triggered.connect(lambda checked=False, mid=obj.model_id:
                                  self._add_instance_of_model(mid))
        instance_count = model._instance_counts.get(obj.model_id, 0)
        del_act = menu.addAction(f"Delete All Instances ({instance_count})")
        del_act.setEnabled(instance_count > 0)
        del_act.triggered.connect(lambda checked=False, mid=obj.model_id:
                                  self._delete_all_instances_of_model(mid))
        menu.exec(view.viewport().mapToGlobal(pos))

    def _rename_object(self, model_id): #vers 1
        """Rename an object's model_name - IN MEMORY ONLY for now
        (updates the IDEObject held by the current GTAWorldLoader and
        refreshes Object Browser's display). Does NOT yet write the
        new name back to the actual IDE file, or update GTA3/VC's
        text-format IPL lines that redundantly store the name (SA's
        format doesn't) - that needs real file-writing infrastructure,
        tracked separately."""
        loader = getattr(self, '_world_loader', None)
        if loader is None:
            return
        obj = loader.get_object(model_id)
        if obj is None:
            return
        new_name, ok = QInputDialog.getText(
            self, "Rename Object", "New name:", text=obj.model_name)
        if not ok or not new_name.strip():
            return
        obj.model_name = new_name.strip()
        for inst in getattr(self, '_all_instances', []):
            if inst.model_id == model_id:
                inst.model_name = obj.model_name
        self._object_browser_model._recompute()
        self._set_status(f"Renamed object {model_id} to '{obj.model_name}' "
                         f"(in memory only - not yet written to disk)")

    def _add_instance_of_model(self, model_id): #vers 1
        """Add a new placement of an existing model - IN MEMORY ONLY
        for now, at a default position (origin, identity rotation).
        Does NOT yet write the new inst line back to the actual IPL
        file - that needs real file-writing infrastructure, tracked
        separately. Keith's fuller Add request (importing new DFF/TXD/
        COL from the desktop into the game's IMG for models that don't
        exist at all yet) is a bigger, separate piece - this covers
        placing another copy of a model that's already loaded."""
        loader = getattr(self, '_world_loader', None)
        obj = loader.get_object(model_id) if loader else None
        if obj is None:
            return
        from apps.methods.gta_dat_parser import IPLInstance
        new_inst = IPLInstance(
            model_id=model_id, model_name=obj.model_name, interior=0,
            pos_x=0.0, pos_y=0.0, pos_z=0.0,
            rot_x=0.0, rot_y=0.0, rot_z=0.0, rot_w=1.0,
            lod_index=-1, source_ipl="(added this session)", line_no=0)
        self._all_instances = getattr(self, '_all_instances', [])
        self._all_instances.append(new_inst)
        if loader is not None:
            loader.instances.append(new_inst)
        counts = getattr(self._object_browser_model, '_instance_counts', {})
        counts[model_id] = counts.get(model_id, 0) + 1
        self._object_browser_model._recompute()
        self._apply_ipl_visibility_filter()
        self._center_on_instance(new_inst)
        self._set_status(f"Added a new instance of '{obj.model_name}' at the origin "
                         f"(in memory only - not yet written to disk)")

    def _delete_all_instances_of_model(self, model_id): #vers 1
        """Remove every placement of a model - the simpler 'just
        remove from IPL' case Keith described (leaves the IDE
        definition, COL, and IMG-packed DFF/TXD untouched, so other
        references to the same model_id, if any, stay valid). IN
        MEMORY ONLY for now - doesn't yet rewrite the actual IPL
        file(s) on disk, or offer the 'purge everything and free the
        ID' alternative Keith also described (that needs IDE/COL/IMG
        write support this project doesn't have yet)."""
        loader = getattr(self, '_world_loader', None)
        all_inst = getattr(self, '_all_instances', [])
        removed = sum(1 for i in all_inst if i.model_id == model_id)
        if removed == 0:
            return
        confirm = QMessageBox.question(
            self, "Delete All Instances",
            f"Remove all {removed} placement(s) of this model from the "
            f"currently loaded world?\n\n"
            f"This only removes the placements - the object definition, "
            f"collision, and model/texture files are left untouched.\n\n"
            f"(In-memory only for now - not yet written back to the "
            f"actual IPL file(s) on disk.)",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if confirm != QMessageBox.StandardButton.Yes:
            return
        self._all_instances = [i for i in all_inst if i.model_id != model_id]
        if loader is not None:
            loader.instances[:] = [i for i in loader.instances if i.model_id != model_id]
        counts = getattr(self._object_browser_model, '_instance_counts', {})
        counts.pop(model_id, None)
        self._object_browser_model._recompute()
        self._apply_ipl_visibility_filter()
        self._set_status(f"Removed {removed} placement(s) "
                         f"(in memory only - not yet written to disk)")

    def _on_object_row_double_clicked(self, index): #vers 1
        """Double-click a row to toggle its favourite status."""
        model = getattr(self, '_object_browser_model', None)
        if model is None:
            return
        obj = model.object_at(index.row())
        if obj is None:
            return
        new_favourites = model.toggle_favourite(obj.model_id)
        self.map_settings.set('favourite_objects', new_favourites)
        self.map_settings.save()

    def _create_instance_list_dock(self): #vers 3
        """Instance List dock - a browsable table (ID + Model only) of
        every loaded world placement. Single-click (or keyboard
        navigation) shows/updates the non-modal object edit panel for
        that instance; double-click additionally centres the camera in
        all three World View panes on it.

        QTableView + a lazy model (_InstanceTableModel) rather than
        QTableWidget - the old QTableWidget approach eagerly created a
        QTableWidgetItem per cell (5 per instance) and resolved every
        instance's TXD name immediately, timed at 2.6s of UI freeze for
        51,711 instances. The model defers both to data() calls for
        whatever's actually visible."""
        from PyQt6.QtWidgets import QTableView

        view = QTableView()
        view.horizontalHeader().setSectionResizeMode(
            0, QHeaderView.ResizeMode.ResizeToContents)
        view.horizontalHeader().setSectionResizeMode(
            1, QHeaderView.ResizeMode.Stretch)
        view.verticalHeader().setVisible(False)
        view.setEditTriggers(QTableView.EditTrigger.NoEditTriggers)
        view.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        view.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        view.customContextMenuRequested.connect(self._on_instance_list_context_menu)
        view.doubleClicked.connect(self._on_instance_row_double_clicked)
        self._instance_table = view

        dock = QDockWidget("Instance List", self)
        dock.setObjectName("Instance List")
        dock.setWidget(view)
        dock.setFeatures(QDockWidget.DockWidgetFeature.DockWidgetMovable |
                        QDockWidget.DockWidgetFeature.DockWidgetFloatable)
        self._instance_list_dock = dock
        return dock

    def _find_lod_primary_key(self, instance): #vers 1
        """Given an instance currently displayed in a row (which may be
        either the 'primary' or its LOD-paired counterpart, depending
        on the current global/override mode), find the id() key to use
        with self._lod_overrides - always the primary's id(), whichever
        member is actually showing right now."""
        pairs = getattr(self, '_lod_pairs', None)
        if not pairs:
            return None
        iid = id(instance)
        if iid in pairs:
            return iid
        for primary_id, target in pairs.items():
            if target is instance:
                return primary_id
        return None

    def _on_instance_row_double_clicked(self, index): #vers 3
        """Double-click does the same as single-click (centre camera +
        gizmo + edit panel) - kept as a separate handler since it's
        also the natural place to land double-click-on-the-rendered-
        marker-in-the-viewport picking, once that's built."""
        model = self._instance_table.model()
        inst = model.instance_at(index.row()) if model else None
        if inst is None:
            return
        self._center_on_instance(inst)

    def _on_viewport_instance_picked(self, inst, pane): #vers 2
        """Called when the user clicks directly on a rendered marker in
        one specific World View pane - zooms in close on that instance
        WITHIN THAT PANE ONLY, and shows the edit panel.

        Fixes two things Keith reported: clicking an object was
        repositioning all 3 views (the previous version called
        _center_on_instance, which loops over every pane in
        self._world_panes) instead of responding to just the pane that
        was actually clicked; and it wasn't zooming in at all, only
        panning to centre while leaving distance/zoom completely
        untouched, so the object could still appear small/far away
        after being 'centred'."""
        pane._pan_x = -inst.pos_x
        pane._pan_y = -inst.pos_y
        pane._dist = 15.0   # close-up distance - a reasonable default,
                            # not tuned against real object sizes
        if pane._projection == 'ortho':
            try:
                pane.resizeGL(pane.width(), pane.height())   # refresh ortho half_h for the new dist
            except Exception:
                pass
        pane.set_gizmo_position((inst.pos_x, inst.pos_y, inst.pos_z))
        pane.update()
        self._show_instance_edit_panel(inst)

    def _center_on_instance(self, inst, nav_info=None): #vers 2
        """Centre all three World View panes' cameras on an instance,
        show an XYZ gizmo at its position, and show/update its edit
        panel - the shared behaviour for both single- and double-
        clicking an Instance List row, and (since the Object Browser
        merge) selecting a model row with one or more placements.
        nav_info, if given, is (current_index, total_count) for
        Prev/Next cycling through a model's other placements."""
        for pane in getattr(self, '_world_panes', []):
            pane._pan_x = -inst.pos_x
            pane._pan_y = -inst.pos_y
            pane.set_gizmo_position((inst.pos_x, inst.pos_y, inst.pos_z))
        self._show_instance_edit_panel(inst, nav_info)

    def _show_instance_edit_panel(self, inst, nav_info=None): #vers 2
        """Show (creating on first use) the non-modal object edit panel
        for one instance, positioned in the top-left corner of the
        window - stays open and gets its content refreshed for
        whichever instance is currently selected, rather than a modal
        dialog that blocks interaction and needs reopening each time."""
        panel = getattr(self, '_instance_edit_panel', None)
        if panel is None:
            panel = _InstanceEditPanel(self)
            self._instance_edit_panel = panel
        panel.show_for_instance(inst, getattr(self, '_world_loader', None), nav_info,
                                getattr(self, '_model_cache', None))
        top_level = self.window()
        panel.move(top_level.mapToGlobal(top_level.rect().topLeft()) +
                   QPoint(8, 8))
        panel.show()
        panel.raise_()

    def _on_instance_edited(self, inst): #vers 1
        """Called by _InstanceEditPanel whenever a position/rotation
        nudge changes an instance in memory - refreshes the World View
        panes to reflect the new position (they cache instance data as
        plain tuples for fast rendering, built at the last filter
        application, so mutating the IPLInstance alone doesn't
        propagate until this re-applies the current filter)."""
        self._apply_ipl_visibility_filter()

    def _on_instance_list_context_menu(self, pos): #vers 2
        """Right-click a row - Add/Remove Favourites always available
        (reusing the same favourite_objects setting Object Browser
        uses, so favouriting stays in sync between both panels);
        per-instance LOD override options only added when this
        instance actually has a resolved LOD pair (see
        resolve_lod_pairs)."""
        view = self._instance_table
        index = view.indexAt(pos)
        if not index.isValid():
            return
        model = view.model()
        inst = model.instance_at(index.row())
        if inst is None:
            return

        menu = QMenu(view)
        favourites = self.map_settings.get('favourite_objects') or []
        is_fav = inst.model_id in favourites
        fav_act = menu.addAction("Remove from Favourites" if is_fav else "Add to Favourites")
        fav_act.triggered.connect(
            lambda checked=False, mid=inst.model_id: self._toggle_instance_favourite(mid))

        primary_key = self._find_lod_primary_key(inst)
        if primary_key is not None:
            menu.addSeparator()
            current = getattr(self, '_lod_overrides', {}).get(primary_key)
            for mode, label in (('normal', "Show Normal"), ('lod', "Show LOD"),
                                ('both', "Show Both")):
                act = menu.addAction(label)
                act.setCheckable(True)
                act.setChecked(current == mode)
                act.triggered.connect(
                    lambda checked=False, k=primary_key, m=mode: self._set_lod_override(k, m))
            clear_act = menu.addAction("Use Global Setting")
            clear_act.setEnabled(current is not None)
            clear_act.triggered.connect(
                lambda checked=False, k=primary_key: self._set_lod_override(k, None))

        menu.exec(view.viewport().mapToGlobal(pos))

    def _toggle_instance_favourite(self, model_id): #vers 1
        """Add/remove a model_id from favourite_objects - shared with
        Object Browser's own favourite toggle, so favouriting an object
        from either panel stays in sync. Refreshes Object Browser's
        model if it's currently showing the Favourites filter."""
        favourites = set(self.map_settings.get('favourite_objects') or [])
        if model_id in favourites:
            favourites.discard(model_id)
        else:
            favourites.add(model_id)
        self.map_settings.set('favourite_objects', sorted(favourites))
        self.map_settings.save()
        ob_model = getattr(self, '_object_browser_model', None)
        if ob_model is not None:
            ob_model._favourites = favourites
            ob_model._recompute()

    def _set_lod_override(self, primary_key, mode): #vers 1
        """Set (or clear, if mode is None) a per-instance LOD override
        for one specific pair, keyed by the primary instance's id()."""
        overrides = getattr(self, '_lod_overrides', None)
        if overrides is None:
            return
        if mode is None:
            overrides.pop(primary_key, None)
        else:
            overrides[primary_key] = mode
        self._apply_ipl_visibility_filter()

    def _populate_instance_list(self, loader): #vers 2
        """Point the Instance List view at a completed GTAWorldLoader
        load's instances - the model resolves each instance's TXD name
        (via loader.get_object) lazily, per-cell, rather than all up
        front for every row regardless of whether it's ever scrolled
        into view."""
        view = getattr(self, '_instance_table', None)
        if view is None:
            return
        model = _InstanceTableModel(loader.instances, loader)
        view.setModel(model)
        sel_model = view.selectionModel()
        if sel_model is not None:
            sel_model.currentRowChanged.connect(
                lambda cur, prev: self._on_instance_row_selected(model, cur.row()))

    def _on_instance_row_selected(self, model, row): #vers 4
        """Single-click (or keyboard navigation) centres the camera,
        shows an XYZ gizmo, and shows/updates the object edit panel for
        the newly-selected instance - same as double-click; both share
        _center_on_instance()."""
        if row < 0:
            return
        inst = model.instance_at(row)
        if inst is None:
            return
        self._center_on_instance(inst)

    def _apply_viewport_movement_settings(self, pane, label): #vers 1
        """Apply the configured pan-button/rotate-button/invert-axis
        settings for one World View pane, based on its current view
        label (Top/Side/Front/3D each get their own invert settings,
        since their different camera orientations don't necessarily
        need the same correction)."""
        invert = self.map_settings.get('viewport_pan_invert') or {}
        axis = invert.get(label, {'x': False, 'y': False})
        pane.configure_movement(
            pan_button=self.map_settings.get('viewport_pan_button'),
            rotate_button=self.map_settings.get('viewport_rotate_button'),
            invert_x=axis.get('x', False),
            invert_y=axis.get('y', False))

    def _create_world_viewport_dock(self): #vers 1
        """Top/Side/3D triple-pane world viewport, in its own dock so it
        can sit alongside the still-present paint canvas rather than
        replacing it outright. Same MapViewport class, camera/pane-lock
        contract, and right-click-to-reassign pattern as Model
        Workshop's existing DFFViewport quad viewport - reused here
        rather than inventing a new multi-pane scheme."""
        try:
            from apps.components.Map_Editor.depends.map_viewport import MapViewport
        except Exception as e:
            print(f"[MapWorkshop] MapViewport unavailable: {e}")
            return None

        presets = [("Top", 0, 0, 'ortho'),
                   ("Side", 90, 0, 'ortho'),
                   ("3D", 45, 25, 'perspective')]
        self._world_panes = []
        for label, yaw, pitch, proj in presets:
            pane = MapViewport()
            pane.set_view_lock(proj == 'ortho', label, yaw=yaw, pitch=pitch,
                               projection=proj)
            self._apply_viewport_movement_settings(pane, label)
            pane.set_pick_callback(self._on_viewport_instance_picked)
            pane.setContextMenuPolicy(Qt.ContextMenuPolicy.NoContextMenu)
            pane._label_widget.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
            pane._label_widget.customContextMenuRequested.connect(
                lambda pos, p=pane: self._show_world_pane_menu(p, p._label_widget.mapToGlobal(pos)))
            # Double-click to maximize/restore - same pattern as Model
            # Workshop's quad viewport, via event filter rather than
            # touching MapViewport's own mouse handlers.
            pane.installEventFilter(self)
            self._world_panes.append(pane)

        splitter = QSplitter(Qt.Orientation.Horizontal)
        for pane in self._world_panes:
            splitter.addWidget(pane)
        self._world_splitter = splitter
        self._maximized_world_pane = None

        dock = QDockWidget("World View", self)
        dock.setObjectName("World View")
        dock.setWidget(splitter)
        dock.setFeatures(QDockWidget.DockWidgetFeature.DockWidgetMovable |
                        QDockWidget.DockWidgetFeature.DockWidgetFloatable)
        self._world_view_dock = dock

        # Default to showing only 3D, per Keith's request ("I suggest
        # showing 3d only, unless the user wants to change the
        # viewpoint") - reuses the existing maximize/restore mechanism
        # (previously only reachable by double-clicking a pane) rather
        # than inventing a separate "default view" concept. Double-
        # click any pane to restore all 3; right-click a pane's label
        # to reassign which view it shows.
        self._toggle_world_pane_maximize(self._world_panes[2])   # index 2 = "3D"

        return dock

    def eventFilter(self, obj, event): #vers 3
        """No longer triggers maximize/restore on double-click - that
        conflicted with click-to-pick (Keith reported clicking to pick
        an object was reverting the view to all 3 panes, which is
        exactly what an accidental or perceived double-click during a
        pick attempt would do via the old trigger here). Maximize/
        Restore remains available via the right-click menu on a
        pane's label (_show_world_pane_menu), so no functionality is
        lost - just the ambiguous, easily-mistaken gesture.

        Also watches the Object Browser mode-button row for resize
        events, switching its buttons between icon+text and icon-only
        depending on available width - per Keith's request that these
        buttons show icon+text normally but fall back to icon-only
        when space is limited. And watches the Object Browser table
        view itself for resize events, recomputing the Model column's
        width to fill available space (see
        _recompute_object_browser_model_width)."""
        if obj is getattr(self, '_ob_top_row_widget', None):
            from PyQt6.QtCore import QEvent
            if event.type() == QEvent.Type.Resize:
                QTimer.singleShot(0, self._update_mode_button_style)
        elif obj is getattr(self, '_object_browser_view', None):
            from PyQt6.QtCore import QEvent
            if event.type() == QEvent.Type.Resize:
                QTimer.singleShot(0, self._recompute_object_browser_model_width)
        return super().eventFilter(obj, event)

    def _update_mode_button_style(self): #vers 2
        """Switch the Object Browser mode buttons (All/Most Used/
        Favourites/Generic) between icon+text and icon-only, based on
        whether the row currently has enough width to show all four
        with their text labels. Defaults to icon-only (per Keith:
        "show icons only, unless someone widens the pane, then switch
        to icons and text"), switching to icon+text only once there's
        actually enough room.

        Uses each button's pre-cached estimated text-mode width
        (computed once from font metrics at creation time) rather than
        actually switching styles to measure - the previous version
        temporarily forced icon+text mode first to measure the row's
        real needed width, which could itself trigger another resize
        event (changing a button's style changes its size hint) and
        end up comparing against stale geometry from before Qt's
        layout system had finished recalculating - the likely cause of
        the truncated-text bug Keith reported (buttons stuck showing
        partial text like "Most U", "Favouri" instead of cleanly
        switching to icon-only)."""
        buttons = getattr(self, '_object_mode_buttons', None)
        text_widths = getattr(self, '_object_mode_button_text_widths', None)
        row_widget = getattr(self, '_ob_top_row_widget', None)
        action_widget = getattr(self, '_ob_action_row_widget', None)
        tab_buttons = getattr(self, '_object_browser_tab_buttons', None)
        if not buttons or not text_widths or row_widget is None:
            return
        text_style = Qt.ToolButtonStyle.ToolButtonTextBesideIcon
        icon_style = Qt.ToolButtonStyle.ToolButtonIconOnly

        needed = sum(text_widths.values())
        if tab_buttons:
            needed += sum(btn.sizeHint().width() for btn in tab_buttons.values())
        if action_widget is not None and action_widget.isVisible():
            needed += action_widget.sizeHint().width()
        available = row_widget.width()

        target_style = text_style if available >= needed else icon_style
        for btn in buttons.values():
            if btn.toolButtonStyle() != target_style:
                btn.setToolButtonStyle(target_style)

    def _toggle_world_pane_maximize(self, pane): #vers 2
        """Maximize the given world-view pane to fill the whole dock
        (hiding its siblings), or restore all 3 if already maximized.
        Simpler than Model Workshop's version - one flat splitter here,
        not a nested 2x2 grid, so just hiding the other panes is enough.

        Switches the maximized pane's label to 'Full View' - that label
        is also the pane's exclusive right-click-menu trigger (see
        _create_world_viewport_dock), so without it there'd be no
        labelled target to right-click while maximized. Restores the
        original view-name label (Top/Side/3D) when un-maximized."""
        panes = getattr(self, '_world_panes', None)
        if not panes:
            return
        if getattr(self, '_maximized_world_pane', None) is pane:
            for p in panes:
                p.setVisible(True)
            self._maximized_world_pane = None
            restore_label = getattr(pane, '_pre_maximize_label', None) or pane._view_label
            pane._label_widget.setText(restore_label)
            pane._label_widget.adjustSize()
            return
        for p in panes:
            p.setVisible(p is pane)
        self._maximized_world_pane = pane
        pane._pre_maximize_label = pane._view_label
        pane._label_widget.setText("Full View")
        pane._label_widget.adjustSize()

    def _show_world_pane_menu(self, pane, global_pos): #vers 3
        """Right-click a world-view pane's LABEL to reassign its view -
        same idea as Model Workshop's quad-pane menu - plus Maximize/
        Restore, for discoverability alongside the double-click
        shortcut. Right-clicking anywhere else on the pane does the
        normal rotate-drag instead. global_pos is already in screen
        coordinates, mapped by the caller from the label widget it was
        actually clicked on, not the pane itself."""
        options = [("Top", 0, 0, 'ortho'),
                   ("Side", 90, 0, 'ortho'),
                   ("Front", 0, 90, 'ortho'),
                   ("3D", 45, 25, 'perspective')]
        menu = QMenu(pane)
        for label, yaw, pitch, proj in options:
            act = menu.addAction(label)
            act.triggered.connect(
                lambda checked=False, l=label, y=yaw, p=pitch, pr=proj:
                    self._assign_world_pane_view(pane, l, y, p, pr))
        menu.addSeparator()
        is_max = getattr(self, '_maximized_world_pane', None) is pane
        max_act = menu.addAction("Restore All Panes" if is_max else "Maximize This Pane")
        max_act.triggered.connect(
            lambda checked=False, p=pane: self._toggle_world_pane_maximize(p))
        menu.exec(global_pos)

    def _assign_world_pane_view(self, pane, label, yaw, pitch, projection): #vers 2
        """Apply a user-chosen preset to one world-view pane. If this
        pane is currently maximized, re-apply the 'Full View' label
        afterward - set_view_lock always writes the new view name into
        the label, which would otherwise silently drop the full-view
        indication while still actually maximized. Also re-applies
        movement settings for the new label, since Top/Side/Front/3D
        can each have their own pan-invert configuration."""
        pane.set_view_lock(projection == 'ortho', label, yaw=yaw, pitch=pitch,
                            projection=projection)
        self._apply_viewport_movement_settings(pane, label)
        if getattr(self, '_maximized_world_pane', None) is pane:
            pane._pre_maximize_label = label
            pane._label_widget.setText("Full View")
            pane._label_widget.adjustSize()



    #    File I/O                                                               

    def _open_zoom_lens(self): #vers 2
        """Embedded overlay zoom lens — top-left corner of the canvas scroll area.
        Never drops behind. Resizable by scroll wheel or +/- buttons when hovered.
        Toggle: calling again hides/shows."""
        # Toggle if already open
        existing = getattr(self, '_zoom_lens', None)
        if existing:
            existing.setVisible(not existing.isVisible())
            if existing.isVisible():
                existing.raise_()
                existing._refresh()
            return

        from PyQt6.QtWidgets import QWidget, QVBoxLayout, QHBoxLayout, QLabel, QPushButton
        from PyQt6.QtCore import Qt, QTimer, QRect
        from PyQt6.QtGui import QImage, QPixmap, QPainter, QColor, QPen

        # Parent = scroll area viewport so it stays inside and never goes behind
        sa = getattr(self, '_canvas_scroll', None)
        if not sa:
            return
        parent_vp = sa.viewport()

        class _ZoomOverlay(QWidget):
            """Overlay lens widget — parented to canvas viewport, top-left anchored."""

            def __init__(self, workshop):  #vers 1
                super().__init__(parent_vp)
                self._ws   = workshop
                self._mag  = [8]    # mutable magnification
                self._sz   = [180]  # overlay pixel size (square)
                self.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents, False)
                self.setMouseTracking(True)
                self._hovered = False
                self._drag_start = None
                self._rebuild()
                self._timer = QTimer(self)
                self._timer.timeout.connect(self._refresh)
                self._timer.start(80)
                self._place()

            def _rebuild(self):  #vers 1
                sz = self._sz[0]
                self.setFixedSize(sz, sz + 22)   # +22 for header bar

            def _place(self):  #vers 1
                self.move(4, 4)

            def _refresh(self):  #vers 1
                if not self.isVisible(): return
                ws = self._ws
                canvas = getattr(ws, 'map_canvas', None)
                if not canvas: return
                try:
                    mag = self._mag[0]
                    # Use mouse position if available, fall back to scroll centre
                    if hasattr(canvas, '_zoom_lens_pos'):
                        cx, cy = canvas._zoom_lens_pos
                    else:
                        z   = getattr(canvas, 'zoom', 1)
                        sa2 = getattr(ws, '_canvas_scroll', None)
                        if sa2:
                            cx = int((sa2.horizontalScrollBar().value()
                                      + sa2.viewport().width() // 2) / max(1, z))
                            cy = int((sa2.verticalScrollBar().value()
                                      + sa2.viewport().height() // 2) / max(1, z))
                        else:
                            cx, cy = canvas.tex_w // 2, canvas.tex_h // 2

                    tw, th = canvas.tex_w, canvas.tex_h
                    sz  = self._sz[0]
                    lw  = max(1, sz // mag); lh = max(1, sz // mag)
                    x0  = max(0, min(cx - lw // 2, tw - lw))
                    y0  = max(0, min(cy - lh // 2, th - lh))
                    x1, y1 = x0 + lw, y0 + lh

                    rgba     = bytes(canvas.rgba)
                    crop_w   = x1 - x0; crop_h = y1 - y0
                    if crop_w <= 0 or crop_h <= 0: return

                    cropped = bytearray(crop_h * crop_w * 4)
                    for row in range(crop_h):
                        s = ((y0 + row) * tw + x0) * 4
                        d = row * crop_w * 4
                        cropped[d:d + crop_w * 4] = rgba[s:s + crop_w * 4]

                    qi = QImage(bytes(cropped), crop_w, crop_h,
                                crop_w * 4, QImage.Format.Format_RGBA8888)
                    self._pixmap = QPixmap.fromImage(qi).scaled(
                        sz, sz,
                        Qt.AspectRatioMode.IgnoreAspectRatio,
                        Qt.TransformationMode.FastTransformation)
                    self._info = f"{cx},{cy}  {mag}×"
                    self.update()
                except Exception:
                    pass

            def paintEvent(self, ev):  #vers 1
                from PyQt6.QtGui import QPainter, QColor, QPen, QFont
                p = QPainter(self)
                if not p.isActive():
                    return
                sz = self._sz[0]
                pal = self.palette()
                # Header uses toolbar bg so it matches the rest of the UI
                _hdr_bg  = pal.color(pal.ColorRole.Window)
                _hdr_txt = pal.color(pal.ColorRole.BrightText)
                _hint_c  = pal.color(pal.ColorRole.PlaceholderText)
                _brd_c   = pal.color(pal.ColorRole.Highlight)
                _vp_bg   = pal.color(pal.ColorRole.Base)
                # Header bar
                p.fillRect(0, 0, sz, 22, _hdr_bg)
                p.setPen(_hdr_txt)
                f = QFont("Arial", 8); p.setFont(f)
                info = getattr(self, '_info', 'Zoom Lens')
                # SVG zoom icon drawn as simple geometric shapes
                p.setBrush(_hdr_txt)
                p.setPen(Qt.PenStyle.NoPen)
                # Circle
                p.drawEllipse(3, 4, 10, 10)
                p.setBrush(_hdr_bg)
                p.drawEllipse(5, 6, 6, 6)
                # Handle
                p.setBrush(_hdr_txt)
                p.drawRect(12, 13, 5, 2)
                p.setPen(_hdr_txt)
                p.setFont(QFont("Arial", 8))
                p.drawText(20, 15, info)
                # [+] [-] zoom magnification buttons
                btn_w, btn_h = 18, 14
                btn_y = 4
                # [-] button
                minus_x = sz - btn_w - 2
                p.setBrush(_hint_c); p.setPen(Qt.PenStyle.NoPen)
                p.drawRoundedRect(minus_x, btn_y, btn_w, btn_h, 2, 2)
                p.setPen(_hdr_bg)
                p.setFont(QFont("Arial", 8, QFont.Weight.Bold))
                p.drawText(minus_x + 5, btn_y + 11, "-")
                # [+] button
                plus_x = minus_x - btn_w - 2
                p.setBrush(_hint_c); p.setPen(Qt.PenStyle.NoPen)
                p.drawRoundedRect(plus_x, btn_y, btn_w, btn_h, 2, 2)
                p.setPen(_hdr_bg)
                p.drawText(plus_x + 4, btn_y + 11, "+")
                # Lens image
                pm = getattr(self, '_pixmap', None)
                if pm:
                    p.drawPixmap(0, 22, pm)
                    # Tint overlay — right-click to set
                    tint_a = getattr(self, '_tint_alpha', 0)
                    if tint_a > 0:
                        from PyQt6.QtGui import QColor as _TC
                        tc = QColor(getattr(self, '_tint_color',
                                            QColor(255, 255, 255)))
                        tc.setAlpha(tint_a)
                        p.fillRect(0, 22, sz, sz, tc)
                else:
                    p.fillRect(0, 22, sz, sz, _vp_bg)
                # Resize grip — bottom-right corner dots
                p.setPen(_hint_c)
                for gx, gy in [(sz-4,sz+18),(sz-4,sz+14),(sz-8,sz+18)]:
                    p.drawEllipse(gx, gy, 2, 2)
                # Border
                p.setPen(QPen(_brd_c, 1))
                p.drawRect(0, 0, sz - 1, sz + 22 - 1)
                # Crosshair
                mid = sz // 2
                p.setPen(QPen(QColor(255, 80, 80, 160), 1))
                p.drawLine(mid - 8, 22 + mid, mid + 8, 22 + mid)
                p.drawLine(mid, 22 + mid - 8, mid, 22 + mid + 8)
                p.end()

            def _change_mag(self, delta):  #vers 1
                self._mag[0] = max(2, min(32, self._mag[0] + delta))
                self._refresh()

            def _change_size(self, delta):  #vers 1
                self._sz[0] = max(100, min(400, self._sz[0] + delta))
                self._rebuild()
                self.update()

            def wheelEvent(self, ev):  #vers 1
                """Scroll to resize the overlay panel."""
                delta = 1 if ev.angleDelta().y() > 0 else -1
                self._change_size(delta * 20)
                ev.accept()

            def mousePressEvent(self, ev):  #vers 1
                sz = self._sz[0]
                x, y = ev.position().x(), ev.position().y()
                total_h = sz + 22
                # Resize grip: bottom-right 14×14 corner
                if x > sz - 14 and y > total_h - 14:
                    self._resize_drag  = True
                    self._resize_start = (ev.globalPosition().toPoint(), sz)
                    ev.accept(); return
                self._resize_drag = False
                if y < 22:   # header — check [+] [-] or start drag
                    btn_w = 18
                    minus_x = sz - btn_w - 2
                    plus_x  = minus_x - btn_w - 2
                    if x >= minus_x:
                        self._change_mag(-1)
                    elif x >= plus_x:
                        self._change_mag(1)
                    else:
                        self._drag_start = ev.globalPosition().toPoint() - self.pos()
                ev.accept()

            def contextMenuEvent(self, ev):  #vers 1
                """Right-click: tint controls."""
                from PyQt6.QtWidgets import QMenu, QWidgetAction, QSlider, QLabel, QColorDialog
                menu = QMenu(self)
                menu.addAction("No tint",   lambda: self._set_tint(0))
                menu.addAction("Light tint (25%)", lambda: self._set_tint(64))
                menu.addAction("Medium tint (50%)", lambda: self._set_tint(128))
                menu.addAction("Strong tint (75%)", lambda: self._set_tint(192))
                menu.addSeparator()
                menu.addAction("Pick tint colour…", self._pick_tint_color)
                menu.exec(ev.globalPos())

            def _set_tint(self, alpha):  #vers 1
                self._tint_alpha = alpha
                self._refresh()

            def _pick_tint_color(self):  #vers 1
                from PyQt6.QtWidgets import QColorDialog
                from PyQt6.QtGui import QColor
                c = QColorDialog.getColor(
                    getattr(self, "_tint_color", QColor(255,255,255)),
                    self, "Pick Tint Colour")
                if c.isValid():
                    self._tint_color = c
                    if not getattr(self, "_tint_alpha", 0):
                        self._tint_alpha = 64
                    self._refresh()

            def mouseMoveEvent(self, ev):  #vers 1
                if getattr(self, '_resize_drag', False) and \
                        ev.buttons() & Qt.MouseButton.LeftButton:
                    gp = ev.globalPosition().toPoint()
                    start_gp, start_sz = self._resize_start
                    new_sz = max(120, min(400, start_sz + gp.x() - start_gp.x()))
                    self._sz[0] = new_sz
                    self.setFixedSize(new_sz, new_sz + 22)
                    self.update()
                    ev.accept(); return
                if self._drag_start and ev.buttons() & Qt.MouseButton.LeftButton:
                    new_pos = ev.globalPosition().toPoint() - self._drag_start
                    pw = self.parent().width()  - self.width()
                    ph = self.parent().height() - self.height()
                    self.move(max(0, min(new_pos.x(), pw)),
                              max(0, min(new_pos.y(), ph)))
                ev.accept()

            def mouseReleaseEvent(self, ev):  #vers 1
                self._drag_start  = None
                self._resize_drag = False
                ev.accept()

        overlay = _ZoomOverlay(self)
        overlay.show()
        overlay.raise_()
        self._zoom_lens = overlay

    # ─────────────────────────────────────────────────────────────────────
    #  Canvas Tool Overlay — shared by Snow, Colour Adjust, Seamless
    # ─────────────────────────────────────────────────────────────────────
    class _ToolOverlay(QWidget): #vers 1
        """
        Lens-style overlay parented to the canvas viewport.
        Shows Original | Result side-by-side with scrollbars,
        height-capped, with action buttons at the bottom.
        """
        def __init__(self, parent_vp, workshop, title,
                     controls_widget, apply_fn, generate_fn=None):  #vers 1
            super().__init__(parent_vp)
            self._ws   = workshop
            self._apply_fn    = apply_fn
            self._generate_fn = generate_fn
            self.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
            self.setAutoFillBackground(True)

            # Size: compact fixed width, capped height, bottom-left corner
            vp_w = parent_vp.width()  or 600
            vp_h = parent_vp.height() or 400
            panel_w = min(520, max(360, vp_w // 2))
            panel_h = min(340, max(240, vp_h // 2))
            self.setFixedWidth(panel_w)
            self.setFixedHeight(panel_h)
            # Anchor to bottom-left of viewport
            self.move(0, vp_h - panel_h)
            self.raise_()
            self.show()

            root = QVBoxLayout(self)
            root.setContentsMargins(4, 4, 4, 4)
            root.setSpacing(3)

            # Title bar
            title_row = QHBoxLayout()
            title_lbl = QLabel(f"  {title}")
            title_lbl.setFont(QFont("Arial", 9, QFont.Weight.Bold))
            close_btn = QPushButton()
            close_btn.setFixedSize(22, 22)
            close_btn.setFlat(True)
            close_btn.setToolTip("Close")
            close_btn.clicked.connect(self._close)
            try:
                _ic = workshop._get_icon_color() if hasattr(workshop, "_get_icon_color") else "#cccccc"
                close_btn.setIcon(SVGIconFactory.close_icon(16, _ic))
                close_btn.setIconSize(QSize(16, 16))
            except Exception:
                close_btn.setText("✕")
            title_row.addWidget(title_lbl, 1)
            title_row.addWidget(close_btn)
            root.addLayout(title_row)

            # Preview area — two scroll areas side by side
            preview_row = QHBoxLayout()
            preview_row.setSpacing(3)
            for side in ('orig', 'result'):
                col = QVBoxLayout()
                hdr = QLabel("Original" if side == 'orig' else "Result")
                hdr.setAlignment(Qt.AlignmentFlag.AlignCenter)
                hdr.setFont(QFont("Arial", 8))
                col.addWidget(hdr)
                sa = QScrollArea()
                sa.setWidgetResizable(True)
                sa.setHorizontalScrollBarPolicy(
                    Qt.ScrollBarPolicy.ScrollBarAsNeeded)
                sa.setVerticalScrollBarPolicy(
                    Qt.ScrollBarPolicy.ScrollBarAsNeeded)
                lbl = QLabel()
                lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
                lbl.setMinimumSize(80, 80)
                sa.setWidget(lbl)
                col.addWidget(sa, 1)
                preview_row.addLayout(col, 1)
                if side == 'orig':
                    self._orig_lbl = lbl
                else:
                    self._result_lbl = lbl
            root.addLayout(preview_row, 1)

            # Controls + buttons
            ctrl_row = QHBoxLayout()
            ctrl_row.setSpacing(6)
            if controls_widget:
                ctrl_row.addWidget(controls_widget, 1)

            btn_col = QVBoxLayout()
            apply_btn = QPushButton("Apply")
            apply_btn.clicked.connect(self._apply)
            btn_col.addWidget(apply_btn)
            if generate_fn:
                gen_btn = QPushButton("Generate")
                gen_btn.clicked.connect(self._generate)
                btn_col.addWidget(gen_btn)
            close_btn2 = QPushButton("Cancel")
            close_btn2.clicked.connect(self._close)
            btn_col.addWidget(close_btn2)
            btn_col.addStretch()
            ctrl_row.addLayout(btn_col)
            root.addLayout(ctrl_row)

        def set_orig_pixmap(self, pm):  #vers 1
            if not pm.isNull():
                sz = self._orig_lbl.size()
                self._orig_lbl.setPixmap(
                    pm.scaled(max(80, sz.width()), max(80, sz.height()),
                              Qt.AspectRatioMode.KeepAspectRatio,
                              Qt.TransformationMode.SmoothTransformation))

        def set_result_pixmap(self, pm):  #vers 1
            if not pm.isNull():
                sz = self._result_lbl.size()
                self._result_lbl.setPixmap(
                    pm.scaled(max(80, sz.width()), max(80, sz.height()),
                              Qt.AspectRatioMode.KeepAspectRatio,
                              Qt.TransformationMode.SmoothTransformation))

        def _generate(self):  #vers 1
            if self._generate_fn:
                self._generate_fn()

        def _apply(self):  #vers 1
            if self._apply_fn:
                self._apply_fn()
            self._close()

        def _close(self):  #vers 1
            self.hide()
            self.deleteLater()

        def resizeEvent(self, e):  #vers 1
            super().resizeEvent(e)


    #    Image tools (seamless, colour correction, snow, sharpen, blur)

    def _open_dp5_seamless(self): #vers 2
        """Seamless tiling — inline canvas overlay."""
        if not self.map_canvas: return
        try:
            from apps.methods.txd_tools import rgba_to_qpixmap, _apply_seamless, _preview_bg
            from PyQt6.QtWidgets import (QWidget, QHBoxLayout, QVBoxLayout,
                QLabel, QSlider, QSpinBox, QComboBox, QScrollArea)
            rgba = bytes(self.map_canvas.rgba)
            w, h = self.map_canvas.tex_w, self.map_canvas.tex_h

            ctrl = QWidget()
            cl = QVBoxLayout(ctrl); cl.setContentsMargins(0,0,0,0); cl.setSpacing(2)

            cl.addWidget(QLabel("Method:"))
            _mode = QComboBox()
            _mode.addItems(["Wrap Blend", "Patch / Heal",
                             "Histogram Blend", "Offset & Mirror"])
            cl.addWidget(_mode)

            blend_row = QHBoxLayout()
            blend_row.addWidget(QLabel("Blend %:"))
            _blend_sl = QSlider(Qt.Orientation.Horizontal)
            _blend_sl.setRange(5, 50); _blend_sl.setValue(25)
            _blend_sp = QSpinBox(); _blend_sp.setRange(5, 50); _blend_sp.setValue(25)
            _blend_sl.valueChanged.connect(_blend_sp.setValue)
            _blend_sp.valueChanged.connect(_blend_sl.setValue)
            blend_row.addWidget(_blend_sl, 1); blend_row.addWidget(_blend_sp)
            cl.addLayout(blend_row)

            cl.addWidget(QLabel("Preview tiling:"))
            _tile_mode = QComboBox()
            _tile_mode.addItems(["1×1", "2×2", "3×3"])
            _tile_mode.setCurrentIndex(2)
            cl.addWidget(_tile_mode)

            _result = [None]
            parent_vp = self._canvas_scroll.viewport() if hasattr(self, '_canvas_scroll') else self

            def _gen():  #vers 1
                result = _apply_seamless(rgba, w, h,
                    mode=_mode.currentIndex(),
                    blend=_blend_sl.value() / 100.0)
                _result[0] = result
                n = _tile_mode.currentIndex() + 1
                from apps.methods.txd_tools import _tile_rgba
                tiled = _tile_rgba(result, w, h, n)
                pm = rgba_to_qpixmap(tiled, w*n, h*n, _preview_bg(overlay))
                overlay.set_result_pixmap(pm)

            def _apply():  #vers 1
                if _result[0]:
                    self._push_undo()
                    self.map_canvas.rgba = bytearray(_result[0])
                    self.map_canvas.update()
                    self._set_status("Seamless applied")

            _tile_mode.currentIndexChanged.connect(lambda _: _gen() if _result[0] else None)

            overlay = self._ToolOverlay(
                parent_vp, self, "Seamless — Canvas",
                ctrl, apply_fn=_apply, generate_fn=_gen)

            n = _tile_mode.currentIndex() + 1
            from apps.methods.txd_tools import _tile_rgba
            tiled_orig = _tile_rgba(rgba, w, h, n)
            pm_orig = rgba_to_qpixmap(tiled_orig, w*n, h*n, _preview_bg(overlay))
            overlay.set_orig_pixmap(pm_orig)

        except Exception as e:
            self._set_status(f"Seamless error: {e}")

    def _open_dp5_colour_adjust(self): #vers 2
        """Colour adjustments — inline canvas overlay."""
        if not self.map_canvas: return
        try:
            from apps.methods.txd_tools import rgba_to_qpixmap, _apply_colour_adjust, _preview_bg
            from PyQt6.QtWidgets import (QWidget, QHBoxLayout, QVBoxLayout,
                QLabel, QSlider, QSpinBox, QCheckBox, QScrollArea)
            rgba = bytes(self.map_canvas.rgba)
            w, h = self.map_canvas.tex_w, self.map_canvas.tex_h

            ctrl = QWidget()
            cl = QVBoxLayout(ctrl); cl.setContentsMargins(0,0,0,0); cl.setSpacing(2)

            sliders = {}
            def _sl(label, lo, hi, val):  #vers 1
                row = QHBoxLayout()
                row.addWidget(QLabel(f"{label}:"))
                sl = QSlider(Qt.Orientation.Horizontal)
                sl.setRange(lo, hi); sl.setValue(val)
                sp = QSpinBox(); sp.setRange(lo, hi); sp.setValue(val)
                sl.valueChanged.connect(sp.setValue)
                sp.valueChanged.connect(sl.setValue)
                row.addWidget(sl, 1); row.addWidget(sp)
                cl.addLayout(row)
                sliders[label] = sl
                return sl

            _sl("Brightness", -100, 100, 0)
            _sl("Contrast",   -100, 100, 0)
            _sl("Hue",        -180, 180, 0)
            _sl("Saturation", -100, 100, 0)
            _sl("Sharpness",     0, 200, 100)
            _sl("Opacity",       0, 100, 100)

            _result = [None]
            parent_vp = self._canvas_scroll.viewport() if hasattr(self, '_canvas_scroll') else self

            def _gen():  #vers 1
                result = _apply_colour_adjust(rgba, w, h,
                    brightness=sliders["Brightness"].value(),
                    contrast=sliders["Contrast"].value(),
                    hue=sliders["Hue"].value(),
                    saturation=sliders["Saturation"].value(),
                    sharpness=sliders["Sharpness"].value() / 100.0,
                    opacity=sliders["Opacity"].value() / 100.0)
                _result[0] = result
                pm = rgba_to_qpixmap(result, w, h, _preview_bg(overlay))
                overlay.set_result_pixmap(pm)

            def _apply():  #vers 1
                if _result[0]:
                    self._push_undo()
                    self.map_canvas.rgba = bytearray(_result[0])
                    self.map_canvas.update()
                    self._set_status("Colour adjustments applied")

            # Wire sliders to live preview
            for sl in sliders.values():
                sl.valueChanged.connect(lambda _: _gen())

            overlay = self._ToolOverlay(
                parent_vp, self, "Colour Adjust — Canvas",
                ctrl, apply_fn=_apply, generate_fn=_gen)

            pm_orig = rgba_to_qpixmap(rgba, w, h, _preview_bg(overlay))
            overlay.set_orig_pixmap(pm_orig)
            _gen()  # initial preview

        except Exception as e:
            self._set_status(f"Colour adjust error: {e}")

    def _open_dp5_snow(self): #vers 2
        """Snow effect generator — inline canvas overlay."""
        if not self.map_canvas: return
        try:
            from apps.methods.txd_tools import rgba_to_qpixmap, _apply_snow, _preview_bg
            from PyQt6.QtWidgets import (QWidget, QHBoxLayout, QVBoxLayout,
                QLabel, QSlider, QSpinBox, QGroupBox, QScrollArea)
            rgba = bytes(self.map_canvas.rgba)
            w, h = self.map_canvas.tex_w, self.map_canvas.tex_h

            # Build compact controls
            ctrl = QWidget()
            cl = QVBoxLayout(ctrl); cl.setContentsMargins(0,0,0,0); cl.setSpacing(2)

            def _sl(label, lo, hi, val):  #vers 1
                row = QHBoxLayout()
                row.addWidget(QLabel(f"{label}:"))
                sl = QSlider(Qt.Orientation.Horizontal)
                sl.setRange(lo, hi); sl.setValue(val)
                sp = QSpinBox(); sp.setRange(lo, hi); sp.setValue(val)
                sl.valueChanged.connect(sp.setValue)
                sp.valueChanged.connect(sl.setValue)
                row.addWidget(sl, 1); row.addWidget(sp)
                cl.addLayout(row)
                return sl

            _threshold = _sl("B/W Threshold", 0, 255, 180)
            _depth     = _sl("Surface Depth", 0, 100, 30)
            _coverage  = _sl("Coverage %",    0, 100, 70)
            _layers    = _sl("Layers",        1, 8,    3)
            _tile      = _sl("Tile",          1, 8,    2)

            _result = [None]
            parent_vp = self._canvas_scroll.viewport() if hasattr(self, '_canvas_scroll') else self

            overlay = self._ToolOverlay(
                parent_vp, self, "Snow — Canvas", ctrl,
                apply_fn=None, generate_fn=None)

            def _gen():  #vers 1
                result = _apply_snow(rgba, w, h,
                    threshold=_threshold.value(),
                    depth=_depth.value() / 100.0,
                    coverage=_coverage.value() / 100.0,
                    layers=_layers.value(),
                    tile=_tile.value())
                _result[0] = result
                pm = rgba_to_qpixmap(result, w, h, _preview_bg(overlay))
                overlay.set_result_pixmap(pm)

            def _apply():  #vers 1
                if _result[0]:
                    self._push_undo()
                    self.map_canvas.rgba = bytearray(_result[0])
                    self.map_canvas.update()
                    self._set_status("Snow applied")

            overlay._generate_fn = _gen
            overlay._apply_fn = _apply

            pm_orig = rgba_to_qpixmap(rgba, w, h, _preview_bg(overlay))
            overlay.set_orig_pixmap(pm_orig)

        except Exception as e:
            self._set_status(f"Snow error: {e}")

    def _open_icon_browser(self): #vers 2
        """Open SVG icon browser as floating panel — click icon to load into canvas."""
        try:
            from apps.components.DP5_Workshop.svg_icon_browser import SVGIconBrowser
            if not hasattr(self, '_svg_browser_panel'):
                self._svg_browser_panel = SVGIconBrowser(workshop=self, parent=None)

            if self._svg_browser_panel.isVisible():
                self._svg_browser_panel.hide()
            else:
                # Position to the left of the DP5 window
                pos = self.mapToGlobal(self.rect().topLeft())
                pw  = self._svg_browser_panel.width()
                self._svg_browser_panel.move(
                    max(0, pos.x() - pw - 6), pos.y() + 40)
                self._svg_browser_panel.resize(220, self.height() - 60)
                self._svg_browser_panel.show()
                self._svg_browser_panel.raise_()
        except Exception as e:
            self._set_status(f"Icon Browser error: {e}")

    def _dp5_sharpen(self, amount: float = 1.5): #vers 1
        """Sharpen the canvas using PIL ImageFilter."""
        if not self.map_canvas: return
        try:
            from PIL import Image, ImageFilter
            rgba = bytes(self.map_canvas.rgba)
            w, h = self.map_canvas.tex_w, self.map_canvas.tex_h
            img = Image.frombytes('RGBA', (w, h), rgba)
            # Unsharp mask: radius, percent, threshold
            sharpened = img.filter(ImageFilter.UnsharpMask(
                radius=1, percent=int(amount * 100), threshold=3))
            self._push_undo()
            self.map_canvas.rgba = bytearray(sharpened.tobytes())
            self.map_canvas.update()
            self._set_status(f"Sharpened ×{amount}")
        except Exception as e:
            self._set_status(f"Sharpen error: {e}")

    def _dp5_blur(self, radius: float = 1.0): #vers 1
        """Gaussian blur the canvas."""
        if not self.map_canvas: return
        try:
            from PIL import Image, ImageFilter
            rgba = bytes(self.map_canvas.rgba)
            w, h = self.map_canvas.tex_w, self.map_canvas.tex_h
            img = Image.frombytes('RGBA', (w, h), rgba)
            blurred = img.filter(ImageFilter.GaussianBlur(radius=radius))
            self._push_undo()
            self.map_canvas.rgba = bytearray(blurred.tobytes())
            self.map_canvas.update()
            self._set_status(f"Blurred r={radius}")
        except Exception as e:
            self._set_status(f"Blur error: {e}")

    def _dp5_emboss(self): #vers 1
        """Emboss filter."""
        if not self.map_canvas: return
        try:
            from PIL import Image, ImageFilter
            rgba = bytes(self.map_canvas.rgba)
            w, h = self.map_canvas.tex_w, self.map_canvas.tex_h
            img = Image.frombytes('RGBA', (w, h), rgba)
            rgb = img.convert('RGB').filter(ImageFilter.EMBOSS).convert('RGBA')
            # Preserve original alpha
            r, g, b, _ = rgb.split()
            _, _, _, a  = img.split()
            result = Image.merge('RGBA', (r, g, b, a))
            self._push_undo()
            self.map_canvas.rgba = bytearray(result.tobytes())
            self.map_canvas.update()
            self._set_status("Emboss applied")
        except Exception as e:
            self._set_status(f"Emboss error: {e}")

    def _dp5_edge_detect(self): #vers 1
        """Edge detection filter."""
        if not self.map_canvas: return
        try:
            from PIL import Image, ImageFilter
            rgba = bytes(self.map_canvas.rgba)
            w, h = self.map_canvas.tex_w, self.map_canvas.tex_h
            img = Image.frombytes('RGBA', (w, h), rgba)
            rgb = img.convert('RGB').filter(ImageFilter.FIND_EDGES).convert('RGBA')
            r, g, b, _ = rgb.split()
            _, _, _, a  = img.split()
            result = Image.merge('RGBA', (r, g, b, a))
            self._push_undo()
            self.map_canvas.rgba = bytearray(result.tobytes())
            self.map_canvas.update()
            self._set_status("Edge detect applied")
        except Exception as e:
            self._set_status(f"Edge detect error: {e}")

    #    Settings / theme

    def _show_workshop_settings(self): #vers 1
        """Open DP5-specific settings dialog (NOT the global theme dialog)."""
        old_icon_sz = self.map_settings.get('tool_icon_size')

        dlg = MapSettingsDialog(self.map_settings, self)
        if dlg.exec():
            # Apply menu bar style changes live
            self._apply_menu_bar_style()
            # Also apply orientation change if it changed
            self.set_menu_orientation(self.map_settings.get('menu_style', 'topbar'))

            # Apply changed settings live
            show_left = (self.map_settings.get('show_bitmap_list') and
                        self.map_settings.get('widget_bitmaps_enabled'))
            if hasattr(self, '_bitmaps_dock'):
                self._bitmaps_dock.setVisible(show_left)
            for entry in self._WIDGET_REGISTRY:
                if not entry['enabled_setting']:
                    continue   # Bitmaps - handled above, combines two settings
                dock = getattr(self, entry['dock_attr'], None)
                if dock is not None:
                    dock.setVisible(self.map_settings.get(entry['enabled_setting']))
            if hasattr(self, '_status_bar'):
                self._status_bar.setVisible(self.map_settings.get('show_statusbar'))

            if self.map_canvas:
                self.map_canvas.show_grid = self.map_settings.get('show_pixel_grid')
                self.map_canvas.grid_color = QColor(self.map_settings.get('grid_color'))
                self.map_canvas.show_cell_grid = self.map_settings.get('show_cell_grid')
                self.map_canvas.marching_ants_enabled = self.map_settings.get('marching_ants_enabled')
                self.map_canvas.marching_ants_fg = QColor(self.map_settings.get('marching_ants_fg'))
                self.map_canvas.marching_ants_bg = QColor(self.map_settings.get('marching_ants_bg'))
                self.map_canvas.marching_ants_style = self.map_settings.get('marching_ants_style')
                self.map_canvas.marching_ants_speed = self.map_settings.get('marching_ants_speed')
                self.map_canvas._sync_marching_ants_timer()
                self.map_canvas.update()
                if self.map_settings.get('zoom_to_fit_resize'):
                    self._fit_canvas_to_viewport()
            for _tb in (getattr(self, '_tools_ribbon', None),
                        getattr(self, '_image_ops_ribbon', None)):
                if _tb is not None:
                    self._apply_ribbon_style(_tb)
            self._set_platform(self.map_settings.get('platform_mode'))

            show_mb = (self.map_settings.get('show_menubar') and
                       self.map_settings.get('menu_style') == 'topbar')
            c = getattr(self, '_menu_bar_container', self._menu_bar if hasattr(self, '_menu_bar') else None)
            if c:
                c.setMinimumHeight(0)
                c.setMaximumHeight(16777215 if show_mb else 0)
                c.setVisible(show_mb)

            # If icon size changed, update the Tools ribbon. Column count
            # no longer applies - Tools is a QToolBar now, which handles
            # its own layout/wrapping natively.
            new_icon_sz = self.map_settings.get('tool_icon_size')
            if new_icon_sz != old_icon_sz:
                self._rebuild_right_panel()

            self._set_status("Settings saved.")


    def _rebuild_right_panel(self): #vers 4
        """Update the Tools ribbon's icon size in place - was a full
        teardown/rebuild of the adaptive-column gadget grid, but Tools is
        now a QToolBar (no column concept - QToolBar handles its own
        layout/wrapping natively), so just updating setIconSize() and
        refreshing the action icons is all that's needed. Uses whichever
        of the vertical/horizontal ribbon icon size settings matches the
        ribbon's current orientation, rather than the old single
        tool_icon_size setting."""
        tb = getattr(self, '_tools_ribbon', None)
        if tb is None:
            return
        self._apply_ribbon_style(tb)
        vertical = (tb.orientation() == Qt.Orientation.Vertical)
        self._tool_icon_sz = self.map_settings.get(
            'ribbon_icon_size_vert' if vertical else 'ribbon_icon_size_horz')
        self._refresh_icons()   # redraws all action icons at the new size
        # Re-select current tool so icons reflect active state
        if self.map_canvas:
            self._select_tool(self.map_canvas.tool, from_button_click=False)
        self._update_color_swatches()
        self._sync_brush_thumb()   # restore thumbnail after rebuild

    # Entire export cluster removed (_on_splitter_moved through
    # _export_icns, ~26 export methods plus their shared helpers -
    # _load_rgba/_rgb_to_9bit/_9bit_to_rgb/
    # _canvas_to_256colour_indexed/_get_canvas_pil/
    # _write_amiga_info/_write_icns) - confirmed dead: every
    # caller was from within this same cluster, none reachable
    # from any live entry point, none touching Map Workshop
    # functionality.

    def _apply_theme(self): #vers 5
        """Apply global app theme — uses QApplication stylesheet set by app_settings."""
        try:
            mw = getattr(self, 'main_window', None)
            app_settings = None
            if hasattr(self, 'app_settings') and self.app_settings:
                app_settings = self.app_settings
            elif mw and hasattr(mw, 'app_settings'):
                app_settings = mw.app_settings

            if app_settings and hasattr(app_settings, 'get_stylesheet'):
                # Apply to QApplication so all widgets inherit it
                from PyQt6.QtWidgets import QApplication
                ss = app_settings.get_stylesheet()
                if ss:
                    QApplication.instance().setStyleSheet(ss)
                    # Apply panel effects (fill/gradient/pattern) if configured
            try:
                from apps.utils.app_settings_system import apply_panel_effects
                apply_panel_effects(self, app_settings)
            except Exception:
                pass
            # Clear any widget-level override so we inherit from QApplication
            self.setStyleSheet("")

            # Re-apply ribbon backgrounds and the outer window/separator
            # styling so they pick up the new theme's colours - both are
            # set once at creation time and otherwise never refresh on a
            # later theme switch.
            for _tb in (getattr(self, '_tools_ribbon', None),
                        getattr(self, '_image_ops_ribbon', None)):
                if _tb is not None:
                    self._apply_ribbon_style(_tb)
            if getattr(self, '_outer_mw', None) is not None and app_settings:
                _tc = app_settings.get_theme_colors()
                _hexval = _tc.get('panel_bg')
                _accentval = _tc.get('accent_primary')
                _sep_bg_rule = f"background: {_accentval}; " if _accentval else ""
                if _hexval:
                    self._outer_mw.setStyleSheet(
                        f"QMainWindow {{ background: {_hexval}; }} "
                        f"QMainWindow::separator {{ {_sep_bg_rule}"
                        "width: 1px; height: 1px; } "
                        "QMainWindow::separator:hover { background: palette(highlight); }")

            # Re-apply dock title bar backgrounds too - same "set once,
            # never refreshed" gap as the ribbon/outer_mw above.
            if app_settings:
                _tc2 = app_settings.get_theme_colors()
                _hexval2 = _tc2.get('panel_bg') or _tc2.get('bg_primary')
                if _hexval2:
                    for _bar in self.findChildren(QWidget, "dp5_dock_titlebar"):
                        _bar.setStyleSheet(
                            f"QWidget#dp5_dock_titlebar {{ background: {_hexval2}; }}")

            # Registry-driven theme refresh - same list setup_ui() uses to
            # build the docks, so a widget swapped/added there is
            # automatically covered here too.
            for entry in self._WIDGET_REGISTRY:
                try:
                    module = __import__(entry['module'], fromlist=['refresh_theme'])
                    module.refresh_theme(self)
                except Exception:
                    pass

            # gadgetbar_bg applied via QFrame#titlebar in global stylesheet — no manual refresh needed
            # Refresh icons so they contrast correctly with new theme
            self._refresh_icons()
        except Exception as e:
            print(f"Theme application error: {e}")


    def _refresh_icons(self): #vers 2
        """Refresh ALL icons with current theme colours."""
        SVGIconFactory.clear_cache()
        icon_col = self._get_icon_color()

        # Get tile_bg from theme (panel_bg or bg_secondary)
        tile_bg = ''
        try:
            if self.app_settings:
                tc = self.app_settings.get_theme_colors() or {}
                tile_bg = tc.get('gadgetbar_bg',
                            tc.get('toolbar_bg',
                                tc.get('bg_secondary', '')))
        except Exception:
            pass

        # Gadget bar buttons (settings, properties, window chrome)
        for attr, method in [
            ('settings_btn',    'settings_icon'),
            ('properties_btn',  'properties_icon'),
        ]:
            if hasattr(self, attr):
                getattr(self, attr).setIcon(
                    getattr(SVGIconFactory, method)(20, icon_col))
        for attr, method in [('minimize_btn','minimize_icon'),
                              ('maximize_btn','maximize_icon'),
                              ('close_btn','close_icon')]:
            if hasattr(self, attr):
                getattr(self, attr).setIcon(
                    getattr(SVGIconFactory, method)(20, icon_col))

        # Tool grid actions — redraw with theme colours
        icon_sz = self.map_settings.get('tool_icon_size', 22)
        for tool_id, btn in getattr(self, '_tool_btns', {}).items():
            from apps.components.Map_Editor.map_workshop import _load_tool_icon
            active = btn.isChecked()
            ico = _load_tool_icon(tool_id, icon_sz, active=active,
                                  tile_bg=tile_bg, icon_col=icon_col)
            btn.setIcon(ico)

        # Brush manager button if present
        if hasattr(self, 'brush_mgr_btn'):
            try:
                from apps.components.Map_Editor.map_workshop import get_brushes_icon
                self.brush_mgr_btn.setIcon(get_brushes_icon(18, icon_col))
            except Exception:
                pass

        # Recent colour buttons — theme-aware via the shared helper
        from apps.components.DP5_Workshop.depends.brushcolors_widget import (
            _style_empty_history_slot)
        for i, btn in enumerate(getattr(self, '_color_hist_btns', [])):
            if i < len(getattr(self, '_color_history', [])):
                h = self._color_history[i]
                border = 'palette(mid)'
                if self.app_settings and hasattr(self.app_settings, 'get_theme_colors'):
                    tc = self.app_settings.get_theme_colors()
                    border = tc.get('border') or border
                btn.setStyleSheet(f"background:{h}; border:1px solid {border};")
            else:
                _style_empty_history_slot(self, btn)


    def _launch_theme_settings(self): #vers 1
        try:
            if not APPSETTINGS_AVAILABLE: return
            dialog = SettingsDialog(self.app_settings, self)
            dialog.themeChanged.connect(lambda _: self._apply_theme())
            if dialog.exec():
                self._apply_theme()
        except Exception as e:
            QMessageBox.warning(self, "Theme Error", str(e))


    def _get_icon_color(self) -> str: #vers 4
        """Return icon colour per tool_icon_color setting and theme."""
        mode = self.map_settings.get('tool_icon_color', 'color')
        if self.app_settings:
            colors = self.app_settings.get_theme_colors()
            if mode == 'white':
                return colors.get('text_background', colors.get('text_primary', '#eeeeee'))
            if mode == 'dark':
                return colors.get('text_primary', colors.get('panel_primary', '#222222'))
            # 'color' — auto-detect from gadgetbar_bg
            bar_bg = colors.get('gadgetbar_bg', colors.get('toolbar_bg', ''))
            try:
                r = int(bar_bg[1:3], 16)
                g = int(bar_bg[3:5], 16)
                b = int(bar_bg[5:7], 16)
                if (r*299 + g*587 + b*114) // 1000 > 128:
                    return colors.get('text_primary', colors.get('panel_primary', '#222222'))
                else:
                    return colors.get('text_background', colors.get('text_primary', '#eeeeee'))
            except Exception:
                return colors.get('text_primary', '#eeeeee')
        return '#eeeeee'


    #    Window management

    def _set_status(self, msg: str): #vers 1
        if hasattr(self, '_status_bar'):
            self._status_bar.showMessage(msg)
        # Always refresh permanent info labels
        if hasattr(self, '_status_size_lbl'):
            w = getattr(self, '_canvas_width', 0)
            h = getattr(self, '_canvas_height', 0)
            self._status_size_lbl.setText(f"{w}×{h}")
        if hasattr(self, '_status_depth_lbl'):
            depth = getattr(self, '_canvas_bit_depth', 0)
            labels = {0:'RGBA32', 1:'RGB24', 2:'RGB16', 3:'Idx8'}
            self._status_depth_lbl.setText(labels.get(depth, 'RGBA32'))


    def _toggle_maximize(self): #vers 1
        if self.isMaximized(): self.showNormal()
        else: self.showMaximized()


    def keyPressEvent(self, e): #vers 1
        k   = e.key()
        mod = e.modifiers()
        Ctrl  = Qt.KeyboardModifier.ControlModifier
        Shift = Qt.KeyboardModifier.ShiftModifier
        NoMod = Qt.KeyboardModifier.NoModifier

        # Ctrl combos
        if mod == Ctrl:
            if k == Qt.Key.Key_Z: self._undo_canvas()
            elif k == Qt.Key.Key_Y: self._redo_canvas()
            elif k == Qt.Key.Key_X: self._cut_selection()
            elif k == Qt.Key.Key_C: self._copy_selection()
            elif k == Qt.Key.Key_V: self._paste_selection()
            elif k == Qt.Key.Key_A: self._select_all()
            elif k in (Qt.Key.Key_Plus, Qt.Key.Key_Equal):
                z = self._canvas_zoom
                self._set_zoom(z * 1.25 if z < 1 else min(64, z + 1))
            elif k == Qt.Key.Key_Minus:
                z = self._canvas_zoom
                self._set_zoom(max(0.05, z * 0.8 if z <= 1 else z - 1))
            else: super().keyPressEvent(e)
            return

        # Escape — exit stamp mode, cancel floating move, or deselect
        if k == Qt.Key.Key_Escape:
            if self.map_canvas and self.map_canvas.tool == TOOL_STAMP:
                # Exit stamp mode → back to select tool
                self._select_tool(TOOL_SELECT)
                self._set_status("Stamp mode off")
            elif self.map_canvas and self.map_canvas._sel_floating:
                self.map_canvas.cancel_sel_move()
                self.map_canvas._sel_active = True
            else:
                self._deselect()
            if self.map_canvas:
                self.map_canvas._curve_phase = None
                self.map_canvas._curve_p0 = None
                self.map_canvas._curve_p1 = None
                self.map_canvas._curve_control = None
                self.map_canvas._curve_dragging_warp = False
                self.map_canvas._poly_pts  = []
                self.map_canvas.update()
            return

        # Arrow keys — nudge float OR scroll viewport
        arrow_map = {
            Qt.Key.Key_Left:  (-1,  0),
            Qt.Key.Key_Right: ( 1,  0),
            Qt.Key.Key_Up:    ( 0, -1),
            Qt.Key.Key_Down:  ( 0,  1),
        }
        if k in arrow_map:
            dx, dy = arrow_map[k]
            step = 10 if mod == Shift else 1
            c = self.map_canvas
            if c and c._sel_floating:
                # Nudge the floating object
                c.nudge_float(dx * step, dy * step)
            else:
                # Scroll the viewport
                sa = getattr(self, '_canvas_scroll', None)
                if sa:
                    scroll_step = step * max(1, int(self._canvas_zoom))
                    sa.horizontalScrollBar().setValue(
                        sa.horizontalScrollBar().value() + dx * scroll_step)
                    sa.verticalScrollBar().setValue(
                        sa.verticalScrollBar().value() + dy * scroll_step)
            return

        # Enter/Return — stamp floating object and deselect
        if k in (Qt.Key.Key_Return, Qt.Key.Key_Enter):
            if self.map_canvas and self.map_canvas._sel_floating:
                self.map_canvas._stamp_selection(keep_floating=False)
                self.map_canvas._sel_active  = False
                self.map_canvas._sel_buffer  = None
                self.map_canvas._sel_buf_w   = 0
                self.map_canvas._sel_buf_h   = 0
                self.map_canvas._sel_float_pos = None
                self.map_canvas.update()
                self._set_status("Object stamped")
            elif self.map_canvas and self.map_canvas.tool == TOOL_CURVE and \
                    self.map_canvas._curve_phase == 'warp':
                c = self.map_canvas
                c._push_undo_canvas()
                c.draw_quadratic_curve(c._curve_p0, c._curve_control,
                                        c._curve_p1, c.color)
                c._curve_phase = None
                c._curve_p0 = c._curve_p1 = c._curve_control = None
                c._curve_dragging_warp = False
                c.update()
                self._set_status("Curve committed")
            return
        tool_keys = {
            Qt.Key.Key_P: TOOL_PENCIL,
            Qt.Key.Key_E: TOOL_ERASER,
            Qt.Key.Key_F: TOOL_FILL,
            Qt.Key.Key_S: TOOL_SPRAY,
        Qt.Key.Key_B: TOOL_BLUR_BRUSH,
        Qt.Key.Key_U: TOOL_SMUDGE,
            Qt.Key.Key_K: TOOL_PICKER,
            Qt.Key.Key_Q: TOOL_CURVE,
            Qt.Key.Key_L: TOOL_LINE,
            Qt.Key.Key_R: TOOL_RECT,
            Qt.Key.Key_C: TOOL_CIRCLE,
            Qt.Key.Key_O: TOOL_POLYGON,
            Qt.Key.Key_T: TOOL_TRIANGLE,
            Qt.Key.Key_Asterisk: TOOL_STAR,
            Qt.Key.Key_M: TOOL_SELECT,
            Qt.Key.Key_G: TOOL_LASSO,
            Qt.Key.Key_H: TOOL_MOVE,
            Qt.Key.Key_Z: TOOL_ZOOM,
            Qt.Key.Key_I: TOOL_TEXT,
        }
        if k in tool_keys:
            self._select_tool(tool_keys[k])
        elif k == Qt.Key.Key_X and mod == NoMod:
            if hasattr(self, '_fgbg_swatch'):
                self._fgbg_swatch.swap()
        elif k == Qt.Key.Key_Delete:
            self._cut_selection()
        # Animation shortcuts (only when anim strip visible)
        elif k == Qt.Key.Key_Comma and hasattr(self,'_anim_strip') and self._anim_strip.isVisible():
            self._anim_prev()
        elif k == Qt.Key.Key_Period and hasattr(self,'_anim_strip') and self._anim_strip.isVisible():
            self._anim_next()
        elif k == Qt.Key.Key_Space and hasattr(self,'_anim_strip') and self._anim_strip.isVisible():
            self._anim_toggle_play()
        else:
            super().keyPressEvent(e)


    def _save_outer_layout(self): #vers 1
        """Save outer_mw's current dock/ribbon layout - factored out of
        closeEvent so it can also be triggered reliably when the
        workshop is embedded as a tab (the common case via
        open_map_workshop), where closeEvent likely never fires at all
        - it's a QWidget-level event that only triggers when .close()
        is actually called on this widget specifically, not when the
        whole app quits or a tab is closed/switched away from while
        this widget stays alive as a child of main_tab_widget."""
        if not (hasattr(self, '_outer_mw') and self._outer_mw):
            return
        try:
            from PyQt6.QtCore import QByteArray
            self.map_settings.set(
                'outer_layout_state',
                self._outer_mw.saveState(self._OUTER_LAYOUT_VERSION).toHex().data().decode())
            self.map_settings.set('outer_layout_version', self._OUTER_LAYOUT_VERSION)
            self.map_settings.save()
        except Exception:
            pass

    def closeEvent(self, event): #vers 4
        # Save dock/ribbon layout so it restores on next open
        self._save_outer_layout()
        # Remove injected tool menu from imgfactory menubar
        try:
            mw = getattr(self, 'main_window', None) or getattr(self, '_imgfactory', None)
            if mw and hasattr(mw, '_update_tool_menu_for_tab'):
                mw._update_tool_menu_for_tab(None)
        except Exception:
            pass
        self.window_closed.emit()
        event.accept()

    #    Drag and drop loading

    # dragEnterEvent/dragMoveEvent/dropEvent/_is_importable_ext
    # removed - only existed to support the drag-and-drop image
    # import system, which Keith's removal list eliminated
    # entirely (_import_dropped_file and the whole _import_*
    # format family it dispatched to).

    def _update_transform_text_panel_visibility(self): #vers 2
        """Toggle between text+icon panel (wide) and icon-only strip (narrow).
        Reads threshold from IMG Factory settings. Also collapses bottom buttons."""
        tp   = getattr(self, '_transform_text_panel_ref', None)
        ip   = getattr(self, '_transform_icon_panel_ref', None)
        mode = getattr(self, 'button_display_mode', 'both')

        if mode == 'icons':
            if tp: tp.setVisible(False)
            if ip: ip.setVisible(True)
            return
        if mode == 'text':
            if tp: tp.setVisible(True)
            if ip: ip.setVisible(False)
            return

        # Measure right panel width directly
        rp = getattr(self, '_right_panel_ref', None)
        if rp:
            ref_w = rp.width()
        else:
            splitter = getattr(self, '_main_splitter', None)
            ref_w = self.width()
            if splitter and tp:
                w = tp
                while w and w.parent() is not splitter:
                    w = w.parent() if hasattr(w, 'parent') else None
                if w:
                    ref_w = w.width()

        try:
            from apps.methods.imgfactory_ui_settings import get_collapse_threshold
            threshold = get_collapse_threshold(getattr(self, 'main_window', None))
        except Exception:
            threshold = 550
        wide = ref_w >= threshold
        if tp: tp.setVisible(wide)
        if ip: ip.setVisible(not wide)

        # Toggle bottom panel rows the same way
        btr = getattr(self, '_bottom_text_row', None)
        bir = getattr(self, '_bottom_icon_row', None)
        if btr: btr.setVisible(wide)
        if bir: bir.setVisible(not wide)


    def _get_resize_corner(self, pos): #vers 1
        size = self.corner_size; w = self.width(); h = self.height()
        if pos.x() < size and pos.y() < size:           return "top-left"
        if pos.x() > w - size and pos.y() < size:       return "top-right"
        if pos.x() < size and pos.y() > h - size:       return "bottom-left"
        if pos.x() > w - size and pos.y() > h - size:   return "bottom-right"
        return None


    def _update_cursor(self, direction): #vers 1
        cursors = {
            "top-left":     Qt.CursorShape.SizeFDiagCursor,
            "bottom-right": Qt.CursorShape.SizeFDiagCursor,
            "top-right":    Qt.CursorShape.SizeBDiagCursor,
            "bottom-left":  Qt.CursorShape.SizeBDiagCursor,
        }
        self.setCursor(cursors.get(direction, Qt.CursorShape.ArrowCursor))


    def _get_resize_direction(self, pos): #vers 1
        """Determine resize direction based on mouse position"""
        rect = self.rect()
        margin = self.resize_margin

        left = pos.x() < margin
        right = pos.x() > rect.width() - margin
        top = pos.y() < margin
        bottom = pos.y() > rect.height() - margin

        if left and top:
            return "top-left"
        elif right and top:
            return "top-right"
        elif left and bottom:
            return "bottom-left"
        elif right and bottom:
            return "bottom-right"
        elif left:
            return "left"
        elif right:
            return "right"
        elif top:
            return "top"
        elif bottom:
            return "bottom"

        return None


    def _is_on_draggable_area(self, pos): #vers 1
        if not hasattr(self, 'titlebar'):
            return False
        if not self.titlebar.rect().contains(pos):
            return False
        for w in self.titlebar.findChildren(QPushButton):
            if w.isVisible() and w.geometry().contains(pos):
                return False
        return True


    def mousePressEvent(self, event): #vers 1
        if event.button() != Qt.MouseButton.LeftButton:
            return super().mousePressEvent(event)
        pos = event.pos()
        self.resize_corner = self._get_resize_corner(pos)
        if self.resize_corner:
            self.resizing = True
            self.drag_position    = event.globalPosition().toPoint()
            self.initial_geometry = self.geometry()
            event.accept(); return
        if hasattr(self, 'titlebar') and self.titlebar.geometry().contains(pos):
            tb_pos = self.titlebar.mapFromParent(pos)
            if self._is_on_draggable_area(tb_pos):
                handle = self.windowHandle()
                if handle: handle.startSystemMove()
                event.accept(); return
        super().mousePressEvent(event)


    def mouseMoveEvent(self, event): #vers 1
        if event.buttons() == Qt.MouseButton.LeftButton:
            if self.resizing and self.resize_corner:
                self._handle_corner_resize(event.globalPosition().toPoint())
                event.accept(); return
        else:
            corner = self._get_resize_corner(event.pos())
            if corner != self.hover_corner:
                self.hover_corner = corner
                self.update()
            self._refresh_corner_overlay()
            self._update_cursor(corner)
        super().mouseMoveEvent(event)


    def mouseReleaseEvent(self, event): #vers 1
        if event.button() == Qt.MouseButton.LeftButton:
            self.dragging = self.resizing = False
            self.resize_corner = None
            self.setCursor(Qt.CursorShape.ArrowCursor)
            event.accept()


    def _handle_corner_resize(self, global_pos): #vers 1
        if not self.resize_corner or not self.drag_position: return
        delta    = global_pos - self.drag_position
        geometry = self.initial_geometry
        min_w, min_h = 900, 560
        if self.resize_corner == "bottom-right":
            nw = geometry.width() + delta.x()
            nh = geometry.height() + delta.y()
            if nw >= min_w and nh >= min_h: self.resize(nw, nh)
        elif self.resize_corner == "bottom-left":
            nx = geometry.x() + delta.x()
            nw = geometry.width() - delta.x()
            nh = geometry.height() + delta.y()
            if nw >= min_w and nh >= min_h:
                self.setGeometry(nx, geometry.y(), nw, nh)
        elif self.resize_corner == "top-right":
            ny = geometry.y() + delta.y()
            nw = geometry.width() + delta.x()
            nh = geometry.height() - delta.y()
            if nw >= min_w and nh >= min_h:
                self.setGeometry(geometry.x(), ny, nw, nh)
        elif self.resize_corner == "top-left":
            nx = geometry.x() + delta.x()
            ny = geometry.y() + delta.y()
            nw = geometry.width() - delta.x()
            nh = geometry.height() - delta.y()
            if nw >= min_w and nh >= min_h:
                self.setGeometry(nx, ny, nw, nh)


    def _handle_resize(self, global_pos): #vers 1
        """Handle window resizing"""
        if not self.resize_direction or not self.drag_position:
            return

        delta = global_pos - self.drag_position
        geometry = self.frameGeometry()

        min_width = 800
        min_height = 600

        # Handle horizontal resizing
        if "left" in self.resize_direction:
            new_width = geometry.width() - delta.x()
            if new_width >= min_width:
                geometry.setLeft(geometry.left() + delta.x())
        elif "right" in self.resize_direction:
            new_width = geometry.width() + delta.x()
            if new_width >= min_width:
                geometry.setRight(geometry.right() + delta.x())

        # Handle vertical resizing
        if "top" in self.resize_direction:
            new_height = geometry.height() - delta.y()
            if new_height >= min_height:
                geometry.setTop(geometry.top() + delta.y())
        elif "bottom" in self.resize_direction:
            new_height = geometry.height() + delta.y()
            if new_height >= min_height:
                geometry.setBottom(geometry.bottom() + delta.y())

        self.setGeometry(geometry)
        self.drag_position = global_pos


    def paintEvent(self, event): #vers 2
        super().paintEvent(event)
        # Corner handles drawn by _corner_overlay overlay widget

    def _setup_corner_overlay(self): #vers 3
        """Create or re-raise the corner resize overlay.
        Only active in standalone (frameless) mode.
        Called from showEvent and resizeEvent with a delay so all child
        widgets are laid out before we raise_() above them.
        """
        if not self.standalone_mode:
            return
        if not (self.windowFlags() & Qt.WindowType.FramelessWindowHint):
            return
        if hasattr(self, '_corner_overlay') and self._corner_overlay:
            self._corner_overlay.setGeometry(0, 0, self.width(), self.height())
            self._corner_overlay.raise_()
            self._corner_overlay.update_state(
                getattr(self, 'hover_corner', None), self.app_settings)
            return
        overlay = _CornerOverlay(self)
        overlay.update_state(getattr(self, 'hover_corner', None), self.app_settings)
        self._corner_overlay = overlay
        overlay.setGeometry(0, 0, self.width(), self.height())
        overlay.show()
        overlay.raise_()

    def showEvent(self, event): #vers 3
        super().showEvent(event)
        from PyQt6.QtCore import QTimer
        # Two-shot: 100ms for layout settle, 400ms to ensure all children rendered
        QTimer.singleShot(100, self._setup_corner_overlay)
        QTimer.singleShot(400, self._setup_corner_overlay)
        # Rebuild right panel so tool icons lay out correctly on first show
        QTimer.singleShot(150, self._rebuild_right_panel)

    def resizeEvent(self, event): #vers 1
        super().resizeEvent(event)
        from PyQt6.QtCore import QTimer
        QTimer.singleShot(50, self._setup_corner_overlay)

    def _refresh_corner_overlay(self): #vers 2
        if hasattr(self, '_corner_overlay') and self._corner_overlay:
            self._corner_overlay.setGeometry(0, 0, self.width(), self.height())
            self._corner_overlay.update_state(
                getattr(self, 'hover_corner', None), self.app_settings)
            self._corner_overlay.raise_()


#  Public factory function

def open_map_workshop(main_window=None, game_root: str = None,
                      dat_path: str = None) -> MapWorkshop: #vers 3
    """Open Map Workshop - embedded in a tab if main_window has one,
    standalone window otherwise. game_root: if given (e.g. passed in
    from Dat Browser, which already has a game root loaded), auto-loads
    that game's world data immediately - same underlying load either
    way, matching how Model/TXD/COL Workshop can be opened either via
    an explicit path or by picking up whatever's already open. dat_path:
    if given instead, loads from that specific .dat file directly
    (e.g. right-clicking a .dat entry in the DAT Browser tree)."""
    try:
        if main_window and hasattr(main_window, 'main_tab_widget'):
            import os as _os
            from PyQt6.QtWidgets import QWidget, QVBoxLayout
            container = QWidget()
            layout = QVBoxLayout(container)
            layout.setContentsMargins(0, 0, 0, 0)
            workshop = MapWorkshop(container, main_window)
            workshop.setWindowFlags(Qt.WindowType.Widget)
            layout.addWidget(workshop)
            if game_root:
                tab_label = _os.path.basename(game_root.rstrip('/\\'))
            elif dat_path:
                tab_label = _os.path.basename(dat_path)
            else:
                tab_label = "Map Workshop"
            try:
                from apps.methods.imgfactory_svg_icons import get_map_workshop_icon
                icon = get_map_workshop_icon(24)
                idx = main_window.main_tab_widget.addTab(container, icon, tab_label)
            except Exception:
                idx = main_window.main_tab_widget.addTab(container, tab_label)
            main_window.main_tab_widget.setCurrentIndex(idx)
            if hasattr(main_window, '_ensure_tab_area_visible'):
                main_window._ensure_tab_area_visible()
            workshop.show()
        else:
            workshop = MapWorkshop(None, main_window)
            workshop.setWindowFlags(Qt.WindowType.Window)
            workshop.setWindowTitle(App_name)
            workshop.resize(1400, 800)
            try:
                from apps.methods.imgfactory_svg_icons import get_map_workshop_icon
                workshop.setWindowIcon(get_map_workshop_icon(64))
            except Exception:
                pass
            workshop.show()

        if game_root:
            workshop._load_game_folder(preset_root=game_root)
        elif dat_path:
            workshop._load_game_dat_file(preset_dat_path=dat_path)

        return workshop
    except Exception as e:
        import traceback; traceback.print_exc()
        if main_window and hasattr(main_window, 'log_message'):
            main_window.log_message(f"Map Workshop error: {e}")
        elif main_window:
            QMessageBox.critical(main_window, App_name + " Error", str(e))
        return None


#  Standalone entry point
if __name__ == "__main__": #vers 1
    import traceback

    # Filter known-benign Qt warnings from AppPanelEffect's monkey-patched
    # paint pattern (a second, independent QPainter opened on a widget from
    # inside its own wrapped paintEvent - can transiently fail Qt's paint-
    # device validity check, especially under Wayland). Not a fix for that
    # underlying fragile architecture, just silencing console spam for
    # diagnostics that are non-fatal (Qt already skips the paint attempt
    # when this happens).
    def _qt_message_filter(mode, context, message): #vers 1
        _benign = ("QWidget::paintEngine: Should no longer be called",
                   "QPainter::begin: Paint device returned engine == 0")
        if any(b in message for b in _benign):
            return
        print(message)
    try:
        from PyQt6.QtCore import qInstallMessageHandler
        qInstallMessageHandler(_qt_message_filter)
    except Exception:
        pass

    print(f"{App_name} starting…")
    try:
        app = QApplication(sys.argv)
        app.setApplicationName(App_name)
        app.setApplicationVersion("1.6")
        app.setOrganizationName("X-Seti")

        # Set app icon — appears in taskbar, alt-tab, dock
        try:
            from apps.methods.imgfactory_svg_icons import get_map_workshop_icon
            app_icon = QIcon()
            for sz in (16, 32, 48, 64, 128):
                ico = get_map_workshop_icon(sz)
                app_icon.addPixmap(ico.pixmap(sz, sz))
            app.setWindowIcon(app_icon)
        except Exception:
            pass

        w = MapWorkshop()
        w.setWindowTitle(App_name + " – Standalone")
        w.resize(1400, 800)
        w.show()
        sys.exit(app.exec())
    except Exception as e:
        print(f"ERROR: {e}")
        traceback.print_exc()
        sys.exit(1)


__all__ = ['MapWorkshop', 'open_map_workshop']
