# X-Seti - Jul07 2026 - IMG Factory 1.6 - DFF OpenGL Viewport
# this belongs in apps/methods/dff_viewport.py - Version: 14
"""
DFFViewport - Shared OpenGL viewport for DFF model rendering.
Used by Model Viewer, Model Workshop, Vehicle Workshop (docked).
Standalone tools import from their own methods/dff_viewport.py.

##Methods list -
# DFFViewport.__init__
# DFFViewport._anim_tick
# DFFViewport._apply_selection_click
# DFFViewport._auto_fit
# DFFViewport._calc_world_matrix
# DFFViewport._closest_point_on_ray
# DFFViewport._draw_assembly
# DFFViewport._draw_axes
# DFFViewport._draw_grid
# DFFViewport._draw_selection_overlay
# DFFViewport._draw_solid
# DFFViewport._draw_textured
# DFFViewport._draw_wireframe
# DFFViewport._emit_verts
# DFFViewport._face_color
# DFFViewport._flush_pending_textures
# DFFViewport._geom_flags
# DFFViewport._get_anim_rotation
# DFFViewport._get_bg_color
# DFFViewport._get_selection_count
# DFFViewport._get_ui_color
# DFFViewport._get_wheel_geom_data
# DFFViewport._notify_selection_changed
# DFFViewport._pick_edge
# DFFViewport._pick_face
# DFFViewport._pick_ray
# DFFViewport._pick_vertex
# DFFViewport._point_seg_dist2
# DFFViewport._ray_triangle_intersect
# DFFViewport._rebuild_anim_geoms
# DFFViewport._refresh
# DFFViewport._rw_wrap_to_gl
# DFFViewport._selected_set_for_mode
# DFFViewport._setup_lighting
# DFFViewport._strip_tex_suffix
# DFFViewport._upload_textures
# DFFViewport.clear_textures
# DFFViewport.fit_to_window
# DFFViewport.flip_horizontal
# DFFViewport.flip_vertical
# DFFViewport.initializeGL
# DFFViewport.load_all_geometries
# DFFViewport.load_geometry
# DFFViewport.load_wheels_dff
# DFFViewport.mouseMoveEvent
# DFFViewport.mousePressEvent
# DFFViewport.mouseReleaseEvent
# DFFViewport.paintGL
# DFFViewport.pan
# DFFViewport.reset_camera
# DFFViewport.reset_view
# DFFViewport.resizeGL
# DFFViewport.rotate_ccw
# DFFViewport.rotate_cw
# DFFViewport.set_ambient
# DFFViewport.set_animation
# DFFViewport.set_animation_speed
# DFFViewport.set_assembly_mode
# DFFViewport.set_backface
# DFFViewport.set_backface_cull
# DFFViewport.set_background_color
# DFFViewport.set_checkerboard_background
# DFFViewport.set_current_model
# DFFViewport.set_diffuse
# DFFViewport.set_light_dir
# DFFViewport.set_prelight
# DFFViewport.set_render_mode
# DFFViewport.set_show_grid
# DFFViewport.set_show_lod
# DFFViewport.set_show_mesh
# DFFViewport.set_view_lock
# DFFViewport.set_wheel_heading
# DFFViewport.toggle_door
# DFFViewport.toggle_snap_axis_constraint
# DFFViewport.toggle_snap_target
# DFFViewport.wheelEvent
# DFFViewport.zoom_in
# DFFViewport.zoom_out
# DFFViewport.set_show_grid
# DFFViewport.set_show_lod
# DFFViewport.set_view_lock
# DFFViewport.wheelEvent
# DFFViewport._upload_textures
"""

import math
import struct
import numpy as np
from typing import Dict, List, Optional

from PyQt6.QtCore import Qt, QPoint
from PyQt6.QtWidgets import QWidget, QLabel
from PyQt6.QtGui import QColor

# Default viewport camera keybindings (Aug 16 2026, per Keith: "the
# arrow keys dont pan or move the view left, right, up or down; the
# arrow keys rotate instead. We need to be able to operate the tools
# with keys, zoom in and out; it could be the numpad + -. A new tab
# is needed in map workshop settings to define keys.") - arrow keys
# now pan (previously rotated - see the class docstring on
# keyPressEvent for that history), numpad 4/6/8/2 keep rotating
# (unchanged), numpad +/- zoom. Each binding is {'key': int(Qt.Key),
# 'numpad': bool} - numpad flag matters for digit/+/- keys (Qt
# doesn't otherwise distinguish a numpad "4" from a top-row "4" by
# key code alone) but not for the dedicated arrow keys. Overridable
# per-instance via set_key_bindings() - map_workshop.py's Settings >
# Keybindings tab persists a user's chosen bindings in MapSettings
# and injects them the same way other viewport settings (background
# colour, path line colour, etc.) already get pushed in.
DEFAULT_KEY_BINDINGS = {
    'pan_left':          {'key': int(Qt.Key.Key_Left),  'numpad': False},
    'pan_right':         {'key': int(Qt.Key.Key_Right), 'numpad': False},
    'pan_up':            {'key': int(Qt.Key.Key_Up),    'numpad': False},
    'pan_down':          {'key': int(Qt.Key.Key_Down),  'numpad': False},
    'rotate_yaw_left':   {'key': int(Qt.Key.Key_4),     'numpad': True},
    'rotate_yaw_right':  {'key': int(Qt.Key.Key_6),     'numpad': True},
    'rotate_pitch_up':   {'key': int(Qt.Key.Key_8),     'numpad': True},
    'rotate_pitch_down': {'key': int(Qt.Key.Key_2),     'numpad': True},
    'zoom_in':           {'key': int(Qt.Key.Key_Plus),  'numpad': True},
    'zoom_out':          {'key': int(Qt.Key.Key_Minus), 'numpad': True},
}
# Human-readable labels for the Settings > Keybindings tab - kept
# alongside the bindings themselves rather than duplicated there,
# since both need to agree on exactly which actions exist.
KEY_BINDING_LABELS = {
    'pan_left':          "Pan Left",
    'pan_right':         "Pan Right",
    'pan_up':            "Pan Up",
    'pan_down':          "Pan Down",
    'rotate_yaw_left':   "Rotate Left (Yaw)",
    'rotate_yaw_right':  "Rotate Right (Yaw)",
    'rotate_pitch_up':   "Rotate Up (Pitch)",
    'rotate_pitch_down': "Rotate Down (Pitch)",
    'zoom_in':           "Zoom In",
    'zoom_out':          "Zoom Out",
}

try:
    from PyQt6.QtOpenGLWidgets import QOpenGLWidget
    from PyQt6.QtGui import QSurfaceFormat
    from OpenGL.GL import *
    from OpenGL.GLU import *
    OPENGL_AVAILABLE = True
    _fmt = QSurfaceFormat()
    _fmt.setProfile(QSurfaceFormat.OpenGLContextProfile.CompatibilityProfile)
    _fmt.setVersion(2, 1)
    # Multisampling (Aug 20 2026, per Keith: "we need some kind of
    # anti-alising, far away lines doesn't appear to flicker") - a
    # real, standard 4x MSAA level, comprehensive rather than line-
    # only (unlike GL_LINE_SMOOTH below, this anti-aliases every
    # primitive - triangle edges too, not just lines), applied here
    # at the surface-format level rather than per-drawing-call, so it
    # costs nothing extra to manage per render path. Never configured
    # here before this - genuinely absent, not just off.
    _fmt.setSamples(4)
    QSurfaceFormat.setDefaultFormat(_fmt)
except Exception:
    QOpenGLWidget = QWidget
    OPENGL_AVAILABLE = False


class DFFViewport(QOpenGLWidget if OPENGL_AVAILABLE else QWidget):
    """OpenGL viewport for RenderWare DFF model rendering.
    Supports wireframe, solid, and textured modes.
    Shared base for Model Viewer, Model Workshop, Vehicle Workshop.
    """

    def __init__(self, parent=None): #vers 2
        super().__init__(parent)
        # Mouse tracking (Aug 19 2026, for auto-highlight-on-hover) -
        # off by default in Qt, meaning mouseMoveEvent normally only
        # fires while a button is actually held. Without this, the
        # hover-detection branch added to mouseMoveEvent below would
        # never receive an event to run at all when no button is
        # pressed, silently doing nothing regardless of the setting -
        # caught this before it could ship as a feature that looked
        # complete but never actually fired.
        self.setMouseTracking(True)
        if OPENGL_AVAILABLE:
            # Per-instance format, not just the module-level default
            # (Aug 1 2026, per Keith: "we have a blank window in the
            # last push... QOpenGLWidget: Failed to create context").
            # QSurfaceFormat.setDefaultFormat (set at this module's
            # import time, above) only reliably takes effect if it
            # runs *before* QApplication is constructed - true when
            # Map Workshop runs standalone (its own __main__ block
            # constructs QApplication after this module is already
            # imported... but genuinely too late whenever this module
            # gets imported into an *already-running* host application
            # instead - exactly Keith's confirmed setup, Map Workshop
            # embedded as a tab inside IMG Factory's own main window,
            # whose QApplication already exists before this module is
            # ever imported). setFormat() directly on each widget
            # instance works correctly regardless of that timing, so
            # doing this too whenever an instance is actually created
            # removes the dependency on import-order timing entirely.
            self.setFormat(_fmt)
        self.setMinimumSize(200, 200)
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)

        # Geometry data
        self._vertices:  List  = []
        self._normals:   List  = []
        self._uvs:       List  = []
        self._triangles: List  = []
        self._materials: List  = []
        self._prelit:    List  = []
        self._tex_ids:   Dict[str,int] = {}
        self._tex_wrap:  Dict[str,tuple] = {}

        # Camera
        self._dist  = 10.0
        self._yaw   = 45.0
        self._pitch = 25.0
        self._pan_x = 0.0
        self._pan_y = 0.0
        self._last_pos = QPoint()
        # Configurable keyboard camera controls (Aug 16 2026) - see
        # DEFAULT_KEY_BINDINGS' own comment above for the full story.
        self._key_bindings = dict(DEFAULT_KEY_BINDINGS)

        # Render state
        self._mode          = 'solid'
        self._backface_cull = False  # GTA models are often 2-sided; off by default
        self._show_grid     = True
        # Grid visual style (Aug 20 2026, per Keith: "can we have an
        # option for grid type squares, grid with blue square inside,
        # marching ants lines, just dots, and switch grid off
        # completly" - "off" is the existing self._show_grid=False,
        # this is the style used whenever it's True). 'lines': the
        # original, only style this ever had - open grid lines, no
        # fill. 'squares': each cell gets a real, semi-transparent
        # blue fill in addition to its own outline (Keith's own
        # "blue square inside" wording, not just a differently-
        # coloured outline). 'dashed': the real "marching ants" look
        # - genuinely dashed lines via GL_LINE_STIPPLE (a real, valid
        # legacy-OpenGL feature this app's own fixed-function pipeline
        # already relies on elsewhere, not something invented for
        # this), not just a colour/width change on the existing solid
        # lines. 'dots': only the real grid intersection points drawn
        # (GL_POINTS), no connecting lines at all - genuinely sparser
        # than every other style, not dots drawn along the same lines.
        self._grid_type      = 'lines'
        # 'squares' grid style's own real fill (Aug 20 2026, per
        # Keith: "in the preview settings, the option with blue full
        # colour, option to set the colour, blue as default, or a
        # texture shown as the grid, with settings for 64x64 -
        # 1028x1028 tiled" - "1028" read as the standard power-of-2
        # 1024, the nearest real texture size to that figure, not a
        # literal 1028). fill_mode='color' (the existing, original
        # behaviour, unchanged) or 'texture' (a real image, tiled at
        # tile_size world units per repeat - genuinely independent of
        # the grid's own step spacing, since Keith's own request was
        # for a configurable tile size, not "make the texture match
        # whatever step happens to be active"). color stored as a
        # real (r,g,b) 0-255 tuple, matching the same convention
        # already used for cull/zone/occlusion box colours elsewhere
        # in this app - blue (51,128,230) as the real, stated default.
        # tex_id lazily loaded/cached the same real way _ensure_auzo_
        # icon_texture's own SVG texture already is, not reloaded
        # every frame - see _ensure_squares_texture's own docstring.
        self._squares_fill_mode  = 'color'
        self._squares_color      = (51, 128, 230)
        self._squares_texture_path = ''
        self._squares_tile_size  = 256
        self._squares_tex_id     = None
        self._squares_tex_path_loaded = None   # which real path tex_id was actually loaded from
        self._squares_texture_alpha = 0.5   # texture-fill opacity, 0.0-1.0
        self._grid_show_lines = True   # separate from grid_type='none' - lets squares/texture fill show without outline lines on top
        self._grid_hide_over_radar_tiles = False   # suppress grid lines within the radar tex layer's own real bounds, still shown outside
        # Grid line/dot colour + thickness (Aug 20 2026)
        self._grid_line_color = (76, 76, 102)   # matches old (0.3,0.3,0.4)
        self._grid_line_size  = 4               # 4-10px range
        self._grid_spacing    = 5               # divisor of _dist -> step (zoom_relative mode)
        self._grid_fixed_step = 200             # world units per cell (fixed mode - real map scale, not the small zoom_relative divisor)
        self._grid_cell_count = 24               # total grid diameter in cells (replaces the old fixed *10 multiplier)
        # Radar tex layer - real generated radar tiles shown at their
        # own real world positions, as an alternative to the grid
        self._show_radar_tex_layer = False
        self._radar_tex_tiles = []      # list of dicts: path, min_x, min_y, max_x, max_y, tex_id
        self._grid_scale_mode = 'zoom_relative'  # or 'fixed' or 'radar_tiles'
        self._radar_grid_tile_size = 500.0   # world units per tile, set from RADAR_GRID_PRESETS
        self._radar_grid_half_extent = 3000.0   # grid_size/2 for the current game
        # Skybox/skydome background image (Aug 20 2026)
        self._skybox_path = ''
        self._skybox_tex_id = None
        self._skybox_tex_path_loaded = None
        # Timecyc file + play/stop time-of-day (Aug 20 2026)
        self._timecyc_path = ''
        self._timecyc_entries = []     # list of parsed weather/hour entries
        self._timecyc_playing = False
        self._timecyc_hour = 12.0
        self._sky_gradient_top = None   # (r,g,b) or None - set by _apply_timecyc_hour
        self._sky_gradient_bot = None
        self._sky_gradient_horizon = None   # brighter horizon-glow colour (from sun_core)
        self._sky_gradient_flipped = False   # settings toggle - swaps zenith/horizon in case the timecyc data's own real orientation turns out to be the other way
        self._ambient_tint = (1.0, 1.0, 1.0)   # RGB multiplier, set by _apply_timecyc_hour
        self._use_prelight  = False
        self._ambient       = 0.4
        self._diffuse       = 0.9
        self._light_dir     = (0.5, 1.0, 0.7, 0.0)
        self._paint1        = (1.0, 0.0, 0.0)
        self._paint2        = (0.0, 0.0, 1.0)

        # Assembly / LOD
        self._all_geoms     = []
        self._assembly_mode = False
        self._show_lod      = False

        # World instances (Aug 1 2026, per Keith: "wire every pane
        # into the viewport, when I load ipl, these dont show" -
        # full multi-instance 3D world view) - each entry is a dict:
        # {'vertices','normals','uvs','triangles','materials','prelit',
        #  'pos':(x,y,z), 'rot':(x,y,z,w) quaternion, 'scale':(x,y,z)}.
        # Distinct from _all_geoms (which draws multiple geometries at
        # the SAME shared origin/camera transform, for viewing one
        # DFF's assembled parts) - these each get their own
        # glPushMatrix/glTranslatef/rotate/glScalef/glPopMatrix.
        self._world_instances = []
        # Display-list cache, keyed by (model_key, render mode) (Aug 1
        # 2026, per Keith: "bottlenecking is trying to move the
        # objects in the viewer") - immediate-mode OpenGL (glBegin/
        # glVertex per triangle) was being fully re-executed in Python
        # for every instance, every single repaint (including every
        # frame during an interactive camera drag) - with many
        # instances sharing a handful of distinct models, this
        # compiles each DISTINCT model's geometry into a GL display
        # list ONCE, then every instance of it just replays the
        # pre-compiled list (glCallList) - the expensive per-triangle
        # work only happens once per model per render mode, not once
        # per instance per frame.
        self._world_display_lists = {}
        # Dots mode's own cube shape (Aug 20 2026) - deliberately NOT
        # cleared alongside self._world_display_lists on a new world
        # load (see clear_world_instances just below) - unlike those,
        # this one static shape never depends on which world/models
        # are currently loaded at all, so it's compiled once per
        # session and reused across every world load, not needlessly
        # rebuilt every time a new IPL loads.
        self._dots_cube_list_id = None

        # Collision overlay toggles (Aug 14 2026, per Keith: "add
        # collisions to the IPL control pane... load solid collision,
        # load semi-solid, wireframe cols, and solid with surface
        # mapping" -> "Ghost is a good idea; Show Ghosted Col, Show
        # Surface Mapped Col, Show Semi-Solid Col, Show Wireframe
        # Col") - four independent checkboxes, not an exclusive
        # group like render mode: any combination can be on at once
        # (e.g. Wireframe Col over a Ghosted Col fill is a normal
        # thing to want). Each draws as an overlay on top of the
        # already-drawn model, never replacing it - "ghost" is the
        # whole point, not just one of the four modes. All off by
        # default.
        self.show_col_ghosted        = False
        self.show_col_semi_solid     = False
        self.show_col_wireframe      = False
        self.show_col_surface_mapped = False
        # Separate display-list cache, keyed by (model_key, col mode) -
        # mirrors _world_display_lists exactly but kept apart since a
        # model's collision geometry is entirely different data
        # (COLModel vertices/faces, not DFF) from its render mesh, and
        # the two get cleared independently: toggling a collision
        # checkbox never needs to touch the model's own display lists.
        self._col_display_lists = {}

        # Path visualization (Aug 14 2026, per Keith: "when
        # displaying the paths in the viewpoint, I was expecting red
        # lines and nodes. And a way to change the colour of the path
        # lines in settings") - each entry in _path_segments is a pair
        # of (x,y,z) endpoints, ((x1,y1,z1),(x2,y2,z2)), one real
        # graph edge - NOT a polyline/list-of-consecutive-nodes
        # (Aug 16 2026 rework, per Keith's real screenshot: "they
        # don't look linked, node to node, instead one point" - long
        # spurious lines fanning from one area. Root cause: a path
        # group's raw on-disk node order does NOT match its actual
        # connectivity - confirmed against Project Cerbera's own VC
        # paths.ipl format doc: each node has its own "Next" field, a
        # 0-11 index into that SAME group's fixed 12-node array
        # (verified against Cerbera's own worked example, e.g. node 8
        # linking to node 11, skipping 9-10 entirely) - naive
        # "connect node i to node i+1" was simply the wrong topology,
        # not just missing a few links. map_workshop.py's conversion
        # step now builds the real edge list per node.node_type/
        # next_id (see _refresh_path_visualization) - this widget
        # stays pure-GL, no PathGroup/PathNode dataclass dependency
        # here, just consumes whatever segments it's given). Off by
        # default, matching every other optional overlay in this
        # widget (Show Tobj, the Col overlays).
        self.show_paths = False
        self._path_segments = []
        self._path_line_color = (1.0, 0.0, 0.0)   # red, per Keith's expectation
        self._path_node_color = (1.0, 0.8, 0.0)   # amber - distinct from the line itself
        # Line thickness/node size (Aug 16 2026, per Keith: "under
        # rander in settings, line thinkness, and node circle size,
        # and color change option") - defaults match the values that
        # used to be hardcoded (2.0->1.2px line, 6->3.5px node, from
        # the earlier "blend in with the map" softening pass).
        self._path_line_thickness = 1.2
        self._path_node_size = 3.5

        # Interactive path node editing (Aug 17 2026, per Keith: "lets
        # address the unbuilt work, editing paths first" - a real
        # click-to-select-and-drag interaction for path nodes, the
        # first piece of the larger "editing paths / moving whole IPL
        # sections / rotating map sections" request, tackled in the
        # order Keith himself prioritised). Scoped to VC/SA-style
        # loader.paths only (the same scope New/Delete Path Group
        # already settled on) - GTA III's own IDE-embedded paths
        # attach to instances by model_id rather than holding a
        # position of their own, a fundamentally different edit model
        # not covered here.
        #
        # _path_node_owner_map keys each unique node's rounded (x,y,z)
        # position to (group_ref, node_index) - the real, live
        # PathGroup object and which of its 12 node slots this is -
        # so a completed drag can be committed back to the actual
        # data, not just this widget's own display cache. Populated
        # by map_workshop.py's _refresh_path_visualization via set_
        # path_node_owners, built alongside the segments list every
        # refresh so the two never drift apart.
        self._path_edit_mode = False
        self._path_node_owner_map = {}
        self._dragging_path_node_start_key = None
        self._dragging_path_node_current_pos = None
        self._path_node_drag_callback = None

        # Whole-IPL-section dragging (Aug 18 2026, per Keith's own
        # priority order for the interactive editing layer - "editing
        # paths first" [done], then this: "Moving IPL file whole
        # entires to anywhere on the map"). Click-drag any instance
        # belonging to a loaded IPL to move that IPL's ENTIRE data as
        # one rigid body - reuses update_instance_transform (built
        # earlier for the Item Editor Dialog's own fast-path nudges)
        # for cheap, real-time visual feedback on every instance
        # belonging to the dragged IPL without touching the real
        # IPLInstance data until release, and reuses the already-
        # existing, already-verified _shift_ipl_coordinates (the
        # dialog-based Shift Coordinates tool) to actually commit the
        # move - including paths/cull/zone/occl, not just instances -
        # once the drag finishes. First version's own honest scope
        # limit: only INSTANCES get live visual feedback during the
        # drag itself (cull/zone/occl boxes and paths have no
        # equivalent identity-based "just update this one cached
        # entry" mechanism the way instances do) - those snap to
        # their correct new position on release, not mid-drag.
        self._ipl_drag_mode = False
        # Multi-IPL selection/drag (Aug 19 2026, per Keith's own
        # careful workflow spec: "holding [left control] left click
        # entire .ipl is dragged / holding [left shift] and select
        # multi entire ipls, ... if there all selected, it drags them
        # all"). Ctrl+click starts an immediate single-IPL drag (the
        # original, already-built behaviour, just now gated behind
        # Ctrl instead of being the only option a plain click gave).
        # Shift+click doesn't drag anything by itself - it toggles
        # that instance's own whole IPL into/out of _multi_selected_
        # ipl_names, building up a selection across as many separate
        # Shift+clicks as wanted. A later plain click+drag (no
        # modifier held), as long as that selection is non-empty,
        # drags every one of those selected IPLs together as one
        # combined rigid-body move - _dragging_ipl_names holds
        # whichever IPL name(s) are actually being dragged at any
        # given moment (set at press time, either {the one Ctrl-
        # clicked IPL} or a copy of the whole multi-selection),
        # generalised from a single name to a set so the same
        # mouseMove/mouseRelease logic below works unchanged whether
        # one IPL or several are being moved together.
        self._multi_selected_ipl_names = set()
        self._dragging_ipl_names = set()
        self._dragging_ipl_start_state = []   # list of (inst, pos, rot, scale) at drag start
        self._dragging_ipl_ground_start = None
        self._dragging_ipl_delta = (0.0, 0.0, 0.0)
        self._dragging_ipl_clicked_inst = None
        self._dragging_ipl_clicked_start_pos = None
        self._ipl_drag_callback = None
        self._ipl_selection_callback = None
        # Axis lock (Aug 18 2026, per Keith: "[Drag ipl] right-click
        # options, like lock z, only move x, y"). Z is already always
        # effectively locked by the existing ground-plane-constrained
        # drag design (the plane is fixed at the clicked instance's
        # own starting height, so the resolved delta's own Z
        # component is always 0 regardless of this setting - there's
        # no separate "lock Z" toggle needed for that). This
        # specifically covers the two REMAINING practical choices:
        # locking X (so only Y actually moves) or locking Y (so only
        # X moves) - None means free X/Y movement, the existing
        # default behaviour.
        self._ipl_drag_axis_lock = None
        # 3-state Drag/Move/Rotate cycle (Aug 19 2026, per Keith: "1
        # click turns into move ipl, click again rotate ipl, click
        # back to drag ipl"). 'drag' (the existing default) means a
        # click-and-hold on an instance starts the live-preview mouse
        # drag already built; 'move'/'rotate' mean a plain click
        # instead immediately fires ipl_click_callback with the
        # picked IPL's name and does NOT start any drag tracking at
        # all - map_workshop.py opens the corresponding numeric
        # dialog (Shift Coordinates / Rotate) from that callback,
        # since those two are precise-numeric-entry interactions, not
        # mouse-drag ones.
        self._ipl_interaction_mode = 'drag'
        self._ipl_click_callback = None

        # Auto-highlight on hover (Aug 19 2026, per Keith: "Auto
        # object highlight setting in map_workshop settings: this
        # could be a model, path node, anything in the viewpoint;
        # once highlighted, right-click for options"). Off by
        # default - a real, continuous per-mouse-move cost (same
        # class of cost LOD Test mode's own callback already pays,
        # not a new kind of expense this app hasn't already accepted
        # elsewhere), so opt-in rather than always-on. Scoped to
        # instances only for this first version, not "anything" quite
        # yet - path nodes already have their own dedicated pick-up-
        # and-drag interaction in Edit Paths mode, a genuinely
        # different, more specific gesture than a general hover
        # highlight, so left for a future pass rather than merged in
        # here without a clear picture of how the two should coexist
        # if both were active at once.
        self._hover_highlight_enabled = False
        self._hovered_instance_idx = None
        self._hover_context_callback = None

        # Train track waypoints (Aug 17 2026, per Keith: "then the
        # other path .dat files you pointed out earlier") - each
        # track is drawn as one continuous polyline (ordered waypoint
        # list, unlike VC/GTA3 paths' own Type/Next node graph - real
        # tracks.dat/tracks2.dat data confirmed this is genuinely just
        # a simple ordered point sequence, nothing more complex).
        # Silver/grey by default, distinct from every other overlay
        # colour this widget already uses, loosely evoking real rail.
        self.show_tracks = False
        self._track_polylines = []   # list of [(x,y,z), ...] - one per track file
        self._track_color = (0.75, 0.75, 0.8)
        self._track_line_thickness = 1.5
        # Airtrain/Plane path colour - settings-only for now, no
        # confirmed data field to split these from generic paths yet
        self._airtrain_color = (0.9, 0.6, 0.2)
        self._airtrain_line_thickness = 1.2

        # SA path node graph (Aug 19 2026, per Keith's real NODES0-63.
        # DAT data - "lets do those next" following the whole real
        # path-file investigation this session). Unlike tracks (one
        # continuous ordered line strip per file), SA's own path data
        # is a genuine graph - disconnected line segments, one per
        # link between two nodes, not a single strip - so this is a
        # flat list of (start_xyz, end_xyz) segment pairs rather than
        # a list of polylines. Resolution (looking up each link's own
        # target node position, including across different area
        # files, since links can cross area boundaries) happens in
        # map_workshop.py, not here - this widget only ever draws
        # already-resolved plain coordinate pairs, matching every
        # other overlay's own "widget draws plain data, caller
        # resolves it from the real objects" split.
        self.show_sa_nodes = False
        self._sa_node_segments = []   # list of ((x1,y1,z1),(x2,y2,z2))
        self._sa_node_color = (0.3, 0.9, 0.5)   # green, distinct from tracks' own silver-grey

        # SA audio zones (Aug 20 2026, per Keith: "Implement support
        # for the remaining SA, audiozone placements with sound svg
        # icons; play the sounds"). Each entry is a plain (center_x,
        # center_y, center_z, name, sound_id, environment_type,
        # music_description) tuple - resolution (cube-vs-sphere
        # center point, AUZO_TYPES lookup) happens in map_workshop.py,
        # this widget only ever deals in plain, already-resolved data,
        # matching every other overlay's own split. Rendered as a
        # billboarded (always facing the camera) sound-icon texture
        # quad at each zone's own center rather than a wireframe box/
        # sphere outline the way cull/zone/occl already are - Keith's
        # own request was specifically for icons, not shape outlines.
        self.show_auzo_zones = False
        self._auzo_zones = []
        self._auzo_icon_tex_id = None   # lazy-loaded once, cached (see _ensure_auzo_icon_texture)

        # Water shapes (Aug 20 2026, per Keith: "lets get all the
        # functions in" - water/radar recalculation on map moves).
        # Each entry is a plain list of (x,y,z) corner tuples (3 or 4
        # per shape) plus a water_type int - resolution (converting
        # from the real WaterShape/WaterCorner dataclasses) happens in
        # map_workshop.py, matching every other overlay's own "widget
        # draws plain data, caller resolves it" split. Drawn as flat,
        # translucent polygons rather than 3D boxes, since a real
        # water shape genuinely is a flat plane, not a volume - a
        # wireframe box the way cull/zone/occlusion already draw
        # would misrepresent the real shape.
        self.show_water = False
        self._water_shapes = []
        self._waterpro_cells = []   # list of (min_x, min_y, max_x, max_y, height) - flat, pre-resolved from map_workshop.py
        self._water_display_style = 'fill'   # 'fill'/'lines'/'dots'/'hexagons'
        self._water_texture_path = ''
        self._water_texture_tex_id = None
        self._water_texture_tex_path_loaded = None
        self._water_tile_size = 256
        self._water_hide_outside_map = False
        self._water_map_half_extent = 3000.0   # grid_size/2 for the currently loaded game

        # Cull zone boxes (Aug 16 2026, per Keith: "continue with the
        # cull files next", following the same "so I can view them"
        # pattern as Show Paths/the .zon wiring) - the older MapView-
        # port class (depends/map_viewport.py) already had cull-box
        # drawing, but it's only ever reachable through the disabled
        # 4-Pane View feature, not this, the actual primary viewport
        # - cull boxes have never been visible to Keith in practice.
        # Each entry in _cull_boxes is a plain (x1,y1,z1,x2,y2,z2)
        # tuple - map_workshop.py's own conversion step reads the
        # real CullEntry dataclass fields (fixed this session - see
        # CullEntry's own docstring for the "was a wrong 7-field
        # center/width/height guess, real format has 11 fields, two
        # genuine corner points" story), this widget only ever deals
        # in plain corner coordinates, same separation as paths/Col
        # overlays elsewhere in this file.
        self.show_cull_boxes = False
        self._cull_boxes = []
        self._cull_box_color = (1.0, 0.85, 0.2)   # amber-yellow, distinct from paths' red

        # Zone boxes (Aug 16 2026, per Keith: "ive loaded zon files,
        # these show in the IPL File Display and the ZON tab is
        # highlighted, but I cant see them in the viewpoint") - .zon
        # loading/table-display was wired earlier this session, but
        # nothing ever pushed the parsed zones to the 3D view - this
        # is genuinely new, zones never had ANY viewport rendering at
        # all before, unlike cull (which at least had dead code
        # reaching an unreachable disabled feature). Same shape as
        # cull boxes (min/max corners, axis-aligned) so this mirrors
        # _draw_cull_boxes directly, just a different default colour.
        self.show_zone_boxes = False
        self._zone_boxes = []
        self._zone_box_color = (0.3, 0.7, 1.0)   # sky blue, distinct from cull's amber and paths' red
        # Zon render style (Aug 16 2026, per Keith: "in zons, the
        # render dropdown could show, Zon - Ghosted, Zon - Wireframe,
        # Zon - translucent") - one of 'ghosted' (filled+outlined,
        # default), 'wireframe' (edges only), 'translucent' (filled
        # only, no outline, more see-through than ghosted). Scoped to
        # zone boxes only, per Keith's own request - cull/occlusion
        # keep their fixed ghosted look.
        self._zone_render_style = 'ghosted'

        # Occlusion zones (Aug 16 2026, continuing the cull/zon work -
        # "occl" was never even a recognised VC section keyword
        # before this, let alone rendered) - each entry is a plain
        # (mid_x, mid_y, bottom_z, width_x, width_y, height, rotation)
        # tuple, NOT axis-aligned like cull/zone boxes: rotation turns
        # the box around its own vertical (Z) axis, so this needs its
        # own draw method (computing 4 rotated corners from the
        # center+half-extents) rather than reusing _draw_wireframe_
        # boxes, which only ever takes two already-axis-aligned
        # corner points.
        self.show_occl_boxes = False
        self._occl_boxes = []
        self._occl_box_color = (1.0, 0.4, 0.8)   # pink, distinct from cull/zone/paths
        # Axis-colored box faces (Aug 18 2026, per Keith: "Cull, Occl,
        # Zon boxes have coloured sides: x (green), y (red) and z
        # (blue) faces, which makes that easy to see") - off by
        # default, overrides each box type's own configured colour
        # when on (see _draw_ghosted_box_from_corners's own docstring
        # for the exact per-face colour logic).
        self._box_axis_colors = False

        # Unique colour per box (Aug 19 2026, per Keith: "add colour
        # zone boxes" - with real reference screenshots showing
        # several distinct, individually-coloured cull/zone boxes
        # side by side, not one uniform colour per box TYPE the way
        # this app already does, and not axis-face colouring either -
        # each individual box gets its own colour so adjacent/
        # overlapping zones are easy to tell apart at a glance). A
        # fixed, varied palette assigned by each box's own index
        # within its loaded list, cycling if there are more boxes
        # than palette entries - deterministic (the same box always
        # gets the same colour within one session, not randomised
        # every refresh) rather than a true random colour per box,
        # which would flicker differently on every reload. Takes
        # priority under axis_colored when both would otherwise
        # apply to the same box - see _draw_ghosted_box_from_corners's
        # own docstring for the exact precedence.
        self._box_unique_colors = False
        self._box_color_palette = [
            (1.0, 0.55, 0.0),   # orange
            (0.2, 0.8, 0.2),    # green
            (1.0, 0.2, 0.8),    # magenta/pink
            (0.85, 0.15, 0.15), # red
            (0.25, 0.45, 1.0),  # blue
            (0.95, 0.85, 0.1),  # yellow
            (0.6, 0.25, 0.85),  # purple
            (0.1, 0.75, 0.75),  # teal
        ]

        # Box corner resize (Aug 19 2026, per Keith's own priority
        # order - "lets continue to complete that list. starting with
        # No-clip" - box resizing itself is the real prerequisite,
        # per this same feature's own earlier docstring: "actually
        # moving a corner to resize the box is a separate, larger
        # follow-up... mouse picking, drag math, and live data
        # mutation, none of which exist yet"). Scoped to cull/zone
        # only for this first version - both are simple, axis-aligned
        # two-corner boxes with no rotation of their own to complicate
        # things; occlusion boxes DO have their own rotation field, so
        # "drag a corner" there would mean interpreting the drag in
        # the box's own rotated local space rather than world space
        # directly, a genuinely different, harder problem left for a
        # separate pass rather than guessed at here.
        #
        # _pickable_box_corners is rebuilt fresh every _draw_cull_
        # boxes/_draw_zone_boxes call (same "rebuild every refresh"
        # pattern _path_node_owner_map already uses) - each entry maps
        # a real, currently-drawn corner sphere's own world position
        # to (box_type, box_ref, opposite corner's own position), so a
        # click can resolve straight back to which real box/corner was
        # actually picked.
        self._box_edit_mode = False
        self._pickable_box_corners = {}
        self._dragging_box_corner_key = None
        self._dragging_box_corner_info = None   # (box_type, box_ref, fixed_opposite_xy, z1, z2)
        self._dragging_box_corner_current_pos = None
        self._box_resize_callback = None
        self._no_clip_boxes = False


        # Wheels
        self._wheels_model      = None
        self._wheels_model_path = ''
        self._wheel_type        = 'wheel_saloon_l0'

        # App settings ref (optional — set by host tool)
        self.app_settings = None
        # Explicit background override (R,G,B 0-255) — None means use theme colour
        self._bg_color_override = None

        # Sub-object selection state (vertex / edge / face / poly / object)
        self._selected_verts = set()    # set of vertex indices
        self._selected_edges = set()    # set of (vi, vj) tuples, vi < vj
        self._selected_faces = set()    # set of triangle indices
        self._select_mode    = 'object'  # 'vertex'|'edge'|'face'|'poly'|'object'

        # Snap target toggles (Aug 19 2026, per Keith: "if the snap
        # options are on, icons already exist on ribbons; use Edge of
        # model, Centre of model, then we can remove the snaps we
        # dont need from the ribbons" - simplified down from the
        # original 7 mesh-editing-style targets (grid/pivot/vertex/
        # endpoint/midpoint/edge/face) inherited from Model Workshop's
        # own base, none of which were ever actually wired to any real
        # behaviour here - confirmed by this code's own prior comment
        # ("the actual snap-during-drag math... is a follow-up task,
        # not wired yet") before removing anything, not assumed.
        # 'centre' is the one made genuinely functional this pass -
        # see DFFViewport's own whole-IPL-drag mouseMoveEvent logic.
        # 'edge' stays as a real toggle but its own ribbon button is
        # disabled with an explanatory tooltip rather than faked: a
        # genuine "snap to the edge of a model" needs that model's own
        # loaded geometry bounding box, which doesn't exist anywhere
        # in this viewport yet - approximating it with an arbitrary
        # offset would look like real geometry-based snapping while
        # actually being a guess, which is worse than being upfront
        # that it isn't built yet.
        self._snap_targets = {'edge': False, 'centre': False}
        self._snap_axis_constraint = False   # "Enable Axis Constraints in Snaps"

        # Multi-pane view lock (3ds Max style Top/Front/Side/Perspective panes)
        self._view_locked = False
        self._view_label  = ""
        self._projection  = 'perspective'   # 'perspective' or 'ortho'
        self._on_geometry_loaded = None     # optional callback, set by host tool

        self._label_widget = QLabel(self)
        self._label_widget.setStyleSheet(
            "color: rgba(255,255,255,190); background: transparent; font-size: 10px;")
        self._label_widget.move(4, 2)
        self._label_widget.hide()

    def _get_ui_color(self, key): #vers 2
        """Get theme color — tries app_settings, falls back to defaults."""
        defaults = {
            'bg_panel': (25, 25, 35),
            'text_primary': (220, 220, 220),
            'border': (60, 60, 80),
        }
        if self.app_settings:
            try:
                colors = self.app_settings.get_theme_colors()
                val = colors.get(key, '')
                if val and val.startswith('#'):
                    r = int(val[1:3], 16)
                    g = int(val[3:5], 16)
                    b = int(val[5:7], 16)
                    return QColor(r, g, b)
            except Exception:
                pass
        rgb = defaults.get(key, (40, 40, 50))
        return QColor(*rgb)

    def _get_bg_color(self): #vers 1
        """Resolve actual background colour — explicit override takes priority over theme."""
        if self._bg_color_override is not None:
            return QColor(*self._bg_color_override)
        return self._get_ui_color('bg_panel')

    # - Sub-object picking (vertex / edge / face)
    # Replicates paintGL's camera transform so a ray can be cast from a
    # mouse click even though picking happens outside the paint cycle.

    def _pick_ray(self, mx: float, my: float): #vers 1
        """Return (origin, direction) as two (x,y,z) tuples for a world-space
        ray through the given widget-space pixel, or None if GL/picking
        isn't available right now (e.g. widget not yet shown)."""
        if not OPENGL_AVAILABLE or not self.isValid():
            return None
        try:
            self.makeCurrent()
            glMatrixMode(GL_PROJECTION); glLoadIdentity()
            w = max(1, self.width()); h = max(1, self.height())
            gluPerspective(45.0, w / h, 0.01, 100000.0)
            glMatrixMode(GL_MODELVIEW); glLoadIdentity()
            gluLookAt(0, 0, self._dist, 0, 0, 0, 0, 1, 0)
            glRotatef(-self._pitch, 1, 0, 0)
            glRotatef(self._yaw, 0, 0, 1)
            glTranslatef(self._pan_x, self._pan_y, 0)

            model_mat = glGetDoublev(GL_MODELVIEW_MATRIX)
            proj_mat  = glGetDoublev(GL_PROJECTION_MATRIX)
            viewport  = glGetIntegerv(GL_VIEWPORT)
            # Qt widget Y is top-down; GL viewport Y is bottom-up
            wy = h - my

            near = gluUnProject(mx, wy, 0.0, model_mat, proj_mat, viewport)
            far  = gluUnProject(mx, wy, 1.0, model_mat, proj_mat, viewport)
            self.doneCurrent()
        except Exception:
            try: self.doneCurrent()
            except Exception: pass
            return None

        ox, oy, oz = near
        dx, dy, dz = far[0]-near[0], far[1]-near[1], far[2]-near[2]
        ln = math.sqrt(dx*dx + dy*dy + dz*dz) or 1.0
        return (ox, oy, oz), (dx/ln, dy/ln, dz/ln)

    @staticmethod
    def _point_seg_dist2(p, a, b): #vers 1
        """Squared distance from point p to segment a-b (3D)."""
        abx, aby, abz = b[0]-a[0], b[1]-a[1], b[2]-a[2]
        apx, apy, apz = p[0]-a[0], p[1]-a[1], p[2]-a[2]
        ab2 = abx*abx + aby*aby + abz*abz
        t = 0.0 if ab2 < 1e-12 else max(0.0, min(1.0, (apx*abx+apy*aby+apz*abz)/ab2))
        cx, cy, cz = a[0]+abx*t, a[1]+aby*t, a[2]+abz*t
        ddx, ddy, ddz = p[0]-cx, p[1]-cy, p[2]-cz
        return ddx*ddx + ddy*ddy + ddz*ddz

    def _closest_point_on_ray(self, origin, direction, point): #vers 1
        """Param t (distance along ray) of the closest approach to `point`,
        and the squared distance from the ray to that point at that t."""
        ox, oy, oz = origin; dx, dy, dz = direction
        px, py, pz = point[0]-ox, point[1]-oy, point[2]-oz
        t = px*dx + py*dy + pz*dz
        cx, cy, cz = ox+dx*t, oy+dy*t, oz+dz*t
        ddx, ddy, ddz = point[0]-cx, point[1]-cy, point[2]-cz
        return t, ddx*ddx + ddy*ddy + ddz*ddz

    def _pick_vertex(self, mx: float, my: float): #vers 1
        """Return index of the closest vertex to the ray through (mx,my)
        within a small screen-space-equivalent tolerance, or None."""
        ray = self._pick_ray(mx, my)
        if ray is None or not self._vertices:
            return None
        origin, direction = ray
        # Tolerance scales with camera distance so it stays roughly
        # constant in screen pixels regardless of zoom.
        tol2 = (self._dist * 0.02) ** 2
        best_i, best_t, best_d2 = None, None, tol2
        for i, v in enumerate(self._vertices):
            t, d2 = self._closest_point_on_ray(origin, direction, v)
            if t < 0:
                continue
            if d2 < best_d2 or (best_i is not None and d2 <= best_d2 and t < best_t):
                best_i, best_t, best_d2 = i, t, d2
        return best_i

    def _pick_edge(self, mx: float, my: float): #vers 1
        """Return (vi, vj) (vi<vj) of the closest triangle edge to the ray,
        or None. Edges are derived from triangle sides, deduplicated."""
        ray = self._pick_ray(mx, my)
        if ray is None or not self._vertices or not self._triangles:
            return None
        origin, direction = ray
        tol2 = (self._dist * 0.02) ** 2
        edges = set()
        for tri in self._triangles:
            a, b, c = tri[0], tri[1], tri[2]
            for i, j in ((a, b), (b, c), (c, a)):
                edges.add((i, j) if i < j else (j, i))
        best_key, best_t, best_d2 = None, None, tol2
        verts = self._vertices
        for (i, j) in edges:
            try:
                va, vb = verts[i], verts[j]
            except IndexError:
                continue
            mid = ((va[0]+vb[0])/2, (va[1]+vb[1])/2, (va[2]+vb[2])/2)
            t, d2 = self._closest_point_on_ray(origin, direction, mid)
            if t < 0:
                continue
            if d2 < best_d2 or (best_key is not None and d2 <= best_d2 and t < best_t):
                best_key, best_t, best_d2 = (i, j), t, d2
        return best_key

    def _pick_world_instance(self, mx: float, my: float): #vers 1
        """Return the index into self._world_instances closest to the
        ray through (mx,my), within a tolerance that scales with
        camera distance (same pattern as _pick_vertex/_pick_edge just
        above - reuses the exact same _pick_ray/_closest_point_on_ray
        infrastructure, just testing against each instance's world
        position instead of mesh vertices/edges). Picks by distance
        from the instance's origin point to the ray, not full
        per-triangle mesh intersection (_ray_triangle_intersect exists
        and would be more precise, but re-testing every triangle of
        every instance on every click would be considerably slower for
        a whole loaded map - this is fast and good enough for clicking
        roughly on/near an object). Aug 1 2026, per Keith: "im trying
        to select a tree double clicking on it, so I can see its edit
        dialog window.\""""
        ray = self._pick_ray(mx, my)
        if ray is None or not self._world_instances:
            return None
        origin, direction = ray
        tol2 = (self._dist * 0.05) ** 2
        best_i, best_t, best_d2 = None, None, tol2
        for i, entry in enumerate(self._world_instances):
            pos = entry.get('pos')
            if pos is None:
                continue
            t, d2 = self._closest_point_on_ray(origin, direction, pos)
            if t < 0:
                continue
            if d2 < best_d2 or (best_i is not None and d2 <= best_d2 and t < best_t):
                best_i, best_t, best_d2 = i, t, d2
        return best_i

    def mouseDoubleClickEvent(self, event): #vers 1
        """Double-clicking a world instance opens its edit dialog (Aug
        1 2026, per Keith - see _pick_world_instance's docstring).
        Only active when a world (multi-instance) view is actually
        loaded - self._workshop_ref is set at construction
        (model_workshop.py) regardless of mode, so this checks
        _world_instances specifically rather than assuming."""
        if self._world_instances:
            pos = event.position()
            idx = self._pick_world_instance(pos.x(), pos.y())
            if idx is not None:
                ws = getattr(self, '_workshop_ref', None)
                if ws is not None and hasattr(ws, '_on_world_instance_picked'):
                    ws._on_world_instance_picked(idx)
                    return
        super().mouseDoubleClickEvent(event)

    def _ray_triangle_intersect(self, origin, direction, v0, v1, v2): #vers 1
        """Möller–Trumbore ray/triangle test. Returns t (distance along the
        ray) on hit, or None. Backface-tolerant (tests both winding orders)."""
        eps = 1e-9
        e1 = (v1[0]-v0[0], v1[1]-v0[1], v1[2]-v0[2])
        e2 = (v2[0]-v0[0], v2[1]-v0[1], v2[2]-v0[2])
        dx, dy, dz = direction
        px = dy*e2[2] - dz*e2[1]
        py = dz*e2[0] - dx*e2[2]
        pz = dx*e2[1] - dy*e2[0]
        det = e1[0]*px + e1[1]*py + e1[2]*pz
        if -eps < det < eps:
            return None
        inv_det = 1.0 / det
        tx, ty, tz = origin[0]-v0[0], origin[1]-v0[1], origin[2]-v0[2]
        u = (tx*px + ty*py + tz*pz) * inv_det
        if u < -1e-6 or u > 1 + 1e-6:
            return None
        qx = ty*e1[2] - tz*e1[1]
        qy = tz*e1[0] - tx*e1[2]
        qz = tx*e1[1] - ty*e1[0]
        v = (dx*qx + dy*qy + dz*qz) * inv_det
        if v < -1e-6 or u + v > 1 + 1e-6:
            return None
        t = (e1[0]*qx + e1[1]*qy + e1[2]*qz) * inv_det
        if t < 1e-6:
            return None
        return t

    def _pick_face(self, mx: float, my: float): #vers 1
        """Return index of the closest triangle hit by the ray through
        (mx,my), or None. Picks the nearest intersection along the ray
        (i.e. respects depth — front-most triangle wins)."""
        ray = self._pick_ray(mx, my)
        if ray is None or not self._vertices or not self._triangles:
            return None
        origin, direction = ray
        verts = self._vertices
        best_i, best_t = None, None
        for i, tri in enumerate(self._triangles):
            a, b, c = tri[0], tri[1], tri[2]
            try:
                v0, v1, v2 = verts[a], verts[b], verts[c]
            except IndexError:
                continue
            t = self._ray_triangle_intersect(origin, direction, v0, v1, v2)
            if t is not None and (best_t is None or t < best_t):
                best_i, best_t = i, t
        return best_i

    def _selected_set_for_mode(self, mode=None): #vers 1
        """Return the live selection set for the given (or current) mode."""
        mode = mode or getattr(self, '_select_mode', 'object')
        if mode == 'vertex':
            return self._selected_verts
        if mode == 'edge':
            return self._selected_edges
        return self._selected_faces   # 'face' and 'poly' share one set

    def _apply_selection_click(self, mode, key, modifiers): #vers 1
        """Apply a single click selection. Ctrl+click toggles the item;
        Shift+click adds without replacing; plain click replaces selection."""
        sel = self._selected_set_for_mode(mode)
        if modifiers & Qt.KeyboardModifier.ControlModifier:
            if key in sel:
                sel.discard(key)
            else:
                sel.add(key)
        elif modifiers & Qt.KeyboardModifier.ShiftModifier:
            sel.add(key)
        else:
            sel.clear()
            sel.add(key)
        self._notify_selection_changed()

    def toggle_snap_target(self, target: str): #vers 1
        """Flip one snap target on/off (independently toggleable, not
        single-select - matches 3ds Max's snap ribbon where Vertex+Endpoint+
        Midpoint etc. can all be active together). No-op for unknown keys
        rather than raising, since this is called from button clicks."""
        if target in self._snap_targets:
            self._snap_targets[target] = not self._snap_targets[target]

    def toggle_snap_axis_constraint(self): #vers 1
        self._snap_axis_constraint = not self._snap_axis_constraint

    def _get_selection_count(self): #vers 1
        """Count of currently selected items in the active sub-object mode.
        Mirrors COL3DViewport's identically-named method - see note in
        toolbar_layout_manager.py session about consolidating the two
        viewport classes' duplicated selection logic into methods/."""
        mode = getattr(self, '_select_mode', 'face')
        if mode == 'vertex':
            return len(self._selected_verts)
        if mode == 'edge':
            return len(self._selected_edges)
        return len(self._selected_faces)   # 'face' and 'poly' both live here

    def _notify_selection_changed(self): #vers 3
        """Tell the parent ModelWorkshop panel the selection set changed,
        so it can refresh the 'N Vertices/Edges/Faces/Polygons Selected'
        label. Uses _workshop_ref (set at construction in model_workshop.py)
        rather than walking the Qt parent chain - more reliable since this
        widget is set up with a direct back-reference already."""
        ws = getattr(self, '_workshop_ref', None)
        if ws is not None and hasattr(ws, '_update_selection_count_label'):
            ws._update_selection_count_label()
        if ws is not None and hasattr(ws, '_sync_selection_to_other_viewports'):
            ws._sync_selection_to_other_viewports(self)

    def initializeGL(self): #vers 2
        if not OPENGL_AVAILABLE: return
        bg = self._get_bg_color()
        glClearColor(bg.redF(), bg.greenF(), bg.blueF(), 1.0)
        glEnable(GL_DEPTH_TEST)
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
        self._setup_lighting()

    def _setup_lighting(self): #vers 6
        if not OPENGL_AVAILABLE: return
        import ctypes

        _GL_LIGHTING             = 0x0B50
        _GL_LIGHT0               = 0x4000
        _GL_POSITION             = 0x1203
        _GL_AMBIENT              = 0x1200
        _GL_DIFFUSE              = 0x1201
        _GL_SPECULAR             = 0x1202
        _GL_COLOR_MATERIAL       = 0x0B57
        _GL_FRONT_AND_BACK       = 0x0408
        _GL_AMBIENT_AND_DIFFUSE  = 0x1602

        _libGL = None
        for _name in ('libGL.so.1', 'libGL.so', '/usr/lib/libGL.so.1',
                      '/usr/lib/libGL.so', '/usr/lib64/libGL.so.1',
                      'libOpenGL.so.0', '/usr/lib/x86_64-linux-gnu/libGL.so.1'):
            try:
                _libGL = ctypes.CDLL(_name)
                break
            except OSError:
                _libGL = None

        if _libGL is None:
            import glob
            for _p in glob.glob('/usr/lib*/**/libGL.so*', recursive=True):
                try:
                    _libGL = ctypes.CDLL(_p)
                    break
                except OSError:
                    _libGL = None

        if _libGL is None:
            print("[DFFViewport] libGL not found — lighting disabled")
            return

        _f4 = ctypes.c_float * 4
        _libGL.glEnable(_GL_LIGHTING)
        _libGL.glEnable(_GL_LIGHT0)
        ld = self._light_dir
        a, d = self._ambient, self._diffuse
        tr, tg, tb = getattr(self, '_ambient_tint', (1.0, 1.0, 1.0))
        _libGL.glLightfv(_GL_LIGHT0, _GL_POSITION, _f4(ld[0], ld[1], ld[2], ld[3]))
        _libGL.glLightfv(_GL_LIGHT0, _GL_AMBIENT,  _f4(a * tr, a * tg, a * tb, 1.0))
        _libGL.glLightfv(_GL_LIGHT0, _GL_DIFFUSE,  _f4(d * tr, d * tg, d * tb, 1.0))
        _libGL.glLightfv(_GL_LIGHT0, _GL_SPECULAR, _f4(0.3, 0.3, 0.3, 1.0))
        _libGL.glEnable(_GL_COLOR_MATERIAL)
        _libGL.glColorMaterial(_GL_FRONT_AND_BACK, _GL_AMBIENT_AND_DIFFUSE)

    def resizeGL(self, w, h): #vers 2
        if not OPENGL_AVAILABLE: return
        glViewport(0, 0, max(1, w), max(1, h))
        glMatrixMode(GL_PROJECTION); glLoadIdentity()
        aspect = max(1, w) / max(1, h)
        if self._projection == 'ortho':
            half_h = max(0.01, self._dist * 0.5)
            glOrtho(-half_h*aspect, half_h*aspect, -half_h, half_h, -100000.0, 100000.0)
        else:
            gluPerspective(45.0, aspect, 0.01, 100000.0)
        glMatrixMode(GL_MODELVIEW)
        self._label_widget.move(4, 2)

    def paintGL(self): #vers 7
        if not OPENGL_AVAILABLE: return
        bg = self._get_bg_color()
        glClearColor(bg.redF(), bg.greenF(), bg.blueF(), 1.0)
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
        glLoadIdentity()
        gluLookAt(0, 0, self._dist, 0, 0, 0, 0, 1, 0)
        glRotatef(-self._pitch, 1, 0, 0)
        glRotatef(self._yaw, 0, 0, 1)
        # Sky drawn here - after yaw/pitch rotate the scene, before pan
        # translates it (Aug 20 2026, per Keith: "the images show the
        # rotation, but the sky doesn't pane" [pan]). The old version
        # drew a fixed, screen-space orthographic overlay before any
        # camera transform at all, so it never rotated with yaw/pitch
        # the way a real sky visibly would - same flat gradient no
        # matter which way the camera was actually facing. Now a real,
        # world-space box sky (4 large side quads, same real gradient
        # colours) that rotates along with yaw/pitch just like any
        # other object in the scene, but sits before the pan
        # translate so scrolling/panning the map doesn't drag the sky
        # along with it the way a real, infinitely-distant sky
        # wouldn't be affected by moving around on the ground.
        if self._skybox_path:
            self._draw_skybox()
        elif self._sky_gradient_top and self._sky_gradient_bot:
            self._draw_sky_gradient()
        glTranslatef(self._pan_x, self._pan_y, 0)
        if self._backface_cull:
            glEnable(GL_CULL_FACE); glCullFace(GL_BACK)
        else:
            glDisable(GL_CULL_FACE)
        self._setup_lighting()
        has_world = bool(getattr(self, '_world_instances', None))
        has_geoms = bool(getattr(self, '_all_geoms', None))
        has_verts = bool(self._vertices)
        if has_world:
            if self._show_radar_tex_layer:
                self._draw_radar_tex_layer()
            self._draw_world_instances()
            self._draw_2dfx_lights()
            if self.show_paths:
                self._draw_paths()
            # Cleared once per full render pass, right before either
            # box type might register into it (Aug 19 2026, for box-
            # corner resizing) - clearing inside _draw_cull_boxes or
            # _draw_zone_boxes individually would wipe out whichever
            # box type's own entries got registered first when the
            # other one's draw call ran right after it.
            self._pickable_box_corners = {}
            if self.show_cull_boxes:
                self._draw_cull_boxes()
            if self.show_zone_boxes:
                self._draw_zone_boxes()
            if self.show_occl_boxes:
                self._draw_occl_boxes()
            if self.show_tracks:
                self._draw_tracks()
            if self.show_sa_nodes:
                self._draw_sa_nodes()
            if self.show_auzo_zones:
                self._draw_auzo_zones()
            if self.show_water:
                self._draw_water_shapes()
                self._draw_waterpro_water()
            if getattr(self, '_hovered_instance_idx', None) is not None:
                self._draw_hover_highlight()
            if getattr(self, '_lod_test_center', None) is not None:
                self._draw_lod_test_circle()
            if self._show_grid: self._draw_grid()
            self._draw_axes()
            return
        if not has_geoms and not has_verts:
            if self._show_grid: self._draw_grid()
            self._draw_axes()
            return
        if has_geoms:
            self._draw_assembly()
        elif has_verts:
            if   self._mode == 'wireframe': self._draw_wireframe()
            elif self._mode == 'solid':     self._draw_solid()
            elif self._mode == 'semi_solid': self._draw_solid(alpha_multiplier=0.5)
            elif self._mode == 'textured':  self._draw_textured()
            self._draw_selection_overlay()
        if self._show_grid: self._draw_grid()
        self._draw_axes()

    def _draw_paths(self): #vers 4
        """Draw every real path link (red by default, per Keith: "I
        was expecting red lines and nodes") plus a small marker at
        each unique node position - line colour/thickness and node
        size/colour all configurable via Settings > Render (Aug 16
        2026, per Keith: "I like the path colors as a default but
        under rander in settings, line thinkness, and node circle
        size, and color change option" - node colour was previously
        fixed/unconfigurable, "Keith only asked for the line colour
        to be adjustable" no longer holds now that node colour was
        explicitly requested too).

        Draws self._path_segments (a flat list of ((x1,y1,z1),
        (x2,y2,z2)) edge pairs, already resolved to the real per-node
        Next-index graph by map_workshop.py's conversion step - see
        that method's own docstring for the full "why raw file order
        was wrong" story) as independent GL_LINES, not a connected
        polyline - real path links routinely aren't one continuous
        sequence (a node's Next can point anywhere else in its own
        12-node group, and separate groups only connect where an
        External node's position exactly matches another group's),
        so nothing here should assume adjacency between one segment
        and the next.

        Thinner/smaller/semi-transparent (Aug 16 2026, per Keith,
        comparing against MooMapper's own path overlay: "notice how
        it blends in with the map") - was a flat 2px opaque line with
        6px opaque dots, closer to a bold HUD overlay than something
        that reads as part of the world. Depth test is still left
        disabled here, same as before, not changed alongside this -
        genuinely occluding paths behind buildings/terrain (so they
        sit "in" the world rather than always drawing on top) is a
        bigger, riskier change on real data (path Z values might not
        track terrain height closely enough everywhere to avoid
        making paths patchily invisible instead of just less bold) -
        worth trying separately once this smaller change is
        confirmed, not bundled into the same one."""
        if not OPENGL_AVAILABLE or not self._path_segments: return
        glDisable(GL_LIGHTING)
        glDisable(GL_DEPTH_TEST)   # paths read clearer drawn on top, same as 2DFX lights
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
        r, g, b = self._path_line_color
        glLineWidth(self._path_line_thickness)
        glColor4f(r, g, b, 0.75)
        glBegin(GL_LINES)
        for (x1, y1, z1), (x2, y2, z2) in self._path_segments:
            glVertex3f(x1, y1, z1)
            glVertex3f(x2, y2, z2)
        glEnd()
        nr, ng, nb = self._path_node_color
        glColor4f(nr, ng, nb, 0.75)
        glPointSize(self._path_node_size)
        # Round points instead of the default squares (Aug 16 2026,
        # per Keith: "we could make the path nodes round circles,
        # makes it easy to click on them" - a real, standard OpenGL
        # feature for this, not a custom shape: GL_POINT_SMOOTH
        # anti-aliases each point into a circle rather than leaving
        # its square corners visible. GL_NICEST asks for the best-
        # quality rounding available, since these are large enough
        # (node_size can go up to 30px, per Settings > Render) that
        # visible squared-off corners would be obvious at that size.
        # This is the visual half of Keith's own stated reason
        # ("easy to click on them") - actual click-to-select/drag
        # interaction on a node is separate, unbuilt work (same class
        # as the still-open corner-sphere-dragging TODO), not
        # included here.
        glEnable(GL_POINT_SMOOTH)
        glHint(GL_POINT_SMOOTH_HINT, GL_NICEST)
        glBegin(GL_POINTS)
        seen = set()
        for a, b_pt in self._path_segments:
            for pt in (a, b_pt):
                if pt not in seen:
                    seen.add(pt)
                    glVertex3f(*pt)
        glEnd()
        glDisable(GL_POINT_SMOOTH)
        glDisable(GL_BLEND)
        glEnable(GL_DEPTH_TEST)
        glEnable(GL_LIGHTING)

        # Highlight the currently held/dragged node, if any (Aug 18
        # 2026, per Keith: "Clicking nodes brings up nothing?" - the
        # real gap was that a successful pick never drew anything
        # differently at all, so a click that didn't also move the
        # mouse produced no visible change even though picking up the
        # node had actually worked). Drawn as its own separate,
        # larger, white point on top of everything else above, so
        # picking a node up is visible the instant it happens, before
        # any drag movement.
        held_pos = getattr(self, '_dragging_path_node_current_pos', None)
        if held_pos is not None and OPENGL_AVAILABLE:
            glDisable(GL_LIGHTING)
            glDisable(GL_DEPTH_TEST)
            glEnable(GL_BLEND)
            glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
            glEnable(GL_POINT_SMOOTH)
            glHint(GL_POINT_SMOOTH_HINT, GL_NICEST)
            glColor4f(1.0, 1.0, 1.0, 0.95)
            glPointSize(self._path_node_size * 1.8)
            glBegin(GL_POINTS)
            glVertex3f(*held_pos)
            glEnd()
            glDisable(GL_POINT_SMOOTH)
            glDisable(GL_BLEND)
            glEnable(GL_DEPTH_TEST)
            glEnable(GL_LIGHTING)

    def _draw_cull_boxes(self): #vers 3
        """Draw every loaded cull zone as a ghosted (semi-transparent
        filled + outlined) box, corner-to-corner (Aug 16 2026, per
        Keith: "instead of wireframe boxes, we go for ghosted, see
        through boxes, like the semi solid" - was plain wireframe
        until this request). Uses the box's own two real corner
        points directly (x1,y1,z1)-(x2,y2,z2) - the older MapViewport
        class's own _draw_cull_boxes assumed a center+width/height
        shape (matching the wrong 7-field parse that was fixed
        alongside the original wireframe version), this draws the
        actual documented box, no assumption needed."""
        self._draw_ghosted_boxes(self._cull_boxes, self._cull_box_color)

    def _draw_zone_boxes(self): #vers 4
        """Draw every loaded map zone, style selectable via self.
        _zone_render_style (Aug 16 2026, per Keith: "in zons, the
        render dropdown could show, Zon - Ghosted, Zon - Wireframe,
        Zon - translucent") - 'wireframe' draws edges only (no fill,
        no corner spheres skipped either - see _draw_box_wireframe_
        from_corners); 'ghosted' (default) and 'translucent' both go
        through the same shared _draw_ghosted_box_from_corners, just
        with different fill_alpha/draw_outline - translucent is more
        see-through and has no outline at all, letting the corner
        spheres alone mark the box's extent.

        Per-box unique colouring (Aug 19 2026, per Keith: "add colour
        zone boxes") applies here too, same _palette_color_for_index
        lookup and same axis-colored-takes-priority rule as cull's
        own _draw_ghosted_boxes - color set per-box inside each loop
        below rather than once outside it, so the wireframe style
        (which sets its own GL colour explicitly) can vary per box
        too, not just the filled styles."""
        if not OPENGL_AVAILABLE or not self._zone_boxes: return
        style = self._zone_render_style
        glDisable(GL_LIGHTING)
        glDisable(GL_DEPTH_TEST)
        r, g, b = self._zone_box_color
        axis_colored = getattr(self, '_box_axis_colors', False)
        unique_colors = getattr(self, '_box_unique_colors', False)
        owners = getattr(self, '_zone_box_owners', [])
        if style == 'wireframe':
            glLineWidth(1.5)
            for i, (x1, y1, z1, x2, y2, z2) in enumerate(self._zone_boxes):
                box_r, box_g, box_b = self._palette_color_for_index(i) \
                    if (unique_colors and not axis_colored) else (r, g, b)
                glColor3f(box_r, box_g, box_b)
                corners_xy = [(x1, y1), (x2, y1), (x2, y2), (x1, y2)]
                self._draw_box_wireframe_from_corners(corners_xy, z1, z2)
                self._draw_box_corner_spheres(corners_xy, z1, z2, box_r, box_g, box_b)
                if i < len(owners):
                    self._register_pickable_box_corners('zone', i, corners_xy, z1, z2, owners[i])
        else:
            glEnable(GL_BLEND)
            glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
            fill_alpha = 0.16 if style == 'translucent' else 0.32
            draw_outline = style != 'translucent'
            for i, (x1, y1, z1, x2, y2, z2) in enumerate(self._zone_boxes):
                box_r, box_g, box_b = self._palette_color_for_index(i) \
                    if (unique_colors and not axis_colored) else (r, g, b)
                corners_xy = [(x1, y1), (x2, y1), (x2, y2), (x1, y2)]
                self._draw_ghosted_box_from_corners(
                    corners_xy, z1, z2, box_r, box_g, box_b,
                    fill_alpha=fill_alpha, draw_outline=draw_outline,
                    axis_colored=axis_colored)
                self._draw_box_corner_spheres(corners_xy, z1, z2, box_r, box_g, box_b)
                if i < len(owners):
                    self._register_pickable_box_corners('zone', i, corners_xy, z1, z2, owners[i])
            glDisable(GL_BLEND)
        glEnable(GL_DEPTH_TEST)
        glEnable(GL_LIGHTING)

    def _register_pickable_box_corners(self, box_type, box_index, corners_xy, z1, z2, box_ref): #vers 1
        """Register one box's own 8 corners as pickable for resizing
        (Aug 19 2026, for box-corner resizing - see the fuller
        explanation where self._pickable_box_corners is first
        declared in __init__). Shared by cull's own _draw_ghosted_
        boxes and both of zone's own render-style branches (wireframe
        and ghosted/translucent) rather than duplicating the same
        opposite-corner bookkeeping three times over."""
        if box_ref is None:
            return
        for ci, (cx, cy) in enumerate(corners_xy):
            ox, oy = corners_xy[(ci + 2) % 4]   # diagonally opposite XY corner
            for cz, oz in ((z1, z2), (z2, z1)):
                key = (box_type, box_index, ci, cz)
                self._pickable_box_corners[key] = {
                    'pos': (cx, cy, cz),
                    'opposite': (ox, oy, oz),
                    'box_type': box_type,
                    'box_ref': box_ref,
                }

    def _draw_ghosted_boxes(self, boxes, color): #vers 2
        """Shared ghosted axis-aligned-box drawing helper for cull
        (Aug 16 2026, per Keith: "instead of wireframe boxes, we go
        for ghosted, see through boxes, like the semi solid" -
        replaces the earlier _draw_wireframe_boxes; zone moved to its
        own _draw_zone_boxes once it gained selectable render styles)
        - boxes is a list of (x1,y1,z1,x2,y2,z2) corner-pair tuples,
        color an (r,g,b) 0-1 tuple. Derives the 4 XY corners from the
        two opposite points and hands off to _draw_ghosted_box_from_
        corners, the same per-box fill+outline routine _draw_occl_
        boxes' rotated boxes use - only the corner computation
        differs between an axis-aligned box and a rotated one, not
        how it's actually drawn once corners exist. Also draws a
        small solid sphere at each of the box's 8 corners (Aug 16
        2026, per Keith: "the boxes we see need little solid spheres
        on each corner so you can move the 6 sides, bigger, shorter,
        longer, deeper, higher" - visual handles only for now, not
        yet draggable/interactive; that's a bigger follow-up matching
        the project's already-open "gizmo-based free object movement"
        TODO, same class of feature)."""
        if not OPENGL_AVAILABLE or not boxes: return
        glDisable(GL_LIGHTING)
        glDisable(GL_DEPTH_TEST)
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
        r, g, b = color
        axis_colored = getattr(self, '_box_axis_colors', False)
        unique_colors = getattr(self, '_box_unique_colors', False)
        owners = getattr(self, '_cull_box_owners', [])
        for i, (x1, y1, z1, x2, y2, z2) in enumerate(boxes):
            box_r, box_g, box_b = self._palette_color_for_index(i) \
                if (unique_colors and not axis_colored) else (r, g, b)
            corners_xy = [(x1, y1), (x2, y1), (x2, y2), (x1, y2)]
            self._draw_ghosted_box_from_corners(corners_xy, z1, z2, box_r, box_g, box_b,
                axis_colored=axis_colored)
            self._draw_box_corner_spheres(corners_xy, z1, z2, box_r, box_g, box_b)
            # Register each of this box's 8 corners as pickable (Aug
            # 19 2026, for box-corner resizing) - fails safe if the
            # owners list is missing or shorter than boxes (that box
            # just isn't resizable this refresh), not with an
            # IndexError.
            if i < len(owners):
                self._register_pickable_box_corners('cull', i, corners_xy, z1, z2, owners[i])
        glDisable(GL_BLEND)
        glEnable(GL_DEPTH_TEST)
        glEnable(GL_LIGHTING)

    def _draw_ghosted_box_from_corners(self, corners_xy, z1, z2, r, g, b,
                                        fill_alpha=0.32, edge_alpha=0.85,
                                        draw_outline=True, axis_colored=False): #vers 3
        """Draw one ghosted (semi-transparent filled faces + an
        optional, more opaque wireframe outline for definition) box
        from 4 already-computed (x,y) corner points in loop order and
        a z1/z2 extrusion range (Aug 16 2026, per Keith: "instead of
        wireframe boxes, we go for ghosted, see through boxes, like
        the semi solid") - matches the existing collision Semi-Solid
        render mode's own convention (_draw_solid's alpha_multiplier
        path: filled alpha-blended triangles plus a subtle darker
        edge pass), just for a simple box instead of arbitrary mesh
        triangles. draw_outline=False (Aug 16 2026, for Zon -
        Translucent) skips the edge pass entirely - just the filled
        faces, letting the corner spheres alone mark the box's shape.
        Shared by the axis-aligned cull/zone case (_draw_ghosted_
        boxes/_draw_zone_boxes) and the rotated occlusion case
        (_draw_occl_boxes) - only the corner computation differs
        between them, not this actual drawing routine. Caller is
        responsible for glEnable(GL_BLEND)/blend func and disabling
        lighting/depth test around a whole batch, not repeated per
        box here.

        axis_colored=True (Aug 19 2026, per Keith's colour spec: "box
        sides colour Z sides blue, Y sides green, X sides red", then
        corrected right back: "X=red/Y=green, swap them around to
        X-Green, Y-Red" - so X=green, Y=red, Z=blue is the actual,
        final intended scheme) - overrides the passed (r,g,b) with a
        fixed per-face colour instead: the top/bottom caps (the box's
        own Z extent) are blue, and of the 4 side faces, the two
        connecting corners[0]-corners[1] and corners[2]-corners[3]
        are red (these vary in X while Y stays constant along each -
        i.e. their face normal points along Y), the other two
        (corners[1]-corners[2], corners[3]-corners[0]) are green
        (X-constant, normal along X). This holds correctly for
        rotated boxes (occlusion) too, not just axis-aligned ones
        (cull/zone) - verified with a standalone rotation test before
        trusting it: corners_xy is always built by rotating the SAME
        four local corner offsets in the SAME order, so which pair of
        indices is the "local X face" vs "local Y face" is fixed by
        construction and doesn't depend on the box's current rotation
        in world space, even though the actual (x,y) values do."""
        def _face_color(default_alpha, side_index=None):
            if not axis_colored:
                return (r, g, b, default_alpha)
            if side_index is None:
                return (0.2, 0.4, 1.0, default_alpha)   # Z (top/bottom caps) - blue
            return (1.0, 0.25, 0.25, default_alpha) if side_index % 2 == 0 \
                else (0.25, 1.0, 0.25, default_alpha)    # Y (red) / X (green)

        cr, cg, cb, ca = _face_color(fill_alpha)
        glColor4f(cr, cg, cb, ca)
        glBegin(GL_QUADS)
        for cx, cy in corners_xy:
            glVertex3f(cx, cy, z1)
        glEnd()
        glBegin(GL_QUADS)
        for cx, cy in corners_xy:
            glVertex3f(cx, cy, z2)
        glEnd()
        for i in range(4):
            cx0, cy0 = corners_xy[i]
            cx1, cy1 = corners_xy[(i + 1) % 4]
            fr, fg, fb, fa = _face_color(fill_alpha, side_index=i)
            glColor4f(fr, fg, fb, fa)
            glBegin(GL_QUADS)
            glVertex3f(cx0, cy0, z1); glVertex3f(cx1, cy1, z1)
            glVertex3f(cx1, cy1, z2); glVertex3f(cx0, cy0, z2)
            glEnd()
        if not draw_outline:
            return
        glColor4f(r, g, b, edge_alpha)
        glLineWidth(1.2)
        glBegin(GL_LINE_LOOP)
        for cx, cy in corners_xy:
            glVertex3f(cx, cy, z1)
        glEnd()
        glBegin(GL_LINE_LOOP)
        for cx, cy in corners_xy:
            glVertex3f(cx, cy, z2)
        glEnd()
        glBegin(GL_LINES)
        for cx, cy in corners_xy:
            glVertex3f(cx, cy, z1); glVertex3f(cx, cy, z2)
        glEnd()

    def _draw_box_wireframe_from_corners(self, corners_xy, z1, z2): #vers 1
        """Edges-only box drawing from 4 already-computed (x,y)
        corner points and a z1/z2 extrusion range (Aug 16 2026, for
        Zon - Wireframe) - the plain wireframe look every box type
        had before the Semi-Solid-style ghosted rework; kept around
        as an explicit style choice for zones specifically, per
        Keith's own request, rather than removed entirely. Caller
        sets colour/line width beforehand - this only emits
        vertices."""
        glBegin(GL_LINE_LOOP)
        for cx, cy in corners_xy:
            glVertex3f(cx, cy, z1)
        glEnd()
        glBegin(GL_LINE_LOOP)
        for cx, cy in corners_xy:
            glVertex3f(cx, cy, z2)
        glEnd()
        glBegin(GL_LINES)
        for cx, cy in corners_xy:
            glVertex3f(cx, cy, z1); glVertex3f(cx, cy, z2)
        glEnd()

    def _draw_box_corner_spheres(self, corners_xy, z1, z2, r, g, b, radius=0.35): #vers 1
        """Draw a small solid sphere at each of a box's 8 corners
        (Aug 16 2026, per Keith: "the boxes we see need little solid
        spheres on each corner so you can move the 6 sides, bigger,
        shorter, longer, deeper, higher" - visual handles only for
        now, not yet clickable/draggable; actually moving a corner to
        resize the box is a separate, larger follow-up matching the
        project's already-open "gizmo-based free object movement"
        TODO - same class of feature: mouse picking, drag math, and
        live data mutation, none of which exist yet for anything in
        this viewport).

        Uses GLU's gluSphere (GLU already imported wildcard at module
        level, alongside GL) at a deliberately low poly count (6
        slices, 4 stacks) - a scene can easily have hundreds of boxes
        visible at once (8 corners each), and immediate-mode gluSphere
        calls aren't free; kept cheap per-corner rather than smooth,
        since these are meant to read as small handles, not as
        rendered objects in their own right. A single QuadricObj is
        created once and reused (lazily, on first use) rather than
        recreated every call."""
        quadric = getattr(self, '_corner_sphere_quadric', None)
        if quadric is None:
            quadric = gluNewQuadric()
            self._corner_sphere_quadric = quadric
        glColor4f(r, g, b, 1.0)
        for cx, cy in corners_xy:
            for cz in (z1, z2):
                glPushMatrix()
                glTranslatef(cx, cy, cz)
                gluSphere(quadric, radius, 6, 4)
                glPopMatrix()

    def _draw_occl_boxes(self): #vers 3
        """Draw every loaded occlusion zone as a ghosted (semi-
        transparent filled + outlined) box (Aug 16 2026, per Keith:
        "instead of wireframe boxes, we go for ghosted, see through
        boxes, like the semi solid" - was plain wireframe until this
        request). Unlike cull/zone boxes, an occlusion zone can be
        ROTATED around its own vertical (Z) axis - computes all 4 XY
        corners explicitly: half-extents from width_x/width_y,
        rotated by `rotation` around (mid_x, mid_y), extruded from
        bottom_z to bottom_z+height - then hands off to the same
        _draw_ghosted_box_from_corners the axis-aligned cull/zone
        boxes use, since once corners exist the actual fill+outline
        drawing is identical regardless of rotation. Also draws
        corner-sphere handles, same as cull/zone (see _draw_box_
        corner_spheres) - the spheres themselves aren't rotated
        (they're just points), only the box's corner positions are.

        Rotation is treated as degrees, standard 2D rotation matrix
        around Z - matches the field's evident purpose (turning an
        axis-aligned box to match a rotated building) and its real
        value range in Keith's data (up to ~180, consistent with
        degrees, not radians), but the exact sign/direction
        convention GTA itself uses (clockwise vs counter-clockwise)
        is NOT independently confirmed against real in-game
        behaviour - only the field values and their parsing are
        verified, this rendering interpretation is a reasonable but
        unverified best guess, same honesty standard as the GTA III
        IDE path coordinates' unconfirmed scale factor."""
        if not OPENGL_AVAILABLE or not self._occl_boxes: return
        glDisable(GL_LIGHTING)
        glDisable(GL_DEPTH_TEST)
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
        r, g, b = self._occl_box_color
        axis_colored = getattr(self, '_box_axis_colors', False)
        unique_colors = getattr(self, '_box_unique_colors', False)
        for i, (mid_x, mid_y, bottom_z, width_x, width_y, height, rotation) in enumerate(self._occl_boxes):
            box_r, box_g, box_b = self._palette_color_for_index(i) \
                if (unique_colors and not axis_colored) else (r, g, b)
            hw, hh = width_x / 2.0, width_y / 2.0
            rad = math.radians(rotation)
            cos_r, sin_r = math.cos(rad), math.sin(rad)
            corners_xy = []
            for dx, dy in ((-hw, -hh), (hw, -hh), (hw, hh), (-hw, hh)):
                rx = dx * cos_r - dy * sin_r
                ry = dx * sin_r + dy * cos_r
                corners_xy.append((mid_x + rx, mid_y + ry))
            z1, z2 = bottom_z, bottom_z + height
            self._draw_ghosted_box_from_corners(corners_xy, z1, z2, box_r, box_g, box_b,
                axis_colored=axis_colored)
            self._draw_box_corner_spheres(corners_xy, z1, z2, box_r, box_g, box_b)
        glDisable(GL_BLEND)
        glEnable(GL_DEPTH_TEST)
        glEnable(GL_LIGHTING)

    def _nice_grid_step(self, raw_step): #vers 1
        """Snap to the nearest value in a 1-2-5-10-20-50... sequence
        instead of every raw integer (Aug 20 2026, per Keith: "its
        not really locked, it wiggles around when zooming in or out,
        making is jitter") - a plain int(raw_step) changes by 1 on
        almost every frame during continuous zoom, visibly
        repositioning every grid line each time; snapping to a small,
        widely-spaced set of round steps means the grid only actually
        changes size a handful of times across a full zoom range,
        not continuously."""
        raw_step = max(1.0, raw_step)
        import math
        magnitude = 10 ** math.floor(math.log10(raw_step))
        for mult in (1, 2, 5, 10):
            candidate = magnitude * mult
            if raw_step <= candidate * 1.3:
                return int(candidate)
        return int(magnitude * 10)

    def _draw_grid(self): #vers 3
        """Draw the viewport's own reference grid, in whichever real
        visual style self._grid_type currently selects (Aug 20 2026,
        per Keith: "can we have an option for grid type squares, grid
        with blue square inside, marching ants lines, just dots, and
        switch grid off completly" - "off" is the pre-existing self.
        _show_grid=False, checked by every caller of this method
        already; this method itself only ever runs when the grid is
        genuinely on, dispatching purely on which of the other 4 real
        styles is active). Shared step/rng extent computation, same
        real values the original single-style version already used -
        every style below covers the exact same real grid area, only
        how each line/cell/point is actually drawn differs.

        Real bug fixed in this same pass (Aug 20 2026, per Keith:
        "the other thing I noticed about the original grid, is it
        didn't cover the whole area, bigger maps overlapped it
        massively... not just grid pattern size, but grid area size,
        or limitless?") - the grid used to always sit fixed at world
        origin (0,0), completely independent of self._pan_x/_pan_y
        (the camera's own real pan position) - so panning away from
        the origin at all, on ANY map (not just a genuinely large
        one), left the grid behind entirely rather than following the
        view; the "bigger maps overlapped it" symptom was really this
        same bug, just more visible on a map large enough that normal
        navigation moves the camera far from the origin as a matter
        of course.

        Real answer to Keith's own "grid area size, or limitless?"
        question: limitless, not a fixed size - genuinely camera-
        relative now rather than tied to any assumed map size (which
        would need knowing the real map bounds in the first place,
        the exact thing Keith said he can't calculate) - the grid's
        own centre re-computes to the camera's real current focal
        point every frame, snapped to the nearest real step-aligned
        position first (self._pan_x/_pan_y are real floats, an
        unsnapped centre would make the grid's own lines visibly
        drift/jitter as the camera moves by sub-step amounts, rather
        than the fixed, stable world-space reference lines a grid is
        actually supposed to be) - only the visible RANGE of grid
        lines shifts with the camera, the lines' own real world
        positions stay fixed at step-aligned multiples throughout."""
        if not OPENGL_AVAILABLE: return
        glDisable(GL_LIGHTING)
        if self._grid_scale_mode == 'radar_tiles':
            step = max(1, int(self._radar_grid_tile_size))
        elif self._grid_scale_mode == 'fixed':
            step = max(1, int(self._grid_fixed_step))
        else:
            step = self._nice_grid_step(self._dist / self._grid_spacing)
        cell_radius = max(1, self._grid_cell_count // 2)
        if self._grid_scale_mode == 'radar_tiles':
            rng = int(self._radar_grid_half_extent)
        elif self._grid_scale_mode == 'fixed':
            # Fixed cell size shouldn't cap visible range too - scale
            # with zoom distance too so zooming out to see a
            # far-away model still extends the grid to reach it
            # (Aug 20 2026, per Keith: "the grid count doesn't cover
            # the map, you can see the outer ipl models and the
            # little yellow grid in the middle").
            rng = max(step * cell_radius, int(self._dist * 2))
        else:
            rng = step * cell_radius
        # The real world point the camera is currently looking at is
        # (-pan_x, -pan_y) - the scene itself is translated by (pan_x,
        # pan_y) before the fixed camera views it (the same real
        # relationship already verified numerically for capture_
        # radar_tile's own camera math earlier this session).
        if self._grid_scale_mode == 'radar_tiles':
            # Anchored to the real, fixed tile origin (0,0 - matching
            # RADAR_GRID_PRESETS' own center_x/center_y default), not
            # camera-relative - so cell boundaries genuinely line up
            # with the actual exported radar tiles, not wherever the
            # camera happens to be looking.
            cx = 0
            cy = 0
        else:
            cx = round(-self._pan_x / step) * step
            cy = round(-self._pan_y / step) * step
        grid_type = getattr(self, '_grid_type', 'lines')
        if grid_type == 'none':
            glEnable(GL_LIGHTING)
            return
        # Real fix (Aug 20 2026, per Keith: "grid disappears when IPL
        # models are loaded") - the grid was never disabling GL_DEPTH_
        # TEST the way every other overlay (_draw_paths, cull/zone
        # boxes, 2DFX lights) already does. Before any world instances
        # load, nothing's in the depth buffer to occlude the grid's
        # own Z=0 lines - once ground-level building/road geometry
        # draws before it (paintGL's own real order), depth testing
        # correctly hides the grid wherever that geometry sits at or
        # in front of it, same as any other occluded overlay would be.
        # Reference overlays are supposed to draw on top regardless,
        # matching the same real reasoning _draw_paths' own docstring
        # already gives for doing this.
        was_depth_test = glIsEnabled(GL_DEPTH_TEST)
        glDisable(GL_DEPTH_TEST)
        if grid_type == 'squares':
            self._draw_grid_squares(step, rng, cx, cy)
        elif grid_type == 'dashed':
            self._draw_grid_dashed(step, rng, cx, cy)
        elif grid_type == 'dots':
            self._draw_grid_dots(step, rng, cx, cy)
        elif grid_type == 'honeycomb':
            self._draw_grid_honeycomb(step, rng, cx, cy)
        elif grid_type == 'honeycomb_dashed':
            self._draw_grid_honeycomb(step, rng, cx, cy, dashed=True)
        else:
            self._draw_grid_lines(step, rng, cx, cy)
        if was_depth_test:
            glEnable(GL_DEPTH_TEST)
        glEnable(GL_LIGHTING)

    def _draw_grid_lines(self, step, rng, cx=0, cy=0): #vers 3
        """'lines' grid style - the original, only style this feature
        ever had before Keith's own request for real alternatives:
        open grid lines, no fill, no dashing. cx/cy: the real, step-
        aligned world centre this grid's own visible range is
        currently built around (see _draw_grid's own docstring for
        the full "why camera-relative, not fixed" reasoning).

        Line anti-aliasing added here (Aug 20 2026, per Keith: "we
        need some kind of anti-alising, far away lines doesn't appear
        to flicker?") - a thin, unsmoothed line far from the camera
        covers less than one pixel's worth of screen space per grid
        step, so as the camera moves even slightly, which pixels the
        line rasterizes to flips on and off between frames - the real
        cause of the reported flicker. GL_LINE_SMOOTH genuinely
        smooths line edges via real, correct alpha coverage rather
        than an all-or-nothing pixel test, needing GL_BLEND enabled
        to actually take visual effect (line smoothing without
        blending is a real, well-known no-op) - both saved and
        restored around just this method's own real line-drawing
        work, not left globally on afterward, so this can't silently
        affect any other, unrelated drawing call elsewhere that never
        asked for blending. Applied here specifically (not wrapped
        around the whole _draw_grid dispatch instead) because 'squares'
        style already manages its own separate GL_BLEND window around
        its own fill before ever reaching this method - wrapping here
        instead avoids stepping on that already-correct, separate
        blend toggle.

        The real, more comprehensive fix for the same report is 4x
        MSAA, now enabled at this whole viewport's own QSurfaceFormat
        (see this module's own top-level format setup) - anti-aliases
        every primitive, not just lines. This method's own GL_LINE_
        SMOOTH is a real, additional, line-specific layer on top of
        that, since MSAA sample coverage alone doesn't always fully
        resolve a line that's sub-pixel-thin at a genuine distance."""
        was_blend = glIsEnabled(GL_BLEND)
        glEnable(GL_LINE_SMOOTH)
        glHint(GL_LINE_SMOOTH_HINT, GL_NICEST)
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
        glLineWidth(self._grid_line_size)
        r, g, b = self._grid_line_color
        glColor4f(r / 255, g / 255, b / 255, 0.4)

        # Hide-over-radar-tiles (Aug 20 2026, per Keith: "toggle the
        # grid over radar, see it outside, but not on the radar
        # tiles") - each line is drawn in up to 2 segments, skipping
        # whichever middle portion falls within the radar tex layer's
        # own real, known bounds (a square centred at the world
        # origin, per RADAR_GRID_PRESETS/set_radar_grid_extent) -
        # still drawn in full outside that area. Only takes effect
        # while the radar tex layer is actually on; otherwise there's
        # nothing to hide the grid "over", so the full grid draws as
        # normal regardless of this setting.
        hide_over_radar = self._grid_hide_over_radar_tiles and self._show_radar_tex_layer
        half = self._radar_grid_half_extent if hide_over_radar else 0

        def draw_segment(x1, y1, x2, y2): #vers 1
            glVertex3f(x1, y1, 0); glVertex3f(x2, y2, 0)

        glBegin(GL_LINES)
        for i in range(cx - rng, cx + rng + 1, step):
            if hide_over_radar and -half < i < half:
                if cy - rng < -half:
                    draw_segment(i, cy - rng, i, -half)
                if cy + rng > half:
                    draw_segment(i, half, i, cy + rng)
            else:
                draw_segment(i, cy - rng, i, cy + rng)
        for i in range(cy - rng, cy + rng + 1, step):
            if hide_over_radar and -half < i < half:
                if cx - rng < -half:
                    draw_segment(cx - rng, i, -half, i)
                if cx + rng > half:
                    draw_segment(half, i, cx + rng, i)
            else:
                draw_segment(cx - rng, i, cx + rng, i)
        glEnd()
        glDisable(GL_LINE_SMOOTH)
        if not was_blend:
            glDisable(GL_BLEND)

    def _draw_grid_squares(self, step, rng, cx=0, cy=0): #vers 4
        """'squares' grid style - colour fill (default) or a tiled
        user image/texture, per fill_mode. Outline lines on top are
        now gated on self._grid_show_lines (Aug 20 2026, per Keith:
        "the Hide grid needs to be elsewhere so that I can show the
        texture without the grid") - separate from grid_type='none',
        which hides the whole style (fill included); this only hides
        the line overlay, so squares/texture fill can show cleanly on
        its own."""
        if self._squares_fill_mode == 'texture' and self._squares_texture_path:
            tex_id = self._ensure_squares_texture()
            if tex_id:
                glEnable(GL_TEXTURE_2D)
                glBindTexture(GL_TEXTURE_2D, tex_id)
                glEnable(GL_BLEND)
                glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
                glColor4f(1, 1, 1, self._squares_texture_alpha)
                ts = self._squares_tile_size
                glBegin(GL_QUADS)
                for gy in range(cy - rng, cy + rng, step):
                    for gx in range(cx - rng, cx + rng, step):
                        u0, v0 = gx / ts, gy / ts
                        u1, v1 = (gx + step) / ts, (gy + step) / ts
                        glTexCoord2f(u0, v0); glVertex3f(gx, gy, 0)
                        glTexCoord2f(u1, v0); glVertex3f(gx + step, gy, 0)
                        glTexCoord2f(u1, v1); glVertex3f(gx + step, gy + step, 0)
                        glTexCoord2f(u0, v1); glVertex3f(gx, gy + step, 0)
                glEnd()
                glDisable(GL_BLEND)
                glBindTexture(GL_TEXTURE_2D, 0)
                glDisable(GL_TEXTURE_2D)
                if self._grid_show_lines:
                    self._draw_grid_lines(step, rng, cx, cy)
                return
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
        r, g, b = self._squares_color
        glColor4f(r / 255, g / 255, b / 255, 0.12)
        glBegin(GL_QUADS)
        for gy in range(cy - rng, cy + rng, step):
            for gx in range(cx - rng, cx + rng, step):
                glVertex3f(gx, gy, 0)
                glVertex3f(gx + step, gy, 0)
                glVertex3f(gx + step, gy + step, 0)
                glVertex3f(gx, gy + step, 0)
        glEnd()
        glDisable(GL_BLEND)
        if self._grid_show_lines:
            self._draw_grid_lines(step, rng, cx, cy)

    def _ensure_squares_texture(self): #vers 1
        """Lazily load self._squares_texture_path as a repeating GL
        texture, re-loading only if the path changed since last time."""
        if self._squares_tex_id and self._squares_tex_path_loaded == self._squares_texture_path:
            return self._squares_tex_id
        try:
            from PyQt6.QtGui import QImage
            image = QImage(self._squares_texture_path).convertToFormat(QImage.Format.Format_RGBA8888)
            if image.isNull():
                self._squares_tex_id = False
                return False
            w, h = image.width(), image.height()
            ptr = image.bits(); ptr.setsize(image.sizeInBytes())
            rgba = bytes(ptr)
            if self._squares_tex_id and self._squares_tex_id is not False:
                glDeleteTextures([self._squares_tex_id])
            gl_id = glGenTextures(1)
            glBindTexture(GL_TEXTURE_2D, gl_id)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
            glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, rgba)
            glBindTexture(GL_TEXTURE_2D, 0)
            self._squares_tex_id = gl_id
            self._squares_tex_path_loaded = self._squares_texture_path
        except Exception as e:
            print(f"[DFFViewport] Failed to load grid texture: {e}")
            self._squares_tex_id = False
        return self._squares_tex_id

    def set_squares_fill(self, mode, color=None, path=None, tile_size=None): #vers 1
        """mode: 'color' or 'texture'."""
        self._squares_fill_mode = mode
        if color is not None:
            self._squares_color = color
        if path is not None and path != self._squares_texture_path:
            self._squares_texture_path = path
            self._squares_tex_id = None   # force reload
        if tile_size is not None:
            self._squares_tile_size = tile_size
        self.update()

    def set_squares_texture_alpha(self, alpha): #vers 1
        self._squares_texture_alpha = max(0.0, min(1.0, alpha))
        self.update()

    def set_grid_show_lines(self, show): #vers 1
        """Separate from grid_type='none' (Aug 20 2026, per Keith:
        "the Hide grid needs to be elsewhere so that I can show the
        texture without the grid") - hides only the outline lines
        drawn on top of squares/texture fill, not the fill itself."""
        self._grid_show_lines = bool(show)
        self.update()

    def set_grid_hide_over_radar_tiles(self, hide): #vers 1
        """Suppress grid lines specifically within the radar tex
        layer's own real bounds, while still drawing them outside
        that area (Aug 20 2026, per Keith: "toggle the grid over
        radar, see it outside, but not on the radar tiles"). No
        effect while the radar tex layer itself is off - see _draw_
        grid_lines' own real docstring/logic for how the split is
        computed."""
        self._grid_hide_over_radar_tiles = bool(hide)
        self.update()

    def _ensure_skybox_texture(self): #vers 1
        """Lazily load self._skybox_path as a GL texture."""
        if self._skybox_tex_id and self._skybox_tex_path_loaded == self._skybox_path:
            return self._skybox_tex_id
        try:
            from PyQt6.QtGui import QImage
            image = QImage(self._skybox_path).convertToFormat(QImage.Format.Format_RGBA8888)
            if image.isNull():
                self._skybox_tex_id = False
                return False
            w, h = image.width(), image.height()
            ptr = image.bits(); ptr.setsize(image.sizeInBytes())
            rgba = bytes(ptr)
            if self._skybox_tex_id and self._skybox_tex_id is not False:
                glDeleteTextures([self._skybox_tex_id])
            gl_id = glGenTextures(1)
            glBindTexture(GL_TEXTURE_2D, gl_id)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
            glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, rgba)
            glBindTexture(GL_TEXTURE_2D, 0)
            self._skybox_tex_id = gl_id
            self._skybox_tex_path_loaded = self._skybox_path
        except Exception as e:
            print(f"[DFFViewport] Failed to load skybox texture: {e}")
            self._skybox_tex_id = False
        return self._skybox_tex_id

    def _draw_skybox(self): #vers 2
        """Real, world-space "box sky" - same real technique _draw_
        sky_gradient now uses (Aug 20 2026, per Keith: "the images
        show the rotation, but the sky doesn't pane" [pan]) - this
        used to be a fixed, screen-space orthographic overlay, drawn
        before any camera transform at all, so a skybox image never
        actually rotated with yaw/pitch the way a real sky visibly
        would. The image is mapped once around the 4 side faces in
        sequence (each face gets one quarter of the image's own
        width, wrapping around as the camera yaws), rather than the
        same single frame repeated on all 4 sides - a real skybox
        image is generally authored as a single wraparound panorama,
        not 4 identical copies."""
        if not self._skybox_path:
            return
        tex_id = self._ensure_skybox_texture()
        if not tex_id:
            return
        glDisable(GL_DEPTH_TEST)
        glDisable(GL_LIGHTING)
        glEnable(GL_TEXTURE_2D)
        glBindTexture(GL_TEXTURE_2D, tex_id)
        glColor4f(1, 1, 1, 1)
        radius = 80000.0
        top_z = radius
        bottom_z = 0.0   # kept at the real horizon line, not below it - same real fix _draw_sky_gradient's own docstring explains (Aug 20 2026, per Keith's own "weird glitching... red in the sky" report)
        glBegin(GL_QUADS)
        for i, (x1, y1, x2, y2) in enumerate((
            (-radius, radius, radius, radius),      # north
            (radius, -radius, -radius, -radius),    # south
            (radius, radius, radius, -radius),      # east
            (-radius, -radius, -radius, radius),    # west
        )):
            u0, u1 = i * 0.25, (i + 1) * 0.25
            glTexCoord2f(u0, 0); glVertex3f(x1, y1, bottom_z)
            glTexCoord2f(u1, 0); glVertex3f(x2, y2, bottom_z)
            glTexCoord2f(u1, 1); glVertex3f(x2, y2, top_z)
            glTexCoord2f(u0, 1); glVertex3f(x1, y1, top_z)
        glEnd()
        glBindTexture(GL_TEXTURE_2D, 0)
        glDisable(GL_TEXTURE_2D)
        glEnable(GL_DEPTH_TEST)

    def _draw_sky_gradient(self): #vers 3
        """Real, world-space "box sky" - 4 large vertical quads (N/S/
        E/W) forming a box around the origin, each with the same real
        3-stop blend (sky_top at the zenith, sky_bot lower, sun_core
        as a brighter horizon-glow band near the bottom) rather than
        a flat single colour or a plain 2-colour linear blend (Aug 20
        2026, per Keith: "still isn't being rendered like it would be
        in game... all its doing it cycling through colours, no
        horizon and sky bands") - the real, general shape a GTA sky
        actually has: brighter/warmer near the horizon, darker/deeper
        overhead, not uniform.

        Real fix (Aug 20 2026, per Keith: "Sky effects, weird
        glitching in the background... I dont remember RED in the
        sky") - the horizon-glow band used to extend from the
        horizon (Z=0) down to well below it (-radius*0.15). Since
        this app's own map geometry is finite (unlike a real, endless
        game world), a wide-angle view could genuinely look past the
        edge of the map's own ground and into that "underground"
        portion of the box sky in the gap beyond it - showing the
        glow band's own bright colour (often a warm red/orange near
        sunrise/sunset) somewhere a real sky would never actually be
        visible from, since real terrain always extends to the
        horizon in every direction. Kept the whole box sky at or
        above the real horizon line (Z=0) now - the glow band is a
        thin strip just above it instead of extending below."""
        glDisable(GL_DEPTH_TEST)
        glDisable(GL_LIGHTING)
        top_color, bot_color, horizon_color = (
            self._sky_gradient_top, self._sky_gradient_bot,
            self._sky_gradient_horizon or self._sky_gradient_bot)
        if self._sky_gradient_flipped:
            # Settings toggle - swaps zenith/horizon (Aug 20 2026, per
            # Keith: "Remember when I said the timecyc was upside
            # down? We need a toggle to switch it either way, just in
            # case I was wrong") - his own earlier report drove the
            # "flip vertically" fix already shipped; this gives a way
            # to flip it back if that fix turns out backwards after all.
            top_color, horizon_color = horizon_color, top_color
        tr, tg, tb = top_color
        mr, mg, mb = bot_color
        hr, hg, hb = horizon_color
        radius = 80000.0
        top_z = radius
        mid_z = radius * 0.35
        horizon_z = 0.0
        glBegin(GL_QUADS)
        for x1, y1, x2, y2 in (
            (-radius, radius, radius, radius),      # north
            (radius, -radius, -radius, -radius),    # south
            (radius, radius, radius, -radius),      # east
            (-radius, -radius, -radius, radius),    # west
        ):
            # Upper: zenith (top) down to sky_bot at mid_z
            glColor3f(mr / 255, mg / 255, mb / 255); glVertex3f(x1, y1, mid_z)
            glColor3f(mr / 255, mg / 255, mb / 255); glVertex3f(x2, y2, mid_z)
            glColor3f(tr / 255, tg / 255, tb / 255); glVertex3f(x2, y2, top_z)
            glColor3f(tr / 255, tg / 255, tb / 255); glVertex3f(x1, y1, top_z)
            # Lower: horizon-glow at the real horizon line (Z=0) up to sky_bot at mid_z
            glColor3f(hr / 255, hg / 255, hb / 255); glVertex3f(x1, y1, horizon_z)
            glColor3f(hr / 255, hg / 255, hb / 255); glVertex3f(x2, y2, horizon_z)
            glColor3f(mr / 255, mg / 255, mb / 255); glVertex3f(x2, y2, mid_z)
            glColor3f(mr / 255, mg / 255, mb / 255); glVertex3f(x1, y1, mid_z)
        glEnd()
        glEnable(GL_DEPTH_TEST)

    def set_sky_gradient_flipped(self, flipped): #vers 1
        self._sky_gradient_flipped = bool(flipped)
        self.update()

    def set_skybox_path(self, path): #vers 1
        if path != self._skybox_path:
            self._skybox_path = path or ''
            self._skybox_tex_id = None
        self.update()

    def set_timecyc_path(self, path): #vers 1
        """Load a timecyc.dat file via the real, already-field-mapped
        TimecycParser from Timecyc_Editor (same parser/mapping that
        tool's own preview uses - not a second, separate parse)."""
        self._timecyc_path = path or ''
        self._timecyc_entries = []
        if not path:
            return
        try:
            from apps.components.Timecyc_Editor.timecyc_workshop import TimecycParser
            parser = TimecycParser()
            if parser.load(path):
                self._timecyc_entries = list(parser.rows)
                self._timecyc_game = parser.game
        except Exception as e:
            print(f"[DFFViewport] Failed to load timecyc file: {e}")

    def _timecyc_colors_for_hour(self, hour): #vers 4
        """Nearest row's sky-top/sky-bottom/ambient/sun-core colours
        for the given real-world hour (0-23), weather 0 (default
        slot) - same field offsets already confirmed in Timecyc_
        Editor's own _update_preview (ambient at [0-2] for every
        game; sky_top/sky_bot/sun_core differ per game - SA [9-11]/
        [12-14]/[15-17], GTA3 [6-8]/[9-11]/[12-14], VC [15-17]/
        [18-20]/[21-23]).

        Real, more accurate fix (Aug 20 2026, per Keith: "still isn't
        being rendered like it would be in game... all its doing it
        cycling through colours, no horizon and sky bands") - a
        simple 2-colour top/bottom blend was a real improvement over
        one flat colour, but still isn't what a real GTA sky actually
        looks like: brighter near the horizon (often into sun_core's
        own warm glow at sunrise/sunset), darker overhead - not a
        plain linear blend between two colours. sun_core is now read
        too so _draw_sky_gradient can build a real 3-stop gradient
        instead of 2.

        Note: Timecyc_Editor's own _update_preview has a real,
        separate bug for SA's own sun_core specifically - its code
        reads rgb(12) (the same offset as sky_bot, a likely copy-
        paste slip) despite its own comment saying [15-17]. This
        method uses the documented [15-17] offset, not that copied
        value, since this is a separate implementation, not a reuse
        of that one.

        Returns (sky_top, sky_bot, ambient, sun_core), each an
        (r,g,b) tuple, or None if no timecyc data is loaded.

        Real bug fixed earlier (caught during a full review rather
        than reported): row.time is NOT directly a 0-23 hour for
        every game - only VC actually has 24 sequential time slots.
        SA has 8 real, non-uniformly-spaced slots (confirmed directly
        against Timecyc_Editor's own SA_TIME_LABELS: Midnight/5AM/
        6AM/7AM/Noon/7PM/8PM/10PM -> real hours [0,5,6,7,12,19,20,
        22]), and GTA3 has 12 slots at 2-hour intervals (confirmed
        against that same file's own time_labels generation: hour =
        time_index*2)."""
        if not self._timecyc_entries:
            return None
        game = getattr(self, '_timecyc_game', 'VC')
        offsets = {
            'SA':   {'sky_top': 9,  'sky_bot': 12, 'ambient': 0, 'sun_core': 15},
            'GTA3': {'sky_top': 6,  'sky_bot': 9,  'ambient': 0, 'sun_core': 12},
            'VC':   {'sky_top': 15, 'sky_bot': 18, 'ambient': 0, 'sun_core': 21},
        }.get(game, {'sky_top': 15, 'sky_bot': 18, 'ambient': 0, 'sun_core': 21})
        sa_slot_hours = [0, 5, 6, 7, 12, 19, 20, 22]
        def real_hour_for_slot(time_index): #vers 1
            if game == 'SA':
                return sa_slot_hours[time_index] if 0 <= time_index < len(sa_slot_hours) else time_index
            if game == 'GTA3':
                return time_index * 2
            return time_index   # VC: slot index already is the real hour
        best = min(self._timecyc_entries,
                   key=lambda r: (r.weather != 0, abs(real_hour_for_slot(r.time) - hour)))
        vals = best.values
        def rgb_at(idx): #vers 1
            if idx + 2 >= len(vals):
                return None
            r = max(0, min(255, int(float(vals[idx]))))
            g = max(0, min(255, int(float(vals[idx + 1]))))
            b = max(0, min(255, int(float(vals[idx + 2]))))
            return (r, g, b)
        sky_top = rgb_at(offsets['sky_top'])
        sky_bot = rgb_at(offsets['sky_bot'])
        ambient = rgb_at(offsets['ambient'])
        sun_core = rgb_at(offsets['sun_core'])
        if sky_top is None or sky_bot is None or ambient is None or sun_core is None:
            return None
        return (sky_top, sky_bot, ambient, sun_core)

    def set_timecyc_playing(self, playing): #vers 2
        """Real fix (Aug 20 2026, per Keith: "there appears to be
        another timer running besides the tojb timer") - this used to
        run its own separate QTimer/hour counter, a second, redundant
        "time of day" clock alongside the app's real, existing one
        (the TObj time-flow timer, self._time_flow_timer in map_
        workshop.py, driving self._tobj_time_spin). Removed that
        second timer entirely - set_timecyc_hour (called from map_
        workshop.py's own _on_tobj_time_changed, which already fires
        on every real change to that same shared clock) now drives
        the hour instead, so timecyc genuinely tracks the one real
        simulated time of day this app already has, not a second one
        running independently alongside it. This method is now just
        an on/off flag - recomputes immediately from whatever hour
        was last set, so toggling on reflects the current real time
        right away rather than waiting for the next change."""
        self._timecyc_playing = bool(playing)
        if playing:
            self._apply_timecyc_hour()
        else:
            self.update()

    def set_timecyc_hour(self, hour): #vers 1
        """Real, external hour source (Aug 20 2026) - called from map_
        workshop.py's own _on_tobj_time_changed whenever the app's
        one real simulated time of day changes, replacing this
        viewport's own former independent timer/hour counter. No
        effect while set_timecyc_playing(False) is the current state,
        same as before."""
        self._timecyc_hour = hour % 24
        if self._timecyc_playing:
            self._apply_timecyc_hour()

    def _apply_timecyc_hour(self): #vers 1
        """Real, more accurate fix (Aug 20 2026, per Keith: "still
        isn't being rendered like it would be in game... all its
        doing it cycling through colours, no horizon and sky bands")
        - reads _timecyc_colors_for_hour's own 4-colour version
        (sky_top/sky_bot/ambient/sun_core), storing sun_core as self.
        _sky_gradient_horizon so _draw_sky_gradient can build a real
        3-stop gradient (brighter near the horizon, darker overhead -
        the real, general shape of a GTA sky, rather than a flat
        2-colour linear blend).

        Real fix (Aug 20 2026, per Keith: "the color effect seems to
        blink on and off, more so when I move or turn the view") -
        this used to also call self.makeCurrent()/_setup_lighting()/
        self.doneCurrent() directly here. paintGL already calls
        _setup_lighting() itself, unconditionally, on every single
        real frame - that direct call was pure duplication, and since
        the old caller ran off its own independent QTimer rather than
        Qt's own paint lifecycle, it could end up calling makeCurrent/
        doneCurrent at the same moment Qt itself was handling a real,
        rapid paintGL call during camera movement - the two competing
        over the same GL context is what actually caused the reported
        blinking, worse the more repaints were happening (i.e. worse
        while actively moving/turning). Removed entirely - self.
        update() below is enough on its own; the next real paintGL
        call already re-applies the new ambient tint via its own
        existing _setup_lighting() call, the same way any other state
        change already works in this app."""
        colors = self._timecyc_colors_for_hour(self._timecyc_hour)
        if colors:
            sky_top, sky_bot, ambient, sun_core = colors
            self._sky_gradient_top = sky_top
            self._sky_gradient_bot = sky_bot
            self._sky_gradient_horizon = sun_core
            self._bg_color_override = sky_bot
            # Ambient tint as a 0-1 multiplier per channel, normalised
            # against its own max channel so it tints without also
            # darkening everything to near-black on a dim timecyc row
            # (a raw /255 per channel would do that whenever the
            # brightest channel itself is well under 255).
            peak = max(ambient) or 1
            self._ambient_tint = tuple(c / peak for c in ambient)
            self.update()

    def _draw_grid_dashed(self, step, rng, cx=0, cy=0): #vers 2
        """'dashed' grid style - Keith's own "marching ants lines"
        wording, matching the same visual language already used
        elsewhere in this app for an edit-mode indicator (see _Map
        OverlayToggleButton's own docstring for that other, static-
        dashed-border use) - genuinely dashed here too, via real,
        valid legacy-OpenGL GL_LINE_STIPPLE (this app's whole
        rendering pipeline is already fixed-function/legacy OpenGL
        throughout, so this is a real, correct fit for it, not a
        modern-GL feature this codebase couldn't actually use). cx/cy:
        see _draw_grid_lines' own docstring."""
        glEnable(GL_LINE_STIPPLE)
        glLineStipple(2, 0x00FF)   # a real, standard short-dash pattern
        self._draw_grid_lines(step, rng, cx, cy)
        glDisable(GL_LINE_STIPPLE)

    def _draw_grid_dots(self, step, rng, cx=0, cy=0): #vers 3
        """'dots' grid style - Keith's own literal "just dots" wording
        - only the real grid intersection points, no connecting lines
        of any kind, genuinely sparser than every other style rather
        than dots drawn along the same lines the other styles use.
        cx/cy: see _draw_grid_lines' own docstring.

        Point anti-aliasing added here too (Aug 20 2026, same real
        "far away lines...flicker" report that fixed _draw_grid_lines
        - a small point far from the camera is just as vulnerable to
        the same sub-pixel on/off flicker a thin line is, so this
        style needed the same real treatment, its own separate GL_
        POINT_SMOOTH rather than GL_LINE_SMOOTH since this draws
        GL_POINTS, a genuinely different primitive type) - saved and
        restored the same way, not left globally on."""
        was_blend = glIsEnabled(GL_BLEND)
        glEnable(GL_POINT_SMOOTH)
        glHint(GL_POINT_SMOOTH_HINT, GL_NICEST)
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
        glPointSize(self._grid_line_size)
        glBegin(GL_POINTS)
        r, g, b = self._grid_line_color
        glColor4f(r / 255, g / 255, b / 255, 0.6)
        for gy in range(cy - rng, cy + rng + 1, step):
            for gx in range(cx - rng, cx + rng + 1, step):
                glVertex3f(gx, gy, 0)
        glEnd()
        glDisable(GL_POINT_SMOOTH)
        if not was_blend:
            glDisable(GL_BLEND)

    def _draw_grid_honeycomb(self, step, rng, cx=0, cy=0, dashed=False): #vers 2
        """'honeycomb' grid style, plus a dashed ("marching ants
        honeycomb") variant (Aug 20 2026, per Keith: "marching ants
        honeycomb"). See this method's own earlier version for the
        real hexagon-tiling math; dashed just wraps the same drawing
        in GL_LINE_STIPPLE, same real technique _draw_grid_dashed
        already uses for the plain 'lines' style."""
        import math
        s = step
        hex_w = math.sqrt(3) * s
        hex_h = 1.5 * s
        was_blend = glIsEnabled(GL_BLEND)
        glEnable(GL_LINE_SMOOTH)
        glHint(GL_LINE_SMOOTH_HINT, GL_NICEST)
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
        glLineWidth(self._grid_line_size)
        if dashed:
            glEnable(GL_LINE_STIPPLE)
            glLineStipple(2, 0x00FF)
        r, g, b = self._grid_line_color
        glColor4f(r / 255, g / 255, b / 255, 0.4)
        row = int((cy - rng) / hex_h) - 1
        max_row = int((cy + rng) / hex_h) + 1
        while row <= max_row:
            row_y = row * hex_h
            row_offset = (hex_w / 2.0) if (row % 2) else 0.0
            col = int((cx - rng - row_offset) / hex_w) - 1
            max_col = int((cx + rng - row_offset) / hex_w) + 1
            while col <= max_col:
                hx = col * hex_w + row_offset
                hy = row_y
                glBegin(GL_LINE_LOOP)
                for i in range(6):
                    ang = math.radians(60 * i - 30)
                    glVertex3f(hx + s * math.cos(ang), hy + s * math.sin(ang), 0)
                glEnd()
                col += 1
            row += 1
        if dashed:
            glDisable(GL_LINE_STIPPLE)
        glDisable(GL_LINE_SMOOTH)
        if not was_blend:
            glDisable(GL_BLEND)

    def _draw_axes(self): #vers 1
        if not OPENGL_AVAILABLE: return
        glDisable(GL_LIGHTING); glLineWidth(1.5)
        s = max(1.0, self._dist * 0.1)
        glBegin(GL_LINES)
        glColor3f(1,0,0); glVertex3f(0,0,0); glVertex3f(s,0,0)
        glColor3f(0,1,0); glVertex3f(0,0,0); glVertex3f(0,s,0)
        glColor3f(0,0,1); glVertex3f(0,0,0); glVertex3f(0,0,s)
        glEnd()
        glEnable(GL_LIGHTING)

    def _face_color(self, mat_id): #vers 5
        """Return (r,g,b,a) 0-1 for a material including alpha."""
        mats = self._materials
        if mats and 0 <= mat_id < len(mats):
            mat  = mats[mat_id]
            c    = mat.colour
            r = getattr(c,'r',180); g = getattr(c,'g',180)
            b = getattr(c,'b',180); a = getattr(c,'a',255)
            has_tex = bool(getattr(mat,'texture_name',''))
            if r==0 and g==0 and b==0 and not has_tex:
                return 0.55, 0.55, 0.55, 1.0
            if g==255 and r<100 and b<50:
                return self._paint1[0], self._paint1[1], self._paint1[2], a/255
            if r==255 and g<50 and b>100:
                return self._paint2[0], self._paint2[1], self._paint2[2], a/255
            return r/255, g/255, b/255, a/255
        return 0.7, 0.7, 0.7, 1.0

    def _emit_verts(self, v1, v2, v3, use_prelit=False, use_uv=False): #vers 1
        verts = self._vertices; norms = self._normals
        uvs   = self._uvs;      prelit = self._prelit
        has_n = len(norms)  == len(verts)
        has_u = len(uvs)    == len(verts) and use_uv
        has_p = len(prelit) == len(verts) and use_prelit
        for vi in (v1, v2, v3):
            if vi >= len(verts): continue
            if has_p:
                p = prelit[vi]
                glColor3f(p[0]/255, p[1]/255, p[2]/255)
            if has_n:
                n = norms[vi]; glNormal3f(n[0], n[1], n[2])
            if has_u:
                u = uvs[vi]; glTexCoord2f(u[0], u[1])
            v = verts[vi]; glVertex3f(v[0], v[1], v[2])

    def _draw_collision_faces(self, mode): #vers 1
        """Draw self._col_vertices/self._col_triangles as a ghost
        overlay on top of whatever's already been drawn for this
        instance (Aug 14 2026, per Keith: "Ghost is a good idea" -
        collision never replaces the model, always draws over it).
        mode: 'ghosted'|'semi_solid'|'wireframe'|'surface_mapped'.
        Unlit throughout (COLVertex carries no normal, unlike DFF
        geometry) - flat colour reads clearly enough for a collision
        overlay and avoids needing to fabricate face normals just for
        lighting. col_triangles entries are (v1,v2,v3,r,g,b) with
        r,g,b already resolved to floats 0-1 by the caller (map_
        workshop.py, via col_materials.get_material_colour) -
        surface_mapped uses them per-face, the other three modes use
        one flat colour so every mode stays visually distinct from
        the model's own render style and from each other."""
        if not OPENGL_AVAILABLE: return
        verts = getattr(self, '_col_vertices', None)
        tris  = getattr(self, '_col_triangles', None)
        if not verts or not tris: return
        glDisable(GL_LIGHTING)
        if mode == 'wireframe':
            glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
            glColor3f(1.0, 0.3, 0.3); glLineWidth(1.0)
            glBegin(GL_TRIANGLES)
            for v1, v2, v3, r, g, b in tris:
                for vi in (v1, v2, v3):
                    if vi < len(verts):
                        v = verts[vi]; glVertex3f(v[0], v[1], v[2])
            glEnd()
            glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
        else:
            alpha = {'ghosted': 0.25, 'semi_solid': 0.5, 'surface_mapped': 0.45}.get(mode, 0.3)
            glEnable(GL_BLEND); glDepthMask(False)
            glBegin(GL_TRIANGLES)
            for v1, v2, v3, r, g, b in tris:
                if mode == 'surface_mapped':
                    glColor4f(r, g, b, alpha)
                else:
                    glColor4f(1.0, 0.55, 0.15, alpha)
                for vi in (v1, v2, v3):
                    if vi < len(verts):
                        v = verts[vi]; glVertex3f(v[0], v[1], v[2])
            glEnd()
            glDepthMask(True); glDisable(GL_BLEND)
        glEnable(GL_LIGHTING)

    def _draw_wireframe(self): #vers 1
        if not OPENGL_AVAILABLE: return
        glDisable(GL_LIGHTING)
        glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
        glColor3f(0.65, 0.75, 1.0); glLineWidth(0.8)
        glBegin(GL_TRIANGLES)
        for v1,v2,v3,mid in self._triangles:
            self._emit_verts(v1,v2,v3)
        glEnd()
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
        glEnable(GL_LIGHTING)

    def _draw_selection_overlay(self): #vers 1
        """Highlight the active sub-object selection (vertex/edge/face/poly)
        on top of the already-rendered mesh. No-op in 'object' mode."""
        if not OPENGL_AVAILABLE: return
        mode = getattr(self, '_select_mode', 'object')
        if mode == 'object':
            return
        verts = self._vertices
        if not verts:
            return
        glDisable(GL_LIGHTING)
        glDisable(GL_DEPTH_TEST)

        if mode == 'vertex' and self._selected_verts:
            glColor3f(1.0, 0.8, 0.1)
            glPointSize(max(4.0, min(10.0, self._dist * 0.03)))
            glBegin(GL_POINTS)
            for vi in self._selected_verts:
                if 0 <= vi < len(verts):
                    v = verts[vi]; glVertex3f(v[0], v[1], v[2])
            glEnd()

        elif mode == 'edge' and self._selected_edges:
            glColor3f(1.0, 0.8, 0.1)
            glLineWidth(3.0)
            glBegin(GL_LINES)
            for (vi, vj) in self._selected_edges:
                if 0 <= vi < len(verts) and 0 <= vj < len(verts):
                    a, b = verts[vi], verts[vj]
                    glVertex3f(*a); glVertex3f(*b)
            glEnd()

        elif mode in ('face', 'poly') and self._selected_faces:
            glColor4f(1.0, 0.8, 0.1, 0.45)
            glEnable(GL_BLEND); glDepthMask(False)
            glBegin(GL_TRIANGLES)
            for fi in self._selected_faces:
                if 0 <= fi < len(self._triangles):
                    v1, v2, v3, _ = self._triangles[fi]
                    self._emit_verts(v1, v2, v3)
            glEnd()
            glDepthMask(True)
            # Outline on top so the selection reads clearly in solid mode
            glColor3f(1.0, 0.9, 0.2); glLineWidth(2.0)
            glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
            glBegin(GL_TRIANGLES)
            for fi in self._selected_faces:
                if 0 <= fi < len(self._triangles):
                    v1, v2, v3, _ = self._triangles[fi]
                    self._emit_verts(v1, v2, v3)
            glEnd()
            glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)

        glEnable(GL_DEPTH_TEST)
        glEnable(GL_LIGHTING)

    def _draw_solid(self, alpha_multiplier=1.0): #vers 4
        if not OPENGL_AVAILABLE: return
        flags = self._geom_flags()
        use_lighting = bool(flags & self.rpGEOMETRYLIGHT) and bool(self._normals)
        use_prelit   = bool(flags & self.rpGEOMETRYPRELIT) and bool(self._prelit)
        use_modulate = bool(flags & self.rpGEOMETRYMODULATEMATERIALCOLOR)
        if use_lighting:
            glEnable(GL_LIGHTING)
        else:
            glDisable(GL_LIGHTING)
        use_p = (use_prelit or self._use_prelight) and bool(self._prelit)
        opaque = []; transparent = []
        # alpha_multiplier < 1.0 (Aug 1 2026, Semi-Solid render mode -
        # per Keith: "Render view should me merged with LOD view,
        # labeled as Render: Texture, Non-texture, Semi-Solid,
        # Wireframe...") forces every triangle through the blend path
        # below instead of the opaque one, scaling its alpha down
        # uniformly - a plain "ghosted" look, distinct from Non-
        # Textured (which is fully opaque flat/lit shading).
        force_transparent = alpha_multiplier < 0.999
        for tri in self._triangles:
            fc = self._face_color(tri[3])
            is_transparent = force_transparent or (len(fc) > 3 and fc[3] < 0.99)
            (transparent if is_transparent else opaque).append((tri, fc))
        glBegin(GL_TRIANGLES)
        for (v1,v2,v3,mid),(r,g,b,*rest) in opaque:
            a = rest[0] if rest else 1.0
            if not use_p:
                if use_modulate: glColor4f(r,g,b,a)
                else: glColor4f(1.0,1.0,1.0,1.0)
            self._emit_verts(v1,v2,v3, use_prelit=use_p)
        glEnd()
        if transparent:
            glEnable(GL_BLEND); glDepthMask(False)
            glBegin(GL_TRIANGLES)
            for (v1,v2,v3,mid),fc in transparent:
                r, g, b = fc[0], fc[1], fc[2]
                a = (fc[3] if len(fc) > 3 else 1.0) * alpha_multiplier
                if not use_p:
                    if use_modulate: glColor4f(r,g,b,a)
                    else: glColor4f(1.0,1.0,1.0,a)
                self._emit_verts(v1,v2,v3, use_prelit=use_p)
            glEnd()
            glDepthMask(True); glDisable(GL_BLEND)
        glEnable(GL_LIGHTING)
        glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
        glColor4f(0,0,0,0.18); glLineWidth(0.5)
        glEnable(GL_POLYGON_OFFSET_LINE); glPolygonOffset(-1,-1)
        glBegin(GL_TRIANGLES)
        for v1,v2,v3,mid in self._triangles:
            self._emit_verts(v1,v2,v3)
        glEnd()
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
        glDisable(GL_POLYGON_OFFSET_LINE)

    def _draw_textured(self): #vers 4
        if not OPENGL_AVAILABLE: return
        flags = self._geom_flags()
        use_lighting = bool(flags & self.rpGEOMETRYLIGHT) and bool(self._normals)
        use_prelit   = bool(flags & self.rpGEOMETRYPRELIT) and bool(self._prelit)
        use_modulate = bool(flags & self.rpGEOMETRYMODULATEMATERIALCOLOR)
        if use_lighting:
            glEnable(GL_LIGHTING)
        else:
            glDisable(GL_LIGHTING)
        glEnable(GL_TEXTURE_2D)
        glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE,
                  GL_MODULATE if use_modulate else GL_REPLACE)
        # Alpha-textured objects (Aug 1 2026, per Keith: "show any
        # objects with alpha textures, as that would display in the
        # game") - chain-link fences, foliage, glass etc. rely on
        # per-pixel alpha baked into the texture itself (already
        # uploaded to the GPU correctly via GL_RGBA in
        # _upload_textures), not material face-color alpha, which is
        # what the opaque/transparent batch split below is actually
        # keyed on - most such objects' materials are still alpha=1.0,
        # so without this they rendered fully opaque regardless of
        # what their texture's own alpha channel says. GL_ALPHA_TEST
        # gives cutout-style transparency (a pixel either draws fully
        # or not at all, based on a threshold) rather than smooth
        # blending - deliberately, since it needs no back-to-front
        # sorting and doesn't disturb depth writes, unlike GL_BLEND.
        glEnable(GL_ALPHA_TEST)
        glAlphaFunc(GL_GREATER, 0.5)
        use_p = (use_prelit or self._use_prelight) and bool(self._prelit)
        mats  = self._materials
        batches: Dict[tuple,list] = {}
        no_tex = []
        for tri in self._triangles:
            v1,v2,v3,mid = tri
            tname = ''
            if mats and 0 <= mid < len(mats):
                tname = getattr(mats[mid],'texture_name','') or ''
            gl_id = self._tex_ids.get(tname.lower(), 0)
            if gl_id:
                r,g,b,a = self._face_color(mid)
                key = (gl_id, round(r,2), round(g,2), round(b,2), round(a,2))
                batches.setdefault(key,[]).append(tri)
            else:
                no_tex.append(tri)
        opaque_b = {k:v for k,v in batches.items() if k[4]>=0.99}
        transp_b = {k:v for k,v in batches.items() if k[4]<0.99}
        for batch_dict, use_blend in [(opaque_b,False),(transp_b,True)]:
            if use_blend: glEnable(GL_BLEND); glDepthMask(False)
            for key, tris in batch_dict.items():
                gl_id=key[0]; r=key[1]; g=key[2]; b=key[3]; a=key[4]
                glBindTexture(GL_TEXTURE_2D, gl_id)
                if not use_p:
                    if use_modulate: glColor4f(r,g,b,a)
                    else: glColor4f(1.0,1.0,1.0,a)
                glBegin(GL_TRIANGLES)
                for v1,v2,v3,mid in tris:
                    self._emit_verts(v1,v2,v3, use_prelit=use_p, use_uv=True)
                glEnd()
            if use_blend: glDepthMask(True); glDisable(GL_BLEND)
        glBindTexture(GL_TEXTURE_2D, 0); glDisable(GL_TEXTURE_2D)
        glDisable(GL_ALPHA_TEST)
        no_opaque = [t for t in no_tex if self._face_color(t[3])[3]>=0.99]
        no_transp = [t for t in no_tex if self._face_color(t[3])[3]<0.99]
        for tri_list, use_blend in [(no_opaque,False),(no_transp,True)]:
            if use_blend: glEnable(GL_BLEND); glDepthMask(False)
            for v1,v2,v3,mid in tri_list:
                r,g,b,a = self._face_color(mid)
                if not use_p:
                    if use_modulate: glColor4f(r,g,b,a)
                    else: glColor4f(1.0,1.0,1.0,a)
                glBegin(GL_TRIANGLES)
                self._emit_verts(v1,v2,v3, use_prelit=use_p)
                glEnd()
            if use_blend: glDepthMask(True); glDisable(GL_BLEND)
        glEnable(GL_LIGHTING)

    # RW geometry flags
    rpGEOMETRYTRISTRIP          = 0x0001
    rpGEOMETRYPOSITIONS         = 0x0002
    rpGEOMETRYTEXTURED          = 0x0004
    rpGEOMETRYPRELIT            = 0x0008
    rpGEOMETRYNORMALS           = 0x0010
    rpGEOMETRYLIGHT             = 0x0020
    rpGEOMETRYMODULATEMATERIALCOLOR = 0x0040
    rpGEOMETRYTEXTURED2         = 0x0080

    def _geom_flags(self): #vers 1
        """Return geometry flags from current model, or sensible defaults."""
        return getattr(self, '_current_geom_flags',
               self.rpGEOMETRYLIGHT | self.rpGEOMETRYMODULATEMATERIALCOLOR | self.rpGEOMETRYNORMALS)

    def _strip_tex_suffix(self, name: str) -> str: #vers 2
        """Strip GTA texture suffix.
        Handles: buildrt4_fehihwm (alpha suffix) and vehiclegeneric256 (numeric size suffix)."""
        import re as _re
        n = _re.sub(r'_[a-z]{4,8}$', '', name)
        n = _re.sub(r'\d+$', '', n)
        return n

    def _rw_wrap_to_gl(self, rw: int) -> int: #vers 1
        """Convert RW addressing mode to GL wrap constant.
        0=NONE 1=WRAP 2=CLAMP 3=MIRROR"""
        if not OPENGL_AVAILABLE: return 0
        if rw == 2: return GL_CLAMP_TO_EDGE
        if rw == 3: return GL_MIRRORED_REPEAT
        return GL_REPEAT

    def _upload_textures(self, textures: list, additive: bool = False): #vers 4
        if not OPENGL_AVAILABLE: return
        # Guard: don't attempt upload if GL context not initialized
        try:
            if hasattr(self, 'isValid') and not self.isValid():
                self._pending_textures = getattr(self, '_pending_textures', []) + textures
                from PyQt6.QtCore import QTimer
                QTimer.singleShot(200, lambda: self._flush_pending_textures())
                return
        except Exception:
            pass
        self.makeCurrent()
        if not additive:
            self.clear_textures()
        for tex in textures:
            name = tex.get('name','').lower()
            rgba = tex.get('rgba_data', b'')
            w    = tex.get('width', 0); h = tex.get('height', 0)
            if not (name and rgba and w > 0 and h > 0): continue
            # Skip re-uploading an already-loaded texture (Aug 1 2026,
            # per Keith: "loading textures using alot of memory",
            # plus a real crash at glTexImage2D) - this previously
            # created a brand new GL texture object unconditionally on
            # every single call, even for a name already in self.
            # _tex_ids, silently orphaning the old GL texture ID's
            # VRAM (self._tex_ids[name] = gl_id just overwrites the
            # dict entry, never calling glDeleteTextures on what it
            # replaced) - a genuine, severe leak. Most damaging under
            # LOD Test mode specifically, where _refresh_world_view
            # (and therefore this method, with additive=True) reruns
            # on every single mouse move, re-uploading the same
            # already-loaded textures repeatedly and leaking a fresh
            # copy of each one's VRAM every time, until the driver
            # eventually fails to allocate more and crashes exactly
            # where Keith's traceback shows. A texture's pixel data
            # for a given name doesn't change between calls, so
            # there's nothing to gain from re-uploading it - name is
            # a stable, sufficient cache key here.
            if name in self._tex_ids:
                continue
            if getattr(self, '_texture_downscale_enabled', False):
                threshold = getattr(self, '_texture_downscale_threshold', 512)
                if w > threshold or h > threshold:
                    target = getattr(self, '_texture_downscale_target', 256)
                    rgba, w, h = self._downscale_rgba(rgba, w, h, target)
            wrap_u = tex.get('wrap_u', 1)
            wrap_v = tex.get('wrap_v', 1)
            gl_wrap_s = self._rw_wrap_to_gl(wrap_u)
            gl_wrap_t = self._rw_wrap_to_gl(wrap_v)
            gl_id = glGenTextures(1)
            glBindTexture(GL_TEXTURE_2D, gl_id)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, gl_wrap_s)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, gl_wrap_t)
            try:
                glTexImage2D(GL_TEXTURE_2D,0,GL_RGBA,w,h,0,GL_RGBA,GL_UNSIGNED_BYTE,rgba)
                glGenerateMipmap(GL_TEXTURE_2D)
                self._tex_ids[name] = gl_id
                self._tex_wrap[name] = (wrap_u, wrap_v)
            except Exception as e:
                print(f"[DFFViewport] Tex upload fail '{name}': {e}")
                glDeleteTextures(1,[gl_id])
        glBindTexture(GL_TEXTURE_2D, 0); self.doneCurrent()

    def _flush_pending_textures(self): #vers 2
        """Upload any textures that were queued before GL context was ready."""
        pending = getattr(self, '_pending_textures', [])
        if not pending: return
        if hasattr(self, 'isValid') and not self.isValid():
            from PyQt6.QtCore import QTimer
            QTimer.singleShot(200, self._flush_pending_textures)
            return
        self._pending_textures = []
        self._upload_textures(pending, additive=True)
        self.update()

    def set_texture_downscale_settings(self, enabled, threshold=512, target=256): #vers 1
        """Configure texture downscaling - per Keith: "loading
        textures using alot of memory, so im thinking about a texture
        reduction option, keep 64. 128, 256 untouched but render down
        to 256x256 anything over 512x512." Stored as instance
        attributes rather than threaded through every call, since
        _upload_textures is the single central place all texture
        uploads go through regardless of caller (the world-view
        pipeline, _flush_pending_textures, and any other direct
        caller) - setting it once here covers all of them."""
        self._texture_downscale_enabled = enabled
        self._texture_downscale_threshold = threshold
        self._texture_downscale_target = target

    def _downscale_rgba(self, rgba, w, h, target): #vers 1
        """Downsample RGBA8888 pixel data to target x target using
        numpy - per Keith's texture reduction request (see set_
        texture_downscale_settings). Block-averaging for the clean-
        multiple case (w and h both evenly divisible by target - true
        for every size Keith actually mentioned: 512/256=2,
        1024/256=4, 2048/256=8, all clean integer ratios for power-of-
        2 game textures), which gives noticeably better quality than
        nearest-neighbor since it blends each output pixel from its
        whole source block rather than picking one sample and
        discarding the rest. Falls back to simple nearest-neighbor
        index sampling for any size that doesn't divide evenly (rare
        for game textures, but not impossible) - always produces a
        valid target x target result either way, never raises for a
        mismatched size. Returns (new_rgba_bytes, target, target)."""
        arr = np.frombuffer(rgba, dtype=np.uint8)
        expected = w * h * 4
        if arr.size != expected:
            arr = np.resize(arr, expected)   # defensive - malformed input shouldn't crash the upload
        arr = arr.reshape(h, w, 4)
        if w % target == 0 and h % target == 0:
            block_w = w // target
            block_h = h // target
            arr = arr.reshape(target, block_h, target, block_w, 4)
            downsampled = arr.mean(axis=(1, 3)).astype(np.uint8)
        else:
            row_idx = (np.arange(target) * h // target)
            col_idx = (np.arange(target) * w // target)
            downsampled = arr[row_idx][:, col_idx]
        return downsampled.tobytes(), target, target

    def clear_textures(self): #vers 2
        if OPENGL_AVAILABLE and self._tex_ids:
            try: glDeleteTextures(len(self._tex_ids), list(self._tex_ids.values()))
            except Exception: pass
        self._tex_ids.clear()
        self._tex_wrap.clear()

    def load_geometry(self, geometry, materials: list): #vers 3
        self._all_geoms  = []  # clear multi-geom data
        self._current_geom_flags = getattr(geometry, 'flags', 0)
        self._vertices  = [(v.x,v.y,v.z) for v in geometry.vertices]
        self._normals   = [(n.x,n.y,n.z) for n in geometry.normals] if geometry.normals else []
        self._uvs       = [(u.u,u.v) for u in geometry.uv_layers[0]] if geometry.uv_layers else []
        self._triangles = [(t.v1,t.v2,t.v3,t.material_id) for t in geometry.triangles]
        self._materials = materials
        self._prelit    = [(c.r,c.g,c.b,c.a) for c in getattr(geometry,'colors',[])] if geometry.colors else []
        self._auto_fit(); self.update()
        if self._on_geometry_loaded:
            try: self._on_geometry_loaded()
            except Exception: pass

    def _auto_fit(self): #vers 1
        if not self._vertices: return
        xs=[v[0] for v in self._vertices]; ys=[v[1] for v in self._vertices]; zs=[v[2] for v in self._vertices]
        diag = math.sqrt((max(xs)-min(xs))**2+(max(ys)-min(ys))**2+(max(zs)-min(zs))**2)
        self._dist  = max(diag*1.5, 2.0)
        self._pan_x = -(max(xs)+min(xs))/2
        self._pan_y = -(max(ys)+min(ys))/2
        self.update()

    def set_render_mode(self, mode: str): #vers 1
        self._mode = mode; self.update()

    def set_backface_cull(self, v: bool): #vers 1
        self._backface_cull = v; self.update()

    def set_show_grid(self, v: bool): #vers 1
        self._show_grid = v; self.update()

    def set_grid_type(self, grid_type: str): #vers 1
        """Real, direct setter for self._grid_type (Aug 20 2026, per
        Keith's own real grid-style request) - matches set_show_grid's
        own existing pattern right above exactly, for the same
        settings-caller-applies-real-state convention already used
        for every other viewport setting map_workshop.py's own apply_
        settings drives. Valid values: 'lines'/'squares'/'dashed'/
        'dots' - see _draw_grid's own docstring for what each one
        actually draws. An unrecognised value falls back to 'lines'
        (_draw_grid's own dispatch already treats anything it doesn't
        recognise this way), so this never silently no-ops on a typo'd
        value."""
        self._grid_type = grid_type; self.update()

    def set_grid_colors(self, bg_rgb, line_rgb): #vers 1
        """Set squares-fill (bg) and line/dot colour, each an (r,g,b) 0-255 tuple."""
        self._squares_color = bg_rgb
        self._grid_line_color = line_rgb
        self.update()

    def set_grid_line_size(self, size): #vers 1
        """4-10px range for grid lines/dots."""
        self._grid_line_size = max(4, min(10, size))
        self.update()

    def set_grid_spacing(self, spacing): #vers 1
        self._grid_spacing = max(1, spacing)
        self.update()

    def set_grid_fixed_step(self, step): #vers 1
        """World units per cell for 'fixed' (resize-with-models) mode
        - separate from _grid_spacing (that one's a divisor for
        'zoom_relative' mode, a real map-scale value like 200 would
        be much too large used the same way there)."""
        self._grid_fixed_step = max(1, step)
        self.update()

    def set_grid_cell_count(self, count): #vers 1
        """Total grid diameter in cells (Aug 20 2026, per Keith:
        "number of cells, hex 12, 24, 36, 48, 60, 72 etc") - replaces
        the old fixed *10 multiplier in _draw_grid's own rng
        computation, applies to every grid style (not just honeycomb),
        since all styles share the same rng/step-based coverage."""
        self._grid_cell_count = max(2, count)
        self.update()

    def set_radar_tex_layer(self, enabled, tile_textures=None, game_key='sa'): #vers 2
        """Show the real radar tile textures (read directly from the
        game's own loaded IMG archive - radarNN.txd entries, per
        Keith: "those radar.txd files are in the gta3... unless it's
        SOL where they're in another file") at their own real world
        positions, as an alternative to the grid.

        tile_textures: list of (rgba_bytes, width, height) or None
        per tile (None for a tile whose TXD wasn't found), in the
        same tile-index order compute_radar_grid produces - the
        caller (map_workshop.py) is the one that actually reads these
        from ModelCache.get_textures(), matching this app's existing
        "widget draws plain data, caller resolves real data source"
        split; this viewport never touches IMG files or ModelCache
        directly. Clears any previously loaded tiles' own GL textures
        first, so switching games/reloading doesn't leak the old
        ones."""
        for tile in self._radar_tex_tiles:
            if tile.get('tex_id'):
                try:
                    self.makeCurrent()
                    glDeleteTextures([tile['tex_id']])
                    self.doneCurrent()
                except Exception:
                    pass
        self._radar_tex_tiles = []
        self._show_radar_tex_layer = enabled
        if not enabled or not tile_textures:
            self.update()
            return
        from apps.methods.gta_dat_parser import compute_radar_grid, RADAR_GRID_PRESETS
        preset = RADAR_GRID_PRESETS.get(game_key, RADAR_GRID_PRESETS['sa'])
        grid_tiles = compute_radar_grid(**preset)
        for i, tex in enumerate(tile_textures):
            if i >= len(grid_tiles) or tex is None:
                continue
            rgba_bytes, w, h = tex
            gt = grid_tiles[i]
            self._radar_tex_tiles.append({
                'rgba': rgba_bytes, 'width': w, 'height': h, 'tex_id': None,
                'min_x': gt.min_x, 'min_y': gt.min_y,
                'max_x': gt.max_x, 'max_y': gt.max_y,
            })
        self.update()

    def _ensure_radar_tex_tile(self, tile): #vers 2
        """Lazily upload one radar tile's own raw RGBA bytes (already
        decoded by the caller via ModelCache.get_textures/parse_txd -
        this method only uploads, it never reads a file itself)."""
        if tile['tex_id']:
            return tile['tex_id']
        try:
            gl_id = glGenTextures(1)
            glBindTexture(GL_TEXTURE_2D, gl_id)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
            glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tile['width'], tile['height'],
                         0, GL_RGBA, GL_UNSIGNED_BYTE, tile['rgba'])
            glBindTexture(GL_TEXTURE_2D, 0)
            tile['tex_id'] = gl_id
        except Exception as e:
            print(f"[DFFViewport] Failed to upload radar tile texture: {e}")
            tile['tex_id'] = False
        return tile['tex_id']

    def _draw_radar_tex_layer(self): #vers 2
        """V-coordinates: row 0 of the saved PNG is north (capture_
        radar_tile uses grabFramebuffer's own display-ready, already-
        correctly-oriented output), which uploads as V=0 - so V=0
        must map to the quad's own north (max_y) edge, not min_y, or
        every tile would render upside-down (Aug 20 2026, caught
        before commit rather than shipped and found later)."""
        if not self._radar_tex_tiles:
            return
        glDisable(GL_LIGHTING)
        # Real fix (Aug 20 2026, same class of bug as _draw_grid's own
        # "disappears when IPL models load" - this draws at Z=0 too,
        # and drawing it BEFORE world instances means it would write
        # depth values that then incorrectly occlude any real ground-
        # level geometry at or below Z=0 drawn right after it. This is
        # meant to be a pure background layer, not something that
        # should participate in depth-testing against real geometry at
        # all - disabling both the test and depth writes, not just the
        # test, so it never affects what draws afterward either.
        was_depth_test = glIsEnabled(GL_DEPTH_TEST)
        glDisable(GL_DEPTH_TEST)
        glDepthMask(GL_FALSE)
        glEnable(GL_TEXTURE_2D)
        glColor4f(1, 1, 1, 1)
        for tile in self._radar_tex_tiles:
            tex_id = self._ensure_radar_tex_tile(tile)
            if not tex_id:
                continue
            glBindTexture(GL_TEXTURE_2D, tex_id)
            glBegin(GL_QUADS)
            glTexCoord2f(0, 1); glVertex3f(tile['min_x'], tile['min_y'], 0)
            glTexCoord2f(1, 1); glVertex3f(tile['max_x'], tile['min_y'], 0)
            glTexCoord2f(1, 0); glVertex3f(tile['max_x'], tile['max_y'], 0)
            glTexCoord2f(0, 0); glVertex3f(tile['min_x'], tile['max_y'], 0)
            glEnd()
        glBindTexture(GL_TEXTURE_2D, 0)
        glDisable(GL_TEXTURE_2D)
        glDepthMask(GL_TRUE)
        if was_depth_test:
            glEnable(GL_DEPTH_TEST)
        glEnable(GL_LIGHTING)

    def set_grid_scale_mode(self, mode): #vers 2
        """'zoom_relative' (default - grid stays same on-screen size
        regardless of zoom), 'fixed' (constant world-unit cell size,
        scales with zoom like models), or 'radar_tiles' (matches the
        real, game-specific radar tile grid - see set_radar_grid_
        extent)."""
        valid = ('zoom_relative', 'fixed', 'radar_tiles')
        self._grid_scale_mode = mode if mode in valid else 'zoom_relative'
        self.update()

    def set_radar_grid_extent(self, tile_size, half_extent): #vers 1
        """Real world-unit tile size + half the total grid extent for
        the currently loaded game, per RADAR_GRID_PRESETS (Aug 20
        2026, per Keith: "resize grid locked to the radar size...
        show the radar rendered under the model spawn layout")."""
        self._radar_grid_tile_size = tile_size
        self._radar_grid_half_extent = half_extent
        self.update()

    def load_all_geometries(self, geometries, materials_list, frames, atomics, damaged=False): #vers 4
        self._all_geoms = []
        self._vertices  = []  # clear single-geom data
        # Use flags from first geometry as representative
        if geometries:
            self._current_geom_flags = getattr(geometries[0], 'flags', 0)
        fname = {i: (f.name.lower() if f.name else '') for i,f in enumerate(frames)}
        for i, geom in enumerate(geometries):
            atomic = next((a for a in atomics if a.geometry_index == i), None)
            if not atomic: continue
            fi   = atomic.frame_index
            name = fname.get(fi, '')
            is_dam = name.endswith('_dam')
            is_ok  = name.endswith('_ok')
            is_lod = name.endswith('_vlo') or name.endswith('_lo')
            if is_dam and not damaged: continue
            if is_ok  and damaged: continue
            if is_lod and not getattr(self, '_show_lod', False): continue
            # Skip frames hidden by the frame tree
            if name and name in getattr(self, '_hidden_frames', set()): continue
            rot, tx, ty, tz = self._calc_world_matrix(frames, fi)
            verts = [(rot[0]*v.x+rot[1]*v.y+rot[2]*v.z+tx,
                      rot[3]*v.x+rot[4]*v.y+rot[5]*v.z+ty,
                      rot[6]*v.x+rot[7]*v.y+rot[8]*v.z+tz) for v in geom.vertices]
            norms = [(rot[0]*n.x+rot[1]*n.y+rot[2]*n.z,
                      rot[3]*n.x+rot[4]*n.y+rot[5]*n.z,
                      rot[6]*n.x+rot[7]*n.y+rot[8]*n.z) for n in geom.normals] if geom.normals else []
            uvs   = [(u.u,u.v) for u in geom.uv_layers[0]] if geom.uv_layers else []
            tris  = [(t.v1,t.v2,t.v3,t.material_id) for t in geom.triangles]
            prelit= [(c.r,c.g,c.b,c.a) for c in geom.colors] if geom.colors else []
            geom_flags = getattr(geom, 'flags', 0)
            self._all_geoms.append((verts,norms,uvs,tris,geom.materials,prelit,geom_flags))

        # Place wheels at dummy frames using wheels.DFF — only when _show_wheels is set
        if getattr(self, '_show_wheels', False):
            wheel_data = self._get_wheel_geom_data()
            if wheel_data:
                wv,wn,wu,wt,wm,wp = wheel_data[:6]
                wflags = wheel_data[6] if len(wheel_data) > 6 else 0
                front_scale = getattr(self, '_wheel_front_scale', 1.0)
                rear_scale  = getattr(self, '_wheel_rear_scale',  1.0)
                mult        = getattr(self, '_wheel_scale_mult', 1.0)
                front_scale *= mult
                rear_scale  *= mult
                for fi2, fn2 in fname.items():
                    if 'dummy' not in fn2: continue
                    if not any(w in fn2 for w in ('wheel_lf','wheel_rf','wheel_lb','wheel_rb',
                                                   'wheel_lm','wheel_rm')): continue
                    r2,tx2,ty2,tz2 = self._calc_world_matrix(frames, fi2)
                    is_left  = 'wheel_l' in fn2
                    is_front = 'wheel_lf' in fn2 or 'wheel_rf' in fn2
                    scale    = front_scale if is_front else rear_scale
                    v2 = []
                    for vx,vy,vz in wv:
                        # Scale wheel geometry around its local origin
                        svx, svy, svz = vx*scale, vy*scale, vz*scale
                        wx = r2[0]*svx+r2[1]*svy+r2[2]*svz+tx2
                        wy = r2[3]*svx+r2[4]*svy+r2[5]*svz+ty2
                        wz = r2[6]*svx+r2[7]*svy+r2[8]*svz+tz2
                        if is_left:
                            wx = tx2 - (r2[0]*svx+r2[1]*svy+r2[2]*svz)
                        v2.append((wx,wy,wz))
                    n2 = [(r2[0]*nx+r2[1]*ny+r2[2]*nz,
                           r2[3]*nx+r2[4]*ny+r2[5]*nz,
                           r2[6]*nx+r2[7]*ny+r2[8]*nz) for nx,ny,nz in wn] if wn else []
                    self._all_geoms.append((v2,n2,wu,wt,wm,wp,wflags))
        all_pts=[p for g in self._all_geoms for p in g[0]]
        if all_pts:
            xs=[p[0] for p in all_pts]; ys=[p[1] for p in all_pts]
            diag=math.sqrt(max(1,(max(xs)-min(xs))**2+(max(ys)-min(ys))**2))
            self._dist=max(diag*2.0,2.0)
            self._pan_x=-(max(xs)+min(xs))/2; self._pan_y=-(max(ys)+min(ys))/2
        self.update()
        if self._on_geometry_loaded:
            try: self._on_geometry_loaded()
            except Exception: pass

    def _calc_world_matrix(self, frames, frame_idx): #vers 1
        r=[1,0,0,0,1,0,0,0,1]; tx=ty=tz=0.0
        visited=set(); idx=frame_idx; chain=[]
        while 0<=idx<len(frames) and idx not in visited:
            visited.add(idx); chain.append(frames[idx]); idx=frames[idx].parent_index
        for frame in reversed(chain):
            fr=frame.rotation; fp=frame.position
            nr=[r[0]*fr[0]+r[1]*fr[3]+r[2]*fr[6],r[0]*fr[1]+r[1]*fr[4]+r[2]*fr[7],r[0]*fr[2]+r[1]*fr[5]+r[2]*fr[8],
                r[3]*fr[0]+r[4]*fr[3]+r[5]*fr[6],r[3]*fr[1]+r[4]*fr[4]+r[5]*fr[7],r[3]*fr[2]+r[4]*fr[5]+r[5]*fr[8],
                r[6]*fr[0]+r[7]*fr[3]+r[8]*fr[6],r[6]*fr[1]+r[7]*fr[4]+r[8]*fr[7],r[6]*fr[2]+r[7]*fr[5]+r[8]*fr[8]]
            r=nr; tx+=fp.x; ty+=fp.y; tz+=fp.z
        return r, tx, ty, tz

    def load_wheels_dff(self, path: str, wheel_type: str = 'wheel_saloon_l0'): #vers 1
        try:
            from apps.methods.dff_parser import load_dff
            self._wheels_model = load_dff(path)
            self._wheel_type   = wheel_type
        except Exception as e:
            print(f"[DFFViewport] Wheel DFF load fail: {e}")

    def _get_wheel_geom_data(self): #vers 2
        """Return geometry data for the current wheel type from wheels.DFF.
        Matches _wheel_type exactly (e.g. wheel_saloon_l0), falls back to first wheel."""
        m = self._wheels_model
        if not m or not m.geometries: return None
        fname = {i: (f.name.lower() if f.name else '') for i,f in enumerate(m.frames)}
        wtype = getattr(self, '_wheel_type', 'wheel_saloon_l0').lower()

        def _geom_data(geom):
            return (
                [(v.x,v.y,v.z) for v in geom.vertices],
                [(n.x,n.y,n.z) for n in geom.normals] if geom.normals else [],
                [(u.u,u.v) for u in geom.uv_layers[0]] if geom.uv_layers else [],
                [(t.v1,t.v2,t.v3,t.material_id) for t in geom.triangles],
                geom.materials,
                [(c.r,c.g,c.b,c.a) for c in geom.colors] if geom.colors else [],
                getattr(geom,'flags',0)
            )

        # Pass 1: exact match on full wheel type name
        for i, geom in enumerate(m.geometries):
            atomic = next((a for a in m.atomics if a.geometry_index==i), None)
            if not atomic: continue
            name = fname.get(atomic.frame_index,'')
            if name == wtype:
                return _geom_data(geom)

        # Pass 2: wheel type contained in frame name (e.g. wheel_saloon_l0 in wheel_saloon_l0_dam)
        for i, geom in enumerate(m.geometries):
            atomic = next((a for a in m.atomics if a.geometry_index==i), None)
            if not atomic: continue
            name = fname.get(atomic.frame_index,'')
            if wtype in name and not name.endswith('_dam'):
                return _geom_data(geom)

        # Pass 3: base type without _l0 suffix
        base = wtype.replace('_l0','').replace('_lo','')
        for i, geom in enumerate(m.geometries):
            atomic = next((a for a in m.atomics if a.geometry_index==i), None)
            if not atomic: continue
            name = fname.get(atomic.frame_index,'')
            if base in name and not name.endswith('_dam'):
                return _geom_data(geom)

        # Pass 4: first non-damaged wheel geometry as fallback
        for i, geom in enumerate(m.geometries):
            atomic = next((a for a in m.atomics if a.geometry_index==i), None)
            if not atomic: continue
            name = fname.get(atomic.frame_index,'')
            if 'wheel' in name and not name.endswith('_dam'):
                return _geom_data(geom)
        return None

    def set_assembly_mode(self, enabled: bool): #vers 1
        self._assembly_mode = enabled; self.update()

    def set_show_col_ghosted(self, enabled: bool): #vers 1
        self.show_col_ghosted = enabled; self.update()

    def set_show_col_semi_solid(self, enabled: bool): #vers 1
        self.show_col_semi_solid = enabled; self.update()

    def set_show_col_wireframe(self, enabled: bool): #vers 1
        self.show_col_wireframe = enabled; self.update()

    def set_show_col_surface_mapped(self, enabled: bool): #vers 1
        self.show_col_surface_mapped = enabled; self.update()

    def set_show_paths(self, enabled: bool): #vers 1
        self.show_paths = enabled; self.update()

    def set_path_segments(self, segments): #vers 2
        """Replace the path data drawn when show_paths is on. Each
        entry is a pair of (x,y,z) endpoint tuples, one real graph
        edge (Aug 16 2026 rework - was a per-group ordered coordinate
        list drawn as a connected polyline, wrong topology for real
        path data; see _draw_paths' own docstring for the full
        story). Conversion from PathGroup/PathNode (gta_dat_parser.
        py's parsed loader.paths) into this flat edge-list shape
        happens in map_workshop.py's _refresh_path_visualization -
        this widget only ever deals in plain coordinate pairs, same
        separation as everywhere else in this file (no PathGroup/
        PathNode/COLModel dataclass imports here)."""
        self._path_segments = segments or []
        self.update()

    def set_path_node_owners(self, owner_map): #vers 1
        """Set the (position -> (group_ref, node_index)) mapping used
        to resolve a picked/dragged node back to its real, live
        PathGroup/PathNode data (Aug 17 2026, for interactive path
        node editing - see the fuller explanation in __init__ where
        self._path_node_owner_map is first declared). Built by map_
        workshop.py's _refresh_path_visualization alongside the flat
        segments list set_path_segments takes, using the exact same
        (x,y,z) tuples - so a position looked up here always matches
        a position actually present in _path_segments, no rounding or
        tolerance needed for the dict lookup itself (only picking,
        which is a nearest-point search over screen-space distance,
        needs a tolerance)."""
        self._path_node_owner_map = owner_map or {}

    def set_path_edit_mode(self, enabled: bool): #vers 1
        """Toggle click-to-select-and-drag path node editing (Aug 17
        2026, per Keith: "lets address the unbuilt work, editing
        paths first"). While on, a left-click near a rendered path
        node picks it up and drags it along the ground plane at that
        node's own height (not free in 3D - a 2D mouse drag can't
        unambiguously set 3 coordinates at once, and path nodes are
        ground-level positions by nature, so constraining to height-
        preserving horizontal movement is the actually useful
        behaviour, not a limitation) until release, which commits the
        new position to the real PathGroup/PathNode data via
        set_path_node_drag_callback."""
        self._path_edit_mode = enabled
        if not enabled:
            self._dragging_path_node_start_key = None
            self._dragging_path_node_current_pos = None
        self.update()

    def set_box_edit_mode(self, enabled: bool): #vers 1
        """Toggle click-to-select-and-drag box corner resizing (Aug
        19 2026, per Keith's own priority order - box resize is the
        real prerequisite for no-clip box editing, see the fuller
        explanation where self._box_edit_mode is first declared in
        __init__). While on, a left-click near a rendered cull/zone
        box corner picks it up and drags it along the ground plane at
        that corner's own height (not free in 3D - same "2D drag,
        height-preserving" reasoning as path node editing) until
        release, which commits the new corner position - and thus the
        box's own new size - to the real CullEntry/zone dict via
        set_box_resize_callback. The diagonally opposite corner (both
        in XY and in Z) stays fixed throughout the drag, the same way
        dragging one corner of a selection rectangle keeps the
        opposite corner anchored."""
        self._box_edit_mode = enabled
        if not enabled:
            self._dragging_box_corner_key = None
            self._dragging_box_corner_info = None
            self._dragging_box_corner_current_pos = None
        self.update()

    def set_box_resize_callback(self, callback): #vers 1
        """Set (or clear, with None) the function called when a box
        corner drag completes: callback(box_type, box_ref, new_x1,
        new_y1, new_x2, new_y2). map_workshop.py applies this to the
        real, live CullEntry/zone dict and triggers a refresh -
        mirrors set_path_node_drag_callback's own widget-owns-
        interaction, caller-owns-data split exactly."""
        self._box_resize_callback = callback

    def set_no_clip_boxes(self, enabled: bool): #vers 1
        """Toggle no-clip during box resizing (Aug 19 2026, per
        Keith: "have a no-clipping option where you can't move one
        box into another"). When on, a resize that would make the
        dragged box's own new extents overlap any OTHER currently-
        loaded cull/zone box is simply rejected for that mouse-move -
        the box holds its last known-good, non-overlapping size
        instead of jumping to the new, colliding one, rather than
        attempting to compute some "closest non-overlapping size"
        automatically (a harder geometric problem, especially with
        several other boxes potentially blocking from different
        directions at once, that a rushed first version could easily
        get subtly wrong in a way that's worse than simply holding
        still). See _box_resize_would_overlap's own docstring for the
        actual AABB overlap test. Off by default - most resizing
        genuinely doesn't need this, and the check adds a real per-
        mouse-move cost testing against every other loaded box."""
        self._no_clip_boxes = enabled

    def set_path_node_drag_callback(self, callback): #vers 1
        """Set (or clear, with None) the function called when a path
        node drag completes: callback(group_ref, node_index, new_x,
        new_y, new_z). map_workshop.py wires this once to a method
        that mutates the real PathNode and refreshes - mirrors the
        existing set_lod_test_callback pattern (a widget-owns-
        interaction, caller-owns-data split already established
        elsewhere in this file, not a new convention)."""
        self._path_node_drag_callback = callback

    def set_ipl_drag_mode(self, enabled: bool): #vers 1
        """Toggle click-drag whole-IPL-section moving (Aug 18 2026,
        per Keith's own priority order for the interactive editing
        layer). While on, clicking any instance belonging to a loaded
        IPL and dragging moves that IPL's entire data as one rigid
        body, along the ground at the clicked instance's own starting
        height (same "2D drag, height-preserving" reasoning as path
        node editing - see set_path_edit_mode's own docstring) until
        release, which commits the final offset via set_ipl_drag_
        callback."""
        self._ipl_drag_mode = enabled
        if not enabled:
            self._dragging_ipl_names = set()
            self._dragging_ipl_start_state = []
            self._dragging_ipl_ground_start = None
            self._dragging_ipl_clicked_inst = None
            self._dragging_ipl_clicked_start_pos = None
            self._multi_selected_ipl_names = set()
        self.update()

    def set_ipl_drag_callback(self, callback): #vers 1
        """Set (or clear, with None) the function called when a whole-
        IPL drag completes: callback(ipl_name, dx, dy, dz). map_
        workshop.py wires this once to a method that calls the
        already-existing _shift_ipl_coordinates (the same one the
        dialog-based Shift Coordinates tool already uses) - mirrors
        set_path_node_drag_callback/set_lod_test_callback's own
        widget-owns-interaction, caller-owns-data split."""
        self._ipl_drag_callback = callback

    def set_ipl_drag_axis_lock(self, axis): #vers 1
        """Set the axis-lock mode for whole-IPL dragging (Aug 18
        2026, per Keith: "[Drag ipl] right-click options, like lock
        z, only move x, y"). axis is None (free X/Y - the existing
        default), 'x' (lock X - only Y actually moves), or 'y' (lock
        Y - only X moves). Z is already always effectively locked by
        the ground-plane-constrained drag design itself, independent
        of this setting - see the fuller explanation where self.
        _ipl_drag_axis_lock is first declared in __init__."""
        self._ipl_drag_axis_lock = axis if axis in ('x', 'y') else None

    def set_ipl_interaction_mode(self, mode): #vers 1
        """Set which of the 3-state Drag/Move/Rotate cycle is active
        for whole-IPL interaction (Aug 19 2026, per Keith's own
        priority order for the interactive editing layer). See the
        fuller explanation where self._ipl_interaction_mode is first
        declared in __init__ for exactly how each mode changes what a
        click does. Falls back to 'drag' for anything unrecognised."""
        self._ipl_interaction_mode = mode if mode in ('drag', 'move', 'rotate') else 'drag'

    def set_ipl_click_callback(self, callback): #vers 1
        """Set (or clear, with None) the function called when an
        instance is picked while in Move or Rotate mode: callback(
        ipl_name). Never called in Drag mode - that one still goes
        through the existing set_ipl_drag_callback on release instead.
        map_workshop.py wires this once to a method that opens the
        corresponding numeric dialog - mirrors every other widget-
        owns-interaction, caller-owns-data callback in this file."""
        self._ipl_click_callback = callback

    def set_ipl_selection_callback(self, callback): #vers 1
        """Set (or clear, with None) the function called whenever the
        Shift+click multi-IPL selection changes: callback(set_of_ipl_
        names) - see the fuller multi-select workflow explanation
        where self._multi_selected_ipl_names is first declared in
        __init__. map_workshop.py wires this once to a method that
        shows the current selection in the status bar - mirrors every
        other widget-owns-interaction, caller-owns-data callback in
        this file."""
        self._ipl_selection_callback = callback

    def set_multi_selected_ipl_names(self, names): #vers 1
        """Directly set the current multi-IPL selection from outside
        (Aug 19 2026, per Keith: "Shift + left-click selects the
        entire .ipls in the Object Browser" - a second, list-based way
        to build the same selection this viewport's own Shift+click-
        on-an-instance gesture builds, so map_workshop.py can sync
        whichever rows are selected in the IPL Sections table into
        this same underlying set). Both selection mechanisms feed the
        one set - a Ctrl+drag or plain click-drag in the viewport
        picks up whatever's currently selected regardless of which of
        the two ways it was actually selected."""
        self._multi_selected_ipl_names = set(names) if names else set()
        self.update()

    def set_hover_highlight_enabled(self, enabled: bool): #vers 1
        """Toggle auto-highlight-on-hover (Aug 19 2026, per Keith's
        own request - see the fuller explanation where self._hover_
        highlight_enabled is first declared in __init__). Clears any
        currently-hovered instance when turned off, so a stale
        highlight can't linger on screen after the feature itself is
        disabled."""
        self._hover_highlight_enabled = enabled
        if not enabled:
            self._hovered_instance_idx = None
        self.update()

    def set_hover_context_callback(self, callback): #vers 1
        """Set (or clear, with None) the function called on a right-
        click while something is currently hovered: callback(inst).
        map_workshop.py wires this once to open a context menu for
        that instance - mirrors every other widget-owns-interaction,
        caller-owns-data callback in this file."""
        self._hover_context_callback = callback

    def set_show_tracks(self, enabled: bool): #vers 1
        self.show_tracks = enabled; self.update()

    def set_track_polylines(self, polylines): #vers 1
        """Replace the track data drawn when show_tracks is on. Each
        entry is an ordered list of (x,y,z) waypoints forming one
        continuous track (Aug 17 2026) - conversion from TrackWaypoint
        (gta_dat_parser.py's parsed loader.tracks) happens in map_
        workshop.py, same separation as every other overlay in this
        widget (no TrackWaypoint dataclass import here)."""
        self._track_polylines = polylines or []
        self.update()

    def set_track_color(self, r: float, g: float, b: float): #vers 1
        self._track_color = (r, g, b)
        self.update()

    def _draw_tracks(self): #vers 1
        """Draw every loaded train track as one continuous line strip
        per track (Aug 17 2026) - simpler than _draw_paths, since real
        tracks.dat/tracks2.dat data confirmed this is genuinely just
        an ordered waypoint sequence, no node-type/Next-index graph to
        resolve. No node markers - a train track has no meaningful
        "node" concept the way a vehicle/ped path does; the polyline
        itself is the whole picture."""
        if not OPENGL_AVAILABLE or not self._track_polylines: return
        glDisable(GL_LIGHTING)
        glDisable(GL_DEPTH_TEST)
        r, g, b = self._track_color
        glColor3f(r, g, b)
        glLineWidth(self._track_line_thickness)
        for polyline in self._track_polylines:
            if len(polyline) < 2:
                continue
            glBegin(GL_LINE_STRIP)
            for x, y, z in polyline:
                glVertex3f(x, y, z)
            glEnd()
        glEnable(GL_DEPTH_TEST)
        glEnable(GL_LIGHTING)

    def set_track_line_thickness(self, thickness): #vers 1
        self._track_line_thickness = thickness; self.update()

    def set_airtrain_color(self, r, g, b): #vers 1
        self._airtrain_color = (r, g, b); self.update()

    def set_airtrain_line_thickness(self, thickness): #vers 1
        self._airtrain_line_thickness = thickness; self.update()

    def set_show_sa_nodes(self, enabled: bool): #vers 1
        self.show_sa_nodes = enabled; self.update()

    def set_sa_node_segments(self, segments): #vers 1
        """Replace the SA path-node graph data drawn when show_sa_
        nodes is on. Each entry is a plain ((x1,y1,z1),(x2,y2,z2))
        segment pair - one per real link between two nodes (Aug 19
        2026, per Keith's real NODES0-63.DAT data). Resolution (each
        link's own target node position, including across different
        area files) happens in map_workshop.py - this widget never
        imports SAPathNode/SAPathLink/SAPathFile, matching every
        other overlay's own plain-data split."""
        self._sa_node_segments = segments or []
        self.update()

    def set_sa_node_color(self, r: float, g: float, b: float): #vers 1
        self._sa_node_color = (r, g, b)
        self.update()

    def set_show_auzo_zones(self, enabled: bool): #vers 1
        self.show_auzo_zones = enabled; self.update()

    def set_auzo_zones(self, zones): #vers 1
        """Replace the audio zone data drawn when show_auzo_zones is
        on. Each entry is a plain (center_x, center_y, center_z, name,
        sound_id, environment_type, music_description) tuple -
        conversion from the real AuzoEntry dataclass (cube-vs-sphere
        center resolution, AUZO_TYPES lookup) happens in map_
        workshop.py's own refresh method, this widget only ever deals
        in plain, already-resolved data."""
        self._auzo_zones = zones or []
        self.update()

    def _ensure_auzo_icon_texture(self): #vers 1
        """Lazily load a sound-icon SVG into a real OpenGL texture the
        first time an audio zone actually needs to be drawn, caching
        the result so this only ever happens once per session (Aug 20
        2026, per Keith: "audiozone placements with sound svg
        icons"). Reuses the app's own already-proven SVG-to-QPixmap
        pipeline (apps/components/Map_Editor/depends/svg_icon_factory.
        py's own volume_up_icon/_create_icon, the exact same QSvgRenderer
        + QPixmap + QPainter approach already used for every other SVG
        icon in this app) rather than building a second, separate SVG
        rendering path - converts the resulting QPixmap to raw RGBA
        bytes via QImage, then uploads it the same way real model
        textures already are (same glGenTextures/glTexImage2D calls,
        same _tex_ids-style single-entry cache pattern, just keyed
        under its own reserved name rather than a real model texture
        name, so it can never collide with one).

        Local import here, not at this module's own top level - the
        SVG icon factory lives under apps/components/Map_Editor/,
        and this is a shared apps/methods/ module; importing a
        components/-level module at the top of a methods/ module
        would point the dependency the wrong way round. Only a caller
        that actually needs the sound icon (i.e., Show Auzo Zones
        actually turned on) pays this import's own cost."""
        if self._auzo_icon_tex_id is not None:
            return self._auzo_icon_tex_id
        try:
            from apps.components.Map_Editor.depends.svg_icon_factory import SVGIconFactory
            from PyQt6.QtGui import QImage
            icon = SVGIconFactory.volume_up_icon(size=64, color='#ffcc33')
            pixmap = icon.pixmap(64, 64)
            image = pixmap.toImage().convertToFormat(QImage.Format.Format_RGBA8888)
            w, h = image.width(), image.height()
            ptr = image.bits()
            ptr.setsize(image.sizeInBytes())
            rgba = bytes(ptr)
            gl_id = glGenTextures(1)
            glBindTexture(GL_TEXTURE_2D, gl_id)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
            glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, rgba)
            glBindTexture(GL_TEXTURE_2D, 0)
            self._auzo_icon_tex_id = gl_id
        except Exception as e:
            print(f"[DFFViewport] Failed to load auzo sound icon texture: {e}")
            self._auzo_icon_tex_id = False   # tried and failed - don't keep retrying every frame
        return self._auzo_icon_tex_id

    def _draw_auzo_zones(self): #vers 1
        """Draw a billboarded (always facing the camera) sound-icon
        quad at each real audio zone's own center position (Aug 20
        2026, per Keith: "audiozone placements with sound svg icons").
        Billboard orientation extracted directly from the current
        modelview matrix's own right/up basis vectors (the standard,
        general technique for "always face the camera" billboards,
        correct regardless of the camera's current rotation/tilt,
        unlike a simpler "always upright, only yaw" approximation)."""
        if not OPENGL_AVAILABLE or not self._auzo_zones:
            return
        tex_id = self._ensure_auzo_icon_texture()
        if not tex_id:
            return
        mv = glGetFloatv(GL_MODELVIEW_MATRIX)
        # Column-major: right = row 0 of the rotation part, up = row 1
        right = (mv[0][0], mv[1][0], mv[2][0])
        up    = (mv[0][1], mv[1][1], mv[2][1])
        half = 1.5   # world-unit half-size of the billboard quad
        glDisable(GL_LIGHTING)
        glEnable(GL_TEXTURE_2D)
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
        glBindTexture(GL_TEXTURE_2D, tex_id)
        glColor4f(1.0, 1.0, 1.0, 1.0)
        for cx, cy, cz, name, sound_id, env_type, music in self._auzo_zones:
            corners = []
            for su, sv in ((-1, -1), (1, -1), (1, 1), (-1, 1)):
                corners.append((
                    cx + (right[0]*su + up[0]*sv) * half,
                    cy + (right[1]*su + up[1]*sv) * half,
                    cz + (right[2]*su + up[2]*sv) * half,
                ))
            glBegin(GL_QUADS)
            glTexCoord2f(0, 1); glVertex3f(*corners[0])
            glTexCoord2f(1, 1); glVertex3f(*corners[1])
            glTexCoord2f(1, 0); glVertex3f(*corners[2])
            glTexCoord2f(0, 0); glVertex3f(*corners[3])
            glEnd()
        glBindTexture(GL_TEXTURE_2D, 0)
        glDisable(GL_BLEND)
        glDisable(GL_TEXTURE_2D)
        glEnable(GL_LIGHTING)

    def set_show_water(self, enabled: bool): #vers 1
        self.show_water = enabled; self.update()

    def set_water_shapes(self, shapes): #vers 1
        """Replace the water shape data drawn when show_water is on.
        Each entry is a plain (corners, water_type) tuple - corners a
        list of 3 or 4 (x,y,z) tuples, water_type the real 0-3 flag
        value (see WaterShape's own docstring in gta_dat_parser.py).
        Resolution from the real WaterShape/WaterCorner dataclasses
        happens in map_workshop.py - this widget only ever deals in
        plain, already-resolved data."""
        self._water_shapes = shapes or []
        self.update()

    def _draw_water_shapes(self): #vers 2
        """Draw real water.dat shapes as flat, translucent polygons
        (Aug 20 2026, per Keith: "lets get all the functions in" -
        water/radar recalculation on map moves). A real water shape
        genuinely is a flat plane (3 or 4 corners, see WaterShape's
        own docstring), not a volume - drawn as GL_TRIANGLE_FAN
        (correct for both a triangle and a quad, unlike GL_QUADS
        which would only handle the 4-corner case) rather than a
        wireframe box the way cull/zone/occlusion boxes already are,
        since a box would misrepresent the real shape entirely, not
        just look different.

        Honest, real uncertainty checked directly rather than assumed
        away: fanning from corners[0] is a reasonable, standard choice
        for turning an arbitrary quad into 2 triangles, but checking
        it against real example data from the same documentation this
        format was confirmed from (see WaterShape's own docstring)
        found at least one real 4-corner line whose own corner order,
        connected edge-to-edge in sequence, is a self-intersecting
        "bowtie" shape (confirmed via the shoelace formula - zero net
        area) rather than a simple, convex quad - the documented "NE-
        NW-SE-SW" corner order doesn't hold for every real line found.
        Fanning from corners[0] still produces a valid, non-crossing
        pair of triangles regardless (it never assumes a particular
        edge-walk order the way a naive "just connect them in
        sequence" approach would), and is a reasonable approximation
        for an editing aid showing roughly where water is - but this
        is NOT a confirmed-exact match to whatever specific
        triangulation the real game engine itself uses internally for
        every possible real corner ordering, and that distinction is
        worth remembering if a specific shape's own polygon fill ever
        looks visually wrong for its real corner data.

        Colour distinguishes real water type at a glance rather than
        one flat colour for everything - deep blue for ocean
        (is_shallow False, real infinite depth per the documented
        format), lighter cyan-ish for a pool (is_shallow True, real
        6-unit depth) - a real, meaningful distinction in the data
        itself, not an arbitrary choice. Genuinely invisible water
        (is_visible False - real, valid, intentional per the
        documented format, used so a modder's own custom/animated
        texture can show through) still gets drawn here at a lower
        alpha rather than skipped entirely - this is an editing aid
        showing where water actually IS, not a simulation of what a
        player would see in-game."""
        if not OPENGL_AVAILABLE or not self._water_shapes:
            return
        glDisable(GL_LIGHTING)
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
        glDisable(GL_CULL_FACE)
        # Depth writes off, test still on (Aug 20 2026, per Keith:
        # "when water is being rendered, don't render over loaded IPL
        # models") - GL_DEPTH_TEST was already enabled globally
        # (initializeGL) and nothing before this in paintGL's own
        # draw order disables it without restoring, so opaque model
        # geometry drawn earlier already correctly occludes water
        # behind it. The real, standard gap for any semi-transparent
        # surface like this is depth writes, not the test itself -
        # leaving them on (the default) means this flat water plane's
        # own depth values could interfere with other transparent
        # overlays drawn after it. Read depth, don't write it - the
        # correct, standard pattern for translucent geometry.
        glDepthMask(GL_FALSE)
        for corners, water_type in self._water_shapes:
            is_shallow = bool(water_type & 2)
            is_visible = bool(water_type & 1)
            if is_shallow:
                r, g, b = 0.3, 0.75, 0.85   # pool - lighter cyan
            else:
                r, g, b = 0.1, 0.25, 0.65   # ocean - deep blue
            alpha = 0.45 if is_visible else 0.2
            glColor4f(r, g, b, alpha)
            glBegin(GL_TRIANGLE_FAN)
            for cx, cy, cz in corners:
                glVertex3f(cx, cy, cz)
            glEnd()
            glColor4f(r, g, b, min(alpha + 0.35, 1.0))
            glLineWidth(1.2)
            glBegin(GL_LINE_LOOP)
            for cx, cy, cz in corners:
                glVertex3f(cx, cy, cz)
            glEnd()
        glEnable(GL_CULL_FACE)
        glDepthMask(GL_TRUE)
        glDisable(GL_BLEND)
        glEnable(GL_LIGHTING)

    def set_waterpro_cells(self, cells): #vers 1
        """Replace the waterpro.dat grid drawn when show_water is on
        (Aug 20 2026, per Keith: "water function should also load the
        waterpro.dat, and display it in the same way water_workshop
        works"). Each entry is a plain (min_x, min_y, max_x, max_y,
        height) tuple - one real, already-resolved cell of the grid's
        own visible_map, per WaterProFile/WaterProLevel in gta_dat_
        parser.py. Resolution (grid cell -> real world bounds, level
        index -> real height) happens in map_workshop.py, same real
        "widget only ever draws plain, already-resolved data" split
        every other overlay here already follows - this widget never
        touches WaterProFile/gta_dat_parser.py directly."""
        self._waterpro_cells = cells or []
        self.update()

    def set_water_display_style(self, style): #vers 1
        valid = ('fill', 'lines', 'dots', 'hexagons', 'texture')
        self._water_display_style = style if style in valid else 'fill'
        self.update()

    def set_water_texture(self, path, tile_size=None): #vers 1
        if path != self._water_texture_path:
            self._water_texture_path = path or ''
            self._water_texture_tex_id = None
        if tile_size is not None:
            self._water_tile_size = tile_size
        self.update()

    def set_water_hide_outside_map(self, hide): #vers 1
        self._water_hide_outside_map = bool(hide)
        self.update()

    def set_water_map_extent(self, half_extent): #vers 1
        self._water_map_half_extent = half_extent
        self.update()

    def _draw_waterpro_water(self): #vers 2
        """Draw waterpro.dat's own real grid, one per real grid cell,
        at that cell's own real height (Aug 20 2026). Style now
        configurable (Aug 20 2026, per Keith: "Maybe show water
        should be in lines, dots, hexagons, with the water file
        path... another entry for custom textures to be shown instead
        of the grid") - 'fill' (the original, flat translucent quad,
        same real deep-blue colour/alpha water.dat's own ocean water
        already uses since waterpro.dat has no shallow/pool
        distinction of its own to draw a second colour from), 'lines'
        (outline only), 'dots' (a single point per cell centre),
        'hexagons' (a hexagon outline approximating each cell's own
        area, reusing the same real hex-tiling math the grid's own
        honeycomb style already uses), or a user-chosen texture tiled
        across the cells the same way the grid's own squares-texture
        fill already works.

        "Hide outside map boundary" (Aug 20 2026) - skips any cell
        whose own centre falls outside self._water_map_half_extent,
        when that setting is on."""
        if not OPENGL_AVAILABLE or not self._waterpro_cells:
            return
        cells = self._waterpro_cells
        if self._water_hide_outside_map:
            half = self._water_map_half_extent
            cells = [c for c in cells
                     if abs((c[0] + c[2]) / 2) <= half and abs((c[1] + c[3]) / 2) <= half]
            if not cells:
                return
        glDisable(GL_LIGHTING)
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
        # Depth writes off, test still on (Aug 20 2026, per Keith:
        # "when water is being rendered, don't render over loaded IPL
        # models") - same real reasoning _draw_water_shapes' own
        # docstring gives for this exact same fix.
        glDepthMask(GL_FALSE)
        style = self._water_display_style

        if style == 'texture' and self._water_texture_path:
            tex_id = self._ensure_water_texture()
            if tex_id:
                glEnable(GL_TEXTURE_2D)
                glBindTexture(GL_TEXTURE_2D, tex_id)
                glColor4f(1, 1, 1, 0.6)
                ts = self._water_tile_size
                glBegin(GL_QUADS)
                for min_x, min_y, max_x, max_y, height in cells:
                    glTexCoord2f(min_x / ts, min_y / ts); glVertex3f(min_x, min_y, height)
                    glTexCoord2f(max_x / ts, min_y / ts); glVertex3f(max_x, min_y, height)
                    glTexCoord2f(max_x / ts, max_y / ts); glVertex3f(max_x, max_y, height)
                    glTexCoord2f(min_x / ts, max_y / ts); glVertex3f(min_x, max_y, height)
                glEnd()
                glBindTexture(GL_TEXTURE_2D, 0)
                glDisable(GL_TEXTURE_2D)
                glDepthMask(GL_TRUE)
                glDisable(GL_BLEND)
                glEnable(GL_LIGHTING)
                return

        glColor4f(0.1, 0.25, 0.65, 0.45)
        if style == 'fill':
            glBegin(GL_QUADS)
            for min_x, min_y, max_x, max_y, height in cells:
                glVertex3f(min_x, min_y, height)
                glVertex3f(max_x, min_y, height)
                glVertex3f(max_x, max_y, height)
                glVertex3f(min_x, max_y, height)
            glEnd()
        elif style == 'lines':
            glLineWidth(1.2)
            for min_x, min_y, max_x, max_y, height in cells:
                glBegin(GL_LINE_LOOP)
                glVertex3f(min_x, min_y, height)
                glVertex3f(max_x, min_y, height)
                glVertex3f(max_x, max_y, height)
                glVertex3f(min_x, max_y, height)
                glEnd()
        elif style == 'dots':
            glPointSize(4.0)
            glBegin(GL_POINTS)
            for min_x, min_y, max_x, max_y, height in cells:
                glVertex3f((min_x + max_x) / 2, (min_y + max_y) / 2, height)
            glEnd()
        elif style == 'hexagons':
            import math
            for min_x, min_y, max_x, max_y, height in cells:
                cx, cy = (min_x + max_x) / 2, (min_y + max_y) / 2
                s = (max_x - min_x) / 2
                glBegin(GL_LINE_LOOP)
                for i in range(6):
                    ang = math.radians(60 * i - 30)
                    glVertex3f(cx + s * math.cos(ang), cy + s * math.sin(ang), height)
                glEnd()
        glDepthMask(GL_TRUE)
        glDisable(GL_BLEND)
        glEnable(GL_LIGHTING)

    def _ensure_water_texture(self): #vers 1
        """Lazily load self._water_texture_path as a GL texture, same
        real pattern _ensure_squares_texture already uses - no self.
        makeCurrent()/doneCurrent() here since this is always called
        from within an already-active paintGL, same real reasoning
        that fixed the earlier real segfault (Aug 20 2026)."""
        if self._water_texture_tex_id and self._water_texture_tex_path_loaded == self._water_texture_path:
            return self._water_texture_tex_id
        try:
            from PyQt6.QtGui import QImage
            image = QImage(self._water_texture_path).convertToFormat(QImage.Format.Format_RGBA8888)
            if image.isNull():
                self._water_texture_tex_id = False
                return False
            w, h = image.width(), image.height()
            ptr = image.bits(); ptr.setsize(image.sizeInBytes())
            rgba = bytes(ptr)
            if self._water_texture_tex_id and self._water_texture_tex_id is not False:
                glDeleteTextures([self._water_texture_tex_id])
            gl_id = glGenTextures(1)
            glBindTexture(GL_TEXTURE_2D, gl_id)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
            glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, rgba)
            glBindTexture(GL_TEXTURE_2D, 0)
            self._water_texture_tex_id = gl_id
            self._water_texture_tex_path_loaded = self._water_texture_path
        except Exception as e:
            print(f"[DFFViewport] Failed to load water texture: {e}")
            self._water_texture_tex_id = False
        return self._water_texture_tex_id

    def _draw_sa_nodes(self): #vers 1
        """Draw SA's real vehicle/ped path node graph as disconnected
        line segments (Aug 19 2026, per Keith's real NODES0-63.DAT
        data - "lets do those next") - genuinely different from
        _draw_tracks' own single-continuous-strip-per-file approach:
        this is a real graph (nodes can have more than 2 links, and
        links aren't necessarily chained in any particular order), not
        an ordered sequence, so GL_LINES (independent segment pairs)
        is the correct primitive here, not GL_LINE_STRIP. No node
        markers for this first version, matching _draw_tracks' own
        reasoning - the segments themselves already show every real
        node's own position as a line endpoint."""
        if not OPENGL_AVAILABLE or not self._sa_node_segments: return
        glDisable(GL_LIGHTING)
        glDisable(GL_DEPTH_TEST)
        r, g, b = self._sa_node_color
        glColor3f(r, g, b)
        glLineWidth(1.0)
        glBegin(GL_LINES)
        for (x1, y1, z1), (x2, y2, z2) in self._sa_node_segments:
            glVertex3f(x1, y1, z1)
            glVertex3f(x2, y2, z2)
        glEnd()
        glEnable(GL_DEPTH_TEST)
        glEnable(GL_LIGHTING)

    def _draw_hover_highlight(self): #vers 1
        """Draw a marker around whatever instance is currently hovered
        (Aug 19 2026, per Keith: "Auto object highlight setting in
        map_workshop settings: this could be a model, path node,
        anything in the viewpoint; once highlighted, right-click for
        options" - the visual half of that request; scoped to
        instances only for this first version, see the fuller
        explanation where self._hover_highlight_enabled is first
        declared in __init__ for why path nodes aren't included yet).

        A bright, semi-transparent wireframe sphere at the instance's
        own position, reusing the exact same gluSphere technique and
        lazily-created-once quadric object already proven for the
        cull/zone/occlusion box corner handles - a genuinely simple,
        cheap indicator rather than a full model-shaped outline, which
        would need this instance's own loaded geometry/bounding box,
        not otherwise needed just to show "this one is hovered"."""
        idx = getattr(self, '_hovered_instance_idx', None)
        if idx is None or idx >= len(self._world_instances):
            return
        entry = self._world_instances[idx]
        x, y, z = entry['pos']
        quadric = getattr(self, '_corner_sphere_quadric', None)
        if quadric is None:
            quadric = gluNewQuadric()
            self._corner_sphere_quadric = quadric
        glDisable(GL_LIGHTING)
        glDisable(GL_DEPTH_TEST)
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
        glColor4f(1.0, 1.0, 0.2, 0.55)
        glPushMatrix()
        glTranslatef(x, y, z)
        gluSphere(quadric, 1.5, 10, 8)
        glPopMatrix()
        glDisable(GL_BLEND)
        glEnable(GL_DEPTH_TEST)
        glEnable(GL_LIGHTING)

    def _pick_path_node(self, mx: float, my: float): #vers 1
        """Return the (x,y,z) position of the closest path node to
        the ray through (mx,my), within a small screen-space-
        equivalent tolerance, or None - same pattern as _pick_vertex/
        _pick_world_instance just above (reuses the exact same _pick_
        ray/_closest_point_on_ray infrastructure), testing against
        self._path_node_owner_map's own keys rather than mesh
        vertices or instance positions."""
        ray = self._pick_ray(mx, my)
        if ray is None or not self._path_node_owner_map:
            return None
        origin, direction = ray
        tol2 = (self._dist * 0.02) ** 2
        best_pos, best_t, best_d2 = None, None, tol2
        for pos in self._path_node_owner_map.keys():
            t, d2 = self._closest_point_on_ray(origin, direction, pos)
            if t < 0:
                continue
            if d2 < best_d2 or (best_pos is not None and d2 <= best_d2 and t < best_t):
                best_pos, best_t, best_d2 = pos, t, d2
        return best_pos

    def _pick_box_corner(self, mx: float, my: float): #vers 1
        """Return the key (into self._pickable_box_corners) of the
        closest box corner to the ray through (mx,my), within a small
        screen-space-equivalent tolerance, or None (Aug 19 2026, for
        box-corner resizing) - same _pick_ray/_closest_point_on_ray
        pattern _pick_path_node just above already uses, testing
        against each entry's own 'pos' value rather than the dict's
        keys directly (a box corner's own identity is the composite
        (box_type, box_index, corner_index, z) key itself, not its
        position - unlike path nodes, where position IS the key)."""
        ray = self._pick_ray(mx, my)
        if ray is None or not self._pickable_box_corners:
            return None
        origin, direction = ray
        tol2 = (self._dist * 0.02) ** 2
        best_key, best_t, best_d2 = None, None, tol2
        for key, info in self._pickable_box_corners.items():
            t, d2 = self._closest_point_on_ray(origin, direction, info['pos'])
            if t < 0:
                continue
            if d2 < best_d2 or (best_key is not None and d2 <= best_d2 and t < best_t):
                best_key, best_t, best_d2 = key, t, d2
        return best_key

    def _box_resize_would_overlap(self, box_type, box_index, x1, y1, x2, y2, z1, z2): #vers 1
        """Standard AABB-vs-AABB overlap test: would a box with these
        new proposed extents intersect any OTHER currently-loaded
        cull or zone box (Aug 19 2026, for Snap: No-Clip - see set_
        no_clip_boxes' own docstring for the full behaviour this
        gates). Checks against both self._cull_boxes and self._zone_
        boxes together, not just same-type boxes - Keith's own
        wording ("you can't move one box into another") wasn't scoped
        to same-type collisions only, and there's no real reason a
        cull box overlapping a zone box would be any less of "a mess"
        than two cull boxes overlapping each other. Excludes only the
        specific (box_type, box_index) actually being resized, so a
        box is never considered to be overlapping itself.

        Strict inequalities (< / >, not <=/>=) - boxes that merely
        touch edge-to-edge with zero actual overlap volume are not
        considered colliding, only boxes that genuinely intersect in
        3D space are."""
        def overlaps(other):
            ox1, oy1, oz1, ox2, oy2, oz2 = other
            return (x1 < ox2 and x2 > ox1 and
                    y1 < oy2 and y2 > oy1 and
                    z1 < oz2 and z2 > oz1)

        for i, box in enumerate(self._cull_boxes):
            if box_type == 'cull' and i == box_index:
                continue
            if overlaps(box):
                return True
        for i, box in enumerate(self._zone_boxes):
            if box_type == 'zone' and i == box_index:
                continue
            if overlaps(box):
                return True
        return False

    def set_path_line_color(self, r: float, g: float, b: float): #vers 1
        """Aug 14 2026, per Keith: "a way to change the colour of the
        path lines in settings" - r/g/b as 0-1 floats, matching every
        other colour this widget already works in (glColor3f etc)."""
        self._path_line_color = (r, g, b)
        self.update()

    def set_path_node_color(self, r: float, g: float, b: float): #vers 1
        """Aug 16 2026, per Keith: "...and color change option" -
        node markers were previously a fixed amber with no way to
        change them; now configurable the same way line colour
        already was."""
        self._path_node_color = (r, g, b)
        self.update()

    def set_path_line_thickness(self, px: float): #vers 1
        """Aug 16 2026, per Keith: "under rander in settings, line
        thinkness..." - px is the raw glLineWidth value."""
        self._path_line_thickness = max(0.1, px)
        self.update()

    def set_path_node_size(self, px: float): #vers 1
        """Aug 16 2026, per Keith: "...and node circle size..." - px
        is the raw glPointSize value."""
        self._path_node_size = max(0.1, px)
        self.update()

    def set_show_cull_boxes(self, enabled: bool): #vers 1
        self.show_cull_boxes = enabled; self.update()

    def set_cull_boxes(self, boxes): #vers 1
        """Replace the cull-zone boxes drawn when show_cull_boxes is
        on. Each entry is a plain (x1,y1,z1,x2,y2,z2) corner-pair
        tuple - conversion from the real CullEntry dataclass happens
        in map_workshop.py's _refresh_cull_box_visualization, this
        widget only ever deals in plain coordinates."""
        self._cull_boxes = boxes or []
        self.update()

    def set_cull_box_owners(self, owners): #vers 1
        """Set the real CullEntry object for each entry in self.
        _cull_boxes, same index/order (Aug 19 2026, for box-corner
        resizing - see the fuller explanation where self._pickable_
        box_corners is first declared in __init__). Mirrors set_path_
        node_owners' own already-proven pattern: this widget still
        never imports CullEntry itself or deals in it directly for
        drawing - the plain-tuple set_cull_boxes call is completely
        unchanged - this is purely a parallel identity list so a
        resize commit can be applied to the real, live object it
        actually came from."""
        self._cull_box_owners = owners or []

    def set_cull_box_color(self, r: float, g: float, b: float): #vers 1
        self._cull_box_color = (r, g, b)
        self.update()

    def set_show_zone_boxes(self, enabled: bool): #vers 1
        self.show_zone_boxes = enabled; self.update()

    def set_zone_boxes(self, boxes): #vers 1
        """Replace the map-zone boxes drawn when show_zone_boxes is
        on. Each entry is a plain (x1,y1,z1,x2,y2,z2) corner-pair
        tuple - conversion from the parsed zone dict (Name/Type/
        Min/Max/Island fields) happens in map_workshop.py's
        _refresh_zone_box_visualization, this widget only ever deals
        in plain coordinates."""
        self._zone_boxes = boxes or []
        self.update()

    def set_zone_box_owners(self, owners): #vers 1
        """Set the real zone dict for each entry in self._zone_boxes,
        same index/order (Aug 19 2026) - mirrors set_cull_box_owners'
        own docstring exactly, just for zones' own plain-dict
        representation instead of a CullEntry dataclass."""
        self._zone_box_owners = owners or []

    def set_zone_box_color(self, r: float, g: float, b: float): #vers 1
        self._zone_box_color = (r, g, b)
        self.update()

    def set_zone_render_style(self, style: str): #vers 1
        """Aug 16 2026, per Keith: "in zons, the render dropdown
        could show, Zon - Ghosted, Zon - Wireframe, Zon -
        translucent" - style is one of 'ghosted'/'wireframe'/
        'translucent', see _draw_zone_boxes for what each looks
        like. Falls back to 'ghosted' for an unrecognised value
        rather than silently drawing nothing."""
        self._zone_render_style = style if style in (
            'ghosted', 'wireframe', 'translucent') else 'ghosted'
        self.update()

    def set_show_occl_boxes(self, enabled: bool): #vers 1
        self.show_occl_boxes = enabled; self.update()

    def set_occl_boxes(self, boxes): #vers 1
        """Replace the occlusion-zone boxes drawn when show_occl_
        boxes is on. Each entry is a plain (mid_x, mid_y, bottom_z,
        width_x, width_y, height, rotation) tuple - conversion from
        the real OcclEntry dataclass happens in map_workshop.py's
        _refresh_occl_box_visualization, this widget only ever deals
        in plain coordinates."""
        self._occl_boxes = boxes or []
        self.update()

    def set_occl_box_color(self, r: float, g: float, b: float): #vers 1
        self._occl_box_color = (r, g, b)
        self.update()

    def set_box_axis_colors(self, enabled: bool): #vers 1
        """Toggle axis-colored box faces (Aug 18 2026, per Keith's own
        request - see _draw_ghosted_box_from_corners's own docstring
        for the full colour scheme). Overrides each box type's own
        configured colour when on, for every cull/zone/occlusion box
        at once - not a per-type setting, since the whole point is a
        consistent way to read orientation regardless of which box
        type is being edited."""
        self._box_axis_colors = enabled
        self.update()

    def set_box_unique_colors(self, enabled: bool): #vers 1
        """Toggle unique-colour-per-box (Aug 19 2026, per Keith's own
        request - see the fuller explanation where self._box_unique_
        colors is first declared in __init__)."""
        self._box_unique_colors = enabled
        self.update()

    def _palette_color_for_index(self, idx: int): #vers 1
        """Deterministic colour lookup for unique-per-box colouring -
        cycles through self._box_color_palette by index, so the same
        box (same position within its own loaded list) always gets
        the same colour within a session rather than a true random
        colour that would flicker differently on every reload."""
        palette = self._box_color_palette
        return palette[idx % len(palette)]

    def _clear_col_display_lists(self): #vers 1
        """Same reasoning as _clear_world_display_lists, kept as its
        own method since collision lists live in a separate cache -
        call when the world/IPL set genuinely changes (a model's
        collision data can't change without that)."""
        if OPENGL_AVAILABLE and self._col_display_lists and self.isValid():
            try:
                self.makeCurrent()
                for list_id in self._col_display_lists.values():
                    glDeleteLists(list_id, 1)
                self.doneCurrent()
            except Exception:
                pass
        self._col_display_lists = {}

    def set_show_lod(self, enabled: bool): #vers 1
        self._show_lod = enabled; self.update()

    def _draw_assembly(self): #vers 2
        if not OPENGL_AVAILABLE: return
        for entry in getattr(self,'_all_geoms',[]):
            verts,norms,uvs,tris,mats,prelit = entry[:6]
            geom_flags = entry[6] if len(entry) > 6 else self._current_geom_flags
            old_v,old_n,old_u,old_t,old_m,old_p,old_f = (
                self._vertices,self._normals,self._uvs,
                self._triangles,self._materials,self._prelit,
                getattr(self,'_current_geom_flags',0))
            self._vertices=verts; self._normals=norms; self._uvs=uvs
            self._triangles=tris; self._materials=mats; self._prelit=prelit
            self._current_geom_flags=geom_flags
            if   self._mode=='wireframe': self._draw_wireframe()
            elif self._mode=='solid':     self._draw_solid()
            elif self._mode=='semi_solid': self._draw_solid(alpha_multiplier=0.5)
            elif self._mode=='textured':  self._draw_textured()
            (self._vertices,self._normals,self._uvs,
             self._triangles,self._materials,self._prelit,
             self._current_geom_flags) = (old_v,old_n,old_u,old_t,old_m,old_p,old_f)

    def set_world_instances(self, entries, auto_fit=True, clear_display_lists=True): #vers 4
        """Load a whole set of positioned instances for a full
        multi-instance world view (Aug 1 2026, per Keith: "wire every
        pane into the viewport, when I load ipl, these dont show").
        entries: list of dicts, each:
          {'vertices': [(x,y,z),...], 'normals': [...] or [],
           'uvs': [...] or [], 'triangles': [(v1,v2,v3,mat_id),...],
           'materials': [...], 'prelit': [...] or [],
           'pos': (x,y,z), 'rot': (x,y,z,w) quaternion, 'scale': (x,y,z),
           'model_key': <hashable, shared by every instance of the
           same model - used to build one display list per distinct
           model instead of one per instance>}
        Caller (ModelWorkshop._refresh_world_view) is responsible for
        converting each instance's cached DFFModel geometry into this
        shape - same field names/format load_geometry() already uses
        internally, just per-instance instead of one shared set.

        clear_display_lists=False (Aug 1 2026, per Keith: "touching
        time, tick, 12:00 [Play] Appears to freeze things?") - by
        default this discards every previously-compiled display list
        on every single call ("a new world/IPL selection means the
        old models' compiled geometry is no longer relevant" - true
        for a genuine new-world load, but this same method also gets
        called on every single TOBJ time-flow tick and every 2DFX
        light refresh via _refresh_world_view, none of which change
        which distinct *models* exist - only which instances of them
        are currently visible). Unconditionally recompiling every
        model's display lists once a second (the default tick
        interval) for a map with many distinct models is exactly the
        "freeze" Keith saw when pressing Play - unnecessary work, not
        a real limitation. Callers that know the model set itself
        hasn't changed (just instance-level visibility) can now skip
        the wipe and let already-compiled lists for still-visible
        models keep being reused as-is.

        auto_fit=False (Aug 1 2026, per Keith: "When moving an object
        with the object editor... the viewpoint zooms out
        automatically; the viewpoint should stay on the chosen
        object") - every nudge edit re-applies the IPL visibility
        filter to keep the World View panes in sync
        (ModelWorkshop._on_instance_edited), which calls this same
        method; auto-fitting on every single nudge was re-framing the
        camera to the whole map's bounding box each time, fighting
        whatever position the user had just navigated to. Real loads/
        IPL switches still want the fit (finding the newly-visible
        content is the point there); edit-triggered refreshes don't."""
        if clear_display_lists:
            self._clear_world_display_lists()
            self._clear_col_display_lists()
        self._world_instances = entries or []
        if self._world_instances and auto_fit:
            self._auto_fit_world()
        self.update()

    def update_instance_transform(self, inst, pos, rot, scale): #vers 1
        """Update just one already-rendered instance's position/
        rotation/scale in place, without touching any other entry or
        rebuilding the world-instances list at all - per Keith: "when
        moving any object using the IPL object editor, it takes so
        long for anything to change; is there a way to only update
        the object thats been moved, not freshing the whole
        viewport." Previously every nudge went through the full
        _apply_ipl_visibility_filter -> _refresh_world_view pipeline,
        rebuilding a fresh entry dict for every visible instance in
        the whole map just to reflect one changed instance - correct
        but wasteful for a single edit, since geometry/display lists
        for every *other* instance are completely unaffected by it.

        Finds the matching entry by identity (entry['instance'] is
        inst, set when _refresh_world_view originally built the list -
        see its own code) rather than by position/name, since those
        are exactly what's changing and can't be used to look the
        instance up. Returns True if a match was found and updated,
        False otherwise (caller should fall back to the full pipeline
        in that case - e.g. the very first time this instance is
        edited before any full refresh has ever run)."""
        for entry in getattr(self, '_world_instances', None) or []:
            if entry.get('instance') is inst:
                entry['pos'] = pos
                entry['rot'] = rot
                entry['scale'] = scale
                self.update()
                return True
        return False

    def clear_world_instances(self): #vers 2
        self._clear_world_display_lists()
        self._clear_col_display_lists()
        self._world_instances = []
        self.update()

    def _clear_world_display_lists(self): #vers 1
        if OPENGL_AVAILABLE and self._world_display_lists and self.isValid():
            try:
                self.makeCurrent()
                for list_id in self._world_display_lists.values():
                    glDeleteLists(list_id, 1)
                self.doneCurrent()
            except Exception:
                pass
        self._world_display_lists = {}

    @staticmethod
    def _quat_to_gl_matrix(x, y, z, w): #vers 1
        """Quaternion -> 16-float column-major 4x4 rotation matrix,
        the layout glMultMatrixf expects directly. Standard formula -
        each group of 4 below is one column, not one row."""
        xx, yy, zz = x*x, y*y, z*z
        xy, xz, yz = x*y, x*z, y*z
        wx, wy, wz = w*x, w*y, w*z
        return [
            1.0-2.0*(yy+zz), 2.0*(xy+wz),     2.0*(xz-wy),     0.0,
            2.0*(xy-wz),     1.0-2.0*(xx+zz), 2.0*(yz+wx),     0.0,
            2.0*(xz+wy),     2.0*(yz-wx),     1.0-2.0*(xx+yy), 0.0,
            0.0,              0.0,             0.0,             1.0,
        ]

    def _ensure_dots_cube_display_list(self): #vers 1
        """Lazily build (once, cached) a display list for the small,
        axis-coloured cube Dots mode draws at every instance's own
        position (Aug 20 2026, per Keith: "dots look good, maybe 3
        colour cubes, like the zons, Green, Red and Blue sides") -
        genuinely built once and reused via translate-only for every
        instance (see _draw_world_instances' own dots-mode branch),
        not rebuilt per instance or per frame - a huge map's worth of
        instances all share this exact same compiled shape.

        Colours match cull/zone/occlusion boxes' own already-
        established axis scheme exactly (see _draw_ghosted_box_from_
        corners' own docstring for the full colour-choice history) -
        X sides green (0.25,1.0,0.25), Y sides red (1.0,0.25,0.25), Z
        sides (top/bottom) blue (0.2,0.4,1.0) - same RGB triples,
        copied directly from that method rather than approximated, so
        a cube here and a zone box elsewhere read as the same colour
        language rather than two similar-but-not-quite-matching
        schemes.

        Small, fixed half-size (0.5 world units - a 1x1x1 cube) -
        these are meant to read as placement markers at normal map-
        viewing zoom, not compete visually with real building-sized
        geometry the way a full-scale model would."""
        if self._dots_cube_list_id is not None:
            return self._dots_cube_list_id
        h = 0.5   # half-size
        # 8 corners of a cube centred on the origin
        corners = [
            (-h, -h, -h), (h, -h, -h), (h, h, -h), (-h, h, -h),   # bottom (z1)
            (-h, -h,  h), (h, -h,  h), (h, h,  h), (-h, h,  h),   # top (z2)
        ]
        # Each face as (indices into corners, colour) - X faces green,
        # Y faces red, Z (top/bottom) faces blue, matching _draw_
        # ghosted_box_from_corners' own scheme exactly.
        green = (0.25, 1.0, 0.25)
        red   = (1.0, 0.25, 0.25)
        blue  = (0.2, 0.4, 1.0)
        faces = [
            ([0, 1, 2, 3], blue),    # bottom (Z-)
            ([4, 5, 6, 7], blue),    # top (Z+)
            ([0, 1, 5, 4], red),     # Y- side (normal along Y)
            ([2, 3, 7, 6], red),     # Y+ side (normal along Y)
            ([1, 2, 6, 5], green),   # X+ side (normal along X)
            ([3, 0, 4, 7], green),   # X- side (normal along X)
        ]
        list_id = glGenLists(1)
        glNewList(list_id, GL_COMPILE)
        glBegin(GL_QUADS)
        for indices, (fr, fg, fb) in faces:
            glColor3f(fr, fg, fb)
            for idx in indices:
                glVertex3f(*corners[idx])
        glEnd()
        glEndList()
        self._dots_cube_list_id = list_id
        return list_id

    def _draw_world_instances(self): #vers 2
        """Per instance: glPushMatrix/translate/rotate/scale, then
        replay a pre-compiled display list (Aug 1 2026 perf fix, per
        Keith: "bottlenecking is trying to move the objects in the
        viewer") - built once per (model, render mode) the first time
        it's needed, cached in self._world_display_lists, and just
        glCallList'd (cheap - no Python per-triangle loop, no
        per-vertex glBegin/glVertex calls) on every subsequent
        instance and every subsequent frame. Building a list happens
        with NO transform applied (raw local-space geometry only) -
        the per-instance position/rotation/scale is applied outside
        the list, every time, via the surrounding
        glPushMatrix/.../glPopMatrix, so one compiled list correctly
        serves every instance of that model regardless of where
        they're each positioned."""
        if not OPENGL_AVAILABLE: return
        # Dots render mode (Aug 20 2026, per Keith: "load just the IPL
        # data as dots, just placement without models or textures",
        # then: "dots look good, maybe 3 colour cubes, like the zons,
        # Green, Red and Blue sides") - a genuinely separate, much
        # simpler fast path, not threaded through the per-instance
        # display-list loop below at all. Dots-mode entries (built in
        # map_workshop.py's own _refresh_world_view_impl) never have
        # real vertices/triangles/materials to begin with - letting
        # them fall through to the normal display-list-compile logic
        # below would just compile and cache an empty, invisible list
        # per model, showing nothing at all rather than the actual
        # visible markers Keith asked for.
        #
        # Small axis-coloured cubes now, not plain points - matches
        # the same X=green/Y=red/Z=blue face-colour convention already
        # used for cull/zone/occlusion boxes (see _draw_ghosted_box_
        # from_corners' own docstring for that established scheme),
        # for visual consistency across every box-shaped overlay in
        # this app, not a new, different colour scheme invented just
        # for this one. Deliberately NOT reusing that same shared
        # helper here, though - it has real transparency/blending
        # overhead built for a handful of large zone boxes per map,
        # whereas Dots mode needs to stay fast for potentially
        # thousands of tiny per-instance markers at once; a cube looks
        # identical regardless of rotation/scale the same way a plain
        # point did, so this still skips the whole glPushMatrix/
        # rotate/scale/glPopMatrix dance entirely too, replacing it
        # with translate-only (see _ensure_dots_cube_display_list's
        # own docstring for the one-compiled-shape-reused-everywhere
        # approach that keeps this genuinely fast).
        if self._mode == 'dots':
            list_id = self._ensure_dots_cube_display_list()
            glDisable(GL_LIGHTING)
            glDisable(GL_TEXTURE_2D)
            for entry in self._world_instances:
                px, py, pz = entry.get('pos', (0.0, 0.0, 0.0))
                glPushMatrix()
                glTranslatef(px, py, pz)
                glCallList(list_id)
                glPopMatrix()
            glEnable(GL_LIGHTING)
            return
        old_v,old_n,old_u,old_t,old_m,old_p,old_f = (
            self._vertices,self._normals,self._uvs,
            self._triangles,self._materials,self._prelit,
            getattr(self,'_current_geom_flags',0))
        # Which collision overlay modes are currently on (Aug 14 2026)
        # - checked once per frame, not per instance, since none of
        # these depend on anything instance-specific.
        col_modes = []
        if self.show_col_ghosted:        col_modes.append('ghosted')
        if self.show_col_semi_solid:     col_modes.append('semi_solid')
        if self.show_col_wireframe:      col_modes.append('wireframe')
        if self.show_col_surface_mapped: col_modes.append('surface_mapped')
        old_cv, old_ct = (getattr(self, '_col_vertices', None),
                          getattr(self, '_col_triangles', None))
        for entry in self._world_instances:
            model_key = entry.get('model_key', id(entry))
            cache_key = (model_key, self._mode)
            list_id = self._world_display_lists.get(cache_key)
            if list_id is None:
                list_id = glGenLists(1)
                self._vertices  = entry.get('vertices', [])
                self._normals   = entry.get('normals', [])
                self._uvs       = entry.get('uvs', [])
                self._triangles = entry.get('triangles', [])
                self._materials = entry.get('materials', [])
                self._prelit    = entry.get('prelit', [])
                glNewList(list_id, GL_COMPILE)
                if   self._mode=='wireframe': self._draw_wireframe()
                elif self._mode=='solid':     self._draw_solid()
                elif self._mode=='semi_solid': self._draw_solid(alpha_multiplier=0.5)
                elif self._mode=='textured':  self._draw_textured()
                glEndList()
                self._world_display_lists[cache_key] = list_id
            glPushMatrix()
            px, py, pz = entry.get('pos', (0.0, 0.0, 0.0))
            glTranslatef(px, py, pz)
            rx, ry, rz, rw = entry.get('rot', (0.0, 0.0, 0.0, 1.0))
            glMultMatrixf(self._quat_to_gl_matrix(rx, ry, rz, rw))
            sx, sy, sz = entry.get('scale', (1.0, 1.0, 1.0))
            glScalef(sx, sy, sz)
            glCallList(list_id)
            # Collision overlay (Aug 14 2026) - drawn inside the same
            # instance transform, right after the model itself, so it
            # sits exactly where the model's own collision belongs.
            # A separate display list per (model_key, col mode), built
            # lazily the same way as the model's own lists - only
            # entries with actual col_vertices/col_triangles (from a
            # model that had matching collision data indexed) produce
            # anything; the rest are silent no-ops via the length
            # check in _draw_collision_faces.
            if col_modes and entry.get('col_vertices') and entry.get('col_triangles'):
                self._col_vertices  = entry.get('col_vertices')
                self._col_triangles = entry.get('col_triangles')
                for mode in col_modes:
                    col_cache_key = (model_key, mode)
                    col_list_id = self._col_display_lists.get(col_cache_key)
                    if col_list_id is None:
                        col_list_id = glGenLists(1)
                        glNewList(col_list_id, GL_COMPILE)
                        self._draw_collision_faces(mode)
                        glEndList()
                        self._col_display_lists[col_cache_key] = col_list_id
                    glCallList(col_list_id)
            glPopMatrix()
        (self._vertices,self._normals,self._uvs,
         self._triangles,self._materials,self._prelit,
         self._current_geom_flags) = (old_v,old_n,old_u,old_t,old_m,old_p,old_f)
        self._col_vertices, self._col_triangles = old_cv, old_ct

    def set_2dfx_lights(self, lights): #vers 1
        """Store the current set of 2DFX light points to render - per
        Keith: "lets add the 2dfx support next, showing 2dfx lighting
        at night." lights: list of (x, y, z, r, g, b, a, size) tuples
        in WORLD space (caller - ModelWorkshop._refresh_2dfx_lights -
        is responsible for computing each light's world position from
        its owning instance's position/rotation plus the 2DFX entry's
        own local offset, and for deciding which lights should be
        showing at all based on the simulated time-of-day, e.g. only
        collecting them at night). Empty list clears them (e.g. Time
        switch off, or daytime)."""
        self._2dfx_lights = lights or []
        self.update()

    def _draw_2dfx_lights(self): #vers 1
        """Render every current 2DFX light as a glowing point - a
        deliberately simple, reliable rendering technique (a single
        GL_POINTS draw with additive blending and no depth writes,
        rather than sprite/billboard geometry) since it needs no UV/
        texture setup and still reads as "something is glowing here"
        at typical map-view zoom levels. size (parsed from the 2DFX
        entry's own corona_size where available, else a fallback)
        scales the point - real corona sprites would scale with
        camera distance for a true billboard look, which this doesn't
        attempt yet."""
        if not OPENGL_AVAILABLE: return
        lights = getattr(self, '_2dfx_lights', None)
        if not lights:
            return
        glDisable(GL_LIGHTING)
        glDisable(GL_TEXTURE_2D)
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE)   # additive - glow, not a flat dot
        glDepthMask(False)
        glPointSize(8.0)
        for x, y, z, r, g, b, a, size in lights:
            glPointSize(max(2.0, 8.0 * size))
            glBegin(GL_POINTS)
            glColor4f(r / 255.0, g / 255.0, b / 255.0, a / 255.0)
            glVertex3f(x, y, z)
            glEnd()
        glDepthMask(True)
        glDisable(GL_BLEND)
        glEnable(GL_LIGHTING)

    def _auto_fit_world(self): #vers 1
        """Frame the camera around every instance's WORLD position
        (not vertex-level detail like _auto_fit - map-scale distances
        make individual meshes irrelevant to the initial framing)."""
        if not self._world_instances: return
        xs = [e.get('pos', (0,0,0))[0] for e in self._world_instances]
        ys = [e.get('pos', (0,0,0))[1] for e in self._world_instances]
        zs = [e.get('pos', (0,0,0))[2] for e in self._world_instances]
        diag = math.sqrt((max(xs)-min(xs))**2+(max(ys)-min(ys))**2+(max(zs)-min(zs))**2)
        self._dist  = max(diag*0.75, 10.0)
        self._pan_x = -(max(xs)+min(xs))/2
        self._pan_y = -(max(ys)+min(ys))/2
        self.update()

    def set_prelight(self, v: bool): #vers 1
        self._use_prelight = v; self.update()

    def set_light_dir(self, x, y, z): #vers 1
        self._light_dir = (x, y, z, 0.0)
        if OPENGL_AVAILABLE and self.isVisible():
            self.makeCurrent(); self._setup_lighting(); self.doneCurrent()
        self.update()

    def set_ambient(self, v: float): #vers 1
        self._ambient = v
        if OPENGL_AVAILABLE and self.isVisible():
            self.makeCurrent(); self._setup_lighting(); self.doneCurrent()
        self.update()

    def set_diffuse(self, v: float): #vers 1
        self._diffuse = v
        if OPENGL_AVAILABLE and self.isVisible():
            self.makeCurrent(); self._setup_lighting(); self.doneCurrent()
        self.update()

    def reset_camera(self): #vers 1
        self._yaw=45.0; self._pitch=25.0; self._pan_x=0.0; self._pan_y=0.0
        self._auto_fit(); self.update()

    def capture_radar_tile(self, center_x: float, center_y: float, tile_size: float,
                           show_grid: bool = False): #vers 2
        """Capture one, exact, correctly-oriented top-down orthographic
        snapshot of the currently loaded world, centred on a real
        RadarTile's own world-space centre (Aug 20 2026, per Keith:
        "look at radar editor for how the radar works" - the actual
        rendering half of map-to-radar generation).

        pitch=0/yaw=0 is the real, numerically-verified top-down,
        north-up orientation for this viewport's own camera convention
        - NOT pitch=90 (an earlier, wrong first guess caught before it
        was ever implemented: the full lookAt+rotate transform chain
        was worked out on paper with real coordinates first, which
        showed pitch=90 is actually a SIDE-on view here, not top-down
        at all - pitch=0 is the one where a taller world point stays
        centred on screen rather than shifting sideways, matching what
        a real top-down capture needs). pan_x/pan_y are the NEGATIVE
        of the desired world centre (also verified numerically, not
        assumed) - the scene itself is translated by this amount
        before the fixed camera views it, so this is what actually
        puts the desired world point at screen centre.

        show_grid=False by default (Aug 20 2026, per Keith: "the
        square grid gets saved in with the radar tiles, can we have a
        settings option to not show the grid") - self._show_grid
        controls the same square reference grid drawn in every normal
        interactive view, and paintGL draws it unconditionally
        whenever that flag is set, with no distinction between an
        interactive view and a background capture like this one - so
        without this, whatever grid state the person's own live view
        happened to be in bled straight into every generated tile.
        Deliberately a real parameter here, not a hardcoded "always
        off" - map_workshop.py's own caller reads the actual, real
        MapSettings toggle and passes it through, so a genuine
        settings option controls this rather than this method quietly
        deciding on its own; this viewport class has no direct
        MapSettings access itself, matching the same "widget draws
        plain data/state, the caller resolves real settings" split
        already used for every other overlay this session.

        Saves and restores every camera state variable touched
        afterward - a batch tile-generation run must not permanently
        disrupt whatever view the person had open before starting it.
        Returns a QImage (the raw captured framebuffer), leaving what
        to do with it (crop to a real radarNN.txd, save as PNG, etc.)
        to the caller."""
        saved = (self._yaw, self._pitch, self._dist, self._pan_x,
                 self._pan_y, self._projection, self._show_grid)
        try:
            self._yaw = 0.0
            self._pitch = 0.0
            self._dist = tile_size
            self._pan_x = -center_x
            self._pan_y = -center_y
            self._projection = 'ortho'
            self._show_grid = show_grid
            self.makeCurrent()
            self.resizeGL(self.width(), self.height())
            self.paintGL()
            image = self.grabFramebuffer()
        finally:
            self._yaw, self._pitch, self._dist, self._pan_x, \
                self._pan_y, self._projection, self._show_grid = saved
            self.makeCurrent()
            self.resizeGL(self.width(), self.height())
            self.update()
        return image

    def set_view_lock(self, locked: bool, label: str = "", yaw: float = None,
                       pitch: float = None, projection: str = 'perspective'): #vers 1
        """Lock/unlock this pane to a fixed preset view (3ds Max style Top/
        Front/Side/Perspective panes). Locked panes cannot rotate-drag and
        use parallel (ortho) projection; unlocked/perspective panes behave
        as before (free rotate)."""
        self._view_locked = locked
        self._view_label  = label
        self._projection  = projection
        if yaw   is not None: self._yaw   = yaw
        if pitch is not None: self._pitch = pitch
        if label:
            self._label_widget.setText(label)
            self._label_widget.adjustSize()
            self._label_widget.show()
        else:
            self._label_widget.hide()
        try:
            if not hasattr(self, 'isValid') or self.isValid():
                self.resizeGL(self.width(), self.height())
        except Exception:
            pass
        self.update()

    def mousePressEvent(self, event): #vers 4
        self._last_pos = event.pos()
        if event.button() == Qt.MouseButton.RightButton:
            # Tracks where a right-click started (Aug 19 2026, for
            # hover-highlight's own right-click-for-options request) -
            # right-click-and-drag already means "rotate the camera"
            # (see mouseMoveEvent), so a genuine click needs telling
            # apart from the start of a drag; compared against the
            # release position in mouseReleaseEvent to decide which
            # one actually happened.
            self._right_press_pos = event.pos()
        if event.button() == Qt.MouseButton.LeftButton:
            # Path node editing (Aug 17 2026) - its own independent
            # toggle, not part of the vertex/edge/face _select_mode
            # system below (a path node isn't part of any loaded
            # mesh), checked first and handled completely separately
            # rather than trying to fold it into that system.
            if getattr(self, '_path_edit_mode', False):
                mx, my = event.pos().x(), event.pos().y()
                pos = self._pick_path_node(mx, my)
                if pos is not None:
                    self._dragging_path_node_start_key = pos
                    self._dragging_path_node_current_pos = pos
                    # Real UX gap found (Aug 18 2026, per Keith:
                    # "Clicking nodes brings up nothing?") - a
                    # successful pick never triggered a repaint here,
                    # and nothing was ever drawn differently for the
                    # currently-held node either (see _draw_paths'
                    # own highlight logic, added alongside this fix)
                    # - so a click that didn't happen to also move
                    # the mouse enough to shift the node's ground
                    # projection produced literally zero visible
                    # change, even though the pick itself had
                    # actually succeeded. This call plus the new
                    # highlight together mean picking a node up is
                    # now visible immediately, before any drag
                    # movement happens at all.
                    self.update()
                return
            if getattr(self, '_box_edit_mode', False):
                mx, my = event.pos().x(), event.pos().y()
                key = self._pick_box_corner(mx, my)
                if key is not None:
                    info = self._pickable_box_corners[key]
                    self._dragging_box_corner_key = key
                    self._dragging_box_corner_info = info
                    self._dragging_box_corner_current_pos = info['pos']
                    self.update()
                return
            # Whole-IPL-section dragging (Aug 18 2026) - also its own
            # independent toggle, checked right after path node
            # editing so the two mutually-exclusive interactive modes
            # don't fight over the same click (a person would only
            # ever have one of them on at a time in practice, but
            # checking both explicitly here rather than assuming
            # keeps that intentional, not accidental).
            if getattr(self, '_ipl_drag_mode', False):
                mx, my = event.pos().x(), event.pos().y()
                idx = self._pick_world_instance(mx, my)
                if idx is not None:
                    entry = self._world_instances[idx]
                    inst = entry.get('instance')
                    ipl_name = getattr(inst, 'source_ipl', None) if inst is not None else None
                    if ipl_name:
                        interaction_mode = getattr(self, '_ipl_interaction_mode', 'drag')
                        if interaction_mode != 'drag':
                            # Move/Rotate mode (Aug 19 2026) - a plain
                            # click immediately hands the picked IPL's
                            # name off to map_workshop.py's own
                            # numeric-dialog callback and stops right
                            # here - no drag-state tracking starts at
                            # all for these two modes, unlike Drag
                            # mode just below. Unaffected by the
                            # Ctrl/Shift multi-select workflow below -
                            # that's specific to Drag mode's own click-
                            # and-hold gesture, which Move/Rotate don't
                            # use at all.
                            callback = getattr(self, '_ipl_click_callback', None)
                            if callback is not None:
                                callback(ipl_name)
                            return

                        # Multi-IPL selection/drag workflow (Aug 19
                        # 2026, per Keith's own careful, two-part spec
                        # - see the fuller explanation where self.
                        # _multi_selected_ipl_names is first declared
                        # in __init__). Shift+click builds a selection
                        # (own IPL Sections table row-clicks feed the
                        # exact same set too, via set_multi_selected_
                        # ipl_names). Ctrl+click+hold+drag is the ONE
                        # way to actually start a drag - "Left Control
                        # key, click and hold left mouse drags entire
                        # IPL(s)" (plural), so it drags the CURRENT
                        # selection if one exists, or falls back to
                        # just the clicked instance's own IPL if
                        # nothing's selected. A plain click with
                        # neither modifier does nothing at all now -
                        # Ctrl is the single, universal, explicit
                        # gesture for "drag", simpler and less
                        # ambiguous than the earlier version of this
                        # same workflow, which split "drag one" and
                        # "drag the selection" across two different
                        # triggers (Ctrl vs a plain click).
                        modifiers = event.modifiers()
                        if modifiers & Qt.KeyboardModifier.ShiftModifier:
                            # Shift+click: toggle this IPL into/out of
                            # the multi-selection, don't start a drag
                            # at all - building the selection is its
                            # own, separate gesture from actually
                            # dragging it.
                            if ipl_name in self._multi_selected_ipl_names:
                                self._multi_selected_ipl_names.discard(ipl_name)
                            else:
                                self._multi_selected_ipl_names.add(ipl_name)
                            callback = getattr(self, '_ipl_selection_callback', None)
                            if callback is not None:
                                callback(set(self._multi_selected_ipl_names))
                            self.update()
                            return
                        elif modifiers & Qt.KeyboardModifier.ControlModifier:
                            drag_names = set(self._multi_selected_ipl_names) \
                                if self._multi_selected_ipl_names else {ipl_name}
                        else:
                            # No modifier at all: does nothing.
                            return


                        self._dragging_ipl_names = drag_names
                        # A list of (inst, pos, rot, scale) tuples, NOT
                        # a dict keyed by inst - IPLInstance is a plain
                        # @dataclass (eq=True by default), which makes
                        # it UNHASHABLE, so using it as a dict key
                        # would crash the very first time this actually
                        # ran. Caught by directly verifying this exact
                        # logic against real IPLInstance objects before
                        # trusting it, not just reasoning about it.
                        self._dragging_ipl_start_state = [
                            (e['instance'], e['pos'], e['rot'], e['scale'])
                            for e in self._world_instances
                            if getattr(e.get('instance'), 'source_ipl', None) in drag_names
                        ]
                        # The specific instance actually clicked (Aug
                        # 19 2026, for Snap: Centre of Model) - stored
                        # separately from the full start_state list
                        # above, which covers EVERY instance across
                        # every dragged IPL; the snap check below needs
                        # to know which one specifically to search a
                        # snap target near, not the whole group's own
                        # centroid or every one of its instances at
                        # once.
                        self._dragging_ipl_clicked_inst = inst
                        self._dragging_ipl_clicked_start_pos = entry['pos']
                        self._dragging_ipl_ground_start = self._screen_to_ground_position(
                            mx, my, ground_z=entry['pos'][2])
                        self._dragging_ipl_delta = (0.0, 0.0, 0.0)
                        self.update()
                return
            mode = getattr(self, '_select_mode', 'object')
            if mode == 'object':
                return
            mx, my = event.pos().x(), event.pos().y()
            if mode == 'vertex':
                key = self._pick_vertex(mx, my)
            elif mode == 'edge':
                key = self._pick_edge(mx, my)
            else:  # 'face' or 'poly'
                key = self._pick_face(mx, my)
            if key is not None:
                self._apply_selection_click(mode, key, event.modifiers())
                self.update()
            elif not (event.modifiers() & (Qt.KeyboardModifier.ControlModifier |
                                           Qt.KeyboardModifier.ShiftModifier)):
                # Clicked empty space with no modifier — clear selection
                self._selected_set_for_mode(mode).clear()
                self._notify_selection_changed()
                self.update()

    def mouseMoveEvent(self, event): #vers 3
        dx = event.pos().x() - self._last_pos.x()
        dy = event.pos().y() - self._last_pos.y()
        sens = getattr(self, '_mouse_sensitivity', 1.0)
        if event.buttons() & Qt.MouseButton.RightButton and not self._view_locked:
            self._yaw   += dx * 0.5 * sens
            self._pitch += dy * 0.5 * sens
        elif event.buttons() & Qt.MouseButton.MiddleButton:
            # Yaw-compensated pan (Aug 1 2026, per Keith: "moving the
            # mouse left, should always reflect moving left in the
            # viewpoint... mouse movement seems to switch depending on
            # viewing angle") - self._pan_x/y get applied via
            # glTranslatef *before* the scene's own glRotatef(yaw,...)
            # in paintGL's transform chain (translate happens first on
            # the actual geometry, since OpenGL applies transforms in
            # the reverse of call order), so the raw screen-space drag
            # delta was being interpreted directly as a world-space
            # offset with no yaw compensation at all - "left" only
            # felt consistent from whatever one specific angle the
            # camera happened to start at. _apply_pan_step (below)
            # pre-rotates the screen delta by -yaw to exactly cancel
            # the scene's own +yaw rotation once applied, keeping the
            # net pan direction locked to actual screen-space
            # regardless of viewing angle - shared with keyboard
            # panning (keyPressEvent) so both feel identical.
            scale = self._dist * 0.002 * sens
            self._apply_pan_step(dx * scale, -dy * scale)
        elif (event.buttons() & Qt.MouseButton.LeftButton
              and getattr(self, '_dragging_path_node_start_key', None) is not None):
            # Path node drag in progress (Aug 17 2026) - constrained
            # to the ground plane at the node's OWN starting height
            # (see set_path_edit_mode's own docstring for why this is
            # the right constraint, not a shortcut), reusing _screen_
            # to_ground_position exactly as LOD Test mode already
            # does just below - same proven ray-plane math, different
            # caller. Updates self._path_segments directly for
            # immediate visual feedback every frame, without touching
            # the real PathNode data or triggering a full map_
            # workshop.py refresh until the drag actually completes
            # (mouseReleaseEvent) - redoing that full resolve/rebuild
            # on every single mouse-move pixel would be needless work
            # for something that only needs to happen once, at the
            # end.
            start_z = self._dragging_path_node_start_key[2]
            new_pos = self._screen_to_ground_position(
                event.pos().x(), event.pos().y(), ground_z=start_z)
            if new_pos is not None:
                old_pos = self._dragging_path_node_current_pos
                self._path_segments = [
                    (new_pos if a == old_pos else a, new_pos if b == old_pos else b)
                    for a, b in self._path_segments]
                self._dragging_path_node_current_pos = new_pos
        elif (event.buttons() & Qt.MouseButton.LeftButton
              and getattr(self, '_dragging_box_corner_key', None) is not None):
            # Box corner resize drag in progress (Aug 19 2026) - same
            # ground-plane-at-starting-height constraint as path node
            # dragging (2D drag, height-preserving - a 2D mouse can't
            # unambiguously set both XY and Z at once). Live preview
            # mutates the actual (x1,y1,z1,x2,y2,z2) tuple sitting in
            # self._cull_boxes/self._zone_boxes at the dragged corner's
            # own box_index directly - unlike _pickable_box_corners
            # (rebuilt fresh from scratch every single paintGL call,
            # so mutating IT wouldn't survive to the next frame), the
            # box tuple lists themselves are genuinely persistent
            # between frames, only ever replaced wholesale when map_
            # workshop.py calls set_cull_boxes/set_zone_boxes again -
            # so swapping just this one box's own tuple is enough for
            # the very next paintGL call to draw the resized box
            # immediately, without needing a second, parallel "live
            # preview" data structure the way path nodes needed one.
            info = self._dragging_box_corner_info
            box_type, box_index, corner_idx, start_z = self._dragging_box_corner_key
            new_pos = self._screen_to_ground_position(
                event.pos().x(), event.pos().y(), ground_z=start_z)
            if new_pos is not None:
                ox, oy, oz = info['opposite']
                x1, x2 = sorted((new_pos[0], ox))
                y1, y2 = sorted((new_pos[1], oy))
                z1, z2 = sorted((start_z, oz))
                box_list = self._cull_boxes if box_type == 'cull' else self._zone_boxes
                if (not self._no_clip_boxes or not self._box_resize_would_overlap(
                        box_type, box_index, x1, y1, x2, y2, z1, z2)):
                    if 0 <= box_index < len(box_list):
                        box_list[box_index] = (x1, y1, z1, x2, y2, z2)
                    self._dragging_box_corner_current_pos = new_pos
                # else: no-clip is on and this would overlap another
                # box - box_list[box_index] is simply left untouched,
                # holding its last known-good, non-overlapping size
                # for this frame rather than jumping to the colliding
                # one; the very next mouse-move tries again from
                # wherever the cursor has moved to by then.
                self.update()
        elif (event.buttons() & Qt.MouseButton.LeftButton
              and getattr(self, '_dragging_ipl_names', None)):
            # Whole-IPL drag in progress (Aug 18 2026, generalised Aug
            # 19 2026 to cover one OR several IPLs being dragged
            # together at once - see the fuller multi-select workflow
            # explanation where self._multi_selected_ipl_names is
            # first declared in __init__) - same ground-plane-at-
            # starting-height constraint as path node dragging,
            # reusing the exact same _screen_to_ground_position call.
            # Computes ONE delta (current ground pos minus the ground
            # pos captured at drag start), then applies that SAME
            # delta to every instance captured in _dragging_ipl_
            # start_state via update_instance_transform - cheap (just
            # updates each instance's own cached display entry, no
            # geometry/display-list rebuilding), and crucially never
            # touches the real IPLInstance data until the drag
            # actually completes (mouseReleaseEvent) - if released
            # with no real movement, or the mode gets turned off mid-
            # drag, nothing was ever actually mutated.
            start_ground = self._dragging_ipl_ground_start
            if start_ground is not None:
                cur_ground = self._screen_to_ground_position(
                    event.pos().x(), event.pos().y(), ground_z=start_ground[2])
                if cur_ground is not None:
                    ddx = cur_ground[0] - start_ground[0]
                    ddy = cur_ground[1] - start_ground[1]
                    ddz = cur_ground[2] - start_ground[2]
                    # Axis lock (Aug 18 2026) - applied as a simple
                    # post-processing mask on the already-computed
                    # delta, rather than changing the underlying
                    # ground-plane projection itself: zero out
                    # whichever axis is locked before it ever reaches
                    # the live preview or gets stored for the eventual
                    # commit, so a locked axis genuinely never moves,
                    # not just visually suppressed.
                    axis_lock = getattr(self, '_ipl_drag_axis_lock', None)
                    if axis_lock == 'x':
                        ddx = 0.0
                    elif axis_lock == 'y':
                        ddy = 0.0
                    if self._snap_targets.get('centre'):
                        # Snap: Centre of Model (Aug 19 2026, per
                        # Keith: "if the snap options are on... use
                        # Edge of model, Centre of model") - once the
                        # clicked instance's own WOULD-BE position
                        # (its start position plus the current delta)
                        # comes within a small threshold of any OTHER
                        # instance's real position (excluding any of
                        # the IPL(s) actually being dragged, so it
                        # can't snap to one of its own siblings - a
                        # real bug fixed here too: this used to
                        # reference a bare "ipl_name" that was never
                        # actually defined anywhere within THIS
                        # method's own scope at all, a NameError
                        # waiting to happen the first time anyone
                        # actually dragged with this snap mode on,
                        # caught while generalising this same check to
                        # cover a whole SET of dragged IPLs rather
                        # than just one), the delta gets nudged so it
                        # lands EXACTLY on that instance's position
                        # instead of merely close to it.
                        clicked_start = getattr(self, '_dragging_ipl_clicked_start_pos', None)
                        dragged_names = getattr(self, '_dragging_ipl_names', set())
                        if clicked_start is not None:
                            wx = clicked_start[0] + ddx
                            wy = clicked_start[1] + ddy
                            wz = clicked_start[2] + ddz
                            best_dist2, best_pos = None, None
                            for e in self._world_instances:
                                other_inst = e.get('instance')
                                if other_inst is None or other_inst.source_ipl in dragged_names:
                                    continue
                                ox, oy, oz = e['pos']
                                d2 = (ox-wx)**2 + (oy-wy)**2 + (oz-wz)**2
                                if best_dist2 is None or d2 < best_dist2:
                                    best_dist2, best_pos = d2, (ox, oy, oz)
                            if best_pos is not None and best_dist2 < 9.0:   # within 3 units
                                ddx = best_pos[0] - clicked_start[0]
                                ddy = best_pos[1] - clicked_start[1]
                                ddz = best_pos[2] - clicked_start[2]
                    self._dragging_ipl_delta = (ddx, ddy, ddz)
                    for inst, opos, orot, oscale in self._dragging_ipl_start_state:
                        new_pos = (opos[0] + ddx, opos[1] + ddy, opos[2] + ddz)
                        self.update_instance_transform(inst, new_pos, orot, oscale)
        elif (event.buttons() == Qt.MouseButton.NoButton
              and getattr(self, '_hover_highlight_enabled', False)):
            # Auto-highlight on hover (Aug 19 2026, per Keith's own
            # request) - only when no button is held at all, so it
            # never fights with rotate/pan/drag, which all already
            # have their own meaning for mouse movement. Reuses the
            # exact same _pick_world_instance already proven for
            # double-click-to-edit and whole-IPL dragging, rather than
            # a new picking mechanism. The unconditional self.update()
            # a few lines below already repaints on every mouseMoveEvent
            # regardless, so this branch only needs to update the
            # stored hover state itself, not trigger its own separate
            # repaint too.
            idx = self._pick_world_instance(event.pos().x(), event.pos().y())
            self._hovered_instance_idx = idx
        self._last_pos = event.pos(); self.update()

        # LOD test mode (Aug 1 2026, per Keith's crash: "AttributeError:
        # 'DFFViewport' object has no attribute 'set_lod_test_callback'"
        # - the original LOD-test implementation only added this to
        # MapViewport, but preview_widget (what the toggle actually
        # wires up to) is a DFFViewport, a different class entirely
        # with its own camera system - same feature, same callback
        # pattern, added here too now).
        callback = getattr(self, '_lod_test_callback', None)
        if callback is not None and not getattr(self, '_dragging_path_node_start_key', None):
            ground_pos = self._screen_to_ground_position(event.pos().x(), event.pos().y())
            if ground_pos is not None:
                self.set_lod_test_center(ground_pos)
                callback(ground_pos)

    def _screen_to_ground_position(self, mx, my, ground_z=0.0): #vers 1
        """Cast a ray from the camera through the given widget-space
        pixel (via the already-existing _pick_ray, which replicates
        paintGL's exact camera transform) and intersect it with the
        horizontal plane z=ground_z - DFFViewport works in GTA's
        native Z-up space directly (unlike MapViewport, which converts
        to Y-up), so the ground plane is a fixed Z here rather than Y.
        Returns None if the ray is parallel to the ground plane or
        points away from it."""
        ray = self._pick_ray(mx, my)
        if ray is None:
            return None
        (ox, oy, oz), (dx, dy, dz) = ray
        if abs(dz) < 1e-9:
            return None
        t = (ground_z - oz) / dz
        if t < 0:
            return None
        return (ox + dx * t, oy + dy * t, oz + dz * t)

    def set_lod_test_callback(self, callback): #vers 1
        """Set (or clear, with None) the function called with the
        current ground-position world point on every mouse move while
        LOD test mode is active - mirrors MapViewport's identical
        method (see its own docstring for the full feature context)."""
        self._lod_test_callback = callback

    def set_lod_test_center(self, world_pos): #vers 1
        """Set (or clear, with None) the LOD test circle's center in
        world space - mirrors MapViewport's identical method."""
        self._lod_test_center = world_pos
        self.update()

    def _draw_lod_test_circle(self): #vers 1
        """Draw a flat circle outline on the ground plane (z=center_z)
        at self._lod_test_center, radius self._lod_test_radius -
        mirrors MapViewport's identical method, adapted for this
        class's native Z-up convention (circle drawn in the XY plane
        here, XZ plane there)."""
        if not OPENGL_AVAILABLE:
            return
        cx, cy, cz = self._lod_test_center
        radius = getattr(self, '_lod_test_radius', 300.0)
        segments = 64
        glColor3f(0.2, 1.0, 0.3)
        glLineWidth(2.0)
        glPushMatrix()
        glTranslatef(cx, cy, cz)
        glBegin(GL_LINE_LOOP)
        for i in range(segments):
            angle = 2 * math.pi * i / segments
            glVertex3f(radius * math.cos(angle), radius * math.sin(angle), 0.0)
        glEnd()
        glPopMatrix()
        glLineWidth(1.0)

    def mouseReleaseEvent(self, event): #vers 3
        self._last_pos = event.pos()
        if event.button() == Qt.MouseButton.RightButton:
            # Right-click for options on a hovered instance (Aug 19
            # 2026, per Keith: "once highlighted, right-click for
            # options"). A real click (barely moved since press) is
            # told apart from a right-click-drag (camera rotation) by
            # comparing against the position stored at press time -
            # small pixel tolerance for a hand that isn't perfectly
            # still between press and release, not a strict pixel-
            # for-pixel match.
            press_pos = getattr(self, '_right_press_pos', None)
            self._right_press_pos = None
            if press_pos is not None:
                moved = (event.pos() - press_pos).manhattanLength()
                hovered_idx = getattr(self, '_hovered_instance_idx', None)
                callback = getattr(self, '_hover_context_callback', None)
                if moved <= 4 and hovered_idx is not None and callback is not None:
                    entry = self._world_instances[hovered_idx]
                    inst = entry.get('instance')
                    if inst is not None:
                        callback(inst)
        # Commit a completed path node drag (Aug 17 2026) - looks up
        # the real (group_ref, node_index) via the ORIGINAL start
        # position (self._path_node_owner_map's own keys never change
        # mid-drag; only the live segments/current-drag-position did,
        # for visual feedback), so this lookup is unaffected by
        # however far the node actually moved.
        start_key = getattr(self, '_dragging_path_node_start_key', None)
        if start_key is not None:
            final_pos = getattr(self, '_dragging_path_node_current_pos', None)
            owner = self._path_node_owner_map.get(start_key)
            callback = getattr(self, '_path_node_drag_callback', None)
            if owner is not None and final_pos is not None and callback is not None:
                group_ref, node_index = owner
                fx, fy, fz = final_pos
                callback(group_ref, node_index, fx, fy, fz)
            self._dragging_path_node_start_key = None
            self._dragging_path_node_current_pos = None
            # Explicit repaint here too (Aug 18 2026), not just relying
            # on the callback's own downstream refresh - if the owner
            # lookup or callback happened to be unavailable for any
            # reason, the held-node highlight state above still needs
            # to clear from the screen, not just from self's own
            # tracking variables.
            self.update()

        # Commit a completed box corner resize (Aug 19 2026) - the
        # committed values are read straight out of box_list[box_
        # index] (whatever the live-preview mouseMoveEvent logic last
        # actually wrote there - already the final, resolved x1/y1/
        # z1/x2/y2/z2, with no-clip's own rejection already baked in
        # if that was on), rather than recomputed here - avoids
        # duplicating the same min/max-against-opposite-corner and
        # no-clip-overlap logic a second time for what should be
        # exactly the same result.
        corner_key = getattr(self, '_dragging_box_corner_key', None)
        if corner_key is not None:
            box_type, box_index, corner_idx, start_z = corner_key
            info = getattr(self, '_dragging_box_corner_info', None)
            callback = getattr(self, '_box_resize_callback', None)
            box_list = self._cull_boxes if box_type == 'cull' else self._zone_boxes
            if (info is not None and callback is not None
                    and 0 <= box_index < len(box_list)):
                x1, y1, z1, x2, y2, z2 = box_list[box_index]
                callback(box_type, info['box_ref'], x1, y1, x2, y2, z1, z2)
            self._dragging_box_corner_key = None
            self._dragging_box_corner_info = None
            self._dragging_box_corner_current_pos = None
            self.update()

        # Commit a completed whole-IPL drag (Aug 18 2026, generalised
        # Aug 19 2026 to cover one or several IPLs at once) - calls
        # the registered callback once PER dragged IPL name, with the
        # same final (dx, dy, dz) for each - map_workshop.py wires
        # this to the already-existing, already-verified _shift_ipl_
        # coordinates (the same method the dialog-based Shift
        # Coordinates tool uses) - that's what actually mutates the
        # real data (instances AND paths/cull/zone/occl, everything
        # belonging to each IPL, not just the instances that got live
        # visual feedback during the drag itself) and triggers a
        # full, correctly-synced refresh for each one. A release with
        # zero actual movement (delta all zeros - e.g. a plain click
        # that picked up a selection but never dragged it) is skipped
        # entirely rather than calling the callback with a no-op
        # move, avoiding a pointless undo-stack entry for nothing
        # having actually happened.
        dragged_names = getattr(self, '_dragging_ipl_names', None)
        if dragged_names:
            dx, dy, dz = getattr(self, '_dragging_ipl_delta', (0.0, 0.0, 0.0))
            callback = getattr(self, '_ipl_drag_callback', None)
            if callback is not None and (dx or dy or dz):
                for ipl_name in dragged_names:
                    callback(ipl_name, dx, dy, dz)
            self._dragging_ipl_names = set()
            self._dragging_ipl_start_state = []
            self._dragging_ipl_ground_start = None
            self._dragging_ipl_delta = (0.0, 0.0, 0.0)
            self._dragging_ipl_clicked_inst = None
            self._dragging_ipl_clicked_start_pos = None
            self.update()

    def wheelEvent(self, event): #vers 4
        """Zoom in/out, optionally toward the mouse cursor rather than
        the current pan centre (Aug 18 2026, per Keith: "when I zoom
        in, or zoom out, have a settings option to zoom in to the
        mouse pointer. so if i move the point to the top, and zoom
        in, it zooms in that area").

        Standard "zoom to cursor" technique, reusing the already-
        proven _screen_to_ground_position (same ray-cast machinery
        already used for LOD Test mode and path node dragging, not
        new geometry code) rather than deriving new trigonometry:
        find the world-space ground point under the cursor BEFORE
        changing _dist, apply the zoom, find where that same screen
        pixel now points to AFTER the zoom, then shift _pan_x/_pan_y
        by the difference. Since _pan_x/_pan_y already directly
        offset the world in the exact same coordinate space _screen_
        to_ground_position resolves into (both go through the same
        glTranslatef(pan_x, pan_y, 0) in the modelview chain), the
        shift needed is just that raw delta - no yaw compensation
        needed here (unlike _apply_pan_step, which takes a raw
        screen-space input and has to pre-rotate it; this delta is
        already in world space, coming out of a real ray-plane
        intersection). Net effect: the world point that was under the
        cursor before the wheel event is still under it afterward,
        so zooming visually pulls in toward wherever the mouse
        actually is instead of always toward the fixed pan centre.

        Off by default (self._zoom_to_cursor, matching the setting's
        own MapSettings default) - preserves the existing, always-
        zooms-toward-pan-centre behaviour unless explicitly turned on."""
        zoom_to_cursor = getattr(self, '_zoom_to_cursor', False)
        before_pos = None
        if zoom_to_cursor:
            pos = event.position()
            before_pos = self._screen_to_ground_position(pos.x(), pos.y())

        f = 0.85 if event.angleDelta().y() > 0 else 1.15
        self._dist = max(0.1, min(50000.0, self._dist*f))

        if before_pos is not None:
            pos = event.position()
            after_pos = self._screen_to_ground_position(pos.x(), pos.y())
            if after_pos is not None:
                self._pan_x += after_pos[0] - before_pos[0]
                self._pan_y += after_pos[1] - before_pos[1]

        if self._projection == 'ortho':
            try:
                self.resizeGL(self.width(), self.height())
            except Exception:
                pass
        self.update()

    def set_zoom_to_cursor(self, enabled: bool): #vers 1
        """Toggle zoom-toward-mouse-cursor (Aug 18 2026, per Keith's
        own request - see wheelEvent's own docstring for the full
        mechanism). Off by default, matching the existing, always-
        zooms-toward-pan-centre behaviour."""
        self._zoom_to_cursor = enabled

    def keyPressEvent(self, event): #vers 2
        """Configurable camera controls, held keys giving continuous
        motion - per Keith: originally "arrow keys, and numpad to
        rotate" (Aug 1 2026), later corrected: "the arrow keys dont
        pan or move the view left, right, up or down; the arrow keys
        rotate instead. We need to be able to operate the tools with
        keys, zoom in and out; it could be the numpad + -" (Aug 16
        2026). Default bindings now: arrows pan, numpad 4/6/8/2
        rotate (unchanged from the original request), numpad +/-
        zoom - see DEFAULT_KEY_BINDINGS. A reliable keyboard
        alternative to drag-based camera control regardless of
        whatever's causing the reported mouse-button flakiness
        (middle-click pan and left-click-select both "don't always
        work", right-click rotate "just fine").

        Bindings are looked up from self._key_bindings (defaults to
        DEFAULT_KEY_BINDINGS, overridable via set_key_bindings) rather
        than hardcoded here, so Settings > Keybindings can rebind any
        of them. Numpad keys detected via KeypadModifier specifically
        (Qt doesn't otherwise distinguish a numpad "4" from a top-row
        "4" by key code alone) - only bindings with 'numpad': True
        require it, so arrow keys (numpad: False) still work
        regardless of NumLock state.

        Continuous motion while a key is held (not one fixed step per
        press) via a repeating QTimer, matching the smooth feel of
        drag-based rotation/pan rather than a discrete jump."""
        key = event.key()
        is_numpad = bool(event.modifiers() & Qt.KeyboardModifier.KeypadModifier)
        bindings = getattr(self, '_key_bindings', None) or DEFAULT_KEY_BINDINGS
        for action, spec in bindings.items():
            if spec['key'] == key and spec['numpad'] == is_numpad:
                held = getattr(self, '_camera_keys_held', None)
                if held is None:
                    held = self._camera_keys_held = {}
                held[key] = action
                self._ensure_camera_key_timer()
                return
        super().keyPressEvent(event)

    def keyReleaseEvent(self, event): #vers 1
        held = getattr(self, '_camera_keys_held', None)
        if held is not None and event.key() in held and not event.isAutoRepeat():
            del held[event.key()]
        super().keyReleaseEvent(event)

    def set_key_bindings(self, bindings: dict): #vers 1
        """Replace the viewport's camera keybindings (Aug 16 2026,
        per Keith's Settings > Keybindings request) - expects the
        same {action: {'key': int, 'numpad': bool}} shape as
        DEFAULT_KEY_BINDINGS; any action missing from the given dict
        keeps its default binding rather than becoming unbound, so a
        partial/older saved-settings dict doesn't silently disable
        actions added after it was saved."""
        merged = dict(DEFAULT_KEY_BINDINGS)
        merged.update(bindings or {})
        self._key_bindings = merged

    def _apply_pan_step(self, screen_dx, screen_dy): #vers 1
        """Apply one yaw-compensated pan step, in already-scaled
        screen-space units (positive screen_dx = pan right, positive
        screen_dy = pan up) - shared by mouseMoveEvent's middle-drag
        pan and keyPressEvent's keyboard pan (Aug 16 2026 refactor;
        see mouseMoveEvent's own comment for the full yaw-compensation
        reasoning) so both feel identical rather than risking drift
        between two separately-maintained copies of the same math."""
        rad = math.radians(-self._yaw)
        cos_a, sin_a = math.cos(rad), math.sin(rad)
        self._pan_x += screen_dx * cos_a - screen_dy * sin_a
        self._pan_y += screen_dx * sin_a + screen_dy * cos_a

    def _ensure_camera_key_timer(self): #vers 1
        """Start the repeating camera-control timer if it isn't
        already running - stops itself automatically once no camera
        keys are held anymore, rather than running an idle timer
        permanently for every viewport instance regardless of
        whether it's ever used."""
        timer = getattr(self, '_camera_key_timer', None)
        if timer is None:
            from PyQt6.QtCore import QTimer
            timer = self._camera_key_timer = QTimer(self)
            timer.timeout.connect(self._on_camera_key_tick)
        if not timer.isActive():
            timer.start(16)   # ~60fps

    def _on_camera_key_tick(self): #vers 1
        """One tick of continuous keyboard camera control - dispatches
        each currently-held action to the matching pan/rotate/zoom
        delta. Several actions can be held at once (e.g. panning
        diagonally by holding two arrow keys), each applied
        independently per tick."""
        held = getattr(self, '_camera_keys_held', None)
        if not held:
            timer = getattr(self, '_camera_key_timer', None)
            if timer is not None:
                timer.stop()
            return
        sens = getattr(self, '_mouse_sensitivity', 1.0)
        rotate_step = 2.0 * sens
        pan_step = self._dist * 0.016 * sens
        for action in held.values():
            if action == 'rotate_yaw_left' and not self._view_locked:
                self._yaw -= rotate_step
            elif action == 'rotate_yaw_right' and not self._view_locked:
                self._yaw += rotate_step
            elif action == 'rotate_pitch_up' and not self._view_locked:
                self._pitch -= rotate_step
            elif action == 'rotate_pitch_down' and not self._view_locked:
                self._pitch += rotate_step
            elif action == 'pan_left':
                self._apply_pan_step(-pan_step, 0)
            elif action == 'pan_right':
                self._apply_pan_step(pan_step, 0)
            elif action == 'pan_up':
                self._apply_pan_step(0, pan_step)
            elif action == 'pan_down':
                self._apply_pan_step(0, -pan_step)
            elif action == 'zoom_in':
                self._dist = max(0.1, self._dist * 0.985)
            elif action == 'zoom_out':
                self._dist = min(50000.0, self._dist * 1.015)
        self.update()

    # - Model Workshop compatibility methods
    # These map COL3DViewport API onto DFFViewport equivalents

    def zoom_in(self): #vers 3
        self._dist = max(0.1, self._dist * 0.8)
        if self._projection == 'ortho':
            try: self.resizeGL(self.width(), self.height())
            except Exception: pass
        self.update()

    def zoom_out(self): #vers 3
        self._dist = min(50000.0, self._dist * 1.25)
        if self._projection == 'ortho':
            try: self.resizeGL(self.width(), self.height())
            except Exception: pass
        self.update()

    def reset_view(self): #vers 1
        self._yaw=45.0; self._pitch=25.0; self._pan_x=0.0; self._pan_y=0.0
        self._auto_fit(); self.update()

    def fit_to_window(self): #vers 1
        self._auto_fit(); self.update()

    def pan(self, dx, dy): #vers 1
        scale = self._dist * 0.002
        self._pan_x += dx * scale; self._pan_y -= dy * scale; self.update()

    def set_show_mesh(self, v: bool): #vers 1
        # DFFViewport always shows mesh — no-op for compatibility
        self.update()

    def set_backface(self, v: bool): #vers 1
        self._backface_cull = not v; self.update()

    def flip_vertical(self): #vers 1
        self._vertices = [(x, -y, z) for x, y, z in self._vertices]; self.update()

    def flip_horizontal(self): #vers 1
        self._vertices = [(-x, y, z) for x, y, z in self._vertices]; self.update()

    def rotate_cw(self): #vers 1
        self._yaw = (self._yaw + 90) % 360; self.update()

    def rotate_ccw(self): #vers 1
        self._yaw = (self._yaw - 90) % 360; self.update()

    def set_current_model(self, model, index=0): #vers 1
        """Load a DFFModel directly — compatibility with COL3DViewport API."""
        if not model or not model.geometries: return
        g = model.geometries[min(index, len(model.geometries)-1)]
        self.load_geometry(g, g.materials)

    def set_checkerboard_background(self): #vers 2
        """No checkerboard rendering in GL mode — clear any colour override back to theme."""
        self._bg_color_override = None
        self.update()

    def set_background_color(self, color): #vers 2
        """Set an explicit background colour, overriding the theme colour.

        color: (r, g, b) tuple 0-255, or a QColor.
        """
        if hasattr(color, 'getRgb'):
            r, g, b, _ = color.getRgb()
            self._bg_color_override = (r, g, b)
        else:
            self._bg_color_override = tuple(color[:3])
        self.update()

    def _refresh(self): #vers 1
        self.update()


class VehicleViewport(DFFViewport):
    """DFFViewport + vehicle animation (doors, rotors, wheels)."""

    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self._anim_enabled      = False
        self._anim_timer        = None
        self._anim_speed        = 1.0
        self._anim_rates        = {'moving_rotor': 360.0, 'moving_rotor2': 360.0,
                                   'prop': 360.0, 'misc_a': 180.0, 'misc_b': 180.0}
        self._anim_frame_angles = {}
        self._anim_door_open    = {}
        self._wheel_heading     = 0.0
        self._dragging          = False
        from PyQt6.QtCore import Qt
        self._drag_btn          = Qt.MouseButton.NoButton

    def set_animation(self, enabled: bool): #vers 1
        self._anim_enabled = enabled
        if enabled:
            if self._anim_timer is None:
                from PyQt6.QtCore import QTimer
                self._anim_timer = QTimer(self)
                self._anim_timer.timeout.connect(self._anim_tick)
            self._anim_timer.start(33)
        else:
            if self._anim_timer: self._anim_timer.stop()
            self.update()

    def _anim_tick(self): #vers 1
        if not self._anim_enabled or not self._assembly_mode: return
        for fname, rate in self._anim_rates.items():
            cur = self._anim_frame_angles.get(fname, 0.0)
            self._anim_frame_angles[fname] = (cur + rate * self._anim_speed / 30.0) % 360.0
        self._rebuild_anim_geoms()

    def _rebuild_anim_geoms(self): #vers 1
        m = getattr(self, '_dff_model', None)
        if not m: return
        self.load_all_geometries(
            m.geometries, [g.materials for g in m.geometries],
            m.frames, m.atomics,
            damaged=getattr(self, '_damaged', False))

    def toggle_door(self, door_name: str): #vers 1
        self._anim_door_open[door_name] = not self._anim_door_open.get(door_name, False)
        self._rebuild_anim_geoms()

    def _get_anim_rotation(self, frame_name: str): #vers 1
        name = frame_name.lower()
        for key in ('moving_rotor', 'moving_rotor2', 'prop', 'misc_a', 'misc_b'):
            if key in name:
                angle = self._anim_frame_angles.get(key, 0.0)
                ca=math.cos(math.radians(angle)); sa=math.sin(math.radians(angle))
                return [ca,-sa,0, sa,ca,0, 0,0,1]
        for key in ('door_lf','door_rf','door_lr','door_rr','bonnet','boot'):
            if key in name:
                is_open = self._anim_door_open.get(name, False)
                angle = 70.0 if is_open else 0.0
                ca=math.cos(math.radians(angle)); sa=math.sin(math.radians(angle))
                return [1,0,0, 0,ca,-sa, 0,sa,ca]
        return None

    def set_animation_speed(self, speed: float): #vers 1
        self._anim_speed = max(0.1, speed)

    def set_wheel_heading(self, angle_deg: float): #vers 1
        self._wheel_heading = angle_deg
        if getattr(self,'_assembly_mode',False) and getattr(self,'_dff_model',None):
            m = self._dff_model
            self.load_all_geometries(m.geometries,[g.materials for g in m.geometries],
                                     m.frames, m.atomics, getattr(self,'_damaged',False))

    def load_wheels_dff(self, path: str, wheel_type: str = 'wheel_saloon_l0'): #vers 1
        try:
            try:
                from apps.methods.dff_parser import load_dff
            except ImportError:
                from apps.components.Vehicle_Workshop.depends.dff_parser import load_dff
            self._wheels_model = load_dff(path)
            self._wheel_type   = wheel_type
        except Exception as e:
            print(f'[VehicleViewport] wheels.DFF load fail: {e}')

    def _get_wheel_geom_data(self): #vers 1
        m = getattr(self, '_wheels_model', None)
        if not m: return None
        wtype = getattr(self, '_wheel_type', 'wheel_saloon_l0').lower()
        for a in m.atomics:
            fi = a.frame_index
            fname = (m.frames[fi].name or '').lower() if fi < len(m.frames) else ''
            if fname == wtype:
                g = m.geometries[a.geometry_index]
                return (
                    [(v.x,v.y,v.z) for v in g.vertices],
                    [(n.x,n.y,n.z) for n in g.normals] if g.normals else [],
                    [(u.u,u.v) for u in g.uv_layers[0]] if g.uv_layers else [],
                    [(t.v1,t.v2,t.v3,t.material_id) for t in g.triangles],
                    g.materials,
                    [(c.r,c.g,c.b,c.a) for c in g.colors] if g.colors else []
                )
        return None

    def mousePressEvent(self, event): #vers 1
        from PyQt6.QtCore import Qt
        self._last_pos = event.pos(); self._dragging=True; self._drag_btn=event.button()

    def mouseMoveEvent(self, event): #vers 1
        from PyQt6.QtCore import Qt
        if not self._dragging: return
        dx = event.pos().x()-self._last_pos.x()
        dy = event.pos().y()-self._last_pos.y()
        if self._drag_btn == Qt.MouseButton.LeftButton:
            self._yaw   += dx*0.5
            self._pitch  = max(-89, min(89, self._pitch+dy*0.5))
        elif self._drag_btn == Qt.MouseButton.MiddleButton:
            s = self._dist/500.0
            self._pan_x += dx*s; self._pan_y -= dy*s
        self._last_pos = event.pos(); self.update()

    def mouseReleaseEvent(self, event): #vers 1
        from PyQt6.QtCore import Qt
        self._dragging=False; self._drag_btn=Qt.MouseButton.NoButton

    def wheelEvent(self, event): #vers 1
        f = 0.88 if event.angleDelta().y()>0 else 1.13
        self._dist = max(0.1, min(50000.0, self._dist*f)); self.update()
