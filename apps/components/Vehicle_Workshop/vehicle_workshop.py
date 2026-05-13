#!/usr/bin/env python3
#this belongs in apps/components/Vehicle_Workshop/vehicle_workshop.py - Version: 2
# X-Seti - May08 2026 - Img Factory 1.6 - Vehicle Workshop

"""
Vehicle Workshop — tabbed editor for GTA III/VC/SA vehicle data files.
Tabs: Handling (handling.cfg) | Car Colours (carcols.dat) | Car Mods (carmods.dat SA)

Parsers are self-contained. Handling parser imported from Handling_Editor.
"""

##Methods list -
# CarColour.__init__
# CarColsParser.__init__
# CarColsParser.load
# CarColsParser.save
# CarColsParser._detect_game
# CarModEntry.__init__
# CarModsParser.__init__
# CarModsParser.load
# CarModsParser.save
# ColourSwatchGrid.__init__
# ColourSwatchGrid.set_palette
# ColourSwatchGrid.paintEvent
# ColourSwatchGrid.mousePressEvent
# HandlingTab.__init__
# HandlingTab._build_ui
# HandlingTab.load_file
# HandlingTab.save_file
# CarColoursTab.__init__
# CarColoursTab._build_ui
# CarColoursTab.load_file
# CarColoursTab.save_file
# CarColoursTab._on_vehicle_selected
# CarColoursTab._populate_vehicle_colours
# CarColoursTab._on_colour_index_clicked
# CarColoursTab._add_vehicle
# CarColoursTab._delete_vehicle
# CarColoursTab._edit_selected_colour
# CarColoursTab._add_pair
# CarColoursTab._remove_pair
# CarModsTab.__init__
# CarModsTab._build_ui
# CarModsTab.load_file
# CarModsTab.save_file
# CarModsTab._on_vehicle_selected
# CarModsTab._populate_mods
# CarModsTab._add_vehicle
# CarModsTab._delete_vehicle
# CarModsTab._add_mod
# CarModsTab._remove_mod
# VehicleWorkshop.__init__
# VehicleWorkshop._build_menus_into_qmenu
# VehicleWorkshop._open_file
# VehicleWorkshop._save_file
# VehicleWorkshop._save_as
# VehicleWorkshop._open_specific
# VehicleWorkshop.setup_ui
# open_vehicle_workshop

import math, sys, os, json
from pathlib import Path
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass, field

current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = Path(current_dir).parents[2]
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QSplitter, QLabel, QLineEdit,
    QListWidget, QListWidgetItem, QScrollArea, QGroupBox, QTabWidget,
    QDoubleSpinBox, QSpinBox, QComboBox, QCheckBox, QPushButton,
    QFileDialog, QMessageBox, QApplication, QFormLayout, QFrame,
    QTableWidget, QTableWidgetItem, QHeaderView, QAbstractItemView,
    QColorDialog, QProgressBar, QMenu, QSizePolicy, QToolButton,
    QDialog, QFontComboBox, QDialogButtonBox, QTextEdit, QButtonGroup
)

from PyQt6.QtCore import Qt, QSize, QPoint, pyqtSignal, QTimer
from PyQt6.QtGui import QColor, QFont, QPainter, QBrush, QPen, QIcon, QKeySequence, QShortcut, QPolygon


try:
    from PyQt6.QtOpenGLWidgets import QOpenGLWidget
    from PyQt6.QtOpenGL import QOpenGLContext
    from PyQt6.QtGui import QSurfaceFormat
    from OpenGL.GL  import *
    from OpenGL.GLU import *
    OPENGL_AVAILABLE = True
    # Request compatibility profile globally — required for fixed-function GL
    # (glMatrixMode, glLightfv, glBegin etc.) on PyQt6 / Python 3.12+
    _fmt = QSurfaceFormat()
    _fmt.setProfile(QSurfaceFormat.OpenGLContextProfile.CompatibilityProfile)
    _fmt.setVersion(2, 1)
    QSurfaceFormat.setDefaultFormat(_fmt)
except Exception:
    QOpenGLWidget      = QWidget
    OPENGL_AVAILABLE   = False
    print("[Vehicle_Workshop] PyOpenGL not available — install python3-opengl")

# ── Path setup ───────────────────────────────────────────────────────────────
# depends/ = tool-specific only (handling_editor, svg_icons, tool_menu_mixin)
# apps/ = shared (methods, utils, themes) — via project_root when in IMG Factory
_depends = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'depends')
if _depends not in sys.path:
    sys.path.insert(0, _depends)

# AppSettings — apps/utils/ (shipped with standalone tools per architecture)
APPSETTINGS_AVAILABLE = False
try:
    from apps.utils.app_settings_system import AppSettings, SettingsDialog
    APPSETTINGS_AVAILABLE = True
except ImportError:
    try:
        from app_settings_system import AppSettings, SettingsDialog
        APPSETTINGS_AVAILABLE = True
    except ImportError:
        AppSettings = SettingsDialog = None

# SVGIconFactory — depends/ tool-specific copy
ICONS_AVAILABLE = False
try:
    from imgfactory_svg_icons import SVGIconFactory
    ICONS_AVAILABLE = True
except ImportError:
    try:
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        ICONS_AVAILABLE = True
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

# ToolMenuMixin — depends/ tool-specific copy
try:
    from tool_menu_mixin import ToolMenuMixin
except ImportError:
    try:
        from apps.gui.tool_menu_mixin import ToolMenuMixin
    except ImportError:
        class ToolMenuMixin:
            def _build_menus_into_qmenu(self, pm): pass

# GLViewportMixin — embedded in this file
class GLViewportMixin: pass

# HandlingParser — depends/ tool-specific copy
try:
    from handling_editor import HandlingParser, HandlingEntry, VC_FIELDS, HANDLING_FLAGS
    _HANDLING_AVAILABLE = True
except ImportError:
    try:
        from apps.components.Handling_Editor.handling_editor import (
            HandlingParser, HandlingEntry, VC_FIELDS, HANDLING_FLAGS
        )
        _HANDLING_AVAILABLE = True
    except ImportError:
        _HANDLING_AVAILABLE = False

    _HANDLING_AVAILABLE = False

# - Detect standalone vs docked
def _is_standalone():
    import inspect
    frame = inspect.currentframe()
    try:
        for _ in range(10):
            frame = frame.f_back
            if frame is None: break
            if 'imgfactory' in frame.f_code.co_filename.lower(): return False
        return True
    finally:
        del frame

STANDALONE_MODE = _is_standalone()
DEBUG_STANDALONE = False


App_name   = "Vehicle Workshop"
App_build  = "Build 2"
App_auth   = "X-Seti"
config_key = "vehicle_workshop"


#    WorkshopSettings
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


# - OpenGL viewport
class DFFViewport(QOpenGLWidget if OPENGL_AVAILABLE else QWidget):
    """Hardware OpenGL viewport for DFF geometry."""

    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self.app_settings = None

        self._vertices:  List  = []
        self._normals:   List  = []
        self._uvs:       List  = []
        self._triangles: List  = []
        self._materials: List  = []
        self._prelit:    List  = []   # list of (r,g,b,a) 0-255
        self._tex_ids:   Dict[str,int] = {}

        # Camera
        self._dist  = 10.0
        self._yaw   = 45.0
        self._pitch = 25.0
        self._pan_x = 0.0
        self._pan_y = 0.0

        # Mouse
        self._last_pos  = QPoint()
        self._dragging  = False
        self._drag_btn  = Qt.MouseButton.NoButton

        # Render state
        self._mode          = 'solid'
        self._backface_cull = True
        self._show_grid     = True
        self._use_prelight  = False
        self._assembly_mode = False
        self._all_geoms     = []
        self._show_lod      = False   # hide _vlo by default
        # Vehicle paint preview colours (user can change via Light Setup or future colour picker)
        self._paint1 = (0.80, 0.20, 0.20)  # primary   — default red
        self._paint2 = (0.20, 0.20, 0.80)  # secondary — default blue
        self._light_dir     = (1.0, 2.0, 1.5, 0.0)   # GL_POSITION homogeneous
        self._ambient       = 0.30
        self._diffuse       = 0.85

        self.setMinimumSize(400, 300)
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)

    def _get_ui_color(self, key): #vers 2
        try:
            if self.app_settings and hasattr(self.app_settings, 'get_ui_color'):
                return self.app_settings.get_ui_color(key)
        except Exception:
            pass
        defaults = {
            'viewport_bg': QColor(18, 20, 28),
            'grid':        QColor(45, 50, 65),
        }
        return defaults.get(key, QColor(200, 200, 200))


    # - OpenGL lifecycle
    def initializeGL(self): #vers 1
        if not OPENGL_AVAILABLE: return
        bg = self._get_ui_color('viewport_bg')
        glClearColor(bg.redF(), bg.greenF(), bg.blueF(), 1.0)
        glEnable(GL_DEPTH_TEST)
        glDepthFunc(GL_LEQUAL)
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
        glShadeModel(GL_SMOOTH)
        glEnable(GL_NORMALIZE)
        self._setup_lighting()

    def _setup_lighting(self): #vers 1
        glEnable(GL_LIGHTING)
        glEnable(GL_LIGHT0)
        glEnable(GL_COLOR_MATERIAL)
        glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE)
        glLightfv(GL_LIGHT0, GL_POSITION, self._light_dir)
        glLightfv(GL_LIGHT0, GL_AMBIENT,  [self._ambient]*3 + [1.0])
        glLightfv(GL_LIGHT0, GL_DIFFUSE,  [self._diffuse]*3 + [1.0])
        glLightfv(GL_LIGHT0, GL_SPECULAR, [0.15, 0.15, 0.15, 1.0])

    def resizeGL(self, w, h): #vers 1
        if not OPENGL_AVAILABLE: return
        glViewport(0, 0, w, h)
        glMatrixMode(GL_PROJECTION); glLoadIdentity()
        aspect = w / h if h > 0 else 1.0
        gluPerspective(45.0, aspect, 0.01, 50000.0)
        glMatrixMode(GL_MODELVIEW)

    def paintGL(self): #vers 2
        if not OPENGL_AVAILABLE: return
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
        glLoadIdentity()
        glTranslatef(self._pan_x, self._pan_y, -self._dist)
        glRotatef(self._pitch, 1, 0, 0)
        glRotatef(self._yaw,   0, 1, 0)

        # Grid drawn in OpenGL world space (Y-up) — before GTA rotation
        if self._show_grid:
            self._draw_grid()

        if not self._vertices:
            if self._show_grid: self._draw_axes()
            return

        # GTA Z-up/Y-forward → OpenGL Y-up/Z-forward (model only)
        glPushMatrix()
        glRotatef(-90, 1, 0, 0)
        if self._show_grid: self._draw_axes()

        if self._backface_cull:
            glEnable(GL_CULL_FACE); glCullFace(GL_BACK)
        else:
            glDisable(GL_CULL_FACE)

        glLightfv(GL_LIGHT0, GL_POSITION, self._light_dir)

        if getattr(self,'_assembly_mode',False) and getattr(self,'_all_geoms',[]):
            self._draw_assembly()
        elif self._mode == 'wireframe': self._draw_wireframe()
        elif self._mode == 'solid':     self._draw_solid()
        elif self._mode == 'textured':  self._draw_textured()
        glPopMatrix()


    # - draw calls
    def _draw_grid(self): #vers 1
        glDisable(GL_LIGHTING)
        gc = self._get_ui_color('grid')
        glColor3f(gc.redF(), gc.greenF(), gc.blueF())
        glLineWidth(1.0)
        step  = max(0.5, self._dist / 20.0)
        count = 20; half = count * step
        glBegin(GL_LINES)
        for i in range(-count, count + 1):
            p = i * step
            glVertex3f(p, 0, -half); glVertex3f(p, 0,  half)
            glVertex3f(-half, 0, p); glVertex3f( half, 0, p)
        glEnd()

    def _draw_axes(self): #vers 1
        glDisable(GL_LIGHTING); glLineWidth(2.0)
        r = self._dist * 0.15
        glBegin(GL_LINES)
        glColor3f(1,0,0); glVertex3f(0,0,0); glVertex3f(r,0,0)
        glColor3f(0,1,0); glVertex3f(0,0,0); glVertex3f(0,r,0)
        glColor3f(0,0,1); glVertex3f(0,0,0); glVertex3f(0,0,r)
        glEnd()

    def _face_color(self, mat_id): #vers 5
        """Return (r,g,b,a) 0-1 for a material including alpha for glass/transparency."""
        mats = self._materials
        if mats and 0 <= mat_id < len(mats):
            mat = mats[mat_id]
            c = mat.colour
            r=getattr(c,'r',180); g=getattr(c,'g',180); b=getattr(c,'b',180); a=getattr(c,'a',255)
            has_tex = bool(getattr(mat,'texture_name',''))
            if r==0 and g==0 and b==0 and not has_tex:
                return 0.55, 0.55, 0.55, 1.0
            if g==255 and r<100 and b<50:  # primary paint slot
                return self._paint1[0], self._paint1[1], self._paint1[2], a/255
            if r==255 and g<50 and b>100:  # secondary paint slot
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
                u = uvs[vi]; glTexCoord2f(u[0], 1.0 - u[1])
            v = verts[vi]; glVertex3f(v[0], v[1], v[2])

    def _draw_wireframe(self): #vers 1
        glDisable(GL_LIGHTING)
        glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
        glColor3f(0.65, 0.75, 1.0); glLineWidth(0.8)
        glBegin(GL_TRIANGLES)
        for v1,v2,v3,mid in self._triangles:
            self._emit_verts(v1,v2,v3)
        glEnd()
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)

    def _draw_solid(self): #vers 2
        glEnable(GL_LIGHTING)
        use_p = self._use_prelight and self._prelit
        # Opaque first, then transparent (alpha < 1) for correct blending
        opaque=[]; transparent=[]
        for tri in self._triangles:
            fc=self._face_color(tri[3])
            (transparent if len(fc)>3 and fc[3]<0.99 else opaque).append((tri,fc))
        glBegin(GL_TRIANGLES)
        for (v1,v2,v3,mid),(r,g,b,*rest) in opaque:
            if not use_p: glColor4f(r,g,b,1.0)
            self._emit_verts(v1,v2,v3, use_prelit=use_p)
        glEnd()
        if transparent:
            glEnable(GL_BLEND); glDepthMask(False)
            glBegin(GL_TRIANGLES)
            for (v1,v2,v3,mid),(r,g,b,a) in transparent:
                if not use_p: glColor4f(r,g,b,a)
                self._emit_verts(v1,v2,v3, use_prelit=use_p)
            glEnd()
            glDepthMask(True); glDisable(GL_BLEND)

        # Wireframe overlay
        glDisable(GL_LIGHTING)
        glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
        glColor4f(0,0,0,0.18); glLineWidth(0.5)
        glEnable(GL_POLYGON_OFFSET_LINE); glPolygonOffset(-1,-1)
        glBegin(GL_TRIANGLES)
        for v1,v2,v3,mid in self._triangles:
            self._emit_verts(v1,v2,v3)
        glEnd()
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
        glDisable(GL_POLYGON_OFFSET_LINE)

    def _draw_textured(self): #vers 2
        glEnable(GL_LIGHTING); glEnable(GL_TEXTURE_2D)
        # GL_MODULATE: final = texture_colour * gl_colour (enables paint tinting)
        glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE)
        use_p = self._use_prelight and self._prelit
        mats  = self._materials

        # Group triangles by (gl_tex_id, mat_colour) so we batch correctly
        # Key: (gl_id, r, g, b) — same texture+colour batched together
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

        # Sort: opaque batches first, then transparent
        opaque_b = {k:v for k,v in batches.items() if len(k)<5 or k[4]>=0.99}
        transp_b = {k:v for k,v in batches.items() if len(k)>=5 and k[4]<0.99}
        for batch_dict, use_blend in [(opaque_b,False),(transp_b,True)]:
            if use_blend: glEnable(GL_BLEND); glDepthMask(False)
            for key, tris in batch_dict.items():
                gl_id=key[0]; r=key[1]; g=key[2]; b=key[3]
                a = key[4] if len(key)>4 else 1.0
                glBindTexture(GL_TEXTURE_2D, gl_id)
                if not use_p: glColor4f(r, g, b, a)
                glBegin(GL_TRIANGLES)
                for v1,v2,v3,mid in tris:
                    self._emit_verts(v1,v2,v3, use_prelit=use_p, use_uv=True)
                glEnd()
            if use_blend: glDepthMask(True); glDisable(GL_BLEND)

        glBindTexture(GL_TEXTURE_2D, 0); glDisable(GL_TEXTURE_2D)
        no_opaque=[t for t in no_tex if self._face_color(t[3])[3]>=0.99]
        no_transp=[t for t in no_tex if self._face_color(t[3])[3]<0.99]
        for tri_list, use_blend in [(no_opaque,False),(no_transp,True)]:
            if use_blend: glEnable(GL_BLEND); glDepthMask(False)
            for v1,v2,v3,mid in tri_list:
                r,g,b,a = self._face_color(mid)
                if not use_p: glColor4f(r,g,b,a)
                glBegin(GL_TRIANGLES)
                self._emit_verts(v1,v2,v3, use_prelit=use_p)
                glEnd()
            if use_blend: glDepthMask(True); glDisable(GL_BLEND)
        glEnable(GL_TEXTURE_2D)


    # - textures
    def _upload_textures(self, textures: list): #vers 1
        if not OPENGL_AVAILABLE: return
        self.makeCurrent(); self.clear_textures()
        for tex in textures:
            name = tex.get('name','').lower()
            rgba = tex.get('rgba_data', b'')
            w    = tex.get('width', 0); h = tex.get('height', 0)
            if not (name and rgba and w > 0 and h > 0): continue
            gl_id = glGenTextures(1)
            glBindTexture(GL_TEXTURE_2D, gl_id)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
            try:
                glTexImage2D(GL_TEXTURE_2D,0,GL_RGBA,w,h,0,GL_RGBA,GL_UNSIGNED_BYTE,rgba)
                glGenerateMipmap(GL_TEXTURE_2D)
                self._tex_ids[name] = gl_id
            except Exception as e:
                print(f"[ModelViewer] Tex upload fail '{name}': {e}")
                glDeleteTextures(1,[gl_id])
        glBindTexture(GL_TEXTURE_2D, 0); self.doneCurrent()

    def clear_textures(self): #vers 1
        if OPENGL_AVAILABLE and self._tex_ids:
            try: glDeleteTextures(len(self._tex_ids), list(self._tex_ids.values()))
            except Exception: pass
        self._tex_ids.clear()


    # - public API
    def load_geometry(self, geometry, materials: list): #vers 1
        self._vertices  = [(v.x,v.y,v.z) for v in geometry.vertices]
        self._normals   = [(n.x,n.y,n.z) for n in geometry.normals] if geometry.normals else []
        self._uvs       = [(u.u,u.v) for u in geometry.uv_layers[0]] if geometry.uv_layers else []
        self._triangles = [(t.v1,t.v2,t.v3,t.material_id) for t in geometry.triangles]
        self._materials = materials
        self._prelit    = [(c.r,c.g,c.b,c.a) for c in getattr(geometry,'colors',[])] if geometry.colors else []
        self._auto_fit(); self.update()

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

    def load_all_geometries(self, geometries, materials_list, frames, atomics, damaged=False): #vers 2
        # Load all geometries with world transforms. damaged=True shows _dam parts.
        self._all_geoms = []
        fname = {i: (f.name.lower() if f.name else '') for i,f in enumerate(frames)}
        for i, geom in enumerate(geometries):
            atomic = next((a for a in atomics if a.geometry_index == i), None)
            if not atomic: continue
            fi = atomic.frame_index
            name = fname.get(fi, '')
            is_dam = name.endswith('_dam') or name.endswith('_hi_dam')
            is_ok  = name.endswith('_ok')  or name.endswith('_hi_ok')
            is_lod = name.endswith('_vlo') or name.endswith('_lo') or name=='chassis_vlo'
            if name in getattr(self,'_hidden_frames',set()): continue
            if is_dam and not damaged: continue
            if is_ok  and damaged: continue
            if is_lod and not getattr(self,'_show_lod',False): continue
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
            self._all_geoms.append((verts,norms,uvs,tris,geom.materials,prelit))
            # Wheel instancing: place at all wheel dummy frames
            # Use wheels.DFF geometry if available, else repeat vehicle's own wheel mesh
            if 'wheel' in name and not is_dam and not is_ok and 'dummy' not in name:
                wheel_data = self._get_wheel_geom_data()
                for fi2, fn2 in fname.items():
                    if fi2 == fi: continue
                    if 'wheel' in fn2 and 'dummy' in fn2:
                        r2,tx2,ty2,tz2 = self._calc_world_matrix(frames, fi2)
                        is_left = fn2.startswith('wheel_l')
                        # Apply steering angle to front wheels
                        is_front = 'lf' in fn2 or 'rf' in fn2
                        if is_front:
                            angle = getattr(self,'_wheel_heading',0.0)
                            if angle:
                                ca=math.cos(math.radians(angle)); sa=math.sin(math.radians(angle))
                                # Rotate around Z axis (GTA Z-up)
                                steer=[ca,-sa,0, sa,ca,0, 0,0,1]
                                r2=[r2[0]*steer[0]+r2[1]*steer[3]+r2[2]*steer[6],
                                    r2[0]*steer[1]+r2[1]*steer[4]+r2[2]*steer[7],
                                    r2[0]*steer[2]+r2[1]*steer[5]+r2[2]*steer[8],
                                    r2[3]*steer[0]+r2[4]*steer[3]+r2[5]*steer[6],
                                    r2[3]*steer[1]+r2[4]*steer[4]+r2[5]*steer[7],
                                    r2[3]*steer[2]+r2[4]*steer[5]+r2[5]*steer[8],
                                    r2[6]*steer[0]+r2[7]*steer[3]+r2[8]*steer[6],
                                    r2[6]*steer[1]+r2[7]*steer[4]+r2[8]*steer[7],
                                    r2[6]*steer[2]+r2[7]*steer[5]+r2[8]*steer[8]]
                        if wheel_data:
                            wv,wn,wu,wt,wm,wp = wheel_data
                            # Transform wheel.DFF verts to this dummy's world position
                            v2=[(r2[0]*vx+r2[1]*vy+r2[2]*vz+tx2,
                                 r2[3]*vx+r2[4]*vy+r2[5]*vz+ty2,
                                 r2[6]*vx+r2[7]*vy+r2[8]*vz+tz2) for vx,vy,vz in wv]
                            if is_left: v2=[(-vx,vy,vz) for vx,vy,vz in v2]
                            self._all_geoms.append((v2,wn,wu,wt,wm,wp))
                        else:
                            # Fallback: repeat vehicle's own wheel mesh
                            v2=[(r2[0]*v.x+r2[1]*v.y+r2[2]*v.z+tx2,
                                 r2[3]*v.x+r2[4]*v.y+r2[5]*v.z+ty2,
                                 r2[6]*v.x+r2[7]*v.y+r2[8]*v.z+tz2) for v in geom.vertices]
                            if is_left: v2=[(-vx,vy,vz) for vx,vy,vz in v2]
                            self._all_geoms.append((v2,norms,uvs,tris,geom.materials,prelit))
        # VC/LC: place wheels.DFF at dummies even when no wheel atomic present
        placed_wheels=any('wheel' in fname.get(a.frame_index,'') and 'dummy' not in fname.get(a.frame_index,'') for a in atomics)
        if not placed_wheels:
            wheel_data=self._get_wheel_geom_data()
            if wheel_data:
                for fi2,fn2 in fname.items():
                    if 'wheel' in fn2 and 'dummy' in fn2:
                        r2,tx2,ty2,tz2=self._calc_world_matrix(frames,fi2)
                        wv,wn,wu,wt,wm,wp=wheel_data
                        v2=[(r2[0]*vx+r2[1]*vy+r2[2]*vz+tx2,r2[3]*vx+r2[4]*vy+r2[5]*vz+ty2,r2[6]*vx+r2[7]*vy+r2[8]*vz+tz2) for vx,vy,vz in wv]
                        if fn2.startswith('wheel_l'): v2=[(-vx,vy,vz) for vx,vy,vz in v2]
                        self._all_geoms.append((v2,wn,wu,wt,wm,wp))
        all_pts=[p for g in self._all_geoms for p in g[0]]
        if all_pts:
            xs=[p[0] for p in all_pts]; ys=[p[1] for p in all_pts]
            diag=math.sqrt(max(1,(max(xs)-min(xs))**2+(max(ys)-min(ys))**2))
            self._dist=max(diag*2.0,2.0)
            self._pan_x=-(max(xs)+min(xs))/2; self._pan_y=-(max(ys)+min(ys))/2
        self.update()

    def _calc_world_matrix(self, frames, frame_idx): #vers 1
        """Compute cumulative world matrix for a frame chain."""
        r=[1,0,0,0,1,0,0,0,1]; tx=ty=tz=0.0
        visited=set(); idx=frame_idx; chain=[]
        while 0<=idx<len(frames) and idx not in visited:
            visited.add(idx); chain.append(frames[idx]); idx=frames[idx].parent_index
        for frame in reversed(chain):
            fr=frame.rotation; fp=frame.position
            nr=[r[0]*fr[0]+r[1]*fr[3]+r[2]*fr[6],r[0]*fr[1]+r[1]*fr[4]+r[2]*fr[7],r[0]*fr[2]+r[1]*fr[5]+r[2]*fr[8],
                r[3]*fr[0]+r[4]*fr[3]+r[5]*fr[6],r[3]*fr[1]+r[4]*fr[4]+r[5]*fr[7],r[3]*fr[2]+r[4]*fr[5]+r[5]*fr[8],
                r[6]*fr[0]+r[7]*fr[3]+r[8]*fr[6],r[6]*fr[1]+r[7]*fr[4]+r[8]*fr[7],r[6]*fr[2]+r[7]*fr[5]+r[8]*fr[8]]
            ntx=r[0]*fp.x+r[1]*fp.y+r[2]*fp.z+tx; nty=r[3]*fp.x+r[4]*fp.y+r[5]*fp.z+ty
            ntz=r[6]*fp.x+r[7]*fp.y+r[8]*fp.z+tz; r,tx,ty,tz=nr,ntx,nty,ntz
        return r,tx,ty,tz


    def set_wheel_heading(self, angle_deg: float): #vers 1
        self._wheel_heading = angle_deg
        if getattr(self,'_assembly_mode',False) and getattr(self,'_dff_model',None):
            m=self._dff_model
            self.load_all_geometries(m.geometries,[g.materials for g in m.geometries],
                                     m.frames,m.atomics,getattr(self,'_damaged',False))

    def load_wheels_dff(self, path: str, wheel_type: str = 'wheel_saloon_l0'): #vers 1

        try:
            from apps.methods.dff_parser import load_dff
            self._wheels_model = load_dff(path)
            self._wheel_type   = wheel_type
        except Exception as e:
            print(f'[wheels.DFF] {e}')

    def _get_wheel_geom_data(self): #vers 1
        """Return geometry tuple for current wheel type from wheels.DFF."""
        m = getattr(self,'_wheels_model',None)
        if not m: return None
        wtype = getattr(self,'_wheel_type','wheel_saloon_l0').lower()
        for a in m.atomics:
            fi = a.frame_index
            fname = (m.frames[fi].name or '').lower() if fi<len(m.frames) else ''
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

    def set_assembly_mode(self, enabled: bool): #vers 1
        self._assembly_mode = enabled; self.update()

    def set_show_lod(self, enabled: bool): #vers 1
        self._show_lod = enabled; self.update()

    def _draw_assembly(self): #vers 1
        """Draw all geometries at their world positions."""
        if not OPENGL_AVAILABLE: return
        for verts,norms,uvs,tris,mats,prelit in getattr(self,'_all_geoms',[]):
            old_v,old_n,old_u,old_t,old_m,old_p = (
                self._vertices,self._normals,self._uvs,
                self._triangles,self._materials,self._prelit)
            self._vertices=verts; self._normals=norms; self._uvs=uvs
            self._triangles=tris; self._materials=mats; self._prelit=prelit
            if   self._mode=='wireframe': self._draw_wireframe()
            elif self._mode=='solid':     self._draw_solid()
            elif self._mode=='textured':  self._draw_textured()
            (self._vertices,self._normals,self._uvs,
             self._triangles,self._materials,self._prelit) = (old_v,old_n,old_u,old_t,old_m,old_p)

    def set_prelight(self, v: bool): #vers 1
        self._use_prelight = v; self.update()

    def set_light_dir(self, x, y, z): #vers 1
        self._light_dir = (x, y, z, 0.0)
        if OPENGL_AVAILABLE and self.isVisible():
            self.makeCurrent()
            glLightfv(GL_LIGHT0, GL_POSITION, self._light_dir)
            self.doneCurrent()
        self.update()

    def set_ambient(self, v: float): #vers 1
        self._ambient = v
        if OPENGL_AVAILABLE and self.isVisible():
            self.makeCurrent()
            glLightfv(GL_LIGHT0, GL_AMBIENT, [v,v,v,1.0])
            self.doneCurrent()
        self.update()

    def set_diffuse(self, v: float): #vers 1
        self._diffuse = v
        if OPENGL_AVAILABLE and self.isVisible():
            self.makeCurrent()
            glLightfv(GL_LIGHT0, GL_DIFFUSE, [v,v,v,1.0])
            self.doneCurrent()
        self.update()

    def reset_camera(self): #vers 1
        self._yaw=45.0; self._pitch=25.0; self._pan_x=0.0; self._pan_y=0.0
        self._auto_fit()


    # - mouse
    def mousePressEvent(self, event): #vers 1
        self._last_pos = event.pos(); self._dragging=True; self._drag_btn=event.button()

    def mouseMoveEvent(self, event): #vers 1
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
        self._dragging=False; self._drag_btn=Qt.MouseButton.NoButton

    def wheelEvent(self, event): #vers 1
        f = 0.88 if event.angleDelta().y()>0 else 1.13
        self._dist = max(0.1, min(50000.0, self._dist*f)); self.update()


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


    def _toggle_assembly_mode(self, enabled: bool): #vers 2
        self.viewport.set_assembly_mode(enabled)
        if enabled and self._dff_model:
            damaged = getattr(self,'_damage_mode',False)
            self.viewport.load_all_geometries(
                self._dff_model.geometries,
                [g.materials for g in self._dff_model.geometries],
                self._dff_model.frames,
                self._dff_model.atomics,
                damaged=damaged)
        self._geom_list.setEnabled(not enabled)

    def _toggle_damage_mode(self, enabled: bool): #vers 1
        self._damage_mode = enabled
        if getattr(self,'_assemble_btn',None) and self._assemble_btn.isChecked():
            self._toggle_assembly_mode(True)

    def _toggle_lod_mode(self, enabled: bool): #vers 1
        self.viewport.set_show_lod(enabled)
        if getattr(self,'_assemble_btn',None) and self._assemble_btn.isChecked():
            self._toggle_assembly_mode(True)

    def _update_paint_btns(self): #vers 1
        vp = self.viewport
        def _css(rgb): return f'background-color:rgb({int(rgb[0]*255)},{int(rgb[1]*255)},{int(rgb[2]*255)});color:{"#000" if sum(rgb)>1.5 else "#fff"}'
        if hasattr(self,'_paint1_btn'): self._paint1_btn.setStyleSheet(_css(vp._paint1))
        if hasattr(self,'_paint2_btn'): self._paint2_btn.setStyleSheet(_css(vp._paint2))

    def _pick_paint1(self): #vers 1
        from PyQt6.QtWidgets import QColorDialog
        from PyQt6.QtGui import QColor
        vp = self.viewport
        col = QColorDialog.getColor(QColor(int(vp._paint1[0]*255),int(vp._paint1[1]*255),int(vp._paint1[2]*255)),self)
        if col.isValid(): vp._paint1=(col.redF(),col.greenF(),col.blueF()); self._update_paint_btns(); vp.update()

    def _pick_paint2(self): #vers 1
        from PyQt6.QtWidgets import QColorDialog
        from PyQt6.QtGui import QColor
        vp = self.viewport
        col = QColorDialog.getColor(QColor(int(vp._paint2[0]*255),int(vp._paint2[1]*255),int(vp._paint2[2]*255)),self)
        if col.isValid(): vp._paint2=(col.redF(),col.greenF(),col.blueF()); self._update_paint_btns(); vp.update()

    def _set_paint_pair(self, p1, p2): #vers 1
        self.viewport._paint1=p1; self.viewport._paint2=p2
        self._update_paint_btns(); self.viewport.update()

    def _load_vehicle_meta(self, vehicle_name: str): #vers 1
        """Load vehicles.ide (wheel type) and carcols colours for vehicle."""
        game_root = self._get_game_root()
        # vehicles.ide — wheel type
        if game_root:
            try:
                from apps.methods.vehicles_ide_parser import get_vehicle_info
                entry = get_vehicle_info(game_root, vehicle_name)
                if entry and entry.wheel_model:
                    wheel_dff = entry.wheel_dff_name()
                    self.viewport._wheel_type = wheel_dff
                    self._set_status(f'IDE: {vehicle_name} txd={entry.txd_name} wheel={wheel_dff}')
            except Exception as e:
                pass
        self._load_carcols(vehicle_name)

    def _get_game_root(self): #vers 1
        """Get game root from viewport or main_window."""
        game_root = ''
        if hasattr(self.viewport,'_find_game_root'):
            game_root = self.viewport._find_game_root()
        if not game_root:
            mw = self.main_window
            if mw:
                for attr in ('game_root','_game_root','game_directory'):
                    val=getattr(mw,attr,None)
                    if val and os.path.isdir(str(val)): game_root=str(val); break
        return game_root

    def _load_carcols(self, vehicle_name: str): #vers 1

        lay = getattr(self,'_carcols_lay',None)
        if not lay: return
        while lay.count():
            item = lay.takeAt(0)
            if item.widget(): item.widget().deleteLater()
        game_root = self._get_game_root()
        if not game_root: return
        try:
            from apps.methods.carcols_parser import get_vehicle_colours
            pairs = get_vehicle_colours(game_root, vehicle_name)
            if not pairs: return
            lbl = QLabel(f'Carcols ({len(pairs)} pairs)')
            lbl.setFont(self.infobar_font); lay.addWidget(lbl)
            for i,(p1,p2) in enumerate(pairs[:8]):
                row=QWidget(); rl=QHBoxLayout(row)
                rl.setContentsMargins(0,0,0,0); rl.setSpacing(2)
                def _css(rgb): return f'background-color:rgb({int(rgb[0]*255)},{int(rgb[1]*255)},{int(rgb[2]*255)});min-height:16px;border:1px solid #333'
                b1=QPushButton(); b1.setFixedSize(20,16); b1.setStyleSheet(_css(p1))
                b2=QPushButton(); b2.setFixedSize(20,16); b2.setStyleSheet(_css(p2))
                b1.setToolTip(f'Primary: rgb({int(p1[0]*255)},{int(p1[1]*255)},{int(p1[2]*255)})')
                b2.setToolTip(f'Secondary: rgb({int(p2[0]*255)},{int(p2[1]*255)},{int(p2[2]*255)})')
                ab=QPushButton(f'#{i+1}'); ab.setFixedHeight(16); ab.setFont(self.infobar_font)
                ab.setToolTip(f'Apply colour pair {i+1}')
                ab.clicked.connect(lambda _=False,a=p1,b=p2: self._set_paint_pair(a,b))
                rl.addWidget(b1); rl.addWidget(b2); rl.addWidget(ab,1)
                lay.addWidget(row)
        except Exception as e:
            print(f'[carcols] {e}')


    def _set_mode(self, mode: str): #vers 1
        self.viewport.set_render_mode(mode)


    # - file operations
    def _open_dff(self): #vers 1
        path, _ = QFileDialog.getOpenFileName(
            self, "Open DFF", self._last_dir,
            "DFF Models (*.dff);;All Files (*)")
        if path:
            self._last_dir = os.path.dirname(path)
            self.WS.set('last_dir', self._last_dir)
            self.load_dff(path)
            for ext in ('.txd','.TXD'):
                txd = os.path.splitext(path)[0]+ext
                if os.path.isfile(txd): self.load_txd(txd); break

    def _open_txd(self): #vers 1
        path, _ = QFileDialog.getOpenFileName(
            self, "Open TXD", self._last_dir,
            "TXD Files (*.txd);;All Files (*)")
        if path:
            self._last_dir = os.path.dirname(path)
            self.load_txd(path)

    def load_dff(self, path: str): #vers 4
        try:
            from apps.methods.dff_parser import load_dff
            self._show_progress(True)
            self._set_status(f"Parsing {os.path.basename(path)}…")
            from PyQt6.QtWidgets import QApplication
            QApplication.processEvents()
            model = load_dff(path)
            if not model or not model.geometries:
                self._set_status(f"Failed: {os.path.basename(path)}"); return
            self._dff_model = model
            self._current_dff_path = path
            # Clear texture cache when loading new DFF
            self.viewport.clear_textures()
            self._tex_list.clear()
            self.WS.add_recent(path); self.WS.save()
            self._populate_geom_list()
            self._geom_list.setCurrentRow(0)
            self._set_status(
                f"Loaded: {os.path.basename(path)} — "
                f"{len(model.geometries)} geometries, {len(model.frames)} frames")
            # Populate frame hierarchy tree
            self._populate_frame_tree()
            # Load vehicles.ide info (wheel type) + carcols colours
            stem = os.path.splitext(os.path.basename(path))[0]
            from PyQt6.QtCore import QTimer
            QTimer.singleShot(100, lambda s=stem: self._load_vehicle_meta(s))
            # Auto-load shared TXDs after primary TXD is loaded
            QTimer.singleShot(500, self._auto_load_shared_txds)
        except Exception as e:
            import traceback; traceback.print_exc()
            self._set_status(f"Error: {e}")
        finally:
            self._show_progress(False)

    def _collect_needed_textures(self): #vers 1
        """Return set of texture names the current DFF needs."""
        if not self._dff_model: return set()
        needed = set()
        for g in self._dff_model.geometries:
            for mat in g.materials:
                name = (mat.texture_name or '').strip().lower()
                if name: needed.add(name)
        return needed

    def _upload_txd_additive(self, path: str): #vers 1
        """Load a TXD and upload textures WITHOUT clearing existing ones."""
        try:
            from apps.methods.txd_parser import parse_txd
            from PyQt6.QtGui import QIcon, QImage, QPixmap
            from PyQt6.QtWidgets import QListWidgetItem
            from PyQt6.QtCore import Qt
            with open(path, 'rb') as f: data = f.read()
            textures = parse_txd(data)
            if not textures: return 0
            # Only upload textures not already loaded
            new_textures = [t for t in textures
                            if t['name'].lower() not in self.viewport._tex_ids]
            if new_textures:
                # Additive upload — don't clear existing
                self.viewport.makeCurrent()
                from OpenGL.GL import (glGenTextures, glBindTexture, GL_TEXTURE_2D,
                    glTexParameteri, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR,
                    GL_TEXTURE_MAG_FILTER, GL_LINEAR, GL_TEXTURE_WRAP_S,
                    GL_TEXTURE_WRAP_T, GL_REPEAT, glTexImage2D, GL_RGBA,
                    GL_UNSIGNED_BYTE, glGenerateMipmap, glDeleteTextures)
                for t in new_textures:
                    name = t['name'].lower()
                    rgba = t.get('rgba_data', b'')
                    w = t.get('width', 0); h = t.get('height', 0)
                    if not (rgba and w > 0 and h > 0): continue
                    gl_id = glGenTextures(1)
                    glBindTexture(GL_TEXTURE_2D, gl_id)
                    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
                    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
                    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
                    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
                    try:
                        glTexImage2D(GL_TEXTURE_2D,0,GL_RGBA,w,h,0,GL_RGBA,GL_UNSIGNED_BYTE,rgba)
                        glGenerateMipmap(GL_TEXTURE_2D)
                        self.viewport._tex_ids[name] = gl_id
                    except Exception:
                        glDeleteTextures(1,[gl_id])
                self.viewport.doneCurrent()
                # Add to texture list
                for t in new_textures:
                    item = QListWidgetItem(f"{t['name']}  {t['width']}\xd7{t['height']}")
                    rgba=t.get('rgba_data',b''); w=t.get('width',0); h=t.get('height',0)
                    if rgba and w>0 and h>0:
                        try:
                            img=QImage(rgba,w,h,w*4,QImage.Format.Format_RGBA8888)
                            px=QPixmap.fromImage(img).scaled(32,32,
                                Qt.AspectRatioMode.KeepAspectRatio,
                                Qt.TransformationMode.SmoothTransformation)
                            item.setIcon(QIcon(px))
                        except Exception: pass
                    self._tex_list.addItem(item)
                self.viewport.update()
            return len(new_textures)
        except Exception as e:
            return 0

    def _find_game_root(self): #vers 1
        """Try to find the GTA SA game root from the DFF path or main_window settings."""
        # From main_window app_settings
        mw = self.main_window
        if mw:
            for attr in ('game_root','_game_root','game_directory'):
                val = getattr(mw, attr, None)
                if val and os.path.isdir(val): return val
            if hasattr(mw,'app_settings'):
                settings = mw.app_settings
                for key in ('game_root','game_directory','sa_root'):
                    val = getattr(settings,'get',lambda k,d=None:d)(key)
                    if val and os.path.isdir(str(val)): return str(val)
        # Walk up from DFF path looking for models/ or data/ folder
        if self._current_dff_path:
            p = os.path.dirname(self._current_dff_path)
            for _ in range(8):
                if os.path.isdir(os.path.join(p,'models')) and os.path.isdir(os.path.join(p,'data')):
                    return p
                p = os.path.dirname(p)
        return ''

    def _auto_load_shared_txds(self): #vers 3
        """Find shared TXDs in a background thread — never blocks the UI."""
        if not self._dff_model: return
        needed  = self._collect_needed_textures()
        already = set(self.viewport._tex_ids.keys())
        missing = needed - already
        if not missing: return

        # Snapshot everything the worker needs — no shared mutable state
        game_root  = self._find_game_root()
        dff_dir    = os.path.dirname(self._current_dff_path) if self._current_dff_path else ''
        dff_stem   = os.path.splitext(os.path.basename(self._current_dff_path))[0].lower() if self._current_dff_path else ''
        img        = getattr(self, '_current_img', None)

        from PyQt6.QtCore import QThread, pyqtSignal as _sig

        viewer_ref = self

        class _Worker(QThread):
            found = _sig(list)   # emits list of {'name','rgba_data','width','height','format'}
            status = _sig(str)

            def run(self):
                from apps.methods.txd_parser import parse_txd
                import tempfile
                collected = []
                miss = set(missing)  # local copy

                def _try_txd_data(data):
                    nonlocal miss
                    try:
                        textures = parse_txd(data)
                        hits = [t for t in textures if t['name'].lower() in miss
                                and t.get('rgba_data') and t['width']>0]
                        if hits:
                            collected.extend(hits)
                            miss -= {t['name'].lower() for t in hits}
                        return len(hits)
                    except Exception:
                        return 0

                # 0. models/generic/
                if game_root:
                    generic = os.path.join(game_root,'models','generic')
                    for fn in ('vehicle.txd','wheels.txd','vehiclecommon.txd'):
                        p = os.path.join(generic, fn)
                        if os.path.isfile(p) and miss:
                            try:
                                with open(p,'rb') as f: _try_txd_data(f.read())
                            except Exception: pass
                    # Store wheels.DFF path for assembly use
                    for wfn in ('wheels.DFF','wheels.dff'):
                        wp=os.path.join(generic,wfn)
                        if os.path.isfile(wp):
                            viewer_ref._wheels_model_path=wp; break
                    if not miss:
                        if collected: self.found.emit(collected)
                        return

                # 1. Same directory as DFF
                if dff_dir and miss:
                    try:
                        for fn in os.listdir(dff_dir):
                            if not fn.lower().endswith('.txd'): continue
                            if fn[:-4].lower() == dff_stem: continue
                            if not miss: break
                            try:
                                with open(os.path.join(dff_dir,fn),'rb') as f: _try_txd_data(f.read())
                            except Exception: pass
                    except Exception: pass

                # 1b. Current IMG (gta3.img) — look for vehicle*.txd entries
                # SA stores vehiclegeneric256 etc inside gta3.img as separate TXDs
                if miss and img and hasattr(img,'entries'):
                    self.status.emit('Scanning gta3.img for vehicle textures...')
                    txd_map={e.name.lower():e for e in img.entries if e.name.lower().endswith('.txd')}
                    # Known SA shared vehicle TXD names inside gta3.img
                    candidates=['vehiclecommon.txd','vehicle.txd','vehicles.txd',
                                 'vehiclegeneric.txd','vehiclegrunge.txd',
                                 'vehiclelights.txd','vehicletyres.txd']
                    # Also try prefix match for any vehicle*.txd
                    candidates += [n for n in txd_map if n.startswith('vehicle') and n not in candidates]
                    tried=set()
                    for cand in candidates:
                        if not miss: break
                        if cand in txd_map and cand not in tried:
                            tried.add(cand)
                            try:
                                data=img.read_entry_data(txd_map[cand])
                                if data: _try_txd_data(data)
                            except Exception: pass

                # 2. Current IMG — ONLY look up exact stem.txd entries, no full scan
                if miss and img and hasattr(img,'entries'):
                    self.status.emit(f'Scanning IMG for {len(miss)} missing textures…')
                    # Build a map of entry names first (fast, no data read)
                    txd_entries = {e.name.lower(): e for e in img.entries
                                   if e.name.lower().endswith('.txd')}
                    # Only try TXDs whose name hints at containing missing textures
                    # Heuristic: match first 6 chars of texture name to TXD stem
                    tried = set()
                    for tex_name in list(miss):
                        if not miss: break
                        stem6 = tex_name[:6].lower()
                        for txd_name, entry in txd_entries.items():
                            if txd_name[:-4] in tried: continue
                            if txd_name.startswith(stem6) or stem6.startswith(txd_name[:4]):
                                tried.add(txd_name[:-4])
                                try:
                                    data = img.read_entry_data(entry)
                                    if data: _try_txd_data(data)
                                except Exception: pass
                                break

                if collected:
                    self.found.emit(collected)

        self._shared_txd_worker = _Worker()
        self._shared_txd_worker.status.connect(self._set_status)
        self._shared_txd_worker.found.connect(self._on_shared_txds_found)
        self._shared_txd_worker.finished.connect(lambda: self._show_progress(False))
        self._show_progress(True)
        self._shared_txd_worker.start()

    def _on_shared_txds_found(self, textures: list): #vers 2
        """Receive shared textures from worker thread and upload to GL on main thread."""
        # Load wheels.DFF if path was discovered by worker
        wheels_path = getattr(self.viewport,'_wheels_model_path',None)
        if wheels_path and not getattr(self.viewport,'_wheels_model',None):
            self.viewport.load_wheels_dff(wheels_path)
        if not textures: return
        # Filter already-loaded
        new = [t for t in textures if t['name'].lower() not in self.viewport._tex_ids]
        if not new: return
        self.viewport._upload_textures(new)
        # Add to tex list
        from PyQt6.QtGui import QIcon, QImage, QPixmap
        from PyQt6.QtWidgets import QListWidgetItem
        from PyQt6.QtCore import Qt
        for t in new:
            item = QListWidgetItem(f"{t['name']}  {t['width']}\xd7{t['height']}")
            rgba=t.get('rgba_data',b''); w=t.get('width',0); h=t.get('height',0)
            if rgba and w>0 and h>0:
                try:
                    img=QImage(rgba,w,h,w*4,QImage.Format.Format_RGBA8888)
                    px=QPixmap.fromImage(img).scaled(32,32,
                        Qt.AspectRatioMode.KeepAspectRatio,
                        Qt.TransformationMode.SmoothTransformation)
                    item.setIcon(QIcon(px))
                except Exception: pass
            self._tex_list.addItem(item)
        self._set_status(f'+{len(new)} shared textures loaded')
        self.viewport.update()


    def load_txd(self, path: str): #vers 2
        try:
            from apps.methods.txd_parser import parse_txd
            from PyQt6.QtGui import QIcon, QImage, QPixmap
            from PyQt6.QtWidgets import QListWidgetItem
            from PyQt6.QtCore import Qt, QTimer
            with open(path,'rb') as f: data=f.read()
            textures = parse_txd(data)
            if not textures:
                self._set_status(f'No textures: {os.path.basename(path)}'); return
            # GL upload — defer if widget not yet shown
            def _do_upload():
                try: self.viewport._upload_textures(textures); self.viewport.update()
                except Exception as ue: self._set_status(f'GL upload: {ue}')
            if self.viewport.isVisible():
                _do_upload()
            else:
                QTimer.singleShot(400, _do_upload)
            # Texture list with thumbnails
            self._tex_list.clear()
            for t in textures:
                item = QListWidgetItem(f"{t['name']}  {t['width']}\xd7{t['height']}")
                rgba=t.get('rgba_data',b''); w=t.get('width',0); h=t.get('height',0)
                if rgba and w>0 and h>0:
                    try:
                        img=QImage(rgba,w,h,w*4,QImage.Format.Format_RGBA8888)
                        px=QPixmap.fromImage(img).scaled(32,32,
                            Qt.AspectRatioMode.KeepAspectRatio,
                            Qt.TransformationMode.SmoothTransformation)
                        item.setIcon(QIcon(px))
                    except Exception: pass
                self._tex_list.addItem(item)
            self._set_status(f'Textures: {len(textures)} from {os.path.basename(path)}')
        except Exception as e:
            import traceback; traceback.print_exc()
            self._set_status(f'TXD error: {e}')

    def load_img(self, img): #vers 1
        """Populate IMG list with DFF entries for quick selection."""
        self._current_img = img
        self._img_list.clear()
        if not img or not hasattr(img, 'entries'):
            return
        for entry in img.entries:
            if entry.name.lower().endswith('.dff'):
                item = QListWidgetItem(entry.name)
                item.setData(Qt.ItemDataRole.UserRole, entry)
                self._img_list.addItem(item)
        count = self._img_list.count()
        self._set_status(f"IMG: {count} DFF entries loaded")

    def _on_img_entry_dclicked(self, item): #vers 2
        """Double-click IMG entry — extract and load DFF + auto TXD with progress."""
        entry = item.data(Qt.ItemDataRole.UserRole)
        img   = getattr(self, '_current_img', None)
        if not entry or not img:
            return
        from PyQt6.QtWidgets import QApplication
        from PyQt6.QtCore import Qt as _Qt
        name = entry.name
        try:
            import tempfile
            # Show busy cursor + progress bar immediately
            QApplication.setOverrideCursor(_Qt.CursorShape.WaitCursor)
            self._show_progress(True)
            self._set_status(f"Loading {name}…")
            QApplication.processEvents()

            # Step 1: Extract DFF
            self._set_status(f"Extracting {name}…")
            QApplication.processEvents()
            data = img.read_entry_data(entry)
            if not data:
                return
            tmp_dir  = tempfile.mkdtemp()
            dff_path = os.path.join(tmp_dir, name)
            with open(dff_path,'wb') as f: f.write(data)

            # Step 2: Parse + load DFF
            self._set_status(f"Parsing {name}…")
            QApplication.processEvents()
            self.load_dff(dff_path)
            QApplication.processEvents()

            # Step 3: Find + extract TXD
            stem = os.path.splitext(name)[0].lower()
            txd_entry = next((e for e in img.entries if e.name.lower() == stem + '.txd'), None)
            if txd_entry:
                self._set_status(f"Loading textures for {stem}…")
                QApplication.processEvents()
                txd_data = img.read_entry_data(txd_entry)
                if txd_data:
                    txd_path = os.path.join(tmp_dir, txd_entry.name)
                    with open(txd_path,'wb') as f: f.write(txd_data)
                    self.load_txd(txd_path)
                    QApplication.processEvents()

        except Exception as ex:
            self._set_status(f"Load error: {ex}")
        finally:
            self._show_progress(False)
            QApplication.restoreOverrideCursor()


    #    Menu button handler
    def _on_menu_btn_clicked(self): #vers 2
        pm = QMenu(self)
        self._build_menus_into_qmenu(pm)
        btn = getattr(self, 'menu_btn', None)
        if btn:
            pm.exec(btn.mapToGlobal(btn.rect().bottomLeft()))
        else:
            pm.exec(self.cursor().pos())


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
        author = getattr(self, "App_author",      "X-Seti")
        year   = getattr(self, "App_year",        "2026")
        desc   = getattr(self, "App_description", "")
        QMessageBox.information(self, f"About {self.App_name}",
            f"{self.App_name}   {self.App_build}\n\n"
            + (f"{desc}\n\n" if desc else "")
            + f"Copyright \u00a9 {year}  {author}\n"
              f"Part of IMG Factory 1.6 — a GTA modding toolkit.")

    def _open_app_settings(self): #vers 1
        try:
            try:
                from apps.utils.app_settings_system import SettingsDialog
            except ImportError:
                from app_settings_system import SettingsDialog
            if self.app_settings:
                dlg = SettingsDialog(self.app_settings, self)
                dlg.exec()
            else:
                self._show_workshop_settings()
        except Exception as e:
            self._set_status(f'Settings error: {e}')
            self._show_workshop_settings()

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
        author = getattr(self, "App_author",      "X-Seti")
        year   = getattr(self, "App_year",        "2026")
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

        #    Tab: Vehicle (game path, viewport settings)
        vt = QWidget(); vl = QVBoxLayout(vt)

        game_grp = QGroupBox("GTA Game Root"); gl = QHBoxLayout(game_grp)
        game_edit = QLineEdit(ws.get("game_root", ""))
        game_edit.setPlaceholderText("e.g. /home/user/GTASA-PC")
        browse_btn = QPushButton("Browse…")
        def _browse_game():
            from PyQt6.QtWidgets import QFileDialog
            d = QFileDialog.getExistingDirectory(dlg, "Select GTA Game Root", game_edit.text())
            if d: game_edit.setText(d)
        browse_btn.clicked.connect(_browse_game)
        gl.addWidget(game_edit, 1); gl.addWidget(browse_btn)
        vl.addWidget(game_grp)

        vp_grp = QGroupBox("3D Viewport"); vpl = QVBoxLayout(vp_grp)
        grid_cb = QCheckBox("Show grid on startup")
        grid_cb.setChecked(ws.get("viewport_grid", True))
        cull_cb = QCheckBox("Backface culling on startup")
        cull_cb.setChecked(ws.get("viewport_cull", True))
        vpl.addWidget(grid_cb); vpl.addWidget(cull_cb)
        vl.addWidget(vp_grp)
        vl.addStretch()
        tabs.addTab(vt, "Vehicle")

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
        ws.set("game_root",                game_edit.text())
        ws.set("viewport_grid",            grid_cb.isChecked())
        ws.set("viewport_cull",            cull_cb.isChecked())
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

    def get_content_margins(self): #vers 1
        return (self.contmergina, self.contmerginb, self.contmerginc, self.contmergind)

    def get_panel_margins(self): #vers 1
        return (self.panelmergina, self.panelmerginb, self.panelmerginc, self.panelmergind)

    def get_tab_margins(self): #vers 1
        return (self.tabmerginsa, self.tabmerginsb, self.tabmerginsc, self.tabmerginsd)

    def setup_ui(self): #vers 2
        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(*self.get_content_margins())
        main_layout.setSpacing(self.setspacing)

        # Viewport must exist before toolbar (toolbar buttons reference it)
        self.viewport = DFFViewport()
        self.viewport.app_settings = self.app_settings

        main_layout.addWidget(self._create_toolbar())

        splitter = QSplitter(Qt.Orientation.Horizontal)
        splitter.addWidget(self._create_left_panel())
        splitter.addWidget(self._create_centre_panel())
        splitter.addWidget(self._create_right_panel())
        splitter.setStretchFactor(0, 0)
        splitter.setStretchFactor(1, 5)
        splitter.setStretchFactor(2, 0)
        splitter.setSizes([200, 800, 180])
        main_layout.addWidget(splitter, 1)
        main_layout.addWidget(self._create_status_bar())

    def _create_left_panel(self): #vers 2
        panel = QFrame(); panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(180); panel.setMaximumWidth(280)
        outer = QVBoxLayout(panel)
        outer.setContentsMargins(*self.get_panel_margins())
        outer.setSpacing(2)


        # Vertical splitter — all three sections resizable
        splitter = QSplitter(Qt.Orientation.Vertical)
        outer.addWidget(splitter, 1)


        # - IMG Entries section
        img_sec = QWidget(); img_lay = QVBoxLayout(img_sec); img_lay.setContentsMargins(0,0,0,0); img_lay.setSpacing(2)
        img_lay.addWidget(self._make_section_header("IMG Entries (DFF)", self._filter_img_list))
        self._img_list = QListWidget()
        self._img_list.setFont(self.panel_font)
        self._img_list.itemDoubleClicked.connect(self._on_img_entry_dclicked)
        img_lay.addWidget(self._img_list)
        splitter.addWidget(img_sec)


        # - Geometries section
        geom_sec = QWidget(); gl = QVBoxLayout(geom_sec); gl.setContentsMargins(0,0,0,0); gl.setSpacing(2)
        gl.addWidget(self._make_section_header("Geometries"))
        self._geom_list = QListWidget()
        self._geom_list.setFont(self.panel_font)
        self._geom_list.currentRowChanged.connect(self._on_geom_selected)
        gl.addWidget(self._geom_list)
        splitter.addWidget(geom_sec)


        # - Textures section with thumbnails
        tex_sec = QWidget(); tl = QVBoxLayout(tex_sec); tl.setContentsMargins(0,0,0,0); tl.setSpacing(2)
        tl.addWidget(self._make_section_header("Textures"))
        self._tex_list = QListWidget()
        self._tex_list.setFont(self.panel_font)
        self._tex_list.setIconSize(QSize(32,32))
        self._tex_list.setViewMode(QListWidget.ViewMode.ListMode)
        tl.addWidget(self._tex_list)
        splitter.addWidget(tex_sec)

        # - Frame Hierarchy section with visibility checkboxes
        frame_sec = QWidget(); fl = QVBoxLayout(frame_sec)
        fl.setContentsMargins(0,0,0,0); fl.setSpacing(2)
        fl.addWidget(self._make_section_header('Frame Hierarchy'))
        from PyQt6.QtWidgets import QTreeWidget, QTreeWidgetItem
        self._frame_tree = QTreeWidget()
        self._frame_tree.setFont(self.panel_font)
        self._frame_tree.setHeaderHidden(True)
        self._frame_tree.setColumnCount(1)
        self._frame_tree.itemChanged.connect(self._on_frame_visibility_changed)
        fl.addWidget(self._frame_tree)
        splitter.addWidget(frame_sec)

        splitter.setSizes([180, 120, 100, 120])

        # Open DFF / Open TXD buttons at bottom of left panel

        btn_row = QHBoxLayout(); btn_row.setSpacing(4)
        self._vw_open_dff_btn = QPushButton("Open DFF")
        self._vw_open_txd_btn = QPushButton("Open TXD")
        for btn, cb in [(self._vw_open_dff_btn, self._vw_pick_dff),
                        (self._vw_open_txd_btn, self._vw_pick_txd)]:
            btn.setFixedHeight(26); btn.setFont(self.panel_font)
            btn.clicked.connect(cb)
            btn_row.addWidget(btn)
        outer.addLayout(btn_row)

        return panel

    def _create_left_panel_old(self):
        """Left panel — list + Add/Remove + info label.
        Override to replace with your own content.
        """
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        ll = QVBoxLayout(panel)
        ll.setContentsMargins(*self.get_panel_margins())

        hdr = QLabel("Models")
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

    def _create_centre_panel(self): #Vers 1
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

    def _create_right_panel_old(self): #Vers 1
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


        # - right panel — model info
    def _create_right_panel(self): #vers 2
        panel = QFrame(); panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(160); panel.setMaximumWidth(200)
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(*self.get_panel_margins())
        lay.setSpacing(4)

        ic = self._get_icon_color()
        def _icon(name, size=16):
            if not ICONS_AVAILABLE: return None
            try:
                fn = getattr(SVGIconFactory, name+'_icon', None)
                return fn(size, ic) if fn else None
            except Exception: return None

        from PyQt6.QtWidgets import QToolButton as _QTB

        def _btn(text, tip, cb, iname=None, checkable=False, checked=False): #Vers 1
            b = _QTB(); b.setFont(self.panel_font)
            b.setToolTip(tip); b.setFixedHeight(26)
            b.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
            ico = _icon(iname)
            if ico:
                b.setIcon(ico); b.setIconSize(QSize(16,16))
                b.setText(text)
                b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
            else:
                b.setText(text)
            if checkable:
                b.setCheckable(True); b.setChecked(checked)
                b.toggled.connect(cb)
            else:
                b.clicked.connect(cb)
            return b

        #  Render mode
        lbl_r = QLabel("Render"); lbl_r.setFont(self.panel_font)
        lay.addWidget(lbl_r)
        self._mode_group = QButtonGroup(self); self._mode_group.setExclusive(True)
        for label, mode, iname in [
            ("Wireframe", "wireframe", "wireframe"),
            ("Solid",     "solid",     "solid"),
            ("Textured",  "textured",  "texture"),
        ]:
            b = _btn(label, f"{mode.capitalize()} render mode",
                     lambda checked=False, m=mode: (self._set_mode(m) if checked else None),
                     iname, checkable=True, checked=(mode=='solid'))
            self._mode_group.addButton(b); lay.addWidget(b)

        lay.addSpacing(6)

        #  View toggles
        lbl_v = QLabel("View"); lbl_v.setFont(self.panel_font)
        lay.addWidget(lbl_v)
        self._cull_btn   = _btn("Backface Cull", "Toggle backface culling",  self.viewport.set_backface_cull, 'backface', True, True)
        self._grid_btn   = _btn("Grid",          "Toggle grid",              self.viewport.set_show_grid,    'grid',     True, True)
        self._prelit_btn = _btn("Pre-Lighting",  "Vertex pre-lighting",      self.viewport.set_prelight,     'shading',  True, False)
        lay.addWidget(self._cull_btn)
        lay.addWidget(self._grid_btn)
        lay.addWidget(self._prelit_btn)

        lay.addSpacing(6)

        #  Camera
        lbl_c = QLabel("Camera"); lbl_c.setFont(self.panel_font)
        lay.addWidget(lbl_c)
        lay.addWidget(_btn("Reset Camera", "Reset camera to default", self.viewport.reset_camera, 'reset'))
        lay.addWidget(_btn("Light Setup",  "Adjust light direction",  self._light_setup_dialog,   'light'))

        lay.addSpacing(6)

        #  Paint Colours
        lbl_p = QLabel("Paint"); lbl_p.setFont(self.panel_font)
        lay.addWidget(lbl_p)

        # Colour swatches row — primary + secondary
        swatch_row = QWidget()
        swatch_lay = QHBoxLayout(swatch_row)
        swatch_lay.setContentsMargins(0,0,0,0); swatch_lay.setSpacing(4)

        self._paint1_btn = QPushButton("Primary")
        self._paint2_btn = QPushButton("Secondary")
        for btn in (self._paint1_btn, self._paint2_btn):
            btn.setFixedHeight(28)
            btn.setFont(self.infobar_font)
        self._paint1_btn.clicked.connect(self._pick_paint1)
        self._paint2_btn.clicked.connect(self._pick_paint2)
        swatch_lay.addWidget(self._paint1_btn)
        swatch_lay.addWidget(self._paint2_btn)
        lay.addWidget(swatch_row)
        self._update_paint_btns()

        # Carcols swatches — populated when DFF loads
        self._carcols_widget = QWidget()
        self._carcols_lay = QVBoxLayout(self._carcols_widget)
        self._carcols_lay.setContentsMargins(0,0,0,2); self._carcols_lay.setSpacing(2)
        lay.addWidget(self._carcols_widget)

        lay.addSpacing(4)

        #  Assembly
        lbl_a = QLabel("Assembly"); lbl_a.setFont(self.panel_font)
        lay.addWidget(lbl_a)
        self._assemble_btn = _btn("All Parts", "Show all parts assembled at world positions", self._toggle_assembly_mode, None, True, False)
        self._damage_btn   = _btn("Damage",    "Show damaged state (_dam parts)",             self._toggle_damage_mode,   None, True, False)
        self._lod_btn      = _btn("Show LOD",  "Show LOD meshes (_vlo parts)",                self._toggle_lod_mode,      None, True, False)
        lay.addWidget(self._assemble_btn)
        lay.addWidget(self._damage_btn)
        lay.addWidget(self._lod_btn)

        lay.addSpacing(6)

        #  Wheels
        lbl_w = QLabel('Wheels'); lbl_w.setFont(self.panel_font)
        lay.addWidget(lbl_w)
        from PyQt6.QtWidgets import QSlider
        self._wheel_heading_slider = QSlider(Qt.Orientation.Horizontal)
        self._wheel_heading_slider.setRange(-35, 35)
        self._wheel_heading_slider.setValue(0)
        self._wheel_heading_slider.setToolTip('Front wheel heading angle')
        self._wheel_heading_slider.valueChanged.connect(
            lambda v: self.viewport.set_wheel_heading(v))
        heading_row = QHBoxLayout()
        heading_lbl = QLabel('Steer'); heading_lbl.setFont(self.infobar_font)
        heading_row.addWidget(heading_lbl)
        heading_row.addWidget(self._wheel_heading_slider, 1)
        lay.addLayout(heading_row)

        lay.addSpacing(6)

        #  Model Info
        lbl_i = QLabel('Model Info'); lbl_i.setFont(self.panel_font)
        lay.addWidget(lbl_i)
        self._info_lbl = QLabel('—')
        self._info_lbl.setFont(self.infobar_font)
        self._info_lbl.setWordWrap(True)
        self._info_lbl.setAlignment(Qt.AlignmentFlag.AlignTop)
        lay.addWidget(self._info_lbl)

        lay.addStretch()
        return panel



    def _populate_sidebar(self): #Vers 1
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


    # - status bar
    def _create_status_bar(self): #vers 2
        bar = QFrame()
        bar.setFrameStyle(QFrame.Shape.StyledPanel | QFrame.Shadow.Sunken)
        bar.setFixedHeight(self.statusheight)
        hl = QHBoxLayout(bar)
        hl.setContentsMargins(*self.get_tab_margins())
        self.status_label = QLabel("No model loaded")
        self.status_label.setFont(self.infobar_font)
        hl.addWidget(self.status_label, 1)
        from PyQt6.QtWidgets import QProgressBar
        self._progress = QProgressBar()
        self._progress.setFixedWidth(120)
        self._progress.setFixedHeight(14)
        self._progress.setRange(0, 0)   # indeterminate / pulsing
        self._progress.setVisible(False)
        self._progress.setTextVisible(False)
        hl.addWidget(self._progress)
        return bar


    # - left panel — geometry + texture lists
    def _make_section_header(self, title, search_cb=None): #vers 1
        """Collapsible section header row with optional search box."""
        row = QWidget(); rl = QHBoxLayout(row); rl.setContentsMargins(0,0,0,0); rl.setSpacing(2)
        lbl = QLabel(title); lbl.setFont(self.panel_font); rl.addWidget(lbl, 1)
        if search_cb:
            from PyQt6.QtWidgets import QLineEdit
            se = QLineEdit(); se.setPlaceholderText("Filter…")
            se.setFixedHeight(20); se.setMaximumWidth(90)
            se.textChanged.connect(search_cb)
            rl.addWidget(se)
        return row

    def _filter_img_list(self, text): #vers 1
        ft = text.lower()
        for i in range(self._img_list.count()):
            item = self._img_list.item(i)
            item.setHidden(bool(ft) and ft not in item.text().lower())

    def _on_img_entry_dclicked(self, item): #vers 2
        """Double-click IMG entry — extract and load DFF + auto TXD with progress."""
        entry = item.data(Qt.ItemDataRole.UserRole)
        img   = getattr(self, '_current_img', None)
        if not entry or not img:
            return
        from PyQt6.QtWidgets import QApplication
        from PyQt6.QtCore import Qt as _Qt
        name = entry.name
        try:
            import tempfile
            # Show busy cursor + progress bar immediately
            QApplication.setOverrideCursor(_Qt.CursorShape.WaitCursor)
            self._show_progress(True)
            self._set_status(f"Loading {name}…")
            QApplication.processEvents()

            # Step 1: Extract DFF
            self._set_status(f"Extracting {name}…")
            QApplication.processEvents()
            data = img.read_entry_data(entry)
            if not data:
                return
            tmp_dir  = tempfile.mkdtemp()
            dff_path = os.path.join(tmp_dir, name)
            with open(dff_path,'wb') as f: f.write(data)

            # Step 2: Parse + load DFF
            self._set_status(f"Parsing {name}…")
            QApplication.processEvents()
            self.load_dff(dff_path)
            QApplication.processEvents()

            # Step 3: Find + extract TXD
            stem = os.path.splitext(name)[0].lower()
            txd_entry = next((e for e in img.entries if e.name.lower() == stem + '.txd'), None)
            if txd_entry:
                self._set_status(f"Loading textures for {stem}…")
                QApplication.processEvents()
                txd_data = img.read_entry_data(txd_entry)
                if txd_data:
                    txd_path = os.path.join(tmp_dir, txd_entry.name)
                    with open(txd_path,'wb') as f: f.write(txd_data)
                    self.load_txd(txd_path)
                    QApplication.processEvents()

        except Exception as ex:
            self._set_status(f"Load error: {ex}")
        finally:
            self._show_progress(False)
            QApplication.restoreOverrideCursor()


    def _light_setup_dialog(self): #vers 2
        """Light direction, intensity and vehicle paint colour dialog."""
        dlg = QDialog(self); dlg.setWindowTitle("Light & Paint Setup"); dlg.resize(360, 380)
        lay = QVBoxLayout(dlg)
        form = QFormLayout()

        vp = self.viewport

        # Light direction sliders
        def _slider(lo, hi, val, scale=10):
            s = QSlider(Qt.Orientation.Horizontal)
            s.setRange(int(lo*scale), int(hi*scale))
            s.setValue(int(val*scale))
            return s

        sx = _slider(-10, 10, vp._light_dir[0])
        sy = _slider(-10, 10, vp._light_dir[1])
        sz = _slider(-10, 10, vp._light_dir[2])
        sa = _slider(0, 1, vp._ambient, 100)
        sd = _slider(0, 2, vp._diffuse, 100)

        lx_lbl = QLabel(f"{vp._light_dir[0]:.1f}")
        ly_lbl = QLabel(f"{vp._light_dir[1]:.1f}")
        lz_lbl = QLabel(f"{vp._light_dir[2]:.1f}")
        la_lbl = QLabel(f"{vp._ambient:.2f}")
        ld_lbl = QLabel(f"{vp._diffuse:.2f}")

        def _upd():
            x = sx.value()/10; y = sy.value()/10; z = sz.value()/10
            a = sa.value()/100; d = sd.value()/100
            lx_lbl.setText(f"{x:.1f}"); ly_lbl.setText(f"{y:.1f}"); lz_lbl.setText(f"{z:.1f}")
            la_lbl.setText(f"{a:.2f}"); ld_lbl.setText(f"{d:.2f}")
            vp.set_light_dir(x, y, z); vp.set_ambient(a); vp.set_diffuse(d)

        for s in (sx,sy,sz,sa,sd): s.valueChanged.connect(_upd)

        def _row(lbl, slider, val_lbl):
            rw = QWidget(); rl = QHBoxLayout(rw); rl.setContentsMargins(0,0,0,0)
            rl.addWidget(slider,1); rl.addWidget(val_lbl)
            form.addRow(lbl, rw)

        _row("Light X:", sx, lx_lbl)
        _row("Light Y:", sy, ly_lbl)
        _row("Light Z:", sz, lz_lbl)
        _row("Ambient:", sa, la_lbl)
        _row("Diffuse:", sd, ld_lbl)

        lay.addLayout(form)

        # Paint colours
        from PyQt6.QtWidgets import QGroupBox, QColorDialog
        paint_grp = QGroupBox("Vehicle Paint Preview")
        paint_lay = QHBoxLayout(paint_grp)

        def _colour_btn(label, current_rgb, setter):
            def _to_qcolor(rgb):
                from PyQt6.QtGui import QColor
                return QColor(int(rgb[0]*255), int(rgb[1]*255), int(rgb[2]*255))
            btn = QPushButton(label)
            btn.setStyleSheet(f"background-color: rgb({int(current_rgb[0]*255)},{int(current_rgb[1]*255)},{int(current_rgb[2]*255)})")
            btn.setFixedHeight(28)
            def _pick():
                col = QColorDialog.getColor(_to_qcolor(setter.__self__._paint1 if 'paint1' in label.lower() or 'primary' in label.lower() else setter.__self__._paint2), dlg)
                if col.isValid():
                    rgb = (col.redF(), col.greenF(), col.blueF())
                    setter(rgb)
                    btn.setStyleSheet(f"background-color: {col.name()}")
                    vp.update()
            btn.clicked.connect(_pick)
            return btn

        vp = self.viewport
        p1_btn = QPushButton("Primary Paint")
        p1_btn.setStyleSheet(f"background-color: rgb({int(vp._paint1[0]*255)},{int(vp._paint1[1]*255)},{int(vp._paint1[2]*255)})")
        p1_btn.setFixedHeight(28)
        def _pick1():
            from PyQt6.QtGui import QColor
            col = QColorDialog.getColor(QColor(int(vp._paint1[0]*255),int(vp._paint1[1]*255),int(vp._paint1[2]*255)), dlg)
            if col.isValid():
                vp._paint1 = (col.redF(), col.greenF(), col.blueF())
                p1_btn.setStyleSheet(f"background-color: {col.name()}")
                vp.update()
        p1_btn.clicked.connect(_pick1)

        p2_btn = QPushButton("Secondary Paint")
        p2_btn.setStyleSheet(f"background-color: rgb({int(vp._paint2[0]*255)},{int(vp._paint2[1]*255)},{int(vp._paint2[2]*255)})")
        p2_btn.setFixedHeight(28)
        def _pick2():
            from PyQt6.QtGui import QColor
            col = QColorDialog.getColor(QColor(int(vp._paint2[0]*255),int(vp._paint2[1]*255),int(vp._paint2[2]*255)), dlg)
            if col.isValid():
                vp._paint2 = (col.redF(), col.greenF(), col.blueF())
                p2_btn.setStyleSheet(f"background-color: {col.name()}")
                vp.update()
        p2_btn.clicked.connect(_pick2)

        paint_lay.addWidget(p1_btn); paint_lay.addWidget(p2_btn)
        lay.addWidget(paint_grp)

        reset_btn = QPushButton("Reset Defaults")
        def _reset():
            sx.setValue(10); sy.setValue(20); sz.setValue(15)
            sa.setValue(30); sd.setValue(85)
            vp._paint1 = (0.80, 0.20, 0.20)
            vp._paint2 = (0.20, 0.20, 0.80)
            p1_btn.setStyleSheet("background-color: rgb(204,51,51)")
            p2_btn.setStyleSheet("background-color: rgb(51,51,204)")
            vp.update()
        reset_btn.clicked.connect(_reset)

        btn_row = QHBoxLayout()
        close_btn = QPushButton("Close"); close_btn.clicked.connect(dlg.accept)
        btn_row.addWidget(reset_btn); btn_row.addStretch(); btn_row.addWidget(close_btn)
        lay.addLayout(btn_row)
        dlg.exec()


    def _populate_frame_tree(self): #vers 1
        tree = getattr(self,'_frame_tree',None)
        if not tree or not self._dff_model: return
        from PyQt6.QtWidgets import QTreeWidgetItem
        from PyQt6.QtCore import Qt
        tree.blockSignals(True)
        tree.clear()
        frames = self._dff_model.frames
        items = {}
        # Build tree from parent indices
        for i,f in enumerate(frames):
            name = f.name or f'frame_{i}'
            item = QTreeWidgetItem([name])
            item.setFlags(item.flags() | Qt.ItemFlag.ItemIsUserCheckable)
            item.setCheckState(0, Qt.CheckState.Checked)
            item.setData(0, Qt.ItemDataRole.UserRole, name.lower())
            items[i] = item
        for i,f in enumerate(frames):
            p = f.parent_index
            if p >= 0 and p != i and p in items:
                items[p].addChild(items[i])
            elif p == -1 or p == i:
                tree.addTopLevelItem(items[i])
        tree.expandAll()
        tree.blockSignals(False)

    def _on_frame_visibility_changed(self, item, col): #vers 1
        from PyQt6.QtCore import Qt
        name = item.data(0, Qt.ItemDataRole.UserRole) or ''
        if not hasattr(self.viewport,'_hidden_frames'):
            self.viewport._hidden_frames = set()
        if item.checkState(0) == Qt.CheckState.Checked:
            self.viewport._hidden_frames.discard(name)
        else:
            self.viewport._hidden_frames.add(name)
        # Rebuild assembly if active
        if getattr(self,'_assemble_btn',None) and self._assemble_btn.isChecked():
            self._toggle_assembly_mode(True)
        else:
            self.viewport.update()

    def _populate_geom_list(self): #vers 1

        self._geom_list.clear()
        if not self._dff_model: return
        for i, g in enumerate(self._dff_model.geometries):
            name = f"geometry_{i}"
            a = next((a for a in self._dff_model.atomics if a.geometry_index==i), None)
            if a and a.frame_index < len(self._dff_model.frames):
                fn = self._dff_model.frames[a.frame_index].name
                if fn: name = fn
            self._geom_list.addItem(f"[{i}] {name}  {len(g.vertices)}v {len(g.triangles)}t")


    def _on_geom_selected(self, row: int): #vers 1
        if not self._dff_model or row < 0 or row >= len(self._dff_model.geometries): return
        self._current_geom = row
        g = self._dff_model.geometries[row]
        self.viewport.load_geometry(g, g.materials)
        has_prelit = bool(g.colors)
        self._prelit_btn.setEnabled(has_prelit)
        self._info_lbl.setText(
            f"Verts:  {len(g.vertices)}\n"
            f"Tris:   {len(g.triangles)}\n"
            f"Mats:   {len(g.materials)}\n"
            f"UVs:    {len(g.uv_layers)}\n"
            f"Norms:  {'yes' if g.normals else 'no'}\n"
            f"PreLit: {'yes' if has_prelit else 'no'}")

    def _set_status(self, msg: str): #vers 1
        self.status_label.setText(msg)
        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message(f"[ModelViewer] {msg}")




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
    def _export_file(self):
        QMessageBox.information(self, "Export", "Export not yet implemented.")
    def _import_file(self):
        QMessageBox.information(self, "Import", "Import not yet implemented.")
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


# GUIWorkshop — assembles all four sections

class GUIWorkshop(_ToolbarMixin, _LayoutMixin, _LogicStubsMixin,
                  ToolMenuMixin, QWidget):
    """Reusable workshop base.  Subclass this, override App_name/config_key
    and the stubs in Section 4.  All chrome, theme, settings, and window
    management are inherited from the four sections above.
    """

    #    Subclass identity — OVERRIDE ALL OF THESE
    App_name        = "Vehicle Workshop"
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
        self.tabmerginsa  = 2; self.tabmerginsb  = 2
        self.tabmerginsc  = 2; self.tabmerginsd  = 2
        self.panelspacing = 4; self.titlebarheight = 36
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
    def _get_resize_corner(self, pos): #Vers 1
        s = self.corner_size; x, y = pos.x(), pos.y()
        w, h = self.width(), self.height()
        if x < s and y < s:    return "top-left"
        if x > w-s and y < s:  return "top-right"
        if x < s and y > h-s:  return "bottom-left"
        if x > w-s and y > h-s: return "bottom-right"
        return None


    def _update_cursor(self, direction): #Vers 2
        cursors = {
            "top":          Qt.CursorShape.SizeVerCursor,
            "bottom":       Qt.CursorShape.SizeVerCursor,
            "left":         Qt.CursorShape.SizeHorCursor,
            "right":        Qt.CursorShape.SizeHorCursor,
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


    def _is_on_draggable_area(self, pos): #Vers 1
        if not hasattr(self, 'titlebar'):
            return False
        if not self.titlebar.rect().contains(pos):
            return False
        for w in self.titlebar.findChildren(QPushButton):
            if w.isVisible() and w.geometry().contains(pos):
                return False
        return True


    def mousePressEvent(self, ev): #Vers 1
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


    def _toggle_maximize(self): #Vers 1
        if self.isMaximized(): self.showNormal()
        else: self.showMaximized()


    def toggle_dock_mode(self): pass  # override if dock support needed


    def mouseReleaseEvent(self, event): #Vers 2
        if event.button() == Qt.MouseButton.LeftButton:
            self.dragging = self.resizing = False
            self.resize_corner = None
            self.setCursor(Qt.CursorShape.ArrowCursor)
            event.accept()


    def _handle_corner_resize(self, global_pos): #Vers 1
        if not self.resize_corner or not self.drag_position:
            return
        delta = global_pos - self.drag_position
        geometry = self.initial_geometry
        min_w, min_h = 800, 500
        if self.resize_corner == "bottom-right":
            nw = geometry.width() + delta.x()
            nh = geometry.height() + delta.y()
            if nw >= min_w and nh >= min_h:
                self.resize(nw, nh)
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


    def paintEvent(self, event): #Vers 2
        super().paintEvent(event)
        # Corner handles drawn by _corner_overlay — see _setup_corner_overlay

    def _setup_corner_overlay(self): #vers 4
        """Create or refresh the corner resize overlay."""
        if not self.standalone_mode:
            return
        # Destroy stale overlay if window was resized before it was created
        existing = getattr(self, '_corner_overlay', None)
        if existing is not None:
            existing.setGeometry(0, 0, self.width(), self.height())
            existing.raise_()
            existing.update()
            return
        overlay = _CornerOverlay(self)
        self._corner_overlay = overlay
        overlay.setGeometry(0, 0, self.width(), self.height())
        overlay.show()
        overlay.raise_()

    def _refresh_corner_overlay(self): #vers 1
        if hasattr(self, '_corner_overlay'):
            self._corner_overlay.setGeometry(0, 0, self.width(), self.height())
            self._corner_overlay.update_state(
                getattr(self, 'hover_corner', None),
                self.app_settings)
            self._corner_overlay.raise_()

    def showEvent(self, event): #vers 2
        super().showEvent(event)
        if self.standalone_mode:
            # Small delay ensures window geometry is finalised
            QTimer.singleShot(150, self._setup_corner_overlay)

    def resizeEvent(self, event): #vers 2
        super().resizeEvent(event)
        if hasattr(self,'size_grip'): self.size_grip.move(self.width()-16,self.height()-16)
        self._refresh_corner_overlay()

    def closeEvent(self, event): #Vers 2
        # Save window geometry
        if self.standalone_mode:
            g = self.geometry()
            self.WS.set('window_x', g.x())
            self.WS.set('window_y', g.y())
            self.WS.set('window_w', g.width())
            self.WS.set('window_h', g.height())
            self.WS.save()
        self.window_closed.emit()
        event.accept()

# carcols.dat parser

@dataclass
class CarColour: #vers 1
    r: int = 0
    g: int = 0
    b: int = 0

    def to_qcolor(self) -> QColor:
        return QColor(self.r, self.g, self.b)

    def __str__(self):
        return f"{self.r},{self.g},{self.b}"


@dataclass
class CarColEntry: #vers 1
    name:     str                    = ""
    palettes: List[Tuple[int, int]]  = field(default_factory=list)


class CarColsParser: #vers 1
    def __init__(self): #vers 1
        self.colours:      List[CarColour]   = []
        self.vehicles:     List[CarColEntry] = []
        self.header_lines: List[str]         = []
        self.game:         str               = "VC"

    def _detect_game(self, lines: List[str]) -> str: #vers 1
        for ln in lines:
            s = ln.strip()
            if s.lower().startswith("col") and not s.startswith(";"):
                if len(s.split()) > 15:
                    return "SA"
        return "VC"

    def load(self, path: str) -> bool: #vers 1
        try:
            self.colours.clear(); self.vehicles.clear(); self.header_lines.clear()
            with open(path, "r", encoding="latin-1") as f:
                lines = f.readlines()
            self.game = self._detect_game(lines)
            in_col = False
            for ln in lines:
                s = ln.strip()
                if not s or s.startswith(";"):
                    if not in_col: self.header_lines.append(ln)
                    continue
                parts = s.split()
                if not parts: continue
                # Colour palette row: "R,G,B"
                if not parts[0].lower().startswith("col") and not in_col:
                    try:
                        rgb = parts[0].split(",")
                        if len(rgb) == 3:
                            self.colours.append(CarColour(int(rgb[0]), int(rgb[1]), int(rgb[2])))
                            continue
                        elif len(parts) >= 3 and all(p.isdigit() for p in parts[:3]):
                            self.colours.append(CarColour(int(parts[0]), int(parts[1]), int(parts[2])))
                            continue
                    except (ValueError, IndexError):
                        pass
                if parts[0].lower() in ("col", "car2"):
                    in_col = True
                    if len(parts) < 2: continue
                    entry = CarColEntry(name=parts[1])
                    for pair_str in parts[2:]:
                        try:
                            pair = pair_str.split(",")
                            if len(pair) == 2:
                                entry.palettes.append((int(pair[0]), int(pair[1])))
                        except ValueError:
                            pass
                    self.vehicles.append(entry)
            return True
        except Exception as ex:
            print(f"CarColsParser.load: {ex}"); return False

    def save(self, path: str) -> bool: #vers 1
        try:
            with open(path, "w", encoding="latin-1") as f:
                for ln in self.header_lines: f.write(ln)
                for c in self.colours: f.write(f"{c.r},{c.g},{c.b}\n")
                f.write("\n")
                for v in self.vehicles:
                    pairs = "   ".join(f"{p},{s}" for p, s in v.palettes)
                    f.write(f"col   {v.name}   {pairs}\n")
            return True
        except Exception as ex:
            print(f"CarColsParser.save: {ex}"); return False


# carmods.dat parser

@dataclass
class CarModEntry: #vers 1
    vehicle: str       = ""
    mods:    List[str] = field(default_factory=list)


class CarModsParser: #vers 1
    def __init__(self): #vers 1
        self.entries:      List[CarModEntry] = []
        self.header_lines: List[str]         = []

    def load(self, path: str) -> bool: #vers 1
        try:
            self.entries.clear(); self.header_lines.clear()
            in_data = False
            with open(path, "r", encoding="latin-1") as f:
                for ln in f:
                    s = ln.strip()
                    if not s or s.startswith(";") or s.startswith("#"):
                        if not in_data: self.header_lines.append(ln)
                        continue
                    in_data = True
                    parts = s.split()
                    if parts:
                        self.entries.append(CarModEntry(vehicle=parts[0], mods=parts[1:]))
            return True
        except Exception as ex:
            print(f"CarModsParser.load: {ex}"); return False

    def save(self, path: str) -> bool: #vers 1
        try:
            with open(path, "w", encoding="latin-1") as f:
                for ln in self.header_lines: f.write(ln)
                for e in self.entries:
                    f.write(f"{e.vehicle}   {chr(32).join(e.mods)}\n")
            return True
        except Exception as ex:
            print(f"CarModsParser.save: {ex}"); return False


# Colour swatch grid

class ColourSwatchGrid(QWidget): #vers 1
    def __init__(self, parent=None):
        super().__init__(parent)
        self._palette:  List[CarColour] = []
        self._selected: int = -1
        self._cols      = 16
        self._cell      = 24
        self.setMinimumHeight(80)
        self.setCursor(Qt.CursorShape.PointingHandCursor)

    def set_palette(self, palette: List[CarColour]): #vers 1
        self._palette = palette
        rows = max(1, (len(palette) + self._cols - 1) // self._cols)
        self.setMinimumHeight(rows * self._cell + 4)
        self.update()

    def paintEvent(self, event): #vers 1
        p = QPainter(self)
        cs = self._cell
        for i, col in enumerate(self._palette):
            x = (i % self._cols) * cs
            y = (i // self._cols) * cs
            p.fillRect(x, y, cs, cs, col.to_qcolor())
            if i == self._selected:
                p.setPen(QPen(QColor(255, 255, 255), 2))
                p.drawRect(x+1, y+1, cs-2, cs-2)
            else:
                p.setPen(QPen(QColor(0, 0, 0, 60), 1))
                p.drawRect(x, y, cs, cs)
        if not self._palette:
            p.setPen(QColor(120, 120, 120))
            p.drawText(self.rect(), Qt.AlignmentFlag.AlignCenter, "No palette loaded")

    def mousePressEvent(self, event): #vers 1
        cs = self._cell
        idx = (int(event.position().y()) // cs) * self._cols + (int(event.position().x()) // cs)
        if 0 <= idx < len(self._palette):
            self._selected = idx
            self.update()
            self.colour_clicked(idx)

    def colour_clicked(self, idx: int): pass  # override


# Tab 1 — Handling

class HandlingTab(QWidget): #vers 1
    def __init__(self, parent=None):
        super().__init__(parent)
        self._parser   = HandlingParser() if _HANDLING_AVAILABLE else None
        self._cur_idx  = -1
        self._blocking = False
        self._field_widgets: Dict[str, QWidget] = {}
        self._stat_bars:     Dict[str, tuple]   = {}
        self._flag_labels:   Dict[int, QLabel]  = {}
        self._build_ui()

    def _build_ui(self): #vers 1
        root = QHBoxLayout(self); root.setContentsMargins(4, 4, 4, 4)
        if not _HANDLING_AVAILABLE:
            root.addWidget(QLabel("Handling Editor unavailable — check Handling_Editor/handling_editor.py"))
            return
        sp = QSplitter(Qt.Orientation.Horizontal)
        # Left
        left = QWidget(); ll = QVBoxLayout(left); ll.setContentsMargins(2,2,2,2)
        ll.addWidget(QLabel("Vehicles"))
        self._search = QLineEdit(); self._search.setPlaceholderText("Search…")
        self._search.textChanged.connect(lambda t: self._refresh_list(t))
        ll.addWidget(self._search)
        self._veh_list = QListWidget()
        self._veh_list.currentRowChanged.connect(self._on_select)
        ll.addWidget(self._veh_list)
        br = QHBoxLayout()
        for lbl, fn in [("Add", self._add), ("Del", self._delete), ("Dup", self._dup)]:
            b = QPushButton(lbl); b.setFixedHeight(24); b.clicked.connect(fn); br.addWidget(b)
        ll.addLayout(br)
        sp.addWidget(left)
        # Centre
        scroll = QScrollArea(); scroll.setWidgetResizable(True)
        ctr = QWidget(); scroll.setWidget(ctr)
        self._form = QFormLayout(ctr); self._form.setSpacing(3); self._form.setContentsMargins(6,6,6,6)
        for fname, ftype, fmin, fmax, tip in VC_FIELDS:
            lbl = QLabel(fname); lbl.setToolTip(tip); lbl.setFixedWidth(200)
            if ftype == "float":
                w = QDoubleSpinBox(); w.setRange(float(fmin), float(fmax)); w.setDecimals(4); w.setSingleStep(0.01)
                w.valueChanged.connect(lambda v, n=fname: self._changed(n, v))
            elif ftype == "int":
                w = QSpinBox(); w.setRange(int(fmin), int(fmax))
                w.valueChanged.connect(lambda v, n=fname: self._changed(n, v))
            elif ftype == "bool":
                w = QCheckBox()
                w.stateChanged.connect(lambda v, n=fname: self._changed(n, int(v > 0)))
            elif ftype == "char":
                w = QComboBox()
                w.addItems(["F","R","4"] if fname == "DriveType" else ["P","D","E"])
                w.currentTextChanged.connect(lambda v, n=fname: self._changed(n, v))
            else:
                w = QLineEdit()
                w.textChanged.connect(lambda v, n=fname: self._changed(n, v))
            w.setToolTip(tip)
            self._field_widgets[fname] = w
            self._form.addRow(lbl, w)
        sp.addWidget(scroll)
        # Right
        right = QWidget(); rl = QVBoxLayout(right); rl.setContentsMargins(2,2,2,2)
        rl.addWidget(QLabel("Vehicle Stats"))
        for label, fn, mx in [("Top Speed","MaxVelocity",200),("Mass","Mass",5000),
                               ("Braking","BrakeDeceleration",30),("Traction","TractionMultiplier",3),
                               ("Engine","EngineAcceleration",20),("Suspension","SuspensionForceLevel",5)]:
            row = QHBoxLayout(); l2 = QLabel(label); l2.setFixedWidth(80)
            bar = QProgressBar(); bar.setRange(0,100); bar.setFixedHeight(18)
            self._stat_bars[fn] = (bar, mx); row.addWidget(l2); row.addWidget(bar); rl.addLayout(row)
        rl.addStretch()
        grp = QGroupBox("Active Flags"); fl = QVBoxLayout(grp)
        for bit, name in list(HANDLING_FLAGS.items())[:16]:
            lbl2 = QLabel(name); lbl2.setStyleSheet("color:#888;")
            lbl2.setFont(QFont("Monospace",8))
            self._flag_labels[bit] = lbl2; fl.addWidget(lbl2)
        rl.addWidget(grp)
        sp.addWidget(right)
        sp.setSizes([200, 600, 220])
        root.addWidget(sp)

    def load_file(self, path: str) -> bool: #vers 1
        if not self._parser: return False
        ok = self._parser.load(path)
        if ok: self._refresh_list()
        return ok

    def save_file(self, path: str) -> bool: #vers 1
        return self._parser.save(path) if self._parser else False

    def _refresh_list(self, ft: str = ""): #vers 1
        self._veh_list.clear()
        for i, e in enumerate(self._parser.entries):
            if ft and ft.lower() not in e.name.lower(): continue
            item = QListWidgetItem(e.name); item.setData(Qt.ItemDataRole.UserRole, i)
            self._veh_list.addItem(item)

    def _on_select(self, row): #vers 1
        item = self._veh_list.item(row)
        if not item: return
        idx = item.data(Qt.ItemDataRole.UserRole)
        if idx is None or idx >= len(self._parser.entries): return
        self._cur_idx = idx; self._populate(self._parser.entries[idx])

    def _populate(self, entry): #vers 1
        self._blocking = True
        for i, (fname, ftype, *_) in enumerate(VC_FIELDS):
            if i >= len(entry.values): break
            w = self._field_widgets.get(fname)
            if not w: continue
            try:
                v = entry.values[i]
                if ftype == "float": w.setValue(float(v))
                elif ftype == "int": w.setValue(int(v))
                elif ftype == "bool": w.setChecked(int(v) != 0)
                elif ftype == "char" and hasattr(w,"setCurrentText"): w.setCurrentText(str(v))
                elif hasattr(w,"setText"): w.setText(str(v))
            except: pass
        self._blocking = False
        self._update_stats(entry)

    def _changed(self, fname, value): #vers 1
        if self._blocking or self._cur_idx < 0: return
        entry = self._parser.entries[self._cur_idx]
        for i, (fn, *_) in enumerate(VC_FIELDS):
            if fn == fname and i < len(entry.values): entry.values[i] = str(value); break
        self._update_stats(entry)

    def _update_stats(self, entry): #vers 1
        fm = {f[0]: i for i, f in enumerate(VC_FIELDS)}
        for fn, (bar, mx) in self._stat_bars.items():
            idx = fm.get(fn)
            if idx is not None and idx < len(entry.values):
                try: v=float(entry.values[idx]); bar.setValue(min(100,int(v/mx*100))); bar.setFormat(f"{v:.1f}")
                except: bar.setValue(0)
        hf = fm.get("HandlingFlags")
        if hf and hf < len(entry.values):
            try:
                flags = int(entry.values[hf], 16)
                for bit, lbl in self._flag_labels.items():
                    lbl.setStyleSheet("color:#50e090;font-weight:bold;" if flags & bit else "color:#888;")
            except: pass

    def _add(self): #vers 1
        if not self._parser: return
        tmpl = self._parser.entries[0].values[:] if self._parser.entries else ["NEWVEHICLE"]+["0.0"]*36
        tmpl[0] = "NEWVEHICLE"
        e = HandlingEntry(); e.values = tmpl
        self._parser.entries.append(e); self._refresh_list(self._search.text())
        self._veh_list.setCurrentRow(self._veh_list.count()-1)

    def _delete(self): #vers 1
        if self._cur_idx < 0 or not self._parser: return
        name = self._parser.entries[self._cur_idx].name
        if QMessageBox.question(self,"Delete",f"Delete {name}?") != QMessageBox.StandardButton.Yes: return
        self._parser.entries.pop(self._cur_idx); self._cur_idx = -1
        self._refresh_list(self._search.text())

    def _dup(self): #vers 1
        if self._cur_idx < 0 or not self._parser: return
        src = self._parser.entries[self._cur_idx]
        e = HandlingEntry(); e.values = src.values[:]; e.values[0] = src.values[0]+"_COPY"
        self._parser.entries.insert(self._cur_idx+1, e); self._refresh_list(self._search.text())


# Tab 2 — Car Colours

class CarColoursTab(QWidget): #vers 1
    def __init__(self, parent=None):
        super().__init__(parent)
        self._parser  = CarColsParser()
        self._cur_veh = -1
        self._sel_col = -1
        self._build_ui()

    def _build_ui(self): #vers 1
        root = QVBoxLayout(self); root.setContentsMargins(4,4,4,4)
        sp = QSplitter(Qt.Orientation.Horizontal)

        # Left
        left = QWidget(); ll = QVBoxLayout(left); ll.setContentsMargins(2,2,2,2)
        ll.addWidget(QLabel("Vehicles"))
        self._search = QLineEdit(); self._search.setPlaceholderText("Search…")
        self._search.textChanged.connect(lambda t: self._refresh_veh_list(t))
        ll.addWidget(self._search)
        self._veh_list = QListWidget()
        self._veh_list.currentRowChanged.connect(self._on_vehicle_selected)
        ll.addWidget(self._veh_list)
        br = QHBoxLayout()
        for lbl, fn in [("Add",self._add_vehicle),("Del",self._delete_vehicle)]:
            b=QPushButton(lbl); b.setFixedHeight(24); b.clicked.connect(fn); br.addWidget(b)
        ll.addLayout(br)
        sp.addWidget(left)

        # Centre
        centre = QWidget(); cl = QVBoxLayout(centre); cl.setContentsMargins(4,4,4,4)
        cl.addWidget(QLabel("Global Colour Palette  (click to select / double-click to edit)"))
        self._pal_grid = ColourSwatchGrid()
        self._pal_grid.colour_clicked = self._on_colour_index_clicked
        pal_scroll = QScrollArea(); pal_scroll.setWidgetResizable(True)
        pal_scroll.setWidget(self._pal_grid); pal_scroll.setFixedHeight(160)
        cl.addWidget(pal_scroll)
        cl.addWidget(QLabel("Vehicle Colour Pairs"))
        self._pairs = QTableWidget(0, 3)
        self._pairs.setHorizontalHeaderLabels(["#","Primary","Secondary"])
        self._pairs.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.Stretch)
        self._pairs.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        cl.addWidget(self._pairs)
        pr = QHBoxLayout()
        for lbl, fn in [("Add Pair",self._add_pair),("Remove Pair",self._remove_pair)]:
            b=QPushButton(lbl); b.setFixedHeight(24); b.clicked.connect(fn); pr.addWidget(b)
        cl.addLayout(pr)
        sp.addWidget(centre)

        # Right
        right = QWidget(); rl = QVBoxLayout(right); rl.setContentsMargins(4,4,4,4)
        rl.addWidget(QLabel("Selected Colour"))
        self._swatch = QFrame(); self._swatch.setFixedSize(80,80)
        self._swatch.setStyleSheet("background:rgb(0,0,0);border:2px solid #555;")
        rl.addWidget(self._swatch)
        self._col_info = QLabel("Index: —\nRGB: —"); rl.addWidget(self._col_info)
        edit_btn = QPushButton("Edit Colour"); edit_btn.clicked.connect(self._edit_selected_colour)
        rl.addWidget(edit_btn); rl.addStretch()
        self._pal_count = QLabel("0 colours"); rl.addWidget(self._pal_count)
        sp.addWidget(right)
        sp.setSizes([200,560,150]); root.addWidget(sp)

    def load_file(self, path: str) -> bool: #vers 1
        ok = self._parser.load(path)
        if ok:
            self._pal_grid.set_palette(self._parser.colours)
            self._pal_count.setText(f"{len(self._parser.colours)} colours")
            self._refresh_veh_list()
        return ok

    def save_file(self, path: str) -> bool: #vers 1
        return self._parser.save(path)

    def _refresh_veh_list(self, ft: str = ""): #vers 1
        self._veh_list.clear()
        for i, v in enumerate(self._parser.vehicles):
            if ft and ft.lower() not in v.name.lower(): continue
            item = QListWidgetItem(v.name); item.setData(Qt.ItemDataRole.UserRole, i)
            self._veh_list.addItem(item)

    def _on_vehicle_selected(self, row): #vers 1
        item = self._veh_list.item(row)
        if not item: return
        idx = item.data(Qt.ItemDataRole.UserRole)
        if idx is None: return
        self._cur_veh = idx; self._populate_vehicle_colours(self._parser.vehicles[idx])

    def _populate_vehicle_colours(self, entry: CarColEntry): #vers 1
        self._pairs.setRowCount(0)
        for i, (p, s) in enumerate(entry.palettes):
            self._pairs.insertRow(i)
            self._pairs.setItem(i, 0, QTableWidgetItem(f"Pair {i+1}"))
            pc = self._parser.colours[p] if p < len(self._parser.colours) else CarColour()
            pi = QTableWidgetItem(f"{p}  ({pc.r},{pc.g},{pc.b})")
            pi.setBackground(pc.to_qcolor())
            pi.setForeground(QColor(255,255,255) if (pc.r+pc.g+pc.b)<384 else QColor(0,0,0))
            self._pairs.setItem(i, 1, pi)
            sc2 = self._parser.colours[s] if s < len(self._parser.colours) else CarColour()
            si = QTableWidgetItem(f"{s}  ({sc2.r},{sc2.g},{sc2.b})")
            si.setBackground(sc2.to_qcolor()); si.setForeground(QColor(255,255,255) if (sc2.r+sc2.g+sc2.b)<384 else QColor(0,0,0))
            self._pairs.setItem(i, 2, si)

    def _on_colour_index_clicked(self, idx): #vers 1
        self._sel_col = idx
        if idx < len(self._parser.colours):
            c = self._parser.colours[idx]
            self._swatch.setStyleSheet(f"background:rgb({c.r},{c.g},{c.b});border:2px solid #555;")
            self._col_info.setText(f"Index: {idx}\nRGB: {c.r}, {c.g}, {c.b}")

    def _edit_selected_colour(self): #vers 1
        idx = self._sel_col
        if idx < 0 or idx >= len(self._parser.colours): return
        c = self._parser.colours[idx]
        col = QColorDialog.getColor(c.to_qcolor(), self, f"Edit Colour {idx}")
        if not col.isValid(): return
        self._parser.colours[idx] = CarColour(col.red(), col.green(), col.blue())
        self._pal_grid.set_palette(self._parser.colours)
        self._on_colour_index_clicked(idx)
        if self._cur_veh >= 0: self._populate_vehicle_colours(self._parser.vehicles[self._cur_veh])

    def _add_vehicle(self): #vers 1
        self._parser.vehicles.append(CarColEntry(name="NEWVEHICLE", palettes=[(0,1)]))
        self._refresh_veh_list(self._search.text())

    def _delete_vehicle(self): #vers 1
        if self._cur_veh < 0: return
        name = self._parser.vehicles[self._cur_veh].name
        if QMessageBox.question(self,"Delete",f"Delete {name}?") != QMessageBox.StandardButton.Yes: return
        self._parser.vehicles.pop(self._cur_veh); self._cur_veh = -1
        self._refresh_veh_list(self._search.text())

    def _add_pair(self): #vers 1
        if self._cur_veh < 0: return
        self._parser.vehicles[self._cur_veh].palettes.append((0,1))
        self._populate_vehicle_colours(self._parser.vehicles[self._cur_veh])

    def _remove_pair(self): #vers 1
        if self._cur_veh < 0: return
        row = self._pairs.currentRow(); v = self._parser.vehicles[self._cur_veh]
        if 0 <= row < len(v.palettes): v.palettes.pop(row); self._populate_vehicle_colours(v)


# Tab 3 — Car Mods (SA only)

class CarModsTab(QWidget): #vers 1
    def __init__(self, parent=None):
        super().__init__(parent)
        self._parser  = CarModsParser()
        self._cur_idx = -1
        self._build_ui()

    def _build_ui(self): #vers 1
        root = QHBoxLayout(self); root.setContentsMargins(4,4,4,4)
        sp = QSplitter(Qt.Orientation.Horizontal)
        left = QWidget(); ll = QVBoxLayout(left); ll.setContentsMargins(2,2,2,2)
        ll.addWidget(QLabel("Vehicles (SA only)"))
        self._search = QLineEdit(); self._search.setPlaceholderText("Search…")
        self._search.textChanged.connect(lambda t: self._refresh_list(t)); ll.addWidget(self._search)
        self._veh_list = QListWidget(); self._veh_list.currentRowChanged.connect(self._on_vehicle_selected)
        ll.addWidget(self._veh_list)
        br = QHBoxLayout()
        for lbl, fn in [("Add",self._add_vehicle),("Del",self._delete_vehicle)]:
            b=QPushButton(lbl); b.setFixedHeight(24); b.clicked.connect(fn); br.addWidget(b)
        ll.addLayout(br); sp.addWidget(left)
        right = QWidget(); rl = QVBoxLayout(right); rl.setContentsMargins(4,4,4,4)
        rl.addWidget(QLabel("Mod Slots"))
        self._mod_list = QListWidget()
        self._mod_list.setSelectionMode(QAbstractItemView.SelectionMode.ExtendedSelection)
        rl.addWidget(self._mod_list)
        mr = QHBoxLayout()
        self._new_mod = QLineEdit(); self._new_mod.setPlaceholderText("mod_model_name"); mr.addWidget(self._new_mod)
        for lbl, fn in [("Add",self._add_mod),("Remove",self._remove_mod)]:
            b=QPushButton(lbl); b.setFixedHeight(24); b.clicked.connect(fn); mr.addWidget(b)
        rl.addLayout(mr); sp.addWidget(right)
        sp.setSizes([250,600]); root.addWidget(sp)

    def load_file(self, path: str) -> bool: #vers 1
        ok = self._parser.load(path)
        if ok: self._refresh_list()
        return ok

    def save_file(self, path: str) -> bool: #vers 1
        return self._parser.save(path)

    def _refresh_list(self, ft: str = ""): #vers 1
        self._veh_list.clear()
        for i, e in enumerate(self._parser.entries):
            if ft and ft.lower() not in e.vehicle.lower(): continue
            item = QListWidgetItem(e.vehicle); item.setData(Qt.ItemDataRole.UserRole, i)
            self._veh_list.addItem(item)

    def _on_vehicle_selected(self, row): #vers 1
        item = self._veh_list.item(row)
        if not item: return
        idx = item.data(Qt.ItemDataRole.UserRole)
        if idx is None: return
        self._cur_idx = idx; self._populate_mods(self._parser.entries[idx])

    def _populate_mods(self, entry: CarModEntry): #vers 1
        self._mod_list.clear()
        for mod in entry.mods: self._mod_list.addItem(mod)

    def _add_vehicle(self): #vers 1
        self._parser.entries.append(CarModEntry(vehicle="newvehicle",mods=[]))
        self._refresh_list(self._search.text())

    def _delete_vehicle(self): #vers 1
        if self._cur_idx < 0: return
        name = self._parser.entries[self._cur_idx].vehicle
        if QMessageBox.question(self,"Delete",f"Delete {name}?") != QMessageBox.StandardButton.Yes: return
        self._parser.entries.pop(self._cur_idx); self._cur_idx = -1
        self._refresh_list(self._search.text())

    def _add_mod(self): #vers 1
        if self._cur_idx < 0: return
        name = self._new_mod.text().strip()
        if not name: return
        entry = self._parser.entries[self._cur_idx]
        entry.mods.append(name); self._populate_mods(entry); self._new_mod.clear()

    def _remove_mod(self): #vers 1
        if self._cur_idx < 0: return
        rows = sorted({i.row() for i in self._mod_list.selectedItems()}, reverse=True)
        entry = self._parser.entries[self._cur_idx]
        for r in rows:
            if 0 <= r < len(entry.mods): entry.mods.pop(r)
        self._populate_mods(entry)


# Main workshop
class VehicleWorkshop(GLViewportMixin, GUIWorkshop): #vers 3

    def __init__(self, main_window=None, parent=None): #vers 4
        super().__init__(parent)
        self.main_window = main_window
        self._handling_path: Optional[str] = None
        self._carcols_path:  Optional[str] = None
        self._carmods_path:  Optional[str] = None
        # Docked mode: no FramelessWindowHint, widget is embedded in tab
        if parent is None and main_window is None:
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint)
        # setup_ui() already called by GUIWorkshop.__init__ via super().__init__()

    def _create_centre_panel(self): #vers 1
        from PyQt6.QtWidgets import QFrame, QVBoxLayout
        panel = QFrame(); panel.setFrameStyle(QFrame.Shape.StyledPanel)
        lay = QVBoxLayout(panel); lay.setContentsMargins(0,0,0,0); lay.setSpacing(0)
        self._tabs = QTabWidget()
        self._tab_handling = HandlingTab()
        self._tab_carcols  = CarColoursTab()
        self._tab_carmods  = CarModsTab()
        self._tab_preview = self._create_preview_tab()
        self._tabs.addTab(self._tab_preview, "3D Preview")
        self._tabs.setCurrentIndex(0)
        self._tabs.addTab(self._tab_handling, "Handling")
        self._tabs.addTab(self._tab_carcols,  "Car Colours")
        self._tabs.addTab(self._tab_carmods,  "Car Mods (SA)")
        lay.addWidget(self._tabs)
        return panel

    def setup_ui(self): #vers 3
        super().setup_ui()

    def _create_preview_tab(self): #vers 1
        """Build the 3D Preview tab with DFFViewport and vehicle controls."""
        from PyQt6.QtWidgets import (QWidget, QVBoxLayout, QHBoxLayout,
                                     QPushButton, QLabel, QComboBox, QSplitter)
        from PyQt6.QtCore import Qt
        tab = QWidget()
        lay = QVBoxLayout(tab); lay.setContentsMargins(4,4,4,4); lay.setSpacing(4)

        # No toolbar bar in 3D Preview tab — controls are in right panel
        # Open DFF/TXD are in the left panel; Render/Assembly/Paint in right panel

        # GL Viewport — use the shared self.viewport created in setup_ui
        self._vw_viewport = self.viewport
        lay.addWidget(self._vw_viewport, 1)

        # Status
        self._vw_status = QLabel('Open a DFF to preview')
        lay.addWidget(self._vw_status)
        return tab

    # Methods from model_viewer


    def _show_progress(self, visible: bool): #vers 1
        if hasattr(self, '_progress'):
            self._progress.setVisible(visible)
            if visible:
                from PyQt6.QtWidgets import QApplication
                QApplication.processEvents()

    def _light_setup_dialog(self): #vers 2
        """Light direction, intensity and vehicle paint colour dialog."""
        dlg = QDialog(self); dlg.setWindowTitle("Light & Paint Setup"); dlg.resize(360, 380)
        lay = QVBoxLayout(dlg)
        form = QFormLayout()

        vp = self.viewport

        # Light direction sliders
        def _slider(lo, hi, val, scale=10):
            s = QSlider(Qt.Orientation.Horizontal)
            s.setRange(int(lo*scale), int(hi*scale))
            s.setValue(int(val*scale))
            return s

        sx = _slider(-10, 10, vp._light_dir[0])
        sy = _slider(-10, 10, vp._light_dir[1])
        sz = _slider(-10, 10, vp._light_dir[2])
        sa = _slider(0, 1, vp._ambient, 100)
        sd = _slider(0, 2, vp._diffuse, 100)

        lx_lbl = QLabel(f"{vp._light_dir[0]:.1f}")
        ly_lbl = QLabel(f"{vp._light_dir[1]:.1f}")
        lz_lbl = QLabel(f"{vp._light_dir[2]:.1f}")
        la_lbl = QLabel(f"{vp._ambient:.2f}")
        ld_lbl = QLabel(f"{vp._diffuse:.2f}")

        def _upd():
            x = sx.value()/10; y = sy.value()/10; z = sz.value()/10
            a = sa.value()/100; d = sd.value()/100
            lx_lbl.setText(f"{x:.1f}"); ly_lbl.setText(f"{y:.1f}"); lz_lbl.setText(f"{z:.1f}")
            la_lbl.setText(f"{a:.2f}"); ld_lbl.setText(f"{d:.2f}")
            vp.set_light_dir(x, y, z); vp.set_ambient(a); vp.set_diffuse(d)

        for s in (sx,sy,sz,sa,sd): s.valueChanged.connect(_upd)

        def _row(lbl, slider, val_lbl):
            rw = QWidget(); rl = QHBoxLayout(rw); rl.setContentsMargins(0,0,0,0)
            rl.addWidget(slider,1); rl.addWidget(val_lbl)
            form.addRow(lbl, rw)

        _row("Light X:", sx, lx_lbl)
        _row("Light Y:", sy, ly_lbl)
        _row("Light Z:", sz, lz_lbl)
        _row("Ambient:", sa, la_lbl)
        _row("Diffuse:", sd, ld_lbl)

        lay.addLayout(form)

        # Paint colours
        from PyQt6.QtWidgets import QGroupBox, QColorDialog
        paint_grp = QGroupBox("Vehicle Paint Preview")
        paint_lay = QHBoxLayout(paint_grp)

        def _colour_btn(label, current_rgb, setter):
            def _to_qcolor(rgb):
                from PyQt6.QtGui import QColor
                return QColor(int(rgb[0]*255), int(rgb[1]*255), int(rgb[2]*255))
            btn = QPushButton(label)
            btn.setStyleSheet(f"background-color: rgb({int(current_rgb[0]*255)},{int(current_rgb[1]*255)},{int(current_rgb[2]*255)})")
            btn.setFixedHeight(28)
            def _pick():
                col = QColorDialog.getColor(_to_qcolor(setter.__self__._paint1 if 'paint1' in label.lower() or 'primary' in label.lower() else setter.__self__._paint2), dlg)
                if col.isValid():
                    rgb = (col.redF(), col.greenF(), col.blueF())
                    setter(rgb)
                    btn.setStyleSheet(f"background-color: {col.name()}")
                    vp.update()
            btn.clicked.connect(_pick)
            return btn

        vp = self.viewport
        p1_btn = QPushButton("Primary Paint")
        p1_btn.setStyleSheet(f"background-color: rgb({int(vp._paint1[0]*255)},{int(vp._paint1[1]*255)},{int(vp._paint1[2]*255)})")
        p1_btn.setFixedHeight(28)
        def _pick1():
            from PyQt6.QtGui import QColor
            col = QColorDialog.getColor(QColor(int(vp._paint1[0]*255),int(vp._paint1[1]*255),int(vp._paint1[2]*255)), dlg)
            if col.isValid():
                vp._paint1 = (col.redF(), col.greenF(), col.blueF())
                p1_btn.setStyleSheet(f"background-color: {col.name()}")
                vp.update()
        p1_btn.clicked.connect(_pick1)

        p2_btn = QPushButton("Secondary Paint")
        p2_btn.setStyleSheet(f"background-color: rgb({int(vp._paint2[0]*255)},{int(vp._paint2[1]*255)},{int(vp._paint2[2]*255)})")
        p2_btn.setFixedHeight(28)
        def _pick2():
            from PyQt6.QtGui import QColor
            col = QColorDialog.getColor(QColor(int(vp._paint2[0]*255),int(vp._paint2[1]*255),int(vp._paint2[2]*255)), dlg)
            if col.isValid():
                vp._paint2 = (col.redF(), col.greenF(), col.blueF())
                p2_btn.setStyleSheet(f"background-color: {col.name()}")
                vp.update()
        p2_btn.clicked.connect(_pick2)

        paint_lay.addWidget(p1_btn); paint_lay.addWidget(p2_btn)
        lay.addWidget(paint_grp)

        reset_btn = QPushButton("Reset Defaults")
        def _reset():
            sx.setValue(10); sy.setValue(20); sz.setValue(15)
            sa.setValue(30); sd.setValue(85)
            vp._paint1 = (0.80, 0.20, 0.20)
            vp._paint2 = (0.20, 0.20, 0.80)
            p1_btn.setStyleSheet("background-color: rgb(204,51,51)")
            p2_btn.setStyleSheet("background-color: rgb(51,51,204)")
            vp.update()
        reset_btn.clicked.connect(_reset)

        btn_row = QHBoxLayout()
        close_btn = QPushButton("Close"); close_btn.clicked.connect(dlg.accept)
        btn_row.addWidget(reset_btn); btn_row.addStretch(); btn_row.addWidget(close_btn)
        lay.addLayout(btn_row)
        dlg.exec()

    def _vw_set_mode(self, mode): #vers 1
        vp = getattr(self,'_vw_viewport',None)
        if vp and hasattr(vp,'set_render_mode'): vp.set_render_mode(mode)

    def _vw_pick_dff(self): #vers 1
        from PyQt6.QtWidgets import QFileDialog
        path,_=QFileDialog.getOpenFileName(self,'Open DFF','','DFF (*.dff);;All (*)')
        if not path: return
        self._vw_load_dff(path)

    def _vw_pick_txd(self): #vers 1
        from PyQt6.QtWidgets import QFileDialog
        path,_=QFileDialog.getOpenFileName(self,'Open TXD','','TXD (*.txd);;All (*)')
        if not path: return
        self._vw_load_txd(path)

    def _vw_load_dff(self, path: str): #vers 1
        try:
            from apps.methods.dff_parser import load_dff
            m = load_dff(path)
            if not m: return
            self._vw_model = m
            vp = getattr(self,'_vw_viewport',None)
            if vp and hasattr(vp,'load_geometry') and m.geometries:
                g = m.geometries[0]; vp.load_geometry(g, g.materials)
            self._vw_status.setText(f'{os.path.basename(path)} — {len(m.geometries)} geoms')
            # Auto-load matching TXD
            import os as _os
            for ext in ('.txd','.TXD'):
                txd=_os.path.splitext(path)[0]+ext
                if _os.path.isfile(txd): self._vw_load_txd(txd); break
        except Exception as e:
            self._vw_status.setText(f'Error: {e}')

    def _vw_load_txd(self, path: str): #vers 1
        try:
            from apps.methods.txd_parser import parse_txd
            with open(path,'rb') as f: data=f.read()
            textures=parse_txd(data)
            vp=getattr(self,'_vw_viewport',None)
            if vp and hasattr(vp,'_upload_textures') and textures:
                vp._upload_textures(textures)
                vp.update()
        except Exception as e:
            self._vw_status.setText(f'TXD error: {e}')

    def _vw_toggle_assembly(self, enabled: bool): #vers 1
        m = getattr(self,'_vw_model',None)
        vp = getattr(self,'_vw_viewport',None)
        if not m or not vp or not hasattr(vp,'set_assembly_mode'): return
        vp.set_assembly_mode(enabled)
        if enabled:
            vp.load_all_geometries(m.geometries,[g.materials for g in m.geometries],
                                   m.frames,m.atomics)
        elif m.geometries:
            vp.load_geometry(m.geometries[0],m.geometries[0].materials)

    def _vw_pick_paint1(self): #vers 1
        from PyQt6.QtWidgets import QColorDialog
        from PyQt6.QtGui import QColor
        vp=getattr(self,'_vw_viewport',None)
        if not vp or not hasattr(vp,'_paint1'): return
        p=vp._paint1
        col=QColorDialog.getColor(QColor(int(p[0]*255),int(p[1]*255),int(p[2]*255)),self)
        if col.isValid():
            vp._paint1=(col.redF(),col.greenF(),col.blueF())
            self._vw_paint1.setStyleSheet(f'background:{col.name()}')
            vp.update()

    def _vw_pick_paint2(self): #vers 1
        from PyQt6.QtWidgets import QColorDialog
        from PyQt6.QtGui import QColor
        vp=getattr(self,'_vw_viewport',None)
        if not vp or not hasattr(vp,'_paint2'): return
        p=vp._paint2
        col=QColorDialog.getColor(QColor(int(p[0]*255),int(p[1]*255),int(p[2]*255)),self)
        if col.isValid():
            vp._paint2=(col.redF(),col.greenF(),col.blueF())
            self._vw_paint2.setStyleSheet(f'background:{col.name()}')
            vp.update()

    def _open_file(self, path=None): #vers 1

        if path is None:
            path, _ = QFileDialog.getOpenFileName(
                self, "Open Vehicle Data File", "",
                "Vehicle data (handling.cfg carcols.dat carmods.dat *.cfg *.dat);;All files (*)")
        if not path: return
        name = os.path.basename(path).lower()
        if "handling" in name:
            if self._tab_handling.load_file(path):
                self._handling_path = path
                self._tabs.setCurrentWidget(self._tab_handling)
                self._set_status(f"Handling: {os.path.basename(path)}")
        elif "carcols" in name:
            if self._tab_carcols.load_file(path):
                self._carcols_path = path
                self._tabs.setCurrentWidget(self._tab_carcols)
                self._set_status(f"Car Colours: {os.path.basename(path)}")
        elif "carmods" in name:
            if self._tab_carmods.load_file(path):
                self._carmods_path = path
                self._tabs.setCurrentWidget(self._tab_carmods)
                self._set_status(f"Car Mods: {os.path.basename(path)}")
        else:
            QMessageBox.warning(self, "Unknown", "Expected handling.cfg, carcols.dat or carmods.dat")

    def _save_file(self): #vers 1
        idx = self._tabs.currentIndex()
        if idx == 0 and self._handling_path:
            self._tab_handling.save_file(self._handling_path)
            self._set_status(f"Saved {os.path.basename(self._handling_path)}")
        elif idx == 1 and self._carcols_path:
            self._tab_carcols.save_file(self._carcols_path)
            self._set_status(f"Saved {os.path.basename(self._carcols_path)}")
        elif idx == 2 and self._carmods_path:
            self._tab_carmods.save_file(self._carmods_path)
            self._set_status(f"Saved {os.path.basename(self._carmods_path)}")
        else:
            self._save_as()

    def _save_as(self): #vers 1
        idx = self._tabs.currentIndex()
        hints = ["Handling (handling.cfg *.cfg)", "Car Colours (carcols.dat *.dat)", "Car Mods (carmods.dat *.dat)"]
        path, _ = QFileDialog.getSaveFileName(self, "Save As", "", hints[idx])
        if not path: return
        if idx == 0: self._handling_path = path; self._tab_handling.save_file(path)
        elif idx == 1: self._carcols_path = path; self._tab_carcols.save_file(path)
        elif idx == 2: self._carmods_path = path; self._tab_carmods.save_file(path)

    def _open_specific(self, kind: str): #vers 1
        f = {"handling":"Handling (handling.cfg *.cfg)",
             "carcols": "Car Colours (carcols.dat *.dat)",
             "carmods":  "Car Mods (carmods.dat *.dat)"}[kind]
        path, _ = QFileDialog.getOpenFileName(self, f"Open {kind}", "", f)
        if path: self._open_file(path)

    def _build_menus_into_qmenu(self, pm): #vers 1
        fm = pm.addMenu("File")
        fm.addAction("Open…",              self._open_file)
        fm.addAction("Save",               self._save_file)
        fm.addAction("Save As…",           self._save_as)
        fm.addSeparator()
        fm.addAction("Open handling.cfg",  lambda: self._open_specific("handling"))
        fm.addAction("Open carcols.dat",   lambda: self._open_specific("carcols"))
        fm.addAction("Open carmods.dat",   lambda: self._open_specific("carmods"))
        fm.addSeparator()
        fm.addAction("Close", self.close)


# - Main workshop widget (RadarWorkshop pattern)
def open_vehicle_workshop(main_window=None, path: str = None): #vers 1
    if not QApplication.instance():
        try:
            from PyQt6.QtGui import QSurfaceFormat
            _f = QSurfaceFormat()
            _f.setProfile(QSurfaceFormat.OpenGLContextProfile.CompatibilityProfile)
            _f.setVersion(2, 1)
            QSurfaceFormat.setDefaultFormat(_f)
        except Exception:
            pass
    app = QApplication.instance() or QApplication(sys.argv)
    w = VehicleWorkshop(main_window)
    w.resize(1200, 720)
    w.show()
    if path: w._open_file(path)
    return w


if __name__ == "__main__":
    # Must set compat profile BEFORE QApplication
    try:
        from PyQt6.QtGui import QSurfaceFormat
        _f = QSurfaceFormat()
        _f.setProfile(QSurfaceFormat.OpenGLContextProfile.CompatibilityProfile)
        _f.setVersion(2, 1)
        QSurfaceFormat.setDefaultFormat(_f)
    except Exception:
        pass
    app = QApplication(sys.argv)
    w = VehicleWorkshop()
    w.resize(1200, 720)
    w.show()
    sys.exit(app.exec())
