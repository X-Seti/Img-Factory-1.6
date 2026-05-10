#this belongs in apps/components/Model_Viewer/model_viewer.py - Version: 1
# X-Seti - May10 2026 - IMG Factory 1.6 - DFF Model Viewer
"""
DFF Model Viewer - OpenGL-based 3D model viewer for GTA RenderWare DFF files.
Clean implementation using QOpenGLWidget with proper hardware rendering.
View-only — no editing. Orbit/pan/zoom, wireframe/solid/textured modes,
backface culling, texture upload from TXD.
"""

import os, sys, math, struct, ctypes
from typing import Optional, List, Dict

current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(os.path.dirname(os.path.dirname(current_dir)))
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

from PyQt6.QtWidgets import (
    QApplication, QWidget, QVBoxLayout, QHBoxLayout, QSplitter,
    QLabel, QPushButton, QListWidget, QListWidgetItem, QFrame,
    QFileDialog, QSizePolicy, QButtonGroup, QComboBox, QCheckBox,
    QMessageBox
)
from PyQt6.QtCore import Qt, QPoint, QSize, QTimer, pyqtSignal
from PyQt6.QtGui import QColor, QFont, QImage
from PyQt6.QtOpenGLWidgets import QOpenGLWidget
from PyQt6.QtOpenGL import QOpenGLTexture

try:
    from OpenGL.GL import *
    from OpenGL.GLU import *
    OPENGL_AVAILABLE = True
except ImportError:
    OPENGL_AVAILABLE = False
    print("[ModelViewer] PyOpenGL not available — install python3-opengl")

try:
    from apps.methods.imgfactory_svg_icons import SVGIconFactory
    ICONS_AVAILABLE = True
except ImportError:
    ICONS_AVAILABLE = False

## Methods list -
# DFFViewport.__init__
# DFFViewport._get_ui_color
# DFFViewport.initializeGL
# DFFViewport.resizeGL
# DFFViewport.paintGL
# DFFViewport._draw_grid
# DFFViewport._draw_axes
# DFFViewport._draw_wireframe
# DFFViewport._draw_solid
# DFFViewport._draw_textured
# DFFViewport._upload_textures
# DFFViewport._auto_fit
# DFFViewport.load_geometry
# DFFViewport.clear_textures
# DFFViewport.set_render_mode
# DFFViewport.set_backface_cull
# DFFViewport.set_show_grid
# DFFViewport.mousePressEvent
# DFFViewport.mouseMoveEvent
# DFFViewport.mouseReleaseEvent
# DFFViewport.wheelEvent
# DFFViewport.reset_camera
# ModelViewerWidget.__init__
# ModelViewerWidget._build_ui
# ModelViewerWidget._build_toolbar
# ModelViewerWidget._build_geom_panel
# ModelViewerWidget._open_dff
# ModelViewerWidget._open_txd
# ModelViewerWidget._on_geom_selected
# ModelViewerWidget._populate_geom_list
# ModelViewerWidget._set_status
# ModelViewerWidget.load_dff
# ModelViewerWidget.load_txd
# open_model_viewer


# ─────────────────────────────────────────────────────────────
#  OpenGL viewport
# ─────────────────────────────────────────────────────────────

class DFFViewport(QOpenGLWidget if OPENGL_AVAILABLE else QWidget):
    """Hardware-accelerated OpenGL viewport for DFF geometry."""

    def __init__(self, parent=None):
        super().__init__(parent)

        # Geometry data (set via load_geometry)
        self._vertices:  List  = []   # list of (x,y,z)
        self._normals:   List  = []   # list of (nx,ny,nz) — same length as _vertices
        self._uvs:       List  = []   # list of (u,v)
        self._triangles: List  = []   # list of (v1,v2,v3,mat_id)
        self._materials: List  = []   # list of dff_classes.Material
        self._tex_ids:   Dict[str,int] = {}   # texture_name -> GL texture id

        # Camera
        self._dist   = 10.0
        self._yaw    = 45.0
        self._pitch  = 25.0
        self._pan_x  = 0.0
        self._pan_y  = 0.0

        # Mouse
        self._last_pos   = QPoint()
        self._dragging   = False
        self._drag_btn   = Qt.MouseButton.NoButton

        # Render state
        self._mode          = 'solid'   # 'wireframe' | 'solid' | 'textured'
        self._backface_cull = True
        self._show_grid     = True

        self.setMinimumSize(400, 300)
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)

    # ── helpers ──────────────────────────────────────────────

    def _get_ui_color(self, key): #vers 1
        pal = self.palette()
        defaults = {
            'viewport_bg': pal.color(pal.ColorRole.Base),
            'grid':        QColor(60, 65, 80),
        }
        return defaults.get(key, QColor(200, 200, 200))

    # ── OpenGL lifecycle ─────────────────────────────────────

    def initializeGL(self): #vers 1
        if not OPENGL_AVAILABLE:
            return
        bg = self._get_ui_color('viewport_bg')
        glClearColor(bg.redF(), bg.greenF(), bg.blueF(), 1.0)
        glEnable(GL_DEPTH_TEST)
        glDepthFunc(GL_LEQUAL)
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
        glShadeModel(GL_SMOOTH)
        # Fixed-function lighting
        glEnable(GL_LIGHTING)
        glEnable(GL_LIGHT0)
        glEnable(GL_COLOR_MATERIAL)
        glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE)
        glLightfv(GL_LIGHT0, GL_POSITION, [1.0, 2.0, 1.5, 0.0])
        glLightfv(GL_LIGHT0, GL_AMBIENT,  [0.30, 0.30, 0.30, 1.0])
        glLightfv(GL_LIGHT0, GL_DIFFUSE,  [0.85, 0.85, 0.85, 1.0])
        glLightfv(GL_LIGHT0, GL_SPECULAR, [0.20, 0.20, 0.20, 1.0])
        glEnable(GL_NORMALIZE)

    def resizeGL(self, w, h): #vers 1
        if not OPENGL_AVAILABLE:
            return
        glViewport(0, 0, w, h)
        glMatrixMode(GL_PROJECTION)
        glLoadIdentity()
        aspect = w / h if h > 0 else 1.0
        gluPerspective(45.0, aspect, 0.01, 10000.0)
        glMatrixMode(GL_MODELVIEW)

    def paintGL(self): #vers 1
        if not OPENGL_AVAILABLE:
            return
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
        glLoadIdentity()

        # Camera transform
        glTranslatef(self._pan_x, self._pan_y, -self._dist)
        glRotatef(self._pitch, 1, 0, 0)
        glRotatef(self._yaw,   0, 1, 0)

        if self._show_grid:
            self._draw_grid()
            self._draw_axes()

        if not self._vertices:
            return

        # Backface culling
        if self._backface_cull:
            glEnable(GL_CULL_FACE)
            glCullFace(GL_BACK)
        else:
            glDisable(GL_CULL_FACE)

        if self._mode == 'wireframe':
            self._draw_wireframe()
        elif self._mode == 'solid':
            self._draw_solid()
        elif self._mode == 'textured':
            self._draw_textured()

    # ── draw calls ───────────────────────────────────────────

    def _draw_grid(self): #vers 1
        glDisable(GL_LIGHTING)
        gc = self._get_ui_color('grid')
        glColor3f(gc.redF(), gc.greenF(), gc.blueF())
        glLineWidth(1.0)
        step = max(0.5, self._dist / 20.0)
        count = 20
        half = count * step
        glBegin(GL_LINES)
        for i in range(-count, count + 1):
            p = i * step
            glVertex3f(p, 0, -half); glVertex3f(p, 0,  half)
            glVertex3f(-half, 0, p); glVertex3f( half, 0, p)
        glEnd()

    def _draw_axes(self): #vers 1
        glDisable(GL_LIGHTING)
        glLineWidth(2.0)
        r = self._dist * 0.15
        glBegin(GL_LINES)
        glColor3f(1,0,0); glVertex3f(0,0,0); glVertex3f(r,0,0)
        glColor3f(0,1,0); glVertex3f(0,0,0); glVertex3f(0,r,0)
        glColor3f(0,0,1); glVertex3f(0,0,0); glVertex3f(0,0,r)
        glEnd()

    def _draw_wireframe(self): #vers 1
        glDisable(GL_LIGHTING)
        glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
        glColor3f(0.7, 0.8, 1.0)
        glLineWidth(0.8)
        self._emit_triangles(use_tex=False)
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)

    def _draw_solid(self): #vers 1
        glEnable(GL_LIGHTING)
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
        # Draw filled faces with material colour
        verts = self._vertices
        norms = self._normals
        has_n = len(norms) == len(verts)
        mats  = self._materials

        glBegin(GL_TRIANGLES)
        for v1, v2, v3, mat_id in self._triangles:
            # Material colour
            if mats and 0 <= mat_id < len(mats):
                c = mats[mat_id].colour
                r = getattr(c,'r',200)/255; g = getattr(c,'g',200)/255; b = getattr(c,'b',200)/255
            else:
                r = g = b = 0.75
            glColor3f(r, g, b)
            for vi in (v1, v2, v3):
                if vi < len(verts):
                    if has_n and vi < len(norms):
                        n = norms[vi]; glNormal3f(n[0], n[1], n[2])
                    v = verts[vi]; glVertex3f(v[0], v[1], v[2])
        glEnd()

        # Wireframe overlay
        glDisable(GL_LIGHTING)
        glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
        glColor4f(0, 0, 0, 0.25)
        glLineWidth(0.5)
        glEnable(GL_POLYGON_OFFSET_LINE)
        glPolygonOffset(-1, -1)
        self._emit_triangles(use_tex=False, color_override=(0,0,0,0.15))
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
        glDisable(GL_POLYGON_OFFSET_LINE)

    def _draw_textured(self): #vers 1
        glEnable(GL_LIGHTING)
        glEnable(GL_TEXTURE_2D)
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)

        verts = self._vertices
        norms = self._normals
        uvs   = self._uvs
        has_n = len(norms) == len(verts)
        has_u = len(uvs)   == len(verts)
        mats  = self._materials

        # Group by texture to minimise bind calls
        by_tex: Dict[int, list] = {}   # gl_id → [(v1,v2,v3,mat_id),...]
        no_tex: list = []

        for tri in self._triangles:
            v1, v2, v3, mat_id = tri
            tex_name = ''
            if mats and 0 <= mat_id < len(mats):
                tex_name = getattr(mats[mat_id], 'texture_name', '') or ''
            gl_id = self._tex_ids.get(tex_name.lower(), 0)
            if gl_id:
                by_tex.setdefault(gl_id, []).append(tri)
            else:
                no_tex.append(tri)

        # Textured pass
        for gl_id, tris in by_tex.items():
            glBindTexture(GL_TEXTURE_2D, gl_id)
            glColor3f(1, 1, 1)
            glBegin(GL_TRIANGLES)
            for v1, v2, v3, mat_id in tris:
                for vi in (v1, v2, v3):
                    if vi < len(verts):
                        if has_n and vi < len(norms):
                            n = norms[vi]; glNormal3f(n[0], n[1], n[2])
                        if has_u and vi < len(uvs):
                            u = uvs[vi]; glTexCoord2f(u[0], 1.0 - u[1])
                        v = verts[vi]; glVertex3f(v[0], v[1], v[2])
            glEnd()

        # Untextured pass — solid colour
        glBindTexture(GL_TEXTURE_2D, 0)
        glDisable(GL_TEXTURE_2D)
        for tri in no_tex:
            v1, v2, v3, mat_id = tri
            if mats and 0 <= mat_id < len(mats):
                c = mats[mat_id].colour
                glColor3f(getattr(c,'r',180)/255, getattr(c,'g',180)/255, getattr(c,'b',180)/255)
            else:
                glColor3f(0.7, 0.7, 0.7)
            glBegin(GL_TRIANGLES)
            for vi in (v1, v2, v3):
                if vi < len(verts):
                    if has_n and vi < len(norms):
                        n = norms[vi]; glNormal3f(n[0], n[1], n[2])
                    v = verts[vi]; glVertex3f(v[0], v[1], v[2])
            glEnd()
        glEnable(GL_TEXTURE_2D)

    def _emit_triangles(self, use_tex=False, color_override=None): #vers 1
        """Simple triangle emit without per-material handling."""
        verts = self._vertices
        if color_override:
            glColor4f(*color_override)
        glBegin(GL_TRIANGLES)
        for v1, v2, v3, mat_id in self._triangles:
            for vi in (v1, v2, v3):
                if vi < len(verts):
                    v = verts[vi]; glVertex3f(v[0], v[1], v[2])
        glEnd()

    # ── texture upload ────────────────────────────────────────

    def _upload_textures(self, textures: list): #vers 1
        """Upload list of {name, rgba_data, width, height} dicts as GL textures."""
        if not OPENGL_AVAILABLE:
            return
        self.makeCurrent()
        self.clear_textures()
        for tex in textures:
            name  = tex.get('name', '').lower()
            rgba  = tex.get('rgba_data', b'')
            w     = tex.get('width',  0)
            h     = tex.get('height', 0)
            if not (name and rgba and w > 0 and h > 0):
                continue
            gl_id = glGenTextures(1)
            glBindTexture(GL_TEXTURE_2D, gl_id)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
            try:
                glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0,
                             GL_RGBA, GL_UNSIGNED_BYTE, rgba)
                glGenerateMipmap(GL_TEXTURE_2D)
                self._tex_ids[name] = gl_id
            except Exception as e:
                print(f"[ModelViewer] Texture upload failed '{name}': {e}")
                glDeleteTextures(1, [gl_id])
        glBindTexture(GL_TEXTURE_2D, 0)
        self.doneCurrent()

    # ── camera fit ────────────────────────────────────────────

    def _auto_fit(self): #vers 1
        """Fit camera to loaded geometry bounding box."""
        if not self._vertices:
            return
        xs = [v[0] for v in self._vertices]
        ys = [v[1] for v in self._vertices]
        zs = [v[2] for v in self._vertices]
        cx = (max(xs)+min(xs))/2
        cy = (max(ys)+min(ys))/2
        cz = (max(zs)+min(zs))/2
        diag = math.sqrt((max(xs)-min(xs))**2 + (max(ys)-min(ys))**2 + (max(zs)-min(zs))**2)
        self._dist  = max(diag * 1.5, 2.0)
        self._pan_x = -cx
        self._pan_y = -cy
        self.update()

    # ── public API ────────────────────────────────────────────

    def load_geometry(self, geometry, materials: list): #vers 1
        """Load a DFF Geometry object into the viewport."""
        self._vertices  = [(v.x, v.y, v.z) for v in geometry.vertices]
        self._normals   = [(n.x, n.y, n.z) for n in geometry.normals] if geometry.normals else []
        self._uvs       = [(u.u, u.v) for u in geometry.uv_layers[0]] if geometry.uv_layers else []
        self._triangles = [(t.v1, t.v2, t.v3, t.material_id) for t in geometry.triangles]
        self._materials = materials
        self._auto_fit()
        self.update()

    def clear_textures(self): #vers 1
        """Delete all uploaded GL textures."""
        if OPENGL_AVAILABLE and self._tex_ids:
            ids = list(self._tex_ids.values())
            try:
                glDeleteTextures(len(ids), ids)
            except Exception:
                pass
        self._tex_ids.clear()

    def set_render_mode(self, mode: str): #vers 1
        """'wireframe' | 'solid' | 'textured'"""
        self._mode = mode
        self.update()

    def set_backface_cull(self, enabled: bool): #vers 1
        self._backface_cull = enabled
        self.update()

    def set_show_grid(self, enabled: bool): #vers 1
        self._show_grid = enabled
        self.update()

    def reset_camera(self): #vers 1
        self._yaw = 45.0; self._pitch = 25.0
        self._pan_x = 0.0; self._pan_y = 0.0
        self._auto_fit()

    # ── mouse ─────────────────────────────────────────────────

    def mousePressEvent(self, event): #vers 1
        self._last_pos = event.pos()
        self._dragging = True
        self._drag_btn = event.button()

    def mouseMoveEvent(self, event): #vers 1
        if not self._dragging:
            return
        dx = event.pos().x() - self._last_pos.x()
        dy = event.pos().y() - self._last_pos.y()
        if self._drag_btn == Qt.MouseButton.LeftButton:
            self._yaw   += dx * 0.5
            self._pitch += dy * 0.5
            self._pitch  = max(-89, min(89, self._pitch))
        elif self._drag_btn == Qt.MouseButton.MiddleButton:
            scale = self._dist / 500.0
            self._pan_x += dx * scale
            self._pan_y -= dy * scale
        self._last_pos = event.pos()
        self.update()

    def mouseReleaseEvent(self, event): #vers 1
        self._dragging = False
        self._drag_btn = Qt.MouseButton.NoButton

    def wheelEvent(self, event): #vers 1
        delta = event.angleDelta().y()
        factor = 0.88 if delta > 0 else 1.13
        self._dist = max(0.1, min(50000.0, self._dist * factor))
        self.update()


# ─────────────────────────────────────────────────────────────
#  Main viewer widget
# ─────────────────────────────────────────────────────────────

class ModelViewerWidget(QWidget):
    """Full model viewer — toolbar + geometry list + OpenGL viewport."""

    fileLoaded = pyqtSignal(str)

    def __init__(self, parent=None, main_window=None):
        super().__init__(parent)
        self.main_window   = main_window
        self._dff_model    = None
        self._last_dir     = ''
        self._current_geom = 0
        self._build_ui()

    # ── UI construction ───────────────────────────────────────

    def _build_ui(self): #vers 2
        root = QVBoxLayout(self)
        root.setContentsMargins(0, 0, 0, 0)
        root.setSpacing(0)

        # Viewport must exist before toolbar (toolbar buttons reference it)
        self.viewport = DFFViewport()

        root.addWidget(self._build_toolbar())

        # Splitter: geom list | viewport
        splitter = QSplitter(Qt.Orientation.Horizontal)
        splitter.addWidget(self._build_geom_panel())
        splitter.addWidget(self.viewport)
        splitter.setStretchFactor(0, 0)
        splitter.setStretchFactor(1, 1)
        splitter.setSizes([180, 600])
        root.addWidget(splitter, 1)

        # Status bar
        self._status_lbl = QLabel("No model loaded")
        self._status_lbl.setContentsMargins(6, 2, 6, 2)
        root.addWidget(self._status_lbl)

    def _build_toolbar(self) -> QFrame: #vers 1
        bar = QFrame()
        bar.setFrameShape(QFrame.Shape.StyledPanel)
        bar.setFixedHeight(40)
        lay = QHBoxLayout(bar)
        lay.setContentsMargins(6, 2, 6, 2)
        lay.setSpacing(4)

        def _btn(text, tip, cb, checkable=False, checked=False):
            b = QPushButton(text)
            b.setToolTip(tip)
            b.setFixedHeight(28)
            if checkable:
                b.setCheckable(True)
                b.setChecked(checked)
                b.toggled.connect(cb)
            else:
                b.clicked.connect(cb)
            return b

        lay.addWidget(_btn("Open DFF", "Open DFF model file", self._open_dff))
        lay.addWidget(_btn("Open TXD", "Open TXD texture file", self._open_txd))
        lay.addSpacing(8)

        # Render mode group
        self._mode_group = QButtonGroup(self)
        for label, mode in [("Wire","wireframe"),("Solid","solid"),("Tex","textured")]:
            b = QPushButton(label)
            b.setCheckable(True)
            b.setFixedHeight(28)
            b.setFixedWidth(44)
            b.setToolTip(f"{mode.capitalize()} render mode")
            b.clicked.connect(lambda _, m=mode: self.viewport.set_render_mode(m))
            self._mode_group.addButton(b)
            lay.addWidget(b)
            if mode == 'solid':
                b.setChecked(True)

        lay.addSpacing(8)
        self._cull_btn = _btn("Cull", "Toggle backface culling", self.viewport.set_backface_cull, checkable=True, checked=True)
        self._cull_btn.setFixedWidth(44)
        lay.addWidget(self._cull_btn)

        self._grid_btn = _btn("Grid", "Toggle grid", self.viewport.set_show_grid, checkable=True, checked=True)
        self._grid_btn.setFixedWidth(44)
        lay.addWidget(self._grid_btn)

        lay.addSpacing(8)
        lay.addWidget(_btn("Reset", "Reset camera", self.viewport.reset_camera))

        lay.addStretch()

        self._info_lbl = QLabel("")
        self._info_lbl.setAlignment(Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter)
        lay.addWidget(self._info_lbl)

        return bar

    def _build_geom_panel(self) -> QWidget: #vers 1
        panel = QWidget()
        panel.setFixedWidth(180)
        lay = QVBoxLayout(panel)
        lay.setContentsMargins(4, 4, 4, 4)
        lay.setSpacing(4)

        lbl = QLabel("Geometries")
        lbl.setFont(QFont("", -1, QFont.Weight.Bold))
        lay.addWidget(lbl)

        self._geom_list = QListWidget()
        self._geom_list.currentRowChanged.connect(self._on_geom_selected)
        lay.addWidget(self._geom_list, 1)

        lbl2 = QLabel("Textures")
        lbl2.setFont(QFont("", -1, QFont.Weight.Bold))
        lay.addWidget(lbl2)

        self._tex_list = QListWidget()
        self._tex_list.setFixedHeight(120)
        lay.addWidget(self._tex_list)

        return panel

    # ── file loading ──────────────────────────────────────────

    def _open_dff(self): #vers 1
        path, _ = QFileDialog.getOpenFileName(
            self, "Open DFF", self._last_dir,
            "DFF Models (*.dff);;All Files (*)")
        if path:
            self._last_dir = os.path.dirname(path)
            self.load_dff(path)
            # Auto-find TXD
            for ext in ('.txd', '.TXD'):
                txd = os.path.splitext(path)[0] + ext
                if os.path.isfile(txd):
                    self.load_txd(txd)
                    break

    def _open_txd(self): #vers 1
        path, _ = QFileDialog.getOpenFileName(
            self, "Open TXD", self._last_dir,
            "TXD Files (*.txd);;All Files (*)")
        if path:
            self._last_dir = os.path.dirname(path)
            self.load_txd(path)

    def load_dff(self, path: str): #vers 1
        try:
            from apps.methods.dff_parser import load_dff
            model = load_dff(path)
            if not model or not model.geometries:
                self._set_status(f"Failed to load: {os.path.basename(path)}")
                return
            self._dff_model = model
            self._populate_geom_list()
            self._geom_list.setCurrentRow(0)
            self._set_status(f"Loaded: {os.path.basename(path)} — "
                             f"{len(model.geometries)} geometries, "
                             f"{len(model.frames)} frames")
            self.fileLoaded.emit(path)
        except Exception as e:
            import traceback; traceback.print_exc()
            self._set_status(f"Error: {e}")

    def load_txd(self, path: str): #vers 1
        try:
            from apps.methods.txd_parser import parse_txd
            with open(path, 'rb') as f:
                data = f.read()
            textures = parse_txd(data)
            if not textures:
                self._set_status(f"No textures in {os.path.basename(path)}")
                return
            self.viewport._upload_textures(textures)
            self._tex_list.clear()
            for t in textures:
                item = QListWidgetItem(f"{t['name']}  {t['width']}×{t['height']}")
                self._tex_list.addItem(item)
            self._set_status(f"Textures: {len(textures)} from {os.path.basename(path)}")
            self.viewport.update()
        except Exception as e:
            self._set_status(f"TXD error: {e}")

    # ── geometry list ─────────────────────────────────────────

    def _populate_geom_list(self): #vers 1
        self._geom_list.clear()
        if not self._dff_model:
            return
        for i, g in enumerate(self._dff_model.geometries):
            # Find frame name via atomic
            name = f"geometry_{i}"
            atomic = next((a for a in self._dff_model.atomics if a.geometry_index == i), None)
            if atomic and atomic.frame_index < len(self._dff_model.frames):
                fn = self._dff_model.frames[atomic.frame_index].name
                if fn:
                    name = fn
            label = f"[{i}] {name}  {len(g.vertices)}v {len(g.triangles)}t"
            self._geom_list.addItem(QListWidgetItem(label))

    def _on_geom_selected(self, row: int): #vers 1
        if not self._dff_model or row < 0 or row >= len(self._dff_model.geometries):
            return
        self._current_geom = row
        g = self._dff_model.geometries[row]
        self.viewport.load_geometry(g, g.materials)
        self._info_lbl.setText(f"V:{len(g.vertices)}  T:{len(g.triangles)}  M:{len(g.materials)}")

    # ── status ────────────────────────────────────────────────

    def _set_status(self, msg: str): #vers 1
        self._status_lbl.setText(msg)
        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message(f"[ModelViewer] {msg}")


# ─────────────────────────────────────────────────────────────
#  Entry point
# ─────────────────────────────────────────────────────────────

def open_model_viewer(main_window=None, dff_path=None, txd_path=None): #vers 1
    """Open the model viewer as a floating window."""
    from PyQt6.QtWidgets import QMainWindow
    win = QMainWindow(main_window)
    win.setWindowTitle("Model Viewer")
    win.resize(1000, 700)
    viewer = ModelViewerWidget(main_window=main_window)
    win.setCentralWidget(viewer)
    win.show()
    if dff_path:
        viewer.load_dff(dff_path)
    if txd_path:
        viewer.load_txd(txd_path)
    return win, viewer


# Standalone launch
if __name__ == '__main__':
    app = QApplication(sys.argv)
    win, viewer = open_model_viewer()
    if len(sys.argv) > 1:
        viewer.load_dff(sys.argv[1])
    if len(sys.argv) > 2:
        viewer.load_txd(sys.argv[2])
    sys.exit(app.exec())

__all__ = ['ModelViewerWidget', 'DFFViewport', 'open_model_viewer']
