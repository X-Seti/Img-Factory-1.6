#!/usr/bin/env python3
#this belongs in apps/methods/gl_viewport_mixin.py - Version: 1
# X-Seti - May11 2026 - IMG Factory 1.6 - GL Viewport Mixin
"""
GLViewportMixin — adds OpenGL DFFViewport toggle to any workshop that uses
a QPainter-based COL3DViewport as self.preview_widget.

Usage:
    class MyWorkshop(GLViewportMixin, QWidget):
        ...
        def _setup_preview(self):
            self.preview_widget = COL3DViewport()
            self.preview_row.addWidget(self.preview_widget)
            self.setup_gl_toggle(self.preview_row)  # call after adding QPainter widget

GLViewportMixin adds:
  - "GL" toggle button (injected into existing toolbar)
  - self.gl_viewport  — DFFViewport instance (lazy created)
  - self._gl_mode     — bool
  - switch_to_gl() / switch_to_2d()
  - load_dff_in_gl(geometry, materials) / load_txd_in_gl(textures)
"""

## Methods list -
# GLViewportMixin.setup_gl_toggle
# GLViewportMixin.switch_to_gl
# GLViewportMixin.switch_to_2d
# GLViewportMixin._toggle_gl_mode
# GLViewportMixin.load_dff_in_gl
# GLViewportMixin.load_txd_in_gl
# GLViewportMixin._get_gl_viewport


try:
    from apps.components.Model_Viewer.model_viewer import DFFViewport
    GL_AVAILABLE = True
except Exception:
    GL_AVAILABLE = False


class GLViewportMixin:
    """Mixin: adds OpenGL DFFViewport toggle to QPainter-based workshops."""

    def setup_gl_toggle(self, toolbar_layout, icon_color='#ffffff'): #vers 1
        """Call after creating self.preview_widget to inject GL toggle button.
        toolbar_layout: the QHBoxLayout or QToolBar that holds viewport controls.
        """
        self._gl_mode    = False
        self._gl_viewport = None
        self._qp_viewport = getattr(self, 'preview_widget', None)

        if not GL_AVAILABLE:
            return

        from PyQt6.QtWidgets import QPushButton
        from PyQt6.QtCore import Qt

        self._gl_toggle_btn = QPushButton("GL")
        self._gl_toggle_btn.setCheckable(True)
        self._gl_toggle_btn.setChecked(False)
        self._gl_toggle_btn.setFixedSize(32, 26)
        self._gl_toggle_btn.setToolTip("Switch to OpenGL 3D viewport")
        self._gl_toggle_btn.toggled.connect(self._toggle_gl_mode)

        try:
            from apps.methods.imgfactory_svg_icons import SVGIconFactory
            ico = SVGIconFactory.cube_icon(16, icon_color)
            if ico:
                self._gl_toggle_btn.setIcon(ico)
                from PyQt6.QtCore import QSize
                self._gl_toggle_btn.setIconSize(QSize(16, 16))
        except Exception:
            pass

        # Try to add to existing icon button list (model_workshop pattern)
        if hasattr(self, '_mod_icon_buttons'):
            self._mod_icon_buttons.append(self._gl_toggle_btn)
        toolbar_layout.addWidget(self._gl_toggle_btn)

    def _get_gl_viewport(self): #vers 1
        """Lazy-create the DFFViewport."""
        if self._gl_viewport is None and GL_AVAILABLE:
            self._gl_viewport = DFFViewport()
            # Inherit app_settings if available
            app_settings = getattr(self, 'app_settings', None)
            if not app_settings:
                mw = getattr(self, 'main_window', None)
                if mw: app_settings = getattr(mw, 'app_settings', None)
            self._gl_viewport.app_settings = app_settings
        return self._gl_viewport

    def _toggle_gl_mode(self, enabled: bool): #vers 1
        if enabled:
            self.switch_to_gl()
        else:
            self.switch_to_2d()

    def switch_to_gl(self): #vers 1
        """Replace QPainter viewport with OpenGL viewport in the layout."""
        if not GL_AVAILABLE:
            return
        qp = getattr(self, '_qp_viewport', None)
        gl = self._get_gl_viewport()
        if not qp or not gl:
            return
        # Swap widgets in parent layout
        parent = qp.parent()
        layout = qp.parentWidget().layout() if parent else None
        if layout:
            idx = layout.indexOf(qp)
            stretch = layout.stretch(idx) if hasattr(layout, 'stretch') else 1
            qp.hide()
            layout.insertWidget(idx, gl, stretch)
        else:
            qp.hide()
            if hasattr(self, 'preview_row'):
                self.preview_row.addWidget(gl)
        gl.show()
        self._gl_mode = True
        if hasattr(self, '_gl_toggle_btn'):
            self._gl_toggle_btn.setChecked(True)
            self._gl_toggle_btn.setToolTip("Switch to 2D viewport")

    def switch_to_2d(self): #vers 1
        """Restore QPainter viewport."""
        gl = self._gl_viewport
        qp = getattr(self, '_qp_viewport', None)
        if gl:
            gl.hide()
        if qp:
            qp.show()
        self._gl_mode = False
        if hasattr(self, '_gl_toggle_btn'):
            self._gl_toggle_btn.setChecked(False)
            self._gl_toggle_btn.setToolTip("Switch to OpenGL 3D viewport")

    def load_dff_in_gl(self, geometry, materials): #vers 1
        """Load a DFF geometry into the GL viewport (auto-switches if in GL mode)."""
        if not GL_AVAILABLE:
            return
        gl = self._get_gl_viewport()
        if gl:
            gl.load_geometry(geometry, materials)
            if self._gl_mode:
                gl.update()

    def load_txd_in_gl(self, textures: list): #vers 1
        """Upload TXD textures to GL viewport."""
        if not GL_AVAILABLE:
            return
        gl = self._get_gl_viewport()
        if gl:
            gl._upload_textures(textures)
            if self._gl_mode:
                gl.update()
