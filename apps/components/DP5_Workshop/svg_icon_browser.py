#!/usr/bin/env python3
#this belongs in apps/components/DP5_Workshop/svg_icon_browser.py - Version: 4
# X-Seti - April26 2026 - IMG Factory 1.6 - SVG Icon Browser Panel
"""
SVG Icon Browser — floating panel that lists all SVGIconFactory icons.
Click an icon to open it in the DP5 canvas at correct size.
Save as .svg, as Python method, or back into imgfactory_svg_icons.py.
"""

##Methods list -
# SVGIconBrowser.__init__
# SVGIconBrowser._build_ui
# SVGIconBrowser._load_icons
# SVGIconBrowser._populate_list
# SVGIconBrowser._icon_color
# SVGIconBrowser._apply_filter
# SVGIconBrowser._on_icon_selected
# SVGIconBrowser._get_svg_source
# SVGIconBrowser._get_svg_size
# SVGIconBrowser._get_full_method_source
# SVGIconBrowser._load_into_canvas
# SVGIconBrowser._export_svg
# SVGIconBrowser._export_as_method
# SVGIconBrowser._save_to_file
# SVGIconBrowser._inject_svg
# SVGIconBrowser._apply_theme

import re
import os
import inspect
from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QLineEdit, QListWidget,
    QListWidgetItem, QLabel, QPushButton, QFileDialog,
    QMessageBox, QApplication, QFrame, QSizePolicy
)
from PyQt6.QtCore import Qt, QSize, QTimer
from PyQt6.QtGui import QFont, QIcon, QColor, QPixmap, QImage


class SVGIconBrowser(QWidget):
    """
    Floating panel — lists SVGIconFactory icons on the left.
    Click any icon to load it into the DP5 canvas for editing.
    Save buttons at bottom: .svg / method .py / back to imgfactory_svg_icons.py.
    """

    def __init__(self, workshop=None, parent=None):
        super().__init__(parent, Qt.WindowType.Tool |
                         Qt.WindowType.WindowStaysOnTopHint)
        self.workshop      = workshop
        self.app_settings  = getattr(workshop, 'app_settings', None) if workshop else None
        self._factory      = None
        self._icon_names   = []
        self._current_name = None
        self._icons_path   = None
        self.setWindowTitle("SVG Icons")
        self.resize(220, 520)
        self._build_ui()
        self._load_icons()
        QTimer.singleShot(0, self._apply_theme)

    # ── UI ────────────────────────────────────────────────────────────────────

    def _build_ui(self): #vers 1
        root = QVBoxLayout(self)
        root.setContentsMargins(4, 4, 4, 4)
        root.setSpacing(3)

        # Search
        self._search = QLineEdit()
        self._search.setPlaceholderText("Filter icons…")
        self._search.textChanged.connect(self._apply_filter)
        root.addWidget(self._search)

        self._count_lbl = QLabel("0 icons")
        self._count_lbl.setFont(QFont("Arial", 7))
        root.addWidget(self._count_lbl)

        # Icon list — icon + name, single column
        self._list = QListWidget()
        self._list.setViewMode(QListWidget.ViewMode.IconMode)
        self._list.setIconSize(QSize(32, 32))
        self._list.setGridSize(QSize(80, 58))
        self._list.setResizeMode(QListWidget.ResizeMode.Adjust)
        self._list.setMovement(QListWidget.Movement.Static)
        self._list.setWordWrap(True)
        self._list.setUniformItemSizes(True)
        self._list.itemDoubleClicked.connect(self._load_into_canvas)
        self._list.itemSelectionChanged.connect(self._on_icon_selected)
        root.addWidget(self._list, 1)

        # Current selection label
        self._name_lbl = QLabel("")
        self._name_lbl.setFont(QFont("Arial", 8))
        self._name_lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._name_lbl.setWordWrap(True)
        root.addWidget(self._name_lbl)

        # Open in canvas button
        self._open_btn = QPushButton("Open in Canvas")
        self._open_btn.setEnabled(False)
        self._open_btn.clicked.connect(self._load_into_canvas)
        root.addWidget(self._open_btn)

        # Divider
        line = QFrame()
        line.setFrameShape(QFrame.Shape.HLine)
        root.addWidget(line)

        # Save buttons
        self._save_svg_btn = QPushButton("Export .svg")
        self._save_svg_btn.setEnabled(False)
        self._save_svg_btn.clicked.connect(self._export_svg)
        root.addWidget(self._save_svg_btn)

        self._save_method_btn = QPushButton("Export as Method")
        self._save_method_btn.setEnabled(False)
        self._save_method_btn.clicked.connect(self._export_as_method)
        root.addWidget(self._save_method_btn)

        self._save_file_btn = QPushButton("Save to imgfactory_svg_icons.py")
        self._save_file_btn.setEnabled(False)
        self._save_file_btn.setToolTip(
            "Write the current canvas back into imgfactory_svg_icons.py\n"
            "as the SVG source for the selected icon")
        self._save_file_btn.clicked.connect(self._save_to_file)
        root.addWidget(self._save_file_btn)

    # ── Icon loading ──────────────────────────────────────────────────────────

    def _load_icons(self): #vers 1
        try:
            from apps.methods.imgfactory_svg_icons import SVGIconFactory
            import apps.methods.imgfactory_svg_icons as _mod
            self._factory    = SVGIconFactory
            self._icons_path = os.path.normpath(os.path.abspath(_mod.__file__))
            names = sorted([
                n for n in dir(SVGIconFactory)
                if n.endswith('_icon') and not n.startswith('__')
            ])
            self._icon_names = names
            self._populate_list(names)
        except Exception as e:
            self._count_lbl.setText(f"Load error: {e}")

    def _populate_list(self, names): #vers 1
        self._list.clear()
        color = self._icon_color()
        for name in names:
            try:
                icon  = getattr(self._factory, name)(32, color)
                label = name.replace('_icon', '').replace('_', ' ')
                item  = QListWidgetItem(icon, label)
                item.setData(Qt.ItemDataRole.UserRole, name)
                item.setToolTip(f"{name}\nDouble-click to open in canvas")
                item.setSizeHint(QSize(78, 56))
                self._list.addItem(item)
            except Exception:
                pass
        self._count_lbl.setText(f"{self._list.count()} icons")

    def _icon_color(self): #vers 1
        try:
            if self.app_settings:
                tc = self.app_settings.get_theme_colors() or {}
                return tc.get('text_primary', '#aaaaaa')
        except Exception:
            pass
        return self.palette().color(self.palette().ColorRole.WindowText).name()

    # ── Selection ─────────────────────────────────────────────────────────────

    def _apply_filter(self, text): #vers 1
        text = text.lower().strip()
        filtered = self._icon_names if not text else [
            n for n in self._icon_names if text in n]
        self._populate_list(filtered)

    def _on_icon_selected(self): #vers 1
        items = self._list.selectedItems()
        if not items:
            return
        name = items[0].data(Qt.ItemDataRole.UserRole)
        self._current_name = name
        label = name.replace('_icon', '').replace('_', ' ').title()
        self._name_lbl.setText(label)
        for btn in [self._open_btn, self._save_svg_btn,
                    self._save_method_btn, self._save_file_btn]:
            btn.setEnabled(True)

    # ── Canvas integration ────────────────────────────────────────────────────

    def _get_svg_source(self, name): #vers 1
        """Extract raw SVG string from method source."""
        try:
            src = inspect.getsource(getattr(self._factory, name))
            for pat in [r"svg_data\s*=\s*'''(.*?)'''",
                        r'svg_data\s*=\s*"""(.*?)"""']:
                m = re.search(pat, src, re.DOTALL)
                if m:
                    return m.group(1).strip()
            return src
        except Exception:
            return ''

    def _get_svg_size(self, svg_src: str): #vers 1
        """Extract width/height from SVG viewBox or width/height attrs."""
        m = re.search(r'viewBox=["\'][\d.]+\s+[\d.]+\s+([\d.]+)\s+([\d.]+)["\']',
                      svg_src)
        if m:
            return int(float(m.group(1))), int(float(m.group(2)))
        m = re.search(r'width=["\'](\d+)["\'].*?height=["\'](\d+)["\']', svg_src)
        if m:
            return int(m.group(1)), int(m.group(2))
        return 32, 32  # default icon size

    def _get_full_method_source(self, name): #vers 1
        try:
            return inspect.getsource(getattr(self._factory, name))
        except Exception as e:
            return f"# Error: {e}"

    def _load_into_canvas(self, *_): #vers 1
        """Render selected SVG icon into the DP5 canvas."""
        if not self._current_name or not self.workshop:
            return
        ws = self.workshop
        if not hasattr(ws, 'dp5_canvas') or not ws.dp5_canvas:
            QMessageBox.warning(self, "No Canvas",
                "DP5 canvas not ready.")
            return

        svg_src = self._get_svg_source(self._current_name)
        if not svg_src:
            QMessageBox.warning(self, "No SVG Source",
                f"Could not extract SVG from {self._current_name}")
            return

        # Build full SVG if needed
        if not svg_src.strip().startswith('<svg'):
            svg_src = (
                f'<svg viewBox="0 0 32 32" '
                f'xmlns="http://www.w3.org/2000/svg">\n{svg_src}\n</svg>')

        w, h = self._get_svg_size(svg_src)
        # Render at 2× for quality, then store at base size
        scale = max(1, 64 // max(w, h))  # scale up small icons
        rw, rh = w * scale, h * scale

        try:
            from PyQt6.QtSvg import QSvgRenderer
            from PyQt6.QtGui import QPainter
            renderer = QSvgRenderer(svg_src.encode())
            pm = QPixmap(rw, rh)
            pm.fill(Qt.GlobalColor.transparent)
            p = QPainter(pm)
            renderer.render(p)
            p.end()

            # Convert to RGBA
            img = pm.toImage().convertToFormat(
                QImage.Format.Format_RGBA8888)
            rgba = bytearray(img.bits().asarray(rw * rh * 4))

            # Load into canvas
            ws._push_undo() if hasattr(ws, '_push_undo') else None
            ws.dp5_canvas.tex_w = rw
            ws.dp5_canvas.tex_h = rh
            ws.dp5_canvas.rgba  = rgba
            ws._canvas_width    = rw
            ws._canvas_height   = rh
            ws.dp5_canvas.update()
            if hasattr(ws, '_fit_canvas_to_viewport'):
                ws._fit_canvas_to_viewport()
            if hasattr(ws, '_set_status'):
                ws._set_status(
                    f"Loaded: {self._current_name} ({rw}×{rh})")
            self._save_file_btn.setEnabled(True)

        except Exception as e:
            QMessageBox.warning(self, "Render Error", str(e))

    # ── Save / Export ─────────────────────────────────────────────────────────

    def _get_current_svg(self) -> str:
        """Get SVG from canvas if modified, else from source."""
        # Try to get from canvas — render back to SVG not possible directly,
        # so we always use the stored source. User edits via DP5 tools.
        return self._get_svg_source(self._current_name)

    def _export_svg(self): #vers 1
        if not self._current_name:
            return
        svg = self._get_current_svg()
        if not svg.strip().startswith('<svg'):
            w, h = self._get_svg_size(svg)
            svg = (f'<svg viewBox="0 0 {w} {h}" '
                   f'xmlns="http://www.w3.org/2000/svg">\n{svg}\n</svg>')
        default = self._current_name.replace('_icon', '') + '.svg'
        path, _ = QFileDialog.getSaveFileName(
            self, "Export SVG", default, "SVG Files (*.svg)")
        if not path:
            return
        try:
            with open(path, 'w') as f:
                f.write(svg)
            QMessageBox.information(self, "Exported", f"Saved:\n{path}")
        except Exception as e:
            QMessageBox.warning(self, "Export Error", str(e))

    def _export_as_method(self): #vers 1
        if not self._current_name:
            return
        path, _ = QFileDialog.getSaveFileName(
            self, "Export Method",
            self._current_name + '.py', "Python Files (*.py)")
        if not path:
            return
        try:
            method_src = self._get_full_method_source(self._current_name)
            header = (
                f"#!/usr/bin/env python3\n"
                f"# Exported from SVG Icon Browser — IMG Factory\n"
                f"# Icon: {self._current_name}\n"
                f"# Add inside class SVGIconFactory in "
                f"apps/methods/imgfactory_svg_icons.py\n\n"
                f"from PyQt6.QtCore import QSize, QByteArray\n"
                f"from PyQt6.QtGui import QIcon, QPixmap, QPainter\n"
                f"from PyQt6.QtSvg import QSvgRenderer\n\n\n"
            )
            with open(path, 'w') as f:
                f.write(header + method_src)
            QMessageBox.information(self, "Exported", f"Method saved:\n{path}")
        except Exception as e:
            QMessageBox.warning(self, "Export Error", str(e))

    def _save_to_file(self): #vers 1
        """
        Save current canvas back into imgfactory_svg_icons.py.
        Converts canvas RGBA → SVG path data is not possible directly,
        so instead we save the stored SVG source (as edited by the user
        via the source editor or loaded from file).
        """
        if not self._current_name or not self._icons_path:
            return
        # For now — inform user to use Export .svg and manually update
        # A future version could embed raster as base64 in the method
        QMessageBox.information(self, "Save to File",
            f"To update '{self._current_name}' in imgfactory_svg_icons.py:\n\n"
            f"1. Use 'Export .svg' to save the current SVG\n"
            f"2. Edit the svg_data string in the method\n"
            f"3. Or use 'Export as Method' to get the full Python method\n\n"
            f"Source file:\n{self._icons_path}")

    def _inject_svg(self, method_src: str, new_svg: str) -> str: #vers 1
        for pat in [r"(svg_data\s*=\s*''')(.*?)(''')",
                    r'(svg_data\s*=\s*""")(.*?)(""")'  ]:
            m = re.search(pat, method_src, re.DOTALL)
            if m:
                return (method_src[:m.start(2)]
                        + '\n        ' + new_svg + '\n    '
                        + method_src[m.end(2):])
        return method_src

    # ── Theme ─────────────────────────────────────────────────────────────────

    def _apply_theme(self): #vers 1
        pal  = self.palette()
        bg   = pal.color(pal.ColorRole.Window).name()
        fg   = pal.color(pal.ColorRole.WindowText).name()
        mid  = pal.color(pal.ColorRole.Base).name()
        acc  = pal.color(pal.ColorRole.Highlight).name()
        try:
            if self.app_settings:
                tc  = self.app_settings.get_theme_colors() or {}
                bg  = tc.get('bg_primary',    bg)
                fg  = tc.get('text_primary',   fg)
                mid = tc.get('bg_secondary',   mid)
                acc = tc.get('accent_primary', acc)
        except Exception:
            pass
        self.setStyleSheet(f"""
            QWidget     {{ background:{bg}; color:{fg}; }}
            QListWidget {{ background:{mid}; border:1px solid {acc}; }}
            QListWidget::item:selected {{ background:{acc}; color:{fg}; }}
            QLineEdit   {{ background:{mid}; color:{fg};
                           border:1px solid {acc}; padding:2px; }}
            QPushButton {{ background:{mid}; color:{fg};
                           border:1px solid {acc};
                           padding:4px 8px; border-radius:3px; }}
            QPushButton:hover    {{ background:{acc}; }}
            QPushButton:disabled {{ opacity:0.4; }}
            QLabel      {{ color:{fg}; background:transparent; }}
        """)
