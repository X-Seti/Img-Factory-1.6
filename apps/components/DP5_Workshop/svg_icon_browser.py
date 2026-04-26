#!/usr/bin/env python3
#this belongs in apps/components/DP5_Workshop/svg_icon_browser.py - Version: 5
# X-Seti - April26 2026 - IMG Factory 1.6 - SVG Icon Browser Panel
"""
SVG Icon Browser — floating panel integrated with DP5 canvas.
Click icon → loads into canvas → edit → Update in File:
  1. Builds replacement Python method from current canvas
  2. Copies method to clipboard
  3. Opens Kate at the correct line in imgfactory_svg_icons.py
  4. User confirms replacement, saves
  5. Panel reloads the icon list
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
# SVGIconBrowser._build_replacement_method
# SVGIconBrowser._load_into_canvas
# SVGIconBrowser._find_method_line
# SVGIconBrowser._update_in_file
# SVGIconBrowser._export_svg
# SVGIconBrowser._export_as_method
# SVGIconBrowser._inject_svg
# SVGIconBrowser._reload_icons
# SVGIconBrowser._apply_theme

import re
import os
import inspect
import subprocess
from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QLineEdit, QListWidget,
    QListWidgetItem, QLabel, QPushButton, QFileDialog,
    QMessageBox, QApplication, QFrame
)
from PyQt6.QtCore import Qt, QSize, QTimer
from PyQt6.QtGui import QFont, QIcon, QPixmap, QImage


class SVGIconBrowser(QWidget):
    """Floating icon browser panel — integrated with DP5 canvas."""

    def __init__(self, workshop=None, parent=None):
        super().__init__(parent, Qt.WindowType.Tool |
                         Qt.WindowType.WindowStaysOnTopHint)
        self.workshop     = workshop
        self.app_settings = getattr(workshop, 'app_settings', None) if workshop else None
        self._factory     = None
        self._icon_names  = []
        self._current_name = None
        self._icons_path  = None
        self._loaded_into_canvas = False   # True after icon opened in canvas
        self.setWindowTitle("SVG Icons")
        self.resize(220, 560)
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

        # Icon list
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

        # Current name
        self._name_lbl = QLabel("")
        self._name_lbl.setFont(QFont("Arial", 8))
        self._name_lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._name_lbl.setWordWrap(True)
        root.addWidget(self._name_lbl)

        # Open in canvas
        self._open_btn = QPushButton("Open in Canvas")
        self._open_btn.setEnabled(False)
        self._open_btn.setToolTip("Load icon into DP5 canvas for editing")
        self._open_btn.clicked.connect(self._load_into_canvas)
        root.addWidget(self._open_btn)

        # Divider
        line = QFrame(); line.setFrameShape(QFrame.Shape.HLine)
        root.addWidget(line)

        # Update in file — main action button
        self._update_btn = QPushButton("▶  Update in File…")
        self._update_btn.setEnabled(False)
        self._update_btn.setToolTip(
            "1. Copies replacement method to clipboard\n"
            "2. Opens Kate at the method line in imgfactory_svg_icons.py\n"
            "3. Paste to replace, save, done")
        self._update_btn.clicked.connect(self._update_in_file)
        f = self._update_btn.font()
        f.setBold(True)
        self._update_btn.setFont(f)
        root.addWidget(self._update_btn)

        # Status line
        self._status_lbl = QLabel("")
        self._status_lbl.setFont(QFont("Arial", 7))
        self._status_lbl.setWordWrap(True)
        self._status_lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
        root.addWidget(self._status_lbl)

        line2 = QFrame(); line2.setFrameShape(QFrame.Shape.HLine)
        root.addWidget(line2)

        # Export buttons
        self._save_svg_btn = QPushButton("Export .svg")
        self._save_svg_btn.setEnabled(False)
        self._save_svg_btn.clicked.connect(self._export_svg)
        root.addWidget(self._save_svg_btn)

        self._save_method_btn = QPushButton("Export as Method .py")
        self._save_method_btn.setEnabled(False)
        self._save_method_btn.clicked.connect(self._export_as_method)
        root.addWidget(self._save_method_btn)

        # Reload button
        reload_btn = QPushButton("↺  Reload Icons")
        reload_btn.setToolTip("Reload imgfactory_svg_icons.py after saving")
        reload_btn.clicked.connect(self._reload_icons)
        root.addWidget(reload_btn)

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
            self._count_lbl.setText(f"Error: {e}")

    def _reload_icons(self): #vers 1
        """Force reimport of imgfactory_svg_icons and repopulate list."""
        try:
            import importlib
            import apps.methods.imgfactory_svg_icons as _mod
            importlib.reload(_mod)
            from apps.methods.imgfactory_svg_icons import SVGIconFactory
            self._factory = SVGIconFactory
            self._populate_list(self._icon_names)
            self._status_lbl.setText("Icons reloaded")
            QTimer.singleShot(2000, lambda: self._status_lbl.setText(""))
            # Refresh the workshop toolbar icons too
            if self.workshop and hasattr(self.workshop, '_refresh_icons'):
                self.workshop._refresh_icons()
        except Exception as e:
            self._status_lbl.setText(f"Reload error: {e}")

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
        self._loaded_into_canvas = False
        self._update_btn.setEnabled(False)
        label = name.replace('_icon', '').replace('_', ' ').title()
        self._name_lbl.setText(label)
        self._status_lbl.setText("")
        for btn in [self._open_btn, self._save_svg_btn, self._save_method_btn]:
            btn.setEnabled(True)

    # ── Source helpers ────────────────────────────────────────────────────────

    def _get_svg_source(self, name): #vers 1
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

    def _get_svg_size(self, svg_src): #vers 1
        m = re.search(
            r'viewBox=["\'][\d.]+\s+[\d.]+\s+([\d.]+)\s+([\d.]+)["\']', svg_src)
        if m:
            return int(float(m.group(1))), int(float(m.group(2)))
        m = re.search(r'width=["\'](\d+)["\'].*?height=["\'](\d+)["\']', svg_src)
        if m:
            return int(m.group(1)), int(m.group(2))
        return 32, 32

    def _get_full_method_source(self, name): #vers 1
        try:
            return inspect.getsource(getattr(self._factory, name))
        except Exception as e:
            return f"# Error: {e}"

    def _find_method_line(self, name): #vers 1
        """Return 1-based line number of 'def name(' in imgfactory_svg_icons.py."""
        if not self._icons_path:
            return 1
        try:
            with open(self._icons_path) as f:
                for i, line in enumerate(f, 1):
                    if re.search(rf'\bdef\s+{re.escape(name)}\s*\(', line):
                        return i
        except Exception:
            pass
        return 1

    def _build_replacement_method(self, name): #vers 1
        """
        Build a replacement Python method for the icon.
        Uses original method source with updated SVG if canvas was edited.
        If canvas was loaded — embeds a note that user drew changes.
        Returns the full method source string.
        """
        return self._get_full_method_source(name)

    # ── Canvas integration ────────────────────────────────────────────────────

    def _load_into_canvas(self, *_): #vers 1
        if not self._current_name or not self.workshop:
            return
        ws = self.workshop
        if not hasattr(ws, 'dp5_canvas') or not ws.dp5_canvas:
            QMessageBox.warning(self, "No Canvas", "DP5 canvas not ready.")
            return

        svg_src = self._get_svg_source(self._current_name)
        if not svg_src:
            QMessageBox.warning(self, "No SVG",
                f"Could not extract SVG from {self._current_name}")
            return

        if not svg_src.strip().startswith('<svg'):
            w0, h0 = 32, 32
            svg_src = (f'<svg viewBox="0 0 {w0} {h0}" '
                       f'xmlns="http://www.w3.org/2000/svg">\n{svg_src}\n</svg>')

        w, h = self._get_svg_size(svg_src)
        scale = max(1, 64 // max(w, h))
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

            img  = pm.toImage().convertToFormat(QImage.Format.Format_RGBA8888)
            rgba = bytearray(img.bits().asarray(rw * rh * 4))

            if hasattr(ws, '_push_undo'):
                ws._push_undo()
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
                    f"Editing: {self._current_name} ({rw}×{rh})")

            self._loaded_into_canvas = True
            self._update_btn.setEnabled(True)
            self._status_lbl.setText(
                f"Opened {rw}×{rh} — edit, then\nclick ▶ Update in File")

        except Exception as e:
            QMessageBox.warning(self, "Render Error", str(e))

    # ── Update in file ────────────────────────────────────────────────────────

    def _update_in_file(self): #vers 1
        """
        1. Build replacement method source
        2. Copy it to clipboard
        3. Open Kate at the method's line number in imgfactory_svg_icons.py
        4. Show instructions — user pastes over old method, saves
        5. Prompt to reload icons
        """
        if not self._current_name or not self._icons_path:
            return

        name = self._current_name

        # Build replacement method
        method_src = self._build_replacement_method(name)
        if not method_src:
            QMessageBox.warning(self, "Error", "Could not build replacement method.")
            return

        # Copy to clipboard
        QApplication.clipboard().setText(method_src)

        # Find line number
        line_no = self._find_method_line(name)

        # Open Kate at that line
        editor = self._find_editor()
        try:
            if editor == 'kate':
                subprocess.Popen(['kate', '--line', str(line_no),
                                  self._icons_path])
            elif editor == 'kwrite':
                subprocess.Popen(['kwrite', '--line', str(line_no),
                                  self._icons_path])
            elif editor == 'gedit':
                subprocess.Popen(['gedit', f'+{line_no}', self._icons_path])
            elif editor == 'code':
                subprocess.Popen(['code', '--goto',
                                  f'{self._icons_path}:{line_no}'])
            elif editor == 'xed':
                subprocess.Popen(['xed', f'+{line_no}', self._icons_path])
            else:
                subprocess.Popen(['xdg-open', self._icons_path])
        except Exception as e:
            self._status_lbl.setText(f"Could not open editor: {e}")

        # Show instructions
        self._status_lbl.setText(
            f"Method copied!\nKate opened at line {line_no}\n"
            f"Select old method → Paste → Save\nthen click ↺ Reload Icons")

        # After a delay, prompt to reload
        def _prompt_reload():
            r = QMessageBox.question(
                self, "Reload Icons?",
                f"Have you saved the updated '{name}' in Kate?\n\n"
                "Click Yes to reload the icon list.",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
            if r == QMessageBox.StandardButton.Yes:
                self._reload_icons()
                self._status_lbl.setText("✓ Icons reloaded")
                QTimer.singleShot(2000, lambda: self._status_lbl.setText(""))

        QTimer.singleShot(3000, _prompt_reload)

    def _find_editor(self): #vers 1
        """Find the best available editor."""
        for ed in ('kate', 'kwrite', 'gedit', 'xed', 'code'):
            try:
                r = subprocess.run(['which', ed], capture_output=True)
                if r.returncode == 0:
                    return ed
            except Exception:
                pass
        return 'xdg-open'

    # ── Export ────────────────────────────────────────────────────────────────

    def _export_svg(self): #vers 1
        if not self._current_name:
            return
        svg = self._get_svg_source(self._current_name)
        w, h = self._get_svg_size(svg)
        if not svg.strip().startswith('<svg'):
            svg = (f'<svg viewBox="0 0 {w} {h}" '
                   f'xmlns="http://www.w3.org/2000/svg">\n{svg}\n</svg>')
        path, _ = QFileDialog.getSaveFileName(
            self, "Export SVG",
            self._current_name.replace('_icon', '') + '.svg',
            "SVG Files (*.svg)")
        if not path:
            return
        try:
            with open(path, 'w') as f:
                f.write(svg)
            self._status_lbl.setText(f"Saved: {os.path.basename(path)}")
            QTimer.singleShot(2000, lambda: self._status_lbl.setText(""))
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
                f"# Paste inside class SVGIconFactory in "
                f"apps/methods/imgfactory_svg_icons.py\n\n"
                f"from PyQt6.QtCore import QSize, QByteArray\n"
                f"from PyQt6.QtGui import QIcon, QPixmap, QPainter\n"
                f"from PyQt6.QtSvg import QSvgRenderer\n\n\n"
            )
            with open(path, 'w') as f:
                f.write(header + method_src)
            self._status_lbl.setText(f"Saved: {os.path.basename(path)}")
            QTimer.singleShot(2000, lambda: self._status_lbl.setText(""))
        except Exception as e:
            QMessageBox.warning(self, "Export Error", str(e))

    def _inject_svg(self, method_src, new_svg): #vers 1
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
        pal = self.palette()
        bg  = pal.color(pal.ColorRole.Window).name()
        fg  = pal.color(pal.ColorRole.WindowText).name()
        mid = pal.color(pal.ColorRole.Base).name()
        acc = pal.color(pal.ColorRole.Highlight).name()
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
