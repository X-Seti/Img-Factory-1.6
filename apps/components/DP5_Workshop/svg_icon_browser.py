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
        # Restore dock state
        if workshop and hasattr(workshop, 'dp5_settings'):
            if workshop.dp5_settings.get('svg_browser_docked', False):
                QTimer.singleShot(200, self._snap_to_canvas)

    # ── UI ────────────────────────────────────────────────────────────────────

    def _build_ui(self): #vers 1
        root = QVBoxLayout(self)
        root.setContentsMargins(4, 4, 4, 4)
        root.setSpacing(3)

        # Title + D button
        _title_row = QHBoxLayout()
        _title_row.addWidget(QLabel("SVG Icons"))
        self._dock_btn = QPushButton("D")
        self._dock_btn.setFixedSize(22, 22)
        self._dock_btn.setFlat(True)
        self._dock_btn.setCheckable(True)
        self._dock_btn.setToolTip("Snap to left of canvas / float")
        self._dock_btn.clicked.connect(self._toggle_dock)
        _title_row.addWidget(self._dock_btn)
        root.addLayout(_title_row)

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

    def _load_icons(self): #vers 2
        try:
            from apps.methods.imgfactory_svg_icons import SVGIconFactory
            import apps.methods.imgfactory_svg_icons as _mod
            self._factory    = SVGIconFactory
            self._icons_path = os.path.normpath(os.path.abspath(_mod.__file__))
            # Built-in icon methods
            names = sorted([
                n for n in dir(SVGIconFactory)
                if n.endswith('_icon') and not n.startswith('__')
            ])
            # Also include any overrides in apps/icons/ not already in factory
            icons_dir = self._get_icons_dir()
            self._file_overrides = set()   # names that have a file override
            if icons_dir and os.path.isdir(icons_dir):
                for fname in sorted(os.listdir(icons_dir)):
                    base, ext = os.path.splitext(fname)
                    if ext.lower() in ('.svg', '.png'):
                        # Mark as having a file override
                        name = base + '_icon'
                        self._file_overrides.add(name)
                        # Add to list if not already a factory method
                        if name not in names:
                            names.append(name + '__file')  # file-only
            else:
                self._file_overrides = set()
            self._icon_names = sorted(names)
            self._populate_list(self._icon_names)
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

    def _populate_list(self, names): #vers 2
        self._list.clear()
        color = self._icon_color()
        icons_dir = self._get_icons_dir()
        overrides = getattr(self, '_file_overrides', set())
        for name in names:
            try:
                # File-only entries (not in factory)
                if name.endswith('__file'):
                    real_name = name[:-6]  # strip __file
                    label = real_name.replace('_icon','').replace('_',' ') + ' [F]'
                    icon  = SVGIconFactory._load_from_file(
                        real_name.replace('_icon',''), 32, color) or QIcon()
                    item  = QListWidgetItem(icon, label)
                    item.setData(Qt.ItemDataRole.UserRole, real_name)
                    item.setData(Qt.ItemDataRole.UserRole + 1, 'file_only')
                    item.setToolTip(f"{real_name}\nFile override in apps/icons/")
                    item.setSizeHint(QSize(78, 56))
                    self._list.addItem(item)
                    continue
                # Factory icon — check if file override exists
                has_override = name in overrides
                base = name.replace('_icon', '')
                file_icon = SVGIconFactory._load_from_file(base, 32, color) if has_override else None
                icon = file_icon if file_icon else getattr(self._factory, name)(32, color)
                label = base.replace('_', ' ')
                if has_override:
                    label += ' ★'   # mark overridden icons
                item = QListWidgetItem(icon, label)
                item.setData(Qt.ItemDataRole.UserRole, name)
                item.setData(Qt.ItemDataRole.UserRole + 1,
                             'override' if has_override else 'factory')
                tip = f"{name}"
                if has_override:
                    tip += f"\n★ File override in apps/icons/{base}.*"
                tip += "\nDouble-click to open in canvas\nRight-click to replace with file"
                item.setToolTip(tip)
                item.setSizeHint(QSize(78, 56))
                self._list.addItem(item)
            except Exception:
                pass
        self._count_lbl.setText(f"{self._list.count()} icons")

        # Enable right-click context menu on the list
        self._list.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        if not hasattr(self, '_ctx_connected') or not self._ctx_connected:
            self._list.customContextMenuRequested.connect(self._icon_context_menu)
            self._ctx_connected = True

    def _icon_color(self): #vers 1
        try:
            if self.app_settings:
                tc = self.app_settings.get_theme_colors() or {}
                return tc.get('text_primary', '#aaaaaa')
        except Exception:
            pass
        return self.palette().color(self.palette().ColorRole.WindowText).name()

    # ── Selection ─────────────────────────────────────────────────────────────

    def _icon_context_menu(self, pos): #vers 1
        """Right-click: replace icon with file, remove override, open in canvas."""
        from PyQt6.QtWidgets import QMenu
        item = self._list.itemAt(pos)
        if not item:
            return
        name   = item.data(Qt.ItemDataRole.UserRole)
        kind   = item.data(Qt.ItemDataRole.UserRole + 1)
        base   = name.replace('_icon', '')
        icons_dir = self._get_icons_dir()

        menu = QMenu(self)
        menu.addAction("Open in Canvas", self._load_into_canvas)

        act_replace = menu.addAction("Replace with PNG / SVG file…")
        act_replace.triggered.connect(lambda: self._replace_with_file(name, base, icons_dir))

        if kind in ('override', 'file_only'):
            act_remove = menu.addAction("Remove file override (restore built-in)")
            act_remove.triggered.connect(lambda: self._remove_override(base, icons_dir))

        menu.addSeparator()
        menu.addAction("Export .svg", self._export_svg)
        menu.addAction("Export as Method .py", self._export_as_method)
        menu.exec(self._list.mapToGlobal(pos))

    def _replace_with_file(self, name, base, icons_dir): #vers 1
        """Copy a PNG or SVG file into apps/icons/ as the override for this icon."""
        if not icons_dir:
            self._status_lbl.setText("apps/icons/ not found"); return
        path, _ = QFileDialog.getOpenFileName(
            self, f"Replace {name} with file",
            "", "Images (*.png *.svg *.bmp);;All Files (*)")
        if not path:
            return
        import shutil
        ext = os.path.splitext(path)[1].lower()
        dst = os.path.join(icons_dir, f"{base}{ext}")
        try:
            os.makedirs(icons_dir, exist_ok=True)
            shutil.copy2(path, dst)
            self._status_lbl.setText(f"✓ Saved to apps/icons/{base}{ext}")
            QTimer.singleShot(100, self._reload_icons)  # refresh list
        except Exception as e:
            self._status_lbl.setText(f"Error: {e}")

    def _remove_override(self, base, icons_dir): #vers 1
        """Delete the file override from apps/icons/, restoring built-in icon."""
        if not icons_dir:
            return
        removed = False
        for ext in ('.png', '.svg'):
            fpath = os.path.join(icons_dir, f"{base}{ext}")
            if os.path.isfile(fpath):
                try:
                    os.remove(fpath)
                    removed = True
                except Exception as e:
                    self._status_lbl.setText(f"Error: {e}"); return
        if removed:
            self._status_lbl.setText(f"✓ Removed override, restored built-in")
            QTimer.singleShot(100, self._reload_icons)
        else:
            self._status_lbl.setText("No override file found")

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

    def _build_replacement_method(self, name): #vers 3
        """
        Build replacement method.
        If canvas was edited:
          - Save canvas as PNG to apps/icons/{name}.png
          - SVGIconFactory._load_from_file() will pick it up automatically
          - Return original method source (no Python source change needed)
        If not edited: return original source unchanged.
        """
        if not self._loaded_into_canvas or not self.workshop:
            return self._get_full_method_source(name)

        ws = self.workshop
        if not hasattr(ws, 'dp5_canvas') or not ws.dp5_canvas:
            return self._get_full_method_source(name)

        try:
            from PyQt6.QtGui import QImage
            canvas  = ws.dp5_canvas
            w, h    = canvas.tex_w, canvas.tex_h
            rgba    = bytes(canvas.rgba)
            img     = QImage(rgba, w, h, w * 4, QImage.Format.Format_RGBA8888)
            icons_dir = self._get_icons_dir()
            if icons_dir:
                os.makedirs(icons_dir, exist_ok=True)
                # Save as SVG name (strip _icon suffix for file name)
                base = name.replace('_icon', '')
                png_path = os.path.join(icons_dir, f'{base}.png')
                img.save(png_path, 'PNG')
                self._status_lbl.setText(
                    f"Canvas saved to apps/icons/{base}.png\n"
                    f"Click ↺ Reload to apply — no Python edit needed")
            # Return original source — file override handles the rest
            return self._get_full_method_source(name)
        except Exception as e:
            print(f"[_build_replacement_method] {e}")
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

    def _update_in_file(self): #vers 2
        """Open built-in find/replace dialog — OS-agnostic."""
        if not self._current_name or not self._icons_path:
            return
        name       = self._current_name
        method_src = self._build_replacement_method(name)
        if not method_src:
            QMessageBox.warning(self, "Error", "Could not build method source.")
            return
        dlg = self._MethodReplaceDialog(
            self, name, self._icons_path, method_src)
        if dlg._result:
            self._reload_icons()
            self._status_lbl.setText("✓ Saved and reloaded")
            QTimer.singleShot(2000, lambda: self._status_lbl.setText(""))
            # Open in editor for verification if available
            editor = self._find_editor()
            if editor != 'xdg-open':
                line_no = self._find_method_line(name)
                try:
                    args = {
                        'kate':   ['kate',  '--line', str(line_no), self._icons_path],
                        'kwrite': ['kwrite','--line', str(line_no), self._icons_path],
                        'gedit':  ['gedit', f'+{line_no}', self._icons_path],
                        'code':   ['code',  '--goto', f'{self._icons_path}:{line_no}'],
                        'xed':    ['xed',   f'+{line_no}', self._icons_path],
                    }.get(editor)
                    if args:
                        subprocess.Popen(args)
                except Exception:
                    pass

    def _find_editor(self): #vers 1
        """Find the best available editor (for post-save viewing)."""
        for ed in ('kate', 'kwrite', 'gedit', 'xed', 'code'):
            try:
                r = subprocess.run(['which', ed], capture_output=True)
                if r.returncode == 0:
                    return ed
            except Exception:
                pass
        return 'xdg-open'

    # ── Export ────────────────────────────────────────────────────────────────

    def _get_icons_dir(self): #vers 1
        """Return apps/icons/ path."""
        try:
            from apps.methods.imgfactory_svg_icons import SVGIconFactory
            d = SVGIconFactory._get_icons_dir()
            if d:
                return d
        except Exception:
            pass
        # Fallback: relative to this file
        import os
        here = os.path.dirname(os.path.abspath(__file__))
        for _ in range(4):
            c = os.path.join(here, 'apps', 'icons')
            if os.path.isdir(c):
                return c
            c2 = os.path.join(here, 'icons')
            if os.path.isdir(c2):
                return c2
            here = os.path.dirname(here)
        return ''

    def _export_svg(self): #vers 2
        """Export SVG — defaults to apps/icons/ for instant override effect."""
        if not self._current_name:
            return
        svg = self._get_svg_source(self._current_name)
        w, h = self._get_svg_size(svg)
        if not svg.strip().startswith('<svg'):
            svg = (f'<svg viewBox="0 0 {w} {h}" '
                   f'xmlns="http://www.w3.org/2000/svg">\n{svg}\n</svg>')
        icons_dir = self._get_icons_dir()
        default = os.path.join(
            icons_dir,
            self._current_name.replace('_icon', '') + '.svg') if icons_dir else \
            self._current_name.replace('_icon', '') + '.svg'
        path, _ = QFileDialog.getSaveFileName(
            self, "Export SVG — Save to apps/icons/ to override built-in",
            default, "SVG Files (*.svg)")
        if not path:
            return
        try:
            with open(path, 'w') as f:
                f.write(svg)
            self._status_lbl.setText(f"Saved: {os.path.basename(path)}")
            QTimer.singleShot(2000, lambda: self._status_lbl.setText(""))
            # If saved into icons_dir — it will be picked up immediately on reload
            if icons_dir and os.path.dirname(os.path.abspath(path)) == \
                    os.path.abspath(icons_dir):
                self._status_lbl.setText(
                    "✓ Saved to apps/icons/ — click ↺ Reload to apply")
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

    # ── Built-in find/replace dialog ─────────────────────────────────────────

    class _MethodReplaceDialog: #vers 1
        """
        OS-agnostic method replacement dialog.
        Shows the old method source, lets user confirm, then replaces and saves.
        """
        def __init__(self, browser, name, icons_path, new_method):
            from PyQt6.QtWidgets import (
                QDialog, QVBoxLayout, QHBoxLayout, QLabel,
                QTextEdit, QPushButton, QSplitter, QWidget, QFrame
            )
            from PyQt6.QtGui import QFont, QColor, QTextCharFormat, QTextCursor
            from PyQt6.QtCore import Qt, QSize

            self._browser    = browser
            self._name       = name
            self._icons_path = icons_path
            self._new_method = new_method
            self._saved      = False

            dlg = QDialog(browser)
            dlg.setWindowTitle(f"Replace Method — {name}")
            dlg.resize(820, 600)
            self._dlg = dlg

            root = QVBoxLayout(dlg)
            root.setSpacing(6)

            # Header
            hdr = QLabel(
                f"<b>Replacing:</b> <code>{name}</code>  in  "
                f"<code>{icons_path}</code>")
            hdr.setWordWrap(True)
            root.addWidget(hdr)

            splitter = QSplitter(Qt.Orientation.Horizontal)

            # Left — old method (read-only, highlighted)
            left = QWidget()
            ll = QVBoxLayout(left); ll.setContentsMargins(0,0,0,0)
            ll.addWidget(QLabel("Current method in file:"))
            self._old_edit = QTextEdit()
            self._old_edit.setReadOnly(True)
            self._old_edit.setFont(QFont("Courier New", 9))
            ll.addWidget(self._old_edit, 1)
            splitter.addWidget(left)

            # Right — new method (editable)
            right = QWidget()
            rl = QVBoxLayout(right); rl.setContentsMargins(0,0,0,0)
            rl.addWidget(QLabel("New method (edit if needed):"))
            self._new_edit = QTextEdit()
            self._new_edit.setFont(QFont("Courier New", 9))
            self._new_edit.setPlainText(new_method)
            rl.addWidget(self._new_edit, 1)
            splitter.addWidget(right)

            splitter.setStretchFactor(0, 1)
            splitter.setStretchFactor(1, 1)
            root.addWidget(splitter, 1)

            # Status
            self._status = QLabel("")
            self._status.setFont(QFont("Arial", 8))
            root.addWidget(self._status)

            # Buttons
            btn_row = QHBoxLayout()

            find_btn = QPushButton("Find in File")
            find_btn.setToolTip("Locate the method in imgfactory_svg_icons.py")
            find_btn.clicked.connect(self._find)

            self._replace_btn = QPushButton("Replace && Save")
            self._replace_btn.setEnabled(False)
            self._replace_btn.setToolTip("Replace old method with new and save file")
            self._replace_btn.clicked.connect(self._replace_and_save)
            f = self._replace_btn.font(); f.setBold(True)
            self._replace_btn.setFont(f)

            cancel_btn = QPushButton("Cancel")
            cancel_btn.clicked.connect(dlg.reject)

            btn_row.addWidget(find_btn)
            btn_row.addStretch()
            btn_row.addWidget(self._replace_btn)
            btn_row.addWidget(cancel_btn)
            root.addLayout(btn_row)

            # Auto-find on open
            from PyQt6.QtCore import QTimer
            QTimer.singleShot(100, self._find)

            dlg.exec()
            self._result = self._saved

        def _find(self):
            """Read current method from file and show it."""
            try:
                import inspect, re
                factory = self._browser._factory
                if factory is None:
                    self._status.setText("Factory not loaded")
                    return
                fn = getattr(factory, self._name, None)
                if fn is None:
                    self._status.setText(f"Method {self._name} not found")
                    return
                old_src = inspect.getsource(fn)
                self._old_edit.setPlainText(old_src)
                self._status.setText(
                    f"Found — {len(old_src.splitlines())} lines. "
                    f"Review, then click Replace && Save.")
                self._replace_btn.setEnabled(True)
                # Highlight differences
                self._highlight_diff(old_src, self._new_method)
            except Exception as e:
                self._status.setText(f"Find error: {e}")

        def _highlight_diff(self, old, new):
            """Simple highlight — mark lines that differ."""
            from PyQt6.QtGui import QTextCharFormat, QColor, QTextCursor
            old_lines = old.splitlines()
            new_lines = new.splitlines()
            cursor = self._new_edit.textCursor()
            cursor.select(cursor.SelectionType.Document)
            fmt = QTextCharFormat()
            cursor.setCharFormat(fmt)
            # Re-set plain text then highlight changed lines
            self._new_edit.setPlainText(new)

        def _replace_and_save(self):
            """Replace old method in file with edited new method and save."""
            try:
                import inspect, ast as _ast, re
                factory   = self._browser._factory
                fn        = getattr(factory, self._name, None)
                if fn is None:
                    self._status.setText("Method not found in factory")
                    return
                old_src  = inspect.getsource(fn)
                new_src  = self._new_edit.toPlainText()

                with open(self._icons_path, 'r') as f:
                    file_src = f.read()

                if old_src not in file_src:
                    self._status.setText(
                        "Could not locate method in file — source may have changed.")
                    return

                new_file = file_src.replace(old_src, new_src, 1)

                # Syntax check before saving
                try:
                    _ast.parse(new_file)
                except SyntaxError as e:
                    self._status.setText(f"Syntax error — not saved: {e}")
                    return

                with open(self._icons_path, 'w') as f:
                    f.write(new_file)

                self._saved = True
                self._status.setText("✓ Saved successfully")
                self._replace_btn.setEnabled(False)

                from PyQt6.QtCore import QTimer
                QTimer.singleShot(800, self._dlg.accept)

            except Exception as e:
                self._status.setText(f"Save error: {e}")


    # ── Dock ──────────────────────────────────────────────────────────────────

    def _toggle_dock(self): #vers 1
        if self._dock_btn.isChecked():
            self._snap_to_canvas()
        else:
            self._float_panel()
        ws = self.workshop
        if ws and hasattr(ws, 'dp5_settings'):
            ws.dp5_settings.set('svg_browser_docked', self._dock_btn.isChecked())
            ws.dp5_settings.save()

    def _snap_to_canvas(self): #vers 2
        """Snap to left edge of canvas viewport."""
        ws = self.workshop
        if not ws or not hasattr(ws, '_canvas_scroll'):
            return
        vp = ws._canvas_scroll.viewport()
        self.setWindowFlags(Qt.WindowType.Widget)
        self.setParent(vp)
        self.setWindowOpacity(0.88)
        self._dock_btn.setChecked(True)
        from PyQt6.QtCore import QTimer
        QTimer.singleShot(0, self._reposition_docked)
        self.show(); self.raise_()

    def _reposition_docked(self): #vers 1
        ws = self.workshop
        if not ws or not hasattr(ws, '_canvas_scroll'):
            return
        vp = ws._canvas_scroll.viewport()
        self.move(4, 4)
        self.resize(220, vp.height() - 8)
        self.raise_()

    def _float_panel(self): #vers 2
        """Return to floating Tool window."""
        try:
            pos = self.mapToGlobal(self.rect().topLeft())
        except Exception:
            pos = None
        self.setWindowOpacity(1.0)
        self.setParent(None)
        self.setWindowFlags(Qt.WindowType.Tool |
                            Qt.WindowType.WindowStaysOnTopHint)
        if pos:
            self.move(pos)
        self.resize(220, 520)
        self.show()
        self._dock_btn.setChecked(False)

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
