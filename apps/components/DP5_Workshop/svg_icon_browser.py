#!/usr/bin/env python3
#this belongs in apps/components/DP5_Workshop/svg_icon_browser.py - Version: 2
# X-Seti - April25 2026 - IMG Factory 1.6 - SVG Icon Browser for DP5 Workshop
"""
SVG Icon Browser — browse, preview, edit, save, export SVG icons
from imgfactory_svg_icons.py.
Open via DP5 → Image Ops → Icon Browser.
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
# SVGIconBrowser._get_full_method_source
# SVGIconBrowser._on_source_changed
# SVGIconBrowser._preview_refresh
# SVGIconBrowser._export_svg
# SVGIconBrowser._export_as_method
# SVGIconBrowser._copy_method
# SVGIconBrowser._save_to_file
# SVGIconBrowser._replace_icon
# SVGIconBrowser._inject_svg
# SVGIconBrowser._on_close
# SVGIconBrowser._apply_theme

import re
import os
import inspect
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLineEdit, QListWidget,
    QListWidgetItem, QLabel, QTextEdit, QPushButton, QSplitter,
    QWidget, QFrame, QFileDialog, QMessageBox, QApplication
)
from PyQt6.QtCore import Qt, QSize, QTimer
from PyQt6.QtGui import QFont, QIcon


class SVGIconBrowser(QDialog):
    """Browse, edit, export and save SVG icons from SVGIconFactory."""

    def __init__(self, main_window=None, parent=None):
        super().__init__(parent)
        self.main_window   = main_window
        self.app_settings  = getattr(main_window, 'app_settings', None)
        self._factory      = None
        self._icon_names   = []
        self._current_name = None
        self._icons_path   = None
        self._dirty        = False
        self.setWindowTitle("SVG Icon Browser — IMG Factory")
        self.setMinimumSize(960, 640)
        self.resize(1100, 720)
        self._build_ui()
        self._load_icons()
        self._apply_theme()

    # ── UI ────────────────────────────────────────────────────────────────────

    def _build_ui(self): #vers 1
        root = QVBoxLayout(self)
        root.setContentsMargins(8, 8, 8, 8)
        root.setSpacing(6)

        top = QHBoxLayout()
        self._search = QLineEdit()
        self._search.setPlaceholderText("Filter icons by name…")
        self._search.textChanged.connect(self._apply_filter)
        top.addWidget(QLabel("Search:"))
        top.addWidget(self._search, 1)
        self._count_lbl = QLabel("0 icons")
        self._count_lbl.setFont(QFont("Arial", 8))
        top.addWidget(self._count_lbl)
        root.addLayout(top)

        splitter = QSplitter(Qt.Orientation.Horizontal)
        splitter.setHandleWidth(5)

        # Left: icon grid
        left = QWidget()
        ll = QVBoxLayout(left)
        ll.setContentsMargins(0, 0, 0, 0)
        ll.setSpacing(4)
        ll.addWidget(QLabel("Icons:"))
        self._list = QListWidget()
        self._list.setViewMode(QListWidget.ViewMode.IconMode)
        self._list.setIconSize(QSize(36, 36))
        self._list.setGridSize(QSize(90, 70))
        self._list.setResizeMode(QListWidget.ResizeMode.Adjust)
        self._list.setMovement(QListWidget.Movement.Static)
        self._list.setWordWrap(True)
        self._list.itemSelectionChanged.connect(self._on_icon_selected)
        ll.addWidget(self._list, 1)
        splitter.addWidget(left)

        # Right: preview + editor
        right = QWidget()
        rl = QVBoxLayout(right)
        rl.setContentsMargins(0, 0, 0, 0)
        rl.setSpacing(6)

        prev_row = QHBoxLayout()
        self._preview_lbl = QLabel()
        self._preview_lbl.setFixedSize(96, 96)
        self._preview_lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._preview_lbl.setFrameShape(QFrame.Shape.StyledPanel)
        prev_row.addWidget(self._preview_lbl)

        info = QVBoxLayout()
        self._name_lbl = QLabel("Select an icon")
        self._name_lbl.setFont(QFont("Arial", 11, QFont.Weight.Bold))
        self._method_lbl = QLabel("")
        self._method_lbl.setFont(QFont("Courier New", 9))
        self._dirty_lbl = QLabel("")
        self._dirty_lbl.setFont(QFont("Arial", 8))
        info.addWidget(self._name_lbl)
        info.addWidget(self._method_lbl)
        info.addWidget(self._dirty_lbl)
        info.addStretch()
        self._refresh_prev_btn = QPushButton("Refresh Preview")
        self._refresh_prev_btn.setFixedWidth(130)
        self._refresh_prev_btn.setEnabled(False)
        self._refresh_prev_btn.clicked.connect(self._preview_refresh)
        info.addWidget(self._refresh_prev_btn)
        prev_row.addLayout(info, 1)
        rl.addLayout(prev_row)

        rl.addWidget(QLabel(
            "SVG Source  (edit then click 'Save to File' to update imgfactory_svg_icons.py):"))
        self._source_edit = QTextEdit()
        self._source_edit.setFont(QFont("Courier New", 9))
        self._source_edit.setMinimumHeight(200)
        self._source_edit.textChanged.connect(self._on_source_changed)
        rl.addWidget(self._source_edit, 1)

        btn_row = QHBoxLayout()

        self._save_btn = QPushButton("Save to File")
        self._save_btn.setToolTip(
            "Write edited SVG back into imgfactory_svg_icons.py")
        self._save_btn.clicked.connect(self._save_to_file)
        self._save_btn.setEnabled(False)

        self._export_btn = QPushButton("Export .svg")
        self._export_btn.setToolTip("Save as standalone .svg file")
        self._export_btn.clicked.connect(self._export_svg)
        self._export_btn.setEnabled(False)

        self._export_method_btn = QPushButton("Export as Method")
        self._export_method_btn.setToolTip(
            "Export complete Python method as .py file")
        self._export_method_btn.clicked.connect(self._export_as_method)
        self._export_method_btn.setEnabled(False)

        self._copy_btn = QPushButton("Copy Method")
        self._copy_btn.setToolTip("Copy full Python method to clipboard")
        self._copy_btn.clicked.connect(self._copy_method)
        self._copy_btn.setEnabled(False)

        self._replace_btn = QPushButton("Replace from File…")
        self._replace_btn.setToolTip(
            "Load an .svg file and replace current editor content")
        self._replace_btn.clicked.connect(self._replace_icon)
        self._replace_btn.setEnabled(False)

        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self._on_close)

        btn_row.addWidget(self._save_btn)
        btn_row.addWidget(self._export_btn)
        btn_row.addWidget(self._export_method_btn)
        btn_row.addWidget(self._copy_btn)
        btn_row.addWidget(self._replace_btn)
        btn_row.addStretch()
        btn_row.addWidget(close_btn)
        rl.addLayout(btn_row)

        splitter.addWidget(right)
        splitter.setStretchFactor(0, 1)
        splitter.setStretchFactor(1, 2)
        root.addWidget(splitter, 1)

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
            QMessageBox.warning(self, "Load Error",
                f"Could not load icons:\n{e}")

    def _populate_list(self, names): #vers 1
        self._list.clear()
        color = self._icon_color()
        for name in names:
            try:
                icon  = getattr(self._factory, name)(32, color)
                label = name.replace('_icon', '').replace('_', ' ')
                item  = QListWidgetItem(icon, label)
                item.setData(Qt.ItemDataRole.UserRole, name)
                item.setToolTip(name)
                item.setSizeHint(QSize(88, 68))
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
        return self.palette().color(
            self.palette().ColorRole.WindowText).name()

    # ── Filtering & selection ─────────────────────────────────────────────────

    def _apply_filter(self, text): #vers 1
        text = text.lower().strip()
        filtered = self._icon_names if not text else [
            n for n in self._icon_names if text in n]
        self._populate_list(filtered)

    def _on_icon_selected(self): #vers 1
        items = self._list.selectedItems()
        if not items:
            return
        if self._dirty:
            r = QMessageBox.question(self, "Unsaved Changes",
                "Discard unsaved edits?")
            if r != QMessageBox.StandardButton.Yes:
                return
        name = items[0].data(Qt.ItemDataRole.UserRole)
        self._current_name = name
        self._dirty        = False
        self._dirty_lbl.setText("")
        self._name_lbl.setText(
            name.replace('_icon', '').replace('_', ' ').title())
        self._method_lbl.setText(f"SVGIconFactory.{name}(size, color)")

        try:
            icon = getattr(self._factory, name)(80, self._icon_color())
            self._preview_lbl.setPixmap(icon.pixmap(80, 80))
        except Exception:
            self._preview_lbl.clear()

        src = self._get_svg_source(name)
        self._source_edit.blockSignals(True)
        self._source_edit.setPlainText(src)
        self._source_edit.blockSignals(False)

        self._save_btn.setEnabled(False)
        self._export_btn.setEnabled(bool(src))
        self._export_method_btn.setEnabled(True)
        self._copy_btn.setEnabled(True)
        self._replace_btn.setEnabled(True)
        self._refresh_prev_btn.setEnabled(True)

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
        except Exception as e:
            return f"# Could not extract source: {e}"

    def _get_full_method_source(self, name): #vers 1
        try:
            return inspect.getsource(getattr(self._factory, name))
        except Exception as e:
            return f"# Error: {e}"

    def _on_source_changed(self): #vers 1
        if self._current_name:
            self._dirty = True
            self._dirty_lbl.setText("● unsaved changes")
            self._save_btn.setEnabled(True)

    def _preview_refresh(self): #vers 1
        """Re-render preview from editor content using QSvgRenderer."""
        svg = self._source_edit.toPlainText().strip()
        if not svg:
            return
        try:
            from PyQt6.QtSvg import QSvgRenderer
            from PyQt6.QtGui import QPixmap, QPainter
            if not svg.strip().startswith('<svg'):
                svg = (f'<svg viewBox="0 0 24 24" '
                       f'xmlns="http://www.w3.org/2000/svg">\n{svg}\n</svg>')
            renderer = QSvgRenderer(svg.encode())
            pm = QPixmap(80, 80)
            pm.fill(Qt.GlobalColor.transparent)
            p = QPainter(pm)
            renderer.render(p)
            p.end()
            self._preview_lbl.setPixmap(pm)
        except Exception:
            self._preview_lbl.setText("?")

    # ── Actions ───────────────────────────────────────────────────────────────

    def _export_svg(self): #vers 1
        if not self._current_name:
            return
        svg = self._source_edit.toPlainText().strip()
        if not svg:
            return
        default = self._current_name.replace('_icon', '') + '.svg'
        path, _ = QFileDialog.getSaveFileName(
            self, "Export SVG", default, "SVG Files (*.svg)")
        if not path:
            return
        try:
            if not svg.startswith('<svg'):
                svg = (f'<svg viewBox="0 0 24 24" '
                       f'xmlns="http://www.w3.org/2000/svg">\n{svg}\n</svg>')
            with open(path, 'w') as f:
                f.write(svg)
            QMessageBox.information(self, "Exported", f"Saved:\n{path}")
        except Exception as e:
            QMessageBox.warning(self, "Export Error", str(e))

    def _export_as_method(self): #vers 1
        """Export the complete Python method as a .py file."""
        if not self._current_name:
            return
        name    = self._current_name
        svg     = self._source_edit.toPlainText().strip()
        default = name + '.py'
        path, _ = QFileDialog.getSaveFileName(
            self, "Export Method", default, "Python Files (*.py)")
        if not path:
            return
        try:
            method_src = self._get_full_method_source(name)
            if self._dirty and svg:
                method_src = self._inject_svg(method_src, svg)
            header = (
                f"#!/usr/bin/env python3\n"
                f"# Exported from SVG Icon Browser — IMG Factory\n"
                f"# Icon: {name}\n"
                f"# Add to apps/methods/imgfactory_svg_icons.py "
                f"inside class SVGIconFactory\n\n"
                f"from PyQt6.QtCore import QSize, QByteArray\n"
                f"from PyQt6.QtGui import QIcon, QPixmap, QPainter\n"
                f"from PyQt6.QtSvg import QSvgRenderer\n\n\n"
            )
            with open(path, 'w') as f:
                f.write(header + method_src)
            QMessageBox.information(self, "Exported", f"Method saved:\n{path}")
        except Exception as e:
            QMessageBox.warning(self, "Export Error", str(e))

    def _copy_method(self): #vers 1
        if not self._current_name:
            return
        src = self._get_full_method_source(self._current_name)
        if self._dirty:
            src = self._inject_svg(src,
                self._source_edit.toPlainText().strip())
        QApplication.clipboard().setText(src)
        self._copy_btn.setText("Copied!")
        QTimer.singleShot(1500, lambda: self._copy_btn.setText("Copy Method"))

    def _save_to_file(self): #vers 1
        """Write edited SVG back into imgfactory_svg_icons.py."""
        if not self._current_name or not self._icons_path or not self._dirty:
            return
        new_svg = self._source_edit.toPlainText().strip()
        if not new_svg:
            QMessageBox.warning(self, "Save", "SVG source is empty.")
            return
        try:
            with open(self._icons_path, 'r') as f:
                file_src = f.read()

            old_meth = inspect.getsource(
                getattr(self._factory, self._current_name))
            new_meth = self._inject_svg(old_meth, new_svg)

            if old_meth not in file_src:
                QMessageBox.warning(self, "Save Error",
                    "Cannot locate method in source file.\n"
                    "It may be inherited or dynamically generated.")
                return

            new_file = file_src.replace(old_meth, new_meth, 1)

            import ast
            try:
                ast.parse(new_file)
            except SyntaxError as e:
                QMessageBox.critical(self, "Syntax Error",
                    f"Edit would break the file:\n{e}")
                return

            with open(self._icons_path, 'w') as f:
                f.write(new_file)

            self._dirty = False
            self._dirty_lbl.setText("✓ saved")
            self._save_btn.setEnabled(False)
            QTimer.singleShot(2000, lambda: self._dirty_lbl.setText(""))
            QMessageBox.information(self, "Saved",
                f"'{self._current_name}' updated.\n"
                "Restart to see changes.")
        except Exception as e:
            QMessageBox.warning(self, "Save Error", str(e))

    def _replace_icon(self): #vers 1
        """Load an .svg file and paste its content into the editor."""
        if not self._current_name:
            return
        path, _ = QFileDialog.getOpenFileName(
            self, "Select SVG", "", "SVG Files (*.svg);;All Files (*)")
        if not path:
            return
        try:
            with open(path, 'r') as f:
                svg = f.read().strip()
        except Exception as e:
            QMessageBox.warning(self, "Read Error", str(e))
            return
        svg = re.sub(r'<\?xml[^>]*\?>', '', svg).strip()
        svg = re.sub(r'<!DOCTYPE[^>]*>', '', svg).strip()
        self._source_edit.setPlainText(svg)

    def _on_close(self): #vers 1
        if self._dirty:
            r = QMessageBox.question(self, "Unsaved Changes",
                "Discard unsaved edits and close?")
            if r != QMessageBox.StandardButton.Yes:
                return
        self.accept()

    # ── Helpers ───────────────────────────────────────────────────────────────

    def _inject_svg(self, method_src: str, new_svg: str) -> str: #vers 1
        """Replace svg_data content inside a method source string."""
        for pat in [r"(svg_data\s*=\s*''')(.*?)(''')",
                    r'(svg_data\s*=\s*""")(.*?)(""")'  ]:
            m = re.search(pat, method_src, re.DOTALL)
            if m:
                return (method_src[:m.start(2)]
                        + '\n        ' + new_svg + '\n    '
                        + method_src[m.end(2):])
        return method_src

    # ── Theme ─────────────────────────────────────────────────────────────────

    def _apply_theme(self): #vers 2
        """Theme-aware styling — no hardcoded colours."""
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
            QDialog     {{ background:{bg}; color:{fg}; }}
            QListWidget {{ background:{mid}; border:1px solid {acc}; }}
            QListWidget::item:selected {{ background:{acc}; color:{fg}; }}
            QTextEdit   {{ background:{mid}; color:{fg};
                           border:1px solid {acc}; }}
            QLineEdit   {{ background:{mid}; color:{fg};
                           border:1px solid {acc}; padding:3px; }}
            QPushButton {{ background:{mid}; color:{fg};
                           border:1px solid {acc};
                           padding:4px 10px; border-radius:3px; }}
            QPushButton:hover    {{ background:{acc}; }}
            QPushButton:disabled {{ opacity:0.4; }}
            QLabel      {{ color:{fg}; background:transparent; }}
            QSplitter::handle {{ background:{acc}; }}
        """)
