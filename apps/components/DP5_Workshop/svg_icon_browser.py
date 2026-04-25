# apps/components/DP5_Workshop/svg_icon_browser.py — Version 1
# X-Seti - Apr25 2026 - IMG Factory 1.6 - SVG Icon Browser for DP5 Workshop
"""Browse, preview, export and replace SVG icons from imgfactory_svg_icons.py.
Open via DP5 → Image Ops → Icon Browser.
"""

##Methods list -
# SVGIconBrowser.__init__
# SVGIconBrowser._build_ui
# SVGIconBrowser._load_icons
# SVGIconBrowser._apply_filter
# SVGIconBrowser._on_icon_selected
# SVGIconBrowser._get_svg_source
# SVGIconBrowser._export_svg
# SVGIconBrowser._copy_method
# SVGIconBrowser._replace_icon
# SVGIconBrowser._apply_theme

import re
import os
import inspect
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLineEdit, QListWidget,
    QListWidgetItem, QLabel, QTextEdit, QPushButton, QSplitter,
    QWidget, QFrame, QScrollArea, QFileDialog, QMessageBox,
    QSizePolicy, QGridLayout
)
from PyQt6.QtCore import Qt, QSize
from PyQt6.QtGui import QFont, QIcon


class SVGIconBrowser(QDialog):
    """Browse all icons in SVGIconFactory, preview SVG, export or replace."""

    def __init__(self, main_window=None, parent=None):
        super().__init__(parent)
        self.main_window = main_window
        self.setWindowTitle("SVG Icon Browser — IMG Factory")
        self.setMinimumSize(900, 600)
        self.resize(1050, 680)
        self._icon_names = []  # list of (name, method) tuples
        self._current_name = None
        self._build_ui()
        self._load_icons()
        self._apply_theme()

    def _build_ui(self): #vers 1
        root = QVBoxLayout(self)
        root.setContentsMargins(8, 8, 8, 8)
        root.setSpacing(6)

        # Search bar
        top = QHBoxLayout()
        self._search = QLineEdit()
        self._search.setPlaceholderText("Filter icons by name…")
        self._search.textChanged.connect(self._apply_filter)
        top.addWidget(QLabel("Search:"))
        top.addWidget(self._search, 1)
        root.addLayout(top)

        # Main splitter: icon list | preview + source
        splitter = QSplitter(Qt.Orientation.Horizontal)
        splitter.setHandleWidth(5)

        # Left: icon grid list
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

        self._count_lbl = QLabel("0 icons")
        self._count_lbl.setFont(QFont("Arial", 8))
        ll.addWidget(self._count_lbl)
        splitter.addWidget(left)

        # Right: preview + SVG source
        right = QWidget()
        rl = QVBoxLayout(right)
        rl.setContentsMargins(0, 0, 0, 0)
        rl.setSpacing(6)

        # Preview row
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
        info.addWidget(self._name_lbl)
        info.addWidget(self._method_lbl)
        info.addStretch()
        prev_row.addLayout(info, 1)
        rl.addLayout(prev_row)

        # SVG source
        rl.addWidget(QLabel("SVG Source:"))
        self._source_edit = QTextEdit()
        self._source_edit.setFont(QFont("Courier New", 9))
        self._source_edit.setReadOnly(True)
        self._source_edit.setMinimumHeight(180)
        rl.addWidget(self._source_edit, 1)

        # Buttons
        btn_row = QHBoxLayout()
        self._export_btn = QPushButton("Export .svg")
        self._export_btn.setToolTip("Save SVG source as a .svg file")
        self._export_btn.clicked.connect(self._export_svg)
        self._export_btn.setEnabled(False)

        self._copy_btn = QPushButton("Copy Method")
        self._copy_btn.setToolTip("Copy full Python method to clipboard")
        self._copy_btn.clicked.connect(self._copy_method)
        self._copy_btn.setEnabled(False)

        self._replace_btn = QPushButton("Replace Icon…")
        self._replace_btn.setToolTip("Load an SVG file and update this icon in imgfactory_svg_icons.py")
        self._replace_btn.clicked.connect(self._replace_icon)
        self._replace_btn.setEnabled(False)

        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.accept)

        btn_row.addWidget(self._export_btn)
        btn_row.addWidget(self._copy_btn)
        btn_row.addWidget(self._replace_btn)
        btn_row.addStretch()
        btn_row.addWidget(close_btn)
        rl.addLayout(btn_row)

        splitter.addWidget(right)
        splitter.setStretchFactor(0, 1)
        splitter.setStretchFactor(1, 2)
        root.addWidget(splitter, 1)

    def _load_icons(self): #vers 1
        """Enumerate all static methods ending in _icon from SVGIconFactory."""
        try:
            from apps.methods.imgfactory_svg_icons import SVGIconFactory
            self._factory = SVGIconFactory
            names = sorted([
                name for name in dir(SVGIconFactory)
                if name.endswith('_icon') and not name.startswith('__')
            ])
            self._icon_names = names
            self._populate_list(names)
        except Exception as e:
            QMessageBox.warning(self, "Load Error", f"Could not load icons:\n{e}")

    def _populate_list(self, names): #vers 1
        self._list.clear()
        color = '#aaaaaa'
        try:
            if self.main_window and hasattr(self.main_window, 'app_settings'):
                tc = self.main_window.app_settings.get_theme_colors() or {}
                color = tc.get('text_primary', color)
        except Exception:
            pass

        for name in names:
            try:
                method = getattr(self._factory, name)
                icon = method(32, color)
                label = name.replace('_icon', '').replace('_', ' ')
                item = QListWidgetItem(icon, label)
                item.setData(Qt.ItemDataRole.UserRole, name)
                item.setToolTip(name)
                item.setSizeHint(QSize(88, 68))
                self._list.addItem(item)
            except Exception:
                pass
        self._count_lbl.setText(f"{self._list.count()} icons")

    def _apply_filter(self, text): #vers 1
        text = text.lower().strip()
        if not text:
            filtered = self._icon_names
        else:
            filtered = [n for n in self._icon_names if text in n]
        self._populate_list(filtered)

    def _on_icon_selected(self): #vers 1
        items = self._list.selectedItems()
        if not items:
            return
        name = items[0].data(Qt.ItemDataRole.UserRole)
        self._current_name = name
        self._name_lbl.setText(name.replace('_icon', '').replace('_', ' ').title())
        self._method_lbl.setText(f"SVGIconFactory.{name}(size, color)")

        # Large preview
        try:
            method = getattr(self._factory, name)
            icon = method(80, '#cccccc')
            self._preview_lbl.setPixmap(icon.pixmap(80, 80))
        except Exception:
            self._preview_lbl.clear()

        # SVG source
        src = self._get_svg_source(name)
        self._source_edit.setPlainText(src)

        self._export_btn.setEnabled(bool(src))
        self._copy_btn.setEnabled(True)
        self._replace_btn.setEnabled(True)

    def _get_svg_source(self, name): #vers 1
        """Extract the svg_data string from the method source."""
        try:
            method = getattr(self._factory, name)
            src = inspect.getsource(method)
            # Extract svg_data content between triple quotes
            m = re.search(r"svg_data\s*=\s*'''(.*?)'''", src, re.DOTALL)
            if m:
                return m.group(1).strip()
            m = re.search(r'svg_data\s*=\s*"""(.*?)"""', src, re.DOTALL)
            if m:
                return m.group(1).strip()
            return src
        except Exception as e:
            return f"# Could not extract source: {e}"

    def _get_full_method_source(self, name): #vers 1
        """Return the full Python method source."""
        try:
            method = getattr(self._factory, name)
            return inspect.getsource(method)
        except Exception as e:
            return f"# Error: {e}"

    def _export_svg(self): #vers 1
        if not self._current_name:
            return
        svg = self._get_svg_source(self._current_name)
        if not svg:
            return
        default = self._current_name.replace('_icon', '') + '.svg'
        path, _ = QFileDialog.getSaveFileName(
            self, "Export SVG", default, "SVG Files (*.svg)")
        if not path:
            return
        try:
            # Wrap in full SVG document if needed
            if not svg.strip().startswith('<svg'):
                svg = f'<svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">\n{svg}\n</svg>'
            with open(path, 'w') as f:
                f.write(svg)
            QMessageBox.information(self, "Exported", f"Saved to:\n{path}")
        except Exception as e:
            QMessageBox.warning(self, "Export Error", str(e))

    def _copy_method(self): #vers 1
        if not self._current_name:
            return
        from PyQt6.QtWidgets import QApplication
        src = self._get_full_method_source(self._current_name)
        QApplication.clipboard().setText(src)
        self._copy_btn.setText("Copied!")
        from PyQt6.QtCore import QTimer
        QTimer.singleShot(1500, lambda: self._copy_btn.setText("Copy Method"))

    def _replace_icon(self): #vers 1
        """Load an .svg file and update the svg_data in imgfactory_svg_icons.py."""
        if not self._current_name:
            return
        path, _ = QFileDialog.getOpenFileName(
            self, "Select SVG to import", "", "SVG Files (*.svg)")
        if not path:
            return
        try:
            with open(path, 'r') as f:
                new_svg = f.read().strip()
        except Exception as e:
            QMessageBox.warning(self, "Read Error", str(e))
            return

        # Strip XML declaration / DOCTYPE if present
        new_svg = re.sub(r'<\?xml[^>]*\?>', '', new_svg).strip()
        new_svg = re.sub(r'<!DOCTYPE[^>]*>', '', new_svg).strip()

        # Find and update in the source file
        icons_path = os.path.join(
            os.path.dirname(__file__), '..', '..', 'methods', 'imgfactory_svg_icons.py')
        icons_path = os.path.normpath(icons_path)
        try:
            with open(icons_path, 'r') as f:
                source = f.read()

            # Find the method and replace its svg_data
            method = getattr(self._factory, self._current_name)
            old_src = inspect.getsource(method)
            m = re.search(r"(svg_data\s*=\s*''')(.*?)(''')", old_src, re.DOTALL)
            if not m:
                m = re.search(r'(svg_data\s*=\s*""")(.*?)(""")', old_src, re.DOTALL)
            if not m:
                QMessageBox.warning(self, "Replace Error",
                    "Could not locate svg_data string in method source.")
                return

            new_method_src = old_src[:m.start(2)] + '\n        ' + new_svg + '\n    ' + old_src[m.end(2):]

            if old_src not in source:
                QMessageBox.warning(self, "Replace Error",
                    "Could not locate method in source file (possibly inherited).")
                return

            new_source = source.replace(old_src, new_method_src)
            with open(icons_path, 'w') as f:
                f.write(new_source)

            # Refresh preview
            self._source_edit.setPlainText(new_svg)
            QMessageBox.information(self, "Replaced",
                f"Icon '{self._current_name}' updated in imgfactory_svg_icons.py\n"
                f"Restart IMG Factory to see the change take effect.")
        except Exception as e:
            QMessageBox.warning(self, "Replace Error", str(e))

    def _apply_theme(self): #vers 1
        try:
            if not self.main_window or not hasattr(self.main_window, 'app_settings'):
                return
            tc = self.main_window.app_settings.get_theme_colors() or {}
            bg   = tc.get('bg_primary', '#1e1e1e')
            fg   = tc.get('text_primary', '#ffffff')
            mid  = tc.get('panel_bg', '#333333')
            acc  = tc.get('accent_primary', '#1976d2')
            self.setStyleSheet(f"""
                QDialog {{ background: {bg}; color: {fg}; }}
                QListWidget {{ background: {mid}; border: 1px solid {acc}; }}
                QListWidget::item:selected {{ background: {acc}; }}
                QTextEdit {{ background: {mid}; color: {fg}; border: 1px solid {acc}; }}
                QLineEdit {{ background: {mid}; color: {fg}; border: 1px solid {acc}; padding: 3px; }}
                QPushButton {{ background: {mid}; color: {fg}; border: 1px solid {acc}; padding: 4px 10px; border-radius: 3px; }}
                QPushButton:hover {{ background: {acc}; }}
                QLabel {{ color: {fg}; background: transparent; }}
                QSplitter::handle {{ background: {acc}; }}
            """)
        except Exception:
            pass
