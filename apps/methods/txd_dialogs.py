#this belongs in apps/methods/txd_dialogs.py - Version: 7
# X-Seti - September30 2026 - IMG Factory 1.6 - TXD dialogs

"""
Texture windows shared by TXD and Asset Workshop: bumpmap, mipmap, properties, preview.
"""

# _work_dir

##class TexturePreviewWidget: -
# _get_ui_color
# paintEvent
# set_checkerboard_background

##class ZoomablePreview: -
# _draw_checkerboard
# fit_to_window
# _get_ui_color
# __init__
# mouseMoveEvent
# mousePressEvent
# mouseReleaseEvent
# paintEvent
# pan
# reset_view
# set_background_color
# set_checkerboard_background
# set_pixmap
# setPixmap
# _update_scaled_pixmap
# wheelEvent
# zoom_in
# zoom_out

##class TexturePropertiesDialog: -
# _apply_changes
# _change_workshop_button_mode
# _create_advanced_tab
# _create_basic_tab
# _create_format_tab
# _create_mipmap_tab
# _create_settings_tab
# _do_generate_mipmaps
# _export_mipmaps
# _generate_mipmaps
# __init__
# _ok_clicked
# setup_ui
# _view_mipmaps

##class MipmapManagerWindow: -
# _apply_changes
# _auto_generate_mipmaps
# _before_change
# _clear_all_levels
# _create_action_section
# _create_bottom_bar
# _create_info_section
# _create_level_card
# _create_preview_widget
# _create_stat_box
# _create_stats_grid
# _create_title_bar
# _create_toolbar
# _delete_level
# _edit_main_texture
# _export_all_levels
# _export_level
# _import_all_levels
# _import_level
# __init__
# _level
# mouseMoveEvent
# mousePressEvent
# mouseReleaseEvent
# setup_ui

##class BumpmapManagerWindow: -
# _apply_changes
# _apply_changes
# closeEvent
# _convert_numpy_to_qimage
# _create_left_panel
# _create_menu_bar
# _create_middle_panel
# _create_reflection_panel
# _create_right_panel
# _create_title_bar
# _delete_bumpmap
# _export_bumpmap
# _export_reflection_maps
# _generate_bumpmap
# _generate_reflection_from_normal
# _generate_reflection_maps
# _has_bumpmap
# _import_bumpmap
# _import_reflection_maps
# __init__
# setup_ui
# _toggle_maximize
# _update_bumpmap_preview
# _update_reflection_previews

import os
import numpy as np
from PyQt6.QtCore import QPoint, Qt
from PyQt6.QtGui import QCursor, QFont, QImage, QPainter, QPixmap
from PyQt6.QtWidgets import QComboBox, QDialog, QFormLayout, QFrame, QGroupBox, QHBoxLayout, QLabel, QLineEdit, QMessageBox, QPushButton, QScrollArea, QTabWidget, QTextEdit, QVBoxLayout, QWidget
from apps.methods.txd_dxt_encode import _encode_dxt1, _encode_dxt5

from apps.methods.imgfactory_svg_icons import SVGIconFactory

__all__ = ['TexturePreviewWidget', 'ZoomablePreview', 'TexturePropertiesDialog', 'MipmapManagerWindow', 'BumpmapManagerWindow']


def _work_dir(owner) -> str: #vers 1
    """Workshop's last used folder for dialog start paths, else home."""
    ws = getattr(owner, 'parent_workshop', None)
    if ws is not None and hasattr(ws, '_start_dir'):
        return ws._start_dir()
    return os.path.expanduser('~')


class TexturePreviewWidget(QLabel): #vers 1
    """ Test preview widget  """

    def _get_ui_color(self, key): #vers 1
        """Theme colour via methods/ui_color."""
        from apps.methods.ui_color import get_ui_color
        return get_ui_color(self, key)

    def set_checkerboard_background(self): #vers 1
        """Set checkerboard pattern background"""
        self.background_mode = 'checkerboard'
        self.bg_color = None
        self.update()

    def paintEvent(self, event): #vers 3 (update existing)
        """Paint the preview with proper background"""
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        painter.setRenderHint(QPainter.RenderHint.SmoothPixmapTransform)

        # Draw background
        if hasattr(self, 'background_mode') and self.background_mode == 'checkerboard':
            # Draw checkerboard pattern
            checker_size = 16
            light_gray = self._get_ui_color('border')
            dark_gray = self._get_ui_color('viewport_text')

            for y in range(0, self.height(), checker_size):
                for x in range(0, self.width(), checker_size):
                    if ((x // checker_size) + (y // checker_size)) % 2 == 0:
                        painter.fillRect(x, y, checker_size, checker_size, light_gray)
                    else:
                        painter.fillRect(x, y, checker_size, checker_size, dark_gray)
        elif self.bg_color:
            # Use palette(base) if no explicit color set (theme-aware)
          bg = self.bg_color
          if bg is None:
              from PyQt6.QtGui import QColor
              win = self.palette().color(self.palette().ColorRole.Window)
              bg = self._get_ui_color('viewport_bg') if win.lightness() > 128 else self._get_ui_color('viewport_bg')
          painter.fillRect(self.rect(), bg)
        else:
            painter.fillRect(self.rect(), self._get_ui_color('viewport_bg'))

        if self.pixmap and not self.pixmap.isNull():
            # Calculate position to center the image
            x = (self.width() - self.scaled_pixmap.width()) // 2
            y = (self.height() - self.scaled_pixmap.height()) // 2
            painter.drawPixmap(x, y, self.scaled_pixmap)
        elif self.placeholder_text:
            painter.setPen(self._get_ui_color('viewport_text'))
            painter.drawText(self.rect(), Qt.AlignmentFlag.AlignCenter, self.placeholder_text)


class ZoomablePreview(QLabel): #vers 2
    """Fixed preview widget with zoom and pan"""

    def __init__(self, parent=None):  #vers 1
        super().__init__(parent)
        self.main_window = parent
        self.original_pixmap = None
        self.scaled_pixmap = None
        self.zoom_level = 1.0
        self.pan_offset = QPoint(0, 0)
        self.dragging = False
        self.drag_start = QPoint(0, 0)
        # Theme-aware default: set in first paintEvent from palette
        self.bg_color = None   # None = auto from palette
        self.background_mode = 'solid'
        self._checkerboard_size = 16
        self.placeholder_text = "No texture loaded"

        self.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.setMinimumSize(400, 400)
        self.setStyleSheet("border: 1px solid palette(mid);")
        self.setMouseTracking(True)


    def setPixmap(self, pixmap): #vers 2
        """Set pixmap and update display"""
        if pixmap and not pixmap.isNull():
            self.original_pixmap = pixmap
            self.placeholder_text = None
            self._update_scaled_pixmap()
        else:
            self.original_pixmap = None
            self.scaled_pixmap = None
            self.placeholder_text = "No texture loaded"

        self.update()  # Trigger repaint



    def _get_ui_color(self, key): #vers 2
        """Theme QColor via shared helper."""
        from apps.methods.ui_color import get_ui_color
        return get_ui_color(self, key)

    def _update_scaled_pixmap(self): #vers 1

        """Update the scaled pixmap based on zoom level"""
        if not self.original_pixmap:
            self.scaled_pixmap = None
            return

        scaled_size = self.original_pixmap.size() * self.zoom_level
        self.scaled_pixmap = self.original_pixmap.scaled(
            scaled_size,
            Qt.AspectRatioMode.KeepAspectRatio,
            Qt.TransformationMode.SmoothTransformation
        )


    def paintEvent(self, event): #vers 3
        """Paint the preview with background and image"""
        from PyQt6.QtGui import QColor
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        painter.setRenderHint(QPainter.RenderHint.SmoothPixmapTransform)

        win = self.palette().color(self.palette().ColorRole.Window)
        is_light = win.lightness() > 128

        if self.background_mode == 'checkerboard':
            self._draw_checkerboard(painter)
        else:
            bg = self.bg_color
            if bg is None:
                bg = self._get_ui_color('viewport_bg') if is_light else self._get_ui_color('viewport_bg')
            painter.fillRect(self.rect(), bg)

        if self.scaled_pixmap and not self.scaled_pixmap.isNull():
            x = (self.width() - self.scaled_pixmap.width()) // 2 + self.pan_offset.x()
            y = (self.height() - self.scaled_pixmap.height()) // 2 + self.pan_offset.y()
            painter.drawPixmap(x, y, self.scaled_pixmap)
        elif self.placeholder_text:
            pen_color = self._get_ui_color('text_secondary') if is_light else self._get_ui_color('viewport_text')
            painter.setPen(pen_color)
            painter.drawText(self.rect(), Qt.AlignmentFlag.AlignCenter, self.placeholder_text)


    def _draw_checkerboard(self, painter): #vers 1
        """Draw checkerboard background pattern"""
        size = self._checkerboard_size
        color1 = self._get_ui_color('border')
        color2 = self._get_ui_color('viewport_text')

        for y in range(0, self.height(), size):
            for x in range(0, self.width(), size):
                color = color1 if ((x // size) + (y // size)) % 2 == 0 else color2
                painter.fillRect(x, y, size, size, color)


    def zoom_in(self): #vers 2
        """Zoom in by 20%"""
        self.zoom_level = min(self.zoom_level * 1.2, 10.0)
        self._update_scaled_pixmap()
        self.update()


    def zoom_out(self): #vers 2
        """Zoom out by 20%"""
        self.zoom_level = max(self.zoom_level / 1.2, 0.1)
        self._update_scaled_pixmap()
        self.update()


    def reset_view(self): #vers 2
        """Reset zoom and pan to defaults"""
        self.zoom_level = 1.0
        self.pan_offset = QPoint(0, 0)
        self._update_scaled_pixmap()
        self.update()


    def fit_to_window(self): #vers 2
        """Fit image to window size"""
        if not self.original_pixmap:
            return

        img_size = self.original_pixmap.size()
        widget_size = self.size()

        zoom_w = widget_size.width() / img_size.width()
        zoom_h = widget_size.height() / img_size.height()

        self.zoom_level = min(zoom_w, zoom_h) * 0.95
        self.pan_offset = QPoint(0, 0)
        self._update_scaled_pixmap()
        self.update()


    def pan(self, dx, dy): #vers 1
        """Pan the view by dx, dy pixels"""
        self.pan_offset += QPoint(dx, dy)
        self.update()


    def set_pixmap(self, pixmap): #vers 1
        """Store texture pixmap (with alpha) and refresh display"""
        self.original_pixmap = pixmap
        self._update_scaled_pixmap()
        self.update()


    def set_checkerboard_background(self): #vers 1
        """Enable checkerboard background"""
        self.background_mode = 'checkerboard'
        self.update()


    def set_background_color(self, color): #vers 2
        """Set solid background color (None = auto from palette)"""
        self.background_mode = 'solid'
        self.bg_color = color
        self.update()


    def mousePressEvent(self, event): #vers 1
        """Start pan drag on left button"""
        if event.button() == Qt.MouseButton.LeftButton:
            self.dragging = True
            self.drag_start = event.pos()
            self.setCursor(QCursor(Qt.CursorShape.ClosedHandCursor))


    def mouseMoveEvent(self, event): #vers 1
        """Handle pan dragging"""
        if self.dragging:
            delta = event.pos() - self.drag_start
            self.pan_offset += delta
            self.drag_start = event.pos()
            self.update()


    def mouseReleaseEvent(self, event): #vers 1
        """End pan drag"""
        if event.button() == Qt.MouseButton.LeftButton:
            self.dragging = False
            self.setCursor(QCursor(Qt.CursorShape.ArrowCursor))


    def wheelEvent(self, event): #vers 1
        """Mouse wheel zoom"""
        if event.angleDelta().y() > 0:
            self.zoom_in()
        else:
            self.zoom_out()


class TexturePropertiesDialog(QDialog): #vers 1
    """Complete texture properties dialog with all settings"""

    def __init__(self, parent, texture_data, main_window=None):  #vers 2
        super().__init__(parent)
        self.parent_workshop = parent
        self.texture_data = texture_data.copy()  # Work on copy
        self.original_texture = texture_data
        self.main_window = main_window
        self.changes_made = False

        self.setWindowTitle(f"Properties: {texture_data.get('name', 'Unknown')}")
        self.setModal(True)
        self.resize(500, 600)
        self.setup_ui()


    def setup_ui(self): #vers 3
        """Setup properties dialog UI"""
        layout = QVBoxLayout(self)
        layout.setContentsMargins(10, 10, 10, 10)

        # Header with texture name
        header = QLabel(f"Texture: {self.texture_data.get('name', 'Unknown')}")
        header.setFont(QFont("Arial", 14, QFont.Weight.Bold))
        layout.addWidget(header)

        # Tabs for different property sections
        tabs = QTabWidget()

        # Tab 1: Basic Info
        basic_tab = self._create_basic_tab()
        tabs.addTab(basic_tab, "Basic")

        # Tab 2: Format Settings
        format_tab = self._create_format_tab()
        tabs.addTab(format_tab, "Format")

        # Tab 3: Mipmap Info
        mipmap_tab = self._create_mipmap_tab()
        tabs.addTab(mipmap_tab, "Mipmaps")

        # Tab 4: Advanced
        advanced_tab = self._create_advanced_tab()
        tabs.addTab(advanced_tab, "Advanced")

        # Tab 5: Settings
        tabs.addTab(self._create_settings_tab(), "Settings")

        layout.addWidget(tabs)

        # Bottom buttons
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        apply_btn = QPushButton("Apply")
        apply_btn.clicked.connect(self._apply_changes)
        button_layout.addWidget(apply_btn)

        ok_btn = QPushButton("OK")
        ok_btn.clicked.connect(self._ok_clicked)
        ok_btn.setDefault(True)
        button_layout.addWidget(ok_btn)

        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(self.reject)
        button_layout.addWidget(cancel_btn)

        layout.addLayout(button_layout)


    def _create_settings_tab(self): #vers 2
        """Create settings/appearance tab with button mode"""
        tab = QWidget()
        layout = QVBoxLayout(tab)
        layout.setContentsMargins(10, 10, 10, 10)

        # Workshop Appearance group
        workshop_group = QGroupBox("Workshop Appearance")
        workshop_layout = QFormLayout(workshop_group)

        # Button display mode
        button_mode_combo = QComboBox()
        button_mode_combo.addItems(["Icons + Text", "Icons Only", "Text Only"])

        # Set current mode
        if hasattr(self.parent_workshop, 'button_display_mode'):
            mode_map = {'both': 0, 'icons': 1, 'text': 2}
            current_index = mode_map.get(self.parent_workshop.button_display_mode, 0)
            button_mode_combo.setCurrentIndex(current_index)

        # Connect to update
        button_mode_combo.currentIndexChanged.connect(
            lambda idx: self._change_workshop_button_mode(idx)
        )

        workshop_layout.addRow("Button Style:", button_mode_combo)

        layout.addWidget(workshop_group)

        # ... rest of settings tab ...

        layout.addStretch()
        return tab


    def _change_workshop_button_mode(self, index): #vers 1
        """Change button display mode from properties"""
        if not hasattr(self, 'parent_workshop'):
            return

        mode_map = {0: 'both', 1: 'icons', 2: 'text'}
        new_mode = mode_map[index]

        self.parent_workshop.button_display_mode = new_mode
        self.parent_workshop._update_all_buttons()


    def _create_basic_tab(self): #vers 1
        """Create basic info tab"""
        tab = QWidget()
        layout = QVBoxLayout(tab)
        layout.setContentsMargins(10, 10, 10, 10)

        # Names group
        names_group = QGroupBox("Names")
        names_layout = QFormLayout(names_group)

        self.name_edit = QLineEdit(self.texture_data.get('name', ''))
        self.name_edit.setMaxLength(32)
        names_layout.addRow("Texture Name:", self.name_edit)

        # Alpha name (if has alpha)
        if self.texture_data.get('has_alpha', False):
            alpha_name = self.texture_data.get('alpha_name', self.texture_data.get('name', '') + 'a')
            self.alpha_name_edit = QLineEdit(alpha_name)
            self.alpha_name_edit.setMaxLength(32)
            names_layout.addRow("Alpha Name:", self.alpha_name_edit)
        else:
            self.alpha_name_edit = None

        layout.addWidget(names_group)

        # Dimensions group
        dim_group = QGroupBox("Dimensions")
        dim_layout = QFormLayout(dim_group)

        width = self.texture_data.get('width', 0)
        height = self.texture_data.get('height', 0)

        dim_label = QLabel(f"{width} x {height} pixels")
        dim_label.setStyleSheet("font-weight: bold;")
        dim_layout.addRow("Size:", dim_label)

        # Calculate memory size
        uncompressed_size = width * height * 4  # RGBA
        size_kb = uncompressed_size / 1024
        size_mb = size_kb / 1024

        if size_mb >= 1:
            size_str = f"{size_mb:.2f} MB"
        else:
            size_str = f"{size_kb:.2f} KB"

        size_label = QLabel(f"{size_str} (uncompressed)")
        dim_layout.addRow("Memory:", size_label)

        # Aspect ratio
        if width > 0 and height > 0:
            from math import gcd
            divisor = gcd(width, height)
            aspect_w = width // divisor
            aspect_h = height // divisor
            aspect_label = QLabel(f"{aspect_w}:{aspect_h}")
            dim_layout.addRow("Aspect Ratio:", aspect_label)

        layout.addWidget(dim_group)

        # Color info group
        color_group = QGroupBox("Color Information")
        color_layout = QFormLayout(color_group)

        depth = self.texture_data.get('depth', 32)
        color_layout.addRow("Bit Depth:", QLabel(f"{depth} bit"))

        has_alpha = self.texture_data.get('has_alpha', False)
        alpha_status = QLabel("Yes" if has_alpha else "No")
        alpha_status.setStyleSheet("color: red; font-weight: bold;" if has_alpha else "")
        color_layout.addRow("Alpha Channel:", alpha_status)

        layout.addWidget(color_group)

        layout.addStretch()
        return tab


    def _create_format_tab(self): #vers 1
        """Create format settings tab"""
        tab = QWidget()
        layout = QVBoxLayout(tab)
        layout.setContentsMargins(10, 10, 10, 10)

        # Current format group
        format_group = QGroupBox("Texture Format")
        format_layout = QFormLayout(format_group)

        current_format = self.texture_data.get('format', 'Unknown')
        format_label = QLabel(current_format)
        format_label.setStyleSheet("font-weight: bold;")
        format_layout.addRow("Current Format:", format_label)

        # Compression status
        is_compressed = 'DXT' in current_format
        compress_label = QLabel("Compressed" if is_compressed else "Uncompressed")
        compress_label.setStyleSheet("color: green;" if is_compressed else "color: orange;")
        format_layout.addRow("Status:", compress_label)

        # Show compression ratio if compressed
        if is_compressed:
            width = self.texture_data.get('width', 0)
            height = self.texture_data.get('height', 0)
            uncompressed = width * height * 4

            rgba_data = self.texture_data.get('rgba_data', b'')
            compressed = len(rgba_data) if rgba_data else 0

            if uncompressed > 0 and compressed > 0:
                ratio = uncompressed / compressed
                ratio_label = QLabel(f"{ratio:.1f}:1")
                format_layout.addRow("Compression Ratio:", ratio_label)

        layout.addWidget(format_group)

        # Format conversion group
        convert_group = QGroupBox("Format Conversion")
        convert_layout = QVBoxLayout(convert_group)

        convert_layout.addWidget(QLabel("Select target format:"))

        self.format_combo = QComboBox()
        self.format_combo.addItems([
            "DXT1 (No Alpha, 6:1)",
            "DXT3 (Sharp Alpha, 4:1)",
            "DXT5 (Smooth Alpha, 4:1)",
            "ARGB8888 (Uncompressed)",
            "RGB888 (Uncompressed, No Alpha)"
        ])

        # Set current selection
        format_map = {
            'DXT1': 0, 'DXT3': 1, 'DXT5': 2,
            'ARGB8888': 3, 'RGB888': 4
        }
        current_idx = format_map.get(current_format, 0)
        self.format_combo.setCurrentIndex(current_idx)

        convert_layout.addWidget(self.format_combo)

        convert_note = QLabel(
            "Note: Format conversion will be applied when you click Apply or OK.\n"
            "DXT formats reduce file size but may lose quality."
        )
        convert_note.setStyleSheet("color: #888; font-size: 10px;")
        convert_note.setWordWrap(True)
        convert_layout.addWidget(convert_note)

        layout.addWidget(convert_group)

        layout.addStretch()
        return tab


    def _create_mipmap_tab(self): #vers 1
        """Create mipmap info tab"""
        tab = QWidget()
        layout = QVBoxLayout(tab)
        layout.setContentsMargins(10, 10, 10, 10)

        # Mipmap status group
        status_group = QGroupBox("Mipmap Status")
        status_layout = QFormLayout(status_group)

        mipmap_levels = self.texture_data.get('mipmap_levels', [])
        num_mipmaps = len(mipmap_levels)

        status_layout.addRow("Levels:", QLabel(str(num_mipmaps)))

        if num_mipmaps > 0:
            # Show level details
            details = QTextEdit()
            details.setReadOnly(True)
            details.setMaximumHeight(200)

            details_text = ""
            for level in mipmap_levels:
                level_num = level.get('level', 0)
                w = level.get('width', 0)
                h = level.get('height', 0)
                size = level.get('compressed_size', 0)
                size_kb = size / 1024

                details_text += f"Level {level_num}: {w}x{h} ({size_kb:.1f} KB)\n"

            details.setText(details_text)
            status_layout.addRow("Details:", details)
        else:
            no_mipmap_label = QLabel("No mipmaps generated")
            no_mipmap_label.setStyleSheet("color: orange;")
            status_layout.addRow("Status:", no_mipmap_label)

        layout.addWidget(status_group)

        # Mipmap actions group
        actions_group = QGroupBox("Mipmap Actions")
        actions_layout = QVBoxLayout(actions_group)

        generate_btn = QPushButton("Generate Mipmaps")
        generate_btn.clicked.connect(self._generate_mipmaps)
        actions_layout.addWidget(generate_btn)

        if num_mipmaps > 0:
            view_btn = QPushButton("View All Levels")
            view_btn.clicked.connect(self._view_mipmaps)
            actions_layout.addWidget(view_btn)

            export_btn = QPushButton("Export All Levels")
            export_btn.clicked.connect(self._export_mipmaps)
            actions_layout.addWidget(export_btn)

        layout.addWidget(actions_group)

        layout.addStretch()
        return tab


    def _create_advanced_tab(self): #vers 1
        """Create advanced settings tab"""
        tab = QWidget()
        layout = QVBoxLayout(tab)
        layout.setContentsMargins(10, 10, 10, 10)

        # Technical info group
        tech_group = QGroupBox("Technical Information")
        tech_layout = QFormLayout(tech_group)

        # RenderWare version
        rw_version = self.texture_data.get('rw_version', 'Unknown')
        tech_layout.addRow("RW Version:", QLabel(str(rw_version)))

        # Platform
        platform = self.texture_data.get('platform', 'PC')
        tech_layout.addRow("Platform:", QLabel(platform))

        # Texture flags
        flags = self.texture_data.get('flags', 0)
        tech_layout.addRow("Flags:", QLabel(f"0x{flags:04X}"))

        layout.addWidget(tech_group)

        # Memory stats group
        mem_group = QGroupBox("Memory Statistics")
        mem_layout = QFormLayout(mem_group)

        width = self.texture_data.get('width', 0)
        height = self.texture_data.get('height', 0)

        # Uncompressed size
        uncompressed = width * height * 4
        mem_layout.addRow("Uncompressed:", QLabel(f"{uncompressed:,} bytes"))

        # Current size
        rgba_data = self.texture_data.get('rgba_data', b'')
        current_size = len(rgba_data) if rgba_data else 0
        mem_layout.addRow("Current:", QLabel(f"{current_size:,} bytes"))

        # With mipmaps
        mipmap_levels = self.texture_data.get('mipmap_levels', [])
        total_mipmap_size = sum(level.get('compressed_size', 0) for level in mipmap_levels)
        mem_layout.addRow("With Mipmaps:", QLabel(f"{total_mipmap_size:,} bytes"))

        layout.addWidget(mem_group)

        layout.addStretch()
        return tab


    def _apply_changes(self): #vers 1
        """Apply changes to texture"""
        # Update name
        new_name = self.name_edit.text().strip()
        if new_name and new_name != self.original_texture.get('name', ''):
            self.original_texture['name'] = new_name
            self.changes_made = True

        # Update alpha name if exists
        if self.alpha_name_edit:
            new_alpha_name = self.alpha_name_edit.text().strip()
            if new_alpha_name and new_alpha_name != self.original_texture.get('alpha_name', ''):
                self.original_texture['alpha_name'] = new_alpha_name
                self.changes_made = True

        # Update format if changed
        format_map = ['DXT1', 'DXT3', 'DXT5', 'ARGB8888', 'RGB888']
        new_format = format_map[self.format_combo.currentIndex()]

        if new_format != self.original_texture.get('format', ''):
            # Mark for format conversion
            self.original_texture['target_format'] = new_format
            self.changes_made = True

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"ℹ️ Format change queued: {new_format}")

        if self.changes_made:
            # Notify parent workshop
            if hasattr(self.parent_workshop, '_mark_as_modified'):
                self.parent_workshop._mark_as_modified()

            if hasattr(self.parent_workshop, '_reload_texture_table'):
                self.parent_workshop._reload_texture_table()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("Properties updated")


    def _ok_clicked(self): #vers 1
        """Apply changes and close"""
        self._apply_changes()
        self.accept()


    def _generate_mipmaps(self): #vers 2
        """Generate mipmaps with user-selected depth"""
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QLabel, QSlider, QPushButton, QHBoxLayout

        # Calculate possible mipmap levels
        width = self.texture_data.get('width', 256)
        height = self.texture_data.get('height', 256)
        max_dimension = max(width, height)

        # Calculate how many levels possible (down to 1x1)
        import math
        max_levels = int(math.log2(max_dimension)) + 1

        # Create selection dialog
        dialog = QDialog(self)
        dialog.setWindowTitle("Generate Mipmaps")
        dialog.setModal(True)
        dialog.resize(400, 250)

        layout = QVBoxLayout(dialog)

        # Header info
        header = QLabel(f"Texture Size: {width}x{height}\nSelect minimum mipmap size:")
        header.setStyleSheet("font-weight: bold; padding: 10px;")
        layout.addWidget(header)

        # Slider with level preview
        slider_layout = QVBoxLayout()

        self.mipmap_slider = QSlider(Qt.Orientation.Horizontal)
        self.mipmap_slider.setMinimum(0)  # Down to 1x1
        self.mipmap_slider.setMaximum(max_levels - 1)
        self.mipmap_slider.setValue(max_levels - 6)  # Default to ~32x32
        self.mipmap_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.mipmap_slider.setTickInterval(1)

        # Preview label showing dimensions at each level
        self.mipmap_preview = QLabel()
        self.mipmap_preview.setStyleSheet("font-size: 14px; padding: 10px; background: palette(base); border-radius: 3px;")
        self.mipmap_preview.setAlignment(Qt.AlignmentFlag.AlignCenter)


        def update_preview(value):  #vers 1
            # Calculate dimensions at this level
            levels_from_top = max_levels - 1 - value
            min_w = max(1, width >> levels_from_top)
            min_h = max(1, height >> levels_from_top)
            num_levels = max_levels - value

            preview_text = f"Minimum Size: {min_w}x{min_h}\n"
            preview_text += f"Total Levels: {num_levels}\n\n"
            preview_text += f"Levels: {width}x{height}"

            # Show a few intermediate levels
            current_w, current_h = width, height
            shown = 1
            for i in range(1, num_levels):
                current_w = max(1, current_w // 2)
                current_h = max(1, current_h // 2)
                if shown < 4 or i == num_levels - 1:  # Show first 3 and last
                    preview_text += f" -> {current_w}x{current_h}"
                    shown += 1
                elif shown == 4:
                    preview_text += " -> ..."
                    shown += 1

            self.mipmap_preview.setText(preview_text)

        self.mipmap_slider.valueChanged.connect(update_preview)
        update_preview(self.mipmap_slider.value())

        slider_layout.addWidget(QLabel("More Levels <-  ->  Fewer Levels"))
        slider_layout.addWidget(self.mipmap_slider)
        slider_layout.addWidget(self.mipmap_preview)

        layout.addLayout(slider_layout)

        # Buttons
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        generate_btn = QPushButton("Generate")
        generate_btn.clicked.connect(lambda: self._do_generate_mipmaps(dialog, max_levels))
        button_layout.addWidget(generate_btn)

        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(dialog.reject)
        button_layout.addWidget(cancel_btn)

        layout.addLayout(button_layout)

        dialog.exec()


    def _do_generate_mipmaps(self, dialog, max_levels): #vers 1
        """Actually generate the mipmaps with selected depth"""
        slider_value = self.mipmap_slider.value()
        num_levels = max_levels - slider_value

        dialog.accept()

        if hasattr(self.parent_workshop, '_auto_generate_mipmaps_to_level'):
            # Use enhanced version with level control
            old_selection = self.parent_workshop.selected_texture
            self.parent_workshop.selected_texture = self.original_texture

            self.parent_workshop._auto_generate_mipmaps_to_level(num_levels)

            self.parent_workshop.selected_texture = old_selection
        elif hasattr(self.parent_workshop, '_auto_generate_mipmaps'):
            # Fallback to basic version
            old_selection = self.parent_workshop.selected_texture
            self.parent_workshop.selected_texture = self.original_texture

            self.parent_workshop._auto_generate_mipmaps()

            self.parent_workshop.selected_texture = old_selection

        # Refresh dialog
        self.close()
        new_dialog = TexturePropertiesDialog(self.parent_workshop, self.original_texture, self.main_window)
        new_dialog.exec()


    def _view_mipmaps(self): #vers 1
        """Open mipmap manager"""
        if hasattr(self.parent_workshop, '_open_mipmap_manager'):
            old_selection = self.parent_workshop.selected_texture
            self.parent_workshop.selected_texture = self.original_texture

            self.parent_workshop._open_mipmap_manager()

            self.parent_workshop.selected_texture = old_selection


    def _export_mipmaps(self): #vers 2
        """Export all mipmap levels"""
        from PyQt6.QtWidgets import QFileDialog
        import os

        output_dir = QFileDialog.getExistingDirectory(self, "Select Output Directory", _work_dir(self))
        if not output_dir:
            return

        mipmap_levels = self.texture_data.get('mipmap_levels', [])
        name = self.texture_data.get('name', 'texture')

        exported = 0
        for level in mipmap_levels:
            level_num = level.get('level', 0)
            rgba_data = level.get('rgba_data')
            width = level.get('width', 0)
            height = level.get('height', 0)

            if rgba_data and width > 0:
                file_path = os.path.join(output_dir, f"{name}_level{level_num}.png")
                if hasattr(self.parent_workshop, '_save_texture_png'):
                    self.parent_workshop._save_texture_png(rgba_data, width, height, file_path)
                    exported += 1

        from PyQt6.QtWidgets import QMessageBox
        QMessageBox.information(self, "Export Complete", f"Exported {exported} mipmap levels")


class MipmapManagerWindow(QWidget): #vers 2
    """Mipmap Manager - Modern card-based design matching mockup"""

    def __init__(self, parent, texture_data, main_window=None):  #vers 1
        super().__init__(parent)
        self.parent_workshop = parent
        self.texture_data = texture_data
        self.main_window = main_window
        self.modified_levels = {}  # Track modified levels

        texture_name = texture_data.get('name', 'Unknown')
        width = texture_data.get('width', 0)
        height = texture_data.get('height', 0)
        fmt = texture_data.get('format', 'Unknown')

        self.setWindowTitle(f"Mipmap Manager - {texture_name}")
        self.resize(1080, 700)  # 20% wider (900 * 1.2 = 1080)

        # Frameless window with custom styling
        self.setWindowFlags(Qt.WindowType.FramelessWindowHint)

        # Corner resize variables
        self.dragging = False
        self.drag_position = None
        self.resizing = False
        self.resize_corner = None
        self.corner_size = 20
        self.hover_corner = None

        self.setup_ui()

        # Enable mouse tracking for hover effects
        self.setMouseTracking(True)


    def setup_ui(self): #vers 2
        """Setup modern UI matching mockup"""
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # Toolbar with Apply/Close buttons
        toolbar = self._create_toolbar()
        layout.addWidget(toolbar)

        # Scrollable content area
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        content_widget = QWidget()
        self.content_layout = QVBoxLayout(content_widget)
        self.content_layout.setContentsMargins(15, 15, 15, 15)
        self.content_layout.setSpacing(15)

        # Create level cards
        mipmap_levels = self.texture_data.get('mipmap_levels', [])
        for level_data in mipmap_levels:
            card = self._create_level_card(level_data)
            self.content_layout.addWidget(card)

        self.content_layout.addStretch()
        scroll.setWidget(content_widget)
        layout.addWidget(scroll)

        # Bottom status bar
        bottom_bar = self._create_bottom_bar()
        layout.addWidget(bottom_bar)
        # Title bar
        title_bar = self._create_title_bar()
        layout.addWidget(title_bar)


    def _create_title_bar(self): #vers 2
        """Create custom title bar"""
        title_bar = QFrame()
        title_bar.setFrameStyle(QFrame.Shape.StyledPanel)
        title_bar.setFixedHeight(40)

        layout = QHBoxLayout(title_bar)
        layout.setContentsMargins(15, 0, 15, 0)

        # Title text
        texture_name = self.texture_data.get('name', 'Unknown')
        width = self.texture_data.get('width', 0)
        height = self.texture_data.get('height', 0)
        fmt = self.texture_data.get('format', 'Unknown')

        title_label = QLabel(f"Mipmap Manager - {texture_name} ({width}x{height}, {fmt})")
        layout.addWidget(title_label)

        layout.addStretch()

        # Drag handle
        drag_btn = QPushButton()
        drag_btn.setIcon(SVGIconFactory.hamburger_menu_icon())
        drag_btn.setFixedSize(30, 30)
        drag_btn.setCursor(Qt.CursorShape.SizeAllCursor)
        layout.addWidget(drag_btn)

        return title_bar


    def _create_toolbar(self): #vers 3
        """Create toolbar with action buttons AND Apply/Close"""
        toolbar = QFrame()
        toolbar.setFrameStyle(QFrame.Shape.StyledPanel)
        toolbar.setFixedHeight(50)

        layout = QHBoxLayout(toolbar)
        layout.setContentsMargins(10, 0, 10, 0)
        layout.setSpacing(10)

        # Left side - Action buttons
        autogen_btn = QPushButton("Auto-Generate")
        autogen_btn.setIcon(SVGIconFactory.reset_icon())
        autogen_btn.setToolTip("Generate all mipmap levels")
        autogen_btn.clicked.connect(self._auto_generate_mipmaps)
        layout.addWidget(autogen_btn)

        export_all_btn = QPushButton("Export All")
        export_all_btn.setIcon(SVGIconFactory.export_icon())
        export_all_btn.setToolTip("Export all levels as PNG")
        export_all_btn.clicked.connect(self._export_all_levels)
        layout.addWidget(export_all_btn)

        import_all_btn = QPushButton("Import All")
        import_all_btn.setIcon(SVGIconFactory.import_icon())
        import_all_btn.setToolTip("Import levels from PNG files")
        import_all_btn.clicked.connect(self._import_all_levels)
        layout.addWidget(import_all_btn)

        clear_btn = QPushButton("Clear All")
        clear_btn.setIcon(SVGIconFactory.trash_icon())
        clear_btn.setToolTip("Remove all mipmap levels except Level 0")
        clear_btn.clicked.connect(self._clear_all_levels)
        layout.addWidget(clear_btn)

        layout.addStretch()

        # Right side - Apply/Close buttons
        apply_btn = QPushButton("Apply Changes")
        apply_btn.setIcon(SVGIconFactory.check_icon())
        apply_btn.clicked.connect(self._apply_changes)
        layout.addWidget(apply_btn)

        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.close)
        layout.addWidget(close_btn)

        return toolbar


    def _create_level_card(self, level_data): #vers 3
        """Create modern level card matching mockup"""
        card = QFrame()
        card.setFrameStyle(QFrame.Shape.StyledPanel)
        card.setMinimumHeight(140)

        layout = QHBoxLayout(card)
        layout.setContentsMargins(15, 15, 15, 15)
        layout.setSpacing(15)

        # Preview thumbnail
        preview_widget = self._create_preview_widget(level_data)
        layout.addWidget(preview_widget)

        # Level info section
        info_section = self._create_info_section(level_data)
        layout.addWidget(info_section, stretch=1)

        # Action buttons
        action_section = self._create_action_section(level_data)
        layout.addWidget(action_section)

        return card


    def _create_preview_widget(self, level_data): #vers 2
        """Create preview thumbnail with checkerboard"""
        level_num = level_data.get('level', 0)
        width = level_data.get('width', 0)
        height = level_data.get('height', 0)
        rgba_data = level_data.get('rgba_data')

        # Scale preview size based on level
        preview_size = max(45, 120 - (level_num * 15))

        preview = QLabel()
        preview.setFixedSize(preview_size, preview_size)
        preview.setAlignment(Qt.AlignmentFlag.AlignCenter)

        if rgba_data and width > 0:
            try:
                image = QImage(rgba_data, width, height, width * 4, QImage.Format.Format_RGBA8888)
                if not image.isNull():
                    pixmap = QPixmap.fromImage(image)
                    scaled_pixmap = pixmap.scaled(
                        preview_size - 10, preview_size - 10,
                        Qt.AspectRatioMode.KeepAspectRatio,
                        Qt.TransformationMode.SmoothTransformation
                    )
                    preview.setPixmap(scaled_pixmap)
            except:
                preview.setText("No preview")
        else:
            preview.setText("No preview")

        return preview


    def _create_info_section(self, level_data): #vers 2
        """Create info section with stats grid"""
        info_widget = QWidget()
        layout = QVBoxLayout(info_widget)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(10)

        # Header with level number and dimensions
        header_layout = QHBoxLayout()

        level_num = level_data.get('level', 0)
        level_badge = QLabel(f"Level {level_num}")
        header_layout.addWidget(level_badge)

        width = level_data.get('width', 0)
        height = level_data.get('height', 0)
        dim_label = QLabel(f"{width} x {height}")
        header_layout.addWidget(dim_label)

        # Main texture indicator
        if level_num == 0:
            main_badge = QLabel("● Main Texture")
            main_badge.setStyleSheet("color: #4caf50; font-size: 12px;")
            header_layout.addWidget(main_badge)

        header_layout.addStretch()
        layout.addLayout(header_layout)

        # Stats grid
        stats_grid = self._create_stats_grid(level_data)
        layout.addWidget(stats_grid)

        return info_widget


    def _create_stats_grid(self, level_data): #vers 1
        """Create stats grid"""
        grid_widget = QWidget()
        grid_layout = QHBoxLayout(grid_widget)
        grid_layout.setContentsMargins(0, 0, 0, 0)
        grid_layout.setSpacing(8)

        fmt = level_data.get('format', self.texture_data.get('format', 'Unknown'))
        size = level_data.get('compressed_size', 0)
        size_kb = size / 1024

        # Format stat
        format_stat = self._create_stat_box("Format:", fmt)
        grid_layout.addWidget(format_stat)

        # Size stat
        size_stat = self._create_stat_box("Size:", f"{size_kb:.1f} KB")
        grid_layout.addWidget(size_stat)

        # Compression stat
        if 'DXT' in fmt:
            ratio = "4:1" if 'DXT5' in fmt or 'DXT3' in fmt else "6:1"
            comp_stat = self._create_stat_box("Compression:", ratio)
        else:
            comp_stat = self._create_stat_box("Compression:", "None")
        grid_layout.addWidget(comp_stat)

        # Status stat
        is_modified = level_data.get('level', 0) in self.modified_levels
        status_text = "Modified" if is_modified else "Valid"
        status_color = "#ff9800" if is_modified else "#4caf50"
        status_stat = self._create_stat_box("Status:", status_text, status_color)
        grid_layout.addWidget(status_stat)

        return grid_widget


    def _create_stat_box(self, label, value, value_color="#e0e0e0"): #vers 2
        """Create individual stat box"""
        stat = QFrame()

        layout = QHBoxLayout(stat)
        layout.setContentsMargins(8, 4, 8, 4)

        label_widget = QLabel(label)
        layout.addWidget(label_widget)

        value_widget = QLabel(value)
        value_widget.setStyleSheet(f"color: {value_color}; font-weight: bold; font-size: 12px;")
        layout.addWidget(value_widget)

        return stat


    def _create_action_section(self, level_data): #vers 2
        """Create action buttons section"""
        action_widget = QWidget()
        layout = QVBoxLayout(action_widget)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(5)

        level_num = level_data.get('level', 0)

        # Export button
        export_btn = QPushButton("Export")
        export_btn.setIcon(SVGIconFactory.export_icon())
        export_btn.clicked.connect(lambda: self._export_level(level_num))
        layout.addWidget(export_btn)

        # Import button
        import_btn = QPushButton("Import")
        import_btn.setIcon(SVGIconFactory.import_icon())
        import_btn.clicked.connect(lambda: self._import_level(level_num))
        layout.addWidget(import_btn)

        # Delete button (not for level 0) or Edit button (for level 0)
        if level_num == 0:
            edit_btn = QPushButton("Edit")
            edit_btn.setIcon(SVGIconFactory.edit_icon())
            edit_btn.clicked.connect(self._edit_main_texture)
            layout.addWidget(edit_btn)
        else:
            delete_btn = QPushButton("Delete")
            delete_btn.setIcon(SVGIconFactory.trash_icon())
            delete_btn.clicked.connect(lambda: self._delete_level(level_num))
            layout.addWidget(delete_btn)

        return action_widget


    def _create_bottom_bar(self): #vers 2
        """Create bottom status bar"""
        bottom_bar = QFrame()
        bottom_bar.setFrameStyle(QFrame.Shape.StyledPanel)
        bottom_bar.setFixedHeight(45)

        layout = QHBoxLayout(bottom_bar)
        layout.setContentsMargins(15, 0, 15, 0)

        # Left side - Stats
        mipmap_levels = self.texture_data.get('mipmap_levels', [])
        num_levels = len(mipmap_levels)
        total_size = sum(level.get('compressed_size', 0) for level in mipmap_levels)
        total_size_kb = total_size / 1024

        stats_label = QLabel(f"Total Levels: {num_levels} | Total Size: {total_size_kb:.1f} KB")
        layout.addWidget(stats_label)

        # Modified badge if there are changes
        if self.modified_levels:
            modified_badge = QLabel("● Modified")
            modified_badge.setStyleSheet("""
                QLabel {
                    background: #ff6b35;
                    color: white;
                    padding: 4px 8px;
                    border-radius: 3px;
                    font-size: 11px;
                    font-weight: bold;
                    margin-left: 10px;
                }
            """)
            layout.addWidget(modified_badge)

        layout.addStretch()

        return bottom_bar


    def mousePressEvent(self, event): #vers 1
        """Enable window dragging from title bar"""
        if event.button() == Qt.MouseButton.LeftButton and event.pos().y() < 40:
            self.dragging = True
            self.drag_position = event.globalPosition().toPoint() - self.frameGeometry().topLeft()

    def mouseMoveEvent(self, event): #vers 1
        """Handle window dragging"""
        if self.dragging and event.buttons() == Qt.MouseButton.LeftButton:
            self.move(event.globalPosition().toPoint() - self.drag_position)


    def mouseReleaseEvent(self, event): #vers 1
        """Stop dragging"""
        self.dragging = False


    def _auto_generate_mipmaps(self): #vers 1
        """Auto-generate all mipmap levels"""
        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message("Auto-generating mipmaps...")
        # Call parent's generate method
        if hasattr(self.parent_workshop, '_auto_generate_mipmaps'):
            old_selection = self.parent_workshop.selected_texture
            self.parent_workshop.selected_texture = self.texture_data
            self.parent_workshop._auto_generate_mipmaps()
            self.parent_workshop.selected_texture = old_selection
            # Refresh window
            self.close()
            new_window = MipmapManagerWindow(self.parent_workshop, self.texture_data, self.main_window)
            new_window.show()


    def _export_all_levels(self): #vers 3
        """Export all mipmap levels as PNG files to a chosen folder."""
        import os
        from PyQt6.QtWidgets import QFileDialog, QMessageBox
        from PIL import Image

        levels = self.texture_data.get('mipmap_levels', [])
        if not levels:
            QMessageBox.warning(self, "No Mipmaps", "No mipmap levels found.")
            return

        output_dir = QFileDialog.getExistingDirectory(self, "Select Export Directory", _work_dir(self))
        if not output_dir:
            return

        tex_name = self.texture_data.get('name', 'texture')
        exported = 0
        for level_data in levels:
            lvl   = level_data.get('level', 0)
            w, h  = level_data.get('width', 1), level_data.get('height', 1)
            rgba  = level_data.get('rgba_data')
            if not rgba:
                continue
            try:
                img  = Image.frombytes('RGBA', (w, h), rgba)
                path_out = os.path.join(output_dir, f"{tex_name}_mip{lvl}_{w}x{h}.png")
                img.save(path_out)
                exported += 1
            except Exception as e:
                print(f"Mipmap {lvl} export error: {e}")

        msg = f"Exported {exported} mipmap level(s) to:\n{output_dir}"
        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message(msg)
        QMessageBox.information(self, "Export Complete", msg)


    #Keep, needs work
    def _import_all_levels(self): #vers 5
        """Import mipmap levels from PNG files — filename must contain _mipN_."""
        import os, re
        from PyQt6.QtWidgets import QFileDialog, QMessageBox
        from PIL import Image

        paths, _ = QFileDialog.getOpenFileNames(
            self, "Select Mipmap PNG files", _work_dir(self),
            "PNG Images (*.png);;All Files (*)")
        if not paths:
            return

        self._before_change("Import mipmap levels")
        imported = 0
        for file_path in sorted(paths):
            fname = os.path.basename(file_path)
            m = re.search(r'_mip(\d+)_', fname)
            level_num = int(m.group(1)) if m else imported

            try:
                img  = Image.open(file_path).convert('RGBA')
                rgba = img.tobytes()
                levels = self.texture_data.setdefault('mipmap_levels', [])
                # Replace or append
                existing = next((l for l in levels if l.get('level') == level_num), None)
                if existing:
                    existing['rgba_data'] = rgba
                    existing['width']     = img.width
                    existing['height']    = img.height
                else:
                    levels.append({'level': level_num, 'width': img.width,
                                   'height': img.height, 'rgba_data': rgba})
                self.modified_levels[level_num] = True
                imported += 1
            except Exception as e:
                print(f"Mipmap import {file_path}: {e}")

        self.texture_data['mipmaps'] = len(self.texture_data.get('mipmap_levels', []))
        if imported and hasattr(self.parent_workshop, '_mark_as_modified'):
            self.parent_workshop._mark_as_modified()
        msg = f"Imported {imported} mipmap level(s)."
        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message(msg)
        QMessageBox.information(self, "Import Complete", msg)
        # Reopen to refresh
        self.close()
        MipmapManagerWindow(self.parent_workshop, self.texture_data, self.main_window).show()


    def _clear_all_levels(self): #vers 2
        """Clear all mipmap levels except Level 0"""
        reply = QMessageBox.question(
            self, "Clear Mipmaps",
            "Remove all mipmap levels except Level 0?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
        )

        if reply == QMessageBox.StandardButton.Yes:
            if 'mipmap_levels' in self.texture_data:
                # Keep only level 0
                level_0 = next((l for l in self.texture_data['mipmap_levels'] if l.get('level') == 0), None)
                if level_0:
                    self._before_change("Clear mipmaps")
                    self.texture_data['mipmap_levels'] = [level_0]
                    self.texture_data['mipmaps'] = 1
                    self.parent_workshop._mark_as_modified()
                    self.close()
                    new_window = MipmapManagerWindow(self.parent_workshop, self.texture_data, self.main_window)
                    new_window.show()


    def _level(self, level_num): #vers 1
        """Mipmap level dict, or None."""
        return next((l for l in self.texture_data.get('mipmap_levels', [])
                     if l.get('level') == level_num), None)

    def _before_change(self, action: str): #vers 1
        """Undo step in the parent workshop before a level edit."""
        if hasattr(self.parent_workshop, '_save_undo_state'):
            self.parent_workshop._save_undo_state(action)

    def _export_level(self, level_num): #vers 3
        """Save one mipmap level as PNG."""
        from PyQt6.QtWidgets import QFileDialog
        from PIL import Image
        lv = self._level(level_num)
        if not lv or not lv.get('rgba_data'):
            QMessageBox.warning(self, "Export Level", f"Level {level_num} has no image data")
            return
        name = f"{self.texture_data.get('name', 'texture')}_mip{level_num}_.png"
        path, _ = QFileDialog.getSaveFileName(self, "Export Mipmap Level", os.path.join(_work_dir(self), name), "PNG Images (*.png)")
        if not path:
            return
        Image.frombytes('RGBA', (lv['width'], lv['height']), bytes(lv['rgba_data'])).save(path)
        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message(f"Exported level {level_num}: {path}")

    def _import_level(self, level_num): #vers 3
        """Replace one mipmap level from an image (scaled to the level size)."""
        from PyQt6.QtWidgets import QFileDialog
        from PIL import Image
        lv = self._level(level_num)
        if not lv:
            QMessageBox.warning(self, "Import Level", f"Level {level_num} doesn't exist")
            return
        path, _ = QFileDialog.getOpenFileName(self, "Import Mipmap Level", _work_dir(self),
                                              "Images (*.png *.bmp *.tga *.jpg *.jpeg)")
        if not path:
            return
        img = Image.open(path).convert('RGBA')
        if img.size != (lv['width'], lv['height']):
            img = img.resize((lv['width'], lv['height']), Image.Resampling.LANCZOS)
        self._before_change(f"Import mipmap level {level_num}")
        lv['rgba_data'] = img.tobytes()
        lv['compressed_data'] = None
        if level_num == 0:
            self.texture_data['rgba_data'] = lv['rgba_data']
        self.modified_levels[level_num] = True
        self.parent_workshop._mark_as_modified()
        self.close()
        MipmapManagerWindow(self.parent_workshop, self.texture_data, self.main_window).show()


    def _delete_level(self, level_num): #vers 2
        """Delete mipmap level"""
        reply = QMessageBox.question(
            self, "Delete Level",
            f"Delete Level {level_num}?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
        )

        if reply == QMessageBox.StandardButton.Yes:
            self._before_change(f"Delete mipmap level {level_num}")
            self.texture_data['mipmap_levels'] = [
                l for l in self.texture_data.get('mipmap_levels', [])
                if l.get('level') != level_num
            ]
            self.parent_workshop._mark_as_modified()
            self.close()
            new_window = MipmapManagerWindow(self.parent_workshop, self.texture_data, self.main_window)
            new_window.show()


    def _edit_main_texture(self): #vers 2
        """Select this texture in the parent workshop for editing."""
        try:
            pw = self.parent_workshop
            tex = self.texture_data
            if hasattr(pw, 'texture_list') and tex in pw.texture_list:
                row = pw.texture_list.index(tex)
                if hasattr(pw, 'texture_table'):
                    pw.texture_table.selectRow(row)
                pw.selected_texture = tex
                if hasattr(pw, '_update_texture_info'):
                    pw._update_texture_info(tex)
            pw.raise_()
            pw.activateWindow()
            self.close()
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Editing: {tex.get('name', 'texture')}")
        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Edit error: {e}")


    def _apply_changes(self): #vers 1
        """Apply all changes and close"""
        if self.modified_levels:
            # Update parent workshop
            if hasattr(self.parent_workshop, '_mark_as_modified'):
                self.parent_workshop._mark_as_modified()

            if hasattr(self.parent_workshop, '_reload_texture_table'):
                self.parent_workshop._reload_texture_table()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("Mipmap changes applied")

        self.close()


    # --- DXT1 and DXT5 encoders (pure Python) ---


class BumpmapManagerWindow(QWidget): #vers 1
    """Bumpmap Manager - Modern design matching Mipmap Manager"""

    def __init__(self, parent, texture_data, main_window=None): #vers 2
        """Initialize with 30% smaller height"""
        super().__init__(parent)
        self.parent_workshop = parent
        self.texture_data = texture_data
        self.main_window = main_window
        self.modified = False

        from PyQt6.QtGui import QFont
        self.panel_font = QFont('Segoe UI', 10)
        self.button_font = QFont('Segoe UI', 9)
        self.title_font = QFont('Segoe UI', 10)

        texture_name = texture_data.get('name', 'Unknown')
        width = texture_data.get('width', 0)
        height = texture_data.get('height', 0)

        self.setWindowTitle(f"Bumpmap Manager - {texture_name}")
        self.resize(900, 600)  # Changed from 650 to 455 (30% smaller)

        # Frameless window with custom styling
        self.setWindowFlags(Qt.WindowType.FramelessWindowHint)

        # Corner resize variables
        self.dragging = False
        self.drag_position = None
        self.resizing = False
        self.resize_corner = None
        self.corner_size = 20
        self.hover_corner = None
        self.current_txd_path = None

        self.setup_ui()

        # Enable mouse tracking for hover effects
        self.setMouseTracking(True)


    def setup_ui(self): #vers 9
        """Setup modern UI - Now includes reflection maps"""
        from PyQt6.QtWidgets import (QWidget, QVBoxLayout, QHBoxLayout, QLabel,
                                    QPushButton, QGroupBox, QSplitter, QFrame)
        from PyQt6.QtCore import Qt

        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(0, 0, 0, 0)
        main_layout.setSpacing(0)

        # Create custom title bar
        title_bar = self._create_title_bar()
        main_layout.addWidget(title_bar)

        # Main content area - Use splitter for resizable panels
        content = QWidget()
        content_layout = QHBoxLayout(content)
        content_layout.setContentsMargins(5, 5, 5, 5)
        content_layout.setSpacing(5)

        # Create splitter for panels
        from apps.methods.grip_splitter import GripSplitter
        splitter = GripSplitter(Qt.Orientation.Horizontal)

        # Left: Texture info and preview
        left_panel = self._create_left_panel()
        splitter.addWidget(left_panel)

        # Middle: Bumpmap controls
        middle_panel = self._create_middle_panel()
        splitter.addWidget(middle_panel)

        # Right: Bumpmap preview
        right_panel = self._create_right_panel()
        splitter.addWidget(right_panel)

        # Fourth: Reflection maps panel
        reflection_panel = self._create_reflection_panel()
        splitter.addWidget(reflection_panel)

        # Set splitter sizes (25% each for 4 panels)
        splitter.setSizes([250, 250, 250, 250])

        content_layout.addWidget(splitter)
        main_layout.addWidget(content)

        # REMOVE THIS SECTION - No bottom button bar needed
        # main_layout.addWidget(button_bar)

        # Set dark theme

        # Update previews AFTER all widgets are created
        if hasattr(self, 'bumpmap_preview'):
            self._update_bumpmap_preview()
        if 'reflection_map' in self.texture_data and hasattr(self, 'reflection_preview'):
            self._update_reflection_previews()


    def _create_left_panel(self): #vers 7
        """Create left panel - title on far right"""
        panel = QGroupBox("Main Texture    .")
        # Style to move title to the right

        layout = QVBoxLayout(panel)
        layout.setSpacing(10)

        # Info container with proper spacing
        info_container = QWidget()
        info_container.setFixedHeight(85)
        info_layout = QVBoxLayout(info_container)
        info_layout.setContentsMargins(5, 5, 5, 5)
        info_layout.setSpacing(5)

        # Name
        name_label = QLabel(f"Name: {self.texture_data.get('name', 'Unknown')}")
        name_label.setWordWrap(False)
        info_layout.addWidget(name_label)

        # Size
        width = self.texture_data.get('width', 0)
        height = self.texture_data.get('height', 0)
        size_label = QLabel(f"Size: {width} x {height}")
        info_layout.addWidget(size_label)

        # Format
        fmt = self.texture_data.get('format', 'Unknown')
        format_label = QLabel(f"Format: {fmt}")
        info_layout.addWidget(format_label)

        info_layout.addStretch()

        layout.addWidget(info_container)

        # Main texture preview
        preview_label = QLabel()
        preview_label.setMinimumHeight(250)
        preview_label.setAlignment(Qt.AlignmentFlag.AlignCenter)

        # Load texture preview
        rgba_data = self.texture_data.get('rgba_data')
        if rgba_data and width > 0:
            image = QImage(rgba_data, width, height, width * 4, QImage.Format.Format_RGBA8888)
            pixmap = QPixmap.fromImage(image)
            preview_label.setPixmap(
                pixmap.scaled(250, 250,
                            Qt.AspectRatioMode.KeepAspectRatio,
                            Qt.TransformationMode.SmoothTransformation)
            )
        else:
            preview_label.setText("No texture data")

        layout.addWidget(preview_label)
        return panel



    def _create_middle_panel(self): #vers 3
        """Create middle panel with bumpmap controls"""
        from PyQt6.QtWidgets import QWidget, QVBoxLayout, QPushButton, QGroupBox, QLabel

        panel = QGroupBox("Controls    .")
        # Match your styling

        layout = QVBoxLayout(panel)
        layout.setSpacing(10)

        # Info text
        info_label = QLabel(
            "Bumpmaps add surface detail.\n"
            "Generate from texture or import."
        )
        info_label.setFont(self.panel_font)
        info_label.setWordWrap(True)
        layout.addWidget(info_label)

        # Generate button (F9)
        generate_btn = QPushButton("Generate from Texture (F9)")
        generate_btn.setFont(self.button_font)
        generate_btn.clicked.connect(self._generate_bumpmap)
        layout.addWidget(generate_btn)

        # Import button (F10)
        import_btn = QPushButton("Import from File (F10)")
        import_btn.setFont(self.button_font)
        import_btn.clicked.connect(self._import_bumpmap)
        layout.addWidget(import_btn)

        # Export button
        export_btn = QPushButton("Export to File")
        export_btn.setFont(self.button_font)
        export_btn.clicked.connect(self._export_bumpmap)
        export_btn.setEnabled(self._has_bumpmap())
        layout.addWidget(export_btn)

        # Delete button (F11)
        delete_btn = QPushButton("Delete Bumpmap (F11)")
        delete_btn.setFont(self.button_font)
        delete_btn.clicked.connect(self._delete_bumpmap)
        delete_btn.setEnabled(self._has_bumpmap())
        layout.addWidget(delete_btn)

        layout.addStretch()

        # Type info
        type_info = QLabel(
            "Types:\n"
            "• Grayscale Height Map\n"
            "• RGB Normal Map\n"
            "• Both (Height + Normal)"
        )
        type_info.setFont(self.panel_font)
        type_info.setWordWrap(True)
        layout.addWidget(type_info)
        return panel

    def _create_right_panel(self): #vers 7
        """Create right panel - title on far right"""
        panel = QGroupBox("Bumpmap    .")
        # Style to move title to the right

        layout = QVBoxLayout(panel)
        layout.setSpacing(10)

        # Info container with same height as left
        info_container = QWidget()
        info_container.setFixedHeight(85)
        info_layout = QVBoxLayout(info_container)
        info_layout.setContentsMargins(1, 1, 1, 1)
        info_layout.setSpacing(5)

        # Status
        has_bumpmap = self._has_bumpmap()
        status_label = QLabel(f"Status: {'Present' if has_bumpmap else 'Not present'}")
        status_label.setFont(self.panel_font)
        status_label.setStyleSheet(
        "line-height: 1.4; color: #4CAF50;" if has_bumpmap
        else "line-height: 1.4; color: #888;"
        )
        info_layout.addWidget(status_label)

        # Type
        type_label = QLabel("Type: Environment map (Normal map)")
        type_label.setFont(self.panel_font)
        info_layout.addWidget(type_label)

        info_layout.addStretch()

        layout.addWidget(info_container)

        # Bumpmap preview
        self.bumpmap_preview = QLabel()
        self.bumpmap_preview.setMinimumHeight(250)
        self.bumpmap_preview.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.bumpmap_preview.setFont(self.panel_font)

        # Load bumpmap preview if available
        if has_bumpmap:
            self._update_bumpmap_preview()
        else:
            self.bumpmap_preview.setText("No bumpmap data\n\nPress F9 or use Edit -> Generate Bumpmap")

        layout.addWidget(self.bumpmap_preview)

        return panel


    def _create_title_bar(self): #vers 9
        """Create title bar with 14px button text"""
        title_bar = QFrame()
        title_bar.setFixedHeight(40)

        layout = QHBoxLayout(title_bar)
        layout.setContentsMargins(10, 5, 10, 5)
        layout.setSpacing(10)

        # Menu on far left
        menu_bar = self._create_menu_bar()
        menu_bar.setFixedWidth(50)
        layout.addWidget(menu_bar)

        # Title in center
        title_label = QLabel(f"{self.texture_data.get('name', 'Unknown')}")
        title_label.setObjectName("title_label")
        title_label.setFont(self.panel_font)
        title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(title_label, stretch=1)

        # Right side buttons
        button_width = 90
        button_height = 30

        # Add button
        add_btn = QPushButton("+ Add")
        add_btn.setFixedSize(button_width, button_height)
        add_btn.clicked.connect(self._generate_bumpmap)
        add_btn.setToolTip("Generate bumpmap (F9)")
        add_btn.setFont(self.button_font)

        layout.addWidget(add_btn)

        # Delete button
        delete_btn = QPushButton("Delete")
        delete_btn.setFixedSize(button_width, button_height)
        delete_btn.clicked.connect(self._delete_bumpmap)
        delete_btn.setEnabled(self._has_bumpmap())
        delete_btn.setToolTip("Remove bumpmap (F11)")
        delete_btn.setFont(self.button_font)
        layout.addWidget(delete_btn)

        # Apply button
        apply_btn = QPushButton("Apply")
        apply_btn.setFixedSize(button_width, button_height)
        apply_btn.clicked.connect(self._apply_changes)
        apply_btn.setToolTip("Apply changes")
        layout.addWidget(apply_btn)

        # Close button
        close_btn = QPushButton("Close")
        close_btn.setFixedSize(button_width, button_height)
        close_btn.clicked.connect(self.close)
        close_btn.setToolTip("Close window")
        layout.addWidget(close_btn)

        return title_bar


    def _create_menu_bar(self): #vers 4
        """Create compact menu bar for embedding in title bar"""
        from PyQt6.QtWidgets import QMenuBar
        from PyQt6.QtGui import QKeySequence
        try:
            from PyQt6.QtGui import QAction
        except ImportError:
            from PyQt6.QtWidgets import QAction

        menu_bar = QMenuBar()

        # Edit menu
        edit_menu = menu_bar.addMenu("Edit")

        # Add (Generate) - F9
        add_action = QAction("Add (Generate Bumpmap)", self)
        add_action.setShortcut(QKeySequence(Qt.Key.Key_F9))
        add_action.triggered.connect(self._generate_bumpmap)
        edit_menu.addAction(add_action)

        # Change (Import/Replace) - F10
        change_action = QAction("Change (Import/Replace)", self)
        change_action.setShortcut(QKeySequence(Qt.Key.Key_F10))
        change_action.triggered.connect(self._import_bumpmap)
        edit_menu.addAction(change_action)

        # Delete - F11
        delete_action = QAction("Delete", self)
        delete_action.setShortcut(QKeySequence(Qt.Key.Key_F11))
        delete_action.triggered.connect(self._delete_bumpmap)
        edit_menu.addAction(delete_action)

        edit_menu.addSeparator()

        # Export
        export_action = QAction("Export Bumpmap...", self)
        export_action.triggered.connect(self._export_bumpmap)
        edit_menu.addAction(export_action)

        return menu_bar


    def _apply_changes(self): #vers 4
        """Apply changes and ensure parent workshop is fully updated"""
        if not self.modified:
            self.close()
            return

        try:
            if hasattr(self.parent_workshop, '_save_undo_state'):
                self.parent_workshop._save_undo_state("Bumpmap changes")
            # Mark parent as modified
            if hasattr(self.parent_workshop, '_mark_as_modified'):
                self.parent_workshop._mark_as_modified()

            # Find and update the texture in parent's texture list
            if hasattr(self.parent_workshop, 'texture_list'):
                texture_name = self.texture_data.get('name', '')
                for i, tex in enumerate(self.parent_workshop.texture_list):
                    if tex.get('name') == texture_name:
                        # Copy all bumpmap-related data back
                        if 'bumpmap_data' in self.texture_data:
                            tex['bumpmap_data'] = self.texture_data['bumpmap_data']
                            tex['has_bumpmap'] = True
                            tex['bumpmap_type'] = self.texture_data.get('bumpmap_type', 0)
                            tex['raster_format_flags'] = self.texture_data.get('raster_format_flags', 0) | 0x10
                        else:
                            # Remove bumpmap if deleted
                            if 'bumpmap_data' in tex:
                                del tex['bumpmap_data']
                            tex['has_bumpmap'] = False
                            tex['raster_format_flags'] = self.texture_data.get('raster_format_flags', 0) & ~0x10
                        break

            # Update selected texture if it's the current one
            if hasattr(self.parent_workshop, 'selected_texture'):
                if self.parent_workshop.selected_texture and \
                self.parent_workshop.selected_texture.get('name') == self.texture_data.get('name'):

                    # Copy bumpmap data to selected texture
                    if 'bumpmap_data' in self.texture_data:
                        self.parent_workshop.selected_texture['bumpmap_data'] = self.texture_data['bumpmap_data']
                        self.parent_workshop.selected_texture['has_bumpmap'] = True
                        self.parent_workshop.selected_texture['bumpmap_type'] = self.texture_data.get('bumpmap_type', 0)
                        self.parent_workshop.selected_texture['raster_format_flags'] = \
                            self.texture_data.get('raster_format_flags', 0) | 0x10
                    else:
                        if 'bumpmap_data' in self.parent_workshop.selected_texture:
                            del self.parent_workshop.selected_texture['bumpmap_data']
                        self.parent_workshop.selected_texture['has_bumpmap'] = False
                        self.parent_workshop.selected_texture['raster_format_flags'] = \
                            self.texture_data.get('raster_format_flags', 0) & ~0x10

                    # Force UI update
                    if hasattr(self.parent_workshop, '_update_texture_info'):
                        self.parent_workshop._update_texture_info(self.parent_workshop.selected_texture)

            # Log message
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("Bumpmap changes applied")

            # Reset modified flag
            self.modified = False

            QMessageBox.information(self, "Success", "Changes applied to texture")

        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to apply changes:\n{str(e)}")


    def _toggle_maximize(self): #vers 1
        """Toggle window maximize"""
        if self.isMaximized():
            self.showNormal()
        else:
            self.showMaximized()


    def _has_bumpmap(self): #vers 2
        """Check if texture has bumpmap"""
        if self.texture_data.get('bumpmap_data') or self.texture_data.get('has_bumpmap', False):
            return True
        if 'raster_format_flags' in self.texture_data:
            return bool(self.texture_data.get('raster_format_flags', 0) & 0x10)
        return False

    def _update_bumpmap_preview(self): #vers 2
        """Update bumpmap preview display"""
        # Safety check - make sure widget exists
        if not hasattr(self, 'bumpmap_preview'):
            return

        try:
            if 'bumpmap_data' in self.texture_data:
                # Decode bumpmap data
                if hasattr(self.parent_workshop, '_decode_bumpmap'):
                    bumpmap_image = self.parent_workshop._decode_bumpmap(
                        self.texture_data['bumpmap_data']
                    )

                    # Convert grayscale to RGB for proper display
                    if bumpmap_image.format() == QImage.Format.Format_Grayscale8:
                        bumpmap_image = bumpmap_image.convertToFormat(QImage.Format.Format_RGB888)

                    pixmap = QPixmap.fromImage(bumpmap_image)
                    self.bumpmap_preview.setPixmap(
                        pixmap.scaled(280, 280,
                                    Qt.AspectRatioMode.KeepAspectRatio,
                                    Qt.TransformationMode.SmoothTransformation)
                    )
            else:
                self.bumpmap_preview.setText("No bumpmap data")
        except Exception as e:
            self.bumpmap_preview.setText(f"Preview error:\n{str(e)}")

    def _generate_bumpmap(self): #vers 2
        """Generate bumpmap from texture"""
        if hasattr(self.parent_workshop, '_generate_bumpmap_from_texture'):
            # Temporarily set selected texture
            old_selection = self.parent_workshop.selected_texture
            self.parent_workshop.selected_texture = self.texture_data

            # Generate
            self.parent_workshop._generate_bumpmap_from_texture()

            # Copy bumpmap data back to our texture_data
            if 'bumpmap_data' in self.parent_workshop.selected_texture:
                self.texture_data['bumpmap_data'] = self.parent_workshop.selected_texture['bumpmap_data']
                self.texture_data['bumpmap_type'] = self.parent_workshop.selected_texture.get('bumpmap_type', 0)
                self.texture_data['has_bumpmap'] = True
                self.texture_data['raster_format_flags'] = \
                    self.parent_workshop.selected_texture.get('raster_format_flags', 0)

            # Restore selection
            self.parent_workshop.selected_texture = old_selection

            # Update preview
            self._update_bumpmap_preview()
            self.modified = True

    def _import_bumpmap(self): #vers 1
        """Import bumpmap from file"""
        if hasattr(self.parent_workshop, '_import_bumpmap'):
            old_selection = self.parent_workshop.selected_texture
            self.parent_workshop.selected_texture = self.texture_data

            self.parent_workshop._import_bumpmap()

            self.parent_workshop.selected_texture = old_selection

            self._update_bumpmap_preview()
            self.modified = True

    def _export_bumpmap(self): #vers 1
        """Export bumpmap to file"""
        if not self._has_bumpmap():
            QMessageBox.warning(self, "No Bumpmap", "This texture has no bumpmap to export")
            return

        if hasattr(self.parent_workshop, '_export_bumpmap'):
            old_selection = self.parent_workshop.selected_texture
            self.parent_workshop.selected_texture = self.texture_data

            self.parent_workshop._export_bumpmap()

            self.parent_workshop.selected_texture = old_selection

    def _delete_bumpmap(self): #vers 1
        """F11 - Delete bumpmap"""
        if not self._has_bumpmap():
            QMessageBox.information(self, "No Bumpmap", "This texture has no bumpmap")
            return

        reply = QMessageBox.question(
            self, "Delete Bumpmap",
            "Remove bumpmap from this texture?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
        )

        if reply == QMessageBox.StandardButton.Yes:
            # Remove bumpmap
            if 'bumpmap_data' in self.texture_data:
                del self.texture_data['bumpmap_data']
            self.texture_data['has_bumpmap'] = False

            if 'raster_format_flags' in self.texture_data:
                self.texture_data['raster_format_flags'] &= ~0x10

            # Update preview
            self.bumpmap_preview.setText("No bumpmap data\n\nPress F9 or use Edit -> Generate Bumpmap")
            self.modified = True

            # Mark parent as modified
            if hasattr(self.parent_workshop, '_mark_as_modified'):
                self.parent_workshop._mark_as_modified()

            QMessageBox.information(self, "Success", "Bumpmap deleted")

    def _create_reflection_panel(self): #vers 3
        """Create panel for reflection map display and generation - WITH IMPORT"""
        from PyQt6.QtWidgets import QVBoxLayout, QHBoxLayout, QLabel, QPushButton, QGroupBox
        from PyQt6.QtCore import Qt

        panel = QGroupBox("Reflection Maps    .")

        layout = QVBoxLayout(panel)
        layout.setSpacing(10)

        # Info label
        info = QLabel("Generate from normal map\nor import existing maps")
        info.setFont(self.panel_font)
        info.setWordWrap(True)
        layout.addWidget(info)

        # Reflection preview
        reflection_label = QLabel("Reflection Vector Map")
        reflection_label.setFont(self.panel_font)
        reflection_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(reflection_label)

        self.reflection_preview = QLabel()
        self.reflection_preview.setMinimumSize(150, 150)
        self.reflection_preview.setMaximumSize(150, 150)
        self.reflection_preview.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.reflection_preview.setText("No data")
        self.reflection_preview.setFont(self.panel_font)
        layout.addWidget(self.reflection_preview)

        # Fresnel preview
        fresnel_label = QLabel("Fresnel Reflectivity")
        fresnel_label.setFont(self.panel_font)
        fresnel_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(fresnel_label)

        self.fresnel_preview = QLabel()
        self.fresnel_preview.setMinimumSize(150, 150)
        self.fresnel_preview.setMaximumSize(150, 150)
        self.fresnel_preview.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.fresnel_preview.setText("No data")
        self.fresnel_preview.setFont(self.panel_font)
        layout.addWidget(self.fresnel_preview)

        # Buttons
        button_layout = QHBoxLayout()

        generate_btn = QPushButton("Generate")
        generate_btn.setFont(self.button_font)
        generate_btn.setToolTip("Generate from RGB normal map")
        generate_btn.clicked.connect(self._generate_reflection_maps)
        button_layout.addWidget(generate_btn)

        layout.addLayout(button_layout) #moved and fixed

        # ADD IMPORT BUTTON
        import_btn = QPushButton("Import")
        import_btn.setFont(self.button_font)
        import_btn.setToolTip("Import reflection maps from files")
        import_btn.clicked.connect(self._import_reflection_maps)
        button_layout.addWidget(import_btn)

        # Export button (separate row)
        export_btn = QPushButton("Export")
        export_btn.setFont(self.button_font)
        export_btn.setToolTip("Export reflection maps to PNG files")
        export_btn.clicked.connect(self._export_reflection_maps)
        layout.addWidget(export_btn)

        layout.addStretch()

        return panel


    def _import_reflection_maps(self): #vers 2
        """Import reflection and Fresnel maps from files"""
        from PyQt6.QtWidgets import QFileDialog, QMessageBox
        from PyQt6.QtGui import QImage

        try:
            # Get reflection map file
            reflection_path, _ = QFileDialog.getOpenFileName(
                self, "Import Reflection Vector Map",
                _work_dir(self),
                "Image Files (*.png *.jpg *.bmp);;All Files (*)"
            )

            if not reflection_path:
                return

            # Get Fresnel map file
            fresnel_path, _ = QFileDialog.getOpenFileName(
                self, "Import Fresnel Reflectivity Map",
                _work_dir(self),
                "Image Files (*.png *.jpg *.bmp);;All Files (*)"
            )

            if not fresnel_path:
                return

            width = self.texture_data.get('width', 0)
            height = self.texture_data.get('height', 0)

            # Load reflection map (RGB)
            reflection_img = QImage(reflection_path)
            if reflection_img.isNull():
                QMessageBox.warning(self, "Error", "Failed to load reflection map")
                return

            # Scale to texture size if needed
            if reflection_img.width() != width or reflection_img.height() != height:
                reflection_img = reflection_img.scaled(
                    width, height,
                    Qt.AspectRatioMode.IgnoreAspectRatio,
                    Qt.TransformationMode.SmoothTransformation
                )

            # Convert to RGB888
            reflection_img = reflection_img.convertToFormat(QImage.Format.Format_RGB888)
            reflection_data = reflection_img.bits().asstring(reflection_img.sizeInBytes())

            # Load Fresnel map (Grayscale)
            fresnel_img = QImage(fresnel_path)
            if fresnel_img.isNull():
                QMessageBox.warning(self, "Error", "Failed to load Fresnel map")
                return

            # Scale to texture size if needed
            if fresnel_img.width() != width or fresnel_img.height() != height:
                fresnel_img = fresnel_img.scaled(
                    width, height,
                    Qt.AspectRatioMode.IgnoreAspectRatio,
                    Qt.TransformationMode.SmoothTransformation
                )

            # Convert to grayscale
            fresnel_img = fresnel_img.convertToFormat(QImage.Format.Format_Grayscale8)
            fresnel_data = fresnel_img.bits().asstring(fresnel_img.sizeInBytes())

            # Store in texture data
            self.texture_data['reflection_map'] = reflection_data
            self.texture_data['fresnel_map'] = fresnel_data
            self.texture_data['has_reflection'] = True

            # Update previews
            self._update_reflection_previews()

            self.modified = True

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("Imported reflection maps")

            QMessageBox.information(self, "Success", "Reflection maps imported successfully")

        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to import:\n{str(e)}")


    def _update_reflection_previews(self): #vers 1
        """Update reflection and Fresnel map previews"""
        from PyQt6.QtGui import QPixmap
        from PyQt6.QtCore import Qt

        try:
            width = self.texture_data.get('width', 0)
            height = self.texture_data.get('height', 0)

            # Update reflection map preview
            if 'reflection_map' in self.texture_data:
                reflection_data = self.texture_data['reflection_map']

                # Convert to numpy for QImage
                reflection_arr = np.frombuffer(reflection_data, dtype=np.uint8)
                reflection_arr = reflection_arr.reshape((height, width, 3))

                img = self._convert_numpy_to_qimage(
                    reflection_arr, width, height, is_grayscale=False
                )

                if img:
                    pixmap = QPixmap.fromImage(img)
                    self.reflection_preview.setPixmap(
                        pixmap.scaled(200, 200,
                                    Qt.AspectRatioMode.KeepAspectRatio,
                                    Qt.TransformationMode.SmoothTransformation)
                    )

            # Update Fresnel map preview
            if 'fresnel_map' in self.texture_data:
                fresnel_data = self.texture_data['fresnel_map']

                # Convert to numpy for QImage
                fresnel_arr = np.frombuffer(fresnel_data, dtype=np.uint8)
                fresnel_arr = fresnel_arr.reshape((height, width))

                img = self._convert_numpy_to_qimage(
                    fresnel_arr, width, height, is_grayscale=True
                )

                if img:
                    pixmap = QPixmap.fromImage(img)
                    self.fresnel_preview.setPixmap(
                        pixmap.scaled(200, 200,
                                    Qt.AspectRatioMode.KeepAspectRatio,
                                    Qt.TransformationMode.SmoothTransformation)
                    )

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Preview update error: {str(e)}")


    def _generate_reflection_maps(self): #vers 2
        """Generate reflection and Fresnel maps from normal map data"""
        from PyQt6.QtWidgets import QMessageBox, QInputDialog
        from PyQt6.QtGui import QPixmap
        from PyQt6.QtCore import Qt

        try:
            # Check if we have normal map data
            bumpmap_data = self.texture_data.get('bumpmap_data')
            if not bumpmap_data:
                QMessageBox.warning(self, "No Normal Map",
                    "Generate or import a normal map first")
                return

            # Check if it's RGB normal map
            bumpmap_type = self.texture_data.get('bumpmap_type', 0)
            if bumpmap_type == 0:  # Grayscale height map
                QMessageBox.information(self, "Info",
                    "Reflection maps require RGB normal map.\n"
                    "Current bumpmap is grayscale height map.")
                return

            # Check if numpy is available
            try:
                import numpy as np
            except ImportError:
                QMessageBox.warning(self, "Missing Dependency",
                    "Reflection map generation requires numpy.\n\n"
                    "Install with: pip install numpy")
                return

            # Get F0 value from user
            F0, ok = QInputDialog.getDouble(
                self, "Fresnel Reflectivity",
                "Base reflectivity (F0):\n"
                "0.04 = Dielectric (glass, plastic)\n"
                "0.5-1.0 = Metal",
                0.04, 0.01, 1.0, 2
            )
            if not ok:
                return

            width = self.texture_data.get('width', 0)
            height = self.texture_data.get('height', 0)

            # Extract normal map data
            if bumpmap_type == 1:  # RGB normal map
                normal_data = bumpmap_data
            elif bumpmap_type == 2:  # Both
                # Skip first byte (type identifier) and grayscale data
                normal_data = bumpmap_data[1 + width * height:]
            else:
                QMessageBox.warning(self, "Error", "Unknown bumpmap type")
                return

            # Generate reflection maps
            result = self._generate_reflection_from_normal(
                normal_data, width, height, auto_flip=True, F0=F0
            )

            if result:
                # Store in texture data
                self.texture_data['reflection_map'] = result['reflection_map']
                self.texture_data['fresnel_map'] = result['fresnel_map']
                self.texture_data['has_reflection'] = True

                # Update previews
                self._update_reflection_previews()

                self.modified = True

                if self.main_window and hasattr(self.main_window, 'log_message'):
                    flip_msg = " (Y-axis corrected)" if result['y_flipped'] else ""
                    self.main_window.log_message(
                        f"Generated reflection maps{flip_msg}"
                    )

                QMessageBox.information(self, "Success",
                    f"Generated reflection maps\nF0: {F0}")

        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to generate:\n{str(e)}")


    def _export_reflection_maps(self): #vers 2
        """Export reflection and Fresnel maps as PNG files"""
        from PyQt6.QtWidgets import QFileDialog, QMessageBox
        from PIL import Image
        import os

        try:
            if 'reflection_map' not in self.texture_data:
                QMessageBox.warning(self, "No Data", "No reflection maps to export")
                return

            # Get output directory
            output_dir = QFileDialog.getExistingDirectory(
                self, "Select Output Directory", _work_dir(self)
            )
            if not output_dir:
                return

            texture_name = self.texture_data.get('name', 'texture')
            width = self.texture_data.get('width', 0)
            height = self.texture_data.get('height', 0)

            exported = []

            # Export reflection map
            if 'reflection_map' in self.texture_data:
                reflection_arr = np.frombuffer(
                    self.texture_data['reflection_map'], dtype=np.uint8
                )
                reflection_arr = reflection_arr.reshape((height, width, 3))

                img = Image.fromarray(reflection_arr, mode='RGB')
                path = os.path.join(output_dir, f"{texture_name}_reflection.png")
                img.save(path)
                exported.append("reflection_vector_map.png")

            # Export Fresnel map
            if 'fresnel_map' in self.texture_data:
                fresnel_arr = np.frombuffer(
                    self.texture_data['fresnel_map'], dtype=np.uint8
                )
                fresnel_arr = fresnel_arr.reshape((height, width))

                img = Image.fromarray(fresnel_arr, mode='L')
                path = os.path.join(output_dir, f"{texture_name}_fresnel.png")
                img.save(path)
                exported.append("fresnel_reflectivity.png")

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(
                    f"Exported {len(exported)} maps to: {output_dir}"
                )

            QMessageBox.information(self, "Success",
                f"Exported {len(exported)} maps:\n" + "\n".join(exported))

        except Exception as e:
            QMessageBox.critical(self, "Error", f"Export failed:\n{str(e)}")


    def _generate_reflection_from_normal(self, normal_data, width, height, auto_flip=True, F0=0.04): #vers 2
        """
        Generate reflection and Fresnel maps from normal map data

        """
        try:
            # Convert bytes to numpy array
            normal_arr = np.frombuffer(normal_data, dtype=np.uint8)
            normal_arr = normal_arr.reshape((height, width, 3))

            # Convert to float [0, 1]
            normal_float = normal_arr.astype(np.float32) / 255.0

            # Auto-detect Y flip if requested
            y_flipped = False
            if auto_flip and self.parent_workshop._detect_y_flip(normal_float):
                normal_float[:, :, 1] = 1.0 - normal_float[:, :, 1]
                y_flipped = True

            # Convert back to uint8
            normal_arr = (normal_float * 255.0).astype(np.uint8)

            # Generate reflection and Fresnel maps
            reflection, fresnel = self.parent_workshop._normal_to_reflection(normal_arr, F0=F0)

            return {
                'reflection_map': reflection.tobytes(),
                'fresnel_map': fresnel.tobytes(),
                'y_flipped': y_flipped
            }

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Reflection generation error: {str(e)}")
            return None


    def _convert_numpy_to_qimage(self, numpy_array, width, height, is_grayscale=False): #vers 1
        """Convert numpy array to QImage for preview"""
        try:
            if is_grayscale:
                # Grayscale image
                img = QImage(numpy_array.tobytes(), width, height, width,
                            QImage.Format.Format_Grayscale8)
            else:
                # RGB image
                img = QImage(numpy_array.tobytes(), width, height, width * 3,
                            QImage.Format.Format_RGB888)
            return img
        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Image conversion error: {str(e)}")
            return None


    def _apply_changes(self): #vers 1
        """Apply changes to parent workshop without closing window"""
        if self.modified:
            # Mark parent as modified
            if hasattr(self.parent_workshop, '_mark_as_modified'):
                self.parent_workshop._mark_as_modified()

            # Update parent texture info
            if hasattr(self.parent_workshop, '_update_texture_info'):
                self.parent_workshop._update_texture_info(self.texture_data)

            # Log message
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("Bumpmap changes applied")

            # Reset modified flag
            self.modified = False

            QMessageBox.information(self, "Success", "Changes applied to texture")


    def closeEvent(self, event): #vers 5
        """Handle window close event"""

        if self.modified:
            from PyQt6.QtWidgets import QMessageBox
            reply = QMessageBox.question(
                self, "Unsaved Changes",
                "Apply changes before closing?",
                QMessageBox.StandardButton.Yes |
                QMessageBox.StandardButton.No |
                QMessageBox.StandardButton.Cancel
            )

            if reply == QMessageBox.StandardButton.Yes:
                self._apply_changes()
                event.accept()
            elif reply == QMessageBox.StandardButton.No:
                event.accept()
            else:
                event.ignore()
        else:
            event.accept()
