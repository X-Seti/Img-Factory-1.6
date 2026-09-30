#this belongs in apps/components/Txd_Editor/depends/txd_logic_func.py - Version: 7
# X-Seti - September30 2026 - IMG Factory 1.6 - TXD Workshop logic

"""
TXD Workshop logic - TXD load/save, texture edits, import/export, mipmaps, bumpmaps.
"""

##class TXDLogicMixin: -
# _add_texture_to_table
# _add_warning_badge
# _apply_gaussian_blur
# _auto_generate_mipmaps
# _auto_generate_mipmaps_to_level
# _batch_import_from_folder
# _build_txd_from_dff
# _change_bit_depth
# _change_format
# _check_alpha_validity
# _check_txd_vs_dff
# _clear_texture_search
# _compress_texture
# _convert_texture
# _copy_texture
# _create_blank_texture
# _create_bumpmap_data
# _create_empty_txd_data
# _create_mipmaps_dialog
# _create_new_texture_entry
# _create_new_txd
# _create_thumbnail
# _decode_bumpmap
# _decompress_dxt1
# _decompress_dxt3
# _decompress_dxt5
# _decompress_texture
# _decompress_uncompressed
# _delete_bumpmap
# _delete_texture
# _detect_txd_info
# _detect_y_flip
# _display_mobile_textures
# _display_xtx_texture
# dragEnterEvent
# dragMoveEvent
# dropEvent
# _dropped_files
# _duplicate_texture
# _emboss_filter
# _encode_bumpmap
# export_all_textures
# _export_alpha_only
# _export_bumpmap
# export_selected_texture
# _extract_alpha_channel
# _extract_txd_from_img
# _flip_horizontal
# _flip_vertical
# _force_save_txd
# _generate_alpha_mask
# _generate_bumpmap_from_texture
# _generate_rgb_normal_map
# _get_current_rgba
# _get_format_description
# _has_bumpmap_data
# _height_map
# _import_alpha_texture
# _import_bumpmap
# _import_normal_texture
# _import_texture_files
# _import_textures
# _invert_grayscale
# load_from_img_archive
# _load_img_txd_list
# _load_settings
# _load_txd_textures
# _log
# _mark_as_modified
# _normal_to_reflection
# _normalize_vector
# _on_texture_selected
# _on_texture_table_double_click
# _on_txd_selected
# _open_alpha_coverage
# _open_chk_file
# _open_colour_adjust
# _open_filters_dialog
# open_img_archive
# _open_mipmap_manager
# _open_mobile_texture_db
# _open_paint_editor
# _open_ps2_txd
# _open_seamless_tool
# _open_snow_tool
# open_txd_file
# _open_xtd_file
# _open_xtx_file
# _parse_dff_materials
# _parse_single_texture
# _paste_texture
# _perform_ai_upscale
# _preview_bumpmap_generation
# _quick_alpha_check
# _rebuild_txd_data
# _reload_texture_table
# _remove_mipmaps
# _rename_texture
# _rename_texture_shortcut
# _resize_texture
# _resize_texture_data
# _rgba_to_iff_ilbm
# _rotate_clockwise
# _rotate_counterclockwise
# _save_alpha_name
# _save_as_txd_file
# _save_as_txd_file_with_version_selector
# _save_settings
# _save_texture_format
# _save_texture_name
# _save_texture_png
# save_txd_file
# _save_txd_file
# _save_txd_to_img_with_version_selector
# _save_undo_state
# _set_current_rgba
# _set_undo_enabled
# show_properties
# _show_txd_info
# _show_version_selector_dialog
# _sobel_filter
# _strip_unsupported_features_for_version
# _texture_statistics
# _toggle_alpha_invert
# _uncompress_texture
# _undo_last_action
# _upscale_texture
# _view_bumpmap

import numpy as np
import os
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QFont, QImage, QPixmap
from PyQt6.QtWidgets import QCheckBox, QColorDialog, QComboBox, QDialog, QFileDialog, QFormLayout, QGroupBox, QHBoxLayout, QInputDialog, QLabel, QLineEdit, QListWidgetItem, QMessageBox, QPushButton, QRadioButton, QSlider, QSpinBox, QTableWidgetItem, QTextEdit, QVBoxLayout
from apps.components.Txd_Editor.depends.txd_ui_func import App_name
from apps.methods.txd_dialogs import BumpmapManagerWindow, MipmapManagerWindow
from apps.methods.txd_dxt_encode import _encode_dxt1
from apps.methods.txd_versions import detect_txd_version, get_game_from_version, get_platform_name, get_version_capabilities, is_bumpmap_supported, validate_txd_format
from apps.methods.img_factory_settings import get_user_config_dir

_DROP_EXTS = ('.txd', '.img', '.png', '.jpg', '.jpeg', '.bmp', '.tga', '.dds', '.gif', '.tiff', '.webp')

class TXDLogicMixin: #vers 1
    """logic methods for TXDWorkshop."""

    def _detect_txd_info(self, txd_data: bytes) -> bool: #vers 1
        """
        Detect and store TXD version and platform information
        Called when loading any TXD file
        """
        try:
            # Validate format first
            is_valid, message = validate_txd_format(txd_data)

            if not is_valid:
                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"TXD Validation: {message}")
                return False

            # Detect version info
            self.txd_version_id, self.txd_device_id, self.txd_version_str = detect_txd_version(txd_data)

            # Get platform and game info
            self.txd_platform_name = get_platform_name(self.txd_device_id)
            self.txd_game = get_game_from_version(self.txd_version_id, self.txd_device_id)

            # Get capabilities
            self.txd_capabilities = get_version_capabilities(self.txd_version_id)

            # Log detection
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(
                    f"TXD: {self.txd_version_str} | "
                    f"Platform: {self.txd_platform_name} | "
                    f"Game: {self.txd_game}"
                )

            return True

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Version detection error: {str(e)}")
            return False

    def _create_mipmaps_dialog(self): #vers 1
        """Open dialog to create mipmaps with depth selection"""
        if not self.selected_texture:
            QMessageBox.warning(self, "No Selection", "Please select a texture first")
            return

        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QLabel, QSlider, QPushButton, QHBoxLayout
        import math

        # Calculate possible mipmap levels
        width = self.selected_texture.get('width', 256)
        height = self.selected_texture.get('height', 256)
        max_dimension = max(width, height)

        # Calculate how many levels possible (down to 1x1)
        max_levels = int(math.log2(max_dimension)) + 1

        # Create selection dialog
        dialog = QDialog(self)
        dialog.setWindowTitle("Create Mipmaps")
        dialog.setModal(True)
        dialog.resize(400, 250)

        layout = QVBoxLayout(dialog)

        # Header info
        header = QLabel(f"Texture: {self.selected_texture['name']}\n"
                    f"Size: {width}x{height}\n\n"
                    f"Select minimum mipmap size:")
        header.setStyleSheet("font-weight: bold; padding: 10px;")
        layout.addWidget(header)

        # Slider with level preview
        slider_layout = QVBoxLayout()

        mipmap_slider = QSlider(Qt.Orientation.Horizontal)
        mipmap_slider.setMinimum(0)  # Down to 1x1
        mipmap_slider.setMaximum(max_levels - 1)
        mipmap_slider.setValue(max(0, max_levels - 6))  # Default to ~32x32
        mipmap_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        mipmap_slider.setTickInterval(1)

        # Preview label showing dimensions at each level
        mipmap_preview = QLabel()
        mipmap_preview.setStyleSheet("font-size: 14px; padding: 10px; "
                                    "background: palette(base); border-radius: 3px;")
        mipmap_preview.setAlignment(Qt.AlignmentFlag.AlignCenter)

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

            mipmap_preview.setText(preview_text)

        mipmap_slider.valueChanged.connect(update_preview)
        update_preview(mipmap_slider.value())

        slider_layout.addWidget(QLabel("More Levels <-  ->  Fewer Levels"))
        slider_layout.addWidget(mipmap_slider)
        slider_layout.addWidget(mipmap_preview)

        layout.addLayout(slider_layout)

        # Buttons
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        def do_generate():  #vers 1
            slider_value = mipmap_slider.value()
            num_levels = max_levels - slider_value
            dialog.accept()

            # Generate mipmaps
            if hasattr(self, '_auto_generate_mipmaps_to_level'):
                self._auto_generate_mipmaps_to_level(num_levels)
            else:
                self._auto_generate_mipmaps()

        generate_btn = QPushButton("Generate")
        generate_btn.clicked.connect(do_generate)
        button_layout.addWidget(generate_btn)

        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(dialog.reject)
        button_layout.addWidget(cancel_btn)

        layout.addLayout(button_layout)

        dialog.exec()

    def _remove_mipmaps(self): #vers 1
        """Remove all mipmap levels except Level 0"""
        if not self.selected_texture:
            QMessageBox.warning(self, "No Selection", "Please select a texture first")
            return

        mipmap_levels = self.selected_texture.get('mipmap_levels', [])
        if len(mipmap_levels) <= 1:
            QMessageBox.information(self, "No Mipmaps",
                                "This texture has no mipmap levels to remove")
            return

        reply = QMessageBox.question(
            self, "Remove Mipmaps",
            f"Remove all mipmap levels from '{self.selected_texture['name']}'?\n\n"
            f"This will keep only Level 0 (main texture).",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
        )

        if reply == QMessageBox.StandardButton.Yes:
            # Keep only level 0
            level_0 = next((l for l in mipmap_levels if l.get('level') == 0), None)

            if level_0:
                self.selected_texture['mipmap_levels'] = [level_0]
                self.selected_texture['mipmaps'] = 1
            else:
                # No level 0 found, create it from main texture data
                self.selected_texture['mipmap_levels'] = [{
                    'level': 0,
                    'width': self.selected_texture['width'],
                    'height': self.selected_texture['height'],
                    'rgba_data': self.selected_texture['rgba_data'],
                    'compressed_data': None,
                    'compressed_size': len(self.selected_texture.get('rgba_data', b''))
                }]
                self.selected_texture['mipmaps'] = 1

            # Update display
            self._save_undo_state("Remove mipmaps")
            self._update_texture_info(self.selected_texture)
            self._reload_texture_table()
            self._mark_as_modified()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Removed mipmaps from: {self.selected_texture['name']}")

    def _on_texture_table_double_click(self, item): #vers 1
        """Handle double-click on texture table - open mipmap manager"""
        try:
            row = item.row()

            # Get texture for this row
            if row < 0 or row >= len(self.texture_list):
                return

            texture = self.texture_list[row]

            # Check if texture has mipmaps
            mipmap_levels = texture.get('mipmap_levels', [])
            if len(mipmap_levels) > 1:
                # Has mipmaps - open mipmap manager
                self.selected_texture = texture
                self._open_mipmap_manager()
            else:
                # No mipmaps - just select the texture
                self.selected_texture = texture
                self._update_texture_info(texture)

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Double-click error: {str(e)}")

    def _change_bit_depth(self): #vers 1
        """Change texture bit depth"""
        if not self.selected_texture:
            QMessageBox.warning(self, "No Selection", "Please select a texture first")
            return

        from PyQt6.QtWidgets import QInputDialog

        current_depth = self.selected_texture.get('depth', 32)

        bit_depths = ["32bit (RGBA)", "24bit (RGB)", "16bit (ARGB1555)", "16bit (ARGB4444)", "16bit (RGB565)", "8bit (Indexed)"]
        depth_values = [32, 24, 16, 16, 16, 8]

        # Find current selection
        try:
            current_index = depth_values.index(current_depth)
        except:
            current_index = 0

        choice, ok = QInputDialog.getItem(
            self,
            "Change Bit Depth",
            f"Current: {current_depth}bit\n\nSelect new bit depth:",
            bit_depths,
            current_index,
            False
        )

        if ok:
            new_depth = depth_values[bit_depths.index(choice)]

            if new_depth != current_depth:
                self._save_undo_state("Change bit depth")
                self.selected_texture['depth'] = new_depth

                self._update_texture_info(self.selected_texture)
                self._update_table_display()
                self._mark_as_modified()

                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"Bit depth changed: {current_depth}bit -> {new_depth}bit")

    def _generate_bumpmap_from_texture(self): #vers 2
        """Generate bumpmap from texture with type selection"""
        if not self.selected_texture:
            QMessageBox.warning(self, "No Selection", "Please select a texture first")
            return

        rgba_data = self.selected_texture.get('rgba_data')
        width = self.selected_texture.get('width', 0)
        height = self.selected_texture.get('height', 0)

        if not rgba_data or width == 0:
            QMessageBox.warning(self, "No Data", "Texture has no image data")
            return

        try:
            # Create generation dialog
            dialog = QDialog(self)
            dialog.setWindowTitle("Generate Bumpmap")
            dialog.setMinimumWidth(450)

            layout = QVBoxLayout(dialog)

            # === BUMPMAP TYPE SELECTION ===
            type_group = QGroupBox("Bumpmap Type")
            type_layout = QVBoxLayout()

            type_combo = QComboBox()
            type_combo.addItems([
                "Grayscale Height Map (Traditional)",
                "RGB Normal Map (Colorful)",
                "Both (Height + Normal)"
            ])
            type_layout.addWidget(type_combo)

            type_info = QLabel()
            type_info.setStyleSheet("color: #888; font-size: 9pt; padding: 5px;")
            type_info.setWordWrap(True)

            def update_type_info(index):  #vers 1
                if index == 0:  # Grayscale
                    type_info.setText(
                        "Grayscale Height Map:\n"
                        "• White = raised areas, Black = recessed\n"
                        "• Used by game engine to calculate normals in real-time\n"
                        "• Smaller file size, standard for GTA"
                    )
                elif index == 1:  # RGB Normal
                    type_info.setText(
                        "RGB Normal Map:\n"
                        "• Pre-calculated surface normals (colorful)\n"
                        "• R=X, G=Y, B=Z normal directions\n"
                        "• Higher quality, larger file size"
                    )
                else:  # Both
                    type_info.setText(
                        "Both Types:\n"
                        "• Stores both height map and normal map\n"
                        "• Maximum compatibility\n"
                        "• Largest file size"
                    )

            type_combo.currentIndexChanged.connect(update_type_info)
            update_type_info(0)

            type_layout.addWidget(type_info)
            type_group.setLayout(type_layout)
            layout.addWidget(type_group)

            # === GENERATION METHOD ===
            method_group = QGroupBox("Generation Method")
            method_layout = QVBoxLayout()

            method_combo = QComboBox()
            method_combo.addItems([
                "Sobel Filter (Edge Detection)",
                "Height Map (Grayscale)",
                "Normal Map (RGB)",
                "Emboss Filter"
            ])
            method_layout.addWidget(method_combo)

            method_info = QLabel(
                "Sobel: Detects edges for bump effect\n"
                "Height: Uses brightness as height\n"
                "Normal: Creates RGB normal map\n"
                "Emboss: Creates raised/lowered effect"
            )
            method_info.setStyleSheet("color: #888; font-size: 9pt;")
            method_layout.addWidget(method_info)

            method_group.setLayout(method_layout)
            layout.addWidget(method_group)

            # === STRENGTH CONTROL ===
            strength_group = QGroupBox("Strength")
            strength_layout = QFormLayout()

            strength_slider = QSlider(Qt.Orientation.Horizontal)
            strength_slider.setRange(1, 100)
            strength_slider.setValue(50)
            strength_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
            strength_slider.setTickInterval(10)

            strength_label = QLabel("50%")
            strength_slider.valueChanged.connect(
                lambda v: strength_label.setText(f"{v}%")
            )

            strength_layout.addRow("Intensity:", strength_slider)
            strength_layout.addRow("", strength_label)

            strength_group.setLayout(strength_layout)
            layout.addWidget(strength_group)

            # === SMOOTHING CONTROL ===
            smooth_group = QGroupBox("Smoothing")
            smooth_layout = QFormLayout()

            smooth_slider = QSlider(Qt.Orientation.Horizontal)
            smooth_slider.setRange(0, 10)
            smooth_slider.setValue(2)
            smooth_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
            smooth_slider.setTickInterval(1)

            smooth_label = QLabel("2")
            smooth_slider.valueChanged.connect(
                lambda v: smooth_label.setText(str(v))
            )

            smooth_layout.addRow("Blur Radius:", smooth_slider)
            smooth_layout.addRow("", smooth_label)

            smooth_group.setLayout(smooth_layout)
            layout.addWidget(smooth_group)

            # === INVERT OPTION ===
            invert_check = QCheckBox("Invert bumpmap (swap raised/lowered)")
            layout.addWidget(invert_check)

            # === BUTTONS ===
            button_layout = QHBoxLayout()
            button_layout.addStretch()

            preview_btn = QPushButton("Preview")
            preview_btn.clicked.connect(
                lambda: self._preview_bumpmap_generation(
                    rgba_data, width, height,
                    type_combo.currentIndex(),
                    method_combo.currentIndex(),
                    strength_slider.value(),
                    smooth_slider.value(),
                    invert_check.isChecked()
                )
            )
            button_layout.addWidget(preview_btn)

            generate_btn = QPushButton("Generate")
            generate_btn.setDefault(True)
            generate_btn.clicked.connect(dialog.accept)
            button_layout.addWidget(generate_btn)

            cancel_btn = QPushButton("Cancel")
            cancel_btn.clicked.connect(dialog.reject)
            button_layout.addWidget(cancel_btn)

            layout.addLayout(button_layout)

            # === EXECUTE DIALOG ===
            if dialog.exec() == QDialog.DialogCode.Accepted:
                # Generate bumpmap with selected settings
                bumpmap_type = type_combo.currentIndex()
                method = method_combo.currentIndex()
                strength = strength_slider.value() / 100.0
                smooth = smooth_slider.value()
                invert = invert_check.isChecked()

                bumpmap_data = self._create_bumpmap_data(
                    rgba_data, width, height, bumpmap_type, method, strength, smooth, invert
                )

                if bumpmap_data:
                    # Save undo state
                    self._save_undo_state("Generate bumpmap")

                    # Add bumpmap to texture
                    self.selected_texture['bumpmap_data'] = bumpmap_data
                    self.selected_texture['bumpmap_type'] = bumpmap_type  # Store type
                    self.selected_texture['has_bumpmap'] = True
                    self.selected_texture['raster_format_flags'] = \
                        self.selected_texture.get('raster_format_flags', 0) | 0x10

                    # Mark modified
                    self._mark_as_modified()

                    # Update UI
                    self._update_texture_info(self.selected_texture)

                    type_names = ["Grayscale Height Map", "RGB Normal Map", "Both (Height + Normal)"]
                    QMessageBox.information(self, "Success",
                        f"Bumpmap generated successfully!\nType: {type_names[bumpmap_type]}")

                    if self.main_window and hasattr(self.main_window, 'log_message'):
                        self.main_window.log_message(
                            f"Generated {type_names[bumpmap_type]} for: {self.selected_texture.get('name', 'texture')}"
                        )

        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to generate bumpmap:\n{str(e)}")
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Bumpmap generation error: {str(e)}")

    def _create_bumpmap_data(self, rgba_data, width, height, bumpmap_type, method, strength, smooth, invert): #vers 2
        """Create bumpmap data with type selection"""
        import struct

        try:
            # Convert RGBA to grayscale first
            grayscale = bytearray(width * height)
            for i in range(0, len(rgba_data), 4):
                r, g, b = rgba_data[i:i+3]
                # Luminosity method
                gray = int(0.299 * r + 0.587 * g + 0.114 * b)
                grayscale[i // 4] = gray

            # Apply smoothing if requested
            if smooth > 0:
                grayscale = self._apply_gaussian_blur(grayscale, width, height, smooth)

            # Generate based on type
            if bumpmap_type == 0:  # Grayscale Height Map
                # Generate height map
                if method == 0:  # Sobel Filter
                    bumpmap = self._sobel_filter(grayscale, width, height, strength)
                elif method == 1:  # Height Map
                    bumpmap = self._height_map(grayscale, width, height, strength)
                elif method == 2:  # Normal Map (but output as grayscale)
                    bumpmap = self._sobel_filter(grayscale, width, height, strength)
                elif method == 3:  # Emboss
                    bumpmap = self._emboss_filter(grayscale, width, height, strength)
                else:
                    bumpmap = grayscale

                # Invert if requested
                if invert:
                    bumpmap = bytearray(255 - b for b in bumpmap)

                return bytes(bumpmap)

            elif bumpmap_type == 1:  # RGB Normal Map
                # Generate RGB normal map
                normal_map = self._generate_rgb_normal_map(grayscale, width, height, strength)

                # Invert if requested (flip normals)
                if invert:
                    inverted = bytearray(len(normal_map))
                    for i in range(0, len(normal_map), 3):
                        inverted[i] = 255 - normal_map[i]      # Invert R
                        inverted[i+1] = 255 - normal_map[i+1]  # Invert G
                        inverted[i+2] = normal_map[i+2]        # Keep B (Z) the same
                    normal_map = inverted

                return bytes(normal_map)

            else:  # Both types
                # Generate both and combine with header
                height_map = self._sobel_filter(grayscale, width, height, strength)
                if invert:
                    height_map = bytearray(255 - b for b in height_map)

                normal_map = self._generate_rgb_normal_map(grayscale, width, height, strength)
                if invert:
                    inverted = bytearray(len(normal_map))
                    for i in range(0, len(normal_map), 3):
                        inverted[i] = 255 - normal_map[i]
                        inverted[i+1] = 255 - normal_map[i+1]
                        inverted[i+2] = normal_map[i+2]
                    normal_map = inverted

                # Combine: [type_byte][height_map][normal_map]
                combined = bytearray()
                combined.append(2)  # Type identifier: 2 = both
                combined.extend(height_map)
                combined.extend(normal_map)

                return bytes(combined)

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Bumpmap creation error: {str(e)}")
            return None

    def _generate_rgb_normal_map(self, grayscale, width, height, strength): #vers 2
        """Generate proper RGB normal map from height data"""
        normal_map = bytearray(width * height * 3)

        for y in range(1, height - 1):
            for x in range(1, width - 1):
                # Sample neighboring heights
                left = grayscale[y * width + (x - 1)]
                right = grayscale[y * width + (x + 1)]
                up = grayscale[(y - 1) * width + x]
                down = grayscale[(y + 1) * width + x]

                # Calculate normal vector using height differences
                dx = (left - right) * strength * 2.0  # Increased multiplier
                dy = (up - down) * strength * 2.0     # Increased multiplier
                dz = 128.0  # Base Z strength

                # Normalize vector
                length = (dx*dx + dy*dy + dz*dz) ** 0.5
                if length > 0:
                    dx /= length
                    dy /= length
                    dz /= length

                # Map to RGB range [0-255]
                # Normal maps: flat surface = (128, 128, 255) in RGB = (0.5, 0.5, 1.0) in normalized
                r = int((dx * 0.5 + 0.5) * 255)
                g = int((dy * 0.5 + 0.5) * 255)
                b = int((dz * 0.5 + 0.5) * 255)

                idx = (y * width + x) * 3
                normal_map[idx] = max(0, min(255, r))
                normal_map[idx + 1] = max(0, min(255, g))
                normal_map[idx + 2] = max(0, min(255, b))

        # Fill edges with flat normal (128, 128, 255)
        for y in range(height):
            for x in range(width):
                if y == 0 or y == height - 1 or x == 0 or x == width - 1:
                    idx = (y * width + x) * 3
                    normal_map[idx] = 128      # R = 0.5 (no X tilt)
                    normal_map[idx + 1] = 128  # G = 0.5 (no Y tilt)
                    normal_map[idx + 2] = 255  # B = 1.0 (pointing up)

        return normal_map

    def _normalize_vector(self, v): #vers 1
        """Normalize vector array"""
        norm = np.linalg.norm(v, axis=2, keepdims=True)
        norm[norm == 0] = 1.0
        return v / norm

    def _detect_y_flip(self, normal): #vers 1
        """Heuristic to detect if Y channel is flipped (DirectX vs OpenGL)"""
        pos_y_ratio = np.mean(normal[:, :, 1] > 0.5)
        return pos_y_ratio < 0.4

    def _normal_to_reflection(self, normal_map, view=(0, 0, 1), F0=0.04): #vers 1
        """
        Generate reflection vector map and Fresnel reflectivity from normal map
        """
        n = normal_map.astype(np.float32) / 255.0
        n = n * 2.0 - 1.0
        n = self._normalize_vector(n)

        V = np.array(view, dtype=np.float32)
        V = V / np.linalg.norm(V)

        # Calculate dot product V·N
        VdotN = np.sum(V * n, axis=2, keepdims=True)

        # Calculate reflection vector: R = V - 2(V·N)N
        R = V - 2.0 * VdotN * n
        R = self._normalize_vector(R)

        # Encode reflection vector to RGB (map from [-1,1] to [0,255])
        R_enc = ((R + 1.0) * 0.5 * 255.0).astype(np.uint8)

        # Calculate Fresnel reflectivity using Schlick's approximation
        VdotN_scalar = np.clip(VdotN.squeeze(), -1.0, 1.0)
        one_minus = 1.0 - np.clip(VdotN_scalar, 0.0, 1.0)
        F = F0 + (1.0 - F0) * (one_minus ** 5)
        F_img = (F * 255.0).astype(np.uint8)

        return R_enc, F_img

    def _sobel_filter(self, data, width, height, strength): #vers 2
        """Apply Sobel edge detection filter"""
        result = bytearray(width * height)

        # Sobel kernels
        gx = [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]]
        gy = [[-1, -2, -1], [0, 0, 0], [1, 2, 1]]

        for y in range(1, height - 1):
            for x in range(1, width - 1):
                px = 0
                py = 0

                # Apply kernels
                for ky in range(-1, 2):
                    for kx in range(-1, 2):
                        pixel = data[(y + ky) * width + (x + kx)]
                        px += pixel * gx[ky + 1][kx + 1]
                        py += pixel * gy[ky + 1][kx + 1]

                # Calculate magnitude
                magnitude = int(((px * px + py * py) ** 0.5) * strength * 2)  # Added multiplier
                magnitude = max(0, min(255, magnitude))

                result[y * width + x] = magnitude

        return result

    def _height_map(self, data, width, height, strength): #vers 2
        """Convert grayscale to height map"""
        result = bytearray(width * height)

        for i in range(len(data)):
            # Apply strength - don't reduce brightness
            value = int(data[i] * (0.5 + strength * 0.5))  # Scale from 0.5-1.0x
            result[i] = max(0, min(255, value))

        return result

    def _emboss_filter(self, data, width, height, strength): #vers 1
        """Apply emboss filter"""
        result = bytearray(width * height)

        # Emboss kernel
        kernel = [[-2, -1, 0], [-1, 1, 1], [0, 1, 2]]

        for y in range(1, height - 1):
            for x in range(1, width - 1):
                value = 0

                for ky in range(-1, 2):
                    for kx in range(-1, 2):
                        pixel = data[(y + ky) * width + (x + kx)]
                        value += pixel * kernel[ky + 1][kx + 1]

                value = int(128 + value * strength)
                value = max(0, min(255, value))

                result[y * width + x] = value

        return result

    def _apply_gaussian_blur(self, data, width, height, radius): #vers 1
        """Apply Gaussian blur for smoothing"""
        if radius == 0:
            return data

        result = bytearray(width * height)
        kernel_size = radius * 2 + 1
        sigma = radius / 3.0

        # Generate Gaussian kernel
        kernel = []
        kernel_sum = 0
        for y in range(-radius, radius + 1):
            row = []
            for x in range(-radius, radius + 1):
                value = (1.0 / (2.0 * 3.14159 * sigma * sigma)) * \
                        (2.71828 ** (-(x*x + y*y) / (2.0 * sigma * sigma)))
                row.append(value)
                kernel_sum += value
            kernel.append(row)

        # Normalize kernel
        kernel = [[v / kernel_sum for v in row] for row in kernel]

        # Apply kernel
        for y in range(height):
            for x in range(width):
                value = 0

                for ky in range(-radius, radius + 1):
                    for kx in range(-radius, radius + 1):
                        px = max(0, min(width - 1, x + kx))
                        py = max(0, min(height - 1, y + ky))
                        value += data[py * width + px] * kernel[ky + radius][kx + radius]

                result[y * width + x] = int(value)

        return result

    def _preview_bumpmap_generation(self, rgba_data, width, height, bumpmap_type, method, strength, smooth, invert): #vers 2
        """Preview bumpmap generation in separate window"""
        try:
            # Generate preview bumpmap
            strength_val = strength / 100.0
            bumpmap_data = self._create_bumpmap_data(
                rgba_data, width, height, bumpmap_type, method, strength_val, smooth, invert
            )

            if not bumpmap_data:
                return

            # Create preview window
            preview = QDialog(self)
            preview.setWindowTitle("Bumpmap Preview")
            preview.setMinimumSize(400, 400)

            layout = QVBoxLayout(preview)

            # Preview label
            preview_label = QLabel()
            preview_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
            preview_label.setMinimumSize(350, 350)
            preview_label.setStyleSheet("border: 1px solid palette(mid); background: palette(base);")

            # Decode and display based on type
            if bumpmap_type == 0:  # Grayscale Height Map
                # Convert grayscale to RGB for display
                rgb_data = bytearray(width * height * 3)
                for i in range(width * height):
                    if i < len(bumpmap_data):
                        value = bumpmap_data[i]
                        rgb_data[i*3] = value
                        rgb_data[i*3+1] = value
                        rgb_data[i*3+2] = value

                image = QImage(bytes(rgb_data), width, height, width * 3, QImage.Format.Format_RGB888)

            elif bumpmap_type == 1:  # RGB Normal Map
                # Direct RGB display
                image = QImage(bytes(bumpmap_data), width, height, width * 3, QImage.Format.Format_RGB888)

            else:  # Both types - show normal map
                expected_gray = width * height
                expected_rgb = width * height * 3
                offset = 1 + expected_gray
                normal_data = bumpmap_data[offset:offset + expected_rgb]
                image = QImage(bytes(normal_data), width, height, width * 3, QImage.Format.Format_RGB888)

            pixmap = QPixmap.fromImage(image)
            preview_label.setPixmap(
                pixmap.scaled(350, 350,
                            Qt.AspectRatioMode.KeepAspectRatio,
                            Qt.TransformationMode.SmoothTransformation)
            )

            layout.addWidget(preview_label)

            # Info label showing type
            type_names = ["Grayscale Height Map", "RGB Normal Map", "Both Types"]
            info_label = QLabel(f"Type: {type_names[bumpmap_type]}")
            info_label.setStyleSheet("color: #888; padding: 5px;")
            info_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
            layout.addWidget(info_label)

            # Close button
            close_btn = QPushButton("Close")
            close_btn.clicked.connect(preview.accept)
            layout.addWidget(close_btn)

            preview.exec()

        except Exception as e:
            QMessageBox.warning(self, "Preview Error", f"Failed to preview:\n{str(e)}")

    def _delete_bumpmap(self): #vers 1
        """Delete bumpmap from selected texture"""
        if not self.selected_texture:
            return

        if not self._has_bumpmap_data(self.selected_texture):
            QMessageBox.information(self, "No Bumpmap", "This texture has no bumpmap")
            return

        reply = QMessageBox.question(
            self, "Delete Bumpmap",
            "Remove bumpmap from this texture?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
        )

        if reply == QMessageBox.StandardButton.Yes:
            # Save undo state
            self._save_undo_state("Delete bumpmap")

            # Remove bumpmap data
            if 'bumpmap_data' in self.selected_texture:
                del self.selected_texture['bumpmap_data']
            self.selected_texture['has_bumpmap'] = False

            # Clear bumpmap flag
            if 'raster_format_flags' in self.selected_texture:
                self.selected_texture['raster_format_flags'] &= ~0x10

            # Mark modified
            self._mark_as_modified()

            # Update UI
            self._update_texture_info(self.selected_texture)

            QMessageBox.information(self, "Success", "Bumpmap deleted")

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(
                    f"Deleted bumpmap from: {self.selected_texture.get('name', 'texture')}"
                )

    def _has_bumpmap_data(self, texture): #vers 1
        """Check if texture has bumpmap data"""
        if not texture:
            return False

        # Check explicit bumpmap data
        if 'bumpmap_data' in texture or texture.get('has_bumpmap', False):
            return True

        # Check format flags
        if 'raster_format_flags' in texture:
            flags = texture.get('raster_format_flags', 0)
            if flags & 0x10:  # Bit 4 = bumpmap/environment
                return True

        return False

    def _auto_generate_mipmaps_to_level(self, num_levels): #vers 3
        """Generate mipmaps down to specified level count"""
        if not self.selected_texture:
            return

        # Get main texture (level 0)
        main_rgba = self.selected_texture.get('rgba_data')
        if not main_rgba:
            QMessageBox.warning(self, "No Data", "Texture has no image data")
            return

        try:
            width = self.selected_texture['width']
            height = self.selected_texture['height']

            # Convert to QImage
            source_image = QImage(main_rgba, width, height, width * 4, QImage.Format.Format_RGBA8888)

            if source_image.isNull():
                QMessageBox.warning(self, "Error", "Failed to create source image")
                return

            self._save_undo_state("Generate mipmaps")
            # Clear existing mipmap levels except level 0
            if 'mipmap_levels' not in self.selected_texture:
                self.selected_texture['mipmap_levels'] = []

            # Keep level 0 if it exists
            level_0 = None
            for level in self.selected_texture['mipmap_levels']:
                if level['level'] == 0:
                    level_0 = level
                    break

            # Start fresh
            self.selected_texture['mipmap_levels'] = []

            # Add level 0
            if level_0:
                self.selected_texture['mipmap_levels'].append(level_0)
            else:
                self.selected_texture['mipmap_levels'].append({
                    'level': 0,
                    'width': width,
                    'height': height,
                    'rgba_data': main_rgba,
                    'compressed_data': None,
                    'compressed_size': len(main_rgba)
                })

            # Generate levels down to specified depth
            current_width = width // 2
            current_height = height // 2
            level_num = 1

            while level_num < num_levels and current_width >= 1 and current_height >= 1:
                # Scale down
                scaled_image = source_image.scaled(
                    current_width, current_height,
                    Qt.AspectRatioMode.IgnoreAspectRatio,
                    Qt.TransformationMode.SmoothTransformation
                )

                if scaled_image.isNull():
                    break

                # Convert to RGBA
                scaled_image = scaled_image.convertToFormat(QImage.Format.Format_RGBA8888)
                ptr = scaled_image.bits()
                ptr.setsize(scaled_image.sizeInBytes())
                rgba_data = bytes(ptr)

                # Add mipmap level
                mipmap_level = {
                    'level': level_num,
                    'width': current_width,
                    'height': current_height,
                    'rgba_data': rgba_data,
                    'compressed_data': None,
                    'compressed_size': len(rgba_data)
                }
                self.selected_texture['mipmap_levels'].append(mipmap_level)

                # Next level
                current_width = max(1, current_width // 2)
                current_height = max(1, current_height // 2)
                level_num += 1

            # Update mipmap count
            self.selected_texture['mipmaps'] = len(self.selected_texture['mipmap_levels'])

            # Update display
            self._update_texture_info(self.selected_texture)
            self._mark_as_modified()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Generated {level_num} mipmap levels")

            actual_levels = len(self.selected_texture['mipmap_levels'])
            min_dim = min(current_width * 2, current_height * 2)
            QMessageBox.information(self, "Success",
                f"Generated {actual_levels} mipmap levels\n"
                f"From {width}x{height} down to {min_dim}x{min_dim}")

        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to generate mipmaps: {str(e)}")

    def _import_normal_texture(self): #vers 1
        """Import normal texture (RGB/RGBA)"""
        file_path, _ = QFileDialog.getOpenFileName(
            self, "Import Normal Texture", "",
            "Image Files (*.png *.jpg *.bmp *.tga);;All Files (*)"
        )

        if not file_path:
            return

        try:
            from PyQt6.QtGui import QImage

            # Load image
            img = QImage(file_path)
            if img.isNull():
                QMessageBox.critical(self, "Error", "Failed to load image")
                return

            # Convert to RGBA8888
            img = img.convertToFormat(QImage.Format.Format_RGBA8888)

            # Get image data
            width = img.width()
            height = img.height()
            ptr = img.bits()
            ptr.setsize(img.sizeInBytes())
            rgba_data = bytes(ptr)

            # Check if image has alpha
            has_alpha = False
            for i in range(3, len(rgba_data), 4):
                if rgba_data[i] < 255:
                    has_alpha = True
                    break

            # Update texture
            self._save_undo_state("Import normal texture")
            self.selected_texture['width'] = width
            self.selected_texture['height'] = height
            self.selected_texture['rgba_data'] = rgba_data
            self.selected_texture['has_alpha'] = has_alpha

            # Update alpha name if texture now has alpha
            if has_alpha and 'alpha_name' not in self.selected_texture:
                self.selected_texture['alpha_name'] = self.selected_texture['name'] + 'a'

            self._update_texture_info(self.selected_texture)
            self._update_table_display()
            self._mark_as_modified()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                alpha_msg = "with alpha" if has_alpha else "no alpha"
                self.main_window.log_message(f"Imported normal texture: {width}x{height} ({alpha_msg})")

        except Exception as e:
            QMessageBox.critical(self, "Import Error", f"Failed to import: {str(e)}")

    def _import_alpha_texture(self): #vers 2
        """Import alpha channel - creates alpha if doesn't exist"""
        file_path, _ = QFileDialog.getOpenFileName(
            self, "Import Alpha Channel", "",
            "Image Files (*.png *.jpg *.bmp *.tga);;All Files (*)"
        )

        if not file_path:
            return

        try:
            from PyQt6.QtGui import QImage

            # Load alpha image
            img = QImage(file_path)
            if img.isNull():
                QMessageBox.critical(self, "Error", "Failed to load image")
                return

            # Get current texture dimensions
            tex_width = self.selected_texture.get('width', 0)
            tex_height = self.selected_texture.get('height', 0)

            # If no texture data exists, create blank texture with alpha
            if not self.selected_texture.get('rgba_data') or tex_width == 0 or tex_height == 0:
                # Use alpha image dimensions
                tex_width = img.width()
                tex_height = img.height()

                # Create blank RGB texture (gray)
                blank_rgba = bytearray()
                for _ in range(tex_width * tex_height):
                    blank_rgba.extend([128, 128, 128, 255])  # Gray with full alpha

                self.selected_texture['width'] = tex_width
                self.selected_texture['height'] = tex_height
                self.selected_texture['rgba_data'] = bytes(blank_rgba)

                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"Created blank texture {tex_width}x{tex_height} for alpha import")

            # Check dimensions match
            if img.width() != tex_width or img.height() != tex_height:
                reply = QMessageBox.question(
                    self, "Size Mismatch",
                    f"Alpha image is {img.width()}x{img.height()}, texture is {tex_width}x{tex_height}. Resize alpha?",
                    QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
                )

                if reply == QMessageBox.StandardButton.Yes:
                    img = img.scaled(tex_width, tex_height, Qt.AspectRatioMode.IgnoreAspectRatio,
                                Qt.TransformationMode.SmoothTransformation)
                else:
                    return

            # Convert to grayscale
            img = img.convertToFormat(QImage.Format.Format_Grayscale8)

            # Get alpha data
            ptr = img.bits()
            ptr.setsize(img.sizeInBytes())
            alpha_data = bytes(ptr)

            # Apply alpha to existing texture
            self._save_undo_state("Import alpha channel")

            rgba_data = bytearray(self.selected_texture['rgba_data'])

            # If current texture has no alpha (all 255), we're adding it
            has_existing_alpha = any(rgba_data[i] < 255 for i in range(3, len(rgba_data), 4))

            for i, alpha_val in enumerate(alpha_data):
                if i * 4 + 3 < len(rgba_data):
                    rgba_data[i * 4 + 3] = alpha_val  # Set alpha channel

            self.selected_texture['rgba_data'] = bytes(rgba_data)
            self.selected_texture['has_alpha'] = True

            # Add alpha name if not present
            if 'alpha_name' not in self.selected_texture:
                self.selected_texture['alpha_name'] = self.selected_texture['name'] + 'a'

            # Update format to support alpha if currently non-alpha format
            current_format = self.selected_texture.get('format', 'DXT1')
            if current_format in ['DXT1', 'RGB888', 'RGB565']:
                # Switch to alpha-capable format
                if 'DXT' in current_format:
                    self.selected_texture['format'] = 'DXT5'
                    format_msg = " (format changed to DXT5)"
                else:
                    self.selected_texture['format'] = 'ARGB8888'
                    format_msg = " (format changed to ARGB8888)"
            else:
                format_msg = ""

            self._update_texture_info(self.selected_texture)
            self._update_table_display()
            self._mark_as_modified()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                action = "Added" if not has_existing_alpha else "Replaced"
                self.main_window.log_message(f"{action} alpha channel from: {os.path.basename(file_path)}{format_msg}")

        except Exception as e:
            QMessageBox.critical(self, "Import Error", f"Failed to import alpha: {str(e)}")

    def _load_img_txd_list(self): #vers 2
        """Load TXD files from IMG archive"""
        try:
            # Safety check for standalone mode
            if self.standalone_mode or not hasattr(self, 'txd_list_widget') or self.txd_list_widget is None:
                return

            self.txd_list_widget.clear()
            self.txd_list = []

            if not self.current_img:
                return

            for entry in self.current_img.entries:
                if entry.name.lower().endswith('.txd'):
                    self.txd_list.append(entry)
                    item = QListWidgetItem(entry.name)
                    item.setData(Qt.ItemDataRole.UserRole, entry)
                    size_kb = entry.size / 1024
                    item.setToolTip(f"{entry.name}\nSize: {size_kb:.1f} KB")
                    self.txd_list_widget.addItem(item)

            hdr = getattr(self, '_txd_list_header', None)
            if hdr:
                hdr.setText(f"TXD Files  ({len(self.txd_list)})")
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Found {len(self.txd_list)} TXD files")
        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Error loading TXD list: {str(e)}")

    def _create_blank_texture(self, width, height, with_alpha=False): #vers 3
        """Create blank RGBA texture data with optional alpha"""
        if with_alpha:
            # Gray with transparent alpha (128)
            return bytes([128, 128, 128, 128] * (width * height))
        else:
            # Gray with full opaque alpha (255)
            return bytes([128, 128, 128, 255] * (width * height))

    def _create_empty_txd_data(self): #vers 1
        """Create minimal empty TXD structure"""
        import struct
        # RenderWare TXD header (simplified)
        header = struct.pack('<III', 0x16, 0, 0x1803FFFF)  # Type, Size, Version
        return header

    def _create_new_texture_entry(self): #vers 3
        """Create new blank texture with size dialog"""
        if not self.current_img and not self.current_txd_data:
            QMessageBox.warning(self, "No TXD", "Please open or create a TXD file first")
            return

        # Ask for texture size
        dialog = QDialog(self)
        dialog.setWindowTitle("Create New Texture")
        dialog.setMinimumWidth(350)

        layout = QVBoxLayout(dialog)

        # Name input
        name_group = QGroupBox("Texture Name")
        name_layout = QVBoxLayout()

        name_input = QLineEdit()
        name_input.setPlaceholderText("Enter texture name")
        name_input.setText(f"texture_{len(self.texture_list) + 1}")
        name_layout.addWidget(name_input)

        name_group.setLayout(name_layout)
        layout.addWidget(name_group)

        # Size selection
        size_group = QGroupBox("Texture Size")
        size_layout = QVBoxLayout()

        # Common size presets
        preset_layout = QHBoxLayout()
        preset_layout.addWidget(QLabel("Presets:"))

        preset_combo = QComboBox()
        preset_combo.addItems([
            "Custom",
            "64 x 64",
            "128 x 128",
            "256 x 256",
            "512 x 512",
            "1024 x 1024",
            "2048 x 2048",
            "512 x 256",
            "1024 x 512",
            "256 x 128"
        ])
        preset_combo.setCurrentIndex(3)  # Default 256x256
        preset_layout.addWidget(preset_combo)
        size_layout.addLayout(preset_layout)

        # Custom size inputs
        custom_layout = QFormLayout()

        width_spin = QSpinBox()
        width_spin.setRange(1, 4096)
        width_spin.setValue(256)
        width_spin.setSingleStep(64)
        custom_layout.addRow("Width:", width_spin)

        height_spin = QSpinBox()
        height_spin.setRange(1, 4096)
        height_spin.setValue(256)
        height_spin.setSingleStep(64)
        custom_layout.addRow("Height:", height_spin)

        size_layout.addLayout(custom_layout)

        # Update spinboxes when preset changes
        def update_from_preset(index):  #vers 1
            if index == 0:  # Custom
                width_spin.setEnabled(True)
                height_spin.setEnabled(True)
            else:
                width_spin.setEnabled(False)
                height_spin.setEnabled(False)

                preset_sizes = {
                    1: (64, 64),
                    2: (128, 128),
                    3: (256, 256),
                    4: (512, 512),
                    5: (1024, 1024),
                    6: (2048, 2048),
                    7: (512, 256),
                    8: (1024, 512),
                    9: (256, 128)
                }

                if index in preset_sizes:
                    w, h = preset_sizes[index]
                    width_spin.setValue(w)
                    height_spin.setValue(h)

        preset_combo.currentIndexChanged.connect(update_from_preset)
        update_from_preset(3)  # Set initial state

        size_group.setLayout(size_layout)
        layout.addWidget(size_group)

        # Color selection
        color_group = QGroupBox("Initial Color")
        color_layout = QHBoxLayout()

        color_btn = QPushButton("Choose Color")
        selected_color = self._get_ui_color('viewport_text')  # Default gray

        def choose_color():  #vers 1
            nonlocal selected_color
            color = QColorDialog.getColor(selected_color, dialog, "Choose Texture Color")
            if color.isValid():
                selected_color = color
                color_btn.setStyleSheet(f"background-color: {color.name()}; color: white;")
                color_btn.setText(color.name().upper())

        color_btn.clicked.connect(choose_color)
        color_btn.setStyleSheet(f"background-color: {selected_color.name()}; color: white;")
        color_btn.setText(selected_color.name().upper())
        color_layout.addWidget(color_btn)

        color_group.setLayout(color_layout)
        layout.addWidget(color_group)

        # Buttons
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        create_btn = QPushButton("Create")
        create_btn.setDefault(True)
        create_btn.clicked.connect(dialog.accept)
        button_layout.addWidget(create_btn)

        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(dialog.reject)
        button_layout.addWidget(cancel_btn)

        layout.addLayout(button_layout)

        # Execute dialog
        if dialog.exec() == QDialog.DialogCode.Accepted:
            texture_name = name_input.text().strip()
            if not texture_name:
                QMessageBox.warning(self, "Invalid Name", "Please enter a texture name")
                return

            width = width_spin.value()
            height = height_spin.value()

            # Create blank RGBA data with selected color
            rgba_data = bytearray(width * height * 4)
            r, g, b = selected_color.red(), selected_color.green(), selected_color.blue()

            for i in range(0, len(rgba_data), 4):
                rgba_data[i] = r
                rgba_data[i+1] = g
                rgba_data[i+2] = b
                rgba_data[i+3] = 255  # Full alpha

            # Create new texture entry
            new_texture = {
                'name': texture_name,
                'alpha_name': texture_name + 'a',
                'width': width,
                'height': height,
                'format': 'ARGB8888',
                'depth': 32,
                'has_alpha': True,
                'mipmaps': 1,
                'rgba_data': bytes(rgba_data),
                'mipmap_levels': [{
                    'level': 0,
                    'width': width,
                    'height': height,
                    'rgba_data': bytes(rgba_data),
                    'compressed_data': None,
                    'compressed_size': len(rgba_data)
                }]
            }

            self._save_undo_state("Create texture")
            # Add to texture list
            self.texture_list.append(new_texture)
            self._add_texture_to_table(new_texture)

            # Mark as modified
            self._mark_as_modified()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(
                    f"Created new texture: {texture_name} ({width}x{height})"
                )

            QMessageBox.information(self, "Success",
                f"Created new texture:\n{texture_name}\nSize: {width}x{height}")

    def _create_new_txd(self): #vers 1
        """Create a new empty TXD file"""
        name, ok = QInputDialog.getText(self, "New TXD", "Enter TXD filename (without .txd):")
        if ok and name:
            if not name.lower().endswith('.txd'):
                name += '.txd'

            # Create minimal TXD structure
            self.current_txd_name = name
            self.current_txd_data = self._create_empty_txd_data()
            self.texture_list = []
            self.texture_table.setRowCount(0)

            self.setWindowTitle(f"TXD Workshop: {name}")
            self.save_txd_btn.setEnabled(True)

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Created new TXD: {name}")

    def _delete_texture(self): #vers 4
        """Delete texture with granular component selection"""
        if not self.selected_texture:
            QMessageBox.warning(self, "No Selection", "Please select a texture first")
            return

        texture_name = self.selected_texture.get('name', 'texture')

        # Check what components exist
        has_alpha = self.selected_texture.get('has_alpha', False)
        has_mipmaps = len(self.selected_texture.get('mipmap_levels', [])) > 1  # More than 1 level
        has_bumpmap = self.selected_texture.get('has_bumpmap', False)
        has_reflection = self.selected_texture.get('has_reflection', False) or bool(self.selected_texture.get('reflection_map', b''))

        # Check if texture has any deletable components
        has_components = any([has_alpha, has_mipmaps, has_bumpmap, has_reflection])

        # Create dialog
        dialog = QDialog(self)
        dialog.setWindowTitle(f"Delete Options: {texture_name}")
        dialog.setMinimumWidth(450)

        layout = QVBoxLayout(dialog)

        # Header
        header = QLabel(f"Select what to delete from:\n{texture_name}")
        header.setFont(QFont("Arial", 11, QFont.Weight.Bold))
        header.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(header)

        layout.addSpacing(10)

        # Main texture option
        main_group = QGroupBox("Main Texture")
        main_layout = QVBoxLayout()

        delete_all_radio = QRadioButton("Delete entire texture (all components)")
        delete_all_radio.setChecked(True)
        delete_all_radio.setToolTip("Remove this texture completely from the TXD")
        main_layout.addWidget(delete_all_radio)

        keep_main_radio = QRadioButton("Keep main texture, delete selected components only")
        keep_main_radio.setEnabled(has_components)
        if not has_components:
            keep_main_radio.setToolTip("No deletable components available")
        else:
            keep_main_radio.setToolTip("Keep the base texture but remove selected extra data")
        main_layout.addWidget(keep_main_radio)

        main_group.setLayout(main_layout)
        layout.addWidget(main_group)

        # Components group (only show if has components)
        components_group = QGroupBox("Components to Delete")
        components_layout = QVBoxLayout()

        # Alpha channel
        alpha_check = None
        if has_alpha:
            alpha_name = self.selected_texture.get('alpha_name', texture_name + 'a')
            alpha_check = QCheckBox(f"Delete Alpha Channel: {alpha_name}")
            alpha_check.setChecked(True)
            alpha_check.setEnabled(False)  # Disabled until "Keep main" is selected
            alpha_check.setToolTip("Remove alpha channel transparency data")
            components_layout.addWidget(alpha_check)

        # Mipmaps
        mipmap_check = None
        if has_mipmaps:
            num_levels = len(self.selected_texture.get('mipmap_levels', []))
            mipmap_check = QCheckBox(f"Delete Mipmaps ({num_levels} levels)")
            mipmap_check.setChecked(True)
            mipmap_check.setEnabled(False)
            mipmap_check.setToolTip("Remove all mipmap LOD levels")
            components_layout.addWidget(mipmap_check)

        # Bumpmap
        bumpmap_check = None
        if has_bumpmap:
            bumpmap_type = self.selected_texture.get('bumpmap_type', 0)
            type_names = ['Height Map', 'Normal Map', 'Combined']
            bumpmap_check = QCheckBox(f"Delete Bumpmap ({type_names[bumpmap_type]})")
            bumpmap_check.setChecked(True)
            bumpmap_check.setEnabled(False)
            bumpmap_check.setToolTip("Remove bumpmap data")
            components_layout.addWidget(bumpmap_check)

        # Reflection maps
        reflection_check = None
        if has_reflection:
            reflection_check = QCheckBox("Delete Reflection Maps")
            reflection_check.setChecked(True)
            reflection_check.setEnabled(False)
            reflection_check.setToolTip("Remove reflection and Fresnel maps")
            components_layout.addWidget(reflection_check)

        # Show message if no components
        if not has_components:
            no_components_label = QLabel("This texture has no extra components to delete.")
            no_components_label.setStyleSheet("color: #888; font-style: italic; padding: 10px;")
            components_layout.addWidget(no_components_label)

        components_group.setLayout(components_layout)
        layout.addWidget(components_group)

        # Enable/disable component checkboxes based on radio selection
        def update_component_state():  #vers 1
            enabled = keep_main_radio.isChecked() and has_components
            if alpha_check:
                alpha_check.setEnabled(enabled)
            if mipmap_check:
                mipmap_check.setEnabled(enabled)
            if bumpmap_check:
                bumpmap_check.setEnabled(enabled)
            if reflection_check:
                reflection_check.setEnabled(enabled)

        delete_all_radio.toggled.connect(update_component_state)
        keep_main_radio.toggled.connect(update_component_state)

        layout.addSpacing(10)

        # Info label
        info_label = QLabel()
        info_label.setStyleSheet("color: #888; font-style: italic; padding: 5px;")
        info_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        info_label.setWordWrap(True)

        def update_info_text():  #vers 1
            if delete_all_radio.isChecked():
                info_label.setText("The entire texture will be removed from the TXD.")
            else:
                info_label.setText("The main texture will be kept. Uncheck components to preserve them.")

        delete_all_radio.toggled.connect(update_info_text)
        keep_main_radio.toggled.connect(update_info_text)
        update_info_text()

        layout.addWidget(info_label)

        layout.addSpacing(10)

        # Buttons
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(dialog.reject)
        button_layout.addWidget(cancel_btn)

        delete_btn = QPushButton("Delete")
        delete_btn.setStyleSheet("background-color: palette(highlight); color: palette(highlightedText); font-weight: bold;")
        delete_btn.clicked.connect(dialog.accept)
        button_layout.addWidget(delete_btn)

        layout.addLayout(button_layout)

        # Show dialog
        if dialog.exec() != QDialog.DialogCode.Accepted:
            return

        # Process deletion
        self._save_undo_state("Delete texture")
        try:
            if delete_all_radio.isChecked():
                # Delete entire texture
                if self.selected_texture in self.texture_list:
                    self.texture_list.remove(self.selected_texture)

                self.selected_texture = None
                self._reload_texture_table()
                self._mark_as_modified()

                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"Deleted entire texture: {texture_name}")

                QMessageBox.information(self, "Deleted",
                    f"Texture '{texture_name}' has been completely removed.")

            else:
                # Keep main texture, delete selected components
                components_deleted = []

                # Delete alpha
                if alpha_check and alpha_check.isChecked() and has_alpha:
                    self.selected_texture['has_alpha'] = False
                    if 'alpha_name' in self.selected_texture:
                        del self.selected_texture['alpha_name']
                    components_deleted.append("alpha channel")

                # Delete mipmaps
                if mipmap_check and mipmap_check.isChecked() and has_mipmaps:
                    # Keep only the main level (level 0) if it exists
                    mipmap_levels = self.selected_texture.get('mipmap_levels', [])
                    if mipmap_levels:
                        # Keep first level only
                        self.selected_texture['mipmap_levels'] = [mipmap_levels[0]] if mipmap_levels else []
                    else:
                        self.selected_texture['mipmap_levels'] = []
                    components_deleted.append(f"mipmap levels")

                # Delete bumpmap
                if bumpmap_check and bumpmap_check.isChecked() and has_bumpmap:
                    self.selected_texture['has_bumpmap'] = False
                    self.selected_texture['bumpmap_data'] = b''
                    self.selected_texture['bumpmap_type'] = 0
                    components_deleted.append("bumpmap")

                # Delete reflection
                if reflection_check and reflection_check.isChecked() and has_reflection:
                    self.selected_texture['has_reflection'] = False
                    self.selected_texture['reflection_map'] = b''
                    if 'fresnel_map' in self.selected_texture:
                        self.selected_texture['fresnel_map'] = b''
                    components_deleted.append("reflection maps")

                if components_deleted:
                    # Update texture info display
                    self._update_texture_info(self.selected_texture)

                    # Reload table to show changes
                    self._reload_texture_table()

                    # Mark as modified
                    self._mark_as_modified()

                    # Log
                    components_str = ", ".join(components_deleted)
                    if self.main_window and hasattr(self.main_window, 'log_message'):
                        self.main_window.log_message(f"Deleted from {texture_name}: {components_str}")

                    QMessageBox.information(self, "Components Deleted",
                        f"Deleted from '{texture_name}':\n\n{components_str}\n\nMain texture preserved.")
                else:
                    QMessageBox.information(self, "No Changes",
                        "No components were selected for deletion.")

        except Exception as e:
            QMessageBox.critical(self, "Delete Error", f"Failed to delete: {str(e)}")

    def _mark_as_modified(self): #vers 1
        """Mark the TXD as modified and enable save button"""
        self.save_txd_btn.setEnabled(True)
        self.save_txd_btn.setStyleSheet("background-color: palette(highlight); font-weight: bold;")
        current_title = self.windowTitle()
        if not current_title.endswith("*"):
            self.setWindowTitle(current_title + "*")

    def _open_mipmap_manager(self): #vers 1
        """Open Mipmap Manager window for selected texture"""
        if not self.selected_texture:
            QMessageBox.warning(self, "No Selection", "Please select a texture first")
            return

        mipmap_levels = self.selected_texture.get('mipmap_levels', [])
        if not mipmap_levels:
            QMessageBox.information(self, "No Mipmaps", "This texture has no mipmap levels")
            return

        # Create and show Mipmap Manager window
        manager = MipmapManagerWindow(self, self.selected_texture, self.main_window)
        manager.show()

        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message(f"Opened Mipmap Manager for: {self.selected_texture['name']}")

    def _on_texture_selected(self): #vers 7
        """Handle texture selection"""
        try:
            row = self.texture_table.currentRow()

            # Invalid selection - disable everything
            if row < 0 or row >= len(self.texture_list):
                self.selected_texture = None
                self.export_btn.setEnabled(False)
                self.switch_btn.setEnabled(False)
                self.invert_btn.setEnabled(False)
                self.gen_alpha_btn.setEnabled(False)

                # Disable all optional buttons
                if hasattr(self, 'switch_btn'):
                    self.switch_btn.setEnabled(False)
                if hasattr(self, 'gen_alpha_btn'):
                    self.gen_alpha_btn.setEnabled(False)
                if hasattr(self, 'props_btn'):
                    self.props_btn.setEnabled(False)
                if hasattr(self, 'duplicate_texture_btn'):
                    self.duplicate_texture_btn.setEnabled(False)
                if hasattr(self, 'delete_texture_btn'):
                    self.delete_texture_btn.setEnabled(False)
                if hasattr(self, 'resize_btn'):
                    self.resize_btn.setEnabled(False)
                if hasattr(self, 'upscale_btn'):
                    self.upscale_btn.setEnabled(False)
                if hasattr(self, 'format_combo'):
                    self.format_combo.setEnabled(False)
                if hasattr(self, 'compress_btn'):
                    self.compress_btn.setEnabled(False)
                if hasattr(self, 'uncompress_btn'):
                    self.uncompress_btn.setEnabled(False)
                if hasattr(self, 'bitdepth_btn'):
                    self.bitdepth_btn.setEnabled(False)

                # Disable mipmap buttons
                if hasattr(self, 'create_mipmaps_btn'):
                    self.create_mipmaps_btn.setEnabled(False)
                if hasattr(self, 'remove_mipmaps_btn'):
                    self.remove_mipmaps_btn.setEnabled(False)
                if hasattr(self, 'show_mipmaps_btn'):
                    self.show_mipmaps_btn.setEnabled(False)

                # Disable bumpmap buttons
                if hasattr(self, 'view_bumpmap_btn'):
                    self.view_bumpmap_btn.setEnabled(False)
                if hasattr(self, 'export_bumpmap_btn'):
                    self.export_bumpmap_btn.setEnabled(False)
                if hasattr(self, 'import_bumpmap_btn'):
                    self.import_bumpmap_btn.setEnabled(False)

                # Disable all transform buttons in both panels
                self._set_transform_buttons_enabled(False)

                return

            # Valid selection - get texture data
            self.selected_texture = self.texture_list[row]

            tex_name = self.selected_texture.get('name', '')
            has_alpha = self.selected_texture.get('has_alpha', False)

            # Restore saved view state for this texture, or default to Normal
            saved_state = self.texture_view_states.get(tex_name, 0)
            self._current_view_state = saved_state

            # Update switch button text
            state_labels = ["Normal", "Alpha", "Both", "Overlay"]
            self.switch_btn.setText(state_labels[saved_state])
            self.switch_btn.setEnabled(True)

            # Enable [Inv] only if in Alpha view and has alpha
            #self.invert_btn.setEnabled(saved_state == 1 and has_alpha)
            self.invert_btn.setEnabled((saved_state == 1 or saved_state == 3) and has_alpha)


            # Enable [+] button
            self.gen_alpha_btn.setEnabled(True)

            # Check mipmap state
            mipmap_levels = self.selected_texture.get('mipmap_levels', [])
            num_levels = len(mipmap_levels)
            has_mipmaps = num_levels > 1

            # Check bumpmap state
            has_bumpmap = self._has_bumpmap_data(self.selected_texture) if hasattr(self, '_has_bumpmap_data') else False
            can_support_bumpmap = is_bumpmap_supported(self.txd_version_id, self.txd_device_id) if self.txd_version_id else False

            # Update display FIRST (this should show the texture preview)
            self._update_texture_info(self.selected_texture)

            # Debug log
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(
                    f"Selected: {self.selected_texture.get('name')} | "
                    f"Levels: {num_levels} | Mipmaps: {has_mipmaps} | Bumpmap: {has_bumpmap}"
                )

            # Enable basic buttons
            self.export_btn.setEnabled(True)

            if hasattr(self, 'props_btn'):
                self.props_btn.setEnabled(True)
            if hasattr(self, 'info_btn'):
                self.info_btn.setEnabled(True)
            if hasattr(self, 'duplicate_texture_btn'):
                self.duplicate_texture_btn.setEnabled(True)
            if hasattr(self, 'delete_texture_btn'):
                self.delete_texture_btn.setEnabled(True)
            if hasattr(self, 'resize_btn'):
                self.resize_btn.setEnabled(True)
            if hasattr(self, 'upscale_btn'):
                self.upscale_btn.setEnabled(True)
            if hasattr(self, 'format_combo'):
                self.format_combo.setEnabled(True)
                # Sync combo to current texture format
                fmt = self.selected_texture.get('format', '')
                fmt_map = {  # map stored format names to combo entries
                    'ARGB8888': 'ARGB8888', 'RGB888': 'RGB888',
                    'RGB565':   'RGB565',   'ARGB1555': 'ARGB1555',
                    'ARGB4444': 'ARGB4444', 'RGB555':   'RGB565',
                    'PAL8':     'ARGB8888', 'PAL4':     'ARGB8888',
                    'LUM8':     'RGB565',   'A8L8':     'ARGB8888',
                    'DXT1': 'DXT1', 'DXT2': 'DXT3', 'DXT3': 'DXT3',
                    'DXT4': 'DXT5', 'DXT5': 'DXT5',
                    # PS2 native formats — map to nearest PC equivalent for combo display
                    'PSMT8':        'ARGB8888', 'PSMT4':        'ARGB8888',
                    'PSMT8-PAL8':   'ARGB8888', 'PSMT4-PAL4':   'ARGB8888',
                    'PSMCT32':      'ARGB8888', 'PSMCT16':      'ARGB1555',
                    'PSMCT16S':     'ARGB1555',
                }
                combo_text = fmt_map.get(fmt, fmt)
                idx = self.format_combo.findText(combo_text)
                if idx >= 0:
                    self.format_combo.blockSignals(True)
                    self.format_combo.setCurrentIndex(idx)
                    self.format_combo.blockSignals(False)
            if hasattr(self, 'compress_btn'):
                self.compress_btn.setEnabled(True)
            if hasattr(self, 'uncompress_btn'):
                self.uncompress_btn.setEnabled(True)
            if hasattr(self, 'bitdepth_btn'):
                self.bitdepth_btn.setEnabled(True)

            if hasattr(self, 'switch_btn'):
                has_alpha = self.selected_texture.get('has_alpha', False)
                self.switch_btn.setEnabled(True)  # Always enabled now

            # NEW: Always enable gen_alpha_btn when texture selected
            if hasattr(self, 'gen_alpha_btn'):
                self.gen_alpha_btn.setEnabled(True)

            # Mipmap buttons
            if hasattr(self, 'create_mipmaps_btn'):
                self.create_mipmaps_btn.setEnabled(not has_mipmaps)
            if hasattr(self, 'remove_mipmaps_btn'):
                self.remove_mipmaps_btn.setEnabled(has_mipmaps)
            if hasattr(self, 'show_mipmaps_btn'):
                self.show_mipmaps_btn.setEnabled(has_mipmaps)

            # Bumpmap buttons
            if hasattr(self, 'view_bumpmap_btn'):
                # ALWAYS enable Manage button so user can generate/import bumpmaps
                self.view_bumpmap_btn.setEnabled(can_support_bumpmap)
            if hasattr(self, 'export_bumpmap_btn'):
                # Only enable export if bumpmap exists
                self.export_bumpmap_btn.setEnabled(has_bumpmap)
            if hasattr(self, 'import_bumpmap_btn'):
                # Only enable import if version supports bumpmaps
                self.import_bumpmap_btn.setEnabled(can_support_bumpmap)

            # Enable all transform buttons in BOTH icon and text panels
            self._set_transform_buttons_enabled(True)
            if hasattr(self, 'paste_btn'):
                self.paste_btn.setEnabled(True)
            if hasattr(self, 'paint_btn'):
                self.paint_btn.setEnabled(True)
            if hasattr(self, 'filters_btn'):
                self.filters_btn.setEnabled(True)

            # Update display
            self._update_texture_info(self.selected_texture)

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Selection error: {str(e)}")
                import traceback
                self.main_window.log_message(traceback.format_exc())

    def _reload_texture_table(self): #vers 4
        """Reload texture table — preserves row selection after reload."""
        # Remember which texture was selected by object identity
        selected_name = (self.selected_texture.get('name') 
                         if self.selected_texture else None)
        self.texture_table.setRowCount(0)
        for tex in self.texture_list:
            self._add_texture_to_table(tex)
        # Restore selection
        if selected_name:
            for row, tex in enumerate(self.texture_list):
                if tex.get('name') == selected_name:
                    self.texture_table.selectRow(row)
                    break

    def _save_undo_state(self, action_name): #vers 3
        """
        Save current state to undo stack - FIXED: Properly preserves binary data

        The issue was that copy.deepcopy was losing binary data fields like
        compressed_data and original_bgra_data, causing corruption on undo.

        Args:
            action_name: Description of the action being saved
        """
        texture_list_copy = []

        for texture in self.texture_list:
            tex_copy = texture.copy()

            # CRITICAL: Explicitly preserve all binary data fields
            if 'compressed_data' in texture:
                tex_copy['compressed_data'] = texture['compressed_data']

            if 'original_bgra_data' in texture:
                tex_copy['original_bgra_data'] = texture['original_bgra_data']

            if 'rgba_data' in texture:
                tex_copy['rgba_data'] = texture['rgba_data']

            if 'bumpmap_data' in texture:
                tex_copy['bumpmap_data'] = texture['bumpmap_data']

            if 'reflection_map' in texture:
                tex_copy['reflection_map'] = texture['reflection_map']

            if 'fresnel_map' in texture:
                tex_copy['fresnel_map'] = texture['fresnel_map']

            # Copy mipmap levels with binary data
            if 'mipmap_levels' in texture:
                mipmap_copy = []
                for level in texture['mipmap_levels']:
                    level_copy = level.copy()

                    if 'compressed_data' in level:
                        level_copy['compressed_data'] = level['compressed_data']

                    if 'original_bgra_data' in level:
                        level_copy['original_bgra_data'] = level['original_bgra_data']

                    if 'rgba_data' in level:
                        level_copy['rgba_data'] = level['rgba_data']

                    mipmap_copy.append(level_copy)

                tex_copy['mipmap_levels'] = mipmap_copy

            texture_list_copy.append(tex_copy)

        state = {
            'action': action_name,
            'texture_list': texture_list_copy
        }

        self.undo_stack.append(state)

        # Limit undo stack to 10 items
        if len(self.undo_stack) > 10:
            self.undo_stack.pop(0)
        self._set_undo_enabled()

    def _set_undo_enabled(self): #vers 1
        """Undo buttons follow the undo stack."""
        for btn in getattr(self, '_undo_buttons', []):
            btn.setEnabled(bool(self.undo_stack))

    def _undo_last_action(self): #vers 3
        """Undo the last action from undo stack"""
        if not self.undo_stack:
            return

        try:
            row = self.texture_table.currentRow()
            last_state = self.undo_stack.pop()
            self.texture_list = last_state.get('texture_list', [])
            self.selected_texture = None
            self._reload_texture_table()
            if self.texture_list:
                self.texture_table.selectRow(min(max(row, 0), len(self.texture_list) - 1))
            self._mark_as_modified()
            self._set_undo_enabled()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("Undo applied")

        except Exception as e:
            QMessageBox.critical(self, "Undo Error", f"Failed to undo: {str(e)}")

    def _auto_generate_mipmaps(self): #vers 3
        """Auto-generate all mipmap levels from main texture"""
        if not self.selected_texture:
            QMessageBox.warning(self, "No Selection", "Please select a texture first")
            return

        # Get main texture (level 0)
        main_rgba = self.selected_texture.get('rgba_data')
        if not main_rgba:
            QMessageBox.warning(self, "No Data", "Texture has no image data")
            return

        try:
            width = self.selected_texture['width']
            height = self.selected_texture['height']

            # Convert to QImage
            source_image = QImage(main_rgba, width, height, width * 4, QImage.Format.Format_RGBA8888)

            if source_image.isNull():
                QMessageBox.warning(self, "Error", "Failed to create source image")
                return

            self._save_undo_state("Generate mipmaps")
            # Clear existing mipmap levels except level 0
            if 'mipmap_levels' not in self.selected_texture:
                self.selected_texture['mipmap_levels'] = []

            # Keep level 0 if it exists
            level_0 = None
            for level in self.selected_texture['mipmap_levels']:
                if level['level'] == 0:
                    level_0 = level
                    break

            # Start fresh
            self.selected_texture['mipmap_levels'] = []

            # Add level 0
            if level_0:
                self.selected_texture['mipmap_levels'].append(level_0)
            else:
                self.selected_texture['mipmap_levels'].append({
                    'level': 0,
                    'width': width,
                    'height': height,
                    'rgba_data': main_rgba,
                    'compressed_data': None,
                    'compressed_size': len(main_rgba)
                })

            # Generate remaining levels
            current_width = width // 2
            current_height = height // 2
            level_num = 1

            while current_width >= 1 and current_height >= 1:
                # Scale down
                scaled_image = source_image.scaled(
                    current_width, current_height,
                    Qt.AspectRatioMode.IgnoreAspectRatio,
                    Qt.TransformationMode.SmoothTransformation
                )

                if scaled_image.isNull():
                    break

                # Convert to RGBA
                scaled_image = scaled_image.convertToFormat(QImage.Format.Format_RGBA8888)
                ptr = scaled_image.bits()
                ptr.setsize(scaled_image.sizeInBytes())
                rgba_data = bytes(ptr)

                # Add mipmap level
                mipmap_level = {
                    'level': level_num,
                    'width': current_width,
                    'height': current_height,
                    'rgba_data': rgba_data,
                    'compressed_data': None,
                    'compressed_size': len(rgba_data)
                }
                self.selected_texture['mipmap_levels'].append(mipmap_level)
                level_num += 1
                if current_width == 1 and current_height == 1:
                    break
                current_width = max(1, current_width // 2)
                current_height = max(1, current_height // 2)

            # Update mipmap count
            self.selected_texture['mipmaps'] = len(self.selected_texture['mipmap_levels'])

            # Update display
            self._update_texture_info(self.selected_texture)
            self._mark_as_modified()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Generated {level_num} mipmap levels")

            QMessageBox.information(self, "Success",
                f"Generated {level_num} mipmap levels\n"
                f"From {width}x{height} down to {current_width}x{current_height}")

        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to generate mipmaps: {str(e)}")

    def _generate_alpha_mask(self): #vers 2
        """Generate alpha mask from texture luminosity"""
        if not self.selected_texture:
            QMessageBox.warning(self, "No Selection", "Please select a texture first")
            return

        # Check if already has alpha
        if self.selected_texture.get('has_alpha', False):
            reply = QMessageBox.question(self, "Replace Alpha?",
                "This texture already has an alpha channel.\n\n"
                "Replace existing alpha with luminosity-based mask?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)

            if reply != QMessageBox.StandardButton.Yes:
                return

        try:
            rgba_data = self.selected_texture.get('rgba_data')
            if not rgba_data:
                QMessageBox.warning(self, "No Data", "Texture has no image data")
                return

            width = self.selected_texture.get('width', 0)
            height = self.selected_texture.get('height', 0)

            if width == 0 or height == 0:
                QMessageBox.warning(self, "Invalid Size", "Texture has invalid dimensions")
                return

            # Save undo state
            self._save_undo_state("Generate alpha mask from luminosity")

            # Generate alpha from luminosity
            new_rgba = bytearray(rgba_data)

            for i in range(0, len(new_rgba), 4):
                r = new_rgba[i]
                g = new_rgba[i + 1]
                b = new_rgba[i + 2]

                # Calculate luminosity: 0.299*R + 0.587*G + 0.114*B
                luminosity = int(0.299 * r + 0.587 * g + 0.114 * b)
                new_rgba[i + 3] = luminosity

            # Update texture
            self.selected_texture['rgba_data'] = bytes(new_rgba)
            self.selected_texture['has_alpha'] = True

            # Add alpha name if not present
            if 'alpha_name' not in self.selected_texture:
                self.selected_texture['alpha_name'] = self.selected_texture['name'] + 'a'

            # Update format to support alpha
            current_format = self.selected_texture.get('format', 'DXT1')
            if current_format in ['DXT1', 'RGB888', 'RGB565']:
                if 'DXT' in current_format:
                    self.selected_texture['format'] = 'DXT5'
                    format_msg = " (format changed to DXT5)"
                else:
                    self.selected_texture['format'] = 'ARGB8888'
                    format_msg = " (format changed to ARGB8888)"
            else:
                format_msg = ""

            # Update display
            self._update_texture_info(self.selected_texture)
            self._update_table_display()
            self._mark_as_modified()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Generated alpha mask from luminosity{format_msg}")

        except Exception as e:
            QMessageBox.critical(self, "Generation Error", f"Failed to generate alpha mask:\n{str(e)}")

    def _toggle_alpha_invert(self): #vers 2
        """Toggle alpha channel color inversion - WORKS FOR ALPHA AND OVERLAY"""
        if not self.selected_texture:
            return

        # Allow invert in Alpha view (1) OR Overlay view (3)
        if self._current_view_state not in [1, 3]:
            return

        self._invert_alpha = not self._invert_alpha
        self.invert_btn.setChecked(self._invert_alpha)

        # Refresh display
        self._update_texture_info(self.selected_texture)

        if self.main_window and hasattr(self.main_window, 'log_message'):
            status = "enabled" if self._invert_alpha else "disabled"
            view_name = "Alpha" if self._current_view_state == 1 else "Overlay"
            self.main_window.log_message(f"Alpha invert {status} ({view_name} view)")

    def load_from_img_archive(self, img_path): #vers 1
        """Load TXD list from IMG archive"""
        try:
            if self.main_window and hasattr(self.main_window, 'current_img'):
                self.current_img = self.main_window.current_img
            else:
                from apps.methods.img_core_classes import IMGFile
                self.current_img = IMGFile(img_path)
                self.current_img.open()

            img_name = os.path.basename(img_path)
            self.setWindowTitle(f"TXD Workshop: {img_name}")
            self._load_img_txd_list()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"TXD Workshop loaded: {img_name}")
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to load IMG: {str(e)}")

    def _show_txd_info(self): #vers 4
        """Show TXD Workshop information dialog - About and capabilities"""
        dialog = QDialog(self)
        dialog.setWindowTitle("About TXD Workshop")
        dialog.setMinimumWidth(600)
        dialog.setMinimumHeight(500)

        layout = QVBoxLayout(dialog)
        layout.setSpacing(15)

        # Header
        header = QLabel(f"TXD Workshop - {App_name}")
        header.setFont(QFont("Arial", 14, QFont.Weight.Bold))
        header.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(header)

        # Author info
        author_label = QLabel("Author: X-Seti")
        author_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(author_label)

        # Version info
        version_label = QLabel("Version: 1.5 - October 2025")
        version_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(version_label)

        layout.addWidget(QLabel(""))  # Spacer

        # Capabilities section
        capabilities = QTextEdit()
        capabilities.setReadOnly(True)
        capabilities.setMaximumHeight(350)

        info_text = """<b>TXD Workshop Capabilities:</b><br><br>

<b>File Operations:</b><br>
- Open TXD files (standalone or from IMG archives)<br>
- Save TXD files back to IMG or as standalone<br>
- Create new TXD files from scratch<br>
- Multi-TXD management from IMG archives<br><br>

<b>Texture Viewing & Editing:</b><br>
- View all textures with thumbnails<br>
- Preview textures with zoom and pan controls<br>
- Flip textures (horizontal/vertical)<br>
- Rotate textures (90°, 180°, 270°)<br>
- Resize textures with interpolation<br>
- Rename textures and alpha channels<br>
- View texture properties (size, format, compression)<br><br>

<b>Texture Management:</b><br>
- Import textures (PNG, JPG, BMP, TGA, DDS)<br>
- Import 8-bit indexed formats (PCX, GIF, IFF/Amiga)<br>
- Export single or multiple textures<br>
- Duplicate textures<br>
- Delete textures<br>
- Undo/Redo operations<br><br>

<b>Format Support:</b><br>
- DXT1/DXT3/DXT5 compression<br>
- Uncompressed ARGB8888, RGB888<br>
- 16-bit and 32-bit formats<br>
- Palette-based textures<br>
- Platform-specific formats (PC, Xbox, PS2)<br><br>

<b>Advanced Features:</b><br>
- Mipmap generation and editing<br>
- Bumpmap support (generate from height/normal maps)<br>
- Alpha channel extraction and editing<br>
- Batch export operations<br>
- Texture filtering and search<br>
- External editor integration<br>
- AI upscaling support (if configured)<br><br>

<b>Platform Detection:</b><br>
- Automatic RenderWare version detection<br>
- Platform identification (PC, Xbox, PS2, Android)<br>
- Game detection (GTA III, VC, SA, Manhunt)<br>
- Format capability validation<br><br>

<b>Import Format Support:</b><br>"""

        # Add format support dynamically
        formats_available = []

        # Standard formats (always via PIL)
        formats_available.append("- PNG, JPG, JPEG (all variants)")
        formats_available.append("- BMP (8/16/24/32-bit)")
        formats_available.append("- TGA/Targa (all variants)")
        formats_available.append("- DDS (DirectDraw Surface)")

        # Check indexed format support
        try:
            if self.iff_import_enabled:
                formats_available.append("- IFF/ILBM (Amiga 8-bit)")
        except:
            pass

        # Always available via indexed_color_import
        formats_available.append("- PCX (ZSoft Paintbrush)")
        formats_available.append("- GIF (with transparency)")
        formats_available.append("- PNG (8-bit indexed mode)")

        info_text += "<br>".join(formats_available)
        info_text += "<br><br>"

        # Settings info
        info_text += """<b>Customization:</b><br>
- Configurable dimension limiting<br>
- Adjustable texture name length (8-64 chars)<br>
- Splash screen dimension support<br>
- Button display modes (Icons/Text/Both)<br>
- Font customization<br>
- Preview zoom and pan offsets<br><br>

<b>Keyboard Shortcuts:</b><br>
- Ctrl+O: Open TXD<br>
- Ctrl+S: Save TXD<br>
- Ctrl+I: Import Texture<br>
- Ctrl+E: Export Selected<br>
- Ctrl+Z: Undo<br>
- Delete: Remove Texture<br>
- Ctrl+D: Duplicate Texture<br>"""

        capabilities.setHtml(info_text)
        layout.addWidget(capabilities)

        # Close button
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(dialog.accept)
        close_btn.setDefault(True)
        layout.addWidget(close_btn)

        dialog.exec()

    def _on_txd_selected(self, item): #vers 2
        """Handle TXD file selection"""
        try:
            entry = item.data(Qt.ItemDataRole.UserRole)
            if entry:
                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"Loading TXD: {entry.name} offset={hex(entry.offset)} size={entry.size}")
                txd_data = self._extract_txd_from_img(entry)
                if txd_data:
                    # Validate TXD header before loading
                    import struct
                    if len(txd_data) >= 4:
                        header_type = struct.unpack('<I', txd_data[:4])[0]
                        if self.main_window and hasattr(self.main_window, 'log_message'):
                            self.main_window.log_message(f"TXD header type: 0x{header_type:08X}")
                    self.current_txd_data = txd_data
                    self.current_txd_name = entry.name
                    self._load_txd_textures(txd_data, entry.name)
        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Error selecting TXD: {str(e)}")

    def _extract_txd_from_img(self, entry): #vers 2
        """Extract TXD data from IMG entry"""
        try:
            if not self.current_img:
                return None
            return self.current_img.read_entry_data(entry)
        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Extract error: {str(e)}")
            return None

    def _load_txd_textures(self, txd_data, txd_name): #vers 15
        """Load textures from TXD data with detailed structural parsing, log output, and granular control"""
        try:
            from PyQt6.QtWidgets import (QProgressDialog, QMessageBox, QDialog,
                                        QVBoxLayout, QHBoxLayout, QTextEdit, QPushButton, QLabel)
            from PyQt6.QtCore import Qt
            import struct

            # Create custom progress dialog with log output
            dialog = QDialog(self)
            dialog.setWindowTitle("TXD Structural Parser")
            dialog.setMinimumWidth(800)
            dialog.setMinimumHeight(600)
            dialog.setModal(True)

            layout = QVBoxLayout(dialog)

            # Header
            header = QLabel(f"Loading: {txd_name}")
            header.setStyleSheet("font-size: 14px; font-weight: bold; padding: 5px;")
            layout.addWidget(header)

            # Progress bar
            from PyQt6.QtWidgets import QProgressBar
            progress_bar = QProgressBar()
            progress_bar.setRange(0, 100)
            progress_bar.setValue(0)
            layout.addWidget(progress_bar)

            # Log output
            log_output = QTextEdit()
            log_output.setReadOnly(True)
            log_output.setStyleSheet("font-family: 'Courier New', monospace; font-size: 10px;")
            layout.addWidget(log_output)

            # Button layout
            button_layout = QHBoxLayout()

            cancel_btn = QPushButton("Cancel Loading")
            cancel_btn.setStyleSheet("background-color: palette(highlight); color: palette(highlightedText);")
            button_layout.addWidget(cancel_btn)

            button_layout.addStretch()
            layout.addLayout(button_layout)

            # Show dialog
            dialog.show()
            dialog.raise_()
            dialog.activateWindow()

            # Tracking
            alpha_errors = []
            skip_alpha_textures = False
            ignore_all_errors = False
            skip_all_entries = False
            user_cancelled = False

            def log(message):  #vers 1
                """Add message to log output"""
                log_output.append(message)
                log_output.verticalScrollBar().setValue(log_output.verticalScrollBar().maximum())
                dialog.repaint()

            def update_progress(value, message=None):  #vers 1
                """Update progress bar and optionally log"""
                progress_bar.setValue(value)
                if message:
                    log(message)
                if user_cancelled:
                    raise Exception("Loading cancelled by user")

            # Connect cancel button
            def handle_cancel():  #vers 1
                nonlocal user_cancelled
                user_cancelled = True

            cancel_btn.clicked.connect(handle_cancel)

            # Reset state
            update_progress(1, "=" * 80)
            update_progress(1, "TXD STRUCTURAL PARSER - INITIALIZING")
            update_progress(1, "=" * 80)

            self.texture_table.setRowCount(0)
            self.texture_list = []
            textures = []

            # Detect TXD info
            log("")
            log("PHASE 1: FORMAT DETECTION")
            log("-" * 80)
            if self.txd_version_id == 0:
                self._detect_txd_info(txd_data)

            self.current_txd_data = txd_data
            self.current_txd_name = txd_name

            update_progress(5)
            log(f"File Name      : {txd_name}")
            log(f"File Size      : {len(txd_data):,} bytes ({len(txd_data)/1024:.2f} KB)")
            log(f"RW Version     : 0x{self.txd_version_id:08X}")
            log(f"Device ID      : 0x{self.txd_device_id:08X}")

            # === PARSE TXD HEADER STRUCTURE ===
            log("")
            log("PHASE 2: TXD HEADER STRUCTURE")
            log("-" * 80)
            update_progress(10)

            if len(txd_data) < 12:
                raise Exception("File too small - missing TXD header")

            # Read main TXD dictionary header
            main_type, main_size, main_version = struct.unpack('<III', txd_data[0:12])
            log(f"Main TXD Dictionary Section:")
            log(f"  Offset       : 0")
            log(f"  Type         : 0x{main_type:02X} (Texture Dictionary)")
            log(f"  Size         : {main_size:,} bytes")
            log(f"  Version      : 0x{main_version:08X}")

            if main_type != 0x16:
                raise Exception(f"Invalid TXD header - expected 0x16, got 0x{main_type:02X}")

            # === PARSE STRUCT SECTION (Texture Count) ===
            log("")
            log("PHASE 3: STRUCT SECTION (Texture Count)")
            log("-" * 80)
            update_progress(15)

            offset = 12
            texture_count = 0

            if offset + 12 < len(txd_data):
                struct_type, struct_size, struct_version = struct.unpack('<III', txd_data[offset:offset+12])
                log(f"Struct Section:")
                log(f"  Offset       : {offset}")
                log(f"  Type         : 0x{struct_type:02X} (Struct)")
                log(f"  Size         : {struct_size} bytes")
                log(f"  Version      : 0x{struct_version:08X}")
                offset += 12

                if struct_type != 0x01:
                    raise Exception(f"Invalid struct section - expected 0x01, got 0x{struct_type:02X}")

                if struct_size >= 4:
                    # SA (RW >= 0x1803FFFF): uint16 tex_count + uint16 device_id
                    # GTA3/VC (RW < 0x1803FFFF): uint32 tex_count
                    if self.txd_version_id >= 0x1803FFFF:
                        texture_count, device_id = struct.unpack('<HH', txd_data[offset:offset+4])
                        log(f"  Texture Count: {texture_count} (SA format, device_id={device_id})")
                    else:
                        texture_count = struct.unpack('<I', txd_data[offset:offset+4])[0]
                        log(f"  Texture Count: {texture_count}")
                    offset += struct_size
                else:
                    raise Exception(f"Struct section too small: {struct_size} bytes")

            # Validate texture count
            update_progress(20)
            log("")
            log(f"Validation: {texture_count} textures declared")

            if texture_count <= 0:
                log("  Warning: TXD contains 0 textures (stub/placeholder file)")
                raise Exception("__STUB_TXD__")

            if texture_count > 4096:
                raise Exception(f"Invalid texture count: {texture_count} (likely corrupt header)")

            # === PARSE TEXTURE NATIVE SECTIONS ===
            log("")
            log(f"PHASE 4: PARSING {texture_count} TEXTURE NATIVE SECTIONS")
            log("=" * 80)
            update_progress(25)

            for i in range(texture_count):
                # Check bounds
                if offset + 12 > len(txd_data):
                    log("")
                    log(f"[TEXTURE {i+1}] ERROR: Premature end of data at offset {offset:,}")
                    log(f"            Remaining textures cannot be read")
                    break

                try:
                    # Progress calculation
                    texture_progress = 25 + int((i / texture_count) * 60)

                    log("")
                    log(f"[TEXTURE {i+1}/{texture_count}]")
                    log("-" * 80)

                    # Read Texture Native section header
                    log(f"Reading Texture Native header at offset {offset:,}...")

                    tex_type, tex_size, tex_version = struct.unpack('<III', txd_data[offset:offset+12])

                    log(f"  Section Type : 0x{tex_type:02X}")
                    log(f"  Section Size : {tex_size:,} bytes")
                    log(f"  Version      : 0x{tex_version:08X}")

                    update_progress(texture_progress)

                    # Verify texture native type
                    if tex_type != 0x15:
                        log(f"  ERROR        : Expected Texture Native (0x15), got 0x{tex_type:02X}")
                        log(f"  Action       : Skipping to next section")
                        offset += 12 + tex_size
                        continue

                    # Parse texture structure (88-byte header + data)
                    log(f"  Status       : Parsing 88-byte texture structure...")

                    tex = self._parse_single_texture(txd_data, offset, i, rw_version=self.txd_version_id)

                    if tex:
                        tex_name = tex.get('name', f'texture_{i}')
                        tex_width = tex.get('width', 0)
                        tex_height = tex.get('height', 0)
                        tex_format = tex.get('format', 'Unknown')
                        has_alpha = tex.get('has_alpha', False)
                        alpha_name = tex.get('alpha_name', '')

                        log(f"  Name         : {tex_name}")
                        log(f"  Dimensions   : {tex_width}x{tex_height}")
                        log(f"  Format       : {tex_format}")
                        log(f"  Depth        : {tex.get('depth', 32)}-bit")
                        log(f"  Alpha        : {has_alpha}")
                        if has_alpha:
                            log(f"  Alpha Name   : {alpha_name}")

                        # === ALPHA CHANNEL VALIDATION ===
                        if has_alpha and not skip_alpha_textures and not skip_all_entries:
                            log(f"  Validating alpha channel...")

                            rgba_data = tex.get('rgba_data', b'')
                            if rgba_data and len(rgba_data) >= 4:
                                has_transparency = False
                                all_opaque = True
                                identical_to_rgb = True

                                sample_size = min(1000, len(rgba_data) // 4)

                                for pixel_idx in range(sample_size):
                                    byte_offset = pixel_idx * 4
                                    if byte_offset + 3 < len(rgba_data):
                                        r = rgba_data[byte_offset]
                                        g = rgba_data[byte_offset + 1]
                                        b = rgba_data[byte_offset + 2]
                                        a = rgba_data[byte_offset + 3]

                                        if a < 255:
                                            all_opaque = False
                                            has_transparency = True

                                        if a != r and a != g and a != b:
                                            identical_to_rgb = False

                                # Detect problematic alpha
                                alpha_error = None
                                if all_opaque:
                                    alpha_error = f"Alpha channel is all opaque (255) - no transparency"
                                    log(f"  ALPHA ERROR  : {alpha_error}")
                                elif identical_to_rgb:
                                    alpha_error = f"Alpha channel identical to RGB data - corrupted"
                                    log(f"  ALPHA ERROR  : {alpha_error}")

                                # Handle alpha errors
                                if alpha_error and not ignore_all_errors:
                                    alpha_errors.append((i+1, tex_name, alpha_error))

                                    # Show error dialog with options
                                    error_dialog = QDialog(dialog)
                                    error_dialog.setWindowTitle("Alpha Channel Error")
                                    error_dialog.setModal(True)
                                    error_dialog.setMinimumWidth(500)

                                    error_layout = QVBoxLayout(error_dialog)

                                    error_label = QLabel(
                                        f"Corrupted alpha channel detected:\n\n"
                                        f"Texture {i+1}: {tex_name}\n"
                                        f"Error: {alpha_error}\n\n"
                                        f"How would you like to proceed?"
                                    )
                                    error_label.setWordWrap(True)
                                    error_layout.addWidget(error_label)

                                    btn_layout = QHBoxLayout()

                                    ignore_entry_btn = QPushButton("Ignore Entry")
                                    ignore_entry_btn.setToolTip("Load this texture with alpha as-is")

                                    ignore_all_btn = QPushButton("Ignore All")
                                    ignore_all_btn.setToolTip("Ignore all alpha errors and continue")

                                    skip_alpha_btn = QPushButton("Strip Alpha")
                                    skip_alpha_btn.setToolTip("Remove alpha from this texture only")

                                    skip_all_btn = QPushButton("Strip All Alpha")
                                    skip_all_btn.setToolTip("Remove alpha from all remaining textures")
                                    skip_all_btn.setStyleSheet("background-color: palette(button); color: palette(buttonText);")

                                    cancel_load_btn = QPushButton("Cancel Loading")
                                    cancel_load_btn.setStyleSheet("background-color: palette(highlight); color: palette(highlightedText);")

                                    btn_layout.addWidget(ignore_entry_btn)
                                    btn_layout.addWidget(ignore_all_btn)
                                    btn_layout.addWidget(skip_alpha_btn)
                                    btn_layout.addWidget(skip_all_btn)
                                    btn_layout.addWidget(cancel_load_btn)

                                    error_layout.addLayout(btn_layout)

                                    user_choice = [None]

                                    def set_choice(choice):  #vers 1
                                        user_choice[0] = choice
                                        error_dialog.accept()

                                    ignore_entry_btn.clicked.connect(lambda: set_choice('ignore_entry'))
                                    ignore_all_btn.clicked.connect(lambda: set_choice('ignore_all'))
                                    skip_alpha_btn.clicked.connect(lambda: set_choice('skip_alpha'))
                                    skip_all_btn.clicked.connect(lambda: set_choice('skip_all'))
                                    cancel_load_btn.clicked.connect(lambda: set_choice('cancel'))

                                    error_dialog.exec()

                                    choice = user_choice[0]

                                    if choice == 'cancel':
                                        log(f"  User Action  : CANCELLED LOADING")
                                        raise Exception("Loading cancelled due to alpha errors")
                                    elif choice == 'ignore_entry':
                                        log(f"  User Action  : Ignored this entry, loading with alpha")
                                    elif choice == 'ignore_all':
                                        ignore_all_errors = True
                                        log(f"  User Action  : Ignoring all future alpha errors")
                                    elif choice == 'skip_alpha':
                                        tex['has_alpha'] = False
                                        if 'alpha_name' in tex:
                                            del tex['alpha_name']
                                        log(f"  User Action  : Stripped alpha from this texture")
                                    elif choice == 'skip_all':
                                        skip_alpha_textures = True
                                        tex['has_alpha'] = False
                                        if 'alpha_name' in tex:
                                            del tex['alpha_name']
                                        log(f"  User Action  : Stripping alpha from all remaining textures")

                            else:
                                log(f"  Alpha Valid  : Channel validated successfully")

                        elif skip_alpha_textures and has_alpha:
                            tex['has_alpha'] = False
                            if 'alpha_name' in tex:
                                del tex['alpha_name']
                            log(f"  Alpha Action : Stripped (skip all active)")

                        textures.append(tex)
                        log(f"  Result       : SUCCESS - Added to texture list")
                    else:
                        log(f"  Result       : FAILED - Parse returned no data")

                    # Advance offset by section size
                    offset += 12 + tex_size

                except struct.error as e:
                    log(f"[TEXTURE {i+1}] STRUCT ERROR at offset {offset:,}: {str(e)}")
                    break
                except Exception as e:
                    log(f"[TEXTURE {i+1}] ERROR: {str(e)}")
                    offset += 1000
                    continue

            # === POPULATE TABLE ===
            log("")
            log("=" * 80)
            log(f"PHASE 5: POPULATING TABLE WITH {len(textures)} TEXTURES")
            log("=" * 80)
            update_progress(85)

            if not textures:
                raise Exception("No valid textures loaded from TXD")

            for idx, tex in enumerate(textures):
                table_progress = 85 + int((idx / len(textures)) * 14)
                tex_name = tex.get('name', f'texture_{idx}')

                log(f"Adding to table: {tex_name} ({idx+1}/{len(textures)})")
                update_progress(table_progress)

                from apps.methods.txd_splice import tag_loaded_texture
                tag_loaded_texture(tex)
                self.texture_list.append(tex)
                self._add_texture_to_table(tex)

            for row in range(self.texture_table.rowCount()):
                self.texture_table.setRowHeight(row, 100)
            self.texture_table.setColumnWidth(0, 80)

            # === COMPLETE ===
            log("")
            log("=" * 80)
            log("LOADING COMPLETE")
            log("=" * 80)
            log(f"Total Textures Loaded: {len(textures)}")

            if alpha_errors:
                log(f"Alpha Warnings: {len(alpha_errors)}")
                for tex_num, tex_name, error in alpha_errors:
                    log(f"  - Texture {tex_num} ({tex_name}): {error}")

            if skip_alpha_textures:
                log("Alpha channels were stripped from textures")

            update_progress(100)

            # Change button to close
            cancel_btn.setText("Close")
            cancel_btn.setStyleSheet("background-color: palette(button); color: palette(buttonText);")
            try: cancel_btn.clicked.disconnect()
            except Exception: pass
            cancel_btn.clicked.connect(dialog.accept)

            dialog.exec()

            # Update window title and parent tab
            import re as _re
            clean_name = _re.sub(r'_[a-z0-9]{6,12}(?=\.txd$|$)', '', txd_name, flags=_re.IGNORECASE)
            self.setWindowTitle(f"TXD Workshop: {clean_name} ({len(textures)} textures)")
            if self.main_window and hasattr(self.main_window, 'main_tab_widget'):
                tw = self.main_window.main_tab_widget
                for i in range(tw.count()):
                    if tw.widget(i) and self in tw.widget(i).findChildren(type(self)):
                        tw.setTabText(i, clean_name)
                        break

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Loaded {len(textures)} textures from {txd_name}")

        except Exception as e:
            if 'dialog' in locals():
                dialog.close()

            if str(e) == "__STUB_TXD__":
                QMessageBox.warning(self, "Stub TXD",
                    "This is a stub/placeholder .txd with no textures.\n\nPlease pick another file.")
                return

            if "cancelled" not in str(e).lower():
                QMessageBox.critical(self, "Load Error", f"Failed to load TXD:\n\n{str(e)}")

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"TXD load error: {str(e)}")

    def _upscale_texture(self): #vers 3
        """AI upscale selected texture with size management"""
        from PyQt6.QtWidgets import QInputDialog

        if not self.selected_texture:
            QMessageBox.warning(self, "No Selection", "Please select a texture first")
            return

        # Get scale factor
        factor, ok = QInputDialog.getInt(self, "AI Upscale", "Scale factor:", value=2, min=2, max=8)
        if not ok:
            return

        current_width = self.selected_texture.get('width', 256)
        current_height = self.selected_texture.get('height', 256)

        new_width = current_width * factor
        new_height = current_height * factor

        # Calculate memory and file size impact
        old_size_mb = (current_width * current_height * 4) / (1024 * 1024)
        new_size_mb = (new_width * new_height * 4) / (1024 * 1024)

        if new_size_mb > 16:  # Warn for textures over 16MB uncompressed
            reply = QMessageBox.question(self, "Large Upscale",
                                    f"Upscaling {factor}x will create a {new_width}x{new_height} texture "
                                    f"(~{new_size_mb:.1f}MB uncompressed). "
                                    f"This will significantly increase TXD size. Continue?",
                                    QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
            if reply != QMessageBox.StandardButton.Yes:
                return

        self._save_undo_state("AI upscale")
        # Perform the upscale
        if self._perform_ai_upscale(factor):
            self.selected_texture['width'] = new_width
            self.selected_texture['height'] = new_height

            self._update_texture_info(self.selected_texture)
            self._update_table_display()
            self._mark_as_modified()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"AI upscaled texture {factor}x to {new_width}x{new_height}")
        else:
            QMessageBox.critical(self, "Error", "AI upscale failed")

    def _perform_ai_upscale(self, factor): #vers 1
        """Perform AI upscaling on texture data"""
        try:
            if not self.selected_texture.get('rgba_data'):
                return False

            # For now, use basic upscaling (could be with actual AI upscaling libraries)
            return self._resize_texture_data(
                self.selected_texture['width'] * factor,
                self.selected_texture['height'] * factor
            )

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"AI upscale error: {str(e)}")
            return False

    def export_selected_texture(self): #vers 2
        """Export selected texture with channel options"""
        if not self.selected_texture:
            QMessageBox.warning(self, "No Selection", "Please select a texture first")
            return

        try:
            name = self.selected_texture.get('name', 'texture')

            # Ask user what to export
            dialog = QMessageBox(self)
            dialog.setWindowTitle("Export Options")
            dialog.setText(f"Export {name} as:")

            normal_btn = dialog.addButton("Normal (RGBA)", QMessageBox.ButtonRole.AcceptRole)
            alpha_btn = dialog.addButton("Alpha Channel Only", QMessageBox.ButtonRole.AcceptRole)
            both_btn = dialog.addButton("Both Separately", QMessageBox.ButtonRole.AcceptRole)
            cancel_btn = dialog.addButton("Cancel", QMessageBox.ButtonRole.RejectRole)

            dialog.exec()
            clicked = dialog.clickedButton()

            if clicked == cancel_btn:
                return

            # Get save location
            default_name = f"{name}.png"
            file_path, _ = QFileDialog.getSaveFileName(self, "Export Texture", default_name,
                                                    "PNG Files (*.png);;All Files (*)")

            if not file_path:
                return

            rgba_data = self.selected_texture.get('rgba_data')
            width = self.selected_texture.get('width', 0)
            height = self.selected_texture.get('height', 0)

            if not rgba_data or width <= 0:
                QMessageBox.critical(self, "Error", "Cannot export this texture")
                return

            if clicked == normal_btn:
                self._save_texture_png(rgba_data, width, height, file_path)
                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"Exported: {file_path}")

            elif clicked == alpha_btn:
                alpha_data = self._extract_alpha_channel(rgba_data)
                self._save_texture_png(alpha_data, width, height, file_path)
                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"Exported alpha: {file_path}")

            elif clicked == both_btn:
                # Save normal
                self._save_texture_png(rgba_data, width, height, file_path)
                # Save alpha
                alpha_path = file_path.replace('.png', '_alpha.png')
                alpha_data = self._extract_alpha_channel(rgba_data)
                self._save_texture_png(alpha_data, width, height, alpha_path)
                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"Exported both: {file_path} and {alpha_path}")

            QMessageBox.information(self, "Success", "Texture exported successfully!")

        except Exception as e:
            QMessageBox.critical(self, "Error", f"Export failed: {str(e)}")

    @staticmethod
    def _rgba_to_iff_ilbm(rgba: bytes, w: int, h: int) -> bytes: #vers 1
        """Write RGBA pixel data as 24-bit IFF ILBM (Amiga true colour).
        Compatible with PPaint, DPaint 5, and any IFF-aware tool."""
        import struct

        def _iff_chunk(tag: str, data: bytes) -> bytes:  #vers 1
            hdr = tag.encode('ascii') + struct.pack('>I', len(data))
            return hdr + data + (b'\x00' if len(data) % 2 else b'')

        n_planes = 24   # 8R + 8G + 8B bitplanes
        row_bytes = (w + 15) // 16 * 2   # row width in bytes, word-aligned

        # BMHD — bitmap header
        bmhd = struct.pack('>HHhhBBBBHBBhh',
            w, h,          # width, height
            0, 0,          # x, y origin
            n_planes,      # planes
            0,             # masking (none)
            0,             # compression (none — uncompressed)
            0,             # pad
            0,             # transparent colour
            1, 1,          # x/y aspect
            w, h)          # page width/height

        # CAMG — Amiga viewport mode (0 = normal, no HAM/EHB)
        camg = struct.pack('>I', 0x0000)

        # BODY — interleaved bitplanes: for each scan row, 24 separate plane rows
        # 8 planes for Red, 8 for Green, 8 for Blue (plane 0 = LSB)
        body = bytearray()
        for y in range(h):
            # One row_bytes buffer per bitplane
            planes_r = [bytearray(row_bytes) for _ in range(8)]
            planes_g = [bytearray(row_bytes) for _ in range(8)]
            planes_b = [bytearray(row_bytes) for _ in range(8)]
            for x in range(w):
                idx = (y * w + x) * 4
                r, g, b = rgba[idx], rgba[idx+1], rgba[idx+2]
                bx  = x // 8
                bit = 0x80 >> (x % 8)
                for p in range(8):
                    if r & (1 << p): planes_r[p][bx] |= bit
                    if g & (1 << p): planes_g[p][bx] |= bit
                    if b & (1 << p): planes_b[p][bx] |= bit
            # Write planes in order: R0..R7, G0..G7, B0..B7
            for p in range(8): body += bytes(planes_r[p])
            for p in range(8): body += bytes(planes_g[p])
            for p in range(8): body += bytes(planes_b[p])

        ilbm = (b'ILBM'
                + _iff_chunk('BMHD', bmhd)
                + _iff_chunk('CAMG', camg)
                + _iff_chunk('BODY', bytes(body)))
        return b'FORM' + struct.pack('>I', len(ilbm)) + ilbm

    def _save_texture_format(self, rgba: bytes, w: int, h: int,
                              path: str, fmt: str): #vers 1
        """Save RGBA pixel data to path in the requested format.
        fmt: 'PNG' | 'IFF' | 'TGA' | 'DDS' | 'BMP'"""
        fmt = fmt.upper()
        if fmt == 'IFF':
            data = self._rgba_to_iff_ilbm(rgba, w, h)
            with open(path, 'wb') as f:
                f.write(data)
        elif fmt == 'DDS':
            # DDS: minimal DXT1 header — write as uncompressed RGBA8888
            import struct
            DDSD_CAPS=1; DDSD_HEIGHT=2; DDSD_WIDTH=4
            DDSD_PIXELFORMAT=0x1000; DDSD_LINEARSIZE=0x80000
            DDPF_ALPHAPIXELS=1; DDPF_RGB=0x40
            flags = DDSD_CAPS|DDSD_HEIGHT|DDSD_WIDTH|DDSD_PIXELFORMAT|DDSD_LINEARSIZE
            pitch = w * 4
            pf = struct.pack('<II4sIIIIII',
                32, DDPF_ALPHAPIXELS|DDPF_RGB, b'    ',
                32, 0x00FF0000, 0x0000FF00, 0x000000FF, 0xFF000000, 0)
            caps = struct.pack('<IIII', 0x1000, 0, 0, 0)  # DDSCAPS_TEXTURE
            hdr = (b'DDS ' + struct.pack('<I', 124) +
                   struct.pack('<IIIII', flags, h, w, pitch, 1) +
                   b'\x00'*44 + pf + caps)
            # Convert RGBA->BGRA for DDS
            bgra = bytearray(len(rgba))
            for i in range(0, len(rgba), 4):
                bgra[i]=rgba[i+2]; bgra[i+1]=rgba[i+1]
                bgra[i+2]=rgba[i]; bgra[i+3]=rgba[i+3]
            with open(path,'wb') as f:
                f.write(hdr); f.write(bytes(bgra))
        elif fmt == 'TGA':
            import struct
            # TGA: uncompressed BGRA
            hdr = struct.pack('<BBBHHBHHHHBB',
                0, 0, 2, 0, 0, 0, 0, 0, w, h, 32, 8)
            bgra = bytearray(len(rgba))
            for i in range(0, len(rgba), 4):
                bgra[i]=rgba[i+2]; bgra[i+1]=rgba[i+1]
                bgra[i+2]=rgba[i]; bgra[i+3]=rgba[i+3]
            with open(path,'wb') as f:
                f.write(hdr); f.write(bytes(bgra))
        else:
            # PNG / BMP — use QImage
            from PyQt6.QtGui import QImage
            img = QImage(rgba, w, h, w*4, QImage.Format.Format_RGBA8888)
            ext = 'BMP' if fmt == 'BMP' else 'PNG'
            img.save(path, ext)

    def export_all_textures(self): #vers 3
        """Export all textures from the current TXD in chosen format(s)."""
        if not self.texture_list:
            QMessageBox.warning(self, "No Textures", "No textures loaded to export.")
            return

        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout,
            QLabel, QCheckBox, QDialogButtonBox, QGroupBox, QFileDialog)

        dlg = QDialog(self)
        dlg.setWindowTitle("Export All Textures")
        dlg.setMinimumWidth(340)
        lay = QVBoxLayout(dlg)

        lay.addWidget(QLabel(
            f"Export {len(self.texture_list)} textures from current TXD:"))

        fmt_box = QGroupBox("Output format(s)")
        fmt_lay = QVBoxLayout(fmt_box)
        fmt_checks = {}
        for fmt, default in [('IFF / ILBM (Amiga)', True),
                              ('PNG',                True),
                              ('TGA',                False),
                              ('DDS',                False),
                              ('BMP',                False)]:
            cb = QCheckBox(fmt)
            cb.setChecked(default)
            fmt_lay.addWidget(cb)
            fmt_checks[fmt] = cb
        lay.addWidget(fmt_box)

        btns = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok |
            QDialogButtonBox.StandardButton.Cancel)
        btns.accepted.connect(dlg.accept)
        btns.rejected.connect(dlg.reject)
        lay.addWidget(btns)

        if dlg.exec() != QDialog.DialogCode.Accepted:
            return

        # Build list of selected (key->ext) pairs
        fmt_map = {'IFF / ILBM (Amiga)': 'IFF', 'PNG': 'PNG',
                   'TGA': 'TGA', 'DDS': 'DDS', 'BMP': 'BMP'}
        ext_map  = {'IFF':'iff','PNG':'png','TGA':'tga','DDS':'dds','BMP':'bmp'}
        selected = [fmt_map[k] for k, cb in fmt_checks.items() if cb.isChecked()]
        if not selected:
            QMessageBox.warning(self, "No Format", "Select at least one format.")
            return

        output_dir = QFileDialog.getExistingDirectory(
            self, "Select Export Folder")
        if not output_dir:
            return

        from PyQt6.QtWidgets import QProgressDialog, QApplication
        from PyQt6.QtCore import Qt as _Qt
        prog = QProgressDialog(
            f"Exporting {len(self.texture_list)} textures…",
            "Cancel", 0, len(self.texture_list), self)
        prog.setWindowModality(_Qt.WindowModality.ApplicationModal)
        prog.show()

        exported = skipped = 0
        errors = []

        for i, texture in enumerate(self.texture_list):
            prog.setValue(i)
            QApplication.processEvents()
            if prog.wasCanceled():
                break

            name = (texture.get('name') or f'texture_{i}').strip('\x00').strip()
            if not name:
                name = f'texture_{i}'

            rgba_data = texture.get('rgba_data')
            width  = texture.get('width', 0)
            height = texture.get('height', 0)
            if not rgba_data or width == 0 or height == 0:
                levels = texture.get('mip_levels') or texture.get('mipmap_levels', [])
                if levels:
                    lv = levels[0]; rgba_data = lv.get('rgba_data')
                    width  = lv.get('width', width)
                    height = lv.get('height', height)

            if not rgba_data or width == 0 or height == 0:
                skipped += 1
                continue

            rgba_bytes = bytes(rgba_data)
            for fmt in selected:
                ext  = ext_map[fmt]
                path = os.path.join(output_dir, f"{name}.{ext}")
                try:
                    self._save_texture_format(rgba_bytes, width, height, path, fmt)
                    exported += 1
                except Exception as e:
                    errors.append(f"{name}.{ext}: {e}")

        prog.close()

        msg = (f"Exported {exported} file(s) to:\n{output_dir}"
               f"\nFormats: {', '.join(selected)}")
        if skipped:
            msg += f"\nSkipped {skipped} (no pixel data)"
        if errors:
            msg += f"\nErrors ({len(errors)}):\n" + "\n".join(errors[:5])

        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message(
                f"TXD export: {exported} file(s) -> {output_dir} "
                f"[{', '.join(selected)}]"
                + (f" ({skipped} skipped)" if skipped else ""))

        QMessageBox.information(self, "Export Complete", msg)

    def _extract_alpha_channel(self, rgba_data): #vers 1
        """Extract alpha channel as grayscale RGBA"""
        alpha_data = bytearray()
        for i in range(0, len(rgba_data), 4):
            a = rgba_data[i+3]
            alpha_data.extend([a, a, a, 255])
        return bytes(alpha_data)

    def _save_texture_png(self, rgba_data, width, height, file_path): #vers 1
        """Save RGBA data as PNG"""
        image = QImage(rgba_data, width, height, width*4, QImage.Format.Format_RGBA8888)
        if not image.save(file_path):
            raise Exception("Failed to save PNG")

    def _export_alpha_only(self): #vers 1
        """Export only alpha channel"""
        if not self.selected_texture:
            return

        name = self.selected_texture.get('name', 'texture')
        file_path, _ = QFileDialog.getSaveFileName(self, "Export Alpha Channel", f"{name}_alpha.png",
                                                "PNG Files (*.png)")
        if file_path:
            rgba_data = self.selected_texture.get('rgba_data')
            width = self.selected_texture.get('width', 0)
            height = self.selected_texture.get('height', 0)

            if rgba_data:
                alpha_data = self._extract_alpha_channel(rgba_data)
                self._save_texture_png(alpha_data, width, height, file_path)
                QMessageBox.information(self, "Success", "Alpha channel exported!")

    def _change_format(self, format_name): #vers 3
        """Change texture format - only set has_alpha if alpha data exists"""
        if not self.selected_texture:
            return

        self._save_undo_state("Change format")
        old_format = self.selected_texture.get('format', 'Unknown')
        self.selected_texture['format'] = format_name

        # Check if texture actually has alpha data
        has_actual_alpha = False
        rgba_data = self.selected_texture.get('rgba_data')

        if rgba_data:
            # Check if any alpha values are not 255 (fully opaque)
            width = self.selected_texture.get('width', 0)
            height = self.selected_texture.get('height', 0)

            if width > 0 and height > 0:
                # Sample alpha channel - check every pixel's alpha value
                for i in range(3, len(rgba_data), 4):  # Every 4th byte is alpha
                    if rgba_data[i] < 255:  # Found non-opaque pixel
                        has_actual_alpha = True
                        break

        # Update alpha flag based on format AND actual alpha data
        if format_name in ['DXT3', 'DXT5', 'ARGB8888', 'ARGB1555', 'ARGB4444']:
            # Only set has_alpha if texture actually contains alpha data
            self.selected_texture['has_alpha'] = has_actual_alpha

            # If format supports alpha but texture doesn't have it, warn user
            if not has_actual_alpha:
                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"{format_name} supports alpha, but texture has no alpha data")

        elif format_name in ['DXT1', 'RGB888', 'RGB565']:
            # These formats don't support alpha
            self.selected_texture['has_alpha'] = False

            # Remove alpha_name if switching to non-alpha format
            if 'alpha_name' in self.selected_texture:
                del self.selected_texture['alpha_name']

        self._update_texture_info(self.selected_texture)
        self._update_table_display()
        self._mark_as_modified()

        if self.main_window and hasattr(self.main_window, 'log_message'):
            alpha_status = "with alpha" if self.selected_texture['has_alpha'] else "no alpha"
            self.main_window.log_message(f"Format changed: {old_format} -> {format_name} ({alpha_status})")

    def _compress_texture(self): #vers 3
        """Compress selected texture to DXT format"""
        if not self.selected_texture:
            QMessageBox.warning(self, "No Selection", "Please select a texture first")
            return

        current_format = self.selected_texture.get('format', 'ARGB8888')

        if 'DXT' in current_format:
            # Already compressed - offer to change DXT version
            from PyQt6.QtWidgets import QInputDialog

            dxt_formats = ["DXT1", "DXT3", "DXT5"]
            new_format, ok = QInputDialog.getItem(
                self,
                "Change DXT Format",
                f"Current format: {current_format}\n\nSelect target DXT format:",
                dxt_formats,
                0,
                False
            )

            if ok and new_format != current_format:
                self._save_undo_state("Change DXT format")
                self.selected_texture['format'] = new_format

                # Update has_alpha based on format
                if new_format == 'DXT1':
                    # DXT1 can have 1-bit alpha, keep existing alpha state
                    pass
                elif new_format in ['DXT3', 'DXT5']:
                    # DXT3/DXT5 have alpha
                    if not self.selected_texture.get('has_alpha'):
                        self.selected_texture['has_alpha'] = True
                        if 'alpha_name' not in self.selected_texture:
                            self.selected_texture['alpha_name'] = self.selected_texture['name'] + 'a'

                # Update dropdown
                if hasattr(self, 'format_combo'):
                    index = self.format_combo.findText(new_format)
                    if index >= 0:
                        self.format_combo.setCurrentIndex(index)

                self._update_texture_info(self.selected_texture)
                self._update_table_display()
                self._mark_as_modified()

                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"Changed format: {current_format} -> {new_format}")

            return

        # Not compressed - compress to DXT
        if not self.selected_texture.get('rgba_data'):
            QMessageBox.warning(self, "No Data", "Texture has no image data to compress")
            return

        try:
            # Let user choose DXT format
            from PyQt6.QtWidgets import QInputDialog

            has_alpha = self.selected_texture.get('has_alpha', False)

            if has_alpha:
                dxt_formats = ["DXT3", "DXT5", "DXT1"]
                default_format = "DXT5"
                message = "Texture has alpha channel.\n\nRecommended: DXT5 (best quality)\nAlternative: DXT3 (simpler alpha)\nDXT1: 1-bit alpha only"
            else:
                dxt_formats = ["DXT1", "DXT3", "DXT5"]
                default_format = "DXT1"
                message = "Texture has no alpha channel.\n\nRecommended: DXT1 (smallest size)"

            target_format, ok = QInputDialog.getItem(
                self,
                "Compress Texture",
                f"Current format: {current_format}\n\n{message}\n\nSelect DXT format:",
                dxt_formats,
                dxt_formats.index(default_format),
                False
            )

            if not ok:
                return

            # Save undo state
            self._save_undo_state("Compress texture")
            self.selected_texture['format'] = target_format

            # Update has_alpha if compressing to DXT3/DXT5
            if target_format in ['DXT3', 'DXT5'] and not has_alpha:
                self.selected_texture['has_alpha'] = True
                self.selected_texture['alpha_name'] = self.selected_texture['name'] + 'a'

            # Update dropdown
            if hasattr(self, 'format_combo'):
                index = self.format_combo.findText(target_format)
                if index >= 0:
                    self.format_combo.setCurrentIndex(index)

            self._update_texture_info(self.selected_texture)
            self._update_table_display()
            self._mark_as_modified()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Compressed: {current_format} -> {target_format}")

        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to compress: {str(e)}")

    def _uncompress_texture(self): #vers 3
        """Uncompress selected texture from DXT to ARGB8888"""
        if not self.selected_texture:
            QMessageBox.warning(self, "No Selection", "Please select a texture first")
            return

        current_format = self.selected_texture.get('format', 'ARGB8888')

        if 'DXT' not in current_format:
            QMessageBox.information(self, "Not Compressed", "Texture is not in DXT format")
            return

        try:
            reply = QMessageBox.question(
                self,
                "Uncompress Texture",
                f"Uncompress to ARGB8888?\n\n"
                f"Current: {current_format}\n"
                f"Target: ARGB8888\n\n"
                f"This will convert the texture to uncompressed format.",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
            )

            if reply != QMessageBox.StandardButton.Yes:
                return

            if self.selected_texture.get('rgba_data'):
                self._save_undo_state("Uncompress texture")
                self.selected_texture['format'] = 'ARGB8888'

                # Update dropdown to show ARGB8888
                if hasattr(self, 'format_combo'):
                    index = self.format_combo.findText('ARGB8888')
                    if index >= 0:
                        self.format_combo.setCurrentIndex(index)

                self._update_texture_info(self.selected_texture)
                self._update_table_display()
                self._mark_as_modified()

                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"Uncompressed: {current_format} -> ARGB8888")
            else:
                QMessageBox.warning(self, "No Data", "Texture has no decompressed data available")

        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to uncompress: {str(e)}")

    def _rebuild_txd_data(self): #vers 4
        """Rebuild TXD data with modified texture names and properties"""
        try:
            if not self.current_txd_data:
                return None

            # Preserve original version header
            if len(self.current_txd_data) < 28:
                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Cannot rebuild: insufficient header data")
                return None

            # Read original header to preserve version
            original_header = bytearray(self.current_txd_data[:28])

            # Extract version info if not already detected
            if self.txd_version_id == 0:
                self._detect_txd_info(self.current_txd_data)

            # Check if we have a target version from save_txd_file
            target_version = self.txd_version_id
            target_device = self.txd_device_id

            if hasattr(self, '_save_target_version') and hasattr(self, '_save_target_device'):
                target_version = self._save_target_version
                target_device = self._save_target_device

            # Update header if converting to different version
            if target_version != self.txd_version_id or target_device != self.txd_device_id:
                import struct
                # Update RenderWare version at offset 4
                struct.pack_into('<I', original_header, 4, target_version)

                if self.main_window and hasattr(self.main_window, 'log_message'):
                    from apps.methods.txd_versions import get_version_string
                    self.main_window.log_message(
                        f"Converting to {get_version_string(target_version, target_device)}"
                    )
            else:
                # Log rebuild info with original version
                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(
                        f"Rebuilding TXD with version: {self.txd_version_str}"
                    )

            # Import struct for header manipulation
            import struct

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Rebuilding TXD...")

            # If we have original data, update it in place with new header
            if self.current_txd_data and len(self.current_txd_data) > 100:
                # Splice from the original bytes so edits (rename/replace/
                # delete/add) are kept and untouched textures stay byte-exact
                from apps.methods.txd_splice import rebuild_txd
                from apps.methods.txd_splice import build_d3d8_chunk
                _rw = struct.unpack_from('<I', self.current_txd_data, 8)[0]
                spliced = rebuild_txd(self.current_txd_data, self.texture_list,
                                      lambda t: build_d3d8_chunk(t, _rw, _encode_dxt1)) \
                    if getattr(self, 'texture_list', None) else None
                base = spliced if spliced else self.current_txd_data
                if spliced:
                    original_header = bytearray(spliced[:28])
                    if target_version != self.txd_version_id:
                        struct.pack_into('<I', original_header, 4, target_version)
                rebuilt_data = bytes(original_header) + base[28:]

                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"Rebuilt: {len(rebuilt_data)} bytes")

                return rebuilt_data

            # No original data? Use serializer as fallback
            if self.texture_list:
                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"Using serializer...")

                # Try methods folder first (docked/IMG Factory)
                from apps.methods.txd_serializer import serialize_txd_file
                return serialize_txd_file(self.texture_list, target_version, target_device)


        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Rebuild error: {str(e)}")
            return None

    def _get_format_description(self) -> str: #vers 1
        """Get human-readable format description for UI display"""
        desc_parts = []

        if self.txd_capabilities:
            # Bit depths
            if self.txd_capabilities.get('bit_depths'):
                depths = ', '.join(str(d) for d in self.txd_capabilities['bit_depths'])
                desc_parts.append(f"{depths}-bit")

            # Features
            features = []
            if self.txd_capabilities.get('mipmaps'):
                features.append("Mipmaps")
            if self.txd_capabilities.get('bumpmaps'):
                features.append("Bumpmaps")
            if self.txd_capabilities.get('dxt_compression'):
                features.append("DXT")
            if self.txd_capabilities.get('palette'):
                features.append("Palette")
            if self.txd_capabilities.get('swizzled'):
                features.append("Swizzled")

            if features:
                desc_parts.append(', '.join(features))

        return ' | '.join(desc_parts) if desc_parts else "Standard format"

    def _resize_texture(self): #vers 1
        """Resize selected texture with size validation"""
        if not self.selected_texture:
            QMessageBox.warning(self, "No Selection", "Please select a texture first")
            return

        # Get current dimensions
        current_width = self.selected_texture.get('width', 256)
        current_height = self.selected_texture.get('height', 256)

        # Get new dimensions from user
        w, ok1 = QInputDialog.getInt(self, "Resize Texture", "New width:",value=current_width, min=1, max=4096)
        if not ok1:
            return

        h, ok2 = QInputDialog.getInt(
            self, "Resize Texture", "New height:",
            value=current_height, min=1, max=4096
        )
        if not ok2:
            return

        # Calculate size impact
        old_pixels = current_width * current_height
        new_pixels = w * h
        size_multiplier = new_pixels / old_pixels if old_pixels > 0 else 1

        # Warn for large size increases
        if size_multiplier > 4:
            reply = QMessageBox.question(
                self, "Large Resize",
                f"Resizing to {w}x{h} will increase texture size by {size_multiplier:.1f}x. "
                f"This may require IMG rebuilding. Continue?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
            )
            if reply != QMessageBox.StandardButton.Yes:
                return

        # Update texture dimensions
        self.selected_texture['width'] = w
        self.selected_texture['height'] = h

        # If we have RGBA data, resize it
        if self.selected_texture.get('rgba_data'):
            self._resize_texture_data(w, h)

        # Update display
        self._update_texture_info(self.selected_texture)
        self._update_table_display()

        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message(f"Resized texture to {w}x{h}")

    def _resize_texture_data(self, new_width, new_height): #vers 1
        """Resize the actual texture image data using QImage"""
        try:
            if not self.selected_texture.get('rgba_data'):
                return False

            # Convert current RGBA data to QImage
            rgba_data = self.selected_texture['rgba_data']
            old_width = self.selected_texture['width']
            old_height = self.selected_texture['height']

            qimg = QImage(rgba_data, old_width, old_height, old_width * 4, QImage.Format.Format_RGBA8888)

            # Resize image with high quality
            resized_img = qimg.scaled(new_width, new_height,
                                    Qt.AspectRatioMode.IgnoreAspectRatio,
                                    Qt.TransformationMode.SmoothTransformation)

            # Convert back to RGBA data
            resized_img = resized_img.convertToFormat(QImage.Format.Format_RGBA8888)

            # Get raw bytes from QImage
            ptr = resized_img.bits()
            ptr.setsize(resized_img.sizeInBytes())
            new_rgba_data = bytes(ptr)

            # Update texture data
            self.selected_texture['rgba_data'] = new_rgba_data
            return True

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Resize data error: {str(e)}")
            return False

    def _show_version_selector_dialog(self): #vers 1
        """Show RW version selector dialog for export"""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QLabel,
                                    QComboBox, QPushButton, QGroupBox, QCheckBox)
        from PyQt6.QtCore import Qt

        dialog = QDialog(self)
        dialog.setWindowTitle("Select Export Version")
        dialog.setMinimumWidth(400)

        layout = QVBoxLayout(dialog)

        # Info label
        info_label = QLabel("Select target game and platform for TXD export:")
        layout.addWidget(info_label)

        # Game selection
        game_group = QGroupBox("Target Game")
        game_layout = QVBoxLayout()

        game_combo = QComboBox()
        game_combo.addItems([
            "Auto-detect from current",
            "GTA III",
            "GTA Vice City",
            "GTA San Andreas"
        ])

        # Set current based on detected or settings
        if hasattr(self, 'txd_game') and self.txd_game:
            if "III" in self.txd_game:
                game_combo.setCurrentIndex(1)
            elif "Vice" in self.txd_game or "VC" in self.txd_game:
                game_combo.setCurrentIndex(2)
            elif "San" in self.txd_game or "SA" in self.txd_game:
                game_combo.setCurrentIndex(3)
        elif hasattr(self, 'export_target_game'):
            game_map = {"auto": 0, "gta3": 1, "vc": 2, "sa": 3}
            game_combo.setCurrentIndex(game_map.get(self.export_target_game, 0))

        game_layout.addWidget(game_combo)
        game_group.setLayout(game_layout)
        layout.addWidget(game_group)

        # Platform selection
        platform_group = QGroupBox("Target Platform")
        platform_layout = QVBoxLayout()

        platform_combo = QComboBox()
        platform_combo.addItems([
            "PC",
            "Xbox",
            "PS2"
        ])

        if hasattr(self, 'export_target_platform'):
            platform_map = {"pc": 0, "xbox": 1, "ps2": 2}
            platform_combo.setCurrentIndex(platform_map.get(self.export_target_platform, 0))

        platform_layout.addWidget(platform_combo)
        platform_group.setLayout(platform_layout)
        layout.addWidget(platform_group)

        # Version info display
        version_info_label = QLabel()
        version_info_label.setWordWrap(True)
        version_info_label.setStyleSheet("padding: 10px; background-color: palette(base); border-radius: 4px;")
        layout.addWidget(version_info_label)

        # Capability warnings
        warning_label = QLabel()
        warning_label.setWordWrap(True)
        warning_label.setStyleSheet("color: palette(windowText); padding: 5px;")
        layout.addWidget(warning_label)

        # Update info when selection changes
        def update_version_info():  #vers 1
            game_idx = game_combo.currentIndex()
            platform_idx = platform_combo.currentIndex()

            # Map to version IDs
            version_map = {
                (1, 0): (0x0C02FFFF, 0x01, "3.3.0.2 (GTA III PC)"),      # GTA III PC
                (1, 1): (0x35000, 0x08, "3.5.0.0 (GTA III Xbox)"),       # GTA III Xbox
                (1, 2): (0x00000310, 0x06, "3.1.0.0 (GTA III PS2)"),     # GTA III PS2
                (2, 0): (0x1003FFFF, 0x01, "3.4.0.3 (Vice City PC)"),    # VC PC
                (2, 1): (0x35000, 0x08, "3.5.0.0 (Vice City Xbox)"),     # VC Xbox
                (2, 2): (0x0C02FFFF, 0x06, "3.3.0.2 (Vice City PS2)"),   # VC PS2
                (3, 0): (0x1803FFFF, 0x08, "3.6.0.3 (San Andreas PC)"),  # SA PC
                (3, 1): (0x1803FFFF, 0x08, "3.6.0.3 (San Andreas Xbox)"),# SA Xbox
                (3, 2): (0x1803FFFF, 0x06, "3.6.0.3 (San Andreas PS2)"), # SA PS2
            }

            if game_idx == 0:  # Auto-detect
                version_id = self.txd_version_id if self.txd_version_id else 0x1803FFFF
                device_id = self.txd_device_id if self.txd_device_id else 0x08
                version_str = f"Current: 0x{version_id:08X} (device: 0x{device_id:02X})"
            else:
                version_id, device_id, version_str = version_map.get((game_idx, platform_idx),
                                                                    (0x1803FFFF, 0x08, "3.6.0.3 (SA PC)"))

            version_info_label.setText(f"<b>RW Version:</b> {version_str}")

            # Check capabilities and show warnings
            warnings = []

            # GTA III limitations
            if game_idx == 1:
                warnings.append("GTA III: Mipmaps and bumpmaps not supported - will be removed")
                warnings.append("Limited texture formats supported")

            # VC limitations
            if game_idx == 2 and platform_idx == 2:  # VC PS2
                warnings.append("VC PS2: Mipmaps and bumpmaps not supported - will be removed")

            if warnings:
                warning_label.setText("\n".join(warnings))
                warning_label.setVisible(True)
            else:
                warning_label.setVisible(False)

            # Store selection
            dialog.selected_version = version_id
            dialog.selected_device = device_id
            dialog.selected_game_idx = game_idx

        game_combo.currentIndexChanged.connect(update_version_info)
        platform_combo.currentIndexChanged.connect(update_version_info)
        update_version_info()  # Initial update

        # Remember choice checkbox
        remember_cb = QCheckBox("Remember this choice for future exports")
        layout.addWidget(remember_cb)

        # Buttons
        button_layout = QHBoxLayout()
        ok_btn = QPushButton("Export")
        cancel_btn = QPushButton("Cancel")

        ok_btn.clicked.connect(dialog.accept)
        cancel_btn.clicked.connect(dialog.reject)

        button_layout.addStretch()
        button_layout.addWidget(ok_btn)
        button_layout.addWidget(cancel_btn)
        layout.addLayout(button_layout)

        # Execute dialog
        if dialog.exec() == QDialog.DialogCode.Accepted:
            # Update settings if remember is checked
            if remember_cb.isChecked():
                game_map = {0: "auto", 1: "gta3", 2: "vc", 3: "sa"}
                platform_map = {0: "pc", 1: "xbox", 2: "ps2"}
                self.export_target_game = game_map.get(dialog.selected_game_idx, "auto")
                self.export_target_platform = platform_map.get(platform_combo.currentIndex(), "pc")

            return (dialog.selected_version, dialog.selected_device, dialog.selected_game_idx)

        return None

    def _strip_unsupported_features_for_version(self, game_idx): #vers 1
        """Remove unsupported features based on target game version"""
        if game_idx == 1:  # GTA III
            # Remove mipmaps and bumpmaps from all textures
            removed_mipmaps = 0
            removed_bumpmaps = 0

            for texture in self.texture_list:
                # Remove mipmaps
                if texture.get('mipmap_levels'):
                    removed_mipmaps += len(texture['mipmap_levels'])
                    texture['mipmap_levels'] = []
                    texture['mipmaps'] = 1

                # Remove bumpmaps
                if texture.get('has_bumpmap') or texture.get('bumpmap_data'):
                    removed_bumpmaps += 1
                    texture['has_bumpmap'] = False
                    texture['bumpmap_data'] = b''
                    texture['bumpmap_type'] = 0

                    # Clear bumpmap flag
                    if 'raster_format_flags' in texture:
                        texture['raster_format_flags'] &= ~0x10

                # Remove reflection maps
                if texture.get('has_reflection'):
                    texture['has_reflection'] = False
                    texture['reflection_map'] = b''
                    texture['fresnel_map'] = b''

            if self.main_window and hasattr(self.main_window, 'log_message'):
                if removed_mipmaps > 0:
                    self.main_window.log_message(f"Removed {removed_mipmaps} mipmap levels (GTA III doesn't support mipmaps)")
                if removed_bumpmaps > 0:
                    self.main_window.log_message(f"Removed {removed_bumpmaps} bumpmaps (GTA III doesn't support bumpmaps)")

    def _save_as_txd_file(self): #vers 5
        """Save as standalone TXD file - respects save location setting"""
        import os
        from PyQt6.QtWidgets import QFileDialog, QMessageBox

        # Determine default filename
        if self.current_txd_name:
            default_name = self.current_txd_name
        else:
            default_name = "untitled.txd"

        # Determine initial directory based on setting
        if self.save_to_source_location:
            # Option 1: Save to source location (default)
            if hasattr(self, 'current_txd_path') and self.current_txd_path:
                # Use the original TXD file's directory
                initial_path = self.current_txd_path
            elif hasattr(self, 'current_img') and self.current_img and hasattr(self.current_img, 'file_path'):
                # Use IMG file's directory
                img_dir = os.path.dirname(self.current_img.file_path)
                initial_path = os.path.join(img_dir, default_name)
            elif self.last_save_directory:
                # Use last saved directory
                initial_path = os.path.join(self.last_save_directory, default_name)
            else:
                # Fallback to just filename
                initial_path = default_name
        else:
            # Option 2: Use last saved directory or current directory
            if self.last_save_directory:
                initial_path = os.path.join(self.last_save_directory, default_name)
            else:
                initial_path = default_name

        file_path, _ = QFileDialog.getSaveFileName(
            self, "Save TXD File",
            initial_path,
            "TXD / XTX Files (*.txd *.xtx);;TXD Files (*.txd);;XTX Textures (*.xtx);;All Files (*)"
        )

        if not file_path:
            return

        try:
            # Rebuild TXD data
            modified_txd_data = self._rebuild_txd_data()

            if not modified_txd_data:
                QMessageBox.critical(self, "Error", "Failed to rebuild TXD data")
                return

            # Write to file
            from apps.methods.file_backup import backup_file, note_change
            if os.path.exists(file_path):
                note_change(f"Save TXD {os.path.basename(file_path)}")
                if backup_file(file_path) is None:
                    QMessageBox.warning(self, "Save", "Backup failed - file not overwritten.")
                    return
            with open(file_path, 'wb') as f:
                f.write(modified_txd_data)

            # Store paths for next time
            self.current_txd_path = file_path
            self.current_txd_name = os.path.basename(file_path)
            self.last_save_directory = os.path.dirname(file_path)
            self._save_settings()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Saved TXD file: {file_path}")

            QMessageBox.information(self, "Success",
                f"TXD saved successfully!\n\n{file_path}")

            # Clear modified state
            self.save_txd_btn.setEnabled(False)
            self.save_txd_btn.setStyleSheet("")
            title = self.windowTitle().replace("*", "")
            self.setWindowTitle(title)

        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to save TXD:\n\n{str(e)}")

    def save_txd_file(self): #vers 6
        """Save TXD file with version selector"""
        from PyQt6.QtWidgets import QMessageBox

        if not self.current_img:
            # Standalone TXD save with version selector
            return self._save_as_txd_file_with_version_selector()
        else:
            # IMG-based TXD save with version selector
            return self._save_txd_to_img_with_version_selector()

    def _save_txd_file(self): #vers 2
        """Save TXD file with detailed structural logging"""
        if not self.current_txd_path and not self.current_txd_name:
            QMessageBox.warning(self, "No TXD", "No TXD file loaded")
            return

        try:
            from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout,
                                        QTextEdit, QPushButton, QLabel, QProgressBar)
            from PyQt6.QtCore import Qt
            import struct

            # Ask for save location
            if self.current_txd_path:
                default_path = self.current_txd_path
            else:
                default_path = self.current_txd_name

            file_path, _ = QFileDialog.getSaveFileName(
                self, "Save TXD File", default_path,
                "TXD / XTX Files (*.txd *.xtx);;TXD Files (*.txd);;XTX Textures (*.xtx);;All Files (*)"
            )

            if not file_path:
                return

            # Create detailed progress dialog with log
            dialog = QDialog(self)
            dialog.setWindowTitle("TXD Structural Builder")
            dialog.setMinimumWidth(800)
            dialog.setMinimumHeight(600)
            dialog.setModal(True)

            layout = QVBoxLayout(dialog)

            # Header
            header = QLabel(f"Building TXD: {os.path.basename(file_path)}")
            header.setStyleSheet("font-size: 14px; font-weight: bold; padding: 5px;")
            layout.addWidget(header)

            # Progress bar
            progress_bar = QProgressBar()
            progress_bar.setRange(0, 100)
            progress_bar.setValue(0)
            layout.addWidget(progress_bar)

            # Log output
            log_output = QTextEdit()
            log_output.setReadOnly(True)
            log_output.setStyleSheet("font-family: 'Courier New', monospace; font-size: 10px;")
            layout.addWidget(log_output)

            # Button layout
            button_layout = QHBoxLayout()

            cancel_btn = QPushButton("Cancel")
            cancel_btn.setStyleSheet("background-color: palette(highlight); color: palette(highlightedText);")
            button_layout.addWidget(cancel_btn)

            button_layout.addStretch()
            layout.addLayout(button_layout)

            # Show dialog
            dialog.show()
            dialog.raise_()
            dialog.activateWindow()

            user_cancelled = False

            def log(message):  #vers 1
                """Add message to log output"""
                log_output.append(message)
                log_output.verticalScrollBar().setValue(log_output.verticalScrollBar().maximum())
                dialog.repaint()

            def update_progress(value, message=None):  #vers 1
                """Update progress bar and optionally log"""
                progress_bar.setValue(value)
                if message:
                    log(message)
                if user_cancelled:
                    raise Exception("Save cancelled by user")

            def handle_cancel():  #vers 1
                nonlocal user_cancelled
                user_cancelled = True

            cancel_btn.clicked.connect(handle_cancel)

            # Start building
            update_progress(1, "=" * 80)
            update_progress(1, "TXD STRUCTURAL BUILDER - INITIALIZING")
            update_progress(1, "=" * 80)

            log("")
            log("PHASE 1: PRE-BUILD VALIDATION")
            log("-" * 80)
            update_progress(5)

            if not self.texture_list:
                raise Exception("No textures to save")

            log(f"Output File    : {file_path}")
            log(f"Texture Count  : {len(self.texture_list)}")
            log(f"RW Version     : 0x{self.txd_version_id:08X}")
            log(f"Device ID      : 0x{self.txd_device_id:08X}")

            # Validate textures
            log("")
            log("Validating textures...")
            for idx, tex in enumerate(self.texture_list):
                tex_name = tex.get('name', f'texture_{idx}')
                has_data = bool(tex.get('rgba_data') or tex.get('compressed_data') or tex.get('original_bgra_data'))
                log(f"  [{idx+1}] {tex_name}: {'OK' if has_data else 'MISSING DATA'}")

                if not has_data:
                    raise Exception(f"Texture {tex_name} has no image data")

            # Initialize serializer
            log("")
            log("PHASE 2: INITIALIZING SERIALIZER")
            log("-" * 80)
            update_progress(10)

            from apps.methods.txd_serializer import TXDSerializer
            log("Loaded serializer from apps.methods.txd_serializer.py")

            serializer = TXDSerializer()
            log("Serializer initialized")

            # Build texture sections
            log("")
            log(f"PHASE 3: BUILDING {len(self.texture_list)} TEXTURE NATIVE SECTIONS")
            log("=" * 80)
            update_progress(15)

            texture_sections = []

            for idx, texture in enumerate(self.texture_list):
                texture_progress = 15 + int((idx / len(self.texture_list)) * 50)

                tex_name = texture.get('name', f'texture_{idx}')
                tex_width = texture.get('width', 0)
                tex_height = texture.get('height', 0)
                tex_format = texture.get('format', 'Unknown')
                has_alpha = texture.get('has_alpha', False)
                alpha_name = texture.get('alpha_name', '')

                log("")
                log(f"[TEXTURE {idx+1}/{len(self.texture_list)}]")
                log("-" * 80)
                log(f"  Name         : {tex_name}")
                log(f"  Dimensions   : {tex_width}x{tex_height}")
                log(f"  Format       : {tex_format}")
                log(f"  Depth        : {texture.get('depth', 32)}-bit")
                log(f"  Alpha        : {has_alpha}")
                if has_alpha:
                    log(f"  Alpha Name   : {alpha_name}")

                # Check data preservation
                has_compressed = bool(texture.get('compressed_data'))
                has_original_bgra = bool(texture.get('original_bgra_data'))
                has_rgba = bool(texture.get('rgba_data'))

                log(f"  Data Sources :")
                log(f"    Compressed     : {'YES' if has_compressed else 'NO'} ({len(texture.get('compressed_data', b'')):,} bytes)")
                log(f"    Original BGRA  : {'YES' if has_original_bgra else 'NO'} ({len(texture.get('original_bgra_data', b'')):,} bytes)")
                log(f"    RGBA (display) : {'YES' if has_rgba else 'NO'} ({len(texture.get('rgba_data', b'')):,} bytes)")

                # Mipmaps
                mipmap_levels = texture.get('mipmap_levels', [])
                if mipmap_levels:
                    log(f"  Mipmaps      : {len(mipmap_levels)} levels")
                    for level_idx, level in enumerate(mipmap_levels):
                        level_width = level.get('width', 0)
                        level_height = level.get('height', 0)
                        level_has_compressed = bool(level.get('compressed_data'))
                        level_has_bgra = bool(level.get('original_bgra_data'))
                        log(f"    Level {level_idx}: {level_width}x{level_height} | Compressed: {level_has_compressed} | BGRA: {level_has_bgra}")

                # Bumpmap
                if texture.get('has_bumpmap', False):
                    bumpmap_size = len(texture.get('bumpmap_data', b''))
                    bumpmap_type = texture.get('bumpmap_type', 0)
                    type_names = ['Height', 'Normal', 'Combined']
                    log(f"  Bumpmap      : {type_names[bumpmap_type]} ({bumpmap_size:,} bytes)")

                # Reflection
                if texture.get('has_reflection', False):
                    reflection_size = len(texture.get('reflection_map', b''))
                    fresnel_size = len(texture.get('fresnel_map', b''))
                    log(f"  Reflection   : {reflection_size:,} bytes")
                    if fresnel_size:
                        log(f"  Fresnel      : {fresnel_size:,} bytes")

                update_progress(texture_progress, f"  Building texture native section...")

                # Build texture native
                try:
                    tex_section = serializer._build_texture_native(texture)
                    texture_sections.append(tex_section)

                    log(f"  Section Size : {len(tex_section):,} bytes")
                    log(f"  Result       : SUCCESS")

                except Exception as e:
                    log(f"  Result       : FAILED - {str(e)}")
                    raise Exception(f"Failed to build texture {tex_name}: {str(e)}")

            # Build TXD dictionary
            log("")
            log("PHASE 4: BUILDING TXD DICTIONARY STRUCTURE")
            log("=" * 80)
            update_progress(65)

            log("")
            log("Building main TXD dictionary header...")

            # Calculate sizes
            struct_size = 4  # texture count (u32)
            struct_data = struct.pack('<I', len(self.texture_list))

            log(f"  Struct Section:")
            log(f"    Type         : 0x01 (Struct)")
            log(f"    Size         : {struct_size} bytes")
            log(f"    Data         : Texture count = {len(self.texture_list)}")

            total_size = 12 + struct_size + 12  # struct header + data + extension header
            for tex_section in texture_sections:
                total_size += len(tex_section)

            log(f"  Main Dictionary:")
            log(f"    Type         : 0x16 (Texture Dictionary)")
            log(f"    Total Size   : {total_size:,} bytes")
            log(f"    Version      : 0x{serializer.RW_VERSION:08X}")

            update_progress(70, "Assembling TXD structure...")

            # Build complete TXD
            result = bytearray()

            # Write Texture Dictionary header
            log("")
            log("Writing TXD sections:")
            log(f"  [Offset 0] Main TXD Dictionary header (12 bytes)")
            result.extend(serializer._write_section_header(
                serializer.SECTION_TEXTURE_DICTIONARY,
                total_size - 12,
                serializer.RW_VERSION
            ))

            # Write Struct section
            log(f"  [Offset {len(result)}] Struct section header (12 bytes)")
            result.extend(serializer._write_section_header(
                serializer.SECTION_STRUCT,
                struct_size,
                serializer.RW_VERSION
            ))

            log(f"  [Offset {len(result)}] Struct data ({struct_size} bytes)")
            result.extend(struct_data)

            # Write texture sections
            for idx, tex_section in enumerate(texture_sections):
                update_progress(70 + int((idx / len(texture_sections)) * 20))
                tex_name = self.texture_list[idx].get('name', f'texture_{idx}')
                log(f"  [Offset {len(result)}] Texture {idx+1} ({tex_name}): {len(tex_section):,} bytes")
                result.extend(tex_section)

            # Write Extension section
            log(f"  [Offset {len(result)}] Extension section (12 bytes)")
            result.extend(serializer._write_section_header(
                serializer.SECTION_EXTENSION,
                0,
                serializer.RW_VERSION
            ))

            # Write to file
            log("")
            log("PHASE 5: WRITING TO DISK")
            log("=" * 80)
            update_progress(90)

            log(f"Final TXD size: {len(result):,} bytes ({len(result)/1024:.2f} KB)")
            log(f"Writing to: {file_path}")

            from apps.methods.file_backup import backup_file, note_change
            if os.path.exists(file_path):
                note_change(f"Save TXD {os.path.basename(file_path)}")
                if backup_file(file_path) is None:
                    QMessageBox.warning(self, "Save", "Backup failed - file not overwritten.")
                    return
            with open(file_path, 'wb') as f:
                f.write(result)

            update_progress(95)
            log("File written successfully")

            # Verify file
            log("")
            log("PHASE 6: VERIFICATION")
            log("-" * 80)

            if os.path.exists(file_path):
                file_size = os.path.getsize(file_path)
                log(f"File exists: YES")
                log(f"File size: {file_size:,} bytes")

                if file_size == len(result):
                    log("Size verification: PASSED")
                else:
                    log(f"Size verification: FAILED (expected {len(result):,}, got {file_size:,})")

            # Complete
            log("")
            log("=" * 80)
            log("TXD BUILD COMPLETE")
            log("=" * 80)
            log(f"Textures saved: {len(self.texture_list)}")
            log(f"Output file: {file_path}")
            log(f"Total size: {len(result):,} bytes ({len(result)/1024:.2f} KB)")

            update_progress(100)

            # Change button to close
            cancel_btn.setText("Close")
            cancel_btn.setStyleSheet("background-color: palette(button); color: palette(buttonText);")
            try: cancel_btn.clicked.disconnect()
            except Exception: pass
            cancel_btn.clicked.connect(dialog.accept)

            dialog.exec()

            # Update internal state
            self.current_txd_path = file_path
            self.current_txd_name = os.path.basename(file_path)

            # Clear modified flag
            self.save_txd_btn.setEnabled(False)
            self.save_txd_btn.setStyleSheet("")
            title = self.windowTitle().replace("*", "")
            self.setWindowTitle(title)

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Saved TXD: {file_path} ({len(self.texture_list)} textures)")

            QMessageBox.information(self, "Save Complete",
                f"TXD file saved successfully:\n\n{file_path}\n\n"
                f"Textures: {len(self.texture_list)}\n"
                f"Size: {len(result):,} bytes ({len(result)/1024:.2f} KB)")

        except Exception as e:
            if 'dialog' in locals():
                dialog.close()

            if "cancelled" not in str(e).lower():
                QMessageBox.critical(self, "Save Error", f"Failed to save TXD:\n\n{str(e)}")

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"TXD save error: {str(e)}")

    def _save_as_txd_file_with_version_selector(self): #vers 2
        """Save standalone TXD with version selector"""
        from PyQt6.QtWidgets import QFileDialog, QMessageBox
        import os

        # Show version selector
        version_info = self._show_version_selector_dialog()
        if not version_info:
            return  # User cancelled

        target_version, target_device, game_idx = version_info

        # Strip unsupported features based on game version
        self._strip_unsupported_features_for_version(game_idx)

        # Determine default filename
        if self.current_txd_name:
            default_name = self.current_txd_name
        else:
            default_name = "untitled.txd"

        # Determine initial directory
        if self.save_to_source_location:
            if hasattr(self, 'current_txd_path') and self.current_txd_path:
                initial_path = self.current_txd_path
            elif hasattr(self, 'current_img') and self.current_img and hasattr(self.current_img, 'file_path'):
                img_dir = os.path.dirname(self.current_img.file_path)
                initial_path = os.path.join(img_dir, default_name)
            elif hasattr(self, 'last_save_directory') and self.last_save_directory:
                initial_path = os.path.join(self.last_save_directory, default_name)
            else:
                initial_path = default_name
        else:
            if hasattr(self, 'last_save_directory') and self.last_save_directory:
                initial_path = os.path.join(self.last_save_directory, default_name)
            else:
                initial_path = default_name

        file_path, _ = QFileDialog.getSaveFileName(
            self, "Save TXD File",
            initial_path,
            "TXD / XTX Files (*.txd *.xtx);;TXD Files (*.txd);;XTX Textures (*.xtx);;All Files (*)"
        )

        if not file_path:
            return

        try:
            # Store target version for rebuild
            self._save_target_version = target_version
            self._save_target_device = target_device

            # Rebuild TXD data with target version
            modified_txd_data = self._rebuild_txd_data()

            if not modified_txd_data:
                QMessageBox.critical(self, "Error", "Failed to rebuild TXD data")
                return

            # Write to file
            from apps.methods.file_backup import backup_file, note_change
            if os.path.exists(file_path):
                note_change(f"Save TXD {os.path.basename(file_path)}")
                if backup_file(file_path) is None:
                    QMessageBox.warning(self, "Save", "Backup failed - file not overwritten.")
                    return
            with open(file_path, 'wb') as f:
                f.write(modified_txd_data)

            # Store paths
            self.current_txd_path = file_path
            self.current_txd_name = os.path.basename(file_path)
            self.last_save_directory = os.path.dirname(file_path)
            self._save_settings()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Saved TXD: {file_path}")
                self.main_window.log_message(f"   Version: 0x{target_version:08X}, Device: 0x{target_device:02X}")

            QMessageBox.information(self, "Success",
                f"TXD saved successfully!\n\n{file_path}\n\nVersion: 0x{target_version:08X}")

            # Clear modified state
            if hasattr(self, 'save_txd_btn'):
                self.save_txd_btn.setEnabled(False)
                self.save_txd_btn.setStyleSheet("")
            title = self.windowTitle().replace("*", "")
            self.setWindowTitle(title)

            # Clean up
            if hasattr(self, '_save_target_version'):
                delattr(self, '_save_target_version')
            if hasattr(self, '_save_target_device'):
                delattr(self, '_save_target_device')

        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to save TXD:\n\n{str(e)}")

    def _save_txd_to_img_with_version_selector(self): #vers 1
        """Save TXD back to IMG with version selector"""
        from PyQt6.QtWidgets import QMessageBox

        if not self.current_img or not self.current_txd_name:
            QMessageBox.warning(self, "Cannot Save", "No IMG archive or TXD loaded")
            return

        # Show version selector
        version_info = self._show_version_selector_dialog()
        if not version_info:
            return  # User cancelled

        target_version, target_device, game_idx = version_info

        # Strip unsupported features
        self._strip_unsupported_features_for_version(game_idx)

        try:
            # Store target version for rebuild
            self._save_target_version = target_version
            self._save_target_device = target_device

            # Rebuild TXD data
            modified_txd_data = self._rebuild_txd_data()

            if not modified_txd_data:
                QMessageBox.critical(self, "Error", "Failed to rebuild TXD data")
                return

            # Find entry in IMG
            entry = None
            for e in self.current_img.entries:
                if e.name.lower() == self.current_txd_name.lower():
                    entry = e
                    break

            if not entry:
                QMessageBox.critical(self, "Error", f"Entry '{self.current_txd_name}' not found in IMG")
                return

            # Update entry data
            entry.data = modified_txd_data
            entry.size = len(modified_txd_data)

            # Mark IMG as modified
            self.current_img.modified = True

            if self.main_window:
                # Refresh table
                if hasattr(self.main_window, '_refresh_table'):
                    self.main_window._refresh_table()

                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"Updated {self.current_txd_name} in IMG")
                    self.main_window.log_message(f"   Version: 0x{target_version:08X}, Device: 0x{target_device:02X}")
                    self.main_window.log_message(f"   Size: {len(modified_txd_data)} bytes")

            QMessageBox.information(self, "Success",
                f"TXD updated in IMG archive!\n\n{self.current_txd_name}\nVersion: 0x{target_version:08X}")

            # Clear modified state
            if hasattr(self, 'save_txd_btn'):
                self.save_txd_btn.setEnabled(False)
                self.save_txd_btn.setStyleSheet("")
            title = self.windowTitle().replace("*", "")
            self.setWindowTitle(title)

            # Clean up
            if hasattr(self, '_save_target_version'):
                delattr(self, '_save_target_version')
            if hasattr(self, '_save_target_device'):
                delattr(self, '_save_target_device')

        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to save to IMG:\n\n{str(e)}")

    def _save_texture_name(self): #vers 1
        """Save edited texture name"""
        if not self.selected_texture:
            return

        new_name = self.info_name.text().strip()
        if new_name and new_name != self.selected_texture.get('name', ''):
            old_name = self.selected_texture.get('name', '')
            self.selected_texture['name'] = new_name
            self._save_undo_state(f"Rename texture: {old_name} -> {new_name}")
            self._reload_texture_table()
            self._mark_as_modified()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Renamed: {old_name} -> {new_name}")

        self.info_name.setReadOnly(True)

    def _save_alpha_name(self): #vers 1
        """Save edited alpha name"""
        if not self.selected_texture or not self.selected_texture.get('has_alpha'):
            return

        new_alpha_name = self.info_alpha_name.text().strip()
        if new_alpha_name and new_alpha_name != self.selected_texture.get('alpha_name', ''):
            old_name = self.selected_texture.get('alpha_name', '')
            self.selected_texture['alpha_name'] = new_alpha_name
            self._save_undo_state(f"Rename alpha: {old_name} -> {new_alpha_name}")
            self._reload_texture_table()
            self._mark_as_modified()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Alpha renamed: {old_name} -> {new_alpha_name}")

        self.info_alpha_name.setReadOnly(True)

    def _force_save_txd(self): #vers 1
        """Force save TXD regardless of modified state (Alt+Shift+S)"""
        if not self.texture_list:
            QMessageBox.warning(self, "No Textures", "No textures to save")
            return

        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message("Force save triggered (Alt+Shift+S)")

        # Temporarily mark as modified to enable save
        original_title = self.windowTitle()
        if not original_title.endswith("*"):
            self.setWindowTitle(original_title + "*")

        # Call save function
        self._save_txd_file()

    def _check_alpha_validity(self, texture): #vers 1
        """Check if normal and alpha channels contain the same image"""
        if not texture or not texture.get('has_alpha', False):
            QMessageBox.information(self, "No Alpha", "This texture has no alpha channel")
            return

        rgba_data = texture.get('rgba_data', b'')
        if not rgba_data:
            return

        width = texture.get('width', 0)
        height = texture.get('height', 0)

        # Check by comparing dimensions first (fast)
        # Then check if RGB matches alpha (slower)

        matches_found = 0
        total_pixels = width * height

        for i in range(0, len(rgba_data), 4):
            r = rgba_data[i]
            g = rgba_data[i + 1]
            b = rgba_data[i + 2]
            a = rgba_data[i + 3]

            # Calculate luminosity of RGB
            luminosity = int(0.299 * r + 0.587 * g + 0.114 * b)

            # Check if alpha matches luminosity (within tolerance)
            if abs(luminosity - a) < 10:
                matches_found += 1

        match_percentage = (matches_found / total_pixels) * 100

        result_text = f"Alpha Validity Check Results:\n\n"
        result_text += f"Texture: {texture.get('name')}\n"
        result_text += f"Dimensions: {width}x{height}\n"
        result_text += f"Total Pixels: {total_pixels:,}\n\n"
        result_text += f"Alpha-RGB Match: {match_percentage:.1f}%\n\n"

        if match_percentage > 90:
            result_text += "WARNING: Normal and alpha appear to contain\n"
            result_text += "the same image data. This may indicate an error.\n"
            result_text += "Consider regenerating the alpha channel."
        elif match_percentage > 50:
            result_text += "CAUTION: Significant similarity between\n"
            result_text += "normal and alpha channels detected."
        else:
            result_text += "Normal and alpha channels appear distinct."

        QMessageBox.information(self, "Alpha Validity Check", result_text)

    def _parse_single_texture(self, txd_data, offset, index, rw_version=0x1803FFFF): #vers 6
        """
        Parse single texture from TXD with bumpmap and reflection support
        ADDED: Extract separate alpha mask for display switching
        """
        import struct

        tex = {
            'name': f'texture_{index}',
            'width': 0,
            'height': 0,
            'depth': 32,
            'format': 'DXT1',
            'has_alpha': False,
            'mipmaps': 1,
            'rgba_data': b'',
            'alpha_mask': b'',              # NEW: Separate grayscale alpha channel
            'compressed_data': b'',
            'original_bgra_data': b'',
            'mipmap_levels': [],
            'bumpmap_data': b'',
            'bumpmap_type': 0,
            'has_bumpmap': False,
            'reflection_map': b'',
            'fresnel_map': b'',
            'has_reflection': False,
            'raster_format_flags': 0
        }

        try:
            # TextureNative structure
            parent_type, parent_size, parent_version = struct.unpack('<III', txd_data[offset:offset+12])

            if parent_type != 0x15:
                return tex

            # Struct section
            struct_offset = offset + 12
            struct_type, struct_size, struct_version = struct.unpack('<III', txd_data[struct_offset:struct_offset+12])

            if struct_type != 0x01:
                return tex

            pos = struct_offset + 12

            # Read 88-byte header
            platform_id, filter_mode, uv_addressing = struct.unpack('<I2B', txd_data[pos:pos+6])[:3]

            #    Xbox (platform_id == 5): delegate to Xbox parser               
            if platform_id == 5:
                try:
                    from apps.methods.txd_platform_xbox import parse_xbox_nativetex
                    xbox_tex = parse_xbox_nativetex(txd_data, offset, index)
                    if xbox_tex:
                        # Decode compressed data to RGBA for display
                        if xbox_tex.get('compressed_data') and not xbox_tex.get('rgba_data'):
                            fmt = xbox_tex.get('format', 'DXT1')
                            w   = xbox_tex.get('width', 0)
                            h   = xbox_tex.get('height', 0)
                            if w > 0 and h > 0 and 'DXT' in fmt:
                                xbox_tex['rgba_data'] = self._decompress_texture(
                                    xbox_tex['compressed_data'], w, h, fmt)
                        return xbox_tex
                except Exception as _xe:
                    print(f"[Xbox TXD] Texture {index}: {_xe}")
                return tex  # return empty rather than crash
            #    End Xbox                                                      

            pos += 8  # Skip padding

            name_bytes = txd_data[pos:pos+32]
            # Use first-null termination (like C strlen) not rstrip — DragonFF does this too
            _null = name_bytes.find(b'\x00')
            tex['name'] = (name_bytes[:_null] if _null >= 0 else name_bytes).decode('ascii', errors='ignore') or f'texture_{index}'
            pos += 32

            mask_bytes = txd_data[pos:pos+32]
            _null2 = mask_bytes.find(b'\x00')
            alpha_name = (mask_bytes[:_null2] if _null2 >= 0 else mask_bytes).decode('ascii', errors='ignore')
            if alpha_name:
                tex['alpha_name'] = alpha_name
                tex['has_alpha'] = True
            pos += 32

            raster_format_flags, d3d_format, width, height, depth, num_levels, raster_type = struct.unpack('<IIHHBBB', txd_data[pos:pos+15])
            tex['width'] = width
            tex['height'] = height
            tex['depth'] = depth
            tex['mipmaps'] = num_levels
            tex['raster_format_flags'] = raster_format_flags

            # Check for bumpmap flag (bit 0x10)
            if raster_format_flags & 0x10:
                tex['has_bumpmap'] = True

            pos += 15

            platform_prop = struct.unpack('<B', txd_data[pos:pos+1])[0]
            pos += 1

            # Format detection - version-aware
            is_pal8     = bool(raster_format_flags & 0x2000)  # FORMAT_EXT_PAL8
            is_pal4     = bool(raster_format_flags & 0x4000)  # FORMAT_EXT_PAL4
            pixel_fmt   = raster_format_flags & 0x0F00  # bits 8-11 only, excludes PAL flags
            is_sa_plus  = (rw_version >= 0x1803FFFF)
            # GTA3/VC: RGBA palette (no swap); SA: BGRA palette (swap B<->R)
            tex['palette_is_bgra'] = is_sa_plus

            raster_pixel_map = {
                0x0100: 'ARGB1555', 0x0200: 'RGB565',
                0x0300: 'ARGB4444', 0x0400: 'LUM8',
                0x0500: 'ARGB8888', 0x0600: 'RGB888',
                0x0A00: 'RGB555',
            }

            # Xbox (platform_id=5): compression byte 0x0C/0x0E/0x10 = DXT1/3/5
            # D3D8 GTA3/VC: platform_prop 1/3/5 = DXT1/3/5
            # SA D3D9: use d3d_format FourCC
            is_xbox = (platform_id == 5)
            if is_pal8:
                tex['format'] = 'PAL8'
            elif is_pal4:
                tex['format'] = 'PAL4'
            elif is_xbox and platform_prop == 0x00:
                # Raw ARGB8888 - use raster_format pixel bits
                tex['format'] = {0x0500:'ARGB8888',0x0600:'RGB888'}.get(pixel_fmt, 'ARGB8888')
            elif is_xbox and platform_prop in (0x0B, 0x0C):
                # 0x0B = LIN_DXT1 (linear/standard), 0x0C = DXT1 (swizzled)
                # Both decode identically via PIL DDS
                tex['format'] = 'DXT1'
            elif is_xbox and platform_prop in (0x0E, 0x0F):
                # 0x0E = DXT3 (swizzled), 0x0F = LIN_DXT3 (linear/standard)
                tex['format'] = 'DXT3'
                tex['has_alpha'] = True
            elif is_xbox and platform_prop in (0x10, 0x11):
                # 0x10 = DXT5 (swizzled), 0x11 = LIN_DXT5 (linear/standard)
                tex['format'] = 'DXT5'
                tex['has_alpha'] = True
            elif d3d_format == 0x31545844:
                tex['format'] = 'DXT1'
            elif d3d_format == 0x33545844:
                tex['format'] = 'DXT3'
                tex['has_alpha'] = True
            elif d3d_format == 0x35545844:
                tex['format'] = 'DXT5'
                tex['has_alpha'] = True
            elif not is_xbox and platform_id == 8 and platform_prop == 1:
                # D3D8 only: platform_prop 1/3/5 = DXT type
                # D3D9 uses d3d_format field — platform_prop is alpha/cube/mip/compressed flags
                tex['format'] = 'DXT1'
            elif not is_xbox and platform_id == 8 and platform_prop == 3:
                tex['format'] = 'DXT3'
                tex['has_alpha'] = True
            elif not is_xbox and platform_id == 8 and platform_prop == 5:
                tex['format'] = 'DXT5'
                tex['has_alpha'] = True
            elif is_sa_plus:
                d3d_fmt_map = {
                    # D3D9 format enum -> internal format name
                    21: 'ARGB8888',  # D3DFMT_A8R8G8B8 - stored BGRA 4bpp
                    22: 'ARGB8888',  # D3DFMT_X8R8G8B8 - stored BGRX 4bpp, treat as ARGB8888 (alpha=255)
                    32: 'ARGB8888',  # D3DFMT_A8B8G8R8
                    20: 'RGB888',    # D3DFMT_R8G8B8   - true 24-bit, rare
                    23: 'RGB565',    # D3DFMT_R5G6B5
                    25: 'ARGB1555',  # D3DFMT_A1R5G5B5
                    26: 'ARGB4444',  # D3DFMT_A4R4G4B4
                    24: 'RGB555',    # D3DFMT_X1R5G5B5
                    50: 'LUM8',      # D3DFMT_L8
                    51: 'A8L8',      # D3DFMT_A8L8
                    41: 'PAL8',      # D3DFMT_P8
                }
                tex['format'] = d3d_fmt_map.get(d3d_format,
                    raster_pixel_map.get(pixel_fmt, f'UNKNOWN_{raster_format_flags:08X}'))
            else:
                tex['format'] = raster_pixel_map.get(pixel_fmt,
                    f'UNKNOWN_{raster_format_flags:08X}')

            # D3DFMT_X8R8G8B8 (22): stored BGRX, X channel is padding not alpha
            if d3d_format == 22:
                tex['force_opaque'] = True  # force alpha=255 when decoding

            if tex['format'] in ('ARGB8888', 'ARGB1555', 'ARGB4444', 'DXT3', 'DXT5', 'PAL8', 'A8L8'):
                if not tex.get('has_alpha') and not tex.get('force_opaque'):
                    tex['has_alpha'] = True

            # Read mipmap data
            # SA (D3D9, RW >= 0x1803FFFF): ALL formats have a 4-byte data_size field PER mipmap level
            # GTA3/VC (D3D8): ONLY DXT formats have data_size per level; raw/PAL data follows directly
            # Xbox (platform_id=5): ONE total data_size field covers ALL mipmap levels combined
            fmt = tex['format']
            is_dxt = 'DXT' in fmt
            is_xbox = (platform_id == 5)
            has_data_size_field = (is_dxt or is_sa_plus) and not is_xbox

            # Xbox: read single total size, then consume all levels from that block
            if is_xbox and pos + 4 <= len(txd_data):
                xbox_total_size = struct.unpack('<I', txd_data[pos:pos+4])[0]
                pos += 4
                xbox_data_block = txd_data[pos:pos+xbox_total_size]
                pos += xbox_total_size
                # Split into per-level chunks using calculated sizes
                block_pos = 0
                w, h = width, height
                for level in range(num_levels):
                    if 'DXT1' in fmt:
                        lsize = max(1,(w+3)//4)*max(1,(h+3)//4)*8
                    elif 'DXT' in fmt:
                        lsize = max(1,(w+3)//4)*max(1,(h+3)//4)*16
                    elif fmt == 'RGB888':
                        lsize = w*h*(4 if depth==32 else 3)
                    elif fmt in ('RGB565','ARGB1555','ARGB4444','RGB555'):
                        lsize = w*h*2
                    else:  # ARGB8888, LUM8, etc
                        lsize = w*h*(depth//8)
                    level_data = xbox_data_block[block_pos:block_pos+lsize]
                    block_pos += lsize
                    lw, lh = w, h
                    if 'DXT' in fmt:
                        rgba_data = self._decompress_texture(level_data, lw, lh, fmt)
                    else:
                        rgba_data = self._decompress_uncompressed(
                            level_data, lw, lh, fmt, depth=depth,
                            force_opaque=tex.get('force_opaque', False))
                    mipmap_level = {'level': level, 'width': lw, 'height': lh,
                        'rgba_data': rgba_data, 'compressed_data': level_data if is_dxt else None,
                        'compressed_size': len(level_data)}
                    tex['mipmap_levels'].append(mipmap_level)
                    if level == 0:
                        tex['rgba_data'] = rgba_data
                        if tex['has_alpha'] and rgba_data and len(rgba_data) == width*height*4:
                            alpha_mask = bytearray(width*height)
                            for i in range(width*height):
                                alpha_mask[i] = rgba_data[i*4+3]
                            tex['alpha_mask'] = bytes(alpha_mask)
                    w = max(1, w//2); h = max(1, h//2)
            else:
                w, h = width, height
                for level in range(num_levels):
                    # Calculate expected size for this mipmap level
                    if 'DXT1' in fmt:
                        expected = max(1, (w+3)//4) * max(1, (h+3)//4) * 8
                    elif 'DXT' in fmt:
                        expected = max(1, (w+3)//4) * max(1, (h+3)//4) * 16
                    elif fmt in ('ARGB8888', 'A8L8'):
                        expected = w * h * 4
                    elif fmt == 'RGB888':
                        expected = w * h * (4 if tex.get('depth', 0) == 32 else 3)
                    elif fmt in ('RGB565', 'ARGB1555', 'ARGB4444', 'RGB555'):
                        expected = w * h * 2
                    elif fmt == 'LUM8':
                        expected = w * h
                    elif fmt == 'PAL8':
                        # Layout: palette(1024 raw) + pixel_size(4) + pixels(w*h)
                        # DragonFF: read_palette has no prefix; read_pixels has 4-byte prefix
                        expected = 1024 + 4 + w * h
                    elif fmt == 'PAL4':
                        # Layout: palette(64 or 128 raw) + pixel_size(4) + pixels((w*h+1)//2)
                        _pal4_sz = 64 if depth == 4 else 128
                        expected = _pal4_sz + 4 + (w * h + 1) // 2
                    else:
                        expected = w * h * 2

                    # Read declared data_size if present, use it if sane
                    if has_data_size_field:
                        if pos + 4 > len(txd_data):
                            break
                        declared = struct.unpack('<I', txd_data[pos:pos+4])[0]
                        pos += 4
                        size = declared if (expected // 2 <= declared <= expected * 4) else expected
                    else:
                        size = expected

                    lw, lh = w, h

                    if pos + size > len(txd_data):
                        break

                    level_data = txd_data[pos:pos+size]
                    pos += size

                    # Decompress if needed
                    lw = max(1, width >> level)
                    lh = max(1, height >> level)
                    if 'DXT' in tex['format']:
                        rgba_data = self._decompress_texture(level_data, lw, lh, tex['format'])
                    elif tex['format'] in ('PAL8', 'PAL4'):
                        # GTA3/VC palettes are RGBA; SA (>=0x1803FFFF) palettes are BGRA
                        # palette_is_bgra stored during header parse
                        pal_entry_fmt = tex.get('palette_entry_format', 'ARGB8888')
                        palette_is_bgra = tex.get('palette_is_bgra', True)
                        # DragonFF: pal8_noalpha when has_alpha()==False
                        # raster_format_type in (888, 565, 555, LUM) -> no alpha
                        _NO_ALPHA_TYPES = {0x0600, 0x0200, 0x0A00, 0x0400}  # 888,565,555,LUM
                        _pix_type = tex.get('raster_format_flags', 0) & 0x0F00
                        force_opaque_pal = _pix_type in _NO_ALPHA_TYPES
                        # PAL8=1024 bytes, PAL4=64 bytes (depth==4) or 128 bytes (depth!=4)
                        # Matches DragonFF read_palette() logic
                        if tex['format'] == 'PAL8':
                            pal_size = 1024
                        elif tex.get('depth', 4) == 4:
                            pal_size = 64
                        else:
                            pal_size = 128
                        if len(level_data) >= pal_size:
                            pal_data = level_data[:pal_size]
                            # Skip the 4-byte pixel data size prefix that follows palette
                            # (DragonFF read_pixels always reads size-prefixed; palette is raw)
                            pix_offset = pal_size + 4
                            pix_data = level_data[pix_offset:]
                            rgba_data = self._decompress_uncompressed(
                                pix_data, lw, lh, tex['format'],
                                palette=pal_data, palette_entry_fmt=pal_entry_fmt,
                                palette_is_bgra=palette_is_bgra,
                                force_opaque=force_opaque_pal)
                        else:
                            rgba_data = b'\x00' * (lw * lh * 4)
                    else:
                        rgba_data = self._decompress_uncompressed(
                            level_data, lw, lh, tex['format'],
                            depth=tex.get('depth', 0),
                            force_opaque=tex.get('force_opaque', False))

                    mipmap_level = {
                        'level': level,
                        'width': max(1, width >> level),
                        'height': max(1, height >> level),
                        'rgba_data': rgba_data,
                        'compressed_data': level_data if 'DXT' in tex['format'] else None,
                        'compressed_size': len(level_data)
                    }
                    tex['mipmap_levels'].append(mipmap_level)

                    # Store main texture data
                    if level == 0:
                        if rgba_data is None:
                            rgba_data = b'\x00' * (lw * lh * 4)
                        tex['rgba_data'] = rgba_data

                        # NEW: Extract alpha channel as separate grayscale mask
                        if tex['has_alpha'] and rgba_data and len(rgba_data) == width * height * 4:
                            alpha_mask = bytearray(width * height)
                            for i in range(width * height):
                                alpha_mask[i] = rgba_data[i * 4 + 3]  # Extract alpha byte
                            tex['alpha_mask'] = bytes(alpha_mask)

                    # Advance mipmap dimensions
                    w = max(1, w // 2)
                    h = max(1, h // 2)

            # Read bumpmap data (if present)
            if tex['has_bumpmap'] and pos + 5 <= len(txd_data):
                try:
                    bumpmap_size = struct.unpack('<I', txd_data[pos:pos+4])[0]
                    pos += 4

                    bumpmap_type = struct.unpack('<B', txd_data[pos:pos+1])[0]
                    pos += 1

                    if pos + bumpmap_size <= len(txd_data):
                        tex['bumpmap_data'] = txd_data[pos:pos+bumpmap_size]
                        tex['bumpmap_type'] = bumpmap_type
                        pos += bumpmap_size

                        if self.main_window and hasattr(self.main_window, 'log_message'):
                            type_names = ['Height Map', 'Normal Map', 'Both']
                            type_name = type_names[bumpmap_type] if bumpmap_type < 3 else 'Unknown'
                            self.main_window.log_message(
                                f"  Bumpmap: {type_name} ({bumpmap_size} bytes)"
                            )
                except Exception as e:
                    if self.main_window and hasattr(self.main_window, 'log_message'):
                        self.main_window.log_message(f"  Bumpmap read error: {str(e)}")

            # Read reflection map data (if present)
            if pos + 8 <= len(txd_data):
                try:
                    reflection_size = struct.unpack('<I', txd_data[pos:pos+4])[0]
                    pos += 4

                    expected_reflection_size = width * height * 3
                    if reflection_size == expected_reflection_size and pos + reflection_size <= len(txd_data):
                        tex['reflection_map'] = txd_data[pos:pos+reflection_size]
                        tex['has_reflection'] = True
                        pos += reflection_size

                        if pos + 4 <= len(txd_data):
                            fresnel_size = struct.unpack('<I', txd_data[pos:pos+4])[0]
                            pos += 4

                            expected_fresnel_size = width * height
                            if fresnel_size == expected_fresnel_size and pos + fresnel_size <= len(txd_data):
                                tex['fresnel_map'] = txd_data[pos:pos+fresnel_size]
                                pos += fresnel_size

                                if self.main_window and hasattr(self.main_window, 'log_message'):
                                    self.main_window.log_message(
                                        f"  Reflection maps: "
                                        f"Vector ({reflection_size}B) + Fresnel ({fresnel_size}B)"
                                    )
                except Exception as e:
                    pass

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Texture parse error: {str(e)}")

        return tex

    def _decompress_texture(self, compressed_data, width, height, format_str): #vers 3
        """
        Decompress DXT texture data to RGBA using PIL (fast) with pure-Python fallback.
        """
        import struct, io
        fmt = 'DXT1' if 'DXT1' in format_str else 'DXT3' if 'DXT3' in format_str else 'DXT5' if 'DXT5' in format_str else None
        if fmt is None:
            return compressed_data

        # --- PIL/DDS path (fast, accurate) ---
        try:
            from PIL import Image
            fourcc = fmt.encode('ascii')
            pitch = max(1, (width + 3) // 4) * (8 if fmt == 'DXT1' else 16)
            hdr = bytearray(128)
            struct.pack_into('<I', hdr,  0, 0x20534444)        # 'DDS '
            struct.pack_into('<I', hdr,  4, 124)               # header size
            struct.pack_into('<I', hdr,  8, 0x1|0x2|0x4|0x1000)
            struct.pack_into('<I', hdr, 12, height)
            struct.pack_into('<I', hdr, 16, width)
            struct.pack_into('<I', hdr, 20, pitch * max(1, (height + 3) // 4))
            struct.pack_into('<I', hdr, 28, 1)
            struct.pack_into('<I', hdr, 76, 32)                # pixel format size
            struct.pack_into('<I', hdr, 80, 0x4)               # DDPF_FOURCC
            hdr[84:88] = fourcc
            dds = bytes(hdr) + compressed_data
            img = Image.open(io.BytesIO(dds)).convert('RGBA')
            return bytes(img.tobytes())
        except Exception:
            pass

        # --- Pure Python fallback ---
        if fmt == 'DXT1':
            return self._decompress_dxt1(compressed_data, width, height)
        elif fmt == 'DXT3':
            return self._decompress_dxt3(compressed_data, width, height)
        else:
            return self._decompress_dxt5(compressed_data, width, height)

    def _decompress_dxt1(self, dxt_data, width, height): #vers 1
        """DXT1 decompression"""
        try:
            import struct
            rgba = bytearray(width * height * 4)
            blocks_x = (width + 3) // 4
            blocks_y = (height + 3) // 4

            for by in range(blocks_y):
                for bx in range(blocks_x):
                    block_offset = (by * blocks_x + bx) * 8
                    if block_offset + 8 > len(dxt_data):
                        break

                    c0, c1 = struct.unpack('<HH', dxt_data[block_offset:block_offset+4])
                    indices = struct.unpack('<I', dxt_data[block_offset+4:block_offset+8])[0]

                    colors = []
                    for c in [c0, c1]:
                        r = ((c >> 11) & 0x1F) << 3
                        g = ((c >> 5) & 0x3F) << 2
                        b = (c & 0x1F) << 3
                        colors.append((r, g, b, 255))

                    if c0 > c1:
                        colors.append(((2*colors[0][0]+colors[1][0])//3, (2*colors[0][1]+colors[1][1])//3, (2*colors[0][2]+colors[1][2])//3, 255))
                        colors.append(((colors[0][0]+2*colors[1][0])//3, (colors[0][1]+2*colors[1][1])//3, (colors[0][2]+2*colors[1][2])//3, 255))
                    else:
                        colors.append(((colors[0][0]+colors[1][0])//2, (colors[0][1]+colors[1][1])//2, (colors[0][2]+colors[1][2])//2, 255))
                        colors.append((0, 0, 0, 0))

                    for py in range(4):
                        for px in range(4):
                            if (bx*4+px < width) and (by*4+py < height):
                                index = (indices >> ((py*4+px)*2)) & 0x03
                                pixel_offset = ((by*4+py)*width+(bx*4+px))*4
                                rgba[pixel_offset:pixel_offset+4] = colors[index]
            return bytes(rgba)
        except:
            return None

    def _decompress_dxt3(self, dxt_data, width, height): #vers 1
        """DXT3 decompression"""
        try:
            import struct
            rgba = bytearray(width * height * 4)
            blocks_x = (width + 3) // 4
            blocks_y = (height + 3) // 4

            for by in range(blocks_y):
                for bx in range(blocks_x):
                    block_offset = (by * blocks_x + bx) * 16
                    if block_offset + 16 > len(dxt_data):
                        break

                    alpha_data = struct.unpack('<Q', dxt_data[block_offset:block_offset+8])[0]
                    c0, c1 = struct.unpack('<HH', dxt_data[block_offset+8:block_offset+12])
                    indices = struct.unpack('<I', dxt_data[block_offset+12:block_offset+16])[0]

                    colors = []
                    for c in [c0, c1]:
                        r = ((c >> 11) & 0x1F) << 3
                        g = ((c >> 5) & 0x3F) << 2
                        b = (c & 0x1F) << 3
                        colors.append((r, g, b))

                    colors.append(((2*colors[0][0]+colors[1][0])//3, (2*colors[0][1]+colors[1][1])//3, (2*colors[0][2]+colors[1][2])//3))
                    colors.append(((colors[0][0]+2*colors[1][0])//3, (colors[0][1]+2*colors[1][1])//3, (colors[0][2]+2*colors[1][2])//3))

                    for py in range(4):
                        for px in range(4):
                            if (bx*4+px < width) and (by*4+py < height):
                                color_index = (indices >> ((py*4+px)*2)) & 0x03
                                alpha_index = py*4 + px
                                alpha = ((alpha_data >> (alpha_index*4)) & 0x0F) * 17
                                pixel_offset = ((by*4+py)*width+(bx*4+px))*4
                                rgba[pixel_offset:pixel_offset+3] = colors[color_index]
                                rgba[pixel_offset+3] = alpha
            return bytes(rgba)
        except:
            return None

    def _decompress_dxt5(self, dxt_data, width, height): #vers 1
        """DXT5 decompression"""
        try:
            import struct
            rgba = bytearray(width * height * 4)
            blocks_x = (width + 3) // 4
            blocks_y = (height + 3) // 4

            for by in range(blocks_y):
                for bx in range(blocks_x):
                    block_offset = (by * blocks_x + bx) * 16
                    if block_offset + 16 > len(dxt_data):
                        break

                    a0 = dxt_data[block_offset]
                    a1 = dxt_data[block_offset + 1]
                    alpha_indices = struct.unpack('<Q', dxt_data[block_offset:block_offset+8])[0] >> 16
                    alpha_palette = [a0, a1]
                    if a0 > a1:
                        # 6 interpolated values: weighted blend a0..a1 in 7 steps
                        for i in range(1, 7):
                            alpha_palette.append(round(a0 * ((7-i)/7) + a1 * (i/7)))
                    else:
                        # 4 interpolated + hard 0 and 255
                        for i in range(1, 5):
                            alpha_palette.append(round(a0 * ((5-i)/5) + a1 * (i/5)))
                        alpha_palette.extend([0, 255])

                    c0, c1 = struct.unpack('<HH', dxt_data[block_offset+8:block_offset+12])
                    indices = struct.unpack('<I', dxt_data[block_offset+12:block_offset+16])[0]

                    colors = []
                    for c in [c0, c1]:
                        r = ((c >> 11) & 0x1F) << 3
                        g = ((c >> 5) & 0x3F) << 2
                        b = (c & 0x1F) << 3
                        colors.append((r, g, b))

                    colors.append(((2*colors[0][0]+colors[1][0])//3, (2*colors[0][1]+colors[1][1])//3, (2*colors[0][2]+colors[1][2])//3))
                    colors.append(((colors[0][0]+2*colors[1][0])//3, (colors[0][1]+2*colors[1][1])//3, (colors[0][2]+2*colors[1][2])//3))

                    for py in range(4):
                        for px in range(4):
                            if (bx*4+px < width) and (by*4+py < height):
                                color_index = (indices >> ((py*4+px)*2)) & 0x03
                                alpha_index = (alpha_indices >> ((py*4+px)*3)) & 0x07
                                pixel_offset = ((by*4+py)*width+(bx*4+px))*4
                                rgba[pixel_offset:pixel_offset+3] = colors[color_index]
                                rgba[pixel_offset+3] = alpha_palette[alpha_index]
            return bytes(rgba)
        except:
            return None

    def _decompress_uncompressed(self, data, width, height, format_type, palette=None, palette_entry_fmt='ARGB8888', depth=0, force_opaque=False, palette_is_bgra=True): #vers 7
        """Decompress all RenderWare uncompressed/palettized formats to RGBA"""
        try:
            import struct
            pixel_count = width * height
            rgba = bytearray(pixel_count * 4)

            if format_type == 'PAL8':
                # 8-bit indexed, 256 x 4-byte palette entries
                # GTA3/VC: palette is RGBA (no swap needed)
                # SA:       palette is BGRA (swap B<->R)
                if not palette or len(palette) < 1024:
                    return None
                force_opaque = (palette_entry_fmt == 'RGB888')
                for i in range(min(pixel_count, len(data))):
                    p = data[i] * 4
                    if p + 3 < len(palette):
                        if palette_is_bgra:
                            b, g, r, a = palette[p], palette[p+1], palette[p+2], palette[p+3]
                        else:
                            r, g, b, a = palette[p], palette[p+1], palette[p+2], palette[p+3]
                        rgba[i*4:i*4+4] = [r, g, b, 255 if force_opaque else a]

            elif format_type == 'PAL4':
                # 4-bit indexed, 16 x 4-byte palette entries
                # GTA3/VC: palette is RGBA; SA: palette is BGRA
                # CRITICAL: high nibble = first pixel, low nibble = second (DragonFF)
                if not palette or len(palette) < 64:
                    return None
                force_opaque = (palette_entry_fmt == 'RGB888')
                pixel = 0
                for i in range(len(data)):
                    for idx in ((data[i] >> 4) & 0x0F, data[i] & 0x0F):
                        if pixel >= pixel_count:
                            break
                        p = idx * 4
                        if p + 3 < len(palette):
                            if palette_is_bgra:
                                b, g, r, a = palette[p], palette[p+1], palette[p+2], palette[p+3]
                            else:
                                r, g, b, a = palette[p], palette[p+1], palette[p+2], palette[p+3]
                            rgba[pixel*4:pixel*4+4] = [r, g, b, 255 if force_opaque else a]
                        pixel += 1

            elif 'ARGB8888' in format_type or 'ARGB32' in format_type:
                # RenderWare stores as BGRA; X8R8G8B8 (force_opaque) has padding not alpha
                for i in range(pixel_count):
                    if i*4+4 <= len(data):
                        b, g, r, a = data[i*4], data[i*4+1], data[i*4+2], data[i*4+3]
                        rgba[i*4:i*4+4] = [r, g, b, 255 if force_opaque else a]

            elif 'RGB888' in format_type:
                # Stored as BGR (3 bpp) or BGRX (4 bpp when depth==32)
                stride = 4 if depth == 32 else 3
                for i in range(pixel_count):
                    if i*stride+stride <= len(data):
                        b, g, r = data[i*stride], data[i*stride+1], data[i*stride+2]
                        rgba[i*4:i*4+4] = [r, g, b, 255]

            elif 'RGB565' in format_type:
                for i in range(pixel_count):
                    if i*2+2 <= len(data):
                        pixel = struct.unpack('<H', data[i*2:i*2+2])[0]
                        r = ((pixel >> 11) & 0x1F) << 3
                        g = ((pixel >> 5) & 0x3F) << 2
                        b = (pixel & 0x1F) << 3
                        rgba[i*4:i*4+4] = [r, g, b, 255]

            elif 'ARGB1555' in format_type:
                for i in range(pixel_count):
                    if i*2+2 <= len(data):
                        pixel = struct.unpack('<H', data[i*2:i*2+2])[0]
                        a = 255 if (pixel & 0x8000) else 0
                        r = ((pixel >> 10) & 0x1F) << 3
                        g = ((pixel >> 5) & 0x1F) << 3
                        b = (pixel & 0x1F) << 3
                        rgba[i*4:i*4+4] = [r, g, b, a]

            elif 'ARGB4444' in format_type:
                for i in range(pixel_count):
                    if i*2+2 <= len(data):
                        pixel = struct.unpack('<H', data[i*2:i*2+2])[0]
                        a = ((pixel >> 12) & 0x0F) * 17
                        r = ((pixel >> 8) & 0x0F) * 17
                        g = ((pixel >> 4) & 0x0F) * 17
                        b = (pixel & 0x0F) * 17
                        rgba[i*4:i*4+4] = [r, g, b, a]

            elif 'RGB555' in format_type:
                for i in range(pixel_count):
                    if i*2+2 <= len(data):
                        pixel = struct.unpack('<H', data[i*2:i*2+2])[0]
                        r = ((pixel >> 10) & 0x1F) << 3
                        g = ((pixel >> 5) & 0x1F) << 3
                        b = (pixel & 0x1F) << 3
                        rgba[i*4:i*4+4] = [r, g, b, 255]

            elif 'A8L8' in format_type:
                for i in range(pixel_count):
                    if i*2+2 <= len(data):
                        lum, a = data[i*2], data[i*2+1]
                        rgba[i*4:i*4+4] = [lum, lum, lum, a]

            elif 'LUM8' in format_type or 'L8' in format_type:
                for i in range(pixel_count):
                    if i < len(data):
                        lum = data[i]
                        rgba[i*4:i*4+4] = [lum, lum, lum, 255]

            else:
                # Unknown format - render as grey so it at least shows something
                for i in range(pixel_count):
                    rgba[i*4:i*4+4] = [128, 128, 128, 255]

            return bytes(rgba)
        except Exception as e:
            return None

    def _create_thumbnail(self, rgba_data, width, height): #vers 2
        """Create thumbnail from RGBA data"""
        try:
            if not rgba_data or width <= 0 or height <= 0:
                return None

            # Keep buffer reference alive - QImage doesn't own it and GC will
            # free it before Qt renders, causing blank thumbnails on small textures
            buf = bytes(rgba_data)
            image = QImage(buf, width, height, width*4, QImage.Format.Format_RGBA8888)
            if image.isNull():
                return None

            # copy() makes Qt own the data - buffer reference no longer needed
            image = image.copy()
            pixmap = QPixmap.fromImage(image)
            thumb_size = 64
            if width < thumb_size and height < thumb_size:
                # Scale up small textures with nearest-neighbour to show pixels clearly
                return pixmap.scaled(thumb_size, thumb_size,
                    Qt.AspectRatioMode.KeepAspectRatio,
                    Qt.TransformationMode.FastTransformation)
            return pixmap.scaled(thumb_size, thumb_size,
                Qt.AspectRatioMode.KeepAspectRatio,
                Qt.TransformationMode.SmoothTransformation)
        except:
            return None

    def _add_warning_badge(self, pixmap): #vers 1
        """Composite a small warning triangle onto bottom-left of thumbnail pixmap"""
        from PyQt6.QtGui import QPainter, QColor, QFont
        from PyQt6.QtCore import Qt
        result = pixmap.copy()
        painter = QPainter(result)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        # Draw small yellow triangle badge (12x12) at bottom-left
        badge_size = 14
        x = 1
        y = result.height() - badge_size - 1
        painter.setBrush(QColor(255, 200, 0, 220))
        painter.setPen(QColor(180, 120, 0, 220))
        from PyQt6.QtGui import QPolygon
        from PyQt6.QtCore import QPoint
        tri = QPolygon([
            QPoint(x + badge_size // 2, y),
            QPoint(x, y + badge_size),
            QPoint(x + badge_size, y + badge_size),
        ])
        painter.drawPolygon(tri)
        # Draw exclamation mark
        painter.setPen(QColor(80, 50, 0, 255))
        font = QFont()
        font.setPixelSize(9)
        font.setBold(True)
        painter.setFont(font)
        painter.drawText(x + 5, y + badge_size - 2, "!")
        painter.end()
        return result

    def _convert_texture(self): #vers 3
        """Convert texture format with GTA III 8-bit support"""
        if not self.selected_texture:
            QMessageBox.warning(self, "No Selection", "Please select a texture first")
            return

        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QHBoxLayout, QLabel, QComboBox, QPushButton, QGroupBox

        dialog = QDialog(self)
        dialog.setWindowTitle("Convert Texture Format")
        dialog.setModal(True)
        dialog.resize(400, 350)

        layout = QVBoxLayout(dialog)

        # Current format
        current_format = self.selected_texture.get('format', 'Unknown')
        current_depth = self.selected_texture.get('depth', 32)
        current_label = QLabel(f"Current: {current_format} ({current_depth}bit)")
        current_label.setStyleSheet("font-weight: bold; font-size: 12px; padding: 10px;")
        layout.addWidget(current_label)

        # Format selection
        format_group = QGroupBox("Convert To")
        format_layout = QVBoxLayout(format_group)

        format_combo = QComboBox()
        format_combo.addItems([
            "DXT1 (No Alpha, 6:1 compression) - 32bit",
            "DXT3 (Sharp Alpha, 4:1 compression) - 32bit",
            "DXT5 (Smooth Alpha, 4:1 compression) - 32bit",
            "ARGB8888 (32-bit Uncompressed)",
            "RGB888 (24-bit No Alpha)",
            "ARGB1555 (16-bit with Alpha)",
            "RGB565 (16-bit No Alpha)",
            "PAL8 (8-bit Indexed - GTA III)"
        ])
        format_layout.addWidget(format_combo)

        # Info label
        info_label = QLabel()
        info_label.setStyleSheet("color: #ff9800; font-size: 10px; padding: 5px;")
        info_label.setWordWrap(True)

        def update_info(index):  #vers 1
            if index == 7:  # PAL8
                info_label.setText("8-bit indexed format (GTA III). Limited to 256 colors with palette.")
            else:
                info_label.setText("Note: Converting between compressed formats may result in quality loss.")

        format_combo.currentIndexChanged.connect(update_info)
        update_info(0)

        format_layout.addWidget(info_label)
        layout.addWidget(format_group)

        # Size estimate
        width = self.selected_texture.get('width', 0)
        height = self.selected_texture.get('height', 0)

        size_group = QGroupBox("Size Estimate")
        size_layout = QVBoxLayout(size_group)

        size_label = QLabel(f"Texture: {width}x{height}")
        size_layout.addWidget(size_label)

        def update_size_estimate(index):  #vers 1
            format_map = [
                ('DXT1', 32), ('DXT3', 32), ('DXT5', 32),
                ('ARGB8888', 32), ('RGB888', 24),
                ('ARGB1555', 16), ('RGB565', 16),
                ('PAL8', 8)
            ]
            selected_format, bit_depth = format_map[index]

            # Calculate estimated size
            pixel_count = width * height
            if 'DXT1' in selected_format:
                estimated = pixel_count // 2
            elif 'DXT' in selected_format:
                estimated = pixel_count
            elif 'PAL8' in selected_format:
                estimated = pixel_count + 1024  # 256 color palette (256 * 4 bytes)
            elif 'ARGB8888' in selected_format:
                estimated = pixel_count * 4
            elif 'RGB888' in selected_format:
                estimated = pixel_count * 3
            else:  # 16-bit formats
                estimated = pixel_count * 2

            size_kb = estimated / 1024
            estimate_label.setText(f"Estimated: {size_kb:.1f} KB ({bit_depth}bit)")

        estimate_label = QLabel("Select format above")
        size_layout.addWidget(estimate_label)

        format_combo.currentIndexChanged.connect(update_size_estimate)
        update_size_estimate(0)

        layout.addWidget(size_group)

        layout.addStretch()

        # Buttons
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        def do_convert():  #vers 1
            format_map = [
                ('DXT1', 32), ('DXT3', 32), ('DXT5', 32),
                ('ARGB8888', 32), ('RGB888', 24),
                ('ARGB1555', 16), ('RGB565', 16),
                ('PAL8', 8)
            ]
            selected_format, bit_depth = format_map[format_combo.currentIndex()]

            self._save_undo_state(f"Convert: {current_format} -> {selected_format}")

            tex = self.selected_texture
            w, h = tex.get('width', 0), tex.get('height', 0)
            rgba = tex.get('rgba_data', b'')

            try:
                from PIL import Image
                if rgba and w and h:
                    img = Image.frombytes('RGBA', (w, h), rgba)

                    if selected_format == 'ARGB8888':
                        tex['rgba_data'] = img.convert('RGBA').tobytes()
                    elif selected_format == 'RGB888':
                        # Strip alpha — convert to RGB then back to RGBA with alpha=255
                        rgb = img.convert('RGB')
                        rgba_out = rgb.convert('RGBA')
                        tex['rgba_data'] = rgba_out.tobytes()
                        tex['has_alpha'] = False
                    elif selected_format in ('RGB565', 'ARGB1555', 'ARGB4444'):
                        # Quantise to 16-bit precision — store as RGBA
                        tex['rgba_data'] = img.convert('RGBA').tobytes()
                        tex['has_alpha'] = selected_format in ('ARGB1555', 'ARGB4444')
                    elif selected_format == 'PAL8':
                        # Quantise to 256-colour palette
                        pal_img = img.convert('P', palette=Image.Palette.ADAPTIVE, colors=256)
                        # Convert back to RGBA for display
                        tex['rgba_data'] = pal_img.convert('RGBA').tobytes()
                        tex['has_alpha'] = False
                    elif selected_format in ('DXT1', 'DXT3', 'DXT5'):
                        # Store RGBA for display; DXT compression applied on TXD save
                        tex['rgba_data'] = img.convert('RGBA').tobytes()
                        tex['has_alpha'] = selected_format in ('DXT3', 'DXT5')

            except Exception as conv_err:
                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"Convert warning: {conv_err}")

            tex['format'] = selected_format
            tex['depth']  = bit_depth

            self._mark_as_modified()
            self._update_texture_info(tex)
            self._update_table_display()
            dialog.accept()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(
                    f"Converted {tex.get('name','')}: {current_format} -> {selected_format}")

        convert_btn = QPushButton("Convert")
        convert_btn.clicked.connect(do_convert)
        button_layout.addWidget(convert_btn)

        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(dialog.reject)
        button_layout.addWidget(cancel_btn)

        layout.addLayout(button_layout)

        dialog.exec()

    def _open_paint_editor(self): #vers 5
        """Open DP5 Workshop paint editor for the selected texture."""
        if not self.selected_texture or not self.selected_texture.get('rgba_data'):
            QMessageBox.warning(self, "No Texture",
                "Select a texture with RGBA data first.")
            return
        try:
            from apps.components.DP5_Workshop.dp5_workshop import DP5Workshop
            from PyQt6.QtWidgets import QVBoxLayout, QDialog

            tex  = self.selected_texture
            w    = tex.get('width', 256)
            h    = tex.get('height', 256)
            rgba = bytearray(tex['rgba_data'])

            # Create DP5 as a modal dialog so TXD Workshop waits for the edit
            dlg = QDialog(self)
            dlg.setWindowTitle(f"DP5 Paint — {tex.get('name', 'texture')}")
            dlg.resize(1400, 820)
            lay = QVBoxLayout(dlg)
            lay.setContentsMargins(0, 0, 0, 0)

            workshop = DP5Workshop(dlg, None)
            workshop.setWindowFlags(Qt.WindowType.Widget)

            # Load the texture through DP5's own loader (size, palette, bitmap list)
            workshop._load_rgba(rgba, w, h, tex.get('name', 'texture'))
            workshop._set_zoom(max(0.05, min(4, 512 / max(w, h, 1))))

            lay.addWidget(workshop)

            # Add OK / Cancel at the bottom
            from PyQt6.QtWidgets import QHBoxLayout, QPushButton
            btn_row = QHBoxLayout()
            btn_row.addStretch()
            ok_btn  = QPushButton("Apply to Texture")
            ok_btn.setDefault(True)
            ok_btn.clicked.connect(dlg.accept)
            can_btn = QPushButton("Cancel")
            can_btn.clicked.connect(dlg.reject)
            btn_row.addWidget(ok_btn); btn_row.addWidget(can_btn)
            lay.addLayout(btn_row)

            if dlg.exec() == QDialog.DialogCode.Accepted:
                # Write edited RGBA back into the texture dict
                if workshop.dp5_canvas:
                    tex['rgba_data'] = bytes(workshop.dp5_canvas.rgba)
                    tex['width']     = workshop.dp5_canvas.tex_w
                    tex['height']    = workshop.dp5_canvas.tex_h
                self._save_undo_state("DP5 Paint edit")
                self._update_texture_info(tex)
                self._update_table_display()
                self._mark_as_modified()

        except Exception as e:
            import traceback; traceback.print_exc()
            QMessageBox.warning(self, "Paint Editor Error", str(e))

    def _open_filters_dialog(self): #vers 1
        """Open filters dialog"""
        if not self.selected_texture:
            return

        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QLabel, QSlider, QPushButton, QHBoxLayout

        dialog = QDialog(self)
        dialog.setWindowTitle("Image Filters")
        dialog.setModal(True)
        dialog.resize(400, 400)

        layout = QVBoxLayout(dialog)

        # Brightness
        layout.addWidget(QLabel("Brightness:"))
        brightness_slider = QSlider(Qt.Orientation.Horizontal)
        brightness_slider.setMinimum(-100)
        brightness_slider.setMaximum(100)
        brightness_slider.setValue(0)
        layout.addWidget(brightness_slider)

        # Contrast
        layout.addWidget(QLabel("Contrast:"))
        contrast_slider = QSlider(Qt.Orientation.Horizontal)
        contrast_slider.setMinimum(-100)
        contrast_slider.setMaximum(100)
        contrast_slider.setValue(0)
        layout.addWidget(contrast_slider)

        # Saturation
        layout.addWidget(QLabel("Saturation:"))
        saturation_slider = QSlider(Qt.Orientation.Horizontal)
        saturation_slider.setMinimum(-100)
        saturation_slider.setMaximum(100)
        saturation_slider.setValue(0)
        layout.addWidget(saturation_slider)

        # Hue
        layout.addWidget(QLabel("Hue Shift:"))
        hue_slider = QSlider(Qt.Orientation.Horizontal)
        hue_slider.setMinimum(0)
        hue_slider.setMaximum(360)
        hue_slider.setValue(0)
        layout.addWidget(hue_slider)

        layout.addStretch()

        # Buttons
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        apply_btn = QPushButton("Apply")
        def _apply_filters():  #vers 1
            if not self.selected_texture or not self.selected_texture.get('rgba_data'):
                return
            try:
                from PIL import Image, ImageEnhance, ImageFilter
                import io
                tex = self.selected_texture
                img = Image.frombytes('RGBA', (tex['width'], tex['height']), tex['rgba_data'])

                b_val = brightness_slider.value() / 100.0
                c_val = contrast_slider.value()    / 100.0
                s_val = saturation_slider.value()  / 100.0
                h_val = hue_slider.value()

                # Brightness: +100 = double, -100 = black
                if b_val != 0:
                    factor = 1.0 + b_val
                    img = ImageEnhance.Brightness(img).enhance(max(0.0, factor))

                # Contrast: +100 = double, -100 = grey
                if c_val != 0:
                    factor = 1.0 + c_val
                    img = ImageEnhance.Contrast(img).enhance(max(0.0, factor))

                # Saturation
                if s_val != 0:
                    factor = 1.0 + s_val
                    img = ImageEnhance.Color(img).enhance(max(0.0, factor))

                # Hue shift via HSV
                if h_val != 0:
                    import colorsys
                    pixels = list(img.getdata())
                    new_pixels = []
                    for r, g, b, a in pixels:
                        h, s, v = colorsys.rgb_to_hsv(r/255, g/255, b/255)
                        h = (h + h_val / 360.0) % 1.0
                        r2, g2, b2 = colorsys.hsv_to_rgb(h, s, v)
                        new_pixels.append((int(r2*255), int(g2*255), int(b2*255), a))
                    img.putdata(new_pixels)

                self._save_undo_state("Apply filters")
                tex['rgba_data'] = img.tobytes()
                self._update_texture_info(tex)
                self._update_table_display()
                self._mark_as_modified()
                dialog.accept()
            except Exception as e:
                QMessageBox.critical(dialog, "Filter Error", str(e))

        apply_btn.clicked.connect(_apply_filters)
        button_layout.addWidget(apply_btn)

        reset_btn = QPushButton("Reset")
        reset_btn.clicked.connect(lambda: [
            brightness_slider.setValue(0),
            contrast_slider.setValue(0),
            saturation_slider.setValue(0),
            hue_slider.setValue(0)
        ])
        button_layout.addWidget(reset_btn)

        close_btn = QPushButton("Close")
        close_btn.clicked.connect(dialog.close)
        button_layout.addWidget(close_btn)

        layout.addLayout(button_layout)

        dialog.exec()

    def _invert_grayscale(self, grayscale_data): #vers 1
        """Invert grayscale RGBA data"""
        inverted = bytearray(grayscale_data)
        for i in range(0, len(inverted), 4):
            inverted[i] = 255 - inverted[i]
            inverted[i + 1] = 255 - inverted[i + 1]
            inverted[i + 2] = 255 - inverted[i + 2]
        return bytes(inverted)

    def _view_bumpmap(self): #vers 4
        """Open Bumpmap Manager window - ALWAYS opens manager regardless of bumpmap state"""
        if not self.selected_texture:
            QMessageBox.warning(self, "No Selection", "Please select a texture")
            return

        # Check version support - show warning but context matters
        if not is_bumpmap_supported(self.txd_version_id, self.txd_device_id):
            QMessageBox.warning(self, "Not Supported",
                f"Bumpmaps not supported for {self.txd_game}\n"
                f"Only San Andreas and Manhunt support bumpmaps")
            return

        # Open Bumpmap Manager window (works with or without existing bumpmap)
        manager = BumpmapManagerWindow(self, self.selected_texture, self.main_window)
        manager.show()

        if self.main_window and hasattr(self.main_window, 'log_message'):
            has_bumpmap = self._has_bumpmap_data(self.selected_texture) if hasattr(self, '_has_bumpmap_data') else False
            status = "with bumpmap" if has_bumpmap else "no bumpmap (can generate/import)"
            self.main_window.log_message(
                f"Opened Bumpmap Manager: {self.selected_texture['name']} ({status})"
            )

    def _export_bumpmap(self): #vers 1
        """Export bumpmap as separate image file"""
        if not self.selected_texture:
            return

        try:
            texture_data = self.selected_texture
            texture_name = texture_data.get('name', 'texture')

            # Get save path
            file_path, _ = QFileDialog.getSaveFileName(
                self, "Export Bumpmap",
                f"{texture_name}_bumpmap.png",
                "PNG Images (*.png);;All Files (*)"
            )

            if not file_path:
                return

            # Extract and save bumpmap
            if 'bumpmap_data' in texture_data:
                bumpmap_image = self._decode_bumpmap(texture_data['bumpmap_data'])

                if bumpmap_image.save(file_path):
                    QMessageBox.information(self, "Success",
                        f"Bumpmap exported to:\n{file_path}")

                    if self.main_window and hasattr(self.main_window, 'log_message'):
                        self.main_window.log_message(f"Exported bumpmap: {os.path.basename(file_path)}")
                else:
                    QMessageBox.warning(self, "Error", "Failed to save bumpmap")
            else:
                QMessageBox.information(self, "No Bumpmap",
                    "No bumpmap data found in texture")

        except Exception as e:
            QMessageBox.critical(self, "Error", f"Export failed: {str(e)}")

    def _import_bumpmap(self): #vers 2
        """Import bumpmap from image file"""
        if not self.selected_texture:
            QMessageBox.warning(self, "No Selection",
                "Please select a texture to add bumpmap to")
            return

        # Check if version supports bumpmaps
        if not is_bumpmap_supported(self.txd_version_id, self.txd_device_id):
            QMessageBox.warning(self, "Not Supported",
                f"Bumpmaps not supported for {self.txd_game}\n"
                f"Only San Andreas and State of Liberty support bumpmaps")
            return

        try:
            file_path, _ = QFileDialog.getOpenFileName(
                self, "Import Bumpmap",
                "",
                "Image Files (*.png *.jpg *.bmp *.tga);;All Files (*)"
            )

            if not file_path:
                return

            # Load bumpmap image
            bumpmap_image = QImage(file_path)

            if bumpmap_image.isNull():
                QMessageBox.warning(self, "Error", "Failed to load bumpmap image")
                return

            # Encode bumpmap data
            bumpmap_data = self._encode_bumpmap(bumpmap_image)

            self._save_undo_state("Import bumpmap")
            # Add to texture data
            self.selected_texture['bumpmap_data'] = bumpmap_data
            self.selected_texture['has_bumpmap'] = True

            # Mark as modified
            self._mark_as_modified()

            # Update UI
            self._update_texture_info(self.selected_texture)

            QMessageBox.information(self, "Success",
                "Bumpmap imported successfully")

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(
                    f"Imported bumpmap from: {os.path.basename(file_path)}"
                )

        except Exception as e:
            QMessageBox.critical(self, "Error", f"Import failed: {str(e)}")

    def _encode_bumpmap(self, image: QImage) -> bytes: #vers 1
        """Encode image as grayscale height map at texture size."""
        width = self.selected_texture.get('width', image.width())
        height = self.selected_texture.get('height', image.height())
        img = image.scaled(width, height).convertToFormat(QImage.Format.Format_Grayscale8)
        bpl = img.bytesPerLine()
        raw = bytes(img.constBits().asstring(bpl * height))
        return b''.join(raw[y * bpl:y * bpl + width] for y in range(height))

    def _decode_bumpmap(self, bumpmap_data: bytes) -> QImage: #vers 3
        """Decode bumpmap data to QImage - supports all types"""
        try:
            width = self.selected_texture.get('width', 256)
            height = self.selected_texture.get('height', 256)

            expected_gray = width * height
            expected_rgb = width * height * 3

            # Detect format
            if len(bumpmap_data) == expected_rgb:
                # RGB Normal Map
                image = QImage(bytes(bumpmap_data), width, height, width * 3, QImage.Format.Format_RGB888)
                return image

            elif len(bumpmap_data) > expected_gray and bumpmap_data[0] == 2:
                # Combined type (both height + normal)
                # Skip type byte, extract normal map portion
                offset = 1 + expected_gray
                normal_data = bumpmap_data[offset:offset + expected_rgb]
                image = QImage(bytes(normal_data), width, height, width * 3, QImage.Format.Format_RGB888)
                return image

            else:
                # Grayscale Height Map - convert to RGB for display
                rgb_data = bytearray(expected_rgb)
                for i in range(min(expected_gray, len(bumpmap_data))):
                    value = bumpmap_data[i]
                    rgb_data[i*3] = value
                    rgb_data[i*3+1] = value
                    rgb_data[i*3+2] = value

                image = QImage(bytes(rgb_data), width, height, width * 3, QImage.Format.Format_RGB888)
                return image

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Bumpmap decode error: {str(e)}")
            return QImage()

    def _flip_vertical(self): #vers 3
        """Flip texture vertically using PIL (fast)."""
        if not self.selected_texture or not self.selected_texture.get('rgba_data'):
            QMessageBox.warning(self, "No Selection", "Please select a texture first")
            return
        try:
            from PIL import Image
            tex = self.selected_texture
            img = Image.frombytes('RGBA', (tex['width'], tex['height']), tex['rgba_data'])
            flipped = img.transpose(Image.Transpose.FLIP_TOP_BOTTOM)
            self._save_undo_state("Flip vertical")
            tex['rgba_data'] = flipped.tobytes()
            self._update_texture_info(tex)
            self._update_table_display()
            self._mark_as_modified()
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("Flipped vertically")
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to flip: {str(e)}")

    def _flip_horizontal(self): #vers 2
        """Flip texture horizontally using PIL (fast)."""
        if not self.selected_texture or not self.selected_texture.get('rgba_data'):
            QMessageBox.warning(self, "No Selection", "Please select a texture first")
            return
        try:
            from PIL import Image
            tex = self.selected_texture
            img = Image.frombytes('RGBA', (tex['width'], tex['height']), tex['rgba_data'])
            flipped = img.transpose(Image.Transpose.FLIP_LEFT_RIGHT)
            self._save_undo_state("Flip horizontal")
            tex['rgba_data'] = flipped.tobytes()
            self._update_texture_info(tex)
            self._update_table_display()
            self._mark_as_modified()
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("Flipped horizontally")
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to flip: {str(e)}")

    def _rotate_clockwise(self): #vers 2
        """Rotate texture 90° CW using PIL (fast)."""
        if not self.selected_texture or not self.selected_texture.get('rgba_data'):
            QMessageBox.warning(self, "No Selection", "Please select a texture first")
            return
        try:
            from PIL import Image
            tex = self.selected_texture
            img = Image.frombytes('RGBA', (tex['width'], tex['height']), tex['rgba_data'])
            rotated = img.transpose(Image.Transpose.ROTATE_270)  # 270 CCW = 90 CW
            self._save_undo_state("Rotate 90° CW")
            tex['rgba_data'] = rotated.tobytes()
            tex['width'], tex['height'] = rotated.width, rotated.height
            self._update_texture_info(tex)
            self._update_table_display()
            self._mark_as_modified()
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Rotated 90° CW -> {rotated.width}x{rotated.height}")
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to rotate: {str(e)}")

    def _rotate_counterclockwise(self): #vers 2
        """Rotate texture 90° CCW using PIL (fast)."""
        if not self.selected_texture or not self.selected_texture.get('rgba_data'):
            QMessageBox.warning(self, "No Selection", "Please select a texture first")
            return
        try:
            from PIL import Image
            tex = self.selected_texture
            img = Image.frombytes('RGBA', (tex['width'], tex['height']), tex['rgba_data'])
            rotated = img.transpose(Image.Transpose.ROTATE_90)  # 90 CCW
            self._save_undo_state("Rotate 90° CCW")
            tex['rgba_data'] = rotated.tobytes()
            tex['width'], tex['height'] = rotated.width, rotated.height
            self._update_texture_info(tex)
            self._update_table_display()
            self._mark_as_modified()
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Rotated 90° CCW -> {rotated.width}x{rotated.height}")
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to rotate: {str(e)}")

    def _rename_texture_shortcut(self): #vers 1
        """Rename selected texture via F2 shortcut"""
        if not self.selected_texture:
            return

        # Focus the name input field and enable editing
        if hasattr(self, 'info_name'):
            self.info_name.setReadOnly(False)
            self.info_name.selectAll()
            self.info_name.setFocus()

    def _rename_texture(self, alpha=False): #vers 3
        """Rename texture or alpha name and mark as modified"""
        from PyQt6.QtWidgets import QInputDialog

        if not self.selected_texture:
            QMessageBox.warning(self, "No Selection", "Please select a texture first")
            return

        current_name = self.selected_texture.get('name', 'texture')

        if alpha:
            if not self.selected_texture.get('has_alpha', False):
                QMessageBox.information(self, "No Alpha", "This texture does not have an alpha channel")
                return

            alpha_name = self.selected_texture.get('alpha_name', current_name + 'a')
            new_name, ok = QInputDialog.getText(self, "Rename Alpha", "Enter alpha name:", text=alpha_name)
            if ok and new_name and new_name != alpha_name:
                self._save_undo_state("Rename alpha")
                self.selected_texture['alpha_name'] = new_name
                self.info_alpha_name.setText(f"Alpha: {new_name}")
                self._update_table_display()
                self._mark_as_modified()  # Mark as modified
                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"Alpha renamed to: {new_name}")
        else:
            new_name, ok = QInputDialog.getText(self, "Rename Texture", "Enter texture name:", text=current_name)
            if ok and new_name and new_name != current_name:
                self._save_undo_state("Rename texture")
                self.selected_texture['name'] = new_name
                self.info_name.setText(f"Name: {new_name}")
                self._update_table_display()
                self._mark_as_modified()  # Mark as modified
                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"Texture renamed: {current_name} -> {new_name}")

    def open_img_archive(self): #vers 1
        """Open IMG archive and load TXD file list"""
        try:
            file_path, _ = QFileDialog.getOpenFileName(self, "Open IMG Archive", "", "IMG Files (*.img);;All Files (*)")
            if file_path:
                self.load_from_img_archive(file_path)
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to open IMG: {str(e)}")

    def open_txd_file(self, file_path=None): #vers 3
        """Open standalone TXD file with version detection"""
        try:
            if not file_path:
                file_path, _ = QFileDialog.getOpenFileName(
                    self, "Open TXD File", "",
                    "All Texture Files (*.txd *.xtx *.txt *.dat *.toc *.tmb *.chk *.wtd *.ytd);;TXD Files (*.txd);;XTX Textures (*.xtx);;Mobile DB — open .dat or .txt (*.dat *.txt);;Mobile DB sidecar (*.toc *.tmb);;PS2 Splash (*.chk);;All Files (*)"
                )
            if file_path:
                self.current_txd_path = file_path  # Store the full path
                self.current_txd_name = os.path.basename(file_path)


            if file_path:
                # Hard-reject non-TXD extensions before any parsing
                _ext = os.path.splitext(file_path)[1].lower()
                if _ext in ('.toc', '.tmb'):
                    # .toc and .tmb are mobile DB sidecar files — never valid TXDs.
                    # Route to mobile DB loader using the companion .dat file.
                    try:
                        from apps.methods.mobile_texture_db import detect_mobile_db
                        _detected = detect_mobile_db(file_path)
                        if _detected:
                            self._open_mobile_texture_db(file_path)
                        else:
                            from PyQt6.QtWidgets import QMessageBox
                            QMessageBox.warning(
                                self, "Unsupported File",
                                f"{os.path.basename(file_path)} is a mobile texture sidecar "
                                "(.toc/.tmb).\n\nOpen the matching .dat or .txt file instead "
                                "to load the full texture database."
                            )
                    except Exception as _e:
                        print(f"[TXDWorkshop] Mobile DB error: {_e}")
                    return

                # Route mobile texture DB files (.txt / .dat)
                try:
                    from apps.methods.mobile_texture_db import detect_mobile_db
                    if detect_mobile_db(file_path):
                        self._open_mobile_texture_db(file_path)
                        return
                except Exception:
                    pass  # not a mobile DB — fall through to TXD

                # Route .chk files to CHK parser (GTA III PS2 splash)
                if file_path.lower().endswith('.chk'):
                    self._open_chk_file(file_path)
                    return

                # Route .xtx files to XTX reader
                if file_path.lower().endswith('.xtx'):
                    self._open_xtx_file(file_path)
                    return

                #    Undocumented: XTD texture dicts (.wtd/.ytd)               
                if _ext in ('.wtd', '.ytd'):
                    self._open_xtd_file(file_path)
                    return

                # Detect PS2 TXD before full parse
                try:
                    with open(file_path, 'rb') as _f:
                        _hdr = _f.read(64)
                    from apps.methods.txd_ps2_parser import detect_ps2_txd
                    if detect_ps2_txd(_hdr):
                        self._open_ps2_txd(file_path)
                        return
                except Exception:
                    pass  # fall through to regular TXD parser

                with open(file_path, 'rb') as f:
                    txd_data = f.read()

                # Detect version info FIRST
                if not self._detect_txd_info(txd_data):
                    QMessageBox.warning(self, "Invalid TXD",
                        "Could not detect valid TXD format")
                    return

                # Load textures
                self._load_txd_textures(txd_data, os.path.basename(file_path))

                # Update window title with version info
                self.setWindowTitle(
                    f"TXD Workshop: {os.path.basename(file_path)} "
                    f"[{self.txd_version_str}]"
                )

        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to open TXD: {str(e)}")

    def _get_current_rgba(self):  #vers 1
        """Return (rgba, w, h, name) for the selected texture, or (None,0,0,'')."""
        t = getattr(self, 'selected_texture', None)
        if not t:
            return None, 0, 0, ''
        return (t.get('rgba_data', b''), t.get('width', 0),
                t.get('height', 0), t.get('name', 'texture'))

    def _set_current_rgba(self, rgba: bytes):  #vers 1
        """Replace selected texture rgba_data and refresh preview."""
        t = getattr(self, 'selected_texture', None)
        if not t:
            return
        t['rgba_data'] = rgba
        t['modified']  = True
        # Also update the texture_list entry
        row = self.texture_table.currentRow() if hasattr(self, 'texture_table') else -1
        if row >= 0 and hasattr(self, 'texture_list') and row < len(self.texture_list):
            self.texture_list[row]['rgba_data'] = rgba
        # Refresh preview using the existing display pipeline
        try:
            self._update_texture_info(t)
        except Exception:
            try:
                from PyQt6.QtGui import QImage, QPixmap
                qi = QImage(rgba, t['width'], t['height'],
                            t['width'] * 4, QImage.Format.Format_RGBA8888)
                self.preview_widget.setPixmap(QPixmap.fromImage(qi))
            except Exception:
                pass

    def _open_colour_adjust(self): #vers 1
        """Colour adjustments — brightness/contrast/hue/sat/sharp/opacity."""
        from apps.methods.txd_tools import ColourAdjustDialog
        rgba, w, h, name = self._get_current_rgba()
        if not rgba:
            if hasattr(self, 'status_label'): self.status_label.setText("Select a texture first")
            return
        dlg = ColourAdjustDialog(rgba, w, h, name, self)
        dlg.applied.connect(self._set_current_rgba)
        dlg.exec()

    def _open_seamless_tool(self): #vers 1
        """Seamless texture conversion tool."""
        from apps.methods.txd_tools import SeamlessDialog
        rgba, w, h, name = self._get_current_rgba()
        if not rgba:
            if hasattr(self, 'status_label'): self.status_label.setText("Select a texture first")
            return
        dlg = SeamlessDialog(rgba, w, h, name, self)
        dlg.applied.connect(self._set_current_rgba)
        dlg.exec()

    def _open_snow_tool(self): #vers 1
        """Snow effect generator."""
        from apps.methods.txd_tools import SnowDialog
        rgba, w, h, name = self._get_current_rgba()
        if not rgba:
            if hasattr(self, 'status_label'): self.status_label.setText("Select a texture first")
            return
        dlg = SnowDialog(rgba, w, h, name, self)
        dlg.applied.connect(self._set_current_rgba)
        dlg.exec()

    def _open_alpha_coverage(self): #vers 1
        """Scale alpha for mipmap coverage (foliage, fences, decals)."""
        from apps.methods.txd_tools import compute_mip0_coverage, scale_alpha_for_coverage
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QLabel, QDoubleSpinBox
        from PyQt6.QtWidgets import QHBoxLayout, QPushButton, QCheckBox

        rgba, w, h, name = self._get_current_rgba()
        if not rgba:
            self._set_status("Select a texture first"); return

        coverage = compute_mip0_coverage(rgba, w, h)

        dlg = QDialog(self)
        dlg.setWindowTitle(f"Alpha Coverage — {name}")
        lo = QVBoxLayout(dlg)

        lo.addWidget(QLabel(
            "Current mip-0 alpha coverage: " + "{:.1%}".format(coverage) +
            "\n\nThis tool scales the alpha channel so that downsampled\n"
            "mip levels preserve the same coverage fraction.\n"
            "Critical for SA foliage, fences, and decals."))

        row = QHBoxLayout()
        row.addWidget(QLabel("Target coverage:"))
        sp = QDoubleSpinBox()
        sp.setRange(0.01, 1.0); sp.setSingleStep(0.01)
        sp.setValue(round(coverage, 2)); sp.setDecimals(2)
        row.addWidget(sp)
        lo.addLayout(row)

        use_mip = QCheckBox("Apply to all mip levels (recommended)")
        use_mip.setChecked(True)
        lo.addWidget(use_mip)

        btns = QHBoxLayout()
        ok = QPushButton("Apply"); ok.setDefault(True)
        cancel = QPushButton("Cancel")
        ok.clicked.connect(dlg.accept); cancel.clicked.connect(dlg.reject)
        btns.addWidget(ok); btns.addWidget(cancel)
        lo.addLayout(btns)

        if dlg.exec() == QDialog.DialogCode.Accepted:
            target = sp.value()
            new_rgba = scale_alpha_for_coverage(rgba, w, h, target)
            self._set_current_rgba(new_rgba)
            new_cov = compute_mip0_coverage(new_rgba, w, h)
            msg = "Alpha coverage adjusted: {:.1%} -> {:.1%} (target {:.1%})".format(
                coverage, new_cov, target)
            if hasattr(self, 'status_label'): self.status_label.setText(msg)

    def _open_xtd_file(self, file_path: str): #vers 2
        """Open a XTD texture dictionary (.wtd GTA IV / .ytd GTA V/RDR2).
        Read-only import source — textures appear in the list for export or
        transfer into a regular TXD session.  Completely unsupported/undocumented.
        """
        try:
            from apps.methods.xtd_textures import open_xtd_dict, get_xtd_game
            from PyQt6.QtWidgets import QProgressDialog
            from PyQt6.QtCore import Qt
            import os

            game = get_xtd_game(file_path)
            name = os.path.basename(file_path)

            prog = QProgressDialog(f"Reading {name}…", None, 0, 0, self)
            prog.setWindowModality(Qt.WindowModality.WindowModal)
            prog.setMinimumDuration(300)
            prog.show()
            from PyQt6.QtWidgets import QApplication
            QApplication.processEvents()

            rd = open_xtd_dict(file_path)
            prog.close()

            if rd.error:
                from PyQt6.QtWidgets import QMessageBox
                QMessageBox.warning(self, "Cannot open",
                    f"{name}\n\n{rd.error}")
                return

            if not rd.textures:
                from PyQt6.QtWidgets import QMessageBox
                QMessageBox.information(self, "No textures",
                    f"No textures found in {name}.")
                return

            # Build synthetic texture list matching our internal format
            txd_list = []
            for rt in rd.textures:
                # Build a minimal texture dict matching what _load_txd_textures produces
                entry = {
                    'name':          rt.name,
                    'width':         rt.width,
                    'height':        rt.height,
                    'format_name':   rt.fmt,
                    'bit_depth':     32,
                    'mipmap_count':  rt.mips,
                    'rgba_data':     rt.rgba if rt.rgba else bytes(rt.width * rt.height * 4),
                    'raw_data':      rt.raw,
                    'has_alpha':     True,
                    'has_bumpmap':   False,
                    'bumpmap_data':  b'',
                    'alpha_name':    '',
                    'raster_format': 0x0500,  # RASTER_DEFAULT
                    'platform_id':   9,        # PC
                    'filter_flags':  0,
                    'is_xtd_import': True,    # marker — read-only
                    'xtd_game':     rd.game,
                    'xtd_fmt':      rt.fmt,
                }
                txd_list.append(entry)

            self.texture_list = txd_list
            self.current_txd_path  = file_path
            self.current_txd_name  = name
            self.txd_version_str   = f"XTD RSC{'7' if rd.game=='IV' else '8'} v{rd.version}"
            self.txd_platform_name = f"GTA {rd.game} PC"
            self.txd_game          = f"GTA {rd.game}"

            if hasattr(self, 'texture_table'):
                self.texture_table.setRowCount(0)
            for tex in txd_list:
                self._add_texture_to_table(tex)
            if hasattr(self, 'texture_table') and self.texture_table.rowCount():
                self.texture_table.selectRow(0)
                self._on_texture_selected()
            self.setWindowTitle(f"TXD Workshop: {name} [GTA {rd.game}]")

            # Status bar hint that this is read-only
            self._set_status(
                f"GTA {rd.game} — {len(txd_list)} textures  |  "
                f"read-only import source  |  "
                f"export or drag into a TXD session to use")

            self._log(f"Opened {name}: {len(txd_list)} textures (GTA {rd.game})")

        except Exception as e:
            import traceback
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.critical(self, "Error", f"Failed to open XTD dict:\n{e}")
            traceback.print_exc()

    def _open_xtx_file(self, file_path: str): #vers 1
        """Open a VCS PS2/PC XTX palettized texture and display it in the workshop."""
        try:
            from apps.methods.xtx_reader import read_xtx, xtx_to_qimage
            info = read_xtx(file_path)
            if info['error']:
                QMessageBox.warning(self, "XTX Error",
                    f"Failed to read XTX file:\n{info['error']}")
                return

            name = os.path.basename(file_path)
            w, h = info['width'], info['height']

            # Build a fake texture list entry compatible with the existing display
            qimg = xtx_to_qimage(file_path)
            if qimg is None:
                QMessageBox.warning(self, "XTX Error", "Could not decode XTX texture")
                return

            from PyQt6.QtGui import QPixmap
            from PyQt6.QtCore import Qt
            pixmap = QPixmap.fromImage(qimg)

            # Display in the texture preview
            self._display_xtx_texture(name, pixmap, info)

            self.current_txd_path = file_path
            self.current_txd_name = name
            self.setWindowTitle(f"TXD Workshop: {name} [XTX — VCS PS2 Palettized {w}x{h}]")
            self._log(f"Opened XTX: {name} ({w}x{h}, 256-colour indexed)")

        except Exception as e:
            QMessageBox.critical(self, "XTX Error", f"Failed to open XTX:\n{str(e)}")

    def _log(self, msg: str):  #vers 1
        """Safe logging — uses print() since TXDWorkshop has no log_message."""
        print(f"[TXDWorkshop] {msg}")

    def _open_chk_file(self, file_path: str): #vers 1
        """Open a GTA III PS2 CHK splash texture file."""
        try:
            from apps.methods.chk_parser import load_chk
            tex = load_chk(file_path)
            if not tex:
                from PyQt6.QtWidgets import QMessageBox
                QMessageBox.warning(self, "CHK Error", f"Failed to parse CHK file:\n{file_path}")
                return

            name = tex['name']
            self.setWindowTitle(f"TXD Workshop: {name}.CHK [{tex['width']}x{tex['height']}]")
            self._set_status(f"Opened CHK: {name}  "
                             f"{tex['width']}x{tex['height']}  8bpp palettised")

            # Display as a single-texture list
            self.texture_list = [tex]
            if hasattr(self, 'texture_table'):
                self.texture_table.setRowCount(0)

            self._add_texture_to_table(tex)
            self.selected_texture = tex

            # Selecting the row drives the normal preview/info path
            if hasattr(self, 'texture_table') and self.texture_table.rowCount():
                self.texture_table.selectRow(0)
                self._on_texture_selected()

        except Exception as e:
            import traceback; traceback.print_exc()
            print(f"[TXDWorkshop] CHK error: {e}")

    def _open_mobile_texture_db(self, file_path: str): #vers 1
        """Open a mobile texture database (.txt+.toc+.dat+.tmb quad-file set).
        Supports SA/VC iOS (PVRTC) and Android (ETC1) mobile texture formats."""
        try:
            from apps.methods.mobile_texture_db import (
                load_mobile_texture_db, describe_mobile_db
            )
            self._log(f"Loading mobile texture DB: {os.path.basename(file_path)}")
            db = load_mobile_texture_db(file_path, load_pixel_data=True)
            if db is None:
                QMessageBox.warning(self, "Mobile DB",
                    "Could not recognise as a mobile texture database.\n"
                    "Expected: name.txt + name.pvr.dat (SA iOS/VC iOS)\n"
                    "       or: name.dxt.dat + name.dxt.toc (SA Android)\n"
                    "       or: name.pvr.dat + name.pvr.toc (VC Android)\n"
                    "Open the .dat or .txt file, not the .toc or .tmb sidecar.")
                return

            if db.errors:
                for err in db.errors:
                    self._log(f"Warning: {err}")

            real_textures = [t for t in db.textures if not t.is_affiliate]
            desc = describe_mobile_db(db)
            self._log(f"Mobile DB: {desc}")

            self.current_txd_path = file_path
            self.current_txd_name = os.path.basename(file_path)
            platform_str = "iOS (PVRTC)" if db.is_ios else "Android (ETC1)"
            self.setWindowTitle(
                f"TXD Workshop: {db.name} [{platform_str} — {len(real_textures)} textures]")

            self._display_mobile_textures(db)

        except Exception as e:
            import traceback; traceback.print_exc()
            QMessageBox.critical(self, "Mobile DB Error",
                f"Failed to load mobile texture database:\n{e}")

    def _display_mobile_textures(self, db): #vers 2
        """Populate texture_table with mobile texture database textures."""
        from apps.methods.mobile_texture_decode import to_pil_image
        from apps.methods.mobile_texture_db import ENCODING_IS_PVRTC

        real_textures = [t for t in db.textures if not t.is_affiliate]

        # Clear existing textures
        self.texture_list = []
        if hasattr(self, 'texture_table'):
            self.texture_table.setRowCount(0)

        for tex in real_textures:
            enc_name = tex.encoding_name
            pvrtc = tex.encoding_type in ENCODING_IS_PVRTC

            # Decode to RGBA
            pil_img = to_pil_image(tex)
            if pil_img:
                rgba = pil_img.tobytes('raw', 'RGBA')
            else:
                rgba = bytes(tex.width * tex.height * 4)

            fmt = enc_name + (" (preview)" if pvrtc else "")
            tex_entry = {
                'name':                tex.name,
                'width':               tex.width,
                'height':              tex.height,
                'depth':               tex.bpp,
                'format':              fmt,
                'has_alpha':           tex.encoding_type in (2, 4, 7, 8),
                'alpha_name':          '',
                'mipmaps':             tex.mip_count,
                'rgba_data':           rgba,
                'raster_format_flags': 0,
                'is_swizzled':         False,
                'platform':            db.platform.upper(),
                'compressed_size':     tex.compressed_size,
                'mobile_tex':          tex,
            }
            self.texture_list.append(tex_entry)
            if hasattr(self, '_add_texture_to_table'):
                self._add_texture_to_table(tex_entry)

        if hasattr(self, 'texture_table') and self.texture_list:
            self.texture_table.selectRow(0)

        self._log(f"Mobile DB: {db.name}.{db.platform} — {len(real_textures)} textures loaded")

    def _open_ps2_txd(self, file_path: str): #vers 2
        """Open a GTA PS2 TXD (all games/regions — device_id 0 or 6).

        Populates self.texture_list and texture_table exactly like a regular
        TXD so that export, undo, and info panel all work correctly.
        """
        try:
            from apps.methods.txd_ps2_parser import parse_ps2_txd, ps2_tex_to_rgba

            with open(file_path, 'rb') as f:
                data = f.read()

            textures = parse_ps2_txd(data)
            if not textures:
                QMessageBox.warning(self, "PS2 TXD",
                    "No textures found in this PS2 TXD file.\n"
                    "The file may be corrupt or use an unsupported variant.")
                return

            name = os.path.basename(file_path)
            self.current_txd_path = file_path
            self.current_txd_name = name

            depth_fmt = {4: "PSMT4", 8: "PSMT8", 16: "PSMCT16", 32: "PSMCT32"}

            # Clear existing texture data
            self.texture_list = []
            if hasattr(self, 'texture_table'):
                self.texture_table.setRowCount(0)

            for tex in textures:
                rgba = ps2_tex_to_rgba(tex)
                if rgba is None:
                    rgba = bytes(tex['width'] * tex['height'] * 4)

                d = tex['depth']
                palette_type = (tex['raster_format_flags'] >> 13) & 0x3
                fmt = depth_fmt.get(d, f"{d}bpp")
                if palette_type in (1, 2):
                    fmt += f"-PAL{'8' if palette_type==1 else '4'}"

                tex_entry = {
                    'name':                 tex['name'],
                    'width':                tex['width'],
                    'height':               tex['height'],
                    'depth':                d,
                    'format':               fmt,
                    'has_alpha':            True,
                    'alpha_name':           tex.get('mask', ''),
                    'mipmaps':              1,
                    'rgba_data':            rgba,
                    'raster_format_flags':  tex['raster_format_flags'],
                    'is_swizzled':          False,   # already unswizzled
                    'platform':             'PS2',
                    'compressed_size':      tex.get('pixels_size', 0),
                }
                self.texture_list.append(tex_entry)
                if hasattr(self, '_add_texture_to_table'):
                    self._add_texture_to_table(tex_entry)

            # Select first texture
            if hasattr(self, 'texture_table') and self.texture_list:
                self.texture_table.selectRow(0)

            dev = textures[0].get('device_id', 0) if textures else 0
            game_hint = 'SA' if dev == 6 else 'LC/VC'
            self.setWindowTitle(
                f"TXD Workshop: {name} [PS2/{game_hint} — {len(textures)} textures]")
            self._log(f"Opened PS2 TXD: {name} — {len(textures)} textures "
                      f"(device_id={dev}, {game_hint})")

        except Exception as e:
            import traceback; traceback.print_exc()
            QMessageBox.critical(self, "PS2 TXD Error",
                f"Failed to open PS2 TXD:\n{e}")

    def _display_xtx_texture(self, name: str, pixmap, info: dict): #vers 1
        """Show an XTX texture in the workshop preview area."""
        try:
            w, h = info['width'], info['height']

            # Clear existing texture list and add XTX entry
            if hasattr(self, 'txd_list_widget'):
                self.txd_list_widget.clear()
                item = QListWidgetItem(
                    f"{name}  [{w}x{h}]  XTX/PSMT8-256col"
                )
                self.txd_list_widget.addItem(item)
                self.txd_list_widget.setCurrentRow(0)

            # Show in preview
            if hasattr(self, 'texture_preview'):
                scaled = pixmap.scaled(
                    self.texture_preview.width(),
                    self.texture_preview.height(),
                    Qt.AspectRatioMode.KeepAspectRatio,
                    Qt.TransformationMode.SmoothTransformation,
                )
                self.texture_preview.setPixmap(scaled)

            # Update info labels if present
            for attr, val in [
                ('texture_name_label',   name),
                ('texture_size_label',   f"{w} x {h} pixels"),
                ('texture_format_label', "PSMT8 (8-bit palette-indexed)"),
                ('texture_depth_label',  "8 bpp -> 256 colours"),
                ('texture_alpha_label',  "Yes (PS2 alpha 0–128 range)"),
            ]:
                lbl = getattr(self, attr, None)
                if lbl and hasattr(lbl, 'setText'):
                    lbl.setText(val)

            # Store for potential export
            self._xtx_pixmap = pixmap
            self._xtx_info   = info

        except Exception as e:
            self._log(f"XTX display error: {e}")

    def _import_textures(self): #vers 8
        """Pick image file(s) and import them as textures."""
        from PyQt6.QtWidgets import QFileDialog
        file_paths, _ = QFileDialog.getOpenFileNames(
            self, "Import Texture(s)", "",
            "Image Files (*.png *.jpg *.jpeg *.bmp *.tga *.dds *.gif *.tiff *.webp);;"
            "All Files (*.*)")
        if file_paths:
            self._import_texture_files(file_paths)

    def _import_texture_files(self, file_paths): #vers 1
        """Import image files as textures.
        - If a texture is selected and one file given: ask to replace or add.
        - Multiple files: always add.
        - Works without a TXD loaded (creates texture list from scratch).
        - Accepts any size/bit depth, resamples to match target if replacing.
        """
        from PyQt6.QtWidgets import QMessageBox
        from PIL import Image

        # Single file + texture selected -> offer replace
        replace_mode = False
        if len(file_paths) == 1 and self.selected_texture:
            sel_name = self.selected_texture.get('name', '')
            sel_w    = self.selected_texture.get('width', 0)
            sel_h    = self.selected_texture.get('height', 0)
            sel_fmt  = self.selected_texture.get('format', 'ARGB8888')
            reply = QMessageBox.question(
                self, "Replace or Add?",
                f"Replace selected texture '{sel_name}' ({sel_w}x{sel_h}, {sel_fmt})\n"
                f"with {os.path.basename(file_paths[0])}?\n\n"
                "Yes = Replace selected texture\n"
                "No  = Add as new texture",
                QMessageBox.StandardButton.Yes |
                QMessageBox.StandardButton.No  |
                QMessageBox.StandardButton.Cancel)
            if reply == QMessageBox.StandardButton.Cancel:
                return
            replace_mode = (reply == QMessageBox.StandardButton.Yes)

        imported = 0
        failed   = 0
        for file_path in file_paths:
            try:
                fname = os.path.basename(file_path)
                img = Image.open(file_path).convert('RGBA')
                has_alpha = img.mode == 'RGBA' and any(p[3] < 255 for p in img.getdata())
                
                # Auto-detect format from pixel content
                fmt = 'ARGB8888' if has_alpha else 'RGB888'

                if replace_mode and self.selected_texture:
                    # Replace: resample to original dimensions if different
                    tw, th = self.selected_texture['width'], self.selected_texture['height']
                    if (img.width, img.height) != (tw, th):
                        img = img.resize((tw, th), Image.Resampling.LANCZOS)
                        if self.main_window and hasattr(self.main_window, 'log_message'):
                            self.main_window.log_message(
                                f"Resampled {fname} from "
                                f"{img.width}x{img.height} -> {tw}x{th}")
                    # Keep original format and name
                    fmt  = self.selected_texture.get('format', fmt)
                    name = self.selected_texture.get('name', os.path.splitext(fname)[0])
                    self._save_undo_state(f"Replace texture: {name}")
                    self.selected_texture['rgba_data'] = img.tobytes()
                    self.selected_texture['has_alpha']  = has_alpha
                    self.selected_texture['format']     = fmt
                    # Keep width/height as original (we resampled)
                    if self.main_window and hasattr(self.main_window, 'log_message'):
                        self.main_window.log_message(
                            f"Replaced '{name}' with {fname} ({tw}x{th}, {fmt})")
                    imported += 1
                else:
                    # Add as new texture
                    name = os.path.splitext(os.path.basename(file_path))[0]
                    # Truncate name if needed
                    if getattr(self, 'name_limit_enabled', False):
                        name = name[:getattr(self, 'max_texture_name_length', 32)]
                    new_tex = {
                        'name':               name,
                        'width':              img.width,
                        'height':             img.height,
                        'rgba_data':          img.tobytes(),
                        'has_alpha':          has_alpha,
                        'format':             fmt,
                        'alpha_name':         name + 'a' if has_alpha else '',
                        'mipmaps':            1,
                        'mipmap_levels':      [],
                        'raster_format_flags':0x2600 if not has_alpha else 0x2500,
                        'depth':              32,
                        'platform_id':        8,
                        'filter_flags':       0x1102,
                    }
                    self.texture_list.append(new_tex)
                    if self.main_window and hasattr(self.main_window, 'log_message'):
                        self.main_window.log_message(
                            f"Added '{name}' ({img.width}x{img.height}, {fmt})")
                    imported += 1
            except Exception as e:
                failed += 1
                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(
                        f"Import failed: {os.path.basename(file_path)} — {e}")

        if imported:
            self._reload_texture_table()
            self._mark_as_modified()
            # Re-select replaced texture or select newly added
            if replace_mode and self.selected_texture:
                self._update_texture_info(self.selected_texture)
                self._update_table_display()
            else:
                # Select the last added
                last_row = len(self.texture_list) - 1
                if last_row >= 0:
                    self.texture_table.selectRow(last_row)
            # Enable save/export now that we have textures
            if hasattr(self, 'save_txd_btn'):
                self.save_txd_btn.setEnabled(True)
            if hasattr(self, 'export_all_btn'):
                self.export_all_btn.setEnabled(True)

        msg = f"Imported {imported} texture(s)"
        if failed:
            msg += f", {failed} failed"
        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message(msg)

    def show_properties(self): #vers 5
        """Show TXD properties or detailed texture information"""
        # If a texture is selected, show texture details
        if self.selected_texture:
            tex = self.selected_texture

            dialog = QDialog(self)
            dialog.setWindowTitle("Texture Properties")
            dialog.setMinimumWidth(500)
            layout = QFormLayout(dialog)

            # Basic texture info
            layout.addRow("Name:", QLabel(tex.get('name', 'Unknown')))
            layout.addRow("Dimensions:", QLabel(f"{tex.get('width', 0)}x{tex.get('height', 0)}"))
            layout.addRow("Format:", QLabel(tex.get('format', 'Unknown')))
            layout.addRow("Has Alpha:", QLabel('Yes' if tex.get('has_alpha', False) else 'No'))

            if tex.get('alpha_name'):
                layout.addRow("Alpha Name:", QLabel(tex.get('alpha_name')))

            # Raw data information
            if tex.get('rgba_data'):
                data_size = len(tex['rgba_data'])
                layout.addRow("", QLabel(""))  # Spacer
                layout.addRow("Raw Data Size:", QLabel(f"{data_size:,} bytes ({data_size/1024:.1f} KB)"))

                pixels = tex.get('width', 0) * tex.get('height', 0)
                if pixels > 0:
                    layout.addRow("Pixel Count:", QLabel(f"{pixels:,}"))
                    layout.addRow("Bytes per Pixel:", QLabel(f"{data_size/pixels:.1f}"))

            # Estimated compressed sizes
            est_dxt1 = (tex.get('width', 0) * tex.get('height', 0)) // 2
            est_dxt5 = tex.get('width', 0) * tex.get('height', 0)
            layout.addRow("", QLabel(""))  # Spacer
            layout.addRow(QLabel("<b>Estimated Compressed Sizes:</b>"))
            layout.addRow("  DXT1:", QLabel(f"{est_dxt1:,} bytes"))
            layout.addRow("  DXT5:", QLabel(f"{est_dxt5:,} bytes"))

            # Close button
            close_btn = QPushButton("Close")
            close_btn.clicked.connect(dialog.accept)
            layout.addRow("", close_btn)

            dialog.exec()
            return

        # Otherwise, show TXD properties
        if not self.current_txd_name:
            QMessageBox.information(self, "No TXD", "No TXD file loaded")
            return

        try:
            dialog = QDialog(self)
            dialog.setWindowTitle("TXD Properties")
            dialog.setMinimumWidth(500)
            layout = QFormLayout(dialog)

            # Basic info
            layout.addRow("TXD Name:", QLabel(self.current_txd_name))
            layout.addRow("Texture Count:", QLabel(str(len(self.texture_list))))

            # Version information
            layout.addRow("", QLabel(""))  # Spacer
            layout.addRow("RenderWare Version:", QLabel(self.txd_version_str))
            layout.addRow("Platform:", QLabel(self.txd_platform_name))
            layout.addRow("Game:", QLabel(self.txd_game))
            layout.addRow("Format:", QLabel(self._get_format_description()))

            # Capabilities
            if self.txd_capabilities:
                layout.addRow("", QLabel(""))  # Spacer
                caps_label = QLabel("<b>Capabilities:</b>")
                layout.addRow(caps_label)
                if self.txd_capabilities.get('mipmaps'):
                    layout.addRow("  Mipmaps:", QLabel("Supported"))
                if self.txd_capabilities.get('bumpmaps'):
                    layout.addRow("  Bumpmaps:", QLabel("Supported"))
                if self.txd_capabilities.get('dxt_compression'):
                    layout.addRow("  DXT Compression:", QLabel("Supported"))
                if self.txd_capabilities.get('palette'):
                    layout.addRow("  Palette:", QLabel("Supported"))
                if self.txd_capabilities.get('swizzled'):
                    layout.addRow("  Swizzled:", QLabel("Yes (Console)"))

            # File size info
            if self.current_txd_data:
                size_kb = len(self.current_txd_data) / 1024
                layout.addRow("", QLabel(""))  # Spacer
                layout.addRow("File Size:", QLabel(f"{size_kb:.2f} KB"))

            # Close button
            close_btn = QPushButton("Close")
            close_btn.clicked.connect(dialog.accept)
            layout.addRow("", close_btn)

            dialog.exec()

        except Exception as e:
            QMessageBox.warning(self, "Error", f"Could not show properties: {str(e)}")

    def _clear_texture_search(self): #vers 1
        """Clear texture search"""
        if hasattr(self, 'search_input'):
            self.search_input.clear()

        # Show all rows
        if hasattr(self, 'texture_table'):
            for row in range(self.texture_table.rowCount()):
                self.texture_table.setRowHidden(row, False)

    def _duplicate_texture(self): #vers 5
        """Duplicate selected texture - FIXED: Only copy alpha if it exists"""
        if not self.selected_texture:
            QMessageBox.warning(self, "No Selection", "Please select a texture to duplicate")
            return

        try:
            has_alpha = self.selected_texture.get('has_alpha', False)

            # Create copy preserving ALL binary data
            new_texture = {
                'name': self.selected_texture.get('name', 'texture') + "_copy",
                'width': self.selected_texture.get('width', 0),
                'height': self.selected_texture.get('height', 0),
                'format': self.selected_texture.get('format', 'Unknown'),
                'depth': self.selected_texture.get('depth', 32),
                'rgba_data': self.selected_texture.get('rgba_data'),
                'has_alpha': has_alpha,  #  Use the actual has_alpha value
                'mipmap_levels': self.selected_texture.get('mipmap_levels', []).copy(),
                'filter_flags': self.selected_texture.get('filter_flags', 0x1102),
                'platform_id': self.selected_texture.get('platform_id', 8),
                'raster_format_flags': self.selected_texture.get('raster_format_flags', 0),
            }

            #  ONLY add alpha_name if texture actually has alpha
            if has_alpha and 'alpha_name' in self.selected_texture:
                alpha_name = self.selected_texture.get('alpha_name', '')
                if alpha_name:
                    new_texture['alpha_name'] = alpha_name + "_copy"

            # CRITICAL: Preserve original binary data
            if 'compressed_data' in self.selected_texture:
                new_texture['compressed_data'] = self.selected_texture['compressed_data']

            if 'original_bgra_data' in self.selected_texture:
                new_texture['original_bgra_data'] = self.selected_texture['original_bgra_data']

            if 'bumpmap_data' in self.selected_texture:
                new_texture['bumpmap_data'] = self.selected_texture['bumpmap_data']

            if 'reflection_map' in self.selected_texture:
                new_texture['reflection_map'] = self.selected_texture['reflection_map']

            if 'fresnel_map' in self.selected_texture:
                new_texture['fresnel_map'] = self.selected_texture['fresnel_map']

            self._save_undo_state("Duplicate texture")
            # Add to texture list
            self.texture_list.append(new_texture)

            # Reload table
            self._reload_texture_table()

            # Mark as modified
            self._mark_as_modified()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                alpha_status = "with alpha" if has_alpha else "no alpha"
                self.main_window.log_message(f"Duplicated: {new_texture['name']} ({alpha_status})")

        except Exception as e:
            QMessageBox.critical(self, "Duplicate Error", f"Failed to duplicate texture: {str(e)}")

    def _copy_texture(self): #vers 2
        """Copy texture to clipboard - FIXED: Preserves binary data"""
        if not self.selected_texture:
            QMessageBox.warning(self, "No Selection", "Please select a texture to copy")
            return

        try:
            # Copy preserving ALL binary data
            self.clipboard_texture = {
                'name': self.selected_texture.get('name', 'texture'),
                'width': self.selected_texture.get('width', 0),
                'height': self.selected_texture.get('height', 0),
                'format': self.selected_texture.get('format', 'Unknown'),
                'depth': self.selected_texture.get('depth', 32),
                'rgba_data': self.selected_texture.get('rgba_data'),
                'has_alpha': self.selected_texture.get('has_alpha', False),
                'alpha_name': self.selected_texture.get('alpha_name', ''),
                'mipmap_levels': self.selected_texture.get('mipmap_levels', []),
                'filter_flags': self.selected_texture.get('filter_flags', 0x1102),
                'platform_id': self.selected_texture.get('platform_id', 8),
                'raster_format_flags': self.selected_texture.get('raster_format_flags', 0),
            }

            # CRITICAL: Preserve original binary data
            if 'compressed_data' in self.selected_texture:
                self.clipboard_texture['compressed_data'] = self.selected_texture['compressed_data']

            if 'original_bgra_data' in self.selected_texture:
                self.clipboard_texture['original_bgra_data'] = self.selected_texture['original_bgra_data']

            if 'bumpmap_data' in self.selected_texture:
                self.clipboard_texture['bumpmap_data'] = self.selected_texture['bumpmap_data']

            if 'reflection_map' in self.selected_texture:
                self.clipboard_texture['reflection_map'] = self.selected_texture['reflection_map']

            if 'fresnel_map' in self.selected_texture:
                self.clipboard_texture['fresnel_map'] = self.selected_texture['fresnel_map']

            self.paste_btn.setEnabled(True)

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Copied: {self.selected_texture.get('name')}")

        except Exception as e:
            QMessageBox.critical(self, "Copy Error", f"Failed to copy texture: {str(e)}")

    def _paste_texture(self): #vers 4
        """Paste copied texture data - FIXED: Preserves binary data"""
        if not hasattr(self, 'clipboard_texture') or not self.clipboard_texture:
            QMessageBox.warning(self, "Nothing to Paste", "Clipboard is empty")
            return

        try:
            # Create new texture entry with ALL clipboard data
            new_texture = self.clipboard_texture.copy()
            new_texture['name'] = new_texture['name'] + "_copy"
            if new_texture.get('alpha_name'):
                new_texture['alpha_name'] = new_texture['alpha_name'] + "_copy"

            # CRITICAL: Explicitly preserve binary data from clipboard
            if 'compressed_data' in self.clipboard_texture:
                new_texture['compressed_data'] = self.clipboard_texture['compressed_data']

            if 'original_bgra_data' in self.clipboard_texture:
                new_texture['original_bgra_data'] = self.clipboard_texture['original_bgra_data']

            if 'bumpmap_data' in self.clipboard_texture:
                new_texture['bumpmap_data'] = self.clipboard_texture['bumpmap_data']

            if 'reflection_map' in self.clipboard_texture:
                new_texture['reflection_map'] = self.clipboard_texture['reflection_map']

            if 'fresnel_map' in self.clipboard_texture:
                new_texture['fresnel_map'] = self.clipboard_texture['fresnel_map']

            self._save_undo_state("Paste texture")
            # Add to texture list
            self.texture_list.append(new_texture)

            # Reload table
            self._reload_texture_table()

            # Mark as modified
            self._mark_as_modified()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Pasted: {new_texture['name']}")

        except Exception as e:
            QMessageBox.critical(self, "Paste Error", f"Failed to paste texture: {str(e)}")

    def _texture_statistics(self): #vers 1
        """Show texture statistics"""
        if not self.texture_list:
            QMessageBox.information(self, "Statistics", "No textures loaded")
            return

        # Calculate statistics
        total_textures = len(self.texture_list)
        total_size = 0
        format_counts = {}
        alpha_count = 0
        size_distribution = {"Small (≤256)": 0, "Medium (512-1024)": 0, "Large (≥2048)": 0}

        for texture in self.texture_list:
            # Size calculation
            if texture.get('rgba_data'):
                total_size += len(texture['rgba_data'])

            # Format count
            fmt = texture.get('format', 'Unknown')
            format_counts[fmt] = format_counts.get(fmt, 0) + 1

            # Alpha count
            if texture.get('has_alpha', False):
                alpha_count += 1

            # Size distribution
            max_dim = max(texture.get('width', 0), texture.get('height', 0))
            if max_dim <= 256:
                size_distribution["Small (≤256)"] += 1
            elif max_dim <= 1024:
                size_distribution["Medium (512-1024)"] += 1
            else:
                size_distribution["Large (≥2048)"] += 1

        # Build statistics text
        stats = f"=== TXD Statistics ===\n"
        stats += f"Total Textures: {total_textures}\n"
        stats += f"Total Data Size: {total_size:,} bytes ({total_size/1024:.1f} KB)\n"
        stats += f"Textures with Alpha: {alpha_count} ({alpha_count/total_textures*100:.1f}%)\n\n"

        stats += "=== Format Distribution ===\n"
        for fmt, count in sorted(format_counts.items()):
            percentage = count / total_textures * 100
            stats += f"{fmt}: {count} ({percentage:.1f}%)\n"

        stats += "\n=== Size Distribution ===\n"
        for size_cat, count in size_distribution.items():
            percentage = count / total_textures * 100
            stats += f"{size_cat}: {count} ({percentage:.1f}%)\n"

        QMessageBox.information(self, "TXD Statistics", stats)

    def _check_txd_vs_dff(self): #vers 3
        """Check TXD texture names against DFF model - ENHANCED"""
        if not self.texture_list:
            QMessageBox.warning(self, "No Textures", "No textures loaded in TXD")
            return

        # Select DFF file
        dff_path, _ = QFileDialog.getOpenFileName(
            self, "Select DFF Model File", "",
            "DFF Files (*.dff);;All Files (*)"
        )

        if not dff_path:
            return

        try:
            # Parse DFF and extract material names
            dff_textures = self._parse_dff_materials(dff_path)

            if not dff_textures:
                QMessageBox.warning(self, "Parse Error",
                    "Could not extract material names from DFF.\n"
                    "File may be corrupted or unsupported format.")
                return

            # Get TXD texture names
            txd_textures = set(tex['name'].lower() for tex in self.texture_list)
            dff_textures_lower = set(name.lower() for name in dff_textures)

            # Find missing textures
            missing_in_txd = dff_textures_lower - txd_textures
            extra_in_txd = txd_textures - dff_textures_lower

            # Build report
            result_text = "=== DFF Texture Check Results ===\n\n"
            result_text += f"DFF File: {os.path.basename(dff_path)}\n"
            result_text += f"TXD Textures: {len(self.texture_list)}\n"
            result_text += f"DFF Materials: {len(dff_textures)}\n\n"

            result_text += "=== Textures in DFF ===\n"
            for tex_name in sorted(dff_textures):
                result_text += f"  • {tex_name}\n"

            if missing_in_txd:
                result_text += f"\nMissing in TXD ({len(missing_in_txd)}):\n"
                for tex_name in sorted(missing_in_txd):
                    result_text += f"  {tex_name}\n"
            else:
                result_text += "\nAll DFF materials found in TXD\n"

            if extra_in_txd:
                result_text += f"\nExtra in TXD ({len(extra_in_txd)}):\n"
                for tex_name in sorted(extra_in_txd):
                    result_text += f"  • {tex_name}\n"

            # Show results
            QMessageBox.information(self, "DFF Check Complete", result_text)

        except Exception as e:
            QMessageBox.critical(self, "Check Error", f"Failed to check DFF:\n\n{str(e)}")

    def _parse_dff_materials(self, dff_path): #vers 2
        """Texture names used by a DFF's materials, in file order, no repeats."""
        from apps.methods.rw_chunks import texture_names
        try:
            with open(dff_path, 'rb') as f:
                names = texture_names(f.read())
        except OSError as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"DFF read error: {e}")
            return []
        return list(dict.fromkeys(n for n in names if n))

    def _build_txd_from_dff(self): #vers 2
        """Build TXD structure from DFF material names with version/platform selection"""
        # Select DFF file
        dff_path, _ = QFileDialog.getOpenFileName(
            self, "Select DFF File", "",
            "DFF Files (*.dff);;All Files (*)"
        )

        if not dff_path:
            return

        try:
            # Parse DFF materials
            materials = self._parse_dff_materials(dff_path)

            if not materials:
                QMessageBox.warning(self, "No Materials",
                    "Could not extract material names from DFF.\n"
                    "File may be corrupted or unsupported.")
                return

            # Show build dialog
            from PyQt6.QtWidgets import QDialog, QVBoxLayout, QLabel, QComboBox, QPushButton, QCheckBox

            dialog = QDialog(self)
            dialog.setWindowTitle("Build TXD from DFF")
            dialog.setMinimumWidth(400)

            layout = QVBoxLayout(dialog)

            # Info
            info_label = QLabel(f"Found {len(materials)} materials in DFF:\n" +
                            "\n".join(f"  • {m}" for m in materials[:10]) +
                            (f"\n  ... and {len(materials)-10} more" if len(materials) > 10 else ""))
            layout.addWidget(info_label)

            # Game selection
            layout.addWidget(QLabel("\nTarget Game:"))
            game_combo = QComboBox()
            game_combo.addItems(["GTA III", "GTA Vice City", "GTA San Andreas"])
            game_combo.setCurrentIndex(2)  # Default to SA
            layout.addWidget(game_combo)

            # Platform selection
            layout.addWidget(QLabel("\nPlatform:"))
            platform_combo = QComboBox()
            platform_combo.addItems(["PC", "PS2", "Xbox"])
            layout.addWidget(platform_combo)

            # Options
            import_textures_cb = QCheckBox("Auto-import texture files from folder")
            import_textures_cb.setChecked(True)
            layout.addWidget(import_textures_cb)

            # Buttons
            from PyQt6.QtWidgets import QHBoxLayout
            btn_layout = QHBoxLayout()

            build_btn = QPushButton("Build TXD")
            build_btn.clicked.connect(dialog.accept)
            btn_layout.addWidget(build_btn)

            cancel_btn = QPushButton("Cancel")
            cancel_btn.clicked.connect(dialog.reject)
            btn_layout.addWidget(cancel_btn)

            layout.addLayout(btn_layout)

            if dialog.exec() != QDialog.DialogCode.Accepted:
                return

            # Get selections
            game = game_combo.currentText()
            platform = platform_combo.currentText()
            auto_import = import_textures_cb.isChecked()

            # Create new TXD with blank textures
            self._create_new_txd()
            self.texture_list.clear()

            # Add blank texture for each material
            for mat_name in materials:
                tex = {
                    'name': mat_name,
                    'width': 256,
                    'height': 256,
                    'depth': 32,
                    'format': 'DXT1',
                    'has_alpha': False,
                    'mipmaps': 1,
                    'rgba_data': self._create_blank_texture(256, 256, False),
                    'mipmap_levels': []
                }
                self.texture_list.append(tex)

            # Update display
            self._reload_texture_table()

            # Auto-import if requested
            if auto_import:
                reply = QMessageBox.question(self, "Import Textures",
                    "Select folder containing texture files?\n\n"
                    "Files should be named to match material names.",
                    QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)

                if reply == QMessageBox.StandardButton.Yes:
                    folder = QFileDialog.getExistingDirectory(self, "Select Texture Folder")
                    if folder:
                        self._batch_import_from_folder(folder)

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(
                    f"Built TXD with {len(materials)} textures "
                    f"({game}, {platform})"
                )

        except Exception as e:
            QMessageBox.critical(self, "Build Error", f"Failed to build TXD:\n\n{str(e)}")

    def _dropped_files(self, event): #vers 1
        """Local .txd/.img/image paths carried by a drag event."""
        md = event.mimeData()
        if not md.hasUrls():
            return []
        return [u.toLocalFile() for u in md.urls()
                if u.isLocalFile() and u.toLocalFile().lower().endswith(_DROP_EXTS)]

    def dragEnterEvent(self, event): #vers 1
        """Accept .txd, .img and image files."""
        if self._dropped_files(event):
            event.acceptProposedAction()
        else:
            event.ignore()

    def dragMoveEvent(self, event): #vers 1
        """Keep accepting while over the workshop."""
        self.dragEnterEvent(event)

    def dropEvent(self, event): #vers 1
        """Images import into the open TXD; .txd/.img open here or in a new tab."""
        import sys
        open_txd_workshop = sys.modules[type(self).__module__].open_txd_workshop  # avoids circular import
        paths = self._dropped_files(event)
        if not paths:
            event.ignore()
            return
        event.acceptProposedAction()
        images = [p for p in paths if not p.lower().endswith(('.txd', '.img'))]
        archives = [p for p in paths if p.lower().endswith(('.txd', '.img'))]
        if images:
            self._import_texture_files(images)
        if not archives:
            return
        tw = getattr(self.main_window, 'main_tab_widget', None)
        if self.texture_list and tw is not None:
            for path in archives:
                open_txd_workshop(self.main_window, path)
            return
        if self.texture_list:
            reply = QMessageBox.question(
                self, "Dropped file",
                f"Replace the open TXD with {os.path.basename(archives[0])}?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
            if reply != QMessageBox.StandardButton.Yes:
                return
        first = archives[0]
        if first.lower().endswith('.img'):
            self.load_from_img_archive(first)
        else:
            self.open_txd_file(first)
        if tw is not None:
            for path in archives[1:]:
                open_txd_workshop(self.main_window, path)

    def _batch_import_from_folder(self, folder): #vers 1
        """Batch import textures from folder matching material names"""
        import os

        imported = 0

        for texture in self.texture_list:
            tex_name = texture['name']

            # Try different extensions
            for ext in ['.png', '.bmp', '.tga', '.jpg', '.jpeg']:
                file_path = os.path.join(folder, tex_name + ext)

                if os.path.exists(file_path):
                    try:
                        # Import texture
                        from PyQt6.QtGui import QImage

                        img = QImage(file_path)
                        if img.isNull():
                            continue

                        # Convert to RGBA
                        img = img.convertToFormat(QImage.Format.Format_RGBA8888)

                        width = img.width()
                        height = img.height()

                        ptr = img.bits()
                        ptr.setsize(img.sizeInBytes())
                        rgba_data = bytes(ptr)

                        # Check for alpha
                        has_alpha = any(rgba_data[i] < 255 for i in range(3, len(rgba_data), 4))

                        # Update texture
                        texture['width'] = width
                        texture['height'] = height
                        texture['rgba_data'] = rgba_data
                        texture['has_alpha'] = has_alpha

                        if has_alpha:
                            texture['format'] = 'DXT5'
                            texture['alpha_name'] = tex_name + 'a'

                        imported += 1
                        break

                    except Exception as e:
                        if self.main_window and hasattr(self.main_window, 'log_message'):
                            self.main_window.log_message(f"Failed to import {file_path}: {str(e)}")

        # Update display
        self._reload_texture_table()

        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message(f"Imported {imported}/{len(self.texture_list)} textures")

    def _add_texture_to_table(self, texture): #vers 3
        """Add texture to table with file size and warning icon"""
        row = self.texture_table.rowCount()
        self.texture_table.insertRow(row)

        # Create thumbnail
        thumb_item = QTableWidgetItem()
        rgba_data = texture.get('rgba_data')
        width = texture.get('width', 0)
        height = texture.get('height', 0)

        if rgba_data and width > 0:
            pixmap = self._create_thumbnail(rgba_data, width, height)
            if pixmap:
                # Composite warning badge onto thumbnail if alpha looks suspicious
                if texture.get('has_alpha', False) and self._quick_alpha_check(texture):
                    pixmap = self._add_warning_badge(pixmap)
                thumb_item.setData(Qt.ItemDataRole.DecorationRole, pixmap)
            else:
                thumb_item.setText("!" if texture.get('has_alpha', False) and self._quick_alpha_check(texture) else "")
        else:
            thumb_item.setText("")

        self.texture_table.setItem(row, 0, thumb_item)

        # Create details with FILE SIZE
        name = texture['name']
        file_size_kb = len(rgba_data) / 1024 if rgba_data else 0
        depth = texture.get('depth', 32)
        fmt = texture.get('format', 'Unknown')
        has_alpha = texture.get('has_alpha', False)

        # NEW FORMAT: texname, 10kb, 16bit, format, alpha
        details = f"{name}, {file_size_kb:.1f}KB, {depth}bit\n"
        details += f"Size: {width}x{height}\n"
        details += f"Format: {fmt}\n"

        if has_alpha:
            alpha_name = texture.get('alpha_name', '')
            details += f"Alpha: {alpha_name}"
        else:
            details += "Alpha: No"

        details_item = QTableWidgetItem(details)

        # Build extended tooltip for hover
        mipmaps = texture.get('mipmaps', 1)
        raster_flags = texture.get('raster_format_flags', 0)
        filter_flags = texture.get('filter_flags', 0)
        has_bumpmap = texture.get('has_bumpmap', False)
        is_swizzled = texture.get('is_swizzled', False)
        palette_fmt = texture.get('palette_entry_format', '')
        alpha_name  = texture.get('alpha_name', '')
        on_disk_kb  = texture.get('compressed_size', 0) / 1024 if texture.get('compressed_size') else 0

        tt_lines = [
            f"<b>{name}</b>",
            f"Dimensions : {width} x {height}  ({depth}-bit)",
            f"Format     : {fmt}" + (f"  [{palette_fmt} palette]" if palette_fmt else ""),
            f"Alpha      : {'Yes — ' + alpha_name if has_alpha and alpha_name else ('Yes' if has_alpha else 'No')}",
            f"Mipmaps    : {mipmaps}",
        ]
        if on_disk_kb:
            tt_lines.append(f"Disk size  : {on_disk_kb:.1f} KB")
        tt_lines.append(f"RGBA size  : {file_size_kb:.1f} KB")
        if has_bumpmap:
            tt_lines.append("Bumpmap    : Present")
        if is_swizzled:
            tt_lines.append("Swizzled   : Yes (PS2)")
        tt_lines.append(f"raster_fmt : 0x{raster_flags:08X}")
        if filter_flags:
            tt_lines.append(f"filter     : 0x{filter_flags:08X}")

        tooltip_html = "<br>".join(tt_lines)
        thumb_item.setToolTip(tooltip_html)
        details_item.setToolTip(tooltip_html)

        self.texture_table.setItem(row, 1, details_item)
        self.texture_table.setRowHeight(row, 100)
        self.texture_table.setColumnWidth(0, 80)

    def _quick_alpha_check(self, texture): #vers 1
        """Quick check if alpha might be same as RGB (for warning icon)"""
        if not texture.get('has_alpha', False):
            return False

        rgba_data = texture.get('rgba_data', b'')
        if not rgba_data or len(rgba_data) < 400:  # Need at least 100 pixels
            return False

        # Sample first 100 pixels
        matches = 0
        samples = min(100, len(rgba_data) // 4)

        for i in range(0, samples * 4, 4):
            r = rgba_data[i]
            g = rgba_data[i + 1]
            b = rgba_data[i + 2]
            a = rgba_data[i + 3]

            luminosity = int(0.299 * r + 0.587 * g + 0.114 * b)

            if abs(luminosity - a) < 10:
                matches += 1

        # If more than 90% match, flag as suspicious
        return (matches / samples) > 0.9

    def _load_settings(self): #vers 2
        """Load settings from config file"""
        import json

        settings_file = get_user_config_dir() / 'txd_workshop_settings.json'

        try:
            if os.path.exists(settings_file):
                with open(settings_file, 'r') as f:
                    settings = json.load(f)
                    self.save_to_source_location = settings.get('save_to_source_location', True)
                    self.last_save_directory = settings.get('last_save_directory', None)
        except Exception as e:
            print(f"Failed to load settings: {e}")

    def _save_settings(self): #vers 3
        """Save settings to config file"""
        import json

        settings_file = get_user_config_dir() / 'txd_workshop_settings.json'

        try:
            settings = {
                'save_to_source_location': self.save_to_source_location,
                'last_save_directory': self.last_save_directory
            }

            settings_file.parent.mkdir(parents=True, exist_ok=True)
            with open(settings_file, 'w') as f:
                json.dump(settings, indent=2, fp=f)
        except Exception as e:
            print(f"Failed to save settings: {e}")
