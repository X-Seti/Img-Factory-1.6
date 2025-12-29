#this belongs in methods/colour_ui_for_loaded_img.py - Version: 3
# X-Seti - August05 2025 - IMG Factory 1.5 - UI Update for Loaded IMG

"""
IMG Factory UI Update for Loaded IMG
Enhanced version with all color themes and settings menu integration
"""

import os
from PyQt6.QtWidgets import (
    QTableWidgetItem, QDialog, QVBoxLayout, QHBoxLayout, 
    QLabel, QPushButton, QGroupBox, QGridLayout, QMessageBox
)
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QColor

##Methods list -
# update_ui_for_loaded_img
# show_file_window_color_selector
# apply_file_window_theme
# get_all_color_themes
# integrate_color_ui_system

def get_all_color_themes():
    """Get all available color themes including new ones"""
    return {
        # Original presets
        "Light Pink": {
            'background': '#FDF2F8', 'alternate_row': '#FCE7F3', 'selected_background': '#EC4899',
            'selected_text': '#FFFFFF', 'header_background': '#F9A8D4', 'header_text': '#831843', 'border_color': '#F472B6'
        },
        "Hot Pink": {
            'background': '#FDF2F8', 'alternate_row': '#F9A8D4', 'selected_background': '#BE185D',
            'selected_text': '#FFFFFF', 'header_background': '#EC4899', 'header_text': '#FFFFFF', 'border_color': '#BE185D'
        },
        "Rose Gold": {
            'background': '#FFF7ED', 'alternate_row': '#FED7AA', 'selected_background': '#EA580C',
            'selected_text': '#FFFFFF', 'header_background': '#FB923C', 'header_text': '#FFFFFF', 'border_color': '#F97316'
        },
        "Lavender": {
            'background': '#FAF5FF', 'alternate_row': '#E9D5FF', 'selected_background': '#7C3AED',
            'selected_text': '#FFFFFF', 'header_background': '#A855F7', 'header_text': '#FFFFFF', 'border_color': '#8B5CF6'
        },
        
        # Light themes
        "Yellow": {
            'background': '#FEFCE8', 'alternate_row': '#FEF08A', 'selected_background': '#CA8A04',
            'selected_text': '#FFFFFF', 'header_background': '#EAB308', 'header_text': '#FFFFFF', 'border_color': '#D97706'
        },
        "Orange": {
            'background': '#FFF7ED', 'alternate_row': '#FFCC9C', 'selected_background': '#EA580C',
            'selected_text': '#FFFFFF', 'header_background': '#F97316', 'header_text': '#FFFFFF', 'border_color': '#EA580C'
        },
        "Red": {
            'background': '#FEF2F2', 'alternate_row': '#FECACA', 'selected_background': '#DC2626',
            'selected_text': '#FFFFFF', 'header_background': '#EF4444', 'header_text': '#FFFFFF', 'border_color': '#DC2626'
        },
        "Purple": {
            'background': '#FAF5FF', 'alternate_row': '#DDD6FE', 'selected_background': '#7C3AED',
            'selected_text': '#FFFFFF', 'header_background': '#8B5CF6', 'header_text': '#FFFFFF', 'border_color': '#7C3AED'
        },
        "Green": {
            'background': '#F0FDF4', 'alternate_row': '#BBF7D0', 'selected_background': '#16A34A',
            'selected_text': '#FFFFFF', 'header_background': '#22C55E', 'header_text': '#FFFFFF', 'border_color': '#16A34A'
        },
        "Blue": {
            'background': '#EFF6FF', 'alternate_row': '#BFDBFE', 'selected_background': '#2563EB',
            'selected_text': '#FFFFFF', 'header_background': '#3B82F6', 'header_text': '#FFFFFF', 'border_color': '#2563EB'
        },
        
        # Dark themes
        "Dark Yellow": {
            'background': '#1C1C14', 'alternate_row': '#2D2D1F', 'selected_background': '#FCD34D',
            'selected_text': '#000000', 'header_background': '#374151', 'header_text': '#FCD34D', 'border_color': '#FCD34D'
        },
        "Dark Orange": {
            'background': '#1C1410', 'alternate_row': '#2D241A', 'selected_background': '#FB923C',
            'selected_text': '#000000', 'header_background': '#374151', 'header_text': '#FB923C', 'border_color': '#FB923C'
        },
        "Dark Red": {
            'background': '#1C1111', 'alternate_row': '#2D1B1B', 'selected_background': '#F87171',
            'selected_text': '#000000', 'header_background': '#374151', 'header_text': '#F87171', 'border_color': '#F87171'
        },
        "Dark Pink": {
            'background': '#1C111A', 'alternate_row': '#2D1B28', 'selected_background': '#F472B6',
            'selected_text': '#000000', 'header_background': '#374151', 'header_text': '#F472B6', 'border_color': '#F472B6'
        },
        "Dark Purple": {
            'background': '#1A1121', 'alternate_row': '#281B35', 'selected_background': '#A78BFA',
            'selected_text': '#000000', 'header_background': '#374151', 'header_text': '#A78BFA', 'border_color': '#A78BFA'
        },
        "Dark Green": {
            'background': '#111C14', 'alternate_row': '#1B2D1F', 'selected_background': '#4ADE80',
            'selected_text': '#000000', 'header_background': '#374151', 'header_text': '#4ADE80', 'border_color': '#4ADE80'
        },
        "Dark Blue": {
            'background': '#111827', 'alternate_row': '#1F2937', 'selected_background': '#60A5FA',
            'selected_text': '#000000', 'header_background': '#374151', 'header_text': '#60A5FA', 'border_color': '#60A5FA'
        },
        
        # Pure dark themes
        "Black & Yellow": {
            'background': '#000000', 'alternate_row': '#1A1A1A', 'selected_background': '#FBBF24',
            'selected_text': '#000000', 'header_background': '#111111', 'header_text': '#FBBF24', 'border_color': '#FBBF24'
        },
        "Black & Orange": {
            'background': '#000000', 'alternate_row': '#1A1A1A', 'selected_background': '#FB923C',
            'selected_text': '#000000', 'header_background': '#111111', 'header_text': '#FB923C', 'border_color': '#FB923C'
        },
        "Black & Red": {
            'background': '#000000', 'alternate_row': '#1A1A1A', 'selected_background': '#F87171',
            'selected_text': '#000000', 'header_background': '#111111', 'header_text': '#F87171', 'border_color': '#F87171'
        },
        "Black & Pink": {
            'background': '#000000', 'alternate_row': '#1A1A1A', 'selected_background': '#F472B6',
            'selected_text': '#000000', 'header_background': '#111111', 'header_text': '#F472B6', 'border_color': '#F472B6'
        },
        "Black & Purple": {
            'background': '#000000', 'alternate_row': '#1A1A1A', 'selected_background': '#A78BFA',
            'selected_text': '#000000', 'header_background': '#111111', 'header_text': '#A78BFA', 'border_color': '#A78BFA'
        },
        "Black & Green": {
            'background': '#000000', 'alternate_row': '#1A1A1A', 'selected_background': '#4ADE80',
            'selected_text': '#000000', 'header_background': '#111111', 'header_text': '#4ADE80', 'border_color': '#4ADE80'
        },
        "Black & Blue": {
            'background': '#000000', 'alternate_row': '#1A1A1A', 'selected_background': '#60A5FA',
            'selected_text': '#000000', 'header_background': '#111111', 'header_text': '#60A5FA', 'border_color': '#60A5FA'
        }
    }


def apply_file_window_theme(main_window, theme_name: str = "Light Pink"):
    """Apply a specific color theme to the file window"""
    try:
        themes = get_all_color_themes()
        
        if theme_name not in themes:
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"⚠️ Theme '{theme_name}' not found, using Light Pink")
            theme_name = "Light Pink"
        
        colors = themes[theme_name]
        
        # Apply theme using color_file_table method if available
        try:
            from apps.methods.colour_file_table import apply_pink_file_table_theme
            apply_pink_file_table_theme(main_window, colors)
            
        except ImportError:
            # Fallback: apply basic styling
            if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
                table = main_window.gui_layout.table
                style = f"""
                    QTableWidget {{
                        background-color: {colors['background']};
                        alternate-background-color: {colors['alternate_row']};
                        border: 2px solid {colors['border_color']};
                        border-radius: 8px;
                        gridline-color: {colors['border_color']};
                        selection-background-color: {colors['selected_background']};
                        selection-color: {colors['selected_text']};
                    }}
                    QHeaderView::section {{
                        background-color: {colors['header_background']};
                        color: {colors['header_text']};
                        border: 2px solid {colors['border_color']};
                        font-weight: bold;
                        padding: 8px;
                    }}
                """
                table.setStyleSheet(style)
                table.setAlternatingRowColors(True)
        
        # Store current theme
        main_window._current_file_window_theme = theme_name
        
        # Save to settings if available
        if hasattr(main_window, 'app_settings') and hasattr(main_window.app_settings, 'current_settings'):
            main_window.app_settings.current_settings['file_window_theme'] = theme_name
            if hasattr(main_window.app_settings, 'save_settings'):
                main_window.app_settings.save_settings()
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"🎨 Applied '{theme_name}' theme to file window")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Error applying theme '{theme_name}': {str(e)}")
        return False


def show_file_window_color_selector(main_window):
    """Show color theme selector dialog for file window"""
    try:
        dialog = QDialog(main_window)
        dialog.setWindowTitle("🎨 File Window Color Theme")
        dialog.setMinimumSize(600, 500)
        dialog.setModal(True)
        
        layout = QVBoxLayout(dialog)
        
        # Title
        title_label = QLabel("Choose a color theme for the file window:")
        title_label.setStyleSheet("font-size: 14px; font-weight: bold; margin: 10px;")
        layout.addWidget(title_label)
        
        # Get current theme
        current_theme = getattr(main_window, '_current_file_window_theme', 'Light Pink')
        
        # Theme selection area
        themes_group = QGroupBox("Available Themes")
        themes_layout = QGridLayout(themes_group)
        
        themes = get_all_color_themes()
        selected_theme = [current_theme]  # Use list for mutable reference
        
        def create_theme_button(theme_name, colors):
            btn = QPushButton(theme_name)
            btn.setMinimumSize(120, 50)
            btn.setMaximumSize(150, 60)
            
            # Style button with theme colors
            btn.setStyleSheet(f"""
                QPushButton {{
                    background: qlineargradient(x1:0, y1:0, x2:0, y2:1,
                        stop:0 {colors['header_background']}, 
                        stop:1 {colors['background']});
                    color: {colors['header_text']};
                    border: 3px solid {colors['border_color']};
                    border-radius: 8px;
                    font-weight: bold;
                    font-size: 10px;
                }}
                QPushButton:hover {{
                    border: 4px solid {colors['selected_background']};
                    background: qlineargradient(x1:0, y1:0, x2:0, y2:1,
                        stop:0 {colors['selected_background']}, 
                        stop:1 {colors['alternate_row']});
                }}
                QPushButton:pressed {{
                    background-color: {colors['selected_background']};
                    color: {colors['selected_text']};
                }}
            """)
            
            # Mark current theme
            if theme_name == current_theme:
                btn.setText(f"✓ {theme_name}")
                btn.setStyleSheet(btn.styleSheet() + f"""
                    QPushButton {{
                        border: 4px solid #FFD700;
                        font-size: 11px;
                    }}
                """)
            
            def select_theme():
                selected_theme[0] = theme_name
                # Update all buttons
                for i in range(themes_layout.count()):
                    widget = themes_layout.itemAt(i).widget()
                    if isinstance(widget, QPushButton):
                        text = widget.text().replace("✓ ", "")
                        if text == theme_name:
                            widget.setText(f"✓ {text}")
                        else:
                            widget.setText(text)
                
                # Apply preview if checkbox is checked
                if preview_checkbox.isChecked():
                    apply_file_window_theme(main_window, theme_name)
            
            btn.clicked.connect(select_theme)
            return btn
        
        # Organize themes by category
        light_themes = ["Light Pink", "Hot Pink", "Rose Gold", "Lavender", "Yellow", "Orange", "Red", "Purple", "Green", "Blue"]
        dark_themes = ["Dark Yellow", "Dark Orange", "Dark Red", "Dark Pink", "Dark Purple", "Dark Green", "Dark Blue"]
        black_themes = ["Black & Yellow", "Black & Orange", "Black & Red", "Black & Pink", "Black & Purple", "Black & Green", "Black & Blue"]
        
        row = 0
        col = 0
        
        # Add category labels and themes
        categories = [
            ("Light Themes", light_themes),
            ("Dark Themes", dark_themes),
            ("Black Themes", black_themes)
        ]
        
        for category_name, theme_list in categories:
            # Category label
            category_label = QLabel(category_name)
            category_label.setStyleSheet("font-weight: bold; font-size: 12px; color: #333; margin-top: 10px;")
            themes_layout.addWidget(category_label, row, 0, 1, 4)
            row += 1
            
            # Theme buttons
            for theme_name in theme_list:
                if theme_name in themes:
                    btn = create_theme_button(theme_name, themes[theme_name])
                    themes_layout.addWidget(btn, row, col)
                    col += 1
                    if col >= 4:  # 4 buttons per row
                        col = 0
                        row += 1
            
            if col > 0:  # Start new row for next category
                col = 0
                row += 1
        
        layout.addWidget(themes_group)
        
        # Preview checkbox
        preview_checkbox = QCheckBox("🔍 Live Preview")
        preview_checkbox.setChecked(True)
        preview_checkbox.setStyleSheet("font-size: 12px; margin: 10px;")
        layout.addWidget(preview_checkbox)
        
        # Buttons
        button_layout = QHBoxLayout()
        
        # Apply button
        apply_btn = QPushButton("✅ Apply Theme")
        apply_btn.setStyleSheet("""
            QPushButton {
                background-color: #22C55E;
                color: white;
                border: none;
                border-radius: 6px;
                padding: 10px 20px;
                font-weight: bold;
                font-size: 12px;
            }
            QPushButton:hover {
                background-color: #16A34A;
            }
        """)
        
        def apply_and_close():
            apply_file_window_theme(main_window, selected_theme[0])
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"🎨 File window theme changed to '{selected_theme[0]}'")
            dialog.accept()
        
        apply_btn.clicked.connect(apply_and_close)
        button_layout.addWidget(apply_btn)
        
        # Reset button
        reset_btn = QPushButton("🔄 Reset to Light Pink")
        reset_btn.setStyleSheet("""
            QPushButton {
                background-color: #F59E0B;
                color: white;
                border: none;
                border-radius: 6px;
                padding: 10px 20px;
                font-weight: bold;
                font-size: 12px;
            }
            QPushButton:hover {
                background-color: #D97706;
            }
        """)
        
        def reset_theme():
            selected_theme[0] = "Light Pink"
            apply_file_window_theme(main_window, "Light Pink")
            dialog.accept()
        
        reset_btn.clicked.connect(reset_theme)
        button_layout.addWidget(reset_btn)
        
        # Cancel button
        cancel_btn = QPushButton("❌ Cancel")
        cancel_btn.setStyleSheet("""
            QPushButton {
                background-color: #EF4444;
                color: white;
                border: none;
                border-radius: 6px;
                padding: 10px 20px;
                font-weight: bold;
                font-size: 12px;
            }
            QPushButton:hover {
                background-color: #DC2626;
            }
        """)
        cancel_btn.clicked.connect(dialog.reject)
        button_layout.addWidget(cancel_btn)
        
        layout.addLayout(button_layout)
        
        # Show dialog
        result = dialog.exec()
        return result == QDialog.DialogCode.Accepted
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Error showing color selector: {str(e)}")
        return False


def update_ui_for_loaded_img(main_window): #vers 7
    """Update UI when IMG file is loaded - ENHANCED COLOR VERSION"""
    try:
        if not hasattr(main_window, 'current_img') or not main_window.current_img:
            if hasattr(main_window, 'log_message'):
                main_window.log_message("⚠️ update_ui_for_loaded_img called but no current_img")
            return False

        # Update window title
        file_name = os.path.basename(main_window.current_img.file_path)
        main_window.setWindowTitle(f"IMG Factory 1.5 - {file_name}")

        # Setup enhanced table with current theme
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            try:
                # Use the enhanced table setup
                from apps.methods.colour_file_table import setup_enhanced_img_table
                setup_enhanced_img_table(main_window.gui_layout.table, main_window.current_img)
                
                # Apply saved theme or default
                saved_theme = "Light Pink"
                if hasattr(main_window, 'app_settings') and hasattr(main_window.app_settings, 'current_settings'):
                    saved_theme = main_window.app_settings.current_settings.get('file_window_theme', 'Light Pink')
                
                apply_file_window_theme(main_window, saved_theme)
                
                if hasattr(main_window, 'log_message'):
                    main_window.log_message(f"🎨 Color table populated with {len(main_window.current_img.entries)} entries ({saved_theme} theme)")
                    
            except ImportError:
                # Fallback to basic table setup
                if hasattr(main_window, 'log_message'):
                    main_window.log_message("⚠️ Enhanced color table not available, using basic table")
                _setup_basic_table(main_window.gui_layout.table, main_window.current_img)
                
        else:
            if hasattr(main_window, 'log_message'):
                main_window.log_message("⚠️ GUI layout or table not available")

        # SAFE progress hiding using unified system
        try:
            from apps.methods.progressbar_functions import hide_progress
            hide_progress(main_window, "IMG loaded")
        except ImportError:
            # Fallback: safe direct access
            if hasattr(main_window, 'gui_layout'):
                if hasattr(main_window.gui_layout, 'hide_progress'):
                    try:
                        main_window.gui_layout.hide_progress()
                    except:
                        pass
                
                if hasattr(main_window.gui_layout, 'progress_bar') and main_window.gui_layout.progress_bar is not None:
                    try:
                        main_window.gui_layout.progress_bar.setVisible(False)
                        main_window.gui_layout.progress_bar.setValue(0)
                    except:
                        pass

        # Force table to be visible
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            main_window.gui_layout.table.setVisible(True)
            main_window.gui_layout.table.show()

        # Update status bar if available
        if hasattr(main_window, 'statusBar'):
            entry_count = len(main_window.current_img.entries) if main_window.current_img.entries else 0
            main_window.statusBar().showMessage(f"Loaded: {file_name} ({entry_count} entries)")

        if hasattr(main_window, 'log_message'):
            main_window.log_message("🎨 IMG UI updated with enhanced color system")
        
        return True

    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Error updating UI for loaded IMG: {str(e)}")
        return False


def _setup_basic_table(table, img_file):
    """Fallback basic table setup without color theme"""
    try:
        # Setup columns with hex column
        table.setColumnCount(7)
        table.setHorizontalHeaderLabels([
            "Name", "Type", "Size", "Offset", "Hex", "RW Version", "Info"
        ])
        
        # Set column widths
        table.setColumnWidth(0, 200)  # Name
        table.setColumnWidth(1, 60)   # Type
        table.setColumnWidth(2, 80)   # Size
        table.setColumnWidth(3, 80)   # Offset
        table.setColumnWidth(4, 100)  # Hex
        table.setColumnWidth(5, 120)  # RW Version
        table.setColumnWidth(6, 120)  # Info
        
        # Clear and populate
        table.setRowCount(0)
        
        for i, entry in enumerate(img_file.entries):
            table.insertRow(i)
            
            # Name
            table.setItem(i, 0, QTableWidgetItem(entry.name))
            
            # Type
            file_ext = entry.name.split('.')[-1].upper() if '.' in entry.name else "Unknown"
            table.setItem(i, 1, QTableWidgetItem(file_ext))
            
            # Size
            size_text = _format_file_size(entry.size)
            table.setItem(i, 2, QTableWidgetItem(size_text))
            
            # Offset
            table.setItem(i, 3, QTableWidgetItem(f"0x{entry.offset:X}"))
            
            # Hex preview
            hex_preview = _get_basic_hex_preview(entry)
            table.setItem(i, 4, QTableWidgetItem(hex_preview))
            
            # RW Version
            rw_version = getattr(entry, 'rw_version_name', 'Unknown')
            if not rw_version or rw_version == 'Unknown':
                rw_version = f"0x{getattr(entry, 'rw_version', 0):X}" if getattr(entry, 'rw_version', 0) > 0 else "N/A"
            table.setItem(i, 5, QTableWidgetItem(rw_version))
            
            # Info
            info = "OK"
            if hasattr(entry, 'compression_type') and entry.compression_type != 0:
                info = "Compressed"
            table.setItem(i, 6, QTableWidgetItem(info))
            
    except Exception as e:
        print(f"Error in basic table setup: {e}")


def _format_file_size(size_bytes: int) -> str:
    """Format file size in human readable format"""
    if size_bytes < 1024:
        return f"{size_bytes} B"
    elif size_bytes < 1024 * 1024:
        return f"{size_bytes / 1024:.1f} KB"
    else:
        return f"{size_bytes / (1024 * 1024):.1f} MB"


def _get_basic_hex_preview(entry) -> str:
    """Basic hex preview fallback"""
    try:
        if hasattr(entry, 'get_data'):
            data = entry.get_data()
            if data and len(data) > 0:
                hex_bytes = data[:8]  # First 8 bytes
                return ' '.join(f'{b:02X}' for b in hex_bytes)
        return "No Data"
    except:
        return "Error"


def integrate_color_ui_system(main_window): #vers 3
    """Integrate the enhanced color UI system into main window"""
    try:
        # Add methods to main window
        main_window._update_ui_for_loaded_img = lambda: update_ui_for_loaded_img(main_window)
        main_window.show_file_window_color_selector = lambda: show_file_window_color_selector(main_window)
        main_window.apply_file_window_theme = lambda theme="Light Pink": apply_file_window_theme(main_window, theme)
        
        # Also integrate enhanced color table system if available
        try:
            from apps.methods.colour_file_table import integrate_pink_file_table
            integrate_pink_file_table(main_window)
        except ImportError:
            if hasattr(main_window, 'log_message'):
                main_window.log_message("⚠️ Enhanced color table system not available")
        
        # Load saved theme
        if hasattr(main_window, 'app_settings') and hasattr(main_window.app_settings, 'current_settings'):
            saved_theme = main_window.app_settings.current_settings.get('file_window_theme', 'Light Pink')
            main_window._current_file_window_theme = saved_theme
        else:
            main_window._current_file_window_theme = 'Light Pink'
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("🎨 Enhanced color UI system integrated")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Error integrating color UI system: {str(e)}")
        return False


# Export functions
__all__ = [
    'update_ui_for_loaded_img',
    'show_file_window_color_selector', 
    'apply_file_window_theme',
    'get_all_color_themes',
    'integrate_color_ui_system'
]
