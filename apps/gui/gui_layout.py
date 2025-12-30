#this belongs in gui/ gui_layout.py - Version: 27
# X-Seti - JULY29 2025 - Img Factory 1.5 - GUI Layout Module

import os
import re
from PyQt6.QtWidgets import (
    QDialog, QWidget, QVBoxLayout, QHBoxLayout, QGridLayout, QSplitter,
    QTableWidget, QTableWidgetItem, QTextEdit, QGroupBox, QLabel,
    QPushButton, QComboBox, QLineEdit, QHeaderView, QAbstractItemView,
    QMenuBar, QStatusBar, QProgressBar, QTabWidget, QCheckBox, QSpinBox,
    QMessageBox, QSizePolicy, QButtonGroup, QListWidget, QListWidgetItem,
    QFormLayout, QScrollArea, QFrame
)
from PyQt6.QtCore import Qt, QTimer, QSize, pyqtSignal, QPoint, QItemSelectionModel
from PyQt6.QtGui import QFont, QAction, QIcon, QShortcut, QKeySequence, QPalette, QTextCursor
from apps.core.gui_search import ASearchDialog, SearchManager

from apps.methods.imgfactory_svg_icons import (
    get_add_icon, get_open_icon, get_refresh_icon, get_close_icon, 
    get_save_icon, get_export_icon, get_import_icon, get_remove_icon,
    get_edit_icon, get_view_icon, get_search_icon, get_settings_icon,
    get_rebuild_icon
)
from apps.locals.localization import tr_button
from typing import Optional, Dict, Any, List, Callable
from dataclasses import dataclass, field
from apps.components.Img_Creator.img_creator import NewIMGDialog, IMGCreationThread
from apps.components.Ide_Editor.ide_editor import open_ide_editor
from apps.gui.gui_backend import GUIBackend, ButtonDisplayMode

#core
from apps.core.impotr import import_files_function
from apps.core.import_via import import_via_function
#from apps.core.import_via import integrate_import_via_functions
from apps.core.remove import remove_selected_function
from apps.core.remove_via import integrate_remove_via_functions
from apps.core.remove_via import remove_via_function as remove_via_entries_function
from apps.core.export import export_selected_function
# export_all_function, integrate_export_functions
from apps.core.export_via import export_via_function
from apps.core.quick_export import quick_export_function
from apps.core.clean import integrate_clean_utilities
from apps.core.rebuild import rebuild_current_img_native
from apps.core.rebuild_all import rebuild_all_open_tabs
#from apps.core.rebuild import rebuild_current_img #old function.
from apps.core.dump import dump_all_function # dump_selected_function, integrate_dump_functions
from apps.core.img_split import split_img, integrate_split_functions
from apps.core.img_merger import merge_img_function
from apps.core.convert import convert_img, convert_img_format
from apps.core.rename import rename_entry
from apps.core.imgcol_replace import replace_selected
from apps.core.extract import extract_textures_function
from apps.core.reload import reload_current_file
from apps.core.create import create_new_img
from apps.core.open import _detect_and_open_file, open_file_dialog, _detect_file_type
from apps.core.close import close_img_file, close_all_img, install_close_functions, setup_close_manager
from apps.methods.colour_ui_for_loaded_img import integrate_color_ui_system
from apps.gui.gui_context import open_col_editor_dialog
#from apps.methods.refresh_table_functions import refresh_table


def edit_txd_file(main_window): #vers 3
    """Edit selected TXD file with TXD Workshop"""
    try:
        entries_table = main_window.gui_layout.table
        selected_items = entries_table.selectedItems()
        if not selected_items:
            main_window.log_message("No TXD file selected")
            return

        row = selected_items[0].row()
        filename_item = entries_table.item(row, 0)
        filename = filename_item.text()

        if not filename.lower().endswith('.txd'):
            main_window.log_message("Selected file is not a TXD file")
            return

        # Open TXD Workshop
        from apps.components.Txd_Editor.txd_workshop import open_txd_workshop  # FIXED PATH

        # Pass current IMG path if available
        img_path = None
        if hasattr(main_window, 'current_img') and main_window.current_img:
            img_path = main_window.current_img.file_path

        workshop = open_txd_workshop(main_window, img_path)

        if workshop:
            main_window.log_message(f"TXD Workshop opened for: {filename}")
        else:
            main_window.log_message(f"Failed to open TXD Workshop")

    except Exception as e:
        main_window.log_message(f"Error opening TXD Workshop: {e}")


def edit_col_file(main_window): #vers 1
    """Edit selected COL file with COL Workshop - matches TXD pattern"""
    try:
        entries_table = main_window.gui_layout.table
        selected_items = entries_table.selectedItems()

        if not selected_items:
            main_window.log_message("No COL file selected")
            return

        row = selected_items[0].row()
        filename = entries_table.item(row, 0).text()

        if not filename.lower().endswith('.col'):
            main_window.log_message("Selected file is not a COL file")
            return

        from apps.components.Col_Editor.col_workshop import open_col_workshop

        img_path = None
        if hasattr(main_window, 'current_img') and main_window.current_img:
            img_path = main_window.current_img.file_path

        workshop = open_col_workshop(main_window, img_path)

        if workshop:
            main_window.log_message(f"COL Workshop opened for: {filename}")
    except Exception as e:
        main_window.log_message(f"Error opening COL Workshop: {e}")


class IMGFactoryGUILayout:
    """Handles the complete GUI layout for IMG Factory 1.5 with theme system"""
    
    def __init__(self, main_window): #vers 2
        """Initialize GUI layout with theme-controlled components"""
        self.main_window = main_window
        self.table = None
        self.log = None
        self.main_splitter = None
        self.img_buttons = []
        self.entry_buttons = []
        self.options_buttons = []

        # Status bar components
        self.status_bar = None
        self.status_label = None
        self.progress_bar = None
        self.img_info_label = None

        # Tab-related components
        self.main_type_tabs = None
        self.tab_widget = None
        self.left_vertical_splitter = None
        self.status_window = None
        self.info_bar = None
        self.tearoff_button = None

        # Initialize backend for button management
        self.backend = GUIBackend(main_window)
        
        # Initialize method_mappings FIRST before buttons
        self.method_mappings = self._create_method_mappings()


    def _create_method_mappings(self): #vers 5
        """Create centralized method mappings for all buttons"""
        method_mappings = {
            # IMG/COL Operations            'edit_txd_file': lambda: edit_txd_file(self.main_window),
            'create_new_img': lambda: create_new_img(self.main_window),
            'open_img_file': lambda: open_file_dialog(self.main_window),
            'reload_table': lambda: reload_current_file(self.main_window),
            'useless_button': lambda: self._safe_log("🎯 useless_button!"),
            'close_img_file': lambda: close_img_file(self.main_window),
            'close_all_img': lambda: close_all_img(self.main_window),
            'rebuild_img': lambda: rebuild_current_img_native(self.main_window),
            #'rebuild_all_img': lambda: integrate_batch_rebuild_functions(self.main_window),
            'rebuild_all_img': lambda: rebuild_all_open_tabs(self.main_window),
            #'save_img_entry': lambda: save_img_entry_function(self.main_window),
            'save_img_entry': lambda: self.main_window.save_img_entry(),
            'merge_img': lambda: merge_img_function(self.main_window),
            'split_img': lambda: split_img(self.main_window),
            'convert_img_format': lambda: convert_img_format(self.main_window),

            # Import methods
            'import_files': lambda: import_files_function(self.main_window),
            'import_files_via': lambda: import_via_function(self.main_window),
            'refresh_table': lambda: refresh_table(self.main_window),

            # Export methods
            'export_selected': lambda: self.main_window.export_selected(),
            'export_selected_via': lambda: self.main_window.export_via(),
            'quick_export_selected': lambda: self.main_window.quick_export(),            'edit_txd_file': lambda: edit_txd_file(self.main_window),
            'dump_entries': lambda: self.main_window.dump_all(),

            # Remove methods
            'remove_selected': lambda: remove_selected_function(self.main_window),
            'remove_via_entries': lambda: remove_via_entries_function(self.main_window),

            # Selection methods
            'select_all_entries': lambda: self.select_all_entries(),
            'select_inverse': lambda: self.select_inverse(),
            'show_search_dialog': lambda: self.show_search_dialog(),
            'sort_entries': lambda: self.sort_entries(),
            'sort_entries_to_match_ide': lambda: self.sort_entries_to_match_ide(),
            'pin_selected_entries': lambda: self.pin_selected_entries(),

            # Edit methods
            'rename_selected': lambda: rename_entry(self.main_window),
            'replace_selected': lambda: replace_selected(self.main_window),
            'extract_textures': lambda: extract_textures_function(self.main_window),

            # Editor methods
            'edit_col_file': lambda: edit_col_file(self.main_window),
            'edit_txd_file': lambda: edit_txd_file(self.main_window),
            'edit_dff_file': lambda: self._log_missing_method('edit_dff_file'),
            'edit_ipf_file': lambda: self._log_missing_method('edit_ipf_file'),
            'edit_ide_file': lambda: open_ide_editor(self.main_window),
            'edit_ipl_file': lambda: self._log_missing_method('edit_ipl_file'),
            'edit_dat_file': lambda: self._log_missing_method('edit_dat_file'),
            'edit_zones_cull': lambda: self._log_missing_method('edit_zones_cull'),
            'edit_weap_file': lambda: self._log_missing_method('edit_weap_file'),
            'edit_vehi_file': lambda: self._log_missing_method('edit_vehi_file'),
            'edit_peds_file': lambda: self._log_missing_method('edit_peds_file'),
            'edit_radar_map': lambda: self._log_missing_method('edit_radar_map'),
            'edit_paths_map': lambda: self._log_missing_method('edit_paths_map'),
            'edit_waterpro': lambda: self._log_missing_method('edit_waterpro'),
            'edit_weather': lambda: self._log_missing_method('edit_weather'),
            'edit_2dfx': lambda: self._log_missing_method('edit_2dfx'),
            'edit_objects': lambda: self._log_missing_method('edit_objects'),
            'editscm': lambda: self._log_missing_method('editscm'),
            'editgxt': lambda: self._log_missing_method('editgxt'),
            'editmenu': lambda: self._log_missing_method('editmenu'),
        }

        print(f"Method mappings created: {len(method_mappings)} methods")
        return method_mappings


    def _log_missing_method(self, method_name): #vers 1
        """Log missing method - unified placeholder"""
        if hasattr(self.main_window, 'log_message') and hasattr(self.main_window, 'gui_layout'):
            self.main_window.log_message(f"Method '{method_name}' not yet implemented")
        else:
            print(f"Method '{method_name}' not yet implemented")



    def _get_button_theme_template(self, theme_name="default"): #vers 2
        """Get button color templates based on theme"""
        if self._is_dark_theme():
            return {
                # Dark Theme Button Colors
                'create_action': '#3D5A5A',     # Dark teal for create/new actions
                'open_action': '#3D4A5F',       # Dark blue for open/load actions
                'reload_action': '#2D4A3A',     # Dark green for refresh/reload
                'close_action': '#5A4A3D',      # Dark orange for close actions
                'build_action': '#2D4A3A',      # Dark mint for build/rebuild
                'save_action': '#4A2D4A',       # Dark purple for save actions
                'merge_action': '#3A2D4A',      # Dark violet for merge/split
                'convert_action': '#4A4A2D',    # Dark yellow for convert
                'import_action': '#2D4A4F',     # Dark cyan for import
                'export_action': '#2D4A3A',     # Dark emerald for export
                'remove_action': '#4A2D2D',     # Dark red for remove/delete
                'edit_action': '#4A3A2D',       # Dark amber for edit actions
                'select_action': '#3A4A2D',     # Dark lime for select actions
                'editor_col': '#2D3A4F',        # Dark blue for COL editor
                'editor_txd': '#4A2D4A',        # Dark magenta for TXD editor
                'editor_dff': '#2D4A4F',        # Dark cyan for DFF editor
                'editor_data': '#3A4A2D',       # Dark olive for data editors
                'editor_map': '#4A2D4A',        # Dark purple for map editors
                'editor_vehicle': '#2D4A3A',    # Dark teal for vehicle editors
                'editor_script': '#4A3A2D',     # Dark gold for script editors
                'placeholder': '#2A2A2A',       # Dark gray for spacers
            }
        else:
            return {
                # Light Theme Button Colors
                'create_action': '#EEFAFA',     # Light teal for create/new actions
                'open_action': '#E3F2FD',       # Light blue for open/load actions
                'reload_action': '#E8F5E8',     # Light green for refresh/reload
                'close_action': '#FFF3E0',      # Light orange for close actions
                'build_action': '#E8F5E8',      # Light mint for build/rebuild
                'save_action': '#F8BBD9',       # Light pink for save actions
                'merge_action': '#F3E5F5',      # Light violet for merge/split
                'convert_action': '#FFF8E1',    # Light yellow for convert
                'import_action': '#E1F5FE',     # Light cyan for import
                'export_action': '#E8F5E8',     # Light emerald for export
                'remove_action': '#FFEBEE',     # Light red for remove/delete
                'edit_action': '#FFF8E1',       # Light amber for edit actions
                'select_action': '#F1F8E9',     # Light lime for select actions
                'editor_col': '#E3F2FD',        # Light blue for COL editor
                'editor_txd': '#F8BBD9',        # Light pink for TXD editor
                'editor_dff': '#E1F5FE',        # Light cyan for DFF editor
                'editor_data': '#D3F2AD',       # Light lime for data editors
                'editor_map': '#F8BBD9',        # Light pink for map editors
                'editor_vehicle': '#E3F2BD',    # Light olive for vehicle editors
                'editor_script': '#FFD0BD',     # Light peach for script editors
                'placeholder': '#FEFEFE',       # Light gray for spacers
            }


    def _get_img_buttons_data(self): #vers 3
        """Get IMG buttons data with theme colors"""
        colors = self._get_button_theme_template()
        return [
            ("Create", "new", "document-new", colors['create_action'], "create_new_img"),
            ("Open", "open", "document-open", colors['open_action'], "open_img_file"),
            ("Reload", "reload", "document-reload", colors['reload_action'], "reload_table"),
            ("     ", "space", "placeholder", colors['placeholder'], "useless_button"),
            ("Close", "close", "window-close", colors['close_action'], "close_img_file"),
            ("Close All", "close_all", "edit-clear", colors['close_action'], "close_all_img"),
            ("Rebuild", "rebuild", "view-rebuild", colors['build_action'], "rebuild_img"),
            ("Rebuild All", "rebuild_all", "document-save", colors['build_action'], "rebuild_all_img"),
            ("Save Entry", "save_entry", "document-save-entry", colors['save_action'], "save_img_entry"),
            ("Merge", "merge", "document-merge", colors['merge_action'], "merge_img"),
            ("Split via", "split", "edit-cut", colors['merge_action'], "split_img"),
            ("Convert", "convert", "transform", colors['convert_action'], "convert_img_format"),
        ]


    def _get_entry_buttons_data(self): #vers 3
        """Get Entry buttons data with theme colors"""
        colors = self._get_button_theme_template()
        return [
            ("Import", "import", "document-import", colors['import_action'], "import_files"),
            ("Import via", "import_via", "document-import", colors['import_action'], "import_files_via"),
            ("Refresh", "update", "view-refresh", colors['reload_action'], "refresh_table"),
            ("Export", "export", "document-export", colors['export_action'], "export_selected"),
            ("Export via", "export_via", "document-export", colors['export_action'], "export_selected_via"),
            ("Dump", "dump", "document-dump", colors['merge_action'], "dump_entries"),
            #("Quick Exp", "quick_export", "document-send", colors['export_action'], "quick_export_selected"),
            ("Remove", "remove", "edit-delete", colors['remove_action'], "remove_selected"),
            ("Remove via", "remove_via", "document-remvia", colors['remove_action'], "remove_via_entries"),
            ("Extract", "extract", "document-export", colors['export_action'], "extract_textures"),
            ("Rename", "rename", "edit-rename", colors['edit_action'], "rename_selected"),
            ("Select All", "select_all", "edit-select-all", colors['select_action'], "select_all_entries"),
            ("Inverse", "sel_inverse", "edit-select", colors['select_action'], "select_inverse"),
            ("Sort via", "sort", "view-sort", colors['select_action'], "sort_entries"),
            ("Sort IDE", "sort_ide", "view-sort-ide", colors['select_action'], "sort_entries_to_match_ide"),
            ("Pin selected", "pin_selected", "pin", colors['select_action'], "pin_selected_entries"),
        ]


    def _get_options_buttons_data(self): #vers 3
        """Get Options buttons data with theme colors"""
        colors = self._get_button_theme_template()
        return [
            ("Col Edit", "col_edit", "col-edit", colors['editor_col'], "edit_col_file"),
            ("Txd Edit", "txd_edit", "txd-edit", colors['editor_txd'], "edit_txd_file"),
            ("Dff Edit", "dff_edit", "dff-edit", colors['editor_dff'], "edit_dff_file"),
            ("Ipf Edit", "ipf_edit", "ipf-edit", colors['editor_data'], "edit_ipf_file"),
            ("IDE Edit", "ide_edit", "ide-edit", colors['editor_data'], "edit_ide_file"),
            ("IPL Edit", "ipl_edit", "ipl-edit", colors['editor_data'], "edit_ipl_file"),
            ("Dat Edit", "dat_edit", "dat-edit", colors['editor_data'], "edit_dat_file"),
            ("Zons Cull Ed", "zones_cull", "zones-cull", colors['editor_data'], "edit_zones_cull"),
            ("Weap Edit", "weap_edit", "weap-edit", colors['editor_vehicle'], "edit_weap_file"),
            ("Vehi Edit", "vehi_edit", "vehi-edit", colors['editor_vehicle'], "edit_vehi_file"),
            ("Peds Edit", "peds_edit", "peds-edit", colors['editor_vehicle'], "edit_peds_file"),
            ("Radar Map", "radar_map", "radar-map", colors['editor_map'], "edit_radar_map"),
            ("Paths Map", "paths_map", "paths-map", colors['editor_map'], "edit_paths_map"),
            ("Waterpro", "timecyc", "timecyc", colors['editor_data'], "edit_waterpro"),
            ("Weather", "timecyc", "timecyc", colors['editor_data'], "edit_weather"),
            ("Handling", "handling", "handling", colors['editor_vehicle'], "edit_handling"),
            ("Objects", "ojs_breakble", "ojs-breakble", colors['editor_data'], "edit_objects"),
            ("SCM code", "scm_code", "scm-code", colors['editor_script'], "editscm"),
            ("GXT font", "gxt_font", "gxt-font", colors['editor_script'], "editgxt"),
            ("Menu Edit", "menu_font", "menu-font", colors['editor_script'], "editmenu"),
        ]


    def _is_dark_theme(self): #vers 2
        """Detect if the application is using a dark theme"""
        try:
            # Method 1: Check if main window has theme property or setting
            if hasattr(self.main_window, 'current_theme'):
                return 'dark' in self.main_window.current_theme.lower()
            
            # Method 2: Check app_settings for theme
            if hasattr(self.main_window, 'app_settings'):
                current_settings = getattr(self.main_window.app_settings, 'current_settings', {})
                theme_name = current_settings.get('theme', '').lower()
                if theme_name:
                    return 'dark' in theme_name
            
            # Method 3: Check if you have a theme_mode property
            if hasattr(self, 'theme_mode'):
                return self.theme_mode == 'dark'

            # Method 4: Check application palette as fallback
            from PyQt6.QtWidgets import QApplication
            palette = QApplication.palette()
            window_color = palette.color(QPalette.ColorRole.Window)
            # If window background is darker, assume dark theme
            return window_color.lightness() < 128

        except Exception as e:
            # Fallback to light theme if detection fails
            print(f"Theme detection failed: {e}, defaulting to light theme")
            return False


    def set_theme_mode(self, theme_name): #vers 2
        """Set the current theme mode and refresh all styling"""
        self.theme_mode = 'dark' if 'dark' in theme_name.lower() else 'light'
        print(f"Theme mode set to: {self.theme_mode}")
        
        # Force refresh all buttons with new theme colors
        self._refresh_all_buttons()
        
        # Apply all window themes
        self.apply_all_window_themes()


    def _setup_tearoff_button_for_tabs(self): #vers 1
        """Setup tearoff button in tab widget corner"""
        try:
            # Create tearoff button with square arrow icon
            self.tearoff_button = QPushButton("⧉")  # Square with arrow symbol
            self.tearoff_button.setFixedSize(24, 24)
            self.tearoff_button.setToolTip("Tear off tab widget to separate window")

            # Apply theme-aware styling
            self._apply_tearoff_button_theme()

            # Connect to tearoff handler
            self.tearoff_button.clicked.connect(self._handle_tab_widget_tearoff)

            # Set as corner widget on the right side of tabs
            self.tab_widget.setCornerWidget(self.tearoff_button, Qt.Corner.TopRightCorner)

            self.main_window.log_message("Tearoff button added to tab widget corner")

        except Exception as e:
            self.main_window.log_message(f"Error setting up tearoff button: {str(e)}")


    def _apply_tearoff_button_theme(self): #vers 1
        """Apply theme-aware styling to tearoff button"""
        if not self.tearoff_button:
            return

        is_dark = self._is_dark_theme()

        if is_dark:
            # Dark theme tearoff button
            button_style = """
                QPushButton {
                    border: 1px solid {border_color};
                    border-radius: 1px;
                    background-color: {button_bg};
                    color: {text_color};
                    font-size: 12px;
                    font-weight: bold;
                    padding: 0px;
                    margin: 2px;
                }
                QPushButton:hover {
                    background-color: {hover_bg};
                    border: 1px solid {border_color};
                    color: {text_secondary};
                }
                QPushButton:pressed {
                    background-color: {pressed_bg};
                    border: 1px solid {border_color};
                    color: {text_primary};
                }
            """
        else:
            # Light theme tearoff button
            button_style = """
                QPushButton {
                    border: 1px solid {border_color};
                    border-radius: 1px;
                    background-color: {button_bg};
                    color: {text_color};
                    font-size: 12px;
                    font-weight: bold;
                    padding: 0px;
                    margin: 2px;
                }
                QPushButton:hover {
                    background-color: {hover_bg};
                    border: 1px solid {border_color};
                    color: {text_secondary};
                }
                QPushButton:pressed {
                    background-color: {pressed_bg};
                    border: 1px solid {border_color};
                    color: {text_primary};
                }

            """

        self.tearoff_button.setStyleSheet(button_style)


    # FIXED TEAROFF METHODS

    def _handle_tab_widget_tearoff(self): #vers 2
        """Handle tearoff button click for tab widget - FIXED"""
        try:
            if not self.tab_widget:
                return

            # Check if already torn off
            if hasattr(self.tab_widget, 'is_torn_off') and self.tab_widget.is_torn_off:
                # Dock it back
                self._dock_tab_widget_back()
                return

            # Store original parent info BEFORE removing from layout
            original_parent = self.tab_widget.parent()
            original_layout = original_parent.layout() if original_parent else None

            if not original_parent or not original_layout:
                self.main_window.log_message("Cannot tear off: no parent layout found")
                return

            # Store references on tab widget BEFORE manipulation
            self.tab_widget.original_parent = original_parent
            self.tab_widget.original_layout = original_layout

            # Import tearoff system
            try:
                from gui.tear_off import TearOffPanel
            except ImportError:
                self.main_window.log_message("TearOffPanel not available")
                return

            # Create tearoff panel WITHOUT a layout initially
            panel_id = "file_tabs_panel"
            title = "File Tabs"
            tearoff_panel = TearOffPanel(panel_id, title, self.main_window)

            # Create layout for tearoff panel if it doesn't have one
            if not tearoff_panel.layout():
                tearoff_panel_layout = QVBoxLayout(tearoff_panel)
                tearoff_panel_layout.setContentsMargins(2, 2, 2, 2)
            else:
                tearoff_panel_layout = tearoff_panel.layout()

            # Remove tab widget from current parent layout
            original_layout.removeWidget(self.tab_widget)

            # Add tab widget to tearoff panel
            tearoff_panel_layout.addWidget(self.tab_widget)

            # Store tearoff panel reference
            self.tab_widget.tearoff_panel = tearoff_panel
            self.tab_widget.is_torn_off = True

            # Update button appearance
            self._update_tearoff_button_state(True)

            # Show tearoff panel
            tearoff_panel.show()
            tearoff_panel.raise_()

            # Position near cursor
            from PyQt6.QtGui import QCursor
            cursor_pos = QCursor.pos()
            tearoff_panel.move(cursor_pos.x() - 100, cursor_pos.y() - 50)

            self.main_window.log_message("Tab widget torn off to separate window")

        except Exception as e:
            self.main_window.log_message(f"Error handling tab widget tearoff: {str(e)}")
            import traceback
            traceback.print_exc()

    def _dock_tab_widget_back(self): #vers 2
        """Dock torn off tab widget back to main window - FIXED"""
        try:
            # Check if actually torn off
            if not hasattr(self.tab_widget, 'is_torn_off') or not self.tab_widget.is_torn_off:
                self.main_window.log_message("Tab widget is not torn off")
                return

            # Get stored references with safety checks
            original_parent = getattr(self.tab_widget, 'original_parent', None)
            original_layout = getattr(self.tab_widget, 'original_layout', None)
            tearoff_panel = getattr(self.tab_widget, 'tearoff_panel', None)

            # Validate we have the required references
            if not original_parent:
                self.main_window.log_message("Cannot dock back: no original parent stored")
                return

            if not original_layout:
                self.main_window.log_message("Cannot dock back: no original layout stored")
                return

            # Verify original parent still exists and has layout
            try:
                if original_parent.layout() != original_layout:
                    self.main_window.log_message("Original layout changed, using current layout")
                    original_layout = original_parent.layout()
                    if not original_layout:
                        self.main_window.log_message("Original parent no longer has a layout")
                        return
            except:
                self.main_window.log_message("Original parent is no longer valid")
                return

            # Remove from tearoff panel first
            if tearoff_panel:
                try:
                    tearoff_panel_layout = tearoff_panel.layout()
                    if tearoff_panel_layout:
                        tearoff_panel_layout.removeWidget(self.tab_widget)
                    tearoff_panel.hide()
                    tearoff_panel.deleteLater()
                except Exception as e:
                    self.main_window.log_message(f"Error cleaning up tearoff panel: {str(e)}")

            # Add back to original parent layout
            try:
                original_layout.addWidget(self.tab_widget)
            except Exception as e:
                self.main_window.log_message(f"Error adding back to original layout: {str(e)}")
                return

            # Clean up references
            try:
                delattr(self.tab_widget, 'original_parent')
                delattr(self.tab_widget, 'original_layout')
                delattr(self.tab_widget, 'tearoff_panel')
                delattr(self.tab_widget, 'is_torn_off')
            except:
                pass  # Attributes might not exist

            # Update button appearance
            self._update_tearoff_button_state(False)

            # Force widget to show and update
            self.tab_widget.show()
            self.tab_widget.update()

            self.main_window.log_message("Tab widget docked back to main window")

        except Exception as e:
            self.main_window.log_message(f"Error docking tab widget back: {str(e)}")
            import traceback
            traceback.print_exc()


    def _update_tearoff_button_state(self, is_torn_off): #vers 2
        """Update tearoff button appearance based on state - SAFER VERSION"""
        try:
            if not hasattr(self, 'tearoff_button') or not self.tearoff_button:
                return

            if is_torn_off:
                self.tearoff_button.setText("⧈")  # Different icon when torn off
                self.tearoff_button.setToolTip("Dock tab widget back to main window")
            else:
                self.tearoff_button.setText("⧉")  # Original icon when docked
                self.tearoff_button.setToolTip("Tear off tab widget to separate window")

            # Reapply theme styling to ensure consistency
            if hasattr(self, '_apply_tearoff_button_theme'):
                self._apply_tearoff_button_theme()

        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Error updating tearoff button state: {str(e)}")
            else:
                print(f"Error updating tearoff button state: {str(e)}")


    def _refresh_all_buttons(self): #vers 4
        """Refresh all buttons with current theme colors"""
        try:
            # Get new theme colors
            img_colors = self._get_img_buttons_data()
            entry_colors = self._get_entry_buttons_data() 
            options_colors = self._get_options_buttons_data()
            
            # Update IMG buttons
            if hasattr(self, 'img_buttons'):
                for i, btn in enumerate(self.img_buttons):
                    if i < len(img_colors):
                        label, action_type, icon, color, method_name = img_colors[i]
                        self._update_button_theme(btn, color)
            
            # Update Entry buttons
            if hasattr(self, 'entry_buttons'):
                for i, btn in enumerate(self.entry_buttons):
                    if i < len(entry_colors):
                        label, action_type, icon, color, method_name = entry_colors[i]
                        self._update_button_theme(btn, color)
                        
            # Update Options buttons
            if hasattr(self, 'options_buttons'):
                for i, btn in enumerate(self.options_buttons):
                    if i < len(options_colors):
                        label, action_type, icon, color, method_name = options_colors[i]
                        self._update_button_theme(btn, color)
                        
            print(f"Refreshed {len(self.img_buttons + self.entry_buttons + self.options_buttons)} buttons for theme")
                        
        except Exception as e:
            print(f"Error refreshing buttons: {e}")


    def add_txd_editor_button(self): #vers 3
        """Add TXD Editor button to toolbar"""
        if hasattr(self.main_window, 'button_panel'):
            txd_button = QPushButton("TXD Editor")
            txd_button.clicked.connect(self.launch_txd_editor)
            txd_button.setToolTip("Open TXD Texture Editor")
            self.main_window.button_panel.addWidget(txd_button)


    def launch_txd_editor(self): #vers 3
        """Launch TXD Workshop - works with or without IMG loaded"""
        try:
            from apps.components.Txd_Editor.txd_workshop import open_txd_workshop

            # Get current IMG path if available (optional)
            img_path = None
            if hasattr(self.main_window, 'current_img') and self.main_window.current_img:
                img_path = self.main_window.current_img.file_path

            # Open workshop - works without IMG too
            workshop = open_txd_workshop(self.main_window, img_path)

            if workshop:
                if img_path:
                    self.main_window.log_message("TXD Workshop opened with IMG")
                else:
                    self.main_window.log_message("TXD Workshop opened (standalone mode)")
            else:
                self.main_window.log_message("Failed to open TXD Workshop")

        except Exception as e:
            self.main_window.log_message(f"Failed to launch TXD Workshop: {e}")


    def set_button_display_mode(self, mode: str):
        """
        Set button display mode: 'text_only', 'icons_only', or 'icons_with_text'
        """
        try:
            # Store the current mode
            self.button_display_mode = mode
            
            # Update all buttons to reflect the new mode
            self._update_all_buttons_display_mode()
            
            print(f"Button display mode set to: {mode}")
            
        except Exception as e:
            print(f"Error setting button display mode: {e}")


    def _update_all_buttons_display_mode(self):
        """Update all buttons to reflect the current display mode"""
        try:
            # Get all button collections
            all_buttons = []
            if hasattr(self, 'img_buttons'):
                all_buttons.extend(self.img_buttons)
            if hasattr(self, 'entry_buttons'):
                all_buttons.extend(self.entry_buttons)
            if hasattr(self, 'options_buttons'):
                all_buttons.extend(self.options_buttons)
            
            # Update each button
            for btn in all_buttons:
                self._update_button_display_mode(btn)
                
        except Exception as e:
            print(f"Error updating all buttons display mode: {e}")


    def _update_button_display_mode(self, btn):
        """Update a single button to reflect the current display mode"""
        try:
            mode = getattr(self, 'button_display_mode', 'text_only')  # Default to text_only
            
            if mode == 'text_only':
                # Show text only, hide icon
                btn.setText(btn.localized_text if hasattr(btn, 'localized_text') else btn.text())
                btn.setIcon(QIcon())  # Remove icon
                btn.setMinimumWidth(0)  # Reset minimum width
                btn.setMaximumWidth(16777215)  # Maximum width (default)
                
            elif mode == 'icons_only':
                # Show icon only, hide text
                btn.setText("")  # Remove text
                # Keep the icon if it exists
                if hasattr(btn, 'original_text'):
                    btn.setToolTip(btn.original_text)  # Add tooltip with original text
                elif hasattr(btn, 'localized_text'):
                    btn.setToolTip(btn.localized_text)
                else:
                    btn.setToolTip(btn.text())
                btn.setMinimumWidth(64)  # Set fixed width for icon-only mode
                btn.setMaximumWidth(64)
                btn.setMinimumHeight(64)  # Set fixed height for icon-only mode
                btn.setMaximumHeight(64)
                
            elif mode == 'icons_with_text':
                # Show both icon and text
                btn.setText(btn.localized_text if hasattr(btn, 'localized_text') else btn.text())
                # Keep the icon if it exists
                btn.setMinimumWidth(0)  # Reset minimum width
                btn.setMaximumWidth(16777215)  # Maximum width (default)
                btn.setMinimumHeight(20)  # Reset height
                btn.setMaximumHeight(22)
                
            else:
                # Default to text only
                btn.setText(btn.localized_text if hasattr(btn, 'localized_text') else btn.text())
                btn.setIcon(QIcon())
                btn.setMinimumWidth(0)
                btn.setMaximumWidth(16777215)
                
        except Exception as e:
            print(f"Error updating button display mode: {e}")


    def _update_button_theme(self, btn, bg_color): #vers 2
        """Update a single button's theme styling"""
        try:
            is_dark_theme = self._is_dark_theme()

            if is_dark_theme:
                # Dark theme styling
                button_bg = self._darken_color(bg_color, 0.4)
                border_color = self._lighten_color(bg_color, 1.3)
                text_color = self._lighten_color(bg_color, 1.5)
                hover_bg = self._darken_color(bg_color, 0.3)
                hover_border = self._lighten_color(bg_color, 1.4)
                pressed_bg = self._darken_color(bg_color, 0.5)
            else:
                # Light theme styling
                button_bg = bg_color
                border_color = self._darken_color(bg_color, 0.6)
                text_color = self._darken_color(bg_color, 1.8)
                hover_bg = self._darken_color(bg_color, 0.9)
                hover_border = self._darken_color(bg_color, 0.5)
                pressed_bg = self._darken_color(bg_color, 0.8)


            # Apply updated styling
            btn.setStyleSheet(f"""
                QPushButton {{
                    background-color: {button_bg};
                    border: 1px solid {border_color};
                    border-radius: 3px;
                    padding: 2px 6px;
                    font-size: 8pt;
                    font-weight: bold;
                    color: {text_color};
                }}
                QPushButton:hover {{
                    background-color: {hover_bg};
                    border: 1px solid {hover_border};
                }}
                QPushButton:pressed {{
                    background-color: {pressed_bg};
                }}
            """)
        except Exception as e:
            print(f"Error updating button theme: {e}")


    def create_pastel_button(self, label, action_type, icon, bg_color, method_name, use_pastel=True, high_contrast=False): #vers 3
        """Create a button with pastel coloring that adapts to light/dark themes"""
        # Get localized label
        localized_label = tr_button(label)
        
        # Create button with the [%][text] format - showing both icon and text by default
        btn = QPushButton(localized_label)
        btn.setMaximumHeight(24)  # Slightly taller to accommodate both icon and text
        btn.setMinimumHeight(22)

        # Detect if we're using a dark theme
        is_dark_theme = self._is_dark_theme()

        # Determine if we should use high contrast based on settings
        if hasattr(self.main_window, 'app_settings'):
            use_pastel = self.main_window.app_settings.current_settings.get('use_pastel_buttons', True)
            high_contrast = self.main_window.app_settings.current_settings.get('high_contrast_buttons', False) and not use_pastel

        if high_contrast:
            # High contrast theme - use more distinct colors
            if is_dark_theme:
                button_bg = "#333333"  # Dark gray
                border_color = "#ffffff"  # White border
                text_color = "#ffffff"    # White text
                hover_bg = "#555555"      # Lighter gray on hover
                hover_border = "#ffffff"  # White border on hover
                pressed_bg = "#111111"    # Darker gray when pressed
            else:
                button_bg = "#ffffff"  # White background
                border_color = "#000000"  # Black border
                text_color = "#000000"    # Black text
                hover_bg = "#e0e0e0"      # Light gray on hover
                hover_border = "#000000"  # Black border on hover
                pressed_bg = "#cccccc"    # Medium gray when pressed
        elif use_pastel:
            # Original pastel theme
            if is_dark_theme:
                # Dark theme: darker pastel background, lighter edges, light text
                button_bg = self._darken_color(bg_color, 0.4)  # Much darker pastel
                border_color = self._lighten_color(bg_color, 1.3)  # Light border
                text_color = self._lighten_color(bg_color, 1.5)   # Light text
                hover_bg = self._darken_color(bg_color, 0.3)      # Slightly lighter on hover
                hover_border = self._lighten_color(bg_color, 1.4)  # Even lighter border on hover
                pressed_bg = self._darken_color(bg_color, 0.5)    # Darker when pressed
            else:
                # Light theme: light pastel background, dark edges, dark text
                button_bg = bg_color  # Original pastel color
                border_color = self._darken_color(bg_color, 0.6)  # Dark border
                text_color = self._darken_color(bg_color, 1.8)    # Dark text
                hover_bg = self._darken_color(bg_color, 0.9)      # Slightly darker on hover
                hover_border = self._darken_color(bg_color, 0.5)  # Darker border on hover
                pressed_bg = self._darken_color(bg_color, 0.8)    # Darker when pressed
        else:
            # Standard theme without pastel effect
            if is_dark_theme:
                button_bg = "#2d2d2d"  # Dark gray
                border_color = "#555555"  # Medium gray border
                text_color = "#ffffff"    # White text
                hover_bg = "#3d3d3d"      # Lighter gray on hover
                hover_border = "#666666"  # Lighter border on hover
                pressed_bg = "#1d1d1d"    # Darker gray when pressed
            else:
                button_bg = "#f0f0f0"  # Light gray
                border_color = "#a0a0a0"  # Medium gray border
                text_color = "#000000"    # Black text
                hover_bg = "#e0e0e0"      # Lighter gray on hover
                hover_border = "#909090"  # Darker border on hover
                pressed_bg = "#d0d0d0"    # Medium gray when pressed

        # Set icon based on the icon identifier
        # Detect if we're using a dark theme to potentially adjust icon colors
        is_dark_theme = self._is_dark_theme()
        icon_obj = self._get_svg_icon(icon, is_dark_theme)
        if icon_obj:
            btn.setIcon(icon_obj)
            # Get icon size from settings if available
            icon_size = 16
            if hasattr(self.main_window, 'app_settings'):
                icon_size = self.main_window.app_settings.current_settings.get('icon_size', 16)
            btn.setIconSize(QSize(icon_size, icon_size))

        # Apply theme-aware styling
        btn.setStyleSheet(f"""
            QPushButton {{
                background-color: {button_bg};
                border: 1px solid {border_color};
                border-radius: 3px;
                padding: 3px 8px;
                font-size: 8pt;
                font-weight: bold;
                color: {text_color};
            }}
            QPushButton:hover {{
                background-color: {hover_bg};
                border: 1px solid {hover_border};
            }}
            QPushButton:pressed {{
                background-color: {pressed_bg};
            }}
        """)

        # Set action type property
        btn.setProperty("action-type", action_type)

        # Store original and localized labels for later use
        btn.original_text = label
        btn.localized_text = localized_label
        btn.full_text = localized_label
        btn.short_text = self._get_short_text(localized_label)
        btn.icon_name = icon

        # Connect to method_mappings
        try:
            if method_name in self.method_mappings:
                btn.clicked.connect(self.method_mappings[method_name])
                if hasattr(self.main_window, 'gui_layout'):
                    print(f"Connected '{label}' to method_mappings[{method_name}]")
            else:
                btn.clicked.connect(lambda: self._safe_log(f"Method '{method_name}' not in method_mappings"))
                if hasattr(self.main_window, 'gui_layout'):
                    print(f"Method '{method_name}' not found in method_mappings for '{label}'")
        except Exception as e:
            if hasattr(self.main_window, 'gui_layout'):
                print(f"Error connecting button '{label}': {e}")
            btn.clicked.connect(lambda: self._safe_log(f"Button '{label}' connection error"))

        return btn


    def _lighten_color(self, color, factor): #vers 2
        """Lighten a hex color by factor (>1.0 lightens, <1.0 darkens)"""
        try:
            if not color.startswith('#'):
                return color
            
            color = color.lstrip('#')
            r, g, b = tuple(int(color[i:i+2], 16) for i in (0, 2, 4))
            
            # Lighten by moving towards white
            r = min(255, int(r + (255 - r) * (factor - 1.0)))
            g = min(255, int(g + (255 - g) * (factor - 1.0)))
            b = min(255, int(b + (255 - b) * (factor - 1.0)))
            
            return f"#{r:02x}{g:02x}{b:02x}"
        except:
            return color


    def _darken_color(self, color, factor): #vers 2
        """Darken a hex color by factor (0.0-1.0, where 0.8 = 20% darker)"""
        try:
            if not color.startswith('#'):
                return color
                
            color = color.lstrip('#')
            r, g, b = tuple(int(color[i:i+2], 16) for i in (0, 2, 4))
            
            # Darken by multiplying by factor
            r = max(0, int(r * factor))
            g = max(0, int(g * factor))
            b = max(0, int(b * factor))
            
            return f"#{r:02x}{g:02x}{b:02x}"
        except:
            return color


    def _get_svg_icon(self, icon_name: str, is_dark_theme: bool = False) -> QIcon:
        """Get SVG icon based on icon name identifier"""
        icon_map = {
            # Create/new icons
            "new": get_add_icon(),
            "document-new": get_add_icon(),
            
            # Open icons
            "open": get_open_icon(),
            "document-open": get_open_icon(),
            
            # Reload/refresh icons
            "reload": get_refresh_icon(),
            "document-reload": get_refresh_icon(),
            "view-refresh": get_refresh_icon(),
            "update": get_refresh_icon(),
            "rebuild": get_rebuild_icon(),
            "view-rebuild": get_rebuild_icon(),
            
            # Close icons
            "close": get_close_icon(),
            "window-close": get_close_icon(),
            "edit-clear": get_close_icon(),
            
            # Save icons
            "save_entry": get_save_icon(),
            "document-save": get_save_icon(),
            "document-save-entry": get_save_icon(),
            
            # Import icons
            "import": get_import_icon(),
            "document-import": get_import_icon(),
            "import_via": get_import_icon(),
            
            # Export icons
            "export": get_export_icon(),
            "document-export": get_export_icon(),
            "export_via": get_export_icon(),
            "document-send": get_export_icon(),
            
            # Remove/delete icons
            "remove": get_remove_icon(),
            "edit-delete": get_remove_icon(),
            "remove_via": get_remove_icon(),
            "document-remvia": get_remove_icon(),
            
            # Edit icons
            "rename": get_edit_icon(),
            "edit-rename": get_edit_icon(),
            "edit_select": get_edit_icon(),
            "edit-select": get_edit_icon(),
            "select_all": get_edit_icon(),
            "edit-select-all": get_edit_icon(),
            "sel_inverse": get_edit_icon(),
            "sort": get_edit_icon(),
            "view-sort": get_edit_icon(),
            "pin_selected": get_edit_icon(),
            "pin": get_edit_icon(),
            "extract": get_export_icon(),
            
            # Other icons
            "document-merge": get_view_icon(),
            "edit-cut": get_view_icon(),
            "transform": get_view_icon(),
            "document-dump": get_view_icon(),
            
            # Search icons
            "search": get_search_icon(),
            
            # Placeholder (no icon)
            "placeholder": None,
        }
        
        return icon_map.get(icon_name, None)


    def _get_short_text(self, label): #vers 1
        """Get short text for button"""
        # First get the localized version of the label
        localized_label = tr_button(label)
        
        short_map = {
            "Create": "New", "Open": "Open", "Reload": "Reload", "     ": " ",
            "Close": "Close", "Close All": "Close A", "Rebuild": "Rebld",
            "Rebuild All": "Rebld Al", "Save Entry": "Save", "Merge": "Merge",
            "Device": "Dev", "Convert": "Conv", "Import": "Imp",  # Updated for localized "Device"
            "Import via": "Imp via", "Refresh": "Refresh", "Export": "Exp",
            "Export via": "Exp via", "Quick Exp": "Q Exp", "Remove": "Rem",
            "Remove via": "Rem via", "Dump": "Dump", "Pin selected": "Pin",
            "Rename": "Rename", "Extract": "Extract", "Select All": "Select",
            "Inverse": "Inverse", "Sort via": "Sort", "Col Edit": "Col Edit",
            "Txd Edit": "Txd Edit", "Dff Edit": "Dff Edit", "Ipf Edit": "Ipf Edit",
            "IDE Edit": "IDE Edit", "IPL Edit": "IPL Edit", "Dat Edit": "Dat Edit",
            "Zons Cull Ed": "Zons Cull", "Weap Edit": "Weap Edit", "Vehi Edit": "Vehi Edit",
            "Peds Edit": "Peds Edit", "Radar Map": "Radar Map", "Paths Map": "Paths Map",
            "Waterpro": "Waterpro", "Weather": "Weather", "Handling": "Handling",
            "Objects": "Objects", "SCM code": "SCM Code", "GXT font": "GXT Edit",
            "Menu Edit": "Menu Ed",
        }
        return short_map.get(localized_label, localized_label)


    def create_main_ui_with_splitters(self, main_layout): #vers 3
        """Create the main UI with correct 3-section layout"""
        # Create main horizontal splitter
        self.main_splitter = QSplitter(Qt.Orientation.Horizontal)
        
        # Left side - vertical layout with 3 sections
        left_panel = self._create_left_three_section_panel()
        
        # Right side - control buttons with pastel colors
        right_panel = self.create_right_panel_with_pastel_buttons()
        
        # Add panels to splitter
        self.main_splitter.addWidget(left_panel)
        self.main_splitter.addWidget(right_panel)
        
        # Set splitter proportions and force constraints
        self.main_splitter.setSizes([1000, 280])  # Fixed right panel to 280px
        
        # Add size constraints to force the right panel width
        right_panel.setMaximumWidth(280)  # Fixed at 280px
        right_panel.setMinimumWidth(280)  # Fixed at 280px
        
        # Style the main horizontal splitter handle with theme colors
        self._apply_main_splitter_theme()
        
        # Prevent panels from collapsing completely
        self.main_splitter.setCollapsible(0, False)  # Left panel
        self.main_splitter.setCollapsible(1, False)  # Right panel
        
        # Add splitter to main layout
        main_layout.addWidget(self.main_splitter)


    def _create_left_three_section_panel(self): #vers 3
        """Create left panel with 3 sections: File Window, Status Window"""
        left_container = QWidget()
        left_layout = QVBoxLayout(left_container)
        left_layout.setContentsMargins(3, 3, 3, 3)
        left_layout.setSpacing(0)  # No spacing - splitter handles this

        # Create vertical splitter for the sections
        self.left_vertical_splitter = QSplitter(Qt.Orientation.Vertical)

        # 1. MIDDLE: File Window (table with sub-tabs)
        file_window = self._create_file_window()
        self.left_vertical_splitter.addWidget(file_window)

        # 2. BOTTOM: Status Window (log and status)
        status_window = self.create_status_window()
        self.left_vertical_splitter.addWidget(status_window)

        # Set section proportions: File(760px), Status(200px)
        self.left_vertical_splitter.setSizes([760, 200])

        # Prevent sections from collapsing completely
        self.left_vertical_splitter.setCollapsible(0, True)  # File window
        self.left_vertical_splitter.setCollapsible(1, True)  # Status window

        # Apply theme styling to vertical splitter
        self._apply_vertical_splitter_theme()

        left_layout.addWidget(self.left_vertical_splitter)
        return left_container


    def _create_file_window(self): #vers 3
        """Create file window with tabs for different views"""
        file_window = QWidget()
        file_layout = QVBoxLayout(file_window)
        file_layout.setContentsMargins(5, 5, 5, 5)
        file_layout.setSpacing(3)
        
        # Create tab widget
        self.tab_widget = QTabWidget()
        
        # Tab 1: File Entries (main table)
        entries_tab = QWidget()
        entries_layout = QVBoxLayout(entries_tab)
        entries_layout.setContentsMargins(0, 0, 0, 0)
        
        # Create main table
        self.table = QTableWidget()
        self.table.setColumnCount(9)
        self.table.setHorizontalHeaderLabels([
            "Num", "Name", "Extension", "Size", "Hash", "Hex", "Version", "Compression", "Status"
        ])
        
        # Table configuration
        self.table.setAlternatingRowColors(True)
        self.table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        self.table.setSelectionMode(QAbstractItemView.SelectionMode.ExtendedSelection)
        self.table.setSortingEnabled(True)
        self.table.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)

        # Column sizing
        header = self.table.horizontalHeader()
        header.setSectionResizeMode(0, QHeaderView.ResizeMode.ResizeToContents)  # Num
        header.setSectionResizeMode(1, QHeaderView.ResizeMode.ResizeToContents)  # Name
        header.setSectionResizeMode(2, QHeaderView.ResizeMode.ResizeToContents)  # Extension
        header.setSectionResizeMode(3, QHeaderView.ResizeMode.ResizeToContents)  # Size
        header.setSectionResizeMode(4, QHeaderView.ResizeMode.Stretch)  # Hash
        header.setSectionResizeMode(5, QHeaderView.ResizeMode.Stretch)  # Hex Value
        header.setSectionResizeMode(6, QHeaderView.ResizeMode.Stretch)  # Version
        header.setSectionResizeMode(7, QHeaderView.ResizeMode.Stretch)  # Compression
        header.setSectionResizeMode(8, QHeaderView.ResizeMode.Stretch)  # Status
        
        # Apply theme styling to table
        self._apply_table_theme_styling()
        
        entries_layout.addWidget(self.table)
        self.tab_widget.addTab(entries_tab, "File Entries")
        

        # Tab 2: Directory Tree (placeholder for integration)
        tree_tab = QWidget()
        tree_layout = QVBoxLayout(tree_tab)
        tree_layout.setContentsMargins(0, 0, 0, 0)

        # Placeholder content - will be replaced by integration
        placeholder_label = QLabel("🌳 Directory Tree")
        placeholder_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        placeholder_label.setStyleSheet("font-size: 14px; color: #888; font-style: italic;")
        tree_layout.addWidget(placeholder_label)

        info_label = QLabel("Directory tree will appear here after integration.")
        info_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        info_label.setStyleSheet("color: #666; font-size: 12px;")
        tree_layout.addWidget(info_label)

        tree_layout.addStretch()

        # Add the tab to the widget
        self.tab_widget.addTab(tree_tab, "Directory Tree")
        
        # Tab 3: Search Results (future enhancement)
        search_tab = QWidget()
        search_layout = QVBoxLayout(search_tab)
        search_layout.setContentsMargins(0, 0, 0, 0)

        search_placeholder = QLabel("Search results will be displayed here")
        search_placeholder.setAlignment(Qt.AlignmentFlag.AlignCenter)
        search_placeholder.setStyleSheet("font-style: italic;")
        search_layout.addWidget(search_placeholder)

        self.tab_widget.addTab(search_tab, "Search Results")

        # Apply theme styling to file window tabs
        self._apply_file_list_window_theme_styling()

        self._setup_tearoff_button_for_tabs()

        file_layout.addWidget(self.tab_widget)
        return file_window


    def create_right_panel_with_pastel_buttons(self): #vers 3
        """Create right panel with theme-controlled pastel buttons"""
        right_panel = QWidget()
        right_layout = QVBoxLayout(right_panel)
        right_layout.setContentsMargins(4, 4, 4, 4)
        right_layout.setSpacing(6)

        # IMG Section with theme colors
        img_box = QGroupBox("IMG, COL, TXD Files")
        img_layout = QGridLayout()
        img_layout.setSpacing(2)
        
        # Use theme-controlled button data
        img_buttons_data = self._get_img_buttons_data()
        
        for i, (label, action_type, icon, color, method_name) in enumerate(img_buttons_data):
            btn = self.create_pastel_button(label, action_type, icon, color, method_name)
            self.img_buttons.append(btn)
            # Add to backend as well
            if hasattr(self, 'backend'):
                self.backend.img_buttons.append(btn)
            img_layout.addWidget(btn, i // 3, i % 3)
        
        img_box.setLayout(img_layout)
        right_layout.addWidget(img_box)

        # Entries Section with theme colors  
        entries_box = QGroupBox("File Entries")
        entries_layout = QGridLayout()
        entries_layout.setSpacing(2)
        
        # Use theme-controlled button data
        entry_buttons_data = self._get_entry_buttons_data()
        
        for i, (label, action_type, icon, color, method_name) in enumerate(entry_buttons_data):
            btn = self.create_pastel_button(label, action_type, icon, color, method_name)
            self.entry_buttons.append(btn)
            # Add to backend as well
            if hasattr(self, 'backend'):
                self.backend.entry_buttons.append(btn)
            entries_layout.addWidget(btn, i // 3, i % 3)
        
        entries_box.setLayout(entries_layout)
        right_layout.addWidget(entries_box)

        # Options Section with theme colors
        options_box = QGroupBox("Editing Options")
        options_layout = QGridLayout()
        options_layout.setSpacing(2)
        
        # Use theme-controlled button data
        options_buttons_data = self._get_options_buttons_data()
        
        for i, (label, action_type, icon, color, method_name) in enumerate(options_buttons_data):
            btn = self.create_pastel_button(label, action_type, icon, color, method_name)
            self.options_buttons.append(btn)
            # Add to backend as well
            if hasattr(self, 'backend'):
                self.backend.options_buttons.append(btn)
            options_layout.addWidget(btn, i // 3, i % 3)
        
        options_box.setLayout(options_layout)
        right_layout.addWidget(options_box)

        # Search button section - positioned at the bottom right
        search_button_layout = QHBoxLayout()
        search_button_layout.addStretch()  # Add stretch to push button to the right
        search_btn = self.create_pastel_button("Search", "search", "search", self._get_button_theme_template()['select_action'], "show_search_dialog")
        # Add to backend as well
        if hasattr(self, 'backend'):
            self.backend.options_buttons.append(search_btn)  # Add to options buttons for consistency
        search_button_layout.addWidget(search_btn)
        right_layout.addLayout(search_button_layout)

        return right_panel

    def set_button_display_mode(self, mode: str):
        """
        Set button display mode: 'text_only', 'icons_only', or 'icons_with_text'
        """
        try:
            # Store the current mode
            self.button_display_mode = mode
            
            # Update all buttons to reflect the new mode
            self._update_all_buttons_display_mode()
            
            # Also update via backend if available
            if hasattr(self, 'backend'):
                # Convert string mode to enum
                if mode == "text_only":
                    display_mode = ButtonDisplayMode.TEXT_ONLY
                elif mode == "icons_only":
                    display_mode = ButtonDisplayMode.ICONS_ONLY
                elif mode == "icons_with_text":
                    display_mode = ButtonDisplayMode.ICONS_WITH_TEXT
                else:
                    display_mode = ButtonDisplayMode.ICONS_WITH_TEXT  # Default
                
                self.backend.set_button_display_mode(display_mode)
            
            print(f"Button display mode set to: {mode}")
            
        except Exception as e:
            print(f"Error setting button display mode: {e}")

    def update_button_settings(self, settings):
        """Update button settings from app settings"""
        # Update button display mode
        button_mode = settings.get('button_display_mode', 'icons_with_text')
        self.set_button_display_mode(button_mode)
        
        # Update button size if available
        button_size = settings.get('button_size', None)
        if button_size:
            self.set_button_size(button_size)
        
        # Update icon size if available
        icon_size = settings.get('icon_size', 16)
        self.set_icon_size(icon_size)
        
        # Update pastel effect setting
        use_pastel = settings.get('use_pastel_buttons', True)
        self.set_pastel_effect(use_pastel)
        
        # Update high contrast setting
        high_contrast = settings.get('high_contrast_buttons', False)
        self.set_high_contrast(high_contrast)
        
        # Update button format
        button_format = settings.get('button_format', 'both')
        self.set_button_format(button_format)

    def set_button_size(self, size):
        """Set button size for all buttons"""
        if hasattr(self, 'backend'):
            all_buttons = (self.img_buttons + self.entry_buttons + self.options_buttons +
                          self.backend.img_buttons + self.backend.entry_buttons + self.backend.options_buttons)
            for btn in all_buttons:
                btn.setMaximumHeight(size)
                btn.setMinimumHeight(max(20, size - 4))  # Maintain reasonable min height

    def set_icon_size(self, size):
        """Set icon size for all buttons"""
        if hasattr(self, 'backend'):
            all_buttons = (self.img_buttons + self.entry_buttons + self.options_buttons +
                          self.backend.img_buttons + self.backend.entry_buttons + self.backend.options_buttons)
            for btn in all_buttons:
                if btn.icon():
                    btn.setIconSize(QSize(size, size))

    def set_pastel_effect(self, enabled):
        """Enable or disable pastel effect on buttons"""
        # This would modify the button styling based on pastel setting
        # Implementation depends on how pastel vs regular buttons are handled
        pass

    def set_high_contrast(self, enabled):
        """Enable or disable high contrast mode for buttons"""
        # This would modify the button styling for high contrast
        pass

    def set_button_format(self, format_type):
        """Set button format: 'both', 'icon_only', 'text_only', or 'separate'"""
        # Update the button format based on setting
        if format_type == 'separate':
            # This would change how the text is displayed on buttons
            pass
        elif format_type == 'both':
            self.set_button_display_mode('icons_with_text')
        elif format_type == 'icon_only':
            self.set_button_display_mode('icons_only')
        elif format_type == 'text_only':
            self.set_button_display_mode('text_only')


    def create_status_window(self): #vers 5
        """Create status window with log"""
        self.status_window = QWidget()
        status_layout = QVBoxLayout(self.status_window)
        status_layout.setContentsMargins(5, 5, 5, 5)
        status_layout.setSpacing(3)

        # Title
        title_layout = QHBoxLayout()
        title_label = QLabel("Activity Log")
        title_label.setStyleSheet("font-weight: bold; font-size: 10pt;")
        title_layout.addWidget(title_label)

        # Status indicators
        title_layout.addStretch()

        # Status label
        self.status_label = QLabel("Ready")
        title_layout.addWidget(self.status_label)
        status_layout.addLayout(title_layout)

        # Log with scrollbars
        self.log = QTextEdit()
        self.log.setReadOnly(True)
        self.log.setPlaceholderText("Activity log will appear here...")

        # Enable scrollbars for log
        self.log.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        self.log.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)

        # Apply theme styling to log
        self._apply_log_theme_styling()
        status_layout.addWidget(self.log)

        # Apply theme styling to status window
        self._apply_status_window_theme_styling()
        
        return self.status_window


    def _apply_table_theme_styling(self): #vers 5
        """Apply theme styling to the table widget"""
        theme_colors = self._get_theme_colors("default")

        # Use standard theme variables from app_settings_system.py
        panel_bg = theme_colors.get('panel_bg', '#ffffff')
        bg_secondary = theme_colors.get('bg_secondary', '#f8f9fa')
        bg_tertiary = theme_colors.get('bg_tertiary', '#e9ecef')
        border = theme_colors.get('border', '#dee2e6')
        text_primary = theme_colors.get('text_primary', '#000000')
        text_secondary = theme_colors.get('text_secondary', '#495057')
        accent_primary = theme_colors.get('accent_primary', '#1976d2')

        self.table.setStyleSheet(f"""
            QTableWidget {{
                background-color: {bg_secondary};
                alternate-background-color: {bg_tertiary};
                border: 1px solid {border};
                border-radius: 3px;
                gridline-color: {border};
                color: {text_primary};
                font-size: 9pt;
            }}
            QTableWidget::item {{
                padding: 5px;
                border: none;
            }}
            QTableWidget::item:selected {{
                background-color: {accent_primary};
                color: white;
            }}
            QHeaderView::section {{
                background-color: {panel_bg};
                color: {text_secondary};
                padding: 5px;
                border: 1px solid {border};
                font-weight: bold;
                font-size: 9pt;
            }}
        """)


    def _apply_main_splitter_theme(self): #vers 6
        """Apply theme styling to main horizontal splitter"""
        theme_colors = self._get_theme_colors("default")

        # Extract variables FIRST
        bg_secondary = theme_colors.get('bg_secondary', '#f8f9fa')
        bg_primary = theme_colors.get('bg_primary', '#ffffff')
        bg_tertiary = theme_colors.get('bg_tertiary', '#e9ecef')

        self.main_splitter.setStyleSheet(f"""
            QSplitter::handle:horizontal {{
                background-color: {bg_secondary};
                border: 1px solid {bg_primary};
                border-left: 1px solid {bg_tertiary};
                width: 8px;
                margin: 2px 1px;
                border-radius: 3px;
            }}

            QSplitter::handle:horizontal:hover {{
                background-color: {bg_primary};
                border-color: {bg_tertiary};
            }}

            QSplitter::handle:horizontal:pressed {{
                background-color: {bg_tertiary};
            }}
        """)


    def _apply_vertical_splitter_theme(self): #vers 6
        """Apply theme styling to the vertical splitter"""
        theme_colors = self._get_theme_colors("default")

        # Extract variables FIRST
        bg_secondary = theme_colors.get('bg_secondary', '#f8f9fa')
        bg_tertiary = theme_colors.get('bg_tertiary', '#e9ecef')

        self.left_vertical_splitter.setStyleSheet(f"""
            QSplitter::handle:vertical {{
                background-color: {bg_secondary};
                border: 1px solid {bg_tertiary};
                height: 4px;
                margin: 1px 2px;
                border-radius: 2px;
            }}
            QSplitter::handle:vertical:hover {{
                background-color: {bg_tertiary};
            }}
        """)


    def _apply_log_theme_styling(self): #vers 7
        """Apply theme styling to the log widget"""
        theme_colors = self._get_theme_colors("default")

        # Extract variables FIRST
        panel_bg = theme_colors.get('panel_bg', '#f0f0f0')
        text_primary = theme_colors.get('text_primary', '#000000')
        border = theme_colors.get('border', '#dee2e6')

        self.log.setStyleSheet(f"""
            QTextEdit {{
                background-color: {panel_bg};
                color: {text_primary};
                border: 1px solid {border};
                border-radius: 3px;
                padding: 5px;
                font-family: 'Consolas', 'Monaco', monospace;
                font-size: 9pt;
            }}
        """)

    def _apply_status_window_theme_styling(self): #vers 1
        """Apply theme styling to the status window"""
        theme_colors = self._get_theme_colors("default")
        if hasattr(self, 'status_window'):
             # Extract variables FIRST
            panel_bg = theme_colors.get('panel_bg', '#f0f0f0')
            text_primary = theme_colors.get('text_primary', '#000000')
            border = theme_colors.get('border', '#dee2e6')

            self.status_window.setStyleSheet(f"""
                QWidget {{
                    background-color: {panel_bg};
                    border: 1px solid {border};
                    border-radius: 3px;
                }}
                QLabel {{
                    color: #{text_primary};
                    font-weight: bold;
                }}
            """)


    def _apply_file_list_window_theme_styling(self): #vers 7
        """Apply theme styling to the file list window"""
        theme_colors = self._get_theme_colors("default")

        # Extract variables FIRST
        bg_secondary = theme_colors.get('bg_secondary', '#f8f9fa')
        border = theme_colors.get('border', '#dee2e6')
        button_normal = theme_colors.get('button_normal', '#e0e0e0')
        text_primary = theme_colors.get('text_primary', '#000000')
        bg_tertiary = theme_colors.get('bg_tertiary', '#e9ecef')

        if hasattr(self, 'tab_widget'):
            self.tab_widget.setStyleSheet(f"""
                QTabWidget::pane {{
                    background-color: {bg_secondary};
                    border: 1px solid {border};
                    border-radius: 3px;
                }}
                QTabBar::tab {{
                    background-color: {button_normal};
                    color: {text_primary};
                    padding: 5px 10px;
                    margin: 2px;
                    border-radius: 3px;
                }}
                QTabBar::tab:selected {{
                    background-color: {bg_tertiary};
                    border: 1px solid {border};
                }}
            """)


    def _get_theme_colors(self, theme_name): #vers 3
        """Get theme colors - properly connected to app_settings_system"""
        try:
            # Method 1: Use app_settings get_theme_colors() method
            if hasattr(self.main_window, 'app_settings') and hasattr(self.main_window.app_settings, 'get_theme_colors'):
                colors = self.main_window.app_settings.get_theme_colors()
                if colors:
                    print(f"Using app_settings theme colors: {len(colors)} colors loaded")
                    return colors

            # Method 2: Try direct theme access
            if hasattr(self.main_window, 'app_settings') and hasattr(self.main_window.app_settings, 'themes'):
                current_theme = self.main_window.app_settings.current_settings.get("theme", "IMG_Factory")
                theme_data = self.main_window.app_settings.themes.get(current_theme, {})
                colors = theme_data.get('colors', {})
                if colors:
                    print(f"Using direct theme access: {current_theme}")
                    return colors

        except Exception as e:
            print(f"Theme color lookup error: {e}")

        # Fallback with proper theme variables
        print("Using fallback theme colors")
        is_dark = self._is_dark_theme()
        if is_dark:
            return {
                'bg_primary': '#2b2b2b', 'bg_secondary': '#3c3c3c', 'bg_tertiary': '#4a4a4a',
                'panel_bg': '#333333', 'text_primary': '#ffffff', 'text_secondary': '#cccccc',
                'border': '#666666', 'accent_primary': '#FFECEE', 'button_normal': '#404040'
            }
        else:
            return {
                'bg_primary': '#ffffff', 'bg_secondary': '#f8f9fa', 'bg_tertiary': '#e9ecef',
                'panel_bg': '#f0f0f0', 'text_primary': '#000000', 'text_secondary': '#495057',
                'border': '#dee2e6', 'accent_primary': '#1976d2', 'button_normal': '#e0e0e0'
            }


    def apply_all_window_themes(self): #vers 1
        """Apply theme styling to all windows"""
        if hasattr(self, 'tearoff_button') and self.tearoff_button:
            self._apply_tearoff_button_theme()

        self._apply_table_theme_styling()
        self._apply_log_theme_styling()
        self._apply_vertical_splitter_theme()
        self._apply_main_splitter_theme()
        self._apply_status_window_theme_styling()
        self._apply_file_list_window_theme_styling()


    def apply_table_theme(self): #vers 1
        """Legacy method - Apply theme styling to table and related components"""
        # This method is called by main application for compatibility
        self.apply_all_window_themes()


    def _safe_log(self, message): #vers 1
        """Safe logging that won't cause circular dependency"""
        if hasattr(self.main_window, 'log_message') and hasattr(self.main_window, 'gui_layout'):
            self.main_window.log_message(message)
        else:
            print(f"GUI Layout: {message}")

    def log_message(self, message): #vers 1
        """Add message to activity log"""
        if self.log:
            from PyQt6.QtCore import QDateTime
            timestamp = QDateTime.currentDateTime().toString("hh:mm:ss")
            self.log.append(f"[{timestamp}] {message}")
            # Auto-scroll to bottom
            self.log.verticalScrollBar().setValue(
                self.log.verticalScrollBar().maximum()
            )

    # SETTINGS & CONFIGURATION
    def apply_settings_changes(self, settings): #vers 1
        """Apply settings changes to the GUI layout"""
        try:
            # Apply tab settings if they exist
            if any(key.startswith('tab_') or key in ['main_tab_height', 'individual_tab_height', 'tab_font_size', 'tab_padding', 'tab_container_height'] for key in settings.keys()):
                main_height = settings.get("main_tab_height", 30)
                tab_height = settings.get("individual_tab_height", 24)
                font_size = settings.get("tab_font_size", 9)
                padding = settings.get("tab_padding", 4)
                container_height = settings.get("tab_container_height", 40)

                self._apply_dynamic_tab_styling(
                    main_height, tab_height, font_size, padding, container_height
                )

            # Apply button icon settings
            if 'show_button_icons' in settings:
                self._update_button_icons_state(settings['show_button_icons'])

            # Apply other GUI settings as needed
            if 'table_row_height' in settings:
                self._update_table_row_height(settings['table_row_height'])

            if 'widget_spacing' in settings:
                self._update_widget_spacing(settings['widget_spacing'])

            # Apply theme changes
            if 'theme_changed' in settings:
                self.apply_all_window_themes()

        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Error applying settings changes: {str(e)}")

    def _update_table_row_height(self, height): #vers 1
        """Update table row height"""
        try:
            if hasattr(self, 'table') and self.table:
                self.table.verticalHeader().setDefaultSectionSize(height)
        except Exception:
            pass

    def _update_widget_spacing(self, spacing): #vers 1
        """Update widget spacing"""
        try:
            if hasattr(self, 'main_splitter') and self.main_splitter:
                # Update splitter spacing
                self.main_splitter.setHandleWidth(max(4, spacing))
        except Exception:
            pass

    # RESPONSIVE DESIGN & ADAPTIVE LAYOUT
    def handle_resize_event(self, event): #vers 1
        """Handle window resize to adapt button text"""
        if self.main_splitter:
            sizes = self.main_splitter.sizes()
            if len(sizes) > 1:
                right_panel_width = sizes[1]
                self.adapt_buttons_to_width(right_panel_width)

    def adapt_buttons_to_width(self, width): #vers 1
        """Adapt button text based on available width"""
        all_buttons = []
        if hasattr(self, 'img_buttons'):
            all_buttons.extend(self.img_buttons)
        if hasattr(self, 'entry_buttons'):
            all_buttons.extend(self.entry_buttons)
        if hasattr(self, 'options_buttons'):
            all_buttons.extend(self.options_buttons)
        
        for button in all_buttons:
            if hasattr(button, 'full_text'):
                if width > 280:
                    button.setText(button.full_text)
                elif width > 200:
                    # Medium text - remove some words
                    text = button.full_text.replace(' via', '>').replace(' lst', '')
                    button.setText(text)
                elif width > 150:
                    button.setText(button.short_text)
                else:
                    # Icon only mode
                    button.setText("")

    # PROGRESS & STATUS MANAGEMENT

    def show_progress(self, value, text="Working..."): #vers 1
        """Show progress using unified progress system"""
        try:
            from apps.methods.progressbar_functions import show_progress as unified_show_progress
            unified_show_progress(self.main_window, value, text)
        except ImportError:
            # Fallback to old system if unified not available
            if hasattr(self.main_window, 'show_progress'):
                self.main_window.show_progress(text, 0, 100)
                self.main_window.update_progress(value)
            elif hasattr(self.main_window, 'progress_bar'):
                self.main_window.progress_bar.setValue(value)
                self.main_window.progress_bar.setVisible(value >= 0)
            else:
                # Final fallback to status bar
                if hasattr(self.main_window, 'statusBar'):
                    self.main_window.statusBar().showMessage(f"{text} ({value}%)")

    def hide_progress(self): #vers 1
        """Hide progress using unified progress system"""
        try:
            from apps.methods.progressbar_functions import hide_progress as unified_hide_progress
            unified_hide_progress(self.main_window, "Ready")
        except ImportError:
            # Fallback to old system
            if hasattr(self.main_window, 'hide_progress'):
                self.main_window.hide_progress()
            elif hasattr(self.main_window, 'statusBar'):
                self.main_window.statusBar().showMessage("Ready")

    def update_file_info(self, info_text): #vers 1
        """Update file info using unified progress for completion"""
        if hasattr(self.main_window, 'update_img_status'):
            # Extract info from text if possible
            if "entries" in info_text:
                try:
                    count = int(info_text.split()[0])
                    self.main_window.update_img_status(entry_count=count)
                except:
                    pass

    def create_status_bar(self): #vers 1
        """Create status bar with unified progress integration"""
        try:
            from gui.status_bar import create_status_bar
            create_status_bar(self.main_window)

            # Integrate unified progress system
            try:
                from apps.methods.progressbar_functions import integrate_progress_system
                integrate_progress_system(self.main_window)
                self.log_message("Status bar with unified progress created")
            except ImportError:
                self.log_message("Status bar created (unified progress not available)")

        except ImportError:
            # Fallback - create basic status bar
            from PyQt6.QtWidgets import QStatusBar
            self.main_window.setStatusBar(QStatusBar())
            self.main_window.statusBar().showMessage("Ready")
            self.log_message("Basic status bar created (gui.status_bar not available)")
        except Exception as e:
            self.log_message(f"Status bar creation error: {str(e)}")

    def select_all_entries(self):  # vers 1
        """Select all entries in the table"""
        try:
            if self.table and hasattr(self.table, 'selectAll'):
                self.table.selectAll()
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("All entries selected")
            else:
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Table not available for selection")
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Select all entries error: {str(e)}")


    def select_inverse(self):  # vers 4
        """Invert the current selection in the table"""
        try:
            if self.table:
                # Get the selection model
                selection_model = self.table.selectionModel()
                if selection_model:
                    # Store currently selected rows (only consider row level, not individual cells)
                    currently_selected_rows = set()
                    for index in selection_model.selectedIndexes():
                        currently_selected_rows.add(index.row())

                    # Clear current selection
                    self.table.clearSelection()

                    # Select all rows that were NOT selected
                    for row in range(self.table.rowCount()):
                        if row not in currently_selected_rows:
                            # Select the entire row by selecting the first cell in the row
                            index = self.table.model().index(row, 0)
                            selection_model.select(index, QItemSelectionModel.SelectionFlag.Select | QItemSelectionModel.SelectionFlag.Rows)
                else:
                    # Fallback method if selection model is not available
                    # Get all items in the table
                    all_items = []
                    for row in range(self.table.rowCount()):
                        for col in range(self.table.columnCount()):
                            item = self.table.item(row, col)
                            if item:
                                all_items.append(item)
                    # Store currently selected items
                    currently_selected = set(self.table.selectedItems())
                    # Clear selection
                    self.table.clearSelection()
                    # Select items that were not selected
                    for item in all_items:
                        if item not in currently_selected:
                            item.setSelected(True)
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Selection inverted")
            else:
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Table not available for selection")
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Select inverse error: {str(e)}")
            import traceback
            traceback.print_exc()


    def sort_entries(self, sort_order="name"):  # vers 3
        """Sort entries in the table with various options - shows dialog for options"""
        try:
            if self.table:
                # Show sort options dialog
                from PyQt6.QtWidgets import QDialog, QVBoxLayout, QHBoxLayout, QPushButton, QComboBox, QLabel
                dialog = QDialog(self.main_window)
                dialog.setWindowTitle("Sort Options")
                dialog.setModal(True)
                
                layout = QVBoxLayout()
                
                # Sort by label and combo box
                sort_layout = QHBoxLayout()
                sort_layout.addWidget(QLabel("Sort by:"))
                sort_combo = QComboBox()
                sort_combo.addItems(["Name", "Type", "Size", "IDE Model Order"])
                sort_layout.addWidget(sort_combo)
                layout.addLayout(sort_layout)
                
                # OK and Cancel buttons
                button_layout = QHBoxLayout()
                ok_btn = QPushButton("OK")
                cancel_btn = QPushButton("Cancel")
                
                def on_ok():
                    selected_sort = sort_combo.currentText().lower().replace(" ", "_").replace("model_", "")
                    if "ide" in sort_combo.currentText().lower():
                        selected_sort = "ide_order"
                    dialog.accept()
                    # Import the sorting functionality
                    from apps.core.sort import sort_entries_in_table, get_associated_ide_file, parse_ide_file
                    
                    # Get the current IMG file path if available
                    img_path = None
                    if hasattr(self.main_window, 'current_img') and self.main_window.current_img:
                        img_path = self.main_window.current_img.file_path
                    
                    ide_entries = []
                    
                    # If sorting by IDE order, try to find and parse the associated IDE file
                    if selected_sort == "ide_order":
                        if img_path:
                            ide_path = get_associated_ide_file(img_path)
                            if ide_path:
                                ide_entries = parse_ide_file(ide_path)
                                if ide_entries:
                                    self.main_window.log_message(f"Found associated IDE file: {ide_path}")
                                else:
                                    self.main_window.log_message(f"IDE file found but could not be parsed: {ide_path}")
                                    # Fall back to name sorting if IDE parsing failed
                                    selected_sort = "name"
                            else:
                                self.main_window.log_message("No associated IDE file found, using name sort")
                                selected_sort = "name"
                        else:
                            self.main_window.log_message("No IMG file loaded, using name sort")
                            selected_sort = "name"
                    
                    # Perform the sorting
                    sort_entries_in_table(self.table, selected_sort, ide_entries)
                    
                    if selected_sort == "ide_order":
                        self.main_window.log_message("Entries sorted by IDE model order (TXDs at bottom)")
                    else:
                        self.main_window.log_message(f"Entries sorted by {selected_sort} (TXDs at bottom)")
                
                def on_cancel():
                    dialog.reject()
                
                ok_btn.clicked.connect(on_ok)
                cancel_btn.clicked.connect(on_cancel)
                
                button_layout.addWidget(ok_btn)
                button_layout.addWidget(cancel_btn)
                
                layout.addLayout(button_layout)
                dialog.setLayout(layout)
                
                # Show the dialog
                result = dialog.exec()
            else:
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Table not available for sorting")
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Sort entries error: {str(e)}")
            import traceback
            traceback.print_exc()

    def sort_entries_to_match_ide(self):  # vers 2
        """Sort entries to match IDE model order - direct sort without dialog"""
        try:
            if self.table:
                # Import the sorting functionality
                from apps.core.sort import sort_entries_in_table, get_associated_ide_file, parse_ide_file
                
                # Get the current IMG file path if available
                img_path = None
                if hasattr(self.main_window, 'current_img') and self.main_window.current_img:
                    img_path = self.main_window.current_img.file_path
                
                ide_entries = []
                
                # Find and parse the associated IDE file
                if img_path:
                    ide_path = get_associated_ide_file(img_path)
                    if ide_path:
                        ide_entries = parse_ide_file(ide_path)
                        if ide_entries:
                            self.main_window.log_message(f"Found associated IDE file: {ide_path}")
                        else:
                            self.main_window.log_message(f"IDE file found but could not be parsed: {ide_path}")
                            return  # Don't fall back to name sort since user specifically wants IDE sort
                    else:
                        self.main_window.log_message("No associated IDE file found")
                        return
                else:
                    self.main_window.log_message("No IMG file loaded")
                    return
                
                # Perform the sorting
                sort_entries_in_table(self.table, "ide_order", ide_entries)
                self.main_window.log_message("Entries sorted by IDE model order (TXDs at bottom)")
            else:
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Table not available for sorting")
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Sort entries to match IDE error: {str(e)}")
            import traceback
            traceback.print_exc()

    def pin_selected_entries(self):  # vers 1
        """Pin selected entries to keep them at the top of the table"""
        try:
            if self.table and self.table.selectedItems():
                # Get selected rows
                selected_items = self.table.selectedItems()
                selected_rows = set(item.row() for item in selected_items)
                
                # Store the selected rows data
                pinned_data = []
                for row in sorted(selected_rows):
                    row_data = []
                    for col in range(self.table.columnCount()):
                        item = self.table.item(row, col)
                        if item:
                            row_data.append(item.text())
                        else:
                            row_data.append("")
                    pinned_data.append(row_data)
                
                # Remove selected rows from the table (in reverse order to maintain indices)
                for row in sorted(selected_rows, reverse=True):
                    self.table.removeRow(row)
                
                # Insert pinned rows at the top
                for i, row_data in enumerate(pinned_data):
                    self.table.insertRow(i)
                    for j, cell_data in enumerate(row_data):
                        self.table.setItem(i, j, QTableWidgetItem(cell_data))
                
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"{len(pinned_data)} entries pinned to top")
            else:
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("No selected entries to pin or table not available")
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Pin selected entries error: {str(e)}")

    def show_search_dialog(self):  # vers 1
        """Show the search dialog"""
        try:
            # Create and show the search dialog
            search_dialog = ASearchDialog(self.main_window)
            search_dialog.exec()
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Search dialog error: {str(e)}")

    def move_entries_up(self):  # vers 1
        """Move selected entries up in the table"""
        try:
            if self.table and self.table.selectedItems():
                # Get selected rows
                selected_items = self.table.selectedItems()
                selected_rows = sorted(set(item.row() for item in selected_items))
                
                # Check if any selected rows are already at the top
                if 0 in selected_rows:
                    if hasattr(self.main_window, 'log_message'):
                        self.main_window.log_message("Cannot move entries up: some are already at top")
                    return
                
                # Store data for selected rows
                selected_data = []
                for row in selected_rows:
                    row_data = []
                    for col in range(self.table.columnCount()):
                        item = self.table.item(row, col)
                        if item:
                            row_data.append(item.text())
                        else:
                            row_data.append("")
                    selected_data.append(row_data)
                
                # Remove selected rows from the table (in reverse order to maintain indices)
                for row in sorted(selected_rows, reverse=True):
                    self.table.removeRow(row)
                
                # Calculate new positions (move up by 1)
                new_start_pos = min(selected_rows) - 1
                if new_start_pos < 0:
                    new_start_pos = 0
                
                # Insert rows at new positions
                for i, row_data in enumerate(selected_data):
                    insert_row = new_start_pos + i
                    self.table.insertRow(insert_row)
                    for j, cell_data in enumerate(row_data):
                        self.table.setItem(insert_row, j, QTableWidgetItem(cell_data))
                
                # Re-select the moved rows
                self.table.clearSelection()
                for i in range(len(selected_data)):
                    for col in range(self.table.columnCount()):
                        item = self.table.item(new_start_pos + i, col)
                        if item:
                            item.setSelected(True)
                
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"{len(selected_data)} entries moved up")
            else:
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("No selected entries to move or table not available")
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Move entries up error: {str(e)}")

    def move_entries_down(self):  # vers 1
        """Move selected entries down in the table"""
        try:
            if self.table and self.table.selectedItems():
                # Get selected rows
                selected_items = self.table.selectedItems()
                selected_rows = sorted(set(item.row() for item in selected_items))
                
                # Check if any selected rows are already at the bottom
                if max(selected_rows) >= self.table.rowCount() - 1:
                    if hasattr(self.main_window, 'log_message'):
                        self.main_window.log_message("Cannot move entries down: some are already at bottom")
                    return
                
                # Store data for selected rows
                selected_data = []
                for row in reversed(selected_rows):  # Process in reverse to maintain indices when removing
                    row_data = []
                    for col in range(self.table.columnCount()):
                        item = self.table.item(row, col)
                        if item:
                            row_data.append(item.text())
                        else:
                            row_data.append("")
                    selected_data.insert(0, row_data)  # Insert at beginning to maintain order
                
                # Remove selected rows from the table (in reverse order to maintain indices)
                for row in sorted(selected_rows, reverse=True):
                    self.table.removeRow(row)
                
                # Calculate new positions (move down by 1)
                new_start_pos = min(selected_rows) + 1
                
                # Insert rows at new positions
                for i, row_data in enumerate(selected_data):
                    insert_row = new_start_pos + i
                    self.table.insertRow(insert_row)
                    for j, cell_data in enumerate(row_data):
                        self.table.setItem(insert_row, j, QTableWidgetItem(cell_data))
                
                # Re-select the moved rows
                self.table.clearSelection()
                for i in range(len(selected_data)):
                    for col in range(self.table.columnCount()):
                        item = self.table.item(new_start_pos + i, col)
                        if item:
                            item.setSelected(True)
                
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"{len(selected_data)} entries moved down")
            else:
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("No selected entries to move or table not available")
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Move entries down error: {str(e)}")

# LEGACY COMPATIBILITY FUNCTIONS

def create_control_panel(main_window): #vers 1
    """Create the main control panel - LEGACY FUNCTION"""
    # Redirect to new method for compatibility
    if hasattr(main_window, 'gui_layout'):
        return main_window.gui_layout.create_right_panel_with_pastel_buttons()
    return None


__all__ = [
    'IMGFactoryGUILayout',
    'create_control_panel',  # Legacy compatibility
]
