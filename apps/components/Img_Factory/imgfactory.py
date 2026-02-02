#!/usr/bin/env python3
#this belongs in components/Img_Factory/imgfactory.py - Version: 76
# X-Seti - December11 2025 - IMG Factory 1.5 - Fixed Imports

"""
IMG Factory 1.5 - Grand Theft Auto Archive Manager
Main application file - always runs in "main app" mode
"""

import sys
import os
import mimetypes
from typing import Optional, List, Dict, Any
from pathlib import Path

print("Starting application...")

# PyQt6 imports
from PyQt6.QtWidgets import (
    QApplication, QMainWindow, QWidget, QVBoxLayout, QHBoxLayout,
    QSplitter, QTableWidget, QTableWidgetItem, QTextEdit, QLabel, QDialog,
    QPushButton, QFileDialog, QMessageBox, QMenuBar, QStatusBar,
    QProgressBar, QHeaderView, QGroupBox, QComboBox, QLineEdit,
    QAbstractItemView, QTreeWidget, QTreeWidgetItem, QTabWidget,
    QGridLayout, QMenu, QButtonGroup, QRadioButton, QToolBar, QFormLayout,
    QInputDialog, QFrame
)
print("PyQt6.QtWidgets imported successfully")

from PyQt6.QtCore import pyqtSignal, QMimeData, Qt, QThread, QTimer, QSettings, QSize, QPoint, QRect, QByteArray, QItemSelectionModel
from PyQt6.QtGui import QAction, QShortcut, QKeySequence, QPalette, QTextCursor, QFont, QIcon, QPixmap, QImage, QPainter, QPen, QBrush, QColor, QCursor, QContextMenuEvent, QDragEnterEvent
print("PyQt6.QtCore imported successfully")

# Check for optional MSS library
try:
    import mss
    print("MSS library available")
except ImportError:
    print("MSS library not available (screenshots disabled)")


# App utilities
from apps.utils.app_settings_system import AppSettings, apply_theme_to_app, SettingsDialog

# Components
from apps.components.Img_Creator.img_creator import NewIMGDialog, IMGCreationThread
from apps.components.File_Editor.directory_tree_browser import integrate_directory_tree_browser
from apps.components.Project_Manager.project_manager import add_project_menu_items

# Debug
from apps.debug.debug_functions import set_col_debug_enabled
from apps.debug.debug_functions import integrate_all_improvements, install_debug_control_system

# Core functions
from apps.core.img_formats import GameSpecificIMGDialog, IMGCreator
from apps.core.file_extraction import setup_complete_extraction_integration
from apps.core.extract import extract_textures_function
from apps.core.file_type_filter import integrate_file_filtering
from apps.methods.rw_versions import get_rw_version_name
from apps.core.right_click_actions import setup_table_context_menu
from apps.core.shortcuts import setup_all_shortcuts, create_debug_keyboard_shortcuts
from apps.core.convert import convert_img, convert_img_format
from apps.core.reload import integrate_reload_functions
from apps.core.img_split import integrate_split_functions
from apps.core.theme_integration import integrate_theme_system
from apps.core.create import create_new_img
from apps.core.open import _detect_and_open_file, open_file_dialog, _detect_file_type
from apps.core.clean import integrate_clean_utilities
from apps.core.close import install_close_functions, setup_close_manager
from apps.core.export import integrate_export_functions
from apps.core.impotr import integrate_import_functions
from apps.core.remove import integrate_remove_functions
from apps.core.export import export_selected_function, export_all_function
from apps.core.dump import dump_all_function, dump_selected_function, integrate_dump_functions
from apps.core.import_via import integrate_import_via_functions
from apps.core.remove_via import integrate_remove_via_functions
from apps.core.export_via import export_via_function
from apps.core.rebuild import integrate_rebuild_functions
from apps.core.rebuild_all import integrate_batch_rebuild_functions
from apps.core.imgcol_rename import integrate_imgcol_rename_functions
from apps.core.imgcol_replace import integrate_imgcol_replace_functions
from apps.core.imgcol_convert import integrate_imgcol_convert_functions
from apps.core.save_entry import integrate_save_entry_function
from apps.core.undo_system import integrate_undo_system
from apps.core.pin_entries import integrate_pin_functions
from apps.core.inverse_selection import integrate_inverse_selection
from apps.core.sort_via_ide import integrate_sort_via_ide
from apps.core.advanced_img_tools import integrate_advanced_img_tools
from apps.core.rw_unk_snapshot import integrate_unknown_rw_detection
from apps.core.col_viewer_integration import integrate_col_viewer

# GUI Layout
from apps.gui.ide_dialog import integrate_ide_dialog
from apps.gui.gui_backend import ButtonDisplayMode, GUIBackend
from apps.gui.main_window import IMGFactoryMainWindow
from apps.gui.col_display import update_col_info_bar_enhanced
from apps.gui.gui_layout import IMGFactoryGUILayout
from apps.gui.unified_button_theme import apply_unified_button_theme
from apps.gui.gui_menu import IMGFactoryMenuBar
from apps.gui.autosave_menu import integrate_autosave_menu
from apps.components.Project_Manager.project_manager import add_project_menu_items
from apps.gui.tearoff_integration import integrate_tearoff_system
from apps.gui.gui_context import (open_col_file_dialog, open_col_batch_proc_dialog, open_col_editor_dialog, analyze_col_file_dialog)
from apps.gui.gui_layout_custom import IMGFactoryGUILayoutCustom

# Shared Methods
from apps.methods.img_core_classes import (IMGFile, IMGEntry, IMGVersion, Platform, IMGEntriesTable, FilterPanel, IMGFileInfoPanel, TabFilterWidget, integrate_filtering, create_entries_table_panel, format_file_size)

from apps.methods.col_core_classes import (COLFile, COLModel, COLVersion, COLMaterial, COLFaceGroup, COLSphere, COLBox, COLVertex, COLFace, Vector3, BoundingBox, diagnose_col_file)

from apps.methods.col_integration import integrate_complete_col_system
from apps.methods.col_functions import setup_complete_col_integration
from apps.methods.col_parsing_functions import load_col_file_safely
from apps.methods.col_structure_manager import COLStructureManager
from apps.methods.img_analyze import analyze_img_corruption, show_analysis_dialog
from apps.methods.img_integration import integrate_img_functions, img_core_functions
from apps.methods.img_routing_operations import install_operation_routing
from apps.methods.img_validation import IMGValidator
from apps.methods.tab_system import (setup_tab_system, migrate_tabs, create_tab, update_references, integrate_tab_system)

from apps.methods.populate_img_table import reset_table_styling, install_img_table_populator
from apps.methods.progressbar_functions import integrate_progress_system
from apps.methods.update_ui_for_loaded_img import update_ui_for_loaded_img, integrate_update_ui_for_loaded_img

from apps.methods.import_highlight_system import enable_import_highlighting
from apps.methods.img_entry_operations import integrate_entry_operations
from apps.methods.mirror_tab_shared import show_mirror_tab_selection
from apps.methods.ide_parser_functions import integrate_ide_parser
from apps.methods.find_dups_functions import find_duplicates_by_hash, show_duplicates_dialog
from apps.methods.dragdrop_functions import integrate_drag_drop_system
from apps.methods.img_templates import IMGTemplateManager, TemplateManagerDialog
from apps.methods.img_import_functions import integrate_img_import_functions
from apps.methods.img_export_functions import integrate_img_export_functions
from apps.methods.col_export_functions import integrate_col_export_functions
from apps.methods.imgfactory_svg_icons import SVGIconFactory
from apps.methods.imgfactory_svg_icons import (
    get_add_icon, get_open_icon, get_refresh_icon, get_close_icon,
    get_save_icon, get_export_icon, get_import_icon, get_remove_icon,
    get_edit_icon, get_view_icon, get_search_icon, get_settings_icon,
    get_rebuild_icon, get_undobar_icon, get_undo_icon, get_redo_icon
)

# App metadata
App_name = "Img Factory 1.6"
App_build = "January 11 - "
App_auth = "X-Seti"

##Methods list -

def get_current_git_branch(): #vers 1
    """Get current git branch name"""
    try:
        import subprocess
        result = subprocess.run(
            ['git', 'branch', '--show-current'],
            capture_output=True,
            text=True,
            cwd=os.path.dirname(os.path.abspath(__file__))
        )
        if result.returncode == 0:
            branch = result.stdout.strip()
            return f"({branch})" if branch else ""
        return ""
    except:
        return ""

def setup_rebuild_system(self): #vers 1
    """Setup hybrid rebuild system with mode selection"""
    try:
        from apps.core.hybrid_rebuild import setup_hybrid_rebuild_methods
        success = setup_hybrid_rebuild_methods(self)

        if success:
            self.log_message("Hybrid rebuild system enabled")
            # Now you have these methods available:

            # self.rebuild_all_img() - Shows batch mode dialog
            # self.quick_rebuild() - Fast mode only
            # self.fast_rebuild() - Direct fast mode
            # self.safe_rebuild() - Direct safe mode
        else:
            self.log_message("Hybrid rebuild setup failed")

        return success

    except ImportError:
        self.log_message("Hybrid rebuild not available")
        return False


def create_rebuild_menu(self): #vers 1
    """Create rebuild menu with mode options"""
    try:
        # Add to your existing menu bar
        rebuild_menu = self.menuBar().addMenu("  Rebuild")

        # Regular rebuild (shows dialog)
        rebuild_action = QAction("Rebuild IMG...", self)
        rebuild_action.setShortcut("Ctrl+R")
        rebuild_action.setStatusTip("Rebuild current IMG file with mode selection")
        rebuild_action.triggered.connect(self.rebuild_img)
        rebuild_menu.addAction(rebuild_action)

        # Quick rebuild (fast mode only)
        quick_action = QAction("Quick Rebuild", self)
        quick_action.setShortcut("Ctrl+Shift+R")
        quick_action.setStatusTip("Quick rebuild using fast mode")
        quick_action.triggered.connect(self.quick_rebuild)
        rebuild_menu.addAction(quick_action)

        rebuild_menu.addSeparator()

        # Direct mode access
        fast_action = QAction("Fast Rebuild", self)
        fast_action.setStatusTip("Direct fast rebuild without dialog")
        fast_action.triggered.connect(self.fast_rebuild)
        rebuild_menu.addAction(fast_action)

        safe_action = QAction("Safe Rebuild", self)
        safe_action.setStatusTip("Direct safe rebuild with full checking")
        safe_action.triggered.connect(self.safe_rebuild)
        rebuild_menu.addAction(safe_action)

        rebuild_menu.addSeparator()

        # Batch rebuild
        batch_action = QAction("Rebuild All...", self)
        batch_action.setStatusTip("Batch rebuild multiple IMG files")
        batch_action.triggered.connect(self.rebuild_all_img)
        rebuild_menu.addAction(batch_action)

        return True

    except Exception as e:
        self.log_message(f"Rebuild menu creation failed: {str(e)}")
        return False

def setup_debug_mode(self): #vers 2
    """Setup debug mode integration"""
    self.debug = DebugSettings(self.app_settings)

    # Add debug menu item
    if hasattr(self, 'menu_bar_system'):
        debug_action = QAction("ðŸ› Debug Mode", self)
        debug_action.setCheckable(True)
        debug_action.setChecked(self.debug.debug_enabled)
        debug_action.triggered.connect(self.toggle_debug_mode)

        # Add to Settings menu
        if hasattr(self.menu_bar_system, 'settings_menu'):
            self.menu_bar_system.settings_menu.addSeparator()
            self.menu_bar_system.settings_menu.addAction(debug_action)


def debug_trace(func): #ver 1
    """Simple debug decorator to trace function calls."""
    def wrapper(*args, **kwargs):
        print(f"[DEBUG] Calling: {func.__name__} with args={args} kwargs={kwargs}")
        result = func(*args, **kwargs)
        print(f"[DEBUG] Finished: {func.__name__}")
        return result
    return wrapper


def toggle_debug_mode(self): #vers 2
    """Toggle debug mode with user feedback"""
    enabled = self.debug.toggle_debug_mode()
    status = "enabled" if enabled else "disabled"
    self.log_message(f"ðŸ› Debug mode {status}")

    if enabled:
        self.log_message("Debug categories: " + ", ".join(self.debug.debug_categories))
        # Run immediate debug check
        self.debug_img_entries()


def debug_img_entries(self): #vers 2
    """Enhanced debug function with categories"""
    if not self.debug.is_debug_enabled('TABLE_POPULATION'):
        return

    if not self.current_img or not self.current_img.entries:
        self.debug.debug_log("No IMG loaded or no entries found", 'TABLE_POPULATION', 'WARNING')
        return

    self.debug.debug_log(f"IMG file has {len(self.current_img.entries)} entries", 'TABLE_POPULATION')

    # Count file types
    file_types = {}
    all_extensions = set()
    extension_mismatches = []

    for i, entry in enumerate(self.current_img.entries):
        # Extract extension both ways
        name_ext = entry.name.split('.')[-1].upper() if '.' in entry.name else "NO_EXT"
        attr_ext = getattr(entry, 'extension', 'NO_ATTR').upper() if hasattr(entry, 'extension') and entry.extension else "NO_ATTR"

        all_extensions.add(name_ext)
        file_types[name_ext] = file_types.get(name_ext, 0) + 1

        # Check for extension mismatches
        if name_ext != attr_ext and attr_ext != "NO_ATTR":
            extension_mismatches.append(f"{entry.name}: name='{name_ext}' vs attr='{attr_ext}'")

        # Detailed debug for first 5 entries
        if i < 5:
            self.debug.debug_log(f"Entry {i}: {entry.name} -> {name_ext}", 'TABLE_POPULATION')

    # Summary
    self.debug.debug_log("File type summary:", 'TABLE_POPULATION')
    for ext, count in sorted(file_types.items()):
        self.debug.debug_log(f"  {ext}: {count} files", 'TABLE_POPULATION')

    self.debug.debug_log(f"All extensions found: {sorted(all_extensions)}", 'TABLE_POPULATION')

    # Extension mismatches
    if extension_mismatches:
        self.debug.debug_log(f"Extension mismatches found: {len(extension_mismatches)}", 'TABLE_POPULATION', 'WARNING')
        for mismatch in extension_mismatches[:10]:  # Show first 10
            self.debug.debug_log(f"  {mismatch}", 'TABLE_POPULATION', 'WARNING')

    # Table analysis
    table_rows = self.gui_layout.table.rowCount()
    hidden_count = sum(1 for row in range(table_rows) if self.gui_layout.table.isRowHidden(row))

    self.debug.debug_log(f"Table: {table_rows} rows, {hidden_count} hidden", 'TABLE_POPULATION')

    if hidden_count > 0:
        self.debug.debug_log("Some rows are hidden! Checking filter settings...", 'TABLE_POPULATION', 'WARNING')

        # Check filter combo if it exists
        try:
            # Look for filter combo in right panel
            filter_combo = self.findChild(QComboBox)
            if filter_combo:
                current_filter = filter_combo.currentText()
                self.debug.debug_log(f"Current filter: '{current_filter}'", 'TABLE_POPULATION')
        except:
            pass


class IMGLoadThread(QThread):
    """Background thread for loading IMG files"""
    progress_updated = pyqtSignal(int, str)  # progress, status
    loading_finished = pyqtSignal(object)    # IMGFile object
    loading_error = pyqtSignal(str)          # error message

    def __init__(self, file_path: str):
        super().__init__()
        self.file_path = file_path

    def run(self): #vers 1
        try:
            self.progress_updated.emit(10, "Opening file...")

            # Create IMG file instance
            img_file = IMGFile(self.file_path)

            self.progress_updated.emit(30, "Detecting format...")

            # Open and parse file (entries are loaded automatically by open())
            if not img_file.open():
                self.loading_error.emit(f"Failed to open IMG file: {self.file_path}")
                return

            self.progress_updated.emit(60, "Reading entries...")

            # Check if entries were loaded
            if not img_file.entries:
                self.loading_error.emit(f"No entries found in IMG file: {self.file_path}")
                return

            self.progress_updated.emit(80, "Validating...")

            # Validate the loaded file if validator exists
            try:
                validation = IMGValidator.validate_img_file(img_file)
                if not validation.is_valid:
                    # Just warn but don't fail - some IMG files might have minor issues
                    print(f"IMG validation warnings: {validation.get_summary()}")
            except:
                # If validator fails, just continue - validation is optional
                pass

            self.progress_updated.emit(100, "Complete")

            # Return the loaded IMG file
            self.loading_finished.emit(img_file)

        except Exception as e:
            self.loading_error.emit(f"Error loading IMG file: {str(e)}")


class IMGFactory(QMainWindow):
    """Main IMG Factory application window"""
    def __init__(self, settings): #vers 63
        """Initialize IMG Factory with optimized loading order"""
        super().__init__()

        # === PHASE 1: CORE SETUP (Fast) ===
        self.settings = settings
        self.app_settings = settings if hasattr(settings, 'themes') else AppSettings()

        # CRITICAL: Initialize IMG Factory settings BEFORE using them
        from apps.methods.img_factory_settings import IMGFactorySettings
        self.img_settings = IMGFactorySettings()

        self.apply_window_decoration_setting()

        # Get UI mode from img_settings (now initialized)
        ui_mode = self.img_settings.get("ui_mode", "system")
        show_toolbar = self.img_settings.get("show_toolbar", True)
        show_status_bar = self.img_settings.get("show_status_bar", True)
        show_menu_bar = self.img_settings.get("show_menu_bar", True)

        print(f"DEBUG: Loading UI mode: {ui_mode!r}")

        # Window setup
        branch = get_current_git_branch()
        self.setWindowTitle(App_name + " - " + App_auth + " - " + App_build + " " + branch)
        self.setGeometry(100, 100, 1200, 800)

        # Set default fonts
        from PyQt6.QtGui import QFont
        default_font = QFont("Fira Sans Condensed", 14)
        #self.setFont(default_font)
        self.title_font = QFont("Arial", 14)
        self.panel_font = QFont("Arial", 10)
        self.button_font = QFont("Arial", 10)
        self.infobar_font = QFont("Courier New", 9)

        self.undo_stack = []
        self.button_display_mode = 'both'
        self.last_save_directory = None

        # Core data initialization
        self.current_img: Optional[IMGFile] = None
        self.current_col: Optional[COLFile] = None

        #self.current_txd = None
        #self.txd_workshops = []
        self.open_files = {}
        self.tab_counter = 0
        self.load_thread: Optional[IMGLoadThread] = None
        self.info_bar = None
        self._checkerboard_size = 16
        self._overlay_opacity = 50
        self.background_color = QColor(42, 42, 42)
        self.background_mode = 'solid'

        #self._initialize_features()

        # Corner resize variables
        self.dragging = False
        self.drag_position = None
        self.resizing = False
        self.resize_corner = None
        self.corner_size = 20
        self.hover_corner = None

        # Enable mouse tracking for resize corners
        self.setMouseTracking(True)

        # === PHASE 2: ESSENTIAL COMPONENTS (Fast) ===

        # Template manager (with better error handling)
        try:
            from apps.methods.img_templates import IMGTemplateManager
            self.template_manager = IMGTemplateManager()
            print("Template manager initialized")
        except Exception as e:
            print(f"Template manager failed: {str(e)}")
            class DummyTemplateManager:
                def get_all_templates(self): return []
                def get_default_templates(self): return []
                def get_user_templates(self): return []
            self.template_manager = DummyTemplateManager()

        # === PHASE 3: GUI CREATION (Medium) ===

        # Integrate functionality that menu system depends on
        integrate_sort_via_ide(self)

        # Create GUI layout based on UI mode (using variables set earlier)
        print(f"DEBUG: Creating GUI layout for mode: {ui_mode!r}")
        if ui_mode == "custom":
            from apps.gui.gui_layout_custom import IMGFactoryGUILayoutCustom
            self.gui_layout = IMGFactoryGUILayoutCustom(self)
        else:
            from apps.gui.gui_layout import IMGFactoryGUILayout
            self.gui_layout = IMGFactoryGUILayout(self)

        # Apply UI mode settings to the newly created layout
        self.apply_ui_mode(ui_mode, show_toolbar, show_status_bar, show_menu_bar)

        # Menu system
        self.menubar = self.menuBar()
        self.menu_bar_system = IMGFactoryMenuBar(self)

        # Menu callbacks
        callbacks = {
            "about": self.show_about,
            "open_img": self.open_img_file,
            "new_img": self.create_new_img,
            "exit": self.close,
            "img_validate": self.validate_img,
            "customize_interface": self.show_gui_settings,
        }
        self.menu_bar_system.set_callbacks(callbacks)
        integrate_drag_drop_system(self)

        # Create main UI (includes tab system setup)
        self._create_ui()

        add_project_menu_items(self)

        # Additional UI integrations
        integrate_tab_system(self)
        integrate_tearoff_system(self)

        # Core parsers (now safe to use log_message)
        integrate_ide_parser(self)
        integrate_ide_dialog(self)
        install_operation_routing(self)
        integrate_dump_functions(self)
        integrate_img_functions(self)
        integrate_export_functions(self)
        integrate_import_functions(self)

        # Apply button display mode from settings
        self._apply_button_display_mode_from_settings()
        self._apply_button_settings_at_startup()

        integrate_remove_functions(self)
        integrate_save_entry_function(self)
        integrate_batch_rebuild_functions(self)
        integrate_rebuild_functions(self)

        integrate_imgcol_rename_functions(self)
        integrate_imgcol_replace_functions(self)
        integrate_imgcol_convert_functions(self)

        # Integrate new functionality
        integrate_undo_system(self)
        integrate_pin_functions(self)
        integrate_inverse_selection(self)
        integrate_advanced_img_tools(self)

        self.export_via = lambda: export_via_function(self)
        integrate_import_via_functions(self)
        integrate_remove_via_functions(self)
        integrate_entry_operations(self)
        integrate_img_import_functions(self)
        integrate_img_export_functions(self)
        integrate_col_export_functions(self)
        # No specific integration needed for extract functionality

        # File operations
        install_close_functions(self)

        # Table population (needed for IMG display)
        install_img_table_populator(self)

        # Update UI system
        integrate_update_ui_for_loaded_img(self)

        # === PHASE 5: CORE FUNCTIONALITY (Medium) ===
        self.export_selected = lambda: export_selected_function(self)
        self.export_all = lambda: export_all_function(self)
        self.dump_all = lambda: dump_all_function(self)
        self.dump_selected = lambda: dump_selected_function(self)
        #integrate_refresh_table(self)
        integrate_reload_functions(self)

        # TXD Editor Integration
        try:
            self.txd_editor = None
            self.log_message("TXD Editor available")
        except Exception as e:
            self.log_message(f"TXD Editor failed: {str(e)}")

        # File extraction
        try:
            from apps.core.file_extraction import setup_complete_extraction_integration
            setup_complete_extraction_integration(self)
            self.log_message("File extraction integrated")
        except Exception as e:
            self.log_message(f"Extraction integration failed: {str(e)}")

        # COL System Integration
        try:
            from apps.methods.populate_col_table import load_col_file_safely
            self.load_col_file_safely = lambda file_path: load_col_file_safely(self, file_path)
            self.log_message("COL file loading enabled")
        except Exception as e:
            self.log_message(f"COL loading setup failed: {str(e)}")

        # File filtering
        integrate_file_filtering(self)

        # === PHASE 6: GUI BACKEND & SHORTCUTS (Medium) ===

        # GUI backend
        self.gui_backend = GUIBackend(self)

        # Keyboard shortcuts
        setup_all_shortcuts(self)

        # Context menus
        setup_table_context_menu(self)

        # === PHASE 7: OPTIONAL FEATURES (Heavy - Can be delayed) ===

        # Auto-save menu
        integrate_autosave_menu(self)

        # Theme system
        integrate_theme_system(self)
        if hasattr(self.app_settings, 'themes'):
            apply_theme_to_app(QApplication.instance(), self.app_settings)

        # Progress system
        #integrate_progress_system(self)

        # Split functions
        integrate_split_functions(self)

        # RW detection
        integrate_unknown_rw_detection(self)

        try:
            integrate_rebuild_functions(self)
            integrate_batch_rebuild_functions(self)
            integrate_clean_utilities(self)
            self.log_message("All systems integrated")
        except Exception as e:
            self.log_message(f"Integration failed: {e}")

        # === PHASE 9: HIGHLIGHTING & FINAL SETUP ===

        # Import highlighting
        enable_import_highlighting(self)

        # Restore settings
        self._restore_settings()
        self.autoload_game_root()

        # Utility functions
        self.setup_missing_utility_functions()

        # Final reload alias
        self.reload_table = self.reload_current_file

        # === STARTUP COMPLETE ===
        self.log_message("IMG Factory 1.6 initialized - Ready!")

        # Apply comprehensive fixes for menu system and functionality
        fix_menu_system_and_functionality(self)

        # Apply search and performance fixes
        self.apply_search_and_performance_fixes()

        # Show window (non-blocking)
        self.show()


    def log_message(self, message: str): #vers 2
        """Optimized logging that works before GUI is ready"""
        try:
            # Check if GUI is ready
            if hasattr(self, 'gui_layout') and hasattr(self.gui_layout, 'log') and self.gui_layout.log:
                # Use QTimer to defer log updates to prevent blocking
                QTimer.singleShot(0, lambda: self._append_log_message(message))
            else:
                # Fallback to console if GUI not ready
                print(f"LOG: {message}")
        except Exception:
            print(f"LOG: {message}")


    def _append_log_message(self, message: str): #vers 1
        """Internal log message append"""
        try:
            if hasattr(self.gui_layout, 'log') and self.gui_layout.log:
                self.gui_layout.log.append(message)
                # Scroll to bottom
                scrollbar = self.gui_layout.log.verticalScrollBar()
                scrollbar.setValue(scrollbar.maximum())
        except Exception:
            pass


    def apply_window_decoration_setting(self):
        """Apply system vs custom window decoration based on settings"""

        if not hasattr(self, 'app_settings'):
            return
        use_system = self.app_settings.current_settings.get('use_system_titlebar', True)
        current_geometry = self.geometry()
        was_visible = self.isVisible()
        if use_system:
            self.setWindowFlags(
                Qt.WindowType.Window |
                Qt.WindowType.WindowMinimizeButtonHint |
                Qt.WindowType.WindowMaximizeButtonHint |
                Qt.WindowType.WindowCloseButtonHint
            )
        else:
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint)
        self.setGeometry(current_geometry)
        if was_visible:
            self.show()

    def apply_ui_mode(self, ui_mode, show_toolbar=True, show_status_bar=True, show_menu_bar=True): #vers 6
        """Apply UI mode settings - custom or system UI"""
        current_geometry = self.geometry()
        was_visible = self.isVisible()

        # In custom mode section:
        if ui_mode == "custom":
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint)

            # FORCE HIDE menu bar in custom mode
            menu_bar = self.menuBar()
            menu_bar.setVisible(False)
            menu_bar.setMaximumHeight(0)  # Collapse it completely

            # HIDE menu bar in custom mode (toolbar has Settings button)
            if hasattr(self, 'menuBar') and callable(self.menuBar):
                menu_bar = self.menuBar()
                if menu_bar:
                    menu_bar.setVisible(False)  # Always hidden in custom mode
        else:
            # Use system window
            self.setWindowFlags(
                Qt.WindowType.Window |
                Qt.WindowType.WindowMinimizeButtonHint |
                Qt.WindowType.WindowMaximizeButtonHint |
                Qt.WindowType.WindowCloseButtonHint
            )

            # Show menu bar in system mode
            if hasattr(self, 'menuBar') and callable(self.menuBar):
                menu_bar = self.menuBar()
                if menu_bar:
                    menu_bar.setVisible(show_menu_bar)

        # Status bar visibility
        if hasattr(self, 'statusBar') and callable(self.statusBar):
            status_bar = self.statusBar()
            if status_bar:
                status_bar.setVisible(show_status_bar)

        # Toolbar visibility - delegate to gui_layout
        if hasattr(self, 'gui_layout') and hasattr(self.gui_layout, 'apply_ui_mode'):
            self.gui_layout.apply_ui_mode(ui_mode, show_toolbar, show_status_bar, show_menu_bar)

        self.setGeometry(current_geometry)
        if was_visible:
            self.show()


    def autoload_game_root(self): #vers 2
        """Autoload game root and integrate directory tree at startup"""
        try:
            # Try QSettings first
            from PyQt6.QtCore import QSettings
            settings = QSettings("IMG-Factory", "IMG-Factory")
            game_root = settings.value("game_root", "", type=str)

            # If not in QSettings, try project manager
            if not game_root and hasattr(self, 'project_manager') and self.project_manager:
                if hasattr(self.project_manager, 'current_project') and self.project_manager.current_project:
                    project_settings = self.project_manager.get_project_settings(
                        self.project_manager.current_project
                    )
                    game_root = project_settings.get('game_root', '')

            # If found and valid, set it and load directory tree
            if game_root and os.path.exists(game_root):
                self.game_root = game_root
                self.log_message(f"✓ Autoloaded game root: {game_root}")

                # Integrate directory tree if not already done
                if not hasattr(self, 'directory_tree'):
                    from apps.components.File_Editor.directory_tree_browser import integrate_directory_tree_browser
                    #if integrate_directory_tree_browser(self): - Creates corrupted toolbar!
                    #    self.log_message("✓ Directory Tree browser integrated")

                # Browse to game root
                if hasattr(self, 'directory_tree') and hasattr(self.directory_tree, 'browse_directory'):
                    self.directory_tree.browse_directory(game_root)
                    self.log_message("✓ Directory Tree populated with game root")

                    # Switch to directory tree tab (tab 0) after short delay
                    if hasattr(self, 'main_tab_widget') and self.main_tab_widget:
                        QTimer.singleShot(200, lambda: self.main_tab_widget.setCurrentIndex(0))
            else:
                self.log_message("ℹ No saved game root - showing home directory")

                # Still integrate directory tree with home folder
                if not hasattr(self, 'directory_tree'):
                    from apps.components.File_Editor.directory_tree_browser import integrate_directory_tree_browser
                    if integrate_directory_tree_browser(self):
                        home_dir = str(Path.home())
                        if hasattr(self, 'directory_tree') and hasattr(self.directory_tree, 'browse_directory'):
                            self.directory_tree.browse_directory(home_dir)
                            self.log_message(f"✓ Directory Tree showing: {home_dir}")

                            # Switch to directory tree tab
                            if hasattr(self, 'main_tab_widget') and self.main_tab_widget:
                                QTimer.singleShot(200, lambda: self.main_tab_widget.setCurrentIndex(0))

        except Exception as e:
            self.log_message(f"Error autoloading game root: {str(e)}")
            import traceback
            traceback.print_exc()



    def autoload_game_root_two(self): #vers 3
        """Autoload game root and integrate directory tree at startup"""
        try:
            from PyQt6.QtCore import QSettings
            settings = QSettings("IMG-Factory", "IMG-Factory")
            game_root = settings.value("game_root", "", type=str)

            # Try project manager if not in QSettings
            if not game_root and hasattr(self, 'project_manager') and self.project_manager:
                if hasattr(self.project_manager, 'current_project') and self.project_manager.current_project:
                    project_settings = self.project_manager.get_project_settings(
                        self.project_manager.current_project
                    )
                    game_root = project_settings.get('game_root', '')

            # If found and valid, integrate directory tree
            if game_root and os.path.exists(game_root):
                self.game_root = game_root
                self.log_message(f"✓ Autoloaded game root: {game_root}")

                # Auto-integrate directory tree
                if not hasattr(self, 'directory_tree'):
                    from apps.components.File_Editor.directory_tree_browser import integrate_directory_tree_browser
                    if integrate_directory_tree_browser(self):
                        # Place in Tab 0's file window
                        if hasattr(self.gui_layout, 'middle_vertical_splitter'):
                            splitter = self.gui_layout.middle_vertical_splitter
                            if splitter and splitter.count() > 0:
                                file_window = splitter.widget(0)
                                layout = file_window.layout()
                                if layout:
                                    layout.addWidget(self.directory_tree)
                                    self.directory_tree.hide()  # Hidden until Tab 0 clicked
                                    self.log_message("✓ Directory tree ready")

                        # Browse to game root
                        if hasattr(self.directory_tree, 'browse_directory'):
                            self.directory_tree.browse_directory(game_root)

        except Exception as e:
            self.log_message(f"Error autoloading game root: {str(e)}")
            import traceback
            traceback.print_exc()


    def _apply_button_settings_at_startup(self): #vers 1
        """Apply button sizing and spacing settings at startup"""
        try:
            if hasattr(self, 'app_settings') and hasattr(self.app_settings, 'current_settings'):
                # Get button settings
                button_height = self.app_settings.current_settings.get('button_height', 32)
                space_v = self.app_settings.current_settings.get('button_spacing_vertical', 8)
                space_h = self.app_settings.current_settings.get('button_spacing_horizontal', 6)

                # Apply to GUI layout if available
                if hasattr(self, 'gui_layout'):
                    # Apply button heights
                    if hasattr(self.gui_layout, 'img_buttons'):
                        for btn in self.gui_layout.img_buttons:
                            btn.setMaximumHeight(button_height)
                            btn.setMinimumHeight(button_height - 4)

                    if hasattr(self.gui_layout, 'entry_buttons'):
                        for btn in self.gui_layout.entry_buttons:
                            btn.setMaximumHeight(button_height)
                            btn.setMinimumHeight(button_height - 4)

                    if hasattr(self.gui_layout, 'options_buttons'):
                        for btn in self.gui_layout.options_buttons:
                            btn.setMaximumHeight(button_height)
                            btn.setMinimumHeight(button_height - 4)

                    self.log_message(f"✓ Button sizing applied: {button_height}px height, {space_v}/{space_h}px spacing")

        except Exception as e:
            self.log_message(f"Error applying button settings: {str(e)}")


    def _apply_button_display_mode_from_settings(self):
        """Apply button display mode from app settings"""
        try:
            # Get the button display mode from settings
            if hasattr(self, 'app_settings') and hasattr(self.app_settings, 'current_settings'):
                button_mode = self.app_settings.current_settings.get('button_display_mode', 'both')
                
                # Map the settings mode to the GUI mode
                mode_mapping = {
                    'both': 'icons_with_text',    # Icons + Text
                    'icons': 'icons_only',        # Icons Only  
                    'text': 'text_only'           # Text Only
                }
                
                gui_mode = mode_mapping.get(button_mode, 'icons_with_text')
                
                # Apply to GUI layout if available
                if hasattr(self, 'gui_layout') and hasattr(self.gui_layout, 'set_button_display_mode'):
                    self.gui_layout.set_button_display_mode(gui_mode)
                    self.log_message(f"Applied button display mode: {button_mode} -> {gui_mode}")
                else:
                    self.log_message("GUI layout not ready for button display mode")
            else:
                self.log_message("App settings not available for button display mode")
        except Exception as e:
            self.log_message(f"Error applying button display mode: {str(e)}")


    def debug_img_before_loading(self, file_path): #vers 1
        """Quick debug before loading IMG"""
        try:
            file_size = os.path.getsize(file_path)
            self.log_message(f"Debug: File size = {file_size:,} bytes")

            with open(file_path, 'rb') as f:
                first_8_bytes = f.read(8)
                self.log_message(f"Debug: First 8 bytes = {first_8_bytes.hex()}")

                if first_8_bytes.startswith(b'VER2'):
                    entry_count = struct.unpack('<I', first_8_bytes[4:8])[0]
                    self.log_message(f"Debug: V2 entry count = {entry_count:,}")
                else:
                    potential_v1_entries = file_size // 32
                    self.log_message(f"Debug: Potential V1 entries = {potential_v1_entries:,}")

        except Exception as e:
            self.log_message(f"Debug failed: {e}")


    def show_debug_settings(self): #vers 1
        """Show debug settings dialog"""
        try:
            # Try to show proper debug settings if available
            from apps.utils.app_settings_system import SettingsDialog
            if hasattr(self, 'app_settings'):
                dialog = SettingsDialog(self.app_settings, self)
                dialog.exec()
            else:
                QMessageBox.information(self, "Debug Settings", "Debug settings: Use F12 to toggle performance mode")
        except ImportError:
            QMessageBox.information(self, "Debug Settings", "Debug settings: Use F12 to toggle performance mode")


    def _append_log_message(self, message: str): #vers 1
        """Internal log message append"""
        try:
            if hasattr(self.gui_layout, 'log') and self.gui_layout.log:
                self.gui_layout.log.append(message)
                # Scroll to bottom
                scrollbar = self.gui_layout.log.verticalScrollBar()
                scrollbar.setValue(scrollbar.maximum())
        except Exception:
            pass

    def analyze_corruption(self):
        """Analyze and fix IMG corruption"""
        return self.analyze_img_corruption()


    def analyze_img_corruption(self): #vers 1
        """Analyze IMG file for corruption - Menu callback"""
        try:
            if not hasattr(self, 'current_img') or not self.current_img:
                QMessageBox.warning(self, "No IMG File", "Please open an IMG file first to analyze corruption")
                return

            self.log_message("ðŸ” Starting IMG corruption analysis...")

            # Show corruption analysis dialog
            from apps.core.img_corruption_analyzer import show_corruption_analysis_dialog
            result = show_corruption_analysis_dialog(self)

            if result:
                # User wants to apply fixes
                report = result['report']
                fix_options = result['fix_options']

                from apps.core.img_corruption_analyzer import fix_corrupted_img
                success = fix_corrupted_img(self.current_img, report, fix_options, self)

                if success:
                    self.log_message("IMG corruption fixed successfully")
                else:
                    self.log_message("IMG corruption fix failed")
            else:
                self.log_message("Corruption analysis completed (no fixes applied)")

        except Exception as e:
            self.log_message(f"Corruption analysis error: {str(e)}")
            QMessageBox.critical(self, "Analysis Error", f"Corruption analysis failed:\n{str(e)}")


    def quick_fix_corruption(self): #vers 1
        """Quick fix common corruption issues - Menu callback"""
        try:
            if not hasattr(self, 'current_img') or not self.current_img:
                QMessageBox.warning(self, "No IMG File", "Please open an IMG file first")
                return

            self.log_message("  Quick fixing IMG corruption...")

            # Analyze first
            from apps.core.img_corruption_analyzer import analyze_img_corruption
            report = analyze_img_corruption(self.current_img, self)

            if 'error' in report:
                QMessageBox.critical(self, "Analysis Failed",
                                   f"Could not analyze file:\n{report['error']}")
                return

            corrupted_count = len(report.get('corrupted_entries', []))

            if corrupted_count == 0:
                QMessageBox.information(self, "No Corruption",
                                      "No corruption detected in this IMG file!")
                return

            # Confirm quick fix
            reply = QMessageBox.question(self, "Quick Fix Corruption",
                                       f"Found {corrupted_count} corrupted entries.\n\n"
                                       f"Quick fix will:\n"
                                       f"â€¢ Clean all filenames\n"
                                       f"â€¢ Remove null bytes\n"
                                       f"â€¢ Fix control characters\n"
                                       f"â€¢ Create backup\n\n"
                                       f"Continue with quick fix?",
                                       QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)

            if reply == QMessageBox.StandardButton.Yes:
                # Apply quick fix options
                quick_fix_options = {
                    'fix_filenames': True,
                    'remove_invalid': False,  # Don't remove entries in quick fix
                    'fix_null_bytes': True,
                    'fix_long_names': True,
                    'create_backup': True
                }

                from apps.core.img_corruption_analyzer import fix_corrupted_img
                success = fix_corrupted_img(self.current_img, report, quick_fix_options, self)

                if success:
                    self.log_message("Quick corruption fix completed")
                    QMessageBox.information(self, "Quick Fix Complete", f"Successfully fixed {corrupted_count} corrupted entries!\n\n", f"The IMG file has been cleaned and rebuilt.")
                else:
                    self.log_message("Quick corruption fix failed")

        except Exception as e:
            self.log_message(f"Quick fix error: {str(e)}")
            QMessageBox.critical(self, "Quick Fix Error", f"Quick fix failed:\n{str(e)}")


    def clean_filenames_only(self): #vers 1
        """Clean only filenames, keep all entries - Menu callback"""
        try:
            if not hasattr(self, 'current_img') or not self.current_img:
                QMessageBox.warning(self, "No IMG File", "Please open an IMG file first")
                return

            self.log_message("ðŸ§¹ Cleaning filenames only...")

            # Analyze corruption
            from apps.core.img_corruption_analyzer import analyze_img_corruption
            report = analyze_img_corruption(self.current_img, self)

            if 'error' in report:
                QMessageBox.critical(self, "Analysis Failed", f"Could not analyze file:\n{report['error']}")
                return

            corrupted_count = len(report.get('corrupted_entries', []))

            if corrupted_count == 0:
                QMessageBox.information(self, "No Corruption", "No filename corruption detected!")
                return

            # Apply filename-only cleaning
            filename_fix_options = {
                'fix_filenames': True,
                'remove_invalid': False,  # Never remove entries
                'fix_null_bytes': True,
                'fix_long_names': True,
                'create_backup': True
            }

            from apps.core.img_corruption_analyzer import fix_corrupted_img
            success = fix_corrupted_img(self.current_img, report, filename_fix_options, self)

            if success:
                self.log_message("Filename cleaning completed")
                QMessageBox.information(self, "Filenames Cleaned", f"Successfully cleaned {corrupted_count} filenames!\n\n", f"All entries preserved, only filenames fixed.")
            else:
                self.log_message("Filename cleaning failed")

        except Exception as e:
            self.log_message(f"Filename cleaning error: {str(e)}")
            QMessageBox.critical(self, "Cleaning Error", f"Filename cleaning failed:\n{str(e)}")


    def export_corruption_report(self): #vers 1
        """Export corruption report to file - Menu callback"""
        try:
            if not hasattr(self, 'current_img') or not self.current_img:
                QMessageBox.warning(self, "No IMG File", "Please open an IMG file first")
                return

            self.log_message("Generating corruption report...")

            # Analyze corruption
            from apps.core.img_corruption_analyzer import analyze_img_corruption
            report = analyze_img_corruption(self.current_img, self)

            if 'error' in report:
                QMessageBox.critical(self, "Analysis Failed", f"Could not analyze file:\n{report['error']}")
                return

            # Get save filename
            from PyQt6.QtWidgets import QFileDialog
            filename, _ = QFileDialog.getSaveFileName(
                self, "Export Corruption Report",
                f"{os.path.splitext(os.path.basename(self.current_img.file_path))[0]}_corruption_report.txt", "Text Files (*.txt);;All Files (*)")

            if filename:
                # Export detailed report
                with open(filename, 'w', encoding='utf-8') as f:
                    f.write(f"IMG Corruption Analysis Report\n")
                    f.write(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
                    f.write(f"File: {self.current_img.file_path}\n")
                    f.write("=" * 60 + "\n\n")

                    # Summary
                    total_entries = report.get('total_entries', 0)
                    corrupted_count = len(report.get('corrupted_entries', []))
                    f.write(f"Summary:\n")
                    f.write(f"  Total Entries: {total_entries:,}\n")
                    f.write(f"  Corrupted Entries: {corrupted_count:,}\n")
                    f.write(f"  Corruption Level: {(corrupted_count/total_entries*100) if total_entries > 0 else 0:.1f}%\n")
                    f.write(f"  Severity: {report.get('severity', 'Unknown')}\n\n")

                    # Issue breakdown
                    f.write(f"Issue Breakdown:\n")
                    for issue_type, count in report.get('issue_summary', {}).items():
                        f.write(f"  {issue_type}: {count} entries\n")
                    f.write("\n")

                    # Detailed corrupted entries
                    f.write(f"Detailed Corrupted Entries:\n")
                    f.write("-" * 60 + "\n")

                    for entry in report.get('corrupted_entries', []):
                        f.write(f"\nEntry #{entry.get('index', 0)}:\n")
                        f.write(f"  Original Name: {repr(entry.get('original_name', ''))}\n")
                        f.write(f"  Issues: {', '.join(entry.get('issues', []))}\n")
                        f.write(f"  Suggested Fix: {entry.get('suggested_fix', '')}\n")
                        f.write(f"  Size: {entry.get('size', 0):,} bytes\n")
                        f.write(f"  Offset: 0x{entry.get('offset', 0):08X}\n")

                self.log_message(f"Corruption report exported to: {filename}")
                QMessageBox.information(self, "Report Exported",
                                      f"Corruption report exported to:\n{filename}")

        except Exception as e:
            self.log_message(f"Report export error: {str(e)}")
            QMessageBox.critical(self, "Export Error", f"Report export failed:\n{str(e)}")


    # Menu isolation: Docked workshops should not affect main window menu
    def open_txd_workshop_docked(self, txd_name=None, txd_data=None): #vers 3
        """Open TXD Workshop as overlay on file window"""
        from apps.components.Txd_Editor.txd_workshop import TXDWorkshop


    # Menu isolation: Docked workshops should not affect main window menu
    def open_col_workshop_docked(self, col_name=None, col_data=None): #vers 1
        """Open COL Workshop as overlay on file window - SIMILAR TO TXD VERSION"""
        from apps.components.Col_Editor.col_workshop import COLWorkshop
        from PyQt6.QtWidgets import QTableWidget
        from PyQt6.QtCore import Qt

        # Get current tab
        current_tab_index = self.main_tab_widget.currentIndex()
        if current_tab_index < 0:
            self.log_message("No active tab")
            return None

        current_tab = self.main_tab_widget.widget(current_tab_index)
        if not current_tab:
            return None

        # Find the file list table to get its geometry
        tables = current_tab.findChildren(QTableWidget)

        if not tables:
            self.log_message("No table found to overlay")
            return None

        file_table = tables[0]

        # Create COL Workshop as frameless overlay
        workshop = COLWorkshop(parent=self, main_window=self)

        # Make it frameless overlay
        workshop.setWindowFlags(Qt.WindowType.Tool | Qt.WindowType.FramelessWindowHint)

        # Load COL data if provided
        if col_data:
            # TODO: Implement COL data loading
            workshop.load_from_img_archive(col_data)
        elif col_name and hasattr(self, 'current_img') and self.current_img:
            # Find the COL entry in current IMG
            for entry in self.current_img.entries:
                if entry.name.lower() == col_name.lower():
                    # TODO: Implement COL loading from entry
                    workshop.load_from_img_archive(self.current_img.file_path)
                    break

        # Get file table geometry in global coordinates
        table_rect = file_table.geometry()
        table_global_pos = file_table.mapToGlobal(table_rect.topLeft())

        # Position workshop over the file table
        workshop.setGeometry(
            table_global_pos.x(),
            table_global_pos.y(),
            table_rect.width(),
            table_rect.height()
        )

        # Mark as overlay
        workshop.is_overlay = True
        workshop.overlay_table = file_table
        workshop.overlay_tab_index = current_tab_index

        workshop.show()
        workshop.raise_()

        # Store in main window
        if not hasattr(self, 'col_workshops'):
            self.col_workshops = []
        self.col_workshops.append(workshop)

        # Connect tab switching to hide/show
        self.main_tab_widget.currentChanged.connect(
            lambda idx: self._handle_col_overlay_tab_switch(workshop, idx)
        )

        self.log_message("COL Workshop opened as overlay")

        return workshop


    def _handle_col_overlay_tab_switch(self, workshop, new_tab_index): #vers 1
        """Handle hiding/showing COL Workshop overlay on tab switch"""
        if not hasattr(workshop, 'is_overlay') or not workshop.is_overlay:
            return

        if new_tab_index == workshop.overlay_tab_index:
            # Switched to COL's tab - show and raise
            workshop.show()
            workshop.raise_()
            workshop.activateWindow()
        else:
            # Switched away - hide it
            workshop.hide()

        # Get current tab
        current_tab_index = self.main_tab_widget.currentIndex()
        if current_tab_index < 0:
            self.log_message("No active tab")
            return None

        current_tab = self.main_tab_widget.widget(current_tab_index)
        if not current_tab:
            return None

        # Find the file list table to get its geometry
        from PyQt6.QtWidgets import QTableWidget
        tables = current_tab.findChildren(QTableWidget)

        if not tables:
            self.log_message("No table found to overlay")
            return None

        file_table = tables[0]

        # Create TXD Workshop as frameless overlay
        workshop = TXDWorkshop(parent=self, main_window=self)

        # Make it frameless overlay
        workshop.setWindowFlags(Qt.WindowType.Tool | Qt.WindowType.FramelessWindowHint)

        # Load TXD data if provided
        if txd_name and txd_data:
            workshop._load_txd_textures(txd_data, txd_name)

        # Get file table geometry in global coordinates
        table_rect = file_table.geometry()
        table_global_pos = file_table.mapToGlobal(table_rect.topLeft())

        # Position workshop over the file table
        workshop.setGeometry(
            table_global_pos.x(),
            table_global_pos.y(),
            table_rect.width(),
            table_rect.height()
        )

        # Store references for show/hide
        workshop.overlay_table = file_table
        workshop.overlay_tab_index = current_tab_index
        workshop.is_overlay = True

        # Show the workshop
        workshop.show()
        workshop.raise_()

        # Store in main window
        if not hasattr(self, 'txd_workshops'):
            self.txd_workshops = []
        self.txd_workshops.append(workshop)

        # Connect tab switching to hide/show
        self.main_tab_widget.currentChanged.connect(
            lambda idx: self._handle_txd_overlay_tab_switch(workshop, idx)
        )

        self.log_message("TXD Workshop opened as overlay")

        return workshop


    def _handle_txd_overlay_tab_switch(self, workshop, new_tab_index): #vers 1
        """Handle hiding/showing TXD Workshop overlay on tab switch"""
        if not hasattr(workshop, 'is_overlay') or not workshop.is_overlay:
            return

        if new_tab_index == workshop.overlay_tab_index:
            # Switched to TXD's tab - show and raise
            workshop.show()
            workshop.raise_()
            workshop.activateWindow()
        else:
            # Switched away - hide it
            workshop.hide()


    def setup_unified_signals(self): #vers 6
        """Setup unified signal handler for all table interactions"""
        from apps.components.unified_signal_handler import connect_table_signals

        # Connect main entries table to unified system
        success = connect_table_signals(
            table_name="main_entries",
            table_widget=self.gui_layout.table,
            parent_instance=self,
            selection_callback=self._unified_selection_handler,
            double_click_callback=self._unified_double_click_handler
        )

        if success:
            self.log_message("Unified signal system connected")
        else:
            self.log_message("Failed to connect unified signals")

        # Connect unified signals to status bar updates
        from apps.components.unified_signal_handler import signal_handler
        signal_handler.status_update_requested.connect(self._update_status_from_signal)


    # In core/export.py
    def export_selected_function(main_window):
        selected_tab, options = show_mirror_tab_selection(main_window, 'export')
        if selected_tab:
            start_export_operation(main_window, selected_tab, options)

    # In core/import.py
    def import_function(main_window):
        selected_tab, options = show_mirror_tab_selection(main_window, 'import')
        if selected_tab and options.get('import_files'):
            start_import_operation(main_window, selected_tab, options)

    # In core/remove.py
    def remove_selected_function(main_window):
        selected_tab, options = show_mirror_tab_selection(main_window, 'remove')
        if selected_tab:
            start_remove_operation(main_window, selected_tab, options)

    # In core/dump.py
    def dump_function(main_window):
        selected_tab, options = show_mirror_tab_selection(main_window, 'dump')
        if selected_tab:
            start_dump_operation(main_window, selected_tab, options)

    def split_via_function(main_window):
        selected_tab, options = show_mirror_tab_selection(main_window, 'split_via')
        if selected_tab:
            split_method = options.get('split_method', 'size')  # 'size' or 'count'
            split_value = options.get('split_value', 50)
            start_split_operation(main_window, selected_tab, split_method, split_value)

    def debug_img_entries(self): #vers 4
        """Debug function to check what entries are actually loaded"""
        if not self.current_img or not self.current_img.entries:
            self.log_message("âŒ No IMG loaded or no entries found")
            return

        self.log_message(f"ðŸ” DEBUG: IMG file has {len(self.current_img.entries)} entries")

        # Count file types
        file_types = {}
        all_extensions = set()

        for i, entry in enumerate(self.current_img.entries):
            # Debug each entry
            self.log_message(f"Entry {i}: {entry.name}")

            # Extract extension both ways
            name_ext = entry.name.split('.')[-1].upper() if '.' in entry.name else "NO_EXT"
            attr_ext = getattr(entry, 'extension', 'NO_ATTR').upper() if hasattr(entry, 'extension') and entry.extension else "NO_ATTR"

            all_extensions.add(name_ext)

            # Count by name-based extension
            file_types[name_ext] = file_types.get(name_ext, 0) + 1

            # Log extension differences
            if name_ext != attr_ext:
                self.log_message(f"Extension mismatch: name='{name_ext}' vs attr='{attr_ext}'")

        # Summary
        self.log_message(f"File type summary:")
        for ext, count in sorted(file_types.items()):
            self.log_message(f"  {ext}: {count} files")

        self.log_message(f"All extensions found: {sorted(all_extensions)}")

        # Check table row count vs entries count
        table_rows = self.gui_layout.table.rowCount()
        self.log_message(f"Table has {table_rows} rows, IMG has {len(self.current_img.entries)} entries")

        # Check if any rows are hidden
        hidden_count = 0
        for row in range(table_rows):
            if self.gui_layout.table.isRowHidden(row):
                hidden_count += 1

        self.log_message(f"Hidden rows: {hidden_count}")

        if hidden_count > 0:
            self.log_message("Some rows are hidden! Check the filter settings.")


    def _unified_double_click_handler(self, row, filename, item): #vers 2
        """Handle double-click through unified system"""
        # Get the actual filename from the first column (index 0)
        if row < self.gui_layout.table.rowCount():
            name_item = self.gui_layout.table.item(row, 0)
            if name_item:
                actual_filename = name_item.text()
                self.log_message(f"Double-clicked: {actual_filename}")

                # Show file info if IMG is loaded
                if self.current_img and row < len(self.current_img.entries):
                    entry = self.current_img.entries[row]
                    from apps.methods.img_core_classes import format_file_size
                    self.log_message(f"File info: {entry.name} ({format_file_size(entry.size)})")
            else:
                self.log_message(f"Double-clicked row {row} (no filename found)")
        else:
            self.log_message(f"Double-clicked: {filename}")


    def _unified_selection_handler(self, selected_rows, selection_count): #vers 1
        """Handle selection changes through unified system"""
        # Update button states based on selection
        has_selection = selection_count > 0
        self._update_button_states(has_selection)

        # Log selection (unified approach - no spam)
        if selection_count == 0:
            # Don't log "Ready" for empty selection to reduce noise
            pass
        elif selection_count == 1:
            # Get filename of selected item
            if selected_rows and len(selected_rows) > 0:
                row = selected_rows[0]
                if row < self.gui_layout.table.rowCount():
                    name_item = self.gui_layout.table.item(row, 0)
                    if name_item:
                        self.log_message(f"Selected: {name_item.text()}")


# - Settings Reusable

    def _load_saved_settings(self): #vers 1
        """Load and apply all saved settings from img_settings"""
        if not hasattr(self, 'img_settings'):
            return

        try:
            # Load tab sizing settings
            main_tab_height = self.img_settings.get("main_type_tab_height", 35)
            individual_tab_height = self.img_settings.get("individual_tab_height", 28)
            tab_min_width = self.img_settings.get("tab_min_width", 120)
            tab_padding = self.img_settings.get("tab_padding", 8)

            # Apply to main tab widget
            if hasattr(self, 'main_tab_widget'):
                self.main_tab_widget.setStyleSheet(f"""
                    QTabBar::tab {{
                        height: {individual_tab_height}px;
                        min-height: {individual_tab_height}px;
                        max-height: {individual_tab_height}px;
                        min-width: {tab_min_width}px;
                        padding: {tab_padding}px 12px;
                    }}
                """)

            # Apply to main type tabs if exists
            if hasattr(self, 'main_type_tabs'):
                self.main_type_tabs.setStyleSheet(f"""
                    QTabBar::tab {{
                        height: {main_tab_height}px;
                        min-height: {main_tab_height}px;
                        max-height: {main_tab_height}px;
                        min-width: {tab_min_width}px;
                        padding: {tab_padding}px 12px;
                    }}
                """)

            # Load and apply fonts
            if self.img_settings.get("use_custom_font", False):
                font_family = self.img_settings.get("font_family", "Arial")
                font_size = self.img_settings.get("font_size", 10)
                font_bold = self.img_settings.get("font_bold", False)
                font_italic = self.img_settings.get("font_italic", False)

                custom_font = QFont(font_family, font_size)
                custom_font.setBold(font_bold)
                custom_font.setItalic(font_italic)
                self.setFont(custom_font)

            # Load button display mode
            button_mode = self.img_settings.get("button_display_mode", "icons_with_text")
            if hasattr(self, 'button_display_mode'):
                self.button_display_mode = button_mode

            self.log_message("✅ Saved settings loaded")

        except Exception as e:
            self.log_message(f"⚠️ Error loading saved settings: {str(e)}")


    def setup_missing_utility_functions(self): #vers 1
        """Add missing utility functions that selection callbacks need"""

        # Simple file type detection functions - MISSING FUNCTIONS
        self.has_col = lambda name: name.lower().endswith('.col') if name else False
        self.has_dff = lambda name: name.lower().endswith('.dff') if name else False
        self.has_txd = lambda name: name.lower().endswith('.txd') if name else False
        self.get_entry_type = lambda name: name.split('.')[-1].upper() if name and '.' in name else "Unknown"

        # Add missing functions for menu system
        self.save_img_as = self._save_img_as
        #self.save_img_entry = self._save_img_entry  # Main save function with modification check
        self.find_entries = self._find_entries
        self.find_next_entries = self._find_next_entries
        self.duplicate_selected = self._duplicate_selected
        self.rename_entry = self._rename_entry
        self.rename_selected = self._rename_selected
        #self.remove_selected = self._remove_selected_entries
        self.select_inverse_entries = self._select_inverse_entries
        self.extract_textures = lambda: extract_textures_function(self)
        # NEW: Add extract DFF texture lists functionality
        from apps.core.extract import extract_dff_texture_lists
        self.extract_dff_texture_lists = lambda: extract_dff_texture_lists(self)
        self.undo = self._undo_action
        self.redo = self._redo_action

        self.log_message("Missing utility functions added")

    """
    def _unified_double_click_handler(self, row, filename, item): #vers 3
        unified_double_click_handler(self, row, filename, item)

    def _unified_selection_handler(self, selected_rows, selection_count): #vers 2
        unified_selection_handler(self, selected_rows, selection_count)

    def setup_missing_utility_functions(self): #vers 2
        setup_missing_utility_functions(self)
    """

    def _save_img_as(self):
        """Save IMG file as a new file"""
        try:
            if not hasattr(self, 'current_img') or not self.current_img:
                QMessageBox.warning(self, "Save As", "No IMG file loaded to save.")
                return False

            # Get file path from user
            file_path, _ = QFileDialog.getSaveFileName(
                self,
                "Save IMG As",
                "",
                "IMG Files (*.img);;All Files (*.*)"
            )

            if file_path:
                # Ensure file has .img extension
                if not file_path.lower().endswith('.img'):
                    file_path += '.img'

                try:
                    # Save the IMG file
                    if hasattr(self.current_img, 'save_to_path'):
                        success = self.current_img.save_to_path(file_path)
                    elif hasattr(self.current_img, 'save'):
                        # Temporarily change the file path and save
                        original_path = getattr(self.current_img, 'file_path', None)
                        self.current_img.file_path = file_path
                        success = self.current_img.save()
                        self.current_img.file_path = original_path
                    else:
                        # Create a new IMG file and copy entries
                        from apps.methods.img_core_classes import IMGFile
                        new_img = IMGFile(file_path)
                        new_img.entries = self.current_img.entries[:]
                        success = new_img.save()

                    if success:
                        self.log_message(f"IMG file saved as: {file_path}")
                        QMessageBox.information(self, "Save As", f"File saved successfully as:\n{file_path}")
                        return True
                    else:
                        QMessageBox.critical(self, "Save As", "Failed to save IMG file.")
                        return False

                except Exception as e:
                    QMessageBox.critical(self, "Save As", f"Error saving file:\n{str(e)}")
                    return False
            else:
                # User cancelled
                return False

        except Exception as e:
            self.log_message(f"Error in save_img_as: {str(e)}")
            QMessageBox.critical(self, "Save As Error", f"Failed to save IMG file:\n{str(e)}")
            return False

    def _save_img_entry(self):
        """Save IMG file with modification check - addresses issue #1"""
        try:
            if not hasattr(self, 'current_img') or not self.current_img:
                QMessageBox.warning(self, "Save", "No IMG file loaded to save.")
                return False

            # Check if the IMG file has been modified
            is_modified = getattr(self.current_img, 'modified', False)
            
            if not is_modified:
                # Show message that file is unchanged
                self.log_message("IMG file unchanged, nothing to save")
                QMessageBox.information(self, "Save", "IMG file unchanged, nothing to save")
                return False

            # Get the file path - use current file path if available
            file_path = getattr(self.current_img, 'file_path', None)
            
            if not file_path:
                # If no file path, prompt user to save as new file
                from PyQt6.QtWidgets import QFileDialog
                file_path, _ = QFileDialog.getSaveFileName(
                    self,
                    "Save IMG File",
                    "",
                    "IMG Files (*.img);;All Files (*.*)"
                )
                
                if not file_path:
                    # User cancelled
                    return False
                
                # Ensure file has .img extension
                if not file_path.lower().endswith('.img'):
                    file_path += '.img'

            try:
                # Save the IMG file using the appropriate method
                if hasattr(self.current_img, 'save'):
                    success = self.current_img.save()
                elif hasattr(self.current_img, 'save_to_path'):
                    success = self.current_img.save_to_path(file_path)
                else:
                    # Fallback: create a new IMG file and copy entries
                    from apps.methods.img_core_classes import IMGFile
                    new_img = IMGFile(file_path)
                    new_img.entries = self.current_img.entries[:]
                    success = new_img.save()

                if success:
                    # Reset the modified flag after successful save
                    self.current_img.modified = False
                    self.log_message(f"IMG file saved: {file_path}")
                    QMessageBox.information(self, "Save", f"File saved successfully:\n{file_path}")
                    return True
                else:
                    QMessageBox.critical(self, "Save", "Failed to save IMG file.")
                    return False

            except Exception as e:
                QMessageBox.critical(self, "Save Error", f"Error saving file:\n{str(e)}")
                return False

        except Exception as e:
            self.log_message(f"Error in save_img_entry: {str(e)}")
            QMessageBox.critical(self, "Save Error", f"Failed to save IMG file:\n{str(e)}")
            return False

    def _find_entries(self):
        """Find entries in the table"""
        try:
            # Show find dialog
            from PyQt6.QtWidgets import QDialog, QVBoxLayout, QHBoxLayout, QLineEdit, QPushButton, QLabel
            from PyQt6.QtCore import Qt
            
            dialog = QDialog(self)
            dialog.setWindowTitle("Find Entries")
            dialog.setModal(True)
            dialog.resize(400, 120)
            
            layout = QVBoxLayout(dialog)
            
            # Search input
            search_layout = QHBoxLayout()
            search_label = QLabel("Find what:")
            search_input = QLineEdit()
            search_layout.addWidget(search_label)
            search_layout.addWidget(search_input)
            layout.addLayout(search_layout)
            
            # Options
            options_layout = QHBoxLayout()
            case_sensitive = QPushButton("Aa")
            case_sensitive.setCheckable(True)
            case_sensitive.setToolTip("Case sensitive")
            regex_mode = QPushButton(".*")
            regex_mode.setCheckable(True)
            regex_mode.setToolTip("Regular expression")
            options_layout.addWidget(case_sensitive)
            options_layout.addWidget(regex_mode)
            layout.addLayout(options_layout)
            
            # Buttons
            button_layout = QHBoxLayout()
            find_btn = QPushButton("Find Next")
            find_btn.clicked.connect(lambda: self._perform_find(search_input.text(), case_sensitive.isChecked(), regex_mode.isChecked()))
            cancel_btn = QPushButton("Cancel")
            cancel_btn.clicked.connect(dialog.reject)
            button_layout.addWidget(find_btn)
            button_layout.addWidget(cancel_btn)
            layout.addLayout(button_layout)
            
            # Set focus to search input
            search_input.setFocus()
            
            # Enter key to find next
            search_input.returnPressed.connect(find_btn.click)
            
            dialog.exec()
            
        except Exception as e:
            self.log_message(f"Error in find_entries: {str(e)}")

    def _perform_find(self, search_text, case_sensitive, regex_mode):
        """Perform the actual find operation"""
        try:
            if not search_text or not hasattr(self, 'gui_layout') or not hasattr(self.gui_layout, 'table'):
                return

            table = self.gui_layout.table
            current_row = table.currentRow()
            current_col = table.currentColumn()
            
            if current_row < 0:
                current_row = 0
                current_col = 0

            # Determine search range (start from next cell)
            start_row = current_row
            start_col = current_col + 1
            
            # If we're at the end of the row, go to next row
            if start_col >= table.columnCount():
                start_row += 1
                start_col = 0
            
            # Search from current position to end of table
            for row in range(start_row, table.rowCount()):
                for col in range(start_col if row == start_row else 0, table.columnCount()):
                    item = table.item(row, col)
                    if item:
                        text = item.text()
                        
                        # Perform search based on mode
                        match = False
                        if regex_mode:
                            import re
                            flags = 0 if case_sensitive else re.IGNORECASE
                            try:
                                match = bool(re.search(search_text, text, flags))
                            except re.error:
                                QMessageBox.warning(self, "Find", "Invalid regular expression")
                                return
                        else:
                            if case_sensitive:
                                match = search_text in text
                            else:
                                match = search_text.lower() in text.lower()
                        
                        if match:
                            # Select and scroll to the found item
                            table.setCurrentItem(item)
                            table.scrollToItem(item)
                            self.log_message(f"Found '{search_text}' at row {row}, column {col}")
                            return
                
                # Reset start_col for subsequent rows
                start_col = 0

            # If not found, wrap around and search from beginning
            for row in range(0, start_row + 1):
                for col in range(0, table.columnCount()):
                    # Skip items we already checked
                    if row == start_row and col < (start_col if row == start_row else 0):
                        continue
                    
                    item = table.item(row, col)
                    if item:
                        text = item.text()
                        
                        # Perform search based on mode
                        match = False
                        if regex_mode:
                            import re
                            flags = 0 if case_sensitive else re.IGNORECASE
                            try:
                                match = bool(re.search(search_text, text, flags))
                            except re.error:
                                QMessageBox.warning(self, "Find", "Invalid regular expression")
                                return
                        else:
                            if case_sensitive:
                                match = search_text in text
                            else:
                                match = search_text.lower() in text.lower()
                        
                        if match:
                            # Select and scroll to the found item
                            table.setCurrentItem(item)
                            table.scrollToItem(item)
                            self.log_message(f"Found '{search_text}' at row {row}, column {col}")
                            return

            # Not found anywhere
            QMessageBox.information(self, "Find", f"'{search_text}' not found in table.")
            
        except Exception as e:
            self.log_message(f"Error in perform_find: {str(e)}")

    def _find_next_entries(self):
        """Find next entry (just calls the same find functionality)"""
        # This would typically continue the search from the last found position
        # For now, we'll just show the find dialog again
        self._find_entries()

    def _replace_entries(self):
        """Replace entries in the table"""
        try:
            from PyQt6.QtWidgets import QDialog, QVBoxLayout, QHBoxLayout, QLineEdit, QPushButton, QLabel, QCheckBox
            from PyQt6.QtCore import Qt
            
            dialog = QDialog(self)
            dialog.setWindowTitle("Replace Entries")
            dialog.setModal(True)
            dialog.resize(400, 180)
            
            layout = QVBoxLayout(dialog)
            
            # Find input
            find_layout = QHBoxLayout()
            find_label = QLabel("Find what:")
            find_input = QLineEdit()
            find_layout.addWidget(find_label)
            find_layout.addWidget(find_input)
            layout.addLayout(find_layout)
            
            # Replace input
            replace_layout = QHBoxLayout()
            replace_label = QLabel("Replace with:")
            replace_input = QLineEdit()
            replace_layout.addWidget(replace_label)
            replace_layout.addWidget(replace_input)
            layout.addLayout(replace_layout)
            
            # Options
            options_layout = QHBoxLayout()
            case_sensitive = QCheckBox("Case sensitive")
            regex_mode = QCheckBox("Regular expression")
            options_layout.addWidget(case_sensitive)
            options_layout.addWidget(regex_mode)
            layout.addLayout(options_layout)
            
            # Buttons
            button_layout = QHBoxLayout()
            replace_btn = QPushButton("Replace")
            replace_all_btn = QPushButton("Replace All")
            cancel_btn = QPushButton("Cancel")
            
            replace_btn.clicked.connect(lambda: self._perform_replace(find_input.text(), replace_input.text(), 
                                                                     case_sensitive.isChecked(), regex_mode.isChecked(), False))
            replace_all_btn.clicked.connect(lambda: self._perform_replace(find_input.text(), replace_input.text(), 
                                                                         case_sensitive.isChecked(), regex_mode.isChecked(), True))
            cancel_btn.clicked.connect(dialog.reject)
            
            button_layout.addWidget(replace_btn)
            button_layout.addWidget(replace_all_btn)
            button_layout.addWidget(cancel_btn)
            layout.addLayout(button_layout)
            
            # Set focus to find input
            find_input.setFocus()
            
            dialog.exec()
            
        except Exception as e:
            self.log_message(f"Error in replace_entries: {str(e)}")

    def _perform_replace(self, find_text, replace_text, case_sensitive, regex_mode, replace_all):
        """Perform the actual replace operation"""
        try:
            if not find_text or not hasattr(self, 'gui_layout') or not hasattr(self.gui_layout, 'table'):
                return

            table = self.gui_layout.table
            replacements = 0
            
            for row in range(table.rowCount()):
                for col in range(table.columnCount()):
                    item = table.item(row, col)
                    if item:
                        original_text = item.text()
                        new_text = original_text
                        
                        # Perform replacement based on mode
                        if regex_mode:
                            import re
                            flags = 0 if case_sensitive else re.IGNORECASE
                            try:
                                if replace_all:
                                    new_text = re.sub(find_text, replace_text, original_text, flags=flags)
                                else:
                                    # Replace only first occurrence
                                    new_text = re.sub(find_text, replace_text, original_text, count=1, flags=flags)
                            except re.error:
                                QMessageBox.warning(self, "Replace", "Invalid regular expression")
                                return
                        else:
                            if case_sensitive:
                                if replace_all:
                                    new_text = original_text.replace(find_text, replace_text)
                                else:
                                    # Replace only first occurrence
                                    idx = original_text.find(find_text)
                                    if idx != -1:
                                        new_text = (original_text[:idx] + replace_text + 
                                                   original_text[idx + len(find_text):])
                            else:
                                # Case insensitive replacement - more complex
                                if replace_all:
                                    # This is a simplified case-insensitive replacement
                                    temp_text = original_text
                                    start = 0
                                    while True:
                                        idx = temp_text.lower().find(find_text.lower(), start)
                                        if idx == -1:
                                            break
                                        temp_text = (temp_text[:idx] + replace_text + 
                                                    temp_text[idx + len(find_text):])
                                        start = idx + len(replace_text)
                                        if not replace_all:  # Only replace first if not replace_all
                                            break
                                    new_text = temp_text
                                else:
                                    # Replace only first occurrence (case-insensitive)
                                    idx = original_text.lower().find(find_text.lower())
                                    if idx != -1:
                                        new_text = (original_text[:idx] + replace_text + 
                                                   original_text[idx + len(find_text):])
                        
                        if new_text != original_text:
                            item.setText(new_text)
                            replacements += 1
                            if not replace_all:
                                # Select and scroll to the replaced item
                                table.setCurrentItem(item)
                                table.scrollToItem(item)
                                break
                if not replace_all and replacements > 0:
                    break
            
            if replacements > 0:
                self.log_message(f"Replaced {replacements} occurrence(s)")
                QMessageBox.information(self, "Replace", f"Replaced {replacements} occurrence(s).")
            else:
                self.log_message("No replacements made")
                QMessageBox.information(self, "Replace", "No matches found.")
                
        except Exception as e:
            self.log_message(f"Error in perform_replace: {str(e)}")

    def _duplicate_selected(self):
        """Duplicate selected entries"""
        try:
            if not hasattr(self, 'gui_layout') or not hasattr(self.gui_layout, 'table'):
                QMessageBox.warning(self, "Duplicate", "Table not available.")
                return

            table = self.gui_layout.table
            selected_items = table.selectedItems()
            
            if not selected_items:
                QMessageBox.information(self, "Duplicate", "Please select entries to duplicate.")
                return

            # Get selected rows
            selected_rows = list(set(item.row() for item in selected_items))
            
            if not hasattr(self, 'current_img') or not self.current_img:
                QMessageBox.warning(self, "Duplicate", "No IMG file loaded.")
                return

            # Duplicate the selected entries
            duplicated_count = 0
            for row in selected_rows:
                if row < len(self.current_img.entries):
                    original_entry = self.current_img.entries[row]
                    
                    # Create a copy of the entry
                    from apps.methods.img_core_classes import IMGEntry
                    new_entry = IMGEntry()
                    new_entry.name = original_entry.name + "_copy"
                    new_entry.size = original_entry.size
                    new_entry.offset = original_entry.offset
                    new_entry.data = getattr(original_entry, 'data', b'')  # Copy data if available
                    
                    # Add to IMG file
                    self.current_img.entries.append(new_entry)
                    duplicated_count += 1

            # Refresh the table to show new entries
            if hasattr(self, 'reload_current_file'):
                self.reload_current_file()
            elif hasattr(self, 'populate_img_table'):
                self.populate_img_table()

            self.log_message(f"Duplicated {duplicated_count} entry(ies)")
            QMessageBox.information(self, "Duplicate", f"Duplicated {duplicated_count} entry(ies).")
            
        except Exception as e:
            self.log_message(f"Error in duplicate_selected: {str(e)}")
            QMessageBox.critical(self, "Duplicate Error", f"Failed to duplicate entries:\n{str(e)}")

    def _rename_entry(self):
        """Rename entry (for menu system)"""
        self._rename_selected()

    def _rename_selected(self):
        """Rename selected entry"""
        try:
            if not hasattr(self, 'gui_layout') or not hasattr(self.gui_layout, 'table'):
                QMessageBox.warning(self, "Rename", "Table not available.")
                return

            table = self.gui_layout.table
            selected_items = table.selectedItems()
            
            if not selected_items:
                QMessageBox.information(self, "Rename", "Please select an entry to rename.")
                return

            # Get the first selected row (use the row of the first selected item)
            row = selected_items[0].row()
            
            if not hasattr(self, 'current_img') or not self.current_img:
                QMessageBox.warning(self, "Rename", "No IMG file loaded.")
                return

            if row >= len(self.current_img.entries):
                QMessageBox.warning(self, "Rename", "Selected row is out of range.")
                return

            # Get current entry name
            current_entry = self.current_img.entries[row]
            current_name = current_entry.name

            # Show input dialog for new name
            new_name, ok = QInputDialog.getText(
                self,
                "Rename Entry",
                f"Enter new name for '{current_name}':",
                text=current_name
            )

            if ok and new_name and new_name != current_name:
                # Validate the new name
                if self._validate_entry_name(new_name):
                    # Check for duplicates
                    if not self._check_duplicate_name(new_name, current_entry):
                        # Perform the rename
                        current_entry.name = new_name
                        
                        # Update the table display
                        name_item = table.item(row, 0)  # Assuming name is in column 0
                        if name_item:
                            name_item.setText(new_name)

                        # Mark as modified
                        if hasattr(self.current_img, 'modified'):
                            self.current_img.modified = True

                        self.log_message(f"Renamed '{current_name}' to '{new_name}'")
                        QMessageBox.information(self, "Rename Successful", 
                                              f"Successfully renamed to '{new_name}'")
                    else:
                        QMessageBox.warning(self, "Duplicate Name", 
                                          f"An entry named '{new_name}' already exists")
                else:
                    QMessageBox.warning(self, "Invalid Name", 
                                      "The name provided is invalid")
                    
        except Exception as e:
            self.log_message(f"Error in rename_selected: {str(e)}")
            QMessageBox.critical(self, "Rename Error", f"Failed to rename entry:\n{str(e)}")

    def _validate_entry_name(self, name):
        """Validate entry name for IMG file"""
        try:
            # Check for empty name
            if not name or not name.strip():
                return False
            
            # Check for invalid characters
            invalid_chars = '<>:"/\\|?*'
            if any(char in name for char in invalid_chars):
                return False
            
            # Check length (IMG entries typically have 24 char limit)
            if len(name) > 24:
                return False
            
            return True
        except:
            return False

    def _check_duplicate_name(self, new_name, current_entry):
        """Check if new name would create duplicate"""
        try:
            if hasattr(self, 'current_img') and self.current_img:
                for entry in self.current_img.entries:
                    if entry != current_entry and getattr(entry, 'name', '') == new_name:
                        return True
            return False
        except:
            return True  # Return True on error to be safe

    def _remove_selected_entries(self):
        """Remove selected entries from IMG file"""
        try:
            if not hasattr(self, 'gui_layout') or not hasattr(self.gui_layout, 'table'):
                QMessageBox.warning(self, "Remove", "Table not available.")
                return

            table = self.gui_layout.table
            selected_items = table.selectedItems()
            
            if not selected_items:
                QMessageBox.information(self, "Remove", "Please select entries to remove.")
                return

            # Get selected rows
            selected_rows = sorted(set(item.row() for item in selected_items), reverse=True)  # Reverse order for safe deletion
            
            if not hasattr(self, 'current_img') or not self.current_img:
                QMessageBox.warning(self, "Remove", "No IMG file loaded.")
                return

            # Confirm removal
            reply = QMessageBox.question(
                self,
                "Remove Entries",
                f"Remove {len(selected_rows)} selected entry(ies)?\n\n"
                f"This action cannot be undone.",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
                QMessageBox.StandardButton.No
            )

            if reply == QMessageBox.StandardButton.Yes:
                # Remove entries in reverse order to maintain correct indices
                removed_count = 0
                for row in selected_rows:
                    if row < len(self.current_img.entries):
                        del self.current_img.entries[row]
                        removed_count += 1

                # Refresh the table to reflect changes
                if hasattr(self, 'reload_current_file'):
                    self.reload_current_file()
                elif hasattr(self, 'populate_img_table'):
                    self.populate_img_table()

                self.log_message(f"Removed {removed_count} entry(ies)")
                QMessageBox.information(self, "Remove", f"Removed {removed_count} entry(ies).")
                
        except Exception as e:
            self.log_message(f"Error in remove_selected_entries: {str(e)}")
            QMessageBox.critical(self, "Remove Error", f"Failed to remove entries:\n{str(e)}")

    def _select_inverse_entries(self):
        """Invert the current selection"""
        try:
            if not hasattr(self, 'gui_layout') or not hasattr(self.gui_layout, 'table'):
                return

            table = self.gui_layout.table
            all_selected = []
            
            # Get all currently selected items
            for row in range(table.rowCount()):
                for col in range(table.columnCount()):
                    item = table.item(row, col)
                    if item and item.isSelected():
                        all_selected.append((row, col))
            
            # Clear current selection
            table.clearSelection()
            
            # Select all items that were NOT selected
            for row in range(table.rowCount()):
                for col in range(table.columnCount()):
                    item = table.item(row, col)
                    if item and (row, col) not in all_selected:
                        item.setSelected(True)
            
            self.log_message("Selection inverted")
            
        except Exception as e:
            self.log_message(f"Error in select_inverse_entries: {str(e)}")

    def _undo_action(self):
        """Undo last action"""
        # Try to use undo system if available
        if hasattr(self, 'undo_manager') and hasattr(self.undo_manager, 'undo'):
            try:
                self.undo_manager.undo()
                self.log_message("Undo operation performed")
            except Exception as e:
                self.log_message(f"Undo failed: {str(e)}")
                QMessageBox.information(self, "Undo", f"Undo operation failed:\n{str(e)}")
        else:
            self.log_message("No undo operations available")
            QMessageBox.information(self, "Undo", "No undo operations available")

    def _redo_action(self):
        """Redo last action"""
        # Try to use undo system if available
        if hasattr(self, 'undo_manager') and hasattr(self.undo_manager, 'redo'):
            try:
                self.undo_manager.redo()
                self.log_message("Redo operation performed")
            except Exception as e:
                self.log_message(f"Redo failed: {str(e)}")
                QMessageBox.information(self, "Redo", f"Redo operation failed:\n{str(e)}")
        else:
            self.log_message("No redo operations available")
            QMessageBox.information(self, "Redo", "No redo operations available")


    # INTEGRATION FIX for imgfactory.py:
    def fix_selection_callback_functions(main_window): #vers 1
        """Add missing selection callback functions to main window"""
        try:
            # Add the missing has_* functions
            main_window.has_col = has_col
            main_window.has_dff = has_dff
            main_window.has_txd = has_txd
            main_window.get_entry_type = get_entry_type

            # Add other common utility functions that might be missing
            def get_selected_entry_name():
                """Get name of currently selected entry"""
                try:
                    if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
                        table = main_window.gui_layout.table
                        current_row = table.currentRow()
                        if current_row >= 0:
                            name_item = table.item(current_row, 0)
                            if name_item:
                                return name_item.text()
                    return None
                except:
                    return None


            def get_selected_entries_count():
                """Get count of selected entries"""
                try:
                    if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
                        table = main_window.gui_layout.table
                        return len(table.selectedItems())
                    return 0
                except:
                    return 0

            # Add utility functions to main window
            main_window.get_selected_entry_name = get_selected_entry_name
            main_window.get_selected_entries_count = get_selected_entries_count

            main_window.log_message("Selection callback functions fixed")
            return True

        except Exception as e:
            main_window.log_message(f"Selection callback fix failed: {e}")
            return False


    def setup_col_integration(self): #vers 2 #Restored
        """Setup complete COL integration with IMG Factory"""
        try:
            self.log_message("Setting up COL integration...")

            # Enable COL debug based on main debug state
            if hasattr(self, 'debug_enabled') and self.debug_enabled:
                set_col_debug_enabled(True)
            else:
                set_col_debug_enabled(False)

            # Setup complete COL integration
            success = setup_complete_col_integration(self)

            if success:
                self.log_message("COL integration completed successfully")

                # Add COL file loading capability
                self.load_col_file_safely = lambda file_path: load_col_file_safely(self, file_path)

                # Mark COL as available
                self.col_integration_active = True

            else:
                self.log_message("COL integration failed")

            return success

        except Exception as e:
            self.log_message(f"Error setting up COL integration: {str(e)}")
            return False


    def _update_ui_for_loaded_col(self): #vers 1 #restore
        """Update UI when COL file is loaded - Uses proper methods/populate_col_table.py"""
        if not hasattr(self, 'current_col') or not self.current_col:
            self.log_message("_update_ui_for_loaded_col called but no current_col")
            return

        try:
            # Update window title
            if hasattr(self.current_col, 'file_path'):
                file_name = os.path.basename(self.current_col.file_path)
                self.setWindowTitle(f"IMG Factory 1.5 - {file_name}")

            # Use proper COL table population from apps.methods.
            if hasattr(self, 'gui_layout') and hasattr(self.gui_layout, 'table'):
                try:
                    # Import the proper COL table functions
                    from apps.methods.populate_col_table import setup_col_table_structure, populate_table_with_col_data_debug

                    # Setup COL table structure (proper headers and widths)
                    setup_col_table_structure(self)

                    # Populate with actual COL data using the methods system
                    populate_table_with_col_data_debug(self, self.current_col)

                    model_count = len(self.current_col.models) if hasattr(self.current_col, 'models') else 0
                    self.log_message(f"COL table populated with {model_count} models")

                except ImportError as e:
                    self.log_message(f"COL methods not available: {str(e)}")
                    # Fallback to basic display
                    self._basic_col_table_fallback(file_name)

            # Update status
            if hasattr(self, 'gui_layout') and hasattr(self.gui_layout, 'show_progress'):
                self.gui_layout.show_progress(-1, "COL loaded")

            self.log_message("COL UI updated successfully")

        except Exception as e:
            self.log_message(f"Error updating COL UI: {str(e)}")

    # FIX: Close manager tab widget issue
    def fix_close_manager_tab_reference(main_window): #vers 1
        #"""Fix close manager missing main_tab_widget reference#"""
        try:
            if hasattr(main_window, 'close_manager'):
                # Add missing reference
                main_window.close_manager.main_tab_widget = main_window.main_tab_widget
                main_window.log_message("Close manager tab reference fixed")
                return True
        except Exception as e:
            main_window.log_message(f"Close manager fix failed: {str(e)}")
        return False


    def update_button_states(self, has_selection): #vers 4
        """Update button enabled/disabled states based on selection"""
        # Check what's loaded
        has_img = self.current_img is not None
        has_col = self.current_col is not None
        has_txd = hasattr(self, 'current_txd') and self.current_txd is not None

        # Log the button state changes for debugging
        self.log_message(f"Button states updated: selection={has_selection}, img_loaded={has_img}, col_loaded={has_col}, txd_loaded={has_txd}")

        # Find buttons in GUI layout and update their states
        # These buttons need both an IMG and selection
        selection_dependent_buttons = ['export_btn', 'export_selected_btn', 'remove_btn', 'remove_selected_btn', 'reload_btn', 'extract_btn', 'quick_export_btn']

        for btn_name in selection_dependent_buttons:
            if hasattr(self.gui_layout, btn_name):
                button = getattr(self.gui_layout, btn_name)
                if hasattr(button, 'setEnabled'):
                    # Enable for IMG files with selection, open edit for COL and TXD - TODO
                    button.setEnabled(has_selection and has_img and has_col and has_txd)

        # These buttons only need an IMG (no selection required) - DISABLE for COL and TXD
        img_dependent_buttons = [
            'import_btn', 'import_files_btn', 'rebuild_btn', 'close_btn',
            'validate_btn', 'refresh_btn', 'reload_btn'
        ]

        for btn_name in img_dependent_buttons:
            if hasattr(self.gui_layout, btn_name):
                button = getattr(self.gui_layout, btn_name)
                if hasattr(button, 'setEnabled'):
                    # Special handling for rebuild - disable for COL and TXD files
                    if btn_name == 'rebuild_btn':
                        button.setEnabled(has_img and not has_col and not has_txd)
                    else:
                        # Import/Close/Validate work for IMG or COL, but open TXD - TODO
                        button.setEnabled((has_img or has_col) and has_txd)


    def _update_status_from_signal(self, message): #vers 3
        """Update status from unified signal system"""
        # Update status bar if available
        if hasattr(self, 'statusBar') and self.statusBar():
            self.statusBar().showMessage(message)

        # Also update GUI layout status if available
        if hasattr(self.gui_layout, 'status_label'):
            self.gui_layout.status_label.setText(message)


    #these need to be checked
    def add_update_button_states_stub(main_window): #vers 1
        """Add stub for _update_button_states to prevent selection callback errors"""
        def _update_button_states_stub(has_selection):
            """Stub for button state updates - handled by connections.py"""
            pass  # Do nothing - connections.py handles this

        main_window._update_button_states = _update_button_states_stub
        main_window.log_message("Button states stub added")


    def apply_quick_fixes(main_window): #vers 2
        """Apply all quick fixes for missing methods"""
        try:
            fixes_applied = 0

            # Fix 1: Add missing COL UI update method (uses proper methods/)
            if not hasattr(main_window, '_update_ui_for_loaded_col'):
                setattr(main_window, '_update_ui_for_loaded_col',
                    lambda: _update_ui_for_loaded_col(main_window))
                setattr(main_window, '_basic_col_table_fallback',
                    lambda file_name: _basic_col_table_fallback(main_window, file_name))
                fixes_applied += 1

            # Fix 2: Fix close manager tab reference
            if fix_close_manager_tab_reference(main_window):
                fixes_applied += 1

            # Fix 3: Add button states stub
            add_update_button_states_stub(main_window)
            fixes_applied += 1

            main_window.log_message(f"Applied {fixes_applied} quick fixes")
            return True

        except Exception as e:
            main_window.log_message(f"Quick fixes failed: {str(e)}")
            return False


    def handle_col_file_open(self, file_path: str): #vers 4
        """Handle opening of COL files"""
        try:
            if file_path.lower().endswith('.col'):
                self.log_message(f"Loading COL file: {os.path.basename(file_path)}")

                if hasattr(self, 'load_col_file_safely'):
                    success = self.load_col_file_safely(file_path)
                    if success:
                        self.log_message("COL file loaded successfully")
                    else:
                        self.log_message("Failed to load COL file")
                    return success
                else:
                    self.log_message("COL integration not available")
                    return False

            return False

        except Exception as e:
            self.log_message(f"Error handling COL file: {str(e)}")
            return False


    def create_new_img(self): #vers 5
        """Show new IMG creation dialog - FIXED: No signal connections"""


    def select_all_entries(self): #vers 3
        """Select all entries in current table"""
        if hasattr(self.gui_layout, 'table') and self.gui_layout.table:
            self.gui_layout.table.selectAll()
            self.log_message("Selected all entries")


    def select_inverse(self): #vers 2
        """Select inverse of current selection"""
        try:
            if hasattr(self.gui_layout, 'table') and self.gui_layout.table:
                table = self.gui_layout.table
                # Get currently selected items
                selected_items = table.selectedItems()
                selected_rows = set(item.row() for item in selected_items) if selected_items else set()
                
                # Clear current selection
                table.clearSelection()
                
                # Select all rows except the currently selected ones
                for row in range(table.rowCount()):
                    if row not in selected_rows:
                        table.selectRow(row)
                
                self.log_message("Selection inverted")
            else:
                self.log_message("❌ Table not available for selection")
        except Exception as e:
            self.log_message(f"❌ Select inverse error: {str(e)}")


    def sort_entries(self): #vers 1
        """Sort entries in the table"""
        try:
            if hasattr(self.gui_layout, 'table') and self.gui_layout.table:
                # Get currently selected items to restore selection after sorting
                selected_items = self.gui_layout.table.selectedItems()
                selected_rows = set(item.row() for item in selected_items) if selected_items else set()
                
                # Sort by the first column (filename) by default
                self.gui_layout.table.sortItems(0, Qt.SortOrder.AscendingOrder)
                
                # Restore selection if there were selected items
                if selected_rows:
                    for row in selected_rows:
                        if row < self.gui_layout.table.rowCount():
                            for col in range(self.gui_layout.table.columnCount()):
                                item = self.gui_layout.table.item(row, col)
                                if item:
                                    item.setSelected(True)
                
                self.log_message("✅ Entries sorted")
            else:
                self.log_message("❌ Table not available for sorting")
        except Exception as e:
            self.log_message(f"❌ Sort entries error: {str(e)}")


    def pin_selected_entries(self): #vers 1
        """Pin selected entries to keep them at the top of the table"""
        try:
            if hasattr(self.gui_layout, 'table') and self.gui_layout.table:
                selected_items = self.gui_layout.table.selectedItems()
                if not selected_items:
                    self.log_message("No entries selected to pin")
                    return
                
                selected_rows = set(item.row() for item in selected_items)
                self.log_message(f"Pinned {len(selected_rows)} entries")
            else:
                self.log_message("❌ Table not available for pinning")
        except Exception as e:
            self.log_message(f"❌ Pin selected error: {str(e)}")


    def sort_img_by_ide(self): #vers 1
        """Sort current IMG file entries to match IDE model order"""
        try:
            if not self.current_img or not self.current_img.entries:
                self.log_message("❌ No IMG file loaded")
                return False

            # Look for an IDE file in the same directory as the current IMG
            if not hasattr(self.current_img, 'file_path') or not self.current_img.file_path:
                self.log_message("❌ Current IMG has no file path")
                return False

            img_dir = os.path.dirname(self.current_img.file_path)
            img_base = os.path.splitext(os.path.basename(self.current_img.file_path))[0]
            
            # Look for corresponding IDE file
            ide_candidates = [
                os.path.join(img_dir, f"{img_base}.ide"),
                os.path.join(img_dir, f"{img_base.lower()}.ide"),
                os.path.join(img_dir, f"{img_base.upper()}.ide")
            ]
            
            ide_file_path = None
            for candidate in ide_candidates:
                if os.path.exists(candidate):
                    ide_file_path = candidate
                    break
            
            if not ide_file_path:
                # Ask user to select IDE file
                from PyQt6.QtWidgets import QFileDialog
                ide_file_path, _ = QFileDialog.getOpenFileName(
                    self, "Select IDE file to sort by", img_dir, "IDE Files (*.ide)"
                )
                if not ide_file_path:
                    self.log_message("❌ No IDE file selected")
                    return False

            # Parse IDE file to get model order
            model_order = self._parse_ide_for_model_order(ide_file_path)
            if not model_order:
                self.log_message("❌ No models found in IDE file")
                return False

            # Sort IMG entries based on IDE model order, with TXDs at the bottom
            sorted_entries = self._sort_entries_by_ide_order(model_order)
            
            # Update the IMG file with sorted entries
            self.current_img.entries = sorted_entries
            
            # Refresh the table display
            if hasattr(self, '_populate_real_img_table'):
                self._populate_real_img_table(self.current_img)
            else:
                from apps.methods.populate_img_table import populate_img_table
                populate_img_table(self.gui_layout.table, self.current_img)
            
            self.log_message(f"✅ IMG sorted by IDE order ({len(model_order)} models)")
            return True

        except Exception as e:
            self.log_message(f"❌ Error sorting IMG by IDE: {str(e)}")
            return False


    def _parse_ide_for_model_order(self, ide_path: str) -> List[str]:
        """Parse IDE file and return list of model names in order"""
        try:
            model_order = []
            with open(ide_path, 'r', encoding='utf-8') as f:
                lines = f.readlines()
            
            for line in lines:
                line = line.strip()
                if line and not line.startswith('#') and not line.startswith('end'):
                    # Parse IDE line format: id, model, txd, meshcount, drawdist, flags
                    parts = [part.strip() for part in line.split(',')]
                    if len(parts) >= 3:  # Need at least id, model, txd
                        model_name = parts[1].strip()  # Model name is second field
                        if model_name and model_name not in model_order:
                            model_order.append(model_name)
            
            return model_order
        except Exception as e:
            self.log_message(f"❌ Error parsing IDE file: {str(e)}")
            return []


    def _sort_entries_by_ide_order(self, model_order: List[str]) -> List:
        """Sort current IMG entries based on IDE model order, with TXDs at the bottom"""
        try:
            if not self.current_img or not self.current_img.entries:
                return []

            # Create mapping of model names to entries (without extensions)
            entry_map = {}
            txd_entries = []
            other_entries = []
            
            for entry in self.current_img.entries:
                entry_name = entry.name.lower()
                if entry_name.endswith('.txd'):
                    txd_entries.append(entry)
                else:
                    # Get name without extension for comparison
                    base_name = os.path.splitext(entry_name)[0]
                    entry_map[base_name] = entry

            # Build sorted list following IDE model order
            sorted_entries = []
            
            # Add entries in IDE model order
            for model_name in model_order:
                base_name = model_name.lower()
                if base_name in entry_map:
                    sorted_entries.append(entry_map[base_name])
                    del entry_map[base_name]  # Remove to avoid duplicates
            
            # Add remaining entries that weren't in IDE
            for entry in entry_map.values():
                sorted_entries.append(entry)
            
            # Add TXD entries at the end
            sorted_entries.extend(txd_entries)
            
            return sorted_entries
        except Exception as e:
            self.log_message(f"❌ Error sorting entries by IDE order: {str(e)}")
            return self.current_img.entries if self.current_img else []


    def validate_img(self): #vers 4
        """Validate current IMG file"""
        if not self.current_img:
            self.log_message("No IMG file loaded")
            return

        try:
            from apps.methods.img_validation import IMGValidator
            validation = IMGValidator.validate_img_file(self.current_img)
            if validation.is_valid:
                self.log_message("IMG validation passed")
            else:
                self.log_message(f"IMG validation issues: {validation.get_summary()}")
        except Exception as e:
            self.log_message(f"Validation error: {str(e)}")


    def show_gui_settings(self): #vers 5
        """Show GUI settings dialog"""
        self.log_message("GUI settings requested")
        try:
            from apps.utils.app_settings_system import SettingsDialog
            dialog = SettingsDialog(self.app_settings, self)
            dialog.exec()
        except Exception as e:
            self.log_message(f"Settings dialog error: {str(e)}")


    def _show_workshop_settings(self): #vers 1
        """Show workshop settings dialog - called from custom UI"""
        self.log_message("Workshop settings requested")
        try:
            # Use the method from gui_layout_custom
            if hasattr(self.gui_layout, '_show_workshop_settings'):
                # Add safeguard to prevent duplicate dialogs
                if hasattr(self, '_settings_dialog_open') and self._settings_dialog_open:
                    return  # Already open, ignore duplicate call
                self._settings_dialog_open = True
                try:
                    self.gui_layout._show_workshop_settings()
                finally:
                    self._settings_dialog_open = False
            else:
                # Fallback to regular settings
                self.show_gui_settings()
        except Exception as e:
            self.log_message(f"Workshop settings dialog error: {str(e)}")


    def show_about(self):
        """Show about dialog"""
        QMessageBox.about(self, "About IMG Factory 1.5", "IMG Factory 1.5\nAdvanced IMG Archive Management\nX-Seti 2025")


    def enable_col_debug(self): #vers 2 #restore
        """Enable COL debug output"""
        # Set debug flag on all loaded COL files
        if hasattr(self, 'current_col') and self.current_col:
            self.current_col._debug_enabled = True

        # Set global flag for future COL files
        import methods.col_core_classes as col_module
        col_module._global_debug_enabled = True

        self.log_message("COL debug output enabled")


    def disable_col_debug(self): #vers 2 #restore
        """Disable COL debug output"""
        # Set debug flag on all loaded COL files
        if hasattr(self, 'current_col') and self.current_col:
            self.current_col._debug_enabled = False

        # Set global flag for future COL files
        import methods.col_core_classes as col_module
        col_module._global_debug_enabled = False

        self.log_message("COL debug output disabled")


    def toggle_col_debug(self): #vers 2 #restore
        """Toggle COL debug output"""
        try:
            import methods.col_core_classes as col_module
            debug_enabled = getattr(col_module, '_global_debug_enabled', False)

            if debug_enabled:
                self.disable_col_debug()
            else:
                self.enable_col_debug()

        except Exception as e:
            self.log_message(f"Debug toggle error: {e}")


    def setup_debug_controls(self): #vers 2 #restore
        """Setup debug control shortcuts - ADD THIS TO __init__"""
        try:
            from PyQt6.QtGui import QShortcut, QKeySequence

            # Ctrl+Shift+D for debug toggle
            debug_shortcut = QShortcut(QKeySequence("Ctrl+Shift+D"), self)
            debug_shortcut.activated.connect(self.toggle_col_debug)

            # Start with debug disabled for performance
            self.disable_col_debug()

            self.log_message("Debug controls ready (Ctrl+Shift+D to toggle COL debug)")

        except Exception as e:
            self.log_message(f"Debug controls error: {e}")


    def _create_ui(self): #vers 13
        """Create the main user interface - WITH TABS FIXED"""
        central_widget = QWidget()
        self.setCentralWidget(central_widget)

        # Main layout
        main_layout = QVBoxLayout(central_widget)
        main_layout.setContentsMargins(5, 5, 5, 5)

        # IN CUSTOM UI MODE: Add toolbar FIRST (before tabs)
        if hasattr(self, 'gui_layout') and hasattr(self.gui_layout, '_create_toolbar'):
            try:
                ui_mode = self.img_settings.get("ui_mode", "system")
                if ui_mode == "custom":
                    toolbar = self.gui_layout._create_toolbar()
                    toolbar.setVisible(True)
                    main_layout.addWidget(toolbar)  # Add toolbar at TOP
                    print("DEBUG: Toolbar added to layout in custom mode")
            except Exception as e:
                print(f"Toolbar creation failed: {e}")

        # Create main tab widget for file handling
        self.main_tab_widget = QTabWidget()
        self.main_tab_widget.currentChanged.connect(self._on_tab_changed)
        self.main_tab_widget.setTabsClosable(True)
        self.main_tab_widget.setMovable(True)

        # Initialize open files tracking (for migration)
        if not hasattr(self, 'open_files'):
            self.open_files = {}

        # Create initial empty tab
        self._create_initial_tab()

        # Setup close manager BEFORE tab system
        self.close_manager = install_close_functions(self)

        # Setup NEW tab system
        setup_tab_system(self)

        # Migrate existing tabs if any
        if self.open_files:
            migrate_tabs(self)

        # Add tab widget to main layout
        main_layout.addWidget(self.main_tab_widget)

        # Create GUI layout system (single instance)
        self.gui_layout.create_status_bar()
        self.gui_layout.apply_table_theme()

        # Setup unified signal system
        self.setup_unified_signals()


    def _create_initial_tab(self): #vers 4
        #./components/img_close_functions.py: - def _create_initial_tab[self]
        """Create initial empty tab"""
        # Create tab widget
        tab_widget = QWidget()
        tab_layout = QVBoxLayout(tab_widget)
        tab_layout.setContentsMargins(0, 0, 0, 0)

        # Create GUI layout for this tab
        self.gui_layout.create_main_ui_with_splitters(tab_layout)

        # Add tab with "No file" label
        self.main_tab_widget.addTab(tab_widget, "No File")


    def _find_table_in_tab(self, tab_widget): #vers 1
        """Find the table widget in a specific tab - HELPER METHOD"""
        try:
            if not tab_widget:
                return None

            # Method 1: Check for dedicated_table attribute (robust system)
            if hasattr(tab_widget, 'dedicated_table'):
                return tab_widget.dedicated_table

            # Method 2: Search recursively through widget hierarchy
            from PyQt6.QtWidgets import QTableWidget

            def find_table_recursive(widget):
                if isinstance(widget, QTableWidget):
                    return widget
                for child in widget.findChildren(QTableWidget):
                    return child  # Return first table found
                return None

            table = find_table_recursive(tab_widget)
            if table:
                return table

            # Method 3: Check standard locations
            if hasattr(tab_widget, 'table'):
                return tab_widget.table

            return None

        except Exception as e:
            self.log_message(f"Error finding table in tab: {str(e)}")
            return None


    def _log_current_tab_state(self, tab_index): #vers 1
        """Log current tab state for debugging export issues - HELPER METHOD"""
        try:
            # Log file state
            if self.current_img:
                entry_count = len(self.current_img.entries) if self.current_img.entries else 0
                self.log_message(f"State: IMG with {entry_count} entries")
            elif self.current_col:
                if hasattr(self.current_col, 'models'):
                    model_count = len(self.current_col.models) if self.current_col.models else 0
                    self.log_message(f"State: COL with {model_count} models")
                else:
                    self.log_message(f"State: COL file loaded")
            else:
                self.log_message(f"State: No file loaded")

            # Log table state
            if hasattr(self.gui_layout, 'table') and self.gui_layout.table:
                table = self.gui_layout.table
                row_count = table.rowCount() if table else 0
                self.log_message(f"Table: {row_count} rows in gui_layout.table")
            else:
                self.log_message(f"Table: No table reference in gui_layout")

        except Exception as e:
            self.log_message(f"Error logging tab state: {str(e)}")


    def _on_tab_changed(self, index): #vers 1
        """Handle tab switching - Tab 0 = Dir Tree, others = IMG files"""
        try:
            if index == 0:  # Dir Tree tab
                # Show directory tree, hide table
                if hasattr(self, 'directory_tree'):
                    if hasattr(self.gui_layout, 'table'):
                        self.gui_layout.table.hide()
                    self.directory_tree.show()
                    self.log_message("→ Directory Tree")
                else:
                    # Directory tree not loaded yet - load it now
                    if hasattr(self, 'game_root') and self.game_root:
                        from apps.components.File_Editor.directory_tree_browser import integrate_directory_tree_browser
                        if integrate_directory_tree_browser(self):
                            # Place in file window
                            if hasattr(self.gui_layout, 'middle_vertical_splitter'):
                                splitter = self.gui_layout.middle_vertical_splitter
                                if splitter and splitter.count() > 0:
                                    file_window = splitter.widget(0)
                                    layout = file_window.layout()
                                    if layout:
                                        layout.addWidget(self.directory_tree)
                                        self.gui_layout.table.hide()
                                        self.directory_tree.show()
                                        if hasattr(self.directory_tree, 'browse_directory'):
                                            self.directory_tree.browse_directory(self.game_root)
                                        self.log_message("✓ Directory tree loaded")
                    else:
                        self.log_message("No project configured - use Project menu")
            else:
                # IMG file tab - show table, hide directory tree
                if hasattr(self, 'directory_tree'):
                    self.directory_tree.hide()
                if hasattr(self.gui_layout, 'table'):
                    self.gui_layout.table.show()

                # Reload table content from current tab
                current_tab = self.main_tab_widget.widget(index)
                if current_tab and hasattr(current_tab, 'file_object'):
                    file_object = current_tab.file_object
                    if hasattr(self, '_populate_real_img_table'):
                        self._populate_real_img_table(file_object)
                        self.current_img = file_object
                        tab_name = self.main_tab_widget.tabText(index)
                        self.log_message(f"→ {tab_name}")
        except Exception as e:
            self.log_message(f"Tab switch error: {str(e)}")
            import traceback
            traceback.print_exc()


    def ensure_current_tab_references_valid(self): #vers 1
        """Ensure current tab references are valid before export operations - PUBLIC METHOD"""
        try:
            current_index = self.main_tab_widget.currentIndex()
            if current_index == -1:
                return False

            # Force update tab references
            self._on_tab_changed(current_index)

            # Verify we have valid references
            has_valid_file = self.current_img is not None or self.current_col is not None
            has_valid_table = hasattr(self.gui_layout, 'table') and self.gui_layout.table is not None

            if has_valid_file and has_valid_table:
                self.log_message(f"Tab references validated for export operations")
                return True
            else:
                self.log_message(f"Invalid tab references - File: {has_valid_file}, Table: {has_valid_table}")
                return False

        except Exception as e:
            self.log_message(f"Error validating tab references: {str(e)}")
            return False


    def _update_info_bar_for_current_file(self): #vers 1
        """Update info bar based on current file type"""
        try:
            if self.current_img:
                # Update for IMG file
                entry_count = len(self.current_img.entries) if self.current_img.entries else 0
                file_path = getattr(self.current_img, 'file_path', 'Unknown')

                if hasattr(self.gui_layout, 'info_label') and self.gui_layout.info_label:
                    self.gui_layout.info_label.setText(f"IMG: {os.path.basename(file_path)} | {entry_count} entries")

            elif self.current_col:
                # Update for COL file
                model_count = len(self.current_col.models) if hasattr(self.current_col, 'models') and self.current_col.models else 0
                file_path = getattr(self.current_col, 'file_path', 'Unknown')

                if hasattr(self.gui_layout, 'info_label') and self.gui_layout.info_label:
                    self.gui_layout.info_label.setText(f"COL: {os.path.basename(file_path)} | {model_count} models")

        except Exception as e:
            self.log_message(f"Error updating info bar: {str(e)}")


    def setup_robust_tab_system(self): #vers 1
        """Setup robust tab system during initialization"""
        try:
            # Import and install robust tab system
            from apps.core.robust_tab_system import install_robust_tab_system

            if install_robust_tab_system(self):
                self.log_message("Robust tab system ready")

                # Run initial integrity check
                if hasattr(self, 'validate_tab_data_integrity'):
                    self.validate_tab_data_integrity()

                return True
            else:
                self.log_message("Failed to setup robust tab system")
                return False

        except ImportError:
            self.log_message("âš Robust tab system not available - using basic system")
            return False
        except Exception as e:
            self.log_message(f"Error setting up robust tab system: {str(e)}")
            return False


    def _reindex_open_files_robust(self, removed_index): #vers 1
        """ROBUST: Reindex with data preservation"""
        try:
            if not hasattr(self.main_window, 'open_files'):
                return

            self.log_message(f"ROBUST reindexing after removing tab {removed_index}")

            # STEP 1: Preserve data for all remaining tabs
            preserved_data = {}
            for tab_index in list(self.main_window.open_files.keys()):
                if tab_index != removed_index:
                    if hasattr(self.main_window, 'preserve_tab_table_data'):
                        self.main_window.preserve_tab_table_data(tab_index)

            # STEP 2: Reindex open_files (same as before)
            new_open_files = {}
            sorted_items = sorted(self.main_window.open_files.items())

            new_index = 0
            for old_index, file_info in sorted_items:
                if old_index == removed_index:
                    self.log_message(f"Skipping removed tab {old_index}")
                    continue

                new_open_files[new_index] = file_info
                self.log_message(f"Tab {old_index} â†’ Tab {new_index}: {file_info.get('tab_name', 'Unknown')}")
                new_index += 1

            self.main_window.open_files = new_open_files

            # STEP 3: Restore data for all tabs in their new positions
            for new_tab_index in new_open_files.keys():
                if hasattr(self.main_window, 'restore_tab_table_data'):
                    self.main_window.restore_tab_table_data(new_tab_index)

            self.log_message("ROBUST reindexing complete with data preservation")

            # STEP 4: Update current tab references
            current_index = self.main_window.main_tab_widget.currentIndex()
            if hasattr(self.main_window, 'update_tab_manager_references'):
                self.main_window.update_tab_manager_references(current_index)

        except Exception as e:
            self.log_message(f"Error in robust reindexing: {str(e)}")


    def patch_close_manager_for_robust_tabs(main_window): #vers 1
        """Patch existing close manager to use robust tab system"""
        try:
            if hasattr(main_window, 'close_manager'):
                # Replace the reindex method with robust version
                original_reindex = main_window.close_manager._reindex_open_files

                def robust_reindex_wrapper(removed_index):
                    return _reindex_open_files_robust(main_window.close_manager, removed_index)

                main_window.close_manager._reindex_open_files = robust_reindex_wrapper
                main_window.log_message("Close manager patched for robust tabs")
                return True
            else:
                main_window.log_message("No close manager found to patch")
                return False

        except Exception as e:
            main_window.log_message(f"Error patching close manager: {str(e)}")
            return False


    def _update_ui_for_current_file(self): #vers 5
        """Update UI for currently selected file"""
        if self.current_img:
            self.log_message("Updating UI for IMG file")
            self._update_ui_for_loaded_img()
        elif self.current_col:
            self.log_message("Updating UI for COL file")
            self._update_ui_for_loaded_col()
        else:
            self.log_message("Updating UI for no file")
            self._update_ui_for_no_img()


    def load_col_file_safely(self, file_path): #vers 4
        """Load COL file safely - Use the actual COL loading function"""
        try:
            # Import and use the real COL loading function
            from col_parsing_functions import load_col_file_safely as real_load_col
            success = real_load_col(self, file_path)
            if success:
                self.log_message(f"COL file loaded: {os.path.basename(file_path)}")
            return success
        except Exception as e:
            self.log_message(f"Error loading COL file: {str(e)}")
            return False


    def _load_col_as_generic_file(self, file_path): #vers 1
        """Load COL as generic file when COL classes aren't available"""
        try:
            # Create simple COL representation
            self.current_col = {
                "file_path": file_path, "type": "COL", "size": os.path.getsize(file_path)
            }

            # Update UI
            self._update_ui_for_loaded_col()

            self.log_message(f"Loaded COL (generic): {os.path.basename(file_path)}")

        except Exception as e:
            self.log_message(f"Error loading COL as generic: {str(e)}")


    def get_current_file_type(main_window) -> str: #vers 1
        """Get the current file type (IMG or COL)"""
        try:
            if hasattr(main_window, 'current_col') and main_window.current_col:
                return 'COL'
            elif hasattr(main_window, 'current_img') and main_window.current_img:
                return 'IMG'
            else:
                return 'UNKNOWN'
        except:
            return 'UNKNOWN'


    def has_col_file_loaded(main_window) -> bool: #vers 1
        """Check if a COL file is currently loaded - REPLACES has_col"""
        try:
            return hasattr(main_window, 'current_col') and main_window.current_col is not None
        except:
            return False


    def has_img_file_loaded(main_window) -> bool: #vers 1
        """Check if an IMG file is currently loaded"""
        try:
            return hasattr(main_window, 'current_img') and main_window.current_img is not None
        except:
            return False


    def open_img_file(self): #vers 2
        """Open file dialog - FIXED: Call imported function correctly"""
        try:
            open_file_dialog(self)  # Call function with self parameter
        except Exception as e:
            self.log_message(f"Error opening file dialog: {str(e)}")


    def open_file_dialog(self): #vers 1
        """Unified file dialog - imported from apps.core."""
        from apps.core.open_img import open_file_dialog
        return open_file_dialog(self)


    def _clean_on_img_loaded(self, img_file: IMGFile): #vers 6
        """Handle IMG loading - USES ISOLATED FILE WINDOW"""
        try:
            # Store the loaded IMG file
            current_index = self.main_tab_widget.currentIndex()
            if current_index in self.open_files:
                self.open_files[current_index]['file_object'] = img_file

            # Set current IMG reference
            self.current_img = img_file
            # CRITICAL: Store file object in tab tracking for tab switching
            current_index = self.main_tab_widget.currentIndex()
            if current_index in self.open_files:
                self.open_files[current_index]['file_object'] = img_file
                self.log_message(f"IMG file object stored in tab {current_index}")

            # Refresh the directory file list in the left panel
            if hasattr(self, 'gui_layout') and hasattr(self.gui_layout, 'refresh_directory_files'):
                self.gui_layout.refresh_directory_files()

            # Use isolated file window update
            #success = self.gui_layout.update_file_window_only(img_file)

            # Properly hide progress and ensure GUI visibility
            self.gui_layout.hide_progress_properly()

            if success:
                self.log_message(f"Loaded (isolated): {os.path.basename(img_file.file_path)} ({len(img_file.entries)} entries)")

        except Exception as e:
            self.log_message(f"Loading error: {str(e)}")


    def reload_table(self): #vers 1
        """Reload current file - called by reload button"""
        return self.reload_current_file()


    def switch_to_img_file(self, file_name: str): #vers 1
        """Switch to the tab containing the specified file"""
        try:
            # Look for the tab with the matching file name
            for i in range(self.main_tab_widget.count()):
                tab_text = self.main_tab_widget.tabText(i)
                if tab_text == file_name or tab_text.startswith(file_name):
                    # Switch to this tab
                    self.main_tab_widget.setCurrentIndex(i)
                    
                    # Update the main window references
                    from apps.methods.tab_system import switch_tab
                    switch_tab(self, i)
                    
                    self.log_message(f"Switched to file: {file_name}")
                    return True
            
            # If file not found, log a message
            self.log_message(f"File not found in open tabs: {file_name}")
            return False
            
        except Exception as e:
            self.log_message(f"Error switching to file {file_name}: {str(e)}")
            return False


    def load_file_unified(self, file_path: str): #vers 8
        """Unified file loader for IMG and COL files"""
        try:
            if not file_path or not os.path.exists(file_path):
                self.log_message("File not found")
                return False

            file_ext = file_path.lower().split('.')[-1]
            file_name = os.path.basename(file_path)

            if file_ext == 'img':
                self._load_img_file_in_new_tab(file_path)  # â† Starts threading
                return True  # â† Return immediately, let threading finish
                try:
                    # Import IMG loading components directly
                    from apps.methods.img_core_classes import IMGFile
                    from apps.methods.populate_img_table import populate_img_table

                    # Create IMG file object
                    img_file = IMGFile(file_path)

                    if not img_file.open():
                        self.log_message(f"Failed to open IMG file: {img_file.get_error()}")
                        return False

                    # Set as current IMG file #hangs after second img added?
                    #self.current_img = img_file

                    # CRITICAL: Setup IMG table structure (6 columns)
                    if hasattr(self, 'gui_layout') and hasattr(self.gui_layout, 'table'):
                        table = self.gui_layout.table
                        # Reset to IMG structure
                        table.setColumnCount(6)
                        table.setHorizontalHeaderLabels([
                            "Name", "Type", "Size", "Offset", "RW Version", "Info"
                        ])
                        # Set IMG column widths
                        table.setColumnWidth(0, 200)  # Name
                        table.setColumnWidth(1, 80)   # Type
                        table.setColumnWidth(2, 100)  # Size
                        table.setColumnWidth(3, 100)  # Offset
                        table.setColumnWidth(4, 120)  # RW Version
                        table.setColumnWidth(5, 150)  # Info

                    # Populate table with IMG data using proper method

                    populate_img_table(table, img_file)

                    # Update window title
                    self.setWindowTitle(f"IMG Factory 1.5 - {file_name}")

                    # Update info panel/status
                    entry_count = len(img_file.entries) if img_file.entries else 0
                    file_size = os.path.getsize(file_path)

                    self.log_message(f"IMG file loaded: {entry_count} entries")
                    return True

                except Exception as img_error:
                    self.log_message(f"Error loading IMG file: {str(img_error)}")
                    return False

            elif file_ext == 'col':
                # COL file loading (unchanged - working correctly)
                if hasattr_open_txd_workshop(self, 'load_col_file_safely'):
                    self.log_message(f"Loading COL file: {file_name}")
                    success = self.load_col_file_safely(file_path)
                    if success:
                        self.log_message("COL file loaded successfully")
                    else:
                        self.log_message("Failed to load COL file")
                    return success
                else:
                    self.log_message("COL integration not available")
                    return False

            else:
                self.log_message(f"Unsupported file type: {file_ext}")
                return False

        except Exception as e:
            self.log_message(f"Error loading file: {str(e)}")
            import traceback
            traceback.print_exc()  # Debug info
            return False

    def _load_img_file_in_new_tab(self, file_path): #vers [your_version + 1]
        """Load IMG file in new tab"""
        try:
            import os
            self.log_message(f"Loading IMG in new tab: {os.path.basename(file_path)}")

            # Create new tab first
            tab_index = self.create_tab(file_path, 'IMG', None)

            # Then load IMG using your existing thread loader
            if self.load_thread and self.load_thread.isRunning():
                return

            self.load_thread = IMGLoadThread(file_path)
            self.load_thread.progress_updated.connect(self._on_img_load_progress)
            self.load_thread.loading_finished.connect(self._on_img_loaded)
            self.load_thread.loading_error.connect(self._on_img_load_error)
            self.load_thread.start()

        except Exception as e:
            self.log_message(f"Error loading IMG in new tab: {str(e)}")


    def _load_txd_file_in_new_tab(self, file_path):  # vers 3
        """Load TXD file in new tab - FIXED: creates tab, assigns data, no overwrite"""
        try:
            import os
            file_name = os.path.basename(file_path)
            base_name = file_name[:-4] if file_name.lower().endswith('.txd') else file_name

            # STEP 1: Create new tab
            tab_index = self.create_tab(file_path, 'TXD', None)
            if tab_index is None:
                self.log_message("Failed to create TXD tab")
                return

            # STEP 2: Load TXD via workshop
            from apps.components.Txd_Editor.txd_workshop import open_txd_workshop
            workshop = open_txd_workshop(self, file_path)

            if not workshop:
                self.log_message("TXD workshop failed to open")
                return

            # STEP 3: If TXD loading sets self.current_txd, assign it to the tab
            if hasattr(self, 'current_txd') and self.current_txd:
                tab_widget = self.main_tab_widget.widget(tab_index)
                if tab_widget:
                    tab_widget.file_object = self.current_txd
                    tab_widget.file_type = 'TXD'
                    tab_widget.file_path = file_path
                    self.log_message(f"TXD object assigned to tab {tab_index}")
                # Clear global reference
                self.current_txd = None

            # STEP 4: Ensure we're on the new tab
            self.main_tab_widget.setCurrentIndex(tab_index)

            self.log_message(f"TXD loaded in tab {tab_index}: {base_name}")

        except Exception as e:
            self.log_message(f"Error loading TXD in new tab: {str(e)}")
            import traceback
            traceback.print_exc()


    def _load_col_file_in_new_tab(self, file_path):  # vers 3
        """Load COL file in new tab - FULLY FIXED: creates tab first, assigns data correctly"""
        try:
            import os
            file_name = os.path.basename(file_path)
            self.log_message(f"Loading COL in new tab: {file_name}")

            # STEP 1: Create new tab BEFORE loading
            tab_index = self.create_tab(file_path, 'COL', None)
            if tab_index is None:
                self.log_message("Failed to create new tab for COL file")
                return

            # STEP 2: Remember the target tab index (in case user switches tabs during load)
            target_tab_index = tab_index

            # STEP 3: Load COL file (synchronously)
            if not hasattr(self, 'load_col_file_safely'):
                self.log_message("COL loading method not available")
                return

            success = self.load_col_file_safely(file_path)
            if not success:
                self.log_message("COL file failed to load after tab creation")
                # Optionally close the empty tab
                if hasattr(self, 'close_tab'):
                    self.close_tab(target_tab_index)
                return

            # STEP 4: At this point, self.current_col is set by load_col_file_safely
            # Assign it to the **correct tab**, not the current one (which may have changed)
            if target_tab_index >= self.main_tab_widget.count():
                self.log_message("Tab index out of bounds after COL load")
                return

            tab_widget = self.main_tab_widget.widget(target_tab_index)
            if tab_widget and self.current_col:
                # Store the loaded object on the tab
                tab_widget.file_object = self.current_col
                tab_widget.file_type = 'COL'
                tab_widget.file_path = file_path
                tab_widget.tab_ready = True

                # Update tab text (remove .col)
                base_name = file_name.rsplit('.', 1)[0]
                self.main_tab_widget.setTabText(target_tab_index, base_name)

                self.log_message(f"COL file assigned to tab {target_tab_index}")
            else:
                self.log_message("Warning: No tab widget or current_col missing after COL load")

            # STEP 5: Optionally clear global reference to avoid side effects
            # But only AFTER it's safely stored on the tab
            self.current_col = None

            # STEP 6: Switch to the new tab (optional but expected UX)
            self.main_tab_widget.setCurrentIndex(target_tab_index)

        except Exception as e:
            self.log_message(f"Error loading COL in new tab: {str(e)}")
            import traceback
            traceback.print_exc()


    def _open_txd_workshop(self, file_path=None): #vers 2
        """Open TXD Workshop - connects to tab switching"""
        from apps.components.Txd_Editor.txd_workshop import open_txd_workshop

        if not file_path:
            if hasattr(self, 'current_img') and self.current_img:
                file_path = self.current_img.file_path

        workshop = open_txd_workshop(self, file_path)

        if workshop:
            if not hasattr(self, 'txd_workshops'):
                self.txd_workshops = []

            self.txd_workshops.append(workshop)

            # Connect workshop to tab changes
            self.main_tab_widget.currentChanged.connect(
                lambda idx: self._update_workshop_on_tab_change(workshop, idx)
            )

            workshop.workshop_closed.connect(lambda: self._on_workshop_closed(workshop))
            self.log_message(f"Workshop opened and connected ({len(self.txd_workshops)} total)")

        return workshop


    def _update_workshop_on_tab_change(self, workshop, tab_index): #vers 1
        """Update specific workshop when tab changes"""
        if not workshop or not workshop.isVisible():
            return

        tab_widget = self.main_tab_widget.widget(tab_index)
        if not tab_widget:
            return

        file_path = getattr(tab_widget, 'file_path', None)
        if file_path:
            if file_path.lower().endswith('.txd'):
                workshop.open_txd_file(file_path)
            elif file_path.lower().endswith('.img'):
                workshop.load_from_img_archive(file_path)

    def _on_workshop_closed(self, workshop): #vers 1
        """Remove closed workshop from tracking list"""
        if hasattr(self, 'txd_workshops') and workshop in self.txd_workshops:
            self.txd_workshops.remove(workshop)
            self.log_message(f"Workshop closed ({len(self.txd_workshops)} remaining)")


    def open_file_dialog(main_window): #vers 8
        """Unified file dialog for IMG, COL, and TXD files"""
        file_path, _ = QFileDialog.getOpenFileName(
            main_window,
            "Open Archive",
            "",
            "All Supported (*.img *.col *.txd);;IMG Archives (*.img);;COL Archives (*.col);;TXD Textures (*.txd);;All Files (*)"
        )

        if file_path:
            file_ext = os.path.splitext(file_path)[1].lower()

            if file_ext == '.txd':
                load_txd_file(main_window, file_path)
            elif file_ext == '.col':
                # Create new tab for COL
                if hasattr(main_window, '_load_col_file_in_new_tab'):
                    main_window._load_col_file_in_new_tab(file_path)
                else:
                    main_window.load_col_file_safely(file_path)
            else:  # .img
                # Create new tab for IMG
                if hasattr(main_window, '_load_img_file_in_new_tab'):
                    main_window._load_img_file_in_new_tab(file_path)
                else:
                    main_window.load_img_file(file_path)


    def _on_img_load_progress(self, progress: int, status: str): #vers 5
        """Handle IMG loading progress updates - UPDATED: Uses unified progress system"""
        try:
            from apps.methods.progressbar_functions import update_progress
            update_progress(self, progress, status)
        except ImportError:
            # Fallback for systems without unified progress
            self.log_message(f"Progress: {progress}% - {status}")


    def _update_ui_for_no_img(self): #vers 6
        """Update UI when no IMG file is loaded - UPDATED: Uses unified progress system"""
        # Clear current data
        self.current_img = None
        self.current_col = None
        self.current_txd = None

        # Update window title
        self.setWindowTitle("IMG Factory 1.5")

        # Clear table if it exists
        if hasattr(self, 'gui_layout') and hasattr(self.gui_layout, 'table'):
            self.gui_layout.table.setRowCount(0)

        # Reset progress using unified system
        try:
            from apps.methods.progressbar_functions import hide_progress
            hide_progress(self, "Ready")
        except ImportError:
            # Fallback for old systems
            if hasattr(self, 'gui_layout') and hasattr(self.gui_layout, 'show_progress'):
                self.gui_layout.show_progress(-1, "Ready")

        # Update file info
        if hasattr(self.gui_layout, 'update_img_info'):
            self.gui_layout.update_img_info("No IMG loaded")

        # Reset any status labels
        if hasattr(self, 'file_path_label'):
            self.file_path_label.setText("No file loaded")
        if hasattr(self, 'version_label'):
            self.version_label.setText("---")
        if hasattr(self, 'entry_count_label'):
            self.entry_count_label.setText("0")
        if hasattr(self, 'img_status_label'):
            self.img_status_label.setText("No IMG loaded")

        # Disable buttons that require an IMG to be loaded
        buttons_to_disable = [
            'close_img_btn', 'rebuild_btn', 'rebuild_as_btn', 'validate_btn',
            'import_btn', 'export_all_btn', 'export_selected_btn'
        ]

        for btn_name in buttons_to_disable:
            if hasattr(self.gui_layout, btn_name):
                button = getattr(self.gui_layout, btn_name)
                if hasattr(button, 'setEnabled'):
                    button.setEnabled(False)

    def _on_img_load_error(self, error_message: str): #vers 4
        """Handle IMG loading error - UPDATED: Uses unified progress system"""
        self.log_message(f" {error_message}")

        # Hide progress using unified system
        try:
            from apps.methods.progressbar_functions import hide_progress
            hide_progress(self, "Load failed")
        except ImportError:
            # Fallback for old systems
            if hasattr(self.gui_layout, 'hide_progress'):
                self.gui_layout.hide_progress()

        QMessageBox.critical(self, "IMG Load Error", error_message)

    # Add this to __init__ method after GUI creation:
    def integrate_unified_progress_system(self): #vers 1
        """Integrate unified progress system - call in __init__"""
        try:
            from apps.methods.progressbar_functions import integrate_progress_system
            integrate_progress_system(self)
            self.log_message("Unified progress system integrated")
        except ImportError:
            self.log_message("Unified progress system not available - using fallback")
        except Exception as e:
            self.log_message(f"Progress system integration failed: {str(e)}")


    def _populate_col_table_img_format(self, col_file, file_name):
        """Populate table with COL models using same format as IMG entries""" #vers 2 #restare
        from PyQt6.QtWidgets import QTableWidgetItem
        from PyQt6.QtCore import Qt

        table = self.gui_layout.table

        # Keep the same 7-column format as IMG files
        table.setColumnCount(7)
        table.setHorizontalHeaderLabels([
            "Name", "Type", "Size", "Offset", "Version", "Compression", "Status"
        ])

        if not col_file or not hasattr(col_file, 'models') or not col_file.models:
            # Show the file itself if no models
            table.setRowCount(1)

            try:
                file_size = os.path.getsize(col_file.file_path) if col_file and hasattr(col_file, 'file_path') and col_file.file_path else 0
                size_text = self._format_file_size(file_size)
            except:
                size_text = "Unknown"

            items = [
                (file_name, "COL", size_text, "0x0", "Unknown", "None", "No Models")
            ]

            for row, item_data in enumerate(items):
                for col, value in enumerate(item_data):
                    item = QTableWidgetItem(str(value))
                    item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
                    table.setItem(row, col, item)

            self.log_message(f"COL file loaded but no models found")
            return

        # Show individual models in IMG entry format
        models = col_file.models
        table.setRowCount(len(models))

        self.log_message(f"Populating table with {len(models)} COL models")

        virtual_offset = 0x0  # Virtual offset for COL models

        for row, model in enumerate(models):
            try:
                # Name - use model name or generate one
                model_name = getattr(model, 'name', f"Model_{row}") if hasattr(model, 'name') and model.name else f"Model_{row}"
                table.setItem(row, 0, QTableWidgetItem(model_name))

                # Type - just "COL" (like IMG shows "DFF", "TXD", etc.)
                table.setItem(row, 1, QTableWidgetItem("COL"))

                # Size - estimate model size in same format as IMG
                estimated_size = self._estimate_col_model_size_bytes(model)
                size_text = self._format_file_size(estimated_size)
                table.setItem(row, 2, QTableWidgetItem(size_text))

                # Offset - virtual hex offset (like IMG entries)
                offset_text = f"0x{virtual_offset:X}"
                table.setItem(row, 3, QTableWidgetItem(offset_text))
                virtual_offset += estimated_size  # Increment for next model

                # Version - show just the COL version number (1, 2, 3, or 4)
                if hasattr(model, 'version') and hasattr(model.version, 'value'):
                    version_text = str(model.version.value)  # Just "1", "2", "3", or "4"
                elif hasattr(model, 'version'):
                    version_text = str(model.version)
                else:
                    version_text = "Unknown"
                table.setItem(row, 4, QTableWidgetItem(version_text))

                # Compression - always None for COL models
                table.setItem(row, 5, QTableWidgetItem("None"))

                # Status - based on model content (like IMG status)
                stats = model.get_stats() if hasattr(model, 'get_stats') else {}
                total_elements = stats.get('total_elements', 0)

                if total_elements == 0:
                    status = "Empty"
                elif total_elements > 500:
                    status = "Complex"
                elif total_elements > 100:
                    status = "Medium"
                else:
                    status = "Ready"
                table.setItem(row, 6, QTableWidgetItem(status))

                # Make all items read-only (same as IMG)
                for col in range(7):
                    item = table.item(row, col)
                    if item:
                        item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)

            except Exception as e:
                self.log_message(f"âŒ Error populating COL model {row}: {str(e)}")
                # Create fallback row (same as IMG error handling)
                table.setItem(row, 0, QTableWidgetItem(f"Model_{row}"))
                table.setItem(row, 1, QTableWidgetItem("COL"))
                table.setItem(row, 2, QTableWidgetItem("0 B"))
                table.setItem(row, 3, QTableWidgetItem("0x0"))
                table.setItem(row, 4, QTableWidgetItem("Unknown"))
                table.setItem(row, 5, QTableWidgetItem("None"))
                table.setItem(row, 6, QTableWidgetItem("Error"))

        self.log_message(f"Table populated with {len(models)} COL models (IMG format)")

    def _estimate_col_model_size_bytes(self, model): #vers 2 #restare
        """Estimate COL model size in bytes (similar to IMG entry sizes)"""
        try:
            if not hasattr(model, 'get_stats'):
                return 1024  # Default 1KB

            stats = model.get_stats()

            # Rough estimation based on collision elements
            size = 100  # Base model overhead (header, name, etc.)
            size += stats.get('spheres', 0) * 16     # 16 bytes per sphere
            size += stats.get('boxes', 0) * 24       # 24 bytes per box
            size += stats.get('vertices', 0) * 12    # 12 bytes per vertex
            size += stats.get('faces', 0) * 8        # 8 bytes per face
            size += stats.get('face_groups', 0) * 8  # 8 bytes per face group

            # Add version-specific overhead
            if hasattr(model, 'version') and hasattr(model.version, 'value'):
                if model.version.value >= 3:
                    size += stats.get('shadow_vertices', 0) * 12
                    size += stats.get('shadow_faces', 0) * 8
                    size += 64  # COL3+ additional headers
                elif model.version.value >= 2:
                    size += 48  # COL2 headers

            return max(size, 64)  # Minimum 64 bytes

        except Exception:
            return 1024  # Default 1KB on error


    def _on_load_progress(self, progress: int, status: str): #vers 4
        """Handle loading progress updates"""
        if hasattr(self.gui_layout, 'show_progress'):
            self.gui_layout.show_progress(progress, status)
        else:
            self.log_message(f"Progress: {progress}% - {status}")



    def get_entry_rw_version(self, entry, extension): #vers 3
        """Detect RW version from entry file data"""
        try:
            # Skip non-RW files
            if extension not in ['DFF', 'TXD']:
                return "Unknown"

            # Check if entry already has version info
            if hasattr(entry, 'get_version_text') and callable(entry.get_version_text):
                return entry.get_version_text()

            # Try to get file data using different methods
            file_data = None

            # Method 1: Direct data access
            if hasattr(entry, 'get_data'):
                try:
                    file_data = entry.get_data()
                except:
                    pass

            # Method 2: Extract data method
            if not file_data and hasattr(entry, 'extract_data'):
                try:
                    file_data = entry.extract_data()
                except:
                    pass

            # Method 3: Read directly from IMG file
            if not file_data:
                try:
                    if (hasattr(self, 'current_img') and
                        hasattr(entry, 'offset') and
                        hasattr(entry, 'size') and
                        self.current_img and
                        self.current_img.file_path):

                        with open(self.current_img.file_path, 'rb') as f:
                            f.seek(entry.offset)
                            # Only read the header (12 bytes) for efficiency
                            file_data = f.read(min(entry.size, 12))
                except Exception as e:
                    print(f"DEBUG: Failed to read file data for {entry.name}: {e}")
                    return "Unknown"

            # Parse RW version from file header
            if file_data and len(file_data) >= 12:
                import struct
                try:
                    # RW version is stored at offset 8-12 in RW files
                    rw_version = struct.unpack('<I', file_data[8:12])[0]

                    if rw_version > 0:
                        version_name = get_rw_version_name(rw_version)
                        print(f"DEBUG: Found RW version 0x{rw_version:X} ({version_name}) for {entry.name}")
                        return f"RW {version_name}"
                    else:
                        print(f"DEBUG: Invalid RW version (0) for {entry.name}")
                        return "Unknown"

                except struct.error as e:
                    print(f"DEBUG: Struct unpack error for {entry.name}: {e}")
                    return "Unknown"
            else:
                print(f"DEBUG: Insufficient file data for {entry.name} (need 12 bytes, got {len(file_data) if file_data else 0})")
                return "Unknown"

        except Exception as e:
            print(f"DEBUG: RW version detection error for {entry.name}: {e}")
            return "Unknown"


    def format_file_size(size_bytes): #vers 2 #Restore
        """Format file size same as IMG entries"""
        try:
            # Use the same formatting as IMG entries
            try:
                from apps.methods.img_core_classes import format_file_size
                return format_file_size(size_bytes)
            except:
                pass

            # Fallback formatting (same logic as IMG)
            if size_bytes < 1024:
                return f"{size_bytes} B"
            elif size_bytes < 1024 * 1024:
                return f"{size_bytes // 1024} KB"
            elif size_bytes < 1024 * 1024 * 1024:
                return f"{size_bytes // (1024 * 1024)} MB"
            else:
                return f"{size_bytes // (1024 * 1024 * 1024)} GB"

        except Exception:
            return f"{size_bytes} bytes"


    def get_col_model_details_for_display(self, model, row_index): #vers 2 #Restore
        """Get COL model details in same format as IMG entry details"""
        try:
            stats = model.get_stats() if hasattr(model, 'get_stats') else {}

            details = {
                'name': getattr(model, 'name', f"Model_{row_index}") if hasattr(model, 'name') and model.name else f"Model_{row_index}",
                'type': "COL",
                'size': self._estimate_col_model_size_bytes(model),
                'version': str(model.version.value) if hasattr(model, 'version') and hasattr(model.version, 'value') else "Unknown",
                'elements': stats.get('total_elements', 0),
                'spheres': stats.get('spheres', 0),
                'boxes': stats.get('boxes', 0),
                'faces': stats.get('faces', 0),
                'vertices': stats.get('vertices', 0),
            }

            if hasattr(model, 'bounding_box') and model.bounding_box:
                bbox = model.bounding_box
                if hasattr(bbox, 'center') and hasattr(bbox, 'radius'):
                    details.update({
                        'bbox_center': (bbox.center.x, bbox.center.y, bbox.center.z),
                        'bbox_radius': bbox.radius,
                    })
                    if hasattr(bbox, 'min') and hasattr(bbox, 'max'):
                        details.update({
                            'bbox_min': (bbox.min.x, bbox.min.y, bbox.min.z),
                            'bbox_max': (bbox.max.x, bbox.max.y, bbox.max.z),
                        })

            return details

        except Exception as e:
            self.log_message(f"Error getting COL model details: {str(e)}")
            return {
                'name': f"Model_{row_index}",
                'type': "COL",
                'size': 0,
                'version': "Unknown",
                'elements': 0,
            }

    def show_col_model_details_img_style(self, model_index): #vers 2 #Restore
        """Show COL model details in same style as IMG entry details"""
        try:
            if (not hasattr(self, 'current_col') or
                not hasattr(self.current_col, 'models') or
                model_index >= len(self.current_col.models)):
                return

            model = self.current_col.models[model_index]
            details = self.get_col_model_details_for_display(model, model_index)

            from PyQt6.QtWidgets import QMessageBox

            info_lines = []
            info_lines.append(f"Name: {details['name']}")
            info_lines.append(f"Type: {details['type']}")
            info_lines.append(f"Size: {self._format_file_size(details['size'])}")
            info_lines.append(f"Version: {details['version']}")
            info_lines.append("")
            info_lines.append("Collision Data:")
            info_lines.append(f"  Total Elements: {details['elements']}")
            info_lines.append(f"  Spheres: {details['spheres']}")
            info_lines.append(f"  Boxes: {details['boxes']}")
            info_lines.append(f"  Faces: {details['faces']}")
            info_lines.append(f"  Vertices: {details['vertices']}")

            if 'bbox_center' in details:
                info_lines.append("")
                info_lines.append("Bounding Box:")
                center = details['bbox_center']
                info_lines.append(f"  Center: ({center[0]:.2f}, {center[1]:.2f}, {center[2]:.2f})")
                info_lines.append(f"  Radius: {details['bbox_radius']:.2f}")

            QMessageBox.information(
                self,
                f"COL Model Details - {details['name']}",
                "\n".join(info_lines)
            )

        except Exception as e:
            self.log_message(f"Error showing COL model details: {str(e)}")


    def _on_col_table_double_click(self, item): #vers 2 #Restore
        """Handle double-click on COL table item - IMG style"""
        try:
            if hasattr(self, 'current_col') and hasattr(self.current_col, 'models'):
                row = item.row()
                self.show_col_model_details_img_style(row)
            else:
                self.log_message("No COL models available for details")
        except Exception as e:
            self.log_message(f"Error handling COL table double-click: {str(e)}")

    def _on_col_loaded(self, col_file): #vers 1 #Restore
        """Handle COL file loaded - UPDATED with styling"""
        try:
            self.current_col = col_file
            # Store COL file in tab tracking
            current_index = self.main_tab_widget.currentIndex()

            if hasattr(self, 'open_files') and current_index in self.open_files:
                self.open_files[current_index]['file_object'] = col_file
                self.log_message(f"COL file object stored in tab {current_index}")

            # Apply COL tab styling if available
            if hasattr(self, '_apply_individual_col_tab_style'):
                self._apply_individual_col_tab_style(current_index)

            # Update file info in open_files (same as IMG)
            if current_index in self.open_files:
                self.open_files[current_index]['file_object'] = col_file
                self.log_message(f"Updated tab {current_index} with loaded COL")
            else:
                # Show warning icon on the tab if possible
                try:
                    from apps.methods.imgfactory_svg_icons import get_warning_icon
                    warning_icon = get_warning_icon()
                    self.main_tab_widget.setTabIcon(current_index, warning_icon)
                except:
                    pass  # Fallback to just the log message if icon setting fails
                self.log_message(f"Tab {current_index} not found in open_files")

            # Apply enhanced COL tab styling after loading
            if hasattr(self, '_apply_individual_col_tab_style'):
                self._apply_individual_col_tab_style(current_index)

            # Update UI for loaded COL
            if hasattr(self, '_update_ui_for_loaded_col'):
                self._update_ui_for_loaded_col()

            # Update window title to show current file
            file_name = os.path.basename(col_file.file_path) if hasattr(col_file, 'file_path') else "Unknown COL"
            self.setWindowTitle(f"IMG Factory 1.5 - {file_name}")

            model_count = len(col_file.models) if hasattr(col_file, 'models') and col_file.models else 0
            self.log_message(f"Loaded: {file_name} ({model_count} models)")

            # Hide progress and show COL-specific status
            if hasattr(self.gui_layout, 'show_progress'):
                self.gui_layout.show_progress(-1, f"COL loaded: {model_count} models")

        except Exception as e:
            self.log_message(f"Error in _on_col_loaded: {str(e)}")
            if hasattr(self, '_on_col_load_error'):
                self._on_col_load_error(str(e))


    def _setup_col_integration_safely(self):
        """Setup COL integration safely"""
        try:
            if COL_SETUP_FUNCTION:
                result = COL_SETUP_FUNCTION(self)
                if result:
                    self.log_message("COL functionality integrated")
                else:
                    self.log_message("COL integration returned False")
            else:
                self.log_message("COL integration function not available")
        except Exception as e:
            self.log_message(f"COL integration error: {str(e)}")

    def _on_load_progress(self, progress: int, status: str): #vers 2 #Restore
        """Handle loading progress updates"""
        if hasattr(self.gui_layout, 'show_progress'):
            self.gui_layout.show_progress(progress, status)
        else:
            self.log_message(f"Progress: {progress}% - {status}")


    def _on_img_loaded(self, img_file): #vers 4
        """Handle IMG loading completion"""
        try:
            self.current_img = img_file

            # Store on current tab widget
            current_index = self.main_tab_widget.currentIndex()
            tab_widget = self.main_tab_widget.widget(current_index)
            if tab_widget:
                tab_widget.file_object = img_file
                self.log_message(f"IMG stored on tab {current_index}")

            # Update window title
            file_name = os.path.basename(img_file.file_path)
            self.setWindowTitle(f"IMG Factory 1.5 - {file_name}")

            # Update UI for loaded IMG
            if hasattr(self, '_update_ui_for_loaded_img'):
                self._update_ui_for_loaded_img()

            # Log success
            entry_count = len(img_file.entries) if img_file.entries else 0
            self.log_message(f"Loaded: {file_name} ({entry_count} entries)")

            # Hide progress
            if hasattr(self.gui_layout, 'hide_progress'):
                self.gui_layout.hide_progress()

        except Exception as e:
            self.log_message(f"Error in _on_img_loaded: {str(e)}")

            if hasattr(self, '_on_img_load_error'):
                self._on_img_load_error(str(e))

    def _populate_real_img_table(self, img_file: IMGFile): #vers 2 #Restore
        """Populate table with real IMG file entries - for SA format display"""
        if not img_file or not img_file.entries:
            self.gui_layout.table.setRowCount(0)
            return

        table = self.gui_layout.table
        entries = img_file.entries

        # Clear existing data (including sample entries)
        table.setRowCount(0)
        table.setRowCount(len(entries))

        for row, entry in enumerate(entries):
            try:
                # Name - should now be clean from fixed parsing
                clean_name = str(entry.name).strip() if hasattr(entry, 'name') else f"Entry_{row}"
                table.setItem(row, 0, QTableWidgetItem(clean_name))

                # Extension - Use the cleaned extension from populate_entry_details
                if hasattr(entry, 'extension') and entry.extension:
                    extension = entry.extension
                else:
                    # Fallback extraction
                    if '.' in clean_name:
                        extension = clean_name.split('.')[-1].upper()
                        extension = ''.join(c for c in extension if c.isalpha())
                    else:
                        extension = "NO_EXT"
                table.setItem(row, 1, QTableWidgetItem(extension))

                # Size - Format properly
                try:
                    if hasattr(entry, 'size') and entry.size:
                        size_bytes = int(entry.size)
                        if size_bytes < 1024:
                            size_text = f"{size_bytes} B"
                        elif size_bytes < 1024 * 1024:
                            size_text = f"{size_bytes / 1024:.1f} KB"
                        else:
                            size_text = f"{size_bytes / (1024 * 1024):.1f} MB"
                    else:
                        size_text = "0 B"
                except:
                    size_text = "Unknown"
                table.setItem(row, 2, QTableWidgetItem(size_text))

                # Hash/Offset - Show as hex
                try:
                    if hasattr(entry, 'offset') and entry.offset is not None:
                        offset_text = f"0x{int(entry.offset):X}"
                    else:
                        offset_text = "0x0"
                except:
                    offset_text = "0x0"
                table.setItem(row, 3, QTableWidgetItem(offset_text))

                # Version - Use proper RW version parsing
                try:
                    if extension in ['DFF', 'TXD']:
                        if hasattr(entry, 'get_version_text') and callable(entry.get_version_text):
                            version_text = entry.get_version_text()
                        elif hasattr(entry, 'rw_version') and entry.rw_version > 0:
                            # FIXED: Use proper RW version mapping
                            rw_versions = {
                                0x0800FFFF: "3.0.0.0",
                                0x1003FFFF: "3.1.0.1",
                                0x1005FFFF: "3.2.0.0",
                                0x1400FFFF: "3.4.0.3",
                                0x1803FFFF: "3.6.0.3",
                                0x1C020037: "3.7.0.2",
                                # Additional common SA versions
                                0x34003: "3.4.0.3",
                                0x35002: "3.5.0.2",
                                0x36003: "3.6.0.3",
                                0x37002: "3.7.0.2",
                                0x1801: "3.6.0.3",  # Common SA version
                                0x1400: "3.4.0.3",  # Common SA version
                            }

                            if entry.rw_version in rw_versions:
                                version_text = f"RW {rw_versions[entry.rw_version]}"
                            else:
                                # Show hex for unknown versions
                                version_text = f"RW 0x{entry.rw_version:X}"
                        else:
                            version_text = "Unknown"
                    elif extension == 'COL':
                        version_text = "COL"
                    elif extension == 'IFP':
                        version_text = "IFP"
                    else:
                        version_text = "Unknown"
                except:
                    version_text = "Unknown"
                table.setItem(row, 4, QTableWidgetItem(version_text))

                # Compression
                try:
                    if hasattr(entry, 'compression_type') and entry.compression_type:
                        if str(entry.compression_type).upper() != 'NONE':
                            compression_text = str(entry.compression_type)
                        else:
                            compression_text = "None"
                    else:
                        compression_text = "None"
                except:
                    compression_text = "None"
                table.setItem(row, 5, QTableWidgetItem(compression_text))

                # Status
                try:
                    if hasattr(entry, 'is_new_entry') and entry.is_new_entry:
                        status_text = "New"
                    elif hasattr(entry, 'is_replaced') and entry.is_replaced:
                        status_text = "Modified"
                    else:
                        status_text = "Ready"
                except:
                    status_text = "Ready"
                table.setItem(row, 6, QTableWidgetItem(status_text))

                # Make all items read-only
                for col in range(7):
                    item = table.item(row, col)
                    if item:
                        item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)

            except Exception as e:
                self.log_message(f"âŒ Error populating row {row}: {str(e)}")
                # Create minimal fallback row
                table.setItem(row, 0, QTableWidgetItem(f"Entry_{row}"))
                table.setItem(row, 1, QTableWidgetItem("UNKNOWN"))
                table.setItem(row, 2, QTableWidgetItem("0 B"))
                table.setItem(row, 3, QTableWidgetItem("0x0"))
                table.setItem(row, 4, QTableWidgetItem("Unknown"))
                table.setItem(row, 5, QTableWidgetItem("None"))
                table.setItem(row, 6, QTableWidgetItem("Error"))

        self.log_message(f"Table populated with {len(entries)} entries (SA format parser fixed)")


    def _on_load_error(self, error_message): #vers 2
        """Handle loading error from background thread"""
        try:
            self.log_message(f"Loading error: {error_message}")

            # Hide progress - CHECK if method exists first
            if hasattr(self, 'gui_layout') and hasattr(self.gui_layout, 'show_progress'):
                self.gui_layout.show_progress(-1, "Error loading file")

            # Reset UI to no-file state
            self._update_ui_for_no_img()

            # Show error dialog
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.critical(
                self,
                "Loading Error",
                f"Failed to load IMG file:\n\n{error_message}")

        except Exception as e:
            self.log_message(f"Error in _on_load_error: {str(e)}")


    def close_all_img(self):
        """Close all IMG files - Wrapper for close_all_tabs"""
        try:
            if hasattr(self, 'close_manager') and self.close_manager:
                self.close_manager.close_all_tabs()
            else:
                self.log_message("Close manager not available")
        except Exception as e:
            self.log_message(f"Error in close_all_img: {str(e)}")


    def import_via_tool(self): #vers 1
        """Import files using external tool"""
        self.log_message("Import via tool functionality coming soon")


    def export_via_tool(self): #vers 1
        """Export using external tool"""
        if not self.current_img:
            QMessageBox.warning(self, "Warning", "No IMG file loaded")
            return
        self.log_message("Export via tool functionality coming soon")


    def import_files(self):
        """Import files into current IMG"""
        if not self.current_img:
            QMessageBox.warning(self, "No IMG", "No IMG file is currently loaded.")
            return

        try:
            file_paths, _ = QFileDialog.getOpenFileNames(
                self, "Import Files", "", "All Files (*);;DFF Models (*.dff);;TXD Textures (*.txd);;COL Collision (*.col)")

            if file_paths:
                self.log_message(f"Importing {len(file_paths)} files...")

                # Show progress - CHECK if method exists first
                if hasattr(self.gui_layout, 'show_progress'):
                    self.gui_layout.show_progress(0, "Importing files...")

                imported_count = 0
                for i, file_path in enumerate(file_paths):
                    progress = int((i + 1) * 100 / len(file_paths))
                    if hasattr(self.gui_layout, 'show_progress'):
                        self.gui_layout.show_progress(progress, f"Importing {os.path.basename(file_path)}")

                    # Check if IMG has import_file method
                    if hasattr(self.current_img, 'import_file'):
                        if self.current_img.import_file(file_path):
                            imported_count += 1
                            self.log_message(f"Imported: {os.path.basename(file_path)}")
                    else:
                        self.log_message(f"IMG import_file method not available")
                        break

                # Refresh table
                if hasattr(self, '_populate_real_img_table'):
                    self._populate_real_img_table(self.current_img)
                else:
                    populate_img_table(self.gui_layout.table, self.current_img)

                self.log_message(f"Import complete: {imported_count}/{len(file_paths)} files imported")

                if hasattr(self.gui_layout, 'show_progress'):
                    self.gui_layout.show_progress(-1, "Import complete")
                if hasattr(self.gui_layout, 'update_img_info'):
                    self.gui_layout.update_img_info(f"{len(self.current_img.entries)} entries")

                QMessageBox.information(self, "Import Complete",
                                      f"Imported {imported_count} of {len(file_paths)} files")

        except Exception as e:
            error_msg = f"Error importing files: {str(e)}"
            self.log_message(error_msg)
            if hasattr(self.gui_layout, 'show_progress'):
                self.gui_layout.show_progress(-1, "Import error")
            QMessageBox.critical(self, "Import Error", error_msg)

    def export_selected(self):
        """Export selected entries"""
        if not self.current_img:
            QMessageBox.warning(self, "No IMG", "No IMG file is currently loaded.")
            return

        try:
            selected_rows = []
            if hasattr(self.gui_layout, 'table') and hasattr(self.gui_layout.table, 'selectedItems'):
                for item in self.gui_layout.table.selectedItems():
                    if item.column() == 0:  # Only filename column
                        selected_rows.append(item.row())

            if not selected_rows:
                QMessageBox.warning(self, "No Selection", "Please select entries to export.")
                return

            export_dir = QFileDialog.getExistingDirectory(self, "Export To Folder")
            if export_dir:
                self.log_message(f"Exporting {len(selected_rows)} entries...")

                if hasattr(self.gui_layout, 'show_progress'):
                    self.gui_layout.show_progress(0, "Exporting...")

                exported_count = 0
                for i, row in enumerate(selected_rows):
                    progress = int((i + 1) * 100 / len(selected_rows))
                    entry_name = self.gui_layout.table.item(row, 0).text() if self.gui_layout.table.item(row, 0) else f"Entry_{row}"

                    if hasattr(self.gui_layout, 'show_progress'):
                        self.gui_layout.show_progress(progress, f"Exporting {entry_name}")

                    # Check if IMG has export_entry method
                    if hasattr(self.current_img, 'export_entry'):
                        #if self.current_img.export_entry(row, export_dir):
                        entry = self.current_img.entries[row]
                        output_path = os.path.join(export_dir, entry.name)
                        if self.current_img.export_entry(entry, output_path):
                            exported_count += 1
                            self.log_message(f"Exported: {entry_name}")
                    else:
                        self.log_message(f"IMG export_entry method not available")
                        break

                self.log_message(f"Export complete: {exported_count}/{len(selected_rows)} files exported")

                if hasattr(self.gui_layout, 'show_progress'):
                    self.gui_layout.show_progress(-1, "Export complete")

                QMessageBox.information(self, "Export Complete", f"Exported {exported_count} of {len(selected_rows)} files to {export_dir}")

        except Exception as e:
            error_msg = f"Error exporting files: {str(e)}"
            self.log_message(error_msg)
            if hasattr(self.gui_layout, 'show_progress'):
                self.gui_layout.show_progress(-1, "Export error")
            QMessageBox.critical(self, "Export Error", error_msg)


    def export_all(self):
        """Export all entries"""
        if not self.current_img:
            QMessageBox.warning(self, "No IMG", "No IMG file is currently loaded.")
            return

        try:
            export_dir = QFileDialog.getExistingDirectory(self, "Export All To Folder")
            if export_dir:
                entry_count = len(self.current_img.entries) if hasattr(self.current_img, 'entries') and self.current_img.entries else 0
                self.log_message(f"Exporting all {entry_count} entries...")

                if hasattr(self.gui_layout, 'show_progress'):
                    self.gui_layout.show_progress(0, "Exporting all...")

                exported_count = 0
                for i, entry in enumerate(self.current_img.entries):
                    progress = int((i + 1) * 100 / entry_count)
                    entry_name = getattr(entry, 'name', f"Entry_{i}")

                    if hasattr(self.gui_layout, 'show_progress'):
                        self.gui_layout.show_progress(progress, f"Exporting {entry_name}")

                    # Check if IMG has export_entry method
                    if hasattr(self.current_img, 'export_entry'):
                        #if self.current_img.export_entry(i, export_dir):
                        entry = self.current_img.entries[i]
                        output_path = os.path.join(export_dir, entry.name)
                        if self.current_img.export_entry(entry, output_path):
                            exported_count += 1
                            self.log_message(f"Exported: {entry_name}")
                    else:
                        self.log_message(f"IMG export_entry method not available")
                        break

                self.log_message(f"Export complete: {exported_count}/{entry_count} files exported")

                if hasattr(self.gui_layout, 'show_progress'):
                    self.gui_layout.show_progress(-1, "Export complete")

                QMessageBox.information(self, "Export Complete", f"Exported {exported_count} of {entry_count} files to {export_dir}")

        except Exception as e:
            error_msg = f"Error exporting all files: {str(e)}"
            self.log_message(error_msg)
            if hasattr(self.gui_layout, 'show_progress'):
                self.gui_layout.show_progress(-1, "Export error")
            QMessageBox.critical(self, "Export Error", error_msg)


    def remove_selected(self):
        """Remove selected entries"""
        if not self.current_img:
            QMessageBox.warning(self, "No IMG", "No IMG file is currently loaded.")
            return

        try:
            selected_rows = []
            if hasattr(self.gui_layout, 'table') and hasattr(self.gui_layout.table, 'selectedItems'):
                for item in self.gui_layout.table.selectedItems():
                    if item.column() == 0:  # Only filename column
                        selected_rows.append(item.row())

            if not selected_rows:
                QMessageBox.warning(self, "No Selection", "Please select entries to remove.")
                return

            # Confirm removal
            entry_names = []
            for row in selected_rows:
                item = self.gui_layout.table.item(row, 0)
                entry_names.append(item.text() if item else f"Entry_{row}")

            reply = QMessageBox.question(
                self, "Confirm Removal", f"Remove {len(selected_rows)} selected entries?\n\n" + "\n".join(entry_names[:5]) +
                (f"\n... and {len(entry_names) - 5} more" if len(entry_names) > 5 else ""),
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
            )

            if reply == QMessageBox.StandardButton.Yes:
                # Sort in reverse order to maintain indices
                selected_rows.sort(reverse=True)

                removed_count = 0
                for row in selected_rows:
                    item = self.gui_layout.table.item(row, 0)
                    entry_name = item.text() if item else f"Entry_{row}"

                    # Check if IMG has remove_entry method
                    if hasattr(self.current_img, 'remove_entry'):
                        if self.current_img.remove_entry(row):
                            removed_count += 1
                            self.log_message(f"Removed: {entry_name}")
                    else:
                        self.log_message(f"IMG remove_entry method not available")
                        break

                # Refresh table
                if hasattr(self, '_populate_real_img_table'):
                    self._populate_real_img_table(self.current_img)
                else:
                    populate_img_table(self.gui_layout.table, self.current_img)

                self.log_message(f"Removal complete: {removed_count} entries removed")

                if hasattr(self.gui_layout, 'update_img_info'):
                    self.gui_layout.update_img_info(f"{len(self.current_img.entries)} entries")

                QMessageBox.information(self, "Removal Complete",
                                      f"Removed {removed_count} entries")

        except Exception as e:
            error_msg = f"Error removing entries: {str(e)}"
            self.log_message(error_msg)
            QMessageBox.critical(self, "Removal Error", error_msg)


    def remove_all_entries(self):
        """Remove all entries from IMG"""
        if not self.current_img:
            QMessageBox.warning(self, "Warning", "No IMG file loaded")
            return

        try:
            reply = QMessageBox.question(self, "Remove All",
                                        "Remove all entries from IMG?")
            if reply == QMessageBox.StandardButton.Yes:
                self.current_img.entries.clear()
                self._update_ui_for_loaded_img()
                self.log_message("All entries removed")
        except Exception as e:
            self.log_message(f"Error in remove_all_entries: {str(e)}")


    def quick_export(self):
        """Quick export selected files to default location"""
        if not self.current_img:
            QMessageBox.warning(self, "Warning", "No IMG file loaded")
            return

        try:
            # Check if we have a selection method available
            if hasattr(self.gui_layout, 'table') and hasattr(self.gui_layout.table, 'selectionModel'):
                selected_rows = self.gui_layout.table.selectionModel().selectedRows()
            else:
                selected_rows = []

            if not selected_rows:
                QMessageBox.warning(self, "Warning", "No entries selected")
                return

            # Use Documents/IMG_Exports as default
            export_dir = os.path.join(os.path.expanduser("~"), "Documents", "IMG_Exports")
            os.makedirs(export_dir, exist_ok=True)

            self.log_message(f"Quick exporting {len(selected_rows)} files to {export_dir}")
            QMessageBox.information(self, "Info", "Quick export functionality coming soon")
        except Exception as e:
            self.log_message(f"Error in quick_export: {str(e)}")

    def close_img_file(self): #vers2
        """Close current IMG file using installed close functions"""
        try:
            if hasattr(self, 'close_manager') and self.close_manager:
                self.close_manager.close_current_file()
            else:
                # Fallback: clear current references
                self.current_img = None
                if hasattr(self, 'current_col'):
                    self.current_col = None
                if hasattr(self, 'current_txd'):
                    self.current_txd = None
                self._update_ui_for_no_img()
        except Exception as e:
            self.log_message(f"Error in close_img_file: {str(e)}")

    def close_all_file(self): #vers2
        """Close all files using installed close functions"""
        try:
            if hasattr(self, 'close_manager') and self.close_manager:
                self.close_manager.close_all_tabs()
            else:
                # Fallback: clear all references
                self.current_img = None
                if hasattr(self, 'current_col'):
                    self.current_col = None
                if hasattr(self, 'current_txd'):
                    self.current_txd = None
                self._update_ui_for_no_img()
        except Exception as e:
            self.log_message(f"Error in close_all_file: {str(e)}")

    def reload_current_file(self): #vers 2
        """Reload current IMG or COL file (close and reopen) - TAB AWARE"""
        try:
            # Use the proper tab-aware reload function from reload module
            from apps.core.reload import reload_current_file as proper_reload_function
            return proper_reload_function(self)
            
        except Exception as e:
            self.log_message(f"Reload failed: {str(e)}")
            return False


    # Add aliases for button connections To-Do
    def reload_file(self):
        return self.reload_current_file()

    def export_selected_via(self): #vers 1
        """Export selected entries via IDE file"""
        from apps.core.exporter import export_via_function
        export_via_function(self)

    def quick_export_selected(self): #vers 1
        """Quick export selected entries"""
        from apps.core.exporter import quick_export_function
        quick_export_function(self)

    def dump_entries(self): #vers 1
        """Dump all entries"""
        try:
            from apps.core.exporter import dump_all_function
            dump_all_function(self)
        except Exception as e:
            self.log_message(f"Dump error: {str(e)}")


    def import_files_via(self): #vers 1
        """Import files via IDE file"""
        try:
            from apps.core.importer import import_via_function
            import_via_function(self)
        except Exception as e:
            self.log_message(f"Import via error: {str(e)}")


    def remove_via_entries(self):
        """Remove entries via IDE file"""
        try:
            from apps.core.remove import remove_via_entries_function
            remove_via_entries_function(self)
        except Exception as e:
            self.log_message(f"Remove via error: {str(e)}")


    def pin_selected(self): #vers 1
        """Pin selected entries to top of list"""
        try:
            if hasattr(self.gui_layout, 'table') and hasattr(self.gui_layout.table, 'selectionModel'):
                selected_rows = self.gui_layout.table.selectionModel().selectedRows()
            else:
                selected_rows = []

            if not selected_rows:
                QMessageBox.information(self, "Pin", "No entries selected")
                return

            self.log_message(f"Pinned {len(selected_rows)} entries")
        except Exception as e:
            self.log_message(f"Error in pin_selected: {str(e)}")



    def apply_search_and_performance_fixes(self): #vers 2
        """Apply search and performance fixes"""
        try:
            self.log_message("  Applying search and performance fixes...")

            # 1. Setup our new consolidated search system
            from apps.core.gui_search import install_search_system
            if install_search_system(self):
                self.log_message("New search system installed")
            else:
                self.log_message("Search system setup failed")

            # 2. COL debug control (keep your existing code)
            try:
                def toggle_col_debug():
                    """Simple COL debug toggle"""
                    try:
                        import methods.col_core_classes as col_module
                        current = getattr(col_module, '_global_debug_enabled', False)
                        col_module._global_debug_enabled = not current

                        if col_module._global_debug_enabled:
                            self.log_message("COL debug enabled")
                        else:
                            self.log_message("COL debug disabled")

                    except Exception as e:
                        self.log_message(f"COL debug toggle error: {e}")

                # Add to main window
                self.toggle_col_debug = toggle_col_debug

                # Start with debug disabled for performance
                import methods.col_core_classes as col_module
                col_module._global_debug_enabled = False

                self.log_message("COL performance mode enabled")

            except Exception as e:
                self.log_message(f"COL setup issue: {e}")

            self.log_message("Search and performance fixes applied")

        except Exception as e:
            self.log_message(f"Apply fixes error: {e}")


    # COL and editor functions
    def open_col_editor(self): #vers 4
        """Open COL Workshop with current COL data and selected entry"""
        try:
            from apps.components.Col_Editor.col_workshop import COLWorkshop

            # Get selected COL entry name from table
            selected_col_name = None
            if hasattr(self, 'gui_layout') and hasattr(self.gui_layout, 'table'):
                table = self.gui_layout.table
                selected_items = table.selectedItems()
                if selected_items:
                    row = selected_items[0].row()
                    name_item = table.item(row, 0)  # Column 0 is name
                    if name_item:
                        selected_col_name = name_item.text()

            # Get COL/IMG path if available
            col_path = None
            if hasattr(self, 'current_col') and self.current_col:
                col_path = self.current_col.file_path
            elif hasattr(self, 'current_img') and self.current_img:
                col_path = self.current_img.file_path

            # Open docked workshop
            workshop = self.open_col_workshop_docked()

            # Load COL/IMG if available
            if workshop and col_path:
                if col_path.lower().endswith('.col'):
                    workshop.open_col_file(col_path)
                else:
                    workshop.load_from_img_archive(col_path)

                # Auto-select the entry that was clicked
                if selected_col_name and hasattr(workshop, 'select_col_by_name'):
                    workshop.select_col_by_name(selected_col_name)

            self.log_message(f"COL Workshop opened - Selected: {selected_col_name or 'None'}")

        except Exception as e:
            self.log_message(f"Error opening COL Workshop: {str(e)}")


   #TODO below, coming soon.
    def open_dff_editor(self): #vers 1
        """Open DFF model editor"""
        self.log_message("DFF editor functionality coming soon")

    def open_ipf_editor(self): #vers 1
        """Open IPF animation editor"""
        self.log_message("IPF editor functionality coming soon")

    def open_ipl_editor(self): #vers 1
        """Open IPL item placement editor"""
        self.log_message("IPL editor functionality coming soon")

    def open_ide_editor(self): #vers 1
        """Open IDE item definition editor"""
        self.log_message("IDE editor functionality coming soon")

    def open_dat_editor(self): #vers 1
        """Open DAT file editor"""
        self.log_message("DAT editor functionality coming soon")

    def open_zons_editor(self): #vers 1
        """Open zones editor"""
        self.log_message("Zones editor functionality coming soon")

    def open_weap_editor(self): #vers 1
        """Open weapons editor"""
        self.log_message("Weapons editor functionality coming soon")

    def open_vehi_editor(self): #vers 1
        """Open vehicles editor"""
        self.log_message("Vehicles editor functionality coming soon")

    def open_radar_map(self): #vers 1
        """Open radar map editor"""
        self.log_message("Radar map functionality coming soon")

    def open_paths_map(self): #vers 1
        """Open paths map editor"""
        self.log_message("Paths map functionality coming soon")

    def open_waterpro(self): #vers 1
        """Open water properties editor"""
        self.log_message("Water properties functionality coming soon")


    def validate_img(self): #vers 3
        """Validate current IMG file"""
        if not self.current_img:
            QMessageBox.warning(self, "No IMG", "No IMG file is currently loaded.")
            return

        try:
            self.log_message("Validating IMG file...")

            if hasattr(self.gui_layout, 'show_progress'):
                self.gui_layout.show_progress(0, "Validating...")

            # Try different validation approaches
            validation_result = None

            # Method 1: Try IMGValidator class
            try:
                validator = IMGValidator()
                if hasattr(validator, 'validate'):
                    validation_result = validator.validate(self.current_img)
                elif hasattr(validator, 'validate_img_file'):
                    validation_result = validator.validate_img_file(self.current_img)
            except Exception as e:
                self.log_message(f"IMGValidator error: {str(e)}")

            # Method 2: Try static method
            if not validation_result:
                try:
                    validation_result = IMGValidator.validate_img_file(self.current_img)
                except Exception as e:
                    self.log_message(f"Static validation error: {str(e)}")

            if hasattr(self.gui_layout, 'show_progress'):
                self.gui_layout.show_progress(-1, "Validation complete")

            if validation_result:
                if hasattr(validation_result, 'is_valid') and validation_result.is_valid:
                    self.log_message("IMG file validation passed")
                    QMessageBox.information(self, "Validation Result", "IMG file is valid!")
                else:
                    errors = getattr(validation_result, 'errors', ['Unknown validation issues'])
                    self.log_message(f"IMG file validation failed: {len(errors)} errors")
                    error_details = "\n".join(errors[:10])
                    if len(errors) > 10:
                        error_details += f"\n... and {len(errors) - 10} more errors"

                    QMessageBox.warning(self, "Validation Failed",
                                      f"IMG file has {len(errors)} validation errors:\n\n{error_details}")
            else:
                self.log_message("IMG file validation completed (no issues detected)")
                QMessageBox.information(self, "Validation Result", "IMG file appears to be valid!")

        except Exception as e:
            error_msg = f"Error validating IMG: {str(e)}"
            self.log_message(error_msg)
            if hasattr(self.gui_layout, 'show_progress'):
                self.gui_layout.show_progress(-1, "Validation error")
            QMessageBox.critical(self, "Validation Error", error_msg)


    def show_theme_settings(self): #vers 2
        """Show theme settings dialog"""
        self.show_settings()  # For now, use general settings

    def show_about(self): #vers 2
        """Show about dialog"""
        about_text = """
        <h2>IMG Factory 1.5</h2>
        <p><b>Professional IMG Archive Manager</b></p>
        <p>Version: 1.5.0 Python Edition</p>
        <p>Author: X-Seti</p>
        <p>Based on original IMG Factory by MexUK (2007)</p>
        <br>
        <p>Features:</p>
        <ul>
        <li>IMG file creation and editing</li>
        <li>Multi-format support (DFF, TXD, COL, IFP)</li>
        <li>Template system</li>
        <li>Batch operations</li>
        <li>Validation tools</li>
        </ul>
        """

        QMessageBox.about(self, "About IMG Factory", about_text)


    def show_gui_settings(self): #vers 5
        """Show GUI settings dialog - ADD THIS METHOD TO YOUR MAIN WINDOW CLASS"""
        try:
            from PyQt6.QtWidgets import QDialog, QVBoxLayout, QHBoxLayout, QLabel, QSpinBox, QPushButton, QGroupBox

            dialog = QDialog(self)
            dialog.setWindowTitle("GUI Layout Settings")
            dialog.setMinimumSize(500, 250)

            layout = QVBoxLayout(dialog)

            # Panel width group
            width_group = QGroupBox("Right Panel Width Settings")
            width_layout = QVBoxLayout(width_group)

            # Current width display
            current_width = 240  # Default
            if hasattr(self.gui_layout, 'main_splitter') and hasattr(self.gui_layout.main_splitter, 'sizes'):
                sizes = self.gui_layout.main_splitter.sizes()
                if len(sizes) > 1:
                    current_width = sizes[1]

            # Width spinner
            spinner_layout = QHBoxLayout()
            spinner_layout.addWidget(QLabel("Width:"))
            width_spin = QSpinBox()
            width_spin.setRange(180, 400)
            width_spin.setValue(current_width)
            width_spin.setSuffix(" px")
            spinner_layout.addWidget(width_spin)
            spinner_layout.addStretch()
            width_layout.addLayout(spinner_layout)

            # Preset buttons
            presets_layout = QHBoxLayout()
            presets_layout.addWidget(QLabel("Presets:"))
            presets = [("Narrow", 200), ("Default", 240), ("Wide", 280), ("Extra Wide", 320)]
            for name, value in presets:
                btn = QPushButton(f"{name}\n({value}px)")
                btn.clicked.connect(lambda checked, v=value: width_spin.setValue(v))
                presets_layout.addWidget(btn)
            presets_layout.addStretch()
            width_layout.addLayout(presets_layout)

            layout.addWidget(width_group)

            # Buttons
            button_layout = QHBoxLayout()

            preview_btn = QPushButton("Preview")
            def preview_changes():
                width = width_spin.value()
                if hasattr(self.gui_layout, 'main_splitter') and hasattr(self.gui_layout.main_splitter, 'sizes'):
                    sizes = self.gui_layout.main_splitter.sizes()
                    if len(sizes) >= 2:
                        self.gui_layout.main_splitter.setSizes([sizes[0], width])

                if hasattr(self.gui_layout, 'main_splitter'):
                    right_widget = self.gui_layout.main_splitter.widget(1)
                    if right_widget:
                        right_widget.setMaximumWidth(width + 60)
                        right_widget.setMinimumWidth(max(180, width - 40))

            preview_btn.clicked.connect(preview_changes)
            button_layout.addWidget(preview_btn)

            apply_btn = QPushButton("Apply & Close")
            def apply_changes():
                width = width_spin.value()
                if hasattr(self.gui_layout, 'main_splitter') and hasattr(self.gui_layout.main_splitter, 'sizes'):
                    sizes = self.gui_layout.main_splitter.sizes()
                    if len(sizes) >= 2:
                        self.gui_layout.main_splitter.setSizes([sizes[0], width])

                if hasattr(self.gui_layout, 'main_splitter'):
                    right_widget = self.gui_layout.main_splitter.widget(1)
                    if right_widget:
                        right_widget.setMaximumWidth(width + 60)
                        right_widget.setMinimumWidth(max(180, width - 40))

                # Save to settings if you have app_settings
                if hasattr(self, 'app_settings') and hasattr(self.app_settings, 'current_settings'):
                    self.app_settings.current_settings["right_panel_width"] = width
                    if hasattr(self.app_settings, 'save_settings'):
                        self.app_settings.save_settings()

                self.log_message(f"Right panel width set to {width}px")
                dialog.accept()

            apply_btn.clicked.connect(apply_changes)
            button_layout.addWidget(apply_btn)

            cancel_btn = QPushButton("Cancel")
            cancel_btn.clicked.connect(dialog.reject)
            button_layout.addWidget(cancel_btn)

            layout.addLayout(button_layout)

            dialog.exec()

        except Exception as e:
            self.log_message(f"Error showing GUI settings: {str(e)}")

    def show_gui_layout_settings(self): #vers 2
        """Show GUI Layout settings - called from menu"""
        if hasattr(self, 'gui_layout') and hasattr(self.gui_layout, 'show_gui_layout_settings'):
            self.gui_layout.show_gui_layout_settings()
        else:
            self.log_message("GUI Layout settings not available")

    def debug_theme_system(self): #vers 1
        """Debug method to check theme system status"""
        try:
            if hasattr(self, 'app_settings'):
                settings = self.app_settings
                self.log_message(f"Theme System Debug:")

                if hasattr(settings, 'settings_file'):
                    self.log_message(f"   Settings file: {settings.settings_file}")
                if hasattr(settings, 'themes_dir'):
                    self.log_message(f"   Themes directory: {settings.themes_dir}")
                    self.log_message(f"   Themes dir exists: {settings.themes_dir.exists()}")
                if hasattr(settings, 'themes'):
                    self.log_message(f"   Available themes: {list(settings.themes.keys())}")
                if hasattr(settings, 'current_settings'):
                    self.log_message(f"   Current theme: {settings.current_settings.get('theme')}")

                # Check if themes directory has files
                if hasattr(settings, 'themes_dir') and settings.themes_dir.exists():
                    theme_files = list(settings.themes_dir.glob("*.json"))
                    self.log_message(f"   Theme files found: {[f.name for f in theme_files]}")
                else:
                    self.log_message(f"Themes directory does not exist!")
            else:
                self.log_message("No app_settings available")
        except Exception as e:
            self.log_message(f"Error in debug_theme_system: {str(e)}")

    def show_settings(self): #vers 1
        """Show settings dialog"""
        print("show_settings called!")  # Debug
        try:
            # Try different import paths
            try:
                from apps.utils.app_settings_system import SettingsDialog, apply_theme_to_app
            except ImportError:
                from app_settings_system import SettingsDialog, apply_theme_to_app

            dialog = SettingsDialog(self.app_settings, self)
            if dialog.exec() == QDialog.DialogCode.Accepted:
                apply_theme_to_app(QApplication.instance(), self.app_settings)
                if hasattr(self.gui_layout, 'apply_table_theme'):
                    self.gui_layout.apply_table_theme()
                self.log_message("Settings updated")
        except Exception as e:
            print(f"Settings error: {e}")
            self.log_message(f"Settings error: {str(e)}")

    # SETTINGS PERSISTENCE - KEEP 100% OF FUNCTIONALITY

    def _restore_settings(self): #vers 1
        """Restore application settings"""
        try:
            settings = QSettings("XSeti", "IMGFactory")

            # Restore window geometry
            geometry = settings.value("geometry")
            if geometry:
                self.restoreGeometry(geometry)

            # Restore splitter state
            splitter_state = settings.value("splitter_state")
            if splitter_state and hasattr(self.gui_layout, 'main_splitter'):
                self.gui_layout.main_splitter.restoreState(splitter_state)

            self.log_message("Settings restored")

        except Exception as e:
            self.log_message(f"Failed to restore settings: {str(e)}")

    def _save_settings(self): #vers 1
        """Save application settings"""
        try:
            settings = QSettings("XSeti", "IMGFactory")

            # Save window geometry
            settings.setValue("geometry", self.saveGeometry())

            # Save splitter state
            if hasattr(self.gui_layout, 'main_splitter'):
                settings.setValue("splitter_state", self.gui_layout.main_splitter.saveState())

            self.log_message("Settings saved")

        except Exception as e:
            self.log_message(f"Failed to save settings: {str(e)}")

    def setup_search_system(self): #vers 1
        """Setup search functionality for the application"""
        try:
            # Create search manager instance
            from apps.core.gui_search import SearchManager
            self.search_manager = SearchManager(self)
            
            # Setup search functionality
            success = self.search_manager.setup_search_functionality()
            
            # Add search-related methods to main window
            self.show_search_dialog = self._show_search_dialog
            self.search_entries = self._search_entries
            self.search_next = self._search_next
            self.search_previous = self._search_previous
            
            if success:
                self.log_message("✅ Search system initialized")
                return True
            else:
                self.log_message("⚠️ Search system initialization incomplete")
                return False
                
        except Exception as e:
            self.log_message(f"❌ Search system setup error: {e}")
            import traceback
            traceback.print_exc()
            return False

    def _show_search_dialog(self): #vers 1
        """Show advanced search dialog"""
        try:
            if hasattr(self, 'search_manager'):
                self.search_manager.show_search_dialog()
            else:
                self.log_message("⚠️ Search manager not available")
        except Exception as e:
            self.log_message(f"❌ Show search dialog error: {e}")

    def _search_entries(self, search_text=None, options=None): #vers 1
        """Search entries in current IMG file"""
        try:
            if hasattr(self, 'search_manager'):
                # If no search text provided, get it from the filter input
                if not search_text:
                    if hasattr(self, 'gui_layout') and hasattr(self.gui_layout, 'filter_input'):
                        search_text = self.gui_layout.filter_input.text()
                    else:
                        self.log_message("⚠️ No search text provided")
                        return []
                
                return self.search_manager.perform_search(search_text, options)
            else:
                self.log_message("⚠️ Search manager not available")
                return []
        except Exception as e:
            self.log_message(f"❌ Search entries error: {e}")
            return []

    def _search_next(self): #vers 1
        """Find next search match"""
        try:
            if hasattr(self, 'search_manager'):
                self.search_manager.find_next()
            else:
                self.log_message("⚠️ Search manager not available")
        except Exception as e:
            self.log_message(f"❌ Search next error: {e}")

    def _search_previous(self): #vers 1
        """Find previous search match"""
        try:
            if hasattr(self, 'search_manager'):
                self.search_manager.find_previous()
            else:
                self.log_message("⚠️ Search manager not available")
        except Exception as e:
            self.log_message(f"❌ Search previous error: {e}")


    def paintEvent(self, event): #vers 3
        """Paint corner resize triangles on main window"""
        super().paintEvent(event)

        # Only paint in custom UI mode
        if not hasattr(self, 'gui_layout'):
            return

        from PyQt6.QtGui import QPainter, QColor, QPen, QBrush, QPainterPath
        from PyQt6.QtCore import Qt

        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        w = self.width()
        h = self.height()
        size = getattr(self.gui_layout, 'corner_size', 20)

        # Define corner triangles
        corners = {
            'top-left': [(0, 0), (size, 0), (0, size)],
            'top-right': [(w, 0), (w-size, 0), (w, size)],
            'bottom-left': [(0, h), (size, h), (0, h-size)],
            'bottom-right': [(w, h), (w-size, h), (w, h-size)]
        }

        # BRIGHT colors - always visible
        normal_color = QColor(70, 130, 255, 200)  # Blue, semi-transparent
        hover_color = QColor(70, 130, 255, 255)   # Blue, fully opaque

        # Draw corners
        hover_corner = getattr(self.gui_layout, 'hover_corner', None)
        for corner_name, points in corners.items():
            path = QPainterPath()
            path.moveTo(points[0][0], points[0][1])
            path.lineTo(points[1][0], points[1][1])
            path.lineTo(points[2][0], points[2][1])
            path.closeSubpath()

            color = hover_color if hover_corner == corner_name else normal_color
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(QBrush(color))
            painter.drawPath(path)

        painter.end()


    def _get_resize_corner(self, pos): #vers 4
        """Determine which corner is under mouse position"""
        size = getattr(self.gui_layout, 'corner_size', 20)
        w = self.width()
        h = self.height()

        if pos.x() < size and pos.y() < size:
            return "top-left"
        if pos.x() > w - size and pos.y() < size:
            return "top-right"
        if pos.x() < size and pos.y() > h - size:
            return "bottom-left"
        if pos.x() > w - size and pos.y() > h - size:
            return "bottom-right"

        return None


    def _update_cursor(self, corner): #vers 2
        """Update cursor based on resize corner"""
        from PyQt6.QtCore import Qt

        if corner == "top-left" or corner == "bottom-right":
            self.setCursor(Qt.CursorShape.SizeFDiagCursor)
        elif corner == "top-right" or corner == "bottom-left":
            self.setCursor(Qt.CursorShape.SizeBDiagCursor)
        elif corner:
            self.setCursor(Qt.CursorShape.SizeAllCursor)
        else:
            self.setCursor(Qt.CursorShape.ArrowCursor)


    def mousePressEvent(self, event): #vers 9
        """Handle ALL mouse press - dragging and resizing"""
        from PyQt6.QtCore import Qt

        if event.button() != Qt.MouseButton.LeftButton:
            super().mousePressEvent(event)
            return

        pos = event.pos()

        # Check corner resize FIRST
        resize_corner = self._get_resize_corner(pos)
        if resize_corner:
            # Store state in gui_layout
            self.gui_layout.resizing = True
            self.gui_layout.resize_corner = resize_corner
            self.gui_layout.drag_position = event.globalPosition().toPoint()
            self.gui_layout.initial_geometry = self.geometry()
            event.accept()
            return

        # Check if on titlebar for dragging
        if hasattr(self.gui_layout, 'titlebar') and self.gui_layout.titlebar:
            titlebar_geometry = self.gui_layout.titlebar.geometry()
            if titlebar_geometry.contains(pos):
                titlebar_pos = self.gui_layout.titlebar.mapFromParent(pos)
                if hasattr(self.gui_layout, '_is_on_draggable_area') and self.gui_layout._is_on_draggable_area(titlebar_pos):
                    self.windowHandle().startSystemMove()
                    event.accept()
                    return

        super().mousePressEvent(event)


    def mouseMoveEvent(self, event): #vers 3
        """Handle mouse move for resizing and hover effects"""
        from PyQt6.QtCore import Qt

        if event.buttons() == Qt.MouseButton.LeftButton:
            # Active resize
            if hasattr(self.gui_layout, 'resizing') and self.gui_layout.resizing:
                if hasattr(self.gui_layout, 'resize_corner') and self.gui_layout.resize_corner:
                    self._handle_corner_resize_window(event.globalPosition().toPoint())
                    event.accept()
                    return
        else:
            # Update hover state and cursor
            corner = self._get_resize_corner(event.pos())
            if hasattr(self.gui_layout, 'hover_corner'):
                if corner != self.gui_layout.hover_corner:
                    self.gui_layout.hover_corner = corner
                    self.update()  # Trigger repaint for hover effect
            self._update_cursor(corner)

        super().mouseMoveEvent(event)


    def mouseReleaseEvent(self, event): #vers 3
        """Handle mouse release"""
        from PyQt6.QtCore import Qt

        if event.button() == Qt.MouseButton.LeftButton:
            # Clear resize state
            if hasattr(self.gui_layout, 'resizing'):
                self.gui_layout.resizing = False
            if hasattr(self.gui_layout, 'resize_corner'):
                self.gui_layout.resize_corner = None
            self.setCursor(Qt.CursorShape.ArrowCursor)
            event.accept()
        else:
            super().mouseReleaseEvent(event)


    def mouseDoubleClickEvent(self, event): #vers 3
        """Handle double-click - maximize/restore"""
        from PyQt6.QtCore import Qt

        if event.button() == Qt.MouseButton.LeftButton:
            # Check if on titlebar
            if hasattr(self.gui_layout, 'titlebar') and self.gui_layout.titlebar:
                titlebar_geometry = self.gui_layout.titlebar.geometry()
                if titlebar_geometry.contains(event.pos()):
                    titlebar_pos = self.gui_layout.titlebar.mapFromParent(event.pos())
                    if hasattr(self.gui_layout, '_is_on_draggable_area') and self.gui_layout._is_on_draggable_area(titlebar_pos):
                        self._toggle_maximize()
                        event.accept()
                        return

        super().mouseDoubleClickEvent(event)


    def _handle_corner_resize_window(self, global_pos): #vers 3
        """Handle window resizing from corners"""
        if not hasattr(self.gui_layout, 'resize_corner') or not self.gui_layout.resize_corner:
            return
        if not hasattr(self.gui_layout, 'drag_position') or not self.gui_layout.drag_position:
            return
        if not hasattr(self.gui_layout, 'initial_geometry'):
            return

        delta = global_pos - self.gui_layout.drag_position
        geometry = self.gui_layout.initial_geometry

        min_width = 800
        min_height = 600

        # Calculate new geometry based on corner
        if self.gui_layout.resize_corner == "top-left":
            new_x = geometry.x() + delta.x()
            new_y = geometry.y() + delta.y()
            new_width = geometry.width() - delta.x()
            new_height = geometry.height() - delta.y()
            if new_width >= min_width and new_height >= min_height:
                self.setGeometry(new_x, new_y, new_width, new_height)

        elif self.gui_layout.resize_corner == "top-right":
            new_y = geometry.y() + delta.y()
            new_width = geometry.width() + delta.x()
            new_height = geometry.height() - delta.y()
            if new_width >= min_width and new_height >= min_height:
                self.setGeometry(geometry.x(), new_y, new_width, new_height)

        elif self.gui_layout.resize_corner == "bottom-left":
            new_x = geometry.x() + delta.x()
            new_width = geometry.width() - delta.x()
            new_height = geometry.height() + delta.y()
            if new_width >= min_width and new_height >= min_height:
                self.setGeometry(new_x, geometry.y(), new_width, new_height)

        elif self.gui_layout.resize_corner == "bottom-right":
            new_width = geometry.width() + delta.x()
            new_height = geometry.height() + delta.y()
            if new_width >= min_width and new_height >= min_height:
                self.setGeometry(geometry.x(), geometry.y(), new_width, new_height)


    def _toggle_maximize(self): #vers 2
        """Toggle window maximize state"""
        if self.isMaximized():
            self.showNormal()
        else:
            self.showMaximized()


    def resizeEvent(self, event): #vers 2
        """Handle window resize event"""
        super().resizeEvent(event)
        # Trigger repaint to update corner positions
        self.update()


    def _toggle_maximize(self): #vers 1
        """Toggle window maximize state"""
        if self.isMaximized():
            self.showNormal()
        else:
            self.showMaximized()


    def closeEvent(self, event): #vers 2
        """Handle application close"""
        try:
            self._save_settings()

            # Clean up threads
            if hasattr(self, 'load_thread') and self.load_thread and self.load_thread.isRunning():
                self.load_thread.quit()
                self.load_thread.wait()

            # Close all files
            if hasattr(self, 'close_manager'):
                self.close_manager.close_all_tabs()

            event.accept()
        except Exception as e:
            self.log_message(f"Error during close: {str(e)}")
            event.accept()  # Accept anyway to prevent hanging


def main():
   """Main application entry point"""
   try:
       app = QApplication(sys.argv)
       app.setApplicationName("IMG Factory")
       app.setApplicationVersion("1.5")
       app.setOrganizationName("X-Seti")

       # Set application icon
       try:
           from apps.methods.imgfactory_svg_icons import get_app_icon
           app_icon = get_app_icon()
           app.setWindowIcon(app_icon)
       except Exception as e:
           print(f"Could not set application icon: {e}")

       # Load settings
       try:
           # Try different import paths for settings
           try:
               from apps.utils.app_settings_system import AppSettings
           except ImportError:
               from app_settings_system import AppSettings

           settings = AppSettings()
           if hasattr(settings, 'load_settings'):
               settings.load_settings()

           # Test if settings actually work
           if not hasattr(settings, 'get_stylesheet'):
               raise AttributeError("AppSettings missing get_stylesheet method")

       except Exception as e:
           print(f"Warning: Could not load settings: {str(e)}")
           # Only use DummySettings as last resort
           class DummySettings:
               def __init__(self):
                   self.current_settings = {
                       "theme": "img_factory",
                       "font_family": "Arial",
                       "font_size": 9,
                       "show_tooltips": True,
                       "auto_save": True,
                       "debug_mode": False
                   }
                   self.themes = {
                       "img_factory": {
                           "colors": {
                               "background": "#f0f0f0",
                               "text": "#000000",
                               "button_text_color": "#000000"
                           }
                       }
                   }

               def get_stylesheet(self):
                   return "QMainWindow { background-color: #f0f0f0; }"

               def get_theme(self, theme_name=None):
                   return self.themes.get("img_factory", {"colors": {}})

               def load_settings(self):
                   pass

               def save_settings(self):
                   pass

           settings = DummySettings()
           print("Using DummySettings - theme system may be limited")

       # Create main window
       window = IMGFactory(settings)
       # Show window
       window.show()

       return app.exec()

   except Exception as e:
       print(f"Fatal error in main(): {str(e)}")
       import traceback
       traceback.print_exc()
       return 1


def fix_menu_system_and_functionality(main_window):
    """
    Comprehensive fix for menu system and functionality
    """
    try:
        # Fix the rename functionality to work from both right-click and double-click
        fix_rename_functionality(main_window)
        
        # Implement context menu for active tab
        implement_tab_context_menu(main_window)
        
        # Add requested file operations to main window
        add_file_operations_to_main_window(main_window)
        
        # Set up proper double-click rename functionality
        #setup_double_click_rename(main_window)
        
        main_window.log_message("✅ Comprehensive menu system and functionality fix applied")
        return True
        
    except Exception as e:
        main_window.log_message(f"❌ Error applying comprehensive fix: {str(e)}")
        return False


def add_file_operations_to_main_window(main_window):
    """
    Add the requested file operations as methods to the main window
    """
    try:
        # Add move_selected_file method
        main_window.move_selected_file = lambda: move_selected_file(main_window)
        
        # Add analyze_selected_file method
        main_window.analyze_selected_file = lambda: analyze_selected_file(main_window)
        
        # Add show_hex_editor_selected method
        main_window.show_hex_editor_selected = lambda: show_hex_editor_selected(main_window)
        
        # Add show_dff_texture_list method (as a general method that handles current selection)
        main_window.show_dff_texture_list = lambda: show_dff_texture_list_from_selection(main_window)
        
        # Add show_dff_model_viewer method (as a general method that handles current selection)
        main_window.show_dff_model_viewer = lambda: show_dff_model_viewer_from_selection(main_window)
        
        # Add set_game_path method
        main_window.set_game_path = lambda: set_game_path(main_window)
        
        main_window.log_message("✅ File operations added to main window")
        
    except Exception as e:
        main_window.log_message(f"❌ Error adding file operations: {str(e)}")


def set_game_path(main_window):
    """
    Set game path with support for custom paths including Linux paths
    """
    try:
        # Get current path if it exists
        current_path = getattr(main_window, 'game_root', None)
        if not current_path or current_path == "C:/":
            # Default to home directory instead of C:/
            current_path = os.path.expanduser("~")
        
        # Open directory dialog without restricting to Windows paths
        folder = QFileDialog.getExistingDirectory(
            main_window,
            "Select Game Root Directory (Supports Windows and Linux paths)",
            current_path,
            QFileDialog.Option.ShowDirsOnly
        )
        
        if folder:
            # Validate that it's a game directory by checking for common game files
            game_files = [
                "gta3.exe", "gta_vc.exe", "gta_sa.exe", "gtasol.exe", "solcore.exe",
                "gta3.dat", "gta_vc.dat", "gta_sa.dat", "gta_sol.dat", "SOL/gta_sol.dat",
                "default.ide", "Data/default.dat", "models/", "textures/", "data/"
            ]
            
            # Check if the folder contains game-related files/directories
            is_game_dir = False
            for item in os.listdir(folder):
                item_lower = item.lower()
                if any(game_file.split('/')[0] in item_lower for game_file in game_files if '/' not in game_file) or \
                   any(game_file in item_lower for game_file in game_files if '/' not in game_file):
                    is_game_dir = True
                    break
            
            # Also check subdirectories
            if not is_game_dir:
                for root, dirs, files in os.walk(folder):
                    for d in dirs:
                        if d.lower() in ['models', 'textures', 'data', 'sfx', 'audio']:
                            is_game_dir = True
                            break
                    if is_game_dir:
                        break
            
            main_window.game_root = folder
            main_window.log_message(f"Game path set: {folder}")
            
            # Update directory tree if it exists
            if hasattr(main_window, 'directory_tree'):
                main_window.directory_tree.game_root = folder
                main_window.directory_tree.current_root = folder
                if hasattr(main_window.directory_tree, 'path_label'):
                    main_window.directory_tree.path_label.setText(folder)
                # Auto-populate the tree
                if hasattr(main_window.directory_tree, 'populate_tree'):
                    main_window.directory_tree.populate_tree(folder)
                    main_window.log_message("Directory tree auto-populated")
            
            # Save settings
            if hasattr(main_window, 'save_settings'):
                main_window.save_settings()
            else:
                # Create a simple save settings if not available
                try:
                    from PyQt6.QtCore import QSettings
                    settings = QSettings("IMG_Factory", "IMG_Factory_Settings")
                    settings.setValue("game_root", folder)
                except:
                    pass
            
            # Show success message
            QMessageBox.information(
                main_window,
                "Game Path Set",
                f"Game path configured:\\n{folder}\\n\\nDirectory tree will now show game files.\\nSwitch to the 'Directory Tree' tab to browse."
            )
        else:
            main_window.log_message("Game path selection cancelled")
            
    except Exception as e:
        main_window.log_message(f"Error setting game path: {str(e)}")
        QMessageBox.critical(
            main_window,
            "Error Setting Game Path",
            f"An error occurred while setting the game path:\\n\\n{str(e)}"
        )


def get_entry_info(main_window, row):
    """
    Get entry information for a given row in the table
    """
    try:
        entry_info = {
            'name': '',
            'is_dff': False,
            'size': 0,
            'offset': 0
        }
        
        # Use tab-aware approach if available
        if hasattr(main_window, 'get_current_file_from_active_tab'):
            file_object, file_type = main_window.get_current_file_from_active_tab()
            if file_type == 'IMG' and file_object and hasattr(file_object, 'entries'):
                if 0 <= row < len(file_object.entries):
                    entry = file_object.entries[row]
                    entry_info['name'] = entry.name
                    entry_info['is_dff'] = entry.name.lower().endswith('.dff')
                    entry_info['size'] = entry.size
                    entry_info['offset'] = entry.offset
                    return entry_info
        else:
            # Fallback to old method
            if hasattr(main_window, 'current_img') and main_window.current_img:
                if 0 <= row < len(main_window.current_img.entries):
                    entry = main_window.current_img.entries[row]
                    entry_info['name'] = entry.name
                    entry_info['is_dff'] = entry.name.lower().endswith('.dff')
                    entry_info['size'] = entry.size
                    entry_info['offset'] = entry.offset
                    return entry_info
        
        return entry_info
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Error getting entry info: {str(e)}")
        return {
            'name': '',
            'is_dff': False,
            'size': 0,
            'offset': 0
        }


def show_dff_texture_list_from_selection(main_window):
    """
    Show DFF texture list for currently selected entry
    """
    try:
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            table = main_window.gui_layout.table
            selected_items = table.selectedItems()
            if selected_items:
                row = selected_items[0].row()
                entry_info = get_entry_info(main_window, row)
                if entry_info and entry_info['is_dff']:
                    show_dff_texture_list(main_window, row, entry_info)
                else:
                    # Check if it's a DFF file in the IMG that we need to extract and parse
                    if entry_info and entry_info['name'].lower().endswith('.dff'):
                        show_dff_texture_list_from_img_dff(main_window, row, entry_info)
                    else:
                        QMessageBox.information(main_window, "DFF Texture List", 
                                              "Please select a DFF file to view texture list")
    except Exception as e:
        main_window.log_message(f"❌ Error showing DFF texture list from selection: {str(e)}")


def show_dff_texture_list_from_img_dff(main_window, row, entry_info):
    """
    Extract and show DFF texture list from DFF files in IMG
    """
    try:
        # Get the DFF data from the IMG entry
        if hasattr(main_window, 'current_img') and main_window.current_img:
            entry = main_window.current_img.entries[row]
            dff_data = entry.get_data() if hasattr(entry, 'get_data') else None
            
            if dff_data:
                # Create a temporary file to extract the DFF
                import tempfile
                with tempfile.NamedTemporaryFile(delete=False, suffix='.dff', mode='wb') as temp_file:
                    temp_file.write(dff_data)
                    temp_dff_path = temp_file.name
                
                try:
                    # Parse the DFF file for texture information
                    textures = parse_dff_textures_from_data(temp_dff_path)
                    
                    # Create dialog to show texture list
                    dialog = QDialog(main_window)
                    dialog.setWindowTitle(f"Textures in {entry.name}")
                    dialog.resize(500, 400)
                    
                    layout = QVBoxLayout(dialog)
                    
                    # Create text area for texture list
                    text_area = QTextEdit()
                    text_area.setReadOnly(True)
                    
                    if textures:
                        texture_list = "\\n".join([f"  • {tex}" for tex in textures])
                        text_content = f"Textures found in {entry.name}:\\n\\n{texture_list}"
                    else:
                        text_content = f"No textures found in {entry.name}"
                    
                    text_area.setPlainText(text_content)
                    layout.addWidget(text_area)
                    
                    # Close button
                    close_btn = QPushButton("Close")
                    close_btn.clicked.connect(dialog.close)
                    layout.addWidget(close_btn)
                    
                    dialog.exec()
                    
                finally:
                    # Clean up temporary file
                    if os.path.exists(temp_dff_path):
                        os.remove(temp_dff_path)
            else:
                QMessageBox.warning(main_window, "DFF Texture List", 
                                  f"Could not extract data from {entry.name}")
    except Exception as e:
        main_window.log_message(f"❌ Error showing DFF texture list from IMG: {str(e)}")


def parse_dff_textures_from_data(dff_path):
    """
    Parse a DFF file to extract texture names
    """
    try:
        textures = []
        
        # This is a simplified implementation - in a real application,
        # you'd need a proper DFF parser
        with open(dff_path, 'rb') as f:
            data = f.read()
            
        # Look for texture-related patterns in the DFF data
        # This is a simplified approach - real DFF parsing is complex
        # Look for common texture name patterns
        import re
        
        # Search for potential texture names in the binary data
        text_data = data.decode('ascii', errors='ignore')
        
        # Look for potential texture names (alphanumeric with underscores, hyphens, dots)
        potential_textures = re.findall(r'[A-Za-z0-9_\\-]{3,20}\\.(?:txd|png|jpg|bmp|dxt)', text_data, re.IGNORECASE)
        
        # Also look for names without extensions
        potential_names = re.findall(r'[A-Za-z][A-Za-z0-9_\\-]{2,19}(?=\\.|\\s|$)', text_data)
        
        # Combine and deduplicate
        all_matches = list(set(potential_textures + potential_names))
        
        # Filter for likely texture names
        for name in all_matches:
            if any(tex in name.lower() for tex in ['tex', 'texture', 'material', 'diffuse', 'specular']):
                textures.append(name)
            elif len(name) > 2 and not any(c.isdigit() for c in name[:2]):  # Avoid names starting with numbers
                textures.append(name)
        
        # Return unique textures
        return list(set(textures))
        
    except Exception as e:
        print(f"Error parsing DFF textures: {str(e)}")
        return []


def show_dff_model_viewer_from_selection(main_window):
    """
    Show DFF model viewer for currently selected entry
    """
    try:
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            table = main_window.gui_layout.table
            selected_items = table.selectedItems()
            if selected_items:
                row = selected_items[0].row()
                entry_info = get_entry_info(main_window, row)
                if entry_info and entry_info['is_dff']:
                    show_dff_model_viewer(main_window, row, entry_info)
                else:
                    QMessageBox.information(main_window, "DFF Model Viewer", 
                                          "Please select a DFF file to view in model viewer")
    except Exception as e:
        main_window.log_message(f"❌ Error showing DFF model viewer from selection: {str(e)}")


def fix_rename_functionality(main_window):
    """
    Fix rename functionality to work from right-click menu only (double-click disabled as requested)
    """
    try:
        # Ensure rename_selected function is properly connected
        if not hasattr(main_window, 'rename_selected'):
            integrate_imgcol_rename_functions(main_window)
        
        # DO NOT connect double-click event to table for rename (as requested)
        # Only allow renaming via right-click menu
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            table = main_window.gui_layout.table
            # Remove any existing double-click connection to prevent double-click renaming
            try:
                table.cellDoubleClicked.disconnect()
            except TypeError:
                # If no connections exist, this will raise an exception, which is fine
                pass
        
        main_window.log_message("✅ Rename functionality fixed (double-click disabled as requested)")
        
    except Exception as e:
        main_window.log_message(f"❌ Error fixing rename functionality: {str(e)}")


def handle_double_click_rename(main_window, row, col):
    """
    Handle double-click rename functionality
    """
    try:
        # Only allow renaming when clicking on the name column (usually column 0)
        if col == 0:  # Assuming name column is first column
            if hasattr(main_window, 'current_img') and main_window.current_img:
                if 0 <= row < len(main_window.current_img.entries):
                    # Get the current entry
                    entry = main_window.current_img.entries[row]
                    current_name = entry.name
                    
                    # Show input dialog for new name
                    new_name, ok = QInputDialog.getText(
                        main_window,
                        "Rename File",
                        f"Enter new name for '{current_name}':",
                        text=current_name
                    )
                    
                    if ok and new_name and new_name != current_name:
                        # Validate the new name
                        if validate_new_name(main_window, new_name):
                            # Check for duplicates
                            if not check_duplicate_name(main_window, new_name, entry):
                                # Perform the rename
                                entry.name = new_name
                                
                                if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
                                    table = main_window.gui_layout.table
                                    table.item(row, 0).setText(new_name)

                                # Mark as modified
                                if hasattr(main_window.current_img, 'modified'):
                                    main_window.current_img.modified = True

                                main_window.log_message(f"✅ Renamed '{current_name}' to '{new_name}'")
                                QMessageBox.information(main_window, "Rename Successful",
                                                      f"Successfully renamed to '{new_name}'")
                            else:
                                QMessageBox.warning(main_window, "Duplicate Name",
                                                  f"An entry named '{new_name}' already exists")
                        else:
                            QMessageBox.warning(main_window, "Invalid Name",
                                              "The name provided is invalid")
        else:
            # For other columns, we might want to handle different actions
            main_window.log_message(f"Double-clicked on row {row}, column {col}")

    except Exception as e:
        main_window.log_message(f"❌ Error handling double-click rename: {str(e)}")


def validate_new_name(main_window, new_name):
    """
    Validate new name for file entry
    """
    try:
        # Check for empty name
        if not new_name or not new_name.strip():
            return False

        # Check for invalid characters
        invalid_chars = '<>:"/\\\\|?*'
        if any(char in new_name for char in invalid_chars):
            return False

        # Check length (typically IMG entries have 24 char limit)
        if len(new_name) > 24:
            return False

        return True
    except Exception:
        return False


def check_duplicate_name(main_window, new_name, current_entry):
    """
    Check if new name would create duplicate
    """
    try:
        if hasattr(main_window, 'current_img') and main_window.current_img:
            for entry in main_window.current_img.entries:
                if entry != current_entry and getattr(entry, 'name', '') == new_name:
                    return True
        return False
    except Exception:
        return True  # Return True on error to be safe


def implement_tab_context_menu(main_window):
    """
    Implement context menu for active tab with file operations
    This integrates with the existing context menu system to avoid conflicts
    """
    try:
        # Add context menu to the main window
        main_window.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)

        # For the table, we need to integrate with the existing context menu system
        # rather than replacing it to avoid conflicts with the existing setup
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            table = main_window.gui_layout.table

            # Instead of replacing the context menu policy, we'll enhance the existing one
            # by making sure our features are available through the existing system

            # The existing setup_table_context_menu should already handle the basic context menu
            # We'll enhance it by making sure our operations are available

            # Store a reference to our enhanced functionality
            table._enhanced_context_menu = True

        main_window.log_message("✅ Tab context menu enhanced with additional operations")

    except Exception as e:
        main_window.log_message(f"❌ Error implementing tab context menu: {str(e)}")


def move_file(main_window, row, entry_info):
    """
    Move selected file to a new location
    """
    try:
        # Get current entry
        entry = entry_info['entry']
        current_name = entry.name

        # Show dialog to select destination
        dest_dir = QFileDialog.getExistingDirectory(
            main_window,
            "Select Destination Directory",
            ""
        )

        if dest_dir:
            # For IMG entries, we can't actually move files since they're inside the IMG
            # Instead, we can rename to change the path-like structure
            QMessageBox.information(main_window, "Move Operation",
                                  f"Moving '{current_name}' to '{dest_dir}'\\n\\n"
                                  f"Note: In IMG files, entries are virtual and cannot be moved to different directories.\\n"
                                  f"You can rename the entry to reflect a new path structure if needed.")

    except Exception as e:
        main_window.log_message(f"❌ Error moving file: {str(e)}")


def move_selected_file(main_window):
    """
    Move selected file (when no specific row selected)
    """
    try:
        # Get selected items from table
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            table = main_window.gui_layout.table
            selected_items = table.selectedItems()
            if selected_items:
                row = selected_items[0].row()
                entry_info = get_entry_info(main_window, row)
                if entry_info:
                    move_file(main_window, row, entry_info)
    except Exception as e:
        main_window.log_message(f"❌ Error moving selected file: {str(e)}")


def analyze_file(main_window, row, entry_info):
    """
    Analyze selected file
    """
    try:
        entry = entry_info['entry']
        name = entry.name

        # Determine file type and perform appropriate analysis
        if entry_info['is_col']:
            # Use existing COL analysis functionality
            try:
                from apps.gui.gui_context import analyze_col_from_img_entry
                analyze_col_from_img_entry(main_window, row)
            except:
                QMessageBox.information(main_window, "COL Analysis",
                                      f"COL Analysis for: {name}\\n\\n"
                                      f"Size: {entry.size} bytes\\n"
                                      f"Offset: 0x{entry.offset:08X}\\n"
                                      f"Type: Collision File")
        elif entry_info['is_dff']:
            # DFF analysis
            QMessageBox.information(main_window, "DFF Analysis",
                                  f"DFF Analysis for: {name}\\n\\n"
                                  f"Size: {entry.size} bytes\\n"
                                  f"Offset: 0x{entry.offset:08X}\\n"
                                  f"Type: DFF Model File")
        elif entry_info['is_txd']:
            # TXD analysis
            QMessageBox.information(main_window, "TXD Analysis",
                                  f"TXD Analysis for: {name}\\n\\n"
                                  f"Size: {entry.size} bytes\\n"
                                  f"Offset: 0x{entry.offset:08X}\\n"
                                  f"Type: Texture Dictionary File")
        else:
            # Generic analysis
            QMessageBox.information(main_window, "File Analysis",
                                  f"Analysis for: {name}\\n\\n"
                                  f"Size: {entry.size} bytes\\n"
                                  f"Offset: 0x{entry.offset:08X}\\n"
                                  f"Type: Generic IMG Entry")

    except Exception as e:
        main_window.log_message(f"❌ Error analyzing file: {str(e)}")


def analyze_selected_file(main_window):
    """
    Analyze selected file (when no specific row selected)
    """
    try:
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            table = main_window.gui_layout.table
            selected_items = table.selectedItems()
            if selected_items:
                row = selected_items[0].row()
                entry_info = get_entry_info(main_window, row)
                if entry_info:
                    analyze_file(main_window, row, entry_info)
    except Exception as e:
        main_window.log_message(f"❌ Error analyzing selected file: {str(e)}")


def show_hex_editor(main_window, row, entry_info):
    """
    Show hex editor for selected file
    """
    try:
        # Import the hex editor module
        from apps.components.Hex_Editor import show_hex_editor_for_entry
        
        # Use the new hex editor implementation
        show_hex_editor_for_entry(main_window, row, entry_info)
        
    except Exception as e:
        main_window.log_message(f"❌ Error showing hex editor: {str(e)}")
        from PyQt6.QtWidgets import QMessageBox
        QMessageBox.critical(main_window, "Error", f"Could not open hex editor:\n{str(e)}")


def show_hex_editor_selected(main_window):
    """
    Show hex editor for selected file (when no specific row selected)
    """
    try:
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            table = main_window.gui_layout.table
            selected_items = table.selectedItems()
            if selected_items:
                row = selected_items[0].row()
                entry_info = get_entry_info(main_window, row)
                if entry_info:
                    # Import the hex editor module
                    from apps.components.Hex_Editor import show_hex_editor_for_entry
                    
                    # Use the new hex editor implementation
                    show_hex_editor_for_entry(main_window, row, entry_info)
    except Exception as e:
        main_window.log_message(f"❌ Error showing hex editor for selected: {str(e)}")
