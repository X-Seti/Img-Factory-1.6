#!/usr/bin/env python3
#this belongs in components.Col_Editor.img_workshop.py - Version: 1
# X-Seti - December31 2025 - gui base template.

"""
components/Col_Editor/col_workshop.py
COL Editor - Main collision editor interface
"""

import os
# Force X11/GLX backend for NVIDIA on Wayland
os.environ['QT_QPA_PLATFORM'] = 'xcb'
os.environ['QSG_RHI_BACKEND'] = 'opengl'
os.environ['LIBGL_ALWAYS_SOFTWARE'] = '0'  # Use hardware acceleration

import tempfile
import subprocess
import shutil
import struct
import sys
import io
import numpy as np
from pathlib import Path
from typing import Optional, List, Dict, Tuple

# Add project root to path for standalone mode
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(os.path.dirname(os.path.dirname(current_dir)))
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

# Import PyQt6
from PyQt6.QtWidgets import (QApplication, QSlider, QCheckBox,
    QWidget, QVBoxLayout, QHBoxLayout, QSplitter, QListWidget, QDialog, QFormLayout, QSpinBox,  QListWidgetItem, QLabel, QPushButton, QFrame, QFileDialog, QLineEdit, QTextEdit, QMessageBox, QScrollArea, QGroupBox, QTableWidget, QTableWidgetItem, QColorDialog, QHeaderView, QAbstractItemView, QMenu, QComboBox, QInputDialog, QTabWidget, QDoubleSpinBox, QRadioButton
)
from PyQt6.QtWidgets import (
    QDialog, QWidget, QVBoxLayout, QHBoxLayout, QGridLayout, QSplitter,
    QTableWidget, QTableWidgetItem, QTextEdit, QGroupBox, QLabel,
    QPushButton, QComboBox, QLineEdit, QHeaderView, QAbstractItemView,
    QMenuBar, QStatusBar, QProgressBar, QTabWidget, QCheckBox, QSpinBox,
    QMessageBox, QSizePolicy, QButtonGroup, QListWidget, QListWidgetItem,
    QFormLayout, QScrollArea, QFrame
)

from PyQt6.QtCore import Qt, pyqtSignal, QSize, QPoint, QRect, QByteArray, QTimer, QItemSelectionModel

from PyQt6.QtGui import QPixmap, QImage, QPainter, QPen, QBrush, QColor, QCursor, QFont, QAction, QIcon, QShortcut, QKeySequence, QPalette, QTextCursor

from PyQt6.QtSvg import QSvgRenderer
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

# Import project modules AFTER path setup
from apps.methods.imgfactory_svg_icons import SVGIconFactory

# Add root directory to path
App_name = "IMG Workshop"
DEBUG_STANDALONE = False

# Temporary 3D viewport placeholder
class COL3DViewport(QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setMinimumSize(400, 400)
        layout = QVBoxLayout(self)
        label = QLabel("3D Viewport - Placeholder")
        label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(label)
    def set_current_file(self, col_file): pass
    def set_view_options(self, **options): pass

VIEWPORT_AVAILABLE = False  # 3D viewport not yet implemented

# Import AppSettings
try:
    from apps.utils.app_settings_system import AppSettings, SettingsDialog
    APPSETTINGS_AVAILABLE = True
except ImportError:
    APPSETTINGS_AVAILABLE = False
    print("Warning: AppSettings not available")


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

    workshop_closed = pyqtSignal()
    window_closed = pyqtSignal()

    def __init__(self, parent=None, main_window=None): #vers 10
        """initialize_features"""
        if DEBUG_STANDALONE and main_window is None:
            print(App_name + " Initializing ...")

        # Set default fonts
        from PyQt6.QtGui import QFont
        default_font = QFont("Fira Sans Condensed", 14)
        self.setFont(default_font)
        self.title_font = QFont("Arial", 14)
        self.panel_font = QFont("Arial", 10)
        self.button_font = QFont("Arial", 10)
        self.infobar_font = QFont("Courier New", 9)
        self.standalone_mode = (main_window is None)

        self.setWindowTitle(App_name)
        self.setWindowIcon(SVGIconFactory.img_workshop_icon())
        self.icon_factory = SVGIconFactory()

        self.main_window = main_window
        self.table = None
        self.log = None
        self.main_splitter = None
        self.img_buttons = []
        self.entry_buttons = []
        self.options_buttons = []


        if main_window and hasattr(main_window, 'app_settings'):
            self.app_settings = main_window.app_settings
        else:
            # FIXED: Create AppSettings for standalone mode
            try:
                from apps.utils.app_settings_system import AppSettings
                self.app_settings = AppSettings()
            except Exception as e:
                print(f"Could not initialize AppSettings: {e}")
                self.app_settings = None

        if hasattr(self.app_settings, 'theme_changed'):
            self.app_settings.theme_changed.connect(self._refresh_icons)

        self._show_boxes = True
        self._show_mesh = True

        self._checkerboard_size = 16
        self._overlay_opacity = 50
        self.zoom_level = 1.0
        self.pan_offset = QPoint(0, 0)
        self.background_color = QColor(42, 42, 42)
        self.background_mode = 'solid'
        self.placeholder_text = "No Surface"
        #self.setMinimumSize(200, 200)
        preview_widget = False

        # Docking state
        self.is_docked = (main_window is not None)
        self.dock_widget = None
        self.is_overlay = False
        self.overlay_table = None
        self.overlay_tab_index = -1

        self.setWindowTitle(App_name + ": No File")
        self.resize(1400, 800)
        self.use_system_titlebar = False
        #self.window_always_on_top = False

        # Window flags
        self.setWindowFlags(Qt.WindowType.FramelessWindowHint)

        self._initialize_features()

        # Corner resize variables
        self.dragging = False
        self.drag_position = None
        self.resizing = False
        self.resize_corner = None
        self.corner_size = 20
        self.hover_corner = None

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

        self.undo_stack = []
        self.button_display_mode = 'both'
        self.last_save_directory = None


        # Setup UI FIRST
        self.setup_ui()

        # Setup hotkeys
        #self._setup_hotkeys()

        # Apply theme ONCE at the end
        self._apply_theme()


    def _log_missing_method(self, method_name): #vers 1
        """Log missing method - unified placeholder"""
        if hasattr(self.main_window, 'log_message') and hasattr(self.main_window, 'gui_layout'):
            self.main_window.log_message(f"Method '{method_name}' not yet implemented")
        else:
            print(f"Method '{method_name}' not yet implemented")


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


    def setup_ui(self): #vers 7
        """Setup the main UI layout"""
        #main_layout = QVBoxLayout(self)
        #main_layout.setContentsMargins(5, 5, 5, 5)
        #main_layout.setSpacing(5)

        # Toolbar
        toolbar = self._create_toolbar()
        #main_layout.addWidget(toolbar)

        # Tab bar for multiple col files
        #self.col_tabs = QTabWidget()
        #self.col_tabs.setTabsClosable(True)
        #self.col_tabs.tabCloseRequested.connect(self._close_col_tab)


        # Create initial tab with main content
        #initial_tab = QWidget()
        #tab_layout = QVBoxLayout(initial_tab)
        #tab_layout.setContentsMargins(0, 0, 0, 0)


        # Main splitter
        main_splitter = QSplitter(Qt.Orientation.Horizontal)

        # Create all panels first
        #left_panel = self._create_left_panel()
        middle_panel = self._create_middle_panel()
        right_panel = self._create_right_panel()


        # Status indicators if available
        if hasattr(self, '_setup_status_indicators'):
            status_frame = self._setup_status_indicators()
            main_layout.addWidget(status_frame)


    def _enable_name_edit(self, event, is_alpha): #vers 1
        """Enable name editing on click"""
        self.info_name.setReadOnly(False)
        self.info_name.selectAll()
        self.info_name.setFocus()


    def _update_status_indicators(self): #vers 2
        """Update status indicators"""
        pass


# - Button Mapping

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


# - Panel Creation

    def _create_status_bar(self): #vers 1
        """Create bottom status bar - single line compact"""
        from PyQt6.QtWidgets import QFrame, QHBoxLayout, QLabel

        status_bar = QFrame()
        status_bar.setFrameStyle(QFrame.Shape.StyledPanel | QFrame.Shadow.Sunken)
        status_bar.setFixedHeight(22)

        layout = QHBoxLayout(status_bar)
        layout.setContentsMargins(5, 0, 5, 0)
        layout.setSpacing(15)

        # Left: Ready
        self.status_label = QLabel("Ready")
        layout.addWidget(self.status_label)

        if hasattr(self, 'status_info'):
            size_kb = len(col_data) / 1024
            tex_count = len(self.collision_list)
            #self.status_col_info.setText(f"Collision: {tex_count} | col: {size_kb:.1f} KB")

        return status_bar

    def _refresh_icons(self): #vers 1
        """Refresh all icons after theme change"""
        SVGIconFactory.clear_cache()


# - Settings Reusable

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

# - RESPONSIVE DESIGN & ADAPTIVE LAYOUT

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



    def _show_workshop_settings(self): #vers 1
        """Show complete workshop settings dialog"""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QPushButton,
                                    QTabWidget, QWidget, QGroupBox, QFormLayout,
                                    QSpinBox, QComboBox, QSlider, QLabel, QCheckBox,
                                    QFontComboBox)
        from PyQt6.QtCore import Qt
        from PyQt6.QtGui import QFont

        dialog = QDialog(self)
        dialog.setWindowTitle(App_name + "Settings")
        dialog.setMinimumWidth(650)
        dialog.setMinimumHeight(550)

        layout = QVBoxLayout(dialog)

        # Create tabs
        tabs = QTabWidget()

        # TAB 1: FONTS (FIRST TAB)

        fonts_tab = QWidget()
        fonts_layout = QVBoxLayout(fonts_tab)

        # Default Font
        default_font_group = QGroupBox("Default Font")
        default_font_layout = QHBoxLayout()

        default_font_combo = QFontComboBox()
        default_font_combo.setCurrentFont(self.font())
        default_font_layout.addWidget(default_font_combo)

        default_font_size = QSpinBox()
        default_font_size.setRange(8, 24)
        default_font_size.setValue(self.font().pointSize())
        default_font_size.setSuffix(" pt")
        default_font_size.setFixedWidth(80)
        default_font_layout.addWidget(default_font_size)

        default_font_group.setLayout(default_font_layout)
        fonts_layout.addWidget(default_font_group)

        # Title Font
        title_font_group = QGroupBox("Title Font")
        title_font_layout = QHBoxLayout()

        title_font_combo = QFontComboBox()
        if hasattr(self, 'title_font'):
            title_font_combo.setCurrentFont(self.title_font)
        else:
            title_font_combo.setCurrentFont(QFont("Arial", 14))
        title_font_layout.addWidget(title_font_combo)

        title_font_size = QSpinBox()
        title_font_size.setRange(10, 32)
        title_font_size.setValue(getattr(self, 'title_font', QFont("Arial", 14)).pointSize())
        title_font_size.setSuffix(" pt")
        title_font_size.setFixedWidth(80)
        title_font_layout.addWidget(title_font_size)

        title_font_group.setLayout(title_font_layout)
        fonts_layout.addWidget(title_font_group)

        # Panel Font
        panel_font_group = QGroupBox("Panel Headers Font")
        panel_font_layout = QHBoxLayout()

        panel_font_combo = QFontComboBox()
        if hasattr(self, 'panel_font'):
            panel_font_combo.setCurrentFont(self.panel_font)
        else:
            panel_font_combo.setCurrentFont(QFont("Arial", 10))
        panel_font_layout.addWidget(panel_font_combo)

        panel_font_size = QSpinBox()
        panel_font_size.setRange(8, 18)
        panel_font_size.setValue(getattr(self, 'panel_font', QFont("Arial", 10)).pointSize())
        panel_font_size.setSuffix(" pt")
        panel_font_size.setFixedWidth(80)
        panel_font_layout.addWidget(panel_font_size)

        panel_font_group.setLayout(panel_font_layout)
        fonts_layout.addWidget(panel_font_group)

        # Button Font
        button_font_group = QGroupBox("Button Font")
        button_font_layout = QHBoxLayout()

        button_font_combo = QFontComboBox()
        if hasattr(self, 'button_font'):
            button_font_combo.setCurrentFont(self.button_font)
        else:
            button_font_combo.setCurrentFont(QFont("Arial", 10))
        button_font_layout.addWidget(button_font_combo)

        button_font_size = QSpinBox()
        button_font_size.setRange(8, 16)
        button_font_size.setValue(getattr(self, 'button_font', QFont("Arial", 10)).pointSize())
        button_font_size.setSuffix(" pt")
        button_font_size.setFixedWidth(80)
        button_font_layout.addWidget(button_font_size)

        button_font_group.setLayout(button_font_layout)
        fonts_layout.addWidget(button_font_group)

        # Info Bar Font
        infobar_font_group = QGroupBox("Info Bar Font")
        infobar_font_layout = QHBoxLayout()

        infobar_font_combo = QFontComboBox()
        if hasattr(self, 'infobar_font'):
            infobar_font_combo.setCurrentFont(self.infobar_font)
        else:
            infobar_font_combo.setCurrentFont(QFont("Courier New", 9))
        infobar_font_layout.addWidget(infobar_font_combo)

        infobar_font_size = QSpinBox()
        infobar_font_size.setRange(7, 14)
        infobar_font_size.setValue(getattr(self, 'infobar_font', QFont("Courier New", 9)).pointSize())
        infobar_font_size.setSuffix(" pt")
        infobar_font_size.setFixedWidth(80)
        infobar_font_layout.addWidget(infobar_font_size)

        infobar_font_group.setLayout(infobar_font_layout)
        fonts_layout.addWidget(infobar_font_group)

        fonts_layout.addStretch()
        tabs.addTab(fonts_tab, "Fonts")

        # TAB 2: DISPLAY SETTINGS

        display_tab = QWidget()
        display_layout = QVBoxLayout(display_tab)

        # Button display mode
        button_group = QGroupBox("Button Display Mode")
        button_layout = QVBoxLayout()

        button_mode_combo = QComboBox()
        button_mode_combo.addItems(["Icons + Text", "Icons Only", "Text Only"])
        current_mode = getattr(self, 'button_display_mode', 'both')
        mode_map = {'both': 0, 'icons': 1, 'text': 2}
        button_mode_combo.setCurrentIndex(mode_map.get(current_mode, 0))
        button_layout.addWidget(button_mode_combo)

        button_hint = QLabel("Changes how toolbar buttons are displayed")
        button_hint.setStyleSheet("color: #888; font-style: italic;")
        button_layout.addWidget(button_hint)

        button_group.setLayout(button_layout)
        display_layout.addWidget(button_group)

        # Table display
        table_group = QGroupBox("Surface List Display")
        table_layout = QVBoxLayout()

        show_thumbnails = QCheckBox("Show Surface types")
        show_thumbnails.setChecked(True)
        table_layout.addWidget(show_thumbnails)

        show_warnings = QCheckBox("Show warning icons for suspicious files")
        show_warnings.setChecked(True)
        show_warnings.setToolTip("Shows surface types")
        table_layout.addWidget(show_warnings)

        table_group.setLayout(table_layout)
        display_layout.addWidget(table_group)

        display_layout.addStretch()
        tabs.addTab(display_tab, "Display")


        # TAB 3: placeholder
        # TAB 4: PERFORMANCE

        perf_tab = QWidget()
        perf_layout = QVBoxLayout(perf_tab)

        perf_group = QGroupBox("Performance Settings")
        perf_form = QFormLayout()

        preview_quality = QComboBox()
        preview_quality.addItems(["Low (Fast)", "Medium", "High (Slow)"])
        preview_quality.setCurrentIndex(1)
        perf_form.addRow("Preview Quality:", preview_quality)

        thumb_size = QSpinBox()
        thumb_size.setRange(32, 128)
        thumb_size.setValue(64)
        thumb_size.setSuffix(" px")
        perf_form.addRow("Thumbnail Size:", thumb_size)

        perf_group.setLayout(perf_form)
        perf_layout.addWidget(perf_group)

        # Caching
        cache_group = QGroupBox("Caching")
        cache_layout = QVBoxLayout()

        enable_cache = QCheckBox("Enable surface preview caching")
        enable_cache.setChecked(True)
        cache_layout.addWidget(enable_cache)

        cache_hint = QLabel("Caching improves performance but uses more memory")
        cache_hint.setStyleSheet("color: #888; font-style: italic;")
        cache_layout.addWidget(cache_hint)

        cache_group.setLayout(cache_layout)
        perf_layout.addWidget(cache_group)

        perf_layout.addStretch()
        tabs.addTab(perf_tab, "Performance")

        # TAB 5: PREVIEW SETTINGS (LAST TAB)

        preview_tab = QWidget()
        preview_layout = QVBoxLayout(preview_tab)

        # Zoom Settings
        zoom_group = QGroupBox("Zoom Settings")
        zoom_form = QFormLayout()

        zoom_spin = QSpinBox()
        zoom_spin.setRange(10, 500)
        zoom_spin.setValue(int(getattr(self, 'zoom_level', 1.0) * 100))
        zoom_spin.setSuffix("%")
        zoom_form.addRow("Default Zoom:", zoom_spin)

        zoom_group.setLayout(zoom_form)
        preview_layout.addWidget(zoom_group)

        # Background Settings
        bg_group = QGroupBox("Background Settings")
        bg_layout = QVBoxLayout()

        # Background mode
        bg_mode_layout = QFormLayout()
        bg_mode_combo = QComboBox()
        bg_mode_combo.addItems(["Solid Color", "Checkerboard", "Grid"])
        current_bg_mode = getattr(self, 'background_mode', 'solid')
        mode_idx = {"solid": 0, "checkerboard": 1, "checker": 1, "grid": 2}.get(current_bg_mode, 0)
        bg_mode_combo.setCurrentIndex(mode_idx)
        bg_mode_layout.addRow("Background Mode:", bg_mode_combo)
        bg_layout.addLayout(bg_mode_layout)

        bg_layout.addSpacing(10)

        # Checkerboard size
        cb_label = QLabel("Checkerboard Size:")
        bg_layout.addWidget(cb_label)

        cb_layout = QHBoxLayout()
        cb_slider = QSlider(Qt.Orientation.Horizontal)
        cb_slider.setMinimum(4)
        cb_slider.setMaximum(64)
        cb_slider.setValue(getattr(self, '_checkerboard_size', 16))
        cb_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        cb_slider.setTickInterval(8)
        cb_layout.addWidget(cb_slider)

        cb_spin = QSpinBox()
        cb_spin.setMinimum(4)
        cb_spin.setMaximum(64)
        cb_spin.setValue(getattr(self, '_checkerboard_size', 16))
        cb_spin.setSuffix(" px")
        cb_spin.setFixedWidth(80)
        cb_layout.addWidget(cb_spin)

        bg_layout.addLayout(cb_layout)

        # Hint
        cb_hint = QLabel("Smaller = tighter pattern, larger = bigger squares")
        cb_hint.setStyleSheet("color: #888; font-style: italic; font-size: 10px;")
        bg_layout.addWidget(cb_hint)

        bg_group.setLayout(bg_layout)
        preview_layout.addWidget(bg_group)

        # Overlay Settings
        overlay_group = QGroupBox("Overlay View Settings")
        overlay_layout = QVBoxLayout()

        overlay_label = QLabel("Overlay Opacity (Wireframe over mesh):")
        overlay_layout.addWidget(overlay_label)

        opacity_layout = QHBoxLayout()
        opacity_slider = QSlider(Qt.Orientation.Horizontal)
        opacity_slider.setMinimum(0)
        opacity_slider.setMaximum(100)
        opacity_slider.setValue(getattr(self, '_overlay_opacity', 50))
        opacity_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        opacity_slider.setTickInterval(10)
        opacity_layout.addWidget(opacity_slider)

        opacity_spin = QSpinBox()
        opacity_spin.setMinimum(0)
        opacity_spin.setMaximum(100)
        opacity_spin.setValue(getattr(self, '_overlay_opacity', 50))
        opacity_spin.setSuffix(" %")
        opacity_spin.setFixedWidth(80)
        opacity_layout.addWidget(opacity_spin)

        overlay_layout.addLayout(opacity_layout)

        # Hint
        opacity_hint = QLabel("0")
        opacity_hint.setStyleSheet("color: #888; font-style: italic; font-size: 10px;")
        overlay_layout.addWidget(opacity_hint)

        overlay_group.setLayout(overlay_layout)
        preview_layout.addWidget(overlay_group)

        preview_layout.addStretch()
        tabs.addTab(preview_tab, "Preview")

        # Add tabs to dialog
        layout.addWidget(tabs)

        # BUTTONS

        btn_layout = QHBoxLayout()
        btn_layout.addStretch()

        # Apply button
        apply_btn = QPushButton("Apply Settings")
        apply_btn.setStyleSheet("""
            QPushButton {
                background: #0078d4;
                color: white;
                padding: 10px 24px;
                font-weight: bold;
                border-radius: 4px;
                font-size: 13px;
            }
            QPushButton:hover {
                background: #1984d8;
            }
        """)




        def apply_settings():
            # Adjusted for COL Wireframe, Mesh
            self.setFont(QFont(default_font_combo.currentFont().family(),
                            default_font_size.value()))
            self.title_font = QFont(title_font_combo.currentFont().family(),
                                title_font_size.value())
            self.panel_font = QFont(panel_font_combo.currentFont().family(),
                                panel_font_size.value())
            self.button_font = QFont(button_font_combo.currentFont().family(),
                                    button_font_size.value())
            self.infobar_font = QFont(infobar_font_combo.currentFont().family(),
                                    infobar_font_size.value())

            # Apply fonts to UI
            self._apply_title_font()
            self._apply_panel_font()
            self._apply_button_font()
            self._apply_infobar_font()

            mode_map = {0: 'both', 1: 'icons', 2: 'text'}
            self.button_display_mode = mode_map[button_mode_combo.currentIndex()]

            # EXPORT
            self.default_export_format = self.format_combo.currentText()

            # PREVIEW
            self.zoom_level = zoom_spin.value() / 100.0

            bg_modes = ['solid', 'checkerboard', 'grid']
            self.background_mode = bg_modes[bg_mode_combo.currentIndex()]

            self._checkerboard_size = cb_spin.value()
            self._overlay_opacity = opacity_spin.value()

            # Update preview widget
            if hasattr(self, 'preview_widget'):
                if self.background_mode == 'checkerboard':
                    self.preview_widget.set_checkerboard_background()
                    self.preview_widget._checkerboard_size = self._checkerboard_size
                else:
                    self.preview_widget.set_background_color(self.preview_widget.bg_color)

            # Apply button display mode
            if hasattr(self, '_update_all_buttons'):
                self._update_all_buttons()

            # Refresh display

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("Workshop settings updated successfully")

        apply_btn.clicked.connect(apply_settings)
        btn_layout.addWidget(apply_btn)

        # Close button
        close_btn = QPushButton("Close")
        close_btn.setStyleSheet("padding: 10px 24px; font-size: 13px;")
        close_btn.clicked.connect(dialog.close)
        btn_layout.addWidget(close_btn)

        layout.addLayout(btn_layout)

        # Show dialog
        dialog.exec()


    def _apply_window_flags(self): #vers 1
        """Apply window flags based on settings"""
        # Save current geometry
        current_geometry = self.geometry()
        was_visible = self.isVisible()

        if self.use_system_titlebar:
            # Use system window with title bar
            self.setWindowFlags(
                Qt.WindowType.Window |
                Qt.WindowType.WindowMinimizeButtonHint |
                Qt.WindowType.WindowMaximizeButtonHint |
                Qt.WindowType.WindowCloseButtonHint
            )
        else:
            # Use custom frameless window
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint)

        # Restore geometry and visibility
        self.setGeometry(current_geometry)

        if was_visible:
            self.show()

        if self.main_window and hasattr(self.main_window, 'log_message'):
            mode = "System title bar" if self.use_system_titlebar else "Custom frameless"
            self.main_window.log_message(f"Window mode: {mode}")


    def _apply_always_on_top(self): #vers 1
        """Apply always on top window flag"""
        current_flags = self.windowFlags()

        if self.window_always_on_top:
            new_flags = current_flags | Qt.WindowType.WindowStaysOnTopHint
        else:
            new_flags = current_flags & ~Qt.WindowType.WindowStaysOnTopHint

        if new_flags != current_flags:
            # Save state
            current_geometry = self.geometry()
            was_visible = self.isVisible()

            self.setWindowFlags(new_flags)

            self.setGeometry(current_geometry)
            if was_visible:
                self.show()


    def _scan_available_locales(self): #vers 2
        """Scan locale folder and return list of available languages"""
        import os
        import configparser

        locales = []
        locale_path = os.path.join(os.path.dirname(__file__), 'locale')

        if not os.path.exists(locale_path):
            # Easter egg: Amiga Workbench 3.1 style error
            self._show_amiga_locale_error()
            # Return default English
            return [("English", "en", None)]

        try:
            for filename in os.listdir(locale_path):
                if filename.endswith('.lang'):
                    filepath = os.path.join(locale_path, filename)

                    try:
                        config = configparser.ConfigParser()
                        config.read(filepath, encoding='utf-8')

                        if 'Metadata' in config:
                            lang_name = config['Metadata'].get('LanguageName', 'Unknown')
                            lang_code = config['Metadata'].get('LanguageCode', 'unknown')
                            locales.append((lang_name, lang_code, filepath))

                    except Exception as e:
                        if self.main_window and hasattr(self.main_window, 'log_message'):
                            self.main_window.log_message(f"Failed to load locale {filename}: {e}")

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Locale scan error: {e}")

        locales.sort(key=lambda x: x[0])

        if not locales:
            locales = [("English", "en", None)]

        return locales


    def _show_amiga_locale_error(self): #vers 2
        """Show Amiga Workbench 3.1 style error dialog"""
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QLabel, QPushButton, QHBoxLayout
        from PyQt6.QtCore import Qt
        from PyQt6.QtGui import QFont

        dialog = QDialog(self)
        dialog.setWindowTitle("Workbench Request")
        dialog.setFixedSize(450, 150)

        # Amiga Workbench styling
        dialog.setStyleSheet("""
            QDialog {
                background-color: #aaaaaa;
                border: 2px solid #ffffff;
            }
            QLabel {
                color: #000000;
                background-color: #aaaaaa;
            }
            QPushButton {
                background-color: #8899aa;
                color: #000000;
                border: 2px outset #ffffff;
                padding: 5px 15px;
                min-width: 80px;
            }
            QPushButton:pressed {
                border: 2px inset #555555;
            }
        """)

        layout = QVBoxLayout(dialog)
        layout.setSpacing(15)
        layout.setContentsMargins(20, 20, 20, 20)

        # Amiga Topaz font style
        amiga_font = QFont("Courier", 10, QFont.Weight.Normal)

        # Error message
        message = QLabel("Workbench 3.1 installer\n\nPlease insert Local disk in any drive")
        message.setFont(amiga_font)
        message.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(message)

        layout.addStretch()

        # Button layout
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        # Retry and Cancel buttons (Amiga style)
        retry_btn = QPushButton("Retry")
        retry_btn.setFont(amiga_font)
        retry_btn.clicked.connect(dialog.accept)
        button_layout.addWidget(retry_btn)

        cancel_btn = QPushButton("Cancel")
        cancel_btn.setFont(amiga_font)
        cancel_btn.clicked.connect(dialog.reject)
        button_layout.addWidget(cancel_btn)

        button_layout.addStretch()
        layout.addLayout(button_layout)

        dialog.exec()


# - Docking functions

    def _update_dock_button_visibility(self): #vers 2
        """Show/hide dock and tearoff buttons based on docked state"""
        if hasattr(self, 'dock_btn'):
            # Hide D button when docked, show when standalone
            self.dock_btn.setVisible(not self.is_docked)

        if hasattr(self, 'tearoff_btn'):
            # T button only visible when docked and not in standalone mode
            self.tearoff_btn.setVisible(self.is_docked and not self.standalone_mode)


    def toggle_dock_mode(self): #vers 2
        """Toggle between docked and standalone mode"""
        if self.is_docked:
            self._undock_from_main()
        else:
            self._dock_to_main()

        self._update_dock_button_visibility()


    def _dock_to_main(self): #vers 9
        """Dock handled by overlay system in imgfactory - IMPROVED"""
        try:
            if hasattr(self, 'is_overlay') and self.is_overlay:
                self.show()
                self.raise_()
                return

            # For proper docking, we need to be called from imgfactory
            # This method should be handled by imgfactory's overlay system
            if self.main_window and hasattr(self.main_window, App_name + '_docked'):
                # If available, use the main window's docking system
                self.main_window.open_col_workshop_docked()
            else:
                # Fallback: just show the window
                self.show()
                self.raise_()

            # Update dock state
            self.is_docked = True
            self._update_dock_button_visibility()

            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"{App_name} docked to main window")


        except Exception as e:
            print(f"Error docking: {str(e)}")
            self.show()


    def _undock_from_main(self): #vers 4
        """Undock from overlay mode to standalone window - IMPROVED"""
        try:
            if hasattr(self, 'is_overlay') and self.is_overlay:
                # Switch from overlay to normal window
                self.setWindowFlags(Qt.WindowType.Window)
                self.is_overlay = False
                self.overlay_table = None

            # Set proper window flags for standalone mode
            self.setWindowFlags(Qt.WindowType.Window)
            
            # Ensure proper size when undocking
            if hasattr(self, 'original_size'):
                self.resize(self.original_size)
            else:
                self.resize(1000, 700)  # Reasonable default size
                
            self.is_docked = False
            self._update_dock_button_visibility()

            self.show()
            self.raise_()

            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"{App_name} undocked to standalone")
                
        except Exception as e:
            print(f"Error undocking: {str(e)}")
            # Fallback
            self.setWindowFlags(Qt.WindowType.Window)
            self.show()


    def _apply_button_mode(self, dialog): #vers 1
        """Apply button display mode"""
        mode_index = self.button_mode_combo.currentIndex()
        mode_map = {0: 'both', 1: 'icons', 2: 'text'}

        new_mode = mode_map[mode_index]

        if new_mode != self.button_display_mode:
            self.button_display_mode = new_mode
            self._update_all_buttons()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                mode_names = {0: 'Icons + Text', 1: 'Icons Only', 2: 'Text Only'}
                self.main_window.log_message(f"✨ Button style: {mode_names[mode_index]}")

        dialog.close()


# - Window functionality

    def _initialize_features(self): #vers 3
        """Initialize all features after UI setup"""
        try:
            self._apply_theme()
            self._update_status_indicators()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("All features initialized")

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Feature init error: {str(e)}")


    def _is_on_draggable_area(self, pos): #vers 7
        """Check if position is on draggable titlebar area

        Args:
            pos: Position in titlebar coordinates (from eventFilter)

        Returns:
            True if position is on titlebar but not on any button
        """
        if not hasattr(self, 'titlebar'):
            print("[DRAG] No titlebar attribute")
            return False

        # Verify pos is within titlebar bounds
        if not self.titlebar.rect().contains(pos):
            print(f"[DRAG] Position {pos} outside titlebar rect {self.titlebar.rect()}")
            return False

        # Check if clicking on any button - if so, NOT draggable
        for widget in self.titlebar.findChildren(QPushButton):
            if widget.isVisible():
                # Get button geometry in titlebar coordinates
                button_rect = widget.geometry()
                if button_rect.contains(pos):
                    print(f"[DRAG] Clicked on button: {widget.toolTip()}")
                    return False

        # Not on any button = draggable
        print(f"[DRAG] On draggable area at {pos}")
        return True


# - From the fixed gui - move, drag

    def _update_all_buttons(self): #vers 4
        """Update all buttons to match display mode"""
        buttons_to_update = [
            # Toolbar buttons
            ('open_btn', 'Open'),
            ('save_btn', 'Save'),
            ('save_col_btn', 'Save TXD'),
        ]

        # Adjust transform panel width based on mode
        if hasattr(self, 'transform_icon_panel'):
            if self.button_display_mode == 'icons':
                self.transform_icon_panel.setMaximumWidth(50)
            else:
                self.transform_text_panel.setMaximumWidth(200)

        for btn_name, btn_text in buttons_to_update:
            if hasattr(self, btn_name):
                button = getattr(self, btn_name)
                self._apply_button_mode_to_button(button, btn_text)
        self._update_dock_button_visibility()

    def paintEvent(self, event): #vers 2
        """Paint corner resize triangles"""
        super().paintEvent(event)

        from PyQt6.QtGui import QPainter, QColor, QPen, QBrush, QPainterPath

        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        # Colors
        normal_color = QColor(100, 100, 100, 150)
        hover_color = QColor(150, 150, 255, 200)

        w = self.width()
        h = self.height()
        grip_size = 8  # Make corners visible (8x8px)
        size = self.corner_size

        # Define corner triangles
        corners = {
            'top-left': [(0, 0), (size, 0), (0, size)],
            'top-right': [(w, 0), (w-size, 0), (w, size)],
            'bottom-left': [(0, h), (size, h), (0, h-size)],
            'bottom-right': [(w, h), (w-size, h), (w, h-size)]
        }
        corners2 = {
            "top-left": [(0, grip_size), (0, 0), (grip_size, 0)],
            "top-right": [(w-grip_size, 0), (w, 0), (w, grip_size)],
            "bottom-left": [(0, h-grip_size), (0, h), (grip_size, h)],
            "bottom-right": [(w-grip_size, h), (w, h), (w, h-grip_size)]
        }

        # Get theme colors for corner indicators
        if self.app_settings:
            theme_colors = self.app_settings.get_theme_colors()
            accent_color = QColor(theme_colors.get('accent_primary', '#1976d2'))
            accent_color.setAlpha(180)
        else:
            accent_color = QColor(100, 150, 255, 180)

        hover_color = QColor(accent_color)
        hover_color.setAlpha(255)

        # Draw all corners with hover effect
        for corner_name, points in corners.items():
            path = QPainterPath()
            path.moveTo(points[0][0], points[0][1])
            path.lineTo(points[1][0], points[1][1])
            path.lineTo(points[2][0], points[2][1])
            path.closeSubpath()

            # Use hover color if mouse is over this corner
            color = hover_color if self.hover_corner == corner_name else accent_color

            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(QBrush(color))
            painter.drawPath(path)

        painter.end()


    def _get_resize_corner(self, pos): #vers 3
        """Determine which corner is under mouse position"""
        size = self.corner_size; w = self.width(); h = self.height()

        if pos.x() < size and pos.y() < size:
            return "top-left"
        if pos.x() > w - size and pos.y() < size:
            return "top-right"
        if pos.x() < size and pos.y() > h - size:
            return "bottom-left"
        if pos.x() > w - size and pos.y() > h - size:
            return "bottom-right"

        return None


    def mousePressEvent(self, event): #vers 8
        """Handle ALL mouse press - dragging and resizing"""
        if event.button() != Qt.MouseButton.LeftButton:
            super().mousePressEvent(event)
            return

        pos = event.pos()

        # Check corner resize FIRST
        self.resize_corner = self._get_resize_corner(pos)
        if self.resize_corner:
            self.resizing = True
            self.drag_position = event.globalPosition().toPoint()
            self.initial_geometry = self.geometry()
            event.accept()
            return

        # Check if on titlebar
        if hasattr(self, 'titlebar') and self.titlebar.geometry().contains(pos):
            titlebar_pos = self.titlebar.mapFromParent(pos)
            if self._is_on_draggable_area(titlebar_pos):
                self.windowHandle().startSystemMove()
                event.accept()
                return

        super().mousePressEvent(event)


    def mouseMoveEvent(self, event): #vers 4
        """Handle mouse move for resizing and hover effects

        Window dragging is handled by eventFilter to avoid conflicts
        """
        if event.buttons() == Qt.MouseButton.LeftButton:
            if self.resizing and self.resize_corner:
                self._handle_corner_resize(event.globalPosition().toPoint())
                event.accept()
                return
        else:
            # Update hover state and cursor
            corner = self._get_resize_corner(event.pos())
            if corner != self.hover_corner:
                self.hover_corner = corner
                self.update()  # Trigger repaint for hover effect
            self._update_cursor(corner)

        # Let parent handle everything else
        super().mouseMoveEvent(event)


    def mouseReleaseEvent(self, event): #vers 2
        """Handle mouse release"""
        if event.button() == Qt.MouseButton.LeftButton:
            self.dragging = False
            self.resizing = False
            self.resize_corner = None
            self.setCursor(Qt.CursorShape.ArrowCursor)
            event.accept()


    def _handle_corner_resize(self, global_pos): #vers 2
        """Handle window resizing from corners"""
        if not self.resize_corner or not self.drag_position:
            return

        delta = global_pos - self.drag_position
        geometry = self.initial_geometry

        min_width = 800
        min_height = 600

        # Calculate new geometry based on corner
        if self.resize_corner == "top-left":
            new_x = geometry.x() + delta.x()
            new_y = geometry.y() + delta.y()
            new_width = geometry.width() - delta.x()
            new_height = geometry.height() - delta.y()

            if new_width >= min_width and new_height >= min_height:
                self.setGeometry(new_x, new_y, new_width, new_height)

        elif self.resize_corner == "top-right":
            new_y = geometry.y() + delta.y()
            new_width = geometry.width() + delta.x()
            new_height = geometry.height() - delta.y()

            if new_width >= min_width and new_height >= min_height:
                self.setGeometry(geometry.x(), new_y, new_width, new_height)

        elif self.resize_corner == "bottom-left":
            new_x = geometry.x() + delta.x()
            new_width = geometry.width() - delta.x()
            new_height = geometry.height() + delta.y()

            if new_width >= min_width and new_height >= min_height:
                self.setGeometry(new_x, geometry.y(), new_width, new_height)

        elif self.resize_corner == "bottom-right":
            new_width = geometry.width() + delta.x()
            new_height = geometry.height() + delta.y()

            if new_width >= min_width and new_height >= min_height:
                self.resize(new_width, new_height)


    def _get_resize_direction(self, pos): #vers 1
        """Determine resize direction based on mouse position"""
        rect = self.rect()
        margin = self.resize_margin

        left = pos.x() < margin
        right = pos.x() > rect.width() - margin
        top = pos.y() < margin
        bottom = pos.y() > rect.height() - margin

        if left and top:
            return "top-left"
        elif right and top:
            return "top-right"
        elif left and bottom:
            return "bottom-left"
        elif right and bottom:
            return "bottom-right"
        elif left:
            return "left"
        elif right:
            return "right"
        elif top:
            return "top"
        elif bottom:
            return "bottom"

        return None


    def _update_cursor(self, direction): #vers 1
        """Update cursor based on resize direction"""
        if direction == "top" or direction == "bottom":
            self.setCursor(Qt.CursorShape.SizeVerCursor)
        elif direction == "left" or direction == "right":
            self.setCursor(Qt.CursorShape.SizeHorCursor)
        elif direction == "top-left" or direction == "bottom-right":
            self.setCursor(Qt.CursorShape.SizeFDiagCursor)
        elif direction == "top-right" or direction == "bottom-left":
            self.setCursor(Qt.CursorShape.SizeBDiagCursor)
        else:
            self.setCursor(Qt.CursorShape.ArrowCursor)


    def _handle_resize(self, global_pos): #vers 1
        """Handle window resizing"""
        if not self.resize_direction or not self.drag_position:
            return

        delta = global_pos - self.drag_position
        geometry = self.frameGeometry()

        min_width = 800
        min_height = 600

        # Handle horizontal resizing
        if "left" in self.resize_direction:
            new_width = geometry.width() - delta.x()
            if new_width >= min_width:
                geometry.setLeft(geometry.left() + delta.x())
        elif "right" in self.resize_direction:
            new_width = geometry.width() + delta.x()
            if new_width >= min_width:
                geometry.setRight(geometry.right() + delta.x())

        # Handle vertical resizing
        if "top" in self.resize_direction:
            new_height = geometry.height() - delta.y()
            if new_height >= min_height:
                geometry.setTop(geometry.top() + delta.y())
        elif "bottom" in self.resize_direction:
            new_height = geometry.height() + delta.y()
            if new_height >= min_height:
                geometry.setBottom(geometry.bottom() + delta.y())

        self.setGeometry(geometry)
        self.drag_position = global_pos


    def resizeEvent(self, event): #vers 1
        '''Keep resize grip in bottom-right corner'''
        super().resizeEvent(event)
        if hasattr(self, 'size_grip'):
            self.size_grip.move(self.width() - 16, self.height() - 16)


    def mouseDoubleClickEvent(self, event): #vers 2
        """Handle double-click - maximize/restore

        Handled here instead of eventFilter for better control
        """
        if event.button() == Qt.MouseButton.LeftButton:
            # Convert to titlebar coordinates if needed
            if hasattr(self, 'titlebar'):
                titlebar_pos = self.titlebar.mapFromParent(event.pos())
                if self._is_on_draggable_area(titlebar_pos):
                    self._toggle_maximize()
                    event.accept()
                    return

        super().mouseDoubleClickEvent(event)

# - Marker 3

    def _toggle_maximize(self): #vers 1
        """Toggle window maximize state"""
        if self.isMaximized():
            self.showNormal()
        else:
            self.showMaximized()


    def closeEvent(self, event): #vers 1
        """Handle close event"""
        self.window_closed.emit()
        event.accept()


# - Panel Setup

    def _create_toolbar(self): #vers 13
        """Create toolbar - FIXED: Hide drag button when docked, ensure buttons visible"""
        self.titlebar = QFrame()
        self.titlebar.setFrameStyle(QFrame.Shape.StyledPanel)
        self.titlebar.setFixedHeight(45)
        self.titlebar.setObjectName("titlebar")

        # Install event filter for drag detection
        #self.titlebar.installEventFilter(self)
        #self.titlebar.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents, False)
        #self.titlebar.setMouseTracking(True)

        self.layout = QHBoxLayout(self.titlebar)
        self.layout.setContentsMargins(5, 5, 5, 5)
        self.layout.setSpacing(5)

        # Get icon color from theme
        icon_color = self._get_icon_color()

        self.toolbar = QFrame()
        self.toolbar.setFrameStyle(QFrame.Shape.StyledPanel)
        self.toolbar.setMaximumHeight(50)

        layout = QHBoxLayout(self.toolbar)
        layout.setContentsMargins(5, 5, 5, 5)
        layout.setSpacing(5)

        # Settings button
        self.settings_btn = QPushButton()
        self.settings_btn.setFont(self.button_font)
        #self.settings_btn.setIcon(self.icon_factory.settings_icon())
        self.settings_btn.setText("Settings")
        self.settings_btn.setIconSize(QSize(20, 20))
        self.settings_btn.clicked.connect(self._show_workshop_settings)
        self.settings_btn.setToolTip("Workshop Settings")
        layout.addWidget(self.settings_btn)

        layout.addStretch()

        # App title in center
        self.title_label = QLabel(App_name)
        self.title_label.setFont(self.title_font)
        self.title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(self.title_label)

        layout.addStretch()
        #layout.addStretch()

        # Open button
        self.open_btn = QPushButton()
        self.open_btn.setFont(self.button_font)
        #self.open_btn.setIcon(self.icon_factory.open_icon())
        self.open_btn.setText("Open")
        self.open_btn.setIconSize(QSize(20, 20))
        self.open_btn.setShortcut("Ctrl+O")
        if self.button_display_mode == 'icons':
            self.open_btn.setFixedSize(40, 40)
        self.open_btn.setToolTip("Open COL file (Ctrl+O)")
        #self.open_btn.clicked.connect(self._open_file)
        layout.addWidget(self.open_btn)

        # Save button
        self.save_btn = QPushButton()
        self.save_btn.setFont(self.button_font)
        #self.save_btn.setIcon(self.icon_factory.save_icon())
        self.save_btn.setText("Save")
        self.save_btn.setIconSize(QSize(20, 20))
        self.save_btn.setShortcut("Ctrl+S")
        if self.button_display_mode == 'icons':
            self.save_btn.setFixedSize(40, 40)
        self.save_btn.setEnabled(False)  # Enable when modified
        self.save_btn.setToolTip("Save COL file (Ctrl+S)")
        #self.save_btn.clicked.connect(self._save_file)
        layout.addWidget(self.save_btn)

        self.export_all_btn = QPushButton("Extract")
        self.export_all_btn.setFont(self.button_font)
        #self.export_all_btn.setIcon(self.icon_factory.package_icon())
        self.export_all_btn.setIconSize(QSize(20, 20))
        self.export_all_btn.setToolTip("Export all as col, cst or 3ds files")
        #self.export_all_btn.clicked.connect(self.export_all)
        self.export_all_btn.setEnabled(False)
        layout.addWidget(self.export_all_btn)

        self.undo_btn = QPushButton()
        self.undo_btn.setFont(self.button_font)
        #self.undo_btn.setIcon(self.icon_factory.undo_icon())
        self.undo_btn.setText("Undo")
        self.undo_btn.setIconSize(QSize(20, 20))
        #self.undo_btn.clicked.connect(self._undo_last_action)
        self.undo_btn.setEnabled(False)
        self.undo_btn.setToolTip("Undo last change")
        layout.addWidget(self.undo_btn)

        # Info button
        self.info_btn = QPushButton("")
        self.info_btn.setText("")  # CHANGED from "Info"
        self.info_btn.setIcon(self.icon_factory.info_icon())
        self.info_btn.setMinimumWidth(40)
        self.info_btn.setMaximumWidth(40)
        self.info_btn.setMinimumHeight(30)
        self.info_btn.setToolTip("Information")

        self.info_btn.setIconSize(QSize(20, 20))
        self.info_btn.setFixedWidth(35)
        #self.info_btn.clicked.connect(self._show_col_info)
        layout.addWidget(self.info_btn)

        # Properties/Theme button
        self.properties_btn = QPushButton()
        self.properties_btn.setFont(self.button_font)
        self.properties_btn.setIcon(SVGIconFactory.properties_icon(24, icon_color))
        self.properties_btn.setToolTip("Theme")
        self.properties_btn.setFixedSize(35, 35)
        self.properties_btn.clicked.connect(self._launch_theme_settings)
        self.properties_btn.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        #self.properties_btn.customContextMenuRequested.connect(self._show_settings_context_menu)
        layout.addWidget(self.properties_btn)

        # Dock button [D]
        self.dock_btn = QPushButton("D")
        #self.dock_btn.setFont(self.button_font)
        self.dock_btn.setMinimumWidth(40)
        self.dock_btn.setMaximumWidth(40)
        self.dock_btn.setMinimumHeight(30)
        self.dock_btn.setToolTip("Dock")

        #self.dock_btn.clicked.connect(self.toggle_dock_mode)
        layout.addWidget(self.dock_btn)

        # Tear-off button [T] - only in IMG Factory mode
        if not self.standalone_mode:
            self.tearoff_btn = QPushButton("T")
            #self.tearoff_btn.setFont(self.button_font)
            self.tearoff_btn.setMinimumWidth(40)
            self.tearoff_btn.setMaximumWidth(40)
            self.tearoff_btn.setMinimumHeight(30)
            #self.tearoff_btn.clicked.connect(self._toggle_tearoff)
            self.tearoff_btn.setToolTip("TXD Workshop - Tearoff window")

            layout.addWidget(self.tearoff_btn)

        # Window controls
        self.minimize_btn = QPushButton()
        self.minimize_btn.setIcon(self.icon_factory.minimize_icon())
        self.minimize_btn.setIconSize(QSize(20, 20))
        self.minimize_btn.setMinimumWidth(40)
        self.minimize_btn.setMaximumWidth(40)
        self.minimize_btn.setMinimumHeight(30)
        #self.minimize_btn.clicked.connect(self.showMinimized)
        self.minimize_btn.setToolTip("Minimize Window") # click tab to restore
        layout.addWidget(self.minimize_btn)

        self.maximize_btn = QPushButton()
        self.maximize_btn.setIcon(self.icon_factory.maximize_icon())
        self.maximize_btn.setIconSize(QSize(20, 20))
        self.maximize_btn.setMinimumWidth(40)
        self.maximize_btn.setMaximumWidth(40)
        self.maximize_btn.setMinimumHeight(30)
        #self.maximize_btn.clicked.connect(self._toggle_maximize)
        self.maximize_btn.setToolTip("Maximize/Restore Window")
        layout.addWidget(self.maximize_btn)

        self.close_btn = QPushButton()
        self.close_btn.setIcon(self.icon_factory.close_icon())
        self.close_btn.setIconSize(QSize(20, 20))
        self.close_btn.setMinimumWidth(40)
        self.close_btn.setMaximumWidth(40)
        self.close_btn.setMinimumHeight(30)
        #self.close_btn.clicked.connect(self.close)
        self.close_btn.setToolTip("Close Window") # closes tab
        layout.addWidget(self.close_btn)

        return self.toolbar

    #Left side vertical panel
    def _create_transform_icon_panel(self): #vers 12
        """Create transform panel with icons - aligned with text panel"""
        self.transform_icon_panel = QFrame()
        self.transform_icon_panel.setFrameStyle(QFrame.Shape.StyledPanel)
        self.transform_icon_panel.setMinimumWidth(45)
        self.transform_icon_panel.setMaximumWidth(45)

        layout = QVBoxLayout(self.transform_icon_panel)
        layout.setContentsMargins(3, 5, 3, 5)
        layout.setSpacing(1)

        btn_height = 32
        btn_width = 40
        icon_size = QSize(20, 20)
        spacer = 3

        layout.addSpacing(2)

        # Flip Vertical
        self.flip_vert_btn = QPushButton()
        self.flip_vert_btn.setIcon(self.icon_factory.flip_vert_icon())
        self.flip_vert_btn.setIconSize(icon_size)
        self.flip_vert_btn.setFixedHeight(btn_height)
        self.flip_vert_btn.setMinimumWidth(btn_width)
        self.flip_vert_btn.setEnabled(False)
        self.flip_vert_btn.setToolTip("Flip col vertically")
        layout.addWidget(self.flip_vert_btn)
        layout.addSpacing(spacer)

        # Flip Horizontal
        self.flip_horz_btn = QPushButton()
        self.flip_horz_btn.setIcon(self.icon_factory.flip_horz_icon())
        self.flip_horz_btn.setIconSize(icon_size)
        self.flip_horz_btn.setFixedHeight(btn_height)
        self.flip_horz_btn.setMinimumWidth(btn_width)
        self.flip_horz_btn.setEnabled(False)
        self.flip_horz_btn.setToolTip("Flip col horizontally")
        layout.addWidget(self.flip_horz_btn)
        layout.addSpacing(spacer)

        # Rotate Clockwise
        self.rotate_cw_btn = QPushButton()
        self.rotate_cw_btn.setIcon(self.icon_factory.rotate_cw_icon())
        self.rotate_cw_btn.setIconSize(icon_size)
        self.rotate_cw_btn.setFixedHeight(btn_height)
        self.rotate_cw_btn.setMinimumWidth(btn_width)
        self.rotate_cw_btn.setEnabled(False)
        self.rotate_cw_btn.setToolTip("Rotate 90 degrees clockwise")
        layout.addWidget(self.rotate_cw_btn)
        layout.addSpacing(spacer)

        # Rotate Counter-Clockwise
        self.rotate_ccw_btn = QPushButton()
        self.rotate_ccw_btn.setIcon(self.icon_factory.rotate_ccw_icon())
        self.rotate_ccw_btn.setIconSize(icon_size)
        self.rotate_ccw_btn.setFixedHeight(btn_height)
        self.rotate_ccw_btn.setMinimumWidth(btn_width)
        self.rotate_ccw_btn.setEnabled(False)
        self.rotate_ccw_btn.setToolTip("Rotate 90 degrees counter-clockwise")
        layout.addWidget(self.rotate_ccw_btn)
        layout.addSpacing(spacer)

        # Analyze
        self.analyze_btn = QPushButton()
        self.analyze_btn.setIcon(self.icon_factory.analyze_icon())
        self.analyze_btn.setIconSize(icon_size)
        self.analyze_btn.setFixedHeight(btn_height)
        self.analyze_btn.setMinimumWidth(btn_width)
        #self.analyze_btn.clicked.connect(self._analyze_collision)
        self.analyze_btn.setEnabled(False)
        self.analyze_btn.setToolTip("Analyze collision data")
        layout.addWidget(self.analyze_btn)
        layout.addSpacing(spacer)

        # Copy
        self.copy_btn = QPushButton()
        self.copy_btn.setIcon(self.icon_factory.copy_icon())
        self.copy_btn.setIconSize(icon_size)
        self.copy_btn.setFixedHeight(btn_height)
        self.copy_btn.setMinimumWidth(btn_width)
        self.copy_btn.setEnabled(False)
        self.copy_btn.setToolTip("Copy col to clipboard")
        layout.addWidget(self.copy_btn)
        layout.addSpacing(spacer)

        # Paste
        self.paste_btn = QPushButton()
        self.paste_btn.setIcon(self.icon_factory.paste_icon())
        self.paste_btn.setIconSize(icon_size)
        self.paste_btn.setFixedHeight(btn_height)
        self.paste_btn.setMinimumWidth(btn_width)
        self.paste_btn.setEnabled(False)
        self.paste_btn.setToolTip("Paste col from clipboard")
        layout.addWidget(self.paste_btn)
        layout.addSpacing(spacer)

        # Create
        self.create_surface_btn = QPushButton()
        self.create_surface_btn.setIcon(self.icon_factory.add_icon())
        self.create_surface_btn.setIconSize(icon_size)
        self.create_surface_btn.setFixedHeight(btn_height)
        self.create_surface_btn.setMinimumWidth(btn_width)
        self.create_surface_btn.setToolTip("Create new blank Collision")
        layout.addWidget(self.create_surface_btn)
        layout.addSpacing(spacer)

        # Delete
        self.delete_surface_btn = QPushButton()
        self.delete_surface_btn.setIcon(self.icon_factory.delete_icon())
        self.delete_surface_btn.setIconSize(icon_size)
        self.delete_surface_btn.setFixedHeight(btn_height)
        self.delete_surface_btn.setMinimumWidth(btn_width)
        self.delete_surface_btn.setEnabled(False)
        self.delete_surface_btn.setToolTip("Remove selected Collision")
        layout.addWidget(self.delete_surface_btn)
        layout.addSpacing(spacer)

        # Duplicate
        self.duplicate_surface_btn = QPushButton()
        self.duplicate_surface_btn.setIcon(self.icon_factory.duplicate_icon())
        self.duplicate_surface_btn.setIconSize(icon_size)
        self.duplicate_surface_btn.setFixedHeight(btn_height)
        self.duplicate_surface_btn.setMinimumWidth(btn_width)
        self.duplicate_surface_btn.setEnabled(False)
        self.duplicate_surface_btn.setToolTip("Clone selected Collision")
        layout.addWidget(self.duplicate_surface_btn)
        layout.addSpacing(spacer)

        # Paint
        self.paint_btn = QPushButton()
        self.paint_btn.setIcon(self.icon_factory.paint_icon())
        self.paint_btn.setIconSize(icon_size)
        self.paint_btn.setFixedHeight(btn_height)
        self.paint_btn.setMinimumWidth(btn_width)
        self.paint_btn.setEnabled(False)
        self.paint_btn.setToolTip("Paint free hand on surface")
        layout.addWidget(self.paint_btn)
        layout.addSpacing(spacer)

        # Surface Type
        self.surface_type_btn = QPushButton()
        self.surface_type_btn.setIcon(self.icon_factory.checkerboard_icon())
        self.surface_type_btn.setIconSize(icon_size)
        self.surface_type_btn.setFixedHeight(btn_height)
        self.surface_type_btn.setMinimumWidth(btn_width)
        self.surface_type_btn.setToolTip("Surface types")
        layout.addWidget(self.surface_type_btn)
        layout.addSpacing(spacer)

        # Surface Edit
        self.surface_edit_btn = QPushButton()
        self.surface_edit_btn.setIcon(self.icon_factory.surfaceedit_icon())
        self.surface_edit_btn.setIconSize(icon_size)
        self.surface_edit_btn.setFixedHeight(btn_height)
        self.surface_edit_btn.setMinimumWidth(btn_width)
        self.surface_edit_btn.setToolTip("Surface Editor")
        layout.addWidget(self.surface_edit_btn)
        layout.addSpacing(spacer)

        # Build from TXD
        self.build_from_txd_btn = QPushButton()
        self.build_from_txd_btn.setIcon(self.icon_factory.build_icon())
        self.build_from_txd_btn.setIconSize(icon_size)
        self.build_from_txd_btn.setFixedHeight(btn_height)
        self.build_from_txd_btn.setMinimumWidth(btn_width)
        self.build_from_txd_btn.setToolTip("Create col surface from txd texture names")
        layout.addWidget(self.build_from_txd_btn)

        layout.addStretch()
        return self.transform_icon_panel


    def _create_transform_text_panel(self): #vers 12
        """Create transform panel with text - aligned with icon panel"""
        self.transform_text_panel = QFrame()
        self.transform_text_panel.setFrameStyle(QFrame.Shape.StyledPanel)
        self.transform_text_panel.setMinimumWidth(140)
        self.transform_text_panel.setMaximumWidth(140)

        layout = QVBoxLayout(self.transform_text_panel)
        layout.setContentsMargins(5, 5, 5, 5)
        layout.setSpacing(1)

        btn_height = 32
        spacer = 3

        layout.addSpacing(2)

        # Flip Vertical
        self.flip_vert_btn = QPushButton("Flip Vertical")
        self.flip_vert_btn.setFont(self.button_font)
        self.flip_vert_btn.setFixedHeight(btn_height)
        self.flip_vert_btn.setEnabled(False)
        self.flip_vert_btn.setToolTip("Flip col vertically")
        layout.addWidget(self.flip_vert_btn)
        layout.addSpacing(spacer)

        # Flip Horizontal
        self.flip_horz_btn = QPushButton("Flip Horizontal")
        self.flip_horz_btn.setFont(self.button_font)
        self.flip_horz_btn.setFixedHeight(btn_height)
        self.flip_horz_btn.setEnabled(False)
        self.flip_horz_btn.setToolTip("Flip col horizontally")
        layout.addWidget(self.flip_horz_btn)
        layout.addSpacing(spacer)

        # Rotate Clockwise
        self.rotate_cw_btn = QPushButton("Rotate 90° CW")
        self.rotate_cw_btn.setFont(self.button_font)
        self.rotate_cw_btn.setFixedHeight(btn_height)
        self.rotate_cw_btn.setEnabled(False)
        self.rotate_cw_btn.setToolTip("Rotate 90 degrees clockwise")
        layout.addWidget(self.rotate_cw_btn)
        layout.addSpacing(spacer)

        # Rotate Counter-Clockwise
        self.rotate_ccw_btn = QPushButton("Rotate 90° CCW")
        self.rotate_ccw_btn.setFont(self.button_font)
        self.rotate_ccw_btn.setFixedHeight(btn_height)
        self.rotate_ccw_btn.setEnabled(False)
        self.rotate_ccw_btn.setToolTip("Rotate 90 degrees counter-clockwise")
        layout.addWidget(self.rotate_ccw_btn)
        layout.addSpacing(spacer)

        # Analyze
        self.analyze_btn = QPushButton("Analyze")
        self.analyze_btn.setFont(self.button_font)
        self.analyze_btn.setFixedHeight(btn_height)
        #self.analyze_btn.clicked.connect(self._analyze_collision)
        self.analyze_btn.setEnabled(False)
        self.analyze_btn.setToolTip("Analyze collision data")
        layout.addWidget(self.analyze_btn)
        layout.addSpacing(spacer)

        # Copy
        self.copy_btn = QPushButton("Copy")
        self.copy_btn.setFont(self.button_font)
        self.copy_btn.setFixedHeight(btn_height)
        self.copy_btn.setEnabled(False)
        self.copy_btn.setToolTip("Copy col to clipboard")
        layout.addWidget(self.copy_btn)
        layout.addSpacing(spacer)

        # Paste
        self.paste_btn = QPushButton("Paste")
        self.paste_btn.setFont(self.button_font)
        self.paste_btn.setFixedHeight(btn_height)
        self.paste_btn.setEnabled(False)
        self.paste_btn.setToolTip("Paste col from clipboard")
        layout.addWidget(self.paste_btn)
        layout.addSpacing(spacer)

        # Create
        self.create_surface_btn = QPushButton("Create")
        self.create_surface_btn.setFont(self.button_font)
        self.create_surface_btn.setFixedHeight(btn_height)
        self.create_surface_btn.setToolTip("Create new blank Collision")
        layout.addWidget(self.create_surface_btn)
        layout.addSpacing(spacer)

        # Delete
        self.delete_surface_btn = QPushButton("Delete")
        self.delete_surface_btn.setFont(self.button_font)
        self.delete_surface_btn.setFixedHeight(btn_height)
        self.delete_surface_btn.setEnabled(False)
        self.delete_surface_btn.setToolTip("Remove selected Collision")
        layout.addWidget(self.delete_surface_btn)
        layout.addSpacing(spacer)

        # Duplicate
        self.duplicate_surface_btn = QPushButton("Duplicate")
        self.duplicate_surface_btn.setFont(self.button_font)
        self.duplicate_surface_btn.setFixedHeight(btn_height)
        self.duplicate_surface_btn.setEnabled(False)
        self.duplicate_surface_btn.setToolTip("Clone selected Collision")
        layout.addWidget(self.duplicate_surface_btn)
        layout.addSpacing(spacer)

        # Paint
        self.paint_btn = QPushButton("Paint")
        self.paint_btn.setFont(self.button_font)
        self.paint_btn.setFixedHeight(btn_height)
        self.paint_btn.setEnabled(False)
        self.paint_btn.setToolTip("Paint free hand on surface")
        layout.addWidget(self.paint_btn)
        layout.addSpacing(spacer)

        # Surface Type
        self.surface_type_btn = QPushButton("Surface type")
        self.surface_type_btn.setFont(self.button_font)
        self.surface_type_btn.setFixedHeight(btn_height)
        self.surface_type_btn.setToolTip("Surface types")
        layout.addWidget(self.surface_type_btn)
        layout.addSpacing(spacer)

        # Surface Edit
        self.surface_edit_btn = QPushButton("Surface Edit")
        self.surface_edit_btn.setFont(self.button_font)
        self.surface_edit_btn.setFixedHeight(btn_height)
        self.surface_edit_btn.setToolTip("Surface Editor")
        layout.addWidget(self.surface_edit_btn)
        layout.addSpacing(spacer)

        # Build from TXD
        self.build_from_txd_btn = QPushButton("Build col via")
        self.build_from_txd_btn.setFont(self.button_font)
        self.build_from_txd_btn.setFixedHeight(btn_height)
        self.build_from_txd_btn.setToolTip("Create col surface from txd texture names")
        layout.addWidget(self.build_from_txd_btn)

        layout.addStretch()
        return self.transform_text_panel


    def _create_left_panel(self): #vers 5
        """Create left panel - COL file list (only in IMG Factory mode)"""
        # In standalone mode, don't create this panel
        if self.standalone_mode:
            self.col_list_widget = None  # Explicitly set to None
            return None

        if not self.main_window:
            # Standalone mode - return None to hide this panel
            return None

        # Only create panel in IMG Factory mode
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(200)
        panel.setMaximumWidth(300)

        layout = QVBoxLayout(panel)
        layout.setContentsMargins(5, 5, 5, 5)

        header = QLabel("COL Files")
        header.setFont(QFont("Arial", 10, QFont.Weight.Bold))
        layout.addWidget(header)

        self.col_list_widget = QListWidget()
        self.col_list_widget.setAlternatingRowColors(True)
        #self.col_list_widget.itemClicked.connect(self._on_col_selected)
        layout.addWidget(self.col_list_widget)
        return panel


    def _create_middle_panel(self): #vers 4
        """Create middle panel with COL models table - theme-aware"""
        panel = QGroupBox("File Window")
        self._create_file_window()
        return panel

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
        placeholder_label = QLabel("Directory Tree")
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

        #self._setup_tearoff_button_for_tabs()

        file_layout.addWidget(self.tab_widget)
        return file_window

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


    def _create_right_panel(self): #vers 11
        """Create right panel with editing controls - compact layout"""
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(200)
        self.create_right_panel_with_pastel_buttons()
        return panel

    def create_right_panel_with_pastel_buttons(self): #vers 4
        """Create right panel with theme-controlled pastel buttons"""
        right_panel = QWidget()
        right_layout = QVBoxLayout(right_panel)
        right_layout.setContentsMargins(4, 4, 4, 4)

        # Get spacing from settings
        if hasattr(self.main_window, 'app_settings') and hasattr(self.main_window.app_settings, 'current_settings'):
            space_between_btnv = self.main_window.app_settings.current_settings.get('button_spacing_vertical', 8)
            space_between_btnh = self.main_window.app_settings.current_settings.get('button_spacing_horizontal', 6)
            button_height = self.main_window.app_settings.current_settings.get('button_height', 32)
        else:
            # Defaults if settings not available
            space_between_btnv = 8
            space_between_btnh = 6
            button_height = 32

        right_layout.setSpacing(space_between_btnv)

        # IMG Section with theme colors
        img_box = QGroupBox("IMG, COL, TXD Files")
        img_layout = QGridLayout()
        img_layout.setSpacing(space_between_btnv)
        img_layout.setHorizontalSpacing(space_between_btnh)
        img_layout.setVerticalSpacing(space_between_btnv)

        # Use theme-controlled button data
        img_buttons_data = self._get_img_buttons_data()

        for i, (label, action_type, icon, color, method_name) in enumerate(img_buttons_data):
            btn = self.create_pastel_button(label, action_type, icon, color, method_name)
            btn.setMaximumHeight(button_height)
            btn.setMinimumHeight(button_height - 4)
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
        entries_layout.setSpacing(space_between_btnv)
        entries_layout.setHorizontalSpacing(space_between_btnh)
        entries_layout.setVerticalSpacing(space_between_btnv)

        # Use theme-controlled button data
        entry_buttons_data = self._get_entry_buttons_data()

        for i, (label, action_type, icon, color, method_name) in enumerate(entry_buttons_data):
            btn = self.create_pastel_button(label, action_type, icon, color, method_name)
            btn.setMaximumHeight(button_height)
            btn.setMinimumHeight(button_height - 4)
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
        options_layout.setSpacing(space_between_btnv)
        options_layout.setHorizontalSpacing(space_between_btnh)
        options_layout.setVerticalSpacing(space_between_btnv)

        # Use theme-controlled button data
        options_buttons_data = self._get_options_buttons_data()

        for i, (label, action_type, icon, color, method_name) in enumerate(options_buttons_data):
            btn = self.create_pastel_button(label, action_type, icon, color, method_name)
            btn.setMaximumHeight(button_height)
            btn.setMinimumHeight(button_height - 4)
            self.options_buttons.append(btn)
            # Add to backend as well
            if hasattr(self, 'backend'):
                self.backend.options_buttons.append(btn)
            options_layout.addWidget(btn, i // 3, i % 3)

        options_box.setLayout(options_layout)
        right_layout.addWidget(options_box)

        # Add stretch to push everything up
        right_layout.addStretch()
        return right_panel

    def _create_preview_controls(self): #vers 5
        """Create preview control buttons - vertical layout on right"""
        controls_frame = QFrame()
        controls_frame.setFrameStyle(QFrame.Shape.StyledPanel)
        controls_frame.setMaximumWidth(50)
        controls_layout = QVBoxLayout(controls_frame)
        controls_layout.setContentsMargins(5, 5, 5, 5)
        controls_layout.setSpacing(5)

        # Check if using 3D viewport or 2D preview
        is_3d_viewport = VIEWPORT_AVAILABLE and isinstance(self.preview_widget, COL3DViewport)

        # Zoom In
        zoom_in_btn = QPushButton()
        zoom_in_btn.setIcon(self.icon_factory.zoom_in_icon())
        zoom_in_btn.setIconSize(QSize(20, 20))
        zoom_in_btn.setFixedSize(40, 40)
        zoom_in_btn.setToolTip("Zoom In")
        #zoom_in_btn.clicked.connect(self.preview_widget.zoom_in)
        controls_layout.addWidget(zoom_in_btn)

        # Zoom Out
        zoom_out_btn = QPushButton()
        zoom_out_btn.setIcon(self.icon_factory.zoom_out_icon())
        zoom_out_btn.setIconSize(QSize(20, 20))
        zoom_out_btn.setFixedSize(40, 40)
        zoom_out_btn.setToolTip("Zoom Out")
        #zoom_out_btn.clicked.connect(self.preview_widget.zoom_out)
        controls_layout.addWidget(zoom_out_btn)

        # Reset
        reset_btn = QPushButton()
        reset_btn.setIcon(self.icon_factory.reset_icon())
        reset_btn.setIconSize(QSize(20, 20))
        reset_btn.setFixedSize(40, 40)
        reset_btn.setToolTip("Reset View")
        #reset_btn.clicked.connect(self.preview_widget.reset_view)
        controls_layout.addWidget(reset_btn)

        # Fit
        fit_btn = QPushButton()
        fit_btn.setIcon(self.icon_factory.fit_icon())
        fit_btn.setIconSize(QSize(20, 20))
        fit_btn.setFixedSize(40, 40)
        fit_btn.setToolTip("Fit to Window")
        #fit_btn.clicked.connect(self.preview_widget.fit_to_window)
        controls_layout.addWidget(fit_btn)

        controls_layout.addSpacing(10)

        # Pan Up
        pan_up_btn = QPushButton()
        pan_up_btn.setIcon(self.icon_factory.arrow_up_icon())
        pan_up_btn.setIconSize(QSize(20, 20))
        pan_up_btn.setFixedSize(40, 40)
        pan_up_btn.setToolTip("Pan Up")
        #pan_up_btn.clicked.connect(lambda: self._pan_preview(0, -20))
        controls_layout.addWidget(pan_up_btn)

        # Pan Down
        pan_down_btn = QPushButton()
        pan_down_btn.setIcon(self.icon_factory.arrow_down_icon())
        pan_down_btn.setIconSize(QSize(20, 20))
        pan_down_btn.setFixedSize(40, 40)
        pan_down_btn.setToolTip("Pan Down")
        #pan_down_btn.clicked.connect(lambda: self._pan_preview(0, 20))
        controls_layout.addWidget(pan_down_btn)

        # Pan Left
        pan_left_btn = QPushButton()
        pan_left_btn.setIcon(self.icon_factory.arrow_left_icon())
        pan_left_btn.setIconSize(QSize(20, 20))
        pan_left_btn.setFixedSize(40, 40)
        pan_left_btn.setToolTip("Pan Left")
        #pan_left_btn.clicked.connect(lambda: self._pan_preview(-20, 0))
        controls_layout.addWidget(pan_left_btn)

        # Pan Right
        pan_right_btn = QPushButton()
        pan_right_btn.setIcon(self.icon_factory.arrow_right_icon())
        pan_right_btn.setIconSize(QSize(20, 20))
        pan_right_btn.setFixedSize(40, 40)
        pan_right_btn.setToolTip("Pan Right")
        #pan_right_btn.clicked.connect(lambda: self._pan_preview(20, 0))
        controls_layout.addWidget(pan_right_btn)

        # Color picker
        bg_custom_btn = QPushButton()
        bg_custom_btn.setIcon(self.icon_factory.color_picker_icon())
        bg_custom_btn.setIconSize(QSize(20, 20))
        bg_custom_btn.setFixedSize(40, 40)
        bg_custom_btn.setToolTip("Pick Background Color")
        #bg_custom_btn.clicked.connect(self._pick_background_color)
        controls_layout.addWidget(bg_custom_btn)

        controls_layout.addSpacing(5)

        # View Spheres toggle
        self.view_spheres_btn = QPushButton()
        self.view_spheres_btn.setIcon(self.icon_factory.sphere_icon())
        self.view_spheres_btn.setIconSize(QSize(20, 20))
        self.view_spheres_btn.setFixedSize(40, 40)
        self.view_spheres_btn.setCheckable(True)
        self.view_spheres_btn.setChecked(True)
        self.view_spheres_btn.setToolTip("Toggle Spheres")
        #self.view_spheres_btn.clicked.connect(self._toggle_spheres)
        controls_layout.addWidget(self.view_spheres_btn)

        # View Boxes toggle
        self.view_boxes_btn = QPushButton()
        self.view_boxes_btn.setIcon(self.icon_factory.box_icon())
        self.view_boxes_btn.setIconSize(QSize(20, 20))
        self.view_boxes_btn.setFixedSize(40, 40)
        self.view_boxes_btn.setCheckable(True)
        self.view_boxes_btn.setChecked(True)
        self.view_boxes_btn.setToolTip("Toggle Boxes")
        #self.view_boxes_btn.clicked.connect(self._toggle_boxes)
        controls_layout.addWidget(self.view_boxes_btn)

        # View Mesh toggle
        self.view_mesh_btn = QPushButton()
        self.view_mesh_btn.setIcon(self.icon_factory.mesh_icon())
        self.view_mesh_btn.setIconSize(QSize(20, 20))
        self.view_mesh_btn.setFixedSize(40, 40)
        self.view_mesh_btn.setCheckable(True)
        self.view_mesh_btn.setChecked(True)
        self.view_mesh_btn.setToolTip("Toggle Mesh")
        #self.view_mesh_btn.clicked.connect(self._toggle_mesh)
        controls_layout.addWidget(self.view_mesh_btn)
        controls_layout.addSpacing(5)

        # Icon label
        self.backface_btn = QPushButton()
        self.backface_btn.setIcon(self.icon_factory.backface_icon())
        self.backface_btn.setIconSize(QSize(20, 20))
        self.backface_btn.setFixedSize(40, 40)
        self.backface_btn.setCheckable(True)
        self.backface_btn.setChecked(False)
        self.backface_btn.setToolTip("Toggle Backface")
        #self.backface_btn.clicked.connect(self._toggle_backface_culling)
        controls_layout.addWidget(self.backface_btn)

        return controls_frame


    def _update_toolbar_for_docking_state(self): #vers 1
        """Update toolbar visibility based on docking state"""
        # Hide/show drag button based on docking state
        if hasattr(self, 'drag_btn'):
            self.drag_btn.setVisible(not self.is_docked)


# - Rest of the logic for the panels

    def _apply_title_font(self): #vers 1
        """Apply title font to title bar labels"""
        if hasattr(self, 'title_font'):
            # Find all title labels
            for label in self.findChildren(QLabel):
                if label.objectName() == "title_label" or "🗺️" in label.text():
                    label.setFont(self.title_font)


    def _apply_panel_font(self): #vers 1
        """Apply panel font to info panels and labels"""
        if hasattr(self, 'panel_font'):
            # Apply to info labels (Mipmaps, Bumpmaps, status labels)
            for label in self.findChildren(QLabel):
                if any(x in label.text() for x in ["Mipmaps:", "Bumpmaps:", "Status:", "Type:", "Format:"]):
                    label.setFont(self.panel_font)


    def _apply_button_font(self): #vers 1
        """Apply button font to all buttons"""
        if hasattr(self, 'button_font'):
            for button in self.findChildren(QPushButton):
                button.setFont(self.button_font)


    def _apply_infobar_font(self): #vers 1
        """Apply fixed-width font to info bar at bottom"""
        if hasattr(self, 'infobar_font'):
            if hasattr(self, 'info_bar'):
                self.info_bar.setFont(self.infobar_font)

# - Logic taken from old GUI_layout
# - Theme Functions

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


    def _get_icon_color(self): #vers 1
        """Get icon color from current theme"""
        if APPSETTINGS_AVAILABLE and self.app_settings:
            colors = self.app_settings.get_theme_colors()
            return colors.get('text_primary', '#ffffff')
        return '#ffffff'

# - Tearoff - Panel Functions


    def _toggle_tearoff(self): #vers 2
        """Toggle tear-off state (merge back to IMG Factory) - IMPROVED"""
        try:
            if self.is_docked:
                # Undock from main window
                self._undock_from_main()
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"{App_name} torn off from main window")
            else:
                # Dock back to main window
                self._dock_to_main()
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"{App_name} docked back to main window")

        except Exception as e:
            print(f"Error toggling tear-off: {str(e)}")
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "Tear-off Error", f"Could not toggle tear-off state:\n{str(e)}")


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
                from apps.gui.tear_off import TearOffPanel
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
        """Dock torn off tab widget back to main window """
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


# - Button Functions

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


# - Rest of the logic for the panels

    def _pan_preview(self, dx, dy): #vers 2
        """Pan preview by dx, dy pixels - FIXED"""
        if hasattr(self, 'preview_widget') and self.preview_widget:
            self.preview_widget.pan(dx, dy)

    def _pick_background_color(self): #vers 1
        """Open color picker for background"""
        color = QColorDialog.getColor(self.preview_widget.bg_color, self, "Pick Background Color")
        if color.isValid():
            self.preview_widget.set_background_color(color)

    def _set_checkerboard_bg(self): #vers 1
        """Set checkerboard background"""
        # Create checkerboard pattern
        self.preview_widget.setStyleSheet("""
            border: 1px solid #3a3a3a;
            background-image:
                linear-gradient(45deg, #333 25%, transparent 25%),
                linear-gradient(-45deg, #333 25%, transparent 25%),
                linear-gradient(45deg, transparent 75%, #333 75%),
                linear-gradient(-45deg, transparent 75%, #333 75%);
            background-size: 20px 20px;
            background-position: 0 0, 0 10px, 10px -10px, -10px 0px;
        """)


    def _create_level_card(self, level_data): #vers 2
        """Create modern level card matching mockup"""
        card = QFrame()
        card.setFrameStyle(QFrame.Shape.StyledPanel)
        card.setStyleSheet("""
            QFrame {
                background: #1e1e1e;
                border: 1px solid #3a3a3a;
                border-radius: 5px;
            }
            QFrame:hover {
                border-color: #4a6fa5;
                background: #252525;
            }
        """)
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

    def _create_preview_widget(self, level_data=None): #vers 5
        """Create preview widget - CollisionPreviewWidget for col_workshop"""
        if level_data is None:
            # Return collision preview widget for main preview area
            preview = CollisionPreviewWidget(self)
            print(f"Created CollisionPreviewWidget: {type(preview)}")  # ADD THIS
            return preview

        # Original logic with level_data for mipmap/level cards (if needed)
        level_num = level_data.get('level', 0)
        width = level_data.get('width', 0)
        height = level_data.get('height', 0)
        rgba_data = level_data.get('rgba_data')
        preview_size = max(45, 120 - (level_num * 15))

        preview = QLabel()
        preview.setFixedSize(preview_size, preview_size)
        preview.setStyleSheet("""
            QLabel {
                background: #0a0a0a;
                border: 2px solid #3a3a3a;
                border-radius: 3px;
            }
        """)
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
                preview.setText("No Data")
        else:
            preview.setText("No Data")

        return preview


    def _create_info_section(self, level_data): #vers 1
        """Create info section with stats grid"""
        info_widget = QWidget()
        layout = QVBoxLayout(info_widget)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(10)

        # Header with level number and dimensions
        header_layout = QHBoxLayout()

        level_num = level_data.get('level', 0)
        level_badge = QLabel(f"Level {level_num}")
        level_badge.setStyleSheet("""
            QLabel {
                background: #0d47a1;
                color: white;
                padding: 4px 12px;
                border-radius: 3px;
                font-weight: bold;
                font-size: 13px;
            }
        """)
        header_layout.addWidget(level_badge)

        width = level_data.get('width', 0)
        height = level_data.get('height', 0)
        dim_label = QLabel(f"{width} x {height}")
        dim_label.setStyleSheet("font-size: 16px; font-weight: bold; color: #4a9eff;")
        header_layout.addWidget(dim_label)

        # Main indicator
        if level_num == 0:
            main_badge = QLabel("Main Surface")
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

        fmt = level_data.get('format', self.collision_data.get('format', 'Unknown'))
        size = level_data.get('compressed_size', 0)
        size_kb = size / 1024

        # Format stat
        format_stat = self._create_stat_box("Format:", fmt)
        grid_layout.addWidget(format_stat)

        # Size stat
        size_stat = self._create_stat_box("Size:", f"{size_kb:.1f} KB")
        grid_layout.addWidget(size_stat)
        grid_layout.addWidget(comp_stat)

        # Status stat
        is_modified = level_data.get('level', 0) in self.modified_levels
        status_text = "⚠ Modified" if is_modified else "✓ Valid"
        status_color = "#ff9800" if is_modified else "#4caf50"
        status_stat = self._create_stat_box("Status:", status_text, status_color)
        grid_layout.addWidget(status_stat)

        return grid_widget


    def _create_stat_box(self, label, value, value_color="#e0e0e0"): #vers 1
        """Create individual stat box"""
        stat = QFrame()
        stat.setStyleSheet("""
            QFrame {
                background: #252525;
                border-radius: 3px;
                padding: 6px 10px;
            }
        """)

        layout = QHBoxLayout(stat)
        layout.setContentsMargins(8, 4, 8, 4)

        label_widget = QLabel(label)
        label_widget.setStyleSheet("color: #888; font-size: 12px;")
        layout.addWidget(label_widget)

        value_widget = QLabel(value)
        value_widget.setStyleSheet(f"color: {value_color}; font-weight: bold; font-size: 12px;")
        layout.addWidget(value_widget)

        return stat


    def _create_action_section(self, level_data): #vers 1
        """Create action buttons section"""
        action_widget = QWidget()
        layout = QVBoxLayout(action_widget)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(5)

        level_num = level_data.get('level', 0)

        # Export button
        export_btn = QPushButton("Export")
        export_btn.setStyleSheet("""
            QPushButton {
                background: #2e5d2e;
                border: 1px solid #3d7d3d;
                color: white;
                padding: 6px 12px;
                border-radius: 3px;
                font-size: 11px;
            }
            QPushButton:hover {
                background: #3d7d3d;
            }
        """)
        export_btn.clicked.connect(lambda: self._export_level(level_num))
        layout.addWidget(export_btn)

        # Import button
        import_btn = QPushButton("Import")
        import_btn.setStyleSheet("""
            QPushButton {
                background: #5d3d2e;
                border: 1px solid #7d4d3d;
                color: white;
                padding: 6px 12px;
                border-radius: 3px;
                font-size: 11px;
            }
            QPushButton:hover {
                background: #7d4d3d;
            }
        """)
        import_btn.clicked.connect(lambda: self._import_level(level_num))
        layout.addWidget(import_btn)

        # Delete button (not for level 0) or Edit button (for level 0)
        if level_num == 0:
            edit_btn = QPushButton("Edit")
            edit_btn.setStyleSheet("""
                QPushButton {
                    background: #3a3a3a;
                    border: 1px solid #4a4a4a;
                    color: white;
                    padding: 6px 12px;
                    border-radius: 3px;
                    font-size: 11px;
                }
                QPushButton:hover {
                    background: #4a4a4a;
                }
            """)
            edit_btn.clicked.connect(self._edit_main_surface)
            layout.addWidget(edit_btn)
        else:
            delete_btn = QPushButton("Delete")
            delete_btn.setStyleSheet("""
                QPushButton {
                    background: #5d2e2e;
                    border: 1px solid #7d3d3d;
                    color: white;
                    padding: 6px 12px;
                    border-radius: 3px;
                    font-size: 11px;
                }
                QPushButton:hover {
                    background: #7d3d3d;
                }
            """)
            delete_btn.clicked.connect(lambda: self._delete_level(level_num))
            layout.addWidget(delete_btn)

        return action_widget


# - Functions and main Logic


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
            from apps.gui.status_bar import create_status_bar
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


# - Marker 6

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

    def _open_settings_dialog(self): #vers 1
        """Open settings dialog and refresh on save"""
        dialog = SettingsDialog(self.mel_settings, self)
        if dialog.exec():
            # Refresh platform list with new ROM path
            self._scan_platforms()
            self.status_label.setText("Settings saved - platforms refreshed")


    def _launch_theme_settings(self): #vers 2
        """Launch theme engine from app_settings_system"""
        try:
            from apps.utils.app_settings_system import AppSettings, SettingsDialog

            # Get or create app_settings
            if not hasattr(self, 'app_settings') or self.app_settings is None:
                self.app_settings = AppSettings()
                if not hasattr(self.app_settings, 'current_settings'):
                    print("AppSettings failed to initialize")
                    from PyQt6.QtWidgets import QMessageBox
                    QMessageBox.warning(self, "Error", "Could not initialize theme system")
                    return

            # Launch settings dialog
            dialog = SettingsDialog(self.app_settings, self)

            # Connect theme change signal to apply theme
            dialog.themeChanged.connect(lambda theme: self._apply_theme())

            if dialog.exec():
                # Apply theme after dialog closes
                self._apply_theme()
                print("Theme settings applied")
                if hasattr(self, 'main_window') and self.main_window:
                    if hasattr(self.main_window, 'log_message'):
                        self.main_window.log_message("Theme settings updated")

        except Exception as e:
            print(f"Theme settings error: {e}")
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self, "Theme Error", f"Could not load theme system:\n{e}")


    def _setup_settings_button(self): #vers 1
        """Setup settings button in UI"""
        settings_btn = QPushButton("âš™ Settings")
        settings_btn.clicked.connect(self._open_settings_dialog)
        settings_btn.setMaximumWidth(120)
        return settings_btn


    def _show_settings_dialog(self): #vers 5
        """Show comprehensive settings dialog with all tabs including hotkeys"""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QTabWidget,
                                    QWidget, QLabel, QPushButton, QGroupBox,
                                    QCheckBox, QSpinBox, QFormLayout, QScrollArea,
                                    QKeySequenceEdit, QComboBox, QMessageBox)
        from PyQt6.QtCore import Qt
        from PyQt6.QtGui import QKeySequence

        dialog = QDialog(self)
        dialog.setWindowTitle(App_name + " Settings")
        dialog.setMinimumWidth(700)
        dialog.setMinimumHeight(600)

        layout = QVBoxLayout(dialog)

        # Create tabs
        tabs = QTabWidget()

        # === DISPLAY TAB ===
        display_tab = QWidget()
        display_layout = QVBoxLayout(display_tab)

        # Thumbnail settings
        thumb_group = QGroupBox("Thumbnail Display")
        thumb_layout = QVBoxLayout()

        thumb_size_layout = QHBoxLayout()
        thumb_size_layout.addWidget(QLabel("Thumbnail size:"))
        thumb_size_spin = QSpinBox()
        thumb_size_spin.setRange(32, 256)
        thumb_size_spin.setValue(self.thumbnail_size if hasattr(self, 'thumbnail_size') else 64)
        thumb_size_spin.setSuffix(" px")
        thumb_size_layout.addWidget(thumb_size_spin)
        thumb_size_layout.addStretch()
        thumb_layout.addLayout(thumb_size_layout)

        thumb_group.setLayout(thumb_layout)
        display_layout.addWidget(thumb_group)

        # Table display settings
        table_group = QGroupBox("Table Display")
        table_layout = QVBoxLayout()

        row_height_layout = QHBoxLayout()
        row_height_layout.addWidget(QLabel("Row height:"))
        row_height_spin = QSpinBox()
        row_height_spin.setRange(50, 200)
        row_height_spin.setValue(getattr(self, 'table_row_height', 100))
        row_height_spin.setSuffix(" px")
        row_height_layout.addWidget(row_height_spin)
        row_height_layout.addStretch()
        table_layout.addLayout(row_height_layout)

        show_grid_check = QCheckBox("Show grid lines")
        show_grid_check.setChecked(getattr(self, 'show_grid_lines', True))
        table_layout.addWidget(show_grid_check)

        table_group.setLayout(table_layout)
        display_layout.addWidget(table_group)

        display_layout.addStretch()
        tabs.addTab(display_tab, "Display")

        # === PREVIEW TAB ===
        preview_tab = QWidget()
        preview_layout = QVBoxLayout(preview_tab)

        # Preview window settings
        preview_window_group = QGroupBox("Preview Window")
        preview_window_layout = QVBoxLayout()

        show_preview_check = QCheckBox("Show preview window by default")
        show_preview_check.setChecked(getattr(self, 'show_preview_default', True))
        show_preview_check.setToolTip("Automatically open preview when selecting surface")
        preview_window_layout.addWidget(show_preview_check)

        auto_refresh_check = QCheckBox("Auto-refresh preview on selection")
        auto_refresh_check.setChecked(getattr(self, 'auto_refresh_preview', True))
        auto_refresh_check.setToolTip("Update preview immediately when clicking surface")
        preview_window_layout.addWidget(auto_refresh_check)

        preview_window_group.setLayout(preview_window_layout)
        preview_layout.addWidget(preview_window_group)

        # Preview size settings
        preview_size_group = QGroupBox("Preview Size")
        preview_size_layout = QVBoxLayout()

        preview_width_layout = QHBoxLayout()
        preview_width_layout.addWidget(QLabel("Default width:"))
        preview_width_spin = QSpinBox()
        preview_width_spin.setRange(200, 1920)
        preview_width_spin.setValue(getattr(self, 'preview_width', 512))
        preview_width_spin.setSuffix(" px")
        preview_width_layout.addWidget(preview_width_spin)
        preview_width_layout.addStretch()
        preview_size_layout.addLayout(preview_width_layout)

        preview_height_layout = QHBoxLayout()
        preview_height_layout.addWidget(QLabel("Default height:"))
        preview_height_spin = QSpinBox()
        preview_height_spin.setRange(200, 1080)
        preview_height_spin.setValue(getattr(self, 'preview_height', 512))
        preview_height_spin.setSuffix(" px")
        preview_height_layout.addWidget(preview_height_spin)
        preview_height_layout.addStretch()
        preview_size_layout.addLayout(preview_height_layout)

        preview_size_group.setLayout(preview_size_layout)
        preview_layout.addWidget(preview_size_group)

        # Preview background
        preview_bg_group = QGroupBox("Preview Background")
        preview_bg_layout = QVBoxLayout()

        bg_combo = QComboBox()
        bg_combo.addItems(["Black", "White", "Gray", "Custom Color"])
        bg_combo.setCurrentText(getattr(self, 'preview_background', 'Checkerboard'))
        preview_bg_layout.addWidget(bg_combo)

        preview_bg_group.setLayout(preview_bg_layout)
        preview_layout.addWidget(preview_bg_group)

        # Preview zoom
        preview_zoom_group = QGroupBox("Preview Zoom")
        preview_zoom_layout = QVBoxLayout()

        fit_to_window_check = QCheckBox("Fit to window by default")
        fit_to_window_check.setChecked(getattr(self, 'preview_fit_to_window', True))
        preview_zoom_layout.addWidget(fit_to_window_check)

        smooth_zoom_check = QCheckBox("Use smooth scaling")
        smooth_zoom_check.setChecked(getattr(self, 'preview_smooth_scaling', True))
        smooth_zoom_check.setToolTip("Better quality but slower for large model mesh")
        preview_zoom_layout.addWidget(smooth_zoom_check)

        preview_zoom_group.setLayout(preview_zoom_layout)
        preview_layout.addWidget(preview_zoom_group)

        preview_layout.addStretch()
        tabs.addTab(preview_tab, "Preview")

        # === EXPORT TAB ===
        export_tab = QWidget()
        export_layout = QVBoxLayout(export_tab)

        # Export format
        format_group = QGroupBox("Default Collision Export Format")
        format_layout = QVBoxLayout()

        format_combo = QComboBox()
        format_combo.addItems(["COL", "COL2", "COL3", "CST", "3DS"])
        format_combo.setCurrentText(getattr(self, 'default_export_format', 'COL'))
        format_layout.addWidget(format_combo)
        format_hint = QLabel("COL recommended for GTAIII/VC, COL2 for SA")
        format_hint.setStyleSheet("color: #888; font-style: italic;")
        format_layout.addWidget(format_hint)

        format_group.setLayout(format_layout)
        export_layout.addWidget(format_group)

        # Export options
        export_options_group = QGroupBox("Export Options")
        export_options_layout = QVBoxLayout()

        preserve_shadow_check = QCheckBox("Preserve Shadow Mesh when exporting")
        preserve_shadow_check.setChecked(getattr(self, 'export_preserve_shadow', True))
        export_options_layout.addWidget(preserve_shadow_check)

        export_shadowm_check = QCheckBox("Export shadow as separate files")
        export_shadowm_check.setChecked(getattr(self, 'export_shadow_separate', False))
        export_shadowm_check.setToolTip("Save each shadow map as _shadow.col, etc.")
        export_options_layout.addWidget(export_shadowm_check)

        create_subfolders_check = QCheckBox("Create subfolders when exporting all")
        create_subfolders_check.setChecked(getattr(self, 'export_create_subfolders', False))
        create_subfolders_check.setToolTip("Organize exports into folders by col type")
        export_options_layout.addWidget(create_subfolders_check)

        export_options_group.setLayout(export_options_layout)
        export_layout.addWidget(export_options_group)

        # Compatibility note
        compat_label = QLabel(
            "Note: PLACEholder." #TODO
        )
        compat_label.setWordWrap(True)
        compat_label.setStyleSheet("padding: 10px; background-color: #3a3a3a; border-radius: 4px;")
        export_layout.addWidget(compat_label)

        export_layout.addStretch()
        tabs.addTab(export_tab, "Export")

        # === IMPORT TAB ===
        import_tab = QWidget()
        import_layout = QVBoxLayout(import_tab)

        # Import behavior
        import_behavior_group = QGroupBox("Import Behavior")
        import_behavior_layout = QVBoxLayout()

        replace_check = QCheckBox("Replace existing collision with same name")
        replace_check.setChecked(getattr(self, 'import_replace_existing', False))
        import_behavior_layout.addWidget(replace_check)

        auto_format_check = QCheckBox("Automatically select best format")
        auto_format_check.setChecked(getattr(self, 'import_auto_format', True))
        auto_format_check.setToolTip("Choose COL/COL3 based on collision version")
        import_behavior_layout.addWidget(auto_format_check)

        import_behavior_group.setLayout(import_behavior_layout)
        import_layout.addWidget(import_behavior_group)

        # Import format
        import_format_group = QGroupBox("Default Collision Format")
        import_format_layout = QVBoxLayout()

        import_format_combo = QComboBox()
        import_format_combo.addItems(["COL", "COL2", "COL3", "CST", "3DS"])
        import_format_combo.setCurrentText(getattr(self, 'default_import_format', 'COL'))
        import_format_layout.addWidget(import_format_combo)

        format_note = QLabel("COL2/COL3: compression\n, COL type 1 always Uncompressed")
        format_note.setStyleSheet("color: #888; font-style: italic;")
        import_format_layout.addWidget(format_note)

        import_format_group.setLayout(import_format_layout)
        import_layout.addWidget(import_format_group)

        import_layout.addStretch()
        tabs.addTab(import_tab, "Import")

        # === Collision CONSTRAINTS TAB ===
        constraints_tab = QWidget()
        constraints_layout = QVBoxLayout(constraints_tab)

        # Collision naming
        naming_group = QGroupBox("Collision Naming")
        naming_layout = QVBoxLayout()

        name_limit_check = QCheckBox("Enable name length limit")
        name_limit_check.setChecked(getattr(self, 'name_limit_enabled', True))
        name_limit_check.setToolTip("Enforce maximum Collision name length")
        naming_layout.addWidget(name_limit_check)

        char_limit_layout = QHBoxLayout()
        char_limit_layout.addWidget(QLabel("Maximum characters:"))
        char_limit_spin = QSpinBox()
        char_limit_spin.setRange(8, 64)
        char_limit_spin.setValue(getattr(self, 'max_collision_name_length', 32))
        char_limit_spin.setToolTip("RenderWare default is 32 characters")
        char_limit_layout.addWidget(char_limit_spin)
        char_limit_layout.addStretch()
        naming_layout.addLayout(char_limit_layout)

        naming_group.setLayout(naming_layout)
        constraints_layout.addWidget(naming_group)

        # Format support
        format_support_group = QGroupBox("Format Support")
        format_support_layout = QVBoxLayout()

        format_support_group.setLayout(format_support_layout)
        constraints_layout.addWidget(format_support_group)

        constraints_layout.addStretch()
        tabs.addTab(constraints_tab, "Constraints")

        # === KEYBOARD SHORTCUTS TAB ===
        hotkeys_tab = QWidget()
        hotkeys_layout = QVBoxLayout(hotkeys_tab)

        # Add scroll area for hotkeys
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll_widget = QWidget()
        scroll_layout = QVBoxLayout(scroll_widget)

        # File Operations Group
        file_group = QGroupBox("File Operations")
        file_form = QFormLayout()

        hotkey_edit_open = QKeySequenceEdit(self.hotkey_open.key() if hasattr(self, 'hotkey_open') else QKeySequence.StandardKey.Open)
        file_form.addRow("Open col:", hotkey_edit_open)

        hotkey_edit_save = QKeySequenceEdit(self.hotkey_save.key() if hasattr(self, 'hotkey_save') else QKeySequence.StandardKey.Save)
        file_form.addRow("Save col:", hotkey_edit_save)

        hotkey_edit_force_save = QKeySequenceEdit(self.hotkey_force_save.key() if hasattr(self, 'hotkey_force_save') else QKeySequence("Alt+Shift+S"))
        force_save_layout = QHBoxLayout()
        force_save_layout.addWidget(hotkey_edit_force_save)
        force_save_hint = QLabel("(Force save even if unmodified)")
        force_save_hint.setStyleSheet("color: #888; font-style: italic;")
        force_save_layout.addWidget(force_save_hint)
        file_form.addRow("Force Save:", force_save_layout)

        hotkey_edit_save_as = QKeySequenceEdit(self.hotkey_save_as.key() if hasattr(self, 'hotkey_save_as') else QKeySequence.StandardKey.SaveAs)
        file_form.addRow("Save As:", hotkey_edit_save_as)

        hotkey_edit_close = QKeySequenceEdit(self.hotkey_close.key() if hasattr(self, 'hotkey_close') else QKeySequence.StandardKey.Close)
        file_form.addRow("Close:", hotkey_edit_close)

        file_group.setLayout(file_form)
        scroll_layout.addWidget(file_group)

        # Edit Operations Group
        edit_group = QGroupBox("Edit Operations")
        edit_form = QFormLayout()

        hotkey_edit_undo = QKeySequenceEdit(self.hotkey_undo.key() if hasattr(self, 'hotkey_undo') else QKeySequence.StandardKey.Undo)
        edit_form.addRow("Undo:", hotkey_edit_undo)

        hotkey_edit_copy = QKeySequenceEdit(self.hotkey_copy.key() if hasattr(self, 'hotkey_copy') else QKeySequence.StandardKey.Copy)
        edit_form.addRow("Copy Collision:", hotkey_edit_copy)

        hotkey_edit_paste = QKeySequenceEdit(self.hotkey_paste.key() if hasattr(self, 'hotkey_paste') else QKeySequence.StandardKey.Paste)
        edit_form.addRow("Paste Collision:", hotkey_edit_paste)

        hotkey_edit_delete = QKeySequenceEdit(self.hotkey_delete.key() if hasattr(self, 'hotkey_delete') else QKeySequence.StandardKey.Delete)
        edit_form.addRow("Delete:", hotkey_edit_delete)

        hotkey_edit_duplicate = QKeySequenceEdit(self.hotkey_duplicate.key() if hasattr(self, 'hotkey_duplicate') else QKeySequence("Ctrl+D"))
        edit_form.addRow("Duplicate:", hotkey_edit_duplicate)

        hotkey_edit_rename = QKeySequenceEdit(self.hotkey_rename.key() if hasattr(self, 'hotkey_rename') else QKeySequence("F2"))
        edit_form.addRow("Rename:", hotkey_edit_rename)

        edit_group.setLayout(edit_form)
        scroll_layout.addWidget(edit_group)

        # Collision Operations Group
        coll_group = QGroupBox("Collision Operations")
        coll_group = QFormLayout()

        hotkey_edit_import = QKeySequenceEdit(self.hotkey_import.key() if hasattr(self, 'hotkey_import') else QKeySequence("Ctrl+I"))
        coll_form.addRow("Import Collision:", hotkey_edit_import)

        hotkey_edit_export = QKeySequenceEdit(self.hotkey_export.key() if hasattr(self, 'hotkey_export') else QKeySequence("Ctrl+E"))
        coll_form.addRow("Export Collision:", hotkey_edit_export)

        hotkey_edit_export_all = QKeySequenceEdit(self.hotkey_export_all.key() if hasattr(self, 'hotkey_export_all') else QKeySequence("Ctrl+Shift+E"))
        coll_form.addRow("Export All:", hotkey_edit_export_all)

        coll_group.setLayout(coll_form)
        scroll_layout.addWidget(coll_group)

        # View Operations Group
        view_group = QGroupBox("View Operations")
        view_form = QFormLayout()

        hotkey_edit_refresh = QKeySequenceEdit(self.hotkey_refresh.key() if hasattr(self, 'hotkey_refresh') else QKeySequence.StandardKey.Refresh)
        view_form.addRow("Refresh:", hotkey_edit_refresh)

        hotkey_edit_properties = QKeySequenceEdit(self.hotkey_properties.key() if hasattr(self, 'hotkey_properties') else QKeySequence("Alt+Return"))
        view_form.addRow("Properties:", hotkey_edit_properties)

        hotkey_edit_find = QKeySequenceEdit(self.hotkey_find.key() if hasattr(self, 'hotkey_find') else QKeySequence.StandardKey.Find)
        view_form.addRow("Find/Search:", hotkey_edit_find)

        hotkey_edit_help = QKeySequenceEdit(self.hotkey_help.key() if hasattr(self, 'hotkey_help') else QKeySequence.StandardKey.HelpContents)
        view_form.addRow("Help:", hotkey_edit_help)

        view_group.setLayout(view_form)
        scroll_layout.addWidget(view_group)

        scroll_layout.addStretch()

        scroll.setWidget(scroll_widget)
        hotkeys_layout.addWidget(scroll)

        # Reset to defaults button
        reset_layout = QHBoxLayout()
        reset_layout.addStretch()
        reset_hotkeys_btn = QPushButton("Reset to Plasma6 Defaults")

        def reset_hotkeys():
            reply = QMessageBox.question(dialog, "Reset Hotkeys",
                "Reset all keyboard shortcuts to Plasma6 defaults?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)

            if reply == QMessageBox.StandardButton.Yes:
                hotkey_edit_open.setKeySequence(QKeySequence.StandardKey.Open)
                hotkey_edit_save.setKeySequence(QKeySequence.StandardKey.Save)
                hotkey_edit_force_save.setKeySequence(QKeySequence("Alt+Shift+S"))
                hotkey_edit_save_as.setKeySequence(QKeySequence.StandardKey.SaveAs)
                hotkey_edit_close.setKeySequence(QKeySequence.StandardKey.Close)
                hotkey_edit_undo.setKeySequence(QKeySequence.StandardKey.Undo)
                hotkey_edit_copy.setKeySequence(QKeySequence.StandardKey.Copy)
                hotkey_edit_paste.setKeySequence(QKeySequence.StandardKey.Paste)
                hotkey_edit_delete.setKeySequence(QKeySequence.StandardKey.Delete)
                hotkey_edit_duplicate.setKeySequence(QKeySequence("Ctrl+D"))
                hotkey_edit_rename.setKeySequence(QKeySequence("F2"))
                hotkey_edit_import.setKeySequence(QKeySequence("Ctrl+I"))
                hotkey_edit_export.setKeySequence(QKeySequence("Ctrl+E"))
                hotkey_edit_export_all.setKeySequence(QKeySequence("Ctrl+Shift+E"))
                hotkey_edit_refresh.setKeySequence(QKeySequence.StandardKey.Refresh)
                hotkey_edit_properties.setKeySequence(QKeySequence("Alt+Return"))
                hotkey_edit_find.setKeySequence(QKeySequence.StandardKey.Find)
                hotkey_edit_help.setKeySequence(QKeySequence.StandardKey.HelpContents)

        reset_hotkeys_btn.clicked.connect(reset_hotkeys)
        reset_layout.addWidget(reset_hotkeys_btn)
        hotkeys_layout.addLayout(reset_layout)

        tabs.addTab(hotkeys_tab, "Keyboard Shortcuts")

        # Add tabs widget to main layout
        layout.addWidget(tabs)

        # Dialog buttons
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(dialog.reject)
        button_layout.addWidget(cancel_btn)

        def apply_settings(close_dialog=False):
            """Apply all settings"""
            # Apply display settings
            self.thumbnail_size = thumb_size_spin.value()
            self.table_row_height = row_height_spin.value()
            self.show_grid_lines = show_grid_check.isChecked()

            # Apply preview settings
            self.show_preview_default = show_preview_check.isChecked()
            self.auto_refresh_preview = auto_refresh_check.isChecked()
            self.preview_width = preview_width_spin.value()
            self.preview_height = preview_height_spin.value()
            self.preview_background = bg_combo.currentText()
            self.preview_fit_to_window = fit_to_window_check.isChecked()
            self.preview_smooth_scaling = smooth_zoom_check.isChecked()

            # Apply export settings
            self.default_export_format = format_combo.currentText()
            self.export_preserve_alpha = preserve_alpha_check.isChecked()
            self.export_shadow_separate = export_shadow_check.isChecked()
            self.export_create_subfolders = create_subfolders_check.isChecked()

            # Apply import settings
            self.import_auto_name = auto_name_check.isChecked()
            self.import_replace_existing = replace_check.isChecked()
            self.import_auto_format = auto_format_check.isChecked()
            self.default_import_format = import_format_combo.currentText()

            # Apply constraint settings
            self.dimension_limiting_enabled = dimension_check.isChecked()
            self.splash_screen_mode = splash_check.isChecked()
            self.custom_max_dimension = max_dim_spin.value()
            self.name_limit_enabled = name_limit_check.isChecked()
            self.max_surface_name_length = char_limit_spin.value()
            self.iff_import_enabled = iff_check.isChecked()

            # Apply hotkeys
            if hasattr(self, 'hotkey_open'):
                self.hotkey_open.setKey(hotkey_edit_open.keySequence())
            if hasattr(self, 'hotkey_save'):
                self.hotkey_save.setKey(hotkey_edit_save.keySequence())
            if hasattr(self, 'hotkey_force_save'):
                self.hotkey_force_save.setKey(hotkey_edit_force_save.keySequence())
            if hasattr(self, 'hotkey_save_as'):
                self.hotkey_save_as.setKey(hotkey_edit_save_as.keySequence())
            if hasattr(self, 'hotkey_close'):
                self.hotkey_close.setKey(hotkey_edit_close.keySequence())
            if hasattr(self, 'hotkey_undo'):
                self.hotkey_undo.setKey(hotkey_edit_undo.keySequence())
            if hasattr(self, 'hotkey_copy'):
                self.hotkey_copy.setKey(hotkey_edit_copy.keySequence())
            if hasattr(self, 'hotkey_paste'):
                self.hotkey_paste.setKey(hotkey_edit_paste.keySequence())
            if hasattr(self, 'hotkey_delete'):
                self.hotkey_delete.setKey(hotkey_edit_delete.keySequence())
            if hasattr(self, 'hotkey_duplicate'):
                self.hotkey_duplicate.setKey(hotkey_edit_duplicate.keySequence())
            if hasattr(self, 'hotkey_rename'):
                self.hotkey_rename.setKey(hotkey_edit_rename.keySequence())
            if hasattr(self, 'hotkey_import'):
                self.hotkey_import.setKey(hotkey_edit_import.keySequence())
            if hasattr(self, 'hotkey_export'):
                self.hotkey_export.setKey(hotkey_edit_export.keySequence())
            if hasattr(self, 'hotkey_export_all'):
                self.hotkey_export_all.setKey(hotkey_edit_export_all.keySequence())
            if hasattr(self, 'hotkey_refresh'):
                self.hotkey_refresh.setKey(hotkey_edit_refresh.keySequence())
            if hasattr(self, 'hotkey_properties'):
                self.hotkey_properties.setKey(hotkey_edit_properties.keySequence())
            if hasattr(self, 'hotkey_find'):
                self.hotkey_find.setKey(hotkey_edit_find.keySequence())
            if hasattr(self, 'hotkey_help'):
                self.hotkey_help.setKey(hotkey_edit_help.keySequence())

            # Refresh UI with new settings
            if hasattr(self, '_reload_surface_table'):
                self._reload_surface_table()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("Settings applied")

            if close_dialog:
                dialog.accept()

        apply_btn = QPushButton("Apply")
        apply_btn.clicked.connect(lambda: apply_settings(close_dialog=False))
        button_layout.addWidget(apply_btn)

        ok_btn = QPushButton("OK")
        ok_btn.setDefault(True)
        ok_btn.clicked.connect(lambda: apply_settings(close_dialog=True))
        button_layout.addWidget(ok_btn)

        layout.addLayout(button_layout)

        dialog.exec()


    def _show_settings_context_menu(self, pos): #vers 1
        """Show context menu for Settings button"""
        from PyQt6.QtWidgets import QMenu

        menu = QMenu(self)

        # Move window action
        move_action = menu.addAction("Move Window")
        move_action.triggered.connect(self._enable_move_mode)

        # Maximize window action
        max_action = menu.addAction("Maximize Window")
        max_action.triggered.connect(self._toggle_maximize)

        # Minimize action
        min_action = menu.addAction("Minimize")
        min_action.triggered.connect(self.showMinimized)

        menu.addSeparator()

        # Upscale Native action
        upscale_action = menu.addAction("Upscale Native")
        upscale_action.setCheckable(True)
        upscale_action.setChecked(False)
        upscale_action.triggered.connect(self._toggle_upscale_native)

        # Shaders action
        shaders_action = menu.addAction("Shaders")
        shaders_action.triggered.connect(self._show_shaders_dialog)

        menu.addSeparator()

        # Icon display mode submenu # TODO icon only system is missing.
        display_menu = menu.addMenu("Platform Display")

        icons_text_action = display_menu.addAction("Icons & Text")
        icons_text_action.setCheckable(True)
        icons_text_action.setChecked(self.icon_display_mode == "icons_and_text")
        icons_text_action.triggered.connect(lambda: self._set_icon_display_mode("icons_and_text"))

        icons_only_action = display_menu.addAction("Icons Only")
        icons_only_action.setCheckable(True)
        icons_only_action.setChecked(self.icon_display_mode == "icons_only")
        icons_only_action.triggered.connect(lambda: self._set_icon_display_mode("icons_only"))

        text_only_action = display_menu.addAction("Text Only")
        text_only_action.setCheckable(True)
        text_only_action.setChecked(self.icon_display_mode == "text_only")
        text_only_action.triggered.connect(lambda: self._set_icon_display_mode("text_only"))

        # Show menu at button position
        menu.exec(self.settings_btn.mapToGlobal(pos))

    def _enable_move_mode(self): #vers 2
        """Enable move window mode using system move"""
        # Use Qt's system move which works on Windows, Linux, etc.
        if hasattr(self.windowHandle(), 'startSystemMove'):
            self.windowHandle().startSystemMove()
        else:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.information(self, "Move Window",
                "Drag the titlebar to move the window")

    def _toggle_upscale_native(self): #vers 1
        """Toggle upscale native resolution"""
        # Placeholder for upscale native functionality
        print("Upscale Native toggled")

    def _show_shaders_dialog(self): #vers 1
        """Show shaders configuration dialog"""
        from PyQt6.QtWidgets import QMessageBox
        QMessageBox.information(self, "Shaders",
            "Shader configuration coming soon!\n\nThis will allow you to:\n"
            "- Select shader presets\n"
            "- Configure CRT effects\n"
            "- Adjust visual filters")

    def _show_window_context_menu(self, pos): #vers 1
        """Show context menu for titlebar right-click"""
        from PyQt6.QtWidgets import QMenu


        # Move window action
        move_action = menu.addAction("Move Window")
        move_action.triggered.connect(self._enable_move_mode)

        # Maximize/Restore action
        if self.isMaximized():
            max_action = menu.addAction("Restore Window")
        else:
            max_action = menu.addAction("Maximize Window")
        max_action.triggered.connect(self._toggle_maximize)

        # Minimize action
        min_action = menu.addAction("Minimize")
        min_action.triggered.connect(self.showMinimized)

        menu.addSeparator()

        # Close action
        close_action = menu.addAction("Close")
        close_action.triggered.connect(self.close)

        # Show menu at global position
        menu.exec(self.mapToGlobal(pos))


    def _apply_fonts_to_widgets(self): #vers 1
        """Apply fonts from AppSettings to all widgets"""
        if not hasattr(self, 'default_font'):
            return

        print("\n=== Applying Fonts ===")
        print(f"Default font: {self.default_font.family()} {self.default_font.pointSize()}pt")
        print(f"Title font: {self.title_font.family()} {self.title_font.pointSize()}pt")
        print(f"Panel font: {self.panel_font.family()} {self.panel_font.pointSize()}pt")
        print(f"Button font: {self.button_font.family()} {self.button_font.pointSize()}pt")

        # Apply default font to main window
        self.setFont(self.default_font)

        # Apply title font to titlebar
        if hasattr(self, 'title_label'):
            self.title_label.setFont(self.title_font)

        # Apply panel font to lists
        if hasattr(self, 'platform_list'):
            self.platform_list.setFont(self.panel_font)
        if hasattr(self, 'game_list'):
            self.game_list.setFont(self.panel_font)

        # Apply button font to all buttons
        for btn in self.findChildren(QPushButton):
            btn.setFont(self.button_font)

        print("Fonts applied to widgets")
        print("======================\n")


    def _apply_theme(self): #vers 3
        """Apply theme from app_settings"""
        try:
            # Use self.app_settings first, then fall back to main_window
            app_settings = None
            if hasattr(self, 'app_settings') and self.app_settings:
                app_settings = self.app_settings
            elif self.main_window and hasattr(self.main_window, 'app_settings'):
                app_settings = self.main_window.app_settings

            if app_settings:
                # Get current theme
                theme_name = app_settings.current_settings.get('theme', 'App_Factory')
                stylesheet = app_settings.get_stylesheet()

                # Apply stylesheet
                self.setStyleSheet(stylesheet)

                # Force update
                self.update()

                print(f"Theme applied: {theme_name}")
                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"Theme applied: {theme_name}")
            else:
                # Fallback dark theme
                self.setStyleSheet("""
                    QWidget {
                        background-color: #2b2b2b;
                        color: #e0e0e0;
                    }
                    QListWidget, QTableWidget, QTextEdit {
                        background-color: #1e1e1e;
                        border: 1px solid #3a3a3a;
                    }
                """)
                print("No app_settings found, using fallback theme")
        except Exception as e:
            print(f"Theme application error: {e}")


    def _apply_settings(self, dialog): #vers 5
        """Apply settings from dialog"""
        from PyQt6.QtGui import QFont

        # Store font settings
        self.title_font = QFont(self.title_font_combo.currentFont().family(), self.title_font_size.value())
        self.panel_font = QFont(self.panel_font_combo.currentFont().family(), self.panel_font_size.value())
        self.button_font = QFont(self.button_font_combo.currentFont().family(), self.button_font_size.value())
        self.infobar_font = QFont(self.infobar_font_combo.currentFont().family(), self.infobar_font_size.value())

        # Apply fonts to specific elements
        self._apply_title_font()
        self._apply_panel_font()
        self._apply_button_font()
        self._apply_infobar_font()
        self.default_export_format = format_combo.currentText()

        # Apply button display mode
        mode_map = ["icons", "text", "both"]
        new_mode = mode_map[self.settings_display_combo.currentIndex()]
        if new_mode != self.button_display_mode:
            self.button_display_mode = new_mode
            self._update_all_buttons()

        # Locale setting (would need implementation)
        locale_text = self.settings_locale_combo.currentText()


# - Marker 7

    def _refresh_main_window(self): #vers 1
        """Refresh the main window to show changes"""
        try:
            if self.main_window:
                # Try to refresh the main table
                if hasattr(self.main_window, 'refresh_table'):
                    self.main_window.refresh_table()
                elif hasattr(self.main_window, 'reload_current_file'):
                    self.main_window.reload_current_file()
                elif hasattr(self.main_window, 'update_display'):
                    self.main_window.update_display()

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Refresh error: {str(e)}")


    def _create_preview_widget(self, level_data=None): #vers 3
        """Create preview widget - large collision preview like TXD Workshop"""
        if level_data is None:
            # Return preview label for collision display
            preview = QLabel()
            preview.setMinimumSize(400, 400)
            preview.setAlignment(Qt.AlignmentFlag.AlignCenter)
            preview.setStyleSheet("""
                QLabel {
                    background: #0a0a0a;
                    border: 2px solid #3a3a3a;
                    border-radius: 3px;
                    color: #888;
                }
            """)
            preview.setText("Preview Area\n\nSelect a collision model to preview")
            return preview


    def _toggle_spheres(self, checked): #vers 3
        """Toggle sphere visibility"""
        try:
            if hasattr(self, 'viewer_3d'):
                self.viewer_3d.set_view_options(show_spheres=checked)
            print(f"Spheres visibility: {checked}")
        except Exception as e:
            print(f"Error toggling spheres: {str(e)}")


    def _toggle_boxes(self, checked): #vers 3
        """Toggle box visibility"""
        try:
            if hasattr(self, 'viewer_3d'):
                self.viewer_3d.set_view_options(show_boxes=checked)
            print(f"Boxes visibility: {checked}")
        except Exception as e:
            print(f"Error toggling boxes: {str(e)}")


    def _toggle_mesh(self, checked): #vers 3
        """Toggle mesh visibility"""
        try:
            if hasattr(self, 'viewer_3d'):
                self.viewer_3d.set_view_options(show_mesh=checked)
            print(f"Mesh visibility: {checked}")
        except Exception as e:
            print(f"Error toggling mesh: {str(e)}")


# ----- Render functions


    def _setup_hotkeys_disabled(self): #vers 4 - kept for later
        """Setup Plasma6-style keyboard shortcuts for this application - checks for existing methods"""
        from PyQt6.QtGui import QShortcut, QKeySequence
        from PyQt6.QtCore import Qt

        # === FILE OPERATIONS ===

        # Open col (Ctrl+O)
        self.hotkey_open = QShortcut(QKeySequence.StandardKey.Open, self)
        if hasattr(self, 'open_col_file'):
            self.hotkey_open.activated.connect(self.open_col_file)
        elif hasattr(self, '_open_col_file'):
            self.hotkey_open.activated.connect(self._open_col_file)

        # Save col (Ctrl+S)
        self.hotkey_save = QShortcut(QKeySequence.StandardKey.Save, self)
        if hasattr(self, '_save_col_file'):
            self.hotkey_save.activated.connect(self._save_col_file)
        elif hasattr(self, 'save_col_file'):
            self.hotkey_save.activated.connect(self.save_col_file)

        # Force Save col (Alt+Shift+S)
        self.hotkey_force_save = QShortcut(QKeySequence("Alt+Shift+S"), self)
        if not hasattr(self, '_force_save_col'):
            # Create force save method inline if it doesn't exist
            def force_save():
                if not self.collision_list:
                    from PyQt6.QtWidgets import QMessageBox
                    QMessageBox.warning(self, "No Collision", "No Collision to save")
                    return
                if self.main_window and hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Force save triggered (Alt+Shift+S)")
                # Call save regardless of modified state
                if hasattr(self, '_save_col_file'):
                    self._save_col_file()

            self.hotkey_force_save.activated.connect(force_save)
        else:
            self.hotkey_force_save.activated.connect(self._force_save_col)

        # Save As (Ctrl+Shift+S)
        self.hotkey_save_as = QShortcut(QKeySequence.StandardKey.SaveAs, self)
        if hasattr(self, '_save_as_col_file'):
            self.hotkey_save_as.activated.connect(self._save_as_col_file)
        elif hasattr(self, '_save_col_file'):
            self.hotkey_save_as.activated.connect(self._save_col_file)

        # Close (Ctrl+W)
        self.hotkey_close = QShortcut(QKeySequence.StandardKey.Close, self)
        self.hotkey_close.activated.connect(self.close)

        # === EDIT OPERATIONS ===

        # Undo (Ctrl+Z)
        self.hotkey_undo = QShortcut(QKeySequence.StandardKey.Undo, self)
        if hasattr(self, '_undo_last_action'):
            self.hotkey_undo.activated.connect(self._undo_last_action)
        # else: not implemented yet, no connection

        # Copy (Ctrl+C)
        self.hotkey_copy = QShortcut(QKeySequence.StandardKey.Copy, self)
        if hasattr(self, '_copy_collision'):
            self.hotkey_copy.activated.connect(self._copy_surface)


        # Paste (Ctrl+V)
        self.hotkey_paste = QShortcut(QKeySequence.StandardKey.Paste, self)
        if hasattr(self, '_paste_collision'):
            self.hotkey_paste.activated.connect(self._paste_surface)


        # Delete (Delete)
        self.hotkey_delete = QShortcut(QKeySequence.StandardKey.Delete, self)
        if hasattr(self, '_delete_collision'):
            self.hotkey_delete.activated.connect(self._delete_surface)


        # Duplicate (Ctrl+D)
        self.hotkey_duplicate = QShortcut(QKeySequence("Ctrl+D"), self)
        if hasattr(self, '_duplicate_collision'):
            self.hotkey_duplicate.activated.connect(self._duplicate_surface)


        # Rename (F2)
        self.hotkey_rename = QShortcut(QKeySequence("F2"), self)
        if not hasattr(self, '_rename_collsion_shortcut'):
            # Create rename shortcut method inline
            def rename_shortcut():
                # Focus the name input field if it exists
                if hasattr(self, 'info_name'):
                    self.info_name.setReadOnly(False)
                    self.info_name.selectAll()
                    self.info_name.setFocus()
            self.hotkey_rename.activated.connect(rename_shortcut)
        else:
            self.hotkey_rename.activated.connect(self._rename_shadow_shortcut)

        # === Collision OPERATIONS ===

        # Import Collision (Ctrl+I)
        self.hotkey_import = QShortcut(QKeySequence("Ctrl+I"), self)
        if hasattr(self, '_import_collision'):
            self.hotkey_import.activated.connect(self._import_surface)

        # Export Collision (Ctrl+E)
        self.hotkey_export = QShortcut(QKeySequence("Ctrl+E"), self)
        if hasattr(self, 'export_selected_collision'):
            self.hotkey_export.activated.connect(self.export_selected_surface)

        # Export All (Ctrl+Shift+E)
        self.hotkey_export_all = QShortcut(QKeySequence("Ctrl+Shift+E"), self)
        if hasattr(self, 'export_all_collision'):
            self.hotkey_export_all.activated.connect(self.export_all_surfaces)


        # === VIEW OPERATIONS ===

        # Refresh (F5)d_btn
        self.hotkey_refresh = QShortcut(QKeySequence.StandardKey.Refresh, self)
        if hasattr(self, '_reload_surface_table'):
            self.hotkey_refresh.activated.connect(self._reload_surface_table)
        elif hasattr(self, 'reload_surface_table'):
            self.hotkey_refresh.activated.connect(self.reload_surface_table)
        elif hasattr(self, 'refresh'):
            self.hotkey_refresh.activated.connect(self.refresh)

        # Properties (Alt+Enter)
        self.hotkey_properties = QShortcut(QKeySequence("Alt+Return"), self)
        if hasattr(self, '_show_detailed_info'):
            self.hotkey_properties.activated.connect(self._show_detailed_info)
        elif hasattr(self, '_show_surface_info'):
            self.hotkey_properties.activated.connect(self._show_surface_info)

        # Settings (Ctrl+,)
        self.hotkey_settings = QShortcut(QKeySequence.StandardKey.Preferences, self)
        if hasattr(self, '_show_settings_dialog'):
            self.hotkey_settings.activated.connect(self._show_settings_dialog)
        elif hasattr(self, 'show_settings_dialog'):
            self.hotkey_settings.activated.connect(self.show_settings_dialog)
        elif hasattr(self, '_show_settings_hotkeys'):
            self.hotkey_settings.activated.connect(self._show_settings_hotkeys)

        # === NAVIGATION ===

        # Select All (Ctrl+A) - reserved for future
        self.hotkey_select_all = QShortcut(QKeySequence.StandardKey.SelectAll, self)
        # Not connected - reserved for future multi-select

        # Find (Ctrl+F)
        self.hotkey_find = QShortcut(QKeySequence.StandardKey.Find, self)
        if not hasattr(self, '_focus_search'):
            # Create focus search method inline
            def focus_search():
                if hasattr(self, 'search_input'):
                    self.search_input.setFocus()
                    self.search_input.selectAll()
            self.hotkey_find.activated.connect(focus_search)
        else:
            self.hotkey_find.activated.connect(self._focus_search)

        # === HELP ===

        # Help (F1)
        self.hotkey_help = QShortcut(QKeySequence.StandardKey.HelpContents, self)

        if hasattr(self, 'show_help'):
            self.hotkey_help.activated.connect(self.show_help)

        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message("Hotkeys initialized (Plasma6 standard)")


    def _reset_hotkeys_to_defaults(self, parent_dialog): #vers 1
        """Reset all hotkeys to Plasma6 defaults"""
        from PyQt6.QtWidgets import QMessageBox
        from PyQt6.QtGui import QKeySequence

        reply = QMessageBox.question(parent_dialog, "Reset Hotkeys",
            "Reset all keyboard shortcuts to Plasma6 defaults?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)

        if reply == QMessageBox.StandardButton.Yes:
            # Reset to defaults
            self.hotkey_edit_open.setKeySequence(QKeySequence.StandardKey.Open)
            self.hotkey_edit_save.setKeySequence(QKeySequence.StandardKey.Save)
            self.hotkey_edit_force_save.setKeySequence(QKeySequence("Alt+Shift+S"))
            self.hotkey_edit_save_as.setKeySequence(QKeySequence.StandardKey.SaveAs)
            self.hotkey_edit_close.setKeySequence(QKeySequence.StandardKey.Close)
            self.hotkey_edit_undo.setKeySequence(QKeySequence.StandardKey.Undo)
            self.hotkey_edit_copy.setKeySequence(QKeySequence.StandardKey.Copy)
            self.hotkey_edit_paste.setKeySequence(QKeySequence.StandardKey.Paste)
            self.hotkey_edit_delete.setKeySequence(QKeySequence.StandardKey.Delete)
            self.hotkey_edit_duplicate.setKeySequence(QKeySequence("Ctrl+D"))
            self.hotkey_edit_rename.setKeySequence(QKeySequence("F2"))
            self.hotkey_edit_import.setKeySequence(QKeySequence("Ctrl+I"))
            self.hotkey_edit_export.setKeySequence(QKeySequence("Ctrl+E"))
            self.hotkey_edit_export_all.setKeySequence(QKeySequence("Ctrl+Shift+E"))
            self.hotkey_edit_refresh.setKeySequence(QKeySequence.StandardKey.Refresh)
            self.hotkey_edit_properties.setKeySequence(QKeySequence("Alt+Return"))
            self.hotkey_edit_find.setKeySequence(QKeySequence.StandardKey.Find)
            self.hotkey_edit_help.setKeySequence(QKeySequence.StandardKey.HelpContents)


    def _apply_hotkey_settings(self, dialog, close=False): #vers 1
        """Apply hotkey changes"""
        # Update all hotkeys with new sequences
        self.hotkey_open.setKey(self.hotkey_edit_open.keySequence())
        self.hotkey_save.setKey(self.hotkey_edit_save.keySequence())
        self.hotkey_force_save.setKey(self.hotkey_edit_force_save.keySequence())
        self.hotkey_save_as.setKey(self.hotkey_edit_save_as.keySequence())
        self.hotkey_close.setKey(self.hotkey_edit_close.keySequence())
        self.hotkey_undo.setKey(self.hotkey_edit_undo.keySequence())
        self.hotkey_copy.setKey(self.hotkey_edit_copy.keySequence())
        self.hotkey_paste.setKey(self.hotkey_edit_paste.keySequence())
        self.hotkey_delete.setKey(self.hotkey_edit_delete.keySequence())
        self.hotkey_duplicate.setKey(self.hotkey_edit_duplicate.keySequence())
        self.hotkey_rename.setKey(self.hotkey_edit_rename.keySequence())
        self.hotkey_import.setKey(self.hotkey_edit_import.keySequence())
        self.hotkey_export.setKey(self.hotkey_edit_export.keySequence())
        self.hotkey_export_all.setKey(self.hotkey_edit_export_all.keySequence())
        self.hotkey_refresh.setKey(self.hotkey_edit_refresh.keySequence())
        self.hotkey_properties.setKey(self.hotkey_edit_properties.keySequence())
        self.hotkey_find.setKey(self.hotkey_edit_find.keySequence())
        self.hotkey_help.setKey(self.hotkey_edit_help.keySequence())

        if self.main_window and hasattr(self.main_window, 'log_message'):
            self.main_window.log_message("Hotkeys updated")

        # TODO: Save to config file for persistence

        if close:
            dialog.accept()


    def _show_settings_hotkeys(self): #vers 1
        """Show settings dialog with hotkey customization"""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QTabWidget,
                                    QWidget, QLabel, QLineEdit, QPushButton,
                                    QGroupBox, QFormLayout, QKeySequenceEdit)
        from PyQt6.QtCore import Qt

        dialog = QDialog(self)
        dialog.setWindowTitle(App_name + " Settings")
        dialog.setMinimumWidth(600)
        dialog.setMinimumHeight(500)

        layout = QVBoxLayout(dialog)

        # Create tabs
        tabs = QTabWidget()

        # === HOTKEYS TAB ===
        hotkeys_tab = QWidget()
        hotkeys_layout = QVBoxLayout(hotkeys_tab)

        # File Operations Group
        file_group = QGroupBox("File Operations")
        file_form = QFormLayout()

        self.hotkey_edit_open = QKeySequenceEdit(self.hotkey_open.key())
        file_form.addRow("Open col:", self.hotkey_edit_open)

        self.hotkey_edit_save = QKeySequenceEdit(self.hotkey_save.key())
        file_form.addRow("Save col:", self.hotkey_edit_save)

        self.hotkey_edit_force_save = QKeySequenceEdit(self.hotkey_force_save.key())
        force_save_layout = QHBoxLayout()
        force_save_layout.addWidget(self.hotkey_edit_force_save)
        force_save_hint = QLabel("(Force save even if unmodified)")
        force_save_hint.setStyleSheet("color: #888; font-style: italic;")
        force_save_layout.addWidget(force_save_hint)
        file_form.addRow("Force Save:", force_save_layout)

        self.hotkey_edit_save_as = QKeySequenceEdit(self.hotkey_save_as.key())
        file_form.addRow("Save As:", self.hotkey_edit_save_as)

        self.hotkey_edit_close = QKeySequenceEdit(self.hotkey_close.key())
        file_form.addRow("Close:", self.hotkey_edit_close)

        file_group.setLayout(file_form)
        hotkeys_layout.addWidget(file_group)

        # Edit Operations Group
        edit_group = QGroupBox("Edit Operations")
        edit_form = QFormLayout()

        self.hotkey_edit_undo = QKeySequenceEdit(self.hotkey_undo.key())
        edit_form.addRow("Undo:", self.hotkey_edit_undo)

        self.hotkey_edit_copy = QKeySequenceEdit(self.hotkey_copy.key())
        edit_form.addRow("Copy Collision:", self.hotkey_edit_copy)

        self.hotkey_edit_paste = QKeySequenceEdit(self.hotkey_paste.key())
        edit_form.addRow("Paste Collision:", self.hotkey_edit_paste)

        self.hotkey_edit_delete = QKeySequenceEdit(self.hotkey_delete.key())
        edit_form.addRow("Delete:", self.hotkey_edit_delete)

        self.hotkey_edit_duplicate = QKeySequenceEdit(self.hotkey_duplicate.key())
        edit_form.addRow("Duplicate:", self.hotkey_edit_duplicate)

        self.hotkey_edit_rename = QKeySequenceEdit(self.hotkey_rename.key())
        edit_form.addRow("Rename:", self.hotkey_edit_rename)

        edit_group.setLayout(edit_form)
        hotkeys_layout.addWidget(edit_group)

        # Collision Group
        coll_group = QGroupBox("Collision Operations")
        coll_form = QFormLayout()

        self.hotkey_edit_import = QKeySequenceEdit(self.hotkey_import.key())
        coll_form.addRow("Import Collision:", self.hotkey_edit_import)

        self.hotkey_edit_export = QKeySequenceEdit(self.hotkey_export.key())
        coll_form.addRow("Export Collision:", self.hotkey_edit_export)

        self.hotkey_edit_export_all = QKeySequenceEdit(self.hotkey_export_all.key())
        coll_form.addRow("Export All:", self.hotkey_edit_export_all)

        coll_group.setLayout(coll_form)
        hotkeys_layout.addWidget(coll_group)

        # View Operations Group
        view_group = QGroupBox("View Operations")
        view_form = QFormLayout()

        self.hotkey_edit_refresh = QKeySequenceEdit(self.hotkey_refresh.key())
        view_form.addRow("Refresh:", self.hotkey_edit_refresh)

        self.hotkey_edit_properties = QKeySequenceEdit(self.hotkey_properties.key())
        view_form.addRow("Properties:", self.hotkey_edit_properties)

        self.hotkey_edit_find = QKeySequenceEdit(self.hotkey_find.key())
        view_form.addRow("Find/Search:", self.hotkey_edit_find)

        self.hotkey_edit_help = QKeySequenceEdit(self.hotkey_help.key())
        view_form.addRow("Help:", self.hotkey_edit_help)

        view_group.setLayout(view_form)
        hotkeys_layout.addWidget(view_group)

        hotkeys_layout.addStretch()

        # Reset to defaults button
        reset_hotkeys_btn = QPushButton("Reset to Plasma6 Defaults")
        reset_hotkeys_btn.clicked.connect(lambda: self._reset_hotkeys_to_defaults(dialog))
        hotkeys_layout.addWidget(reset_hotkeys_btn)

        tabs.addTab(hotkeys_tab, "Keyboard Shortcuts")

        # === GENERAL TAB (for future settings) ===
        general_tab = QWidget()
        general_layout = QVBoxLayout(general_tab)

        placeholder_label = QLabel("Additional settings will appear here in future versions.")
        placeholder_label.setStyleSheet("color: #888; font-style: italic; padding: 20px;")
        placeholder_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        general_layout.addWidget(placeholder_label)
        general_layout.addStretch()

        tabs.addTab(general_tab, "General")

        layout.addWidget(tabs)

        # Dialog buttons
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(dialog.reject)
        button_layout.addWidget(cancel_btn)

        apply_btn = QPushButton("Apply")
        apply_btn.clicked.connect(lambda: self._apply_hotkey_settings(dialog))
        button_layout.addWidget(apply_btn)

        ok_btn = QPushButton("OK")
        ok_btn.setDefault(True)
        ok_btn.clicked.connect(lambda: self._apply_hotkey_settings(dialog, close=True))
        button_layout.addWidget(ok_btn)

        layout.addLayout(button_layout)

        dialog.exec()


    def _show_img_info(self): #vers 4
        """Show TXD Workshop information dialog - About and capabilities"""
        dialog = QDialog(self)
        dialog.setWindowTitle("About" + App_name)
        dialog.setMinimumWidth(600)
        dialog.setMinimumHeight(500)

        layout = QVBoxLayout(dialog)
        layout.setSpacing(15)

        # Header
        header = QLabel(App_name)
        header.setFont(QFont("Arial", 14, QFont.Weight.Bold))
        header.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(header)

        # Author info
        author_label = QLabel("Author: X-Seti")
        author_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(author_label)

        # Version info
        version_label = QLabel("Version: 1.5 - November 2025")
        version_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(version_label)

        layout.addWidget(QLabel(""))  # Spacer

        # Capabilities section
        capabilities = QTextEdit()
        capabilities.setReadOnly(True)
        capabilities.setMaximumHeight(350)

        info_text = """<b>COL Workshop Capabilities:</b><br><br>

<b>✓ File Operations:</b><br>


<b>✓ Collision Viewing & Editing:</b><br>


<b>✓ Collision Management:</b><br>


<b>✓ Collision Surface Painting:</b><br>


<b>✓ Format Support:</b><br>


<b>✓ Advanced Features:</b><br>"""

        # Add format support dynamically
        formats_available = []

        # Standard formats (always via PIL)

        info_text += "<br>".join(formats_available)
        info_text += "<br><br>"

        # Settings info
        info_text += """<b>✓ Customization:</b><br>
- Adjustable texture name length (8-64 chars)<br>
- Button display modes (Icons/Text/Both)<br>
- Font customization<br>
- Preview zoom and pan offsets<br><br>

<b>Keyboard Shortcuts:</b><br>
- Ctrl+O: Open COL<br>
- Ctrl+S: Save COL<br>
- Ctrl+I: Import Collision col, cst, 3ds<br>
- Ctrl+E: Export Selected col, cst, 3ds<br>
- Ctrl+Z: Undo<br>
- Delete: Remove Collision<br>
- Ctrl+D: Duplicate Collision<br>"""

        capabilities.setHtml(info_text)
        layout.addWidget(capabilities)

        # Close button
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(dialog.accept)
        close_btn.setDefault(True)
        layout.addWidget(close_btn)

        dialog.exec()


#class SvgIcons: #vers 1 - Once functions are updated this class will be moved to the bottom
    """SVG icon data to QIcon with theme color support"""

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

#moved to scg_icon_factory

class ZoomablePreview(QLabel): #vers 2
    """Fixed preview widget with zoom and pan"""

    def __init__(self, parent=None):
        self.icon_factory = SVGIconFactory()
        super().__init__(parent)
        self.main_window = parent
        self.setMinimumSize(400, 400)
        self.setAlignment(Qt.AlignmentFlag.AlignCenter)
        #self.setStyleSheet("border: 2px solid #3a3a3a; background: #0a0a0a;")
        self.setStyleSheet("border: 1px solid #3a3a3a;")
        self.setMouseTracking(True)

        # Display state
        self.current_model = None
        self.original_pixmap = None
        self.scaled_pixmap = None

        # View controls
        self.zoom_level = 1.0
        self.pan_offset = QPoint(0, 0)
        self.rotation_x = 45  # X-axis rotation (up/down tilt)
        self.rotation_y = 0   # Y-axis rotation (left/right spin)
        self.rotation_z = 0   # Z-axis rotation (roll)

        # View toggles
        self.show_spheres = True
        self.show_boxes = True
        self.show_mesh = True

        # Mouse interaction
        self.dragging = False
        self.drag_start = QPoint(0, 0)
        self.drag_mode = None  # 'pan' or 'rotate'

        # Background
        self.bg_color = QColor(42, 42, 42)

        self.placeholder_text = "Select a collision model to preview"

        self.background_mode = 'solid'
        self._checkerboard_size = 16


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


    def set_model(self, model): #vers 1
        """Set collision model to display"""
        self.current_model = model
        self.render_collision()


    def render_collision(self): #vers 2
        """Render the collision model with current view settings"""
        if not self.current_model:
            self.setText(self.placeholder_text)
            self.original_pixmap = None
            self.scaled_pixmap = None
            return

        width = max(400, self.width())
        height = max(400, self.height())

        # Use the parent's render method
        if hasattr(self.parent(), '_render_collision_preview'):
            self.original_pixmap = self.parent()._render_collision_preview(
                self.current_model,
                width,
                height
            )
        else:
            # Fallback - just show text for now
            name = getattr(self.current_model, 'name', 'Unknown')
            self.setText(f"Collision Model: {name}\n\nRendering...")
            return

        self._update_scaled_pixmap()
        self.update()


    def _update_scaled_pixmap(self): #vers
        """Update scaled pixmap based on zoom"""
        if not self.original_pixmap:
            self.scaled_pixmap = None
            return

        scaled_width = int(self.original_pixmap.width() * self.zoom_level)
        scaled_height = int(self.original_pixmap.height() * self.zoom_level)

        self.scaled_pixmap = self.original_pixmap.scaled(
            scaled_width, scaled_height,
            Qt.AspectRatioMode.KeepAspectRatio,
            Qt.TransformationMode.SmoothTransformation
        )


    def paintEvent(self, event): #vers 2
        """Paint the preview with background and image"""
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        painter.setRenderHint(QPainter.RenderHint.SmoothPixmapTransform)

        # Draw background
        if self.background_mode == 'checkerboard':
            self._draw_checkerboard(painter)
        else:
            painter.fillRect(self.rect(), self.bg_color)

        # Draw image if available
        if self.scaled_pixmap and not self.scaled_pixmap.isNull():
            # Calculate centered position with pan offset
            x = (self.width() - self.scaled_pixmap.width()) // 2 + self.pan_offset.x()
            y = (self.height() - self.scaled_pixmap.height()) // 2 + self.pan_offset.y()
            painter.drawPixmap(x, y, self.scaled_pixmap)
        elif self.placeholder_text:
            # Draw placeholder text
            painter.setPen(QColor(150, 150, 150))
            painter.drawText(self.rect(), Qt.AlignmentFlag.AlignCenter, self.placeholder_text)


    def set_checkerboard_background(self): #vers 1
        """Enable checkerboard background"""
        self.background_mode = 'checkerboard'
        self.update()


    def set_background_color(self, color): #vers 1
        """Set solid background color"""
        self.background_mode = 'solid'
        self.bg_color = color
        self.update()


    def _draw_checkerboard(self, painter): #vers 1
        """Draw checkerboard background pattern"""
        size = self._checkerboard_size
        color1 = QColor(200, 200, 200)
        color2 = QColor(150, 150, 150)

        for y in range(0, self.height(), size):
            for x in range(0, self.width(), size):
                color = color1 if ((x // size) + (y // size)) % 2 == 0 else color2
                painter.fillRect(x, y, size, size, color)


    # Zoom controls
    def zoom_in(self): #vers 1
        """Zoom in"""
        self.zoom_level = min(5.0, self.zoom_level * 1.2)
        self._update_scaled_pixmap()
        self.update()


    def zoom_out(self): #vers 1
        """Zoom out"""
        self.zoom_level = max(0.1, self.zoom_level / 1.2)
        self._update_scaled_pixmap()
        self.update()


    def reset_view(self): #vers 1
        """Reset to default view"""
        self.zoom_level = 1.0
        self.pan_offset = QPoint(0, 0)
        self.rotation_x = 45
        self.rotation_y = 0
        self.rotation_z = 0
        self.render_collision()


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


    # Rotation controls
    def rotate_x(self, degrees): #vers 1
        """Rotate around X axis"""
        self.rotation_x = (self.rotation_x + degrees) % 360
        self.render_collision()


    def rotate_y(self, degrees): #vers 1
        """Rotate around Y axis"""
        self.rotation_y = (self.rotation_y + degrees) % 360
        self.render_collision()


    def rotate_z(self, degrees): #vers 1
        """Rotate around Z axis"""
        self.rotation_z = (self.rotation_z + degrees) % 360
        self.render_collision()


    # Mouse events
    def mousePressEvent(self, event): #vers 1
        """Handle mouse press"""
        if event.button() == Qt.MouseButton.LeftButton:
            self.dragging = True
            self.drag_start = event.pos()
            self.drag_mode = 'rotate' if event.modifiers() & Qt.KeyboardModifier.ControlModifier else 'pan'


    def mouseMoveEvent(self, event): #vers 1
        """Handle mouse drag"""
        if self.dragging:
            delta = event.pos() - self.drag_start

            if self.drag_mode == 'rotate':
                # Rotate based on drag
                self.rotate_y(delta.x() * 0.5)
                self.rotate_x(-delta.y() * 0.5)
            else:
                # Pan
                self.pan_offset += delta
                self.update()

            self.drag_start = event.pos()


    def mouseReleaseEvent(self, event): #vers 1
        """Handle mouse release"""
        if event.button() == Qt.MouseButton.LeftButton:
            self.dragging = False
            self.drag_mode = None


    def wheelEvent(self, event): #vers 1
        """Handle mouse wheel for zoom"""
        delta = event.angleDelta().y()
        if delta > 0:
            self.zoom_in()
        else:
            self.zoom_out()


class IMGEditorDialog(QDialog): #vers 3
    """Enhanced COL Editor Dialog"""


    def __init__(self, parent=None):
        self.icon_factory = SVGIconFactory()
        super().__init__(parent)
        self.setWindowTitle(App_name + " - IMG Factory 1.5")
        self.setModal(False)  # Allow non-modal operation
        self.resize(1000, 700)

        self.current_file = None
        self.current_model = None
        self.file_path = None
        self.is_modified = False

        self.setup_ui()
        self.connect_signals()

        print(App_name + " dialog created")


    def setup_ui(self): #vers 1
        """Setup editor UI"""
        layout = QVBoxLayout(self)

        # Toolbar
        self.toolbar = COLToolbar(self)
        layout.addWidget(self.toolbar)

        # Main splitter
        main_splitter = QSplitter(Qt.Orientation.Horizontal)
        layout.addWidget(main_splitter)

        # Left panel - Model list and properties
        left_panel = QSplitter(Qt.Orientation.Vertical)
        left_panel.setFixedWidth(350)

        # Model list
        models_group = QGroupBox("Models")
        models_layout = QVBoxLayout(models_group)

        self.model_list = COLModelListWidget()
        models_layout.addWidget(self.model_list)

        left_panel.addWidget(models_group)

        # Properties
        properties_group = QGroupBox("Properties")
        properties_layout = QVBoxLayout(properties_group)

        self.properties_widget = COLPropertiesWidget()
        properties_layout.addWidget(self.properties_widget)

        left_panel.addWidget(properties_group)

        # Set left panel sizes
        left_panel.setSizes([200, 400])

        main_splitter.addWidget(left_panel)

        # Right panel - 3D viewer
        viewer_group = QGroupBox("3D Viewer")
        viewer_layout = QVBoxLayout(viewer_group)

        if VIEWPORT_AVAILABLE:
            self.viewer_3d = COL3DViewport()
            viewer_layout.addWidget(self.viewer_3d)

            # Add 3DS Max style controls at bottom
            controls = self._create_viewport_controls()
            viewer_layout.addWidget(controls)
        else:
            self.viewer_3d = QLabel("3D Viewport unavailable\nInstall: pip install PyOpenGL")
            self.viewer_3d.setAlignment(Qt.AlignmentFlag.AlignCenter)
            viewer_layout.addWidget(self.viewer_3d)

        # Set main splitter sizes
        main_splitter.setSizes([350, 650])

        # Status bar
        self.status_bar = QStatusBar()
        self.status_bar.showMessage("Ready")
        layout.addWidget(self.status_bar)

        # Progress bar (hidden by default)
        self.progress_bar = QProgressBar()
        self.progress_bar.setVisible(False)
        layout.addWidget(self.progress_bar)


    def connect_signals(self): #vers 1
        """Connect UI signals"""
        # Toolbar actions
        self.toolbar.open_action.triggered.connect(self.open_file)
        self.toolbar.save_action.triggered.connect(self.save_file)
        self.toolbar.analyze_action.triggered.connect(self.analyze_file)

        # View options
        self.toolbar.view_spheres_action.toggled.connect(
            lambda checked: self.viewer_3d.set_view_options(show_spheres=checked)
        )
        self.toolbar.view_boxes_action.toggled.connect(
            lambda checked: self.viewer_3d.set_view_options(show_boxes=checked)
        )
        self.toolbar.view_mesh_action.toggled.connect(
            lambda checked: self.viewer_3d.set_view_options(show_mesh=checked)
        )

        # Model selection
        self.model_list.model_selected.connect(self.on_model_selected)
        self.viewer_3d.model_selected.connect(self.on_model_selected)

        # Properties changes
        self.properties_widget.property_changed.connect(self.on_property_changed)


    def _create_viewport_controls(self): #vers 1
        """Create 3D viewport controls - 3DS Max style toolbar at bottom"""
        if not VIEWPORT_AVAILABLE:
            return QWidget()

        controls_widget = QFrame()
        controls_widget.setFrameStyle(QFrame.Shape.StyledPanel | QFrame.Shadow.Sunken)
        controls_widget.setStyleSheet("""
            QFrame {
                background-color: #3a3a3a;
                border: 1px solid #555555;
                border-radius: 3px;
                padding: 3px;
            }
            QPushButton {
                background-color: #4a4a4a;
                border: 1px solid #666666;
                border-radius: 2px;
                padding: 4px;
                min-width: 28px;
                min-height: 28px;
            }
            QPushButton:hover {
                background-color: #5a5a5a;
                border: 1px solid #888888;
            }
            QPushButton:pressed {
                background-color: #2a2a2a;
            }
            QPushButton:checked {
                background-color: #006699;
                border: 1px solid #0088cc;
            }
        """)

        layout = QHBoxLayout(controls_widget)
        layout.setContentsMargins(4, 2, 4, 2)
        layout.setSpacing(2)

        # View mode buttons
        btn_spheres = QPushButton()
        btn_spheres.setIcon(self.icon_factory.sphere_icon())
        btn_spheres.setCheckable(True)
        btn_spheres.setChecked(True)
        btn_spheres.setToolTip("Toggle Spheres")
        btn_spheres.toggled.connect(
            lambda checked: self.viewer_3d.set_view_options(show_spheres=checked)
        )

        btn_boxes = QPushButton()
        btn_boxes.setIcon(self.icon_factory.box_icon())
        btn_boxes.setCheckable(True)
        btn_boxes.setChecked(True)
        btn_boxes.setToolTip("Toggle Boxes")
        btn_boxes.toggled.connect(
            lambda checked: self.viewer_3d.set_view_options(show_boxes=checked)
        )

        btn_mesh = QPushButton()
        btn_mesh.setIcon(self.icon_factory.mesh_icon())
        btn_mesh.setCheckable(True)
        btn_mesh.setChecked(True)
        btn_mesh.setToolTip("Toggle Mesh")
        btn_mesh.toggled.connect(
            lambda checked: self.viewer_3d.set_view_options(show_mesh=checked)
        )

        btn_wireframe = QPushButton()
        btn_wireframe.setIcon(self.icon_factory.wireframe_icon())
        btn_wireframe.setCheckable(True)
        btn_wireframe.setChecked(True)
        btn_wireframe.setToolTip("Toggle Wireframe")
        btn_wireframe.toggled.connect(
            lambda checked: self.viewer_3d.set_view_options(show_wireframe=checked)
        )

        btn_bounds = QPushButton()
        btn_bounds.setIcon(self.icon_factory.bounds_icon())
        btn_bounds.setCheckable(True)
        btn_bounds.setChecked(True)
        btn_bounds.setToolTip("Toggle Bounding Box")
        btn_bounds.toggled.connect(
            lambda checked: self.viewer_3d.set_view_options(show_bounds=checked)
        )

        # Separator
        separator1 = QFrame()
        separator1.setFrameShape(QFrame.Shape.VLine)
        separator1.setFrameShadow(QFrame.Shadow.Sunken)
        separator1.setStyleSheet("color: #666666;")

        # Camera controls
        btn_reset = QPushButton()
        btn_reset.setIcon(self.icon_factory.reset_view_icon())
        btn_reset.setToolTip("Reset View")
        btn_reset.clicked.connect(self.viewer_3d.reset_view)

        btn_top = QPushButton("T")
        btn_top.setToolTip("Top View")
        btn_top.clicked.connect(lambda: self._set_camera_view('top'))

        btn_front = QPushButton("F")
        btn_front.setToolTip("Front View")
        btn_front.clicked.connect(lambda: self._set_camera_view('front'))

        btn_side = QPushButton("S")
        btn_side.setToolTip("Side View")
        btn_side.clicked.connect(lambda: self._set_camera_view('side'))

        # Add widgets to layout
        layout.addWidget(btn_spheres)
        layout.addWidget(btn_boxes)
        layout.addWidget(btn_mesh)
        layout.addWidget(btn_wireframe)
        layout.addWidget(btn_bounds)
        layout.addWidget(separator1)
        layout.addWidget(btn_reset)
        layout.addWidget(btn_top)
        layout.addWidget(btn_front)
        layout.addWidget(btn_side)
        layout.addStretch()

        return controls_widget


    def _set_camera_view(self, view_type): #vers 1
        """Set predefined camera view"""
        if not VIEWPORT_AVAILABLE or not hasattr(self, 'viewer_3d'):
            return

        if view_type == 'top':
            self.viewer_3d.rotation_x = 0.0
            self.viewer_3d.rotation_y = 0.0
        elif view_type == 'front':
            self.viewer_3d.rotation_x = 90.0
            self.viewer_3d.rotation_y = 0.0
        elif view_type == 'side':
            self.viewer_3d.rotation_x = 90.0
            self.viewer_3d.rotation_y = 90.0

        self.viewer_3d.update()


    def _svg_to_icon(self, svg_data, size=24): #vers 1
        """Convert SVG to QIcon"""
        from PyQt6.QtGui import QIcon, QPixmap, QPainter, QColor
        from PyQt6.QtSvg import QSvgRenderer
        from PyQt6.QtCore import QByteArray

        try:
            text_color = self.palette().color(self.foregroundRole())
            svg_str = svg_data.decode('utf-8')
            svg_str = svg_str.replace('currentColor', text_color.name())
            svg_data = svg_str.encode('utf-8')

            renderer = QSvgRenderer(QByteArray(svg_data))
            if not renderer.isValid():
                print(f"Invalid SVG data in col_workshop")
                return QIcon()

            pixmap = QPixmap(size, size)
            pixmap.fill(QColor(0, 0, 0, 0))

            painter = QPainter(pixmap)
            renderer.render(painter)
            painter.end()

            return QIcon(pixmap)
        except:
            return QIcon()


    def on_property_changed(self, property_name: str, new_value): #vers 2
        """Handle property changes from properties widget"""
        try:
            if not self.current_file or not hasattr(self.current_file, 'models'):
                return

            selected_index = self.model_list.currentRow()
            if selected_index < 0 or selected_index >= len(self.current_file.models):
                return

            current_model = self.current_file.models[selected_index]

            # Update model properties
            if property_name == 'name':
                current_model.name = str(new_value)
                self.model_list.item(selected_index).setText(new_value)
            elif property_name == 'version':
                current_model.version = new_value
            elif property_name == 'material':
                if hasattr(current_model, 'material'):
                    current_model.material = new_value

            # Mark as modified
            self.is_modified = True
            self.status_bar.showMessage(f"Modified: {property_name} changed")

            # Update viewer if needed
            if hasattr(self, 'viewer_3d') and VIEWPORT_AVAILABLE:
                self.viewer_3d.set_current_model(current_model, selected_index)

            print(f"Property changed: {property_name} = {new_value}")

        except Exception as e:
            print(f"Error handling property change: {str(e)}")
            self.status_bar.showMessage(f"Error: {str(e)}")


        def _set_camera_view(self, view_type): #vers 1
            """Set predefined camera view"""
            if view_type == 'top':
                self.viewer_3d.rotation_x = 0.0
                self.viewer_3d.rotation_y = 0.0
            elif view_type == 'front':
                self.viewer_3d.rotation_x = 90.0
                self.viewer_3d.rotation_y = 0.0
            elif view_type == 'side':
                self.viewer_3d.rotation_x = 90.0
                self.viewer_3d.rotation_y = 90.0

            self.viewer_3d.update()



    def closeEvent(self, event): #vers 1
        """Handle close event"""
        if self.is_modified:
            reply = QMessageBox.question(
                self, "Unsaved Changes",
                "The file has unsaved changes. Do you want to save before closing?",
                QMessageBox.StandardButton.Save |
                QMessageBox.StandardButton.Discard |
                QMessageBox.StandardButton.Cancel
            )

            if reply == QMessageBox.StandardButton.Save:
                self.save_file()
                event.accept()
            elif reply == QMessageBox.StandardButton.Discard:
                event.accept()
            else:
                event.ignore()
        else:
            event.accept()

        print("COL Editor dialog closed")


    # Add import/export functionality when docked



# Convenience functions
def open_col_editor(parent=None, file_path: str = None) -> IMGEditorDialog: #vers 2
    """Open COL editor dialog - ENHANCED VERSION"""
    try:
        editor = IMGEditorDialog(parent)

        if file_path:
            if editor.load_col_file(file_path):
                print(f"COL editor opened with file: {file_path}")
            else:
                print(f"Failed to load file in COL editor: {file_path}")

        editor.show()
        return editor

    except Exception as e:
        print(f"Error opening COL editor: {str(e)}")
        if parent:
            QMessageBox.critical(parent, "COL Editor Error", f"Failed to open COL editor:\n{str(e)}")
        return None


def update_view_options(viewer: 'COL3DViewport', **options): #vers 1
    """Update 3D viewer options"""
    try:
        viewer.set_view_options(**options)
        print(f"View options updated: {options}")

        def _ensure_standalone_functionality(self): #vers 1
            """Ensure popped-out windows work independently of img factory"""
            try:
                # When popped out, ensure all necessary functionality is available
                if not self.is_docked or getattr(self, 'is_overlay', False) == False:
                    # Enable all UI elements that might be disabled when docked
                    if hasattr(self, 'toolbar') and self.toolbar:
                        self.toolbar.setEnabled(True)

                    # Ensure all buttons and controls work independently
                    if hasattr(self, 'main_window') and self.main_window is None:
                        # This is truly standalone, enable all features
                        if hasattr(self, 'dock_btn'):
                            self.dock_btn.setText("X")  # Change to close button when standalone
                            self.dock_btn.setToolTip("Close window")

                    # Set proper window flags for standalone operation
                    if not getattr(self, 'is_overlay', False):
                        self.setWindowFlags(Qt.WindowType.Window)

            except Exception as e:
                print(f"Error ensuring standalone functionality: {str(e)}")

    except Exception as e:
        print(f"Error updating view options: {str(e)}")



def apply_changes(editor: IMGEditorDialog) -> bool: #vers 1
    """Apply all pending changes"""
    try:
        # TODO: Implement change application
        print("Apply changes - not yet implemented")
        return True

    except Exception as e:
        print(f"Error applying changes: {str(e)}")
        return False


# --- External AI upscaler integration helper ---
import subprocess
import tempfile
import shutil
import sys


def open_workshop(main_window, img_path=None): #vers 3
    """Open Workshop from main window - works with or without IMG"""
    try:
        workshop = COLWorkshop(main_window, main_window)

        if img_path:
            # Check if it's a col file or IMG file
            if img_path.lower().endswith('.col'):
                # Load standalone col file
                workshop.open_col_file(img_path)
            else:
                # Load from IMG archive
                workshop.load_from_img_archive(img_path)
        else:
            # Open in standalone mode (no IMG loaded)
            if main_window and hasattr(main_window, 'log_message'):
                main_window.log_message(App_name + " opened in standalone mode")

        workshop.show()
        return workshop
    except Exception as e:
        QMessageBox.critical(main_window, App_name * " Error", f"Failed to open: {str(e)}")
        return None


# Compatibility alias for imports
IMGEditorDialog = IMGFactoryGUILayout  #vers 1


def open_col_workshop(main_window, img_path=None): #vers 1
    """Open COL Workshop - matches TXD Workshop pattern"""
    try:
        workshop = COLWorkshop(main_window, main_window)
        if img_path:
            if img_path.lower().endswith(".col"):
                if hasattr(workshop, "open_col_file"):
                    workshop.open_col_file(img_path)
                elif hasattr(workshop, "load_col_file"):
                    workshop.load_col_file(img_path)
        workshop.setWindowTitle(App_name + " 1.5")
        workshop.show()
        return workshop
    except Exception as e:
        if main_window and hasattr(main_window, "log_message"):
            main_window.log_message(f"Error: {str(e)}")
        return None

IMGEditorDialog = IMGFactoryGUILayout

if __name__ == "__main__":
    import sys
    import traceback

    print(App_name + " Starting.")

    try:
        app = QApplication(sys.argv)
        print("QApplication created")

        workshop = IMGFactoryGUILayout()
        print(App_name + " instance created")

        workshop.setWindowTitle(App_name + " - Standalone")
        workshop.resize(1200, 800)
        workshop.show()
        print("Window shown, entering event loop")
        print(f"Window visible: {workshop.isVisible()}")
        print(f"Window geometry: {workshop.geometry()}")

        sys.exit(app.exec())

    except Exception as e:
        print(f"ERROR: {e}")
        traceback.print_exc()
        sys.exit(1)


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
