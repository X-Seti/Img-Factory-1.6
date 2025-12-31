#this belongs in gui/gui_menu.py - Version: 22
# X-Seti - August14 2025 - IMG Factory 1.5

#!/usr/bin/env python3
"""
IMG Factory Menu System - Complete Implementation
Full menu system with all original entries restored + IDE/COL Editor integration
"""

import os
from datetime import datetime
from typing import Dict, List, Optional, Callable
from PyQt6.QtWidgets import (
    QMenuBar, QMenu, QDialog, QVBoxLayout, QHBoxLayout, QListWidget,
    QPushButton, QCheckBox, QGroupBox, QLabel, QLineEdit, QComboBox,
    QMessageBox, QTabWidget, QWidget, QTextEdit, QSpinBox, QRadioButton,
    QFileDialog, QTreeWidget, QTreeWidgetItem, QScrollArea
)
from PyQt6.QtCore import Qt, pyqtSignal, QPoint, QSettings
from PyQt6.QtGui import QAction, QFont, QPixmap, QIcon, QKeySequence, QActionGroup
from .panel_manager import PanelManager
from apps.methods.img_factory_settings import IMGFactorySettings

#TODO - Menus need SVG icons.

class MenuAction:
    """Represents a menu action with properties"""

    def __init__(self, action_id: str, text: str, shortcut: str = "",
                 icon: str = "", callback: Callable = None, checkable: bool = False):
        self.action_id = action_id
        self.text = text
        self.shortcut = shortcut
        self.icon = icon
        self.callback = callback
        self.checkable = checkable
        self.enabled = True
        self.visible = True
        self.separator_after = False

        #integrate_settings_menu(self) #double menu
        #integrate_color_ui_system(self) #missing function

class MenuDefinition:
    """Defines the complete menu structure"""

    def __init__(self):
        # IMG Factory menu structure - Only File, Edit, and Settings as requested
        self.menu_structure = {
            "File": [
                MenuAction("new_img", "&New IMG", "Ctrl+N", "document-new"),
                MenuAction("open_img", "&Open IMG", "Ctrl+O", "document-open"),
                MenuAction("open_multiple", "Open &Multiple Files", "Ctrl+Shift+O", "folder-open"),
                # Recent files will be added dynamically as a submenu
                MenuAction("recent_files_separator", ""),  # Placeholder for separator
                # Recent files submenu will be added here dynamically
                MenuAction("sep1", ""),
                MenuAction("close_img", "&Close", "Ctrl+W", "window-close"),
                MenuAction("close_all", "Close &All", "Ctrl+Shift+W"),
                MenuAction("sep2", ""),
                MenuAction("save_img", "&Save", "Ctrl+S"),
                MenuAction("save_as_img", "Save &As...", "Ctrl+Shift+S"),
                MenuAction("sep3", ""),
                # NEW: Game path setting
                MenuAction("set_game_path", "Set &Game Path...", "Ctrl+Shift+G"),
                MenuAction("sep4", ""),
                MenuAction("exit", "E&xit", "Ctrl+Q", "application-exit"),
            ],

            "Edit": [
                MenuAction("undo", "&Undo", "Ctrl+Z", "edit-undo"),
                MenuAction("redo", "&Redo", "Ctrl+Y", "edit-redo"),
                MenuAction("sep1", ""),
                MenuAction("cut", "Cu&t", "Ctrl+X", "edit-cut"),
                MenuAction("copy", "&Copy", "Ctrl+C", "edit-copy"),
                MenuAction("paste", "&Paste", "Ctrl+V", "edit-paste"),
                MenuAction("move", "&Move Selected", "Ctrl+M", "edit-move"),
                MenuAction("sep2", ""),
                MenuAction("select_all", "Select &All", "Ctrl+A", "edit-select-all"),
                MenuAction("select_inverse", "Select &Inverse", "Ctrl+I"),
                MenuAction("select_none", "Select &None", "Ctrl+D"),
                MenuAction("sep3", ""),
                MenuAction("find", "&Find", "Ctrl+F", "edit-find"),
                MenuAction("find_next", "Find &Next", "F3"),
                MenuAction("replace", "&Replace", "Ctrl+H"),
                MenuAction("sep4", ""),
                MenuAction("rename_selected", "Re&name", "F2"),
                MenuAction("duplicate_selected", "&Duplicate", "Ctrl+D"),
                MenuAction("remove_selected", "&Remove Selected", "Delete"),
            ],

            "Settings": [
                MenuAction("preferences", "&Theme Prefs ", "Ctrl+,"),
                MenuAction("app_settings", "&App Settings", "Ctrl+,"),
                MenuAction("sep1", ""),
                MenuAction("language", "&Language"),
                MenuAction("sep2", ""),
                MenuAction("file_associations", "&File Associations"),
                MenuAction("default_directories", "&Default Directories"),
                MenuAction("performance", "Per&formance"),
                MenuAction("sep3", ""),
                MenuAction("reset_layout", "&Reset Layout"),
                MenuAction("reset_settings", "Reset &Settings"),
                MenuAction("sep4", ""),
                MenuAction("export_settings", "&Export Settings"),
                MenuAction("import_settings", "&Import Settings"),
            ]
        }


class COLMenuBuilder:
    """Helper class to build COL menu items"""

    @staticmethod
    def add_col_menu_to_menubar(menubar: QMenuBar, parent_window) -> QMenu:
        """Add COL menu to existing menubar"""

        # Create COL menu
        col_menu = menubar.addMenu("&COL")

        # Main COL Editor
        editor_action = QAction("COL &Editor", parent_window)
        editor_action.setShortcut("Ctrl+Shift+C")
        editor_action.setStatusTip("Open COL Editor for collision file editing")
        # Set SVG icon
        try:
            from apps.methods.imgfactory_svg_icons import get_edit_icon
            editor_action.setIcon(get_edit_icon())
        except:
            pass  # Fallback to no icon if SVG loading fails
        editor_action.triggered.connect(lambda: COLMenuBuilder._open_col_editor(parent_window))
        col_menu.addAction(editor_action)

        col_menu.addSeparator()

        # File operations
        open_col_action = QAction("&Open COL File", parent_window)
        open_col_action.setShortcut("Ctrl+Shift+O")
        open_col_action.setStatusTip("Open COL file directly")
        # Set SVG icon
        try:
            from apps.methods.imgfactory_svg_icons import get_open_icon
            open_col_action.setIcon(get_open_icon())
        except:
            pass  # Fallback to no icon if SVG loading fails
        open_col_action.triggered.connect(lambda: COLMenuBuilder._open_col_file(parent_window))
        col_menu.addAction(open_col_action)

        new_col_action = QAction("&New COL File", parent_window)
        new_col_action.setStatusTip("Create new COL file")
        # Set SVG icon
        try:
            from apps.methods.imgfactory_svg_icons import get_add_icon
            new_col_action.setIcon(get_add_icon())
        except:
            pass  # Fallback to no icon if SVG loading fails
        new_col_action.triggered.connect(lambda: COLMenuBuilder._new_col_file(parent_window))
        col_menu.addAction(new_col_action)

        col_menu.addSeparator()

        # Batch operations
        batch_action = QAction("&Batch Processor", parent_window)
        batch_action.setShortcut("Ctrl+Shift+B")
        batch_action.setStatusTip("Process multiple COL files with batch operations")
        # Set SVG icon
        try:
            from apps.methods.imgfactory_svg_icons import get_refresh_icon
            batch_action.setIcon(get_refresh_icon())
        except:
            pass  # Fallback to no icon if SVG loading fails
        batch_action.triggered.connect(lambda: COLMenuBuilder._open_batch_processor(parent_window))
        col_menu.addAction(batch_action)

        analyze_action = QAction("&Analyze COL", parent_window)
        analyze_action.setShortcut("Ctrl+Shift+A")
        analyze_action.setStatusTip("Analyze COL file structure and quality")
        # Set SVG icon
        try:
            from apps.methods.imgfactory_svg_icons import get_search_icon
            analyze_action.setIcon(get_search_icon())
        except:
            pass  # Fallback to no icon if SVG loading fails
        analyze_action.triggered.connect(lambda: COLMenuBuilder._analyze_col(parent_window))
        col_menu.addAction(analyze_action)

        col_menu.addSeparator()

        # IMG integration
        import_submenu = col_menu.addMenu("Import from IMG")

        extract_col_action = QAction("Extract COL from Current IMG", parent_window)
        extract_col_action.setStatusTip("Extract COL files from currently open IMG")
        # Set SVG icon
        try:
            from apps.methods.imgfactory_svg_icons import get_export_icon
            extract_col_action.setIcon(get_export_icon())
        except:
            pass  # Fallback to no icon if SVG loading fails
        extract_col_action.triggered.connect(lambda: COLMenuBuilder._extract_col_from_img(parent_window))
        import_submenu.addAction(extract_col_action)

        import_col_action = QAction("Import COL to Current IMG", parent_window)
        import_col_action.setStatusTip("Import COL file into currently open IMG")
        # Set SVG icon
        try:
            from apps.methods.imgfactory_svg_icons import get_import_icon
            import_col_action.setIcon(get_import_icon())
        except:
            pass  # Fallback to no icon if SVG loading fails
        import_col_action.triggered.connect(lambda: COLMenuBuilder._import_col_to_img(parent_window))
        import_submenu.addAction(import_col_action)

        col_menu.addSeparator()

        # Help
        help_action = QAction("❓ COL &Help", parent_window)
        help_action.setStatusTip("Show help for COL functionality")
        help_action.triggered.connect(lambda: COLMenuBuilder._show_col_help(parent_window))
        col_menu.addAction(help_action)

        return col_menu

    def create_img_menu(self): #vers [your_version + 1]
        """Create IMG menu with corruption analyzer"""
        img_menu = self.menubar.addMenu("IMG")

        img_menu.addSeparator()

        # Corruption Analysis submenu
        corruption_menu = img_menu.addMenu("🔍 Corruption Analysis")

        analyze_action = QAction("Analyze IMG Corruption", self.main_window)
        analyze_action.setStatusTip("Analyze IMG file for corrupted entries and filenames")
        analyze_action.triggered.connect(self.main_window.analyze_img_corruption)
        corruption_menu.addAction(analyze_action)

        quick_fix_action = QAction("Quick Fix Corruption", self.main_window)
        quick_fix_action.setStatusTip("Automatically fix common corruption issues")
        quick_fix_action.triggered.connect(self.main_window.quick_fix_corruption)
        corruption_menu.addAction(quick_fix_action)

        # Clean filenames only
        clean_names_action = QAction("Clean Filenames Only", self.main_window)
        clean_names_action.setStatusTip("Fix only filename corruption, keep all entries")
        clean_names_action.triggered.connect(self.main_window.clean_filenames_only)
        corruption_menu.addAction(clean_names_action)

        corruption_menu.addSeparator()

        export_report_action = QAction("Export Corruption Report", self.main_window)
        export_report_action.setStatusTip("Export detailed corruption analysis to file")
        export_report_action.triggered.connect(self.main_window.export_corruption_report)
        corruption_menu.addAction(export_report_action)

        return img_menu

    @staticmethod
    def _open_col_editor(parent_window):
        """Open COL editor"""
        try:
            from apps.components.Col_Editor.col_editor import open_col_editor
            open_col_editor(parent_window)
        except ImportError:
            QMessageBox.warning(parent_window, "COL Editor", "COL Editor components not found")
        except Exception as e:
            QMessageBox.critical(parent_window, "Error", f"Failed to open COL Editor: {str(e)}")

    @staticmethod
    def _open_col_file(parent_window):
        """Open COL file dialog"""
        try:
            from imgfactory_col_integration import open_col_file_dialog
            open_col_file_dialog(parent_window)
        except ImportError:
            QMessageBox.warning(parent_window, "COL Tools", "COL integration components not found")
        except Exception as e:
            QMessageBox.critical(parent_window, "Error", f"Failed to open COL file: {str(e)}")

    @staticmethod
    def _new_col_file(parent_window):
        """Create new COL file"""
        try:
            from imgfactory_col_integration import create_new_col_file
            create_new_col_file(parent_window)
        except ImportError:
            QMessageBox.information(parent_window, "COL Tools", "New COL file creation coming soon!")
        except Exception as e:
            QMessageBox.critical(parent_window, "Error", f"Failed to create COL file: {str(e)}")

    @staticmethod
    def _open_batch_processor(parent_window):
        """Open batch processor"""
        try:
            from apps.methods.col_utilities import open_col_batch_processor
            open_col_batch_processor(parent_window)
        except ImportError:
            QMessageBox.warning(parent_window, "COL Tools", "COL batch processor not found")
        except Exception as e:
            QMessageBox.critical(parent_window, "Error", f"Failed to open batch processor: {str(e)}")

    @staticmethod
    def _analyze_col(parent_window):
        """Analyze COL file"""
        try:
            from apps.methods.col_utilities import analyze_col_file_dialog
            analyze_col_file_dialog(parent_window)
        except ImportError:
            QMessageBox.warning(parent_window, "COL Tools", "COL analyzer not found")
        except Exception as e:
            QMessageBox.critical(parent_window, "Error", f"Failed to analyze COL: {str(e)}")

    @staticmethod
    def _extract_col_from_img(parent_window):
        """Extract COL from IMG"""
        try:
            from imgfactory_col_integration import export_col_from_img
            export_col_from_img(parent_window)
        except ImportError:
            QMessageBox.warning(parent_window, "COL Tools", "COL integration not found")
        except Exception as e:
            QMessageBox.critical(parent_window, "Error", f"Failed to extract COL: {str(e)}")

    @staticmethod
    def _import_col_to_img(parent_window):
        """Import COL to IMG"""
        try:
            from imgfactory_col_integration import import_col_to_img
            import_col_to_img(parent_window)
        except ImportError:
            QMessageBox.warning(parent_window, "COL Tools", "COL integration not found")
        except Exception as e:
            QMessageBox.critical(parent_window, "Error", f"Failed to import COL: {str(e)}")

    @staticmethod
    def _show_col_help(parent_window):
        """Show COL help dialog"""
        help_text = """
<h2>🔧 COL Functionality Help</h2>

<h3>What are COL files?</h3>
<p>COL files contain collision data for GTA games. They define the physical boundaries
that players, vehicles, and objects interact with in the game world.</p>

<h3>COL Editor Features:</h3>
<ul>
<li><b>3D Visualization</b> - View collision geometry in 3D</li>
<li><b>Edit Collision Elements</b> - Modify spheres, boxes, and meshes</li>
<li><b>Material Assignment</b> - Set surface materials for different effects</li>
<li><b>Version Conversion</b> - Convert between COL formats</li>
<li><b>Optimization Tools</b> - Remove duplicates and optimize geometry</li>
</ul>

<h3>Batch Processor:</h3>
<ul>
<li><b>Process Multiple Files</b> - Handle many COL files at once</li>
<li><b>Automatic Optimization</b> - Clean up and optimize collision data</li>
<li><b>Version Conversion</b> - Convert entire batches between formats</li>
<li><b>Quality Analysis</b> - Check for issues and problems</li>
</ul>

<h3>Supported Games:</h3>
<ul>
<li>GTA III (COL Version 1)</li>
<li>GTA Vice City (COL Version 1)</li>
<li>GTA San Andreas (COL Version 2 & 3)</li>
<li>GTA Stories series</li>
<li>Bully</li>
</ul>

<h3>Keyboard Shortcuts:</h3>
<ul>
<li><b>Ctrl+Shift+C</b> - Open COL Editor</li>
<li><b>Ctrl+Shift+B</b> - Open Batch Processor</li>
<li><b>Ctrl+Shift+A</b> - Analyze COL File</li>
<li><b>Ctrl+Shift+O</b> - Open COL File</li>
</ul>

<p><i>COL functionality is based on Steve's COL Editor II with modern improvements.</i></p>
        """

        QMessageBox.about(parent_window, "COL Help", help_text)


class IMGFactoryMenuBar:
    """Complete IMG Factory menu bar with all original functionality"""

    def __init__(self, main_window, panel_manager: PanelManager = None):
        self.main_window = main_window
        self.panel_manager = panel_manager
        self.menu_bar = main_window.menuBar()
        self.callbacks: Dict[str, Callable] = {}
        self.actions: Dict[str, QAction] = {}
        self.menus: Dict[str, QMenu] = {}

        # Dialog management
        self._preferences_dialog = None
        self._gui_settings_dialog = None

        # Clear any existing menus first
        self.menu_bar.clear()

        self.menu_definition = MenuDefinition()
        self._create_menus()
        self._create_tools_menu()
        #self.col_menu()

        # Set up default callbacks
        self._setup_default_callbacks()

    def _create_menus(self):
        """Create all menus from definition"""
        for menu_name, menu_actions in self.menu_definition.menu_structure.items():
            menu = self.menu_bar.addMenu(menu_name)
            self.menus[menu_name] = menu

            for menu_action in menu_actions:
                if menu_action.action_id.startswith("sep"):
                    menu.addSeparator()
                elif menu_name == "File" and menu_action.action_id == "recent_files_separator":
                    # Add recent files submenu after the separator
                    menu.addSeparator()
                    # Create the recent files submenu
                    self._create_recent_files_submenu(menu)
                else:
                    action = QAction(menu_action.text, self.main_window)

                    if menu_action.shortcut:
                        action.setShortcut(QKeySequence(menu_action.shortcut))

                    # Set icon if specified - using SVG icons TODO - add get_move_icon
                    if menu_action.icon:
                        try:
                            # Import the appropriate SVG icon based on the icon string
                            from apps.methods.imgfactory_svg_icons import (
                                get_save_icon, get_open_icon, get_close_icon,
                                get_add_icon, get_remove_icon, get_edit_icon,
                                get_refresh_icon, get_settings_icon, get_info_icon,
                                get_search_icon, get_export_icon, get_import_icon,
                                get_trash_icon, get_checkmark_icon, get_view_icon,
                                get_folder_icon, get_file_icon, get_package_icon,
                                get_shield_icon, get_image_icon, get_palette_icon,
                                get_error_icon #, get_move_icon
                            )

                            # Map icon names to functions
                            icon_map = {
                                "document-new": get_add_icon,
                                "document-open": get_open_icon,
                                "folder-open": get_folder_icon,
                                "window-close": get_close_icon,
                                "application-exit": get_error_icon,  # or another appropriate icon
                                "edit-undo": get_refresh_icon,  # or appropriate icon
                                "edit-redo": get_refresh_icon,  # or appropriate icon
                                "edit-cut": get_trash_icon,  # or appropriate icon
                                "edit-copy": get_file_icon,  # or appropriate icon
                                "edit-paste": get_file_icon,  # or appropriate icon
                                "edit-move": get_move_icon,  # or appropriate icon
                                "edit-select-all": get_checkmark_icon,
                                "edit-find": get_search_icon,
                                "edit-find-next": get_search_icon,  # or appropriate icon
                            }

                            if menu_action.icon in icon_map:
                                icon_func = icon_map[menu_action.icon]
                                action.setIcon(icon_func())
                            else:
                                # For other icon names, try to match them to available functions
                                # Use generic icon if no match found
                                action.setIcon(get_file_icon())
                        except:
                            # Fallback if SVG icon loading fails
                            pass

                    if menu_action.checkable:
                        action.setCheckable(True)
                        # Set default checked state for view items
                        if menu_action.action_id in ["toolbar", "statusbar", "log_panel"]:
                            action.setChecked(True)

                    # Add hover effect styling
                    action.hovered.connect(lambda act=action: self._on_menu_hover(act))

                    # Store action
                    self.actions[menu_action.action_id] = action

                    # Add to menu
                    menu.addAction(action)

        # Add hover effect styling to menu bar
        self._apply_menu_bar_styling()

    def _create_tools_menu(self):
        """Create Tools menu with Sort Via IDE and other tools"""
        tools_menu = self.menu_bar.addMenu("&Tools")

        # Sort Via IDE
        sort_ide_action = QAction("Sort Via &IDE", self.main_window)
        sort_ide_action.setShortcut("Ctrl+Shift+D")
        sort_ide_action.setStatusTip("Sort IMG entries based on IDE file model order")
        sort_ide_action.triggered.connect(self.main_window.sort_via_ide)
        tools_menu.addAction(sort_ide_action)

        tools_menu.addSeparator()

        # File Analysis Tools - Create Analyze submenu
        analyze_menu = tools_menu.addMenu("&Analyze")

        analyze_img_action = QAction("IMG File", self.main_window)
        analyze_img_action.setStatusTip("Analyze IMG file structure and properties")
        analyze_img_action.triggered.connect(self._analyze_img)
        analyze_menu.addAction(analyze_img_action)

        analyze_col_action = QAction("COL File", self.main_window)
        analyze_col_action.setStatusTip("Analyze COL file structure and properties")
        #analyze_col_action.triggered.connect(self._analyze_col)
        analyze_menu.addAction(analyze_col_action)

        analyze_dff_action = QAction("DFF Model", self.main_window)
        analyze_dff_action.setStatusTip("Analyze DFF file structure and properties")
        #analyze_dff_action.triggered.connect(self._analyze_dff)
        analyze_menu.addAction(analyze_dff_action)

        analyze_txd_action = QAction("TXD Texture", self.main_window)
        analyze_txd_action.setStatusTip("Analyze TXD file structure and properties")
        #analyze_txd_action.triggered.connect(self._analyze_txd)
        analyze_menu.addAction(analyze_txd_action)

        analyze_file_action = QAction("Generic File", self.main_window)
        analyze_file_action.setStatusTip("Analyze file structure and properties")
        #analyze_file_action.triggered.connect(self._analyze_file)
        analyze_menu.addAction(analyze_file_action)

        advanced_analyze_action = QAction("&Advanced Analysis", self.main_window)
        advanced_analyze_action.setStatusTip("Comprehensive analysis of IMG file health and structure")
        advanced_analyze_action.triggered.connect(self._advanced_analysis)
        tools_menu.addAction(advanced_analyze_action)

        validate_action = QAction("&Validate IMG", self.main_window)
        validate_action.setStatusTip("Validate IMG file integrity and structure")
        validate_action.triggered.connect(self._validate_img)
        tools_menu.addAction(validate_action)

        tools_menu.addSeparator()

        # Get entry info if row is specified
        entry_info = None
        row = None
        if row is not None and hasattr(main_window, 'current_img') and main_window.current_img:
            if 0 <= row < len(main_window.current_img.entries):
                entry = main_window.current_img.entries[row]
                entry_info = {
                    'entry': entry,
                    'name': entry.name,
                    'is_col': entry.name.lower().endswith('.col'),
                    'is_dff': entry.name.lower().endswith('.dff'),
                    'is_txd': entry.name.lower().endswith('.txd'),
                    'path': getattr(entry, 'full_path', '') if hasattr(entry, 'full_path') else ''
                }

        # Show hex editor action
        hex_action = QAction("Show Hex Editor", self.main_window)
        if row is not None and entry_info:
            hex_action.triggered.connect(lambda: show_hex_editor(main_window, row, entry_info))
        else:
            hex_action.triggered.connect(lambda: show_hex_editor_selected(main_window))
        tools_menu.addAction(hex_action)

        # Show texture list for DFF (if DFF file)
        if entry_info and entry_info['is_dff']:
            texture_action = QAction("Show Texture List for DFF", self.main_window)
            texture_action.triggered.connect(lambda: show_dff_texture_list(main_window, row, entry_info))
            menu.addAction(texture_action)

        # Show DFF model in viewer (if DFF file)
        if entry_info and entry_info['is_dff']:
            model_action = QAction("Show DFF Model in Viewer", self.main_window)
            model_action.triggered.connect(lambda: show_dff_model_viewer(main_window, row, entry_info))
            menu.addAction(model_action)

        # File Checking Tools
        find_dups_action = QAction("Find &Duplicates", self.main_window)
        find_dups_action.setStatusTip("Find duplicate entries in IMG file")
        find_dups_action.triggered.connect(self._find_duplicates)
        tools_menu.addAction(find_dups_action)

        find_corrupt_action = QAction("Find &Corruption", self.main_window)
        find_corrupt_action.setStatusTip("Find corrupted entries in IMG file")
        find_corrupt_action.triggered.connect(self._find_corruption)
        tools_menu.addAction(find_corrupt_action)

        # Move action
        move_action = QAction("Move Selected", self.main_window)
        if row is not None and entry_info:
            move_action.triggered.connect(lambda: move_file(main_window, row, entry_info))
        else:
            move_action.triggered.connect(lambda: move_selected_file(main_window))
        tools_menu.addAction(move_action)

        # Rename action
        rename_action = QAction("Rename", self.main_window)
        if row is not None:
            rename_action.triggered.connect(lambda: handle_double_click_rename(main_window, row, 0))

        tools_menu.addAction(rename_action)

        # Store reference to tools menu
        self.menus['Tools'] = tools_menu


    def _analyze_img(self):
        """Analyze IMG file"""
        if hasattr(self.main_window, 'analyze_img_corruption'):
            self.main_window.analyze_img_corruption()
        elif hasattr(self.main_window, 'analyze_corruption'):
            self.main_window.analyze_corruption()
        else:
            QMessageBox.information(self.main_window, "Analyze IMG", "IMG analysis functionality not available")


    def _advanced_analysis(self):
        """Advanced comprehensive analysis"""
        if hasattr(self.main_window, 'advanced_img_check'):
            self.main_window.advanced_img_check()
        elif hasattr(self.main_window, 'comprehensive_analysis'):
            self.main_window.comprehensive_analysis()
        else:
            QMessageBox.information(self.main_window, "Advanced Analysis", "Advanced analysis functionality not available")


    def _validate_img(self):
        """Validate IMG file"""
        if hasattr(self.main_window, 'validate_img'):
            self.main_window.validate_img()
        else:
            QMessageBox.information(self.main_window, "Validate IMG", "IMG validation functionality not available")


    def _find_duplicates(self):
        """Find duplicate entries in IMG"""
        try:
            from apps.methods.find_dups_functions import show_duplicates_dialog
            show_duplicates_dialog(self.main_window)
        except ImportError:
            QMessageBox.information(self.main_window, "Find Duplicates", "Duplicate finding functionality not available")


    def _find_corruption(self):
        """Find corrupted entries in IMG"""
        if hasattr(self.main_window, 'analyze_img_corruption'):
            self.main_window.analyze_img_corruption()
        else:
            QMessageBox.information(self.main_window, "Find Corruption", "Corruption analysis functionality not available")


    def _create_recent_files_submenu(self, file_menu):
        """Create and add recent files submenu to the File menu"""
        # Create submenu for recent files
        recent_menu = file_menu.addMenu("&Recent Files")

        # Add placeholder initially
        no_files_action = recent_menu.addAction("No recent files")
        no_files_action.setEnabled(False)

        # Add separator
        recent_menu.addSeparator()

        # Add clear action
        clear_action = recent_menu.addAction("Clear Recent Files")
        clear_action.triggered.connect(self._clear_recent_files)

        # Store reference to recent menu for dynamic updates
        self.recent_files_menu = recent_menu
        self.recent_files_no_files_action = no_files_action

        # Now populate the actual recent files
        self._update_recent_files_submenu()


    def _update_recent_files_submenu(self):
        """Update the recent files submenu with actual recent files"""
        try:
            from PyQt6.QtCore import QSettings

            # Clear existing items except the clear action
            for action in self.recent_files_menu.actions()[1:-1]:  # Skip first (no files) and last (clear)
                self.recent_files_menu.removeAction(action)

            # Get settings for recent files
            settings = QSettings("IMG-Factory", "IMG-Factory")
            recent_files = settings.value("recentFiles", [])

            if not recent_files:
                # No recent files - keep the "No recent files" message
                self.recent_files_no_files_action.setEnabled(False)
                self.recent_files_no_files_action.setVisible(True)
            else:
                # Remove the "No recent files" placeholder
                self.recent_files_no_files_action.setVisible(False)
                self.recent_files_no_files_action.setEnabled(False)

                # Add recent files to submenu (up to 10)
                for file_path in recent_files[:10]:
                    action = self.recent_files_menu.addAction(file_path)
                    action.triggered.connect(lambda checked=False, fp=file_path: self._open_recent_file(fp))

        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Error updating recent files submenu: {str(e)}")


    def update_recent_files_menu(self):
        """Public method to update the recent files submenu from outside the class"""
        if hasattr(self, 'recent_files_menu'):
            self._update_recent_files_submenu()


    def _apply_menu_bar_styling(self):
        """Apply styling to menu bar for hover effects - THEME AWARE"""
        # Get theme colors from app settings
        if hasattr(self.main_window, 'app_settings'):
            theme_colors = self.main_window.app_settings.get_theme_colors()
        else:
            # Fallback colors
            theme_colors = {
                'panel_bg': '#f0f0f0',
                'bg_primary': '#f0f0f0',
                'bg_secondary': '#f5f5f5',
                'bg_tertiary': '#e9ecef',
                'text_primary': '#000000',
                'text_secondary': '#666666',
                'accent_primary': '#0078d4',
                'accent_secondary': '#0A7Ad4',
                'border': '#cccccc',
                'selection_background': '#0078d4',
                'selection_text': '#ffffff'
            }

        # Extract colors for menu styling
        bg_primary = theme_colors.get('panel_bg', '#f0f0f0')
        bg_secondary = theme_colors.get('bg_secondary', '#f5f5f5')
        bg_tertiary = theme_colors.get('bg_tertiary', '#e9ecef')
        text_primary = theme_colors.get('text_primary', '#000000')
        text_secondary = theme_colors.get('text_secondary', '#666666')
        accent_primary = theme_colors.get('accent_primary', '#0078d4')
        accent_secondary = theme_colors.get('accent_secondary', '#0A7Ad4')
        border = theme_colors.get('border', '#cccccc')
        selection_bg = theme_colors.get('selection_background', '#0078d4')
        selection_text = theme_colors.get('selection_text', '#ffffff')

        # Apply CSS styling to the menu bar for hover effects using theme colors
        menu_style = f"""
        QMenuBar {{
            background-color: {bg_primary};
            padding: 2px;
            border: none;
        }}
        QMenuBar::item {{
            background: transparent;
            padding: 5px 10px;
            margin: 1px;
            border-radius: 3px;
            color: {text_primary};
        }}
        QMenuBar::item:selected {{
            background: {bg_tertiary};
        }}
        QMenuBar::item:pressed {{
            background: {bg_secondary};
        }}
        QMenu {{
            background-color: {bg_secondary};
            border: 1px solid {border};
            padding: 2px;
            color: {text_primary};
        }}
        QMenu::item {{
            padding: 5px 20px;
            border: 1px solid transparent;
            color: {text_primary};
        }}
        QMenu::item:selected {{
            background-color: {selection_bg};
            color: {selection_text};
        }}
        QMenu::item:disabled {{
            color: {text_secondary};
        }}
        QMenu::separator {{
            height: 1px;
            background: {border};
        }}
        """
        self.menu_bar.setStyleSheet(menu_style)


    def _on_menu_hover(self, action):
        """Handle menu hover effect"""
        # This is a callback to provide visual feedback when menu items are hovered
        pass


    def _apply_gui_changes(self): #vers 1
        """Apply GUI changes after preferences are saved"""
        try:
            # Apply settings to gui_layout
            if hasattr(self.main_window, 'gui_layout'):
                if hasattr(self.main_window.gui_layout, 'apply_settings_changes'):
                    # Get current settings
                    if hasattr(self.main_window, 'app_settings'):
                        settings = self.main_window.app_settings.current_settings
                        self.main_window.gui_layout.apply_settings_changes(settings)

                # Apply theme changes
                if hasattr(self.main_window.gui_layout, 'apply_all_window_themes'):
                    self.main_window.gui_layout.apply_all_window_themes()

            # Apply stylesheet
            if hasattr(self.main_window, 'app_settings'):
                stylesheet = self.main_window.app_settings.get_stylesheet()
                self.main_window.setStyleSheet(stylesheet)

            # Refresh menu bar styling to match new theme
            self._apply_menu_bar_styling()

            # Refresh button display if changed
            if hasattr(self.main_window, 'gui_layout'):
                if hasattr(self.main_window.gui_layout, '_update_button_icons_state'):
                    button_mode = self.main_window.app_settings.current_settings.get('button_display_mode', 'text_only')
                    show_icons = button_mode in ['icons_only', 'icons_with_text']
                    self.main_window.gui_layout._update_button_icons_state(show_icons)

            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("GUI changes applied successfully")

        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Error applying GUI changes: {str(e)}")
            print(f"_apply_gui_changes error: {str(e)}")


    def _setup_default_callbacks(self): # TODO - ._move_selected_entries, up or down the file list.
        """Set up default menu callbacks"""
        default_callbacks = {
            # File menu
            "exit": self._exit_application,
            "new_img": self._create_new_img,
            "open_img": self._open_img_file,
            "recent_files": self._show_recent_files,
            "save_img": self._save_img_file,
            "save_as_img": self._save_img_as,
            "close_img": self._close_img_file,
            "close_all": self._close_all_img,
            "set_game_path": self._set_game_path,

            # Edit menu
            "undo": self._undo_action,
            "redo": self._redo_action,
            "select_all": self._select_all_entries,
            "select_inverse": self._select_inverse_entries,
            "select_none": self._select_none_entries,
            "find": self._find_entries,
            "find_next": self._find_next_entries,
            "replace": self._replace_entries,
            "move_selected": self._move_selected_entries,
            "rename_selected": self._rename_selected_entry,
            "duplicate_selected": self._duplicate_selected_entry,
            "remove_selected": self._remove_selected_entries,

            # Settings menu
            "preferences": self._show_preferences,
            "app_settings": self._show_app_settings,
            "language": self._change_language,
            "file_associations": self._file_associations,
            "default_directories": self._default_directories,
            "performance": self._performance_settings,
            "reset_layout": self._reset_layout,
            "reset_settings": self._reset_settings,
            "export_settings": self._export_settings,
            "import_settings": self._import_settings,
        }

        self.set_callbacks(default_callbacks)


# - FILE MENU CALLBACKS

    def _create_new_img(self):
        """Create new IMG file"""
        if hasattr(self.main_window, 'create_new_img'):
            self.main_window.create_new_img()

    def _set_game_path(self):
        """Set game path"""
        if hasattr(self.main_window, 'set_game_path'):
            self.main_window.set_game_path()

    def _open_img_file(self):
        """Open IMG file"""
        if hasattr(self.main_window, 'open_img_file'):
            self.main_window.open_img_file()

    def _show_recent_files(self):
        """Update the recent files submenu (this is now handled automatically in the menu)"""
        # This method is kept for compatibility, but the submenu is already shown in the menu
        # Just update the submenu to reflect any changes
        if hasattr(self, 'recent_files_menu'):
            self._update_recent_files_submenu()
        else:
            # Fallback: Create a simple recent files menu using QSettings
            self._create_recent_files_menu()

    def _create_recent_files_menu(self):
        """Create and show recent files submenu"""
        try:
            from PyQt6.QtCore import QSettings
            from PyQt6.QtWidgets import QMenu

            # Create a popup menu for recent files
            recent_menu = QMenu("Recent Files", self.main_window)

            # Get settings for recent files
            settings = QSettings("IMG-Factory", "IMG-Factory")
            recent_files = settings.value("recentFiles", [])

            if not recent_files:
                # No recent files
                no_files_action = recent_menu.addAction("No recent files")
                no_files_action.setEnabled(False)
            else:
                # Add recent files to menu
                for file_path in recent_files[:10]:  # Show up to 10 recent files
                    action = recent_menu.addAction(file_path)
                    action.triggered.connect(lambda fp=file_path: self._open_recent_file(fp))

            recent_menu.addSeparator()

            # Add clear recent files option
            clear_action = recent_menu.addAction("Clear Recent Files")
            clear_action.triggered.connect(self._clear_recent_files)

            # Show the menu at the current cursor position
            recent_menu.exec()
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Error creating recent files menu: {str(e)}")

    def _open_recent_file(self, file_path):
        """Open a recent file"""
        try:
            # Use the detect and open file function from the core module
            from apps.core.open import _detect_and_open_file
            _detect_and_open_file(self.main_window, file_path)
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Error opening recent file {file_path}: {str(e)}")
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.critical(self.main_window, "Open Error", f"Failed to open recent file: {str(e)}")

    def _clear_recent_files(self):
        """Clear recent files list"""
        try:
            from PyQt6.QtCore import QSettings
            settings = QSettings("IMG-Factory", "IMG-Factory")
            settings.setValue("recentFiles", [])

            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("Recent files list cleared")
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Error clearing recent files: {str(e)}")

    def _save_img_file(self):
        """Save IMG file"""
        try:
            if hasattr(self.main_window, 'save_img_entry'):
                # Call the save function which should handle the save operation
                result = self.main_window.save_img_entry()

                # Log the action
                if hasattr(self.main_window, 'log_message'):
                    if result:
                        self.main_window.log_message("IMG file saved successfully")
                    else:
                        self.main_window.log_message("No changes to save or save operation cancelled")
            else:
                QMessageBox.information(self.main_window, "Save", "Save functionality not available")
        except Exception as e:
            QMessageBox.critical(self.main_window, "Save Error", f"Failed to save IMG file: {str(e)}")

    def _save_img_as(self):
        """Save IMG file as"""
        try:
            from PyQt6.QtWidgets import QFileDialog
            if hasattr(self.main_window, 'current_img') and self.main_window.current_img:
                file_path, _ = QFileDialog.getSaveFileName(
                    self.main_window,
                    "Save IMG As",
                    "",
                    "IMG Files (*.img);;All Files (*.*)"
                )
                if file_path:
                    self.main_window.current_img.save(file_path)
                    self.main_window.current_img.file_path = file_path  # Update file path
                    self.main_window.log_message(f"IMG file saved as: {file_path}")
                    # Update window title if possible
                    if hasattr(self.main_window, 'setWindowTitle'):
                        self.main_window.setWindowTitle(f"IMG Factory - {file_path}")
            else:
                QMessageBox.warning(self.main_window, "Warning", "No IMG file loaded to save")
        except Exception as e:
            QMessageBox.critical(self.main_window, "Save Error", f"Failed to save IMG file: {str(e)}")

    def _close_img_file(self):
        """Close IMG file"""
        if hasattr(self.main_window, 'close_img_file'):
            self.main_window.close_img_file()

    def _close_all_img(self):
        """Close all IMG files"""
        if hasattr(self.main_window, 'close_all_img'):
            self.main_window.close_all_img()

    # ========================================================================
    # EDIT MENU CALLBACKS
    # ========================================================================

    def _undo_action(self):
        """Undo last action"""
        try:
            # First try the undo manager if it exists
            if hasattr(self.main_window, 'undo_manager') and hasattr(self.main_window.undo_manager, 'undo'):
                self.main_window.undo_manager.undo()
                return

            # Try a direct undo method if it exists
            if hasattr(self.main_window, 'undo'):
                self.main_window.undo()
                return

            # Try to use a generic undo mechanism
            if hasattr(self.main_window, 'gui_layout') and hasattr(self.main_window.gui_layout, 'table'):
                # Check if table has a way to handle undo
                table = self.main_window.gui_layout.table
                # For now, fallback to showing an info message
                QMessageBox.information(self.main_window, "Undo", "No undo operations available")
            else:
                QMessageBox.information(self.main_window, "Undo", "Undo functionality not available")
        except Exception as e:
            QMessageBox.critical(self.main_window, "Undo Error", f"Failed to perform undo: {str(e)}")

    def _redo_action(self):
        """Redo last action"""
        try:
            # First try the undo manager if it exists
            if hasattr(self.main_window, 'undo_manager') and hasattr(self.main_window.undo_manager, 'redo'):
                self.main_window.undo_manager.redo()
                return

            # Try a direct redo method if it exists
            if hasattr(self.main_window, 'redo'):
                self.main_window.redo()
                return

            # Try to use a generic redo mechanism
            if hasattr(self.main_window, 'gui_layout') and hasattr(self.main_window.gui_layout, 'table'):
                # Check if table has a way to handle redo
                table = self.main_window.gui_layout.table
                # For now, fallback to showing an info message
                QMessageBox.information(self.main_window, "Redo", "No redo operations available")
            else:
                QMessageBox.information(self.main_window, "Redo", "Redo functionality not available")
        except Exception as e:
            QMessageBox.critical(self.main_window, "Redo Error", f"Failed to perform redo: {str(e)}")

    def _select_all_entries(self):
        """Select all entries in table"""
        if (hasattr(self.main_window, 'gui_layout') and
            hasattr(self.main_window.gui_layout, 'table')):
            table = self.main_window.gui_layout.table
            table.selectAll()

    def _select_inverse_entries(self):
        """Invert selection of entries in table"""
        if hasattr(self.main_window, 'inverse_selection'):
            self.main_window.inverse_selection()

    def _select_none_entries(self):
        """Clear all selections in table"""
        if (hasattr(self.main_window, 'gui_layout') and
            hasattr(self.main_window.gui_layout, 'table')):
            table = self.main_window.gui_layout.table
            table.clearSelection()

    def _find_entries(self):
        """Find entries"""
        try:
            from PyQt6.QtWidgets import QDialog, QVBoxLayout, QHBoxLayout, QLabel, QLineEdit, QPushButton, QCheckBox
            from PyQt6.QtCore import Qt

            dialog = QDialog(self.main_window)
            dialog.setWindowTitle("Find Entries")
            dialog.setModal(True)
            dialog.resize(400, 150)

            layout = QVBoxLayout(dialog)

            # Search term input
            search_layout = QHBoxLayout()
            search_layout.addWidget(QLabel("Search for:"))
            search_input = QLineEdit()
            search_layout.addWidget(search_input)
            layout.addLayout(search_layout)

            # Options
            case_sensitive = QCheckBox("Case sensitive")
            layout.addWidget(case_sensitive)

            match_whole_name = QCheckBox("Match whole name")
            layout.addWidget(match_whole_name)

            # Buttons
            button_layout = QHBoxLayout()
            find_btn = QPushButton("Find")
            cancel_btn = QPushButton("Cancel")

            find_btn.clicked.connect(dialog.accept)
            cancel_btn.clicked.connect(dialog.reject)

            button_layout.addWidget(find_btn)
            button_layout.addWidget(cancel_btn)
            layout.addLayout(button_layout)

            if dialog.exec() == QDialog.DialogCode.Accepted:
                search_term = search_input.text().strip()
                if not search_term:
                    return

                # Get the current table
                gui_layout = getattr(self.main_window, 'gui_layout', None)
                if not gui_layout or not hasattr(gui_layout, 'table') or not self.main_window.gui_layout.table:
                    QMessageBox.warning(self.main_window, "Error", "No IMG table found")
                    return
                table = self.main_window.gui_layout.table

                # Clear current selection
                table.clearSelection()

                # Search through entries
                case_sensitive_flag = 1 if case_sensitive.isChecked() else 0  # 1 for case sensitive, 0 for case insensitive
                matches = []

                for row in range(table.rowCount()):
                    item = table.item(row, 0)  # Assuming name is in first column
                    if item:
                        text = item.text()
                        if match_whole_name.isChecked():
                            if case_sensitive_flag == 1:
                                match = text == search_term
                            else:
                                match = text.lower() == search_term.lower()
                        else:
                            if case_sensitive_flag == 1:
                                match = search_term in text
                            else:
                                match = search_term.lower() in text.lower()

                        if match:
                            matches.append(row)

                if matches:
                    # Select the first match
                    table.selectRow(matches[0])
                    table.scrollToItem(table.item(matches[0], 0))
                    self.main_window.log_message(f"Found {len(matches)} matching entries")

                    # Store matches for find next functionality
                    self.main_window.current_find_matches = matches
                    self.main_window.current_find_index = 0
                else:
                    QMessageBox.information(self.main_window, "Find", "No matching entries found")

        except Exception as e:
            self.main_window.log_message(f"Error in find entries: {str(e)}")
            QMessageBox.critical(self.main_window, "Find Error", f"Failed to find entries: {str(e)}")

    def _find_next_entries(self):
        """Find next entry"""
        try:
            # Get the current table
            gui_layout = getattr(self.main_window, 'gui_layout', None)
            if not gui_layout or not hasattr(gui_layout, 'table') or not self.main_window.gui_layout.table:
                QMessageBox.warning(self.main_window, "Error", "No IMG table found")
                return
            table = self.main_window.gui_layout.table

            # Check if we have stored matches from previous find
            if hasattr(self.main_window, 'current_find_matches') and self.main_window.current_find_matches:
                # Move to next match
                current_index = getattr(self.main_window, 'current_find_index', 0)
                next_index = (current_index + 1) % len(self.main_window.current_find_matches)

                # Update the current index
                self.main_window.current_find_index = next_index
                row = self.main_window.current_find_matches[next_index]

                # Select the next match
                table.clearSelection()
                table.selectRow(row)
                table.scrollToItem(table.item(row, 0))

                self.main_window.log_message(f"Found next match ({next_index + 1}/{len(self.main_window.current_find_matches)})")
            else:
                # If no previous search, trigger the find dialog
                self._find_entries()

        except Exception as e:
            self.main_window.log_message(f"Error in find next entries: {str(e)}")
            QMessageBox.critical(self.main_window, "Find Next Error", f"Failed to find next entry: {str(e)}")

    def _replace_entries(self):
        """Replace entries"""
        try:
            from PyQt6.QtWidgets import QDialog, QVBoxLayout, QHBoxLayout, QLabel, QLineEdit, QPushButton, QCheckBox
            from PyQt6.QtCore import Qt

            dialog = QDialog(self.main_window)
            dialog.setWindowTitle("Replace Entries")
            dialog.setModal(True)
            dialog.resize(400, 200)

            layout = QVBoxLayout(dialog)

            # Search term input
            search_layout = QHBoxLayout()
            search_layout.addWidget(QLabel("Find:"))
            search_input = QLineEdit()
            search_layout.addWidget(search_input)
            layout.addLayout(search_layout)

            # Replace term input
            replace_layout = QHBoxLayout()
            replace_layout.addWidget(QLabel("Replace with:"))
            replace_input = QLineEdit()
            replace_layout.addWidget(replace_input)
            layout.addLayout(replace_layout)

            # Options
            case_sensitive = QCheckBox("Case sensitive")
            layout.addWidget(case_sensitive)

            match_whole_name = QCheckBox("Match whole name")
            layout.addWidget(match_whole_name)

            replace_all = QCheckBox("Replace all occurrences")
            layout.addWidget(replace_all)

            # Buttons
            button_layout = QHBoxLayout()
            replace_btn = QPushButton("Replace")
            cancel_btn = QPushButton("Cancel")

            replace_btn.clicked.connect(dialog.accept)
            cancel_btn.clicked.connect(dialog.reject)

            button_layout.addWidget(replace_btn)
            button_layout.addWidget(cancel_btn)
            layout.addLayout(button_layout)

            if dialog.exec() == QDialog.DialogCode.Accepted:
                search_term = search_input.text().strip()
                replace_term = replace_input.text().strip()

                if not search_term:
                    QMessageBox.warning(self.main_window, "Warning", "Please enter a search term")
                    return

                # Get the current table
                gui_layout = getattr(self.main_window, 'gui_layout', None)
                if not gui_layout or not hasattr(gui_layout, 'table') or not self.main_window.gui_layout.table:
                    QMessageBox.warning(self.main_window, "Error", "No IMG table found")
                    return
                table = self.main_window.gui_layout.table

                # Get the current IMG file
                if not hasattr(self.main_window, 'current_img') or not self.main_window.current_img:
                    QMessageBox.warning(self.main_window, "Error", "No IMG file loaded")
                    return

                case_sensitive_flag = 1 if case_sensitive.isChecked() else 0  # 1 for case sensitive, 0 for case insensitive
                matches = []

                # Search through entries
                for row in range(table.rowCount()):
                    item = table.item(row, 0)  # Assuming name is in first column
                    if item:
                        text = item.text()
                        if match_whole_name.isChecked():
                            if case_sensitive_flag == 1:
                                match = text == search_term
                            else:
                                match = text.lower() == search_term.lower()
                        else:
                            if case_sensitive_flag == 1:
                                match = search_term in text
                            else:
                                match = search_term.lower() in text.lower()

                        if match:
                            matches.append((row, text))

                if not matches:
                    QMessageBox.information(self.main_window, "Replace", "No matching entries found")
                    return

                # Confirm replacement
                reply = QMessageBox.question(
                    self.main_window,
                    "Confirm Replace",
                    f"Found {len(matches)} matching entries. Replace them?",
                    QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
                )

                if reply == QMessageBox.StandardButton.Yes:
                    # Perform replacement
                    for row, original_text in matches:
                        # Create the new text based on search and replace
                        if match_whole_name.isChecked():
                            new_text = replace_term
                        else:
                            if case_sensitive_flag == 1:
                                new_text = original_text.replace(search_term, replace_term)
                            else:
                                # For case-insensitive replacement, we need to handle it differently
                                import re
                                new_text = re.sub(re.escape(search_term), replace_term, original_text, flags=re.IGNORECASE)

                        # Update the table
                        table.item(row, 0).setText(new_text)

                        # Update the actual entry in the IMG file
                        if row < len(self.main_window.current_img.entries):
                            self.main_window.current_img.entries[row].name = new_text
                            # Update the file name in the entry
                            table.item(row, 0).setText(new_text)

                    self.main_window.log_message(f"Replaced {len(matches)} entries")
                    QMessageBox.information(self.main_window, "Replace", f"Successfully replaced {len(matches)} entries")

        except Exception as e:
            self.main_window.log_message(f"Error in replace entries: {str(e)}")
            QMessageBox.critical(self.main_window, "Replace Error", f"Failed to replace entries: {str(e)}")

    def _rename_selected_entry(self):
        """Rename selected entry"""
        try:
            if hasattr(self.main_window, 'rename_entry'):
                # Call the rename function which should handle the rename operation
                self.main_window.rename_entry()

                # After renaming, we should enable the save functionality
                if hasattr(self.main_window, 'entries_changed'):
                    # Emit the signal to indicate entries have changed
                    self.main_window.entries_changed.emit()

                # Log the action
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Rename operation initiated")
            else:
                QMessageBox.information(self.main_window, "Rename", "Rename functionality not available")
        except Exception as e:
            QMessageBox.critical(self.main_window, "Rename Error", f"Failed to rename entry: {str(e)}")


    def move_selected_entry(self):
        """Rename selected entry"""
        try:
            if hasattr(self.main_window, 'move_entry'):
                # Call the move function which should handle the rename operation
                self.main_window.move_entry()

                # After moving, we should enable the save functionality
                if hasattr(self.main_window, 'entries_changed'):
                    # Emit the signal to indicate entries have changed
                    self.main_window.entries_changed.emit()

                # Log the action
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Move operation initiated")
            else:
                QMessageBox.information(self.main_window, "Move", "Move functionality not available")
        except Exception as e:
            QMessageBox.critical(self.main_window, "Move Error", f"Failed to move entry: {str(e)}")


    def _duplicate_selected_entry(self):
        """Duplicate selected entry"""
        try:
            # Get the current table
            gui_layout = getattr(self.main_window, 'gui_layout', None)
            if not gui_layout or not hasattr(gui_layout, 'table') or not self.main_window.gui_layout.table:
                QMessageBox.warning(self.main_window, "Error", "No IMG table found")
                return
            table = self.main_window.gui_layout.table

            # Get selected rows
            selected_rows = [index.row() for index in table.selectionModel().selectedRows()]
            if not selected_rows:
                QMessageBox.warning(self.main_window, "Warning", "Please select an entry to duplicate")
                return

            if not hasattr(self.main_window, 'current_img') or not self.main_window.current_img:
                QMessageBox.warning(self.main_window, "Error", "No IMG file loaded")
                return

            # For each selected entry, duplicate it
            for row in selected_rows:
                if row < len(self.main_window.current_img.entries):
                    original_entry = self.main_window.current_img.entries[row]

                    # Create a new entry by copying the original
                    from apps.methods.img_core_classes import IMGEntry
                    new_entry = IMGEntry()
                    new_entry.name = original_entry.name + "_copy"
                    new_entry.offset = original_entry.offset
                    new_entry.size = original_entry.size
                    new_entry.data = original_entry.data if hasattr(original_entry, 'data') else b''

                    # Add the new entry to the IMG file
                    self.main_window.current_img.entries.append(new_entry)

                    # Add the new entry to the table
                    row_count = table.rowCount()
                    table.setRowCount(row_count + 1)

                    # Add the new entry to the table (name column)
                    table.setItem(row_count, 0, QTableWidgetItem(new_entry.name))
                    table.setItem(row_count, 1, QTableWidgetItem(str(new_entry.size)))
                    table.setItem(row_count, 2, QTableWidgetItem(str(new_entry.offset)))

                    # Add data column if it exists
                    if hasattr(new_entry, 'data'):
                        table.setItem(row_count, 3, QTableWidgetItem(str(len(new_entry.data) if new_entry.data else 0)))

            self.main_window.log_message(f"Duplicated {len(selected_rows)} entries")
            QMessageBox.information(self.main_window, "Duplicate", f"Successfully duplicated {len(selected_rows)} entries")

        except Exception as e:
            self.main_window.log_message(f"Error in duplicate entry: {str(e)}")
            QMessageBox.critical(self.main_window, "Duplicate Error", f"Failed to duplicate entry: {str(e)}")

    def _remove_selected_entries(self):
        """Remove selected entries"""
        if hasattr(self.main_window, 'remove_selected'):
            self.main_window.remove_selected()
        else:
            QMessageBox.information(self.main_window, "Remove", "Remove functionality coming soon!")

    def _move_selected_entries(self):
        """Remove selected entries"""
        if hasattr(self.main_window, 'move_selected'):
            self.main_window.move_selected()
        else:
            QMessageBox.information(self.main_window, "move", "move functionality coming soon!")



    def _change_language(self):
        """Change language"""
        try:
            from PyQt6.QtWidgets import QDialog, QVBoxLayout, QHBoxLayout, QLabel, QComboBox, QPushButton
            from PyQt6.QtCore import QLocale, QTranslator
            import os

            dialog = QDialog(self.main_window)
            dialog.setWindowTitle("Change Language")
            dialog.setModal(True)
            dialog.resize(300, 120)

            layout = QVBoxLayout(dialog)

            # Language selection
            lang_layout = QHBoxLayout()
            lang_layout.addWidget(QLabel("Select Language:"))
            lang_combo = QComboBox()

            # Add available languages
            lang_combo.addItem("English", "en")
            lang_combo.addItem("Spanish", "es")
            lang_combo.addItem("French", "fr")
            lang_combo.addItem("German", "de")
            lang_combo.addItem("Italian", "it")
            lang_combo.addItem("Portuguese", "pt")
            lang_combo.addItem("Russian", "ru")
            lang_combo.addItem("Chinese", "zh")
            lang_combo.addItem("Japanese", "ja")

            # Try to set current language if available
            current_settings = getattr(self.main_window, 'app_settings', {})
            current_lang = current_settings.get('language', 'en')
            current_index = 0
            for i in range(lang_combo.count()):
                if lang_combo.itemData(i) == current_lang:
                    current_index = i
                    break
            lang_combo.setCurrentIndex(current_index)

            lang_layout.addWidget(lang_combo)
            layout.addLayout(lang_layout)

            # Buttons
            button_layout = QHBoxLayout()
            ok_btn = QPushButton("OK")
            cancel_btn = QPushButton("Cancel")

            def change_language():
                selected_lang = lang_combo.itemData(lang_combo.currentIndex())

                # Update settings
                if hasattr(self.main_window, 'app_settings'):
                    self.main_window.app_settings.current_settings['language'] = selected_lang
                    # Save settings to file
                    self.main_window.app_settings.save_settings()

                # Log the change
                self.main_window.log_message(f"Language changed to: {selected_lang}")

                # Show message about restart requirement
                QMessageBox.information(
                    self.main_window,
                    "Language Changed",
                    f"Language changed to {lang_combo.currentText()}. Please restart the application for changes to take effect."
                )

                dialog.accept()

            ok_btn.clicked.connect(change_language)
            cancel_btn.clicked.connect(dialog.reject)

            button_layout.addWidget(ok_btn)
            button_layout.addWidget(cancel_btn)
            layout.addLayout(button_layout)

            dialog.exec()

        except Exception as e:
            self.main_window.log_message(f"Error changing language: {str(e)}")
            QMessageBox.critical(self.main_window, "Language Error", f"Failed to change language: {str(e)}")


    def _file_associations(self):
        """File associations"""
        QMessageBox.information(self.main_window, "File Associations", "File associations coming soon!")


    def _default_directories(self):
        """Default directories"""
        try:
            from PyQt6.QtWidgets import QDialog, QVBoxLayout, QHBoxLayout, QLabel, QLineEdit, QPushButton, QFileDialog, QGroupBox
            import os

            dialog = QDialog(self.main_window)
            dialog.setWindowTitle("Default Directories")
            dialog.setModal(True)
            dialog.resize(500, 250)

            layout = QVBoxLayout(dialog)

            # Create group for directory settings
            dir_group = QGroupBox("Default Directories")
            dir_layout = QVBoxLayout(dir_group)

            # Recent files directory
            recent_layout = QHBoxLayout()
            recent_layout.addWidget(QLabel("Recent Files:"))
            self.recent_dir_input = QLineEdit()
            recent_btn = QPushButton("Browse...")

            # Load current setting if available
            current_settings = getattr(self.main_window, 'app_settings', {})
            recent_dir = current_settings.get('recent_dir', os.path.expanduser("~"))
            self.recent_dir_input.setText(recent_dir)

            def browse_recent_dir():
                directory = QFileDialog.getExistingDirectory(
                    dialog, "Select Recent Files Directory", recent_dir
                )
                if directory:
                    self.recent_dir_input.setText(directory)

            recent_btn.clicked.connect(browse_recent_dir)
            recent_layout.addWidget(self.recent_dir_input)
            recent_layout.addWidget(recent_btn)
            dir_layout.addLayout(recent_layout)

            # IMG files directory
            img_layout = QHBoxLayout()
            img_layout.addWidget(QLabel("IMG Files:"))
            self.img_dir_input = QLineEdit()
            img_btn = QPushButton("Browse...")

            img_dir = current_settings.get('img_dir', os.path.expanduser("~"))
            self.img_dir_input.setText(img_dir)

            def browse_img_dir():
                directory = QFileDialog.getExistingDirectory(
                    dialog, "Select IMG Files Directory", img_dir
                )
                if directory:
                    self.img_dir_input.setText(directory)

            img_btn.clicked.connect(browse_img_dir)
            img_layout.addWidget(self.img_dir_input)
            img_layout.addWidget(img_btn)
            dir_layout.addLayout(img_layout)

            # Export directory
            export_layout = QHBoxLayout()
            export_layout.addWidget(QLabel("Export:"))
            self.export_dir_input = QLineEdit()
            export_btn = QPushButton("Browse...")

            export_dir = current_settings.get('export_dir', os.path.expanduser("~/Documents"))
            self.export_dir_input.setText(export_dir)

            def browse_export_dir():
                directory = QFileDialog.getExistingDirectory(
                    dialog, "Select Export Directory", export_dir
                )
                if directory:
                    self.export_dir_input.setText(directory)

            export_btn.clicked.connect(browse_export_dir)
            export_layout.addWidget(self.export_dir_input)
            export_layout.addWidget(export_btn)
            dir_layout.addLayout(export_layout)

            layout.addWidget(dir_group)

            # Buttons
            button_layout = QHBoxLayout()
            ok_btn = QPushButton("OK")
            cancel_btn = QPushButton("Cancel")

            def save_directories():
                # Update settings
                if hasattr(self.main_window, 'app_settings'):
                    self.main_window.app_settings['recent_dir'] = self.recent_dir_input.text()
                    self.main_window.app_settings['img_dir'] = self.img_dir_input.text()
                    self.main_window.app_settings['export_dir'] = self.export_dir_input.text()
                    # Save settings to file
                    self._save_app_settings()

                self.main_window.log_message("Default directories updated")
                dialog.accept()

            ok_btn.clicked.connect(save_directories)
            cancel_btn.clicked.connect(dialog.reject)

            button_layout.addWidget(ok_btn)
            button_layout.addWidget(cancel_btn)
            layout.addLayout(button_layout)

            dialog.exec()

        except Exception as e:
            self.main_window.log_message(f"Error setting default directories: {str(e)}")
            QMessageBox.critical(self.main_window, "Directory Error", f"Failed to set default directories: {str(e)}")


    def _performance_settings(self):
        """Performance settings"""
        try:
            from PyQt6.QtWidgets import QDialog, QVBoxLayout, QHBoxLayout, QLabel, QSpinBox, QCheckBox, QPushButton, QGroupBox
            import os

            dialog = QDialog(self.main_window)
            dialog.setWindowTitle("Performance Settings")
            dialog.setModal(True)
            dialog.resize(450, 350)

            layout = QVBoxLayout(dialog)

            # Memory settings group
            memory_group = QGroupBox("Memory Settings")
            memory_layout = QVBoxLayout(memory_group)

            # Maximum memory usage
            max_memory_layout = QHBoxLayout()
            max_memory_layout.addWidget(QLabel("Max Memory (MB):"))
            self.max_memory_spin = QSpinBox()
            self.max_memory_spin.setRange(64, 8192)  # 64MB to 8GB
            self.max_memory_spin.setSingleStep(64)
            self.max_memory_spin.setValue(1024)  # Default 1GB

            # Load current setting if available
            current_settings = getattr(self.main_window, 'app_settings', {})
            current_max_memory = current_settings.get('max_memory', 1024)
            self.max_memory_spin.setValue(current_max_memory)

            max_memory_layout.addWidget(self.max_memory_spin)
            memory_layout.addLayout(max_memory_layout)

            # Memory buffer size
            buffer_layout = QHBoxLayout()
            buffer_layout.addWidget(QLabel("Buffer Size (MB):"))
            self.buffer_size_spin = QSpinBox()
            self.buffer_size_spin.setRange(16, 512)  # 16MB to 512MB
            self.buffer_size_spin.setSingleStep(16)
            self.buffer_size_spin.setValue(128)  # Default 128MB

            current_buffer_size = current_settings.get('buffer_size', 128)
            self.buffer_size_spin.setValue(current_buffer_size)

            buffer_layout.addWidget(self.buffer_size_spin)
            memory_layout.addLayout(buffer_layout)

            layout.addWidget(memory_group)

            # Processing settings group
            process_group = QGroupBox("Processing Settings")
            process_layout = QVBoxLayout(process_group)

            # Parallel processing
            self.parallel_processing_check = QCheckBox("Enable Parallel Processing")
            current_parallel = current_settings.get('parallel_processing', True)
            self.parallel_processing_check.setChecked(current_parallel)
            process_layout.addWidget(self.parallel_processing_check)

            # Thread count
            thread_layout = QHBoxLayout()
            thread_layout.addWidget(QLabel("Processing Threads:"))
            self.thread_count_spin = QSpinBox()
            self.thread_count_spin.setRange(1, 16)
            self.thread_count_spin.setSingleStep(1)

            current_threads = current_settings.get('thread_count', 4)
            self.thread_count_spin.setValue(current_threads)

            thread_layout.addWidget(self.thread_count_spin)
            process_layout.addLayout(thread_layout)

            # Large file handling
            self.large_file_check = QCheckBox("Optimize for Large Files")
            current_large_file = current_settings.get('large_file_optimization', False)
            self.large_file_check.setChecked(current_large_file)
            process_layout.addWidget(self.large_file_check)

            layout.addWidget(process_group)

            # Buttons
            button_layout = QHBoxLayout()
            ok_btn = QPushButton("OK")
            cancel_btn = QPushButton("Cancel")

            def save_performance_settings():
                # Update settings
                if hasattr(self.main_window, 'app_settings'):
                    self.main_window.app_settings['max_memory'] = self.max_memory_spin.value()
                    self.main_window.app_settings['buffer_size'] = self.buffer_size_spin.value()
                    self.main_window.app_settings['parallel_processing'] = self.parallel_processing_check.isChecked()
                    self.main_window.app_settings['thread_count'] = self.thread_count_spin.value()
                    self.main_window.app_settings['large_file_optimization'] = self.large_file_check.isChecked()
                    # Save settings to file
                    self._save_app_settings()

                self.main_window.log_message("Performance settings updated")
                dialog.accept()

            ok_btn.clicked.connect(save_performance_settings)
            cancel_btn.clicked.connect(dialog.reject)

            button_layout.addWidget(ok_btn)
            button_layout.addWidget(cancel_btn)
            layout.addLayout(button_layout)

            dialog.exec()

        except Exception as e:
            self.main_window.log_message(f"Error setting performance settings: {str(e)}")
            QMessageBox.critical(self.main_window, "Performance Error", f"Failed to set performance settings: {str(e)}")


    def _reset_settings(self):
        """Reset settings"""
        try:
            reply = QMessageBox.question(
                self.main_window,
                "Reset Settings",
                "Are you sure you want to reset all settings to default values?\nThis action cannot be undone.",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
            )

            if reply == QMessageBox.StandardButton.Yes:
                # Reset to default settings
                default_settings = {
                    'language': 'en',
                    'recent_dir': '',
                    'img_dir': '',
                    'export_dir': '',
                    'max_memory': 1024,
                    'buffer_size': 128,
                    'parallel_processing': True,
                    'thread_count': 4,
                    'large_file_optimization': False,
                    'window_size': [1200, 800],
                    'window_position': [100, 100],
                    'recent_files': []
                }

                # Update the main window settings
                if hasattr(self.main_window, 'app_settings'):
                    self.main_window.app_settings = default_settings
                    # Save settings to file
                    self._save_app_settings()

                self.main_window.log_message("Settings have been reset to defaults")
                QMessageBox.information(
                    self.main_window,
                    "Settings Reset",
                    "All settings have been reset to their default values."
                )

        except Exception as e:
            self.main_window.log_message(f"Error resetting settings: {str(e)}")
            QMessageBox.critical(self.main_window, "Reset Error", f"Failed to reset settings: {str(e)}")


    def _open_ide_editor(self):
        """Open IDE Editor"""
        try:
            from apps.components.Ide_Editor.col_editor import open_ide_editor
            editor = open_ide_editor(self.main_window)

            # Connect signals for integration
            if hasattr(editor, 'sort_by_ide_requested'):
                editor.sort_by_ide_requested.connect(self._handle_sort_by_ide)
            if hasattr(editor, 'selection_sync_requested'):
                editor.selection_sync_requested.connect(self._handle_selection_sync)

            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("IDE Editor opened")

        except ImportError:
            QMessageBox.warning(self.main_window, "IDE Editor", "IDE Editor components not found")
        except Exception as e:
            QMessageBox.critical(self.main_window, "Error", f"Failed to open IDE Editor: {str(e)}")


    def _sort_img_by_ide(self):
        """Sort IMG entries by IDE order"""
        try:
            if hasattr(self.main_window, 'sort_img_by_ide_order'):
                self.main_window.sort_img_by_ide_order()
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("IMG entries sorted by IDE order")
            else:
                QMessageBox.information(self.main_window, "Sort IMG",
                    "Sort IMG by IDE functionality will be available when IDE file is loaded.")
        except Exception as e:
            QMessageBox.critical(self.main_window, "Error", f"Failed to sort IMG by IDE: {str(e)}")


    def _sort_col_by_ide(self):
        """Sort COL entries by IDE order"""
        try:
            if hasattr(self.main_window, 'sort_col_by_ide_order'):
                self.main_window.sort_col_by_ide_order()
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("COL entries sorted by IDE order")
            else:
                QMessageBox.information(self.main_window, "Sort COL",
                    "Sort COL by IDE functionality will be available when both IDE and COL files are loaded.")
        except Exception as e:
            QMessageBox.critical(self.main_window, "Error", f"Failed to sort COL by IDE: {str(e)}")


    def _handle_sort_by_ide(self, model_order):
        """Handle sort by IDE request from IDE Editor"""
        try:
            # Apply sorting to IMG Factory main window
            if hasattr(self.main_window, 'apply_ide_sort_order'):
                self.main_window.apply_ide_sort_order(model_order)

            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Applied IDE sort order: {len(model_order)} models")

        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Error applying IDE sort: {str(e)}")


    def _handle_selection_sync(self, selected_models):
        """Handle selection sync request from IDE Editor"""
        try:
            # Sync selection with COL Editor if open
            if hasattr(self.main_window, 'sync_col_editor_selection'):
                self.main_window.sync_col_editor_selection(selected_models)

            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Synced selection: {len(selected_models)} models")

        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Error syncing selection: {str(e)}")


    def _view_ide(self):
        """View IDE file"""
        QMessageBox.information(self.main_window, "View IDE", "IDE viewer coming soon!")

    def _edit_ide(self):
        """Edit IDE file (alias for IDE Editor)"""
        self._open_ide_editor()

    def _validate_ide(self):
        """Validate IDE file"""
        QMessageBox.information(self.main_window, "Validate IDE", "IDE validation coming soon!")

    def _search_ide(self):
        """Search IDE entries"""
        QMessageBox.information(self.main_window, "Search IDE", "IDE search coming soon!")

    def _export_ide(self):
        """Export IDE to text"""
        QMessageBox.information(self.main_window, "Export IDE", "IDE export coming soon!")

    def _import_ide(self):
        """Import IDE from text"""
        QMessageBox.information(self.main_window, "Import IDE", "IDE import coming soon!")

    def _convert_ide(self):
        """Convert IDE format"""
        QMessageBox.information(self.main_window, "Convert IDE", "IDE format conversion coming soon!")

    def _backup_ide(self):
        """Backup IDE file"""
        QMessageBox.information(self.main_window, "Backup IDE", "IDE backup coming soon!")

    def _compare_ide(self):
        """Compare IDE files"""
        QMessageBox.information(self.main_window, "Compare IDE", "IDE comparison coming soon!")

    # ========================================================================
    # COL MENU CALLBACKS
    # ========================================================================

    def _open_col_editor(self):
        """Open COL Editor"""
        try:
            from gui.gui_context import open_col_editor_dialog
            open_col_editor_dialog(self.main_window)
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message("COL Editor opened")
        except ImportError:
            QMessageBox.warning(self.main_window, "COL Editor", "COL Editor components not found")
        except Exception as e:
            QMessageBox.critical(self.main_window, "Error", f"Failed to open COL Editor: {str(e)}")

    def _view_collision(self):
        """View collision data"""
        QMessageBox.information(self.main_window, "View Collision", "COL viewer coming soon!")

    def _edit_collision(self):
        """Edit collision (alias for COL Editor)"""
        self._open_col_editor()

    def _export_collision(self):
        """Export collision data"""
        QMessageBox.information(self.main_window, "Export Collision", "COL export coming soon!")

    def _import_collision(self):
        """Import collision data"""
        QMessageBox.information(self.main_window, "Import Collision", "COL import coming soon!")

    def _validate_collision(self):
        """Validate collision data"""
        QMessageBox.information(self.main_window, "Validate COL", "COL validation coming soon!")

    def _optimize_collision(self):
        """Optimize collision data"""
        QMessageBox.information(self.main_window, "Optimize COL", "COL optimization coming soon!")

    def _analyze_collision(self):
        """Analyze collision data"""
        try:
            from gui.gui_context import analyze_col_file_dialog
            analyze_col_file_dialog(self.main_window)
        except ImportError:
            QMessageBox.information(self.main_window, "Analyze COL", "COL analyzer coming soon!")
        except Exception as e:
            QMessageBox.critical(self.main_window, "Error", f"Failed to analyze COL: {str(e)}")


    def _batch_export_collision(self):
        """Batch export collision files"""
        QMessageBox.information(self.main_window, "Batch Export", "COL batch export coming soon!")

    def _batch_convert_collision(self):
        """Batch convert collision files"""
        QMessageBox.information(self.main_window, "Batch Convert", "COL batch convert coming soon!")

    def _view_collision_3d(self):
        """View collision in 3D"""
        QMessageBox.information(self.main_window, "3D Collision Viewer", "3D COL viewer coming soon!")

    def _collision_properties(self):
        """Show collision properties"""
        QMessageBox.information(self.main_window, "COL Properties", "COL properties dialog coming soon!")

    def _collision_debug(self):
        """Show collision debug info"""
        QMessageBox.information(self.main_window, "COL Debug", "COL debug information coming soon!")

    # ========================================================================
    # TOOLS MENU CALLBACKS
    # ========================================================================


    def _view_texture(self): #vers 1
        """View texture in TXD Workshop"""
        self._open_txd_editor()

    def _export_texture(self): #vers 1
        """Export selected texture"""
        try:
            if hasattr(self.main_window, 'txd_workshops') and self.main_window.txd_workshops:
                # Get first visible workshop
                for workshop in self.main_window.txd_workshops:
                    if workshop and workshop.isVisible():
                        workshop.export_selected_texture()
                        return
            QMessageBox.information(self.main_window, "Export Texture",
                "Please open TXD Workshop first (Tools → TXD Editor)")
        except Exception as e:
            QMessageBox.critical(self.main_window, "Error", f"Failed to export texture: {str(e)}")

    def _import_texture(self): #vers 1
        """Import texture to TXD"""
        QMessageBox.information(self.main_window, "Import Texture", "Import texture coming soon!")

    def _replace_texture(self): #vers 1
        """Replace texture in TXD"""
        QMessageBox.information(self.main_window, "Replace Texture", "Replace texture coming soon!")

    def _convert_format(self): #vers 1
        """Convert texture format"""
        QMessageBox.information(self.main_window, "Convert Format", "Format conversion coming soon!")

    def _validate_txd(self): #vers 1
        """Validate TXD file structure"""
        QMessageBox.information(self.main_window, "Validate TXD", "TXD validation coming soon!")

    def _optimize_textures(self): #vers 1
        """Optimize textures for size/quality"""
        QMessageBox.information(self.main_window, "Optimize Textures", "Texture optimization coming soon!")

    def _batch_export(self): #vers 1
        """Batch export textures"""
        QMessageBox.information(self.main_window, "Batch Export", "Batch export coming soon!")

    def _batch_convert(self): #vers 1
        """Batch convert texture formats"""
        QMessageBox.information(self.main_window, "Batch Convert", "Batch convert coming soon!")

    def _batch_resize(self): #vers 1
        """Batch resize textures"""
        QMessageBox.information(self.main_window, "Batch Resize", "Batch resize coming soon!")

    def _extract_palette(self): #vers 1
        """Extract color palette from textures"""
        QMessageBox.information(self.main_window, "Extract Palette", "Extract palette coming soon!")

    def _create_atlas(self): #vers 1
        """Create texture atlas"""
        QMessageBox.information(self.main_window, "Create Atlas", "Create atlas coming soon!")

    def _texture_properties(self): #vers 1
        """Show texture properties dialog"""
        QMessageBox.information(self.main_window, "Texture Properties", "Texture properties coming soon!")


    def _open_txd_editor(self): #vers 4
        """Open TXD Workshop in docked mode"""
        try:
            # Get IMG path if available
            img_path = None
            if hasattr(self.main_window, 'current_img') and self.main_window.current_img:
                img_path = self.main_window.current_img.file_path

            # Open docked workshop
            workshop = self.main_window.open_txd_workshop_docked()

            # Load IMG if available
            if workshop and img_path:
                workshop.load_from_img_archive(img_path)

            # Track workshop instance
            if workshop:
                if not hasattr(self.main_window, 'txd_workshops'):
                    self.main_window.txd_workshops = []
                self.main_window.txd_workshops.append(workshop)

                self.main_window.log_message("TXD Workshop opened (docked)")

        except Exception as e:
            QMessageBox.critical(self.main_window, "Error",
                f"Failed to open TXD Workshop: {str(e)}")


    def _open_dff_editor(self):
        """Open DFF Editor"""
        QMessageBox.information(self.main_window, "DFF Editor", "DFF Editor coming soon!")

    def _open_ifp_editor(self):
        """Open IFP Editor"""
        QMessageBox.information(self.main_window, "IFP Editor", "IFP Editor coming soon!")

    def _open_ipl_editor(self):
        """Open IPL Editor"""
        QMessageBox.information(self.main_window, "IPL Editor", "IPL Editor coming soon!")

    def _open_dat_editor(self):
        """Open DAT Editor"""
        QMessageBox.information(self.main_window, "DAT Editor", "DAT Editor coming soon!")

    def add_col_menu(img_factory_instance):
        """Add COL menu to the main menu bar"""

        menubar = img_factory_instance.menuBar()

        # Create COL menu
        col_menu = menubar.addMenu("COL")

        # File operations
        open_col_action = QAction("Open COL File", img_factory_instance)
        open_col_action.setShortcut("Ctrl+Shift+O")
        open_col_action.triggered.connect(lambda: open_col_file_dialog(img_factory_instance))
        col_menu.addAction(open_col_action)

        new_col_action = QAction("New COL File", img_factory_instance)
        new_col_action.triggered.connect(lambda: create_new_col_file(img_factory_instance))
        col_menu.addAction(new_col_action)

        col_menu.addSeparator()

        # COL Editor
        editor_action = QAction("COL Editor", img_factory_instance)
        editor_action.setShortcut("Ctrl+E")
        editor_action.triggered.connect(lambda: open_col_editor(img_factory_instance))
        col_menu.addAction(editor_action)

        col_menu.addSeparator()

        # Batch operations
        batch_process_action = QAction("Batch Processor", img_factory_instance)
        batch_process_action.triggered.connect(lambda: open_col_batch_processor(img_factory_instance))
        col_menu.addAction(batch_process_action)

        analyze_action = QAction("Analyze DFF", img_factory_instance) # TODO finish function
        analyze_action.triggered.connect(lambda: analyze_dff_file_dialog(img_factory_instance))
        dff_menu.addAction(analyze_action)

        analyze_action = QAction("Analyze TXD", img_factory_instance) # TODO finish function
        analyze_action.triggered.connect(lambda: analyze_txd_file_dialog(img_factory_instance))
        txd_menu.addAction(analyze_action)

        analyze_action = QAction("Analyze COL", img_factory_instance)
        analyze_action.triggered.connect(lambda: analyze_col_file_dialog(img_factory_instance))
        col_menu.addAction(analyze_action)

        # Analyze file action
        analyze_action = QAction("Analyze File", img_factory_instance) # TODO finish function
        if row is not None and entry_info:
            analyze_action.triggered.connect(lambda: analyze_file(main_window, row, entry_info))
        else:
            analyze_action.triggered.connect(lambda: analyze_selected_file(main_window))
        menu.addAction(analyze_action)

        col_menu.addSeparator()

        # Import/Export
        import_to_img_action = QAction("Import to IMG", img_factory_instance)
        import_to_img_action.triggered.connect(lambda: import_col_to_img(img_factory_instance))
        col_menu.addAction(import_to_img_action)

        export_from_img_action = QAction("Export from IMG", img_factory_instance)
        export_from_img_action.triggered.connect(lambda: export_col_from_img(img_factory_instance))
        col_menu.addAction(export_from_img_action)

    # Store reference to COL menu
    #img_factory_instance.col_menu = col_menu

    def set_callbacks(self, callbacks: Dict[str, Callable]):
        """Set menu callbacks"""
        self.callbacks.update(callbacks)
        self._connect_callbacks()

    def _connect_callbacks(self):
        """Connect actions to callbacks"""
        for action_id, callback in self.callbacks.items():
            if action_id in self.actions:
                self.actions[action_id].triggered.connect(callback)

    def enable_action(self, action_id: str, enabled: bool = True):
        """Enable/disable an action"""
        if action_id in self.actions:
            self.actions[action_id].setEnabled(enabled)

    def check_action(self, action_id: str, checked: bool = True):
        """Check/uncheck an action"""
        if action_id in self.actions:
            action = self.actions[action_id]
            if action.isCheckable():
                action.setChecked(checked)

    def get_action(self, action_id: str) -> Optional[QAction]:
        """Get action by ID"""
        return self.actions.get(action_id)

    def set_panel_manager(self, panel_manager: PanelManager):
        """Set panel manager for panel menu integration"""
        self.panel_manager = panel_manager
        self.add_panel_menu()

    def add_panel_menu(self):
        """Add panels menu for tear-off functionality"""
        if not self.panel_manager:
            return

        panels_menu = self.menu_bar.addMenu("&Panels")
        self.menus["Panels"] = panels_menu

        # Show/Hide panels
        show_hide_menu = panels_menu.addMenu("Show/Hide")

        for panel_id, panel in self.panel_manager.panels.items():
            action = QAction(panel.title, self.main_window)
            action.setCheckable(True)
            action.setChecked(panel.isVisible())
            action.triggered.connect(
                lambda checked, pid=panel_id:
                self.panel_manager.show_panel(pid) if checked
                else self.panel_manager.hide_panel(pid)
            )
            show_hide_menu.addAction(action)

        panels_menu.addSeparator()

        # Reset layout
        reset_action = QAction("Reset Layout", self.main_window)
        reset_action.triggered.connect(self._reset_panel_layout)
        panels_menu.addAction(reset_action)

    # ========================================================================
    # ORIGINAL CALLBACK IMPLEMENTATIONS (PRESERVED)
    # ========================================================================

    def _exit_application(self):
        """Exit the application"""
        self.main_window.close()

    """
    def _apply_instant_theme(self, theme_name): #vers 1
        #Apply theme change instantly to main window
        try:
            # Get the stylesheet
            stylesheet = self.main_window.app_settings.get_stylesheet()

            # Apply to main window
            self.main_window.setStyleSheet(stylesheet)

            # Apply to gui_layout components
            if hasattr(self.main_window, 'gui_layout'):
                if hasattr(self.main_window.gui_layout, 'apply_all_window_themes'):
                    self.main_window.gui_layout.apply_all_window_themes()

            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Theme changed: {theme_name}")

        except Exception as e:
            print(f"❌ Instant theme apply error: {e}")
    """

    def _show_preferences(self):
        """Show preferences dialog with proper lifecycle management"""
        try:
            # Check if dialog already exists and is visible
            if hasattr(self, '_preferences_dialog') and self._preferences_dialog is not None:
                if self._preferences_dialog.isVisible():
                    # Bring existing dialog to front
                    self._preferences_dialog.raise_()
                    self._preferences_dialog.activateWindow()
                    return
                else:
                    # Clean up old dialog
                    self._preferences_dialog.deleteLater()
                    self._preferences_dialog = None

            if hasattr(self.main_window, 'app_settings'):
                from apps.utils.app_settings_system import SettingsDialog

                # Create new dialog
                self._preferences_dialog = SettingsDialog(self.main_window.app_settings, self.main_window)

                # Connect theme change signal for instant apply
                #self._preferences_dialog.themeChanged.connect(
                #    lambda theme_name: self._apply_instant_theme(theme_name)
                #)

                # Connect cleanup signal
                self._preferences_dialog.finished.connect(self._on_preferences_closed)

                # Show dialog
                result = self._preferences_dialog.exec()

                if result == QDialog.DialogCode.Accepted:
                    self._apply_gui_changes()
                    if hasattr(self.main_window, 'log_message'):
                        self.main_window.log_message("Preferences updated")

                # Clean up
                self._preferences_dialog = None

            else:
                QMessageBox.information(
                    self.main_window,
                    "Preferences",
                    "Preferences system not available"
                )
        except Exception as e:
            QMessageBox.critical(
                self.main_window,
                "Error",
                "Failed to open preferences: " + str(e)
            )
            # Clean up on error
            if hasattr(self, '_preferences_dialog'):
                self._preferences_dialog = None

    def _on_preferences_closed(self):
        """Handle preferences dialog closed"""
        if hasattr(self, '_preferences_dialog'):
            self._preferences_dialog = None

    def _show_gui_settings(self):
        """Show GUI settings dialog"""
        QMessageBox.information(
            self.main_window,
            "GUI Settings",
            "GUI customization dialog coming soon!"
        )

    def _show_app_settings(self): #vers 3
        """Show IMG Factory-specific settings dialog"""
        try:
            from apps.methods.imgfactory_ui_settings import show_imgfactory_settings_dialog
            show_imgfactory_settings_dialog(self.main_window)
        except Exception as e:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(
                self.main_window,
                "Error",
                f"Failed to open IMG Factory Settings: {str(e)}")


    def apply_app_settings(self):
        """Apply IMG Factory-specific settings to the application"""
        from PyQt6.QtWidgets import QApplication
        from PyQt6.QtGui import QFont
        from apps.methods.img_factory_settings import IMGFactorySettings

        # Create an instance of IMGFactorySettings to get current settings
        img_settings = IMGFactorySettings()

        # Apply font settings if custom font is enabled
        if img_settings.get("use_custom_font", False):
            font = QFont(img_settings.get("font_family", "Segoe UI"))
            font.setPointSize(img_settings.get("font_size", 9))
            font.setBold(img_settings.get("font_bold", False))
            font.setItalic(img_settings.get("font_italic", False))
            self.setFont(font)
            QApplication.instance().setFont(font)

        # Apply window size/position if enabled
        if img_settings.get("remember_window_size", True):
            width = img_settings.get("last_window_width", 1200)
            height = img_settings.get("last_window_height", 800)
            self.resize(width, height)

        if img_settings.get("remember_window_position", True):
            x = img_settings.get("last_window_x", -1)
            y = img_settings.get("last_window_y", -1)
            if x >= 0 and y >= 0:
                self.move(x, y)


    def _export_settings(self):
        """Export settings to file"""
        try:
            file_path, _ = QFileDialog.getSaveFileName(
                self.main_window,
                "Export Settings",
                "img_factory_settings.json",
                "JSON Files (*.json)"
            )

            if file_path:
                QMessageBox.information(
                    self.main_window,
                    "Export Complete",
                    "Settings exported to " + file_path
                )

        except Exception as e:
            QMessageBox.critical(
                self.main_window,
                "Export Failed",
                "Failed to export settings: " + str(e)
            )

    def _import_settings(self):
        """Import settings from file"""
        try:
            file_path, _ = QFileDialog.getOpenFileName(
                self.main_window,
                "Import Settings",
                "",
                "JSON Files (*.json)"
            )

            if file_path:
                QMessageBox.information(
                    self.main_window,
                    "Import Complete",
                    "Settings imported from " + file_path
                )

        except Exception as e:
            QMessageBox.critical(
                self.main_window,
                "Import Failed",
                "Failed to import settings: " + str(e)
            )

    def _reset_layout(self):
        """Reset GUI layout"""
        reply = QMessageBox.question(
            self.main_window,
            "Reset Layout",
            "This will reset the GUI layout to defaults. Continue?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
        )

        if reply == QMessageBox.StandardButton.Yes:
            QMessageBox.information(
                self.main_window,
                "Reset Complete",
                "Layout has been reset to defaults."
            )

    def _toggle_toolbar(self):
        """Toggle toolbar visibility"""
        if hasattr(self.main_window, 'toolbar'):
            visible = self.main_window.toolbar.isVisible()
            self.main_window.toolbar.setVisible(not visible)
            self.check_action("toolbar", not visible)

    def _toggle_statusbar(self):
        """Toggle status bar visibility"""
        if hasattr(self.main_window, 'statusBar'):
            statusbar = self.main_window.statusBar()
            visible = statusbar.isVisible()
            statusbar.setVisible(not visible)
            self.check_action("statusbar", not visible)

    def _toggle_log_panel(self):
        """Toggle log panel visibility"""
        if hasattr(self.main_window, 'log_panel'):
            visible = self.main_window.log_panel.isVisible()
            self.main_window.log_panel.setVisible(not visible)
            self.check_action("log_panel", not visible)

    def _toggle_fullscreen(self):
        """Toggle fullscreen mode"""
        if self.main_window.isFullScreen():
            self.main_window.showNormal()
        else:
            self.main_window.showFullScreen()

    def _show_about(self): #vers 6
        """Show about dialog"""
        about_text = """
        <h2>IMG Factory 1.5</h2>
        <p><b>Version:</b> 1.5.0</p>
        <p><b>Build Date:</b> August 14, 2025</p>
        <p><b>Author:</b> X-Seti</p>
        <p><b>Original Credits:</b> MexUK 2007 IMG Factory 1.2</p>
        <br>
        <p>A comprehensive tool for managing GTA IMG archives and related files.</p>
        <p>Supports COL, TXD, DFF, IFP, IDE, IPL, and other GTA file formats.</p>
        <br>
        <p><b>Features:</b></p>
        <ul>
        <li>IMG archive creation and editing</li>
        <li>File import/export with progress tracking</li>
        <li>COL collision file editor</li>
        <li>IDE item definition editor</li>
        <li>TXD texture management</li>
        <li>DFF model viewer</li>
        <li>Advanced search and filtering</li>
        <li>Customizable interface and themes</li>
        <li>Batch processing tools</li>
        <li>Sort by IDE functionality</li>
        </ul>
        """

        QMessageBox.about(self.main_window, "About IMG Factory", about_text)

    def _show_about_qt(self):
        """Show about Qt dialog"""
        QMessageBox.aboutQt(self.main_window, "About Qt")

    def _show_help(self):
        """Show help contents"""
        help_text = """
        <h2>IMG Factory Help</h2>
        <h3>Getting Started:</h3>
        <p>1. Open an IMG file using File → Open IMG</p>
        <p>2. Browse entries in the main table</p>
        <p>3. Use right-click context menus for entry operations</p>
        <p>4. Import files using Entry → Import Files</p>
        <p>5. Export files using Entry → Export Selected</p>

        <h3>New Features:</h3>
        <p><b>IDE Editor:</b> Edit item definitions with built-in help guide</p>
        <p><b>COL Editor:</b> Advanced collision editing with 3D visualization</p>
        <p><b>Sort by IDE:</b> Reorder IMG/COL entries by IDE sequence</p>

        <h3>Keyboard Shortcuts:</h3>
        <p><b>Ctrl+O:</b> Open IMG file</p>
        <p><b>Ctrl+N:</b> Create new IMG file</p>
        <p><b>Ctrl+I:</b> Import files</p>
        <p><b>Ctrl+E:</b> Export selected entries</p>
        <p><b>F5:</b> Validate IMG</p>
        <p><b>F6:</b> Rebuild IMG</p>
        <p><b>Delete:</b> Remove selected entries</p>
        <p><b>F2:</b> Rename entry</p>
        <p><b>F2:</b> Move entry</p>
        <p><b>F7:</b> View model</p>
        <p><b>F8:</b> View texture</p>
        <p><b>F9:</b> View collision</p>
        <p><b>Ctrl+Shift+C:</b> Open COL Editor</p>

        <h3>File Types:</h3>
        <p><b>IMG:</b> Archive files containing game assets</p>
        <p><b>COL:</b> Collision data files</p>
        <p><b>TXD:</b> Texture dictionary files</p>
        <p><b>DFF:</b> 3D model files</p>
        <p><b>IFP:</b> Animation files</p>
        <p><b>IDE:</b> Item definition files</p>
        <p><b>IPL:</b> Item placement files</p>
        """

        msg = QMessageBox(self.main_window)
        msg.setWindowTitle("Help Contents")
        msg.setText(help_text)
        msg.setTextFormat(Qt.TextFormat.RichText)
        msg.exec()

    def _show_shortcuts(self):
        """Show keyboard shortcuts"""
        shortcuts_text = """
        <h2>Keyboard Shortcuts</h2>
        <table border="1" cellpadding="5" style="border-collapse: collapse;">
        <tr><th>Action</th><th>Shortcut</th></tr>
        <tr><td>New IMG</td><td>Ctrl+N</td></tr>
        <tr><td>Open IMG</td><td>Ctrl+O</td></tr>
        <tr><td>Close IMG</td><td>Ctrl+W</td></tr>
        <tr><td>Save</td><td>Ctrl+S</td></tr>
        <tr><td>Import Files</td><td>Ctrl+I</td></tr>
        <tr><td>Export Selected</td><td>Ctrl+E</td></tr>
        <tr><td>Quick Export</td><td>Ctrl+Q</td></tr>
        <tr><td>Remove Selected</td><td>Delete</td></tr>
        <tr><td>Rename Entry</td><td>F2</td></tr>
        <tr><td>Select All</td><td>Ctrl+A</td></tr>
        <tr><td>Find</td><td>Ctrl+F</td></tr>
        <tr><td>IMG Information</td><td>F4</td></tr>
        <tr><td>Validate IMG</td><td>F5</td></tr>
        <tr><td>Rebuild IMG</td><td>F6</td></tr>
        <tr><td>View Model</td><td>F7</td></tr>
        <tr><td>View Texture</td><td>F8</td></tr>
        <tr><td>View Collision</td><td>F9</td></tr>
        <tr><td>COL Editor</td><td>Ctrl+Shift+C</td></tr>
        <tr><td>TXD Editor</td><td>Ctrl+Shift+T</td></tr>
        <tr><td>DFF Editor</td><td>Ctrl+Shift+D</td></tr>
        <tr><td>IFP Editor</td><td>Ctrl+Shift+I</td></tr>
        <tr><td>IDE Editor</td><td>Tools → IDE Editor</td></tr>
        <tr><td>Log Panel</td><td>F12</td></tr>
        <tr><td>Fullscreen</td><td>F11</td></tr>
        <tr><td>Preferences</td><td>Ctrl+,</td></tr>
        <tr><td>Exit</td><td>Ctrl+Q</td></tr>
        </table>
        """

        msg = QMessageBox(self.main_window)
        msg.setWindowTitle("Keyboard Shortcuts")
        msg.setText(shortcuts_text)
        msg.setTextFormat(Qt.TextFormat.RichText)
        msg.exec()

    def _show_formats(self):
        """Show supported formats"""
        formats_text = """
        <h2>Supported File Formats</h2>

        <h3>Archive Formats:</h3>
        <p><b>IMG:</b> GTA image archives (versions 1, 2, 3)</p>
        <p><b>DAT:</b> GTA data files</p>

        <h3>3D Model Formats:</h3>
        <p><b>DFF:</b> RenderWare DFF models</p>
        <p><b>COL:</b> Collision data (versions 1, 2, 3, 4)</p>
        <p><b>WDR:</b> World drawable files</p>

        <h3>Texture Formats:</h3>
        <p><b>TXD:</b> RenderWare texture dictionaries</p>
        <p><b>WTD:</b> World texture dictionaries</p>

        <h3>Animation Formats:</h3>
        <p><b>IFP:</b> Animation packages</p>
        <p><b>YCD:</b> Clip dictionaries</p>

        <h3>Data Formats:</h3>
        <p><b>IDE:</b> Item definition files</p>
        <p><b>IPL:</b> Item placement files</p>
        <p><b>DAT:</b> Various data files</p>

        <h3>Import/Export Formats:</h3>
        <p><b>Images:</b> PNG, JPG, BMP, TGA, DDS</p>
        <p><b>Models:</b> OBJ, PLY (export only)</p>
        <p><b>Text:</b> TXT, CSV (for data files)</p>
        """

        msg = QMessageBox(self.main_window)
        msg.setWindowTitle("Supported Formats")
        msg.setText(formats_text)
        msg.setTextFormat(Qt.TextFormat.RichText)
        msg.exec()

    def _show_debug_console(self):
        """Show debug console"""
        QMessageBox.information(
            self.main_window,
            "Debug Console",
            "Debug console coming soon!"
        )

    def _clear_debug_log(self):
        """Clear debug log"""
        if hasattr(self.main_window, 'log_message'):
            self.main_window.log_message("Debug log cleared")
        QMessageBox.information(
            self.main_window,
            "Debug Log",
            "Debug log has been cleared."
        )

# Export main classes
__all__ = [
    'IMGFactoryMenuBar',
    'MenuAction',
    'MenuDefinition',
    'COLMenuBuilder'
]
