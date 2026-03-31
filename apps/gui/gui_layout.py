#this belongs in gui/ gui_layout.py - Version: 31
# X-Seti - February04 2026 - Img Factory 1.6 - GUI Layout Module

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
from apps.methods.imgfactory_svg_icons import SVGIconFactory

from apps.methods.imgfactory_svg_icons import (
    get_add_icon, get_open_icon, get_refresh_icon, get_close_icon, get_close_all_icon,
    get_save_icon, get_export_icon, get_import_icon, get_remove_icon,
    get_edit_icon, get_view_icon, get_search_icon, get_settings_icon,
    get_rebuild_icon, get_rebuild_all_icon, get_merge_icon, get_split_icon, get_convert_icon,
    get_import_via_icon, get_export_via_icon, get_dump_icon,
    get_remove_via_icon, get_select_all_icon, get_select_inverse_icon,
    get_sort_icon, get_pin_icon, get_col_workshop_icon, get_txd_workshop_icon,
    get_hybrid_load_icon, get_scan_folder_icon,
    get_recent_scans_icon, get_tba_icon,
    get_dat_browser_icon, get_ide_editor_icon
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
from apps.core.img_split import split_img, split_img_via, integrate_split_functions
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

from apps.methods.imgfactory_svg_icons import (
    get_settings_icon, get_open_icon, get_save_icon,
    get_extract_icon, get_undo_icon, get_info_icon,
    get_minimize_icon, get_maximize_icon, get_close_icon
)

def _register_tool_taskbar(main_window, key, label, icon_fn, tooltip="", target=None):
    """Register a tool in the main window taskbar if it exists."""
    try:
        if hasattr(main_window, 'register_tool'):
            main_window.register_tool(key, label, icon_fn, target, tooltip)
    except Exception:
        pass


def edit_txd_file(main_window): #vers 6
    """Open TXD Workshop.
    
    If a .txd entry is selected in the table or dir tree, open that file.
    If nothing is selected (or selection is not a TXD), open the workshop
    against the current IMG so the user can browse all TXDs inside it.
    """
    try:
        # Priority 1: dir tree selection
        selected = getattr(main_window, '_dir_tree_selected_file', None)
        if not selected:
            if hasattr(main_window, 'directory_tree'):
                tree = main_window.directory_tree
                for attr in ('tree', '_second_tree'):
                    t = getattr(tree, attr, None)
                    if t:
                        item = t.currentItem()
                        if item:
                            from PyQt6.QtCore import Qt
                            import os
                            path = item.data(0, Qt.ItemDataRole.UserRole)
                            if path and os.path.isfile(path):
                                selected = path
                                break

        if selected and selected.lower().endswith('.txd'):
            if hasattr(main_window, '_load_txd_file_in_new_tab'):
                main_window._load_txd_file_in_new_tab(selected)
                _register_tool_taskbar(main_window, "txd", "TXD", get_txd_workshop_icon, "TXD Workshop")
            else:
                from apps.components.Txd_Editor.txd_workshop import open_txd_workshop
                w = open_txd_workshop(main_window, selected)
                _register_tool_taskbar(main_window, "txd", "TXD", get_txd_workshop_icon, "TXD Workshop", w)
            main_window.log_message(f"TXD Workshop opened: {os.path.basename(selected)}")
            return

        # Priority 2: IMG table selection (if it's a TXD entry)
        from apps.methods.export_shared import get_active_table
        entries_table = get_active_table(main_window)
        selected_items = entries_table.selectedItems() if entries_table else []
        if selected_items:
            row = selected_items[0].row()
            item0 = entries_table.item(row, 0)
            filename = item0.text() if item0 else ""
            if filename.lower().endswith('.txd'):
                # Open workshop with the current IMG, auto-select this TXD
                img_path = None
                if hasattr(main_window, 'current_img') and main_window.current_img:
                    img_path = main_window.current_img.file_path
                from apps.components.Txd_Editor.txd_workshop import open_txd_workshop
                w = open_txd_workshop(main_window, img_path)
                _register_tool_taskbar(main_window, "txd", "TXD", get_txd_workshop_icon, "TXD Workshop", w)
                main_window.log_message(f"TXD Workshop opened for: {filename}")
                return

        # Priority 3: Nothing selected — open workshop against current IMG
        # so the user can browse its textures without needing to select one first
        from apps.components.Txd_Editor.txd_workshop import open_txd_workshop
        img_path = None
        if hasattr(main_window, 'current_img') and main_window.current_img:
            img_path = main_window.current_img.file_path
        w = open_txd_workshop(main_window, img_path)
        _register_tool_taskbar(main_window, "txd", "TXD", get_txd_workshop_icon, "TXD Workshop", w)
        if img_path:
            main_window.log_message(f"TXD Workshop opened (browse mode): {os.path.basename(img_path)}")
        else:
            main_window.log_message("TXD Workshop opened (no file loaded)")
        return

        # (dead code kept for reference — was the old "nothing selected" error)
        if False:
            main_window.log_message("No TXD file selected")
            return
        row = selected_items[0].row()
        filename = entries_table.item(row, 0).text()
        if not filename.lower().endswith('.txd'):
            main_window.log_message("Selected file is not a TXD file")
            return
        img_path = None
        if hasattr(main_window, 'current_img') and main_window.current_img:
            img_path = main_window.current_img.file_path
        from apps.components.Txd_Editor.txd_workshop import open_txd_workshop
        workshop = open_txd_workshop(main_window, img_path)
        if workshop:
            main_window.log_message(f"TXD Workshop opened for: {filename}")
            _register_tool_taskbar(main_window, "txd", "TXD", get_txd_workshop_icon, "TXD Workshop", workshop)
    except Exception as e:
        main_window.log_message(f"Error opening TXD Workshop: {e}")


def edit_col_file(main_window): #vers 4
    """Open COL Workshop.

    Priority 1: dir tree .col selection  → open that file directly.
    Priority 2: IMG table .col entry highlighted → open workshop with that file.
    Priority 3: Nothing selected → open workshop against current IMG (browse mode).
    """
    try:
        import os

        # Priority 1: dir tree selection
        selected = getattr(main_window, '_dir_tree_selected_file', None)
        if not selected:
            if hasattr(main_window, 'directory_tree'):
                tree = main_window.directory_tree
                for attr in ('tree', '_second_tree'):
                    t = getattr(tree, attr, None)
                    if t:
                        item = t.currentItem()
                        if item:
                            from PyQt6.QtCore import Qt
                            path = item.data(0, Qt.ItemDataRole.UserRole)
                            if path and os.path.isfile(path):
                                selected = path
                                break

        if selected and selected.lower().endswith('.col'):
            from apps.components.Col_Editor.col_workshop import open_col_workshop
            w = open_col_workshop(main_window, selected)
            _register_tool_taskbar(main_window, "col", "COL", get_col_workshop_icon, "COL Workshop", w)
            main_window.log_message(f"COL Workshop opened: {os.path.basename(selected)}")
            return

        # Priority 2: IMG table .col entry highlighted — extract and open directly
        from apps.methods.export_shared import get_active_table
        entries_table = get_active_table(main_window)
        selected_items = entries_table.selectedItems() if entries_table else []
        if selected_items:
            row = selected_items[0].row()
            item0 = entries_table.item(row, 0)
            filename = item0.text() if item0 else ""
            if filename.lower().endswith('.col'):
                img = getattr(main_window, 'current_img', None)
                if img and hasattr(img, 'entries') and 0 <= row < len(img.entries):
                    entry = img.entries[row]
                    try:
                        import tempfile
                        data = img.read_entry_data(entry)
                        if data:
                            tmp = tempfile.NamedTemporaryFile(
                                delete=False, suffix='.col',
                                prefix=os.path.splitext(filename)[0] + '_')
                            tmp.write(data)
                            tmp.close()
                            from apps.components.Col_Editor.col_workshop import open_col_workshop
                            w = open_col_workshop(main_window, tmp.name)
                            _register_tool_taskbar(main_window, "col", "COL",
                                get_col_workshop_icon, "COL Workshop", w)
                            main_window.log_message(f"COL Workshop: {filename}")
                            return
                    except Exception:
                        pass
                # Fallback: browse mode with IMG path
                img_path = img.file_path if img else None
                from apps.components.Col_Editor.col_workshop import open_col_workshop
                w = open_col_workshop(main_window, img_path)
                _register_tool_taskbar(main_window, "col", "COL", get_col_workshop_icon, "COL Workshop", w)
                main_window.log_message(f"COL Workshop opened for: {filename}")
                return

        # Priority 3: Nothing selected — open against current IMG (browse mode)
        from apps.components.Col_Editor.col_workshop import open_col_workshop
        img_path = None
        if hasattr(main_window, 'current_img') and main_window.current_img:
            img_path = main_window.current_img.file_path
        w = open_col_workshop(main_window, img_path)
        _register_tool_taskbar(main_window, "col", "COL", get_col_workshop_icon, "COL Workshop", w)
        if img_path:
            main_window.log_message(f"COL Workshop opened (browse mode): {os.path.basename(img_path)}")
        else:
            main_window.log_message("COL Workshop opened (no file loaded)")

    except Exception as e:
        main_window.log_message(f"Error opening COL Workshop: {e}")


class IMGFactoryGUILayout:
    """Handles the complete GUI layout for IMG Factory 1.6 with theme system"""
    
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
            # Nav Operations - Keep hidden unless custom theme requires these 3 buttons.
            'filelistwindow': lambda: _switch_to_file_entries(self.main_window),
            'switch_to_dirlist': lambda: _switch_to_directory_tree(self.main_window),
            'switch_to_search': lambda: _switch_to_search(self.main_window),

            # IMG/COL Operations
            'create_new_img': lambda: create_new_img(self.main_window),
            'open_img_file': lambda: open_file_dialog(self.main_window),
            'open_multiple_files': lambda: open_file_dialog(self.main_window),
            'hybrid_load': lambda: getattr(self.main_window, 'open_hybrid_load', lambda: None)(),
            'scan_img_folder': lambda: getattr(self.main_window, 'scan_img_folder', lambda: None)(),
            'scan_img_recent': lambda: getattr(self.main_window, 'scan_img_recent', lambda: None)(),
            'reload_table': lambda: reload_current_file(self.main_window),
            'encrypt_img': lambda: getattr(self.main_window, 'encrypt_img', lambda: None)(),
            'close_img_file': lambda: close_img_file(self.main_window),
            'close_all_img': lambda: close_all_img(self.main_window),
            'rebuild_img': lambda: rebuild_current_img_native(self.main_window),
            'rebuild_all_img': lambda: rebuild_all_open_tabs(self.main_window),
            'save_img_entry': lambda: self.main_window.save_img_entry(),
            'merge_img': lambda: merge_img_function(self.main_window),
            'split_img': lambda: split_img(self.main_window),
            'split_img_via': lambda: split_img_via(self.main_window),
            'convert_img_format': lambda: convert_img_format(self.main_window),

            # Import methods
            'import_files': lambda: import_files_function(self.main_window),
            'import_files_via': lambda: import_via_function(self.main_window),
            'refresh_table': lambda: refresh_table(self.main_window),

            # Export methods
            'export_selected': lambda: self.main_window.export_selected(),
            'export_selected_via': lambda: self.main_window.export_via(),
            'quick_export_selected': lambda: self.main_window.quick_export(),
            'edit_txd_file': lambda: edit_txd_file(self.main_window),
            'dump_entries': lambda: self.main_window.dump_all(),

            # Remove methods
            'remove_selected': lambda: remove_selected_function(self.main_window),
            'remove_via_entries': lambda: remove_via_entries_function(self.main_window),

            # Selection methods
            'select_all_entries': lambda: self.select_all_entries(),
            'select_inverse': lambda: self.select_inverse(),
            'show_search_dialog': lambda: self.show_search_dialog(),
            'sort_entries': lambda: self.sort_entries(),
            'move_entry_up':   lambda: getattr(self.main_window, '_move_entries_up',   lambda: None)(),
            'open_ide_editor': lambda: getattr(self.main_window, 'open_ide_editor',    lambda: None)(),
            'toggle_dir_tree': lambda: getattr(self.main_window, 'toggle_dir_tree',    lambda: None)(),
            'move_entry_down': lambda: getattr(self.main_window, '_move_entries_down', lambda: None)(),

            # Removed, TODO need to choose the ide file, to sort with.
            #'sort_entries_to_match_ide': lambda: self.sort_entries_to_match_ide(),
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
            'edit_ide_file': lambda: getattr(self.main_window, 'open_ide_editor_docked', lambda: getattr(self.main_window, 'open_ide_editor', lambda: None)())(),
            'edit_ipl_file': lambda: self._open_selected_text_file('.ipl'),
            'edit_dat_file': self._open_dat_browser,
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

    def _open_dat_browser(self): #vers 2
        """Open (or re-open) the DAT Browser tab — wired to the 'Dat Edit' button."""
        try:
            from apps.components.Dat_Browser.dat_browser import show_dat_browser
            show_dat_browser(self.main_window)
            # Register with the actual widget as target so taskbar can raise it
            widget = getattr(self.main_window, 'dat_browser', None)
            _register_tool_taskbar(self.main_window, 'dat', 'DAT',
                get_dat_browser_icon, 'DAT Browser — game data editor',
                target=widget)
        except Exception as e:
            self._log_missing_method('edit_dat_file')

    def _open_selected_text_file(self, preferred_ext: str = None): #vers 1
        """Open a file from the dir tree selection in the text editor.
        Falls back to a QFileDialog filtered by preferred_ext if nothing is selected."""
        try:
            from apps.core.notepad import open_text_file_in_editor

            # Try dir-tree current selection first
            file_path = self._get_dir_tree_selected_file()
            if file_path and os.path.isfile(file_path):
                ext = os.path.splitext(file_path)[1].lower()
                editable = (".ide", ".ipl", ".dat", ".txt", ".cfg", ".ini",
                            ".zon", ".cut", ".fxt")
                if ext in editable:
                    open_text_file_in_editor(file_path, self.main_window)
                    return

            # Nothing useful selected — open file dialog
            from PyQt6.QtWidgets import QFileDialog
            ext_label = preferred_ext.upper().lstrip('.') if preferred_ext else "Text"
            ext_filter = f"{ext_label} Files (*{preferred_ext});;All Files (*)" \
                         if preferred_ext else "Text Files (*.ide *.ipl *.dat *.txt *.cfg *.ini);;All Files (*)"
            start_dir = getattr(self.main_window, "game_root", "") or os.path.expanduser("~")
            path, _ = QFileDialog.getOpenFileName(
                None, f"Open {ext_label} File", start_dir, ext_filter)
            if path:
                open_text_file_in_editor(path, self.main_window)
        except Exception as e:
            if hasattr(self.main_window, "log_message"):
                self.main_window.log_message(f"Text editor open error: {e}")

    def _get_dir_tree_selected_file(self) -> str: #vers 1
        """Return the currently selected file path in the dir tree, or ''."""
        mw = self.main_window
        # Check _dir_tree_selected_file attr first (set by dir list)
        path = getattr(mw, "_dir_tree_selected_file", None)
        if path and os.path.isfile(path):
            return path
        # Check directory_tree widget selection
        dt = getattr(mw, "directory_tree", None)
        if dt:
            try:
                items = dt.tree.selectedItems()
                if items:
                    p = items[0].data(0, Qt.ItemDataRole.UserRole)
                    if p and os.path.isfile(p):
                        return p
            except Exception:
                pass
        return ""

    def _dat_edit_context_menu(self, btn, pos): #vers 1
        """Right-click context menu on the 'Dat Edit' button."""
        from PyQt6.QtWidgets import QMenu
        menu = QMenu(btn)

        open_act = menu.addAction("Open DAT Browser")
        open_act.triggered.connect(self._open_dat_browser)

        menu.addSeparator()

        set_root_act = menu.addAction("Set Game Root from Dir Tree")
        set_root_act.triggered.connect(self._set_dat_game_root_from_dir_tree)

        # Show what root is currently set (greyed info item)
        root = getattr(self.main_window, 'game_root', None)
        if root:
            info = menu.addAction(f"  Current: {root}")
            info.setEnabled(False)

        menu.exec(btn.mapToGlobal(pos))

    def _set_dat_game_root_from_dir_tree(self): #vers 1
        """Push the dir-tree current path into the DAT Browser as game root."""
        try:
            from apps.components.Dat_Browser.dat_browser import set_game_root_from_dir_tree
            set_game_root_from_dir_tree(self.main_window)
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Set game root error: {e}")



    def _get_button_theme_template(self, theme_name="default"): #vers 2
        """Get button color templates based on theme"""
        if self._is_dark_theme():
            return {
                # Dark Theme Button Colors
                'filelistwindow': '#3D4A5F',    # Light blue for open/load actions
                'dirlistwindow': '#3D4A5F',     # Light blue for open/load actions
                'filesearch': '#3D4A5F',        # Light blue for open/load actions
                'create_action': '#3D5A5A',     # Dark teal for create/new actions
                'open_action': '#3D4A5F',       # Dark blue for open/load actions
                'reload_action': '#2D4A3A',     # Dark green for refresh/reload
                'close_action': '#5A4A3D',      # Dark orange for close actions
                'build_action': '#2D4A3A',      # Dark mint for build/rebuild
                'save_action': '#4A2D4A',       # Dark purple for save actions
                'merge_action': '#3A2D4A',      # Dark violet for merge/split
                'split_action': '#3A2D4A',      # Dark violet for merge/split
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
                'filelistwindow': '#E3F2FD',    # Light blue for open/load actions
                'dirlistwindow': '#E3F2FD',     # Light blue for open/load actions
                'filesearch': '#E3F2FD',        # Light blue for open/load actions
                'create_action': '#EEFAFA',     # Light teal for create/new actions
                'open_action': '#E3F2FD',       # Light blue for open/load actions
                'reload_action': '#E8F5E8',     # Light green for refresh/reload
                'close_action': '#FFF3E0',      # Light orange for close actions
                'build_action': '#E8F5E8',      # Light mint for build/rebuild
                'save_action': '#F8BBD9',       # Light pink for save actions
                'merge_action': '#F3E5F5',      # Light violet for merge/split
                'split_action': '#F3E5F5',      # Light violet for merge/split
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

    def _get_nav_buttons_data(self): #vers 1
        """Get IMG buttons data with theme colors"""
        colors = self._get_button_theme_template()
            # TODO Only show on gui_layout.py but hidden on gui_layout_custom.py
        return [
            ("File List", "filelist", "doc-filelist", colors['filelistwindow'], "switch_to_img_file"),
            ("Merge View", "dirtree", "doc-dirtree", colors['dirtree'], "switch_to_dirlist"),
            ("Search", "search", "file-search", colors['filesearch'], "switch_to_search"),
        ]


    def _get_img_buttons_data(self): #vers 4
        """Get IMG buttons data with theme colors"""
        colors = self._get_button_theme_template()
        return [
            ("Create",       "new",          "document-new",    colors['create_action'],  "create_new_img"),
            ("Open",         "open",         "document-open",   colors['open_action'],    "open_img_file"),
            ("Hybrid Load",  "hybrid_load",  "hybrid-load",     colors['open_action'],    "hybrid_load"),
            ("Scan Folder",  "scan_folder",  "scan-folder",     colors['open_action'],    "scan_img_folder"),
            ("Recent Scans", "recent_scans", "recent-scans",    colors['open_action'],    "scan_img_recent"),
            ("Reload",       "reload",       "document-reload", colors['reload_action'],  "reload_table"),
            ("Encrypt",      "encrypt",      "encrypt",         colors['build_action'],   "encrypt_img"),
            ("Close",        "close",        "window-close",    colors['close_action'],   "close_img_file"),
            ("Close All",    "close_all",    "window-close-all",colors['close_action'],   "close_all_img"),
            ("Rebuild",      "rebuild",      "view-rebuild",    colors['build_action'],   "rebuild_img"),
            ("Rebuild All",  "rebuild_all",  "rebuild-all",     colors['build_action'],   "rebuild_all_img"),
            ("Save Entry",   "save_entry",   "document-save-entry", colors['save_action'],"save_img_entry"),
            ("Merge",        "merge",        "merge",           colors['merge_action'],   "merge_img"),
            ("Split via",    "split",        "split",           colors['split_action'],   "split_img_via"),
            ("Convert",      "convert",      "convert",         colors['convert_action'], "convert_img_format"),
        ]


    def _get_entry_buttons_data(self): #vers 3
        """Get Entry buttons data with theme colors"""
        colors = self._get_button_theme_template()
        return [
            ("Import", "import", "document-import", colors['import_action'], "import_files"),
            ("Import via", "import_via", "import_via", colors['import_action'], "import_files_via"),
            ("Refresh", "update", "view-refresh", colors['reload_action'], "refresh_table"),
            ("Export", "export", "document-export", colors['export_action'], "export_selected"),
            ("Export via", "export_via", "export_via", colors['export_action'], "export_selected_via"),
            ("Dump", "dump", "document-dump", colors['merge_action'], "dump_entries"),
            #("Quick Exp", "quick_export", "document-send", colors['export_action'], "quick_export_selected"),
            ("Remove", "remove", "edit-delete", colors['remove_action'], "remove_selected"),
            ("Remove via", "remove_via", "document-remvia", colors['remove_action'], "remove_via_entries"),
            ("Extract", "extract", "document-export", colors['export_action'], "extract_textures"),
            ("Rename", "rename", "edit-rename", colors['edit_action'], "rename_selected"),
            ("Select All", "select_all", "edit-select-all", colors['select_action'], "select_all_entries"),
            ("Inverse", "sel_inverse", "edit-select", colors['select_action'], "select_inverse"),
            
            ("Sort via", "sort", "view-sort", colors['select_action'], "sort_entries"),
            ("Pin selected", "pin_selected", "pin", colors['select_action'], "pin_selected_entries"),
        ]


    def _get_options_buttons_data(self): #vers 3
        """Get Options buttons data with theme colors"""
        colors = self._get_button_theme_template()
        return [
            ("Dir Tree", "dir_tree", "dir-tree", colors['editor_data'], "toggle_dir_tree"),
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


    def refresh_icons(self, color: str): #vers 2
        """Refresh all toolbar SVG icons using the given color (text_primary from theme)"""
        try:
            from apps.methods.imgfactory_svg_icons import (
                get_twin_panel_icon, get_layout_w1left_icon,
                get_single_panel_icon, get_view_icon, SVGIconFactory
            )
            if hasattr(self, 'f_entries_btn'):
                self.f_entries_btn.setIcon(get_twin_panel_icon(20, color))
            if hasattr(self, 'split_toggle_btn'):
                self.split_toggle_btn.setIcon(get_layout_w1left_icon(20, color))
            if hasattr(self, 'search_btn'):
                self.search_btn.setIcon(get_single_panel_icon(20, color))
            if hasattr(self, 'log_btn'):
                self.log_btn.setIcon(get_view_icon(20, color))
            if hasattr(self, 'rw_scan_btn'):
                self.rw_scan_btn.setIcon(SVGIconFactory.rw_scan_icon(20, color))
                self.rw_scan_btn.setIconSize(__import__('PyQt6.QtCore', fromlist=['QSize']).QSize(18, 18))
        except Exception as e:
            print(f"refresh_icons failed: {e}")

    def set_theme_mode(self, theme_name): #vers 3
        """Set the current theme mode and refresh all styling"""
        self.theme_mode = 'dark' if 'dark' in theme_name.lower() else 'light'
        print(f"Theme mode set to: {self.theme_mode}")

        # Get text_primary from theme and refresh icons
        if hasattr(self, 'main_window') and hasattr(self.main_window, 'app_settings'):
            theme_colors = self.main_window.app_settings.get_theme_colors(theme_name)
            if theme_colors:
                icon_color = theme_colors.get('text_primary', '#000000')
                from apps.methods.imgfactory_svg_icons import SVGIconFactory
                SVGIconFactory.set_theme_color(icon_color)
                self.refresh_icons(icon_color)

        # Force refresh all buttons with new theme colors
        self._refresh_all_buttons()
        
        # Apply all window themes
        self.apply_all_window_themes()


    def _setup_tearoff_button_for_tabs(self): #vers 2
        """Setup tearoff button for the file window - no tabs version"""
        try:
            # Create tearoff button with square arrow icon
            self.tearoff_button = QPushButton("⧉")  # Square with arrow symbol
            self.tearoff_button.setFixedSize(24, 24)
            self.tearoff_button.setToolTip("Tear off file window to separate window")

            # Apply theme-aware styling
            self._apply_tearoff_button_theme()

            # Connect to tearoff handler
            self.tearoff_button.clicked.connect(self._handle_file_window_tearoff)

            # Since there are no tabs anymore, we'll add the button differently
            # We'll place it as part of the file window itself
            if hasattr(self, 'table'):
                # Add button to a layout near the table
                pass  # Skip for now since we're not using tabs

            self.main_window.log_message("Tearoff button setup for file window")

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


    def _handle_file_window_tearoff(self): #vers 1
        """Handle tearoff button click for file window - no tabs version"""
        try:
            # Since there are no tabs, we'll just log for now
            # The functionality will be adapted for the table/widget instead
            self.main_window.log_message("File window tear-off functionality needs implementation")
        except Exception as e:
            self.main_window.log_message(f"Error handling file window tearoff: {str(e)}")


    def _dock_tab_widget_back(self): #vers 2
        """Dock torn off tab widget back to main window """
        try:
            # First check if tab_widget exists
            if not self.tab_widget:
                self.main_window.log_message("No tab widget to dock back")
                return
                
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
            if tearoff_panel and self.tab_widget:
                try:
                    tearoff_panel_layout = tearoff_panel.layout()
                    if tearoff_panel_layout:
                        tearoff_panel_layout.removeWidget(self.tab_widget)
                    tearoff_panel.hide()
                    tearoff_panel.deleteLater()
                except Exception as e:
                    self.main_window.log_message(f"Error cleaning up tearoff panel: {str(e)}")

            # Add back to original parent layout
            if self.tab_widget:
                try:
                    original_layout.addWidget(self.tab_widget)
                except Exception as e:
                    self.main_window.log_message(f"Error adding back to original layout: {str(e)}")
                    return

            # Clean up references
            if self.tab_widget:
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
            if self.tab_widget:
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


    def launch_txd_editor(self): #vers 4
        """Launch TXD Workshop - uses dir tree selected file, current IMG, or file dialog"""
        try:
            from apps.components.Txd_Editor.txd_workshop import open_txd_workshop

            # Priority 1: stored selection from single-click
            selected = getattr(self.main_window, '_dir_tree_selected_file', None)

            # Priority 2: read currentItem directly from tree (catches keyboard nav / highlight without click)
            if not selected and hasattr(self.main_window, 'directory_tree'):
                tree = self.main_window.directory_tree
                for attr in ('tree', '_second_tree'):
                    t = getattr(tree, attr, None)
                    if t:
                        item = t.currentItem()
                        if item:
                            from PyQt6.QtCore import Qt
                            path = item.data(0, Qt.ItemDataRole.UserRole)
                            if path and os.path.isfile(path):
                                selected = path
                                break

            if selected and selected.lower().endswith('.txd') and os.path.isfile(selected):
                if hasattr(self.main_window, '_load_txd_file_in_new_tab'):
                    self.main_window._load_txd_file_in_new_tab(selected)
                else:
                    open_txd_workshop(self.main_window, selected)
                self.main_window.log_message(f"TXD Workshop opened: {os.path.basename(selected)}")
                return

            # Priority 3: current IMG
            img_path = None
            if hasattr(self.main_window, 'current_img') and self.main_window.current_img:
                img_path = self.main_window.current_img.file_path

            workshop = open_txd_workshop(self.main_window, img_path)
            if workshop:
                self.main_window.log_message("TXD Workshop opened")
            else:
                self.main_window.log_message("Failed to open TXD Workshop")

        except Exception as e:
            self.main_window.log_message(f"Failed to launch TXD Workshop: {e}")


    def _update_all_buttons_display_mode(self):
        """Update all buttons to reflect the current display mode"""
        try:
            all_buttons = []
            if hasattr(self, 'img_buttons'):
                all_buttons.extend(self.img_buttons)
            if hasattr(self, 'entry_buttons'):
                all_buttons.extend(self.entry_buttons)
            if hasattr(self, 'options_buttons'):
                all_buttons.extend(self.options_buttons)

            parents = set()
            for btn in all_buttons:
                self._update_button_display_mode(btn)
                if btn.parentWidget():
                    parents.add(btn.parentWidget())

            # Force parent layouts to reflow
            for parent in parents:
                if parent.layout():
                    parent.layout().activate()
                parent.update()
                
        except Exception as e:
            print(f"Error updating all buttons display mode: {e}")


    def _update_button_display_mode(self, btn): #vers 2
        """Update a single button display mode - text only at full width, icons via splitter"""
        try:
            mode = getattr(self, 'button_display_mode', 'text_only')

            if mode == 'icons_only':
                # Handled by _set_right_panel_icon_only
                pass
            else:
                # text_only or icons_with_text both show text at full width
                label = btn.property("full_label") or (btn.localized_text if hasattr(btn, 'localized_text') else btn.text())
                if label:
                    btn.setText(label)
                btn.setIcon(QIcon())
                btn.setMinimumSize(0, 0)
                btn.setMaximumSize(16777215, 16777215)
                btn.setMinimumHeight(22)

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

        # Store icon on button for icon-only mode, but default to text-only
        is_dark_theme = self._is_dark_theme()
        icon_obj = self._get_svg_icon(icon, is_dark_theme, bg_color=button_bg)
        if icon_obj:
            btn.setProperty("stored_icon", icon_obj)
            btn.setProperty("stored_bg", button_bg)
        # No icon shown at full width - text only

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
        btn.setProperty("full_label", localized_label)

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


    def _get_svg_icon(self, icon_name: str, is_dark_theme: bool = False, bg_color: str = None) -> QIcon: #vers 2
        """Get SVG icon based on icon name identifier"""
        icon_map = {
            # IMG file actions
            "new": get_add_icon,            "document-new": get_add_icon,
            "open": get_open_icon,          "document-open": get_open_icon,
            "reload": get_refresh_icon,     "document-reload": get_refresh_icon,
            "rebuild": get_rebuild_icon,    "view-rebuild": get_rebuild_icon,
            "rebuild-all": get_rebuild_all_icon,
            "close": get_close_icon,        "window-close": get_close_icon,
            "window-close-all": get_close_all_icon,
            "save_entry": get_save_icon,    "document-save": get_save_icon,
            "document-save-entry": get_save_icon,
            "merge": get_merge_icon,        "document-merge": get_merge_icon,
            "split": get_split_icon,
            "convert": get_convert_icon,

            # File entry actions
            "import": get_import_icon,      "document-import": get_import_icon,
            "import_via": get_import_via_icon,
            "export": get_export_icon,      "document-export": get_export_icon,
            "export_via": get_export_via_icon,
            "dump": get_dump_icon,          "document-dump": get_dump_icon,
            "remove": get_remove_icon,      "edit-delete": get_remove_icon,
            "remove_via": get_remove_via_icon, "document-remvia": get_remove_via_icon,
            "extract": get_extract_icon,
            "rename": get_edit_icon,        "edit-rename": get_edit_icon,
            "select_all": get_select_all_icon, "edit-select-all": get_select_all_icon,
            "sel_inverse": get_select_inverse_icon, "edit-select": get_select_inverse_icon,
            "sort": get_sort_icon,          "view-sort": get_sort_icon,
            "pin_selected": get_pin_icon,   "pin": get_pin_icon,

            # Workshop editors
            "col-edit": get_col_workshop_icon,
            "txd-edit": get_txd_workshop_icon,
            "hybrid-load": get_hybrid_load_icon,
            "scan-folder": get_scan_folder_icon,
            "recent-scans": get_recent_scans_icon,
            "tba": get_tba_icon,
            "dat-browser": get_dat_browser_icon,
            "ide-editor": get_ide_editor_icon,

            # Other editor buttons - TBA where not yet implemented
            "dff-edit": get_tba_icon,       "ipf-edit": get_tba_icon,
            "ide-edit": get_edit_icon,      "ipl-edit": get_edit_icon,
            "dat-edit": get_edit_icon,
            "zones-cull": get_tba_icon,
            "weap-edit": get_tba_icon,
            "vehi-edit": get_tba_icon,
            "peds-edit": get_tba_icon,
            "radar-map": get_tba_icon,
            "paths-map": get_tba_icon,
            "timecyc": get_tba_icon,
            "handling": get_tba_icon,
            "ojs-breakble": get_tba_icon,
            "scm-code": get_tba_icon,
            "gxt-font": get_tba_icon,
            "menu-font": get_tba_icon,
        }

        fn = icon_map.get(icon_name, get_tba_icon)
        if fn is None:
            return QIcon()
        stroke_color = "#222222" if not is_dark_theme else "#dddddd"
        return fn(size=24, color=stroke_color, bg_color=bg_color) if bg_color else fn(size=24, color=stroke_color)


    def _get_short_text(self, label): #vers 1
        """Get short text for button"""
        # First get the localized version of the label
        localized_label = tr_button(label)
        
        short_map = {
            "Create": "New",
            "Open": "Open",
            "Reload": "Reload",
            "     ": " ",
            "Close": "Close",
            "Close All": "Close A",
            "Rebuild": "Rebld",
            "Rebuild All": "Rebld Al",
            "Save Entry": "Save",
            "Merge": "Merge",
            "Split Via": "Split",
            "Convert": "Conv",
            "Import": "Imp",
            "Import via": "Imp via",
            "Refresh": "Refresh", "Export": "Exp",
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


    def create_main_ui_with_splitters(self, main_layout): #vers 4

        """Create the main UI with 3-panel layout similar to COL Workshop"""
        # Create main horizontal splitter for 3 panels
        self.main_splitter = QSplitter(Qt.Orientation.Horizontal)
        
        # LEFT: File list panel (like COL Workshop) #disabled
        #left_panel = self._create_left_file_list_panel()
        
        # MIDDLE: File window (table with sub-tabs) - main content area
        middle_panel = self._create_middle_file_window_panel()
        
        # RIGHT: Control buttons with pastel colors
        right_panel = self.create_right_panel_with_pastel_buttons()
        
        # Add panels to splitter
        #self.main_splitter.addWidget(left_panel)
        self.main_splitter.addWidget(middle_panel)
        self.main_splitter.addWidget(right_panel)
        
        # Set splitter proportions (similar to COL Workshop: 2:3:2 ratio)
        self.main_splitter.setSizes([200, 700, 280])  # Left: 200px, Middle: 700px, Right: 280px
        
        # Add size constraints
        #left_panel.setMaximumWidth(400)  # Max 400px
        #left_panel.setMinimumWidth(150)  # Min 150px
        right_panel.setMaximumWidth(350)  # Max 350px
        right_panel.setMinimumWidth(200)  # Min 200px
        
        # Style the main horizontal splitter handle with theme colors
        self._apply_main_splitter_theme()
        
        # Allow all panels to be collapsible so user can adjust
        self.main_splitter.setCollapsible(0, True)  # Left panel
        self.main_splitter.setCollapsible(1, True)  # Middle panel
        self.main_splitter.setCollapsible(2, True)  # Right panel

        # Adapt right panel on resize
        self.main_splitter.splitterMoved.connect(self._on_main_splitter_moved)
        self._right_panel_icon_only = False

        # Add splitter to main layout
        main_layout.addWidget(self.main_splitter)


    def _create_left_file_list_panel_disabled(self): #vers 2
        """Create left panel for file list showing files in the same directory as the loaded IMG file (similar to COL Workshop left panel)"""
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(150)
        panel.setMaximumWidth(400)

        layout = QVBoxLayout(panel)
        layout.setContentsMargins(5, 5, 5, 5)

        # Header with refresh button
        header_layout = QHBoxLayout()
        header = QLabel("Directory Files")
        header.setFont(QFont("Arial", 10, QFont.Weight.Bold))
        header_layout.addWidget(header)
        
        # Refresh button to update the file list
        refresh_button = QPushButton("Refresh")
        refresh_button.setFixedSize(60, 20)
        refresh_button.clicked.connect(self.refresh_directory_files)
        header_layout.addWidget(refresh_button)
        header_layout.addStretch()  # Add stretch to push refresh button to the right
        
        layout.addLayout(header_layout)

        # Create a list widget for files in the same directory
        self.directory_files_list = QListWidget()
        self.directory_files_list.setAlternatingRowColors(True)

        # Connect to a function to handle file selection
        self.directory_files_list.itemClicked.connect(self._on_directory_file_selected)
        self.directory_files_list.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.directory_files_list.customContextMenuRequested.connect(self._on_directory_list_context_menu)
        layout.addWidget(self.directory_files_list)
        
        # Initially populate the list (will be empty until an IMG file is loaded)
        self.refresh_directory_files()
        return panel


    def _create_middle_file_window_panel(self): #vers 1
        """Create middle panel with file window (table with sub-tabs) - main content area"""
        middle_container = QWidget()
        middle_layout = QVBoxLayout(middle_container)
        middle_layout.setContentsMargins(3, 3, 3, 3)
        middle_layout.setSpacing(0)  # No spacing - splitter handles this

        # Create vertical splitter for the sections in middle panel
        self.middle_vertical_splitter = QSplitter(Qt.Orientation.Vertical)

        # 1. TOP: File Window (table with sub-tabs)
        self.file_window = self._create_file_window()
        self.middle_vertical_splitter.addWidget(self.file_window)

        # 2. BOTTOM: Status Window (log and status)
        status_window = self.create_status_window()
        self.middle_vertical_splitter.addWidget(status_window)

        # Set section proportions: File(760px), Status(200px)
        self.middle_vertical_splitter.setSizes([760, 200])

        # Prevent sections from collapsing completely
        self.middle_vertical_splitter.setCollapsible(0, True)  # File window
        self.middle_vertical_splitter.setCollapsible(1, True)  # Status window

        # Apply theme styling to vertical splitter
        self._apply_vertical_splitter_theme()

        middle_layout.addWidget(self.middle_vertical_splitter)
        return middle_container


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


    def _create_file_window(self): #vers 5
        """Create file window with table as main content (tabs moved to toolbar)"""
        file_window = QWidget()
        file_layout = QVBoxLayout(file_window)
        file_layout.setContentsMargins(5, 5, 5, 5)
        file_layout.setSpacing(3)

        # Splitter: left = stacked panel (dir tree / dat browser), right = main tab widget
        self.content_splitter = QSplitter(Qt.Orientation.Horizontal)
        self.content_splitter.setAutoFillBackground(True)

        # Left stacked panel — page 0: dir tree, page 1: DAT browser
        from PyQt6.QtWidgets import QStackedWidget
        self.left_stack = QStackedWidget()
        self.left_stack.setAutoFillBackground(True)
        self.left_stack.setMinimumWidth(0)
        # Placeholder pages — real widgets inserted when panels first open
        self._left_stack_placeholder = QWidget()  # page 0 dir tree slot
        self._left_stack_dat_placeholder = QWidget()  # page 1 dat browser slot
        self.left_stack.addWidget(self._left_stack_placeholder)   # page 0
        self.left_stack.addWidget(self._left_stack_dat_placeholder)  # page 1
        self.left_stack.hide()  # hidden until Dir or DAT opened
        self.content_splitter.addWidget(self.left_stack)

        # Create main table placeholder (replaced by main_tab_widget in _create_ui)
        from apps.methods.populate_img_table import DragSelectTableWidget
        self.table = DragSelectTableWidget()
        self.table.setAlternatingRowColors(True)
        self.table.setSortingEnabled(True)
        self._apply_table_theme_styling()

        self.content_splitter.addWidget(self.table)
        self.content_splitter.setStretchFactor(0, 0)
        self.content_splitter.setStretchFactor(1, 1)
        file_layout.addWidget(self.content_splitter)

        self._apply_file_list_window_theme_styling()
        self._setup_tearoff_button_for_tabs()

        # Ctrl+Up / Ctrl+Down to reorder entries
        from PyQt6.QtGui import QKeySequence, QShortcut
        _sc_up = QShortcut(QKeySequence("Ctrl+Up"), file_window)
        _sc_up.activated.connect(
            lambda: getattr(self.main_window, '_move_entries_up', lambda: None)())
        _sc_dn = QShortcut(QKeySequence("Ctrl+Down"), file_window)
        _sc_dn.activated.connect(
            lambda: getattr(self.main_window, '_move_entries_down', lambda: None)())

        return file_window


    # Updated function for gui_layout.py - Replace existing create_right_panel_with_pastel_buttons

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
            # Wire right-click context menu for the Dat Edit button
            if action_type == "dat_edit":
                btn.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
                btn.customContextMenuRequested.connect(
                    lambda pos, b=btn: self._dat_edit_context_menu(b, pos))
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


    def set_button_display_mode(self, mode: str):
        """
        Set button display mode: 'text_only', 'icons_only', or 'icons_with_text'
        """
        try:
            self.button_display_mode = mode
            self._update_all_buttons_display_mode()

            # Also update via backend if available
            if hasattr(self, 'backend'):
                if mode == "text_only":
                    display_mode = ButtonDisplayMode.TEXT_ONLY
                elif mode == "icons_only":
                    display_mode = ButtonDisplayMode.ICONS_ONLY
                elif mode == "icons_with_text":
                    display_mode = ButtonDisplayMode.ICONS_WITH_TEXT
                else:
                    display_mode = ButtonDisplayMode.ICONS_WITH_TEXT
                self.backend.set_button_display_mode(display_mode)

            # Force Qt to reflow and repaint
            if hasattr(self, 'main_window') and self.main_window:
                self.main_window.update()
                self.main_window.repaint()
                from PyQt6.QtWidgets import QApplication
                QApplication.processEvents()

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
        icon_size = settings.get('icon_size', 24)
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


    def create_status_window(self): #vers 3
        """Create toolbar - ALL BUTTONS CONNECTED"""
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        from PyQt6.QtGui import QFont

        if not hasattr(self, 'title_font'):
            self.title_font = QFont("Arial", 14)
        if not hasattr(self, 'button_font'):
            self.button_font = QFont("Arial", 10)
        if not hasattr(self, 'icon_factory'):
            self.icon_factory = SVGIconFactory()
        if not hasattr(globals(), 'App_name'):
            from apps.components.Img_Factory.imgfactory import App_name

        # Get icon color from theme text_primary
        icon_color = '#000000'
        if hasattr(self, 'main_window') and hasattr(self.main_window, 'app_settings'):
            current_theme = self.main_window.app_settings.current_settings.get("theme", "default")
            theme_colors = self.main_window.app_settings.get_theme_colors(current_theme)
            if theme_colors:
                icon_color = theme_colors.get('text_primary', '#000000')
        status_container = QWidget()
        status_layout = QVBoxLayout(status_container)
        status_layout.setContentsMargins(5, 5, 5, 5)
        status_layout.setSpacing(2)

        # Header with buttons
        header_layout = QHBoxLayout()
        header = QLabel("Activity Logs")
        header.setFont(QFont("Arial", 10, QFont.Weight.Bold))
        header_layout.addWidget(header)
        header_layout.addStretch()

        # File Entries button - icon only
        # Import icons
        from apps.methods.imgfactory_svg_icons import (
            get_twin_panel_icon, get_split_horizontal_icon,
            get_single_panel_icon, get_search_icon, get_view_icon,
            get_layout_w1left_icon
        )

        self.f_entries_btn = QPushButton()
        from apps.methods.imgfactory_svg_icons import get_tree_icon
        self.f_entries_btn.setIcon(get_tree_icon(20))
        self.f_entries_btn.setFixedSize(24, 24)
        self.f_entries_btn.setIconSize(QSize(20, 20))
        self.f_entries_btn.setToolTip("Directory Tree")
        self.f_entries_btn.clicked.connect(lambda: self._switch_to_directory_tree() if hasattr(self, '_switch_to_directory_tree') else None)
        header_layout.addWidget(self.f_entries_btn)

        # Merge View toggle
        self._merge_view_horizontal = False
        self.split_toggle_btn = QPushButton()
        self.split_toggle_btn.setFixedSize(24, 24)
        self.split_toggle_btn.setIcon(get_layout_w1left_icon(20))
        self.split_toggle_btn.setIconSize(QSize(20, 20))
        self.split_toggle_btn.setToolTip("Files left | Tree right → click: Files top / Tree bottom")
        self.split_toggle_btn.clicked.connect(self._toggle_merge_view_layout)
        header_layout.addWidget(self.split_toggle_btn)

        # Search button
        self.search_btn = QPushButton()
        self.search_btn.setFixedSize(24, 24)
        self.search_btn.setIcon(get_single_panel_icon(20))
        self.search_btn.setIconSize(QSize(20, 20))
        self.search_btn.setToolTip("Search in files (Ctrl+F)")
        self.search_btn.clicked.connect(self._show_search)
        header_layout.addWidget(self.search_btn)

        # RW Scan button - rescan RW versions for current IMG
        self.rw_scan_btn = QPushButton()
        self.rw_scan_btn.setFixedSize(24, 24)
        self.rw_scan_btn.setToolTip("RW Version Scan — show detected RenderWare versions and rescan if needed")
        self.rw_scan_btn.setVisible(False)  # Hidden until an IMG tab is active
        self.rw_scan_btn.clicked.connect(self._show_rw_scan_dialog)
        header_layout.addWidget(self.rw_scan_btn)

        # RW scan button initial icon (uses same icon_color from top of create_status_window)
        try:
            self.rw_scan_btn.setIcon(SVGIconFactory.rw_scan_icon(20, icon_color))
            self.rw_scan_btn.setIconSize(QSize(18, 18))
        except Exception:
            pass

        # Show Logs button - icon only, shows log file content
        self.log_btn = QPushButton()
        self.log_btn.setFixedSize(24, 24)
        self.log_btn.setIcon(get_view_icon())
        self.log_btn.setIconSize(QSize(20, 20))
        self.log_btn.setToolTip("Show activity log file")
        self.log_btn.clicked.connect(self._show_log_file)
        header_layout.addWidget(self.log_btn)

        status_layout.addLayout(header_layout)

        # Connect tab changes to show/hide rw_scan_btn
        try:
            if hasattr(self, 'main_window') and hasattr(self.main_window, 'main_tab_widget'):
                self.main_window.main_tab_widget.currentChanged.connect(self._update_rw_btn_visibility)
        except Exception:
            pass

        # Activity log text area
        self.log = QTextEdit()
        self.log.setReadOnly(True)
        self.log.setMaximumHeight(180)
        self.log.setPlaceholderText("Activity log will appear here...")

        # Enable scrollbars
        self.log.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        self.log.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)

        # Apply theme styling to log
        self._apply_log_theme_styling()

        status_layout.addWidget(self.log)

        # CRITICAL: Assign to self.status_window for theme styling
        self.status_window = status_container

        # Apply theme styling to status window
        self._apply_status_window_theme_styling()

        return status_container


    def _switch_to_directory_tree(self): #vers 14
        """Cycle tree visibility: hidden → split → tree-full → hidden"""
        try:
            mw = self.main_window
            splitter = getattr(self, 'content_splitter', None)

            # First time setup
            if not hasattr(mw, '_dirtree_setup_complete') or not mw._dirtree_setup_complete:
                if not hasattr(mw, 'game_root') or not mw.game_root:
                    from PyQt6.QtWidgets import QMessageBox
                    reply = QMessageBox.question(
                        mw, "No Project Configured",
                        "No project is currently configured.\n\nOpen Project Manager?",
                        QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
                    )
                    if reply == QMessageBox.StandardButton.Yes:
                        from apps.components.Project_Manager.project_manager import show_project_manager_dialog
                        show_project_manager_dialog(mw)
                    return
                if not hasattr(mw, 'directory_tree'):
                    from apps.components.File_Editor.directory_tree_browser import integrate_directory_tree_browser
                    if not integrate_directory_tree_browser(mw):
                        mw.log_message("Failed to load Directory Tree")
                        return
                if splitter:
                    splitter.addWidget(mw.directory_tree)
                if hasattr(mw.directory_tree, 'browse_directory'):
                    mw.directory_tree.browse_directory(mw.game_root)
                mw._dirtree_setup_complete = True
                mw._dirtree_state = 0  # will advance below

            if not splitter or splitter.count() < 2:
                return

            total = sum(splitter.sizes()) or 10000
            # Cycle: 0=hidden → 1=split → 2=tree-full → 0=hidden
            state = (getattr(mw, '_dirtree_state', 0) + 1) % 3
            mw._dirtree_state = state

            tab_widget = getattr(mw, 'main_tab_widget', None)

            if state == 0:  # hidden - files full
                if tab_widget: tab_widget.show()
                splitter.setSizes([total, 0])
                mw.log_message("→ Files only")
            elif state == 1:  # split
                if tab_widget: tab_widget.show()
                splitter.setSizes([total // 2, total // 2])
                mw.log_message("→ Split view")
            elif state == 2:  # tree full
                if tab_widget: tab_widget.show()  # keep tabs visible
                splitter.setSizes([0, total])
                mw.log_message("→ Tree full")

        except Exception as e:
            self.main_window.log_message(f"Dir tree error: {str(e)}")
            import traceback
            traceback.print_exc()


    def _ensure_tab_0_for_directory_tree(self): #vers 1
        """Ensure Tab 0 exists and contains directory tree widget"""
        try:
            if not hasattr(self.main_window, 'main_tab_widget') or not self.main_window.main_tab_widget:
                return False

            tab_widget = self.main_window.main_tab_widget

            # Check if Tab 0 exists
            if tab_widget.count() == 0:
                # Create Tab 0
                from PyQt6.QtWidgets import QWidget, QVBoxLayout
                tab_0 = QWidget()
                tab_0_layout = QVBoxLayout(tab_0)
                tab_0_layout.setContentsMargins(0, 0, 0, 0)

                # Add directory tree if it exists
                if hasattr(self.main_window, 'directory_tree'):
                    tab_0_layout.addWidget(self.main_window.directory_tree)

                tab_widget.insertTab(0, tab_0, "Merge View")
                self.main_window.log_message("Created Tab 0 for Directory Tree")
                return True
            else:
                # Tab 0 exists - ensure it has directory tree
                tab_0 = tab_widget.widget(0)
                if tab_0 and hasattr(self.main_window, 'directory_tree'):
                    # Check if directory tree is already in tab 0
                    if self.main_window.directory_tree.parent() != tab_0:
                        layout = tab_0.layout()
                        if not layout:
                            from PyQt6.QtWidgets import QVBoxLayout
                            layout = QVBoxLayout(tab_0)
                            layout.setContentsMargins(0, 0, 0, 0)
                        layout.addWidget(self.main_window.directory_tree)

                    # Update tab label
                    tab_widget.setTabText(0, "Dir Tree")
                    return True
            return False
        except Exception as e:
            self.main_window.log_message(f"Error ensuring Tab 0: {str(e)}")
            return False


    def _show_search(self): #vers 1
        """Show search dialog"""
        try:
            if hasattr(self.main_window, 'search_manager'):
                self.main_window.search_manager.show_search_dialog()
            else:
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Search not available")
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Search error: {str(e)}")


    def _toggle_log_visibility(self): #vers 1
        """Toggle activity log visibility"""
        if hasattr(self, 'log'):
            self.log.setVisible(not self.log.isVisible())

    def _show_log_file(self): #vers 1
        """Show log file content in a dialog, or show in-app log if file logging disabled"""
        try:
            mw = self.main_window
            settings = getattr(mw, 'img_settings', None)
            log_to_file = settings.get("log_to_file", False) if settings else False

            if not log_to_file:
                # Just toggle the in-app log widget
                if hasattr(self, 'log'):
                    self.log.setVisible(not self.log.isVisible())
                return

            log_path = settings.get("log_file_path", "imgfactory_activity.log")

            import os
            if not os.path.exists(log_path):
                from PyQt6.QtWidgets import QMessageBox
                QMessageBox.information(mw, "Log File", f"No log file found at:\n{log_path}\n\nEnable 'Log to File' in Settings to start logging.")
                return

            with open(log_path, 'r') as f:
                content = f.read()

            from PyQt6.QtWidgets import QDialog, QVBoxLayout, QTextEdit, QPushButton, QHBoxLayout
            dlg = QDialog(mw)
            dlg.setWindowTitle("Activity Log")
            dlg.resize(800, 500)
            layout = QVBoxLayout(dlg)

            text = QTextEdit()
            text.setReadOnly(True)
            text.setPlainText(content)
            text.verticalScrollBar().setValue(text.verticalScrollBar().maximum())
            layout.addWidget(text)

            btn_layout = QHBoxLayout()
            clear_btn = QPushButton("Clear Log File")
            clear_btn.clicked.connect(lambda: (open(log_path, 'w').close(), text.clear()))
            close_btn = QPushButton("Close")
            close_btn.clicked.connect(dlg.accept)
            btn_layout.addWidget(clear_btn)
            btn_layout.addStretch()
            btn_layout.addWidget(close_btn)
            layout.addLayout(btn_layout)

            dlg.exec()

        except Exception as e:
            if hasattr(self, 'main_window'):
                self.main_window.log_message(f"Log file error: {str(e)}")



    def _update_rw_btn_visibility(self, index=None): #vers 1
        """Show RW scan button only when the active tab contains an IMG file."""
        try:
            if not hasattr(self, 'rw_scan_btn'):
                return
            mw = self.main_window
            if index is None:
                index = mw.main_tab_widget.currentIndex() if hasattr(mw, 'main_tab_widget') else -1
            tab_widget = mw.main_tab_widget.widget(index) if hasattr(mw, 'main_tab_widget') and index >= 0 else None
            file_type = getattr(tab_widget, 'file_type', None) if tab_widget else None
            img = getattr(tab_widget, 'file_object', None) if tab_widget else None
            is_img = (file_type == 'IMG') or (img is not None and hasattr(img, 'entries'))
            self.rw_scan_btn.setVisible(bool(is_img))
        except Exception:
            pass

    def _rescan_rw_versions(self, img_file, progress_cb=None): #vers 1
        """Aggressively rescan RW versions for all DFF/TXD entries.
        Reads 128 bytes per entry and probes all 4-byte-aligned offsets 0..64."""
        import struct as _s, os as _os

        def _is_valid(v):
            if not v: return False
            if 0x300 <= v <= 0x3FF: return True
            if 0x30000 <= v <= 0x3FFFF: return True
            if (v & 0xFFFF) == 0xFFFF and 0x0400 <= (v >> 16) <= 0x1C03: return True
            if v == 0x1C020037: return True
            return False

        def _scan128(data):
            for off in range(0, min(65, len(data) - 3), 4):
                v = _s.unpack_from('<I', data, off)[0]
                if _is_valid(v):
                    return v
            return None

        from apps.methods.rw_versions import get_rw_version_name

        data_file = img_file.file_path
        if data_file.lower().endswith('.dir'):
            data_file = data_file[:-4] + '.img'
        if not _os.path.exists(data_file):
            return 0, 0

        scanned = updated = 0
        try:
            with open(data_file, 'rb') as f:
                for i, entry in enumerate(img_file.entries):
                    if progress_cb and i % 50 == 0:
                        progress_cb(i)
                    ext = getattr(entry, 'extension', '').upper()
                    if ext not in ('DFF', 'TXD'):
                        continue
                    if entry.size == 0:
                        continue
                    scanned += 1
                    try:
                        f.seek(entry.offset)
                        data = f.read(min(128, entry.size))
                        v = _scan128(data)
                        if v:
                            entry.rw_version = v
                            entry.rw_version_name = get_rw_version_name(v)
                            entry._version_detected = True
                            updated += 1
                    except Exception:
                        continue
        except Exception:
            pass
        return scanned, updated

    def _show_rw_scan_dialog(self): #vers 1
        """Show RW version scan dialog: version frequency list + rescan + reload table."""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QLabel,
                                     QPushButton, QTableWidget, QTableWidgetItem,
                                     QHeaderView, QProgressBar, QAbstractItemView)
        from PyQt6.QtCore import Qt
        from PyQt6.QtGui import QColor, QPalette

        mw = self.main_window
        img_file = getattr(mw, 'current_img', None)

        pal = mw.palette()
        c_base    = pal.color(QPalette.ColorRole.Base).name()
        c_altbase = pal.color(QPalette.ColorRole.AlternateBase).name()
        c_text    = pal.color(QPalette.ColorRole.Text).name()
        c_hl      = pal.color(QPalette.ColorRole.Highlight).name()
        c_btn     = pal.color(QPalette.ColorRole.Button).name()
        c_btntext = pal.color(QPalette.ColorRole.ButtonText).name()

        dlg = QDialog(mw)
        dlg.setWindowTitle("RW Version Scan")
        dlg.resize(700, 480)
        dlg.setStyleSheet(
            f"QDialog {{ background: {c_base}; color: {c_text}; }}"
            f"QLabel {{ color: {c_text}; }}"
            f"QTableWidget {{ background: {c_base}; color: {c_text};"
            f"  gridline-color: {c_altbase}; border: 1px solid {c_altbase}; }}"
            f"QHeaderView::section {{ background: {c_altbase}; color: {c_text};"
            f"  padding: 4px; border: none; }}"
            f"QPushButton {{ background: {c_btn}; color: {c_btntext};"
            f"  border: 1px solid {c_altbase}; border-radius: 4px; padding: 4px 10px; }}"
            f"QPushButton:hover {{ background: {c_hl}; color: white; }}"
            f"QProgressBar {{ background: {c_altbase}; border: 1px solid {c_altbase};"
            f"  border-radius: 3px; text-align: center; color: {c_text}; }}"
            f"QProgressBar::chunk {{ background: {c_hl}; border-radius: 3px; }}"
        )

        layout = QVBoxLayout(dlg)

        if not img_file or not img_file.entries:
            layout.addWidget(QLabel("No IMG file loaded."))
            close = QPushButton("Close")
            close.clicked.connect(dlg.accept)
            layout.addWidget(close)
            dlg.exec()
            return

        fname = img_file.file_path.replace('\\', '/').split('/')[-1]
        ver_name = img_file.version.name if img_file.version else "UNKNOWN"
        total = len(img_file.entries)
        dff_txd = [e for e in img_file.entries if getattr(e,'extension','').upper() in ('DFF','TXD')]
        detected = [e for e in dff_txd if getattr(e,'rw_version',0) > 0]
        unknown  = [e for e in dff_txd if not getattr(e,'rw_version',0)]

        info = QLabel(
            f"<b>File:</b> {fname} &nbsp;|&nbsp; <b>Format:</b> {ver_name} &nbsp;|&nbsp; "
            f"<b>Entries:</b> {total} &nbsp;|&nbsp; "
            f"<b>DFF/TXD:</b> {len(dff_txd)} &nbsp;|&nbsp; "
            f"<b style=\'color:{c_hl}\'>Detected:</b> {len(detected)} &nbsp;|&nbsp; "
            f"<b style=\'color:orange\'>Unknown:</b> {len(unknown)}"
        )
        info.setTextFormat(Qt.TextFormat.RichText)
        info.setWordWrap(True)
        layout.addWidget(info)

        from collections import Counter

        def build_counts():
            counts = Counter()
            for e in dff_txd:
                rv = getattr(e, 'rw_version', 0)
                if rv:
                    name = getattr(e, 'rw_version_name', '') or f"0x{rv:08X}"
                    counts[f"RW {name}  [0x{rv:08X}]"] += 1
                else:
                    counts["Unknown / Not Detected"] += 1
            return counts

        def populate_table(tbl, counts):
            tbl.setRowCount(len(counts))
            for row, (ver_str, cnt) in enumerate(sorted(counts.items())):
                pct = f"{100*cnt/max(len(dff_txd),1):.1f}%"
                tbl.setItem(row, 0, QTableWidgetItem(ver_str))
                tbl.setItem(row, 1, QTableWidgetItem(str(cnt)))
                tbl.setItem(row, 2, QTableWidgetItem(pct))
                if "Unknown" in ver_str:
                    for col in range(3):
                        item = tbl.item(row, col)
                        if item:
                            item.setForeground(QColor("orange"))

        tbl = QTableWidget()
        tbl.setColumnCount(3)
        tbl.setHorizontalHeaderLabels(["RW Version", "Count", "% of DFF/TXD"])
        tbl.horizontalHeader().setSectionResizeMode(0, QHeaderView.ResizeMode.Stretch)
        tbl.horizontalHeader().setSectionResizeMode(1, QHeaderView.ResizeMode.ResizeToContents)
        tbl.horizontalHeader().setSectionResizeMode(2, QHeaderView.ResizeMode.ResizeToContents)
        tbl.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        tbl.setSelectionMode(QAbstractItemView.SelectionMode.SingleSelection)
        tbl.setAlternatingRowColors(True)
        populate_table(tbl, build_counts())
        layout.addWidget(tbl)

        progress = QProgressBar()
        progress.setMaximum(max(total, 1))
        progress.setValue(0)
        progress.setVisible(False)
        layout.addWidget(progress)

        status_lbl = QLabel("")
        status_lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(status_lbl)

        btn_row = QHBoxLayout()
        rescan_btn = QPushButton("Rescan RW Versions")
        rescan_btn.setToolTip("Re-read first 128 bytes of every DFF/TXD and probe all offsets")
        reload_btn = QPushButton("Reload Table")
        reload_btn.setEnabled(False)
        reload_btn.setToolTip("Repopulate the IMG table with updated RW version data")
        close_btn = QPushButton("Close")
        btn_row.addWidget(rescan_btn)
        btn_row.addWidget(reload_btn)
        btn_row.addStretch()
        btn_row.addWidget(close_btn)
        layout.addLayout(btn_row)

        def do_rescan():
            rescan_btn.setEnabled(False)
            progress.setVisible(True)
            progress.setValue(0)
            status_lbl.setText("Scanning...")
            dlg.repaint()

            scanned, updated = self._rescan_rw_versions(img_file, lambda i: progress.setValue(i) or dlg.repaint())
            progress.setValue(total)
            status_lbl.setText(f"Scanned {scanned} DFF/TXD entries — updated {updated} RW versions")
            reload_btn.setEnabled(True)
            rescan_btn.setEnabled(True)

            populate_table(tbl, build_counts())
            new_det = len([e for e in dff_txd if getattr(e,'rw_version',0) > 0])
            new_unk = len([e for e in dff_txd if not getattr(e,'rw_version',0)])
            info.setText(
                f"<b>File:</b> {fname} &nbsp;|&nbsp; <b>Format:</b> {ver_name} &nbsp;|&nbsp; "
                f"<b>Entries:</b> {total} &nbsp;|&nbsp; "
                f"<b>DFF/TXD:</b> {len(dff_txd)} &nbsp;|&nbsp; "
                f"<b style=\'color:{c_hl}\'>Detected:</b> {new_det} &nbsp;|&nbsp; "
                f"<b style=\'color:orange\'>Unknown:</b> {new_unk}"
            )

        def do_reload():
            try:
                from PyQt6.QtWidgets import QTableWidget as QTW
                tab_widget = mw.main_tab_widget.currentWidget()
                table = getattr(tab_widget, 'table_ref', None)
                if table is None and tab_widget:
                    tables = tab_widget.findChildren(QTW)
                    table = tables[0] if tables else None
                if table and hasattr(mw, '_populate_img_table_widget'):
                    mw._populate_img_table_widget(table, img_file)
                    status_lbl.setText("Table reloaded.")
                else:
                    status_lbl.setText("Could not find active table widget.")
            except Exception as ex:
                status_lbl.setText(f"Reload error: {ex}")

        rescan_btn.clicked.connect(do_rescan)
        reload_btn.clicked.connect(do_reload)
        close_btn.clicked.connect(dlg.accept)
        dlg.exec()

    def _toggle_merge_view_layout(self): #vers 4
        """Cycle left panel: left|right, top/bottom, right|left, hidden"""
        try:
            from apps.methods.imgfactory_svg_icons import (
                get_layout_w1left_icon, get_layout_w1top_icon,
                get_layout_w2left_icon, get_layout_w2top_icon
            )
            if not hasattr(self, 'content_splitter') or not hasattr(self, 'left_stack'):
                return

            splitter = self.content_splitter
            mw = self.main_window
            tab_widget = getattr(mw, 'main_tab_widget', None)

            self._merge_view_state = (getattr(self, '_merge_view_state', 0) + 1) % 4
            state = self._merge_view_state

            if state == 0:  # left stack left | tabs right
                splitter.setOrientation(Qt.Orientation.Horizontal)
                splitter.insertWidget(0, self.left_stack)
                if tab_widget: splitter.insertWidget(1, tab_widget)
                splitter.setSizes([350, 650])
                self.left_stack.show()
                if hasattr(self, 'split_toggle_btn'):
                    self.split_toggle_btn.setIcon(get_layout_w1left_icon(20))
                    self.split_toggle_btn.setToolTip("Panel left | Files right → click: top/bottom")
            elif state == 1:  # left stack top / tabs bottom
                splitter.setOrientation(Qt.Orientation.Vertical)
                splitter.insertWidget(0, self.left_stack)
                if tab_widget: splitter.insertWidget(1, tab_widget)
                splitter.setSizes([300, 500])
                self.left_stack.show()
                if hasattr(self, 'split_toggle_btn'):
                    self.split_toggle_btn.setIcon(get_layout_w1top_icon(20))
                    self.split_toggle_btn.setToolTip("Panel top / Files bottom → click: right|left")
            elif state == 2:  # tabs left | left stack right
                splitter.setOrientation(Qt.Orientation.Horizontal)
                if tab_widget: splitter.insertWidget(0, tab_widget)
                splitter.insertWidget(1, self.left_stack)
                splitter.setSizes([650, 350])
                self.left_stack.show()
                if hasattr(self, 'split_toggle_btn'):
                    self.split_toggle_btn.setIcon(get_layout_w2left_icon(20))
                    self.split_toggle_btn.setToolTip("Files left | Panel right → click: hidden")
            elif state == 3:  # hidden
                splitter.setOrientation(Qt.Orientation.Horizontal)
                if tab_widget: splitter.insertWidget(0, tab_widget)
                total = sum(splitter.sizes()) or 1000
                splitter.setSizes([0, total])
                self.left_stack.hide()
                if hasattr(self, 'split_toggle_btn'):
                    self.split_toggle_btn.setIcon(get_layout_w2top_icon(20))
                    self.split_toggle_btn.setToolTip("Panel hidden → click: left|right")

        except Exception as e:
            if hasattr(self, 'main_window'):
                self.main_window.log_message(f"Layout toggle error: {str(e)}")


    def create_status_window_OLD(self): #vers 5 -kept old method
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


    def _apply_vertical_splitter_theme(self): #vers 7
        """Apply theme styling to both vertical splitters"""
        theme_colors = self._get_theme_colors("default")

        # Extract variables FIRST
        bg_secondary = theme_colors.get('bg_secondary', '#f8f9fa')
        bg_tertiary = theme_colors.get('bg_tertiary', '#e9ecef')

        # Apply to left vertical splitter if it exists
        if hasattr(self, 'left_vertical_splitter') and self.left_vertical_splitter:
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

        # Apply to middle vertical splitter if it exists
        if hasattr(self, 'middle_vertical_splitter') and self.middle_vertical_splitter:
            self.middle_vertical_splitter.setStyleSheet(f"""
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
                    color: {text_primary};
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

        if hasattr(self, 'tab_widget') and self.tab_widget is not None:
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

    def log_message(self, message): #vers 2
        """Add message to activity log — skips timestamp if message already has one
        (img_debugger pre-formats lines as '[HH:MM:SS] LEVEL message')."""
        if self.log:
            from PyQt6.QtCore import QDateTime
            import re
            # Only add timestamp if message doesn't already start with [HH:MM:SS]
            if re.match(r'^\[\d{2}:\d{2}:\d{2}\]', str(message)):
                self.log.append(str(message))
            else:
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
    def _on_main_splitter_moved(self, pos, index): #vers 1
        """Switch right panel between icon+text and icon-only at ~50% width"""
        sizes = self.main_splitter.sizes()
        if len(sizes) < 2:
            return
        right_width = sizes[-1]
        threshold = 250  # below this → icon only (panel max is 350)

        if right_width > 0 and right_width < threshold and not getattr(self, '_right_panel_icon_only', False):
            self._set_right_panel_icon_only(True)
        elif right_width >= threshold and getattr(self, '_right_panel_icon_only', False):
            self._set_right_panel_icon_only(False)

    def _set_right_panel_icon_only(self, icon_only: bool): #vers 3
        """Toggle right panel buttons between text-only and 32x32 icon-only"""
        self._right_panel_icon_only = icon_only

        all_buttons = []
        for attr in ('img_buttons', 'entry_buttons', 'options_buttons'):
            if hasattr(self, attr):
                all_buttons.extend(getattr(self, attr))

        for btn in all_buttons:
            if not hasattr(btn, 'setIcon'):
                continue
            if icon_only:
                stored = btn.property("stored_icon")
                if stored:
                    btn.setIcon(stored)
                    btn.setIconSize(QSize(28, 28))
                btn.setText("")
                btn.setFixedSize(36, 36)
                btn.setToolTip(btn.property("full_label") or "")
            else:
                btn.setIcon(QIcon())
                label = btn.property("full_label") or btn.text() or ""
                btn.setText(label)
                btn.setMinimumSize(0, 0)
                btn.setMaximumSize(16777215, 16777215)
                btn.setFixedHeight(btn.sizeHint().height() if btn.sizeHint().height() > 0 else 28)

        from PyQt6.QtWidgets import QApplication
        QApplication.processEvents()

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

    def update_file_info(self, info_text): #vers 2
        """Update file info using unified progress for completion"""
        if hasattr(self.main_window, 'update_img_status'):
            # Extract info from text if possible
            if "entries" in info_text:
                try:
                    count = int(info_text.split()[0])
                    self.main_window.update_img_status(entry_count=count)
                except:
                    pass
            # Also update the operation status if it's available
            if "loading" in info_text.lower() or "processing" in info_text.lower():
                if hasattr(self.main_window, 'set_operation_status'):
                    self.main_window.set_operation_status("working", info_text)
            elif "loaded" in info_text.lower() or "completed" in info_text.lower():
                if hasattr(self.main_window, 'set_operation_status'):
                    self.main_window.set_operation_status("success", info_text)

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


    def sort_entries(self, sort_order="name"):  # vers 5
        """Sort entries in the table with various options - shows dialog for options"""
        try:
            # Get the active tab's table instead of self.table
            from apps.methods.tab_system import get_tab_table, get_active_tab_index
            
            tab_index = get_active_tab_index(self.main_window)
            if tab_index < 0:
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("No active tab for sorting")
                return
            
            tab_widget = self.main_window.main_tab_widget.widget(tab_index)
            if not tab_widget:
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("No tab widget found")
                return
            
            active_table = get_tab_table(tab_widget)
            if not active_table:
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Table not available for sorting")
                return
            
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

            sort_combo.addItems(["Name", "Type", "Size", "Order by IDE", "Pick IDE"])
            sort_layout.addWidget(sort_combo)
            layout.addLayout(sort_layout)

            # OK and Cancel buttons
            button_layout = QHBoxLayout()
            ok_btn = QPushButton("OK")
            cancel_btn = QPushButton("Cancel")

            def on_ok():
                selected_sort = sort_combo.currentText().lower().replace(" ", "_").replace("model_", "")
                
                # Import required modules
                from apps.core.sort import sort_entries_in_table, get_associated_ide_file, parse_ide_file
                from PyQt6.QtWidgets import QFileDialog
                
                # Get the current IMG file path if available
                img_path = None
                if hasattr(self.main_window, 'current_img') and self.main_window.current_img:
                    img_path = self.main_window.current_img.file_path

                ide_entries = []
                
                # Handle "Pick IDE" option
                if "pick_ide" in selected_sort:
                    dialog.accept()
                    # Open file dialog to select IDE file
                    ide_path, _ = QFileDialog.getOpenFileName(
                        self.main_window,
                        "Select IDE File for Sorting",
                        "",
                        "IDE Files (*.ide);;All Files (*.*)"
                    )
                    
                    if ide_path:
                        ide_entries = parse_ide_file(ide_path)
                        if ide_entries:
                            selected_sort = "ide_order"
                            self.main_window.log_message(f"Using selected IDE file: {ide_path}")
                        else:
                            self.main_window.log_message(f"Could not parse IDE file: {ide_path}")
                            selected_sort = "name"
                    else:
                        self.main_window.log_message("No IDE file selected, using name sort")
                        selected_sort = "name"
                
                # Handle automatic IDE detection
                elif "ide" in sort_combo.currentText().lower():
                    selected_sort = "ide_order"
                    dialog.accept()
                    
                    if img_path:
                        ide_path = get_associated_ide_file(img_path)
                        if ide_path:
                            ide_entries = parse_ide_file(ide_path)
                            if ide_entries:
                                self.main_window.log_message(f"Found associated IDE file: {ide_path}")
                            else:
                                self.main_window.log_message(f"IDE file found but could not be parsed: {ide_path}")
                                selected_sort = "name"
                        else:
                            self.main_window.log_message("No associated IDE file found, using name sort")
                            selected_sort = "name"
                    else:
                        self.main_window.log_message("No IMG file loaded, using name sort")
                        selected_sort = "name"
                else:
                    dialog.accept()

                # Perform the sorting on the ACTIVE TAB'S TABLE
                sort_entries_in_table(active_table, selected_sort, ide_entries)

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

    def pin_selected_entries(self):  # vers 3
        """Pin selected entries - adds pin icon to Status column and saves to .pin file"""
        try:
            from apps.methods.export_shared import get_active_table
            active_table = get_active_table(self.main_window)
            if active_table and active_table.selectedItems():
                self.table = active_table  # keep in sync
                # Get selected rows
                selected_items = active_table.selectedItems()
                selected_rows = set(item.row() for item in selected_items)

                # Find Status and Name column indices
                status_col = None
                name_col = None
                for col in range(active_table.columnCount()):
                    header_item = active_table.horizontalHeaderItem(col)
                    if header_item:
                        header_text = header_item.text().lower()
                        if header_text == "status":
                            status_col = col
                        elif header_text == "name":
                            name_col = col

                if status_col is None:
                    if hasattr(self.main_window, 'log_message'):
                        self.main_window.log_message("Status column not found")
                    return

                # Get IMG file path for .pin file
                img_path = None
                if hasattr(self.main_window, 'current_img') and self.main_window.current_img:
                    img_path = self.main_window.current_img.file_path

                # Add pin icon, colour row, set is_pinned, save to .pin file
                from PyQt6.QtGui import QBrush
                from apps.core.undo_system import get_pin_row_colours, set_entry_date
                pin_icon = "📌"
                pin_colour, pin_fg = get_pin_row_colours(self.main_window)
                pinned_count = 0

                # Get entry objects for is_pinned flag
                file_object = getattr(self.main_window, 'current_img', None)
                entry_map = {}
                if file_object and hasattr(file_object, 'entries'):
                    for e in file_object.entries:
                        entry_map[getattr(e, 'name', '')] = e

                for row in selected_rows:
                    # Get entry name
                    entry_name = None
                    if name_col is not None:
                        name_item = active_table.item(row, name_col)
                        if name_item:
                            entry_name = name_item.text()

                    # Update Status column
                    status_item = active_table.item(row, status_col)
                    if status_item:
                        current_text = status_item.text()
                        if pin_icon not in current_text:
                            active_table.item(row, status_col).setText(
                                f"{pin_icon} {current_text}" if current_text else pin_icon)
                            pinned_count += 1
                    else:
                        active_table.setItem(row, status_col, QTableWidgetItem(pin_icon))
                        pinned_count += 1

                    # Colour entire row
                    for col in range(active_table.columnCount()):
                        cell = active_table.item(row, col)
                        if not cell:
                            cell = QTableWidgetItem("")
                            active_table.setItem(row, col, cell)
                        cell.setBackground(QBrush(pin_colour))
                        cell.setForeground(QBrush(pin_fg))

                    # Set is_pinned and stamp date on entry object
                    if entry_name and entry_name in entry_map:
                        e = entry_map[entry_name]
                        e.is_pinned = True
                        _ip = getattr(getattr(self.main_window, "current_img", None), "file_path", None)
                        set_entry_date(e, _ip)

                    # Save to .pin file
                    if img_path and entry_name:
                        from apps.methods.pin_file_manager import pin_entry
                        pin_entry(img_path, entry_name)

                if hasattr(self.main_window, 'log_message'):
                    if pinned_count == 1:
                        self.main_window.log_message("Entry pinned")
                    else:
                        self.main_window.log_message(f"{pinned_count} entries pinned")
            else:
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("No entries selected to pin")
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Pin entries error: {str(e)}")
            import traceback
            traceback.print_exc()

    def unpin_selected_entries(self):  # vers 3
        """Unpin selected entries - removes pin icon from Status column and updates .pin file"""
        try:
            from apps.methods.export_shared import get_active_table
            active_table = get_active_table(self.main_window)
            if active_table and active_table.selectedItems():
                self.table = active_table  # keep in sync
                # Get selected rows
                selected_items = active_table.selectedItems()
                selected_rows = set(item.row() for item in selected_items)

                # Find Status and Name column indices
                status_col = None
                name_col = None
                for col in range(active_table.columnCount()):
                    header_item = active_table.horizontalHeaderItem(col)
                    if header_item:
                        header_text = header_item.text().lower()
                        if header_text == "status":
                            status_col = col
                        elif header_text == "name":
                            name_col = col

                if status_col is None:
                    if hasattr(self.main_window, 'log_message'):
                        self.main_window.log_message("Status column not found")
                    return

                # Get IMG file path for .pin file
                img_path = None
                if hasattr(self.main_window, 'current_img') and self.main_window.current_img:
                    img_path = self.main_window.current_img.file_path

                # Remove pin icon, restore normal colours, clear is_pinned, update .pin file
                from PyQt6.QtGui import QBrush
                pin_icon = "📌"
                unpinned_count = 0

                file_object = getattr(self.main_window, 'current_img', None)
                entry_map = {}
                if file_object and hasattr(file_object, 'entries'):
                    for e in file_object.entries:
                        entry_map[getattr(e, 'name', '')] = e

                for row in selected_rows:
                    entry_name = None
                    if name_col is not None:
                        name_item = active_table.item(row, name_col)
                        if name_item:
                            entry_name = name_item.text()

                    status_item = active_table.item(row, status_col)
                    if status_item:
                        current_text = status_item.text()
                        if pin_icon in current_text:
                            status_item.setText(current_text.replace(pin_icon, "").strip())
                            unpinned_count += 1

                    # Restore default row colours
                    for col in range(active_table.columnCount()):
                        cell = active_table.item(row, col)
                        if cell:
                            cell.setBackground(QBrush())
                            cell.setForeground(QBrush())

                    # Clear is_pinned
                    if entry_name and entry_name in entry_map:
                        entry = entry_map[entry_name]
                        if hasattr(entry, 'is_pinned'):
                            del entry.is_pinned

                    # Update .pin file
                    if img_path and entry_name:
                        from apps.methods.pin_file_manager import unpin_entry
                        unpin_entry(img_path, entry_name)

                if hasattr(self.main_window, 'log_message'):
                    if unpinned_count == 1:
                        self.main_window.log_message("Entry unpinned")
                    elif unpinned_count > 1:
                        self.main_window.log_message(f"{unpinned_count} entries unpinned")
                    else:
                        self.main_window.log_message("No pinned entries selected")
            else:
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("No entries selected to unpin")
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Unpin entries error: {str(e)}")
            import traceback
            traceback.print_exc()

    def load_and_apply_pins(self, img_path: str): # vers 4 Fixed
        """Load .pin file, apply pin icons to Status column, colour rows, set entry.is_pinned, restore date_modified."""
        try:
            from apps.methods.export_shared import get_active_table
            from apps.methods.pin_file_manager import load_pin_file
            from PyQt6.QtGui import QBrush

            table = get_active_table(self.main_window) or self.table
            if not table or not img_path:
                return

            pin_data = load_pin_file(img_path)
            pinned_entries = pin_data.get("entries", {})
            if not pinned_entries:
                return  # no pin data at all - nothing to restore  # nothing pinned and no saved dates - nothing to do

            # Find Status and Name column indices
            status_col = name_col = None
            for col in range(table.columnCount()):
                header_item = table.horizontalHeaderItem(col)
                if header_item:
                    ht = header_item.text().lower()
                    if ht == "status":
                        status_col = col
                    elif ht == "name":
                        name_col = col

            if name_col is None:
                return

            # Get entry objects for setting is_pinned
            file_object = getattr(self.main_window, 'current_img', None)
            entry_map = {}
            if file_object and hasattr(file_object, 'entries'):
                for entry in file_object.entries:
                    entry_map[getattr(entry, 'name', '')] = entry

            from apps.core.undo_system import get_pin_row_colours
            pin_icon = "📌"
            pin_colour, pin_fg = get_pin_row_colours(self.main_window)
            pins_applied = 0

            # Find Date column index for restoring date_modified
            date_col = None
            for col in range(table.columnCount()):
                header_item = table.horizontalHeaderItem(col)
                if header_item and header_item.text().lower() == "date":
                    date_col = col
                    break

            for row in range(table.rowCount()):
                name_item = table.item(row, name_col)
                if not name_item:
                    continue
                entry_name = name_item.text()
                entry_data = pinned_entries.get(entry_name, {})

                # Restore date_modified for ALL entries that have it saved
                saved_date = entry_data.get("date_modified", "")
                if saved_date:
                    if entry_name in entry_map:
                        entry_map[entry_name].date_modified = saved_date
                    if date_col is not None:
                        date_item = table.item(row, date_col)
                        if date_item:
                            date_item.setText(saved_date)
                        else:
                            from PyQt6.QtWidgets import QTableWidgetItem
                            table.setItem(row, date_col, QTableWidgetItem(saved_date))

                if not entry_data.get("pinned", False):
                    continue

                # Set is_pinned on entry object
                if entry_name in entry_map:
                    entry_map[entry_name].is_pinned = True

                # Pin icon in Status column
                if status_col is not None:
                    status_item = table.item(row, status_col)
                    if status_item:
                        if pin_icon not in status_item.text():
                            t = status_item.text()
                            status_item.setText(f"{pin_icon} {t}" if t else pin_icon)
                    else:
                        table.setItem(row, status_col, QTableWidgetItem(pin_icon))

                # Colour entire row
                for col in range(table.columnCount()):
                    cell = table.item(row, col)
                    if not cell:
                        cell = QTableWidgetItem("")
                        table.setItem(row, col, cell)
                    cell.setBackground(QBrush(pin_colour))
                    cell.setForeground(QBrush(pin_fg))

                pins_applied += 1

            if pins_applied > 0 and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Restored {pins_applied} pinned entries")

        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Error loading pins: {str(e)}")
            import traceback
            traceback.print_exc()

    def show_search_dialog(self):  # vers 1
        """Show the search dialog"""
        try:
            # Create and show the search dialog
            search_dialog = ASearchDialog(self.main_window)
            search_dialog.exec()
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Search dialog error: {str(e)}")

    def move_entries_up(self):  # vers 3
        """Move selected entries up - updates file_object.entries and pushes undo."""
        try:
            from apps.methods.export_shared import get_active_table
            from apps.methods.tab_system import get_current_file_from_active_tab
            from apps.methods.img_entry_operations import move_entries_in_file
            self.table = get_active_table(self.main_window) or self.table
            if not self.table or not self.table.selectedItems():
                self.main_window.log_message("No entries selected to move")
                return
            rows = sorted(set(i.row() for i in self.table.selectedItems()))
            file_object, _ = get_current_file_from_active_tab(self.main_window)
            if not file_object:
                return
            if move_entries_in_file(self.main_window, file_object, rows, -1):
                from apps.core.undo_system import refresh_after_undo
                refresh_after_undo(self.main_window)
                # Re-select moved rows
                new_start = min(rows) - 1
                self.table.clearSelection()
                for i in range(len(rows)):
                    for col in range(self.table.columnCount()):
                        item = self.table.item(new_start + i, col)
                        if item:
                            item.setSelected(True)
                self.main_window.log_message(f"{len(rows)} entries moved up")
        except Exception as e:
            self.main_window.log_message(f"Move up error: {str(e)}")

    def move_entries_down(self):  # vers 3
        """Move selected entries down - updates file_object.entries and pushes undo."""
        try:
            from apps.methods.export_shared import get_active_table
            from apps.methods.tab_system import get_current_file_from_active_tab
            from apps.methods.img_entry_operations import move_entries_in_file
            self.table = get_active_table(self.main_window) or self.table
            if not self.table or not self.table.selectedItems():
                self.main_window.log_message("No entries selected to move")
                return
            rows = sorted(set(i.row() for i in self.table.selectedItems()))
            file_object, _ = get_current_file_from_active_tab(self.main_window)
            if not file_object:
                return
            if move_entries_in_file(self.main_window, file_object, rows, 1):
                from apps.core.undo_system import refresh_after_undo
                refresh_after_undo(self.main_window)
                new_start = min(rows) + 1
                self.table.clearSelection()
                for i in range(len(rows)):
                    for col in range(self.table.columnCount()):
                        item = self.table.item(new_start + i, col)
                        if item:
                            item.setSelected(True)
                self.main_window.log_message(f"{len(rows)} entries moved down")
        except Exception as e:
            self.main_window.log_message(f"Move down error: {str(e)}")

    def _on_open_file_selected(self, item):  # vers 1
        """Handle selection of an open file in the left panel"""
        try:
            # Get the file path or identifier from the item
            file_name = item.text()
            
            # Switch to the corresponding tab or IMG file
            if hasattr(self.main_window, 'switch_to_img_file'):
                self.main_window.switch_to_img_file(file_name)
            elif hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Selected file: {file_name}")
                
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Error selecting file: {str(e)}")


    def update_open_files_list(self):  # vers 1
        """Update the left panel list with all open files"""
        try:
            if hasattr(self, 'open_files_list') and self.open_files_list:
                # Clear the current list
                self.open_files_list.clear()
                
                # Get the main window's open_files dictionary
                if hasattr(self.main_window, 'open_files'):
                    for tab_index, file_info in self.main_window.open_files.items():
                        # Get the tab name from the tab widget
                        tab_name = self.main_window.main_tab_widget.tabText(tab_index)
                        if tab_name and tab_name != "No File":
                            # After main_tab_widget creation
                            self.main_tab_widget.tabBarDoubleClicked.connect(self._on_tab_double_click)
                            item = QListWidgetItem(tab_name)
                            item.setData(Qt.ItemDataRole.UserRole, tab_index)  # Store tab index
                            self.open_files_list.addItem(item)

                    for tab_index, file_info in self.main_window.open_files.items():
                        # Get the tab name from the tab widget
                        tab_name = self.main_window.main_tab_widget.tabText(tab_index)
                        if tab_name and tab_name != "Tab 0":
                            item = QListWidgetItem(tab_name)
                            item.setData(Qt.ItemDataRole.UserRole, tab_index)  # Store tab index
                            self.open_files_list.addItem(item)
                
                # If no files are open, add a placeholder
                if self.open_files_list.count() == 0:
                    placeholder = QListWidgetItem("No open files")
                    placeholder.setFlags(Qt.ItemFlag.NoItemFlags)  # Make it non-selectable
                    self.open_files_list.addItem(placeholder)
                    
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Error updating open files list: {str(e)}")


    def _on_tab_double_click(self, index): #vers 5
        """Double-click file tab: toggle own-full <-> split"""
        try:
            splitter = getattr(self, 'content_splitter', None)
            if not splitter or splitter.count() < 2:
                return
            total = sum(splitter.sizes()) or 10000

            # Identify tab side: whichever index is NOT the directory_tree
            mw = self.main_window
            dir_tree = getattr(mw, 'directory_tree', None)
            tree_idx = -1
            for i in range(splitter.count()):
                if splitter.widget(i) is dir_tree:
                    tree_idx = i
                    break
            tab_idx = 1 - tree_idx if tree_idx != -1 else 0

            sizes = splitter.sizes()
            if sizes[tab_idx] >= total * 0.9:
                splitter.setSizes([total // 2, total // 2])
                self.main_window.log_message("→ Split view")
            else:
                s = [0, 0]
                s[tab_idx] = total
                splitter.setSizes(s)
                self.main_window.log_message("→ Files full")
        except Exception as e:
            self.main_window.log_message(f"Tab double-click error: {str(e)}")


    def refresh_directory_files(self):  # vers 1
        """Refresh the list of files in the same directory as the currently loaded IMG file"""
        try:
            # Clear the current list
            if hasattr(self, 'directory_files_list') and self.directory_files_list:
                self.directory_files_list.clear()
                
                # Get the current IMG file path
                if (hasattr(self.main_window, 'current_img') and 
                    self.main_window.current_img and 
                    hasattr(self.main_window.current_img, 'file_path') and
                    self.main_window.current_img.file_path):
                    
                    # Get the directory of the current IMG file
                    img_dir = os.path.dirname(self.main_window.current_img.file_path)
                    
                    # Get all files in the directory (excluding the current IMG file to avoid confusion)
                    current_img_filename = os.path.basename(self.main_window.current_img.file_path)
                    
                    # List all files in the directory
                    all_files = os.listdir(img_dir)
                    
                    # Filter for common file types that might be related to IMG files
                    valid_extensions = ('.img', '.txd', '.col', '.dff', '.ide', '.ipl', '.dat', '.ifp', '.cfg', '.txt', '.ini')
                    filtered_files = [f for f in all_files if f.lower().endswith(valid_extensions) and f != current_img_filename]
                    
                    # Add the files to the list
                    for filename in sorted(filtered_files):
                        item = QListWidgetItem(filename)
                        # Store the full path as user data
                        full_path = os.path.join(img_dir, filename)
                        item.setData(Qt.ItemDataRole.UserRole, full_path)
                        self.directory_files_list.addItem(item)
                
                # If no IMG file is loaded, show a placeholder
                if self.directory_files_list.count() == 0:
                    placeholder = QListWidgetItem("No directory files")
                    placeholder.setFlags(Qt.ItemFlag.NoItemFlags)  # Make it non-selectable
                    self.directory_files_list.addItem(placeholder)
                    
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Error refreshing directory files: {str(e)}")


    def _on_directory_file_selected(self, item):  # vers 1
        """Handle selection of a file in the same directory as the loaded IMG file"""
        try:
            # Get the file path from the item's user data
            file_path = item.data(Qt.ItemDataRole.UserRole)
            
            if file_path and os.path.isfile(file_path):
                # Check the file extension to determine how to handle it
                file_ext = os.path.splitext(file_path)[1].lower()
                
                # Log the selected file
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"Selected file: {os.path.basename(file_path)} ({file_ext})")

                # Update COL menu action visibility
                self.update_col_menu_for_selection(file_path)
                
                # For now, we'll just open the file with the appropriate handler
                # based on its extension, but in a real implementation you might want
                # to have specific logic for different file types
                if file_ext == '.img':
                    # Open IMG file
                    if hasattr(self.main_window, 'open_img_file_from_path'):
                        self.main_window.open_img_file_from_path(file_path)
                    else:
                        self.main_window.log_message(f"Opening IMG file: {file_path}")
                elif file_ext == '.txd':
                    # Open TXD file with TXD Workshop
                    from apps.components.Txd_Editor.txd_workshop import open_txd_workshop
                    workshop = open_txd_workshop(self.main_window, file_path)
                    if workshop:
                        self.main_window.log_message(f"TXD Workshop opened for: {file_path}")
                elif file_ext == '.col':
                    if hasattr(self.main_window, 'load_file_unified'):
                        self.main_window.load_file_unified(file_path)
                    elif hasattr(self.main_window, 'load_col_file_safely'):
                        self.main_window.load_col_file_safely(file_path)
                else:
                    # For other file types, you might want to open them in a text editor or handle differently
                    self.main_window.log_message(f"Selected file: {file_path}")
                    
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Error selecting directory file: {str(e)}")


    def _on_directory_list_context_menu(self, pos): #vers 2
        """Show context menu for directory file list"""
        try:
            from PyQt6.QtWidgets import QMenu
            item = self.directory_files_list.itemAt(pos)
            if not item:
                return

            file_path = item.data(Qt.ItemDataRole.UserRole)
            if not file_path:
                return

            menu = QMenu(self.directory_files_list)
            file_ext = os.path.splitext(file_path)[1].lower()

            # ── COL Workshop ─────────────────────────────────────────────
            if file_ext == '.col':
                open_action = menu.addAction("Open in COL Workshop")
                try:
                    from apps.methods.imgfactory_svg_icons import get_edit_icon
                    open_action.setIcon(get_edit_icon())
                except Exception:
                    pass
                open_action.triggered.connect(lambda: self._open_file_in_col_workshop(file_path))
                menu.addSeparator()

            # ── IDE Editor ───────────────────────────────────────────────
            if file_ext == '.ide':
                ide_action = menu.addAction("Open in IDE Editor")
                ide_action.triggered.connect(lambda: self._open_file_in_ide_editor(file_path))
                menu.addSeparator()

            # ── Generic text editor for editable types ───────────────────
            _TEXT_EDITABLE = ('.ide', '.ipl', '.dat', '.txt', '.cfg',
                              '.ini', '.zon', '.cut', '.fxt')
            if file_ext in _TEXT_EDITABLE:
                edit_action = menu.addAction(f"Edit  {os.path.basename(file_path)}")
                try:
                    from apps.methods.imgfactory_svg_icons import get_edit_icon
                    edit_action.setIcon(get_edit_icon())
                except Exception:
                    pass
                edit_action.triggered.connect(
                    lambda _=False, p=file_path: self._open_file_in_text_editor(p))
                menu.addSeparator()

            load_action = menu.addAction("Load File")
            load_action.triggered.connect(lambda: self._on_directory_file_selected(item))
            menu.exec(self.directory_files_list.mapToGlobal(pos))

        except Exception as e:
            if hasattr(self, 'log_message'):
                self.log_message(f"Context menu error: {str(e)}")

    def _open_file_in_col_workshop(self, file_path): #vers 1
        """Open a COL file in COL Workshop"""
        try:
            from apps.components.Col_Editor.col_workshop import open_col_workshop
            workshop = open_col_workshop(self.main_window, file_path)
            if workshop:
                self.main_window.log_message(f"COL Workshop opened: {os.path.basename(file_path)}")
            else:
                self.main_window.log_message("Failed to open COL Workshop")
        except Exception as e:
            self.main_window.log_message(f"COL Workshop error: {str(e)}")

    def _open_file_in_text_editor(self, file_path: str): #vers 1
        """Open any text-editable GTA file in the IMG Factory text editor."""
        try:
            from apps.core.notepad import open_text_file_in_editor
            open_text_file_in_editor(file_path, self.main_window)
            self.main_window.log_message(
                f"Text Editor opened: {os.path.basename(file_path)}")
        except Exception as e:
            self.main_window.log_message(f"Text Editor error: {str(e)}")

    def _open_file_in_ide_editor(self, file_path: str): #vers 1
        """Open an .ide file in the structured IDE Editor."""
        try:
            from apps.components.Ide_Editor.ide_editor import open_ide_editor
            editor = open_ide_editor(self.main_window)
            editor.load_ide_file(file_path)
            self.main_window.log_message(
                f"IDE Editor opened: {os.path.basename(file_path)}")
        except Exception as e:
            self.main_window.log_message(f"IDE Editor error: {str(e)}")

    def update_col_menu_for_selection(self, file_path=None): #vers 1
        """Enable/disable COL menu workshop action based on selected file"""
        try:
            if not hasattr(self.main_window, '_col_workshop_menu_action'):
                return
            is_col = file_path and os.path.splitext(file_path)[1].lower() == '.col'
            self.main_window._col_workshop_menu_action.setEnabled(bool(is_col))
            if is_col:
                self.main_window._col_workshop_menu_action.setStatusTip(
                    f"Open {os.path.basename(file_path)} in COL Workshop")
        except Exception:
            pass


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
