#this belongs in apps/components/File_Editor/directory_tree_browser.py - Version: 2
# X-Seti - January10 2026 - IMG Factory 1.6 - Complete Directory Tree Browser
"""
COMPLETE DIRECTORY TREE BROWSER
Single unified file browser with full functionality
NO fallback code - works or doesn't work
"""
import os
import shutil
import subprocess
import platform
import fnmatch
from typing import Dict, List, Optional
from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QTreeWidget, QTreeWidgetItem,
    QMenuBar, QMenu, QToolBar, QPushButton, QLineEdit, QLabel, QMessageBox,
    QInputDialog, QDialog, QFormLayout, QCheckBox, QListWidget, QSplitter
)
from PyQt6.QtCore import Qt, pyqtSignal, QSettings
from PyQt6.QtGui import QAction
# SVG Icons
try:
    from apps.methods.imgfactory_svg_icons import (
        get_folder_icon, get_file_icon, get_img_file_icon,
        get_txd_file_icon, get_col_file_icon, get_refresh_icon,
        get_view_icon, get_edit_icon, get_image_icon,
        get_copy_icon, get_paste_icon, get_cut_icon, get_rename_icon,
        get_back_icon, get_forward_icon, get_up_icon, get_home_icon,
        get_search_icon, get_properties_icon, get_new_folder_icon,
        get_trash_icon, get_undo_icon, get_redo_icon,
        get_twin_panel_icon, get_single_panel_icon
    )
except ImportError:
    # Fallback implementations if the icons module is not available
    def get_folder_icon():
        from PyQt6.QtGui import QIcon
        return QIcon()
    
    def get_file_icon():
        from PyQt6.QtGui import QIcon
        return QIcon()
        
    def get_img_file_icon():
        from PyQt6.QtGui import QIcon
        return QIcon()
        
    def get_txd_file_icon():
        from PyQt6.QtGui import QIcon
        return QIcon()
        
    def get_col_file_icon():
        from PyQt6.QtGui import QIcon
        return QIcon()
        
    def get_refresh_icon():
        from PyQt6.QtGui import QIcon
        return QIcon()
        
    def get_view_icon():
        from PyQt6.QtGui import QIcon
        return QIcon()
        
    def get_edit_icon():
        from PyQt6.QtGui import QIcon
        return QIcon()
        
    def get_image_icon():
        from PyQt6.QtGui import QIcon
        return QIcon()
        
    def get_copy_icon():
        from PyQt6.QtGui import QIcon
        return QIcon()
        
    def get_paste_icon():
        from PyQt6.QtGui import QIcon
        return QIcon()
        
    def get_cut_icon():
        from PyQt6.QtGui import QIcon
        return QIcon()
        
    def get_rename_icon():
        from PyQt6.QtGui import QIcon
        return QIcon()
        
    def get_back_icon():
        from PyQt6.QtGui import QIcon
        return QIcon()
        
    def get_forward_icon():
        from PyQt6.QtGui import QIcon
        return QIcon()
        
    def get_up_icon():
        from PyQt6.QtGui import QIcon
        return QIcon()
        
    def get_home_icon():
        from PyQt6.QtGui import QIcon
        return QIcon()
        
    def get_search_icon():
        from PyQt6.QtGui import QIcon
        return QIcon()
        
    def get_properties_icon():
        from PyQt6.QtGui import QIcon
        return QIcon()
        
    def get_new_folder_icon():
        from PyQt6.QtGui import QIcon
        return QIcon()
        
    def get_trash_icon():
        from PyQt6.QtGui import QIcon
        return QIcon()
        
    def get_undo_icon():
        from PyQt6.QtGui import QIcon
        return QIcon()
        
    def get_redo_icon():
        from PyQt6.QtGui import QIcon
        return QIcon()

App_name = "File_Editor"
standalone = False

##Methods list -
# apply_browser_styling
# browse_directory
# copy_files
# copy_path_to_clipboard
# count_tree_items
# create_menubar
# create_new_folder
# create_toolbar
# cut_files
# delete_selected
# file_contains_text
# get_file_type_display
# get_file_type_icon
# integrate_directory_tree_browser
# is_dark_theme
# load_browser_settings
# navigate_back
# navigate_forward
# navigate_home
# navigate_to_address
# navigate_up
# on_item_clicked
# on_item_double_clicked
# open_in_explorer
# paste_files
# populate_tree
# populate_tree_recursive
# refresh_browser
# rename_selected
# save_browser_settings
# search_files
# setup_connections
# setup_edit_menu
# setup_file_menu
# setup_settings_menu
# setup_tools_menu
# setup_tree_view
# setup_ui
# show_context_menu
# show_file_properties
# show_file_search_dialog

class DirectoryTreeBrowser(QWidget):
    """Complete directory tree browser widget"""
    # Signals
    file_selected = pyqtSignal(str)
    file_opened = pyqtSignal(str)
    directory_changed = pyqtSignal(str)

    def __init__(self, parent=None): #vers 2
        super().__init__(parent)
        self.main_window = parent
        self.standalone = standalone
        self.undo_stack = []  # List of {'action': 'cut'/'delete'/'rename', 'src': ..., 'dest': ...}
        self.redo_stack = []

        self.current_path = None
        self.clipboard_files = []
        self.clipboard_operation = None
        self.navigation_history = []
        self.history_index = -1
        self.browser_settings = self.load_browser_settings()
        self._set_initial_path()
        self.setup_ui()
        self.setup_connections()


    def setup_ui(self): #vers 3
        """Setup complete browser UI - toolbar then single container (address bar + tree)"""
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        if self.standalone:
            self.menubar = self.create_menubar()
            layout.addWidget(self.menubar)

        self.toolbar = self.create_toolbar()
        layout.addWidget(self.toolbar)

        # Single panel container - hide/show as one unit when toggling twin view
        self._single_container = QWidget()
        sc_layout = QVBoxLayout(self._single_container)
        sc_layout.setContentsMargins(0, 0, 0, 0)
        sc_layout.setSpacing(0)

        addr_layout = QHBoxLayout()
        addr_layout.setContentsMargins(2, 2, 2, 2)
        self.address_bar = QLineEdit()
        self.address_bar.setPlaceholderText("Path...")
        self.address_bar.returnPressed.connect(self.navigate_to_address)
        addr_layout.addWidget(self.address_bar)
        from apps.methods.imgfactory_svg_icons import get_go_icon
        go_btn = QPushButton()
        go_btn.setIcon(get_go_icon(16))
        go_btn.setFixedSize(24, 24)
        go_btn.setToolTip("Go")
        go_btn.clicked.connect(self.navigate_to_address)
        addr_layout.addWidget(go_btn)
        sc_layout.addLayout(addr_layout)

        self.tree = QTreeWidget()
        self.tree.setHeaderLabel("Directory Structure")
        self.setup_tree_view()
        sc_layout.addWidget(self.tree)

        layout.addWidget(self._single_container)
        layout.setStretchFactor(self._single_container, 1)


    def setup_tree_view(self): #vers 2
        """Setup tree widget"""
        self.tree.setAlternatingRowColors(True)
        self.tree.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.tree.customContextMenuRequested.connect(self.show_context_menu)
        self.tree.itemClicked.connect(self.on_item_clicked)
        self.tree.itemDoubleClicked.connect(self.on_item_double_clicked)
        self.tree._browser = self
        self._setup_tree_dragdrop(self.tree, 'left')

    def _setup_tree_dragdrop(self, tree, side='left'): #vers 1
        """Wire drag/drop onto a tree widget"""
        try:
            from apps.methods.dragdrop_functions import setup_tree_drag_drop, setup_tree_as_extract_target
            mw = getattr(self, 'main_window', None)
            setup_tree_drag_drop(tree, mw, side)
            setup_tree_as_extract_target(tree, mw)
        except Exception as e:
            print(f"Tree drag/drop setup error: {e}")


    def apply_browser_styling(self): #vers 1
        """Apply theme-aware styling"""
        if self.is_dark_theme():
            stylesheet = """
            QTreeWidget {
                background-color: #1e1e1e;
                color: #ffffff;
                border: 1px solid #3a3a3a;
            }
            QTreeWidget::item:selected {
                background-color: #3a7ca8;
            }
            QTreeWidget::item:hover {
                background-color: #2a2a2a;
            }
            """
        else:
            stylesheet = """
            QTreeWidget {
                background-color: #ffffff;
                color: #000000;
                border: 1px solid #cccccc;
            }
            QTreeWidget::item:selected {
                background-color: #0078d7;
                color: #ffffff;
            }
            QTreeWidget::item:hover {
                background-color: #e5f3ff;
            }
            """
        self.tree.setStyleSheet(stylesheet)


    def is_dark_theme(self) -> bool: #vers 1
        """Detect if dark theme is active"""
        bg_color = self.palette().color(self.backgroundRole())
        return bg_color.lightness() < 128


    def setup_connections(self): #vers 1
        """Setup signal connections"""
        pass


    def create_menubar(self): #vers 1
        """Create menu bar"""
        menubar = QMenuBar()
        file_menu = menubar.addMenu("File")
        self.setup_file_menu(file_menu)
        edit_menu = menubar.addMenu("Edit")
        self.setup_edit_menu(edit_menu)
        tools_menu = menubar.addMenu("Tools")
        self.setup_tools_menu(tools_menu)
        settings_menu = menubar.addMenu("Settings")
        self.setup_settings_menu(settings_menu)
        return menubar


    def setup_file_menu(self, menu): #vers 1
        """Setup File menu"""
        new_folder_action = QAction("New Folder", self)
        new_folder_action.setIcon(get_new_folder_icon())
        new_folder_action.triggered.connect(self.create_new_folder)
        menu.addAction(new_folder_action)
        menu.addSeparator()
        properties_action = QAction("Properties", self)
        properties_action.setIcon(get_properties_icon())
        properties_action.triggered.connect(self.show_file_properties)
        menu.addAction(properties_action)


    def setup_edit_menu(self, menu): #vers 1
        """Setup Edit menu"""
        cut_action = QAction("Cut", self)
        cut_action.setIcon(get_cut_icon())
        cut_action.triggered.connect(self.cut_files)
        menu.addAction(cut_action)
        copy_action = QAction("Copy", self)
        copy_action.setIcon(get_copy_icon())
        copy_action.triggered.connect(self.copy_files)
        menu.addAction(copy_action)
        paste_action = QAction("Paste", self)
        paste_action.setIcon(get_paste_icon())
        paste_action.triggered.connect(self.paste_files)
        menu.addAction(paste_action)
        menu.addSeparator()
        delete_action = QAction("Delete", self)
        delete_action.setIcon(get_trash_icon())
        delete_action.triggered.connect(self.delete_selected)
        menu.addAction(delete_action)
        rename_action = QAction("Rename", self)
        rename_action.setIcon(get_rename_icon())
        rename_action.triggered.connect(self.rename_selected)
        menu.addAction(rename_action)


    def setup_tools_menu(self, menu): #vers 1
        """Setup Tools menu"""
        search_action = QAction("Search Files", self)
        search_action.setIcon(get_search_icon())
        search_action.triggered.connect(self.show_file_search_dialog)
        menu.addAction(search_action)


    def setup_settings_menu(self, menu): #vers 1
        """Setup Settings menu"""
        refresh_action = QAction("Refresh", self)
        refresh_action.setIcon(get_refresh_icon())
        refresh_action.triggered.connect(self.refresh_browser)
        menu.addAction(refresh_action)


    def create_toolbar(self): #vers 1
        """Create toolbar - ICONS ONLY"""
        toolbar = QWidget()
        layout = QHBoxLayout(toolbar)
        layout.setContentsMargins(2, 2, 2, 2)
        layout.setSpacing(2)

        back_btn = QPushButton()
        back_btn.setIcon(get_back_icon())
        back_btn.setToolTip("Back")
        back_btn.setMaximumSize(32, 32)
        back_btn.clicked.connect(self.navigate_back)
        layout.addWidget(back_btn)

        forward_btn = QPushButton()
        forward_btn.setIcon(get_forward_icon())
        forward_btn.setToolTip("Forward")
        forward_btn.setMaximumSize(32, 32)
        forward_btn.clicked.connect(self.navigate_forward)
        layout.addWidget(forward_btn)

        up_btn = QPushButton()
        up_btn.setIcon(get_up_icon())
        up_btn.setToolTip("Up")
        up_btn.setMaximumSize(32, 32)
        up_btn.clicked.connect(self.navigate_up)
        layout.addWidget(up_btn)

        home_btn = QPushButton()
        home_btn.setIcon(get_home_icon())
        home_btn.setToolTip("Home")
        home_btn.setMaximumSize(32, 32)
        home_btn.clicked.connect(self.navigate_home)
        layout.addWidget(home_btn)

        layout.addSpacing(10)

        new_folder_btn = QPushButton()
        new_folder_btn.setIcon(get_new_folder_icon())
        new_folder_btn.setToolTip("New Folder")
        new_folder_btn.setMaximumSize(32, 32)
        new_folder_btn.clicked.connect(self.create_new_folder)
        layout.addWidget(new_folder_btn)

        cut_btn = QPushButton()
        cut_btn.setIcon(get_cut_icon())
        cut_btn.setToolTip("Cut")
        cut_btn.setMaximumSize(32, 32)
        cut_btn.clicked.connect(self.cut_files)
        layout.addWidget(cut_btn)

        copy_btn = QPushButton()
        copy_btn.setIcon(get_copy_icon())
        copy_btn.setToolTip("Copy")
        copy_btn.setMaximumSize(32, 32)
        copy_btn.clicked.connect(self.copy_files)
        layout.addWidget(copy_btn)

        paste_btn = QPushButton()
        paste_btn.setIcon(get_paste_icon())
        paste_btn.setToolTip("Paste")
        paste_btn.setMaximumSize(32, 32)
        paste_btn.clicked.connect(self.paste_files)
        layout.addWidget(paste_btn)

        rename_btn = QPushButton()
        rename_btn.setIcon(get_rename_icon())
        rename_btn.setToolTip("Rename")
        rename_btn.setMaximumSize(32, 32)
        rename_btn.clicked.connect(self.rename_selected)
        layout.addWidget(rename_btn)

        delete_btn = QPushButton()
        delete_btn.setIcon(get_trash_icon())
        delete_btn.setToolTip("Delete")
        delete_btn.setMaximumSize(32, 32)
        delete_btn.clicked.connect(self.delete_selected)
        layout.addWidget(delete_btn)

        undo_btn = QPushButton()
        undo_btn.setIcon(get_undo_icon())
        undo_btn.setToolTip("Undo")
        undo_btn.setMaximumSize(32, 32)
        undo_btn.clicked.connect(self.undo_selected)
        layout.addWidget(undo_btn)

        redo_btn = QPushButton()
        redo_btn.setIcon(get_redo_icon())
        redo_btn.setToolTip("Redo")
        redo_btn.setMaximumSize(32, 32)
        redo_btn.clicked.connect(self.redo_selected)
        layout.addWidget(redo_btn)

        layout.addStretch()

        search_btn = QPushButton()
        search_btn.setIcon(get_search_icon())
        search_btn.setToolTip("Search")
        search_btn.setMaximumSize(32, 32)
        search_btn.clicked.connect(self.show_file_search_dialog)
        layout.addWidget(search_btn)

        refresh_btn = QPushButton()
        refresh_btn.setIcon(get_refresh_icon())
        refresh_btn.setToolTip("Refresh")
        refresh_btn.setMaximumSize(32, 32)
        refresh_btn.clicked.connect(self.refresh_browser)
        layout.addWidget(refresh_btn)

        layout.addSpacing(6)

        self.panel_toggle_btn = QPushButton()
        self.panel_toggle_btn.setIcon(get_twin_panel_icon())
        self.panel_toggle_btn.setToolTip("Switch to twin panel view")
        self.panel_toggle_btn.setMaximumSize(32, 32)
        self.panel_toggle_btn.clicked.connect(self._toggle_panel_mode)
        layout.addWidget(self.panel_toggle_btn)

        from apps.methods.imgfactory_svg_icons import get_layout_w1left_icon
        self.layout_cycle_btn = QPushButton()
        self.layout_cycle_btn.setIcon(get_layout_w1left_icon())
        self.layout_cycle_btn.setToolTip("W1 left | W2 right")
        self.layout_cycle_btn.setMaximumSize(32, 32)
        self.layout_cycle_btn.clicked.connect(self._cycle_layout)
        self.layout_cycle_btn.hide()  # only visible in twin mode
        self._layout_state = 0
        layout.addWidget(self.layout_cycle_btn)

        return toolbar


    def _toggle_panel_mode(self): #vers 1
        """Toggle between single and twin panel view"""
        if hasattr(self, '_twin_container') and self._twin_container:
            self._enable_single_panel()
        else:
            self._enable_twin_panel()

    def _cycle_layout(self): #vers 1
        """Cycle through 4 twin panel layout states"""
        from apps.methods.imgfactory_svg_icons import (
            get_layout_w1left_icon, get_layout_w1top_icon,
            get_layout_w2left_icon, get_layout_w2top_icon
        )
        if not hasattr(self, '_twin_splitter') or not self._twin_splitter:
            return

        self._layout_state = (self._layout_state + 1) % 4
        state = self._layout_state

        if state == 0:  # W1 left | W2 right
            self._twin_splitter.setOrientation(Qt.Orientation.Horizontal)
            self._twin_splitter.insertWidget(0, self._left_panel)
            self._twin_splitter.insertWidget(1, self._right_panel)
            self.layout_cycle_btn.setIcon(get_layout_w1left_icon())
            self.layout_cycle_btn.setToolTip("W1 left | W2 right → click: W1 top / W2 bottom")
        elif state == 1:  # W1 top / W2 bottom
            self._twin_splitter.setOrientation(Qt.Orientation.Vertical)
            self._twin_splitter.insertWidget(0, self._left_panel)
            self._twin_splitter.insertWidget(1, self._right_panel)
            self.layout_cycle_btn.setIcon(get_layout_w1top_icon())
            self.layout_cycle_btn.setToolTip("W1 top / W2 bottom → click: W2 left | W1 right")
        elif state == 2:  # W2 left | W1 right
            self._twin_splitter.setOrientation(Qt.Orientation.Horizontal)
            self._twin_splitter.insertWidget(0, self._right_panel)
            self._twin_splitter.insertWidget(1, self._left_panel)
            self.layout_cycle_btn.setIcon(get_layout_w2left_icon())
            self.layout_cycle_btn.setToolTip("W2 left | W1 right → click: W2 top / W1 bottom")
        elif state == 3:  # W2 top / W1 bottom
            self._twin_splitter.setOrientation(Qt.Orientation.Vertical)
            self._twin_splitter.insertWidget(0, self._right_panel)
            self._twin_splitter.insertWidget(1, self._left_panel)
            self.layout_cycle_btn.setIcon(get_layout_w2top_icon())
            self.layout_cycle_btn.setToolTip("W2 top / W1 bottom → click: W1 left | W2 right")

        self._twin_splitter.setSizes([500, 500])

    def _enable_twin_panel(self): #vers 3
        """Split into two independent panels - each with own address bar and tree"""
        try:
            if hasattr(self, '_twin_container') and self._twin_container:
                return

            from apps.methods.imgfactory_svg_icons import get_arrow_right_icon, get_go_icon, get_single_panel_icon
            layout = self.layout()

            # Hide single panel widgets (keep references alive)
            self._single_container.hide()

            # --- Left panel ---
            left_panel = QWidget()
            left_layout = QVBoxLayout(left_panel)
            left_layout.setContentsMargins(0, 0, 0, 0)
            left_layout.setSpacing(2)

            left_addr_layout = QHBoxLayout()
            left_addr_layout.setContentsMargins(2, 2, 2, 2)
            left_addr_layout.addWidget(QLabel("L:"))
            self._left_addr = QLineEdit()
            self._left_addr.setText(self.current_path or "")
            self._left_addr.returnPressed.connect(lambda: self._twin_navigate(self._left_addr.text(), 'left'))
            left_addr_layout.addWidget(self._left_addr)
            left_go = QPushButton()
            left_go.setIcon(get_go_icon(16))
            left_go.setFixedSize(24, 24)
            left_go.setToolTip("Go")
            left_go.clicked.connect(lambda: self._twin_navigate(self._left_addr.text(), 'left'))
            left_addr_layout.addWidget(left_go)

            self._copy_dir_btn = QPushButton()
            self._copy_dir_btn.setIcon(get_arrow_right_icon(16))
            self._copy_dir_btn.setFixedSize(24, 24)
            self._copy_dir_btn.setToolTip("Copy direction: Left → Right (click to reverse)")
            self._copy_dir_btn._direction = 'LR'
            self._copy_dir_btn.clicked.connect(self._toggle_copy_direction)
            left_addr_layout.addWidget(self._copy_dir_btn)

            copy_btn = QPushButton()
            copy_btn.setIcon(get_go_icon(16))
            copy_btn.setFixedSize(24, 24)
            copy_btn.setToolTip("Copy selected files")
            copy_btn.clicked.connect(self._copy_selected_files)
            left_addr_layout.addWidget(copy_btn)
            left_layout.addLayout(left_addr_layout)

            self.tree.setSelectionMode(QTreeWidget.SelectionMode.ExtendedSelection)
            self.tree.setParent(left_panel)
            left_layout.addWidget(self.tree)

            # --- Right panel ---
            right_panel = QWidget()
            right_layout = QVBoxLayout(right_panel)
            right_layout.setContentsMargins(0, 0, 0, 0)
            right_layout.setSpacing(2)

            right_addr_layout = QHBoxLayout()
            right_addr_layout.setContentsMargins(2, 2, 2, 2)
            right_addr_layout.addWidget(QLabel("R:"))
            self._right_addr = QLineEdit()
            self._right_addr.setText(self.current_path or "")
            self._right_addr.returnPressed.connect(lambda: self._twin_navigate(self._right_addr.text(), 'right'))
            right_addr_layout.addWidget(self._right_addr)
            right_go = QPushButton()
            right_go.setIcon(get_go_icon(16))
            right_go.setFixedSize(24, 24)
            right_go.setToolTip("Go")
            right_go.clicked.connect(lambda: self._twin_navigate(self._right_addr.text(), 'right'))
            right_addr_layout.addWidget(right_go)
            right_layout.addLayout(right_addr_layout)

            self._second_tree = QTreeWidget()
            self._second_tree.setHeaderLabel("Directory Structure")
            self._second_tree.setAlternatingRowColors(True)
            self._second_tree.setSelectionMode(QTreeWidget.SelectionMode.ExtendedSelection)
            self._second_tree.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
            self._second_tree.customContextMenuRequested.connect(self.show_context_menu)
            self._second_tree._browser = self
            self._setup_tree_dragdrop(self._second_tree, 'right')
            right_layout.addWidget(self._second_tree)

            # --- Twin container ---
            self._twin_container = QWidget()
            twin_layout = QVBoxLayout(self._twin_container)
            twin_layout.setContentsMargins(0, 0, 0, 0)
            self._left_panel = left_panel
            self._right_panel = right_panel
            self._layout_state = 0
            self._twin_splitter = QSplitter(Qt.Orientation.Horizontal)
            self._twin_splitter.addWidget(left_panel)
            self._twin_splitter.addWidget(right_panel)
            self._twin_splitter.setSizes([500, 500])
            twin_layout.addWidget(self._twin_splitter)

            layout.addWidget(self._twin_container)
            layout.setStretchFactor(self._twin_container, 1)
            layout.setStretchFactor(self._single_container, 0)

            if self.current_path:
                self._populate_second_tree(self.current_path)
                self._right_addr.setText(self.current_path)

            from apps.methods.imgfactory_svg_icons import get_layout_w1left_icon
            self.panel_toggle_btn.setIcon(get_single_panel_icon())
            self.panel_toggle_btn.setToolTip("Switch to single panel view")
            self.layout_cycle_btn.setIcon(get_layout_w1left_icon())
            self.layout_cycle_btn.setToolTip("W1 left | W2 right → click: W1 top / W2 bottom")
            self.layout_cycle_btn.show()

        except Exception as e:
            import traceback
            traceback.print_exc()

    def _toggle_copy_direction(self): #vers 2
        """Toggle copy direction arrow between → and ←"""
        from apps.methods.imgfactory_svg_icons import get_arrow_right_icon, get_arrow_left_icon
        btn = self._copy_dir_btn
        if btn._direction == 'LR':
            btn._direction = 'RL'
            btn.setIcon(get_arrow_left_icon(16))
            btn.setToolTip("Copy direction: Right → Left (click to reverse)")
        else:
            btn._direction = 'LR'
            btn.setIcon(get_arrow_right_icon(16))
            btn.setToolTip("Copy direction: Left → Right (click to reverse)")

    def _copy_selected_files(self): #vers 2
        """Copy selected files in the active direction"""
        try:
            direction = getattr(self._copy_dir_btn, '_direction', 'LR')
            if direction == 'LR':
                src_tree = self.tree
                dst_path = self._right_addr.text()
            else:
                src_tree = self._second_tree
                dst_path = self._left_addr.text()

            selected = src_tree.selectedItems()
            if not selected:
                return
            if not os.path.isdir(dst_path):
                from PyQt6.QtWidgets import QMessageBox
                QMessageBox.warning(self, "Copy", f"Destination not valid:\n{dst_path}")
                return

            copied_dests = []
            for item in selected:
                src = item.data(0, Qt.ItemDataRole.UserRole)
                if src and os.path.exists(src):
                    dst = os.path.join(dst_path, os.path.basename(src))
                    if os.path.isdir(src):
                        shutil.copytree(src, dst)
                    else:
                        shutil.copy2(src, dst)
                    copied_dests.append(dst)

            if copied_dests:
                self.undo_stack.append({'action': 'copy', 'paths': copied_dests})
                self.redo_stack.clear()
                self.log_message(f"Copied {len(copied_dests)} item(s) → {dst_path}")

            # Refresh destination panel
            if direction == 'LR':
                self._populate_second_tree(dst_path)
            else:
                self.browse_directory(dst_path)

        except Exception as e:
            print(f"Copy error: {e}")

    def _twin_navigate(self, path: str, side: str): #vers 1
        """Navigate a specific twin panel side to a path"""
        if not os.path.isdir(path):
            return
        if side == 'left':
            self.browse_directory(path)
            self._left_addr.setText(path)
        else:
            self._populate_second_tree(path)
            self._right_addr.setText(path)

    def _enable_single_panel(self): #vers 3
        """Restore single panel view"""
        try:
            if not hasattr(self, '_twin_container') or not self._twin_container:
                return

            from apps.methods.imgfactory_svg_icons import get_twin_panel_icon
            layout = self.layout()

            # Move primary tree back into single container before destroying twin
            sc_layout = self._single_container.layout()
            self.tree.setParent(self._single_container)
            sc_layout.addWidget(self.tree)

            layout.removeWidget(self._twin_container)
            self._twin_container.deleteLater()
            self._twin_container = None
            self._twin_splitter = None
            self._second_tree = None

            self.tree.setSelectionMode(QTreeWidget.SelectionMode.SingleSelection)
            self._single_container.show()
            layout = self.layout()
            layout.setStretchFactor(self._single_container, 1)

            self.panel_toggle_btn.setIcon(get_twin_panel_icon())
            self.panel_toggle_btn.setToolTip("Switch to twin panel view")
            self.layout_cycle_btn.hide()
            self._left_panel = None
            self._right_panel = None

        except Exception as e:
            import traceback
            traceback.print_exc()

    def _populate_second_tree(self, path: str): #vers 1
        """Populate the second panel tree with the given path"""
        try:
            if not hasattr(self, '_second_tree') or not self._second_tree:
                return
            self._second_tree.clear()
            self._second_tree.setHeaderLabel(f"  {os.path.basename(path)}")
            # Reuse populate_tree logic on second tree by temporarily swapping
            original_tree = self.tree
            self.tree = self._second_tree
            self.populate_tree(path)
            self.tree = original_tree
        except Exception as e:
            print(f"Second tree populate error: {e}")

    def browse_directory(self, path: str): #vers 1
        """Browse to specific directory"""
        if os.path.exists(path) and os.path.isdir(path):
            self.current_path = path
            self.address_bar.setText(path)
            self.populate_tree(path)
            self.directory_changed.emit(path)
            if self.history_index < len(self.navigation_history) - 1:
                self.navigation_history = self.navigation_history[:self.history_index + 1]

            self.navigation_history.append(path)
            self.history_index = len(self.navigation_history) - 1

            # Save the current path to settings so it persists
            from PyQt6.QtCore import QSettings
            settings = QSettings("IMG-Factory", "IMG-Factory")
            settings.setValue("game_root", path)

            self.log_message(f"Browsing: {path}")


    def _set_initial_path(self):
        """Set the initial path based on project manager or other sources"""
        # First priority: Check if project manager has a current project with game_root
        if (hasattr(self.main_window, 'project_manager') and
            self.main_window.project_manager and
            self.main_window.project_manager.current_project):

            current_project_settings = self.main_window.project_manager.get_project_settings(
                self.main_window.project_manager.current_project
            )
            project_path = current_project_settings.get('game_root', '')
            if project_path and os.path.exists(project_path):
                self.current_path = project_path
                self.log_message(f"Using project game root from active project: {project_path}")
                return

        # Second priority: Check if main window has a game_root attribute
        if hasattr(self.main_window, 'game_root') and self.main_window.game_root:
            if os.path.exists(self.main_window.game_root):
                self.current_path = self.main_window.game_root
                self.log_message(f"Using game_root from main window: {self.main_window.game_root}")
                return

        # Third priority: Check QSettings for saved root
        from PyQt6.QtCore import QSettings
        settings = QSettings("IMG-Factory", "IMG-Factory")
        saved_root = settings.value("game_root", "", type=str)
        if saved_root and os.path.exists(saved_root):
            self.current_path = saved_root
            self.log_message(f"Using saved game root from settings: {saved_root}")
            return

        # Last resort: Use workspace directory
        workspace_dir = os.getcwd()
        self.current_path = workspace_dir
        self.log_message(f"Using workspace directory as fallback: {workspace_dir}")


    def populate_tree(self, root_path: str): #vers 1
        """Populate tree with directory contents"""
        try:
            self.tree.clear()
            root_item = QTreeWidgetItem(self.tree)
            root_item.setText(0, os.path.basename(root_path) or root_path)
            root_item.setData(0, Qt.ItemDataRole.UserRole, root_path)
            root_item.setIcon(0, get_folder_icon())
            self.populate_tree_recursive(root_item, root_path, max_depth=3)
            root_item.setExpanded(True)
            for i in range(min(root_item.childCount(), 5)):
                child = root_item.child(i)
                if child and child.childCount() > 0:
                    child.setExpanded(True)
            total_items = self.count_tree_items(root_item)
            self.log_message(f"Loaded: {root_path} ({total_items} items)")
        except Exception as e:
            self.log_message(f"Error populating tree: {str(e)}")


    def count_tree_items(self, item): #vers 1
        """Count total items in tree"""
        if not item:
            return 0
        count = 1
        for i in range(item.childCount()):
            count += self.count_tree_items(item.child(i))
        return count


    def populate_tree_recursive(self, parent_item: QTreeWidgetItem,
                                dir_path: str, max_depth: int = 3,
                                current_depth: int = 0): #vers 1
        """Recursively populate tree"""
        if current_depth >= max_depth:
            return
        try:
            all_items = os.listdir(dir_path)
            if not self.browser_settings.get('show_hidden', False):
                filtered_items = [item for item in all_items if not item.startswith('.')]
            else:
                filtered_items = all_items[:]
            directories = []
            files = []
            for item in filtered_items:
                item_path = os.path.join(dir_path, item)
                try:
                    if os.path.isdir(item_path):
                        directories.append(item)
                    elif os.path.isfile(item_path):
                        files.append(item)
                except (PermissionError, OSError):
                    continue
            directories.sort(key=str.lower)
            files.sort(key=str.lower)
            for directory in directories:
                item_path = os.path.join(dir_path, directory)
                tree_item = QTreeWidgetItem(parent_item)
                tree_item.setText(0, directory)
                tree_item.setData(0, Qt.ItemDataRole.UserRole, item_path)
                tree_item.setIcon(0, get_folder_icon())
                self.populate_tree_recursive(tree_item, item_path, max_depth, current_depth + 1)
            for file in files:
                item_path = os.path.join(dir_path, file)
                tree_item = QTreeWidgetItem(parent_item)
                tree_item.setText(0, file)
                tree_item.setData(0, Qt.ItemDataRole.UserRole, item_path)
                file_ext = os.path.splitext(file)[1].lower()
                icon = self.get_file_type_icon(file_ext)
                tree_item.setIcon(0, icon)
        except Exception:
            pass


    def get_file_type_icon(self, file_ext: str): #vers 1
        """Get SVG icon for file type"""
        icon_map = {
            '.img': get_img_file_icon,
            '.txd': get_txd_file_icon,
            '.col': get_col_file_icon,
            '.dff': get_image_icon,
            '.ide': get_edit_icon,
            '.ipl': get_view_icon,
            '.dat': get_file_icon,
        }
        icon_func = icon_map.get(file_ext, get_file_icon)
        return icon_func()


    def get_file_type_display(self, file_ext: str) -> str: #vers 1
        """Get display name for file type"""
        type_map = {
            '.img': 'IMG Archive',
            '.txd': 'Texture Dictionary',
            '.col': 'Collision File',
            '.dff': '3D Model',
            '.ide': 'Item Definition',
            '.ipl': 'Item Placement',
            '.dat': 'Data File',
        }
        return type_map.get(file_ext, 'File')


    def on_item_clicked(self, item, column): #vers 1
        """Handle item click"""
        file_path = item.data(0, Qt.ItemDataRole.UserRole)
        if file_path and os.path.isfile(file_path):
            self.file_selected.emit(file_path)


    def on_item_double_clicked(self, item, column): #vers 1
        """Handle item double-click"""
        file_path = item.data(0, Qt.ItemDataRole.UserRole)
        if os.path.isdir(file_path):
            self.browse_directory(file_path)
        elif os.path.isfile(file_path):
            self.file_opened.emit(file_path)


    def show_context_menu(self, position): #vers 1
        """Show context menu"""
        item = self.tree.itemAt(position)
        if not item:
            return
        menu = QMenu(self)
        file_path = item.data(0, Qt.ItemDataRole.UserRole)
        if os.path.isfile(file_path):
            open_action = QAction("Open", self)
            open_action.setIcon(get_folder_icon())
            open_action.triggered.connect(lambda: self.file_opened.emit(file_path))
            menu.addAction(open_action)
            menu.addSeparator()
        copy_action = QAction("Copy", self)
        copy_action.setIcon(get_copy_icon())
        copy_action.triggered.connect(self.copy_files)
        menu.addAction(copy_action)
        cut_action = QAction("Cut", self)
        cut_action.setIcon(get_cut_icon())
        cut_action.triggered.connect(self.cut_files)
        menu.addAction(cut_action)
        paste_action = QAction("Paste", self)
        paste_action.setIcon(get_paste_icon())
        paste_action.triggered.connect(self.paste_files)
        menu.addAction(paste_action)
        menu.addSeparator()
        delete_action = QAction("Delete", self)
        delete_action.setIcon(get_trash_icon())
        delete_action.triggered.connect(self.delete_selected)
        menu.addAction(delete_action)
        rename_action = QAction("Rename", self)
        rename_action.setIcon(get_rename_icon())
        rename_action.triggered.connect(self.rename_selected)
        menu.addAction(rename_action)
        menu.addSeparator()
        copy_path_action = QAction("Copy Path", self)
        copy_path_action.setIcon(get_copy_icon())
        copy_path_action.triggered.connect(lambda: self.copy_path_to_clipboard(file_path))
        menu.addAction(copy_path_action)
        explorer_action = QAction("Open in File Manager", self)
        explorer_action.setIcon(get_folder_icon())
        explorer_action.triggered.connect(lambda: self.open_in_explorer(file_path))
        menu.addAction(explorer_action)
        menu.addSeparator()
        props_action = QAction("Properties", self)
        props_action.setIcon(get_properties_icon())
        props_action.triggered.connect(self.show_file_properties)
        menu.addAction(props_action)
        menu.exec(self.tree.mapToGlobal(position))


    def navigate_back(self): #vers 1
        """Navigate back in history"""
        if self.history_index > 0:
            self.history_index -= 1
            path = self.navigation_history[self.history_index]
            self.current_path = path
            self.address_bar.setText(path)
            self.populate_tree(path)


    def navigate_forward(self): #vers 1
        """Navigate forward in history"""
        if self.history_index < len(self.navigation_history) - 1:
            self.history_index += 1
            path = self.navigation_history[self.history_index]
            self.current_path = path
            self.address_bar.setText(path)
            self.populate_tree(path)


    def navigate_up(self): #vers 1
        """Navigate to parent directory"""
        if self.current_path:
            parent = os.path.dirname(self.current_path)
            if parent != self.current_path:
                self.browse_directory(parent)


    def navigate_home(self): #vers 1
        """Navigate to home directory"""
        home_path = os.path.expanduser("~")
        self.browse_directory(home_path)


    def navigate_to_address(self): #vers 1
        """Navigate to address bar path"""
        path = self.address_bar.text()
        if os.path.exists(path):
            self.browse_directory(path)
        else:
            QMessageBox.warning(self, "Invalid Path", f"Path does not exist: {path}")


    def create_new_folder(self): #vers 1
        """Create new folder"""
        if not self.current_path:
            QMessageBox.warning(self, "No Directory", "Navigate to a directory first")
            return
        name, ok = QInputDialog.getText(self, "New Folder", "Folder name:")
        if ok and name:
            new_path = os.path.join(self.current_path, name)
            try:
                os.makedirs(new_path, exist_ok=False)
                self.refresh_browser()
                self.log_message(f"Created folder: {name}")
            except FileExistsError:
                QMessageBox.warning(self, "Exists", f"Folder '{name}' already exists")
            except Exception as e:
                QMessageBox.critical(self, "Error", f"Failed to create folder: {str(e)}")


    def copy_files(self): #vers 1
        """Copy selected files to clipboard"""
        selected_items = self.tree.selectedItems()
        if selected_items:
            self.clipboard_files = [item.data(0, Qt.ItemDataRole.UserRole) for item in selected_items]
            self.clipboard_operation = 'copy'
            self.log_message(f"Copied {len(self.clipboard_files)} item(s)")


    def cut_files(self): #vers 2
        selected_items = self.tree.selectedItems()
        if selected_items:
            self.clipboard_files = [item.data(0, Qt.ItemDataRole.UserRole) for item in selected_items]
            self.clipboard_operation = 'cut'
            # No undo yet — actual move happens on paste
            self.log_message(f"Cut {len(self.clipboard_files)} item(s)")


    def paste_files(self): #vers 2
        if not self.clipboard_files or not self.current_path:
            return
        try:
            undo_entry = {
                'action': 'paste',
                'operation': self.clipboard_operation,
                'sources': self.clipboard_files[:],
                'destinations': [],
                'parent': self.current_path
            }
            for src in self.clipboard_files:
                if not os.path.exists(src):
                    continue
                dest = os.path.join(self.current_path, os.path.basename(src))
                undo_entry['destinations'].append(dest)
                if self.clipboard_operation == 'copy':
                    if os.path.isdir(src):
                        shutil.copytree(src, dest)
                    else:
                        shutil.copy2(src, dest)
                elif self.clipboard_operation == 'cut':
                    shutil.move(src, dest)
            self.refresh_browser()
            self.log_message(f"Pasted {len(self.clipboard_files)} item(s)")
            # Push to undo stack
            self.undo_stack.append(undo_entry)
            self.redo_stack.clear()  # Clear redo on new action
            if self.clipboard_operation == 'cut':
                self.clipboard_files = []
        except Exception as e:
            QMessageBox.critical(self, "Paste Error", f"Error: {str(e)}")


    def delete_selected(self): #vers 3
        selected_items = self.tree.selectedItems()
        if not selected_items:
            return
        reply = QMessageBox.question(
            self, "Move to Trash",
            f"Move {len(selected_items)} item(s) to trash?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
        )
        if reply == QMessageBox.StandardButton.Yes:
            try:
                from send2trash import send2trash
                paths = [item.data(0, Qt.ItemDataRole.UserRole) for item in selected_items]
                for path in paths:
                    if path and os.path.exists(path):
                        send2trash(path)
                self.refresh_browser()
                self.log_message(f"Moved {len(paths)} item(s) to trash")
                self.undo_stack.append({'action': 'trash', 'paths': paths})
                self.redo_stack.clear()
            except ImportError:
                QMessageBox.critical(self, "Missing Dependency", "send2trash not installed.\nRun: pip install send2trash")
            except Exception as e:
                QMessageBox.critical(self, "Trash Error", f"Error: {str(e)}")


    def rename_selected(self): #vers 2
        selected_items = self.tree.selectedItems()
        if not selected_items:
            return
        item = selected_items[0]
        old_path = item.data(0, Qt.ItemDataRole.UserRole)
        old_name = os.path.basename(old_path)
        new_name, ok = QInputDialog.getText(self, "Rename", "New name:", text=old_name)
        if ok and new_name and new_name != old_name:
            try:
                new_path = os.path.join(os.path.dirname(old_path), new_name)
                os.rename(old_path, new_path)
                self.refresh_browser()
                self.log_message(f"Renamed: {old_name} → {new_name}")
                self.undo_stack.append({'action': 'rename', 'old': old_path, 'new': new_path})
                self.redo_stack.clear()
            except Exception as e:
                QMessageBox.critical(self, "Rename Error", f"Error: {str(e)}")


    def undo_selected(self): #vers 2
        if not self.undo_stack:
            self.log_message("Nothing to undo")
            return
        action = self.undo_stack.pop()
        self.redo_stack.append(action)
        try:
            if action['action'] in ('paste', 'copy'):
                # Remove copied/pasted items
                for dest in action.get('destinations', action.get('paths', [])):
                    if os.path.isdir(dest):
                        shutil.rmtree(dest)
                    elif os.path.isfile(dest):
                        os.remove(dest)
                self.log_message(f"Undid {action['action']}")
            elif action['action'] == 'trash':
                QMessageBox.information(self, "Undo Trash",
                    "Items were moved to trash.\nRestore them manually from your system trash.")
            elif action['action'] == 'rename':
                os.rename(action['new'], action['old'])
                self.log_message(f"Undid rename: {os.path.basename(action['new'])} → {os.path.basename(action['old'])}")
            self.refresh_browser()
        except Exception as e:
            self.log_message(f"Undo failed: {str(e)}")


    def redo_selected(self): #vers 1
        if not self.redo_stack:
            self.log_message("Nothing to redo")
            return
        action = self.redo_stack.pop()
        self.undo_stack.append(action)
        try:
            if action['action'] == 'paste':
                # Re-paste
                op = action['operation']
                parent = action['parent']
                for src in action['sources']:
                    dest = os.path.join(parent, os.path.basename(src))
                    if op == 'copy':
                        if os.path.isdir(src):
                            shutil.copytree(src, dest)
                        else:
                            shutil.copy2(src, dest)
                    elif op == 'cut':
                        shutil.move(src, dest)
                self.log_message("Redid paste")
            elif action['action'] == 'delete':
                QMessageBox.warning(self, "Redo Limitation", "Deleted files cannot be restored.")
            elif action['action'] == 'rename':
                os.rename(action['old'], action['new'])
                self.log_message(f"Redid rename: {os.path.basename(action['old'])} → {os.path.basename(action['new'])}")
            self.refresh_browser()
        except Exception as e:
            self.log_message(f"Redo failed: {str(e)}")


    def copy_path_to_clipboard(self, file_path: str): #vers 1
        """Copy file path to clipboard"""
        try:
            from PyQt6.QtWidgets import QApplication
            clipboard = QApplication.clipboard()
            clipboard.setText(file_path)
            self.log_message(f"Path copied: {file_path}")
        except Exception as e:
            self.log_message(f"Error copying path: {str(e)}")


    def open_in_explorer(self, file_path: str): #vers 1
        """Open file in system file manager"""
        try:
            system = platform.system()
            if system == "Linux":
                subprocess.Popen(['xdg-open', os.path.dirname(file_path)])
            elif system == "Darwin":
                subprocess.Popen(['open', '-R', file_path])
            elif system == "Windows":
                subprocess.run(['explorer', '/select,', file_path])
        except Exception as e:
            self.log_message(f"Error opening file manager: {str(e)}")


    def show_file_properties(self): #vers 1
        """Show file properties"""
        selected_items = self.tree.selectedItems()
        if not selected_items:
            return
        file_path = selected_items[0].data(0, Qt.ItemDataRole.UserRole)
        try:
            stats = os.stat(file_path)
            info = f"File: {os.path.basename(file_path)}\n"
            info += f"Path: {file_path}\n"
            info += f"Size: {stats.st_size:,} bytes\n"
            QMessageBox.information(self, "Properties", info)
        except Exception as e:
            QMessageBox.warning(self, "Error", f"Could not get properties: {str(e)}")


    def show_file_search_dialog(self): #vers 1
        """Show file search dialog"""
        dialog = QDialog(self)
        dialog.setWindowTitle("Search Files")
        dialog.setModal(True)
        layout = QVBoxLayout(dialog)
        filename_input = QLineEdit()
        filename_input.setPlaceholderText("Filename pattern (e.g., *.img)")
        options_layout = QHBoxLayout()
        options_layout.addWidget(QLabel("Filename:"))
        options_layout.addWidget(filename_input)
        layout.addLayout(options_layout)
        exact_match = QCheckBox("Exact match only")
        layout.addWidget(exact_match)
        results_list = QListWidget()
        layout.addWidget(results_list)
        button_layout = QHBoxLayout()
        search_btn = QPushButton("Search")
        search_btn.setIcon(get_search_icon())
        button_layout.addWidget(search_btn)
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(dialog.accept)
        button_layout.addWidget(close_btn)
        layout.addLayout(button_layout)
        def perform_search():
            results_list.clear()
            pattern = filename_input.text().strip()
            if not pattern:
                QMessageBox.warning(dialog, "No Pattern", "Enter a filename pattern")
                return
            results = self.search_files(self.current_path, pattern, exact_match.isChecked())
            if results:
                for file_path in results:
                    results_list.addItem(file_path)
                    self.log_message(f"Search found {len(results)} files matching '{pattern}'")

                for file_path in results:
                    self.log_message(f"  • {file_path}")

                # Also send to main window's activity window if available
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"Directory search found {len(results)} files matching '{pattern}'")
                    for file_path in results[:10]:  # Limit to first 10 to avoid flooding
                        self.main_window.log_message(f"  Found: {file_path}")
                    if len(results) > 10:
                        self.main_window.log_message(f"  ... and {len(results)-10} more files")
            else:
                results_list.addItem("No files found")
                self.log_message(f"No files found matching '{pattern}'")
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"Directory search: No files found matching '{pattern}'")

        search_btn.clicked.connect(perform_search)
        dialog.exec()


    def search_files(self, search_path: str, pattern: str, exact_match: bool) -> List[str]: #vers 1
        """Search for files by name"""
        found_files = []
        try:
            for root, dirs, files in os.walk(search_path):
                for filename in files:
                    if exact_match:
                        if filename.lower() == pattern.lower():
                            found_files.append(os.path.join(root, filename))
                    else:
                        if fnmatch.fnmatch(filename.lower(), pattern.lower()):
                            found_files.append(os.path.join(root, filename))
        except Exception as e:
            self.log_message(f"Search error: {str(e)}")
        return found_files


    def file_contains_text(self, file_path: str, search_text: str) -> bool: #vers 1
        """Check if file contains text"""
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                return search_text.lower() in content.lower()
        except Exception:
            return False


    def refresh_browser(self): #vers 1
        """Refresh current view"""
        if self.current_path:
            self.populate_tree(self.current_path)
            self.log_message("Refreshed")


    def load_browser_settings(self) -> dict: #vers 1
        """Load browser settings"""
        settings = QSettings("IMG_Factory", "FileBrowser")
        return {
            'show_hidden': settings.value('show_hidden', False, type=bool),
        }


    def save_browser_settings(self): #vers 1
        """Save browser settings"""
        settings = QSettings("IMG_Factory", "FileBrowser")
        for key, value in self.browser_settings.items():
            settings.setValue(key, value)


    def log_message(self, message: str): #vers 2
        """Send log to main window if available, else print"""
        if hasattr(self.main_window, 'log_message'):
            self.main_window.log_message(message)
        else:
            print(f"[DirectoryBrowser] {message}")


def integrate_directory_tree_browser(main_window): #vers 4
    """Integrate directory browser into main window - updated for button-based UI"""
    import os
    from pathlib import Path
    try:
        # Check if directory tree already exists to avoid duplication
        if hasattr(main_window, 'directory_tree') and main_window.directory_tree:
            main_window.log_message("Directory tree already exists")
            return True

        # First, create the directory browser
        directory_browser = DirectoryTreeBrowser(main_window)
        
        # Store it in main window for later access
        main_window.directory_tree = directory_browser
        
        # Connect file opening if available
        if hasattr(main_window, 'load_file_unified'):
            directory_browser.file_opened.connect(main_window.load_file_unified)
            
        # Check if we have a tab widget (for backwards compatibility)

        if hasattr(main_window, 'main_tab_widget') and main_window.main_tab_widget:
            tab_widget = main_window.main_tab_widget

            # Get Tab 0
            if tab_widget.count() > 0:
                tab_0 = tab_widget.widget(0)

                # Find the main_splitter in Tab 0
                from PyQt6.QtWidgets import QSplitter
                main_splitter = None
                for child in tab_0.children():
                    if isinstance(child, QSplitter) and child.orientation() == Qt.Orientation.Horizontal:
                        main_splitter = child
                        break

                if main_splitter and main_splitter.count() > 0:
                    # Get middle_panel (widget 0 of main_splitter)
                    middle_panel = main_splitter.widget(0)

                    # Find middle_vertical_splitter inside middle_panel
                    middle_vertical_splitter = None
                    for child in middle_panel.findChildren(QSplitter):
                        if child.orientation() == Qt.Orientation.Vertical:
                            middle_vertical_splitter = child
                            break

                    if middle_vertical_splitter and middle_vertical_splitter.count() > 0:
                        # Replace widget 0 (file_window) with directory_browser
                        file_window = middle_vertical_splitter.widget(0)
                        file_window.hide()
                        middle_vertical_splitter.insertWidget(0, directory_browser)
                        main_window.log_message("✓ Directory tree in file window area")

                        tab_widget.setTabText(0, "Dir Tree")
                        tab_widget.setTabIcon(0, get_folder_icon())
                    else:
                        main_window.log_message("⚠ Could not find middle_vertical_splitter")
                else:
                    main_window.log_message("⚠ Could not find main_splitter in Tab 0")

        else:
            # For button-based UI - we need to make sure the directory browser is accessible
            # It can be added to a central widget or managed by the switch functions
            main_window.log_message("No tab widget found - directory tree stored for button-based UI")

        # Load saved game root if available
        settings = QSettings("IMG-Factory", "IMG-Factory")
        saved_root = settings.value("game_root", "", type=str)
        if saved_root and os.path.exists(saved_root):
            directory_browser.browse_directory(saved_root)
            main_window.log_message(f"Loaded saved game root: {saved_root}")
        elif hasattr(main_window, 'game_root') and main_window.game_root:
            # Use the game_root from the main window if available
            directory_browser.browse_directory(main_window.game_root)
            main_window.log_message(f"Loaded project game root: {main_window.game_root}")
        else:
            # If no specific project root is set, browse to workspace directory
            import os
            workspace_dir = os.getcwd()  # Start with current working directory
            # Or check if there's a projects.json to determine the project directory
            projects_file = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(__file__))), "projects.json")
            if os.path.exists(projects_file):
                try:
                    import json
                    with open(projects_file, 'r') as f:
                        projects_data = json.load(f)
                    # The projects.json is a dictionary with project names as keys
                    # Get the first project's game_root or project_folder
                    if isinstance(projects_data, dict) and projects_data:
                        first_project_key = next(iter(projects_data.keys()))
                        first_project = projects_data[first_project_key]
                        if isinstance(first_project, dict):
                            # Try to get game_root first, then project_folder
                            project_path = first_project.get('game_root', first_project.get('project_folder', workspace_dir))
                            if os.path.exists(project_path):
                                directory_browser.browse_directory(project_path)
                                main_window.log_message(f"Loaded project directory: {project_path}")
                            else:
                                directory_browser.browse_directory(workspace_dir)
                                main_window.log_message(f"Loaded workspace directory: {workspace_dir}")
                        else:
                            directory_browser.browse_directory(workspace_dir)
                            main_window.log_message(f"Loaded workspace directory: {workspace_dir}")
                    else:
                        directory_browser.browse_directory(workspace_dir)
                        main_window.log_message(f"Loaded workspace directory: {workspace_dir}")
                except Exception as e:
                    directory_browser.browse_directory(workspace_dir)
                    main_window.log_message(f"Loaded workspace directory (fallback): {workspace_dir}")
            else:
                directory_browser.browse_directory(workspace_dir)
                main_window.log_message(f"Loaded workspace directory: {workspace_dir}")

        main_window.log_message("Directory browser integrated")
        return True
    except Exception as e:
        main_window.log_message(f"Error integrating directory browser: {str(e)}")
        import traceback
        traceback.print_exc()
        return False

__all__ = [
    'DirectoryTreeBrowser',
    'integrate_directory_tree_browser'
]
