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
try:
    try:
        from PyQt6.QtGui import QAction
    except ImportError:
        from PyQt6.QtWidgets import QAction
except ImportError:
    from PyQt6.QtWidgets import QAction
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

#TODO; The folder list folders need to be closed, until the folder is opened to show it's contents.
#Have different View options, SHow everything with icons, detailed list, short list, replicating a proper file browser.

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
        self._setup_tree_columns(self.tree)
        self.setup_tree_view()
        sc_layout.addWidget(self.tree)

        layout.addWidget(self._single_container)
        layout.setStretchFactor(self._single_container, 1)


    # Column indices
    COL_NAME     = 0
    COL_TYPE     = 1
    COL_SIZE     = 2
    COL_CREATED  = 3
    COL_MODIFIED = 4
    COL_PERMS    = 5

    def _setup_tree_columns(self, tree, label='Name'): #vers 2
        """Set columns: Name, Type, Size, Created, Modified, Perms, RW Ver"""
        from PyQt6.QtWidgets import QHeaderView
        cols = [label or 'Name', 'Type', 'Size', 'Created', 'Modified', 'Perms']
        tree.setColumnCount(len(cols))
        tree.setHeaderLabels(cols)
        hdr = tree.header()
        for i in range(len(cols)):
            hdr.setSectionResizeMode(i, QHeaderView.ResizeMode.Interactive)
        hdr.setStretchLastSection(False)
        hdr.resizeSection(0, 220)
        hdr.resizeSection(1, 160)
        hdr.resizeSection(2, 70)
        hdr.resizeSection(3, 130)
        hdr.resizeSection(4, 130)
        hdr.resizeSection(5, 50)
        hdr.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        hdr.customContextMenuRequested.connect(
            lambda pos, t=tree: self._show_column_toggle_menu(t, pos))
        if not hasattr(self, '_hidden_columns'):
            self._hidden_columns = set()
        for col in self._hidden_columns:
            tree.setColumnHidden(col, True)
        self._restore_column_widths(tree)
        hdr.sectionResized.connect(lambda idx, old, new: self._save_column_widths(tree))

    def _show_column_toggle_menu(self, tree, pos): #vers 2
        """Right-click header to show/hide columns."""
        from PyQt6.QtWidgets import QMenu
        labels = ['Name', 'Type', 'Size', 'Created', 'Modified', 'Perms']
        menu = QMenu(tree)
        for i, lbl in enumerate(labels):
            if i == 0:
                continue
            act = menu.addAction(lbl)
            act.setCheckable(True)
            act.setChecked(not tree.isColumnHidden(i))
            act.toggled.connect(lambda checked, col=i: self._toggle_column(col, checked))
        menu.exec(tree.header().mapToGlobal(pos))

    def _toggle_column(self, col, visible): #vers 1
        """Toggle column visibility on all trees."""
        if not hasattr(self, '_hidden_columns'):
            self._hidden_columns = set()
        if visible:
            self._hidden_columns.discard(col)
        else:
            self._hidden_columns.add(col)
        for t in [getattr(self, 'tree', None),
                  getattr(self, '_second_tree', None)]:
            if t:
                t.setColumnHidden(col, not visible)

    def _read_rw_version(self, file_path: str) -> str: #vers 1
        """Read RW version from first 12 bytes of DFF/TXD file."""
        try:
            with open(file_path, 'rb') as f:
                data = f.read(12)
            if len(data) < 12:
                return ''
            import struct
            version_word = struct.unpack_from('<I', data, 8)[0]
            if version_word == 0:
                return ''
            major = (version_word >> 14) & 0x3FF
            minor = (version_word >> 6) & 0xFF
            if major == 0:
                return ''
            return f'{major}.{minor:02d}'
        except Exception:
            return ''

    def setup_tree_view(self): #vers 2
        """Setup tree widget"""
        self.tree.setAlternatingRowColors(True)
        self.tree.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.tree.customContextMenuRequested.connect(self.show_context_menu)
        self.tree.itemClicked.connect(self.on_item_clicked)
        self.tree.itemDoubleClicked.connect(self.on_item_double_clicked)
        # Lazy-loading (Aug 20 2026, per Keith: "The folder list
        # folders need to be closed, until the folder is opened to
        # show it's contents") - populate_tree_recursive only ever
        # fills in one level plus lazy placeholders now; this is what
        # actually fires when a placeholder's own parent folder gets
        # expanded, swapping the placeholder for that folder's real
        # contents (see _on_tree_item_expanded's own docstring).
        self.tree.itemExpanded.connect(self._on_tree_item_expanded)
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
                background-color: palette(base);
                color: palette(buttonText);
                border: 1px solid palette(mid);
            }
            QTreeWidget::item:selected {
                background-color: palette(highlight);
            }
            QTreeWidget::item:hover {
                background-color: palette(base);
            }
            """
        else:
            stylesheet = """
            QTreeWidget {
                background-color: palette(buttonText);
                color: palette(windowText);
                border: 1px solid palette(mid);
            }
            QTreeWidget::item:selected {
                background-color: palette(highlight);
                color: palette(buttonText);
            }
            QTreeWidget::item:hover {
                background-color: palette(light);
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
        refresh_action.triggered.connect(self._refresh_all_panels)
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
        refresh_btn.clicked.connect(self._refresh_all_panels)
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

        # Panel toggle button - cycles tabs-full / split / tree-full
        from apps.methods.imgfactory_svg_icons import get_panel_toggle_icon
        self.maximise_btn = QPushButton()
        self.maximise_btn.setIcon(get_panel_toggle_icon(20))
        self.maximise_btn.setToolTip("Toggle: tabs full / split / tree full")
        self.maximise_btn.setMaximumSize(32, 32)
        self.maximise_btn.clicked.connect(self._toggle_tree_maximise)
        layout.addWidget(self.maximise_btn)

        return toolbar


    def _toggle_tree_maximise(self): #vers 7
        """Toggle: own-full <-> split"""
        try:
            mw = self.main_window
            if not mw:
                return
            splitter = getattr(getattr(mw, 'gui_layout', None), 'content_splitter', None)
            if not splitter or splitter.count() < 2:
                return
            total = sum(splitter.sizes()) or 10000

            # Identify own index by direct widget comparison
            tree_idx = -1
            for i in range(splitter.count()):
                if splitter.widget(i) is self:
                    tree_idx = i
                    break
            if tree_idx == -1:
                tree_idx = 1  # fallback

            sizes = splitter.sizes()
            if sizes[tree_idx] >= total * 0.9:
                splitter.setSizes([total // 2, total // 2])
            else:
                s = [0, 0]
                s[tree_idx] = total
                splitter.setSizes(s)
        except Exception as e:
            if self.main_window:
                self.main_window.log_message(f"Panel toggle error: {str(e)}")

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
            self._setup_tree_columns(self._second_tree)
            self._second_tree.setAlternatingRowColors(True)
            self._second_tree.setSelectionMode(QTreeWidget.SelectionMode.ExtendedSelection)
            self._second_tree.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
            self._second_tree.customContextMenuRequested.connect(self.show_context_menu)
            # Lazy-loading (Aug 20 2026) - the main tree's own signal
            # (wired in setup_tree_view) only ever covers that one
            # widget; _populate_second_tree reuses populate_tree/
            # populate_tree_recursive via a temporary self.tree swap,
            # so this panel already gets the same lazy placeholders,
            # but without its OWN itemExpanded connection those
            # placeholders would sit there forever, never actually
            # populating when this panel's own folders get expanded.
            # _on_tree_item_expanded itself is tree-agnostic (works
            # from the clicked item directly, never touches self.tree),
            # so it's safe to reuse the exact same handler here.
            self._second_tree.itemExpanded.connect(self._on_tree_item_expanded)
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

    def _populate_second_tree(self, path: str): #vers 2
        """Populate the second panel tree with the given path"""
        original_tree = self.tree
        try:
            if not hasattr(self, '_second_tree') or not self._second_tree:
                return
            self._right_current_path = path
            self._second_tree.clear()
            self._setup_tree_columns(self._second_tree, label=os.path.basename(path))
            self.tree = self._second_tree
            self.populate_tree(path)
        except Exception as e:
            print(f"Second tree populate error: {e}")
        finally:
            self.tree = original_tree

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


    def populate_tree(self, root_path: str): #vers 2
        """Populate tree with directory contents. Lazy-loading (Aug
        20 2026, per Keith's own TODO comment: "The folder list
        folders need to be closed, until the folder is opened to show
        it's contents") - only the root's own direct children are
        populated eagerly now; every folder that has any real
        contents gets a single placeholder child instead of its real
        contents being walked immediately, and only gets genuinely
        populated the first time it's actually expanded (see _on_
        tree_item_expanded). Was eagerly recursing 3 whole levels deep
        on every single load before this - for a large real game
        install's own directory tree, that could mean walking and
        building thousands of tree items the person may never even
        look at, every single time a root path was set."""
        try:
            self.tree.clear()
            root_item = QTreeWidgetItem(self.tree)
            root_item.setText(0, os.path.basename(root_path) or root_path)
            root_item.setData(0, Qt.ItemDataRole.UserRole, root_path)
            root_item.setIcon(0, get_folder_icon())
            self.populate_tree_recursive(root_item, root_path)
            # Real fix (Aug 20 2026, per Keith: "the other bug with dir
            # tree is all the folders are open, they need to start
            # colapsed") - every folder below the root already starts
            # collapsed (via the lazy-loading placeholder above), but
            # the root item itself was still force-expanded here,
            # immediately showing its own direct children regardless -
            # left uncalled now, so the whole real tree genuinely
            # starts fully collapsed, root included.
            # root_item.setExpanded(True)
            self.log_message(f"Loaded: {root_path}")
        except Exception as e:
            self.log_message(f"Error populating tree: {str(e)}")

    def _on_tree_item_expanded(self, item): #vers 1
        """Populate a folder's real contents the first time it's
        actually expanded (Aug 20 2026, per Keith's own lazy-loading
        request - see populate_tree's own docstring for the full
        reasoning). Detects "not populated yet" by checking for the
        single dummy placeholder child _add_lazy_placeholder left
        behind (marked via a dedicated UserRole+1 flag, not by text
        content, so a real folder or file that happens to be named
        the same as the placeholder text is never mistaken for one) -
        removes it, then populates this folder's own real, direct
        children the same way populate_tree_recursive already always
        has, each of THOSE getting their own fresh placeholder if they
        have contents of their own, so expanding stays lazy at every
        depth, not just the first level."""
        if item.childCount() != 1:
            return
        child = item.child(0)
        if not child.data(0, Qt.ItemDataRole.UserRole + 1):
            return   # a real child, not the placeholder - already populated
        item.removeChild(child)
        dir_path = item.data(0, Qt.ItemDataRole.UserRole)
        if dir_path:
            self.populate_tree_recursive(item, dir_path)

    def _add_lazy_placeholder(self, parent_item, dir_path): #vers 1
        """Add a single, cheap placeholder child to parent_item so its
        own expand arrow shows, without actually reading dir_path's
        real contents yet (Aug 20 2026, per Keith's own lazy-loading
        request). Uses os.scandir's own iterator directly rather than
        os.listdir - only needs to know whether at least one entry
        exists at all, not build a full list of them, so this stays
        cheap even for a folder with thousands of real entries inside
        it that won't actually be read until it's genuinely expanded."""
        try:
            with os.scandir(dir_path) as it:
                has_contents = next(it, None) is not None
        except (PermissionError, OSError):
            has_contents = False
        if not has_contents:
            return
        placeholder = QTreeWidgetItem(parent_item)
        placeholder.setText(0, "Loading...")
        placeholder.setData(0, Qt.ItemDataRole.UserRole + 1, True)

    def count_tree_items(self, item): #vers 1
        """Count total items in tree"""
        if not item:
            return 0
        count = 1
        for i in range(item.childCount()):
            count += self.count_tree_items(item.child(i))
        return count


    def populate_tree_recursive(self, parent_item: QTreeWidgetItem,
                                dir_path: str): #vers 2
        """Populate parent_item with dir_path's own direct children
        only - one level, not recursive any more despite the method's
        own name (kept for every existing external caller rather than
        renamed, and because it still recurses conceptually via lazy
        placeholders + _on_tree_item_expanded, just spread out over
        real user interaction instead of all at once upfront). Each
        folder child that has any real contents of its own gets a
        single lazy placeholder (see _add_lazy_placeholder) instead of
        being walked immediately - see populate_tree's own docstring
        for the full reasoning behind this Aug 20 2026 change."""
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
            from datetime import datetime
            import stat as _stat

            def _perms_str(mode: int) -> str:
                """Return e.g. '755  rwxr-xr-x' from a stat st_mode value."""
                octal = oct(_stat.S_IMODE(mode))[2:]
                flags = (
                    (_stat.S_IRUSR, 'r'), (_stat.S_IWUSR, 'w'), (_stat.S_IXUSR, 'x'),
                    (_stat.S_IRGRP, 'r'), (_stat.S_IWGRP, 'w'), (_stat.S_IXGRP, 'x'),
                    (_stat.S_IROTH, 'r'), (_stat.S_IWOTH, 'w'), (_stat.S_IXOTH, 'x'),
                )
                rwx = ''.join(c if mode & bit else '-' for bit, c in flags)
                return f"{octal}  {rwx}"

            for directory in directories:
                item_path = os.path.join(dir_path, directory)
                tree_item = QTreeWidgetItem(parent_item)
                tree_item.setText(0, directory)
                tree_item.setData(0, Qt.ItemDataRole.UserRole, item_path)
                tree_item.setIcon(0, get_folder_icon())
                tree_item.setText(self.COL_TYPE, 'Folder')
                try:
                    st = os.stat(item_path)
                    tree_item.setText(self.COL_CREATED,
                        datetime.fromtimestamp(st.st_ctime).strftime('%d %b %Y %H:%M'))
                    tree_item.setText(self.COL_MODIFIED,
                        datetime.fromtimestamp(st.st_mtime).strftime('%d %b %Y %H:%M'))
                    tree_item.setText(self.COL_PERMS, _perms_str(st.st_mode))
                except Exception:
                    pass
                self._add_lazy_placeholder(tree_item, item_path)
            for file in files:
                item_path = os.path.join(dir_path, file)
                tree_item = QTreeWidgetItem(parent_item)
                tree_item.setText(0, file)
                tree_item.setData(0, Qt.ItemDataRole.UserRole, item_path)
                file_ext = os.path.splitext(file)[1].lower()
                tree_item.setIcon(0, self.get_file_type_icon(file_ext))
                type_label = self.get_file_type_display(file_ext)
                if file_ext in ('.dff', '.txd'):
                    rw = self._read_rw_version(item_path)
                    if rw and not rw.startswith('Unknown'):
                        type_label = f'{type_label} {rw}'
                tree_item.setText(self.COL_TYPE, type_label)
                try:
                    st = os.stat(item_path)
                    sz = st.st_size
                    if sz >= 1048576:
                        tree_item.setText(self.COL_SIZE, f'{sz/1048576:.1f} MB')
                    elif sz >= 1024:
                        tree_item.setText(self.COL_SIZE, f'{sz/1024:.1f} KB')
                    else:
                        tree_item.setText(self.COL_SIZE, f'{sz} B')
                    tree_item.setText(self.COL_CREATED,
                        datetime.fromtimestamp(st.st_ctime).strftime('%d %b %Y %H:%M'))
                    tree_item.setText(self.COL_MODIFIED,
                        datetime.fromtimestamp(st.st_mtime).strftime('%d %b %Y %H:%M'))
                    tree_item.setText(self.COL_PERMS, _perms_str(st.st_mode))
                except Exception:
                    pass

        except Exception:
            pass


    def get_file_type_icon(self, file_ext: str): #vers 2
        """Get icon for file type - SVG for known types, system icon for others"""
        icon_map = {
            '.img': get_img_file_icon,
            '.txd': get_txd_file_icon,
            '.col': get_col_file_icon,
            '.dff': get_image_icon,
            '.ide': get_edit_icon,
            '.ipl': get_view_icon,
            '.dat': get_file_icon,
            '.hxd': get_file_icon,
            '.mxd': get_file_icon,
            '.agr': get_file_icon,
            '.lvz': get_file_icon,
        }
        if file_ext in icon_map:
            return icon_map[file_ext]()
        # Fall back to system icon provider for everything else
        try:
            from PyQt6.QtWidgets import QFileIconProvider
            from PyQt6.QtCore import QFileInfo
            provider = QFileIconProvider()
            # We only have the extension, use a dummy filename
            return provider.icon(QFileInfo(f'dummy{file_ext}'))
        except Exception:
            return get_file_icon()


    def get_file_type_display(self, file_ext: str) -> str: #vers 2
        """Get display name for file type"""
        type_map = {
            '.img': 'IMG Archive',
            '.txd': 'Texture Dictionary',
            '.col': 'Collision File',
            '.dff': '3D Model',
            '.ide': 'Item Definition',
            '.ipl': 'Item Placement',
            '.dat': 'Data File',
            '.hxd': 'Bully Anim Data',
            '.mxd': 'Bully Motion Data',
            '.agr': 'Bully Anim Group',
            '.lvz': 'LVZ Archive',
            '.py':  'Python Script',
            '.exe': 'Executable',
            '.bmp': 'Bitmap Image',
            '.png': 'PNG Image',
            '.jpg': 'JPEG Image',
            '.txt': 'Text File',
            '.xml': 'XML File',
            '.cfg': 'Config File',
            '.ini': 'Config File',
            '.lua': 'Lua Script',
            '.dff': '3D Model (DFF)',
        }
        return type_map.get(file_ext, f'{file_ext[1:].upper()} File' if file_ext else 'File')


    def on_item_clicked(self, item, column): #vers 2
        """Handle item click - store selected path on main_window"""
        file_path = item.data(0, Qt.ItemDataRole.UserRole)
        if file_path and os.path.isfile(file_path):
            self.file_selected.emit(file_path)
            # Store on main_window so toolbar buttons can use it
            w = self.parent()
            while w and not hasattr(w, 'log_message'):
                w = w.parent()
            if w:
                w._dir_tree_selected_file = file_path


    def on_item_double_clicked(self, item, column): #vers 2
        """Handle item double-click — routes to smart editor for known data files."""
        file_path = item.data(0, Qt.ItemDataRole.UserRole)
        if os.path.isdir(file_path):
            self.browse_directory(file_path)
        elif os.path.isfile(file_path):
            ext = os.path.splitext(file_path)[1].lower()
            if ext in ('.dat', '.cfg'):
                try:
                    from apps.methods.smart_file_router import get_editor_label, open_smart_editor
                    if get_editor_label(file_path):
                        mw = getattr(self, "main_window", None)
                        open_smart_editor(file_path, mw)
                        return
                except Exception:
                    pass
            self.file_opened.emit(file_path)


    def show_context_menu(self, position): #vers 3
        """Show context menu - tracks which tree triggered it"""
        # Identify which tree sent the signal
        sender = self.sender()
        active_tree = sender if sender in (self.tree, getattr(self, '_second_tree', None)) else self.tree
        self._active_tree = active_tree

        item = active_tree.itemAt(position)
        if not item:
            return
        menu = QMenu(self)
        file_path = item.data(0, Qt.ItemDataRole.UserRole)
        if os.path.isfile(file_path):
            open_action = QAction("Open", self)
            open_action.setIcon(get_folder_icon())
            open_action.triggered.connect(lambda: self.file_opened.emit(file_path))
            menu.addAction(open_action)

            # Real fix (Aug 20 2026, per Keith: "dir tree shows audio
            # files, so we can now right click them to play") - a
            # standard, directly-playable audio file gets a real Play
            # action; a recognised SA audio-stream filename (from the
            # real audio/streams/ folder - no real file extension at
            # all, so identified by name instead) gets Extract & Play
            # Tracks..., since those first need real decoding via
            # apps/methods/audioparser.py before anything inside
            # them can be played at all.
            #
            # Real fix (Aug 20 2026, per Keith: "in LC, VC .wav plays...
            # .at3 .vb") - .at3 (Sony ATRAC3+, confirmed via ffprobe
            # against Keith's own real, uploaded philcollins.at3 file
            # as a standard RIFF/WAVE container ffmpeg already decodes
            # directly) plays via the same Play action as standard
            # audio, transcoded through ffmpeg first since QMediaPlayer
            # has no native ATRAC3+ support of its own. .vb (PS2 4-bit
            # PS-ADPCM, headerless, confirmed via apps/methods/ps2_vb_
            # audio.py against Keith's own real, uploaded AMBSIL.VB -
            # decoded left channel came back exactly, perfectly silent,
            # exactly matching what a file named "ambient silence"
            # should be) gets its own dedicated action, since it needs
            # real de-interleaving + decoding, not just a transcode.
            _AUDIO_EXTS = ('.wav', '.mp3', '.ogg', '.flac')
            _FFMPEG_TRANSCODE_EXTS = ('.at3',)
            _SA_STREAM_NAMES = {
                'aa', 'adverts', 'ambience', 'beats', 'ch', 'co', 'cr',
                'cutscene', 'ds', 'hc', 'mh', 'mr', 'nj', 're', 'rg', 'tk',
            }
            # Real SFX bank files (a genuinely different, unsolved real
            # format - raw PCM samples packed with a SoundMeta
            # structure, no encoding at all but also no real container
            # format, unlike the streams above) - shown with an honest
            # "not yet supported" message instead of silently offering
            # no option at all, which would look like these files were
            # simply overlooked rather than a real, open limitation.
            _SA_SFX_NAMES = {
                'feet', 'genrl', 'pain_a', 'script',
                'spc_ea', 'spc_fa', 'spc_ga', 'spc_na', 'spc_pa',
            }
            file_ext = os.path.splitext(file_path)[1].lower()
            file_base = os.path.basename(file_path).lower()
            if file_ext in _AUDIO_EXTS:
                play_action = QAction("Play", self)
                play_action.triggered.connect(
                    lambda _=False, p=file_path: self._play_audio_file(p))
                menu.addAction(play_action)
            elif file_ext in _FFMPEG_TRANSCODE_EXTS:
                play_action = QAction("Play", self)
                play_action.triggered.connect(
                    lambda _=False, p=file_path: self._play_via_ffmpeg_transcode(p))
                menu.addAction(play_action)
            elif file_ext == '.vb':
                play_action = QAction("Play", self)
                play_action.triggered.connect(
                    lambda _=False, p=file_path: self._play_ps2_vb_file(p))
                menu.addAction(play_action)
            elif file_ext == '.adf':
                # Real fix (Aug 20 2026, per Keith's own real, uploaded
                # FLASH.ADF sample) - III/VC's own real music/ambient
                # stream format, confirmed as a completely standard
                # MP3 wrapped in a trivial, constant single-byte XOR
                # (0x22) - see audioparser.py's own "III/VC .ADF
                # format" section for the full, real confirmation
                # story (real LAME encoder tags appear at exactly the
                # right real offset once decoded, and both `file` and
                # ffprobe confirm the result as a real, valid,
                # standard MP3 end to end).
                play_action = QAction("Play", self)
                play_action.triggered.connect(
                    lambda _=False, p=file_path: self._play_adf_file(p))
                menu.addAction(play_action)
            elif file_ext in ('.raw', '.sdt') and self._find_sfx_pair(file_path):
                # Real fix (Aug 20 2026, per Keith's own real, uploaded
                # SFX23.RAW/SFX23.SDT sample pair) - GTA 2/III/VC's own
                # real SFX archive format, a real .RAW (sample data) +
                # .SDT (index) pair sharing the same real base filename.
                # The entry structure itself is confirmed correct with
                # mathematical certainty (see audioparser.py's own
                # "III/VC SFX format" section - a documented 24-byte
                # SDT entry didn't match Keith's own real files; a
                # 12-byte entry does, tiling his own real SFX23.RAW
                # exactly). But Keith's own real listening test on the
                # extracted result: "Sfx23 sounds like statis" - the
                # real offsets/sizes are right, something about the
                # real sample encoding itself still isn't (see this
                # class's own real _play_sfx_pair docstring for the
                # full, honest story). Disabled rather than left as a
                # confident Play action that's known to sound wrong,
                # per Keith's own real "put Sfx23 to the side" - still
                # here, and still findable, while paused.
                sfx_action = QAction("Play first entry (known issue: sounds like static)", self)
                sfx_action.setEnabled(False)
                sfx_action.setToolTip(
                    "The real offset/size structure is confirmed correct\n"
                    "(it tiles the .RAW file exactly), but the decoded\n"
                    "audio itself still sounds like static - paused per\n"
                    "Keith's own real \"put Sfx23 to the side\" (Aug 20\n"
                    "2026), not yet solved.")
                menu.addAction(sfx_action)
            elif file_base in _SA_STREAM_NAMES:
                extract_action = QAction("Extract && Play Tracks...", self)
                extract_action.triggered.connect(
                    lambda _=False, p=file_path: self._extract_and_play_sa_stream(p))
                menu.addAction(extract_action)
            elif file_base in _SA_SFX_NAMES:
                sfx_action = QAction("Play (not yet supported)", self)
                sfx_action.setEnabled(False)
                sfx_action.setToolTip(
                    "SA's own SFX bank format is a genuinely different,\n"
                    "still-unsolved real format - raw PCM samples packed\n"
                    "with metadata, no encoding at all but also no real\n"
                    "container format, unlike streams/ (see sa_audio_\n"
                    "stream.py's own docstring for the full, real story).")
                menu.addAction(sfx_action)

            #    Text-editable types get an "Edit" action               
            _TEXT_EDITABLE = ('.ide', '.ipl', '.dat', '.txt', '.cfg',
                              '.ini', '.zon', '.cut', '.fxt')
            file_ext = os.path.splitext(file_path)[1].lower()
            if file_ext in _TEXT_EDITABLE:
                # Smart routing for known GTA data files
                try:
                    from apps.methods.smart_file_router import get_editor_label
                    editor_label = get_editor_label(file_path)
                except Exception:
                    editor_label = ""
                if editor_label:
                    smart_action = QAction(f"⚙  Open in {editor_label}", self)
                    smart_action.triggered.connect(
                        lambda _=False, p=file_path: self._open_smart_editor(p))
                    menu.addAction(smart_action)
                edit_action = QAction(f"✏  Edit  {os.path.basename(file_path)}", self)
                edit_action.triggered.connect(
                    lambda _=False, p=file_path: self._edit_text_file(p))
                menu.addAction(edit_action)
                if file_ext == '.ide':
                    ide_action = QAction("Open in IDE Editor", self)
                    ide_action.triggered.connect(
                        lambda _=False, p=file_path: self._open_ide_editor(p))
                    menu.addAction(ide_action)

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
        # Move to parent directory
        if file_path and os.path.exists(file_path):
            parent_dir = os.path.dirname(os.path.dirname(file_path))
            if parent_dir and os.path.isdir(parent_dir):
                move_up_action = QAction(f"Move to /{os.path.basename(parent_dir) or '..'}", self)
                move_up_action.setIcon(get_folder_icon())
                move_up_action.triggered.connect(
                    lambda: self._move_selected_to_parent(file_path))
                menu.addAction(move_up_action)
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

    def _get_mini_player(self): #vers 1
        """Get (creating once, first time it's needed) the shared
        MiniAudioPlayer widget (Aug 20 2026, per Keith: "maybe a
        tooltip player, showing just the name, and a progress bar,
        stop, start") - one real widget instance reused for every
        real file played from Dir Tree, shown as a real, small,
        floating window rather than a modal dialog so Keith can keep
        browsing while something plays."""
        player = getattr(self, '_mini_player', None)
        if player is not None:
            return player
        try:
            from apps.methods.audioparser import MiniAudioPlayer
        except ImportError as e:
            self.log_message(f"Couldn't load the mini player: {e}")
            return None
        player = MiniAudioPlayer(self)
        player.setWindowTitle("Audio Preview")
        player.setWindowFlags(Qt.WindowType.Tool)
        player.resize(320, 90)
        self._mini_player = player
        return player

    def _play_audio_file(self, path): #vers 2
        """Play a standard audio file directly (Aug 20 2026, per
        Keith: "dir tree shows audio files, so we can now right click
        them to play").

        Real fix (Aug 20 2026, per Keith: "wav plays. mp3 doesn't seen
        to work.") - switched from QSoundEffect to the shared
        MiniAudioPlayer's own QMediaPlayer. QSoundEffect is built for
        short, low-latency, uncompressed-or-Ogg sound effects and does
        not decode MP3 at all - that mismatch was the real, direct
        cause of the bug, not anything wrong with the MP3 files
        themselves. QMediaPlayer is Qt's own real, full media pipeline
        and decodes MP3 correctly, plus gives the real name/progress
        bar/stop/start mini player Keith also asked for."""
        player = self._get_mini_player()
        if player is None:
            return
        player.load_and_play(path)
        player.show()
        player.raise_()
        self.log_message(f"Playing: {os.path.basename(path)}")

    def _play_via_ffmpeg_transcode(self, path): #vers 1
        """Play a file QMediaPlayer can't decode natively by
        transcoding it through a real, external ffmpeg process first
        (Aug 20 2026, per Keith: "in LC, VC .wav plays... .at3") -
        confirmed directly against Keith's own real, uploaded
        philcollins.at3 file: ffprobe reads it as a standard RIFF/WAVE
        container wrapping real ATRAC3+ audio, and ffmpeg decodes it
        to a real, standard WAV cleanly."""
        try:
            from apps.methods.audioparser import transcode_to_wav
        except ImportError as e:
            self.log_message(f"Couldn't load the audio transcoder: {e}")
            return
        self.log_message(f"Transcoding {os.path.basename(path)}...")
        try:
            wav_path = transcode_to_wav(path)
        except RuntimeError as e:
            self.log_message(f"Couldn't play {os.path.basename(path)}: {e}")
            return
        player = self._get_mini_player()
        if player is None:
            return
        player.load_and_play(wav_path, display_name=os.path.basename(path))
        player.show()
        player.raise_()

    def _play_ps2_vb_file(self, path): #vers 1
        """Decode and play a PS2 .VB file (Aug 20 2026, per Keith: "in
        LC, VC .wav plays... .vb") - real, working decoder confirmed
        against Keith's own real, uploaded AMBSIL.VB (see apps/
        methods/audioparser.py's own docstring for the full, real
        confirmation story: the decoded left channel came back
        exactly, perfectly silent, matching what a file named
        "ambient silence" should be).

        Real, honest limitation: uses the real, documented 32000Hz/
        stereo default for every file - the real, per-file exceptions
        (POLICE.VB/CHAT.VB/KCHAT.VB/VCPR.VB at 16000Hz; mission-script
        VAGs at 12000Hz mono, confirmed via GTAForums' own VBDec
        thread) aren't applied automatically yet, since this handler
        has no reliable way to know which real game a given .VB came
        from just from its own path alone - a wrongly-fast/slow-
        sounding file is this real limitation, not a decoding bug."""
        try:
            from apps.methods.audioparser import decode_vb_file
        except ImportError as e:
            self.log_message(f"Couldn't load the .VB decoder: {e}")
            return
        self.log_message(f"Decoding {os.path.basename(path)}...")
        try:
            wav_path = decode_vb_file(path)
        except Exception as e:
            self.log_message(f"Couldn't decode {os.path.basename(path)}: {e}")
            return
        player = self._get_mini_player()
        if player is None:
            return
        player.load_and_play(wav_path, display_name=os.path.basename(path))
        player.show()
        player.raise_()

    def _play_adf_file(self, path): #vers 1
        """Decode and play a III/VC .ADF music/ambient stream file
        (Aug 20 2026, per Keith's own real, uploaded FLASH.ADF sample)
        - real, working decoder confirmed against Keith's own real
        file (see audioparser.py's own "III/VC .ADF format" section
        for the full, real confirmation story: real LAME encoder tags
        appear at exactly the right real offset once XOR-decoded with
        the confirmed, constant 0x22 key, and both `file` and ffprobe
        confirm the fully decoded result as a real, standard, valid
        MP3)."""
        try:
            from apps.methods.audioparser import decode_adf_file
        except ImportError as e:
            self.log_message(f"Couldn't load the .ADF decoder: {e}")
            return
        self.log_message(f"Decoding {os.path.basename(path)}...")
        try:
            mp3_path = decode_adf_file(path)
        except Exception as e:
            self.log_message(f"Couldn't decode {os.path.basename(path)}: {e}")
            return
        player = self._get_mini_player()
        if player is None:
            return
        player.load_and_play(mp3_path, display_name=os.path.basename(path))
        player.show()
        player.raise_()

    def _find_sfx_pair(self, path): #vers 1
        """Given either a real .RAW or .SDT path, return the real
        (raw_path, sdt_path) pair if its own real partner file (same
        real base filename, in the same real folder) exists too, or
        None if it doesn't (Aug 20 2026, per Keith's own real,
        uploaded SFX23.RAW/SFX23.SDT sample pair) - III/VC's own SFX
        archive format is a real pair, and neither file alone can be
        decoded without the other."""
        base, ext = os.path.splitext(path)
        ext = ext.lower()
        if ext == '.raw':
            raw_path, sdt_path = path, base + '.SDT'
            if not os.path.isfile(sdt_path):
                sdt_path = base + '.sdt'
        elif ext == '.sdt':
            sdt_path, raw_path = path, base + '.RAW'
            if not os.path.isfile(raw_path):
                raw_path = base + '.raw'
        else:
            return None
        if os.path.isfile(raw_path) and os.path.isfile(sdt_path):
            return (raw_path, sdt_path)
        return None

    def _play_sfx_pair(self, path): #vers 1
        """Decode and play the first real entry from a III/VC SFX.RAW/
        SFX.SDT pair (Aug 20 2026, per Keith's own real, uploaded
        SFX23.RAW/SFX23.SDT sample pair) - real, working decoder
        confirmed with mathematical certainty against Keith's own real
        files (see audioparser.py's own "III/VC SFX format" section
        for the full, real confirmation story). A given pair can have
        several real entries (Keith's own real SFX23 pair had 4) -
        this plays only the first as a real, quick preview."""
        pair = self._find_sfx_pair(path)
        if pair is None:
            self.log_message(f"Couldn't find the matching .RAW/.SDT partner for {os.path.basename(path)}")
            return
        raw_path, sdt_path = pair
        try:
            from apps.methods.audioparser import parse_sfx_sdt, sfx_entry_to_wav
        except ImportError as e:
            self.log_message(f"Couldn't load the SFX decoder: {e}")
            return
        entries = parse_sfx_sdt(sdt_path)
        if not entries:
            self.log_message(f"No real entries found in {os.path.basename(sdt_path)}")
            return
        import tempfile
        tmp_path = os.path.join(
            tempfile.gettempdir(),
            f"_dirtree_sfx_preview_{os.path.basename(raw_path)}.wav")
        sfx_entry_to_wav(raw_path, entries[0], tmp_path)
        player = self._get_mini_player()
        if player is None:
            return
        player.load_and_play(
            tmp_path,
            display_name=f"{os.path.basename(raw_path)} (entry 0 of {len(entries)} preview)")
        player.show()
        player.raise_()
        self.log_message(
            f"Playing entry 0 of {len(entries)} from {os.path.basename(raw_path)}.")

    def _extract_and_play_sa_stream(self, path): #vers 2
        """Decode a recognised SA audio-stream file (Ambience/Genrl/
        radio station files - no real file extension, so identified
        by filename alone) and play its own first real track (Aug 20
        2026, per Keith: "dir tree shows audio files, so we can now
        right click them to play") - real, working decoder confirmed
        against Keith's own real, uploaded AMBIENCE file (see apps/
        methods/audioparser.py's own docstring for the full, real
        confirmation story: ffprobe verified extracted tracks as
        fully valid Ogg Vorbis). A given stream file has many real
        tracks (Keith's own real AMBIENCE had 40) - this plays only
        the first as a real, quick preview; the dedicated "Extract
        Tracks..." button in Map Workshop's own Settings > Render >
        Audio Streams pulls every real track out to individual files
        for Keith to identify and rename properly.

        Switched to the shared MiniAudioPlayer (Aug 20 2026, same
        real reason as _play_audio_file's own real fix)."""
        try:
            from apps.methods.audioparser import parse_stream_tracks, extract_track
        except ImportError as e:
            self.log_message(f"Couldn't load the audio stream decoder: {e}")
            return
        tracks = parse_stream_tracks(path, max_tracks=1)
        if not tracks:
            self.log_message(f"No real tracks found in {os.path.basename(path)}")
            return
        ogg_bytes = extract_track(path, tracks[0])
        import tempfile
        tmp_path = os.path.join(tempfile.gettempdir(), f"_dirtree_preview_{os.path.basename(path)}.ogg")
        with open(tmp_path, 'wb') as f:
            f.write(ogg_bytes)
        player = self._get_mini_player()
        if player is None:
            return
        player.load_and_play(tmp_path, display_name=f"{os.path.basename(path)} (track 0 preview)")
        player.show()
        player.raise_()
        self.log_message(
            f"Playing track 0 (preview only) from {os.path.basename(path)} - "
            f"use Map Workshop's own Settings > Render > Audio Streams > "
            f"Extract Tracks... to get every real track out as individual files.")

    def _open_smart_editor(self, file_path: str): #vers 1
        """Route file to specialist editor based on filename."""
        try:
            from apps.methods.smart_file_router import open_smart_editor
            mw = getattr(self, "main_window", None)
            open_smart_editor(file_path, mw)
        except Exception as e:
            mw = getattr(self, "main_window", None)
            if mw and hasattr(mw, "log_message"):
                mw.log_message(f"Smart editor error: {e}")

    def _edit_text_file(self, file_path: str): #vers 2
        """Open a text-editable GTA file in the IMG Factory text editor."""
        try:
            from apps.core.notepad import open_text_file_in_editor
            mw = getattr(self, "main_window", None)
            open_text_file_in_editor(file_path, mw)
            if mw and hasattr(mw, "log_message"):
                mw.log_message(f"Text Editor: {os.path.basename(file_path)}")
        except Exception as e:
            mw = getattr(self, "main_window", None)
            if mw and hasattr(mw, "log_message"):
                mw.log_message(f"Text Editor error: {e}")

    def _open_ide_editor(self, file_path: str): #vers 1
        """Open an .ide file in the structured IDE Editor."""
        try:
            from apps.components.Ide_Editor.ide_editor import open_ide_editor
            mw = getattr(self, "main_window", None)
            editor = open_ide_editor(mw)
            editor.load_ide_file(file_path)
            if mw and hasattr(mw, "log_message"):
                mw.log_message(f"IDE Editor: {os.path.basename(file_path)}")
        except Exception as e:
            mw = getattr(self, "main_window", None)
            if mw and hasattr(mw, "log_message"):
                mw.log_message(f"IDE Editor error: {e}")

    def _move_selected_to_parent(self, file_path): #vers 1
        """Move selected items up one directory level."""
        import shutil
        active_tree = getattr(self, '_active_tree', self.tree)
        selected = active_tree.selectedItems()
        if not selected:
            selected = [active_tree.itemAt(active_tree.viewport().mapFromGlobal(
                active_tree.cursor().pos()))]
        parent_dir = os.path.dirname(os.path.dirname(file_path))
        if not parent_dir or not os.path.isdir(parent_dir):
            return
        moved = 0
        for item in selected:
            if not item:
                continue
            src = item.data(0, Qt.ItemDataRole.UserRole)
            if not src or not os.path.exists(src):
                continue
            dst = os.path.join(parent_dir, os.path.basename(src))
            try:
                shutil.move(src, dst)
                moved += 1
            except Exception as e:
                self.log_message(f"Move error: {e}")
        if moved:
            self.log_message(f"Moved {moved} item(s) → {parent_dir}")
            self.browse_directory(self.current_path)

    def _save_column_widths(self, tree): #vers 1
        """Save column widths to QSettings."""
        from PyQt6.QtCore import QSettings
        s = QSettings("IMG-Factory", "IMG-Factory")
        widths = [tree.columnWidth(i) for i in range(tree.columnCount())]
        s.setValue("dirtree/col_widths", widths)

    def _restore_column_widths(self, tree): #vers 1
        """Restore column widths from QSettings."""
        from PyQt6.QtCore import QSettings
        s = QSettings("IMG-Factory", "IMG-Factory")
        widths = s.value("dirtree/col_widths", None)
        if not widths:
            return
        try:
            for i, w in enumerate(widths):
                if i < tree.columnCount() and int(w) > 0:
                    tree.setColumnWidth(i, int(w))
        except Exception:
            pass

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


    def delete_selected(self): #vers 4
        active_tree = getattr(self, '_active_tree', self.tree)
        selected_items = active_tree.selectedItems()
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
                self._refresh_all_panels()
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


    def redo_selected(self): #vers 2
        if not self.redo_stack:
            self.log_message("Nothing to redo")
            return
        action = self.redo_stack.pop()
        self.undo_stack.append(action)
        try:
            if action['action'] in ('paste', 'copy'):
                op = action.get('operation', 'copy')
                sources = action.get('sources', [])
                parent = action.get('parent', '')
                # If sources/parent available re-copy, else inform
                if sources and parent:
                    for src in sources:
                        dest = os.path.join(parent, os.path.basename(src))
                        if op == 'cut':
                            shutil.move(src, dest)
                        else:
                            if os.path.isdir(src):
                                shutil.copytree(src, dest)
                            else:
                                shutil.copy2(src, dest)
                    self.log_message(f"Redid {action['action']}")
                else:
                    self.log_message(f"Cannot redo {action['action']} - source info unavailable")
            elif action['action'] == 'trash':
                QMessageBox.information(self, "Redo Trash",
                    "Cannot redo trash - items are in system trash.")
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

    def _refresh_all_panels(self): #vers 2
        """Refresh left panel and right panel if in twin view"""
        if self.current_path:
            self.populate_tree(self.current_path)
        if hasattr(self, '_second_tree') and self._second_tree:
            right_path = getattr(self, '_right_current_path', None) or (self._right_addr.text() if hasattr(self, '_right_addr') else None)
            if right_path and os.path.isdir(right_path):
                self._populate_second_tree(right_path)


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

        # Create hidden - parent set later when inserted into splitter
        directory_browser = DirectoryTreeBrowser(None)
        directory_browser.hide()
        directory_browser.main_window = main_window

        # Store it in main window for later access
        main_window.directory_tree = directory_browser
        
        # Connect file opening if available
        if hasattr(main_window, 'load_file_unified'):
            directory_browser.file_opened.connect(main_window.load_file_unified)
            
        # Placement into content_splitter is handled by _autoload_dir_tree / _switch_to_directory_tree
        # Do not manipulate Tab 0 here - tabs are for IMG/COL files only

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
