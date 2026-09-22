#!/usr/bin/env python3
#this belongs in components/File_Browser/dolphin_dialog.py - Version: 22
# X-Seti - Sep 22 2026 - IMG Factory 1.6 - Dolphin Style File Browser

"""
Dolphin Style File Browser - Custom themed file dialog
Replaces native Qt dialogs with fully themed browser supporting:
- Single/Multi selection
- Create folder, rename, delete
- Theme integration from IMG Factory
- SVG icons
"""

import sys, os
from pathlib import Path
_root = Path(__file__).resolve().parents[3]
if str(_root) not in sys.path: sys.path.insert(0, str(_root))

from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QTreeWidget, QTreeWidgetItem,
    QLabel, QPushButton, QLineEdit, QComboBox, QSplitter, QMenu,
    QMessageBox, QInputDialog, QToolBar, QWidget, QHeaderView,
    QMainWindow, QListWidget, QListWidgetItem, QStackedWidget,
    QAbstractItemView, QApplication, QSlider, QWidgetAction, QFileIconProvider,
    QTabWidget, QFrame, QSizePolicy, QCheckBox
)
from PyQt6.QtCore import Qt, pyqtSignal, QDir, QFileInfo, QSize, QTimer, QByteArray
from PyQt6.QtGui import QIcon, QPixmap, QPainter, QColor, QFont, QAction, QPalette, QShortcut, QKeySequence
from PyQt6.QtSvg import QSvgRenderer
import os
import json
import datetime

try:
    from apps.methods.imgfactory_svg_icons import SVGIconFactory
except Exception:
    SVGIconFactory = None

##Methods list -
# __init__
# _add_place
# _add_project_folders
# _add_sidebar_section
# _add_storage_devices
# _add_tree_item
# _apply_colors
# _apply_default_styling
# _apply_filter
# _apply_icon_scale
# _apply_theme_styling
# _apply_tree_columns
# _build_breadcrumb
# _build_toolbars
# _change_view_mode
# _add_recent_file
# _close_tab
# _config_get
# _config_set
# _create_address_bar
# _create_archive_icon
# _create_back_icon
# _create_bottom_bar
# _create_cancel_icon
# _create_collision_icon
# _create_delete_icon
# _create_desktop_icon
# _create_document_icon
# _create_download_icon
# _create_drive_icon
# _create_edit_icon
# _create_export_icon
# _create_file_icon
# _create_folder_icon
# _create_forward_icon
# _create_home_icon
# _create_image_icon
# _create_import_icon
# _create_info_panel
# _create_model_icon
# _create_new_folder
# _create_new_folder_icon
# _create_open_icon
# _create_pane
# _create_places_sidebar
# _create_properties_icon
# _create_refresh_icon
# _create_save_icon
# _create_svg_icon
# _create_text_icon
# _create_texture_icon
# _create_toolbar
# _create_up_icon
# _current_selected_paths
# _delete_item
# _file_icon_for
# _format_file_size
# _get_project_folder_icon
# _get_file_details
# _get_file_icon
# _go_back
# _go_forward
# _go_home
# _go_up
# _handle_action_button
# _icon
# _item_double_clicked
# _load_directory
# _load_directory_silent
# _navigate_to_address
# _new_tab
# _open_as
# _open_search
# _open_with_default
# _parse_filter
# _path_icon
# _pin_place
# _place_clicked
# _places_context_menu
# _populate_filter_combo
# _recent_clicked
# _refresh_directory
# _refresh_places_icons
# _rename_item
# _resolve_icon_color
# _restore_toolbar_state
# _save_toolbar_state
# _selection_changed
# _setup_dialog_properties
# _setup_ui
# _show_address_edit
# _show_context_menu
# _show_properties
# _tab_changed
# _toggle_info_panel
# _toggle_path_edit
# _toggle_places_panel
# _toggle_split_view
# _toggle_system_icons
# _toolbar_context_menu
# _unpin_place
# _update_info_panel
# _update_preview
# _zoom_in
# _zoom_out
# _zoom_reset
# accept
# get_existing_directory
# get_open_filename
# get_open_filenames
# get_save_filename
# get_selected_path
# get_selected_paths
# reject
# set_active_pane

##class BrowserPane: -
# __init__
# _activate
# _build_icon_list
# _build_tree
# _icon_item_double_clicked

##class FileSearchDialog: -
# __init__
# _open_result
# _run_search

##class _VerticalLabel: -
# minimumSizeHint
# paintEvent
# sizeHint

##Classes -
# BrowserPane
# DolphinFileDialog
# FileSearchDialog
# _VerticalLabel

class _VerticalLabel(QLabel): #vers 1
    """QLabel that paints its text rotated 90° - for header titles in
    vertical ribbon toolbars, where Qt doesn't auto-rotate widgets."""

    def paintEvent(self, event): #vers 1
        painter = QPainter(self)
        painter.setPen(self.palette().color(self.foregroundRole()))
        painter.translate(0, self.height())
        painter.rotate(-90)
        painter.drawText(0, 0, self.height(), self.width(),
                          Qt.AlignmentFlag.AlignCenter, self.text())
        painter.end()

    def sizeHint(self): #vers 1
        s = super().sizeHint()
        return QSize(s.height(), s.width())

    def minimumSizeHint(self): #vers 1
        s = super().minimumSizeHint()
        return QSize(s.height(), s.width())


class FileSearchDialog(QDialog): #vers 1
    """Recursive search under the current directory - Simple mode
    matches filenames, Detailed mode also greps file contents."""

    def __init__(self, dialog): #vers 1
        super().__init__(dialog)
        self.dialog = dialog
        self.setWindowTitle("Search")
        self.resize(500, 400)
        layout = QVBoxLayout(self)

        row = QHBoxLayout()
        self.pattern_input = QLineEdit()
        self.pattern_input.setPlaceholderText("Search text...")
        self.pattern_input.returnPressed.connect(self._run_search)
        row.addWidget(self.pattern_input, 1)
        self.mode_combo = QComboBox()
        self.mode_combo.addItems(["Simple (filename)", "Detailed (content)"])
        row.addWidget(self.mode_combo)
        search_btn = QPushButton("Search")
        search_btn.clicked.connect(self._run_search)
        row.addWidget(search_btn)
        layout.addLayout(row)

        self.results_list = QListWidget()
        self.results_list.itemDoubleClicked.connect(self._open_result)
        layout.addWidget(self.results_list)

        self.status_label = QLabel("")
        layout.addWidget(self.status_label)

    def _run_search(self): #vers 1
        """Walk self.dialog.current_path, match filenames (Simple)
        and optionally file contents (Detailed), capped at 500 hits."""
        pattern = self.pattern_input.text().strip()
        if not pattern:
            return
        self.results_list.clear()
        root = self.dialog.current_path
        detailed = self.mode_combo.currentIndex() == 1
        pattern_lower = pattern.lower()
        count = 0
        skip_dirs = {'.git', '__pycache__', 'node_modules', '.vscode', '.idea'}
        for dirpath, dirnames, filenames in os.walk(root):
            dirnames[:] = [d for d in dirnames if d not in skip_dirs]
            for name in filenames:
                full = os.path.join(dirpath, name)
                matched = pattern_lower in name.lower()
                if not matched and detailed:
                    try:
                        with open(full, 'r', errors='ignore') as fh:
                            matched = pattern_lower in fh.read().lower()
                    except Exception:
                        pass
                if matched:
                    item = QListWidgetItem(full)
                    item.setData(Qt.ItemDataRole.UserRole, full)
                    self.results_list.addItem(item)
                    count += 1
                    if count >= 500:
                        break
            if count >= 500:
                break
        self.status_label.setText(f"{count} match{'es' if count != 1 else ''}"
                                   + (" (capped at 500)" if count >= 500 else ""))

    def _open_result(self, item): #vers 1
        """Navigate the browser to the selected result's directory."""
        path = item.data(Qt.ItemDataRole.UserRole)
        if path and os.path.exists(path):
            self.dialog._load_directory(os.path.dirname(path))
            self.accept()


class BrowserPane(QWidget): #vers 1
    """One Dolphin-style browsing pane: own tree/icon-list view,
    current directory and history. Dialog tabs/split view hold
    multiple panes; shared toolbar/breadcrumb/info panel act on
    whichever pane last had activity via dialog.set_active_pane()."""

    def __init__(self, dialog, initial_path=None): #vers 1
        super().__init__(dialog)
        self.dialog = dialog
        self.current_path = initial_path or dialog._pending_current_path
        self.history = []
        self.history_index = -1
        self.view_index = 0

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        self.view_stack = QStackedWidget()
        self.tree = self._build_tree()
        self.view_stack.addWidget(self.tree)
        self.icon_list = self._build_icon_list()
        self.view_stack.addWidget(self.icon_list)
        layout.addWidget(self.view_stack)

    def _activate(self, fn, *args): #vers 1
        """Make this pane active, then forward the original signal call."""
        self.dialog.set_active_pane(self)
        if fn:
            fn(*args)

    def _build_tree(self): #vers 1
        """Build this pane's Details/Condensed tree view."""
        d = self.dialog
        tree = QTreeWidget()
        tree.setHeaderLabels(["Name", "Size", "Type", "Date Modified"])
        tree.setRootIsDecorated(False)
        tree.setAlternatingRowColors(True)
        tree.setSortingEnabled(True)
        tree.setSelectionMode(
            QTreeWidget.SelectionMode.ExtendedSelection if d.multi_select
            else QTreeWidget.SelectionMode.SingleSelection
        )
        tree.setColumnWidth(0, 300)
        tree.setColumnWidth(1, 100)
        tree.setColumnWidth(2, 120)
        tree.setColumnWidth(3, 150)
        tree.itemDoubleClicked.connect(
            lambda item, col: self._activate(d._item_double_clicked, item, col))
        tree.itemClicked.connect(lambda item, col: self._activate(None))
        tree.itemSelectionChanged.connect(lambda: self._activate(d._selection_changed))
        tree.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        tree.customContextMenuRequested.connect(
            lambda pos: self._activate(d._show_context_menu, pos))
        return tree

    def _build_icon_list(self): #vers 1
        """Build this pane's Icons grid view, mirrors the tree's data."""
        d = self.dialog
        lw = QListWidget()
        lw.setViewMode(QListWidget.ViewMode.IconMode)
        lw.setIconSize(QSize(48, 48))
        lw.setGridSize(QSize(96, 84))
        lw.setResizeMode(QListWidget.ResizeMode.Adjust)
        lw.setMovement(QListWidget.Movement.Static)
        lw.setWordWrap(True)
        lw.setSelectionMode(
            QListWidget.SelectionMode.ExtendedSelection if d.multi_select
            else QListWidget.SelectionMode.SingleSelection
        )
        lw.itemDoubleClicked.connect(
            lambda item: self._activate(self._icon_item_double_clicked, item))
        lw.itemClicked.connect(lambda item: self._activate(None))
        lw.itemSelectionChanged.connect(lambda: self._activate(d._selection_changed))
        lw.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        lw.customContextMenuRequested.connect(
            lambda pos: self._activate(d._show_context_menu, pos))
        return lw

    def _icon_item_double_clicked(self, item): #vers 1
        """Icons view double-click - same navigate/select behaviour as tree."""
        d = self.dialog
        path = item.data(Qt.ItemDataRole.UserRole)
        if not path:
            return
        if item.text() == "..":
            d._go_up()
            return
        file_info = QFileInfo(path)
        if file_info.isDir():
            d._load_directory(path)
        elif d.mode in ['open', 'import']:
            d.selected_items = [path]
            d.accept()


class DolphinFileDialog(QDialog): #vers 1
    """Custom file dialog with Dolphin-style interface"""

    # Signals
    path_selected = pyqtSignal(str)
    paths_selected = pyqtSignal(list)
    open_in_workshop = pyqtSignal(str, str)  # handler, path

    def __init__(self, parent=None, mode='open', multi_select=False, file_filter="All Files (*.*)"): #vers 1
        """
        Initialize Dolphin file dialog

        Args:
            parent: Parent widget
            mode: 'open', 'save', 'import', or 'export'
            multi_select: Allow multiple file selection
            file_filter: File extension filter
        """
        super().__init__(parent)

        self.parent_window = parent
        self.mode = mode
        self.multi_select = multi_select
        self.file_filter = file_filter
        self.active_pane = None
        self.panes = []
        self._pending_current_path = QDir.homePath()
        self.selected_items = []

        # Real native OS icons (QFileIconProvider - Plasma/GTK on Linux,
        # Explorer on Windows, Finder on macOS) vs the SVGIconFactory
        # static icons. Loaded from saved config, default off.
        self._icon_provider = QFileIconProvider()
        self._use_system_icons = False
        try:
            self._use_system_icons = json.loads(
                (Path.home()/'.config'/'imgfactory'/'file_browser.json').read_text()
            ).get('use_system_icons', False)
        except Exception:
            pass

        # Text zoom (Ctrl+=/Ctrl+-/Ctrl+0) - persisted, applied to all
        # tree/icon views across every pane and the sidebar.
        self._font_pt = 9
        try:
            self._font_pt = json.loads(
                (Path.home()/'.config'/'imgfactory'/'file_browser.json').read_text()
            ).get('font_pt', 9)
        except Exception:
            pass

        # Setup dialog
        self._setup_dialog_properties()


    def _setup_dialog_properties(self): #vers 2
        """Setup dialog window properties"""
        # Set title based on mode
        titles = {
            'open': 'Open File',
            'save': 'Save File',
            'import': 'Import Files',
            'export': 'Export Files'
        }
        self.setWindowTitle(titles.get(self.mode, 'Browse'))
        self.setWindowIcon(self._create_folder_icon())

        # Set size
        self.setMinimumSize(900, 600)
        self.resize(1000, 650)
        self.setModal(True)


    # ---- Active-pane forwarding ---------------------------------------
    # tree/icon_list/current_path/history/history_index/_view_stack all
    # resolve to whichever BrowserPane is active, so every method below
    # that reads/writes them keeps working unchanged across tabs+split.

    @property
    def tree(self): #vers 1
        return self.active_pane.tree if self.active_pane else None

    @property
    def icon_list(self): #vers 1
        return self.active_pane.icon_list if self.active_pane else None

    @property
    def _view_stack(self): #vers 1
        return self.active_pane.view_stack if self.active_pane else None

    @property
    def current_path(self): #vers 1
        return self.active_pane.current_path if self.active_pane else self._pending_current_path

    @current_path.setter
    def current_path(self, value): #vers 1
        if self.active_pane:
            self.active_pane.current_path = value
        else:
            self._pending_current_path = value

    @property
    def history(self): #vers 1
        return self.active_pane.history if self.active_pane else []

    @history.setter
    def history(self, value): #vers 1
        if self.active_pane:
            self.active_pane.history = value

    @property
    def history_index(self): #vers 1
        return self.active_pane.history_index if self.active_pane else -1

    @history_index.setter
    def history_index(self, value): #vers 1
        if self.active_pane:
            self.active_pane.history_index = value


    def get_selected_path(self): #vers 1
        """Get single selected file path"""
        if self.selected_items:
            return self.selected_items[0]
        return None


    def get_selected_paths(self): #vers 1
        """Get all selected file paths"""
        return self.selected_items


    def accept(self): #vers 4
        """Handle dialog accept. Keep-open checked -> emit and stay open."""
        if self.selected_items:
            if self.mode in ('open', 'import'):
                for p in self.selected_items:
                    if os.path.isfile(p):
                        self._add_recent_file(p)
            if self.multi_select:
                self.paths_selected.emit(self.selected_items)
            else:
                self.path_selected.emit(self.selected_items[0])
        if hasattr(self, 'keep_open_check') and self.keep_open_check.isChecked():
            self.selected_items = []
            if hasattr(self, 'selection_label'):
                self._selection_changed()
            return
        self._save_toolbar_state()
        super().accept()


    def reject(self): #vers 2
        """Handle dialog cancel"""
        self._save_toolbar_state()
        self.selected_items = []
        super().reject()


    def _setup_ui(self): #vers 1
        """Setup main UI layout"""
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # Toolbar at top
        self.toolbar = self._create_toolbar()
        layout.addWidget(self.toolbar)

        # Address bar
        address_widget = self._create_address_bar()
        layout.addWidget(address_widget)

        # Main content area with splitter
        self.main_splitter = QSplitter(Qt.Orientation.Horizontal)
        self.main_splitter.setSizePolicy(
            QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)

        # Left: Places sidebar
        self.places_widget = self._create_places_sidebar()
        self.main_splitter.addWidget(self.places_widget)

        # Center: Tabs of BrowserPanes (Dolphin-style tabs + split view).
        # self.tree/self.icon_list/self.current_path/self.history all
        # forward to whichever pane last had activity - see BrowserPane
        # and set_active_pane().
        self.tab_widget = QTabWidget()
        self.tab_widget.setDocumentMode(True)
        self.tab_widget.setContentsMargins(0, 0, 0, 0)
        self.tab_widget.setTabsClosable(True)
        self.tab_widget.tabCloseRequested.connect(self._close_tab)
        self.tab_widget.currentChanged.connect(self._tab_changed)
        first_pane = self._create_pane()
        first_page = QSplitter(Qt.Orientation.Horizontal)
        first_page.addWidget(first_pane)
        self.tab_widget.addTab(first_page, self._create_folder_icon(), "Tab 1")
        self.active_pane = first_pane
        self.file_tree = self.tab_widget
        self.main_splitter.addWidget(self.file_tree)

        # Right: Preview/info panel
        self.info_panel = self._create_info_panel()
        self.main_splitter.addWidget(self.info_panel)

        # Set splitter proportions (20% | 50% | 30%)
        self.main_splitter.setStretchFactor(0, 2)
        self.main_splitter.setStretchFactor(1, 5)
        self.main_splitter.setStretchFactor(2, 3)

        layout.addWidget(self.main_splitter)

        # Bottom: Filename input and buttons
        bottom_widget = self._create_bottom_bar()
        layout.addWidget(bottom_widget)

        # Apply theme
        self._apply_theme_styling()

        # Function-key panel/view shortcuts (Dolphin-style)
        QShortcut(QKeySequence("F5"), self, activated=self._refresh_directory)
        QShortcut(QKeySequence("F9"), self,
                  activated=lambda: self._places_toggle_act.trigger())
        QShortcut(QKeySequence("F3"), self,
                  activated=lambda: self._info_toggle_act.trigger())

        # Text zoom
        QShortcut(QKeySequence("Ctrl+="), self, activated=self._zoom_in)
        QShortcut(QKeySequence("Ctrl++"), self, activated=self._zoom_in)
        QShortcut(QKeySequence("Ctrl+-"), self, activated=self._zoom_out)
        QShortcut(QKeySequence("Ctrl+0"), self, activated=self._zoom_reset)
        self._apply_font_scale()

        # Load initial directory
        self._load_directory(self.current_path)


    def _create_toolbar(self): #vers 2
        """Ribbon toolbar - QMainWindow+QToolBar, movable/floatable/
        icon-size/save-restore, replacing the old fixed QPushButton row."""
        icon_color = self._resolve_icon_color()

        inner_mw = QMainWindow()
        inner_mw.setWindowFlags(Qt.WindowType.Widget)
        inner_mw.setContentsMargins(0, 0, 0, 0)
        inner_mw.setDockOptions(
            QMainWindow.DockOption.AllowNestedDocks |
            QMainWindow.DockOption.AllowTabbedDocks)
        central = QWidget()
        central.setMaximumHeight(0)
        inner_mw.setCentralWidget(central)
        # Fixed height again - float/side-docking never actually
        # worked in this embedded 0-size-central mw (see _tb() below),
        # so removing the fixed height just brought the blank space
        # back with nothing gained. Toolbar is now restricted to the
        # top row only, where this fixed height is correct.
        inner_mw.setFixedHeight(36)
        self._inner_mw = inner_mw

        self._build_toolbars(inner_mw, icon_color)

        QTimer.singleShot(300, self._restore_toolbar_state)

        return inner_mw

    def _build_toolbars(self, mw, icon_color: str): #vers 1
        """Build ribbon QToolBar(s) using QAction."""
        _saved_px = 20
        try:
            _saved_px = json.loads(
                (Path.home()/'.config'/'imgfactory'/'file_browser.json').read_text()
            ).get('icon_scale', 20)
        except Exception:
            pass
        icon_size = QSize(_saved_px, _saved_px)
        self._ribbon_actions = []

        def _tb(name, area=Qt.ToolBarArea.TopToolBarArea):
            tb = QToolBar(name, mw)
            tb.setObjectName(name)
            tb.setIconSize(icon_size)
            # Movable within the top row only - this mw has no real
            # side/bottom docking room (0-size central widget), so
            # floating/side-docking never actually worked, it just
            # looked draggable. Restrict to what's honest and works.
            tb.setMovable(True)
            tb.setFloatable(False)
            tb.setAllowedAreas(Qt.ToolBarArea.TopToolBarArea)
            tb.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
            tb.customContextMenuRequested.connect(
                lambda pos, t=tb: self._toolbar_context_menu(t, pos))
            mw.addToolBar(area, tb)
            return tb

        def _act(tb, name, icon_fn, callback=None, checkable=False, attr=None):
            try:
                icon = getattr(SVGIconFactory, icon_fn)(20, icon_color) if SVGIconFactory else QIcon()
            except Exception:
                icon = QIcon()
            act = QAction(icon, name, mw)
            act.setToolTip(name)
            act.setCheckable(checkable)
            if callback:
                if checkable:
                    act.toggled.connect(callback)
                else:
                    act.triggered.connect(callback)
            tb.addAction(act)
            self._ribbon_actions.append({'action': act, 'toolbar': tb, 'name': name})
            if attr:
                setattr(self, attr, act)
            return act

        # Single toolbar, logical groups separated - keeps everything on
        # one row instead of the old 3-toolbar layout wrapping to 2+
        # rows whenever the window wasn't wide enough.
        tb_main = _tb("Main")
        _act(tb_main, "Back", 'get_back_icon', self._go_back, attr='back_btn')
        _act(tb_main, "Forward", 'get_forward_icon', self._go_forward, attr='forward_btn')
        _act(tb_main, "Up", 'get_up_icon', self._go_up, attr='up_btn')
        _act(tb_main, "Refresh  F5", 'get_refresh_icon', self._refresh_directory, attr='refresh_btn')
        _act(tb_main, "Home", 'get_home_icon', self._go_home, attr='home_btn')
        self.back_btn.setEnabled(False)
        self.forward_btn.setEnabled(False)
        tb_main.addSeparator()

        self.view_mode = QComboBox()
        self.view_mode.addItems(["Details", "Icons", "Condensed"])
        self.view_mode.setCurrentIndex(0)
        self.view_mode.currentIndexChanged.connect(self._change_view_mode)
        tb_main.addWidget(self.view_mode)
        tb_main.addSeparator()

        _act(tb_main, "Places Panel  F9", 'get_panel_toggle_icon',
             self._toggle_places_panel, checkable=True, attr='_places_toggle_act')
        self._places_toggle_act.setChecked(True)
        _act(tb_main, "Info Panel  F3", 'get_panel_toggle_icon',
             self._toggle_info_panel, checkable=True, attr='_info_toggle_act')
        self._info_toggle_act.setChecked(True)
        _act(tb_main, "System Icons", 'get_image_icon',
             self._toggle_system_icons, checkable=True, attr='_system_icons_act')
        self._system_icons_act.setChecked(self._use_system_icons)
        tb_main.addSeparator()

        _act(tb_main, "New Folder", 'get_new_folder_icon', self._create_new_folder, attr='new_folder_btn')
        tb_main.addSeparator()

        _act(tb_main, "New Tab", 'get_add_icon', self._new_tab, attr='new_tab_btn')
        _act(tb_main, "Split View", 'get_panel_toggle_icon', self._toggle_split_view,
             checkable=True, attr='_split_view_act')
        tb_main.addSeparator()

        # Text zoom - plain QActions, no icon lookup (label carries it)
        zoom_out_act = QAction("A-", mw)
        zoom_out_act.setToolTip("Zoom out text  Ctrl+-")
        zoom_out_act.triggered.connect(self._zoom_out)
        tb_main.addAction(zoom_out_act)
        zoom_in_act = QAction("A+", mw)
        zoom_in_act.setToolTip("Zoom in text  Ctrl+=")
        zoom_in_act.triggered.connect(self._zoom_in)
        tb_main.addAction(zoom_in_act)
        tb_main.addSeparator()

        _act(tb_main, "Search", 'get_search_icon', self._open_search, attr='search_btn')

        self._tb_main = tb_main

    def _toolbar_context_menu(self, toolbar, pos): #vers 1
        """Right-click context menu on any toolbar."""
        menu = QMenu(self)

        size_menu = menu.addMenu("Icon Size")
        slider = QSlider(Qt.Orientation.Horizontal)
        slider.setRange(14, 32)
        slider.setSingleStep(2)
        try:
            data = json.loads(
                (Path.home()/'.config'/'imgfactory'/'file_browser.json').read_text())
            slider.setValue(data.get('icon_scale', 20))
        except Exception:
            slider.setValue(20)
        slider.valueChanged.connect(self._apply_icon_scale)
        wa = QWidgetAction(menu)
        wa.setDefaultWidget(slider)
        size_menu.addAction(wa)

        menu.addSeparator()
        menu.addAction("Lock All Toolbars",
            lambda: [tb.setMovable(False) for tb in self._inner_mw.findChildren(QToolBar)])
        menu.addAction("Unlock All Toolbars",
            lambda: [tb.setMovable(True) for tb in self._inner_mw.findChildren(QToolBar)])
        menu.exec(toolbar.mapToGlobal(pos))

    def _apply_icon_scale(self, px: int): #vers 1
        """Apply icon size to all toolbars live and persist it."""
        mw = getattr(self, '_inner_mw', None)
        if mw:
            for tb in mw.findChildren(QToolBar):
                tb.setIconSize(QSize(px, px))
        try:
            path = Path.home() / '.config' / 'imgfactory' / 'file_browser.json'
            try:
                data = json.loads(path.read_text())
            except Exception:
                data = {}
            data['icon_scale'] = px
            path.write_text(json.dumps(data, indent=2))
        except Exception:
            pass

    def _open_search(self): #vers 1
        """Open the Simple/Detailed recursive search dialog."""
        dlg = FileSearchDialog(self)
        dlg.exec()

    def _open_with_default(self, path): #vers 1
        """Open a file with its associated workshop or external app."""
        from apps.methods.file_associations import get_handler, is_internal, launch_external
        handler = get_handler(path)
        if not handler:
            QMessageBox.information(self, "No Association",
                "No app associated with this file type.\nUse 'Open As...' to set one.")
            return
        if is_internal(handler):
            self.open_in_workshop.emit(handler, path)
        else:
            launch_external(handler, path)

    def _open_as(self, path): #vers 2
        """Pick a handler for this file, optionally set as default.
        'Browse for App...' lets the user import their own executable."""
        from apps.methods.file_associations import get_associations, save_associations, is_internal, launch_external
        ext = Path(path).suffix.lstrip('.').lower()
        assoc = get_associations()
        browse_label = "Browse for App..."
        choices = [browse_label] + sorted(set(assoc.values()))
        current = assoc.get(ext, choices[0] if choices else '')
        choice, ok = QInputDialog.getItem(
            self, "Open As", f"Open .{ext} files with:",
            choices, choices.index(current) if current in choices else 0, True)
        if not ok or not choice:
            return
        if choice == browse_label:
            from PyQt6.QtWidgets import QFileDialog
            app_path, _ = QFileDialog.getOpenFileName(self, "Select Application", "/usr/bin")
            if not app_path:
                return
            choice = app_path
        if QMessageBox.question(self, "Set Default",
                f"Always open .{ext} with '{choice}'?") == QMessageBox.StandardButton.Yes:
            assoc[ext] = choice
            save_associations(assoc)
        if is_internal(choice):
            self.open_in_workshop.emit(choice, path)
        else:
            launch_external(choice, path)

    def _config_get(self, key, default=None): #vers 1
        """Read one key from file_browser.json."""
        try:
            path = Path.home() / '.config' / 'imgfactory' / 'file_browser.json'
            return json.loads(path.read_text()).get(key, default)
        except Exception:
            return default

    def _config_set(self, key, value): #vers 1
        """Write one key to file_browser.json, merging with existing data."""
        try:
            path = Path.home() / '.config' / 'imgfactory' / 'file_browser.json'
            try:
                data = json.loads(path.read_text())
            except Exception:
                data = {}
            data[key] = value
            path.write_text(json.dumps(data, indent=2))
        except Exception:
            pass

    def _pin_place(self, name, path): #vers 1
        """Pin a folder to Places, persisted across sessions."""
        pinned = self._config_get('pinned_places', [])
        if any(p['path'] == path for p in pinned):
            return
        pinned.append({'name': name, 'path': path})
        self._config_set('pinned_places', pinned)
        item = self._add_place(name, path, self._path_icon(path, self._create_folder_icon))
        item.setData(0, Qt.ItemDataRole.UserRole + 1, True)

    def _unpin_place(self, path): #vers 1
        """Remove a pinned folder from Places."""
        pinned = [p for p in self._config_get('pinned_places', []) if p['path'] != path]
        self._config_set('pinned_places', pinned)
        for i in range(self.places_tree.topLevelItemCount() - 1, -1, -1):
            item = self.places_tree.topLevelItem(i)
            if item.data(0, Qt.ItemDataRole.UserRole) == path:
                self.places_tree.takeTopLevelItem(i)

    def _places_context_menu(self, position): #vers 1
        """Right-click on Places - Remove for pinned entries only."""
        item = self.places_tree.itemAt(position)
        if not item or not item.data(0, Qt.ItemDataRole.UserRole + 1):
            return
        path = item.data(0, Qt.ItemDataRole.UserRole)
        menu = QMenu(self)
        remove_action = menu.addAction(self._create_delete_icon(), "Remove from Places")
        remove_action.triggered.connect(lambda: self._unpin_place(path))
        menu.exec(self.places_tree.viewport().mapToGlobal(position))

    def _add_recent_file(self, path): #vers 1
        """Track a recently opened file, persisted, max 10 entries."""
        recent = [p for p in self._config_get('recent_files', []) if p != path]
        recent.insert(0, path)
        recent = recent[:10]
        self._config_set('recent_files', recent)

    def _recent_clicked(self, item, column): #vers 1
        """Open the folder containing a recent file."""
        path = item.data(0, Qt.ItemDataRole.UserRole)
        if path and os.path.exists(path):
            self._load_directory(os.path.dirname(path))

    def _zoom_in(self): #vers 1
        """Ctrl+= / Ctrl++ - increase list/tree text size."""
        self._font_pt = min(self._font_pt + 1, 18)
        self._apply_font_scale()

    def _zoom_out(self): #vers 1
        """Ctrl+- - decrease list/tree text size."""
        self._font_pt = max(self._font_pt - 1, 7)
        self._apply_font_scale()

    def _zoom_reset(self): #vers 1
        """Ctrl+0 - reset list/tree text size to default."""
        self._font_pt = 9
        self._apply_font_scale()

    def _apply_font_scale(self): #vers 1
        """Apply self._font_pt to every pane's tree/icon list and the
        sidebar trees, and persist it."""
        style = f"font-size: {self._font_pt}pt;"
        for pane in self.panes:
            pane.tree.setStyleSheet(style)
            pane.icon_list.setStyleSheet(style)
        for name in ('places_tree', 'devices_tree', 'project_tree', 'recent_tree'):
            tree = getattr(self, name, None)
            if tree:
                tree.setStyleSheet(style)
        try:
            path = Path.home() / '.config' / 'imgfactory' / 'file_browser.json'
            try:
                data = json.loads(path.read_text())
            except Exception:
                data = {}
            data['font_pt'] = self._font_pt
            path.write_text(json.dumps(data, indent=2))
        except Exception:
            pass

    def _save_toolbar_state(self): #vers 1
        """Save QMainWindow toolbar state to file_browser.json."""
        mw = getattr(self, '_inner_mw', None)
        if mw is None:
            return
        try:
            path = Path.home() / '.config' / 'imgfactory' / 'file_browser.json'
            try:
                data = json.loads(path.read_text())
            except Exception:
                data = {}
            data['toolbar_state'] = mw.saveState(1).toHex().data().decode()
            data['toolbar_state_version'] = 1
            path.write_text(json.dumps(data, indent=2))
        except Exception as _e:
            print(f"[DolphinFileDialog] _save_toolbar_state error: {_e}")

    def _restore_toolbar_state(self): #vers 1
        """Restore QMainWindow toolbar state from file_browser.json."""
        mw = getattr(self, '_inner_mw', None)
        if mw is None:
            return
        try:
            from PyQt6.QtCore import QByteArray
            path = Path.home() / '.config' / 'imgfactory' / 'file_browser.json'
            if not path.exists():
                return
            data = json.loads(path.read_text())
            state_hex = data.get('toolbar_state')
            if state_hex and data.get('toolbar_state_version') == 1:
                mw.restoreState(QByteArray.fromHex(state_hex.encode()), 1)
        except Exception as _e:
            print(f"[DolphinFileDialog] _restore_toolbar_state error: {_e}")

    def _toggle_places_panel(self, checked: bool): #vers 1
        """F9 - toggle the left Places/Devices/Project-folders sidebar."""
        if hasattr(self, 'places_widget'):
            self.places_widget.setVisible(checked)

    def _toggle_info_panel(self, checked: bool): #vers 1
        """F3 - toggle the right preview/info panel."""
        if hasattr(self, 'info_panel'):
            self.info_panel.setVisible(checked)

    def _toggle_system_icons(self, checked: bool): #vers 2
        """Switch between real native OS icons (QFileIconProvider) and
        the themed SVGIconFactory icon set, and persist the choice."""
        self._use_system_icons = checked
        try:
            path = Path.home() / '.config' / 'imgfactory' / 'file_browser.json'
            try:
                data = json.loads(path.read_text())
            except Exception:
                data = {}
            data['use_system_icons'] = checked
            path.write_text(json.dumps(data, indent=2))
        except Exception:
            pass
        if self.active_pane:
            self._refresh_directory()
        if hasattr(self, 'places_tree'):
            self._refresh_places_icons()

    def _refresh_places_icons(self): #vers 1
        """Re-resolve icons for the places sidebar after a System Icons toggle."""
        icon_fns = {
            "Home": self._create_home_icon, "Desktop": self._create_desktop_icon,
            "Documents": self._create_document_icon, "Downloads": self._create_download_icon,
            "Pictures": self._create_image_icon,
        }
        for i in range(self.places_tree.topLevelItemCount()):
            item = self.places_tree.topLevelItem(i)
            fallback = icon_fns.get(item.text(0))
            if fallback:
                path = item.data(0, Qt.ItemDataRole.UserRole)
                item.setIcon(0, self._path_icon(path, fallback))
        for i in range(self.devices_tree.topLevelItemCount()):
            item = self.devices_tree.topLevelItem(i)
            path = item.data(0, Qt.ItemDataRole.UserRole)
            item.setIcon(0, self._path_icon(path, self._create_drive_icon))

    def _create_pane(self, path=None): #vers 2
        """Build a new BrowserPane wired to this dialog's shared handlers."""
        pane = BrowserPane(self, initial_path=path)
        style = f"font-size: {self._font_pt}pt;"
        pane.tree.setStyleSheet(style)
        pane.icon_list.setStyleSheet(style)
        self.panes.append(pane)
        return pane

    def set_active_pane(self, pane): #vers 1
        """Make `pane` active; syncs shared toolbar/breadcrumb/info panel
        to its state without touching any pane's own history."""
        if pane is None or pane is self.active_pane:
            return
        self.active_pane = pane
        if hasattr(self, 'address_input'):
            self.address_input.setText(pane.current_path)
        if hasattr(self, '_breadcrumb_layout'):
            self._build_breadcrumb(pane.current_path)
            self._addr_stack.setCurrentWidget(self._breadcrumb_widget)
        if hasattr(self, 'back_btn'):
            self.back_btn.setEnabled(pane.history_index > 0)
            self.forward_btn.setEnabled(pane.history_index < len(pane.history) - 1)
            self.up_btn.setEnabled(QDir(pane.current_path).cdUp())
        if hasattr(self, 'view_mode'):
            self.view_mode.blockSignals(True)
            self.view_mode.setCurrentIndex(getattr(pane, 'view_index', 0))
            self.view_mode.blockSignals(False)
        if hasattr(self, 'selection_label'):
            self._selection_changed()

    def _new_tab(self): #vers 1
        """Open a new tab starting at the active pane's current directory."""
        start_path = self.active_pane.current_path if self.active_pane else QDir.homePath()
        pane = self._create_pane(start_path)
        page = QSplitter(Qt.Orientation.Horizontal)
        page.addWidget(pane)
        idx = self.tab_widget.addTab(page, self._create_folder_icon(), os.path.basename(start_path.rstrip('/')) or "/")
        self.tab_widget.setCurrentIndex(idx)
        self.set_active_pane(pane)
        self._load_directory(start_path)

    def _close_tab(self, index): #vers 2
        """Close a tab; the last remaining tab cannot be closed."""
        if self.tab_widget.count() <= 1:
            return
        w = self.tab_widget.widget(index)
        self.tab_widget.removeTab(index)
        # hide() first - a reparented widget briefly becomes its own
        # top-level window (visible) until deleteLater() actually runs.
        w.hide()
        w.deleteLater()

    def _tab_changed(self, index): #vers 1
        """Sync active pane to the newly current tab, if it changed."""
        page = self.tab_widget.widget(index)
        if not isinstance(page, QSplitter) or page.count() == 0:
            return
        panes = [page.widget(i) for i in range(page.count())]
        if self.active_pane not in panes:
            self.set_active_pane(panes[0])

    def _toggle_split_view(self, checked): #vers 2
        """Split the current tab into two side-by-side panes, or
        collapse back to one (Dolphin-style split view)."""
        page = self.tab_widget.currentWidget()
        if not isinstance(page, QSplitter):
            return
        if checked:
            if page.count() < 2:
                pane = self._create_pane(self.active_pane.current_path)
                page.addWidget(pane)
                self.set_active_pane(pane)
                self._load_directory(pane.current_path)
        else:
            if page.count() > 1:
                extra = page.widget(1)
                if self.active_pane is extra:
                    self.set_active_pane(page.widget(0))
                if extra in self.panes:
                    self.panes.remove(extra)
                # hide() first - a reparented widget briefly becomes its
                # own top-level window (visible) until deleteLater() runs.
                extra.hide()
                extra.setParent(None)
                extra.deleteLater()

    def _create_address_bar(self): #vers 3
        """Dolphin-style breadcrumb path bar - clickable segments by
        default, click the edit icon (or Ctrl+L) to type a path. One
        edit/confirm button and a framed path container cover both
        modes."""
        widget = QWidget()
        widget.setMaximumHeight(34)
        layout = QHBoxLayout(widget)
        layout.setContentsMargins(5, 2, 5, 2)

        path_frame = QFrame()
        path_frame.setObjectName("PathContainer")
        path_frame.setFrameShape(QFrame.Shape.StyledPanel)
        path_frame.setMaximumHeight(30)
        path_layout = QHBoxLayout(path_frame)
        path_layout.setContentsMargins(2, 0, 2, 0)
        path_layout.setSpacing(2)

        self._addr_stack = QStackedWidget()
        self._addr_stack.setFixedHeight(26)

        # Breadcrumb page
        self._breadcrumb_widget = QWidget()
        self._breadcrumb_layout = QHBoxLayout(self._breadcrumb_widget)
        self._breadcrumb_layout.setContentsMargins(4, 2, 4, 2)
        self._breadcrumb_layout.setSpacing(0)
        self._addr_stack.addWidget(self._breadcrumb_widget)

        # Edit page
        self.address_input = QLineEdit()
        self.address_input.setPlaceholderText("Enter path...")
        self.address_input.setText(self.current_path)
        self.address_input.returnPressed.connect(self._navigate_to_address)
        # NOT editingFinished - it fires on the focus round-trip right
        # after setFocus()/selectAll() in _show_address_edit(), which
        # immediately reverted back to breadcrumb view (the "blink").
        # Enter (returnPressed) navigates; _load_directory() itself
        # returns the view to breadcrumb and resets the button icon.
        self._addr_stack.addWidget(self.address_input)

        path_layout.addWidget(self._addr_stack, 1)

        # Single edit/confirm button - visible in both modes
        self._path_edit_btn = QPushButton()
        self._path_edit_btn.setIcon(self._create_edit_icon())
        self._path_edit_btn.setFlat(True)
        self._path_edit_btn.setFixedWidth(24)
        self._path_edit_btn.setToolTip("Edit path  Ctrl+L")
        self._path_edit_btn.clicked.connect(self._toggle_path_edit)
        path_layout.addWidget(self._path_edit_btn)

        layout.addWidget(path_frame, 1)

        QShortcut(QKeySequence("Ctrl+L"), self, activated=self._show_address_edit)

        # Filter combo
        self.filter_combo = QComboBox()
        self.filter_combo.setMinimumWidth(150)
        self._populate_filter_combo()
        self.filter_combo.currentIndexChanged.connect(self._apply_filter)
        layout.addWidget(self.filter_combo)

        return widget

    def _toggle_path_edit(self): #vers 1
        """Single path button - edit when showing breadcrumb, confirm
        (navigate) when showing the text input."""
        if self._addr_stack.currentWidget() is self._breadcrumb_widget:
            self._show_address_edit()
        else:
            self._navigate_to_address()

    def _build_breadcrumb(self, path: str): #vers 1
        """Rebuild the clickable breadcrumb segments for the given path."""
        while self._breadcrumb_layout.count():
            child = self._breadcrumb_layout.takeAt(0)
            w = child.widget()
            if w:
                w.deleteLater()

        norm = path.rstrip('/') or '/'
        segments = []
        cur = norm
        while True:
            name = os.path.basename(cur) or cur
            segments.append((name, cur))
            parent = os.path.dirname(cur)
            if parent == cur:
                break
            cur = parent
        segments.reverse()

        for i, (name, full) in enumerate(segments):
            btn = QPushButton(name if name not in ('', '/') else "/")
            btn.setFlat(True)
            btn.setCursor(Qt.CursorShape.PointingHandCursor)
            btn.setStyleSheet("QPushButton { padding: 2px 6px; border: none; }")
            btn.clicked.connect(lambda checked=False, p=full: self._load_directory(p))
            if i == len(segments) - 1:
                f = btn.font()
                f.setBold(True)
                btn.setFont(f)
                btn.setEnabled(False)
            self._breadcrumb_layout.addWidget(btn)
            if i < len(segments) - 1:
                sep = QLabel("›")
                self._breadcrumb_layout.addWidget(sep)

        self._breadcrumb_layout.addStretch(1)

    def _show_address_edit(self): #vers 2
        """Switch the breadcrumb bar to editable text mode."""
        self.address_input.setText(self.current_path)
        self._addr_stack.setCurrentWidget(self.address_input)
        self.address_input.setFocus()
        self.address_input.selectAll()
        if hasattr(self, '_path_edit_btn'):
            self._path_edit_btn.setIcon(self._create_open_icon())
            self._path_edit_btn.setToolTip("Go  Enter")



    def _populate_filter_combo(self): #vers 1
        """Populate file filter dropdown"""
        filters = [
            "All Files (*.*)",
            "IMG Archives (*.img)",
            "TXD Textures (*.txd)",
            "DFF Models (*.dff)",
            "COL Collision (*.col)",
            "Images (*.png *.jpg *.bmp *.dds)",
            "Archives (*.zip *.rar *.7z)"
        ]

        # Add custom filter if provided
        if self.file_filter and self.file_filter not in filters:
            filters.insert(0, self.file_filter)

        self.filter_combo.addItems(filters)

        # Set current filter
        if self.file_filter:
            index = self.filter_combo.findText(self.file_filter)
            if index >= 0:
                self.filter_combo.setCurrentIndex(index)


    def _add_project_folders(self): #vers 1
        """Add project folders from IMG Factory settings"""
        try:
            # Try to get project folders from parent window settings
            if hasattr(self.parent_window, 'app_settings'):
                settings = self.parent_window.app_settings.current_settings

                project_folders = {
                    'GTA Folder': settings.get('working_gta_folder', ''),
                    'Assists': settings.get('assists_folder', ''),
                    'Textures': settings.get('textures_folder', ''),
                    'Collisions': settings.get('collisions_folder', ''),
                    'Generics': settings.get('generics_folder', ''),
                    'Water': settings.get('water_folder', ''),
                    'Radar': settings.get('radar_folder', ''),
                    'Game Art': settings.get('gameart_folder', ''),
                    'Peds': settings.get('peds_folder', ''),
                    'Vehicles': settings.get('vehicles_folder', ''),
                    'Weapons': settings.get('weapons_folder', '')
                }

                # Add folders that exist
                for name, path in project_folders.items():
                    if path and os.path.exists(path):
                        icon = self._get_project_folder_icon(name)
                        item = QTreeWidgetItem(self.project_tree)
                        item.setText(0, name)
                        item.setData(0, Qt.ItemDataRole.UserRole, path)
                        item.setIcon(0, icon)
            else:
                # Standalone mode - show message
                item = QTreeWidgetItem(self.project_tree)
                item.setText(0, "No project loaded")
                item.setDisabled(True)

        except Exception as e:
            print(f"Project folders error: {e}")


    def _get_project_folder_icon(self, folder_name): #vers 1
        """Get appropriate icon for project folder type"""
        folder_name_lower = folder_name.lower()

        if 'texture' in folder_name_lower:
            return self._create_texture_icon()
        elif 'collision' in folder_name_lower:
            return self._create_collision_icon()
        elif 'vehicle' in folder_name_lower or 'ped' in folder_name_lower:
            return self._create_model_icon()
        elif 'weapon' in folder_name_lower:
            return self._create_file_icon()
        elif 'art' in folder_name_lower or 'radar' in folder_name_lower:
            return self._create_image_icon()
        elif 'gta' in folder_name_lower:
            return self._create_home_icon()
        else:
            return self._create_folder_icon()


    def _add_storage_devices(self): #vers 2
        """Add storage devices to devices tree"""
        drives = QDir.drives()

        for drive in drives:
            drive_path = drive.absolutePath()
            drive_name = drive_path.replace("/", "").replace("\\", "") or "Root"

            # Create drive item in devices tree
            item = QTreeWidgetItem(self.devices_tree)
            item.setText(0, f"Drive {drive_name}")
            item.setData(0, Qt.ItemDataRole.UserRole, drive_path)
            item.setIcon(0, self._create_drive_icon())

    def _create_places_sidebar(self): #vers 6
        """Left sidebar - Places/Devices/Project Folders as resizable,
        collapsible sections in a vertical QSplitter (drag the handle
        between sections to resize; click the arrow to collapse). No
        box borders - darker theme-aware bg. Project Folders is
        hidden entirely when not docked."""
        self._sidebar_splitter = QSplitter(Qt.Orientation.Vertical)
        self._sidebar_splitter.setChildrenCollapsible(False)

        # Places
        self.places_tree = QTreeWidget()
        self.places_tree.setObjectName("SidebarTree")
        self.places_tree.setHeaderHidden(True)
        self.places_tree.setMaximumWidth(200)
        self.places_tree.itemClicked.connect(self._place_clicked)
        self.places_tree.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.places_tree.customContextMenuRequested.connect(self._places_context_menu)
        self._add_sidebar_section("Places", self.places_tree)

        home = QDir.homePath()
        self._add_place("Home", home, self._path_icon(home, self._create_home_icon))
        self._add_place("Desktop", home + "/Desktop", self._path_icon(home + "/Desktop", self._create_desktop_icon))
        self._add_place("Documents", home + "/Documents", self._path_icon(home + "/Documents", self._create_document_icon))
        self._add_place("Downloads", home + "/Downloads", self._path_icon(home + "/Downloads", self._create_download_icon))
        self._add_place("Pictures", home + "/Pictures", self._path_icon(home + "/Pictures", self._create_image_icon))
        for p in self._config_get('pinned_places', []):
            if os.path.exists(p['path']):
                item = self._add_place(p['name'], p['path'], self._path_icon(p['path'], self._create_folder_icon))
                item.setData(0, Qt.ItemDataRole.UserRole + 1, True)

        # Recent files
        self.recent_tree = QTreeWidget()
        self.recent_tree.setObjectName("SidebarTree")
        self.recent_tree.setHeaderHidden(True)
        self.recent_tree.setMaximumWidth(200)
        self.recent_tree.itemClicked.connect(self._recent_clicked)
        self._add_sidebar_section("Recent", self.recent_tree)
        for path in self._config_get('recent_files', []):
            if os.path.exists(path):
                item = QTreeWidgetItem(self.recent_tree)
                item.setText(0, os.path.basename(path))
                item.setData(0, Qt.ItemDataRole.UserRole, path)
                item.setIcon(0, self._create_file_icon())

        # Devices
        self.devices_tree = QTreeWidget()
        self.devices_tree.setObjectName("SidebarTree")
        self.devices_tree.setHeaderHidden(True)
        self.devices_tree.setMaximumWidth(200)
        self.devices_tree.itemClicked.connect(self._place_clicked)
        self._add_sidebar_section("Devices", self.devices_tree)
        self._add_storage_devices()

        # Project Folders - only when docked in IMG Factory
        self.project_tree = QTreeWidget()
        self.project_tree.setObjectName("SidebarTree")
        self.project_tree.setHeaderHidden(True)
        self.project_tree.setMaximumWidth(200)
        self.project_tree.itemClicked.connect(self._place_clicked)
        if hasattr(self.parent_window, 'app_settings'):
            self._add_sidebar_section("Project Folders", self.project_tree)
            self._add_project_folders()

        return self._sidebar_splitter

    def _add_sidebar_section(self, title, tree): #vers 3
        """Add one resizable, collapsible section (header + tree) to
        the sidebar splitter - drag the splitter handle below it to
        resize, click the arrow to collapse."""
        section = QWidget()
        section.setObjectName("SidebarSection")
        v = QVBoxLayout(section)
        v.setContentsMargins(0, 0, 0, 0)
        v.setSpacing(0)

        header = QWidget()
        header.setObjectName("SidebarHeader")
        hl = QHBoxLayout(header)
        hl.setContentsMargins(4, 2, 4, 2)
        collapse_btn = QPushButton("▾")
        collapse_btn.setObjectName("SidebarCollapseBtn")
        collapse_btn.setFlat(True)
        collapse_btn.setFixedWidth(16)
        label = QLabel(title)
        label.setStyleSheet("font-weight: bold; font-size: 11px;")
        hl.addWidget(collapse_btn)
        hl.addWidget(label)
        hl.addStretch()
        v.addWidget(header)
        v.addWidget(tree)

        def _toggle_section(): #vers 3
            visible = not tree.isVisible()
            tree.setVisible(visible)
            collapse_btn.setText("▾" if visible else "▸")
        collapse_btn.clicked.connect(_toggle_section)

        self._sidebar_splitter.addWidget(section)
        return section


    def _add_place(self, name, path, icon): #vers 1
        """Add a place to sidebar"""
        item = QTreeWidgetItem(self.places_tree)
        item.setText(0, name)
        item.setData(0, Qt.ItemDataRole.UserRole, path)
        item.setIcon(0, icon)
        return item


    def _add_storage_devices(self): #vers 2
        """Add storage devices to places"""
        drives = QDir.drives()

        for drive in drives:
            drive_path = drive.absolutePath()
            drive_name = drive_path.replace("/", "").replace("\\", "") or "Root"

            # Create drive item
            item = QTreeWidgetItem(self.places_tree)
            item.setText(0, f"Drive {drive_name}")
            item.setData(0, Qt.ItemDataRole.UserRole, drive_path)
            item.setIcon(0, self._path_icon(drive_path, self._create_drive_icon))


    def _load_directory(self, path): #vers 2
        """Load directory contents into tree (and icon grid)"""
        self.tree.clear()
        if hasattr(self, 'icon_list'):
            self.icon_list.clear()
        self.current_path = path
        self.address_input.setText(path)
        if hasattr(self, '_breadcrumb_layout'):
            self._build_breadcrumb(path)
            if hasattr(self, '_addr_stack'):
                self._addr_stack.setCurrentWidget(self._breadcrumb_widget)
                if hasattr(self, '_path_edit_btn'):
                    self._path_edit_btn.setIcon(self._create_edit_icon())
                    self._path_edit_btn.setToolTip("Edit path  Ctrl+L")

        # Update history
        if not self.history or self.history[self.history_index] != path:
            # Remove forward history if we navigate from middle
            if self.history_index < len(self.history) - 1:
                self.history = self.history[:self.history_index + 1]
            self.history.append(path)
            self.history_index = len(self.history) - 1

        # Update navigation buttons
        self.back_btn.setEnabled(self.history_index > 0)
        self.forward_btn.setEnabled(self.history_index < len(self.history) - 1)
        self.up_btn.setEnabled(QDir(path).cdUp())

        # Get directory info
        dir_info = QDir(path)

        # Get current filter
        filter_text = self.filter_combo.currentText()
        name_filters = self._parse_filter(filter_text)

        # Set filters
        dir_info.setNameFilters(name_filters)
        dir_info.setFilter(QDir.Filter.AllEntries | QDir.Filter.NoDotAndDotDot)
        dir_info.setSorting(QDir.SortFlag.DirsFirst | QDir.SortFlag.Name | QDir.SortFlag.IgnoreCase)

        # Add parent directory (..) if not at root
        if dir_info.cdUp():
            up_icon = self._create_up_icon()
            parent_item = QTreeWidgetItem(self.tree)
            parent_item.setText(0, "..")
            parent_item.setIcon(0, up_icon)
            parent_item.setData(0, Qt.ItemDataRole.UserRole, dir_info.absolutePath())
            if hasattr(self, 'icon_list'):
                up_li = QListWidgetItem(up_icon, "..")
                up_li.setData(Qt.ItemDataRole.UserRole, dir_info.absolutePath())
                self.icon_list.addItem(up_li)
            dir_info.cd(path)  # Go back to current

        # Load entries
        entries = dir_info.entryInfoList()

        for entry in entries:
            self._add_tree_item(entry)


    def _add_tree_item(self, file_info): #vers 2
        """Add file/folder item to tree (and mirror into icon grid)"""
        item = QTreeWidgetItem(self.tree)

        # Name
        item.setText(0, file_info.fileName())
        item.setData(0, Qt.ItemDataRole.UserRole, file_info.absoluteFilePath())

        # Icon
        icon = self._file_icon_for(file_info)
        item.setIcon(0, icon)

        # Size
        if file_info.isFile():
            size = file_info.size()
            item.setText(1, self._format_file_size(size))
            item.setData(1, Qt.ItemDataRole.UserRole, size)  # For sorting
        else:
            item.setText(1, "--")

        # Type
        if file_info.isDir():
            item.setText(2, "Folder")
        else:
            suffix = file_info.suffix().upper()
            item.setText(2, f"{suffix} File" if suffix else "File")

        # Date modified
        modified = file_info.lastModified().toString("yyyy-MM-dd HH:mm")
        item.setText(3, modified)

        if hasattr(self, 'icon_list'):
            li = QListWidgetItem(icon, file_info.fileName())
            li.setData(Qt.ItemDataRole.UserRole, file_info.absoluteFilePath())
            self.icon_list.addItem(li)

        return item


    def _create_info_panel(self): #vers 1
        """Create right info/preview panel"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        layout.setContentsMargins(5, 5, 5, 5)

        # Info label
        info_label = QLabel("Information")
        info_label.setStyleSheet("font-weight: bold; font-size: 11px;")
        layout.addWidget(info_label)

        # Preview area
        self.preview_label = QLabel("No selection")
        self.preview_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.preview_label.setMinimumHeight(100)
        self.preview_label.setMaximumHeight(250)
        self.preview_label.setStyleSheet("border: 1px solid palette(mid); background: palette(window);")
        layout.addWidget(self.preview_label)

        # File details
        details_group = QWidget()
        details_layout = QVBoxLayout(details_group)
        details_layout.setSpacing(8)

        self.name_label = QLabel("Name: --")
        self.type_label = QLabel("Type: --")
        self.size_label = QLabel("Size: --")
        self.modified_label = QLabel("Modified: --")
        self.path_label = QLabel("Path: --")
        self.path_label.setWordWrap(True)

        details_layout.addWidget(self.name_label)
        details_layout.addWidget(self.type_label)
        details_layout.addWidget(self.size_label)
        details_layout.addWidget(self.modified_label)
        details_layout.addWidget(self.path_label)

        layout.addWidget(details_group)
        layout.addStretch()

        return widget


    def _create_info_panel(self): #vers 1
        """Create right info/preview panel"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        layout.setContentsMargins(5, 5, 5, 5)

        # Info label
        info_label = QLabel("Information")
        info_label.setStyleSheet("font-weight: bold; font-size: 11px;")
        layout.addWidget(info_label)

        # Preview area
        self.preview_label = QLabel("No selection")
        self.preview_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.preview_label.setMinimumHeight(100)
        self.preview_label.setMaximumHeight(250)
        self.preview_label.setStyleSheet("border: 1px solid palette(mid); background: palette(window);")
        layout.addWidget(self.preview_label)

        # File details
        details_group = QWidget()
        details_layout = QVBoxLayout(details_group)
        details_layout.setSpacing(8)

        self.name_label = QLabel("Name: --")
        self.type_label = QLabel("Type: --")
        self.size_label = QLabel("Size: --")
        self.modified_label = QLabel("Modified: --")
        self.path_label = QLabel("Path: --")
        self.path_label.setWordWrap(True)

        details_layout.addWidget(self.name_label)
        details_layout.addWidget(self.type_label)
        details_layout.addWidget(self.size_label)
        details_layout.addWidget(self.modified_label)
        details_layout.addWidget(self.path_label)

        layout.addWidget(details_group)
        layout.addStretch()

        return widget

    def _update_info_panel(self, file_path): #vers 1
        """Update info panel with file details"""
        if not file_path or not os.path.exists(file_path):
            self.preview_label.setText("No selection")
            self.name_label.setText("Name: --")
            self.type_label.setText("Type: --")
            self.size_label.setText("Size: --")
            self.modified_label.setText("Modified: --")
            self.path_label.setText("Path: --")
            return

        file_info = QFileInfo(file_path)

        # Update details
        self.name_label.setText(f"Name: {file_info.fileName()}")

        if file_info.isDir():
            self.type_label.setText("Type: Folder")
            self.size_label.setText("Size: --")
        else:
            suffix = file_info.suffix().upper()
            self.type_label.setText(f"Type: {suffix} File" if suffix else "Type: File")
            self.size_label.setText(f"Size: {self._format_file_size(file_info.size())}")

        modified = file_info.lastModified().toString("yyyy-MM-dd HH:mm:ss")
        self.modified_label.setText(f"Modified: {modified}")
        self.path_label.setText(f"Path: {file_path}")

        # Update preview
        self._update_preview(file_path, file_info)

    def _update_preview(self, file_path, file_info): #vers 2
        """Update preview image or file info"""
        if file_info.isDir():
            self.preview_label.setText("Folder")
            self.preview_label.setPixmap(QPixmap())
            return

        # Check if image file
        image_extensions = ['.png', '.jpg', '.jpeg', '.bmp', '.gif', '.dds']
        suffix = file_info.suffix().lower()

        if f".{suffix}" in image_extensions:
            try:
                pixmap = QPixmap(file_path)
                if not pixmap.isNull():
                    # Scale to fit preview
                    scaled = pixmap.scaled(
                        self.preview_label.size(),
                        Qt.AspectRatioMode.KeepAspectRatio,
                        Qt.TransformationMode.SmoothTransformation
                    )
                    self.preview_label.setPixmap(scaled)
                    return
            except:
                pass

        # Use file command to get detailed info
        file_details = self._get_file_details(file_path)
        if file_details:
            self.preview_label.setText(file_details)
            self.preview_label.setWordWrap(True)
            self.preview_label.setAlignment(Qt.AlignmentFlag.AlignTop | Qt.AlignmentFlag.AlignLeft)
            self.preview_label.setStyleSheet("padding: 10px; font-size: 10px;")
        else:
            # Fallback: show file type
            self.preview_label.setText(f"{file_info.suffix().upper()}\nFile")

        self.preview_label.setPixmap(QPixmap())

    def _get_file_details(self, file_path): #vers 1
        """Get detailed file information using system commands"""
        import subprocess
        import platform

        system = platform.system()
        details = []

        try:
            if system == "Linux":
                # Use 'file' command
                result = subprocess.run(['file', '-b', file_path],
                                    capture_output=True, text=True, timeout=2)
                if result.returncode == 0:
                    details.append(result.stdout.strip())

            elif system == "Windows":
                # Use PowerShell Get-Item
                ps_cmd = f'Get-Item "{file_path}" | Select-Object -ExpandProperty VersionInfo | Format-List'
                result = subprocess.run(['powershell', '-Command', ps_cmd],
                                    capture_output=True, text=True, timeout=2)
                if result.returncode == 0 and result.stdout.strip():
                    details.append(result.stdout.strip()[:200])  # Limit output
                else:
                    # Fallback: basic info
                    details.append(f"Windows file\n{os.path.splitext(file_path)[1].upper()} format")

            elif system == "Darwin":  # macOS
                # Use 'file' command (available on macOS)
                result = subprocess.run(['file', '-b', file_path],
                                    capture_output=True, text=True, timeout=2)
                if result.returncode == 0:
                    details.append(result.stdout.strip())

                # Also try mdls for additional metadata
                result = subprocess.run(['mdls', '-name', 'kMDItemContentType', file_path],
                                    capture_output=True, text=True, timeout=2)
                if result.returncode == 0:
                    content_type = result.stdout.strip().split('=')[-1].strip().strip('"')
                    if content_type and content_type != '(null)':
                        details.append(f"Type: {content_type}")

            return "\n".join(details) if details else None

        except Exception as e:
            print(f"File details error: {e}")
            return None

    def _get_file_details(self, file_path): #vers 1
        """Get detailed file information using system commands"""
        import subprocess
        import platform

        system = platform.system()
        details = []

        try:
            if system == "Linux":
                # Use 'file' command
                result = subprocess.run(['file', '-b', file_path],
                                    capture_output=True, text=True, timeout=2)
                if result.returncode == 0:
                    details.append(result.stdout.strip())

            elif system == "Windows":
                # Use PowerShell Get-Item
                ps_cmd = f'Get-Item "{file_path}" | Select-Object -ExpandProperty VersionInfo | Format-List'
                result = subprocess.run(['powershell', '-Command', ps_cmd],
                                    capture_output=True, text=True, timeout=2)
                if result.returncode == 0 and result.stdout.strip():
                    details.append(result.stdout.strip()[:200])  # Limit output
                else:
                    # Fallback: basic info
                    details.append(f"Windows file\n{os.path.splitext(file_path)[1].upper()} format")

            elif system == "Darwin":  # macOS
                # Use 'file' command (available on macOS)
                result = subprocess.run(['file', '-b', file_path],
                                    capture_output=True, text=True, timeout=2)
                if result.returncode == 0:
                    details.append(result.stdout.strip())

                # Also try mdls for additional metadata
                result = subprocess.run(['mdls', '-name', 'kMDItemContentType', file_path],
                                    capture_output=True, text=True, timeout=2)
                if result.returncode == 0:
                    content_type = result.stdout.strip().split('=')[-1].strip().strip('"')
                    if content_type and content_type != '(null)':
                        details.append(f"Type: {content_type}")

            return "\n".join(details) if details else None

        except Exception as e:
            print(f"File details error: {e}")
            return None

    def _create_bottom_bar(self): #vers 1
        """Create bottom bar with filename input and action buttons"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        layout.setContentsMargins(5, 5, 5, 5)

        # Filename input row (for save mode)
        if self.mode in ['save', 'export']:
            filename_layout = QHBoxLayout()

            filename_label = QLabel("Filename:")
            filename_layout.addWidget(filename_label)

            self.filename_input = QLineEdit()
            self.filename_input.setPlaceholderText("Enter filename...")
            filename_layout.addWidget(self.filename_input)

            layout.addLayout(filename_layout)

        # Selection info and buttons
        button_layout = QHBoxLayout()

        # Selection count label
        self.selection_label = QLabel("No items selected")
        button_layout.addWidget(self.selection_label)

        button_layout.addStretch()

        # Keep dialog open after selection (multi-file workflows)
        self.keep_open_check = QCheckBox("Keep open")
        self.keep_open_check.setToolTip("Don't close after selecting")
        button_layout.addWidget(self.keep_open_check)

        # Action buttons based on mode
        if self.mode == 'open':
            self.action_btn = QPushButton("Open")
            self.action_btn.setIcon(self._create_open_icon())
        elif self.mode == 'save':
            self.action_btn = QPushButton("Save")
            self.action_btn.setIcon(self._create_save_icon())
        elif self.mode == 'import':
            self.action_btn = QPushButton("Import")
            self.action_btn.setIcon(self._create_import_icon())
        elif self.mode == 'export':
            self.action_btn = QPushButton("Export")
            self.action_btn.setIcon(self._create_export_icon())

        self.action_btn.setFixedWidth(100)
        self.action_btn.setFixedHeight(32)
        self.action_btn.clicked.connect(self._handle_action_button)
        self.action_btn.setEnabled(False)
        button_layout.addWidget(self.action_btn)

        # Cancel button
        cancel_btn = QPushButton("Cancel")
        cancel_btn.setIcon(self._create_cancel_icon())
        cancel_btn.setFixedWidth(100)
        cancel_btn.setFixedHeight(32)
        cancel_btn.clicked.connect(self.reject)
        button_layout.addWidget(cancel_btn)

        layout.addLayout(button_layout)

        return widget

    def _handle_action_button(self): #vers 1
        """Handle main action button click"""
        if self.mode in ['save', 'export']:
            # Get filename from input
            if hasattr(self, 'filename_input'):
                filename = self.filename_input.text().strip()
                if not filename:
                    QMessageBox.warning(self, "No Filename", "Please enter a filename.")
                    return

                # Build full path
                full_path = os.path.join(self.current_path, filename)

                # Check if file exists
                if os.path.exists(full_path):
                    reply = QMessageBox.question(
                        self,
                        "File Exists",
                        f"File '{filename}' already exists.\nOverwrite?",
                        QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
                    )
                    if reply == QMessageBox.StandardButton.No:
                        return

                self.selected_items = [full_path]
        else:
            # Get selected items from whichever view is active
            paths = [p for p in self._current_selected_paths() if p and p != ".."]
            if not paths:
                QMessageBox.warning(self, "No Selection", "Please select a file or folder.")
                return

            self.selected_items = paths

        # Accept dialog
        self.accept()

    def _current_selected_paths(self): #vers 1
        """Selected item paths from whichever view (tree or icon grid) is active."""
        if hasattr(self, 'icon_list') and self.icon_list.isVisible():
            return [it.data(Qt.ItemDataRole.UserRole)
                    for it in self.icon_list.selectedItems()]
        return [it.data(0, Qt.ItemDataRole.UserRole)
                for it in self.tree.selectedItems()]

    def _selection_changed(self): #vers 2
        """Handle selection change in either view (tree or icon grid)"""
        paths = self._current_selected_paths()
        count = len(paths)

        # Update selection label
        if count == 0:
            self.selection_label.setText("No items selected")
            self.action_btn.setEnabled(False)
        elif count == 1:
            self.selection_label.setText("1 item selected")
            self.action_btn.setEnabled(True)

            # Update info panel
            path = paths[0]
            if path and path != "..":
                self._update_info_panel(path)
        else:
            self.selection_label.setText(f"{count} items selected")
            self.action_btn.setEnabled(True)
            self.preview_label.setText(f"{count} items\nselected")
            self.preview_label.setPixmap(QPixmap())

    def _item_double_clicked(self, item, column): #vers 1
        """Handle tree item double click"""
        path = item.data(0, Qt.ItemDataRole.UserRole)

        if not path:
            return

        # Check if it's parent directory (..)
        if item.text(0) == "..":
            self._go_up()
            return

        file_info = QFileInfo(path)

        # If directory, navigate into it
        if file_info.isDir():
            self._load_directory(path)
        else:
            # If file and in open mode, accept selection
            if self.mode in ['open', 'import']:
                self.selected_items = [path]
                self.accept()

    def _go_back(self): #vers 1
        """Navigate to previous directory in history"""
        if self.history_index > 0:
            self.history_index -= 1
            path = self.history[self.history_index]
            self._load_directory_silent(path)

    def _go_forward(self): #vers 1
        """Navigate to next directory in history"""
        if self.history_index < len(self.history) - 1:
            self.history_index += 1
            path = self.history[self.history_index]
            self._load_directory_silent(path)

    def _go_up(self): #vers 1
        """Navigate to parent directory"""
        dir_info = QDir(self.current_path)
        if dir_info.cdUp():
            self._load_directory(dir_info.absolutePath())

    def _go_home(self): #vers 1
        """Navigate to home directory"""
        self._load_directory(QDir.homePath())

    def _refresh_directory(self): #vers 1
        """Refresh current directory"""
        self._load_directory_silent(self.current_path)

    def _navigate_to_address(self): #vers 1
        """Navigate to path entered in address bar"""
        path = self.address_input.text().strip()

        if not path:
            return

        if os.path.exists(path) and os.path.isdir(path):
            self._load_directory(path)
        else:
            QMessageBox.warning(self, "Invalid Path", f"Path does not exist:\n{path}")
            self.address_input.setText(self.current_path)

    def _place_clicked(self, item, column): #vers 1
        """Handle places sidebar click"""
        path = item.data(0, Qt.ItemDataRole.UserRole)
        if path and os.path.exists(path):
            self._load_directory(path)

    def _load_directory_silent(self, path): #vers 1
        """Load directory without adding to history"""
        old_index = self.history_index
        self._load_directory(path)
        self.history_index = old_index

    def _change_view_mode(self, index): #vers 3
        """Change view mode: 0=Details, 1=Icons, 2=Condensed"""
        if not self.active_pane:
            return
        self.active_pane.view_index = index
        if index == 1:
            self._view_stack.setCurrentWidget(self.icon_list)
        else:
            self._view_stack.setCurrentWidget(self.tree)
            self._apply_tree_columns(condensed=(index == 2))
        self._selection_changed()

    def _apply_tree_columns(self, condensed: bool): #vers 1
        """Condensed = Name column only, tight rows. Details = all columns."""
        self.tree.setHeaderHidden(condensed)
        for col in (1, 2, 3):
            self.tree.setColumnHidden(col, condensed)
        self.tree.setStyleSheet(
            "QTreeWidget::item { height: 18px; }" if condensed else "")


    def _show_context_menu(self, position): #vers 2
        """Show context menu for file operations - works from either
        the tree (Details/Condensed) or the icon grid (Icons view)."""
        active = self.icon_list if (hasattr(self, 'icon_list') and self.icon_list.isVisible()) else self.tree
        if active is self.icon_list:
            item = self.icon_list.itemAt(position)
            path = item.data(Qt.ItemDataRole.UserRole) if item else None
            name = item.text() if item else None
        else:
            item = self.tree.itemAt(position)
            path = item.data(0, Qt.ItemDataRole.UserRole) if item else None
            name = item.text(0) if item else None

        menu = QMenu(self)

        if item and path and path != "..":
            # Open action
            open_action = menu.addAction(self._create_open_icon(), "Open")
            open_action.triggered.connect(
                lambda: self._icon_item_double_clicked(item) if active is self.icon_list
                        else self._item_double_clicked(item, 0))

            # Copy path
            copy_action = menu.addAction(self._create_edit_icon(), "Copy Path")
            copy_action.triggered.connect(lambda: QApplication.clipboard().setText(path))

            if QFileInfo(path).isDir():
                pin_action = menu.addAction(self._create_folder_icon(), "Add to Places")
                pin_action.triggered.connect(lambda: self._pin_place(name, path))
            else:
                open_with_action = menu.addAction(self._create_open_icon(), "Open With Default App")
                open_with_action.triggered.connect(lambda: self._open_with_default(path))
                open_as_action = menu.addAction(self._create_open_icon(), "Open As...")
                open_as_action.triggered.connect(lambda: self._open_as(path))

            menu.addSeparator()

            # Rename action
            rename_action = menu.addAction(self._create_edit_icon(), "Rename")
            rename_action.triggered.connect(lambda: self._rename_item(path, name))

            # Delete action
            delete_action = menu.addAction(self._create_delete_icon(), "Delete")
            delete_action.triggered.connect(lambda: self._delete_item(path, name))

            menu.addSeparator()

            # Properties action
            props_action = menu.addAction(self._create_properties_icon(), "Properties")
            props_action.triggered.connect(lambda: self._show_properties(path))
        else:
            # Empty space context menu
            new_folder_action = menu.addAction(self._create_folder_icon(), "New Folder")
            new_folder_action.triggered.connect(self._create_new_folder)

            menu.addSeparator()

            refresh_action = menu.addAction(self._create_refresh_icon(), "Refresh")
            refresh_action.triggered.connect(self._refresh_directory)

        menu.exec(active.viewport().mapToGlobal(position))


    def _create_new_folder(self): #vers 1
        """Create new folder in current directory"""
        folder_name, ok = QInputDialog.getText(
            self,
            "New Folder",
            "Enter folder name:",
            QLineEdit.EchoMode.Normal,
            "New Folder"
        )

        if ok and folder_name:
            new_path = os.path.join(self.current_path, folder_name)

            if os.path.exists(new_path):
                QMessageBox.warning(self, "Folder Exists", f"Folder '{folder_name}' already exists.")
                return

            try:
                os.makedirs(new_path)
                self._refresh_directory()

                if hasattr(self.parent_window, 'log_message'):
                    self.parent_window.log_message(f"Created folder: {folder_name}")
            except Exception as e:
                QMessageBox.critical(self, "Error", f"Failed to create folder:\n{str(e)}")


    def _rename_item(self, old_path, old_name): #vers 2
        """Rename selected file or folder (path/name, works for either view)"""
        new_name, ok = QInputDialog.getText(
            self,
            "Rename",
            "Enter new name:",
            QLineEdit.EchoMode.Normal,
            old_name
        )

        if ok and new_name and new_name != old_name:
            new_path = os.path.join(os.path.dirname(old_path), new_name)

            if os.path.exists(new_path):
                QMessageBox.warning(self, "Name Exists", f"'{new_name}' already exists.")
                return

            try:
                os.rename(old_path, new_path)
                self._refresh_directory()

                if hasattr(self.parent_window, 'log_message'):
                    self.parent_window.log_message(f"Renamed: {old_name} → {new_name}")
            except Exception as e:
                QMessageBox.critical(self, "Error", f"Failed to rename:\n{str(e)}")


    def _delete_item(self, path, name): #vers 2
        """Delete selected file or folder (path/name, works for either view)"""
        file_info = QFileInfo(path)
        item_type = "folder" if file_info.isDir() else "file"

        reply = QMessageBox.question(
            self,
            "Confirm Delete",
            f"Delete {item_type} '{name}'?\nThis action cannot be undone.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
        )

        if reply == QMessageBox.StandardButton.Yes:
            try:
                if file_info.isDir():
                    import shutil
                    shutil.rmtree(path)
                else:
                    os.remove(path)

                self._refresh_directory()

                if hasattr(self.parent_window, 'log_message'):
                    self.parent_window.log_message(f"Deleted: {name}")
            except Exception as e:
                QMessageBox.critical(self, "Error", f"Failed to delete:\n{str(e)}")


    def _show_properties(self, path): #vers 2
        """Show file/folder properties dialog (path, works for either view)"""
        file_info = QFileInfo(path)

        props_text = f"Name: {file_info.fileName()}\n"
        props_text += f"Path: {file_info.absolutePath()}\n"
        props_text += f"Type: {'Folder' if file_info.isDir() else 'File'}\n"

        if file_info.isFile():
            props_text += f"Size: {self._format_file_size(file_info.size())}\n"

        props_text += f"Modified: {file_info.lastModified().toString('yyyy-MM-dd HH:mm:ss')}\n"
        props_text += f"Permissions: {'Read' if file_info.isReadable() else ''}"
        props_text += f"{' Write' if file_info.isWritable() else ''}"
        props_text += f"{' Execute' if file_info.isExecutable() else ''}"

        QMessageBox.information(self, "Properties", props_text)


    def _parse_filter(self, filter_text): #vers 1
        """Parse filter text into name filters list"""
        # Extract patterns from filter text like "Images (*.png *.jpg)"
        import re
        patterns = re.findall(r'\*\.\w+', filter_text)

        if patterns:
            return patterns
        else:
            return ["*"]  # All files

    def _format_file_size(self, size): #vers 1
        """Format file size in human readable format"""
        for unit in ['B', 'KB', 'MB', 'GB', 'TB']:
            if size < 1024.0:
                return f"{size:.1f} {unit}"
            size /= 1024.0
        return f"{size:.1f} PB"

    def _path_icon(self, path: str, fallback_creator) -> 'QIcon': #vers 1
        """Real OS icon for a places/bookmark path if System Icons is
        on and the path exists, else the given SVGIconFactory fallback."""
        if self._use_system_icons and os.path.exists(path):
            try:
                return self._icon_provider.icon(QFileInfo(path))
            except Exception:
                pass
        return fallback_creator()

    def _file_icon_for(self, file_info) -> 'QIcon': #vers 1
        """Icon for a real filesystem entry - real native OS icon
        (QFileIconProvider) when System Icons is on, else the themed
        SVGIconFactory icon set."""
        if self._use_system_icons:
            try:
                return self._icon_provider.icon(file_info)
            except Exception:
                pass
        if file_info.isDir():
            return self._create_folder_icon()
        return self._get_file_icon(file_info.suffix())

    def _get_file_icon(self, extension): #vers 1
        """Get appropriate icon for file extension"""
        ext = extension.lower()

        # Image files
        if ext in ['png', 'jpg', 'jpeg', 'bmp', 'gif', 'dds']:
            return self._create_image_icon()

        # Archive files
        elif ext in ['img', 'zip', 'rar', '7z']:
            return self._create_archive_icon()

        # Model files
        elif ext in ['dff', 'obj', 'fbx']:
            return self._create_model_icon()

        # Texture files
        elif ext == 'txd':
            return self._create_texture_icon()

        # Collision files
        elif ext == 'col':
            return self._create_collision_icon()

        # Text files
        elif ext in ['txt', 'log', 'ini', 'cfg']:
            return self._create_text_icon()

        # Default file icon
        else:
            return self._create_file_icon()

    def _apply_filter(self): #vers 1
        """Apply current file filter"""
        self._refresh_directory()

    def _apply_theme(self): #vers 1
        """Apply theme via shared workshop_theme module."""
        try:
            from apps.methods.workshop_theme import apply_workshop_theme
            mw = getattr(self, 'main_window', None)
            apply_workshop_theme(self, mw)
        except Exception as e:
            print(f"File browser theme error: {e}")

    def _apply_theme_styling(self): #vers 2
        """Apply IMG Factory theme to dialog"""
        try:
            # Get theme colors from parent
            if hasattr(self.parent_window, 'app_settings'):
                theme_name = self.parent_window.app_settings.current_settings.get("theme", "IMG_Factory")
                theme_data = self.parent_window.app_settings.themes.get(theme_name, {})
                colors = theme_data.get('colors', {})

                if colors:
                    self._apply_colors(colors)
                    return
        except Exception as e:
            print(f"Theme error: {e}")

        # Detect system theme
        from PyQt6.QtWidgets import QApplication
        from PyQt6.QtGui import QPalette

        palette = QApplication.palette()
        is_dark = palette.color(QPalette.ColorRole.Window).lightness() < 128

        if is_dark:
            self._apply_dark_fallback()
        else:
            self._apply_light_fallback()

    def _apply_dark_fallback(self): #vers 2
        """Apply dark theme fallback for standalone mode"""
        colors = {
            'bg_primary': '#2b2b2b',
            'bg_secondary': '#3c3c3c',
            'bg_tertiary': '#4a4a4a',
            'panel_bg': '#333333',
            'text_primary': '#ffffff',
            'text_secondary': '#cccccc',
            'text_accent': '#60A5FA',
            'accent_primary': '#FFECEE',
            'accent_secondary': '#FFD4D9',
            'border': '#666666',
            'button_normal': '#404040',
            'button_hover': '#505050',
            'button_pressed': '#303030',
            'selection_background': '#0078d4',
            'selection_text': '#ffffff',
            'table_row_even': '#2b2b2b',
            'table_row_odd': '#353535'
        }
        self._apply_colors(colors)

    def _apply_light_fallback(self): #vers 2
        """Apply light theme fallback for standalone mode"""
        colors = {
            'bg_primary': '#ffffff',
            'bg_secondary': '#f5f5f5',
            'bg_tertiary': '#e9ecef',
            'panel_bg': '#f0f0f0',
            'text_primary': '#000000',
            'text_secondary': '#666666',
            'text_accent': '#0066cc',
            'accent_primary': '#0078d4',
            'accent_secondary': '#0A7Ad4',
            'border': '#cccccc',
            'button_normal': '#e0e0e0',
            'button_hover': '#d0d0d0',
            'button_pressed': '#b0b0b0',
            'selection_background': '#0078d4',
            'selection_text': '#ffffff',
            'table_row_even': '#ffffff',
            'table_row_odd': '#f8f9fa'
        }
        self._apply_colors(colors)


    def _apply_colors(self, colors): #vers 2
        """Apply theme colors to dialog components"""
        bg_primary = colors.get('bg_primary', '#ffffff')
        bg_secondary = colors.get('bg_secondary', '#f5f5f5')
        bg_tertiary = colors.get('bg_tertiary', '#e9ecef')
        panel_bg = colors.get('panel_bg', '#f0f0f0')
        text_primary = colors.get('text_primary', '#000000')
        text_secondary = colors.get('text_secondary', '#666666')
        text_accent = colors.get('text_accent', '#0066cc')
        accent = colors.get('accent_primary', '#0078d4')
        accent_secondary = colors.get('accent_secondary', '#0A7Ad4')
        border = colors.get('border', '#cccccc')
        button_normal = colors.get('button_normal', '#e0e0e0')
        button_hover = colors.get('button_hover', '#d0d0d0')
        button_pressed = colors.get('button_pressed', '#b0b0b0')
        selection_bg = colors.get('selection_background', '#0078d4')
        selection_text = colors.get('selection_text', '#ffffff')
        table_row_even = colors.get('table_row_even', bg_primary)
        table_row_odd = colors.get('table_row_odd', bg_secondary)

        dialog_style = f"""
            QDialog {{
                background-color: {bg_primary};
                color: {text_primary};
            }}
            QTreeWidget {{
                background-color: {bg_primary};
                alternate-background-color: {bg_secondary};
                border: 1px solid {border};
                color: {text_primary};
                selection-background-color: {selection_bg};
                selection-color: {selection_text};
            }}
            QTreeWidget::item {{
                padding: 4px;
            }}
            QTreeWidget::item:hover {{
                background-color: {bg_secondary};
            }}
            QTreeWidget::item:alternate {{
                background-color: {table_row_odd};
            }}
            QHeaderView::section {{
                background-color: {bg_secondary};
                color: {text_primary};
                border: 1px solid {border};
                padding: 2px;
                font-weight: bold;
            }}
            QPushButton {{
                background-color: {button_normal};
                color: {text_primary};
                border: 1px solid {border};
                border-radius: 4px;
                padding: 5px 10px;
            }}
            QPushButton:hover {{
                background-color: {button_hover};
            }}
            QPushButton:pressed {{
                background-color: {button_pressed};
            }}
            QPushButton:disabled {{
                background-color: {bg_secondary};
                color: {text_secondary};
            }}
            QLineEdit, QComboBox {{
                background-color: {bg_primary};
                color: {text_primary};
                border: 1px solid {border};
                border-radius: 3px;
                padding: 4px;
            }}
            QLineEdit:focus, QComboBox:focus {{
                border: 1px solid {accent};
            }}
            QLabel {{
                color: {text_primary};
            }}
            QToolBar {{
                background-color: {bg_secondary};
                border-bottom: 1px solid {border};
                spacing: 3px;
                padding: 2px;
            }}
            QWidget {{
                background-color: {panel_bg};
            }}
            QTreeWidget#SidebarTree {{
                background-color: {bg_tertiary};
                border: none;
            }}
            QWidget#SidebarSection, QWidget#SidebarHeader {{
                background-color: {bg_tertiary};
            }}
            QPushButton#SidebarCollapseBtn {{
                background-color: transparent;
                border: none;
                padding: 0px;
            }}
            QFrame#PathContainer {{
                background-color: {bg_primary};
                border: 1px solid {border};
                border-radius: 3px;
            }}
            QTabWidget::pane {{
                border: none;
                margin: 0px;
                padding: 0px;
            }}
            QTabBar::tab {{
                padding: 2px 10px;
                margin: 0px;
            }}
        """

        self.setStyleSheet(dialog_style)


    def _apply_default_styling(self): #vers 2
        """Apply default styling if theme not available"""
        default_style = """
            QDialog {
                background-color: palette(buttonText);
                color: palette(windowText);
            }
            QTreeWidget {
                background-color: palette(buttonText);
                alternate-background-color: palette(window);
                border: 1px solid palette(mid);
                color: palette(windowText);
                selection-background-color: palette(highlight);
                selection-color: palette(buttonText);
            }
            QTreeWidget::item {
                padding: 4px;
            }
            QTreeWidget::item:hover {
                background-color: palette(button);
            }
            QTreeWidget::item:selected {
                background-color: palette(highlight);
                color: palette(buttonText);
            }
            QHeaderView::section {
                background-color: palette(window);
                color: palette(windowText);
                border: 1px solid palette(mid);
                padding: 5px;
                font-weight: bold;
            }
            QPushButton {
                background-color: palette(mid);
                color: palette(windowText);
                border: 1px solid palette(mid);
                border-radius: 4px;
                padding: 5px 10px;
            }
            QPushButton:hover {
                background-color: palette(mid);
            }
            QPushButton:pressed {
                background-color: palette(mid);
            }
            QLineEdit, QComboBox {
                background-color: palette(buttonText);
                color: palette(windowText);
                border: 1px solid palette(mid);
                border-radius: 3px;
                padding: 4px;
            }
            QLabel {
                color: palette(windowText);
            }
        """
        self.setStyleSheet(default_style)

    def _resolve_icon_color(self) -> str: #vers 1
        """Icon colour priority: IMG Factory theme (docked/parent has
        app_settings) first, else app_settings_system override if set,
        else OS/system palette (follows KDE/Wayland theme live)."""
        try:
            if hasattr(self.parent_window, 'app_settings'):
                theme_name = self.parent_window.app_settings.current_settings.get("theme", "IMG_Factory")
                theme_data = self.parent_window.app_settings.themes.get(theme_name, {})
                colors = theme_data.get('colors', {})
                if colors.get('text_primary'):
                    return colors['text_primary']
        except Exception:
            pass
        try:
            from apps.utils.app_settings_system import AppSettings
            sys_settings = AppSettings()
            sys_color = sys_settings.current_settings.get('icon_color')
            if sys_color:
                return sys_color
        except Exception:
            pass
        palette = QApplication.palette()
        is_dark = palette.color(QPalette.ColorRole.Window).lightness() < 128
        return '#ffffff' if is_dark else '#202020'

    def _create_svg_icon(self, svg_data, size=20): #vers 2
        """Convert raw SVG bytes to a themed QIcon (legacy call path -
        kept for any external caller still passing hand-built SVG)."""
        color = self._resolve_icon_color()
        if isinstance(svg_data, bytes):
            svg_data = svg_data.decode('utf-8')
        svg_data = svg_data.replace('currentColor', color)
        renderer = QSvgRenderer(svg_data.encode('utf-8'))
        pixmap = QPixmap(QSize(size, size))
        pixmap.fill(Qt.GlobalColor.transparent)
        painter = QPainter(pixmap)
        renderer.render(painter)
        painter.end()
        return QIcon(pixmap)

    #    Icon lookup — routed through the shared, theme-aware
    #    SVGIconFactory (apps/methods/imgfactory_svg_icons.py) instead
    #    of this file's own hand-drawn SVGs, which never applied a
    #    colour and were invisible on dark themes. Method names kept
    #    unchanged so every existing call site still works.

    def _icon(self, name: str) -> 'QIcon': #vers 1
        """Look up a themed icon by SVGIconFactory method name."""
        color = self._resolve_icon_color()
        try:
            if SVGIconFactory is not None:
                return getattr(SVGIconFactory, name)(20, color)
        except Exception:
            pass
        return QIcon()

    def _create_folder_icon(self):     return self._icon('get_folder_icon')     #vers 2
    def _create_file_icon(self):       return self._icon('get_file_icon')       #vers 2
    def _create_image_icon(self):      return self._icon('get_image_icon')      #vers 2
    def _create_archive_icon(self):    return self._icon('package_icon')        #vers 2
    def _create_model_icon(self):      return self._icon('mesh_icon')           #vers 2
    def _create_texture_icon(self):    return self._icon('texture_icon')        #vers 2
    def _create_collision_icon(self):  return self._icon('get_col_file_icon')   #vers 2
    def _create_text_icon(self):       return self._icon('get_file_icon')       #vers 2
    def _create_back_icon(self):       return self._icon('get_back_icon')       #vers 2
    def _create_forward_icon(self):    return self._icon('get_forward_icon')    #vers 2
    def _create_up_icon(self):         return self._icon('get_up_icon')         #vers 2
    def _create_refresh_icon(self):    return self._icon('get_refresh_icon')    #vers 2
    def _create_home_icon(self):       return self._icon('get_home_icon')       #vers 2
    def _create_desktop_icon(self):    return self._icon('box_icon')            #vers 2
    def _create_document_icon(self):   return self._icon('get_new_file_icon')   #vers 2
    def _create_download_icon(self):   return self._icon('get_import_icon')     #vers 2
    def _create_drive_icon(self):      return self._icon('box_icon')            #vers 2
    def _create_open_icon(self):       return self._icon('get_open_icon')       #vers 2
    def _create_save_icon(self):       return self._icon('get_save_icon')       #vers 2
    def _create_import_icon(self):     return self._icon('get_import_icon')     #vers 2
    def _create_export_icon(self):     return self._icon('get_export_icon')     #vers 2
    def _create_cancel_icon(self):     return self._icon('get_close_icon')      #vers 2
    def _create_edit_icon(self):       return self._icon('get_edit_icon')       #vers 2
    def _create_delete_icon(self):     return self._icon('get_trash_icon')      #vers 2
    def _create_properties_icon(self): return self._icon('get_properties_icon') #vers 2
    def _create_new_folder_icon(self): return self._icon('get_new_folder_icon') #vers 1


@staticmethod
def get_open_filename(parent=None, caption="Open File", directory="", file_filter="All Files (*.*)"): #vers 1
    """Static method to show open file dialog"""
    dialog = DolphinFileDialog(parent, mode='open', multi_select=False, file_filter=file_filter)

    if directory:
        dialog.current_path = directory
        dialog._load_directory(directory)

    if dialog.exec() == QDialog.DialogCode.Accepted:
        return dialog.get_selected_path()
    return None

@staticmethod
def get_open_filenames(parent=None, caption="Open Files", directory="", file_filter="All Files (*.*)"): #vers 1
    """Static method to show open multiple files dialog"""
    dialog = DolphinFileDialog(parent, mode='open', multi_select=True, file_filter=file_filter)

    if directory:
        dialog.current_path = directory
        dialog._load_directory(directory)

    if dialog.exec() == QDialog.DialogCode.Accepted:
        return dialog.get_selected_paths()
    return []

@staticmethod
def get_save_filename(parent=None, caption="Save File", directory="", file_filter="All Files (*.*)"): #vers 1
    """Static method to show save file dialog"""
    dialog = DolphinFileDialog(parent, mode='save', multi_select=False, file_filter=file_filter)

    if directory:
        dialog.current_path = directory
        dialog._load_directory(directory)

    if dialog.exec() == QDialog.DialogCode.Accepted:
        return dialog.get_selected_path()
    return None

@staticmethod
def get_existing_directory(parent=None, caption="Select Directory", directory=""): #vers 1
    """Static method to show directory selection dialog"""
    dialog = DolphinFileDialog(parent, mode='open', multi_select=False, file_filter="")
    dialog.setWindowTitle(caption)

    if directory:
        dialog.current_path = directory
        dialog._load_directory(directory)

    # Modify to only allow folder selection
    dialog.tree.itemDoubleClicked.disconnect()
    dialog.tree.itemDoubleClicked.connect(lambda item, col: dialog._load_directory(item.data(0, Qt.ItemDataRole.UserRole)) if QFileInfo(item.data(0, Qt.ItemDataRole.UserRole)).isDir() else None)

    if dialog.exec() == QDialog.DialogCode.Accepted:
        return dialog.current_path
    return None


# Test code - Remove before production
if __name__ == "__main__":
    from PyQt6.QtWidgets import QApplication
    import sys

    app = QApplication(sys.argv)

    # Test open dialog
    dialog = DolphinFileDialog(None, mode='open', multi_select=False, file_filter="All Files (*.*)")
    dialog._setup_ui()

    if dialog.exec() == QDialog.DialogCode.Accepted:
        print("Selected:", dialog.get_selected_path())
    else:
        print("Cancelled")

    sys.exit()
