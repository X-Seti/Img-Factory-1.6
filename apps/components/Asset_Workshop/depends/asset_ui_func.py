#this belongs in apps/components/Asset_Workshop/depends/asset_ui_func.py - Version: 2
# X-Seti - October 29 2026 - IMG Factory 1.6 - Asset Workshop layout

"""
Asset Workshop layout: panels, ribbons, checker tab, texture panels.
Mixed into AssetWorkshop (asset_workshop.py) ahead of TXDWorkshop.
"""

##class AssetUIMixin: -
# _build_checker_tab
# _build_toolbars
# _create_left_panel
# _create_middle_panel
# _create_right_panel
# _make_column
# _on_checker_sync_scroll
# _on_checker_view_changed
# _on_texture_selected
# _populate_columns_view
# _populate_cross_reference_view
# _populate_merged_view
# _refresh_checker_summary
# _restore_toolbar_state
# _set_column_header
# _set_transform_buttons_enabled
# _show_checked_files_popup
# _show_diff_popup
# _show_texture_context_menu
# _show_txd_info
# _update_texture_info
# _update_texture_panel_visibility
# _update_transform_text_panel_visibility
# _xref_context_menu
# setup_ui

from PyQt6.QtWidgets import (QApplication, QWidget, QVBoxLayout,
    QHBoxLayout, QSplitter, QListWidget, QDialog, QLabel, QPushButton, QFrame, QTextEdit,  QTableWidget, QTableWidgetItem, QAbstractItemView, QMenu, QComboBox, QTabWidget, QStackedWidget
)
from PyQt6.QtWidgets import QMainWindow
from PyQt6.QtCore import Qt, QSize
from PyQt6.QtGui import QFont, QColor

from apps.methods.dock_panels import add_outer_dock, make_outer_mw
from apps.methods.txd_dialogs import ZoomablePreview
from apps.methods.txd_versions import ( is_bumpmap_supported)


App_name = "Asset Workshop"


class _NumericSortItem(QTableWidgetItem): #vers 1
    """Table item sorting by real numeric value, not text. Blank
    IDs sort last instead of crashing on empty string."""
    def __lt__(self, other): #vers 1
        def _num(item):
            try:
                return int(item.text())
            except (ValueError, AttributeError):
                return float('inf')
        return _num(self) < _num(other)


class AssetUIMixin: #vers 1
    """Asset Workshop layout: panels, ribbons, checker tab, texture panels."""

    def setup_ui(self): #vers 10
        """Setup the main UI layout"""
        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(5, 5, 5, 5)
        main_layout.setSpacing(5)

        # Toolbar - hidden when embedded in main window tab
        toolbar = self._create_toolbar()
        self._workshop_toolbar = toolbar
        if not self.standalone_mode:
            toolbar.setVisible(False)
        main_layout.addWidget(toolbar)

        # Tab bar for multiple TXD files
        self.txd_tabs = QTabWidget()
        self.txd_tabs.setTabsClosable(True)
        self.txd_tabs.tabCloseRequested.connect(self._close_txd_tab)
        self.txd_tabs.currentChanged.connect(self._switch_txd_tab)

        # Create initial tab with main content
        initial_tab = QWidget()
        tab_layout = QVBoxLayout(initial_tab)
        tab_layout.setContentsMargins(0, 0, 0, 0)


        # Viewport + ribbons central; Textures list as a dock
        #left_panel = self._create_left_panel() # Disabled pane.
        middle_panel = self._create_middle_panel()
        self._middle_panel = middle_panel
        right_panel = self._create_right_panel()
        outer_mw = make_outer_mw(right_panel)
        self._outer_mw = outer_mw
        self._outer_docks = []          # Textures dock follows TXD load state
        self._tex_dock = add_outer_dock(self, outer_mw, "Textures", middle_panel, 250)
        self._tex_dock.setVisible(False)   # hidden until a TXD is actually loaded
        main_layout.addWidget(outer_mw)
        from PyQt6.QtCore import QTimer
        QTimer.singleShot(400, self._restore_outer_layout)
        QTimer.singleShot(450, self._update_texture_panel_visibility)
        self.window_closed.connect(self._save_outer_layout)

        # Apply themed icons now UI is fully built
        self._refresh_icons()

        # Connect signals AFTER texture_table is created
        #self._connect_texture_table_signals()

        # NEW: Status bar at bottom with texture info
        #self.status_bar = self._create_status_bar()
        #main_layout.addWidget(self.status_bar)

        # Status indicators - hidden when embedded in main window tab, or
        # if the user has turned off Show "Workshop" status bar
        if hasattr(self, '_setup_status_indicators'):
            status_frame = self._setup_status_indicators()
            if not self.standalone_mode or not getattr(self, 'show_status_bar', True):
                status_frame.setVisible(False)
            main_layout.addWidget(status_frame)

    def _show_checked_files_popup(self, names): #vers 1
        """Small non-modal popup listing checked files, auto-closes
        after 5 seconds."""
        from PyQt6.QtCore import QTimer
        popup = QDialog(self)
        popup.setWindowTitle(f"Checked files ({len(names)})")
        v = QVBoxLayout(popup)
        lst = QListWidget()
        lst.addItems(names)
        v.addWidget(lst)
        popup.resize(360, 400)
        popup.setModal(False)
        popup.show()
        QTimer.singleShot(5000, popup.close)

    def _build_checker_tab(self): #vers 2
        """Build Asset Check: summary/actions row + stacked 4-column/
        merged/cross-reference views. Added as a page in the right
        panel's own viewport stack, alongside the texture preview."""
        tab = QWidget()
        lay = QVBoxLayout(tab)
        lay.setContentsMargins(2, 2, 2, 2)

        self._checker_top = QHBoxLayout()
        lay.addLayout(self._checker_top)

        self._checker_stack = QStackedWidget()
        lay.addWidget(self._checker_stack, 1)

        columns_widget = QWidget()
        columns_lay = QHBoxLayout(columns_widget)
        splitter = QSplitter(Qt.Orientation.Horizontal)
        columns_lay.addWidget(splitter)

        self.id_list = self._make_column(splitter, "ID", None)
        self.ide_list = self._make_column(splitter, "IDE entry list", None)
        self.img_list = self._make_column(splitter, "IMG archive", None)
        self.col_list = self._make_column(splitter, "COL archive", None)
        self.error_list = self._make_column(splitter, "Error list", None)
        self._checker_stack.addWidget(columns_widget)

        self._sync_lists = [self.id_list, self.ide_list, self.img_list, self.col_list]
        self._sync_guard = False
        for lst in self._sync_lists:
            lst.verticalScrollBar().valueChanged.connect(self._on_checker_sync_scroll)

        self.merged_table = QTableWidget()
        self.merged_table.setColumnCount(3)
        self.merged_table.setHorizontalHeaderLabels(["Model Name", "Source", "Status"])
        self.merged_table.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
        self.merged_table.horizontalHeader().setStretchLastSection(True)
        self._checker_stack.addWidget(self.merged_table)

        self.xref_table = QTableWidget()
        self.xref_table.setColumnCount(6)
        self.xref_table.setHorizontalHeaderLabels(
            ["ID", "DFF", "COL", "IDE Model Name", "Texture entry", "Errors"])
        self.xref_table.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
        self.xref_table.horizontalHeader().setStretchLastSection(True)
        self.xref_table.setSortingEnabled(True)
        self.xref_table.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.xref_table.customContextMenuRequested.connect(self._xref_context_menu)
        self._checker_stack.addWidget(self.xref_table)

        self._checker_tab = tab
        self._viewport_stack.addWidget(tab)

    def _refresh_checker_summary(self): #vers 1
        """Rebuild the checker tab's own summary/actions row,
        recomputed against self.result each time."""
        while self._checker_top.count():
            item = self._checker_top.takeAt(0)
            if item.widget():
                item.widget().deleteLater()

        all_checked = self._all_checked_names()
        if not all_checked:
            summary = "No sibling files found"
        elif len(all_checked) <= 3:
            summary = "Checked: " + ", ".join(all_checked)
        else:
            summary = (f"Checked: {', '.join(all_checked[:2])} "
                       f"+{len(all_checked) - 2} more")
        self._checker_top.addWidget(QLabel(summary))
        if len(all_checked) > 3:
            show_files_btn = QPushButton("Show list")
            show_files_btn.clicked.connect(
                lambda: self._show_checked_files_popup(all_checked))
            self._checker_top.addWidget(show_files_btn)
        self._checker_top.addStretch()
        self._checker_top.addWidget(QLabel("View:"))
        self.checker_view_combo = QComboBox()
        # Master IDE is always reachable, even with nothing loaded yet
        # (Sep 17 2026, per Keith: it "can't be called from Asset_
        # workshop.py when that's standalone" - previously gated on
        # self.result.ide_path, so a bare/empty Asset Workshop had no
        # way to reach it at all). With no real IDE path yet, it just
        # opens Master IDE empty, ready for its own Load/Insert buttons.
        self.checker_view_combo.addItems(
            ["4-Column View", "Merged View", "Cross-Reference Table", "Master IDE..."])
        self.checker_view_combo.currentIndexChanged.connect(self._on_checker_view_changed)
        self._checker_top.addWidget(self.checker_view_combo)
        if hasattr(self, 'master_ide_ribbon_btn'):
            self.master_ide_ribbon_btn.setEnabled(True)
        export_report_btn = QPushButton("Export Full Report...")
        export_report_btn.setToolTip(
            "Export every real missing/extra entry across IMG, COL, and IDE - "
            "the same real check every cross-reference row shows, in one file.")
        export_report_btn.clicked.connect(self._on_export_full_report)
        self._checker_top.addWidget(export_report_btn)

        ide_count = len(self.result.ide_names)
        img_extra_count = len(self.result.img_extra_over_ide) if self.result.img_path and self.result.ide_path else 0
        img_missing_count = len(self.result.missing_from_img) if self.result.img_path and self.result.ide_path else 0
        col_extra_count = len(self.result.col_extra_over_ide) if self.result.col_path and self.result.ide_path else 0
        col_missing_count = len(self.result.missing_from_col) if self.result.col_path and self.result.ide_path else 0
        self._set_column_header(self.ide_list, "IDE entry list", ide_count)
        self._set_column_header(self.img_list, "IMG archive", ide_count,
            diffs=self._real_diffs(img_extra_count, img_missing_count, "IMG"))
        self._set_column_header(self.col_list, "COL archive", ide_count,
            diffs=self._real_diffs(col_extra_count, col_missing_count, "COL"))

    def _set_column_header(self, lst, title, count, diffs=None): #vers 1
        """Rebuild one column's header row (label + diff buttons)."""
        header_lay = lst.property("header_layout")
        if header_lay is None:
            return
        while header_lay.count():
            item = header_lay.takeAt(0)
            if item.widget():
                item.widget().deleteLater()
        label_text = title if count is None else f"{title} ({count})"
        header_lbl = QLabel(label_text)
        bright = self.palette().color(self.palette().currentColorGroup(),
                                       self.palette().ColorRole.BrightText)
        header_lbl.setStyleSheet(f"color: {bright.name()}; font-weight: bold;")
        header_lay.addWidget(header_lbl)
        for label, tooltip, on_click in (diffs or []):
            diff_btn = QPushButton(label)
            diff_btn.setFlat(True)
            diff_btn.setCursor(Qt.CursorShape.PointingHandCursor)
            diff_btn.setStyleSheet(
                "text-decoration: underline; padding: 0px 3px; font-size: 11px;")
            diff_btn.setMaximumWidth(diff_btn.fontMetrics().horizontalAdvance(label) + 10)
            diff_btn.setToolTip(tooltip)
            if on_click:
                diff_btn.clicked.connect(on_click)
            header_lay.addWidget(diff_btn)
        header_lay.addStretch()

    def _make_column(self, splitter, title, count, diffs=None): #vers 1
        container = QWidget()
        container.setMinimumWidth(150)
        v = QVBoxLayout(container)
        v.setContentsMargins(2, 2, 2, 2)
        header_row = QHBoxLayout()
        v.addLayout(header_row)
        lst = QListWidget()
        lst.setAlternatingRowColors(True)
        lst.setProperty("header_layout", header_row)
        v.addWidget(lst)
        splitter.addWidget(container)
        self._set_column_header(lst, title, count, diffs)
        return lst

    def _on_checker_sync_scroll(self, value): #vers 1
        if self._sync_guard:
            return
        self._sync_guard = True
        try:
            for lst in self._sync_lists:
                if lst.verticalScrollBar().value() != value:
                    lst.verticalScrollBar().setValue(value)
        finally:
            self._sync_guard = False

    def _show_diff_popup(self, names, title): #vers 1
        dlg = QDialog(self)
        dlg.setWindowTitle(title)
        dlg.resize(360, 400)
        v = QVBoxLayout(dlg)
        v.addWidget(QLabel(f"{len(names)} entr{'y' if len(names) == 1 else 'ies'}:"))
        lst = QListWidget()
        lst.addItems(sorted(names))
        v.addWidget(lst)
        btn_row = QHBoxLayout()
        copy_one_btn = QPushButton("Copy Selected")
        copy_one_btn.clicked.connect(
            lambda: QApplication.clipboard().setText(
                lst.currentItem().text() if lst.currentItem() else ""))
        copy_all_btn = QPushButton("Copy All")
        copy_all_btn.clicked.connect(
            lambda: QApplication.clipboard().setText("\n".join(sorted(names))))
        export_btn = QPushButton("Export to File...")
        export_btn.clicked.connect(lambda: self._export_text_report(title, sorted(names)))
        btn_row.addWidget(copy_one_btn)
        btn_row.addWidget(copy_all_btn)
        btn_row.addWidget(export_btn)
        btn_row.addStretch()
        v.addLayout(btn_row)
        dlg.exec()

    def _on_checker_view_changed(self, index): #vers 2
        """Master IDE isn't a real page in _checker_stack - it opens
        its own workshop - so route that entry to _on_master_ide and
        snap the dropdown back to whatever page is actually showing."""
        if self.checker_view_combo.itemText(index) == "Master IDE...":
            self._on_master_ide()
            self.checker_view_combo.blockSignals(True)
            self.checker_view_combo.setCurrentIndex(self._checker_stack.currentIndex())
            self.checker_view_combo.blockSignals(False)
            return
        self._checker_stack.setCurrentIndex(index)

    def _populate_columns_view(self): #vers 1
        r = self.result

        def _ide_order_key(name):
            return (r.ide_id_by_name.get(name, float('inf')), name)
        self.img_list.clear()
        self.col_list.clear()
        self.ide_list.clear()
        self.id_list.clear()
        self.error_list.clear()
        self.img_list.addItems(sorted(r.img_names, key=_ide_order_key))
        self.col_list.addItems(sorted(r.col_names, key=_ide_order_key))
        sorted_ide_names = sorted(r.ide_names, key=lambda n: r.ide_id_by_name.get(n, 0))
        self.ide_list.addItems(sorted_ide_names)
        self.id_list.addItems(str(r.ide_id_by_name.get(name, "")) for name in sorted_ide_names)

        errors = []
        for name in sorted(r.missing_from_col):
            errors.append(f"Missing in COL: {name}")
        for name in sorted(r.missing_from_img):
            errors.append(f"Missing in IMG: {name}")
        for name in sorted(r.not_in_ide):
            errors.append(f"Not found in IDE: {name}")
        if not errors:
            errors.append("No mismatches found among the checked sources.")
        self.error_list.addItems(errors)

    def _populate_merged_view(self): #vers 1
        base = self.palette().color(self.palette().currentColorGroup(),
                                     self.palette().ColorRole.Base)
        shades = {
            'IMG': base.lighter(112),
            'COL': base,
            'IDE': base.darker(108),
        }
        rows = []
        for name in self.result.all_names:
            if name in self.result.img_names:
                rows.append((name, 'IMG', self.result.status_for(name)))
            if name in self.result.col_names:
                rows.append((name, 'COL', self.result.status_for(name)))
            if name in self.result.ide_names:
                rows.append((name, 'IDE', self.result.status_for(name)))

        self.merged_table.setRowCount(len(rows))
        for row, (name, source, status) in enumerate(rows):
            for col, val in enumerate([name, source, status]):
                item = QTableWidgetItem(val)
                item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
                item.setBackground(shades[source])
                self.merged_table.setItem(row, col, item)

    def _populate_cross_reference_view(self): #vers 1
        """Uses a real numeric-sort item for the ID column only, so
        "10000" doesn't sort before "1001"."""
        base = self.palette().color(self.palette().currentColorGroup(),
                                     self.palette().ColorRole.Base)
        error_tint = QColor(
            min(255, base.red() + 40), max(0, base.green() - 25), max(0, base.blue() - 25))

        rows = self.result.cross_reference_rows()
        self.xref_table.setSortingEnabled(False)
        self.xref_table.setRowCount(len(rows))
        for row, values in enumerate(rows):
            has_error = values[5] != "OK"
            for col, val in enumerate(values):
                item = _NumericSortItem(val) if col == 0 else QTableWidgetItem(val)
                item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
                if has_error:
                    item.setBackground(error_tint)
                self.xref_table.setItem(row, col, item)
        self.xref_table.setSortingEnabled(True)

    def _xref_context_menu(self, pos): #vers 1
        item = self.xref_table.itemAt(pos)
        if item is None:
            return
        row = item.row()
        menu = QMenu(self)

        copy_cell_act = menu.addAction("Copy Cell")
        copy_cell_act.triggered.connect(lambda: self._xref_copy_cell(item))
        copy_row_act = menu.addAction("Copy Row")
        copy_row_act.triggered.connect(lambda: self._xref_copy_row(row))

        texture_item = self.xref_table.item(row, 4)
        if (texture_item and "(missing)" in texture_item.text()
                and self.result.img_path):
            menu.addSeparator()
            txd_act = menu.addAction("Open in TXD Workshop to add missing texture")
            txd_act.triggered.connect(lambda: self._xref_open_txd_workshop())

        model_item = self.xref_table.item(row, 3)
        errors_item = self.xref_table.item(row, 5)
        model_name = model_item.text() if model_item else ""
        errors_text = errors_item.text() if errors_item else ""
        if model_name and "Missing DFF" in errors_text and self.result.img_path:
            menu.addSeparator()
            add_dff_act = menu.addAction("Add file externally... (DFF)")
            add_dff_act.triggered.connect(lambda: self._xref_add_missing_dff(model_name))
        if model_name and "Missing COL" in errors_text:
            menu.addSeparator()
            add_col_act = menu.addAction("Add file externally... (COL)")
            add_col_act.triggered.connect(lambda: self._xref_add_missing_col(model_name))

        menu.exec(self.xref_table.viewport().mapToGlobal(pos))

    def _update_transform_text_panel_visibility(self): #vers 6
        """Apply the icon/text/both display mode to the ribbon toolbars via
        QToolBar's native setToolButtonStyle - replaces the old approach of
        keeping two separate panels (icon-only strip + wide text panel) and
        toggling their visibility."""
        from PyQt6.QtCore import Qt as _Qt
        mode = getattr(self, 'button_display_mode', 'both')
        style = {
            'icons': _Qt.ToolButtonStyle.ToolButtonIconOnly,
            'text':  _Qt.ToolButtonStyle.ToolButtonTextOnly,
        }.get(mode, _Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
        for tb in (getattr(self, '_tb_asset', None),
                   getattr(self, '_tb_transform', None)):
            if tb:
                tb.setToolButtonStyle(style)

    def _create_left_panel(self): #vers 6
        """Left panel disabled - TXD file list moved out, Asset Check
        now uses the right panel's own viewport instead."""
        self.txd_list_widget = None
        return None

    def _create_middle_panel(self): #vers 7
        """Create middle panel - just the texture list. The old mini
        toolbar (Open/Save/Extract/Undo) was dropped (Sep 17 2026) -
        those are all still reachable via the Tools menu and their
        existing keyboard shortcuts (Ctrl+O/S/E), so a second set of
        buttons here was redundant. This panel's real job is giving
        the user something to click to set self.selected_texture -
        without it, load/replace/save/preview all silently do
        nothing useful (see the disabled call site's own history)."""
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(250)

        layout = QVBoxLayout(panel)
        layout.setContentsMargins(5, 5, 5, 5)
        layout.setSpacing(4)

        # Header label - dock title shows "Textures" now
        #self._textures_header = QLabel("Textures")
        #self._textures_header.setFont(QFont("Arial", 10, QFont.Weight.Bold))
        #layout.addWidget(self._textures_header)

        #    Texture table
        self.texture_table = QTableWidget()
        self.texture_table.setColumnCount(2)
        self.texture_table.setHorizontalHeaderLabels(["Preview", "Details"])
        self.texture_table.horizontalHeader().setStretchLastSection(True)
        self.texture_table.setSelectionBehavior(
            QAbstractItemView.SelectionBehavior.SelectRows)
        self.texture_table.setSelectionMode(
            QAbstractItemView.SelectionMode.SingleSelection)
        self.texture_table.setAlternatingRowColors(True)
        self.texture_table.itemSelectionChanged.connect(self._on_texture_selected)
        self.texture_table.setIconSize(QSize(64, 64))
        self.texture_table.setContextMenuPolicy(
            Qt.ContextMenuPolicy.CustomContextMenu)
        self.texture_table.customContextMenuRequested.connect(
            self._show_texture_context_menu)
        layout.addWidget(self.texture_table)

        return panel

    def _create_right_panel(self): #vers 14
        """Right panel using QMainWindow + QToolBar for native docking.
        Central widget is a stack: texture preview or Asset Check
        (load_result swaps pages) - ribbons stay docked to inner_mw
        either way, unaffected by which page is showing."""
        icon_color = self._get_icon_color()

        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(250)
        self._right_panel_ref = panel   # used by visibility method
        has_bumpmap = False
        outer_layout = QVBoxLayout(panel)
        outer_layout.setContentsMargins(4, 4, 4, 4)
        outer_layout.setSpacing(3)

        from PyQt6.QtWidgets import QMainWindow
        inner_mw = QMainWindow()
        inner_mw.setWindowFlags(Qt.WindowType.Widget)
        inner_mw.setDockOptions(
            QMainWindow.DockOption.AllowNestedDocks |
            QMainWindow.DockOption.AllowTabbedDocks)
        self._inner_mw = inner_mw

        self.preview_widget = ZoomablePreview(self)
        self._viewport_stack = QStackedWidget()
        self._viewport_stack.addWidget(self.preview_widget)   # page 0
        inner_mw.setCentralWidget(self._viewport_stack)

        self._build_toolbars(inner_mw, icon_color)
        self._apply_custom_icons()

        outer_layout.addWidget(inner_mw, stretch=1)

        from PyQt6.QtCore import QTimer as _QTimer
        _QTimer.singleShot(400, self._restore_toolbar_state)

        self.window_closed.connect(self._save_toolbar_state)

        return panel

    def _build_toolbars(self, mw: 'QMainWindow', icon_color: str): #vers 10
        """Build all QToolBar instances using QAction (Model/COL Workshop
        pattern). Replaces the old DockableToolbar-based
        _create_transform_icon_panel/_create_transform_text_panel/
        _create_preview_controls panels. attr= names match the original
        QPushButton names exactly so _set_transform_buttons_enabled(),
        _set_selection_buttons_enabled(), _on_texture_selected() and
        _refresh_icons() elsewhere in this file keep working unchanged -
        QAction supports setEnabled()/setIcon()/setText() same as
        QPushButton."""
        from PyQt6.QtWidgets import QToolBar
        from PyQt6.QtGui import QAction
        _saved_px = 20
        try:
            import json
            _saved_px = json.loads(
                self._ribbon_config_path().read_text()
            ).get('icon_scale', 20)
        except Exception:
            pass
        icon_size = QSize(_saved_px, _saved_px)
        self._ribbon_actions = []

        def _tb(name, area=Qt.ToolBarArea.TopToolBarArea): #vers 1
            tb = QToolBar(name, mw)
            tb.setObjectName(name)
            tb.setIconSize(icon_size)
            tb.setMovable(True)
            tb.setFloatable(True)
            tb.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
            tb.customContextMenuRequested.connect(
                lambda pos, t=tb: self._toolbar_context_menu(t, pos))
            mw.addToolBar(area, tb)
            return tb

        def _act(tb, name, icon_fn, callback=None, checkable=False,
                 checked=False, attr=None, enabled=True): #vers 1
            try:
                icon = icon_fn(color=icon_color)
            except Exception:
                icon = self.icon_factory.settings_icon(color=icon_color)
            act = QAction(icon, name, mw)
            act.setToolTip(name)
            act.setCheckable(checkable)
            act.setEnabled(enabled)
            if checkable:
                act.setChecked(checked)
            if callback:
                if checkable:
                    act.toggled.connect(callback)
                else:
                    act.triggered.connect(callback)
            tb.addAction(act)
            self._ribbon_actions.append({
                'action': act, 'toolbar': tb, 'name': name,
                'icon_fn': icon_fn, 'checkable': checkable,
            })
            if attr:
                setattr(self, attr, act)
            return act

        #    Ribbon 0: Asset Check
        tb_asset = _tb("Asset Check")
        _act(tb_asset, "Load from .dat...", self.icon_factory.folder_icon,
             self._on_load_from_dat)
        _act(tb_asset, "Master IDE...", self.icon_factory.master_ide_icon,
             self._on_master_ide_ribbon, enabled=False, attr='master_ide_ribbon_btn')
        _act(tb_asset, "Integrity Check...", self.icon_factory.check_icon,
             self._on_integrity_check)
        tb_asset.addSeparator()
        _act(tb_asset, "Exclude Selected", self.icon_factory.exclude_icon,
             self._on_exclude_selected)
        _act(tb_asset, "Manage Exclusions...", self.icon_factory.list_icon,
             self._on_manage_exclusions)

        #    Ribbon 1: Transform
        tb_xform = _tb("Transform")
        _act(tb_xform, "Copy",  self.icon_factory.copy_icon,
             self._copy_texture,  enabled=False, attr='copy_btn')
        _act(tb_xform, "Paste", self.icon_factory.paste_icon,
             self._paste_texture, enabled=False, attr='paste_btn')
        tb_xform.addSeparator()
        _act(tb_xform, "Create",    self.icon_factory.create_texture_icon,
             self._create_new_texture_entry, attr='create_texture_btn')
        _act(tb_xform, "Delete",    self.icon_factory.delete_texture_icon,
             self._delete_texture,     enabled=False, attr='delete_texture_btn')
        _act(tb_xform, "Duplicate", self.icon_factory.duplicate_icon,
             self._duplicate_texture,  enabled=False, attr='duplicate_texture_btn')
        tb_xform.addSeparator()
        _act(tb_xform, "Paint", self.icon_factory.paint_texture_icon,
             self._open_paint_editor, enabled=False, attr='paint_btn')
        _act(tb_xform, "Check DFF",      self.icon_factory.analyze_icon,
             self._check_txd_vs_dff,    attr='check_dff_btn')
        _act(tb_xform, "Build from DFF", self.icon_factory.build_from_dff_icon,
             self._build_txd_from_dff,  attr='build_from_dff_btn')
        _act(tb_xform, "Filters", self.icon_factory.filter_icon,
             self._open_filters_dialog, enabled=False, attr='filters_btn')
        tb_xform.addSeparator()
        _act(tb_xform, "Properties",    self.icon_factory.texture_properties_icon,
             self.show_properties,          enabled=False, attr='props_btn')

        # Store toolbar refs
        self._tb_asset      = tb_asset
        self._tb_transform = tb_xform

        # Apply current icons-vs-text display mode
        self._update_transform_text_panel_visibility()

    def _restore_toolbar_state(self): #vers 5
        """Restore QMainWindow toolbar state from asset_workshop.json.
        Uses an explicit layout version - bumped whenever ribbons are
        added/removed/renamed - so a stale save from an older ribbon
        layout is cleanly rejected instead of silently failing to
        restore (Qt's own toolbar-name hashing does this invisibly and
        without any way to detect success/failure)."""
        mw = getattr(self, '_inner_mw', None)
        if mw is None:
            return
        try:
            import json
            from PyQt6.QtCore import QByteArray
            path = self._ribbon_config_path()
            if not path.exists():
                return
            data = json.loads(path.read_text())
            state_hex = data.get('toolbar_state')
            saved_version = data.get('toolbar_state_version')
            if state_hex and saved_version == self._RIBBON_LAYOUT_VERSION:
                ok = mw.restoreState(QByteArray.fromHex(state_hex.encode()),
                                      self._RIBBON_LAYOUT_VERSION)
                if ok:
                    self._set_status("Ribbon config loaded")
                    if self.main_window and hasattr(self.main_window, 'log_message'):
                        self.main_window.log_message(App_name + ": Ribbon config loaded")
                else:
                    print("[" + App_name + "] _restore_toolbar_state: restoreState() returned False")
            elif state_hex:
                print(f"[" + App_name + "] Saved ribbon layout is from an older version "
                      f"({saved_version} != {self._RIBBON_LAYOUT_VERSION}) - skipping, "
                      f"will save fresh on next change.")
        except Exception as _e:
            print(f"[{App_name}] _restore_toolbar_state error: {_e}")
        finally:
            # Safety net: restoreState() can leave a ribbon fully hidden
            # (e.g. if it was saved mid-drag, floating off-screen, or
            # squeezed out) with no way for the user to bring it back -
            # there's no "closed" state exposed anywhere for these ribbons,
            # so force every one of them visible no matter what happened
            # above. Only position/floating/row is meant to be restorable,
            # never full visibility.
            for tb in (getattr(self, '_tb_asset', None),
                       getattr(self, '_tb_transform', None)):
                if tb is not None:
                    tb.setVisible(True)
                    tb.toggleViewAction().setChecked(True)

    def _set_transform_buttons_enabled(self, enabled: bool): #vers 1
        """Enable/disable transform buttons in BOTH icon and text panels.
        The text panel's _btn() calls overwrite self.flip_vert_btn etc, so when
        the icon panel is visible (narrow mode) those self.X refs point to hidden
        text-panel buttons. Fix: enable all QPushButtons in the icon panel too.
        """
        # Text panel buttons (via self.X refs)
        transform_attrs = [
            'copy_btn', 'delete_texture_btn', 'duplicate_texture_btn',
            'filters_btn', 'paint_btn', 'props_btn',
        ]
        for attr in transform_attrs:
            btn = getattr(self, attr, None)
            if btn is not None:
                btn.setEnabled(enabled)

        # Icon panel buttons — find by walking the panel's children
        icon_panel = getattr(self, '_transform_icon_panel_ref', None)
        if icon_panel:
            from PyQt6.QtWidgets import QPushButton
            for btn in icon_panel.findChildren(QPushButton):
                btn.setEnabled(enabled)

    def _on_texture_selected(self): #vers 8
        """Handle texture selection"""
        try:
            row = self.texture_table.currentRow()

            # Invalid selection - disable everything
            if row < 0 or row >= len(self.texture_list):
                self.selected_texture = None

                # Disable all optional buttons
                if hasattr(self, 'props_btn'):
                    self.props_btn.setEnabled(False)
                if hasattr(self, 'duplicate_texture_btn'):
                    self.duplicate_texture_btn.setEnabled(False)
                if hasattr(self, 'delete_texture_btn'):
                    self.delete_texture_btn.setEnabled(False)

                # Disable all transform buttons in both panels
                self._set_transform_buttons_enabled(False)

                return

            # Valid selection - get texture data
            self.selected_texture = self.texture_list[row]

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
            if hasattr(self, 'props_btn'):
                self.props_btn.setEnabled(True)
            if hasattr(self, 'info_btn'):
                self.info_btn.setEnabled(True)
            if hasattr(self, 'duplicate_texture_btn'):
                self.duplicate_texture_btn.setEnabled(True)
            if hasattr(self, 'delete_texture_btn'):
                self.delete_texture_btn.setEnabled(True)

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

    def _update_texture_panel_visibility(self): #vers 2
        """Show the texture list panel only while a TXD is actually
        loaded (Sep 17 2026, per Keith: "any reason for it, maybe
        hide it when not in use") - Asset Workshop's real job is the
        checker + Master IDE, texture editing is secondary, so this
        panel should stay out of the way until there's something in
        it."""
        if hasattr(self, '_tex_dock'):
            self._tex_dock.setVisible(bool(self.texture_list))

    def _show_texture_context_menu(self, position): #vers 2
        """Show context menu for texture operations - simplified"""
        if not self.selected_texture:
            return

        has_alpha = self.selected_texture.get('has_alpha', False)

        menu = QMenu(self)

        # Import submenu
        import_menu = menu.addMenu(self.icon_factory.import_icon(color=self._get_icon_color()), "Import")

        import_normal_action = import_menu.addAction("Import Texture")
        import_normal_action.triggered.connect(self._import_normal_texture)

        if has_alpha:
            import_alpha_action = import_menu.addAction("Import Alpha Channel")
            import_alpha_action.triggered.connect(self._import_alpha_texture)

        # Export submenu
        export_menu = menu.addMenu(self.icon_factory.export_icon(color=self._get_icon_color()), "Export")

        export_texture_action = export_menu.addAction("Export Texture")
        export_texture_action.triggered.connect(self.export_selected_texture)

        if has_alpha:
            export_alpha_action = export_menu.addAction("Export Alpha Channel")
            export_alpha_action.triggered.connect(self._export_alpha_only)

        menu.addSeparator()

        # Delete texture
        delete_action = menu.addAction(self.icon_factory.trash_icon(color=self._get_icon_color()), "Delete Texture")
        delete_action.triggered.connect(self._delete_texture)

        menu.exec(self.texture_table.viewport().mapToGlobal(position))

    def _show_txd_info(self): #vers 5
        """Show Workshop information dialog - About and capabilities.
        Rewritten to describe what Asset Workshop actually does now
        (Asset Checker + Master IDE), not the old full TXD editor
        feature set most of which has since been removed."""
        dialog = QDialog(self)
        dialog.setWindowTitle("About " + App_name)
        dialog.setMinimumWidth(500)
        dialog.setMinimumHeight(400)

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

        layout.addWidget(QLabel(""))  # Spacer

        # Capabilities section
        capabilities = QTextEdit()
        capabilities.setReadOnly(True)
        capabilities.setMaximumHeight(280)

        info_text = """<b>✓ Asset Checker:</b><br>
- 4-Column view (ID / IDE / IMG / COL)<br>
- Merged view and Cross-Reference table<br>
- Export full report of missing/extra entries<br>
- Add missing files externally (DFF/COL)<br><br>

<b>✓ Master IDE:</b><br>
- Opens from the View dropdown or ribbon<br>
- Merge, insert, add/remove IDs, reassign ID blocks<br><br>

<b>✓ Texture (via Tools menu):</b><br>
- Import / Export / Rename / Duplicate / Delete<br>
- Undo last change<br><br>

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

    def _update_texture_info(self, texture): #vers 10
        """Update texture display with 4-state view support and checkerboard"""
        if not texture:
            if hasattr(self, 'info_name'):
                self.info_name.setText("")
            if hasattr(self, 'info_alpha_name'):
                self.info_alpha_name.setText("")
                self.info_alpha_name.setVisible(False)
            if hasattr(self, '_info_alpha_name_action'):
                self._info_alpha_name_action.setVisible(False)
            if hasattr(self, 'alpha_label'):
                self.alpha_label.setVisible(False)
            if hasattr(self, '_alpha_label_action'):
                self._alpha_label_action.setVisible(False)
            if hasattr(self, 'preview_widget'):
                self.preview_widget.setText("No texture selected")
            return

        # Set name
        name = texture.get('name', 'Unknown')
        if hasattr(self, 'info_name'):
            self.info_name.setText(name)

        # Set alpha name if has alpha
        has_alpha = texture.get('has_alpha', False)
        if has_alpha:
            alpha_name = texture.get('alpha_name', name + 'a')
            if hasattr(self, 'info_alpha_name'):
                self.info_alpha_name.setText(alpha_name)
                self.info_alpha_name.setVisible(True)
            if hasattr(self, '_info_alpha_name_action'):
                self._info_alpha_name_action.setVisible(True)
            if hasattr(self, 'alpha_label'):
                self.alpha_label.setVisible(True)
            if hasattr(self, '_alpha_label_action'):
                self._alpha_label_action.setVisible(True)
        else:
            if hasattr(self, 'info_alpha_name'):
                self.info_alpha_name.setText("")
                self.info_alpha_name.setVisible(False)
            if hasattr(self, '_info_alpha_name_action'):
                self._info_alpha_name_action.setVisible(False)
            if hasattr(self, 'alpha_label'):
                self.alpha_label.setVisible(False)
            if hasattr(self, '_alpha_label_action'):
                self._alpha_label_action.setVisible(False)

        # Update size info WITH FILE SIZE
        width = texture.get('width', 0)
        height = texture.get('height', 0)
        rgba_data = texture.get('rgba_data', b'')
        file_size_kb = len(rgba_data) / 1024 if rgba_data else 0

        if hasattr(self, 'info_size'):
            self.info_size.setText(f"Size: {width}x{height}, {file_size_kb:.1f}KB")

        # Update format
        fmt = texture.get('format', 'Unknown')
        if hasattr(self, 'format_status_label'):
            self.format_status_label.setText(f"Format: {fmt}")

        # Update bit depth label - derive from format, not raw header byte
        fmt_depth = {
            'ARGB8888': 32, 'RGB888': 24,
            'RGB565': 16, 'ARGB1555': 16, 'ARGB4444': 16, 'RGB555': 16,
            'DXT1': 4, 'DXT2': 8, 'DXT3': 8, 'DXT4': 8, 'DXT5': 8,
            'PAL8': 8, 'PAL4': 4, 'LUM8': 8, 'A8L8': 16,
        }
        fmt_str = texture.get('format', '')
        depth = fmt_depth.get(fmt_str, texture.get('depth', 32))
        if hasattr(self, 'info_bitdepth'):
            self.info_bitdepth.setText(f"[{depth}bit]")

        # Get current view state
        tex_name = texture.get('name', '')
        view_state = self.texture_view_states.get(tex_name, 0)
        self._current_view_state = view_state

        # Update preview based on view state
        if hasattr(self, 'preview_widget') and rgba_data:

            if view_state == 0:  # Normal view
                self._show_normal_view(rgba_data, width, height)

            elif view_state == 1:  # Alpha mask view
                if has_alpha:
                    self._show_alpha_view(rgba_data, width, height)
                else:
                    self.preview_widget.setText("No alpha channel")

            elif view_state == 2:  # Split view (side-by-side)
                if has_alpha:
                    self._show_split_view(rgba_data, width, height)
                else:
                    self.preview_widget.setText("No alpha channel")

            elif view_state == 3:  # Overlay view
                if has_alpha:
                    self._show_overlay_view(rgba_data, width, height)
                else:
                    self.preview_widget.setText("No alpha channel")
