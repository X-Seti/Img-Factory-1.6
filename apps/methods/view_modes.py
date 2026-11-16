#this belongs in apps/methods/view_modes.py - Version: 2
# X-Seti - November 16 2026 - IMG Factory 1.6 - Tree view modes

"""
View modes for a QTreeWidget: Detailed (as built), Icons (grid, names under)
and Compact (one tight line per item). Clicks, double-clicks and right-clicks
in the icon grid are forwarded to the tree so the tool's own handlers run.
"""

##Methods list -

##class ViewModeController: -
# __init__
# _collect
# _forward_context
# _grid_icon
# _refresh_icons
# _schedule_refresh
# _set_compact
# buttons
# mode
# set_mode

from PyQt6.QtCore import QObject, QPoint, QSize, Qt, QTimer
from PyQt6.QtGui import QIcon
from PyQt6.QtWidgets import QListView, QListWidget, QListWidgetItem, QToolButton

MODES = ('detailed', 'icons', 'compact')
_TIPS = {'detailed': "Detailed list", 'icons': "Icons with names", 'compact': "Compact list"}


class ViewModeController(QObject): #vers 1
    """Switches one QTreeWidget between detailed, icon-grid and compact views."""

    def __init__(self, tree, key: str, icon_color=None, parent=None): #vers 1
        super().__init__(parent or tree)
        self.tree = tree
        self.key = key
        self._mode = 'detailed'
        self._saved_cols = None
        self._saved_style = None
        self._icon_color = icon_color
        self._timer = QTimer(self)
        self._timer.setSingleShot(True)
        self._timer.timeout.connect(self._refresh_icons)

        self.grid = QListWidget(tree.parentWidget())
        self.grid.setViewMode(QListView.ViewMode.IconMode)
        self.grid.setResizeMode(QListView.ResizeMode.Adjust)
        self.grid.setMovement(QListView.Movement.Static)
        self.grid.setWordWrap(True)
        self.grid.setUniformItemSizes(True)
        self.grid.setIconSize(QSize(40, 40))
        self.grid.setGridSize(QSize(96, 80))
        self.grid.setSpacing(2)
        self.grid.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.grid.itemClicked.connect(lambda li: self._forward(li, False))
        self.grid.itemDoubleClicked.connect(lambda li: self._forward(li, True))
        self.grid.customContextMenuRequested.connect(self._forward_context)
        self.grid.hide()
        lay = tree.parentWidget().layout()
        lay.insertWidget(lay.indexOf(tree) + 1, self.grid)
        lay.setStretchFactor(self.grid, lay.stretch(lay.indexOf(tree)) or 1)

        model = tree.model()
        for sig in (model.rowsInserted, model.rowsRemoved, model.modelReset, model.layoutChanged):
            sig.connect(self._schedule_refresh)

        self._btns = {}
        self._load_saved()

    def _load_saved(self): #vers 1
        """Apply the remembered mode."""
        try:
            from apps.methods.img_factory_settings import get_img_factory_qsettings
            m = get_img_factory_qsettings().value(f"view_mode/{self.key}", 'detailed', type=str)
        except Exception:
            m = 'detailed'
        self.set_mode(m if m in MODES else 'detailed', save=False)

    def buttons(self) -> list: #vers 1
        """Three exclusive tool buttons: Detailed, Icons, Compact."""
        from apps.methods.imgfactory_svg_icons import SVGIconFactory as S, list_icon, grid_icon
        fns = {'detailed': list_icon, 'icons': grid_icon, 'compact': S.compact_list_icon}
        out = []
        for m in MODES:
            b = QToolButton()
            b.setIcon(fns[m](18, self._icon_color))
            b.setToolTip(_TIPS[m])
            b.setCheckable(True)
            b.setAutoRaise(True)
            b.setChecked(m == self._mode)
            b.clicked.connect(lambda _c, mm=m: self.set_mode(mm))
            self._btns[m] = b
            out.append(b)
        return out

    def mode(self) -> str: #vers 1
        """Current mode name."""
        return self._mode

    def set_mode(self, mode: str, save: bool = True): #vers 1
        """Switch view mode."""
        if mode not in MODES:
            return
        self._set_compact(mode == 'compact')
        self.tree.setVisible(mode != 'icons')
        self.grid.setVisible(mode == 'icons')
        self._mode = mode
        for m, b in self._btns.items():
            b.setChecked(m == mode)
        if mode == 'icons':
            self._refresh_icons()
        if save:
            try:
                from apps.methods.img_factory_settings import get_img_factory_qsettings
                get_img_factory_qsettings().setValue(f"view_mode/{self.key}", mode)
            except Exception:
                pass

    def _set_compact(self, on: bool): #vers 1
        """Compact: name column only, small icons, tight rows."""
        t = self.tree
        if on and self._saved_cols is None:
            self._saved_cols = [t.isColumnHidden(c) for c in range(t.columnCount())]
            self._saved_style = (t.styleSheet(), t.iconSize(), t.indentation())
            for c in range(1, t.columnCount()):
                t.setColumnHidden(c, True)
            t.setIconSize(QSize(14, 14))
            t.setIndentation(12)
            t.setStyleSheet(t.styleSheet() + " QTreeView::item { padding: 0px; }")
        elif not on and self._saved_cols is not None:
            for c, h in enumerate(self._saved_cols[:t.columnCount()]):
                t.setColumnHidden(c, h)
            sheet, isz, ind = self._saved_style
            t.setStyleSheet(sheet)
            t.setIconSize(isz)
            t.setIndentation(ind)
            self._saved_cols = None

    def _collect(self) -> list: #vers 1
        """Items to show: one root's children, else the top-level items."""
        root = self.tree.invisibleRootItem()
        if root.childCount() == 1 and root.child(0).childCount() > 0:
            root = root.child(0)
        return [root.child(i) for i in range(root.childCount())
                if not root.child(i).isHidden()]

    def _schedule_refresh(self, *_a): #vers 1
        """Rebuild the grid shortly after the tree settles."""
        if self._mode == 'icons':
            self._timer.start(80)

    def recolour(self, color): #vers 1
        """Theme change: buttons and grid icons take the new colour."""
        from apps.methods.imgfactory_svg_icons import SVGIconFactory as S, list_icon, grid_icon
        self._icon_color = color
        fns = {'detailed': list_icon, 'icons': grid_icon, 'compact': S.compact_list_icon}
        for m, b in self._btns.items():
            b.setIcon(fns[m](18, color))
        self._schedule_refresh()

    def _refresh_icons(self): #vers 1
        """Rebuild the icon grid from the tree."""
        self.grid.clear()
        for it in self._collect():
            ico = self._grid_icon(it)
            li = QListWidgetItem(ico, it.text(0))
            li.setToolTip(" | ".join(x for x in (it.text(c) for c in range(self.tree.columnCount())) if x))
            li.setData(Qt.ItemDataRole.UserRole + 1, it)
            self.grid.addItem(li)

    def _grid_icon(self, it) -> QIcon: #vers 1
        """Folder icon for folders, else a file page showing the extension letters."""
        import os
        from apps.methods.imgfactory_svg_icons import SVGIconFactory as S
        path = it.data(0, Qt.ItemDataRole.UserRole)
        if isinstance(path, str) and path:
            if os.path.isdir(path):
                return S.folder_icon(40, self._icon_color)
            ext = os.path.splitext(path)[1]
        else:
            ext = os.path.splitext(it.text(0).split('[')[0].strip())[1]
            if not ext and it.childCount():
                return S.folder_icon(40, self._icon_color)
            ext = ext or (it.text(1) if it.columnCount() > 1 else '')
        return S.ext_file_icon(ext, 48, self._icon_color)

    def _forward(self, li, double: bool): #vers 1
        """Run the tree's own click / double-click handler for this item."""
        it = li.data(Qt.ItemDataRole.UserRole + 1)
        if it is None:
            return
        self.tree.setCurrentItem(it)
        (self.tree.itemDoubleClicked if double else self.tree.itemClicked).emit(it, 0)

    def _forward_context(self, pos: QPoint): #vers 1
        """Right-click on a grid item: raise the tree's own context menu."""
        li = self.grid.itemAt(pos)
        if li is None:
            return
        it = li.data(Qt.ItemDataRole.UserRole + 1)
        par = it.parent()
        while par is not None:
            par.setExpanded(True)
            par = par.parent()
        self.tree.setCurrentItem(it)
        self.tree.scrollToItem(it)
        rect = self.tree.visualItemRect(it)
        self.tree.customContextMenuRequested.emit(rect.center())
