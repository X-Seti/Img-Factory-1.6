#!/usr/bin/env python3
# apps/components/Ipl_Editor/ipl_workshop.py - Version: 1
# X-Seti - Apr 2026 - IMG Factory 1.6 - IPL Workshop
# Item Placement List editor for GTA III / VC / SA / SOL
# Built on GUIWorkshop base (temp_workshop pattern)
# Section 1: IPL parser / writer
# Section 2: IPL Workshop UI (GUIWorkshop subclass)
# Section 3: IPL logic - open/save/edit/search/filter

import sys, os, re
from pathlib import Path
from typing import List, Optional

from PyQt6.QtWidgets import (
    QApplication, QWidget, QVBoxLayout, QHBoxLayout, QFrame,
    QLabel, QToolButton, QPushButton, QListWidget, QListWidgetItem,
    QFileDialog, QMessageBox, QTabWidget, QScrollArea, QSizePolicy,
    QDialog, QFormLayout, QDialogButtonBox, QDoubleSpinBox, QMenu,
    QSplitter, QTableWidget, QTableWidgetItem, QHeaderView,
    QLineEdit, QComboBox, QCheckBox, QAbstractItemView
)
from PyQt6.QtGui import (
    QColor, QPainter, QFont, QIcon, QKeySequence, QShortcut
)
from PyQt6.QtCore import Qt, QSize, pyqtSignal, QSortFilterProxyModel

# ── GUIWorkshop base ──────────────────────────────────────────────────────────
def _find_gui_workshop():
    _dep = Path(__file__).parent.parent / "Tmp_Template" / "gui_workshop.py"
    if _dep.exists():
        import importlib.util as _u
        _s = _u.spec_from_file_location("gui_workshop", _dep)
        _m = _u.module_from_spec(_s); _s.loader.exec_module(_m)
        return _m.GUIWorkshop
    try:
        from apps.components.Tmp_Template.gui_workshop import GUIWorkshop as _G
        return _G
    except ImportError:
        sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))
        from apps.components.Tmp_Template.gui_workshop import GUIWorkshop as _G
        return _G

GUIWorkshop = _find_gui_workshop()

try:
    from apps.methods.imgfactory_svg_icons import SVGIconFactory
except ImportError:
    class SVGIconFactory:
        @staticmethod
        def _s(sz=20, c=None): return QIcon()
        open_icon = save_icon = export_icon = import_icon = undo_icon = \
        search_icon = locate_icon = edit_icon = remove_icon = \
        add_icon = info_icon = staticmethod(_s)

App_name = "IPL Workshop"
Build    = "Build 1"


# =============================================================================
# SECTION 1 — IPL parser / writer
# =============================================================================

class IPLEntry:
    """One instance line from an IPL file."""
    __slots__ = ('model_id','model_name','interior',
                 'px','py','pz','rx','ry','rz','rw','lod','source_line')

    def __init__(self, model_id=0, model_name="", interior=0,
                 px=0.0, py=0.0, pz=0.0,
                 rx=0.0, ry=0.0, rz=0.0, rw=1.0,
                 lod=-1, source_line=""):
        self.model_id   = model_id
        self.model_name = model_name
        self.interior   = interior
        self.px = px;  self.py = py;  self.pz = pz
        self.rx = rx;  self.ry = ry;  self.rz = rz;  self.rw = rw
        self.lod        = lod
        self.source_line = source_line

    def to_gta3_line(self) -> str:
        """GTA III / VC: id, model, px,py,pz, sx,sy,sz(=1), rx,ry,rz,rw"""
        return (f"{self.model_id}, {self.model_name}, "
                f"{self.px:.6f}, {self.py:.6f}, {self.pz:.6f}, "
                f"1.0, 1.0, 1.0, "
                f"{self.rx:.6f}, {self.ry:.6f}, {self.rz:.6f}, {self.rw:.6f}")

    def to_sa_line(self) -> str:
        """SA: id, model, interior, px,py,pz, rx,ry,rz,rw [,lod]"""
        base = (f"{self.model_id}, {self.model_name}, {self.interior}, "
                f"{self.px:.6f}, {self.py:.6f}, {self.pz:.6f}, "
                f"{self.rx:.6f}, {self.ry:.6f}, {self.rz:.6f}, {self.rw:.6f}")
        return base if self.lod < 0 else base + f", {self.lod}"


class IPLSection:
    """A named section (inst/zone/cull/cars/grge/enex/pick/path/mult/occl)."""
    def __init__(self, name: str):
        self.name  = name
        self.lines: List[str] = []   # raw lines (non-inst sections kept verbatim)
        self.entries: List[IPLEntry] = []  # parsed inst entries

    def is_inst(self): return self.name == "inst"


class IPLFile:
    """Reads and writes GTA .ipl files, preserving all sections and comments."""

    # Detect SA by checking if inst lines have 10-11 comma-separated fields
    KNOWN_SECTIONS = {"inst","zone","cull","cars","grge","enex","pick",
                      "path","mult","occl","auzo","nplp","slip","tunnel"}

    def __init__(self):
        self.sections:  List[IPLSection] = []
        self.header_lines: List[str]     = []   # comments before first section
        self.game       = "auto"   # "gta3", "vc", "sa", or "auto"
        self.path       = ""
        self._dirty     = False

    # ── Detection ─────────────────────────────────────────────────────────────
    def _detect_game(self, raw_inst_lines: List[str]) -> str:
        """Detect GTA version from field count of inst lines."""
        for line in raw_inst_lines[:20]:
            parts = [p.strip() for p in line.split(",")]
            if len(parts) >= 10:
                return "sa"
            if len(parts) == 12:
                return "gta3"
        return "gta3"

    # ── Load ──────────────────────────────────────────────────────────────────
    def load(self, path: str) -> bool:
        self.path = path
        self.sections = []
        self.header_lines = []
        self._dirty = False

        try:
            text = Path(path).read_text(encoding="latin1", errors="replace")
        except Exception as e:
            raise IOError(f"Cannot read {path}: {e}")

        lines = text.splitlines()
        current: Optional[IPLSection] = None
        raw_inst: List[str] = []

        for raw in lines:
            stripped = raw.strip()
            low = stripped.lower()

            # End of section
            if low == "end":
                if current:
                    self.sections.append(current)
                    current = None
                continue

            # Section header
            if low in self.KNOWN_SECTIONS and "," not in stripped:
                current = IPLSection(low)
                continue

            # Before first section
            if current is None:
                self.header_lines.append(raw)
                continue

            # Inside a section
            if current.is_inst() and stripped and not stripped.startswith("#"):
                raw_inst.append(stripped)
                entry = self._parse_inst_line(stripped)
                if entry:
                    current.entries.append(entry)
            else:
                current.lines.append(raw)

        # Detect game
        if self.game == "auto":
            self.game = self._detect_game(raw_inst)

        return True

    def _parse_inst_line(self, line: str) -> Optional[IPLEntry]:
        # Strip inline comments
        line = line.split("#")[0].strip()
        if not line:
            return None
        parts = [p.strip() for p in line.split(",")]
        try:
            n = len(parts)
            if n >= 12:  # GTA III/VC: id, model, px,py,pz, sx,sy,sz, rx,ry,rz,rw
                return IPLEntry(
                    model_id=int(parts[0]), model_name=parts[1],
                    px=float(parts[2]), py=float(parts[3]), pz=float(parts[4]),
                    # parts[5-7] = scale (ignored, kept as 1.0)
                    rx=float(parts[8]), ry=float(parts[9]),
                    rz=float(parts[10]), rw=float(parts[11]),
                    source_line=line)
            elif n >= 10:  # SA: id, model, interior, px,py,pz, rx,ry,rz,rw [,lod]
                lod = int(parts[10]) if n >= 11 else -1
                return IPLEntry(
                    model_id=int(parts[0]), model_name=parts[1],
                    interior=int(parts[2]),
                    px=float(parts[3]), py=float(parts[4]), pz=float(parts[5]),
                    rx=float(parts[6]), ry=float(parts[7]),
                    rz=float(parts[8]), rw=float(parts[9]),
                    lod=lod, source_line=line)
        except (ValueError, IndexError):
            pass
        return None

    # ── Save ──────────────────────────────────────────────────────────────────
    def save(self, path: str = ""):
        out_path = path or self.path
        lines = list(self.header_lines)
        for sec in self.sections:
            lines.append(sec.name)
            if sec.is_inst():
                for e in sec.entries:
                    lines.append(e.to_sa_line() if self.game == "sa"
                                 else e.to_gta3_line())
            else:
                lines.extend(sec.lines)
            lines.append("end")
            lines.append("")
        Path(out_path).write_text("\n".join(lines), encoding="latin1")
        self._dirty = False
        if path:
            self.path = path

    # ── Convenience ───────────────────────────────────────────────────────────
    @property
    def instances(self) -> List[IPLEntry]:
        for sec in self.sections:
            if sec.is_inst():
                return sec.entries
        return []

    @property
    def inst_section(self) -> Optional[IPLSection]:
        for sec in self.sections:
            if sec.is_inst():
                return sec
        return None

    def section_names(self) -> List[str]:
        return [s.name for s in self.sections]


# =============================================================================
# SECTION 2 — IPL Workshop (GUIWorkshop subclass)
# =============================================================================

class IPLWorkshop(GUIWorkshop):
    """GTA Item Placement List editor — docks in IMG Factory or runs standalone."""

    App_name        = "IPL Workshop"
    App_build       = Build
    App_author      = "X-Seti"
    App_year        = "2026"
    App_description = ("GTA III / VC / SA / SOL — .ipl item placement files\n"
                       "Edit object instances: position, rotation, model ID\n"
                       "Supports inst / zone / cull / cars and all other sections")
    config_key      = "ipl_workshop"

    # Column indices for the instance table
    COL_ID    = 0
    COL_MODEL = 1
    COL_INT   = 2
    COL_PX    = 3
    COL_PY    = 4
    COL_PZ    = 5
    COL_RX    = 6
    COL_RY    = 7
    COL_RZ    = 8
    COL_RW    = 9
    COL_LOD   = 10
    NUM_COLS  = 11

    COL_HEADERS = ["ID", "Model", "Int", "X", "Y", "Z",
                   "Rot X", "Rot Y", "Rot Z", "Rot W", "LOD"]

    def __init__(self, parent=None, main_window=None):
        self._ipl         = None      # IPLFile
        self._file_path   = ""
        self._undo_stack  = []
        self._redo_stack  = []
        self._active_section = "inst"
        super().__init__(parent, main_window)
        try:
            from apps.methods.imgfactory_svg_icons import SVGIconFactory as _S
            self.setWindowIcon(_S.ipl_editor_icon(64) if hasattr(_S,'ipl_editor_icon')
                               else _S.locate_icon(64))
        except Exception:
            pass

    # ── Menu ──────────────────────────────────────────────────────────────────
    def _build_menus_into_qmenu(self, pm):
        fm = pm.addMenu("File")
        fm.addAction("Open IPL…  Ctrl+O",     self._open_file)
        fm.addAction("Load all IPLs from DAT…", self._load_all_from_dat)
        fm.addAction("Save       Ctrl+S",      self._save_file)
        fm.addAction("Save As…",               self._save_as)
        fm.addSeparator()
        fm.addAction("Export CSV…",            self._export_csv)
        fm.addSeparator()
        recent = self.WS.get_recent()
        if recent:
            rm = fm.addMenu("Recent Files")
            for rp in recent:
                act = rm.addAction(Path(rp).name)
                act.triggered.connect(lambda c=False, p=rp: self._open_file(p))
            rm.addSeparator()
            rm.addAction("Clear Recent", self._clear_recent)

        em = pm.addMenu("Edit")
        em.addAction("Undo  Ctrl+Z",           self._undo)
        em.addAction("Redo  Ctrl+Y",           self._redo)
        em.addSeparator()
        em.addAction("Add Entry",              self._add_entry)
        em.addAction("Delete Selected",        self._delete_selected)
        em.addAction("Duplicate Selected",     self._duplicate_selected)
        em.addSeparator()
        em.addAction("Select All  Ctrl+A",     self._select_all)
        em.addAction("Find…  Ctrl+F",          self._show_find)

        vm = pm.addMenu("View")
        vm.addAction("Fit Columns",            self._fit_columns)
        vm.addAction("Filter by Interior…",    self._filter_interior)
        vm.addAction("Statistics",             self._show_stats)
        vm.addSeparator()
        vm.addAction("About IPL Workshop",     self._show_about)

    # ── Left panel — section list ─────────────────────────────────────────────
    def _create_left_panel(self):
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        ll = QVBoxLayout(panel)
        ll.setContentsMargins(*self.get_panel_margins())

        # Docked file buttons
        if not self.standalone_mode:
            ic = self._get_icon_color()
            br = QHBoxLayout(); br.setSpacing(2)
            def _pb(icon_fn, tip, slot):
                b = QPushButton()
                try:
                    b.setIcon(getattr(SVGIconFactory, icon_fn)(16, ic))
                    b.setIconSize(QSize(16, 16))
                except Exception:
                    pass
                b.setToolTip(tip); b.setFixedHeight(26)
                b.clicked.connect(slot); br.addWidget(b); return b
            _pb("open_icon",   "Open IPL (Ctrl+O)",  self._open_file)
            _pb("import_icon", "Load all IPLs from DAT…", self._load_all_from_dat)
            _pb("save_icon",   "Save (Ctrl+S)",       self._save_file)
            _pb("export_icon", "Export CSV",          self._export_csv)
            ll.addLayout(br)
            sep = QFrame(); sep.setFrameShape(QFrame.Shape.HLine)
            ll.addWidget(sep)

        hdr = QLabel("Sections")
        hdr.setFont(self.panel_font)
        hdr.setStyleSheet("font-weight:bold; padding:2px;")
        hdr.setAlignment(Qt.AlignmentFlag.AlignCenter)
        ll.addWidget(hdr)

        self._section_list = QListWidget()
        self._section_list.setAlternatingRowColors(True)
        self._section_list.currentRowChanged.connect(self._on_section_changed)
        ll.addWidget(self._section_list)

        sep2 = QFrame(); sep2.setFrameShape(QFrame.Shape.HLine)
        ll.addWidget(sep2)

        self._info_lbl = QLabel("No file loaded")
        self._info_lbl.setFont(self.infobar_font)
        self._info_lbl.setWordWrap(True)
        self._info_lbl.setStyleSheet("padding:2px; color:#aaa;")
        ll.addWidget(self._info_lbl)

        self._dirty_lbl = QLabel("Modified: no")
        self._dirty_lbl.setFont(self.infobar_font)
        ll.addWidget(self._dirty_lbl)
        return panel

    # ── Centre panel — instance table + search ────────────────────────────────
    def _create_centre_panel(self): #vers 2
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        cl = QVBoxLayout(panel)
        cl.setContentsMargins(0, 0, 0, 0)
        cl.setSpacing(0)

        # Tabs: Table editor | World Map
        self._centre_tabs = QTabWidget()
        self._centre_tabs.setTabPosition(QTabWidget.TabPosition.North)

        # ── Tab 1: Table editor ───────────────────────────────────────────
        table_widget = QFrame()
        tl = QVBoxLayout(table_widget)
        tl.setContentsMargins(0, 2, 0, 0)
        tl.setSpacing(2)

        sbar = QHBoxLayout(); sbar.setSpacing(4); sbar.setContentsMargins(4,2,4,2)
        self._search_box = QLineEdit()
        self._search_box.setPlaceholderText("Search model name…")
        self._search_box.textChanged.connect(self._on_search_changed)
        self._search_box.setFixedHeight(24)
        sbar.addWidget(QLabel("🔍")); sbar.addWidget(self._search_box)

        self._int_filter = QComboBox()
        self._int_filter.addItem("All interiors")
        self._int_filter.setFixedWidth(120)
        self._int_filter.currentIndexChanged.connect(self._on_filter_changed)
        sbar.addWidget(self._int_filter)

        clr = QPushButton("✕")
        clr.setFixedSize(24, 24); clr.setToolTip("Clear search")
        clr.clicked.connect(self._clear_search)
        sbar.addWidget(clr)
        tl.addLayout(sbar)

        self._table = QTableWidget(0, self.NUM_COLS)
        self._table.setHorizontalHeaderLabels(self.COL_HEADERS)
        self._table.setAlternatingRowColors(True)
        self._table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        self._table.setEditTriggers(QAbstractItemView.EditTrigger.DoubleClicked |
                                    QAbstractItemView.EditTrigger.SelectedClicked)
        self._table.setSortingEnabled(True)
        self._table.horizontalHeader().setStretchLastSection(False)
        self._table.horizontalHeader().setSectionResizeMode(
            self.COL_MODEL, QHeaderView.ResizeMode.Stretch)
        self._table.itemChanged.connect(self._on_cell_edited)
        self._table.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self._table.customContextMenuRequested.connect(self._show_context_menu)
        tl.addWidget(self._table)

        from PyQt6.QtWidgets import QTextEdit
        self._text_view = QTextEdit()
        self._text_view.setReadOnly(False)
        self._text_view.setFont(QFont("Courier New", 9))
        self._text_view.setPlaceholderText("Section content appears here…")
        self._text_view.setVisible(False)
        self._text_view.textChanged.connect(self._on_text_edited)
        tl.addWidget(self._text_view)

        self._centre_tabs.addTab(table_widget, "📋  Table")

        # ── Tab 2: World Map ──────────────────────────────────────────────
        self._map_panel = IPLMapPanel(self)
        self._centre_tabs.addTab(self._map_panel, "🗺  World Map")

        # Switch to map → refresh with current entries
        self._centre_tabs.currentChanged.connect(self._on_centre_tab_changed)

        cl.addWidget(self._centre_tabs)
        return panel

    def _on_centre_tab_changed(self, idx: int): #vers 2
        """Refresh map (and path overlay) when switching to the World Map tab."""
        if idx == 1:  # World Map tab
            entries = self._current_entries()
            self._map_panel.refresh(entries)
            # Load path nodes from all path sections
            if self._ipl:
                path_secs = [s for s in self._ipl.sections if s.name == 'path']
                self._map_panel._map.load_path_nodes(path_secs)

    def _current_entries(self) -> list: #vers 1
        """Return the currently visible inst entries (from active section)."""
        if not self._ipl:
            return []
        sec_name = getattr(self, '_active_section', 'inst')
        sec = next((s for s in self._ipl.sections if s.name == sec_name), None)
        if sec and sec.is_inst():
            return list(sec.entries)
        # Fallback: all inst entries
        return list(self._ipl.instances)

    # ── Right sidebar ─────────────────────────────────────────────────────────
    def _populate_sidebar(self):
        sl  = self._sidebar_layout
        ic  = self._get_icon_color()
        BTN = 36

        def _nb(icon_fn, tip, slot):
            b = QToolButton(); b.setFixedSize(BTN, BTN)
            try: b.setIcon(getattr(SVGIconFactory, icon_fn)(20, ic))
            except Exception: b.setText(tip[:2])
            b.setToolTip(tip); b.clicked.connect(slot); return b

        def _row(*btns):
            row = QHBoxLayout(); row.setSpacing(2); row.setContentsMargins(0,0,0,0)
            for b in btns: row.addWidget(b)
            if len(btns) == 1: row.addStretch()
            sl.addLayout(row)

        def _sep():
            s = QFrame(); s.setFrameShape(QFrame.Shape.HLine)
            sl.addSpacing(2); sl.addWidget(s); sl.addSpacing(2)

        _row(_nb("add_icon",     "Add entry",          self._add_entry),
             _nb("remove_icon",  "Delete selected",     self._delete_selected))
        _row(_nb("edit_icon",    "Duplicate selected",  self._duplicate_selected),
             _nb("undo_icon",    "Undo (Ctrl+Z)",       self._undo))
        _sep()
        _row(_nb("search_icon",  "Find model (Ctrl+F)", self._show_find),
             _nb("locate_icon",  "Select all (Ctrl+A)", self._select_all))
        _row(_nb("info_icon",    "Statistics",          self._show_stats),
             _nb("export_icon",  "Export CSV",          self._export_csv))
        _row(_nb("import_icon",  "Load all IPLs from DAT…", self._load_all_from_dat))
        _sep()
        sl.addWidget(QLabel("IPL Workshop", alignment=Qt.AlignmentFlag.AlignCenter))


# =============================================================================
# SECTION 3 — IPL logic
# =============================================================================

    # ── File ops ──────────────────────────────────────────────────────────────
    def _open_file(self, path=None):
        if not path:
            path, _ = QFileDialog.getOpenFileName(
                self, "Open IPL File", "",
                "IPL Files (*.ipl *.IPL);;All Files (*)")
        if not path:
            return
        try:
            ipl = IPLFile()
            ipl.load(path)
            self._ipl = ipl
            self._file_path = path
            self.WS.add_recent(path)
            self._populate_section_list()
            self._select_inst_section()
            self._update_info()
            self._dirty_lbl.setText("Modified: no")
            self.save_btn.setEnabled(True)
            self._set_status(f"Loaded {Path(path).name}  |  {len(ipl.instances)} instances"
                             f"  |  game={ipl.game}  |  sections: {', '.join(ipl.section_names())}")
        except Exception as e:
            QMessageBox.critical(self, "Load Error", f"Failed to load {Path(path).name}:\n{e}")

    def _save_file(self):
        if not self._ipl:
            self._set_status("No IPL loaded"); return
        if not self._file_path:
            self._save_as(); return
        try:
            self._ipl.save()
            self._dirty_lbl.setText("Modified: no")
            self.save_btn.setEnabled(False)
            self._set_status(f"Saved {Path(self._file_path).name}")
        except Exception as e:
            QMessageBox.critical(self, "Save Error", str(e))

    def _save_as(self):
        if not self._ipl:
            self._set_status("No IPL loaded"); return
        p, _ = QFileDialog.getSaveFileName(
            self, "Save IPL As", self._file_path or "",
            "IPL Files (*.ipl);;All Files (*)")
        if p:
            try:
                self._ipl.save(p)
                self._file_path = p
                self._dirty_lbl.setText("Modified: no")
                self.save_btn.setEnabled(False)
                self._set_status(f"Saved as {Path(p).name}")
            except Exception as e:
                QMessageBox.critical(self, "Save Error", str(e))

    def _export_file(self): self._export_csv()
    def _import_file(self): self._open_file()

    def _export_csv(self):
        if not self._ipl:
            QMessageBox.information(self, "Export", "Load an IPL file first.")
            return
        p, _ = QFileDialog.getSaveFileName(
            self, "Export CSV", Path(self._file_path).stem + "_inst.csv",
            "CSV Files (*.csv)")
        if not p: return
        try:
            rows = ["id,model,interior,x,y,z,rx,ry,rz,rw,lod"]
            for e in self._ipl.instances:
                rows.append(f"{e.model_id},{e.model_name},{e.interior},"
                            f"{e.px:.6f},{e.py:.6f},{e.pz:.6f},"
                            f"{e.rx:.6f},{e.ry:.6f},{e.rz:.6f},{e.rw:.6f},{e.lod}")
            Path(p).write_text("\n".join(rows))
            self._set_status(f"Exported {len(self._ipl.instances)} instances to {Path(p).name}")
        except Exception as e:
            QMessageBox.critical(self, "Export Error", str(e))

    def _clear_recent(self):
        self.WS._data["recent_files"] = []; self.WS.save()
        self._set_status("Recent cleared")

    # ── Section management ────────────────────────────────────────────────────
    def _populate_section_list(self):
        self._section_list.clear()
        if not self._ipl: return
        for sec in self._ipl.sections:
            n = len(sec.entries) if sec.is_inst() else len(sec.lines)
            item = QListWidgetItem(f"{sec.name}  ({n})")
            item.setData(Qt.ItemDataRole.UserRole, sec.name)
            if sec.is_inst():
                item.setForeground(QColor("#4a9fd4"))
            self._section_list.addItem(item)

    def _select_inst_section(self):
        for i in range(self._section_list.count()):
            item = self._section_list.item(i)
            if item.data(Qt.ItemDataRole.UserRole) == "inst":
                self._section_list.setCurrentRow(i)
                return
        if self._section_list.count():
            self._section_list.setCurrentRow(0)

    def _on_section_changed(self, row: int):
        if not self._ipl or row < 0: return
        item = self._section_list.item(row)
        if not item: return
        sec_name = item.data(Qt.ItemDataRole.UserRole)
        self._active_section = sec_name

        sec = next((s for s in self._ipl.sections if s.name == sec_name), None)
        if not sec: return

        if sec.is_inst():
            self._text_view.setVisible(False)
            self._table.setVisible(True)
            self._populate_table(sec.entries)
            self._update_int_filter(sec.entries)
        else:
            self._table.setVisible(False)
            self._text_view.setVisible(True)
            self._text_view.blockSignals(True)
            self._text_view.setPlainText("\n".join(sec.lines))
            self._text_view.blockSignals(False)
            self._set_status(f"Section '{sec_name}' — {len(sec.lines)} lines (raw text)")

    # ── Table population ──────────────────────────────────────────────────────
    def _populate_table(self, entries: List[IPLEntry]):
        self._table.blockSignals(True)
        self._table.setSortingEnabled(False)
        self._table.setRowCount(len(entries))

        def _num(v) -> QTableWidgetItem:
            item = QTableWidgetItem(f"{v:.6f}" if isinstance(v, float) else str(v))
            item.setTextAlignment(Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter)
            return item

        for row, e in enumerate(entries):
            self._table.setItem(row, self.COL_ID,    QTableWidgetItem(str(e.model_id)))
            self._table.setItem(row, self.COL_MODEL,  QTableWidgetItem(e.model_name))
            self._table.setItem(row, self.COL_INT,    _num(e.interior))
            self._table.setItem(row, self.COL_PX,     _num(e.px))
            self._table.setItem(row, self.COL_PY,     _num(e.py))
            self._table.setItem(row, self.COL_PZ,     _num(e.pz))
            self._table.setItem(row, self.COL_RX,     _num(e.rx))
            self._table.setItem(row, self.COL_RY,     _num(e.ry))
            self._table.setItem(row, self.COL_RZ,     _num(e.rz))
            self._table.setItem(row, self.COL_RW,     _num(e.rw))
            lod_item = _num(e.lod)
            if e.lod < 0:
                lod_item.setForeground(QColor("#666"))
            self._table.setItem(row, self.COL_LOD,    lod_item)
            self._table.setRowHeight(row, 20)

        self._table.setSortingEnabled(True)
        self._table.blockSignals(False)
        self._set_status(f"Section 'inst' — {len(entries)} instances")

    def _update_int_filter(self, entries: List[IPLEntry]):
        interiors = sorted(set(e.interior for e in entries))
        self._int_filter.blockSignals(True)
        self._int_filter.clear()
        self._int_filter.addItem("All interiors")
        for i in interiors:
            self._int_filter.addItem(f"Interior {i}", i)
        self._int_filter.blockSignals(False)

    # ── Search / filter ───────────────────────────────────────────────────────
    def _on_search_changed(self, text: str):
        self._apply_filter(text, self._int_filter.currentData())

    def _on_filter_changed(self):
        self._apply_filter(self._search_box.text(),
                           self._int_filter.currentData())

    def _apply_filter(self, text: str, interior):
        text = text.lower()
        for row in range(self._table.rowCount()):
            model_item = self._table.item(row, self.COL_MODEL)
            int_item   = self._table.item(row, self.COL_INT)
            model_match = not text or (model_item and text in model_item.text().lower())
            int_match   = interior is None or (
                int_item and int(int_item.text() or "0") == interior)
            self._table.setRowHidden(row, not (model_match and int_match))

    def _clear_search(self):
        self._search_box.clear()
        self._int_filter.setCurrentIndex(0)
        for row in range(self._table.rowCount()):
            self._table.setRowHidden(row, False)

    # ── Editing ───────────────────────────────────────────────────────────────
    def _on_cell_edited(self, item: QTableWidgetItem):
        if not self._ipl: return
        sec = self._current_inst_section()
        if not sec: return
        row = item.row()
        if row >= len(sec.entries): return
        e = sec.entries[row]
        col = item.column()
        try:
            val = item.text().strip()
            if   col == self.COL_ID:    e.model_id   = int(val)
            elif col == self.COL_MODEL: e.model_name = val
            elif col == self.COL_INT:   e.interior   = int(val)
            elif col == self.COL_PX:    e.px = float(val)
            elif col == self.COL_PY:    e.py = float(val)
            elif col == self.COL_PZ:    e.pz = float(val)
            elif col == self.COL_RX:    e.rx = float(val)
            elif col == self.COL_RY:    e.ry = float(val)
            elif col == self.COL_RZ:    e.rz = float(val)
            elif col == self.COL_RW:    e.rw = float(val)
            elif col == self.COL_LOD:   e.lod = int(val)
            self._mark_dirty()
        except ValueError:
            pass  # bad input — leave as-is

    def _on_text_edited(self):
        if not self._ipl: return
        sec = next((s for s in self._ipl.sections
                    if s.name == self._active_section), None)
        if sec and not sec.is_inst():
            sec.lines = self._text_view.toPlainText().splitlines()
            self._mark_dirty()

    def _mark_dirty(self):
        self._dirty_lbl.setText("Modified: yes")
        self.save_btn.setEnabled(True)

    def _current_inst_section(self) -> Optional[IPLSection]:
        if not self._ipl: return None
        return self._ipl.inst_section

    # ── Load all from DAT ────────────────────────────────────────────────────
    def _load_all_from_dat(self):
        """Collect all IPL paths from a .dat file and load them as merged IPLFile."""
        dat_path = self._pick_dat_path()
        if not dat_path:
            return
        try:
            self._do_load_all_from_dat(dat_path)
        except Exception as e:
            import traceback
            QMessageBox.critical(self, "Load Error",
                f"Failed to load from DAT:\n{e}\n\n{traceback.format_exc()[-500:]}")

    def _pick_dat_path(self) -> str:
        """Return a .dat path — from DAT Browser if open, else file dialog."""
        # Try DAT Browser's loaded dat first
        mw = self.main_window
        if mw:
            db = getattr(mw, 'dat_browser', None)
            if db and hasattr(db, 'loader'):
                main_dat = getattr(db.loader, 'main_dat', None)
                if main_dat and getattr(main_dat, 'dat_path', ''):
                    p = main_dat.dat_path
                    reply = QMessageBox.question(self, "Load from DAT",
                        f"Use DAT Browser's loaded file?\n\n{p}",
                        QMessageBox.StandardButton.Yes |
                        QMessageBox.StandardButton.No |
                        QMessageBox.StandardButton.Cancel)
                    if reply == QMessageBox.StandardButton.Cancel:
                        return ""
                    if reply == QMessageBox.StandardButton.Yes:
                        return p
        # File dialog
        p, _ = QFileDialog.getOpenFileName(self, "Select DAT File", "",
            "DAT Files (*.dat *.DAT);;All Files (*)")
        return p

    def _do_load_all_from_dat(self, dat_path: str):
        """Parse DAT, collect all IPL entries, merge into one multi-section IPLFile."""
        from apps.methods.gta_dat_parser import DATParser, GTAGame

        # Detect game from DAT filename
        name = Path(dat_path).stem.lower()
        if "gta_sa" in name or "sa" in name:
            game = GTAGame.SA
        elif "vice" in name or "vc" in name:
            game = GTAGame.VC
        else:
            game = GTAGame.GTA3

        game_root = str(Path(dat_path).parent.parent)
        dat = DATParser(game)
        dat.parse(dat_path, game_root)
        ipl_entries = dat.ipl_entries()

        if not ipl_entries:
            QMessageBox.information(self, "No IPL Files",
                f"No IPL directives found in:\n{dat_path}")
            return

        # Show selection dialog
        ipl_entries = [e for e in ipl_entries]  # all
        sel = self._select_ipls_dialog(ipl_entries, dat_path)
        if sel is None:  # cancelled
            return
        if not sel:
            self._set_status("No IPL files selected")
            return

        # Load selected IPLs and merge
        merged_ipl = IPLFile()
        merged_ipl.path  = dat_path
        merged_ipl.game  = "sa" if game == GTAGame.SA else "gta3"
        merged_ipl.header_lines = [f"# Merged from {Path(dat_path).name}",
                                    f"# {len(sel)} IPL files", ""]

        total_inst = 0
        failed     = []
        for entry in sel:
            if not entry.exists:
                failed.append(f"MISSING: {entry.path}")
                continue
            sub = IPLFile()
            try:
                sub.load(entry.abs_path)
                for sec in sub.sections:
                    # Merge inst sections together; keep others separately
                    if sec.is_inst():
                        # Tag each entry with source filename
                        for e in sec.entries:
                            e.source_line = entry.path
                        # Find or create merged inst section
                        m_inst = merged_ipl.inst_section
                        if m_inst is None:
                            new_sec = IPLSection("inst")
                            merged_ipl.sections.append(new_sec)
                            m_inst = new_sec
                        m_inst.entries.extend(sec.entries)
                        total_inst += len(sec.entries)
                    else:
                        # Keep non-inst sections labelled by source
                        sec_copy = IPLSection(f"{sec.name}_{Path(entry.abs_path).stem}")
                        sec_copy.lines = [f"# from {Path(entry.abs_path).name}"] + sec.lines
                        merged_ipl.sections.append(sec_copy)
            except Exception as ex:
                failed.append(f"ERROR {Path(entry.abs_path).name}: {ex}")

        self._ipl = merged_ipl
        self._file_path = dat_path
        self.WS.add_recent(dat_path)
        self._populate_section_list()
        self._select_inst_section()
        self._update_info()
        self._dirty_lbl.setText("Modified: no")
        self.save_btn.setEnabled(False)

        msg = (f"Loaded {len(sel)} IPL files  |  {total_inst:,} total instances"
               f"  |  game={merged_ipl.game}"
               + (f"  |  {len(failed)} failed" if failed else ""))
        self._set_status(msg)

        if failed:
            QMessageBox.warning(self, "Some Files Failed",
                "The following IPL files could not be loaded:\n\n" +
                "\n".join(failed[:20]))

    def _select_ipls_dialog(self, entries, dat_path: str):
        """Show a checklist dialog — let user pick which IPLs to load."""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout,
                                      QListWidget, QDialogButtonBox,
                                      QListWidgetItem, QPushButton, QLabel)
        dlg = QDialog(self)
        dlg.setWindowTitle(f"Select IPL Files — {Path(dat_path).name}")
        dlg.resize(500, 500)
        lo = QVBoxLayout(dlg)

        lo.addWidget(QLabel(f"DAT: {dat_path}\n"
                            f"Found {len(entries)} IPL files — select which to load:"))

        lst = QListWidget()
        lst.setSelectionMode(QAbstractItemView.SelectionMode.MultiSelection)
        for e in entries:
            label = Path(e.path).name + ("" if e.exists else "  ⚠ missing")
            item  = QListWidgetItem(label)
            item.setData(Qt.ItemDataRole.UserRole, e)
            if not e.exists:
                item.setForeground(QColor("#ff6666"))
            lst.addItem(item)
        # Select all existing by default
        for i in range(lst.count()):
            item = lst.item(i)
            e = item.data(Qt.ItemDataRole.UserRole)
            if e.exists:
                item.setSelected(True)
        lo.addWidget(lst)

        # Select all / none buttons
        br = QHBoxLayout()
        for label, sel in [("All", True), ("None", False), ("Existing only", None)]:
            b = QPushButton(label)
            def _click(checked=False, s=sel, L=lst, E=entries):
                for i in range(L.count()):
                    item = L.item(i)
                    e = item.data(Qt.ItemDataRole.UserRole)
                    if s is None:
                        item.setSelected(e.exists)
                    else:
                        item.setSelected(s)
            b.clicked.connect(_click)
            br.addWidget(b)
        lo.addLayout(br)

        btns = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok |
                                 QDialogButtonBox.StandardButton.Cancel)
        btns.accepted.connect(dlg.accept)
        btns.rejected.connect(dlg.reject)
        lo.addWidget(btns)

        if dlg.exec() != QDialog.DialogCode.Accepted:
            return None
        return [lst.item(i).data(Qt.ItemDataRole.UserRole)
                for i in range(lst.count()) if lst.item(i).isSelected()]

    # ── Entry operations ──────────────────────────────────────────────────────
    def _push_undo(self):
        sec = self._current_inst_section()
        if sec:
            import copy
            self._undo_stack.append(copy.deepcopy(sec.entries))
            if len(self._undo_stack) > 30: self._undo_stack.pop(0)
            self._redo_stack.clear()

    def _undo(self):
        sec = self._current_inst_section()
        if not sec or not self._undo_stack:
            self._set_status("Nothing to undo"); return
        import copy
        self._redo_stack.append(copy.deepcopy(sec.entries))
        sec.entries = self._undo_stack.pop()
        self._populate_table(sec.entries)
        self._mark_dirty(); self._set_status("Undo")

    def _redo(self):
        sec = self._current_inst_section()
        if not sec or not self._redo_stack:
            self._set_status("Nothing to redo"); return
        import copy
        self._undo_stack.append(copy.deepcopy(sec.entries))
        sec.entries = self._redo_stack.pop()
        self._populate_table(sec.entries)
        self._mark_dirty(); self._set_status("Redo")

    def _add_entry(self):
        sec = self._current_inst_section()
        if not sec:
            self._set_status("Load an IPL file first"); return
        self._push_undo()
        new_e = IPLEntry(model_id=0, model_name="new_obj")
        row = self._table.currentRow()
        if row >= 0 and row < len(sec.entries):
            sec.entries.insert(row + 1, new_e)
        else:
            sec.entries.append(new_e)
        self._populate_table(sec.entries)
        self._mark_dirty()
        self._set_status("Entry added")

    def _delete_selected(self):
        sec = self._current_inst_section()
        if not sec: return
        rows = sorted(set(i.row() for i in self._table.selectedItems()), reverse=True)
        if not rows: return
        if QMessageBox.question(self, "Delete",
                f"Delete {len(rows)} selected entries?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
                ) != QMessageBox.StandardButton.Yes:
            return
        self._push_undo()
        for r in rows:
            if r < len(sec.entries):
                sec.entries.pop(r)
        self._populate_table(sec.entries)
        self._update_section_list_count(sec)
        self._mark_dirty()
        self._set_status(f"Deleted {len(rows)} entries")

    def _duplicate_selected(self):
        sec = self._current_inst_section()
        if not sec: return
        rows = sorted(set(i.row() for i in self._table.selectedItems()))
        if not rows: return
        import copy
        self._push_undo()
        new_entries = [copy.copy(sec.entries[r]) for r in rows if r < len(sec.entries)]
        # Insert after last selected
        insert_at = rows[-1] + 1
        for e in reversed(new_entries):
            sec.entries.insert(insert_at, e)
        self._populate_table(sec.entries)
        self._mark_dirty()
        self._set_status(f"Duplicated {len(rows)} entries")

    def _select_all(self):
        self._table.selectAll()

    def _update_section_list_count(self, sec: IPLSection):
        for i in range(self._section_list.count()):
            item = self._section_list.item(i)
            if item.data(Qt.ItemDataRole.UserRole) == sec.name:
                n = len(sec.entries) if sec.is_inst() else len(sec.lines)
                item.setText(f"{sec.name}  ({n})")
                break

    # ── Search / stats ────────────────────────────────────────────────────────
    def _show_find(self):
        text, ok = self._input_dialog("Find Model", "Model name contains:")
        if ok and text:
            self._search_box.setText(text)

    def _filter_interior(self):
        text, ok = self._input_dialog("Filter Interior", "Interior index (0=outdoor):")
        if ok:
            try:
                idx = int(text)
                for i in range(self._int_filter.count()):
                    if self._int_filter.itemData(i) == idx:
                        self._int_filter.setCurrentIndex(i)
                        return
            except ValueError:
                pass

    def _input_dialog(self, title, label):
        from PyQt6.QtWidgets import QInputDialog
        return QInputDialog.getText(self, title, label)

    def _fit_columns(self):
        self._table.resizeColumnsToContents()
        self._table.horizontalHeader().setSectionResizeMode(
            self.COL_MODEL, QHeaderView.ResizeMode.Stretch)

    def _show_stats(self):
        if not self._ipl:
            QMessageBox.information(self, "Stats", "Load an IPL file first.")
            return
        inst = self._ipl.instances
        interiors = sorted(set(e.interior for e in inst))
        models    = sorted(set(e.model_name for e in inst))
        QMessageBox.information(self, "IPL Statistics",
            f"File:       {Path(self._file_path).name}\n"
            f"Game:       {self._ipl.game}\n"
            f"Sections:   {', '.join(self._ipl.section_names())}\n\n"
            f"Instances:  {len(inst)}\n"
            f"Interiors:  {len(interiors)}  ({interiors[:5]}{'…' if len(interiors)>5 else ''})\n"
            f"Models:     {len(models)} unique\n"
            f"  First 5:  {models[:5]}")

    def _update_info(self):
        if not self._ipl:
            self._info_lbl.setText("No file loaded"); return
        name = Path(self._file_path).name
        n    = len(self._ipl.instances)
        self._info_lbl.setText(f"{name}\n{n} instances\ngame={self._ipl.game}")

    # ── Context menu ──────────────────────────────────────────────────────────
    def _show_context_menu(self, pos):
        menu = QMenu(self)
        menu.addAction("Add Entry",           self._add_entry)
        menu.addAction("Delete Selected",     self._delete_selected)
        menu.addAction("Duplicate Selected",  self._duplicate_selected)
        menu.addSeparator()
        menu.addAction("Export CSV…",         self._export_csv)
        menu.exec(self._table.viewport().mapToGlobal(pos))

    # ── GUIWorkshop stubs ─────────────────────────────────────────────────────
    def _on_list_selection_changed(self, row: int): pass
    def _on_add_item(self): self._add_entry()
    def _on_remove_item(self): self._delete_selected()
    def _on_tab_changed(self, idx: int): pass
    def _zoom(self, f): pass
    def _fit(self): self._fit_columns()
    def _jump(self): pass
    def _on_toolbar_action(self, action: str): pass
    def _copy_item(self): pass
    def _paste_item(self): pass


# =============================================================================
# Docked opener (called from imgfactory)
# =============================================================================

def open_ipl_workshop(main_window, file_path=None):
    """Open IPL Workshop docked in IMG Factory tab. Returns workshop instance."""
    try:
        from PyQt6.QtWidgets import QVBoxLayout, QWidget
        from PyQt6.QtCore import Qt
        from apps.methods.imgfactory_svg_icons import SVGIconFactory

        if hasattr(main_window, 'main_tab_widget') and main_window.main_tab_widget:
            tw = main_window.main_tab_widget
            # Re-use existing tab
            for i in range(tw.count()):
                w = tw.widget(i)
                if w:
                    found = w.findChildren(IPLWorkshop)
                    if found:
                        tw.setCurrentIndex(i)
                        if file_path:
                            found[0]._open_file(file_path)
                        return found[0]

            # New docked tab
            tab = QWidget()
            tab.file_type = "WORKSHOP"
            lo = QVBoxLayout(tab)
            lo.setContentsMargins(0, 0, 0, 0)
            lo.setSpacing(0)

            workshop = IPLWorkshop(tab, main_window)
            workshop.setWindowFlags(Qt.WindowType.Widget)
            lo.addWidget(workshop)

            try:
                icon = SVGIconFactory.ipl_editor_icon(20)
                idx = tw.addTab(tab, icon, "IPL")
            except Exception:
                idx = tw.addTab(tab, "IPL")
            tw.setCurrentIndex(idx)
            workshop.show()

            if file_path:
                from PyQt6.QtCore import QTimer
                QTimer.singleShot(100, lambda: workshop._open_file(file_path))

            # Ensure tab area visible
            if hasattr(main_window, '_ensure_tab_area_visible'):
                main_window._ensure_tab_area_visible()

            # Register in taskbar
            try:
                from apps.gui.gui_layout import _register_tool_taskbar
                _register_tool_taskbar(main_window, "ipl", "IPL",
                    SVGIconFactory.ipl_editor_icon,
                    "IPL Workshop — Item Placement Editor",
                    target=tab)
            except Exception as e:
                if hasattr(main_window, 'log_message'):
                    main_window.log_message(f"IPL taskbar error: {e}")

            if hasattr(main_window, 'log_message'):
                main_window.log_message("IPL Workshop opened (docked)")
            return workshop

        # Standalone fallback (outside imgfactory)
        workshop = IPLWorkshop(parent=None, main_window=main_window)
        workshop.setWindowTitle("IPL Workshop — Standalone")
        workshop.resize(1300, 800)
        workshop.show()
        if file_path:
            workshop._open_file(file_path)
        return workshop

    except Exception as e:
        import traceback
        traceback.print_exc()
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"IPL Workshop error: {e}")


# =============================================================================
# Standalone launcher
# =============================================================================

if __name__ == "__main__":
    import traceback
    print(f"{App_name} {Build} starting...")
    try:
        app = QApplication(sys.argv)
        w = IPLWorkshop()
        w.setWindowTitle(f"{App_name} — Standalone")
        w.resize(1300, 800)
        w.show()
        if len(sys.argv) > 1 and Path(sys.argv[1]).is_file():
            w._open_file(sys.argv[1])
        sys.exit(app.exec())
    except Exception as e:
        print(f"ERROR: {e}")
        traceback.print_exc()
        sys.exit(1)


# ── IPL World Map ─────────────────────────────────────────────────────────────

class IPLMapView(QFrame):  # vers 1
    """Interactive 2D top-down world map showing IPL instances as cubes.
    Supports pan (middle/left-drag), zoom (wheel), multi-select, and
    bulk translate/rotate operations."""

    selection_changed = pyqtSignal(list)   # emits list of selected IPLEntry

    # GTA world extents (SA/SOL coordinate space)
    WORLD_MIN_X, WORLD_MAX_X = -3000.0,  3000.0
    WORLD_MIN_Y, WORLD_MAX_Y = -3000.0,  3000.0

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setFrameStyle(QFrame.Shape.StyledPanel)
        self.setMinimumSize(400, 300)
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)
        self.setCursor(Qt.CursorShape.CrossCursor)

        self._entries: list   = []      # list of IPLEntry
        self._selected: set   = set()   # indices of selected entries
        self._ipl_colors: dict= {}      # source_ipl → QColor
        self._show_paths      = True
        self._path_nodes: list = []   # list of (x, y, z, type) tuples
        self._show_all_ipls   = True
        self._active_ipls: set= set()   # filter: only show these source_ipls
        self._radar_image       = None   # QImage composite from RadarWorkshop
        self._radar_world_bounds= (-3000.0, 3000.0, -3000.0, 3000.0)  # (xmin,xmax,ymin,ymax)
        self._show_radar        = True
        self._water_quads: list = []   # SA water quads: [{'corners':[{x,y}...],'flag':int}]
        self._water_rects: list = []   # GTA3/VC water.dat rects: [(x1,y1,x2,y2,level)]
        self._show_water        = True

        # View state
        self._zoom   = 1.0
        self._pan_x  = 0.0
        self._pan_y  = 0.0
        self._drag_start = None
        self._drag_pan   = False
        self._sel_rect   = None   # rubber-band selection (screen coords)

        self._dirty = True

    # ── Data loading ──────────────────────────────────────────────────────
    def load_entries(self, entries: list, ipl_filter: set = None):
        """Load IPL entries. ipl_filter: set of source_ipl basenames to show."""
        self._entries = entries or []
        self._selected.clear()
        self._active_ipls = ipl_filter or set()

        # Auto-colour each source IPL
        import hashlib
        self._ipl_colors.clear()
        for e in self._entries:
            key = e.source_line or ''
            if key not in self._ipl_colors:
                h = int(hashlib.md5(key.encode()).hexdigest()[:6], 16)
                r = (h >> 16) & 0xFF
                g = (h >> 8)  & 0xFF
                b =  h        & 0xFF
                # Ensure visible on dark bg — lift brightness
                mn = 80
                r = max(r, mn); g = max(g, mn); b = max(b, mn)
                self._ipl_colors[key] = QColor(r, g, b, 200)

        # Also extract path nodes from path sections if available
        self._path_nodes.clear()
        self._fit_all()
        self.update()

    def load_path_nodes(self, ipl_sections: list): #vers 1
        """Load path nodes from IPL path sections for overlay on the map.
        ipl_sections: list of IPLSection objects with name == 'path'."""
        self._path_nodes.clear()
        for sec in ipl_sections:
            if not hasattr(sec, 'name') or sec.name != 'path':
                continue
            for line in getattr(sec, 'lines', []):
                line = line.strip()
                if not line or line.startswith('#'):
                    continue
                parts = [p.strip() for p in line.split(',')]
                if len(parts) >= 4:
                    try:
                        # path format: type, posX, posY, posZ[, ...]
                        node_type = parts[0].strip()
                        px = float(parts[1])
                        py = float(parts[2])
                        pz = float(parts[3])
                        self._path_nodes.append((px, py, pz, node_type))
                    except (ValueError, IndexError):
                        pass
        self.update()

    def load_radar_image(self, radar_image, world_bounds): #vers 1
        """Set the radar composite image for background overlay.
        radar_image: QImage from RadarWorkshop.get_composite_image()
        world_bounds: (xmin, xmax, ymin, ymax) from get_world_bounds()"""
        self._radar_image        = radar_image
        self._radar_world_bounds = world_bounds
        self.update()

    def load_water(self, quads: list, rects: list): #vers 1
        """Load water geometry for overlay.
        quads: SA water quads from SaWaterParser.quads
        rects: GTA3/VC rects from WaterDatParser.rects"""
        self._water_quads = quads or []
        self._water_rects = rects or []
        self.update()

    def set_ipl_filter(self, active_ipls: set):
        self._active_ipls = active_ipls
        self.update()

    # ── Coordinate transform ──────────────────────────────────────────────
    def _world_to_screen(self, wx, wy):
        """Convert GTA world XY → screen pixel."""
        W, H = self.width(), self.height()
        cx = W / 2 + self._pan_x
        cy = H / 2 + self._pan_y
        scale = min(W, H) / (self.WORLD_MAX_X - self.WORLD_MIN_X) * self._zoom
        sx =  cx + wx * scale
        sy =  cy - wy * scale   # Y flipped (screen Y down, world Y up)
        return sx, sy

    def _screen_to_world(self, sx, sy):
        W, H = self.width(), self.height()
        cx = W / 2 + self._pan_x
        cy = H / 2 + self._pan_y
        scale = min(W, H) / (self.WORLD_MAX_X - self.WORLD_MIN_X) * self._zoom
        wx = (sx - cx) / scale
        wy = (cy - sy) / scale
        return wx, wy

    def _fit_all(self):
        """Fit all visible entries in view."""
        visible = self._visible_entries()
        if not visible:
            self._zoom = 1.0; self._pan_x = self._pan_y = 0.0; return
        xs = [e.px for e in visible]
        ys = [e.py for e in visible]
        cx = (min(xs) + max(xs)) / 2
        cy = (min(ys) + max(ys)) / 2
        rng = max(max(xs)-min(xs), max(ys)-min(ys), 100)
        W, H = max(self.width(),400), max(self.height(),300)
        scale = min(W, H) / (self.WORLD_MAX_X - self.WORLD_MIN_X)
        self._zoom = min(W, H) / (rng + 200) / scale
        # Pan so world (cx,cy) lands at screen centre
        self._pan_x = -cx * (min(W,H) / (self.WORLD_MAX_X - self.WORLD_MIN_X) * self._zoom)
        self._pan_y =  cy * (min(W,H) / (self.WORLD_MAX_X - self.WORLD_MIN_X) * self._zoom)

    def _visible_entries(self):
        if self._active_ipls:
            return [e for e in self._entries
                    if (e.source_line or '') in self._active_ipls]
        return self._entries

    # ── Paint ─────────────────────────────────────────────────────────────
    def paintEvent(self, event):
        from PyQt6.QtGui import QPainter, QPen, QBrush, QColor, QFont
        from PyQt6.QtCore import QRectF
        p = QPainter(self)
        p.setRenderHint(QPainter.RenderHint.Antialiasing)
        W, H = self.width(), self.height()

        # Background
        p.fillRect(self.rect(), QColor(20, 22, 30))

        # Radar map background image
        if self._show_radar and self._radar_image and not self._radar_image.isNull():
            xmin, xmax, ymin, ymax = self._radar_world_bounds
            # World corners → screen corners
            sx0, sy0 = self._world_to_screen(xmin, ymax)   # top-left (world Y max = screen top)
            sx1, sy1 = self._world_to_screen(xmax, ymin)   # bottom-right
            from PyQt6.QtCore import QRectF as _QRF2
            p.setOpacity(0.55)
            p.drawImage(_QRF2(sx0, sy0, sx1-sx0, sy1-sy0), self._radar_image)
            p.setOpacity(1.0)

        # Grid lines
        self._draw_grid(p, W, H)

        # Entries as cubes
        visible = self._visible_entries()
        cube_sz = max(2.0, 6.0 * self._zoom)
        half    = cube_sz / 2

        from PyQt6.QtCore import QRectF
        for i, e in enumerate(self._entries):
            if self._active_ipls and (e.source_line or '') not in self._active_ipls:
                continue
            sx, sy = self._world_to_screen(e.px, e.py)
            col = self._ipl_colors.get(e.source_line or '', QColor(140, 160, 200, 180))
            is_sel = i in self._selected

            if is_sel:
                p.setPen(QPen(QColor(255, 220, 50), 1.5))
                p.setBrush(QBrush(QColor(255, 220, 50, 200)))
            else:
                p.setPen(QPen(col.darker(140), 0.5))
                p.setBrush(QBrush(col))

            p.drawRect(QRectF(sx - half, sy - half, cube_sz, cube_sz))

        # Rubber-band selection rect
        if self._sel_rect:
            p.setPen(QPen(QColor(100, 200, 255), 1, Qt.PenStyle.DashLine))
            p.setBrush(QBrush(QColor(100, 200, 255, 30)))
            p.drawRect(self._sel_rect)

        # Path nodes overlay
        if self._show_paths and self._path_nodes:
            from PyQt6.QtCore import QRectF as _QRF
            node_sz = max(3.0, 4.0 * self._zoom)
            p.setPen(QPen(QColor(255, 200, 50, 180), 1.0))
            p.setBrush(QBrush(QColor(255, 200, 50, 100)))
            for nx, ny, nz, ntype in self._path_nodes:
                sx2, sy2 = self._world_to_screen(nx, ny)
                p.drawEllipse(_QRF(sx2-node_sz/2, sy2-node_sz/2, node_sz, node_sz))

        # Water overlay
        if self._show_water and (self._water_quads or self._water_rects):
            from PyQt6.QtCore import QRectF as _WQRF
            from PyQt6.QtGui import QPainterPath as _QPP
            p.setPen(QPen(QColor(30, 140, 255, 200), 1.0))
            p.setBrush(QBrush(QColor(20, 100, 220, 40)))
            # SA quads (4-corner polygons)
            for q in self._water_quads:
                corners = q.get("corners", [])
                if len(corners) < 3:
                    continue
                pp = _QPP()
                sx0, sy0 = self._world_to_screen(corners[0]["x"], corners[0]["y"])
                pp.moveTo(sx0, sy0)
                for c in corners[1:]:
                    sx, sy = self._world_to_screen(c["x"], c["y"])
                    pp.lineTo(sx, sy)
                pp.closeSubpath()
                p.drawPath(pp)
            # GTA3/VC rects (x1,y1,x2,y2,level)
            p.setPen(QPen(QColor(80, 160, 255, 180), 0.8))
            p.setBrush(QBrush(QColor(20, 100, 220, 30)))
            for r in self._water_rects:
                if len(r) < 4:
                    continue
                sx0, sy0 = self._world_to_screen(r[0], r[3])  # x1,y2 (top-left in screen)
                sx1, sy1 = self._world_to_screen(r[2], r[1])  # x2,y1 (bottom-right)
                p.drawRect(_WQRF(sx0, sy0, sx1-sx0, sy1-sy0))

        # HUD
        p.setPen(QColor(180, 180, 180))
        p.setFont(QFont('Arial', 9))
        n_sel   = len(self._selected)
        n_vis   = len(visible)
        n_paths = len(self._path_nodes)
        n_water = len(self._water_quads) + len(self._water_rects)
        p.drawText(6, 16,
            f"Zoom: {self._zoom:.2f}×   Objects: {n_vis:,}"
            + (f"   Paths: {n_paths}" if n_paths else "")
            + (f"   Water: {n_water}" if n_water else "")
            + (f"   Selected: {n_sel}" if n_sel else ""))

        p.end()

    def _draw_grid(self, p, W, H):
        from PyQt6.QtGui import QPen, QColor
        from PyQt6.QtCore import QLineF
        # Draw world-space grid at nice intervals
        for interval in [100, 500, 1000, 2000]:
            sx0, _ = self._world_to_screen(0, 0)
            sx1, _ = self._world_to_screen(interval, 0)
            px_per_unit = abs(sx1 - sx0)
            if px_per_unit > 40:
                break
        col = QColor(45, 50, 65)
        col_axis = QColor(70, 80, 110)
        p.setPen(QPen(col, 0.5))
        x = -((self.WORLD_MAX_X) // interval) * interval
        while x <= self.WORLD_MAX_X:
            sx, _ = self._world_to_screen(x, 0)
            pen = QPen(col_axis if x == 0 else col, 0.5)
            p.setPen(pen)
            p.drawLine(int(sx), 0, int(sx), H)
            x += interval
        y = -((self.WORLD_MAX_Y) // interval) * interval
        while y <= self.WORLD_MAX_Y:
            _, sy = self._world_to_screen(0, y)
            pen = QPen(col_axis if y == 0 else col, 0.5)
            p.setPen(pen)
            p.drawLine(0, int(sy), W, int(sy))
            y += interval

    # ── Mouse interaction ─────────────────────────────────────────────────
    def wheelEvent(self, event):
        factor = 1.15 if event.angleDelta().y() > 0 else 1/1.15
        self._zoom = max(0.05, min(200.0, self._zoom * factor))
        self.update()

    def mousePressEvent(self, event):
        if event.button() == Qt.MouseButton.MiddleButton:
            self._drag_start = event.position()
            self._drag_pan   = True
        elif event.button() == Qt.MouseButton.LeftButton:
            self._drag_start = event.position()
            self._drag_pan   = False
            self._sel_rect   = None

    def mouseMoveEvent(self, event):
        if self._drag_start is None:
            return
        dx = event.position().x() - self._drag_start.x()
        dy = event.position().y() - self._drag_start.y()
        if self._drag_pan or (event.buttons() & Qt.MouseButton.MiddleButton):
            self._pan_x += dx; self._pan_y += dy
            self._drag_start = event.position()
            self.update()
        elif event.buttons() & Qt.MouseButton.LeftButton:
            from PyQt6.QtCore import QRectF
            x0 = min(self._drag_start.x(), event.position().x())
            y0 = min(self._drag_start.y(), event.position().y())
            x1 = max(self._drag_start.x(), event.position().x())
            y1 = max(self._drag_start.y(), event.position().y())
            self._sel_rect = QRectF(x0, y0, x1-x0, y1-y0)
            self.update()

    def mouseReleaseEvent(self, event):
        if event.button() == Qt.MouseButton.LeftButton and self._drag_start is not None:
            if self._sel_rect and (self._sel_rect.width() > 4 or
                                    self._sel_rect.height() > 4):
                # Box select
                mods = event.modifiers()
                if not (mods & Qt.KeyboardModifier.ShiftModifier):
                    self._selected.clear()
                r = self._sel_rect
                for i, e in enumerate(self._entries):
                    if self._active_ipls and (e.source_line or '') not in self._active_ipls:
                        continue
                    sx, sy = self._world_to_screen(e.px, e.py)
                    if r.contains(sx, sy):
                        self._selected.add(i)
            else:
                # Point click — find nearest entry
                px = event.position().x(); py = event.position().y()
                best_i = None; best_d = 12.0
                for i, e in enumerate(self._entries):
                    if self._active_ipls and (e.source_line or '') not in self._active_ipls:
                        continue
                    sx, sy = self._world_to_screen(e.px, e.py)
                    d = ((sx-px)**2 + (sy-py)**2)**0.5
                    if d < best_d:
                        best_d = d; best_i = i
                mods = event.modifiers()
                if best_i is not None:
                    if mods & Qt.KeyboardModifier.ShiftModifier:
                        if best_i in self._selected:
                            self._selected.discard(best_i)
                        else:
                            self._selected.add(best_i)
                    else:
                        self._selected = {best_i}
                elif not (mods & Qt.KeyboardModifier.ShiftModifier):
                    self._selected.clear()

            self._sel_rect = None
            self.update()
            self.selection_changed.emit(
                [self._entries[i] for i in sorted(self._selected)])
        self._drag_start = None
        self._drag_pan   = False

    def keyPressEvent(self, event):
        if event.key() == Qt.Key.Key_A and \
                event.modifiers() & Qt.KeyboardModifier.ControlModifier:
            self._selected = set(range(len(self._entries)))
            self.selection_changed.emit(list(self._entries))
            self.update()
        elif event.key() == Qt.Key.Key_Escape:
            self._selected.clear()
            self.selection_changed.emit([])
            self.update()
        elif event.key() == Qt.Key.Key_F:
            self._fit_all(); self.update()

    # ── Selection helpers ─────────────────────────────────────────────────
    def select_by_ipl(self, ipl_name: str, add=False):
        if not add:
            self._selected.clear()
        for i, e in enumerate(self._entries):
            if (e.source_line or '') == ipl_name:
                self._selected.add(i)
        self.update()
        self.selection_changed.emit(
            [self._entries[i] for i in sorted(self._selected)])

    def selected_entries(self):
        return [self._entries[i] for i in sorted(self._selected)]

    # ── Bulk operations ───────────────────────────────────────────────────
    def translate_selected(self, dx: float, dy: float, dz: float):
        """Move selected entries by (dx, dy, dz)."""
        if not self._selected:
            return
        for i in self._selected:
            e = self._entries[i]
            e.px += dx
            e.py += dy
            e.pz += dz
        self.update()

    def rotate_selected_yaw(self, degrees: float):
        """Rotate selected entries around Z axis (yaw) about their centroid."""
        import math
        if not self._selected:
            return
        sel = [self._entries[i] for i in self._selected]
        cx = sum(e.px for e in sel) / len(sel)
        cy = sum(e.py for e in sel) / len(sel)
        rad = math.radians(degrees)
        cos_a, sin_a = math.cos(rad), math.sin(rad)
        for i in self._selected:
            e = self._entries[i]
            rx = e.px - cx; ry = e.py - cy
            e.px = cx + rx * cos_a - ry * sin_a
            e.py = cy + rx * sin_a + ry * cos_a
            # Update quaternion — rotate around Z
            # Current quat: (rx, ry, rz, rw)
            half = rad / 2
            dqz, dqw = math.sin(half), math.cos(half)
            qx, qy, qz, qw = e.rx, e.ry, e.rz, e.rw
            e.rx = qw*0   + qx*dqw + qy*dqz - qz*0
            e.ry = qw*0   - qx*dqz + qy*dqw + qz*0
            e.rz = qw*dqz + qx*0   - qy*0   + qz*dqw
            e.rw = qw*dqw - qx*0   - qy*0   - qz*dqz
        self.update()


class IPLMapPanel(QFrame):  # vers 1
    """Full map panel with map view + translate/rotate controls."""

    def __init__(self, workshop, parent=None):
        super().__init__(parent)
        self._ws = workshop
        self._build_ui()

    def _build_ui(self):
        from PyQt6.QtWidgets import (QVBoxLayout, QHBoxLayout, QGroupBox,
            QDoubleSpinBox, QPushButton, QLabel, QCheckBox, QComboBox)
        root = QVBoxLayout(self)
        root.setContentsMargins(0, 0, 0, 0)
        root.setSpacing(2)

        # ── Toolbar ───────────────────────────────────────────────────────
        bar = QHBoxLayout(); bar.setContentsMargins(4, 2, 4, 2); bar.setSpacing(6)

        fit_btn = QPushButton("Fit [F]")
        fit_btn.setFixedHeight(24)
        fit_btn.setToolTip("Fit all objects in view (F)")
        fit_btn.clicked.connect(self._fit)
        bar.addWidget(fit_btn)

        sel_ipl_btn = QPushButton("Select by IPL")
        sel_ipl_btn.setFixedHeight(24)
        sel_ipl_btn.setToolTip("Select all objects from a specific IPL file")
        sel_ipl_btn.clicked.connect(self._select_by_ipl_dialog)
        bar.addWidget(sel_ipl_btn)

        sel_all_btn = QPushButton("Select All [Ctrl+A]")
        sel_all_btn.setFixedHeight(24)
        sel_all_btn.clicked.connect(self._select_all)
        bar.addWidget(sel_all_btn)

        clr_btn = QPushButton("Clear [Esc]")
        clr_btn.setFixedHeight(24)
        clr_btn.clicked.connect(self._clear_sel)
        bar.addWidget(clr_btn)

        path_btn = QPushButton("Paths ◎")
        path_btn.setCheckable(True)
        path_btn.setChecked(True)
        path_btn.setFixedHeight(24)
        path_btn.setToolTip("Toggle path node overlay (yellow circles)")
        path_btn.toggled.connect(lambda v: setattr(self._map,'_show_paths',v) or self._map.update())
        bar.addWidget(path_btn)

        radar_btn = QPushButton("Radar 🗺")
        radar_btn.setCheckable(True)
        radar_btn.setChecked(True)
        radar_btn.setFixedHeight(24)
        radar_btn.setToolTip("Toggle radar map background (from open Radar Workshop tab)")
        radar_btn.toggled.connect(lambda v: setattr(self._map,'_show_radar',v) or self._map.update())
        bar.addWidget(radar_btn)

        load_radar_btn = QPushButton("Load Radar…")
        load_radar_btn.setFixedHeight(24)
        load_radar_btn.setToolTip(
            "Load radar from Radar Workshop tab or browse for BMP/PNG")
        load_radar_btn.clicked.connect(self._load_radar)
        bar.addWidget(load_radar_btn)

        water_btn = QPushButton("Water 💧")
        water_btn.setCheckable(True)
        water_btn.setChecked(True)
        water_btn.setFixedHeight(24)
        water_btn.setToolTip("Toggle water geometry overlay (blue polygons)")
        water_btn.toggled.connect(lambda v: setattr(self._map,'_show_water',v) or self._map.update())
        bar.addWidget(water_btn)

        load_water_btn = QPushButton("Load Water…")
        load_water_btn.setFixedHeight(24)
        load_water_btn.setToolTip("Load water geometry from open Water Workshop tab or browse")
        load_water_btn.clicked.connect(self._load_water)
        bar.addWidget(load_water_btn)

        bar.addStretch()
        self._sel_lbl = QLabel("No selection")
        self._sel_lbl.setStyleSheet("color: palette(mid);")
        bar.addWidget(self._sel_lbl)
        root.addLayout(bar)

        # ── Map view ──────────────────────────────────────────────────────
        self._map = IPLMapView()
        self._map.selection_changed.connect(self._on_selection_changed)
        root.addWidget(self._map, stretch=1)

        # ── Translate / Rotate controls ───────────────────────────────────
        ctrl = QHBoxLayout(); ctrl.setContentsMargins(4, 2, 4, 4); ctrl.setSpacing(8)

        # Translate group
        tg = QGroupBox("Translate selection")
        tl = QHBoxLayout(tg); tl.setSpacing(4)
        self._tx = self._spin(-9999, 9999, 0, "X offset")
        self._ty = self._spin(-9999, 9999, 0, "Y offset")
        self._tz = self._spin(-9999, 9999, 0, "Z offset")
        for lbl, sp in [("X:", self._tx), ("Y:", self._ty), ("Z:", self._tz)]:
            tl.addWidget(QLabel(lbl)); tl.addWidget(sp)
        apply_t = QPushButton("Apply")
        apply_t.setFixedHeight(24)
        apply_t.setToolTip("Move selected objects by X/Y/Z offset")
        apply_t.clicked.connect(self._apply_translate)
        tl.addWidget(apply_t)
        ctrl.addWidget(tg)

        # Rotate group
        rg = QGroupBox("Rotate selection (yaw around Z)")
        rl = QHBoxLayout(rg); rl.setSpacing(4)
        self._rdeg = self._spin(-360, 360, 90, "Degrees to rotate")
        rl.addWidget(QLabel("°:"))
        rl.addWidget(self._rdeg)
        for deg_lbl, deg_val in [("-90°", -90), ("+90°", 90), ("180°", 180)]:
            b = QPushButton(deg_lbl); b.setFixedHeight(24); b.setFixedWidth(42)
            b.clicked.connect(lambda _=False, d=deg_val: self._rotate(d))
            rl.addWidget(b)
        apply_r = QPushButton("Apply")
        apply_r.setFixedHeight(24)
        apply_r.clicked.connect(lambda: self._rotate(self._rdeg.value()))
        rl.addWidget(apply_r)
        ctrl.addWidget(rg)

        root.addLayout(ctrl)

    def _spin(self, lo, hi, val, tip):
        from PyQt6.QtWidgets import QDoubleSpinBox
        s = QDoubleSpinBox(); s.setRange(lo, hi); s.setValue(val)
        s.setDecimals(2); s.setFixedWidth(72); s.setFixedHeight(24)
        s.setToolTip(tip)
        return s

    def refresh(self, entries, ipl_filter=None):
        self._map.load_entries(entries, ipl_filter)

    def _fit(self):
        self._map._fit_all(); self._map.update()

    def _select_all(self):
        self._map._selected = set(range(len(self._map._entries)))
        self._map.update()
        self._on_selection_changed(self._map._entries)

    def _clear_sel(self):
        self._map._selected.clear()
        self._map.update()
        self._on_selection_changed([])

    def _on_selection_changed(self, entries):
        n = len(entries)
        self._sel_lbl.setText(
            f"{n:,} object(s) selected" if n else "No selection")

    def _select_by_ipl_dialog(self):
        from PyQt6.QtWidgets import QInputDialog
        ipls = sorted({(e.source_line or '') for e in self._map._entries} - {''})
        if not ipls:
            return
        name, ok = QInputDialog.getItem(
            self, "Select by IPL", "IPL file:", ipls, 0, False)
        if ok and name:
            mods = self._map.focusWidget()
            self._map.select_by_ipl(name)

    def _load_water(self): #vers 1
        """Load water geometry from open Water Workshop tab."""
        mw = getattr(self._ws, 'main_window', None)
        water_ws = None
        if mw and hasattr(mw, 'main_tab_widget'):
            tw = mw.main_tab_widget
            try:
                from apps.components.Water_Editor.water_workshop import WaterWorkshop
                for i in range(tw.count()):
                    w = tw.widget(i)
                    if isinstance(w, WaterWorkshop):
                        water_ws = w; break
                    for child in (w.findChildren(WaterWorkshop) if w else []):
                        water_ws = child; break
                    if water_ws:
                        break
            except ImportError:
                pass

        if water_ws:
            quads = water_ws.get_water_quads()
            rects = water_ws.get_water_rects()
            if quads or rects:
                self._map.load_water(quads, rects)
                n = len(quads) + len(rects)
                if mw and hasattr(mw,'log_message'):
                    mw.log_message(
                        f"IPL Map: water overlay loaded ({len(quads)} quads, {len(rects)} rects)")
                return
            else:
                from PyQt6.QtWidgets import QMessageBox
                QMessageBox.information(
                    self,"No Water Data",
                    "Water Workshop is open but no water file is loaded.")
                return

        from PyQt6.QtWidgets import QMessageBox
        QMessageBox.information(
            self,"No Water Workshop",
            "Open Water Workshop and load a water.dat or waterpro.dat first,\n"
            "then click Load Water to overlay it on the map.")

    def _get_asset_db(self): #vers 1
        """Return asset_db from main_window if available."""
        mw = getattr(self,'main_window',None)
        return getattr(mw,'asset_db',None) if mw else None

    def _load_radar(self): #vers 1
        """Load radar composite from open Radar Workshop tab, or browse for image."""
        # First try to find an open RadarWorkshop tab
        mw = getattr(self._ws, 'main_window', None)
        radar_ws = None
        if mw and hasattr(mw, 'main_tab_widget'):
            tw = mw.main_tab_widget
            for i in range(tw.count()):
                w = tw.widget(i)
                # RadarWorkshop may be inside a container
                from apps.components.Radar_Editor.radar_workshop import RadarWorkshop
                if isinstance(w, RadarWorkshop):
                    radar_ws = w; break
                # Check children
                for child in w.findChildren(RadarWorkshop) if w else []:
                    radar_ws = child; break
                if radar_ws:
                    break

        if radar_ws and radar_ws._tile_rgba:
            img    = radar_ws.get_composite_image(max_size=4096)
            bounds = radar_ws.get_world_bounds()
            if img and not img.isNull():
                self._map.load_radar_image(img, bounds)
                n = len(radar_ws._tile_rgba)
                if mw and hasattr(mw,'log_message'):
                    mw.log_message(
                        f"IPL Map: radar background loaded ({n} tiles, "
                        f"bounds {bounds[0]:.0f}..{bounds[1]:.0f})")
                return

        # No radar workshop open — browse for a BMP/PNG file
        from PyQt6.QtWidgets import QFileDialog, QInputDialog
        path, _ = QFileDialog.getOpenFileName(
            self, "Load Radar Map Image", "",
            "Images (*.bmp *.png *.jpg *.jpeg);;All Files (*)")
        if not path:
            return

        from PyQt6.QtGui import QImage
        img = QImage(path)
        if img.isNull():
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(self,"Load Failed","Could not load image:\n"+path)
            return

        # Ask for world bounds
        game_presets = [
            ("GTA III / VC  (-2000 to 2000)", (-2000.0, 2000.0, -2000.0, 2000.0)),
            ("SA / SOL  (-3000 to 3000)",     (-3000.0, 3000.0, -3000.0, 3000.0)),
            ("SOL Large  (-6000 to 6000)",    (-6000.0, 6000.0, -6000.0, 6000.0)),
        ]
        choice, ok = QInputDialog.getItem(
            self, "World Bounds", "Select game world coverage:",
            [g[0] for g in game_presets], 1, False)
        if not ok:
            return
        bounds = next(g[1] for g in game_presets if g[0]==choice)
        self._map.load_radar_image(img, bounds)
        if mw and hasattr(mw,'log_message'):
            mw.log_message(f"IPL Map: radar background loaded from {path}")

    def _apply_translate(self):
        dx = self._tx.value(); dy = self._ty.value(); dz = self._tz.value()
        if dx == dy == dz == 0:
            return
        self._map.translate_selected(dx, dy, dz)
        n = len(self._map._selected)
        # Sync back to workshop table
        if hasattr(self._ws, '_table'):
            self._ws._populate_table(self._ws._current_entries())
        if self._ws.main_window and hasattr(self._ws.main_window, 'log_message'):
            self._ws.main_window.log_message(
                f"IPL: moved {n} object(s) by X={dx:+.1f} Y={dy:+.1f} Z={dz:+.1f}")

    def _rotate(self, degrees):
        self._map.rotate_selected_yaw(degrees)
        n = len(self._map._selected)
        if hasattr(self._ws, '_table'):
            self._ws._populate_table(self._ws._current_entries())
        if self._ws.main_window and hasattr(self._ws.main_window, 'log_message'):
            self._ws.main_window.log_message(
                f"IPL: rotated {n} object(s) by {degrees:+.0f}°")
