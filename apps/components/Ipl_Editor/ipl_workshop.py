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
    def _create_centre_panel(self):
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        cl = QVBoxLayout(panel)
        cl.setContentsMargins(0, 0, 0, 0)
        cl.setSpacing(2)

        # Search / filter bar
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
        cl.addLayout(sbar)

        # Instance table
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
        cl.addWidget(self._table)

        # Text view for non-inst sections
        from PyQt6.QtWidgets import QTextEdit
        self._text_view = QTextEdit()
        self._text_view.setReadOnly(False)
        self._text_view.setFont(QFont("Courier New", 9))
        self._text_view.setPlaceholderText("Section content appears here…")
        self._text_view.setVisible(False)
        self._text_view.textChanged.connect(self._on_text_edited)
        cl.addWidget(self._text_view)

        return panel

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
