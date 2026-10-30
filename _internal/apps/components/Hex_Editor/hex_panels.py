#this belongs in apps/components/Hex_Editor/hex_panels.py - Version: 4
# X-Seti - September 2026 - IMG Factory 1.6 - Hex Workshop side panels

"""hex_panels.py - Inspector (values at the cursor), Structure (RenderWare section tree with
RW Analyze style colours and section operations, plus COL / IMG / DIR / binary IPL listings),
Search (hex, text, UTF-16, numbers; find / all / replace), Bookmarks and Compare panels.
The panels never touch bytes themselves: they emit signals / call the host's callbacks."""

##Methods list -
# InspectorPanel
# StructurePanel
# SearchPanel
# BookmarksPanel
# ComparePanel
# parse_search_pattern
# find_all
# diff_ranges

import hashlib
import struct
import zlib
from typing import List, Optional, Tuple

from PyQt6.QtCore import Qt, pyqtSignal
from PyQt6.QtGui import QBrush, QColor, QFont
from PyQt6.QtWidgets import (
    QAbstractItemView, QCheckBox, QComboBox, QFileDialog, QHBoxLayout, QInputDialog, QLabel,
    QLineEdit, QListWidget, QListWidgetItem, QMenu, QMessageBox, QPushButton, QSizePolicy, QTableWidget,
    QTableWidgetItem, QTreeWidget, QTreeWidgetItem, QVBoxLayout, QWidget)

from apps.methods import rw_chunks as rw


# ---------------------------------------------------------------- helpers
def parse_search_pattern(kind: str, text: str) -> bytes:
    """User text -> bytes to look for. kind: hex | text | utf16 | u8/i16.. | f32 | f64 (+ ' BE')."""
    t = text.strip()
    if kind == "Hex bytes":
        h = t.replace("0x", " ").replace(",", " ").replace(" ", "")
        if len(h) % 2 or not h:
            raise ValueError("Hex needs an even number of digits, e.g. 4A 6F 0B")
        return bytes.fromhex(h)
    if kind == "Text (ASCII)":
        return t.encode("latin1")
    if kind == "Text (UTF-16)":
        return t.encode("utf-16le")
    be = kind.endswith("BE")
    e = ">" if be else "<"
    base = kind.replace(" BE", "").replace(" LE", "")
    fmt = {"Int8": "b", "UInt8": "B", "Int16": "h", "UInt16": "H", "Int32": "i", "UInt32": "I",
           "Int64": "q", "UInt64": "Q", "Float32": "f", "Float64": "d"}[base]
    val = float(t) if fmt in "fd" else int(t, 0)
    return struct.pack(e + fmt, val)


SEARCH_KINDS = ["Hex bytes", "Text (ASCII)", "Text (UTF-16)", "Int8", "UInt8", "Int16", "UInt16", "Int32",
                "UInt32", "Int64", "UInt64", "Float32", "Float64",
                "Int16 BE", "UInt16 BE", "Int32 BE", "UInt32 BE", "Float32 BE"]


def find_all(data: bytearray, pat: bytes, limit: int = 20000) -> List[int]:
    out, pos = [], 0
    while len(out) < limit:
        i = data.find(pat, pos)
        if i < 0:
            break
        out.append(i)
        pos = i + 1
    return out


def diff_ranges(a: bytes, b: bytes, limit: int = 50000) -> List[Tuple[int, int]]:
    """Byte ranges where a and b differ (a longer/shorter tail counts as one range)."""
    out: List[Tuple[int, int]] = []
    n = min(len(a), len(b))
    B = 65536
    i = 0
    cur = None
    while i < n and len(out) < limit:
        j = min(n, i + B)
        if a[i:j] == b[i:j]:
            if cur is not None:
                out.append(cur)
                cur = None
            i = j
            continue
        for k in range(i, j):
            if a[k] != b[k]:
                if cur is None:
                    cur = [k, k + 1]
                elif cur[1] == k:
                    cur[1] = k + 1
                else:
                    out.append(tuple(cur))
                    cur = [k, k + 1]
        i = j
    if cur is not None:
        out.append(tuple(cur))
    if len(a) != len(b):
        out.append((n, max(len(a), len(b))))
    return [tuple(x) for x in out]


# ---------------------------------------------------------------- inspector
class InspectorPanel(QTableWidget):
    def __init__(self, parent=None):
        super().__init__(0, 2, parent)
        self.setHorizontalHeaderLabels(["Type", "Value at cursor"])
        self.horizontalHeader().setSectionResizeMode(0, self.horizontalHeader().ResizeMode.ResizeToContents)
        self.horizontalHeader().setStretchLastSection(True)
        self.verticalHeader().setVisible(False)
        self.verticalHeader().setDefaultSectionSize(24)
        self.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        self.setFont(QFont("Monospace", 9))

    def update_from(self, data, off: int):
        rows = []
        g = lambda n: bytes(data[off:off + n]) if off + n <= len(data) else None

        def add(name, fmt, size, be=False):
            b = g(size)
            if b is not None:
                v = struct.unpack((">" if be else "<") + fmt, b)[0]
                rows.append((name, f"{v:.6g}" if fmt in "fd" else (f"{v}  (0x{v & ((1 << 8 * size) - 1):X})")))
        add("Int8", "b", 1); add("UInt8", "B", 1)
        add("Int16", "h", 2); add("UInt16", "H", 2)
        add("Int32", "i", 4); add("UInt32", "I", 4)
        add("Int64", "q", 8); add("UInt64", "Q", 8)
        add("Float32", "f", 4); add("Float64", "d", 8)
        add("Int16 BE", "h", 2, True); add("UInt32 BE", "I", 4, True); add("Float32 BE", "f", 4, True)
        b = g(2)
        if b is not None:
            rows.append(("Fixed /128 (COL vertex)", f"{struct.unpack('<h', b)[0] / 128.0:.5f}"))
        b = g(4)
        if b is not None:
            rows.append(("Bytes", b.hex(" ").upper()))
        b = g(1)
        if b is not None:
            rows.append(("Binary", f"{b[0]:08b}"))
        s = bytes(data[off:off + 24])
        rows.append(("ASCII", s.split(b"\0", 1)[0].decode("latin1", "replace")))
        v = g(4)
        if v is not None:
            st = struct.unpack("<I", v)[0]
            rows.append(("RW version stamp", f"{rw.version_text(st)}" if st else "-"))
        self.setRowCount(len(rows))
        for r, (a, b2) in enumerate(rows):
            self.setItem(r, 0, QTableWidgetItem(a))
            self.setItem(r, 1, QTableWidgetItem(b2))
        self.resizeColumnToContents(0)


# ---------------------------------------------------------------- structure
_KIND_COLOURS = {"complex": QColor(70, 130, 220), "data": QColor(80, 190, 110),
                 "empty": QColor(235, 150, 50), "faulty": QColor(230, 70, 70)}

RW_VERSIONS = [("GTA III  (3.1.0.1)", 0x31001), ("GTA III/VC PC (3.3.0.2)", 0x33002),
               ("GTA VC PC (3.4.0.3)", 0x34003), ("GTA SA PC (3.6.0.3)", 0x36003),
               ("RW 3.5.0.0", 0x35000), ("RW 3.7.0.2", 0x37002)]


class StructurePanel(QWidget):
    goto = pyqtSignal(int, int)                   # offset, length
    apply_bytes = pyqtSignal(bytes, str)          # new whole-file bytes, undo label

    def __init__(self, parent=None):
        super().__init__(parent)
        lay = QVBoxLayout(self)
        lay.setContentsMargins(2, 2, 2, 2)
        top = QHBoxLayout()
        self.kind_lbl = QLabel("No structure")
        self.kind_lbl.setWordWrap(True)
        self.kind_lbl.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        top.addWidget(self.kind_lbl, 1)
        self.btn_validate = QPushButton("Validate")
        self.btn_validate.setMinimumHeight(28)
        self.btn_validate.clicked.connect(self.show_validation)
        top.addWidget(self.btn_validate)
        lay.addLayout(top)
        find = QHBoxLayout()
        self.find_type = QComboBox()
        self.find_type.setToolTip("Section type to find")
        find.addWidget(self.find_type, 1)
        b = QPushButton("Find next")
        b.setMinimumHeight(28)
        b.clicked.connect(self.find_next_type)
        find.addWidget(b)
        lay.addLayout(find)
        self.tree = QTreeWidget()
        self.tree.setHeaderLabels(["Section", "Offset", "Size", "RW version"])
        self.tree.setUniformRowHeights(True)
        self.tree.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.tree.customContextMenuRequested.connect(self._menu)
        self.tree.itemClicked.connect(self._clicked)
        lay.addWidget(self.tree, 1)
        self.fields = QTableWidget(0, 3)
        self.fields.setHorizontalHeaderLabels(["Field", "Value", "Offset"])
        self.fields.horizontalHeader().setStretchLastSection(True)
        self.fields.verticalHeader().setVisible(False)
        self.fields.itemChanged.connect(self._field_edited)
        self.fields.setVisible(False)
        lay.addWidget(self.fields, 1)
        self._fields_loading = False
        self.legend = QLabel("blue = has sections   green = data   orange = empty   red = faulty")
        self.legend.setStyleSheet("padding:2px;")
        self.legend.setWordWrap(True)
        lay.addWidget(self.legend)
        self.data = b""
        self.roots: List[rw.RWNode] = []
        self.mode = "none"
        self._clip: Optional[bytes] = None

    # -- build
    def rebuild(self, data: bytes, filename: str = ""):
        self.data = bytes(data)
        self.tree.clear()
        self.roots = []
        low = filename.lower()
        if low.endswith(".dir"):
            self._build_dir()
        elif data[:4] == b"VER2":
            self._build_img2()
        elif data[:4] in (b"COLL", b"COL2", b"COL3", b"COL4"):
            self._build_col()
        elif data[:4] == b"bnry":
            self._build_bnry()
        elif self._looks_rw():
            self._build_rw()
        else:
            self.mode = "none"
            self.kind_lbl.setText("No structure known for this file (RenderWare, COL, IMG and DIR are)")

    def _looks_rw(self) -> bool:
        if len(self.data) < 12:
            return False
        t, size, stamp = struct.unpack_from("<III", self.data, 0)
        return t in rw.CHUNK_NAMES and size + 12 <= len(self.data) + 16 and rw._plausible_stamp(stamp)

    def _item(self, parent, cols, off, length, colour=None):
        it = QTreeWidgetItem(parent if parent is not None else self.tree, cols)
        it.setData(0, Qt.ItemDataRole.UserRole, (off, length))
        if colour is not None:
            c = QColor(colour)
            c.setAlpha(90)
            for k in range(4):
                it.setBackground(k, QBrush(c))
        return it

    def _build_rw(self): #vers 2
        self.mode = "rw"
        self.roots = rw.parse_rw(self.data)
        types = sorted({n.type for r in self.roots for n in r.walk()})
        self.find_type.clear()
        for t in types:
            self.find_type.addItem(rw.chunk_name(t), t)
        self.kind_lbl.setText(f"RenderWare stream - {rw.version_text(self.roots[0].stamp) if self.roots else '?'}")

        def add(n: rw.RWNode, parent):
            it = self._item(parent, [n.name, f"0x{n.offset:08X}", str(n.size), rw.version_text(n.stamp)],
                            n.offset, 12 + max(0, n.size), _KIND_COLOURS.get(n.kind))
            it.setData(1, Qt.ItemDataRole.UserRole, n)
            if n.error:
                it.setToolTip(0, n.error)
            for c in n.children:
                add(c, it)
            return it
        for r in self.roots:
            add(r, None)
        self.tree.expandToDepth(1)
        self.tree.resizeColumnToContents(0)

    def _build_col(self):
        from apps.methods.col_splice import split_records
        self.mode = "col"
        recs, tail = split_records(self.data)
        self.kind_lbl.setText(f"COL file - {len(recs)} model(s)" + (f", {len(tail)} trailing byte(s)" if tail else ""))
        pos = 0
        for r in recs:
            name = r[8:30].split(b"\0", 1)[0].decode("latin1")
            self._item(None, [f"{name}  ({r[:4].decode()})", f"0x{pos:08X}", str(len(r) - 8), ""], pos, len(r))
            pos += len(r)
        if tail:
            self._item(None, ["Trailing bytes", f"0x{pos:08X}", str(len(tail)), ""], pos, len(tail), _KIND_COLOURS["empty"])

    def _build_img2(self):
        self.mode = "img"
        n = struct.unpack_from("<I", self.data, 4)[0]
        self.kind_lbl.setText(f"IMG version 2 - {n} entries")
        for i in range(min(n, 200000)):
            o = 8 + i * 32
            if o + 32 > len(self.data):
                break
            off, sz = struct.unpack_from("<II", self.data, o)
            name = self.data[o + 8:o + 32].split(b"\0", 1)[0].decode("latin1")
            ok = off * 2048 + sz * 2048 <= len(self.data)
            self._item(None, [name, f"0x{off * 2048:08X}", str(sz * 2048), ""], off * 2048, sz * 2048,
                       None if ok else _KIND_COLOURS["faulty"])

    def _build_dir(self):
        self.mode = "dir"
        n = len(self.data) // 32
        self.kind_lbl.setText(f"IMG directory (.dir) - {n} entries (offset/size in the .img)")
        for i in range(min(n, 200000)):
            off, sz = struct.unpack_from("<II", self.data, i * 32)
            name = self.data[i * 32 + 8:i * 32 + 32].split(b"\0", 1)[0].decode("latin1")
            self._item(None, [name, f"0x{off * 2048:08X}", str(sz * 2048), ""], i * 32, 32)

    def _build_bnry(self): #vers 2
        """SA binary IPL: offsets read from the 76-byte header."""
        self.mode = "bnry"
        if len(self.data) < 76:
            self.kind_lbl.setText("Binary IPL - header too short")
            return
        inst, cars = struct.unpack_from("<I", self.data, 4)[0], struct.unpack_from("<I", self.data, 20)[0]
        inst_off, cars_off = struct.unpack_from("<I", self.data, 28)[0], struct.unpack_from("<I", self.data, 60)[0]
        self.kind_lbl.setText(f"Binary IPL - {inst} instance(s), {cars} parked car(s)")
        self._item(None, ["Header 'bnry'", "0x00000000", "76", ""], 0, 76)
        for label, cnt, off, size in (("Instances (40 bytes each)", inst, inst_off, 40),
                                      ("Parked cars (48 bytes each)", cars, cars_off, 48)):
            if cnt:
                bad = off + cnt * size > len(self.data)
                self._item(None, [label, f"0x{off:08X}", str(cnt * size), ""], off, cnt * size,
                           _KIND_COLOURS["faulty"] if bad else None)

    # -- interaction
    def _clicked(self, item, _col): #vers 2
        v = item.data(0, Qt.ItemDataRole.UserRole)
        if v:
            self.goto.emit(*v)
        self._show_fields(self.node_of(item))

    def _show_fields(self, n): #vers 1
        """Named, editable fields for known Struct sections (Clump, Frame List, Geometry...)."""
        from apps.methods.rw_structs import decode_struct
        rows = []
        if n is not None and n.type == 0x01 and n.parent is not None:
            rows = decode_struct(self.data, n, n.parent.type, rw.version_of(n.stamp))
        self._fields_loading = True
        self.fields.setRowCount(len(rows))
        for r, (name, off, fmt, val) in enumerate(rows):
            a = QTableWidgetItem(name)
            a.setFlags(a.flags() & ~Qt.ItemFlag.ItemIsEditable)
            b = QTableWidgetItem(f"{val:.6g}" if isinstance(val, float) else str(val))
            b.setData(Qt.ItemDataRole.UserRole, (off, fmt))
            c = QTableWidgetItem(f"0x{off:08X}")
            c.setFlags(c.flags() & ~Qt.ItemFlag.ItemIsEditable)
            self.fields.setItem(r, 0, a); self.fields.setItem(r, 1, b); self.fields.setItem(r, 2, c)
        self.fields.resizeColumnToContents(0)
        self.fields.setVisible(bool(rows))
        self._fields_loading = False

    def _field_edited(self, item): #vers 1
        """Write an edited field back as one undoable edit."""
        if self._fields_loading or item.column() != 1:
            return
        from apps.methods.rw_structs import encode_field
        off, fmt = item.data(Qt.ItemDataRole.UserRole)
        try:
            raw = encode_field(fmt, item.text().strip())
        except (ValueError, struct.error) as e:
            QMessageBox.warning(self, "Field", str(e))
            return
        buf = bytearray(self.data)
        buf[off:off + len(raw)] = raw
        name = self.fields.item(item.row(), 0).text()
        self._emit(bytes(buf), f"Set {name}")

    def select_offset(self, off: int): #vers 1
        """Select the deepest section containing a byte offset (hex cursor -> tree)."""
        if self.mode != "rw" or not self.roots:
            return
        n = rw.node_at(self.roots, off)
        if n is None:
            return
        cur = self.node_of(self.tree.currentItem())
        if cur is not None and cur.offset == n.offset and cur.type == n.type:
            return
        from PyQt6.QtWidgets import QTreeWidgetItemIterator
        it = QTreeWidgetItemIterator(self.tree)
        while it.value():
            node = self.node_of(it.value())
            if node is not None and node.offset == n.offset and node.type == n.type:
                self.tree.blockSignals(True)
                self.tree.setCurrentItem(it.value())
                self.tree.scrollToItem(it.value())
                self.tree.blockSignals(False)
                self._show_fields(node)
                return
            it += 1

    def find_next_type(self): #vers 1
        """Select the next section of the chosen type after the current one."""
        t = self.find_type.currentData()
        if t is None:
            return
        nodes = [n for r in self.roots for n in r.walk() if n.type == t]
        if not nodes:
            return
        cur = self.node_of(self.tree.currentItem())
        after = [n for n in nodes if cur is None or n.offset > cur.offset]
        n = (after or nodes)[0]
        self.select_offset(n.offset)
        self.goto.emit(n.offset, n.end - n.offset)

    def show_validation(self):
        if self.mode != "rw":
            QMessageBox.information(self, "Validate", "Validation reports are available for RenderWare streams.")
            return
        prob = rw.validate(self.roots)
        QMessageBox.information(self, "Validate",
                                "No problems found." if not prob else f"{len(prob)} problem(s):\n\n" + "\n".join(prob[:40]))

    def node_of(self, item) -> Optional[rw.RWNode]:
        return item.data(1, Qt.ItemDataRole.UserRole) if item else None

    def _menu(self, pos): #vers 3
        it = self.tree.itemAt(pos)
        m = QMenu(self)
        if it is not None:
            v = it.data(0, Qt.ItemDataRole.UserRole)
            if v:
                m.addAction("Select these bytes", lambda: self.goto.emit(*v))
            n = self.node_of(it)
            if n is not None:
                m.addSeparator()
                m.addAction("Export section...", lambda: self._export(n))
                m.addAction("Copy section", lambda: self._copy(n))
                m.addAction("Import section after this one...", lambda: self._import_after(n))
                if self._clip:
                    m.addAction("Paste copied section after this one", lambda: self._paste_after(n))
                if n.type in rw.CONTAINERS:
                    m.addAction("Import section as first child...", lambda: self._import_child(n))
                    if self._clip:
                        m.addAction("Paste copied section as first child", lambda: self._paste_child(n))
                m.addAction("Move up", lambda: self._emit(rw.move_section(self.data, n, -1), f"Move {n.name} up"))
                m.addAction("Move down", lambda: self._emit(rw.move_section(self.data, n, 1), f"Move {n.name} down"))
                m.addAction("Add empty section after...", lambda: self._add_empty(n, first_child=False))
                if n.type in rw.CONTAINERS:
                    m.addAction("Add empty section as first child...", lambda: self._add_empty(n, first_child=True))
                if n.type in (0x02, 0x06, 0x15):
                    m.addAction("Rename...", lambda: self._rename(n))
                m.addSeparator()
                m.addAction("Clear payload (keep header)", lambda: self._emit(rw.clear_section(self.data, n), f"Clear {n.name}"))
                m.addAction("Delete section", lambda: self._delete(n))
        if self.mode == "rw":
            m.addSeparator()
            m.addAction("Recompute all section sizes", self.recompute)
            m.addAction("Change RW version...", self.change_version)
            m.addAction("Append a file's sections...", self.append_file)
            m.addAction("Copy tree as text", self.copy_text)
            m.addAction("Texture names...", self.show_textures)
        if not m.isEmpty():
            m.exec(self.tree.viewport().mapToGlobal(pos))

    def _emit(self, new: bytes, label: str):
        self.apply_bytes.emit(new, label)

    def _delete(self, n):
        if QMessageBox.question(self, "Delete section", f"Delete '{n.name}' ({12 + n.size} bytes) and everything inside it?") \
                == QMessageBox.StandardButton.Yes:
            self._emit(rw.delete_section(self.data, n), f"Delete {n.name}")

    def _export(self, n):
        p, _ = QFileDialog.getSaveFileName(self, "Export section", f"{n.name.replace(' ', '_')}.bin", "All files (*)")
        if p:
            from apps.methods.file_backup import safe_write_bytes
            safe_write_bytes(p, rw.export_section(self.data, n))

    def _copy(self, n):
        self._clip = rw.export_section(self.data, n)

    def _paste_after(self, n):
        if self._clip:
            self._emit(rw.insert_section(self.data, n, self._clip), "Paste section")

    def _import_after(self, n):
        p, _ = QFileDialog.getOpenFileName(self, "Import section (bytes of one whole section)", "", "All files (*)")
        if p:
            self._emit(rw.insert_section(self.data, n, open(p, "rb").read()), "Import section")

    def _paste_child(self, n): #vers 1
        """Paste copied section as first child of container n."""
        if self._clip:
            self._emit(rw.insert_section(self.data, None, self._clip, parent=n), "Paste section")

    def _import_child(self, n): #vers 1
        """Import a section file as first child of container n."""
        p, _ = QFileDialog.getOpenFileName(self, "Import section (bytes of one whole section)", "", "All files (*)")
        if p:
            with open(p, "rb") as f:
                self._emit(rw.insert_section(self.data, None, f.read(), parent=n), "Import section")

    def _add_empty(self, n, first_child): #vers 1
        """Insert an empty section of a chosen type (stamp copied from n)."""
        names = sorted((v, k) for k, v in rw.CHUNK_NAMES.items())
        pick, ok = QInputDialog.getItem(self, "Add empty section", "Section type:", [f"{v}  (0x{k:X})" for v, k in names], 0, False)
        if not ok:
            return
        t = names[[f"{v}  (0x{k:X})" for v, k in names].index(pick)][1]
        chunk = rw.make_section(t, n.stamp)
        if first_child:
            self._emit(rw.insert_section(self.data, None, chunk, parent=n), f"Add {rw.chunk_name(t)}")
        else:
            self._emit(rw.insert_section(self.data, n, chunk), f"Add {rw.chunk_name(t)}")

    def _rename(self, n): #vers 1
        """Rename a String, a Texture (its name String) or a Texture Native (fixed 32-byte name)."""
        if n.type == 0x15:
            st = next((c for c in n.children if c.type == 0x01), None)
            if st is None or st.size < 40:
                return
            o = st.data_start + 8
            cur = bytes(self.data[o:o + 32]).split(b"\0", 1)[0].decode('latin-1')
            txt, ok = QInputDialog.getText(self, "Rename texture", "Name (max 31):", text=cur)
            if ok and txt.strip():
                from apps.methods.rw_structs import encode_field
                try:
                    raw = encode_field('s32', txt.strip())
                except ValueError as e:
                    QMessageBox.warning(self, "Rename", str(e))
                    return
                buf = bytearray(self.data)
                buf[o:o + 32] = raw
                self._emit(bytes(buf), f"Rename texture {cur} -> {txt.strip()}")
            return
        target = n if n.type == 0x02 else next((c for c in n.children if c.type == 0x02), None)
        if target is None:
            return
        cur = bytes(self.data[target.data_start:target.end]).split(b"\0", 1)[0].decode('latin-1')
        txt, ok = QInputDialog.getText(self, "Rename", "Name:", text=cur)
        if ok and txt.strip() and txt.strip() != cur:
            self._emit(rw.replace_payload(self.data, target, rw.string_payload(txt.strip())),
                       f"Rename {cur} -> {txt.strip()}")

    def recompute(self): #vers 1
        """Recompute every RW section size."""
        if self.mode == "rw":
            self._emit(rw.recompute_sizes(self.data), "Recompute sizes")

    def change_version(self):
        if self.mode != "rw":
            return
        names = [n for n, _ in RW_VERSIONS] + ["Custom..."]
        cur = rw.version_text(self.roots[0].stamp) if self.roots else "?"
        pick, ok = QInputDialog.getItem(self, "Change RW version", f"Stream is {cur}. Set every section to:", names, 3, False)
        if not ok:
            return
        if pick == "Custom...":
            txt, ok = QInputDialog.getText(self, "Custom version", "Version like 3.6.0.3:")
            if not ok:
                return
            try:
                a, b, c, d = (int(x) for x in txt.split("."))
                ver = (a << 16) | (b << 12) | (c << 8) | d
            except ValueError:
                QMessageBox.warning(self, "Version", "Use the form 3.6.0.3")
                return
        else:
            ver = dict(RW_VERSIONS)[pick]
        new, n = rw.set_stream_version(self.data, ver)
        self._emit(new, f"RW version -> {rw.version_text(rw.stamp_for(ver))} ({n} sections)")

    def append_file(self):
        p, _ = QFileDialog.getOpenFileName(self, "Append the sections of a RenderWare file", "", "All files (*)")
        if not p:
            return
        add = open(p, "rb").read()
        if self.roots and rw.parse_rw(add):
            new, _ = rw.set_stream_version(add, rw.version_of(self.roots[0].stamp))     # match this stream's version
            self._emit(self.data + new, "Append file")

    def copy_text(self):
        from PyQt6.QtWidgets import QApplication
        QApplication.clipboard().setText(rw.dump_tree_text(self.roots))

    def show_textures(self):
        names = rw.texture_names(self.data)
        QMessageBox.information(self, "Textures", "\n".join(names) if names else "No textures found.")


# ---------------------------------------------------------------- search
class SearchPanel(QWidget):
    find_requested = pyqtSignal(bytes, bool)          # pattern, forward
    all_requested = pyqtSignal(bytes)
    replace_requested = pyqtSignal(bytes, bytes, bool)  # find, replace, all
    goto = pyqtSignal(int, int)

    def __init__(self, parent=None):
        super().__init__(parent)
        lay = QVBoxLayout(self)
        lay.setContentsMargins(2, 2, 2, 2)
        r = QHBoxLayout()
        self.kind = QComboBox()
        self.kind.addItems(SEARCH_KINDS)
        r.addWidget(self.kind)
        lay.addLayout(r)
        self.find = QLineEdit()
        self.find.setPlaceholderText("Find...")
        self.find.returnPressed.connect(lambda: self._emit_find(True))
        lay.addWidget(self.find)
        self.repl = QLineEdit()
        self.repl.setPlaceholderText("Replace with...")
        lay.addWidget(self.repl)
        b = QHBoxLayout()
        for label, fn in (("Prev", lambda: self._emit_find(False)), ("Next", lambda: self._emit_find(True)),
                          ("All", self._emit_all)):
            x = QPushButton(label)
            x.setMinimumHeight(28)
            x.clicked.connect(fn)
            b.addWidget(x)
        lay.addLayout(b)
        b2 = QHBoxLayout()
        for label, all_ in (("Replace", False), ("Replace all", True)):
            x = QPushButton(label)
            x.setMinimumHeight(28)
            x.clicked.connect(lambda _c=False, a=all_: self._emit_replace(a))
            b2.addWidget(x)
        lay.addLayout(b2)
        self.results = QListWidget()
        self.results.itemClicked.connect(lambda it: self.goto.emit(*it.data(Qt.ItemDataRole.UserRole)))
        lay.addWidget(self.results, 1)
        self.info = QLabel("")
        self.info.setWordWrap(True)
        lay.addWidget(self.info)

    def pattern(self, text_edit=None) -> Optional[bytes]:
        try:
            return parse_search_pattern(self.kind.currentText(), (text_edit or self.find).text())
        except Exception as e:
            self.info.setText(str(e))
            return None

    def _emit_find(self, fwd):
        p = self.pattern()
        if p:
            self.find_requested.emit(p, fwd)

    def _emit_all(self):
        p = self.pattern()
        if p:
            self.all_requested.emit(p)

    def _emit_replace(self, all_):
        p = self.pattern()
        r = self.pattern(self.repl) if self.repl.text() else b""
        if p is not None and r is not None:
            self.replace_requested.emit(p, r, all_)

    def show_hits(self, hits: List[int], length: int):
        self.results.clear()
        for h in hits[:2000]:
            it = QListWidgetItem(f"0x{h:08X}")
            it.setData(Qt.ItemDataRole.UserRole, (h, length))
            self.results.addItem(it)
        self.info.setText(f"{len(hits)} match(es)" + (" (first 2000 listed)" if len(hits) > 2000 else ""))


# ---------------------------------------------------------------- bookmarks
class BookmarksPanel(QWidget):
    goto = pyqtSignal(int, int)
    changed = pyqtSignal()

    def __init__(self, parent=None):
        super().__init__(parent)
        lay = QVBoxLayout(self)
        lay.setContentsMargins(2, 2, 2, 2)
        self.list = QListWidget()
        self.list.itemClicked.connect(lambda it: self.goto.emit(it.data(Qt.ItemDataRole.UserRole), 0))
        lay.addWidget(self.list, 1)
        b = QPushButton("Remove selected")
        b.setMinimumHeight(28)
        b.clicked.connect(self._remove)
        lay.addWidget(b)
        self.marks = {}

    def add(self, off: int, label: str):
        self.marks[off] = label
        self._refresh()

    def _remove(self):
        it = self.list.currentItem()
        if it:
            self.marks.pop(it.data(Qt.ItemDataRole.UserRole), None)
            self._refresh()

    def _refresh(self):
        self.list.clear()
        for off in sorted(self.marks):
            it = QListWidgetItem(f"0x{off:08X}  {self.marks[off]}")
            it.setData(Qt.ItemDataRole.UserRole, off)
            self.list.addItem(it)
        self.changed.emit()


# ---------------------------------------------------------------- compare
class ComparePanel(QWidget):
    goto = pyqtSignal(int, int)
    diffs_changed = pyqtSignal(list)

    def __init__(self, parent=None):
        super().__init__(parent)
        lay = QVBoxLayout(self)
        lay.setContentsMargins(2, 2, 2, 2)
        self.label = QLabel("Compare this file with another file")
        self.label.setWordWrap(True)
        lay.addWidget(self.label)
        b = QPushButton("Pick file to compare...")
        b.setMinimumHeight(28)
        b.clicked.connect(self.pick)
        lay.addWidget(b)
        self.list = QListWidget()
        self.list.itemClicked.connect(lambda it: self.goto.emit(*it.data(Qt.ItemDataRole.UserRole)))
        lay.addWidget(self.list, 1)
        self.get_data = lambda: b""
        self.ranges: List[Tuple[int, int]] = []

    def pick(self):
        p, _ = QFileDialog.getOpenFileName(self, "Compare with", "", "All files (*)")
        if p:
            self.compare(open(p, "rb").read(), p)

    def compare(self, other: bytes, name: str = ""):
        a = bytes(self.get_data())
        self.ranges = diff_ranges(a, other)
        self.list.clear()
        for s, e in self.ranges[:5000]:
            it = QListWidgetItem(f"0x{s:08X} - 0x{e:08X}  ({e - s} byte(s))")
            it.setData(Qt.ItemDataRole.UserRole, (s, e - s))
            self.list.addItem(it)
        tot = sum(e - s for s, e in self.ranges)
        self.label.setText(f"vs {name.split('/')[-1]}: " + ("identical" if not self.ranges else
                           f"{len(self.ranges)} difference range(s), {tot} byte(s)"
                           + (f" (sizes {len(a)} / {len(other)})" if len(a) != len(other) else "")))
        self.diffs_changed.emit(self.ranges)

    def clear(self):
        self.ranges = []
        self.list.clear()
        self.diffs_changed.emit([])
        self.label.setText("Compare this file with another file")


def hashes(data: bytes) -> dict:
    return {"CRC32": f"{zlib.crc32(data) & 0xFFFFFFFF:08X}", "MD5": hashlib.md5(data).hexdigest(),
            "SHA1": hashlib.sha1(data).hexdigest(), "SHA256": hashlib.sha256(data).hexdigest()}


# ---------------------------------------------------------------- conversion
class ConvertDialog(QWidget):
    """Convert the open file (or a batch of files) between GTA III / VC / SA formats.
    DFF / TXD: RenderWare version + layout changes; COL: COL1 / COL2 / COL3."""
    convert_open = pyqtSignal(bytes, str)        # converted bytes for the open file, report

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowFlag(Qt.WindowType.Window, True)
        self.setWindowTitle("Convert DFF / TXD / COL")
        self.resize(640, 560)
        lay = QVBoxLayout(self)
        self.head = QLabel("")
        self.head.setWordWrap(True)
        lay.addWidget(self.head)
        row = QHBoxLayout()
        row.addWidget(QLabel("Convert to:"))
        self.target = QComboBox()
        row.addWidget(self.target, 1)
        lay.addLayout(row)
        self.opt_strip = QCheckBox("DFF: remove Rockstar sections the target game does not have (recommended)")
        self.opt_strip.setChecked(True)
        self.opt_clamp = QCheckBox("COL: clamp vertices that do not fit COL2/COL3 (16-bit, +-255.99)")
        lay.addWidget(self.opt_strip)
        lay.addWidget(self.opt_clamp)
        self.report = QListWidget()
        lay.addWidget(self.report, 1)
        b = QHBoxLayout()
        self.b_open = QPushButton("Convert the open file")
        self.b_copy = QPushButton("Convert and save a copy...")
        self.b_batch = QPushButton("Batch: convert several files...")
        for x in (self.b_open, self.b_copy, self.b_batch):
            x.setMinimumHeight(30)
            b.addWidget(x)
        lay.addLayout(b)
        self.b_open.clicked.connect(self._open)
        self.b_copy.clicked.connect(self._copy)
        self.b_batch.clicked.connect(self._batch)
        self.data = b""
        self.kind = None

    def start(self, data: bytes, name: str):
        from apps.methods import rw_convert as cv
        self.data, self.kind = bytes(data), cv.detect_kind(bytes(data))
        self.report.clear()
        self.target.clear()
        if self.kind:
            self.head.setText(f"<b>{name}</b> - {self.kind.upper()}, {cv.current_version(self.data, self.kind)}")
            for label, v in cv.targets_for(self.kind, self.data):
                self.target.addItem(label, v)
        else:
            self.head.setText(f"<b>{name}</b> - not a DFF, TXD or COL (batch conversion still works)")
        self.opt_strip.setVisible(self.kind in (None, "dff"))
        self.opt_clamp.setVisible(self.kind in (None, "col"))
        self.b_open.setEnabled(bool(self.kind))
        self.b_copy.setEnabled(bool(self.kind))
        self.show()
        self.raise_()

    def _run(self, data: bytes, kind: str, target):
        from apps.methods import rw_convert as cv
        if kind == "dff":
            return cv.convert_dff(data, target, self.opt_strip.isChecked())
        if kind == "txd":
            return cv.convert_txd(data, target)
        return cv.convert_col(data, target, self.opt_clamp.isChecked())

    def _show(self, lines):
        self.report.clear()
        self.report.addItems(lines)

    def _open(self):
        from apps.methods import rw_convert as cv
        try:
            new, rep = self._run(self.data, self.kind, self.target.currentData())
        except cv.ConvertError as e:
            self._show(["NOT CONVERTED", *str(e).split("\n")])
            return
        self._show(rep + ["", "Applied to the open file (undo with Ctrl+Z; nothing is saved until you Save)."])
        self.convert_open.emit(new, "; ".join(rep[:1]))

    def _copy(self):
        from apps.methods import rw_convert as cv
        from apps.methods.file_backup import safe_write_bytes
        try:
            new, rep = self._run(self.data, self.kind, self.target.currentData())
        except cv.ConvertError as e:
            self._show(["NOT CONVERTED", *str(e).split("\n")])
            return
        p, _ = QFileDialog.getSaveFileName(self, "Save converted copy", "", "All files (*)")
        if p:
            safe_write_bytes(p, new)
            self._show(rep + ["", f"Saved {p}"])

    def _batch(self):
        from apps.methods import rw_convert as cv
        from apps.methods.file_backup import safe_write_bytes
        import os
        files, _ = QFileDialog.getOpenFileNames(self, "Files to convert (all the same kind: DFF, TXD or COL)", "",
                                                "DFF / TXD / COL (*.dff *.txd *.col);;All files (*)")
        if not files:
            return
        kinds = {os.path.splitext(f)[1].lower() for f in files}
        if len(kinds) != 1:
            self._show(["Pick files of one kind at a time (all .dff, all .txd or all .col)."])
            return
        kind = {".dff": "dff", ".txd": "txd", ".col": "col"}.get(kinds.pop())
        if not kind:
            self._show(["Not a DFF, TXD or COL file."])
            return
        first = open(files[0], "rb").read()
        opts = cv.targets_for(kind, first)
        names = [n for n, _ in opts]
        pick, ok = QInputDialog.getItem(self, "Batch convert", f"{len(files)} {kind.upper()} file(s) to:", names, 0, False)
        if not ok:
            return
        target = dict(opts)[pick]
        outdir = QFileDialog.getExistingDirectory(self, "Folder for the converted copies (not the source folder)")
        if not outdir:
            return
        rep, done = [], 0
        for f in files:
            dst = os.path.join(outdir, os.path.basename(f))
            if os.path.abspath(dst) == os.path.abspath(f):
                rep.append(f"{os.path.basename(f)}: skipped - output folder is the source folder")
                continue
            try:
                new, r = self._run(open(f, "rb").read(), kind, target)
                safe_write_bytes(dst, new)
                done += 1
                rep.append(f"{os.path.basename(f)}: ok  ({r[-1]})")
            except cv.ConvertError as e:
                rep.append(f"{os.path.basename(f)}: NOT converted - {str(e).splitlines()[0]}")
            except Exception as e:
                rep.append(f"{os.path.basename(f)}: error - {e}")
        self._show([f"{done} of {len(files)} converted into {outdir}", ""] + rep)
