#this belongs in apps/components/Hex_Editor/hex_canvas.py - Version: 1
# X-Seti - September 2026 - IMG Factory 1.6 - Hex Workshop editing canvas

"""hex_canvas.py - The byte document and the paint-on-demand hex view of the Hex Workshop.
HexDoc  : bytearray + undo/redo journal (replace / insert / delete / replace-all) + a list of
          modified ranges (kept in step with inserts and deletes) for the orange highlight.
HexCanvas: QAbstractScrollArea that paints ONLY the visible rows, so multi-hundred-MB files open
          instantly. Selection by mouse/keyboard, hex-nibble and ASCII typing (overwrite or
          insert), copy/cut/paste (hex text or raw text), highlight layers (chunk regions,
          search hits, differences, bookmarks)."""

##Methods list -
# HexDoc
# HexCanvas

from typing import Dict, List, Optional, Tuple

from PyQt6.QtCore import Qt, QRect, QPoint, pyqtSignal, QObject
from PyQt6.QtGui import QColor, QFont, QFontMetrics, QPainter, QPen, QKeySequence
from PyQt6.QtWidgets import QAbstractScrollArea, QApplication, QMenu

_HEX = "0123456789abcdefABCDEF"


class HexDoc(QObject):
    changed = pyqtSignal()

    LIMIT = 1000                              # undo steps kept

    def __init__(self, data: bytes = b"", parent=None):
        super().__init__(parent)
        self.data = bytearray(data)
        self.undo_stack: list = []
        self.redo_stack: list = []
        self.mod: List[List[int]] = []       # [start, end) ranges changed since load/save
        self.saved_version = 0
        self.version = 0

    # -- state
    @property
    def modified(self) -> bool:
        return self.version != self.saved_version

    def mark_saved(self):
        self.saved_version = self.version
        self.mod = []
        self.changed.emit()

    def __len__(self):
        return len(self.data)

    # -- modified ranges
    def _add_mod(self, a: int, b: int):
        if b <= a:
            return
        self.mod.append([a, b])
        self.mod.sort()
        merged = [self.mod[0]]
        for s, e in self.mod[1:]:
            if s <= merged[-1][1]:
                merged[-1][1] = max(merged[-1][1], e)
            else:
                merged.append([s, e])
        self.mod = merged

    def _shift_mod(self, at: int, delta: int):
        out = []
        for s, e in self.mod:
            if delta < 0 and s < at - delta and e > at:          # part removed
                s2, e2 = (s if s < at else at), (e + delta if e > at - delta else at)
                if e2 > s2:
                    out.append([s2, e2])
                continue
            out.append([s + delta if s >= at else s, e + delta if e >= at else e])
        self.mod = out

    def is_mod(self, off: int) -> bool:
        for s, e in self.mod:
            if s <= off < e:
                return True
            if s > off:
                break
        return False

    # -- raw ops (used by do/undo)
    def _apply(self, op):
        kind = op[0]
        if kind == "rep":
            _, off, old, new = op
            self.data[off:off + len(new)] = new
            self._add_mod(off, off + len(new))
        elif kind == "ins":
            _, off, new = op
            self.data[off:off] = new
            self._shift_mod(off, len(new))
            self._add_mod(off, off + len(new))
        elif kind == "del":
            _, off, old = op
            del self.data[off:off + len(old)]
            self._shift_mod(off, -len(old))
        elif kind == "all":
            _, old, new = op
            self.data[:] = new
            # mark only what really differs
            if len(old) == len(new):
                B, cur = 65536, None
                for i in range(0, len(new), B):
                    if old[i:i + B] == new[i:i + B]:
                        continue
                    for k in range(i, min(len(new), i + B)):
                        if old[k] != new[k]:
                            if cur is not None and cur[1] == k:
                                cur[1] = k + 1
                            else:
                                if cur is not None:
                                    self._add_mod(*cur)
                                cur = [k, k + 1]
                if cur is not None:
                    self._add_mod(*cur)
            else:                                   # bytes inserted / removed: mark the changed middle only
                a = 0
                m = min(len(old), len(new))
                while a < m and old[a] == new[a]:
                    a += 1
                b = 0
                while b < m - a and old[-1 - b] == new[-1 - b]:
                    b += 1
                end = len(new) - b
                self._add_mod(a, end if end > a else min(a + 1, len(new)))

    @staticmethod
    def _inverse(op):
        kind = op[0]
        if kind == "rep":
            return ("rep", op[1], op[3], op[2])
        if kind == "ins":
            return ("del", op[1], op[2])
        if kind == "del":
            return ("ins", op[1], op[2])
        return ("all", op[2], op[1])

    def _do(self, op, record=True):
        self._apply(op)
        if record:
            self.undo_stack.append(op)
            del self.undo_stack[:-self.LIMIT]
            self.redo_stack.clear()
        self.version += 1
        self.changed.emit()

    # -- public edits
    def replace(self, off: int, new: bytes, merge: bool = False):
        """Overwrite bytes. merge=True folds a one-byte change into the previous one-byte
        change at the same offset (typing two hex digits = ONE undo step)."""
        new = bytes(new)
        if merge and len(new) == 1 and self.undo_stack:
            last = self.undo_stack[-1]
            if last[0] == "rep" and last[1] == off and len(last[3]) == 1 and self.data[off:off + 1] == last[3]:
                self.data[off:off + 1] = new
                self.undo_stack[-1] = ("rep", off, last[2], new)
                self._add_mod(off, off + 1)
                self.version += 0
                self.redo_stack.clear()
                self.changed.emit()
                return
        old = bytes(self.data[off:off + len(new)])
        if len(old) != len(new):                       # runs off the end: append the rest
            self.insert(off + len(old), new[len(old):])
            new = new[:len(old)]
            if not new:
                return
        if old != new:
            self._do(("rep", off, old, new))

    def insert(self, off: int, new: bytes):
        if new:
            self._do(("ins", off, bytes(new)))

    def delete(self, off: int, length: int):
        old = bytes(self.data[off:off + length])
        if old:
            self._do(("del", off, old))

    def replace_all(self, new: bytes):
        new = bytes(new)
        old = bytes(self.data)
        if new != old:
            self._do(("all", old, new))

    def undo(self) -> bool:
        if not self.undo_stack:
            return False
        op = self.undo_stack.pop()
        self.redo_stack.append(op)
        self._apply(self._inverse(op))
        self.version -= 1
        if self.version == self.saved_version:
            self.mod = []
        self.changed.emit()
        return True

    def redo(self) -> bool:
        if not self.redo_stack:
            return False
        op = self.redo_stack.pop()
        self.undo_stack.append(op)
        self._apply(op)
        self.version += 1
        if self.version == self.saved_version:
            self.mod = []
        self.changed.emit()
        return True


class HexCanvas(QAbstractScrollArea):
    cursor_changed = pyqtSignal(int)
    selection_changed = pyqtSignal(int, int)          # start, length
    status = pyqtSignal(str)

    def __init__(self, doc: HexDoc, parent=None):
        super().__init__(parent)
        self.doc = doc
        doc.changed.connect(self._on_doc_changed)
        self.bpr = 16
        self.cur = 0                 # cursor byte
        self.nib = 0                 # 0 = high nibble next, 1 = low
        self.anchor: Optional[int] = None
        self.pane = "hex"            # hex | ascii
        self.insert_mode = False
        self.offset_hex = True
        self.base_offset = 0
        self.regions: List[Tuple[int, int, QColor]] = []      # chunk / structure highlight
        self.hits: List[Tuple[int, int]] = []                 # search results
        self.diffs: List[Tuple[int, int]] = []
        self.marks: Dict[int, str] = {}
        self.setFont(QFont("Monospace", 10))
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)
        self.viewport().setCursor(Qt.CursorShape.IBeamCursor)
        self.verticalScrollBar().valueChanged.connect(lambda _v: self.viewport().update())
        self.horizontalScrollBar().valueChanged.connect(lambda _v: self.viewport().update())
        self._recalc()

    # -- metrics / layout
    def _recalc(self):
        fm = QFontMetrics(self.font())
        self.cw, self.lh = fm.horizontalAdvance("0"), fm.height() + 2
        self.addr_w = self.cw * 10
        self.hex_x = self.addr_w + self.cw
        gaps = (self.bpr - 1) // 8
        self.hex_w = self.cw * (self.bpr * 3 + gaps)
        self.asc_x = self.hex_x + self.hex_w + self.cw * 2
        self.total_w = self.asc_x + self.cw * self.bpr + self.cw
        self.rows = max(1, (len(self.doc) + self.bpr) // self.bpr)   # +1 row so the end position is reachable
        vp = self.viewport().height() // self.lh
        self.verticalScrollBar().setRange(0, max(0, self.rows - max(1, vp) + 1))
        self.verticalScrollBar().setPageStep(max(1, vp - 1))
        self.horizontalScrollBar().setRange(0, max(0, self.total_w - self.viewport().width()))
        self.horizontalScrollBar().setPageStep(self.viewport().width())

    def set_bytes_per_row(self, n: int):
        self.bpr = max(4, n)
        self._recalc()
        self.viewport().update()

    def resizeEvent(self, e):
        super().resizeEvent(e)
        self._recalc()

    def _on_doc_changed(self):
        self.cur = max(0, min(self.cur, len(self.doc)))
        self.anchor = None if self.anchor is not None and self.anchor > len(self.doc) else self.anchor
        self._recalc()
        self.viewport().update()

    def set_font_size(self, pt: int):
        f = QFont(self.font())
        f.setPointSize(pt)
        self.setFont(f)
        self._recalc()
        self.viewport().update()

    # -- selection helpers
    def selection(self) -> Tuple[int, int]:
        """(start, length); length 0 when nothing is selected."""
        if self.anchor is None or self.anchor == self.cur:
            return self.cur, 0
        a, b = sorted((self.anchor, self.cur))
        return a, b - a

    def selected_bytes(self) -> bytes:
        a, n = self.selection()
        return bytes(self.doc.data[a:a + n])

    def select_range(self, start: int, length: int):
        self.anchor, self.cur = start, min(len(self.doc), start + length)
        self.nib = 0
        self._ensure_visible(self.cur)
        self._notify()

    def goto(self, off: int, length: int = 0):
        off = max(0, min(off, len(self.doc)))
        self.anchor = off if length else None
        self.cur = min(len(self.doc), off + length) if length else off
        self.nib = 0
        self._ensure_visible(off)
        self._notify()

    def _notify(self):
        self.viewport().update()
        self.cursor_changed.emit(self.cur)
        self.selection_changed.emit(*self.selection())

    def _ensure_visible(self, off: int):
        row = off // self.bpr
        top = self.verticalScrollBar().value()
        vis = max(1, self.viewport().height() // self.lh)
        if row < top:
            self.verticalScrollBar().setValue(row)
        elif row >= top + vis - 1:
            self.verticalScrollBar().setValue(max(0, row - vis + 3))

    # -- geometry
    def _pos_to_offset(self, p: QPoint) -> Tuple[int, str]:
        x = p.x() + self.horizontalScrollBar().value()
        row = self.verticalScrollBar().value() + max(0, p.y()) // self.lh
        if x >= self.asc_x - self.cw:
            col, pane = min(self.bpr - 1, max(0, (x - self.asc_x) // self.cw)), "ascii"
        else:
            rel = max(0, x - self.hex_x)
            grp = rel // (self.cw * 25)                      # 8 bytes * 3 chars + 1 gap
            within = (rel - grp * self.cw * 25) // (self.cw * 3)
            col, pane = min(self.bpr - 1, grp * 8 + min(7, within)), "hex"
        return min(len(self.doc), row * self.bpr + col), pane

    def _byte_x(self, col: int) -> int:
        return self.hex_x + self.cw * (col * 3 + col // 8) - self.horizontalScrollBar().value()

    # -- painting
    def paintEvent(self, ev):
        p = QPainter(self.viewport())
        pal = self.palette()
        p.fillRect(self.viewport().rect(), pal.color(pal.ColorRole.Base))
        p.setFont(self.font())
        fm = QFontMetrics(self.font())
        asc = fm.ascent() + 1
        top = self.verticalScrollBar().value()
        vis = self.viewport().height() // self.lh + 2
        data, n = self.doc.data, len(self.doc.data)
        sel_a, sel_n = self.selection()
        sel_b = sel_a + sel_n
        text_c, dim_c = pal.color(pal.ColorRole.Text), pal.color(pal.ColorRole.PlaceholderText)
        hx = self.horizontalScrollBar().value()
        col_sel = pal.color(pal.ColorRole.Highlight)
        col_mod, col_hit = QColor(255, 170, 60, 110), QColor(255, 230, 60, 120)
        col_diff, col_mark = QColor(255, 70, 70, 100), QColor(80, 200, 255, 140)
        for r in range(top, min(self.rows, top + vis)):
            y = (r - top) * self.lh
            base = r * self.bpr
            shown = self.base_offset + base
            p.setPen(dim_c)
            p.drawText(-hx, y + asc, f"{shown:08X}" if self.offset_hex else f"{shown:8d}")
            end = min(n, base + self.bpr)
            # backgrounds
            for i in range(base, min(base + self.bpr, n + 1)):
                col = i - base
                bg = None
                for a, b, c in self.regions:
                    if a <= i < b:
                        bg = c
                        break
                if self.doc.mod and i < n and self.doc.is_mod(i):
                    bg = col_mod
                if any(a <= i < b for a, b in self.hits):
                    bg = col_hit
                if any(a <= i < b for a, b in self.diffs):
                    bg = col_diff
                if i in self.marks:
                    bg = col_mark
                if sel_n and sel_a <= i < sel_b:
                    bg = col_sel
                if bg is not None:
                    p.fillRect(self._byte_x(col), y, self.cw * 2, self.lh, bg)
                    p.fillRect(self.asc_x + col * self.cw - hx, y, self.cw, self.lh, bg)
            # text
            p.setPen(text_c)
            for i in range(base, end):
                col = i - base
                p.drawText(self._byte_x(col), y + asc, f"{data[i]:02X}")
                c = data[i]
                p.drawText(self.asc_x + col * self.cw - hx, y + asc, chr(c) if 32 <= c < 127 else ".")
            # cursor
            if self.cur // self.bpr == r:
                col = self.cur - base
                p.setPen(QPen(pal.color(pal.ColorRole.Highlight), 2 if self.pane == "hex" else 1))
                x = self._byte_x(col) + (self.nib * self.cw if self.pane == "hex" else 0)
                p.drawRect(x, y, self.cw * (1 if self.pane == "hex" else 2), self.lh - 1)
                p.setPen(QPen(pal.color(pal.ColorRole.Highlight), 2 if self.pane == "ascii" else 1))
                p.drawRect(self.asc_x + col * self.cw - hx, y, self.cw, self.lh - 1)
        p.end()

    # -- mouse
    def mousePressEvent(self, e):
        if e.button() != Qt.MouseButton.LeftButton:
            return
        off, self.pane = self._pos_to_offset(e.position().toPoint())
        if e.modifiers() & Qt.KeyboardModifier.ShiftModifier:
            if self.anchor is None:
                self.anchor = self.cur
        else:
            self.anchor = off
        self.cur, self.nib = off, 0
        self._notify()
        self.setFocus()

    def mouseMoveEvent(self, e):
        if e.buttons() & Qt.MouseButton.LeftButton:
            off, _ = self._pos_to_offset(e.position().toPoint())
            if self.anchor is None:
                self.anchor = self.cur
            self.cur = off
            self._ensure_visible(off)
            self._notify()

    def mouseReleaseEvent(self, e):
        if self.anchor == self.cur:
            self.anchor = None
            self._notify()

    def wheelEvent(self, e):
        sb = self.verticalScrollBar()
        sb.setValue(sb.value() - int(e.angleDelta().y() / 40))

    def contextMenuEvent(self, e):
        m = QMenu(self)
        m.addAction("Copy as hex", self.copy_hex)
        m.addAction("Copy as text", self.copy_text)
        m.addAction("Cut", self.cut)
        m.addAction("Paste", self.paste)
        m.addSeparator()
        m.addAction("Select all", self.select_all)
        m.addAction("Delete", self.delete_selection)
        m.exec(e.globalPos())

    # -- clipboard / edit commands
    def select_all(self):
        self.anchor, self.cur = 0, len(self.doc)
        self._notify()

    def copy_hex(self):
        b = self.selected_bytes()
        if b:
            QApplication.clipboard().setText(" ".join(f"{x:02X}" for x in b))
            self.status.emit(f"Copied {len(b)} byte(s) as hex")

    def copy_text(self):
        b = self.selected_bytes()
        if b:
            QApplication.clipboard().setText(b.decode("latin1"))
            self.status.emit(f"Copied {len(b)} byte(s) as text")

    def cut(self):
        self.copy_hex()
        self.delete_selection()

    def delete_selection(self):
        a, n = self.selection()
        if n:
            self.doc.delete(a, n)
            self.anchor, self.cur = None, a
        elif self.cur < len(self.doc):
            self.doc.delete(self.cur, 1)
        self._notify()

    def paste(self):
        txt = QApplication.clipboard().text()
        if not txt:
            return
        clean = txt.replace("0x", " ").replace(",", " ")
        parts = clean.split()
        if parts and all(len(x) % 2 == 0 and all(c in _HEX for c in x) for x in parts) and self.pane == "hex":
            data = bytes.fromhex("".join(parts))
        elif len(parts) == 1 and all(c in _HEX for c in parts[0]) and len(parts[0]) % 2 == 0 and len(parts[0]) >= 4:
            data = bytes.fromhex(parts[0])
        else:
            data = txt.encode("latin1", errors="replace")
        self.put_bytes(data)

    def put_bytes(self, data: bytes):
        """Write bytes at the cursor (over the selection if any): overwrite or insert per mode."""
        a, n = self.selection()
        if n:
            self.doc.delete(a, n)
            self.doc.insert(a, data)
        elif self.insert_mode or a >= len(self.doc):
            self.doc.insert(a, data)
        else:
            self.doc.replace(a, data)
        self.anchor, self.cur, self.nib = None, a + len(data), 0
        self._ensure_visible(self.cur)
        self._notify()

    # -- keyboard
    def keyPressEvent(self, e):
        k, mods = e.key(), e.modifiers()
        ctrl = bool(mods & Qt.KeyboardModifier.ControlModifier)
        shift = bool(mods & Qt.KeyboardModifier.ShiftModifier)
        if ctrl:
            if k == Qt.Key.Key_C:
                (self.copy_text if shift else self.copy_hex)()
            elif k == Qt.Key.Key_X:
                self.cut()
            elif k == Qt.Key.Key_V:
                self.paste()
            elif k == Qt.Key.Key_A:
                self.select_all()
            elif k == Qt.Key.Key_Z:
                self.doc.undo()
            elif k == Qt.Key.Key_Y:
                self.doc.redo()
            else:
                super().keyPressEvent(e)
            return
        nav = {Qt.Key.Key_Left: -1, Qt.Key.Key_Right: 1, Qt.Key.Key_Up: -self.bpr, Qt.Key.Key_Down: self.bpr,
               Qt.Key.Key_PageUp: -self.bpr * max(1, self.viewport().height() // self.lh - 1),
               Qt.Key.Key_PageDown: self.bpr * max(1, self.viewport().height() // self.lh - 1)}
        if k in nav:
            if shift and self.anchor is None:
                self.anchor = self.cur
            elif not shift:
                self.anchor = None
            self.cur = max(0, min(len(self.doc), self.cur + nav[k]))
            self.nib = 0
            self._ensure_visible(self.cur)
            self._notify()
        elif k in (Qt.Key.Key_Home, Qt.Key.Key_End):
            if shift and self.anchor is None:
                self.anchor = self.cur
            elif not shift:
                self.anchor = None
            self.cur = (self.cur // self.bpr * self.bpr) if k == Qt.Key.Key_Home else min(len(self.doc), self.cur // self.bpr * self.bpr + self.bpr - 1)
            self.nib = 0
            self._notify()
        elif k == Qt.Key.Key_Tab:
            self.pane = "ascii" if self.pane == "hex" else "hex"
            self.nib = 0
            self.viewport().update()
        elif k == Qt.Key.Key_Insert:
            self.insert_mode = not self.insert_mode
            self.status.emit("Insert mode" if self.insert_mode else "Overwrite mode")
        elif k == Qt.Key.Key_Delete:
            self.delete_selection()
        elif k == Qt.Key.Key_Backspace:
            a, n = self.selection()
            if n:
                self.delete_selection()
            elif self.cur > 0:
                self.cur -= 1
                self.doc.delete(self.cur, 1)
                self.nib = 0
                self._notify()
        elif e.text() and self.pane == "hex" and e.text() in _HEX and len(e.text()) == 1:
            self._type_nibble(int(e.text(), 16))
        elif e.text() and self.pane == "ascii" and len(e.text()) == 1 and 32 <= ord(e.text()) < 127:
            self.put_bytes(e.text().encode("latin1"))
        else:
            super().keyPressEvent(e)

    def _type_nibble(self, d: int):
        n = len(self.doc)
        a, sl = self.selection()
        if sl:                                   # typing over a selection replaces it with one byte
            self.doc.delete(a, sl)
            self.anchor, self.cur, self.nib = None, a, 0
            n = len(self.doc)
        if self.nib == 0:
            if self.insert_mode or self.cur >= n:
                self.doc.insert(self.cur, bytes([d << 4]))
            else:
                self.doc.replace(self.cur, bytes([(d << 4) | (self.doc.data[self.cur] & 0x0F)]))
            self.nib = 1
        else:
            self.doc.replace(self.cur, bytes([(self.doc.data[self.cur] & 0xF0) | d]), merge=True)
            self.cur += 1
            self.nib = 0
        self._ensure_visible(self.cur)
        self._notify()
