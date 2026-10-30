#this belongs in apps/methods/ide_file.py - Version: 1
# X-Seti - September 21 2026 - IMG Factory 1.6 - IDE file model

"""ide_file.py - Section aware IDE reader/writer that keeps the ORIGINAL
file: comments, blank lines, every section (objs/tobj/anim/cars/peds/weap/
hier/2dfx/path/...), spacing and CRLF stay exactly as they were. A row that
was not edited is written back verbatim; an edited row only has the changed
comma-fields swapped in; deleted rows are dropped, new rows are added after
the last row of their section, and a section can be re-ordered. An untouched
file saves byte-identical. Rows are generic comma-field lines so every game's
column layout (III/VC/SA/SOL) works without game specific code."""

##Methods list -
# IDERow
# IDESection
# IDEFile
# column_headers

import os
import re
from pathlib import Path
from typing import List, Optional

_SPLIT = re.compile(r'([,\s]+)')

# sections whose rows start with a model ID (2dfx too: its first field is the ID)
ID_SECTIONS = ("objs", "tobj", "anim", "cars", "peds", "weap", "hier", "2dfx")
NAMED_SECTIONS = ("objs", "tobj", "anim", "cars", "peds", "weap", "hier")

_HEAD = {
    "objs":  ["ID", "Model", "TXD", "Meshes", "DrawDist", "Flags", "DrawDist2", "DrawDist3"],
    "tobj":  ["ID", "Model", "TXD", "Meshes", "DrawDist", "Flags", "TimeOn", "TimeOff"],
    "anim":  ["ID", "Model", "TXD", "Anim", "DrawDist", "Flags"],
    "cars":  ["ID", "Model", "TXD", "Type", "Handling", "GameName", "Class", "Frequency",
              "Level", "Comprules", "WheelModel", "WheelScale"],
    "peds":  ["ID", "Model", "TXD", "PedType", "Behaviour", "AnimGroup", "CarMask",
              "Flags", "Radio1", "Radio2"],
    "weap":  ["ID", "Model", "TXD", "Anim", "Meshes", "DrawDist"],
    "hier":  ["ID", "Model", "TXD"],
    "2dfx":  ["ID", "X", "Y", "Z"],
}


def column_headers(section: str, n: int, n_fields_hint: int = 0) -> List[str]:
    """Header names for a section with n columns. III objs have no Meshes
    column (id,model,txd,drawdist,flags) - handled by shifting when short."""
    base = list(_HEAD.get(section, []))
    if section in ("objs", "tobj") and 0 < n_fields_hint <= 5 + (2 if section == "tobj" else 0):
        base = ["ID", "Model", "TXD", "DrawDist", "Flags", "TimeOn", "TimeOff"]
    out = [base[i] if i < len(base) else f"F{i + 1}" for i in range(n)]
    return out


def _fields(line: str) -> List[str]:
    body = line.split('#')[0]
    return [p.strip() for p in body.split(',')]


class IDERow:
    __slots__ = ("fields", "raw_index", "orig")

    def __init__(self, fields, raw_index=None):
        self.fields: List[str] = list(fields)
        self.raw_index = raw_index
        self.orig = tuple(fields) if raw_index is not None else None

    @property
    def changed(self) -> bool:
        return self.orig is None or tuple(self.fields) != self.orig

    def copy_as_new(self) -> "IDERow":
        return IDERow(list(self.fields), None)

    def new_line(self) -> str:
        return ", ".join(self.fields)

    def patched_line(self, raw: str) -> str:
        """raw with only the changed fields replaced; separators and any
        '#' comment stay untouched."""
        body, hash_, comment = raw.partition('#')
        toks = _SPLIT.split(body)
        slots = [i for i in range(0, len(toks), 2) if toks[i] != '']
        orig = self.orig or ()
        for k, val in enumerate(self.fields):
            if k < len(orig) and orig[k] == val:
                continue
            if k < len(slots):
                toks[slots[k]] = val
            else:
                toks.append(f", {val}")
        # fields removed from the end
        if len(self.fields) < len(slots):
            keep = slots[len(self.fields) - 1] if self.fields else -1
            toks = toks[:keep + 1]
        return ''.join(toks) + hash_ + comment


class IDESection:
    def __init__(self, name: str):
        self.name = name
        self.raw: List[str] = []
        self.rows: List[IDERow] = []
        self.header_raw = name
        self.end_raw: Optional[str] = "end"
        self.reordered = False
        self._count0 = 0

    @property
    def dirty(self) -> bool:
        return (self.reordered or len(self.rows) != self._count0
                or any(r.changed for r in self.rows))

    def width(self) -> int:
        return max((len(r.fields) for r in self.rows), default=0)


class IDEFile:
    def __init__(self):
        self.items: list = []          # raw str lines and IDESection objects, in order
        self.path = ""
        self.eol = "\n"

    @property
    def sections(self) -> List[IDESection]:
        return [i for i in self.items if isinstance(i, IDESection)]

    def section(self, name: str) -> Optional[IDESection]:
        return next((s for s in self.sections if s.name == name), None)

    @property
    def dirty(self) -> bool:
        return any(s.dirty for s in self.sections)

    # -- load
    def load(self, path: str) -> bool:
        self.path = path
        return self._load_text(Path(path).read_bytes().decode("latin1"))

    def _load_text(self, text: str) -> bool:
        self.eol = "\r\n" if "\r\n" in text else "\n"
        self.items = []
        cur: Optional[IDESection] = None
        for line in text.split(self.eol):
            s = line.strip()
            low = s.split('#')[0].strip().lower()
            if cur is None:
                if low and low != "end" and re.fullmatch(r"[a-z0-9]{2,8}", low):
                    cur = IDESection(low)
                    cur.header_raw = line
                    self.items.append(cur)
                else:
                    self.items.append(line)
                continue
            if low == "end":
                cur.end_raw = line
                cur._count0 = len(cur.rows)
                cur = None
                continue
            if low and ',' in low:
                cur.rows.append(IDERow(_fields(line), len(cur.raw)))
            cur.raw.append(line)
        if cur is not None:
            cur.end_raw = None
            cur._count0 = len(cur.rows)
        return True

    # -- save
    def to_text(self) -> str:
        out = []
        for item in self.items:
            if not isinstance(item, IDESection):
                out.append(item)
                continue
            out.append(item.header_raw)
            out.extend(self._section_lines(item))
            if item.end_raw is not None:
                out.append(item.end_raw)
        return self.eol.join(out)

    @staticmethod
    def _section_lines(sec: IDESection) -> List[str]:
        keep = [r for r in sec.rows if r.raw_index is not None]
        new = [r for r in sec.rows if r.raw_index is None]
        row_slots = {r.raw_index for r in keep}
        # every original row line (kept or deleted) - deleted ones vanish
        all_orig = {i for i, l in enumerate(sec.raw) if l.split('#')[0].strip() and ',' in l.split('#')[0]}
        body, last = [], -1
        if sec.reordered:
            slots = sorted(row_slots)
            placement = dict(zip(slots, keep))          # slot -> row taking that place
        else:
            placement = {r.raw_index: r for r in keep}
        for i, line in enumerate(sec.raw):
            if i in all_orig:
                r = placement.get(i)
                if r is None:
                    continue                            # deleted row
                src = sec.raw[r.raw_index]
                body.append(r.patched_line(src) if r.changed else src)
                last = len(body) - 1
            else:
                body.append(line)
        if new:
            rows = [r.new_line() for r in new]
            at = last + 1 if last >= 0 else len(body)
            body[at:at] = rows
        return body

    def save(self, path: str = ""):
        """Atomic write (temp file in the same folder, then swap in)."""
        import tempfile
        out_path = path or self.path
        data = self.to_text().encode("latin1", errors="replace")
        d = os.path.dirname(os.path.abspath(out_path))
        fd, tmp = tempfile.mkstemp(dir=d, prefix=".ide_", suffix=".tmp")
        try:
            with os.fdopen(fd, "wb") as f:
                f.write(data)
            if os.path.exists(out_path):
                try:
                    os.chmod(tmp, os.stat(out_path).st_mode & 0o7777)
                except OSError:
                    pass
            os.replace(tmp, out_path)
        except Exception:
            if os.path.exists(tmp):
                os.unlink(tmp)
            raise
        self.path = out_path
        self._adopt(data.decode("latin1"))

    def _adopt(self, text: str):
        """Re-baseline against what was just written, keeping the row objects
        (so the table, undo history and selections stay valid)."""
        fresh = IDEFile()
        fresh._load_text(text)
        for a, b in zip(self.sections, fresh.sections):
            kept = [r for r in a.rows if r.raw_index is not None]
            if not a.reordered:
                kept.sort(key=lambda r: r.raw_index)      # file order
            in_file_order = kept + [r for r in a.rows if r.raw_index is None]
            for r, f in zip(in_file_order, b.rows):
                r.fields = list(f.fields)
                r.raw_index, r.orig = f.raw_index, f.orig
            a.rows = in_file_order
            a.raw, a.header_raw, a.end_raw = b.raw, b.header_raw, b.end_raw
            a.reordered, a._count0 = False, len(a.rows)
