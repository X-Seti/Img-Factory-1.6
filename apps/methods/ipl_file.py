#this belongs in apps/methods/ipl_file.py - Version: 1
# X-Seti - September 21 2026 - IMG Factory 1.6 - IPL file model (text IPL)

"""ipl_file.py - Text IPL reader/writer that keeps the ORIGINAL file.
Every line (comments, blank lines, other sections, spacing, CRLF) is kept in
place; an inst entry that was not edited is written back verbatim, an edited
one only has the changed fields swapped in (its scale, spacing and trailing
comment survive), deleted entries are dropped and new ones are added after
the last entry. An untouched file saves byte-identical.
Layouts: III/VC inst = id,name,x,y,z,sx,sy,sz,rx,ry,rz,rw (12 fields);
SA inst = id,name,interior,x,y,z,rx,ry,rz,rw[,lod] (10-11 fields)."""

##Methods list -
# IPLEntry
# IPLSection
# IPLFile
# detect_layout

import copy
import os
import re
from pathlib import Path
from typing import List, Optional

_SPLIT = re.compile(r'([,\s]+)')

KNOWN_SECTIONS = {"inst", "zone", "cull", "cars", "grge", "enex", "pick",
                  "path", "mult", "occl", "auzo", "nplp", "slip", "tunnel"}

# field index per attribute for each layout
_FIELDS = {
    "vc":   {"model_id": 0, "model_name": 1, "interior": 2, "px": 3, "py": 4, "pz": 5,
             "rx": 9, "ry": 10, "rz": 11, "rw": 12},
    "gta3": {"model_id": 0, "model_name": 1, "px": 2, "py": 3, "pz": 4,
             "rx": 8, "ry": 9, "rz": 10, "rw": 11},
    "sa":   {"model_id": 0, "model_name": 1, "interior": 2, "px": 3, "py": 4, "pz": 5,
             "rx": 6, "ry": 7, "rz": 8, "rw": 9, "lod": 10},
}
_TRACKED = ("model_id", "model_name", "interior", "px", "py", "pz", "rx", "ry", "rz", "rw", "lod")


def _num(v: float) -> str:
    s = f"{v:.6f}".rstrip('0').rstrip('.')
    return s if s not in ('', '-0') else '0'


def detect_layout(n_fields: int) -> str:
    """13 fields = VC (has interior + scale), 12 = III, 10/11 = SA."""
    return "vc" if n_fields >= 13 else ("gta3" if n_fields == 12 else "sa")


class IPLEntry:
    """One inst line. source_line is a free tag (Path Workshop / map use it
    for the owning IPL's name in merged views)."""
    __slots__ = ('model_id', 'model_name', 'interior', 'px', 'py', 'pz',
                 'rx', 'ry', 'rz', 'rw', 'lod', 'source_line',
                 'layout', 'raw_index', 'orig')

    def __init__(self, model_id=0, model_name="", interior=0,
                 px=0.0, py=0.0, pz=0.0, rx=0.0, ry=0.0, rz=0.0, rw=1.0,
                 lod=-1, source_line="", layout="sa"):
        self.model_id, self.model_name, self.interior = model_id, model_name, interior
        self.px, self.py, self.pz = px, py, pz
        self.rx, self.ry, self.rz, self.rw = rx, ry, rz, rw
        self.lod, self.source_line, self.layout = lod, source_line, layout
        self.raw_index = None      # line index in the section's raw lines (None = new)
        self.orig = None           # snapshot at load, to know if it changed

    def snap(self):
        return tuple(getattr(self, a) for a in _TRACKED)

    @property
    def changed(self) -> bool:
        return self.orig is None or self.snap() != self.orig

    def copy_as_new(self) -> "IPLEntry":
        e = copy.copy(self)
        e.raw_index, e.orig = None, None
        return e

    def new_line(self) -> str:
        if self.layout == "gta3":
            return (f"{self.model_id}, {self.model_name}, {_num(self.px)}, {_num(self.py)}, "
                    f"{_num(self.pz)}, 1, 1, 1, {_num(self.rx)}, {_num(self.ry)}, "
                    f"{_num(self.rz)}, {_num(self.rw)}")
        if self.layout == "vc":
            return (f"{self.model_id}, {self.model_name}, {self.interior}, {_num(self.px)}, "
                    f"{_num(self.py)}, {_num(self.pz)}, 1, 1, 1, {_num(self.rx)}, "
                    f"{_num(self.ry)}, {_num(self.rz)}, {_num(self.rw)}")
        base = (f"{self.model_id}, {self.model_name}, {self.interior}, {_num(self.px)}, "
                f"{_num(self.py)}, {_num(self.pz)}, {_num(self.rx)}, {_num(self.ry)}, "
                f"{_num(self.rz)}, {_num(self.rw)}")
        return base + (f", {self.lod}" if self.lod >= 0 else ", -1")

    # keep the old API name for callers that used it
    def to_gta3_line(self):
        e = copy.copy(self); e.layout = "gta3"; return e.new_line()

    def to_sa_line(self):
        e = copy.copy(self); e.layout = "sa"; return e.new_line()

    def patched_line(self, raw: str) -> str:
        """raw with only the changed fields replaced (separators, extra
        fields such as scale and any '#' comment stay as they were)."""
        body, hash_, comment = raw.partition('#')
        toks = _SPLIT.split(body)
        slots = [i for i in range(0, len(toks), 2) if toks[i] != '']
        fmap = _FIELDS[self.layout]
        for attr, o in zip(_TRACKED, self.orig or ()):
            if attr not in fmap or getattr(self, attr) == o:
                continue
            k = fmap[attr]
            v = getattr(self, attr)
            if k < len(slots):
                toks[slots[k]] = (v if isinstance(v, str) else
                                  str(v) if isinstance(v, int) else _num(v))
            elif attr == "lod":                       # SA line had no lod field yet
                toks.append(f", {v}")
        return ''.join(toks) + hash_ + comment


class IPLSection:
    """A named section. raw = every original line between header and end.
    inst sections also carry parsed entries (raw_index points into raw)."""

    def __init__(self, name: str):
        self.name = name
        self.raw: List[str] = []
        self.entries: List[IPLEntry] = []
        self.header_raw = name
        self.end_raw = "end"
        self.reordered = False                        # inst entries re-ordered (Sort)
        self._lines_orig: Optional[List[str]] = None   # non-inst text as loaded

    def is_inst(self):
        return self.name == "inst"

    # non-inst text editing (Text view of the Workshop)
    @property
    def lines(self) -> List[str]:
        return self.raw

    @lines.setter
    def lines(self, v: List[str]):
        self.raw = list(v)

    @property
    def text_changed(self) -> bool:
        return self._lines_orig is not None and self.raw != self._lines_orig


class IPLFile:
    """Text IPL. items = raw lines (str) and IPLSection objects in file order."""

    KNOWN_SECTIONS = KNOWN_SECTIONS

    def __init__(self):
        self.items: list = []
        self.game = "auto"
        self.path = ""
        self.eol = "\n"
        self._count0 = 0          # inst entries when loaded
        self._trail = False

    # -- compat views
    @property
    def sections(self) -> List[IPLSection]:
        return [i for i in self.items if isinstance(i, IPLSection)]

    @property
    def header_lines(self) -> List[str]:
        out = []
        for i in self.items:
            if isinstance(i, IPLSection):
                break
            out.append(i)
        return out

    @property
    def instances(self) -> List[IPLEntry]:
        s = self.inst_section
        return s.entries if s else []

    @property
    def inst_section(self) -> Optional[IPLSection]:
        for s in self.sections:
            if s.is_inst():
                return s
        return None

    def section_names(self) -> List[str]:
        return [s.name for s in self.sections]

    # -- load
    def load(self, path: str) -> bool:
        self.path = path
        raw = Path(path).read_bytes()
        if raw[:4] == b"bnry":
            raise ValueError("binary IPL (streaming) - only text IPL files can be edited")
        return self._load_text(raw.decode("latin1"))

    def _load_text(self, text: str) -> bool:
        self.eol = "\r\n" if "\r\n" in text else "\n"
        lines = text.split(self.eol)
        self.items = []
        cur: Optional[IPLSection] = None
        counts = {"gta3": 0, "vc": 0, "sa": 0}
        for line in lines:
            s = line.strip()
            low = s.split("#")[0].strip().lower()
            if cur is None:
                if low in KNOWN_SECTIONS and "," not in low:
                    cur = IPLSection(low)
                    cur.header_raw = line
                    self.items.append(cur)
                else:
                    self.items.append(line)
                continue
            if low == "end":
                cur.end_raw = line
                if not cur.is_inst():
                    cur._lines_orig = list(cur.raw)
                cur = None
                continue
            if cur.is_inst() and low:
                e = self._parse_inst(s.split("#")[0])
                if e is not None:
                    e.raw_index = len(cur.raw)
                    e.orig = e.snap()
                    e.source_line = ""
                    cur.entries.append(e)
                    counts[e.layout] += 1
            cur.raw.append(line)
        if cur is not None:                     # missing final 'end' - keep as is
            if not cur.is_inst():
                cur._lines_orig = list(cur.raw)
            cur.end_raw = None
        if self.game == "auto":
            self.game = max(counts, key=counts.get) if any(counts.values()) else "gta3"
        self._count0 = len(self.instances)
        return True

    def _parse_inst(self, line: str) -> Optional[IPLEntry]:
        parts = [p.strip() for p in line.split(",")]
        n = len(parts)
        try:
            if n >= 13:
                return IPLEntry(model_id=int(parts[0]), model_name=parts[1],
                                interior=int(parts[2]),
                                px=float(parts[3]), py=float(parts[4]), pz=float(parts[5]),
                                rx=float(parts[9]), ry=float(parts[10]),
                                rz=float(parts[11]), rw=float(parts[12]), layout="vc")
            if n == 12:
                return IPLEntry(model_id=int(parts[0]), model_name=parts[1],
                                px=float(parts[2]), py=float(parts[3]), pz=float(parts[4]),
                                rx=float(parts[8]), ry=float(parts[9]),
                                rz=float(parts[10]), rw=float(parts[11]), layout="gta3")
            if n >= 10:
                return IPLEntry(model_id=int(parts[0]), model_name=parts[1],
                                interior=int(parts[2]),
                                px=float(parts[3]), py=float(parts[4]), pz=float(parts[5]),
                                rx=float(parts[6]), ry=float(parts[7]),
                                rz=float(parts[8]), rw=float(parts[9]),
                                lod=int(parts[10]) if n >= 11 else -1, layout="sa")
        except (ValueError, IndexError):
            pass
        return None

    # -- state
    @property
    def dirty(self) -> bool:
        for s in self.sections:
            if s.is_inst():
                if s.reordered or len(s.entries) != self._count0 or any(e.changed for e in s.entries):
                    return True
            elif s.text_changed:
                return True
        return False

    # -- save
    def _new_layout(self) -> str:
        return self.game if self.game in ("gta3", "vc", "sa") else "sa"

    def to_text(self) -> str:
        out = []
        for item in self.items:
            if not isinstance(item, IPLSection):
                out.append(item)
                continue
            out.append(item.header_raw)
            if not item.is_inst():
                out.extend(item.raw)
            else:
                kept = [e for e in item.entries if e.raw_index is not None]
                if item.reordered:
                    # re-ordered: surviving entries take the surviving line slots in their new order
                    alive = dict(zip(sorted(e.raw_index for e in kept), kept))
                else:
                    alive = {e.raw_index: e for e in kept}
                new = [e for e in item.entries if e.raw_index is None]
                last = -1
                body = []
                for i, line in enumerate(item.raw):
                    e = alive.get(i)
                    if e is None and self._is_inst_line(line):
                        continue                     # an entry that was deleted
                    if e is not None:
                        src = item.raw[e.raw_index]              # the entry's OWN original line
                        body.append(e.patched_line(src) if e.changed else src)
                        last = len(body) - 1
                    else:
                        body.append(line)
                if new:
                    rows = []
                    for e in new:
                        e.layout = e.layout or self._new_layout()
                        rows.append(e.new_line())
                    at = last + 1 if last >= 0 else len(body)
                    body[at:at] = rows
                out.extend(body)
            if item.end_raw is not None:
                out.append(item.end_raw)
        return self.eol.join(out)

    def _is_inst_line(self, line: str) -> bool:
        s = line.split("#")[0].strip()
        return bool(s) and self._parse_inst(s) is not None

    def save(self, path: str = ""):
        """Atomic write (temp file in the same folder, then swap in)."""
        import tempfile
        out_path = path or self.path
        data = self.to_text().encode("latin1", errors="replace")
        d = os.path.dirname(os.path.abspath(out_path))
        fd, tmp = tempfile.mkstemp(dir=d, prefix=".ipl_", suffix=".tmp")
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
        """After a save: make the in-memory sections match the file just
        written (raw lines, raw_index, baselines) WITHOUT replacing the
        entry objects, so the table, undo stacks and selections stay valid."""
        fresh = IPLFile()
        fresh.game = self.game
        tmp = self.path
        import tempfile
        # parse from text via a scratch load (no disk round trip needed)
        fresh._load_text(text)
        mine, theirs = self.sections, fresh.sections
        for a, b in zip(mine, theirs):
            if a.is_inst():
                kept = [e for e in a.entries if e.raw_index is not None]
                if not a.reordered:
                    kept.sort(key=lambda e: e.raw_index)
                new = [e for e in a.entries if e.raw_index is None]
                ordered = kept + new
                for e, f in zip(ordered, b.entries):
                    for attr in _TRACKED:
                        setattr(e, attr, getattr(f, attr))
                    e.raw_index, e.orig, e.layout = f.raw_index, f.orig, f.layout
                a.entries = ordered
                a.reordered = False
            a.raw = b.raw
            a.header_raw, a.end_raw = b.header_raw, b.end_raw
            a._lines_orig = None if a.is_inst() else list(b.raw)
        self.items = [i for i in fresh.items if not isinstance(i, IPLSection)] and \
            self._merge_items(fresh)
        self._count0 = len(self.instances)

    def _merge_items(self, fresh):
        it = iter(self.sections)
        out = []
        for i in fresh.items:
            out.append(next(it) if isinstance(i, IPLSection) else i)
        return out
