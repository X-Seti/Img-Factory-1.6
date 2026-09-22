#this belongs in apps/methods/path_formats.py - Version: 1
# X-Seti - September 21 2026 - IMG Factory 1.6 - Path file layers

"""path_formats.py - Read/edit/write GTA path data as uniform "layers"
for Path Workshop. Every format keeps the ORIGINAL bytes/lines and only
patches what was edited, so an untouched file saves byte-identical:
  TextWaypointLayer  flight*.dat / spath*.dat / train*.dat / tracks*.dat
                     (optional count line, then x y z [...] rows)
  IplPathLayer       'path' section of a text IPL (paths.ipl, SOL/LCS)
  NodesDatLayer      SA binary nodes*.dat (node positions patched in place)
Layer API: points [(x,y,z,group)], edges [(i,j)], move/add/delete,
snapshot/restore (undo), save(path), dirty."""

##Methods list -
# PathLayer
# TextWaypointLayer
# IplPathLayer
# NodesDatLayer
# load_path_layer

import os
import re
import struct
from pathlib import Path
from typing import List, Optional, Tuple

_SPLIT = re.compile(r'([,\s]+)')


def _fmt(v: float) -> str:
    s = f"{v:.4f}".rstrip('0').rstrip('.')
    return s if s not in ('', '-0') else '0'


def _replace_xyz(line: str, x: float, y: float, z: float, first: int = 0) -> str:
    """Swap the numeric tokens first..first+2 of a comma/space separated
    line, keeping every separator and every other token untouched."""
    toks = _SPLIT.split(line)
    idx = [i for i in range(0, len(toks), 2) if toks[i] != '']
    for slot, val in zip(idx[first:first + 3], (x, y, z)):
        toks[slot] = _fmt(val)
    return ''.join(toks)


class PathLayer:
    kind = "generic"
    can_add = False
    can_delete = False

    def __init__(self, path: str):
        self.path = path
        self.name = os.path.basename(path)
        self.points: List[list] = []     # [x, y, z, group]
        self.edges: List[Tuple[int, int]] = []
        self.visible = True
        self._orig = []
        self._loaded = True

    @property
    def dirty(self) -> bool:
        return self.snapshot() != self._orig

    def snapshot(self):
        return [tuple(p) for p in self.points]

    def restore(self, snap):
        self.points = [list(p) for p in snap]
        self._rebuild_edges()

    def _rebuild_edges(self):
        pass

    def move(self, i: int, x: float, y: float, z: Optional[float] = None):
        p = self.points[i]
        p[0], p[1] = x, y
        if z is not None:
            p[2] = z

    def add(self, x, y, z=0.0, after: Optional[int] = None) -> int:
        raise NotImplementedError

    def delete(self, i: int):
        raise NotImplementedError

    def to_bytes(self) -> bytes:
        raise NotImplementedError

    def save(self, path: str):
        """Atomic write (temp file in the same folder, then swap in)."""
        import tempfile
        data = self.to_bytes()
        d = os.path.dirname(os.path.abspath(path))
        fd, tmp = tempfile.mkstemp(dir=d, prefix=".path_", suffix=".tmp")
        try:
            with os.fdopen(fd, "wb") as f:
                f.write(data)
            if os.path.exists(path):
                try:
                    os.chmod(tmp, os.stat(path).st_mode & 0o7777)
                except OSError:
                    pass
            os.replace(tmp, path)
        except Exception:
            if os.path.exists(tmp):
                os.unlink(tmp)
            raise
        self.path, self.name = path, os.path.basename(path)
        self._orig = self.snapshot()


class _LineLayer(PathLayer):
    """Shared line-preserving save for the two text formats."""
    _first_field = 0

    def _read(self, path):
        raw = Path(path).read_bytes().decode("latin1")
        self._eol = "\r\n" if "\r\n" in raw else "\n"
        self._lines = raw.split(self._eol)

    def _emit(self, line_of, count_line, count_fmt) -> str:
        """Rebuild the file: kept rows verbatim unless moved, deleted rows
        dropped, new rows after the last row, count line updated."""
        alive = {p[4]: p for p in self.points if len(p) > 4 and p[4] is not None}
        new = [p for p in self.points if len(p) <= 4 or p[4] is None]
        out, last_row = [], -1
        for i, line in enumerate(self._lines):
            if i == count_line:
                out.append(count_fmt(len(self.points), line))
            elif i in line_of:
                p = alive.get(i)
                if p is None:
                    continue
                o = line_of[i]
                out.append(line if (p[0], p[1], p[2]) == o
                           else _replace_xyz(line, p[0], p[1], p[2], self._first_field))
                last_row = len(out) - 1
            else:
                out.append(line)
        if new:
            tmpl = next((self._lines[i] for i in sorted(line_of)), "")
            rows = [_replace_xyz(tmpl, p[0], p[1], p[2], self._first_field) if tmpl
                    else f"{_fmt(p[0])} {_fmt(p[1])} {_fmt(p[2])}" for p in new]
            at = last_row + 1 if last_row >= 0 else len(out)
            out[at:at] = rows
        return self._eol.join(out)


class TextWaypointLayer(_LineLayer):
    kind = "waypoints"
    can_add = True
    can_delete = True

    def __init__(self, path):
        super().__init__(path)
        self._read(path)
        self._line_of, self._count_line = {}, None
        seen_data = False
        for i, line in enumerate(self._lines):
            s = line.strip()
            if not s or s.startswith((";", "#")):
                continue
            toks = [t for t in _SPLIT.split(s) if t.strip(" ,\t")]
            if not seen_data and len(toks) == 1 and re.fullmatch(r"\d+", toks[0]):
                self._count_line = i
                seen_data = True
                continue
            seen_data = True
            try:
                x, y, z = (float(t) for t in toks[:3])
            except ValueError:
                continue
            if len(toks) < 3:
                continue
            self.points.append([x, y, z, 0, i])
            self._line_of[i] = (x, y, z)
        if not self.points:
            raise ValueError("no waypoints found")
        self._rebuild_edges()
        self._orig = self.snapshot()

    def _rebuild_edges(self):
        self.edges = [(i, i + 1) for i in range(len(self.points) - 1)]

    def add(self, x, y, z=0.0, after=None):
        at = len(self.points) if after is None else after + 1
        self.points.insert(at, [x, y, z, 0, None])
        self._rebuild_edges()
        return at

    def delete(self, i):
        del self.points[i]
        self._rebuild_edges()

    def to_bytes(self) -> bytes:
        text = self._emit(self._line_of, self._count_line,
                          lambda n, old: re.sub(r"\d+", str(n), old, count=1))
        return text.encode("latin1", errors="replace")


class IplPathLayer(_LineLayer):
    """'path' section of a text IPL: node rows have >= 6 comma fields with
    x,y,z at fields 3..5; two-field rows are group headers. Edit = move only
    (nodes reference each other by index, so add/delete would break them)."""
    kind = "ipl_path"
    _first_field = 3

    def __init__(self, path):
        super().__init__(path)
        self._read(path)
        self._line_of, self._count_line = {}, None
        self._links = []                       # (point index, target row in group, group)
        self._rows = {}                        # (group, row) -> point index
        in_path, group, row = False, 0, 0
        for i, line in enumerate(self._lines):
            s = line.split("#")[0].strip()
            low = s.lower()
            if low == "path":
                in_path = True
                continue
            if in_path and low == "end":
                in_path = False
                continue
            if not in_path or not s:
                continue
            f = [t.strip() for t in s.split(",")]
            if len(f) < 6:                     # group header, e.g. "1, -1"
                group += 1
                row = 0
                continue
            this_row, row = row, row + 1
            try:
                ntype, nxt = int(f[0]), int(f[1])
                x, y, z = float(f[3]), float(f[4]), float(f[5])
            except ValueError:
                continue
            if ntype == 0:                     # unused padding slot (x=y=z=0)
                continue
            self.points.append([x, y, z, group, i])
            self._line_of[i] = (x, y, z)
            self._rows[(group, this_row)] = len(self.points) - 1
            self._links.append((len(self.points) - 1, nxt, group))
        if not self.points:
            raise ValueError("no path nodes found")
        self._rebuild_edges()
        self._orig = self.snapshot()

    def _rebuild_edges(self):
        self.edges = []
        for pi, nxt, g in self._links:
            tj = self._rows.get((g, nxt)) if nxt >= 0 else None
            if tj is not None and tj != pi:
                self.edges.append((pi, tj))

    def to_bytes(self) -> bytes:
        return self._emit(self._line_of, None, None).encode("latin1", errors="replace")


class NodesDatLayer(PathLayer):
    """SA nodes*.dat. Only node positions (int16, 1/8 unit) are patched in
    place; every other byte (links, navi nodes, flags) is untouched."""
    kind = "nodes_dat"

    def __init__(self, path):
        super().__init__(path)
        self._data = bytearray(Path(path).read_bytes())
        if len(self._data) < 20:
            raise ValueError("too small for nodes.dat")
        n_total, n_veh, n_ped, n_navi, n_links = struct.unpack_from("<5I", self._data, 0)
        if n_total != n_veh + n_ped or 20 + n_total * 28 > len(self._data):
            raise ValueError("not a SA nodes.dat")
        self._n_veh, self._n_navi, self._n_links = n_veh, n_navi, n_links
        base = 20
        for i in range(n_total):
            o = base + i * 28
            x, y, z = struct.unpack_from("<3h", self._data, o + 8)
            self.points.append([x / 8.0, y / 8.0, z / 8.0, 0 if i < n_veh else 1, o])
        links_off = base + n_total * 28 + n_navi * 14
        area = struct.unpack_from("<H", self._data, base + 18)[0] if n_total else 0
        links = []
        if links_off + n_links * 4 <= len(self._data):
            links = [struct.unpack_from("<HH", self._data, links_off + k * 4) for k in range(n_links)]
        for i in range(n_total):
            o = base + i * 28
            link_id = struct.unpack_from("<H", self._data, o + 16)[0]
            count = struct.unpack_from("<I", self._data, o + 24)[0] & 0xF
            for k in range(link_id, min(link_id + count, len(links))):
                a, nid = links[k]
                if a == area and nid < n_total and nid > i:
                    self.edges.append((i, nid))
        self._orig = self.snapshot()

    def to_bytes(self) -> bytes:
        out = bytearray(self._data)
        for p in self.points:
            o = p[4]
            for k in range(3):
                v = max(-32768, min(32767, int(round(p[k] * 8.0))))
                struct.pack_into("<h", out, o + 8 + k * 2, v)
        return bytes(out)


def load_path_layer(path: str) -> PathLayer: #vers 1
    """Pick the right layer type from the file name/contents."""
    low = os.path.basename(path).lower()
    if low.endswith(".ipl"):
        return IplPathLayer(path)
    raw = Path(path).read_bytes()
    if re.match(r"nodes\d*\.dat$", low) or (len(raw) >= 20 and b"\0" in raw[:64]):
        return NodesDatLayer(path)
    return TextWaypointLayer(path)
