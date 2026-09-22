#this belongs in apps/methods/vehicle_data_files.py - Version: 1
# X-Seti - September 21 2026 - IMG Factory 1.6 - carcols.dat / carmods.dat file models

"""vehicle_data_files.py - carcols.dat and carmods.dat reader/writer that keep
the ORIGINAL file (comments, section words, blank lines, CRLF, the SA car3/car4
and link/wheel sections). Only rows that were edited are rewritten (and only the
changed numbers inside them); deleted rows are dropped, new rows go at the end of
their section. An untouched file saves byte-identical.
carcols.dat: 'col' = palette rows (r,g,b # comment), 'car' = name + colour index
pairs. 'car3'/'car4' rows (SA extra colours) are kept verbatim.
carmods.dat: 'mods' = vehicle + its mod names; 'link' and 'wheel' kept verbatim."""

##Methods list -
# CarColour
# CarColEntry
# CarModEntry
# CarColsFile
# CarModsFile

import os
import re
import tempfile
from pathlib import Path
from typing import List, Optional, Tuple

_SEP = re.compile(r'([,\s]+)')
_NUM = re.compile(r'-?\d+')


def _atomic_write(path: str, data: bytes):
    d = os.path.dirname(os.path.abspath(path))
    fd, tmp = tempfile.mkstemp(dir=d, prefix=".veh_", suffix=".tmp")
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


def _split_comment(line: str):
    """(body, comment_with_marker) - a comment starts at '#' or ';'."""
    for i, ch in enumerate(line):
        if ch in "#;":
            return line[:i], line[i:]
    return line, ""


class CarColour:
    def __init__(self, r: int = 0, g: int = 0, b: int = 0):
        self.r, self.g, self.b = r, g, b

    def __eq__(self, o):
        return isinstance(o, CarColour) and (self.r, self.g, self.b) == (o.r, o.g, o.b)

    def __str__(self):
        return f"{self.r},{self.g},{self.b}"


class CarColEntry:
    def __init__(self, name: str = "", palettes: Optional[List[Tuple[int, int]]] = None, raw_index=None):
        self.name = name
        self.palettes: List[Tuple[int, int]] = list(palettes or [])
        self.raw_index = raw_index
        self.orig = (name, tuple(self.palettes)) if raw_index is not None else None

    @property
    def changed(self) -> bool:
        return self.orig is None or self.orig != (self.name, tuple(self.palettes))


class CarModEntry:
    def __init__(self, vehicle: str = "", mods: Optional[List[str]] = None, raw_index=None):
        self.vehicle = vehicle
        self.mods: List[str] = list(mods or [])
        self.raw_index = raw_index
        self.orig = (vehicle, tuple(self.mods)) if raw_index is not None else None

    @property
    def changed(self) -> bool:
        return self.orig is None or self.orig != (self.vehicle, tuple(self.mods))


class _LineFile:
    def _read(self, path: str):
        text = Path(path).read_bytes().decode("latin1")
        self._eol = "\r\n" if "\r\n" in text else "\n"
        self._lines = text.split(self._eol)
        self.path = path

    @staticmethod
    def _section_of(line: str, known) -> Optional[str]:
        w = _split_comment(line)[0].strip().lower()
        return w if w in known else None

    def _write(self, path: str, text: str):
        _atomic_write(path, text.encode("latin1", errors="replace"))
        self.path = path


class CarColsFile(_LineFile):
    """Compatible with the Vehicle Workshop tab: .colours, .vehicles, .header_lines, .game."""
    _KNOWN = ("col", "car", "car3", "car4")

    def __init__(self, colour_cls=CarColour):
        self._colour_cls = colour_cls
        self.colours: list = []
        self.vehicles: List[CarColEntry] = []
        self.header_lines: List[str] = []
        self.game = "VC"
        self.path = ""
        self._lines: List[str] = []
        self._eol = "\n"
        self._col_src: List[Tuple[int, tuple]] = []     # per original colour: (line index, rgb)
        self._veh_lines: set = set()                    # line index of every original 'car' row
        self._n_veh0 = 0

    @property
    def dirty(self) -> bool:
        cols = [(c.r, c.g, c.b) for c in self.colours]
        if len(cols) != len(self._col_src) or any(cols[i] != self._col_src[i][1] for i in range(len(cols))):
            return True
        return len(self.vehicles) != self._n_veh0 or any(v.changed for v in self.vehicles)

    def load(self, path: str) -> bool:
        try:
            self._parse(path)
            return True
        except Exception as ex:
            print(f"CarColsFile.load: {ex}")
            return False

    def _parse(self, path: str):
        self._read(path)
        self.colours, self.vehicles, self._col_src, self._veh_lines = [], [], [], set()
        sec, first = None, None
        for i, line in enumerate(self._lines):
            s = _split_comment(line)[0].strip()
            if sec is None:
                k = self._section_of(line, self._KNOWN)
                if k:
                    sec = k
                    first = i if first is None else first
                continue
            if s.lower() == "end":
                sec = None
                continue
            if not s:
                continue
            if sec == "col":
                n = _NUM.findall(s)
                if len(n) >= 3:
                    rgb = (int(n[0]), int(n[1]), int(n[2]))
                    self.colours.append(self._colour_cls(*rgb))
                    self._col_src.append((i, rgb))
            elif sec == "car":
                parts = [p.strip() for p in s.split(',')]
                nums = [int(x) for x in parts[1:] if re.fullmatch(r'-?\d+', x)]
                pal = [(nums[k], nums[k + 1]) for k in range(0, len(nums) - 1, 2)]
                if parts and parts[0] and pal:
                    self.vehicles.append(CarColEntry(parts[0], pal, i))
                    self._veh_lines.add(i)
        self._n_veh0 = len(self.vehicles)
        self.header_lines = self._lines[:first] if first else []
        self.game = "SA" if any(self._section_of(l, ("car4", "car3")) for l in self._lines) else "VC"

    @staticmethod
    def _car_line(v: CarColEntry, comment: str = "") -> str:
        return f"{v.name}, " + ", ".join(f"{p},{s}" for p, s in v.palettes) + (("\t" + comment) if comment else "")

    def to_text(self) -> str:
        n_keep = min(len(self._col_src), len(self.colours))
        cur = {self._col_src[k][0]: self.colours[k] for k in range(n_keep)}
        dead = {self._col_src[k][0] for k in range(n_keep, len(self._col_src))}
        rgb0 = dict(self._col_src)
        alive = {v.raw_index: v for v in self.vehicles if v.raw_index is not None}
        new_veh = [v for v in self.vehicles if v.raw_index is None]
        new_cols = list(self.colours[len(self._col_src):])
        out, sec = [], None
        last_col = last_car = -1
        for i, line in enumerate(self._lines):
            body, cm = _split_comment(line)
            low = body.strip().lower()
            if sec is None:
                sec = self._section_of(line, self._KNOWN)
                out.append(line)
                continue
            if low == "end":
                if sec == "col" and new_cols:
                    at = last_col + 1 if last_col >= 0 else len(out)
                    out[at:at] = [f"{c.r},{c.g},{c.b}" for c in new_cols]
                    new_cols = []
                if sec == "car" and new_veh:
                    at = last_car + 1 if last_car >= 0 else len(out)
                    out[at:at] = [self._car_line(v) for v in new_veh]
                    new_veh = []
                sec = None
                out.append(line)
                continue
            if sec == "col" and i in rgb0:
                if i in dead:
                    continue
                c = cur[i]
                if (c.r, c.g, c.b) == rgb0[i]:
                    out.append(line)
                else:
                    toks = _SEP.split(body)
                    nums = [k for k, t in enumerate(toks) if _NUM.fullmatch(t or "")]
                    for k, val in zip(nums[:3], (c.r, c.g, c.b)):
                        toks[k] = str(val)
                    out.append("".join(toks) + cm)
                last_col = len(out) - 1
                continue
            if sec == "car" and i in self._veh_lines:
                v = alive.get(i)
                if v is None:
                    continue                                   # deleted vehicle
                if not v.changed:
                    out.append(line)
                elif v.name == v.orig[0] and len(v.palettes) == len(v.orig[1]):
                    toks = _SEP.split(body)
                    nums = [k for k, t in enumerate(toks) if _NUM.fullmatch(t or "")]
                    for k, val in zip(nums, [x for pr in v.palettes for x in pr]):
                        toks[k] = str(val)
                    out.append("".join(toks) + cm)
                else:
                    out.append(self._car_line(v, cm))
                last_car = len(out) - 1
                continue
            out.append(line)
        return self._eol.join(out)

    def save(self, path: str) -> bool:
        try:
            text = self.to_text()
            self._write(path, text)
            mine_v = sorted((v for v in self.vehicles if v.raw_index is not None), key=lambda v: v.raw_index) \
                + [v for v in self.vehicles if v.raw_index is None]
            fresh = CarColsFile(self._colour_cls)
            fresh._parse(path)
            for v, f in zip(mine_v, fresh.vehicles):
                v.raw_index, v.orig = f.raw_index, f.orig
            self.vehicles = mine_v
            self._lines, self._col_src, self._veh_lines, self._n_veh0 = \
                fresh._lines, fresh._col_src, fresh._veh_lines, fresh._n_veh0
            return True
        except Exception as ex:
            print(f"CarColsFile.save: {ex}")
            return False


class CarModsFile(_LineFile):
    """carmods.dat: .entries (vehicle + mods from the 'mods' section), .header_lines."""
    _KNOWN = ("link", "mods", "wheel")

    def __init__(self):
        self.entries: List[CarModEntry] = []
        self.header_lines: List[str] = []
        self.path = ""
        self._lines: List[str] = []
        self._eol = "\n"
        self._orig_lines: set = set()
        self._n0 = 0

    @property
    def dirty(self) -> bool:
        return len(self.entries) != self._n0 or any(e.changed for e in self.entries)

    def load(self, path: str) -> bool:
        try:
            self._parse(path)
            return True
        except Exception as ex:
            print(f"CarModsFile.load: {ex}")
            return False

    def _parse(self, path: str):
        self._read(path)
        self.entries, self._orig_lines = [], set()
        sec, first = None, None
        for i, line in enumerate(self._lines):
            s = _split_comment(line)[0].strip()
            if sec is None:
                k = self._section_of(line, self._KNOWN)
                if k:
                    sec = k
                    first = i if first is None else first
                continue
            if s.lower() == "end":
                sec = None
                continue
            if sec == "mods" and s:
                parts = [p.strip() for p in s.replace("\t", " ").split(",")]
                if len(parts) == 1:
                    parts = s.split()
                self.entries.append(CarModEntry(parts[0], [p for p in parts[1:] if p], i))
                self._orig_lines.add(i)
        self._n0 = len(self.entries)
        self.header_lines = self._lines[:first] if first else []

    def to_text(self) -> str:
        alive = {e.raw_index: e for e in self.entries if e.raw_index is not None}
        new = [e for e in self.entries if e.raw_index is None]
        out, sec, last = [], None, -1
        for i, line in enumerate(self._lines):
            body, cm = _split_comment(line)
            if sec is None:
                sec = self._section_of(line, self._KNOWN)
                out.append(line)
                continue
            if body.strip().lower() == "end":
                if sec == "mods" and new:
                    at = last + 1 if last >= 0 else len(out)
                    out[at:at] = [", ".join([e.vehicle] + e.mods) for e in new]
                    new = []
                sec = None
                out.append(line)
                continue
            if sec == "mods" and i in self._orig_lines:
                e = alive.get(i)
                if e is None:
                    continue
                out.append(line if not e.changed else ", ".join([e.vehicle] + e.mods) + (("\t" + cm) if cm else ""))
                last = len(out) - 1
                continue
            out.append(line)
        return self._eol.join(out)

    def save(self, path: str) -> bool:
        try:
            self._write(path, self.to_text())
            mine = sorted((e for e in self.entries if e.raw_index is not None), key=lambda e: e.raw_index) \
                + [e for e in self.entries if e.raw_index is None]
            fresh = CarModsFile()
            fresh._parse(path)
            for e, f in zip(mine, fresh.entries):
                e.raw_index, e.orig = f.raw_index, f.orig
            self.entries = mine
            self._lines, self._orig_lines, self._n0 = fresh._lines, fresh._orig_lines, fresh._n0
            return True
        except Exception as ex:
            print(f"CarModsFile.save: {ex}")
            return False
