#this belongs in apps/methods/handling_file.py - Version: 1
# X-Seti - September 21 2026 - IMG Factory 1.6 - handling.cfg file model

"""handling_file.py - handling.cfg reader/writer that keeps the ORIGINAL
file. Comments, blank lines, the SA special lines (% boats, $ planes/helis,
! bikes, & animations), spacing and CRLF all stay as they were; a vehicle
line that was not edited is written back verbatim, an edited one only has the
changed values swapped in (its own whitespace and trailing ';' comment
survive); deleted lines are dropped and new ones go after the last vehicle.
An untouched file saves byte-identical."""

##Methods list -
# HandlingEntry
# HandlingParser
# detect_game
# header_fields

import copy
import os
import re
from pathlib import Path
from typing import List, Optional

_WS = re.compile(r'(\s+)')


def detect_game(n_values: int) -> str:
    """Token count of a vehicle line: III/LC = 32, VC = 33, SA = 36+."""
    if n_values >= 36:
        return 'SA'
    if n_values >= 33:
        return 'VC'
    return 'III'


_LEGEND = re.compile(r'^;\s*\(([A-Za-z]+)\)\s*(.*)$')


def header_fields(lines: List[str]) -> list:
    """Column definitions read from the file's own '> FIELD DESCRIPTIONS <'
    legend (works for III, VC, SA and mod files alike). Returns
    [(name, type, min, max, tooltip)] in column order, or [] if no legend."""
    out, on = [], False
    for ln in lines:
        low = ln.lower()
        if 'field descriptions' in low:
            on = True
            continue
        if on and 'the data' in low:
            break
        m = _LEGEND.match(ln.strip()) if on else None
        if not m:
            continue
        desc = m.group(2).split('//')[0]
        if 'not used' in desc.lower() or 'notused' in desc.lower():
            continue                   # a legend remark, not a column (SA lists '(E) (not used)' but the column exists as CoM.x)
        rng = re.search(r'\[([-\d.]+)\s*to\s*([-\d.]+)\]', desc)
        label = re.split(r'\s{2,}|\t|\[', desc.strip())[0].strip(' ,')
        label = re.sub(r'!+.*$', '', label).strip() or m.group(1)
        label = label.replace('TransmissionData.', '')
        if re.match(r'^[fnb][A-Z]', label):
            kind = {'f': 'float', 'n': 'int', 'b': 'bool'}[label[0]]
            label = label[1:]
        elif 'flags' in label.lower():
            kind = 'hex'
        elif 'identifier' in label.lower() or 'anim group' in label.lower():
            kind = 'str'
        elif 'lights' in label.lower():
            kind = 'int'
        else:
            kind = 'float'
        label = label.split(' (')[0].split('.')[-1] if label.lower().startswith(('front lights', 'rear lights')) else label
        label = label.replace(' ', '').replace('-', '')[:32]
        if not out:
            label = 'HandlingName'
        if label.lower() in ('flags', 'modelflags', 'handlingflags'):
            kind = 'hex'
        if 'DriveType' in label or 'EngineType' in label:
            kind = 'char'
        lo, hi = (float(rng.group(1)), float(rng.group(2))) if rng else \
                 ((-100000.0, 100000.0) if kind == 'float' else (0, 999999))
        out.append((label, kind, lo, hi, m.group(2).strip()))
    return out


class HandlingEntry:
    """One vehicle line: values[0] is the handling name."""
    __slots__ = ('values', 'comment', 'raw_index', 'orig')

    def __init__(self, values=None, comment: str = "", raw_index: Optional[int] = None):
        self.values: List[str] = list(values or [])
        self.comment = comment
        self.raw_index = raw_index
        self.orig = tuple(self.values) if raw_index is not None else None

    @property
    def name(self) -> str:
        return self.values[0] if self.values else ''

    @property
    def changed(self) -> bool:
        return self.orig is None or tuple(self.values) != self.orig

    def copy_as_new(self) -> "HandlingEntry":
        return HandlingEntry(list(self.values), self.comment, None)

    @staticmethod
    def from_line(line: str) -> Optional["HandlingEntry"]:
        s = line.strip()
        if not s or s.startswith(';') or s[0] in ('%', '$', '!', '&'):
            return None
        body, sc, cm = s.partition(';')
        parts = body.split()
        if len(parts) < 10:
            return None
        return HandlingEntry(parts, sc + cm if sc else "")

    def new_line(self) -> str:
        return '\t'.join(self.values) + (f'\t{self.comment}' if self.comment else '')

    def patched_line(self, raw: str) -> str:
        """raw with only the changed values replaced (whitespace and the
        trailing comment stay as they were)."""
        body, sc, cm = raw.partition(';')
        toks = _WS.split(body)
        # toks alternates: leading-ws?, word, ws, word ... - find the word slots
        slots = [i for i, t in enumerate(toks) if t and not t.isspace()]
        orig = self.orig or ()
        for k, val in enumerate(self.values):
            if k < len(orig) and orig[k] == val:
                continue
            if k < len(slots):
                toks[slots[k]] = val
            else:
                toks.append('\t' + val)
        return ''.join(toks) + sc + cm


class HandlingParser:
    def __init__(self):
        self.entries: List[HandlingEntry] = []
        self._lines: List[str] = []
        self.game = 'VC'
        self.path = ""
        self.eol = "\n"
        self._count0 = 0

    @property
    def header_lines(self) -> List[str]:
        first = min((e.raw_index for e in self.entries if e.raw_index is not None), default=len(self._lines))
        return self._lines[:first]

    @property
    def dirty(self) -> bool:
        return len(self.entries) != self._count0 or any(e.changed for e in self.entries)

    def load(self, path: str) -> bool:
        try:
            text = Path(path).read_bytes().decode("latin1")
        except Exception as ex:
            print(f"HandlingParser.load error: {ex}")
            return False
        self._load_text(text)
        self.path = path
        return True

    def _load_text(self, text: str):
        self.eol = "\r\n" if "\r\n" in text else "\n"
        self._lines = text.split(self.eol)
        self.entries = []
        for n, line in enumerate(self._lines):
            e = HandlingEntry.from_line(line)
            if e is not None:
                e.raw_index = n
                e.orig = tuple(e.values)
                self.entries.append(e)
        self.game = detect_game(max((len(e.values) for e in self.entries), default=0))
        self._count0 = len(self.entries)

    def to_text(self) -> str:
        alive = {e.raw_index: e for e in self.entries if e.raw_index is not None}
        new = [e for e in self.entries if e.raw_index is None]
        out, last = [], -1
        for i, line in enumerate(self._lines):
            e = alive.get(i)
            if e is None and HandlingEntry.from_line(line) is not None:
                continue                                   # a vehicle that was deleted
            if e is not None:
                out.append(e.patched_line(line) if e.changed else line)
                last = len(out) - 1
            else:
                out.append(line)
        if new:
            at = last + 1 if last >= 0 else len(out)
            out[at:at] = [e.new_line() for e in new]
        return self.eol.join(out)

    def save(self, path: str) -> bool:
        """Atomic write (temp file in the same folder, then swap in), then
        re-baseline against what was written, keeping the entry objects."""
        import tempfile
        text = self.to_text()
        d = os.path.dirname(os.path.abspath(path))
        fd, tmp = tempfile.mkstemp(dir=d, prefix=".hnd_", suffix=".tmp")
        try:
            with os.fdopen(fd, "wb") as f:
                f.write(text.encode("latin1", errors="replace"))
            if os.path.exists(path):
                try:
                    os.chmod(tmp, os.stat(path).st_mode & 0o7777)
                except OSError:
                    pass
            os.replace(tmp, path)
        except Exception as ex:
            if os.path.exists(tmp):
                os.unlink(tmp)
            print(f"HandlingParser.save error: {ex}")
            return False
        keep = sorted((e for e in self.entries if e.raw_index is not None), key=lambda e: e.raw_index) \
            + [e for e in self.entries if e.raw_index is None]
        self._load_text(text)
        for old, new in zip(keep, self.entries):
            old.values, old.raw_index, old.orig = list(new.values), new.raw_index, new.orig
        self.entries = keep
        self.path = path
        return True
