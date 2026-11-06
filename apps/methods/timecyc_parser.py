#this belongs in apps/methods/timecyc_parser.py - Version: 1
# X-Seti - November 03 2026 - IMG Factory 1.6 - timecyc.dat parser

"""
timecyc.dat / timecycp.dat reader and writer (III, VC, SA); shared by
Timecyc Workshop and the 3D viewport sky.
"""

##Methods list -
# _fmt_value
#
##class TimecycParser: -
# __init__
# _detect_game
# _get_game_layout
# _parse_line
# dirty
# get_row
# load
# save
# to_text
#
##class TimecycRow: -
# changed

import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import List, Optional


@dataclass
class TimecycRow: #vers 2
    weather: int = 0
    time:    int = 0
    values:  List[int] = field(default_factory=lambda: [0] * 36)
    comment: str = ""
    raw_index: int = -1            # line index in the loaded file (-1 = none)
    orig: tuple = ()               # values as loaded, to know what changed

    @property
    def changed(self) -> bool:
        return tuple(self.values) != self.orig


def _fmt_value(v, old_token: str = "") -> str:
    if isinstance(v, float) or '.' in old_token:
        t = f"{float(v):.4f}".rstrip('0')
        return t + '0' if t.endswith('.') else t
    return str(int(v))


class TimecycParser: #vers 2
    """timecyc.dat / timecycp.dat. Keeps the original lines (section comments,
    spacing, CRLF); only rows whose values changed are rewritten, and only the
    changed numbers inside them - an untouched file saves byte-identical."""

    def __init__(self): #vers 2
        self.rows:         List[TimecycRow] = []
        self.header_lines: List[str]        = []
        self.game:         str              = 'VC'
        self.cols_per_row: int              = 33
        self._lines:       List[str]        = []
        self._eol:         str              = "\n"

    @property
    def dirty(self) -> bool:
        return any(r.changed for r in self.rows)

    def _detect_game(self, num_values: int, filename: str = '') -> str: #vers 3
        # LC/GTA3=40 fields, VC=52 fields, SA=51 fields, timecycp=52 fields (SA PSP)
        import os as _os
        if _os.path.basename(filename).lower() == 'timecycp.dat': return 'SA'
        if num_values >= 52: return 'VC'
        if num_values >= 51: return 'SA'
        if num_values >= 40: return 'GTA3'
        return 'GTA3'

    def _get_game_layout(self) -> tuple: #vers 1
        """Return (n_weathers, n_times) for current game.
        File ordering is always weather-major (all times for weather0, then weather1 etc).
        GTA3/LC: 4 weathers x 24 times = 96 rows (but we treat as 8x12 by convention)
        VC:      7 weathers x 24 times = 168 rows
        SA:      23 weathers x 8 times = 184 rows
        """
        if self.game == 'SA':   return 23, 8
        if self.game == 'GTA3': return 8, 12   # 8 logical weathers, 12 time slots
        return 7, 24  # VC

    def _parse_line(self, line: str, weather: int, time: int) -> Optional[TimecycRow]: #vers 3
        """Parse one data line into a TimecycRow."""
        s = line.strip()
        if not s or s.startswith('/'):
            return None
        comment = ""
        if '//' in s:
            idx = s.index('//')
            comment = s[idx:]
            s = s[:idx].strip()
        parts = s.split()
        if len(parts) < 10:
            return None
        try:
            values = [(float(p) if '.' in p else int(float(p))) for p in parts]
        except ValueError:
            return None
        return TimecycRow(weather=weather, time=time, values=values, comment=comment)

    def load(self, path: str, known_game: str = None) -> bool: #vers 3
        try:
            text = Path(path).read_bytes().decode("latin1")
            self._eol = "\r\n" if "\r\n" in text else "\n"
            self._lines = text.split(self._eol)
            self.rows.clear()
            self.header_lines.clear()
            first = None
            for ln in self._lines:
                s = ln.strip()
                if s and not s.startswith('/'):
                    parts = s.split('//')[0].split()
                    if len(parts) >= 10:
                        first = len(parts)
                        break
            if known_game:
                self.game = 'VC' if known_game.lower() == 'sol' else known_game.upper()
            elif first:
                self.game = self._detect_game(first, path)
            if first:
                self.cols_per_row = first
            n_weathers, n_times = self._get_game_layout()
            self.header_lines = [ln for ln in self._lines if ln.strip().startswith('/')][:3]
            row_idx = 0
            for i, ln in enumerate(self._lines):
                r = self._parse_line(ln, row_idx // n_times, row_idx % n_times)
                if r:
                    r.raw_index, r.orig = i, tuple(r.values)
                    self.rows.append(r)
                    row_idx += 1
            return True
        except Exception as ex:
            print(f"TimecycParser.load: {ex}")
            return False

    def to_text(self) -> str: #vers 1
        by_line = {r.raw_index: r for r in self.rows}
        out = []
        for i, ln in enumerate(self._lines):
            r = by_line.get(i)
            if r is None or not r.changed:
                out.append(ln)
                continue
            body, sl, cm = ln.partition('//')
            toks = re.split(r'(\s+)', body)
            slots = [k for k, t in enumerate(toks) if t and not t.isspace()]
            for k, (old, new) in enumerate(zip(r.orig, r.values)):
                if old != new and k < len(slots):
                    toks[slots[k]] = _fmt_value(new, toks[slots[k]])
            out.append(''.join(toks) + sl + cm)
        return self._eol.join(out)

    def save(self, path: str) -> bool: #vers 2
        """Atomic write (temp file in the same folder, then swap in), then
        re-baseline so `changed` is false again."""
        import tempfile
        try:
            data = self.to_text().encode("latin1", errors="replace")
            d = os.path.dirname(os.path.abspath(path))
            fd, tmp = tempfile.mkstemp(dir=d, prefix=".tcy_", suffix=".tmp")
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
            self._lines = data.decode("latin1").split(self._eol)
            for r in self.rows:
                r.orig = tuple(r.values)
            return True
        except Exception as ex:
            print(f"TimecycParser.save: {ex}")
            return False

    def get_row(self, weather: int, time: int) -> Optional[TimecycRow]: #vers 1
        for r in self.rows:
            if r.weather == weather and r.time == time:
                return r
        return None
