#this belongs in apps/methods/asset_integrity.py - Version: 1
# X-Seti - September 20 2026 - IMG Factory 1.6 - Asset Integrity Check

"""asset_integrity.py - Read-only consistency report over a loaded
MasterIDEResult plus its IPL files. Writes nothing. TXD sharing
(e.g. generic.txd used ~30 times) is normal and only reported as
information, never as a problem."""

##Methods list -
# IntegrityReport
# check_integrity
# format_report
# show_integrity_dialog

import os
import re
from collections import Counter
from dataclasses import dataclass, field
from typing import List

from apps.methods.id_reassign import (
    _ID_DECLARING_SECTIONS, _IPL_ID_SECTIONS, _PATH_HEADER_RE, find_free_id_gaps)


@dataclass
class IntegrityReport: #vers 1
    orphan_ipl: list = field(default_factory=list)     # (file, line_no, id, name, reason)
    orphan_2dfx: list = field(default_factory=list)    # (file, id)
    orphan_path: list = field(default_factory=list)    # (file, line_no, id, name)
    dup_names: list = field(default_factory=list)      # (name, [(id, file)])
    dup_ids: list = field(default_factory=list)        # (id, [(name, file)])
    txd_usage: list = field(default_factory=list)      # (txd, count) high to low - info only
    id_used: int = 0
    id_max: int = 0
    id_gaps: list = field(default_factory=list)        # (start, end)

    @property
    def problem_count(self): #vers 1
        return (len(self.orphan_ipl) + len(self.orphan_2dfx) + len(self.orphan_path)
                + len(self.dup_names) + len(self.dup_ids))


def _read_sections(path, sections):
    """Yield (line_no, stripped_line) for lines inside the given sections."""
    try:
        lines = open(path, "r", encoding="ascii", errors="ignore").readlines()
    except Exception:
        return
    cur = None
    for n, raw in enumerate(lines, 1):
        s = raw.split("#")[0].strip()
        low = s.lower()
        if low == "end":
            cur = None
        elif cur is None and low in sections:
            cur = low
        elif cur and s:
            yield n, s


def check_integrity(result, ipl_paths: List[str]) -> IntegrityReport: #vers 1
    """Orphaned IPL placements / 2dfx / path blocks, duplicate IDs and
    names, TXD usage (info), ID-space summary. IPL/path checks are only
    accurate when every IDE of the game is loaded in result."""
    rep = IntegrityReport()
    declared = {}   # id -> lowercase name
    for sec in _ID_DECLARING_SECTIONS:
        for o in result.objects_by_section.get(sec, []):
            declared.setdefault(o.model_id, o.model_name.lower())

    for o in result.objects_by_section.get("2dfx", []):
        if o.model_id not in declared:
            rep.orphan_2dfx.append((o.source_ide, o.model_id))

    for path in ipl_paths:
        for n, s in _read_sections(path, _IPL_ID_SECTIONS):
            parts = [p.strip() for p in s.split(",", 2)]
            try:
                mid = int(parts[0])
            except ValueError:
                continue
            name = parts[1] if len(parts) > 1 else ""
            if mid not in declared:
                rep.orphan_ipl.append((path, n, mid, name, "ID not declared in any IDE"))
            elif name and declared[mid] != name.lower():
                rep.orphan_ipl.append((path, n, mid, name, f"name differs from IDE ({declared[mid]})"))

    for path in result.source_files:
        in_path = False
        try:
            lines = open(path, "r", encoding="ascii", errors="ignore").readlines()
        except Exception:
            continue
        for n, raw in enumerate(lines, 1):
            low = raw.split("#")[0].strip().lower()
            if not in_path:
                in_path = low == "path"
            elif low == "end":
                in_path = False
            else:
                m = _PATH_HEADER_RE.match(raw.rstrip("\n"))
                if m and int(m.group("id")) not in declared:
                    rep.orphan_path.append((path, n, int(m.group("id")), m.group("name").strip()))

    for nc in getattr(result, "name_collisions", []):
        ents = getattr(nc, "entries", [])
        rep.dup_names.append((getattr(nc, "model_name", ""), [(e[0], e[-1] if len(e) > 1 else "") if isinstance(e, tuple) else (str(e), "") for e in ents]))
    for c in getattr(result, "collisions", []):
        rep.dup_ids.append((c.model_id, [(e[0], e[-1] if len(e) > 1 else "") if isinstance(e, tuple) else (str(e), "") for e in c.entries]))

    txd = Counter()
    for sec in ("objs", "tobj"):
        for o in result.objects_by_section.get(sec, []):
            txd[o.txd_name.lower()] += 1
    rep.txd_usage = txd.most_common()

    if declared:
        rep.id_used = len(declared)
        rep.id_max = max(declared)
        rep.id_gaps = find_free_id_gaps(result, min(declared), rep.id_max)
    return rep


def format_report(rep: IntegrityReport, top_txd: int = 15) -> str: #vers 1
    """Plain-text report for the dialog / export."""
    b = lambda p: os.path.basename(p)
    out = [f"Problems found: {rep.problem_count}", ""]

    def sect(title, rows):
        out.append(f"== {title}: {len(rows)}")
        out.extend(rows[:200])
        if len(rows) > 200:
            out.append(f"   ... {len(rows) - 200} more")
        out.append("")

    sect("IPL placements with no matching IDE entry",
         [f"   {b(f)}:{n}  id {i} '{nm}' - {why}" for f, n, i, nm, why in rep.orphan_ipl])
    sect("2dfx entries with no object",
         [f"   {b(f)}  id {i}" for f, i in rep.orphan_2dfx])
    sect("path blocks with no vehicle/object",
         [f"   {b(f)}:{n}  id {i} '{nm}'" for f, n, i, nm in rep.orphan_path])
    sect("Duplicate model names",
         [f"   {nm}: " + ", ".join(f"{i}@{b(str(f))}" for i, f in e) for nm, e in rep.dup_names])
    sect("Duplicate IDs",
         [f"   {i}: " + ", ".join(f"{nm}@{b(str(f))}" for nm, f in e) for i, e in rep.dup_ids])

    out.append("== TXD usage (information only - sharing is normal)")
    out.extend(f"   {t}: {c}" for t, c in rep.txd_usage[:top_txd])
    out.append(f"   ({len(rep.txd_usage)} distinct TXDs)")
    out.append("")
    out.append("== ID space")
    out.append(f"   {rep.id_used} IDs in use, highest {rep.id_max}, {len(rep.id_gaps)} free gaps")
    for a, z in rep.id_gaps[:30]:
        out.append(f"   free {a}" if a == z else f"   free {a}-{z}")
    return "\n".join(out)


def show_integrity_dialog(parent, text: str): #vers 1
    """Read-only report window with Export."""
    from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QPlainTextEdit,
                                 QPushButton, QFileDialog)
    from PyQt6.QtGui import QFont
    dlg = QDialog(parent)
    dlg.setWindowTitle("Integrity Check")
    dlg.resize(760, 560)
    lay = QVBoxLayout(dlg)
    box = QPlainTextEdit(text)
    box.setReadOnly(True)
    box.setFont(QFont("monospace"))
    lay.addWidget(box)
    row = QHBoxLayout()
    exp = QPushButton("Export...")
    close = QPushButton("Close")
    row.addStretch()
    row.addWidget(exp)
    row.addWidget(close)
    lay.addLayout(row)

    def _export():
        path, _ = QFileDialog.getSaveFileName(dlg, "Export report", "integrity_report.txt", "Text (*.txt)")
        if path:
            with open(path, "w", encoding="utf-8") as f:
                f.write(text)
    exp.clicked.connect(_export)
    close.clicked.connect(dlg.accept)
    dlg.exec()
