#!/usr/bin/env python3
#this belongs in apps/components/Map_Editor/depends/map_integration.py - Version: 1
# X-Seti - September23 2026 - IMG Factory 1.6 - Map Workshop links to SCM / engine log / radar

"""
Map Workshop integration - script placements from main.scm (markers + jump
to SCM Workshop), engine load log replay, and radar tiles touched by edits.
"""

##Methods list -
# build_load_log
# tiles_touching

##class EngineLoadLogDialog: -
# __init__
# _export
# _filter

##class ScriptPlacementsDialog: -
# __init__
# closeEvent
# _open_in_scm
# _row_clicked
# _toggle_markers

import os

from PyQt6.QtWidgets import (QCheckBox, QDialog, QFileDialog, QHBoxLayout, QLabel, QLineEdit,
                             QPlainTextEdit, QPushButton, QTableWidget, QTableWidgetItem, QVBoxLayout)

KIND_COLOURS = {'object': (0.2, 0.9, 1.0), 'pickup': (1.0, 0.85, 0.1), 'cargen': (1.0, 0.35, 0.8)}


class ScriptPlacementsDialog(QDialog):
    """Table of script placements; click centres the view, button opens SCM Workshop at the opcode."""

    def __init__(self, workshop, scm_path, hits): #vers 1
        super().__init__(workshop)
        self.ws, self.path, self.hits = workshop, scm_path, hits
        self.setWindowTitle(f"Script Placements - {os.path.basename(scm_path)}")
        self.resize(760, 520)
        lay = QVBoxLayout(self)
        counts = {k: sum(1 for h in hits if h['kind'] == k) for k in KIND_COLOURS}
        lay.addWidget(QLabel(f"{len(hits)} found: {counts['object']} objects (cyan), {counts['pickup']} pickups "
                             f"(yellow), {counts['cargen']} car generators (pink)"))
        self.table = QTableWidget(len(hits), 6)
        self.table.setHorizontalHeaderLabels(["Kind", "Model", "Name", "X", "Y", "Z / offset"])
        loader = workshop._world_loader
        for r, h in enumerate(hits):
            obj = loader.get_object(h['model']) if isinstance(h['model'], int) and h['model'] >= 0 else None
            name = h['model_name'] or (obj.model_name if obj is not None else '')
            for c, v in enumerate((h['kind'], h['model'], name, f"{h['x']:.2f}", f"{h['y']:.2f}",
                                   f"{h['z']:.2f}  @0x{h['offset']:X}")):
                self.table.setItem(r, c, QTableWidgetItem(str(v)))
        self.table.setSelectionBehavior(QTableWidget.SelectionBehavior.SelectRows)
        self.table.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
        self.table.cellClicked.connect(self._row_clicked)
        self.table.cellDoubleClicked.connect(lambda r, _c: self._open_in_scm())
        self.table.resizeColumnsToContents()
        lay.addWidget(self.table, 1)
        row = QHBoxLayout()
        self.show_chk = QCheckBox("Show markers in viewport")
        self.show_chk.setChecked(True)
        self.show_chk.toggled.connect(self._toggle_markers)
        row.addWidget(self.show_chk)
        row.addStretch()
        b = QPushButton("Open in SCM Workshop")
        b.clicked.connect(self._open_in_scm)
        row.addWidget(b)
        lay.addLayout(row)
        self._toggle_markers(True)

    def _toggle_markers(self, on): #vers 1
        vp = getattr(self.ws, 'preview_widget', None)
        if vp is not None and hasattr(vp, 'set_script_markers'):
            vp.set_script_markers([(h['x'], h['y'], h['z'], KIND_COLOURS[h['kind']]) for h in self.hits] if on else [])

    def _row_clicked(self, row, _col): #vers 1
        """Centre the viewport on the placement."""
        h = self.hits[row]
        vp = getattr(self.ws, 'preview_widget', None)
        if vp is not None:
            vp._pan_x, vp._pan_y = -h['x'], -h['y']
            vp.update()

    def _open_in_scm(self): #vers 1
        """Open main.scm in SCM Workshop at the selected opcode."""
        row = self.table.currentRow()
        if row < 0:
            return
        from apps.components.Scm_Workshop.scm_workshop import open_scm_workshop
        w = open_scm_workshop(getattr(self.ws, 'main_window', None), self.path)
        if w is not None and hasattr(w, 'goto_offset'):
            w.goto_offset(self.hits[row]['offset'])

    def closeEvent(self, ev): #vers 1
        self._toggle_markers(False)
        super().closeEvent(ev)


def build_load_log(loader, lod_parent_of, streams_by_parent) -> list: #vers 1
    """Engine-order load log lines: DAT entries in order, IPL instances, then SA streams."""
    game = getattr(loader, 'game', '?')
    lines = [f"# Engine load replay - {game.upper()} - {len(loader.instances)} instances",
             "# 'placed' = instance created at load; the model itself streams in later near the player."]
    by_ipl = {}
    for i in loader.instances:
        by_ipl.setdefault(i.source_ipl, []).append(i)
    for phase, etype, path, ok in getattr(loader, 'load_log', []) or []:
        base = os.path.basename(path)
        lines.append(f"[{phase}] Loading {etype} {base}" + ("" if ok else "  - FAILED"))
        if etype == "IPL":
            for i in by_ipl.get(base, []):
                lines.append(f"    placed {i.model_name}.dff  id {i.model_id}  at "
                             f"({i.pos_x:.1f}, {i.pos_y:.1f}, {i.pos_z:.1f})")
            for stream in streams_by_parent.get(base, []):
                lines.append(f"  Streaming {stream}")
                for i in by_ipl.get(stream, []):
                    lod = f"  lod -> {i.lod_index}" if i.lod_index >= 0 else ""
                    lines.append(f"      placed {i.model_name}.dff  id {i.model_id}{lod}")
    return lines


class EngineLoadLogDialog(QDialog):
    """Read-only load replay with a filter box and text export."""

    def __init__(self, parent, lines): #vers 1
        super().__init__(parent)
        self.lines = lines
        self.setWindowTitle("Engine Load Log")
        self.resize(900, 640)
        lay = QVBoxLayout(self)
        row = QHBoxLayout()
        self.filt = QLineEdit()
        self.filt.setPlaceholderText("Filter (e.g. airport, .ipl, lod)")
        self.filt.textChanged.connect(self._filter)
        row.addWidget(self.filt, 1)
        b = QPushButton("Export...")
        b.clicked.connect(self._export)
        row.addWidget(b)
        lay.addLayout(row)
        self.text = QPlainTextEdit()
        self.text.setReadOnly(True)
        self.text.setLineWrapMode(QPlainTextEdit.LineWrapMode.NoWrap)
        lay.addWidget(self.text, 1)
        self._filter("")

    def _filter(self, txt): #vers 1
        t = txt.strip().lower()
        self.text.setPlainText("\n".join(l for l in self.lines if not t or t in l.lower()))

    def _export(self): #vers 1
        p, _ = QFileDialog.getSaveFileName(self, "Export load log", "load_log.txt", "Text (*.txt)")
        if p:
            with open(p, 'w', encoding='utf-8') as f:
                f.write("\n".join(self.lines) + "\n")


def tiles_touching(tiles, points, margin=0.0) -> list: #vers 1
    """Indices of radar tiles containing any (x, y) point (old or new positions)."""
    out = set()
    for t in tiles:
        for x, y in points:
            if t.min_x - margin <= x <= t.max_x + margin and t.min_y - margin <= y <= t.max_y + margin:
                out.add(t.index)
                break
    return sorted(out)
