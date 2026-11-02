#this belongs in apps/components/Weapon_Workshop/depends/weapon_logic_func.py - Version: 3
# X-Seti - November 02 2026 - IMG Factory 1.6 - Weapon Workshop logic

"""
Weapon Workshop: load weapon.dat, melee.dat, ar_stats.dat from a data
folder, edit cells in place (spacing kept), flags explained in tooltips,
text tab for anything else, save with backups.
"""

##Methods list -
#
##class WeaponLogicMixin: -
# _open_file
# _save_file
# _wp_autoload
# _wp_cell_changed
# _wp_fill
# _wp_fill_table
# _wp_show_combo
# _wp_show_text
# _wp_tab_changed

import os

from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import QFileDialog, QMessageBox, QTableWidgetItem

from apps.methods.weapon_files import (AR_STATS, COMBO_ATTACK, III_FLAGS, III_WEAPON, MELEE_FLAGS, SA_AIM,
                                       SA_GUN, SA_GUN_FLAGS, SA_MELEE, VC_FLAGS, VC_WEAPON, data_rows,
                                       flag_names, melee_combos, set_word)

FILES = ('weapon.dat', 'melee.dat', 'ar_stats.dat')


class WeaponLogicMixin: #vers 1
    """Open / edit / save the weapon data files."""

    def _open_file(self, path=None): #vers 2
        """A data folder, or one of its weapon files (the others beside it load too)."""
        if not path:
            path, _ = QFileDialog.getOpenFileName(self, "Open weapon data", "",
                                                  "Weapon data (weapon.dat melee.dat ar_stats.dat);;All files (*)")
        if not path:
            return
        folder = path if os.path.isdir(path) else os.path.dirname(path)
        names = {f.lower(): f for f in os.listdir(folder)}
        self._wp_paths, self._wp_text_by, self._wp_dirty = {}, {}, set()
        for f in FILES:
            if f in names:
                p = os.path.join(folder, names[f])
                with open(p, 'rb') as fh:
                    self._wp_text_by[f] = fh.read().decode('latin-1').replace('\r\n', '\n')
                self._wp_paths[f] = p
        if 'weapon.dat' not in self._wp_paths:
            QMessageBox.warning(self, self.App_name, f"No weapon.dat in {folder}")
            return
        self.WS.set('last_path', folder)
        self.WS.add_recent(folder)
        self._wp_src_lbl.setText(folder)
        self._wp_files.clear()
        self._wp_files.addItems(list(self._wp_paths))
        self._wp_fill()
        self._wm_load_game(os.path.dirname(folder))       # weapon models of this game

    def _wp_autoload(self, game_root: str = ''): #vers 1
        """Game folder given: its data folder; else the last one opened."""
        if game_root and os.path.isdir(game_root):
            data = next((os.path.join(game_root, d) for d in os.listdir(game_root) if d.lower() == 'data'), None)
            if data and any(f.lower() == 'weapon.dat' for f in os.listdir(data)):
                self._open_file(data)
                return
        last = self.WS.get('last_path', '')
        if last and os.path.isdir(last):
            self._open_file(last)

    def _wp_fill(self): #vers 1
        """Tables for the game found: SA guns / melee / aim, or III / VC weapons; combos, stats."""
        txt = self._wp_text_by.get('weapon.dat', '')
        sa = bool(data_rows(txt, '$'))
        rows = data_rows(txt)
        game = 'sa' if sa else ('vc' if rows and len(rows[0][1]) >= 26 else 'iii')
        self._wp_game = game
        self._wp_flags = {'guns': (SA_GUN, SA_GUN_FLAGS), 'melee': (SA_MELEE, MELEE_FLAGS),
                          'weapons': ((VC_WEAPON, VC_FLAGS) if game == 'vc' else (III_WEAPON, III_FLAGS))}
        self._wp_fill_table('guns', 'weapon.dat', data_rows(txt, '$') if sa else [], SA_GUN, 1)
        self._wp_fill_table('melee', 'weapon.dat', data_rows(txt, '\xa3') if sa else [], SA_MELEE, 1)
        self._wp_fill_table('aim', 'weapon.dat', data_rows(txt, '%') if sa else [], SA_AIM, 1)
        self._wp_fill_table('weapons', 'weapon.dat', [] if sa else rows,
                            VC_WEAPON if game == 'vc' else III_WEAPON, 0)
        self._wp_fill_table('stats', 'ar_stats.dat', data_rows(self._wp_text_by.get('ar_stats.dat', '')), AR_STATS, 0)
        self._wp_combos = melee_combos(self._wp_text_by.get('melee.dat', ''))
        self._wp_combo_pick.blockSignals(True)
        self._wp_combo_pick.clear()
        self._wp_combo_pick.addItems([c['name'] for c in self._wp_combos])
        self._wp_combo_pick.blockSignals(False)
        self._wp_show_combo(0)
        for key, idx in (('guns', 0), ('melee', 1), ('aim', 2), ('weapons', 3), ('stats', 4), ('combos', 5)):
            has = self._wp_tables[key].rowCount() > 0
            self._wp_tabs.setTabVisible(idx, has)
        self._wp_text_pick.blockSignals(True)
        self._wp_text_pick.clear()
        self._wp_text_pick.addItems(list(self._wp_text_by))
        self._wp_text_pick.blockSignals(False)
        self._wp_show_text(0)
        self._set_status(f"{game.upper()} weapon data: " + ", ".join(self._wp_text_by))

    def _wp_fill_table(self, key: str, file: str, rows, cols, skip: int): #vers 1
        """Rows into a table; each item remembers (file, line, word index); flags get tooltips."""
        t = self._wp_tables[key]
        self._wp_loading = True
        t.clear()
        width = max([len(w) for _i, w in rows] + [len(cols), 1])
        t.setColumnCount(width)
        t.setHorizontalHeaderLabels(list(cols) + [f"Col {i + 1}" for i in range(len(cols), width)])
        t.setRowCount(len(rows))
        flags = self._wp_flags.get(key)
        for r, (line, words) in enumerate(rows):
            for c in range(width):
                it = QTableWidgetItem(words[c] if c < len(words) else '')
                it.setData(Qt.ItemDataRole.UserRole, (file, line, c + skip))
                if flags and c < len(cols) and cols[c] == 'Flags' and c < len(words):
                    it.setToolTip(", ".join(flag_names(words[c], flags[1])) or "no flags")
                t.setItem(r, c, it)
        t.resizeColumnsToContents()
        self._wp_loading = False

    def _wp_show_combo(self, idx: int): #vers 1
        """One melee.dat combo block as rows."""
        combos = getattr(self, '_wp_combos', [])
        rows = combos[idx]['rows'] if 0 <= idx < len(combos) else []
        self._wp_fill_table('combos', 'melee.dat', rows, COMBO_ATTACK, 0)
        t = self._wp_tables['combos']
        for r, (_line, words) in enumerate(rows):
            if words and words[0] == 'FLAGS' and len(words) > 1:
                t.item(r, 1).setToolTip(", ".join(flag_names(words[1].replace('0x', ''), MELEE_FLAGS)))

    def _wp_cell_changed(self, item): #vers 1
        """Cell edited: replace that word in its file line."""
        if getattr(self, '_wp_loading', False):
            return
        file, line, word = item.data(Qt.ItemDataRole.UserRole)
        value = item.text().strip()
        if not value or any(ch.isspace() for ch in value):
            self._set_status("One word per cell (no spaces)")
            return
        lines = self._wp_text_by[file].split('\n')
        lines[line] = set_word(lines[line], word, value)
        self._wp_text_by[file] = '\n'.join(lines)
        self._wp_dirty.add(file)
        self._set_status(f"{file}: line {line + 1} changed (not saved)")

    def _wp_show_text(self, idx: int): #vers 1
        files = list(getattr(self, '_wp_text_by', {}))
        self._wp_text_file = files[idx] if 0 <= idx < len(files) else None
        self._wp_text.setPlainText(self._wp_text_by.get(self._wp_text_file, '') if self._wp_text_file else '')

    def _wp_tab_changed(self, idx: int): #vers 1
        """Leaving the Text tab: its edits go back into the file and the tables."""
        prev, self._wp_prev_tab = getattr(self, '_wp_prev_tab', -1), idx
        text_tab = self._wp_tabs.count() - 1
        f = getattr(self, '_wp_text_file', None)
        if prev == text_tab and f and self._wp_text.toPlainText() != self._wp_text_by.get(f):
            self._wp_text_by[f] = self._wp_text.toPlainText()
            self._wp_dirty.add(f)
            cur = self._wp_combo_pick.currentIndex()
            self._wp_fill()
            self._wp_combo_pick.setCurrentIndex(cur)
        elif idx == text_tab:
            self._wp_show_text(self._wp_text_pick.currentIndex())

    def _save_file(self): #vers 1
        """Write the changed files back (CRLF, backups by safe_write_bytes)."""
        if self._wp_tabs.currentIndex() == self._wp_tabs.count() - 1:
            self._wp_tab_changed(-1)
        dirty = sorted(getattr(self, '_wp_dirty', set()))
        if not dirty:
            self._set_status("Nothing changed")
            return
        from apps.methods.file_backup import safe_write_bytes
        for f in dirty:
            safe_write_bytes(self._wp_paths[f], self._wp_text_by[f].replace('\n', '\r\n').encode('latin-1'))
        self._wp_dirty = set()
        self._set_status("Saved " + ", ".join(dirty))
