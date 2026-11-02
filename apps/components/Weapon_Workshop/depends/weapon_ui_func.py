#this belongs in apps/components/Weapon_Workshop/depends/weapon_ui_func.py - Version: 2
# X-Seti - November 02 2026 - IMG Factory 1.6 - Weapon Workshop UI

"""
Weapon Workshop panels: file list (left); one tab per table (guns, melee,
aim offsets or III / VC weapons, melee combos, stats) and a text tab.
"""

##Methods list -
#
##class WeaponUIMixin: -
# _create_centre_panel
# _create_left_panel
# _wp_table

from PyQt6.QtWidgets import (QComboBox, QHBoxLayout, QLabel, QListWidget, QPlainTextEdit, QSplitter,
                             QTableWidget, QTabWidget, QVBoxLayout, QWidget)
from PyQt6.QtCore import Qt


class WeaponUIMixin: #vers 1
    """Panels for Weapon Workshop."""

    def _create_left_panel(self): #vers 1
        """Data files found and the folder they came from."""
        w = QWidget()
        lay = QVBoxLayout(w)
        lay.setContentsMargins(2, 2, 2, 2)
        self._wp_src_lbl = QLabel("Open the game's data folder, or weapon.dat / melee.dat / ar_stats.dat")
        self._wp_src_lbl.setWordWrap(True)
        lay.addWidget(self._wp_src_lbl)
        self._wp_files = QListWidget()
        lay.addWidget(self._wp_files, 1)
        return w

    def _wp_table(self) -> QTableWidget: #vers 1
        t = QTableWidget(0, 1)
        t.itemChanged.connect(self._wp_cell_changed)
        t.setAlternatingRowColors(True)
        return t

    def _create_centre_panel(self): #vers 1
        """Tabs filled per game by _wp_fill_tabs; Combos has a combo picker."""
        self._wp_tabs = QTabWidget()
        self._wp_tables = {}
        for key, title in (('guns', "Guns"), ('melee', "Melee weapons"), ('aim', "Aim offsets"),
                           ('weapons', "Weapons"), ('stats', "Stats")):
            t = self._wp_table()
            self._wp_tables[key] = t
            self._wp_tabs.addTab(t, title)
        combo = QWidget()
        cl = QVBoxLayout(combo)
        row = QHBoxLayout()
        row.addWidget(QLabel("Combo:"))
        self._wp_combo_pick = QComboBox()
        self._wp_combo_pick.currentIndexChanged.connect(self._wp_show_combo)
        row.addWidget(self._wp_combo_pick, 1)
        cl.addLayout(row)
        t = self._wp_table()
        self._wp_tables['combos'] = t
        cl.addWidget(t, 1)
        self._wp_tabs.addTab(combo, "Melee combos")
        text = QWidget()
        tl = QVBoxLayout(text)
        row = QHBoxLayout()
        row.addWidget(QLabel("File:"))
        self._wp_text_pick = QComboBox()
        self._wp_text_pick.currentIndexChanged.connect(self._wp_show_text)
        row.addWidget(self._wp_text_pick, 1)
        tl.addLayout(row)
        self._wp_text = QPlainTextEdit()
        self._wp_text.setLineWrapMode(QPlainTextEdit.LineWrapMode.NoWrap)
        tl.addWidget(self._wp_text, 1)
        self._wp_tabs.addTab(text, "Text")
        self._wp_tabs.currentChanged.connect(self._wp_tab_changed)
        return self._wp_tabs
