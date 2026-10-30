#this belongs in apps/components/Retro_GTA_Workshop/depends/retro_block_func.py - Version: 11
# X-Seti - October 23 2026 - IMG Factory 1.6 - Retro GTA Workshop block editor

"""
Block dock for Retro GTA Workshop: the levels of the picked map cell
(GTA1 6, GTA2 8), and an editor for one block - face tiles (lid, left,
right, top, bottom) with the flags that game has, ground type, slope and
arrows. The map class turns faces into one word layout, so one editor
serves both games. Every edit is undoable; lid painting uses the same path.
"""

##class RetroBlockMixin: -
# _block_apply
# _block_changed
# _block_fill_editor
# _block_help
# _block_help_button
# _block_level_picked
# _block_map_opened
# _block_paint
# _block_row_values
# _block_set
# _block_show_cell
# _block_undo
# _create_block_dock
# _tile_preview

from PyQt6.QtGui import QImage, QPixmap
from PyQt6.QtWidgets import (QAbstractItemView, QComboBox, QGridLayout,
                             QHBoxLayout, QLabel, QSpinBox, QTableWidget,
                             QTableWidgetItem, QVBoxLayout, QWidget)

from apps.methods.gta2_map import face_tile

FACES = ('lid', 'left', 'right', 'top', 'bottom')
MAX_LEVELS, MAX_ARROWS = 8, 8
BLOCK_HELP = ("Click a block on the map, pick a level row, edit its faces below.\n"
              "Tiles panel: pick a tile, Use on face.\n"
              "Arrows: traffic routes (GTA2 green / red sets, GTA1 one set).\n"
              "New block: add a block on this level. Delete: remove it. Clear: empty block.\n"
              "Right-click the map: snapshot, export / import map PNG.")


class RetroBlockMixin: #vers 2
    """Block inspector / editor and lid painting for RetroGTAWorkshop."""

    def _create_block_dock(self) -> QWidget: #vers 8
        """Levels table | splitter | editor rows that never stack: face / tile / rotation + texture,
        flags, light / ground / slope, 8 arrows, New / Delete / Clear; narrow: icons only."""
        from PyQt6.QtCore import Qt
        from PyQt6.QtWidgets import QSplitter, QToolButton
        self._cell, self._edit_z, self._undo_stack = None, 0, []
        page = QWidget()
        lay = QVBoxLayout(page)
        lay.setContentsMargins(4, 4, 4, 4)
        t = QTableWidget(MAX_LEVELS, 7)
        t.setHorizontalHeaderLabels(["Lid", "Left", "Right", "Top", "Bottom", "Ground", "Slope"])
        t.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        t.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        t.setSelectionMode(QAbstractItemView.SelectionMode.SingleSelection)
        t.currentCellChanged.connect(lambda r, _c, _pr, _pc: self._block_level_picked(r))
        t.setMinimumHeight(t.verticalHeader().defaultSectionSize() * 4 + t.horizontalHeader().height() + 4)
        self._level_table = t
        ed = QWidget()
        el = QVBoxLayout(ed)
        el.setContentsMargins(0, 0, 0, 0)
        from apps.methods.fold_widgets import ButtonRow, IconLabel, fit_height
        from apps.methods.imgfactory_svg_icons import SVGIconFactory as I
        ic = self._get_icon_color()

        def tool(text, icon, tip, checkable=False):
            b = QToolButton()
            b.setText(text)
            b.setIcon(icon(16, ic))
            b.setToolTip(tip)
            b.setCheckable(checkable)
            return b
        self._face_combo = QComboBox()
        self._face_combo.addItems([f.title() for f in FACES])
        self._face_combo.currentIndexChanged.connect(lambda _i: self._block_fill_editor())
        self._tile_spin = QSpinBox()
        self._tile_spin.setRange(0, 1023)
        self._rot_combo = QComboBox()
        self._rot_combo.addItems(["0", "90", "180", "270"])
        form = QGridLayout()                             # rows stay rows; labels become icons
        for r, (text, icon, w) in enumerate((("Face:", I.face_select_icon, self._face_combo),
                                             ("Tile:", I.grid_icon, self._tile_spin),
                                             ("Rotation:", I.rotate_cw_icon, self._rot_combo))):
            form.addWidget(IconLabel(text, icon(16, ic)), r, 0)
            form.addWidget(w, r, 1)
        form.setColumnStretch(1, 1)
        self._tile_prev = QLabel()
        self._tile_prev.setFixedSize(72, 72)
        self._tile_prev.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._tile_prev.setToolTip("Texture of this face (rotation / flip shown)")
        top = QHBoxLayout()
        top.addLayout(form, 1)
        top.addWidget(self._tile_prev, 0, Qt.AlignmentFlag.AlignTop)
        el.addLayout(top)
        self._flip_chk = tool("Flip", I.flip_horz_icon, "Flip: face mirrored", True)
        self._flat_chk = tool("Flat", I.show_backfaces_icon, "Flat: see-through face, both sides drawn", True)
        self._wall_chk = tool("Wall", I.surface_brick_icon, "Wall: cars and peds collide", True)
        self._bullet_chk = tool("Bullet wall", I.get_shield_icon, "Bullet wall: stops bullets", True)
        flags = QHBoxLayout()
        flags.addWidget(IconLabel("Flags:", I.get_shield_icon(16, ic)))
        flags.addWidget(ButtonRow([self._flip_chk, self._flat_chk, self._wall_chk, self._bullet_chk]), 1)
        el.addLayout(flags)
        self._light_spin = QSpinBox()
        self._light_spin.setRange(0, 3)
        self._light_spin.setToolTip("Lid lighting level 0-3 (GTA2)")
        self._ground_combo = QComboBox()
        self._slope_spin = QSpinBox()
        self._slope_spin.setRange(0, 63)
        self._slope_spin.setToolTip("Slope code 0-63 (1-44 gradients, 45+ diagonals / partial blocks)")
        for c in (self._face_combo, self._ground_combo):
            c.setSizeAdjustPolicy(QComboBox.SizeAdjustPolicy.AdjustToMinimumContentsLengthWithIcon)
            c.setMinimumContentsLength(4)
        for w in (self._light_spin, self._slope_spin):  # 0-63: two digits + arrows
            w.setMinimumWidth(44)
        self._ground_combo.setMinimumWidth(60)
        row = QHBoxLayout()
        for text, icon, w in (("Light:", I.light_icon, self._light_spin),
                              ("Ground:", I.surface_grass_icon, self._ground_combo),
                              ("Slope:", I.arrow_up_icon, self._slope_spin)):
            row.addWidget(IconLabel(text, icon(16, ic)))
            row.addWidget(w, 1 if w is self._ground_combo else 0)
        el.addLayout(row)
        arrows = QHBoxLayout()
        arrows.setSpacing(2)
        arrows.addWidget(IconLabel("Arrows:", I.swap_arrows_icon(16, ic)))
        self._arrow_chks = []
        for i in range(MAX_ARROWS):
            c = QToolButton()
            c.setCheckable(True)
            c.setAutoRaise(True)
            arrows.addWidget(c)
            self._arrow_chks.append(c)
        arrows.addStretch(1)
        el.addLayout(arrows)
        btns = [tool("New block", I.new_icon, "Put a new block on this level"),
                tool("Delete", I.delete_icon, "Remove the block from this level"),
                tool("Clear", I.get_clear_canvas_icon, "Keep the level, empty block (no faces)")]
        for b, slot in zip(btns, (lambda: self._block_set(self._map.new_block()) if self._map else None,
                                  lambda: self._block_set(None),
                                  lambda: self._block_set(type(self._map.new_block())()) if self._map else None)):
            b.clicked.connect(lambda _c=False, s=slot: s())
        el.addWidget(ButtonRow(btns))
        sp = QSplitter(Qt.Orientation.Vertical)
        sp.addWidget(t)
        sp.addWidget(ed)
        sp.setStretchFactor(0, 1)                        # table takes spare height
        sp.setStretchFactor(1, 0)
        sp.setCollapsible(1, False)
        fit_height(ed)                                   # editor no taller than its rows
        lay.addWidget(sp, 1)
        for w in (self._tile_spin, self._light_spin, self._slope_spin):
            w.valueChanged.connect(lambda _v: self._block_changed())
        for w in (self._rot_combo, self._ground_combo):
            w.currentIndexChanged.connect(lambda _i: self._block_changed())
        for w in (self._flip_chk, self._flat_chk, self._wall_chk, self._bullet_chk, *self._arrow_chks):
            w.toggled.connect(lambda _on: self._block_changed())
        self._block_widgets = [self._tile_spin, self._light_spin, self._slope_spin, self._rot_combo,
                               self._ground_combo, self._flip_chk, self._flat_chk, self._wall_chk,
                               self._bullet_chk, *self._arrow_chks]
        self._block_help()
        return page

    def _block_help(self, text: str = ''): #vers 2
        """Block dock title: 'Block' or 'Block - <cell>'."""
        lbl = getattr(getattr(self, '_block_dock', None), '_title_label', None)
        if lbl is not None:
            lbl.setText(f"Block - {text}" if text else "Block")

    def _block_help_button(self, dock): #vers 4
        """[Help] icon (26 x 26) beside the Block dock title; tooltip holds the help."""
        from PyQt6.QtCore import QSize, Qt
        from PyQt6.QtGui import QCursor
        from PyQt6.QtWidgets import QToolButton, QToolTip
        from apps.methods.imgfactory_svg_icons import SVGIconFactory as I
        self._block_dock = dock
        bar = dock.titleBarWidget()
        if bar is None or bar.layout() is None:
            return
        b = QToolButton()
        b.setIcon(I.info_icon(16, self._get_icon_color()))
        b.setIconSize(QSize(16, 16))
        b.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonIconOnly)
        b.setFixedSize(26, 26)
        b.setAutoRaise(True)
        b.setToolTip(BLOCK_HELP)
        b.clicked.connect(lambda: QToolTip.showText(QCursor.pos(), BLOCK_HELP, b))
        lay = bar.layout()
        lay.insertWidget(1, b)
        if hasattr(self, '_tb_icon_btns'):               # recoloured with the theme
            self._tb_icon_btns.append((b, 'info_icon', 16))

    def _block_map_opened(self): #vers 2
        """New map: level rows, ground names and arrows for its game."""
        m = self._map
        self._cell, self._undo_stack = None, []
        self._level_table.clearContents()
        self._level_table.setRowCount(m.LEVELS)
        self._level_table.setVerticalHeaderLabels([f"z{z}" for z in range(m.LEVELS - 1, -1, -1)])
        self._ground_combo.blockSignals(True)
        self._ground_combo.clear()
        self._ground_combo.addItems([g.title() for g in m.GROUND])
        self._ground_combo.blockSignals(False)
        self._slope_spin.setMaximum(m.SLOPE_MAX)
        from apps.methods.imgfactory_svg_icons import SVGIconFactory as I
        for i, c in enumerate(self._arrow_chks):
            name = m.ARROWS[i] if i < len(m.ARROWS) else ''
            colour = '#e03030' if name.startswith('Red') else '#30b030' if name.startswith('Green') else None
            way = name.split()[-1].lower() if name else 'up'
            c.setIcon(getattr(I, f"arrow_{way}_icon")(18, colour))
            c.setToolTip(f"{name} arrow (traffic route)" if name else '')
            c.setVisible(i < len(m.ARROWS))
        self._block_help()

    def _block_row_values(self, b) -> list: #vers 1
        """Levels table cells for a block."""
        if b is None:
            return [''] * 7
        m = self._map
        return ([str(face_tile(m.face_word(b, f))) for f in FACES] +
                [m.GROUND[m.ground_of(b)], str(m.slope_of(b))])

    def _block_show_cell(self, x: int, y: int, keep_z=None): #vers 3
        """Fill the levels table for a cell; pick keep_z, else its top block."""
        self._cell = (x, y)
        m = self._map
        self._block_help(f"{m.GAME} block {x}, {y}")
        top = -1
        for r in range(m.LEVELS):
            z = m.LEVELS - 1 - r
            b = m.block_at(x, y, z)
            for c, v in enumerate(self._block_row_values(b)):
                self._level_table.setItem(r, c, QTableWidgetItem(v))
            if b is not None and top < 0 and any(b.key()):
                top = z
        self._level_table.resizeColumnsToContents()
        z = keep_z if keep_z is not None else (top if self._view.level < 0 else self._view.level)
        self._level_table.selectRow(m.LEVELS - 1 - max(0, z))
        self._block_level_picked(m.LEVELS - 1 - max(0, z))

    def _block_level_picked(self, row: int): #vers 2
        if row < 0 or self._map is None:
            return
        self._edit_z = self._map.LEVELS - 1 - row
        self._block_fill_editor()

    def _block_fill_editor(self): #vers 2
        """Editor widgets from the current block / face (signals held)."""
        if self._cell is None:
            return
        m = self._map
        b = m.block_at(*self._cell, self._edit_z)
        face = FACES[self._face_combo.currentIndex()]
        caps = m.face_caps(face)
        for w in self._block_widgets:
            w.blockSignals(True)
            w.setEnabled(b is not None)
        if b is not None:
            v = m.face_word(b, face)
            self._tile_spin.setMaximum(m.tile_max(face))
            self._tile_spin.setValue(face_tile(v))
            self._rot_combo.setCurrentIndex((v >> 14) & 3)
            self._flip_chk.setChecked(bool(v & 0x2000))
            self._flat_chk.setChecked(bool(v & 0x1000))
            self._wall_chk.setChecked('wall' in caps and bool(v & 0x400))
            self._bullet_chk.setChecked('bullet' in caps and bool(v & 0x800))
            self._light_spin.setValue((v >> 10) & 3 if 'light' in caps else 0)
            for w, cap in ((self._rot_combo, 'rotation'), (self._flip_chk, 'flip'), (self._flat_chk, 'flat'),
                           (self._wall_chk, 'wall'), (self._bullet_chk, 'bullet'), (self._light_spin, 'light')):
                w.setEnabled(cap in caps)
            self._ground_combo.setCurrentIndex(m.ground_of(b))
            self._slope_spin.setValue(m.slope_of(b))
            bits = m.arrows_of(b)
            for i, c in enumerate(self._arrow_chks):
                c.setChecked(bool(bits & (1 << i)))
            self._tile_preview(face, face_tile(v))
        for w in self._block_widgets:
            w.blockSignals(False)

    def _tile_preview(self, face: str, n: int): #vers 4
        """Face texture as the game shows it: rotated clockwise, then flipped (lids)."""
        import numpy as np
        a = self._style.face_rgba(face, n)
        if face == 'lid':
            a = np.rot90(a, -self._rot_combo.currentIndex())
        if self._flip_chk.isChecked():
            a = a[:, ::-1]
        a = np.ascontiguousarray(a)
        h, w = a.shape[:2]
        img = QImage(a.data, w, h, w * 4, QImage.Format.Format_RGBA8888).copy()
        self._tile_prev.setPixmap(QPixmap.fromImage(img).scaled(72, 72))      # PS1 tiles are 32 px

    def _block_changed(self): #vers 2
        """Editor widget changed: write the face / block."""
        if self._cell is None:
            return
        m = self._map
        b = m.block_at(*self._cell, self._edit_z)
        if b is None:
            return
        face = FACES[self._face_combo.currentIndex()]
        caps = m.face_caps(face)
        v = self._tile_spin.value() | (self._rot_combo.currentIndex() << 14 if 'rotation' in caps else 0)
        v |= (0x2000 if self._flip_chk.isChecked() and 'flip' in caps else 0)
        v |= (0x1000 if self._flat_chk.isChecked() and 'flat' in caps else 0)
        v |= (self._light_spin.value() << 10) if 'light' in caps else 0
        v |= (0x400 if self._wall_chk.isChecked() and 'wall' in caps else 0)
        v |= (0x800 if self._bullet_chk.isChecked() and 'bullet' in caps else 0)
        nb = b.copy()
        m.set_face_word(nb, face, v)
        m.set_ground_slope(nb, self._ground_combo.currentIndex(), self._slope_spin.value())
        m.set_arrows(nb, sum(1 << i for i, c in enumerate(self._arrow_chks) if c.isChecked()))
        if nb.key() != b.key():
            self._block_set(nb, refill=False)
            self._tile_preview(face, self._tile_spin.value())

    def _block_set(self, block, refill: bool = True, cell=None, z=None): #vers 2
        """Put / clear a block (undoable, whole column kept) and redraw."""
        x, y = cell or self._cell or (None, None)
        if x is None:
            return
        z = self._edit_z if z is None else z
        self._undo_stack.append((x, y, dict(self._map.column(x, y))))
        self._map.set_block(x, y, z, block)
        self._map_dirty = True
        self._view.refresh_cell(x, y)
        if (x, y) == self._cell:
            self._block_apply(refill)

    def _block_apply(self, refill: bool): #vers 2
        """Levels table (and editor) after an edit."""
        z, row = self._edit_z, self._level_table.currentRow()
        if refill:
            self._block_show_cell(*self._cell, keep_z=z)
        else:
            for c, v in enumerate(self._block_row_values(self._map.block_at(*self._cell, z))):
                self._level_table.setItem(row, c, QTableWidgetItem(v))
        self._update_title()

    def _block_paint(self, x: int, y: int): #vers 2
        """Paint tool: lid of the top block (or the shown level) gets the picked tile."""
        g = getattr(self, '_paint_tile', None)
        n = self._style.global_face('lid', g) if g is not None else None
        if n is None:
            self._set_status("Paint: pick a lid tile in the Tiles panel first")
            return
        m = self._map
        if self._view.level >= 0:
            z, b = self._view.level, m.block_at(x, y, self._view.level)
        else:
            z, b = m.top_block(x, y)
            if b is None:
                col = m.column(x, y)
                z = max(col) if col else 0
                b = m.block_at(x, y, z)
        nb = b.copy() if b is not None else m.new_block()
        word = m.face_word(nb, 'lid')
        if face_tile(word) == n and b is not None:
            return
        m.set_face_word(nb, 'lid', (word & ~0x3FF) | n)
        self._block_set(nb, refill=True, cell=(x, y), z=z)

    def _block_undo(self): #vers 3
        """Undo the last block edit (the column as it was)."""
        if not self._undo_stack:
            self._set_status("Nothing to undo")
            return
        x, y, col = self._undo_stack.pop()
        self._map.columns[y][x] = col
        self._map.edited = True
        self._view.refresh_cell(x, y)
        if (x, y) == self._cell:
            self._block_show_cell(x, y)
        self._set_status(f"Undo: block {x}, {y}")
