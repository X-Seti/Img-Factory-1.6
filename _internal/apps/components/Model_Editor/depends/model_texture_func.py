#this belongs in apps/components/Model_Editor/depends/model_texture_func.py - Version: 4
# X-Seti - October 12 2026 - IMG Factory 1.6 - Model Workshop textures

"""
Texture panel, TXD loading, texlist folders, texture export for ModelWorkshop.
"""

##class ModelTextureMixin: -
# _add_embedded_textures
# _add_textures_from_txd
# _auto_load_from_texlist
# _auto_load_txd_from_imgs
# _browse_texlist_folder
# _build_txd_from_textures
# _create_texture_panel
# _export_textures_as_png
# _hide_tex_hover
# _load_iff_as_qimage
# _load_texlist_setting
# _load_txd_file
# _load_txd_file_from_data
# _load_txd_into_workshop
# _on_tex_selected
# _open_linked_txd
# _open_txd_combined
# _open_txd_smart
# _parse_txd_lightweight
# _pass_textures_to_dp5_workshop
# _pass_textures_to_txd_workshop
# _populate_tex_thumbnails
# _populate_texture_list
# _remove_selected_textures
# _save_texlist_setting
# _save_textures_as_txd
# _show_tex_hover
# _tex_context_menu
# _texture_back_from_dp5
# _toggle_tex_view

import os
from PyQt6.QtCore import QSize, Qt
from PyQt6.QtGui import QFont, QIcon
from PyQt6.QtWidgets import QAbstractItemView, QApplication, QDialog, QFileDialog, QFrame, QHBoxLayout, QLabel, QLineEdit, QMenu, QMessageBox, QPushButton, QTableWidget, QTableWidgetItem, QVBoxLayout, QWidget
from apps.methods.img_factory_settings import get_user_config_dir

class ModelTextureMixin: #vers 1
    """Texture panel, TXD loading, texlist folders, texture export for ModelWorkshop."""


    def _open_txd_combined(self, _checked=False): #vers 1
        """Open TXD — smart loader for DFF+TXD workflow.
        Shift+click always browses. Otherwise tries:
          1. asset_db lookup (if DB built)
          2. Auto-find from open IMGs using IDE txd_name
          3. Browse for file
        """
        from PyQt6.QtWidgets import QApplication
        shift_held = bool(QApplication.keyboardModifiers() &
                          __import__('PyQt6.QtCore', fromlist=['Qt']).Qt.KeyboardModifier.ShiftModifier)

        if not shift_held:
            # Try smart auto-load first
            txd_name = (getattr(self, '_ide_txd_name', '') or '').strip()
            if not txd_name:
                # Try from current DFF materials
                model = getattr(self, '_current_dff_model', None)
                if model and hasattr(model, 'geometries') and model.geometries:
                    geom = model.geometries[0]
                    mats = getattr(geom, 'materials', [])
                    if mats:
                        txd_name = getattr(mats[0], 'texture_name', '') or ''
                        if txd_name:
                            # strip to stem
                            import os
                            txd_name = os.path.splitext(txd_name)[0]

            if txd_name:
                # Try DB first
                mw = self.main_window
                db = getattr(mw, 'asset_db', None) if mw else None
                ok = False
                if db:
                    try:
                        row = db.find_img_entry(txd_name + '.txd')
                        if row:
                            from apps.methods.img_core_classes import IMGFile
                            import os
                            arc = IMGFile(row['source_path']); arc.open()
                            entry = next((e for e in arc.entries
                                if e.name.lower() == row['entry_name'].lower()), None)
                            if entry:
                                data = arc.read_entry_data(entry)
                                if data:
                                    self._load_txd_file_from_data(data, txd_name + '.txd')
                                    if mw and hasattr(mw, 'log_message'):
                                        mw.log_message(
                                            f"TXD loaded via DB: {txd_name}.txd "
                                            f"from {os.path.basename(row['source_path'])}")
                                    ok = True
                    except Exception:
                        pass
                if not ok:
                    ok = self._auto_load_txd_from_imgs(txd_name)
                if ok:
                    return
                # Auto-find failed — fall through to browse

        # Browse for TXD file
        self._load_txd_into_workshop()

    # - Texture management

    def _create_texture_panel(self): #vers 7
        """Collapsible texture panel in middle column.
        Shows textures loaded from TXD files.
        Toggle between List view (table) and Thumbnail view (64x64 grid).
        Click a thumbnail to see it at up to 128x128 in a popup."""
        icon_color = self._get_icon_color()

        self._tex_panel = QFrame()
        self._tex_panel.setFrameStyle(QFrame.Shape.StyledPanel)
        self._tex_panel.setVisible(False)   # hidden until TXD loaded
        lay = QVBoxLayout(self._tex_panel)
        lay.setContentsMargins(4, 4, 4, 4)
        lay.setSpacing(3)

        # - Header row (count + view toggle only - the dock's own title
        # bar already shows "Textures", a second label here was duplicate)
        self._tex_count_lbl = QLabel("0 textures")       # shown in the dock title bar
        self._tex_count_lbl.setFont(self.panel_font)

        # View toggle: list ⇔ thumbnails
        self._tex_view_mode = 'list'
        self._tex_view_btn = QPushButton("⊞ Thumbnails")
        self._tex_view_btn.setFixedHeight(26)
        self._tex_view_btn.setFont(self.panel_font)
        self._tex_view_btn.setToolTip(
            "Toggle between list view and 64×64 thumbnail grid")
        self._tex_view_btn.clicked.connect(self._toggle_tex_view)

        # - Button row
        btn_row = QHBoxLayout()
        btn_row.setSpacing(3)

        # Collapse/expand toggle - hides Load/Texlist/WShop/Save to
        # reclaim space, independent of the icon-only/icon+text compacting.
        self._tex_btn_row_collapsed = False
        self.tex_btn_collapse_btn = QPushButton("−")
        self.tex_btn_collapse_btn.setFixedSize(18, 26)
        self.tex_btn_collapse_btn.setToolTip("Collapse/expand this toolbar")
        self.tex_btn_collapse_btn.clicked.connect(self._toggle_tex_btn_row_collapsed)
        btn_row.addWidget(self.tex_btn_collapse_btn)

        # tex buttons: adaptive — show label when panel wide enough, icon-only when narrow
        self._tex_btns_meta = []   # [(attr, label, icon_method)] for compact update

        def _tbtn(attr, text, icon_method, tip, slot, enabled=True):  #vers 1
            b = QPushButton(text)
            b.setFont(self.button_font)
            try:
                b.setIcon(getattr(self.icon_factory, icon_method)(color=icon_color))
                b.setIconSize(QSize(18, 18))
            except Exception:
                pass
            b.setFixedHeight(26)
            b.setMinimumWidth(28)   # icon-only minimum
            b.setToolTip(tip)
            b.setEnabled(enabled)
            b.clicked.connect(slot)
            b._tex_label = text
            setattr(self, attr, b)
            btn_row.addWidget(b)
            self._tex_btns_meta.append((attr, text, icon_method))
            return b

        # Adaptive: _update_tex_btn_compact() reduces to icon-only when panel < 260px
        _tbtn('tex_load_btn',    'Load',   'open_icon',
              'Load a TXD file into this workshop',
              self._load_txd_into_workshop)
        _tbtn('tex_browse_btn',  'Texlist',    'folder_icon',  # folder_icon is appropriate
              'Browse texlist/ folder and import individual textures',
              self._browse_texlist_folder)
        _tbtn('tex_pass_btn',    'WShop',  'export_icon',  # export_icon used; txd_workshop_icon if added later
              'Send all textures to TXD Workshop for editing/rebuilding',
              self._pass_textures_to_txd_workshop, enabled=False)
        _tbtn('tex_dp5_btn',     'DP5',    'dp5_workshop_icon',
              'Send the selected textures (or all) to DP5 Workshop to paint',
              self._pass_textures_to_dp5_workshop, enabled=False)
        _tbtn('tex_save_btn',    'Save',   'save_icon',
              'Save current textures as a new TXD file',
              self._save_textures_as_txd, enabled=False)
        btn_row.addStretch()
        lay.addLayout(btn_row)

        # - Stacked: List view (table) + Thumbnail view (scroll area)
        from PyQt6.QtWidgets import QStackedWidget, QScrollArea, QGridLayout
        self._tex_stack = QStackedWidget()
        lay.addWidget(self._tex_stack, stretch=1)

        # Page 0: Table list
        self._tex_list = QTableWidget()
        self._tex_list.setColumnCount(4)
        self._tex_list.setHorizontalHeaderLabels(["", "Name", "Size", "Format"])
        self._tex_list.horizontalHeader().setStretchLastSection(True)
        self._tex_list.setColumnWidth(0, 56)  # thumbnail
        self._tex_list.setColumnWidth(2, 72)  # size
        self._tex_list.setColumnWidth(3, 56)  # format
        self._tex_list.setSelectionBehavior(
            QAbstractItemView.SelectionBehavior.SelectRows)
        self._tex_list.setSelectionMode(
            QAbstractItemView.SelectionMode.ExtendedSelection)
        self._tex_list.setAlternatingRowColors(True)
        self._tex_list.setIconSize(QSize(32, 32))
        self._tex_list.verticalHeader().setDefaultSectionSize(52)
        self._tex_list.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self._tex_list.customContextMenuRequested.connect(self._tex_context_menu)
        self._tex_list.itemSelectionChanged.connect(self._on_tex_selected)
        self._tex_stack.addWidget(self._tex_list)   # index 0

        # Page 1: Thumbnail grid
        self._tex_scroll = QScrollArea()
        self._tex_scroll.setWidgetResizable(True)
        self._tex_scroll.setFrameShape(QFrame.Shape.NoFrame)
        self._tex_grid_widget = QWidget()
        self._tex_grid_layout = QGridLayout(self._tex_grid_widget)
        self._tex_grid_layout.setContentsMargins(4, 4, 4, 4)
        self._tex_grid_layout.setSpacing(6)
        self._tex_scroll.setWidget(self._tex_grid_widget)
        self._tex_stack.addWidget(self._tex_scroll)   # index 1

        # Internal storage
        self._mod_textures = []
        self._texlist_path = ''

        # Hook the panel's own resize event directly - self.resizeEvent()
        # only fires when the whole ModelWorkshop widget's outer bounding
        # box changes, but dragging the Models/viewport dock divider only
        # changes this panel's own width, so relying solely on
        # self.resizeEvent() left the compact check stuck at whatever it
        # computed on first layout and never re-evaluated afterwards.
        _orig_tex_resize = self._tex_panel.resizeEvent
        def _tex_panel_resize(event, _orig=_orig_tex_resize):  #vers 1
            _orig(event)
            self._update_tex_btn_compact()
        self._tex_panel.resizeEvent = _tex_panel_resize

        return self._tex_panel


    def _toggle_tex_view(self): #vers 1
        """Switch texture panel between list view and 64×64 thumbnail grid."""
        if getattr(self, '_tex_view_mode', 'list') == 'list':
            self._tex_view_mode = 'thumb'
            try:
                from apps.methods.imgfactory_svg_icons import SVGIconFactory as _SVGT
                self._tex_view_btn.setIcon(_SVGT._list_icon(color=self._get_icon_color()))
                self._tex_view_btn.setIconSize(QSize(16,16))
            except Exception:
                pass
            self._tex_view_btn.setText("List")
            self._tex_view_btn.setToolTip("Switch to list view")
            stack = getattr(self, '_tex_stack', None)
            if stack:
                stack.setCurrentIndex(1)
            self._populate_tex_thumbnails()
        else:
            self._tex_view_mode = 'list'
            try:
                from apps.methods.imgfactory_svg_icons import SVGIconFactory as _SVGT
                self._tex_view_btn.setIcon(_SVGT.fit_grid_icon(color=self._get_icon_color()))
                self._tex_view_btn.setIconSize(QSize(16,16))
            except Exception:
                pass
            self._tex_view_btn.setText("Thumbnails")
            self._tex_view_btn.setToolTip("Switch to 64×64 thumbnail grid")
            stack = getattr(self, '_tex_stack', None)
            if stack:
                stack.setCurrentIndex(0)

    def _populate_tex_thumbnails(self): #vers 1
        """Fill the thumbnail grid with 64×64 previews of all loaded textures.
        Each thumbnail is a clickable label — click opens a larger popup."""
        from PyQt6.QtWidgets import QLabel, QVBoxLayout, QWidget
        from PyQt6.QtGui import QImage, QPixmap, QColor, QFont
        from PyQt6.QtCore import Qt as _Qt

        grid   = getattr(self, '_tex_grid_layout', None)
        if grid is None:
            return

        # Clear previous thumbnails
        while grid.count():
            item = grid.takeAt(0)
            if item and item.widget():
                item.widget().deleteLater()

        textures = getattr(self, '_mod_textures', [])
        vp       = getattr(self, 'preview_widget', None)
        tc       = getattr(vp, '_tex_cache', {}) if vp else {}

        THUMB_SIZE = 64
        # Fit as many columns as the scroll area width allows (min 2)
        scroll_w = getattr(self, '_tex_scroll', None)
        avail_w  = scroll_w.viewport().width() if scroll_w else 220
        COL_COUNT = max(2, avail_w // (THUMB_SIZE + 10))

        for i, tex in enumerate(textures):
            name     = tex.get('name', f'tex_{i}')
            rgba     = tex.get('rgba_data', b'')
            w        = tex.get('width',  0)
            h        = tex.get('height', 0)
            fmt      = tex.get('format', '?')
            in_cache = name.lower() in tc

            # Build 64×64 pixmap
            pix = QPixmap(THUMB_SIZE, THUMB_SIZE)
            pix.fill(QColor(40, 40, 40))
            if rgba and w > 0 and h > 0 and len(rgba) >= w * h * 4:
                try:
                    qimg = QImage(rgba[:w*h*4], w, h,
                                  w * 4, QImage.Format.Format_RGBA8888)
                    pix = QPixmap.fromImage(qimg).scaled(
                        THUMB_SIZE, THUMB_SIZE,
                        _Qt.AspectRatioMode.KeepAspectRatio,
                        _Qt.TransformationMode.SmoothTransformation)
                    # Pad to square
                    if pix.width() != THUMB_SIZE or pix.height() != THUMB_SIZE:
                        padded = QPixmap(THUMB_SIZE, THUMB_SIZE)
                        padded.fill(self._get_ui_color('viewport_bg'))
                        from PyQt6.QtGui import QPainter
                        p = QPainter(padded)
                        x = (THUMB_SIZE - pix.width())  // 2
                        y = (THUMB_SIZE - pix.height()) // 2
                        p.drawPixmap(x, y, pix)
                        p.end()
                        pix = padded
                except Exception:
                    pass

            # Container widget for thumb + name label
            cell = QWidget()
            cell.setFixedSize(THUMB_SIZE + 4, THUMB_SIZE + 18)
            cell_lay = QVBoxLayout(cell)
            cell_lay.setContentsMargins(2, 2, 2, 2)
            cell_lay.setSpacing(1)

            # Clickable image label
            img_lbl = QLabel()
            img_lbl.setFixedSize(THUMB_SIZE, THUMB_SIZE)
            img_lbl.setPixmap(pix)
            img_lbl.setAlignment(_Qt.AlignmentFlag.AlignCenter)
            img_lbl.setCursor(
                __import__('PyQt6.QtCore', fromlist=['Qt']).Qt.CursorShape.PointingHandCursor)
            # Cache indicator border
            border_col = 'palette(link)' if in_cache else 'palette(mid)'
            img_lbl.setStyleSheet(
                f"border: 2px solid {border_col}; background: palette(base);")
            img_lbl.setToolTip(
                f"{name}\n{w}×{h}  {fmt}\n"
                f"{'✓ in texture cache' if in_cache else '✗ not in cache'}\n"
                "Click to view full size")

            # Capture tex for lambda
            _tex = tex
            img_lbl._tex_raw  = _tex
            img_lbl._hover_wnd = [None]
            def _enter_g(ev, lbl=img_lbl, ws=self):  #vers 1
                ws._show_tex_hover(lbl, getattr(lbl,'_tex_raw',None))
            def _leave_g(ev, lbl=img_lbl, ws=self):  #vers 1
                ws._hide_tex_hover(lbl)
            img_lbl.enterEvent = _enter_g
            img_lbl.leaveEvent = _leave_g
            cell_lay.addWidget(img_lbl)

            # Name label (truncated)
            name_lbl = QLabel(name[:12] + '…' if len(name) > 12 else name)
            name_lbl.setFont(QFont("Arial", 7))
            name_lbl.setAlignment(_Qt.AlignmentFlag.AlignCenter)
            name_lbl.setStyleSheet("color: palette(windowText);")
            cell_lay.addWidget(name_lbl)

            grid.addWidget(cell, i // COL_COUNT, i % COL_COUNT)

        # Fill remaining cells with stretch
        grid.setColumnStretch(COL_COUNT, 1)


    def _show_tex_hover(self, anchor_widget, tex: dict): #vers 1
        """Show a frameless texture preview that follows the mouse into the popup.
        Disappears when mouse leaves both the thumbnail and the popup itself."""
        if not tex:
            return
        from PyQt6.QtWidgets import QLabel, QVBoxLayout, QWidget
        from PyQt6.QtGui import QImage, QPixmap, QColor, QFont, QCursor
        from PyQt6.QtCore import Qt as _Qt

        # Dismiss any existing hover window
        self._hide_tex_hover(anchor_widget)

        name = tex.get('name', 'texture')
        rgba = tex.get('rgba_data', b'')
        w    = tex.get('width',  0)
        h    = tex.get('height', 0)
        fmt  = tex.get('format', '?')
        MAX_D = 192

        popup = QWidget(None,
            _Qt.WindowType.Tool |
            _Qt.WindowType.FramelessWindowHint |
            _Qt.WindowType.WindowStaysOnTopHint)
        popup.setAttribute(_Qt.WidgetAttribute.WA_DeleteOnClose)
        popup.setStyleSheet(
            "QWidget { background:palette(base); border:1px solid palette(mid); border-radius:3px; }"
            "QLabel  { color:#ccc; background:transparent; }")

        v = QVBoxLayout(popup)
        v.setContentsMargins(6, 6, 6, 6)
        v.setSpacing(4)

        img_lbl = QLabel()
        img_lbl.setAlignment(_Qt.AlignmentFlag.AlignCenter)
        pix = QPixmap(MAX_D, MAX_D)
        pix.fill(QColor(40, 40, 55))
        if rgba and w > 0 and h > 0 and len(rgba) >= w*h*4:
            try:
                qi = QImage(rgba[:w*h*4], w, h,
                            w*4, QImage.Format.Format_RGBA8888)
                if w > MAX_D or h > MAX_D:
                    qi = qi.scaled(MAX_D, MAX_D,
                        _Qt.AspectRatioMode.KeepAspectRatio,
                        _Qt.TransformationMode.SmoothTransformation)
                pix = QPixmap.fromImage(qi)
            except Exception:
                pass
        img_lbl.setPixmap(pix)
        img_lbl.setFixedSize(pix.width(), pix.height())
        v.addWidget(img_lbl)

        info = QLabel(f"{name}   {w}×{h}   {fmt}")
        info.setFont(QFont("Arial", 8))
        v.addWidget(info)

        popup.adjustSize()
        pos = QCursor.pos()
        popup.move(pos.x() + 12, pos.y() + 8)

        # Hide when mouse leaves the popup
        popup.leaveEvent = lambda ev: popup.close()
        # Also close on click
        popup.mousePressEvent = lambda ev: popup.close()

        popup.show()
        anchor_widget._hover_wnd[0] = popup

    def _hide_tex_hover(self, anchor_widget): #vers 1
        """Close the hover popup if open."""
        holder = getattr(anchor_widget, '_hover_wnd', [None])
        w = holder[0] if holder else None
        if w is not None:
            try:
                w.close()
            except Exception:
                pass
            holder[0] = None


    def _auto_load_txd_from_imgs(self, txd_stem: str = '') -> bool: #vers 2
        """Search all open IMG tabs and current_img for txd_stem.txd.
        Extracts and loads it into the texture cache. Returns True if found.
        Falls back to the current DFF's own filename (same-folder-same-name
        convention) if no IDE-linked name is available either."""
        mw = self.main_window
        if not txd_stem:
            # Try to get txd_stem from IDE link
            txd_stem = (getattr(self, '_ide_txd_name', '') or '').strip().lower()
            if not txd_stem:
                # No IDE link either - fall back to the DFF's own filename
                # stem (same-folder-same-name convention).
                dff_path = getattr(self, '_current_dff_path', '') or ''
                if dff_path:
                    txd_stem = os.path.splitext(os.path.basename(dff_path))[0].lower()
                if not txd_stem:
                    return False

        txd_stem_lo = txd_stem.lower().split('.')[0]  # strip extension if present
        txd_filename = txd_stem_lo + '.txd'

        # Same-folder-as-DFF direct disk check first (cheap, common case)
        dff_path = getattr(self, '_current_dff_path', '') or ''
        if dff_path:
            dff_dir = os.path.dirname(dff_path)
            try:
                for fname in os.listdir(dff_dir):
                    if fname.lower() == txd_filename:
                        self._load_txd_file(os.path.join(dff_dir, fname))
                        if mw and hasattr(mw, 'log_message'):
                            mw.log_message(f"Auto-loaded TXD: {fname} (same folder as DFF)")
                        return True
            except OSError:
                pass

        # Gather all candidate IMG sources
        img_candidates   = []   # IMGFile objects
        img_path_search  = []   # raw paths to open directly if needed

        # 1. current_img on workshop or main_window
        ci = getattr(self, 'current_img', None) or (
             getattr(mw, 'current_img', None) if mw else None)
        # Diagnostic: log what we actually got, to help pin down the
        # docked-mode "TXD not found" regression, this tier used to work,
        # but nothing in this function was touched recently, so the cause
        # isn't confirmed yet.
        if mw and hasattr(mw, 'log_message'):
            mw.log_message(
                f"[TXD lookup] looking for '{txd_filename}' - "
                f"self.current_img={getattr(self, 'current_img', None)!r}, "
                f"mw.current_img={getattr(mw, 'current_img', None)!r}, "
                f"entries={len(getattr(ci, 'entries', []) or [])}")
        if ci:
            img_candidates.append(ci)

        # 2. Source IMG path stored when DFF was extracted
        src_img = getattr(self, '_source_img_path', '') or ''
        if src_img and os.path.isfile(src_img):
            img_path_search.append(src_img)

        # 3. All open IMG tabs
        if mw and hasattr(mw, 'main_tab_widget'):
            tw = mw.main_tab_widget
            for i in range(tw.count()):
                w = tw.widget(i)
                if w and getattr(w, 'file_type', '') == 'IMG':
                    fo = getattr(w, 'file_object', None)
                    if fo and fo is not ci:
                        img_candidates.append(fo)
                # Also check file_path of any tab
                fp = getattr(w, 'file_path', '') if w else ''
                if fp and fp.lower().endswith('.img') and fp not in img_path_search:
                    img_path_search.append(fp)

        # 4. Search by opening the source IMG directly (for extracted DFFs)
        seen_paths = {getattr(img, 'file_path', '') for img in img_candidates}
        for ipath in img_path_search:
            if ipath not in seen_paths:
                try:
                    from apps.methods.img_core_classes import IMGFile
                    arc = IMGFile(ipath)
                    arc.open()
                    img_candidates.append(arc)
                    seen_paths.add(ipath)
                except Exception:
                    pass

        for img in img_candidates:
            entries = getattr(img, 'entries', [])
            entry = next(
                (e for e in entries if e.name.lower() == txd_filename),
                None)
            if entry:
                try:
                    data = img.read_entry_data(entry)
                    if data:
                        self._load_txd_file_from_data(data, txd_filename)
                        if mw and hasattr(mw, 'log_message'):
                            mw.log_message(
                                f"Auto-loaded TXD: {txd_filename} "
                                f"from {os.path.basename(img.file_path)}")
                        return True
                except Exception as e:
                    if mw and hasattr(mw, 'log_message'):
                        mw.log_message(f"TXD extract error: {e}")

        if mw and hasattr(mw, 'log_message'):
            mw.log_message(
                f"TXD not in any open IMG: {txd_filename} — trying texlist/")
        # Fallback: scan texlist/ folder for pre-exported image files
        return self._auto_load_from_texlist(txd_stem_lo)


    def _auto_load_from_texlist(self, txd_stem: str = '') -> bool: #vers 1
        """Scan the configured texlist/ folder recursively for image files
        matching the textures needed by the current DFF model.
        Supports PNG, IFF/ILBM, TGA, BMP.  Returns True if at least one
        texture was loaded into the viewport cache."""
        txd_stem_lo = (txd_stem or '').lower().split('.')[0]
        texlist_root = getattr(self, '_texlist_folder', '') or ''

        # Auto-discover texlist/ relative to the current DFF or TXD file
        if not texlist_root or not os.path.isdir(texlist_root):
            for attr in ('_current_dff_path', '_current_txd_path'):
                base = getattr(self, attr, '') or ''
                if not base:
                    continue
                base_dir = os.path.dirname(base)
                for rel in ('texlist',
                            os.path.join('..', 'texlist'),
                            os.path.join('..', '..', 'texlist')):
                    cand = os.path.normpath(os.path.join(base_dir, rel))
                    if os.path.isdir(cand):
                        texlist_root = cand
                        break
                if texlist_root:
                    break

        if not texlist_root or not os.path.isdir(texlist_root):
            mw = self.main_window
            if mw and hasattr(mw, 'log_message'):
                mw.log_message(
                    "texlist: no folder set — use 'Set texlist…' to configure")
            return False

        mw  = self.main_window
        pw  = getattr(self, 'preview_widget', None)
        extensions = ('.png', '.iff', '.tga', '.bmp', '.jpg', '.jpeg')

        # Collect texture names we are looking for from the current model
        needed = set()
        for tex in getattr(self, '_mod_textures', []):
            n = (tex.get('name') or '').lower().strip('\x00').strip()
            if n:
                needed.add(n)

        # When _mod_textures is empty accept anything under the txd_stem subfolder
        use_stem_only = not needed
        if use_stem_only and not txd_stem_lo:
            return False

        found_count = 0

        for dirpath, _dirs, filenames in os.walk(texlist_root):
            # For named-structure dumps, prefer the txd_stem subfolder
            rel_dir = os.path.relpath(dirpath, texlist_root).lower()
            in_stem_folder = (
                rel_dir == '.' or
                rel_dir == txd_stem_lo or
                txd_stem_lo in rel_dir.split(os.sep))

            for fname in filenames:
                ext = os.path.splitext(fname)[1].lower()
                if ext not in extensions:
                    continue
                tex_name = os.path.splitext(fname)[0].lower()

                if use_stem_only:
                    if not in_stem_folder:
                        continue
                else:
                    if tex_name not in needed:
                        continue

                fpath = os.path.join(dirpath, fname)
                try:
                    if ext == '.iff':
                        qimg = self._load_iff_as_qimage(fpath)
                    else:
                        from PyQt6.QtGui import QImage
                        qimg = QImage(fpath)

                    if qimg is None or qimg.isNull():
                        continue

                    from PyQt6.QtGui import QImage as _QI
                    if qimg.format() != _QI.Format.Format_RGBA8888:
                        qimg = qimg.convertToFormat(_QI.Format.Format_RGBA8888)

                    if pw and hasattr(pw, '_tex_cache'):
                        pw._tex_cache[tex_name] = qimg.copy()
                    found_count += 1

                    if mw and hasattr(mw, 'log_message'):
                        mw.log_message(f"texlist: {fname} -> {tex_name}")

                except Exception as e:
                    if mw and hasattr(mw, 'log_message'):
                        mw.log_message(f"texlist load error {fname}: {e}")

        if found_count:
            if pw:
                pw.set_render_mode('textured')
                pw.update()
            if mw and hasattr(mw, 'log_message'):
                mw.log_message(
                    f"texlist: {found_count} texture(s) loaded "
                    f"from {texlist_root}")
        else:
            if mw and hasattr(mw, 'log_message'):
                mw.log_message(
                    f"texlist: no matching textures in {texlist_root}")

        return found_count > 0

    def _load_iff_as_qimage(self, path: str): #vers 1
        """Load an Amiga IFF ILBM file and return a QImage (RGBA8888), or None.
        Supports 24-bit true colour and 8-bit CMAP-indexed ILBM files."""
        import struct
        from PyQt6.QtGui import QImage
        try:
            data = open(path, 'rb').read()
            if len(data) < 12 or data[:4] != b'FORM' or data[8:12] != b'ILBM':
                return None
            i = 12
            bmhd = body = cmap = None
            while i < len(data) - 8:
                tag  = data[i:i+4]
                size = struct.unpack_from('>I', data, i+4)[0]
                chunk = data[i+8:i+8+size]
                if   tag == b'BMHD': bmhd = struct.unpack_from('>HHhhBBBBHBBhh', chunk)
                elif tag == b'CMAP': cmap = chunk
                elif tag == b'BODY': body = chunk
                i += 8 + size + (size % 2)
                if i >= len(data):
                    break

            if not bmhd or body is None:
                return None

            w, h, n_planes = bmhd[0], bmhd[1], bmhd[4]
            row_bytes = (w + 15) // 16 * 2

            # 24-bit true colour (R8 G8 B8 interleaved bitplanes)
            if n_planes == 24:
                rgba = bytearray(w * h * 4)
                bp = 0
                for y in range(h):
                    planes = []
                    for _ in range(24):
                        planes.append(body[bp:bp+row_bytes])
                        bp += row_bytes
                    for x in range(w):
                        bx  = x // 8
                        bit = 0x80 >> (x % 8)
                        r = sum(1<<p for p in range(8) if bx<len(planes[p])   and planes[p][bx]   & bit)
                        g = sum(1<<p for p in range(8) if bx<len(planes[8+p]) and planes[8+p][bx] & bit)
                        b = sum(1<<p for p in range(8) if bx<len(planes[16+p])and planes[16+p][bx]& bit)
                        off = (y*w+x)*4
                        rgba[off]=r; rgba[off+1]=g; rgba[off+2]=b; rgba[off+3]=255
                return QImage(bytes(rgba), w, h, w*4,
                              QImage.Format.Format_RGBA8888).copy()

            # 8-bit indexed with CMAP palette
            if n_planes == 8 and cmap:
                pal = [(cmap[j*3], cmap[j*3+1], cmap[j*3+2])
                       for j in range(len(cmap)//3)]
                rgba = bytearray(w * h * 4)
                bp = 0
                for y in range(h):
                    planes = [body[bp+p*row_bytes:bp+p*row_bytes+row_bytes]
                              for p in range(8)]
                    bp += 8 * row_bytes
                    for x in range(w):
                        bx  = x // 8
                        bit = 0x80 >> (x % 8)
                        pv  = sum(1<<p for p in range(8)
                                  if bx<len(planes[p]) and planes[p][bx] & bit)
                        r, g, b = pal[pv] if pv < len(pal) else (0, 0, 0)
                        off = (y*w+x)*4
                        rgba[off]=r; rgba[off+1]=g; rgba[off+2]=b; rgba[off+3]=255
                return QImage(bytes(rgba), w, h, w*4,
                              QImage.Format.Format_RGBA8888).copy()

            return None
        except Exception:
            return None


    def _save_texlist_setting(self): #vers 2
        """Persist the texlist folder path to ~/.config/imgfactory/model_workshop.json"""
        import json
        cfg_dir = str(get_user_config_dir())
        os.makedirs(cfg_dir, exist_ok=True)
        try:
            p = os.path.join(cfg_dir, 'model_workshop.json')
            data = {}
            if os.path.isfile(p):
                data = json.load(open(p))
            data['texlist_folder'] = getattr(self, '_texlist_folder', '')
            json.dump(data, open(p, 'w'), indent=2)
        except Exception:
            pass

    def _load_texlist_setting(self): #vers 2
        """Load the texlist folder path from ~/.config/imgfactory/model_workshop.json"""
        import json
        p = os.path.join(get_user_config_dir(), 'model_workshop.json')
        if os.path.isfile(p):
            try:
                data = json.load(open(p))
                self._texlist_folder = data.get('texlist_folder', '')
                return
            except Exception:
                pass
        self._texlist_folder = ''


    def _load_txd_into_workshop(self): #vers 2
        """Open a TXD file and load its textures into Model Workshop."""
        path, _ = QFileDialog.getOpenFileName(
            self, "Load TXD",
            os.path.dirname(getattr(self, '_current_dff_path', '')),
            "Texture Files (*.txd *.wtd *.nft *.xtx *.chk);;TXD Files (*.txd);;All Files (*)")
        if not path:
            return
        self._load_txd_file(path)

    def _parse_txd_lightweight(self, data: bytes) -> list: #vers 5
        """Parse TXD data via txd_parser (all platforms, TXD Workshop reader)."""
        try:
            from apps.methods.txd_parser import parse_txd
            return parse_txd(data)
        except Exception as e:
            print(f"_parse_txd_lightweight error: {e}")
            return []

    def _add_embedded_textures(self, model): #vers 1
        """Model's own textures (GTA IV .wdr / .wft) into the texture pane and viewport;
        names already loaded from its TXD / WTD are kept."""
        emb = list((getattr(model, 'embedded_textures', None) or {}).values())
        if not emb:
            return
        from apps.methods.txd_splice import tag_loaded_texture
        have = {t.get('name', '').lower() for t in (getattr(self, '_mod_textures', None) or [])}
        new = [t for t in emb if t.get('name', '').lower() not in have]
        for t in new:
            t.setdefault('alpha_name', t.get('mask', '') or '')
            tag_loaded_texture(t)
        self._mod_textures = list(getattr(self, '_mod_textures', None) or []) + new
        self._populate_texture_list()
        self._tex_panel.setVisible(True)
        from PyQt6.QtCore import QTimer
        QTimer.singleShot(0, self._update_tex_btn_compact)
        pw = getattr(self, 'preview_widget', None)
        if pw is not None and hasattr(pw, '_upload_textures'):
            pw._upload_textures(new, additive=True)
            pw.set_render_mode('textured')
            pw.update()
        self._set_status(f"{len(new)} embedded texture(s) from the model")

    def _load_txd_file(self, path: str): #vers 4
        """Parse a TXD file, populate texture panel, and feed textures into viewport."""
        try:
            with open(path, 'rb') as f:
                data = f.read()
            textures = self._parse_txd_lightweight(data)
            if not textures:
                QMessageBox.warning(self, "TXD",
                    f"No textures found in {os.path.basename(path)}")
                return
            from apps.methods.txd_splice import tag_loaded_texture
            for t in textures:
                t.setdefault('alpha_name', t.get('mask', ''))
                tag_loaded_texture(t)
            self._mod_textures = textures
            self._current_txd_path = path
            self._current_txd_bytes = data
            self._populate_texture_list()
            self._tex_panel.setVisible(True)
            from PyQt6.QtCore import QTimer as _QTTex
            _QTTex.singleShot(0, self._update_tex_btn_compact)   # visibility alone doesn't fire resizeEvent; defer so layout has settled
            txd_stem = os.path.splitext(os.path.basename(path))[0]
            if hasattr(self, 'info_txd_name'):
                self.info_txd_name.setText(txd_stem)
            self._set_status(
                f"Loaded TXD: {os.path.basename(path)} "
                f"— {len(textures)} textures")
            # Push textures into the 3D viewport cache for textured rendering
            pw = getattr(self, 'preview_widget', None)
            if pw and hasattr(pw, '_upload_textures'):
                pw._upload_textures(textures)
                pw.set_render_mode('textured')
                pw.update()
            elif pw and hasattr(pw, 'load_textures'):
                pw.load_textures(textures)
                if len(textures) > 0:
                    pw.set_render_mode('textured')
        except Exception as e:
            import traceback; traceback.print_exc()
            QMessageBox.critical(self, "TXD Error",
                f"Failed to load TXD:\n{e}")

    def _add_textures_from_txd(self, path: str): #vers 2
        """Add textures from a TXD file to existing texture list (no replace)."""
        try:
            with open(path, 'rb') as f:
                data = f.read()
            new_texs = self._parse_txd_lightweight(data)
            if not new_texs:
                return
            # Merge — skip duplicates by name
            existing = {t['name'].lower() for t in self._mod_textures}
            added = 0
            for tex in new_texs:
                if tex['name'].lower() not in existing:
                    self._mod_textures.append(tex)
                    existing.add(tex['name'].lower())
                    added += 1
            self._populate_texture_list()
            self._set_status(f"Added {added} texture(s) from {os.path.basename(path)}")
        except Exception as e:
            print(f"Add textures error: {e}")

    def _populate_texture_list(self): #vers 3
        """Fill the texture QTableWidget from self._mod_textures."""
        from PyQt6.QtGui import QImage, QPixmap, QIcon
        tbl = self._tex_list
        tbl.setRowCount(0)
        tbl.setIconSize(QSize(48, 48))
        tbl.verticalHeader().setDefaultSectionSize(52)

        for i, tex in enumerate(self._mod_textures):
            row = tbl.rowCount()
            tbl.insertRow(row)

            w = tex.get('width',  0)
            h = tex.get('height', 0)
            name = tex.get('name', '?')
            fmt  = tex.get('format', '?')
            mips = tex.get('mip_count', 1)
            rgba = tex.get('rgba_data', b'')

            # Thumbnail
            thumb_item = QTableWidgetItem()
            thumb_item.setTextAlignment(
                Qt.AlignmentFlag.AlignHCenter | Qt.AlignmentFlag.AlignVCenter)
            if rgba and w > 0 and h > 0:
                try:
                    qimg = QImage(rgba, w, h, w * 4, QImage.Format.Format_RGBA8888)
                    pix  = QPixmap.fromImage(qimg).scaled(
                        48, 48,
                        Qt.AspectRatioMode.KeepAspectRatio,
                        Qt.TransformationMode.SmoothTransformation)
                    thumb_item.setIcon(QIcon(pix))
                except Exception:
                    thumb_item.setText("?")
            else:
                thumb_item.setText("—")

            # Name + number as tooltip
            name_item = QTableWidgetItem(name)
            name_item.setToolTip(
                f"#{i+1}  {name}\n{w}×{h}  {fmt}  {mips} mip(s)")

            size_item = QTableWidgetItem(f"{w}×{h}")
            fmt_item  = QTableWidgetItem(fmt)

            tbl.setItem(row, 0, thumb_item)
            tbl.setItem(row, 1, name_item)
            tbl.setItem(row, 2, size_item)
            tbl.setItem(row, 3, fmt_item)

        count = len(self._mod_textures)
        if hasattr(self, '_tex_count_lbl'):
            self._tex_count_lbl.setText(f"{count} texture{'s' if count != 1 else ''}")
        has = count > 0
        for btn in ('tex_pass_btn', 'tex_dp5_btn', 'tex_save_btn'):
            b = getattr(self, btn, None)
            if b: b.setEnabled(has)
        # Refresh thumbnail grid if it's currently visible
        if getattr(self, '_tex_view_mode', 'list') == 'thumb':
            self._populate_tex_thumbnails()

    def _browse_texlist_folder(self): #vers 2
        """Browse a Texlist folder — shows all TXDs, lets user add individual textures."""
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QHBoxLayout, QTreeWidget, QTreeWidgetItem
        folder = QFileDialog.getExistingDirectory(
            self, "Select Texlist Folder",
            self._texlist_path or os.path.expanduser('~'))
        if not folder:
            return
        self._texlist_path = folder

        # Scan folder for TXD files
        txd_files = []
        for root, dirs, files in os.walk(folder):
            dirs.sort()
            for fn in sorted(files):
                if fn.lower().endswith('.txd'):
                    txd_files.append(os.path.join(root, fn))

        if not txd_files:
            QMessageBox.information(self, "Texlist", "No TXD files found in that folder.")
            return

        # Show browser dialog
        dlg = QDialog(self)
        dlg.setWindowTitle(f"Texlist Browser — {os.path.basename(folder)}")
        dlg.resize(600, 500)
        dlg_lay = QVBoxLayout(dlg)

        info = QLabel(f"{len(txd_files)} TXD files found in {os.path.basename(folder)}")
        info.setFont(self.panel_font)
        dlg_lay.addWidget(info)

        # Search bar
        search_row = QHBoxLayout()
        search_edit = QLineEdit()
        search_edit.setPlaceholderText("Filter by name…")
        search_row.addWidget(search_edit)
        dlg_lay.addLayout(search_row)

        # Tree: TXD file → texture names
        tree = QTreeWidget()
        tree.setHeaderLabels(["TXD / Texture", "Size", "Format"])
        tree.setColumnWidth(0, 280)
        tree.setAlternatingRowColors(True)
        tree.setSelectionMode(QAbstractItemView.SelectionMode.ExtendedSelection)
        dlg_lay.addWidget(tree, stretch=1)

        # Populate tree
        def _populate(filter_text=''):  #vers 1
            tree.clear()
            ft = filter_text.lower()
            for txd_path in txd_files:
                txd_name = os.path.basename(txd_path)
                if ft and ft not in txd_name.lower():
                    continue
                file_item = QTreeWidgetItem([txd_name, '', ''])
                file_item.setData(0, Qt.ItemDataRole.UserRole, ('file', txd_path))
                file_item.setFont(0, QFont("Arial", 9, QFont.Weight.Bold))
                # Lazy-load: parse on expand
                placeholder = QTreeWidgetItem(['Loading…'])
                file_item.addChild(placeholder)
                tree.addTopLevelItem(file_item)

        _populate()
        search_edit.textChanged.connect(_populate)

        def _on_expand(item):  #vers 1
            if item.childCount() == 1 and item.child(0).text(0) == 'Loading…':
                item.removeChild(item.child(0))
                txd_path = (item.data(0, Qt.ItemDataRole.UserRole) or (None, None))[1]
                if not txd_path:
                    return
                try:
                    with open(txd_path, 'rb') as f:
                        data = f.read()
                    texs = self._parse_txd_lightweight(data)
                    for tex in (texs or []):
                        child = QTreeWidgetItem([
                            tex.get('name', '?'),
                            f"{tex.get('width',0)}×{tex.get('height',0)}",
                            tex.get('format', '?'),
                        ])
                        child.setData(0, Qt.ItemDataRole.UserRole, ('tex', txd_path, tex['name']))
                        item.addChild(child)
                except Exception as e:
                    item.addChild(QTreeWidgetItem([f"Error: {e}"]))

        tree.itemExpanded.connect(_on_expand)

        # Buttons
        btn_row = QHBoxLayout()
        add_file_btn = QPushButton("Add whole TXD")
        add_file_btn.setToolTip("Add all textures from selected TXD file(s)")
        add_tex_btn  = QPushButton("Add selected textures")
        add_tex_btn.setToolTip("Add only the selected texture(s)")
        cancel_btn   = QPushButton("Close")
        btn_row.addWidget(add_file_btn)
        btn_row.addWidget(add_tex_btn)
        btn_row.addStretch()
        btn_row.addWidget(cancel_btn)
        dlg_lay.addLayout(btn_row)
        cancel_btn.clicked.connect(dlg.close)

        def _add_whole_txd():  #vers 1
            added_any = False
            for item in tree.selectedItems():
                role = item.data(0, Qt.ItemDataRole.UserRole)
                if role and role[0] == 'file':
                    self._add_textures_from_txd(role[1])
                    added_any = True
            if added_any:
                dlg.close()

        def _add_selected_textures():  #vers 1
            # Group by TXD file, parse once, add matching names
            by_file = {}
            for item in tree.selectedItems():
                role = item.data(0, Qt.ItemDataRole.UserRole)
                if role and role[0] == 'tex':
                    _, txd_path, tex_name = role
                    by_file.setdefault(txd_path, []).append(tex_name.lower())
            if not by_file:
                return
            for txd_path, names in by_file.items():
                try:
                    with open(txd_path, 'rb') as f:
                        data = f.read()
                    texs = self._parse_txd_lightweight(data) or []
                    existing = {t['name'].lower() for t in self._mod_textures}
                    for tex in texs:
                        if tex['name'].lower() in names and tex['name'].lower() not in existing:
                            self._mod_textures.append(tex)
                            existing.add(tex['name'].lower())
                except Exception as e:
                    print(f"Texlist add error: {e}")
            self._populate_texture_list()
            self._tex_panel.setVisible(True)
            from PyQt6.QtCore import QTimer as _QTTex
            _QTTex.singleShot(0, self._update_tex_btn_compact)   # visibility alone doesn't fire resizeEvent; defer so layout has settled
            dlg.close()

        add_file_btn.clicked.connect(_add_whole_txd)
        add_tex_btn.clicked.connect(_add_selected_textures)
        dlg.exec()

    def _on_tex_selected(self): #vers 1
        """Texture row selected — show preview in status."""
        rows = self._tex_list.selectionModel().selectedRows()
        if not rows:
            return
        idx = rows[0].row()
        if 0 <= idx < len(self._mod_textures):
            tex = self._mod_textures[idx]
            self._set_status(
                f"Texture: {tex.get('name','?')}  "
                f"{tex.get('width',0)}×{tex.get('height',0)}  "
                f"{tex.get('format','?')}  "
                f"{tex.get('mipmaps',1)} mip(s)")

    def _tex_context_menu(self, pos): #vers 2
        """Right-click context menu on texture list."""
        from PyQt6.QtWidgets import QMenu
        rows = self._tex_list.selectionModel().selectedRows()
        menu = QMenu(self)
        if rows:
            menu.addAction("Remove selected texture(s)",
                           self._remove_selected_textures)
            menu.addAction("Export texture(s) as PNG…",
                           self._export_textures_as_png)
            menu.addSeparator()
        menu.addAction("Load TXD…",        self._load_txd_into_workshop)
        menu.addAction("Browse Texlist…",  self._browse_texlist_folder)
        if self._mod_textures:
            menu.addSeparator()
            menu.addAction("Pass to TXD Workshop", self._pass_textures_to_txd_workshop)
            menu.addAction("Pass to DP5 Workshop", self._pass_textures_to_dp5_workshop)
            menu.addAction("Save as TXD…",         self._save_textures_as_txd)
        menu.exec(self._tex_list.viewport().mapToGlobal(pos))

    def _remove_selected_textures(self): #vers 1
        rows = sorted(
            {r.row() for r in self._tex_list.selectionModel().selectedRows()},
            reverse=True)
        for r in rows:
            if 0 <= r < len(self._mod_textures):
                del self._mod_textures[r]
        self._populate_texture_list()

    def _export_textures_as_png(self): #vers 1
        """Export selected textures to PNG files."""
        rows = {r.row() for r in self._tex_list.selectionModel().selectedRows()}
        if not rows:
            return
        folder = QFileDialog.getExistingDirectory(self, "Export Textures to Folder")
        if not folder:
            return
        from PyQt6.QtGui import QImage
        exported = 0
        for idx in rows:
            if not (0 <= idx < len(self._mod_textures)):
                continue
            tex = self._mod_textures[idx]
            try:
                w, h = tex.get('width', 0), tex.get('height', 0)
                data = tex.get('pixel_data') or tex.get('compressed_data', b'')
                if data and w and h and 'DXT' not in tex.get('format', ''):
                    qimg = QImage(data[:w*h*4], w, h, QImage.Format.Format_RGBA8888)
                    out = os.path.join(folder, tex.get('name', f'tex_{idx}') + '.png')
                    qimg.save(out)
                    exported += 1
            except Exception as e:
                print(f"Export tex error: {e}")
        self._set_status(f"Exported {exported} texture(s) to {folder}")

    def _pass_textures_to_dp5_workshop(self): #vers 2
        """Selected textures (or all) into DP5 Workshop; DP5's Send back returns them here."""
        from apps.methods.dp5_link import send_textures_to_dp5
        if not self._mod_textures:
            return
        rows = sorted({i.row() for i in self._tex_list.selectionModel().selectedRows()})
        texs = [self._mod_textures[r] for r in rows if r < len(self._mod_textures)] or list(self._mod_textures)
        sent = send_textures_to_dp5(self, texs, self._titlebar_title(), self._texture_back_from_dp5)
        self._set_status(f"Passed {sent} texture(s) to DP5 Workshop")

    def _texture_back_from_dp5(self, tex, rgba, w, h): #vers 1
        """DP5 Send back: painted pixels into the texture, list and viewport."""
        from apps.methods.dp5_link import texture_from_dp5
        if tex not in self._mod_textures or texture_from_dp5(tex, rgba, w, h) is None:
            self._set_status("DP5: texture no longer loaded here")
            return
        self._populate_texture_list()
        self.preview_widget._upload_textures(self._mod_textures)  # re-upload all (replaces the old one)
        self.preview_widget.update()
        self._set_status(f"{tex.get('name')} updated from DP5 - Save TXD to keep")

    def _pass_textures_to_txd_workshop(self): #vers 1
        """Send current textures to TXD Workshop for editing."""
        if not self._mod_textures:
            return
        mw = getattr(self, 'main_window', None)

        # Build a minimal TXD in memory from self._mod_textures
        txd_data = self._build_txd_from_textures()

        if txd_data and mw and hasattr(mw, 'open_txd_workshop_docked'):
            # Write to a temp file and open
            import tempfile
            stem = os.path.splitext(
                os.path.basename(getattr(self, '_current_dff_path', 'model')))[0]
            tmp_dir = tempfile.mkdtemp()
            tmp_path = os.path.join(tmp_dir, f'{stem}.txd')
            with open(tmp_path, 'wb') as _f: _f.write(txd_data)
            mw.open_txd_workshop_docked(file_path=tmp_path)
            self._set_status(
                f"Passed {len(self._mod_textures)} texture(s) to TXD Workshop")
        elif txd_data:
            # Standalone mode — open TXD Workshop window
            import tempfile
            stem = os.path.splitext(
                os.path.basename(getattr(self, '_current_dff_path', 'model')))[0]
            tmp_dir = tempfile.mkdtemp()
            tmp_path = os.path.join(tmp_dir, f'{stem}.txd')
            with open(tmp_path, 'wb') as _f: _f.write(txd_data)
            try:
                from apps.components.Txd_Editor.txd_workshop import TXDWorkshop
                txd_win = TXDWorkshop(main_window=None)
                txd_win.setWindowTitle(f"TXD Workshop — {stem}.txd")
                txd_win.show(); txd_win.resize(1000, 700)
                txd_win.open_txd_file(tmp_path)
                if not hasattr(self, '_standalone_txd_windows'):
                    self._standalone_txd_windows = []
                self._standalone_txd_windows.append(txd_win)
            except Exception as e:
                QMessageBox.critical(self, "TXD Error", f"Failed to open TXD Workshop:\n{e}")
        else:
            QMessageBox.warning(self, "TXD", "Could not build TXD from current textures.")

    def _build_txd_from_textures(self): #vers 3
        """TXD bytes from self._mod_textures; untouched textures keep their bytes."""
        from apps.methods.txd_splice import txd_from_textures
        try:
            return txd_from_textures(self._mod_textures, getattr(self, '_current_txd_bytes', None))
        except Exception as e:
            print(f"TXD build error: {e}")
            return None

    def _save_textures_as_txd(self): #vers 1
        """Save current textures as a new TXD file."""
        if not self._mod_textures:
            return
        stem = os.path.splitext(
            os.path.basename(getattr(self, '_current_dff_path', 'model')))[0]
        path, _ = QFileDialog.getSaveFileName(
            self, "Save TXD",
            os.path.join(os.path.dirname(
                getattr(self, '_current_dff_path', '')), stem + '.txd'),
            "TXD Files (*.txd);;All Files (*)")
        if not path:
            return
        data = self._build_txd_from_textures()
        if data:
            from apps.methods.file_backup import safe_write_bytes      # backup if it exists + atomic write
            safe_write_bytes(path, bytes(data))
            self._set_status(f"Saved TXD: {os.path.basename(path)}")
        else:
            QMessageBox.warning(self, "TXD", "Could not build TXD from current textures.")

    def _open_txd_smart(self): #vers 3
        """Open TXD — tries in order:
        1. IDE-linked TXD name → search current IMG (case-insensitive)
        2. DFF filename stem → search same folder as DFF (case-insensitive)
        3. DFF filename stem → search game_root recursively (case-insensitive)
        4. Ask user via file dialog, or skip
        Works the same whether docked or standalone."""
        import os, glob
        from PyQt6.QtWidgets import QFileDialog, QMessageBox

        mw  = getattr(self, 'main_window', None)
        dff_path = getattr(self, '_current_dff_path', '') or ''

        # Build candidate TXD names to search for (case-insensitive)
        candidates = []
        obj = getattr(self, '_current_ide_obj', None)
        if obj and getattr(obj, 'txd_name', ''):
            candidates.append(obj.txd_name.lower())
        if dff_path:
            candidates.append(os.path.splitext(os.path.basename(dff_path))[0].lower())
        candidates = list(dict.fromkeys(candidates))   # dedupe, preserve order

        def _open(path):  #vers 1
            if mw and hasattr(mw, 'open_txd_workshop_docked'):
                mw.open_txd_workshop_docked(file_path=path)
            else:
                from apps.components.Txd_Editor.txd_workshop import open_txd_workshop
                open_txd_workshop(self, path)

        def _name_matches(filename, candidate):  #vers 1
            return os.path.splitext(filename.lower())[0] == candidate

        # 1. Search current IMG
        img = (getattr(mw, 'current_img', None) if mw else None) or \
              getattr(self, 'current_img', None)
        if img:
            for cand in candidates:
                for entry in getattr(img, 'entries', []):
                    if _name_matches(entry.name, cand):
                        if mw and hasattr(mw, 'open_txd_workshop_docked'):
                            mw.open_txd_workshop_docked(txd_name=entry.name)
                        return

        # 2. Same folder as DFF — case-insensitive
        if dff_path:
            dff_dir = os.path.dirname(dff_path)
            for fname in os.listdir(dff_dir):
                if fname.lower().endswith('.txd'):
                    stem = os.path.splitext(fname.lower())[0]
                    if stem in candidates:
                        _open(os.path.join(dff_dir, fname))
                        return

        # 3. Recursive game_root search
        xref = self._get_xref() if hasattr(self, '_get_xref') else None
        game_root = getattr(xref, 'game_root', '') if xref else ''
        if not game_root and mw:
            game_root = getattr(mw, '_game_root', '') or ''
        if game_root:
            for cand in candidates:
                for fpath in glob.glob(
                        os.path.join(game_root, '**', '*.txd'), recursive=True):
                    if os.path.splitext(os.path.basename(fpath).lower())[0] == cand:
                        _open(fpath)
                        return

        # 4. Not found — ask user or skip
        cand_str = ' / '.join(f"{c}.txd" for c in candidates) or 'TXD'
        ans = QMessageBox.question(
            self, "TXD Not Found",
            f"Could not automatically find '{cand_str}'.\n"
            "Browse for it, or skip?",
            QMessageBox.StandardButton.Open |
            QMessageBox.StandardButton.Cancel,
            QMessageBox.StandardButton.Open)
        if ans == QMessageBox.StandardButton.Open:
            path, _ = QFileDialog.getOpenFileName(
                self, "Open TXD",
                os.path.dirname(dff_path) if dff_path else '',
                "Texture Files (*.txd *.wtd *.nft *.xtx *.chk);;TXD Files (*.txd);;All Files (*)")
            if path:
                _open(path)

    def _open_linked_txd(self): #vers 2

        """Open the IDE-linked TXD in TXD Workshop."""
        obj = getattr(self, '_current_ide_obj', None)
        if not obj or not obj.txd_name:
            return
        txd_name = obj.txd_name.lower()
        mw = getattr(self, 'main_window', None)

        # Try via main_window.open_txd_workshop_docked (IMG mode)
        if mw and hasattr(mw, 'open_txd_workshop_docked'):
            # Check if TXD is in the current IMG
            img = getattr(mw, 'current_img', None) or getattr(self, 'current_img', None)
            if img:
                for entry in getattr(img, 'entries', []):
                    if entry.name.lower() == txd_name + '.txd':
                        mw.open_txd_workshop_docked(txd_name=txd_name + '.txd')
                        return
            # Try standalone via xref game_root
            xref = self._get_xref()
            game_root = getattr(xref, 'game_root', '') if xref else ''
            if game_root:
                # Search for the TXD file on disk
                import glob
                pattern = os.path.join(game_root, '**', txd_name + '.txd')
                matches = glob.glob(pattern, recursive=True)
                if matches:
                    mw.open_txd_workshop_docked(file_path=matches[0])
                    return

        # Fallback: open file dialog pre-filtered to TXD
        from PyQt6.QtWidgets import QFileDialog
        path, _ = QFileDialog.getOpenFileName(
            self, f"Open TXD for {obj.txd_name}",
            getattr(self, '_current_dff_path', ''),
            "Texture Files (*.txd *.wtd *.nft *.xtx *.chk);;TXD Files (*.txd);;All Files (*)")
        if path and mw and hasattr(mw, 'open_txd_workshop_docked'):
            mw.open_txd_workshop_docked(file_path=path)
        elif path:
            from apps.components.Txd_Editor.txd_workshop import open_txd_workshop
            open_txd_workshop(self, path)


    def _load_txd_file_from_data(self, data: bytes, name: str): #vers 1
        """Load TXD from raw bytes into the texture panel."""
        try:
            import tempfile
            tmp_dir = tempfile.mkdtemp()
            tmp_path = os.path.join(tmp_dir, name if name.lower().endswith('.txd') else os.path.splitext(name)[0] + '.txd')
            with open(tmp_path, 'wb') as _f: _f.write(data)
            self._load_txd_file(tmp_path)
        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"TXD load error: {e}")
