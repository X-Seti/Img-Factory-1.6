#this belongs in apps/components/Retro_GTA_Workshop/depends/retro_media_func.py - Version: 5
# X-Seti - October 24 2026 - IMG Factory 1.6 - Retro GTA Workshop media tab

"""
Media tab: Sounds / Images / Videos sub-tabs. Each lists its files of the
game folder (Open adds one); the picked file's sounds or images are listed
with play controls or a preview and Import / Export / Export all buttons
(icons only when narrow). Import writes the file straight away (backup first).
"""

##Methods list -
#
##class RetroMediaMixin: -
# _create_media_tab
# _media_export_all
# _media_export_one
# _media_file_picked
# _media_back_from_dp5
# _media_image_write
# _media_import
# _media_item_bytes
# _media_item_picked
# _media_list_folder
# _media_open
# _media_page
# _media_part_picked
# _media_play
# _media_step
# _media_to_dp5

import os
import tempfile

import numpy as np
from PyQt6.QtCore import QSize, Qt
from PyQt6.QtGui import QIcon
from PyQt6.QtWidgets import (QComboBox, QFileDialog, QHBoxLayout, QLabel, QListView, QListWidget,
                             QListWidgetItem, QMessageBox, QScrollArea, QSplitter, QTabWidget, QToolButton,
                             QVBoxLayout, QWidget)

from apps.methods.fold_widgets import ButtonRow
from apps.methods.gta1_psx import vsr_gfx, vsr_gfx_rgba
from apps.methods.retro_media import (bank_replace, bank_samples, bank_wav, media_in_folder, sdt_replace,
                                      sdt_samples, sdt_wav, str_to_mp4, vsr_banks, vsr_image_folders, vsr_image_set)
from apps.methods.vsr_archive import VSRArchive

MEDIA_EXTS = ('.sdt', '.str', '.wav', '.vsr')
PAGE_OF = {'sdt': 'sounds', 'bank': 'sounds', 'wav': 'sounds', 'images': 'images', 'str': 'videos'}


class RetroMediaMixin: #vers 2
    """Media tab of the Retro GTA Workshop."""

    def _create_media_tab(self) -> QWidget: #vers 2
        """Sounds / Images / Videos sub-tabs."""
        from apps.methods.imgfactory_svg_icons import SVGIconFactory as I
        self._mp = {}
        tabs = QTabWidget()
        for kind, title, icon in (('sounds', "Sounds", I.volume_up_icon()), ('images', "Images", I.get_image_icon()),
                                  ('videos', "Videos", I.video_device_icon())):
            tabs.addTab(self._media_page(kind), icon, title)
        self._media_tabs = tabs
        return tabs

    def _media_page(self, kind: str) -> QWidget: #vers 2
        """Files | part combo, items, preview / player, buttons."""
        from apps.methods.imgfactory_svg_icons import SVGIconFactory as I
        p = {'files': QListWidget(), 'part': QComboBox(), 'list': QListWidget(), 'media': [], 'cur': None,
             'items': [], 'info': QLabel("")}
        self._mp[kind] = p
        page = QWidget()
        sp = QSplitter()
        left = QWidget()
        ll = QVBoxLayout(left)
        ll.setContentsMargins(0, 0, 0, 0)
        p['files'].currentRowChanged.connect(lambda r, k=kind: self._media_file_picked(k, r))
        ll.addWidget(p['files'], 1)
        ob = QToolButton()
        ob.setIcon(I.open_icon())
        ob.setText("Open")
        ob.setToolTip("Open a sound bank, image .vsr, video or WAV")
        ob.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
        ob.clicked.connect(lambda _c=False: self._media_open(''))
        orow = QHBoxLayout()
        orow.addWidget(ob)
        orow.addStretch(1)
        ll.addLayout(orow)
        right = QWidget()
        rl = QVBoxLayout(right)
        rl.setContentsMargins(0, 0, 0, 0)
        p['part'].setToolTip("Sound bank / image folder")
        p['part'].currentIndexChanged.connect(lambda _i, k=kind: self._media_part_picked(k))
        p['list'].currentRowChanged.connect(lambda r, k=kind: self._media_item_picked(k, r))
        rl.addWidget(p['info'])
        rl.addWidget(p['part'])

        def tb(icon, text, tip, fn):
            b = QToolButton()
            b.setIcon(icon)
            b.setText(text)
            b.setToolTip(tip)
            b.clicked.connect(lambda _c=False: fn())
            return b
        imp = tb(I.import_icon(), "Import", "Replace the picked item from a file (written at once, backup first)",
                 lambda k=kind: self._media_import(k))
        exp = tb(I.export_icon(), "Export", "Save the picked item", lambda k=kind: self._media_export_one(k))
        exa = tb(I.multi_export_icon(), "Export all", "Save every item of the bank / folder",
                 lambda k=kind: self._media_export_all(k))
        if kind == 'images':
            p['list'].setViewMode(QListView.ViewMode.IconMode)
            p['list'].setIconSize(QSize(48, 48))
            p['list'].setResizeMode(QListView.ResizeMode.Adjust)
            p['prev'] = QLabel()
            p['prev'].setAlignment(Qt.AlignmentFlag.AlignCenter)
            scroll = QScrollArea()
            scroll.setWidget(p['prev'])
            scroll.setWidgetResizable(True)
            mid = QSplitter(Qt.Orientation.Vertical)
            mid.addWidget(p['list'])
            mid.addWidget(scroll)
            rl.addWidget(mid, 1)
            dp5 = tb(I.paint_icon(), "Edit in DP5",
                     "Paint the picked image in DP5 Workshop; Send back writes the .vsr (backup first)",
                     self._media_to_dp5)
            rl.addWidget(ButtonRow([imp, exp, exa, dp5]))
        elif kind == 'sounds':
            from apps.methods.audioparser import MiniAudioPlayer
            p['list'].itemDoubleClicked.connect(lambda _it, k=kind: self._media_play(k))
            rl.addWidget(p['list'], 1)
            self._media_player = MiniAudioPlayer()
            rl.addWidget(self._media_player)
            prev = tb(I.frame_prev_icon(), "Previous", "Play the previous sound", lambda: self._media_step(-1))
            play = tb(I.play_icon(), "Play", "Play the picked sound", lambda k=kind: self._media_play(k))
            nxt = tb(I.frame_next_icon(), "Next", "Play the next sound", lambda: self._media_step(1))
            rl.addWidget(ButtonRow([prev, play, nxt, imp, exp, exa]))
        else:
            rl.addWidget(p['list'], 1)
            conv = tb(I.video_device_icon(), "Convert to MP4", "Video and its XA audio to MP4 (ffmpeg)",
                      lambda k=kind: self._media_play(k))
            rl.addWidget(ButtonRow([conv, exp]))
        sp.addWidget(left)
        sp.addWidget(right)
        sp.setSizes([220, 600])
        QVBoxLayout(page).addWidget(sp)
        return page

    def _media_list_folder(self, folder: str): #vers 2
        """Media of a game folder (and its sub folders) into the three tabs."""
        for p in self._mp.values():
            p['media'], p['cur'] = [], None
            p['files'].clear()
        for m in media_in_folder(folder):
            p = self._mp[PAGE_OF[m['kind']]]
            p['media'].append(m)
            p['files'].addItem(m['name'])

    def _media_open(self, path: str): #vers 2
        if not path:
            path, _ = QFileDialog.getOpenFileName(self, "Open sound bank, image .vsr, video or WAV",
                                                  self._data_dir or '', "Media (*.sdt *.vsr *.str *.wav);;All files (*)")
            if not path:
                return
        hit = [m for m in media_in_folder(os.path.dirname(path)) if m['path'] == path]
        if not hit:
            QMessageBox.warning(self, self.App_name, f"No sounds, images or video in {os.path.basename(path)}")
            return
        for m in hit:
            kind = PAGE_OF[m['kind']]
            p = self._mp[kind]
            p['media'].append(m)
            p['files'].addItem(m['name'])
            self._media_tabs.setCurrentIndex(list(self._mp).index(kind))
            p['files'].setCurrentRow(len(p['media']) - 1)

    def _media_file_picked(self, kind: str, row: int): #vers 2
        """Load a media file; fill the parts combo."""
        p = self._mp[kind]
        if not 0 <= row < len(p['media']):
            return
        m = dict(p['media'][row])
        try:
            if m['kind'] == 'sdt':
                with open(m['path'], 'rb') as f:
                    m['sdtdata'] = f.read()
                with open(m['raw'], 'rb') as f:
                    m['rawdata'] = f.read()
                m['samples'] = sdt_samples(m['sdtdata'])
                parts = [m['name']]
            elif m['kind'] in ('bank', 'images'):
                with open(m['path'], 'rb') as f:
                    m['arch'] = VSRArchive.from_bytes(f.read(), m['path'])
                parts = vsr_banks(m['arch']) if m['kind'] == 'bank' else vsr_image_folders(m['arch'])
            else:
                parts = [m['name']]
        except (OSError, ValueError, KeyError) as ex:
            self._set_status(str(ex))
            return
        p['cur'] = m
        p['part'].blockSignals(True)
        p['part'].clear()
        p['part'].addItems(parts)
        p['part'].blockSignals(False)
        p['part'].setVisible(len(parts) > 1 or m['kind'] in ('bank', 'images'))
        self._media_part_picked(kind)

    def _media_part_picked(self, kind: str): #vers 2
        """Items of the picked bank / folder."""
        p = self._mp[kind]
        m = p['cur']
        p['list'].clear()
        p['items'] = []
        if m is None:
            return
        part = p['part'].currentText()
        if m['kind'] == 'sdt':
            p['items'] = m['samples']
        elif m['kind'] == 'bank':
            p['items'] = bank_samples(m['arch'], part)
        elif m['kind'] == 'images':
            m['gfx'] = vsr_gfx(m['arch'], part)
            p['items'] = list(range(len(m['gfx']['index'])))
        else:
            p['items'] = [m]
        for i, e in enumerate(p['items']):
            if m['kind'] in ('sdt', 'bank'):
                it = QListWidgetItem(f"{e['n']:03d}   {e['size']:,} bytes   {e['rate']} Hz")
            elif m['kind'] == 'images':
                it = QListWidgetItem(QIcon(self._rgba_pixmap(np.ascontiguousarray(vsr_gfx_rgba(m['gfx'], i)))),
                                     f"{i:03d}")
            else:
                it = QListWidgetItem(m['name'])
            p['list'].addItem(it)
        p['info'].setText(f"{m['name']}: {len(p['items'])} items")
        if p['items']:
            p['list'].setCurrentRow(0)

    def _media_item_picked(self, kind: str, row: int): #vers 2
        p = self._mp[kind]
        m = p['cur']
        if m is None or not 0 <= row < len(p['items']) or m['kind'] != 'images':
            return
        a = np.ascontiguousarray(vsr_gfx_rgba(m['gfx'], row))
        pm = self._rgba_pixmap(a)
        k = max(1, min(4, 320 // max(a.shape[:2])))
        p['prev'].setPixmap(pm.scaled(pm.width() * k, pm.height() * k))
        p['info'].setText(f"{m['name']} {p['part'].currentText()} {row}: {a.shape[1]} x {a.shape[0]}")

    def _media_image_write(self, m: dict, row: int, rgba, gfx=None): #vers 1
        """Image row of a .vsr folder (default: the shown one) from RGBA; .vsr written (backup first)."""
        from apps.methods.file_backup import safe_write_bytes
        vsr_image_set(m['arch'], gfx or m['gfx'], row, rgba)
        safe_write_bytes(m['path'], m['arch'].to_bytes(), f"{self.App_name}: {m['name']}")

    def _media_to_dp5(self): #vers 1
        """Send the picked .vsr image to DP5 Workshop."""
        from apps.methods.dp5_link import send_textures_to_dp5
        p = self._mp['images']
        m, row = p['cur'], p['list'].currentRow()
        if m is None or row < 0:
            self._set_status("Pick an image first")
            return
        a = np.ascontiguousarray(vsr_gfx_rgba(m['gfx'], row))
        tex = {'name': f"{p['part'].currentText()}_{row:03d}", 'width': a.shape[1], 'height': a.shape[0],
               'media': m, 'gfx': m['gfx'], 'row': row, 'rgba_data': a.tobytes()}
        if send_textures_to_dp5(self, [tex], self.App_name, self._media_back_from_dp5):
            self._set_status(f"Image {row} sent to DP5 Workshop")

    def _media_back_from_dp5(self, tex, rgba, w, h): #vers 1
        """DP5 Send back: pixels into the .vsr image (same size, nearest CLUT colours)."""
        m, row = tex['media'], tex['row']
        try:
            a = np.frombuffer(bytes(rgba), np.uint8)[:w * h * 4].reshape(h, w, 4)
            self._media_image_write(m, row, a, tex['gfx'])
        except Exception as ex:
            QMessageBox.warning(self, "DP5", str(ex))
            return
        p = self._mp['images']
        if p['cur'] is m:
            self._media_part_picked('images')
            p['list'].setCurrentRow(row)
        self._set_status(f"Image {row} written from DP5: {m['path']}")

    def _media_item_bytes(self, kind: str, row: int) -> tuple: #vers 2
        """(file name, bytes) of an item: WAV for sounds, PNG for images, the file for videos / WAVs."""
        from PyQt6.QtCore import QBuffer, QIODevice
        p = self._mp[kind]
        m, part = p['cur'], p['part'].currentText()
        if m['kind'] == 'sdt':
            e = p['items'][row]
            return f"{os.path.splitext(m['name'])[0]}_{e['n']:03d}.wav", sdt_wav(m['rawdata'], e)
        if m['kind'] == 'bank':
            e = p['items'][row]
            return f"{part}_{e['n']:03d}.wav", bank_wav(m['arch'], part, e)
        if m['kind'] == 'images':
            buf = QBuffer()
            buf.open(QIODevice.OpenModeFlag.WriteOnly)
            self._rgba_pixmap(np.ascontiguousarray(vsr_gfx_rgba(m['gfx'], row))).save(buf, 'PNG')
            return f"{part}_{row:03d}.png", bytes(buf.data())
        with open(m['path'], 'rb') as f:
            return os.path.basename(m['path']), f.read()

    def _media_play(self, kind: str = 'sounds'): #vers 2
        """Play the picked sound (or the .wav); a video is converted to MP4."""
        p = self._mp[kind]
        m, row = p['cur'], p['list'].currentRow()
        if m is None or row < 0:
            return
        if m['kind'] == 'str':
            out, _ = QFileDialog.getSaveFileName(self, "Convert video", os.path.splitext(m['path'])[0] + '.mp4',
                                                 "MP4 video (*.mp4)")
            if out:
                try:
                    str_to_mp4(m['path'], out)
                except (OSError, ValueError, RuntimeError) as ex:
                    QMessageBox.warning(self, self.App_name, str(ex))
                    return
                self._set_status(f"Video converted: {out}")
            return
        if m['kind'] == 'wav':
            self._media_player.load_and_play(m['path'], m['name'])
            return
        try:
            name, data = self._media_item_bytes(kind, row)
        except (OSError, ValueError, RuntimeError) as ex:
            self._set_status(str(ex))
            return
        path = os.path.join(tempfile.gettempdir(), "retro_gta_" + name)
        with open(path, 'wb') as f:
            f.write(data)
        self._media_player.load_and_play(path, name)

    def _media_step(self, delta: int): #vers 1
        """Previous / next sound, played."""
        lst = self._mp['sounds']['list']
        if lst.count():
            lst.setCurrentRow((lst.currentRow() + delta) % lst.count())
            self._media_play('sounds')

    def _media_export_one(self, kind: str): #vers 2
        p = self._mp[kind]
        row = p['list'].currentRow()
        if p['cur'] is None or row < 0:
            return
        name, data = self._media_item_bytes(kind, row)
        path, _ = QFileDialog.getSaveFileName(self, "Export", os.path.join(self._data_dir or '', name))
        if path:
            with open(path, 'wb') as f:
                f.write(data)
            self._set_status(f"Exported {path}")

    def _media_export_all(self, kind: str): #vers 2
        """Every item of the picked bank / folder as WAV / PNG files."""
        p = self._mp[kind]
        m = p['cur']
        if m is None or m['kind'] in ('str', 'wav'):
            return
        folder = QFileDialog.getExistingDirectory(self, "Export all to folder", self._data_dir or '')
        if not folder:
            return
        out = os.path.join(folder, os.path.splitext(m['name'])[0] + '_' + p['part'].currentText())
        os.makedirs(out, exist_ok=True)
        n = 0
        for row in range(len(p['items'])):
            try:
                name, data = self._media_item_bytes(kind, row)
            except (OSError, ValueError, RuntimeError) as ex:
                self._set_status(str(ex))
                continue
            with open(os.path.join(out, name), 'wb') as f:
                f.write(data)
            n += 1
        self._set_status(f"Exported {n} files to {out}")

    def _media_import(self, kind: str): #vers 2
        """Replace the picked sound (WAV) / image (PNG, same size); the file is written (backup first)."""
        from apps.methods.file_backup import safe_write_bytes
        from apps.methods.retro_palette import png_rgba
        p = self._mp[kind]
        m, row = p['cur'], p['list'].currentRow()
        if m is None or row < 0:
            return
        flt = "PNG images (*.png)" if kind == 'images' else "WAV sounds (*.wav)"
        src, _ = QFileDialog.getOpenFileName(self, "Import", self._data_dir or '', flt)
        if not src:
            return
        part = p['part'].currentText()
        try:
            if m['kind'] == 'images':
                self._media_image_write(m, row, png_rgba(src))
            else:
                with open(src, 'rb') as f:
                    wav = f.read()
                if m['kind'] == 'sdt':
                    sdt, raw = sdt_replace(m['sdtdata'], m['rawdata'], p['items'][row]['n'], wav)
                    safe_write_bytes(m['path'], sdt, f"{self.App_name}: {m['name']}")
                    safe_write_bytes(m['raw'], raw, f"{self.App_name}: {os.path.basename(m['raw'])}")
                elif m['kind'] == 'bank':
                    bank_replace(m['arch'], part, p['items'][row]['n'], wav)
                    safe_write_bytes(m['path'], m['arch'].to_bytes(), f"{self.App_name}: {m['name']}")
                else:
                    safe_write_bytes(m['path'], wav, f"{self.App_name}: {m['name']}")
        except Exception as ex:
            QMessageBox.warning(self, "Import", str(ex))
            return
        files = p['files']
        self._media_file_picked(kind, files.currentRow())
        idx = p['part'].findText(part)
        if idx >= 0:
            p['part'].setCurrentIndex(idx)
        p['list'].setCurrentRow(row)
        self._set_status(f"Imported {os.path.basename(src)} into {m['name']}")
