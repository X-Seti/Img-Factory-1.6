#this belongs in apps/components/DP5_Workshop/depends/dp5_slideshow.py - Version: 1
# X-Seti - November 14 2026 - IMG Factory 1.6 - DP5 slideshow viewer

"""
Full window viewer: one picture at a time, Left / Right to step, Space plays or pauses an
animated GIF, F toggles fit / actual size, Esc closes. Items come from the open canvas tabs
or from a folder of images (loaded one at a time).
"""

##Methods list -
# folder_items
# load_file_frames
# tab_items

##class SlideshowDialog: -
# __init__
# _bar
# _caption
# _show
# _step
# _tick
# keyPressEvent
# resizeEvent
# toggle_fit
# toggle_play

import os

from PyQt6.QtCore import Qt, QTimer
from PyQt6.QtGui import QImage, QPixmap
from PyQt6.QtWidgets import QDialog, QHBoxLayout, QLabel, QPushButton, QVBoxLayout, QWidget

_EXT = ('.gif', '.png', '.jpg', '.jpeg', '.bmp', '.webp', '.tga', '.tif', '.tiff', '.iff', '.ilbm')


def _qimage(rgba, w: int, h: int) -> QImage: #vers 1
    return QImage(bytes(rgba), w, h, w * 4, QImage.Format.Format_RGBA8888).copy()


def load_file_frames(path: str): #vers 1
    """(frames [QImage], delays ms) of an image file; every GIF frame composited."""
    from PIL import Image
    im = Image.open(path)
    frames, delays = [], []
    try:
        while True:
            f = im.copy().convert('RGBA')
            frames.append(_qimage(f.tobytes(), f.width, f.height))
            delays.append(int(im.info.get('duration', 100) or 100))
            im.seek(im.tell() + 1)
    except EOFError:
        pass
    return frames, delays


def folder_items(path: str) -> list: #vers 1
    """Items for every image file in the folder of path (or in path itself), sorted by name."""
    d = path if os.path.isdir(path) else os.path.dirname(path)
    names = sorted(n for n in os.listdir(d) if n.lower().endswith(_EXT))
    return [{'name': n, 'path': os.path.join(d, n)} for n in names]


def tab_items(ws) -> list: #vers 1
    """Items for the open canvas tabs; the active tab plays its animation frames."""
    cur = ws._capture_canvas_tab_state()
    tabs = ws._canvas_tabs or [{'label': '1', 'state': cur}]
    items = []
    for i, t in enumerate(tabs):
        st = cur if i == ws._active_canvas_tab_idx and cur else t['state']
        if i == ws._active_canvas_tab_idx and len(ws._frames) > 1 and len(ws._frames[0]) == st['tex_w'] * st['tex_h'] * 4:
            ws._anim_save_current_frame()
            fr = [_qimage(f, st['tex_w'], st['tex_h']) for f in ws._frames]
            dl = list(ws._frame_delays) + [100] * (len(fr) - len(ws._frame_delays))
        else:
            fr, dl = [_qimage(st['rgba'], st['tex_w'], st['tex_h'])], [100]
        items.append({'name': str(t['label']), 'frames': fr, 'delays': dl[:len(fr)]})
    return items


class SlideshowDialog(QDialog): #vers 1
    """One picture at a time; arrows step, Space plays an animation."""

    def __init__(self, parent, items: list, start: int = 0): #vers 1
        super().__init__(parent)
        self.setWindowTitle("Slideshow")
        self.setWindowFlag(Qt.WindowType.FramelessWindowHint, True)
        self.setStyleSheet("QDialog{background:#000;} QLabel{color:#ddd;} "
                           "QPushButton{padding:4px 12px;background:#222;color:#ddd;border:1px solid #444;}")
        self._items = items
        self._idx = max(0, min(start, len(items) - 1))
        self._frames, self._delays, self._f = [], [], 0
        self._fit = True
        self._timer = QTimer(self)
        self._timer.setSingleShot(True)
        self._timer.timeout.connect(self._tick)
        lay = QVBoxLayout(self)
        lay.setContentsMargins(0, 0, 0, 0)
        self._view = QLabel()
        self._view.setAlignment(Qt.AlignmentFlag.AlignCenter)
        lay.addWidget(self._view, 1)
        self._cap = QLabel()
        self._cap.setAlignment(Qt.AlignmentFlag.AlignCenter)
        lay.addWidget(self._cap)
        lay.addWidget(self._bar())
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)
        self.showFullScreen()
        self._show()

    def _bar(self): #vers 1
        w = QWidget()
        row = QHBoxLayout(w)
        row.addStretch(1)
        self._play_btn = QPushButton("Play")
        for text, fn in (("Prev", lambda: self._step(-1)), (None, self.toggle_play), ("Next", lambda: self._step(1)),
                         ("Fit / actual", self.toggle_fit), ("Close", self.close)):
            b = self._play_btn if text is None else QPushButton(text)
            b.clicked.connect(lambda _=False, f=fn: f())
            b.setFocusPolicy(Qt.FocusPolicy.NoFocus)
            row.addWidget(b)
        row.addStretch(1)
        return w

    def _show(self): #vers 1
        """Load the current item and show its first frame."""
        self._timer.stop()
        it = self._items[self._idx]
        try:
            if 'frames' not in it:
                it['frames'], it['delays'] = load_file_frames(it['path'])
            self._frames, self._delays = it['frames'], it['delays']
        except Exception as ex:
            self._frames, self._delays = [], []
            self._view.setText(f"Cannot open {it['name']}: {ex}")
            self._cap.setText("")
            return
        self._f = 0
        self._paint()
        self._play_btn.setVisible(len(self._frames) > 1)
        self._play_btn.setText("Play")
        self._caption()

    def _paint(self): #vers 1
        if not self._frames:
            return
        pm = QPixmap.fromImage(self._frames[self._f])
        if self._fit:
            pm = pm.scaled(self._view.size(), Qt.AspectRatioMode.KeepAspectRatio, Qt.TransformationMode.FastTransformation
                           if min(pm.width(), pm.height()) < 128 else Qt.TransformationMode.SmoothTransformation)
        self._view.setPixmap(pm)

    def _caption(self): #vers 1
        it = self._items[self._idx]
        n = len(self._frames)
        fr = f"  frame {self._f + 1}/{n}" if n > 1 else ""
        self._cap.setText(f"{it['name']}   {self._idx + 1}/{len(self._items)}{fr}   (Left / Right step, Space play, F fit, Esc close)")

    def _step(self, d: int): #vers 1
        self._idx = (self._idx + d) % len(self._items)
        self._show()

    def toggle_play(self): #vers 1
        if len(self._frames) < 2:
            return
        if self._timer.isActive():
            self._timer.stop()
            self._play_btn.setText("Play")
        else:
            self._play_btn.setText("Pause")
            self._timer.start(max(10, self._delays[self._f]))

    def _tick(self): #vers 1
        self._f = (self._f + 1) % len(self._frames)
        self._paint()
        self._caption()
        self._timer.start(max(10, self._delays[self._f]))

    def toggle_fit(self): #vers 1
        self._fit = not self._fit
        self._paint()

    def resizeEvent(self, e): #vers 1
        super().resizeEvent(e)
        self._paint()

    def keyPressEvent(self, e): #vers 1
        k = e.key()
        if k in (Qt.Key.Key_Escape, Qt.Key.Key_Q):
            self.close()
        elif k in (Qt.Key.Key_Right, Qt.Key.Key_PageDown):
            self._step(1)
        elif k in (Qt.Key.Key_Left, Qt.Key.Key_PageUp):
            self._step(-1)
        elif k == Qt.Key.Key_Home:
            self._idx = 0
            self._show()
        elif k == Qt.Key.Key_End:
            self._idx = len(self._items) - 1
            self._show()
        elif k == Qt.Key.Key_Space:
            self.toggle_play()
        elif k == Qt.Key.Key_F:
            self.toggle_fit()
        else:
            super().keyPressEvent(e)

    def closeEvent(self, e): #vers 1
        self._timer.stop()
        super().closeEvent(e)
