#this belongs in apps/components/DP5_Workshop/plugins/gif_resize.py - Version: 1
# X-Seti - November 14 2026 - IMG Factory 1.6 - DP5 GIF Resize Plugin.

"""
GIF Resize - resize or upscale every frame of the open animation to an exact size,
a fit-inside box or a percentage; or batch resize many GIF files into a folder.
Frame delays and loop count are kept. GIF transparency is 1-bit.
"""

##Methods list -
# batch
# open_animation
# resize_gif_file
# resize_image
# target_size

import os

from PIL import Image
from PyQt6.QtWidgets import (QCheckBox, QComboBox, QDialog, QDialogButtonBox, QFileDialog, QFormLayout,
                             QHBoxLayout, QLabel, QLineEdit, QMessageBox, QProgressDialog, QPushButton,
                             QSpinBox, QVBoxLayout)

PLUGIN_NAME = "GIF Resize"

METHODS = (("Smooth (Lanczos)", Image.LANCZOS), ("Bicubic", Image.BICUBIC), ("Nearest (pixel art)", Image.NEAREST))
MODES = (("Exact size", 'exact'), ("Fit inside (keep aspect)", 'fit'), ("Percent", 'percent'))


def target_size(w: int, h: int, spec: dict): #vers 1
    """(new w, new h) for a w x h frame under spec {mode, w, h, percent}."""
    mode = spec['mode']
    if mode == 'percent':
        p = spec['percent'] / 100.0
        return max(1, round(w * p)), max(1, round(h * p))
    if mode == 'fit':
        s = min(spec['w'] / w, spec['h'] / h)
        return max(1, round(w * s)), max(1, round(h * s))
    return max(1, spec['w']), max(1, spec['h'])


def resize_image(img, size, method): #vers 1
    """RGBA image resized; transparent pixels keep clean edges (premultiplied)."""
    if img.size == size:
        return img
    a = img.getchannel('A')
    rgb = img.convert('RGB').resize(size, method)
    return Image.merge('RGBA', (*rgb.split(), a.resize(size, method)))


def resize_gif_file(src: str, dst: str, spec: dict): #vers 1
    """Resize every frame of the GIF src into dst; returns (frames, new w, new h)."""
    gif = Image.open(src)
    frames, delays = [], []
    try:
        while True:
            frames.append(gif.copy().convert('RGBA'))
            delays.append(int(gif.info.get('duration', 100) or 100))
            gif.seek(gif.tell() + 1)
    except EOFError:
        pass
    size = target_size(frames[0].width, frames[0].height, spec)
    out = []
    for f in frames:
        r = resize_image(f, size, spec['method'])
        r.putalpha(r.getchannel('A').point(lambda v: 255 if v >= 128 else 0))
        out.append(r)
    out[0].save(dst, format='GIF', save_all=True, append_images=out[1:], duration=delays,
                loop=gif.info.get('loop', 0), disposal=2, optimize=False)
    return len(out), size[0], size[1]


def _settings_form(form, w0: int = 0, h0: int = 0): #vers 1
    """Mode, size, percent and method rows; returns a function giving the spec."""
    mode = QComboBox()
    for label, key in MODES:
        mode.addItem(label, key)
    ws, hs = QSpinBox(), QSpinBox()
    for s, v in ((ws, w0 or 256), (hs, h0 or 256)):
        s.setRange(1, 16384)
        s.setValue(v)
    pc = QSpinBox()
    pc.setRange(1, 1600)
    pc.setValue(200)
    pc.setSuffix(" %")
    meth = QComboBox()
    for label, m in METHODS:
        meth.addItem(label, m)
    form.addRow("Mode", mode)
    form.addRow("Width", ws)
    form.addRow("Height", hs)
    form.addRow("Percent", pc)
    form.addRow("Method", meth)

    def sync():
        k = mode.currentData()
        ws.setEnabled(k != 'percent')
        hs.setEnabled(k != 'percent')
        pc.setEnabled(k == 'percent')
    mode.currentIndexChanged.connect(sync)
    sync()
    return lambda: {'mode': mode.currentData(), 'w': ws.value(), 'h': hs.value(),
                    'percent': pc.value(), 'method': meth.currentData()}


def open_animation(host): #vers 1
    """Resize every frame of the open animation."""
    fr = host.frames()
    if not fr:
        host.status("No image open")
        return
    frames, delays, w, h = fr
    dlg = QDialog(host.widget)
    dlg.setWindowTitle("Resize animation")
    lay = QVBoxLayout(dlg)
    lay.addWidget(QLabel(f"{len(frames)} frame(s), now {w} x {h}"))
    form = QFormLayout()
    spec = _settings_form(form, w, h)
    lay.addLayout(form)
    bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
    bb.accepted.connect(dlg.accept)
    bb.rejected.connect(dlg.reject)
    lay.addWidget(bb)
    if not dlg.exec():
        return
    sp = spec()
    size = target_size(w, h, sp)
    out = []
    for f in frames:
        im = Image.frombytes('RGBA', (w, h), bytes(f))
        out.append(bytearray(resize_image(im, size, sp['method']).tobytes()))
    host.set_frames(out, delays, size[0], size[1])
    host.status(f"Resized {len(out)} frame(s) to {size[0]} x {size[1]} (File > Animation > Revert clean-up undoes)")


def batch(host): #vers 1
    """Resize many GIF files into a folder."""
    files, _ = QFileDialog.getOpenFileNames(host.widget, "GIF files to resize", "", "GIF (*.gif);;All Files (*)")
    if not files:
        return
    dlg = QDialog(host.widget)
    dlg.setWindowTitle(f"Batch resize {len(files)} GIF file(s)")
    lay = QVBoxLayout(dlg)
    form = QFormLayout()
    spec = _settings_form(form)
    out_edit = QLineEdit(os.path.join(os.path.dirname(files[0]), "resized"))
    browse = QPushButton("Browse…")
    row = QHBoxLayout()
    row.addWidget(out_edit, 1)
    row.addWidget(browse)
    browse.clicked.connect(lambda: out_edit.setText(QFileDialog.getExistingDirectory(dlg, "Output folder", out_edit.text()) or out_edit.text()))
    form.addRow("Output folder", row)
    lay.addLayout(form)
    lay.addWidget(QLabel("Originals are never changed; results keep the file names."))
    bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
    bb.accepted.connect(dlg.accept)
    bb.rejected.connect(dlg.reject)
    lay.addWidget(bb)
    if not dlg.exec():
        return
    sp = spec()
    out_dir = out_edit.text().strip()
    if not out_dir:
        return
    os.makedirs(out_dir, exist_ok=True)
    prog = QProgressDialog("Resizing…", "Stop", 0, len(files), host.widget)
    prog.setWindowTitle("Batch resize")
    prog.setMinimumDuration(0)
    done, bad = 0, []
    from PyQt6.QtWidgets import QApplication
    for i, src in enumerate(files):
        prog.setValue(i)
        prog.setLabelText(os.path.basename(src))
        QApplication.processEvents()
        if prog.wasCanceled():
            break
        try:
            resize_gif_file(src, os.path.join(out_dir, os.path.basename(src)), sp)
            done += 1
        except Exception as ex:
            bad.append(f"{os.path.basename(src)}: {type(ex).__name__}: {ex}")
    prog.setValue(len(files))
    host.status(f"Batch resize: {done} done, {len(bad)} failed -> {out_dir}")
    if bad:
        QMessageBox.warning(host.widget, "Batch resize", f"{len(bad)} file(s) failed:\n" + "\n".join(bad[:15]))


PLUGIN_ACTIONS = [("Resize open animation (all frames)…", open_animation),
                  (None, None),
                  ("Batch resize GIF files…", batch)]
