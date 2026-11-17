#this belongs in apps/methods/image_actions.py - Version: 1
# X-Seti - November 17 2026 - TC Workshop - Image Actions.

"""
Image Actions - Rotate, flip, convert and set as wallpaper for selected images.
"""

##Methods list -
# convert_images
# is_image
# move_up
# rotate_images
# set_wallpaper

import os
import shutil
import subprocess

from PyQt6.QtGui import QImage, QImageReader, QTransform
from PyQt6.QtWidgets import QMessageBox

FORMATS = {"PNG": "png", "JPG": "jpg", "BMP": "bmp", "WEBP": "webp", "TIFF": "tif"}
_READ = {bytes(f).decode().lower() for f in QImageReader.supportedImageFormats()}


def is_image(path: str) -> bool: #vers 1
    """True for files Qt can read as an image."""
    return os.path.isfile(path) and os.path.splitext(path)[1][1:].lower() in _READ


def _free(path: str) -> str: #vers 1
    """Unused path: name, name (2), ..."""
    base, ext = os.path.splitext(path)
    n, new = 2, path
    while os.path.exists(new):
        new, n = f"{base} ({n}){ext}", n + 1
    return new


def _report(parent, done: int, errs: list): #vers 1
    if errs:
        QMessageBox.warning(parent, "Image", f"{done} done, failed:\n" + "\n".join(errs[:10]))


def rotate_images(paths: list, how: str, refresh=None, parent=None): #vers 1
    """how: cw, ccw, 180, flipx, flipy. Overwrites each image."""
    tf = QTransform()
    if how in ("cw", "ccw", "180"):
        tf.rotate({"cw": 90, "ccw": -90, "180": 180}[how])
    else:
        tf.scale(-1 if how == "flipx" else 1, -1 if how == "flipy" else 1)
    done, errs = 0, []
    for p in paths:
        img = QImage(p)
        if img.isNull() or not img.transformed(tf).save(p):
            errs.append(os.path.basename(p))
        else:
            done += 1
    _report(parent, done, errs)
    if refresh:
        refresh()


def convert_images(paths: list, ext: str, refresh=None, parent=None): #vers 1
    """Save a copy of each image as ext beside the original."""
    done, errs = 0, []
    for p in paths:
        out = _free(os.path.splitext(p)[0] + "." + ext)
        img = QImage(p)
        if ext in ("jpg", "bmp") and img.hasAlphaChannel():
            img = img.convertToFormat(QImage.Format.Format_RGB32)
        if img.isNull() or not img.save(out):
            errs.append(os.path.basename(p))
        else:
            done += 1
    _report(parent, done, errs)
    if refresh:
        refresh()


def set_wallpaper(path: str, parent=None): #vers 1
    """Set the desktop wallpaper (Plasma, else GNOME)."""
    if shutil.which("plasma-apply-wallpaperimage"):
        cmd = ["plasma-apply-wallpaperimage", path]
    elif shutil.which("gsettings"):
        cmd = ["gsettings", "set", "org.gnome.desktop.background", "picture-uri", "file://" + path]
    else:
        QMessageBox.warning(parent, "Wallpaper", "No wallpaper tool found.")
        return
    subprocess.Popen(cmd)


def move_up(paths: list, refresh=None, parent=None): #vers 1
    """Move items into the parent of their folder (../)."""
    errs = []
    for p in paths:
        dst = os.path.join(os.path.dirname(os.path.dirname(p)), os.path.basename(p))
        if os.path.exists(dst):
            errs.append(os.path.basename(p) + " exists")
            continue
        try:
            shutil.move(p, dst)
        except OSError as e:
            errs.append(f"{os.path.basename(p)}: {e.strerror}")
    _report(parent, len(paths) - len(errs), errs)
    if refresh:
        refresh()
