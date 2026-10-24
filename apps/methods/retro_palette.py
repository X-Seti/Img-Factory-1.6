#this belongs in apps/methods/retro_palette.py - Version: 2
# X-Seti - October 24 2026 - IMG Factory 1.6 - top-down GTA palette helpers

"""
Shared colour helpers of the GTA1 / GTA2 styles: PS1 15-bit colours to
RGBA, RGBA to palette indices (nearest opaque colour, alpha < 128 to a
transparent entry), and 4-bit pixel writes into packed pages.
"""

##Methods list -
# png_rgba
# ps1_rgba
# put_4bit
# quantize
# rgba_ps1

import numpy as np


def ps1_rgba(c: np.ndarray) -> np.ndarray: #vers 1
    """PS1 15-bit colours (R5 G5 B5, 0 = transparent) as RGBA."""
    c = c.astype(np.uint32)
    out = np.stack([(c & 31) << 3, ((c >> 5) & 31) << 3, ((c >> 10) & 31) << 3,
                    np.where(c == 0, 0, 255)], -1)
    return out.astype(np.uint8)


def rgba_ps1(rgba: np.ndarray, old: np.ndarray = None) -> np.ndarray: #vers 1
    """RGBA as PS1 15-bit colours (alpha < 128 -> 0, black -> 0x8000); unchanged old colours kept."""
    r, g, b = (rgba[..., i].astype(np.uint16) >> 3 for i in range(3))
    c = r | (g << 5) | (b << 10)
    c = np.where(c == 0, 0x8000, c)
    c = np.where(rgba[..., 3] < 128, 0, c).astype(np.uint16)
    if old is not None:                                  # keep STP bits of unedited colours
        c = np.where((ps1_rgba(old) == rgba).all(-1), old, c).astype(np.uint16)
    return c


def png_rgba(path: str) -> np.ndarray: #vers 1
    """h x w x 4 RGBA of an image file (Qt reader)."""
    from PyQt6.QtGui import QImage
    img = QImage(path)
    if img.isNull():
        raise ValueError(f"Can't read image {path}")
    img = img.convertToFormat(QImage.Format.Format_RGBA8888)
    a = np.frombuffer(img.constBits().asstring(img.sizeInBytes()), np.uint8)
    return a.reshape(img.height(), img.bytesPerLine() // 4, 4)[:, :img.width()].copy()


def quantize(rgba: np.ndarray, pal: np.ndarray) -> np.ndarray: #vers 1
    """h x w palette indices: nearest opaque entry of pal (N x 4 RGBA); alpha < 128 -> a clear entry."""
    solid = np.nonzero(pal[:, 3] != 0)[0]
    clear = np.nonzero(pal[:, 3] == 0)[0]
    if not len(solid):
        raise ValueError("Palette has no opaque colours")
    h, w = rgba.shape[:2]
    cols = pal[solid, :3].astype(np.int32)
    px = rgba[:, :, :3].reshape(-1, 3).astype(np.int32)
    idx = np.empty(len(px), np.int64)
    for i in range(0, len(px), 4096):                    # chunks keep memory small
        d = ((px[i:i + 4096, None, :] - cols[None, :, :]) ** 2).sum(2)
        idx[i:i + 4096] = solid[d.argmin(1)]
    idx = np.where(rgba[:, :, 3].reshape(-1) < 128, clear[0] if len(clear) else 0, idx)
    return idx.astype(np.uint8).reshape(h, w)


def put_4bit(page: np.ndarray, x: int, y: int, idx: np.ndarray): #vers 1
    """Write 4-bit indices into a page of packed bytes (low nibble = even pixel) at pixel x, y."""
    h, w = idx.shape
    xs = x + np.arange(w)
    cols, low = xs // 2, (xs % 2) == 0
    rows = page[y:y + h]
    cur = rows[:, cols]
    cur = np.where(low[None, :], (cur & 0xF0) | idx, (cur & 0x0F) | (idx << 4))
    for k, c in enumerate(cols):                         # two pixels may share a byte
        if low[k] and k + 1 < w and cols[k + 1] == c:
            cur[:, k] = (idx[:, k] & 15) | (idx[:, k + 1] << 4)
            cur[:, k + 1] = cur[:, k]
    rows[:, cols] = cur
