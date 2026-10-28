#this belongs in apps/components/DP5_Workshop/depends/dp5_retro_screens.py - Version: 1
# X-Seti - October 27 2026 - IMG Factory 1.6 - DP5 retro screen encoders.

"""
RGBA canvas -> native screen memory (no Qt), with per-cell colour choice:
ZX Spectrum SCR (6912 bytes, 2 colours per 8x8 cell, bright per cell),
C64 hires (bitmap + screen matrix) and multicolour (bitmap, screen,
colour RAM, background), Amstrad CPC mode 0 / 1 screen (16 KB at &C000).
"""

##Methods list -
# _nearest
# _resize
# c64_hires
# c64_multi
# cpc_screen
# zx_scr

import numpy as np
from PIL import Image

ZX_RGB = np.array([(0, 0, 0), (0, 0, 215), (215, 0, 0), (215, 0, 215), (0, 215, 0), (0, 215, 215),
                   (215, 215, 0), (215, 215, 215)], np.int32)           # by ZX colour number (GRB bits)
C64_RGB = np.array([(0, 0, 0), (255, 255, 255), (104, 55, 43), (112, 164, 178), (111, 61, 134),
                    (88, 141, 67), (53, 40, 121), (184, 199, 111), (111, 79, 37), (67, 57, 0),
                    (154, 103, 89), (68, 68, 68), (108, 108, 108), (154, 210, 132), (108, 94, 181),
                    (149, 149, 149)], np.int32)                         # Pepto PAL
CPC_RGB = np.array([((n // 3) % 3 * 0x80 - (n // 3) % 3 // 2, n // 9 * 0x80 - n // 9 // 2,
                     n % 3 * 0x80 - n % 3 // 2) for n in range(27)], np.int32)   # firmware colours 0-26


def _resize(rgba, w: int, h: int, tw: int, th: int) -> np.ndarray: #vers 1
    """th x tw x 3 int32 RGB, nearest-neighbour scaled."""
    img = Image.frombytes('RGBA', (w, h), bytes(rgba)).convert('RGB')
    if (w, h) != (tw, th):
        img = img.resize((tw, th), Image.Resampling.NEAREST)
    return np.asarray(img, np.int32)


def _nearest(rgb: np.ndarray, pal: np.ndarray) -> np.ndarray: #vers 1
    """Index of the nearest palette colour for each pixel (... x 3 -> ...)."""
    d = ((rgb[..., None, :] - pal) ** 2).sum(-1)
    return d.argmin(-1)


def zx_scr(rgba, w: int, h: int) -> bytes: #vers 1
    """6912-byte SCR: best ink / paper / bright for each 8 x 8 cell."""
    rgb = _resize(rgba, w, h, 256, 192)
    both = np.concatenate([ZX_RGB, np.where(ZX_RGB > 0, 255, 0)])     # 0-7 normal, 8-15 bright
    idx = _nearest(rgb, both)
    bitmap, attrs = bytearray(6144), bytearray(768)
    for cy in range(24):
        for cx in range(32):
            cell = idx[cy * 8:cy * 8 + 8, cx * 8:cx * 8 + 8]
            base = cell % 8
            bright = int((cell[base > 0] >= 8).sum() * 2 > (base > 0).sum())
            pal = both[8:] if bright else both[:8]
            counts = np.bincount(base.ravel(), minlength=8)
            order = np.argsort(-counts, kind='stable')
            ink, paper = int(order[0]), int(order[1]) if counts[order[1]] else int(order[0])
            px = rgb[cy * 8:cy * 8 + 8, cx * 8:cx * 8 + 8]
            on = ((px - pal[ink]) ** 2).sum(-1) <= ((px - pal[paper]) ** 2).sum(-1)
            if ink == paper:
                on[:] = False
            for row in range(8):
                y = cy * 8 + row
                addr = (y // 64) * 2048 + (y % 8) * 256 + ((y % 64) // 8) * 32 + cx
                bitmap[addr] = int(np.packbits(on[row])[0])
            attrs[cy * 32 + cx] = (bright << 6) | (paper << 3) | ink
    return bytes(bitmap + attrs)


def c64_hires(rgba, w: int, h: int): #vers 1
    """(bitmap 8000, screen 1000): per cell fg (upper nibble) / bg, C64 palette."""
    idx = _nearest(_resize(rgba, w, h, 320, 200), C64_RGB)
    bitmap, screen = bytearray(8000), bytearray(1000)
    for cy in range(25):
        for cx in range(40):
            cell = idx[cy * 8:cy * 8 + 8, cx * 8:cx * 8 + 8]
            counts = np.bincount(cell.ravel(), minlength=16)
            order = np.argsort(-counts, kind='stable')
            fg, bg = int(order[0]), int(order[1]) if counts[order[1]] else 0
            px = C64_RGB[cell]
            on = ((px - C64_RGB[fg]) ** 2).sum(-1) <= ((px - C64_RGB[bg]) ** 2).sum(-1)
            i = cy * 40 + cx
            screen[i] = (fg << 4) | bg
            bitmap[i * 8:i * 8 + 8] = np.packbits(on, axis=1).ravel().tobytes()
    return bytes(bitmap), bytes(screen)


def c64_multi(rgba, w: int, h: int): #vers 1
    """(bitmap 8000, screen 1000, colour 1000, background): 160 x 200, 4 colours per cell."""
    idx = _nearest(_resize(rgba, w, h, 160, 200), C64_RGB)
    bg = int(np.bincount(idx.ravel(), minlength=16).argmax())
    bitmap, screen, colour = bytearray(8000), bytearray(1000), bytearray(1000)
    for cy in range(25):
        for cx in range(40):
            cell = idx[cy * 8:cy * 8 + 8, cx * 4:cx * 4 + 4]
            counts = np.bincount(cell.ravel(), minlength=16)
            counts[bg] = 0
            top = [int(c) for c in np.argsort(-counts, kind='stable')[:3]]
            top = [c if counts[c] else bg for c in top]
            pal = np.array([C64_RGB[bg]] + [C64_RGB[c] for c in top])
            code = ((C64_RGB[cell][:, :, None, :] - pal) ** 2).sum(-1).argmin(-1)   # 0 bg, 1-3 colours
            i = cy * 40 + cx
            screen[i] = (top[0] << 4) | top[1]
            colour[i] = top[2]
            for row in range(8):
                b = 0
                for col in range(4):
                    b = (b << 2) | int(code[row, col])
                bitmap[i * 8 + row] = b
    return bytes(bitmap), bytes(screen), bytes(colour), bg


def cpc_screen(rgba, w: int, h: int, mode: int = 1): #vers 1
    """(16384-byte screen for &C000, firmware inks list). Mode 0: 160 x 200 16 inks, mode 1: 320 x 200 4 inks."""
    pens = 16 if mode == 0 else 4
    width = 160 if mode == 0 else 320
    rgb = _resize(rgba, w, h, width, 200)
    fw = _nearest(rgb, CPC_RGB)
    inks = [int(c) for c in np.argsort(-np.bincount(fw.ravel(), minlength=27), kind='stable')[:pens]]
    pen = _nearest(rgb, CPC_RGB[inks])
    mem = bytearray(16384)
    per = 2 if mode == 0 else 4                          # pixels per byte
    if mode == 0:
        bits = ((7, 3, 5, 1), (6, 2, 4, 0))              # pixel 0 / 1: pen bits 0-3
    else:
        bits = ((7, 3), (6, 2), (5, 1), (4, 0))          # pixels 0-3: pen bits 0-1
    for y in range(200):
        base = (y // 8) * 80 + (y % 8) * 2048
        for xb in range(80):
            b = 0
            for p, pb in enumerate(bits):
                v = int(pen[y, xb * per + p])
                for k, bit in enumerate(pb):
                    if v >> k & 1:
                        b |= 1 << bit
            mem[base + xb] = b
    return bytes(mem), inks
