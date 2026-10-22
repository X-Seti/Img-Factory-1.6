#this belongs in apps/methods/terrain_image.py - Version: 1
# X-Seti - October 22 2026 - IMG Factory 1.6 - Terrain from image / radar

"""
Base terrain from a picture: any image file, or the game's radar map
stitched from its radar tiles at true world position. The picture becomes
a guide texture and its colours can pre-paint terrain materials.
"""

##Methods list -
# classify_cells
# guide_tile_texture
# load_image_rgba
# radar_extent
# radar_tiles_from_img
# stitch_radar

from typing import Dict, List, Optional, Tuple

import numpy as np

# Typical radar / map colours per material name (picture -> material).
GUIDE_COLOURS: Dict[str, List[Tuple[int, int, int]]] = {
    'Grass':     [(70, 110, 50), (90, 130, 60), (60, 95, 45)],
    'Dry grass': [(140, 140, 80), (160, 150, 95)],
    'Sand':      [(200, 180, 130), (215, 200, 150), (185, 165, 115)],
    'Dirt':      [(130, 100, 70), (110, 85, 60)],
    'Rock':      [(115, 105, 95)],
    'Stone':     [(160, 160, 155), (180, 180, 175)],
    'Road':      [(60, 60, 60), (85, 85, 85), (40, 40, 45)],
    'Wet land':  [(40, 70, 110), (60, 100, 150), (75, 120, 170)],   # water on radar
}


def load_image_rgba(path: str) -> np.ndarray: #vers 1
    """Image file as (h, w, 4) uint8 RGBA."""
    from PyQt6.QtGui import QImage
    img = QImage(path)
    if img.isNull():
        raise IOError(f"can't read image {path}")
    img = img.convertToFormat(QImage.Format.Format_RGBA8888)
    w, h = img.width(), img.height()
    data = bytes(img.constBits().asstring(img.sizeInBytes()))
    return np.frombuffer(data, np.uint8).reshape(h, img.bytesPerLine() // 4, 4)[:, :w].copy()


def radar_extent(game: str) -> Tuple[float, float, float, int]: #vers 1
    """(west x, south y, size, tiles per side) of a game's radar map."""
    from apps.methods.gta_dat_parser import RADAR_GRID_PRESETS
    p = RADAR_GRID_PRESETS[game]
    half = p['grid_size'] / 2.0
    return -half, -half, p['grid_size'], p['tiles_per_side']


def radar_tiles_from_img(img_path: str, game: str) -> List[Optional[Tuple[bytes, int, int]]]: #vers 1
    """Radar tiles (rgba, w, h) or None, in tile order, from an IMG archive."""
    from apps.methods.img_core_classes import IMGFile
    from apps.methods.model_cache import _parse_textures
    _x, _y, _s, per = radar_extent(game)
    arc = IMGFile(img_path)
    if not arc.open():
        raise IOError(f"can't open {img_path}")
    by_name = {e.name.lower(): e for e in arc.entries}
    out = []
    for i in range(per * per):
        name = f"radar{i:04d}.txd" if game == 'sol' else f"radar{i:02d}.txd"
        e = by_name.get(name)
        texs = _parse_textures(arc.read_entry_data(e), False) if e is not None else None
        t = texs[0] if texs else None
        out.append((t['rgba_data'], t['width'], t['height']) if t and t.get('rgba_data') else None)
    return out


def stitch_radar(tiles, per_side: int) -> np.ndarray: #vers 1
    """One RGBA picture from radar tiles (row 0 = north); missing tiles clear."""
    size = max((t[1] for t in tiles if t), default=128)
    out = np.zeros((per_side * size, per_side * size, 4), np.uint8)
    for i, t in enumerate(tiles):
        if not t:
            continue
        rgba, w, h = t
        tile = np.frombuffer(rgba, np.uint8)[:w * h * 4].reshape(h, w, 4)
        if (w, h) != (size, size):                       # odd-sized tile: nearest resize
            ys = (np.arange(size) * h // size)
            xs = (np.arange(size) * w // size)
            tile = tile[ys][:, xs]
        r, c = divmod(i, per_side)
        out[r * size:(r + 1) * size, c * size:(c + 1) * size] = tile
    return out


def classify_cells(image: np.ndarray, ny: int, nx: int, library: List[Dict]) -> np.ndarray: #vers 1
    """Material index per cell (ny, nx) from the picture's colours.
    Image covers the whole terrain, top row = north. Clear pixels -> 0."""
    h, w = image.shape[:2]
    ys = (np.arange(ny)[::-1] * h // ny)                 # grid row 0 is south
    xs = (np.arange(nx) * w // nx)
    ye = np.minimum(ys + max(1, h // ny), h)
    xe = np.minimum(xs + max(1, w // nx), w)
    # block average via integral image
    rgb = image[..., :3].astype(np.float64)
    a = image[..., 3]
    ii = np.zeros((h + 1, w + 1, 3))
    ii[1:, 1:] = rgb.cumsum(0).cumsum(1)
    s = ii[ye][:, xe] - ii[ys][:, xe] - ii[ye][:, xs] + ii[ys][:, xs]
    n = ((ye - ys)[:, None] * (xe - xs)[None, :])[..., None]
    avg = s / n
    refs, ids = [], []
    for k, mat in enumerate(library):
        cols = list(GUIDE_COLOURS.get(mat['name'], [])) + [tuple(mat.get('colour', (128, 128, 128)))]
        for col in cols:
            refs.append(col)
            ids.append(k)
    refs = np.array(refs, np.float64)
    d = ((avg[:, :, None, :] - refs[None, None]) ** 2).sum(-1)
    mats = np.array(ids, np.uint8)[d.argmin(-1)]
    clear = a[ys][:, xs] < 16                            # missing radar tiles
    mats[clear] = 0
    return mats


def guide_tile_texture(image: np.ndarray, frac: Tuple[float, float, float, float], name: str,
                       max_px: int = 512) -> dict: #vers 1
    """Texture dict for the picture part covering a tile.
    frac = (left, top, right, bottom) as 0..1 of the image."""
    from PyQt6.QtCore import Qt
    from PyQt6.QtGui import QImage
    h, w = image.shape[:2]
    x0, y0 = int(frac[0] * w), int(frac[1] * h)
    x1, y1 = max(x0 + 1, int(round(frac[2] * w))), max(y0 + 1, int(round(frac[3] * h)))
    crop = np.ascontiguousarray(image[y0:y1, x0:x1])
    ch, cw = crop.shape[:2]
    img = QImage(crop.data, cw, ch, cw * 4, QImage.Format.Format_RGBA8888)
    side = 1 << max(3, min(max_px, max(cw, ch)).bit_length() - 1)   # power of two
    img = img.scaled(side, side, Qt.AspectRatioMode.IgnoreAspectRatio,
                     Qt.TransformationMode.SmoothTransformation)
    img = img.convertToFormat(QImage.Format.Format_RGBA8888)
    data = bytes(img.constBits().asstring(img.sizeInBytes()))
    return {'name': name, 'width': side, 'height': side, 'rgba_data': data, 'format': 'DXT1'}
