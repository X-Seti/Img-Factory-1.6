#this belongs in apps/methods/terrain_materials.py - Version: 2
# X-Seti - October 22 2026 - IMG Factory 1.6 - Terrain material library

"""
Terrain material library: name, texture (from a game TXD, a PNG, or a
generated placeholder), UV size in metres and COL surface. Presets give
ground, grass, sand, rock, stone, dirt, wet land, and path types (road,
lined road, rail track, pavement, cobbles, bricks, gravel, boardwalk).
"""

##Methods list -
# default_library
# load_library
# material_texture
# _pattern
# placeholder_texture
# png_texture
# save_library

import json
import os
from typing import Dict, List

import numpy as np

# name, colour (r, g, b), noise strength, UV size (m), SA surface, VC surface[, pattern]
PRESETS = (
    ("Grass",    (78, 122, 52),   26, 8.0,  "Grass (medium, lush)", "Grass"),
    ("Dry grass", (138, 130, 70), 26, 8.0,  "Grass (medium, dry)",  "Grass"),
    ("Sand",     (206, 186, 132), 14, 6.0,  "Sand (beach)",         "Sand"),
    ("Rock",     (112, 106, 98),  40, 10.0, "Rock (dry)",           "Rock"),
    ("Stone",    (150, 148, 140), 30, 4.0,  "Gravel",               "Concrete"),
    ("Dirt",     (120, 92, 62),   24, 6.0,  "Dirt",                 "Dirt"),
    ("Wet land", (70, 72, 46),    20, 6.0,  "Mud (wet)",            "Mud"),
    ("Road",     (64, 64, 66),    8,  8.0,  "Tarmac",               "Street"),
    ("Path",     (150, 120, 84),  18, 4.0,  "Dirt Track",           "Dirt"),
    ("Road (lined)", (62, 62, 64), 8, 8.0,  "Tarmac",               "Street",   'lines'),
    ("Rail track", (112, 100, 86), 22, 4.0,  "Rail Track",           "Concrete", 'rail'),
    ("Pavement", (160, 158, 152), 10, 4.0,  "Pavement",             "Concrete", 'slabs'),
    ("Cobblestone", (118, 114, 108), 18, 2.0, "Pavement (cracked)", "Concrete", 'cobble'),
    ("Brick path", (150, 72, 52), 14, 2.0,  "Pavement",             "Concrete", 'bricks'),
    ("Gravel",   (168, 162, 150), 34, 3.0,  "Gravel",               "Concrete"),
    ("Boardwalk", (132, 98, 62),  12, 3.0,  "Wood (solid)",         "Wood",     'planks'),
)


def _pattern(kind: str, size: int):
    """(shade, mask) arrays for a texture pattern: shade multiplies the base colour,
    mask (0..1) blends in the pattern's own colour (rails, line paint)."""
    u = np.arange(size)[None, :] / size                    # along the strip
    v = np.arange(size)[:, None] / size                    # across
    shade = np.ones((size, size))
    mask = np.zeros((size, size))
    if kind == 'lines':                                     # dashed centre line
        mask = ((np.abs(v - 0.5) < 0.025) & ((u * 4) % 1 < 0.55)).astype(float) + 0 * u
    elif kind == 'rail':                                    # sleepers across, two rails along
        sleeper = ((u * 6) % 1 < 0.38) & (np.abs(v - 0.5) < 0.30)
        shade = np.where(sleeper, 0.55, 1.0)
        mask = ((np.abs(v - 0.37) < 0.02) | (np.abs(v - 0.63) < 0.02)).astype(float) + 0 * u
    elif kind == 'slabs':                                   # paving slabs
        shade = np.where(((u * 4) % 1 < 0.03) | ((v * 4) % 1 < 0.03), 0.7, 1.0)
    elif kind in ('bricks', 'cobble'):                       # running bond
        rows = 8 if kind == 'bricks' else 6
        cols = 4 if kind == 'bricks' else 6
        row = np.floor(v * rows)
        off = (row % 2) * 0.5
        joint = ((v * rows) % 1 < 0.08) | (((u * cols + off) % 1) < 0.06)
        shade = np.where(joint, 0.6, 1.0) + 0 * u
    elif kind == 'planks':                                  # boards along, gaps between
        shade = np.where((v * 5) % 1 < 0.06, 0.45, 1.0) + 0 * u
    return shade, mask


def placeholder_texture(name: str, colour, noise: int, size: int = 64, pattern: str = '') -> dict: #vers 2
    """Seamless noisy colour texture (RGBA) so a material works without art;
    pattern adds lines / rails / slabs / bricks / cobble / planks."""
    if pattern:
        size = max(size, 128)
    rng = np.random.default_rng(abs(hash(name)) % (2 ** 32))
    n = rng.normal(0.0, 1.0, (size, size))
    for _ in range(2):                                    # soften, wrapping edges
        n = (n + np.roll(n, 1, 0) + np.roll(n, -1, 0) + np.roll(n, 1, 1) + np.roll(n, -1, 1)) / 5.0
    n = n / (np.abs(n).max() or 1.0)
    rgb = np.asarray(colour, dtype=np.float64)[None, None, :] + n[..., None] * noise
    if pattern:
        shade, mask = _pattern(pattern, size)
        paint = (235.0, 235.0, 225.0) if pattern == 'lines' else (150.0, 152.0, 158.0)   # line paint / steel
        rgb = rgb * shade[..., None]
        rgb = rgb * (1 - mask[..., None]) + np.asarray(paint)[None, None, :] * mask[..., None]
    rgb = np.clip(rgb, 0, 255)
    rgba = np.concatenate([rgb, np.full((size, size, 1), 255.0)], axis=-1).astype(np.uint8)
    return {'name': name, 'width': size, 'height': size, 'rgba_data': rgba.tobytes(), 'format': 'DXT1'}


def png_texture(path: str, name: str) -> dict: #vers 1
    """RGBA texture from an image file, resized to a power of two (max 512)."""
    from PyQt6.QtGui import QImage
    img = QImage(path)
    if img.isNull():
        raise ValueError(f"can't read image {path}")
    def p2(v):
        return max(4, min(512, 1 << (max(1, v) - 1).bit_length()))
    img = img.convertToFormat(QImage.Format.Format_RGBA8888).scaled(p2(img.width()), p2(img.height()))
    return {'name': name, 'width': img.width(), 'height': img.height(),
            'rgba_data': bytes(img.constBits().asstring(img.sizeInBytes())), 'format': 'DXT1'}


def default_library() -> List[Dict]: #vers 2
    """The preset materials (generated textures)."""
    out = []
    for p in PRESETS:
        n, c, nz, uv, sa, vc = p[:6]
        tex = ''.join(ch for ch in n.lower().replace(' ', '_') if ch.isalnum() or ch == '_')
        out.append({'name': n, 'texture': tex, 'source': 'placeholder', 'colour': list(c), 'noise': nz,
                    'uv_size': uv, 'surface_sa': sa, 'surface_vc': vc, 'pattern': p[6] if len(p) > 6 else ''})
    return out


def material_texture(mat: dict, game_textures=None) -> dict: #vers 2
    """Texture dict for a library entry: TXD texture, PNG, or placeholder."""
    tex_name = mat['texture']
    src = mat.get('source', 'placeholder')
    if src == 'png' and mat.get('path') and os.path.isfile(mat['path']):
        return png_texture(mat['path'], tex_name)
    if src == 'txd' and game_textures:
        t = game_textures(mat.get('txd', ''), mat.get('txd_texture', tex_name))
        if t is not None:
            out = dict(t)
            out['name'] = tex_name
            return out
    return placeholder_texture(tex_name, mat.get('colour', (128, 128, 128)), mat.get('noise', 20),
                               pattern=mat.get('pattern', ''))


def save_library(lib: List[Dict], path: str) -> None: #vers 1
    with open(path, 'w') as f:
        json.dump(lib, f, indent=1)


def load_library(path: str) -> List[Dict]: #vers 1
    with open(path) as f:
        return json.load(f)
