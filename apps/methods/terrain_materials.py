#this belongs in apps/methods/terrain_materials.py - Version: 1
# X-Seti - October 22 2026 - IMG Factory 1.6 - Terrain material library

"""
Terrain material library: name, texture (from a game TXD, a PNG, or a
generated placeholder), UV size in metres and COL surface. Presets give
ground, grass, sand, rock, stone, dirt, wet land, road and path out of the box.
"""

##Methods list -
# default_library
# load_library
# material_texture
# placeholder_texture
# png_texture
# save_library

import json
import os
from typing import Dict, List

import numpy as np

# name, colour (r, g, b), noise strength, UV size (m), SA surface, VC surface
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
)


def placeholder_texture(name: str, colour, noise: int, size: int = 64) -> dict: #vers 1
    """Seamless noisy colour texture (RGBA) so a material works without art."""
    rng = np.random.default_rng(abs(hash(name)) % (2 ** 32))
    n = rng.normal(0.0, 1.0, (size, size))
    for _ in range(2):                                    # soften, wrapping edges
        n = (n + np.roll(n, 1, 0) + np.roll(n, -1, 0) + np.roll(n, 1, 1) + np.roll(n, -1, 1)) / 5.0
    n = n / (np.abs(n).max() or 1.0)
    rgb = np.clip(np.asarray(colour, dtype=np.float64)[None, None, :] + n[..., None] * noise, 0, 255)
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


def default_library() -> List[Dict]: #vers 1
    """The preset materials (generated textures)."""
    return [{'name': n, 'texture': n.lower().replace(' ', '_'), 'source': 'placeholder',
             'colour': list(c), 'noise': nz, 'uv_size': uv, 'surface_sa': sa, 'surface_vc': vc}
            for n, c, nz, uv, sa, vc in PRESETS]


def material_texture(mat: dict, game_textures=None) -> dict: #vers 1
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
    return placeholder_texture(tex_name, mat.get('colour', (128, 128, 128)), mat.get('noise', 20))


def save_library(lib: List[Dict], path: str) -> None: #vers 1
    with open(path, 'w') as f:
        json.dump(lib, f, indent=1)


def load_library(path: str) -> List[Dict]: #vers 1
    with open(path) as f:
        return json.load(f)
