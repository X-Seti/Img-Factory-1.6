#this belongs in apps/methods/dp5_link.py - Version: 1
# X-Seti - October 22 2026 - IMG Factory 1.6 - Send textures to DP5 and back

"""
Shared hand-off to DP5 Workshop: an editor sends texture dicts to DP5's
bitmap list; DP5's Send back returns the painted pixels to that editor.
"""

##Methods list -
# open_dp5
# send_textures_to_dp5
# texture_from_dp5

from typing import Callable, List, Optional


def open_dp5(owner): #vers 1
    """DP5 Workshop: IMG Factory tab when docked, else its own window."""
    mw = getattr(owner, 'main_window', None)
    if mw is not None and hasattr(mw, 'open_dp5_workshop_docked'):
        return mw.open_dp5_workshop_docked()
    from apps.components.DP5_Workshop.dp5_workshop import open_dp5_workshop
    dp5 = open_dp5_workshop()
    owner._dp5_window = dp5                                    # keep the window alive
    return dp5


def send_textures_to_dp5(owner, textures: List[dict], label: str,
                         on_back: Callable[[dict, bytes, int, int], None]) -> int: #vers 1
    """Put textures in DP5's bitmap list; on_back(tex, rgba, w, h) on Send back."""
    dp5 = open_dp5(owner)
    if dp5 is None:
        return 0
    sent = 0
    for t in textures:
        w, h, rgba = t.get('width', 0), t.get('height', 0), t.get('rgba_data')
        if rgba and w and h and len(rgba) >= w * h * 4:
            dp5._load_rgba(bytearray(bytes(rgba)[:w * h * 4]), w, h, t.get('name', 'texture'),
                           return_cb=lambda _n, px, nw, nh, tex=t: on_back(tex, px, nw, nh),
                           return_label=label)
            sent += 1
    return sent


def texture_from_dp5(tex: dict, rgba: bytes, w: int, h: int) -> Optional[dict]: #vers 1
    """Store painted pixels in a texture dict; re-encoded when the TXD is saved."""
    if len(rgba) < w * h * 4:
        return None
    tex['rgba_data'] = bytes(rgba[:w * h * 4])
    tex['width'], tex['height'] = w, h
    tex['compressed_data'], tex['mipmap_levels'] = b'', []
    tex.pop('dxt_levels', None)                                  # viewport uses the new pixels
    if tex.get('format') in ('PAL8', 'PAL4'):
        tex['format'] = 'DXT1'
    return tex
