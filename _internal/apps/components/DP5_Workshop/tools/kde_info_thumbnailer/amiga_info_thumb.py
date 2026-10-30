#!/usr/bin/env python3
#this belongs in apps/components/DP5_Workshop/tools/kde_info_thumbnailer/amiga_info_thumb.py - Version: 1
# X-Seti - October 27 2026 - IMG Factory 1.6 - Amiga .info thumbnailer for KDE / freedesktop.

"""
Amiga .info thumbnails and details, using amiga_icon.py installed beside it.
  amiga_info_thumb.py thumb IN OUT SIZE      PNG thumbnail (KIO 6.6+ / any freedesktop host)
  amiga_info_thumb.py details IN [--dialog]  type, default tool, tool types, stack, images
"""

##Methods list -
# details
# main
# thumb

import os
import subprocess
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import amiga_icon                                        # noqa: E402  (copied beside this script)


def thumb(src: str, dst: str, size: int) -> int: #vers 1
    """Best image of the icon, scaled up by whole steps (pixel art) within size, as PNG."""
    from PIL import Image
    with open(src, 'rb') as f:
        rgba, w, h, kind = amiga_icon.decode_info(f.read(), amiga_icon.PALETTES['wb39'])
    if not rgba:
        print(f"{src}: {kind}", file=sys.stderr)
        return 1
    img = Image.frombytes('RGBA', (w, h), rgba)
    k = max(1, size // max(w, h))
    if k > 1:
        img = img.resize((w * k, h * k), Image.Resampling.NEAREST)
    if max(img.size) > size:
        img.thumbnail((size, size), Image.Resampling.LANCZOS)
    img.save(dst, 'PNG')
    return 0


def details(src: str, dialog: bool) -> int: #vers 1
    """Print (or show in kdialog) the icon's details."""
    with open(src, 'rb') as f:
        info = amiga_icon.info_details(f.read())
    lines = [f"File: {os.path.basename(src)}", f"Type: {info['type']}",
             f"Images: {', '.join(info['images'])}", f"Stack: {info['stack']}",
             f"Default tool: {info['default_tool'] or '-'}", "Tool types:"]
    lines += [f"  {t}" for t in info['tooltypes']] or ["  -"]
    text = "\n".join(lines)
    if dialog:
        subprocess.run(['kdialog', '--title', 'Amiga icon', '--msgbox', text], check=False)
    else:
        print(text)
    return 0


def main(argv) -> int: #vers 1
    if len(argv) >= 5 and argv[1] == 'thumb':
        return thumb(argv[2], argv[3], int(argv[4]))
    if len(argv) >= 3 and argv[1] == 'details':
        return details(argv[2], '--dialog' in argv)
    print(__doc__, file=sys.stderr)
    return 2


if __name__ == '__main__':
    sys.exit(main(sys.argv))
