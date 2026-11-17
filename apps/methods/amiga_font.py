#this belongs in apps/methods/amiga_font.py - Version: 1
# X-Seti - November 17 2026 - IMG Factory 1.6 - Amiga bitmap font to TTF

"""
Amiga bitmap font (.font + size files) to TrueType; one square per set pixel.
"""

##Methods list -
# _glyph
# convert_font_set
# convert_size_file
# read_size_file

import os
import re
import struct

S = 100                       # font units per pixel


def read_size_file(path: str) -> dict: #vers 1
    """Parse one Amiga DiskFont size file (hunk executable) into its font fields."""
    d = open(path, 'rb').read()
    if d[:4] != b'\x00\x00\x03\xf3':
        raise ValueError("Not an Amiga font size file")
    i = 4
    while struct.unpack('>I', d[i:i+4])[0]:                 # resident library names
        i += 4 + 4 * struct.unpack('>I', d[i:i+4])[0]
    i += 4
    nh, first, last = struct.unpack('>III', d[i:i+12])
    i += 12 + 4 * (last - first + 1)
    if struct.unpack('>I', d[i:i+4])[0] & 0x3fffffff != 0x3e9:
        raise ValueError("Font data hunk not found")
    base = i + 8
    data = d[base:]
    if struct.unpack('>H', data[18:20])[0] != 0x0f80:
        raise ValueError("Not a DiskFont file")
    t = 58
    ysize, = struct.unpack('>H', data[t+20:t+22])
    xsize, baseline = struct.unpack('>HH', data[t+24:t+28])
    lo, hi = data[t+32], data[t+33]
    cdata, modulo, cloc, cspace, ckern = struct.unpack('>IHIII', data[t+34:t+52])
    n = hi - lo + 2
    loc = [struct.unpack('>HH', data[cloc+4*k:cloc+4*k+4]) for k in range(n)]
    sp = [struct.unpack('>h', data[cspace+2*k:cspace+2*k+2])[0] for k in range(n)] if cspace else [xsize] * n
    kn = [struct.unpack('>h', data[ckern+2*k:ckern+2*k+2])[0] for k in range(n)] if ckern else [0] * n
    return dict(data=data, ysize=ysize, xsize=xsize, baseline=baseline, lo=lo, hi=hi,
                cdata=cdata, modulo=modulo, loc=loc, sp=sp, kn=kn)


def _glyph(f: dict, i: int): #vers 1
    """TrueType glyph for character slot i."""
    from fontTools.pens.ttGlyphPen import TTGlyphPen
    off, w = f['loc'][i]
    data, cd, mod = f['data'], f['cdata'], f['modulo']
    pen = TTGlyphPen(None)

    def bit(r, x):
        return (data[cd + r * mod + (off + x) // 8] >> (7 - (off + x) % 8)) & 1
    for r in range(f['ysize']):
        x = 0
        while x < w:
            if bit(r, x):
                x2 = x
                while x2 < w and bit(r, x2):
                    x2 += 1
                x0, x1 = (f['kn'][i] + x) * S, (f['kn'][i] + x2) * S
                y1, y0 = (f['baseline'] - r) * S, (f['baseline'] - r - 1) * S
                pen.moveTo((x0, y0))
                pen.lineTo((x0, y1))
                pen.lineTo((x1, y1))
                pen.lineTo((x1, y0))
                pen.closePath()
                x = x2
            else:
                x += 1
    return pen.glyph()


def convert_size_file(path: str, family: str, out: str) -> dict: #vers 1
    """Write one size file as a TTF; returns the font fields."""
    from fontTools.fontBuilder import FontBuilder
    from fontTools.pens.ttGlyphPen import TTGlyphPen
    f = read_size_file(path)
    lo, hi, ys, b = f['lo'], f['hi'], f['ysize'], f['baseline']
    order, glyphs, adv, cmap = ['.notdef'], {'.notdef': _glyph(f, hi - lo + 1)}, {}, {}
    adv['.notdef'] = max(f['sp'][hi - lo + 1], 1) * S
    for c in range(lo, hi + 1):
        k = c - lo
        name = 'uni%04X' % c
        glyphs[name] = _glyph(f, k)
        order.append(name)
        blank = f['loc'][k][1] == 0
        adv[name] = (max(f['sp'][k], f['kn'][k]) if blank else max(f['sp'][k], 0)) * S
        cmap[c] = name
    if 0x20 not in cmap:
        glyphs['space'] = TTGlyphPen(None).glyph()
        order.append('space')
        cmap[0x20] = 'space'
        adv['space'] = f['xsize'] * S
    fb = FontBuilder(ys * S, isTTF=True)
    fb.setupGlyphOrder(order)
    fb.setupCharacterMap(cmap)
    fb.setupGlyf(glyphs)
    fb.setupHorizontalMetrics({g: (adv.get(g, f['xsize'] * S), 0) for g in order})
    fb.setupHorizontalHeader(ascent=b * S, descent=-(ys - b) * S)
    fb.setupNameTable({'familyName': family, 'styleName': 'Regular'})
    fb.setupOS2(sTypoAscender=b * S, sTypoDescender=-(ys - b) * S, usWinAscent=b * S, usWinDescent=(ys - b) * S)
    fb.setupPost()
    fb.save(out)
    return f


def convert_font_set(font_file: str, out_dir: str) -> list: #vers 1
    """Convert every size of name.font (sizes live in the name folder) to Name<size>.ttf."""
    base = os.path.splitext(font_file)[0]
    folder = base if os.path.isdir(base) else next(
        (os.path.join(os.path.dirname(base), n) for n in os.listdir(os.path.dirname(base) or '.')
         if n.lower() == os.path.basename(base).lower() and os.path.isdir(os.path.join(os.path.dirname(base), n))), None)
    if not folder:
        raise ValueError("Size folder not found next to the .font file")
    name = os.path.basename(folder)
    title = re.sub(r'[_\-]+', ' ', name).strip().title()
    made = []
    for size in sorted((n for n in os.listdir(folder) if n.isdigit()), key=int):
        out = os.path.join(out_dir, f"{name.title().replace(' ', '')}{size}.ttf")
        convert_size_file(os.path.join(folder, size), f"{title} {size}", out)
        made.append(out)
    return made
