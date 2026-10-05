#this belongs in apps/methods/mobile_texture_decode.py - Version: 2
# X-Seti - October05 2026 - IMG Factory 1.6 - Mobile Texture Pixel Codecs

"""
Pixel decoders and encoders for SA/VC mobile texture databases.
"""

# Encoding ids are GL constants (type or compressed format).
# PVRTC codec lives in txd_lc_android.py, DXT encode in txd_dxt_encode.py.

import numpy as np

##Methods list -
# _dxt_colour
# _expand
# decode_dxt
# decode_etc1
# decode_level
# decode_mobile_texture
# encode_etc1
# encode_level
# level_size
# mip_dims
# to_pil_image

GL_RGBA8888 = 0x1401
GL_L8 = 0x1909
GL_RGBA4444 = 0x8033
GL_RGBA5551 = 0x8034
GL_RGB565 = 0x8363
GL_DXT1 = 0x83F0
GL_DXT1A = 0x83F1
GL_DXT3 = 0x83F2
GL_DXT5 = 0x83F3
GL_PVRTC4_RGB = 0x8C00
GL_PVRTC2_RGB = 0x8C01
GL_PVRTC4_RGBA = 0x8C02
GL_PVRTC2_RGBA = 0x8C03
GL_ETC1 = 0x8D64

_PVRTC_BPP = {GL_PVRTC4_RGB: 4, GL_PVRTC2_RGB: 2, GL_PVRTC4_RGBA: 4, GL_PVRTC2_RGBA: 2}
_BLOCK8 = (GL_DXT1, GL_DXT1A, GL_ETC1)
_BLOCK16 = (GL_DXT3, GL_DXT5)
_RAW_BPP = {GL_RGBA8888: 4, GL_L8: 1, GL_RGBA4444: 2, GL_RGBA5551: 2, GL_RGB565: 2}
_ETC_TABLE = np.array([[2, 8], [5, 17], [9, 29], [13, 42],
                       [18, 60], [24, 80], [33, 106], [47, 183]], dtype=np.int32)


def _expand(v, bits): #vers 1
    """Expand n-bit values to 8 bits by bit replication."""
    v = v.astype(np.int32)
    if bits == 1:
        return (v * 255).astype(np.uint8)
    return ((v << (8 - bits)) | (v >> (2 * bits - 8))).astype(np.uint8)


def mip_dims(width, height, mips=True): #vers 1
    """Mip level sizes, full chain to 1x1 when mips set."""
    dims = [(width, height)]
    while mips and (width > 1 or height > 1):
        width, height = max(1, width // 2), max(1, height // 2)
        dims.append((width, height))
    return dims


def level_size(enc, width, height): #vers 1
    """Byte size of one stored level for an encoding."""
    if enc in _RAW_BPP:
        return width * height * _RAW_BPP[enc]
    if enc in _BLOCK8 or enc in _BLOCK16:
        return ((width + 3) // 4) * ((height + 3) // 4) * (8 if enc in _BLOCK8 else 16)
    if enc in _PVRTC_BPP:
        bpp = _PVRTC_BPP[enc]
        return max(width, 16 if bpp == 2 else 8) * max(height, 8) * bpp // 8
    raise ValueError(f"Unknown mobile texture encoding 0x{enc:04X}")


def _dxt_colour(blk): #vers 1
    """Decode 8-byte colour blocks to (n,16,3) and 3-colour flag."""
    c = blk[:, 0:4].copy().view('<u2')
    c0, c1 = c[:, 0].astype(np.int32), c[:, 1].astype(np.int32)
    bits = blk[:, 4:8].copy().view('<u4')[:, 0].astype(np.uint64)
    rgb = lambda v: np.stack([_expand(v >> 11, 5), _expand((v >> 5) & 63, 6),
                              _expand(v & 31, 5)], -1).astype(np.int32)
    p0, p1 = rgb(c0), rgb(c1)
    four = (c0 > c1)[:, None]
    p2 = np.where(four, (2 * p0 + p1) // 3, (p0 + p1) // 2)
    p3 = np.where(four, (p0 + 2 * p1) // 3, 0)
    pal = np.stack([p0, p1, p2, p3], 1)
    idx = ((bits[:, None] >> (2 * np.arange(16, dtype=np.uint64))) & 3).astype(np.int64)
    cols = np.take_along_axis(pal, idx[..., None], 1)
    return cols, idx, ~four[:, 0]


def decode_dxt(data, width, height, enc): #vers 1
    """Decode DXT1/1A/3/5 level to RGBA array."""
    bw, bh = (width + 3) // 4, (height + 3) // 4
    bs = 8 if enc in _BLOCK8 else 16
    blk = np.frombuffer(data, dtype=np.uint8, count=bw * bh * bs).reshape(-1, bs)
    out = np.empty((bw * bh, 16, 4), dtype=np.uint8)
    cols, idx, three = _dxt_colour(blk[:, bs - 8:])
    out[..., :3] = cols
    if enc == GL_DXT3:
        a = blk[:, 0:8].copy().view('<u8')[:, 0]
        out[..., 3] = _expand(((a[:, None] >> (4 * np.arange(16, dtype=np.uint64))) & 15), 4)
    elif enc == GL_DXT5:
        a0, a1 = blk[:, 0].astype(np.int32), blk[:, 1].astype(np.int32)
        w0 = np.array([7, 0, 6, 5, 4, 3, 2, 1])
        w1 = 7 - w0
        pal8 = (w0[None] * a0[:, None] + w1[None] * a1[:, None]) // 7
        v0 = np.array([5, 0, 4, 3, 2, 1, 0, 0])
        v1 = np.array([0, 5, 1, 2, 3, 4, 0, 0])
        pal6 = (v0[None] * a0[:, None] + v1[None] * a1[:, None]) // 5
        pal6[:, 6], pal6[:, 7] = 0, 255
        pal = np.where((a0 > a1)[:, None], pal8, pal6)
        bits = np.zeros(len(blk), dtype=np.uint64)
        for i in range(6):
            bits |= blk[:, 2 + i].astype(np.uint64) << np.uint64(8 * i)
        ai = ((bits[:, None] >> (3 * np.arange(16, dtype=np.uint64))) & 7).astype(np.int64)
        out[..., 3] = np.take_along_axis(pal, ai, 1)
    elif enc == GL_DXT1A:
        out[..., 3] = np.where(three[:, None] & (idx == 3), 0, 255)
    else:
        out[..., 3] = 255
    img = out.reshape(bh, bw, 4, 4, 4).transpose(0, 2, 1, 3, 4).reshape(bh * 4, bw * 4, 4)
    return img[:height, :width]


def decode_etc1(data, width, height): #vers 2
    """Decode ETC1 level to RGBA array."""
    bw, bh = (width + 3) // 4, (height + 3) // 4
    blk = np.frombuffer(data, dtype=np.uint8, count=bw * bh * 8).reshape(-1, 8).astype(np.int32)
    diff = (blk[:, 3] >> 1) & 1
    flip = blk[:, 3] & 1
    base = np.empty((len(blk), 2, 3), dtype=np.int32)
    for ch in range(3):
        b = blk[:, ch]
        c1 = b >> 3
        d = (b & 7) - ((b & 4) << 1)
        dif1 = _expand(c1, 5).astype(np.int32)
        dif2 = _expand(np.clip(c1 + d, 0, 31), 5).astype(np.int32)
        ind1 = (b >> 4) * 17
        ind2 = (b & 15) * 17
        base[:, 0, ch] = np.where(diff == 1, dif1, ind1)
        base[:, 1, ch] = np.where(diff == 1, dif2, ind2)
    tab = np.stack([_ETC_TABLE[blk[:, 3] >> 5], _ETC_TABLE[(blk[:, 3] >> 2) & 7]], 1)
    msb = (blk[:, 4] << 8) | blk[:, 5]
    lsb = (blk[:, 6] << 8) | blk[:, 7]
    i = np.arange(16)
    x, y = i // 4, i % 4
    sel = ((msb[:, None] >> i) & 1) * 2 + ((lsb[:, None] >> i) & 1)
    sub = np.where(flip[:, None] == 1, (y >= 2)[None], (x >= 2)[None]).astype(np.int64)
    t = np.take_along_axis(tab, sub[..., None], 1)
    mag = np.where(sel % 2 == 1, t[..., 1], t[..., 0])
    mod = np.where(sel >= 2, -mag, mag)
    col = np.take_along_axis(base, sub[..., None], 1) + mod[..., None]
    px = np.empty((len(blk), 4, 4, 4), dtype=np.uint8)
    px[:, y, x, :3] = np.clip(col, 0, 255)
    px[..., 3] = 255
    img = px.reshape(bh, bw, 4, 4, 4).transpose(0, 2, 1, 3, 4).reshape(bh * 4, bw * 4, 4)
    return img[:height, :width]


def encode_etc1(rgba, width, height): #vers 1
    """Encode RGBA array to ETC1 level bytes."""
    a = np.asarray(rgba, dtype=np.uint8).reshape(height, width, 4)
    ph, pw = (-height) % 4, (-width) % 4
    if ph or pw:
        a = np.pad(a, ((0, ph), (0, pw), (0, 0)), mode='edge')
    bh, bw = a.shape[0] // 4, a.shape[1] // 4
    px = a.reshape(bh, 4, bw, 4, 4).transpose(0, 2, 3, 1, 4).reshape(-1, 16, 3 + 1)[..., :3].astype(np.int32)
    n = len(px)
    i = np.arange(16)
    x, y = i // 4, i % 4
    best_err = np.full(n, np.inf)
    best = np.zeros((n, 8), dtype=np.uint8)
    for flip in (0, 1):
        sub = (y >= 2) if flip else (x >= 2)
        avg = np.stack([px[:, ~sub].mean(1), px[:, sub].mean(1)], 1)
        for diff in (0, 1):
            if diff:
                q = np.clip(np.rint(avg * 31 / 255), 0, 31).astype(np.int32)
                d = q[:, 1] - q[:, 0]
                ok = np.all((d >= -4) & (d <= 3), 1)
                q[:, 1] = q[:, 0] + np.clip(d, -4, 3)
                cb = _expand(q, 5).astype(np.int32)
            else:
                q = np.clip(np.rint(avg * 15 / 255), 0, 15).astype(np.int32)
                ok = np.ones(n, dtype=bool)
                cb = q * 17
            err_tot = np.zeros(n)
            tsel = np.zeros((n, 2), dtype=np.int32)
            sels = np.zeros((n, 16), dtype=np.int32)
            for s in (0, 1):
                mask = sub if s else ~sub
                p = px[:, mask]
                cand = cb[:, s][:, None, None, :] + np.stack(
                    [_ETC_TABLE[:, 0], _ETC_TABLE[:, 1], -_ETC_TABLE[:, 0], -_ETC_TABLE[:, 1]], 1)[None, :, :, None]
                cand = np.clip(cand, 0, 255)
                e = ((p[:, None, None, :, :] - cand[:, :, :, None, :]) ** 2).sum(-1)
                emin = e.min(2)
                tot = emin.sum(-1)
                tb = tot.argmin(1)
                tsel[:, s] = tb
                err_tot += tot[np.arange(n), tb]
                sels[:, mask] = e[np.arange(n), tb].argmin(1)
            err_tot[~ok] = np.inf
            better = err_tot < best_err
            if not better.any():
                continue
            best_err = np.where(better, err_tot, best_err)
            blk = np.zeros((n, 8), dtype=np.int32)
            if diff:
                dd = (q[:, 1] - q[:, 0]) & 7
                blk[:, 0:3] = (q[:, 0] << 3) | dd
            else:
                blk[:, 0:3] = (q[:, 0] << 4) | q[:, 1]
            blk[:, 3] = (tsel[:, 0] << 5) | (tsel[:, 1] << 2) | (diff << 1) | flip
            # Selector index 0..3 maps to (+a,+b,-a,-b)
            msb = ((sels >= 2).astype(np.int64) << i).sum(1)
            lsb = ((sels % 2).astype(np.int64) << i).sum(1)
            blk[:, 4], blk[:, 5] = msb >> 8, msb & 255
            blk[:, 6], blk[:, 7] = lsb >> 8, lsb & 255
            best[better] = blk[better].astype(np.uint8)
    return best.tobytes()


def decode_level(enc, data, width, height): #vers 1
    """Decode one stored level to an RGBA uint8 array."""
    size = level_size(enc, width, height)
    if len(data) < size:
        raise ValueError(f"Level data short: {len(data)} < {size}")
    if enc in _PVRTC_BPP:
        from apps.methods.txd_lc_android import _decode_pvrtc
        return _decode_pvrtc(bytes(data[:size]), width, height, _PVRTC_BPP[enc])
    if enc == GL_ETC1:
        return decode_etc1(data, width, height)
    if enc in _BLOCK8 or enc in _BLOCK16:
        return decode_dxt(data, width, height, enc)
    v = np.frombuffer(data, dtype=np.uint8, count=size)
    if enc == GL_RGBA8888:
        return v.reshape(height, width, 4).copy()
    out = np.empty((height, width, 4), dtype=np.uint8)
    if enc == GL_L8:
        out[..., :3] = v.reshape(height, width, 1)
        out[..., 3] = 255
        return out
    s = v.view('<u2').reshape(height, width).astype(np.int32)
    if enc == GL_RGB565:
        parts = [(s >> 11, 5), ((s >> 5) & 63, 6), (s & 31, 5)]
        out[..., 3] = 255
    elif enc == GL_RGBA4444:
        parts = [(s >> 12, 4), ((s >> 8) & 15, 4), ((s >> 4) & 15, 4), (s & 15, 4)]
    else:
        parts = [(s >> 11, 5), ((s >> 6) & 31, 5), ((s >> 1) & 31, 5), (s & 1, 1)]
    for ch, (val, bits) in enumerate(parts):
        out[..., ch] = _expand(val, bits)
    return out


def encode_level(enc, rgba, width, height): #vers 1
    """Encode an RGBA array to one stored level."""
    a = np.ascontiguousarray(np.asarray(rgba, dtype=np.uint8).reshape(height, width, 4))
    if enc in _PVRTC_BPP:
        from apps.methods.txd_lc_android import _encode_pvrtc
        return _encode_pvrtc(a, _PVRTC_BPP[enc])
    if enc == GL_ETC1:
        return encode_etc1(a, width, height)
    if enc in _BLOCK8 or enc in _BLOCK16:
        from apps.methods.txd_dxt_encode import _encode_dxt1, _encode_dxt3, _encode_dxt5
        raw = a.tobytes()
        if enc == GL_DXT1:
            return _encode_dxt1(raw, width, height, False)
        if enc == GL_DXT1A:
            return _encode_dxt1(raw, width, height, True)
        return (_encode_dxt3 if enc == GL_DXT3 else _encode_dxt5)(raw, width, height)
    if enc == GL_RGBA8888:
        return a.tobytes()
    c = a.astype(np.uint32)
    if enc == GL_L8:
        return ((c[..., 0] + c[..., 1] + c[..., 2] + 1) // 3).astype(np.uint8).tobytes()
    if enc == GL_RGB565:
        v = ((c[..., 0] >> 3) << 11) | ((c[..., 1] >> 2) << 5) | (c[..., 2] >> 3)
    elif enc == GL_RGBA4444:
        v = ((c[..., 0] >> 4) << 12) | ((c[..., 1] >> 4) << 8) | ((c[..., 2] >> 4) << 4) | (c[..., 3] >> 4)
    elif enc == GL_RGBA5551:
        v = ((c[..., 0] >> 3) << 11) | ((c[..., 1] >> 3) << 6) | ((c[..., 2] >> 3) << 1) | (c[..., 3] >> 7)
    else:
        raise ValueError(f"Unknown mobile texture encoding 0x{enc:04X}")
    return v.astype('<u2').tobytes()


def decode_mobile_texture(tex): #vers 2
    """Decode a MobileTexture top level to RGBA bytes."""
    if tex.is_affiliate or not tex.pixel_data:
        return None
    return decode_level(tex.encoding_type, tex.pixel_data, tex.width, tex.height).tobytes()


def to_pil_image(tex): #vers 2
    """Convert a MobileTexture to a PIL RGBA image."""
    from PIL import Image
    rgba = decode_mobile_texture(tex)
    if rgba is None:
        return None
    return Image.frombytes('RGBA', (tex.width, tex.height), rgba)
