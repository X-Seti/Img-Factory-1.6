#!/usr/bin/env python3
#this belongs in apps/methods/txd_lc_android.py - Version: 3
# X-Seti - October05 2026 - IMG Factory 1.6 - War Drum mobile TXD

"""
War Drum mobile GTA III TXD (RW 0x1005FFFF) read and write.
"""

# Layout notes (struct payload offsets):
#   +0 platform id: 12 UNC, 10 PVRTC
#   +4 filter, +8 u addr, +12 v addr (u32 each)
#   +16 16 bytes 0xCC, +32 name[32], +64 mask[32]
#   +96 mip count, 0, alpha flag, 0xCC (u8 each)
#   +100 u16 width, +102 u16 height
#   +104 u32 GL format (0 UNC, 0x8C01 PVRTC2, 0x8C02 PVRTC4)
#   +108 u32 data size, +112 u32 unknown, +116 data
# UNC: RGB565 opaque, RGBA4444 alpha, full mip chain, no sizes.
# PVR: u32 size per mip, then mips (Morton block order).
# Declared size: 132 + sum(4 + w*h*bpp/8), PC origin

import hashlib
import struct

import numpy as np

##Methods list -
# _box_downsample
# _chunk_digest
# _declared_size
# _decode_16bit
# _decode_pvrtc
# _encode_16bit
# _encode_pvrtc
# _expand_bits
# _infer_size_rule
# _mip_dims
# _morton_table
# _parse_native_chunk
# _pvrtc2_modes
# _pvrtc_bilinear
# _pvrtc_block_colors
# _pvrtc_fit
# _pvrtc_modulation
# _pvrtc_pack_colors
# _pvrtc_padded
# _pvrtc_pick
# _tex_mip_rgba
# _walk_native_chunks
# build_lc_android_chunk
# build_lc_android_txd
# detect_lc_android_txd
# parse_lc_android_txd

RW_VERSION_LC_MOBILE = 0x1005FFFF
PLATFORM_UNC = 12
PLATFORM_PVR = 10
GL_PVRTC2_RGB = 0x8C01
GL_PVRTC4_RGBA = 0x8C02
_HDR_LEN = 116
_REP_VALS = np.array([0, 3, 5, 8], dtype=np.int32)


def _expand_bits(v, bits): #vers 1
    """Expand n-bit channel values to 8 bits by replication."""
    v = v.astype(np.uint32)
    if bits == 4:
        return (v * 17).astype(np.uint8)
    if bits == 5:
        return ((v << 3) | (v >> 2)).astype(np.uint8)
    if bits == 6:
        return ((v << 2) | (v >> 4)).astype(np.uint8)
    raise ValueError(f"Unsupported bit width {bits}")


def _mip_dims(width, height): #vers 1
    """Full mip chain dimensions down to 1x1."""
    dims = [(width, height)]
    while width > 1 or height > 1:
        width, height = max(1, width // 2), max(1, height // 2)
        dims.append((width, height))
    return dims


def _box_downsample(rgba): #vers 1
    """Halve an RGBA array with a 2x2 box filter."""
    h, w = rgba.shape[:2]
    a = rgba.astype(np.uint32)
    div = 1
    if h > 1:
        a = a[0::2] + a[1::2]
        div *= 2
    if w > 1:
        a = a[:, 0::2] + a[:, 1::2]
        div *= 2
    return ((a + div // 2) // div).astype(np.uint8)


def _decode_16bit(raw, width, height, fmt): #vers 1
    """Decode RGB565 or RGBA4444 level to RGBA array."""
    v = np.frombuffer(raw, dtype='<u2', count=width * height).reshape(height, width)
    out = np.empty((height, width, 4), dtype=np.uint8)
    if fmt == 'RGB565':
        out[..., 0] = _expand_bits(v >> 11, 5)
        out[..., 1] = _expand_bits((v >> 5) & 63, 6)
        out[..., 2] = _expand_bits(v & 31, 5)
        out[..., 3] = 255
    elif fmt == 'RGBA4444':
        out[..., 0] = _expand_bits(v >> 12, 4)
        out[..., 1] = _expand_bits((v >> 8) & 15, 4)
        out[..., 2] = _expand_bits((v >> 4) & 15, 4)
        out[..., 3] = _expand_bits(v & 15, 4)
    else:
        raise ValueError(f"Unsupported 16-bit format {fmt}")
    return out


def _encode_16bit(rgba, fmt): #vers 1
    """Encode RGBA array to RGB565 or RGBA4444 bytes."""
    c = rgba.astype(np.uint16)
    if fmt == 'RGB565':
        v = ((c[..., 0] >> 3) << 11) | ((c[..., 1] >> 2) << 5) | (c[..., 2] >> 3)
    elif fmt == 'RGBA4444':
        v = ((c[..., 0] >> 4) << 12) | ((c[..., 1] >> 4) << 8) | \
            ((c[..., 2] >> 4) << 4) | (c[..., 3] >> 4)
    else:
        raise ValueError(f"Unsupported 16-bit format {fmt}")
    return v.astype('<u2').tobytes()


def _pvrtc_padded(width, height, bpp): #vers 1
    """Padded size and block grid for a PVRTC level."""
    bw = 8 if bpp == 2 else 4
    pw = max(width, 16 if bpp == 2 else 8)
    ph = max(height, 8)
    return pw, ph, pw // bw, ph // 4


def _morton_table(nbx, nby): #vers 1
    """Block index in twiddled order for each grid cell."""
    ys, xs = np.mgrid[0:nby, 0:nbx]
    mind = min(nbx, nby)
    tw = np.zeros((nby, nbx), dtype=np.int64)
    bit, dst, shift = 1, 1, 0
    while bit < mind:
        tw |= np.where(ys & bit, dst, 0)
        tw |= np.where(xs & bit, dst << 1, 0)
        bit <<= 1
        dst <<= 2
        shift += 1
    big = xs if nbx > nby else ys
    tw |= (big >> shift) << (2 * shift)
    return tw


def _pvrtc_block_colors(col): #vers 1
    """Unpack colour words to A and B (5-bit RGB, 4-bit A)."""
    col = col.astype(np.uint32)
    shp = col.shape + (4,)
    ca = np.empty(shp, dtype=np.int32)
    cb = np.empty(shp, dtype=np.int32)
    lo = col & 0xFFFF
    op_a = (lo & 0x8000) != 0
    ca[..., 0] = np.where(op_a, (lo >> 10) & 0x1F, ((lo >> 7) & 0x1E) | ((lo >> 11) & 1))
    ca[..., 1] = np.where(op_a, (lo >> 5) & 0x1F, ((lo >> 3) & 0x1E) | ((lo >> 7) & 1))
    ca[..., 2] = np.where(op_a, (lo & 0x1E) | ((lo & 0x1E) >> 4), ((lo << 1) & 0x1C) | ((lo >> 2) & 1))
    ca[..., 3] = np.where(op_a, 0xF, (lo >> 11) & 0xE)
    op_b = (col & 0x80000000) != 0
    cb[..., 0] = np.where(op_b, (col >> 26) & 0x1F, ((col >> 23) & 0x1E) | ((col >> 27) & 1))
    cb[..., 1] = np.where(op_b, (col >> 21) & 0x1F, ((col >> 19) & 0x1E) | ((col >> 23) & 1))
    cb[..., 2] = np.where(op_b, (col >> 16) & 0x1F, ((col >> 15) & 0x1E) | ((col >> 19) & 1))
    cb[..., 3] = np.where(op_b, 0xF, ((col >> 27) & 0xE) | 1)
    return ca, cb


def _pvrtc_bilinear(grid, bw, bh): #vers 1
    """Upscale block colour grid like the PVRTC decoder."""
    nby, nbx = grid.shape[:2]
    ph, pw = nby * bh, nbx * bw
    rx = np.arange(pw) - bw // 2
    ry = np.arange(ph) - bh // 2
    x0 = np.floor_divide(rx, bw) % nbx
    y0 = np.floor_divide(ry, bh) % nby
    fx = (np.mod(rx, bw) / bw)[None, :, None]
    fy = (np.mod(ry, bh) / bh)[:, None, None]
    x1 = (x0 + 1) % nbx
    y1 = (y0 + 1) % nby
    g = grid.astype(np.float64)
    p = g[y0][:, x0]
    q = g[y0][:, x1]
    r = g[y1][:, x0]
    s = g[y1][:, x1]
    return (p * (1 - fx) * (1 - fy) + q * fx * (1 - fy)
            + r * (1 - fx) * fy + s * fx * fy)


def _pvrtc_modulation(mod, col, bpp): #vers 1
    """Per-pixel modulation weight (0..8) and punch mask."""
    nby, nbx = mod.shape
    mod = mod.astype(np.uint64)
    flag = (col & 1).astype(bool)
    if bpp == 4:
        sh = (2 * np.arange(16)).astype(np.uint64)
        idx = ((mod[..., None] >> sh) & 3).astype(np.int32).reshape(nby, nbx, 4, 4)
        val = np.where(flag[..., None, None], np.array([0, 4, 4, 8])[idx], _REP_VALS[idx])
        punch = flag[..., None, None] & (idx == 2)
        val = val.transpose(0, 2, 1, 3).reshape(nby * 4, nbx * 4)
        punch = punch.transpose(0, 2, 1, 3).reshape(nby * 4, nbx * 4)
        return val, punch
    sh = np.arange(32).astype(np.uint64)
    direct = (((mod[..., None] >> sh) & 1) * 8).astype(np.int32).reshape(nby, nbx, 4, 8)
    # Checkerboard mode: fix up sub-mode bits first
    m = mod.copy()
    sub = np.ones((nby, nbx), dtype=np.int32)
    odd = (m & 1) != 0
    sub = np.where(odd & ((m >> 20) & 1 != 0), 3, np.where(odd, 2, sub))
    b21 = (m >> 21) & 1
    m = np.where(odd, (m & ~np.uint64(1 << 20)) | (b21 << 20), m)
    m = (m & ~np.uint64(1)) | ((m >> 1) & 1)
    chk = np.zeros((nby, nbx, 4, 8), dtype=np.int32)
    k = 0
    for y in range(4):
        for x in range(8):
            if ((x ^ y) & 1) == 0:
                chk[:, :, y, x] = _REP_VALS[((m >> np.uint64(2 * k)) & 3).astype(np.int32)]
                k += 1
    use = flag[..., None, None]
    full = np.where(use, chk, direct).transpose(0, 2, 1, 3).reshape(nby * 4, nbx * 8)
    subf = np.repeat(np.repeat(np.where(flag, sub, 0), 4, 0), 8, 1)
    yy, xx = np.mgrid[0:nby * 4, 0:nbx * 8]
    hole = (subf > 0) & (((xx ^ yy) & 1) == 1)
    left = np.roll(full, 1, 1)
    right = np.roll(full, -1, 1)
    up = np.roll(full, 1, 0)
    down = np.roll(full, -1, 0)
    avg4 = (left + right + up + down + 2) // 4
    avgh = (left + right + 1) // 2
    avgv = (up + down + 1) // 2
    interp = np.where(subf == 1, avg4, np.where(subf == 2, avgh, avgv))
    val = np.where(hole, interp, full)
    return val, np.zeros_like(val, dtype=bool)


def _decode_pvrtc(raw, width, height, bpp): #vers 1
    """Decode one PVRTC 2bpp or 4bpp level to RGBA array."""
    pw, ph, nbx, nby = _pvrtc_padded(width, height, bpp)
    need = nbx * nby * 8
    if len(raw) < need:
        raise ValueError(f"PVRTC level too short: {len(raw)} < {need}")
    words = np.frombuffer(raw, dtype='<u4', count=nbx * nby * 2).reshape(-1, 2)
    order = _morton_table(nbx, nby)
    mod = words[order, 0]
    col = words[order, 1]
    ca, cb = _pvrtc_block_colors(col)
    bw = 8 if bpp == 2 else 4
    ua = _pvrtc_bilinear(ca, bw, 4)
    ub = _pvrtc_bilinear(cb, bw, 4)
    scale = np.array([255 / 31, 255 / 31, 255 / 31, 255 / 15])
    ua *= scale
    ub *= scale
    val, punch = _pvrtc_modulation(mod, col, bpp)
    w8 = val[..., None] / 8.0
    out = ua * (1 - w8) + ub * w8
    out[..., 3] = np.where(punch, 0, out[..., 3])
    out = np.clip(np.rint(out), 0, 255).astype(np.uint8)
    return out[:height, :width].copy()


def _pvrtc_pack_colors(a, b, opaque): #vers 1
    """Quantise A/B grids (5-bit RGB, 4-bit A) to colour words."""
    a = np.clip(np.rint(a), 0, None)
    b = np.clip(np.rint(b), 0, None)
    ar = np.minimum(a[..., 0], 31).astype(np.uint32)
    ag = np.minimum(a[..., 1], 31).astype(np.uint32)
    ab = np.minimum(a[..., 2], 31).astype(np.uint32)
    br = np.minimum(b[..., 0], 31).astype(np.uint32)
    bg = np.minimum(b[..., 1], 31).astype(np.uint32)
    bb = np.minimum(b[..., 2], 31).astype(np.uint32)
    aa = np.minimum(a[..., 3], 15).astype(np.uint32)
    ba = np.minimum(b[..., 3], 15).astype(np.uint32)
    wa_op = 0x8000 | (ar << 10) | (ag << 5) | ((ab >> 1) << 1)
    wa_tr = ((aa >> 1) << 12) | ((ar >> 1) << 8) | ((ag >> 1) << 4) | ((ab >> 2) << 1)
    wb_op = 0x8000 | (br << 10) | (bg << 5) | bb
    wb_tr = ((ba >> 1) << 12) | ((br >> 1) << 8) | ((bg >> 1) << 4) | (bb >> 1)
    wa = np.where(opaque, wa_op, wa_tr)
    wb = np.where(opaque, wb_op, wb_tr)
    return (wa | (wb << 16)).astype(np.uint32)


def _encode_pvrtc(rgba, bpp): #vers 1
    """Encode RGBA array to one PVRTC level (standard modulation)."""
    height, width = rgba.shape[:2]
    pw, ph, nbx, nby = _pvrtc_padded(width, height, bpp)
    bw = 8 if bpp == 2 else 4
    img = np.tile(rgba, (ph // height, pw // width, 1)).astype(np.float64)
    scale = np.array([31 / 255, 31 / 255, 31 / 255, 15 / 255])
    tgt = img * scale
    blocks = tgt.reshape(nby, 4, nbx, bw, 4).transpose(0, 2, 1, 3, 4).reshape(nby, nbx, -1, 4)
    lum = blocks[..., :3].sum(-1) + blocks[..., 3]
    mean_l = lum.mean(-1, keepdims=True)
    lo_m = (lum <= mean_l)[..., None]
    hi_m = (lum >= mean_l)[..., None]
    a = (blocks * lo_m).sum(2) / np.maximum(lo_m.sum(2), 1)
    b = (blocks * hi_m).sum(2) / np.maximum(hi_m.sum(2), 1)
    opaque = blocks[..., 3].min(-1) >= 14.5
    levels = np.array([0, 3, 5, 8]) if bpp == 4 else np.array([0, 8])
    # Alternate modulation pick and least squares colour fit
    for _ in range(2):
        word = _pvrtc_pack_colors(a, b, opaque)
        qa, qb = _pvrtc_block_colors(word)
        ua = _pvrtc_bilinear(qa, bw, 4)
        ub = _pvrtc_bilinear(qb, bw, 4)
        mi = _pvrtc_pick(ua, ub, tgt, levels)
        wmod = levels[mi] / 8.0
        a, b = _pvrtc_fit(tgt, wmod, nbx, nby, bw, a, b)
    word = _pvrtc_pack_colors(a, b, opaque)
    qa, qb = _pvrtc_block_colors(word)
    ua = _pvrtc_bilinear(qa, bw, 4)
    ub = _pvrtc_bilinear(qb, bw, 4)
    word = word & 0xFFFFFFFE
    if bpp == 4:
        mi = _pvrtc_pick(ua, ub, tgt, levels).astype(np.uint64)
        cell = mi.reshape(nby, 4, nbx, 4).transpose(0, 2, 1, 3).reshape(nby, nbx, 16)
        sh = (2 * np.arange(16)).astype(np.uint64)
        mod = np.bitwise_or.reduce(cell << sh, axis=-1).astype(np.uint32)
    else:
        mod, checker = _pvrtc2_modes(ua, ub, tgt, nbx, nby)
        word = word | checker.astype(np.uint32)
    order = _morton_table(nbx, nby)
    out = np.zeros((nbx * nby, 2), dtype='<u4')
    out[order.ravel(), 0] = mod.ravel()
    out[order.ravel(), 1] = word.ravel()
    return out.tobytes()


def _pvrtc2_modes(ua, ub, tgt, nbx, nby): #vers 1
    """Pick direct or checkerboard 2bpp modulation per block."""
    ph, pw = tgt.shape[:2]
    unit = np.array([255 / 31, 255 / 31, 255 / 31, 255 / 15])
    wt = np.empty(tgt.shape)
    wt[..., :3] = (tgt[..., 3:] / 15 * 0.9 + 0.1)
    wt[..., 3] = 1.0
    wt = (wt * unit) ** 2

    err_of = lambda v: (((ua * (1 - v[..., None] / 8.0) + ub * (v[..., None] / 8.0)
                          - tgt) ** 2) * wt).sum(-1)

    e_lv = np.stack([err_of(np.full((ph, pw), m, dtype=np.float64)) for m in (0, 3, 5, 8)])
    yy, xx = np.mgrid[0:ph, 0:pw]
    stored = ((xx ^ yy) & 1) == 0
    first = ((xx % 8) == 0) & ((yy % 4) == 0)
    # Checker sample 0 holds only 0 or 3
    e_st = e_lv.copy()
    e_st[1:3, first] = np.inf
    idx_st = np.argmin(e_st, 0)
    grid = np.where(stored, _REP_VALS[idx_st], 0)
    avg4 = (np.roll(grid, 1, 1) + np.roll(grid, -1, 1) + np.roll(grid, 1, 0)
            + np.roll(grid, -1, 0) + 2) // 4
    err_c = np.where(stored, np.take_along_axis(e_st, idx_st[None], 0)[0], err_of(avg4))
    bit_d = (e_lv[3] < e_lv[0]).astype(np.uint64)
    err_d = np.minimum(e_lv[0], e_lv[3])

    per_block = lambda a: a.reshape(nby, 4, nbx, 8).transpose(0, 2, 1, 3).reshape(nby, nbx, 32)

    checker = per_block(err_c).sum(-1) < per_block(err_d).sum(-1)
    mod_d = np.bitwise_or.reduce(per_block(bit_d) << np.arange(32).astype(np.uint64), axis=-1)
    cells = per_block(idx_st.astype(np.uint64))
    pos = [y * 8 + x for y in range(4) for x in range(8) if ((x ^ y) & 1) == 0]
    samp = cells[..., pos]
    samp[..., 0] = np.where(samp[..., 0] == 3, 2, 0)
    mod_c = np.bitwise_or.reduce(samp << (2 * np.arange(16)).astype(np.uint64), axis=-1)
    mod = np.where(checker, mod_c, mod_d).astype(np.uint32)
    return mod, checker


def _pvrtc_pick(ua, ub, tgt, levels): #vers 1
    """Best modulation index per pixel, alpha weighted error."""
    unit = np.array([255 / 31, 255 / 31, 255 / 31, 255 / 15])
    wt = np.empty(tgt.shape)
    wt[..., :3] = (tgt[..., 3:] / 15 * 0.9 + 0.1)
    wt[..., 3] = 1.0
    wt = (wt * unit) ** 2
    err = [(((ua * (1 - m / 8) + ub * (m / 8) - tgt) ** 2) * wt).sum(-1) for m in levels]
    return np.argmin(np.stack(err), 0)


def _pvrtc_fit(tgt, wmod, nbx, nby, bw, a, b): #vers 1
    """Least squares refit of block colours A and B."""
    from scipy.sparse import coo_matrix
    from scipy.sparse.linalg import lsqr
    ph, pw = tgt.shape[:2]
    nb = nbx * nby
    rx = np.arange(pw) - bw // 2
    ry = np.arange(ph) - 2
    x0 = np.floor_divide(rx, bw) % nbx
    y0 = np.floor_divide(ry, 4) % nby
    fx = np.mod(rx, bw) / bw
    fy = np.mod(ry, 4) / 4
    yy, xx = np.meshgrid(np.arange(ph), np.arange(pw), indexing='ij')
    pix = (yy * pw + xx).ravel()
    rows, cols, vals = [], [], []
    for dy, wy in ((0, 1 - fy), (1, fy)):
        for dx, wx in ((0, 1 - fx), (1, fx)):
            bi = ((y0[yy] + dy) % nby) * nbx + (x0[xx] + dx) % nbx
            wt = (wy[yy] * wx[xx]).ravel()
            wm = wmod.ravel()
            rows += [pix, pix]
            cols += [bi.ravel(), bi.ravel() + nb]
            vals += [wt * (1 - wm), wt * wm]
    mat = coo_matrix((np.concatenate(vals), (np.concatenate(rows), np.concatenate(cols))),
                     shape=(ph * pw, 2 * nb)).tocsr()
    na = a.copy()
    nbv = b.copy()
    for ch in range(4):
        x0v = np.concatenate([a[..., ch].ravel(), b[..., ch].ravel()])
        rhs = tgt[..., ch].ravel() - mat @ x0v
        sol = lsqr(mat, rhs, damp=0.05, iter_lim=60)[0] + x0v
        top = 31 if ch < 3 else 15
        sol = np.clip(sol, 0, top)
        na[..., ch] = sol[:nb].reshape(nby, nbx)
        nbv[..., ch] = sol[nb:].reshape(nby, nbx)
    return na, nbv


def _declared_size(width, height, bpp, single): #vers 1
    """Native chunk size as PC original would declare it."""
    dims = [(width, height)] if single else _mip_dims(width, height)
    return 132 + sum(4 + w * h * bpp // 8 for w, h in dims)


def _infer_size_rule(declared, width, height): #vers 1
    """Find (bpp, single level) matching a declared chunk size."""
    for bpp in (24, 32, 16, 8):
        for single in (False, True):
            if _declared_size(width, height, bpp, single) == declared:
                return bpp, single
    raise ValueError(f"Declared size {declared} matches no known rule")


def _walk_native_chunks(data): #vers 1
    """Yield (offset, length) of each native texture chunk."""
    if not detect_lc_android_txd(data):
        raise ValueError("Not a War Drum mobile TXD (RW 0x1005FFFF)")
    count = struct.unpack_from('<H', data, 24)[0]
    pos = 28
    spans = []
    for i in range(count):
        ctype = struct.unpack_from('<I', data, pos)[0]
        stype, ssize = struct.unpack_from('<II', data, pos + 12)
        if ctype != 0x15 or stype != 0x01:
            raise ValueError(f"Bad native chunk {i} at 0x{pos:x}")
        ext = pos + 24 + ssize
        etype, esize = struct.unpack_from('<II', data, ext)
        if etype != 0x03:
            raise ValueError(f"Missing extension after texture {i}")
        end = ext + 12 + esize
        if end > len(data):
            raise ValueError(f"Texture {i} runs past end of file")
        spans.append((pos, end - pos))
        pos = end
    return spans


def _chunk_digest(tex): #vers 1
    """Digest of editable texture fields for splice detection."""
    h = hashlib.blake2b(digest_size=16)
    h.update(tex['rgba_data'])
    h.update(repr((tex['name'], tex['alpha_name'], tex['width'], tex['height'],
                   tex['has_alpha'], tex['format'])).encode('latin-1'))
    return h.hexdigest()


def _parse_native_chunk(data, off, length): #vers 1
    """Parse one native texture chunk into a texture dict."""
    ssize = struct.unpack_from('<I', data, off + 16)[0]
    b = off + 24
    pid, fmode, uaddr, vaddr = struct.unpack_from('<4I', data, b)
    name = data[b + 32:b + 64].split(b'\0')[0].decode('latin-1')
    mask = data[b + 64:b + 96].split(b'\0')[0].decode('latin-1')
    flags = struct.unpack_from('<I', data, b + 96)[0]
    mips, alpha = data[b + 96], data[b + 98]
    width, height, glfmt, dsize = struct.unpack_from('<HHII', data, b + 100)
    if ssize != _HDR_LEN + dsize:
        raise ValueError(f"{name}: struct size {ssize} != 116 + {dsize}")
    dims = _mip_dims(width, height)
    if mips != len(dims):
        raise ValueError(f"{name}: mip count {mips} is not a full chain")
    raw = data[b + _HDR_LEN:b + _HDR_LEN + dsize]
    levels = []
    if pid == PLATFORM_UNC:
        if glfmt != 0:
            raise ValueError(f"{name}: UNC with GL format 0x{glfmt:x}")
        fmt = 'RGBA4444' if alpha else 'RGB565'
        depth = 16
        pos = 0
        for lvl, (w, h) in enumerate(dims):
            rgba = _decode_16bit(raw[pos:pos + w * h * 2], w, h, fmt)
            levels.append({'level': lvl, 'width': w, 'height': h, 'rgba_data': rgba.tobytes()})
            pos += w * h * 2
        if pos != dsize:
            raise ValueError(f"{name}: UNC data size {dsize} != {pos}")
    elif pid == PLATFORM_PVR:
        bpp = {GL_PVRTC2_RGB: 2, GL_PVRTC4_RGBA: 4}.get(glfmt)
        if bpp is None:
            raise ValueError(f"{name}: unknown PVR GL format 0x{glfmt:x}")
        fmt = f'PVRTC{bpp}'
        depth = bpp
        sizes = struct.unpack_from(f'<{mips}I', raw, 0)
        pos = 4 * mips
        for lvl, ((w, h), sz) in enumerate(zip(dims, sizes)):
            rgba = _decode_pvrtc(raw[pos:pos + sz], w, h, bpp)
            levels.append({'level': lvl, 'width': w, 'height': h, 'rgba_data': rgba.tobytes()})
            pos += sz
        if pos != dsize:
            raise ValueError(f"{name}: PVR data size {dsize} != {pos}")
    else:
        raise ValueError(f"{name}: unsupported platform id {pid}")
    tex = {
        'name': name, 'alpha_name': mask, 'width': width, 'height': height,
        'depth': depth, 'format': fmt, 'has_alpha': bool(alpha),
        'rgba_data': levels[0]['rgba_data'], 'mipmap_levels': levels,
        'mipmaps': len(levels), 'filter_flags': flags, 'filter_mode': fmode,
        'u_addr': uaddr, 'v_addr': vaddr, 'platform_id': pid,
        'platform': 'MOBILE_LC', 'gl_format': glfmt,
        'rw_version': RW_VERSION_LC_MOBILE,
        '_chunk_off': off, '_chunk_len': length,
    }
    tex['_digest'] = _chunk_digest(tex)
    return tex


def _tex_mip_rgba(tex): #vers 1
    """RGBA arrays for every mip level, rebuilt if top changed."""
    w, h = tex['width'], tex['height']
    top = np.frombuffer(tex['rgba_data'], dtype=np.uint8)
    if top.size != w * h * 4:
        raise ValueError(f"{tex['name']}: rgba_data size {top.size} != {w * h * 4}")
    top = top.reshape(h, w, 4)
    dims = _mip_dims(w, h)
    lv = tex.get('mipmap_levels') or []
    if (len(lv) == len(dims) and lv[0]['rgba_data'] == tex['rgba_data']
            and all((m['width'], m['height']) == d for m, d in zip(lv, dims))):
        return [np.frombuffer(m['rgba_data'], dtype=np.uint8).reshape(d[1], d[0], 4)
                for m, d in zip(lv, dims)]
    out = [top]
    for _ in dims[1:]:
        out.append(_box_downsample(out[-1]))
    return out


def build_lc_android_chunk(tex, variant, version=RW_VERSION_LC_MOBILE, template_chunk=None): #vers 1
    """Encode one texture as a native chunk with extension."""
    w, h = tex['width'], tex['height']
    if w < 1 or h < 1 or w & (w - 1) or h & (h - 1):
        raise ValueError(f"{tex['name']}: size {w}x{h} must be power of two")
    alpha = bool(tex['has_alpha'])
    levels = _tex_mip_rgba(tex)
    dims = _mip_dims(w, h)
    if variant == PLATFORM_UNC:
        fmt = 'RGBA4444' if alpha else 'RGB565'
        glfmt = 0
        payload = b''.join(_encode_16bit(lv, fmt) for lv in levels)
    elif variant == PLATFORM_PVR:
        fmt = tex.get('format') if tex.get('format') in ('PVRTC2', 'PVRTC4') else ('PVRTC4' if alpha else 'PVRTC2')
        bpp = int(fmt[-1])
        glfmt = GL_PVRTC4_RGBA if bpp == 4 else GL_PVRTC2_RGB
        parts = [_encode_pvrtc(lv, bpp) for lv in levels]
        payload = struct.pack(f'<{len(parts)}I', *[len(p) for p in parts]) + b''.join(parts)
    else:
        raise ValueError(f"Unsupported variant {variant}")
    # Header fields come from template when supplied
    unknown = 0
    ext = struct.pack('<III', 3, 0, version)
    if template_chunk is not None:
        t = template_chunk
        tw, th = struct.unpack_from('<HH', t, 124)
        bpp_rule, single = _infer_size_rule(struct.unpack_from('<I', t, 4)[0], tw, th)
        unknown = struct.unpack_from('<I', t, 136)[0]
        ext = t[24 + struct.unpack_from('<I', t, 16)[0]:]
    else:
        bpp_rule, single = (32 if alpha else 24), False
    hdr = bytearray(_HDR_LEN)
    struct.pack_into('<4I', hdr, 0, variant, tex.get('filter_mode', 6),
                     tex.get('u_addr', 1), tex.get('v_addr', 1))
    hdr[16:32] = b'\xcc' * 16
    # Names are NUL terminated then 0xCC filled
    hdr[32:64] = (tex['name'].encode('latin-1')[:31] + b'\0').ljust(32, b'\xcc')
    hdr[64:96] = ((tex.get('alpha_name') or '').encode('latin-1')[:31] + b'\0').ljust(32, b'\xcc')
    hdr[96:100] = bytes([len(dims), 0, 1 if alpha else 0, 0xCC])
    struct.pack_into('<HHIII', hdr, 100, w, h, glfmt, len(payload), unknown)
    body = bytes(hdr) + payload
    declared = _declared_size(w, h, bpp_rule, single)
    return (struct.pack('<III', 0x15, declared, version)
            + struct.pack('<III', 1, len(body), version) + body + ext)


def build_lc_android_txd(textures, variant, original=None): #vers 1
    """Build whole TXD; unchanged textures splice original chunks."""
    version, device = RW_VERSION_LC_MOBILE, 0
    if original is not None:
        if not detect_lc_android_txd(original):
            raise ValueError("Original is not a War Drum mobile TXD")
        version = struct.unpack_from('<I', original, 8)[0]
        device = struct.unpack_from('<H', original, 26)[0]
    chunks = []
    for tex in textures:
        tmpl = None
        if original is not None and '_chunk_off' in tex:
            off, ln = tex['_chunk_off'], tex['_chunk_len']
            tmpl = original[off:off + ln]
            if (tex.get('platform_id') == variant and tex.get('_digest') == _chunk_digest(tex)
                    and all(tex.get(k) == v for k, v in zip(
                        ('filter_mode', 'u_addr', 'v_addr'),
                        struct.unpack_from('<3I', tmpl, 28)))):
                chunks.append(bytes(tmpl))
                continue
        chunks.append(build_lc_android_chunk(tex, variant, version, tmpl))
    total = 16 + sum(12 + struct.unpack_from('<I', c, 4)[0] for c in chunks) + 12
    head = struct.pack('<III', 0x16, total, version)
    head += struct.pack('<III', 1, 4, version) + struct.pack('<HH', len(chunks), device)
    return head + b''.join(chunks) + struct.pack('<III', 3, 0, version)


def detect_lc_android_txd(data): #vers 3
    """True for War Drum mobile TXDs: RW 0x1005FFFF, platform 10/12."""
    if len(data) < 28:
        return False
    ctype, _, ver = struct.unpack_from('<III', data, 0)
    if ctype != 0x16 or ver != RW_VERSION_LC_MOBILE:
        return False
    off = 24 + struct.unpack_from('<I', data, 16)[0]      # first native
    if off + 28 > len(data):
        return False
    return struct.unpack_from('<I', data, off + 24)[0] in (10, 12)


def parse_lc_android_txd(data): #vers 2
    """Parse every texture with decoded RGBA and mip levels."""
    return [_parse_native_chunk(data, off, ln) for off, ln in _walk_native_chunks(data)]


__all__ = ['detect_lc_android_txd', 'parse_lc_android_txd',
           'build_lc_android_chunk', 'build_lc_android_txd']
