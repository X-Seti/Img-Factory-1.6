#this belongs in apps/methods/txd_dxt_encode.py - Version: 2
# X-Seti - October05 2026 - IMG Factory 1.6 - DXT encoders

"""
DXT1 / DXT3 / DXT5 block encoders (numpy) for TXD, Asset and Radar saves.
"""

##Methods list -
# _blocks
# _colour_blocks
# _encode_dxt1
# _encode_dxt3
# _encode_dxt5

import numpy as np

__all__ = ['_encode_dxt1', '_encode_dxt3', '_encode_dxt5']

_LUM = np.array([0.2126, 0.7152, 0.0722], dtype=np.float32)


def _blocks(rgba_bytes, width, height): #vers 1
    """RGBA bytes -> (n_blocks, 16, 4) uint8, edges repeated to 4x4."""
    a = np.frombuffer(bytes(rgba_bytes[:width * height * 4]), dtype=np.uint8).reshape(height, width, 4)
    ph, pw = (-height) % 4, (-width) % 4
    if ph or pw:
        a = np.pad(a, ((0, ph), (0, pw), (0, 0)), mode='edge')
    by, bx = a.shape[0] // 4, a.shape[1] // 4
    return a.reshape(by, 4, bx, 4, 4).transpose(0, 2, 1, 3, 4).reshape(by * bx, 16, 4)


def _colour_blocks(blk, punch=False): #vers 1
    """8-byte colour blocks; punch=True gives 1-bit alpha (DXT1A)."""
    rgb = blk[:, :, :3].astype(np.int32)
    f = rgb.astype(np.float32)
    cen = f - f.mean(1, keepdims=True)
    cov = np.einsum('nki,nkj->nij', cen, cen)
    axis = np.broadcast_to(_LUM, (len(f), 3)).copy()
    for _ in range(6):                                   # power iteration: principal axis
        axis = np.einsum('nij,nj->ni', cov, axis)
        axis /= np.maximum(np.linalg.norm(axis, axis=1, keepdims=True), 1e-6)
    axis[np.abs(axis).sum(1) < 1e-3] = _LUM
    proj = np.einsum('nki,ni->nk', f, axis)
    clear = (blk[:, :, 3] < 128) if punch else np.zeros(blk.shape[:2], dtype=bool)
    hi = rgb[np.arange(len(rgb)), np.where(clear, -np.inf, proj).argmax(1)]
    lo = rgb[np.arange(len(rgb)), np.where(clear, np.inf, proj).argmin(1)]

    def to565(c):
        return ((c[:, 0] >> 3) << 11) | ((c[:, 1] >> 2) << 5) | (c[:, 2] >> 3)

    c0, c1 = to565(hi), to565(lo)
    three = clear.any(1)                                 # blocks needing 3-colour mode
    swap = np.where(three, c0 > c1, c0 < c1)
    c0, c1 = np.where(swap, c1, c0), np.where(swap, c0, c1)

    def expand(c):
        r, g, b = (c >> 11) & 31, (c >> 5) & 63, c & 31
        return np.stack([(r << 3) | (r >> 2), (g << 2) | (g >> 4), (b << 3) | (b >> 2)], 1)

    p0, p1 = expand(c0), expand(c1)
    four = np.stack([p0, p1, (2 * p0 + p1) // 3, (p0 + 2 * p1) // 3], 1)         # (n,4,3)
    tri = np.stack([p0, p1, (p0 + p1) // 2, np.full_like(p0, 9000)], 1)        # idx 3 = clear
    pal = np.where(three[:, None, None], tri, four)
    d = ((rgb[:, :, None, :] - pal[:, None, :, :]) ** 2).sum(-1)                 # (n,16,4)
    idx = d.argmin(-1).astype(np.uint32)
    idx[(c0 == c1) & ~three] = 0
    idx[clear] = 3
    bits = (idx << (2 * np.arange(16, dtype=np.uint32))).sum(1, dtype=np.uint64).astype(np.uint32)
    out = np.zeros((len(rgb), 8), dtype=np.uint8)
    out[:, 0:2] = c0.astype('<u2').view(np.uint8).reshape(-1, 2)
    out[:, 2:4] = c1.astype('<u2').view(np.uint8).reshape(-1, 2)
    out[:, 4:8] = bits.astype('<u4').view(np.uint8).reshape(-1, 4)
    return out


def _encode_dxt1(rgba_bytes, width, height, alpha=False): #vers 2
    """RGBA8888 bytes -> DXT1 bytes; alpha=True keeps 1-bit alpha."""
    return _colour_blocks(_blocks(rgba_bytes, width, height), alpha).tobytes()


def _encode_dxt3(rgba_bytes, width, height): #vers 1
    """RGBA8888 bytes -> DXT3 bytes (4-bit explicit alpha)."""
    blk = _blocks(rgba_bytes, width, height)
    a4 = (blk[:, :, 3].astype(np.uint64) + 8) // 17
    abits = (a4 << (4 * np.arange(16, dtype=np.uint64))).sum(1, dtype=np.uint64)
    out = np.zeros((len(blk), 16), dtype=np.uint8)
    out[:, 0:8] = abits.astype('<u8').view(np.uint8).reshape(-1, 8)
    out[:, 8:16] = _colour_blocks(blk)
    return out.tobytes()


def _encode_dxt5(rgba_bytes, width, height): #vers 2
    """RGBA8888 bytes -> DXT5 bytes (interpolated alpha)."""
    blk = _blocks(rgba_bytes, width, height)
    a = blk[:, :, 3].astype(np.int32)
    a0, a1 = a.max(1), a.min(1)
    w0 = np.array([7, 0, 6, 5, 4, 3, 2, 1], dtype=np.int32)
    pal = (w0[None, :] * a0[:, None] + (7 - w0[None, :]) * a1[:, None] + 3) // 7    # 8-value mode
    idx = np.abs(a[:, :, None] - pal[:, None, :]).argmin(-1).astype(np.uint64)
    idx[a0 == a1] = 0
    bits = (idx << (3 * np.arange(16, dtype=np.uint64))).sum(1, dtype=np.uint64)
    out = np.zeros((len(blk), 16), dtype=np.uint8)
    out[:, 0] = a0.astype(np.uint8)
    out[:, 1] = a1.astype(np.uint8)
    out[:, 2:8] = bits.astype('<u8').view(np.uint8).reshape(-1, 8)[:, :6]
    out[:, 8:16] = _colour_blocks(blk)
    return out.tobytes()
