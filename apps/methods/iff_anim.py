#this belongs in apps/methods/iff_anim.py - Version: 1
# X-Seti - October 27 2026 - IMG Factory 1.6 - IFF ANIM reader / writer.

"""
IFF ANIM (FORM ANIM of FORM ILBM frames). Read: first frame BODY, then
ANHD + DLTA op 0 (full BODY) or op 5 (byte vertical delta, XOR flag,
any interleave). Write: shared palette, ByteRun1 first frame, op 5
deltas against the frame two back (standard double-buffer playback).
Frames are RGBA bytes; delays in milliseconds.
"""

##Methods list -
# _anhd
# _apply_op5
# _chunky
# _encode_column
# _encode_op5
# _planes
# _rgba
# read_iff_anim
# write_iff_anim

import struct

import numpy as np

from apps.methods.iff_ilbm import (BMHD, BODY, CAMG, CMAP, FORM, ILBM, _build_bmhd, _build_chunk, _build_cmap,
                                   _decode_byterun1, _encode_byterun1, iter_chunks)

ANIM, ANHD, DLTA = b'ANIM', b'ANHD', b'DLTA'
JIFFY = 1000 / 60                                        # ANIM times are 1/60 s


def _planes(body: bytes, w: int, h: int, depth: int, masking: int = 0) -> np.ndarray: #vers 1
    """Interleaved BODY -> (depth, h, row_bytes) uint8 plane array."""
    rb = (w + 15) // 16 * 2
    per = depth + (1 if masking == 1 else 0)
    a = np.frombuffer(bytes(body).ljust(rb * per * h, b'\0')[:rb * per * h], np.uint8).reshape(h, per, rb)
    return np.ascontiguousarray(a[:, :depth].transpose(1, 0, 2))


def _chunky(planes: np.ndarray, w: int) -> np.ndarray: #vers 1
    """(depth, h, rb) planes -> (h, w) colour indices."""
    bits = np.unpackbits(planes, axis=2)[:, :, :w].astype(np.uint16)
    return (bits << np.arange(planes.shape[0], dtype=np.uint16)[:, None, None]).sum(0)


def _rgba(idx: np.ndarray, pal: np.ndarray) -> bytes: #vers 1
    out = np.empty(idx.shape + (4,), np.uint8)
    out[..., :3] = pal[np.minimum(idx, len(pal) - 1)]
    out[..., 3] = 255
    return out.tobytes()


def _apply_op5(planes: np.ndarray, dlta: bytes, xor: bool) -> None: #vers 1
    """Byte vertical delta into planes (in place)."""
    depth, h, rb = planes.shape
    ptrs = struct.unpack_from('>16I', dlta, 0)
    for p in range(depth):
        pos = ptrs[p]
        if not pos:
            continue
        col = planes[p]
        for x in range(rb):
            nops = dlta[pos]
            pos += 1
            y = 0
            for _ in range(nops):
                op = dlta[pos]
                pos += 1
                if op == 0:                                  # same: count, value
                    cnt, val = dlta[pos], dlta[pos + 1]
                    pos += 2
                    seg = col[y:y + cnt, x]
                    col[y:y + cnt, x] = seg ^ val if xor else val
                    y += cnt
                elif op & 0x80:                              # uniq: n literal bytes
                    cnt = op & 0x7F
                    vals = np.frombuffer(dlta, np.uint8, cnt, pos)
                    pos += cnt
                    col[y:y + cnt, x] = col[y:y + cnt, x] ^ vals if xor else vals
                    y += cnt
                else:                                        # skip rows
                    y += op


def read_iff_anim(data: bytes): #vers 1
    """(w, h, [rgba frames], [delays ms]). Raises ValueError on unsupported data."""
    if data[:4] != FORM or data[8:12] != ANIM:
        raise ValueError("Not an IFF ANIM file")
    frames, delays, history = [], [], []
    w = h = depth = 0
    pal = None
    for tag, form in iter_chunks(data):
        if tag != FORM or form[:4] != ILBM:
            continue
        ch = {}
        for t, body in iter_chunks(b'FORM' + struct.pack('>I', len(form)) + form):
            ch.setdefault(t, body)
        if BMHD in ch:
            bh = ch[BMHD]
            w, h = struct.unpack_from('>HH', bh, 0)
            depth, masking, comp = bh[8], bh[9], bh[10]
        if CMAP in ch:
            cm = ch[CMAP]
            pal = np.frombuffer(cm[:len(cm) // 3 * 3], np.uint8).reshape(-1, 3)
            camg = struct.unpack('>I', ch[CAMG][:4])[0] if len(ch.get(CAMG, b'')) >= 4 else 0
            if camg & 0x0800:
                raise ValueError("HAM animations are not supported")
            if camg & 0x0080 and len(pal) == 32:             # extra half-brite
                pal = np.concatenate([pal, pal >> 1])
        anhd = ch.get(ANHD)
        op = anhd[0] if anhd else 0
        if op == 0:
            if BODY not in ch:
                raise ValueError("ANIM frame without BODY")
            rb = (w + 15) // 16 * 2
            per = depth + (1 if masking == 1 else 0)
            body = _decode_byterun1(ch[BODY], rb, h * per) if comp == 1 else ch[BODY]
            cur = _planes(body, w, h, depth, masking)
        elif op == 5:
            inter = anhd[18] or 2
            base = history[-inter] if len(history) >= inter else history[0]
            cur = base.copy()
            _apply_op5(cur, ch.get(DLTA, b''), bool(struct.unpack_from('>I', anhd, 20)[0] & 2))
        else:
            raise ValueError(f"ANIM delta method {op} is not supported (0 and 5 are)")
        history.append(cur)
        if pal is None:
            pal = np.array([(i * 255 // ((1 << depth) - 1),) * 3 for i in range(1 << depth)], np.uint8)
        frames.append(_rgba(_chunky(cur, w), pal))
        delays.append(max(1, int(round((struct.unpack_from('>I', anhd, 14)[0] if anhd else 6) * JIFFY))))
    if not frames:
        raise ValueError("ANIM has no frames")
    return w, h, frames, delays


def _encode_column(old: np.ndarray, new: np.ndarray) -> bytes: #vers 1
    """Op 5 ops for one byte column (count byte first)."""
    ops, y, h = [], 0, len(new)
    while y < h:
        if old[y] == new[y]:                                  # skip run
            n = 1
            while y + n < h and n < 127 and old[y + n] == new[y + n]:
                n += 1
            ops.append(bytes([n]))
            y += n
            continue
        n = 1                                                 # same run of one value
        while y + n < h and n < 255 and new[y + n] == new[y] and old[y + n] != new[y + n]:
            n += 1
        if n >= 3:
            ops.append(bytes([0, n, int(new[y])]))
            y += n
            continue
        n = 1                                                 # literal run up to the next skip / same
        while y + n < h and n < 127 and old[y + n] != new[y + n] and not (
                y + n + 2 < h and new[y + n] == new[y + n + 1] == new[y + n + 2]):
            n += 1
        ops.append(bytes([0x80 | n]) + new[y:y + n].tobytes())
        y += n
    if ops and ops[-1][0] and not ops[-1][0] & 0x80 and len(ops[-1]) == 1:
        ops.pop()                                             # trailing skip is implied
    if len(ops) > 255:                                        # too many ops: whole column as literals
        ops = [bytes([0x80 | len(new[k:k + 127])]) + new[k:k + 127].tobytes() for k in range(0, h, 127)]
    return bytes([len(ops)]) + b''.join(ops)


def _encode_op5(old: np.ndarray, new: np.ndarray) -> bytes: #vers 1
    """DLTA chunk body: 16 plane pointers, then per-plane column ops (unchanged planes 0)."""
    ptrs, data = [0] * 16, bytearray()
    for p in range(new.shape[0]):
        if np.array_equal(old[p], new[p]):
            continue
        ptrs[p] = 64 + len(data)
        for x in range(new.shape[2]):
            data += _encode_column(old[p][:, x], new[p][:, x])
    return struct.pack('>16I', *ptrs) + bytes(data)


def _anhd(op: int, w: int, h: int, jiffies: int) -> bytes: #vers 1
    """40-byte ANHD: op, mask, w, h, x, y, abstime, reltime, interleave 0, bits 0."""
    return struct.pack('>BBHHhhIIBBI16x', op, 0, w, h, 0, 0, 0, jiffies, 0, 0, 0)


def write_iff_anim(frames, w: int, h: int, delays, colours: int = 256) -> bytes: #vers 1
    """FORM ANIM from RGBA frames (one shared palette of up to `colours`)."""
    from PIL import Image
    stack = Image.new('RGB', (w, h * len(frames)))
    for k, f in enumerate(frames):
        stack.paste(Image.frombytes('RGBA', (w, h), bytes(f)).convert('RGB'), (0, h * k))
    n = len(set(stack.getdata()))
    q = stack.quantize(colors=min(colours, max(2, n)), method=Image.Quantize.MEDIANCUT, dither=Image.Dither.NONE)
    pal = (q.getpalette() + [0] * 768)[:768]
    depth = max(1, (max(q.getdata()) or 1).bit_length())
    idx = np.asarray(q, np.uint8).reshape(len(frames), h, w)
    rb = (w + 15) // 16 * 2
    planes = []
    for f in idx:
        padded = np.zeros((h, rb * 8), np.uint8)
        padded[:, :w] = f
        planes.append(np.stack([np.packbits((padded >> p) & 1, axis=1) for p in range(depth)]))
    cmap = [tuple(pal[i * 3:i * 3 + 3]) for i in range(1 << depth)]
    jiff = [max(1, int(round(d / JIFFY))) for d in delays]
    rows = np.ascontiguousarray(planes[0].transpose(1, 0, 2)).reshape(-1, rb)   # interleaved plane rows
    body = b''.join(_encode_byterun1(r.tobytes()) for r in rows)                 # ByteRun1 per row
    first = (ILBM + _build_chunk(BMHD, _build_bmhd(w, h, depth, compress=1)) +
             _build_chunk(CMAP, _build_cmap(cmap)) + _build_chunk(CAMG, struct.pack('>I', 0)) +
             _build_chunk(ANHD, _anhd(0, w, h, jiff[0])) + _build_chunk(BODY, body))
    out = _build_chunk(FORM, first)
    for k in range(1, len(planes)):
        base = planes[k - 2] if k >= 2 else planes[0]
        out += _build_chunk(FORM, ILBM + _build_chunk(ANHD, _anhd(5, w, h, jiff[k])) +
                            _build_chunk(DLTA, _encode_op5(base, planes[k])))
    body = ANIM + out
    return FORM + struct.pack('>I', len(body)) + body
