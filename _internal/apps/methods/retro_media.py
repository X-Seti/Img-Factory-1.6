#this belongs in apps/methods/retro_media.py - Version: 2
# X-Seti - October 24 2026 - IMG Factory 1.6 - top-down GTA sounds, images and videos

"""
Media of the top-down GTAs.
PC sound banks: .sdt index + .raw samples - GTA1 12-byte entries (offset,
size, rate) of 8-bit unsigned mono, GTA2 24-byte entries (offset, size,
rate, loop data) of 16-bit mono.
PS1 GTA1 / London AUDIO.VSR: <name>_sbi (u16 SPU address / 8, size / 8,
rate per sample; bank loaded at SPU 0x1000) + <name>_sbk PS-ADPCM bank.
PS1 .vsr image folders (INDEX + pages + CLUTs): fonts, front end, loading.
PS1 .str video (2336-byte Mode 2 sectors): made into 2352-byte sectors and
converted by ffmpeg (psxstr) to MP4 with its XA audio.
Import: a WAV replaces a PC sample (8 / 16-bit at the WAV's rate) or a PS1
sample (encoded to PS-ADPCM, bank repacked); a PNG replaces a PS1 image
of the same size (nearest CLUT colours).
"""

##Methods list -
# bank_replace
# bank_samples
# bank_wav
# media_in_folder
# pcm_wav
# psx_adpcm
# sdt_replace
# sdt_samples
# sdt_wav
# str_to_mp4
# vsr_banks
# vsr_image_folders
# vsr_image_set
# wav_pcm16

import io
import os

import numpy as np
import struct
import subprocess
import tempfile
import wave
from typing import List


from apps.methods.retro_palette import ps1_rgba, put_4bit, quantize
from apps.methods.vsr_archive import FOLDER, VSRArchive

PSX_FILTERS = ((0, 0), (60, 0), (115, -52), (98, -55), (122, -60))

SPU_BASE = 0x1000                        # SPU RAM address of a loaded bank


def pcm_wav(pcm: bytes, rate: int, width: int) -> bytes: #vers 1
    """Mono WAV bytes of raw PCM (width 1 = 8-bit unsigned, 2 = 16-bit)."""
    out = io.BytesIO()
    with wave.open(out, 'wb') as w:
        w.setnchannels(1)
        w.setsampwidth(width)
        w.setframerate(rate)
        w.writeframes(pcm)
    return out.getvalue()


def sdt_samples(sdt: bytes) -> List[dict]: #vers 1
    """Entries of a GTA1 (12-byte) or GTA2 (24-byte) .sdt."""
    def rates_ok(step):
        return len(sdt) % step == 0 and all(4000 <= struct.unpack_from('<I', sdt, i + 8)[0] <= 48000
                                            for i in range(0, len(sdt), step))
    step, width = (24, 2) if rates_ok(24) else (12, 1)
    out = []
    for i in range(0, len(sdt) - step + 1, step):
        off, size, rate = struct.unpack_from('<3I', sdt, i)
        out.append({'n': len(out), 'offset': off, 'size': size, 'rate': rate, 'width': width})
    return out


def sdt_wav(raw: bytes, e: dict) -> bytes: #vers 1
    pcm = raw[e['offset']:e['offset'] + e['size']]
    return pcm_wav(pcm[:len(pcm) // e['width'] * e['width']], e['rate'], e['width'])


def vsr_banks(arch: VSRArchive) -> List[str]: #vers 1
    """Names of the _sbi / _sbk sound bank pairs of a .vsr."""
    names = {e['name'] for e in arch.entries if e['kind'] != FOLDER}
    return sorted(n[:-4] for n in names if n.endswith('_sbk') and n[:-4] + '_sbi' in names)


def bank_samples(arch: VSRArchive, bank: str) -> List[dict]: #vers 1
    info = arch.blob('', bank + '_sbi')
    out = []
    for i in range(len(info) // 6):
        addr, size, rate = struct.unpack_from('<3H', info, i * 6)
        if size and rate:
            out.append({'n': i, 'offset': addr * 8 - SPU_BASE, 'size': size * 8, 'rate': rate})
    return out


def bank_wav(arch: VSRArchive, bank: str, e: dict) -> bytes: #vers 1
    """WAV of one PS-ADPCM sample (decoded through ffmpeg's VAG reader)."""
    from apps.methods.audioparser import _decode_mono_channel
    adpcm = arch.blob('', bank + '_sbk')[e['offset']:e['offset'] + e['size']]
    return pcm_wav(_decode_mono_channel(adpcm, e['rate']), e['rate'], 2)


def vsr_image_folders(arch: VSRArchive) -> List[str]: #vers 1
    """Folders holding an INDEX, texture pages and CLUTs (not city tiles / sprites)."""
    return [e['folder'] for e in arch.entries if e['kind'] == 0x1A and e['name'] == 'INDEX'
            and e['folder'] not in ('TILE_GFX', 'SPRITE_GFX')]


def str_to_mp4(str_path: str, out_path: str) -> str: #vers 1
    """PS1 .str (2336 or 2352-byte sectors) to MP4 with ffmpeg; returns out_path."""
    with open(str_path, 'rb') as f:
        d = f.read()
    sync = b'\x00' + b'\xff' * 10 + b'\x00'
    if d[:12] != sync:
        if len(d) % 2336:
            raise ValueError("Not a PS1 .str (2336 / 2352-byte sectors)")
        out = bytearray()
        for i in range(len(d) // 2336):
            lba = i + 150
            m, s, fr = lba // 4500, (lba // 75) % 60, lba % 75
            out += sync + bytes([(m // 10) << 4 | m % 10, (s // 10) << 4 | s % 10, (fr // 10) << 4 | fr % 10, 2])
            out += d[i * 2336:(i + 1) * 2336]
        d = bytes(out)
    tmp = tempfile.NamedTemporaryFile(suffix='.str', delete=False)
    try:
        tmp.write(d)
        tmp.close()
        r = subprocess.run(['ffmpeg', '-v', 'error', '-y', '-f', 'psxstr', '-i', tmp.name, out_path],
                           capture_output=True, text=True, timeout=600)
        if r.returncode != 0:
            raise RuntimeError("ffmpeg: " + (r.stderr.strip().splitlines() or ['failed'])[-1])
    except FileNotFoundError:
        raise RuntimeError("ffmpeg is needed to convert PS1 .str video")
    finally:
        os.unlink(tmp.name)
    return out_path


def media_in_folder(folder: str) -> List[dict]: #vers 1
    """Sound banks, image .vsr files, .str videos and .wav music under a game folder (2 levels)."""
    out = []
    for root, dirs, files in os.walk(folder):
        if root[len(folder):].count(os.sep) >= 2:
            dirs[:] = []
        low = {f.lower(): f for f in files}
        for lf, f in sorted(low.items()):
            p = os.path.join(root, f)
            if lf.endswith('.sdt') and lf[:-4] + '.raw' in low:
                out.append({'kind': 'sdt', 'name': f, 'path': p, 'raw': os.path.join(root, low[lf[:-4] + '.raw'])})
            elif lf.endswith('.str'):
                out.append({'kind': 'str', 'name': f, 'path': p})
            elif lf.endswith('.wav'):
                out.append({'kind': 'wav', 'name': os.path.relpath(p, folder), 'path': p})
            elif lf.endswith('.vsr') and os.path.getsize(p) < 4 << 20:
                try:
                    with open(p, 'rb') as fh:
                        arch = VSRArchive.from_bytes(fh.read(), p)
                except (ValueError, KeyError, struct.error):
                    continue
                if arch.entry('CMP', 'BASE') is not None:
                    continue
                if vsr_banks(arch):
                    out.append({'kind': 'bank', 'name': f, 'path': p})
                if vsr_image_folders(arch):
                    out.append({'kind': 'images', 'name': f, 'path': p})
    return out


def wav_pcm16(wav: bytes) -> tuple: #vers 1
    """(mono int16 samples, rate) of a PCM WAV (8 / 16-bit, mono or stereo)."""
    with wave.open(io.BytesIO(wav), 'rb') as w:
        ch, width, rate = w.getnchannels(), w.getsampwidth(), w.getframerate()
        raw = w.readframes(w.getnframes())
    if width == 1:
        a = (np.frombuffer(raw, np.uint8).astype(np.int16) - 128) << 8
    elif width == 2:
        a = np.frombuffer(raw, '<i2').astype(np.int16)
    else:
        raise ValueError("WAV must be 8 or 16-bit PCM")
    if ch > 1:
        a = a.reshape(-1, ch).mean(1).astype(np.int16)
    return a, rate


def sdt_replace(sdt: bytes, raw: bytes, n: int, wav: bytes) -> tuple: #vers 1
    """(sdt, raw) with sample n replaced by a WAV; later samples move up / down."""
    ents = sdt_samples(sdt)
    pcm, rate = wav_pcm16(wav)
    e = ents[n]
    new = (((pcm >> 8) + 128).astype(np.uint8).tobytes() if e['width'] == 1 else pcm.astype('<i2').tobytes())
    step = 24 if e['width'] == 2 else 12
    out_sdt, out_raw = bytearray(sdt), bytearray()
    for x in ents:
        data = new if x['n'] == n else raw[x['offset']:x['offset'] + x['size']]
        struct.pack_into('<3I', out_sdt, x['n'] * step, len(out_raw), len(data), rate if x['n'] == n else x['rate'])
        out_raw += data
    return bytes(out_sdt), bytes(out_raw)


def psx_adpcm(pcm: np.ndarray) -> bytes: #vers 1
    """Mono int16 samples to PS-ADPCM blocks (16 bytes per 28 samples; last block has the end flag)."""
    pcm = np.concatenate([pcm.astype(np.int32), np.zeros((-len(pcm)) % 28, np.int32)])
    out = bytearray()
    s1 = s2 = 0
    nblk = len(pcm) // 28
    for b in range(nblk):
        blk = pcm[b * 28:(b + 1) * 28]
        best = None
        for fi, (f0, f1) in enumerate(PSX_FILTERS):
            for shift in range(13):
                p1, p2, err, nib = s1, s2, 0, []
                for v in blk:
                    pred = (p1 * f0 + p2 * f1 + 32) >> 6
                    d = int(v) - pred
                    q = max(-8, min(7, (d + (1 << (shift - 1) if shift else 0)) >> shift if d >= 0 else
                                -((-d + (1 << (shift - 1) if shift else 0)) >> shift)))
                    r = max(-32768, min(32767, (q << shift) + pred))
                    err += (int(v) - r) ** 2
                    nib.append(q & 15)
                    p2, p1 = p1, r
                    if best is not None and err >= best[0]:
                        break
                if best is None or err < best[0]:
                    best = (err, fi, shift, nib, p1, p2)
        _e, fi, shift, nib, s1, s2 = best
        flags = 1 if b == nblk - 1 else 0
        out += bytes([(fi << 4) | (12 - shift), flags]) + bytes(nib[i] | (nib[i + 1] << 4) for i in range(0, 28, 2))
    return bytes(out)


def bank_replace(arch: VSRArchive, bank: str, n: int, wav: bytes): #vers 1
    """PS1 bank sample n replaced by a WAV (PS-ADPCM at the WAV's rate); bank repacked, sbi updated."""
    info = bytearray(arch.blob('', bank + '_sbi'))
    sbk = arch.blob('', bank + '_sbk')
    pcm, rate = wav_pcm16(wav)
    out = bytearray(16)                                    # leading silent block
    for i in range(len(info) // 6):
        addr, size, r = struct.unpack_from('<3H', info, i * 6)
        if not size:
            continue
        data = psx_adpcm(pcm) if i == n else sbk[addr * 8 - SPU_BASE:addr * 8 - SPU_BASE + size * 8]
        struct.pack_into('<3H', info, i * 6, (SPU_BASE + len(out)) // 8, len(data) // 8,
                         min(rate, 65535) if i == n else r)
        out += data
    if SPU_BASE + len(out) > 0x80000:
        raise ValueError("Sound bank would not fit PS1 sound RAM (512 KB)")
    arch.set_blob('', bank + '_sbi', bytes(info))
    arch.set_blob('', bank + '_sbk', bytes(out))


def vsr_image_set(arch: VSRArchive, g: dict, n: int, rgba: np.ndarray): #vers 1
    """PS1 image n of a gfx folder from RGBA of the same size; its page written into the archive."""
    w, h, x, y, tp, cp, cx, _f, _x = g['index'][n]
    w, h = w or 256, h or 256
    if rgba.shape[:2] != (h, w):
        raise ValueError(f"Image {n} is {w} x {h}")
    if g['bpp'][tp] == 8:
        g['pages'][tp][y:y + h, x:x + w] = quantize(rgba, ps1_rgba(g['cluts'][cp][cx:cx + 256]))
    else:
        put_4bit(g['pages'][tp], x, y, quantize(rgba, ps1_rgba(g['cluts'][cp][cx:cx + 16])))
    arch.entries[tp]['data'] = g['pages'][tp].tobytes()
