#this belongs in apps/components/DP5_Workshop/depends/dp5_tape.py - Version: 2
# X-Seti - October 27 2026 - IMG Factory 1.6 - DP5 tape images and loaders.

"""
Tape images that load and show a picture on the real machine (no Qt):
ZX Spectrum TAP / TZX (BASIC loader + SCREEN$), C64 TAP (viewer PRG,
kernal encoding), Amstrad CPC CDT (binary with its own ink / mode code),
plus WAV audio of any of them for loading from a sound output.
"""

##Methods list -
# build_tape
# tape_wav
# _c64_bytes
# _c64_pulses
# _cpc_crc
# _cpc_record
# _wav
# c64_prg_hires
# c64_prg_multi
# c64_tap
# cpc_cdt
# cpc_screen_bin
# tap_block
# tap_blocks
# tzx_from_tap
# wav_c64
# wav_cdt
# wav_zx
# zx_header
# zx_screen_tap

import io
import struct
import wave

import numpy as np

# system key -> (label, tape file extension, how to load it)
SYSTEMS = {
    'zx':   ("ZX Spectrum", 'tap', 'LOAD ""  (Enter), then play'),
    'c64':  ("C64 hires", 'tap', 'LOAD  (Return), play, then RUN'),
    'c64m': ("C64 multicolour", 'tap', 'LOAD  (Return), play, then RUN'),
    'cpc0': ("Amstrad CPC mode 0", 'cdt', 'RUN""  (Enter), press a key, play'),
    'cpc1': ("Amstrad CPC mode 1", 'cdt', 'RUN""  (Enter), press a key, play'),
}
ZX_CLOCK = 3500000                                       # T-states / s (also TZX timing base)
C64_CLOCK = 985248                                       # PAL CPU Hz
RATE = 44100


def build_tape(system: str, rgba, w: int, h: int, name: str = "PICTURE") -> bytes: #vers 1
    """Tape image (TAP / C64 TAP / CDT) that loads and shows the picture."""
    from apps.components.DP5_Workshop.depends import dp5_retro_screens as RS
    if system == 'zx':
        return zx_screen_tap(RS.zx_scr(rgba, w, h), name)
    if system == 'c64':
        return c64_tap(c64_prg_hires(*RS.c64_hires(rgba, w, h)), name)
    if system == 'c64m':
        return c64_tap(c64_prg_multi(*RS.c64_multi(rgba, w, h)), name)
    mode = 0 if system == 'cpc0' else 1
    mem, inks = RS.cpc_screen(rgba, w, h, mode)
    return cpc_cdt(name, cpc_screen_bin(mem, inks, mode, inks[0]), CPC_LOAD, CPC_ENTRY)


def tape_wav(system: str, tape: bytes) -> bytes: #vers 1
    """WAV audio of a tape made by build_tape."""
    if system == 'zx':
        return wav_zx(tape)
    if system in ('c64', 'c64m'):
        return wav_c64(tape)
    return wav_cdt(tape)


# ZX Spectrum

def tap_block(flag: int, data: bytes) -> bytes: #vers 1
    """TAP block: length, flag, data, XOR checksum."""
    payload = bytes([flag]) + bytes(data)
    chk = 0
    for b in payload:
        chk ^= b
    return struct.pack('<H', len(payload) + 1) + payload + bytes([chk])


def zx_header(kind: int, name: str, length: int, p1: int, p2: int) -> bytes: #vers 1
    """17-byte ROM header (0 program, 3 bytes / CODE)."""
    return bytes([kind]) + name.encode('ascii', 'replace')[:10].ljust(10) + struct.pack('<HHH', length, p1, p2)


def zx_screen_tap(scr: bytes, name: str = "PICTURE") -> bytes: #vers 1
    """Auto-running TAP: 10 BORDER 0: LOAD ""SCREEN$: PAUSE 0, then the SCREEN$ block."""
    zero = b'0\x0e\x00\x00\x00\x00\x00'                  # literal 0 + hidden 5-byte number
    line = b'\xe7' + zero + b':\xef""\xaa:\xf2' + zero + b'\x0d'
    prog = struct.pack('>H', 10) + struct.pack('<H', len(line)) + line
    return (tap_block(0x00, zx_header(0, name, len(prog), 10, len(prog))) + tap_block(0xFF, prog) +
            tap_block(0x00, zx_header(3, name, 6912, 16384, 32768)) + tap_block(0xFF, scr))


def tap_blocks(tap: bytes) -> list: #vers 1
    """Raw block payloads (flag .. checksum) of a TAP file."""
    out, i = [], 0
    while i + 2 <= len(tap):
        n = struct.unpack_from('<H', tap, i)[0]
        out.append(tap[i + 2:i + 2 + n])
        i += 2 + n
    return out


def tzx_from_tap(tap: bytes, pause_ms: int = 1000) -> bytes: #vers 1
    """TZX 1.20 of standard speed (ID 10) blocks."""
    out = bytearray(b'ZXTape!\x1a\x01\x14')
    for blk in tap_blocks(tap):
        out += b'\x10' + struct.pack('<HH', pause_ms, len(blk)) + blk
    return bytes(out)


def wav_zx(tap: bytes) -> bytes: #vers 1
    """ROM-speed audio of a TAP (pilot, sync, bits, 1 s gaps)."""
    segs = []
    for blk in tap_blocks(tap):
        bits = np.unpackbits(np.frombuffer(blk, np.uint8))
        data = np.repeat(np.where(bits, 1710, 855), 2)
        pilot = np.full(8063 if blk[0] < 128 else 3223, 2168)
        segs.append((np.concatenate([pilot, [667, 735], data]) / ZX_CLOCK, 1.0))
    return _wav(segs)


# Commodore 64

def c64_prg_hires(bitmap: bytes, screen: bytes, border: int = 0) -> bytes: #vers 1
    """PRG: 10 SYS2061 viewer; screen matrix $0C00, bitmap $2000; key returns to BASIC."""
    stub = bytes.fromhex('0b080a009e32303631000000')
    code = bytes.fromhex('a93b8d11d0a9088d16d0a9388d18d0a9') + bytes([border]) + \
        bytes.fromhex('8d20d020e4fff0fba91b8d11d0a9c88d16d0a9158d18d060')
    mem = bytearray(0x3F40 - 0x0801)
    body = stub + code
    mem[:len(body)] = body
    mem[0x0C00 - 0x0801:0x0C00 - 0x0801 + 1000] = screen
    mem[0x2000 - 0x0801:0x2000 - 0x0801 + 8000] = bitmap
    return b'\x01\x08' + bytes(mem)


def c64_prg_multi(bitmap: bytes, screen: bytes, colour: bytes, bg: int, border: int = 0) -> bytes: #vers 1
    """PRG: multicolour viewer; colour data at $3F40 copied to $D800."""
    stub = bytes.fromhex('0b080a009e32303631000000')
    copy = bytes.fromhex('a200bd403f9d00d8bd40409d00d9bd40419d00dabd40429d00dbe8d0e5')
    code = copy + bytes.fromhex('a9') + bytes([bg]) + bytes.fromhex('8d21d0a9') + bytes([border]) + \
        bytes.fromhex('8d20d0a93b8d11d0a9188d16d0a9388d18d020e4fff0fba91b8d11d0a9c88d16d0a9158d18d060')
    mem = bytearray(0x4328 - 0x0801)
    body = stub + code
    mem[:len(body)] = body
    mem[0x0C00 - 0x0801:0x0C00 - 0x0801 + 1000] = screen
    mem[0x2000 - 0x0801:0x2000 - 0x0801 + 8000] = bitmap
    mem[0x3F40 - 0x0801:0x3F40 - 0x0801 + 1000] = colour
    return b'\x01\x08' + bytes(mem)


def _c64_bytes(data: bytes) -> list: #vers 1
    """Kernal pulse codes for bytes: marker L M, bits LSB first (0 = S M, 1 = M S), odd parity."""
    S, M, L = 0x30, 0x42, 0x56
    out = []
    for b in data:
        out += [L, M]
        par = 1
        for k in range(8):
            bit = b >> k & 1
            par ^= bit
            out += [M, S] if bit else [S, M]
        out += [M, S] if par else [S, M]
    return out


def _c64_pulses(block: bytes, leader: int) -> list: #vers 1
    """One kernal block: leader, two copies (countdown, data, checksum, end marker), trailer."""
    S, L = 0x30, 0x56
    chk = 0
    for b in block:
        chk ^= b
    out = [S] * leader
    out += _c64_bytes(bytes(range(0x89, 0x80, -1)) + block + bytes([chk])) + [L, S]
    out += [S] * 79
    out += _c64_bytes(bytes(range(0x09, 0x00, -1)) + block + bytes([chk])) + [L, S]
    return out + [S] * 78


def c64_tap(prg: bytes, name: str = "PICTURE") -> bytes: #vers 1
    """C64-TAPE-RAW v1: header block then the program, kernal speed."""
    start = prg[0] | prg[1] << 8
    body = prg[2:]
    header = bytes([1]) + struct.pack('<HH', start, start + len(body)) + \
        name.upper().encode('ascii', 'replace')[:16].ljust(16, b' ')
    header = header.ljust(192, b' ')
    pulses = _c64_pulses(header, 0x6A00) + _c64_pulses(body, 0x1A00)
    data = bytes(pulses)
    return b'C64-TAPE-RAW' + bytes([1, 0, 0, 0]) + struct.pack('<I', len(data)) + data


def wav_c64(tap: bytes) -> bytes: #vers 1
    """Audio of a C64 TAP (each code = one full wave of code x 8 cycles)."""
    codes = np.frombuffer(tap[20:], np.uint8).astype(np.float64)
    return _wav([(np.repeat(codes * 8 / 2, 2) / C64_CLOCK, 0.5)])


# Amstrad CPC

CPC_LOAD, CPC_ENTRY = 0x4000, 0x47D0


def cpc_screen_bin(mem: bytes, inks: list, mode: int, border: int = 0) -> bytes: #vers 1
    """16 KB file for &4000 (entry &47D0): screen + code in the unseen spare bytes; code copies it to &C000."""
    data = bytearray(mem)
    part1 = (bytes.fromhex('2100401100c0010040edb0') +           # LDIR &4000 -> &C000 (load messages gone)
             bytes.fromhex('210000cd05bc') +                       # SCR SET OFFSET 0
             bytes([0x3e, mode]) + bytes.fromhex('cd1cbd') +       # MC SET MODE (no clear)
             bytes([0x06, border, 0x48]) + bytes.fromhex('cd38bc') +   # SCR SET BORDER
             bytes.fromhex('c3d04f'))                              # JP &4FD0
    part2 = (bytes.fromhex('21d057aff54648e5cd32bce123f13cfe1020f1') +  # SCR SET INK 0-15 from &57D0
             bytes.fromhex('cd06bbc9'))                            # KM WAIT CHAR, RET
    data[0x07D0:0x07D0 + len(part1)] = part1
    data[0x0FD0:0x0FD0 + len(part2)] = part2
    data[0x17D0:0x17D0 + 16] = bytes(inks).ljust(16, b'\x00')
    return bytes(data)


def _cpc_crc(seg: bytes) -> bytes: #vers 1
    """Inverted CRC-16-CCITT, high byte first."""
    crc = 0xFFFF
    for b in seg:
        crc ^= b << 8
        for _ in range(8):
            crc = (crc << 1 ^ 0x1021 if crc & 0x8000 else crc << 1) & 0xFFFF
    return struct.pack('>H', crc ^ 0xFFFF)


def _cpc_record(sync: int, payload: bytes) -> bytes: #vers 1
    """Sync byte, 256-byte segments each with CRC, 4-byte trailer."""
    out = bytearray([sync])
    for i in range(0, max(1, len(payload)), 256):
        seg = payload[i:i + 256].ljust(256, b'\x00')
        out += seg + _cpc_crc(seg)
    return bytes(out + b'\xff' * 4)


def cpc_cdt(name: str, data: bytes, load: int, entry: int, ftype: int = 2) -> bytes: #vers 1
    """CDT (TZX ID 11 blocks) of one file in 2 KB header + data record pairs, 1000 baud."""
    out = bytearray(b'ZXTape!\x1a\x01\x14')
    n = (len(data) + 2047) // 2048
    for k in range(n):
        chunk = data[k * 2048:(k + 1) * 2048]
        hdr = (name.upper().encode('ascii', 'replace')[:16].ljust(16, b'\x00') +
               bytes([k + 1, 0xFF if k == n - 1 else 0, ftype]) +
               struct.pack('<HH', len(chunk), load + k * 2048) + bytes([0xFF if k == 0 else 0]) +
               struct.pack('<HH', len(data), entry)).ljust(64, b'\x00')
        for rec, pause in ((_cpc_record(0x2C, hdr), 20), (_cpc_record(0x16, chunk), 2000)):
            out += b'\x11' + struct.pack('<HHHHHHBH', 2333, 1167, 1167, 1167, 2333, 4096, 8, pause)
            out += struct.pack('<I', len(rec))[:3] + rec
    return bytes(out)


def wav_cdt(cdt: bytes) -> bytes: #vers 1
    """Audio of a CDT made by cpc_cdt (ID 11 blocks)."""
    segs, i = [], 10
    while i < len(cdt):
        pilot, s1, s2, zero, one, npilot, _used, pause = struct.unpack_from('<HHHHHHBH', cdt, i + 1)
        n = int.from_bytes(cdt[i + 16:i + 19], 'little')
        rec = np.frombuffer(cdt[i + 19:i + 19 + n], np.uint8)
        bits = np.repeat(np.where(np.unpackbits(rec), one, zero), 2)
        segs.append((np.concatenate([np.full(npilot, pilot), [s1, s2], bits]) / ZX_CLOCK, pause / 1000))
        i += 19 + n
    return _wav(segs)


def _wav(segments) -> bytes: #vers 1
    """8-bit mono WAV of [(half-wave durations s, pause s)]: square wave, silence in gaps."""
    parts = []
    for durs, pause in segments:
        edges = np.round(np.cumsum(durs) * RATE).astype(np.int64)
        level = np.zeros(int(edges[-1]), np.uint8)
        starts = np.concatenate([[0], edges[:-1]])
        idx = np.repeat(np.arange(len(durs)) & 1, edges - starts)
        level[:len(idx)] = np.where(idx, 0x30, 0xD0)
        parts += [level, np.full(int(pause * RATE), 0x80, np.uint8)]
    buf = io.BytesIO()
    with wave.open(buf, 'wb') as wv:
        wv.setnchannels(1)
        wv.setsampwidth(1)
        wv.setframerate(RATE)
        wv.writeframes(np.concatenate(parts).tobytes())
    return buf.getvalue()
