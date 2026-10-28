#this belongs in apps/components/DP5_Workshop/depends/amiga_icon.py - Version: 1
# X-Seti - October 27 2026 - IMG Factory 1.6 - Amiga .info icon codecs.

"""
Amiga Workbench .info pieces (no Qt): DiskObject builder (classic planar
image + tool types), NewIcon IM1= tool type encode / decode, and OS3.5
FORM ICON decode (FACE / IMAG chunks, RLE bit streams) plus OS4 ARGB.
NewIcon coding follows AROS icon.library (DecodeNI).
"""

##Methods list -
# _bits_reader
# _imag
# _rle
# classic_decode
# decode_info
# diskobject
# iconface_decode
# info_details
# newicon_decode
# newicon_lines

import re
import struct
import zlib

import numpy as np

NEWICON_MARK = b"*** DON'T EDIT THE FOLLOWING LINES!! ***"
LINE_CHARS = 120                                         # data chars per IM1= tool type
DO_TYPES = {1: 'WBDISK', 2: 'WBDRAWER', 3: 'WBTOOL', 4: 'WBPROJECT', 5: 'WBGARBAGE', 6: 'WBDEVICE',
            7: 'WBKICK', 8: 'WBAPPICON'}

# Workbench palettes for classic bitplane icons (WB3.9 / WB3.9 XL from palette prefs)
WB39 = [
    (144,148,149),(43,0,0),(255,255,255),(0,98,255),(120,120,120),(175,175,175),(170,144,124),(255,169,151),  # 0
    (149,149,149),(238,85,0),(153,255,17),(238,187,0),(85,85,255),(153,34,255),(0,255,136),(204,204,204),  # 8
    (0,0,0),(219,117,65),(0,0,0),(255,255,255),(68,68,68),(85,85,85),(102,102,102),(119,119,119),  # 16
    (136,136,136),(153,153,153),(170,170,170),(187,187,187),(204,204,204),(221,221,221),(238,238,238),(255,255,255),  # 24
    (0,0,255),(39,79,68),(120,106,253),(255,255,171),(122,110,68),(177,149,93),(249,255,255),(222,253,255),  # 32
    (194,250,255),(166,247,255),(138,244,255),(111,242,255),(83,239,255),(55,236,255),(27,233,255),(0,230,255),  # 40
    (57,56,56),(49,49,49),(41,42,42),(34,34,35),(27,27,27),(14,14,12),(0,0,255),(0,0,152),  # 48
    (0,0,83),(0,10,0),(0,220,255),(0,186,255),(255,68,255),(255,57,213),(255,45,170),(255,34,128),  # 56
    (255,23,85),(255,11,43),(255,0,0),(255,13,0),(255,26,0),(255,38,0),(255,51,0),(255,64,0),  # 64
    (255,77,0),(255,89,0),(255,102,0),(255,115,0),(255,128,0),(255,140,0),(255,153,0),(255,166,0),  # 72
    (255,179,0),(255,191,0),(255,204,0),(255,217,0),(255,230,0),(255,242,0),(255,255,0),(241,255,0),  # 80
    (227,255,0),(213,255,0),(199,255,0),(184,255,0),(170,255,0),(156,255,0),(142,255,0),(128,255,0),  # 88
    (113,255,0),(99,255,0),(85,255,0),(71,255,0),(56,255,0),(42,255,0),(28,255,0),(14,255,0),  # 96
    (0,255,0),(0,247,0),(0,239,0),(0,231,0),(0,223,0),(0,215,0),(0,207,0),(0,199,0),  # 104
    (0,191,0),(0,183,0),(0,175,0),(0,167,0),(0,159,0),(0,151,0),(0,143,0),(0,135,0),  # 112
    (0,127,0),(0,119,0),(0,111,0),(0,103,0),(0,95,0),(0,87,0),(0,79,0),(0,71,0),  # 120
    (0,63,0),(0,55,0),(0,47,0),(0,39,0),(0,31,0),(0,23,0),(43,0,0),(64,0,0),  # 128
    (85,0,0),(107,0,0),(128,0,0),(149,0,0),(170,0,0),(192,0,0),(213,0,0),(234,0,0),  # 136
    (255,0,0),(238,0,25),(221,0,49),(204,0,74),(187,0,98),(170,0,123),(153,0,147),(136,0,172),  # 144
    (119,0,196),(102,0,221),(102,204,102),(34,68,34),(170,255,170),(203,192,12),(186,171,16),(168,149,20),  # 152
    (151,128,23),(134,107,27),(117,86,31),(99,65,35),(82,44,39),(0,62,255),(0,72,255),(0,79,255),  # 160
    (0,92,255),(0,102,255),(0,112,255),(0,121,255),(0,131,255),(0,141,255),(0,151,255),(0,161,255),  # 168
    (47,34,34),(255,170,170),(228,255,175),(197,255,145),(147,255,106),(125,255,73),(110,255,52),(76,255,10),  # 176
    (170,170,170),(238,238,238),(34,0,32),(54,3,40),(74,6,49),(94,9,58),(114,12,67),(134,15,75),  # 184
    (155,18,84),(175,21,93),(195,24,102),(215,27,110),(235,31,119),(255,34,128),(85,170,170),(119,238,238),  # 192
    (255,204,136),(136,102,68),(255,255,204),(204,153,102),(68,51,0),(255,255,170),(255,255,238),(136,204,136),  # 200
    (68,102,68),(204,255,204),(102,153,102),(34,51,34),(170,255,170),(238,255,238),(204,204,136),(102,102,68),  # 208
    (255,255,204),(153,150,102),(51,51,46),(255,255,170),(215,220,218),(68,204,136),(40,62,95),(38,71,152),  # 216
    (104,95,91),(144,144,144),(170,144,124),(172,157,151),(255,68,136),(136,34,68),(255,102,204),(204,51,102),  # 224
    (68,17,34),(255,85,255),(255,119,238),(136,68,136),(68,34,68),(204,102,204),(102,51,102),(34,0,32),  # 232
    (170,85,170),(238,119,238),(223,0,0),(206,0,0),(190,0,0),(174,0,0),(158,0,0),(142,0,0),  # 240
    (125,0,0),(109,0,0),(62,162,190),(144,148,149),(123,123,123),(175,175,175),(170,144,124),(255,169,151),  # 248
]
#     WB3.9 XL 256-colour palette (from amigaos3_9xl.pal)   
WB39_XL = [
    (144,148,149),(43,0,0),(255,255,255),(0,98,255),(123,121,123),(175,175,175),(170,144,124),(255,169,151),  # 0
    (149,149,149),(238,85,0),(153,255,17),(238,187,0),(85,85,255),(153,34,255),(0,255,136),(204,204,204),  # 8
    (0,0,0),(224,4,64),(0,0,0),(224,224,192),(68,68,68),(85,85,85),(102,102,102),(119,119,119),  # 16
    (136,136,136),(153,153,153),(170,170,170),(187,187,187),(204,204,204),(221,221,221),(238,238,238),(255,255,255),  # 24
    (0,0,255),(39,79,68),(120,106,253),(255,255,171),(122,110,68),(177,149,93),(249,255,255),(222,253,255),  # 32
    (194,250,255),(166,247,255),(138,244,255),(111,242,255),(83,239,255),(55,236,255),(27,233,255),(0,230,255),  # 40
    (57,56,56),(49,49,49),(41,42,42),(34,34,35),(27,27,27),(14,14,12),(0,0,255),(0,0,152),  # 48
    (0,0,83),(0,10,0),(0,220,255),(0,186,255),(255,68,255),(255,57,213),(255,45,170),(255,34,128),  # 56
    (255,23,85),(255,11,43),(255,0,0),(255,13,0),(255,26,0),(255,38,0),(255,51,0),(255,64,0),  # 64
    (255,77,0),(255,89,0),(255,102,0),(255,115,0),(255,128,0),(255,140,0),(255,153,0),(255,166,0),  # 72
    (255,179,0),(255,191,0),(255,204,0),(255,217,0),(255,230,0),(255,242,0),(255,255,0),(241,255,0),  # 80
    (227,255,0),(213,255,0),(199,255,0),(184,255,0),(170,255,0),(156,255,0),(142,255,0),(128,255,0),  # 88
    (113,255,0),(99,255,0),(85,255,0),(71,255,0),(56,255,0),(42,255,0),(28,255,0),(14,255,0),  # 96
    (0,255,0),(0,247,0),(0,239,0),(0,231,0),(0,223,0),(0,215,0),(0,207,0),(0,199,0),  # 104
    (0,191,0),(0,183,0),(0,175,0),(0,167,0),(0,159,0),(0,151,0),(0,143,0),(0,135,0),  # 112
    (0,127,0),(0,119,0),(0,111,0),(0,103,0),(0,95,0),(0,87,0),(0,79,0),(0,71,0),  # 120
    (0,63,0),(0,55,0),(0,47,0),(0,39,0),(0,31,0),(0,23,0),(43,0,0),(64,0,0),  # 128
    (85,0,0),(107,0,0),(128,0,0),(149,0,0),(170,0,0),(192,0,0),(213,0,0),(234,0,0),  # 136
    (255,0,0),(238,0,25),(221,0,49),(204,0,74),(187,0,98),(170,0,123),(153,0,147),(136,0,172),  # 144
    (119,0,196),(102,0,221),(102,204,102),(34,68,34),(170,255,170),(203,192,12),(186,171,16),(168,149,20),  # 152
    (151,128,23),(134,107,27),(117,86,31),(99,65,35),(82,44,39),(0,62,255),(0,72,255),(0,79,255),  # 160
    (0,92,255),(0,102,255),(0,112,255),(0,121,255),(0,131,255),(0,141,255),(0,151,255),(0,161,255),  # 168
    (47,34,34),(255,170,170),(228,255,175),(197,255,145),(147,255,106),(125,255,73),(110,255,52),(76,255,10),  # 176
    (170,170,170),(238,238,238),(34,0,32),(54,3,40),(74,6,49),(94,9,58),(114,12,67),(134,15,75),  # 184
    (155,18,84),(175,21,93),(195,24,102),(215,27,110),(235,31,119),(255,34,128),(85,170,170),(119,238,238),  # 192
    (255,204,136),(136,102,68),(255,255,204),(204,153,102),(68,51,0),(255,255,170),(255,255,238),(136,204,136),  # 200
    (68,102,68),(204,255,204),(102,153,102),(34,51,34),(170,255,170),(238,255,238),(204,204,136),(102,102,68),  # 208
    (255,255,204),(153,150,102),(51,51,46),(255,255,170),(215,220,218),(68,204,136),(40,62,95),(38,71,152),  # 216
    (104,95,91),(144,144,144),(170,144,124),(172,157,151),(255,68,136),(136,34,68),(255,102,204),(204,51,102),  # 224
    (68,17,34),(255,85,255),(255,119,238),(136,68,136),(68,34,68),(204,102,204),(102,51,102),(34,0,32),  # 232
    (170,85,170),(238,119,238),(223,0,0),(206,0,0),(190,0,0),(174,0,0),(158,0,0),(142,0,0),  # 240
    (125,0,0),(109,0,0),(62,162,190),(144,148,149),(123,123,123),(175,175,175),(170,144,124),(255,169,151),  # 248
]
#    OCS/ECS WB3 4-colour                                      
OCS = [(0,0,0),(255,255,255),(0,0,0),(102,136,187)]
#    MagicWB 8-colour                                           
MAGIC = [(0,0,0),(0,0,0),(255,255,255),(59,103,162),
         (123,123,123),(149,149,149),(170,144,124),(255,169,0)]
#    AGA WB 16-colour                                           
AGA16 = [(144,148,149),(43,0,0),(255,255,255),(0,98,255),
         (120,120,120),(175,175,175),(170,144,124),(255,169,151),
         (149,149,149),(238,85,0),(153,255,17),(238,187,0),
         (85,85,255),(153,34,255),(0,255,136),(204,204,204)]

PALETTES = {'wb39': WB39, 'aga_wb': WB39, 'wb39xl': WB39_XL, 'ocs': OCS, 'magicwb': MAGIC, 'aga': AGA16}


def diskobject(w: int, h: int, planes: bytes, depth: int, tooltypes=(), do_type: int = 4) -> bytes: #vers 1
    """DiskObject: header, one Image (planes = depth planes of row_bytes x h), optional tool types."""
    do = bytearray(78)
    struct.pack_into('>HH', do, 0, 0xE310, 1)
    struct.pack_into('>HHHHHHH', do, 8, 0, 0, w, h, 0x0005, 0x0003, 0x0001)   # gadget box, flags, BOOLGADGET
    struct.pack_into('>I', do, 22, 0x00010000)                                  # GadgetRender present
    struct.pack_into('>I', do, 44, 1)                                           # UserData: OS2 revision
    do[48] = do_type
    struct.pack_into('>I', do, 54, 0x00020000 if tooltypes else 0)              # do_ToolTypes present
    struct.pack_into('>II', do, 58, 0x80000000, 0x80000000)                     # NO_ICON_POSITION
    struct.pack_into('>I', do, 74, 4096)                                        # stack
    image = struct.pack('>hhHHHIBBI', 0, 0, w, h, depth, 0x00030000, (1 << depth) - 1, 0, 0)
    out = bytes(do) + image + bytes(planes)
    if tooltypes:
        out += struct.pack('>I', (len(tooltypes) + 1) * 4)
        for t in tooltypes:
            out += struct.pack('>I', len(t) + 1) + bytes(t) + b'\0'
    return out


def newicon_lines(rgba, w: int, h: int) -> list: #vers 1
    """IM1= tool types (bytes) for an image up to 93 x 93 and 255 colours (+ transparent)."""
    if not (1 <= w <= 93 and 1 <= h <= 93):
        raise ValueError(f"NewIcons are at most 93 x 93 (canvas is {w} x {h})")
    from PIL import Image
    img = Image.frombytes('RGBA', (w, h), bytes(rgba))
    alpha = np.asarray(img.getchannel('A')) >= 128
    transp = not alpha.all()
    rgb = img.convert('RGB')
    q = rgb.quantize(colors=255 if transp else 256, method=Image.Quantize.MEDIANCUT, dither=Image.Dither.NONE)
    idx = np.asarray(q, np.int32)
    used = int(idx.max()) + 1
    pal = (q.getpalette() + [0] * 768)[:used * 3]
    if transp:                                           # index 0 = transparent
        idx = np.where(alpha, idx + 1, 0)
        pal = [0, 0, 0] + pal
        used += 1
    bits = max(1, (used - 1).bit_length())

    def chars(values, nbits):
        """7-bit characters for values of nbits each (padded with zero bits)."""
        stream = ''.join(format(v, f'0{nbits}b') for v in values)
        stream += '0' * (-len(stream) % 7)
        out = bytearray()
        for k in range(0, len(stream), 7):
            v = int(stream[k:k + 7], 2)
            out.append(v + 0x20 if v < 0x50 else v + 0x51)
        return bytes(out)

    head = bytes([ord('B' if transp else 'C'), w + 0x21, h + 0x21, (used >> 6) + 0x21, (used & 63) + 0x21])
    lines, cur = [], head
    per_group = 8                                        # 7 palette bytes -> 8 chars
    for k in range(0, len(pal), 7):
        if len(cur) + per_group > LINE_CHARS:
            lines.append(cur)
            cur = b''
        cur += chars(pal[k:k + 7], 8)
    lines.append(cur)
    px = idx.ravel().tolist()
    group = 7 * max(1, LINE_CHARS // bits)               # pixels per line: whole 7-pixel groups
    for k in range(0, len(px), group):
        lines.append(chars(px[k:k + group], bits))
    return [b'IM1=' + ln for ln in lines]


def newicon_decode(data: bytes): #vers 1
    """(rgba, w, h) from the IM1= tool types of a .info, or None."""
    lines = re.findall(rb'IM1=([^\x00]*)\x00', data)
    if not lines or len(lines[0]) < 5:
        return None
    hd = lines[0]
    transp = hd[0] == ord('B')
    w, h = hd[1] - 0x21, hd[2] - 0x21
    ncol = ((hd[3] - 0x21) << 6) + (hd[4] - 0x21)
    if w <= 0 or h <= 0 or ncol <= 0:
        return None
    queue = [hd[5:]] + lines[1:]

    def decode(bits, entries):
        out, buf, nb, loop = [], 0, 0, 0
        src = queue.pop(0)
        pos = 0
        while len(out) < entries:
            if loop:
                byte, loop = 0, loop - 1
            else:
                if pos >= len(src):
                    if not queue:
                        raise ValueError("NewIcon data truncated")
                    src, pos, nb = queue.pop(0), 0, 0
                byte = src[pos]
                pos += 1
                if byte < 0xA0:
                    byte -= 0x20
                elif byte < 0xD1:
                    byte -= 0x51
                else:
                    loop, byte = byte - 0xD1, 0
            buf = (buf << 7 | byte) & 0xFFFFFF
            nb += 7
            while nb >= bits and len(out) < entries:
                out.append(buf >> (nb - bits) & ((1 << bits) - 1))
                nb -= bits
        return out
    pal = np.array(decode(8, ncol * 3), np.uint8).reshape(ncol, 3)
    bits = max(1, (ncol - 1).bit_length())
    idx = np.array(decode(bits, w * h), np.int32).reshape(h, w)
    out = np.empty((h, w, 4), np.uint8)
    out[..., :3] = pal[np.minimum(idx, ncol - 1)]
    out[..., 3] = np.where((idx == 0) & transp, 0, 255)
    return out.tobytes(), w, h


def _bits_reader(data: bytes): #vers 1
    """read(n) -> next n bits, MSB first."""
    st = {'pos': 0}

    def read(n):
        v = 0
        for _ in range(n):
            p = st['pos']
            v = v << 1 | (data[p >> 3] >> (7 - (p & 7)) & 1 if (p >> 3) < len(data) else 0)
            st['pos'] = p + 1
        return v
    return read


def _rle(data: bytes, depth: int, count: int) -> list: #vers 1
    """OS3.5 bit-stream RLE: 8-bit control (n < 128: n + 1 literals, n > 128: repeat 257 - n)."""
    read, out = _bits_reader(data), []
    while len(out) < count:
        ctrl = read(8)
        if ctrl < 128:
            out += [read(depth) for _ in range(ctrl + 1)]
        elif ctrl > 128:
            out += [read(depth)] * (257 - ctrl)
    return out[:count]


def _imag(chunk: bytes, w: int, h: int, prev_pal): #vers 1
    """One IMAG chunk -> (rgba bytes, palette)."""
    transp, ncol, flags, ifmt, pfmt, depth, nimg, npal = struct.unpack_from('>BBBBBBHH', chunk, 0)
    ncol, nimg, npal = ncol + 1, nimg + 1, npal + 1
    img = chunk[10:10 + nimg]
    if flags & 2:
        raw = chunk[10 + nimg:10 + nimg + npal]
        pal = np.array(_rle(raw, 8, ncol * 3) if pfmt == 1 else list(raw[:ncol * 3]), np.uint8).reshape(-1, 3)
    else:
        pal = prev_pal
    if pal is None:
        raise ValueError("ICONFACE image has no palette")
    px = np.array(_rle(img, depth, w * h) if ifmt == 1 else list(img[:w * h]), np.int32).reshape(h, w)
    out = np.empty((h, w, 4), np.uint8)
    out[..., :3] = pal[np.minimum(px, len(pal) - 1)]
    out[..., 3] = np.where((px == transp) & bool(flags & 1), 0, 255)
    return out.tobytes(), pal


def iconface_decode(data: bytes): #vers 1
    """(rgba, w, h, kind) from an OS3.5 / OS4 FORM ICON inside a .info, or None."""
    at = data.find(b'FORM')
    while at >= 0 and data[at + 8:at + 12] != b'ICON':
        at = data.find(b'FORM', at + 1)
    if at < 0:
        return None
    end = at + 8 + struct.unpack_from('>I', data, at + 4)[0]
    pos, w = at + 12, 0
    first = argb = None
    pal = None
    while pos + 8 <= min(end, len(data)):
        tag, size = data[pos:pos + 4], struct.unpack_from('>I', data, pos + 4)[0]
        body = data[pos + 8:pos + 8 + size]
        if tag == b'FACE':
            w, h = body[0] + 1, body[1] + 1
        elif tag == b'IMAG' and w and first is None:
            first, pal = _imag(body, w, h, pal)
        elif tag == b'ARGB' and w and argb is None:
            raw = zlib.decompress(body[10:])
            a = np.frombuffer(raw[:w * h * 4], np.uint8).reshape(-1, 4)
            argb = a[:, [1, 2, 3, 0]].tobytes()                  # ARGB -> RGBA
        pos += 8 + size + (size & 1)
    if argb is not None:
        return argb, w, h, 'OS4-ARGB'
    if first is not None:
        return first, w, h, 'OS3.5-ICONFACE'
    return None


def classic_decode(data: bytes, palette): #vers 1
    """(rgba, w, h, depth) of the classic planar Image, colour 0 transparent, or (None, 0, 0, reason)."""
    w, h = struct.unpack_from('>HH', data, 12)
    if w == 0 or h == 0 or w > 1024 or h > 1024:
        return None, 0, 0, f"Invalid dimensions {w}x{h}"
    base = 78 + (56 if struct.unpack_from('>I', data, 66)[0] else 0)
    if base + 20 > len(data):
        return None, 0, 0, "File truncated at Image struct"
    depth = struct.unpack_from('>H', data, base + 8)[0]
    if depth == 0 or depth > 8:
        depth = 4
    row = ((w + 15) // 16) * 2
    plane = row * h
    off = base + 20
    for d in (depth, 4, 2):
        if off + plane * d <= len(data):
            depth = d
            break
    else:
        return None, 0, 0, "File too small for bitplane data"
    p = np.frombuffer(data, np.uint8, plane * depth, off).reshape(depth, h, row)
    bits = np.unpackbits(p, axis=2)[:, :, :w].astype(np.int32)
    idx = (bits << np.arange(depth)[:, None, None]).sum(0)
    pal = np.array(palette, np.uint8)
    out = np.empty((h, w, 4), np.uint8)
    out[..., :3] = pal[idx % len(pal)]
    out[..., 3] = np.where(idx == 0, 0, 255)
    return out.tobytes(), w, h, depth


def decode_info(data: bytes, palette): #vers 1
    """Best image of a .info: OS4 ARGB / OS3.5 ICONFACE, NewIcon, then classic. (rgba, w, h, kind) or (None, 0, 0, reason)."""
    if len(data) < 78 or data[0:2] != b'\xe3\x10':
        return None, 0, 0, "Not a valid .info file"
    if b'FORM' in data:
        face = iconface_decode(data)
        if face:
            return face
    if b'IM1=' in data:
        ni = newicon_decode(data)
        if ni:
            return ni[0], ni[1], ni[2], 'NewIcon-IM1'
    rgba, w, h, depth = classic_decode(data, palette)
    return (rgba, w, h, f'Classic-{depth}bp') if rgba else (None, 0, 0, depth)


def info_details(data: bytes) -> dict: #vers 1
    """type, default tool, tool types, stack size and image kinds of a .info."""
    do_type = data[48]
    stack = struct.unpack_from('>I', data, 74)[0]
    has_drawer, sel = struct.unpack_from('>I', data, 66)[0], struct.unpack_from('>I', data, 26)[0]
    has_tool, has_tt = struct.unpack_from('>I', data, 50)[0], struct.unpack_from('>I', data, 54)[0]
    pos = 78 + (56 if has_drawer else 0)
    for present in (True, bool(sel)):                    # skip normal and selected images
        if not present:
            continue
        w, h, d = struct.unpack_from('>HHH', data, pos + 4)
        pos += 20 + ((w + 15) // 16) * 2 * h * d
    tool, tooltypes = '', []
    if has_tool:
        n = struct.unpack_from('>I', data, pos)[0]
        tool = data[pos + 4:pos + 4 + n].rstrip(b'\0').decode('latin-1')
        pos += 4 + n
    if has_tt:
        count = struct.unpack_from('>I', data, pos)[0] // 4 - 1
        pos += 4
        for _ in range(max(0, count)):
            n = struct.unpack_from('>I', data, pos)[0]
            tooltypes.append(data[pos + 4:pos + 4 + n].rstrip(b'\0').decode('latin-1'))
            pos += 4 + n
    kinds = ['Classic'] + (['NewIcon'] if any(t.startswith('IM1=') for t in tooltypes) else [])
    if b'FORM' in data and iconface_decode(data):
        kinds.append(iconface_decode(data)[3])
    return {'type': DO_TYPES.get(do_type, str(do_type)), 'default_tool': tool, 'stack': stack,
            'tooltypes': [t for t in tooltypes if not t.startswith('IM') and t != NEWICON_MARK.decode()],
            'images': kinds}
