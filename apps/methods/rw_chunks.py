#this belongs in apps/methods/rw_chunks.py - Version: 3
# X-Seti - September 2026 - IMG Factory 1.6 - RenderWare stream section tree

"""rw_chunks.py - Parse a RenderWare binary stream (.dff .txd .rws .anm .bsp ...)
into a section tree and edit it as bytes, in the spirit of Steve-M's RW Analyze:
  * tree of sections (12-byte header: type, size, library stamp), each classed as
    complex (children), data, empty or faulty (parse error);
  * validation: size past parent/file end, children not filling their parent, header
    cut short, struct/string sizes that make no sense;
  * byte level section operations that keep every ancestor's size right: delete, clear,
    insert, export, import, copy; recompute sizes; change the library stamp (RW version)
    of the whole stream; list texture names.
All functions work on / return plain bytes, so a hex editor can apply them as one edit."""

##Methods list -
# RWNode
# parse_rw
# version_of
# version_text
# stamp_for
# chunk_name
# validate
# delete_section
# clear_section
# insert_section
# export_section
# recompute_sizes
# set_stream_version
# texture_names
# dump_tree_text
# move_section
# make_section
# replace_payload
# string_payload
# node_at

import struct
from typing import Dict, List, Optional, Tuple

# Standard RenderWare chunk ids (GTAMods wiki) + Rockstar plugin ids
CHUNK_NAMES: Dict[int, str] = {
    0x0001: "Struct", 0x0002: "String", 0x0003: "Extension", 0x0005: "Camera",
    0x0006: "Texture", 0x0007: "Material", 0x0008: "Material List", 0x0009: "Atomic Section",
    0x000A: "Plane Section", 0x000B: "World", 0x000C: "Spline", 0x000D: "Matrix",
    0x000E: "Frame List", 0x000F: "Geometry", 0x0010: "Clump", 0x0012: "Light",
    0x0013: "Unicode String", 0x0014: "Atomic", 0x0015: "Texture Native",
    0x0016: "Texture Dictionary", 0x0017: "Animation Database", 0x0018: "Image",
    0x0019: "Skin Animation", 0x001A: "Geometry List", 0x001B: "Anim Animation",
    0x001C: "Team", 0x001D: "Crowd", 0x001E: "Delta Morph Animation",
    0x001F: "Right To Render", 0x0020: "Multitexture Effect Native",
    0x0021: "Multitexture Effect Dictionary", 0x0022: "Team Dictionary",
    0x0023: "Platform Independent Texture Dictionary", 0x0024: "Table of Contents",
    0x0025: "Particle Standard Global Data", 0x0026: "Altpipe", 0x0027: "Platform Independent Peds",
    0x0028: "Patch Mesh", 0x0029: "Chunk Group Start", 0x002A: "Chunk Group End",
    0x002B: "UV Animation Dictionary", 0x002C: "Coll Tree",
    0x0101: "Metrics PLG", 0x0102: "Spline PLG", 0x0103: "Stereo PLG", 0x0104: "VRML PLG",
    0x0105: "Morph PLG", 0x0106: "PVS PLG", 0x0107: "Memory Leak PLG", 0x0108: "Animation PLG",
    0x0109: "Gloss PLG", 0x010A: "Logo PLG", 0x010B: "Memory Info PLG", 0x010C: "Random PLG",
    0x010D: "PNG Image PLG", 0x010E: "Bone PLG", 0x010F: "VRML Anim PLG", 0x0110: "Sky Mipmap Val",
    0x0111: "MRM PLG", 0x0112: "LOD Atomic PLG", 0x0113: "ME PLG", 0x0114: "Lightmap PLG",
    0x0115: "Refine PLG", 0x0116: "Skin PLG", 0x0117: "Label PLG", 0x0118: "Particles PLG",
    0x0119: "GeomTX PLG", 0x011A: "Synth Core PLG", 0x011B: "STQPP PLG", 0x011C: "Part PP PLG",
    0x011D: "Collision PLG", 0x011E: "HAnim PLG", 0x011F: "User Data PLG", 0x0120: "Material Effects PLG",
    0x0121: "Particle System PLG", 0x0122: "Delta Morph PLG", 0x0123: "Patch PLG",
    0x0124: "Team PLG", 0x0125: "Crowd PP PLG", 0x0126: "Mip Split PLG", 0x0127: "Anisotropy PLG",
    0x0129: "GCN Material PLG", 0x012A: "Geometric PVS PLG", 0x012B: "XBOX Material PLG",
    0x012C: "Multi Texture PLG", 0x012D: "Chain PLG", 0x012E: "Toon PLG", 0x012F: "PTank PLG",
    0x0130: "Particle Standard PLG", 0x0131: "PDS PLG", 0x0132: "PrtAdv PLG", 0x0133: "Normal Map PLG",
    0x0134: "ADC PLG", 0x0135: "UV Animation PLG",
    0x0150: "Character Set PLG", 0x0151: "NOHS World PLG", 0x0152: "Import Util PLG",
    0x0153: "Slerp PLG", 0x0154: "Optim PLG", 0x0155: "TL World PLG", 0x0156: "Database PLG",
    0x0157: "Raytrace PLG", 0x0158: "Ray PLG", 0x0159: "Library PLG", 0x0180: "2D PLG",
    0x0181: "Tile Render PLG", 0x0182: "JPEG Image PLG", 0x0183: "TGA Image PLG", 0x0184: "GIF Image PLG",
    0x0185: "Quat PLG", 0x0186: "Spline PVS PLG", 0x0187: "Mipmap PLG", 0x0188: "MipmapK PLG",
    0x0189: "2D Font", 0x018A: "Intersection PLG", 0x018B: "TIFF Image PLG", 0x018C: "Pick PLG",
    0x018D: "BMP Image PLG", 0x018E: "RAS Image PLG", 0x018F: "Skin FX PLG", 0x0190: "VCAT PLG",
    0x0191: "2D Path", 0x0192: "2D Brush", 0x0193: "2D Object", 0x0194: "2D Shape", 0x0195: "2D Scene",
    0x0196: "2D Pick Region", 0x0197: "2D Object String", 0x0198: "2D Animation PLG", 0x0199: "2D Animation",
    0x0200: "2D Keyframe", 0x0201: "2D Maestro", 0x0202: "Barycentric",
    0x0203: "Platform Independent Texture Dictionary TK", 0x0204: "TOC TK", 0x0205: "TPL TK",
    0x0206: "Altpipe TK", 0x0207: "Animation TK", 0x0208: "Skin Split Compress", 0x0209: "Compressed Key TK",
    0x020A: "GEO Conditioning PLG", 0x020B: "Wing PLG", 0x020C: "Generic Pipeline TK", 0x020D: "Lightmap Conversion TK",
    0x020E: "Filesystem PLG", 0x020F: "Dictionary TK", 0x0210: "UV Animation Linear", 0x0211: "UV Animation Parameter",
    0x0212: "Bin Mesh PLG", 0x0213: "Native Data PLG", 0x0510: "Native Data PLG",
    0x0253F2F3: "Pipeline Set", 0x0253F2F6: "Specular Material", 0x0253F2F8: "2D Effect",
    0x0253F2F9: "Extra Vert Colour", 0x0253F2FA: "Collision Model", 0x0253F2FC: "Reflection Material",
    0x0253F2FD: "Breakable", 0x0253F2FE: "Frame", 0x0253F2F4: "Night Vertex Colours",
}
# 0x0116/0x0510 collide with the table above only in name; keep the bin-mesh id used by GTA
CHUNK_NAMES[0x050E] = "Bin Mesh PLG"

# sections that hold child sections
CONTAINERS = {0x03, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0E, 0x0F, 0x10, 0x12, 0x14, 0x15, 0x16,
              0x1A, 0x1C, 0x2C}
# data-only sections
LEAVES = {0x01, 0x02, 0x13, 0x0D}


def chunk_name(t: int) -> str:
    return CHUNK_NAMES.get(t, f"Unknown 0x{t:X}")


def version_of(stamp: int) -> int:
    """Library stamp -> RW version (0x36003 = 3.6.0.3)."""
    if stamp & 0xFFFF0000:
        return ((stamp >> 14) & 0x3FF00) + 0x30000 | ((stamp >> 16) & 0x3F)
    return stamp << 8


def version_text(stamp: int) -> str:
    v = version_of(stamp)
    return f"{(v >> 16) & 0xFF}.{(v >> 12) & 0xF}.{(v >> 8) & 0xF}.{v & 0xFF}"


def stamp_for(version: int, build: int = 0xFFFF) -> int:
    """RW version (e.g. 0x36003) -> library stamp with the given build number."""
    v = version - 0x30000
    return ((v & 0x3FF00) << 14) | ((v & 0x3F) << 16) | (build & 0xFFFF)


class RWNode:
    __slots__ = ("offset", "type", "size", "stamp", "children", "parent", "kind", "error")

    def __init__(self, offset, type_, size, stamp, parent=None):
        self.offset, self.type, self.size, self.stamp = offset, type_, size, stamp
        self.children: List["RWNode"] = []
        self.parent = parent
        self.kind = "data"          # complex | data | empty | faulty
        self.error = ""

    @property
    def name(self) -> str:
        if self.type == 0 and self.error.startswith("note:"):
            return "Trailing bytes"
        return chunk_name(self.type)

    @property
    def end(self) -> int:
        return self.offset + 12 + self.size

    @property
    def data_start(self) -> int:
        return self.offset + 12

    def walk(self):
        yield self
        for c in self.children:
            yield from c.walk()

    def depth(self) -> int:
        d, p = 0, self.parent
        while p:
            d, p = d + 1, p.parent
        return d


def _plausible_stamp(stamp: int) -> bool:
    v = version_of(stamp)
    return 0x30000 <= v <= 0x4FFFF or stamp in (0, 0x1003FFFF, 0x0800FFFF)


def _parse_range(data, start: int, end: int, parent: Optional[RWNode], out: List[RWNode], depth=0):
    pos = start
    while pos < end:
        if end - pos < 12:
            n = RWNode(pos, 0, 0, 0, parent)
            n.kind, n.error = "empty", f"note: {end - pos} trailing byte(s) after the last section (padding)"
            n.size = -12 + (end - pos)               # so offset + 12 + size == end
            out.append(n)
            return
        t, size, stamp = struct.unpack_from("<III", data, pos)
        n = RWNode(pos, t, size, stamp, parent)
        out.append(n)
        pos_end = pos + 12 + size
        if pos_end > end:
            n.kind, n.error = "faulty", f"size {size} runs {pos_end - end} byte(s) past its parent/file end"
            n.size = max(0, end - pos - 12)          # clip so the rest can still be shown
            pos_end = end
        if not _plausible_stamp(stamp) and not n.error:
            n.error = f"unusual library stamp 0x{stamp:08X}"
        if size == 0 and not n.error:
            n.kind = "empty"
        elif t in LEAVES:
            n.kind = "faulty" if n.error.startswith("size") else "data"
        elif t in CONTAINERS or (t not in CHUNK_NAMES and _looks_like_container(data, pos + 12, pos_end)):
            n.kind = "faulty" if n.error.startswith("size") else "complex"
            if depth < 40:
                _parse_range(data, pos + 12, pos_end, n, n.children, depth + 1)
                if any(c.kind == "faulty" for c in n.children) and not n.error:
                    n.error = "contains faulty sections"
                    n.kind = "faulty"
        else:
            n.kind = "faulty" if n.error.startswith("size") else "data"
        pos = pos_end


def _looks_like_container(data, start, end) -> bool:
    """Unknown section: treat as complex only when its payload splits exactly into valid sections."""
    pos, n = start, 0
    while pos < end:
        if end - pos < 12:
            return False
        _t, size, stamp = struct.unpack_from("<III", data, pos)
        if not _plausible_stamp(stamp) or pos + 12 + size > end:
            return False
        pos += 12 + size
        n += 1
    return n > 0 and pos == end


def parse_rw(data) -> List[RWNode]:
    """Top-level sections of the stream (each with .children)."""
    roots: List[RWNode] = []
    _parse_range(data, 0, len(data), None, roots)
    return roots


def validate(nodes: List[RWNode]) -> List[str]:
    """One line per problem found anywhere in the tree."""
    out = []
    for r in nodes:
        for n in r.walk():
            if n.error and not n.error.startswith("note:"):
                out.append(f"0x{n.offset:08X}  {n.name}: {n.error}")
            if n.kind == "complex" and n.children:
                got = sum(12 + c.size for c in n.children)
                if got != n.size:
                    out.append(f"0x{n.offset:08X}  {n.name}: children fill {got} of {n.size} byte(s)")
    return out


def _ancestors(node: RWNode):
    p = node.parent
    while p:
        yield p
        p = p.parent


def _bump_sizes(buf: bytearray, node: RWNode, delta: int):
    for a in _ancestors(node):
        struct.pack_into("<I", buf, a.offset + 4, struct.unpack_from("<I", buf, a.offset + 4)[0] + delta)


def delete_section(data: bytes, node: RWNode) -> bytes:
    """The stream without this section (and its children); ancestors' sizes shrink."""
    buf = bytearray(data)
    length = 12 + node.size
    del buf[node.offset:node.offset + length]
    # ancestors start before the removed range, so their offsets are unchanged
    _bump_sizes(buf, node, -length)
    return bytes(buf)


def clear_section(data: bytes, node: RWNode) -> bytes:
    """Keep the section's header but drop its payload (size 0); ancestors shrink."""
    buf = bytearray(data)
    if node.size:
        del buf[node.offset + 12:node.offset + 12 + node.size]
        struct.pack_into("<I", buf, node.offset + 4, 0)
        _bump_sizes(buf, node, -node.size)
    return bytes(buf)


def export_section(data: bytes, node: RWNode) -> bytes:
    return bytes(data[node.offset:node.offset + 12 + node.size])


def insert_section(data: bytes, before: Optional[RWNode], chunk: bytes, parent: Optional[RWNode] = None) -> bytes:
    """Insert whole-section bytes: right after `before` (same parent), or as the first child of
    `parent`, or at the end of the stream when both are None. Ancestors' sizes grow."""
    buf = bytearray(data)
    if before is not None:
        at = before.end
    elif parent is not None:
        at = parent.data_start
    else:
        buf += chunk
        return bytes(buf)
    buf[at:at] = chunk
    target = before
    if target is not None:
        for a in _ancestors(target):
            struct.pack_into("<I", buf, a.offset + 4, struct.unpack_from("<I", buf, a.offset + 4)[0] + len(chunk))
    elif parent is not None:
        struct.pack_into("<I", buf, parent.offset + 4, struct.unpack_from("<I", buf, parent.offset + 4)[0] + len(chunk))
        for a in _ancestors(parent):
            struct.pack_into("<I", buf, a.offset + 4, struct.unpack_from("<I", buf, a.offset + 4)[0] + len(chunk))
    return bytes(buf)


def recompute_sizes(data: bytes) -> bytes:
    """Rewrite every complex section's size from its children (bottom-up)."""
    buf = bytearray(data)
    roots = parse_rw(buf)

    def fix(n: RWNode) -> int:
        if n.kind in ("complex",) and n.children:
            total = sum(fix(c) for c in n.children)
            struct.pack_into("<I", buf, n.offset + 4, total)
            return 12 + total
        return 12 + n.size

    for r in roots:
        fix(r)
    return bytes(buf)


def set_stream_version(data: bytes, version: int, build: Optional[int] = None) -> Tuple[bytes, int]:
    """Set the library stamp of EVERY section to `version` (e.g. 0x36003). `build` None keeps
    each section's own build number. Returns (bytes, sections changed)."""
    buf = bytearray(data)
    n = 0
    for r in parse_rw(buf):
        for x in r.walk():
            if x.type == 0 and x.error.startswith("note:"):
                continue                                   # trailing padding, not a section
            b = (x.stamp & 0xFFFF) if build is None and (x.stamp & 0xFFFF0000) else (0xFFFF if build is None else build)
            new = stamp_for(version, b)
            if new != x.stamp:
                struct.pack_into("<I", buf, x.offset + 8, new)
                n += 1
    return bytes(buf), n


def texture_names(data: bytes) -> List[str]:
    """Texture names in a DFF (Texture -> String children) or a TXD (native texture headers)."""
    names: List[str] = []
    for r in parse_rw(data):
        for n in r.walk():
            if n.type == 0x06:                                   # Texture: struct + name String + mask String
                strs = [c for c in n.children if c.type == 0x02]
                for s in strs[:1]:
                    names.append(bytes(data[s.data_start:s.data_start + s.size]).split(b"\0", 1)[0].decode("latin1"))
            elif n.type == 0x15:                                 # Texture Native: struct holds the name
                st = next((c for c in n.children if c.type == 0x01), None)
                if st and st.size >= 40:
                    nm = bytes(data[st.data_start + 8:st.data_start + 40]).split(b"\0", 1)[0].decode("latin1")
                    if nm:
                        names.append(nm)
    return names


def dump_tree_text(nodes: List[RWNode], with_version=True) -> str:
    lines = []
    for r in nodes:
        for n in r.walk():
            lines.append(f"{'  ' * n.depth()}0x{n.offset:08X}  {n.name}  size={n.size}"
                         + (f"  RW {version_text(n.stamp)}" if with_version else "")
                         + f"  [{n.kind}]" + (f"  ERROR: {n.error}" if n.error else ""))
    return "\n".join(lines)


def move_section(data: bytes, node: RWNode, direction: int) -> bytes: #vers 1
    """Swap a section with its previous (-1) or next (+1) sibling; sizes unchanged."""
    sibs = node.parent.children if node.parent is not None else parse_rw(data)
    idx = next((i for i, s in enumerate(sibs) if s.offset == node.offset), None)
    j = None if idx is None else idx + direction
    if j is None or not (0 <= j < len(sibs)):
        return bytes(data)
    a, b = (sibs[idx], sibs[j]) if direction > 0 else (sibs[j], sibs[idx])
    buf = bytearray(data)
    first, second = bytes(buf[a.offset:a.end]), bytes(buf[b.offset:b.end])
    buf[a.offset:b.end] = second + bytes(buf[a.end:b.offset]) + first
    return bytes(buf)


def make_section(type_: int, stamp: int, payload: bytes = b"") -> bytes: #vers 1
    """Whole-section bytes: 12-byte header + payload."""
    return struct.pack("<III", type_, len(payload), stamp) + payload


def replace_payload(data: bytes, node: RWNode, payload: bytes) -> bytes: #vers 1
    """New payload for a leaf section; its own and every ancestor's size follow."""
    buf = bytearray(data)
    delta = len(payload) - node.size
    buf[node.data_start:node.end] = payload
    struct.pack_into("<I", buf, node.offset + 4, len(payload))
    if delta:
        _bump_sizes(buf, node, delta)
    return bytes(buf)


def string_payload(text: str) -> bytes: #vers 1
    """RW String payload: text + NUL, padded to a multiple of 4."""
    raw = text.encode('latin-1') + b"\0"
    return raw + b"\0" * (-len(raw) % 4)


def node_at(roots: List[RWNode], offset: int) -> Optional[RWNode]: #vers 1
    """Deepest section whose bytes contain offset."""
    best = None
    stack = list(roots)
    while stack:
        n = stack.pop()
        if n.offset <= offset < n.end:
            best = n
            stack = list(n.children)
    return best
