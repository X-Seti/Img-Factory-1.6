#this belongs in apps/methods/path_nodes.py - Version: 6
# X-Seti - October 19 2026 - IMG Factory 1.6 - Path node writers

"""
Write edited path nodes back to IPL/IDE 'path' sections, SA nodesN.dat, tracks .dat.
"""

##Methods list -
# ide_path_section_lines
# ipl_path_section_lines
# replace_text_section
# rotate_vector_by_quaternion
# sa_node_offset
# write_back
# write_rrr
# write_sa_nodes
# write_text_path_section
# write_tracks_file

import os
import struct
from datetime import datetime

SA_HEADER = 20          # five uint32 counts
SA_NODE_SIZE = 28       # Section 1 record size


def _num(v): #vers 2
    """Game-style number text (C %g, 6 significant digits)."""
    return f"{float(v):g}"


def ipl_path_section_lines(groups): #vers 2
    """VC/SOL IPL path lines; coords stored x16 on disk."""
    out = []
    for g in groups:
        out.append(f"{g.header_a}, {g.header_b}")
        for n in g.nodes:
            out.append(f"\t{n.node_type}, {n.next_id}, {n.cross}, {_num(n.x * 16)}, {_num(n.y * 16)}, "
                       f"{_num(n.z * 16)}, {_num(n.median)}, {n.left}, {n.right}, "
                       f"{n.flag1}, {n.flag2}, {_num(n.flag3)}"
                       + "".join(f", {e}" for e in getattr(n, 'extra', ())))
    return out


def ide_path_section_lines(groups): #vers 1
    """GTA III IDE path lines; relative coords kept raw."""
    out = []
    for g in groups:
        out.append(f"{g.group_type}, {g.model_id}, {g.model_name}")
        for n in g.nodes:
            out.append(f"\t{n.node_type}, {n.next_id}, {n.is_crossroad}, {_num(n.x_rel)}, "
                       f"{_num(n.y_rel)}, {_num(n.z_rel)}, {_num(n.median)}, {n.left}, {n.right}")
    return out


def replace_text_section(text, name, body_lines): #vers 1
    """Replace section `name` body (up to 'end'); append if missing."""
    lines = text.splitlines()
    start = next((i for i, l in enumerate(lines)
                  if l.split('#')[0].strip().lower() == name), None)
    if start is None:
        lines += [name] + body_lines + ['end']
    else:
        end = next((i for i in range(start + 1, len(lines))
                    if lines[i].split('#')[0].strip().lower() == 'end'), len(lines))
        lines[start + 1:end] = body_lines
    return '\n'.join(lines) + '\n'


def write_text_path_section(file_path, body_lines): #vers 1
    """Rewrite the 'path' section of an IPL/IDE (backup + atomic)."""
    from apps.methods.file_backup import safe_write_bytes
    with open(file_path, 'r', encoding='ascii', errors='ignore', newline='') as f:
        raw = f.read()
    crlf = '\r\n' in raw
    text = replace_text_section(raw.replace('\r\n', '\n'), 'path', body_lines)
    if crlf:
        text = text.replace('\n', '\r\n')
    safe_write_bytes(file_path, text.encode('ascii', errors='replace'),
                     f"Path Workshop: {os.path.basename(file_path)}")


def rotate_vector_by_quaternion(vx, vy, vz, x, y, z, w): #vers 1
    """Rotate (vx,vy,vz) by quaternion (x,y,z,w)."""
    xx, yy, zz = x * x, y * y, z * z
    xy, xz, yz = x * y, x * z, y * z
    wx, wy, wz = w * x, w * y, w * z
    return ((1 - 2 * (yy + zz)) * vx + 2 * (xy - wz) * vy + 2 * (xz + wy) * vz,
            2 * (xy + wz) * vx + (1 - 2 * (xx + zz)) * vy + 2 * (yz - wx) * vz,
            2 * (xz - wy) * vx + 2 * (yz + wx) * vy + (1 - 2 * (xx + yy)) * vz)


def sa_node_offset(path_file, node): #vers 1
    """Byte offset of an SA node's Section 1 record."""
    return SA_HEADER + SA_NODE_SIZE * path_file.all_nodes.index(node)


def _patch_sa_bytes(data, path_file, nodes): #vers 1
    """Patch x/y/z, width, flood fill, flags of changed nodes."""
    buf = bytearray(data)
    for n in nodes:
        o = sa_node_offset(path_file, n)
        struct.pack_into('<hhh', buf, o + 8, *(int(round(v * 8)) for v in (n.x, n.y, n.z)))
        struct.pack_into('<BBI', buf, o + 22, int(round(n.path_width * 8)) & 0xFF,
                         n.flood_fill & 0xFF, n.flags & 0xFFFFFFFF)
    return bytes(buf)


def write_back(src, make_bytes): #vers 3
    """Rewrite a loose file or an 'x.img:entry' in place (backup first).
    make_bytes(old) -> new; an IMG entry is padded in place, or rewritten when it grows."""
    img_path, sep, entry_name = src.rpartition(':')
    if sep and img_path.lower().endswith('.img') and os.path.isfile(img_path):
        from apps.methods.img_core_classes import IMGFile
        from apps.methods.file_backup import _backup_dir_for
        arc = IMGFile(img_path)
        arc.open()
        entry = next(e for e in arc.entries if e.name.lower() == entry_name.lower())
        data = arc.read_entry_data(entry)
        new = make_bytes(data)
        bdir = _backup_dir_for(img_path)
        os.makedirs(bdir, exist_ok=True)
        stamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        with open(os.path.join(bdir, f"{entry_name}.{stamp}.bak"), 'wb') as f:
            f.write(data)                      # entry backup, not whole IMG
        same_rsc = new[:4] != b'RSC\x05' or new[8:12] == data[8:12]   # IV table keeps RSC flags
        if len(new) <= len(data) and same_rsc:
            arc.write_entry_data(entry, new.ljust(len(data), b'\0'))
        elif not arc.add_entry(entry.name, new):          # grew: IMG rewrites the entry
            raise ValueError(f"{entry_name}: {getattr(arc, 'last_error', '') or 'IMG refused the larger entry'}")
        return
    from apps.methods.file_backup import safe_write_bytes
    with open(src, 'rb') as f:
        data = f.read()
    safe_write_bytes(src, make_bytes(data), f"Path Workshop: {os.path.basename(src)}")


def write_rrr(frames): #vers 1
    """Save an SA car recording to its loose .rrr or carrec.img entry."""
    from apps.methods.sa_path_parser import build_rrr
    write_back(frames[0].source_path, lambda _old: build_rrr(frames))


def write_sa_nodes(path_file, nodes): #vers 2
    """Save changed SA nodes to a loose nodesN.dat or its IMG entry."""
    write_back(path_file.source_path, lambda data: _patch_sa_bytes(data, path_file, nodes))


def write_tracks_file(file_path, points): #vers 1
    """Save tracks/flight/spath .dat: count line, then 'x y z [flag]' (backup + atomic)."""
    from apps.methods.file_backup import safe_write_bytes
    with open(file_path, 'rb') as f:
        crlf = b'\r\n' in f.read(256)
    rows = [str(len(points))]
    for p in points:
        row = f"{_num(p.x)} {_num(p.y)} {_num(p.z)}"
        rows.append(row if p.flag is None else f"{row} {p.flag}")
    nl = '\r\n' if crlf else '\n'
    safe_write_bytes(file_path, (nl.join(rows) + nl).encode('ascii'),
                     f"Path Workshop: {os.path.basename(file_path)}")
