#this belongs in apps/methods/col_workshop_writer.py - Version: 1
# X-Seti - March 2026 - IMG Factory 1.6 - COL Binary Writer
# Based on DragonFF col.py __write_col/__write_col_new (Parik, GPL-2.0+)
"""
COL Binary Writer — serialises COLModel objects back to binary .col format.
Supports COL1 (GTA III/VC) and COL2/3/4 (SA).

##Functions -
# model_to_bytes   - serialise one COLModel → bytes
# models_to_bytes  - serialise list of COLModels → bytes (archive)
# save_col_file    - write archive to disk
"""

import struct
from typing import List

from apps.components.Mdl_Editor.depends.col_workshop_classes import COLModel, COLVersion


#    surface packing                                                           

def _surface_bytes(obj) -> bytes:
    """Extract (material, flag, brightness, light) from a COLSphere/COLBox/COLFace."""
    if hasattr(obj, 'material') and not isinstance(obj.material, int):
        mat = getattr(obj.material, 'material_id', 0)
        flg = getattr(obj.material, 'flag', 0)
    else:
        mat = getattr(obj, 'material', 0) if isinstance(getattr(obj,'material',0), int) else 0
        flg = getattr(obj, 'flag', 0)
    bri = getattr(obj, 'brightness', 0)
    lgt = getattr(obj, 'light', 0)
    return bytes([mat & 0xFF, flg & 0xFF, bri & 0xFF, lgt & 0xFF])


def _vec3(obj) -> tuple:
    """Return (x, y, z) from a Vector3, tuple, or list."""
    if hasattr(obj, 'x'):
        return obj.x, obj.y, obj.z
    return float(obj[0]), float(obj[1]), float(obj[2])


#    COL1 writer                                                                

def _write_col1(model: COLModel) -> bytes:
    """Serialise bounds + legacy (COL1) data section."""
    buf = bytearray()

    # Bounds COL1: radius(4) + center(12) + min(12) + max(12) = 40 bytes
    b = model.bounds
    r = getattr(b, 'radius', 0.0)
    cx, cy, cz = _vec3(getattr(b, 'center', (0,0,0)))
    mnx, mny, mnz = _vec3(getattr(b, 'min', (0,0,0)))
    mxx, mxy, mxz = _vec3(getattr(b, 'max', (0,0,0)))
    buf += struct.pack('<f', r)
    buf += struct.pack('<fff', cx, cy, cz)
    buf += struct.pack('<fff', mnx, mny, mnz)
    buf += struct.pack('<fff', mxx, mxy, mxz)

    # Spheres: count(4) + [center(12)+radius(4)+surface(4)] * n
    spheres = getattr(model, 'spheres', [])
    buf += struct.pack('<I', len(spheres))
    for sph in spheres:
        cx2, cy2, cz2 = _vec3(getattr(sph, 'center', (0,0,0)))
        buf += struct.pack('<fff', cx2, cy2, cz2)
        buf += struct.pack('<f', getattr(sph, 'radius', 0.0))
        buf += _surface_bytes(sph)

    # Unknown / lines (always 0 in COL1)
    buf += struct.pack('<I', 0)

    # Boxes: count(4) + [min(12)+max(12)+surface(4)] * n
    boxes = getattr(model, 'boxes', [])
    buf += struct.pack('<I', len(boxes))
    for box in boxes:
        mn = getattr(box, 'min_point', getattr(box, 'min', (0,0,0)))
        mx = getattr(box, 'max_point', getattr(box, 'max', (0,0,0)))
        buf += struct.pack('<fff', *_vec3(mn))
        buf += struct.pack('<fff', *_vec3(mx))
        buf += _surface_bytes(box)

    # Vertices COL1: count(4) + [float x3] * n
    verts = getattr(model, 'vertices', [])
    buf += struct.pack('<I', len(verts))
    for v in verts:
        buf += struct.pack('<fff', *_vec3(v))

    # Faces COL1: count(4) + [uint32 a,b,c + surface(4)] * n
    faces = getattr(model, 'faces', [])
    buf += struct.pack('<I', len(faces))
    for f in faces:
        buf += struct.pack('<III', int(f.a), int(f.b), int(f.c))
        buf += _surface_bytes(f)

    return bytes(buf)


#    COL2/3/4 writer                                                            

def _compress_vertex(v) -> bytes:
    """Convert float vertex to COL2/3 int16 fixed-point (* 128, clamped to ±255.99)."""
    x, y, z = _vec3(v)
    ix = max(-32767, min(32767, int(round(x * 128))))
    iy = max(-32767, min(32767, int(round(y * 128))))
    iz = max(-32767, min(32767, int(round(z * 128))))
    return struct.pack('<hhh', ix, iy, iz)


def _write_col_new(model: COLModel) -> bytes:
    """Serialise bounds + COL2/3/4 offset-table data section.
    Mirrors DragonFF __write_col_new exactly."""
    ver = model.version.value if hasattr(model.version, 'value') else int(model.version)

    spheres  = getattr(model, 'spheres',  [])
    boxes    = getattr(model, 'boxes',    [])
    verts    = getattr(model, 'vertices', [])
    faces    = getattr(model, 'faces',    [])
    shadow_v = getattr(model, 'shadow_verts', [])
    shadow_f = getattr(model, 'shadow_faces', [])

    flags  = 0
    flags |= 2 if (spheres or boxes or faces) else 0
    flags |= 16 if (shadow_f and ver >= 3) else 0

    # header_len = total bytes before first data block, offset formula:
    # stored_offset = (file_pos_of_data - 4) - start_offset
    # = fourcc(4)+size(4)+name(22)+id(2)+bounds(40)+new_col_hdr(36) - 4 = 104
    header_len  = 104
    header_len += 12 if ver >= 3 else 0   # shadow counts+offsets
    header_len += 4  if ver >= 4 else 0

    data_buf = bytearray()
    offsets  = []

    # Spheres (no count prefix — offset points 4 bytes before data)
    offsets.append(len(data_buf) + header_len)
    for sph in spheres:
        cx2, cy2, cz2 = _vec3(getattr(sph, 'center', (0,0,0)))
        data_buf += struct.pack('<fff', cx2, cy2, cz2)
        data_buf += struct.pack('<f', getattr(sph, 'radius', 0.0))
        data_buf += _surface_bytes(sph)

    # Boxes
    offsets.append(len(data_buf) + header_len)
    for box in boxes:
        mn = getattr(box, 'min_point', getattr(box, 'min', (0,0,0)))
        mx = getattr(box, 'max_point', getattr(box, 'max', (0,0,0)))
        data_buf += struct.pack('<fff', *_vec3(mn))
        data_buf += struct.pack('<fff', *_vec3(mx))
        data_buf += _surface_bytes(box)

    # Cones offset (unused, always 0)
    offsets.append(0)

    # Vertices (int16 fixed-point)
    offsets.append(len(data_buf) + header_len)
    for v in verts:
        data_buf += _compress_vertex(v)

    # Faces (uint16 indices + material + light = 8 bytes)
    offsets.append(len(data_buf) + header_len)
    for f in faces:
        mat_id = f.material if isinstance(f.material, int) else \
                 getattr(f.material, 'material_id', 0)
        light  = getattr(f, 'light', 0)
        data_buf += struct.pack('<HHH', int(f.a), int(f.b), int(f.c))
        data_buf += bytes([mat_id & 0xFF, light & 0xFF])

    # Triangle planes offset (unused)
    offsets.append(0)

    # Shadow mesh (COL3+)
    if ver >= 3:
        offsets.append(len(data_buf) + header_len)  # shadow_verts_off
        for v in shadow_v:
            data_buf += _compress_vertex(v)
        offsets.append(len(data_buf) + header_len)  # shadow_faces_off
        for f in shadow_f:
            mat_id = f.material if isinstance(f.material, int) else \
                     getattr(f.material, 'material_id', 0)
            light  = getattr(f, 'light', 0)
            data_buf += struct.pack('<HHH', int(f.a), int(f.b), int(f.c))
            data_buf += bytes([mat_id & 0xFF, light & 0xFF])

    # Bounds COL2/3: min(12) + max(12) + center(12) + radius(4) = 40 bytes
    b = model.bounds
    mnx, mny, mnz = _vec3(getattr(b, 'min', (0,0,0)))
    mxx, mxy, mxz = _vec3(getattr(b, 'max', (0,0,0)))
    cx, cy, cz    = _vec3(getattr(b, 'center', (0,0,0)))
    r = getattr(b, 'radius', 0.0)
    bounds_bytes = (struct.pack('<fff', mnx, mny, mnz) +
                    struct.pack('<fff', mxx, mxy, mxz) +
                    struct.pack('<fff', cx, cy, cz) +
                    struct.pack('<f', r))

    # new_col header: "<HHHBxIIIIIII" = 36 bytes
    hdr = struct.pack('<HHHBx',
        len(spheres), len(boxes), len(faces), 0)  # counts + pad
    hdr += struct.pack('<I', flags)
    hdr += struct.pack('<IIIIII', *offsets[:6])    # 6 offsets
    if ver >= 3:
        hdr += struct.pack('<III',
            len(shadow_f), *offsets[6:8])          # shadow counts+offsets

    return bounds_bytes + hdr + bytes(data_buf)


#    Top-level serialiser                                                       

def model_to_bytes(model: COLModel) -> bytes: #vers 1
    """Serialise one COLModel to binary COL bytes (header + payload)."""
    ver = model.version.value if hasattr(model.version, 'value') else int(model.version)
    name_str = getattr(model, 'name', '') or 'unnamed'

    if ver == 1:
        payload = _write_col1(model)
        fourcc  = b'COLL'
    else:
        payload = _write_col_new(model)
        fourcc  = f'COL{ver}'.encode('ascii')

    # File header: fourcc(4) + size(4) + name(22) + model_id(2) = 32 bytes
    # size = len(payload) + 24  (DragonFF: header_size = 24 = name(22)+id(2)+???
    # Actually: size field = bytes AFTER the size field itself = len(payload) + 22 + 2 = payload+24
    name_b   = name_str.encode('ascii', errors='replace')[:22].ljust(22, b'\x00')
    model_id = getattr(model, 'model_id', 0)
    size     = len(payload) + 24   # DragonFF: len(data) + header_size(24)

    return struct.pack('<4sI22sH', fourcc, size, name_b, model_id) + payload


def models_to_bytes(models: List[COLModel]) -> bytes: #vers 1
    """Serialise a list of COLModels to a binary COL archive."""
    return b''.join(model_to_bytes(m) for m in models)


def save_col_file(models: List[COLModel], file_path: str) -> bool: #vers 1
    """Write COLModels to a .col file. Returns True on success."""
    try:
        data = models_to_bytes(models)
        with open(file_path, 'wb') as f:
            f.write(data)
        return True
    except Exception as e:
        import traceback
        print(f"save_col_file error: {e}")
        traceback.print_exc()
        return False


__all__ = ['model_to_bytes', 'models_to_bytes', 'save_col_file']
