#!/usr/bin/env python3
#this belongs in apps/methods/col_operations.py - Version: 4
# X-Seti - September24 2026 - IMG Factory 1.6 - COL analysis operations (shared)
"""
col_operations.py - COL analysis operations for standalone Col-Workshop
"""

##Methods list -
# _scan_col_models
# cleanup_temporary_file
# col_to_dff_geometry
# create_temporary_col_file
# extract_col_from_img_entry
# get_col_basic_info
# get_col_detailed_analysis
# validate_col_data

import os
import struct
import tempfile

COL_VERSIONS = {b'COLL': 1, b'COL2': 2, b'COL3': 3, b'COL4': 4}


def _scan_col_models(col_data): #vers 1
    """Walk COL chunks, return list of (fourcc, name, size)."""
    models, pos = [], 0
    while pos + 32 <= len(col_data):
        fourcc = col_data[pos:pos + 4]
        if fourcc not in COL_VERSIONS:
            break
        size = struct.unpack_from('<I', col_data, pos + 4)[0]
        name = col_data[pos + 8:pos + 30].split(b'\0')[0].decode('latin-1', 'replace')
        models.append((fourcc, name, size))
        pos += 8 + size
    return models


def cleanup_temporary_file(temp_path): #vers 1
    """Delete temp COL file."""
    if temp_path and os.path.isfile(temp_path):
        os.remove(temp_path)


def col_to_dff_geometry(model): #vers 1
    """COL mesh as (Geometry, materials) for the GL viewport."""
    from apps.methods.dff_classes import Geometry, Material, RGBA, Triangle, Vector3
    from apps.methods.col_materials import get_material_colour
    verts = getattr(model, 'vertices', None) or []
    faces = getattr(model, 'faces', None) or []
    if not verts or not faces:
        return None, []
    def xyz(v):
        return (v.x, v.y, v.z) if hasattr(v, 'x') else (float(v[0]), float(v[1]), float(v[2]))
    geom = Geometry()
    geom.vertices = [Vector3(*xyz(v)) for v in verts]
    mats, slot = [], {}
    for f in faces:
        mid = int(getattr(f, 'material', 0) or 0)
        if mid not in slot:
            h = get_material_colour(mid)
            m = Material()
            m.colour = RGBA(int(h[0:2], 16), int(h[2:4], 16), int(h[4:6], 16), 255)
            m.color = m.colour
            slot[mid] = len(mats); mats.append(m)
        geom.triangles.append(Triangle(f.a, f.b, f.c, slot[mid]))
    return geom, mats


def create_temporary_col_file(col_data, entry_name): #vers 1
    """Write COL bytes to a temp file, return path."""
    base = os.path.splitext(os.path.basename(entry_name))[0] or 'entry'
    fd, path = tempfile.mkstemp(prefix=f'{base}_', suffix='.col')
    with os.fdopen(fd, 'wb') as f:
        f.write(col_data)
    return path


def extract_col_from_img_entry(main_window, row): #vers 1
    """Return (col_bytes, entry_name) for IMG row, or None."""
    img = getattr(main_window, 'current_img', None)
    if not img or not (0 <= row < len(img.entries)):
        return None
    entry = img.entries[row]
    from apps.methods.img_shared_operations import get_entry_data_safely
    data = get_entry_data_safely(entry, img, main_window)
    if not data:
        return None
    return data, entry.name


def get_col_basic_info(col_data): #vers 1
    """Signature, version and model count from COL bytes."""
    if len(col_data) < 32:
        return {'error': 'Data too small for COL'}
    sig = col_data[:4]
    if sig not in COL_VERSIONS:
        return {'error': f'Unknown signature {sig!r}'}
    models = _scan_col_models(col_data)
    return {'signature': sig, 'version': COL_VERSIONS[sig],
            'model_count': len(models)}

def get_col_detailed_analysis(file_path): #vers 3
    """Return basic analysis data for a COL file."""
    try:
        from apps.methods.col_workshop_loader import COLFile
        cf = COLFile()
        if not cf.load_from_file(file_path):
            return {'error': 'Failed to load COL file'}
        models = getattr(cf, 'models', [])
        tot = lambda a: sum(len(getattr(m, a, [])) for m in models)
        return {
            'file_path': file_path,
            'version': getattr(models[0], 'version', None) if models else None,
            'model_count': len(models),
            'total_spheres': tot('spheres'), 'total_boxes': tot('boxes'),
            'total_faces': tot('faces'), 'total_vertices': tot('vertices'),
            'models': [
                {
                    'name': getattr(m, 'name', f'Model_{i}'),
                    'version': getattr(m, 'version', None),
                    'spheres': len(getattr(m, 'spheres', [])),
                    'boxes':   len(getattr(m, 'boxes', [])),
                    'vertices':len(getattr(m, 'vertices', [])),
                    'faces':   len(getattr(m, 'faces', [])),
                }
                for i, m in enumerate(models)
            ]
        }
    except Exception as e:
        return {'error': str(e)}


def validate_col_data(col_data): #vers 1
    """Check COL chunk chain; return is_valid, errors, warnings."""
    errors, warnings = [], []
    models = _scan_col_models(col_data)
    if not models:
        errors.append('No valid COL header')
    used = sum(8 + m[2] for m in models)
    if models and used != len(col_data):
        tail = col_data[used:]
        if tail.strip(b'\0'):
            warnings.append(f'{len(col_data) - used} trailing bytes after last model')
        if used > len(col_data):
            errors.append('Last model size exceeds data')
    if len({m[0] for m in models}) > 1:
        warnings.append('Mixed COL versions in archive')
    return {'valid': not errors, 'errors': errors, 'warnings': warnings,
            'model_count': len(models)}
