"""
col_operations.py - COL analysis operations for standalone Col-Workshop
"""

def get_col_detailed_analysis(file_path):
    """Return basic analysis data for a COL file."""
    try:
        from apps.methods.col_workshop_loader import COLFile
        cf = COLFile()
        if not cf.load_from_file(file_path):
            return {'error': 'Failed to load COL file'}
        models = getattr(cf, 'models', [])
        return {
            'file_path': file_path,
            'model_count': len(models),
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
