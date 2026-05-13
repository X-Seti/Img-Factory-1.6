# X-Seti - May13 2026 - IMG Factory 1.6 - Model Viewer DFF Viewport
# this belongs in apps/components/Model_Viewer/methods/dff_viewport.py - Version: 1
"""
Model Viewer standalone viewport — re-exports DFFViewport from shared module.
Standalone fallback when apps.methods is unavailable.
"""

try:
    from apps.methods.dff_viewport import DFFViewport, OPENGL_AVAILABLE
except ImportError:
    import sys, os
    sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..', '..'))
    from apps.methods.dff_viewport import DFFViewport, OPENGL_AVAILABLE

__all__ = ['DFFViewport', 'OPENGL_AVAILABLE']
