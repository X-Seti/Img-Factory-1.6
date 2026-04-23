#!/usr/bin/env python3
"""
X-Seti - Apr 2026 - IMG Factory 1.6 - Root Launcher
#this belongs in root /launch_imgfactory.py - version 2

Usage:
    python3 launch_imgfactory.py

On WSL2 (Windows Subsystem for Linux):
    The launcher auto-sets QT_QPA_PLATFORM=xcb and DISPLAY if not already set.
    Windows 11: WSLg provides display automatically.
    Windows 10: install VcXsrv and set DISPLAY=:0.0 before launching.

First time setup:
    chmod +x setup_imgfactory.sh
    ./setup_imgfactory.sh
"""

import sys
import os
from pathlib import Path

# ── WSL / display setup ────────────────────────────────────────────────────────
def _configure_display():
    """Set Qt platform and DISPLAY for WSL2 and Wayland environments."""
    # Detect WSL
    is_wsl = False
    try:
        with open('/proc/version', 'r') as f:
            is_wsl = 'microsoft' in f.read().lower()
    except OSError:
        pass

    if is_wsl:
        # WSL must use XCB — Wayland is not available inside WSL
        if 'QT_QPA_PLATFORM' not in os.environ:
            os.environ['QT_QPA_PLATFORM'] = 'xcb'
        if 'DISPLAY' not in os.environ:
            os.environ['DISPLAY'] = ':0'
        if 'LIBGL_ALWAYS_INDIRECT' not in os.environ:
            os.environ['LIBGL_ALWAYS_INDIRECT'] = '1'
        print("[launch] WSL2 detected — QT_QPA_PLATFORM=xcb, DISPLAY=:0")

    # On native Wayland (KDE/GNOME), fall back to XCB if PyQt6 Wayland plugin absent
    elif os.environ.get('WAYLAND_DISPLAY') and 'QT_QPA_PLATFORM' not in os.environ:
        os.environ['QT_QPA_PLATFORM'] = 'xcb'

_configure_display()

# ── Path setup ─────────────────────────────────────────────────────────────────
root_dir = Path(__file__).parent.resolve()
if str(root_dir) not in sys.path:
    sys.path.insert(0, str(root_dir))

# ── Launch ─────────────────────────────────────────────────────────────────────
if __name__ == '__main__':
    try:
        from apps.components.Img_Factory import imgfactory
        if hasattr(imgfactory, 'main'):
            sys.exit(imgfactory.main())
        else:
            print("ERROR: imgfactory.py has no main() function")
            sys.exit(1)
    except ImportError as e:
        print(f"ERROR: Failed to import imgfactory: {e}")
        print(f"  Root:     {root_dir}")
        print(f"  Expected: {root_dir}/apps/components/Img_Factory/imgfactory.py")
        print("")
        print("Have you run setup?")
        print("  chmod +x setup_imgfactory.sh && ./setup_imgfactory.sh")
        sys.exit(1)
    except Exception as e:
        print(f"ERROR: Failed to start IMG Factory: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)
