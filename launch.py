#!/usr/bin/env python3
#this belongs in root /launch.py - Version: 2
# X-Seti - June28 2026 - IMG Factory 1.6 - Launcher

"""
Launcher - Single entry point for IMG Factory. Workshop tools are reached
from the Intro tab inside IMG Factory, not from here.

Usage:
    python3 launch.py

On WSL2:
    Auto-sets QT_QPA_PLATFORM=xcb and DISPLAY if not already set.
    Windows 11: WSLg provides display automatically.
    Windows 10: install VcXsrv and set DISPLAY=:0.0 before launching.
"""

import sys
import os
from pathlib import Path

##Methods list -
# configure_display
# launch_imgfactory
# main

root_dir = Path(__file__).parent.resolve()
if str(root_dir) not in sys.path:
    sys.path.insert(0, str(root_dir))


def configure_display(): #vers 2
    """Set Qt platform and DISPLAY for WSL2 and Wayland environments.

    Also forces the desktop OpenGL RHI backend unconditionally (Aug 1
    2026, per Keith's own terminal log showing "QRhiGles2: Failed to
    create QRhi" followed by repeated "QOpenGLWidget: Failed to
    create context", producing a totally blank Map Workshop window)
    - several individual workshop modules (map_workshop.py included)
    already set os.environ['QSG_RHI_BACKEND'] = 'opengl' at their own
    module level, intending exactly this, but that only actually
    works if the module happens to be imported before any QApplication
    is constructed. Every one of those workshops is opened as a tab
    from inside an already-running IMG Factory - by the time a
    workshop module is actually imported (when the user opens that
    tab), IMG Factory's own QApplication already exists and has
    already selected and locked in its RHI backend, so the workshop's
    own env var setting is always too late to matter. This is the one
    place in the entire process that's guaranteed to run before any
    QApplication anywhere - launch.py's own main() calls this before
    launch_imgfactory() does anything else, including its own imports."""
    if 'QSG_RHI_BACKEND' not in os.environ:
        os.environ['QSG_RHI_BACKEND'] = 'opengl'

    is_wsl = False
    try:
        with open('/proc/version', 'r') as f:
            is_wsl = 'microsoft' in f.read().lower()
    except OSError:
        pass

    if is_wsl:
        if 'QT_QPA_PLATFORM' not in os.environ:
            os.environ['QT_QPA_PLATFORM'] = 'xcb'
        if 'DISPLAY' not in os.environ:
            os.environ['DISPLAY'] = ':0'
        if 'LIBGL_ALWAYS_INDIRECT' not in os.environ:
            os.environ['LIBGL_ALWAYS_INDIRECT'] = '1'
        print("[launch] WSL2 detected - QT_QPA_PLATFORM=xcb, DISPLAY=:0")
    elif os.environ.get('WAYLAND_DISPLAY') and 'QT_QPA_PLATFORM' not in os.environ:
        os.environ['QT_QPA_PLATFORM'] = 'xcb'


def launch_imgfactory(): #vers 1
    from apps.core.dependency_check import run_startup_check
    if not run_startup_check():
        return 1
    from apps.components.Img_Factory import imgfactory
    return imgfactory.main()


def main(): #vers 1
    configure_display()
    try:
        return launch_imgfactory()
    except ImportError as e:
        print(f"ERROR: Failed to import IMG Factory: {e}")
        print(f"  Root: {root_dir}")
        print("")
        print("Have you run setup?")
        print("  chmod +x setup_imgfactory.sh && ./setup_imgfactory.sh")
        return 1
    except Exception as e:
        print(f"ERROR: Failed to start IMG Factory: {e}")
        import traceback
        traceback.print_exc()
        return 1


if __name__ == "__main__":
    sys.exit(main())
