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


def configure_display(): #vers 1
    """Set Qt platform and DISPLAY for WSL2 and Wayland environments."""
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
