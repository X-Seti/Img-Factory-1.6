#!/usr/bin/env python3
#this belongs in root /launch.py - Version: 1
# X-Seti - June22 2026 - IMG Factory 1.6 - Unified Launcher

"""
Unified Launcher - Single entry point for IMG Factory and all standalone
workshop tools. Replaces launch_imgfactory.py, launch_col_workshop.py,
launch_txd_workshop.py, launch_model_workshop.py, launch_dp5_workshop.py
and launch_ai_workshop.py.

Usage:
    python3 launch.py        Show menu, pick a tool
    python3 launch.py 1      Launch IMG Factory directly
    python3 launch.py 2      Launch TXD Workshop directly
    python3 launch.py 3      Launch COL Workshop directly
    python3 launch.py 4      Launch Model Workshop directly
    python3 launch.py 5      Launch DP5 Workshop directly
    python3 launch.py 6      Launch AI Workshop directly

On WSL2:
    Auto-sets QT_QPA_PLATFORM=xcb and DISPLAY if not already set.
    Windows 11: WSLg provides display automatically.
    Windows 10: install VcXsrv and set DISPLAY=:0.0 before launching.

First time setup:
    chmod +x setup_imgfactory.sh
    ./setup_imgfactory.sh
"""

import sys
import os
from pathlib import Path

##Methods list -
# configure_display
# launch_ai_workshop
# launch_col_workshop
# launch_dp5_workshop
# launch_imgfactory
# launch_model_workshop
# launch_txd_workshop
# print_menu
# run_standalone
# main

root_dir = Path(__file__).parent.resolve()
if str(root_dir) not in sys.path:
    sys.path.insert(0, str(root_dir))

TOOLS = {
    "1": ("IMG Factory 1.6", "launch_imgfactory"),
    "2": ("TXD Workshop", "launch_txd_workshop"),
    "3": ("COL Workshop", "launch_col_workshop"),
    "4": ("Model Workshop", "launch_model_workshop"),
    "5": ("DP5 Workshop", "launch_dp5_workshop"),
    "6": ("AI Workshop", "launch_ai_workshop"),
}


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


def run_standalone(module_path, class_name, title): #vers 1
    """Import a workshop module and run its class in a standalone QApplication."""
    from PyQt6.QtWidgets import QApplication
    import importlib

    module = importlib.import_module(module_path)
    if hasattr(module, 'main'):
        return module.main()

    app = QApplication(sys.argv)
    workshop_class = getattr(module, class_name)
    workshop = workshop_class()
    workshop.setWindowTitle(title)
    workshop.resize(1200, 800)
    workshop.show()
    return app.exec()


def launch_imgfactory(): #vers 1
    from apps.core.dependency_check import run_startup_check
    if not run_startup_check():
        return 1
    from apps.components.Img_Factory import imgfactory
    return imgfactory.main()


def launch_txd_workshop(): #vers 1
    return run_standalone(
        "apps.components.Txd_Editor.txd_workshop", "TXDWorkshop",
        "TXD Workshop - Standalone")


def launch_col_workshop(): #vers 1
    return run_standalone(
        "apps.components.Col_Editor.col_workshop", "COLWorkshop",
        "COL Workshop - Standalone")


def launch_model_workshop(): #vers 1
    return run_standalone(
        "apps.components.Model_Editor.model_workshop", "ModelWorkshop",
        "Model Workshop - Standalone")


def launch_dp5_workshop(): #vers 1
    return run_standalone(
        "apps.components.DP5_Workshop.dp5_workshop", "DP5Workshop",
        "DP5 Workshop - Standalone")


def launch_ai_workshop(): #vers 1
    return run_standalone(
        "apps.components.Ai_Workshop.ai_workshop", "AIWorkshop",
        "AI Workshop - Standalone")


LAUNCHERS = {
    "launch_imgfactory": launch_imgfactory,
    "launch_txd_workshop": launch_txd_workshop,
    "launch_col_workshop": launch_col_workshop,
    "launch_model_workshop": launch_model_workshop,
    "launch_dp5_workshop": launch_dp5_workshop,
    "launch_ai_workshop": launch_ai_workshop,
}


def print_menu(): #vers 1
    print("")
    print("=== IMG Factory 1.6 - Launcher ===")
    print("")
    for key, (name, _) in TOOLS.items():
        print(f"  {key}. Launch {name}")
    print("")


def main(): #vers 1
    configure_display()

    if len(sys.argv) > 1:
        choice = sys.argv[1].strip()
    else:
        print_menu()
        choice = input("Choose a tool [1-6]: ").strip()

    if choice not in TOOLS:
        print(f"ERROR: Invalid choice '{choice}' - expected 1-6")
        return 1

    name, func_name = TOOLS[choice]
    print(f"Starting {name}...")

    try:
        return LAUNCHERS[func_name]()
    except ImportError as e:
        print(f"ERROR: Failed to import {name}: {e}")
        print(f"  Root: {root_dir}")
        print("")
        print("Have you run setup?")
        print("  chmod +x setup_imgfactory.sh && ./setup_imgfactory.sh")
        return 1
    except Exception as e:
        print(f"ERROR: Failed to start {name}: {e}")
        import traceback
        traceback.print_exc()
        return 1


if __name__ == "__main__":
    sys.exit(main())
