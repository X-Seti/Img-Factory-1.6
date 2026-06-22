#this belongs in core/dependency_check.py - Version: 1
# X-Seti - June22 2026 - IMG Factory 1.6 - Dependency Check

"""
Dependency Check - Verifies required Python modules are installed and
checks WSL display reachability before the main app is imported.
"""

import os
import sys
import importlib

##Methods list -
# check_dependencies
# check_wsl_display
# is_wsl
# run_startup_check

REQUIRED_MODULES = [
    ("PyQt6", "PyQt6"),
    ("PIL", "Pillow"),
    ("numpy", "numpy"),
    ("send2trash", "send2trash"),
]


def is_wsl(): #vers 1
    """Return True if running inside WSL."""
    try:
        with open('/proc/version', 'r') as f:
            return 'microsoft' in f.read().lower()
    except OSError:
        return False


def check_dependencies(): #vers 1
    """Check required Python modules can be imported.
    Returns list of missing pip package names. Empty list = all OK."""
    missing = []
    for import_name, pip_name in REQUIRED_MODULES:
        try:
            importlib.import_module(import_name)
        except ImportError:
            missing.append(pip_name)
    return missing


def check_wsl_display(): #vers 1
    """On WSL, check DISPLAY is set and the X socket is reachable.
    Returns (ok, message)."""
    if not is_wsl():
        return True, ""

    display = os.environ.get('DISPLAY', '')
    if not display:
        return False, "DISPLAY is not set"

    # WSLg socket lives at /tmp/.X11-unix/X<n>
    socket_path = "/tmp/.X11-unix"
    if not os.path.isdir(socket_path) or not os.listdir(socket_path):
        return False, (
            "No X socket found at /tmp/.X11-unix — WSLg may not be running.\n"
            "    Try: wsl --shutdown (from Windows PowerShell), then reopen your terminal.\n"
            "    On Windows 10, start VcXsrv with 'Disable access control' checked."
        )

    return True, ""


def run_startup_check(): #vers 1
    """Run all startup checks. Prints errors and returns False if anything
    is missing, so the caller can exit cleanly instead of hanging or
    crashing with a raw traceback."""
    missing = check_dependencies()
    if missing:
        print("ERROR: Missing required Python modules:")
        for name in missing:
            print(f"  - {name}")
        print("")
        print("Install with:")
        print(f"  pip install {' '.join(missing)}")
        print("")
        print("Or run the full setup script:")
        print("  chmod +x setup_imgfactory.sh && ./setup_imgfactory.sh")
        return False

    if is_wsl():
        ok, message = check_wsl_display()
        if not ok:
            print(f"ERROR: WSL display not reachable — {message}")
            return False

    return True
