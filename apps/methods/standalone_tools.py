#this belongs in methods/standalone_tools.py - Version: 1
# X-Seti - November 02 2026 - IMG Factory 1.6 - Standalone workshop exes

"""
Finds standalone workshop builds dropped into sibling repo folders
(GitHub/Txd-Workshop/Txd_Workshop.exe ...) and starts them.
"""

##Methods list -
# app_root
# fill_standalone_menu
# find_standalone
# launch_standalone

import os
import subprocess
import sys


def app_root() -> str: #vers 1
    """Folder holding IMG Factory: the exe folder, or the repo root."""
    if getattr(sys, 'frozen', False):
        return os.path.dirname(os.path.abspath(sys.executable))
    return os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


def find_standalone() -> list: #vers 1
    """(title, path) of each program built into a sibling folder."""
    here = app_root()
    parent = os.path.dirname(here)
    out = []
    for d in sorted(os.listdir(parent)):
        folder = os.path.join(parent, d)
        if folder == here or not os.path.isdir(folder):
            continue
        for name in sorted(os.listdir(folder)):
            path = os.path.join(folder, name)
            if sys.platform == 'darwin':
                ok = name.endswith('.app') and os.path.isdir(path)
            elif sys.platform == 'win32':
                ok = name.lower().endswith('.exe') and os.path.isdir(os.path.join(folder, '_internal'))
            else:
                ok = ('.' not in name and os.path.isfile(path) and os.access(path, os.X_OK)
                      and os.path.isdir(os.path.join(folder, '_internal')))
            if ok:
                title = os.path.splitext(name)[0].replace('_', ' ')
                out.append((title, path))
    return out


def launch_standalone(path: str): #vers 1
    """Start a standalone program in its own folder, detached."""
    if sys.platform == 'darwin':
        subprocess.Popen(['open', path])
    else:
        subprocess.Popen([path], cwd=os.path.dirname(path), start_new_session=True)


def fill_standalone_menu(menu): #vers 1
    """Rebuild menu with the standalone programs found."""
    menu.clear()
    found = find_standalone()
    if not found:
        a = menu.addAction("None found (build with --drop)")
        a.setEnabled(False)
        return
    for title, path in found:
        menu.addAction(title).triggered.connect(lambda _c, p=path: launch_standalone(p))
