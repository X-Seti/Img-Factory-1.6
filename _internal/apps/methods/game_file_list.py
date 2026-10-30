#this belongs in apps/methods/game_file_list.py - Version: 1
# X-Seti - October 14 2026 - IMG Factory 1.6 - Game file list for workshop side panels

"""
Every file of a type in the game folder: loose files and entries inside IMGs.
Used by the COL and TXD workshop file panels (exclusions, show excluded, refresh).
"""

##Methods list -
# fill_game_list
# game_img
# game_list_excludes
# game_list_menu
# scan_game_files
# set_game_list_excludes

import json
import os

from PyQt6.QtCore import Qt


def game_img(owner, img_path): #vers 1
    """Opened IMGFile for img_path, cached on owner."""
    from apps.methods.img_core_classes import IMGFile
    cache = owner.__dict__.setdefault('_game_list_imgs', {})
    img = cache.get(img_path)
    if img is None:
        img = IMGFile(img_path)
        if not img.open():
            raise IOError(f"Could not open {img_path}")
        cache[img_path] = img
    return img


def scan_game_files(owner, root, exts): #vers 1
    """[{'name', 'rel', 'kind': 'file'|'img', 'path'|'img'}] for every exts file under root."""
    from PyQt6.QtWidgets import QApplication
    found = []
    for d, dirs, files in os.walk(root):
        dirs.sort()
        for f in sorted(files):
            low = f.lower()
            full = os.path.join(d, f)
            rel = os.path.relpath(full, root)
            if low.endswith(exts):
                found.append({'name': f, 'rel': rel, 'kind': 'file', 'path': full})
            elif low.endswith('.img'):
                try:
                    img = game_img(owner, full)
                except IOError:
                    continue
                for e in img.entries:
                    if e.name.lower().endswith(exts):
                        found.append({'name': e.name, 'rel': f"{rel}/{e.name}", 'kind': 'img',
                                      'img': full})
        QApplication.processEvents()
    return found


def game_list_excludes(cfg_path): #vers 1
    """Excluded paths (relative to the game root) from a workshop config file."""
    data = json.loads(cfg_path.read_text()) if cfg_path.exists() else {}
    return list(data.get('game_list_excludes', []))


def set_game_list_excludes(cfg_path, excludes): #vers 1
    data = json.loads(cfg_path.read_text()) if cfg_path.exists() else {}
    data['game_list_excludes'] = sorted(set(excludes))
    cfg_path.parent.mkdir(parents=True, exist_ok=True)
    cfg_path.write_text(json.dumps(data, indent=2))


def fill_game_list(lw, items, excludes, show_excluded): #vers 1
    """Put scanned items in a QListWidget; returns (shown, excluded)."""
    from PyQt6.QtWidgets import QListWidgetItem
    lw.clear()
    shown = hidden_n = 0
    for data in items:
        rel = data['rel']
        hidden = any(rel == x or rel.startswith(x.rstrip('/') + '/') for x in excludes)
        hidden_n += hidden
        if hidden and not show_excluded:
            continue
        item = QListWidgetItem(data['name'])
        item.setData(Qt.ItemDataRole.UserRole, data)
        item.setToolTip(rel + ("  (excluded)" if hidden else ""))
        if hidden:
            item.setForeground(Qt.GlobalColor.gray)
        lw.addItem(item)
        shown += 1
    return shown, hidden_n


def game_list_menu(owner, lw, pos, cfg_path, reload_fn): #vers 1
    """Right-click: exclude file / folder / IMG, include again, show excluded, clear, refresh.
    reload_fn(rescan) refills the list."""
    from PyQt6.QtWidgets import QMenu
    item = lw.itemAt(pos)
    data = item.data(Qt.ItemDataRole.UserRole) if item else None
    excl = game_list_excludes(cfg_path)
    menu = QMenu(owner)

    def setx(new):
        set_game_list_excludes(cfg_path, new)
        reload_fn(False)
    if isinstance(data, dict) and 'rel' in data:
        rel = data['rel']
        parent = rel.rsplit('/', 1)[0] if data['kind'] == 'img' else os.path.dirname(rel)
        if rel in excl or parent in excl:
            menu.addAction("Include again", lambda: setx([x for x in excl if x not in (rel, parent)]))
        else:
            menu.addAction("Exclude this file", lambda: setx(excl + [rel]))
            if parent:
                menu.addAction("Exclude this IMG" if data['kind'] == 'img' else "Exclude this folder",
                               lambda: setx(excl + [parent]))
    menu.addSeparator()
    act = menu.addAction("Show excluded")
    act.setCheckable(True)
    act.setChecked(getattr(owner, '_game_list_show_excluded', False))
    act.toggled.connect(lambda on: (setattr(owner, '_game_list_show_excluded', on), reload_fn(False)))
    if excl:
        menu.addAction("Clear exclusions", lambda: setx([]))
    menu.addAction("Refresh", lambda: reload_fn(True))
    menu.exec(lw.mapToGlobal(pos))
