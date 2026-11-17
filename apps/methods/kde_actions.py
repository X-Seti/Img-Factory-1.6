#this belongs in apps/methods/kde_actions.py - Version: 1
# X-Seti - November 17 2026 - TC Workshop - KDE Actions.

"""
KDE Actions - Dolphin style right click entries: Open With, service menus, copy location, duplicate, wastebin.
"""

##Methods list -
# _apps_for
# _desktop
# _exec_run
# _matches
# _service_actions
# extend_menu

import configparser
import os
import shutil
import subprocess

from PyQt6.QtCore import QMimeDatabase, QProcess, QUrl
from PyQt6.QtGui import QAction, QGuiApplication, QIcon
from PyQt6.QtWidgets import QInputDialog, QMenu, QMessageBox

from apps.methods import image_actions

_HOME = os.path.expanduser("~")
_DATA = [os.path.join(_HOME, ".local", "share")] + [d for d in (os.environ.get("XDG_DATA_DIRS") or "/usr/local/share:/usr/share").split(":") if d]
_SERVICE_DIRS = [os.path.join(d, sub) for d in _DATA for sub in ("kio/servicemenus", "kservices5/ServiceMenus", "kservices6/ServiceMenus")]
_db = QMimeDatabase()


def _desktop(path: str):  #vers 1
    """configparser of a .desktop file, or None."""
    cp = configparser.RawConfigParser(strict=False, interpolation=None)
    cp.optionxform = str
    try:
        cp.read(path, encoding="utf-8")
    except (configparser.Error, OSError, UnicodeDecodeError):
        return None
    return cp if cp.has_section("Desktop Entry") else None


def _matches(tokens: list, paths: list) -> bool:  #vers 1
    """True when every path fits one of the mime tokens."""
    for p in paths:
        mt = _db.mimeTypeForFile(p)
        isdir = os.path.isdir(p)
        ok = False
        for t in tokens:
            if t in ("all/all", "all/*") or (t == "all/allfiles" and not isdir) or t == mt.name():
                ok = True
            elif t.endswith("/*") and mt.name().startswith(t[:-1]):
                ok = True
            elif t and mt.inherits(t):
                ok = True
            if ok:
                break
        if not ok:
            return False
    return True


def _exec_run(cmd: str, paths: list, name: str = "", icon: str = ""):  #vers 1
    """Run a .desktop Exec line with file placeholders filled in."""
    import shlex
    try:
        parts = shlex.split(cmd)
    except ValueError:
        return
    per_file = any(x in ("%f", "%u") for x in parts)
    runs = [[p] for p in paths] if per_file else [paths]
    for group in runs:
        out = []
        for a in parts:
            if a in ("%F", "%U"):
                out += group if a == "%F" else [QUrl.fromLocalFile(g).toString() for g in group]
            elif a == "%f":
                out.append(group[0])
            elif a == "%u":
                out.append(QUrl.fromLocalFile(group[0]).toString())
            elif a == "%c":
                out.append(name)
            elif a == "%i":
                out += ["--icon", icon] if icon else []
            elif a in ("%k", "%d", "%D", "%n", "%N", "%v", "%m"):
                continue
            else:
                out.append(a.replace("%%", "%"))
        if out:
            QProcess.startDetached(out[0], out[1:], os.path.dirname(group[0]) if group else "")


def _service_actions(paths: list) -> list:  #vers 1
    """[(submenu, name, icon, exec)] from KDE service menu files that fit the paths."""
    seen, out = set(), []
    for d in _SERVICE_DIRS:
        if not os.path.isdir(d):
            continue
        for fn in sorted(os.listdir(d)):
            if not fn.endswith(".desktop") or fn in seen:
                continue
            cp = _desktop(os.path.join(d, fn))
            if cp is None:
                continue
            seen.add(fn)
            ent = cp["Desktop Entry"]
            if ent.get("Hidden", "").lower() == "true" or ent.get("NoDisplay", "").lower() == "true":
                continue
            tokens = [t for t in (ent.get("MimeType") or ent.get("X-KDE-ServiceTypes") or "").replace(",", ";").split(";") if t and "/" in t]
            if tokens and not _matches(tokens, paths):
                continue
            if not tokens and ent.get("X-KDE-ServiceTypes", "").find("KonqPopupMenu") < 0:
                continue
            try:
                need = int(ent.get("X-KDE-RequiredNumberOfUrls", ent.get("X-KDE-MinNumberOfUrls", "0")).split(",")[0] or 0)
            except ValueError:
                need = 0
            if len(paths) < need:
                continue
            sub = ent.get("X-KDE-Submenu", "")
            for key in [a for a in ent.get("Actions", "").split(";") if a]:
                sec = f"Desktop Action {key}"
                if cp.has_section(sec) and cp[sec].get("Exec"):
                    s = cp[sec]
                    out.append((sub, s.get("Name", key), s.get("Icon", ""), s["Exec"]))
    return out


def _apps_for(mime: str) -> list:  #vers 1
    """[(name, icon, exec)] of applications that open a mime type."""
    ids, res = [], []
    for d in _DATA:
        for fn in ("mimeinfo.cache", "mimeapps.list"):
            p = os.path.join(d, "applications", fn)
            if os.path.isfile(p):
                try:
                    for line in open(p, encoding="utf-8", errors="replace"):
                        if line.startswith(mime + "="):
                            ids += [x for x in line.split("=", 1)[1].strip().split(";") if x]
                except OSError:
                    pass
    for i in dict.fromkeys(ids):
        for d in _DATA:
            p = os.path.join(d, "applications", i)
            if os.path.isfile(p):
                cp = _desktop(p)
                if cp and cp["Desktop Entry"].get("NoDisplay", "").lower() != "true":
                    e = cp["Desktop Entry"]
                    res.append((e.get("Name", i), e.get("Icon", ""), e.get("Exec", "")))
                break
    return res[:24]


def extend_menu(menu: QMenu, paths: list, refresh=None, parent=None):  #vers 3
    """Add Dolphin style entries for the selected paths to a context menu."""
    paths = [p for p in paths if p and os.path.exists(p)]
    if not paths:
        return
    menu.addSeparator()
    mime = _db.mimeTypeForFile(paths[0]).name()
    apps = _apps_for(mime)
    if apps:
        ow = menu.addMenu("Open With")
        for name, icon, cmd in apps:
            a = ow.addAction(QIcon.fromTheme(icon), name)
            a.triggered.connect(lambda _c=False, c=cmd, n=name, i=icon: _exec_run(c, paths, n, i))
    loc = menu.addAction("Copy Location")
    loc.triggered.connect(lambda: QGuiApplication.clipboard().setText("\n".join(paths)))
    dup = menu.addAction("Duplicate Here")
    dup.triggered.connect(lambda: _duplicate(paths, refresh, parent))
    mv = menu.addAction("Move to New Folder...")
    mv.triggered.connect(lambda: _to_new_folder(paths, refresh, parent))
    up = menu.addAction("Move to ../")
    up.triggered.connect(lambda: image_actions.move_up(paths, refresh, parent))
    imgs = [p for p in paths if image_actions.is_image(p)]
    if imgs:
        rm = menu.addMenu("Rotate / Flip")
        for label, how in (("Rotate Clockwise", "cw"), ("Rotate Counterclockwise", "ccw"), ("Rotate 180", "180"),
                           ("Flip Horizontal", "flipx"), ("Flip Vertical", "flipy")):
            rm.addAction(label).triggered.connect(lambda _c=False, h=how: image_actions.rotate_images(imgs, h, refresh, parent))
        cm = menu.addMenu("Convert To")
        for label, ext in image_actions.FORMATS.items():
            cm.addAction(label).triggered.connect(lambda _c=False, e=ext: image_actions.convert_images(imgs, e, refresh, parent))
        if len(imgs) == 1:
            menu.addAction("Set as Desktop Background").triggered.connect(lambda: image_actions.set_wallpaper(imgs[0], parent))
    tr = menu.addAction(QIcon.fromTheme("user-trash"), "Move to Wastebin")
    tr.triggered.connect(lambda: _wastebin(paths, refresh, parent))
    acts = _service_actions(paths)
    if acts:
        menu.addSeparator()
        subs = {}
        for sub, name, icon, cmd in acts:
            if sub and sub not in subs:
                subs[sub] = menu.addMenu(sub)
            tgt = subs[sub] if sub else menu
            a = QAction(QIcon.fromTheme(icon), name, tgt)
            a.triggered.connect(lambda _c=False, c=cmd, n=name, i=icon: _exec_run(c, paths, n, i))
            tgt.addAction(a)


def _duplicate(paths: list, refresh, parent):  #vers 1
    """Copy each item next to itself as 'name (copy)'."""
    for p in paths:
        base, ext = os.path.splitext(p.rstrip(os.sep)) if os.path.isfile(p) else (p.rstrip(os.sep), "")
        n, tgt = 1, f"{base} (copy){ext}"
        while os.path.exists(tgt):
            n += 1
            tgt = f"{base} (copy {n}){ext}"
        try:
            shutil.copytree(p, tgt) if os.path.isdir(p) else shutil.copy2(p, tgt)
        except OSError as e:
            QMessageBox.warning(parent, "Duplicate", str(e))
            break
    if refresh:
        refresh()


def _to_new_folder(paths: list, refresh, parent):  #vers 1
    """Make a folder beside the first item and move the selection into it."""
    name, ok = QInputDialog.getText(parent, "Move to New Folder", "Folder name")
    if not ok or not name.strip():
        return
    dest = os.path.join(os.path.dirname(paths[0].rstrip(os.sep)), name.strip())
    try:
        os.makedirs(dest, exist_ok=True)
        for p in paths:
            shutil.move(p, os.path.join(dest, os.path.basename(p.rstrip(os.sep))))
    except (OSError, shutil.Error) as e:
        QMessageBox.warning(parent, "Move", str(e))
    if refresh:
        refresh()


def _wastebin(paths: list, refresh, parent):  #vers 1
    """Send items to the desktop wastebin (gio trash)."""
    if QMessageBox.question(parent, "Wastebin", f"Move {len(paths)} item(s) to the wastebin?") != QMessageBox.StandardButton.Yes:
        return
    exe = shutil.which("gio")
    if exe is None:
        QMessageBox.warning(parent, "Wastebin", "gio is not installed.")
        return
    r = subprocess.run([exe, "trash", "--"] + paths, capture_output=True, text=True)
    if r.returncode != 0:
        QMessageBox.warning(parent, "Wastebin", r.stderr.strip() or "Could not trash.")
    if refresh:
        refresh()
