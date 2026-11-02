#!/usr/bin/env python3
#this belongs in tools/buildscript.py - Version: 7
# X-Seti - October 29 2026 - IMG Factory 1.6 - build for this computer's platform

"""
Builds IMG Factory 1.6 for the platform it runs on (PyInstaller cannot build
for another one): Windows x64 / arm64, Linux x64 / arm64, macOS arm64 /
Intel. GitHub Actions (.github/workflows/build.yml) runs it on each.

    python tools/buildscript.py            install deps, build, test, package
    python tools/buildscript.py --ci       also compile PyInstaller's bootloader, make the installer
    python tools/buildscript.py --installer  make the installer (local run)
    python tools/buildscript.py --no-install
    python tools/buildscript.py --drop     also copy the program and _internal/ into the repo root
    python tools/buildscript.py --name     print the app name
    --core           Img_Factory: IMG Factory with its core components only
    --full           Img_Factory_Full: every workshop in apps/components (default)
    --<workshop>     one workshop standalone, e.g. --txd_workshop, --col_workshop
    --list           print the workshop names
    core / full carry the core pack: every library any workshop imports, so
    workshops added later (--addon, or copied in by hand) run.
    (--variant <name> also works). tools/settings/ goes beside the exe as settings/.
    --addon --<workshop>     add-on for an installed IMG Factory core (Setup.exe on
                             Windows, .tar.gz / .zip with install_addon.sh elsewhere)
    --new Foo_Workshop       placeholder workshop: apps/components/Foo_Workshop/ + depends/
    --new-repo Foo_Workshop  sibling standalone repo ../Foo-Workshop/ (makes the workshop first
                             when missing; Txd-Workshop is the template for buildscript / build.yml)

Runs inside its own .venv (made on first run), so the system Python
(PEP 668 / Arch, Debian, Fedora) is never changed.

Output: <os>_<arch>_dist/<APP>/ (folder build: the program plus its _internal/ files)
and <os>_<arch>_dist/<APP>_<Platform>_<arch>.zip (.tar.gz on Linux, with install.sh).
Installers: Windows <os>_<arch>_dist/<APP>_<Platform>_<arch>_Setup.exe (Inno Setup 6),
macOS <os>_<arch>_dist/<APP>_<Platform>_<arch>.dmg. Linux: install.sh in the .tar.gz.
"""

##Methods list -
# _addon
# _addon_unix
# _addon_windows
# _app_modules
# _arg_value
# _component_ok
# _copy_tree
# _manifest
# _apps_data
# _drop
# _icon
# _install
# _installer
# _linux_scripts
# _mac_dmg
# _new_file
# _new_repo
# _new_workshop
# _pack_modules
# _package
# _platform_tag
# _pyinstaller
# _run
# _set_variant
# _smoke_test
# _use_venv
# _version_file
# _windows_setup
# _workshop_launcher
# _workshops
# main

import os
import platform
import re
import shutil
import subprocess
import sys
import time
import uuid
from collections import deque

HERE = os.path.dirname(os.path.abspath(__file__))          # tools/
ROOT = os.path.dirname(HERE)

# <os>_<arch>_dist: wins_x86_64_dist, linux_arm64_dist, macos_x86_64_dist ...
_OS = {'win32': 'wins', 'darwin': 'macos'}.get(sys.platform, 'linux')
_M = platform.machine().lower()
_ARCH = {'amd64': 'x86_64', 'x64': 'x86_64', 'aarch64': 'arm64'}.get(_M, _M)
DIST = os.path.join(ROOT, f'{_OS}_{_ARCH}_dist')

# ---- this repo -----------------------------------------------------------------------------------
CONFIG = {
    'name': 'Img_Factory_Full',
    'title': 'Img Factory 1.6 Full',
    'launcher': 'launch.py',
    'icon': 'get_app_icon',
    'required': ['PyQt6', 'numpy', 'send2trash', 'requests', 'pycryptodome'],   # pycryptodome: GTA IV IMGs
    'optional': ['PyOpenGL', 'scipy', 'python-lzo', 'mss', 'pygame', 'psutil', 'cairosvg', 'imageio',
                 'opencv-python', 'pyaudio', 'paramiko'],
    'hidden': ['PyQt6.QtSvg', 'PyQt6.QtOpenGL', 'PyQt6.QtOpenGLWidgets', 'PyQt6.QtNetwork',
               'OpenGL.arrays.numpymodule', 'pygame._sdl2.controller', 'Crypto.Cipher.AES'],
    'bundle_files': ['appfactory.settings.json', 'Legal.md'],
    'beside_exe': ['icons'],
    'components': None,                                  # None: every folder in apps/components
}
VARIANTS = {   # --variant: name, title, components bundled (None = all)
    'core': ('Img_Factory', 'Img Factory 1.6',
             {'Img_Factory', 'Dat_Browser', 'File_Editor', 'Img_Creator', 'Img_Browser',
              'Project_Manager', 'templates'}),
    'full': ('Img_Factory_Full', 'Img Factory 1.6 Full', None),
}
# --------------------------------------------------------------------------------------------------

SKIP_DIRS = {'__pycache__', '_to_delete', '.git'}
TAIL = deque(maxlen=40)                                  # last output lines, for error reports


def _run(cmd, env=None, check=True): #vers 2
    """Run a command, echoed; output streamed, last lines kept in TAIL."""
    print('>', ' '.join(cmd), flush=True)
    p = subprocess.Popen(cmd, cwd=ROOT, env=env, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                         text=True, errors='replace')
    for line in p.stdout:
        sys.stdout.write(line)
        TAIL.append(line.rstrip())
    p.wait()
    if check and p.returncode:
        raise subprocess.CalledProcessError(p.returncode, cmd)
    return p


def _install(ci: bool): #vers 1
    """Required packages (stop on failure), optional ones (skipped when no wheel), PyInstaller."""
    pip = [sys.executable, '-m', 'pip', 'install', '--upgrade']
    _run(pip + ['pip'])
    _run(pip + CONFIG['required'] + ['Pillow'])           # Pillow turns the PNG icon into .ico / .icns
    for pkg in CONFIG['optional']:
        if _run(pip + [pkg], check=False).returncode:
            print(f'  optional {pkg}: not available on this platform, skipped')
    if ci:                                               # own bootloader: fewer antivirus false alarms
        env = dict(os.environ, PYINSTALLER_COMPILE_BOOTLOADER='1')
        _run(pip + ['--no-binary', 'pyinstaller', '--no-cache-dir', 'pyinstaller'], env=env)
    else:
        _run(pip + ['pyinstaller'])


def _platform_tag() -> str: #vers 1
    """Windows_x64, Linux_arm64, macOS_arm64 ..."""
    osname = {'win32': 'Windows', 'darwin': 'macOS'}.get(sys.platform, 'Linux')
    m = platform.machine().lower()
    arch = 'arm64' if m in ('arm64', 'aarch64') else 'x64' if m in ('x86_64', 'amd64') else m
    return f'{osname}_{arch}'


def _set_variant(): #vers 2
    """--core | --full | --<workshop> (or --variant <name>): app name, title, launcher, components."""
    shops = _workshops()
    if '--list' in sys.argv:
        print('\n'.join(['core', 'full'] + sorted(shops)))
        raise SystemExit(0)
    keys = [a[2:] for a in sys.argv[1:] if a.startswith('--') and (a[2:] in VARIANTS or a[2:] in shops)]
    if '--variant' in sys.argv:
        keys.append(sys.argv[sys.argv.index('--variant') + 1])
    key = keys[-1] if keys else 'full'
    if key in VARIANTS:
        CONFIG['name'], CONFIG['title'], CONFIG['components'] = VARIANTS[key]
        return
    if key not in shops:
        raise SystemExit(f'Unknown build {key!r}: core, full or one of: {", ".join(sorted(shops))}')
    folder, deps = shops[key]
    prefix = key.split('_')[0]
    CONFIG['name'] = '_'.join(w.capitalize() for w in key.split('_'))
    CONFIG['title'] = CONFIG['name'].replace('_', ' ')
    CONFIG['launcher'] = _workshop_launcher(folder, key)
    CONFIG['icon'] = [f'{key}_icon', f'{prefix}_workshop_icon', f'{prefix}_app_icon', f'get_{key}_icon']
    CONFIG['components'] = deps | {'templates'}


def _workshops() -> dict: #vers 1
    """module name -> (component folder, components it needs) for each runnable workshop."""
    cdir = os.path.join(ROOT, 'apps', 'components')
    uses = {}
    for d in os.listdir(cdir):
        if d in SKIP_DIRS or not os.path.isdir(os.path.join(cdir, d)):
            continue
        found = set()
        for base, dirs, files in os.walk(os.path.join(cdir, d)):
            dirs[:] = [x for x in dirs if x not in SKIP_DIRS]
            for name in files:
                if name.endswith('.py'):
                    with open(os.path.join(base, name), encoding='utf-8', errors='replace') as f:
                        found |= set(re.findall(r'apps\.components\.(\w+)', f.read()))
        uses[d] = found & set(os.listdir(cdir))
    out = {}
    for d in uses:
        if d == 'Img_Factory':
            continue
        for name in os.listdir(os.path.join(cdir, d)):
            path = os.path.join(cdir, d, name)
            if name.endswith('.py') and os.path.isfile(path):
                with open(path, encoding='utf-8', errors='replace') as f:
                    if re.search(r'^if __name__ == [\'"]__main__[\'"]', f.read(), re.M):
                        need, todo = set(), [d]
                        while todo:                      # components it imports, and theirs
                            c = todo.pop()
                            if c not in need:
                                need.add(c)
                                todo += list(uses.get(c, ()))
                        out[name[:-3]] = (d, need)
    return out


def _workshop_launcher(folder: str, module: str) -> str: #vers 1
    """build/launch_<module>.py: runs the workshop module as __main__."""
    out = os.path.join(ROOT, 'build', f'launch_{module}.py')
    os.makedirs(os.path.dirname(out), exist_ok=True)
    with open(out, 'w', encoding='utf-8') as f:
        f.write(f"""import os, runpy, sys
if not getattr(sys, 'frozen', False):
    sys.path.insert(0, {ROOT!r})
runpy.run_module('apps.components.{folder}.{module}', run_name='__main__', alter_sys=True)
""")
    return out


def _component_ok(base: str, d: str) -> bool: #vers 1
    """Folder d under base is bundled: not skipped, and an allowed component."""
    if d in SKIP_DIRS:
        return False
    comps = CONFIG['components']
    return comps is None or os.path.normpath(base) != os.path.join(ROOT, 'apps', 'components') or d in comps


def _manifest() -> str: #vers 1
    """build/installed.txt: bundled component folders (apps/methods/installed_tools.py reads it)."""
    cdir = os.path.join(ROOT, 'apps', 'components')
    names = sorted(d for d in os.listdir(cdir) if os.path.isdir(os.path.join(cdir, d)) and _component_ok(cdir, d))
    out = os.path.join(ROOT, 'build', 'installed.txt')
    os.makedirs(os.path.dirname(out), exist_ok=True)
    with open(out, 'w', encoding='utf-8') as f:
        f.write('\n'.join(names) + '\n')
    return out


def _app_modules() -> list: #vers 3
    """Every Python module under apps/ (linked folders followed; loaded at run time too, so all are bundled)."""
    out = []
    for base, dirs, files in os.walk(os.path.join(ROOT, 'apps'), followlinks=True):
        dirs[:] = [d for d in dirs if _component_ok(base, d)]
        for name in files:
            if name.endswith('.py'):
                rel = os.path.relpath(os.path.join(base, name), ROOT)[:-3].replace(os.sep, '.')
                out.append(rel[:-9] if rel.endswith('.__init__') else rel)
    return out


def _apps_data() -> list: #vers 3
    """'src{sep}dest' pairs: every non-Python file under apps/ (linked folders followed), root files,
    and the bundled components list."""
    out = [f'{_manifest()}{os.pathsep}{os.path.join("apps", "components")}']
    for base, dirs, files in os.walk(os.path.join(ROOT, 'apps'), followlinks=True):
        dirs[:] = [d for d in dirs if _component_ok(base, d)]
        for name in files:
            if not name.endswith(('.py', '.pyc', '.log')):
                out.append(f'{os.path.join(base, name)}{os.pathsep}{os.path.relpath(base, ROOT)}')
    for name in CONFIG['bundle_files']:
        out.append(f'{os.path.join(ROOT, name)}{os.pathsep}.')
    return out


def _pack_modules() -> list: #vers 1
    """Core pack: modules imported anywhere in apps/components that this Python has (core / full only)."""
    if CONFIG['name'] not in (v[0] for v in VARIANTS.values()):
        return []
    import importlib.util
    pat = re.compile(r'^[ \t]*(?:from[ \t]+([A-Za-z_][\w.]*)[ \t]+import|import[ \t]+([\w., \t]+))', re.M)
    names = set()
    for base, dirs, files in os.walk(os.path.join(ROOT, 'apps', 'components')):
        dirs[:] = [d for d in dirs if d not in SKIP_DIRS]
        for name in files:
            if name.endswith('.py'):
                with open(os.path.join(base, name), encoding='utf-8', errors='replace') as f:
                    for a, b in pat.findall(f.read()):
                        for n in ([a] if a else re.split(r'[ \t]*,[ \t]*', b)):
                            n = n.split(' as ')[0].strip()
                            if n and not n.startswith('apps'):
                                names.add(n)
    out = []
    for n in sorted(names):
        try:
            if importlib.util.find_spec(n) is not None:
                out.append(n)
        except (ImportError, ValueError, AttributeError):
            pass                                         # not installed here: left out
    return out


def _icon() -> str: #vers 3
    """App SVG icon rendered to build/<app>.png (PyInstaller converts it per platform), or ''."""
    if not CONFIG['icon']:
        return ''
    os.environ.setdefault('QT_QPA_PLATFORM', 'offscreen')
    sys.path.insert(0, ROOT)
    from PyQt6.QtWidgets import QApplication
    _icon.app = QApplication.instance() or QApplication([])      # kept alive while rendering
    import apps.methods.imgfactory_svg_icons as icons
    names = CONFIG['icon'] if isinstance(CONFIG['icon'], list) else [CONFIG['icon']]
    names = [n for n in names if hasattr(icons.SVGIconFactory, n) or hasattr(icons, n)]
    if not names:
        print('  no app icon for', CONFIG['name'])
        return ''
    fn = getattr(icons.SVGIconFactory, names[0], None) or getattr(icons, names[0])
    out = os.path.join(ROOT, 'build', CONFIG['name'] + '.png')
    os.makedirs(os.path.dirname(out), exist_ok=True)
    import inspect
    icon = fn(256) if inspect.signature(fn).parameters else fn()   # some take no size
    if not icon.pixmap(256, 256).toImage().save(out, 'PNG'):
        raise RuntimeError('Icon render failed')
    return out


def _pyinstaller(icon: str): #vers 3
    """Folder build (<os>_<arch>_dist/<APP>/<APP> + _internal/), windowed, no UPX."""
    args = [os.path.join(ROOT, CONFIG['launcher']), '--name', CONFIG['name'], '--noconfirm', '--clean',
            '--windowed', '--onedir', '--noupx', '--distpath', DIST,
            '--workpath', os.path.join(ROOT, 'build'), '--specpath', os.path.join(ROOT, 'build'),
            '--paths', ROOT, '--exclude-module', 'tkinter']
    pack = _pack_modules()
    for m in _app_modules() + CONFIG['hidden'] + pack:
        args += ['--hidden-import', m]
    if 'OpenGL' in pack:                                 # GL platform modules load by name
        args += ['--collect-submodules', 'OpenGL']
    for d in _apps_data():
        args += ['--add-data', d]
    if sys.platform == 'win32':
        args += ['--version-file', _version_file()]
    if icon:
        args += ['--icon', icon]
    if sys.platform == 'darwin':
        args += ['--osx-bundle-identifier', 'com.xseti.' + CONFIG['name'].lower()]
    _run([sys.executable, '-m', 'PyInstaller'] + args)
    out = os.path.join(DIST, CONFIG['name'])
    for name in CONFIG['beside_exe']:                    # defaults the exe reads / writes next to itself
        src = os.path.join(ROOT, name)
        if os.path.isdir(src):
            shutil.copytree(src, os.path.join(out, name), dirs_exist_ok=True)
    shutil.copytree(os.path.join(HERE, 'settings'), os.path.join(out, 'settings'), dirs_exist_ok=True)


def _version_file() -> str: #vers 1
    """Windows version resource (company, product, version); fewer Defender alarms."""
    name, title = CONFIG['name'], CONFIG['title']
    t = time.localtime()
    nums = (t.tm_year, t.tm_mon, t.tm_mday, 0)
    dotted = '.'.join(str(n) for n in nums)
    strings = {'CompanyName': 'X-Seti', 'FileDescription': title, 'FileVersion': dotted,
               'InternalName': name, 'LegalCopyright': 'X-Seti', 'OriginalFilename': name + '.exe',
               'ProductName': title, 'ProductVersion': dotted}
    items = ',\n'.join(f"          StringStruct('{k}', '{v}')" for k, v in strings.items())
    text = f"""VSVersionInfo(
  ffi=FixedFileInfo(filevers={nums}, prodvers={nums}, mask=0x3f, flags=0x0, OS=0x40004,
                    fileType=0x1, subtype=0x0, date=(0, 0)),
  kids=[
    StringFileInfo([StringTable('040904B0', [
{items}])]),
    VarFileInfo([VarStruct('Translation', [1033, 1200])])
  ]
)
"""
    out = os.path.join(ROOT, 'build', 'version_info.txt')
    os.makedirs(os.path.dirname(out), exist_ok=True)
    with open(out, 'w', encoding='utf-8') as f:
        f.write(text)
    return out


def _drop(): #vers 1
    """--drop: built program and _internal/ copied into the repo root (git ignores them)."""
    name = CONFIG['name']
    if sys.platform == 'darwin':
        pairs = [(os.path.join(DIST, name + '.app'), os.path.join(ROOT, name + '.app'))]
    else:
        exe = name + ('.exe' if sys.platform == 'win32' else '')
        pairs = [(os.path.join(DIST, name, n), os.path.join(ROOT, n)) for n in (exe, '_internal')]
    for src, dst in pairs:
        if os.path.isdir(dst) and not os.path.islink(dst):
            shutil.rmtree(dst)
        elif os.path.lexists(dst):
            os.remove(dst)
        if os.path.isdir(src):
            shutil.copytree(src, dst, symlinks=True)
        else:
            shutil.copy2(src, dst)
    print('Dropped', name, 'into', ROOT)


def _smoke_test(): #vers 3
    """Start the built program; it must still be running after 15 s (Linux without a display: xvfb-run)."""
    name = CONFIG['name']
    if sys.platform == 'darwin':
        exe = os.path.join(DIST, name + '.app', 'Contents', 'MacOS', name)
    else:
        exe = os.path.join(DIST, name, name + ('.exe' if sys.platform == 'win32' else ''))
    cmd, env = [exe], dict(os.environ, QT_QPA_PLATFORM='offscreen')
    if sys.platform.startswith('linux'):
        env.pop('QT_QPA_PLATFORM')                       # apps choose xcb themselves
        if not (os.environ.get('DISPLAY') or os.environ.get('WAYLAND_DISPLAY')):
            if not shutil.which('xvfb-run'):
                raise RuntimeError('No display for the start test: install xvfb (xvfb-run)')
            cmd = ['xvfb-run', '-a'] + cmd
    p = subprocess.Popen(cmd, cwd=os.path.dirname(exe), env=env,
                         stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    for _ in range(30):
        time.sleep(0.5)
        if p.poll() is not None:
            out = p.stdout.read().decode(errors='replace')
            print(out)
            at = out.rfind('Traceback')
            why = out[at:] if at >= 0 else out[-1500:]           # the program's own error first
            raise RuntimeError(f'{name} exited early (code {p.returncode})\n{why[-1500:]}')
    p.kill()
    print(f'{name} started and kept running: OK')


def _package() -> str: #vers 1
    """<os>_<arch>_dist/<APP>_<Platform>_<arch>.zip (macOS: the .app; Linux: .tar.gz)."""
    name, tag = CONFIG['name'], _platform_tag()
    base = os.path.join(DIST, f'{name}_{tag}')
    if sys.platform == 'darwin':
        return shutil.make_archive(base, 'zip', DIST, name + '.app')
    fmt = 'gztar' if sys.platform.startswith('linux') else 'zip'
    return shutil.make_archive(base, fmt, DIST, name)


def _linux_scripts(icon: str): #vers 1
    """install.sh / uninstall.sh and the icon into <os>_<arch>_dist/<APP>/ (per user, no root)."""
    name, title = CONFIG['name'], CONFIG['title']
    out = os.path.join(DIST, name)
    if icon:
        shutil.copy(icon, os.path.join(out, name + '.png'))
    install = f"""#!/bin/sh
# Installs {title} for this user: ~/.local/opt/{name}, menu entry, ~/.local/bin/{name}
set -e
HERE="$(cd "$(dirname "$0")" && pwd)"
DEST="$HOME/.local/opt/{name}"
ICONS="$HOME/.local/share/icons/hicolor/256x256/apps"
APPS="$HOME/.local/share/applications"
mkdir -p "$DEST" "$HOME/.local/bin" "$ICONS" "$APPS"
[ "$HERE" = "$DEST" ] || cp -a "$HERE/." "$DEST/"
ln -sf "$DEST/{name}" "$HOME/.local/bin/{name}"
[ -f "$DEST/{name}.png" ] && cp "$DEST/{name}.png" "$ICONS/{name}.png"
cat > "$APPS/{name}.desktop" <<EOF
[Desktop Entry]
Type=Application
Name={title}
Exec="$DEST/{name}" %f
Icon={name}
Terminal=false
Categories=Graphics;Development;
EOF
command -v update-desktop-database >/dev/null && update-desktop-database "$APPS"
echo "{title} installed in $DEST - start it from the menu or run: {name}"
"""
    uninstall = f"""#!/bin/sh
# Removes {title} installed by install.sh
rm -f "$HOME/.local/bin/{name}" "$HOME/.local/share/applications/{name}.desktop" \\
      "$HOME/.local/share/icons/hicolor/256x256/apps/{name}.png"
rm -rf "$HOME/.local/opt/{name}"
echo "{title} removed"
"""
    for fname, text in (('install.sh', install), ('uninstall.sh', uninstall)):
        path = os.path.join(out, fname)
        with open(path, 'w', newline='\n') as f:
            f.write(text)
        os.chmod(path, 0o755)


def _windows_setup(icon: str) -> str: #vers 1
    """Inno Setup 6 installer: per-user install, Start menu, optional desktop icon, uninstaller."""
    from PIL import Image
    name, title, tag = CONFIG['name'], CONFIG['title'], _platform_tag()
    iscc = shutil.which('iscc') or os.path.join(os.environ.get('ProgramFiles(x86)', r'C:\Program Files (x86)'),
                                                 'Inno Setup 6', 'ISCC.exe')
    if not os.path.exists(iscc):
        raise RuntimeError('Inno Setup 6 not found (ISCC.exe): choco install innosetup')
    build = os.path.join(ROOT, 'build')
    lines = ['[Setup]',
             'AppId={{%s}' % uuid.uuid5(uuid.NAMESPACE_DNS, 'com.xseti.' + name.lower()),
             f'AppName={title}', f'AppVersion={time.strftime("%Y.%m.%d")}', 'AppPublisher=X-Seti',
             f'DefaultDirName={{autopf}}\\{title}', f'DefaultGroupName={title}',
             'PrivilegesRequired=lowest', 'PrivilegesRequiredOverridesAllowed=dialog',
             f'OutputDir={DIST}', f'OutputBaseFilename={name}_{tag}_Setup',
             f'UninstallDisplayIcon={{app}}\\{name}.exe', 'Compression=lzma2', 'SolidCompression=yes',
             'WizardStyle=modern',
             'ArchitecturesAllowed=' + ('arm64' if tag.endswith('arm64') else 'x64compatible'),
             'ArchitecturesInstallIn64BitMode=' + ('arm64' if tag.endswith('arm64') else 'x64compatible')]
    if icon:
        ico = os.path.join(build, name + '.ico')
        Image.open(icon).save(ico, sizes=[(16, 16), (32, 32), (48, 48), (64, 64), (128, 128), (256, 256)])
        lines.append(f'SetupIconFile={ico}')
    lines += ['', '[Tasks]',
              'Name: desktopicon; Description: "{cm:CreateDesktopIcon}"; Flags: unchecked',
              '', '[Files]',
              f'Source: "{os.path.join(DIST, name)}\\*"; DestDir: "{{app}}"; '
              'Flags: ignoreversion recursesubdirs createallsubdirs',
              '', '[Icons]',
              f'Name: "{{group}}\\{title}"; Filename: "{{app}}\\{name}.exe"',
              f'Name: "{{group}}\\Uninstall {title}"; Filename: "{{uninstallexe}}"',
              f'Name: "{{autodesktop}}\\{title}"; Filename: "{{app}}\\{name}.exe"; Tasks: desktopicon',
              '', '[Run]',
              f'Filename: "{{app}}\\{name}.exe"; Description: "{{cm:LaunchProgram,{title}}}"; '
              'Flags: nowait postinstall skipifsilent']
    iss = os.path.join(build, name + '.iss')
    with open(iss, 'w', encoding='utf-8-sig') as f:
        f.write('\n'.join(lines) + '\n')
    _run([iscc, '/Q', iss])
    return os.path.join(DIST, f'{name}_{tag}_Setup.exe')


def _mac_dmg() -> str: #vers 1
    """Disk image with the .app and an Applications link (drag to install)."""
    name, tag = CONFIG['name'], _platform_tag()
    stage = os.path.join(ROOT, 'build', 'dmg')
    shutil.rmtree(stage, ignore_errors=True)
    os.makedirs(stage)
    shutil.copytree(os.path.join(DIST, name + '.app'), os.path.join(stage, name + '.app'), symlinks=True)
    os.symlink('/Applications', os.path.join(stage, 'Applications'))
    out = os.path.join(DIST, f'{name}_{tag}.dmg')
    _run(['hdiutil', 'create', '-volname', CONFIG['title'], '-srcfolder', stage, '-ov', '-format', 'UDZO', out])
    return out


def _installer(icon: str) -> str: #vers 1
    """Windows Setup.exe or macOS .dmg (Linux: install.sh is in the .tar.gz)."""
    if sys.platform == 'win32':
        return _windows_setup(icon)
    if sys.platform == 'darwin':
        return _mac_dmg()
    return ''


# ---- add-ons and scaffolds -----------------------------------------------------------------------

TEMPLATE_REPO = 'Txd-Workshop'                           # standalone buildscript / build.yml source
COPY_SKIP = SKIP_DIRS | {'img_factory_debug.log', 'projects.json'}   # user project list stays local


def _arg_value(flag: str) -> str: #vers 1
    """Value after flag, checked as a Name_Like_This folder name."""
    i = sys.argv.index(flag)
    if i + 1 >= len(sys.argv) or not re.fullmatch(r'[A-Z][A-Za-z0-9]*(_[A-Za-z0-9]+)+', sys.argv[i + 1]):
        raise SystemExit(f'{flag} needs a name like Foo_Workshop')
    return sys.argv[i + 1]


def _new_file(path: str, text: str): #vers 1
    """Write a new file; never overwrites."""
    if os.path.exists(path):
        raise SystemExit(f'Exists, not changed: {path}')
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, 'w', encoding='utf-8', newline='\n') as f:
        f.write(text)
    print('  new', os.path.relpath(path, os.path.dirname(ROOT)))


def _copy_tree(src: str, dst: str): #vers 1
    """Copy a folder without caches, _to_delete or logs."""
    shutil.copytree(src, dst, dirs_exist_ok=True,
                    ignore=lambda d, names: [n for n in names if n in COPY_SKIP or n.endswith(('.pyc', '.log'))])


def _new_workshop(name: str): #vers 1
    """Placeholder workshop in apps/components/<name>/: main file, depends/ mixins, __init__ files."""
    comp = os.path.join(ROOT, 'apps', 'components', name)
    if os.path.exists(comp):
        raise SystemExit(f'Exists, not changed: {comp}')
    mod, title = name.lower(), name.replace('_', ' ')
    pre, cls = mod.split('_')[0], name.replace('_', '')
    mix = pre.capitalize()
    date = time.strftime('%B %d %Y')
    rel = f'apps/components/{name}'
    _new_file(os.path.join(comp, '__init__.py'), f'#this belongs in {rel}/__init__.py - Version: 1\n')
    _new_file(os.path.join(comp, 'depends', '__init__.py'), f'#this belongs in {rel}/depends/__init__.py - Version: 1\n')
    _new_file(os.path.join(comp, mod + '.py'), f'''#!/usr/bin/env python3
#this belongs in {rel}/{mod}.py - Version: 1
# X-Seti - {date} - IMG Factory 1.6 - {title}

"""
{title} - placeholder: GUIWorkshop window, UI and logic in depends/.
"""

##Methods list -
# open_{mod}
#
##class {cls}: -
# __init__

import os
import sys

_root = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
if _root not in sys.path:                      # standalone: make 'apps' importable
    sys.path.insert(0, _root)

from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import QApplication

from apps.methods.gui_workshop import GUIWorkshop
from apps.components.{name}.depends.{pre}_logic_func import {mix}LogicMixin
from apps.components.{name}.depends.{pre}_ui_func import {mix}UIMixin

App_name = "{title}"


class {cls}({mix}UIMixin, {mix}LogicMixin, GUIWorkshop): #vers 1
    """{title} window."""
    App_name = App_name
    config_key = "{mod}"

    def __init__(self, parent=None, main_window=None): #vers 1
        super().__init__(parent, main_window)


def open_{mod}(main_window=None): #vers 1
    """Open docked in IMG Factory (tab) or standalone."""
    if main_window is not None and hasattr(main_window, 'main_tab_widget'):
        ws = {cls}(None, main_window)
        ws.setWindowFlags(Qt.WindowType.Widget)
        idx = main_window.main_tab_widget.addTab(ws, App_name)
        main_window.main_tab_widget.setCurrentIndex(idx)
    else:
        ws = {cls}(main_window=main_window)
        ws.setWindowTitle(App_name)
        ws.resize(1200, 800)
    ws.show()
    return ws


if __name__ == "__main__":
    app = QApplication(sys.argv)
    w = open_{mod}(None)
    sys.exit(app.exec())
''')
    _new_file(os.path.join(comp, 'depends', f'{pre}_ui_func.py'), f'''#this belongs in {rel}/depends/{pre}_ui_func.py - Version: 1
# X-Seti - {date} - IMG Factory 1.6 - {title} UI

"""
{title} panels; replace the placeholder centre panel.
"""

##Methods list -
#
##class {mix}UIMixin: -
# _create_centre_panel

from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import QLabel


class {mix}UIMixin: #vers 1
    """Panels for {title}."""

    def _create_centre_panel(self): #vers 1
        """Placeholder centre panel."""
        lbl = QLabel(self.App_name + " - placeholder")
        lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
        return lbl
''')
    _new_file(os.path.join(comp, 'depends', f'{pre}_logic_func.py'), f'''#this belongs in {rel}/depends/{pre}_logic_func.py - Version: 1
# X-Seti - {date} - IMG Factory 1.6 - {title} logic

"""
{title} file handling; replace the placeholders.
"""

##Methods list -
#
##class {mix}LogicMixin: -
# _open_file
# _save_file


class {mix}LogicMixin: #vers 1
    """File open / save for {title}."""

    def _open_file(self, path=None): #vers 1
        """Placeholder: load a file."""
        self._set_status("Open: not written yet")

    def _save_file(self): #vers 1
        """Placeholder: save the file."""
        self._set_status("Save: not written yet")
''')
    print(f'Workshop {name}: run it with python {rel}/{mod}.py')


def _new_repo(name: str): #vers 1
    """Sibling standalone repo ../<Name-With-Dashes>/ holding the workshop and what it imports."""
    mod, title = name.lower(), name.replace('_', ' ')
    if not os.path.isdir(os.path.join(ROOT, 'apps', 'components', name)):
        _new_workshop(name)
    shops = _workshops()
    if mod not in shops:
        raise SystemExit(f'{name}/{mod}.py has no __main__ block: not a runnable workshop')
    gh = os.path.dirname(ROOT)
    dest, tmpl = os.path.join(gh, name.replace('_', '-')), os.path.join(gh, TEMPLATE_REPO)
    if os.path.exists(dest):
        raise SystemExit(f'Exists, not changed: {dest}')
    for need in ('buildscript.py', os.path.join('tools', 'build.yml'), '.gitignore', 'LICENSE'):
        if not os.path.isfile(os.path.join(tmpl, need)):
            raise SystemExit(f'Template missing: {os.path.join(tmpl, need)}')
    date = time.strftime('%B %d %Y')
    apps = os.path.join(ROOT, 'apps')
    for n in os.listdir(apps):                           # shared code; components and settings links below
        src = os.path.join(apps, n)
        if n in COPY_SKIP or n in ('components', 'utils', 'themes', 'images') or n.endswith('.log'):
            continue
        if os.path.isdir(src):
            _copy_tree(src, os.path.join(dest, 'apps', n))
        else:
            os.makedirs(os.path.join(dest, 'apps'), exist_ok=True)
            shutil.copy2(src, os.path.join(dest, 'apps', n))
    os.makedirs(os.path.join(dest, 'apps', 'components'), exist_ok=True)
    shutil.copy2(os.path.join(apps, 'components', '__init__.py'), os.path.join(dest, 'apps', 'components', '__init__.py'))
    folder, deps = shops[mod]
    for d in sorted(deps):
        _copy_tree(os.path.join(apps, 'components', d), os.path.join(dest, 'apps', 'components', d))
    shared = os.path.join(gh, 'App-Settings-System', 'apps')
    for d in ('utils', 'themes', 'images'):              # linked to App-Settings-System (CI copies them)
        target = os.path.join(shared, d)
        if not os.path.isdir(target):
            raise SystemExit(f'Missing {target}: App-Settings-System repo needed beside this one')
        os.symlink(os.path.relpath(target, os.path.join(dest, 'apps')), os.path.join(dest, 'apps', d))
    shutil.copy2(os.path.join(ROOT, 'appfactory.settings.json'), os.path.join(dest, 'appfactory.settings.json'))
    shutil.copy2(os.path.join(tmpl, 'LICENSE'), os.path.join(dest, 'LICENSE'))
    _copy_tree(os.path.join(ROOT, 'icons'), os.path.join(dest, 'icons'))
    os.makedirs(os.path.join(dest, 'settings'), exist_ok=True)
    _new_file(os.path.join(dest, 'settings', '.gitkeep'), '')
    launcher = f'launch_{mod}.py'
    _new_file(os.path.join(dest, launcher), f'''#!/usr/bin/env python3
#this belongs in root /{launcher} - Version: 1
# X-Seti - {date} - {title} - Root Launcher

import sys
from pathlib import Path

root_dir = Path(__file__).parent.resolve()
if str(root_dir) not in sys.path:
    sys.path.insert(0, str(root_dir))

if __name__ == "__main__":
    from PyQt6.QtWidgets import QApplication
    from apps.components.{folder}.{mod} import open_{mod}
    app = QApplication(sys.argv)
    ws = open_{mod}(None)
    sys.exit(app.exec())
''')
    with open(os.path.join(tmpl, 'buildscript.py'), encoding='utf-8') as f:
        bs = f.read()
    a, b = bs.index('CONFIG = {'), bs.index('\n}\n', bs.index('CONFIG = {')) + 3
    cfg = (f"CONFIG = {{\n    'name': '{name}',\n    'title': '{title}',\n    'launcher': '{launcher}',\n"
           f"    'icon': '',                                   # SVGIconFactory icon name, when made\n"
           f"    'required': {CONFIG['required']!r},\n    'optional': {CONFIG['optional']!r},\n"
           f"    'hidden': {CONFIG['hidden']!r},\n    'bundle_files': ['appfactory.settings.json'],\n"
           f"    'beside_exe': ['settings', 'icons'],\n}}\n")
    bs = bs[:a] + cfg + bs[b:]
    bs = re.sub(r'#this belongs in root /buildscript.py - Version: \d+\n# X-Seti - .*\n',
                f'#this belongs in root /buildscript.py - Version: 1\n# X-Seti - {date} - {title} - build for this computer\'s platform\n', bs, count=1)
    bs = bs.replace(TEMPLATE_REPO.replace('-', ' '), title)
    _new_file(os.path.join(dest, 'buildscript.py'), bs)
    with open(os.path.join(tmpl, 'tools', 'build.yml'), encoding='utf-8') as f:
        _new_file(os.path.join(dest, 'tools', 'build.yml'), f.read())
    with open(os.path.join(tmpl, '.gitignore'), encoding='utf-8') as f:
        tname = TEMPLATE_REPO.replace('-', '_')
        _new_file(os.path.join(dest, '.gitignore'), f.read().replace('/' + tname, '/' + name))
    _new_file(os.path.join(dest, '.gitattributes'), '# Auto detect text files and perform LF normalization\n* text=auto\n')
    _new_file(os.path.join(dest, 'requirements.txt'),
              '\n'.join(sorted(CONFIG['required'] + CONFIG['optional'] + ['Pillow'], key=str.lower)) + '\n')
    _new_file(os.path.join(dest, 'README.md'), f'# {title}\n\nStandalone {title} from IMG Factory 1.6.\n\n'
              f'Run: `python {launcher}`  -  Build: `python buildscript.py`\n')
    _new_file(os.path.join(dest, 'ChangeLog.md'), f'#this belongs in root /ChangeLog.md - Version: 1\n\n\n'
              f'- {time.strftime("%b %d %Y")} - First standalone {title} from IMG Factory 1.6.\n')
    if shutil.which('git'):
        subprocess.run(['git', 'init', '-q', '-b', 'main'], cwd=dest)
    print(f'Repo {dest}: move tools/build.yml to .github/workflows/, add the GitHub remote, then push')


def _addon_windows(folders: list, stage: str) -> str: #vers 1
    """Inno Setup add-on: into an installed IMG Factory, folders listed in installed.txt."""
    name, title, tag = CONFIG['name'], CONFIG['title'], _platform_tag()
    iscc = shutil.which('iscc') or os.path.join(os.environ.get('ProgramFiles(x86)', r'C:\Program Files (x86)'),
                                                 'Inno Setup 6', 'ISCC.exe')
    if not os.path.exists(iscc):
        raise RuntimeError('Inno Setup 6 not found (ISCC.exe): choco install innosetup')
    core = VARIANTS['core'][1]
    adds = '\n'.join(f"    AddLine(F, '{d}');" for d in folders)
    iss = f"""[Setup]
AppId={{{{{uuid.uuid5(uuid.NAMESPACE_DNS, 'com.xseti.addon.' + name.lower())}}}
AppName={title} add-on
AppVersion={time.strftime('%Y.%m.%d')}
AppPublisher=X-Seti
DefaultDirName={{autopf}}\\{core}
DirExistsWarning=no
DisableProgramGroupPage=yes
Uninstallable=no
PrivilegesRequired=lowest
PrivilegesRequiredOverridesAllowed=dialog
OutputDir={DIST}
OutputBaseFilename={name}_Addon_{tag}_Setup
Compression=lzma2
SolidCompression=yes
WizardStyle=modern

[Messages]
SelectDirLabel3=Choose the folder where {core} is installed.

[Files]
Source: "{stage}\\*"; DestDir: "{{app}}\\_internal\\apps\\components"; Flags: ignoreversion recursesubdirs createallsubdirs

[Code]
procedure AddLine(F, L: String);
var S: TArrayOfString; I: Integer;
begin
  if LoadStringsFromFile(F, S) then
    for I := 0 to GetArrayLength(S) - 1 do
      if Trim(S[I]) = L then Exit;
  SaveStringToFile(F, L + #13#10, True);
end;

function NextButtonClick(CurPageID: Integer): Boolean;
begin
  Result := True;
  if (CurPageID = wpSelectDir) and
     not FileExists(ExpandConstant('{{app}}\\_internal\\apps\\components\\installed.txt')) then begin
    MsgBox('{core} is not installed in this folder.', mbError, MB_OK);
    Result := False;
  end;
end;

procedure CurStepChanged(CurStep: TSetupStep);
var F: String;
begin
  if CurStep = ssPostInstall then begin
    F := ExpandConstant('{{app}}\\_internal\\apps\\components\\installed.txt');
{adds}
  end;
end;
"""
    path = os.path.join(ROOT, 'build', name + '_addon.iss')
    with open(path, 'w', encoding='utf-8-sig') as f:
        f.write(iss)
    _run([iscc, '/Q', path])
    return os.path.join(DIST, f'{name}_Addon_{tag}_Setup.exe')


def _addon_unix(folders: list, stage: str) -> str: #vers 1
    """Archive: components/ plus install_addon.sh (finds installed.txt under the given folder)."""
    name, title, tag = CONFIG['name'], CONFIG['title'], _platform_tag()
    lines = ' '.join(folders)
    script = f"""#!/bin/sh
# Adds {title} to an installed IMG Factory: ./install_addon.sh [IMG Factory folder or .app]
set -e
HERE="$(cd "$(dirname "$0")" && pwd)"
TARGET="${{1:-$HOME/.local/opt/Img_Factory}}"
LIST="$(find "$TARGET" -path '*apps/components/installed.txt' | head -1)"
[ -n "$LIST" ] || {{ echo "No IMG Factory (installed.txt) in $TARGET"; exit 1; }}
DEST="$(dirname "$LIST")"
cp -a "$HERE/components/." "$DEST/"
for d in {lines}; do grep -qx "$d" "$LIST" || echo "$d" >> "$LIST"; done
echo "{title} added to $DEST"
"""
    path = os.path.join(os.path.dirname(stage), 'install_addon.sh')
    with open(path, 'w', newline='\n') as f:
        f.write(script)
    os.chmod(path, 0o755)
    base = os.path.join(DIST, f'{name}_Addon_{tag}')
    fmt = 'gztar' if sys.platform.startswith('linux') else 'zip'
    return shutil.make_archive(base, fmt, os.path.dirname(os.path.dirname(stage)), os.path.basename(os.path.dirname(stage)))


def _addon(): #vers 1
    """--addon --<workshop>: the workshop folders a core build lacks, as an add-on for it."""
    comps = CONFIG['components']
    if comps is None or CONFIG['name'] in (v[0] for v in VARIANTS.values()):
        raise SystemExit('--addon needs a workshop, e.g. --addon --txd_workshop')
    folders = sorted(comps - VARIANTS['core'][2])
    root = os.path.join(ROOT, 'build', 'addon', CONFIG['name'] + '_Addon')
    stage = os.path.join(root, 'components')
    shutil.rmtree(root, ignore_errors=True)
    for d in folders:
        _copy_tree(os.path.join(ROOT, 'apps', 'components', d), os.path.join(stage, d))
    os.makedirs(DIST, exist_ok=True)
    out = _addon_windows(folders, stage) if sys.platform == 'win32' else _addon_unix(folders, stage)
    print('Add-on', out, '-', ', '.join(folders))


def _use_venv(): #vers 2
    """Not in a virtual env: make ROOT/.venv (first run) and run this script again with its Python."""
    if sys.prefix != sys.base_prefix:
        return
    venv = os.path.join(HERE, '.venv')
    py = os.path.join(venv, 'Scripts', 'python.exe') if sys.platform == 'win32' else os.path.join(venv, 'bin', 'python')
    if not os.path.exists(py):
        print('Creating', venv, flush=True)
        _run([sys.executable, '-m', 'venv', venv])
    sys.exit(subprocess.run([py, os.path.abspath(__file__)] + sys.argv[1:], cwd=ROOT).returncode)


def main(): #vers 4
    if '--new' in sys.argv:
        _new_workshop(_arg_value('--new'))
        return
    if '--new-repo' in sys.argv:
        _new_repo(_arg_value('--new-repo'))
        return
    _set_variant()
    if '--addon' in sys.argv:
        _addon()
        return
    if '--name' in sys.argv:
        print(CONFIG['name'])
        return
    _use_venv()
    try:
        if '--no-install' not in sys.argv:
            _install('--ci' in sys.argv)
        icon = _icon()
        _pyinstaller(icon)
        _smoke_test()
        if '--drop' in sys.argv:
            _drop()
        if sys.platform.startswith('linux'):
            _linux_scripts(icon)
        print('Built', _package())
        if '--ci' in sys.argv or '--installer' in sys.argv:
            setup = _installer(icon)
            if setup:
                print('Installer', setup)
    except Exception as e:
        if os.environ.get('GITHUB_ACTIONS'):             # visible on the run page without signing in
            lines = f'{type(e).__name__}: {e}'.splitlines()
            if not isinstance(e, RuntimeError):
                lines += list(TAIL)[-25:]
            msg = '%0A'.join(ln.replace('%', '%25').replace('\r', '') for ln in lines)
            print(f'::error title={CONFIG["name"]} {_platform_tag()} build failed::{msg}', flush=True)
        raise


if __name__ == '__main__':
    main()
