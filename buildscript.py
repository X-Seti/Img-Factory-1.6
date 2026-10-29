#!/usr/bin/env python3
#this belongs in root /buildscript.py - Version: 1
# X-Seti - October 29 2026 - IMG Factory 1.6 - build for this computer's platform

"""
Builds IMG Factory 1.6 for the platform it runs on (PyInstaller cannot build
for another one): Windows x64 / arm64, Linux x64 / arm64, macOS arm64 /
Intel. GitHub Actions (.github/workflows/build.yml) runs it on each.

    python buildscript.py            install deps, build, test, package
    python buildscript.py --ci       also compile PyInstaller's bootloader, make the installer
    python buildscript.py --installer  make the installer (local run)
    python buildscript.py --no-install
    python buildscript.py --name     print the app name
    --variant core   Img_Factory: IMG Factory with its core components only
    --variant full   Img_Factory_Full: every workshop in apps/components (default)

Runs inside its own .venv (made on first run), so the system Python
(PEP 668 / Arch, Debian, Fedora) is never changed.

Output: dist/<APP>/ (folder build: the program plus its _internal/ files)
and dist/<APP>_<Platform>_<arch>.zip (.tar.gz on Linux, with install.sh).
Installers: Windows dist/<APP>_<Platform>_<arch>_Setup.exe (Inno Setup 6),
macOS dist/<APP>_<Platform>_<arch>.dmg. Linux: install.sh in the .tar.gz.
"""

##Methods list -
# _app_modules
# _component_ok
# _manifest
# _apps_data
# _icon
# _install
# _installer
# _linux_scripts
# _mac_dmg
# _package
# _platform_tag
# _pyinstaller
# _run
# _smoke_test
# _use_venv
# _windows_setup
# _set_variant
# main

import os
import platform
import shutil
import subprocess
import sys
import time
import uuid
from collections import deque

ROOT = os.path.dirname(os.path.abspath(__file__))

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


def _set_variant(): #vers 1
    """--variant core | full sets the app name, title and bundled components."""
    key = sys.argv[sys.argv.index('--variant') + 1] if '--variant' in sys.argv else 'full'
    if key not in VARIANTS:
        raise SystemExit(f'--variant must be one of: {", ".join(VARIANTS)}')
    CONFIG['name'], CONFIG['title'], CONFIG['components'] = VARIANTS[key]


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


def _icon() -> str: #vers 2
    """App SVG icon rendered to build/<app>.png (PyInstaller converts it per platform), or ''."""
    if not CONFIG['icon']:
        return ''
    os.environ.setdefault('QT_QPA_PLATFORM', 'offscreen')
    sys.path.insert(0, ROOT)
    from PyQt6.QtWidgets import QApplication
    _icon.app = QApplication.instance() or QApplication([])      # kept alive while rendering
    import apps.methods.imgfactory_svg_icons as icons
    fn = getattr(icons.SVGIconFactory, CONFIG['icon'], None) or getattr(icons, CONFIG['icon'])
    out = os.path.join(ROOT, 'build', CONFIG['name'] + '.png')
    os.makedirs(os.path.dirname(out), exist_ok=True)
    import inspect
    icon = fn(256) if inspect.signature(fn).parameters else fn()   # some take no size
    if not icon.pixmap(256, 256).toImage().save(out, 'PNG'):
        raise RuntimeError('Icon render failed')
    return out


def _pyinstaller(icon: str): #vers 1
    """Folder build (dist/<APP>/<APP> + _internal/), windowed, no UPX."""
    args = [os.path.join(ROOT, CONFIG['launcher']), '--name', CONFIG['name'], '--noconfirm', '--clean',
            '--windowed', '--onedir', '--noupx', '--distpath', os.path.join(ROOT, 'dist'),
            '--workpath', os.path.join(ROOT, 'build'), '--specpath', os.path.join(ROOT, 'build'),
            '--paths', ROOT, '--exclude-module', 'tkinter']
    for m in _app_modules() + CONFIG['hidden']:
        args += ['--hidden-import', m]
    for d in _apps_data():
        args += ['--add-data', d]
    if icon:
        args += ['--icon', icon]
    if sys.platform == 'darwin':
        args += ['--osx-bundle-identifier', 'com.xseti.' + CONFIG['name'].lower()]
    _run([sys.executable, '-m', 'PyInstaller'] + args)
    out = os.path.join(ROOT, 'dist', CONFIG['name'])
    for name in CONFIG['beside_exe']:                    # defaults the exe reads / writes next to itself
        src = os.path.join(ROOT, name)
        if os.path.isdir(src):
            shutil.copytree(src, os.path.join(out, name), dirs_exist_ok=True)


def _smoke_test(): #vers 3
    """Start the built program; it must still be running after 15 s (Linux without a display: xvfb-run)."""
    name = CONFIG['name']
    if sys.platform == 'darwin':
        exe = os.path.join(ROOT, 'dist', name + '.app', 'Contents', 'MacOS', name)
    else:
        exe = os.path.join(ROOT, 'dist', name, name + ('.exe' if sys.platform == 'win32' else ''))
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
    """dist/<APP>_<Platform>_<arch>.zip (macOS: the .app; Linux: .tar.gz)."""
    name, tag = CONFIG['name'], _platform_tag()
    base = os.path.join(ROOT, 'dist', f'{name}_{tag}')
    if sys.platform == 'darwin':
        return shutil.make_archive(base, 'zip', os.path.join(ROOT, 'dist'), name + '.app')
    fmt = 'gztar' if sys.platform.startswith('linux') else 'zip'
    return shutil.make_archive(base, fmt, os.path.join(ROOT, 'dist'), name)


def _linux_scripts(icon: str): #vers 1
    """install.sh / uninstall.sh and the icon into dist/<APP>/ (per user, no root)."""
    name, title = CONFIG['name'], CONFIG['title']
    out = os.path.join(ROOT, 'dist', name)
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
             f'OutputDir={os.path.join(ROOT, "dist")}', f'OutputBaseFilename={name}_{tag}_Setup',
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
              f'Source: "{os.path.join(ROOT, "dist", name)}\\*"; DestDir: "{{app}}"; '
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
    return os.path.join(ROOT, 'dist', f'{name}_{tag}_Setup.exe')


def _mac_dmg() -> str: #vers 1
    """Disk image with the .app and an Applications link (drag to install)."""
    name, tag = CONFIG['name'], _platform_tag()
    stage = os.path.join(ROOT, 'build', 'dmg')
    shutil.rmtree(stage, ignore_errors=True)
    os.makedirs(stage)
    shutil.copytree(os.path.join(ROOT, 'dist', name + '.app'), os.path.join(stage, name + '.app'), symlinks=True)
    os.symlink('/Applications', os.path.join(stage, 'Applications'))
    out = os.path.join(ROOT, 'dist', f'{name}_{tag}.dmg')
    _run(['hdiutil', 'create', '-volname', CONFIG['title'], '-srcfolder', stage, '-ov', '-format', 'UDZO', out])
    return out


def _installer(icon: str) -> str: #vers 1
    """Windows Setup.exe or macOS .dmg (Linux: install.sh is in the .tar.gz)."""
    if sys.platform == 'win32':
        return _windows_setup(icon)
    if sys.platform == 'darwin':
        return _mac_dmg()
    return ''


def _use_venv(): #vers 1
    """Not in a virtual env: make ROOT/.venv (first run) and run this script again with its Python."""
    if sys.prefix != sys.base_prefix:
        return
    venv = os.path.join(ROOT, '.venv')
    py = os.path.join(venv, 'Scripts', 'python.exe') if sys.platform == 'win32' else os.path.join(venv, 'bin', 'python')
    if not os.path.exists(py):
        print('Creating', venv, flush=True)
        _run([sys.executable, '-m', 'venv', venv])
    sys.exit(subprocess.run([py, os.path.abspath(__file__)] + sys.argv[1:], cwd=ROOT).returncode)


def main(): #vers 1
    _set_variant()
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
