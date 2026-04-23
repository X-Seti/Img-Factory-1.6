#!/usr/bin/env python3
"""
tools/dev_setup.py — IMG Factory 1.6 Development Setup Tool
X-Seti - Apr 2026
#this belongs in tools/dev_setup.py - version 1

Handles full environment setup for:
  - Linux (Ubuntu/Debian, Arch, Fedora, openSUSE)
  - WSL2 (Windows Subsystem for Linux)
  - macOS (Homebrew)
  - Android via Termux

Installs system packages, Python deps, copies themes to user config,
and verifies the environment is ready to run IMG Factory.

Usage:
    python3 tools/dev_setup.py
    python3 tools/dev_setup.py --check-only
    python3 tools/dev_setup.py --themes-only
"""

##Methods list -
# detect_environment
# install_system_packages
# install_python_deps
# setup_themes
# check_environment
# configure_display
# main

import sys
import os
import subprocess
import shutil
import argparse
import json
from pathlib import Path

# ── Root of the repo (one level up from tools/) ───────────────────────────────
REPO_ROOT   = Path(__file__).parent.parent.resolve()
THEMES_SRC  = REPO_ROOT / 'apps' / 'themes'
VENV_DIR    = REPO_ROOT / '.venv'

# ── Platform detection ─────────────────────────────────────────────────────────
def detect_environment() -> dict: #vers 1
    """Return a dict describing the current OS/environment."""
    import platform
    env = {
        'system':   platform.system(),   # Linux / Darwin / Windows
        'machine':  platform.machine(),  # x86_64 / aarch64 / arm
        'is_wsl':   False,
        'is_termux': False,
        'is_macos':  False,
        'distro':    '',
        'distro_like': '',
    }

    if env['system'] == 'Linux':
        # WSL check
        try:
            env['is_wsl'] = 'microsoft' in Path('/proc/version').read_text().lower()
        except OSError:
            pass

        # Termux check (Android)
        env['is_termux'] = 'com.termux' in os.environ.get('PREFIX', '') \
                           or Path('/data/data/com.termux').exists()

        # Distro from os-release
        try:
            for line in Path('/etc/os-release').read_text().splitlines():
                if line.startswith('ID='):
                    env['distro'] = line.split('=', 1)[1].strip().strip('"').lower()
                if line.startswith('ID_LIKE='):
                    env['distro_like'] = line.split('=', 1)[1].strip().strip('"').lower()
        except OSError:
            pass

    elif env['system'] == 'Darwin':
        env['is_macos'] = True

    return env


def _run(cmd: list, check=True): #vers 1
    """Run a shell command, printing it first."""
    print(f"  $ {' '.join(str(c) for c in cmd)}")
    subprocess.run(cmd, check=check)


def install_system_packages(env: dict): #vers 1
    """Install Qt6, XCB, and build tools via the appropriate package manager."""
    system  = env['system']
    distro  = env['distro']
    like    = env['distro_like']

    print("\n[+] Installing system packages...")

    # ── macOS ──────────────────────────────────────────────────────────────────
    if env['is_macos']:
        if not shutil.which('brew'):
            print("    Homebrew not found — installing...")
            _run(['/bin/bash', '-c',
                  '$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)'])
        _run(['brew', 'install', 'python@3.12', 'qt@6', 'git'])
        # Ensure Qt6 is on PATH
        qt_prefix = subprocess.check_output(['brew', '--prefix', 'qt@6']).decode().strip()
        os.environ['PATH'] = f"{qt_prefix}/bin:{os.environ['PATH']}"
        print(f"    Qt6 prefix: {qt_prefix}")
        return

    # ── Termux (Android) ──────────────────────────────────────────────────────
    if env['is_termux']:
        _run(['pkg', 'update', '-y'])
        _run(['pkg', 'install', '-y',
              'python', 'git', 'clang', 'make',
              'qt6-base', 'qt6-tools',
              'libxcb'])
        return

    # ── Linux ─────────────────────────────────────────────────────────────────
    def is_debian_based():
        return distro in ('ubuntu','debian','linuxmint','pop','raspbian') \
            or 'debian' in like or 'ubuntu' in like

    def is_arch_based():
        return distro in ('arch','manjaro','endeavouros','artix') \
            or 'arch' in like

    def is_fedora_based():
        return distro in ('fedora','rhel','centos','almalinux','rocky') \
            or 'fedora' in like or 'rhel' in like

    def is_suse_based():
        return distro.startswith('opensuse') or distro in ('suse',) \
            or 'suse' in like

    if is_debian_based():
        _run(['sudo', 'apt', 'update'])
        _run(['sudo', 'apt', 'install', '-y',
              'python3', 'python3-pip', 'python3-venv', 'git', 'build-essential',
              'qt6-base-dev', 'qt6-tools-dev', 'qt6-tools-dev-tools',
              'libxcb-cursor0', 'libxcb-xinerama0', 'libxkbcommon-x11-0',
              'libxcb-render0', 'libxcb-shape0', 'libxcb-xfixes0',
              'libxcb-randr0', 'libxcb-glx0', 'libgl1', 'libegl1', 'libx11-xcb1'])

    elif is_arch_based():
        _run(['sudo', 'pacman', '-Sy', '--noconfirm',
              'python', 'python-pip', 'git', 'base-devel',
              'qt6-base', 'qt6-tools',
              'libxcb', 'xcb-util-cursor', 'xcb-util', 'xcb-util-image',
              'xcb-util-keysyms', 'xcb-util-renderutil', 'xcb-util-wm'])

    elif is_fedora_based():
        _run(['sudo', 'dnf', 'install', '-y',
              'python3', 'python3-pip', 'git', '@development-tools',
              'qt6-qtbase', 'qt6-qttools',
              'xcb-util-cursor', 'xcb-util-wm',
              'libxkbcommon-x11', 'mesa-libGL', 'mesa-libEGL'])

    elif is_suse_based():
        _run(['sudo', 'zypper', 'install', '-y',
              'python3', 'python3-pip', 'git', 'gcc', 'make',
              'libQt6Core6', 'libQt6Gui6', 'libQt6Widgets6', 'libqt6-qttools',
              'libxcb-cursor0', 'libxkbcommon-x11-0', 'Mesa-libGL1', 'Mesa-libEGL1'])

    else:
        print(f"    [!] Unknown distro '{distro}' — skipping system packages.")
        print("        Install Qt6, Python3, and XCB libraries manually.")


def install_python_deps(env: dict): #vers 1
    """Create .venv and install Python dependencies."""
    print("\n[+] Setting up Python virtual environment...")

    python = shutil.which('python3') or shutil.which('python')
    if not python:
        print("    [!] python3 not found — install it first.")
        sys.exit(1)

    # On Termux there's no venv module — use system pip
    if env['is_termux']:
        print("    Termux: using system pip (no venv)")
        _run([python, '-m', 'pip', 'install', '--upgrade', 'pip'])
        _run([python, '-m', 'pip', 'install', 'PyQt6', 'Pillow', 'numpy'])
        req = REPO_ROOT / 'requirements.txt'
        if req.exists():
            _run([python, '-m', 'pip', 'install', '-r', str(req)])
        return

    # All other platforms — create .venv
    if not VENV_DIR.exists():
        _run([python, '-m', 'venv', str(VENV_DIR)])
        print(f"    Created .venv at {VENV_DIR}")
    else:
        print(f"    .venv already exists at {VENV_DIR}")

    # pip inside the venv
    venv_pip = VENV_DIR / ('Scripts' if sys.platform == 'win32' else 'bin') / 'pip'
    _run([str(venv_pip), 'install', '--upgrade', 'pip'])
    _run([str(venv_pip), 'install', 'PyQt6', 'Pillow', 'numpy'])

    req = REPO_ROOT / 'requirements.txt'
    if req.exists():
        _run([str(venv_pip), 'install', '-r', str(req)])

    print(f"\n    Activate with:  source {VENV_DIR}/bin/activate")


def setup_themes(env: dict): #vers 1
    """Copy apps/themes/*.json to the user config directory."""
    print("\n[+] Installing themes...")

    if not THEMES_SRC.exists():
        print(f"    [!] Themes source not found: {THEMES_SRC}")
        return

    # Determine user config dir per platform
    if env['is_termux']:
        config_dir = Path(os.environ.get('HOME', '/data/data/com.termux/files/home')) \
                     / '.config' / 'imgfactory' / 'themes'
    elif env['is_macos']:
        config_dir = Path.home() / 'Library' / 'Application Support' / 'ImgFactory' / 'themes'
    elif sys.platform == 'win32' or env['is_wsl']:
        appdata = os.environ.get('APPDATA', '')
        if appdata:
            config_dir = Path(appdata) / 'ImgFactory' / 'themes'
        else:
            config_dir = Path.home() / '.config' / 'imgfactory' / 'themes'
    else:
        config_dir = Path.home() / '.config' / 'imgfactory' / 'themes'

    config_dir.mkdir(parents=True, exist_ok=True)
    print(f"    Target: {config_dir}")

    copied = 0
    skipped = 0
    for src_file in THEMES_SRC.glob('*.json'):
        dst_file = config_dir / src_file.name
        if dst_file.exists():
            # Overwrite only if source is newer
            if src_file.stat().st_mtime > dst_file.stat().st_mtime:
                shutil.copy2(src_file, dst_file)
                print(f"    Updated : {src_file.name}")
                copied += 1
            else:
                skipped += 1
        else:
            shutil.copy2(src_file, dst_file)
            print(f"    Installed: {src_file.name}")
            copied += 1

    print(f"    {copied} theme(s) installed, {skipped} already up to date.")

    # Also write a themes_path.json so imgfactory knows where to look
    prefs_dir = config_dir.parent
    prefs_file = prefs_dir / 'imgfactory_settings.json'
    try:
        data = {}
        if prefs_file.exists():
            data = json.loads(prefs_file.read_text())
        data['themes_path'] = str(config_dir)
        prefs_file.write_text(json.dumps(data, indent=2))
        print(f"    Registered themes path in {prefs_file.name}")
    except Exception as e:
        print(f"    [!] Could not write settings: {e}")


def configure_display(env: dict): #vers 1
    """Print display/platform hints for the current environment."""
    print("\n[+] Display configuration:")

    if env['is_termux']:
        print("    Termux: ensure VNC or X11 server is running.")
        print("    Install: pkg install x11-repo && pkg install tigervnc")
        print("    Start:   vncserver :1 && export DISPLAY=:1")
        print("    Or use:  pkg install termux-x11-nightly")

    elif env['is_wsl']:
        print("    WSL2 detected.")
        print("    Windows 11: WSLg provides display automatically — nothing needed.")
        print("    Windows 10: install VcXsrv, launch with 'Disable access control',")
        print("                then: export DISPLAY=:0.0")
        print("    launch_imgfactory.py sets QT_QPA_PLATFORM=xcb automatically.")

    elif env['is_macos']:
        print("    macOS: XCB not available. Qt6 uses the Cocoa plugin.")
        print("    If the window does not appear: unset QT_QPA_PLATFORM")

    else:
        display = os.environ.get('DISPLAY', '')
        wayland = os.environ.get('WAYLAND_DISPLAY', '')
        if wayland and not display:
            print("    Wayland session detected. launch_imgfactory.py sets xcb fallback.")
        elif display:
            print(f"    X11 display: {display} — OK")
        else:
            print("    [!] No DISPLAY set. Are you in a headless session?")


def check_environment(env: dict) -> bool: #vers 1
    """Verify Python, PyQt6, and themes are in place. Returns True if OK."""
    print("\n[+] Environment check...")
    ok = True

    # Python version
    v = sys.version_info
    if v >= (3, 10):
        print(f"    Python {v.major}.{v.minor}.{v.micro} — OK")
    else:
        print(f"    [!] Python {v.major}.{v.minor} — need 3.10+")
        ok = False

    # PyQt6
    try:
        import PyQt6.QtCore
        print(f"    PyQt6 {PyQt6.QtCore.PYQT_VERSION_STR} — OK")
    except ImportError:
        print("    [!] PyQt6 not found — run setup first")
        ok = False

    # Pillow
    try:
        import PIL
        print(f"    Pillow {PIL.__version__} — OK")
    except ImportError:
        print("    [!] Pillow not found (optional but recommended)")

    # numpy
    try:
        import numpy
        print(f"    numpy {numpy.__version__} — OK")
    except ImportError:
        print("    [!] numpy not found (optional but recommended)")

    # Themes
    if env['is_termux']:
        themes_dir = Path(os.environ.get('HOME','.')) / '.config' / 'imgfactory' / 'themes'
    elif env['is_macos']:
        themes_dir = Path.home() / 'Library' / 'Application Support' / 'ImgFactory' / 'themes'
    else:
        themes_dir = Path.home() / '.config' / 'imgfactory' / 'themes'

    theme_count = len(list(themes_dir.glob('*.json'))) if themes_dir.exists() else 0
    if theme_count > 0:
        print(f"    Themes: {theme_count} installed at {themes_dir} — OK")
    else:
        print(f"    [!] Themes not found at {themes_dir} — run setup or --themes-only")

    # imgfactory.py reachable
    main_py = REPO_ROOT / 'apps' / 'components' / 'Img_Factory' / 'imgfactory.py'
    if main_py.exists():
        print(f"    imgfactory.py found — OK")
    else:
        print(f"    [!] imgfactory.py not found at {main_py}")
        ok = False

    return ok


# ── Entry point ────────────────────────────────────────────────────────────────
def main(): #vers 1
    parser = argparse.ArgumentParser(
        description='IMG Factory 1.6 — development environment setup')
    parser.add_argument('--check-only',   action='store_true',
                        help='Check environment without installing anything')
    parser.add_argument('--themes-only',  action='store_true',
                        help='Only install/update theme JSON files')
    parser.add_argument('--no-system',    action='store_true',
                        help='Skip system package installation')
    parser.add_argument('--no-venv',      action='store_true',
                        help='Skip Python venv creation')
    args = parser.parse_args()

    print()
    print("=== IMG Factory 1.6 — Dev Setup ===")

    env = detect_environment()
    print(f"    Platform : {env['system']} / {env['machine']}")
    if env['is_wsl']:     print("    Mode     : WSL2")
    elif env['is_termux']: print("    Mode     : Termux (Android)")
    elif env['is_macos']:  print("    Mode     : macOS")
    else:                  print(f"    Distro   : {env['distro'] or 'unknown'}")

    if args.check_only:
        check_environment(env)
        configure_display(env)
        return

    if args.themes_only:
        setup_themes(env)
        return

    if not args.no_system:
        install_system_packages(env)

    if not args.no_venv:
        install_python_deps(env)

    setup_themes(env)
    configure_display(env)
    check_environment(env)

    print()
    print("=== Setup complete ===")
    print()
    if env['is_termux']:
        print("Launch:  python launch_imgfactory.py")
    elif env['is_macos']:
        print("Launch:  source .venv/bin/activate && python3 launch_imgfactory.py")
    else:
        print("Launch:  source .venv/bin/activate && python3 launch_imgfactory.py")
    print()


if __name__ == '__main__':
    main()
