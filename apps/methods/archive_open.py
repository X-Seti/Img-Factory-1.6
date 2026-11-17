#this belongs in apps/methods/archive_open.py - Version: 1
# X-Seti - November 16 2026 - IMG Factory 1.6 - Open archives, mount ISO and IMG images

"""
Archive Open - extract archives to a cache folder or mount disc images for browsing.
"""

##Methods list -
# archive_cmd
# eject
# extract_cmd
# kind
# mount_image
# sshfs_cmd

import os
import re
import shutil
import subprocess

_ARCHIVE = ('.zip', '.7z', '.rar', '.tar', '.tar.gz', '.tgz', '.tar.bz2', '.tbz2',
            '.tar.xz', '.txz', '.tar.zst', '.cab', '.arj', '.lzh')
_IMAGE = ('.iso', '.img')
_CACHE = os.path.join(os.path.expanduser('~'), '.cache', 'imgfactory')


def kind(path: str): #vers 1
    """'archive', 'image' or None by file name."""
    low = path.lower()
    if low.endswith(_IMAGE):
        return 'image'
    if low.endswith(_ARCHIVE):
        return 'archive'
    return None


def extract_cmd(path: str):  #vers 1
    """(command, target folder) that extracts path with 7z, or None when 7z is missing."""
    exe = shutil.which('7z') or shutil.which('7zz')
    if exe is None:
        return None
    dest = os.path.join(_CACHE, 'archives', os.path.basename(path))
    shutil.rmtree(dest, ignore_errors=True)
    os.makedirs(dest)
    return [exe, 'x', '-y', '-o' + dest, path], dest


ARCHIVE_FORMATS = ('tar.gz', 'tar.xz', 'tar.zst', 'zip', '7z')


def archive_cmd(fmt: str, out: str, names, root: bool = False): #vers 1
    """Command that archives names (relative to the working folder) into out, or None without 7z."""
    if fmt.startswith('tar.'):
        flag = {'tar.gz': '-czf', 'tar.xz': '-cJf', 'tar.zst': '--zstd -cf'}[fmt].split()
        cmd = ['tar'] + flag + [out] + list(names)
    else:
        exe = shutil.which('7z') or shutil.which('7zz')
        if exe is None:
            return None
        cmd = [exe, 'a', '-y', out] + list(names)
    return (['pkexec'] + cmd) if root else cmd


def sshfs_cmd(target: str): #vers 1
    """(command, mount folder) for user@host:/path, or None without sshfs."""
    if shutil.which('sshfs') is None:
        return None
    safe = re.sub(r'[^A-Za-z0-9_.-]+', '_', target)
    mp = os.path.join(_CACHE, 'mnt', safe)
    os.makedirs(mp, exist_ok=True)
    return ['sshfs', '-o', 'BatchMode=yes,reconnect,ServerAliveInterval=15,ServerAliveCountMax=3',
            target, mp], mp


def mount_image(path: str, root: bool = False): #vers 1
    """Mount read-only; returns {'path', 'method', 'dev'}. Raises RuntimeError with the reason."""
    def run(cmd):
        r = subprocess.run(cmd, capture_output=True, text=True, timeout=60)
        if r.returncode:
            raise RuntimeError((r.stderr or r.stdout).strip() or ' '.join(cmd))
        return r.stdout
    if root:
        mp = os.path.join(_CACHE, 'mnt', os.path.basename(path))
        os.makedirs(mp, exist_ok=True)
        run(['pkexec', 'mount', '-o', 'loop,ro', path, mp])
        return {'path': mp, 'method': 'root', 'dev': None}
    if shutil.which('udisksctl') is None:
        raise RuntimeError("Permission denied: udisksctl not installed")
    out = run(['udisksctl', 'loop-setup', '-r', '-f', path])
    m = re.search(r'(/dev/loop\d+)', out)
    if not m:
        raise RuntimeError(out.strip())
    dev = m.group(1)
    out = run(['udisksctl', 'mount', '-b', dev])
    m = re.search(r' at (.+?)\.?$', out.strip())
    return {'path': m.group(1) if m else '', 'method': 'udisks', 'dev': dev}


def eject(info: dict): #vers 1
    """Unmount a mounted image or delete an extracted cache folder."""
    if info['method'] == 'cache':
        shutil.rmtree(info['path'], ignore_errors=True)
    elif info['method'] == 'sshfs':
        fm = shutil.which('fusermount3') or shutil.which('fusermount')
        subprocess.run([fm, '-u', info['path']], check=True, timeout=60)
        os.rmdir(info['path'])
    elif info['method'] == 'root':
        subprocess.run(['pkexec', 'umount', info['path']], check=True, timeout=60)
        os.rmdir(info['path'])
    else:
        subprocess.run(['udisksctl', 'unmount', '-b', info['dev']], check=True, timeout=60)
        subprocess.run(['udisksctl', 'loop-delete', '-b', info['dev']], check=True, timeout=60)
