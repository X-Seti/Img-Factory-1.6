#this belongs in apps/methods/dir_sync.py - Version: 2
# X-Seti - November 16 2026 - IMG Factory 1.6 - Folder Mirror and Clone through rsync

"""
Dir Sync - rsync Mirror (update older files) and Clone (exact copy) for the twin Dir List.
"""

##Methods list -
# parse_plan
# remove_paths
# rsync_cmd
# rsync_path
# top_level

##class SyncJob: -
# __init__
# cancel
# run

import os
import re
import shutil
import subprocess

from PyQt6.QtCore import QThread, pyqtSignal

_ITEM = re.compile(r'^([<>ch.*][a-zA-Z][^ ]{9}) (.+)$')
_DEL = re.compile(r'^\*deleting\s+(.+)$')


def rsync_path(): #vers 1
    """Path of the rsync program, or None."""
    return shutil.which('rsync')


def rsync_cmd(src: str, dst: str, mode: str, dry: bool = False,
              delete: bool = False, root: bool = False) -> list: #vers 1
    """mode 'mirror' skips files newer on dst; 'clone' makes dst identical to src."""
    cmd = ['rsync', '-aHAX', '--numeric-ids']
    if mode == 'mirror':
        cmd.append('--update')
    if dry:
        cmd += ['-n', '--delete', '--itemize-changes']
    else:
        cmd += ['--info=name1,progress2']
        if delete:
            cmd.append('--delete')
    cmd += [src.rstrip('/') + '/', dst.rstrip('/') + '/']
    return (['pkexec'] + cmd) if root else cmd


def parse_plan(lines, dst: str): #vers 1
    """From a dry run: (copies, new_count, extras as full dst paths)."""
    copies, new, extras = [], 0, []
    for ln in lines:
        m = _DEL.match(ln)
        if m:
            extras.append(os.path.join(dst, m.group(1)))
            continue
        m = _ITEM.match(ln)
        if not m:
            continue
        flags, name = m.groups()
        is_new = '+++++++++' in flags
        if flags[0] in '<>' or is_new:
            copies.append(name)
            new += is_new
    return copies, new, extras


def top_level(extras) -> list: #vers 1
    """Extras not inside another extra folder."""
    dirs = [p for p in extras if p.endswith('/')]
    return [p for p in extras if not any(p != d and p.startswith(d) for d in dirs)]


def remove_paths(paths, permanent: bool) -> int: #vers 2
    """Trash or delete paths; returns the number removed."""
    from PyQt6.QtCore import QFile
    done = 0
    for p in paths:
        p = p.rstrip('/')
        if not os.path.lexists(p):
            done += 1
            continue
        if permanent:
            shutil.rmtree(p) if os.path.isdir(p) and not os.path.islink(p) else os.remove(p)
            done += 1
        elif QFile.moveToTrash(p):
            done += 1
    return done


class SyncJob(QThread): #vers 1
    """Runs a command; line = latest output, done = (return code, errors, all lines)."""
    line = pyqtSignal(str)
    done = pyqtSignal(int, str, list)

    def __init__(self, cmd, parent=None): #vers 1
        super().__init__(parent)
        self._cmd = cmd
        self._proc = None
        self._cancelled = False

    def cancel(self): #vers 1
        """Stop the running command."""
        self._cancelled = True
        if self._proc is not None and self._proc.poll() is None:
            self._proc.terminate()

    def run(self): #vers 1
        lines = []
        try:
            self._proc = subprocess.Popen(self._cmd, stdout=subprocess.PIPE,
                                          stderr=subprocess.STDOUT, text=True, bufsize=1)
            for ln in self._proc.stdout:
                ln = ln.rstrip('\n')
                if ln:
                    lines.append(ln)
                    self.line.emit(ln)
            rc = self._proc.wait()
        except OSError as e:
            self.done.emit(127, str(e), lines)
            return
        errs = [l for l in lines if not _ITEM.match(l) and not _DEL.match(l)]
        self.done.emit(-1 if self._cancelled else rc, '\n'.join(errs[-8:]), lines)
