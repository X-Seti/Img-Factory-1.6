#!/usr/bin/env python3
# apps/components/Ai_Workshop/depends/ssh_file_access.py - Version: 1
# AI Workshop - SSH localhost file access (read/write) via paramiko

import os
import io
import stat
from pathlib import Path


def _paramiko_available() -> bool:
    try:
        import paramiko
        return True
    except ImportError:
        return False


class SSHFileAccess:
    """
    Connects to a local SSH user account for file read/write.
    Supports password auth and optional SSH key.
    """

    def __init__(self):
        self.ssh    = None
        self.sftp   = None
        self.connected = False
        self.host   = "127.0.0.1"
        self.port   = 22
        self.username  = ""
        self.password  = ""
        self.key_path  = ""
        self.root_path = "/"

    # -----------------------------------------------------------------------
    # Connection
    # -----------------------------------------------------------------------

    def connect(self, host: str, port: int, username: str,
                password: str = "", key_path: str = "") -> tuple[bool, str]:
        """Connect and open SFTP channel. Returns (success, message)."""
        if not _paramiko_available():
            return False, (
                "paramiko not installed.\n"
                "Run:  pip install paramiko --break-system-packages"
            )

        import paramiko

        try:
            self.disconnect()

            client = paramiko.SSHClient()
            client.set_missing_host_key_policy(paramiko.AutoAddPolicy())

            connect_kwargs = dict(
                hostname=host,
                port=port,
                username=username,
                timeout=10,
            )

            if key_path and os.path.exists(key_path):
                connect_kwargs["key_filename"] = key_path
                if password:
                    connect_kwargs["passphrase"] = password
            elif password:
                connect_kwargs["password"] = password
            else:
                return False, "No password or key provided."

            client.connect(**connect_kwargs)

            self.ssh       = client
            self.sftp      = client.open_sftp()
            self.connected = True
            self.host      = host
            self.port      = port
            self.username  = username

            return True, f"Connected as {username}@{host}:{port}"

        except Exception as e:
            self.connected = False
            return False, str(e)

    def disconnect(self):
        try:
            if self.sftp:
                self.sftp.close()
            if self.ssh:
                self.ssh.close()
        except Exception:
            pass
        self.sftp = self.ssh = None
        self.connected = False

    # -----------------------------------------------------------------------
    # Directory listing
    # -----------------------------------------------------------------------

    def list_dir(self, remote_path: str) -> tuple[bool, list[dict]]:
        """
        List directory contents.
        Returns (success, [{'name', 'path', 'is_dir', 'size', 'modified'}])
        """
        if not self.connected or not self.sftp:
            return False, []
        try:
            entries = []
            for attr in self.sftp.listdir_attr(remote_path):
                is_dir = stat.S_ISDIR(attr.st_mode) if attr.st_mode else False
                entries.append({
                    "name":     attr.filename,
                    "path":     remote_path.rstrip("/") + "/" + attr.filename,
                    "is_dir":   is_dir,
                    "size":     attr.st_size or 0,
                    "modified": attr.st_mtime or 0,
                })
            entries.sort(key=lambda e: (not e["is_dir"], e["name"].lower()))
            return True, entries
        except Exception as e:
            return False, []

    # -----------------------------------------------------------------------
    # Read
    # -----------------------------------------------------------------------

    def read_text(self, remote_path: str, max_bytes: int = 512 * 1024) -> tuple[bool, str]:
        """Read a text file. Returns (success, content)."""
        if not self.connected or not self.sftp:
            return False, "Not connected"
        try:
            with self.sftp.open(remote_path, "r") as f:
                content = f.read(max_bytes)
            if isinstance(content, bytes):
                content = content.decode("utf-8", errors="replace")
            return True, content
        except Exception as e:
            return False, str(e)

    def read_binary(self, remote_path: str, max_bytes: int = 10 * 1024 * 1024) -> tuple[bool, bytes]:
        """Read a binary file (images etc). Returns (success, bytes)."""
        if not self.connected or not self.sftp:
            return False, b""
        try:
            with self.sftp.open(remote_path, "rb") as f:
                data = f.read(max_bytes)
            return True, data
        except Exception as e:
            return False, b""

    # -----------------------------------------------------------------------
    # Write
    # -----------------------------------------------------------------------

    def write_text(self, remote_path: str, content: str) -> tuple[bool, str]:
        """Write text to a remote file. Returns (success, message)."""
        if not self.connected or not self.sftp:
            return False, "Not connected"
        try:
            with self.sftp.open(remote_path, "w") as f:
                f.write(content)
            return True, f"Written: {remote_path}"
        except Exception as e:
            return False, str(e)

    def run_command(self, cmd: str, timeout: int = 30) -> tuple[int, str, str]:
        """Run a shell command as the SSH user. Returns (returncode, stdout, stderr)."""
        if not self.connected or not self.ssh:
            return -1, "", "Not connected"
        try:
            _, stdout, stderr = self.ssh.exec_command(cmd, timeout=timeout)
            rc  = stdout.channel.recv_exit_status()
            out = stdout.read().decode("utf-8", errors="replace")
            err = stderr.read().decode("utf-8", errors="replace")
            return rc, out, err
        except Exception as e:
            return -1, "", str(e)

    # -----------------------------------------------------------------------
    # Diff helper
    # -----------------------------------------------------------------------

    @staticmethod
    def make_diff(original: str, modified: str, filename: str = "file") -> str:
        """Return a unified diff string between original and modified content."""
        import difflib
        orig_lines = original.splitlines(keepends=True)
        mod_lines  = modified.splitlines(keepends=True)
        diff = difflib.unified_diff(
            orig_lines, mod_lines,
            fromfile=f"a/{filename}",
            tofile=f"b/{filename}",
        )
        return "".join(diff)


# ---------------------------------------------------------------------------
# Qt dialog for SSH settings
# ---------------------------------------------------------------------------

def create_ssh_settings_widget(parent=None, current: dict = None):
    """
    Returns a QGroupBox with SSH connection fields.
    current = {'host', 'port', 'username', 'password', 'key_path', 'root_path'}
    """
    from PyQt6.QtWidgets import (QGroupBox, QFormLayout, QLineEdit,
                                  QSpinBox, QPushButton, QHBoxLayout, QFileDialog)
    from PyQt6.QtCore import Qt

    current = current or {}
    group = QGroupBox("SSH File Access")
    form  = QFormLayout(group)

    host_edit = QLineEdit(current.get("host", "127.0.0.1"))
    form.addRow("Host:", host_edit)

    port_spin = QSpinBox()
    port_spin.setRange(1, 65535)
    port_spin.setValue(current.get("port", 22))
    form.addRow("Port:", port_spin)

    user_edit = QLineEdit(current.get("username", ""))
    form.addRow("Username:", user_edit)

    pass_edit = QLineEdit(current.get("password", ""))
    pass_edit.setEchoMode(QLineEdit.EchoMode.Password)
    form.addRow("Password:", pass_edit)

    key_row   = QHBoxLayout()
    key_edit  = QLineEdit(current.get("key_path", ""))
    key_browse = QPushButton("…")
    key_browse.setFixedWidth(30)
    def _browse_key():
        path, _ = QFileDialog.getOpenFileName(parent, "Select SSH Key", 
                                               os.path.expanduser("~/.ssh"))
        if path:
            key_edit.setText(path)
    key_browse.clicked.connect(_browse_key)
    key_row.addWidget(key_edit)
    key_row.addWidget(key_browse)
    form.addRow("SSH Key (optional):", key_row)

    root_row   = QHBoxLayout()
    root_edit  = QLineEdit(current.get("root_path", "/home"))
    root_browse = QPushButton("…")
    root_browse.setFixedWidth(30)
    root_row.addWidget(root_edit)
    root_row.addWidget(root_browse)
    form.addRow("Remote Root Path:", root_row)

    # Attach getters so caller can read values
    group.get_values = lambda: {
        "host":      host_edit.text().strip(),
        "port":      port_spin.value(),
        "username":  user_edit.text().strip(),
        "password":  pass_edit.text(),
        "key_path":  key_edit.text().strip(),
        "root_path": root_edit.text().strip(),
    }

    return group
