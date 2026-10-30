#this belongs in apps/components/Capture_Workshop/depends/capture_stream_func.py - Version: 3
# X-Seti - October 28 2026 - IMG Factory 1.6 - Capture network streams

"""
Network streams: RTSP cameras and IPTV (HTTP / HLS, .m3u playlists).
Preview: OpenCV (FFmpeg backend) in a reader thread. Recording: ffmpeg
copies the stream (video as sent, sound to AAC). Logins are kept apart
from the URL (user settings only, never in the repo) and hidden in the UI.
"""

##Methods list -
# mask_url
# parse_m3u
# stream_url
#
##class StreamDialog: -
# __init__
# _show_login
# _split_login
# entry
#
##class StreamRecorder: -
# __init__
# close
# is_open
#
##class StreamSource: -
# __init__
# _loop
# close
# open
# read
# size

import os
import re
import shutil
import subprocess
import threading
import time
from urllib.parse import quote, unquote, urlsplit, urlunsplit

from PyQt6.QtGui import QImage
from PyQt6.QtWidgets import QCheckBox, QDialog, QDialogButtonBox, QFormLayout, QLineEdit


def stream_url(entry: dict) -> str: #vers 1
    """Full URL for a saved stream: login (if any) put into the address."""
    url = entry.get('url', '').strip()
    user = entry.get('user', '')
    if not user:
        return url
    p = urlsplit(url)
    host = p.netloc.rsplit('@', 1)[-1]
    auth = quote(user, safe='') + (':' + quote(entry.get('password', ''), safe='') if entry.get('password') else '')
    return urlunsplit((p.scheme, f"{auth}@{host}", p.path, p.query, p.fragment))


def mask_url(url: str) -> str: #vers 1
    """URL with any login hidden: rtsp://***@host/..."""
    return re.sub(r'://[^/@]+@', '://***@', url)


def parse_m3u(text: str) -> list: #vers 1
    """[(name, url)] from an .m3u / .m3u8 IPTV playlist."""
    out, name = [], None
    for line in text.splitlines():
        line = line.strip()
        if line.startswith('#EXTINF'):
            name = line.split(',', 1)[1].strip() if ',' in line else None
        elif line and not line.startswith('#'):
            out.append((name or line.rsplit('/', 1)[-1], line))
            name = None
    return out


class StreamSource: #vers 1
    """Newest frame of a network stream, read in its own thread (reconnects)."""

    def __init__(self): #vers 1
        self.url = None
        self.error = ''
        self._frame = None
        self._lock = threading.Lock()
        self._gen = 0                                    # bumped to stop the current thread
        self._thread = None

    def open(self, url: str, rtsp_tcp: bool = True): #vers 2
        """Start connecting / reading url in the background (the UI never waits).
        Same stream already running: kept (cameras allow few sessions)."""
        if url == self.url and self._thread is not None and self._thread.is_alive():
            return
        prev = self._thread
        self.close()
        if rtsp_tcp and url.lower().startswith('rtsp'):
            os.environ['OPENCV_FFMPEG_CAPTURE_OPTIONS'] = 'rtsp_transport;tcp'
        else:
            os.environ.pop('OPENCV_FFMPEG_CAPTURE_OPTIONS', None)
        self.url, self.error = url, 'Connecting...'
        self._gen += 1
        self._thread = threading.Thread(target=self._loop, args=(self._gen, url, prev), daemon=True)
        self._thread.start()

    def _loop(self, gen: int, url: str, prev=None): #vers 2
        """Reader thread: waits for the previous one to hang up (one camera
        session at a time), connects, keeps the newest frame, reconnects with
        a growing pause (2 .. 10 s). Ends when open() / close() changes the generation."""
        import cv2
        if prev is not None:
            prev.join()
        cap, wait = None, 2
        while gen == self._gen:
            if cap is None:
                cap = cv2.VideoCapture(url, cv2.CAP_FFMPEG)
                if not cap.isOpened():
                    cap.release()
                    cap = None
                    if gen == self._gen:
                        self.error = (f"Can't connect to {mask_url(url)} - retrying in {wait} s "
                                      "(camera busy? close other viewers)")
                    time.sleep(wait)
                    wait = min(wait * 2, 10)
                    continue
                if gen != self._gen:
                    break
                wait = 2
                if gen == self._gen:
                    self.error = ''
            ok, frame = cap.read()
            if ok:
                if gen == self._gen:
                    with self._lock:
                        self._frame = frame
                continue
            cap.release()
            cap = None
            if gen == self._gen:
                self.error = "Stream lost - reconnecting"
            time.sleep(2)
        if cap is not None:
            cap.release()

    def read(self): #vers 1
        """(bgr frame, QImage) of the newest frame, or (None, None)."""
        import cv2
        with self._lock:
            frame = self._frame
        if frame is None:
            return None, None
        rgb = cv2.cvtColor(frame, cv2.COLOR_BGR2RGB)
        h, w, ch = rgb.shape
        return frame, QImage(rgb.data, w, h, ch * w, QImage.Format.Format_RGB888).copy()

    def size(self): #vers 1
        with self._lock:
            frame = self._frame
        return (frame.shape[1], frame.shape[0]) if frame is not None else (0, 0)

    def close(self): #vers 2
        """Stop the reader (it hangs up on its own, the next open waits for that)."""
        self._gen += 1
        self.url = None
        with self._lock:
            self._frame = None


class StreamRecorder: #vers 1
    """ffmpeg records the stream itself: video copied, sound as AAC."""

    def __init__(self, url: str, path: str, rtsp_tcp: bool = True): #vers 1
        self.path = path
        self.proc = None
        if not shutil.which('ffmpeg'):
            return
        cmd = ['ffmpeg', '-hide_banner', '-loglevel', 'error', '-y']
        if rtsp_tcp and url.lower().startswith('rtsp'):
            cmd += ['-rtsp_transport', 'tcp']
        cmd += ['-i', url, '-map', '0:v:0', '-map', '0:a:0?', '-c:v', 'copy', '-c:a', 'aac', path]
        self.proc = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.DEVNULL,
                                     stderr=subprocess.DEVNULL)

    def is_open(self) -> bool: #vers 1
        return self.proc is not None and self.proc.poll() is None

    def close(self): #vers 1
        """Ask ffmpeg to finish the file ('q'), then wait."""
        if self.proc is None:
            return
        if self.proc.poll() is None:
            try:
                self.proc.stdin.write(b'q')
                self.proc.stdin.flush()
            except (BrokenPipeError, OSError):
                pass                                     # ffmpeg already ended
            try:
                self.proc.wait(timeout=10)
            except subprocess.TimeoutExpired:
                self.proc.kill()
        self.proc = None


class StreamDialog(QDialog):
    """Add / edit a stream: name, URL, login (only when ticked), RTSP over TCP."""

    def __init__(self, parent, entry=None): #vers 2
        super().__init__(parent)
        self.setWindowTitle("Network Stream")
        self.setMinimumWidth(480)
        e = entry or {}
        form = self._form = QFormLayout(self)
        self.name = QLineEdit(e.get('name', ''))
        self.url = QLineEdit(e.get('url', ''))
        self.url.setPlaceholderText("rtsp://192.168.1.10:554/stream  or  http://.../live.m3u8")
        self.needs_login = QCheckBox("This connection needs a password")
        self.user = QLineEdit(e.get('user', ''))
        self.password = QLineEdit(e.get('password', ''))
        self.password.setEchoMode(QLineEdit.EchoMode.Password)
        self.tcp = QCheckBox("RTSP over TCP (steadier on Wi-Fi)")
        self.tcp.setChecked(e.get('tcp', True))
        form.addRow("Name:", self.name)
        form.addRow("URL:", self.url)
        form.addRow("", self.needs_login)
        form.addRow("User:", self.user)
        form.addRow("Password:", self.password)
        form.addRow("", self.tcp)
        btns = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        btns.accepted.connect(self.accept)
        btns.rejected.connect(self.reject)
        form.addRow(btns)
        self.needs_login.toggled.connect(self._show_login)
        self.url.editingFinished.connect(self._split_login)
        self.needs_login.setChecked(bool(e.get('user')))
        self._show_login(self.needs_login.isChecked())

    def _show_login(self, on): #vers 1
        """User / Password rows only when the connection needs a password."""
        self._form.setRowVisible(self.user, on)
        self._form.setRowVisible(self.password, on)
        if on:
            self.user.setFocus()
        self.adjustSize()

    def _split_login(self): #vers 1
        """A login pasted into the URL moves to User / Password (box ticked)."""
        m = re.match(r'([a-z]+://)([^/@:]+)(?::([^/@]*))?@(.*)', self.url.text().strip(), re.I)
        if m:
            self.user.setText(unquote(m.group(2)))
            self.password.setText(unquote(m.group(3) or ''))
            self.url.setText(m.group(1) + m.group(4))
            self.needs_login.setChecked(True)

    def entry(self) -> dict: #vers 2
        """The stream as saved; no login kept when the box is not ticked."""
        self._split_login()
        url = self.url.text().strip()
        login = self.needs_login.isChecked()
        return {'name': self.name.text().strip() or mask_url(url), 'url': url,
                'user': self.user.text() if login else '', 'password': self.password.text() if login else '',
                'tcp': self.tcp.isChecked()}
