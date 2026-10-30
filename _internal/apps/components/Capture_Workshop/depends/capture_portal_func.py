#this belongs in apps/components/Capture_Workshop/depends/capture_portal_func.py - Version: 1
# X-Seti - October 14 2026 - IMG Factory 1.6 - Capture portal (Linux)

"""
Linux screen and window capture through xdg-desktop-portal:
Screenshot (stills) and ScreenCast + GStreamer pipewiresrc (video).
"""

##class PortalRequest: -
# __init__
# _on_response
# call

##class PortalScreenshot: -
# __init__
# _done
# take

##class PortalScreenCast: -
# __init__
# _check_proc
# _create_done
# _fail
# _select_done
# _start_done
# start
# stop

import os
import secrets
import signal
import subprocess

from PyQt6.QtCore import QObject, QUrl, pyqtSignal, pyqtSlot
from PyQt6.QtDBus import QDBusMessage

from apps.components.Capture_Workshop.depends.capture_sources_func import missing_gst_elements

PORTAL = ('org.freedesktop.portal.Desktop', '/org/freedesktop/portal/desktop')


class PortalRequest(QObject): #vers 1
    """One portal method call; response(code, results) when the Request answers."""

    response = pyqtSignal(int, dict)

    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        from PyQt6.QtDBus import QDBusConnection
        self.bus = QDBusConnection.sessionBus()
        self.token = 'imgf' + secrets.token_hex(6)
        sender = self.bus.baseService()[1:].replace('.', '_')
        self.path = f'/org/freedesktop/portal/desktop/request/{sender}/{self.token}'

    def call(self, interface, method, *args): #vers 1
        """args end with the options dict; handle_token is added. Error text or ''."""
        self.bus.connect('', self.path, 'org.freedesktop.portal.Request', 'Response', self._on_response)
        args = list(args)
        args[-1] = dict(args[-1], handle_token=self.token)
        msg = QDBusMessage.createMethodCall(PORTAL[0], PORTAL[1], interface, method)
        msg.setArguments(args)
        reply = self.bus.call(msg)
        if reply.type() == QDBusMessage.MessageType.ErrorMessage:
            return reply.errorMessage() or reply.errorName()
        return ''

    @pyqtSlot(QDBusMessage)
    def _on_response(self, msg): #vers 1
        self.bus.disconnect('', self.path, 'org.freedesktop.portal.Request', 'Response', self._on_response)
        code, results = msg.arguments()[:2]
        self.response.emit(int(code), dict(results))


class PortalScreenshot(QObject): #vers 1
    """Desktop or window still through the screenshot portal."""

    finished = pyqtSignal(str, str)          # local file path, error

    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self._req = None

    def take(self, interactive): #vers 1
        """interactive=True lets the user pick a window or area."""
        self._req = PortalRequest(self)
        self._req.response.connect(self._done)
        err = self._req.call('org.freedesktop.portal.Screenshot', 'Screenshot', '',
                             {'interactive': bool(interactive)})
        if err:
            self.finished.emit('', err)

    def _done(self, code, results): #vers 1
        if code:
            self.finished.emit('', 'Cancelled' if code == 1 else 'Screenshot failed')
            return
        self.finished.emit(QUrl(results.get('uri', '')).toLocalFile(), '')


class PortalScreenCast(QObject): #vers 1
    """Screen or window video: portal ScreenCast session recorded by GStreamer."""

    started = pyqtSignal(str)                # output path
    failed = pyqtSignal(str)
    stopped = pyqtSignal(str)

    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self._req = None
        self._session = ''
        self._proc = None
        self._path = ''
        self._types = 1
        self._fmt = 'mp4'

    def start(self, window, path, fmt): #vers 1
        """window=True picks a window, else a screen. fmt 'mp4' or 'webm'."""
        missing = missing_gst_elements(fmt)
        if missing:
            self.failed.emit("GStreamer elements missing: " + ", ".join(missing)
                             + "\n(install gstreamer, gst-plugin-pipewire, gst-plugins-good/ugly)")
            return
        self._types, self._path, self._fmt = (2 if window else 1), path, fmt
        self._req = PortalRequest(self)
        self._req.response.connect(self._create_done)
        err = self._req.call('org.freedesktop.portal.ScreenCast', 'CreateSession',
                             {'session_handle_token': 'imgf' + secrets.token_hex(6)})
        if err:
            self._fail(err)

    def _fail(self, text): #vers 1
        self.failed.emit(text)

    def _create_done(self, code, results): #vers 1
        from PyQt6.QtCore import QMetaType
        from PyQt6.QtDBus import QDBusArgument, QDBusObjectPath
        if code:
            return self._fail('Screen recording cancelled')
        self._session = results['session_handle']
        types, cursor = QDBusArgument(), QDBusArgument()
        types.add(self._types, QMetaType.Type.UInt.value)
        cursor.add(2, QMetaType.Type.UInt.value)            # cursor drawn into the video
        self._req = PortalRequest(self)
        self._req.response.connect(self._select_done)
        err = self._req.call('org.freedesktop.portal.ScreenCast', 'SelectSources',
                             QDBusObjectPath(self._session),
                             {'types': types, 'multiple': False, 'cursor_mode': cursor})
        if err:
            self._fail(err)

    def _select_done(self, code, results): #vers 1
        from PyQt6.QtDBus import QDBusObjectPath
        if code:
            return self._fail('Screen recording cancelled')
        self._req = PortalRequest(self)
        self._req.response.connect(self._start_done)
        err = self._req.call('org.freedesktop.portal.ScreenCast', 'Start',
                             QDBusObjectPath(self._session), '', {})
        if err:
            self._fail(err)

    def _start_done(self, code, results): #vers 1
        from PyQt6.QtDBus import QDBusConnection, QDBusObjectPath
        if code or not results.get('streams'):
            return self._fail('Screen recording cancelled')
        node = int(results['streams'][0][0])
        msg = QDBusMessage.createMethodCall(PORTAL[0], PORTAL[1], 'org.freedesktop.portal.ScreenCast',
                                            'OpenPipeWireRemote')
        msg.setArguments([QDBusObjectPath(self._session), {}])
        reply = QDBusConnection.sessionBus().call(msg)
        if reply.type() == QDBusMessage.MessageType.ErrorMessage:
            return self._fail(reply.errorMessage())
        fd = os.dup(reply.arguments()[0].fileDescriptor())
        enc = (['x264enc', 'tune=zerolatency', 'speed-preset=veryfast', '!', 'h264parse', '!', 'mp4mux']
               if self._fmt == 'mp4' else ['vp8enc', 'deadline=1', '!', 'webmmux'])
        cmd = (['gst-launch-1.0', '-e', 'pipewiresrc', f'fd={fd}', f'path={node}', 'do-timestamp=true',
                '!', 'videoconvert', '!', 'queue', '!'] + enc + ['!', 'filesink', f'location={self._path}'])
        import tempfile
        self._log = tempfile.TemporaryFile()
        self._proc = subprocess.Popen(cmd, pass_fds=(fd,), stdout=subprocess.DEVNULL, stderr=self._log)
        os.close(fd)
        from PyQt6.QtCore import QTimer
        QTimer.singleShot(2000, self._check_proc)
        self.started.emit(self._path)

    def _check_proc(self): #vers 1
        """GStreamer quit early: report its error output."""
        if self._proc is None or self._proc.poll() is None:
            return
        self._log.seek(0)
        text = self._log.read().decode(errors='replace').strip().splitlines()[-3:]
        self._proc = None
        self._fail("Recorder stopped: " + " ".join(text))

    def stop(self): #vers 1
        """End the recording (EOS so the file is finalised) and close the session."""
        from PyQt6.QtDBus import QDBusConnection
        if self._proc is not None:
            self._proc.send_signal(signal.SIGINT)
            self._proc.wait(timeout=15)
            self._proc = None
        if self._session:
            msg = QDBusMessage.createMethodCall(PORTAL[0], self._session,
                                                'org.freedesktop.portal.Session', 'Close')
            QDBusConnection.sessionBus().call(msg)
            self._session = ''
        self.stopped.emit(self._path)
