#this belongs in apps/components/Capture_Workshop/depends/capture_sources_func.py - Version: 2
# X-Seti - October 14 2026 - IMG Factory 1.6 - Capture sources

"""
Capture sources: HDMI/USB devices (OpenCV), Windows screens and windows.
Linux screens/windows go through capture_portal_func.py.
"""

##Methods list -
# grab_screen
# grab_window_area
# is_linux
# list_audio_inputs
# list_devices
# list_windows
# missing_gst_elements

##class DeviceSource: -
# __init__
# close
# open
# read
# size

##class AudioSource: -
# __init__
# _callback
# close
# open

##class AudioRecorder: -
# __init__
# add
# close

##class FrameRecorder: -
# __init__
# add
# close
# is_open

import os
import shutil
import subprocess
import sys

from PyQt6.QtCore import QRect
from PyQt6.QtGui import QGuiApplication, QImage
import wave
try:
    import pyaudio
    HAS_PYAUDIO = True
except ImportError:
    HAS_PYAUDIO = False

GST_MP4 = ('pipewiresrc', 'videoconvert', 'x264enc', 'h264parse', 'mp4mux')
GST_WEBM = ('pipewiresrc', 'videoconvert', 'vp8enc', 'webmmux')


def is_linux(): #vers 1
    return sys.platform.startswith('linux')


def list_devices(): #vers 1
    """[(index, label)] of HDMI/USB capture devices that give a frame."""
    import cv2
    if is_linux():
        nodes = sorted(int(n[5:]) for n in os.listdir('/dev') if n.startswith('video') and n[5:].isdigit())
    else:
        nodes = list(range(5))
    found = []
    for i in nodes:
        cap = cv2.VideoCapture(i, cv2.CAP_V4L2 if is_linux() else cv2.CAP_ANY)
        if cap.isOpened() and cap.read()[0]:
            found.append((i, f"Device {i}" + (f" (/dev/video{i})" if is_linux() else "")))
        cap.release()
    return found


def grab_screen(index): #vers 1
    """QImage of one screen (Windows; Linux uses the portal)."""
    screen = QGuiApplication.screens()[index]
    return screen.grabWindow(0).toImage()


def list_windows(): #vers 1
    """[(hwnd, title)] of visible top-level windows (Windows only)."""
    import ctypes
    from ctypes import wintypes
    user32 = ctypes.windll.user32
    out = []

    def _each(hwnd, _lp):  #vers 1
        n = user32.GetWindowTextLengthW(hwnd)
        if user32.IsWindowVisible(hwnd) and n:
            buf = ctypes.create_unicode_buffer(n + 1)
            user32.GetWindowTextW(hwnd, buf, n + 1)
            out.append((int(hwnd), buf.value))
        return True
    proto = ctypes.WINFUNCTYPE(wintypes.BOOL, wintypes.HWND, wintypes.LPARAM)
    user32.EnumWindows(proto(_each), 0)
    return out


def grab_window_area(hwnd): #vers 1
    """QImage of the screen area a window covers (Windows only)."""
    import ctypes
    from ctypes import wintypes
    r = wintypes.RECT()
    ctypes.windll.user32.GetWindowRect(hwnd, ctypes.byref(r))
    rect = QRect(r.left, r.top, r.right - r.left, r.bottom - r.top)
    screen = QGuiApplication.screenAt(rect.center()) or QGuiApplication.primaryScreen()
    g = screen.geometry()
    return screen.grabWindow(0, rect.x() - g.x(), rect.y() - g.y(), rect.width(), rect.height()).toImage()


def missing_gst_elements(fmt): #vers 1
    """GStreamer elements needed for portal recording that are not installed."""
    if not shutil.which('gst-launch-1.0'):
        return ['gst-launch-1.0']
    need = GST_MP4 if fmt == 'mp4' else GST_WEBM
    return [e for e in need if subprocess.run(['gst-inspect-1.0', e], capture_output=True).returncode]


class DeviceSource: #vers 1
    """One OpenCV capture device."""

    def __init__(self): #vers 1
        self.cap = None

    def open(self, index, width=1920, height=1080, fps=30): #vers 1
        import cv2
        self.close()
        self.cap = cv2.VideoCapture(index, cv2.CAP_V4L2 if is_linux() else cv2.CAP_ANY)
        self.cap.set(cv2.CAP_PROP_FOURCC, cv2.VideoWriter_fourcc(*'MJPG'))
        self.cap.set(cv2.CAP_PROP_FRAME_WIDTH, width)
        self.cap.set(cv2.CAP_PROP_FRAME_HEIGHT, height)
        self.cap.set(cv2.CAP_PROP_FPS, fps)
        return self.cap.isOpened()

    def size(self): #vers 1
        import cv2
        return (int(self.cap.get(cv2.CAP_PROP_FRAME_WIDTH)), int(self.cap.get(cv2.CAP_PROP_FRAME_HEIGHT)))

    def read(self): #vers 1
        """(bgr frame, QImage) or (None, None)."""
        import cv2
        if self.cap is None or not self.cap.isOpened():
            return None, None
        ok, frame = self.cap.read()
        if not ok:
            return None, None
        rgb = cv2.cvtColor(frame, cv2.COLOR_BGR2RGB)
        h, w, ch = rgb.shape
        return frame, QImage(rgb.data, w, h, ch * w, QImage.Format.Format_RGB888).copy()

    def close(self): #vers 1
        if self.cap is not None:
            self.cap.release()
            self.cap = None


def list_audio_inputs(): #vers 1
    """[(index, name)] for every audio input (capture cards, HDMI, mics)."""
    if not HAS_PYAUDIO:
        return []
    p = pyaudio.PyAudio()
    try:
        return [(i, p.get_device_info_by_index(i)['name']) for i in range(p.get_device_count())
                if p.get_device_info_by_index(i).get('maxInputChannels', 0) > 0]
    finally:
        p.terminate()


class AudioSource:
    """Input stream from the chosen device; plays it (monitor) and feeds a recorder."""
    def __init__(self, chunk=1024): #vers 2
        self.rate = 48000
        self.channels = 2
        self.chunk = chunk
        self.p = None
        self.stream = None
        self.is_active = False
        self.monitor = True
        self.recorder = None

    def open(self, device_index): #vers 2
        """Open device_index (callback stream, own thread). False if it can't."""
        self.close()
        if not HAS_PYAUDIO or device_index is None:
            return False
        try:
            self.p = pyaudio.PyAudio()
            info = self.p.get_device_info_by_index(device_index)
            self.channels = max(1, min(2, int(info.get('maxInputChannels', 2))))
            for rate in (48000, 44100, int(info.get('defaultSampleRate', 48000)), 32000, 16000):
                try:
                    if self.p.is_format_supported(rate, input_device_index=device_index,
                                                  input_channels=self.channels, input_format=pyaudio.paInt16):
                        self.rate = rate
                        break
                except ValueError:
                    continue
            self.stream = self.p.open(format=pyaudio.paInt16, channels=self.channels, rate=self.rate,
                                      input=True, input_device_index=device_index, output=True,
                                      frames_per_buffer=self.chunk, stream_callback=self._callback)
            self.is_active = True
            return True
        except Exception as e:
            print(f"Audio open error: {e}")
            self.close()
            return False

    def _callback(self, in_data, frame_count, time_info, status): #vers 1
        """Audio thread: record, then play through (silence when not monitoring)."""
        rec = self.recorder
        if rec is not None:
            rec.add(in_data)
        out = in_data if self.monitor else bytes(len(in_data))
        return out, pyaudio.paContinue

    def close(self): #vers 2
        self.is_active = False
        self.recorder = None
        if self.stream:
            try:
                self.stream.stop_stream()
                self.stream.close()
            except OSError:
                pass
            self.stream = None
        if self.p:
            self.p.terminate()
            self.p = None


class AudioRecorder:
    """WAV file for the audio of a recording (muxed into the video afterwards)."""
    def __init__(self, path, rate, channels): #vers 2
        self.path = path
        self.wf = wave.open(path, 'wb')
        self.wf.setnchannels(channels)
        self.wf.setsampwidth(2)                         # paInt16
        self.wf.setframerate(rate)

    def add(self, data): #vers 2
        if data:
            self.wf.writeframes(data)

    def close(self): #vers 2
        self.wf.close()


class FrameRecorder: #vers 1
    """MP4 writer for device frames (BGR) or QImages."""

    def __init__(self, path, width, height, fps=30.0): #vers 1
        import cv2
        self.size = (width, height)
        self.writer = cv2.VideoWriter(path, cv2.VideoWriter_fourcc(*'mp4v'), fps, self.size)

    def is_open(self): #vers 1
        return self.writer.isOpened()

    def add(self, frame=None, image=None): #vers 1
        """Write a BGR frame, or a QImage (scaled to the recording size)."""
        import cv2
        import numpy as np
        if image is not None:
            img = image.convertToFormat(QImage.Format.Format_RGB888)
            if (img.width(), img.height()) != self.size:
                img = img.scaled(*self.size)
            arr = np.frombuffer(img.constBits().asstring(img.sizeInBytes()), np.uint8)
            arr = arr.reshape(img.height(), img.bytesPerLine())[:, :img.width() * 3]
            frame = cv2.cvtColor(arr.reshape(img.height(), img.width(), 3), cv2.COLOR_RGB2BGR)
        self.writer.write(frame)

    def close(self): #vers 1
        self.writer.release()
