#!/usr/bin/env python3
#this belongs in apps/components/Capture_Workshop/capture_workshop.py - Version: 12
# X-Seti - October 14 2026 - IMG Factory 1.6 - Capture Workshop
# Built on GUIWorkshop base (same as Water Workshop)

"""
Capture Workshop - HDMI/USB capture devices, screen and window capture,
network streams (RTSP cameras, IPTV playlists).
Stills (PNG/JPG) and video (MP4, WebM on Linux screen recording).
Bugs tab: bug tracker on the TODO_bugs folder (capture_bugs_func.py).
"""

##Methods list -
# open_capture_workshop

##class CaptureWorkshop: -
# __init__
# _after_capture
# _audio_changed
# _build_menus_into_qmenu
# _build_ribbons
# _capture_menu
# _capture_name
# _choose_folder
# _create_centre_panel
# _create_left_panel
# _create_toolbar
# _delayed
# _device_snapshot
# _edit_stream
# _fill_audio_combo
# _fill_source_combo
# _frame_source
# _import_playlist
# _list_menu
# _load_captures
# _open_audio
# _open_in_dp5
# _output_dir
# _portal_cast_failed
# _portal_cast_started
# _portal_cast_stopped
# _portal_shot_done
# _record_started
# _remove_stream
# _refresh_sources
# _rename
# _save_image
# _screen_snapshot
# _set_mode
# _show_image
# _stream_entry
# _source_changed
# _start_recording
# _stop_recording
# _tick
# _tick_cast
# _toggle_audio
# _trash
# _toggle_recording
# closeEvent
# setup_ui
# snapshot

##Mixins (depends/) -
# CaptureBugsMixin - capture_bugs_func.py

import os
import shutil
import subprocess
import sys
from datetime import datetime
from pathlib import Path

_root = Path(__file__).resolve().parents[3]       # project root for standalone runs
if str(_root) not in sys.path:
    sys.path.insert(0, str(_root))

from PyQt6.QtCore import QFile, QSize, Qt, QTimer, QUrl
from PyQt6.QtGui import QDesktopServices, QGuiApplication, QIcon, QImage, QPixmap, QShortcut
from PyQt6.QtWidgets import (
    QApplication, QCheckBox, QComboBox, QFileDialog, QFrame, QInputDialog, QLabel,
    QListWidget, QListWidgetItem, QMenu, QMessageBox, QSizePolicy, QSpinBox, QSplitter,
    QStackedWidget, QTabWidget, QVBoxLayout)

from apps.methods.statusbar_func import register_statusbar
from apps.methods.gui_workshop import GUIWorkshop
from apps.methods.img_factory_settings import get_user_config_dir
from apps.methods.imgfactory_svg_icons import SVGIconFactory
from apps.methods.ribbon_system import RibbonMixin
from apps.methods.ribbon_dialog import RibbonIconsMixin
from apps.components.Capture_Workshop.depends.capture_bugs_func import CaptureBugsMixin
from apps.components.Capture_Workshop.depends.capture_sources_func import (
    DeviceSource, FrameRecorder, AudioSource, AudioRecorder, HAS_PYAUDIO,
    grab_screen, grab_window_area, is_linux, list_audio_inputs, list_devices, list_windows)
from apps.components.Capture_Workshop.depends.capture_stream_func import (
    StreamDialog, StreamRecorder, StreamSource, mask_url, parse_m3u, stream_url)

App_name  = "Capture Workshop"
Build     = "Build 8"
IMAGE_EXT = ('.png', '.jpg', '.jpeg')
VIDEO_EXT = ('.mp4', '.webm')


class CaptureWorkshop(CaptureBugsMixin, RibbonIconsMixin, RibbonMixin, GUIWorkshop): #vers 3

    App_name        = App_name
    App_build       = Build
    App_author      = "X-Seti"
    App_year        = "2026"
    App_description = ("HDMI / USB capture devices, screen and window capture.\n"
                       "Stills (PNG / JPG) and video (MP4 / WebM).")
    config_key      = "capture_workshop"
    WS_DEFAULTS     = {"source_mode": "device", "delay": 0, "hide_on_capture": True,
                       "image_format": "PNG", "video_format": "MP4", "output_folder": "",
                       "bugs_folder": "", "changelog_file": "",
                       "audio_enabled": True, "audio_device": "",
                       "streams": [], "stream_index": 0}      # streams: user settings only
    _ribbon_name    = "capture_workshop"
    _RIBBON_LAYOUT_VERSION = 2          # Source / Capture / Output / Bugs ribbons

    def __init__(self, parent=None, main_window=None): #vers 5
        self._device = DeviceSource()
        self._stream = StreamSource()   # RTSP / IPTV (reader thread)
        self._stream_rec = None         # StreamRecorder (ffmpeg)
        self._mode = 'device'           # device / screen / window / stream
        self._recorder = None           # FrameRecorder (device, Windows screen)
        self._portal_shot = None
        self._portal_cast = None
        self._last_image = None
        self._rec_start = None
        super().__init__(parent, main_window)
        self.setWindowIcon(SVGIconFactory.capture_workshop_icon(64))
        self.ribbon_restore_state()
        self._inner_mw = self._ribbon_mw      # Ribbon Manager target
        self._apply_ribbon_layout()
        self._audio_source = AudioSource()
        self._audio_recorder = None
        self._fill_audio_combo()
        self._timer = QTimer(self)
        self._timer.timeout.connect(self._tick)
        self._set_mode(self.WS.get("source_mode", 'device'))
        self._load_captures()
        self._load_bugs()

    def _create_toolbar(self): #vers 1
        """Titlebar keeps Settings / title / Info / Theme; file buttons unused."""
        tb = super()._create_toolbar()
        for name in ("open_btn", "save_btn", "export_btn", "import_btn", "undo_btn"):
            btn = getattr(self, name, None)
            if btn:
                btn.setVisible(False)
        return tb

    def setup_ui(self): #vers 3
        """Titlebar / Captures dock | ribbon-wrapped preview / status bar."""
        ml = QVBoxLayout(self)
        ml.setContentsMargins(*self.get_content_margins())
        ml.setSpacing(self.setspacing)
        ml.addWidget(self._create_toolbar())
        left = self._create_left_panel()
        ml.addWidget(self._dock_layout(self.ribbon_wrap(self._create_centre_panel()),
                                       [("Captures Controls", left, 220, Qt.DockWidgetArea.LeftDockWidgetArea)]), 1)
        self._build_ribbons()
        self._status_widget = self._create_status_bar()
        ml.addWidget(self._status_widget)
        register_statusbar(self, self._status_widget)


    def _build_ribbons(self): #vers 4
        """Source / Capture / Output / Bugs ribbons."""
        B = self.ribbon_button
        tb = self.ribbon_toolbar("Source")

        self._mode_btns = {
            'device': B(tb, "video_device_icon", "HDMI / USB capture device",
                        lambda checked=False: self._set_mode('device'), checkable=True),
            'screen': B(tb, "desktop_capture_icon", "Whole screen",
                        lambda checked=False: self._set_mode('screen'), checkable=True),
            'window': B(tb, "window_capture_icon", "Chosen window",
                        lambda checked=False: self._set_mode('window'), checkable=True),
            'stream': B(tb, "globe_icon", "Network stream: RTSP camera / IPTV",
                        lambda checked=False: self._set_mode('stream'), checkable=True)}
        self._source_combo = QComboBox()
        self._source_combo.setMinimumWidth(220)
        self._source_combo.setToolTip("Device / screen / window to capture")
        self._source_combo.currentIndexChanged.connect(self._source_changed)
        tb.addWidget(self._source_combo)
        self._audio_combo = QComboBox()
        self._audio_combo.setMinimumWidth(200)
        self._audio_combo.setToolTip("Audio input: pick the capture card / HDMI input that carries the sound")
        self._audio_combo.currentIndexChanged.connect(self._audio_changed)
        tb.addWidget(self._audio_combo)
        B(tb, "get_refresh_icon", "Rescan devices / windows / audio inputs", self._refresh_sources)
        self._stream_btns = [
            B(tb, "add_icon", "Add network stream (RTSP / IPTV URL)", lambda: self._edit_stream(new=True)),
            B(tb, "edit_icon", "Edit the selected stream", lambda: self._edit_stream(new=False)),
            B(tb, "delete_icon", "Remove the selected stream", self._remove_stream),
            B(tb, "import_icon", "Import an IPTV playlist (.m3u / .m3u8)", self._import_playlist)]

        tb = self.ribbon_toolbar("Capture")
        B(tb, "screenshot_icon", "Snapshot  (F9)", self.snapshot)
        self._rec_btn = B(tb, "record_icon", "Record / stop  (F10)", self._toggle_recording,
                          checkable=True)
        self._audio_btn = B(tb, "sound_icon", "Toggle audio monitoring / recording",
                            self._toggle_audio, checkable=True)
        self._audio_btn.setChecked(bool(self.WS.get("audio_enabled", True)))
        tb.addSeparator()
        B(tb, "delay_timer_icon", "Delay before capture (seconds)", lambda: self._delay_spin.setFocus())
        self._delay_spin = QSpinBox()
        self._delay_spin.setRange(0, 30)
        self._delay_spin.setSuffix(" s")
        self._delay_spin.setValue(int(self.WS.get("delay", 0)))
        self._delay_spin.valueChanged.connect(lambda v: self.WS.set("delay", v))
        tb.addWidget(self._delay_spin)
        self._hide_chk = QCheckBox("Hide")
        self._hide_chk.setToolTip("Hide this window while capturing the screen")
        self._hide_chk.setChecked(bool(self.WS.get("hide_on_capture", True)))
        self._hide_chk.toggled.connect(lambda on: self.WS.set("hide_on_capture", on))
        tb.addWidget(self._hide_chk)

        tb = self.ribbon_toolbar("Output")
        B(tb, "folder_icon", "Choose capture folder", self._choose_folder)
        self._img_fmt = QComboBox()
        self._img_fmt.addItems(["PNG", "JPG"])
        self._img_fmt.setCurrentText(self.WS.get("image_format", "PNG"))
        self._img_fmt.setToolTip("Still image format")
        self._img_fmt.currentTextChanged.connect(lambda t: self.WS.set("image_format", t))
        tb.addWidget(self._img_fmt)
        self._vid_fmt = QComboBox()
        self._vid_fmt.addItems(["MP4", "WebM"] if is_linux() else ["MP4"])
        self._vid_fmt.setCurrentText(self.WS.get("video_format", "MP4"))
        self._vid_fmt.setToolTip("Video format (WebM: Linux screen recording only)")
        self._vid_fmt.currentTextChanged.connect(lambda t: self.WS.set("video_format", t))
        tb.addWidget(self._vid_fmt)
        self._folder_lbl = self.ribbon_label(tb, "")

        tb = self.ribbon_toolbar("Bugs")
        B(tb, "bug_report_icon", "New bug (with the selected capture)", self._new_bug)
        B(tb, "get_checkmark_icon", "Mark the selected bug fixed / reopen", self._set_status_fixed)
        B(tb, "folder_icon", "Choose bugs folder (TODO_bugs)", self._choose_bugs_folder)

        QShortcut(Qt.Key.Key_F9, self, self.snapshot)
        QShortcut(Qt.Key.Key_F10, self, lambda: self._rec_btn.click())

    def _build_menus_into_qmenu(self, pm): #vers 2
        fm = pm.addMenu("File")
        fm.addAction("Open Capture Folder", lambda: QDesktopServices.openUrl(
            QUrl.fromLocalFile(str(self._output_dir()))))
        fm.addAction("Choose Capture Folder...", self._choose_folder)
        cm = pm.addMenu("Capture")
        cm.addAction("Snapshot  F9", self.snapshot)
        cm.addAction("Record / Stop  F10", lambda: self._rec_btn.click())
        cm.addSeparator()
        for label, mode in (("HDMI / USB Device", 'device'), ("Whole Screen", 'screen'),
                            ("Chosen Window", 'window'), ("Network Stream", 'stream')):
            act = cm.addAction(label, lambda m=mode: self._set_mode(m))
            act.setCheckable(True)
            act.setChecked(self._mode == mode)
        vm = pm.addMenu("View")
        vm.addAction("Rescan Sources", self._refresh_sources)
        vm.addAction(f"About {App_name}", self._show_about)

    def _create_left_panel(self): #vers 2
        """Captures | Bugs tabs."""
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        ll = QVBoxLayout(panel)
        ll.setContentsMargins(*self.get_panel_margins())
        self._left_tabs = QTabWidget()
        self._left_tabs.setDocumentMode(True)
        ll.addWidget(self._left_tabs)
        self._list = QListWidget()
        self._list.setIconSize(QSize(96, 54))
        self._list.setAlternatingRowColors(True)
        self._list.currentItemChanged.connect(
            lambda it, _p: it and it.data(Qt.ItemDataRole.UserRole).lower().endswith(IMAGE_EXT)
            and self._show_image(QImage(it.data(Qt.ItemDataRole.UserRole))))
        self._list.itemDoubleClicked.connect(
            lambda it: QDesktopServices.openUrl(QUrl.fromLocalFile(it.data(Qt.ItemDataRole.UserRole))))
        self._list.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self._list.customContextMenuRequested.connect(self._list_menu)
        self._left_tabs.addTab(self._list, "Captures")
        self._left_tabs.addTab(self._create_bugs_tab(), "Bugs")
        self._left_tabs.currentChanged.connect(
            lambda i: self._centre_stack.setCurrentIndex(1 if i == 1 and self._bugs_list.currentItem() else 0))
        return panel

    def _create_centre_panel(self): #vers 2
        """Stack: capture preview / bug view."""
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        cl = QVBoxLayout(panel)
        cl.setContentsMargins(0, 0, 0, 0)
        self._preview = QLabel("No source")
        self._preview.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._preview.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Ignored)   # no size creep
        self._preview.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self._preview.customContextMenuRequested.connect(self._capture_menu)
        self._centre_stack = QStackedWidget()
        self._centre_stack.addWidget(self._preview)
        self._centre_stack.addWidget(self._create_bug_view())
        cl.addWidget(self._centre_stack)
        return panel

    # Sources

    def _set_mode(self, mode): #vers 2
        """Switch source type; device / stream feed runs only in its mode."""
        if self._recorder or self._portal_cast or self._stream_rec:
            self._set_status("Stop recording before changing source")
            return
        self._mode = mode
        self.WS.set("source_mode", mode)
        for m, b in self._mode_btns.items():
            b.setChecked(m == mode)
        self._timer.stop()
        self._device.close()
        self._stream.close()
        for b in self._stream_btns:
            b.setEnabled(mode == 'stream')
        self._fill_source_combo()

    def _refresh_sources(self): #vers 2
        self._fill_audio_combo()
        self._fill_source_combo()
        self._set_status(f"{self._source_combo.count()} source(s) found")

    def _fill_source_combo(self): #vers 2
        """Devices, screens or windows for the current mode."""
        c = self._source_combo
        c.blockSignals(True)
        c.clear()
        if self._mode == 'device':
            QApplication.setOverrideCursor(Qt.CursorShape.WaitCursor)
            for idx, label in list_devices():
                c.addItem(label, idx)
            QApplication.restoreOverrideCursor()
        elif self._mode == 'stream':
            for i, e in enumerate(self.WS.get("streams", [])):
                c.addItem(e['name'], i)
            c.setCurrentIndex(min(int(self.WS.get("stream_index", 0)), c.count() - 1))
        elif is_linux():
            c.addItem("Chosen when capturing (system picker)", None)
        elif self._mode == 'screen':
            for i, s in enumerate(QGuiApplication.screens()):
                g = s.geometry()
                c.addItem(f"Screen {i + 1}: {s.name()}  {g.width()}x{g.height()}", i)
        else:
            for hwnd, title in list_windows():
                c.addItem(title, hwnd)
        c.blockSignals(False)
        self._source_changed(c.currentIndex())

    def _source_changed(self, _idx): #vers 3
        """Start the device feed, or a live screen preview on Windows."""
        self._timer.stop()
        self._device.close()
        self._stream.close()
        data = self._source_combo.currentData()
        if self._mode == 'stream':
            if data is None:
                self._preview.setText("Add a stream: RTSP camera URL or IPTV playlist (+ / import)")
                return
            self.WS.set("stream_index", data)
            e = self._stream_entry()
            self._stream.open(stream_url(e), e.get('tcp', True))
            self._preview.setText(f"Connecting to {mask_url(e['url'])}...")
            self._set_status(f"Stream: {e['name']}")
            self._timer.start(40)
            return
        if self._mode == 'device':
            if data is None:
                self._preview.setText("No HDMI / USB capture device found")
                return
            if not self._device.open(data):
                self._preview.setText(f"Could not open device {data}")
                return
            self._open_audio()
            #self._timer.start(30)
            w, h = self._device.size()
            self._set_status(f"Device {data}: {w}x{h}")
            self._timer.start(30)
        elif data is not None:
            self._timer.start(100)          # Windows live preview
        else:
            self._preview.setText("Snapshot or Record opens the system picker\n"
                                  + ("(choose a window)" if self._mode == 'window' else "(choose a screen)"))

    def _tick(self): #vers 3
        """Next device / stream frame or screen grab: preview and recording."""
        if self._mode == 'stream':
            _frame, img = self._stream.read()
            if img is None:
                if self._stream.error:
                    self._preview.setText(self._stream.error)
                return
            if self._stream_rec and not self._stream_rec.is_open():
                self._stop_recording()
                self._set_status("Stream recording stopped: ffmpeg ended (stream lost?)")
        elif self._mode == 'device':
            frame, img = self._device.read()
            if img is None:
                return
            if self._recorder:
                self._recorder.add(frame=frame)
        else:
            data = self._source_combo.currentData()
            img = grab_screen(data) if self._mode == 'screen' else grab_window_area(data)
            if self._recorder:
                self._recorder.add(image=img)
        self._last_image = img
        self._show_image(img)
        if self._rec_start:
            secs = int((datetime.now() - self._rec_start).total_seconds())
            self._set_status(f"Recording  {secs // 60:02d}:{secs % 60:02d}")


    def _show_image(self, img): #vers 1
        if img is None or img.isNull():
            return
        self._preview.setPixmap(QPixmap.fromImage(img).scaled(
            self._preview.size(), Qt.AspectRatioMode.KeepAspectRatio,
            Qt.TransformationMode.SmoothTransformation))

    # Output

    def _output_dir(self): #vers 1
        d = Path(self.WS.get("output_folder", "") or (get_user_config_dir() / 'captures'))
        d.mkdir(parents=True, exist_ok=True)
        self._folder_lbl.setText(str(d))
        return d

    def _choose_folder(self): #vers 1
        d = QFileDialog.getExistingDirectory(self, "Capture folder", str(self._output_dir()))
        if d:
            self.WS.set("output_folder", d)
            self.WS.save()
            self._load_captures()

    def _capture_name(self, kind, ext): #vers 1
        """Unique file path in the capture folder: <kind>_<date_time>.<ext>"""
        return str(self._output_dir() / f"{kind}_{datetime.now().strftime('%Y%m%d_%H%M%S')}.{ext}")

    def _save_image(self, img, kind): #vers 1
        fmt = self._img_fmt.currentText().lower()
        path = self._capture_name(kind, fmt)
        if not img.save(path, fmt.upper(), 92 if fmt == 'jpg' else -1):
            self._set_status(f"Could not save {path}")
            return
        self._last_image = img
        self._show_image(img)
        self._load_captures(select=path)
        self._set_status(f"Saved {Path(path).name}")

    def _load_captures(self, select=None): #vers 1
        """Fill the captures list (newest first) with thumbnails."""
        import cv2
        self._list.clear()
        files = sorted((p for p in self._output_dir().iterdir()
                        if p.suffix.lower() in IMAGE_EXT + VIDEO_EXT),
                       key=lambda p: p.stat().st_mtime, reverse=True)
        for p in files:
            item = QListWidgetItem(p.name)
            item.setData(Qt.ItemDataRole.UserRole, str(p))
            if p.suffix.lower() in IMAGE_EXT:
                item.setIcon(QIcon(QPixmap(str(p)).scaled(96, 54, Qt.AspectRatioMode.KeepAspectRatio)))
            else:
                cap = cv2.VideoCapture(str(p))
                ok, frame = cap.read()
                cap.release()
                if ok:
                    rgb = cv2.cvtColor(frame, cv2.COLOR_BGR2RGB)
                    h, w, _c = rgb.shape
                    img = QImage(rgb.data, w, h, 3 * w, QImage.Format.Format_RGB888)
                    item.setIcon(QIcon(QPixmap.fromImage(img).scaled(96, 54, Qt.AspectRatioMode.KeepAspectRatio)))
            self._list.addItem(item)
            if select and str(p) == select:
                self._list.setCurrentItem(item)

    def _list_menu(self, pos): #vers 1
        """Right-click on a capture: open, folder, DP5, copy, rename, delete."""
        item = self._list.itemAt(pos)
        menu = QMenu(self)
        if item:
            path = item.data(Qt.ItemDataRole.UserRole)
            menu.addAction("Open", lambda: QDesktopServices.openUrl(QUrl.fromLocalFile(path)))
            if path.lower().endswith(IMAGE_EXT):
                menu.addAction("Open in DP5 Workshop", lambda: self._open_in_dp5(path))
                menu.addAction("Copy Image", lambda: QGuiApplication.clipboard().setImage(QImage(path)))
            menu.addAction("Rename...", lambda: self._rename(item))
            menu.addAction("Move to Trash", lambda: self._trash(item))
            menu.addSeparator()
        menu.addAction("Open Capture Folder", lambda: QDesktopServices.openUrl(
            QUrl.fromLocalFile(str(self._output_dir()))))
        menu.addAction("Refresh", self._load_captures)
        menu.exec(self._list.mapToGlobal(pos))

    def _rename(self, item): #vers 1
        path = Path(item.data(Qt.ItemDataRole.UserRole))
        name, ok = QInputDialog.getText(self, "Rename", "New name:", text=path.stem)
        if ok and name.strip():
            new = path.with_name(name.strip() + path.suffix)
            if new.exists():
                QMessageBox.warning(self, "Rename", f"{new.name} already exists")
                return
            path.rename(new)
            self._load_captures(select=str(new))

    def _trash(self, item): #vers 1
        path = item.data(Qt.ItemDataRole.UserRole)
        if QFile.moveToTrash(path)[0]:
            self._load_captures()
            self._set_status(f"Moved {Path(path).name} to trash")

    def _open_in_dp5(self, path): #vers 1
        mw = self.main_window
        if mw is not None and hasattr(mw, 'open_dp5_workshop_docked'):
            mw.open_dp5_workshop_docked(path)
            return
        from apps.components.DP5_Workshop.dp5_workshop import open_dp5_workshop
        self._dp5 = open_dp5_workshop()
        self._dp5._import_bitmap_path(path)

    def _capture_menu(self, pos): #vers 1
        """Right-click on the preview: snapshot / record / save frame."""
        menu = QMenu(self)
        menu.addAction("Snapshot", self.snapshot)
        menu.addAction("Stop Recording" if self._rec_btn.isChecked() else "Record",
                       lambda: self._rec_btn.click())
        if self._last_image is not None:
            menu.addAction("Copy Image", lambda: QGuiApplication.clipboard().setImage(self._last_image))
        menu.exec(self._preview.mapToGlobal(pos))

    # Network streams (RTSP / IPTV) - saved in the user's settings, not the repo
    def _frame_source(self): #vers 1
        return self._stream if self._mode == 'stream' else self._device

    def _stream_entry(self): #vers 1
        """Saved stream for the selected source, or None."""
        i = self._source_combo.currentData()
        streams = self.WS.get("streams", [])
        return streams[i] if i is not None and i < len(streams) else None

    def _edit_stream(self, new=True): #vers 1
        """Add / edit a stream (name, URL, login, RTSP over TCP)."""
        streams = list(self.WS.get("streams", []))
        cur = None if new else self._stream_entry()
        if not new and cur is None:
            return
        dlg = StreamDialog(self, cur)
        if dlg.exec() != StreamDialog.DialogCode.Accepted or not dlg.entry()['url']:
            return
        if new:
            streams.append(dlg.entry())
            index = len(streams) - 1
        else:
            index = self._source_combo.currentData()
            streams[index] = dlg.entry()
        self.WS.set("streams", streams)
        self.WS.set("stream_index", index)
        self.WS.save()
        self._fill_source_combo()

    def _remove_stream(self): #vers 1
        i = self._source_combo.currentData()
        streams = list(self.WS.get("streams", []))
        if i is None or i >= len(streams):
            return
        if QMessageBox.question(self, "Remove stream", f"Remove {streams[i]['name']}?") \
                != QMessageBox.StandardButton.Yes:
            return
        del streams[i]
        self.WS.set("streams", streams)
        self.WS.set("stream_index", 0)
        self.WS.save()
        self._fill_source_combo()

    def _import_playlist(self): #vers 1
        """Add every channel of an IPTV .m3u / .m3u8 playlist (file or URL)."""
        path, _ = QFileDialog.getOpenFileName(self, "IPTV playlist", "", "Playlists (*.m3u *.m3u8);;All files (*)")
        if not path:
            return
        with open(path, encoding='utf-8', errors='replace') as f:
            chans = parse_m3u(f.read())
        streams = list(self.WS.get("streams", []))
        known = {e['url'] for e in streams}
        add = [{'name': n, 'url': u, 'user': '', 'password': '', 'tcp': True} for n, u in chans if u not in known]
        self.WS.set("streams", streams + add)
        self.WS.save()
        self._fill_source_combo()
        self._set_status(f"Playlist: {len(add)} channel(s) added")

    # Audio
    def _fill_audio_combo(self): #vers 1
        """Audio inputs; saved choice first, else the first capture / HDMI / USB input."""
        import re
        c = self._audio_combo
        c.blockSignals(True)
        c.clear()
        inputs = list_audio_inputs()
        for idx, name in inputs:
            c.addItem(name, idx)
        if not inputs:
            c.addItem("No audio input (install pyaudio)" if not HAS_PYAUDIO else "No audio input found", None)
        saved = self.WS.get("audio_device", "")
        pick = c.findText(saved) if saved else -1
        if pick < 0:
            pick = next((i for i, (_x, n) in enumerate(inputs)
                         if re.search(r'hdmi|capture|usb|video|cam link|elgato', n, re.I)), 0)
        c.setCurrentIndex(pick)
        c.blockSignals(False)

    def _audio_changed(self, _idx): #vers 1
        """Remember the input by name (indices change) and reopen it."""
        if self._audio_combo.currentData() is not None:
            self.WS.set("audio_device", self._audio_combo.currentText())
        if self._mode == 'device':
            self._open_audio()

    def _open_audio(self): #vers 1
        """Open the chosen input when audio is on; status says if it failed."""
        self._audio_source.close()
        if not self._audio_btn.isChecked():
            return
        dev = self._audio_combo.currentData()
        if dev is None:
            return
        if self._audio_source.open(dev):
            self._set_status(f"Audio: {self._audio_combo.currentText()} "
                             f"({self._audio_source.rate} Hz, {self._audio_source.channels} ch)")
        else:
            self._set_status(f"Audio: can't open {self._audio_combo.currentText()} - pick another input")

    def _toggle_audio(self): #vers 2
        enabled = self._audio_btn.isChecked()
        self.WS.set("audio_enabled", enabled)
        if self._mode == 'device':
            self._open_audio()
        if not enabled:
            self._set_status("Audio muted")

    # Capture

    def _delayed(self, fn, screen=True): #vers 2
        """Run fn after the delay; hide this window first for screen captures."""
        hide = screen and self._mode in ('screen', 'window') and self._hide_chk.isChecked()
        if hide:
            self.window().hide()
        ms = self._delay_spin.value() * 1000 + (400 if hide else 0)
        if self._delay_spin.value():
            self._set_status(f"Capturing in {self._delay_spin.value()} s...")
        QTimer.singleShot(ms, fn)

    def _after_capture(self): #vers 1
        """Bring the window back after a hidden screen capture."""
        if not self.window().isVisible():
            self.window().show()
            self.window().raise_()

    def snapshot(self): #vers 2
        """Still image from the current source."""
        if self._mode in ('device', 'stream'):
            self._delayed(self._device_snapshot, screen=False)
        else:
            self._delayed(self._screen_snapshot)

    def _device_snapshot(self): #vers 2
        _frame, img = self._frame_source().read()
        if img is None:
            self._set_status("No frame from the capture device / stream")
            return
        self._save_image(img, 'snapshot')

    def _screen_snapshot(self): #vers 1
        """Windows: grab directly. Linux: screenshot portal (window mode lets the user pick)."""
        if not is_linux():
            data = self._source_combo.currentData()
            img = grab_screen(data) if self._mode == 'screen' else grab_window_area(data)
            self._after_capture()
            self._save_image(img, self._mode)
            return
        from apps.components.Capture_Workshop.depends.capture_portal_func import PortalScreenshot
        self._portal_shot = PortalScreenshot(self)
        self._portal_shot.finished.connect(self._portal_shot_done)
        self._portal_shot.take(interactive=self._mode == 'window')

    def _portal_shot_done(self, path, error): #vers 1
        """Portal saved its own file: move it into the capture folder."""
        self._after_capture()
        if error:
            self._set_status(f"Screenshot: {error}")
            return
        img = QImage(path)
        if self._img_fmt.currentText() == 'PNG' and path.lower().endswith('.png'):
            dest = self._capture_name(self._mode, 'png')
            shutil.move(path, dest)
            self._last_image = img
            self._show_image(img)
            self._load_captures(select=dest)
            self._set_status(f"Saved {Path(dest).name}")
        else:
            self._save_image(img, self._mode)
            os.remove(path)

    def _toggle_recording(self): #vers 2
        if self._rec_btn.isChecked():
            self._rec_btn.setChecked(False)          # set again once really recording
            self._delayed(self._start_recording, screen=self._mode in ('screen', 'window'))
        else:
            self._stop_recording()

    def _start_recording(self): #vers 4
        """Device / Windows screen: frame recorder. Linux screen: portal ScreenCast."""
        fmt = self._vid_fmt.currentText().lower()
        if self._mode == 'stream':                              # ffmpeg copies the stream (with sound)
            e = self._stream_entry()
            if e is None:
                return
            path = self._capture_name("stream_rec", 'mp4')
            rec = StreamRecorder(stream_url(e), path, e.get('tcp', True))
            if not rec.is_open():
                QMessageBox.warning(self, "Record", "Stream recording needs ffmpeg (not found).")
                return
            self._stream_rec, self._rec_path = rec, path
            self._record_started()
            return
        if self._mode != 'device' and is_linux():
            from apps.components.Capture_Workshop.depends.capture_portal_func import PortalScreenCast
            self._portal_cast = PortalScreenCast(self)
            self._portal_cast.started.connect(self._portal_cast_started)
            self._portal_cast.failed.connect(self._portal_cast_failed)
            self._portal_cast.stopped.connect(self._portal_cast_stopped)
            path = self._capture_name(f"{self._mode}_rec", fmt)
            self._portal_cast.start(self._mode == 'window', path, fmt)
            return
        self._after_capture()
        if self._mode == 'device':
            frame, img = self._device.read()
            if img is None:
                self._set_status("No frame from the capture device")
                return
            w, h = img.width(), img.height()
        else:
            data = self._source_combo.currentData()
            img = grab_screen(data) if self._mode == 'screen' else grab_window_area(data)
            w, h = img.width() // 2 * 2, img.height() // 2 * 2
        path = self._capture_name(f"{self._mode}_rec", 'mp4')
        self._recorder = FrameRecorder(path, w, h, 30.0 if self._mode == 'device' else 10.0)
        if not self._recorder.is_open():
            self._recorder = None
            QMessageBox.warning(self, "Record", "Could not start the video writer (mp4v codec).")
            return

        self._rec_path = path
        if self._audio_source.is_active:                        # sound from the chosen input
            self._rec_audio_path = os.path.splitext(path)[0] + '_audio.wav'
            self._audio_recorder = AudioRecorder(self._rec_audio_path, self._audio_source.rate,
                                                 self._audio_source.channels)
            self._audio_source.recorder = self._audio_recorder
        self._record_started()

    def _record_started(self): #vers 1
        self._rec_start = datetime.now()
        self._rec_btn.setChecked(True)
        self._rec_btn.setIcon(SVGIconFactory.stop_icon(self._RIBBON_ICON, self._get_icon_color()))

    def _stop_recording(self): #vers 3
        """Finish the video, mux the audio in with ffmpeg, list it."""
        if self._portal_cast:
            self._portal_cast.stop()
            return
        if self._stream_rec:
            self._stream_rec.close()
            self._stream_rec = None
            self._load_captures(select=self._rec_path)
            self._set_status(f"Saved {Path(self._rec_path).name}")
        if self._recorder:
            self._recorder.close()
            self._recorder = None
            note = ""
            if self._audio_recorder:
                self._audio_source.recorder = None
                self._audio_recorder.close()
                self._audio_recorder = None
                if shutil.which('ffmpeg'):
                    muxed = self._rec_path.replace('.mp4', '_muxed.mp4')
                    subprocess.run(['ffmpeg', '-y', '-i', self._rec_path, '-i', self._rec_audio_path,
                                    '-c:v', 'copy', '-c:a', 'aac', '-shortest', muxed],
                                   stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
                    if Path(muxed).exists():
                        shutil.move(muxed, self._rec_path)
                        os.remove(self._rec_audio_path)
                        note = " with audio"
                else:
                    note = f" (audio: {Path(self._rec_audio_path).name}, no ffmpeg to join)"
            self._load_captures(select=self._rec_path)
            self._set_status(f"Saved {Path(self._rec_path).name}{note}")

        self._rec_start = None
        self._rec_btn.setChecked(False)
        self._rec_btn.setIcon(SVGIconFactory.record_icon(self._RIBBON_ICON))

    def _portal_cast_started(self, path): #vers 1
        self._after_capture()
        self._rec_path = path
        self._record_started()
        self._preview.setText(f"Recording to\n{Path(path).name}\n\nPress Record again to stop")
        self._status_timer = QTimer(self)
        self._status_timer.timeout.connect(self._tick_cast)
        self._status_timer.start(1000)

    def _tick_cast(self): #vers 1
        secs = int((datetime.now() - self._rec_start).total_seconds())
        self._set_status(f"Recording  {secs // 60:02d}:{secs % 60:02d}")

    def _portal_cast_failed(self, error): #vers 1
        self._after_capture()
        self._portal_cast = None
        self._stop_recording()
        QMessageBox.warning(self, "Screen Recording", error)

    def _portal_cast_stopped(self, path): #vers 1
        self._status_timer.stop()
        self._portal_cast = None
        self._stop_recording()
        self._load_captures(select=path)
        self._set_status(f"Saved {Path(path).name}")

    def closeEvent(self, ev): #vers 3
        """Finish any recording and release the device."""
        if self._rec_btn.isChecked():
            self._stop_recording()
        self._timer.stop()
        self._device.close()
        self._stream.close()
        self._audio_source.close()
        super().closeEvent(ev)


def open_capture_workshop(main_window=None): #vers 1
    """Open Capture Workshop as its own window."""
    w = CaptureWorkshop(None, main_window)
    w.resize(1300, 800)
    w.show()
    return w


if __name__ == "__main__": #vers 1
    app = QApplication(sys.argv)
    w = CaptureWorkshop()
    w.setWindowTitle(f"{App_name} - Standalone")
    w.show()
    sys.exit(app.exec())
