#!/usr/bin/env python3
#this belongs in apps/methods/mini_audio_player.py - Version: 1
# X-Seti - Aug 2026 - IMG Factory 1.6 - Small, shared audio player widget (name, progress bar, stop/start)

"""
Small, shared "mini player" widget (Aug 20 2026, per Keith: "maybe a
tooltip player, showing just the name, and a progress bar, stop,
start") - one real widget used by both Dir Tree's own right-click
Play/Extract & Play actions and Map Workshop's own Auzo list
playback, rather than each firing a silent, fire-and-forget
QSoundEffect.play() with no visible feedback at all.

Uses QMediaPlayer (not QSoundEffect) as its own real playback engine
- QSoundEffect is built for short, low-latency, uncompressed-or-Ogg
sound effects and does not decode MP3 at all (this is the real, direct
cause of Keith's own real bug report: "wav plays. mp3 doesn't seen to
work."). QMediaPlayer is Qt's own real, full media pipeline backed by
the OS's own real codecs, and it also gives real position/duration
signals for free, which the progress bar needs anyway.

For formats neither QMediaPlayer nor its own OS backend can decode at
all (SA's own real "audio stream" format - handled by its own,
separate sa_audio_stream.py, not this module; ATRAC3+/.at3; raw
PS-ADPCM/.vb) a caller transcodes to a temp real WAV via a real,
external ffmpeg process first (transcode_to_wav below), then hands
that real WAV's own path to this same player - same real widget,
same real controls, regardless of the original format.
"""

import os
import subprocess
import tempfile

from PyQt6.QtWidgets import QWidget, QHBoxLayout, QVBoxLayout, QLabel, QPushButton, QSlider
from PyQt6.QtCore import Qt, QUrl

##Methods list -
# transcode_to_wav
# MiniAudioPlayer (class)


def transcode_to_wav(src_path: str, ffmpeg_args: list = None) -> str: #vers 1
    """Transcode any file ffmpeg can read into a real, temporary,
    standard WAV file, returning its own real path - or raises
    RuntimeError with ffmpeg's own real stderr tail if it fails or
    ffmpeg itself isn't available (Aug 20 2026). ffmpeg_args is an
    optional list of extra real input-side arguments (e.g. ['-f',
    'data'] for raw, headerless input with no real container at all)
    inserted before -i; omit for anything ffmpeg can already
    recognise on its own (like .at3's own real RIFF/WAVE-ATRAC3+
    container)."""
    out_path = os.path.join(
        tempfile.gettempdir(),
        f"_imgfactory_audio_preview_{abs(hash(src_path))}.wav")
    if os.path.isfile(out_path):
        return out_path
    cmd = ['ffmpeg', '-y']
    if ffmpeg_args:
        cmd += ffmpeg_args
    cmd += ['-i', src_path, out_path]
    try:
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=60)
    except FileNotFoundError:
        raise RuntimeError("ffmpeg isn't installed/available on this system.")
    except subprocess.TimeoutExpired:
        raise RuntimeError("ffmpeg took too long transcoding this file.")
    if result.returncode != 0 or not os.path.isfile(out_path):
        tail = '\n'.join(result.stderr.strip().splitlines()[-5:])
        raise RuntimeError(f"ffmpeg failed to transcode this file:\n{tail}")
    return out_path


class MiniAudioPlayer(QWidget): #vers 1
    """Small player bar (Aug 20 2026, per Keith's own real request) -
    shows the currently-playing file's own real name, a real,
    seekable progress slider, and Play/Pause + Stop. A single real
    instance is meant to be created once per parent window and reused
    for every file played from it, rather than one new instance per
    file - call load_and_play() again to switch tracks."""

    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        from PyQt6.QtMultimedia import QMediaPlayer, QAudioOutput
        self._player = QMediaPlayer(self)
        self._audio_output = QAudioOutput(self)
        self._player.setAudioOutput(self._audio_output)
        self._audio_output.setVolume(0.7)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(6, 4, 6, 4)
        self._name_label = QLabel("No track loaded")
        self._name_label.setStyleSheet("font-weight: bold;")
        layout.addWidget(self._name_label)

        controls = QHBoxLayout()
        self._play_btn = QPushButton("\u25B6")   # play triangle
        self._play_btn.setFixedWidth(32)
        self._play_btn.clicked.connect(self._toggle_play_pause)
        controls.addWidget(self._play_btn)

        self._stop_btn = QPushButton("\u25A0")   # stop square
        self._stop_btn.setFixedWidth(32)
        self._stop_btn.clicked.connect(self.stop)
        controls.addWidget(self._stop_btn)

        self._progress = QSlider(Qt.Orientation.Horizontal)
        self._progress.setRange(0, 0)
        self._progress.sliderMoved.connect(self._on_seek)
        controls.addWidget(self._progress)

        self._time_label = QLabel("0:00 / 0:00")
        controls.addWidget(self._time_label)
        layout.addLayout(controls)

        self._player.positionChanged.connect(self._on_position_changed)
        self._player.durationChanged.connect(self._on_duration_changed)
        self._player.playbackStateChanged.connect(self._on_state_changed)

    def load_and_play(self, path: str, display_name: str = None): #vers 1
        """Load a real, standard-format audio file (already
        transcoded via transcode_to_wav if needed) and start playing
        it immediately (Aug 20 2026)."""
        self._name_label.setText(display_name or os.path.basename(path))
        self._player.setSource(QUrl.fromLocalFile(path))
        self._player.play()

    def stop(self): #vers 1
        """Stop playback and reset position to the start (Aug 20
        2026, per Keith's own real request for a real Stop
        control)."""
        self._player.stop()

    def _toggle_play_pause(self): #vers 1
        from PyQt6.QtMultimedia import QMediaPlayer
        if self._player.playbackState() == QMediaPlayer.PlaybackState.PlayingState:
            self._player.pause()
        else:
            self._player.play()

    def _on_seek(self, value): #vers 1
        self._player.setPosition(value)

    def _on_position_changed(self, position_ms): #vers 1
        self._progress.blockSignals(True)
        self._progress.setValue(position_ms)
        self._progress.blockSignals(False)
        self._update_time_label(position_ms, self._player.duration())

    def _on_duration_changed(self, duration_ms): #vers 1
        self._progress.setRange(0, duration_ms)
        self._update_time_label(self._player.position(), duration_ms)

    def _on_state_changed(self, state): #vers 1
        from PyQt6.QtMultimedia import QMediaPlayer
        self._play_btn.setText(
            "\u23F8" if state == QMediaPlayer.PlaybackState.PlayingState else "\u25B6")

    def _update_time_label(self, position_ms, duration_ms): #vers 1
        def fmt(ms):
            s = max(0, ms) // 1000
            return f"{s // 60}:{s % 60:02d}"
        self._time_label.setText(f"{fmt(position_ms)} / {fmt(duration_ms)}")
