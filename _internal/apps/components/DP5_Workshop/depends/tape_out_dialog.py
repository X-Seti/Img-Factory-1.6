#this belongs in apps/components/DP5_Workshop/depends/tape_out_dialog.py - Version: 1
# X-Seti - October 27 2026 - IMG Factory 1.6 - DP5 Tape Out dialog.

"""
Tape Out: the canvas as a Spectrum / C64 / Amstrad loading screen played
through an audio output (record it on a tape recorder, or feed the real
machine's EAR / tape input), or saved as WAV / tape file.
"""

##Methods list -
#
##class TapeOutDialog: -
# __init__
# _build
# _device
# _play
# _save_tape
# _save_wav
# _stop
# _tape
# _tick
# closeEvent

import io
import wave

from PyQt6.QtCore import QBuffer, QByteArray, QIODevice, Qt, QTimer
from PyQt6.QtMultimedia import QAudio, QAudioFormat, QAudioSink, QMediaDevices
from PyQt6.QtWidgets import (QCheckBox, QComboBox, QDialog, QFileDialog, QFormLayout, QHBoxLayout, QLabel,
                             QMessageBox, QProgressBar, QPushButton, QSlider, QVBoxLayout)

from apps.components.DP5_Workshop.depends import dp5_tape as T

LEAD_IN = 1.0                                            # s of silence before the tape starts


class TapeOutDialog(QDialog):
    """System picker, audio device, Play / Stop, volume, polarity, Save WAV / tape."""

    def __init__(self, ws, rgba: bytes, w: int, h: int, name: str): #vers 1
        super().__init__(ws)
        self.setWindowTitle("Tape Out")
        self.setMinimumWidth(460)
        self._ws, self._rgba, self._w, self._h, self._name = ws, rgba, w, h, name
        self._cache = {}                                 # system -> (tape, wav)
        self._sink = self._buf = None
        self._timer = QTimer(self)
        self._timer.timeout.connect(self._tick)
        self._build()

    def _build(self): #vers 1
        lay = QVBoxLayout(self)
        form = QFormLayout()
        self.system = QComboBox()
        for key, (label, _ext, _how) in T.SYSTEMS.items():
            self.system.addItem(label, key)
        self.system.currentIndexChanged.connect(self._stop)
        form.addRow("Machine:", self.system)
        self.device = QComboBox()
        for i, dev in enumerate(QMediaDevices.audioOutputs()):
            self.device.addItem(dev.description(), dev)
            if dev.isDefault():
                self.device.setCurrentIndex(i)
        form.addRow("Audio out:", self.device)
        vol = QHBoxLayout()
        self.volume = QSlider(Qt.Orientation.Horizontal)
        self.volume.setRange(10, 100)
        self.volume.setValue(100)
        self.volume.valueChanged.connect(lambda v: self._sink and self._sink.setVolume(v / 100))
        vol.addWidget(self.volume)
        self.invert = QCheckBox("Invert polarity")
        self.invert.setToolTip("Try this if the machine won't load from the recording")
        vol.addWidget(self.invert)
        form.addRow("Volume:", vol)
        lay.addLayout(form)
        self.how = QLabel()
        self.how.setWordWrap(True)
        lay.addWidget(self.how)
        self.progress = QProgressBar()
        self.progress.setFormat("%v / %m s")
        lay.addWidget(self.progress)
        row = QHBoxLayout()
        self.play_btn = QPushButton("Play")
        self.play_btn.clicked.connect(self._play)
        stop = QPushButton("Stop")
        stop.clicked.connect(self._stop)
        wav = QPushButton("Save WAV…")
        wav.clicked.connect(self._save_wav)
        tape = QPushButton("Save tape file…")
        tape.clicked.connect(self._save_tape)
        close = QPushButton("Close")
        close.clicked.connect(self.close)
        for b in (self.play_btn, stop, wav, tape):
            row.addWidget(b)
        row.addStretch()
        row.addWidget(close)
        lay.addLayout(row)
        self.system.currentIndexChanged.connect(lambda _: self.how.setText(
            "On the machine: " + T.SYSTEMS[self.system.currentData()][2] +
            ". Recording: start the recorder, then Play."))
        self.system.currentIndexChanged.emit(0)

    def _tape(self): #vers 1
        """(tape bytes, wav bytes) for the chosen machine, built once."""
        key = self.system.currentData()
        if key not in self._cache:
            self.setCursor(Qt.CursorShape.WaitCursor)
            try:
                tape = T.build_tape(key, self._rgba, self._w, self._h, self._name)
                self._cache[key] = (tape, T.tape_wav(key, tape))
            finally:
                self.unsetCursor()
        return self._cache[key]

    def _device(self): #vers 1
        return self.device.currentData() or QMediaDevices.defaultAudioOutput()

    def _play(self): #vers 1
        """Stream the 8-bit PCM (plus lead-in silence) to the audio device."""
        self._stop()
        try:
            wav = self._tape()[1]
        except Exception as e:
            QMessageBox.warning(self, "Tape Out", str(e))
            return
        with wave.open(io.BytesIO(wav)) as wv:
            rate = wv.getframerate()
            pcm = b'\x80' * int(LEAD_IN * rate) + wv.readframes(wv.getnframes())
        if self.invert.isChecked():
            pcm = pcm.translate(bytes(255 - i for i in range(256)))
        fmt = QAudioFormat()
        fmt.setSampleRate(rate)
        fmt.setChannelCount(1)
        fmt.setSampleFormat(QAudioFormat.SampleFormat.UInt8)
        dev = self._device()
        if dev.isNull():
            QMessageBox.warning(self, "Tape Out", "No audio output found - use Save WAV")
            return
        if not dev.isFormatSupported(fmt):
            QMessageBox.warning(self, "Tape Out", f"{dev.description()} can't play 8-bit {rate} Hz mono")
            return
        self._buf = QBuffer(self)
        self._buf.setData(QByteArray(pcm))
        self._buf.open(QIODevice.OpenModeFlag.ReadOnly)
        self._sink = QAudioSink(dev, fmt, self)
        self._sink.setVolume(self.volume.value() / 100)
        self._sink.stateChanged.connect(lambda st: st == QAudio.State.IdleState and self._stop())
        self._secs = len(pcm) / rate
        self.progress.setRange(0, max(1, int(self._secs)))
        self._sink.start(self._buf)
        self._timer.start(250)
        self.play_btn.setEnabled(False)
        self._ws._set_status(f"Tape Out: playing {self.system.currentText()} ({int(self._secs)} s)")

    def _tick(self): #vers 1
        if self._sink:
            self.progress.setValue(int(self._sink.processedUSecs() / 1e6))

    def _stop(self): #vers 1
        self._timer.stop()
        if self._sink:
            self._sink.stop()
            self._sink.deleteLater()
            self._sink = None
        if self._buf:
            self._buf.close()
            self._buf = None
        self.progress.setValue(0)
        self.play_btn.setEnabled(True)

    def _save_wav(self): #vers 1
        path, _ = QFileDialog.getSaveFileName(self, "Save tape audio", f"{self._name}.wav", "WAV (*.wav)")
        if path:
            with open(path, 'wb') as f:
                f.write(self._tape()[1])
            self._ws._set_status(f"Tape Out: saved {path}")

    def _save_tape(self): #vers 1
        ext = T.SYSTEMS[self.system.currentData()][1]
        path, _ = QFileDialog.getSaveFileName(self, "Save tape file", f"{self._name}.{ext}", f"{ext.upper()} (*.{ext})")
        if path:
            with open(path, 'wb') as f:
                f.write(self._tape()[0])
            self._ws._set_status(f"Tape Out: saved {path}")

    def closeEvent(self, e): #vers 1
        self._stop()
        super().closeEvent(e)
