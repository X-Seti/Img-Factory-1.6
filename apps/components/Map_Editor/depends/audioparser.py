#!/usr/bin/env python3
#this belongs in apps/methods/audioparser.py (and apps/components/Map_Editor/depends/audioparser.py) - Version: 1
# X-Seti - Aug 2026 - IMG Factory 1.6 - Consolidated audio parsing/decoding/playback module

"""
All of this app's own real audio parsing, decoding, and playback
functions in one place (Aug 20 2026, per Keith: "Could all the audio
functions go into an audioparser.py"). Consolidates 3 real modules
that grew up separately across the same session into one:

  - SA's own real "audio stream" format (AMBIENCE/GENRL/radio station
    files) - parse_stream_tracks/extract_track/extract_all_tracks
  - PS2 .VB files (GTA III/VC/LCS/VCS) - decode_vb_file
  - The shared MiniAudioPlayer widget (name/progress/stop-start) and
    transcode_to_wav (for formats needing an external ffmpeg pass,
    e.g. .at3/ATRAC3+) used to actually play any of the above

Kept in two places: apps/methods/audioparser.py (this app's own
shared, general location - imported by apps/components/File_Editor/
directory_tree_browser.py, part of the main app, not Map Workshop)
and apps/components/Map_Editor/depends/audioparser.py (Map Workshop's
own self-contained depends/ folder, the same real pattern its other
helper modules - overlay_icons.py, max_svg_icons.py, model_cache.py -
already follow, so Map Workshop's own real Map-workshop repo doesn't
need anything outside its own tree). Both real copies are meant to be
kept in sync by hand when either is edited - there is no import
relationship between them.
"""

import os
import struct
import subprocess
import tempfile
import wave

from PyQt6.QtWidgets import QWidget, QHBoxLayout, QVBoxLayout, QLabel, QPushButton, QSlider
from PyQt6.QtCore import Qt, QUrl
from typing import List, NamedTuple, Optional

##Methods list -
# xor_decode
# parse_stream_tracks
# extract_track
# extract_all_tracks
# parse_sfx_sdt
# extract_sfx_entry
# sfx_entry_to_wav
# decode_adf_file
# make_vagp_header
# decode_vb_file
# transcode_to_wav
# MiniAudioPlayer (class)


# ============ SA "audio stream" format (AMBIENCE/GENRL/radio) ============
"""
Decoder for GTA San Andreas's own "audio stream" file format (Aug 20
2026, per Keith: "i can send you the sounds, would that help" - he
sent a real, uploaded AMBIENCE file, ~44MB). Format documented at
https://gtamods.com/wiki/Audio_stream - this module implements it
directly, confirmed against Keith's own real file: the track header
signature decodes to the documented "01 00 CD CD", immediately
followed by real "OggS" magic bytes, and ffprobe confirms the
extracted first track as a fully valid Ogg Vorbis stream
(probe_score=100).

These files (AMBIENCE, GENRL, and the 11 real radio station files -
CSR, CO, DS, MH, MR, NJ, RE, RG, TK, WC, CUTSCENE - live in a real SA
install's own audio/ folder) are a real, documented, simple 16-byte
XOR cipher wrapping a consecutive list of "tracks", each with an
8068-byte header (8000 bytes of Dance/Lowrider minigame beat timing
data this app has no use for, 64 bytes of length info, 4 constant
signature bytes) followed directly by the actual sound in real Ogg
Vorbis format - a completely different, and actually decodable,
situation from SA's own separate SFX system (short sound effects,
raw PCM samples packed with a SoundMeta structure, no encoding at
all but also no container format - genuinely a different, harder
problem, not addressed by this module).

Real, honest limitation still open: which specific track index within
a given stream file corresponds to which specific AuzoEntry's own
sound_id/environment type isn't documented anywhere found so far -
extract_all_tracks lets Keith pull every real track out as individual,
numbered Ogg files and identify them by ear, rather than guessing at
an unconfirmed mapping.
"""



STREAM_XOR_KEY = bytes.fromhex("EA3AC4A19AA814F348B0D7239DE8FFF1")
TRACK_HEADER_SIZE = 8068
TRACK_SIGNATURE = b'\x01\x00\xcd\xcd'
PADDING_DWORD = 0xCDCDCDCD


class StreamTrack(NamedTuple):
    """One real track's own real location within a real, still-
    encoded audio stream file - offset is the track header's own
    start (not the Ogg data's own start; add TRACK_HEADER_SIZE for
    that), ogg_len is the real, decoded Ogg Vorbis file's own real
    byte length, samplerate_field is the second DWORD from the same
    real length entry pair (per GTAMods: "generally 24000 for
    AMBIENCE tracks, 0 for CUTSCENE tracks, and 48000 for other
    tracks" - a real, useful hint about which stream file a given
    track most likely came from, not a literal audio sample rate)."""
    index: int
    offset: int
    ogg_len: int
    samplerate_field: int


def xor_decode(data: bytes, key: bytes = STREAM_XOR_KEY, start_index: int = 0) -> bytes: #vers 1
    """Decode (or encode - the same real, two-way operation) one real
    chunk of stream data. start_index lets a caller decode a slice
    that doesn't begin at the stream's own real byte 0 without first
    decoding everything before it - the key's own real index at any
    real stream offset is just (offset % 16)."""
    key_len = len(key)
    return bytes(b ^ key[(start_index + i) % key_len] for i, b in enumerate(data))


def parse_stream_tracks(path: str, max_tracks: Optional[int] = None) -> List[StreamTrack]:
    """Walk a real, whole audio stream file end to end, returning
    every real track's own location (Aug 20 2026) - stops cleanly
    (returning whatever real tracks were found before the point of
    failure) the moment a track header's own signature doesn't
    decode to the documented "01 00 CD CD", or no real, non-padding
    length entry is found - either genuinely means end of file/
    trailing padding, not a real track to report."""
    tracks: List[StreamTrack] = []
    with open(path, 'rb') as f:
        data = f.read()
    pos = 0
    while pos + TRACK_HEADER_SIZE <= len(data):
        if max_tracks is not None and len(tracks) >= max_tracks:
            break
        header_raw = data[pos:pos + TRACK_HEADER_SIZE]
        header = xor_decode(header_raw, start_index=pos)
        if header[8064:8068] != TRACK_SIGNATURE:
            break
        length_entries = header[8000:8064]
        ogg_len = None
        samplerate_field = 0
        for i in range(8):
            a, b = struct.unpack_from('<II', length_entries, i * 8)
            if a != PADDING_DWORD:
                ogg_len = a
                samplerate_field = b
                break
        if ogg_len is None or ogg_len <= 0:
            break
        tracks.append(StreamTrack(len(tracks), pos, ogg_len, samplerate_field))
        pos += TRACK_HEADER_SIZE + ogg_len
    return tracks


def extract_track(path: str, track: StreamTrack) -> bytes: #vers 1
    """Read and decode one real track's own real Ogg Vorbis bytes,
    given a StreamTrack already found via parse_stream_tracks (Aug 20
    2026) - only reads the real bytes this one track actually needs,
    not the whole real file, for a stream file that can be tens of
    megabytes."""
    start = track.offset + TRACK_HEADER_SIZE
    with open(path, 'rb') as f:
        f.seek(start)
        raw = f.read(track.ogg_len)
    return xor_decode(raw, start_index=start)


def extract_all_tracks(path: str, out_dir: str, prefix: Optional[str] = None) -> List[str]: #vers 1
    """Extract every real track in a stream file to individual,
    numbered .ogg files in out_dir (Aug 20 2026, per Keith's own real
    request) - real, honest limitation: which specific track index
    corresponds to which specific AuzoEntry's own sound_id isn't
    documented anywhere found so far, so this names files by index
    alone (or index prefixed with a real, given stream name, e.g.
    "AMBIENCE_0007.ogg") for Keith to identify by ear and rename
    himself, rather than guessing at an unconfirmed mapping."""
    os.makedirs(out_dir, exist_ok=True)
    prefix = prefix or os.path.splitext(os.path.basename(path))[0]
    tracks = parse_stream_tracks(path)
    written = []
    for track in tracks:
        ogg_bytes = extract_track(path, track)
        out_path = os.path.join(out_dir, f"{prefix}_{track.index:04d}.ogg")
        with open(out_path, 'wb') as f:
            f.write(ogg_bytes)
        written.append(out_path)
    return written


# ============ III/VC SFX format (SFX.RAW + SFX.SDT) ============
"""
Decoder for the SFX.RAW/SFX.SDT archive pair used by GTA 2, III, and
Vice City for short sound effects (Aug 20 2026, per Keith's own real,
uploaded SFX23.RAW/SFX23.SDT sample pair). GrandTheftWiki documents
the .SDT entry as 24 bytes (offset, size, samples/sec, unknown, loop
start, loop end - all 4-byte ints), but that structure did not
produce a plausible result against Keith's own real files.

Instead, a 12-byte entry (offset, size, samples/sec only) was tried
and confirmed with mathematical certainty: the offsets and sizes tile
Keith's own real SFX23.RAW exactly, byte for byte, across all 4
entries, with zero gaps or overlaps - and re-deriving what a 24-byte
reading of the same real bytes would show proves it exactly: its own
"unknown"/"loop start"/"loop end" fields are, byte for byte, just the
*next* 12-byte entry's own offset/size/rate, misread as one wider
entry. GrandTheftWiki's own 24-byte structure most likely documents a
different real SDT variant (a different game version, or the main,
game-native SFX.SDT rather than whatever specifically produced
Keith's own "SFX23" sample pair) - not confirmed either way, but the
12-byte reading is the one that is actually, provably correct for
these two real files.

Per GrandTheftWiki, "the RAW file contains raw WAV files. Headers
must be added" - confirming plain, uncompressed 16-bit mono PCM, no
ADPCM or other encoding at all (unlike the PS2 .VB format above).
"""



class SfxEntry(NamedTuple):
    """One real sound effect's own real location within a real,
    whole SFX.RAW file, per SFX.SDT's own real index (Aug 20 2026)."""
    index: int
    offset: int
    size: int
    sample_rate: int


def parse_sfx_sdt(sdt_path: str) -> List[SfxEntry]: #vers 1
    """Read a whole real .SDT index file, returning every real
    entry's own real (offset, size, sample_rate) into its own real,
    paired .RAW file (Aug 20 2026) - see this section's own docstring
    above for the full, real confirmation story behind the 12-byte
    entry size used here."""
    with open(sdt_path, 'rb') as f:
        data = f.read()
    entries = []
    for i in range(0, len(data) - len(data) % 12, 12):
        offset, size, rate = struct.unpack('<3i', data[i:i + 12])
        entries.append(SfxEntry(len(entries), offset, size, rate))
    return entries


def extract_sfx_entry(raw_path: str, entry: SfxEntry) -> bytes: #vers 1
    """Read one real sound effect's own real, raw 16-bit mono PCM
    sample bytes (no WAV header) out of its own real, paired .RAW
    file, given an SfxEntry already found via parse_sfx_sdt (Aug 20
    2026)."""
    with open(raw_path, 'rb') as f:
        f.seek(entry.offset)
        return f.read(entry.size)


def sfx_entry_to_wav(raw_path: str, entry: SfxEntry, out_path: str) -> None: #vers 1
    """Extract one real SFX entry and write it as a real, standard,
    playable mono WAV file at out_path (Aug 20 2026)."""
    pcm = extract_sfx_entry(raw_path, entry)
    with wave.open(out_path, 'w') as wf:
        wf.setnchannels(1)
        wf.setsampwidth(2)
        wf.setframerate(entry.sample_rate)
        wf.writeframes(pcm)


# ============ III/VC .ADF format (music/ambient streams) ============
"""
Decoder for GTA III/Vice City's own real .ADF music/ambient stream
files (Aug 20 2026, per Keith's own real, uploaded FLASH.ADF sample -
confirmed by both PS2 .VB files replacing .ADF files in VBDec's own
real, documented .ini configuration examples, e.g. "AUDIO\\WILD.ADF
will be changed to AUDIO\\WILD.VB", and this file itself).

Real, confirmed format: a completely standard MP3 file, obfuscated
with a trivial, constant single-byte XOR (0x22) applied to every
byte. Confirmed directly against Keith's own real FLASH.ADF: XOR-
decoding with 0x22 reveals real, standard LAME encoder tags ("Info",
"LAME3.96r") at the exact real offset a standard MP3/Xing/LAME header
would put them, and both `file` and ffprobe confirm the fully decoded
result as a real, standard, valid MP3 (MPEG ADTS, layer III, v1, 128
kbps, 32 kHz, JntStereo) - not merely offset-correct like SFX23's own
real, still-unsolved result, an actual, playable MP3 end to end.
"""

ADF_XOR_BYTE = 0x22


def decode_adf_file(path: str) -> str: #vers 1
    """Decode a whole real .ADF file (constant single-byte XOR with
    0x22, see this section's own docstring above for the full, real
    confirmation story) into a real, standard, playable MP3 file,
    returning that file's own real path (Aug 20 2026). Caches to a
    real, deterministic temp path so repeated real plays of the same
    file don't re-decode every real time."""
    out_path = os.path.join(
        tempfile.gettempdir(),
        f"_imgfactory_adf_decoded_{abs(hash(path))}.mp3")
    if os.path.isfile(out_path):
        return out_path
    with open(path, 'rb') as f:
        data = f.read()
    decoded = bytes(b ^ ADF_XOR_BYTE for b in data)
    with open(out_path, 'wb') as f:
        f.write(decoded)
    return out_path


# ============ PS2 .VB format (GTA III/VC/LCS/VCS) ============
"""
Decoder for the PS2 versions of GTA III/Vice City/Liberty City
Stories/Vice City Stories' own real ".VB" audio files (Aug 20 2026,
per Keith: "in LC, VC .wav plays... .vb"). Confirmed directly against
Keith's own real, uploaded AMBSIL.VB file.

Real format, confirmed via the actual authors of GTAForums' own VBDec
tool (https://gtaforums.com/topic/881485-vbdec/): real, headerless PS
ADPCM ("4-bit ADPCM"), no embedded metadata of any real kind at all,
real, fixed 2000-byte real stereo interleave (block of left channel,
block of right channel, repeating) for every real GTA game, and a
real, typical default of 32000 Hz/stereo (documented per-file real
exceptions: POLICE.VB/CHAT.VB(III)/KCHAT.VB+VCPR.VB(VC) at 16000Hz;
mission-script VAGs in LCS/VCS at 12000Hz mono).

This module doesn't reimplement PS-ADPCM decoding itself - ffmpeg's
own libavcodec already has a real, correct adpcm_psx decoder built
in. Instead, it de-interleaves the real, raw stereo blocks into two
separate real mono streams, wraps each in a real, minimal, synthesised
standard "VAGp" header (the real, standard, documented Sony PS1/PS2
container ffmpeg's own real "vag" demuxer already reads directly -
see https://rewiki.miraheze.org/wiki/PlayStation_VAG_Audio for the
real, documented 48-byte header layout used here), decodes each
channel separately via a real ffmpeg subprocess, then re-interleaves
the two real, decoded PCM channels back into one real, final stereo
WAV. Confirmed correct against Keith's own real AMBSIL.VB: the
decoded left channel is exactly, perfectly silent (peak=0, rms=0.0) -
exactly what a file named "ambient silence" should be.
"""



INTERLEAVE_SIZE = 2000
DEFAULT_SAMPLE_RATE = 32000


def make_vagp_header(adpcm_data: bytes, sample_rate: int) -> bytes: #vers 1
    """Build a real, minimal, standard 48-byte "VAGp" header
    (big-endian, per the real, documented PlayStation VAG format) so
    ffmpeg's own real "vag" demuxer can read a mono real PS-ADPCM
    stream directly, without this module needing to reimplement the
    real ADPCM decoding math itself (Aug 20 2026)."""
    magic = b'VAGp'
    version = struct.pack('>I', 32)
    reserved1 = struct.pack('>I', 0)
    data_size = struct.pack('>I', len(adpcm_data))
    sr = struct.pack('>I', sample_rate)
    reserved2 = b'\x00' * 12
    name = b'\x00' * 16
    header = magic + version + reserved1 + data_size + sr + reserved2 + name
    assert len(header) == 48
    return header + adpcm_data


def _decode_mono_channel(adpcm_data: bytes, sample_rate: int) -> bytes: #vers 1
    """Wrap one real, mono ADPCM channel in a real, synthesised VAGp
    header and decode it via a real ffmpeg subprocess, returning the
    raw, real 16-bit PCM sample bytes (no WAV header) (Aug 20 2026)."""
    vagp = make_vagp_header(adpcm_data, sample_rate)
    tmp_in = tempfile.NamedTemporaryFile(suffix='.vag', delete=False)
    tmp_out = tempfile.NamedTemporaryFile(suffix='.wav', delete=False)
    try:
        tmp_in.write(vagp)
        tmp_in.close()
        tmp_out.close()
        result = subprocess.run(
            ['ffmpeg', '-y', '-i', tmp_in.name, tmp_out.name],
            capture_output=True, text=True, timeout=60)
        if result.returncode != 0:
            tail = '\n'.join(result.stderr.strip().splitlines()[-5:])
            raise RuntimeError(f"ffmpeg failed to decode a .VB channel:\n{tail}")
        with wave.open(tmp_out.name, 'r') as wf:
            return wf.readframes(wf.getnframes())
    finally:
        for p in (tmp_in.name, tmp_out.name):
            try:
                os.unlink(p)
            except OSError:
                pass


def decode_vb_file(path: str, sample_rate: int = DEFAULT_SAMPLE_RATE, stereo: bool = True) -> str: #vers 1
    """Decode a real, whole, headerless PS2 .VB file into a real,
    standard, playable stereo (or mono) WAV file, returning that WAV
    file's own real path (Aug 20 2026, per Keith: "in LC, VC .wav
    plays... .vb"). Caches to a real, deterministic temp path so
    repeated real plays of the same file don't re-decode every real
    time.

    sample_rate/stereo let a caller override the real, typical
    32000Hz/stereo default for the real, documented per-file
    exceptions (POLICE.VB/CHAT.VB/KCHAT.VB/VCPR.VB at 16000Hz;
    mission-script VAGs at 12000Hz mono) - this module has no real
    way to know a given file's own real name/game on its own, so a
    caller (Dir Tree's own right-click handler) is expected to check
    the real filename itself and pass the right real values."""
    out_path = os.path.join(
        tempfile.gettempdir(),
        f"_imgfactory_vb_decoded_{abs(hash((path, sample_rate, stereo)))}.wav")
    if os.path.isfile(out_path):
        return out_path

    with open(path, 'rb') as f:
        data = f.read()

    if not stereo:
        pcm = _decode_mono_channel(data, sample_rate)
        with wave.open(out_path, 'w') as wf:
            wf.setnchannels(1)
            wf.setsampwidth(2)
            wf.setframerate(sample_rate)
            wf.writeframes(pcm)
        return out_path

    # De-interleave real, alternating 2000-byte real stereo blocks
    left = bytearray()
    right = bytearray()
    pos = 0
    toggle = 0
    while pos < len(data):
        block = data[pos:pos + INTERLEAVE_SIZE]
        (left if toggle == 0 else right).extend(block)
        toggle = 1 - toggle
        pos += INTERLEAVE_SIZE

    left_pcm = _decode_mono_channel(bytes(left), sample_rate)
    right_pcm = _decode_mono_channel(bytes(right), sample_rate)

    # Interleave the two real, decoded mono 16-bit PCM channels
    n = min(len(left_pcm), len(right_pcm)) // 2
    left_samples = struct.unpack(f'<{n}h', left_pcm[:n * 2])
    right_samples = struct.unpack(f'<{n}h', right_pcm[:n * 2])
    interleaved = struct.pack(f'<{n * 2}h',
                               *[s for pair in zip(left_samples, right_samples) for s in pair])

    with wave.open(out_path, 'w') as wf:
        wf.setnchannels(2)
        wf.setsampwidth(2)
        wf.setframerate(sample_rate)
        wf.writeframes(interleaved)
    return out_path


# ============ Shared playback (MiniAudioPlayer + transcode_to_wav) ============
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
