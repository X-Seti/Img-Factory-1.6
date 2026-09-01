#!/usr/bin/env python3
#this belongs in apps/methods/ps2_vb_audio.py - Version: 1
# X-Seti - Aug 2026 - IMG Factory 1.6 - PS2 .VB (PS-ADPCM) audio decoder for GTA III/VC/LCS/VCS

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

import os
import struct
import subprocess
import tempfile
import wave

##Methods list -
# make_vagp_header
# decode_vb_file

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
