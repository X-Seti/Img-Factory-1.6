#!/usr/bin/env python3
#this belongs in apps/methods/sa_audio_stream.py - Version: 1
# X-Seti - Aug 2026 - IMG Factory 1.6 - GTA SA "audio stream" format decoder (AMBIENCE/GENRL/radio station files)

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

import os
import struct
from typing import List, NamedTuple, Optional

##Methods list -
# xor_decode
# parse_stream_tracks
# extract_track
# extract_all_tracks

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
