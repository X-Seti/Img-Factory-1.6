Drop real audio zone sound files here.

Double-clicking a row in the Auzo list (IPL File Display, when an
Auzo IPL is selected) checks this folder first, before falling back
to a synthetic placeholder tone.

Matched by filename, tried in this order, case-insensitive:
  <sound_id>.wav / .mp3 / .ogg / .flac   (e.g. "5.wav")
  <zone name>.wav / .mp3 / .ogg / .flac  (e.g. "AZ_PARK1.wav")

Whichever matches first for that row is played. No match falls back
to the synthetic tone, as before.
