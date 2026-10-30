Drop real audio zone sound files here.

Double-clicking a row in the Auzo list (IPL File Display, when an
Auzo IPL is selected) checks this folder first, before falling back
to a synthetic placeholder tone.

Matched by filename, tried in this order, case-insensitive:
  <sound_id>.wav / .mp3 / .ogg / .flac        (e.g. "54.wav")
  <zone name>.wav / .mp3 / .ogg / .flac       (e.g. "clothgp.wav")
  <AUZO_TYPES music description>.wav / etc    (e.g. "KDST.wav")

The third option is the most useful for the many Auzo entries whose
own real music is one of SA's own real radio stations (K-DST, CSR,
SFUR, etc. - see AUZO_TYPES in gta_dat_parser.py for the full,
confirmed list) - unlike most other in-game sound effects, radio
station music is typically stored as plain, separate stream files in
a real SA install's own audio/ folder, not compiled into the game's
own inaccessible audio bank format.

Whichever candidate matches first for that row is played. No match
falls back to the synthetic tone, as before.
