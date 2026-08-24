#this belongs in methods/gta_dat_parser.py - Version: 5
# X-Seti - March 2026 - IMG Factory 1.6 - GTA Data File Parser
"""
GTA3 + VC + SA + GTASOL Data File Parser — mirrors the RenderWare engine load chain exactly.

GTA3 load order (verified from real files):
  Phase 1: data/default.dat  -> IDE DATA/DEFAULT.IDE, TEXDICTION, MODELFILE, COLFILE
  Phase 2: data/gta3.dat     -> 16 IDEs, 16 COLFILEs (island index 0-3), 14 IPLs

VC load order (verified from real files):
  Phase 1: data/default.dat  -> IDE DATA/DEFAULT.IDE, TEXDICTION, MODELFILE, COLFILE
  Phase 2: data/gta_vc.dat   -> 31 IDEs, 1 COLFILE, 36 IPLs

SA load order (verified from real files):
  Phase 1: data/default.dat  -> 3 IDEs (DEFAULT.IDE + VEHICLES.IDE + PEDS.IDE), 1 COLFILE
  Phase 2: data/gta.dat      -> 3 IMGs, 54 IDEs, 52 IPLs, 0 COLFILEs
  Alt:     data/gta_quick.dat -> stripped dev variant (1 IMG, 13 IDE, 11 IPL)

SOL (GTASOL mod) load order (verified from real files):
  Phase 1: sol/special.dat   -> IDE models/gta3.ide, TEXDICTION, MODELFILE, 2 COLFILEs
  Phase 2: sol/gta_sol.dat   -> 12 CDIMAGEs, 16 IDEs (+5 .iFX lighting), 11 COLFILEs, 111 IPLs
  Alt DAT: sol/gtasol.dat    -> same content, no-underscore variant
  Sol dir: sol/ or SOL/      -> case-insensitive search on Linux required
  Paths:   relative to SA game root (not to dat file); sol/ and models/ prefixes
  Notes:   .iFX files are listed as IDE directives (SA 2dfx lighting extension)
           CDIMAGE and IMG are interchangeable directives (same meaning)

Field formats per game (verified from real .ide files):
  GTA3 objs: id, model, txd, meshCount, dist1[, dist2], flags
  GTA3 peds: id, model, txd, pedType, behaviour, animGroup, carsDriveMask
  GTA3 cars: id, model, txd, type, handlingId, gameName, class, freq, level, compRules[, wheelId, wheelScale]
  GTA3 weap: id, model, txd, meshCount, drawDist, flags
  GTA3 hier: id, model, txd
  GTA3 inst: id, model, px, py, pz, sx, sy, sz, rx, ry, rz, rw  (12 fields)

  VC/SA peds: ..., carsDriveMask, animFile, radio1, radio2        (+3 vs GTA3)
  VC/SA cars: ..., gameName, animFile, class, ...                  (+animFile vs GTA3)
  VC/SA weap: id, model, txd, animFile, meshCount, drawDist, flags (+animFile vs GTA3)
  VC   hier:  id, model, txd                                        (same as GTA3)
  SA   hier:  id, model, txd, animFile, drawDist                   (5 fields)
  SA   inst:  id, model, interior, px, py, pz, rx, ry, rz, rw[, lod]

  SOL: SA-format IDE/IPL sections (mod runs on SA engine)
"""

import os
import re
import struct
import math
from typing import Dict, List, Optional, Tuple, Any
from dataclasses import dataclass, field


class GTAGame:
    GTA3 = "gta3"
    VC   = "vc"
    SA   = "sa"
    SOL  = "sol"   # GTASOL mod (SA engine, multi-city)

    DAT_FILE = {
        "gta3": "gta3.dat",
        "vc":   "gta_vc.dat",
        "sa":   "gta.dat",
        "sol":  "gta_sol.dat",   # lives in sol/ or SOL/ subfolder
    }

    # Alternative DAT names
    ALT_DAT_FILE = {
        "sa":  "gta_quick.dat",
        "sol": "gtasol.dat",     # no-underscore variant
    }

    # Phase-1 dat loaded before the main dat
    # SOL uses special.dat (in same sol/ folder) as its phase-1 loader
    DEFAULT_DAT = {
        "gta3": "default.dat",   # in data/
        "vc":   "default.dat",   # in data/
        "sa":   "default.dat",   # in data/
        "sol":  "special.dat",   # in sol/ or SOL/
    }

    DATA_SUBDIR = "data"
    SOL_SUBDIRS = ("sol", "SOL")  # case variants to try on Linux

    IDE_SECTIONS = {
        "gta3": {"objs", "tobj", "weap", "hier", "anim", "cars", "peds", "path"},
        "vc":   {"objs", "tobj", "weap", "hier", "anim", "cars", "peds", "path", "txdp"},
        "sa":   {"objs", "tobj", "weap", "hier", "anim", "cars", "peds", "path",
                 "txdp", "2dfx", "tanm"},
        "sol":  {"objs", "tobj", "weap", "hier", "anim", "cars", "peds", "path",
                 "txdp", "2dfx", "tanm"},  # SA engine — same sections
    }

    IPL_SECTIONS = {
        # "path" (Aug 1 2026, per Keith: "we need to address... path
        # for GTAIII and extended for VC") - GTA3/VC only. SA uses a
        # completely different, binary, per-area path file format
        # (not part of the text IPL at all), so it's deliberately
        # excluded here.
        "gta3": {"inst", "cull", "pick", "jump", "enex", "cars", "auzo", "path"},
        # "occl" added to VC (Aug 16 2026 fix) - was missing entirely
        # even though occlu.ipl is a real VC file (confirmed via
        # GTAMods/Grand Theft Wiki: "OCCL is a section... in Vice
        # City, San Andreas, and GTA IV" - VC was simply left off this
        # set by mistake, occl sections in a real VC IPL would have
        # silently gone unrecognised).
        "vc":   {"inst", "cull", "pick", "jump", "enex", "cars", "auzo", "zone", "path", "occl"},
        "sa":   {"inst", "cull", "pick", "jump", "enex", "cars", "auzo",
                 "zone", "occl", "mult", "grge", "tcyc", "scrn"},
        "sol":  {"inst", "cull", "pick", "jump", "enex", "cars", "auzo",
                 "zone", "occl", "mult", "grge", "tcyc", "scrn"},  # SA engine
    }

    ID_RANGES = {
        "gta3": (0,  5999),
        "vc":   (0,  5999),
        "sa":   (0, 19999),
        "sol":  (0, 65535),  # multi-city mod — expanded ID space
    }


@dataclass
class DATEntry:
    directive:   str
    path:        str
    abs_path:    str  = ""
    exists:      bool = False
    extra:       str  = ""     # island index for COLFILE
    source_dat:  str  = ""


@dataclass
class IDEObject:
    model_id:    int
    model_name:  str
    txd_name:    str
    obj_type:    str
    section:     str
    extra:       Dict[str, Any] = field(default_factory=dict)
    source_ide:  str = ""
    line_no:     int = 0


@dataclass
class IPLInstance:
    model_id:    int
    model_name:  str
    interior:    int
    pos_x:       float
    pos_y:       float
    pos_z:       float
    rot_x:       float
    rot_y:       float
    rot_z:       float
    rot_w:       float
    lod_index:   int  = -1
    scale_x:     float = 1.0
    scale_y:     float = 1.0
    scale_z:     float = 1.0
    source_ipl:  str  = ""
    line_no:     int  = 0


@dataclass
class PathNode: #vers 1
    """One sub-node within a path group (Aug 1 2026, per Keith: "we
    need to address... path for GTAIII and extended for VC"). Twelve
    fields per Project Cerbera's VC path documentation: node_type,
    next, zero (always 0, unused), x/y/z (already converted from the
    file's own precision units to standard world units - see
    IPLParser._parse_path_node for the conversion), median, left,
    right, flag1-3. x/y/z land in the same coordinate space as inst
    positions, so a path node's position is directly comparable to
    (and, e.g., nudge-editable alongside) instance/object positions."""
    node_type:  int
    next_id:    int
    x:          float
    y:          float
    z:          float
    median:     float = 0.0
    left:       int   = 0
    right:      int   = 0
    flag1:      int   = 0
    flag2:      int   = 0
    flag3:      int   = 0


@dataclass
class PathGroup: #vers 1
    """One path group - up to 12 PathNodes sharing a header line (two
    values, meaning not fully documented publicly; preserved verbatim
    as header_a/header_b rather than guessed at, since Project Cerbera
    itself only confirms the group defines the vehicle type the sub-
    nodes apply to, not what the specific header values mean)."""
    header_a:   int
    header_b:   int
    nodes:      List[PathNode] = field(default_factory=list)
    source_ipl: str = ""
    line_no:    int = 0


@dataclass
class IDEPathNode: #vers 1
    """One sub-node within a GTA III IDE-embedded path group (Aug 16
    2026, per Keith: "gta3 game files need special treatment; the
    IPL path data is stored within the .ide map files" and his real
    uploaded comse.ide/comSE.ipl sample). Nine fields per Project
    Cerbera's own "PATH (IDE Section)" documentation, confirmed
    field-for-field against that real file: NodeType, NextNode,
    IsCrossRoad, XRel, YRel, ZRel, Median, LeftLanes, RightLanes -
    genuinely different from VC/SA's own path node shape, not just a
    shorter version of it (no separate Flag1-3, an added IsCrossRoad
    flag VC doesn't have).

    x_rel/y_rel/z_rel are relative to wherever the *placed instance*
    of this group's own model actually sits in the world (see
    IDEPathGroup's own docstring) - unlike VC/SA path nodes, which
    already carry absolute world coordinates directly. No confirmed
    scale-division factor for these (unlike SA's documented /8 or
    VC's confirmed /16) - Keith's real sample values are already
    plausible small building-relative offsets (tens to low hundreds
    of units), so stored as read with no scaling applied; revisit if
    real placed-instance rendering later shows otherwise."""
    node_type:    int
    next_id:      int
    is_crossroad: int
    x_rel:        float
    y_rel:        float
    z_rel:        float
    median:       float = 0.0
    left:         int   = 0
    right:        int   = 0


@dataclass
class IDEPathGroup: #vers 1
    """One GTA III IDE-embedded path group - bound to a specific
    object definition rather than living freestanding in an IPL, per
    Project Cerbera: "GTA III uses an IDE-related paths system, which
    binds paths to certain objects." group_type is "ped" or "car"
    (confirmed both appear in Keith's real comse.ide); model_id/
    model_name identify which OBJS entry this group belongs to - a
    group only becomes a real, world-space path once that model_id is
    actually placed somewhere via a normal INST line in a matching
    .ipl (the same model can be placed multiple times, each placement
    getting its own copy of this same relative path, transformed by
    that instance's own position/rotation - not resolved to world
    space here, that's a separate step once instance data is
    available too)."""
    group_type:  str
    model_id:    int
    model_name:  str
    nodes:       List[IDEPathNode] = field(default_factory=list)
    source_ide:  str = ""
    line_no:     int = 0


@dataclass
class GrgeEntry: #vers 1
    """One SA "grge" section entry - a garage (Aug 1 2026, per Keith's
    real example data: "2502.31, -1699.36, 12.4323, 2508.61, -1699.36,
    2502.31, -1691.01, 16.5666, 1, 16, cjsafe"). Eleven fields,
    verified against SannyBuilder forum documentation and Keith's own
    data (door_type=1, garage_type=16 = "Save garage (Ganton)",
    name="cjsafe" - all consistent with each other): X1,Y1,Z1 (lower
    corner), front_x,front_y (front-face corner), X2,Y2,Z2 (upper
    corner), door_type, garage_type, name."""
    x1:          float
    y1:          float
    z1:          float
    front_x:     float
    front_y:     float
    x2:          float
    y2:          float
    z2:          float
    door_type:   int
    garage_type: int
    name:        str
    source_ipl:  str = ""
    line_no:     int = 0


@dataclass
class EnexEntry: #vers 1
    """One SA "enex" section entry - an entrance/exit marker (Aug 1
    2026, per Keith's real example data: "2309.62, -1643.63, 13.8385,
    0, 1.6, 1.6, 8, 2308.12, -1643.63, 13.8385, 93, 0, 260, "BAR2", 0,
    2, 0, 24"). Eighteen fields, verified against Grand Theft Wiki's
    ENEX documentation and confirmed matching Keith's own data field-
    for-field: enter_x/y/z (marker position), enter_angle, size_x/y/z
    (trigger box), exit_x/y/z (where the player ends up), exit_angle,
    target_interior, flags, name (interior name string, e.g. "BAR2"),
    sky, num_peds_to_spawn, time_on, time_off."""
    enter_x:            float
    enter_y:            float
    enter_z:            float
    enter_angle:        float
    size_x:             float
    size_y:             float
    size_z:             float
    exit_x:             float
    exit_y:             float
    exit_z:             float
    exit_angle:         float
    target_interior:    int
    flags:               int
    name:                 str
    sky:                  int
    num_peds_to_spawn:    int
    time_on:              int
    time_off:             int
    source_ipl:           str = ""
    line_no:              int = 0


@dataclass
class CullEntry: #vers 1
    """One GTA3/VC "cull" section entry - a cull zone (Aug 16 2026,
    per Keith's real cull.ipl upload). Eleven fields, confirmed
    against multiple independent wiki sources (GTA Wiki Fandom,
    GTAMods, Grand Theft Wiki all agree) and verified field-for-field
    against Keith's real data: CenterX/Y/Z, X1/Y1/Z1 (one box corner),
    X2/Y2/Z2 (the opposite corner), flags, wanted_level_drop.

    The center position is a real, documented oddity, not a mistake
    in this dataclass: per the wiki, "changing the zone's center
    coordinates does not directly affect the zone itself" - the box
    shape is defined entirely by the two corner points, center is
    only used for distance calculations (e.g. how far the player is
    from the zone) and isn't required to be the box's geometric
    centre at all, so it's stored verbatim rather than derived.

    This replaces a previous version that returned a plain dict with
    an entirely wrong field layout (assumed 7 fields - a center/
    width/height box - when real cull.ipl lines have 11, two real
    corner points, not a width+height pair at all)."""
    center_x:          float
    center_y:          float
    center_z:          float
    x1:                float
    y1:                float
    z1:                float
    x2:                float
    y2:                float
    z2:                float
    flags:             int   = 0
    wanted_level_drop: int   = 0
    source_ipl:        str   = ""
    line_no:           int   = 0


@dataclass
class OcclEntry: #vers 1
    """One "occl" section entry - an occlusion culling zone (Aug 16
    2026, per Keith's real occlu.ipl upload). Seven fields, confirmed
    against GTAMods/Grand Theft Wiki (both agree word-for-word) and
    verified field-for-field against Keith's real data: MidX, MidY,
    BottomZ, WidthX, WidthY, Height, Rotation - an axis-aligned box in
    plan (X/Y extent from the center, per the wiki: "MidX, MidY") but
    NOT axis-aligned in world space, since Rotation turns the whole
    box around its own vertical (Z) axis - genuinely different from
    CullEntry's two-corner-points shape, which has no rotation at
    all. Used "to create occlusion culling zones... since it is
    wasteful to render models behind opaque models, these zones can
    disable the rendering of any models that are directly behind
    them" - per the wiki, deleting buildings near an occlusion zone
    without also removing the zone itself causes visible pop-in,
    since the zone keeps hiding whatever used to be behind the
    building that's no longer there."""
    mid_x:      float
    mid_y:      float
    bottom_z:   float
    width_x:    float
    width_y:    float
    height:     float
    rotation:   float = 0.0
    source_ipl: str   = ""
    line_no:    int   = 0


# Audio Zone Types (Aug 20 2026, per Keith: "Implement support for
# the remaining SA, audiozone placements with sound svg icons; play
# the sounds") - the real, published environment-type/music-track
# table for AUZO's own ID field, confirmed via GTAMods wiki. Any ID
# from 0-70 inclusive not present in this dict generates no
# background sound at all, per the wiki's own note - that's a real,
# documented "silent zone" case, not a gap in this table.
AUZO_TYPES = {
    0: ("drugged", None), 1: ("plain", None), 2: ("forest", None),
    3: ("city", None), 4: ("living room", "St Mark's violin music"),
    5: ("drugged", "Beach party bkgd song"), 6: ("living room", None),
    7: ("drugged", None), 8: ("hangar", "Unused loud hum"),
    9: ("drugged", None), 10: ("drugged", "Awards ceremony music"),
    11: ("drugged", None), 12: ("drugged", "Loud hum heard on ships"),
    13: ("drugged", "Low Rider Challenge bkgd song"), 14: ("living room", None),
    15: ("living room", "Static sound heard on military bases"),
    16: ("living room", None), 17: ("stone room", "Casino bkgd medley"),
    18: ("room", None), 19: ("hangar", "Quiet hum heard in Area 69"),
    20: ("hangar", "Fan-like clicking heard in Abattoir"),
    21: ("living room", "Quiet hum heard in 24-7s"), 22: ("room", None),
    23: ("hangar", "Loud hum heard in Dam interior"),
    24: ("living room", "Racing sounds heard in ITB lobby"),
    25: ("living room", "Quiet hum heard in Planning Dept"),
    26: ("living room", "Quiet hum heard in safe houses"), 27: ("room", None),
    28: ("room", "Dance Club bkgd medley"),
    29: ("stone room", "Dance Club bkgd medley"),
    30: ("living room", "Stream or User Tracks Player"), 31: ("drugged", None),
    32: ("living room", None), 33: ("stone room", None),
    34: ("room", "Pleasure Domes bkgd medley"), 35: ("living room", None),
    36: ("padded cell", "Loud hum heard in Jet interior"),
    37: ("room", "Muzak-type bkgd heard in unused diner interiors"),
    38: ("drugged", None), 39: ("room", "Quiet hum heard in police stations"),
    40: ("living room", None), 41: ("arena", "Stadium event bkgd medley"),
    42: ("living room", None), 43: ("living room", None),
    44: ("living room", "Fast Food Joint bkgd sounds"), 45: ("living room", None),
    46: ("living room", None), 47: ("living room", None),
    48: ("stone room", "Ammunation PA loop"), 49: ("room", None),
    50: ("hangar", "Quiet hum heard in warehouses"),
    51: ("drugged", "Very loud hum heard in cargo plane?"),
    52: ("living room", "Playback FM"), 53: ("living room", "K-ROSE"),
    54: ("living room", "KDST"), 55: ("living room", "Bounce FM"),
    56: ("living room", "SFUR"), 57: ("living room", "Radio Los Santos"),
    58: ("living room", "Radio X"), 59: ("living room", "CSR"),
    60: ("living room", "K-JAH West"), 61: ("living room", "MasterSounds"),
    62: ("living room", "WCTR"), 63: ("living room", None),
    64: ("living room", "Unused quiet hum"), 65: ("living room", None),
    66: ("room", "Strip Club background melody"),
    67: ("living room", "Unused background melody"),
}


@dataclass
class AuzoEntry: #vers 1
    """One "auzo" section entry - a San Andreas audio zone (Aug 20
    2026, per Keith: "Implement support for the remaining SA,
    audiozone placements with sound svg icons; play the sounds").
    Format confirmed against GTAMods wiki: two real shapes exist, told
    apart here by how many numeric fields follow the name/id/switch
    (is_sphere True when there are exactly 4 more - X,Y,Z,Radius -
    False when there are 6 - X1,Y1,Z1,X2,Y2,Z2, matching the two
    documented layouts exactly rather than guessing from field count
    alone). Cube fields (x2/y2/z2) are None for a sphere entry and
    vice versa (radius is None for a cube entry) - never populated
    with a meaningless default like 0.0 that could be mistaken for a
    real, deliberate zero-sized value.

    Real, honest limitation: sound_id only maps to a documented
    environment type and (sometimes) a music/ambience TRACK NAME via
    AUZO_TYPES - not an actual playable audio sample. The real SA
    audio itself lives inside the game's own compiled audio bank
    archives (a completely separate, unrelated binary format this app
    doesn't read at all), so there is no actual sound data anywhere
    in the loaded IPL/IDE data this could point to and play - "play
    the sounds" for now means a placeholder tone confirming which
    zone was clicked, not the real, in-game San Andreas audio."""
    name:       str
    sound_id:   int
    switch:     int
    is_sphere:  bool
    x1: float = 0.0
    y1: float = 0.0
    z1: float = 0.0
    x2: Optional[float] = None
    y2: Optional[float] = None
    z2: Optional[float] = None
    radius: Optional[float] = None
    source_ipl: str = ""
    line_no:    int = 0

    @property
    def environment_type(self): #vers 1
        info = AUZO_TYPES.get(self.sound_id)
        return info[0] if info else None

    @property
    def music_description(self): #vers 1
        info = AUZO_TYPES.get(self.sound_id)
        return info[1] if info else None


@dataclass
class WaterCorner: #vers 1
    """One corner point of a real SA water.dat shape (Aug 20 2026,
    per Keith: "lets get all the functions in" - Water/radar
    recalculation on map moves is the first of the 3 items on his own
    list, and this is the real prerequisite for the "water" half:
    water.dat wasn't parsed at all before this). Format confirmed
    against a detailed, community-verified GTAForums documentation
    thread (steve-m, 2005 - extensively tested and refined by many
    contributors over years, not a single unverified source): 7
    floats per corner - X, Y, Z (world position), then current_x/
    current_y (water current/flow speed along each axis), wave_
    unknown ("influences waves, maximum is 1.0" - the source itself
    documents this field's own exact effect as unconfirmed), and
    wave_height."""
    x: float
    y: float
    z: float
    current_x: float
    current_y: float
    wave_unknown: float
    wave_height: float


@dataclass
class WaterShape: #vers 1
    """One real water.dat shape entry - SA's own text format only
    (Aug 20 2026, see WaterCorner's own docstring for the fuller
    format-confirmation story). A real shape is EITHER a triangle (3
    corners) or a quad (4 corners) - documented and independently re-
    confirmed by testers: "the game only uses cubes, rectangles and
    triangles... at least one corner must be 90°" for a triangle.
    water_type is a real, confirmed 2-bit flag value (documented
    directly, not guessed): bit 0 = visible, bit 1 = shallow/pool
    (vs invisible/deep-ocean) - 0=invisible ocean, 1=visible ocean,
    2=invisible pool, 3=visible pool. Corner order matters for a real,
    game-crash-preventing reason documented directly - the source's
    own emphatic warning: "All X and Y coordinates of corner points
    must be even, rounded numbers... otherwise the game will crash
    when you approach the water" - this app reads coordinates as
    given, verbatim, rather than silently "fixing" them, since
    correctness here is the modder's own responsibility and this app
    altering values without being asked risks introducing exactly
    this crash."""
    corners:    List[WaterCorner]
    water_type: int
    source_file: str = ""
    line_no:    int = 0

    @property
    def is_visible(self): #vers 1
        return bool(self.water_type & 1)

    @property
    def is_shallow(self): #vers 1
        """True = a "pool" (6 units deep per the source's own
        documented depth), False = "ocean" (infinite depth)."""
        return bool(self.water_type & 2)


def parse_water_dat(path: str) -> List[WaterShape]: #vers 1
    """Parse a real SA water.dat file (Aug 20 2026 - see WaterShape's
    own docstring for the full format confirmation). Standalone
    module-level function, not tied to IPLParser/IDEParser - water.
    dat is neither an IPL nor IDE section, it's its own, separate top-
    level file referenced from gta.dat's own WATER directive,
    matching sa_path_parser.py's own established "standalone function
    for a standalone file format" convention rather than being force-
    fit into either of those classes.

    The file's own first non-blank, non-comment line must literally
    be the word "processed" (confirmed directly: "The water.dat file
    starts with the word 'processed' in the first line") - skipped,
    not treated as data. Every subsequent real line is whitespace-
    split and its own value count used to tell a triangle (3*7+1=22
    values) from a quad (4*7+1=29) - real lines that match neither
    count are skipped rather than guessed at. '#'-prefixed lines are
    real, documented comments (same convention as IPL/IDE files) and
    skipped, matching the source's own explicit confirmation of this."""
    shapes: List[WaterShape] = []
    try:
        with open(path, 'r', encoding='ascii', errors='ignore') as f:
            lines = f.readlines()
    except Exception:
        return shapes

    basename = os.path.basename(path)
    seen_processed = False
    for lineno, raw_line in enumerate(lines, start=1):
        line = raw_line.split('#', 1)[0].strip()
        if not line:
            continue
        if not seen_processed:
            if line.lower() == 'processed':
                seen_processed = True
            continue
        parts = line.split()
        if len(parts) == 22:
            n_corners = 3
        elif len(parts) == 29:
            n_corners = 4
        else:
            continue
        try:
            values = [float(p) for p in parts[:-1]]
            water_type = int(float(parts[-1]))
        except ValueError:
            continue
        corners = []
        for i in range(n_corners):
            off = i * 7
            corners.append(WaterCorner(
                x=values[off], y=values[off + 1], z=values[off + 2],
                current_x=values[off + 3], current_y=values[off + 4],
                wave_unknown=values[off + 5], wave_height=values[off + 6]))
        shapes.append(WaterShape(corners=corners, water_type=water_type,
                                 source_file=basename, line_no=lineno))
    return shapes


@dataclass
class WaterProLevel: #vers 2
    """One water level from a real GTA III/VC/PS2-LC/SOL waterpro.dat
    (Aug 20 2026 - the binary counterpart to SA's own text water.dat,
    a completely different game/format despite the similar purpose;
    see WaterShape's own docstring for that one). height is this
    level's own real water height ("recommended 0.0 for GTA III and
    6.0 for GTA Vice City" - a real, documented per-game default, not
    the same for both games despite sharing this one format).

    Real correction made to this class (Aug 20 2026, per Keith
    pointing at a real, existing, more carefully-researched reference
    tool: "look at water_workshop") - an earlier version of this same
    class also carried zone_start_x/y and zone_end_x/y fields,
    interpreting the header's own bytes 196-964 as 48 real {StartX,
    StartY,EndX,EndY} rectangles. That reference tool's own WaterproParser
    (apps/components/Water_Editor/water_workshop.py) - which explicitly
    cites a specific, named source ("WaterHack.cpp") and, critically,
    already correctly handles PS2/SOL variants with genuinely
    different, non-vanilla grid sizes, something the earlier version
    of this class never accounted for at all - treats that exact same
    byte range as opaque, unidentified data instead, preserved
    verbatim for round-tripping rather than decoded into a specific
    structure. Given a more careful, already-proven reference now
    directly contradicts the earlier zone-rectangle interpretation,
    the honest, correct move is to defer to it rather than keep
    presenting an unconfirmed guess as settled fact - see WaterProFile's
    own unk_block field for where those bytes now live instead."""
    height: float


@dataclass
class WaterProFile: #vers 2
    """A fully parsed GTA III/VC/PS2-LC/SOL waterpro.dat (Aug 20 2026,
    see WaterProLevel's own docstring for the real correction made to
    this class and why). levels: up to 48 real WaterProLevel entries
    (only the first `level_count` are meaningful - the file always
    reserves space for the full 48, unlike a variable-length list).
    unk_block: the real, raw 768 bytes at header offset 196-964 -
    genuinely unidentified data (not zone rectangles, an earlier,
    now-corrected guess - see WaterProLevel's own docstring), kept
    verbatim so round-tripping this file (load then save unmodified)
    reproduces it exactly rather than silently discarding or
    reinventing bytes this app doesn't actually understand.

    visible_map: the real grid the game actually shows on the in-game
    radar/minimap, physical_map: the real, separate grid (exactly
    double visible_map's own width/height per side) that actually
    determines where the player can swim/the water is physically
    present - these are genuinely two different real grids at two
    different resolutions, not the same data duplicated. grid_width
    is visible_map's own real width/height (64 for vanilla SA/VC/III -
    physical_map is always exactly double this, both dimensions - but
    genuinely different for a PS2/SOL or custom/expanded map, per
    water_workshop.py's own already-proven variable-grid-size
    handling, not assumed fixed the way an earlier version of this
    class incorrectly did). Each grid cell's own byte value is an
    index into `levels` (which real level's own height applies at
    that grid cell)."""
    level_count:  int
    levels:       List[WaterProLevel]
    grid_width:   int
    unk_block:    bytes
    visible_map:  List[List[int]]   # [row][col], grid_width x grid_width
    physical_map: List[List[int]]   # [row][col], (2*grid_width) x (2*grid_width)
    source_file:  str = ""


def parse_waterpro_dat(path: str) -> Optional[WaterProFile]: #vers 2
    """Parse a real GTA III/VC/PS2-LC/SOL waterpro.dat (Aug 20 2026 -
    see WaterProLevel's own docstring for the real correction made to
    this function and why). Standalone module-level function, not
    tied to any parser class - matches parse_water_dat's own
    established "standalone function for a standalone file format"
    convention just above.

    Real, confirmed 964-byte header (int32 level count + 48 real
    float32 heights + a real, deliberately-unidentified 768-byte
    block - see WaterProLevel/WaterProFile's own docstrings for why
    this isn't decoded into zone rectangles any more), followed by a
    variable-size grid pair whose own real dimensions are derived
    from the file's own remaining size after that header, matching
    water_workshop.py's own already-proven approach exactly rather
    than assuming a fixed vanilla-only 64x64/128x128 size: remaining
    bytes = visible_map (grid_width^2 cells) + physical_map
    ((2*grid_width)^2 cells) = 5*grid_width^2 total, so grid_width =
    sqrt(remaining/5) - checked to be a real, exact perfect square
    (a non-square result means a genuinely corrupt/foreign file, not
    silently truncated data).

    Real, honest limitation, not glossed over (Aug 20 2026, per Keith:
    "water_workshop doesn't handle SOL correctly"): this function's
    grid math and cell layout matches water_workshop.py's own
    WaterproParser.load() exactly - deliberately, since that's the
    more carefully-researched reference this was corrected against
    (see WaterProLevel's own docstring). But that same reference
    tool's own SOL comments (its own `_rebuild_cache`, describing "SOL:
    6x6 map tiles, each tile is TILE_W x TILE_W cells stored
    sequentially") describe SOL's real grid data as internally
    subdivided into 6x6 tiles, each stored as its own contiguous
    block - a genuinely different physical byte layout from a simple,
    flat, row-major grid across the whole map. That de-tiling logic
    only actually exists in that reference tool's own DISPLAY code
    (for rendering the grid visually), not its own WaterproParser.load()
    itself - and since this function's own visible_map/physical_map
    construction was modelled directly on that same load() method,
    this function inherits the identical gap: for a real SOL file,
    visible_map/physical_map here will contain the correct raw bytes
    in the wrong logical (row, col) positions, not de-tiled into
    genuine world-space grid order the way a vanilla SA/VC/III file's
    own (non-tiled) grid data already correctly is. Vanilla SA/VC/III
    files are unaffected by this - their own real grid data was never
    tiled into 6x6 blocks in the first place. Flagged here rather
    than silently claimed working, since neither this function nor
    its own reference has real, confirmed SOL de-tiling logic yet."""
    try:
        with open(path, 'rb') as f:
            data = f.read()
    except Exception:
        return None
    header_size = 964
    remaining = len(data) - header_size
    if remaining <= 0 or remaining % 5 != 0:
        return None
    grid_width = math.isqrt(remaining // 5)
    if grid_width * grid_width != remaining // 5:
        return None
    try:
        level_count = data[0]
        heights = struct.unpack_from('<48f', data, 4)
        levels = [WaterProLevel(height=h) for h in heights]
        unk_block = data[196:964]
        vis_size = grid_width * grid_width
        phys_width = grid_width * 2
        phys_size = phys_width * phys_width
        visible_bytes = data[header_size:header_size + vis_size]
        visible_map = [list(visible_bytes[r * grid_width:(r + 1) * grid_width])
                       for r in range(grid_width)]
        physical_bytes = data[header_size + vis_size:header_size + vis_size + phys_size]
        physical_map = [list(physical_bytes[r * phys_width:(r + 1) * phys_width])
                        for r in range(phys_width)]
    except (struct.error, IndexError):
        return None
    return WaterProFile(
        level_count=level_count, levels=levels, grid_width=grid_width,
        unk_block=unk_block, visible_map=visible_map, physical_map=physical_map,
        source_file=os.path.basename(path))


@dataclass
class RadarTile: #vers 2
    """One real radar/minimap tile's own world-space bounding box for
    a given GTA game (Aug 20 2026, per Keith: "look at radar editor
    for how the radar works", then "radar_workshop has the radar
    code" - following the earlier "map-to-radar generation" request,
    genuinely blocked before this until real confirmed grid numbers
    were found). Corresponds to one real "radarNN.txd" file the game
    actually loads.

    Real, confirmed facts this is built from, not guessed - checked
    directly against two independent real sources, one of them a
    local, already-existing, explicitly-marked "authoritative" tool:

    - `apps/components/Radar_Editor/radar_workshop.py`'s own real,
      already-working `GAME_PRESETS`/`_GAME_WORLD_BOUNDS` (a comment
      on the latter reads literally: "Grid constants (authoritative
      — do not change without verifying against game files)") - SA:
      144 tiles (12x12), world bounds -3000..3000 both axes; VC/III
      (and LCS/VCS too): 64 tiles (8x8), world bounds -2000..2000
      both axes; SOL: 1296 tiles (36x36), world bounds -6000..6000.
      Both SA (500 units/tile) and VC/III (4000/8=500 units/tile)
      land on the exact same 500-unit-per-tile figure independently.
    - The same real tile file naming range this class's own earlier
      version was already built from - "radar00.txd" through
      "radar143.txd" for SA (144 files) - directly matches this
      tool's own real SA preset ("radar00.txd to radar143.txd (144
      tiles, 12x12 grid)" - its own literal hint text).

    Tile-index ordering (row-major, index 0 at the map's own north-
    west corner) - an earlier version of this class carried this as
    an honest, unconfirmed guess. `radar_workshop.py`'s own real code
    states this directly and explicitly, not just implies it: "Tile
    grid origin is top-left = (world_min_x, world_max_y)" - the exact
    same convention this class already assumed, now genuinely
    confirmed rather than merely reasonable."""
    index: int
    row: int
    col: int
    min_x: float
    min_y: float
    max_x: float
    max_y: float


# Real, confirmed per-game radar grid presets (Aug 20 2026, sourced
# directly from radar_workshop.py's own real, already-working, "do
# not change without verifying against game files" GAME_PRESETS/
# _GAME_WORLD_BOUNDS - see RadarTile's own docstring for the fuller
# confirmation story). grid_size is the real, full world-unit span
# (world bounds are symmetric around the origin for every game this
# tool documents, so grid_size/2 gives min/max directly).
RADAR_GRID_PRESETS = {
    'gta3': {'grid_size': 4000.0, 'tiles_per_side': 8},
    'vc':   {'grid_size': 4000.0, 'tiles_per_side': 8},
    'sa':   {'grid_size': 6000.0, 'tiles_per_side': 12},
    'sol':  {'grid_size': 12000.0, 'tiles_per_side': 36},
}


def compute_radar_grid(grid_size: float = 6000.0, tiles_per_side: int = 12,
                       center_x: float = 0.0, center_y: float = 0.0) -> List[RadarTile]: #vers 2
    """Compute the real world-space bounding box for every tile in a
    radar grid (Aug 20 2026 - see RadarTile's own docstring for the
    full confirmation story, and RADAR_GRID_PRESETS for the real,
    confirmed per-game defaults to pass here). Defaults match vanilla
    SA (6000 units, 12x12, centred on the origin) purely for backward
    compatibility with this function's own first version - callers
    generating tiles for a specific game should pass RADAR_GRID_
    PRESETS[game_key]'s own values explicitly rather than relying on
    this default, since VC/GTA III use a genuinely different, smaller
    4000-unit/8x8 grid, not SA's.

    Tile 0 is the north-west corner (min X, max Y), row-major,
    increasing west-to-east then north-to-south - see RadarTile's own
    docstring for the real confirmation behind this (no longer an
    unconfirmed assumption)."""
    tile_size = grid_size / tiles_per_side
    half = grid_size / 2.0
    origin_x = center_x - half   # west edge
    origin_y = center_y + half   # north edge
    tiles = []
    index = 0
    for row in range(tiles_per_side):
        tile_max_y = origin_y - row * tile_size
        tile_min_y = tile_max_y - tile_size
        for col in range(tiles_per_side):
            tile_min_x = origin_x + col * tile_size
            tile_max_x = tile_min_x + tile_size
            tiles.append(RadarTile(
                index=index, row=row, col=col,
                min_x=tile_min_x, min_y=tile_min_y,
                max_x=tile_max_x, max_y=tile_max_y))
            index += 1
    return tiles


@dataclass
class ChaseFrame: #vers 1
    """One recorded frame from a real GTA III CHASE*.DAT file (Aug 19
    2026, per Keith's real sample - "lets do those next"). Format
    confirmed against real, published documentation (GTAMods wiki -
    "near identical to its successor, RRR, in San Andreas") AND
    cross-checked directly against Keith's own real CHASE0.DAT: a
    fixed 28-byte record, no header/count at all - the file's own
    size divided cleanly by 28 with zero remainder (151200 / 28 =
    5400.0 exactly), and real decoded positions land in a tight,
    plausible cluster of real GTA III world coordinates that change
    smoothly frame-to-frame, matching a recorded vehicle path for the
    introduction cutscene's chase scene (13 unique cars, one CHASE*.
    DAT file per car/path index) rather than a coincidental byte
    alignment.

    vel_x/y/z: INT16, divide by 16383.5 for the real float velocity.
    right_x/y/z, top_x/y/z: INT8, divide by 127.0 - the vehicle's own
    right and top orientation basis vectors for that frame (together
    with the implicit forward vector, these fully describe the
    vehicle's 3D rotation for that frame, the same way DFF frame
    matrices store an object's orientation as basis vectors rather
    than Euler angles or a quaternion).
    steering: INT8, divide by 20.0. gas/brake: INT8, divide by 100.0.
    handbrake: bool (1/0 in the file). pos_x/y/z: standard IEEE-754
    float, real world-space position for that frame - no scale
    factor, matching tracks.dat/flight.dat's own real-world-units
    convention rather than IPL/IDE path data's own /16-scaled
    convention."""
    vel_x: float = 0.0
    vel_y: float = 0.0
    vel_z: float = 0.0
    right_x: float = 0.0
    right_y: float = 0.0
    right_z: float = 0.0
    top_x: float = 0.0
    top_y: float = 0.0
    top_z: float = 0.0
    steering: float = 0.0
    gas: float = 0.0
    brake: float = 0.0
    handbrake: bool = False
    pos_x: float = 0.0
    pos_y: float = 0.0
    pos_z: float = 0.0
    source_file: str = ""
    frame_index: int = 0


@dataclass
class RoadblockEntry: #vers 1
    """One police roadblock placement from SA's real ROADBLOX.DAT
    (Aug 19 2026, per Keith's real sample - "lets do those next").
    Format confirmed against real, published documentation (GTAMods
    wiki) AND cross-checked directly against Keith's own real data:
    a 4-byte int32 count, followed by up to 325 fixed (area_id: int16,
    node_id: uint16) slots - only the first `count` are meaningful,
    matching the file's own exact 1304-byte size (4 + 325*4). Each
    entry references a real vehicle path node from the SA node system
    (see sa_nodes/SAPathFile) - a roadblock spawns AT that node's own
    position when the game decides to place one there. Verified this
    isn't just a plausible-looking format match: cross-referenced all
    325 real entries in Keith's own real ROADBLOX.DAT against his own
    real, complete NODES0-63.DAT set - every single one resolves to a
    real, valid vehicle node index within its own stated area, not
    just a format that happens to parse without error."""
    area_id: int = 0
    node_id: int = 0


@dataclass
class TrackWaypoint: #vers 1
    """One waypoint from a real GTA III/VC/SA train track file
    (data/paths/tracks.dat, tracks2.dat, etc - Aug 17 2026, per
    Keith: "then the other path .dat files you pointed out earlier",
    following on from real path node editing). Confirmed via direct
    inspection of Keith's own real tracks.dat/tracks2.dat: a plain
    text file, first line is a waypoint COUNT, then exactly that many
    "X Y Z" lines (space-separated floats), one waypoint per line, in
    real world-space coordinates - no scale factor involved, unlike
    IPL/IDE path data. Genuinely simpler than every other path format
    in this app - just an ordered list of points forming one
    continuous track, no node types, no Next-index graph, no per-
    game format variation. Not referenced anywhere in gta.dat/
    gta3.dat's own directive list - the game loads these from a
    fixed, well-known relative path (data/paths/) rather than a
    listed directive, confirmed by their absence from a real,
    complete gta3.dat.

    Also shared, unchanged, by flight.dat/flight2/3/4.dat (Aug 19
    2026, per Keith's real LC/VC/SA samples) and spath0.dat - all
    confirmed to be the exact same "count then X Y Z lines" shape,
    verified directly against real files rather than assumed from the
    tracks.dat naming alone.

    flag (Aug 19 2026, per Keith's real SA tracks.dat/tracks2/3/4.dat
    samples) - a 4th value some lines carry, previously silently
    dropped entirely rather than stored. Confirmed via direct
    inspection of all 4 real SA tracks files: always present when it
    appears (every SA tracks*.dat line actually has 4 values, not 3 -
    VC/GTA III's own tracks.dat/tracks2.dat samples only ever had 3,
    hence the format being understood as 3 originally), always 0 or
    1, and in the largest file (tracks.dat, 926 points) exactly 6
    points carry a 1 while every other point (and every point in the
    3 smaller files) carries 0 - a strong, plausible match for "this
    is a real station stop" given SA has 6 real train stations, but
    presented as a hypothesis rather than a confirmed fact - no
    published documentation of this specific field was found. None
    for files (like flight*.dat/spath0.dat) that only ever have 3
    values per line."""
    x: float
    y: float
    z: float
    source_file: str = ""
    index:       int = 0
    flag:        Optional[int] = None


@dataclass
class IPLLoadResult:
    """Result of one on-demand IPL load (GTAWorldLoader.load_ipl_by_
    name) - per Keith's request for per-IPL success/error reporting
    ("path/airport.ipl loaded - no errors" / "path/airportN.ipl loaded
    - 4 errors found, check log added to the maps folder"), rather
    than just a bare bool."""
    success:       bool = False
    abs_path:      str = ""
    instance_count: int = 0
    error_count:   int = 0
    warning_count: int = 0
    errors:        List[str] = field(default_factory=list)
    warnings:      List[str] = field(default_factory=list)


@dataclass
class ParseStats:
    total_lines:     int = 0
    ide_files:       int = 0
    ipl_files:       int = 0
    col_files:       int = 0
    img_files:       int = 0
    objects_loaded:  int = 0
    instances:       int = 0
    errors:          List[str] = field(default_factory=list)
    warnings:        List[str] = field(default_factory=list)


def _resolve_ci(base: str, rel_path: str) -> Optional[str]:
    """Case-insensitive path resolution from base directory.
    Walks each path component, matching case-insensitively.
    Returns the real absolute path if found, else None.
    Needed for SOL on Linux where sol/ vs SOL/ (case) appear in the same .dat file.
    """
    parts = rel_path.replace("\\", "/").split("/")
    current = base
    for part in parts:
        if not part:
            continue
        try:
            entries = os.listdir(current)
        except (PermissionError, NotADirectoryError, FileNotFoundError):
            return None
        part_lower = part.lower()
        match = next((e for e in entries if e.lower() == part_lower), None)
        if match is None:
            return None
        current = os.path.join(current, match)
    return current if os.path.isfile(current) else None


class DATParser: #vers 2
    """Parses a single GTA .dat file — handles COLFILE island index and strips inline comments."""

    def __init__(self, game: str = GTAGame.GTA3):
        self.game      = game
        self.game_root = ""
        self.dat_path  = ""
        self.entries:  List[DATEntry] = []
        self.stats     = ParseStats()

    def parse(self, dat_path: str, game_root: str = "") -> bool: #vers 2
        self.dat_path  = dat_path
        self.game_root = game_root or os.path.normpath(
            os.path.join(os.path.dirname(dat_path), ".."))
        self.entries.clear()
        self.stats = ParseStats()

        if not os.path.isfile(dat_path):
            self.stats.errors.append(f"DAT not found: {dat_path}")
            return False
        try:
            with open(dat_path, "r", encoding="ascii", errors="ignore") as f:
                lines = f.readlines()
        except Exception as e:
            self.stats.errors.append(f"Cannot read DAT {dat_path}: {e}")
            return False

        self.stats.total_lines = len(lines)
        dat_basename = os.path.basename(dat_path)

        for raw in lines:
            line = raw.split("#")[0].strip()
            if not line:
                continue
            parts = line.split()
            if len(parts) < 2:
                continue
            directive = parts[0].upper()

            if directive == "COLFILE":
                # COLFILE <island_int> <path>
                if len(parts) < 3:
                    continue
                island   = parts[1]
                raw_path = parts[2]
                abs_path = self._resolve(raw_path)
                self.entries.append(DATEntry(
                    directive=directive, path=raw_path, abs_path=abs_path,
                    exists=os.path.isfile(abs_path), extra=island,
                    source_dat=dat_basename))
                self.stats.col_files += 1
                continue

            if directive == "SPLASH":
                continue   # no file path

            raw_path = parts[1]
            abs_path = self._resolve(raw_path)
            self.entries.append(DATEntry(
                directive=directive, path=raw_path, abs_path=abs_path,
                exists=os.path.isfile(abs_path), source_dat=dat_basename))

            if directive == "IDE":
                self.stats.ide_files += 1
            elif directive == "IPL":
                self.stats.ipl_files += 1
            elif directive in ("IMG", "CDIMAGE"):
                self.stats.img_files += 1

        return True

    def _resolve(self, raw: str) -> str: #vers 3
        """Resolve a Windows-style relative path to an absolute path.
        Uses case-insensitive fallback for Linux (needed for SOL's mixed-case paths)."""
        norm = raw.strip().replace("\\", os.sep).replace("/", os.sep)
        if os.path.isabs(norm):
            return norm
        # Try game_root-relative first (most GTA paths are relative to install root)
        cand = os.path.normpath(os.path.join(self.game_root, norm))
        if os.path.isfile(cand):
            return cand
        # Try dat-file-relative
        cand2 = os.path.normpath(os.path.join(os.path.dirname(self.dat_path), norm))
        if os.path.isfile(cand2):
            return cand2
        # Case-insensitive fallback (Linux: sol/ vs SOL/ in same file)
        ci = _resolve_ci(self.game_root, norm)
        if ci:
            return ci
        return cand  # return game-root candidate even if not found

    def get_by_directive(self, d: str) -> List[DATEntry]:
        return [e for e in self.entries if e.directive == d.upper()]

    def ide_entries(self)  -> List[DATEntry]: return self.get_by_directive("IDE")
    def ipl_entries(self)  -> List[DATEntry]: return self.get_by_directive("IPL")
    def col_entries(self)  -> List[DATEntry]: return self.get_by_directive("COLFILE")
    def water_entries(self) -> List[DATEntry]: #vers 1
        """Real WATER directive entries (Aug 20 2026) - the generic
        "any directive not specifically matched above" branch in
        parse() already captures these correctly (WATER <path>, same
        shape as every other simple single-path directive), this is
        just a named accessor matching col_entries' own established
        pattern rather than callers needing to know the raw directive
        string themselves."""
        return self.get_by_directive("WATER")
    def img_entries(self)  -> List[DATEntry]:
        return self.get_by_directive("IMG") + self.get_by_directive("CDIMAGE")


class IDEParser: #vers 2
    """
    Parses a single GTA3 .ide file.

    GTA3 objs: id, model, txd, meshCount, dist1[, dist2], flags
    GTA3 peds: id, model, txd, pedType, behaviour, animGroup, carsDriveMask
    GTA3 cars: id, model, txd, type, handlingId, gameName, class, freq, level, compRules[, wheelId, wheelScale]
    GTA3 hier: id, model, txd
    """

    def __init__(self, game: str = GTAGame.GTA3):
        self.game    = game
        self.objects: List[IDEObject] = []
        # GTA III's own IDE-embedded path groups (Aug 16 2026, per
        # Keith: "gta3 game files need special treatment; the IPL
        # path data is stored within the .ide map files" and his real
        # comse.ide/comSE.ipl sample) - a completely separate list
        # from self.objects, since a path group isn't an IDEObject at
        # all (no txd/section/extra fields that make sense for it) -
        # see IDEPathGroup's own docstring for the full format story.
        self.ide_paths: List[IDEPathGroup] = []
        self.stats   = ParseStats()
        self._valid  = GTAGame.IDE_SECTIONS.get(game, GTAGame.IDE_SECTIONS[GTAGame.GTA3])

    def parse(self, ide_path: str) -> bool: #vers 3
        if not os.path.isfile(ide_path):
            self.stats.errors.append(f"IDE not found: {ide_path}")
            return False
        try:
            with open(ide_path, "r", encoding="ascii", errors="ignore") as f:
                lines = f.readlines()
        except Exception as e:
            self.stats.errors.append(f"Cannot read IDE {ide_path}: {e}")
            return False

        self.stats.total_lines = len(lines)
        current_section        = None
        current_ide_path_group = None   # Aug 16 2026 - "path" section state
        basename               = os.path.basename(ide_path)

        for lineno, raw in enumerate(lines, 1):
            line = raw.split("#")[0].strip()
            if not line or line.startswith("//"):
                continue
            low = line.lower()
            if low == "end":
                current_section = None
                current_ide_path_group = None
                continue
            if low in self._valid or (re.match(r'^[a-z0-9_]{2,8}$', low) and "," not in line):
                current_section = low
                current_ide_path_group = None
                continue
            if current_section is None:
                continue

            if current_section == "path":
                # A group header ("ped, 1440, scraperkb3_nit") is
                # never indented in the raw line; a node line always
                # is (same tab-indentation convention IPLParser's own
                # VC path handling uses, and the same reason it must
                # be checked against `raw`, not `line` - .strip()
                # above already erased any leading whitespace by this
                # point).
                if raw[:1] not in ('\t', ' '):
                    current_ide_path_group = self._parse_ide_path_group_header(line, basename, lineno)
                    if current_ide_path_group is not None:
                        self.ide_paths.append(current_ide_path_group)
                elif current_ide_path_group is not None:
                    node = self._parse_ide_path_node(line, lineno)
                    if node is not None:
                        current_ide_path_group.nodes.append(node)
                continue

            obj = self._parse_line(current_section, line, basename, lineno)
            if obj:
                self.objects.append(obj)
                self.stats.objects_loaded += 1

        return True

    def _parse_ide_path_group_header(self, line: str, source: str, lineno: int): #vers 1
        """Parse a GTA III IDE path group's header line - "GroupType,
        Id, ModelName" per Project Cerbera's own "PATH (IDE Section)"
        doc, confirmed against Keith's real comse.ide (both "ped" and
        "car" group types appear there)."""
        try:
            p = [x.strip() for x in line.split(",")]
            if len(p) < 3:
                return None
            return IDEPathGroup(
                group_type=p[0].lower(), model_id=int(p[1]), model_name=p[2],
                source_ide=source, line_no=lineno)
        except (ValueError, IndexError):
            return None

    def _parse_ide_path_node(self, line: str, lineno: int): #vers 1
        """Parse one GTA III IDE path node line - "NodeType, NextNode,
        IsCrossRoad, XRel, YRel, ZRel, Median, LeftLanes, RightLanes"
        (9 fields), per Project Cerbera's own doc, confirmed against
        Keith's real comse.ide field-for-field."""
        try:
            p = [x.strip() for x in line.split(",")]
            if len(p) < 6:
                return None
            return IDEPathNode(
                node_type=int(float(p[0])), next_id=int(float(p[1])),
                is_crossroad=int(float(p[2])),
                x_rel=float(p[3]), y_rel=float(p[4]), z_rel=float(p[5]),
                median=float(p[6]) if len(p) > 6 else 0.0,
                left=int(float(p[7])) if len(p) > 7 else 0,
                right=int(float(p[8])) if len(p) > 8 else 0)
        except (ValueError, IndexError):
            return None

    def _parse_line(self, section: str, line: str, source: str, lineno: int) -> Optional[IDEObject]: #vers 2
        try:
            parts = [p.strip() for p in line.split(",")]

            if section in ("objs", "tobj"):
                # Verified against Keith's real LAe.ide (Aug 1 2026,
                # per his own uploaded file): every single objs line
                # is exactly 5 fields, every tobj line exactly 7 -
                # id, model, txd, drawdist, flags[, time_on, time_off]
                # - NOT "id, model, txd, meshCount, dist1[, dist2],
                # flags" as this parser assumed until now. That
                # assumption meant every real draw distance (e.g.
                # "150" in "5390, laeskateparkLA, glenpark7_lae, 150,
                # 0") was being read as a bogus "150 meshes" mesh_
                # count, and the real flags value read as a bogus
                # draw_dist of 0 - exactly backwards, and exactly why
                # Keith's own "draw distance over 300 means LOD" idea
                # couldn't have worked against the previous parsing.
                # No confirmed real-world evidence of the multi-
                # distance-chain variant this previously assumed
                # (checked Keith's whole file: zero occurrences) -
                # kept as a defensive fallback below only for a field
                # count that doesn't match either direct pattern,
                # rather than removed outright, in case some other
                # game/file genuinely uses it.
                if len(parts) < 5:
                    return None
                model_id   = int(parts[0])
                model_name = parts[1]
                txd_name   = parts[2]
                extra: Dict[str, Any] = {}
                if section == "objs" and len(parts) == 5:
                    try: extra["draw_dist"] = float(parts[3])
                    except ValueError: pass
                    try: extra["flags"] = int(parts[4])
                    except ValueError: pass
                elif section == "tobj" and len(parts) == 7:
                    try: extra["draw_dist"] = float(parts[3])
                    except ValueError: pass
                    try: extra["flags"] = int(parts[4])
                    except ValueError: pass
                    try:
                        extra["time_on"]  = int(parts[5])
                        extra["time_off"] = int(parts[6])
                    except ValueError:
                        pass
                else:
                    # Defensive fallback for an unrecognized field
                    # count - best-effort mesh_count-chain guess,
                    # not verified against any real data.
                    try:
                        mesh_count = int(parts[3])
                    except ValueError:
                        mesh_count = 1
                    extra["mesh_count"] = mesh_count
                    dist_end = 4 + mesh_count
                    dists = []
                    for i in range(4, min(dist_end, len(parts))):
                        try: dists.append(float(parts[i]))
                        except ValueError: pass
                    if dists:
                        extra["draw_dist"] = dists[0]
                        if len(dists) > 1:
                            extra["draw_dist2"] = dists[1]
                    if dist_end < len(parts):
                        try: extra["flags"] = int(parts[dist_end])
                        except ValueError: pass
                    if section == "tobj" and len(parts) >= dist_end + 3:
                        try:
                            extra["time_on"]  = int(parts[dist_end + 1])
                            extra["time_off"] = int(parts[dist_end + 2])
                        except ValueError:
                            pass
                return IDEObject(model_id, model_name, txd_name,
                                 "object", section, extra, source, lineno)

            elif section == "cars":
                # GTA3: id, model, txd, type, handlingId, gameName, class, freq, level, compRules[, wheelId, wheelScale]
                # VC/SA: id, model, txd, type, handlingId, gameName, animFile, class, freq, level, compRules[, wheelId, wheelScale]
                # VC/SA add animFile between gameName and class — shift all subsequent fields by 1
                if len(parts) < 7:
                    return None
                model_id   = int(parts[0])
                model_name = parts[1]
                txd_name   = parts[2]
                extra = {
                    "veh_type":  parts[3],
                    "handling":  parts[4],
                    "game_name": parts[5],
                }
                if self.game in (GTAGame.VC, GTAGame.SA):
                    # parts[6] = animFile, parts[7] = class, parts[8] = freq, ...
                    extra["anim_file"] = parts[6] if len(parts) > 6 else ""
                    class_idx = 7
                else:
                    # GTA3: parts[6] = class, parts[7] = freq, ...
                    class_idx = 6
                if len(parts) > class_idx:
                    extra["veh_class"] = parts[class_idx]
                if len(parts) > class_idx + 1:
                    try: extra["freq"]  = int(parts[class_idx + 1])
                    except ValueError: pass
                if len(parts) > class_idx + 2:
                    try: extra["level"] = int(parts[class_idx + 2])
                    except ValueError: pass
                wheel_idx = class_idx + 4
                if len(parts) > wheel_idx:
                    try: extra["wheel_model"] = int(parts[wheel_idx])
                    except ValueError: pass
                if len(parts) > wheel_idx + 1:
                    try: extra["wheel_scale"] = float(parts[wheel_idx + 1])
                    except ValueError: pass
                return IDEObject(model_id, model_name, txd_name,
                                 "vehicle", section, extra, source, lineno)

            elif section in ("peds", "ped"):
                # GTA3: id, model, txd, pedType, behaviour, animGroup, carsDriveMask           (7 fields)
                # VC:   id, model, txd, pedType, behaviour, animGroup, carsDriveMask,
                #            animFile, radio1, radio2                                           (10 fields)
                if len(parts) < 7:
                    return None
                model_id   = int(parts[0])
                model_name = parts[1]
                txd_name   = parts[2]
                extra = {
                    "ped_type":        parts[3],
                    "behaviour":       parts[4],
                    "anim_group":      parts[5],
                    "cars_drive_mask": parts[6],
                }
                if self.game in (GTAGame.VC, GTAGame.SA) and len(parts) > 7:
                    extra["anim_file"] = parts[7]
                if self.game in (GTAGame.VC, GTAGame.SA) and len(parts) > 9:
                    try:
                        extra["radio1"] = int(parts[8])
                        extra["radio2"] = int(parts[9])
                    except ValueError:
                        pass
                return IDEObject(model_id, model_name, txd_name,
                                 "ped", section, extra, source, lineno)

            elif section == "weap":
                # GTA3: id, model, txd, meshCount, drawDist, flags             (6 fields)
                # VC/SA: id, model, txd, animFile, meshCount, drawDist, flags  (7 fields, adds animFile in slot 3)
                if len(parts) < 6:
                    return None
                model_id   = int(parts[0])
                model_name = parts[1]
                txd_name   = parts[2]
                extra: Dict[str, Any] = {}
                if self.game in (GTAGame.VC, GTAGame.SA):
                    # slot 3 = animFile, slot 4 = meshCount, slot 5 = drawDist, slot 6 = flags
                    extra["anim_file"] = parts[3]
                    try: extra["mesh_count"] = int(parts[4])
                    except (ValueError, IndexError): pass
                    try: extra["draw_dist"]  = float(parts[5])
                    except (ValueError, IndexError): pass
                    if len(parts) > 6:
                        try: extra["flags"] = int(parts[6])
                        except ValueError: pass
                else:
                    # GTA3: slot 3 = meshCount, slot 4 = drawDist, slot 5 = flags
                    try: extra["mesh_count"] = int(parts[3])
                    except (ValueError, IndexError): pass
                    try: extra["draw_dist"]  = float(parts[4])
                    except (ValueError, IndexError): pass
                    if len(parts) > 5:
                        try: extra["flags"] = int(parts[5])
                        except ValueError: pass
                return IDEObject(model_id, model_name, txd_name,
                                 "weapon", section, extra, source, lineno)

            elif section == "hier":
                # HIER's real, published format (Aug 20 2026,
                # confirmed via GTAMods) is universal across EVERY
                # game - always exactly 3 fields (Id, ModelName,
                # TxdName), no SA-specific extras at all. The
                # previous version of this branch's own comment
                # claimed "SA hier: id, model, txd, animFile,
                # drawDist (5 fields)" and read those extra fields
                # for SA - that was a real, mistaken conflation with
                # ANIM's own, genuinely separate SA-specific format
                # (which DOES have those extra fields, plus a 6th
                # Flags field this same conflation was also missing
                # entirely - see the dedicated "anim" branch just
                # below). A real hier line only ever has 3 fields to
                # begin with, so this mistake never actually crashed
                # anything (the old code's own `if len(parts) > 3`
                # guard just never fired against real data) - but it
                # was still wrong, not just imprecise.
                if len(parts) < 3:
                    return None
                return IDEObject(int(parts[0]), parts[1], parts[2],
                                 "hierarchy", section, {}, source, lineno)

            elif section == "tanm":
                # TANM is GTA IV-only (added there specifically for
                # time-controlled animated objects, per GTAMods'
                # own ANIM page) - this app doesn't support GTA IV at
                # all (GTAGame only has GTA3/VC/SA/SOL), so a real
                # "tanm" section keyword should never actually appear
                # in any file this app loads. Kept as its own no-op-
                # shaped branch (falls through to returning None via
                # the same length guard hier now uses) rather than
                # silently grouped with hier/anim's own real, different
                # formats - harmless either way in practice, but
                # honest about not actually understanding this
                # section's own real shape rather than quietly
                # guessing at it using anim's or hier's own fields.
                return None

            elif section == "anim":
                # ANIM's real, published SA format (Aug 20 2026,
                # confirmed via GTAMods, and directly verified against
                # two of Keith's own real IDE samples - "10744,
                # BS_building_SFS, bs_sfs, SFs, 130, 128" and "14642,
                # mafcas_spiral_dad, mafcasspiral, int_veg, 100, 0",
                # both matching field-for-field): Id, ModelName,
                # TxdName, AnimationName, DrawDistance, Flags - a real
                # 6 fields, genuinely different from hier's own always-
                # 3-field shape despite superficially similar-looking
                # first 3 fields. The previous version of this code
                # read up to DrawDistance (field 5 of 6) but never
                # Flags (field 6) at all - silently dropping it every
                # single time an anim line was parsed, for every real
                # anim entry in every real SA IDE file.
                if len(parts) < 3:
                    return None
                model_id   = int(parts[0])
                model_name = parts[1]
                txd_name   = parts[2]
                extra: Dict[str, Any] = {}
                if len(parts) > 3:
                    extra["anim_file"] = parts[3]
                if len(parts) > 4:
                    try: extra["draw_dist"] = float(parts[4])
                    except ValueError: pass
                if len(parts) > 5:
                    try: extra["flags"] = int(parts[5])
                    except ValueError: pass
                return IDEObject(model_id, model_name, txd_name,
                                 "hierarchy", section, extra, source, lineno)

            elif section == "txdp":
                if len(parts) >= 2:
                    return IDEObject(0, parts[0], parts[1],
                                     "txdparent", section, {}, source, lineno)

            elif section == "2dfx":
                # id, offsetX, offsetY, offsetZ, r, g, b, a, effectType[, type-specific fields...]
                # (Aug 1 2026, per Keith: "lets add the 2dfx support
                # next, showing 2dfx lighting at night") - previously
                # a placeholder stub with completely empty extra={},
                # no offset/color/type data parsed at all, which
                # can't support rendering an actual light. Based on
                # community-documented format (GTAMods wiki-style,
                # not verified against official documentation or any
                # real sample data - unlike the rotation/LOD fixes
                # earlier this session, which had Keith's actual raw
                # IPL lines to check against): effectType 0 = light is
                # the one this parses fully (offset, RGBA color, plus
                # best-effort corona_far_clip/point_light_range/
                # corona_size where present - lower confidence on the
                # exact field order/count for these SA-specific extras
                # beyond the core offset+color+type, since no real
                # sample data was available to verify against). Other
                # effect types (1=particle, 2=text/ped attractor,
                # 3=sun glare/enter-exit, 4=roadsign, 5=trigger point,
                # 6=cover point, 7=escalator, ...) aren't parsed beyond
                # their own offset/type - not needed for lighting.
                if len(parts) < 9:
                    return None
                model_id = int(parts[0])
                extra: Dict[str, Any] = {}
                try:
                    extra["offset_x"] = float(parts[1])
                    extra["offset_y"] = float(parts[2])
                    extra["offset_z"] = float(parts[3])
                    extra["color_r"] = int(parts[4])
                    extra["color_g"] = int(parts[5])
                    extra["color_b"] = int(parts[6])
                    extra["color_a"] = int(parts[7])
                    extra["effect_type"] = int(parts[8])
                except ValueError:
                    pass
                if extra.get("effect_type") == 0:
                    try:
                        if len(parts) > 9:  extra["corona_far_clip"]    = float(parts[9])
                        if len(parts) > 10: extra["point_light_range"] = float(parts[10])
                        if len(parts) > 11: extra["corona_size"]       = float(parts[11])
                    except ValueError:
                        pass
                return IDEObject(model_id, f"2dfx_{model_id}", "",
                                 "2dfx", section, extra, source, lineno)

        except (ValueError, IndexError):
            pass
        return None


def detect_ipl_format(data: bytes) -> str: #vers 1
    """Detect whether raw IPL bytes (read from disk, or extracted from
    an IMG archive like gta3.img) are plain-text or binary format.
    Binary IPL is used in some GTA SA ports (packed inside IMG archives
    for faster loading, rather than loose text files) - this doesn't
    require knowing the exact binary struct layout, just distinguishing
    'this looks like readable text' from 'this looks like packed
    binary data', which is enough to at least flag binary IPLs
    correctly rather than silently mis-parsing or crashing on them."""
    if not data:
        return 'text'
    head = data[:64]
    try:
        text = head.decode('ascii')
        if all(32 <= ord(c) < 127 or c in '\r\n\t' for c in text):
            return 'text'
    except UnicodeDecodeError:
        pass
    return 'binary'


class BinaryIPLParser: #vers 2
    """Parser for binary-format IPL data (see detect_ipl_format).

    Verified empirically against two real sample files Keith provided
    (crack.ipl: 60 instances, countn2_stream1.ipl: 355 instances) -
    not from official documentation, but cross-checked several
    independent ways: quaternion magnitude is exactly 1.0 for every
    single instance across both files (415 total, not a coincidence);
    world positions cluster in plausible SA coordinate ranges; model
    IDs fall within SA's valid ID range; and the header's own internal
    fields correctly predict the actual computed offset where instance
    data ends (76 + inst_count*40) in both files independently.

    Confirmed structure:
    - Magic: b"bnry" (4 bytes)
    - Header: 18 x int32 LE (72 bytes) immediately after the magic -
      total header is 76 bytes. Only two fields' meaning is confirmed:
      index 0 = inst_count, index 6 = 76 (constant - the header size/
      offset where inst data begins). The other 16 header fields are
      presumably counts/offsets for other sections (cull, zone, etc,
      per the text-format IPL_SECTIONS list) but which index maps to
      which section, and their exact record formats, are NOT yet
      confirmed - only the inst section is parsed here.
    - Each inst record is 40 bytes, starting right after the header:
      7x float32 LE (pos_x, pos_y, pos_z, rot_x, rot_y, rot_z, rot_w),
      then 3x int32 LE (model_id, a second field, lod_index). The
      second field is NOT interior (an earlier guess) - its observed
      values are almost all exact powers of 2 (0/256/512/1024) with
      one outlier (18), strongly suggesting a per-instance flags
      bitmask rather than an interior number; exposed as-is without
      inventing bit meanings that aren't confirmed.

    Cull/zone/other sections are NOT parsed yet - the header fields
    that likely locate them haven't been confirmed the way inst_count/
    inst_offset have. Write-back is not implemented at all yet -
    round-tripping needs the read side proven reliable first."""

    _MAGIC = b"bnry"
    _HEADER_SIZE = 76
    _INST_STRIDE = 40

    def __init__(self, game: str = GTAGame.SA):
        self.game = game
        self.instances: List[IPLInstance] = []
        self.zones: List[Dict] = []
        self.culls: List[Dict] = []
        self.stats = ParseStats()

    def parse(self, data: bytes, source_name: str = "") -> bool: #vers 2
        if len(data) < self._HEADER_SIZE or data[:4] != self._MAGIC:
            self.stats.errors.append(
                f"Not a recognised binary IPL ({source_name or 'unnamed'})")
            return False
        try:
            inst_count = struct.unpack_from('<i', data, 4)[0]
        except struct.error:
            self.stats.errors.append(
                f"Binary IPL header too short ({source_name or 'unnamed'})")
            return False

        needed = self._HEADER_SIZE + inst_count * self._INST_STRIDE
        if inst_count < 0 or needed > len(data):
            self.stats.errors.append(
                f"Binary IPL inst_count ({inst_count}) doesn't fit the "
                f"file size ({source_name or 'unnamed'})")
            return False

        for i in range(inst_count):
            rec_off = self._HEADER_SIZE + i * self._INST_STRIDE
            try:
                px, py, pz, rx, ry, rz, rw = struct.unpack_from('<7f', data, rec_off)
                model_id, _flags, lod = struct.unpack_from('<3i', data, rec_off + 28)
            except struct.error:
                self.stats.warnings.append(
                    f"Skipped truncated inst record {i} ({source_name})")
                continue
            self.instances.append(IPLInstance(
                model_id=model_id, model_name="", interior=0,
                pos_x=px, pos_y=py, pos_z=pz,
                rot_x=rx, rot_y=ry, rot_z=rz, rot_w=rw,
                lod_index=lod, source_ipl=source_name, line_no=i))
        self.stats.instances = len(self.instances)
        # Cull/zone/other sections not parsed yet - see class docstring.
        self.stats.warnings.append(
            f"Binary IPL ({source_name or 'unnamed'}): parsed {len(self.instances)} "
            f"inst entries; cull/zone/other sections not yet supported.")
        return True


def write_binary_ipl_inst_only(instances: List['IPLInstance']) -> bytes: #vers 1
    """Write instances out as binary-format IPL data - inst section
    only (Aug 20 2026, per Keith's own TODO comment: "When working
    with SA files, have the ability to click on a text ipl, convert
    to binary.ipl"). The write-side counterpart to BinaryIPLParser,
    built directly from that class's own confirmed structure - same
    magic, header size, and 40-byte inst record layout.

    REAL, IMPORTANT LIMITATION, stated here as plainly as in the UI
    itself: only 2 of the binary header's 18 int32 fields are actually
    confirmed (index 0 = inst_count, index 6 = the constant 76 - see
    BinaryIPLParser's own docstring for the full reasoning behind
    those two and why the other 16 aren't). This writer sets every
    other header field to 0 - the most conservative, least-assuming
    choice available, not a confirmed-correct value, because what
    those 16 fields actually mean (very possibly counts/offsets for
    other sections a real game-shipped file might expect to find,
    per the published "binary IPL supports inst and cars" scope) has
    never been determined. A per-instance record's own second int32
    field (documented on BinaryIPLParser as "not interior... a per-
    instance flags bitmask" with unconfirmed bit meanings) is also
    written as 0 here for the same reason. This has been verified by
    round-tripping synthetic data back through BinaryIPLParser itself
    and confirming an exact match (see the direct test run before this
    was trusted) - but that only proves the OUTPUT'S OWN INST SECTION
    is byte-correct and self-consistent, not that a real, unmodified
    game would accept the whole file without incident; no real binary
    IPL sample exists in this environment to test that against, and
    the file has never been tested in an actual running game.

    cars is not written at all - not parsed by this app on the read
    side either, so there's nothing here to convert it from; every
    other IPL section (cull/zone/path/occl/grge/enex) genuinely has
    no binary-IPL representation at all per the format's own real,
    published scope (confirmed via research before this was built,
    not assumed) - inst is the only thing a binary IPL can hold that
    this app's own text-IPL data also has."""
    header = bytearray(76)
    header[0:4] = b'bnry'
    struct.pack_into('<i', header, 4, len(instances))
    struct.pack_into('<i', header, 4 + 6 * 4, 76)
    # Every other header int32 (indices 1-5, 7-17) stays 0 - see this
    # function's own docstring for why that's a deliberate, honest
    # default rather than a confirmed-correct value.

    body = bytearray(len(instances) * 40)
    for i, inst in enumerate(instances):
        off = i * 40
        struct.pack_into('<7f', body, off,
                         inst.pos_x, inst.pos_y, inst.pos_z,
                         inst.rot_x, inst.rot_y, inst.rot_z, inst.rot_w)
        struct.pack_into('<3i', body, off + 28,
                         inst.model_id, 0, inst.lod_index)
    return bytes(header) + bytes(body)


class IPLParser: #vers 2
    """
    Parses a single GTA3/VC/SA .ipl file.
    GTA3 inst: id, model, px, py, pz, sx, sy, sz, rx, ry, rz, rw  (12 fields)
    SA   inst: id, model, interior, px, py, pz, rx, ry, rz, rw[, lod]
    """

    def __init__(self, game: str = GTAGame.GTA3):
        self.game       = game
        self.instances: List[IPLInstance] = []
        self.zones:     List[Dict]        = []
        self.culls:     List[CullEntry]   = []
        self.paths:     List[PathGroup]   = []
        self.grges:     List[GrgeEntry]   = []
        self.enexes:    List[EnexEntry]   = []
        self.occls:     List[OcclEntry]   = []
        self.auzos:     List[AuzoEntry]   = []
        self.stats      = ParseStats()
        self._valid     = GTAGame.IPL_SECTIONS.get(game, GTAGame.IPL_SECTIONS[GTAGame.GTA3])

    def parse(self, ipl_path: str) -> bool: #vers 2
        if not os.path.isfile(ipl_path):
            self.stats.errors.append(f"IPL not found: {ipl_path}")
            return False
        try:
            with open(ipl_path, "r", encoding="ascii", errors="ignore") as f:
                lines = f.readlines()
        except Exception as e:
            self.stats.errors.append(f"Cannot read IPL {ipl_path}: {e}")
            return False

        self.stats.total_lines = len(lines)
        current_section        = None
        basename               = os.path.basename(ipl_path)
        current_path_group     = None   # Aug 1 2026, "path" section state

        for lineno, raw in enumerate(lines, 1):
            line = raw.split("#")[0].strip()
            if not line or line.startswith("//"):
                continue
            low = line.lower()
            if low == "end":
                current_section = None
                current_path_group = None
                continue
            if low in self._valid or (re.match(r'^[a-z0-9_]{2,8}$', low) and "," not in line):
                current_section = low
                current_path_group = None
                continue
            if current_section is None:
                continue

            if current_section == "inst":
                obj = self._parse_inst(line, basename, lineno)
                if obj:
                    self.instances.append(obj)
                    self.stats.instances += 1
            elif current_section == "zone":
                z = self._parse_zone(line, basename, lineno)
                if z:
                    self.zones.append(z)
            elif current_section == "cull":
                c = self._parse_cull(line, basename, lineno)
                if c:
                    self.culls.append(c)
            elif current_section == "grge":
                g = self._parse_grge(line, basename, lineno)
                if g is not None:
                    self.grges.append(g)
            elif current_section == "enex":
                e = self._parse_enex(line, basename, lineno)
                if e is not None:
                    self.enexes.append(e)
            elif current_section == "occl":
                o = self._parse_occl(line, basename, lineno)
                if o is not None:
                    self.occls.append(o)
            elif current_section == "auzo":
                a = self._parse_auzo(line, basename, lineno)
                if a is not None:
                    self.auzos.append(a)
            elif current_section == "path":
                # A raw (pre-.strip()) leading tab or space marks a
                # sub-node line belonging to the current group; its
                # absence marks a new group's own header line -
                # exactly the distinction that .split("#")[0].strip()
                # above already erased, so check the original raw
                # text directly rather than the stripped `line`.
                indented = raw[:1] in ('\t', ' ')
                if not indented:
                    current_path_group = self._parse_path_group_header(line, basename, lineno)
                    if current_path_group is not None:
                        self.paths.append(current_path_group)
                elif current_path_group is not None:
                    node = self._parse_path_node(line, lineno)
                    if node is not None:
                        current_path_group.nodes.append(node)
        return True

    def _parse_path_group_header(self, line: str, source: str, lineno: int): #vers 1
        """A path group's own header line - two comma-separated
        integers (Aug 1 2026, per Keith's real uploaded paths.ipl:
        "1, -1" / "0, -1" etc.) preceding up to 12 tab-indented
        PathNode lines."""
        try:
            parts = [p.strip() for p in line.split(",")]
            if len(parts) < 2:
                self.stats.warnings.append(f"path group line {lineno}: expected 2 fields, got {len(parts)}")
                return None
            return PathGroup(
                header_a=int(float(parts[0])), header_b=int(float(parts[1])),
                source_ipl=source, line_no=lineno)
        except (ValueError, IndexError) as e:
            self.stats.warnings.append(f"path group line {lineno}: {e}")
            return None

    def _parse_path_node(self, line: str, lineno: int): #vers 1
        """One sub-node line within a path group - twelve fields per
        Project Cerbera's VC path documentation: Type, Next, 0, X, Y,
        Z, Median, Left, Right, Flag1, Flag2, Flag3.

        X/Y/Z scale conversion (Aug 1 2026, per Keith: "the path
        coords arent the same scale as the IPL data, this needs to be
        worked up") - confirmed against his real uploaded paths.ipl:
        raw coordinates like (-13866.1, -10439.2) are roughly 16x too
        large to be standard world units (VC's map is roughly -2000 to
        +2000), and Project Cerbera's own VC path documentation states
        these are stored in "precision units, which are sixteen times
        smaller than standard units" - dividing by 16 brings them to
        (-866.6, -652.5), squarely within VC's normal world bounds.
        Applied here so a PathNode's x/y/z land in the same coordinate
        space as everything else (inst positions, etc.), not the
        file's own internal, differently-scaled units."""
        try:
            parts = [p.strip() for p in line.split(",")]
            if len(parts) < 6:
                self.stats.warnings.append(f"path node line {lineno}: expected >=6 fields, got {len(parts)}")
                return None
            node_type = int(float(parts[0]))
            next_id   = int(float(parts[1]))
            # parts[2] is the documented-always-zero, unused field
            x = float(parts[3]) / 16.0
            y = float(parts[4]) / 16.0
            z = float(parts[5]) / 16.0
            median = float(parts[6]) if len(parts) > 6 else 0.0
            left   = int(float(parts[7])) if len(parts) > 7 else 0
            right  = int(float(parts[8])) if len(parts) > 8 else 0
            flag1  = int(float(parts[9]))  if len(parts) > 9  else 0
            flag2  = int(float(parts[10])) if len(parts) > 10 else 0
            flag3  = int(float(parts[11])) if len(parts) > 11 else 0
            return PathNode(node_type=node_type, next_id=next_id, x=x, y=y, z=z,
                             median=median, left=left, right=right,
                             flag1=flag1, flag2=flag2, flag3=flag3)
        except (ValueError, IndexError) as e:
            self.stats.warnings.append(f"path node line {lineno}: {e}")
            return None

    def _parse_grge(self, line: str, source: str, lineno: int): #vers 1
        """One SA "grge" (garage) line - eleven fields, per Keith's
        real example data and confirmed SannyBuilder forum
        documentation: X1,Y1,Z1, frontX,frontY, X2,Y2,Z2, DoorType,
        GarageType, Name."""
        try:
            parts = [p.strip() for p in line.split(",")]
            if len(parts) < 11:
                self.stats.warnings.append(f"grge line {lineno}: expected 11 fields, got {len(parts)}")
                return None
            return GrgeEntry(
                x1=float(parts[0]), y1=float(parts[1]), z1=float(parts[2]),
                front_x=float(parts[3]), front_y=float(parts[4]),
                x2=float(parts[5]), y2=float(parts[6]), z2=float(parts[7]),
                door_type=int(float(parts[8])), garage_type=int(float(parts[9])),
                name=parts[10].strip('"'), source_ipl=source, line_no=lineno)
        except (ValueError, IndexError) as e:
            self.stats.warnings.append(f"grge line {lineno}: {e}")
            return None

    def _parse_enex(self, line: str, source: str, lineno: int): #vers 1
        """One SA "enex" (entrance/exit) line - eighteen fields, per
        Keith's real example data and confirmed Grand Theft Wiki
        documentation: X1,Y1,Z1, EnterAngle, SizeX,SizeY,SizeZ,
        X2,Y2,Z2, ExitAngle, TargetInterior, Flags, Name, Sky,
        NumPedsToSpawn, TimeOn, TimeOff. Name arrives as a literal
        quoted string (e.g. "BAR2") - quotes stripped for storage."""
        try:
            parts = [p.strip() for p in line.split(",")]
            if len(parts) < 18:
                self.stats.warnings.append(f"enex line {lineno}: expected 18 fields, got {len(parts)}")
                return None
            return EnexEntry(
                enter_x=float(parts[0]), enter_y=float(parts[1]), enter_z=float(parts[2]),
                enter_angle=float(parts[3]),
                size_x=float(parts[4]), size_y=float(parts[5]), size_z=float(parts[6]),
                exit_x=float(parts[7]), exit_y=float(parts[8]), exit_z=float(parts[9]),
                exit_angle=float(parts[10]),
                target_interior=int(float(parts[11])), flags=int(float(parts[12])),
                name=parts[13].strip('"'),
                sky=int(float(parts[14])), num_peds_to_spawn=int(float(parts[15])),
                time_on=int(float(parts[16])), time_off=int(float(parts[17])),
                source_ipl=source, line_no=lineno)
        except (ValueError, IndexError) as e:
            self.stats.warnings.append(f"enex line {lineno}: {e}")
            return None

    def _parse_inst(self, line: str, source: str, lineno: int) -> Optional[IPLInstance]: #vers 2
        try:
            parts = [p.strip() for p in line.split(",")]
            if self.game in (GTAGame.SA, GTAGame.SOL):
                if len(parts) < 10:
                    return None
                return IPLInstance(
                    model_id=int(parts[0]), model_name=parts[1], interior=int(parts[2]),
                    pos_x=float(parts[3]), pos_y=float(parts[4]), pos_z=float(parts[5]),
                    rot_x=float(parts[6]), rot_y=float(parts[7]),
                    rot_z=float(parts[8]), rot_w=float(parts[9]),
                    lod_index=int(parts[10]) if len(parts) > 10 else -1,
                    source_ipl=source, line_no=lineno)
            elif self.game == GTAGame.VC:
                # VC: id, model, interior, px,py,pz, sx,sy,sz, rx,ry,rz,rw -
                # confirmed empirically (not guessed) against a real line
                # Keith provided: 429, mlamppost, 0, -686.7186279,
                # 593.7156982, 14.58199501, 1, 1, 1, 0, 0, -0.999048233,
                # 0.0436193347 - the last 4 values form a valid unit
                # quaternion (magnitude^2 = 1.0000000182), and MooMapper's
                # own Item Editor labels the field at index 2 "Interior"
                # for a real islandsf.ipl instance. This was previously
                # folded into the same branch as GTA3 with NO interior/
                # scale fields at all, silently reading the interior
                # value as pos_x for every VC instance.
                if len(parts) < 13:
                    return None
                return IPLInstance(
                    model_id=int(parts[0]), model_name=parts[1], interior=int(parts[2]),
                    pos_x=float(parts[3]), pos_y=float(parts[4]), pos_z=float(parts[5]),
                    scale_x=float(parts[6]), scale_y=float(parts[7]), scale_z=float(parts[8]),
                    rot_x=float(parts[9]), rot_y=float(parts[10]),
                    rot_z=float(parts[11]), rot_w=float(parts[12]),
                    source_ipl=source, line_no=lineno)
            else:
                # GTA3: id, model, px, py, pz, sx, sy, sz, rx, ry, rz, rw -
                # NOT yet empirically verified the way VC now is (best-
                # effort recollection: GTA3's simpler/older format has no
                # interior field, unlike VC's confirmed one) - needs its
                # own real sample line to confirm or correct, the same
                # way VC's format just got fixed. Don't assume this is
                # right just because VC turned out to need a similar fix.
                if len(parts) < 12:
                    return None
                return IPLInstance(
                    model_id=int(parts[0]), model_name=parts[1], interior=0,
                    pos_x=float(parts[2]), pos_y=float(parts[3]), pos_z=float(parts[4]),
                    scale_x=float(parts[5]), scale_y=float(parts[6]), scale_z=float(parts[7]),
                    rot_x=float(parts[8]), rot_y=float(parts[9]),
                    rot_z=float(parts[10]), rot_w=float(parts[11]),
                    source_ipl=source, line_no=lineno)
        except (ValueError, IndexError):
            self.stats.warnings.append(f"Skipped INST line {lineno}: {line[:70]}")
        return None

    def _parse_zone(self, line: str, source: str, lineno: int) -> Optional[Dict]: #vers 2
        """Parse one "zone" section line - Name, Type, MinX/Y/Z,
        MaxX/Y/Z, Island[, TextKey]. Now carries source_ipl/line_no
        (Aug 16 2026 fix, per Keith: "ive loaded zon files... but I
        cant see them in the viewpoint") - every other section type
        this parser handles (inst/cull/grge/enex/path) already tracks
        which file a parsed entry came from, for per-IPL visibility
        filtering; zone was the one exception, meaning zone boxes
        couldn't be shown/hidden per-file consistently with
        everything else even once viewport rendering existed."""
        try:
            p = [x.strip() for x in line.split(",")]
            if len(p) < 8:
                return None
            return {"name": p[0], "type": int(p[1]),
                    "min_x": float(p[2]), "min_y": float(p[3]), "min_z": float(p[4]),
                    "max_x": float(p[5]), "max_y": float(p[6]), "max_z": float(p[7]),
                    "island": int(p[8]) if len(p) > 8 else 0,
                    "text_key": p[9] if len(p) > 9 else "",
                    "source_ipl": source, "line_no": lineno}
        except (ValueError, IndexError):
            pass
        return None

    def _parse_cull(self, line: str, source: str, lineno: int) -> Optional[CullEntry]: #vers 2
        """Parse one "cull" section line - CenterX/Y/Z, X1/Y1/Z1,
        X2/Y2/Z2, Flags, WantedLevelDrop (11 fields). Fixed (Aug 16
        2026, per Keith's real cull.ipl upload) - the previous version
        expected only 7 fields (a center/width/height box), which
        never matched real data at all (every real line has 11
        fields, two genuine corner points, not a width+height pair) -
        confirmed against multiple independent wiki sources and
        verified field-for-field against Keith's real file."""
        try:
            p = [x.strip() for x in line.split(",")]
            if len(p) < 9:
                return None
            return CullEntry(
                center_x=float(p[0]), center_y=float(p[1]), center_z=float(p[2]),
                x1=float(p[3]), y1=float(p[4]), z1=float(p[5]),
                x2=float(p[6]), y2=float(p[7]), z2=float(p[8]),
                flags=int(float(p[9])) if len(p) > 9 else 0,
                wanted_level_drop=int(float(p[10])) if len(p) > 10 else 0,
                source_ipl=source, line_no=lineno)
        except (ValueError, IndexError):
            pass
        return None

    def _parse_occl(self, line: str, source: str, lineno: int) -> Optional[OcclEntry]: #vers 1
        """Parse one "occl" section line - MidX, MidY, BottomZ,
        WidthX, WidthY, Height, Rotation (7 fields). Confirmed against
        GTAMods/Grand Theft Wiki (word-for-word agreement between the
        two) and verified field-for-field against Keith's real
        occlu.ipl upload. "occl" wasn't even a recognised section
        keyword for VC before this (see IPL_SECTIONS' own fix note) -
        a real occlu.ipl's occl lines would have silently gone
        unrecognised regardless of this parser existing."""
        try:
            p = [x.strip() for x in line.split(",")]
            if len(p) < 6:
                return None
            return OcclEntry(
                mid_x=float(p[0]), mid_y=float(p[1]), bottom_z=float(p[2]),
                width_x=float(p[3]), width_y=float(p[4]), height=float(p[5]),
                rotation=float(p[6]) if len(p) > 6 else 0.0,
                source_ipl=source, line_no=lineno)
        except (ValueError, IndexError):
            pass
        return None

    def _parse_auzo(self, line: str, source: str, lineno: int) -> Optional[AuzoEntry]: #vers 1
        """Parse one "auzo" section line - either the cube shape
        (Name, ID, Switch, X1, Y1, Z1, X2, Y2, Z2 - 9 fields) or the
        sphere shape (Name, ID, Switch, X, Y, Z, Radius - 7 fields),
        told apart by field count (see AuzoEntry's own docstring for
        the full format confirmation against GTAMods). Name may
        itself legitimately contain a comma-adjacent quoted string in
        some real IPL data the way ENEX's own Name field does, but the
        wiki's own real examples show plain, unquoted zone names with
        no commas inside them, so a simple comma-split (matching cull/
        occl/zone's own established parsing here) is correct for this
        section specifically, unlike ENEX which needs its own quote-
        aware splitting."""
        try:
            p = [x.strip() for x in line.split(",")]
            if len(p) == 9:
                return AuzoEntry(
                    name=p[0], sound_id=int(p[1]), switch=int(p[2]), is_sphere=False,
                    x1=float(p[3]), y1=float(p[4]), z1=float(p[5]),
                    x2=float(p[6]), y2=float(p[7]), z2=float(p[8]),
                    source_ipl=source, line_no=lineno)
            elif len(p) == 7:
                return AuzoEntry(
                    name=p[0], sound_id=int(p[1]), switch=int(p[2]), is_sphere=True,
                    x1=float(p[3]), y1=float(p[4]), z1=float(p[5]),
                    radius=float(p[6]),
                    source_ipl=source, line_no=lineno)
        except (ValueError, IndexError):
            pass
        return None


class IDEDatabase: #vers 1
    """Lightweight standalone IDE database — loads all .ide files from a
    folder tree without requiring a full DAT/world load.
    Shared by Model Workshop (IDE lookup when DAT Browser not loaded),
    IDE Editor (analysis tools), and DAT Browser settings.

    ID limits:
      GTA3 / VC / GTASOL  → 32767  (signed int16 in SCM bytecode)
      SA (streaming only) → 65535  (uint16, but practical SA max ~26316)
    """

    GAME_MAX_ID = {
        GTAGame.GTA3: 32767,
        GTAGame.VC:   32767,
        GTAGame.SA:   65535,
        GTAGame.SOL:  32767,   # VC engine base — safe limit
    }

    def __init__(self, game = None):
        self._game:       object           = game or GTAGame.VC
        self.model_map:   Dict[str, 'IDEObject'] = {}   # stem→IDEObject
        self.id_map:      Dict[int, 'IDEObject'] = {}   # id→IDEObject
        self.source_files: List[str]       = []
        self._loaded      = False

    @property
    def max_id(self) -> int:
        return self.GAME_MAX_ID.get(self._game, 32767)

    def load_folder(self, folder: str,
                    game = None,
                    recurse: bool = True) -> int:
        """Scan folder for .ide files and parse them all.
        Returns total number of objects loaded."""
        if game:
            self._game = game
        if not os.path.isdir(folder):
            return 0

        ide_files = []
        if recurse:
            for dirpath, _, fnames in os.walk(folder):
                for f in fnames:
                    if f.lower().endswith('.ide'):
                        ide_files.append(os.path.join(dirpath, f))
        else:
            ide_files = [os.path.join(folder, f)
                         for f in os.listdir(folder)
                         if f.lower().endswith('.ide')]

        loaded = 0
        parser = IDEParser(self._game)
        for ide_path in ide_files:
            parser.objects.clear()
            if parser.parse(ide_path):
                for obj in parser.objects:
                    stem = obj.model_name.lower()
                    self.model_map[stem] = obj
                    self.id_map[obj.model_id] = obj
                loaded += len(parser.objects)
                self.source_files.append(ide_path)
        self._loaded = True
        return loaded

    def load_file(self, ide_path: str, game = None) -> int:
        """Load a single IDE file into the database."""
        if game:
            self._game = game
        parser = IDEParser(self._game)
        if parser.parse(ide_path):
            for obj in parser.objects:
                self.model_map[obj.model_name.lower()] = obj
                self.id_map[obj.model_id] = obj
            if ide_path not in self.source_files:
                self.source_files.append(ide_path)
            return len(parser.objects)
        return 0

    def lookup(self, model_name: str) -> Optional['IDEObject']:
        """Look up an IDEObject by model name (case-insensitive)."""
        return self.model_map.get(model_name.lower().split('.')[0])

    def lookup_id(self, model_id: int) -> Optional['IDEObject']:
        return self.id_map.get(model_id)

    #    Analysis tools                                                     

    def find_duplicate_ids(self) -> List[int]:
        """Return list of IDs that appear more than once across all loaded IDE files."""
        from collections import Counter
        counts: Counter = Counter()
        parser = IDEParser(self._game)
        for ide_path in self.source_files:
            parser.objects.clear()
            if parser.parse(ide_path):
                for obj in parser.objects:
                    counts[obj.model_id] += 1
        return [id_ for id_, n in counts.items() if n > 1]

    def find_duplicate_names(self) -> List[str]:
        """Return model names that appear more than once."""
        from collections import Counter
        counts: Counter = Counter()
        parser = IDEParser(self._game)
        for ide_path in self.source_files:
            parser.objects.clear()
            if parser.parse(ide_path):
                for obj in parser.objects:
                    counts[obj.model_name.lower()] += 1
        return [n for n, c in counts.items() if c > 1]

    def find_missing_models(self, img_stems: set) -> List['IDEObject']:
        """Return IDE objects whose DFF is not present in img_stems.
        img_stems: set of lowercased model names from IMG entries (no extension)."""
        return [obj for obj in self.model_map.values()
                if obj.model_name.lower() not in img_stems]

    def find_missing_txds(self, img_stems: set) -> List['IDEObject']:
        """Return IDE objects whose TXD is not present in img_stems.
        img_stems: set of lowercased txd names (no extension)."""
        return [obj for obj in self.model_map.values()
                if obj.txd_name and
                   obj.txd_name.lower() not in ('null','') and
                   obj.txd_name.lower() not in img_stems]

    def find_unused_ids(self, used_id_set: set = None) -> List[int]:
        """Return list of free/unused IDs in range 1..max_id.
        If used_id_set is None, uses ids from the loaded IDE objects."""
        if used_id_set is None:
            used_id_set = set(self.id_map.keys())
        return [i for i in range(1, self.max_id + 1) if i not in used_id_set]

    def find_ids_over_limit(self) -> List['IDEObject']:
        """Return IDE objects whose ID exceeds max_id for this game."""
        return [obj for obj in self.model_map.values()
                if obj.model_id > self.max_id]

    def summary(self) -> str:
        used = set(self.id_map.keys())
        over = self.find_ids_over_limit()
        dups = self.find_duplicate_ids()
        return (f"IDE DB: {len(self.model_map)} objects  "
                f"| {len(self.source_files)} files  "
                f"| max_id={self.max_id}  "
                f"| over limit={len(over)}  "
                f"| dup IDs={len(dups)}")


class GTAWorldLoader: #vers 3
    """
    Orchestrates the full two-phase GTA3/VC/SA load chain in engine order:
      Phase 1: default.dat -> base IDEs (DEFAULT.IDE; SA also loads VEHICLES.IDE + PEDS.IDE)
      Phase 2: main .dat   -> map IDEs, then IPLs (SA: also IMG directives)
                              SA alt: gta_quick.dat (stripped dev variant, auto-detected)

    Later IDE definitions override earlier ones (matches engine behaviour).
    """

    def __init__(self, game: str = GTAGame.GTA3):
        self.game        = game
        self.default_dat = DATParser(game)
        self.main_dat    = DATParser(game)
        self.objects:    Dict[int, IDEObject] = {}
        # 2dfx entries share their base object's model_id (e.g. multiple
        # lights/particle effects on one building all use that building's
        # ID) - kept separate from self.objects rather than folded in,
        # since IDEObject entries are looked up by model_id there and a
        # 2dfx "stub" entry (see IDEParser._parse_line) would otherwise
        # silently overwrite the real object definition for any ID that
        # also has an attached effect.
        self.effects_2dfx: Dict[int, List[IDEObject]] = {}
        # tobj (timed/day-night object variants) are tracked separately
        # too, for showing tobj info as part of an object's detail view
        # (matched by ID) - but unlike 2dfx's placeholder stubs, tobj
        # entries carry real model/txd data, so they're ALSO still kept
        # in self.objects as before (preserving existing TXD/Object
        # Browser lookups for tobj-only objects) rather than being
        # removed from it.
        self.timed_objects: Dict[int, List[IDEObject]] = {}
        self.instances:  List[IPLInstance]    = []
        self.paths:      List[PathGroup]      = []
        # GTA III's own IDE-embedded path groups (Aug 16 2026) - kept
        # separate from self.paths (VC/SA's own IPL-section path
        # format) since they're a genuinely different shape (relative
        # to a placed instance, not standalone world coordinates) -
        # see IDEPathGroup's own docstring for the full story.
        self.ide_paths:  List[IDEPathGroup]   = []
        self.grges:      List[GrgeEntry]       = []
        self.enexes:     List[EnexEntry]       = []
        self.zones:      List[Dict]           = []
        self.culls:      List[CullEntry]      = []
        self.occls:      List[OcclEntry]      = []
        self.auzos:      List[AuzoEntry]      = []
        # Train track waypoints (Aug 17 2026) - keyed by source
        # filename (e.g. "tracks.dat", "tracks2.dat"), each value an
        # ordered list of TrackWaypoint - not part of the IDE/IPL
        # section system at all, loaded separately by load_tracks_dat
        # since these files aren't referenced in gta.dat/gta3.dat's
        # own directive list.
        self.tracks:     Dict[str, List[TrackWaypoint]] = {}
        # SA vehicle/ped path node data (Aug 19 2026, per Keith: "i'd
        # be nice to see whats in those node.dat files, for SA") -
        # keyed by area_id (0-63), each value a fully-parsed SAPathFile
        # (apps/methods/sa_path_parser.py - a genuinely separate binary
        # format from III/VC's own self.paths, SA-only, not part of
        # the IDE/IPL section system at all, same "loaded separately"
        # reasoning as self.tracks just above). Populated by
        # load_sa_nodes, SA only.
        self.sa_nodes:   Dict[int, object] = {}
        # Police roadblock placements (Aug 19 2026, per Keith's real
        # ROADBLOX.DAT sample) - SA-only, references real vehicle
        # path nodes from self.sa_nodes above (see RoadblockEntry's
        # own docstring for the full format confirmation). Populated
        # by load_sa_roadblox, SA only.
        self.sa_roadblocks: List[RoadblockEntry] = []
        # Real water plane shapes (Aug 20 2026, per Keith: "lets get
        # all the functions in" - water/radar recalculation on map
        # moves, item 1 of 3 on his own list). SA's own text water.dat
        # only (see WaterShape's own docstring for the full format
        # confirmation) - III/VC use a completely different, binary
        # waterpro.dat format, stored separately below in self.
        # waterpro instead (a genuinely different structure, not a
        # list of shapes at all - see WaterProFile's own docstring).
        # Populated by load_water_dat.
        self.water_shapes: List[object] = []
        # III/VC's own binary waterpro.dat (Aug 20 2026, same request
        # as water_shapes just above) - a single WaterProFile or None,
        # not a list, since the real format itself is one fixed-size
        # binary structure (48 levels + a 64x64 visible map + a
        # 128x128 physical map), not a variable list of shapes the
        # way SA's own text water.dat is. Populated by load_waterpro_dat.
        self.waterpro: Optional[object] = None
        # GTA III chase-scene car paths (Aug 19 2026, per Keith's
        # real CHASE0-19.DAT sample) - keyed by source filename (e.g.
        # "CHASE0.DAT"), each value the full ordered list of real,
        # per-frame ChaseFrame records for that one car's own
        # recorded path through the introduction cutscene. GTA III
        # only, same "loaded separately, not part of the IDE/IPL
        # section system" reasoning as self.tracks. Populated by
        # load_chase_dat.
        self.chase_paths: Dict[str, List[object]] = {}
        # (phase, type, abs_path, success)
        self.load_log:   List[Tuple[str, str, str, bool]] = []
        self.stats       = ParseStats()
        self.progress_cb = None
        # Optional set of IPL basenames (lowercase, no extension) to
        # restrict loading to - None means load every IPL the .dat(s)
        # reference, the existing/default behaviour. Set before calling
        # load()/load_from_dat() - _reset() doesn't touch this, so it
        # survives across those calls.
        self.ipl_filter: Optional[set] = None
        # Per Keith's MooMapper comparison: it lists every available IPL
        # path immediately but doesn't actually parse/load an IPL's
        # content until the user asks for it. Opt-in (default False,
        # existing eager-load-everything behaviour unchanged) since
        # other callers (DAT Browser, Dump TXDs) may depend on every
        # instance actually being loaded after load()/load_from_dat()
        # returns - only Map Workshop sets this True. When True,
        # _process_dat only discovers/records available IPLs (into
        # available_ipls) instead of parsing them; load_ipl_by_name()
        # then does the actual, real load for one specific IPL on
        # demand, exactly matching MooMapper's model.
        self.lazy_ipl_loading: bool = False
        self.available_ipls: Dict[str, DATEntry] = {}   # lowercase stem -> DATEntry
        self.loaded_ipls: set = set()   # lowercase stems already loaded on demand

    def load(self, game_root: str, progress_cb=None) -> bool: #vers 5
        """Full load from a game root directory.
        Always enforces models/gta3.img (called from game exe, not from any .dat)
        so TXD Workshop and the Dump TXDs feature can always find it.
        For SOL, also enforces models/radartex.img if present."""
        self.progress_cb = progress_cb
        self._reset()

        #    Inject exe-loaded archives (not in any .dat)                   
        # gta3.img is always loaded by the game exe — enforce it here so
        # the DAT Browser, Dump TXDs, and xref can see it for all games.
        self._inject_enforced_imgs(game_root)

        #    Locate phase-1 (default/special) dat                          
        default_path = find_default_dat(game_root, self.game)
        if default_path:
            self._progress(0, 1, f"Phase 1: {os.path.basename(default_path)}")
            self.default_dat.parse(default_path, game_root)
            self._process_dat(self.default_dat, "default")
        else:
            self.stats.warnings.append(
                f"Phase-1 dat not found for game '{self.game}' in {game_root}")

        #    Locate phase-2 main dat                                        
        main_path = find_dat_file(game_root, self.game)
        if not main_path:
            self.stats.errors.append(
                f"Main DAT not found for game '{self.game}' in {game_root}")
            return False

        self._progress(0, 1, f"Phase 2: {os.path.basename(main_path)}")
        self.main_dat.parse(main_path, game_root)
        self._process_dat(self.main_dat, "main")

        self.stats.objects_loaded = len(self.objects)
        self.stats.instances      = len(self.instances)
        return True

    def _inject_enforced_imgs(self, game_root: str): #vers 3
        """Inject models/gta3.img which the game exe always loads directly —
        it never appears in any .dat file for GTA3, VC, SA or SOL.
        We deduplicate both by normalised abs-path and by basename so that
        a .dat that happens to list gta3.img explicitly won't cause a second
        entry."""
        # Only gta3.img is exe-loaded and absent from every game's .dat.
        # radartex.img IS listed in gta_sol.dat so we don't enforce it;
        # the _process_dat() call will pick it up from the dat entries.
        rel = os.path.join("models", "gta3.img")

        # Build sets for fast dedup: normalised full path + basename
        seen_abs   = {os.path.normcase(p) for _, _, p, _ in self.load_log}
        seen_stems = {os.path.splitext(os.path.basename(p))[0].lower()
                      for _, et, p, _ in self.load_log
                      if et in ('IMG', 'CDIMAGE')}

        if 'gta3' in seen_stems:
            return   # already in log from a .dat

        abs_path = _resolve_ci(game_root, rel)
        if not abs_path:
            abs_path = os.path.normpath(os.path.join(game_root, rel))

        if os.path.normcase(abs_path) in seen_abs:
            return   # already logged by abs path

        exists = os.path.isfile(abs_path)
        self.load_log.append(("enforced", "IMG", abs_path, exists))
        if exists:
            self.stats.img_files += 1

    def load_from_dat(self, dat_path: str, game_root: str = "",
                      progress_cb=None) -> bool: #vers 1
        """Load from an explicit .dat path."""
        self.progress_cb = progress_cb
        self._reset()
        if not game_root:
            game_root = os.path.normpath(
                os.path.join(os.path.dirname(dat_path), ".."))
        data_dir     = os.path.dirname(dat_path)
        default_name = GTAGame.DEFAULT_DAT.get(self.game)
        if default_name:
            default_path = os.path.join(data_dir, default_name)
            if os.path.isfile(default_path):
                self._progress(0, 1, f"Phase 1: {default_name}")
                self.default_dat.parse(default_path, game_root)
                self._process_dat(self.default_dat, "default")
        self._inject_enforced_imgs(game_root)
        self._progress(0, 1, f"Phase 2: {os.path.basename(dat_path)}")
        self.main_dat.parse(dat_path, game_root)
        self._process_dat(self.main_dat, "main")
        self.stats.objects_loaded = len(self.objects)
        self.stats.instances      = len(self.instances)
        self.load_tracks_dat(data_dir)
        if self.game == GTAGame.SA:
            # SA-only (Aug 19 2026, per Keith: "i'd be nice to see
            # whats in those node.dat files, for SA") - vehicle/ped
            # path nodes are a completely different, binary-only
            # format specific to SA (III uses IDE-embedded paths, VC
            # uses the text IPL "path" section - neither has a
            # nodesN.dat equivalent at all), so this is gated the same
            # way GTA III's own IDE-path resolution already is,
            # unlike load_tracks_dat just above which applies across
            # multiple games.
            self.load_sa_nodes(game_root, data_dir)
            self.load_sa_roadblox(data_dir)
            self.load_water_dat()
        if self.game == GTAGame.GTA3:
            # GTA III-only (Aug 19 2026, per Keith's real CHASE0-19.
            # DAT sample) - the introduction cutscene's own chase-
            # scene car paths, a format specific to III (its own
            # successor RRR/carrec.img is a completely different SA/
            # GTA IV mechanism entirely, not something this app reads
            # or writes today).
            self.load_chase_dat(data_dir)
        if self.game in (GTAGame.GTA3, GTAGame.VC):
            # III/VC-only (Aug 20 2026, continuing the same "lets get
            # all the functions in" water request that started with
            # SA's own water.dat just above) - waterpro.dat is a
            # completely different, binary format specific to these
            # two games (SA uses its own text water.dat instead, see
            # load_water_dat just above).
            self.load_waterpro_dat()
        return True

    def load_tracks_dat(self, data_dir: str): #vers 2
        """Load train track waypoints, and (Aug 19 2026, per Keith's
        real LC/VC/SA path-folder samples) every other file confirmed
        to share the exact same "count then X Y Z[ flag] lines" shape
        - flight.dat/flight2.dat/flight3.dat/flight4.dat (per Keith:
        "some are Airplane paths") and spath0.dat, alongside the
        original tracks.dat/tracks2.dat. Also fixed a real gap this
        same pass: SA genuinely has FOUR tracks files (tracks3.dat/
        tracks4.dat too, confirmed present and same format in Keith's
        real SA sample) - the original `wanted` set only covered two,
        silently missing two real, valid track files for SA every
        time this ran.

        Not referenced anywhere in gta.dat/gta3.dat's own directive
        list for any of these (confirmed absent from a real, complete
        gta3.dat) - the game loads these from this fixed, well-known
        relative path instead, so this is called unconditionally at
        the end of load_from_dat rather than gated by any directive.
        Case-insensitive lookup for both the "paths" subdirectory and
        the filenames themselves, matching this file's own
        established convention for locating real files on a case-
        sensitive filesystem (Linux) against data that may have been
        packaged with different casing.

        flight*.dat/spath0.dat are stored in self.tracks too (same
        dict, same TrackWaypoint shape) rather than a separate
        collection - they're genuinely the same format and the same
        "an ordered point list, no node graph" nature as tracks.dat
        itself, just a different in-game purpose (aircraft paths vs
        rail paths) - a caller that wants to tell them apart can
        still do so via each TrackWaypoint's own source_file."""
        if not data_dir or not os.path.isdir(data_dir):
            return
        paths_dir = None
        for name in os.listdir(data_dir):
            if name.lower() == 'paths' and os.path.isdir(os.path.join(data_dir, name)):
                paths_dir = os.path.join(data_dir, name)
                break
        if paths_dir is None:
            return
        wanted = {'tracks.dat', 'tracks2.dat', 'tracks3.dat', 'tracks4.dat',
                  'flight.dat', 'flight2.dat', 'flight3.dat', 'flight4.dat',
                  'spath0.dat'}
        for name in os.listdir(paths_dir):
            if name.lower() not in wanted:
                continue
            abs_path = os.path.join(paths_dir, name)
            waypoints = self._parse_tracks_file(abs_path, name)
            if waypoints:
                self.tracks[name] = waypoints
                self.load_log.append(("tracks", "TRACKS", abs_path, True))

    def load_sa_nodes(self, game_root: str = "", data_dir: str = ""): #vers 1
        """Load every real, game-used nodesN.dat area file for SA (Aug
        19 2026, per Keith: "i'd be nice to see whats in those
        node.dat files, for SA"). Confirmed via direct research before
        writing this, not assumed: the wiki-documented "the game
        ignores nodesN.dat" claim specifically refers to LOOSE copies
        sitting in data/paths/ on disk - the real, actually-used copies
        the game reads for genuine vehicle/ped pathfinding are the 64
        area files packed INSIDE gta3.img (or another archive), at the
        standard game_root/models/gta3.img location. Tries that real
        location first via apps.methods.sa_path_parser's already-built
        find_nodes_dat_in_img/load_nodes_dat_from_img_entry (a local
        import here, not at this module's own top level - img_core_
        classes.py pulls in PyQt6, and this module is deliberately
        kept GUI-free at import time so it stays usable in a headless
        context; only a caller that actually needs this pays that
        cost). Falls back to the loose data/paths/ directory (via sa_
        path_parser's own load_all_nodes_dat_from_dir) only if the
        archive isn't found/openable - genuinely useful as a fallback
        for comparison/reference even though the wiki says the game
        itself won't read that particular copy, so this doesn't
        refuse to load it, it just can't be presented as the "real"
        in-game data the way the archive copy can.

        Populates self.sa_nodes keyed by area_id. All 64 areas are
        loaded together (not one at a time on demand) because links
        between path nodes can cross between areas - resolving a
        link's own target position correctly needs the whole combined
        set already loaded, not just the one area file a caller might
        currently be looking at."""
        from apps.methods.sa_path_parser import (
            find_nodes_dat_in_img, load_nodes_dat_from_img_entry,
            load_all_nodes_dat_from_dir)
        loaded_any = False
        if game_root:
            img_path = os.path.join(game_root, 'models', 'gta3.img')
            if not os.path.isfile(img_path):
                # Case-insensitive fallback - same reasoning load_
                # tracks_dat already uses for its own subdirectory
                # lookup, real installs on Linux won't always match
                # the documented casing exactly.
                models_dir = os.path.join(game_root, 'models')
                if os.path.isdir(models_dir):
                    for name in os.listdir(models_dir):
                        if name.lower() == 'gta3.img':
                            img_path = os.path.join(models_dir, name)
                            break
            if os.path.isfile(img_path):
                try:
                    from apps.methods.img_core_classes import IMGFile
                    arc = IMGFile(img_path)
                    arc.open()
                    entries = find_nodes_dat_in_img(arc)
                    for area_id, entry in entries.items():
                        parsed = load_nodes_dat_from_img_entry(arc, entry, area_id)
                        if parsed is not None:
                            self.sa_nodes[area_id] = parsed
                            loaded_any = True
                    if loaded_any:
                        self.load_log.append(
                            ("nodes", "SA_NODES", img_path, True))
                except Exception as e:
                    self.stats.errors.append(f"Could not read SA nodes from {img_path}: {e}")
        if not loaded_any and data_dir:
            paths_dir = None
            for name in os.listdir(data_dir) if os.path.isdir(data_dir) else []:
                if name.lower() == 'paths' and os.path.isdir(os.path.join(data_dir, name)):
                    paths_dir = os.path.join(data_dir, name)
                    break
            if paths_dir:
                loose = load_all_nodes_dat_from_dir(paths_dir)
                if loose:
                    self.sa_nodes.update(loose)
                    self.load_log.append(
                        ("nodes", "SA_NODES_LOOSE", paths_dir, True))

    def load_sa_roadblox(self, data_dir: str): #vers 1
        """Load SA's real police-roadblock placement data from data/
        paths/ROADBLOX.DAT (Aug 19 2026, per Keith's real sample -
        "lets do those next"). Format confirmed against real,
        published documentation (GTAMods wiki) AND cross-checked
        directly: a 4-byte int32 count followed by up to 325 fixed
        (area_id: int16, node_id: uint16) slots, matching the real
        file's own exact 1304-byte size (4 + 325*4) - only the first
        `count` slots are meaningful, the rest is fixed padding always
        present regardless of how many roadblocks are actually
        defined. Verified all 325 real entries in Keith's own real
        file resolve to a genuinely valid vehicle node index within
        their own stated area, cross-referenced against his own real,
        complete NODES0-63.DAT set - not just a format that happens
        to parse without error.

        Same case-insensitive "paths" subdirectory lookup convention
        already established by load_tracks_dat/load_sa_nodes."""
        if not data_dir or not os.path.isdir(data_dir):
            return
        paths_dir = None
        for name in os.listdir(data_dir):
            if name.lower() == 'paths' and os.path.isdir(os.path.join(data_dir, name)):
                paths_dir = os.path.join(data_dir, name)
                break
        if paths_dir is None:
            return
        abs_path = None
        for name in os.listdir(paths_dir):
            if name.lower() == 'roadblox.dat':
                abs_path = os.path.join(paths_dir, name)
                break
        if abs_path is None:
            return
        try:
            with open(abs_path, 'rb') as f:
                data = f.read()
            if len(data) < 4:
                return
            count = struct.unpack_from('<i', data, 0)[0]
            if count < 0 or count > 325:
                # Documented as a real, valid way to disable the file
                # entirely ("count can be set to -1... roadblocks
                # will be ignored") - and a corrupt/unexpected value
                # beyond the fixed 325-slot capacity is never trusted
                # either way, rather than reading past the file's own
                # real bounds.
                return
            entries = []
            for i in range(count):
                off = 4 + i * 4
                if off + 4 > len(data):
                    break
                area_id, node_id = struct.unpack_from('<hH', data, off)
                entries.append(RoadblockEntry(area_id=area_id, node_id=node_id))
            self.sa_roadblocks = entries
            self.load_log.append(("roadblox", "SA_ROADBLOX", abs_path, True))
        except Exception as e:
            self.stats.errors.append(f"Could not read SA roadblox data from {abs_path}: {e}")

    def load_water_dat(self): #vers 1
        """Load SA's real water.dat (Aug 20 2026, per Keith: "lets
        get all the functions in" - water/radar recalculation on map
        moves, item 1 of 3 on his own list). Finds the REAL path from
        gta.dat's own already-parsed WATER directive entries (main_
        dat.water_entries()) rather than assuming a fixed "data/
        water.dat" location - the directive is real, documented (per
        GTAMods' own gta.dat page: "these entries link to external
        water plane placement files"), and the generic directive-
        parsing branch in DATParser.parse() already captures it
        correctly (WATER <path>, same shape as every other simple
        single-path directive) even though nothing looked for it by
        name until this method existed. Uses the first real,
        resolved, existing entry found - a real gta.dat could list
        more than one WATER line (GTAMods: "the WATER identifier can
        hold more than one parameter"), but SA's own real water1.dat
        is documented as a dead, unused leftover, so taking the
        first real match is the correct choice, not an oversimplification."""
        entries = getattr(self.main_dat, 'water_entries', lambda: [])()
        for entry in entries:
            if entry.exists:
                shapes = parse_water_dat(entry.abs_path)
                if shapes:
                    self.water_shapes = shapes
                    self.load_log.append(("water", "WATER", entry.abs_path, True))
                    return

    def load_waterpro_dat(self): #vers 1
        """Load GTA III/VC's own binary waterpro.dat (Aug 20 2026,
        continuing the same "lets get all the functions in" request
        that started with SA's own water_shapes/load_water_dat just
        above - this is the III/VC counterpart, a completely
        different binary format, not the same file at all despite
        the similar name/purpose). Finds the real path the exact same
        way load_water_dat already does - via main_dat's own real,
        parsed WATER directive entries - since gta3.dat/gta_vc.dat
        use the identical WATER directive keyword SA's own gta.dat
        does (the gta*.dat directive format itself is shared across
        every game, confirmed via GTAMods' own gta.dat page). Success
        checked via `result is not None` rather than a truthy list
        the way load_water_dat's own check is - parse_waterpro_dat
        returns a single WaterProFile (or None), never a list, since
        the real binary format itself is one fixed-size structure,
        not a variable list of shapes."""
        entries = getattr(self.main_dat, 'water_entries', lambda: [])()
        for entry in entries:
            if entry.exists:
                result = parse_waterpro_dat(entry.abs_path)
                if result is not None:
                    self.waterpro = result
                    self.load_log.append(("water", "WATERPRO", entry.abs_path, True))
                    return

    def load_chase_dat(self, data_dir: str): #vers 1
        """Load every real GTA III CHASE*.DAT chase-scene car path
        found in data/paths/ (Aug 19 2026, per Keith's real sample -
        "lets do those next"). Format confirmed against real,
        published documentation (GTAMods wiki) AND cross-checked
        directly against Keith's own real CHASE0.DAT: no header or
        count at all, just a plain, fixed-size 28-byte record repeated
        for the whole file - the real file's own size divided cleanly
        by 28 with zero remainder, and real decoded positions form a
        tight, plausible cluster of GTA III world coordinates that
        change smoothly frame-to-frame, not a coincidental byte
        alignment. GTA III only - its own successor mechanism (RRR
        files inside carrec.img) is a completely different SA/GTA IV
        format this app doesn't read.

        Scans for ANY file matching CHASE<N>.DAT (case-insensitive) in
        the paths directory, rather than a fixed list of exactly 20 -
        Keith's own real upload only had 14 of the 20 possible index
        numbers present (some indices are simply unused in a real
        install), so a fixed "must have all 20" list would silently
        skip real, present files. Same case-insensitive "paths"
        subdirectory lookup convention already established by load_
        tracks_dat/load_sa_nodes/load_sa_roadblox."""
        if not data_dir or not os.path.isdir(data_dir):
            return
        paths_dir = None
        for name in os.listdir(data_dir):
            if name.lower() == 'paths' and os.path.isdir(os.path.join(data_dir, name)):
                paths_dir = os.path.join(data_dir, name)
                break
        if paths_dir is None:
            return
        chase_re = re.compile(r'^chase\d+\.dat$', re.IGNORECASE)
        for name in os.listdir(paths_dir):
            if not chase_re.match(name):
                continue
            abs_path = os.path.join(paths_dir, name)
            frames = self._parse_chase_file(abs_path, name)
            if frames:
                self.chase_paths[name] = frames
                self.load_log.append(("chase", "CHASE_PATH", abs_path, True))

    def _parse_chase_file(self, abs_path: str, source_name: str): #vers 1
        """Parse one CHASE*.DAT file - a plain sequence of fixed
        28-byte records, no header (see ChaseFrame's own docstring
        for the full field layout and real-data confirmation).
        Ignores any trailing partial record (a file whose size isn't
        an exact multiple of 28 - shouldn't happen for a real,
        uncorrupted file, but division here is int-truncating, so a
        stray few extra bytes at the end are simply never read as a
        record rather than raising or reading past the file)."""
        try:
            with open(abs_path, 'rb') as f:
                data = f.read()
        except Exception as e:
            self.stats.errors.append(f"Could not read {abs_path}: {e}")
            return []
        record_count = len(data) // 28
        frames = []
        for i in range(record_count):
            off = i * 28
            try:
                vx, vy, vz = struct.unpack_from('<hhh', data, off)
                (right_x, right_y, right_z, top_x, top_y, top_z,
                 steer, gas, brake, handbrake) = struct.unpack_from('<10b', data, off + 6)
                pos_x, pos_y, pos_z = struct.unpack_from('<fff', data, off + 16)
            except struct.error:
                break
            frames.append(ChaseFrame(
                vel_x=vx / 16383.5, vel_y=vy / 16383.5, vel_z=vz / 16383.5,
                right_x=right_x / 127.0, right_y=right_y / 127.0, right_z=right_z / 127.0,
                top_x=top_x / 127.0, top_y=top_y / 127.0, top_z=top_z / 127.0,
                steering=steer / 20.0, gas=gas / 100.0, brake=brake / 100.0,
                handbrake=bool(handbrake),
                pos_x=pos_x, pos_y=pos_y, pos_z=pos_z,
                source_file=source_name, frame_index=i))
        return frames

    def _parse_tracks_file(self, abs_path: str, source_name: str): #vers 2
        """Parse one tracks.dat-shaped file - a waypoint count on the
        first line, then exactly that many "X Y Z" (or "X Y Z FLAG")
        lines (see TrackWaypoint's own docstring for the full format
        confirmation, including the 4th value's own confirmation
        against Keith's real SA tracks.dat/tracks2/3/4.dat samples -
        VC/GTA III's own tracks.dat/tracks2.dat only ever had 3 per
        line, which is why this only expected 3 originally). Genuinely
        simpler than every other path format handled in this file -
        no section keywords, no node graph, just an ordered point
        list. A 4th value, when present, is captured into TrackWaypoint
        .flag rather than silently discarded (the previous behaviour) -
        None when a line only has 3 values, matching flight*.dat/
        spath0.dat's own real, confirmed shape (never a 4th value)."""
        try:
            with open(abs_path, 'r', encoding='ascii', errors='ignore') as f:
                lines = [ln.strip() for ln in f if ln.strip()]
        except Exception:
            return []
        if not lines:
            return []
        try:
            count = int(lines[0])
        except ValueError:
            return []
        waypoints = []
        for i, line in enumerate(lines[1:1 + count]):
            parts = line.split()
            if len(parts) < 3:
                continue
            try:
                x, y, z = float(parts[0]), float(parts[1]), float(parts[2])
            except ValueError:
                continue
            flag = None
            if len(parts) >= 4:
                try:
                    flag = int(parts[3])
                except ValueError:
                    flag = None
            waypoints.append(TrackWaypoint(x=x, y=y, z=z, source_file=source_name, index=i, flag=flag))
        return waypoints

    def _process_dat(self, dat: DATParser, phase: str): #vers 5
        ide_list = [e for e in dat.entries if e.directive == "IDE"]
        # GTA3 uses a different directive keyword (MAPZONE) specifically
        # for its zone file (MAP.ZON), while VC/SA load the equivalent
        # file via the ordinary IPL directive - functionally identical
        # (IPLParser handles zone/cull sections the same way regardless
        # of which directive pointed at the file), so both are processed
        # together here rather than MAPZONE being silently ignored.
        ipl_list = [e for e in dat.entries if e.directive in ("IPL", "MAPZONE")]
        if self.ipl_filter is not None:
            allowed = self.ipl_filter
            skipped = [e for e in ipl_list
                      if os.path.splitext(os.path.basename(e.path))[0].lower() not in allowed]
            ipl_list = [e for e in ipl_list
                       if os.path.splitext(os.path.basename(e.path))[0].lower() in allowed]
            for entry in skipped:
                self.load_log.append((phase, "IPL-skipped", entry.abs_path, True))
        img_list = dat.img_entries()   # IMG + CDIMAGE entries
        total = len(ide_list) + len(ipl_list)
        done  = 0
        # Log IMG/CDIMAGE archives so the tree and dump feature see them
        for entry in img_list:
            exists = entry.exists if hasattr(entry, 'exists') else os.path.isfile(entry.abs_path)
            self.load_log.append((phase, "IMG", entry.abs_path, exists))
        for entry in ide_list:
            done += 1
            self._progress(done, total, f"IDE: {os.path.basename(entry.path)}")
            self._load_ide(entry, phase)
        for entry in ipl_list:
            done += 1
            stem = os.path.splitext(os.path.basename(entry.abs_path))[0].lower()
            if self.lazy_ipl_loading:
                self._progress(done, total, f"Found IPL: {os.path.basename(entry.path)}")
                self.available_ipls[stem] = entry
                self.load_log.append((phase, "IPL-available", entry.abs_path, entry.exists))
            else:
                self._progress(done, total, f"IPL: {os.path.basename(entry.path)}")
                self._load_ipl(entry, phase)
        # Log COLFILE entries so DAT Browser tree can display and open them
        for entry in dat.col_entries():
            ok = os.path.isfile(entry.abs_path)
            self.load_log.append((phase, "COLFILE", entry.abs_path, ok))
        # Log standalone TEXDICTION/MODELFILE entries too - these are TXD/
        # DFF files referenced directly (not via an IMG archive), most
        # commonly in default.dat for VC's "generic" wheels/aircraft
        # models. Previously silently dropped entirely (parsed into
        # dat.entries but never surfaced by _process_dat at all) - not
        # loading their actual content yet (that's part of the real
        # per-instance DFF/TXD geometry work), but at least visible/
        # trackable now rather than vanishing without a trace.
        for entry in dat.entries:
            if entry.directive in ("TEXDICTION", "MODELFILE"):
                self.load_log.append((phase, entry.directive, entry.abs_path,
                                      os.path.isfile(entry.abs_path)))
        self.stats.ide_files += len(ide_list)
        self.stats.ipl_files += len(ipl_list)
        self.stats.col_files += len(dat.col_entries())
        self.stats.img_files += len(img_list)

    def _load_ide(self, entry: DATEntry, phase: str): #vers 3
        if not entry.exists:
            self.stats.warnings.append(f"[{phase}] IDE missing: {entry.path}")
            self.load_log.append((phase, "IDE", entry.abs_path, False))
            return
        parser = IDEParser(self.game)
        ok     = parser.parse(entry.abs_path)
        self.load_log.append((phase, "IDE", entry.abs_path, ok))
        for obj in parser.objects:
            if obj.section == "2dfx":
                self.effects_2dfx.setdefault(obj.model_id, []).append(obj)
                continue
            if obj.section == "tobj":
                self.timed_objects.setdefault(obj.model_id, []).append(obj)
            self.objects[obj.model_id] = obj   # later overrides earlier
        self.ide_paths += parser.ide_paths
        self.stats.errors   += parser.stats.errors
        self.stats.warnings += parser.stats.warnings

    def load_ipl_by_name(self, ipl_stem: str) -> IPLLoadResult: #vers 2
        """Actually parse and load one specific IPL's content, given its
        lowercase stem (no extension) as it appears in available_ipls -
        the on-demand counterpart to lazy_ipl_loading's discovery-only
        _process_dat pass. Adds the resulting instances/zones/culls to
        this loader's own lists (so everything downstream - Object
        Browser, World View, LOD pairing, etc - sees them exactly as if
        they'd been loaded eagerly), and records the stem in
        loaded_ipls so it isn't reloaded (or double-counted) if
        requested again.

        Returns an IPLLoadResult (not a bare bool) with per-IPL error/
        warning counts and messages - per Keith's request for per-IPL
        success/error reporting during loading, which needs to know
        specifically what went wrong with THIS one IPL, not just the
        loader's overall accumulated stats."""
        if ipl_stem in self.loaded_ipls:
            return IPLLoadResult(success=True)   # already loaded, nothing to do
        entry = self.available_ipls.get(ipl_stem)
        if entry is None:
            return IPLLoadResult(success=False, errors=[f"Unknown IPL: {ipl_stem}"])
        if not entry.exists:
            msg = f"IPL missing: {entry.path}"
            self.stats.warnings.append(msg)
            return IPLLoadResult(success=False, abs_path=entry.abs_path, errors=[msg])
        parser = IPLParser(self.game)
        ok = parser.parse(entry.abs_path)
        self.load_log.append(("on-demand", "IPL", entry.abs_path, ok))
        self.instances += parser.instances
        self.paths     += parser.paths
        self.grges     += parser.grges
        self.enexes    += parser.enexes
        self.zones     += parser.zones
        self.culls     += parser.culls
        self.occls     += parser.occls
        self.auzos     += parser.auzos
        self.stats.errors   += parser.stats.errors
        self.stats.warnings += parser.stats.warnings
        self.loaded_ipls.add(ipl_stem)
        return IPLLoadResult(
            success=ok, abs_path=entry.abs_path,
            instance_count=len(parser.instances),
            error_count=len(parser.stats.errors),
            warning_count=len(parser.stats.warnings),
            errors=list(parser.stats.errors),
            warnings=list(parser.stats.warnings))

    def _load_ipl(self, entry: DATEntry, phase: str): #vers 2
        if not entry.exists:
            self.stats.warnings.append(f"[{phase}] IPL missing: {entry.path}")
            self.load_log.append((phase, "IPL", entry.abs_path, False))
            return
        parser = IPLParser(self.game)
        ok     = parser.parse(entry.abs_path)
        self.load_log.append((phase, "IPL", entry.abs_path, ok))
        self.instances += parser.instances
        self.paths     += parser.paths
        self.grges     += parser.grges
        self.enexes    += parser.enexes
        self.zones     += parser.zones
        self.culls     += parser.culls
        self.occls     += parser.occls
        self.auzos     += parser.auzos
        self.stats.errors   += parser.stats.errors
        self.stats.warnings += parser.stats.warnings

    def _reset(self): #vers 6
        self.objects.clear(); self.effects_2dfx.clear()
        self.timed_objects.clear(); self.instances.clear()
        self.zones.clear();   self.culls.clear()
        self.occls.clear()
        self.auzos.clear()
        self.ide_paths.clear()
        self.available_ipls.clear(); self.loaded_ipls.clear()
        self.load_log.clear(); self.stats = ParseStats()

    def _progress(self, cur: int, total: int, msg: str): #vers 1
        if callable(self.progress_cb):
            try: self.progress_cb(cur, total, msg)
            except Exception: pass

    def get_object(self, model_id: int) -> Optional[IDEObject]:
        return self.objects.get(model_id)

    def find_by_name(self, name: str) -> List[IDEObject]:
        n = name.lower()
        return [o for o in self.objects.values() if o.model_name.lower() == n]

    def get_instances_for_model(self, model_id: int) -> List[IPLInstance]:
        return [i for i in self.instances if i.model_id == model_id]

    def get_img_paths(self) -> List[str]:
        """Every IMG archive path referenced by the loaded .dat(s),
        that actually exists on disk - already tracked in load_log
        (appended during _inject_enforced_imgs and the main IMG-
        directive processing), just exposed here as a convenience
        accessor rather than callers needing to filter load_log
        themselves. Useful for future features that need to know where
        to write new models/textures (e.g. Add), not just where the
        world data was read from."""
        seen = set()
        paths = []
        for phase, entry_type, abs_path, exists in self.load_log:
            if entry_type == "IMG" and exists and abs_path not in seen:
                seen.add(abs_path)
                paths.append(abs_path)
        return paths

    def get_col_paths(self) -> List[str]: #vers 1
        """Every standalone collision file path from COLFILE
        directives in the loaded .dat(s), that actually exists on
        disk - same accessor pattern as get_img_paths, reading the
        same load_log (COLFILE entries are already appended there in
        _process_dat, "so DAT Browser tree can display and open
        them"). Aug 14 2026, per Keith: in GTA3 collision is ONLY
        reachable this way (no COL entries in the IMG at all - the
        .dat's COLFILE paths point into data/maps/); in VC most
        per-object collision is in gta3.img like the models, but a
        handful of shared collision (e.g. generic.col) is still
        COLFILE-referenced; SA has zero COLFILE directives (all
        collision lives in the IMG, indexed by ModelCache.
        index_img_files instead - see its own docstring)."""
        seen = set()
        paths = []
        for phase, entry_type, abs_path, exists in self.load_log:
            if entry_type == "COLFILE" and exists and abs_path not in seen:
                seen.add(abs_path)
                paths.append(abs_path)
        return paths

    def get_2dfx_for_model(self, model_id: int) -> List[IDEObject]:
        """2DFX effects (lights, particles, etc) attached to a model,
        matched by model_id - see effects_2dfx for why these are kept
        separate from self.objects."""
        return self.effects_2dfx.get(model_id, [])

    def get_tobj_for_model(self, model_id: int) -> List[IDEObject]:
        """Timed/day-night object variants for a model, matched by
        model_id - see timed_objects."""
        return self.timed_objects.get(model_id, [])

    def resolve_lod_pairs(self) -> Dict[int, IPLInstance]:
        """Resolve each instance's paired LOD counterpart, where one
        exists. Two detection strategies, both run for every game and
        combined (Aug 1 2026, widened from being mutually exclusive
        by game - per Keith: "when LOD only is set, it still loads
        everything, when Norm is set, it loads the lods aswell,
        filenames for lods, Start of LOD or lod" - his own real SA
        data has always shown lod_index=-1 in practice (e.g.
        LODroadB48 in LAe.ipl), so the lod_index-only strategy
        previously gated to SA/SOL found nothing there; his own
        model name ("LODroadB48") confirms SA uses the same "LOD"
        prefix naming convention as GTA3/VC, not lod_index alone -
        same lesson as the rotation conjugate fix, which also turned
        out to need widening from an initially SA/VC-specific
        assumption to apply universally):

        1. lod_index field (SA/SOL, best-effort, based on community-
           documented format - not verified against official
           documentation): a positive lod_index is the 0-based
           position, among just the "inst" entries of that SAME
           source .ipl file, of this instance's paired counterpart.
           -1 means no LOD pair via this field. Still checked for
           every game, in case some data genuinely uses it, even
           though real SA sample data seen so far hasn't.

        2. "LOD" name-prefix matching (all games): an instance whose
           model name starts with "lod" (case-insensitive) pairs with
           another instance in the same source IPL file whose model
           name matches the remainder (e.g. "LODdock10" -> "dock10",
           "LODroadB48" -> "roadB48") AND whose position is at (or
           extremely close to) the same coordinates - the position
           check guards against two unrelated objects that happen to
           share a name pattern coincidentally.

        Returns a dict mapping id(instance) -> its paired IPLInstance,
        for every instance that resolves to a valid pair via either
        strategy. Keyed by id() rather than model_id/position, since
        multiple instances can share a model_id and this is about a
        specific placement's specific pairing, not anything
        model-level."""
        pairs: Dict[int, IPLInstance] = {}
        by_file: Dict[str, list] = {}
        for inst in self.instances:
            by_file.setdefault(inst.source_ipl, []).append(inst)

        # Strategy 1: lod_index field
        for file_instances in by_file.values():
            for inst in file_instances:
                if inst.lod_index is not None and inst.lod_index >= 0 \
                        and inst.lod_index < len(file_instances):
                    pairs[id(inst)] = file_instances[inst.lod_index]

        # Strategy 2: "LOD" name-prefix matching
        pos_tol = 0.5   # units - allows tiny float/rounding differences
        for file_instances in by_file.values():
            # Index non-LOD instances in this file by name for fast,
            # tolerant matching below.
            by_name: Dict[str, list] = {}
            for inst in file_instances:
                if not inst.model_name.lower().startswith('lod'):
                    by_name.setdefault(inst.model_name.lower(), []).append(inst)
            for inst in file_instances:
                name = inst.model_name
                if not name.lower().startswith('lod'):
                    continue
                base_name = name[3:].lower()   # strip "LOD"/"lod" prefix
                candidates = by_name.get(base_name, [])
                for cand in candidates:
                    if (abs(cand.pos_x - inst.pos_x) <= pos_tol and
                            abs(cand.pos_y - inst.pos_y) <= pos_tol and
                            abs(cand.pos_z - inst.pos_z) <= pos_tol):
                        pairs[id(cand)] = inst
                        break

        return pairs

    def get_objects_by_type(self, obj_type: str) -> List[IDEObject]:
        t = obj_type.lower()
        return [o for o in self.objects.values() if o.obj_type == t]

    def lookup_img_entry(self, filename_no_ext: str) -> Optional[IDEObject]:
        """Find IDE def for a DFF/TXD name (no extension)."""
        n = filename_no_ext.lower()
        for obj in self.objects.values():
            if obj.model_name.lower() == n:
                return obj
        return None

    def get_summary(self) -> str: #vers 2
        return "\n".join([
            f"Game:        {self.game.upper()}",
            f"default.dat: {os.path.basename(self.default_dat.dat_path) or '(not loaded)'}",
            f"main .dat:   {os.path.basename(self.main_dat.dat_path) or '(not loaded)'}",
            f"IDE files:   {self.stats.ide_files}",
            f"IPL files:   {self.stats.ipl_files}",
            f"COL files:   {self.stats.col_files}",
            f"Objects:     {self.stats.objects_loaded}",
            f"Instances:   {self.stats.instances}",
            f"Zones:       {len(self.zones)}",
            f"Warnings:    {len(self.stats.warnings)}",
            f"Errors:      {len(self.stats.errors)}",
        ])


def _find_sol_dir(game_root: str) -> Optional[str]:
    """Return the absolute path to the sol folder (sol/ or SOL/), or None."""
    for name in GTAGame.SOL_SUBDIRS:
        candidate = os.path.join(game_root, name)
        if os.path.isdir(candidate):
            return candidate
    return None


def detect_game_from_dat_filename(dat_path: str) -> Optional[str]: #vers 1
    """Detect which game a specific .dat file belongs to, purely from
    its own basename - for loading directly from an explicit .dat path
    (e.g. right-clicking one in the DAT Browser tree, or the standalone
    'ask for a .dat file' flow) rather than scanning a game_root folder
    the way detect_game() does. Only matches the unique main-.dat
    filenames (gta3.dat/gta_vc.dat/gta.dat/gta_sol.dat and their alt
    names) - deliberately NOT 'default.dat', since gta3/vc/sa all share
    that exact filename and matching it would risk guessing the wrong
    game."""
    name = os.path.basename(dat_path).lower()
    for game, fname in GTAGame.DAT_FILE.items():
        if name == fname.lower():
            return game
    for game, fname in GTAGame.ALT_DAT_FILE.items():
        if name == fname.lower():
            return game
    return None


def detect_game(game_root: str) -> Optional[str]: #vers 4
    """Detect which GTA game lives at game_root. Checks SA data/ and SOL sol/ subfolder."""
    data = os.path.join(game_root, "data")
    # SOL: check sol/ or SOL/ for gta_sol.dat or gtasol.dat
    sol_dir = _find_sol_dir(game_root)
    if sol_dir:
        for name in (GTAGame.DAT_FILE["sol"], GTAGame.ALT_DAT_FILE["sol"]):
            if os.path.isfile(os.path.join(sol_dir, name)):
                return GTAGame.SOL
    if os.path.isfile(os.path.join(data, "gta.dat")):       return GTAGame.SA
    if os.path.isfile(os.path.join(data, "gta_quick.dat")): return GTAGame.SA
    if os.path.isfile(os.path.join(data, "gta_vc.dat")):    return GTAGame.VC
    if os.path.isfile(os.path.join(data, "gta3.dat")):      return GTAGame.GTA3
    return None


def prescan_dat_ipls(dat_path: str, game_root: str = "", game: str = GTAGame.GTA3): #vers 1
    """Quickly parse a single .dat file's own IPL directives, without
    loading the referenced IDE/IPL files' actual contents - for a pre-
    load selection dialog (DAT Browser's 'Load with Map Workshop',
    listing sections/IPLs to enable or disable before a potentially
    slow full load of everything). Returns a list of DATEntry (path,
    abs_path, exists) for each IPL directive found."""
    if not game_root:
        game_root = os.path.normpath(os.path.join(os.path.dirname(dat_path), ".."))
    dat = DATParser(game)
    dat.parse(dat_path, game_root)
    return [e for e in dat.entries if e.directive == "IPL"]


def find_dat_file(game_root: str, game: str) -> Optional[str]: #vers 3
    """Return absolute path to the main .dat for the given game, or None.
    SOL: searches sol/ and SOL/ subfolders; tries alt name (gtasol.dat) if primary missing."""
    if game == GTAGame.SOL:
        sol_dir = _find_sol_dir(game_root)
        if not sol_dir:
            return None
        for name in (GTAGame.DAT_FILE["sol"], GTAGame.ALT_DAT_FILE["sol"]):
            c = os.path.join(sol_dir, name)
            if os.path.isfile(c):
                return c
        return None
    data = os.path.join(game_root, "data")
    name = GTAGame.DAT_FILE.get(game)
    if name:
        c = os.path.join(data, name)
        if os.path.isfile(c):
            return c
    alt = GTAGame.ALT_DAT_FILE.get(game)
    if alt:
        c = os.path.join(data, alt)
        if os.path.isfile(c):
            return c
    return None


def find_default_dat(game_root: str, game: str) -> Optional[str]: #vers 2
    """Return absolute path to the phase-1 dat (default.dat / special.dat), or None."""
    name = GTAGame.DEFAULT_DAT.get(game)
    if not name:
        return None
    if game == GTAGame.SOL:
        sol_dir = _find_sol_dir(game_root)
        if not sol_dir:
            return None
        c = os.path.join(sol_dir, name)
        # Also try case-insensitive on Linux
        if not os.path.isfile(c):
            ci = _resolve_ci(sol_dir, name)
            return ci
        return c
    c = os.path.join(game_root, "data", name)
    return c if os.path.isfile(c) else None


def integrate_gta_dat_parser(main_window) -> bool: #vers 3
    try:
        main_window.gta_world_loader = GTAWorldLoader()
        main_window.detect_gta_game  = detect_game
        main_window.find_dat_file    = find_dat_file
        if hasattr(main_window, "log_message"):
            main_window.log_message("GTA DAT/IDE/IPL parser integrated (v5, GTA3/VC/SA/SOL)")
        return True
    except Exception as e:
        if hasattr(main_window, "log_message"):
            main_window.log_message(f"DAT parser integrate error: {e}")
        return False


class GTAWorldXRef: #vers 1
    """
    Cross-reference index built from a loaded GTAWorldLoader.
    Used to produce hover tooltips on IMG Factory table entries.

    For a given stem (filename without extension):
      - model_map[stem]   -> IDEObject  (defined in some .ide)
      - txd_stems         -> set of txd names referenced by any IDE object
      - col_stems         -> set of COL file stems from COLFILE entries
      - img_stems         -> set of IMG/CDIMAGE archive stems

    Example tooltip for "landstal.dff":
      "Defined in default.ide (vehicle)
       TXD: landstal  [in gta3.img]
       COL: vehicles  [present]"
    """

    def __init__(self):
        self.model_map:  Dict[str, "IDEObject"] = {}  # stem.lower() -> IDEObject
        self.txd_stems:  set = set()                  # all txd_name.lower() values
        self.col_stems:  set = set()                  # col file stem.lower()
        self.img_stems:  set = set()                  # img/cdimage archive stems

    def find_in_imgs(self, stem: str, load_log: list,
                     game_root: str = "") -> dict: #vers 1
        """Search all IMG archives in load_log for files matching stem.
        Returns dict with keys 'dff', 'txd', 'col' → abs path or None.
        stem should be the model name without extension (e.g. 'landstal').
        Also resolves the txd_name from model_map to find the TXD archive."""
        stem_lo = stem.lower()
        result  = {'dff': None, 'txd': None, 'col': None, 'txd_name': None}

        # Get txd_name from IDE xref
        obj = self.model_map.get(stem_lo)
        txd_stem = obj.txd_name.lower() if (obj and obj.txd_name
                   and obj.txd_name.lower() not in ('null', '')) else None
        result['txd_name'] = txd_stem

        # Scan IMG archives in load log
        try:
            from apps.methods.img_core_classes import IMGFile
        except ImportError:
            return result

        img_paths = [p for _, et, p, ok in load_log
                     if ok and et in ('IMG', 'CDIMAGE') and os.path.isfile(p)]

        for img_path in img_paths:
            if result['dff'] and result['txd'] and result['col']:
                break
            try:
                arc = IMGFile(img_path)
                arc.open()
                for entry in arc.entries:
                    name_lo = entry.name.lower()
                    entry_stem = name_lo.rsplit('.', 1)[0]
                    if not result['dff'] and entry_stem == stem_lo and name_lo.endswith('.dff'):
                        result['dff'] = img_path
                    if not result['col'] and entry_stem == stem_lo and name_lo.endswith('.col'):
                        result['col'] = img_path
                    if not result['txd'] and txd_stem and entry_stem == txd_stem and name_lo.endswith('.txd'):
                        result['txd'] = img_path
            except Exception:
                continue

        return result

    def tooltip_for(self, filename: str) -> str: #vers 5
        """Return a single-line hover tooltip for an IMG entry filename, or '' if nothing known.

        Covers all IDE section types: objs, tobj, cars, peds, weap, hier, anim, tanm, txdp, 2dfx.
        Unknown DFF/TXD/COL files produce an orphan WARNING line.
        """
        if not filename or "." not in filename:
            return ""
        stem = filename.rsplit(".", 1)[0].lower()
        ext  = filename.rsplit(".", 1)[1].lower()

        #    Section label map                                                 
        _section_label = {
            "objs":  "Static Object",
            "tobj":  "Timed Object",
            "cars":  "Vehicle",
            "peds":  "Ped",
            "weap":  "Weapon",
            "hier":  "Clump/Hierarchy",
            "anim":  "Animated Object",
            "tanm":  "Timed Anim Object",
            "txdp":  "TXD Parent",
            "2dfx":  "2DFX Effect",
        }

        obj = self.model_map.get(stem)
        if obj:
            ide = obj.source_ide or "unknown.ide"
            section = obj.section or ""
            label = _section_label.get(section, obj.obj_type.capitalize() if obj.obj_type else "Object")

            parts = [f"{label} in {ide}"]

            #    TXD reference                                                 
            txd = obj.txd_name.lower() if obj.txd_name else ""
            if section == "txdp":
                # txdp: model_name = child txd, txd_name = parent txd
                parts.append(f"parent txd - {txd}.txd")
            elif txd and txd not in ("null", ""):
                if txd in self.txd_stems:
                    parts.append(f"has txd - {txd}.txd")
                else:
                    parts.append(f"missing {txd}.txd")
            elif section not in ("2dfx", "txdp"):
                parts.append("no txd")

            #    Section-specific extras                                       
            extra = obj.extra or {}

            if section == "tobj":
                # Timed objects have on/off time in flags (high byte = on, next = off)
                flags = extra.get("flags")
                if flags is not None:
                    time_on  = (flags >> 8) & 0xFF
                    time_off = (flags >> 16) & 0xFF
                    if time_on or time_off:
                        parts.append(f"active {time_on:02d}:00–{time_off:02d}:00")

            elif section == "cars":
                veh_type = extra.get("veh_type", "")
                handling = extra.get("handling_id", "")
                if veh_type:
                    parts.append(f"type - {veh_type}")
                if handling:
                    parts.append(f"handling - {handling}")
                anim = extra.get("anim_file", "")
                if anim:
                    parts.append(f"anim - {anim}")

            elif section in ("peds", "ped"):
                ped_type = extra.get("ped_type", "")
                anim = extra.get("anim_group", "")
                if ped_type:
                    parts.append(f"type - {ped_type}")
                if anim:
                    parts.append(f"anim - {anim}")

            elif section == "weap":
                anim = extra.get("anim_file", "")
                if anim:
                    parts.append(f"anim - {anim}")

            elif section in ("hier", "anim", "tanm"):
                anim = extra.get("anim_file", "")
                if anim:
                    parts.append(f"anim - {anim}")
                flags = extra.get("flags")
                if flags is not None:
                    parts.append(f"flags - {flags}")

            #    Draw distance (objs / tobj / weap / hier / anim)             
            dd = extra.get("draw_dist")
            if dd is not None and section not in ("cars", "peds", "ped", "txdp", "2dfx"):
                parts.append(f"draw dist - {dd:.0f}")

            #    COL check for DFF files                                       
            if ext == "dff":
                if stem in self.col_stems:
                    parts.append(f"has col - {stem}.col")
                else:
                    parts.append(f"missing {stem}.col")

            return ",  ".join(parts)

        #    No IDE entry found — check orphan status                          
        if ext == "txd":
            if stem in self.txd_stems:
                users = [o.model_name for o in self.model_map.values()
                         if o.txd_name and o.txd_name.lower() == stem][:5]
                suffix = " ..." if len(users) == 5 else ""
                if users:
                    return f"TXD referenced by IDE - used by: {', '.join(users)}{suffix}"
                return "TXD archive referenced by IDE model"
            return f"WARNING: Orphan TXD - {filename} not found in any .ide file"

        elif ext == "col":
            if stem in self.col_stems:
                return "COL listed in COLFILE directive"
            return f"WARNING: Orphan COL - {filename} not found in any COLFILE directive"

        elif ext == "dff":
            return f"WARNING: Orphan model - {filename} not found in any .ide file"

        return ""


def build_xref(loader: "GTAWorldLoader", game_root: str = "") -> GTAWorldXRef: #vers 2
    """Build a cross-reference index from a fully loaded GTAWorldLoader.

    For SA/SOL also scans models/coll/ for external category COL archives
    (peds.col, vehicles.col, weapons.col) and indexes their sub-model stems
    so tooltip_for() can confirm COL presence for vehicle/ped/weapon DFFs.
    """
    xref = GTAWorldXRef()

    def _stem(path: str) -> str:
        """Extract lowercase filename stem, handling both / and \\ separators."""
        name = path.replace("\\", "/").split("/")[-1]
        return name.rsplit(".", 1)[0].lower() if "." in name else name.lower()

    # Index all IDE objects by model name stem
    for obj in loader.objects.values():
        xref.model_map[obj.model_name.lower()] = obj
        if obj.txd_name and obj.txd_name.lower() not in ("null", ""):
            xref.txd_stems.add(obj.txd_name.lower())

    # Index COLFILE stems from both dat parsers
    for dat in (loader.default_dat, loader.main_dat):
        for entry in dat.col_entries():
            xref.col_stems.add(_stem(entry.path))

    # Index IMG/CDIMAGE archive stems
    for dat in (loader.default_dat, loader.main_dat):
        for entry in dat.img_entries():
            xref.img_stems.add(_stem(entry.path))

    # SA/SOL: also scan models/coll/ for external category COL archives.
    # These contain sub-models for vehicles, peds and weapons which are not
    # listed as COLFILE entries in the .dat files.
    if loader.game in (GTAGame.SA, GTAGame.SOL) and game_root:
        coll_dir = os.path.join(game_root, "models", "coll")
        if os.path.isdir(coll_dir):
            for fname in os.listdir(coll_dir):
                if not fname.lower().endswith(".col"):
                    continue
                col_path = os.path.join(coll_dir, fname)
                try:
                    with open(col_path, "rb") as f:
                        data = f.read()
                    # COL archive: scan for sub-model name headers.
                    # Each sub-model starts with "COLL"/"COL2"/"COL3"/"COL4"
                    # followed by uint32 size then 22-byte name field.
                    offset = 0
                    while offset + 32 < len(data):
                        sig = data[offset:offset + 4]
                        if sig in (b"COLL", b"COL2", b"COL3", b"COL4"):
                            name_raw = data[offset + 8: offset + 30]
                            name = name_raw.split(b"\x00")[0].decode(
                                "ascii", errors="ignore").strip().lower()
                            if name:
                                xref.col_stems.add(name)
                            # advance by reported size (uint32 at offset+4) + 8 header bytes
                            import struct
                            blk_size = struct.unpack_from("<I", data, offset + 4)[0]
                            offset += blk_size + 8
                        else:
                            offset += 1
                except Exception as e:
                    print(f"build_xref: could not scan {col_path}: {e}")

    return xref


__all__ = [
    "GTAGame", "DATEntry", "IDEObject", "IPLInstance", "ParseStats",
    "DATParser", "IDEParser", "IPLParser", "GTAWorldLoader",
    "GTAWorldXRef", "build_xref",
    "detect_game", "find_dat_file", "find_default_dat",
    "_find_sol_dir", "_resolve_ci",
    "integrate_gta_dat_parser",
]
