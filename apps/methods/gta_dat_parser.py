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
        # "path" (Aug 1 2026)
        "gta3": {"inst", "cull", "pick", "jump", "enex", "cars", "auzo", "path"},
        # "occl" added to VC (Aug 16 2026 fix)
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
    raw_line:    str  = ""

@dataclass
class PathNode: #vers 1
    """One sub-node within a path group (Aug 1 2026)"""
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
    2026)"""
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
    (confirmed both appear in the real comse.ide); model_id/
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
    """One SA "grge" section entry - a garage (Aug 1 2026)"""
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
    """One SA "enex" section entry - an entrance/exit marker (Aug 1 2026)"""
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
    """One GTA3/VC "cull" section entry - a cull zone (Aug 16 2026)"""
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
    """One "occl" section entry - an occlusion culling zone (Aug 16 2026)"""
    mid_x:      float
    mid_y:      float
    bottom_z:   float
    width_x:    float
    width_y:    float
    height:     float
    rotation:   float = 0.0
    source_ipl: str   = ""
    line_no:    int   = 0


# Audio Zone Types (Aug 20 2026) TODO; Audio zone icons do not display in viewpoint.
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

# Vice City interior numbers -> real, named areas (Aug 20 2026)
VC_INTERIOR_NAMES = {
    0: "Main World (exterior)",
    1: "Ocean View Hotel",
    2: "Diaz's Mansion / Vercetti Estate",
    3: "El Banco Corrupto Grande",
    4: "North Point Mall",
    5: "Pole Position Club",
    6: "Ken Rosenburg's Office",
    7: "Cafe Robina",
    8: "Love Fist Concert Hall",
    9: "Love Fist Recording Studio",
    10: "Shooting Range",
    11: "Apartment 3C / Greasy Choppers",
    12: "VCPD HQ / Auntie Poulet's",
    13: "Everywhere (reserved for pickups)",
    14: "Dirt Ring",
    15: "Bloodring",
    16: "Hotring",
    17: "The Malibu Club",
    18: "Print Works",
}

# San Andreas interior *file* names -> real, named areas (Aug 20 2026)
SA_INTERIOR_FILE_NAMES = {
    "abatoir": "Sindacco Abattoir", "ammun1": "Ammu-Nation",
    "carmod1": "TransFender", "fdrest1": "World of Coq",
    "gf1": "Denise's", "jetint": "Shamal interior",
    "lacs1": "Sub Urban", "lahs1b": "House",
    "mafcas": "Caligulas Casino", "mafcas2": "Penthouse Suites",
    "smashtv": "Warehouse", "svvgho1": "Hotel Suite",
    "sweets": "Sweet's House", "tsdiner": "Truck Stop",
    "wuzibet": "Wu Zi Mu's",
    "barbers": "Barber", "bdups1": "B Dup's Crack Palace",
    "carmod2": "Loco Low Co.", "carter": "Smoke's Crack Palace",
    "gf2": "Katie's", "lahs1a": "House", "lastrip": "Strip Club",
    "ryders": "Ryder's Place", "svvgho2": "Hotel Suite",
    "vghsb1": "House", "vghsb3": "House",
    "barber2": "Barber", "bdups": "B Dup's apartment",
    "bikesch": "Bike School", "brothl1": "Whore House",
    "carls": "The Johnson House", "carmod3": "Wheel Arch Angels",
    "changer": "Wardrobe", "cssprt": "Pro-Laps",
    "drives": "Driving School", "drives2": "Driving School",
    "genotb": "Inside Track Betting", "gf3": "Helena's",
    "lahsb4": "House", "oglocs": "OG Loc's",
    "paper": "Planning Department", "pdomes": "The Pleasure Domes",
    "pdomes2": "The Pleasure Domes", "police3": "LVPD HQ",
    "sexshop": "Sex Shop", "s1test": "Middle of nowhere",
    "strip2": "Strip Club", "studio": "Blastin' Fools Records studio",
    "tatto3": "Tattoo Parlor",
    "ammun2": "Ammu-Nation", "diner1": "Diner",
    "dirbike": "Dirt Stadium", "gf4": "Michelle's",
    "lahs2a": "House", "lahss6": "House", "sfhsm2": "House",
    "x711s2": "24-7",
    "csdesgn": "Victim", "diner2": "Diner", "fdpiza": "Pizza Stack",
    "gang": "Vagos Gang House", "gf5": "Barbara's",
    "gym1": "Ganton Gym", "lacrak": "Crack Den", "lahsb3": "House",
    "maddogs": "Madd Dogg's Crib", "mddogs": "Madd Dogg's Crib",
    "sfhsb1": "House", "svhot1": "Hotel Suite", "vghsm2": "House",
    "ammun3": "Ammu-Nation", "ammun5": "Ammu-Nation",
    "brothel": "Whore House", "gf6": "Millie's",
    "gym2": "Cobra Marital Arts", "lahsb1": "House",
    "police1": "LSPD HQ", "rcplay": "Zero's RC Shop",
    "rest2": "Secret Valley", "sfhsb2": "House", "sfhss2": "House",
    "svcunt": "Safe House", "svsfbg": "Safe House",
    "svsfsm": "Safe House", "x7_11s": "24-7",
    "8track": "8-Track Stadium", "ammun4": "Ammu-Nation",
    "gym3": "Below the Belt Gym", "lahsb2": "House",
    "oftest": "Middle of nowhere",
    "burhous": "Colonel Fuhrberger's", "sfhss1": "House",
    "svlamd": "Safe House",
    "fdchick": "Cluckin' Bell", "lahs2b": "House", "sfhsb3": "House",
    "svgnmt2": "Motel room", "svvgmd": "Safe House",
    "deshous": "Abandoned AC tower", "fdburg": "Burger Shot",
    "police2": "SFPD HQ", "svgnmt1": "Motel room",
    "tricas": "The Four Dragon's", "svsfmd": "Safe House",
    "vghsm3": "House", "x711s3": "24-7",
    "bar2": "Bar", "svlasm": "Safe House",
    "barber3": "Barber", "casino2": "Casino Floor",
    "moroom": "Motel room", "svlabig": "Safe House",
    "csexl": "Didier Sachs", "airpor2": "Los Santos International Airport",
    "airport": "Francis Intl. Airport",
    "cschp": "Binco", "motel1": "Jefferson Motel",
    "sfhsm1": "House", "vghss1": "House", "vgshm2": "House",
    "vgshm3": "House", "vgshs2": "House",
    "tattoo": "Tattoo Parlor", "x7_11c": "24-7",
    "bar1": "Bar", "damin": "Generator Hall",
    "fddonut": "Rusty Brown's", "tatto2": "Tattoo Parlor",
    "x7_11d": "24-7",
    "atriume": "Atrium", "atriumx": "Attrium", "clothgp": "Zip",
    "genwrhs": "Warehouse", "ufobar": "Lil' Probe Inn",
    "x7_11b": "24-7",
}


@dataclass
class AuzoEntry: #vers 1
    """One "auzo" section entry - a San Andreas audio zone (Aug 20 2026)"""
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
    """One corner point of a real SA water.dat shape (Aug 20 2026)"""
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
    (Aug 20 2026)"""
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
    """Parse a real SA water.dat file (Aug 20 2026)"""
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
    (Aug 20 2026)"""
    height: float


@dataclass
class WaterProFile: #vers 2
    """A fully parsed GTA III/VC/PS2-LC/SOL waterpro.dat (Aug 20 2026)"""
    level_count:  int
    levels:       List[WaterProLevel]
    grid_width:   int
    unk_block:    bytes
    visible_map:  List[List[int]]   # [row][col], grid_width x grid_width
    physical_map: List[List[int]]   # [row][col], (2*grid_width) x (2*grid_width)
    source_file:  str = ""


def _detile_sol_grid(raw: bytes, grid_width: int, map_w: int = 6) -> List[List[int]]: #vers 1
    """ de-tiling fix for SOL's own waterpro.dat grid layout (Aug
    20 2026)"""
    tile_w = grid_width // map_w
    out = [[0] * grid_width for _ in range(grid_width)]
    for tile_idx in range(map_w * map_w):
        tile_col = tile_idx % map_w
        tile_row = tile_idx // map_w
        base = tile_idx * tile_w * tile_w
        for r in range(tile_w):
            row_off = base + r * tile_w
            py = tile_row * tile_w + r
            px0 = tile_col * tile_w
            out[py][px0:px0 + tile_w] = raw[row_off:row_off + tile_w]
    return out


def parse_waterpro_dat(path: str) -> Optional[WaterProFile]: #vers 3
    """Parse a real GTA III/VC/PS2-LC/SOL waterpro.dat (Aug 20 2026)"""
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
        physical_bytes = data[header_size + vis_size:header_size + vis_size + phys_size]
        is_tiled = (grid_width % 6 == 0)
        if is_tiled:
            visible_map = _detile_sol_grid(visible_bytes, grid_width, map_w=6)
            physical_map = _detile_sol_grid(physical_bytes, phys_width, map_w=6)
        else:
            visible_map = [list(visible_bytes[r * grid_width:(r + 1) * grid_width])
                           for r in range(grid_width)]
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
    a given GTA game (Aug 20 2026)"""
    index: int
    row: int
    col: int
    min_x: float
    min_y: float
    max_x: float
    max_y: float


# Real, confirmed per-game radar grid presets (Aug 20 2026)
RADAR_GRID_PRESETS = {
    'gta3': {'grid_size': 4000.0, 'tiles_per_side': 8},
    'vc':   {'grid_size': 4000.0, 'tiles_per_side': 8},
    'sa':   {'grid_size': 6000.0, 'tiles_per_side': 12},
    'sol':  {'grid_size': 12000.0, 'tiles_per_side': 36},
}

# Water's own real grid size, separate from RADAR_GRID_PRESETS (Sep 5 2026)
WATER_GRID_PRESETS = {
    'sol': {'grid_size': 24576.0, 'tiles_per_side': 6},
}

# Water grid size ladder.
#
#   tiles_per_side=1 ->  64x64  /  128x128   (vanilla VC/GTA3)
#   tiles_per_side=2 -> 128x128 /  256x256
#   tiles_per_side=3 -> 192x192 /  384x384
#   tiles_per_side=4 -> 256x256 /  512x512
#   tiles_per_side=5 -> 320x320 /  640x640
#   tiles_per_side=6 -> 384x384 /  768x768   (SOL, exactly)
#   tiles_per_side=7 -> 448x448 /  896x896   (1 stage beyond SOL)
#   tiles_per_side=8 -> 512x512 / 1024x1024  (2 stages beyond SOL)
#

_WATER_CHUNK_CELLS = 64          # real cells per chunk, one side (visible)
_WATER_CHUNK_WORLD_UNITS = 4096.0  # real world units per chunk, one side

WATER_TILE_SIZE_PRESETS = {
    n: {
        'tiles_per_side': n,
        'grid_size': n * _WATER_CHUNK_WORLD_UNITS,
        'visible_width': n * _WATER_CHUNK_CELLS,
        'physical_width': n * _WATER_CHUNK_CELLS * 2,
        'label': label,
    }
    for n, label in {
        1: "Vanilla (VC / GTA3)",
        2: "Small",
        3: "Slightly Larger",
        4: "Larger",
        5: "Extra Large",
        6: "SOL (6x6 real tiles)",
        7: "Beyond SOL +1",
        8: "Beyond SOL +2",
    }.items()
}


def get_water_size_preset(tiles_per_side: int) -> dict: #vers 1
    """Look up (or generate, for any tiles_per_side beyond the 8
    named stages above) a water grid size preset - see WATER_TILE_
    SIZE_PRESETS' own module-level comment for the full derivation.
    Works for any positive integer, not just the 8 pre-built ones,
    since the underlying math is just tiles_per_side * the real,
    confirmed 64-unit chunk."""
    if tiles_per_side in WATER_TILE_SIZE_PRESETS:
        return WATER_TILE_SIZE_PRESETS[tiles_per_side]
    return {
        'tiles_per_side': tiles_per_side,
        'grid_size': tiles_per_side * _WATER_CHUNK_WORLD_UNITS,
        'visible_width': tiles_per_side * _WATER_CHUNK_CELLS,
        'physical_width': tiles_per_side * _WATER_CHUNK_CELLS * 2,
        'label': f"{tiles_per_side}x{tiles_per_side} tiles",
    }


# SA's own ladder is shaped differently (Sep 5 2026)
#
#   n=1 ->  16  (Finest)
#   n=2 ->  32  (Small)
#   n=3 ->  48
#   n=4 ->  64  (matches VC/GTA3's own real grid chunk, coincidentally)
#   n=6 ->  96  (Slightly Larger)
#   n=8 -> 128  (Larger)
#   n=16 -> 256 (Extra Large)
_SA_SNAP_UNIT = 16.0   # real world units - see this block's own comment

SA_WATER_SNAP_PRESETS = {
    n: {'snap_size': n * _SA_SNAP_UNIT, 'label': label}
    for n, label in {
        1: "Finest",
        2: "Small",
        3: "Medium-Small",
        4: "Medium (VC/GTA3 chunk size)",
        6: "Slightly Larger",
        8: "Larger",
        16: "Extra Large",
    }.items()
}


def get_sa_water_snap_preset(n: int) -> dict: #vers 1
    """Look up (or generate, for any n beyond the 7 named stages
    above) an SA water snap-size preset - see SA_WATER_SNAP_PRESETS'
    own module-level comment for the full derivation. Works for any
    positive integer, since the underlying math is just n * the
    real, most-commonly-observed 16-unit snap size."""
    if n in SA_WATER_SNAP_PRESETS:
        return SA_WATER_SNAP_PRESETS[n]
    return {'snap_size': n * _SA_SNAP_UNIT, 'label': f"{n * _SA_SNAP_UNIT:.0f} units"}


def compute_radar_grid(grid_size: float = 6000.0, tiles_per_side: int = 12,
                       center_x: float = 0.0, center_y: float = 0.0) -> List[RadarTile]: #vers 2
    """Compute the real world-space bounding box for every tile in a
    radar grid (Aug 20 2026)"""
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
    """One recorded frame from a real GTA III CHASE*.DAT file (Aug 19 2026)"""
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
    (Aug 19 2026)"""
    area_id: int = 0
    node_id: int = 0


@dataclass
class TrackWaypoint: #vers 1
    """One waypoint from a real GTA III/VC/SA train track file
    (data/paths/tracks.dat, tracks2.dat, etc - Aug 17 2026)"""
    x: float
    y: float
    z: float
    source_file: str = ""
    index:       int = 0
    flag:        Optional[int] = None


@dataclass
class IPLLoadResult:
    """Result of one on-demand IPL load (GTAWorldLoader.load_ipl_by_
    name)"""
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
        """ WATER directive entries (Aug 20 2026) - the generic
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

        # GTA III's own IDE-embedded path groups (Aug 16 2026)
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
        doc."""
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
        comse.ide field-for-field."""
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
                # HIER's real, published format (Aug 20 2026)
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
                # two of my own real IDE samples - "10744,
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
                # (Aug 1 2026)
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
    """Parser for binary-format IPL data (see detect_ipl_format)."""

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
    only (Aug 20 2026)"""
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
        self._current_inst_layout = game

    def parse(self, ipl_path: str, layout_override: str = None) -> bool: #vers 3
        """layout_override (Sep 5 2026)"""
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
        # Layout override applies for just this one parse() call - see
        # this method's own docstring above for the real reason.
        effective_layout = layout_override or self.game
        self._valid = GTAGame.IPL_SECTIONS.get(effective_layout, GTAGame.IPL_SECTIONS[GTAGame.GTA3])
        self._current_inst_layout = effective_layout

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
        integers (Aug 1 2026)"""
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

        X/Y/Z scale conversion (Aug 1 2026)"""
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
        """One SA "grge" (garage) line - eleven fields, my real example data and confirmed
        SannyBuilder forum documentation: X1,Y1,Z1, frontX,frontY, X2,Y2,Z2, DoorType,
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
        """One SA "enex" (entrance/exit) line - eighteen fields)"""
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

    def _parse_inst(self, line: str, source: str, lineno: int) -> Optional[IPLInstance]: #vers 4
        try:
            parts = [p.strip() for p in line.split(",")]
            layout = getattr(self, '_current_inst_layout', self.game)
            if layout in (GTAGame.SA, GTAGame.SOL):
                if len(parts) < 10:
                    return None
                inst = IPLInstance(
                    model_id=int(parts[0]), model_name=parts[1], interior=int(parts[2]),
                    pos_x=float(parts[3]), pos_y=float(parts[4]), pos_z=float(parts[5]),
                    rot_x=float(parts[6]), rot_y=float(parts[7]),
                    rot_z=float(parts[8]), rot_w=float(parts[9]),
                    lod_index=int(parts[10]) if len(parts) > 10 else -1,
                    source_ipl=source, line_no=lineno)
            elif layout == GTAGame.VC:
                # VC: id, model, interior, px,py,pz, sx,sy,sz, rx,ry,rz,rw -
                # confirmed empirically (not guessed) against a real line

                if len(parts) < 13:
                    return None
                inst = IPLInstance(
                    model_id=int(parts[0]), model_name=parts[1], interior=int(parts[2]),
                    pos_x=float(parts[3]), pos_y=float(parts[4]), pos_z=float(parts[5]),
                    scale_x=float(parts[6]), scale_y=float(parts[7]), scale_z=float(parts[8]),
                    rot_x=float(parts[9]), rot_y=float(parts[10]),
                    rot_z=float(parts[11]), rot_w=float(parts[12]),
                    source_ipl=source, line_no=lineno)
            else:
                # GTA3: id, model, px, py, pz, sx, sy, sz, rx, ry, rz, rw

                if len(parts) < 12:
                    return None
                inst = IPLInstance(
                    model_id=int(parts[0]), model_name=parts[1], interior=0,
                    pos_x=float(parts[2]), pos_y=float(parts[3]), pos_z=float(parts[4]),
                    scale_x=float(parts[5]), scale_y=float(parts[6]), scale_z=float(parts[7]),
                    rot_x=float(parts[8]), rot_y=float(parts[9]),
                    rot_z=float(parts[10]), rot_w=float(parts[11]),
                    source_ipl=source, line_no=lineno)

            # Diagnostic (Sep 5 2026)
            mag2 = inst.rot_x**2 + inst.rot_y**2 + inst.rot_z**2 + inst.rot_w**2
            if not (0.9 < mag2 < 1.1):
                self.stats.warnings.append(
                    f"{source}:{lineno} - {inst.model_name} rotation quaternion "
                    f"magnitude^2={mag2:.3f} (should be ~1.0) - parsed as "
                    f"'{layout}' layout (self.game='{self.game}'); likely the "
                    f"wrong field layout for this line")
            return inst
        except (ValueError, IndexError):
            self.stats.warnings.append(f"Skipped INST line {lineno}: {line[:70]}")
        return None

    def _parse_zone(self, line: str, source: str, lineno: int) -> Optional[Dict]: #vers 2
        """Parse one "zone" section line - Name, Type, MinX/Y/Z,
        MaxX/Y/Z, Island[, TextKey]. Now carries source_ipl/line_no
        (Aug 16 2026 fix)"""
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

    def _parse_cull(self, line: str, source: str, lineno: int) -> Optional[CullEntry]: #vers 4
        """Parse one "cull" section line.

        III/VC: CenterX/Y/Z, X1/Y1/Z1, X2/Y2/Z2, Flags,
        WantedLevelDrop (11 fields, two genuine corner points) - Aug
        16 2026)"""
        try:
            p = [x.strip() for x in line.split(",")]
            if len(p) < 9:
                return None
            cx, cy, cz = float(p[0]), float(p[1]), float(p[2])
            #  fix (Aug 21 2026)
            if self.game in (GTAGame.SA, GTAGame.SOL):
                xskew, length, bottom = float(p[3]), float(p[4]), float(p[5])
                width, yskew, top = float(p[6]), float(p[7]), float(p[8])
                corners = [
                    (cx - width + xskew, cy + length + yskew),
                    (cx + width + xskew, cy + length - yskew),
                    (cx - width - xskew, cy - length + yskew),
                    (cx + width - xskew, cy - length - yskew),
                ]
                xs = [c[0] for c in corners]
                ys = [c[1] for c in corners]
                return CullEntry(
                    center_x=cx, center_y=cy, center_z=cz,
                    x1=min(xs), y1=min(ys), z1=bottom,
                    x2=max(xs), y2=max(ys), z2=top,
                    flags=int(float(p[9])) if len(p) > 9 else 0,
                    wanted_level_drop=0,
                    source_ipl=source, line_no=lineno)
            return CullEntry(
                center_x=cx, center_y=cy, center_z=cz,
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
        two) and verified field-for-field against my real
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
        # GTA III's own IDE-embedded path groups (Aug 16 2026)
        self.ide_paths:  List[IDEPathGroup]   = []
        self.grges:      List[GrgeEntry]       = []
        self.enexes:     List[EnexEntry]       = []
        self.zones:      List[Dict]           = []
        self.culls:      List[CullEntry]      = []
        self.occls:      List[OcclEntry]      = []
        self.auzos:      List[AuzoEntry]      = []
        # Train track waypoints (Aug 17 2026)
        self.tracks:     Dict[str, List[TrackWaypoint]] = {}
        # SA vehicle/ped path node data (Aug 19 2026)
        # load_sa_nodes, SA only.
        self.sa_nodes:   Dict[int, object] = {}
        # Police roadblock placements (Aug 19 2026)
        self.sa_roadblocks: List[RoadblockEntry] = []
        #  water plane shapes (Aug 20 2026)
        self.water_shapes: List[object] = []
        # III/VC's own binary waterpro.dat (Aug 20 2026)
        self.waterpro: Optional[object] = None
        # GTA III chase-scene car paths (Aug 19 2026)
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
        # MooMapper comparison: it lists every available IPL
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
        # IPL stems (lowercase, no extension) to parse using VC's own
        # section set and instance field layout instead of self.game's
        # default (Sep 5 2026)
        self.vc_layout_ipl_stems: set = set()

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
            # SA-only (Aug 19 2026)
            self.load_sa_nodes(game_root, data_dir)
            self.load_sa_roadblox(data_dir)
            self.load_water_dat(data_dir)
        if self.game == GTAGame.GTA3:
            # GTA III-only (Aug 19 2026)
            self.load_chase_dat(data_dir)
        if self.game in (GTAGame.GTA3, GTAGame.VC, GTAGame.SOL):
            # III/VC/SOL - waterpro.dat is a completely different,
            # binary format specific to these games (SA uses its own
            # text water.dat instead, see load_water_dat just above;
            # SOL is built on the VC engine, so it uses the same
            # binary format VC does, not SA's).
            waterpro_dir = os.path.join(game_root, "data") if self.game == GTAGame.SOL else data_dir
            self.load_waterpro_dat(waterpro_dir)
        return True

    def load_tracks_dat(self, data_dir: str): #vers 2
        """Load train track waypoints, and (Aug 19 2026)"""
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
        19 2026)"""
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
        paths/ROADBLOX.DAT (Aug 19 2026) Same case-insensitive "paths"
        subdirectory lookup convention already established by
        load_tracks_dat/load_sa_nodes."""
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

    def load_water_dat(self, data_dir: str = ''): #vers 2
        """Load SA's real water.dat. Finds the REAL path from gta.
        dat's own already-parsed WATER directive entries first (main_
        dat.water_entries()) - the directive is real, documented (per
        GTAMods' own gta.dat page: "these entries link to external
        water plane placement files"). Falls back to the real,
        standard data_dir/water.dat path (Aug 20 2026)"""
        entries = getattr(self.main_dat, 'water_entries', lambda: [])()
        for entry in entries:
            if entry.exists:
                shapes = parse_water_dat(entry.abs_path)
                if shapes:
                    self.water_shapes = shapes
                    self.load_log.append(("water", "WATER", entry.abs_path, True))
                    return
        if data_dir:
            # Case-insensitive fix (Aug 20 2026)
            fallback_path = _resolve_ci(data_dir, "water.dat")
            if fallback_path:
                shapes = parse_water_dat(fallback_path)
                if shapes:
                    self.water_shapes = shapes
                    self.load_log.append(("water", "WATER", fallback_path, True))
                    return

    def load_waterpro_dat(self, data_dir: str = ''): #vers 4
        """Load GTA III/VC's own binary waterpro.dat.

         fix (Aug 20 2026)"""
        entries = getattr(self.main_dat, 'water_entries', lambda: [])()
        for entry in entries:
            if entry.exists:
                result = parse_waterpro_dat(entry.abs_path)
                if result is not None:
                    self.waterpro = result
                    self.load_log.append(("water", "WATERPRO", entry.abs_path, True))
                    return
        if data_dir:

            fallback_path = _resolve_ci(data_dir, "waterpro.dat")
            if fallback_path:
                result = parse_waterpro_dat(fallback_path)
                if result is not None:
                    self.waterpro = result
                    self.load_log.append(("water", "WATERPRO", fallback_path, True))
                    return

    def load_chase_dat(self, data_dir: str): #vers 1
        """Load every real GTA III CHASE*.DAT chase-scene car path
        found in data/paths/ (Aug 19 2026)"""
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
        """Parse one CHASE*.DAT file."""
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
        against my real SA tracks.dat/tracks2/3/4.dat samples -
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
        """Actually parse and load one specific IPL's content."""
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
        layout_override = GTAGame.VC if ipl_stem.lower() in self.vc_layout_ipl_stems else None
        ok = parser.parse(entry.abs_path, layout_override=layout_override)
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
        ipl_stem = os.path.splitext(os.path.basename(entry.abs_path))[0].lower()
        layout_override = GTAGame.VC if ipl_stem in self.vc_layout_ipl_stems else None
        ok     = parser.parse(entry.abs_path, layout_override=layout_override)
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
        them"). Aug 14 2026)"""
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

    def resolve_lod_pairs(self) -> Dict[int, IPLInstance]: #vers 3
        """Resolve each instance's paired LOD counterpart, where one
        exists. Two detection strategies, both run for every game and
        combined (Aug 1 2026, widened from being mutually exclusive
        by game."""
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

        # Strategy 2: "LOD" name-prefix/suffix matching (Sep 5 2026)
        pos_tol = 0.5   # units - allows tiny float/rounding differences
        by_name: Dict[str, list] = {}
        for inst in self.instances:
            n = inst.model_name.lower()
            if not (n.startswith('lod') or n.endswith('lod')):
                by_name.setdefault(n, []).append(inst)
        for inst in self.instances:
            n = inst.model_name.lower()
            if n.startswith('lod'):
                base_name = n[3:]
            elif n.endswith('lod'):
                base_name = n[:-3]
            else:
                continue
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


def optimize_dat_load_order(dat_path, entries): #vers 1
    """Rewrite a real, already-loaded .dat file's own real IDE/IPL/
    COLFILE/IMG directive lines, grouped and sorted the same real way
    Rockstar's own real gta_vc.dat file explicitly documents doing it
    itself (Aug 21 2026)"""
    try:
        with open(dat_path, 'r', encoding='ascii', errors='ignore') as f:
            original_lines = f.readlines()
    except Exception as e:
        return False, f"Couldn't read {dat_path}: {e}"

    reorderable = {'IDE', 'IPL', 'COLFILE', 'IMG', 'CDIMAGE'}
    by_type = {}
    type_order = []
    for entry in entries:
        d = entry.directive.upper()
        if d not in reorderable:
            continue
        if d not in by_type:
            by_type[d] = []
            type_order.append(d)
        by_type[d].append(entry)

    for d in type_order:
        by_type[d].sort(key=lambda e: (
            os.path.dirname(e.path).replace('\\', '/').lower(),
            os.path.basename(e.path).lower()))

    new_lines = []
    in_directive_block = False
    consumed = {d: 0 for d in type_order}
    for raw in original_lines:
        stripped = raw.split('#')[0].strip()
        parts = stripped.split()
        directive = parts[0].upper() if parts else ''
        if directive in reorderable and directive in by_type:
            idx = consumed[directive]
            if idx < len(by_type[directive]):
                entry = by_type[directive][idx]
                if directive == 'COLFILE':
                    new_lines.append(f"COLFILE  {entry.extra}  {entry.path}\n")
                else:
                    new_lines.append(f"{directive}  {entry.path}\n")
                consumed[directive] += 1
                continue
        new_lines.append(raw)

    backup_path = dat_path + '.bak'
    if not os.path.isfile(backup_path):
        try:
            with open(dat_path, 'r', encoding='ascii', errors='ignore') as f:
                original = f.read()
            with open(backup_path, 'w', encoding='ascii', errors='ignore') as f:
                f.write(original)
        except Exception as e:
            return False, f"Couldn't write backup for {dat_path}: {e}"

    try:
        with open(dat_path, 'w', encoding='ascii', errors='ignore') as f:
            f.writelines(new_lines)
    except Exception as e:
        return False, f"Couldn't write {dat_path}: {e}"

    total = sum(len(v) for v in by_type.values())
    return True, (f"Reordered {total} directive line(s) across "
                  f"{len(type_order)} type(s) in {os.path.basename(dat_path)}")


def convert_inst_fields(parts, from_game, to_game): #vers 1
    """Convert one already-split INST line's own real fields between
    VC and SA/SOL layouts (Aug 21 2026)"""
    sa_like = (GTAGame.SA, GTAGame.SOL)
    if from_game in sa_like and to_game == GTAGame.VC:
        if len(parts) < 10:
            return None
        id_, model, interior, px, py, pz = parts[0:6]
        rx, ry, rz, rw = parts[6:10]
        return [id_, model, interior, px, py, pz,
                "1.0", "1.0", "1.0", rx, ry, rz, rw]
    if from_game == GTAGame.VC and to_game in sa_like:
        if len(parts) < 13:
            return None
        id_, model, interior, px, py, pz = parts[0:6]
        rx, ry, rz, rw = parts[9:13]
        return [id_, model, interior, px, py, pz, rx, ry, rz, rw, "-1"]
    return None


def repair_zero_scale_inst_fields(parts): #vers 1
    """Fix a real, broken VC-layout INST line whose own real scale
    fields (index 6,7,8) are (0,0,0) instead of the real, standard
    (1,1,1) - a real zero scale collapses the object to nothing in-
    game (Aug 21 2026)"""
    if len(parts) < 13:
        return parts
    try:
        sx, sy, sz = float(parts[6]), float(parts[7]), float(parts[8])
    except ValueError:
        return parts
    if sx == 0.0 and sy == 0.0 and sz == 0.0:
        fixed = list(parts)
        fixed[6], fixed[7], fixed[8] = "1.0", "1.0", "1.0"
        return fixed
    return parts


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
