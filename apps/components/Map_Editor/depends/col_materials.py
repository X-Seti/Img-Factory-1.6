#this belongs in apps/methods/col_materials.py - Version: 1
# X-Seti - March29 2026 - IMG Factory 1.6 - COL Material Definitions
# Reference: DragonFF col_materials.py (Parik, GPL-2.0+)
"""
COL Material Definitions
Covers GTA III / VC (COL1) and GTA SA (COL2/3) material/surface IDs.

Usage:
    from apps.methods.col_materials import (
        get_material_name, get_material_colour, get_material_group,
        get_materials_for_version, COLGame, COL_PRESET_GROUP
    )

##Functions list -
# get_material_name
# get_material_colour
# get_material_group
# get_materials_for_version
# get_vehicle_presets
# material_id_from_name

##Data list -
# COL_PRESET_GROUP   - group id -> [name, hex_colour]
# COL_PRESET_SA      - full SA material list (group, flag, id, name, procedural)
# COL_PRESET_VC      - full VC material list
"""

from enum import IntEnum
from typing import List, Tuple, Optional, Dict


# --------------------------------------------------
# GROUPS  (shared across all games)
# --------------------------------------------------

class COLGroup(IntEnum):
    ROAD     =  0
    CONCRETE =  1
    GRAVEL   =  2
    GRASS    =  3
    DIRT     =  4
    SAND     =  5
    GLASS    =  6
    WOOD     =  7
    METAL    =  8
    ROCK     =  9
    BUSHES   = 10
    WATER    = 11
    MISC     = 12
    VEHICLE  = 13


COL_PRESET_GROUP: Dict[int, List[str]] = {
    0:  ["Road",     "303030"],
    1:  ["Concrete", "909090"],
    2:  ["Gravel",   "645E53"],
    3:  ["Grass",    "92C032"],
    4:  ["Dirt",     "775F40"],
    5:  ["Sand",     "E7E17E"],
    6:  ["Glass",    "A7E9FC"],
    7:  ["Wood",     "936944"],
    8:  ["Metal",    "BFC8D5"],
    9:  ["Rock",     "AFAAA0"],
    10: ["Bushes",   "2EA563"],
    11: ["Water",    "6493E1"],
    12: ["Misc",     "F1AB07"],
    13: ["Vehicle",  "FFD4FD"],
}


# --------------------------------------------------
# GAME SELECTOR
# --------------------------------------------------

class COLGame(IntEnum):
    GTA3 = 1   # Uses VC list (same materials)
    VC   = 2
    SA   = 3


# --------------------------------------------------
# SAN ANDREAS — surface flags
# --------------------------------------------------

class COLFlagSA(IntEnum):
    NONE    =  0
    BODY    =  0
    HOOD    =  1
    BOOT    =  2
    BUMP_F  =  3
    BUMP_R  =  4
    DOOR_FL =  5
    DOOR_FR =  6
    DOOR_RL =  7
    DOOR_RR =  8
    WING_FL =  9
    WING_FR = 10
    WING_RL = 11
    WING_RR = 12
    WIND_SH = 19


# Tuple layout: (group, flag, material_id, name, is_procedural)
COL_PRESET_SA: List[Tuple] = [
    # Normal Materials (0–73)
    (COLGroup.ROAD,     COLFlagSA.NONE,     0,  "Default",              False),
    (COLGroup.ROAD,     COLFlagSA.NONE,     1,  "Tarmac",               False),
    (COLGroup.ROAD,     COLFlagSA.NONE,     2,  "Tarmac (worn)",        False),
    (COLGroup.ROAD,     COLFlagSA.NONE,     3,  "Tarmac (very worn)",   False),
    (COLGroup.CONCRETE, COLFlagSA.NONE,     4,  "Pavement",             False),
    (COLGroup.CONCRETE, COLFlagSA.NONE,     5,  "Pavement (cracked)",   False),
    (COLGroup.GRAVEL,   COLFlagSA.NONE,     6,  "Gravel",               False),
    (COLGroup.CONCRETE, COLFlagSA.NONE,     7,  "Concrete (cracked)",   False),
    (COLGroup.CONCRETE, COLFlagSA.NONE,     8,  "Painted Ground",       False),
    (COLGroup.GRASS,    COLFlagSA.NONE,     9,  "Grass (short, lush)",  False),
    (COLGroup.GRASS,    COLFlagSA.NONE,    10,  "Grass (medium, lush)", False),
    (COLGroup.GRASS,    COLFlagSA.NONE,    11,  "Grass (long, lush)",   False),
    (COLGroup.GRASS,    COLFlagSA.NONE,    12,  "Grass (short, dry)",   False),
    (COLGroup.GRASS,    COLFlagSA.NONE,    13,  "Grass (medium, dry)",  False),
    (COLGroup.GRASS,    COLFlagSA.NONE,    14,  "Grass (long, dry)",    False),
    (COLGroup.GRASS,    COLFlagSA.NONE,    15,  "Golf Grass (rough)",   False),
    (COLGroup.GRASS,    COLFlagSA.NONE,    16,  "Golf Grass (smooth)",  False),
    (COLGroup.GRASS,    COLFlagSA.NONE,    17,  "Steep Slidy Grass",    False),
    (COLGroup.ROCK,     COLFlagSA.NONE,    18,  "Steep Cliff",          False),
    (COLGroup.DIRT,     COLFlagSA.NONE,    19,  "Flower Bed",           False),
    (COLGroup.GRASS,    COLFlagSA.NONE,    20,  "Meadow",               False),
    (COLGroup.DIRT,     COLFlagSA.NONE,    21,  "Waste Ground",         False),
    (COLGroup.DIRT,     COLFlagSA.NONE,    22,  "Woodland Ground",      False),
    (COLGroup.BUSHES,   COLFlagSA.NONE,    23,  "Vegetation",           False),
    (COLGroup.DIRT,     COLFlagSA.NONE,    24,  "Mud (wet)",            False),
    (COLGroup.DIRT,     COLFlagSA.NONE,    25,  "Mud (dry)",            False),
    (COLGroup.DIRT,     COLFlagSA.NONE,    26,  "Dirt",                 False),
    (COLGroup.DIRT,     COLFlagSA.NONE,    27,  "Dirt Track",           False),
    (COLGroup.SAND,     COLFlagSA.NONE,    28,  "Sand (deep)",          False),
    (COLGroup.SAND,     COLFlagSA.NONE,    29,  "Sand (medium)",        False),
    (COLGroup.SAND,     COLFlagSA.NONE,    30,  "Sand (compact)",       False),
    (COLGroup.SAND,     COLFlagSA.NONE,    31,  "Sand (arid)",          False),
    (COLGroup.SAND,     COLFlagSA.NONE,    32,  "Sand (more)",          False),
    (COLGroup.SAND,     COLFlagSA.NONE,    33,  "Sand (beach)",         False),
    (COLGroup.CONCRETE, COLFlagSA.NONE,    34,  "Concrete (beach)",     False),
    (COLGroup.ROCK,     COLFlagSA.NONE,    35,  "Rock (dry)",           False),
    (COLGroup.ROCK,     COLFlagSA.NONE,    36,  "Rock (wet)",           False),
    (COLGroup.ROCK,     COLFlagSA.NONE,    37,  "Rock (cliff)",         False),
    (COLGroup.WATER,    COLFlagSA.NONE,    38,  "Water (riverbed)",     False),
    (COLGroup.WATER,    COLFlagSA.NONE,    39,  "Water (shallow)",      False),
    (COLGroup.DIRT,     COLFlagSA.NONE,    40,  "Corn Field",           False),
    (COLGroup.BUSHES,   COLFlagSA.NONE,    41,  "Hedge",                False),
    (COLGroup.WOOD,     COLFlagSA.NONE,    42,  "Wood (crates)",        False),
    (COLGroup.WOOD,     COLFlagSA.NONE,    43,  "Wood (solid)",         False),
    (COLGroup.WOOD,     COLFlagSA.NONE,    44,  "Wood (thin)",          False),
    (COLGroup.GLASS,    COLFlagSA.NONE,    45,  "Glass",                False),
    (COLGroup.GLASS,    COLFlagSA.NONE,    46,  "Glass Window (large)", False),
    (COLGroup.GLASS,    COLFlagSA.NONE,    47,  "Glass Window (small)", False),
    (COLGroup.MISC,     COLFlagSA.NONE,    48,  "Empty 1",              False),
    (COLGroup.MISC,     COLFlagSA.NONE,    49,  "Empty 2",              False),
    (COLGroup.METAL,    COLFlagSA.NONE,    50,  "Garage Door",          False),
    (COLGroup.METAL,    COLFlagSA.NONE,    51,  "Thick Metal Plate",    False),
    (COLGroup.METAL,    COLFlagSA.NONE,    52,  "Scaffold Pole",        False),
    (COLGroup.METAL,    COLFlagSA.NONE,    53,  "Lamp Post",            False),
    (COLGroup.METAL,    COLFlagSA.NONE,    54,  "Metal Gate",           False),
    (COLGroup.METAL,    COLFlagSA.NONE,    55,  "Metal Chain Fence",    False),
    (COLGroup.METAL,    COLFlagSA.NONE,    56,  "Girder",               False),
    (COLGroup.METAL,    COLFlagSA.NONE,    57,  "Fire Hydrant",         False),
    (COLGroup.METAL,    COLFlagSA.NONE,    58,  "Container",            False),
    (COLGroup.METAL,    COLFlagSA.NONE,    59,  "News Vendor",          False),
    (COLGroup.MISC,     COLFlagSA.NONE,    60,  "Wheelbase",            False),
    (COLGroup.MISC,     COLFlagSA.NONE,    61,  "Cardboard Box",        False),
    (COLGroup.MISC,     COLFlagSA.NONE,    62,  "Ped",                  False),
    (COLGroup.METAL,    COLFlagSA.NONE,    63,  "Car (body)",           False),
    (COLGroup.METAL,    COLFlagSA.NONE,    64,  "Car (panel)",          False),
    (COLGroup.METAL,    COLFlagSA.NONE,    65,  "Car (moving)",         False),
    (COLGroup.MISC,     COLFlagSA.NONE,    66,  "Transparent Cloth",    False),
    (COLGroup.MISC,     COLFlagSA.NONE,    67,  "Rubber",               False),
    (COLGroup.MISC,     COLFlagSA.NONE,    68,  "Plastic",              False),
    (COLGroup.ROCK,     COLFlagSA.NONE,    69,  "Transparent Stone",    False),
    (COLGroup.WOOD,     COLFlagSA.NONE,    70,  "Wood (bench)",         False),
    (COLGroup.MISC,     COLFlagSA.NONE,    71,  "Carpet",               False),
    (COLGroup.WOOD,     COLFlagSA.NONE,    72,  "Floorboard",           False),
    (COLGroup.WOOD,     COLFlagSA.NONE,    73,  "Stairs (wood)",        False),

    # Procedural Materials (74–157)
    (COLGroup.SAND,     COLFlagSA.NONE,    74,  "Sand",                 True),
    (COLGroup.SAND,     COLFlagSA.NONE,    75,  "Sand (dense)",         True),
    (COLGroup.SAND,     COLFlagSA.NONE,    76,  "Sand (arid)",          True),
    (COLGroup.SAND,     COLFlagSA.NONE,    77,  "Sand (compact)",       True),
    (COLGroup.SAND,     COLFlagSA.NONE,    78,  "Sand (rocky)",         True),
    (COLGroup.SAND,     COLFlagSA.NONE,    79,  "Sand (beach)",         True),
    (COLGroup.GRASS,    COLFlagSA.NONE,    80,  "Grass (short)",        True),
    (COLGroup.GRASS,    COLFlagSA.NONE,    81,  "Grass (meadow)",       True),
    (COLGroup.GRASS,    COLFlagSA.NONE,    82,  "Grass (dry)",          True),
    (COLGroup.DIRT,     COLFlagSA.NONE,    83,  "Woodland",             True),
    (COLGroup.DIRT,     COLFlagSA.NONE,    84,  "Wood Dense",           True),
    (COLGroup.GRAVEL,   COLFlagSA.NONE,    85,  "Roadside",             True),
    (COLGroup.SAND,     COLFlagSA.NONE,    86,  "Roadside Desert",      True),
    (COLGroup.DIRT,     COLFlagSA.NONE,    87,  "Flowerbed",            True),
    (COLGroup.DIRT,     COLFlagSA.NONE,    88,  "Waste Ground",         True),
    (COLGroup.CONCRETE, COLFlagSA.NONE,    89,  "Concrete",             True),
    (COLGroup.MISC,     COLFlagSA.NONE,    90,  "Office Desk",          True),
    (COLGroup.MISC,     COLFlagSA.NONE,    91,  "Shelf 711 1",          True),
    (COLGroup.MISC,     COLFlagSA.NONE,    92,  "Shelf 711 2",          True),
    (COLGroup.MISC,     COLFlagSA.NONE,    93,  "Shelf 711 3",          True),
    (COLGroup.MISC,     COLFlagSA.NONE,    94,  "Restaurant Table",     True),
    (COLGroup.MISC,     COLFlagSA.NONE,    95,  "Bar Table",            True),
    (COLGroup.SAND,     COLFlagSA.NONE,    96,  "Underwater (lush)",    True),
    (COLGroup.SAND,     COLFlagSA.NONE,    97,  "Underwater (barren)",  True),
    (COLGroup.SAND,     COLFlagSA.NONE,    98,  "Underwater (coral)",   True),
    (COLGroup.SAND,     COLFlagSA.NONE,    99,  "Underwater (deep)",    True),
    (COLGroup.DIRT,     COLFlagSA.NONE,   100,  "Riverbed",             True),
    (COLGroup.GRAVEL,   COLFlagSA.NONE,   101,  "Rubble",               True),
    (COLGroup.MISC,     COLFlagSA.NONE,   102,  "Bedroom Floor",        True),
    (COLGroup.MISC,     COLFlagSA.NONE,   103,  "Kitchen Floor",        True),
    (COLGroup.MISC,     COLFlagSA.NONE,   104,  "Livingroom Floor",     True),
    (COLGroup.MISC,     COLFlagSA.NONE,   105,  "Corridor Floor",       True),
    (COLGroup.MISC,     COLFlagSA.NONE,   106,  "711 Floor",            True),
    (COLGroup.MISC,     COLFlagSA.NONE,   107,  "Fast Food Floor",      True),
    (COLGroup.MISC,     COLFlagSA.NONE,   108,  "Skanky Floor",         True),
    (COLGroup.ROCK,     COLFlagSA.NONE,   109,  "Mountain",             True),
    (COLGroup.DIRT,     COLFlagSA.NONE,   110,  "Marsh",                True),
    (COLGroup.BUSHES,   COLFlagSA.NONE,   111,  "Bushy",                True),
    (COLGroup.BUSHES,   COLFlagSA.NONE,   112,  "Bushy (mix)",          True),
    (COLGroup.BUSHES,   COLFlagSA.NONE,   113,  "Bushy (dry)",          True),
    (COLGroup.BUSHES,   COLFlagSA.NONE,   114,  "Bushy (mid)",          True),
    (COLGroup.GRASS,    COLFlagSA.NONE,   115,  "Grass (wee flowers)",  True),
    (COLGroup.GRASS,    COLFlagSA.NONE,   116,  "Grass (dry, tall)",    True),
    (COLGroup.GRASS,    COLFlagSA.NONE,   117,  "Grass (lush, tall)",   True),
    (COLGroup.GRASS,    COLFlagSA.NONE,   118,  "Grass (green, mix)",   True),
    (COLGroup.GRASS,    COLFlagSA.NONE,   119,  "Grass (brown, mix)",   True),
    (COLGroup.GRASS,    COLFlagSA.NONE,   120,  "Grass (low)",          True),
    (COLGroup.GRASS,    COLFlagSA.NONE,   121,  "Grass (rocky)",        True),
    (COLGroup.GRASS,    COLFlagSA.NONE,   122,  "Grass (small trees)",  True),
    (COLGroup.DIRT,     COLFlagSA.NONE,   123,  "Dirt (rocky)",         True),
    (COLGroup.DIRT,     COLFlagSA.NONE,   124,  "Dirt (weeds)",         True),
    (COLGroup.GRASS,    COLFlagSA.NONE,   125,  "Grass (weeds)",        True),
    (COLGroup.DIRT,     COLFlagSA.NONE,   126,  "River Edge",           True),
    (COLGroup.CONCRETE, COLFlagSA.NONE,   127,  "Poolside",             True),
    (COLGroup.DIRT,     COLFlagSA.NONE,   128,  "Forest (stumps)",      True),
    (COLGroup.DIRT,     COLFlagSA.NONE,   129,  "Forest (sticks)",      True),
    (COLGroup.DIRT,     COLFlagSA.NONE,   130,  "Forest (leaves)",      True),
    (COLGroup.SAND,     COLFlagSA.NONE,   131,  "Desert Rocks",         True),
    (COLGroup.DIRT,     COLFlagSA.NONE,   132,  "Forest (dry)",         True),
    (COLGroup.DIRT,     COLFlagSA.NONE,   133,  "Sparse Flowers",       True),
    (COLGroup.GRAVEL,   COLFlagSA.NONE,   134,  "Building Site",        True),
    (COLGroup.CONCRETE, COLFlagSA.NONE,   135,  "Docklands",            True),
    (COLGroup.CONCRETE, COLFlagSA.NONE,   136,  "Industrial",           True),
    (COLGroup.CONCRETE, COLFlagSA.NONE,   137,  "Industrial Jetty",     True),
    (COLGroup.CONCRETE, COLFlagSA.NONE,   138,  "Concrete (litter)",    True),
    (COLGroup.CONCRETE, COLFlagSA.NONE,   139,  "Alley Rubbish",        True),
    (COLGroup.GRAVEL,   COLFlagSA.NONE,   140,  "Junkyard Piles",       True),
    (COLGroup.DIRT,     COLFlagSA.NONE,   141,  "Junkyard Ground",      True),
    (COLGroup.DIRT,     COLFlagSA.NONE,   142,  "Dump",                 True),
    (COLGroup.SAND,     COLFlagSA.NONE,   143,  "Cactus Dense",         True),
    (COLGroup.CONCRETE, COLFlagSA.NONE,   144,  "Airport Ground",       True),
    (COLGroup.DIRT,     COLFlagSA.NONE,   145,  "Cornfield",            True),
    (COLGroup.GRASS,    COLFlagSA.NONE,   146,  "Grass (light)",        True),
    (COLGroup.GRASS,    COLFlagSA.NONE,   147,  "Grass (lighter)",      True),
    (COLGroup.GRASS,    COLFlagSA.NONE,   148,  "Grass (lighter 2)",    True),
    (COLGroup.GRASS,    COLFlagSA.NONE,   149,  "Grass (mid 1)",        True),
    (COLGroup.GRASS,    COLFlagSA.NONE,   150,  "Grass (mid 2)",        True),
    (COLGroup.GRASS,    COLFlagSA.NONE,   151,  "Grass (dark)",         True),
    (COLGroup.GRASS,    COLFlagSA.NONE,   152,  "Grass (dark 2)",       True),
    (COLGroup.GRASS,    COLFlagSA.NONE,   153,  "Grass (dirt mix)",     True),
    (COLGroup.ROCK,     COLFlagSA.NONE,   154,  "Riverbed (stone)",     True),
    (COLGroup.DIRT,     COLFlagSA.NONE,   155,  "Riverbed (shallow)",   True),
    (COLGroup.DIRT,     COLFlagSA.NONE,   156,  "Riverbed (weeds)",     True),
    (COLGroup.SAND,     COLFlagSA.NONE,   157,  "Seaweed",              True),

    # Normal Materials (158–178)
    (COLGroup.MISC,     COLFlagSA.NONE,   158,  "Door",                 False),
    (COLGroup.MISC,     COLFlagSA.NONE,   159,  "Plastic Barrier",      False),
    (COLGroup.GRASS,    COLFlagSA.NONE,   160,  "Park Grass",           False),
    (COLGroup.ROCK,     COLFlagSA.NONE,   161,  "Stairs (stone)",       False),
    (COLGroup.METAL,    COLFlagSA.NONE,   162,  "Stairs (metal)",       False),
    (COLGroup.MISC,     COLFlagSA.NONE,   163,  "Stairs (carpet)",      False),
    (COLGroup.METAL,    COLFlagSA.NONE,   164,  "Floor (metal)",        False),
    (COLGroup.CONCRETE, COLFlagSA.NONE,   165,  "Floor (concrete)",     False),
    (COLGroup.MISC,     COLFlagSA.NONE,   166,  "Bin Bag",              False),
    (COLGroup.METAL,    COLFlagSA.NONE,   167,  "Thin Metal Sheet",     False),
    (COLGroup.METAL,    COLFlagSA.NONE,   168,  "Metal Barrel",         False),
    (COLGroup.MISC,     COLFlagSA.NONE,   169,  "Plastic Cone",         False),
    (COLGroup.MISC,     COLFlagSA.NONE,   170,  "Plastic Dumpster",     False),
    (COLGroup.METAL,    COLFlagSA.NONE,   171,  "Metal Dumpster",       False),
    (COLGroup.WOOD,     COLFlagSA.NONE,   172,  "Wood Picket Fence",    False),
    (COLGroup.WOOD,     COLFlagSA.NONE,   173,  "Wood Slatted Fence",   False),
    (COLGroup.WOOD,     COLFlagSA.NONE,   174,  "Wood Ranch Fence",     False),
    (COLGroup.GLASS,    COLFlagSA.NONE,   175,  "Unbreakable Glass",    False),
    (COLGroup.MISC,     COLFlagSA.NONE,   176,  "Hay Bale",             False),
    (COLGroup.MISC,     COLFlagSA.NONE,   177,  "Gore",                 False),
    (COLGroup.MISC,     COLFlagSA.NONE,   178,  "Rail Track",           False),

    # Vehicle Presets (SA)
    (COLGroup.VEHICLE,  COLFlagSA.BODY,    63,  "Body",                 False),
    (COLGroup.VEHICLE,  COLFlagSA.BOOT,    63,  "Boot",                 False),
    (COLGroup.VEHICLE,  COLFlagSA.HOOD,    63,  "Bonnet",               False),
    (COLGroup.VEHICLE,  COLFlagSA.WIND_SH, 45,  "Windshield",           False),
    (COLGroup.VEHICLE,  COLFlagSA.BUMP_F,  63,  "Bumper Front",         False),
    (COLGroup.VEHICLE,  COLFlagSA.BUMP_R,  63,  "Bumper Rear",          False),
    (COLGroup.VEHICLE,  COLFlagSA.DOOR_FL, 63,  "Door Front Left",      False),
    (COLGroup.VEHICLE,  COLFlagSA.DOOR_FR, 63,  "Door Front Right",     False),
    (COLGroup.VEHICLE,  COLFlagSA.DOOR_RL, 63,  "Door Rear Left",       False),
    (COLGroup.VEHICLE,  COLFlagSA.DOOR_RR, 63,  "Door Rear Right",      False),
    (COLGroup.VEHICLE,  COLFlagSA.WING_FL, 63,  "Wing Front Left",      False),
    (COLGroup.VEHICLE,  COLFlagSA.WING_FR, 63,  "Wing Front Right",     False),
    (COLGroup.VEHICLE,  COLFlagSA.WING_RL, 63,  "Wing Rear Left",       False),
    (COLGroup.VEHICLE,  COLFlagSA.WING_RR, 63,  "Wing Rear Right",      False),
    (COLGroup.VEHICLE,  COLFlagSA.NONE,    64,  "Flying Part",          False),
    (COLGroup.VEHICLE,  COLFlagSA.NONE,    65,  "Moving Part",          False),
]


# --------------------------------------------------
# VICE CITY / GTA III — surface flags
# --------------------------------------------------

class COLFlagVC(IntEnum):
    NONE    =  0
    BODY    =  0
    HOOD    =  1
    BOOT    =  2
    BUMP_F  =  3
    BUMP_R  =  4
    DOOR_FL =  5
    DOOR_FR =  6
    DOOR_RL =  7
    DOOR_RR =  8
    WING_FL =  9
    WING_FR = 10
    WING_RL = 11
    WING_RR = 12
    WIND_SH = 17  # Note: VC uses 17, SA uses 19


COL_PRESET_VC: List[Tuple] = [
    # Normal Materials (0–34)
    (COLGroup.ROAD,     COLFlagVC.NONE,     0,  "Default",              False),
    (COLGroup.ROAD,     COLFlagVC.NONE,     1,  "Street",               False),
    (COLGroup.GRASS,    COLFlagVC.NONE,     2,  "Grass",                False),
    (COLGroup.DIRT,     COLFlagVC.NONE,     3,  "Mud",                  False),
    (COLGroup.DIRT,     COLFlagVC.NONE,     4,  "Dirt",                 False),
    (COLGroup.CONCRETE, COLFlagVC.NONE,     5,  "Concrete",             False),
    (COLGroup.METAL,    COLFlagVC.NONE,     6,  "Aluminum",             False),
    (COLGroup.GLASS,    COLFlagVC.NONE,     7,  "Glass",                False),
    (COLGroup.METAL,    COLFlagVC.NONE,     8,  "Metal Pole",           False),
    (COLGroup.MISC,     COLFlagVC.NONE,     9,  "Door",                 False),
    (COLGroup.METAL,    COLFlagVC.NONE,    10,  "Metal Sheet",          False),
    (COLGroup.METAL,    COLFlagVC.NONE,    11,  "Metal",                False),
    (COLGroup.METAL,    COLFlagVC.NONE,    12,  "Small Metal Post",     False),
    (COLGroup.METAL,    COLFlagVC.NONE,    13,  "Large Metal Post",     False),
    (COLGroup.METAL,    COLFlagVC.NONE,    14,  "Medium Metal Post",    False),
    (COLGroup.METAL,    COLFlagVC.NONE,    15,  "Steel",                False),
    (COLGroup.METAL,    COLFlagVC.NONE,    16,  "Fence",                False),
    (COLGroup.MISC,     COLFlagVC.NONE,    17,  "Unknown",              False),
    (COLGroup.SAND,     COLFlagVC.NONE,    18,  "Sand",                 False),
    (COLGroup.WATER,    COLFlagVC.NONE,    19,  "Water",                False),
    (COLGroup.WOOD,     COLFlagVC.NONE,    20,  "Wooden Box",           False),
    (COLGroup.WOOD,     COLFlagVC.NONE,    21,  "Wooden Lathes",        False),
    (COLGroup.WOOD,     COLFlagVC.NONE,    22,  "Wood",                 False),
    (COLGroup.METAL,    COLFlagVC.NONE,    23,  "Metal Box 1",          False),
    (COLGroup.METAL,    COLFlagVC.NONE,    24,  "Metal Box 2",          False),
    (COLGroup.BUSHES,   COLFlagVC.NONE,    25,  "Hedge",                False),
    (COLGroup.ROCK,     COLFlagVC.NONE,    26,  "Rock",                 False),
    (COLGroup.METAL,    COLFlagVC.NONE,    27,  "Metal Container",      False),
    (COLGroup.METAL,    COLFlagVC.NONE,    28,  "Metal Barrel",         False),
    (COLGroup.MISC,     COLFlagVC.NONE,    29,  "Unknown",              False),
    (COLGroup.METAL,    COLFlagVC.NONE,    30,  "Metal Card Box",       False),
    (COLGroup.MISC,     COLFlagVC.NONE,    31,  "Unknown",              False),
    (COLGroup.METAL,    COLFlagVC.NONE,    32,  "Gate/Bars",            False),
    (COLGroup.SAND,     COLFlagVC.NONE,    33,  "Sand 2",               False),
    (COLGroup.GRASS,    COLFlagVC.NONE,    34,  "Grass 2",              False),

    # Vehicle Presets (VC)
    (COLGroup.VEHICLE,  COLFlagVC.BODY,     6,  "Body",                 False),
    (COLGroup.VEHICLE,  COLFlagVC.BOOT,     6,  "Boot",                 False),
    (COLGroup.VEHICLE,  COLFlagVC.HOOD,     6,  "Bonnet",               False),
    (COLGroup.VEHICLE,  COLFlagVC.WIND_SH,  7,  "Windshield",           False),
    (COLGroup.VEHICLE,  COLFlagVC.BUMP_F,   6,  "Bumper Front",         False),
    (COLGroup.VEHICLE,  COLFlagVC.BUMP_R,   6,  "Bumper Rear",          False),
    (COLGroup.VEHICLE,  COLFlagVC.DOOR_FL,  6,  "Door Front Left",      False),
    (COLGroup.VEHICLE,  COLFlagVC.DOOR_FR,  6,  "Door Front Right",     False),
    (COLGroup.VEHICLE,  COLFlagVC.DOOR_RL,  6,  "Door Rear Left",       False),
    (COLGroup.VEHICLE,  COLFlagVC.DOOR_RR,  6,  "Door Rear Right",      False),
    (COLGroup.VEHICLE,  COLFlagVC.WING_FL,  6,  "Wing Front Left",      False),
    (COLGroup.VEHICLE,  COLFlagVC.WING_FR,  6,  "Wing Front Right",     False),
    (COLGroup.VEHICLE,  COLFlagVC.WING_RL,  6,  "Wing Rear Left",       False),
    (COLGroup.VEHICLE,  COLFlagVC.WING_RR,  6,  "Wing Rear Right",      False),
]


# --------------------------------------------------
# LOOKUP TABLES  (built once at import time)
# --------------------------------------------------

def _build_lookup(preset_list: List[Tuple]) -> Dict[int, Tuple]:
    """Build {material_id: (group, flag, name, procedural)} for non-vehicle entries."""
    result = {}
    for group, flag, mat_id, name, is_proc in preset_list:
        if group == COLGroup.VEHICLE:
            continue
        if mat_id not in result:  # first entry wins (no duplicates in these lists)
            result[mat_id] = (group, flag, name, is_proc)
    return result


_SA_LOOKUP: Dict[int, Tuple] = _build_lookup(COL_PRESET_SA)
_VC_LOOKUP: Dict[int, Tuple] = _build_lookup(COL_PRESET_VC)


# --------------------------------------------------
# PUBLIC API
# --------------------------------------------------

def get_material_name(material_id: int, game: COLGame = COLGame.SA) -> str: #vers 1
    """Return the human-readable name for a surface/material ID."""
    lookup = _SA_LOOKUP if game == COLGame.SA else _VC_LOOKUP
    entry = lookup.get(material_id)
    if entry:
        return entry[2]
    return f"Unknown ({material_id})"


def get_material_group(material_id: int, game: COLGame = COLGame.SA) -> COLGroup: #vers 1
    """Return the COLGroup for a material ID."""
    lookup = _SA_LOOKUP if game == COLGame.SA else _VC_LOOKUP
    entry = lookup.get(material_id)
    if entry:
        return COLGroup(entry[0])
    return COLGroup.MISC


def get_material_colour(material_id: int, game: COLGame = COLGame.SA) -> str: #vers 1
    """Return the hex colour string (RRGGBB) for a material, via its group."""
    group = get_material_group(material_id, game)
    return COL_PRESET_GROUP.get(int(group), ["Misc", "F1AB07"])[1]


def get_material_qcolor(material_id: int, game: COLGame = COLGame.SA): #vers 1
    """Return a QColor for a material (requires PyQt6 to be available)."""
    try:
        from PyQt6.QtGui import QColor
        return QColor(f"#{get_material_colour(material_id, game)}")
    except ImportError:
        return None


def get_materials_for_version(game: COLGame = COLGame.SA,
                               include_vehicle: bool = False,
                               include_procedural: bool = True) -> List[Tuple[int, str, str]]: #vers 1
    """
    Return a sorted list of (material_id, name, hex_colour) tuples for a game.
    Useful for populating combo boxes / pickers.
    """
    preset = COL_PRESET_SA if game == COLGame.SA else COL_PRESET_VC
    result = []
    seen = set()
    for group, flag, mat_id, name, is_proc in preset:
        if mat_id in seen:
            continue
        if not include_vehicle and group == COLGroup.VEHICLE:
            continue
        if not include_procedural and is_proc:
            continue
        colour = COL_PRESET_GROUP.get(int(group), ["", "F1AB07"])[1]
        result.append((mat_id, name, colour))
        seen.add(mat_id)
    result.sort(key=lambda x: x[0])
    return result


def get_vehicle_presets(game: COLGame = COLGame.SA) -> List[Tuple[int, int, str]]: #vers 1
    """
    Return vehicle presets as (material_id, flag_id, name) tuples.
    """
    preset = COL_PRESET_SA if game == COLGame.SA else COL_PRESET_VC
    result = []
    for group, flag, mat_id, name, _ in preset:
        if group == COLGroup.VEHICLE:
            result.append((mat_id, int(flag), name))
    return result


def material_id_from_name(name: str, game: COLGame = COLGame.SA) -> Optional[int]: #vers 1
    """Reverse lookup — find material ID from name (case-insensitive)."""
    preset = COL_PRESET_SA if game == COLGame.SA else COL_PRESET_VC
    name_lower = name.lower()
    for _, _, mat_id, mat_name, _ in preset:
        if mat_name.lower() == name_lower:
            return mat_id
    return None


__all__ = [
    'COLGame', 'COLGroup', 'COLFlagSA', 'COLFlagVC',
    'COL_PRESET_GROUP', 'COL_PRESET_SA', 'COL_PRESET_VC',
    'get_material_name', 'get_material_colour', 'get_material_group',
    'get_material_qcolor', 'get_materials_for_version',
    'get_vehicle_presets', 'material_id_from_name',
]
