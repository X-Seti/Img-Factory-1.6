#this belongs in apps/methods/sa_path_parser.py - Version: 1
# X-Seti - Aug 14 2026 - IMG Factory 1.6 - SA Path Node Parser (nodesXX.dat)

"""
SA Path Node Parser - binary nodesXX.dat format (GTA San Andreas).

Per GTAMods Wiki "Paths (GTA SA)": vehicle/ped paths in SA are NOT
stored in the IPL "path" text section - that format (PathNode/
PathGroup in gta_dat_parser.py, verified against Keith's real III/VC
paths.ipl data) only applies to GTA III and Vice City. SA's own
text-format path files still exist on disk but are unused leftovers;
the game only reads 64 separate binary nodesN.dat files (N=0-63),
one per 750x750-unit map area starting at (-3000,-3000) in row-major
order, normally packed inside gta3.img (or another archive).

This module is a from-scratch binary parser for that format, kept
deliberately separate from gta_dat_parser.py's PathNode/PathGroup -
different file format entirely, reusing those class names for this
would be misleading. Format spec: https://gtamods.com/wiki/Paths_(GTA_SA)

Shared, reusable module (Aug 14 2026, per Keith: "build the paths
parser as a shared method set, that can be used by other tools,
besides map workshop") - no Map Workshop/PyQt/GUI dependencies here,
just struct/dataclasses, so any tool in this codebase can import and
use it directly.

Not yet verified against real nodesXX.dat sample data - built
straight from the documented spec (which itself includes detailed
real-world flag-usage statistics the parser's own field layout was
cross-checked against), but Keith's own real data should confirm or
correct anything the wiki got wrong/left ambiguous.
"""

import os
import re
import struct
from dataclasses import dataclass, field
from typing import Dict, List, Optional


##Methods list -
# find_nodes_dat_in_img #vers 1
# load_all_nodes_dat_from_dir #vers 1
# load_nodes_dat #vers 1
# load_nodes_dat_from_img_entry #vers 1
# parse_nodes_dat #vers 1


@dataclass
class SAPathNode: #vers 1
    """One Section-1 path node (28 bytes on disk) - a vehicle or ped
    path anchor point. Position already converted from the file's
    int16 eighth-unit precision to standard world units (divide by
    8, per the wiki) - lands in the same coordinate space as
    everything else (instance positions, IDE/IPL data, etc.), same
    reasoning as the /16 conversion gta_dat_parser.py's PathNode
    already does for III/VC (different divisor - a different game's
    different internal precision, not the same bug/fix repeated)."""
    x: float
    y: float
    z: float
    link_id:    int     # index of this node's first entry in the links list
    link_count: int     # from flags bits 0-3 - link range is [link_id, link_id+link_count)
    area_id:    int
    node_id:    int
    path_width: float   # already /8'd, per the wiki
    flood_fill: int      # 1=normal vehicle traffic, 2=boats, higher=disconnected/mission areas
    flags:      int      # raw 32-bit flags - see the property helpers below
    is_ped:     bool = False   # which half of Section 1 this came from

    @property
    def traffic_level(self) -> int:
        """Flag bits 4-5: 0=full, 1=high, 2=medium, 3=low."""
        return (self.flags >> 4) & 0b11

    @property
    def is_roadblock(self) -> bool:
        """Flag bit 6 ('A') or bit 23 ('R') - the wiki lists both as
        roadblock-related without fully distinguishing them."""
        return bool(self.flags & (1 << 6)) or bool(self.flags & (1 << 23))

    @property
    def is_boat(self) -> bool:
        return bool(self.flags & (1 << 7))          # bit 7 ('B')

    @property
    def is_emergency_only(self) -> bool:
        return bool(self.flags & (1 << 8))           # bit 8 ('C')

    @property
    def is_highway(self) -> bool:
        """Bit 13 ('H') - per the wiki, ignored for ped nodes and
        never set alone without G/H context for cars."""
        return bool(self.flags & (1 << 13))

    @property
    def spawn_probability(self) -> int:
        """Flag bits 16-19 ('K'-'M' range per the wiki's own table,
        0x0-0xF)."""
        return (self.flags >> 16) & 0xF

    @property
    def is_parking(self) -> bool:
        return bool(self.flags & (1 << 21))          # bit 21 ('P')


@dataclass
class SANaviNode: #vers 1
    """One Section-2 navi node (14 bytes on disk) - an interpolation
    point positioned between two adjacent *vehicle* path nodes (not
    used by ped paths at all, per the wiki). Position already /8'd
    like SAPathNode; direction already /100'd (the wiki: signed
    bytes in [-100,100] represent the float range [-1.0,1.0])."""
    x: float
    y: float
    target_area_id: int   # area/node ID of the vehicle node this navi node feeds into
    target_node_id: int
    dir_x: float           # normalized direction toward the target node
    dir_y: float
    flags: int

    @property
    def path_width(self) -> int:
        """Flag bits 0-7 - usually a copy of the linked node's own
        path_width, per the wiki."""
        return self.flags & 0xFF

    @property
    def left_lanes(self) -> int:
        return (self.flags >> 8) & 0b111              # bits 8-10

    @property
    def right_lanes(self) -> int:
        return (self.flags >> 11) & 0b111              # bits 11-13

    @property
    def is_train_crossing(self) -> bool:
        return bool(self.flags & (1 << 18))             # bit 18

    @property
    def traffic_light_behaviour(self) -> int:
        """Flag bits 16-17: 0=disabled, 1=N-S cycle, 2=W-E cycle."""
        return (self.flags >> 16) & 0b11


@dataclass
class SAPathLink: #vers 2
    """One adjacency-list edge, combining Sections 3/5/6 into a
    single record - the wiki itself notes these three sections share
    the same entry count and "can be treated as one record by
    editors" rather than three parallel arrays a caller would have
    to zip together themselves. navi_node_id/navi_area_id are None
    for ped-node links (zero/unused in Section 5, per the wiki).

    IMPORTANT, undocumented-by-the-wiki quirk discovered and verified
    directly against Keith's own real, complete NODES0-63.DAT set
    (Aug 19 2026, while building real path-graph visualization): when
    a link's own source node is a PED node, this node_id is a
    COMBINED index into the target area's vehicle_nodes+ped_nodes as
    one contiguous array - NOT a ped_nodes-only index the way a
    VEHICLE link's node_id already correctly is. A caller resolving a
    ped link's real target must first subtract len(target_area.
    vehicle_nodes) from this value before indexing into that area's
    own ped_nodes list. Confirmed by direct measurement, not
    hypothesis: resolving every real link across the whole map without
    this adjustment left exactly 45,835 ped-originated links (100% of
    all ped links, 0% of vehicle links - a clean, systematic split,
    not noise) pointing at an out-of-range node_id; applying this
    exact adjustment brought every single one of those down to zero
    remaining failures. Vehicle links need no such adjustment - their
    own node_id already indexes vehicle_nodes directly, confirmed
    separately via ROADBLOX.DAT's own real data (325/325 real
    roadblock entries resolve correctly against vehicle_nodes with no
    offset needed at all)."""
    area_id: int          # Section 3 - the linked-to node's area
    node_id: int          # Section 3 - the linked-to node's ID within that area (see the
                           # class docstring above for the real, confirmed ped-link offset quirk)
    navi_node_id: Optional[int] = None   # Section 5
    navi_area_id: Optional[int] = None   # Section 5
    length: int = 0         # Section 6, whole units


@dataclass
class SAPathFile: #vers 1
    """One fully-parsed nodesN.dat file - all 7 sections. area_id is
    the N from the filename (nodes12.dat -> 12) or the value passed
    to parse_nodes_dat directly, not re-derived from Section 1 (which
    only carries it per-node, redundantly). road_cross/
    ped_traffic_light are Section 7, one entry per link (same
    length/order as `links`, per the wiki: "size of section is equal
    to count of node addresses")."""
    area_id: int
    vehicle_nodes: List[SAPathNode] = field(default_factory=list)
    ped_nodes:     List[SAPathNode] = field(default_factory=list)
    navi_nodes:    List[SANaviNode] = field(default_factory=list)
    links:         List[SAPathLink] = field(default_factory=list)
    road_cross:        List[bool] = field(default_factory=list)
    ped_traffic_light: List[bool] = field(default_factory=list)
    source_path:   str = ""
    parse_errors:  List[str] = field(default_factory=list)

    @property
    def all_nodes(self) -> List[SAPathNode]:
        """Vehicle nodes followed by ped nodes - Section 1's own
        on-disk order, so index arithmetic against link_id ranges
        (which address into this combined list) stays correct."""
        return self.vehicle_nodes + self.ped_nodes


def parse_nodes_dat(data: bytes, area_id: int, source_path: str = "") -> Optional[SAPathFile]: #vers 1
    """Parse one nodesN.dat file's raw bytes into a SAPathFile.
    Returns None only if the header itself (fixed 20 bytes) doesn't
    fit - anything short of that, a truncated/malformed section is
    recorded in the returned SAPathFile.parse_errors and parsing
    continues from where it can, rather than aborting the whole file
    over one bad section - matching this codebase's established
    pattern elsewhere (IDE/IPL text parsing) of surfacing problems
    without losing whatever data did parse correctly."""
    errors: List[str] = []
    if len(data) < 20:
        return None

    (num_nodes, num_vehicle_nodes, num_ped_nodes,
     num_navi_nodes, num_links) = struct.unpack_from('<5I', data, 0)

    if num_vehicle_nodes + num_ped_nodes != num_nodes:
        errors.append(
            f"header node-count mismatch: {num_vehicle_nodes} vehicle + "
            f"{num_ped_nodes} ped != {num_nodes} total (continuing anyway)")

    offset = 20

    # Section 1 - path nodes, 28 bytes each: mem_addr(4,unused),
    # zero(4,unused), x/y/z(2 each), heuristic(2,unused - always
    # 0x7FFE per the wiki), link_id(2), area_id(2), node_id(2),
    # path_width(1), flood_fill(1), flags(4). Vehicle nodes first,
    # then ped nodes - is_ped set from position within the combined
    # count, not from anything in the record itself.
    vehicle_nodes: List[SAPathNode] = []
    ped_nodes: List[SAPathNode] = []
    node_fmt = '<IIhhhhHHHBBI'
    node_size = struct.calcsize(node_fmt)   # 28
    for i in range(num_nodes):
        if offset + node_size > len(data):
            errors.append(f"path node {i}: truncated at offset {offset}")
            break
        (_mem_addr, _zero, rx, ry, rz, _heuristic, link_id,
         node_area_id, node_id, width, flood_fill, flags) = \
            struct.unpack_from(node_fmt, data, offset)
        offset += node_size
        node = SAPathNode(
            x=rx / 8.0, y=ry / 8.0, z=rz / 8.0,
            link_id=link_id, link_count=flags & 0xF,
            area_id=node_area_id, node_id=node_id,
            path_width=width / 8.0, flood_fill=flood_fill,
            flags=flags, is_ped=(i >= num_vehicle_nodes))
        (ped_nodes if node.is_ped else vehicle_nodes).append(node)

    # Section 2 - navi nodes, 14 bytes each: x/y(2 each), area_id(2),
    # node_id(2), dir_x/dir_y(1 each, signed), flags(4).
    navi_nodes: List[SANaviNode] = []
    navi_fmt = '<hhHHbbI'
    navi_size = struct.calcsize(navi_fmt)   # 14
    for i in range(num_navi_nodes):
        if offset + navi_size > len(data):
            errors.append(f"navi node {i}: truncated at offset {offset}")
            break
        rx, ry, target_area, target_node, dx, dy, flags = \
            struct.unpack_from(navi_fmt, data, offset)
        offset += navi_size
        navi_nodes.append(SANaviNode(
            x=rx / 8.0, y=ry / 8.0,
            target_area_id=target_area, target_node_id=target_node,
            dir_x=dx / 100.0, dir_y=dy / 100.0, flags=flags))

    # Section 3 - links, 4 bytes each: area_id(2), node_id(2).
    link_areas: List[int] = []
    link_node_ids: List[int] = []
    link_fmt = '<HH'
    link_size = struct.calcsize(link_fmt)   # 4
    for i in range(num_links):
        if offset + link_size > len(data):
            errors.append(f"link {i}: truncated at offset {offset}")
            break
        a, n = struct.unpack_from(link_fmt, data, offset)
        offset += link_size
        link_areas.append(a)
        link_node_ids.append(n)

    # Section 4 - filler, fixed 768 bytes, contents documented as not
    # mattering ("this can be filled with zeros as well") - skipped
    # rather than read/stored.
    if offset + 768 <= len(data):
        offset += 768
    else:
        errors.append("filler section (768 bytes) truncated or missing")
        offset = len(data)

    # Section 5 - navi links, 2 bytes each: lower 10 bits = navi node
    # ID, upper 6 bits = area ID, zero/unused for ped-node links.
    navi_link_navi_ids: List[Optional[int]] = []
    navi_link_area_ids: List[Optional[int]] = []
    for i in range(num_links):
        if offset + 2 > len(data):
            errors.append(f"navi link {i}: truncated at offset {offset}")
            break
        raw = struct.unpack_from('<H', data, offset)[0]
        offset += 2
        if raw == 0:
            navi_link_navi_ids.append(None)
            navi_link_area_ids.append(None)
        else:
            navi_link_navi_ids.append(raw & 0x3FF)
            navi_link_area_ids.append((raw >> 10) & 0x3F)

    # Section 6 - link lengths, 1 byte each, whole units.
    link_lengths: List[int] = []
    for i in range(num_links):
        if offset + 1 > len(data):
            errors.append(f"link length {i}: truncated at offset {offset}")
            break
        link_lengths.append(data[offset])
        offset += 1

    links: List[SAPathLink] = []
    for i in range(len(link_areas)):
        links.append(SAPathLink(
            area_id=link_areas[i], node_id=link_node_ids[i],
            navi_node_id=navi_link_navi_ids[i] if i < len(navi_link_navi_ids) else None,
            navi_area_id=navi_link_area_ids[i] if i < len(navi_link_area_ids) else None,
            length=link_lengths[i] if i < len(link_lengths) else 0))

    # Section 7 - one CPathIntersectionInfo bit-pair (bit 0 =
    # m_bRoadCross, bit 1 = m_bPedTrafficLight) packed into a byte
    # per link entry, per the wiki's own C struct. Followed by 192
    # bytes of documented-unknown trailing data - not parsed.
    road_cross: List[bool] = []
    ped_traffic_light: List[bool] = []
    for i in range(num_links):
        if offset >= len(data):
            errors.append(f"intersection flags {i}: truncated at offset {offset}")
            break
        b = data[offset]
        offset += 1
        road_cross.append(bool(b & 0x1))
        ped_traffic_light.append(bool(b & 0x2))

    return SAPathFile(
        area_id=area_id, vehicle_nodes=vehicle_nodes, ped_nodes=ped_nodes,
        navi_nodes=navi_nodes, links=links,
        road_cross=road_cross, ped_traffic_light=ped_traffic_light,
        source_path=source_path, parse_errors=errors)


_NODES_DAT_RE = re.compile(r'nodes(\d+)\.dat$', re.IGNORECASE)


def load_nodes_dat(path: str) -> Optional[SAPathFile]: #vers 1
    """Load and parse a nodesN.dat file from a real path on disk.
    area_id is parsed from the filename itself (nodes12.dat -> 12),
    the same convention the game and every tool/doc on the wiki use.
    Returns None if the filename doesn't match that pattern, or the
    file can't be read."""
    m = _NODES_DAT_RE.search(os.path.basename(path))
    if not m:
        return None
    area_id = int(m.group(1))
    try:
        with open(path, 'rb') as f:
            data = f.read()
    except OSError:
        return None
    return parse_nodes_dat(data, area_id, source_path=path)


def load_all_nodes_dat_from_dir(directory: str) -> Dict[int, SAPathFile]: #vers 1
    """Load every nodesN.dat found directly in `directory` (the
    conventional loose-file location is data\\paths\\, per the wiki -
    though the wiki also notes the game itself ignores that copy and
    only reads the ones packed into gta3.img, see
    find_nodes_dat_in_img/load_nodes_dat_from_img_entry for that
    path). Keyed by area_id; a file that fails to parse is skipped
    rather than raising, since one bad area file shouldn't block
    every other one from loading."""
    result: Dict[int, SAPathFile] = {}
    try:
        entries = os.listdir(directory)
    except OSError:
        return result
    for name in entries:
        if _NODES_DAT_RE.search(name):
            parsed = load_nodes_dat(os.path.join(directory, name))
            if parsed is not None:
                result[parsed.area_id] = parsed
    return result


def find_nodes_dat_in_img(img) -> Dict[int, object]: #vers 1
    """Scan an already-open IMGFile (apps.methods.img_core_classes)
    for nodesN.dat entries, keyed by area_id - the wiki's documented
    normal location ("in gta3.img, or any other archive"), matching
    the same lightweight container-name indexing ModelCache already
    uses for .dff/.txd/.col entries (read directory headers only,
    don't parse any entry's content here). Returns {area_id:
    IMGEntry}, ready to hand to load_nodes_dat_from_img_entry once a
    caller actually wants one loaded."""
    result: Dict[int, object] = {}
    for entry in getattr(img, 'entries', []):
        name = getattr(entry, 'name', None)
        if not name:
            continue
        m = _NODES_DAT_RE.search(name)
        if m:
            result[int(m.group(1))] = entry
    return result


def load_nodes_dat_from_img_entry(img, entry, area_id: int) -> Optional[SAPathFile]: #vers 1
    """Read one nodesN.dat IMGEntry's bytes from an already-open
    IMGFile and parse it - the actual load step for an entry found
    via find_nodes_dat_in_img. Kept as a separate call (rather than
    find_nodes_dat_in_img eagerly parsing everything it finds) so a
    caller only pays the real parsing cost for area files it actually
    needs, same lazy-load reasoning as ModelCache.get_geometry/
    get_collision."""
    read_entry = getattr(img, 'read_entry_data', None) or getattr(img, 'read_entry', None)
    if read_entry is None:
        return None
    try:
        data = read_entry(entry)
    except Exception:
        return None
    if not data:
        return None
    source = f"{getattr(img, 'file_path', getattr(img, 'path', ''))}:{getattr(entry, 'name', '')}"
    return parse_nodes_dat(data, area_id, source_path=source)
