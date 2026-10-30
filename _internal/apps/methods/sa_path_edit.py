#this belongs in apps/methods/sa_path_edit.py - Version: 1
# X-Seti - October 29 2026 - IMG Factory 1.6 - SA path node editing

"""
Edit SA nodesN.dat graphs across areas: add, link, unlink, delete nodes,
navi nodes follow moved nodes; rebuild a whole nodesN.dat (byte-exact
when nothing changed). areas = {area_id: SAPathFile}.
"""

##Methods list -
# _drop_navi
# _get_links
# _navi_place
# _new_navi
# _node_at
# _put_links
# _shift_refs
# build_nodes_dat
# sa_add_node
# sa_area_for
# sa_delete_node
# sa_link
# sa_linked
# sa_move_navi
# sa_unlink

import math
import struct

from apps.methods.sa_path_parser import SANaviNode, SAPathLink, SAPathNode

HEURISTIC = 0x7FFE                      # Section 1 field, always this value


def sa_area_for(x: float, y: float) -> int: #vers 1
    """Area (0-63) of a world point: 8 x 8 squares of 750 units from -3000."""
    cx = min(7, max(0, int((x + 3000) // 750)))
    cy = min(7, max(0, int((y + 3000) // 750)))
    return cy * 8 + cx


def build_nodes_dat(pf) -> bytes: #vers 1
    """Whole nodesN.dat from an SAPathFile (links in node order)."""
    nodes = pf.vehicle_nodes + pf.ped_nodes
    out = bytearray(struct.pack('<5I', len(nodes), len(pf.vehicle_nodes), len(pf.ped_nodes),
                                len(pf.navi_nodes), len(pf.links)))
    for n in nodes:
        out += struct.pack('<IIhhhhHHHBBI', n.mem_addr, 0,
                           *(int(round(v * 8)) for v in (n.x, n.y, n.z)), HEURISTIC,
                           n.link_id, n.area_id, n.node_id,
                           int(round(n.path_width * 8)) & 0xFF, n.flood_fill & 0xFF, n.flags & 0xFFFFFFFF)
    for v in pf.navi_nodes:
        out += struct.pack('<hhHHbbI', int(round(v.x * 8)), int(round(v.y * 8)),
                           v.target_area_id, v.target_node_id,
                           int(round(v.dir_x * 100)), int(round(v.dir_y * 100)), v.flags & 0xFFFFFFFF)
    for lk in pf.links:
        out += struct.pack('<HH', lk.area_id, lk.node_id)
    out += pf.filler if len(pf.filler) == 768 else bytes(768)
    for lk in pf.links:
        out += struct.pack('<H', 0 if lk.navi_node_id is None
                           else (lk.navi_area_id & 0x3F) << 10 | (lk.navi_node_id & 0x3FF))
    out += bytes(min(255, max(0, lk.length)) for lk in pf.links)
    out += bytes(int(rc) | int(pl) << 1 for rc, pl in zip(pf.road_cross, pf.ped_traffic_light))
    return bytes(out + pf.trailing)


def _node_at(areas, area, nid): #vers 1
    """Node by (area, combined index), or None."""
    pf = areas.get(area)
    if pf is None:
        return None
    nodes = pf.vehicle_nodes + pf.ped_nodes
    return nodes[nid] if 0 <= nid < len(nodes) else None


def _get_links(pf) -> list: #vers 1
    """Per node (combined order): [(SAPathLink, road_cross, ped_light)]."""
    out = []
    for n in pf.vehicle_nodes + pf.ped_nodes:
        out.append([(pf.links[i], pf.road_cross[i] if i < len(pf.road_cross) else False,
                     pf.ped_traffic_light[i] if i < len(pf.ped_traffic_light) else False)
                    for i in range(n.link_id, n.link_id + n.link_count) if i < len(pf.links)])
    return out


def _put_links(pf, per_node): #vers 1
    """Rebuild links / intersection flags, link_id, link count bits."""
    pf.links, pf.road_cross, pf.ped_traffic_light = [], [], []
    for n, lst in zip(pf.vehicle_nodes + pf.ped_nodes, per_node):
        n.link_id = len(pf.links)
        n.link_count = len(lst)
        n.flags = (n.flags & ~0xF) | (len(lst) & 0xF)
        for lk, rc, pl in lst:
            pf.links.append(lk)
            pf.road_cross.append(rc)
            pf.ped_traffic_light.append(pl)


def _shift_refs(areas, area, start, delta, navi=False): #vers 1
    """References to nodes (or navi nodes) of area at index >= start move by delta."""
    for pf in areas.values():
        for lk in pf.links:
            if navi:
                if lk.navi_node_id is not None and lk.navi_area_id == area and lk.navi_node_id >= start:
                    lk.navi_node_id += delta
            elif lk.area_id == area and lk.node_id >= start:
                lk.node_id += delta
        if not navi:
            for v in pf.navi_nodes:
                if v.target_area_id == area and v.target_node_id >= start:
                    v.target_node_id += delta
    if not navi:
        pf = areas[area]
        for i, n in enumerate(pf.vehicle_nodes + pf.ped_nodes):
            n.node_id = i


def sa_linked(areas, a, b) -> bool: #vers 1
    """True when node a=(area, id) lists b as a neighbour."""
    pf = areas[a[0]]
    n = _node_at(areas, *a)
    return any((pf.links[i].area_id, pf.links[i].node_id) == tuple(b)
               for i in range(n.link_id, n.link_id + n.link_count))


def _navi_place(areas, nv, a, b, offset=(0.0, 0.0)): #vers 1
    """Navi node between nodes a and b: midpoint (+ offset), direction toward its target."""
    na, nb = _node_at(areas, *a), _node_at(areas, *b)
    nv.x, nv.y = (na.x + nb.x) / 2.0 + offset[0], (na.y + nb.y) / 2.0 + offset[1]
    tgt, other = (na, nb) if (nv.target_area_id, nv.target_node_id) == tuple(a) else (nb, na)
    dx, dy = tgt.x - other.x, tgt.y - other.y
    ln = math.hypot(dx, dy) or 1.0
    nv.dir_x, nv.dir_y = dx / ln, dy / ln


def _new_navi(areas, a, b, flags): #vers 1
    """New navi node for vehicle link a-b in the area of its midpoint; (area, index)."""
    na, nb = _node_at(areas, *a), _node_at(areas, *b)
    area = sa_area_for((na.x + nb.x) / 2.0, (na.y + nb.y) / 2.0)
    if area not in areas:
        area = a[0]
    nv = SANaviNode(x=0.0, y=0.0, target_area_id=b[0], target_node_id=b[1], dir_x=0.0, dir_y=0.0, flags=flags)
    _navi_place(areas, nv, a, b)
    areas[area].navi_nodes.append(nv)
    return area, len(areas[area].navi_nodes) - 1


def sa_link(areas, a, b) -> bool: #vers 1
    """Two-way link a=(area, id) <-> b; vehicle links get a navi node. False if not allowed."""
    na, nb = _node_at(areas, *a), _node_at(areas, *b)
    if na is None or nb is None or tuple(a) == tuple(b) or na.is_ped != nb.is_ped or sa_linked(areas, a, b):
        return False
    if na.link_count >= 15 or nb.link_count >= 15:      # 4 count bits
        return False
    navi = (None, None)
    if not na.is_ped:
        pa = areas[a[0]]
        flags = int(round(na.path_width * 8)) & 0xFF | (1 << 8) | (1 << 11)    # one lane each way
        for i in range(na.link_id, na.link_id + na.link_count):              # else copy a neighbour's
            lk = pa.links[i]
            if lk.navi_node_id is not None and lk.navi_area_id in areas \
                    and lk.navi_node_id < len(areas[lk.navi_area_id].navi_nodes):
                flags = areas[lk.navi_area_id].navi_nodes[lk.navi_node_id].flags
                break
        navi = _new_navi(areas, a, b, flags)
    length = int(math.hypot(nb.x - na.x, nb.y - na.y))
    for (s, t) in ((a, b), (b, a)):
        pf = areas[s[0]]
        per = _get_links(pf)
        per[s[1]].append((SAPathLink(area_id=t[0], node_id=t[1], navi_area_id=navi[0], navi_node_id=navi[1],
                                     length=length), False, False))
        _put_links(pf, per)
    return True


def _drop_navi(areas, area, idx): #vers 1
    """Remove a navi node no link uses any more; later ones shift down."""
    if any(lk.navi_area_id == area and lk.navi_node_id == idx for pf in areas.values() for lk in pf.links):
        return
    del areas[area].navi_nodes[idx]
    _shift_refs(areas, area, idx + 1, -1, navi=True)


def sa_unlink(areas, a, b) -> bool: #vers 1
    """Remove the link between a and b (both directions) and its navi node."""
    if not sa_linked(areas, a, b):
        return False
    navis = set()
    for (s, t) in ((a, b), (b, a)):
        pf = areas[s[0]]
        per = _get_links(pf)
        keep = []
        for lk, rc, pl in per[s[1]]:
            if (lk.area_id, lk.node_id) == tuple(t):
                if lk.navi_node_id is not None:
                    navis.add((lk.navi_area_id, lk.navi_node_id))
            else:
                keep.append((lk, rc, pl))
        per[s[1]] = keep
        _put_links(pf, per)
    for area, idx in sorted(navis, key=lambda v: -v[1]):
        _drop_navi(areas, area, idx)
    return True


def sa_add_node(areas, src, x, y, z): #vers 1
    """New node like src=(area, id) at x, y, z (its own area), linked to src. Returns (area, id) or None."""
    s = _node_at(areas, *src)
    area = sa_area_for(x, y)
    if s is None or area not in areas:
        return None
    pf = areas[area]
    n = SAPathNode(x=x, y=y, z=z, link_id=0, link_count=0, area_id=area, node_id=0,
                   path_width=s.path_width, flood_fill=s.flood_fill, flags=s.flags & ~0xF, is_ped=s.is_ped)
    per = _get_links(pf)
    if s.is_ped:
        pf.ped_nodes.append(n)
        per.append([])
        nid = len(pf.vehicle_nodes) + len(pf.ped_nodes) - 1
    else:
        nid = len(pf.vehicle_nodes)
        _shift_refs(areas, area, nid, 1)              # ped nodes move up one
        pf.vehicle_nodes.append(n)
        per.insert(nid, [])
    _put_links(pf, per)
    for i, nd in enumerate(pf.vehicle_nodes + pf.ped_nodes):
        nd.node_id = i
    sa_link(areas, src, (area, nid))
    return area, nid


def sa_delete_node(areas, node) -> bool: #vers 1
    """Delete node=(area, id) with all its links; later nodes shift down."""
    area, nid = node
    n = _node_at(areas, area, nid)
    if n is None:
        return False
    pf = areas[area]
    for t in [(pf.links[i].area_id, pf.links[i].node_id) for i in range(n.link_id, n.link_id + n.link_count)]:
        sa_unlink(areas, node, t)
    per = _get_links(pf)
    del per[nid]
    (pf.ped_nodes if n.is_ped else pf.vehicle_nodes).remove(n)
    _put_links(pf, per)
    _shift_refs(areas, area, nid + 1, -1)
    return True


def sa_move_navi(areas, node, old_xy): #vers 1
    """After node moved from old_xy: its navi nodes keep their offset from the link midpoint."""
    n = _node_at(areas, *node)
    if n is None or n.is_ped:
        return
    pf = areas[node[0]]
    for i in range(n.link_id, n.link_id + n.link_count):
        lk = pf.links[i]
        b = _node_at(areas, lk.area_id, lk.node_id)
        if b is None or lk.navi_node_id is None or lk.navi_area_id not in areas \
                or lk.navi_node_id >= len(areas[lk.navi_area_id].navi_nodes):
            continue
        nv = areas[lk.navi_area_id].navi_nodes[lk.navi_node_id]
        off = (nv.x - (old_xy[0] + b.x) / 2.0, nv.y - (old_xy[1] + b.y) / 2.0)
        _navi_place(areas, nv, node, (lk.area_id, lk.node_id), off)
