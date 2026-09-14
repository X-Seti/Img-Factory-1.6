#this belongs in apps/methods/txd_dedup.py - Version: 1
# X-Seti - September 12 2026 - IMG Factory 1.6 - TXD Near-Duplicate Detection

"""txd_dedup.py - detect near-duplicate TXD archives and same-name
texture size mismatches across a set of real TXD files (Sep 12
2026, per Keith's own real example: buildhous.txd/buildhoushi.txd/
buildhous112.txd differing by one texture each - devs not
optimizing shared textures across separate TXD archives). Compares
real internal texture NAME sets only (no pixel/image decoding) - a
widely shared single TXD referenced many times across many models
(e.g. "gtaiii.txd" used 40 times) is a different, unrelated, normal
thing and never enters either comparison here."""

##Methods list -
# TXDInfo
# load_txd_info
# TXDNearDuplicate
# find_near_duplicate_txds
# cluster_near_duplicate_txds
# TXDSameNameSizeMismatch
# find_same_name_size_mismatches

import os
from dataclasses import dataclass, field
from typing import Dict, List, Set


@dataclass
class TXDInfo: #vers 1
    path: str = ""
    texture_names: Set[str] = field(default_factory=set)           # lowercase
    texture_sizes: Dict[str, tuple] = field(default_factory=dict)  # lowercase name -> (width, height)


def load_txd_info(txd_paths: List[str]) -> Dict[str, TXDInfo]: #vers 1
    """Real internal texture names + sizes for every given real TXD
    file, parsed once and shared by both checks in this module.
    Reuses txd_parser.load_txd (the already-working RW-chunk parser
    Model Workshop's own viewport uses) rather than a new lightweight
    names-only parser - a genuine future speed optimization, not
    duplicated here."""
    from apps.methods.txd_parser import load_txd
    info = {}
    for path in txd_paths:
        if not path or not os.path.isfile(path):
            continue
        try:
            textures = load_txd(path)
        except Exception:
            continue
        if not textures:
            continue
        txd = TXDInfo(path=path)
        for t in textures:
            name = t.get('name')
            if not name:
                continue
            key = name.lower()
            txd.texture_names.add(key)
            txd.texture_sizes[key] = (t.get('width', 0), t.get('height', 0))
        info[path] = txd
    return info


@dataclass
class TXDNearDuplicate: #vers 1
    txd_a: str = ""
    txd_b: str = ""
    only_in_a: Set[str] = field(default_factory=set)
    only_in_b: Set[str] = field(default_factory=set)
    shared: Set[str] = field(default_factory=set)

    @property
    def diff_count(self): #vers 1
        return len(self.only_in_a) + len(self.only_in_b)


def find_near_duplicate_txds(txd_infos: Dict[str, TXDInfo], max_diff: int = 1) -> List[TXDNearDuplicate]: #vers 1
    """Every real pair of TXDs whose own texture NAME sets differ by
    at most max_diff names (Keith's own "5 different, ignore; -1/+1,
    combine" threshold idea - max_diff is that number), and share at
    least one name - excludes two otherwise-unrelated small TXDs
    that could coincidentally "diff by 1" with zero real overlap."""
    results = []
    paths = list(txd_infos.keys())
    for i in range(len(paths)):
        for j in range(i + 1, len(paths)):
            a, b = txd_infos[paths[i]], txd_infos[paths[j]]
            only_a = a.texture_names - b.texture_names
            only_b = b.texture_names - a.texture_names
            shared = a.texture_names & b.texture_names
            if not shared:
                continue
            if len(only_a) + len(only_b) <= max_diff:
                results.append(TXDNearDuplicate(a.path, b.path, only_a, only_b, shared))
    return results


def cluster_near_duplicate_txds(near_dupes: List[TXDNearDuplicate]) -> List[Set[str]]: #vers 1
    """Group pairwise near-duplicate results into clusters via
    transitive closure (Sep 12 2026, per Keith's own real example -
    buildhous/buildhoushi/buildhous112 should show as ONE group of
    3, not 3 separate pairwise hits)."""
    parent = {}

    def find(x):
        while parent.get(x, x) != x:
            x = parent.get(x, x)
        return x

    def union(x, y):
        rx, ry = find(x), find(y)
        if rx != ry:
            parent[rx] = ry

    for nd in near_dupes:
        parent.setdefault(nd.txd_a, nd.txd_a)
        parent.setdefault(nd.txd_b, nd.txd_b)
        union(nd.txd_a, nd.txd_b)

    groups: Dict[str, Set[str]] = {}
    for path in parent:
        root = find(path)
        groups.setdefault(root, set()).add(path)
    return list(groups.values())


@dataclass
class TXDSameNameSizeMismatch: #vers 1
    texture_name: str = ""
    occurrences: list = field(default_factory=list)   # (txd_path, width, height)

    @property
    def largest(self): #vers 1
        return max(self.occurrences, key=lambda o: o[1] * o[2])


def find_same_name_size_mismatches(txd_infos: Dict[str, TXDInfo]) -> List[TXDSameNameSizeMismatch]: #vers 1
    """Every real texture name that occurs in more than one given
    TXD with a genuinely different real width x height (Sep 12 2026,
    per Keith: "if there is a texture with the same name, but in
    another txd, a larger version with the same name, flag it") - a
    real sign the smaller copy could be dropped for the larger one,
    or that two supposedly-identical textures have quietly drifted
    apart."""
    by_name: Dict[str, list] = {}
    for txd in txd_infos.values():
        for name, size in txd.texture_sizes.items():
            by_name.setdefault(name, []).append((txd.path, size[0], size[1]))

    results = []
    for name, occurrences in by_name.items():
        sizes = {(w, h) for _, w, h in occurrences}
        if len(sizes) > 1:
            results.append(TXDSameNameSizeMismatch(texture_name=name, occurrences=occurrences))
    return results
