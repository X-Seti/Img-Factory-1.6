#!/usr/bin/env python3
#this belongs in apps/methods/carcols_parser.py - Version: 1
# X-Seti - May11 2026 - IMG Factory 1.6 - CarCols Parser
"""
carcols_parser.py - Parses GTA SA/VC/III carcols.dat
Returns colour palette and per-vehicle colour pair lists.

carcols.dat format:
  col section: index, r, g, b  (0-255 palette entries)
  car section: modelname, col1a,col1b, col2a,col2b, ...
                (multiple colour pairs per vehicle)
"""

import os, re

## Methods list -
# parse_carcols
# get_vehicle_colours
# CarcolsData.__init__
# CarcolsData.get_colours


class CarcolsData:
    def __init__(self): #vers 1
        self.palette = {}        # index -> (r,g,b) 0-255
        self.vehicles = {}       # name_lower -> [(col1a,col1b), ...]

    def get_colours(self, vehicle_name: str): #vers 1
        """Return list of (primary,secondary) colour tuples (0-1 float) for vehicle."""
        pairs = self.vehicles.get(vehicle_name.lower(), [])
        result = []
        for ca, cb in pairs:
            p1 = self.palette.get(ca, (180,180,180))
            p2 = self.palette.get(cb, (100,100,100))
            result.append((
                (p1[0]/255, p1[1]/255, p1[2]/255),
                (p2[0]/255, p2[1]/255, p2[2]/255),
            ))
        return result


def parse_carcols(path: str) -> CarcolsData: #vers 1
    """Parse carcols.dat and return CarcolsData."""
    data = CarcolsData()
    if not os.path.isfile(path):
        return data
    try:
        with open(path, 'r', errors='ignore') as f:
            lines = f.readlines()

        section = None
        for line in lines:
            line = line.strip()
            if not line or line.startswith('#') or line.startswith(';'):
                continue
            ll = line.lower()
            if ll.startswith('col'):
                section = 'col'; continue
            if ll.startswith('car'):
                section = 'car'; continue
            if ll.startswith('end'):
                section = None; continue

            if section == 'col':
                # idx, r, g, b
                parts = re.split(r'[\s,]+', line)
                if len(parts) >= 4:
                    try:
                        idx = int(parts[0])
                        r,g,b = int(parts[1]),int(parts[2]),int(parts[3])
                        data.palette[idx] = (r,g,b)
                    except ValueError:
                        pass

            elif section == 'car':
                # modelname, ca,cb [,ca,cb ...]
                parts = re.split(r'[\s,]+', line)
                if len(parts) >= 3:
                    name = parts[0].lower()
                    nums = []
                    for p in parts[1:]:
                        try: nums.append(int(p))
                        except ValueError: pass
                    pairs = []
                    for i in range(0, len(nums)-1, 2):
                        pairs.append((nums[i], nums[i+1]))
                    if pairs:
                        data.vehicles[name] = pairs

    except Exception as e:
        print(f"[carcols_parser] Error: {e}")
    return data


def get_vehicle_colours(game_root: str, vehicle_name: str):
    """Convenience: find carcols.dat from game_root, return colour pairs."""
    for subpath in ('data/carcols.dat', 'DATA/CARCOLS.DAT', 'data/CARCOLS.DAT'):
        path = os.path.join(game_root, subpath)
        if os.path.isfile(path):
            data = parse_carcols(path)
            return data.get_colours(vehicle_name)
    return []
