#!/usr/bin/env python3
#this belongs in apps/methods/vehicles_ide_parser.py - Version: 1
# X-Seti - May11 2026 - IMG Factory 1.6 - vehicles.ide Parser
"""
vehicles_ide_parser.py — Parses GTA SA vehicles.ide
Returns per-vehicle: model name, txd name, type, wheel model, wheel scale, flags.

vehicles.ide CARS section format (SA):
  id, modelname, txdname, type, handlingid, gamename, anims, class,
  frq, lvl, comprules, wheelfudge, wheelrear, wheelfront, wheelscale, flags
"""

import os, re

## Methods list -
# parse_vehicles_ide
# VehicleEntry.__init__
# VehiclesIDE.__init__
# VehiclesIDE.get


WHEEL_TYPES = {
    'wheel_rim':       'wheel_rim_l0',
    'wheel_offroad':   'wheel_offroad_l0',
    'wheel_truck':     'wheel_truck_l0',
    'wheel_sport':     'wheel_sport_l0',
    'wheel_alloy':     'wheel_alloy_l0',
    'wheel_lightvan':  'wheel_lightvan_l0',
    'wheel_lighttruck':'wheel_lighttruck_l0',
    'wheel_classic':   'wheel_classic_l0',
    'wheel_saloon':    'wheel_saloon_l0',
    'wheel_smallcar':  'wheel_smallcar_l0',
}


class VehicleEntry:
    def __init__(self): #vers 1
        self.model_id    = 0
        self.model_name  = ''
        self.txd_name    = ''
        self.veh_type    = ''
        self.handling_id = ''
        self.game_name   = ''
        self.wheel_model = ''   # e.g. 'wheel_saloon'
        self.wheel_scale = 1.0
        self.flags       = ''

    def wheel_dff_name(self) -> str: #vers 1
        """Return the geometry name in wheels.DFF for this vehicle's wheel type."""
        key = self.wheel_model.lower().split('_l')[0].lower()
        return WHEEL_TYPES.get(key, WHEEL_TYPES.get(self.wheel_model.lower(), 'wheel_saloon_l0'))


class VehiclesIDE:
    def __init__(self): #vers 1
        self.vehicles = {}   # model_name_lower -> VehicleEntry

    def get(self, model_name: str) -> VehicleEntry: #vers 1
        return self.vehicles.get(model_name.lower())


def parse_vehicles_ide(path: str) -> VehiclesIDE: #vers 1
    """Parse vehicles.ide and return VehiclesIDE."""
    result = VehiclesIDE()
    if not os.path.isfile(path):
        return result
    try:
        with open(path, 'r', errors='ignore') as f:
            lines = f.readlines()

        section = None
        for line in lines:
            stripped = line.strip()
            if not stripped or stripped.startswith('#'):
                continue
            low = stripped.lower()
            if low == 'cars':   section='cars'; continue
            if low == 'bikes':  section='bikes'; continue
            if low == 'boats':  section='boats'; continue
            if low == 'planes': section='planes'; continue
            if low == 'end':    section=None; continue

            if section not in ('cars','bikes','planes','boats'):
                continue

            # Split on comma or whitespace
            parts = [p.strip() for p in stripped.split(',')]
            if len(parts) < 4:
                continue
            try:
                e = VehicleEntry()
                e.model_id   = int(parts[0])
                e.model_name = parts[1].lower()
                e.txd_name   = parts[2].lower()
                e.veh_type   = parts[3].lower() if len(parts)>3 else ''
                e.handling_id= parts[4].lower() if len(parts)>4 else ''
                e.game_name  = parts[5]         if len(parts)>5 else ''
                # SA CARS: parts[11]=wheelfudge, [12]=wheelrearx, [13]=wheelfrontx, [14]=wheelscale
                if len(parts) > 14:
                    # wheel model is not directly in IDE — derive from model name or default
                    try: e.wheel_scale = float(parts[14])
                    except: e.wheel_scale = 1.0
                # Derive wheel type from handling or flags (SA uses separate file)
                # Default: saloon for cars, offroad for trucks/bikes
                if 'truck' in e.veh_type or 'van' in e.veh_type:
                    e.wheel_model = 'wheel_lightvan'
                elif 'bike' in section:
                    e.wheel_model = 'wheel_rim'
                elif 'plane' in section or 'boat' in section:
                    e.wheel_model = 'wheel_saloon'
                else:
                    e.wheel_model = 'wheel_saloon'
                result.vehicles[e.model_name] = e
            except (ValueError, IndexError):
                continue

    except Exception as ex:
        print(f"[vehicles_ide] Error: {ex}")
    return result


def get_vehicle_info(game_root: str, model_name: str) -> VehicleEntry:
    """Find vehicles.ide in game_root and return entry for model_name."""
    for sub in ('data/vehicles.ide', 'DATA/VEHICLES.IDE', 'data/VEHICLES.IDE'):
        path = os.path.join(game_root, sub)
        if os.path.isfile(path):
            ide = parse_vehicles_ide(path)
            return ide.get(model_name)
    return None
