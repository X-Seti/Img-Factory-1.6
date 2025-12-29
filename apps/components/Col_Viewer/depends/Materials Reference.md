#this belongs in components/col_viewer/Materials_Reference.md - Version: 1
# X-Seti - October22 2025 - COL Viewer Materials Reference

# COL Surface Materials Reference

Data © 2005 by Steve M., illspirit

## Material Groups

| ID | Name       | Color  | Description 
|----|------------|--------|------------------------------------------
| 0  | Default    | Gray   | Default/Unknown surfaces 
| 1  | Concrete   | Gray   | Concrete, pavement, roads 
| 2  | Gravel     | Brown  | Gravel, rubble, roadside 
| 3  | Grass      | Green  | All grass types 
| 4  | Dirt       | Brown  | Dirt, mud, woodland 
| 5  | Sand       | Yellow | Sand, beach, desert 
| 6  | Glass      | Blue   | Glass windows, transparent 
| 7  | Wood       | Brown  | Wood crates, fences, floors 
| 8  | Metal      | Gray   | Metal gates, poles, containers 
| 9  | Stone      | Gray   | Rock, cliffs, stone stairs 
| 10 | Vegetation | Green  | Bushes, hedges, plants 
| 11 | Water      | Blue   | Water surfaces 
| 12 | Misc       | Orange | Miscellaneous (doors, carpet, etc) 

## San Andreas Materials (179 total)

### Common Materials
- **0** - Default
- **1** - Tarmac (roads)
- **4** - Pavement (sidewalks)
- **9-17** - Grass variants (short/medium/long, lush/dry)
- **26** - Dirt
- **28-33** - Sand variants (deep, compact, beach)
- **45-47** - Glass (standard, large windows, small windows)
- **42-44** - Wood (crates, solid, thin)
- **50-59** - Metal objects (garage door, lamp post, container)

### Special Materials
- **62** - Ped (pedestrian)
- **63-65** - Car materials
- **158** - Door
- **161-163** - Stairs (stone, metal, carpet)
- **175** - Unbreakable Glass
- **177** - Gore
- **178** - Rail Track

### Prefab Materials (P prefix)
Materials starting with "P" are prefab materials used in map construction:
- **74-79** - P Sand variants
- **80-82** - P Grass variants
- **89** - P Concrete
- **96-99** - P Underwater materials
- **102-108** - P Interior floors (bedroom, kitchen, 711, etc)

## Vice City / GTA III Materials (35 total)

### Common VC Materials
- **0** - Default
- **1** - Street
- **2** - Grass
- **5** - Concrete
- **7** - Glass
- **11** - Metal
- **18** - Sand
- **19** - Water
- **20-22** - Wood variants
- **25** - Hedge
- **26** - Rock

## Usage in COL Viewer

The viewer automatically:
1. Detects game version from COL format (COL1 = VC/III, COL2+ = SA)
2. Looks up material names from database
3. Displays material count in info bar
4. Logs material names to console

Example output:
```
🛡️ vehicle.col | COL3 | 4 spheres | 8 boxes | 124 vertices | 248 faces | 6 materials
📦 Materials: Metal Gate, Thick Metal Plate, Glass, Rubber, Car (panel)
```

## API Usage

```python
from apps.components.col_viewer.depends.COL_Materials import get_material_name, get_material_info

# Get material name
name = get_material_name(45, "SA")  # Returns "Glass"
name = get_material_name(7, "VC")   # Returns "Glass"

# Get complete info
info = get_material_info(45, "SA")
# Returns: {
#   'id': 45,
#   'name': 'Glass',
#   'group_id': 6,
#   'group_name': 'Glass',
#   'color': 'A7E9FC',
#   'game': 'SA'
# }
```

## Future Enhancements

Possible additions:
- Color-code faces by material group
- Filter faces by material type
- Material statistics panel
- Export material usage report
- Material color legend overlay

## Credits

Material definitions from Steve M's COL Editor II
Database implemented for IMG Factory 1.5 COL Viewer
