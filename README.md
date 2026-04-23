# IMG Factory 1.6

**GTA modding toolkit** — Build 323  
PyQt6 · Python 3.11+ · Linux / Windows / macOS

---

## What is it?

IMG Factory is a multi-format modding suite for GTA3, Vice City, San Andreas and
**GTASOL** (multi-city). It handles the full asset pipeline: open archives, browse
the game world, edit textures, collision and models, then rebuild.

---

## Quick start

```bash
# Launch
python launch_imgfactory.py

# Or directly
python -m apps.components.Img_Factory.imgfactory
```

---

## Tools

| Taskbar | Tool | What it does |
|---|---|---|
| DAT | DAT Browser | Load game world (all IDEs, IPLs, IMGs at once) |
| TXD | TXD Workshop | Browse, preview, replace textures in .txd archives |
| COL | COL Workshop | Edit collision meshes — spheres, boxes, mesh faces |
| DFF | Model Workshop | View DFF geometry, frame hierarchy, linked textures |
| Paint | DP5 Workshop | Amiga-style IFF/ILBM paint editor |
| IDE | IDE Editor | Edit object definition files, ID map/analysis |
| IPL | IPL Workshop | World map, instance placement, water/radar overlays |
| SCM | SCM Workshop | Hex view + coordinate search in mission scripts |

---

## Asset Database (Build 316+)

Click **⬡ DB** in the DAT Browser toolbar to open the asset DB panel.

1. Load a game via DAT Browser
2. Click **⬡ Build DB** — indexes all IMG/IDE/.col files in the game folder
3. Next time: click **↻ Update** — only re-indexes changed files

The DB stores every IMG entry (name/offset/size), COL model names, TXD texture
names and IDE objects in a SQLite file at:
- Linux: `~/.local/share/imgfactory/<profile>.db`
- Windows: `%APPDATA%\ImgFactory\<profile>.db`

All workshops use the DB for instant asset lookup by model name.

---

## Model Workshop — DFF editing

### Toolbar (left panel)
- **V / E / F / P** — Vertex / Edge / Face / Polygon select mode
- **BF** — Backface culling (front faces only)
- **FP** — Front-only paint (skip back-facing faces)
- **+□** — Create primitive (Box/Sphere/Cylinder/Plane with subdivision counts)
- **Paint** — Opens Material List for texture assignment
- **COL** (Create COL from DFF) — Generates COL1/2/3 collision from DFF geometry

### Bottom panel
- **Prelight: [Apply] [Setup…]** — Vertex prelighting (coming soon: bake ambient +
  directional light into vertex colour channel for GTA3/VC/SOL)

### Textures
- **Auto-find TXD** — searches open IMG tabs → texlist/ folder → file browser
- **Scan texlist…** — scan `texlist/` folder for pre-exported PNG/IFF/TGA files
- **Set texlist folder** in Settings → Export → Texture Sources

---

## GTASOL ID rules

World objects must start at **ID 1987** or higher.

IDs 0–1986 are reserved for base GTA3/VC engine assets:
- 0–129: Peds
- 130–239: Vehicles  
- 240–299: Components/weapons/air train
- 300–615: Special map objects
- 616–1986: Generics

Maximum safe ID for GTASOL (VC engine): **32767** (signed int16 in SCM bytecode).

---

## Settings persistence

All per-tool settings are stored as JSON in `~/.config/imgfactory/`:

| File | Contents |
|---|---|
| `dat_browser.json` | Game path, auto-load, texlist folder |
| `model_workshop.json` | Texlist folder path |
| `model_toolbar_layout.json` | DFF/COL Workshop toolbar positions |
| `txd_dump_paths.json` | TXD dump dialog folder paths |
| `welcome_prefs.json` | Show intro on startup toggle |
| `<profile>.db` (data dir) | Asset database |

---

## Requirements

```
PyQt6 >= 6.4
PyQt6-Qt6
pillow (optional — some TXD format support)
```

```bash
pip install PyQt6 pillow
```

---

## Repository structure

```
apps/
  components/         Workshop tools (one folder per tool)
    Img_Factory/      Main window, welcome screen
    Dat_Browser/      DAT/IDE/IPL load-order browser
    Col_Editor/       COL Workshop
    Txd_Editor/       TXD Workshop
    Model_Editor/     Model Workshop (DFF)
    Ide_Editor/       IDE Editor
    Ipl_Editor/       IPL Workshop
    Scm_Workshop/     SCM Workshop
    DP5_Workshop/     DP5 paint editor
  methods/            Parsers, utilities, SVG icons
    asset_db.py       SQLite asset catalogue
    gta_dat_parser.py DAT/IDE/IPL parser + IDEDatabase
    img_core_classes.py IMG archive handler
    imgfactory_svg_icons.py All SVG icon definitions
  gui/                Main UI layout, taskbar, dockable toolbars
  themes/             37 built-in colour themes (JSON)
```

---

## Credits

Built with PyQt6.  
GTAMods wiki — format specifications.  
GTASOL team — master IDE range map.
