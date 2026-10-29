# IMG Factory 1.6 — Development Progress Update
**GTA modding toolkit** — Build 435.107
PyQt6 · Python 3.11+ · Linux / Windows / macOS

IMG Factory has come a long way since versions 1.2 and 1.5, and even the earlier stages of 1.6. What started as a GTA modding tool has grown into a much larger toolkit, bringing together archive management, model and texture editing, map tools, and a growing collection of specialised workshops.

I'm getting close to completing the main body of the project, but there's still plenty of work ahead. At this stage, the focus is shifting towards testing, integration, fixing bugs, and making sure the different tools work together properly.

# What's been done so far?

A lot has changed over recent weeks, so here's a summary of the main progress.

- IMG Factory interface: The main window is being brought into line with the other workshops, with dockable panels, movable button groups, configurable ribbons, a startup progress bar, improved status bars, and more control over themes, colours, transparency and button layouts.

- Core modding tools: The IMG archive handling, DAT Browser, TXD Workshop, COL Workshop, Model Workshop, Map Workshop and other tools have seen extensive development, including format support, editing features, save functions and bug fixes.

- Map and terrain editing: The Map Workshop has gained more world-editing features, while the new Terrain Workshop can create and sculpt terrain, paint materials, add game assets, create paths and water, and export game-compatible files.

- Models and characters: Model editing has gained mesh selection and editing, vertex and face manipulation, texture workflows and additional format support. The Ped Workshop now includes skeletons, animation controls, keyframes and pose editing.

- Specialised workshops: Path, Radar, Water, Vehicle, Asset, AI, Capture and other workshops have been developed or expanded. The new Retro GTA Workshop also brings tools for working with maps, tiles, sprites, missions and media from older GTA games.

- Shared interface and code: A lot of work has gone into consolidating common functions, removing duplicate code, fixing crashes and making the workshops more consistent. Build scripts and packaging support for Linux, Windows and macOS are also being developed.

# What happens next?

The next stage is to keep testing the existing features, find and fix problems, check that edited files save correctly, and verify compatibility with the different supported game formats. Some features already have specific checks behind them, but others still need more testing, including verification in the actual games.

There are also more components to add, including the Cutscene Editor, SCM tools and other planned workshops. The idea is that when a compatible component is added to the apps/components directory, IMG Factory can detect it and display the appropriate tool on the introduction screen, rather than requiring every tool to be permanently built into the main interface.

So, while the main framework is coming together, this isn't the finished product yet. There is still a considerable amount of testing, refinement and development to do, and I don't want to give the impression that everything is complete or fully verified.

For now, I'm continuing to build on what is already there, improve the overall experience and work towards a more complete, integrated modding suite. The changelog is the best place to follow the individual changes as development continues.

- Thanks to everyone following the project. There's a lot more to come!



Older Information - kept for now.
**GTA modding toolkit** — Build 370.80  
PyQt6 · Python 3.11+ · Linux / Windows / macOS

---

Information in this README is out of date. Will update soon.


- Model Workshop bleed-through when docked in IMG Factory — fixed
- TXD parser updated for GTA VC/III format (was only reading SA correctly)
- All `Mdl_Editor` import references replaced with `Model_Editor`
- Standalone repos (Col-Workshop, Txd-Workshop, Model-Workshop) now
  self-contained with required methods included
- DAT Browser bleed-through — fixed (wrong parent widget + missing paintEvent)
- Compact toolbar buttons now respond to splitter drag, not just window resize

---

## What is it?

IMG Factory is a multi-format modding suite for GTA3, Vice City, San Andreas and
**GTASOL** (multi-city). It handles the full asset pipeline: open archives, browse
the game world, edit textures, collision and models, then rebuild.

---

## Quick start

```bash
# Launch (menu - pick a tool)
python3 launch.py

# Launch IMG Factory directly
python3 launch.py 1

# Or directly via module
python -m apps.components.Img_Factory.imgfactory
```

| # | Tool |
|---|---|
| 1 | IMG Factory 1.6 |
| 2 | TXD Workshop |
| 3 | COL Workshop |
| 4 | Model Workshop |
| 5 | DP5 Workshop |
| 6 | AI Workshop |

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

Python modules (installed via pip):

```
PyQt6 >= 6.4
PyQt6-Qt6
Pillow
numpy
send2trash
```

```bash
pip install -r requirements.txt
```

### Linux / WSL system libraries

PyQt6 also needs system-level Qt6/XCB libraries that pip does not install.
Without these, the app will fail to start, or on WSL specifically may hang
with no error on launch.

Run the setup script once after cloning — it detects your distro
(Ubuntu/Debian, Arch, Fedora, openSUSE) and WSL, then installs the correct
system packages and Python modules:

```bash
chmod +x setup_imgfactory.sh
./setup_imgfactory.sh
```

WSL2 (Windows 11) needs WSLg, which ships by default. WSL2 (Windows 10)
needs VcXsrv running separately with "Disable access control" checked.
See `setup_imgfactory.sh` for details.

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
    Map_Editor/       Map Workshop
  methods/            Parsers, utilities, SVG icons
    asset_db.py       SQLite asset catalogue
    gta_dat_parser.py DAT/IDE/IPL parser + IDE database
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
