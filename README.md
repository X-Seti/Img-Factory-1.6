# IMG Factory 1.6 — Development Progress Update
**GTA modding toolkit** — Build 435.126
PyQt6 · Python 3.11+ · Linux / Windows / macOS

<a href="https://postimg.cc/3WVBwsmf" target="_blank"><img src="https://i.postimg.cc/288X0m6Y/img-factory1-6-1.png" alt="img-factory1-6-1">

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


# Install - Pick your OS version

sudo apt install git
git clone https://github.com/X-Seti/Img-Factory-1.6


cd Img-Factory-1.6/

./chmod +x setup_imgfactory.sh

./chmod +x launch.py

./setup_imgfactory.sh

Windows

Download and run the .exe


## What is it?

IMG Factory is a multi-format modding suite for GTA III, Vice City, San Andreas,
the Stories games and **GTASOL** (multi-city). It covers the asset pipeline: open
archives, browse the game world, edit textures, collision, models and data files,
then rebuild.

---

## Quick start

```bash
python3 launch.py                               # IMG Factory with startup progress bar
python -m apps.components.Img_Factory.imgfactory  # direct
```

Each workshop also has its own standalone repo (Txd-Workshop, Col-Workshop, ...).
A workshop folder dropped into `apps/components` shows up on the Intro screen and
the Editing Options buttons; a missing one is hidden.

---

## Interface

- File window, Activity Log and button groups (IMG / Entries / Editing Options) are docks:
  move, float, close; right-click any dock title to reopen panels.
- Buttons as dock panels with a grid, or as ribbons (right-click > Buttons as Ribbons).
- Ribbon Manager: reorder, hide, move buttons between groups, add groups.
- Button text: icons only, text only, both, or auto (icons when the panel is narrow).
- Status bar at the window bottom, under the file pane, or in its own dock.
- App Settings: themes, per-colour transparency (on/off), panel image / gradient,
  Panel Test tab, see-through windows.

---

## Workshops (apps/components)

| Folder | Tool | What it does |
|---|---|---|
| Dat_Browser | DAT Browser | Load the game world: DAT, IDE, IPL, IMG load order, asset DB |
| Txd_Editor | TXD Workshop | Textures: browse, replace, convert (PC, PS2, Xbox, mobile) |
| Asset_Workshop | Asset Workshop | TXD Workshop branch for asset checks (rebuild in progress) |
| Col_Editor | COL Workshop | Collision: spheres, boxes, mesh faces, surface materials |
| Model_Editor | Model Workshop | DFF geometry, frames, mesh editing, textures |
| Model_Viewer | Model Viewer | DFF viewer |
| Map_Editor | Map Workshop | World map, IPL placement, map editing |
| Terrain_Workshop | Terrain Workshop | Sculpt, paint, paths, water, nature; DFF / COL / TXD / IDE / IPL export |
| Path_Workshop | Path Workshop | III / VC paths, SA nodes*.dat editing |
| Radar_Editor | Radar Workshop | Radar tiles |
| Water_Editor | Water Workshop | water.dat / waterpro.dat |
| Vehicle_Workshop | Vehicle Workshop | Vehicles, handling, carcols, animation |
| Handling_Editor | Handling Editor | handling.cfg |
| Ped_Workshop | Ped Workshop | Peds, skeletons, IFP animation, keyframes |
| Timecyc_Editor | Timecyc Workshop | timecyc.dat / timecycp.dat |
| Ide_Editor | IDE Editor | Object definitions |
| Ipl_Editor | IPL Editor | Item placement |
| Breakable_Editor | Breakable Editor | object.dat breakables |
| Scm_Workshop | SCM Workshop | main.scm / script.img |
| Hex_Editor | Hex Workshop | Binary editor |
| Disc_Workshop | Disc Workshop | CD / DVD images |
| Retro_GTA_Workshop | Retro GTA Workshop | GTA1 / GTA2 maps, styles, sprites |
| DP5_Workshop | DP5 Paint | Bitmap paint editor |
| Capture_Workshop | Capture Workshop | Screen / device capture |
| Ai_Workshop | AI Workshop | AI assistant |
| Master_Ide | Master IDE | ID range map |
| File_Editor, Img_Creator, Img_Browser, Project_Manager | Core | Dir tree, new IMG, IMG browser, projects |

---

## Asset Database

DAT Browser > DB: **Build DB** indexes all IMG / IDE / COL in the game folder;
**Update** re-indexes changed files. Stored as SQLite:
- Linux: `~/.local/share/imgfactory/<profile>.db`
- Windows: `%APPDATA%\ImgFactory\<profile>.db`

---

## GTASOL ID rules

World objects start at **ID 1987** or higher. IDs 0-1986 are base GTA3/VC:
0-129 peds, 130-239 vehicles, 240-299 components / weapons, 300-615 special map
objects, 616-1986 generics. Maximum safe ID (VC engine): **32767**.

---

## Settings

User settings are JSON in `~/.config/imgfactory/`, for example:

| File | Contents |
|---|---|
| `imgfactory_docks.json` | IMG Factory dock layout, button groups, ribbons, status bar place |
| `imgfactory_layout.json` | Splitter sizes |
| `<workshop>_ribbons.json` | Workshop ribbon layouts |
| `workshop_ui.json` | Status bar shown per workshop |
| `dat_browser.json` | Game path, auto-load, texlist folder |
| `welcome_prefs.json` | Show Intro on startup |

---

## Requirements

```bash
pip install -r requirements.txt
```

Core: PyQt6, Pillow, numpy, send2trash, requests. Optional per tool: PyOpenGL (3D
views), scipy / pycryptodome / python-lzo (TXD, IMG encryption, LZO), opencv-python /
pyaudio / mss (Capture), cairosvg / imageio (DP5), pygame, psutil, AI Workshop web
packages. See `requirements.txt`.

### Linux / WSL system libraries

PyQt6 needs system Qt6 / XCB libraries pip does not install. Run once after cloning:

```bash
chmod +x setup_imgfactory.sh
./setup_imgfactory.sh
```

WSL2 (Windows 11) needs WSLg; WSL2 (Windows 10) needs VcXsrv with "Disable access
control" checked.

---

## Repository structure

```
apps/
  components/   Workshops (one folder each; see table above)
  methods/      Shared parsers, editors, SVG icons, dock / ribbon systems
  gui/          Main window layout, button groups, taskbar, menus
  utils/        App Settings system (themes, colours, panels)
  themes/       46 colour themes (JSON)
  images/       Panel background images
docs/           AI rules, height rules, reference DB
Legal.md        Legal notice (also Settings > Legal)
```

---

## Credits

See `Credits.md`. Built with PyQt6; GTAMods wiki for format specifications;
GTASOL team for the master IDE range map.
