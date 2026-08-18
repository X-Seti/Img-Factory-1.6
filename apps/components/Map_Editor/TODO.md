# TODO

Extracted from inline `#TODO` comments in map_workshop.py, per Keith
(Aug 1, 2026), to keep the source file cleaner.

## Known bugs / rough edges

- Some GTA3 DFF files show as unknown format - affects both standalone
  and docked versions, when loading files from IMG files.
- [FIXED] Missing splitter between the middle panel and right panel
  (or between the right panel and the "middle panel" that got moved
  to its right) - dock layout didn't have a proper resize handle
  there. Confirmed fixed by Keith (Aug 1 2026).
- "X" close button on collapsible dock title bars: the right-click
  menu recovery for bringing a closed dock back doesn't fully work yet
  ("use the View menu or another dock's right-click menu to bring it
  back" - neither path is fully wired).
- Double-clicking a dock's section header (title bar) to collapse/
  expand seems to work best as the primary interaction - worth
  confirming this is the intended, discoverable way for users to find
  this feature.
- The float/dock toggle button on collapsible dock title bars is
  currently disabled (commented out) - it's meant to affect a bar's
  own open/collapse state, not the surrounding bars that get collapsed
  when it's used; needs rethinking before re-enabling.
- Object Browser's width can still lock up in some cases (reported by
  Keith, Aug 1 2026) - a `QStackedWidget` sizing bug (largest page
  forcing the minimum regardless of visible page) was found and fixed,
  but the lock was still reported afterward; root cause not fully
  confirmed yet.
- `_COMPACT_BUTTON_H`/`_COMPACT_ICON_SIZE` at 18px: text can get
  clipped/corrupted below a minimum-18/maximum-20 range for some
  button styles - needs a general, consistent fix across all the
  places 18px compact sizing is used, not just the one already fixed
  in Control Panel. [Aug 1 2026: audited every `setFixedHeight(18)`
  call in map_workshop.py - none are on a QSpinBox/QDoubleSpinBox
  outside the Item Editor Dialog's Position/Rotation/Scale rows,
  already bumped to 24px earlier in this same file's history. The
  clipping issue specifically (spinboxes needing more room than
  buttons do at the same height) appears isolated to that one place
  and already resolved - leaving this open only for the general
  "still worth a consistent style pass" concern, not a known
  remaining clipping bug.]

## Missing icons

- IPL tab's "Close" button reuses the generic close icon - needs its
  own icon.
- IPL tab's "New" button - needs a dedicated "create new IPL" icon
  (currently disabled/stub anyway, see below).

## Stub / not-yet-built functionality

- `apply_changes` - commit pending edits back to DFF/COL data isn't
  wired up yet.
- `_apply_prelighting` - bake ambient + directional light into DFF
  vertex colour channel isn't implemented (needs light_dir,
  ambient_colour, diffuse_colour from a setup dialog that doesn't
  exist yet either).
- Object Browser's Add/Delete/Rename actions are in-memory only for
  now (mutating `self._all_instances` and the loader's own instances
  list) - writing changes back to the actual IPL/IDE files on disk
  isn't built yet.
- No write-back infrastructure exists for any file type in Map
  Workshop yet (creating/deleting IPL files from disk, etc. are all
  stubs).
- Undo/redo for mapping changes (instance placement, rotation, IPL
  edits) isn't implemented - needs its own instance/IPL-state design,
  since the old raster/pixel-based undo system (from the DP5 paint
  canvas era) isn't portable to this.
- [DONE Aug 1 2026] Pick/goto settings: double-clicking an object in
  the viewport zoomed in too tightly (was a hardcoded distance) -
  added self._goto_zoom_distance (default 40.0, matching the value
  Keith had already settled on), exposed as a spinbox in the same Nav
  settings popup used for mouse sensitivity. Still not scaled to the
  object's own size - a further refinement, not done here.
- Snap function (Aug 1 2026, per Keith): "the biggest problem
  sometimes with making models is sometimes there are gaps, so we
  need a snap function" - snapping vertices/objects together to close
  gaps when building/positioning models. Not designed yet.
- Smooth mesh function (Aug 1 2026, per Keith, same context as snap
  above) - smoothing a mesh's surface. Not designed yet.
- Right-click menu on the IPL Inst File table (Aug 1 2026, per Keith):
  "load into model workshop" and "edit the model in map editor" -
  exact intended behavior for these two needs clarifying before
  building (how they should differ from each other, and from what
  double-clicking already does) - Info and Show Textures were
  straightforward enough to add directly; these two weren't.


## Item Editor Dialog redesign (Aug 1 2026, Keith's spec)

Keith laid out a fuller redesign for the Item Editor Dialog
(_InstanceEditPanel), using a real example (veg_palmkb2, ID 451,
nbeach.ipl). Implemented Aug 1 2026:

- [DONE] Header/label reads "[IPL object editor] ID 451 | veg_palmkb2
  | nbeach.ipl".
- [DONE] Identity section shows both raw lines verbatim (IPL inst
  line and matching IDE line, reconstructed from parsed fields - not
  the original file text, which isn't kept in memory) plus a note on
  which TXD is expected.
- [DONE] Added a Scale nudge section (Position/Rotation already had
  one, Scale never did) plus a "[Set Scaling to 0]" button.
- [DONE] Placement Info's Interior stays as text; 2DFX/TOBJ are now
  [2DFX (n)]/[TOBJ (n)] buttons showing a popup with details on
  click, instead of always-visible text blocks.
- [DONE] Bottom row is now [Apply] [Undo] [Close] [Save] - Apply/Undo/
  Save are honest stubs (clear popup explaining why, not silently
  doing nothing) since none of what they'd need exists yet:

Still open:
- Todo Show objects in map workshop with alpha object layers working.
- [DONE Aug 1 2026] TOBJ time-of-day support: parse time_on/time_off
  (were being silently dropped before), a Time switch + QTimeEdit in
  IPL Controls filtering TOBJ instances by simulated time, Play/Stop/
  Settings buttons with a real QTimer actually advancing time and
  live-refreshing which TOBJ instances show. Settings cog controls
  in-game-minutes-per-tick and real-seconds-per-tick separately,
  giving the "1 min for every Second adjustable" rate Keith asked for
  (not a single fixed ratio). Time and Nav moved to their own row per
  Keith's follow-up.
- Remaining, not yet started - each needs its own rendering-side
  design pass: day/night shading (ambient/directional lighting or
  fog color shifting based on the simulated time, so the world
  visually looks different at different hours, not just which TOBJ
  instances show/hide) and 2DFX objects lighting up at night
  specifically (2DFX light-source entries rendering an actual glow/
  light effect, gated by the same simulated time).
- Todo (Keith's own words): "The above can be edited and saved; any
  changes are updated in the main viewpoint" - editing the raw
  IPL/IDE line text directly (not just the existing Position/
  Rotation/Scale nudge controls, which already do apply live), with
  actual write-back to disk. Depends on the general write-back
  infrastructure noted elsewhere in this file.
- Todo (Keith's own words): "There are other buttons that can go here
  that switch the view. SA has other sections" - SA's IDE format has
  additional section types (peds/cars/hier/etc. beyond objs/tobj)
  that this dialog doesn't account for yet.
- Todo (Keith's own words): "Show info can be removed and added to
  the right-click on the model" - reconsider whether double-click
  should still open this directly once right-click "Info" covers the
  same thing, or keep both.
- Real Undo (currently a stub) - same underlying design work as the
  general undo/redo item above.


## Compare TXD (TXD Workshop, Aug 1 2026, Keith's request)

Per Keith: "Compare TXD should be an option for txd workshop, as we
have generic.txd and Generic.txd, it would say list both txd, and
highlight the extra txd." This is a TXD Workshop
(apps/components/Txd_Editor/txd_workshop.py) feature, not Map
Workshop - a real screenshot showed his actual game folder has both
"Generic.txd" (348.0 KiB) and "generic.txd" (256.4 KiB) as two
different files. A "Compare TXD" option would scan for name
collisions like this (case-different or otherwise duplicate TXD
names across indexed locations) and list/highlight them so the
conflict is visible rather than silently resolved one way or another.
Directly related to why the fallback logic was removed from Map
Workshop's texture loading (see CHANGELOG.md) - Keith's stated
principle is "there should be no fallbacks, it should either work or
fail," and a duplicate-detection feature is the right way to surface
this kind of conflict instead.

## Interactive object movement (Aug 1 2026, Keith's request)

Substantial features, not started - each needs its own design pass:

- Clarify what "semi-solid" render mode should actually look like
  (fixed reduced opacity applied globally to solid shading? something
  else?) - Textured/Non-Textured/Wireframe are wired in IPL Controls'
  Render dropdown, this one is genuinely ambiguous as stated.
- Gizmo-based free object movement: "Pressing Ctrl and left-clicking
  should freely move that object anywhere on the x, y, z axis; the
  centre blue, red, and green thingy... should have clickable arrows
  to lock the pane when freely moving objects." Needs: mouse-drag
  detection distinct from camera-orbit dragging, ray/plane
  intersection math to convert a 2D drag into a 3D position change
  along a constrained axis or plane, rendering an actual move gizmo
  (3 colored arrows/axes at the selected object's position) in the
  viewport, and per-axis lock state driven by clicking the gizmo's
  own arrows.
- Object-to-object snapping while moving: "I should be allowed to
  snap to object map objects, from the side, edge, or middle to
  middle on another object, updating the snap tools." Depends on the
  gizmo movement above existing first - needs proximity detection
  against other loaded instances' bounding geometry, snap-point
  calculation (side/edge/center-to-center), and visual snap feedback.

## Follow-ups from Aug 1 2026 batch

- Alpha-textured objects: Keith raised this again ("textures with
  alpha layers, these need to show like they do in the game") after
  the GL_ALPHA_TEST fix was already pushed - confirmed the fix is
  still correctly in place in the code, but couldn't verify the
  actual visual result (no PyOpenGL in this sandbox). Needs Keith's
  specific feedback on whether it's still wrong, and if so which
  objects/textures, since a hard 0.5 cutout threshold may not match
  every case (some GTA textures might want smoother blending instead
  of a hard cutout).
- VehicleViewport (apps/methods/dff_viewport.py) has its own separate
  mouseMoveEvent override with the identical yaw-uncompensated pan
  bug just fixed in the base DFFViewport class - out of scope for
  this Map Workshop session, but worth fixing there too since Vehicle
  Workshop would have the exact same "mouse left doesn't always mean
  screen left" issue.
- Nav settings popup currently only has mouse sensitivity - Keith
  asked for "other needed settings" too, not yet specified which
  ones.

## Model Workshop 2DFX Editor (Aug 1 2026, Keith's request)

Per Keith: "then after for model_workshop, 2dfx editor, where we can
edit the model with the 2dfx objects." A separate, substantial
feature for a different component (apps/components/Model_Editor/
model_workshop.py, not Map Workshop) - editing a model's own 2DFX
entries (adding/removing/repositioning lights and other effect types,
adjusting color/range/corona size) rather than Map Workshop's
world-view display of them. Not started - needs its own design pass:
a UI for listing a model's current 2DFX entries (now parseable in
detail, see the 2DFX light-parsing work in gta_dat_parser.py this
session), editing each entry's fields, a 3D gizmo or coordinate
entry for positioning a light's offset relative to the model, and
write-back to the IDE file (depends on the general write-back
infrastructure noted elsewhere in this file).

## Cross-component dead status-bar code (Aug 1 2026, found while fixing Map Workshop's)

Model Workshop, COL Workshop, and Map Workshop's own setup_ui all had
the identical `if hasattr(self, '_setup_status_indicators'):` guard -
that method is only ever actually defined in TXD Workshop
(txd_workshop.py), so the check silently evaluates False everywhere
else and no status bar/label ever gets built for those three. Fixed
for Map Workshop this pass (see CHANGELOG.md); Model Workshop and
COL Workshop still have the same dead code and almost certainly the
same "status messages never actually reach the UI" symptom, just not
reported yet - worth the same fix there too.

## Map Workshop taskbar presence when embedded in IMG Factory (Aug 1 2026)

Per Keith: "Map editor isnt needs to show on the task bar in img
factory, and any tools called like ide editor, ipl editor." Two
distinct pieces:

- [DONE Aug 1 2026] The Item Editor Dialog (the floating dock/panel
  for editing a single IPL instance) - Qt's own default floating-
  QDockWidget behaviour uses Tool-type window flags, which most
  window managers deliberately exclude from the taskbar by design
  (same treatment as a floating toolbar/palette). Fixed by explicitly
  switching its window type field to Window after floating. Verified:
  the dock's window type field confirmed genuinely Window, not Tool.
  Any other similar floating dock/dialog "editor" windows should get
  the same treatment if this is found not to cover everything Keith
  meant by "any tools called like ide editor, ipl editor."

- NOT fixable purely from within this component: Map Workshop itself,
  Keith confirmed, currently runs *embedded as a tab/panel inside IMG
  Factory's own main window* when opened that way - a genuine child
  widget with no top-level window of its own at all, not merely a
  window with the wrong flags. A child widget embedded in a parent's
  layout cannot have independent taskbar presence regardless of any
  flags set here; giving Map Workshop its own taskbar entry while
  running from inside IMG Factory would need it to become (or be
  optionally switchable to) its own top-level window instead of a
  tab - a decision and implementation on IMG Factory's own launching
  code, outside apps/components/Map_Editor/map_workshop.py entirely.

## Binary IPL writer - "make our own binary ipls" (Aug 1 2026, Keith's request)

Per Keith: "in time write data to the binary.ipl, and make our own
binary ipls." Not started - the read side (BinaryIPLParser) needs to
stay proven reliable first, per its own docstring, before building a
writer that could round-trip through it.

The confirmed binary format (from BinaryIPLParser's own docstring,
verified against Keith's real sample files - crack.ipl/
countn2_stream1.ipl):
- Magic: `b"bnry"` (4 bytes)
- Header: 18 x int32 LE (72 bytes) - only 2 of 18 fields confirmed:
  index 0 = inst_count, index 6 = 76 (constant, header size). The
  other 16 are presumably offsets/counts for cull/zone/other sections
  (per the text-format IPL_SECTIONS list) but which index maps to
  which section isn't confirmed yet - a writer only handling `inst`
  data could zero-fill the unconfirmed fields, but that would produce
  a file GTA's own engine might not accept if it expects those other
  sections to actually be present/located correctly.
- Each inst record: 40 bytes = 7x float32 LE (pos_x/y/z, rot_x/y/z/w)
  + 3x int32 LE (model_id, an unconfirmed flags-like field - not
  interior, see the parser's own docstring, mostly powers of 2 with
  one outlier - meaning is not confirmed - lod_index).

A round-trip writer (`IPLInstance` list -> raw bnry bytes) is
feasible for the `inst` section alone using this confirmed layout,
but "make our own binary IPLs" that GTA's engine will actually load
needs the unconfirmed header fields and cull/zone/other section
formats worked out first, or the writer would only ever be useful for
Map Workshop's own round-trip (write then re-read with our own
parser), not for producing files the real game accepts.

## DXT3/DXT5 texture decoding still pure-Python (Aug 1 2026)

_decode_dxt1 was rewritten to a vectorized numpy fast path (see
CHANGELOG.md) after Keith's real crash trace showed a freeze/high
memory usage deep inside its per-pixel decode loop. _decode_dxt3 and
_decode_dxt5 have the identical pure-Python per-pixel loop pattern
and very likely the same performance problem for large textures -
not yet fixed, deliberately scoped out of this pass to keep the DXT1
fix itself thoroughly verified (byte-for-byte correctness against
the original loop, across exact-multiple-of-4, edge-clipping,
truncated-data, and both color-ordering branches) rather than
rushing three decoders through in one turn. Same approach (reshape/
transpose/crop instead of a fancy-index scatter, which profiling
showed was actually slower than the original loop) should apply
directly to both.

## Mouse button reliability + game controller support (Aug 1 2026)

Per Keith: "right click held down rotates just fine, middle mouse
doesn't always work, left click to select object doesn't always work,
im thinking about adding keyboard shortcuts, arrow keys, and numpad
to rotate, but why stop there, we could use the thumbsticks on a
games controller."

- [DONE] Keyboard rotation (arrow keys + numpad, KeypadModifier-
  detected so numpad doesn't collide with top-row number keys
  elsewhere) - continuous rotation while held via a repeating QTimer,
  matching the feel of right-click-drag. Gives a reliable alternative
  to whatever's causing the mouse issues below, regardless of root
  cause.

- NOT fixed - mouse button reliability itself: reviewed DFFViewport's
  mouseMoveEvent/mousePressEvent/mouseReleaseEvent in full. Object
  selection is double-click-only (_pick_world_instance, a Möller-
  Trumbore ray/triangle test against every visible instance's
  geometry) - Keith describing this as "left click" suggests either a
  UX mismatch (expecting single-click to also work) or that double-
  click itself is what's landing inconsistently; the ray-pick logic
  itself wasn't found to have an obvious bug on inspection, though a
  precise ray needing to land exactly on a triangle is inherently
  less forgiving than a "click near an object" selection would be.
  Middle-click pan and right-click rotate use an `elif` chain in
  mouseMoveEvent (only one can process per move event) and `_view_
  locked` is checked for rotate but not pan - neither found to
  directly explain "middle sometimes doesn't work, right always
  does" though. Genuinely couldn't rule out an OS/window-manager/
  driver-level cause (e.g. middle-click-paste being a common X11
  convention that could intercept the button before this app ever
  sees it) without being able to reproduce interactively - worth
  Keith checking whether the same flakiness happens in a completely
  different app's own middle-click handling, to help separate "this
  app's bug" from "system-level middle-click behavior."

- NOT started - game controller/thumbstick support. Real feature, not
  a quick add: Qt itself has no built-in gamepad API (would need
  QtGamepad specifically, a separate PyQt6 package not currently a
  dependency - needs checking whether it's actually available/
  installable in Keith's environment) or a third-party library like
  pygame's joystick module or inputs/evdev directly. Also needs a
  polling loop (gamepad state isn't event-driven the way keyboard/
  mouse are) - would reuse the same QTimer-driven continuous-rotation
  pattern the new keyboard shortcuts just established, reading stick
  axis values each tick instead of a fixed per-tick step. Scoping
  this out until the mouse/keyboard side is confirmed solid and
  Keith wants to prioritize it specifically.

## LOD Test tool future expansion (Aug 1 2026)

Per Keith: "this function in the future can be explanded. (todo)" -
now bidirectional (see CHANGELOG). Not scoped yet, but logging the
open door: possible directions include a configurable circle radius
independent of the draw-distance threshold, multiple simultaneous
test circles, a fixed (non-mouse-following) test point for
screenshot/comparison purposes, or extending the same live-switching
mechanism to render mode (Textured/Wireframe/etc.) rather than just
LOD detail level.

## Three duplicate settings dialogs exist (Aug 1 2026)

Per Keith: "class MapSettingsDialog(QDialog): is where the new
settings should be, I don't see Map Assits tab with the Advance
settings moved too?" Investigated and found the earlier Loading/Map
Assets tab work had gone into the wrong dialog entirely.

There are THREE separate, parallel settings-dialog implementations in
this file, likely accumulated from the Model-Workshop-base porting
process without ever being consolidated:

1. `class MapSettingsDialog(QDialog)` - a full, well-structured dialog
   (Canvas/Ribbons/Interface/Widgets/Menu/Gadgets/Loading/Map Assets/
   Viewport tabs, uses the real MapSettings persistence via self.s.set)
   but **never actually instantiated anywhere in the file**. Dead code.
   This is where the Loading/Map Assets tabs were first added -
   wasted work, since nothing ever opens this class.

2. `_show_settings_dialog` (Display/Preview/Export/Import/Constraints/
   Keyboard Shortcuts tabs) - only reachable via a keyboard shortcut
   (hotkey_settings), not any visible button or menu item.

3. `_show_workshop_settings` (Fonts/Display/Performance/Preview tabs)
   - THE REAL ONE, wired to the actual top-bar Settings button
     (self.settings_btn.clicked). This is what Keith actually sees.
     Uses its own ad-hoc self.xxx = ... attributes for persistence,
     NOT MapSettings - a second, different mechanism from #1.

Moved the Loading/Map Assets tabs to #3 (the reachable one), using
MapSettings for their own persistence (matching what IPL Controls
actually reads from) even though the rest of #3's tabs don't.

While testing #3, found its own pre-existing "Apply Settings" button
was ALSO already broken independent of any of this - self.format_
combo and self.preview_widget.bg_color are both referenced but never
actually exist, meaning Apply Settings crashed outright for every tab
before this session, not just the new ones. Guarded both, and wrapped
the rest of that function's pre-existing logic in a broad try/except
as a pragmatic, time-bounded fix rather than auditing the whole
function line by line - can't rule out further undiscovered broken
references in there.

**Not done, worth doing eventually**: pick one dialog (almost
certainly #3, since it's the one users actually see) and either
delete #1 and #2 entirely, or migrate whatever's uniquely useful in
them (e.g. #1's Viewport pan/rotate button settings, Ribbon Manager
access) into #3, then remove the dead duplicates. Right now anyone
editing "the settings dialog" without knowing this history has a
1-in-3 chance of editing something invisible to the actual user.

## Path section parsing done - UI/visualization not started (Aug 1 2026)

`path` section (traffic AI paths, GTA3/VC) now parses correctly - see
CHANGELOG for the fix. Not yet done, needed to actually be useful:
- Show path groups/nodes anywhere in the UI (Object Browser tab? a
  new dedicated panel? overlay in the 3D world view as connected line
  segments between nodes, similar in spirit to the existing cull box
  wireframes)
- Editing (move a node, add/remove a node, change flags)
- Write-back to the .ipl file (the broader "write-back infrastructure
  for any file type" TODO item already covers this in principle)
- pick section support (separate, not yet started)
- cull zones as editable/renamable/resizable boxes (separate, not yet
  started - cull.ipl already parses via _parse_cull, but nothing lets
  Keith actually edit what it parses)
- IDE tobj/path/2dfx editor for Model Workshop (separate component
  from Map Workshop, not yet started)

## IPL Controls row 3 reserved for future visibility toggles (Aug 1 2026)

Per Keith, after moving LOD Test to a ribbon icon: "keep row3 for
future functions, like show tojb, show Paths, show zons." Row 3's
QHBoxLayout is now empty but intact, ready for:
- Show TOBJ (timed objects) visibility toggle
- Show Paths (the new path section, once it has any UI presence at
  all - see the earlier "Path section parsing done - UI/visualization
  not started" TODO entry)
- Show Zones (cull.ipl zones, once they have editable-box UI - see
  the earlier "cull zones as editable/renamable/resizable boxes" TODO
  entry)

## Pre-lighting bake, saved back to models (Aug 1 2026)

Per Keith, confirming Toggle Shading stays: "we want to keep toggle
shading, this can be used to generate pre-lighting, that can be saved
back to the models." Not started - the idea is using the viewport's
existing Lambertian shading calculation (currently just a live
preview effect) to actually bake per-vertex lit colors based on the
current Light Setup Dialog configuration, then write those back into
a model's own prelit color data (DFF geometry flag FLAGS_PRELIT /
the geometry's prelight color array) rather than only ever affecting
what's shown in this viewport. Would need: a "Bake Lighting" action
separate from the toggle itself, the actual per-vertex lighting-to-
color computation (probably reusing whatever the shading preview
already computes per-triangle/per-vertex, if anything is currently
stored that granularly rather than being a pure shader-style live
effect), and DFF write-back (ties into the broader "write-back
infrastructure for any file type" TODO item).

## 4-Pane View disabled - could be rebuilt world-aware later (Aug 1 2026)

Per Keith: "4 panels icon, keep, but the function isnt needed, it
creates a strange beheavour." Disabled rather than removed (icon
stays visible, greyed out, tooltip explains why). Root cause: `_sync_
quad_from_main` only ever mirrored single-model geometry attributes
(inherited directly from Model Workshop's single-DFF editing base),
never `_world_instances` - so it showed blank panes whenever an
actual map was loaded, Map Workshop's real primary use case. If a
genuine "4 world views from different angles" feature is wanted
later, it would need its own sync logic built around `_world_
instances` from scratch, not a fix to the existing single-model one.

## IDE tobj/path "add to ipl objects" - scope unclear, need to ask Keith (Aug 1 2026)

Per Keith's IDE section list: "tobj #to be added to ipl objects" and
"path #to be added to ipl objects" (IDE path, not IPL path - a
different section, model-related). tobj parsing itself is already
done (TOBJ time-flow feature, earlier session). Not clear yet exactly
what "added to ipl objects" means here - possibilities: (a) tobj/
IDE-path model entries should appear in the same Object Browser
listing as regular objs entries, currently separate/not shown there,
(b) something about how tobj-driven instances get included in the
IPL Inst File table specifically. Needs clarifying with Keith rather
than guessing at the wrong integration.

## PICK/JUMP/TCYC/AUZO/MULT still stub tabs (Aug 1 2026)

Now have tabs in IPL Controls (disabled, with tooltips) but no real
parsing/dataclasses - same treatment path/grge/enex got needs doing
for each once real sample data is available to verify against
(MULT is documented as unused by the game itself, may not be worth
building beyond the stub). AUZO specifically per Keith: "show audio
svg icons, plays the sound file" - real feature, needs its own sound-
file-playback mechanism (which audio format/path convention SA audio
zones actually reference needs research), not just parsing.

## Collision rendering follow-ups (Aug 14 2026)

Ghosted/Semi-Solid/Wireframe/Surface Mapped Col overlays are in, both
IMG-embedded (SA/VC) and COLFILE-directive standalone (GTA3/VC) COL
sources are now indexed - but:
- Only COL mesh (vertices/faces) is drawn - spheres and boxes
  (COLSphere/COLBox) aren't rendered at all yet.
- Not yet verified against Keith's real data at all - needs his
  confirmation that collision actually loads/draws correctly once he
  tests it, for all three games (SA/VC/GTA3).

## col_3d_viewport.py field-mismatch bugs - FIXED (Aug 14 2026)

Found while checking col_workshop surface types, fixed per Keith: "if
you found a bug, we fix it". `apps/components/Col_Editor/depends/
col_3d_viewport.py` (IMG Factory 1.5 era) was written against a COL
shape that never matched the real, shared col_workshop_classes.py
dataclasses - every one of these would have raised/silently failed
against real data:
- `face.vertex_indices` -> real COLFace has separate `a`/`b`/`c` int
  fields (draw_face_mesh_shaded's solid-face loop, its wireframe
  overlay, and the set_current_model diagnostic print - 3 sites)
- `vertex.position.x/y/z` -> real COLVertex has `x`/`y`/`z` directly
  (same 3 sites)
- `model.name` -> COLModel has no top-level name, only
  `model.header.name` (diagnostic print)
- `model.bounding_box` / `hasattr(..., 'bounding_box')` -> real field
  is `model.bounds` (draw_bounding_box, its two call-site guards,
  fit_to_model) - this one meant draw_bounding_box() could never
  actually run at all, guarded out every time
- `sphere.center.x/y/z`, `box.min_point`/`box.max_point`,
  `bounds.min.x`/`bounds.max.x` -> COLSphere.center and COLBounds/
  COLBox min/max are plain (x,y,z) tuples, not objects with .x/.y/.z
  attributes (draw_collision_sphere, draw_collision_box - whose
  min_point/max_point hasattr check meant it always returned early
  and never drew a box at all - and fit_to_model)

Fixed all of them - real a/b/c and x/y/z field names throughout, a
small `_Vec3 = namedtuple('_Vec3','x y z')` module-level helper so
tuple fields (box/bounds min/max) can still be read with the
existing `.x`/`.y`/`.z` call sites unchanged, `*sphere.center`
unpacking for the single-use translate call. `ast.parse` clean;
smoke-tested every fixed access pattern directly against real
COLModel/COLHeader/COLBounds/COLVertex/COLFace/COLSphere/COLBox
instances (no OpenGL context needed for the field-access logic
itself) - passed. Still nothing currently imports this module
(confirmed project-wide), so still unverified in the running app,
but no longer known-broken if something does start using it.

## Ghosted render mode for LOD/Normal models (noted Aug 14 2026)

Per Keith: "Ghosted view could be useful for LOD and Normal" - a
passing suggestion, not yet scoped/implemented. Most likely reading:
when the LOD filter is set to "Show Both" (Normal + LOD together),
render one of the two ghosted so overlapping Normal/LOD meshes are
visually distinguishable - same overlay concept as the Col ghosting
already built (DFFViewport._draw_solid already takes an
alpha_multiplier, used for semi_solid - a low-alpha "ghosted" variant
would reuse the same mechanism). Needs Keith to confirm which of
Normal/LOD should be the ghosted one (or if he means something else
entirely) before building it.

## Remember all UI state (noted Aug 16 2026)

Per Keith: "Every UI change, splitter position, and cell size should
be remembered." A broader, systemic version of persistence gaps
already found/fixed piecemeal this session (settings not saving -
MapSettings.set() now auto-saves, debounced; individual column-width
persistence already exists for Object Browser and IPL Sections
specifically via ipl_sections_column_widths/object_browser_column_
widths). Not yet done as a general policy: splitter positions
(_outer_mw's dock splitters, any QSplitter in this UI), other tables'
column widths beyond the two already covered, dock geometry/
floating-vs-docked state, tab order, collapsed/expanded state of the
collapsible dock headers, and anything else that currently resets to
a hardcoded default every launch instead of remembering what the user
last had. Substantial scope - needs auditing every QSplitter/
QTableWidget/QDockWidget in the app and wiring each one's relevant
signal (splitterMoved, sectionResized where not already done,
dockLocationChanged, etc.) through to MapSettings, plus restoring
each on startup. Not started.

## Floating dialog windows: pin/stay-on-top option (noted Aug 16 2026)

Per Keith: "option tick on top of floating dialog windows to stay on
top" - a checkbox/toggle on floating dialogs (Item Editor Dialog,
Path Group Editor, etc.) to keep them above the main window rather
than getting buried when clicking back into the 3D view. Not started.

## Path format conversion between GTA3/VC/SA (noted Aug 16 2026)

Per Keith, describing another user's request: "When looking at path
files for GTA 3, vc or SA, have the ability to convert between them.
With GTA 3 path files, have the ability to copy and save as
paths.ipl; this means scanning all the GTA IDE files for the path
data." Depends on GTA III's IDE-embedded path groups (now parsed,
see CHANGELOG) actually being resolved to real world-space
coordinates first - each group's XRel/YRel/ZRel are relative to
wherever that group's own model_id is actually PLACED via an INST
line, so converting to VC's absolute-coordinate paths.ipl format
means: for every loaded IPL, find every instance whose model_id
matches an IDEPathGroup, apply that instance's own position+rotation
transform to each relative node position, and write the result out
in VC's PathGroup/PathNode text format. Not started - needs the
instance-transform resolution step built first (also needed for
correctly rendering GTA III paths in the viewport at all, since
right now the parsed IDEPathGroup data has no rendering path of its
own yet either).

## Deferred: other path-adjacent files (noted Aug 16 2026, per Keith:

"there are also other path files, i put them in last, those can be
added later, add todo") - flight.dat, flight2.dat, flight3.dat (SA
aircraft flight paths?), spath0.dat (a numbered SA sub-path file? -
naming suggests spath1.dat/spath2.dat/etc likely also exist).
Uploaded but not yet inspected/parsed - deliberately lower priority
per Keith's own framing. Not started.

## File-type support audit against Keith's real, complete GTA III data (Aug 16 2026)

Per Keith: "here are all the data files, we can also check if we have
supported all of them, if there are any files we haven't addressed,
add them to the TODO list, for GTA3/GTAIII" - full inventory from his
uploaded data_all_files.7z (a real, complete GTA III install's data
folder, loaded end-to-end via GTAWorldLoader to confirm: 8689
instances, 870 IDE path groups, 3081 objects, zero errors).

### Already supported (confirmed working against this real data)
.ide, .ipl (inst/cull/zone/path/occl - grge/enex recognized but no SA
sample data exists in a GTA III install to confirm), .col, .dat
(gta3.dat itself), .zon, binary IPL streams, GTA III's own IDE-
embedded path format.

### Genuinely relevant, not yet supported - map/path-adjacent data
- `paths/CHASE0.DAT` through `CHASE19.DAT` (not sequential - files
  found: 0,1,2,3,4,5,6,7,10,11,14,16,18,19) - binary, ~150KB each,
  mission-specific vehicle chase paths. Format not investigated yet.
- `paths/flight.dat`, `flight2.dat`, `flight3.dat`, `flight4.dat` -
  aircraft flight paths (4 files total, not 3 as earlier assumed
  before this real inventory) - explicitly deferred by Keith earlier
  ("there are also other path files, i put them in last, those can be
  added later, add todo"), still not started.
- `paths/tracks.dat`, `tracks2.dat` - **DONE (Aug 17 2026)**: train
  track waypoints, real parser verified against Keith's own files
  (168/557 waypoints, exact coordinate match), full viewport
  rendering (Tracks checkbox, one line strip per track, no node
  markers). Not referenced in gta.dat/gta3.dat's own directive list -
  loaded from a fixed, well-known relative path instead.
- `train.dat`, `train2.dat` - inspected while adding tracks.dat
  support above: NOT the same simple waypoint format. Comma-separated,
  no count header, 14 values per line - looks like station position
  (X,Y,Z), a `999,999,999` sentinel (possibly "no linked track" for
  some entries), two more (X,Y,Z) triplets (possibly linked-track
  positions), then 2 more values (unclear - possibly a speed/angle/
  door parameter). Real data, not yet understood well enough to
  parse correctly - needs more investigation before implementing,
  not a simple follow-on from tracks.dat despite the similar name.
- `CULLZONE.DAT` - GTA III's own binary cull-zone equivalent, exists
  alongside the real, working cull.ipl - confirmed unused by the game
  itself (same situation already documented for VC's own Cullzone.dat
  earlier this session) - informational only, real priority is low.
- `water.dat`, `waterpro.dat`, `GTAiii_water.dat`, `GTAiii_waterpro.dat`
  - water level/property data, not parsed at all currently. Two pairs
  of near-identical files (water.dat and GTAiii_water.dat are the same
  size, 2639 bytes - likely the same content or a versioned pair).
- `object.dat` - object physics properties (mass, elasticity, air
  resistance, uproot limit, collision damage multiplier per object
  name) - plain text, human-readable, confirmed by inspection. Not
  parsed at all currently - would let Map Workshop show/edit an
  object's physical behaviour alongside its visual placement.
- `CAPS.DAT` - only 16 bytes, purpose not determined - too small to
  be meaningful map/path data, likely a minor engine-internal config
  value; probably not worth pursuing unless a specific need comes up.

### Confirmed out of scope for Map Workshop (not map/world data)
`carcols.dat` (vehicle paint colours), `handling.cfg` (vehicle
physics), `weapon.dat` (weapon stats), `ped.dat`/`pedgrp.dat`/
`pedstats.dat` (pedestrian behaviour), `fistfite.dat` (melee combat
moves), `surface.dat` (surface/audio material properties),
`animviewer.dat` (animation viewer tool config), `main.scm` (compiled
mission script) - all real GTA III systems, none of them map/world
placement data, all belong to entirely different tools/systems than
what Map Workshop does.

### Already covered by a different tool
`timecyc.dat` (weather/lighting cycles) - Timecyc Workshop already
exists as its own standalone tool for this, not Map Workshop's scope.

### Unclear, not investigated
`default.dat` - a second, alternate main .dat file with the same
directive structure as gta3.dat (IDE/COLFILE/etc.) - purpose relative
to gta3.dat not determined (possibly a menu/loading-screen or testing
configuration) - not investigated further, low priority unless a
specific need comes up.
