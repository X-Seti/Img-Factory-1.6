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
- Pick/goto settings (Aug 1 2026, per Keith): double-clicking an
  object in the viewport zooms in too tightly (currently a hardcoded
  distance) - needs a proper settings option for how close "go to
  object" zooms in, rather than one fixed value for everyone/every
  object size.
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
- Add real-time cycling play > stop [] in timecyc dock.
- day to night, show lights from 2dfx and timed objects.
- Adjustable time flow: 1 min for every Second adjustable. 
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
