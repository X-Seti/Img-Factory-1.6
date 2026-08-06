# TODO

Extracted from inline `#TODO` comments in map_workshop.py, per Keith
(Aug 1, 2026), to keep the source file cleaner.

## Known bugs / rough edges

- Some GTA3 DFF files show as unknown format - affects both standalone
  and docked versions, when loading files from IMG files.
- Missing splitter between the middle panel and right panel (or
  between the right panel and the "middle panel" that got moved to
  its right) - dock layout doesn't have a proper resize handle there.
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
  in Control Panel.

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
nbeach.ipl):

- Header/label should read like "[IPL object editor] ID 451 |
  veg_palmkb2 | nbeach.ipl" - showing the ID, model name, and source
  IPL right in the title, not just in a sub-section.
- Identity section should show both raw lines verbatim: the IPL inst
  line itself (e.g. "451, veg_palmkb2, 0, -847.8391113, ...") and the
  matching IDE line (e.g. "451, veg_palmkb2, generic, 1, 45, 132"),
  plus a note on which IMG/TXD actually gets loaded for it (e.g.
  "gta3.img/generic.txd is loaded").
- Todo (Keith's own words): "The above can be edited and saved; any
  changes are updated in the main viewpoint" - editing the raw
  IPL/IDE line values directly, with live viewport sync and actual
  write-back to disk. Depends on the general write-back
  infrastructure noted elsewhere in this file.
- Placement Info's Interior/2DFX/TOBJ should become actual buttons
  ([2DFX] [TOBJ]) rather than read-only text sections.
- A "[Set Scaling to 0]" button, tying into the existing Ignore
  Scaling concept.
- Todo (Keith's own words): "There are other buttons that can go here
  that switch the view. SA has other sections" - SA's IDE format has
  additional section types (peds/cars/hier/etc. beyond objs/tobj)
  that this dialog doesn't account for yet.
- Bottom buttons: [Apply] [Undo] [Close] [Save] (currently only
  [Close] exists).
- Todo (Keith's own words): "Show info can be removed and added to
  the right-click on the model" - reconsider whether double-click
  should still open this directly once right-click "Info" covers the
  same thing, or keep both.
