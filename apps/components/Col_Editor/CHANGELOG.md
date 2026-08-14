# Changelog

- **Aug 14, 2026** — Fixed `col_3d_viewport.py`'s field-name
  mismatches against the real `COLModel`/`COLFace`/`COLVertex`/
  `COLSphere`/`COLBox`/`COLBounds` dataclasses (`col_workshop_
  classes.py`) - found while checking Col Workshop's surface-type
  colours for Map Workshop's new collision render options, fixed per
  Keith: "if you found a bug, we fix it". This file (IMG Factory 1.5
  era) was written against field names that never matched the
  current shared classes:
  - `face.vertex_indices` -> real field is separate `a`/`b`/`c` ints
  - `vertex.position.x/y/z` -> real fields are `x`/`y`/`z` directly
  - `model.name` -> no such field, only `model.header.name`
  - `model.bounding_box` -> real field is `model.bounds` (meant
    `draw_bounding_box()` could never actually run - guarded out by
    a `hasattr` check that always failed)
  - `sphere.center.x/y/z`, `box.min_point`/`max_point`, `bounds.min.x`/
    `max.x` -> `COLSphere.center` and `COLBounds`/`COLBox` min/max
    are plain `(x,y,z)` tuples, not objects with `.x`/`.y`/`.z`
    (`box.min_point`/`max_point` don't exist at all - `draw_
    collision_box()` always returned early and never drew anything)

  Fixed throughout with the real field names, plus a small
  `_Vec3 = namedtuple('_Vec3', 'x y z')` helper so tuple fields can
  still be read with the existing `.x`/`.y`/`.z` call sites
  unchanged rather than rewriting every line to bracket indexing.
  `ast.parse` clean; smoke-tested every fixed access pattern directly
  against real dataclass instances (no OpenGL context needed for the
  field-access logic itself) - passed. Nothing in the project
  currently imports this module (confirmed project-wide), so this
  remains unverified in the running app, but is no longer known-
  broken if something starts using it.
