"""
ModelCache - loads and caches DFF geometry + TXD textures for map
instance rendering, resolving models by name from a set of IMG
archives (GTAWorldLoader.get_img_paths()).

Design: indexing (which IMG archive + entry holds "foo.dff") is done
once, eagerly, when a world loads - that's just reading directory
headers, fast even for large archives. Actually loading and parsing
geometry/texture bytes is fully lazy - nothing is read/parsed until a
specific model is requested (get_geometry/get_textures), and the
result (success OR failure) is cached so a missing/broken model isn't
retried on every subsequent instance that references it.
"""

from typing import Dict, List, Optional, Tuple

from apps.methods.dff_parser import DFFParser, detect_dff
from apps.methods.txd_parser import parse_txd
from apps.methods.dff_classes import DFFModel
from apps.components.Model_Editor.depends.col_workshop_classes import COLModel
from apps.components.Model_Editor.depends.col_workshop_loader import COLFile


class ModelCache:
    """See module docstring."""

    def __init__(self): #vers 2
        # lowercase entry name (no extension) -> [(img_path, IMGEntry), ...]
        # A list, not a single tuple (Aug 1 2026) - if the same name is
        # indexed more than once (e.g. Keith's real game folder has
        # both "Generic.txd" and "generic.txd" as genuinely different
        # files), a single-tuple version would silently drop whichever
        # got indexed first, with no way to recover its content. See
        # get_geometry/get_textures for how duplicates get resolved.
        self._dff_index: Dict[str, List[Tuple[str, object]]] = {}
        self._txd_index: Dict[str, List[Tuple[str, object]]] = {}
        # lowercase entry stem -> [(img_path, IMGEntry), ...] for .col
        # entries found directly inside the game's own IMG archives
        # (Aug 14 2026, per Keith: "In SA it should be reading them
        # from the gta3.img... In VC, they can also be found in the
        # gta3.img file, just like the models") - same lightweight
        # container-name indexing as _dff_index/_txd_index (SA/VC
        # collision entries are named to match their model, one model
        # per .col entry, unlike the multi-model standalone archives
        # _col_index below is for). Checked first by get_collision.
        self._col_img_index: Dict[str, List[Tuple[str, object]]] = {}
        # lowercase model name -> [(col_file_path, model_index_in_file), ...]
        # (Aug 14 2026) For standalone .col files reached via COLFILE
        # directives in the .dat (GTAWorldLoader.get_col_paths) - per
        # Keith: GTA3 collision is ONLY reachable this way (no COL in
        # the IMG at all, paths point into data/maps/), VC also has a
        # handful of shared collision this way (e.g. generic.col)
        # alongside its IMG-embedded per-object collision, SA has none
        # of these (zero COLFILE directives - see get_col_paths).
        # Keyed differently to _col_img_index on purpose: these
        # standalone files are genuinely multi-model (e.g. generic.col
        # holds many separately-named collision models) - the
        # container filename is meaningless for lookup, only each
        # model's own header.name inside the file matches an
        # instance's model_name, so this indexes by content, built by
        # actually loading each file (see index_col_files).
        self._col_index: Dict[str, List[Tuple[str, int]]] = {}
        # lowercase model/txd name -> parsed result, or None if loading/
        # parsing failed (cached as None so it's not retried every time)
        self._geometry_cache: Dict[str, Optional[DFFModel]] = {}
        self._dimensions_cache: Dict[str, Optional[Tuple[float, float, float]]] = {}
        self._texture_cache: Dict[str, Optional[Dict[str, dict]]] = {}
        self._collision_cache: Dict[str, Optional[COLModel]] = {}
        # col_file_path -> loaded COLFile (Aug 14 2026) - same reasoning
        # as _opened_img_files: a multi-model .col can be large, don't
        # re-read/re-parse it from disk once per model name inside it.
        self._opened_col_files: Dict[str, object] = {}
        self.indexed_img_paths: List[str] = []
        self.indexed_col_paths: List[str] = []
        self.index_errors: List[str] = []
        # img_path -> already-opened IMGFile (Aug 1 2026, per Keith's
        # real crash trace: a Ctrl+C interrupt during "the app
        # freezes, no indication of doing anything" landed inside
        # IMGFile._open_version_2 -> entry.set_img_file, reached via
        # _read_entry -> img.open()) - _read_entry previously opened
        # the archive fresh on every single call, on the stated
        # assumption that "re-reading raw bytes on a cache miss is
        # cheap compared to re-parsing" - true for the *bytes* read
        # (read_entry_data opens its own short-lived handle for that
        # regardless, see its own docstring/implementation), false for
        # opening itself: IMGFile.open() has to parse the archive's
        # entire directory table (potentially thousands of entries for
        # gta3.img), and _preload_generic_ide_textures (generalized
        # earlier this session to cover every distinct TXD across a
        # whole loaded world, not just generic.ide's own) can call
        # _read_entry hundreds of times in one preload pass - hundreds
        # of full directory re-parses of the same archive, one per
        # distinct TXD, is exactly what a large SA map's worth of
        # objects turns into a multi-minute hang with no progress
        # shown. Caching the opened IMGFile here means the expensive
        # directory parse happens once per archive per session, not
        # once per texture lookup.
        self._opened_img_files: Dict[str, 'object'] = {}

    def index_img_files(self, img_paths: List[str]): #vers 2
        """Scan a list of IMG archive paths, building name -> (path,
        entry) indexes for .dff, .txd, and .col entries. Call once
        after a world loads (or its IMG set changes) - reading
        directory headers only, not entry contents, so this stays
        fast even for large archives. Safe to call again to re-index
        (clears previous indexes first).

        .col entries indexed here (Aug 14 2026) the same lightweight
        way as .dff/.txd - see _col_img_index's own comment for why
        that's correct for SA/VC's IMG-embedded collision specifically
        (one model per entry, named like the model) but not for the
        genuinely multi-model standalone archives index_col_files
        handles separately."""
        from apps.methods.img_core_classes import IMGFile

        self._dff_index.clear()
        self._txd_index.clear()
        self._col_img_index.clear()
        self.indexed_img_paths = []
        self.index_errors = []

        for img_path in img_paths:
            try:
                img = IMGFile(img_path)
                if not img.open():
                    self.index_errors.append(f"Failed to open {img_path}")
                    continue
                self._opened_img_files[img_path] = img   # reuse in _read_entry
                for entry in img.entries:
                    name = entry.name
                    if not name:
                        continue
                    stem, dot, ext = name.rpartition('.')
                    if not dot:
                        continue
                    stem_lower = stem.lower()
                    ext_lower = ext.lower()
                    if ext_lower == 'dff':
                        self._dff_index.setdefault(stem_lower, []).append((img_path, entry))
                    elif ext_lower == 'txd':
                        self._txd_index.setdefault(stem_lower, []).append((img_path, entry))
                    elif ext_lower == 'col':
                        self._col_img_index.setdefault(stem_lower, []).append((img_path, entry))
                self.indexed_img_paths.append(img_path)
            except Exception as e:
                self.index_errors.append(f"{img_path}: {e}")

    def index_col_files(self, col_paths: List[str]): #vers 1
        """Scan a list of standalone .col file paths, building a
        model-name -> (col_path, model_index) index across every
        model in every file (Aug 14 2026, for the IPL Controls
        collision render options - "load solid collision, load
        semi-solid, wireframe cols, and solid with surface mapping").
        Unlike index_img_files, this genuinely loads and parses each
        file up front (a .col has no separate lightweight directory
        header to scan the way an IMG does - COLParser has to walk
        the whole file to find each model's boundaries), so callers
        should only pass files actually worth indexing (e.g. found
        under the game root), not call this speculatively. Safe to
        call again to re-index (clears previous index first)."""
        self._col_index.clear()
        self._opened_col_files.clear()
        self.indexed_col_paths = []

        for col_path in col_paths:
            try:
                col_file = COLFile()
                if not col_file.load_from_file(col_path):
                    self.index_errors.append(f"Failed to open {col_path}: {col_file.load_error}")
                    continue
                self._opened_col_files[col_path] = col_file
                for i, model in enumerate(col_file.models):
                    name = getattr(model.header, 'name', None)
                    if not name:
                        continue
                    self._col_index.setdefault(name.strip().lower(), []).append((col_path, i))
                self.indexed_col_paths.append(col_path)
            except Exception as e:
                self.index_errors.append(f"{col_path}: {e}")

    def clear_indexes(self): #vers 3
        """Drop all indexes and cached geometry/textures/collision -
        call before re-indexing for a newly loaded world, so stale
        entries from a previous world can't leak through."""
        self._dff_index.clear()
        self._txd_index.clear()
        self._col_img_index.clear()
        self._col_index.clear()
        self._geometry_cache.clear()
        self._texture_cache.clear()
        self._dimensions_cache.clear()
        self._collision_cache.clear()
        self._opened_img_files.clear()
        self._opened_col_files.clear()
        self.indexed_img_paths = []
        self.indexed_col_paths = []
        self.index_errors = []

    def get_geometry(self, model_name: str) -> Optional[DFFModel]:
        """Get the parsed DFF geometry for a model name, loading and
        parsing (and caching the result either way) on first request.
        Returns None if the model isn't indexed, or its data failed to
        parse as a valid DFF - callers should fall back to the simple
        point/marker rendering in that case, not treat it as an error
        to surface to the user (a mod's IMG set legitimately might not
        contain every model referenced by its own map data).

        Tries every entry indexed under this name in order until one
        parses successfully (Aug 1 2026) - unlike textures, a model's
        geometry is one coherent mesh, not a set of independently
        useful named items, so duplicates can't be usefully merged the
        same way; this just means a duplicate that fails to parse
        doesn't block a working one indexed under the same name."""
        key = model_name.lower()
        if key in self._geometry_cache:
            return self._geometry_cache[key]

        result = None
        for img_path, entry in self._dff_index.get(key, []):
            try:
                data = self._read_entry(img_path, entry)
                if data and detect_dff(data):
                    result = DFFParser(data, model_name).parse()
                    if result is not None:
                        break
            except Exception:
                continue
        self._geometry_cache[key] = result
        return result

    def get_dimensions(self, model_name: str) -> Optional[Tuple[float, float, float]]:
        """(width, depth, height) in GTA's native local-space axes -
        width=X extent, depth=Y extent, height=Z extent - computed as a
        real axis-aligned bounding box from the model's own vertex data
        (not just the single bounding-sphere radius the parser already
        extracts, which can't give separate per-axis values). Returns
        None if geometry isn't available (same fallback contract as
        get_geometry - not an error to surface, just nothing to show
        yet). Result is cached alongside the geometry itself, so this
        is cheap to call repeatedly once a model's been loaded once."""
        key = model_name.lower()
        if key in self._dimensions_cache:
            return self._dimensions_cache[key]

        model = self.get_geometry(model_name)
        dims = None
        if model is not None and model.geometries:
            min_x = min_y = min_z = float('inf')
            max_x = max_y = max_z = float('-inf')
            found_any = False
            for geom in model.geometries:
                for v in geom.vertices:
                    found_any = True
                    min_x = min(min_x, v.x); max_x = max(max_x, v.x)
                    min_y = min(min_y, v.y); max_y = max(max_y, v.y)
                    min_z = min(min_z, v.z); max_z = max(max_z, v.z)
            if found_any:
                dims = (max_x - min_x, max_y - min_y, max_z - min_z)
        self._dimensions_cache[key] = dims
        return dims

    def get_textures(self, txd_name: str) -> Optional[Dict[str, dict]]:
        """Get the parsed textures for a TXD name, as a dict keyed by
        lowercase texture name (a TXD can hold multiple textures) -
        loading/parsing/caching on first request, same fallback
        contract as get_geometry: None means 'not available', not an
        error to surface.

        Merges across every entry indexed under this name (Aug 1
        2026) rather than only ever reading one - if duplicate-named
        TXDs genuinely differ (Keith's real case: "Generic.txd" and
        "generic.txd" are two different files with different sizes),
        each contributes whichever texture names the others don't
        already have, rather than one silently winning and the
        other's content being unreachable."""
        key = txd_name.lower()
        if key in self._texture_cache:
            return self._texture_cache[key]

        result = None
        entries = self._txd_index.get(key)
        if entries:
            merged = {}
            for img_path, entry in entries:
                try:
                    data = self._read_entry(img_path, entry)
                    if not data:
                        continue
                    textures = parse_txd(data)
                    if not textures:
                        continue
                    for t in textures:
                        name = t.get('name')
                        if name and name.lower() not in merged:
                            merged[name.lower()] = t
                except Exception:
                    continue
            result = merged or None
        self._texture_cache[key] = result
        return result

    def get_collision(self, model_name: str) -> Optional[COLModel]: #vers 2
        """Get the parsed COLModel for a model name. Tries the IMG-
        embedded index first (Aug 14 2026, per Keith: "In SA it should
        be reading them from the gta3.img... In VC, they can also be
        found in the gta3.img file, just like the models") - lazily
        reads+parses the entry's bytes on first request here (index
        time only recorded which entry has this name, same as
        get_geometry does for DFF), picking the model matching
        model_name if the entry happens to contain more than one, else
        its first model. Falls back to the standalone-.col index
        (already fully parsed at index_col_files time) for GTA3's
        map/-folder collision and VC's shared collision files, which
        aren't in any IMG at all. Same fallback contract as
        get_geometry throughout: a model legitimately might have no
        collision data available from either source, that's not an
        error to surface."""
        key = model_name.lower()
        if key in self._collision_cache:
            return self._collision_cache[key]

        result = None
        for img_path, entry in self._col_img_index.get(key, []):
            try:
                data = self._read_entry(img_path, entry)
                if not data:
                    continue
                col_file = COLFile()
                if not col_file.load_from_data(data, name=f"{model_name}.col"):
                    continue
                if not col_file.models:
                    continue
                # Not col_file.get_model_by_name() (Aug 14 2026) - that
                # method compares model.name, but COLModel has no such
                # attribute (it's model.header.name) - would raise on
                # every call. Match by header.name directly instead,
                # falling back to the entry's first/only model when no
                # name matches (the common case for these per-object
                # IMG entries: exactly one model, whose header.name
                # doesn't always exactly equal the DFF's model name).
                result = next(
                    (m for m in col_file.models
                     if getattr(m.header, 'name', '').strip().lower() == key),
                    col_file.models[0])
                if result is not None:
                    break
            except Exception:
                continue

        if result is None:
            for col_path, model_index in self._col_index.get(key, []):
                col_file = self._opened_col_files.get(col_path)
                if col_file is None:
                    continue
                try:
                    if 0 <= model_index < len(col_file.models):
                        result = col_file.models[model_index]
                        break
                except Exception:
                    continue

        self._collision_cache[key] = result
        return result

    def is_col_indexed(self, model_name: str) -> bool: #vers 2
        """True if model_name has collision data available from
        either indexed source (IMG-embedded or standalone) - see
        is_dff_indexed for the same indexed-vs-parsed distinction."""
        key = model_name.lower()
        return key in self._col_img_index or key in self._col_index

    def _read_entry(self, img_path: str, entry) -> Optional[bytes]:
        """Read one entry's raw bytes from its IMG archive - reuses a
        cached, already-opened IMGFile per archive path (Aug 1 2026,
        see self._opened_img_files in __init__ for why: opening an
        archive re-parses its entire directory table, which used to
        happen on every single call here, turning a preload pass
        across many distinct TXDs into a multi-minute hang). The
        actual byte read (read_entry_data) still opens its own
        short-lived file handle per call regardless of this cache -
        only the expensive directory-parsing open() is what's being
        avoided here."""
        from apps.methods.img_core_classes import IMGFile
        img = self._opened_img_files.get(img_path)
        if img is None:
            img = IMGFile(img_path)
            if not img.open():
                return None
            self._opened_img_files[img_path] = img
        return img.read_entry_data(entry)

    def is_dff_indexed(self, model_name: str) -> bool:
        """True if model_name's .dff was found in one of the indexed
        IMG archives at all - distinguishes "genuinely missing from
        the archive" from "present but failed to parse" (get_geometry
        returning None covers both cases; this is for callers that
        need to tell them apart, e.g. Keith's requested "road43.dff
        missing from img file" reporting)."""
        return model_name.lower() in self._dff_index

    def is_txd_indexed(self, txd_name: str) -> bool:
        """Same as is_dff_indexed, for TXD files."""
        return txd_name.lower() in self._txd_index

    def stats(self) -> str: #vers 3
        """One-line human-readable summary, for status bar/log use."""
        return (f"{len(self._dff_index)} DFF, {len(self._txd_index)} TXD, "
               f"{len(self._col_img_index)} COL indexed "
               f"across {len(self.indexed_img_paths)} archive(s), "
               f"{len(self._col_index)} more COL models indexed across "
               f"{len(self.indexed_col_paths)} standalone file(s); "
               f"{len(self._geometry_cache)} models, "
               f"{len(self._texture_cache)} TXDs loaded so far")
