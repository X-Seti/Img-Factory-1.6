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


class ModelCache:
    """See module docstring."""

    def __init__(self): #vers 1
        # lowercase entry name (no extension) -> (img_path, IMGEntry)
        self._dff_index: Dict[str, Tuple[str, object]] = {}
        self._txd_index: Dict[str, Tuple[str, object]] = {}
        # lowercase model/txd name -> parsed result, or None if loading/
        # parsing failed (cached as None so it's not retried every time)
        self._geometry_cache: Dict[str, Optional[DFFModel]] = {}
        self._dimensions_cache: Dict[str, Optional[Tuple[float, float, float]]] = {}
        self._texture_cache: Dict[str, Optional[Dict[str, dict]]] = {}
        self.indexed_img_paths: List[str] = []
        self.index_errors: List[str] = []

    def index_img_files(self, img_paths: List[str]): #vers 1
        """Scan a list of IMG archive paths, building name -> (path,
        entry) indexes for .dff and .txd entries. Call once after a
        world loads (or its IMG set changes) - reading directory
        headers only, not entry contents, so this stays fast even for
        large archives. Safe to call again to re-index (clears
        previous indexes first)."""
        from apps.methods.img_core_classes import IMGFile

        self._dff_index.clear()
        self._txd_index.clear()
        self.indexed_img_paths = []
        self.index_errors = []

        for img_path in img_paths:
            try:
                img = IMGFile(img_path)
                if not img.open():
                    self.index_errors.append(f"Failed to open {img_path}")
                    continue
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
                        self._dff_index[stem_lower] = (img_path, entry)
                    elif ext_lower == 'txd':
                        self._txd_index[stem_lower] = (img_path, entry)
                self.indexed_img_paths.append(img_path)
            except Exception as e:
                self.index_errors.append(f"{img_path}: {e}")

    def clear_indexes(self): #vers 1
        """Drop all indexes and cached geometry/textures - call before
        re-indexing for a newly loaded world, so stale entries from a
        previous world can't leak through."""
        self._dff_index.clear()
        self._txd_index.clear()
        self._geometry_cache.clear()
        self._texture_cache.clear()
        self._dimensions_cache.clear()
        self.indexed_img_paths = []
        self.index_errors = []

    def get_geometry(self, model_name: str) -> Optional[DFFModel]:
        """Get the parsed DFF geometry for a model name, loading and
        parsing (and caching the result either way) on first request.
        Returns None if the model isn't indexed, or its data failed to
        parse as a valid DFF - callers should fall back to the simple
        point/marker rendering in that case, not treat it as an error
        to surface to the user (a mod's IMG set legitimately might not
        contain every model referenced by its own map data)."""
        key = model_name.lower()
        if key in self._geometry_cache:
            return self._geometry_cache[key]

        result = None
        entry_info = self._dff_index.get(key)
        if entry_info is not None:
            img_path, entry = entry_info
            try:
                data = self._read_entry(img_path, entry)
                if data and detect_dff(data):
                    result = DFFParser(data, model_name).parse()
            except Exception:
                result = None
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
        error to surface."""
        key = txd_name.lower()
        if key in self._texture_cache:
            return self._texture_cache[key]

        result = None
        entry_info = self._txd_index.get(key)
        if entry_info is not None:
            img_path, entry = entry_info
            try:
                data = self._read_entry(img_path, entry)
                if data:
                    textures = parse_txd(data)
                    if textures:
                        result = {t['name'].lower(): t for t in textures if t.get('name')}
            except Exception:
                result = None
        self._texture_cache[key] = result
        return result

    def _read_entry(self, img_path: str, entry) -> Optional[bytes]:
        """Read one entry's raw bytes from its IMG archive - opens the
        archive fresh each call rather than keeping file handles held
        open long-term. Not cached at this layer (the parsed result
        above is what's cached; re-reading raw bytes on a cache miss
        is cheap compared to re-parsing)."""
        from apps.methods.img_core_classes import IMGFile
        img = IMGFile(img_path)
        if not img.open():
            return None
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

    def stats(self) -> str: #vers 1
        """One-line human-readable summary, for status bar/log use."""
        return (f"{len(self._dff_index)} DFF, {len(self._txd_index)} TXD indexed "
               f"across {len(self.indexed_img_paths)} archive(s); "
               f"{len(self._geometry_cache)} models, "
               f"{len(self._texture_cache)} TXDs loaded so far")
