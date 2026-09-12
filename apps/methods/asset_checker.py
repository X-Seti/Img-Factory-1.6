#!/usr/bin/env python3
#this belongs in apps/methods/asset_checker.py - Version: 2

##Methods list -
# find_sibling_asset_files
# check_assets
# AssetCheckResult
# AssetCheckResult.img_extra_over_ide
# AssetCheckResult.col_extra_over_ide

"""asset_checker.py - cross-references real model names across an IMG
archive, a COL file, and an IDE file that share the same base
filename (Sep 5 2026)"""

#TODO; All views need to allow copy to clipboard, in Cross Ref Table replace the line numbers with the obj ID.

import os
from dataclasses import dataclass, field
from typing import Optional, Set, Dict


@dataclass
class AssetCheckResult: #vers 2
    img_path: str = ""
    col_path: str = ""
    ide_path: str = ""
    img_names: Set[str] = field(default_factory=set)   # lowercase, no extension - real .dff models only
    img_txd_names: Set[str] = field(default_factory=set)   # lowercase, no extension - real .txd textures
    col_names: Set[str] = field(default_factory=set)   # lowercase
    ide_names: Set[str] = field(default_factory=set)   # lowercase
    ide_id_by_name: Dict[str, int] = field(default_factory=dict)      # lowercase name -> real model_id
    ide_txd_by_name: Dict[str, str] = field(default_factory=dict)     # lowercase name -> real declared txd_name (lowercase, for comparison)
    ide_txd_display_by_name: Dict[str, str] = field(default_factory=dict)   # lowercase name -> real declared txd_name (original case, for display)
    img_error: str = ""
    col_error: str = ""
    ide_error: str = ""

    @property
    def all_names(self): #vers 1
        return sorted(self.img_names | self.col_names | self.ide_names)

    @property
    def missing_from_col(self): #vers 1
        """In IDE but no matching COL model - only meaningful if a COL
        file was actually found; an empty set with col_path=="" means
        "not checked", not "nothing missing"."""
        if not self.col_path:
            return set()
        return self.ide_names - self.col_names

    @property
    def missing_from_img(self): #vers 1
        """In IDE but no matching IMG (DFF) entry."""
        if not self.img_path:
            return set()
        return self.ide_names - self.img_names

    @property
    def not_in_ide(self): #vers 1
        """In IMG or COL but never declared in IDE at all."""
        if not self.ide_path:
            return set()
        return (self.img_names | self.col_names) - self.ide_names

    @property
    def img_extra_over_ide(self): #vers 1
        """In IMG but not declared in IDE - the "+N" direction for the
        IMG column header (Sep 5 2026)"""
        if not self.img_path or not self.ide_path:
            return set()
        return self.img_names - self.ide_names

    @property
    def col_extra_over_ide(self): #vers 1
        """In COL but not declared in IDE - the "+N" direction for the
        COL column header."""
        if not self.col_path or not self.ide_path:
            return set()
        return self.col_names - self.ide_names

    def status_for(self, name: str) -> str: #vers 1
        """One-line status for a given model name, for the merged
        3-lines-per-model view."""
        in_img = name in self.img_names
        in_col = name in self.col_names
        in_ide = name in self.ide_names
        parts = []
        if self.img_path:
            parts.append("IMG" if in_img else "no IMG")
        if self.col_path:
            parts.append("COL" if in_col else "no COL")
        if self.ide_path:
            parts.append("IDE" if in_ide else "no IDE")
        return " / ".join(parts)

    def cross_reference_rows(self): #vers 2
        """One row per real model name, in the real column order. ID | DFF | COL | IDE Model Name | Texture entry | Errors. ID and Texture entry only ever come from a real IDE declaration (a model with no IDE entry has no real id or expected texture to check at all) - Texture entry checks whether the IDE's own declared txd_name actually shows up among the IMG's real .txd entries, not just whether the model's own name has a texture."""
        rows = []
        for name in self.all_names:
            model_id = self.ide_id_by_name.get(name, "")
            dff_status = "Yes" if name in self.img_names else "—"
            col_status = "Yes" if name in self.col_names else "—"
            ide_model_name = name if name in self.ide_names else ""

            declared_txd = self.ide_txd_by_name.get(name)
            declared_txd_display = self.ide_txd_display_by_name.get(name, declared_txd)
            if declared_txd is None:
                texture_status = ""   # not declared in IDE - nothing to check
            elif declared_txd in self.img_txd_names:
                texture_status = "Yes"
            else:
                # Show the real expected filename, not just "Missing"
                # (Sep 5 2026)
                texture_status = f"{declared_txd_display}.txd (missing)"

            errors = []
            if self.ide_path and name not in self.ide_names:
                errors.append("Not in IDE")
            if self.col_path and name not in self.col_names:
                errors.append("Missing COL")
            if self.img_path and name not in self.img_names:
                errors.append("Missing DFF")
            if declared_txd is not None and declared_txd not in self.img_txd_names:
                errors.append(f"Texture '{declared_txd_display}.txd' missing")
            error_text = "; ".join(errors) if errors else "OK"

            rows.append((str(model_id), dff_status, col_status, ide_model_name, texture_status, error_text))
        return rows


def find_sibling_asset_files(clicked_path: str): #vers 2
    """Given one file's real path, look for the other two real sibling
    files sharing the same base stem (case-insensitive) in the same
    folder - the real game_vc.img/game_vc.col/game_vc.ide convention.
    Returns (img_path_or_none, col_path_or_none_or_list, ide_path_or_none)."""
    folder = os.path.dirname(clicked_path)
    stem = os.path.splitext(os.path.basename(clicked_path))[0].lower()
    found = {'.img': None, '.col': None, '.ide': None}
    try:
        for fname in os.listdir(folder):
            fstem, fext = os.path.splitext(fname)
            fext = fext.lower()
            if fext in found and fstem.lower() == stem:
                found[fext] = os.path.join(folder, fname)
    except OSError:
        pass

    if found['.col'] is None and stem == 'gta3':
        gta3_col_names = ('peds.col', 'special.col', 'vehicles.col', 'weapons.col')
        for candidate_dir in (
            os.path.join(folder, 'models', 'coll'),
            os.path.join(folder, '..', 'models', 'coll'),
            os.path.join(folder, '..', '..', 'models', 'coll'),
        ):
            existing = [os.path.join(candidate_dir, n) for n in gta3_col_names
                        if os.path.isfile(os.path.join(candidate_dir, n))]
            if existing:
                found['.col'] = existing
                break

    return found['.img'], found['.col'], found['.ide']


def check_assets(img_path: str = None, col_path=None,
                  ide_path: str = None, game: str = None) -> AssetCheckResult: #vers 4
    """Load whichever of the 3 real files exist and cross-reference
    their real model names. Any of the 3 paths can be None/missing -
    the corresponding *_path stays empty and that source's own
    *_names set stays empty, so callers can tell "not checked" apart
    from "checked, nothing found" via the path fields."""
    result = AssetCheckResult()

    if img_path and os.path.isfile(img_path):
        try:
            from apps.methods.img_core_classes import IMGFile
            img_file = IMGFile(img_path)
            if img_file.open():
                result.img_path = img_path
                # Only real model entries (.dff) count for this
                # comparison (Sep 5 2026)
                result.img_names = {
                    os.path.splitext(e.name)[0].lower() for e in img_file.entries
                    if e.extension.upper() == 'DFF'
                }
                result.img_txd_names = {
                    os.path.splitext(e.name)[0].lower() for e in img_file.entries
                    if e.extension.upper() == 'TXD'
                }
        except Exception as e:
            result.img_error = str(e)

    col_paths = [col_path] if isinstance(col_path, str) else (col_path or [])
    col_paths = [p for p in col_paths if p and os.path.isfile(p)]
    if col_paths:
        try:
            from apps.methods.col_core_classes import COLFile
            merged_names = set()
            for one_path in col_paths:
                col_file = COLFile()
                if col_file.load_from_file(one_path):
                    merged_names |= {m.name.lower() for m in col_file.models if m.name}
            result.col_path = ", ".join(os.path.basename(p) for p in col_paths)
            result.col_names = merged_names
        except Exception as e:
            result.col_error = str(e)

    if ide_path and os.path.isfile(ide_path):
        try:
            from apps.methods.gta_dat_parser import IDEParser, GTAGame
            parser = IDEParser(game or GTAGame.GTA3)
            if parser.parse(ide_path) and parser.objects:
                result.ide_path = ide_path
                result.ide_names = {o.model_name.lower() for o in parser.objects}
                result.ide_id_by_name = {
                    o.model_name.lower(): o.model_id for o in parser.objects
                }
                result.ide_txd_by_name = {
                    o.model_name.lower(): o.txd_name.lower()
                    for o in parser.objects if o.txd_name
                }
                result.ide_txd_display_by_name = {
                    o.model_name.lower(): o.txd_name
                    for o in parser.objects if o.txd_name
                }
        except Exception as e:
            result.ide_error = str(e)

    return result
