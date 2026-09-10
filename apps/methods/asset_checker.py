#this belongs in apps/methods/asset_checker.py - Version: 1

##Methods list -
# find_sibling_asset_files
# check_assets
# AssetCheckResult

"""asset_checker.py - cross-references real model names across an IMG
archive, a COL file, and an IDE file that share the same base
filename (Sep 5 2026, per Keith: "Asset checker as a right click on
img, col and ide entries on dat browser, dir tree... where we could
show missing in COL, missing in IMG, not found in IDE, or any
combination that makes sense"). Real GTA convention - game_vc.img/
game_vc.col/game_vc.ide share the same base stem "game_vc" - is used
to auto-find the other two sibling files from whichever one gets
right-clicked."""

import os
from dataclasses import dataclass, field
from typing import Optional, Set, Dict


@dataclass
class AssetCheckResult: #vers 1
    img_path: str = ""
    col_path: str = ""
    ide_path: str = ""
    img_names: Set[str] = field(default_factory=set)   # lowercase, no extension
    col_names: Set[str] = field(default_factory=set)   # lowercase
    ide_names: Set[str] = field(default_factory=set)   # lowercase
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


def find_sibling_asset_files(clicked_path: str): #vers 1
    """Given one file's real path, look for the other two real sibling
    files sharing the same base stem (case-insensitive) in the same
    folder - the real game_vc.img/game_vc.col/game_vc.ide convention.
    Returns (img_path_or_none, col_path_or_none, ide_path_or_none)."""
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
    return found['.img'], found['.col'], found['.ide']


def check_assets(img_path: str = None, col_path: str = None,
                  ide_path: str = None, game: str = None) -> AssetCheckResult: #vers 2
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
                # comparison (Sep 5 2026, per Keith: "hide tex names
                # from the img file, because im also seeing not found
                # messages") - an IMG archive holds both .dff models
                # and .txd textures, but IDE entries reference a
                # texture by its own txd_name field, not as a model
                # name in its own right, so including .txd entries
                # here just produced noise, never real matches.
                result.img_names = {
                    os.path.splitext(e.name)[0].lower() for e in img_file.entries
                    if e.extension.upper() == 'DFF'
                }
        except Exception as e:
            result.img_error = str(e)

    if col_path and os.path.isfile(col_path):
        try:
            from apps.methods.col_core_classes import COLFile
            col_file = COLFile()
            if col_file.load_from_file(col_path):
                result.col_path = col_path
                result.col_names = {m.name.lower() for m in col_file.models if m.name}
        except Exception as e:
            result.col_error = str(e)

    if ide_path and os.path.isfile(ide_path):
        try:
            from apps.methods.gta_dat_parser import IDEParser, GTAGame
            parser = IDEParser(game or GTAGame.GTA3)
            if parser.parse(ide_path) and parser.objects:
                result.ide_path = ide_path
                result.ide_names = {o.model_name.lower() for o in parser.objects}
        except Exception as e:
            result.ide_error = str(e)

    return result
