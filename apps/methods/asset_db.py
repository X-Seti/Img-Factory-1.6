"""
apps/methods/asset_db.py  — IMG Factory Asset Database  #vers 1

Lightweight SQLite catalogue of IMG / COL / TXD / IDE assets.
Stores file paths, entry names, mtimes for change-detection.
One .db file per game profile, stored in the user data directory.

Schema
------
  source_files   — tracked source files (img/col/ide files) with mtime+size
  img_entries    — every entry inside each IMG archive
  col_entries    — COL model names extracted from IMG or standalone .col files
  txd_entries    — TXD texture names extracted from IMG
  ide_entries    — IDE object records

DB location
-----------
  Linux/Mac:  ~/.local/share/imgfactory/<profile>.db
  Windows:    %APPDATA%\\ImgFactory\\<profile>.db
"""

import os
import sqlite3
import time
from typing import Optional, List, Dict, Tuple

#    User data directory                                                      

def _user_data_dir() -> str:
    """Return platform-appropriate user data directory for IMG Factory."""
    if os.name == 'nt':                          # Windows
        base = os.environ.get('APPDATA', os.path.expanduser('~'))
        return os.path.join(base, 'ImgFactory')
    elif os.uname().sysname == 'Darwin':          # macOS
        return os.path.expanduser('~/Library/Application Support/ImgFactory')
    else:                                         # Linux / XDG
        base = os.environ.get('XDG_DATA_HOME',
                              os.path.expanduser('~/.local/share'))
        return os.path.join(base, 'imgfactory')


#    Schema                                                                    

_SCHEMA = """
PRAGMA journal_mode=WAL;
PRAGMA synchronous=NORMAL;

CREATE TABLE IF NOT EXISTS db_meta (
    key     TEXT PRIMARY KEY,
    value   TEXT
);

-- Source files we are tracking
CREATE TABLE IF NOT EXISTS source_files (
    id          INTEGER PRIMARY KEY,
    path        TEXT UNIQUE NOT NULL,
    file_type   TEXT,           -- IMG, COL, TXD, IDE, IPL
    mtime       REAL,
    fsize       INTEGER,
    indexed_at  REAL
);

-- Every entry inside an IMG archive
CREATE TABLE IF NOT EXISTS img_entries (
    id          INTEGER PRIMARY KEY,
    source_id   INTEGER REFERENCES source_files(id) ON DELETE CASCADE,
    entry_name  TEXT NOT NULL,
    ext         TEXT,           -- DFF, TXD, COL, IFP …
    offset      INTEGER,
    size        INTEGER
);
CREATE INDEX IF NOT EXISTS idx_img_name ON img_entries(entry_name);
CREATE INDEX IF NOT EXISTS idx_img_ext  ON img_entries(ext);

-- COL model names (from IMG-embedded or standalone .col)
CREATE TABLE IF NOT EXISTS col_entries (
    id          INTEGER PRIMARY KEY,
    source_id   INTEGER REFERENCES source_files(id) ON DELETE CASCADE,
    entry_name  TEXT,           -- IMG entry name (e.g. 'generics.col') or NULL
    model_name  TEXT NOT NULL,
    model_id    INTEGER,
    col_version TEXT            -- COL1 / COL2 / COL3
);
CREATE INDEX IF NOT EXISTS idx_col_model ON col_entries(model_name);

-- TXD texture names (from IMG-embedded .txd — lightweight, no pixel data)
CREATE TABLE IF NOT EXISTS txd_entries (
    id          INTEGER PRIMARY KEY,
    source_id   INTEGER REFERENCES source_files(id) ON DELETE CASCADE,
    entry_name  TEXT,           -- IMG entry e.g. 'generics.txd'
    texture_name TEXT NOT NULL
);
CREATE INDEX IF NOT EXISTS idx_txd_tex  ON txd_entries(texture_name);
CREATE INDEX IF NOT EXISTS idx_txd_entry ON txd_entries(entry_name);

-- IDE object records
CREATE TABLE IF NOT EXISTS ide_entries (
    id          INTEGER PRIMARY KEY,
    source_id   INTEGER REFERENCES source_files(id) ON DELETE CASCADE,
    model_id    INTEGER,
    model_name  TEXT NOT NULL,
    txd_name    TEXT,
    draw_dist   REAL,
    flags       INTEGER,
    section     TEXT,
    line_no     INTEGER
);
CREATE INDEX IF NOT EXISTS idx_ide_model ON ide_entries(model_name);
CREATE INDEX IF NOT EXISTS idx_ide_id    ON ide_entries(model_id);
CREATE INDEX IF NOT EXISTS idx_ide_txd   ON ide_entries(txd_name);
"""


#    AssetDB                                                                   

class AssetDB: #vers 1
    """
    Persistent SQLite catalogue of game assets.

    Usage::

        db = AssetDB('GTASOL')           # opens/creates the profile DB
        db.index_img('/path/game_vc.img')    # index an IMG archive
        db.index_ide('/path/game_vc.ide')    # index an IDE file
        db.update_changed()              # re-index any changed files

        # Lookups
        entry = db.find_img_entry('landstal.dff')  # → (img_path, offset, size)
        txd   = db.find_txd_entry('chassis')        # → img_path + entry_name
        col   = db.find_col_model('landstal')       # → (img_path, entry_name)
        ide   = db.find_ide_entry('landstal')       # → row dict
    """

    BUILTIN_PROFILES = ['GTA3', 'VC', 'SA', 'GTASOL']

    def __init__(self, profile: str = 'GTASOL', db_path: str = ''):
        self.profile   = profile
        self._con: Optional[sqlite3.Connection] = None
        if not db_path:
            data_dir = _user_data_dir()
            os.makedirs(data_dir, exist_ok=True)
            safe = ''.join(c for c in profile if c.isalnum() or c in '-_')
            db_path = os.path.join(data_dir, f'{safe.lower()}.db')
        self.db_path = db_path
        self._open()

    #    Connection                                                            

    def _open(self):
        self._con = sqlite3.connect(self.db_path, check_same_thread=False)
        self._con.row_factory = sqlite3.Row
        self._con.executescript(_SCHEMA)
        self._con.execute(
            "INSERT OR IGNORE INTO db_meta VALUES ('profile', ?)",
            (self.profile,))
        self._con.execute(
            "INSERT OR IGNORE INTO db_meta VALUES ('created', ?)",
            (str(time.time()),))
        self._con.commit()

    def close(self):
        if self._con:
            self._con.close()
            self._con = None

    #    File tracking                                                          

    def _get_source_id(self, path: str, file_type: str) -> Tuple[int, bool]:
        """Return (source_id, needs_reindex).  Creates row if absent."""
        try:
            stat = os.stat(path)
            mtime, fsize = stat.st_mtime, stat.st_size
        except OSError:
            return -1, False

        row = self._con.execute(
            "SELECT id, mtime, fsize FROM source_files WHERE path=?",
            (path,)).fetchone()

        if row is None:
            cur = self._con.execute(
                "INSERT INTO source_files(path,file_type,mtime,fsize,indexed_at)"
                " VALUES (?,?,?,?,?)",
                (path, file_type, mtime, fsize, time.time()))
            return cur.lastrowid, True

        # Compare mtime+size for change detection
        needs = (abs(row['mtime'] - mtime) > 0.5) or (row['fsize'] != fsize)
        if needs:
            self._con.execute(
                "UPDATE source_files SET mtime=?,fsize=?,indexed_at=?"
                " WHERE id=?",
                (mtime, fsize, time.time(), row['id']))
        return row['id'], needs

    def _clear_entries(self, source_id: int):
        """Remove all indexed entries for a source file (before re-indexing)."""
        for tbl in ('img_entries', 'col_entries', 'txd_entries', 'ide_entries'):
            self._con.execute(
                f"DELETE FROM {tbl} WHERE source_id=?", (source_id,))

    #    IMG indexing                                                           

    def index_img(self, img_path: str,
                  index_col_names: bool = True,
                  progress_cb=None) -> int:
        """Index an IMG archive. Returns number of entries added.
        If index_col_names=True, reads COL model names from embedded .col files.
        progress_cb(done, total) called periodically."""
        source_id, needs = self._get_source_id(img_path, 'IMG')
        if source_id < 0:
            return 0
        if not needs:
            return 0   # unchanged — skip

        self._clear_entries(source_id)

        try:
            from apps.methods.img_core_classes import IMGFile
            arc = IMGFile(img_path)
            arc.open()
        except Exception:
            self._con.commit()
            return 0

        entries = arc.entries
        total   = len(entries)
        added   = 0

        for i, entry in enumerate(entries):
            if progress_cb and i % 50 == 0:
                progress_cb(i, total)

            name = entry.name or ''
            ext  = name.rsplit('.', 1)[-1].upper() if '.' in name else ''

            self._con.execute(
                "INSERT INTO img_entries(source_id,entry_name,ext,offset,size)"
                " VALUES (?,?,?,?,?)",
                (source_id, name, ext, entry.offset, entry.size))
            added += 1

            # Index COL model names without loading full geometry
            if index_col_names and ext == 'COL':
                try:
                    data = arc.read_entry_data(entry)
                    self._index_col_data(source_id, name, data)
                except Exception:
                    pass

            # Index TXD texture names (lightweight — just names, no pixels)
            if ext == 'TXD':
                try:
                    data = arc.read_entry_data(entry)
                    self._index_txd_names(source_id, name, data)
                except Exception:
                    pass

        self._con.commit()
        return added

    def _index_col_data(self, source_id: int, entry_name: str, data: bytes): #vers 2
        """Extract all COL model names from a COL blob (may contain multiple models).
        Handles COL1/COL2/COL3 chained blocks robustly."""
        import struct
        VALID_SIGS = {b'COL\x00', b'COL\x01', b'COL\x02', b'COL\x03', b'COLL'}
        VER_MAP    = {b'COL\x00':'COL1', b'COL\x01':'COL2',
                      b'COL\x02':'COL3', b'COL\x03':'COL3', b'COLL':'COL1'}
        offset = 0
        added  = 0
        while offset + 8 <= len(data):
            sig = data[offset:offset+4]
            if sig not in VALID_SIGS:
                # Skip byte-by-byte until next valid sig or EOF
                next_sig = -1
                for s in VALID_SIGS:
                    idx = data.find(s, offset+1)
                    if idx >= 0 and (next_sig < 0 or idx < next_sig):
                        next_sig = idx
                if next_sig < 0:
                    break
                offset = next_sig
                continue
            try:
                block_size = struct.unpack_from('<I', data, offset+4)[0]
                if block_size == 0 or offset + 8 + block_size > len(data) + 4:
                    offset += 4; continue   # corrupt block
                # Name: 22 bytes at offset+8
                raw_name   = data[offset+8:offset+30]
                model_name = raw_name.split(b'\x00')[0].decode(
                    'ascii', errors='ignore').strip()
                # Model ID: uint16 at offset+30
                model_id = 0
                if offset + 32 <= len(data):
                    model_id = struct.unpack_from('<H', data, offset+30)[0]
                col_ver  = VER_MAP.get(sig, 'COL?')
                if model_name:
                    self._con.execute(
                        "INSERT INTO col_entries"
                        "(source_id,entry_name,model_name,model_id,col_version)"
                        " VALUES (?,?,?,?,?)",
                        (source_id, entry_name, model_name, model_id, col_ver))
                    added += 1
                offset += 8 + block_size
            except Exception:
                offset += 4   # skip past bad data, keep trying
        return added

    def _index_txd_names(self, source_id: int, entry_name: str, data: bytes):
        """Extract texture names from TXD header (no pixel decoding)."""
        import struct
        if len(data) < 12:
            return
        try:
            main_type, main_size, version = struct.unpack_from('<III', data, 0)
            if main_type != 0x16:
                return
            # Struct chunk
            if len(data) < 24:
                return
            st_type, st_size = struct.unpack_from('<II', data, 12)[:2]
            if st_type != 0x01:
                return
            # Texture count
            if version >= 0x1803FFFF:
                tex_count = struct.unpack_from('<H', data, 24)[0]
            else:
                tex_count = struct.unpack_from('<I', data, 24)[0]
            if tex_count <= 0 or tex_count > 4096:
                return
            offset = 12 + 12 + st_size   # past header + struct
            for _ in range(min(tex_count, 4096)):
                if offset + 12 > len(data):
                    break
                tex_type, tex_size = struct.unpack_from('<II', data, offset)[:2]
                if tex_type == 0x15 and offset + 12 + tex_size <= len(data):
                    # Native texture — name is at struct offset within chunk
                    # Skip the native texture header to reach the name
                    name_off = offset + 12 + 12 + 8  # rough offset to name field
                    if name_off + 32 <= len(data):
                        raw = data[name_off:name_off+32]
                        name = raw.split(b'\x00')[0].decode(
                            'ascii', errors='ignore').strip()
                        if name and name.isprintable():
                            self._con.execute(
                                "INSERT INTO txd_entries"
                                "(source_id,entry_name,texture_name)"
                                " VALUES (?,?,?)",
                                (source_id, entry_name, name.lower()))
                offset += 12 + tex_size
        except Exception:
            pass

    #    IDE indexing                                                           

    def index_col(self, col_path: str) -> int:
        """Index a standalone .col file — extracts all model names.
        Returns number of models added."""
        source_id, needs = self._get_source_id(col_path, 'COL')
        if source_id < 0 or not needs:
            return 0
        self._clear_entries(source_id)
        try:
            with open(col_path, 'rb') as f:
                data = f.read()
        except OSError:
            return 0
        self._index_col_data(source_id,
                             os.path.basename(col_path), data)
        self._con.commit()
        # Count what was added
        n = self._con.execute(
            "SELECT COUNT(*) FROM col_entries WHERE source_id=?",
            (source_id,)).fetchone()[0]
        return n

    def index_ide(self, ide_path: str) -> int:
        """Index a .ide file. Returns number of objects added."""
        source_id, needs = self._get_source_id(ide_path, 'IDE')
        if source_id < 0 or not needs:
            return 0
        self._clear_entries(source_id)
        try:
            from apps.methods.gta_dat_parser import IDEParser, GTAGame
            parser = IDEParser(GTAGame.VC)
            if not parser.parse(ide_path):
                return 0
            for obj in parser.objects:
                dd = obj.extra.get('draw_dist',
                     obj.extra.get('dist1', 0)) if obj.extra else 0
                fl = obj.extra.get('flags', 0) if obj.extra else 0
                self._con.execute(
                    "INSERT INTO ide_entries"
                    "(source_id,model_id,model_name,txd_name,"
                    "draw_dist,flags,section,line_no)"
                    " VALUES (?,?,?,?,?,?,?,?)",
                    (source_id, obj.model_id, obj.model_name.lower(),
                     obj.txd_name.lower() if obj.txd_name else '',
                     dd, fl, obj.section, obj.line_no))
            self._con.commit()
            return len(parser.objects)
        except Exception:
            return 0

    #    Bulk update                                                            

    def update_changed(self, progress_cb=None) -> Dict[str, int]:
        """Re-index any source files whose mtime/size has changed.
        Returns dict of {path: entries_added}."""
        rows = self._con.execute(
            "SELECT path, file_type, mtime, fsize FROM source_files"
        ).fetchall()
        results = {}
        for row in rows:
            path = row['path']
            if not os.path.isfile(path):
                continue
            stat = os.stat(path)
            if (abs(stat.st_mtime - row['mtime']) < 0.5 and
                    stat.st_size == row['fsize']):
                continue   # unchanged
            ft = row['file_type']
            if ft == 'IMG':
                n = self.index_img(path, progress_cb=progress_cb)
            elif ft == 'IDE':
                n = self.index_ide(path)
            else:
                n = 0
            results[path] = n
        return results

    def index_game_root(self, game_root: str,
                        progress_cb=None) -> Dict[str, int]:
        """Walk game_root, index all IMG and IDE files found.
        Returns {path: entries_added}."""
        results = {}
        for dirpath, _, fnames in os.walk(game_root):
            for fn in fnames:
                fpath = os.path.join(dirpath, fn)
                ext   = fn.rsplit('.', 1)[-1].lower() if '.' in fn else ''
                if ext == 'img':
                    n = self.index_img(fpath, progress_cb=progress_cb)
                    results[fpath] = n
                elif ext == 'ide':
                    n = self.index_ide(fpath)
                    results[fpath] = n
                elif ext == 'col':
                    # Standalone .col files (not inside an IMG)
                    n = self.index_col(fpath)
                    results[fpath] = n
        return results

    #    Lookups                                                                

    def find_img_entry(self, entry_name: str) -> Optional[sqlite3.Row]:
        """Find an IMG entry by name (case-insensitive).
        Returns row with: source_path, entry_name, offset, size."""
        return self._con.execute("""
            SELECT sf.path AS source_path,
                   ie.entry_name, ie.offset, ie.size, ie.ext
            FROM img_entries ie
            JOIN source_files sf ON sf.id = ie.source_id
            WHERE lower(ie.entry_name) = lower(?)
            LIMIT 1
        """, (entry_name,)).fetchone()

    def find_txd_entry(self, texture_name: str) -> Optional[sqlite3.Row]:
        """Find a TXD containing a given texture name.
        Returns row with: source_path, entry_name (the .txd), texture_name."""
        return self._con.execute("""
            SELECT sf.path AS source_path,
                   te.entry_name, te.texture_name
            FROM txd_entries te
            JOIN source_files sf ON sf.id = te.source_id
            WHERE te.texture_name = lower(?)
            LIMIT 1
        """, (texture_name,)).fetchone()

    def find_col_file(self, entry_name: str) -> Optional[sqlite3.Row]:
        """Find a COL file entry (e.g. 'generics.col') in an IMG.
        Returns row with: source_path, entry_name, offset, size."""
        col_name = entry_name.lower()
        if not col_name.endswith('.col'):
            col_name += '.col'
        return self._con.execute("""
            SELECT sf.path AS source_path,
                   ie.entry_name, ie.offset, ie.size
            FROM img_entries ie
            JOIN source_files sf ON sf.id = ie.source_id
            WHERE lower(ie.entry_name) = ?
            LIMIT 1
        """, (col_name,)).fetchone()

    def find_col_model(self, model_name: str) -> Optional[sqlite3.Row]:
        """Find a COL entry for a model name.
        Returns row with: source_path, entry_name, model_name, col_version."""
        return self._con.execute("""
            SELECT sf.path AS source_path,
                   ce.entry_name, ce.model_name,
                   ce.model_id, ce.col_version
            FROM col_entries ce
            JOIN source_files sf ON sf.id = ce.source_id
            WHERE lower(ce.model_name) = lower(?)
            LIMIT 1
        """, (model_name,)).fetchone()

    def find_ide_entry(self, model_name: str) -> Optional[sqlite3.Row]:
        """Find an IDE object by model name."""
        return self._con.execute("""
            SELECT sf.path AS source_path,
                   ie.model_id, ie.model_name, ie.txd_name,
                   ie.draw_dist, ie.flags, ie.section
            FROM ide_entries ie
            JOIN source_files sf ON sf.id = ie.source_id
            WHERE ie.model_name = lower(?)
            LIMIT 1
        """, (model_name,)).fetchone()

    def find_ide_by_id(self, model_id: int) -> Optional[sqlite3.Row]:
        return self._con.execute("""
            SELECT sf.path AS source_path,
                   ie.model_id, ie.model_name, ie.txd_name,
                   ie.draw_dist, ie.flags, ie.section
            FROM ide_entries ie
            JOIN source_files sf ON sf.id = ie.source_id
            WHERE ie.model_id = ?
            LIMIT 1
        """, (model_id,)).fetchone()

    def find_all_for_model(self, model_name: str) -> Dict:
        """Return everything known about a model: IDE, IMG entries, COL, TXD."""
        stem = model_name.lower().split('.')[0]
        return {
            'ide': self.find_ide_entry(stem),
            'dff': self.find_img_entry(stem + '.dff'),
            'col': self.find_col_model(stem),
            'txd': (lambda r: self.find_img_entry(
                        (r['txd_name'] or '') + '.txd') if r else None
                    )(self.find_ide_entry(stem)),
        }

    #    Stats                                                                  

    def stats(self) -> Dict[str, int]:
        def _n(tbl):
            return self._con.execute(
                f"SELECT COUNT(*) FROM {tbl}").fetchone()[0]
        return {
            'source_files': _n('source_files'),
            'img_entries':  _n('img_entries'),
            'col_entries':  _n('col_entries'),
            'txd_entries':  _n('txd_entries'),
            'ide_entries':  _n('ide_entries'),
        }

    def summary(self) -> str:
        s = self.stats()
        return (f"AssetDB '{self.profile}': "
                f"{s['img_entries']:,} IMG entries  "
                f"{s['col_entries']:,} COL models  "
                f"{s['txd_entries']:,} textures  "
                f"{s['ide_entries']:,} IDE objects  "
                f"({s['source_files']} files)")

    #    Profile management                                                     

    @staticmethod
    def list_profiles() -> List[str]:
        """Return names of all saved DB profiles."""
        d = _user_data_dir()
        if not os.path.isdir(d):
            return []
        return [os.path.splitext(f)[0].upper()
                for f in os.listdir(d) if f.endswith('.db')]

    @staticmethod
    def delete_profile(profile: str):
        d  = _user_data_dir()
        safe = ''.join(c for c in profile if c.isalnum() or c in '-_')
        p  = os.path.join(d, f'{safe.lower()}.db')
        if os.path.isfile(p):
            os.remove(p)
