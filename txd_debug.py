#!/usr/bin/env python3
"""
TXD Debug Inspector - dumps raw header values from any RW TXD file
Usage:
    python3 txd_debug.py <file.txd>            # inspect all textures
    python3 txd_debug.py <file.txd> --hex      # also dump raw header bytes
    python3 txd_debug.py <dir/>                # scan all .txd files in directory
    python3 txd_debug.py <file.txd> --export   # try to decode and export PNGs
"""

import sys
import os
import struct
import argparse

# ── RW raster format flags (from GTAMods wiki / Magic.TXD source) ────────────
RW_FORMAT = {
    0x0000: 'FORMAT_DEFAULT',
    0x0100: 'C1555 (ARGB1555)',
    0x0200: 'C565  (RGB565)',
    0x0300: 'C4444 (ARGB4444)',
    0x0400: 'LUM8',
    0x0500: 'C8888 (ARGB8888)',
    0x0600: 'C888  (RGB888)',
    0x0900: 'D32',
    0x0A00: 'C555  (RGB555)',
}
RW_FLAGS = {
    0x1000: 'AUTO_MIPMAP',
    0x2000: 'PAL8 (256 colours)',
    0x4000: 'PAL4 (16 colours)',
    0x8000: 'MIPMAP_INCLUDED',
    0x10000: 'SWIZZLED/PS2',
    0x20000: 'HAS_TEXEL_HEADERS',
}

# D3D format codes (used by SA+ / D3D9 platform)
D3D_FORMAT = {
    0: 'UNKNOWN',
    20: 'D3DFMT_R8G8B8',
    21: 'D3DFMT_A8R8G8B8',
    22: 'D3DFMT_X8R8G8B8',
    23: 'D3DFMT_R5G6B5',
    24: 'D3DFMT_X1R5G5B5',
    25: 'D3DFMT_A1R5G5B5',
    26: 'D3DFMT_A4R4G4B4',
    28: 'D3DFMT_A8R3G3B2',
    29: 'D3DFMT_X4R4G4B4',
    32: 'D3DFMT_A8B8G8R8',
    41: 'D3DFMT_P8 (PAL8)',
    50: 'D3DFMT_L8 (LUM8)',
    51: 'D3DFMT_A8L8',
    0x31545844: 'DXT1',
    0x32545844: 'DXT2',
    0x33545844: 'DXT3',
    0x34545844: 'DXT4',
    0x35545844: 'DXT5',
    0x38555644: 'D3DFMT_V8U8',
    0x00003841: 'D3DFMT_A8',
}

PLATFORM_NAME = {
    0: 'NULL',
    1: 'D3D8 alt / GII',
    2: 'PS2',
    4: 'XBOX',
    5: 'GCN / Gamecube',
    6: 'ATITC / mobile',
    7: 'D3D8 (GTA3 alt)',
    8: 'D3D8 (GTA3/VC PC)',
    9: 'D3D9 (GTA SA PC)',
    11: 'PSP',
    0x53325350: 'PS2 (FourCC)',
}


def rw_version_str(v):
    """Convert RW version int to human-readable string."""
    if v == 0:
        return "0x00000000 (GTA3 early)"
    build   = (v & 0x0000FFFF)
    release = (v & 0x000F0000) >> 16
    minor   = (v & 0x00F00000) >> 20
    major   = (v & 0xFF000000) >> 24
    # RW encodes as 0xMMmmrrBB where MM=major, mm=minor (but shifted)
    # Typical: 0x1803FFFF = 3.3.0.3, 0x1C02FFFF = 3.4.0.3
    rw_major = (v >> 14) & 0x3FF
    rw_minor = (v >> 10) & 0x0F  
    rw_build = v & 0xFFFF
    return f"0x{v:08X} (RW {major}.{minor}.{release}.{build & 0xFF})"


def decode_raster_format(rf):
    """Return human-readable breakdown of raster_format_flags."""
    pixel_fmt = rf & 0x0F00  # bits 8-11 only; 0xFF00 would include PAL flags
    fmt_name  = RW_FORMAT.get(pixel_fmt, f'UNKNOWN_PIX(0x{pixel_fmt:04X})')
    flags = []
    for mask, name in sorted(RW_FLAGS.items()):
        if rf & mask:
            flags.append(name)
    flag_str = ' | '.join(flags) if flags else 'none'
    return fmt_name, flag_str


def read_u32(data, pos): return struct.unpack_from('<I', data, pos)[0]
def read_u16(data, pos): return struct.unpack_from('<H', data, pos)[0]
def read_u8(data, pos):  return data[pos]


def parse_txd(data, verbose=False, export_dir=None):
    """Parse TXD and print diagnostic info for every texture."""
    if len(data) < 12:
        print("ERROR: File too small")
        return

    # TXD outer header: type=0x16, size, version
    hdr_type    = read_u32(data, 0)
    hdr_size    = read_u32(data, 4)
    hdr_version = read_u32(data, 8)

    if hdr_type != 0x16:
        print(f"WARNING: Unexpected header type 0x{hdr_type:08X} (expected 0x16)")

    print(f"{'─'*70}")
    print(f"  TXD Header   : type=0x{hdr_type:08X}  size={hdr_size}  {rw_version_str(hdr_version)}")
    is_sa_plus = (hdr_version >= 0x1803FFFF)
    print(f"  Era          : {'SA+ (D3D9 format codes valid)' if is_sa_plus else 'GTA3/VC (use raster_format bits only)'}")

    # Struct section (type=0x01)
    if len(data) < 24:
        print("ERROR: No struct section")
        return
    struct_type    = read_u32(data, 12)
    struct_size    = read_u32(data, 16)
    struct_version = read_u32(data, 20)

    if struct_type != 0x01:
        print(f"WARNING: Expected struct 0x01, got 0x{struct_type:08X}")

    tex_count = read_u32(data, 24)
    print(f"  Texture count: {tex_count}")
    print()

    pos = 12  # start of first texture section
    # Skip TXD struct (12 + struct_size)
    pos = 12 + 12 + struct_size  # after outer header + struct header + struct data

    for tex_idx in range(tex_count):
        print(f"  ┌─ TEXTURE #{tex_idx} {'─'*50}")

        if pos + 12 > len(data):
            print(f"  │  ERROR: ran out of data at offset {pos}")
            break

        # Texture native data section header
        sec_type    = read_u32(data, pos)
        sec_size    = read_u32(data, pos + 4)
        sec_version = read_u32(data, pos + 8)

        if sec_type not in (0x15, 0x01):
            print(f"  │  NOTE: section type 0x{sec_type:08X} (expected 0x15 native texture)")

        sec_end = pos + 12 + sec_size

        # Inner struct
        inner_pos = pos + 12
        if read_u32(data, inner_pos) != 0x01:
            print(f"  │  WARNING: expected inner struct 0x01, got 0x{read_u32(data,inner_pos):08X}")
        inner_data_start = inner_pos + 12

        p = inner_data_start

        # 4: platform_id, 4: filter_flags
        platform_id   = read_u32(data, p);      p += 4
        filter_flags  = read_u32(data, p);      p += 4

        # 32: texture name, 32: alpha name
        tex_name  = data[p:p+32].rstrip(b'\x00').decode('ascii', errors='replace');  p += 32
        mask_name = data[p:p+32].rstrip(b'\x00').decode('ascii', errors='replace');  p += 32

        # raster_format(4) + d3d_format(4) + width(2) + height(2) + depth(1) + levels(1) + raster_type(1)
        raster_fmt  = read_u32(data, p);  p += 4
        d3d_fmt_raw = read_u32(data, p);  p += 4
        width       = read_u16(data, p);  p += 2
        height      = read_u16(data, p);  p += 2
        depth       = read_u8(data, p);   p += 1
        num_levels  = read_u8(data, p);   p += 1
        raster_type = read_u8(data, p);   p += 1

        # Decode raster format flags
        pix_fmt_name, flag_str = decode_raster_format(raster_fmt)
        is_pal8   = bool(raster_fmt & 0x2000)
        is_pal4   = bool(raster_fmt & 0x4000)
        has_mips  = bool(raster_fmt & 0x8000)
        is_swiz   = bool(raster_fmt & 0x10000)

        d3d_name  = D3D_FORMAT.get(d3d_fmt_raw, f'UNKNOWN(0x{d3d_fmt_raw:08X})')
        plat_name = PLATFORM_NAME.get(platform_id, f'UNKNOWN(0x{platform_id:08X})')

        print(f"  │  Name        : '{tex_name}'  mask='{mask_name}'")
        print(f"  │  Platform    : {platform_id} = {plat_name}")
        print(f"  │  Size        : {width}x{height}  depth={depth}  levels={num_levels}  raster_type={raster_type}")
        print(f"  │  raster_fmt  : 0x{raster_fmt:08X}")
        print(f"  │    pixel fmt : {pix_fmt_name}")
        print(f"  │    flags     : {flag_str}")
        print(f"  │    PAL8={is_pal8}  PAL4={is_pal4}  mipmaps={has_mips}  swizzled={is_swiz}")
        print(f"  │  d3d_format  : 0x{d3d_fmt_raw:08X} = {d3d_name}")
        print(f"  │  filter_flags: 0x{filter_flags:08X}")

        # compression byte (platform_prop): 0=raw, 1=DXT1, 3=DXT3, 5=DXT5
        # For D3D8 (GTA3/VC): this overrides raster_format for format detection
        if p < sec_end:
            extra_byte = read_u8(data, p)
            dxt_name = {1:'DXT1', 3:'DXT3', 5:'DXT5'}.get(extra_byte, 'raw/uncompressed')
            print(f"  │  compression : 0x{extra_byte:02X} = {dxt_name}")

        if verbose:
            # Raw header bytes
            raw = data[inner_data_start:inner_data_start+88]
            print(f"  │  RAW HEADER  :")
            for i in range(0, min(88, len(raw)), 16):
                hex_part = ' '.join(f'{b:02X}' for b in raw[i:i+16])
                print(f"  │    +{i:02X}  {hex_part}")

        # What format would our code pick?
        pix_map_pal = {0x0100:'ARGB1555',0x0200:'RGB565',0x0300:'ARGB4444',
                       0x0400:'LUM8',0x0500:'ARGB8888',0x0600:'RGB888',0x0A00:'RGB555'}
        pal_entry_fmt = pix_map_pal.get(raster_fmt & 0x0F00, 'ARGB8888')
        extra_byte = read_u8(data, p) if p < sec_end else 0
        if is_pal8:
            detected = f'PAL8  [palette entries: {pal_entry_fmt}]'
        elif is_pal4:
            detected = f'PAL4  [palette entries: {pal_entry_fmt}]'
        elif extra_byte in (1, 3, 5):
            detected = {1:'DXT1', 3:'DXT3', 5:'DXT5'}[extra_byte] + '  [via compression byte]'
        elif d3d_fmt_raw in (0x31545844, 0x32545844, 0x33545844, 0x34545844, 0x35545844):
            detected = {0x31545844:'DXT1',0x33545844:'DXT3',0x35545844:'DXT5',
                        0x32545844:'DXT2',0x34545844:'DXT4'}[d3d_fmt_raw]
        elif is_sa_plus:
            d3d_map = {21:'ARGB8888',32:'ARGB8888',20:'RGB888',22:'RGB888',
                       23:'RGB565',25:'ARGB1555',26:'ARGB4444',24:'RGB555',
                       50:'LUM8',51:'A8L8',41:'PAL8'}
            pix_map = {0x0100:'ARGB1555',0x0200:'RGB565',0x0300:'ARGB4444',
                       0x0400:'LUM8',0x0500:'ARGB8888',0x0600:'RGB888',0x0A00:'RGB555'}
            detected = d3d_map.get(d3d_fmt_raw, pix_map.get(raster_fmt & 0xFF00, 'UNKNOWN'))
        else:
            pix_map = {0x0100:'ARGB1555',0x0200:'RGB565',0x0300:'ARGB4444',
                       0x0400:'LUM8',0x0500:'ARGB8888',0x0600:'RGB888',0x0A00:'RGB555'}
            detected = pix_map.get(raster_fmt & 0x0F00, f'UNKNOWN(raster=0x{raster_fmt & 0x0F00:04X})')

        print(f"  │  → DETECTED  : {detected}")
        print(f"  └{'─'*63}")
        print()

        pos = sec_end

        # Skip extension section if present
        if pos + 12 <= len(data):
            ext_type = read_u32(data, pos)
            ext_size = read_u32(data, pos + 4)
            if ext_type == 0x03:  # extension
                pos += 12 + ext_size


def parse_txd_summary(data, filename):
    """Parse TXD and return list of summary dicts, one per texture."""
    results = []
    if len(data) < 24:
        return results
    hdr_version = struct.unpack_from('<I', data, 8)[0]
    is_sa_plus  = (hdr_version >= 0x1803FFFF)
    tex_count   = struct.unpack_from('<I', data, 24)[0]
    if tex_count > 4096:
        return results

    struct_size = struct.unpack_from('<I', data, 16)[0]
    pos = 12 + 12 + struct_size

    pix_map = {0x0100:'ARGB1555',0x0200:'RGB565',0x0300:'ARGB4444',
               0x0400:'LUM8',   0x0500:'ARGB8888',0x0600:'RGB888',0x0A00:'RGB555'}
    d3d_map = {21:'ARGB8888',32:'ARGB8888',20:'RGB888',22:'RGB888',
               23:'RGB565',25:'ARGB1555',26:'ARGB4444',24:'RGB555',
               50:'LUM8',51:'A8L8',41:'PAL8',
               0x31545844:'DXT1',0x33545844:'DXT3',0x35545844:'DXT5',
               0x32545844:'DXT2',0x34545844:'DXT4'}

    for _ in range(min(tex_count, 256)):
        if pos + 12 > len(data): break
        sec_size = struct.unpack_from('<I', data, pos+4)[0]
        sec_end  = pos + 12 + sec_size
        inner    = pos + 12
        if read_u32(data, inner) != 0x01: pos = sec_end; continue
        p = inner + 12

        try:
            platform_id  = read_u32(data, p);  p += 4
            p += 4  # filter_flags
            name = data[p:p+32].rstrip(b'\x00').decode('ascii', errors='replace'); p += 32
            p += 32  # mask name
            rf   = read_u32(data, p);  p += 4
            d3df = read_u32(data, p);  p += 4
            w    = read_u16(data, p);  p += 2
            h    = read_u16(data, p);  p += 2
            depth= read_u8(data, p);   p += 1
            lvls = read_u8(data, p);   p += 1

            is_pal8 = bool(rf & 0x2000)
            is_pal4 = bool(rf & 0x4000)
            pf      = rf & 0x0F00
            pal_fmt = pix_map.get(pf, 'ARGB8888')

            if is_pal8:    fmt = f'PAL8/{pal_fmt}'
            elif is_pal4:  fmt = f'PAL4/{pal_fmt}'
            elif d3df in d3d_map: fmt = d3d_map[d3df]
            elif is_sa_plus: fmt = d3d_map.get(d3df, pix_map.get(pf, f'UNK_0x{rf:08X}'))
            else: fmt = pix_map.get(pf, f'UNK_0x{rf:08X}')

            era = 'SA+' if is_sa_plus else 'VC/GTA3'
            results.append({'file':filename,'name':name,'fmt':fmt,
                            'w':w,'h':h,'depth':depth,'lvls':lvls,
                            'rf':rf,'d3d':d3df,'era':era,'plat':platform_id})
        except Exception:
            pass

        pos = sec_end
        if pos + 12 <= len(data) and read_u32(data, pos) == 0x03:
            pos += 12 + read_u32(data, pos+4)

    return results


def scan_directory(dirpath, verbose=False, summary=False, csv_out=False):
    """Scan all .txd files in a directory."""
    txd_files = []
    for root, _, files in os.walk(dirpath):
        for f in files:
            if f.lower().endswith('.txd'):
                txd_files.append(os.path.join(root, f))
    txd_files.sort()

    if summary or csv_out:
        all_rows = []
        for path in txd_files:
            try:
                with open(path, 'rb') as f:
                    data = f.read()
                rows = parse_txd_summary(data, os.path.basename(path))
                all_rows.extend(rows)
            except Exception as e:
                print(f"ERROR {os.path.basename(path)}: {e}")

        if csv_out:
            import csv, sys
            w = csv.writer(sys.stdout)
            w.writerow(['File','Texture','Format','Width','Height','Depth','Levels','Era','raster_fmt','d3d_fmt','Platform'])
            for r in all_rows:
                w.writerow([r['file'],r['name'],r['fmt'],r['w'],r['h'],r['depth'],
                            r['lvls'],r['era'],f'0x{r["rf"]:08X}',f'0x{r["d3d"]:08X}',r['plat']])
        else:
            # Compact table
            col_w = [30, 20, 16, 12, 5, 5, 7]
            hdr = (f"{'File':<{col_w[0]}} {'Texture':<{col_w[1]}} {'Format':<{col_w[2]}}"
                   f" {'raster_fmt':<{col_w[3]}} {'W':>{col_w[4]}} {'H':>{col_w[5]}} {'Era':<{col_w[6]}}")
            print(f"\n{'─'*90}")
            print(hdr)
            print('─'*90)
            for r in all_rows:
                print(f"{r['file']:<{col_w[0]}} {r['name']:<{col_w[1]}} {r['fmt']:<{col_w[2]}}"
                      f" 0x{r['rf']:08X}   {r['w']:>{col_w[4]}} {r['h']:>{col_w[5]}} {r['era']:<{col_w[6]}}")
            print('─'*90)
            # Format frequency
            from collections import Counter
            freq = Counter(r['fmt'] for r in all_rows)
            print(f"\nFormat breakdown ({len(all_rows)} textures in {len(txd_files)} files):")
            for fmt, cnt in freq.most_common():
                print(f"  {cnt:4d}x  {fmt}")
        return

    for path in txd_files:
        print(f"\n{'═'*70}")
        print(f"  FILE: {os.path.basename(path)}  ({os.path.getsize(path):,} bytes)")
        try:
            with open(path, 'rb') as f:
                data = f.read()
            parse_txd(data, verbose=verbose)
        except Exception as e:
            print(f"  ERROR: {e}")

    print(f"\n{'═'*70}")
    print(f"  Scanned {len(txd_files)} TXD files")


def main():
    ap = argparse.ArgumentParser(description='TXD Debug Inspector')
    ap.add_argument('path', help='TXD file or directory')
    ap.add_argument('--hex', action='store_true', help='Dump raw header bytes')
    ap.add_argument('--export', action='store_true', help='Export decoded textures as PNG (requires Pillow)')
    ap.add_argument('--all', dest='summary', action='store_true',
                    help='Compact one-line-per-texture summary table (directory mode)')
    ap.add_argument('--csv', action='store_true',
                    help='Output summary as CSV (directory mode, implies --all)')
    args = ap.parse_args()

    path = args.path

    if os.path.isdir(path):
        scan_directory(path, verbose=args.hex, summary=args.summary, csv_out=args.csv)
    elif os.path.isfile(path):
        print(f"{'═'*70}")
        print(f"  FILE: {os.path.basename(path)}  ({os.path.getsize(path):,} bytes)")
        with open(path, 'rb') as f:
            data = f.read()
        parse_txd(data, verbose=args.hex)
    else:
        print(f"ERROR: Path not found: {path}")
        sys.exit(1)


if __name__ == '__main__':
    main()
