# COL Format Analysis — CollEditor2 Reverse Engineering
**Source**: CollEditor2_unpacked.exe (UPX unpacked)
**Method**: Static analysis via Python struct tracing
**Date**: March 2026

---

## Binary Info
- Format: PE32 executable (GUI) Intel 80386
- Language: Delphi/Borland (statically linked MSVCRT)
- File I/O: KERNEL32.DLL ReadFile/WriteFile (no MSVCRT fread)
- Packer: UPX (removed with `upx -d`)
- Image Base: 0x00400000

## Key Addresses (unpacked)
| Address    | Description |
|------------|-------------|
| 0x004CF8E4 | Delphi VMT / class descriptor for COL object |
| 0x004B79EC | COL1 reader function entry |
| 0x004B79FC | COL2 reader function entry |
| 0x004B7A0C | COL3 reader function entry |
| 0x004B7C44 | Stream read dispatcher (calls virtual stream.Read) |
| 0x004B99FC | "No" dialog string |
| 0x004B9A08 | "Yes" dialog string |

## VMT Layout at 0x004CF8E4
```
+00: 0x004B7080  <- base class method
+04: 'COLL'      <- COL1 magic identifier
+08: 'COL2'      <- COL2 magic identifier
+0C: 'COL3'      <- COL3 magic identifier
+10: 0x004B79EC  <- COL1 read/write function
+14: 0x004B79FC  <- COL2 read/write function
+18: 0x004B7A0C  <- COL3 read/write function
+1C: float       <- unknown (0x3C23D70A)
+20: 0x004B99FC  <- "No" dialog
+24: 0x004B9A08  <- "Yes" dialog
+28: 0x00000001
+38: 0x00000018  <- 24 (name field size)
+3C: 0x00000030  <- 48 (header size?)
```

---

## Confirmed Struct Sizes (from stream reader at 0x004B7C44)

| Size (bytes) | Struct | Notes |
|-------------|--------|-------|
| 40 | Bounding sphere + box | Read twice at start — center(12)+radius(4)+pad(24) |
| 20 | COLSphere | center:Vector3(12) + radius:float(4) + material:uint32(4) |
| 28 | COLBox | min:Vector3(12) + max:Vector3(12) + material:uint32(4) |
| 6  | COLVertex COL1 | x:int16 + y:int16 + z:int16 |
| 12 | COLVertex COL2/3 | x:float + y:float + z:float |
| 8  | COLFace | v0:uint16 + v1:uint16 + v2:uint16 + mat:uint8 + light:uint8 |
| 4  | Count fields | uint32 — sphere_count, box_count, face_count etc. |
| 22 | Unknown | Possibly header name block (24 - 2 for model_id?) |
| 14 | Unknown | Possibly COL1 combined vertex+face? |
| 30 | Unknown | Possibly COLFaceGroup |
| 36 | Shadow mesh vertex | 9 floats? Or 3x Vector3? Shadow mesh COL3 feature |
| 1  | Flags byte | Per-element flag byte |

---

## COL File Header Layout (confirmed)
```c
struct COLHeader {
    char     magic[4];     // "COLL", "COL2", "COL3"
    uint32_t file_size;    // size of data after this point
    char     model_name[24]; // null-padded ASCII name
    uint16_t model_id;     // object model ID
};
// Total header = 4+4+24+2 = 34 bytes
```

## COL1 Model Layout (confirmed sizes)
```c
struct COL1Model {
    // Bounding sphere
    float    bs_center[3];  // 12 bytes
    float    bs_radius;     //  4 bytes
    // Bounding box  
    float    bb_min[3];     // 12 bytes
    float    bb_max[3];     // 12 bytes
    // = 40 bytes total for bounds block

    uint32_t sphere_count;
    COLSphere spheres[];    // each 20 bytes

    uint32_t box_count;
    COLBox   boxes[];       // each 28 bytes

    uint32_t vertex_count;
    COLVertex1 vertices[];  // each 6 bytes (int16 x3)

    uint32_t face_count;
    COLFace  faces[];       // each 8 bytes
};

struct COLSphere {          // 20 bytes
    float    center[3];     // 12
    float    radius;        //  4
    uint8_t  surface;       //  1
    uint8_t  piece;         //  1
    uint16_t pad;           //  2
};

struct COLBox {             // 28 bytes
    float    min[3];        // 12
    float    max[3];        // 12
    uint8_t  surface;       //  1
    uint8_t  piece;         //  1
    uint16_t pad;           //  2
};

struct COLVertex1 {         // 6 bytes
    int16_t  x, y, z;
};

struct COLFace {            // 8 bytes
    uint16_t v0, v1, v2;
    uint8_t  material;
    uint8_t  light;
};
```

## COL2/3 Differences
- Vertices are float32 (12 bytes each) not int16 (6 bytes)
- COL3 adds face groups (shadow mesh)
- Shadow mesh vertex = 36 bytes (possibly 3x Vector3 for triangle?)
- Face flags/lighting stored differently

---

## Notes
- Our existing `col_core_classes.py` struct sizes are CORRECT
- The parsing issue was `_generate_collision_thumbnail` was called but not defined
- Fixed in Build 111 with QPainter 2D top-down renderer
- The 22/30/36 byte unknowns need further investigation
- CollEditor2 is written in Delphi — class methods follow Delphi ABI (ESI=self)

---

## Tools Used
- `upx -d CollEditor2.exe` — unpack
- Custom Python PE analyser (see `apps/utils/ghidra_col_analyzer.py`)
- Ghidra with PyGhidra (`/opt/ghidra/support/pyghidraRun`)
