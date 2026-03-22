# COL Format Analysis — CollEditor2 RE + special.col verification
**Sources**: CollEditor2_unpacked.exe (binary RE) + special.col (GTA:SA SOL mod)
**Method**: Static binary analysis + empirical byte-by-byte parsing
**Date**: March 2026
**Status**: COL1 fully verified — all 5 models in special.col parse correctly

---

## COL1 File Format — VERIFIED CORRECT

### File Structure
A COL file contains one or more models concatenated sequentially.

```
[Model 1][Model 2][Model 3]...
```

Each model:
```
magic(4) + data_size(4) + model_data(data_size bytes)
```
Total model bytes = data_size + 8

---

### COL1 Model Layout

```
Offset  Size  Type      Field
──────────────────────────────────────────────────
0       4     char[4]   magic = "COLL"
4       4     uint32    data_size  (bytes after this field)
── model_data starts here ──
8       22    char[22]  model_name (null-padded ASCII)
30      2     uint16    model_id
── bounding_sphere ──
32      12    float[3]  bs_center  (x, y, z)
44      4     float     bs_radius
── bounding_box ──
48      12    float[3]  bb_min     (x, y, z)
60      12    float[3]  bb_max     (x, y, z)
── spheres ──
72      4     uint32    num_spheres
76      20*N  COLSphere spheres[N]
── boxes ──
?       4     uint32    num_boxes
?       28*M  COLBox    boxes[M]
── face_groups ──
?       4     uint32    num_face_groups
?       28*G  COLFaceGroup face_groups[G]
── vertices ──
?       4     uint32    num_vertices
?       12*V  float[3]  vertices[V]   ← FLOAT not int16!
── faces ──
?       4     uint32    num_faces
?       16*F  COLFace   faces[F]      ← 16 bytes not 8!
```

---

### Struct Definitions

```c
// COLSphere — 20 bytes
struct COLSphere {
    float    center[3];   // 12 bytes
    float    radius;      //  4 bytes
    uint8_t  surface;     //  1 byte  (material ID)
    uint8_t  piece;       //  1 byte
    uint16_t pad;         //  2 bytes
};

// COLBox — 28 bytes
struct COLBox {
    float    min[3];      // 12 bytes
    float    max[3];      // 12 bytes
    uint8_t  surface;     //  1 byte
    uint8_t  piece;       //  1 byte
    uint16_t pad;         //  2 bytes
};

// COLFaceGroup — 28 bytes  (PRESENT IN COL1, not just COL3)
struct COLFaceGroup {
    float    min[3];      // 12 bytes  bounding box min
    float    max[3];      // 12 bytes  bounding box max
    uint8_t  surface;     //  1 byte   material
    uint8_t  pad1;        //  1 byte
    uint16_t pad2;        //  2 bytes
};

// COLVertex — 12 bytes  (FLOAT not int16!)
struct COLVertex {
    float    x, y, z;    // 12 bytes
};

// COLFace — 16 bytes  (uint32 indices, not uint16!)
struct COLFace {
    uint32_t v0, v1, v2; // 12 bytes  vertex indices
    uint8_t  material;   //  1 byte   surface type
    uint8_t  light;      //  1 byte   lighting
    uint16_t pad;        //  2 bytes
};
```

---

### IMPORTANT CORRECTIONS vs GTAMods Wiki

| Field | GTAMods Wiki | ACTUAL (verified) |
|-------|-------------|-------------------|
| model_name size | 24 bytes | **22 bytes** |
| model_id offset | 32 | **30** |
| data_start offset | 34 | **32** |
| COLVertex type | int16 (6 bytes) | **float32 (12 bytes)** |
| COLFace size | 8 bytes | **16 bytes** |
| COLFace v0/v1/v2 type | uint16 | **uint32** |
| Face groups in COL1 | No | **Yes** |

---

### Verified Examples from special.col

| Model | Magic | Spheres | Boxes | FaceGroups | Vertices | Faces |
|-------|-------|---------|-------|------------|----------|-------|
| tram | COLL | 14 | 0 | 1 | 28 | 16 |
| train | COLL | 0 | 0 | 0 | 284 | 198 |
| train_dl | COLL | 0 | 0 | 0 | 126 | 96 |
| train_dr | COLL | 0 | 0 | 0 | 126 | 96 |
| Ferry | COLL | 0 | 0 | 0 | 909 | 1026 |

---

### Material ID 63 (0x3F)
The tram model uses material 63 consistently.
In GTA surface types: 63 = METAL or similar hard surface.

---

## Binary Source Analysis (CollEditor2)

- **Location**: `/opt/ghidra/support/pyghidraRun` for analysis
- **Packer**: UPX — unpack with `upx -d CollEditor2.exe`
- **Language**: Delphi/Borland (statically linked)
- **Stream reader**: `0x004B7C44` — virtual stream.Read dispatcher
- **COL1 reader**: `0x004B79EC` (function after "Col 1\0\0\0" string)
- **COL2 reader**: `0x004B79FC`
- **COL3 reader**: `0x004B7A0C`

### Read sizes confirmed from binary
| Size | Struct |
|------|--------|
| 40 | Bounding sphere + radius (full block) |
| 20 | COLSphere |
| 28 | COLBox |
| 6  | (COL1 vertex — **but actual file shows float=12**) |
| 12 | COLVertex COL2/3 — also used in COL1 |
| 16 | COLFace (confirmed from file parsing) |
| 28 | COLFaceGroup |

---

## Impact on IMG Factory Parser

The existing `col_core_classes.py` and `col_workshop_parser.py` have:
- `COLVertex` storing `position: Vector3` (floats) — **correct**
- `COLFace.vertex_indices: Tuple[int,int,int]` — **correct**
- Face parsing reading uint16 indices — **WRONG, should be uint32**
- Face struct size 8 bytes — **WRONG, should be 16 bytes**
- Name field 24 bytes — **WRONG, should be 22 bytes + 2 for model_id**

## TODO: Fix Parser
- `col_workshop_parser.py`: fix face struct to read uint32 indices
- `col_core_classes.py`: COLVertex already uses float (correct)
- Fix header parsing: name=22 bytes not 24

---

**Last Updated**: March 2026 — Build 112
