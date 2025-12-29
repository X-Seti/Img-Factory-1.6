unit RequiredTypes;

interface

const
  singOne: Single = 1.0;

type

TVector2i = array[0..1] of Integer;
TVector3i = array[0..2] of Integer;
TVector4i = array[0..3] of Integer;

TVector2f = array[0..1] of Single;
TVector3f = array[0..2] of Single;
TVector4f = array[0..3] of Single;

TVector2d = array[0..1] of Double;
TVector3d = array[0..2] of Double;
TVector4d = array[0..3] of Double;

TMatrix2f = array[0..1] of TVector2f;
TMatrix3f = array[0..2] of TVector3f;
TMatrix4f = array[0..3] of TVector4f;

TMatrix2d = array[0..1] of TVector2d;
TMatrix3d = array[0..2] of TVector3d;
TMatrix4d = array[0..3] of TVector4d;

function ArcSin(const x : Single): Single;
function ArcCos(const x: Single): Single; register;

implementation

// maths stuff in here as well
// cant remember who gave me these speedy versions

function ArcSin(const x: Single): Single;
asm
      FLD   X
      FLD   ST
      FMUL  ST, ST
      FSUBR singOne
      FSQRT
      FPATAN
end;

function ArcCos(const x: Single): Single; register;
asm
      FLD   X
      FMUL  ST, ST
      FSUBR singOne
      FSQRT
      FLD   X
      FPATAN
end;

end.
