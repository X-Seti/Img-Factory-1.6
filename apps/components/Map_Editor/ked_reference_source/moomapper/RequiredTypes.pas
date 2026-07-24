unit RequiredTypes;

interface

const
  singOne: Single = 1.0;

  // ids for picking
  idInst = 0;
  idPath = 1;
  idZone = 2;
  idCull = 3;
  idoccl = 4;
  idIDEPath = 5;

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

function encodebigsmall(big, small: integer): integer;
function getbig(const x: integer): integer;
function getsmall(const x: integer): integer;

function ArcSin(const x : Single): Single;
function ArcCos(const x: Single): Single; register;

procedure report(const text: string);

var
 iplpathmp: single = 0.0625;
 pathlinewidth: single = 3;
 pathcubesize: single = 1;

 pathlinecolora: integer = -16711681;
 pathlinecolorb: integer = -16776961;
 pathlinecolorc: integer = -16711936;

 // Artem: Flag to draw GTA3 style paths
 idepathdraw: boolean = false;

implementation

uses main;

procedure report(const text: string);
begin
if main.FormMain.MemoDebug <> nil then if
main.FormMain.MemoDebug.HandleAllocated = true then
main.FormMain.MemoDebug.Lines.add(text);
end;

// bit coding for enchanged object picking by delfi
function encodebigsmall(big, small: integer): integer;
begin
result:= small;
result:= result or (big shl 8);
end;

function getbig(const x: integer): integer;
begin
// get last 24 bits
result:= x shr 8;
end;

function getsmall(const x: integer): integer;
begin
// get first 8 bits
result:= x and $000000ff;
end;

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
