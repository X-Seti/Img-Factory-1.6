unit gtadff;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, OpenGL;

const
// sections
  rwDATA          =  1;
  rwSTRING        =  2;
  rwEXTENSION     =  3;
  rwTEXTURE       =  6;
  rwMATERIALLIST  =  8;
  rwMATERIAL      =  7;
  rwFRAMELIST     =  14;
  rwGEOMETRY      =  15;
  rwCLUMP         =  16;
  rwATOMIC        =  20;
  rwGEOMETRYLIST  =  26;
  rwAnimPlugin      = 286;
  rwMATERIALEFFECTS = 288; // xvehicleenv128
  rwMATERIALSPLIT =  1294;
  rwFRAME         =  39056126;

// constants for geometry data
  rwOBJECT_VERTEX_TRISTRIP = $1;
  rwOBJECT_VERTEX_POSITIONS = $2;
  rwOBJECT_VERTEX_UV = $4;
  rwOBJECT_VERTEX_COLOR = $8;
  rwOBJECT_VERTEX_NORMAL = $10;
  rwOBJECT_VERTEX_LIGHT = $20;
  rwOBJECT_VERTEX_MODULATE = $40;
  rwOBJECT_VERTEX_TEXTURED = $80;

// rockstar north extensions
  rnmultitexturespeca = 39056118;
  col3                = 39056122;

type
  TVector3i = array [0..2] of Longint;
  TVector3f = array [0..2] of Single;
  //TMatrix3f = array [0..2] of TVector3f;
  TMatrix3f = array [0..2, 0..2] of single;//TVector3f;

  TMatrix4f = array [0..3, 0..3] of single;//TVector4f;

  // big part collision info here is based on research of Steve M and Kam.
  TVector3W = array [0..2] of word; // float:= vector[n] / 128;

  Tcollbox = packed record
  box_min, box_max: TVector3f;
  end;

  Tcollsphere = packed record
  sphere_center: TVector3f;
  sphere_radius: single;
  SurfaceA, SurfaceB: word;
  end; // 40 bytes needed

  Tcolface = packed record
  A, B, C: word;
  SurfaceA, SurfaceB: word;
  end;

  Tcollisionmodel = packed record
  col3: array[0..3] of char;
  size: longword;
  name: array[0..23] of char;
  box_min, box_max, sphere_center: TVector3f;
  sphere_radius: single;
  Spherec,
  ColFacec,
  LW12,
  OFSspheres,
  LW0_0,
  LW0_1,
  OFS_VERT,
  OFS_Faces,
  LW0_2,
  ShadowFacec,
  OFSShadowvert,
  OFSShadowFace
  : longword;

  Dspheres: array of Tcollsphere;

  // collision mesh
  ColVerts: array of TVector3W;
  ColFaces: array of Tcolface;

  ColShadeVerts: array of TVector3W;
  ColShadeFaces: array of Tcolface;

  end; // 120 bytes

  TDFFFace = record
    V2: Word;
    V1: Word;
    material: Word;
    V3: Word;
  end;

  TDFFUV = record
    U, V: Single;
  end;

  TDFFUVMAP = array of TDFFUV;

  TDFFFrame = record
    Name: String;

    matrix4: TMatrix4f;

    Matrix: TMatrix3f;
    Coord: TVector3f;

    Parent: LongInt;
    Other1, Other2: Word;
    internaldata: pointer; // used by GGMM
  end;

  // data parts

  TDFFDataClump = record
    ObjectCount: LongWord;
  end;

  TDFFDataFrameList = record
    FrameCount: LongWord;
    Frame: array of TDFFFrame;
  end;

  TDFFDataGeometryList = record
    GeometryCount: LongWord;
  end;

  TDFFDataAtomic = record
    FrameNum: LongWord;
    GeometryNum: LongWord;
    Other1: LongWord; //D: 5
    Other2: LongWord; //D: 0
  end;

  TDFFHeaderDataGeometry = record
    Flags: Word;
    UVmaps, unknown: byte;

    TriangleCount: LongWord;
    VertexCount: LongWord;
    MorphCount: LongWord; // was OtherCount
  end;

  TDFFLightHeaderDataGeometry = record
    Ambient: Single;
    Diffuse: Single;
    Specular: Single;
  end;

  TDFFDataGeometryBoundingSphere = record
    boundingsphere: TVector3f;
    BoundingRadius: single;
    Other1, Other2: LongWord; //D: 1
  end;

  TDFFDataGeometry = record
    Header: TDFFHeaderDataGeometry;

    LightHeader: TDFFLightHeaderDataGeometry;

    Color: array of LongWord;

    UVmaps: array of TDFFUVMAP;

    Face: array of TDFFFace;

    BoundingSphere: TDFFDataGeometryBoundingSphere;

    Vertex: array of TVector3f;

    Normal: array of TVector3f;
  end;

  TDFFDataMaterialList = record
    MaterialCount: LongWord;
    Other: LongWord; //D: FF
  end;

  TDFFColor = packed array[0..4] of Byte;
  TDFFDataMaterial = packed record
    Other1: LongWord; //D: 0 // alpha params?
    Color: TDFFColor;
    Other3: LongWord;
    TextureCount: LongWord; //D: 1

    Other5: Single; //D: 1.0 // shine?
    Other6: Single;          // size?
    Other7: Single; //D: 1.0 // opacity?
  end;

  TDFFDataTexture = record
  end;

  // level 5

  TDFFExtensionTexture = record
  data: array[0..59] of byte;
  x: array[0..47] of char;
  end;

  TDFFTextureMatPlugin = packed record
    lw2a: longword;
    lw2b: longword;
    flags: longword;
    lw0a: longword;

    stuff: array[0..31] of byte;
    maptype: longword; // 16 = san andreas xvehicleenv (second uv map thing), 20 = sphere mapping (like chrome)
    FFFFthing: longword;
    vehicleenv: array[0..15] of char;
    morestuff: array[0..300] of byte;

// no speca:
//    2
//    2
//    0
//    0
//    0
//    0

// with speca:
//    2
//    2
//    bit flags? (1065353216) (-------- -------- *------- --******)
//    0
//    1
//    6
//    72           -> some kind of section size indicator, add to position 4 bytes (data seem to belong to this header) and the data size will be at this number + 8
//    FF FF 03 18
//    1
//    4
//    FF FF 03 18
//    flags (69894)
//    2
//    1
//    FF FF 03 18
//    26 bytes text padded with zeroes to 4 byte alignment (xvehicleenv128)
//    2
//    4
//    FF FF 03 18
//    0
//    3
//    0
//    FF FF 03 18
//    0

    // ..up to 288 bytes of garbage..
  end;

  TDFFTexture = record
    Data: TDFFDataTexture;
    Name: String;
    Desc: String;
    GotName: Boolean;

    // delfi's hack for san andreas reflections
    speca: array[0..255] of char;

    matpluginsize: integer;

    matplugin: TDFFTextureMatPlugin;

//    Extension: TDFFExtensionTexture;
  end;

  // level 4

  TDFFMaterial = record
    Data: TDFFDataMaterial;
    _test_Offset: integer;
    Texture: TDFFTexture;
    //Extension: TDFFExtensionMaterial;
  end;

  // level 3

  TDFFHeaderMaterialSplit = record
    TriagleFormat: LongWord; // 0 = triangles, 1= trianglestrip
    SplitCount: LongWord;
    FaceCount: LongWord;
  end;

  TDFFSplit = record
    FaceIndex: LongWord;
    MaterialIndex: LongWord;

    Index: array of LongWord;
  end;

  TDFFMaterialSplit = record
    Header: TDFFHeaderMaterialSplit;
    Split: array of TDFFSplit;
  end;

  TDFFMaterialList = record
    Data: TDFFDataMaterialList;
    Material: array of TDFFMaterial;
    MaterialCount: Word;
  end;

  // level 2

  TDFFGeometry = record
    Data: TDFFDataGeometry;
    MaterialList: TDFFMaterialList;
    MaterialSplit: TDFFMaterialSplit;
  end;

  // level 1

  TDFFFrameList = record
    Data: TDFFDataFrameList;
  end;

  TDFFGeometryList = record
    Data: TDFFDataGeometryList;
    Geometry: array of TDFFGeometry;
    GeometryCount: LongWord;
  end;

  TDFFAtomic = record
    Data: TDFFDataAtomic;
    //Extension: TDFFExtensionAtomic;
  end;

  // level 0

  TDFFClump = record
    Data: TDFFDataClump;
    FrameList: TDFFFrameList;
    GeometryList: TDFFGeometryList;
    Atomic: array of TDFFAtomic;
    AtomicCount: Word;
    col3: Tcollisionmodel;
  end;

  // header

  TDFFHeader = record
    Start: LongWord;
    Back: LongWord;

    Tag: LongWord;
    Size: LongWord;
    renderversion: longword;
//    Data: Word; //D: 784
//    Version: Word;
  end;

  TDffLoader = class
  private
    function GetNextHeader(Stream: TStream; Level, Parent: LongInt): TDFFHeader;
    procedure ParseData(Stream: TStream; ParseHeader: TDFFHeader; Parent: LongInt);
    procedure ParseMaterialSplit(Stream: TStream; ParseHeader: TDFFHeader; Parent: LongInt);
    procedure ParseHeaders(Stream: TStream; ParseHeader: TDFFHeader; Level, Parent: LongInt);
    procedure ParseString(Stream: TStream; ParseHeader: TDFFHeader; Level, Parent: LongInt);
  public
    Clump: Array of TDFFClump;
    FrameUpTo: LongInt;
    lastofs: integer;
    procedure ResetClump;
    procedure LoadFromFile(FileName: string);
    procedure LoadFromStream(Stream: TStream); overload;
    procedure LoadFromStream(st: TStream; in_start, in_size: Int64); overload;
  end;

  TGTADff = class(TDffLoader)
  private
    Stream: TStream;
    FStart, FSize: Int64;

  public
    Name: String;
    Loaded, InUse: Boolean;

    constructor Create; overload;
    constructor Create(st: TStream; in_name: String; in_start, in_size: Int64); overload;
    destructor Destroy; override;

    procedure LoadFromStream; overload;
    procedure Unload;

    procedure glDraw(in_texture: LongInt);
    procedure glDrawRecurse(in_clump: LongWord; in_frame: LongInt; in_texture: LongInt; TheParent: Boolean);

    function SetTexture(Name: string; in_texture: LongInt): Boolean;
    function SetAlpha(Alpha: string; in_texture: LongInt): Boolean;
  end;

implementation

uses Main, GLView, GTATxd;

// dff loader

function GetRWVersion(ver: cardinal): cardinal; // Steve M.
var b: byte;
begin
  b:=ver shr 24;
  result:=(3 + b shr 6) shl 16
        + ((b shr 2) and $0F) shl 12
        + (b and $03) shl 8
        + byte(ver shr 16);
end;

function DecompressVector(v: TVector3W): TVector3F; // Steve M.
begin
 result[0]:=(smallint(v[0])/128);
 result[1]:=(smallint(v[1])/128);
 result[2]:=(smallint(v[2])/128);
end;

procedure TDffLoader.ResetClump;
var i: integer;
begin
  for i := 0 to High(Clump) do
  begin
    SetLength(Clump[i].Atomic, 0);
    SetLength(Clump[i].GeometryList.Geometry, 0);
    SetLength(Clump[i].FrameList.Data.Frame, 0);
  end;
  Clump := nil;
end;

procedure TDffLoader.LoadFromStream(Stream: TStream);
var
  MainHeader: TDFFHeader;
begin
  ResetClump;
  MainHeader.Start := 16;
  MainHeader.Tag := 0;
  MainHeader.Size := Stream.Size;
  MainHeader.renderversion:= 0;//Data := 0;
//  MainHeader.Version := 0;
  MainHeader.Back := 0;

  ParseHeaders(Stream, MainHeader, 0, 16);

//  lastofs:= stream.position;
end;

procedure TDffLoader.LoadFromStream(st: TStream; in_start, in_size: Int64);
var
  MainHeader: TDFFHeader;
begin
  ResetClump;
  
  MainHeader.Start := in_start + 16;
  MainHeader.Tag := 0;
  MainHeader.Size := in_size;
  MainHeader.renderversion := 0;
  MainHeader.Back := in_start;

  st.Seek(in_start, soFromBeginning);
  ParseHeaders(st, MainHeader, 0, 16);
end;

procedure TDffLoader.LoadFromFile(FileName: String);
var
  Stream: Tmemorystream;
begin
  Stream := Tmemorystream.Create;
  stream.loadfromfile(FileName);
//  application.processmessages;
  LoadFromStream(Stream);
//  application.processmessages;
  Stream.Free;
end;

procedure TDffLoader.ParseMaterialSplit(Stream: TStream; ParseHeader: TDFFHeader; Parent: LongInt);
var
  I: LongInt;
begin
  with Clump[High(Clump)].GeometryList.Geometry[Clump[High(Clump)].GeometryList.GeometryCount - 1].MaterialSplit do
  begin
   // ShowMessage(IntToStr(Stream.Position));
    Stream.Read(Header, SizeOf(Header));

    SetLength(Split, Header.SplitCount);

    for I := 0 to Header.SplitCount - 1 do
    begin
      Stream.Read(Split[I].FaceIndex, 4);
      Stream.Read(Split[I].MaterialIndex, 4);

      SetLength(Split[I].Index, Split[I].FaceIndex);
      Stream.Read(Split[I].Index[0], 4 * Split[I].FaceIndex);
    end;
  end;
end;

procedure TDffLoader.ParseData(Stream: TStream; ParseHeader: TDFFHeader; Parent: LongInt);
var
  I, J, fix: LongWord;
  f: file;
begin

  case Parent of

  rwCLUMP:
    begin
      Stream.Read(Clump[High(Clump)].Data.ObjectCount, 4);
    end;

  rwMATERIALLIST:
    begin
      with Clump[High(Clump)].GeometryList.Geometry[Clump[High(Clump)].GeometryList.GeometryCount - 1].MaterialList.Data do
      begin
        Stream.Read(MaterialCount, 4);
        Stream.Read(Other, 4);
      end;
    end;

  rwMATERIAL:
    begin
      with Clump[High(Clump)].GeometryList.Geometry[Clump[High(Clump)].GeometryList.GeometryCount - 1].MaterialList.Material[Clump[High(Clump)].GeometryList.Geometry[Clump[High(Clump)].GeometryList.GeometryCount - 1].MaterialList.MaterialCount - 1] do
      begin
        Texture.GotName := False;
        _test_Offset := Stream.Position;
        //showmessage(inttostr(Stream.Position));
        Stream.Read(Data, SizeOf(Data));

// output colors
{// outputdebugstring(
pchar(
format('color: %d %d %d %d', [Data.Color[0], Data.Color[1], Data.Color[2], Data.Color[3]
])
)
);}
      end;
    end;

  rwGEOMETRYLIST:
    begin
      Stream.Read(Clump[High(Clump)].GeometryList.Data.GeometryCount, 4);
    end;

  rwATOMIC:
    begin
      with Clump[High(Clump)].Atomic[Clump[High(Clump)].AtomicCount - 1].Data do
      begin
        Stream.Read(FrameNum, 4);
        Stream.Read(GeometryNum, 4);
        Stream.Read(Other1, 4);
        Stream.Read(Other2, 4);
      end;
    end;

  rwFRAMELIST:
    begin
      with Clump[High(Clump)].FrameList.Data do
      begin
        Stream.Read(FrameCount, 4);
        SetLength(Frame, FrameCount);
        FrameUpTo := 0;

        for I := 0 to FrameCount - 1 do
        begin

          for J := 0 to 2 do
          begin
            Stream.Read(Frame[I].Matrix[J], 12);
          end;
          begin
            Stream.Read(Frame[I].Coord, 12);
          end;

        fillchar(Frame[I].Matrix4, sizeof(Frame[I].Matrix4), 0);

        Frame[I].Matrix4[0, 0]:= 1;
        Frame[I].Matrix4[1, 1]:= 1;
        Frame[I].Matrix4[2, 2]:= 1;

        Frame[I].Matrix4[0, 0]:= Frame[I].Matrix[0, 0];
        Frame[I].Matrix4[0, 1]:= Frame[I].Matrix[0, 1];
        Frame[I].Matrix4[0, 2]:= Frame[I].Matrix[0, 2];

        Frame[I].Matrix4[1, 0]:= Frame[I].Matrix[1, 0];
        Frame[I].Matrix4[1, 1]:= Frame[I].Matrix[1, 1];
        Frame[I].Matrix4[1, 2]:= Frame[I].Matrix[1, 2];

        Frame[I].Matrix4[2, 0]:= Frame[I].Matrix[2, 0];
        Frame[I].Matrix4[2, 1]:= Frame[I].Matrix[2, 1];
        Frame[I].Matrix4[2, 2]:= Frame[I].Matrix[2, 2];

        Frame[I].Matrix4[3, 0]:= frame[I].Coord[0];
        Frame[I].Matrix4[3, 1]:= frame[I].Coord[1];
        Frame[I].Matrix4[3, 2]:= frame[I].Coord[2];

        Frame[I].Matrix4[3, 3]:= 1;

          Stream.Read(Frame[I].Parent, 4);
          Stream.Read(Frame[I].Other1, 2);
          Stream.Read(Frame[I].Other2, 2);
        end;
      end;
    end;

  rwGEOMETRY:
    begin
      with Clump[High(Clump)].GeometryList.Geometry[Clump[High(Clump)].GeometryList.GeometryCount - 1].Data do
      begin

      fix:= stream.position;

      Stream.Read(Header, SizeOf(Header));

// Scene colors only for RW versions before 3.4 (GTA3)
      if GetRWVersion(ParseHeader.renderversion) < $34000 then
        Stream.Read(LightHeader, SizeOf(LightHeader))
      else
        FillChar(LightHeader, SizeOf(LightHeader), 0);

// outputdebugstring(pchar('Start: ' + inttostr(fix)));
// outputdebugstring(pchar('Flags: ' + inttostr(Header.Flags)));
// outputdebugstring(pchar('UVmaps: ' + inttostr(Header.UVmaps)));
// outputdebugstring(pchar('unknown: ' + inttostr(Header.unknown)));
// outputdebugstring(pchar('TriangleCount: ' + inttostr(Header.TriangleCount)));
// outputdebugstring(pchar('VertexCount: ' + inttostr(Header.VertexCount)));
// outputdebugstring(pchar('MorphCount: ' + inttostr(Header.MorphCount)));
  
{
  rwOBJECT_VERTEX_TRISTRIP = $1;
  rwOBJECT_VERTEX_POSITIONS = $2;
  rwOBJECT_VERTEX_UV = $4;
  rwOBJECT_VERTEX_COLOR = $8;
  rwOBJECT_VERTEX_NORMAL = $10; // 16
  rwOBJECT_VERTEX_LIGHT = $20;
  rwOBJECT_VERTEX_MODULATE = $40;
  rwOBJECT_VERTEX_TEXTURED = $80;
}

      // read vertex colors
      if (rwOBJECT_VERTEX_COLOR and Header.Flags) = rwOBJECT_VERTEX_COLOR then
      begin
        // outputdebugstring('READING: VERTEX COLORS');
        SetLength(Color, Header.VertexCount);
        Stream.Read(Pointer(Color)^, 4 * Header.VertexCount);
      end else
        SetLength(Color, 0);

// zmodeler2 compatibility - zmodeler2 doesn't set the flags properly.
// we can aniway find if uv channels are present from uv channel count (as gta seem to do this as well)

//        if (rwOBJECT_VERTEX_UV and Header.Flags) = rwOBJECT_VERTEX_UV then
//        If ((Header.Flags and rwOBJECT_VERTEX_UV) <> 0) or ((Header.Flags and 128) <> 0) then
//        begin

        if header.UVmaps <> 0 then begin
        setlength(uvmaps, header.UVmaps);
        // outputdebugstring('READING: UV data');

        for i:= 0 to header.UVmaps-1 do begin
          setlength(uvmaps[i], Header.VertexCount);
          Stream.Read(UVmaps[i][0], 8 * Header.VertexCount);
        end;
        end else
          SetLength(UVmaps, 0);

{        If ((Header.Flags and rwOBJECT_VERTEX_UV) <> 0) or ((Header.Flags and 128) <> 0) then
        begin
          SetLength(UV, Header.VertexCount);
          Stream.Read(UV[0], 8 * Header.VertexCount); // read first uv map

          if header.UVmaps = 2 then // read second uv map
          Stream.Read(UV2[0], 8 * Header.VertexCount);

          stream.Seek((8 * Header.VertexCount) * (header.UVmaps - 1), sofromcurrent);
        end else
          SetLength(UV, 0);}

        // outputdebugstring('READING: FACE INDICES');
        SetLength(Face, Header.TriangleCount);
        Stream.Read(Pointer(Face)^, 8 * Header.TriangleCount);

        // outputdebugstring('READING: Bounding Sphere');
        Stream.Read(BoundingSphere, SizeOf(BoundingSphere));

        // outputdebugstring('READING: VERTICES');
        SetLength(Vertex, Header.VertexCount);
        Stream.Read(Pointer(Vertex)^, 12 * Header.VertexCount);

        if (rwOBJECT_VERTEX_NORMAL and Header.Flags) = rwOBJECT_VERTEX_NORMAL then
        begin
          // outputdebugstring('READING: Normals');
          SetLength(Normal, Header.VertexCount);
          Stream.Read(Pointer(Normal)^, 12 * Header.VertexCount);
        end else
          SetLength(Normal, 0);

          stream.position:= fix + parseheader.Size;

// outputdebugstring(pchar('Color: ' + inttostr(high(Color))));
// outputdebugstring(pchar('UVmaps: ' + inttostr(high(UVmaps))));
// outputdebugstring(pchar('Face: ' + inttostr(high(Face))));
// outputdebugstring(pchar('Vertex: ' + inttostr(high(Vertex))));
// outputdebugstring(pchar('Normal: ' + inttostr(high(Normal))));

      end;
    end;
  end;
end;

procedure TDffLoader.ParseString(Stream: TStream; ParseHeader: TDFFHeader; Level, Parent: LongInt);
var
  Buf: PChar;
  PreString: String;
  I: Integer;
begin
  PreString := '';
  for I := 0 to Level do
    PreString := PreString + '      ';

  GetMem(Buf, ParseHeader.Size+1);
  Buf[ParseHeader.Size] := #0;
  Stream.Read(Pointer(Buf)^, ParseHeader.Size);

  case Parent of

  rwTEXTURE:
    begin
      with Clump[High(Clump)].GeometryList.Geometry[Clump[High(Clump)].GeometryList.GeometryCount - 1].MaterialList.Material[Clump[High(Clump)].GeometryList.Geometry[Clump[High(Clump)].GeometryList.GeometryCount - 1].MaterialList.MaterialCount - 1].Texture do
      begin

// output texture names
//      // outputdebugstring(pchar(Name));

        if GotName then
          Desc := Trim(Buf)
        else
          Name := Trim(Buf);
        GotName := True;
      end;
    end;

  rwFRAMELIST:
    begin
      Clump[High(Clump)].FrameList.Data.Frame[FrameUpTo].Name := Trim(Buf);
      Inc(FrameUpTo);
    end;
  end;

  FreeMem(Buf);
end;

procedure TDffLoader.ParseHeaders(Stream: TStream; ParseHeader: TDFFHeader; Level, Parent: LongInt);
var
  InHeader: TDFFHeader;
  MoreData: Boolean;
  pre: integer;
begin
  MoreData := True;
  while MoreData do
  begin
    InHeader := GetNextHeader(Stream, Level, Parent);

    if (InHeader.Tag = rwClump) then
    begin
      SetLength(Clump, Length(Clump)+1);
      Level := 0;
    end;

    case InHeader.Tag of
      rwATOMIC:
      begin
        Inc(Clump[High(Clump)].AtomicCount);
        SetLength(Clump[High(Clump)].Atomic, Clump[High(Clump)].AtomicCount);
        FillChar(Clump[High(Clump)].Atomic[High(Clump[High(Clump)].Atomic)], SizeOf(TDFFAtomic), 0)
      end;
      rwGEOMETRY:
      begin
        Inc(Clump[High(Clump)].GeometryList.GeometryCount);
        SetLength(Clump[High(Clump)].GeometryList.Geometry, Clump[High(Clump)].GeometryList.GeometryCount);
        FillChar(Clump[High(Clump)].GeometryList.Geometry[High(Clump[High(Clump)].GeometryList.Geometry)], SizeOf(TDFFGeometry), 0);

      end;
      rwMATERIAL:
      begin
        Inc(Clump[High(Clump)].GeometryList.Geometry[Clump[High(Clump)].GeometryList.GeometryCount - 1].MaterialList.MaterialCount);
        SetLength(Clump[High(Clump)].GeometryList.Geometry[Clump[High(Clump)].GeometryList.GeometryCount - 1].MaterialList.Material, Clump[High(Clump)].GeometryList.Geometry[Clump[High(Clump)].GeometryList.GeometryCount - 1].MaterialList.MaterialCount);
        // read speca?

//        showmessage('found material extension at ' + inttostr(stream.position));
      end;
      col3: begin
      pre:= stream.position;
//      stream.read(Clump[High(Clump)].col3, 120); // just header

// read spheres
//      setlength(Clump[High(Clump)].col3.dspheres, Clump[High(Clump)].col3.Spherec);
//      stream.Position:= ParseHeader.Start + Clump[High(Clump)].col3.OFSspheres;
//      stream.read(Clump[High(Clump)].col3.dspheres, Clump[High(Clump)].col3.Spherec * 20);

//      setlength(Clump[High(Clump)].col3.ColFaces, Clump[High(Clump)].col3.ColFacec);
//      setlength(Clump[High(Clump)].col3.ColVerts, Clump[High(Clump)].col3.OFSShadowFace - Clump[High(Clump)].col3.OFSShadowvert div 6);


//      setlength(Clump[High(Clump)].col3.ColShadeFaces, Clump[High(Clump)].col3.ShadowFacec);
//      setlength(Clump[High(Clump)].col3.ColShadeVerts, Clump[High(Clump)].col3.OFSShadowFace - Clump[High(Clump)].col3.OFSShadowvert div 6);

//      stream.seek(inheader.size - 72, sofromcurrent); // skip data
        stream.position:= pre;
      end;

      rnmultitexturespeca:
      begin
        pre:= stream.position;
        stream.seek(4, sofromcurrent); // skip 4 bytes
        stream.read(

        Clump[High(Clump)].GeometryList.Geometry[Clump[High(Clump)].GeometryList.GeometryCount - 1].MaterialList.Material[
        Clump[High(Clump)].GeometryList.Geometry[Clump[High(Clump)].GeometryList.GeometryCount - 1].MaterialList.MaterialCount - 1
        ].Texture.speca

        , InHeader.Size - 4);
        stream.position:= pre;
      end;
      rwMATERIALEFFECTS:
      begin

        pre:= stream.position;
        stream.seek(4, sofromcurrent); // skip 4 bytes

        fillchar(Clump[High(Clump)].GeometryList.Geometry[Clump[High(Clump)].GeometryList.GeometryCount - 1].MaterialList.Material[
        Clump[High(Clump)].GeometryList.Geometry[Clump[High(Clump)].GeometryList.GeometryCount - 1].MaterialList.MaterialCount - 1
        ].Texture.matplugin, inheader.Size, 0);

        Clump[High(Clump)].GeometryList.Geometry[Clump[High(Clump)].GeometryList.GeometryCount - 1].MaterialList.Material[
        Clump[High(Clump)].GeometryList.Geometry[Clump[High(Clump)].GeometryList.GeometryCount - 1].MaterialList.MaterialCount - 1
        ].Texture.matpluginsize:= InHeader.Size;

        stream.read(
        Clump[High(Clump)].GeometryList.Geometry[Clump[High(Clump)].GeometryList.GeometryCount - 1].MaterialList.Material[
        Clump[High(Clump)].GeometryList.Geometry[Clump[High(Clump)].GeometryList.GeometryCount - 1].MaterialList.MaterialCount - 1
        ].Texture.matplugin
        , InHeader.Size);

        stream.position:= pre;
      end;
    end;

    case InHeader.Tag of
      rwTEXTURE,
      rwMATERIALLIST,
      rwMATERIAL,
      rwCLUMP,
      rwFRAMELIST,
      rwGEOMETRYLIST,
      rwGEOMETRY,
      rwATOMIC:
        ParseHeaders(Stream, InHeader, Level + 1, InHeader.Tag);
      rwMATERIALSPLIT:
        ParseMaterialSplit(Stream, InHeader, Parent);
      rwDATA: begin
//        // outputdebugstring(pchar(inttostr(InHeader.Start)));
        try ParseData(Stream, InHeader, Parent); except end;
        end;
      rwEXTENSION:
        if (InHeader.Size > 0) then
          ParseHeaders(Stream, InHeader, Level + 1, Parent);
      rwFRAME,
      rwSTRING:
        ParseString(Stream, InHeader, Level + 1, Parent);
//      rwAnimPlugin:
    end;

    Stream.Seek(InHeader.Back + InHeader.Size, soFromBeginning);

    if (Stream.Position >= (ParseHeader.Back + ParseHeader.Size)) or (InHeader.Tag = 0) then
      MoreData := False;

  end;
end;

function TDffLoader.GetNextHeader(Stream: TStream; Level, Parent: LongInt): TDFFHeader;
var
  OutHeader: TDFFHeader;
begin
  with OutHeader do
  begin
    Start := Stream.position;

    Stream.Read(Tag, 4);
    Stream.Read(Size, 4);
    Stream.Read(renderversion, 4); //(Data, 2);
//    Stream.Read(Version, 2);
    Back := Stream.position;
  end;
  Result := OutHeader
end;

// gtadff

constructor TGTADff.Create;
begin
  inherited Create;
end;

constructor TGTADff.Create(st: TStream; in_name: String; in_start, in_size: Int64);
begin
  inherited Create;
  Name := ChangeFileExt(in_name, '');
  Loaded := False;
  Stream := St;
  FStart := in_start;
  FSize := in_size;
  InUse := False;
  if not GTA_MODEL_LOAD_DEMAND then
    LoadFromStream;
end;

procedure TGTADff.LoadFromStream;
begin
  Loaded := True;
  LoadFromStream(Stream, FStart, FSize);
end;

procedure TGTADff.Unload;
begin
  Loaded := False;
  ResetClump;
end;

destructor TGTADff.Destroy;
begin
  inherited Destroy;
  ResetClump;
end;

procedure TGTADff.glDraw(in_texture: LongInt);
var
  I: LongWord;
begin
  if not Loaded then
    Exit;
    
  if not (Clump = nil) then
    if (Clump[0].FrameList.Data.FrameCount > 0) then for I := 0 to Clump[0].FrameList.Data.FrameCount - 1 do
      if (Clump[0].FrameList.Data.Frame[I].Parent = -1) then
        glDrawRecurse(0, I, in_texture, True);
end;

procedure TGTADff.glDrawRecurse(in_clump: LongWord; in_frame: LongInt; in_texture: LongInt; TheParent: Boolean);
var
  I: Integer;
  Gn, OnC: Longint;
  N: array[0..15] of Single;
  UV, Alp: Boolean;
  Normals: Byte;
begin
  glPushMatrix;

  glColor4f(1.0, 1.0, 1.0, 1.0);

  Gn := -1;
  if (Clump[in_clump].AtomicCount > 0) then for I := 0 to Clump[in_clump].AtomicCount - 1 do
  begin
    if Clump[in_clump].Atomic[i].Data.FrameNum = LongWord(in_frame) then
      Gn := Clump[in_clump].Atomic[i].Data.GeometryNum;
  end;

{

delfi: why bother with matrices?

  if not TheParent then with Clump[in_clump].FrameList.Data.Frame[in_frame] do
  begin
    // rotate offset
    N[0] := Matrix[0][0]; N[4] := Matrix[1][0]; N[8]  := Matrix[2][0]; N[12] := Coord[0];
    N[1] := Matrix[0][1]; N[5] := Matrix[1][1]; N[9]  := Matrix[2][1]; N[13] := Coord[1];
    N[2] := Matrix[0][2]; N[6] := Matrix[1][2]; N[10] := Matrix[2][2]; N[14] := Coord[2];
    N[3] := 0;            N[7] := 0;            N[11] := 0;            N[15] := 1;
    glMultMatrixf(@N);
  end;
}

{

Delfi: i don't know what is this for, but it nakes no sense at all!

  if (Clump[in_clump].FrameList.Data.FrameCount = 0) and (Clump[in_clump].GeometryList.GeometryCount > 0) then
  begin
    with Clump[in_clump].GeometryList.Geometry[0] do
    begin
      if Main.GTA_TEXTURE_MODE then
        SetTexture('', -1);
      if (Length(Data.Normal) = Length(Data.Face)) then
        Normals := 1
      else
        Normals := 0;
      glBegin(GL_TRIANGLES);
      if (Length(Data.Face) > 0) then for Onc := 0 to High(Data.Face) do
      begin
        if (Normals = 1) then glNormal3fv(@Data.Normal[Onc]);
        glVertex3fv(@Data.Vertex[Data.Face[Onc].V1]);
        glVertex3fv(@Data.Vertex[Data.Face[Onc].V2]);
        glVertex3fv(@Data.Vertex[Data.Face[Onc].V3]);
      end;
      glEnd;
    end;
  end else }if (Gn < LongInt(Clump[in_clump].FrameList.Data.FrameCount)) and not (Gn = -1) then
  begin

    // draw all frames
    if True then
    begin

      // draw object in local coordinate system
      with Clump[in_clump].GeometryList.Geometry[Gn] do
      begin

        UV := Length(Data.UVmaps) > 0;
        for i := 0 to MaterialSplit.Header.SplitCount -1 do
        begin
{          if not (Length(MaterialSplit.Split[i].Normal) = 0) then
          begin
            if (MaterialSplit.Header.Data = 0) then
              Normals := 1
            else
              Normals := 2;
          end else
            Normals := 0; }

          // do texture
          if Main.GTA_TEXTURE_MODE then
            SetTexture(MaterialList.Material[MaterialSplit.Split[i].MaterialIndex].Texture.Name, in_texture);

          Alp := False;
          if Assigned(glActiveTextureARB) and Assigned(glClientActiveTextureARB) and not (Trim(MaterialList.Material[MaterialSplit.Split[i].MaterialIndex].Texture.Desc) = '') then
          begin
            Alp := True;

            glActiveTextureARB(GL_TEXTURE1_ARB);
            glClientActiveTextureARB(GL_TEXTURE1_ARB);

            glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
            glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_EXT, GL_PREVIOUS_EXT);
            glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_EXT, GL_SRC_COLOR);
            glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB_EXT, GL_TEXTURE);
            glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_RGB_EXT, GL_SRC_ALPHA);

            SetAlpha(MaterialList.Material[MaterialSplit.Split[i].MaterialIndex].Texture.Desc, in_texture);
          end;

          // draw it
          if true then //GTA_DISPLAY_LISTS then
          begin
            if (MaterialSplit.Header.TriagleFormat = 0) then
              glBegin(GL_TRIANGLES)
            else
              glBegin(GL_TRIANGLE_STRIP);
            for onc := 0 to High(MaterialSplit.Split[i].Index) do
            begin

              if UV then glTexCoord2fv(@Data.UVmaps[MaterialSplit.Split[i].Index[onc]]);
//              if UV then glTexCoord2f(Data.UVmaps[MaterialSplit.Split[i].Index[onc]][0], Data.UVmaps[MaterialSplit.Split[i].Index[onc]][1]);

// NO NORMALS - COMMENTED OUT BY DELFI - because they are not used for map model rendering and this saves a bit calls
//              if (Normals = 1) and (Onc mod 3 = 0) then glNormal3fv(@MaterialSplit.Split[i].Normal[Onc div 3]);
//              if (Normals = 2) then glNormal3fv(@MaterialSplit.Split[i].Normal[Onc]);

//              Delfi's debuging :D
//              MessageBox(0, pchar(format('%d %d', [high(Data.Color), High(MaterialSplit.Split[i].Index)])), 'cap', mb_ok);

{
leave this commented-out ok?
              try
              if high(Data.Color) > 0 then // if prelightning data is present
              glColor3ubv(@Data.Color[MaterialSplit.Split[i].Index[onc]]);
              glColor3ubv(@Data.Color[0]);
              except end;}

              glVertex3fv(@Data.Vertex[MaterialSplit.Split[i].Index[onc]]);

            end;
            glEnd;
          end else
          begin
            glEnableClientState(GL_VERTEX_ARRAY);
            glVertexPointer(3, GL_FLOAT, 0, @Data.Vertex[0]);
            if not (Normals = 0) then
            begin
              glEnableClientState(GL_NORMAL_ARRAY);
              glNormalPointer(GL_FLOAT, 0, @MaterialSplit.Split[i].Normal[0]);
            end else
              glDisableClientState(GL_NORMAL_ARRAY);
            glDisableClientState(GL_COLOR_ARRAY);
            //if UV then
            //begin
            //  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
            //  glTexCoordPointer(2, GL_FLOAT, 0, @Data.UV[0]);
            //end else
              glDisableClientState(GL_TEXTURE_COORD_ARRAY);

            if (MaterialSplit.Header.Data = 0) then
              glDrawElements(GL_TRIANGLES, High(MaterialSplit.Split[i].Index) + 1, GL_UNSIGNED_INT, @MaterialSplit.Split[i].Index[0])
            else
              glDrawElements(GL_TRIANGLE_STRIP, High(MaterialSplit.Split[i].Index) + 1, GL_UNSIGNED_INT, @MaterialSplit.Split[i].Index[0]);
          end;

          if Alp then
          begin
            glActiveTextureARB(GL_TEXTURE0_ARB);
            glClientActiveTextureARB(GL_TEXTURE0_ARB);
          end;
        end;
      end;

    end;

  end;

  // Draw all frames that has the current frame as parent..
  if (Clump[in_clump].FrameList.Data.FrameCount > 0) then for Onc := 0 to Clump[in_clump].FrameList.Data.FrameCount - 1 do
  begin
    if (Clump[in_clump].FrameList.Data.Frame[Onc].Parent = in_frame) then
    begin
      I := Length(Clump[in_clump].FrameList.Data.Frame[Onc].Name);
      if (I >= 3) then
      begin
        if (Clump[in_clump].FrameList.Data.Frame[Onc].Name[I - 2] = '_') and ((Clump[in_clump].FrameList.Data.Frame[Onc].Name[I - 1] = 'L') or (Clump[in_clump].FrameList.Data.Frame[Onc].Name[I - 1] = 'l')) then
        begin
          if (StrToIntDef(Clump[in_clump].FrameList.Data.Frame[Onc].Name[I], -1) <= 0) then
            glDrawRecurse(in_clump, OnC, in_texture, True);
        end else if not ((Clump[in_clump].FrameList.Data.Frame[Onc].Name[1] = 'C') and (Clump[in_clump].FrameList.Data.Frame[Onc].Name[2] = 'o') and (Clump[in_clump].FrameList.Data.Frame[Onc].Name[3] = 'l')) then
          glDrawRecurse(in_clump, OnC, in_texture, False);
      end else
        glDrawRecurse(in_clump, OnC, in_texture, False);
    end;
  end;

  // now pop the matrix, so we don't affect siblings
  glPopMatrix;
end;

function TGTADff.SetTexture(Name: string; in_texture: LongInt): Boolean;
var
  Index: LongInt;
begin
  Result := False;
  if not (Name = '') and not (in_texture = -1) then
  begin
    if not Main.GArchive.GTxd[in_texture].Loaded then
      Main.GArchive.GTxd[in_texture].LoadFromStream;

    if Main.GArchive.GTxd[in_texture].ImageNameList.Find(Name, Index) then
    begin
      glBindTexture(GL_TEXTURE_2D, Main.GArchive.GTxd[in_texture].ImageTexture[LongWord(Main.GArchive.GTxd[in_texture].ImageNameList.Objects[Index])]);
      Result := True;
    end;
  end;
  if not Result then begin
    glBindTexture(GL_TEXTURE_2D, 0);
    if name <> '' then
//    report(format('missing texture "%s" in txd: "%s.txd" requested by model "%s"', [Name, Main.GArchive.GTxd[in_texture].Name, Self.Name]))
    end;
end;

function TGTADff.SetAlpha(Alpha: string; in_texture: LongInt): Boolean;
var
  Index: LongInt;
begin
  Result := False;
  if not (Alpha = '') and not (in_texture = -1) then
  begin
    if not Main.GArchive.GTxd[in_texture].Loaded then
      Main.GArchive.GTxd[in_texture].LoadFromStream;

    if Main.GArchive.GTxd[in_texture].ImageAlphaList.Find(Alpha, Index) then
    begin
      glBindTexture(GL_TEXTURE_2D, Main.GArchive.GTxd[in_texture].ImageTexture[LongWord(Main.GArchive.GTxd[in_texture].ImageAlphaList.Objects[Index])]);
      Result := True;
    end;
  end;
  if not Result then
    glBindTexture(GL_TEXTURE_2D, 0);
end;

function TDffLoader.Normalise(Vect: TVector3f): TVector3f;
var
  Len: Single;
  I: LongWord;
begin
  Len := Sqrt(Vect[0] * Vect[0] +
              Vect[1] * Vect[1] +
              Vect[2] * Vect[2]);

  if Len = 0 then
    Len := 1;

  for I := 0 to 2 do
    Result[I] := Vect[I] / Len;
end;

function TDffLoader.CalcFaceNormal(P1, P2, P3: TVector3f): TVector3f;
var
  a, b: TVector3f;
begin
   a[0]:=p2[0]-p1[0]; a[1]:=p2[1]-p1[1]; a[2]:=p2[2]-p1[2];
   b[0]:=p3[0]-p1[0]; b[1]:=p3[1]-p1[1]; b[2]:=p3[2]-p1[2];

   result[0]:=a[1]*b[2]-a[2]*b[1];
   result[1]:=a[2]*b[0]-a[0]*b[2];
   result[2]:=a[0]*b[1]-a[1]*b[0];

   Result := Normalise(Result);
end;

end.
