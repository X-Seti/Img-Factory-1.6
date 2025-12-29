unit GTADff;

interface

uses OpenGl, Windows, Classes, SysUtils, StrUtils, RequiredTypes;

const
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
  rwFRAME         =  39056126;
  rwMATERIALSPLIT =  1294;

  rwOBJECT_VERTEX_UV     = 4;
  rwOBJECT_VERTEX_COLOR  = 8;
  rwOBJECT_VERTEX_NORMAL = 16;

  MOVE_AMOUNT = 3;
  ROT_AMOUNT = 3;

type
  // All New Structure

  TDFFFace = record
    V2: Word;
    V1: Word;
    Extra: Word;
    V3: Word;
  end;

  TDFFUV = record
    U, V: Single;
  end;

  TDFFFrame = record
    Name: String;
    Matrix: TMatrix3f;
    Coord: TVector3f;
    Parent: LongInt;
    Other1, Other2: Word;
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
    Flags1: Word;
    Flags2: Word;

    TriangleCount: LongWord;
    VertexCount: LongWord;
    OtherCount: LongWord;
  end;
                
  TDFFLightHeaderDataGeometry = record
    Ambient: Single;
    Diffuse: Single;
    Specular: Single;
  end;

  TDFFExtraDataGeometry = record
    U1, U2, U3, U4: Single;
    Other1, Other2: LongWord; //D: 1
  end;

  TDFFDataGeometry = record
    Header: TDFFHeaderDataGeometry;

    LightHeader: TDFFLightHeaderDataGeometry;

    Color: packed array of LongWord;

    UV: packed array of TVector2f;

    Face: packed array of TDFFFace;

    Extra: TDFFExtraDataGeometry;

    Vertex: packed array of TVector3f;

    Normal: packed array of TVector3f;
  end;

  TDFFDataMaterialList = record
    MaterialCount: LongWord;
    Other: LongWord; //D: FF
  end;

  TDFFColor = array[0..4] of Byte;
  TDFFDataMaterial = record
    Other1: LongWord; //D: 0
    Color: TDFFColor;
    Other3: LongWord;
    TextureCount: LongWord; //D: 1

    Other5: Single; //D: 1.0
    Other6: Single;
    Other7: Single; //D: 1.0
  end;

  TDFFDataTexture = record
  end;

  // level 5

  TDFFTexture = record
    Data: TDFFDataTexture;
    Name: String;
    Alpha: String;
    GotName: Boolean;
  end;

  // level 4

  TDFFMaterial = record
    Data: TDFFDataMaterial;
    Texture: TDFFTexture;
  end;

  // level 3

  TDFFHeaderMaterialSplit = record
    Data: LongWord;
    SplitCount: LongWord;
    FaceCount: LongWord;
  end;

  TDFFSplit = record
    FaceIndex: LongWord;
    MaterialIndex: LongWord;

    Index: packed array of LongWord;
    Normal: packed array of TVector3f;
  end;

  TDFFMaterialSplit = record
    Header: TDFFHeaderMaterialSplit;
    Split: array of TDFFSplit;
  end;

  TDFFMaterialList =  record
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
  end;

  // level 0

  TDFFClump = record
    Data: TDFFDataClump;
    FrameList: TDFFFrameList;
    GeometryList: TDFFGeometryList;
    Atomic: array of TDFFAtomic;
    AtomicCount: Word;
  end;

  // header

  TDFFHeader = record
    Start: LongWord;
    Back: LongWord;

    Tag: LongWord;
    Size: LongWord;
    Data1: Word; //D: 784
    Data2: Word; //D: 784
  end;

  TDffLoader = class
  private
    function GetNextHeader(Stream: TStream; Level, Parent: LongInt): TDFFHeader;
    procedure ParseData(Stream: TStream; ParseHeader: TDFFHeader; Parent: LongInt);
    procedure ParseMaterialSplit(Stream: TStream; ParseHeader: TDFFHeader; Parent: LongInt);
    procedure ParseHeaders(Stream: TStream; ParseHeader: TDFFHeader; Level, Parent: LongInt);
    procedure ParseString(Stream: TStream; ParseHeader: TDFFHeader; Level, Parent: LongInt);
    procedure ResetClump;

    function Normalise(Vect: TVector3f): TVector3f;
    function CalcFaceNormal(P1, P2, P3: TVector3f): TVector3f;

  public
    Clump: array of TDFFClump;
    FrameUpTo: LongInt;
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

procedure TDffLoader.ResetClump;
var
  I, J, K: LongWord;
begin
  if (High(Clump) > 0) then for I := 0 to High(Clump) do
  begin
    SetLength(Clump[I].Atomic, 0);
    if (High(Clump[I].GeometryList.Geometry) > 0) then for J := 0 to High(Clump[I].GeometryList.Geometry) do
    begin
      with Clump[I].GeometryList.Geometry[J].Data do
      begin
        SetLength(Color, 0);
        SetLength(UV, 0);
        SetLength(Face, 0);
        SetLength(Vertex, 0);
        SetLength(Normal, 0);
      end;
      SetLength(Clump[I].GeometryList.Geometry[J].MaterialList.Material, 0);
      if (High(Clump[I].GeometryList.Geometry[J].MaterialSplit.Split) > 0) then for K := 0 to High(Clump[I].GeometryList.Geometry[J].MaterialSplit.Split) do
      begin
        with Clump[I].GeometryList.Geometry[J].MaterialSplit.Split[K] do
        begin
          SetLength(Index, 0);
          SetLength(Normal, 0);
        end;
      end;
      SetLength(Clump[I].GeometryList.Geometry[J].MaterialSplit.Split, 0);
    end;
    SetLength(Clump[I].GeometryList.Geometry, 0);
    SetLength(Clump[I].FrameList.Data.Frame, 0);
  end;
  SetLength(Clump, 0);
end;

procedure TDffLoader.LoadFromStream(Stream: TStream);
var
  MainHeader: TDFFHeader;
begin
  ResetClump;

  MainHeader.Start := 16;
  MainHeader.Tag := 0;
  MainHeader.Size := Stream.Size;
  MainHeader.Data1 := 0;
  MainHeader.Data2 := 0;
  MainHeader.Back := 0;

  ParseHeaders(Stream, MainHeader, 0, 16);
end;

procedure TDffLoader.LoadFromStream(st: TStream; in_start, in_size: Int64);
var
  MainHeader: TDFFHeader;
begin
  ResetClump;
  
  MainHeader.Start := in_start + 16;
  MainHeader.Tag := 0;
  MainHeader.Size := in_size;
  MainHeader.Data1 := 0;
  MainHeader.Data2 := 0;
  MainHeader.Back := in_start;

  st.Seek(in_start, soFromBeginning);
  ParseHeaders(st, MainHeader, 0, 16);
end;


procedure TDffLoader.LoadFromFile(FileName: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead + fmShareDenyNone);
  LoadFromStream(Stream);
  Stream.Free;
end;

procedure TDffLoader.ParseMaterialSplit(Stream: TStream; ParseHeader: TDFFHeader; Parent: LongInt);
var
  I: LongInt;
  J: LongWord;
begin
  with Clump[High(Clump)].GeometryList.Geometry[Clump[High(Clump)].GeometryList.GeometryCount - 1] do
  begin
    Stream.ReadBuffer(MaterialSplit.Header, SizeOf(MaterialSplit.Header));

    SetLength(MaterialSplit.Split, MaterialSplit.Header.SplitCount);

    for I := 0 to MaterialSplit.Header.SplitCount - 1 do
    begin
      Stream.ReadBuffer(MaterialSplit.Split[I].FaceIndex, 4);
      Stream.ReadBuffer(MaterialSplit.Split[I].MaterialIndex, 4);

      SetLength(MaterialSplit.Split[I].Index, MaterialSplit.Split[I].FaceIndex);
      Stream.ReadBuffer(MaterialSplit.Split[I].Index[0], 4 * (MaterialSplit.Split[I].FaceIndex));

      if GTA_NORMALS_MODE then
      begin
        if (MaterialSplit.Header.Data = 0) then
        begin
          if true then //GTA_DISPLAY_LISTS then
          begin
            SetLength(MaterialSplit.Split[I].Normal, (MaterialSplit.Split[I].FaceIndex div 3));
            J := 0; while J < MaterialSplit.Split[I].FaceIndex - 2 do
            begin
              MaterialSplit.Split[I].Normal[J div 3] := CalcFaceNormal(Data.Vertex[MaterialSplit.Split[I].Index[J + 0]],
                                                                 Data.Vertex[MaterialSplit.Split[I].Index[J + 1]],
                                                                 Data.Vertex[MaterialSplit.Split[I].Index[J + 2]]);
              Inc(J, 3);
            end;
          end else
          begin
            SetLength(MaterialSplit.Split[I].Normal, Data.Header.VertexCount);
            J := 0; while J < MaterialSplit.Split[I].FaceIndex - 2 do
            begin
              MaterialSplit.Split[I].Normal[MaterialSplit.Split[I].Index[J]] := CalcFaceNormal(Data.Vertex[MaterialSplit.Split[I].Index[J + 0]],
                                                                 Data.Vertex[MaterialSplit.Split[I].Index[J + 1]],
                                                                 Data.Vertex[MaterialSplit.Split[I].Index[J + 2]]);
              MaterialSplit.Split[I].Normal[MaterialSplit.Split[I].Index[J + 1]] := MaterialSplit.Split[I].Normal[MaterialSplit.Split[I].Index[J]];
              MaterialSplit.Split[I].Normal[MaterialSplit.Split[I].Index[J + 2]] := MaterialSplit.Split[I].Normal[MaterialSplit.Split[I].Index[J]];
              Inc(J, 3);
            end;
          end;
        end else
        begin
          if true then //GTA_DISPLAY_LISTS then
          begin
            SetLength(MaterialSplit.Split[I].Normal, MaterialSplit.Split[I].FaceIndex);
            J := 0; while J < MaterialSplit.Split[I].FaceIndex - 2 do
            begin
              MaterialSplit.Split[I].Normal[J + 2] := CalcFaceNormal(Data.Vertex[MaterialSplit.Split[I].Index[J + 0]],
                                                                     Data.Vertex[MaterialSplit.Split[I].Index[J + 1 + (J mod 2)]],
                                                                     Data.Vertex[MaterialSplit.Split[I].Index[J + 2 - (J mod 2)]]);
              Inc(J);
            end;
            if (MaterialSplit.Split[I].FaceIndex >=3) then
            begin
              MaterialSplit.Split[I].Normal[0] := MaterialSplit.Split[I].Normal[2];
              MaterialSplit.Split[I].Normal[1] := MaterialSplit.Split[I].Normal[2];
            end;
          end else
          begin
            SetLength(MaterialSplit.Split[I].Normal, Data.Header.VertexCount);
            J := 0; while J < MaterialSplit.Split[I].FaceIndex - 2 do
            begin
              MaterialSplit.Split[I].Normal[MaterialSplit.Split[I].Index[J + 2]] := CalcFaceNormal(Data.Vertex[MaterialSplit.Split[I].Index[J + 0]],
                                                                     Data.Vertex[MaterialSplit.Split[I].Index[J + 1 + (J mod 2)]],
                                                                     Data.Vertex[MaterialSplit.Split[I].Index[J + 2 - (J mod 2)]]);
              Inc(J);
            end;
            if (MaterialSplit.Split[I].FaceIndex >= 3) then
            begin
              MaterialSplit.Split[I].Normal[MaterialSplit.Split[I].Index[0]] := MaterialSplit.Split[I].Normal[MaterialSplit.Split[I].Index[2]];
              MaterialSplit.Split[I].Normal[MaterialSplit.Split[I].Index[1]] := MaterialSplit.Split[I].Normal[MaterialSplit.Split[I].Index[2]];
            end;
          end;
        end;
      end else
        SetLength(MaterialSplit.Split[I].Normal, 0);
    end;

  end;
end;

procedure TDffLoader.ParseData(Stream: TStream; ParseHeader: TDFFHeader; Parent: LongInt);
var
  I, J: LongWord;
begin
  case Parent of

  rwCLUMP:
    begin
      Stream.ReadBuffer(Clump[High(Clump)].Data.ObjectCount, 4);
    end;

  rwMATERIALLIST:
    begin
      with Clump[High(Clump)].GeometryList.Geometry[Clump[High(Clump)].GeometryList.GeometryCount - 1].MaterialList.Data do
      begin
        Stream.ReadBuffer(MaterialCount, 4);
        Stream.ReadBuffer(Other, 4);
      end;
    end;

  rwMATERIAL:
    begin
      with Clump[High(Clump)].GeometryList.Geometry[Clump[High(Clump)].GeometryList.GeometryCount - 1].MaterialList.Material[Clump[High(Clump)].GeometryList.Geometry[Clump[High(Clump)].GeometryList.GeometryCount - 1].MaterialList.MaterialCount - 1] do
      begin
        Texture.GotName := False;
        Stream.ReadBuffer(Data, SizeOf(Data));
      end;
    end;

  rwGEOMETRYLIST:
    begin
      Stream.ReadBuffer(Clump[High(Clump)].GeometryList.Data.GeometryCount, 4);
    end;

  rwATOMIC:
    begin
      with Clump[High(Clump)].Atomic[Clump[High(Clump)].AtomicCount - 1].Data do
      begin
        Stream.ReadBuffer(FrameNum, 4);
        Stream.ReadBuffer(GeometryNum, 4);
        Stream.ReadBuffer(Other1, 4);
        Stream.ReadBuffer(Other2, 4);
      end;
    end;

  rwFRAMELIST:
    begin
      with Clump[High(Clump)].FrameList.Data do
      begin
        Stream.ReadBuffer(FrameCount, 4);
        SetLength(Frame, FrameCount);
        FrameUpTo := 0;

        for I := 0 to FrameCount - 1 do
        begin
          for J := 0 to 2 do
          begin
            Stream.ReadBuffer(Frame[I].Matrix[J], 12);
          end;
          begin
            Stream.ReadBuffer(Frame[I].Coord, 12);
          end;
          Stream.ReadBuffer(Frame[I].Parent, 4);
          Stream.ReadBuffer(Frame[I].Other1, 2);
          Stream.ReadBuffer(Frame[I].Other2, 2);
        end;
      end;
    end;

  rwGEOMETRY:
    begin
      with Clump[High(Clump)].GeometryList.Geometry[Clump[High(Clump)].GeometryList.GeometryCount - 1].Data do
      begin

        Stream.ReadBuffer(Header, SizeOf(Header));

        if not (ParseHeader.Data2 = 4099) then
          Stream.ReadBuffer(LightHeader, SizeOf(LightHeader))
        else
          FillChar(LightHeader, SizeOf(LightHeader), 0);

        if (rwOBJECT_VERTEX_COLOR and Header.Flags1) = rwOBJECT_VERTEX_COLOR then
        begin
          SetLength(Color, Header.VertexCount);
          Stream.ReadBuffer(Pointer(Color)^, 4 * Header.VertexCount);
        end else
          SetLength(Color, 0);

        if (rwOBJECT_VERTEX_UV and Header.Flags1) = rwOBJECT_VERTEX_UV then
        begin
          SetLength(UV, Header.VertexCount);
          Stream.ReadBuffer(UV[0], 8 * Header.VertexCount);
        end else
          SetLength(UV, 0);

        SetLength(Face, Header.TriangleCount);
        Stream.ReadBuffer(Pointer(Face)^, 8 * Header.TriangleCount);

        Stream.ReadBuffer(Extra, SizeOf(Extra));

        SetLength(Vertex, Header.VertexCount);
        Stream.ReadBuffer(Pointer(Vertex)^, 12 * Header.VertexCount);

        if (rwOBJECT_VERTEX_NORMAL and Header.Flags1) = rwOBJECT_VERTEX_NORMAL then
        begin
          SetLength(Normal, Header.VertexCount);
          Stream.ReadBuffer(Pointer(Normal)^, 12 * Header.VertexCount);
        end else if (Header.VertexCount > 0) and GTA_NORMALS_MODE and (Header.TriangleCount mod 3 = 0) then
        begin;
          SetLength(Normal, Header.TriangleCount);
          for I := 0 to Header.TriangleCount - 1 do
            Normal[I] := CalcFaceNormal(Vertex[Face[I].V1], Vertex[Face[I].V2], Vertex[Face[I].V3]);
        end else
          SetLength(Normal, 0);

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
  Stream.ReadBuffer(Pointer(Buf)^, ParseHeader.Size);

  case Parent of

  rwTEXTURE:
    begin
      with Clump[High(Clump)].GeometryList.Geometry[Clump[High(Clump)].GeometryList.GeometryCount - 1].MaterialList.Material[Clump[High(Clump)].GeometryList.Geometry[Clump[High(Clump)].GeometryList.GeometryCount - 1].MaterialList.MaterialCount - 1].Texture do
      begin
        if GotName then
          Alpha := Trim(Buf)
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
      rwDATA:
        ParseData(Stream, InHeader, Parent);
      rwEXTENSION:
        if (InHeader.Size > 0) then
          ParseHeaders(Stream, InHeader, Level + 1, Parent);
      rwFRAME,
      rwSTRING:
        ParseString(Stream, InHeader, Level + 1, Parent);
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

    Stream.ReadBuffer(Tag, 4);
    Stream.ReadBuffer(Size, 4);
    Stream.ReadBuffer(Data1, 2);
    Stream.ReadBuffer(Data2, 2);
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

  if not TheParent then with Clump[in_clump].FrameList.Data.Frame[in_frame] do
  begin
    // rotate offset
    N[0] := Matrix[0][0]; N[4] := Matrix[1][0]; N[8]  := Matrix[2][0]; N[12] := Coord[0];
    N[1] := Matrix[0][1]; N[5] := Matrix[1][1]; N[9]  := Matrix[2][1]; N[13] := Coord[1];
    N[2] := Matrix[0][2]; N[6] := Matrix[1][2]; N[10] := Matrix[2][2]; N[14] := Coord[2];
    N[3] := 0;            N[7] := 0;            N[11] := 0;            N[15] := 1;
    glMultMatrixf(@N);
  end;

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
  end else if (Gn < LongInt(Clump[in_clump].FrameList.Data.FrameCount)) and not (Gn = -1) then
  begin

    // draw all frames
    if True then
    begin

      // draw object in local coordinate system
      with Clump[in_clump].GeometryList.Geometry[Gn] do
      begin

        UV := Length(Data.UV) > 0;
        for i := 0 to MaterialSplit.Header.SplitCount -1 do
        begin
          if not (Length(MaterialSplit.Split[i].Normal) = 0) then
          begin
            if (MaterialSplit.Header.Data = 0) then
              Normals := 1
            else
              Normals := 2;
          end else
            Normals := 0;

          // do texture
          if Main.GTA_TEXTURE_MODE then
            SetTexture(MaterialList.Material[MaterialSplit.Split[i].MaterialIndex].Texture.Name, in_texture);

          Alp := False;
          if Assigned(glActiveTextureARB) and Assigned(glClientActiveTextureARB) and not (Trim(MaterialList.Material[MaterialSplit.Split[i].MaterialIndex].Texture.Alpha) = '') then
          begin
            Alp := True;

            glActiveTextureARB(GL_TEXTURE1_ARB);
            glClientActiveTextureARB(GL_TEXTURE1_ARB);

            glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
            glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_EXT, GL_PREVIOUS_EXT);
            glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_EXT, GL_SRC_COLOR);
            glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB_EXT, GL_TEXTURE);
            glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_RGB_EXT, GL_SRC_ALPHA);

            SetAlpha(MaterialList.Material[MaterialSplit.Split[i].MaterialIndex].Texture.Alpha, in_texture);
          end;

          // draw it
          if true then //GTA_DISPLAY_LISTS then
          begin
            if (MaterialSplit.Header.Data = 0) then
              glBegin(GL_TRIANGLES)
            else
              glBegin(GL_TRIANGLE_STRIP);
            for onc := 0 to High(MaterialSplit.Split[i].Index) do
            begin
              if UV then glTexCoord2f(Data.UV[MaterialSplit.Split[i].Index[onc]][0], Data.UV[MaterialSplit.Split[i].Index[onc]][1]);
              if (Normals = 1) and (Onc mod 3 = 0) then glNormal3fv(@MaterialSplit.Split[i].Normal[Onc div 3]);
              if (Normals = 2) then glNormal3fv(@MaterialSplit.Split[i].Normal[Onc]);
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
  if not Result then
    glBindTexture(GL_TEXTURE_2D, 0);
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
