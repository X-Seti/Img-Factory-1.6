unit GTAText;

interface

uses Windows, OpenGl, Dialogs, Classes, StrUtils, SysUtils, RequiredTypes; // geometry

const
  FILE_IPL = 0;
  FILE_IDE = 1;

  VICE_DAT_FILE = 'data\gta_vc.dat';
  GTA3_DAT_FILE = 'data\gta3.dat';

  PATH_COUNT = 12;
  PATH_IPL_COUNT = 12;

  EFFECT_LIGHT = 0;
  EFFECT_PARTICLE = 1;
  EFFECT_UNKNOWN = 2;
  EFFECT_ANIMATION = 3;
  EFFECT_REFLECTION = 4;

  OBJECT_FLAGS_WET = 1;
  OBJECT_FLAGS_UNKNOWN2 = 2;
  OBJECT_FLAGS_ALPHA = 4;
  OBJECT_FLAGS_UNKNOWN8 = 8;
  OBJECT_FLAGS_UNKNOWN16 = 16;
  OBJECT_FLAGS_UNKNOWN32 = 32;
  OBJECT_FLAGS_UNKNOWN64 = 64;
  OBJECT_FLAGS_UNKNOWN128 = 128;
  OBJECT_FLAGS_UNKNOWN256 = 256;

  SECTION_INST = 0;
  SECTION_INST_NAME = 'inst';
  SECTION_OBJS = 1;
  SECTION_OBJS_NAME = 'objs';
  SECTION_TOBJ = 2;
  SECTION_TOBJ_NAME = 'tobj';
  SECTION_PATH = 3;
  SECTION_PATH_NAME = 'path';
  SECTION_2DFX = 4;
  SECTION_2DFX_NAME = '2dfx';
  SECTION_CULL = 5;
  SECTION_CULL_NAME = 'cull';
  SECTION_ZONE = 6;
  SECTION_ZONE_NAME = 'zone';

  SECTION_MULT_INST = 10;
  SECTION_SEL_INST = 11;

type

  // Used for sorting
  TSortedThing = record
    Val: LongWord;
    Dist: Double;
  end;

  // Sub Section Headers

  TGTASInstObj = packed record
    HasInterior: Boolean;

    ID: Word;
    ModelName: String;
    Interior: Byte;

    RemFile, RemIndex, RemTimed: Integer;

    Pos: TVector3f;
    Scale: TVector3f;
    Rotation: TVector4f;
  end;

  PGTASInstObj = ^TGTASInstObj;

  TGTASObjsObj = packed record
    glDisplayList: GLUint;
    ListAvailable: Boolean;
    DffNum, TextureNum: LongInt;
    SortVal: LongWord;
    InUse: Boolean;

    ID: Word;

    ModelName: String;
    TextureName: String;

    U4: Byte;

    LOD: Single;

    Flags: Byte;
  end;

  PGTASObjsObj = ^TGTASObjsObj;

  TGTASTObjObj = packed record
    glDisplayList: GLUint;
    ListAvailable: Boolean;
    DffNum, TextureNum: LongInt;
    SortVal: LongWord;
    InUse: Boolean;

    ID: Word;

    ModelName: String;
    TextureName: String;

    U4: Word;

    LOD: Word;

    Flags: Word;

    TimeOn: Byte;

    TimeOff: Byte;
  end;

  TGTASPath2Obj = packed record
    NodeType: Word;
    NodeConnect: SmallInt;

    U3: Word;

    Pos: TVector3i;

    U7: Word;

    LaneLeft: SmallInt;
    LaneRight: SmallInt;
  end;
  
  TGTASPath2IPLObj = packed record
    NodeType: Word;
    NodeConnect: SmallInt;

    U3: Word;

    Pos: TVector3f;

    U7: Single;

    LaneLeft: SmallInt;
    LaneRight: SmallInt;

    U10: Word;
    U11: Word;
    U12: Word;
  end;

  TGTASPathObj = packed record
    PathType: String;

    ID: Word;
    ModelName: String;

    Item: array of TGTASPath2Obj;
    RCount: LongWord;
  end;

  TGTASPathIPLObj = packed record
    PathType: Word;
    PathOther: SmallInt;

    Item: array of TGTASPath2IPLObj;
    RCount: LongWord;
  end;

  TGTAS2dfxObj = packed record
    ID: Word;

    Pos: TVector3f;
    Color: array[0..2] of Byte;

    ViewDistance: Word;

    EffectType: Word;

    // A = LIGHT

      AEffect1: String;
      AEffect2: String;

      ADistance: Word;
      ARangeOuter: Single;
      ASizeLamp: Single;
      ARangeInner: Single;
      ASizeCorona: Single;
      AControl: Word;
      AReflectionWet: Word;
      ALensFlare: Word;
      ADust: Word;

    // B = PARTICLE

      BType: Word;

      BRotation: TVector4f;

    // C = UNKNOWN

    // D = ANIMATION

      DType: Word;

      DDirection1: TVector3f;
      DDirection2: TVector3f;

    // E = REFLECTION

  end;

  TGTASCullObj = packed record
    Pos1: TVector3f;

    Pos2: TVector3f;

    Pos3: TVector3f;

    U10: Word;
    U11: Word;
  end;

  TGTASZoneObj = record
    ZoneName: String;

    Sort: Word;

    Pos1: TVector3f;

    Pos2: TVector3f;

    U9: Word;
  end;

  // Section Headers

  TGTASInst = class
    Item: array of TGTASInstObj;
    Count: LongWord;

    Exist: Boolean;
  public
    function AddItem(InPos: TVector3f): LongWord; overload;
    procedure glDrawAll(JustCompile: Boolean; UseCutoff: Boolean; InCenter: TVector3f; InFile: LongWord; InTime: Byte);

    function AddItem: LongWord; overload;
    procedure DeleteItem(Num: LongWord);
    procedure LoadItem(Details: String); overload;
    function LoadItem(inNum: LongWord; Details: String): Boolean; overload;
    function SaveItem(Num: LongWord): String;
    constructor Create;
    destructor Destroy; override;
  end;

  TGTASObjs = class
    Item: array of TGTASObjsObj;
    Count: LongWord;

    Exist: Boolean;
  public
    ObjectList: TList;

    procedure glDraw(inID: Word);
    procedure glDrawIndex(inIndex: Word; JustCompile: Boolean; Distance: Single);
    procedure glDrawCompile;
    procedure CalcArchiveNums(Num: LongWord);
    function GetLOD(inID: Word): Single;
    function GetMaxID: LongWord;
    procedure SetAvailable(in_name: String);

    procedure CreateList;
    procedure DestroyList;
    procedure SetNotUsed;
    procedure DestroyNotUsed;

    function AddItem: LongWord;
    procedure DeleteItem(Num: LongWord);
    procedure LoadItem(Details: String); overload;
    function LoadItem(inNum: LongWord; Details: String): Boolean; overload;
    function SaveItem(Num: LongWord): String;
    constructor Create;
    destructor Destroy; override;
  end;

  TGTASTObj = class
    Item: array of TGTASTObjObj;
    Count: LongWord;

    Exist: Boolean;
  public
    ObjectList: TList;

    procedure glDraw(inID: Word);
    procedure glDrawIndex(inIndex: Word; JustCompile: Boolean; Distance: Single);
    procedure glDrawCompile;
    procedure CalcArchiveNums(Num: LongWord);
    function GetLOD(inID: Word): Single;
    function GetMaxID: LongWord;
    procedure SetAvailable(in_name: String);

    procedure CreateList;
    procedure DestroyList;
    procedure SetNotUsed;
    procedure DestroyNotUsed;

    function AddItem: LongWord;
    procedure DeleteItem(Num: LongWord);
    procedure LoadItem(Details: String); overload;
    function LoadItem(inNum: LongWord; Details: String): Boolean; overload;
    function SaveItem(Num: LongWord): String;
    constructor Create;
    destructor Destroy; override;
  end;

  TGTASPath = class
    Item: array of TGTASPathObj;
    Count: LongWord;

    Exist: Boolean;
  public
    procedure AddRoute(Num: LongWord; Details: String);

    function AddItem: LongWord;
    procedure DeleteItem(Num: LongWord);
    procedure LoadItem(Details: String); overload;
    function LoadItem(inNum: LongWord; Details: String): Boolean; overload;
    function SaveItem(Num: LongWord): String;
    constructor Create;
    destructor Destroy; override;
  end;

  TGTASPathIPL = class
    Item: array of TGTASPathIPLObj;
    Count: LongWord;

    Exist: Boolean;
  public
    procedure AddRoute(Num: LongWord; Details: String);

    function AddItem: LongWord;
    procedure DeleteItem(Num: LongWord);
    procedure LoadItem(Details: String); overload;
    function LoadItem(inNum: LongWord; Details: String): Boolean; overload;
    function SaveItem(Num: LongWord): String;
    constructor Create;
    destructor Destroy; override;
  end;

  TGTAS2dfx = class
    Item: array of TGTAS2dfxObj;
    Count: LongWord;

    Exist: Boolean;
  public
    function AddItem: LongWord;
    procedure DeleteItem(Num: LongWord);
    procedure LoadItem(Details: String); overload;
    function LoadItem(inNum: LongWord; Details: String): Boolean; overload;
    function SaveItem(Num: LongWord): String;
    constructor Create;
    destructor Destroy; override;
  end;

  TGTASCull = class
    Item: array of TGTASCullObj;
    Count: LongWord;

    Exist: Boolean;
  public
    function AddItem: LongWord;
    procedure DeleteItem(Num: LongWord);
    procedure LoadItem(Details: String); overload;
    function LoadItem(inNum: LongWord; Details: String): Boolean; overload;
    function SaveItem(Num: LongWord): String;
    constructor Create;
    destructor Destroy; override;
  end;

  TGTASZone = class
    Item: array of TGTASZoneObj;
    Count: LongWord;

    Exist: Boolean;
  public
    function AddItem: LongWord;
    procedure DeleteItem(Num: LongWord);
    procedure LoadItem(Details: String); overload;
    function LoadItem(inNum: LongWord; Details: String): Boolean; overload;
    function SaveItem(Num: LongWord): String;
    constructor Create;
    destructor Destroy; override;
  end;

  // File Header

  TGTASFile = class
    Name: String;
    Changed: Boolean;
    InDat: Boolean;
    SubType: Byte;
    Visible: Boolean;
  public
    constructor Create; virtual;
    function LoadFrom(FileName: String): LongWord; virtual;
    function Save: LongWord; virtual;
  end;

  // IDE File

  TIDEFile = class(TGTASFile)
    Objs: TGTASObjs;
    TObj: TGTASTObj;
    Path: TGTASPath;
    Wdfx: TGTAS2dfx;
  public
    function LoadFrom(FileName: String): LongWord; override;
    function Save: LongWord; override;
    constructor Create; override;
    destructor Destroy; override;
  end;


  TIPLFile = class(TGTASFile)
    Inst: TGTASInst;
    Cull: TGTASCull;
    Zone: TGTASZone;
    Path: TGTASPathIPL;
  public
    function LoadFrom(FileName: String): LongWord; override;
    function Save: LongWord; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  // File List Header

  TGTAFileList = class
    Item: array of TGTASFile;
    Count: LongWord;
    DatFile: TStringList;
    DatChanged: Boolean;
  public
    KillNotUsed: Boolean;
    
    procedure SetNotUsed;
    procedure DestroyNotUsed;

    function CreateFileList: TStringList;
    function LoadFromDAT(ObjData: Boolean): LongWord;
    function SaveDat: LongWord;
    procedure glDrawAll(UseCutoff: Boolean; InCenter: TVector3f; InTime: Byte);
    procedure glDraw(inID: Word);
    function GetLOD(inID: Word): Single;
    function GetMaxID: LongWord;
    procedure SetAvailable(in_name: String);

    constructor Create;
    destructor Destroy; override;
  end;

function GetVal(Num: Integer; Str: String): String;

implementation

uses Main, Validate;

// maths functions

function ArcTan2(Y, X: Extended): Extended;
asm
    FLD       Y
    FLD       X
    FPATAN
    FWAIT
end;

function ArcCos(X: Extended): Extended;
begin
  Result := ArcTan2(Sqrt(1 - X * X), X);
end;

// ********
// FILE HEADER
// ********

// *** TGTA FILE LIST

procedure TGTAFileList.glDrawAll(UseCutoff: Boolean; InCenter: TVector3f; InTime: Byte);
var
  I: LongWord;
begin
  if (Count > 0) then for I := 0 to Count - 1 do
    if (Item[I].SubType = FILE_IPL) and (Item[I].Visible) then
    begin
      if Main.MainGLView.Picking then
        glPushName(I);
      TIPLFile(Item[I]).Inst.glDrawAll(False, UseCutoff, InCenter, I, InTime);
      if Main.MainGLView.Picking then
        glPopName;
    end;
end;

procedure TGTAFileList.glDraw(inID: Word);
var
  I: LongWord;
begin
  if (Count > 0) then for I := 0 to Count - 1 do
    if (Item[I].SubType = FILE_IDE) then
    begin
      TIDEFile(Item[I]).Objs.glDraw(inID);
      TIDEFile(Item[I]).TObj.glDraw(inID);
    end;
end;

procedure TGTAFileList.SetAvailable(in_name: String);
var
  I: LongWord;
begin
  if (Count > 0) then for I := 0 to Count - 1 do
    if (Item[I].SubType = FILE_IDE) then
    begin
      TIDEFile(Item[I]).Objs.SetAvailable(in_name);
      TIDEFile(Item[I]).TObj.SetAvailable(in_name);
    end;
end;

function TGTAFileList.GetLOD(inID: Word): Single;
var
  I: LongWord;
  Temp: Single;
begin
  Result := 0;
  if (Count > 0) then for I := 0 to Count - 1 do
    if (Item[I].SubType = FILE_IDE) then
    begin
      Temp := TIDEFile(Item[I]).Objs.GetLOD(inID);
      if (Temp > Result) then Result := Temp;

      Temp := TIDEFile(Item[I]).TObj.GetLOD(inID);
      if (Temp > Result) then Result := Temp;
    end;
end;

function TGTAFileList.GetMaxID: LongWord;
var
  I, Temp: LongWord;
begin
  Result := 0;
  if (Count > 0) then for I := 0 to Count - 1 do
    if (Item[I].SubType = FILE_IDE) then
    begin
      Temp := TIDEFile(Item[I]).Objs.GetMaxID;
      if (Temp > Result) then Result := Temp;

      Temp := TIDEFile(Item[I]).TObj.GetMaxID;
      if (Temp > Result) then Result := Temp;
    end;
end;

procedure TGTAFileList.SetNotUsed;
var
  I: LongWord;
begin
  KillNotUsed := True;
  if (Count > 0) then for I := 0 to Count - 1 do
    if (Item[I].SubType = FILE_IDE) then
    begin
      TIDEFile(Item[I]).Objs.SetNotUsed;
      TIDEFile(Item[I]).TObj.SetNotUsed;
    end;
end;

procedure TGTAFileList.DestroyNotUsed;
var
  I: LongWord;
begin
  if (Count > 0) then for I := 0 to Count - 1 do
    if (Item[I].SubType = FILE_IDE) then
    begin
      TIDEFile(Item[I]).Objs.DestroyNotUsed;
      TIDEFile(Item[I]).TObj.DestroyNotUsed;
    end;
end;

function TGTAFileList.LoadFromDAT(ObjData: Boolean): LongWord;
var
  I: LongInt;
  UpTo: LongWord;
  LoadFile: TextFile;
  ToParse, FileType: String;
  Opened: Boolean;
begin
  Opened := True;
  Result := 0;
  DatFile.Clear;
  DatChanged := False;
  Main.FormLoading.SetPart('Parsing DAT Contents');

  if (GTA_VICE_MODE) then
    AssignFile(LoadFile, GTAPath + VICE_DAT_FILE)
  else
    AssignFile(LoadFile, GTAPath + GTA3_DAT_FILE);

  UpTo := Count;

  {I-}
  try
    FileMode := 0;
    Reset(LoadFile);
  except
    on Exception do
    begin
      Opened := False;
      Result := GetLastError;
    end;
  end;
  {I+}

  if Opened then try
    while not Eof(LoadFile) do
    begin
      Readln(LoadFile, ToParse);
      ToParse := Trim(ToParse);
      DatFile.Add(ToParse);

      if (Length(ToParse) > 0) and not (Copy(ToParse, 0, 1) = '#') then
      begin
        I := Pos(' ', ToParse);
        if (I > 0) then
        begin
          FileType := Trim(LeftStr(ToParse, I-1));
          ToParse := Trim(Copy(ToParse, I+1, Length(ToParse)));

          if (CompareText(FileType, 'IDE') = 0) and ObjData then
          begin

            Inc(Count);
            SetLength(Item, Count);

            Item[Count - 1] := TIDEFile.Create;

            Item[Count - 1].InDat := True;
            Item[Count - 1].Name := ToParse;

          end;

          if ((CompareText(FileType, 'IPL') = 0) or
             (CompareText(FileType, 'MAPZONE') = 0)) and not ObjData then
          begin

            Inc(Count);
            SetLength(Item, Count);

            Item[Count - 1] := TIPLFile.Create;

            Item[Count - 1].InDat := True;
            Item[Count - 1].Name := ToParse;

          end;

          if (CompareText(FileType, 'COLFILE') = 0) and not ObjData then
          begin
          end;

        end;
      end;
    end;
  finally
    if Opened then
      CloseFile(LoadFile);
  end;

  Main.FormLoading.SetPartMax(Count - UpTo);

  if (Count > UpTo) then for I := UpTo to Count - 1 do
  begin
    Main.FormLoading.SetPart(Item[I].Name);
    Item[I].LoadFrom(Item[I].Name);
    if (Item[I].SubType = FILE_IDE) and GTA_MODEL_MODE and GTA_DISPLAY_LISTS and not GTA_TEXTURE_LOAD_DEMAND then
      TIDEFile(Item[I]).Objs.glDrawCompile;
    Main.FormLoading.IncPartPos;
  end;
end;

function TGTAFileList.SaveDat: LongWord;
var
  I: LongWord;

  Opened: Boolean;

  SaveFile: TextFile;
begin
  Opened := True;
  Result := 0;

  if (GTA_VICE_MODE) then
    AssignFile(SaveFile, GTAPath + VICE_DAT_FILE)
  else
    AssignFile(SaveFile, GTAPath + GTA3_DAT_FILE);

  {I-}
  try
    FileMode := 0;
    Rewrite(SaveFile);
  except
    on Exception do
    begin
      Opened := False;
      Result := GetLastError;
    end;
  end;
  {I+}

  if Opened then try
    if (DatFile.Count > 0) then for I := 0 to DatFile.Count - 1 do
      Writeln(SaveFile, DatFile.Strings[I]);
  finally
    if Opened then
      CloseFile(SaveFile);
  end;
end;

function TGTAFileList.CreateFileList: TStringList;
var
  FileList: TStringList;
  I: LongInt;
begin
  FileList := TStringList.Create;

  for I := 0 to Count - 1 do
    FileList.AddObject(Item[I].Name, Item[I]);

  Result := FileList;
end;

constructor TGTAFileList.Create;
begin
  SetLength(Item, 0);
  DATFile := TStringList.Create;
  Count := 0;
  KillNotUsed := False;
end;

destructor TGTAFileList.Destroy;
var
  I: LongWord;
begin
  inherited Destroy;
  if (Count > 0) then for I := 0 to Count - 1 do
    Item[I].Destroy;
  DATFile.Free;
  SetLength(Item, 0);
end;

// *** IDE File

constructor TIDEFile.Create;
begin
  SubType := FILE_IDE;
  Visible := False;
  Objs := TGTASObjs.Create;
  TObj := TGTASTObj.Create;
  Path := TGTASPath.Create;
  Wdfx := TGTAS2dfx.Create;
end;

destructor TIDEFile.Destroy;
begin
  Objs.Destroy;
  TObj.Destroy;
  Wdfx.Destroy;
  inherited Destroy;
end;

// *** IPL FILE

constructor TIPLFile.Create;
begin
  SubType := FILE_IPL;
  Visible := False;
  Inst := TGTASInst.Create;
  Cull := TGTASCull.Create;
  Zone := TGTASZone.Create;
  Path := TGTASPathIPL.Create;
end;

destructor TIPLFile.Destroy;
begin
  Inst.Destroy;
  Cull.Destroy;
  Zone.Destroy;
  Path.Destroy;
  inherited Destroy;
end;

// *** TGTAS FILE

constructor TGTASFile.Create;
begin
  Changed := False;
  InDat := False;
end;

function TGTASFile.LoadFrom(FileName: String): LongWord;
begin
  Result := 0;
end;

function TGTASFile.Save: LongWord;
begin
  Result := 0;
end;

// *** TIPL FILE

function TIPLFile.LoadFrom(FileName: String): LongWord;
var
  Section: Integer;
  DoProcess, ExpectOther: Boolean;

  ToParse: String;
  Opened: Boolean;

  LoadFile: TextFile;
begin
  Opened := True;
  Result := 0;
  Name := FileName;
  Section := -1;
  if (InDat) then
    ToParse := GTAPath + FileName
  else
    ToParse := FileName;
  AssignFile(LoadFile, ToParse);

  {I-}
  try
    FileMode := 0;
    Reset(LoadFile);
  except
    on Exception do
    begin
      Opened := False;
      Result := GetLastError;
    end;
  end;
  {I+}

  if Opened then try
    while not Eof(LoadFile) do
    begin
      Readln(LoadFile, ToParse);
      ExpectOther := (Length(ToParse) > 0) and (ToParse[1] = #9) and (Section = SECTION_PATH);
      ToParse := Trim(ToParse);
      DoProcess := True;

      if (CompareText('end', ToParse) = 0) then
        Section := -1
      else if (Length(ToParse) > 0) and not (ToParse[1] = '#') then
      begin

        if ToParse = SECTION_INST_NAME then
        begin
          Section := SECTION_INST;
          Inst.Exist := True;
          DoProcess := False;
        end else if ToParse = SECTION_CULL_NAME then
        begin
          Section := SECTION_CULL;
          Cull.Exist := True;
          DoProcess := False;
        end else if ToParse = SECTION_ZONE_NAME then
        begin
          Section := SECTION_ZONE;
          Zone.Exist := True;
          DoProcess := False;
        end else if ToParse = SECTION_PATH_NAME then
        begin
          Section := SECTION_Path;
          Path.Exist := True;
          DoProcess := False;
        end;

        if (DoProcess) and (Section >= 0) then
        begin
          if (ExpectOther) then
            Path.AddRoute(Path.Count - 1, ToParse)
          else
          begin
            case Section of
              SECTION_INST:
                Inst.LoadItem(ToParse);
              SECTION_CULL:
                Cull.LoadItem(ToParse);
              SECTION_ZONE:
                Zone.LoadItem(ToParse);
              SECTION_PATH:
                Path.LoadItem(ToParse);
            end;
          end;

        end;

      end;

    end;
  finally
    if Opened then
      CloseFile(LoadFile);
  end;
end;

function TIPLFile.Save: LongWord;
var
  I: LongWord;
  Opened: Boolean;

  ToParse: String;

  SaveFile: TextFile;
begin
  Opened := True;
  Result := 0;
  if (InDat) then
    ToParse := GTAPath + Name
  else
    ToParse := Name;
  AssignFile(SaveFile, ToParse);

  {I-}
  try
    FileMode := 0;
    Rewrite(SaveFile);
  except
    on Exception do
    begin
      Opened := False;
      Result := GetLastError;
    end;
  end;

  {I+}

  if Opened then try
    if Inst.Exist then
    begin
      Writeln(SaveFile, 'inst');
      if (Inst.Count > 0) then for I := 0 to Inst.Count - 1 do
        Writeln(SaveFile, Inst.SaveItem(I));
      Writeln(SaveFile, 'end');
    end;

    if Cull.Exist then
    begin
      Writeln(SaveFile, 'cull');
      if (Cull.Count > 0) then for I := 0 to Cull.Count - 1 do
        Writeln(SaveFile, Cull.SaveItem(I));
      Writeln(SaveFile, 'end');
    end;

    if Zone.Exist then
    begin
      Writeln(SaveFile, 'zone');
      if (Zone.Count > 0) then for I := 0 to Zone.Count - 1 do
        Writeln(SaveFile, Zone.SaveItem(I));
      Writeln(SaveFile, 'end');
    end;

    if Path.Exist then
    begin
      Writeln(SaveFile, 'path');
      if (Path.Count > 0) then for I := 0 to Path.Count - 1 do
        Writeln(SaveFile, Path.SaveItem(I));
      Writeln(SaveFile, 'end');
    end;

    Changed := False;
  finally
    if Opened then
      CloseFile(SaveFile);
  end;
end;

// *** TIDE FILE

function TIDEFile.LoadFrom(FileName: String): LongWord;
var
  Section: Integer;
  DoProcess, ExpectOther: Boolean;

  ToParse: String;
  Opened: Boolean;

  LoadFile: TextFile;
begin
  Opened := True;
  Result := 0;
  Name := FileName;
  Section := -1;
  if (InDat) then
    ToParse := GTAPath + FileName
  else
    ToParse := FileName;
  AssignFile(LoadFile, ToParse);

  {I-}
  try
    FileMode := 0;
    Reset(LoadFile);
  except
    on Exception do
    begin
      Opened := False;
      Result := GetLastError;
    end;
  end;
  {I+}

  if Opened then try
    while not Eof(LoadFile) do
    begin
      Readln(LoadFile, ToParse);
      ExpectOther := (Length(ToParse) > 0) and (ToParse[1] = #9) and (Section = SECTION_PATH);
      ToParse := Trim(ToParse);
      DoProcess := True;

      if (CompareText('end', ToParse) = 0) then
        Section := -1
      else if (Length(ToParse) > 0) and not (ToParse[1] = '#') then
      begin

        if ToParse = SECTION_OBJS_NAME then
        begin
          Section := SECTION_OBJS;
          Objs.Exist := True;
          DoProcess := False;
        end else if ToParse = SECTION_TOBJ_NAME then
        begin
          Section := SECTION_TOBJ;
          TObj.Exist := True;
          DoProcess := False;
        end else if ToParse = SECTION_PATH_NAME then
        begin
          Section := SECTION_PATH;
          Path.Exist := True;
          DoProcess := False;
        end else if ToParse = SECTION_2DFX_NAME then
        begin
          Section := SECTION_2DFX;
          Wdfx.Exist := True;
          DoProcess := False;
        end;

        if (DoProcess) and (Section >= 0) then
        begin

          if (ExpectOther) then
            Path.AddRoute(Path.Count - 1, ToParse)
          else
          begin
            case Section of
              SECTION_OBJS:
                Objs.LoadItem(ToParse);
              SECTION_TOBJ:
                TObj.LoadItem(ToParse);
              SECTION_PATH:
                Path.LoadItem(ToParse);
              SECTION_2DFX:
                Wdfx.LoadItem(ToParse);
            end;

          end;

        end;

      end;

    end;
  finally
    if Opened then
      CloseFile(LoadFile);
  end;
end;

function TIDEFile.Save: LongWord;
var
  I: LongWord;

  ToParse: String;
  Opened: Boolean;

  SaveFile: TextFile;
begin
  Opened := True;
  Result := 0;
  if (InDat) then
    ToParse := GTAPath + Name
  else
    ToParse := Name;
  AssignFile(SaveFile, ToParse);

  {I-}
  try
    FileMode := 0;
    Rewrite(SaveFile);
  except
    on Exception do
    begin
      Opened := False;
      Result := GetLastError;
    end;
  end;
  {I+}

  if Opened then try
    if Objs.Exist then
    begin
      Writeln(SaveFile, 'objs');
      if (Objs.Count > 0) then for I := 0 to Objs.Count - 1 do
        Writeln(SaveFile, Objs.SaveItem(I));
      Writeln(SaveFile, 'end');
    end;

    if TObj.Exist then
    begin
      Writeln(SaveFile, 'tobj');
      if (TObj.Count > 0) then for I := 0 to TObj.Count - 1 do
        Writeln(SaveFile, TObj.SaveItem(I));
      Writeln(SaveFile, 'end');
    end;

    if Path.Exist then
    begin
      Writeln(SaveFile, 'path');
      if (Path.Count > 0) then for I := 0 to Path.Count - 1 do
        Writeln(SaveFile, Path.SaveItem(I));
      Writeln(SaveFile, 'end');
    end;

    if Wdfx.Exist then
    begin
      Writeln(SaveFile, '2dfx');
      if (Wdfx.Count > 0) then for I := 0 to Wdfx.Count - 1 do
        Writeln(SaveFile, Wdfx.SaveItem(I));
      Writeln(SaveFile, 'end');
    end;

    Changed := False;
  finally
    CloseFile(SaveFile);
  end;
end;

// *******
// SECTION HEADERS
// *******

// *** TGTAS Inst

procedure TGTASInst.glDrawAll(JustCompile: Boolean; UseCutoff: Boolean; InCenter: TVector3f; InFile: LongWord; InTime: Byte);
  procedure Quicksort(var List : array of TSortedThing; min, max : LongInt);
  var
    med_value: TSortedThing;
    hi, lo, i: LongInt;
  begin
    if (min >= max) then Exit;

    i := min + Trunc(Random(max - min + 1));
    med_value := List[i];

    List[i] := List[min];

    lo := min; hi := max;
    while (True) do
    begin
        while (List[hi].Dist >= med_value.Dist) and (hi > lo) do
            Dec(Hi);

        if (hi <= lo) then
        begin
            List[lo] := med_value;
            Break;
        end;

        List[lo] := List[hi];

        Inc(Lo);
        while (List[lo].Dist < med_value.Dist) and (lo < hi) do
            Inc(Lo);

        if (lo >= hi) then
        begin
            lo := hi;
            List[hi] := med_value;
            Break;
        end;

        List[hi] := List[lo];
    end;

    Quicksort(List, min, lo - 1);
    Quicksort(List, lo + 1, max);
  end;
var
  I, J, K: LongWord;
  LOD: Boolean;
  Index: LongInt;

  Axis: TVector3f;
  Angle: Single;

  X, Y, Z, W, S: Single;
begin

  if FormMain.ResetObjectList then
  begin
    FormLoading.IncPos;
    FormLoading.SetPartMax(Count);
  end;

  if (Count > 0) then
  begin
    for I := 0 to Count - 1 do
    with Item[I] do
    begin
      if FormMain.ResetObjectList then
      begin
        FormLoading.IncPartPos;
        //FormLoading.SetPart(IntToStr(ID) + ' - ' + ModelName);
      end;

      glPushMatrix;

      glTranslatef(Pos[0], Pos[1], Pos[2]);

      X := ArcSin(Rotation[0]); Y := ArcSin(Rotation[1]); Z := ArcSin(Rotation[2]); W := -Rotation[3];

      S := Sqrt(1.0 - W * W);

      // divide by zero
      if not (S = 0) then
      begin
        Axis[0] := X / S; Axis[1] := Y / S; Axis[2] := Z / S;
        Angle := 2 * ArcCos(W);

        if not (Angle = 0) then
          glRotatef(Angle * 180 / Pi, Axis[0], Axis[1], Axis[2]);
      end;

      if Main.MainGLView.Picking then
        glPushName(I);

      if GTA_MODEL_MODE then
      begin

        LOD := (CompareText(LeftStr(ModelName, 3), 'LOD') = 0);

        if not (RemTimed = -1) and not (RemTimed = InTime) then
        begin
          RemFile := -1;
          RemIndex := -1;
        end;

        if (LOD and ((LODMode = 1) or (LODMode = 2))) or
           ((not LOD) and ((LODMode = 0) or (LODMode = 2))) then
        begin
          if not (RemFile = -1) and not (RemIndex = -1) and
                 (LongWord(RemFile) < Main.GFiles.Count) and
                 (((RemTimed = -1) and (LongWord(RemIndex) < TIDEFile(Main.GFiles.Item[RemFile]).Objs.Count) and
                 (TIDEFile(Main.GFiles.Item[RemFile]).Objs.Item[RemIndex].ID = ID)) or
                 (not (RemTimed = -1) and (LongWord(RemIndex) < TIDEFile(Main.GFiles.Item[RemFile]).TObj.Count) and
                 (TIDEFile(Main.GFiles.Item[RemFile]).TObj.Item[RemIndex].ID = ID))) then
          begin
            if (RemTimed = -1) then
              TIDEFile(Main.GFiles.Item[RemFile]).Objs.glDrawIndex(RemIndex, False, 0)
            else
              TIDEFile(Main.GFiles.Item[RemFile]).TObj.glDrawIndex(RemIndex, False, 0);
          end else
          begin
            RemFile := -1; RemIndex := -1; RemTimed := -1;
            if (Main.GFiles.Count > 0) then
            begin
              J := 0;
              while (J < Main.GFiles.Count) do
              begin
                K := 0;
                if (Main.GFiles.Item[J].SubType = FILE_IDE) and
                   (TIDEFile(Main.GFiles.Item[J]).Objs.Count > 0) then
                  if (CompareText(Main.GFiles.Item[J].Name, ChangeFileExt(Main.GFiles.Item[InFile].Name, 'IDE')) = 0) then
                    if not (TIDEFile(Main.GFiles.Item[J]).Objs.ObjectList = nil) then
                    begin
                      Index := Validate.BinarySearchObjs(ID, TIDEFile(Main.GFiles.Item[J]).Objs.ObjectList);
                      if not (Index = -1) then
                      begin
                        RemFile := J;
                        RemIndex := TGTASObjsObj(TIDEFile(Main.GFiles.Item[J]).Objs.ObjectList.Items[Index]^).SortVal;
                        RemTimed := -1;
                        TIDEFile(Main.GFiles.Item[RemFile]).Objs.glDrawIndex(RemIndex, False, 0);
                      end;
                    end else
                    begin
                      while (K < TIDEFile(Main.GFiles.Item[J]).Objs.Count) do
                      begin
                        if (TIDEFile(Main.GFiles.Item[J]).Objs.Item[K].ID = ID) then
                        begin
                          RemFile := J;
                          RemIndex := K;
                          RemTimed := -1;
                          K := TIDEFile(Main.GFiles.Item[RemFile]).Objs.Count;
                          TIDEFile(Main.GFiles.Item[RemFile]).Objs.glDrawIndex(RemIndex, False, 0);
                        end;
                        Inc(K);
                      end;
                    end;
                K := 0;
                if (RemFile = -1) and (Main.GFiles.Item[J].SubType = FILE_IDE) and
                   (TIDEFile(Main.GFiles.Item[J]).TObj.Count > 0) then
                  if (CompareText(Main.GFiles.Item[J].Name, ChangeFileExt(Main.GFiles.Item[InFile].Name, 'IDE')) = 0) then
                    while (K < TIDEFile(Main.GFiles.Item[J]).TObj.Count) do
                    begin
                      if (TIDEFile(Main.GFiles.Item[J]).TObj.Item[K].ID = ID) then
                      begin
                        if ((TIDEFile(Main.GFiles.Item[J]).TObj.Item[K].TimeOn > TIDEFile(Main.GFiles.Item[J]).TObj.Item[K].TimeOff) and ((InTime >= TIDEFile(Main.GFiles.Item[J]).TObj.Item[K].TimeOn) or (InTime < TIDEFile(Main.GFiles.Item[J]).TObj.Item[K].TimeOff)))
                        or ((TIDEFile(Main.GFiles.Item[J]).TObj.Item[K].TimeOn < TIDEFile(Main.GFiles.Item[J]).TObj.Item[K].TimeOff) and ((InTime >= TIDEFile(Main.GFiles.Item[J]).TObj.Item[K].TimeOn) and (InTime < TIDEFile(Main.GFiles.Item[J]).TObj.Item[K].TimeOff))) then
                        begin
                          RemFile := J;
                          RemIndex := K;
                          RemTimed := InTime;
                          K := TIDEFile(Main.GFiles.Item[RemFile]).TObj.Count;
                          TIDEFile(Main.GFiles.Item[RemFile]).TObj.glDrawIndex(RemIndex, False, 0);
                        end;
                      end;
                      Inc(K);
                    end;
                if not (RemFile = -1) then
                  J := Main.GFiles.Count;
                Inc(J);
              end;

              J := 0;
              if (RemFile = -1) then while (J < Main.GFiles.Count) do
              begin
                K := 0;
                if (Main.GFiles.Item[J].SubType = FILE_IDE) and
                   (TIDEFile(Main.GFiles.Item[J]).Objs.Count > 0) then
                  if not (CompareText(Main.GFiles.Item[J].Name, ChangeFileExt(Main.GFiles.Item[InFile].Name, 'IDE')) = 0) then
                    if not (TIDEFile(Main.GFiles.Item[J]).Objs.ObjectList = nil) then
                    begin
                      Index := Validate.BinarySearchObjs(ID, TIDEFile(Main.GFiles.Item[J]).Objs.ObjectList);
                      if not (Index = -1) then
                      begin
                        RemFile := J;
                        RemIndex := TGTASObjsObj(TIDEFile(Main.GFiles.Item[J]).Objs.ObjectList.Items[Index]^).SortVal;
                        RemTimed := -1;
                        TIDEFile(Main.GFiles.Item[RemFile]).Objs.glDrawIndex(RemIndex, False, 0);
                      end;
                    end else
                    begin
                      while (K < TIDEFile(Main.GFiles.Item[J]).Objs.Count) do
                      begin
                        if (TIDEFile(Main.GFiles.Item[J]).Objs.Item[K].ID = ID) then
                        begin
                          RemFile := J;
                          RemIndex := K;
                          RemTimed := -1;
                          K := TIDEFile(Main.GFiles.Item[RemFile]).Objs.Count;
                          TIDEFile(Main.GFiles.Item[RemFile]).Objs.glDrawIndex(RemIndex, False, 0);
                        end;
                        Inc(K);
                      end;
                    end;
                K := 0;
                if (RemFile = -1) and (Main.GFiles.Item[J].SubType = FILE_IDE) and
                   (TIDEFile(Main.GFiles.Item[J]).TObj.Count > 0) then
                  if not (CompareText(Main.GFiles.Item[J].Name, ChangeFileExt(Main.GFiles.Item[InFile].Name, 'IDE')) = 0) then
                    while (K < TIDEFile(Main.GFiles.Item[J]).TObj.Count) do
                    begin
                      if (TIDEFile(Main.GFiles.Item[J]).TObj.Item[K].ID = ID) then
                      begin
                        if ((TIDEFile(Main.GFiles.Item[J]).TObj.Item[K].TimeOn > TIDEFile(Main.GFiles.Item[J]).TObj.Item[K].TimeOff) and ((InTime >= TIDEFile(Main.GFiles.Item[J]).TObj.Item[K].TimeOn) or (InTime < TIDEFile(Main.GFiles.Item[J]).TObj.Item[K].TimeOff)))
                        or ((TIDEFile(Main.GFiles.Item[J]).TObj.Item[K].TimeOn < TIDEFile(Main.GFiles.Item[J]).TObj.Item[K].TimeOff) and ((InTime >= TIDEFile(Main.GFiles.Item[J]).TObj.Item[K].TimeOn) and (InTime < TIDEFile(Main.GFiles.Item[J]).TObj.Item[K].TimeOff))) then
                        begin
                          RemFile := J;
                          RemIndex := K;
                          RemTimed := InTime;
                          K := TIDEFile(Main.GFiles.Item[RemFile]).TObj.Count;
                          TIDEFile(Main.GFiles.Item[RemFile]).TObj.glDrawIndex(RemIndex, False, 0);
                        end;
                      end;
                      Inc(K);
                    end;
                if not (RemFile = -1) then
                  J := Main.GFiles.Count;
                Inc(J);
              end;
            end;
            if (RemFile = -1) then
              MainGLView.glDrawObject(True);
          end;
        end;
      end else
        // draw a sphere
        MainGLView.glDrawObject(True);

      if Main.MainGLView.Picking then
        glPopName;

      glPopMatrix;
      //end;
    end;
  end;
//  SetLength(Sorted, 0);
end;

function TGTASInst.AddItem(InPos: TVector3f): LongWord;
begin
  Result := AddItem;

  with Item[Result] do
  begin
    Pos := InPos;
  end;
end;

constructor TGTASInst.Create;
begin
  SetLength(Item, 0);
  Exist := False;
  Count := 0;
end;

destructor TGTASInst.Destroy;
begin
  Finalize(Item);
  inherited Destroy;
end;

function TGTASInst.AddItem: LongWord;
begin
  Inc(Count);
  SetLength(Item, Count);
  Exist := True;

  with Item[Count - 1] do
  begin
    // defaults here
    HasInterior := GTA_VICE_MODE;

    ID := 0;
    ModelName := '';
    Interior := 0;

    RemFile := -1;
    RemIndex := -1;
    RemTimed := -1;

    Pos[0] := 0; Pos[1] := 0; Pos[2] := 0;
    Scale[0] := 0; Scale[1] := 0; Scale[2] := 0;
    Rotation[0] := 0; Rotation[1] := 0; Rotation[2] := 0; Rotation[3] := 1;
  end;

  Result := Count - 1;
end;

procedure TGTASInst.DeleteItem(Num: LongWord);
var
  I: LongInt;
begin
  Dec(Count);
  if (Count - 1 > Num) then for I := Num to Count - 1 do
    Item[I] := Item[I+1];
  SetLength(Item, Count);
end;

// *** TGTAS Objs

constructor TGTASObjs.Create;
begin
  SetLength(Item, 0);
  Exist := False;
  Count := 0;
end;

destructor TGTASObjs.Destroy;
var
  I: LongWord;
begin
  if GTA_DISPLAY_LISTS and (Count > 0) then for I := 0 to Count - 1 do
    if not (Item[I].glDisplayList = 0) then
      glDeleteLists(Item[I].glDisplayList, 1);
  DestroyList;
  Finalize(Item);
  inherited Destroy;
end;

function TGTASObjs.AddItem: LongWord;
begin
  Inc(Count);
  SetLength(Item, Count);
  Exist := True;

  with Item[Count - 1] do
  begin
    // defaults here
    glDisplayList := 0;
    ListAvailable := False;
    DffNum := -1; TextureNum := -1;
    SortVal := 0;
    InUse := False;

    ID := GFiles.GetMaxID + 1;

    ModelName := '';
    TextureName := '';

    U4 := 1;

    LOD := 0;

    Flags := 0;
  end;

  Result := Count - 1;
end;

procedure TGTASObjs.DeleteItem(Num: LongWord);
var
  I: LongInt;
begin
  Dec(Count);
  if GTA_DISPLAY_LISTS and not (Item[Num].glDisplayList = 0) then
    glDeleteLists(Item[Num].glDisplayList, 1);
  if (Count - 1 > Num) then for I := Num to Count - 1 do
    Item[I] := Item[I+1];
  SetLength(Item, Count);
end;

// *** TGTAS TObj

constructor TGTASTObj.Create;
begin
  SetLength(Item, 0);
  Exist := False;
  Count := 0;
end;

destructor TGTASTObj.Destroy;
var
  I: LongWord;
begin
  if GTA_DISPLAY_LISTS and (Count > 0) then for I := 0 to Count - 1 do
    if not (Item[I].glDisplayList = 0) then
      glDeleteLists(Item[I].glDisplayList, 1);
  DestroyList;
  Finalize(Item);
  inherited Destroy;
end;

function TGTASTObj.AddItem: LongWord;
begin
  Inc(Count);
  SetLength(Item, Count);
  Exist := True;

  with Item[Count - 1] do
  begin
    // defaults here
    glDisplayList := 0;
    ListAvailable := False;
    DffNum := -1; TextureNum := -1;
    SortVal := 0;
    InUse := False;

    ID := GFiles.GetMaxID + 1;

    ModelName := '';
    TextureName := '';

    U4 := 1;

    LOD := 0;

    Flags := 0;

    TimeOn := 6;
    TimeOff := 19;
  end;

  Result := Count - 1;
end;

procedure TGTASTObj.DeleteItem(Num: LongWord);
var
  I: LongInt;
begin
  Dec(Count);
  if GTA_DISPLAY_LISTS and not (Item[Num].glDisplayList = 0) then
    glDeleteLists(Item[Num].glDisplayList, 1);
  if (Count - 1 > Num) then for I := Num to Count - 1 do
    Item[I] := Item[I+1];
  SetLength(Item, Count);
end;

// *** TGTAS Path

procedure TGTASPath.AddRoute(Num: LongWord; Details: String);
begin
  with Item[Num] do
  begin

  Inc(RCount);
  SetLength(Item, RCount);

  try

  with Item[RCount - 1] do
  begin

  NodeType := StrToIntDef(GetVal(1, Details), 0);
  NodeConnect := StrToIntDef(GetVal(2, Details), -1);

  U3 := StrToIntDef(GetVal(3, Details), 0);

  Pos[0] := StrToIntDef(GetVal(4, Details), 0);
  Pos[1] := StrToIntDef(GetVal(5, Details), 0);
  Pos[2] := StrToIntDef(GetVal(6, Details), 0);

  U7 := StrToIntDef(GetVal(7, Details), 0);

  LaneLeft := StrToIntDef(GetVal(8, Details), 0);
  LaneRight := StrToIntDef(GetVal(9, Details), 0);

  end;

  except
    on E: Exception do
    begin
      Dec(RCount);
      SetLength(Item, RCount);
    end;
  end;

  end;
end;

constructor TGTASPath.Create;
begin
  SetLength(Item, 0);
  Exist := False;
  Count := 0;
end;

destructor TGTASPath.Destroy;
var
  I: LongInt;
begin
  for I := 0 to Count - 1 do
    Finalize(Item[I].Item);
  Finalize(Item);
  inherited Destroy;
end;

function TGTASPath.AddItem: LongWord;
var
  I: LongWord;
begin
  Inc(Count);
  SetLength(Item, Count);
  Exist := True;

  with Item[Count - 1] do
  begin
    // defaults here
    PathType := 'ped';

    ID := 0;
    ModelName := '';

    RCount := PATH_COUNT;
    for I := 0 to RCount - 1 do
      AddRoute(Count - 1, '0, -1, 0, 0, 0, 0, 0, 0, 0');
  end;

  Result := Count - 1;
end;

procedure TGTASPath.DeleteItem(Num: LongWord);
var
  I: LongInt;
begin
  Dec(Count);
  if (Count - 1 > Num) then for I := Num to Count - 1 do
    Item[I] := Item[I+1];
  SetLength(Item, Count);
end;

// *** TGTAS Path - IPL

procedure TGTASPathIPL.AddRoute(Num: LongWord; Details: String);
begin
  with Item[Num] do
  begin

  Inc(RCount);
  SetLength(Item, RCount);

  try

  with Item[RCount - 1] do
  begin

  NodeType := StrToIntDef(GetVal(1, Details), 0);
  NodeConnect := StrToIntDef(GetVal(2, Details), -1);

  U3 := StrToIntDef(GetVal(3, Details), 0);

  Pos[0] := StrToFloatDef(GetVal(4, Details), 0);
  Pos[1] := StrToFloatDef(GetVal(5, Details), 0);
  Pos[2] := StrToFloatDef(GetVal(6, Details), 0);

  U7 := StrToFloatDef(GetVal(7, Details), 0);

  LaneLeft := StrToIntDef(GetVal(8, Details), 0);
  LaneRight := StrToIntDef(GetVal(9, Details), 0);

  U10 := StrToIntDef(GetVal(10, Details), 0);
  U11 := StrToIntDef(GetVal(11, Details), 0);
  U12 := StrToIntDef(GetVal(12, Details), 0);

  end;

  except
    on E: Exception do
    begin
      Dec(RCount);
      SetLength(Item, RCount);
    end;
  end;

  end;
end;

constructor TGTASPathIPL.Create;
begin
  SetLength(Item, 0);
  Exist := False;
  Count := 0;
end;

destructor TGTASPathIPL.Destroy;
var
  I: LongInt;
begin
  for I := 0 to Count - 1 do
    Finalize(Item[I].Item);
  Finalize(Item);
  inherited Destroy;
end;

function TGTASPathIPL.AddItem: LongWord;
var
  I: LongWord;
begin
  Inc(Count);
  SetLength(Item, Count);
  Exist := True;

  with Item[Count - 1] do
  begin
    // defaults here
    PathType := 0;
    PathOther := -1;

    RCount := PATH_IPL_COUNT;
    for I := 0 to RCount - 1 do
      AddRoute(Count - 1, '0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1');
  end;

  Result := Count - 1;
end;

procedure TGTASPathIPL.DeleteItem(Num: LongWord);
var
  I: LongInt;
begin
  Dec(Count);
  if (Count - 1 > Num) then for I := Num to Count - 1 do
    Item[I] := Item[I+1];
  SetLength(Item, Count);
end;

// *** TGTAS 2dfx

constructor TGTAS2dfx.Create;
begin
  SetLength(Item, 0);
  Count := 0;
  Exist := False;
end;

destructor TGTAS2dfx.Destroy;
begin
  Finalize(Item);
  inherited Destroy;
end;

function TGTAS2dfx.AddItem: LongWord;
begin
  Inc(Count);
  SetLength(Item, Count);

  Exist := True;

  with Item[Count - 1] do
  begin
    // defaults here
    Color[0] := $FF;
    Color[1] := $FF;
    Color[2] := $FF;

    ViewDistance := 200;

    EffectType := EFFECT_LIGHT;

    AEffect1 := '"coronastar"';
    AEffect2 := '"shad_exp"';

    ADistance := 100;
    ARangeOuter := 8;
    ASizeLamp := 1;
    ARangeInner := 0;
    ASizeCorona := 40;
    AControl := 0;
    AReflectionWet := 0;
    ALensFlare := 0;
    ADust := 0;

    BType := 0;
    BRotation[0] := 0;
    BRotation[1] := 0;
    BRotation[2] := 0;
    BRotation[3] := 1;

    DType := 0;
    DDirection1[0] := 0;
    DDirection1[1] := 1;
    DDirection1[2] := 0;
    DDirection2[0] := 0;
    DDirection2[1] := 1;
    DDirection2[2] := 0;
  end;

  Result := Count - 1;
end;

procedure TGTAS2dfx.DeleteItem(Num: LongWord);
var
  I: LongInt;
begin
  Dec(Count);
  if (Count - 1 > Num) then for I := Num to Count - 1 do
    Item[I] := Item[I+1];
  SetLength(Item, Count);
end;

// *** TGTAS Cull

constructor TGTASCull.Create;
begin
  SetLength(Item, 0);
  Exist := False;
  Count := 0;
end;

destructor TGTASCull.Destroy;
begin
  Finalize(Item);
  inherited Destroy;
end;

function TGTASCull.AddItem: LongWord;
begin
  Inc(Count);
  SetLength(Item, Count);
  Exist := True;

  with Item[Count - 1] do
  begin
    // defaults here
    Pos1[0] := 0; Pos1[1] := 0; Pos1[2] := 0;
    Pos2[0] := 0; Pos2[1] := 0; Pos2[2] := 0;
    Pos3[0] := 0; Pos3[1] := 0; Pos3[2] := 0;

    U10 := 40;
    U11 := 0;
  end;

  Result := Count - 1;
end;

procedure TGTASCull.DeleteItem(Num: LongWord);
var
  I: LongInt;
begin
  Dec(Count);
  if (Count - 1 > Num) then for I := Num to Count - 1 do
    Item[I] := Item[I+1];
  SetLength(Item, Count);
end;

// *** TGTAS Zone

constructor TGTASZone.Create;
begin
  SetLength(Item, 0);
  Exist := False;
  Count := 0;
end;

destructor TGTASZone.Destroy;
begin
  Finalize(Item);
  inherited Destroy;
end;

function TGTASZone.AddItem: LongWord;
var
  I: LongInt;
begin
  Inc(Count);
  SetLength(Item, Count);
  Exist := True;

  with Item[Count - 1] do
  begin
    // defaults here
    if (Count > 1) then
      I := StrToIntDef(Copy(Item[Count - 2].ZoneName, 5, 2), -1) + 1
    else
      I := -1;
    if (I = -1) then
      ZoneName := 'Zone01'
    else
      ZoneName := Format('Zone%.2d', [I]);

    Sort := 3;
    Pos1[0] := 0; Pos1[1] := 0; Pos1[2] := 0;
    Pos2[0] := 0; Pos2[1] := 0; Pos2[2] := 0;
    U9 := 1;
  end;

  Result := Count - 1;
end;

procedure TGTASZone.DeleteItem(Num: LongWord);
var
  I: LongInt;
begin
  Dec(Count);
  if (Count - 1 > Num) then for I := Num to Count - 1 do
    Item[I] := Item[I+1];
  SetLength(Item, Count);
end;

// *******
// SUB SECTION HEADERS
// *******

// *** TGTAS InstObj

procedure TGTASInst.LoadItem(Details: String);
begin
  Inc(Count);
  SetLength(Item, Count);

  if not LoadItem(Count - 1, Details) then
  begin
    Dec(Count);
    SetLength(Item, Count);
  end;
end;

function TGTASInst.LoadItem(inNum: LongWord; Details: String): Boolean;
begin
  Result := True;

  try

  with Item[inNum] do
  begin

  ID := StrToIntDef(GetVal(1, Details), -1);
  ModelName := GetVal(2, Details);

  if not (GetVal(13, Details) = '') then
  begin
    HasInterior := True;
    Interior := StrToIntDef(GetVal(3, Details), -1);

    Pos[0] := StrToFloatDef(GetVal(4, Details), 0.0);
    Pos[1] := StrToFloatDef(GetVal(5, Details), 0.0);
    Pos[2] := StrToFloatDef(GetVal(6, Details), 0.0);

    Scale[0] := StrToFloatDef(GetVal(7, Details), 0.0);
    Scale[1] := StrToFloatDef(GetVal(8, Details), 0.0);
    Scale[2] := StrToFloatDef(GetVal(9, Details), 0.0);

    Rotation[0] := StrToFloatDef(GetVal(10, Details), 0.0);
    Rotation[1] := StrToFloatDef(GetVal(11, Details), 0.0);
    Rotation[2] := StrToFloatDef(GetVal(12, Details), 0.0);
    Rotation[3] := StrToFloatDef(GetVal(13, Details), 0.0);
  end else
  begin
    HasInterior := False;
    Interior := 0;

    Pos[0] := StrToFloatDef(GetVal(3, Details), 0.0);
    Pos[1] := StrToFloatDef(GetVal(4, Details), 0.0);
    Pos[2] := StrToFloatDef(GetVal(5, Details), 0.0);

    Scale[0] := StrToFloatDef(GetVal(6, Details), 0.0);
    Scale[1] := StrToFloatDef(GetVal(7, Details), 0.0);
    Scale[2] := StrToFloatDef(GetVal(8, Details), 0.0);

    Rotation[0] := StrToFloatDef(GetVal(9, Details), 0.0);
    Rotation[1] := StrToFloatDef(GetVal(10, Details), 0.0);
    Rotation[2] := StrToFloatDef(GetVal(11, Details), 0.0);
    Rotation[3] := StrToFloatDef(GetVal(12, Details), 0.0);
  end;

  RemFile := -1;
  RemIndex := -1;
  RemTimed := -1;

  end;

  except
    on E: Exception do Result := False;
  end;
end;

function TGTASInst.SaveItem(Num: LongWord): String;
begin
  with Item[Num] do
  begin

  if HasInterior then
    Result := Format('%d, %s, %d, %1.6g, %1.6g, %1.6g, %1.6g, %1.6g, %1.6g, %1.6g, %1.6g, %1.6g, %1.6g',
      [ID, ModelName, Interior, Pos[0], Pos[1], Pos[2], Scale[0], Scale[1], Scale[2], Rotation[0], Rotation[1],  Rotation[2],  Rotation[3]])
  else
    Result := Format('%d, %s, %1.6g, %1.6g, %1.6g, %1.6g, %1.6g, %1.6g, %1.6g, %1.6g, %1.6g, %1.6g',
      [ID, ModelName, Pos[0], Pos[1], Pos[2], Scale[0], Scale[1], Scale[2], Rotation[0], Rotation[1],  Rotation[2],  Rotation[3]]);
  end;
end;

// *** TGTAS ObjsObj

procedure TGTASObjs.LoadItem(Details: String);
begin
  Inc(Count);
  SetLength(Item, Count);

  if not LoadItem(Count - 1, Details) then
  begin
    Dec(Count);
    SetLength(Item, Count);
  end;
end;

function TGTASObjs.LoadItem(inNum: LongWord; Details: String): Boolean;
begin
  Result := True;

  try

  with Item[inNum] do
  begin

  glDisplayList := 0;
  ListAvailable := False;
  DffNum := -1; TextureNum := -1;
  SortVal := 0;
  InUse := False;

  glDisplayList := 0;
  ListAvailable := False;
  DffNum := -1; InUse := False;

  ID := StrToIntDef(GetVal(1, Details), -1);
  ModelName := GetVal(2, Details);

  TextureName := GetVal(3, Details);

  U4 := StrToIntDef(GetVal(4, Details), 1);

  LOD := StrToFloatDef(GetVal(5, Details), 100);

  Flags := StrToIntDef(GetVal(6, Details), 0);

  end;

  CalcArchiveNums(Count - 1);

  except
    on E: Exception do Result := False;
  end;
end;

procedure TGTASObjs.CalcArchiveNums(Num: LongWord);
begin
  with Item[Num] do
  begin
    if (TextureName = '') then
      TextureNum := -1
    else
      TextureNum := Main.GArchive.GetTXDNum(TextureName);
  end;
end;

procedure TGTASObjs.glDrawIndex(inIndex: Word; JustCompile: Boolean; Distance: Single);
var
  Continue: Boolean;
begin
  with Item[inIndex] do
  begin
    //if (Distance < 0) then
    //  Continue := False
    //else
    //  Continue := Distance < LOD;

    Continue := True;

    if Continue then
    begin
      InUse := True;
      if not (TextureNum = -1) then
        GArchive.GTxd[TextureNum].InUse := True;

      if GTA_DISPLAY_LISTS then
      begin
        if not ListAvailable then
        begin
          ListAvailable := True;
          if (DffNum = -1) then
            DffNum := GArchive.GetDFFNum(ModelName);
          if (glDisplayList = 0) then
            glDisplayList := glGenLists(1);
          glNewList(glDisplayList, GL_COMPILE);
          if not (DffNum = -1) then
          begin
            if not GArchive.GDff[DffNum].Loaded then
              GArchive.GDff[DffNum].LoadFromStream;
            GArchive.GDff[DffNum].glDraw(TextureNum);
            GArchive.GDff[DffNum].InUse := True;
            GArchive.GDff[DffNum].Unload;
          end else
            MainGLView.glDrawObject(False);
          glEndList;
          glCallList(glDisplayList);
        end else
        begin
          if not (DffNum = -1) then
            GArchive.GDff[DffNum].InUse := True;
          glCallList(glDisplayList);
        end;
      end else
      begin
        if (DffNum = -1) then
          DffNum := GArchive.GetDFFNum(ModelName);
        if not (DffNum = -1) then
        begin
          if not GArchive.GDff[DffNum].Loaded then
            GArchive.GDff[DffNum].LoadFromStream;
          GArchive.GDff[DffNum].glDraw(TextureNum);
          GArchive.GDff[DffNum].InUse := True;
        end else
          MainGLView.glDrawObject(True);
      end;
    end;
  end;
end;

procedure TGTASObjs.glDrawCompile;
var
  I: LongWord;
begin
  if (Count > 0) then for I := 0 to Count - 1 do
    glDrawIndex(I, True, -1);
end;

procedure TGTASObjs.SetAvailable(in_name: String);
var
  I: LongWord;
begin
  if (Count > 0) then for I := 0 to Count - 1 do
    if (CompareText(in_name, Item[I].ModelName + '.dff') = 0) then
      Item[I].ListAvailable := False
    else if (CompareText(in_name, Item[I].ModelName + '.txd') = 0) then
    begin
      CalcArchiveNums(I);
      Item[I].ListAvailable := False;
    end;
end;

procedure TGTASObjs.SetNotUsed;
var
  I: LongWord;
begin
  if (Count > 0) then for I := 0 to Count - 1 do
    Item[I].InUse := False;
end;

procedure TGTASObjs.DestroyNotUsed;
var
  I: LongWord;
begin
  if (Count > 0) then for I := 0 to Count - 1 do
    if not Item[I].InUse then with Item[I] do
    begin
      ListAvailable := False;
      glNewList(Item[I].glDisplayList, GL_COMPILE);
      glEndList;
    end;
end;

procedure TGTASObjs.CreateList;
var
  I: LongWord;
begin
  if not (ObjectList = nil) then
    ObjectList.Free;
  ObjectList := TList.Create;
  if (Count > 0) then for I := 0 to Count - 1 do
  begin
    Item[I].SortVal := I;
    ObjectList.Add(@Item[I]);
  end;
  ObjectList.Sort(@Validate.CompareObjsItems);
end;

procedure TGTASObjs.DestroyList;
begin
  if not (ObjectList = nil) then
  begin
    ObjectList.Free;
    ObjectList := nil;
  end;
end;

procedure TGTASObjs.glDraw(inID: Word);
var
  I: LongWord;
begin
  I := 0;
  if (Count > 0) then
    while (I < Count) do
    begin
      if (Item[I].ID = inID) then
      begin
        glDrawIndex(I, False, -1);
        I := Count;
      end;
      Inc(I);
    end;
end;

function TGTASObjs.GetLOD(inID: Word): Single;
var
  I: LongWord;
begin
  Result := 0;
  I := 0;
  if (Count > 0) then
    while (I < Count) do
    begin
      if (Item[I].ID = inID) then
      begin
        Result := Item[I].LOD;
        I := Count;
      end;
      Inc(I);
    end;
end;

function TGTASObjs.GetMaxID: LongWord;
var
  I: LongWord;
begin
  Result := 0;
  if (Count > 0) then for I := 0 to Count - 1 do
    if (Item[I].ID > Result) then
      Result := Item[I].ID;
end;

function TGTASObjs.SaveItem(Num: LongWord): String;
begin
  with Item[Num] do
  begin

  Result := IntToStr(ID) + ', ' +
            ModelName + ', ' +
            TextureName + ', ' +
            IntToStr(U4) + ', ' +
            FloatToStr(LOD) + ', ' +
            IntToStr(Flags);
  end;
end;

// *** TGTAS TObjObj

procedure TGTASTObj.LoadItem(Details: String);
begin
  Inc(Count);
  SetLength(Item, Count);

  if not LoadItem(Count - 1, Details) then
  begin
    Dec(Count);
    SetLength(Item, Count);
  end;
end;

function TGTASTObj.LoadItem(inNum: LongWord; Details: String): Boolean;
begin
  Result := True;

  try

  with Item[Count - 1] do
  begin

  glDisplayList := 0;
  ListAvailable := False;
  DffNum := -1; TextureNum := -1;
  SortVal := 0;
  InUse := False;

  ID := StrToIntDef(GetVal(1, Details), -1);
  ModelName := GetVal(2, Details);

  TextureName := GetVal(3, Details);

  U4 := StrToInt(GetVal(4, Details));

  LOD := StrToInt(GetVal(5, Details));

  Flags := StrToInt(GetVal(6, Details));

  TimeOn := StrToInt(GetVal(7, Details));

  TimeOff := StrToInt(GetVal(8, Details));

  end;

  CalcArchiveNums(Count - 1);

  except
    on E: Exception do Result := False;
  end;
end;

procedure TGTASTObj.CalcArchiveNums(Num: LongWord);
begin
  with Item[Num] do
  begin
    if (TextureName = '') then
      TextureNum := -1
    else
      TextureNum := Main.GArchive.GetTXDNum(TextureName);
  end;
end;

procedure TGTASTObj.glDrawIndex(inIndex: Word; JustCompile: Boolean; Distance: Single);
var
  Continue: Boolean;
begin
  with Item[inIndex] do
  begin
    //if (Distance < 0) then
    //  Continue := False
    //else
    //  Continue := Distance < LOD;

    Continue := True;

    if Continue then
    begin
      InUse := True;
      if not (TextureNum = -1) then
        GArchive.GTxd[TextureNum].InUse := True;

      if GTA_DISPLAY_LISTS then
      begin
        if not ListAvailable then
        begin
          ListAvailable := True;
          if (DffNum = -1) then
            DffNum := GArchive.GetDFFNum(ModelName);
          if (glDisplayList = 0) then
            glDisplayList := glGenLists(1);
          glNewList(glDisplayList, GL_COMPILE);
          if not (DffNum = -1) then
          begin
            if not GArchive.GDff[DffNum].Loaded then
              GArchive.GDff[DffNum].LoadFromStream;
            GArchive.GDff[DffNum].glDraw(TextureNum);
            GArchive.GDff[DffNum].InUse := True;
            GArchive.GDff[DffNum].Unload;
          end else
            MainGLView.glDrawObject(False);
          glEndList;
          glCallList(glDisplayList);
        end else
        begin
          if not (DffNum = -1) then
            GArchive.GDff[DffNum].InUse := True;
          glCallList(glDisplayList);
        end;
      end else
      begin
        if (DffNum = -1) then
          DffNum := GArchive.GetDFFNum(ModelName);
        if not (DffNum = -1) then
        begin
          if not GArchive.GDff[DffNum].Loaded then
            GArchive.GDff[DffNum].LoadFromStream;
          GArchive.GDff[DffNum].glDraw(TextureNum);
          GArchive.GDff[DffNum].InUse := True;
        end else
          MainGLView.glDrawObject(True);
      end;
    end;
  end;
end;

procedure TGTASTObj.glDrawCompile;
var
  I: LongWord;
begin
  if (Count > 0) then for I := 0 to Count - 1 do
    glDrawIndex(I, True, -1);
end;

procedure TGTASTObj.SetAvailable(in_name: String);
var
  I: LongWord;
begin
  if (Count > 0) then for I := 0 to Count - 1 do
    if (CompareText(in_name, Item[I].ModelName + '.dff') = 0) then
      Item[I].ListAvailable := False
    else if (CompareText(in_name, Item[I].ModelName + '.txd') = 0) then
    begin
      CalcArchiveNums(I);
      Item[I].ListAvailable := False;
    end;
end;

procedure TGTASTObj.SetNotUsed;
var
  I: LongWord;
begin
  if (Count > 0) then for I := 0 to Count - 1 do
    Item[I].InUse := False;
end;

procedure TGTASTObj.DestroyNotUsed;
var
  I: LongWord;
begin
  if (Count > 0) then for I := 0 to Count - 1 do
    if not Item[I].InUse then with Item[I] do
    begin
      ListAvailable := False;
      glNewList(Item[I].glDisplayList, GL_COMPILE);
      glEndList;
    end;
end;

procedure TGTASTObj.CreateList;
var
  I: LongWord;
begin
  if not (ObjectList = nil) then
    ObjectList.Free;
  ObjectList := TList.Create;
  if (Count > 0) then for I := 0 to Count - 1 do
  begin
    Item[I].SortVal := I;
    ObjectList.Add(@Item[I]);
  end;
  ObjectList.Sort(@Validate.CompareTObjItems);
end;

procedure TGTASTObj.DestroyList;
begin
  if not (ObjectList = nil) then
  begin
    ObjectList.Free;
    ObjectList := nil;
  end;
end;

procedure TGTASTObj.glDraw(inID: Word);
var
  I: LongWord;
begin
  I := 0;
  if (Count > 0) then
    while (I < Count) do
    begin
      if (Item[I].ID = inID) then
      begin
        glDrawIndex(I, False, -1);
        I := Count;
      end;
      Inc(I);
    end;
end;

function TGTASTObj.GetLOD(inID: Word): Single;
var
  I: LongWord;
begin
  Result := 0;
  I := 0;
  if (Count > 0) then
    while (I < Count) do
    begin
      if (Item[I].ID = inID) then
      begin
        Result := Item[I].LOD;
        I := Count;
      end;
      Inc(I);
    end;
end;

function TGTASTObj.GetMaxID: LongWord;
var
  I: LongWord;
begin
  Result := 0;
  if (Count > 0) then for I := 0 to Count - 1 do
    if (Item[I].ID > Result) then
      Result := Item[I].ID;
end;

function TGTASTObj.SaveItem(Num: LongWord): String;
begin
  with Item[Num] do
  begin

  Result := IntToStr(ID) + ', ' +
            ModelName + ', ' +
            TextureName + ', ' +
            IntToStr(U4) + ', ' +
            IntToStr(LOD) + ', ' +
            IntToStr(Flags) + ', ' +
            IntToStr(TimeOn) + ', ' +
            IntToStr(TimeOff);
  end;
end;

// *** TGTAS PathObj

procedure TGTASPath.LoadItem(Details: String);
begin
  Inc(Count);
  SetLength(Item, Count);

  if not LoadItem(Count - 1, Details) then
  begin
    Dec(Count);
    SetLength(Item, Count);
  end;
end;

function TGTASPath.LoadItem(inNum: LongWord; Details: String): Boolean;
begin
  Result := True;

  try

  with Item[inNum] do
  begin

  PathType := GetVal(1, Details);

  ID := StrToIntDef(GetVal(2, Details), -1);
  ModelName := GetVal(3, Details);

  RCount := 0;
  SetLength(Item, 0);

  end;

  except
    on E: Exception do Result := False;
  end;
end;

function TGTASPath.SaveItem(Num: LongWord): String;
var
  I: LongWord;
begin
  with Item[Num] do
  begin

  Result := Format('%s, %d, %s',
   [PathType, ID, ModelName]);

  if (Count > 0) then for I := 0 to Count - 1 do with Item[I] do
  begin
    Result := Result + #13#10#9 + Format('%d, %d, %d, %d, %d, %d, %d, %d, %d',
     [NodeType, NodeConnect, U3, Pos[0], Pos[1], Pos[2], U7, LaneLeft, LaneRight]);
  end;

  end;
end;

// *** TGTAS PathObj - IPL

procedure TGTASPathIPL.LoadItem(Details: String);
begin
  Inc(Count);
  SetLength(Item, Count);

  if not LoadItem(Count - 1, Details) then
  begin
    Dec(Count);
    SetLength(Item, Count);
  end;
end;

function TGTASPathIPL.LoadItem(inNum: LongWord; Details: String): Boolean;
begin
  Result := True;

  try

  with Item[inNum] do
  begin

  PathType := StrToIntDef(GetVal(1, Details), 0);
  PathOther := StrToIntDef(GetVal(2, Details), -1);

  RCount := 0;
  SetLength(Item, 0);

  end;

  except
    on E: Exception do Result := False;
  end;
end;

function TGTASPathIPL.SaveItem(Num: LongWord): String;
var
  I: LongWord;
begin
  with Item[Num] do
  begin

  Result := Format('%d, %d',
   [PathType, PathOther]);

  if (Count > 0) then for I := 0 to Count - 1 do with Item[I] do
  begin
    Result := Result + #13#10#9 + Format('%d, %d, %d, %1.6g, %1.6g, %1.6g, %1.6g, %d, %d, %d, %d, %d',
     [NodeType, NodeConnect, U3, Pos[0], Pos[1], Pos[2], U7, LaneLeft, LaneRight, U10, U11, U12]);
  end;

  end;
end;

// *** TGTAS 2dfxObj

procedure TGTAS2dfx.LoadItem(Details: String);
begin
  Inc(Count);
  SetLength(Item, Count);

  if not LoadItem(Count - 1, Details) then
  begin
    Dec(Count);
    SetLength(Item, Count);
  end;
end;

function TGTAS2dfx.LoadItem(inNum: LongWord; Details: String): Boolean;
begin
  Result := True;

  try

  with Item[inNum] do
  begin

  ID := StrToIntDef(GetVal(1, Details), -1);

  Pos[0] := StrToFloat(GetVal(2, Details));
  Pos[1] := StrToFloat(GetVal(3, Details));
  Pos[2] := StrToFloat(GetVal(4, Details));

  Color[0] := StrToInt(GetVal(5, Details));
  Color[1] := StrToInt(GetVal(6, Details));
  Color[2] := StrToInt(GetVal(7, Details));

  ViewDistance := StrToInt(GetVal(8, Details));

  EffectType := StrToInt(GetVal(9, Details));

  case EffectType of
    EFFECT_LIGHT:
    begin
      AEffect1 := GetVal(10, Details);
      AEffect2 := GetVal(11, Details);
      ADistance := StrToInt(GetVal(12, Details));
      ARangeOuter := StrToFloat(GetVal(13, Details));
      ASizeLamp := StrToFloat(GetVal(14, Details));
      ARangeInner := StrToFloat(GetVal(15, Details));
      ASizeCorona := StrToFloat(GetVal(16, Details));
      AControl := StrToInt(GetVal(17, Details));
      AReflectionWet := StrToInt(GetVal(18, Details));
      ALensFlare := StrToInt(GetVal(19, Details));
      ADust := StrToInt(GetVal(20, Details));
    end;

    EFFECT_PARTICLE:
    begin
      BType := StrToInt(GetVal(10, Details));
      BRotation[0] := StrToFloat(GetVal(11, Details));
      BRotation[1] := StrToFloat(GetVal(12, Details));
      BRotation[2] := StrToFloat(GetVal(13, Details));
      BRotation[3] := StrToFloat(GetVal(14, Details));
    end;

    EFFECT_UNKNOWN:
    begin
    end;

    EFFECT_ANIMATION:
    begin
      DType := StrToInt(GetVal(10, Details));
      DDirection1[0] := StrToFloat(GetVal(11, Details));
      DDirection1[1] := StrToFloat(GetVal(12, Details));
      DDirection1[2] := StrToFloat(GetVal(13, Details));
      DDirection2[0] := StrToFloat(GetVal(11, Details));
      DDirection2[1] := StrToFloat(GetVal(12, Details));
      DDirection2[2] := StrToFloat(GetVal(13, Details));
    end;

    EFFECT_REFLECTION:
    begin
    end;
  end;

  end;

  except
    on E: Exception do Result := False;
  end;
end;

function TGTAS2dfx.SaveItem(Num: LongWord): String;
begin
  with Item[Num] do
  begin

  Result := Format('%d, %1.6g, %1.6g, %1.6g, %d, %d, %d, %d, %d, ',
    [ID, Pos[0], Pos[1], Pos[2], Color[0], Color[1], Color[2], ViewDistance, EffectType]);

  case EffectType of
    EFFECT_LIGHT:
    begin
      Result := Result + Format('%s, %s, %d, %1.6g, %1.6g, %1.6g, %1.6g, %d, %d, %d, %d',
        [AEffect1, AEffect2, ADistance, ARangeOuter, ASizeLamp, ARangeInner, ASizeCorona, AControl, AReflectionWet, ALensFlare, ADust]);
    end;

    EFFECT_PARTICLE:
    begin
      Result := Result + Format('%d, %1.6g, %1.6g, %1.6g, %1.6g',
        [BType, BRotation[0], BRotation[1], BRotation[2], BRotation[3]]);
    end;

    EFFECT_UNKNOWN:
    begin
    end;

    EFFECT_ANIMATION:
    begin
      Result := Result + Format('%d, %1.6g, %1.6g, %1.6g, %1.6g',
        [DType, DDirection1[0], DDirection1[1], DDirection1[2], DDirection2[0], DDirection2[1], DDirection2[2]]);
    end;

    EFFECT_REFLECTION:
    begin
    end;

  end;

  end;
end;

// *** TGTAS CullObj

procedure TGTASCull.LoadItem(Details: String);
begin
  Inc(Count);
  SetLength(Item, Count);

  if not LoadItem(Count - 1, Details) then
  begin
    Dec(Count);
    SetLength(Item, Count);
  end;
end;

function TGTASCull.LoadItem(inNum: LongWord; Details: String): Boolean;
begin
  Result := True;
  
  try

  with Item[inNum] do
  begin

  Pos1[0] := StrToFloat(GetVal(1, Details));
  Pos1[1] := StrToFloat(GetVal(2, Details));
  Pos1[2] := StrToFloat(GetVal(3, Details));

  Pos2[0] := StrToFloat(GetVal(4, Details));
  Pos2[1] := StrToFloat(GetVal(5, Details));
  Pos2[2] := StrToFloat(GetVal(6, Details));

  Pos3[0] := StrToFloat(GetVal(7, Details));
  Pos3[1] := StrToFloat(GetVal(8, Details));
  Pos3[2] := StrToFloat(GetVal(9, Details));

  U10 := StrToInt(GetVal(10, Details));
  U11 := StrToInt(GetVal(11, Details));

  end;

  except
    on E: Exception do Result := False;
  end;
end;

function TGTASCull.SaveItem(Num: LongWord): String;
begin
  with Item[Num] do
  begin

  Result := Format('%1.6g, %1.6g, %1.6g, %1.6g, %1.6g, %1.6g, %1.6g, %1.6g, %1.6g, %d, %d',
    [Pos1[0], Pos1[1], Pos1[2], Pos2[0], Pos2[1], Pos2[2], Pos3[0], Pos3[1], Pos3[2], U10, U11]);

  end;
end;

// *** TGTAS ZoneObj

procedure TGTASZone.LoadItem(Details: String);
begin
  Inc(Count);
  SetLength(Item, Count);

  if not LoadItem(Count - 1, Details) then
  begin
    Dec(Count);
    SetLength(Item, Count);
  end;
end;

function TGTASZone.LoadItem(inNum: LongWord; Details: String): Boolean;
begin
  Result := True;

  try

  with Item[inNum] do
  begin

  ZoneName := GetVal(1, Details);

  Sort := StrToInt(GetVal(2, Details));

  Pos1[0] := StrToFloat(GetVal(3, Details));
  Pos1[1] := StrToFloat(GetVal(4, Details));
  Pos1[2] := StrToFloat(GetVal(5, Details));

  Pos2[0] := StrToFloat(GetVal(6, Details));
  Pos2[1] := StrToFloat(GetVal(7, Details));
  Pos2[2] := StrToFloat(GetVal(8, Details));

  U9 := StrToInt(GetVal(9, Details));

  end;

  except
    on E: Exception do
    begin
      Dec(Count);
      SetLength(Item, Count);
    end;
  end;
end;

function TGTASZone.SaveItem(Num: LongWord): String;
begin
  with Item[Num] do
  begin

  Result := Format('%s, %d, %1.6g, %1.6g, %1.6g, %1.6g, %1.6g, %1.6g, %d',
    [ZoneName, Sort, Pos1[0], Pos1[1], Pos1[2], Pos2[0], Pos2[1], Pos2[2], U9]);

  end;
end;

// *******
// REQUIRED TOOLS
// *******

function GetVal(Num: Integer; Str: String): String;
var
  I, CC, CS, CF: Integer;
begin
  Result := '';

  CC := 1;
  if (Num = 1) then
    CS := 0
  else
    CS := -1;
  CF := -1;

  I := 1;
  if (Length(Str) > 0) then while I < Length(Str) + 1 do
  begin
    if Str[I] = ',' then
    begin
      Inc(CC);
      if (CC = Num) then
        CS := I
      else if (CC - 1 = Num) then
      begin
        CF := I;
        I := Length(Str) + 1;
      end;
    end;
    Inc(I);
  end;
  if (CF = -1) then
    CF := Length(Str) + 1;
  CF := CF - CS - 1;
  if not (CS = -1) and (CF > 0) then
  begin
    SetLength(Result, CF);
    CopyMemory(@Result[1], @Str[CS + 1], CF);
  end;
  Result := Trim(Result);
end;

end.
