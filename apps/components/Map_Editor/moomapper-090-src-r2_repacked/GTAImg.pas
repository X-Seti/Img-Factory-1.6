unit GTAImg;

interface

uses Classes, SysUtils, StrUtils, GTADff, GTATxd, Math, Dialogs;

const
  GDIRFile     = 'models\gta3.dir';
  GIMGFile     = 'models\gta3.img';
  TDIRFile     = 'models\txd.dir';
  TIMGFile     = 'models\txd.img';
  GenericTXD   = 'models\generic.txd';

  FILE_UNKNOWN = 0;
  FILE_DFF     = 1;
  FILE_TXD     = 2;

  SORT_ASCENDING  = 0;
  SORT_DESCENDING = 1;

type
  TDirEntry = packed record
    StartBlock: Longword;
    BlockCount: Longword;
    Name: array[0..23] of Char;
    Index: Integer;
    Delete: Boolean;
  end;
  PDirEntry = ^TDirEntry;

  TGTA3Archive = class
  private
    FFileName: string;
    FEntries: TList;
    function GetEntry(Index: Integer): TDirEntry;
  protected
    FDir: TFileStream;
    FImg: TFileStream;
  public
    ArchiveOpened: Boolean;
    ArchiveList: TStringList;
    ReadOnly: Boolean;

    constructor Create(FileName: string; CreateNew: Boolean = False; OnlyTextures: Boolean = False);
    destructor Destroy; override;
    procedure CreateList;
    procedure DestroyList;
    procedure Extract(Index: Integer; Stream: TStream);
    procedure Add(FileName: string; Stream: TStream); virtual;
    procedure Rename(Index: Integer; NewName: string);
    procedure MarkForRemoval(Index: Integer);
    procedure DoRemove;
    procedure Sort(Compare: TListSortCompare);
    function EntryCount: Integer;
    property Entry[Index: Integer]: TDirEntry read GetEntry;
    property ArchiveName: string read FFileName;
  end;

  TGTAImg = class(TGTA3Archive)
  private
    TxdList: TStringList;
    DffList: TStringList;

    procedure LoadAllFromArchive;
    procedure LoadFromArchive(DoDff, DoTxd: Boolean; InImg: TGTA3Archive);
  public
    TxdImg: TGTA3Archive;

    GTxd: array of TGTATxd;
    GTxdCount: LongInt;

    GDff: array of TGTADff;
    GDffCount: LongInt;

    KillNotUsed: Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure CreateTxdList;
    procedure DestroyTxdList;

    procedure CreateDffList;
    procedure DestroyDffList;

    procedure SetNotUsed;
    procedure DestroyNotUsed;

    procedure Add(FileName: string; Stream: TStream); override;

    procedure glDraw(in_name: String; in_txd: LongInt);
    function GetDFFNum(in_name: String): LongInt;
    function GetTXDNum(in_name: String): LongInt;
    function GetEntryNum(in_name: String): LongInt;

    function DffExists(in_name: String): Boolean;
    function TxdExists(in_name: String): Boolean;
  end;

implementation

uses Main, GLView;

// gta archiving

constructor TGTA3Archive.Create(FileName: string; CreateNew, OnlyTextures: Boolean);
var
  Entry: PDirEntry;
  Mode: Word;
  Continue: Boolean;
begin
  if CreateNew then
    Mode := fmCreate
  else
    Mode := fmOpenReadWrite;

  FFileName := FileName;
  FEntries := TList.Create;
  Continue := True;
  ReadOnly := False;

  try
    FImg := TFileStream.Create(FileName, Mode);
    FDir := TFileStream.Create(ChangeFileExt(FileName, '.dir'), Mode);
  except
    on E: Exception do
    begin
      ReadOnly := True;
      Mode := fmShareDenyNone;
      try
        FImg := TFileStream.Create(FileName, Mode);
        FDir := TFileStream.Create(ChangeFileExt(FileName, '.dir'), Mode);
      except
        on E: Exception do
        begin
          if not OnlyTextures then
            GTA_MODEL_MODE := False;
          GTA_TEXTURE_MODE := False;
          GTA_TEXTURE_WHEN_NEEDED := False;

          // commented out till i figure out how to display messages whilst loading
          //MessageDlg('An error occurred opening the GTA3.DIR and GTA3.IMG files.' + #10#13 +
          //           'Please check these files are not in use or marked as read only!', mtError, [mbOk], 0);
          
          Continue := False;
        end;
      end;
    end;
  end;

  if Continue then
  begin
    FEntries.Capacity := FDir.Size div 32 + 1;
    while FDir.Position + 31 < FDir.Size do
    begin
      New(Entry);
      FDir.Read(Entry^, 32);
      Entry^.Index := FEntries.Count;
      Entry^.Delete := False;
      FEntries.Add(Entry)
    end;
  end;

  ArchiveOpened := Continue;
  CreateList;
end;

destructor TGTA3Archive.Destroy;
begin
  DestroyList;
  FDir.Free;
  FImg.Free;
  while FEntries.Count > 0 do
  begin
    Dispose(FEntries.Items[0]);
    FEntries.Delete(0);
  end;
  FEntries.Free;
end;

function TGTA3Archive.GetEntry(Index: Integer): TDirEntry;
begin
  Result := TDirEntry(FEntries.Items[Index]^);
end;

function TGTA3Archive.EntryCount: Integer;
begin
  if ArchiveOpened then
    Result := FEntries.Count
  else
    Result := 0;
end;

procedure TGTA3Archive.Sort(Compare: TListSortCompare);
begin
  FEntries.Sort(Compare);
end;

procedure TGTA3Archive.Extract(Index: Integer; Stream: TStream);
begin
  with Entry[Index] do
  begin
    FImg.Position := StartBlock * 2048;
    if BlockCount > 0 then
      Stream.CopyFrom(FImg, BlockCount * 2048);
  end;
end;

procedure TGTA3Archive.Add(FileName: string; Stream: TStream);
var
  Entry: PDirEntry;
  Old: Int64;
begin
  New(Entry);
  Entry^.BlockCount := Stream.Size div 2048;
  if Stream.Size mod 2048 <> 0 then
    Inc(Entry^.BlockCount);

  FImg.Seek(0, soFromEnd);
  FDir.Seek(0, soFromEnd);
  Old := FImg.Size;

  Entry^.StartBlock := FImg.Position div 2048;
  FImg.Size := FImg.Size + Entry^.BlockCount * 2048;
  FImg.Position := Old;
  Entry^.Delete := False;
  Entry^.Index := FDir.Position div 32;
  FillChar(Entry^.Name, 24, 0);
  Move(FileName[1], Entry^.Name, Min(24, Length(FileName)));
  try
    if Stream.Size > 0 then
    begin
      FImg.CopyFrom(Stream, Stream.Size);
    end;

    FDir.Write(Entry^, 32);
    FEntries.Add(Entry);

  except
    FImg.Size := Old;
    raise;
  end;
  CreateList;
  GFiles.SetAvailable(FileName);
end;

procedure TGTA3Archive.Rename(Index: Integer; NewName: string);
begin
  FillChar(TDirEntry(FEntries.Items[Index]^).Name, 24, 0);
  Move(NewName[1], TDirEntry(FEntries.Items[Index]^).Name, Min(24, Length(NewName)));
  FDir.Position := Entry[Index].Index * 32;
  FDir.Write(FEntries.Items[Index]^, 32);
  CreateList;
end;

procedure TGTA3Archive.MarkForRemoval(Index: Integer);
begin
  TDirEntry(FEntries.Items[Index]^).Delete := True;
end;

procedure TGTA3Archive.DoRemove;
var i: Integer;
begin
  for i := FEntries.Count -1 downto 0 do
  begin
    if TDirEntry(FEntries.Items[i]^).Delete then
    begin
      GFiles.SetAvailable(Trim(TDirEntry(FEntries.Items[i]^).Name));
      FEntries.Delete(i);
    end;
  end;
  FDir.Size := FEntries.Count * 32;
  FDir.Position := 0;
  for i := 0 to FEntries.Count -1 do
  begin
    TDirEntry(FEntries.Items[i]^).Index := i;
    FDir.Write(FEntries.Items[i]^, 32);
  end;
  CreateList;
end;

// sort functions

function CompareInt(Int1, Int2: Integer):integer;
begin
  if Int1 > Int2 then
    result := 1
  else
    if Int1 = Int2 then
      result := 0
    else
      result := -1;
end;

// ali functions

procedure TGTAImg.Add(FileName: string; Stream: TStream);
var
  I: Integer;
begin
  I := EntryCount;
  inherited;
  if (EntryCount > I) and (CompareText(ExtractFileExt(Trim(FileName)), '.txd') = 0) and Assigned(glCompressedTexImage2DARB) then with Entry[EntryCount - 1] do
  begin
    Inc(GTxdCount);
    SetLength(GTxd, GTxdCount);
    GTxd[GTxdCount - 1] := TGTATxd.Create(FImg, Trim(FileName), StartBlock * 2048, BlockCount * 2048);
    CreateTxdList;
  end;
  if (EntryCount > I) and (CompareText(ExtractFileExt(Trim(FileName)), '.dff') = 0) then with Entry[EntryCount - 1] do
  begin
    Inc(GDffCount);
    SetLength(GDff, GDffCount);
    GDff[GDffCount - 1] := TGTADff.Create(FImg, Trim(FileName), StartBlock * 2048, BlockCount * 2048);
    CreateDffList;
  end;
end;

procedure TGTA3Archive.CreateList;
var
  I: LongWord;
begin
  if not (ArchiveList = nil) then
    ArchiveList.Free;
  ArchiveList := TStringList.Create;
  ArchiveList.CaseSensitive := False;

  if ArchiveOpened then
    if (EntryCount > 0) then for I := 0 to EntryCount - 1 do
      ArchiveList.AddObject(Trim(Entry[I].Name), Pointer(I));
      
  ArchiveList.Sort;
end;

procedure TGTA3Archive.DestroyList;
begin
  if not (ArchiveList = nil) then
  begin
    ArchiveList.Free;
    ArchiveList := nil;
  end;
end;

procedure TGTAImg.CreateTxdList;
var
  I: LongWord;
begin
  if not (TxdList = nil) then
    TxdList.Free;
  TxdList := TStringList.Create;
  TxdList.CaseSensitive := False;
  if (GTxdCount > 0) then for I := 0 to GTxdCount - 1 do
    TxdList.AddObject(Trim(GTxd[I].Name), Pointer(I));
  TxdList.Sort;
end;

procedure TGTAImg.DestroyTxdList;
begin
  if not (TxdList = nil) then
  begin
    TxdList.Free;
    TxdList := nil;
  end;
end;

procedure TGTAImg.CreateDffList;
var
  I: LongWord;
begin
  if not (DffList = nil) then
    DffList.Free;
  DffList := TStringList.Create;
  DffList.CaseSensitive := False;
  if (GDffCount > 0) then for I := 0 to GDffCount - 1 do
    DffList.AddObject(Trim(GDff[I].Name), Pointer(I));
  DffList.Sort;
end;

procedure TGTAImg.DestroyDffList;
begin
  if not (DffList = nil) then
  begin
    DffList.Free;
    DffList := nil;
  end;
end;

constructor TGTAImg.Create;
begin
  inherited Create(GTAPath + GIMGFile, False);
  if not Assigned(glCompressedTexImage2DARB) then
  begin
    if FileExists(GTAPath + TIMGFile) then
      TxdImg := TGTA3Archive.Create(GTAPath + TIMGFile, False, True)
    else
    begin
      GTA_TEXTURE_MODE := False;
      GTA_TEXTURE_WHEN_NEEDED := False;
    end;
  end;
  KillNotUsed := False;
  LoadAllFromArchive;
end;

destructor TGTAImg.Destroy;
var
  I: LongWord;
begin
  inherited Destroy;

  if Assigned(TxdImg) then
    TxdImg.Free;

  DestroyDffList;
  DestroyTxdList;

  if (GTxdCount > 0) then for I := 0 to GTxdCount - 1 do
    FreeAndNil(GTxd[I]);
  SetLength(GTxd, 0);
  if (GDffCount > 0) then for I := 0 to GDffCount - 1 do
    FreeAndNil(GDff[I]);
  SetLength(GDff, 0);
end;


procedure TGTAImg.LoadAllFromArchive;
var
  fs: TFileStream;
begin
  // load generic txd file
  if True then
  begin
    GTxdCount := 1;
    SetLength(GTxd, GTxdCount);

    Main.FormLoading.SetPart('Generic Textures');
    fs := TFileStream.Create(GTAPath + GenericTXD, fmOpenRead);
    GTxd[0] := TGTATxd.Create(fs, 'generic', 0, fs.size);
    GTxd[0].LoadFromStream;
    fs.Free;
  end else
  begin
    GTxdCount := 0;
    SetLength(GTxd, GTxdCount);
  end;

  GDffCount := 0;
  SetLength(GDff, GTxdCount);

  // load all from archive
  if Assigned(glCompressedTexImage2DARB) then
    LoadFromArchive(True, True, Self)
  else
  begin
    LoadFromArchive(True, False, Self);
    if Assigned(TxdImg) then
      LoadFromArchive(False, True, TxdImg);
  end;

  CreateTxdList;
  CreateDffList;
end;

procedure TGTAImg.LoadFromArchive(DoDff, DoTxd: Boolean; InImg: TGTA3Archive);
var
  GTxdUpto, GDffUpto, I: LongInt;
  FileType: array of Byte;
begin
  // set current state
  GTxdUpto := GTxdCount;
  GDffUpto := GDffCount;
  SetLength(FileType, InImg.EntryCount);

  // load archive contents
  Main.FormLoading.SetPart('Parsing Archive Contents');

  if (InImg.EntryCount > 0) then for I := 0 to InImg.EntryCount - 1 do with InImg.Entry[I] do
  begin
    FileType[I] := FILE_UNKNOWN;
    if DoTxd and (CompareText('.txd', ExtractFileExt(Trim(Name))) = 0) then
    begin
      FileType[I] := FILE_TXD;
      Inc(GTxdCount);
    end;
    if DoDff and (CompareText('.dff', ExtractFileExt(Trim(Name))) = 0) then
    begin
      FileType[I] := FILE_DFF;
      Inc(GDffCount);
    end;
  end;

  // set bar max
  I := 0;
  if DoTxd then I := I + (GTxdCount - GTxdUpto);
  if DoDff then I := I + (GDffCount - GDffUpto);
  Main.FormLoading.SetPartMax(I);

  // load txd and dff files
  if (InImg.EntryCount > 0) then
  begin
    if DoTxd then
      SetLength(GTxd, GTxdCount);
    if DoDff then
      SetLength(GDff, GDffCount);

    for I := 0 to InImg.EntryCount - 1 do with InImg.Entry[I] do
    begin
      if DoTxd and (FileType[I] = FILE_TXD) then
      begin
        Main.FormLoading.SetPart(Trim(Name));
        GTxd[GTxdUpto] := TGTATxd.Create(InImg.FImg, Trim(Name), StartBlock * 2048, BlockCount * 2048);
        Main.FormLoading.IncPartPos;

        Inc(GTxdUpto);
      end;
      if DoDff and (FileType[I] = FILE_DFF) then
      begin
        Main.FormLoading.SetPart(Trim(Name));
        GDff[GDffUpto] := TGTADff.Create(InImg.FImg, Trim(Name), StartBlock * 2048, BlockCount * 2048);
        Main.FormLoading.IncPartPos;

        Inc(GDffUpto);
      end;
    end;
  end;
end;

procedure TGTAImg.glDraw(in_name: String; in_txd: LongInt);
var
  Index: LongInt;
begin
{  J, Start, Size: LongWord;
  Found: Boolean;
  LoadDff: TGTADff;
begin
  Found := False; Start := 0; Size := 0;
  if (ArchiveList = nil) then
  begin
    I := 0;
    if (EntryCount > 0) then
      while (I < EntryCount) do
      begin
        if (CompareText(in_name + '.dff', Trim(Entry[I].Name)) = 0) then
        begin
          Start := Entry[I].StartBlock;
          Size := Entry[I].BlockCount;
          Found := True;
          I := (EntryCount);
        end;
        Inc(I);
      end;
  end else
  begin
    Found := ArchiveList.Find(in_name + '.dff', Index);
    if Found then
    begin
      J := LongWord(ArchiveList.Objects[Index]);
      Start := Entry[J].StartBlock;
      Size := Entry[J].BlockCount;
    end;
  end;

  if Found then
  begin
    LoadDFF := TGTADff.Create(FImg, in_name, Start * 2048, Size * 2048);
    LoadDFF.glDraw(in_txd);
    LoadDff.Free;
  end;}

  Index := GetDFFNum(in_name);
  if not (Index = -1) then
  begin
    if not GDff[Index].Loaded then
      GDff[Index].LoadFromStream;
    GDff[Index].glDraw(in_txd);
    GDff[Index].InUse := True;
  end;
end;

function TGTAImg.GetDFFNum(in_name: String): LongInt;
var
  I: LongInt;
begin
  Result := -1;
  I := 0;
  if (DffList = nil) then
  begin
    if (GDffCount > 0) then
      while (I < GDffCount) do
      begin
        if (CompareText(in_name, GDff[I].Name) = 0) then
        begin
          Result := I;
          I := (GDffCount);
        end;
        Inc(I);
      end;
  end else
    if DffList.Find(in_name, I) then
      Result := LongInt(DffList.Objects[I]);
end;

procedure TGTAImg.SetNotUsed;
var
  I: LongWord;
begin
  KillNotUsed := True;
  if (GDffCount > 0) then for I := 0 to GDffCount - 1 do
    GDff[I].InUse := False;
  if (GTxdCount > 0) then for I := 0 to GTxdCount - 1 do
    GTxd[I].InUse := False;
end;

procedure TGTAImg.DestroyNotUsed;
var
  I: LongWord;
begin
  if (GDffCount > 0) then for I := 0 to GDffCount - 1 do
    if not GDff[I].InUse then
      GDff[I].Unload;
  if (GTxdCount > 1) then for I := 1 to GTxdCount - 1 do
    if not GTxd[I].InUse then
      if not ((Length(GTxd[I].Name) = 7) and (GTxd[I].Name[1] = 'r') and (GTxd[I].Name[2] = 'a') and (GTxd[I].Name[3] = 'd') and (GTxd[I].Name[4] = 'a') and (GTxd[I].Name[5] = 'r')) then
        GTxd[I].Unload;
end;

function TGTAImg.GetTXDNum(in_name: String): LongInt;
var
  I: LongInt;
begin
  Result := -1;
  I := 0;
  if (CompareText(in_name, 'generic') = 0) and (GTxdCount > 0) then
    Result := 0
  else if (TxdList = nil) then
  begin
    if (GTxdCount > 0) then
      while (I < GTXDCount) do
      begin
        if (CompareText(in_name, GTxd[I].Name) = 0) then
        begin
          Result := I;
          I := (GTXDCount);
        end;
        Inc(I);
      end;
  end else
    if TxdList.Find(in_name, I) then
      Result := LongInt(TxdList.Objects[I]);
end;

function TGTAImg.GetEntryNum(in_name: String): LongInt;
var
  I, Index: LongInt;
begin
  Result := -1;
  I := 0;
  if (ArchiveList = nil) then
  begin
    if (EntryCount > 0) then
      while (I < EntryCount) do
      begin
        if (CompareText(in_name, Trim(Entry[I].Name)) = 0) then
        begin
          Result := I;
          I := (EntryCount);
        end;
        Inc(I);
      end;
  end else
    if ArchiveList.Find(in_name, Index) then
      Result := LongInt(ArchiveList.Objects[Index]);
end;

function TGTAImg.TxdExists(in_name: String): Boolean;
var
  I, Index: LongInt;
begin
  Result := (CompareText(in_name, 'generic') = 0);
  in_name := ChangeFileExt(in_name, '.txd');
  I := 0;
  if not Result then
  begin
    if (ArchiveList = nil) then
    begin
      if (EntryCount > 0) then
        while (I < EntryCount) do
        begin
          if (CompareText(in_name, Trim(Entry[I].Name)) = 0) then
          begin
            Result := True;
            I := (EntryCount);
          end;
          Inc(I);
        end;
    end else
      Result := ArchiveList.Find(in_name, Index);
  end;
end;

function TGTAImg.DffExists(in_name: String): Boolean;
var
  I, Index: LongInt;
begin
  Result := False;
  in_name := ChangeFileExt(in_name, '.dff');
  I := 0;
  if (ArchiveList = nil) then
  begin
    if (EntryCount > 0) then
      while (I < EntryCount) do
      begin
        if (CompareText(in_name, Trim(Entry[I].Name)) = 0) then
        begin
          Result := True;
          I := (EntryCount);
        end;
        Inc(I);
      end;
  end else
    Result := ArchiveList.Find(in_name, Index);
end;

end.
