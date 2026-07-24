unit Validate;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ClipBrd, ActnList;

const
  VALIDATION_IMAGE_SECTION = 22;

  VALIDATION_IMAGE_UNKNOWN = 24;
  VALIDATION_IMAGE_GOOD = 27;
  VALIDATION_IMAGE_BAD = 26;

type
  TFormValidate = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ListResult: TMemo;
    box_fix: TCheckBox;
    Label3: TLabel;
    BtnStart: TButton;
    ListStatus: TListView;
    Label2: TLabel;
    ModeValue: TLabel;
    Label1: TLabel;
    ResultValue: TLabel;
    BtnCopy: TButton;
    Label4: TLabel;
    procedure BtnStartClick(Sender: TObject);
    procedure ListResultDblClick(Sender: TObject);
    procedure BtnCopyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    MainAction: TAction;
    FirstShow: Boolean;
    Processing: Boolean;
    procedure DoValidation;
  public
    procedure SetAction(inAction: TAction);
    constructor Create(AOwner: TComponent); override;
  end;

function CompareObjsItems(Item1, Item2: Pointer): Integer;
function CompareTObjItems(Item1, Item2: Pointer): Integer;
function ComparePathItems(Item1, Item2: Pointer): Integer;
function BinarySearchObjs(Key: LongWord; InList: TList): LongInt;
function BinarySearchTObj(Key: LongWord; InList: TList): LongInt;

implementation

uses Main, GTAText;

{$R *.dfm}

constructor TFormValidate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Processing := False;
end;

procedure TFormValidate.BtnStartClick(Sender: TObject);
begin
  if not Processing then
    DoValidation;
end;

procedure TFormValidate.DoValidation;
var
  I, J: LongWord;
  Index, Index2: LongInt;
  OutStr: String;
  ListItemSec, ListItemCurrent: TListItem;
  ErrorCount, WarningCount, CurrentCount: LongWord;
  ArchiveList: TStringList;
  ObjsList, TObjList, PathList: TList;

  procedure fix(const desc: string);
  begin
  Main.GFiles.Item[I].Changed:= true;
  Main.FormMain.ActionSaveModified.Enabled := True;
  ListResult.lines[ListResult.lines.Count-1]:= ListResult.lines[ListResult.lines.Count-1] + ' -> ' + desc; 
  end;

begin
  // initialise
  Processing := True;
  BtnStart.Enabled := False;
  ErrorCount := 0;
  WarningCount := 0;
  ListResult.Clear;
  ListStatus.Items.Clear;
  if GTA_VICE_MODE then
    ModeValue.Caption := 'Vice City Mode'
  else
    ModeValue.Caption := 'GTA 3 Mode';
  ResultValue.Caption := 'Please Wait..';

  // do processing
  ArchiveList := TStringList.Create;
  ArchiveList.CaseSensitive := False;
  ArchiveList.Add('generic.txd');
  if (Main.GArchive.EntryCount > 0) then for I := 0 to Main.GArchive.EntryCount - 1 do
    ArchiveList.Add(Trim(Main.GArchive.Entry[I].Name));
  ArchiveList.Sort;

  ObjsList := TList.Create;
  if (Main.GFiles.Count > 0) then for I := 0 to Main.GFiles.Count - 1 do
    if (Main.GFiles.Item[I].SubType = FILE_IDE) and (TIDEFile(Main.GFiles.Item[I]).Objs.Count > 0) then
      with TIDEFile(Main.GFiles.Item[I]).Objs do for J := 0 to Count - 1 do
        ObjsList.Add(@Item[J]);
  ObjsList.Sort(@CompareObjsItems);

  TObjList := TList.Create;
  if (Main.GFiles.Count > 0) then for I := 0 to Main.GFiles.Count - 1 do
    if (Main.GFiles.Item[I].SubType = FILE_IDE) and (TIDEFile(Main.GFiles.Item[I]).TObj.Count > 0) then
      with TIDEFile(Main.GFiles.Item[I]).TObj do for J := 0 to Count - 1 do
        TObjList.Add(@Item[J]);
  TObjList.Sort(@CompareTObjItems);

  PathList := TList.Create;
  if (Main.GFiles.Count > 0) then for I := 0 to Main.GFiles.Count - 1 do
    if (Main.GFiles.Item[I].SubType = FILE_IDE) and (TIDEFile(Main.GFiles.Item[I]).Path.Count > 0) then
      with TIDEFile(Main.GFiles.Item[I]).Path do for J := 0 to Count - 1 do
        PathList.Add(@Item[J]);
  PathList.Sort(@ComparePathItems);

  // img archive
  ListItemSec := ListStatus.Items.Add;
  ListItemSec.Caption := 'Main Archive';
  ListItemSec.ImageIndex := VALIDATION_IMAGE_SECTION;
  ListResult.lines.Add('### Error Report: Main Archive  ###');

  CurrentCount := 0;
  ListItemCurrent := ListStatus.Items.Add;
  ListItemCurrent.Caption := 'Duplicate Entries';
  ListItemCurrent.ImageIndex := VALIDATION_IMAGE_UNKNOWN;
  if (ArchiveList.Count > 2) then for I := 0 to ArchiveList.Count - 2 do
    if (CompareText(ArchiveList.Strings[I], ArchiveList.Strings[I + 1]) = 0) then
    begin
      Inc(CurrentCount);
      Inc(WarningCount);
      ListResult.lines.Add('Warning @ File: ' + ArchiveList.Strings[I] + ' = ' +
                           'Duplicate Entries Found!' +
                           '[-1, -1, ' + IntToStr(Main.GArchive.GetEntryNum(ArchiveList.Strings[I])) + ']');
    end;
  if (CurrentCount = 0) then
    ListItemCurrent.ImageIndex := VALIDATION_IMAGE_GOOD
  else
    ListItemCurrent.ImageIndex := VALIDATION_IMAGE_BAD;

  // objs
  ListItemSec := ListStatus.Items.Add;
  ListItemSec.Caption := 'IDE - Section: Objs';
  ListItemSec.ImageIndex := VALIDATION_IMAGE_SECTION;
  ListResult.lines.Add('### Error Report: IDE - Objs ###');

  CurrentCount := 0;
  ListItemCurrent := ListStatus.Items.Add;
  ListItemCurrent.Caption := 'Duplicate IDs';
  ListItemCurrent.ImageIndex := VALIDATION_IMAGE_UNKNOWN;
  if (ObjsList.Count > 2) then for I := 0 to ObjsList.Count - 2 do
    if (TGTASObjsObj(ObjsList.Items[I]^).ID = TGTASObjsObj(ObjsList.Items[I + 1]^).ID) then
    begin
      Inc(CurrentCount);
      Inc(ErrorCount);
      try
      ListResult.lines.Add(format('Error @ file %s [OBJS] %d = Model: "%s" with :: file %s [OBJS] %d = Model: "%s" Duplicate IDs Found! ', [
      Main.GFiles.Item[I].Name,
      TGTASObjsObj(ObjsList.Items[I]^).ID,
      TGTASObjsObj(ObjsList.Items[I]^).ModelName,

      Main.GFiles.Item[I+1].Name,
      TGTASObjsObj(ObjsList.Items[I + 1]^).ID,
      TGTASObjsObj(ObjsList.Items[I + 1]^).ModelName

      ]));
      except
      // for some reason this causes vierd errors in my myriad installation :/
//      ListResult.lines.Add('unknown error that happens alaways here at the same time: ' + Main.GFiles.Item[I].Name + ' - ' + inttostr(i));
      end;

    end;
  if (CurrentCount = 0) then
    ListItemCurrent.ImageIndex := VALIDATION_IMAGE_GOOD
  else
    ListItemCurrent.ImageIndex := VALIDATION_IMAGE_BAD;

  // check time object object ids here!!

  // check for invalid clump models number
  CurrentCount := 0;
  ListItemCurrent := ListStatus.Items.Add;
  ListItemCurrent.Caption := 'Invalid Values';
  ListItemCurrent.ImageIndex := VALIDATION_IMAGE_UNKNOWN;
  if (ObjsList.Count > 2) then for I := 0 to ObjsList.Count - 2 do
    if TGTASObjsObj(ObjsList.Items[I]^).submodels = 0 then
    begin
      Inc(CurrentCount);
      Inc(ErrorCount);
      ListResult.lines.Add(
      format('Error @ ID: %d - %s = ZERO number of clumps in model (!!model invisible!!) !', [TGTASObjsObj(ObjsList.Items[I]^).ID, TGTASObjsObj(ObjsList.Items[I]^).ModelName]));
      // fix
      if box_fix.checked = true then begin
      Main.GFiles.Item[I].Changed:= true;
      TGTASObjsObj(ObjsList.Items[I]^).submodels:= 1; // at least one
      Main.FormMain.ActionSaveModified.Enabled := True;
      end;
    end;
  if (CurrentCount = 0) then
    ListItemCurrent.ImageIndex := VALIDATION_IMAGE_GOOD
  else
    ListItemCurrent.ImageIndex := VALIDATION_IMAGE_BAD;

  CurrentCount := 0;
  ListItemCurrent := ListStatus.Items.Add;
  ListItemCurrent.Caption := 'Models in Archive';
  ListItemCurrent.ImageIndex := VALIDATION_IMAGE_UNKNOWN;
  if (Main.GFiles.Count > 0) then for I := 0 to Main.GFiles.Count - 1 do
    if (Main.GFiles.Item[I].SubType = FILE_IDE) and (TIDEFile(Main.GFiles.Item[I]).Objs.Count > 0) then
      with TIDEFile(Main.GFiles.Item[I]).Objs do for J := 0 to Count - 1 do
        if not ArchiveList.Find(Item[J].ModelName + '.dff', Index) then
        begin
          Inc(CurrentCount);
          Inc(ErrorCount);
          ListResult.lines.Add(format('Error @ file %s [OBJS] %d = Model: "%s" Not Found In Archive! ', [Main.GFiles.Item[I].Name, Item[J].ID, Item[J].ModelName]));
        end;
  if (CurrentCount = 0) then
    ListItemCurrent.ImageIndex := VALIDATION_IMAGE_GOOD
  else
    ListItemCurrent.ImageIndex := VALIDATION_IMAGE_BAD;

  // check textures
  CurrentCount := 0;
  ListItemCurrent := ListStatus.Items.Add;
  ListItemCurrent.Caption := 'Textures in Archive';
  ListItemCurrent.ImageIndex := VALIDATION_IMAGE_UNKNOWN;
  if (Main.GFiles.Count > 0) then for I := 0 to Main.GFiles.Count - 1 do
    if (Main.GFiles.Item[I].SubType = FILE_IDE) and (TIDEFile(Main.GFiles.Item[I]).Objs.Count > 0) then
      with TIDEFile(Main.GFiles.Item[I]).Objs do for J := 0 to Count - 1 do
        if not ArchiveList.Find(Item[J].TextureName + '.txd', Index) then
        begin
          Inc(CurrentCount);
          Inc(ErrorCount);
          ListResult.lines.Add(format('Error @ file %s [OBJS] %d = Texture: "%s" Not Found In Archive! ', [Main.GFiles.Item[I].Name, Item[J].ID, Item[J].TextureName]));
        end;
  if (CurrentCount = 0) then
    ListItemCurrent.ImageIndex := VALIDATION_IMAGE_GOOD
  else
    ListItemCurrent.ImageIndex := VALIDATION_IMAGE_BAD;

  // tobj
  ListItemSec := ListStatus.Items.Add;
  ListItemSec.Caption := 'IDE - Section: TObj';
  ListItemSec.ImageIndex := VALIDATION_IMAGE_SECTION;
  ListResult.lines.Add('### Error Report: IDE - TObj ###');

  CurrentCount := 0;
  ListItemCurrent := ListStatus.Items.Add;
  ListItemCurrent.Caption := 'Models in Archive';
  ListItemCurrent.ImageIndex := VALIDATION_IMAGE_UNKNOWN;
  if (Main.GFiles.Count > 0) then for I := 0 to Main.GFiles.Count - 1 do
    if (Main.GFiles.Item[I].SubType = FILE_IDE) and (TIDEFile(Main.GFiles.Item[I]).TObj.Count > 0) then
      with TIDEFile(Main.GFiles.Item[I]).TObj do for J := 0 to Count - 1 do
        if not ArchiveList.Find(Item[J].ModelName + '.dff', Index) then
        begin
          Inc(CurrentCount);
          Inc(ErrorCount);
          ListResult.lines.Add(format('Error @ file %s [TOBJ] %d = Model: "%s" Not Found In Archive! ', [Main.GFiles.Item[I].Name, Item[J].ID, Item[J].ModelName]));
        end;
  if (CurrentCount = 0) then
    ListItemCurrent.ImageIndex := VALIDATION_IMAGE_GOOD
  else
    ListItemCurrent.ImageIndex := VALIDATION_IMAGE_BAD;

  CurrentCount := 0;
  ListItemCurrent := ListStatus.Items.Add;
  ListItemCurrent.Caption := 'Textures in Archive';
  ListItemCurrent.ImageIndex := VALIDATION_IMAGE_UNKNOWN;
  if (Main.GFiles.Count > 0) then for I := 0 to Main.GFiles.Count - 1 do
    if (Main.GFiles.Item[I].SubType = FILE_IDE) and (TIDEFile(Main.GFiles.Item[I]).TObj.Count > 0) then
      with TIDEFile(Main.GFiles.Item[I]).TObj do for J := 0 to Count - 1 do
        if not ArchiveList.Find(Item[J].TextureName + '.txd', Index) then
        begin
          Inc(CurrentCount);
          Inc(ErrorCount);
          ListResult.lines.Add(format('Error @ file %s [TOBJ] %d = Texture: "%s" Not Found In Archive! ', [Main.GFiles.Item[I].Name, Item[J].ID, Item[J].TextureName]));
        end;
  if (CurrentCount = 0) then
    ListItemCurrent.ImageIndex := VALIDATION_IMAGE_GOOD
  else
    ListItemCurrent.ImageIndex := VALIDATION_IMAGE_BAD;



  // inst missing stuff

  ListItemSec := ListStatus.Items.Add;
  ListItemSec.Caption := 'IPL - Section: Inst';
  ListItemSec.ImageIndex := VALIDATION_IMAGE_SECTION;
  ListResult.lines.Add('### Error Report: IPL - Inst ###');

  CurrentCount := 0;
  ListItemCurrent := ListStatus.Items.Add;
  ListItemCurrent.Caption := 'Associated Definition';
  ListItemCurrent.ImageIndex := VALIDATION_IMAGE_UNKNOWN;
  if (Main.GFiles.Count > 0) then for I := Main.GFiles.Count - 1 downto 0 do // REVERSED because missing ide num fixing routine deletes items.
    if (Main.GFiles.Item[I].SubType = FILE_IPL) and (TIPLFile(Main.GFiles.Item[I]).Inst.Count > 0) then
      with TIPLFile(Main.GFiles.Item[I]).Inst do for J := Count - 1 downto 0 do
      begin
        try // often errors here because of basly formatted files
        Index := BinarySearchObjs(Item[J].ID, ObjsList);
        if not (Index = -1) then
        begin

        // check for interiors
        if (Item[J].HasInterior = false) and (main.GTA_VICE_MODE = true) then begin
            Inc(CurrentCount);
            Inc(WarningCount);
            ListResult.lines.Add(
            format('Warning @ File: %s [OBJS] ID: %d (Model: "%s") - missing VC interior number', [Main.GFiles.Item[I].Name, Item[J].ID, Item[J].ModelName]));
            if box_fix.checked = true then begin // fix
            Item[J].HasInterior:= true;
            Item[J].Interior:= 0;
            SaveItem(j); // update text
            fix('set interior to zero (externior)');
        end; // fix
        end; // interior missing

          if not (CompareText(Item[J].ModelName, TGTASObjsObj(ObjsList.Items[Index]^).ModelName) = 0) then
          begin
            Inc(CurrentCount);
            Inc(ErrorCount);
            try
            ListResult.lines.Add(
            format('Error @ File: %s [OBJS] ID: %d (Model: "%s" should be "%s")', [Main.GFiles.Item[I].Name, Item[J].ID, Item[J].ModelName, TGTASObjsObj(ObjsList.Items[Index]^).ModelName]));
            except
            ListResult.lines.Add(
            format('Error @ File: %s [OBJS] ID: %d (Model: "%s" !! IDE MODEL MISSING!!)', [Main.GFiles.Item[I].Name, Item[J].ID, Item[J].ModelName]));
            end;

            if box_fix.checked = true then begin // fix
            try
            Item[J].ModelName:= TGTASObjsObj(ObjsList.Items[Index]^).ModelName;
            fix('changed ipl name to objs name');
            except fix('FAILED TO FIX, DO IT MANUALLY OR DELETE THE STUFF!!') end;
            end;

          end;
        end else
        begin
          Index2 := BinarySearchTObj(Item[J].ID, TObjList);
          if not (Index2 = -1) then
          begin
            if not (CompareText(Item[J].ModelName, TGTASTObjObj(TObjList.Items[Index2]^).ModelName) = 0) then
            begin
              Inc(CurrentCount);
              Inc(ErrorCount);
              try
              ListResult.lines.Add(
              format('Error @ File: %s [TOBJ] ID: %d (Model: "%s" should be "%s")', [Main.GFiles.Item[I].Name, Item[J].ID, Item[J].ModelName, TGTASTObjObj(TObjList.Items[Index2]^).ModelName]));
              except
              ListResult.lines.Add(
              format('Error @ File: %s [TOBJ] ID: %d (Model: "%s" !! IDE MODEL MISSING!!)', [Main.GFiles.Item[I].Name, Item[J].ID, Item[J].ModelName]));
              end;

              if box_fix.checked = true then begin // fix
              try
              Item[J].ModelName:= TGTASObjsObj(ObjsList.Items[Index]^).ModelName;
              fix('changed ipl name to TOBJ name');
              except fix('FAILED TO FIX, DO IT MANUALLY OR DELETE THE STUFF!!') end;
              end;

            end;
          end else
          begin
            Inc(CurrentCount);
            Inc(ErrorCount);
            try
            ListResult.lines.Add(
            format('Error @ File: %s [INST] ID: "%d" (Model: "%s")', [Main.GFiles.Item[I].Name, Item[J].ID, Item[J].ModelName, TGTASObjsObj(ObjsList.Items[Index]^).ModelName]));
            except
            ListResult.lines.Add(
            format('Error @ File: %s [INST] ID: "%d" (Model: "%s" !! IDE DEFINITION IS MISSING!!)', [Main.GFiles.Item[I].Name, Item[J].ID, Item[J].ModelName]));
            end;

            if box_fix.checked = true then begin // fix
            try
            DeleteItem(J);
            fix('DELETED');
            except fix('FAILED TO FIX, DO IT MANUALLY OR DELETE THE STUFF!!') end;
            end;

          end;
        end;
        except end;
      end;
  if (CurrentCount = 0) then
    ListItemCurrent.ImageIndex := VALIDATION_IMAGE_GOOD
  else
    ListItemCurrent.ImageIndex := VALIDATION_IMAGE_BAD;

  // finishing
  ArchiveList.Free;
  ObjsList.Free;
  TObjList.Free;
  if (ErrorCount = 0) then
  begin
    ModeValue.Font.Color := clGreen;
    ResultValue.Font.Color := clGreen;
    OutStr := 'SUCCESS: No Errors Found!';
  end else
  begin
    ModeValue.Font.Color := clRed;
    ResultValue.Font.Color := clRed;
    OutStr := 'ERROR: ' + IntToStr(ErrorCount) + ' Errors Found!';
  end;
  if (WarningCount > 0) then
    OutStr := OutStr + ' (' + IntToStr(WarningCount) + ' Warnings)';
  ResultValue.Caption := OutStr;

  Processing := False;
  BtnStart.Enabled := True;
end;

function CompareObjsItems(Item1, Item2: Pointer): Integer;
begin
  Result := TGTASObjsObj(Item1^).ID - TGTASObjsObj(Item2^).ID;
end;

function CompareTObjItems(Item1, Item2: Pointer): Integer;
begin
  Result := TGTASTObjObj(Item1^).ID - TGTASTObjObj(Item2^).ID;
end;

function ComparePathItems(Item1, Item2: Pointer): Integer;
begin
  Result := TGTASPathObj(Item1^).ID - TGTASPathObj(Item2^).ID;
end;

function BinarySearchObjs(Key: LongWord; InList: TList): LongInt;
var
  High, J, Low: LongInt;
begin
  if (InList.Count = 0) then
  begin
    Result := -1;
    Exit;
  end;

  Low := 0;
  High := InList.Count - 1;
  while High - Low > 1 do
  begin
    j := (High + Low) div 2;
    if Key <= TGTASObjsObj(InList.Items[J]^).ID then
      High := j
    else Low  := j
  end;
  if TGTASObjsObj(InList.Items[High]^).ID = Key then
    Result := High
  else
    Result := -1;
end;

function BinarySearchTObj(Key: LongWord; InList: TList): LongInt;
var
  High, J, Low: LongInt;
begin
  if (InList.Count = 0) then
  begin
    Result := -1;
    Exit;
  end;

  Low := 0;
  High := InList.Count - 1;
  while High - Low > 1 do
  begin
    J := (High + Low) div 2;
    if (Key <= TGTASTObjObj(InList.Items[J]^).ID) then
      High := J
    else Low  := J
  end;
  if (TGTASTObjObj(InList.Items[High]^).ID = Key) then
    Result := High
  else
    Result := -1;
end;

function BinarySearchPath(Key: LongWord; InList: TList): LongInt;
var
  High, J, Low: LongInt;
begin
  if (InList.Count = 0) then
  begin
    Result := -1;
    Exit;
  end;

  Low := 0;
  High := InList.Count - 1;
  while High - Low > 1 do
  begin
    J := (High + Low) div 2;
    if (Key <= TGTASPathObj(InList.Items[J]^).ID) then
      High := J
    else Low  := J
  end;
  if (TGTASPathObj(InList.Items[High]^).ID = Key) then
    Result := High
  else
    Result := -1;
end;

procedure TFormValidate.ListResultDblClick(Sender: TObject);
{var
  TempStr: String;
  I, J, TempID, TempFile, TempSec: LongInt;}
begin
{  if not (ListResult.ItemIndex = -1) then
  begin
    TempStr := ListResult.lines.Strings[ListResult.ItemIndex];
    I := Pos('[', TempStr);
    if (I = 0) then
      Exit;
    J := Pos(']', TempStr);
    if (J = 0) then
      Exit;
    TempStr := Copy(TempStr, I + 1, J - I - 1);

    TempSec := StrToIntDef(GetVal(1, TempStr), -1);
    TempFile := StrToIntDef(GetVal(2, TempStr), -1);
    TempID := StrToIntDef(GetVal(3, TempStr), -1);

    if (LongWord(TempFile) >= Main.GFiles.Count) or (TempID < 0) then
      Exit;

    if (TempSec < 0) or (TempFile < 0) then
    begin
      if (FormArchive.DisplayList.Count > 0) then for J := 0 to FormArchive.DisplayList.Count - 1 do
        if (LongWord(TempID) = LongWord(FormArchive.DisplayList.Items[J])) then
        begin
          if (J < FormArchive.ListArchive.Items.Count) then
            FormArchive.ListArchive.ItemIndex := J;
        end;
    end else case Main.GFiles.Item[TempFile].SubType of
      FILE_IDE:
      begin
        if (FormIDE.DisplayListFiles.Count > 0) then for I := 0 to FormIDE.DisplayListFiles.Count - 1 do
          if (LongWord(TempFile) = LongWord(FormIDE.DisplayListFiles.Items[I])) then
          begin
            FormIDE.ListFiles.ItemIndex := I;
            FormIDE.RadioSection.ItemIndex := 0;
            if (FormIDE.DisplayListItems.Count > 0) then for J := 0 to FormIDE.DisplayListItems.Count - 1 do
              if (LongWord(TempID) = LongWord(FormIDE.DisplayListItems.Items[J])) then
              begin
                FormIDE.ListItems.ItemIndex := J;
                FormIDE.SelectItem(J);
              end;
          end;
      end;

      FILE_IPL:
      begin
        if (FormIPL.DisplayListFiles.Count > 0) then for I := 0 to FormIPL.DisplayListFiles.Count - 1 do
          if (LongWord(TempFile) = LongWord(FormIPL.DisplayListFiles.Items[I])) then
          begin
            FormIPL.ListFiles.ItemIndex := I;
            FormIPL.RadioSection.ItemIndex := 0;
            if (FormIPL.DisplayListItems.Count > 0) then for J := 0 to FormIPL.DisplayListItems.Count - 1 do
              if (LongWord(TempID) = LongWord(FormIPL.DisplayListItems.Items[J])) then
              begin
                FormIPL.ListItems.ItemIndex := J;
                FormIPL.SelectItem(J, False);
              end;
          end;
      end;
    end;
  end;}
end;

procedure TFormValidate.BtnCopyClick(Sender: TObject);
var
  CopyClip: String;
  I: LongWord;
begin
  CopyClip := '';
  if (ListResult.lines.Count > 0) then for I := 0 to ListResult.lines.Count - 1 do
    CopyClip := CopyClip + ListResult.lines.Strings[I] + #13#10;
  ClipBoard.SetTextBuf(PChar(CopyClip));
end;

procedure TFormValidate.FormCreate(Sender: TObject);
begin
  FirstShow := True;
end;

procedure TFormValidate.FormShow(Sender: TObject);
var
  Rct: TRect;
  L, T: LongInt;
begin
  if FirstShow then
  begin
    GetWindowRect(MainGLView.Handle, Rct);
    L := Rct.Right - Width;
    T := Rct.Top + FormMain.BtnHideShow3DView.Height;
    if (T < 0) then
      T := 0;
    if (L < 0) then
      L := 0;
    Top := T;
    Left := L;
    FirstShow := False;
  end;
end;

procedure TFormValidate.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if not (MainAction = nil) then
    MainAction.Checked := False;
end;

procedure TFormValidate.SetAction(inAction: TAction);
begin
  MainAction := inAction;
end;

end.
