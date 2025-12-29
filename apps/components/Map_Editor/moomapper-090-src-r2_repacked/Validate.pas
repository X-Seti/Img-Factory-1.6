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
    PanelResult: TPanel;
    PanelValidate: TPanel;
    PanelStatus: TPanel;
    ListStatus: TListView;
    PanelControls: TPanel;
    BtnStart: TButton;
    Label1: TLabel;
    Label2: TLabel;
    ModeValue: TLabel;
    ResultValue: TLabel;
    Label3: TLabel;
    ListResult: TListBox;
    BtnCopy: TButton;
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
  ListResult.Items.Add('### Error Report: Main Archive  ###');

  CurrentCount := 0;
  ListItemCurrent := ListStatus.Items.Add;
  ListItemCurrent.Caption := 'Duplicate Entries';
  ListItemCurrent.ImageIndex := VALIDATION_IMAGE_UNKNOWN;
  if (ArchiveList.Count > 2) then for I := 0 to ArchiveList.Count - 2 do
    if (CompareText(ArchiveList.Strings[I], ArchiveList.Strings[I + 1]) = 0) then
    begin
      Inc(CurrentCount);
      Inc(WarningCount);
      ListResult.Items.Add('Warning @ File: ' + ArchiveList.Strings[I] + ' = ' +
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
  ListResult.Items.Add('### Error Report: IDE - Objs ###');

  CurrentCount := 0;
  ListItemCurrent := ListStatus.Items.Add;
  ListItemCurrent.Caption := 'Duplicate IDs';
  ListItemCurrent.ImageIndex := VALIDATION_IMAGE_UNKNOWN;
  if (ObjsList.Count > 2) then for I := 0 to ObjsList.Count - 2 do
    if (TGTASObjsObj(ObjsList.Items[I]^).ID = TGTASObjsObj(ObjsList.Items[I + 1]^).ID) then
    begin
      Inc(CurrentCount);
      Inc(ErrorCount);
      ListResult.Items.Add('Error @ ID: ' + IntToStr(TGTASObjsObj(ObjsList.Items[I]^).ID) + ' = ' +
                           'Duplicate IDs Found!');
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
          ListResult.Items.Add('Error @ ID: ' + IntToStr(Item[J].ID) + ' = ' +
                               'Model: "' + Item[J].ModelName + '" Not Found In Archive! ' +
                               '[0, ' + IntToStr(I) + ', ' + IntToStr(J) + ']');
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
    if (Main.GFiles.Item[I].SubType = FILE_IDE) and (TIDEFile(Main.GFiles.Item[I]).Objs.Count > 0) then
      with TIDEFile(Main.GFiles.Item[I]).Objs do for J := 0 to Count - 1 do
        if not ArchiveList.Find(Item[J].TextureName + '.txd', Index) then
        begin
          Inc(CurrentCount);
          Inc(ErrorCount);
          ListResult.Items.Add('Error @ ID: ' + IntToStr(Item[J].ID) + ' = ' +
                               'Texture: "' + Item[J].TextureName + '" Not Found In Archive! ' +
                               '[0, ' + IntToStr(I) + ', ' + IntToStr(J) + ']');
        end;
  if (CurrentCount = 0) then
    ListItemCurrent.ImageIndex := VALIDATION_IMAGE_GOOD
  else
    ListItemCurrent.ImageIndex := VALIDATION_IMAGE_BAD;

  // tobj
  ListItemSec := ListStatus.Items.Add;
  ListItemSec.Caption := 'IDE - Section: TObj';
  ListItemSec.ImageIndex := VALIDATION_IMAGE_SECTION;
  ListResult.Items.Add('### Error Report: IDE - TObj ###');

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
          ListResult.Items.Add('Error @ ID: ' + IntToStr(Item[J].ID) + ' = ' +
                               'Model: "' + Item[J].ModelName + '" Not Found In Archive! ' +
                               '[1, ' + IntToStr(I) + ', ' + IntToStr(J) + ']');
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
          ListResult.Items.Add('Error @ ID: ' + IntToStr(Item[J].ID) + ' = ' +
                               'Texture: "' + Item[J].TextureName + '" Not Found In Archive! ' +
                               '[1, ' + IntToStr(I) + ', ' + IntToStr(J) + ']');
        end;
  if (CurrentCount = 0) then
    ListItemCurrent.ImageIndex := VALIDATION_IMAGE_GOOD
  else
    ListItemCurrent.ImageIndex := VALIDATION_IMAGE_BAD;

  // inst
  ListItemSec := ListStatus.Items.Add;
  ListItemSec.Caption := 'IPL - Section: Inst';
  ListItemSec.ImageIndex := VALIDATION_IMAGE_SECTION;
  ListResult.Items.Add('### Error Report: IPL - Inst ###');

  CurrentCount := 0;
  ListItemCurrent := ListStatus.Items.Add;
  ListItemCurrent.Caption := 'Associated Definition';
  ListItemCurrent.ImageIndex := VALIDATION_IMAGE_UNKNOWN;
  if (Main.GFiles.Count > 0) then for I := 0 to Main.GFiles.Count - 1 do
    if (Main.GFiles.Item[I].SubType = FILE_IPL) and (TIPLFile(Main.GFiles.Item[I]).Inst.Count > 0) then
      with TIPLFile(Main.GFiles.Item[I]).Inst do for J := 0 to Count - 1 do
      begin
        Index := BinarySearchObjs(Item[J].ID, ObjsList);
        if not (Index = -1) then
        begin
          if not (CompareText(Item[J].ModelName, TGTASObjsObj(ObjsList.Items[Index]^).ModelName) = 0) then
          begin
            Inc(CurrentCount);
            Inc(ErrorCount);
            ListResult.Items.Add('Error @ ID: ' + IntToStr(Item[J].ID) + ' = ' +
                                 'Object Definition (Objs) Model Name Mismatch! ' +
                                 '[0, ' + IntToStr(I) + ', ' + IntToStr(J) + ']');
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
              ListResult.Items.Add('Error @ ID: ' + IntToStr(Item[J].ID) + ' = ' +
                                   'Object Definition (TObj) Model Name Mismatch! ' +
                                   '[0, ' + IntToStr(I) + ', ' + IntToStr(J) + ']');
            end;
          end else
          begin
            Inc(CurrentCount);
            Inc(ErrorCount);
            ListResult.Items.Add('Error @ ID: ' + IntToStr(Item[J].ID) + ' = ' +
                                 'Associated Object Definition (Objs or TObj) Not Found! ' +
                                 '[0, ' + IntToStr(I) + ', ' + IntToStr(J) + ']');
          end;
        end;
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
var
  TempStr: String;
  I, J, TempID, TempFile, TempSec: LongInt;
begin
  if not (ListResult.ItemIndex = -1) then
  begin
    TempStr := ListResult.Items.Strings[ListResult.ItemIndex];
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
  end;
end;

procedure TFormValidate.BtnCopyClick(Sender: TObject);
var
  CopyClip: String;
  I: LongWord;
begin
  CopyClip := '';
  if (ListResult.Items.Count > 0) then for I := 0 to ListResult.Items.Count - 1 do
    CopyClip := CopyClip + ListResult.Items.Strings[I] + #13#10;
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
