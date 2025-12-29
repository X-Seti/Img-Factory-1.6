unit EditorIPL;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ActnList, GTAText, ExtraView, StdCtrls, ExtCtrls, GTAImg, GLView,
  ToolWin, RequiredTypes, Menus;

type
  TFormEditorIPL = class(TForm)
    ListFiles: TListView;
    ListItems: TListView;
    RadioSection: TRadioGroup;
    MainActionList: TActionList;
    MainToolbar: TToolBar;
    TBEditAdd: TToolButton;
    ActionEditAdd: TAction;
    ActionEditDelete: TAction;
    TBEditDel: TToolButton;
    FileToolbar: TToolBar;
    TBFileOpen: TToolButton;
    TBFileDelete: TToolButton;
    ActionFileOpen: TAction;
    ActionFileDelete: TAction;
    ActionFileNew: TAction;
    ActionFileClose: TAction;
    TBFileClose: TToolButton;
    TBFileNew: TToolButton;
    TBSep: TToolButton;
    DlgNewFile: TSaveDialog;
    DlgOpenFile: TOpenDialog;
    PanelInstSelect: TPanel;
    BtnInstSelectAll: TButton;
    ActionEditDuplicate: TAction;
    TBEditDuplicate: TToolButton;
    ToolButton1: TToolButton;
    PopupItems: TPopupMenu;
    PopupAddItem: TMenuItem;
    PopupDeleteItem: TMenuItem;
    PopupDuplicateItem: TMenuItem;
    N1: TMenuItem;
    PopupEditItem: TMenuItem;
    N2: TMenuItem;
    PopupFiles: TPopupMenu;
    PopupFileClose: TMenuItem;
    PopupFileOpen: TMenuItem;
    N3: TMenuItem;
    PopupFileNew: TMenuItem;
    PopupFileDelete: TMenuItem;
    procedure ListFilesData(Sender: TObject; Item: TListItem);
    procedure ListItemsData(Sender: TObject; Item: TListItem);
    procedure ListFilesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ListItemsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure RadioSectionClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListFilesColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListItemsColumnClick(Sender: TObject; Column: TListColumn);
    procedure ActionEditDeleteExecute(Sender: TObject);
    procedure ListItemsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ActionEditAddExecute(Sender: TObject);
    procedure ActionFileCloseExecute(Sender: TObject);
    procedure ActionFileDeleteExecute(Sender: TObject);
    procedure ActionFileNewExecute(Sender: TObject);
    procedure ActionFileOpenExecute(Sender: TObject);
    procedure ListItemsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnInstSelectAllClick(Sender: TObject);
    procedure ActionEditDuplicateExecute(Sender: TObject);
    procedure PopupEditItemClick(Sender: TObject);
  private
    LastSortedFilesBy: Integer;
    LastSortedItemsBy: Integer;

    MainAction: TAction;
    SelSection: Byte;

    procedure DeleteItem(inItem: LongInt);
  public
    SelFile: LongInt;
    DisplayListFiles: TList;
    DisplayListItems: TList;

    function AddItem: LongInt;
    function DuplicateItem(inCopy: LongWord): LongInt;
    procedure SelectItem(inItem: LongInt; Mult: Boolean);
    constructor Create(AOwner: TComponent); override;
    procedure UpdateListFileSize;
    procedure UpdateListItemsSize;
    procedure SetAction(inAction: TAction);
    procedure SortFiles(SortBy: Byte; SortReverse: Boolean);
    procedure SortItems(SortBy: Byte; SortReverse: Boolean);
  end;

  function CompareFileItems(Item1, Item2: LongWord): Integer;
  function CompareItemItems(Item1, Item2: LongWord): Integer;

var
  DisplaySortBy, DisplaySection: Byte;
  DisplayFile: LongInt; DisplayReverse: Boolean;

implementation

uses
  EditorItem, Main;

{$R *.dfm}

constructor TFormEditorIPL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DisplayListFiles := TList.Create;
  DisplayListItems := TList.Create;
  LastSortedFilesBy := -1;
  LastSortedItemsBy := -1;

  SelFile := -1;
  SelSection := SECTION_INST;
  UpdateListFileSize;
  UpdateListItemsSize;
end;

function CompareFileItems(Item1, Item2: LongWord): Integer;
begin
  case DisplaySortBy of
    0: Result := Item1 - Item2;
    1: Result := CompareText(ExtractFileName(Main.GFiles.Item[Item1].Name), ExtractFileName(Main.GFiles.Item[Item2].Name));
  else
    Result := 0;
  end;
  if DisplayReverse then
    Result := -Result;
end;

function CompareItemItems(Item1, Item2: LongWord): Integer;
begin
  case DisplaySection of
    SECTION_INST:
    begin
      with TIPLFile(Main.GFiles.Item[DisplayFile]).Inst do case DisplaySortBy of
        0: Result := Item[Item1].ID - Item[Item2].ID;
        1: Result := CompareText(Item[Item1].ModelName, Item[Item2].ModelName);
      else
        Result := 0;
      end;
    end;
    SECTION_CULL:
    begin
      with TIPLFile(Main.GFiles.Item[DisplayFile]).Cull do case DisplaySortBy of
        0, 1: Result := Item1 - Item2;
      else
        Result := 0;
      end;
    end;
    SECTION_ZONE:
    begin
      with TIPLFile(Main.GFiles.Item[DisplayFile]).Zone do case DisplaySortBy of
        0: Result := Item1 - Item2;
        1: Result := CompareText(Item[Item1].ZoneName, Item[Item2].ZoneName);
      else
        Result := 0;
      end;
    end;
    SECTION_PATH:
    begin
      with TIPLFile(Main.GFiles.Item[DisplayFile]).Path do case DisplaySortBy of
        0: Result := Item[Item1].PathType - Item[Item2].PathType;
        1: Result := Item[Item1].PathOther - Item[Item2].PathOther;
      else
        Result := 0;
      end;
    end;
  else
    Result := 0;
  end;
  if DisplayReverse then
    Result := -Result;
end;

procedure TFormEditorIPL.SortFiles(SortBy: Byte; SortReverse: Boolean);
begin
  DisplaySortBy := SortBy;
  DisplayReverse := SortReverse;
  if (DisplayListFiles.Count > 0) then
    DisplayListFiles.Sort(@CompareFileItems);
  ListItems.ItemIndex := -1;
  UpdateListItemsSize;
  ListFiles.Repaint;
end;

procedure TFormEditorIPL.SortItems(SortBy: Byte; SortReverse: Boolean);
begin
  if not (SelFile = -1) then
  begin
    DisplaySortBy := SortBy;
    DisplaySection := SelSection;
    DisplayFile := SelFile;
    DisplayReverse := SortReverse;
    if (DisplayListItems.Count > 0) then
      DisplayListItems.Sort(@CompareItemItems);
  end;
  ListItems.ItemIndex := -1;
  ListItems.Repaint;
end;

procedure TFormEditorIPL.UpdateListFileSize;
var
  I: LongWord;
begin
  DisplayListFiles.Clear;
  if (GFiles.Count > 0) then for I := 0 to GFiles.Count - 1 do
    if (GFiles.Item[I].SubType = FILE_IPL) then
      DisplayListFiles.Add(Pointer(I));
  ListFiles.Items.Count := DisplayListFiles.Count;
  LastSortedFilesBy := -1;
  ListFiles.Repaint;
end;

procedure TFormEditorIPL.UpdateListItemsSize;
var
  I: LongWord;
begin
  if not (ListFiles.ItemIndex = -1) then
  begin
    SelFile := LongWord(DisplayListFiles.Items[ListFiles.ItemIndex]);
    case SelSection of
      SECTION_INST:
      begin
        PanelInstSelect.Visible := True;
        DisplayListItems.Clear;
        with TIPLFile(GFiles.Item[SelFile]).Inst do
          if (Count > 0) then for I := 0 to Count - 1 do
            DisplayListItems.Add(Pointer(I));
      end;
      SECTION_CULL:
      begin
        PanelInstSelect.Visible := False;
        DisplayListItems.Clear;
        with TIPLFile(GFiles.Item[SelFile]).Cull do
          if (Count > 0) then for I := 0 to Count - 1 do
            DisplayListItems.Add(Pointer(I));
      end;
      SECTION_ZONE:
      begin
        PanelInstSelect.Visible := False;
        DisplayListItems.Clear;
        with TIPLFile(GFiles.Item[SelFile]).Zone do
          if (Count > 0) then for I := 0 to Count - 1 do
            DisplayListItems.Add(Pointer(I));
      end;
      SECTION_PATH:
      begin
        PanelInstSelect.Visible := False;
        DisplayListItems.Clear;
        with TIPLFile(GFiles.Item[SelFile]).Path do
          if (Count > 0) then for I := 0 to Count - 1 do
            DisplayListItems.Add(Pointer(I));
      end;
    else
      DisplayListItems.Clear;
    end;
  end else
    DisplayListItems.Clear;
  ActionEditDuplicate.Enabled := False;
  ListItems.Items.Count := DisplayListItems.Count;
  LastSortedItemsBy := -1;
  ListItems.Repaint;
end;

procedure TFormEditorIPL.ListFilesData(Sender: TObject; Item: TListItem);
var
  TempStr: String;
begin
  if not (GFiles = nil) then
  begin
    if ListFiles.ViewStyle = vsReport then
    begin
      Item.Caption := IntToStr(LongWord(DisplayListFiles.Items[Item.Index]));
      TempStr := GFiles.Item[LongWord(DisplayListFiles.Items[Item.Index])].Name;
      if GFiles.Item[LongWord(DisplayListFiles.Items[Item.Index])].Changed then
        TempStr := TempStr + ' *';
      Item.SubItems.Add(TempStr);
    end else
    begin
      Item.Caption := GFiles.Item[LongWord(DisplayListFiles.Items[Item.Index])].Name;
    end;
  end;
end;

procedure TFormEditorIPL.ListItemsData(Sender: TObject; Item: TListItem);
begin
  if not (GFiles = nil) and not (SelFile = -1) then
  begin
    if ListItems.ViewStyle = vsReport then
    begin
      case SelSection of
        SECTION_INST:
        begin
          Item.Caption := IntToStr(TIPLFile(GFiles.Item[SelFile]).Inst.Item[LongWord(DisplayListItems.Items[Item.Index])].ID);
          Item.SubItems.Add(TIPLFile(GFiles.Item[SelFile]).Inst.Item[LongWord(DisplayListItems.Items[Item.Index])].ModelName);
        end;
        SECTION_CULL:
        begin
          Item.Caption := IntToStr(LongWord(DisplayListItems.Items[Item.Index]));
          with TIPLFile(GFiles.Item[SelFile]).Cull.Item[LongWord(DisplayListItems.Items[Item.Index])] do
            Item.SubItems.Add(Format('(%f1.3, %f1.3, %f1.3)', [Pos1[0], Pos1[1], Pos1[2]]));
        end;
        SECTION_ZONE:
        begin
          Item.Caption := IntToStr(LongWord(DisplayListItems.Items[Item.Index]));
          Item.SubItems.Add(TIPLFile(GFiles.Item[SelFile]).Zone.Item[LongWord(DisplayListItems.Items[Item.Index])].ZoneName);
        end;
        SECTION_PATH:
        begin
          case TIPLFile(GFiles.Item[SelFile]).Path.Item[LongWord(DisplayListItems.Items[Item.Index])].PathType of
            0: Item.Caption := 'Pedestrian';
            1: Item.Caption := 'Vehicle';
          end;
          Item.SubItems.Add(IntToStr(TIPLFile(GFiles.Item[SelFile]).Path.Item[LongWord(DisplayListItems.Items[Item.Index])].PathOther));
        end;
      else
        ListItems.Items.Count := 0;
      end;
    end else
    begin
      Item.Caption := IntToStr(LongWord(DisplayListItems.Items[Item.Index]));
    end;
  end;
end;

procedure TFormEditorIPL.ListFilesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  UpdateListItemsSize;
end;

procedure TFormEditorIPL.ListItemsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if not (Item = nil) then
  begin
    ActionEditDuplicate.Enabled := True;
    case SelSection of
      SECTION_INST: with TIPLFile(GFiles.Item[SelFile]).Inst.Item[LongWord(DisplayListItems.Items[Item.Index])] do
        FormExtra.SetObject(ID);
    end;
  end else
  begin
    ActionEditDuplicate.Enabled := False;
    case SelSection of
      SECTION_INST: FormExtra.SetModelTexture('', -1);
    end;
  end;
end;

procedure TFormEditorIPL.RadioSectionClick(Sender: TObject);
begin
  case RadioSection.ItemIndex of
    0:  SelSection := SECTION_INST;
    1:  SelSection := SECTION_CULL;
    2:  SelSection := SECTION_ZONE;
    3:  SelSection := SECTION_PATH;
  end;
  UpdateListItemsSize;
end;

procedure TFormEditorIPL.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if not (MainAction = nil) then
    MainAction.Checked := False;
end;

procedure TFormEditorIPL.SetAction(inAction: TAction);
begin
  MainAction := inAction;
end;

procedure TFormEditorIPL.ListFilesColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  if (LastSortedFilesBy = Column.Index) then
  begin
    SortFiles(Column.Index, True);
    LastSortedFilesBy := -1;
  end else
  begin
    SortFiles(Column.Index, False);
    LastSortedFilesBy := Column.Index;
  end;
end;

procedure TFormEditorIPL.ListItemsColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  if (LastSortedItemsBy = Column.Index) then
  begin
    SortItems(Column.Index, True);
    LastSortedItemsBy := -1;
  end else
  begin
    SortItems(Column.Index, False);
    LastSortedItemsBy := Column.Index;
  end;

end;

procedure TFormEditorIPL.ActionEditDeleteExecute(Sender: TObject);
begin
  DeleteItem(ListItems.ItemIndex);
end;

procedure TFormEditorIPL.SelectItem(inItem: LongInt; Mult: Boolean);
var
  DoMult: Boolean;
begin
  if not (GFiles = nil) and not (SelFile = -1) and not (inItem = -1) then
  begin
    DoMult := False;
    case SelSection of
      SECTION_INST:
      begin
        DoMult := Mult;
        MainGLView.JumpToLocation(TIPLFile(GFiles.Item[SelFile]).Inst.Item[LongWord(DisplayListItems.Items[inItem])].Pos);
      end;
    end;
    if DoMult then
      FormEditor.SetToMode(FILE_IPL, SECTION_MULT_INST, SelFile, LongWord(DisplayListItems.Items[inItem]))
    else
      FormEditor.SetToMode(FILE_IPL, SelSection, SelFile, LongWord(DisplayListItems.Items[inItem]));
  end else
    FormEditor.SetToNone;
end;

procedure TFormEditorIPL.DeleteItem(inItem: LongInt);
begin
  if ListItems.ItemIndex = inItem then
  begin
    SelectItem(-1, False);
  end;
  if not (GFiles = nil) and not (SelFile = -1) and not (inItem = -1) then
  begin
    case SelSection of
      SECTION_INST:
        TIPLFile(GFiles.Item[SelFile]).Inst.DeleteItem(LongWord(DisplayListItems.Items[inItem]));
      SECTION_CULL:
        TIPLFile(GFiles.Item[SelFile]).Cull.DeleteItem(LongWord(DisplayListItems.Items[inItem]));
      SECTION_ZONE:
        TIPLFile(GFiles.Item[SelFile]).Zone.DeleteItem(LongWord(DisplayListItems.Items[inItem]));
      SECTION_PATH:
        TIPLFile(GFiles.Item[SelFile]).Path.DeleteItem(LongWord(DisplayListItems.Items[inItem]));
    end;
    UpdateListItemsSize;
  end;
end;

function TFormEditorIPL.AddItem: LongInt;
begin
  if not (GFiles = nil) and not (SelFile = -1) then
  begin
    case SelSection of
      SECTION_INST:
        Result := TIPLFile(GFiles.Item[SelFile]).Inst.AddItem(MainGLView.GetPosition);
      SECTION_CULL:
        Result := TIPLFile(GFiles.Item[SelFile]).Cull.AddItem;
      SECTION_ZONE:
        Result := TIPLFile(GFiles.Item[SelFile]).Zone.AddItem;
      SECTION_PATH:
        Result := TIPLFile(GFiles.Item[SelFile]).Path.AddItem;
    else
      Result := -1;
    end;
    UpdateListItemsSize;
  end else
    Result := -1;
end;

function TFormEditorIPL.DuplicateItem(inCopy: LongWord): LongInt;
begin
  if not (GFiles = nil) and not (SelFile = -1) then
  begin
    case SelSection of
      SECTION_INST:
      begin
        Result := TIPLFile(GFiles.Item[SelFile]).Inst.AddItem;
        with TIPLFile(GFiles.Item[SelFile]).Inst.Item[Result] do
        begin
          HasInterior := TIPLFile(GFiles.Item[SelFile]).Inst.Item[inCopy].HasInterior;

          ID := TIPLFile(GFiles.Item[SelFile]).Inst.Item[inCopy].ID;
          ModelName := TIPLFile(GFiles.Item[SelFile]).Inst.Item[inCopy].ModelName;
          Interior := TIPLFile(GFiles.Item[SelFile]).Inst.Item[inCopy].Interior;

          Pos := TIPLFile(GFiles.Item[SelFile]).Inst.Item[inCopy].Pos;
          Scale := TIPLFile(GFiles.Item[SelFile]).Inst.Item[inCopy].Scale;
          Rotation := TIPLFile(GFiles.Item[SelFile]).Inst.Item[inCopy].Rotation;
        end;
      end;
      SECTION_CULL:
      begin
        Result := TIPLFile(GFiles.Item[SelFile]).Cull.AddItem;
        with TIPLFile(GFiles.Item[SelFile]).Cull.Item[Result] do
        begin
          Pos1 := TIPLFile(GFiles.Item[SelFile]).Cull.Item[inCopy].Pos1;

          Pos2 := TIPLFile(GFiles.Item[SelFile]).Cull.Item[inCopy].Pos2;

          Pos3 := TIPLFile(GFiles.Item[SelFile]).Cull.Item[inCopy].Pos3;

          U10 := TIPLFile(GFiles.Item[SelFile]).Cull.Item[inCopy].U10;
          U11 := TIPLFile(GFiles.Item[SelFile]).Cull.Item[inCopy].U11;
        end;
      end;
      SECTION_ZONE:
      begin
        Result := TIPLFile(GFiles.Item[SelFile]).Zone.AddItem;
        with TIPLFile(GFiles.Item[SelFile]).Zone.Item[Result] do
        begin
          ZoneName := TIPLFile(GFiles.Item[SelFile]).Zone.Item[inCopy].ZoneName;

          Sort := TIPLFile(GFiles.Item[SelFile]).Zone.Item[inCopy].Sort;

          Pos1 := TIPLFile(GFiles.Item[SelFile]).Zone.Item[inCopy].Pos1;

          Pos2 := TIPLFile(GFiles.Item[SelFile]).Zone.Item[inCopy].Pos2;

          U9 := TIPLFile(GFiles.Item[SelFile]).Zone.Item[inCopy].U9;
        end;
      end;
    else
      Result := -1;
    end;
    UpdateListItemsSize;
  end else
    Result := -1;
end;

procedure TFormEditorIPL.ListItemsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then
    DeleteItem(ListItems.ItemIndex);
  if Key = VK_RETURN then
    SelectItem(ListItems.ItemIndex, (ssShift in Shift));
end;

procedure TFormEditorIPL.ActionEditAddExecute(Sender: TObject);
begin
  ListItems.ItemIndex := AddItem;
  SelectItem(ListItems.ItemIndex, False);
end;

procedure TFormEditorIPL.ActionFileCloseExecute(Sender: TObject);
var
  I: LongWord;
  DelFile: TGTASFile;
  VerFileName, FileName: String;
begin
  if not (GFiles = nil) and not (SelFile = -1) then
  begin
    if (GTA_VICE_MODE) then
      VerFileName := VICE_DAT_FILE
    else
      VerFileName := GTA3_DAT_FILE;
    FileName := GFiles.Item[SelFile].Name;
    if (MessageDlg('Are you sure you wish to close the following files?' + #13#10 +
        FileName, mtConfirmation, [mbOk, mbCancel], 0) = mrOk) then
    begin
      if (MessageDlg('Do you want to remove the reference from ' + VerFileName + '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      begin
        FormDAT.RemoveIPL(FileName);
      end;
      DelFile := GFiles.Item[SelFile];
      if (LongWord(SelFile) < GFiles.Count - 1) then for I := SelFile to GFiles.Count - 2 do
        GFiles.Item[I] := GFiles.Item[I + 1];
      DelFile.Free;
      Dec(GFiles.Count);
      SetLength(GFiles.Item, GFiles.Count);
      FormMain.UpdateVisible;
      FormEditor.SetToNone;
      UpdateListFileSize;
      UpdateListItemsSize;
    end;
  end;
end;

procedure TFormEditorIPL.ActionFileDeleteExecute(Sender: TObject);
var
  I: LongWord;
  DelFile: TGTASFile;
  VerFileName, FileName: String;
begin
  if not (GFiles = nil) and not (SelFile = -1) then
  begin
    if (GTA_VICE_MODE) then
      VerFileName := VICE_DAT_FILE
    else
      VerFileName := GTA3_DAT_FILE;
    FileName := GFiles.Item[SelFile].Name;
    if (MessageDlg('Are you sure you wish to close the following files?' + #13#10 +
        FileName, mtConfirmation, [mbOk, mbCancel], 0) = mrOk) then
    begin
      if (MessageDlg('Do you want to remove the reference from ' + VerFileName + '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      begin
        FormDAT.RemoveIPL(FileName);
      end;
      DelFile := GFiles.Item[SelFile];
      if (LongWord(SelFile) < GFiles.Count - 1) then for I := SelFile to GFiles.Count - 2 do
        GFiles.Item[I] := GFiles.Item[I + 1];
      DelFile.Free;
      Dec(GFiles.Count);
      SetLength(GFiles.Item, GFiles.Count);
      FormMain.UpdateVisible;
      FormEditor.SetToNone;
      UpdateListFileSize;
      UpdateListItemsSize;
      if DeleteFile(GTAPath + FileName) then
        MessageDlg('SUCCESS: "' + FileName + '" Successfully Deleted!', mtInformation, [mbOk], 0)
      else
        MessageDlg('ERROR: "' + FileName + '" Could NOT Be Deleted! ' + #13#10 +
                   'Please check this file is not in use or marked as read only!', mtError, [mbOk], 0);

    end;
  end;
end;

procedure TFormEditorIPL.ActionFileNewExecute(Sender: TObject);
var
  VerFileName, TempStr: String;
  Valid: Boolean;
begin
  DlgNewFile.InitialDir := GTAPath + 'DATA\';
  if DlgNewFile.Execute then
  begin
    if (GTA_VICE_MODE) then
      VerFileName := VICE_DAT_FILE
    else
      VerFileName := GTA3_DAT_FILE;
    TempStr := ExtractRelativePath(GTAPath, DlgNewFile.FileName);
    Valid := True;
    if not (Pos(' ', TempStr) = 0) or ((Length(TempStr) > 2) and ((TempStr[2] = ':') or (TempStr[2] = '.')) and (TempStr[3] = '\')) then
      Valid := False;
    if Valid then
    begin
      if (MessageDlg('Do you want to add the reference to ' + VerFileName + '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      begin
        FormDAT.AddIPL(TempStr);
      end;
      Inc(GFiles.Count);
      SetLength(GFiles.Item, GFiles.Count);
      GFiles.Item[GFiles.Count - 1] := TIPLFile.Create;
      GFiles.Item[GFiles.Count - 1].Name := TempStr;
      GFiles.Item[GFiles.Count - 1].InDat := True;
      GFiles.Item[GFiles.Count - 1].Changed := True;
      FormMain.UpdateVisible;
      FormEditor.SetToNone;
      UpdateListFileSize;
      UpdateListItemsSize;
      MessageDlg('SUCCESS: "' + TempStr + '" Created Successfully!', mtInformation, [mbOk], 0);
    end else
      MessageDlg('ERROR: "' + TempStr + '" Could Not Be Created!' + #13#10 +
                 'Please check this file is located in a subfolder of the GTA Path' +
                 'and that there are no spaces or illegal characters in the filename!', mtError, [mbOk], 0);
  end;
end;

procedure TFormEditorIPL.ActionFileOpenExecute(Sender: TObject);
var
  VerFileName, TempStr: String;
  Valid: Boolean;
  AddFile: TGTASFile;
begin
  AddFile := nil;
  DlgOpenFile.InitialDir := GTAPath + 'DATA\';
  if DlgOpenFile.Execute then
  begin
    if (GTA_VICE_MODE) then
      VerFileName := VICE_DAT_FILE
    else
      VerFileName := GTA3_DAT_FILE;
    TempStr := ExtractRelativePath(GTAPath, DlgOpenFile.FileName);
    Valid := True;
    if not (Pos(' ', TempStr) = 0) or ((Length(TempStr) > 2) and ((TempStr[2] = ':') or (TempStr[2] = '.')) and (TempStr[3] = '\')) then
      Valid := False;

    if Valid then
    begin
      AddFile := TIPLFile.Create;
      AddFile.Name := TempStr;
      AddFile.InDat := True;
      if not (AddFile.LoadFrom(TempStr) = 0) then
      begin
        Valid := False;
        AddFile.Free;
      end;
    end;

    if Valid then
    begin
      if (MessageDlg('Do you want to add the reference to ' + VerFileName + '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      begin
        FormDAT.AddIPL(TempStr);
      end;
      Inc(GFiles.Count);
      SetLength(GFiles.Item, GFiles.Count);
      GFiles.Item[GFiles.Count - 1] := AddFile;
      FormMain.UpdateVisible;
      FormEditor.SetToNone;
      UpdateListFileSize;
      UpdateListItemsSize;
      MessageDlg('SUCCESS: "' + TempStr + '" Loaded Successfully!', mtInformation, [mbOk], 0);
    end else
      MessageDlg('ERROR: "' + TempStr + '" Could Not Be Loaded!' + #13#10 +
                 'Please check this file is located in a subfolder of the GTA Path' +
                 'and that there are no spaces or illegal characters in the filename!', mtError, [mbOk], 0);
  end;
end;

procedure TFormEditorIPL.ListItemsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ssDouble in Shift) then
    SelectItem(ListItems.ItemIndex, (ssShift in Shift));
end;

procedure TFormEditorIPL.BtnInstSelectAllClick(Sender: TObject);
var
  I: LongWord;
  LItem: TListItem;
  CenPos: TVector3f;
begin
  if not (GFiles = nil) and not (SelFile = -1) and (SelSection = SECTION_INST) then
  begin
    FormEditor.EditIPLMultInstList.Clear;
    if (DisplayListItems.Count > 2) then
    begin
      for I := 0 to 1 do
        SelectItem(I, True);
      for I := 2 to DisplayListItems.Count - 1 do
      begin
        LItem := FormEditor.EditIPLMultInstList.Items.Add;
        LItem.Data := Pointer(LongWord(DisplayListItems.Items[I]));
        LItem.Caption := IntToStr(TIPLFile(GFiles.Item[SelFile]).Inst.Item[LongWord(DisplayListItems.Items[I])].ID) + ' - ' + TIPLFile(GFiles.Item[SelFile]).Inst.Item[LongWord(DisplayListItems.Items[I])].ModelName;
      end;
      CenPos := FormEditor.GetPos;
      FormEditor.EditIPLMultInstItemCount.Caption := IntToStr(FormEditor.EditIPLMultInstList.Items.Count) + ' Items';;
      FormEditor.EditIPLMultInstItemCenter.Caption := Format('%1.6g, %1.6g, %1.6g', [CenPos[0], CenPos[2], CenPos[2]]);
    end else
      if (DisplayListItems.Count > 0) then for I := 0 to DisplayListItems.Count - 1 do
        SelectItem(I, True);
  end;
end;

procedure TFormEditorIPL.ActionEditDuplicateExecute(Sender: TObject);
var
  I, Temp: LongWord;
begin
  if not (GFiles = nil) and not (SelFile = -1) and not (ListItems.ItemIndex = -1) then
  begin
    Temp := DuplicateItem(LongWord(DisplayListItems.Items[ListItems.ItemIndex]));
    if (DisplayListItems.Count > 0) then for I := 0 to DisplayListItems.Count - 1 do
      if (Temp = LongWord(DisplayListItems.Items[I])) then
        ListItems.ItemIndex := I;
    SelectItem(ListItems.ItemIndex, False);
  end;
end;

procedure TFormEditorIPL.PopupEditItemClick(Sender: TObject);
begin
  SelectItem(ListItems.ItemIndex, False);
end;

end.
