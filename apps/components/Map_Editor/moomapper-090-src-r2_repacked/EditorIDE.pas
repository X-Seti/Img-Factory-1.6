unit EditorIDE;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, ActnList, GTAText, ExtraView, GTAImg, GLView,
  ToolWin, Menus;

type
  TFormEditorIDE = class(TForm)
    ListFiles: TListView;
    RadioSection: TRadioGroup;
    ListItems: TListView;
    MainToolbar: TToolBar;
    TBEditAdd: TToolButton;
    TBEditDel: TToolButton;
    MainActionList: TActionList;
    ActionEditAdd: TAction;
    ActionEditDelete: TAction;
    ActionEditUseDefinition: TAction;
    TBSep1: TToolButton;
    TBEditUseDefinition: TToolButton;
    ActionFileNew: TAction;
    ActionFileClose: TAction;
    ActionFileOpen: TAction;
    ActionFileDelete: TAction;
    FileToolbar: TToolBar;
    TBFileOpen: TToolButton;
    TBFileClose: TToolButton;
    TBSep: TToolButton;
    TBFileNew: TToolButton;
    TBFileDelete: TToolButton;
    DlgNewFile: TSaveDialog;
    DlgOpenFile: TOpenDialog;
    ActionEditAddInstance: TAction;
    TBEditAddInstance: TToolButton;
    PopupItems: TPopupMenu;
    PopupAddItem: TMenuItem;
    PopupDeleteItem: TMenuItem;
    N1: TMenuItem;
    PopupEditItem: TMenuItem;
    PopupAddInstance: TMenuItem;
    PopupUseDefinition: TMenuItem;
    N2: TMenuItem;
    PopupFiles: TPopupMenu;
    PopupFileClose: TMenuItem;
    PopupFileDelete: TMenuItem;
    PopupFileNew: TMenuItem;
    PopupFileOpen: TMenuItem;
    N3: TMenuItem;
    procedure RadioSectionClick(Sender: TObject);
    procedure ListFilesData(Sender: TObject; Item: TListItem);
    procedure ListFilesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ListItemsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ListItemsData(Sender: TObject; Item: TListItem);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListFilesColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListItemsColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListItemsDblClick(Sender: TObject);
    procedure ActionEditDeleteExecute(Sender: TObject);
    procedure ListItemsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ActionEditAddExecute(Sender: TObject);
    procedure ActionEditUseDefinitionExecute(Sender: TObject);
    procedure ActionFileCloseExecute(Sender: TObject);
    procedure ActionFileDeleteExecute(Sender: TObject);
    procedure ActionFileNewExecute(Sender: TObject);
    procedure ActionFileOpenExecute(Sender: TObject);
    procedure ActionEditAddInstanceExecute(Sender: TObject);
    procedure PopupEditItemClick(Sender: TObject);
  private
    LastSortedFilesBy: Integer;
    LastSortedItemsBy: Integer;

    MainAction: TAction;
    SelFile: LongInt;
    SelSection: Byte;

    function AddItem: LongInt;
    procedure DeleteItem(inItem: LongInt);
  public
    DisplayListFiles: TList;
    DisplayListItems: TList;

    procedure SelectItem(inItem: LongInt);
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

uses Main;

{$R *.dfm}

constructor TFormEditorIDE.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DisplayListFiles := TList.Create;
  DisplayListItems := TList.Create;
  LastSortedFilesBy := -1;
  LastSortedItemsBy := -1;

  SelFile := -1;
  SelSection := SECTION_OBJS;
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
    SECTION_OBJS:
    begin
      with TIDEFile(Main.GFiles.Item[DisplayFile]).Objs do case DisplaySortBy of
        0: Result := Item[Item1].ID - Item[Item2].ID;
        1: Result := CompareText(Item[Item1].ModelName, Item[Item2].ModelName);
      else
        Result := 0;
      end;
    end;
    SECTION_TOBJ:
    begin
      with TIDEFile(Main.GFiles.Item[DisplayFile]).TObj do case DisplaySortBy of
        0: Result := Item[Item1].ID - Item[Item2].ID;
        1: Result := CompareText(Item[Item1].ModelName, Item[Item2].ModelName);
      else
        Result := 0;
      end;
    end;
    SECTION_PATH:
    begin
      with TIDEFile(Main.GFiles.Item[DisplayFile]).Path do case DisplaySortBy of
        0: Result := Item[Item1].ID - Item[Item2].ID;
        1: Result := CompareText(Item[Item1].ModelName, Item[Item2].ModelName);
      else
        Result := 0;
      end;
    end;
    SECTION_2DFX:
    begin
      with TIDEFile(Main.GFiles.Item[DisplayFile]).Wdfx do case DisplaySortBy of
        0, 1: Result := Item1 - Item2;
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

procedure TFormEditorIDE.SortFiles(SortBy: Byte; SortReverse: Boolean);
begin
  DisplaySortBy := SortBy;
  DisplayReverse := SortReverse;
  if (DisplayListFiles.Count > 0) then
    DisplayListFiles.Sort(@CompareFileItems);
  ListItems.ItemIndex := -1;
  UpdateListFileSize;
  ListFiles.Repaint;
end;

procedure TFormEditorIDE.SortItems(SortBy: Byte; SortReverse: Boolean);
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

procedure TFormEditorIDE.UpdateListFileSize;
var
  I: LongWord;
begin
  DisplayListFiles.Clear;
  if (GFiles.Count > 0) then for I := 0 to GFiles.Count - 1 do
    if (GFiles.Item[I].SubType = FILE_IDE) then
      DisplayListFiles.Add(Pointer(I));
  ListFiles.Items.Count := DisplayListFiles.Count;
  ListFiles.Repaint;
end;

procedure TFormEditorIDE.UpdateListItemsSize;
var
  I: LongWord;
begin
  if not (ListFiles.ItemIndex = -1) then
  begin
    SelFile := LongWord(DisplayListFiles.Items[ListFiles.ItemIndex]);
    case SelSection of
      SECTION_OBJS:
      begin
        DisplayListItems.Clear;
        with TIDEFile(GFiles.Item[SelFile]).Objs do
          if (Count > 0) then for I := 0 to Count - 1 do
            DisplayListItems.Add(Pointer(I));
      end;
      SECTION_TOBJ:
      begin
        DisplayListItems.Clear;
        with TIDEFile(GFiles.Item[SelFile]).TObj do
          if (Count > 0) then for I := 0 to Count - 1 do
            DisplayListItems.Add(Pointer(I));
      end;
      SECTION_PATH:
      begin
        DisplayListItems.Clear;
        with TIDEFile(GFiles.Item[SelFile]).Path do
          if (Count > 0) then for I := 0 to Count - 1 do
            DisplayListItems.Add(Pointer(I));
      end;
      SECTION_2DFX:
      begin
        DisplayListItems.Clear;
        with TIDEFile(GFiles.Item[SelFile]).Wdfx do
          if (Count > 0) then for I := 0 to Count - 1 do
            DisplayListItems.Add(Pointer(I));
      end;
    else
      DisplayListItems.Clear;
    end;
  end else
    DisplayListItems.Clear;
  ListItems.Items.Count := DisplayListItems.Count;
  ActionEditAddInstance.Enabled := False;
  ListItems.Repaint;
end;

procedure TFormEditorIDE.ListFilesData(Sender: TObject; Item: TListItem);
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

procedure TFormEditorIDE.ListItemsData(Sender: TObject; Item: TListItem);
var
  TempStr: String;
begin
  if not (GFiles = nil) and not (SelFile = -1) then
  begin
    if ListItems.ViewStyle = vsReport then
    begin
      case SelSection of
        SECTION_OBJS:
        begin
          Item.Caption := IntToStr(TIDEFile(GFiles.Item[SelFile]).Objs.Item[LongWord(DisplayListItems.Items[Item.Index])].ID);
          Item.SubItems.Add(TIDEFile(GFiles.Item[SelFile]).Objs.Item[LongWord(DisplayListItems.Items[Item.Index])].ModelName);
        end;
        SECTION_TOBJ:
        begin
          Item.Caption := IntToStr(TIDEFile(GFiles.Item[SelFile]).TObj.Item[LongWord(DisplayListItems.Items[Item.Index])].ID);
          Item.SubItems.Add(TIDEFile(GFiles.Item[SelFile]).TObj.Item[LongWord(DisplayListItems.Items[Item.Index])].ModelName);
        end;
        SECTION_PATH:
        begin
          Item.Caption := IntToStr(TIDEFile(GFiles.Item[SelFile]).Path.Item[LongWord(DisplayListItems.Items[Item.Index])].ID);
          Item.SubItems.Add(TIDEFile(GFiles.Item[SelFile]).Path.Item[LongWord(DisplayListItems.Items[Item.Index])].ModelName);
        end;
        SECTION_2DFX:
        begin
          Item.Caption := IntToStr(TIDEFile(GFiles.Item[SelFile]).Wdfx.Item[LongWord(DisplayListItems.Items[Item.Index])].ID);
          with TIDEFile(GFiles.Item[SelFile]).Wdfx.Item[LongWord(DisplayListItems.Items[Item.Index])] do
          begin
            case EffectType of
              EFFECT_LIGHT: TempStr := ' - Light';
              EFFECT_PARTICLE: TempStr := ' - Particle';
              EFFECT_UNKNOWN: TempStr := ' - Unknown';
              EFFECT_ANIMATION: TempStr := ' - Animation';
              EFFECT_REFLECTION: TempStr := ' - Reflection';
            else
              TempStr := '';
            end;
            Item.SubItems.Add(Format('(%d, %d, %d)%s', [Color[0], Color[1], Color[2], TempStr]));
          end;

        end;
      else
        ListItems.Items.Count := 0;
      end;
    end else
    begin
      Item.Caption := IntToStr(Item.Index);
    end;
  end;
end;

procedure TFormEditorIDE.ListFilesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  UpdateListItemsSize;
end;

procedure TFormEditorIDE.ListItemsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if not (Item = nil) then
  begin
    ActionEditAddInstance.Enabled := True;
    case SelSection of
      SECTION_OBJS: with TIDEFile(GFiles.Item[SelFile]).Objs.Item[LongWord(DisplayListItems.Items[Item.Index])] do
        FormExtra.SetObject(ID);
      SECTION_TOBJ: with TIDEFile(GFiles.Item[SelFile]).TObj.Item[LongWord(DisplayListItems.Items[Item.Index])] do
        FormExtra.SetTObject(ID);
    end;
  end else
  begin
    ActionEditAddInstance.Enabled := False;
    case SelSection of
      SECTION_OBJS: FormExtra.SetModelTexture('', -1);
    end;
  end;
end;

procedure TFormEditorIDE.RadioSectionClick(Sender: TObject);
begin
  case RadioSection.ItemIndex of
    0:  SelSection := SECTION_OBJS;
    1:  SelSection := SECTION_TOBJ;
    2:  SelSection := SECTION_PATH;
    3:  SelSection := SECTION_2DFX;
  end;
  UpdateListItemsSize;
end;

procedure TFormEditorIDE.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if not (MainAction = nil) then
    MainAction.Checked := False;
end;

procedure TFormEditorIDE.SetAction(inAction: TAction);
begin
  MainAction := inAction;
end;

procedure TFormEditorIDE.ListFilesColumnClick(Sender: TObject;
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

procedure TFormEditorIDE.ListItemsColumnClick(Sender: TObject;
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

procedure TFormEditorIDE.ListItemsDblClick(Sender: TObject);
begin
  SelectItem(ListItems.ItemIndex);
end;

procedure TFormEditorIDE.ActionEditDeleteExecute(Sender: TObject);
begin
  DeleteItem(ListItems.ItemIndex);
end;

procedure TFormEditorIDE.SelectItem(inItem: LongInt);
begin
  if not (GFiles = nil) and not (SelFile = -1) and not (inItem = -1) then
  begin
    FormEditor.SetToMode(FILE_IDE, SelSection, SelFile, LongWord(DisplayListItems.Items[inItem]));
  end else
    FormEditor.SetToNone;
end;

procedure TFormEditorIDE.DeleteItem(inItem: LongInt);
begin
  if ListItems.ItemIndex = inItem then
  begin
    SelectItem(-1);
  end;
  if not (GFiles = nil) and not (SelFile = -1) and not (inItem = -1) then
  begin
    case SelSection of
      SECTION_OBJS:
        TIDEFile(GFiles.Item[SelFile]).Objs.DeleteItem(LongWord(DisplayListItems.Items[inItem]));
      SECTION_TOBJ:
        TIDEFile(GFiles.Item[SelFile]).TObj.DeleteItem(LongWord(DisplayListItems.Items[inItem]));
      SECTION_PATH:
        TIDEFile(GFiles.Item[SelFile]).Path.DeleteItem(LongWord(DisplayListItems.Items[inItem]));
      SECTION_2DFX:
        TIDEFile(GFiles.Item[SelFile]).Wdfx.DeleteItem(LongWord(DisplayListItems.Items[inItem]));
    end;
    UpdateListItemsSize;
  end;
end;

function TFormEditorIDE.AddItem: LongInt;
begin
  if not (GFiles = nil) and not (SelFile = -1) then
  begin
    case SelSection of
      SECTION_OBJS:
        Result := TIDEFile(GFiles.Item[SelFile]).Objs.AddItem;
      SECTION_TOBJ:
        Result := TIDEFile(GFiles.Item[SelFile]).TObj.AddItem;
      SECTION_PATH:
        Result := TIDEFile(GFiles.Item[SelFile]).Path.AddItem;
      SECTION_2DFX:
        Result := TIDEFile(GFiles.Item[SelFile]).Wdfx.AddItem;
    else
      Result := -1;
    end;
    UpdateListItemsSize;
  end else
    Result := -1;
end;

procedure TFormEditorIDE.ListItemsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then
    DeleteItem(ListItems.ItemIndex);
  if Key = VK_RETURN then
    SelectItem(ListItems.ItemIndex);
end;

procedure TFormEditorIDE.ActionEditAddExecute(Sender: TObject);
begin
  ListItems.ItemIndex := AddItem;
  SelectItem(ListItems.ItemIndex);
end;

procedure TFormEditorIDE.ActionEditUseDefinitionExecute(Sender: TObject);
begin
  if not (GFiles = nil) and not (SelFile = -1) and not (ListItems.ItemIndex = -1) then
  begin
    case SelSection of
      SECTION_OBJS:
        with TIDEFile(GFiles.Item[SelFile]).Objs.Item[LongWord(DisplayListItems.Items[ListItems.ItemIndex])] do
          FormEditor.UpdateIPLDefinition(ID, ModelName);
      SECTION_TOBJ:
        with TIDEFile(GFiles.Item[SelFile]).TObj.Item[LongWord(DisplayListItems.Items[ListItems.ItemIndex])] do
          FormEditor.UpdateIPLDefinition(ID, ModelName);
    end;
  end;
end;

procedure TFormEditorIDE.ActionFileCloseExecute(Sender: TObject);
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
        FormDAT.RemoveIDE(FileName);
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

procedure TFormEditorIDE.ActionFileDeleteExecute(Sender: TObject);
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
        FormDAT.RemoveIDE(FileName);
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

procedure TFormEditorIDE.ActionFileNewExecute(Sender: TObject);
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
        FormDAT.AddIDE(TempStr);
      end;
      Inc(GFiles.Count);
      SetLength(GFiles.Item, GFiles.Count);
      GFiles.Item[GFiles.Count - 1] := TIDEFile.Create;
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

procedure TFormEditorIDE.ActionFileOpenExecute(Sender: TObject);
var
  VerFileName, TempStr: String;
  Valid: Boolean;
  AddFile: TGTASFile;
begin
  DlgOpenFile.InitialDir := GTAPath + 'DATA\';
  AddFile := nil;
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
      AddFile := TIDEFile.Create;
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
        FormDAT.AddIDE(TempStr);
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

procedure TFormEditorIDE.ActionEditAddInstanceExecute(Sender: TObject);
var
  I, Temp: LongWord;
begin
  if not (GFiles = nil) and not (SelFile = -1) and not (ListItems.ItemIndex = -1) then
  begin
    if (FormIPL.SelFile = -1) then
    begin
      MessageDlg('Please ensure a file is selected in the Item Placement panel!', mtError, [mbOk], 0);
    end else
    begin
      if (MessageDlg('Add item instance of selected object to: ' + GFiles.Item[FormIPL.SelFile].Name + '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      begin
        Temp := FormIPL.AddItem;
        if (FormIPL.DisplayListItems.Count > 0) then for I := 0 to FormIPL.DisplayListItems.Count - 1 do
          if (Temp = LongWord(FormIPL.DisplayListItems.Items[I])) then
            FormIPL.ListItems.ItemIndex := I;
        FormIPL.SelectItem(FormIPL.ListItems.ItemIndex, False);
        case SelSection of
          SECTION_OBJS:
            with TIDEFile(GFiles.Item[SelFile]).Objs.Item[LongWord(DisplayListItems.Items[ListItems.ItemIndex])] do
              FormEditor.UpdateIPLDefinition(ID, ModelName);
          SECTION_TOBJ:
            with TIDEFile(GFiles.Item[SelFile]).TObj.Item[LongWord(DisplayListItems.Items[ListItems.ItemIndex])] do
              FormEditor.UpdateIPLDefinition(ID, ModelName);
        end;
      end;
    end;
  end;
end;

procedure TFormEditorIDE.PopupEditItemClick(Sender: TObject);
begin
  SelectItem(ListItems.ItemIndex);
end;

end.
