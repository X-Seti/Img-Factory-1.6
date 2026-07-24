unit EditorArchive;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GTAImg, ComCtrls, ActnList, ToolWin, ExtraView, TextureView, GLView,
  Menus;

type
  TFormEditorArchive = class(TForm)
    ListArchive: TListView;
    TBArchive: TToolBar;
    MainActionList: TActionList;
    TBEditInsert: TToolButton;
    ActionEditInsert: TAction;
    ActionEditDelete: TAction;
    TBEditDelete: TToolButton;
    DlgAdd: TOpenDialog;
    ActionEditExtract: TAction;
    TBEditExtract: TToolButton;
    TBSep1: TToolButton;
    ActionEditInsertDff: TAction;
    ActionEditInsertTxd: TAction;
    PopupMenu1: TPopupMenu;
    PopupExtract: TMenuItem;
    N2: TMenuItem;
    PopupInsert: TMenuItem;
    PopupDelete: TMenuItem;
    procedure ListArchiveData(Sender: TObject; Item: TListItem);
    procedure ActionEditInsertExecute(Sender: TObject);
    procedure ActionEditDeleteExecute(Sender: TObject);
    procedure ActionEditExtractExecute(Sender: TObject);
    procedure ListArchiveSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListArchiveColumnClick(Sender: TObject; Column: TListColumn);
    procedure ActionEditInsertDffExecute(Sender: TObject);
    procedure ActionEditInsertTxdExecute(Sender: TObject);
  private
    MainAction: TAction;
    LastSortedBy: Integer;
    procedure UpdateListSize;
    procedure Sort(SortBy: Byte; SortReverse: Boolean);
   public
    DisplayList: TList;
    constructor Create(AOwner: TComponent); override;

    procedure AddFiles(Files: TStrings);
    procedure ExtractSelected(OutDir: string);
    procedure ExtractEntry(Index: Integer; FileName: string);

    procedure SetAction(inAction: TAction);
  end;

  function CompareItems(Item1, Item2: LongWord): Integer;
  function GetTypeName(Name: string): string;
  function GetTypeIndex(Name: string): Integer;

implementation

uses Loading, DirDialog, Main;

var
  DisplaySortBy: Byte;
  DisplayReverse: Boolean;

{$R *.dfm}

constructor TFormEditorArchive.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LastSortedBy := -1;
  DisplayList := TList.Create;
  if GArchive.ReadOnly then
  begin
    ActionEditInsert.Enabled := False;
    ActionEditDelete.Enabled := False;
  end;
  UpdateListSize;
end;

function CompareItems(Item1, Item2: LongWord): Integer;
begin
  with GArchive do case DisplaySortBy of
    0: Result := Entry[Item1].Index - Entry[Item2].Index;
    1: Result := CompareText(Trim(Entry[Item1].Name), Trim(Entry[Item2].Name));
    2: begin
      Result := CompareText(GetTypeName(Trim(Entry[Item1].Name)), GetTypeName(Trim(Entry[Item2].Name)));
      if (Result = 0) then
        Result := CompareText(Trim(Entry[Item1].Name), Trim(Entry[Item2].Name));
    end;
    3: begin
      Result := Entry[Item1].BlockCount - Entry[Item2].BlockCount;
      if (Result = 0) then
        Result := CompareText(Trim(Entry[Item1].Name), Trim(Entry[Item2].Name));
    end;
  else
    Result := 0;
  end;
  if DisplayReverse then
    Result := -Result;
end;

procedure TFormEditorArchive.Sort(SortBy: Byte; SortReverse: Boolean);
begin
  DisplaySortBy := SortBy;
  DisplayReverse := SortReverse;
  if (DisplayList.Count > 0) then
    DisplayList.Sort(@CompareItems);
  ListArchive.ItemIndex := -1;
  ListArchive.Repaint;
end;

procedure TFormEditorArchive.UpdateListSize;
var
  I: LongWord;
begin
  DisplayList.Clear;
  if (GArchive.EntryCount > 0) then
    for I := 0 to GArchive.EntryCount - 1 do
      DisplayList.Add(Pointer(I));
  ListArchive.Items.Count := DisplayList.Count;
  ListArchive.Repaint;
end;

procedure TFormEditorArchive.ListArchiveData(Sender: TObject;
  Item: TListItem);
begin
  if not (GArchive = nil) then
  begin
    Item.ImageIndex := GetTypeIndex(Item.Caption);
    if ListArchive.ViewStyle = vsReport then
    begin
      Item.Caption := IntToStr(GArchive.Entry[LongWord(DisplayList.Items[Item.Index])].Index);
      Item.SubItems.Add(Trim(GArchive.Entry[LongWord(DisplayList.Items[Item.Index])].Name));
      Item.SubItems.Add(GetTypeName(Trim(GArchive.Entry[LongWord(DisplayList.Items[Item.Index])].Name)));
      Item.SubItems.Add(IntToStr(GArchive.Entry[LongWord(DisplayList.Items[Item.Index])].BlockCount div 2) + ' kB');
    end else
    begin
      Item.Caption := Trim(GArchive.Entry[LongWord(DisplayList.Items[Item.Index])].Name);
    end;
  end;
end;

function GetTypeName(Name: string): string;
var
  Tmp: string;
begin
  Tmp := Uppercase(ExtractFileExt(Name)) + ' File';
  Result := Copy(Tmp, 2, Length(Tmp)-1);
end;

function GetTypeIndex(Name: string): Integer;
var Ext: string;
begin
  Result := 0;
  Ext := LowerCase(ExtractFileExt(Name));
  if Ext = '.txd' then
    Result := 1
  else if Ext = '.dff' then
    Result := 2
  else
end;

procedure TFormEditorArchive.ActionEditInsertExecute(Sender: TObject);
begin
  dlgAdd.Options := [ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofHideReadOnly, ofEnableSizing];
  dlgAdd.Filter := 'Supported Formats (*.txd, *.dff)|*.TXD;*.DFF|All Files (*.*)|*.*';
  dlgAdd.FilterIndex := 1;
  if dlgAdd.Execute then
    AddFiles(dlgAdd.Files);
end;

procedure TFormEditorArchive.AddFiles(Files: TStrings);
var
  i: integer;
  f: TFileStream;
begin
  Screen.Cursor := crHourGlass;
  try
    with TFormLoading.Create(Application) do
    try
      SetStatus('Adding File(s)');
      SetMax(Files.Count);
      if (Files.Count > 0) then for i := 0 to Files.Count -1 do
      begin
        f := TFileStream.Create(Files.Strings[i], fmShareDenyNone);
        GArchive.Add(ExtractFileName(Files.Strings[i]), f);
        IncPos;
        f.Free;
      end;
      UpdateListSize;
    finally
      Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormEditorArchive.ActionEditDeleteExecute(Sender: TObject);
var i: integer;
begin
  if (ListArchive.SelCount > 0) and (MessageDlg('Are you sure you want to delete the selected files?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    try
      for i := 0 to ListArchive.Items.Count -1 do
      begin
        if ListArchive.Items[i].Selected then
        begin
          GArchive.MarkForRemoval(LongWord(DisplayList.Items[i]));
        end;
      end;
    finally
      GArchive.DoRemove;
      UpdateListSize;
      ListArchive.ClearSelection;
    end;
  end;
end;

procedure TFormEditorArchive.ActionEditExtractExecute(Sender: TObject);
var
  Dir: string;
  DirD: TDirDialog;
begin
  DirD := TDirDialog.Create(Application, False);
  DirD.Title := 'Please Specify Location To Extract Files:';

  if DirD.Execute then
  begin
    Dir := IncludeTrailingBackslash(DirD.DirName);
    ExtractSelected(Dir);
  end;
end;

procedure TFormEditorArchive.ExtractSelected(OutDir: string);
var i: Integer;
begin
  if (ListArchive.SelCount > 0) then with TFormLoading.Create(Application) do
  try
    SetStatus('Extracting File(s)');
    SetMax(ListArchive.SelCount);
    for i := 0 to ListArchive.Items.Count -1 do
    begin
      if ListArchive.Items[i].Selected then
      begin
        SetPart(Trim(GArchive.Entry[LongWord(DisplayList.Items[i])].Name));
        SetPartMax(1);
        ExtractEntry(LongWord(DisplayList.Items[i]), OutDir + Trim(GArchive.Entry[LongWord(DisplayList.Items[i])].Name));
        IncPartPos; IncPos;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TFormEditorArchive.ExtractEntry(Index: Integer; FileName: string);
var f: TFileStream;
begin
  f := TFileStream.Create(FileName, fmCreate);
  GArchive.Extract(Index, f);
  f.Free;
end;

procedure TFormEditorArchive.ListArchiveSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  Name: String;
begin
  if not (Item = nil) then
  begin
    Name := ChangeFileExt(Item.SubItems.Strings[0], '');
    if (CompareText(ExtractFileExt(Item.SubItems.Strings[0]), '.dff') = 0) then
      FormExtra.SetModelTexture(Name, GArchive.GetTXDNum(Name));
    if (CompareText(ExtractFileExt(Item.SubItems.Strings[0]), '.txd') = 0) then
      FormTexture.SetTexture(GArchive.GetTXDNum(Name));
  end else
    FormExtra.SetModelTexture('', -1);
end;

procedure TFormEditorArchive.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if not (MainAction = nil) then
    MainAction.Checked := False;
end;

procedure TFormEditorArchive.SetAction(inAction: TAction);
begin
  MainAction := inAction;
end;

procedure TFormEditorArchive.ListArchiveColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  if (LastSortedBy = Column.Index) then
  begin
    Sort(Column.Index, True);
    LastSortedBy := -1;
  end else
  begin
    Sort(Column.Index, False);
    LastSortedBy := Column.Index;
  end;
end;

procedure TFormEditorArchive.ActionEditInsertDffExecute(Sender: TObject);
begin
  dlgAdd.Options := [ofPathMustExist, ofFileMustExist, ofHideReadOnly, ofEnableSizing];
  dlgAdd.Filter := 'Renderware Model File (*.dff)|*.dff';
  dlgAdd.FilterIndex := 0;
  if dlgAdd.Execute then
  begin
    ActionEditInsertDff.Tag := 0;
    AddFiles(dlgAdd.Files);
  end else
    ActionEditInsertDff.Tag := 1;
end;

procedure TFormEditorArchive.ActionEditInsertTxdExecute(Sender: TObject);
begin
  dlgAdd.Options := [ofPathMustExist, ofFileMustExist, ofHideReadOnly, ofEnableSizing];
  dlgAdd.Filter := 'Texture Library File (*.txd)|*.txd';
  dlgAdd.FilterIndex := 0;
  if dlgAdd.Execute then
  begin
    ActionEditInsertTxd.Tag := 0;
    AddFiles(dlgAdd.Files);
  end else
    ActionEditInsertTxd.Tag := 1;
end;

end.
