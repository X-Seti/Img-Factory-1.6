unit EditorDAT;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, ExtCtrls, StrUtils;

type
  TFormEditorDAT = class(TForm)
    ListDAT: TListBox;
    PanelUpdate: TPanel;
    EditLine: TEdit;
    BtnUpdate: TButton;
    BtnSave: TButton;
    BtnAdd: TButton;
    PanelMove: TPanel;
    BtnUp: TButton;
    BtnDown: TButton;
    Label1: TLabel;
    BtnDel: TButton;
    procedure ListDATClick(Sender: TObject);
    procedure BtnUpdateClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnAddClick(Sender: TObject);
    procedure BtnUpClick(Sender: TObject);
    procedure BtnDownClick(Sender: TObject);
    procedure BtnDelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FirstShow: Boolean;
    MainAction: TAction;
  public
    procedure AddIDE(InName: String);
    procedure AddIPL(InName: String);
    procedure RemoveIDE(InName: String);
    procedure RemoveIPL(InName: String);
    procedure DeleteLine(InNum: LongWord);

    constructor Create(AOwner: TComponent); override;
    procedure UpdateListDAT;
    procedure SetAction(inAction: TAction);
  end;

implementation

uses Main, GTAText;

{$R *.dfm}

constructor TFormEditorDAT.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  UpdateListDAT;
end;

procedure TFormEditorDAT.SetAction(inAction: TAction);
begin
  MainAction := inAction;
end;

procedure TFormEditorDAT.UpdateListDAT;
var
  I: LongWord;
begin
  ListDAT.Clear;
  if (GFiles.DATFile.Count > 0) then for I := 0 to GFiles.DATFile.Count - 1 do
    ListDat.Items.Add(GFiles.DATFile.Strings[I]);
  BtnSave.Enabled := GFiles.DatChanged;
end;

procedure TFormEditorDAT.ListDATClick(Sender: TObject);
begin
  if not (ListDAT.ItemIndex = -1) then
  begin
    BtnUpdate.Enabled := True;
    EditLine.Text := GFiles.DatFile.Strings[ListDAT.ItemIndex];
  end else
  begin
    BtnUpdate.Enabled := False;
    EditLine.Text := '';
  end;
end;

procedure TFormEditorDAT.BtnUpdateClick(Sender: TObject);
begin
  if not (ListDAT.ItemIndex = -1) then
  begin
    ListDAT.Items.Strings[ListDAT.ItemIndex] := Trim(EditLine.Text);
    GFiles.DatFile.Strings[ListDAT.ItemIndex] := Trim(EditLine.Text);
    GFiles.DatChanged := True;
    BtnSave.Enabled := True;
  end;
end;

procedure TFormEditorDAT.BtnSaveClick(Sender: TObject);
var
  VerFileName: String;
begin
  if (GTA_VICE_MODE) then
    VerFileName := VICE_DAT_FILE
  else
    VerFileName := GTA3_DAT_FILE;
  if (GFiles.SaveDat = 0) then
  begin
    GFiles.DatChanged := False;
    BtnSave.Enabled := False;
    MessageDlg('SUCCESS: The DAT file "' + VerFileName + '" was saved successfully!', mtError, [mbOk], 0)
  end else
    MessageDlg('ERROR: The DAT file "' + VerFileName + '" failed to save.' + #13#10 +
               'This file could be read only or in use by another program!', mtError, [mbOk], 0);
end;

procedure TFormEditorDAT.BtnAddClick(Sender: TObject);
var
  I: LongWord;
begin
  if (ListDAT.ItemIndex = -1) then
  begin
    ListDAT.Items.Add(Trim(EditLine.Text));
    GFiles.DatFile.Add(Trim(EditLine.Text));
  end else
  begin
    ListDAT.Items.Add('');
    GFiles.DatFile.Add('');
    if (GFiles.DatFile.Count > 1) and (ListDAT.ItemIndex < GFiles.DatFile.Count - 1) then for I := GFiles.DatFile.Count - 2 downto ListDAT.ItemIndex do
    begin
      GFiles.DatFile.Strings[I + 1] := GFiles.DatFile.Strings[I];
      ListDAT.Items.Strings[I + 1] := ListDAT.Items.Strings[I];
    end;
    GFiles.DatFile.Strings[ListDAT.ItemIndex] := Trim(EditLine.Text);
    ListDAT.Items.Strings[ListDAT.ItemIndex] := Trim(EditLine.Text);
  end;
  GFiles.DatChanged := True;
  BtnSave.Enabled := True;
end;

procedure TFormEditorDAT.BtnUpClick(Sender: TObject);
var
  TempStr: String;
begin
  if not (ListDAT.ItemIndex = -1) and (ListDAT.ItemIndex > 0) then
  begin
    TempStr := GFiles.DatFile.Strings[ListDAT.ItemIndex];
    GFiles.DatFile.Strings[ListDAT.ItemIndex] := GFiles.DatFile.Strings[ListDAT.ItemIndex - 1];
    GFiles.DatFile.Strings[ListDAT.ItemIndex - 1] := TempStr;
    ListDAT.Items.Strings[ListDAT.ItemIndex] := GFiles.DatFile.Strings[ListDAT.ItemIndex];
    ListDAT.Items.Strings[ListDAT.ItemIndex - 1] := GFiles.DatFile.Strings[ListDAT.ItemIndex - 1];
    ListDAT.ItemIndex := ListDAT.ItemIndex - 1;
    GFiles.DatChanged := True;
    BtnSave.Enabled := True;
  end;
end;

procedure TFormEditorDAT.BtnDownClick(Sender: TObject);
var
  TempStr: String;
begin
  if not (ListDAT.ItemIndex = -1) and (ListDAT.ItemIndex < GFiles.DatFile.Count - 1) then
  begin
    TempStr := GFiles.DatFile.Strings[ListDAT.ItemIndex];
    GFiles.DatFile.Strings[ListDAT.ItemIndex] := GFiles.DatFile.Strings[ListDAT.ItemIndex + 1];
    GFiles.DatFile.Strings[ListDAT.ItemIndex + 1] := TempStr;
    ListDAT.Items.Strings[ListDAT.ItemIndex] := GFiles.DatFile.Strings[ListDAT.ItemIndex];
    ListDAT.Items.Strings[ListDAT.ItemIndex + 1] := GFiles.DatFile.Strings[ListDAT.ItemIndex + 1];
    ListDAT.ItemIndex := ListDAT.ItemIndex + 1;
    GFiles.DatChanged := True;
    BtnSave.Enabled := True;
  end;
end;

procedure TFormEditorDAT.BtnDelClick(Sender: TObject);
begin
  if not (ListDAT.ItemIndex = -1) then
  begin
    DeleteLine(ListDAT.ItemIndex);
    if (ListDAT.ItemIndex = -1) then
    begin
      ListDAT.ItemIndex := -1;
      BtnUpdate.Enabled := False;
      EditLine.Text := '';
    end else
      EditLine.Text := GFiles.DatFile.Strings[ListDAT.ItemIndex];
  end;
end;

procedure TFormEditorDAT.DeleteLine(InNum: LongWord);
var
  I: LongWord;
begin
  if (GFiles.DatFile.Count > 1) and (Integer(InNum) < GFiles.DatFile.Count - 1) then for I := InNum to GFiles.DatFile.Count - 2 do
  begin
    GFiles.DatFile.Strings[I] := GFiles.DatFile.Strings[I + 1];
    ListDAT.Items.Strings[I] := ListDAT.Items.Strings[I + 1];
  end;
  GFiles.DatFile.Delete(GFiles.DatFile.Count - 1);
  ListDAT.Items.Delete(ListDAT.Items.Count - 1);
  GFiles.DatChanged := True;
  BtnSave.Enabled := True;
end;

procedure TFormEditorDAT.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if not (MainAction = nil) then
    MainAction.Checked := False;
end;

procedure TFormEditorDAT.RemoveIDE(InName: String);
var
  I: LongWord;
  Found: LongInt;
begin
  Found := -1;
  if (GFiles.DatFile.Count > 0) then for I := 0 to GFiles.DatFile.Count - 1 do
    if (CompareText('IDE ' + InName, GFiles.DatFile.Strings[I]) = 0) then
      Found := I;
  if not (Found = -1) then
  begin
    DeleteLine(Found);
    ListDAT.ItemIndex := -1;
    BtnUpdate.Enabled := False;
    EditLine.Text := '';
  end;
end;

procedure TFormEditorDAT.RemoveIPL(InName: String);
var
  I: LongWord;
  Found: LongInt;
begin
  Found := -1;
  if (GFiles.DatFile.Count > 0) then for I := 0 to GFiles.DatFile.Count - 1 do
    if (CompareText('IPL ' + InName, GFiles.DatFile.Strings[I]) = 0) then
      Found := I;
  if not (Found = -1) then
  begin
    DeleteLine(Found);
    ListDAT.ItemIndex := -1;
    BtnUpdate.Enabled := False;
    EditLine.Text := '';
  end;
end;

procedure TFormEditorDAT.AddIDE(InName: String);
var
  I: LongWord;
  Found: LongInt;
begin
  Found := 0;
  
  // insert before which item
  if (GFiles.DatFile.Count > 0) then for I := 0 to GFiles.DatFile.Count - 1 do
    if (Length(GFiles.DatFile.Strings[I]) > 3) and (CompareText(LeftStr(GFiles.DatFile.Strings[I], 3), 'IDE') = 0) then
      Found := I + 1;

  ListDAT.Items.Add('');
  GFiles.DatFile.Add('');
  if (GFiles.DatFile.Count > 1) and (Found < GFiles.DatFile.Count - 1) then for I := GFiles.DatFile.Count - 2 downto Found do
  begin
    GFiles.DatFile.Strings[I + 1] := GFiles.DatFile.Strings[I];
    ListDAT.Items.Strings[I + 1] := ListDAT.Items.Strings[I];
  end;
  GFiles.DatFile.Strings[Found] := Trim('IDE ' + InName);
  ListDAT.Items.Strings[Found] := Trim('IDE ' + InName);

  GFiles.DatChanged := True;
  BtnSave.Enabled := True;
  ListDAT.ItemIndex := -1;
  BtnUpdate.Enabled := False;
  EditLine.Text := '';
end;

procedure TFormEditorDAT.AddIPL(InName: String);
var
  I: LongWord;
  Found: LongInt;
begin
  // insert before which item
  Found := 0;

  // insert before which item
  if (GFiles.DatFile.Count > 0) then for I := 0 to GFiles.DatFile.Count - 1 do
    if (Length(GFiles.DatFile.Strings[I]) > 3) and (CompareText(LeftStr(GFiles.DatFile.Strings[I], 3), 'IPL') = 0) then
      Found := I + 1;

  ListDAT.Items.Add('');
  GFiles.DatFile.Add('');
  if (GFiles.DatFile.Count > 1) and (Found < GFiles.DatFile.Count - 1) then for I := GFiles.DatFile.Count - 2 downto Found do
  begin
    GFiles.DatFile.Strings[I + 1] := GFiles.DatFile.Strings[I];
    ListDAT.Items.Strings[I + 1] := ListDAT.Items.Strings[I];
  end;
  GFiles.DatFile.Strings[Found] := Trim('IPL ' + InName);
  ListDAT.Items.Strings[Found] := Trim('IPL ' + InName);

  GFiles.DatChanged := True;
  BtnSave.Enabled := True;
  ListDAT.ItemIndex := -1;
  BtnUpdate.Enabled := False;
  EditLine.Text := '';
end;

procedure TFormEditorDAT.FormShow(Sender: TObject);
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

procedure TFormEditorDAT.FormCreate(Sender: TObject);
begin
  FirstShow := True;
end;

end.
