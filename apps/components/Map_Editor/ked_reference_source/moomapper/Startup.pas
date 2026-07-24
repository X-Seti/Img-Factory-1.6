//DFIX: BY ADDING GAMES SUPPORT MADE A REAL MESS OF THIS CODE!!

unit Startup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, jpeg, Registry, DirDialog, Buttons,
  delfiparser;

type
  TFormStartup = class(TForm)
    ModelGroup: TRadioGroup;
    MemoryLabel: TLabel;
    MemoryValue: TLabel;
    LaunchButton: TButton;
    ExitButton: TButton;
    BackImage: TImage;
    Label1: TLabel;
    box_games: TListBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    gamename: TEdit;
    box_txd: TCheckBox;
    procedure LaunchButtonClick(Sender: TObject);
    procedure ExitButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GameGroupClick(Sender: TObject);
    procedure ModelGroupClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure gamenameChange(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure box_gamesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  private
    ControlRgn : HRGN;
    procedure CalculateMemUsage;
    procedure DrawBorder;
    procedure HideIt(FormHandle: HWND);
  public
    { Public declarations }
  end;

var
  FormStartup: TFormStartup;

implementation

uses Main, Loading, U_MooSettings, U_imgdlg, ExtraView, TextureView,
  EditorArchive, EditorIPL, EditorIDE, EditorDAT, EditorItem,
  GLViewDetached, Validate, About;

{$R *.dfm}

procedure TFormStartup.CalculateMemUsage;
var
  LMem, HMem: LongWord;
  i: integer;
begin
if box_games.itemindex < 0 then begin box_games.itemindex:= 0; exit; end;

  delfiparser.setworkspace(' ');

  delfiparser.foo.CommaText:= box_games.Items[box_games.itemindex];

  delfiparser.foo[0]:= IncludeTrailingBackslash(delfiparser.foo[0]);

  Main.gamename:= delfiparser.foo[1];

  gamename.Text:= delfiparser.foo[1];

  main.GTAPath:= delfiparser.foo[0]; 

  LMem := 0; HMem := 0;

  Main.GTA_VICE_MODE := fileexists(IncludeTrailingBackslash(main.GTAPath) + 'gta-vc.exe');

  Main.GTA_MODEL_MODE := (ModelGroup.ItemIndex = 1) or (ModelGroup.ItemIndex = 2);
  Main.GTA_TEXTURE_MODE := box_txd.checked;
  if not Main.GTA_TEXTURE_MODE then
    Main.GTA_TEXTURE_WHEN_NEEDED := True
  else
    Main.GTA_TEXTURE_WHEN_NEEDED := False;

  if Main.GTA_VICE_MODE then
  begin
    LMem := LMem + 30;
    HMem := HMem + 30;
    if Main.GTA_MODEL_MODE then
      HMem := HMem + 150;
    if Main.GTA_TEXTURE_MODE then
      HMem := HMem + 200;
  end else
  begin
    LMem := LMem + 60;
    HMem := HMem + 60;
    if Main.GTA_MODEL_MODE then
      HMem := HMem + 150;
    if Main.GTA_TEXTURE_MODE then
      HMem := HMem + 150;
  end;

  if (LMem = HMem) then
    MemoryValue.Caption := IntToStr(LMem) + ' Mb'
  else
    MemoryValue.Caption := IntToStr(LMem) + ' - ' + IntToStr(HMem) + ' Mb';
end;

procedure TFormStartup.FormCreate(Sender: TObject);
begin
  Application.HelpFile := IncludeTrailingBackslash(ExtractFilePath(Application.ExeName)) + 'moomapper.hlp';
  CalculateMemUsage;
end;

procedure TFormStartup.GameGroupClick(Sender: TObject);
begin
  CalculateMemUsage;
end;

procedure TFormStartup.ModelGroupClick(Sender: TObject);
begin
  CalculateMemUsage;
end;

procedure TFormStartup.HideIt(FormHandle: HWND);
begin
  ControlRgn := CreateRoundRectRgn(0, 0,
                                   Width, Height,
                                   20, 20);
  SetWindowRgn(FormHandle, ControlRgn, False);
end;

procedure TFormStartup.DrawBorder;
var
  DC: HDC;
  BR: HGDIOBJ;
  DrawRgn: HRGN;
begin
  DC := GetWindowDC(Handle);
  BR := GetStockObject(BLACK_BRUSH);
  DrawRgn := CreateRectRgn(0, 0, 0, 0);
  GetWindowRgn(Handle, DrawRgn);
  FrameRgn(DC, DrawRgn, BR, 2, 2);
  ReleaseDC(Handle, DC);
end;

procedure TFormStartup.FormShow(Sender: TObject);
var
reg: Tregistry;
begin

// "J:\Grand Theft Auto\Grand Theft Auto Vice City\","MYRIAD ISLANDS"

try
box_games.Items.loadfromfile(extractfiledir(application.exename) + '\GAMES.TXT');
except
// if games file not found.. add some!

Reg := TRegistry.Create;
try
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  if Reg.OpenKey('\Software\Rockstar Games\GTAIII', True) then
  begin
    GTAPath := Reg.ReadString('Path');
    if gtapath <> '' then begin
    delfiparser.setworkspace('');
    delfiparser.foo.add(GTAPath);
    delfiparser.foo.add('GTA 3');
    box_games.Items.add(delfiparser.foo.CommaText);
    box_games.repaint;
    end;
    Reg.CloseKey;
  end;
finally
  Reg.Free;
end;

Reg := TRegistry.Create;
try
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  if Reg.OpenKey('\Software\Rockstar Games\Grand Theft Auto Vice City', True) then
  begin
    GTAPath := Reg.ReadString('Path');

    if gtapath <> '' then begin
    delfiparser.setworkspace('');
    delfiparser.foo.add(GTAPath);
    delfiparser.foo.add('GTA Vice City');
    box_games.Items.add(delfiparser.foo.CommaText);
    box_games.repaint;
    end;

    Reg.CloseKey;
  end;
finally
  Reg.Free;
end;

end;

try
box_games.itemindex:= 0;
ModelGroupClick(self);
except
showmessage('NO GAMES FOUND, ADD THEM MANUALLY!');
end;

ModelGroupClick(self);
HideIt(Handle);
end;

procedure TFormStartup.FormPaint(Sender: TObject);
begin
  DrawBorder;
end;

procedure TFormStartup.BitBtn1Click(Sender: TObject);
var
  DirD: TDirDialog;
  tmp: string;
  iq: string;
begin
ANY_SELECT:= true;

DirD := TDirDialog.Create(Application);
DirD.Title := 'Please Select GTA Vice City Directory:';

if DirD.Execute then tmp:= IncludeTrailingBackslash(DirD.DirName)
else begin ANY_SELECT:= false; DirD.free; exit; end;

DirD.free;

if InputQuery('Game description', 'Enter this game / mod description', iq) = false then exit;

  delfiparser.setworkspace('');
  delfiparser.foo.Add(tmp);
  delfiparser.foo.Add(iq);
  box_games.Items.Add(delfiparser.foo.CommaText);
  box_games.Itemindex:= box_games.Items.count-1;
  ModelGroupClick(self);

end;

procedure TFormStartup.gamenameChange(Sender: TObject);
begin
  delfiparser.setworkspace('');
  delfiparser.foo.CommaText:= box_games.Items[box_games.Itemindex];

  delfiparser.foo[1]:= gamename.Text;
  
  box_games.Items[box_games.Itemindex]:= delfiparser.foo.CommaText;

  box_games.repaint;

end;

procedure TFormStartup.FormHide(Sender: TObject);
begin
box_games.Items.savetofile(extractfiledir(application.exename) + '\GAMES.TXT');
end;

procedure TFormStartup.BitBtn2Click(Sender: TObject);
begin
box_games.Items.Delete(box_games.itemindex);
end;

procedure TFormStartup.box_gamesDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin

with box_games.canvas do begin
FillRect(rect);

delfiparser.setworkspace('');
delfiparser.foo.commatext:= box_games.items[index];

TextOut(rect.left + 2, rect.top, delfiparser.foo[1]);

end;

end;

procedure TFormStartup.ExitButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TFormStartup.LaunchButtonClick(Sender: TObject);
begin
  Hide;
  Main.FormLoading := TFormLoading.Create(Application);
  try
    with Main.FormLoading do
    begin
      SetMax(6);
      Show;

      Application.CreateForm(TFormMain, FormMain);

      SetStatus('Retrieving GTA Path');

      if FormMain.LoadGTAPath then
      begin
        IncPos;

        SetStatus('Initialising OpenGL Display');
        FormMain.LoadOpenGL;
        IncPos;

        PartView(True);

        SetStatus('Loading Models && Textures');
        FormMain.LoadGTAArchive;
        IncPos;

        ResetPartPos;

        SetStatus('Loading IDE (Object Definition) Files');
        FormMain.LoadGTAFiles(True);
        IncPos;

        ResetPartPos;

        SetStatus('Loading IPL (Object Instance) Files');
        FormMain.LoadGTAFiles(False);
        IncPos;

        PartView(False);

        SetStatus('Executing Program'); IncPos;

        Main.FormExtra := TFormExtraView.Create(Application, GTA_TEXTURE_MODE, MainGLView.VRC);
        Main.FormTexture := TFormTextureView.Create(Application, MainGLView.VRC);

        Main.FormArchive := TFormEditorArchive.Create(Application);
        Main.FormIDE := TFormEditorIDE.Create(Application);
        Main.FormIPL := TFormEditorIPL.Create(Application);
        Main.FormDAT := TFormEditorDAT.Create(Application);
        Main.FormEditor := TFormEditorItem.Create(Application);
        Main.FormGLViewDetached := TFormGLViewDetached.Create(Application);
        Main.FormValidate := TFormValidate.Create(Application);
        Main.FormAbout := TFormAbout.Create(Application);

        Main.MainGLView.SetEditForm(Main.FormEditor);
        Main.MainGLView.SetPickFiles(Main.GFiles);

        FormMain.LoadDocking;
        FormMain.Show;
      end;
    end;
  finally
    FreeAndNIL(Main.FormLoading);
  end;
end;

end.
