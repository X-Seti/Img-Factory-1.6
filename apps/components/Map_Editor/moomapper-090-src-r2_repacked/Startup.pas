unit Startup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, jpeg, Registry, DirDialog;

type
  TFormStartup = class(TForm)
    GameGroup: TRadioGroup;
    ModelGroup: TRadioGroup;
    MemoryLabel: TLabel;
    MemoryValue: TLabel;
    LaunchButton: TButton;
    ExitButton: TButton;
    BackImage: TImage;
    ImageName: TImage;
    BtnViceChooseDir: TButton;
    BtnGTA3ChooseDir: TButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure LaunchButtonClick(Sender: TObject);
    procedure GameGroupClick(Sender: TObject);
    procedure ModelGroupClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ExitButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure BtnViceChooseDirClick(Sender: TObject);
    procedure BtnGTA3ChooseDirClick(Sender: TObject);
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
  Continue: Boolean;

implementation

uses Main;

{$R *.dfm}

procedure TFormStartup.CalculateMemUsage;
var
  LMem, HMem: LongWord;
begin
  LMem := 0; HMem := 0;
  Main.GTA_VICE_MODE := (GameGroup.ItemIndex = 0);
  Main.GTA_MODEL_MODE := (ModelGroup.ItemIndex = 1) or (ModelGroup.ItemIndex = 2);
  Main.GTA_TEXTURE_MODE := (ModelGroup.ItemIndex = 2);
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
  Continue := True;
  CalculateMemUsage;
end;

procedure TFormStartup.LaunchButtonClick(Sender: TObject);
begin
  Visible := False;
end;

procedure TFormStartup.GameGroupClick(Sender: TObject);
begin
  CalculateMemUsage;
end;

procedure TFormStartup.ModelGroupClick(Sender: TObject);
begin
  CalculateMemUsage;
end;

procedure TFormStartup.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := False;
  Continue := False;
  Visible := False;
end;

procedure TFormStartup.ExitButtonClick(Sender: TObject);
begin
  Continue := False;
  Visible := False;
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
begin
  HideIt(Handle);
end;

procedure TFormStartup.FormPaint(Sender: TObject);
begin
  DrawBorder;
end;

procedure TFormStartup.BtnViceChooseDirClick(Sender: TObject);
var
  Reg: TRegistry;
  DirD: TDirDialog;
begin
  GTA_VICE_MODE := True;
  DirD := TDirDialog.Create(Application);
  DirD.Title := 'Please Select GTA Vice City Directory:';

  if DirD.Execute then
  begin
    GTAPath := ExtractFilePath(IncludeTrailingBackslash(DirD.DirName));

    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      if Reg.OpenKey('\Software\Rockstar Games\Grand Theft Auto Vice City', True) then
      begin
        Reg.WriteString('Path', GTAPath);
        Reg.CloseKey;
      end;
    finally
      Reg.Free;
    end;
  end;
end;

procedure TFormStartup.BtnGTA3ChooseDirClick(Sender: TObject);
var
  Reg: TRegistry;
  DirD: TDirDialog;
begin
  GTA_VICE_MODE := False;
  DirD := TDirDialog.Create(Self);
  DirD.Title := 'Please Select GTA III Directory:';

  if DirD.Execute then
  begin
    GTAPath := ExtractFilePath(IncludeTrailingBackslash(DirD.DirName));

    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      if Reg.OpenKey('\Software\Rockstar Games\Grand Theft Auto Vice City', True) then
      begin
        Reg.WriteString('Path', GTAPath);
        Reg.CloseKey;
      end;
    finally
      Reg.Free;
    end;
  end;
end;

end.
