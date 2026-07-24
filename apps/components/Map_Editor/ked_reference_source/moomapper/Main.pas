unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, GTADff, GTATxd, GTAText, GTAImg, GLView, Registry,
  ExtCtrls, CheckLst, Loading, ActnList, ToolWin, ComCtrls,
  EditorArchive, EditorIDE, EditorIPL, EditorDAT, ExtraView, TextureView, StdActns,
  Menus, EditorItem, GLViewDetached, ImgList, Validate, About, RequiredTypes,
  Tabs, Buttons, colclass, GTACol, fasttextcrc;

const
  LOD_NONE = 0;
  LOD_ONLY = 1;
  LOD_BOTH = 2;
  ZOOM_AMOUNT = -5;

type
  TFormMain = class(TForm)
    BoxColour: Tcolorbox;
    MainToolbar: TToolBar;
    ChkTexture: Tcheckbox;
    GameTime: Tcombobox;
    ChkAlpha: Tcheckbox;
    ChkWireframe: Tcheckbox;
    MainActionList: TActionList;
    ActionViewArchive: TAction;
    ActionViewIPL: TAction;
    ActionViewIDE: TAction;
    TBViewExtra: TToolButton;
    ActionViewExtra: TAction;
    TBViewTexture: TToolButton;
    ActionViewTexture: TAction;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    HelpMenu: TMenuItem;
    ViewMenu: TMenuItem;
    ActionFileExit: TFileExit;
    FileExitItem: TMenuItem;
    N1: TMenuItem;
    ViewExtraItem: TMenuItem;
    ViewTextureItem: TMenuItem;
    DrawPanel: TPanel;
    ActionSaveModified: TAction;
    FileSaveItem: TMenuItem;
    ActionViewEditor: TAction;
    TBSep3: TToolButton;
    TBViewEditor: TToolButton;
    N4: TMenuItem;
    ViewEditorItem: TMenuItem;
    ActionHelpAbout: TAction;
    HelpAboutItem: TMenuItem;
    ImageList: TImageList;
    ActionValidateAll: TAction;
    TBSep2: TToolButton;
    TBValidateAll: TToolButton;
    FileValidateAll: TMenuItem;
    ActionViewDAT: TAction;
    DATEditor1: TMenuItem;
    N2: TMenuItem;
    TBViewDAT: TToolButton;
    TBSep1: TToolButton;
    ZoomTimer: TTimer;
    HelpHelpItem: TMenuItem;
    Panel1: TPanel;
    BtnHideShow3DView: TPanel;
    BtnHideShowEditors: TPanel;
    BtnHideShowPanel: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet3: TTabSheet;
    LblCurrentPos: TLabel;
    EditCurrentPos: TEdit;
    BtnMoveToPos: TButton;
    Label1: TLabel;
    ResetButton: TButton;
    PanelZoomIn: TPanel;
    PanelZoomOut: TPanel;
    ChkMovement: TCheckBox;
    EditKey: TEdit;
    ChkListVisible: TCheckListBox;
    BtnVisCheckAll: TButton;
    BtnVisUnCheckAll: TButton;
    Label11: TLabel;
    DockPage: TPageControl;
    ToolButton1: TToolButton;
    toolsswitch: TTabSet;
    tools: TNotebook;
    StatusLabel: TLabel;
    StatusValue: TLabel;
    Label2: TLabel;
    RadioMovement: TRadioButton;
    RadioRotation: TRadioButton;
    MovePanel: TPanel;
    RadioX: TRadioButton;
    RadioY: TRadioButton;
    RadioZ: TRadioButton;
    Label3: TLabel;
    LabelControlLeftBtn: TLabel;
    LabelCtrlShift: TLabel;
    Label13: TLabel;
    LabelControlLeftBtnCtrl: TLabel;
    LabelControlLeftBtnCtrlShift: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label10: TLabel;
    LabelControlDoubleBtn: TLabel;
    ComboLod: TRadioGroup;
    GroupBox1: TGroupBox;
    ChkBackgroundMap: TCheckBox;
    Label14: TLabel;
    TrackBar1: TTrackBar;
    TabSheet2: TTabSheet;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    SpecRGB: TTrackBar;
    Dis: TTrackBar;
    DistCheck: TCheckBox;
    MemoDebug: TMemo;
    Label15: TLabel;
    AmbRGB: TTrackBar;
    DiffRGB: TTrackBar;
    GroupBox2: TGroupBox;
    Label16: TLabel;
    TrackBar2: TTrackBar;
    LblFPS: TLabel;
    BitBtn2: TBitBtn;
    Label17: TLabel;
    Button1: TButton;
    ActionSelectGame: TAction;
    FileSelectGame: TMenuItem;
    N3: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ActionSelectGameExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ChkWireframeClick(Sender: TObject);
    procedure BtnMoveToPosClick(Sender: TObject);
    procedure ChkTextureClick(Sender: TObject);
    procedure ChkBackgroundMapClick(Sender: TObject);
    procedure BoxColourChange(Sender: TObject);
    procedure ChkListVisibleClickCheck(Sender: TObject);
    procedure BtnVisCheckAllClick(Sender: TObject);
    procedure BtnVisUnCheckAllClick(Sender: TObject);
    procedure BtnHideShowPanelClick(Sender: TObject);
    procedure ChkMovementClick(Sender: TObject);
    procedure EditKeyKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditKeyKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ActionViewArchiveExecute(Sender: TObject);
    procedure ActionViewIDEExecute(Sender: TObject);
    procedure ActionViewIPLExecute(Sender: TObject);
    procedure ActionViewExtraExecute(Sender: TObject);
    procedure ChkListVisibleDblClick(Sender: TObject);
    procedure ActionViewTextureExecute(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure ActionSaveModifiedExecute(Sender: TObject);
    procedure ActionViewEditorExecute(Sender: TObject);
    procedure BtnHideShowEditorsClick(Sender: TObject);
    procedure BtnHideShow3DViewClick(Sender: TObject);
    procedure RadioMovementClick(Sender: TObject);
    procedure RadioRotationClick(Sender: TObject);
    procedure EditCurrentPosKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ActionValidateAllExecute(Sender: TObject);
    procedure ActionViewDATExecute(Sender: TObject);
    procedure RadioZClick(Sender: TObject);
    procedure RadioYClick(Sender: TObject);
    procedure RadioXClick(Sender: TObject);
    procedure ZoomTimerTimer(Sender: TObject);
    procedure PanelZoomInMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelZoomInMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelZoomOutMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelZoomOutMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ActionHelpAboutExecute(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure DiffRGBChange(Sender: TObject);
    procedure GameTimeChange(Sender: TObject);
    procedure ChkAlphaClick(Sender: TObject);
    procedure HelpHelpItemClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ComboLodClick(Sender: TObject);
    procedure toolsswitchClick(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure ComboLodEnter(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    // Artem: Flag which indicates if the app should exit, or FormStartup should
    // be shown on close.
    ExitApp: Boolean;
    procedure DockForm(inForm: TForm);
    procedure Idle(Sender: TObject; var Done: Boolean);
  public
    TimerIn, ResetObjectList: Boolean;
    function LoadGTAPath: Boolean;
    procedure UpdateVisible;
    procedure LoadOpenGL;
    procedure LoadGTAFiles(Definition: Boolean);
    procedure LoadGTAArchive;
    procedure LoadDocking;
  end;

var
  FormMain: TFormMain;

  FormLoading: TFormLoading;
  FormArchive: TFormEditorArchive;
  FormIDE: TFormEditorIDE;
  FormIPL: TFormEditorIPL;
  FormDAT: TFormEditorDAT;
  FormExtra: TFormExtraView;
  FormTexture: TFormTextureView;
  FormEditor: TFormEditorItem;
  FormGLViewDetached: TFormGLViewDetached;
  FormValidate: TFormValidate;
  FormAbout: TFormAbout;

  MainGLView: TGLView;

  GTA_VICE_MODE: Boolean = True;

  ANY_SELECT: boolean = false;

  GTA_DISPLAY_LISTS: Boolean = False;

  GTA_MODEL_MODE: Boolean = False;
  GTA_NORMALS_MODE: Boolean = True;

  GTA_TEXTURE_MODE: Boolean = False;
  GTA_TEXTURE_WHEN_NEEDED: Boolean = True;

  GTA_TEXTURE_LOAD_DEMAND: Boolean = True;
  GTA_MODEL_LOAD_DEMAND: Boolean = True;

  LODMode: Byte = 0;

  gamename: string;

  AppPath: String = '';
  GTAPath: String = '';

  GFiles: TGTAFileList;
  GArchive: TGTAImg;

  cols: something;

  VisibleList: array of LongWord;
  VisibleCount: LongWord;

  FPSStart: LongWord;
  FPSCount: Byte;

function colsearchanddestroy(what: string): Tcolchunk;

implementation

uses DirDialog, U_MooSettings, Startup;

{$R *.dfm}

function colsearchanddestroy(what: string): Tcolchunk;
var
i, ii: integer;
xt: integer;
begin

//showmessage(what);

result:= nil;

//xt:= makecrc(lowercase(what));

for i:= 0 to cols.ComponentCount-1 do begin
  for ii:= 0 to (cols.Components[i] as Tcolfile).ComponentCount-1 do
//  if ((cols.Components[i] as Tcolfile).Components[ii] as Tcolchunk).namecrc = xt then begin

  if ((cols.Components[i] as Tcolfile).Components[ii] as Tcolchunk).modelname = what then begin

  result:= (cols.Components[i] as Tcolfile).Components[ii] as Tcolchunk;
  exit;
  end;
 end;
end;

function TFormMain.LoadGTAPath: Boolean;
var
  Reg: TRegistry;
  DirD: TDirDialog;
begin
  // Set GTA Path & App Path
  Result := True;

  AppPath := IncludeTrailingBackslash(ExtractFilePath(Application.ExeName));

  if GTAPath <> '' then begin GTAPath := IncludeTrailingBackslash(GTAPath); exit; end;

  if (GTA_VICE_MODE) then
  begin
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      if Reg.OpenKey('\Software\Rockstar Games\Grand Theft Auto Vice City', True) then
      begin
        GTAPath := Reg.ReadString('Path');
        Reg.CloseKey;
      end;
    finally
      Reg.Free;
    end;

    GTAPath := IncludeTrailingBackslash(GTAPath);

    DirD := TDirDialog.Create(Application);
    DirD.Title := 'Please Select GTA Vice City Directory:';

    if not (FileExists(GTAPath + GDIRFile) and
            FileExists(GTAPath + GIMGFile) and
            FileExists(GTAPath + VICE_DAT_FILE)) then
    begin
      if DirD.Execute then
        GTAPath := IncludeTrailingBackslash(DirD.DirName)
      else
        Result := False;

      if Result then
      begin
        GTAPath := ExtractFilePath(GTAPath);

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

    DirD.Free;
  end else
  begin
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      if Reg.OpenKey('\Software\Rockstar Games\GTAIII', True) then
      begin
        GTAPath := Reg.ReadString('Path');
        Reg.CloseKey;
      end;
    finally
      Reg.Free;
    end;

    GTAPath := IncludeTrailingBackslash(GTAPath);

    DirD := TDirDialog.Create(Self);
    DirD.Title := 'Please Select GTA III Directory:';

    if not (FileExists(GTAPath + GDIRFile) and
            FileExists(GTAPath + GIMGFile) and
            FileExists(GTAPath + GTA3_DAT_FILE)) then
    begin
      if DirD.Execute then
        GTAPath := IncludeTrailingBackslash(DirD.DirName)
      else
        Result := False;

      if Result then
      begin
        GTAPath := ExtractFilePath(GTAPath);

        Reg := TRegistry.Create;
        try
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          if Reg.OpenKey('\Software\Rockstar Games\GTAIII', True) then
          begin
            Reg.WriteString('Path', GTAPath);
            Reg.CloseKey;
          end;
        finally
          Reg.Free;
        end;
      end;
    end;

    DirD.Free;
  end;
end;

procedure TFormMain.LoadGTAArchive;
begin
  // initialise archive
  GArchive := TGTAImg.Create;
  // COL CHANGE
  cols:= something.create(application);
end;

procedure TFormMain.LoadGTAFiles(Definition: Boolean);
begin
  // initialise text files
  if (GFiles = nil) then
    GFiles := TGTAFileList.Create;

  if Definition then
    GFiles.LoadFromDAT(True) // COL CHANGE
  else
    GFiles.LoadFromDAT(False);

  UpdateVisible;
end;

procedure TFormMain.LoadDocking;
begin
  DockForm(FormArchive);
  DockForm(FormIDE);
  DockForm(FormIPL);
end;

procedure TFormMain.DockForm(inForm: TForm);
var
  TempTab: TTabSheet;
  I: LongWord;
begin
  if inForm.Floating then
  begin
    TempTab := TTabSheet.Create(Self);
    TempTab.Parent := DockPage;
    inForm.ManualDock(DockPage, TempTab, alClient);
    inForm.Show;
  end;

  for I := 0 to DockPage.PageCount - 1 do
  begin
    if (DockPage.Pages[I].Caption = inForm.Caption) then
    begin
      DockPage.ActivePageIndex := I;
      DockPage.ActivePage.TabVisible := True;
      inForm.Show;
    end;
  end;
end;

procedure TFormMain.LoadOpenGL;
begin
  // initialise opengl panel
  MainGLView := TGLView.Create(DrawPanel, GTA_MODEL_MODE, GTA_TEXTURE_MODE, EditCurrentPos, StatusValue);
  MainGLView.Align := alClient;
  MainGLView.Color := clBlack;
  MainGLView.SendToBack;

  // idle event
  Application.OnIdle := Idle;
end;

procedure TFormMain.UpdateVisible;
var
  I: LongWord;
begin
  VisibleCount := 0;
  ChkListVisible.Clear;
  if (GFiles.Count > 0) then for I := 0 to GFiles.Count - 1 do
    if (GFiles.Item[I].SubType = FILE_IPL) then
    begin
      Inc(VisibleCount);
      SetLength(VisibleList, VisibleCount);
      VisibleList[VisibleCount - 1] := I;
      ChkListVisible.Items.Add(GFiles.Item[I].Name);
      ChkListVisible.Checked[VisibleCount - 1] := GFiles.Item[I].Visible;
    end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // set locale
  DecimalSeparator := '.';
  ResetObjectList := False;

  ExitApp := True;

  ChkTexture.OnClick := nil;
  ChkTexture.Checked := GTA_TEXTURE_MODE;
  ChkTexture.Enabled := GTA_TEXTURE_MODE;
  ChkTexture.OnClick := ChkTextureClick;

  ChkAlpha.OnClick := nil;
  ChkAlpha.Checked := GTA_TEXTURE_MODE;
  ChkAlpha.Enabled := GTA_TEXTURE_MODE;
  ChkAlpha.OnClick := ChkAlphaClick;

  caption:= caption + ' [ ' + gamename + ' ]';

  WindowState := wsMaximized;
end;

procedure TFormMain.Idle(Sender: TObject; var Done: Boolean);
var
  TickTime: LongWord;
  TickCalc: Single;
  FPS: String;
  I: LongWord;
begin
  if (FPSCount = 0) then
    FPSStart := GetTickCount;

  MainGLView.glDraw;

  if Assigned(GFiles) then
    GFiles.glDrawAll(MainGLView.GetMovement, MainGLView.GetPosition, MainGLView.Time);

  SwapBuffers(MainGLView.VDC);

  if Assigned(FormExtra) and (FormExtra.Visible) then
  begin
    FormExtra.glDraw;
    SwapBuffers(FormExtra.VDC);
  end;

  if Assigned(FormTexture) and (FormTexture.Visible) then
  begin
    FormTexture.glDraw;
    SwapBuffers(FormTexture.VDC);
  end;

  if ResetObjectList and Assigned(FormLoading) then
  begin
    FormLoading.IncPos;
    FormLoading.SetStatus('Freeing Object Lists');

    ResetObjectList := False;
    if (GFiles.Count > 0) then for I := 0 to GFiles.Count - 1 do
      if (GFiles.Item[I].SubType = FILE_IDE) then
      begin
        TIDEFile(GFiles.Item[I]).Objs.DestroyList;
        TIDEFile(GFiles.Item[I]).TObj.DestroyList;
      end;

    FreeAndNIL(FormLoading);

    ChkListVisible.Enabled := True;
    ComboLOD.Enabled := True;
  end;

  if Assigned(GArchive) and GArchive.KillNotUsed then
  begin
    GArchive.DestroyNotUsed;
    GArchive.KillNotUsed := False;
  end;

  if Assigned(GFiles) and GFiles.KillNotUsed then
  begin
    GFiles.DestroyNotUsed;
    GFiles.KillNotUsed := False;
  end;

  if GTA_MODEL_MODE then
  begin
    Inc(FPSCount);
    if (FPSCount >= 3) then
    begin
      TickTime := (GetTickCount - FPSStart);

      if (TickTime = 0) then
        FPS := ''
      else
      begin
        TickCalc := 3000 / TickTime;
        FPS := Format('%1.2f fps', [TickCalc]);
      end;

      LblFPS.Caption := FPS;

      FPSCount := 0;
      FPSStart := 0;

      Done := True;
    end else
      Done := False;
  end else
    Done := True;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  if Assigned(MainGLView) then
    MainGLView.ChangeViewport;
end;

procedure TFormMain.BtnMoveToPosClick(Sender: TObject);
var
  X, Y, Z: Single;
begin
  X := StrToFloatDef(GetVal(1, EditCurrentPos.Text), 0.0);
  Y := StrToFloatDef(GetVal(2, EditCurrentPos.Text), 0.0);
  Z := StrToFloatDef(GetVal(3, EditCurrentPos.Text), 0.0);
  MainGLView.JumpToLocation(X, Y, Z);
end;

procedure TFormMain.ChkWireframeClick(Sender: TObject);
begin
  MainGLView.SetWireframe(ChkWireframe.Checked);
end;

procedure TFormMain.ChkTextureClick(Sender: TObject);
begin
  MainGLView.SetTexture(ChkTexture.Checked);
  FormExtra.ChangeTexture(ChkTexture.Checked);
end;

procedure TFormMain.ChkBackgroundMapClick(Sender: TObject);
var
  I: LongWord;
  J: LongInt;
  OutTextures: array of LongWord;
begin

  if (ChkBackgroundMap.Checked) then
  begin
    SetLength(OutTextures, 64);
    for I := 0 to 63 do
    begin
      J := GArchive.GetTxdNum(Format('radar%.2d', [I]));
      if (J = -1) then
        OutTextures[I] := 0
      else
      begin
        if not GArchive.GTxd[J].Loaded then
          GArchive.GTxd[J].LoadFromStream;
        if (GArchive.GTxd[J].ImageCount > 0) then
          OutTextures[I] := GArchive.GTxd[J].ImageTexture[0];
      end;
    end;
    MainGLView.SetBackground(True, OutTextures);
  end else
    MainGLView.SetBackground(False, OutTextures);
end;

procedure TFormMain.ChkMovementClick(Sender: TObject);
begin
  MainGLView.SetMovement(ChkMovement.Checked);
  if EditKey.Visible then EditKey.SetFocus;

  if ChkMovement.Checked then
  begin
    LabelControlLeftBtn.Caption := 'Move Forwards';
    LabelControlLeftBtnCtrl.Caption := 'Move Backwards';
    LabelControlLeftBtnCtrlShift.Caption := 'Move Faster';
    LabelControlDoubleBtn.Caption := 'N/A';
    LabelCtrlShift.Caption := '+ Shift';
  end else
  begin
    LabelControlLeftBtn.Caption := 'Move Camera';
    LabelControlLeftBtnCtrl.Caption := 'Move Object (XY Plane)';
    LabelControlLeftBtnCtrlShift.Caption := 'Move Object (Single Axis)';
    LabelControlDoubleBtn.Caption := 'Select Item && Zoom In';
    LabelCtrlShift.Caption := '+ Ctrl + Shift';
  end;
end;

procedure TFormMain.BoxColourChange(Sender: TObject);
begin
  MainGLView.SetBackColour(BoxColour.Selected);
end;

procedure TFormMain.ChkListVisibleClickCheck(Sender: TObject);
var
  I, Max: LongWord;
begin
  if ResetObjectList then
    Exit;

  if not (ChkListVisible.ItemIndex = -1) then
  begin
    I := ChkListVisible.ItemIndex;
    GFiles.Item[VisibleList[I]].Visible := ChkListVisible.Checked[I];

    if GTA_MODEL_MODE then
    begin
      ResetObjectList := True;
      if (GFiles.Count > 0) then for I := 0 to GFiles.Count - 1 do
        if (GFiles.Item[I].SubType = FILE_IDE) then
        begin
          TIDEFile(GFiles.Item[I]).Objs.CreateList;
          TIDEFile(GFiles.Item[I]).TObj.CreateList;
        end;

      Max := 0;
      if (ChkListVisible.Count > 0) then for I := 0 to ChkListVisible.Count - 1 do
        if ChkListVisible.Checked[I] then
          Inc(Max);

      GFiles.SetNotUsed;
      GArchive.SetNotUsed;

      FormLoading := TFormLoading.Create(Application);
      with FormLoading do
      begin
        ChkListVisible.Enabled := False;
        ComboLOD.Enabled := False;
        ResetPos;

        SetMax(Max + 1); Show;

        SetStatus('Loading Needed Models && Textures');
      end;
    end;
  end;
end;

procedure TFormMain.BtnVisCheckAllClick(Sender: TObject);
var
  I, Max: LongWord;
  Continue: Boolean;
begin
  if ResetObjectList then
    Exit;

  Continue := True;
  if GTA_MODEL_MODE then
    Continue := (MessageDlg('This operation might take a while, the mapper runs far faster viewing a few files instead of everything. Continue?', mtConfirmation, [mbYes, mbNo], 0) = mrYes);
  if Continue then
  begin
    if (VisibleCount > 0) then for I := 0 to VisibleCount - 1 do
    begin
      GFiles.Item[VisibleList[I]].Visible := True;
      ChkListVisible.Checked[I] := True;
    end;
    if GTA_MODEL_MODE then
    begin
      ResetObjectList := True;
      Max := 0;
      if (GFiles.Count > 0) then for I := 0 to GFiles.Count - 1 do
        if (GFiles.Item[I].SubType = FILE_IDE) then
        begin
          TIDEFile(GFiles.Item[I]).Objs.CreateList;
          TIDEFile(GFiles.Item[I]).TObj.CreateList;
        end else if (GFiles.Item[I].SubType = FILE_IPL) then
          Inc(Max);

      FormLoading := TFormLoading.Create(Application);
      with FormLoading do
      begin
        ChkListVisible.Enabled := False;
        ComboLOD.Enabled := False;
        ResetPos;

        SetMax(Max + 1); Show;

        SetStatus('Loading Needed Models && Textures');
      end;
    end;
  end;
end;

procedure TFormMain.BtnVisUnCheckAllClick(Sender: TObject);
var
  I: LongWord;
begin
  if ResetObjectList then
    Exit;

  if (VisibleCount > 0) then for I := 0 to VisibleCount - 1 do
  begin
    GFiles.Item[VisibleList[I]].Visible := False;
    ChkListVisible.Checked[I] := False;
  end;
  if GTA_MODEL_MODE then
  begin
    GFiles.SetNotUsed;
    GArchive.SetNotUsed;
  end;
end;

procedure TFormMain.BtnHideShowPanelClick(Sender: TObject);
begin
  PageControl1.Visible := not PageControl1.Visible;
  if Assigned(MainGLView) then
    MainGLView.ChangeViewport;
end;

procedure TFormMain.EditKeyKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  MainGLView.VKeyDown(Key, Shift);
end;

procedure TFormMain.EditKeyKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  MainGLView.VKeyUp(Key, Shift);
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  MainGLView.VKeyDown(Key, Shift);
end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  MainGLView.VKeyUp(Key, Shift);
end;

procedure TFormMain.ActionViewArchiveExecute(Sender: TObject);
begin
  FormArchive.Visible := not FormArchive.Visible;
  ActionViewArchive.Checked := FormArchive.Visible;
  FormArchive.SetAction(ActionViewArchive);
end;

procedure TFormMain.ActionViewIDEExecute(Sender: TObject);
begin
  FormIDE.Visible := not FormIDE.Visible;
  ActionViewIDE.Checked := FormIDE.Visible;
  FormIDE.SetAction(ActionViewIDE);
end;

procedure TFormMain.ActionViewIPLExecute(Sender: TObject);
begin
  FormIPL.Visible := not FormIPL.Visible;
  ActionViewIPL.Checked := FormIPL.Visible;
  FormIPL.SetAction(ActionViewIPL);
end;

procedure TFormMain.ActionViewExtraExecute(Sender: TObject);
begin
  FormExtra.Visible := not FormExtra.Visible;
  ActionViewExtra.Checked := FormExtra.Visible;
  FormExtra.SetAction(ActionViewExtra);
end;

procedure TFormMain.ChkListVisibleDblClick(Sender: TObject);
var
  I, J: Integer;
begin
  if not (ChkListVisible.ItemIndex = -1) then
  begin
    I := ChkListVisible.ItemIndex;
    if (FormIPL.DisplayListFiles.Count > 0) then for J := 0 to FormIPL.DisplayListFiles.Count - 1 do
      if (LongWord(FormIPL.DisplayListFiles.Items[J]) = VisibleList[I]) then
      begin
        FormIPL.ListFiles.ItemIndex := J;
        FormIPL.UpdateListItemsSize;
      end;
    with TIPLFile(GFiles.Item[VisibleList[I]]).Inst do
      if (Count > 0) then
        MainGLView.JumpToLocation(Item[0].Pos);
  end;
end;

procedure TFormMain.ActionViewTextureExecute(Sender: TObject);
begin
  FormTexture.Visible := not FormTexture.Visible;
  ActionViewTexture.Checked := FormTexture.Visible;
  FormTexture.SetAction(ActionViewTexture);
end;

procedure TFormMain.ActionViewEditorExecute(Sender: TObject);
begin
  FormEditor.Visible := not FormEditor.Visible;
  ActionViewEditor.Checked := FormEditor.Visible;
  FormEditor.SetAction(ActionViewEditor);
end;

procedure TFormMain.ResetButtonClick(Sender: TObject);
begin
  MainGLView.ResetView;
end;

procedure TFormMain.ActionSaveModifiedExecute(Sender: TObject);
var
  I: LongWord;
  Saved, NotSaved, FullMsg: String;
begin
  Saved := '';
  NotSaved := '';
  if GFiles.Count > 0 then for I := 0 to GFiles.Count - 1 do
    if GFiles.Item[I].Changed then
      if (GFiles.Item[I].Save = 0) then
        Saved := Saved + #13#10 + GFiles.Item[I].Name
      else
        NotSaved := NotSaved + #13#10 + GFiles.Item[I].Name;
  FullMsg := '';

  if (Length(Saved) > 0) then
    FullMsg := FullMsg + 'SUCCESS: The following files saved successfully:' + #13#10 + Saved;
  if (Length(NotSaved) > 0) then
    FullMsg := FullMsg + 'ERROR: The following files failed to save:' + #13#10
                       + 'These could be read only, or in use by another program!' + #13#10 + NotSaved;

  if (Length(NotSaved) > 0) then
    MessageDlg(FullMsg, mtError, [mbOk], 0)
  else
    MessageDlg(FullMsg, mtInformation, [mbOk], 0);

  FormIDE.ListFiles.Repaint;
  FormIPL.ListFiles.Repaint;
  ActionSaveModified.Enabled := False;
end;

procedure TFormMain.BtnHideShowEditorsClick(Sender: TObject);
begin
  DockPage.Visible := not DockPage.Visible;
  if Assigned(MainGLView) then
    MainGLView.ChangeViewport;
end;

procedure TFormMain.BtnHideShow3DViewClick(Sender: TObject);
begin
  if FormGLViewDetached.Visible then
    FormGLViewDetached.Close
  else
    FormGLViewDetached.Show;
end;

procedure TFormMain.RadioMovementClick(Sender: TObject);
begin
  MainGLView.SetRotation(False);
end;

procedure TFormMain.RadioRotationClick(Sender: TObject);
begin
  MainGLView.SetRotation(True);
end;

procedure TFormMain.EditCurrentPosKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    BtnMoveToPosClick(Self);    
end;

procedure TFormMain.ActionValidateAllExecute(Sender: TObject);
begin
  FormValidate.Visible := not FormValidate.Visible;
  ActionValidateAll.Checked := FormValidate.Visible;
  FormValidate.SetAction(ActionValidateAll);
end;

procedure TFormMain.ActionViewDATExecute(Sender: TObject);
begin
  FormDAT.Visible := not FormDAT.Visible;
  ActionViewDAT.Checked := FormDAT.Visible;
  FormDAT.SetAction(ActionViewDAT);
end;

procedure TFormMain.RadioZClick(Sender: TObject);
begin
  MainGLView.SetAxisMode(SINGLEAXIS_Z);
end;

procedure TFormMain.RadioYClick(Sender: TObject);
begin
  MainGLView.SetAxisMode(SINGLEAXIS_Y);
end;

procedure TFormMain.RadioXClick(Sender: TObject);
begin
  MainGLView.SetAxisMode(SINGLEAXIS_X);
end;

procedure TFormMain.ZoomTimerTimer(Sender: TObject);
begin
  if TimerIn then
    MainGLView.ZoomBy(ZOOM_AMOUNT)
  else
    MainGLView.ZoomBy(-ZOOM_AMOUNT);
end;

procedure TFormMain.PanelZoomInMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerIn := True;
  ZoomTimer.Enabled := True;
  ZoomTimerTimer(Self);
end;

procedure TFormMain.PanelZoomInMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ZoomTimer.Enabled := False;
end;

procedure TFormMain.PanelZoomOutMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerIn := False;
  ZoomTimer.Enabled := True;
  ZoomTimerTimer(Self);
end;

procedure TFormMain.PanelZoomOutMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ZoomTimer.Enabled := False;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  SaveFiles: Boolean;
  SaveDAT: Boolean;
  Problem: Boolean;
  I: LongWord;
begin
  SaveFiles := False;
  SaveDAT := GFiles.DatChanged;
  Problem := False;
  if GFiles.Count > 0 then for I := 0 to GFiles.Count - 1 do
    if GFiles.Item[I].Changed then
      SaveFiles := True;
  if SaveFiles or SaveDAT then
  begin
    case (MessageDlg('Do you want to save changes?', mtConfirmation, [mbYes, mbNo, mbCancel], 0)) of
      mrYes:
      begin
        if SaveFiles then
        begin
          ActionSaveModified.Execute;
          if ActionSaveModified.Enabled then
            Problem := True;
        end;
        if SaveDAT and not (GFiles.SaveDat = 0) then
          Problem := True;
      end;
      mrCancel: Problem := True;
    end;
  end;
  if Problem then
    CanClose := False;
end;

procedure TFormMain.ActionHelpAboutExecute(Sender: TObject);
begin
  FormAbout.Visible := not FormAbout.Visible;
  ActionHelpAbout.Checked := FormAbout.Visible;
  FormAbout.SetAction(ActionHelpAbout);
end;

procedure TFormMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if (ControlAtPos(MousePos, False, True) = DrawPanel) then
    MainGLView.VMouseWheel(Sender, Shift, WheelDelta, MousePos, Handled);
end;

procedure TFormMain.DiffRGBChange(Sender: TObject);
var
  Diff, Amb, Spec, Pos: TVector4f;
begin
  Diff[0] := DiffRGB.Position / 255;
  Diff[1] := DiffRGB.Position / 255;
  Diff[2] := DiffRGB.Position / 255;
  Diff[3] := 1;

  Amb[0] := AmbRGB.Position / 255;
  Amb[1] := AmbRGB.Position / 255;
  Amb[2] := AmbRGB.Position / 255;
  Amb[3] := 1;

  Spec[0] := SpecRGB.Position / 255;
  Spec[1] := SpecRGB.Position / 255;
  Spec[2] := SpecRGB.Position / 255;
  Spec[3] := 1;

  Pos[0] := 0;
  Pos[1] := 0;
  Pos[2] := Dis.Position;
  if DistCheck.Checked then
    Pos[3] := 1
  else
    Pos[3] := 0;
                    
  MainGLView.ChangeLightingValues(Diff, Amb, Spec);
end;

procedure TFormMain.GameTimeChange(Sender: TObject);
begin
  MainGLView.SetTime(GameTime.ItemIndex);
end;

procedure TFormMain.ChkAlphaClick(Sender: TObject);
begin
  MainGLView.SetAlphaBlend(ChkAlpha.Checked);
  FormExtra.blend:= ChkAlpha.Checked;
end;

procedure TFormMain.HelpHelpItemClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_FINDER, 0);
end;

procedure TFormMain.TrackBar1Change(Sender: TObject);
begin
glview.RADAR_Z:= TrackBar1.position div 3;
application.processmessages;
end;

procedure TFormMain.ToolButton1Click(Sender: TObject);
begin
wnd_setup.show;
end;

procedure TFormMain.ComboLodClick(Sender: TObject);
var
  I, Max: LongWord;
begin
  if ResetObjectList then
    Exit;

  LODMode := ComboLOD.ItemIndex;

  if GTA_MODEL_MODE then
  begin
    ResetObjectList := True;
    if (GFiles.Count > 0) then for I := 0 to GFiles.Count - 1 do
      if (GFiles.Item[I].SubType = FILE_IDE) then
      begin
        TIDEFile(GFiles.Item[I]).Objs.CreateList;
        TIDEFile(GFiles.Item[I]).TObj.CreateList;
      end;

    Max := 0;
    if (ChkListVisible.Count > 0) then for I := 0 to ChkListVisible.Count - 1 do
      if ChkListVisible.Checked[I] then
        Inc(Max);

    GFiles.SetNotUsed;
    GArchive.SetNotUsed;

    FormLoading := TFormLoading.Create(Application);
    with FormLoading do
    begin
      ChkListVisible.Enabled := False;
      ComboLOD.Enabled := False;
      ResetPos;

      SetMax(Max + 1); Show;

      SetStatus('Loading Needed Models && Textures');
    end;
  end;
end;

procedure TFormMain.toolsswitchClick(Sender: TObject);
begin
tools.ActivePage:= tools.Pages[toolsswitch.tabindex];
end;

procedure TFormMain.TrackBar2Change(Sender: TObject);
begin
glview.proj:= TrackBar2.position;
//
end;

procedure TFormMain.BitBtn2Click(Sender: TObject);
begin
TrackBar2.position:= 45;
end;

procedure TFormMain.ComboLodEnter(Sender: TObject);
begin
DrawPanel.setfocus;
end;

procedure TFormMain.Button1Click(Sender: TObject);
var
i, ii: integer;
begin

MemoDebug.lines.beginupdate;

MemoDebug.lines.add(inttostr(cols.ComponentCount) + ' cols');

for i:= 0 to cols.ComponentCount-1 do begin
MemoDebug.lines.add('+ ' + extractfilename((cols.Components[i] as Tcolfile).filename));

for ii:= 0 to (cols.Components[i] as Tcolfile).ComponentCount-1 do
MemoDebug.lines.add(' *' + ((cols.Components[i] as Tcolfile).Components[ii] as Tcolchunk).modelname);

end;

MemoDebug.lines.endupdate;

end;

procedure TFormMain.ActionSelectGameExecute(Sender: TObject);
begin
  ExitApp := False;
  Close;
  FormStartup.Show;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Application.OnIdle := NIL;
  if Assigned(FormAbout) then FreeAndNIL(FormAbout);
  if Assigned(FormValidate) then FreeAndNIL(FormValidate);
  if Assigned(FormGLViewDetached) then FreeAndNIL(FormGLViewDetached);
  if Assigned(FormEditor) then FreeAndNIL(FormEditor);
  if Assigned(FormDAT) then FreeAndNIL(FormDAT);
  if Assigned(FormIPL) then FreeAndNIL(FormIPL);
  if Assigned(FormIDE) then FreeAndNIL(FormIDE);
  if Assigned(FormArchive) then FreeAndNIL(FormArchive);
  if Assigned(FormTexture) then FreeAndNIL(FormTexture);
  if Assigned(FormExtra) then FreeAndNIL(FormExtra);
  if Assigned(GFiles) then FreeAndNil(GFiles);
  if Assigned(GArchive) then FreeAndNil(GArchive);
  if Assigned(MainGLView) then FreeAndNil(MainGLView);
  Action := caFree;
  if ExitApp then
    ActionFileExit.Execute;
end;

end.
