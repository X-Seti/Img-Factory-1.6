unit ExtraView;

interface

uses
  OpenGl, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, ActnList, StdCtrls, RequiredTypes;

const
  EXTRAMODE_MODEL = 0;
  EXTRAMODE_OBJECT = 2;
  EXTRAMODE_PATH = 3;
  EXTRAMODE_LIGHT = 4;

type
  TFormExtraView = class(TForm)
    TimerRotate: TTimer;
    InfoPanel: TPanel;
    BarDepth: TTrackBar;
    BoxColour: TColorBox;
    RotatePanel: TPanel;
    BarRotation: TTrackBar;
    BtnViewDFF: TButton;
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerRotateTimer(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BarDepthChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BoxColourChange(Sender: TObject);
    procedure BarRotationChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnViewDFFClick(Sender: TObject);
  private
    FirstShow: Boolean;

    MainAction: TAction;

    glDisplayList: GLUint;
    ListAvailable: Boolean;

    rc : HGLRC; // Rendering Context
    dc : HDC;   // Device Context
    xRot, yRot, zRot, Depth: glFloat;

    ModelName: String;
    TextureNum: LongInt;
    ViewMode: Byte;
    ObjectID: Word;
    ObjectTimed: Boolean;

    Rotating, AllowTextureMode: Boolean;
    procedure ChangeViewport;
  public
    constructor Create(AOwner: TComponent; inTextureMode: Boolean; inRC: HGLRC);

    property VDC: HDC read dc;

    procedure glDraw;
    procedure ChangeLighting(in_enable: Boolean);
    procedure ChangeTexture(InTexture: Boolean);

    procedure SetModel(InModelName: String);
    procedure SetTexture(InTextureNum: LongInt);
    procedure SetModelTexture(InModelName: String; InTextureNum: LongInt);
    procedure SetObject(inID: Word);
    procedure SetTObject(inID: Word);
    procedure SetAction(inAction: TAction);
  end;

var
  ContextCreated: Boolean = False;

implementation

uses Main, GTAText, GTADff;

{$R *.dfm}

constructor TFormExtraView.Create(AOwner: TComponent; inTextureMode: Boolean; inRC: HGLRC);
var
  pfd : TPIXELFORMATDESCRIPTOR;
  pf: Integer;
  LightSetting, LightPosition: TVector4f;
begin
  inherited Create(AOwner);

  // set parent and get device context
  dc := GetDC(Handle);

  // initialise pixel format
  pfd.nSize := SizeOf(pfd);
  pfd.nVersion := 1 ;
  pfd.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
  pfd.iPixelType := PFD_TYPE_RGBA;
  pfd.iLayerType := PFD_MAIN_PLANE;
  pfd.cColorBits := 16;
  pfd.cDepthBits := 16;

  // set pixel format
  pf := ChoosePixelFormat(dc, @pfd);
  SetPixelFormat(dc, pf, @pfd);

  // set rendering context
  rc := wglCreateContext(dc);
  if not (rc = NULL) then
    ContextCreated := True;
  wglMakeCurrent(dc, rc);
  wglShareLists(inRc, rc);

  // set color and stuff
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClearDepth(1.0);

  glShadeModel(GL_SMOOTH);
  glColorMask(True, True, True, False);

  glEnable(GL_DEPTH_TEST);
  glDepthMask(GL_TRUE);
  glDepthFunc(GL_LESS);

  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_FASTEST);

  if inTextureMode then
    glEnable(GL_TEXTURE_2D)
  else
    glDisable(GL_TEXTURE_2D);
  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

  // enable lighting
  LightSetting[0] := 1;
  LightSetting[1] := 1;
  LightSetting[2] := 1;
  LightSetting[3] := 1;

  LightPosition[0] := 1;
  LightPosition[1] := 0;
  LightPosition[2] := 100.0;
  LightPosition[3] := 1;

  glLightfv(GL_LIGHT0, GL_POSITION, @LightPosition);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @LightSetting);
  glLightfv(GL_LIGHT0, GL_AMBIENT, @LightSetting);
  glLightfv(GL_LIGHT0, GL_SPECULAR, @LightSetting);

  ChangeLighting(not InTextureMode);
  glViewport(0, 0, Width, Height);

  // initial values
  xRot := -45; yRot := 0; zRot := 0;
  Depth := -52;
  Rotating := True;
  AllowTextureMode := inTextureMode;
  ViewMode := EXTRAMODE_MODEL;

  ModelName := ''; TextureNum := -1;
  ListAvailable := False;

  ObjectID := 0;

  // create display list to use
  glDisplayList := glGenLists(1);
end;

procedure TFormExtraView.ChangeTexture(InTexture: Boolean);
begin
  wglMakeCurrent(dc, rc);
  if (InTexture) and (AllowTextureMode) then
  begin
    glEnable(GL_TEXTURE_2D);
  end else
  begin
    glDisable(GL_TEXTURE_2D);
  end;
end;

procedure TFormExtraView.ChangeLighting(in_enable: Boolean);
begin
  if ContextCreated then
  begin
    wglMakeCurrent(dc, rc);
    if (in_enable) then
    begin
      glEnable(GL_LIGHTING);
      glEnable(GL_LIGHT0);
    end else
    begin
      glDisable(GL_LIGHTING);
      glDisable(GL_LIGHT0);
    end;
  end;
end;

procedure TFormExtraView.ChangeViewport;
begin
  if ContextCreated then
  begin
    wglMakeCurrent(dc, rc);
    glViewport(0, 0, Width, Height);
  end;
end;

procedure TFormExtraView.glDraw;
begin
  if ContextCreated then
  begin
    wglMakeCurrent(dc, rc);

    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_ACCUM_BUFFER_BIT);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();

    gluPerspective(45.0, Width/Height, 1.0, 5000.0);
    glTranslatef(0.0, 0.0, Depth);
    glRotatef(xRot, 1, 0, 0);
    glRotatef(yRot, 0, 1, 0);
    glRotatef(zRot, 0, 0, 1);

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    //glTranslatef(-CenX, -CenY, -CenZ);
    glColor3f(1.0, 1.0, 1.0);

    // draw selected model
    case ViewMode of
      EXTRAMODE_OBJECT:
      begin
        Main.GFiles.glDraw(ObjectID);
      end;

      EXTRAMODE_MODEL:
      begin
        if not ListAvailable then
        begin
          glNewList(glDisplayList, GL_COMPILE);
          if not (ModelName = '') then
            Main.GArchive.glDraw(ModelName, TextureNum);
          glEndList;
          ListAvailable := True;
        end;
       glCallList(glDisplayList);
      end;
    end;
  end;
end;

procedure TFormExtraView.FormResize(Sender: TObject);
begin
  ChangeViewport;
end;

procedure TFormExtraView.FormDestroy(Sender: TObject);
begin
  glDeleteLists(glDisplayList, 1);
  wglMakeCurrent(0,0);
  wglDeleteContext(rc);
end;

procedure TFormExtraView.TimerRotateTimer(Sender: TObject);
begin
  if Rotating then
    zRot := zRot + 1;
end;

procedure TFormExtraView.FormHide(Sender: TObject);
begin
  TimerRotate.Enabled := False;
end;

procedure TFormExtraView.FormShow(Sender: TObject);
var
  Rct: TRect;
  L, T: LongInt;
begin
  TimerRotate.Enabled := True;
  if FirstShow then
  begin
    GetWindowRect(MainGLView.Handle, Rct);
    L := Rct.Left;
    T := Rct.Bottom - Width;
    if (T < 0) then
      T := 0;
    if (L < 0) then
      L := 0;
    Top := T;
    Left := L;
    FirstShow := False;
  end;
end;

procedure TFormExtraView.SetModel(InModelName: String);
begin
  ViewMode := EXTRAMODE_MODEL;
  ModelName := InModelName;
  ListAvailable := False;
end;

procedure TFormExtraView.SetModelTexture(InModelName: String; InTextureNum: LongInt);
begin
  ViewMode := EXTRAMODE_MODEL;
  ModelName := InModelName;
  TextureNum := InTextureNum;
  ListAvailable := False;
  if not (Main.FormTexture = nil) then
    Main.FormTexture.SetTexture(TextureNum);
end;

procedure TFormExtraView.SetTexture(InTextureNum: LongInt);
begin
  ViewMode := EXTRAMODE_MODEL;
  TextureNum := InTextureNum;
  ListAvailable := False;
  if not (Main.FormTexture = nil) then
    Main.FormTexture.SetTexture(TextureNum);
end;

procedure TFormExtraView.SetObject(InID: Word);
var
  J, K: LongWord;
  FoundIt: Boolean;
begin
  ViewMode := EXTRAMODE_OBJECT;
  ObjectID := inID;
  ObjectTimed := False;
  TextureNum := -1;
  FoundIt := False;

  if (Main.GFiles.Count > 0) then
  begin
    J := 0;
    while (J < Main.GFiles.Count) do
    begin
      K := 0;
      if (Main.GFiles.Item[J].SubType = FILE_IDE) and
         (TIDEFile(Main.GFiles.Item[J]).Objs.Count > 0) then
         while (K < TIDEFile(Main.GFiles.Item[J]).Objs.Count) do
         begin
           if (TIDEFile(Main.GFiles.Item[J]).Objs.Item[K].ID = InID) then
           begin
             TextureNum := GArchive.GetTXDNum(TIDEFile(GFiles.Item[J]).Objs.Item[K].TextureName);
             FoundIt := True;
           end;
           Inc(K);
         end;
         if FoundIt then
           J := Main.GFiles.Count;
         Inc(J);
    end;
  end;

  if not (Main.FormTexture = nil) then
    Main.FormTexture.SetTexture(TextureNum);
end;

procedure TFormExtraView.SetTObject(InID: Word);
var
  J, K: LongWord;
  FoundIt: Boolean;
begin
  ViewMode := EXTRAMODE_OBJECT;
  ObjectID := inID;
  ObjectTimed := True;
  TextureNum := -1;
  FoundIt := False;

  if (Main.GFiles.Count > 0) then
  begin
    J := 0;
    while (J < Main.GFiles.Count) do
    begin
      K := 0;
      if (Main.GFiles.Item[J].SubType = FILE_IDE) and
         (TIDEFile(Main.GFiles.Item[J]).TObj.Count > 0) then
         while (K < TIDEFile(Main.GFiles.Item[J]).TObj.Count) do
         begin
           if (TIDEFile(Main.GFiles.Item[J]).TObj.Item[K].ID = InID) then
           begin
             TextureNum := GArchive.GetTXDNum(TIDEFile(GFiles.Item[J]).TObj.Item[K].TextureName);
             FoundIt := True;
           end;
           Inc(K);
         end;
         if FoundIt then
           J := Main.GFiles.Count;
         Inc(J);
    end;
  end;

  if not (Main.FormTexture = nil) then
    Main.FormTexture.SetTexture(TextureNum);
end;

procedure TFormExtraView.BarDepthChange(Sender: TObject);
begin
  Depth := -BarDepth.Position;
end;

procedure TFormExtraView.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if not (MainAction = nil) then
    MainAction.Checked := False;
end;

procedure TFormExtraView.SetAction(inAction: TAction);
begin
  MainAction := inAction;
end;

procedure TFormExtraView.BoxColourChange(Sender: TObject);
begin
  if ContextCreated then
  begin
    wglMakeCurrent(dc, rc);
    glClearColor(GetRValue(BoxColour.Selected) / 255,
                 GetGValue(BoxColour.Selected) / 255,
                 GetBValue(BoxColour.Selected) / 255,
                 0.0);
  end;
end;

procedure TFormExtraView.BarRotationChange(Sender: TObject);
begin
  xRot := - BarRotation.Position - 180;
end;

procedure TFormExtraView.FormCreate(Sender: TObject);
begin
  FirstShow := True;
end;

procedure TFormExtraView.BtnViewDFFClick(Sender: TObject);
var
  Dff: TGTADff;
  f: TStream;
begin
  FormArchive.dlgAdd.Options := [ofPathMustExist, ofFileMustExist, ofHideReadOnly, ofEnableSizing];
  FormArchive.dlgAdd.Filter := 'Renderware Model File (*.dff)|*.dff';
  FormArchive.dlgAdd.FilterIndex := 0;
  if FormArchive.dlgAdd.Execute then
  begin
    ViewMode := EXTRAMODE_MODEL;
    ModelName := '';
    TextureNum := -1;
    ListAvailable := True;

    f := TFileStream.Create(FormArchive.dlgAdd.FileName, fmShareDenyNone);
    Dff := TGTADff.Create(f, ExtractFileName(FormArchive.dlgAdd.FileName), 0, f.Size);
    if not Dff.Loaded then
      Dff.LoadFromStream;
    f.Free;

    glNewList(glDisplayList, GL_COMPILE);
    Dff.glDraw(-1);
    glEndList;

    Dff.Free;

  end;
end;

end.
