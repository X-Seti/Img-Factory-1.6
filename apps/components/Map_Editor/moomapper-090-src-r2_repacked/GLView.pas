unit GLView;

interface

uses OpenGl, Windows, Classes, Controls, ExtCtrls, Graphics, SysUtils, StdCtrls, RequiredTypes, EditorItem, GTAText, Dialogs; //geometry

const
  PICKSIZE = 512;

  KEY_UP    = 1;
  KEY_DOWN  = 2;
  KEY_LEFT  = 3;
  KEY_RIGHT = 4;
  KEY_STRAFE_LEFT  = 5;
  KEY_STRAFE_RIGHT  = 6;
  KEY_STRAFE_UP  = 7;
  KEY_STRAFE_DOWN  = 8;

  SINGLEAXIS_X = 0;
  SINGLEAXIS_Y = 1;
  SINGLEAXIS_Z = 2;

  RADAR_SIZE = 512;
  RADAR_Z = -100;

  MAXVALUE_X = 2048;
  MAXVALUE_Y = 2048;
  MAXVALUE_Z = 500;

  SLOW_MOVE_AMOUNT = 1.0;
  MOVE_AMOUNT = 4.0;
  FAST_MOVE_AMOUNT = 10.0;
  MOVE_INTERVAL = 10;
  ROT_AMOUNT = 3.0;

  OBJECT_SIZE = 4.0;
  OBJECT_SIZE_SMALL = 0.5;

  MOUSEMODE_NORMAL = 0;
  MOUSEMODE_INST = 1;
  MOUSEMODE_MULT_INST = 2;

  EPSILON = 10e-6;

  SCREEN_NONE = 0;
  SCREEN_XYPLANE = 1;
  SCREEN_XZPLANE = 2;
  SCREEN_YZPLANE = 3;
  SCREEN_XAXIS = 4;
  SCREEN_YAXIS = 5;
  SCREEN_ZAXIS = 6;
  SCREEN_SCREEN = 7;

  GL_VERTEX_ARRAY                   = $8074;
  GL_NORMAL_ARRAY                   = $8075;
  GL_COLOR_ARRAY                    = $8076;
  GL_INDEX_ARRAY                    = $8077;
  GL_TEXTURE_COORD_ARRAY            = $8078;

  GL_V3F = $2A21;
  GL_N3F_V3F = $2A25;
  GL_T2F_V3F = $2A27;
  GL_T2F_N3F_V3F = $2A2B;

  GL_TEXTURE0_ARB = $84C0;
  GL_TEXTURE1_ARB = $84C1;

  GL_PREVIOUS_EXT                  = $8578;
  GL_OPERAND0_RGB_EXT              = $8590;
  GL_OPERAND1_RGB_EXT              = $8591;
  GL_OPERAND0_ALPHA_EXT            = $8598;
  GL_OPERAND1_ALPHA_EXT            = $8599;
  GL_SOURCE0_RGB_EXT               = $8580;
  GL_SOURCE1_RGB_EXT               = $8581;
  GL_SOURCE0_ALPHA_EXT             = $8588;
  GL_SOURCE1_ALPHA_EXT             = $8589;
  GL_COMBINE_EXT                   = $8570;
  GL_COMBINE_RGB_EXT               = $8571;
  GL_COMBINE_ALPHA_EXT             = $8572;
type
  TColorTableEXT = procedure(target: GLenum; internalFormat: GLenum; width: GLsizei; format: GLenum; ttype: GLenum; const data); stdcall;
  TCompressedTexImage2DARB = procedure(target: GLenum; level, components: GLint; width, height: GLsizei; border: GLint; datasize: GLsizei; pixels: Pointer); stdcall;
  TActiveTextureARB = procedure(target: GLenum); stdcall;
  TClientActiveTextureARB = procedure(target: GLenum); stdcall;

  TGLView = class(TPanel)
  private
    rc : HGLRC; // Rendering Context
    dc : HDC;   // Device Context

    xRot, yRot, zRot, Depth: glFloat;
    FxRot, FyRot, FzRot: glFloat;
    CenX, CenY, CenZ: glFloat;
    DoModels, DoBackground, DoTextures, DoLighting, AllowMovement, AllowTextureMode, Backwards, Rotating: Boolean;
    LightPosition: TVector4f;
    Moving: Word; MoveTimer: TTimer;
    StartV: TVector3d;
    Speed, MoveMode, AxisMode: Byte;

    ObjectList: GLUint; ObjectDrawn: Boolean;

    BackTexture: array of GLuint;

    MouseXVal, MouseYVal: Integer;
    MouseButton: Integer;
    PositionBox: TEdit;
    StatusValue: TLabel;

    PickFiles: TGTAFileList;

    ItemForm: TFormEditorItem;
  public
    Picking: Boolean;
    Time: SmallInt;

    property VDC: HDC read dc;
    property VRC: HGLRC read rc;

    constructor Create(AOwner: TComponent; InModelMode, InTextureMode: Boolean; InPosBox: TEdit; InStatusValue: TLabel);
    destructor Destroy; override;

    procedure ChangeLightingValues(InDiffuse, InAmbient, InSpecular: TVector4f);
    procedure ChangeParent(AOwner: TComponent);
    procedure StartupPixelFormat;
    procedure StartupVariables;

    procedure ChangeLighting(in_enable: Boolean);
    procedure ChangeViewport;
    procedure UpdatePositionBox;
    procedure UpdateStatusValue;

    function GetPosition: TVector3f;
    property GetMovement: Boolean read AllowMovement;

    procedure ResetView;
    procedure SetBackground(InBackground: Boolean; InTextures: array of LongWord);
    procedure SetMovement(InMove: Boolean);
    procedure SetWireframe(InWireframe: Boolean);
    procedure SetTexture(InTexture: Boolean);
    procedure SetAlphaBlend(InAlpha: Boolean);
    procedure SetBackColour(InColour: TColor);
    procedure SetMouseMode(InMode: Byte);
    procedure JumpToLocation(InX, InY, InZ: Single); overload;
    procedure JumpToLocation(InPos: TVector3f); overload;
    procedure ZoomBy(InZoom: Single);
    procedure SetTime(InTime: Byte);

    procedure SetEditForm(InForm: TFormEditorItem);
    procedure SetPickFiles(InFiles: TGTAFileList);

    procedure SetAxisMode(InMode: Byte);
    procedure SetRotation(in_enable: Boolean);
    procedure ScreenToWorldCoords(X, Y: Double; ObjV: TVector3d; inMode: Byte; var OutV: TVector3d);
    procedure ScreenToWorldCoordsDrag(X, Y: Double; var OutV: TVector3d);
    procedure SelectObjectAt(X, Y: LongWord; Multi: Boolean);
    procedure ProcessHits(Hits: GLint; SelectBuffer: array of GLuint; Multi: Boolean);

    procedure glDraw;
    procedure glDrawBackground;
    procedure glDrawObject(AllowList: Boolean);
    procedure glDrawLines;

    procedure VMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure VMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure VMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure VMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure VKeyDown(Key: Word; Shift: TShiftState);
    procedure VKeyUp(Key: Word; Shift: TShiftState);
    procedure VMoveTimer(Sender: TObject);

  end;

function QuatMultiply(Q1, Q2: TVector4f): TVector4f;
function CreateXRotQuad(inRot: Single): TVector4f;
function CreateYRotQuad(inRot: Single): TVector4f;
function CreateZRotQuad(inRot: Single): TVector4f;
procedure QuatNormalise(var Dest: TVector4f);
procedure QuatIdentity(var Dest: TVector4f);

procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external opengl32;
procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;
procedure glDeleteTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;

procedure glDrawElements(mode: GLenum; count: GLsizei; atype: GLenum; const indices: Pointer); stdcall; external opengl32;
procedure glEnableClientState(aarray: GLenum); stdcall; external opengl32;
procedure glDisableClientState(aarray: GLenum); stdcall; external opengl32;
procedure glVertexPointer(size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); stdcall; external opengl32;
procedure glNormalPointer(atype: GLenum; stride: GLsizei; const pointer: Pointer); stdcall; external opengl32;
procedure glTexCoordPointer(size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); stdcall; external opengl32;
procedure glDrawArrays(mode: GLenum; first: GLint; count: GLsizei); stdcall; external opengl32;

var
  glColorTableEXT: TColorTableEXT;
  glCompressedTexImage2DARB: TCompressedTexImage2DARB;
  glActiveTextureARB: TActiveTextureARB;
  glClientActiveTextureARB: TClientActiveTextureARB;

  ContextCreated: Boolean = False;
  MouseMode: Byte;

implementation

procedure TGLView.ChangeParent(AOwner: TComponent);
begin
  ReleaseDC(Handle, dc);
  Parent := TWinControl(AOwner);
  dc := GetDC(Handle);

  StartupPixelFormat;

  wglMakeCurrent(dc, rc);
  glViewport(0, 0, Width, Height);
end;

constructor TGLView.Create(AOwner: TComponent; InModelMode, InTextureMode: Boolean; InPosBox: TEdit; InStatusValue: TLabel);
var
  Buffer: String;
begin
  inherited Create(AOwner);

  // set parent and get device context
  Parent := TWinControl(AOwner);
  dc := GetDC(Handle);

  StartupPixelFormat;

  // set rendering context
  rc := wglCreateContext(dc);
  if not (rc = 0) then
    ContextCreated := True;
  wglMakeCurrent(dc, rc);

  // check extensions
  Buffer := glGetString(GL_EXTENSIONS);

  // load colour extension
  glColorTableEXT := wglGetProcAddress('glColorTableEXT');
  glCompressedTexImage2DARB := wglGetProcAddress('glCompressedTexImage2DARB');

  if (Pos('GL_ARB_multitexture', Buffer) > 0) then
  begin
    glActiveTextureARB := wglGetProcAddress('glActiveTextureARB');
    glClientActiveTextureARB := wglGetProcAddress('glClientActiveTextureARB');
  end else
  begin
    glActiveTextureARB := nil;
    glClientActiveTextureARB := nil;
  end;

  DoModels := inModelMode;
  DoTextures := inTextureMode;
  DoLighting := not InTextureMode and InModelMode;
  DoBackground := False;
  SetLength(BackTexture, 64);

  StartupVariables;

  // initial values
  AllowMovement := False; Speed := 1;
  Moving := 0; Backwards := False;
  xRot := 0; yRot := 0; zRot := 0;
  FxRot := -90; FyRot := 0; FzRot := 0;
  Depth := -600; AxisMode := SINGLEAXIS_Z;
  CenX := 0; CenY := 0; CenZ := 0;
  AllowTextureMode := inTextureMode;
  MouseMode := MOUSEMODE_NORMAL;
  Rotating := False;
  Picking := False; Time := 9;

  // mouse events
  OnMouseDown := VMouseDown;
  OnMouseUp := VMouseUp;
  OnMouseMove := VMouseMove;

  // key movement timer
  MoveTimer := TTimer.Create(Self);
  MoveTimer.Enabled := False;
  MoveTimer.Interval := MOVE_INTERVAL;
  MoveTimer.OnTimer := VMoveTimer;

  PositionBox := InPosBox;
  StatusValue := InStatusValue;
end;

procedure TGLView.StartupPixelFormat;
var
  pfd : TPIXELFORMATDESCRIPTOR;
  pf: Integer;
begin
  // initialise pixel format
  FillChar(pfd, SizeOf(pfd), 0);
  pfd.nSize := SizeOf(pfd);
  pfd.nVersion := 1 ;
  pfd.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
  pfd.iPixelType := PFD_TYPE_RGBA;
  pfd.iLayerType := PFD_MAIN_PLANE;
  pfd.cColorBits := 16;
  pfd.cDepthBits := 16;

  // set pixel format
  pf := ChoosePixelFormat(dc, @pfd);
  if (pf = 0) then
    ShowMessage('Error Choosing Pixel Format: ' + IntToStr(GetLastError()));
  if not SetPixelFormat(dc, pf, @pfd) then
    ShowMessage('Error Setting Pixel Format: ' + IntToStr(GetLastError()));
end;

procedure TGLView.SetPickFiles(InFiles: TGTAFileList);
begin
  PickFiles := InFiles;
end;

procedure TGLView.SetEditForm(InForm: TFormEditorItem);
begin
  ItemForm := InForm;
end;

procedure TGLView.SetTime(InTime: Byte);
begin
  Time := InTime;
end;

procedure TGLView.SetBackground(InBackground: Boolean; InTextures: array of LongWord);
var
  I: LongWord;
begin
  if InBackground and (High(InTextures) = 63) then
  begin
    for I := 0 to 63 do
      BackTexture[I] := InTextures[I];
    DoBackground := True;
  end else
    DoBackground := False;
end;

procedure TGLView.StartupVariables;
var
  LightSetting: TVector4f;
begin
  // set viewport & color and stuff
  glViewport(0, 0, Width, Height);
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClearDepth(1.0);

  glShadeModel(GL_FLAT);

  // blending
  if DoTextures then
  begin
    glEnable(GL_ALPHA_TEST);
    glAlphaFunc (GL_GREATER, 0.0);
  end else
    glDisable(GL_ALPHA_TEST);

  glDepthMask(GL_TRUE);
  glDepthFunc(GL_LEQUAL);
  glEnable(GL_DEPTH_TEST);

  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

  if DoTextures then
    glEnable(GL_TEXTURE_2D)
  else
    glDisable(GL_TEXTURE_2D);
  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

  LightSetting[0] := 1;
  LightSetting[1] := 1;
  LightSetting[2] := 1;
  LightSetting[3] := 1;

  LightPosition[0] := 0;
  LightPosition[1] := 0;
  LightPosition[2] := 700.0;
  LightPosition[3] := 1;

  ChangeLightingValues(LightSetting, LightSetting, LightSetting);

  // enable lighting
  glEnable(GL_LIGHT0);

  if DoLighting then
    glEnable(GL_LIGHTING)
  else
    glDisable(GL_LIGHTING);

  ObjectList := glGenLists(1);
  ObjectDrawn := False;
end;

procedure TGLView.ChangeLightingValues(InDiffuse, InAmbient, InSpecular: TVector4f);
begin
  wglMakeCurrent(dc, rc);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @InDiffuse);
  glLightfv(GL_LIGHT0, GL_AMBIENT, @InAmbient);
  glLightfv(GL_LIGHT0, GL_SPECULAR, @InSpecular);
end;

destructor TGLView.Destroy;
begin
  inherited Destroy;
  if not DoModels then
    glDeleteLists(ObjectList, 1);
  wglMakeCurrent(0, 0);
  wglDeleteContext(rc);
end;

function TGLView.GetPosition: TVector3f;
begin
  Result[0] := CenX;
  Result[1] := CenY;
  Result[2] := CenZ;
end;

procedure TGLView.SetRotation(in_enable: Boolean);
begin
  Rotating := in_enable;
end;

procedure TGLView.ChangeLighting(in_enable: Boolean);
begin
  if ContextCreated then
  begin
    wglMakeCurrent(dc, rc);
    if (in_enable) then
      glEnable(GL_LIGHTING)
    else
      glDisable(GL_LIGHTING);
  end;
end;

procedure TGLView.ChangeViewport;
begin
  if ContextCreated then
  begin
    wglMakeCurrent(dc, rc);
    glViewport(0, 0, Width, Height);
  end;
end;

procedure TGLView.glDrawBackground;
var
  I, J, Num: LongInt;
begin
  glEnable(GL_TEXTURE_2D);

	glColor4f(1.0, 1.0, 1.0, 1.0);
  for I := 0 to 7 do
    for J := 0 to 7 do
    begin
      Num := (7 - J) * 8 + I;

      glBindTexture(GL_TEXTURE_2D, BackTexture[Num]);

      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);

      glBegin(GL_QUADS);

  		glVertex3f( (I - 4) * RADAR_SIZE, (J - 4) * RADAR_SIZE, RADAR_Z);
      glTexCoord2f(1, 1);
  		glVertex3f( (I - 3) * RADAR_SIZE, (J - 4) * RADAR_SIZE, RADAR_Z);
      glTexCoord2f(1, 0);
  		glVertex3f( (I - 3) * RADAR_SIZE, (J - 3) * RADAR_SIZE, RADAR_Z);
      glTexCoord2f(0, 0);
  		glVertex3f( (I - 4) * RADAR_SIZE, (J - 3) * RADAR_SIZE, RADAR_Z);
      glTexCoord2f(0, 1);

      glEnd;
    end;

  if not DoTextures then
    glDisable(GL_TEXTURE_2D);
end;

procedure TGLView.glDrawObject(AllowList: Boolean);
begin
  if (not ObjectDrawn and AllowList) or not AllowList then
  begin
    if AllowList then
      glNewList(ObjectList, GL_COMPILE);

    glColor4f(1.0, 1.0, 1.0, 1.0);

    if DoModels then
    begin
      glBegin(GL_QUADS);

  		glColor4f(0.0, 1.0, 0.0, 1.0);
  		glVertex3f( OBJECT_SIZE_SMALL, OBJECT_SIZE_SMALL,-OBJECT_SIZE_SMALL);
  		glVertex3f(-OBJECT_SIZE_SMALL, OBJECT_SIZE_SMALL,-OBJECT_SIZE_SMALL);
  		glVertex3f(-OBJECT_SIZE_SMALL, OBJECT_SIZE_SMALL, OBJECT_SIZE_SMALL);
  		glVertex3f( OBJECT_SIZE_SMALL, OBJECT_SIZE_SMALL, OBJECT_SIZE_SMALL);

		  glColor4f(1.0, 0.5, 0.0, 1.0);
  		glVertex3f( OBJECT_SIZE_SMALL,-OBJECT_SIZE_SMALL, OBJECT_SIZE_SMALL);
		  glVertex3f(-OBJECT_SIZE_SMALL,-OBJECT_SIZE_SMALL, OBJECT_SIZE_SMALL);
  		glVertex3f(-OBJECT_SIZE_SMALL,-OBJECT_SIZE_SMALL,-OBJECT_SIZE_SMALL);
  		glVertex3f( OBJECT_SIZE_SMALL,-OBJECT_SIZE_SMALL,-OBJECT_SIZE_SMALL);

  		glColor4f(1.0, 0.0, 0.0, 1.0);
  		glVertex3f( OBJECT_SIZE_SMALL, OBJECT_SIZE_SMALL, OBJECT_SIZE_SMALL);
  		glVertex3f(-OBJECT_SIZE_SMALL, OBJECT_SIZE_SMALL, OBJECT_SIZE_SMALL);
  		glVertex3f(-OBJECT_SIZE_SMALL,-OBJECT_SIZE_SMALL, OBJECT_SIZE_SMALL);
  		glVertex3f( OBJECT_SIZE_SMALL,-OBJECT_SIZE_SMALL, OBJECT_SIZE_SMALL);

		  glColor4f(1.0, 1.0, 0.0, 1.0);
  		glVertex3f( OBJECT_SIZE_SMALL,-OBJECT_SIZE_SMALL,-OBJECT_SIZE_SMALL);
  		glVertex3f(-OBJECT_SIZE_SMALL,-OBJECT_SIZE_SMALL,-OBJECT_SIZE_SMALL);
  		glVertex3f(-OBJECT_SIZE_SMALL, OBJECT_SIZE_SMALL,-OBJECT_SIZE_SMALL);
  		glVertex3f( OBJECT_SIZE_SMALL, OBJECT_SIZE_SMALL,-OBJECT_SIZE_SMALL);

  		glColor4f(0.0, 0.0, 1.0, 1.0);
  		glVertex3f(-OBJECT_SIZE_SMALL, OBJECT_SIZE_SMALL, OBJECT_SIZE_SMALL);
  		glVertex3f(-OBJECT_SIZE_SMALL, OBJECT_SIZE_SMALL,-OBJECT_SIZE_SMALL);
  		glVertex3f(-OBJECT_SIZE_SMALL,-OBJECT_SIZE_SMALL,-OBJECT_SIZE_SMALL);
  		glVertex3f(-OBJECT_SIZE_SMALL,-OBJECT_SIZE_SMALL, OBJECT_SIZE_SMALL);

		  glColor4f(1.0, 0.0, 1.0, 1.0);
  		glVertex3f( OBJECT_SIZE_SMALL, OBJECT_SIZE_SMALL,-OBJECT_SIZE_SMALL);
  		glVertex3f( OBJECT_SIZE_SMALL, OBJECT_SIZE_SMALL, OBJECT_SIZE_SMALL);
  		glVertex3f( OBJECT_SIZE_SMALL,-OBJECT_SIZE_SMALL, OBJECT_SIZE_SMALL);
  		glVertex3f( OBJECT_SIZE_SMALL,-OBJECT_SIZE_SMALL,-OBJECT_SIZE_SMALL);

      glEnd;
    end else
    begin
      glBegin(GL_QUADS);

  		glColor4f(0.0, 1.0, 0.0, 1.0);
  		glVertex3f( OBJECT_SIZE, OBJECT_SIZE,-OBJECT_SIZE);
  		glVertex3f(-OBJECT_SIZE, OBJECT_SIZE,-OBJECT_SIZE);
  		glVertex3f(-OBJECT_SIZE, OBJECT_SIZE, OBJECT_SIZE);
  		glVertex3f( OBJECT_SIZE, OBJECT_SIZE, OBJECT_SIZE);

		  glColor4f(1.0, 0.5, 0.0, 1.0);
  		glVertex3f( OBJECT_SIZE,-OBJECT_SIZE, OBJECT_SIZE);
		  glVertex3f(-OBJECT_SIZE,-OBJECT_SIZE, OBJECT_SIZE);
  		glVertex3f(-OBJECT_SIZE,-OBJECT_SIZE,-OBJECT_SIZE);
  		glVertex3f( OBJECT_SIZE,-OBJECT_SIZE,-OBJECT_SIZE);

  		glColor4f(1.0, 0.0, 0.0, 1.0);
  		glVertex3f( OBJECT_SIZE, OBJECT_SIZE, OBJECT_SIZE);
  		glVertex3f(-OBJECT_SIZE, OBJECT_SIZE, OBJECT_SIZE);
  		glVertex3f(-OBJECT_SIZE,-OBJECT_SIZE, OBJECT_SIZE);
  		glVertex3f( OBJECT_SIZE,-OBJECT_SIZE, OBJECT_SIZE);

		  glColor4f(1.0, 1.0, 0.0, 1.0);
  		glVertex3f( OBJECT_SIZE,-OBJECT_SIZE,-OBJECT_SIZE);
  		glVertex3f(-OBJECT_SIZE,-OBJECT_SIZE,-OBJECT_SIZE);
  		glVertex3f(-OBJECT_SIZE, OBJECT_SIZE,-OBJECT_SIZE);
  		glVertex3f( OBJECT_SIZE, OBJECT_SIZE,-OBJECT_SIZE);

  		glColor4f(0.0, 0.0, 1.0, 1.0);
  		glVertex3f(-OBJECT_SIZE, OBJECT_SIZE, OBJECT_SIZE);
  		glVertex3f(-OBJECT_SIZE, OBJECT_SIZE,-OBJECT_SIZE);
  		glVertex3f(-OBJECT_SIZE,-OBJECT_SIZE,-OBJECT_SIZE);
  		glVertex3f(-OBJECT_SIZE,-OBJECT_SIZE, OBJECT_SIZE);

		  glColor4f(1.0, 0.0, 1.0, 1.0);
  		glVertex3f( OBJECT_SIZE, OBJECT_SIZE,-OBJECT_SIZE);
  		glVertex3f( OBJECT_SIZE, OBJECT_SIZE, OBJECT_SIZE);
  		glVertex3f( OBJECT_SIZE,-OBJECT_SIZE, OBJECT_SIZE);
  		glVertex3f( OBJECT_SIZE,-OBJECT_SIZE,-OBJECT_SIZE);

      glEnd;
    end;

    if AllowList then
    begin
      glEndList;
      glCallList(ObjectList);
      ObjectDrawn := True;
    end;
  end else
    glCallList(ObjectList);
end;

procedure TGLView.glDrawLines;
var
  Pos: TVector3f;
begin
  case MouseMode of
    MOUSEMODE_INST:
    begin
      Pos := ItemForm.GetPos;
      glBegin(GL_LINES);
        glColor4f(1.0, 0.0, 0.0, 1.0);
        glVertex3f(Pos[0], Pos[1], -MAXVALUE_Z);
        glVertex3f(Pos[0], Pos[1], MAXVALUE_Z);

        glColor4f(0.0, 1.0, 0.0, 1.0);
        glVertex3f(Pos[0], -MAXVALUE_Y, Pos[2]);
        glVertex3f(Pos[0], MAXVALUE_Y, Pos[2]);

        glColor4f(0.0, 0.0, 1.0, 1.0);
        glVertex3f(-MAXVALUE_X, Pos[1], Pos[2]);
        glVertex3f(MAXVALUE_X, Pos[1], Pos[2]);
      glEnd;
      glColor4f(1.0, 1.0, 1.0, 1.0);
    end;
  end;
end;

procedure TGLView.glDraw;
begin
  if ContextCreated then
  begin
    wglMakeCurrent(dc, rc);

    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();

    gluPerspective(45.0, Width/Height, 1.0, 8000.0);
    if AllowMovement then
    begin
      glRotatef(FxRot, 1, 0, 0);
      glRotatef(FyRot, 0, 1, 0);
      glRotatef(FzRot, 0, 0, 1);
    end else
    begin
      glTranslatef(0.0, 0.0, Depth);
      glRotatef(xRot, 1, 0, 0);
      glRotatef(yRot, 0, 1, 0);
      glRotatef(zRot, 0, 0, 1);
    end;

    glPushMatrix;

    LightPosition[0] := CenX;
    LightPosition[1] := CenY;
    glLightfv(GL_LIGHT0, GL_POSITION, @LightPosition);

    glPopMatrix;

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    glTranslatef(-CenX, -CenY, -CenZ);

    if DoBackground then
      glDrawBackground;

    glDrawLines;
  end;
end;

procedure TGLView.SetMovement(InMove: Boolean);
begin
  Moving := 0;
  AllowMovement := InMove;
end;

procedure TGLView.SelectObjectAt(X, Y: LongWord; Multi: Boolean);
var
  ViewPort: TVector4i;

  SelectBuffer: array[0..PICKSIZE - 1] of GLuint;
  Hits: GLint;
begin
  wglMakeCurrent(dc, rc);
  Picking := True;

  glSelectBuffer(PICKSIZE, @SelectBuffer);
  glRenderMode(GL_SELECT);

	glMatrixMode(GL_PROJECTION);
	glPushMatrix();
	glLoadIdentity();

  glGetIntegerv(GL_VIEWPORT, @ViewPort);
  Y := LongWord(Viewport[3]) - Y - 1;
  gluPickMatrix(X, Y, 2, 2, @Viewport);

  gluPerspective(45.0, Width/Height, 1.0, 8000.0);
  if AllowMovement then
  begin
    glRotatef(FxRot, 1, 0, 0);
    glRotatef(FyRot, 0, 1, 0);
    glRotatef(FzRot, 0, 0, 1);
  end else
  begin
    glTranslatef(0.0, 0.0, Depth);
    glRotatef(xRot, 1, 0, 0);
    glRotatef(yRot, 0, 1, 0);
    glRotatef(zRot, 0, 0, 1);
  end;

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  glTranslatef(-CenX, -CenY, -CenZ);
	glInitNames();

  if Assigned(PickFiles) then
    PickFiles.glDrawAll(GetMovement, GetPosition, Time);

	glMatrixMode(GL_PROJECTION);
	glPopMatrix();
	glMatrixMode(GL_MODELVIEW);
	glFlush();

	Hits := glRenderMode(GL_RENDER);

	if not (Hits = 0) then
  begin
		ProcessHits(Hits, SelectBuffer, Multi);
  end;

  Picking := False;
end;

procedure TGLView.ProcessHits(Hits: GLint; SelectBuffer: array of GLuint; Multi: Boolean);
type
  THit = record
    NCount,
    DNear,
    DFar: GLuint;
    Names: array[0..31] of GLuint;
  end;
  PHit=^THit;
var
  I, MinZ, CurrentPos: LongWord;
  Hit: PHit;
  SelFile, SelObject: LongInt;
begin
  MinZ := LongWord(-1);
  CurrentPos := 0;
  SelFile := -1;
  SelObject := -1;
  if (Hits > PICKSIZE) then
    Hits := PICKSIZE;
  for I := 0 to Hits - 1 do
  begin
    Hit := @SelectBuffer[CurrentPos];
    with Hit^ do
    begin
      Inc(CurrentPos, 3 + NCount);
      if (DNear < MinZ) then
      begin
        SelFile := Names[0];
        SelObject := Names[1];
        MinZ := DNear;
      end;
    end;
  end;
  
  if not (SelFile = -1) and not (SelObject = -1) then
  begin
    if Multi then
      ItemForm.SetToMode(FILE_IPL, SECTION_MULT_INST, SelFile, SelObject)
    else
    begin
      ItemForm.SetToMode(FILE_IPL, SECTION_SEL_INST, SelFile, SelObject);
      JumpToLocation(TIPLFile(PickFiles.Item[SelFile]).Inst.Item[SelObject].Pos);
    end;
  end;
end;

procedure TGLView.SetAxisMode(InMode: Byte);
begin
  AxisMode := InMode;
end;

procedure TGLView.SetWireframe(InWireframe: Boolean);
begin
  wglMakeCurrent(dc, rc);
  if (InWireframe) then
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
  else
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
end;

procedure TGLView.SetTexture(InTexture: Boolean);
begin
  wglMakeCurrent(dc, rc);
  if (InTexture) and (AllowTextureMode) then
  begin
    glEnable(GL_TEXTURE_2D);
    glDisable(GL_LIGHTING);
  end else
  begin
    glDisable(GL_TEXTURE_2D);
    glEnable(GL_LIGHTING);
  end;
end;

procedure TGLView.SetAlphaBlend(InAlpha: Boolean);
begin
  wglMakeCurrent(dc, rc);
  if (InAlpha) then
    glEnable(GL_ALPHA_TEST)
  else
    glDisable(GL_ALPHA_TEST);
end;

procedure TGLView.SetBackColour(InColour: TColor);
begin
  wglMakeCurrent(dc, rc);
  glClearColor(GetRValue(InColour) / 255,
               GetGValue(InColour) / 255,
               GetBValue(InColour) / 255,
               0.0);
end;

function QuatMultiply(Q1, Q2: TVector4f): TVector4f;
begin
  QuatIdentity(Result);

  Result[0] := q1[3] * q2[0] + q1[0] * q2[3] + q1[1] * q2[2] - q1[2] * q2[1];
  Result[1] := q1[3] * q2[1] + q1[1] * q2[3] + q1[2] * q2[0] - q1[0] * q2[2];
  Result[2] := q1[3] * q2[2] + q1[2] * q2[3] + q1[0] * q2[1] - q1[1] * q2[0];
  Result[3] := q1[3] * q2[3] - q1[0] * q2[0] - q1[1] * q2[1] - q1[2] * q2[2];

  QuatNormalise(Result);
end;

function CreateXRotQuad(inRot: Single): TVector4f;
var
  Ca, Sa: Single;
begin
  Sa := Sin(inRot / 2);
  Ca := Cos(inRot / 2);

  Result[0] := Sa;
  Result[1] := 0;
  Result[2] := 0;
  Result[3] := Ca;
end;

function CreateYRotQuad(inRot: Single): TVector4f;
var
  Ca, Sa: Single;
begin
  Sa := Sin(inRot / 2);
  Ca := Cos(inRot / 2);

  Result[0] := 0;
  Result[1] := Sa;
  Result[2] := 0;
  Result[3] := Ca;
end;

function CreateZRotQuad(inRot: Single): TVector4f;
var
  Ca, Sa: Single;
begin
  Sa := Sin(inRot / 2);
  Ca := Cos(inRot / 2);

  Result[0] := 0;
  Result[1] := 0;
  Result[2] := Sa;
  Result[3] := Ca;
end;

procedure QuatIdentity(var Dest: TVector4f);
begin
  Dest[0] := 0.0;
  Dest[1] := 0.0;
  Dest[2] := 0.0;
  Dest[3] := 1.0;
end;

procedure QuatNormalise(var Dest: TVector4f);
var
  RNorm : Single;
begin
  RNorm := 1.0 / Sqrt(Dest[3] * Dest[3] + Dest[0] * Dest[0] + Dest[1] * Dest[1] + Dest[2] * Dest[2]);

  if (RNorm > -EPSILON) and (RNorm < EPSILON) then
    Exit;

  Dest[0] := Dest[0] * RNorm;
  Dest[1] := Dest[1] * RNorm;
  Dest[2] := Dest[2] * RNorm;
  Dest[3] := Dest[3] * RNorm;
end;

procedure TGLView.ScreenToWorldCoordsDrag(X, Y: Double; var OutV: TVector3d);
var
  Proj: TMatrix4d;
  Modl: TMatrix4d;
  ViewPort: TVector4i;
  FindV, NearV, DiffV: TVector3d;
begin
  wglMakeCurrent(dc, rc);

  glGetDoublev(GL_PROJECTION_MATRIX, @Proj);
  glGetDoublev(GL_MODELVIEW_MATRIX, @Modl);
  glGetIntegerv(GL_VIEWPORT, @ViewPort);
  Modl[3][0] := 0;  Modl[3][1] := 0;  Modl[3][2] := 0;
  Y := Viewport[3] - Y - 1;

  gluUnProject(X, Y, 0, @Modl, @Proj, @ViewPort, NearV[0], NearV[1], NearV[2]);
  gluUnProject(X, Y, 1, @Modl, @Proj, @ViewPort, DiffV[0], DiffV[1], DiffV[2]);

  DiffV[0] := DiffV[0] - NearV[0];
  DiffV[1] := DiffV[1] - NearV[1];
  DiffV[2] := DiffV[2] - NearV[2];

  FindV[2] := CenZ - NearV[2];
  FindV[0] := NearV[0] + (FindV[2] / DiffV[2]) * DiffV[0];
  FindV[1] := NearV[1] + (FindV[2] / DiffV[2]) * DiffV[1];

  OutV[0] := NearV[0] + FindV[0];
  OutV[1] := NearV[1] + FindV[1];
  OutV[2] := NearV[2] + FindV[2];
end;

procedure TGLView.ScreenToWorldCoords(X, Y: Double; ObjV: TVector3d; inMode: Byte; var OutV: TVector3d);
var
  Proj: TMatrix4d;
  Modl: TMatrix4d;
  ViewPort: TVector4i;
  FindV, NearV, DiffV: TVector3d;
begin
  wglMakeCurrent(dc, rc);

  glGetDoublev(GL_PROJECTION_MATRIX, @Proj);
  glGetDoublev(GL_MODELVIEW_MATRIX, @Modl);
  glGetIntegerv(GL_VIEWPORT, @ViewPort);
  Y := Viewport[3] - Y - 1;

  gluUnProject(X, Y, 0, @Modl, @Proj, @ViewPort, NearV[0], NearV[1], NearV[2]);
  gluUnProject(X, Y, 1, @Modl, @Proj, @ViewPort, DiffV[0], DiffV[1], DiffV[2]);

  DiffV[0] := DiffV[0] - NearV[0];
  DiffV[1] := DiffV[1] - NearV[1];
  DiffV[2] := DiffV[2] - NearV[2];

  case inMode of
    SCREEN_XYPLANE:
    begin
      FindV[2] := ObjV[2] - NearV[2];
      FindV[0] := NearV[0] + (FindV[2] / DiffV[2]) * DiffV[0];
      FindV[1] := NearV[1] + (FindV[2] / DiffV[2]) * DiffV[1];

      OutV[0] := NearV[0] + FindV[0];
      OutV[1] := NearV[1] + FindV[1];
      OutV[2] := NearV[2] + FindV[2];
    end;
    SCREEN_XZPLANE:
    begin
      FindV[1] := ObjV[1] - NearV[1];
      FindV[0] := NearV[0] + (FindV[1] / DiffV[1]) * DiffV[0];
      FindV[2] := NearV[2] + (FindV[1] / DiffV[1]) * DiffV[2];

      OutV[0] := NearV[0] + FindV[0];
      OutV[1] := NearV[1] + FindV[1];
      OutV[2] := NearV[2] + FindV[2];
    end;
    SCREEN_YZPLANE:
    begin
      FindV[0] := ObjV[0] - NearV[0];
      FindV[1] := NearV[1] + (FindV[0] / DiffV[0]) * DiffV[1];
      FindV[2] := NearV[2] + (FindV[0] / DiffV[0]) * DiffV[2];

      OutV[0] := NearV[0] + FindV[0];
      OutV[1] := NearV[1] + FindV[1];
      OutV[2] := NearV[2] + FindV[2];
    end;
    SCREEN_XAXIS:
    begin
      FindV[1] := ObjV[1] - NearV[1];
      FindV[2] := ObjV[2] - NearV[2];
      FindV[0] := NearV[0] + (FindV[2] / DiffV[2]) * DiffV[0];

      OutV[0] := NearV[0] + FindV[0];
      OutV[1] := NearV[1] + FindV[1];
      OutV[2] := NearV[2] + FindV[2];
    end;
    SCREEN_YAXIS:
    begin
      FindV[0] := ObjV[0] - NearV[0];
      FindV[2] := ObjV[2] - NearV[2];
      FindV[1] := NearV[1] + (FindV[2] / DiffV[2]) * DiffV[1];

      OutV[0] := NearV[0] + FindV[0];
      OutV[1] := NearV[1] + FindV[1];
      OutV[2] := NearV[2] + FindV[2];
    end;
    SCREEN_ZAXIS:
    begin
      FindV[0] := ObjV[0] - NearV[0];
      FindV[1] := ObjV[1] - NearV[1];
      FindV[2] := NearV[2] + (FindV[1] / DiffV[1]) * DiffV[2];

      OutV[0] := NearV[0] + FindV[0];
      OutV[1] := NearV[1] + FindV[1];
      OutV[2] := NearV[2] + FindV[2];
    end;
  end;
end;

procedure TGLView.VMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ObjV: TVector3d;
begin
  if (Button = mbRight) then
  begin
    if (MouseButton = 1) then
      MouseButton := 3
    else
      MouseButton := 2;

    MouseXVal := X;
    MouseYVal := Y;
  end else
  begin
    if AllowMovement then
    begin
      if (MouseButton = 2) then
        MouseButton := 3
      else
        MouseButton := 1;
      Backwards := (ssAlt in Shift);
      Moving := Moving or (1 shl KEY_UP);
      if not MoveTimer.Enabled then
        MoveTimer.Enabled := True;
    end else
    begin
      if (ssDouble in Shift) and (ssLeft in Shift) and not (ssCtrl in Shift) then
        SelectObjectAt(X, Y, (ssShift in Shift))
      else case MouseMode of
        MOUSEMODE_NORMAL:
        begin
          MouseButton := 1;
          MouseXVal := X;
          MouseYVal := Y;

          MoveMode := SCREEN_SCREEN;

          ScreenToWorldCoordsDrag(X, Y, StartV);
        end;

        MOUSEMODE_INST, MOUSEMODE_MULT_INST:
        begin
          if (ssCtrl in Shift) then
          begin
            MouseButton := 1;
            MouseXVal := X;
            MouseYVal := Y;

            MoveMode := SCREEN_XYPLANE;

            ObjV[0] := ItemForm.GetPos[0];
            ObjV[1] := ItemForm.GetPos[1];
            ObjV[2] := ItemForm.GetPos[2];

            ScreenToWorldCoords(X, Y, ObjV, SCREEN_XYPLANE, StartV);
          end else
          begin
            MouseButton := 1;
            MouseXVal := X;
            MouseYVal := Y;

            MoveMode := SCREEN_SCREEN;

            ScreenToWorldCoordsDrag(X, Y, StartV);
          end;
        end;
      end;
    end;
  end;
end;

procedure TGLView.VMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) then
    MouseButton := 1
  else if (ssRight in Shift) then
    MouseButton := 2
  else
    MouseButton := 0;

  MoveMode := SCREEN_NONE;

  if (Button = mbLeft) and AllowMovement then
  begin
    Moving := Moving and not (1 shl KEY_UP);
    if (Moving = 0) then
      MoveTimer.Enabled := False;
  end;
end;

procedure TGLView.VMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  OutV, ObjV: TVector3d;
  SendV: TVector3f;
  SendR: TVector4f;
  La, Lb, Aa, Ab, Change: Single;
begin
  if not (ssRight in Shift) and not (ssLeft in Shift) then
    MouseButton := 0;

  if AllowMovement then
  begin
    if (MouseButton = 2) or (MouseButton = 3) then
    begin
      FxRot := FxRot + (Y - MouseYVal) / 2;  // moving up and down = rot around X-axis
      FzRot := FzRot + (X - MouseXVal) / 2;

      if FxRot < -180 then
        FxRot := -180;
      if FxRot > 0 then
        FxRot := 0;
      if FzRot > 360 then
        FzRot := FzRot - 360;
      if FzRot < 0 then
        FzRot := FzRot + 360;

      Backwards := (ssAlt in Shift);

      MouseXVal := X;
      MouseYVal := Y;
    end;
  end else
  begin
    if (MouseButton = 2) or (MouseButton = 3) then
    begin
      xRot := xRot + (Y - MouseYVal) / 2;  // moving up and down = rot around X-axis
      zRot := zRot + (X - MouseXVal) / 2;

      if xRot > 360 then
        xRot := xRot - 360;
      if xRot < 0 then
        xRot := xRot + 360;
      if zRot > 360 then
        zRot := zRot - 360;
      if zRot < 0 then
        zRot := zRot + 360;

      MouseXVal := X;
      MouseYVal := Y;
    end else if (MouseButton = 1) then
    begin
      case MouseMode of
        MOUSEMODE_NORMAL:
        begin
          ScreenToWorldCoordsDrag(X, Y, OutV);

          if (MoveMode = SCREEN_SCREEN) then
          begin
            SendV[0] := StartV[0] - OutV[0];
            SendV[1] := StartV[1] - OutV[1];
            SendV[2] := StartV[2] - OutV[2];

            CenX := CenX + SendV[0];
            CenY := CenY + SendV[1];

            UpdatePositionBox;
          end else
            MoveMode := SCREEN_SCREEN;

          StartV := OutV;
        end;

        MOUSEMODE_INST:
        begin
          if (ssCtrl in Shift) then
          begin
            if Rotating then
            begin
              // rotating

              if (ssShift in Shift) then
              begin
                ObjV[0] := ItemForm.GetPos[0];
                ObjV[1] := ItemForm.GetPos[1];
                ObjV[2] := ItemForm.GetPos[2];

                ScreenToWorldCoords(X, Y, ObjV, SCREEN_YZPLANE, OutV);

                if (MoveMode = SCREEN_YZPLANE) then
                begin
                  La := OutV[0] - ObjV[0];  Lb := OutV[2] - ObjV[2];
                  Aa := ArcTan(La / Lb);

                  if (Lb < 0) then
                    Aa := PI + Aa;
                  if (Aa < 0) then
                    Aa := 2 * PI + Aa;

                  La := StartV[0] - ObjV[0];  Lb := StartV[2] - ObjV[2];
                  Ab := ArcTan(La / Lb);

                  if (Lb < 0) then
                    Ab := PI + Ab;
                  if (Ab < 0) then
                    Ab := 2 * PI + Ab;

                  Change := Aa - Ab;

                  SendR := QuatMultiply(CreateXRotQuad(Change), ItemForm.GetRot);

                  ItemForm.RotateToPos(SendR);
                end else
                  MoveMode := SCREEN_YZPLANE;

                StartV := OutV;
              end else if (ssAlt in Shift) then
              begin
                ObjV[0] := ItemForm.GetPos[0];
                ObjV[1] := ItemForm.GetPos[1];
                ObjV[2] := ItemForm.GetPos[2];

                ScreenToWorldCoords(X, Y, ObjV, SCREEN_XZPLANE, OutV);

                if (MoveMode = SCREEN_XZPLANE) then
                begin
                  La := OutV[0] - ObjV[0];  Lb := OutV[2] - ObjV[2];
                  Aa := ArcTan(La / Lb);

                  if (Lb < 0) then
                    Aa := PI + Aa;
                  if (Aa < 0) then
                    Aa := 2 * PI + Aa;

                  La := StartV[0] - ObjV[0];  Lb := StartV[2] - ObjV[2];
                  Ab := ArcTan(La / Lb);

                  if (Lb < 0) then
                    Ab := PI + Ab;
                  if (Ab < 0) then
                    Ab := 2 * PI + Ab;

                  Change := Aa - Ab;

                  SendR := QuatMultiply(CreateYRotQuad(Change), ItemForm.GetRot);

                  ItemForm.RotateToPos(SendR);
                end else
                  MoveMode := SCREEN_XZPLANE;

                StartV := OutV;
              end else
              begin
                ObjV[0] := ItemForm.GetPos[0];
                ObjV[1] := ItemForm.GetPos[1];
                ObjV[2] := ItemForm.GetPos[2];

                ScreenToWorldCoords(X, Y, ObjV, SCREEN_XYPLANE, OutV);

                if (MoveMode = SCREEN_XYPLANE) then
                begin
                  La := OutV[0] - ObjV[0];  Lb := OutV[1] - ObjV[1];
                  Aa := ArcTan(La / Lb);

                  if (Lb < 0) then
                    Aa := PI + Aa;
                  if (Aa < 0) then
                    Aa := 2 * PI + Aa;

                  La := StartV[0] - ObjV[0];  Lb := StartV[1] - ObjV[1];
                  Ab := ArcTan(La / Lb);

                  if (Lb < 0) then
                    Ab := PI + Ab;
                  if (Ab < 0) then
                    Ab := 2 * PI + Ab;

                  Change := Aa - Ab;
                  StatusValue.Caption := FloatToStr(Change);

                  SendR := QuatMultiply(CreateZRotQuad(Change), ItemForm.GetRot);

                  ItemForm.RotateToPos(SendR);
                end else
                  MoveMode := SCREEN_XYPLANE;

                StartV := OutV;
              end;

            end else
            begin
              // moving

              if (ssShift in Shift) then
              begin
                ObjV[0] := ItemForm.GetPos[0];
                ObjV[1] := ItemForm.GetPos[1];
                ObjV[2] := ItemForm.GetPos[2];

                case AxisMode of
                  SINGLEAXIS_X:
                  begin
                    ScreenToWorldCoords(X, Y, ObjV, SCREEN_XAXIS, OutV);
                    if (MoveMode = SCREEN_XAXIS) then
                    begin
                      SendV[0] := OutV[0] - StartV[0];
                      SendV[1] := 0;
                      SendV[2] := 0;

                      ItemForm.MoveByAmount(SendV);
                    end else
                      MoveMode := SCREEN_XAXIS;
                  end;
                  SINGLEAXIS_Y:
                  begin
                    ScreenToWorldCoords(X, Y, ObjV, SCREEN_YAXIS, OutV);
                    if (MoveMode = SCREEN_YAXIS) then
                    begin
                      SendV[0] := 0;
                      SendV[1] := OutV[1] - StartV[1];
                      SendV[2] := 0;

                      ItemForm.MoveByAmount(SendV);
                    end else
                      MoveMode := SCREEN_YAXIS;
                  end;
                  SINGLEAXIS_Z:
                  begin
                    ScreenToWorldCoords(X, Y, ObjV, SCREEN_ZAXIS, OutV);
                    if (MoveMode = SCREEN_ZAXIS) then
                    begin
                      SendV[0] := 0;
                      SendV[1] := 0;
                      SendV[2] := OutV[2] - StartV[2];

                      ItemForm.MoveByAmount(SendV);
                    end else
                      MoveMode := SCREEN_ZAXIS;
                  end;
                end;

                StartV := OutV;
              end else
              begin
                ObjV[0] := ItemForm.GetPos[0];
                ObjV[1] := ItemForm.GetPos[1];
                ObjV[2] := ItemForm.GetPos[2];

                ScreenToWorldCoords(X, Y, ObjV, SCREEN_XYPLANE, OutV);

                if (MoveMode = SCREEN_XYPLANE) then
                begin
                  SendV[0] := OutV[0] - StartV[0];
                  SendV[1] := OutV[1] - StartV[1];
                  SendV[2] := 0;

                  ItemForm.MoveByAmount(SendV);
                end else
                  MoveMode := SCREEN_XYPLANE;

                StartV := OutV;
              end;

            end;
          end else
          begin
            ScreenToWorldCoordsDrag(X, Y, OutV);

            if (MoveMode = SCREEN_SCREEN) then
            begin
              SendV[0] := StartV[0] - OutV[0];
              SendV[1] := StartV[1] - OutV[1];
              SendV[2] := StartV[2] - OutV[2];

              CenX := CenX + SendV[0];
              CenY := CenY + SendV[1];

              UpdatePositionBox;
            end else
              MoveMode := SCREEN_SCREEN;

            StartV := OutV;
          end;
        end;

        MOUSEMODE_MULT_INST:
        begin
          if (ssCtrl in Shift) then
          begin
            if (ssShift in Shift) then
            begin
              ObjV[0] := ItemForm.GetPos[0];
              ObjV[1] := ItemForm.GetPos[1];
              ObjV[2] := ItemForm.GetPos[2];

              case AxisMode of
                SINGLEAXIS_X:
                begin
                  ScreenToWorldCoords(X, Y, ObjV, SCREEN_XAXIS, OutV);
                  if (MoveMode = SCREEN_XAXIS) then
                  begin
                    SendV[0] := OutV[0] - StartV[0];
                    SendV[1] := 0;
                    SendV[2] := 0;

                    ItemForm.MoveByAmount(SendV);
                  end else
                    MoveMode := SCREEN_XAXIS;
                end;
                SINGLEAXIS_Y:
                begin
                  ScreenToWorldCoords(X, Y, ObjV, SCREEN_YAXIS, OutV);
                  if (MoveMode = SCREEN_YAXIS) then
                  begin
                    SendV[0] := 0;
                    SendV[1] := OutV[1] - StartV[1];
                    SendV[2] := 0;

                  ItemForm.MoveByAmount(SendV);
                  end else
                    MoveMode := SCREEN_YAXIS;
                end;
                SINGLEAXIS_Z:
                begin
                  ScreenToWorldCoords(X, Y, ObjV, SCREEN_ZAXIS, OutV);
                  if (MoveMode = SCREEN_ZAXIS) then
                  begin
                    SendV[0] := 0;
                    SendV[1] := 0;
                    SendV[2] := OutV[2] - StartV[2];

                    ItemForm.MoveByAmount(SendV);
                  end else
                    MoveMode := SCREEN_ZAXIS;
                end;
              end;

              StartV := OutV;
            end else
            begin
              ObjV[0] := ItemForm.GetPos[0];
              ObjV[1] := ItemForm.GetPos[1];
              ObjV[2] := ItemForm.GetPos[2];

              ScreenToWorldCoords(X, Y, ObjV, SCREEN_XYPLANE, OutV);

              if (MoveMode = SCREEN_XYPLANE) then
              begin
                SendV[0] := OutV[0] - StartV[0];
                SendV[1] := OutV[1] - StartV[1];
                SendV[2] := 0;

                ItemForm.MoveByAmount(SendV);
              end else
                MoveMode := SCREEN_XYPLANE;

              StartV := OutV;
            end;
          end else
          begin
            ScreenToWorldCoordsDrag(X, Y, OutV);

            if (MoveMode = SCREEN_SCREEN) then
            begin
              SendV[0] := StartV[0] - OutV[0];
              SendV[1] := StartV[1] - OutV[1];
              SendV[2] := StartV[2] - OutV[2];

              CenX := CenX + SendV[0];
              CenY := CenY + SendV[1];

              UpdatePositionBox;
            end else
              MoveMode := SCREEN_SCREEN;

            StartV := OutV;
          end;
        end;
      end;
      MouseXVal := X;
      MouseYVal := Y;
    end;
  end;
end;

procedure TGLView.VMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if not AllowMovement then
    Depth := Depth - (Depth * (WheelDelta / 2000));
  Handled := True;
end;

procedure TGLView.VKeyDown(Key: Word; Shift: TShiftState);
begin
  if AllowMovement then case Key of
    VK_HOME: FxRot := -90;
    Ord('W'), VK_UP: Moving := Moving or (1 shl KEY_UP);
    Ord('S'), VK_DOWN: Moving := Moving or (1 shl KEY_DOWN);
    VK_LEFT: Moving := Moving or (1 shl KEY_LEFT);
    VK_RIGHT: Moving := Moving or (1 shl KEY_RIGHT);
    Ord('A'): Moving := Moving or (1 shl KEY_STRAFE_LEFT);
    Ord('D'): Moving := Moving or (1 shl KEY_STRAFE_RIGHT);
    Ord('R'): Moving := Moving or (1 shl KEY_STRAFE_UP);
    Ord('F'): Moving := Moving or (1 shl KEY_STRAFE_DOWN);
    VK_SHIFT:
    begin
      Backwards := (ssAlt in Shift);
      Speed := 2;
    end;
    VK_CONTROL:
    begin
      Backwards := (ssAlt in Shift);
      Speed := 0;
    end;
  end;
  if not MoveTimer.Enabled then
    MoveTimer.Enabled := True;
end;

procedure TGLView.VKeyUp(Key: Word; Shift: TShiftState);
begin
  if AllowMovement then case Key of
    Ord('W'), VK_UP: Moving := Moving and not (1 shl KEY_UP);
    Ord('S'), VK_DOWN: Moving := Moving and not (1 shl KEY_DOWN);
    VK_LEFT: Moving := Moving and not (1 shl KEY_LEFT);
    VK_RIGHT: Moving := Moving and not (1 shl KEY_RIGHT);
    Ord('A'): Moving := Moving and not (1 shl KEY_STRAFE_LEFT);
    Ord('D'): Moving := Moving and not (1 shl KEY_STRAFE_RIGHT);
    Ord('R'): Moving := Moving and not (1 shl KEY_STRAFE_UP);
    Ord('F'): Moving := Moving and not (1 shl KEY_STRAFE_DOWN);
    VK_SHIFT:
    begin
      Backwards := (ssAlt in Shift);
      Speed := 1;
    end;
    VK_CONTROL:
    begin
      Backwards := (ssAlt in Shift);
      Speed := 1;
    end;
  end;
  if (Moving = 0) then
    MoveTimer.Enabled := False;
end;

procedure TGLView.VMoveTimer(Sender: TObject);
var
  ToMove: Single;
begin
  case Speed of
    0: ToMove := SLOW_MOVE_AMOUNT;
    1: ToMove := MOVE_AMOUNT;
    2: ToMove := FAST_MOVE_AMOUNT;
  else
    ToMove := MOVE_AMOUNT;
  end;

  if Backwards then
    ToMove := -ToMove;

  if ((Moving shr KEY_UP) and 1 = 1) then
  begin
    CenX := CenX + Sin(FzRot * PI / 180) * ToMove;
    CenY := CenY + Cos(FzRot * PI / 180) * ToMove;
    CenZ := CenZ - Cos(FxRot * PI / 180) * ToMove;
  end;

  if ((Moving shr KEY_DOWN) and 1 = 1) then
  begin
    CenX := CenX - Sin(FzRot * PI / 180) * ToMove;
    CenY := CenY - Cos(FzRot * PI / 180) * ToMove;
    CenZ := CenZ + Cos(FxRot * PI / 180) * ToMove;
  end;

  if ((Moving shr KEY_STRAFE_LEFT) and 1 = 1) then
  begin
    CenX := CenX + Sin((FzRot - 90) * PI / 180) * ToMove;
    CenY := CenY + Cos((FzRot - 90)  * PI / 180) * ToMove;
  end;

  if ((Moving shr KEY_STRAFE_RIGHT) and 1 = 1) then
  begin
    CenX := CenX + Sin((FzRot + 90)  * PI / 180) * ToMove;
    CenY := CenY + Cos((FzRot + 90)  * PI / 180) * ToMove;
  end;

  if ((Moving shr KEY_STRAFE_UP) and 1 = 1) then
  begin
    CenZ := CenZ + ToMove;
  end;

  if ((Moving shr KEY_STRAFE_DOWN) and 1 = 1) then
  begin
    CenZ := CenZ - ToMove;
  end;

  if ((Moving shr KEY_LEFT) and 1 = 1) then
    FzRot := FzRot - ROT_AMOUNT;

  if ((Moving shr KEY_RIGHT) and 1 = 1) then
    FzRot := FzRot + ROT_AMOUNT;

  UpdatePositionBox;
end;

procedure TGLView.JumpToLocation(InX, InY, InZ: Single);
begin
  CenX := InX;
  CenY := InY;
  CenZ := InZ;

  UpdatePositionBox;
end;

procedure TGLView.ZoomBy(InZoom: Single);
begin
  Depth := Depth + (Depth / InZoom);
end;

procedure TGLView.JumpToLocation(InPos: TVector3f);
begin
  CenX := InPos[0];
  CenY := InPos[1];
  CenZ := InPos[2];

  UpdatePositionBox;
end;

procedure TGLView.SetMouseMode(InMode: Byte);
begin
  MouseMode := inMode;
  UpdateStatusValue;
end;

procedure TGLView.ResetView;
begin
  xRot := 0; yRot := 0; zRot := 0;
  FxRot := -90; FyRot := 0; FzRot := 0;
  Depth := -600;
end;

procedure TGLView.UpdatePositionBox;
begin
  PositionBox.Text := Format('%1.6g, %1.6g, %1.6g', [CenX, CenY, CenZ]);
end;

procedure TGLView.UpdateStatusValue;
begin
  case MouseMode of
    MOUSEMODE_NORMAL:
    begin
      StatusValue.Caption := '<< Nothing >>';
    end;
    MOUSEMODE_INST:
    begin
      StatusValue.Caption := '* Item Selected *';
    end;
    MOUSEMODE_MULT_INST:
    begin
      StatusValue.Caption := '* Lots Selected *';
    end;
  end;
end;

end.
