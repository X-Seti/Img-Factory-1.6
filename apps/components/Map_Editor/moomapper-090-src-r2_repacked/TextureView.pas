unit TextureView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, ActnList, OpenGl, RequiredTypes; // geometry

type
  TLogPal = record
    lpal: TLogPalette;
    Dummy: array[0..255] of TPaletteEntry;
  end;

  TFormTextureView = class(TForm)
    LeftPanel: TPanel;
    ListTextures: TListView;
    InfoPanel: TPanel;
    SizeLabel: TLabel;
    BPPLabel: TLabel;
    SizeValue: TLabel;
    BPPValue: TLabel;
    AlphaLabel: TLabel;
    AlphaValue: TLabel;
    TextureNameLabel: TLabel;
    TextureNameValue: TLabel;
    AlphaNameValue: TLabel;
    AlphaNameLabel: TLabel;
    CompressionLabel: TLabel;
    CompressionValue: TLabel;
    MipMapsLabel: TLabel;
    MipMapsValue: TLabel;
    FilePanel: TPanel;
    FilenameLabel: TLabel;
    FilenameValue: TLabel;
    ViewPanel: TPanel;
    procedure ListTexturesData(Sender: TObject; Item: TListItem);
    procedure ListTexturesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListTexturesColumnClick(Sender: TObject;
      Column: TListColumn);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FirstShow: Boolean;

    TextureNum: LongInt;
    ImageNum: LongInt;

    TextureBind: LongInt;
    TextureWidth, TextureHeight: LongInt;

    MipMapNum: LongWord;
    DisplayList: TList;
    MainAction: TAction;
    LastSortedBy: Integer;
    rc : HGLRC; // Rendering Context
    dc : HDC;   // Device Context

    procedure UpdateLabels;
    procedure UpdateImage;
    procedure UpdateListSize;
    procedure Sort(SortBy: Byte; SortReverse: Boolean);
    procedure ChangeViewport;
  public
    constructor Create(AOwner: TComponent; inRC: HGLRC);

    property VDC: HDC read dc;

    procedure glDraw;

    procedure SetTexture(InTextureNum: LongInt);
    procedure SetAction(inAction: TAction);
  end;

  function CompareItems(Item1, Item2: LongWord): Integer;

var
  ContextCreated: Boolean = False;

implementation

uses Main, GLView;

var
  DisplaySortBy: Byte; DisplayTextureNum: LongInt;
  DisplayReverse: Boolean;

{$R *.dfm}

constructor TFormTextureView.Create(AOwner: TComponent; inRC: HGLRC);
var
  pfd : TPIXELFORMATDESCRIPTOR;
  pf: Integer;
begin
  inherited Create(AOwner);

  // set parent and get device context
  dc := GetDC(ViewPanel.Handle);

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
  glClearColor(GetRValue(Color) / 255, GetGValue(Color) / 255, GetBValue(Color) / 255, 0.0);
  glClearDepth(1.0);

  glShadeModel(GL_SMOOTH);
  glColorMask(True, True, True, False);

  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

  glEnable(GL_TEXTURE_2D);
  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

  // enable lighting
  glDisable(GL_LIGHTING);
  glViewport(0, 0, ViewPanel.Width, ViewPanel.Height);

  // initial values
  TextureNum := -1;
  TextureBind := -1;
  ImageNum := -1;
  LastSortedBy := -1;
  DisplayList := TList.Create;
end;

procedure TFormTextureView.glDraw;
var
	vx, vy, vx1, vy1: Single;
begin
  if ContextCreated then
  begin
    wglMakeCurrent(dc, rc);

    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_ACCUM_BUFFER_BIT);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();

    gluPerspective(45.0, 1.0, 1.0, 100.0);
    glRotatef(180, 1.0, 0.0, 0.0);
    glTranslatef(0.0, 0.0, 10);

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    glScalef(10 / ViewPanel.Width, 10 / ViewPanel.Height, 1);

    glColor3f(1.0, 1.0, 1.0);

    if not (TextureBind = -1) then
    begin
      glBindTexture(GL_TEXTURE_2D, TextureBind);

      // precalc coordinates
      vx := -TextureWidth * 0.5;    vx1 := vx + TextureWidth;
      vy := +TextureHeight * 0.5;   vy1 := vy - TextureHeight;

      // issue quad
      glBegin(GL_QUADS);
         glTexCoord2f(0, 0);       glVertex2f( vx, vy1);
         glTexCoord2f(1, 0);       glVertex2f(vx1, vy1);
         glTexCoord2f(1, 1);       glVertex2f(vx1,  vy);
         glTexCoord2f(0, 1);       glVertex2f( vx,  vy);
      glEnd;
      // restore state
      glDepthMask(True);
    end;
  end;
end;

function CompareItems(Item1, Item2: LongWord): Integer;
begin
  case DisplaySortBy of
    0: Result := Item1 - Item2;
    1: with Main.GArchive.GTxd[DisplayTextureNum] do
      Result := CompareText(Image[Item1].Name, Image[Item2].Name);
  else
    Result := 0;
  end;
  if DisplayReverse then
    Result := -Result;
end;

procedure TFormTextureView.Sort(SortBy: Byte; SortReverse: Boolean);
begin
  DisplaySortBy := SortBy;
  DisplayReverse := SortReverse;
  DisplayTextureNum := TextureNum;
  if (DisplayList.Count > 0) then
    DisplayList.Sort(@CompareItems);
  ListTextures.ItemIndex := -1;
  ImageNum := -1;
  UpdateLabels;
  ListTextures.Repaint;
end;

procedure TFormTextureView.UpdateListSize;
var
  I: LongWord;
begin
  DisplayList.Clear;
  FilenameValue.Caption := '';
  if not (TextureNum = -1) then with Main.GArchive.GTxd[TextureNum] do
  begin
    FilenameValue.Caption := Name + '.txd';
    if not Loaded then
      LoadFromStream;
    if (ImageCount > 0) then for I := 0 to ImageCount - 1 do
      DisplayList.Add(Pointer(I));
  end;
  ListTextures.Items.Count := DisplayList.Count;
  ListTextures.Repaint;
  if (DisplayList.Count > 0) then
  begin
    ImageNum := 0;
    ListTextures.ItemIndex := ImageNum;
  end else
    ImageNum := -1;
  UpdateLabels;
end;

procedure TFormTextureView.SetTexture(InTextureNum: LongInt);
begin
  if (TextureNum > 0) and GTA_TEXTURE_WHEN_NEEDED then
  begin
    Main.GArchive.GTxd[TextureNum].Unload;
  end;
  TextureNum := InTextureNum;
  UpdateListSize;
end;

procedure TFormTextureView.UpdateImage;
begin
  if not (TextureNum = -1) and not (ImageNum = -1) then
  begin
    with Main.GArchive.GTxd[TextureNum] do
    begin
      TextureBind := ImageTexture[ImageNum];
      TextureWidth := Image[ImageNum].Width;
      TextureHeight := Image[ImageNum].Height;
    end;
  end else
    TextureBind := -1;
end;

procedure TFormTextureView.UpdateLabels;
begin
  if not (TextureNum = -1) and not (ImageNum = -1) then
  begin
    with Main.GArchive.GTxd[TextureNum].Image[ImageNum] do
    begin
      MipMapNum := 0;
      TextureNameValue.Caption := Name;
      AlphaNameValue.Caption := AlphaName;
      SizeValue.Caption := IntToStr(Width) + ' x ' + IntToStr(Height);
      BPPValue.Caption := IntToStr(Depth) + ' bpp';
      if (Alpha = 1) then
        AlphaValue.Caption := 'True'
      else
        AlphaValue.Caption := 'False';
      MipMapsValue.Caption := IntToStr(MipMaps);
      case Compression of
        0: CompressionValue.Caption := 'None';
        1: CompressionValue.Caption := 'DXT1';
        3: CompressionValue.Caption := 'DXT3';
        5: CompressionValue.Caption := 'DXT5';
      else
        CompressionValue.Caption := 'Unknown';
      end;
    end;
  end else
  begin
    MipMapNum := 0;
    TextureNameValue.Caption := '';
    AlphaNameValue.Caption := '';
    SizeValue.Caption := '';
    BPPValue.Caption := '';
    AlphaValue.Caption := '';
    MipMapsValue.Caption := '';
    CompressionValue.Caption := '';
  end;
  UpdateImage;
end;

procedure TFormTextureView.ListTexturesData(Sender: TObject;
  Item: TListItem);
begin
  if not (TextureNum = -1) then with Main.GArchive.GTxd[TextureNum].Image[LongWord(DisplayList.Items[Item.Index])] do
  begin
    if ListTextures.ViewStyle = vsReport then
    begin
      Item.Caption := IntToStr(LongWord(DisplayList.Items[Item.Index]));
      Item.SubItems.Add(Name);
    end else
      Item.Caption := Name;
  end;
end;

procedure TFormTextureView.ListTexturesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if not (Item = nil) then
    ImageNum := Item.Index
  else
    ImageNum := -1;
  UpdateLabels;
end;

procedure TFormTextureView.SetAction(inAction: TAction);
begin
  MainAction := inAction;
end;

procedure TFormTextureView.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if not (MainAction = nil) then
    MainAction.Checked := False;
end;

procedure TFormTextureView.ListTexturesColumnClick(Sender: TObject;
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

procedure TFormTextureView.FormResize(Sender: TObject);
begin
  ChangeViewport;
end;

procedure TFormTextureView.ChangeViewport;
begin
  if ContextCreated then
  begin
    wglMakeCurrent(dc, rc);
    glViewport(0, 0, ViewPanel.Width, ViewPanel.Height);
  end;
end;

procedure TFormTextureView.FormCreate(Sender: TObject);
begin
  FirstShow := True;
end;

procedure TFormTextureView.FormShow(Sender: TObject);
var
  Rct: TRect;
  L, T: LongInt;
begin
  if FirstShow then
  begin
    GetWindowRect(MainGLView.Handle, Rct);
    L := Rct.Left;
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

end.
