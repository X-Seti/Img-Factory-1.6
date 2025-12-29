program KEd;

uses
  Forms,
  Main in 'Main.pas' {FormMain},
  GLView in 'GLView.pas',
  GTACol in 'GTACol.pas',
  GTADff in 'GTADff.pas',
  GTAImg in 'GTAImg.pas',
  GTATxd in 'GTATxd.pas',
  GTAZon in 'GTAZon.pas',
  GTAText in 'GTAText.pas',
  DirDialog in 'DirDialog.pas',
  Loading in 'Loading.pas' {FormLoading},
  EditorArchive in 'EditorArchive.pas' {FormEditorArchive},
  EditorIPL in 'EditorIPL.pas' {FormEditorIPL},
  EditorIDE in 'EditorIDE.pas' {FormEditorIDE},
  ExtraView in 'ExtraView.pas' {FormExtraView},
  AudioADF in 'AudioADF.pas',
  TextureView in 'TextureView.pas' {FormTextureView},
  Startup in 'Startup.pas' {FormStartup},
  EditorItem in 'EditorItem.pas' {FormEditorItem},
  GLViewDetached in 'GLViewDetached.pas' {FormGLViewDetached},
  Validate in 'Validate.pas' {FormValidate},
  EditorDAT in 'EditorDAT.pas' {FormEditorDAT},
  About in 'About.pas' {FormAbout},
  RequiredTypes in 'RequiredTypes.pas';

{$R *.res}

var
  FormLoading: TFormLoading;

function ContinueLoad: Boolean;
begin
  FormLoading := TFormLoading.Create(Application);
  with FormLoading do
  try
    SetMax(6); Show;

    Application.CreateForm(TFormMain, FormMain);
    Main.FormLoading := FormLoading;

    SetStatus('Retrieving GTA Path');

    Result := FormMain.LoadGTAPath;

    if Result then
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

    end;
  finally
    Free;
  end;
  FormLoading := nil;
end;

begin
  Application.Initialize;
  Application.Title := 'Moo Mapper';
  Application.CreateForm(TFormStartup, FormStartup);
  while FormStartup.Visible do
    Application.ProcessMessages;
  FormStartup.Free;
  if Startup.Continue then
    if ContinueLoad then
    begin
      Application.BringToFront;
      Application.Run;
    end;
end.
