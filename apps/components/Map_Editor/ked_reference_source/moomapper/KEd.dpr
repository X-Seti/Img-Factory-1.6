program KEd;

uses
  Forms,
  sysutils,
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
  RequiredTypes in 'RequiredTypes.pas',
  DelfiParser in 'DelfiParser.pas',
  U_MooSettings in 'U_MooSettings.pas' {wnd_setup},
  BitUnit in 'BitUnit.pas',
  U_imgdlg in 'U_imgdlg.pas' {wnd_imgfilepicker},
  ColClass in 'COLADDON\ColClass.pas',
  U_records in 'COLADDON\U_records.pas',
  FastTextCRC in 'COLADDON\FastTextCRC.pas';

{$R *.res}

var
f: file;

begin
  Application.Initialize;
  Application.Title := 'Moo Mapper';

  assignfile(f, extractfiledir(application.exename) + '\colors.inf');
  reset(f, 1);
  blockread(f, u_records.colors, sizeof(u_records.colors));
  closefile(f);

  Application.CreateForm(TFormStartup, FormStartup);
  Application.CreateForm(Twnd_setup, wnd_setup);
  Application.CreateForm(Twnd_imgfilepicker, wnd_imgfilepicker);
  Application.Run;
end.
