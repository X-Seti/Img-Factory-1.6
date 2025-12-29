unit GLViewDetached;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TFormGLViewDetached = class(TForm)
    DrawPanel: TPanel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses Main;

{$R *.dfm}

procedure TFormGLViewDetached.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  MainGLView.ChangeParent(FormMain.DrawPanel);
end;

procedure TFormGLViewDetached.FormShow(Sender: TObject);
begin
  MainGLView.ChangeParent(DrawPanel);
end;

procedure TFormGLViewDetached.FormResize(Sender: TObject);
begin
  MainGLView.ChangeViewport;
end;

procedure TFormGLViewDetached.FormCreate(Sender: TObject);
begin
  OnMouseWheel := MainGLView.VMouseWheel;
end;

procedure TFormGLViewDetached.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  MainGLView.VKeyDown(Key, Shift);
end;

procedure TFormGLViewDetached.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  MainGLView.VKeyUp(Key, Shift);
end;

end.
