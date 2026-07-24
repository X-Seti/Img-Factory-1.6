unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  gtadll, StdCtrls, Buttons;

type
  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    OpenDialog1: TOpenDialog;
    Memo1: TMemo;
    procedure BitBtn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.BitBtn1Click(Sender: TObject);
var
i: integer;
begin
if OpenDialog1.execute = false then exit;

IMGLoadImg(pchar(OpenDialog1.filename));

Memo1.lines.beginupdate;
Memo1.lines.clear;

for i:= 0 to IMGFileCount do begin
Memo1.lines.add(IMGGetFileName(i));
end;

Memo1.lines.endupdate;

end;

end.
