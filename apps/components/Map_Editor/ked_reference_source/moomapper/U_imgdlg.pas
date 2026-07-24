unit U_imgdlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Buttons;

type
  Twnd_imgfilepicker = class(TForm)
    GroupBox1: TGroupBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    inp_find: TEdit;
    ListBox1: TListBox;
    listarchive: TListBox;
    procedure inp_findChange(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function FileDialog(const fileo: string; const dffs: boolean): string;
  end;

var
  wnd_imgfilepicker: Twnd_imgfilepicker;

implementation

uses main, GTAImg;

{$R *.dfm}

function Twnd_imgfilepicker.FileDialog(const fileo: string; const dffs: boolean): string;
var
i: integer;
begin
result:= fileo;

ListArchive.Items.Clear;

ListArchive.Items.AddStrings(GArchive.ArchiveList);

inp_find.Text:= fileo;
inp_findChange(self);

if ListBox1.items.count > 0 then begin ListBox1.ItemIndex:= 0; ListBox1Click(self); end;

if wnd_imgfilepicker.ShowModal = mrok then begin
if ListArchive.itemindex <> -1 then result:= ListArchive.Items[ListArchive.Itemindex];
end;

end;

procedure Twnd_imgfilepicker.inp_findChange(Sender: TObject);
var
i: integer;
p: integer;
a: string;
b: string;
begin
ListBox1.items.clear;
try
ListBox1.items.beginupdate;
for i:=0 to listarchive.items.count-1 do begin
a:= lowercase(listarchive.items[i]);
b:= lowercase(inp_find.text);
p:= pos(b, a);

if p <> 0 then ListBox1.items.add(listarchive.items[i]);
end;
finally
ListBox1.items.endupdate;
end;
end;

procedure Twnd_imgfilepicker.ListBox1Click(Sender: TObject);
var i: integer;
begin
for i:= 0 to listarchive.items.count-1 do
listarchive.selected[i]:= false;

listarchive.itemindex:= listarchive.items.indexof(ListBox1.items[ListBox1.itemindex]);
listarchive.selected[listarchive.itemindex]:= true;

end;

end.
