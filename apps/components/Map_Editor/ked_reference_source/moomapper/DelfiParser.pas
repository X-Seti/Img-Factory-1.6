unit DelfiParser;

interface

uses windows, sysutils, classes;

procedure setworkspace(str: string);
procedure setworkspacecomma(str: string);
function indexed(n: integer): string;
function intindex(n: integer): integer;
function fltindex(n: integer): single;

var
foo: Tstringlist;

implementation


procedure showmessage(const t: string);
begin
MessageBox(0, pchar(t), '', mb_ok);
end;

function textreplace(asource, afind, areplace: string): string;
var p :integer;
begin
 result:='';
 p:=pos(lowercase(AFind),lowercase(ASource));
  while p > 0 do begin
   result:= result+Copy(ASource, 1, p - 1) + AReplace;
   Delete(ASource, 1, p + Length(AFind) - 1);
   p:= pos(lowercase(AFind),lowercase(ASource));
  end;
 Result:=Result + ASource;
end;

procedure setworkspacecomma(str: string);
var i: integer;

function cleanup(const txt: string): string;
var
w: integer;
begin
result:= '';
for w:= 1 to length(txt) do
result:= result + txt[w];
end;

function fordelete: boolean;
begin
//if (foo[i] = '')  or (foo[i] = ' ') or (foo[i] = '  ');
end;

begin
if foo = nil then foo:= Tstringlist.create;
foo.clear;

foo.CommaText:= str;//textreplace(str,' ', ',');

//showmessage(foo.Text);

for i:= foo.count-1 downto 0 do foo[i]:= cleanup(foo[i]);
//for i:= foo.count-1 downto 0 do if fordelete = true then foo.Delete(i);

//showmessage(foo.Text);

end;

procedure setworkspace(str: string);
var i: integer;
begin
if foo = nil then foo:= Tstringlist.create;
foo.clear;

foo.CommaText:= textreplace(str,' ', ',');
for i:= foo.count-1 downto 0 do if (foo[i] = '') or (foo[i] = ' ') or (foo[i] = '  ') then foo.Delete(i);
end;

function indexed(n: integer): string;
begin
try
if foo = nil then showmessage('text parser not initialized.') else
result:= foo[n];
except outputdebugstring(pchar(format('indexed failed at %d, text is: %s', [n, foo.commatext]))); end;
end;

function intindex(n: integer): integer;
begin
result:= strtoint(indexed(n));
end;

function fltindex(n: integer): single;
begin
result:= strtofloat(indexed(n));
end;

end.
