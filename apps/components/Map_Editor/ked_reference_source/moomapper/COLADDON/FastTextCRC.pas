unit FastTextCRC;

interface

uses windows;

function makecrc(const text: string): integer;

implementation

function makecrc(const text: string): integer;
var
i: integer;
b: string;

function asbyte(const x: string): byte;
begin
Result:= PBYTE(@x[1])^;
end;

begin
result:= 0;

for i:= 0 to Length(text) do begin
b:= copy(text, i, 1);
result:= result + asbyte(b);
end;

end;

end.
