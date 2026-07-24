unit gtaimg;

interface

type
  Tdirentry = packed record
  startblock: Longword;
  sizeblocks: Longword;
  Name: array[0..23] of char;
  end;

const
  gtainterface = 'gtainterface.dll';

function  IMGLoadImg(const filename: pchar): integer; stdcall; external gtainterface;
procedure IMGExportFile(const index: integer; const filen: pchar); stdcall; external gtainterface;
procedure IMGReplaceFile(const index: integer; const filen: string); stdcall; external gtainterface;
procedure IMGReDoDir; stdcall; external gtainterface;
procedure IMGRemoveFile(const index: integer); stdcall; external gtainterface;
procedure IMGDelete(const index: integer); stdcall; external gtainterface;
function  IMGGetThisFile(const index: integer): Tdirentry; stdcall; external gtainterface;
function  IMGFileCount: integer; stdcall; external gtainterface;


implementation

end.