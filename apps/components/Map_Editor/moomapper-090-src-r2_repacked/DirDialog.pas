unit DirDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ShellAPI, ShlObj;

const
  VICE_DAT_FILE = 'data\gta_vc.dat';
  GTA3_DAT_FILE = 'data\gta3.dat';
  GDIRFile     = 'models\gta3.dir';
  GIMGFile     = 'models\gta3.img';

type
  TDirDialog = class(TComponent)
  private
    FTitle: string;
    FDirName: string;
    { Private declarations }
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; inPrimary: Boolean); overload;
    function Execute: Boolean;
  published
    property Title: string read FTitle write FTitle;
    property DirName: string read FDirName;
  end;
  function BrowseCallbackProc(hwnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer; stdcall;

implementation

uses Main;

var
  FPrimary: Boolean;


{ TDirDialog }

constructor TDirDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPrimary := True;
end;

constructor TDirDialog.Create(AOwner: TComponent; inPrimary: Boolean);
begin
  inherited Create(AOwner);
  FPrimary := inPrimary;
end;

function BrowseCallbackProc(hwnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer; stdcall;
var
  Path: PChar;
begin
  case uMsg of                  
    BFFM_INITIALIZED:
      begin
        SendMessage(hwnd, BFFM_SETSTATUSTEXT, 0, lpData);
        if FPrimary then
          SendMessage(hwnd, BFFM_ENABLEOK, 0, lpData);
      end;
    BFFM_SELCHANGED:
      begin
        GetMem(Path, MAX_PATH);
        if SHGetPathFromIDList(Pointer(lParam), Path) then
        begin
          SendMessage(hwnd, BFFM_SETSTATUSTEXT, 0, Integer(Path));
          if FPrimary then
          begin
            if GTA_VICE_MODE then
            begin
              if not (FileExists(Path + '\' + GDIRFile) and
                      FileExists(Path + '\' + GIMGFile) and
                      FileExists(Path + '\' + VICE_DAT_FILE)) then
                SendMessage(hwnd, BFFM_ENABLEOK, 0, lpData);
            end else
            begin
              if not (FileExists(Path + '\' + GDIRFile) and
                      FileExists(Path + '\' + GIMGFile) and
                      FileExists(Path + '\' + GTA3_DAT_FILE)) then
                SendMessage(hwnd, BFFM_ENABLEOK, 0, lpData);
            end;
          end;
        end;
        FreeMem(Path);
     end;
  end;
  Result := 0;
end;

function TDirDialog.Execute: Boolean;
var
 lpItemID : PItemIDList;
 BrowseInfo : TBrowseInfo;
 DisplayName : array[0..MAX_PATH] of Char;
 TempPath : array[0..MAX_PATH] of Char;
begin
 FillChar(BrowseInfo, sizeof(TBrowseInfo), #0);

 BrowseInfo.hwndOwner := Application.Handle;
 BrowseInfo.pszDisplayName := @DisplayName;
 BrowseInfo.lpszTitle := PChar(FTitle);
 BrowseInfo.ulFlags := BIF_RETURNONLYFSDIRS or BIF_STATUSTEXT;
 BrowseInfo.lpfn := BrowseCallbackProc;

 lpItemID := SHBrowseForFolder(BrowseInfo);
 Result := not (lpItemId = nil);
 if lpItemId <> nil then begin
   SHGetPathFromIDList(lpItemID, TempPath);
   FDirName := TempPath;
   GlobalFreePtr(lpItemID);
 end;
end;

end.
