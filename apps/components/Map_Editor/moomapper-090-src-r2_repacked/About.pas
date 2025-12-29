unit About;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ShellApi, ActnList;

type
  TFormAbout = class(TForm)
    CowImage: TImage;
    Label7: TLabel;
    LabelWebAddress: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    BtnClose: TButton;
    Label21: TLabel;
    Label3: TLabel;
    procedure BtnCloseClick(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure LabelWebAddressClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    MainAction: TAction;
  public
    procedure SetAction(inAction: TAction);
  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.dfm}

procedure TFormAbout.SetAction(inAction: TAction);
begin
  MainAction := inAction;
end;

procedure TFormAbout.BtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormAbout.Label6Click(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'mailto:moomapper@chronetal.co.uk', nil, nil, SW_SHOWNORMAL);
end;

procedure TFormAbout.LabelWebAddressClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'http://www.chronetal.co.uk/gta/', nil, nil, SW_SHOWNORMAL);
end;

procedure TFormAbout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if not (MainAction = nil) then
    MainAction.Checked := False;
end;

end.
