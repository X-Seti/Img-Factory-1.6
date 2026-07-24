unit About;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ShellApi, ActnList, ComCtrls, jpeg;

type
  TFormAbout = class(TForm)
    CowImage: TImage;
    Label9: TLabel;
    Label10: TLabel;
    BtnClose: TButton;
    Label21: TLabel;
    Label3: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label7: TLabel;
    LabelWebAddress: TLabel;
    Label6: TLabel;
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
    Label23: TLabel;
    Label24: TLabel;
    Image1: TImage;
    Label28: TLabel;
    Label8: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label5: TLabel;
    Label25: TLabel;
    Timer1: TTimer;
    procedure BtnCloseClick(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure LabelWebAddressClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Label24Click(Sender: TObject);
    procedure Label23Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
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
PageControl1.hide;
Timer1.enabled:= true;
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

procedure TFormAbout.Label24Click(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'mailto:stdcall@gmail.com', nil, nil, SW_SHOWNORMAL);
end;

procedure TFormAbout.Label23Click(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'http://www.gtatools.com', nil, nil, SW_SHOWNORMAL);
end;

procedure TFormAbout.Timer1Timer(Sender: TObject);
begin
close;
end;

end.
