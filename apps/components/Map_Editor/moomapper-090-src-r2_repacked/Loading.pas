unit Loading;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls;

type
  TFormLoading = class(TForm)
    LblCurrent: TLabel;
    LoadingProgress: TProgressBar;
    PartProgress: TProgressBar;
    LblPart: TLabel;
  private
    { Private declarations }
  public
    procedure SetMax(InMax: Integer);
    procedure SetPartMax(InMax: Integer);
    procedure ResetPos;
    procedure ResetPartPos;
    procedure IncPos;
    procedure IncPartPos;
    procedure SetStatus(InStatus: String);
    procedure SetPart(InStatus: String);
    procedure PartView(InView: Boolean);
  end;

var
  FormLoading: TFormLoading;

implementation

{$R *.dfm}

procedure TFormLoading.ResetPos;
begin
  LoadingProgress.Position := 0;
  LblCurrent.Caption := '';
  Application.ProcessMessages;
end;

procedure TFormLoading.ResetPartPos;
begin
  PartProgress.Position := 0;
  LblPart.Caption := '';
  Application.ProcessMessages;
end;

procedure TFormLoading.SetMax(InMax: Integer);
begin
  LoadingProgress.Max := InMax;
end;

procedure TFormLoading.SetPartMax(InMax: Integer);
begin
  PartProgress.Max := InMax;
end;

procedure TFormLoading.IncPos;
begin
  LoadingProgress.StepIt;
  PartProgress.Position := 0;
  Application.ProcessMessages;
end;

procedure TFormLoading.IncPartPos;
begin
  PartProgress.StepIt;
  Application.ProcessMessages;
end;

procedure TFormLoading.SetStatus(InStatus: String);
begin
  LblCurrent.Caption := InStatus;
  Application.ProcessMessages;
end;

procedure TFormLoading.SetPart(InStatus: String);
begin
  LblPart.Caption := InStatus;
  Application.ProcessMessages;
end;

procedure TFormLoading.PartView(InView: Boolean);
begin
  if InView then
    Height := 115
  else
    Height := 70;
  ResetPartPos;
end;

end.
