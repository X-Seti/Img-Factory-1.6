unit U_MooSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Grids, u_records;

type
  Twnd_setup = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet3: TTabSheet;
    UpDown1: TUpDown;
    Edit1: TEdit;
    Label1: TLabel;
    UpDown2: TUpDown;
    Edit2: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    UpDown3: TUpDown;
    Edit3: TEdit;
    BoxColour: TColorBox;
    ColorBox1: TColorBox;
    ColorBox2: TColorBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Edit4: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    UpDown4: TUpDown;
    Edit5: TEdit;
    TabSheet2: TTabSheet;
    ListBox1: TListBox;
    Label10: TLabel;
    ColorDialog1: TColorDialog;
    chkDrawIDEPaths: TCheckBox;
    procedure chkDrawIDEPathsClick(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown2Changing(Sender: TObject; var AllowChange: Boolean);
    procedure BoxColourChange(Sender: TObject);
    procedure UpDown3Click(Sender: TObject; Button: TUDBtnType);
    procedure Edit4Change(Sender: TObject);
    procedure UpDown4Click(Sender: TObject; Button: TUDBtnType);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ListBox1DblClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  wnd_setup: Twnd_setup;

implementation

uses requiredtypes, GLView;

{$R *.dfm}

procedure Twnd_setup.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
GLView.RADAR_SIZE_X:= UpDown1.position;
end;

procedure Twnd_setup.UpDown2Changing(Sender: TObject;
  var AllowChange: Boolean);
begin
GLView.RADAR_SIZE_Y:= UpDown2.position;
end;

procedure Twnd_setup.BoxColourChange(Sender: TObject);
begin
requiredtypes.pathlinecolora:= BoxColour.selected;
requiredtypes.pathlinecolorb:= ColorBox1.selected;
requiredtypes.pathlinecolorc:= ColorBox2.selected;
end;

procedure Twnd_setup.UpDown3Click(Sender: TObject; Button: TUDBtnType);
begin
requiredtypes.pathlinewidth:= UpDown3.Position;
end;

procedure Twnd_setup.Edit4Change(Sender: TObject);
begin
requiredtypes.iplpathmp:= strtofloat(Edit4.text);
end;

procedure Twnd_setup.UpDown4Click(Sender: TObject; Button: TUDBtnType);
begin
requiredtypes.pathcubesize:= UpDown4.Position * 0.1;
end;

procedure Twnd_setup.ListBox1DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
	with (Control as TListBox).Canvas do
	begin
	FillRect(Rect);
	TextOut(Rect.Left + 17, Rect.Top, (Control as TListBox).Items[Index]);
  Brush.color:= u_records.colors[index];
  pen.color:= clblue;
  pen.Style:= pssolid;
  FillRect(classes.Rect(rect.left + 1, rect.top + 1, rect.left + 15, rect.bottom-1));
	end;
end;

procedure Twnd_setup.ListBox1DblClick(Sender: TObject);
begin
ColorDialog1.Color:= u_records.colors[ListBox1.itemindex];

if ColorDialog1.execute = true then
u_records.colors[ListBox1.itemindex]:= ColorDialog1.Color;
end;

procedure Twnd_setup.FormClose(Sender: TObject; var Action: TCloseAction);
var
  f: file;
begin
  assignfile(f, extractfiledir(application.exename) + '\colors.inf');
  rewrite(f, 1);
  blockwrite(f, u_records.colors, sizeof(u_records.colors));
  closefile(f);
end;

procedure Twnd_setup.chkDrawIDEPathsClick(Sender: TObject);
begin
  idepathdraw := chkDrawIDEPaths.Checked;
end;

end.
