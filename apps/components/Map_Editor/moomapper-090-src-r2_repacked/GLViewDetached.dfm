object FormGLViewDetached: TFormGLViewDetached
  Left = 292
  Top = 186
  Width = 700
  Height = 500
  Caption = '3D View'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DrawPanel: TPanel
    Left = 0
    Top = 0
    Width = 692
    Height = 473
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
  end
end
