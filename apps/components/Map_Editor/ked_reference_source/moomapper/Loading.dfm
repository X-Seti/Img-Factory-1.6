object FormLoading: TFormLoading
  Left = 553
  Top = 229
  BorderIcons = []
  BorderStyle = bsToolWindow
  Caption = 'Please Wait... Processing...'
  ClientHeight = 67
  ClientWidth = 256
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LblCurrent: TLabel
    Left = 3
    Top = 0
    Width = 251
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Initialising Program'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LblPart: TLabel
    Left = 3
    Top = 33
    Width = 251
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LoadingProgress: TProgressBar
    Left = 3
    Top = 16
    Width = 251
    Height = 13
    Smooth = True
    Step = 1
    TabOrder = 0
  end
  object PartProgress: TProgressBar
    Left = 3
    Top = 49
    Width = 251
    Height = 13
    Smooth = True
    Step = 1
    TabOrder = 1
  end
end
