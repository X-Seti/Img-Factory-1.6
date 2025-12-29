object FormValidate: TFormValidate
  Left = 369
  Top = 152
  Width = 600
  Height = 300
  HelpContext = 23
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Validation Display'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelResult: TPanel
    Left = 0
    Top = 256
    Width = 592
    Height = 17
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      592
      17)
    object Label1: TLabel
      Left = 120
      Top = 0
      Width = 49
      Height = 17
      AutoSize = False
      Caption = 'Result:'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object Label2: TLabel
      Left = 0
      Top = 0
      Width = 41
      Height = 17
      AutoSize = False
      Caption = 'Mode:'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object ModeValue: TLabel
      Left = 40
      Top = 0
      Width = 73
      Height = 17
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object ResultValue: TLabel
      Left = 168
      Top = 0
      Width = 329
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object BtnCopy: TButton
      Left = 496
      Top = 0
      Width = 99
      Height = 17
      Caption = 'Copy To Clipboard'
      TabOrder = 0
      OnClick = BtnCopyClick
    end
  end
  object PanelValidate: TPanel
    Left = 0
    Top = 0
    Width = 592
    Height = 256
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object PanelStatus: TPanel
      Left = 0
      Top = 0
      Width = 150
      Height = 256
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object ListStatus: TListView
        Left = 0
        Top = 41
        Width = 150
        Height = 215
        Align = alClient
        BevelEdges = []
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Color = clBtnFace
        Columns = <
          item
            Width = 130
          end>
        ColumnClick = False
        ReadOnly = True
        RowSelect = True
        ShowColumnHeaders = False
        SmallImages = FormMain.ImageList
        TabOrder = 0
        ViewStyle = vsReport
      end
      object PanelControls: TPanel
        Left = 0
        Top = 0
        Width = 150
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Label3: TLabel
          Left = 0
          Top = 26
          Width = 137
          Height = 15
          AutoSize = False
          Caption = 'Currently Checking:'
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
        end
        object BtnStart: TButton
          Left = 0
          Top = 0
          Width = 137
          Height = 25
          Caption = '&Start Validation'
          Default = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          OnClick = BtnStartClick
        end
      end
    end
    object ListResult: TListBox
      Left = 150
      Top = 0
      Width = 442
      Height = 256
      Align = alClient
      BevelInner = bvNone
      BorderStyle = bsNone
      ItemHeight = 13
      TabOrder = 1
      OnDblClick = ListResultDblClick
    end
  end
end
