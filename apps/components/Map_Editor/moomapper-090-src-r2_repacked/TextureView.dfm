object FormTextureView: TFormTextureView
  Left = 222
  Top = 189
  Width = 500
  Height = 300
  HelpContext = 22
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Texture Display'
  Color = clBtnFace
  DragKind = dkDock
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LeftPanel: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 273
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object ListTextures: TListView
      Left = 0
      Top = 17
      Width = 185
      Height = 175
      Align = alClient
      Columns = <
        item
          Caption = 'Index'
          Width = 40
        end
        item
          Caption = 'Texture Name'
          Width = 125
        end>
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      HideSelection = False
      OwnerData = True
      ReadOnly = True
      RowSelect = True
      ParentFont = False
      TabOrder = 0
      ViewStyle = vsReport
      OnColumnClick = ListTexturesColumnClick
      OnData = ListTexturesData
      OnSelectItem = ListTexturesSelectItem
    end
    object InfoPanel: TPanel
      Left = 0
      Top = 192
      Width = 185
      Height = 81
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object SizeLabel: TLabel
        Left = 0
        Top = 32
        Width = 33
        Height = 13
        AutoSize = False
        Caption = 'Size:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentFont = False
      end
      object BPPLabel: TLabel
        Left = 104
        Top = 32
        Width = 41
        Height = 13
        AutoSize = False
        Caption = 'Depth:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentFont = False
      end
      object SizeValue: TLabel
        Left = 40
        Top = 32
        Width = 57
        Height = 13
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object BPPValue: TLabel
        Left = 144
        Top = 32
        Width = 41
        Height = 13
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object AlphaLabel: TLabel
        Left = 0
        Top = 48
        Width = 33
        Height = 13
        AutoSize = False
        Caption = 'Alpha:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentFont = False
      end
      object AlphaValue: TLabel
        Left = 40
        Top = 48
        Width = 57
        Height = 13
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object TextureNameLabel: TLabel
        Left = 0
        Top = 0
        Width = 73
        Height = 13
        AutoSize = False
        Caption = 'Texture Name:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentFont = False
      end
      object TextureNameValue: TLabel
        Left = 80
        Top = 0
        Width = 105
        Height = 13
        AutoSize = False
        DragKind = dkDock
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object AlphaNameValue: TLabel
        Left = 80
        Top = 16
        Width = 105
        Height = 13
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object AlphaNameLabel: TLabel
        Left = 0
        Top = 16
        Width = 73
        Height = 13
        AutoSize = False
        Caption = 'Alpha Name:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentFont = False
      end
      object CompressionLabel: TLabel
        Left = 0
        Top = 64
        Width = 65
        Height = 13
        AutoSize = False
        Caption = 'Compression:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentFont = False
      end
      object CompressionValue: TLabel
        Left = 104
        Top = 64
        Width = 81
        Height = 13
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object MipMapsLabel: TLabel
        Left = 104
        Top = 48
        Width = 49
        Height = 13
        AutoSize = False
        Caption = 'MipMaps:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentFont = False
      end
      object MipMapsValue: TLabel
        Left = 160
        Top = 48
        Width = 25
        Height = 13
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object FilePanel: TPanel
      Left = 0
      Top = 0
      Width = 185
      Height = 17
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      object FilenameLabel: TLabel
        Left = 4
        Top = 0
        Width = 53
        Height = 13
        AutoSize = False
        Caption = 'File Name:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentFont = False
      end
      object FilenameValue: TLabel
        Left = 64
        Top = 0
        Width = 121
        Height = 13
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
  end
  object ViewPanel: TPanel
    Left = 185
    Top = 0
    Width = 307
    Height = 273
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
  end
end
