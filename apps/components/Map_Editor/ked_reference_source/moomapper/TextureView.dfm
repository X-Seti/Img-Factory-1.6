object FormTextureView: TFormTextureView
  Left = 225
  Top = 186
  Width = 639
  Height = 300
  HelpContext = 22
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Texture Display'
  Color = clBtnFace
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
  object Splitter1: TSplitter
    Left = 250
    Top = 22
    Height = 251
    ResizeStyle = rsUpdate
  end
  object LeftPanel: TPanel
    Left = 0
    Top = 22
    Width = 250
    Height = 251
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object ListTextures: TListView
      Left = 0
      Top = 0
      Width = 250
      Height = 170
      Align = alClient
      Columns = <
        item
          Caption = 'Index'
          Width = 40
        end
        item
          Caption = 'Name'
          Width = 100
        end
        item
          Caption = 'Alpha'
          Width = 100
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
      Top = 170
      Width = 250
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
        Caption = '<null>'
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
        Caption = '<null>'
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
        Caption = '<null>'
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
        Caption = '<null>'
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
        Caption = '<null>'
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
        Caption = '<null>'
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
        Caption = '<null>'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
  end
  object ViewPanel: TPanel
    Left = 253
    Top = 22
    Width = 378
    Height = 251
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
  end
  object FilePanel: TPanel
    Left = 0
    Top = 0
    Width = 631
    Height = 22
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object FilenameLabel: TLabel
      Left = 4
      Top = 4
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
      Top = 4
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
    object Label1: TLabel
      Left = 298
      Top = 4
      Width = 61
      Height = 13
      Caption = 'Background:'
    end
    object ColorBox1: TColorBox
      Left = 370
      Top = 0
      Width = 184
      Height = 22
      Selected = clFuchsia
      ItemHeight = 16
      TabOrder = 0
    end
    object boxalpha: TCheckBox
      Left = 198
      Top = 2
      Width = 97
      Height = 17
      Caption = 'Alpha Blend'
      TabOrder = 1
    end
  end
  object GroupBox1: TGroupBox
    Left = 132
    Top = 72
    Width = 208
    Height = 159
    Caption = 'GroupBox1'
    TabOrder = 3
    Visible = False
    object Image1: TImage
      Left = 13
      Top = 47
      Width = 192
      Height = 112
    end
    object BitBtn1: TBitBtn
      Left = 6
      Top = 16
      Width = 75
      Height = 25
      Caption = 'BitBtn1'
      TabOrder = 0
      OnClick = BitBtn1Click
    end
  end
end
