object FormEditorItem: TFormEditorItem
  Left = 216
  Top = 115
  Width = 500
  Height = 300
  HelpContext = 20
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Item Editor Dialog'
  Color = clBtnFace
  DragKind = dkDock
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
  DesignSize = (
    492
    273)
  PixelsPerInch = 96
  TextHeight = 13
  object PanelIDEPath: TPanel
    Left = 0
    Top = 41
    Width = 492
    Height = 232
    Align = alClient
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 8
    Visible = False
    object Label20: TLabel
      Left = 8
      Top = 56
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'ID Number'
    end
    object Label21: TLabel
      Left = 8
      Top = 80
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Model Name'
    end
    object Label22: TLabel
      Left = 8
      Top = 16
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Path Type'
    end
    object EditIDEPathID: TEdit
      Left = 88
      Top = 56
      Width = 121
      Height = 17
      BorderStyle = bsNone
      TabOrder = 2
      OnChange = EditIDEPathIDChange
    end
    object EditIDEPathModel: TEdit
      Left = 88
      Top = 80
      Width = 121
      Height = 17
      BorderStyle = bsNone
      TabOrder = 3
      OnChange = EditIDEPathModelChange
    end
    object EditIDEPathRadioPed: TRadioButton
      Left = 88
      Top = 16
      Width = 113
      Height = 17
      Caption = 'Pedestrian (ped)'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = EditIDEPathRadioPedClick
    end
    object EditIDEPathRadioCar: TRadioButton
      Left = 88
      Top = 32
      Width = 113
      Height = 17
      Caption = 'Vehicle (car)'
      TabOrder = 1
      OnClick = EditIDEPathRadioCarClick
    end
    object EditIDEPathItems: TListView
      Left = 216
      Top = 8
      Width = 273
      Height = 153
      BorderStyle = bsNone
      Columns = <
        item
          Caption = 'Type'
        end
        item
          Caption = 'Connect'
          Width = 60
        end
        item
          AutoSize = True
          Caption = 'Position'
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 4
      ViewStyle = vsReport
      OnSelectItem = EditIDEPathItemsSelectItem
    end
    object PanelIDEPathItem: TPanel
      Left = 8
      Top = 168
      Width = 369
      Height = 57
      TabOrder = 5
      Visible = False
      object Label53: TLabel
        Left = 80
        Top = 34
        Width = 57
        Height = 13
        AutoSize = False
        Caption = 'Position'
      end
      object EditIDEPathItemLL0: TSpeedButton
        Left = 312
        Top = 8
        Width = 17
        Height = 17
        GroupIndex = 1
        Down = True
        Caption = '0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBtnText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = EditIDEPathItemLL0Click
      end
      object EditIDEPathItemLL1: TSpeedButton
        Left = 328
        Top = 8
        Width = 17
        Height = 17
        GroupIndex = 1
        Caption = '1'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBtnText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = EditIDEPathItemLL1Click
      end
      object EditIDEPathItemLL2: TSpeedButton
        Left = 344
        Top = 8
        Width = 17
        Height = 17
        GroupIndex = 1
        Caption = '2'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBtnText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = EditIDEPathItemLL2Click
      end
      object EditIDEPathItemLR0: TSpeedButton
        Left = 312
        Top = 32
        Width = 17
        Height = 17
        GroupIndex = 2
        Down = True
        Caption = '0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBtnText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = EditIDEPathItemLR0Click
      end
      object EditIDEPathItemLR1: TSpeedButton
        Left = 328
        Top = 32
        Width = 17
        Height = 17
        GroupIndex = 2
        Caption = '1'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBtnText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = EditIDEPathItemLR1Click
      end
      object EditIDEPathItemLR2: TSpeedButton
        Left = 344
        Top = 32
        Width = 17
        Height = 17
        GroupIndex = 2
        Caption = '2'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBtnText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = EditIDEPathItemLR2Click
      end
      object Label54: TLabel
        Left = 264
        Top = 10
        Width = 41
        Height = 13
        AutoSize = False
        Caption = 'Lanes'
      end
      object EditIDEPathItemTypeNone: TSpeedButton
        Left = 8
        Top = 8
        Width = 41
        Height = 17
        GroupIndex = 3
        Down = True
        Caption = 'None'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBtnText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = EditIDEPathItemTypeNoneClick
      end
      object EditIDEPathItemTypeMid: TSpeedButton
        Left = 48
        Top = 8
        Width = 33
        Height = 17
        GroupIndex = 3
        Caption = 'Mid'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBtnText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = EditIDEPathItemTypeMidClick
      end
      object EditIDEPathItemTypeEnd: TSpeedButton
        Left = 80
        Top = 8
        Width = 33
        Height = 17
        GroupIndex = 3
        Caption = 'End'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBtnText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = EditIDEPathItemTypeEndClick
      end
      object Label55: TLabel
        Left = 120
        Top = 10
        Width = 57
        Height = 13
        AutoSize = False
        Caption = 'Unknown'
      end
      object Label56: TLabel
        Left = 8
        Top = 34
        Width = 25
        Height = 13
        AutoSize = False
        Caption = 'To'
      end
      object EditIDEPathItemPosX: TEdit
        Left = 136
        Top = 32
        Width = 57
        Height = 17
        BorderStyle = bsNone
        TabOrder = 3
        OnChange = EditIDEPathItemPosXChange
      end
      object EditIDEPathItemPosY: TEdit
        Left = 192
        Top = 32
        Width = 57
        Height = 17
        BorderStyle = bsNone
        TabOrder = 4
        OnChange = EditIDEPathItemPosYChange
      end
      object EditIDEPathItemPosZ: TEdit
        Left = 248
        Top = 32
        Width = 57
        Height = 17
        BorderStyle = bsNone
        TabOrder = 5
        OnChange = EditIDEPathItemPosZChange
      end
      object EditIDEPathItemU3: TEdit
        Left = 184
        Top = 8
        Width = 33
        Height = 17
        BorderStyle = bsNone
        TabOrder = 0
        OnChange = EditIDEPathItemU3Change
      end
      object EditIDEPathItemU7: TEdit
        Left = 216
        Top = 8
        Width = 33
        Height = 17
        BorderStyle = bsNone
        TabOrder = 1
        OnChange = EditIDEPathItemU7Change
      end
      object EditIDEPathItemConnect: TEdit
        Left = 32
        Top = 32
        Width = 33
        Height = 17
        BorderStyle = bsNone
        TabOrder = 2
        OnChange = EditIDEPathItemConnectChange
      end
    end
  end
  object PanelIDE2dfx: TPanel
    Left = 0
    Top = 41
    Width = 492
    Height = 232
    Align = alClient
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 9
    Visible = False
    object Label7: TLabel
      Left = 8
      Top = 16
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'ID Number'
    end
    object Label8: TLabel
      Left = 224
      Top = 16
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Position'
    end
    object Label9: TLabel
      Left = 8
      Top = 48
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Colour'
    end
    object Label10: TLabel
      Left = 304
      Top = 80
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Effect Type'
    end
    object Label63: TLabel
      Left = 304
      Top = 56
      Width = 89
      Height = 13
      AutoSize = False
      Caption = 'View Distance'
    end
    object PanelIDE2dfxLight: TPanel
      Left = 8
      Top = 104
      Width = 369
      Height = 121
      TabOrder = 14
      Visible = False
      object Label11: TLabel
        Left = 8
        Top = 8
        Width = 73
        Height = 13
        AutoSize = False
        Caption = 'Effect 1'
      end
      object Label12: TLabel
        Left = 8
        Top = 32
        Width = 73
        Height = 13
        AutoSize = False
        Caption = 'Effect 2'
      end
      object Label13: TLabel
        Left = 8
        Top = 64
        Width = 73
        Height = 13
        AutoSize = False
        Caption = 'Distance'
      end
      object Label15: TLabel
        Left = 224
        Top = 8
        Width = 41
        Height = 13
        AutoSize = False
        Caption = 'Sizes'
      end
      object Label16: TLabel
        Left = 224
        Top = 32
        Width = 41
        Height = 13
        AutoSize = False
        Caption = 'Range'
      end
      object Label17: TLabel
        Left = 8
        Top = 96
        Width = 97
        Height = 13
        AutoSize = False
        Caption = 'Wet Reflection'
      end
      object Label14: TLabel
        Left = 160
        Top = 96
        Width = 65
        Height = 13
        AutoSize = False
        Caption = 'Lens Flare'
      end
      object Label62: TLabel
        Left = 280
        Top = 96
        Width = 33
        Height = 13
        AutoSize = False
        Caption = 'Dust'
      end
      object EditIDE2dfxAEffect1: TEdit
        Left = 88
        Top = 8
        Width = 121
        Height = 17
        BorderStyle = bsNone
        TabOrder = 0
        OnChange = EditIDE2dfxAEffect1Change
      end
      object EditIDE2dfxAEffect2: TEdit
        Left = 88
        Top = 32
        Width = 121
        Height = 17
        BorderStyle = bsNone
        TabOrder = 1
        OnChange = EditIDE2dfxAEffect2Change
      end
      object EditIDE2dfxADistance: TEdit
        Left = 88
        Top = 64
        Width = 57
        Height = 17
        BorderStyle = bsNone
        TabOrder = 2
        OnChange = EditIDE2dfxADistanceChange
      end
      object EditIDE2dfxASizeLamp: TEdit
        Left = 264
        Top = 8
        Width = 49
        Height = 17
        BorderStyle = bsNone
        TabOrder = 3
        OnChange = EditIDE2dfxASizeLampChange
      end
      object EditIDE2dfxASizeCorona: TEdit
        Left = 312
        Top = 8
        Width = 49
        Height = 17
        BorderStyle = bsNone
        TabOrder = 4
        OnChange = EditIDE2dfxASizeCoronaChange
      end
      object EditIDE2dfxAReflectionWet: TEdit
        Left = 104
        Top = 96
        Width = 49
        Height = 17
        BorderStyle = bsNone
        TabOrder = 8
        OnChange = EditIDE2dfxAReflectionWetChange
      end
      object EditIDE2dfxALensFlare: TEdit
        Left = 224
        Top = 96
        Width = 49
        Height = 17
        BorderStyle = bsNone
        TabOrder = 9
        OnChange = EditIDE2dfxALensFlareChange
      end
      object EditIDE2dfxADust: TEdit
        Left = 312
        Top = 96
        Width = 49
        Height = 17
        BorderStyle = bsNone
        TabOrder = 10
        OnChange = EditIDE2dfxADustChange
      end
      object EditIDE2dfxARangeOuter: TEdit
        Left = 264
        Top = 32
        Width = 49
        Height = 17
        BorderStyle = bsNone
        TabOrder = 5
        OnChange = EditIDE2dfxARangeOuterChange
      end
      object EditIDE2dfxARangeInner: TEdit
        Left = 312
        Top = 32
        Width = 49
        Height = 17
        BorderStyle = bsNone
        TabOrder = 6
        OnChange = EditIDE2dfxARangeInnerChange
      end
      object EditIDE2dfxAControl: TComboBox
        Left = 152
        Top = 64
        Width = 209
        Height = 21
        BevelInner = bvNone
        BevelOuter = bvNone
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 7
        Text = 'Light: Day & Night'
        OnChange = EditIDE2dfxAControlChange
        Items.Strings = (
          'Light: Day & Night'
          'Light: Night Only'
          'Flicker: Day & Night'
          'Flicker: Night Only'
          'Blink (1 sec): Day & Night'
          'Blink (1 sec): Night Only'
          'Blink (2 sec): Day & Night'
          'Blink (2 sec): Night Only'
          'Light: Day Only'
          'Blink (3 sec): Night Only'
          'Blink (3 sec): Day & Night'
          'Random Flicker: Night'
          'Unknown'
          'Unknown'
          'Unknown'
          'Unknown')
      end
    end
    object PanelIDE2dfxParticle: TPanel
      Left = 8
      Top = 103
      Width = 369
      Height = 121
      TabOrder = 15
      Visible = False
      object Label18: TLabel
        Left = 8
        Top = 40
        Width = 73
        Height = 13
        AutoSize = False
        Caption = 'Rotation'
      end
      object Label19: TLabel
        Left = 8
        Top = 8
        Width = 73
        Height = 13
        AutoSize = False
        Caption = 'Unknown'
      end
      object EditIDE2dfxBRotX: TEdit
        Left = 88
        Top = 40
        Width = 65
        Height = 17
        BorderStyle = bsNone
        TabOrder = 1
        OnChange = EditIDE2dfxBRotXChange
      end
      object EditIDE2dfxBRotZ: TEdit
        Left = 216
        Top = 40
        Width = 65
        Height = 17
        BorderStyle = bsNone
        TabOrder = 2
        OnChange = EditIDE2dfxBRotZChange
      end
      object EditIDE2dfxBRotY: TEdit
        Left = 152
        Top = 40
        Width = 65
        Height = 17
        BorderStyle = bsNone
        TabOrder = 3
        OnChange = EditIDE2dfxBRotYChange
      end
      object EditIDE2dfxBRotW: TEdit
        Left = 296
        Top = 40
        Width = 57
        Height = 17
        BorderStyle = bsNone
        TabOrder = 4
        OnChange = EditIDE2dfxBRotWChange
      end
      object EditIDE2dfxBType: TComboBox
        Left = 88
        Top = 8
        Width = 193
        Height = 21
        BevelInner = bvNone
        BevelOuter = bvNone
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 0
        Text = 'Steam: Medium'
        OnChange = EditIDE2dfxBTypeChange
        Items.Strings = (
          'Steam: Medium'
          'Steam: Small'
          'Steam: Large'
          'Fire'
          'Smoke'
          'Water: Spray Up'
          'Water: Spray Down')
      end
    end
    object PanelIDE2dfxAnimation: TPanel
      Left = 8
      Top = 103
      Width = 369
      Height = 121
      TabOrder = 17
      Visible = False
      object Label50: TLabel
        Left = 8
        Top = 40
        Width = 73
        Height = 13
        AutoSize = False
        Caption = 'Direction 1'
      end
      object Label51: TLabel
        Left = 8
        Top = 8
        Width = 73
        Height = 13
        AutoSize = False
        Caption = 'Type'
      end
      object Label52: TLabel
        Left = 8
        Top = 64
        Width = 73
        Height = 13
        AutoSize = False
        Caption = 'Direction 2'
      end
      object EditIDE2dfxDDir1X: TEdit
        Left = 88
        Top = 40
        Width = 65
        Height = 17
        BorderStyle = bsNone
        TabOrder = 1
        OnChange = EditIDE2dfxDDir1XChange
      end
      object EditIDE2dfxDDir1Z: TEdit
        Left = 216
        Top = 40
        Width = 65
        Height = 17
        BorderStyle = bsNone
        TabOrder = 2
        OnChange = EditIDE2dfxDDir1ZChange
      end
      object EditIDE2dfxDDir1Y: TEdit
        Left = 152
        Top = 40
        Width = 65
        Height = 17
        BorderStyle = bsNone
        TabOrder = 3
        OnChange = EditIDE2dfxDDir1YChange
      end
      object EditIDE2dfxDDir2X: TEdit
        Left = 88
        Top = 64
        Width = 65
        Height = 17
        BorderStyle = bsNone
        TabOrder = 4
        OnChange = EditIDE2dfxDDir2XChange
      end
      object EditIDE2dfxDDir2Y: TEdit
        Left = 152
        Top = 64
        Width = 65
        Height = 17
        BorderStyle = bsNone
        TabOrder = 5
        OnChange = EditIDE2dfxDDir2YChange
      end
      object EditIDE2dfxDDir2Z: TEdit
        Left = 216
        Top = 64
        Width = 65
        Height = 17
        BorderStyle = bsNone
        TabOrder = 6
        OnChange = EditIDE2dfxDDir2ZChange
      end
      object EditIDE2dfxDType: TComboBox
        Left = 88
        Top = 8
        Width = 193
        Height = 21
        BevelInner = bvNone
        BevelOuter = bvNone
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 0
        Text = 'Automat (Handle With Object)'
        OnChange = EditIDE2dfxDTypeChange
        Items.Strings = (
          'Automat (Handle With Object)'
          'Seat Place '
          'Bus Stop'
          'Display Window'
          'Unknown')
      end
    end
    object PanelIDE2dfxUnknown: TPanel
      Left = 8
      Top = 103
      Width = 369
      Height = 121
      Caption = 'Not Available'
      TabOrder = 16
      Visible = False
    end
    object PanelIDE2dfxReflection: TPanel
      Left = 8
      Top = 103
      Width = 369
      Height = 121
      Caption = 'Not Available'
      TabOrder = 18
      Visible = False
    end
    object EditIDE2dfxID: TEdit
      Left = 88
      Top = 16
      Width = 121
      Height = 17
      BorderStyle = bsNone
      TabOrder = 0
      OnChange = EditIDE2dfxIDChange
    end
    object EditIDE2dfxPosX: TEdit
      Left = 296
      Top = 16
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 5
      OnChange = EditIDE2dfxPosXChange
    end
    object EditIDE2dfxPosY: TEdit
      Left = 360
      Top = 16
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 6
      OnChange = EditIDE2dfxPosYChange
    end
    object EditIDE2dfxPosZ: TEdit
      Left = 424
      Top = 16
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 7
      OnChange = EditIDE2dfxPosZChange
    end
    object EditIDE2dfxColR: TEdit
      Left = 88
      Top = 48
      Width = 49
      Height = 17
      BorderStyle = bsNone
      TabOrder = 1
      OnChange = EditIDE2dfxColRChange
    end
    object EditIDE2dfxColG: TEdit
      Left = 136
      Top = 48
      Width = 49
      Height = 17
      BorderStyle = bsNone
      TabOrder = 2
      OnChange = EditIDE2dfxColGChange
    end
    object EditIDE2dfxColB: TEdit
      Left = 184
      Top = 48
      Width = 49
      Height = 17
      BorderStyle = bsNone
      TabOrder = 3
      OnChange = EditIDE2dfxColBChange
    end
    object EditIDE2dfxColChooser: TColorBox
      Left = 88
      Top = 72
      Width = 185
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames]
      BevelOuter = bvNone
      ItemHeight = 16
      TabOrder = 4
      OnChange = EditIDE2dfxColChooserChange
    end
    object EditIDE2dfxRadioLight: TRadioButton
      Left = 392
      Top = 80
      Width = 89
      Height = 17
      Caption = 'Lighting'
      Checked = True
      TabOrder = 9
      TabStop = True
      OnClick = EditIDE2dfxRadioLightClick
    end
    object EditIDE2dfxRadioParticle: TRadioButton
      Left = 392
      Top = 96
      Width = 89
      Height = 17
      Caption = 'Particle'
      TabOrder = 10
      OnClick = EditIDE2dfxRadioParticleClick
    end
    object EditIDE2dfxRadioUnknown: TRadioButton
      Left = 392
      Top = 112
      Width = 89
      Height = 17
      Caption = 'N/A'
      Enabled = False
      TabOrder = 11
    end
    object EditIDE2dfxRadioAnimation: TRadioButton
      Left = 392
      Top = 128
      Width = 89
      Height = 17
      Caption = 'Animation'
      TabOrder = 12
      OnClick = EditIDE2dfxRadioAnimationClick
    end
    object EditIDE2dfxRadioReflection: TRadioButton
      Left = 392
      Top = 144
      Width = 89
      Height = 17
      Caption = 'Reflection'
      TabOrder = 13
      OnClick = EditIDE2dfxRadioReflectionClick
    end
    object EditIDE2dfxViewDistance: TEdit
      Left = 392
      Top = 56
      Width = 97
      Height = 17
      BorderStyle = bsNone
      TabOrder = 8
      OnChange = EditIDE2dfxViewDistanceChange
    end
  end
  object PanelNA: TPanel
    Left = 0
    Top = 41
    Width = 492
    Height = 232
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Not Available'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
  end
  object PanelIPLInst: TPanel
    Left = 0
    Top = 41
    Width = 492
    Height = 232
    Align = alClient
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    Visible = False
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'ID Number'
    end
    object Label2: TLabel
      Left = 8
      Top = 32
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Model Name'
    end
    object Label3: TLabel
      Left = 8
      Top = 56
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Interior'
    end
    object Label4: TLabel
      Left = 224
      Top = 8
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Position'
    end
    object Label5: TLabel
      Left = 224
      Top = 56
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Scale'
    end
    object Label6: TLabel
      Left = 8
      Top = 80
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Rotation'
    end
    object Label45: TLabel
      Left = 392
      Top = 80
      Width = 41
      Height = 13
      AutoSize = False
      Caption = 'Rot X'
    end
    object Label48: TLabel
      Left = 392
      Top = 96
      Width = 41
      Height = 13
      AutoSize = False
      Caption = 'Rot Y'
    end
    object Label49: TLabel
      Left = 392
      Top = 112
      Width = 41
      Height = 13
      AutoSize = False
      Caption = 'Rot Z'
    end
    object EditIPLInstPosZR1: TPanel
      Left = 464
      Top = 24
      Width = 17
      Height = 17
      BevelOuter = bvNone
      Caption = '>>'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 23
      OnMouseDown = EditIPLInstPosZR1MouseDown
      OnMouseUp = EditIPLInstRotZR2MouseUp
    end
    object EditIPLInstPosZR2: TPanel
      Left = 456
      Top = 24
      Width = 9
      Height = 17
      BevelOuter = bvNone
      Caption = '>'
      TabOrder = 24
      OnMouseDown = EditIPLInstPosZR2MouseDown
      OnMouseUp = EditIPLInstRotZR2MouseUp
    end
    object EditIPLInstPosZL2: TPanel
      Left = 448
      Top = 24
      Width = 9
      Height = 17
      BevelOuter = bvNone
      Caption = '<'
      TabOrder = 25
      OnMouseDown = EditIPLInstPosZL2MouseDown
      OnMouseUp = EditIPLInstRotZR2MouseUp
    end
    object EditIPLInstPosZL1: TPanel
      Left = 432
      Top = 24
      Width = 17
      Height = 17
      BevelOuter = bvNone
      Caption = '<<'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 26
      OnMouseDown = EditIPLInstPosZL1MouseDown
      OnMouseUp = EditIPLInstRotZR2MouseUp
    end
    object EditIPLInstPosYR1: TPanel
      Left = 400
      Top = 24
      Width = 17
      Height = 17
      BevelOuter = bvNone
      Caption = '>>'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 19
      OnMouseDown = EditIPLInstPosYR1MouseDown
      OnMouseUp = EditIPLInstRotZR2MouseUp
    end
    object EditIPLInstPosYR2: TPanel
      Left = 392
      Top = 24
      Width = 9
      Height = 17
      BevelOuter = bvNone
      Caption = '>'
      TabOrder = 20
      OnMouseDown = EditIPLInstPosYR2MouseDown
      OnMouseUp = EditIPLInstRotZR2MouseUp
    end
    object EditIPLInstPosYL1: TPanel
      Left = 368
      Top = 24
      Width = 17
      Height = 17
      BevelOuter = bvNone
      Caption = '<<'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 22
      OnMouseDown = EditIPLInstPosYL1MouseDown
      OnMouseUp = EditIPLInstRotZR2MouseUp
    end
    object EditIPLInstPosYL2: TPanel
      Left = 384
      Top = 24
      Width = 9
      Height = 17
      BevelOuter = bvNone
      Caption = '<'
      TabOrder = 21
      OnMouseDown = EditIPLInstPosYL2MouseDown
      OnMouseUp = EditIPLInstRotZR2MouseUp
    end
    object EditIPLInstPosXR1: TPanel
      Left = 336
      Top = 24
      Width = 17
      Height = 17
      BevelOuter = bvNone
      Caption = '>>'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 18
      OnMouseDown = EditIPLInstPosXR1MouseDown
      OnMouseUp = EditIPLInstRotZR2MouseUp
    end
    object EditIPLInstPosXR2: TPanel
      Left = 328
      Top = 24
      Width = 9
      Height = 17
      BevelOuter = bvNone
      Caption = '>'
      TabOrder = 17
      OnMouseDown = EditIPLInstPosXR2MouseDown
      OnMouseUp = EditIPLInstRotZR2MouseUp
    end
    object EditIPLInstPosXL1: TPanel
      Left = 304
      Top = 24
      Width = 17
      Height = 17
      BevelOuter = bvNone
      Caption = '<<'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 15
      OnMouseDown = EditIPLInstPosXL1MouseDown
      OnMouseUp = EditIPLInstRotZR2MouseUp
    end
    object EditIPLInstPosXL2: TPanel
      Left = 320
      Top = 24
      Width = 9
      Height = 17
      BevelOuter = bvNone
      Caption = '<'
      TabOrder = 16
      OnMouseDown = EditIPLInstPosXL2MouseDown
      OnMouseUp = EditIPLInstRotZR2MouseUp
    end
    object EditIPLInstID: TEdit
      Left = 88
      Top = 8
      Width = 121
      Height = 17
      BorderStyle = bsNone
      TabOrder = 0
      OnChange = EditIPLInstIDChange
    end
    object EditIPLInstInterior: TEdit
      Left = 88
      Top = 56
      Width = 97
      Height = 17
      BorderStyle = bsNone
      TabOrder = 2
      OnChange = EditIPLInstInteriorChange
    end
    object EditIPLInstPosX: TEdit
      Left = 296
      Top = 8
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 3
      OnChange = EditIPLInstPosXChange
    end
    object EditIPLInstScaleX: TEdit
      Left = 296
      Top = 56
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 6
      OnChange = EditIPLInstScaleXChange
    end
    object EditIPLInstRotX: TEdit
      Left = 88
      Top = 80
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 9
      OnChange = EditIPLInstRotXChange
    end
    object EditIPLInstPosY: TEdit
      Left = 360
      Top = 8
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 4
      OnChange = EditIPLInstPosYChange
    end
    object EditIPLInstPosZ: TEdit
      Left = 424
      Top = 8
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 5
      OnChange = EditIPLInstPosZChange
    end
    object EditIPLInstScaleY: TEdit
      Left = 360
      Top = 56
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 7
      OnChange = EditIPLInstScaleYChange
    end
    object EditIPLInstScaleZ: TEdit
      Left = 424
      Top = 56
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 8
      OnChange = EditIPLInstScaleZChange
    end
    object EditIPLInstRotY: TEdit
      Left = 152
      Top = 80
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 10
      OnChange = EditIPLInstRotYChange
    end
    object EditIPLInstRotZ: TEdit
      Left = 216
      Top = 80
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 11
      OnChange = EditIPLInstRotZChange
    end
    object EditIPLInstRotW: TEdit
      Left = 296
      Top = 80
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 12
      OnChange = EditIPLInstRotWChange
    end
    object EditIPLInstModel: TEdit
      Left = 88
      Top = 32
      Width = 121
      Height = 17
      BorderStyle = bsNone
      TabOrder = 1
      OnChange = EditIPLInstModelChange
    end
    object EditIPLInstHasInterior: TCheckBox
      Left = 192
      Top = 56
      Width = 17
      Height = 17
      TabOrder = 13
      OnClick = EditIPLInstHasInteriorClick
    end
    object EditIPLInstBoxCheck: TGroupBox
      Left = 88
      Top = 128
      Width = 289
      Height = 97
      Caption = 'Validation'
      TabOrder = 14
      object EditIPLInstValidation: TListView
        Left = 8
        Top = 16
        Width = 273
        Height = 73
        BevelEdges = []
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Color = clBtnFace
        Columns = <
          item
            Width = 190
          end
          item
            Width = 80
          end>
        ColumnClick = False
        ReadOnly = True
        RowSelect = True
        ShowColumnHeaders = False
        SmallImages = FormMain.ImageList
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object EditIPLInstRotXL1: TPanel
      Left = 432
      Top = 78
      Width = 17
      Height = 17
      BevelOuter = bvNone
      Caption = '<<'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 27
      OnMouseDown = EditIPLInstRotXL1MouseDown
      OnMouseUp = EditIPLInstRotZR2MouseUp
    end
    object EditIPLInstRotXL2: TPanel
      Left = 448
      Top = 78
      Width = 9
      Height = 17
      BevelOuter = bvNone
      Caption = '<'
      TabOrder = 28
      OnMouseDown = EditIPLInstRotXL2MouseDown
      OnMouseUp = EditIPLInstRotZR2MouseUp
    end
    object EditIPLInstRotXR2: TPanel
      Left = 456
      Top = 78
      Width = 9
      Height = 17
      BevelOuter = bvNone
      Caption = '>'
      TabOrder = 29
      OnMouseDown = EditIPLInstRotXR2MouseDown
      OnMouseUp = EditIPLInstRotZR2MouseUp
    end
    object EditIPLInstRotXR1: TPanel
      Left = 464
      Top = 78
      Width = 17
      Height = 17
      BevelOuter = bvNone
      Caption = '>>'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 30
      OnMouseDown = EditIPLInstRotXR1MouseDown
      OnMouseUp = EditIPLInstRotZR2MouseUp
    end
    object EditIPLInstRotYL1: TPanel
      Left = 432
      Top = 94
      Width = 17
      Height = 17
      BevelOuter = bvNone
      Caption = '<<'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 31
      OnMouseDown = EditIPLInstRotYL1MouseDown
      OnMouseUp = EditIPLInstRotZR2MouseUp
    end
    object EditIPLInstRotYL2: TPanel
      Left = 448
      Top = 94
      Width = 9
      Height = 17
      BevelOuter = bvNone
      Caption = '<'
      TabOrder = 32
      OnMouseDown = EditIPLInstRotYL2MouseDown
      OnMouseUp = EditIPLInstRotZR2MouseUp
    end
    object EditIPLInstRotYR2: TPanel
      Left = 456
      Top = 94
      Width = 9
      Height = 17
      BevelOuter = bvNone
      Caption = '>'
      TabOrder = 33
      OnMouseDown = EditIPLInstRotYR2MouseDown
      OnMouseUp = EditIPLInstRotZR2MouseUp
    end
    object EditIPLInstRotYR1: TPanel
      Left = 464
      Top = 94
      Width = 17
      Height = 17
      BevelOuter = bvNone
      Caption = '>>'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 34
      OnMouseDown = EditIPLInstRotYR1MouseDown
      OnMouseUp = EditIPLInstRotZR2MouseUp
    end
    object EditIPLInstRotZL1: TPanel
      Left = 432
      Top = 110
      Width = 17
      Height = 17
      BevelOuter = bvNone
      Caption = '<<'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 35
      OnMouseDown = EditIPLInstRotZL1MouseDown
      OnMouseUp = EditIPLInstRotZR2MouseUp
    end
    object EditIPLInstRotZL2: TPanel
      Left = 448
      Top = 110
      Width = 9
      Height = 17
      BevelOuter = bvNone
      Caption = '<'
      TabOrder = 36
      OnMouseDown = EditIPLInstRotZL2MouseDown
      OnMouseUp = EditIPLInstRotZR2MouseUp
    end
    object EditIPLInstRotZR2: TPanel
      Left = 456
      Top = 110
      Width = 9
      Height = 17
      BevelOuter = bvNone
      Caption = '>'
      TabOrder = 37
      OnMouseDown = EditIPLInstRotZR2MouseDown
      OnMouseUp = EditIPLInstRotZR2MouseUp
    end
    object EditIPLInstRotZR1: TPanel
      Left = 464
      Top = 110
      Width = 17
      Height = 17
      BevelOuter = bvNone
      Caption = '>>'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 38
      OnMouseDown = EditIPLInstRotZR1MouseDown
      OnMouseUp = EditIPLInstRotZR2MouseUp
    end
    object EditIPLInstRotReset: TPanel
      Left = 392
      Top = 126
      Width = 89
      Height = 17
      BevelOuter = bvNone
      Caption = 'Reset'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 39
      OnClick = EditIPLInstRotResetClick
    end
    object EditIPLInstPosCenter: TPanel
      Left = 296
      Top = 38
      Width = 193
      Height = 17
      BevelOuter = bvNone
      Caption = 'Center On Object'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 40
      OnClick = EditIPLInstPosCenterClick
    end
  end
  object PanelIPLCull: TPanel
    Left = 0
    Top = 41
    Width = 492
    Height = 232
    Align = alClient
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    Visible = False
    object Label43: TLabel
      Left = 8
      Top = 8
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Vertex 1'
    end
    object Label44: TLabel
      Left = 296
      Top = 8
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Unknown'
    end
    object Label46: TLabel
      Left = 8
      Top = 56
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Vertex 2'
    end
    object Label47: TLabel
      Left = 8
      Top = 104
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Vertex 3'
    end
    object EditIPLCullPos1X: TEdit
      Left = 80
      Top = 8
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 0
    end
    object EditIPLCullPos1Y: TEdit
      Left = 144
      Top = 8
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 1
    end
    object EditIPLCullPos1Z: TEdit
      Left = 208
      Top = 8
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 2
    end
    object EditIPLCullU10: TEdit
      Left = 376
      Top = 8
      Width = 49
      Height = 17
      BorderStyle = bsNone
      TabOrder = 3
    end
    object EditIPLCullU11: TEdit
      Left = 424
      Top = 8
      Width = 49
      Height = 17
      BorderStyle = bsNone
      TabOrder = 4
    end
    object EditIPLCullPos2X: TEdit
      Left = 80
      Top = 56
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 5
    end
    object EditIPLCullPos2Y: TEdit
      Left = 144
      Top = 56
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 6
    end
    object EditIPLCullPos2Z: TEdit
      Left = 208
      Top = 56
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 7
    end
    object EditIPLCullPos3X: TEdit
      Left = 80
      Top = 104
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 8
    end
    object EditIPLCullPos3Y: TEdit
      Left = 144
      Top = 104
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 9
    end
    object EditIPLCullPos3Z: TEdit
      Left = 208
      Top = 104
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 10
    end
  end
  object PanelIPLZone: TPanel
    Left = 0
    Top = 41
    Width = 492
    Height = 232
    Align = alClient
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
    Visible = False
    object Label37: TLabel
      Left = 8
      Top = 8
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Zone Name'
    end
    object Label38: TLabel
      Left = 8
      Top = 32
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Type'
    end
    object Label39: TLabel
      Left = 8
      Top = 56
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Unknown'
    end
    object Label40: TLabel
      Left = 224
      Top = 8
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Position'
    end
    object Label41: TLabel
      Left = 224
      Top = 56
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Position'
    end
    object Label42: TLabel
      Left = 296
      Top = 34
      Width = 193
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = 'to'
    end
    object EditIPLZoneName: TEdit
      Left = 88
      Top = 8
      Width = 121
      Height = 17
      BorderStyle = bsNone
      TabOrder = 0
      OnChange = EditIPLZoneNameChange
    end
    object EditIPLZoneSort: TEdit
      Left = 88
      Top = 32
      Width = 121
      Height = 17
      BorderStyle = bsNone
      TabOrder = 1
      OnChange = EditIPLZoneSortChange
    end
    object EditIPLZoneU9: TEdit
      Left = 88
      Top = 56
      Width = 121
      Height = 17
      BorderStyle = bsNone
      TabOrder = 2
      OnChange = EditIPLZoneU9Change
    end
    object EditIPLZonePos1X: TEdit
      Left = 296
      Top = 8
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 3
      OnChange = EditIPLZonePos1XChange
    end
    object EditIPLZonePos1Y: TEdit
      Left = 360
      Top = 8
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 4
      OnChange = EditIPLZonePos1YChange
    end
    object EditIPLZonePos1Z: TEdit
      Left = 424
      Top = 8
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 5
      OnChange = EditIPLZonePos1ZChange
    end
    object EditIPLZonePos2X: TEdit
      Left = 296
      Top = 56
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 6
      OnChange = EditIPLZonePos2XChange
    end
    object EditIPLZonePos2Y: TEdit
      Left = 360
      Top = 56
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 7
      OnChange = EditIPLZonePos2YChange
    end
    object EditIPLZonePos2Z: TEdit
      Left = 424
      Top = 56
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 8
      OnChange = EditIPLZonePos2ZChange
    end
  end
  object PanelIPLPath: TPanel
    Left = 0
    Top = 41
    Width = 492
    Height = 232
    Align = alClient
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    Visible = False
    object Label64: TLabel
      Left = 8
      Top = 16
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Path Type'
    end
    object Label65: TLabel
      Left = 8
      Top = 56
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Unknown'
    end
    object EditIPLPathItems: TListView
      Left = 216
      Top = 8
      Width = 273
      Height = 153
      BorderStyle = bsNone
      Columns = <
        item
          Caption = 'Type'
        end
        item
          Caption = 'Connect'
          Width = 60
        end
        item
          AutoSize = True
          Caption = 'Position'
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnSelectItem = EditIPLPathItemsSelectItem
    end
    object EditIPLPathRadioPed: TRadioButton
      Left = 88
      Top = 16
      Width = 113
      Height = 17
      Caption = 'Pedestrian (0)'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = EditIPLPathRadioPedClick
    end
    object EditIPLPathRadioCar: TRadioButton
      Left = 88
      Top = 32
      Width = 113
      Height = 17
      Caption = 'Vehicle (1)'
      TabOrder = 2
      OnClick = EditIPLPathRadioCarClick
    end
    object EditIPLPathOther: TEdit
      Left = 88
      Top = 56
      Width = 121
      Height = 17
      BorderStyle = bsNone
      TabOrder = 3
      OnChange = EditIPLPathOtherChange
    end
    object PanelIPLPathItem: TPanel
      Left = 8
      Top = 168
      Width = 369
      Height = 57
      TabOrder = 4
      Visible = False
      object Label67: TLabel
        Left = 80
        Top = 34
        Width = 57
        Height = 13
        AutoSize = False
        Caption = 'Position'
      end
      object EditIPLPathItemLL0: TSpeedButton
        Left = 312
        Top = 8
        Width = 17
        Height = 17
        GroupIndex = 1
        Down = True
        Caption = '0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBtnText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = EditIDEPathItemLL0Click
      end
      object EditIPLPathItemLL1: TSpeedButton
        Left = 328
        Top = 8
        Width = 17
        Height = 17
        GroupIndex = 1
        Caption = '1'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBtnText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = EditIDEPathItemLL1Click
      end
      object EditIPLPathItemLL2: TSpeedButton
        Left = 344
        Top = 8
        Width = 17
        Height = 17
        GroupIndex = 1
        Caption = '2'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBtnText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = EditIDEPathItemLL2Click
      end
      object EditIPLPathItemLR0: TSpeedButton
        Left = 312
        Top = 32
        Width = 17
        Height = 17
        GroupIndex = 2
        Down = True
        Caption = '0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBtnText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = EditIDEPathItemLR0Click
      end
      object EditIPLPathItemLR1: TSpeedButton
        Left = 328
        Top = 32
        Width = 17
        Height = 17
        GroupIndex = 2
        Caption = '1'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBtnText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = EditIDEPathItemLR1Click
      end
      object EditIPLPathItemLR2: TSpeedButton
        Left = 344
        Top = 32
        Width = 17
        Height = 17
        GroupIndex = 2
        Caption = '2'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBtnText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = EditIDEPathItemLR2Click
      end
      object EditIPLPathItemTypeNone: TSpeedButton
        Left = 8
        Top = 8
        Width = 41
        Height = 17
        GroupIndex = 3
        Down = True
        Caption = 'None'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBtnText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = EditIPLPathItemTypeNoneClick
      end
      object EditIPLPathItemTypeMid: TSpeedButton
        Left = 48
        Top = 8
        Width = 33
        Height = 17
        GroupIndex = 3
        Caption = 'Mid'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBtnText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = EditIPLPathItemTypeMidClick
      end
      object EditIPLPathItemTypeEnd: TSpeedButton
        Left = 80
        Top = 8
        Width = 33
        Height = 17
        GroupIndex = 3
        Caption = 'End'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBtnText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = EditIPLPathItemTypeEndClick
      end
      object Label70: TLabel
        Left = 8
        Top = 34
        Width = 25
        Height = 13
        AutoSize = False
        Caption = 'To'
      end
      object EditIPLPathItemPosX: TEdit
        Left = 136
        Top = 32
        Width = 57
        Height = 17
        BorderStyle = bsNone
        TabOrder = 3
        OnChange = EditIPLPathItemPosXChange
      end
      object EditIPLPathItemPosY: TEdit
        Left = 192
        Top = 32
        Width = 57
        Height = 17
        BorderStyle = bsNone
        TabOrder = 4
        OnChange = EditIPLPathItemPosYChange
      end
      object EditIPLPathItemPosZ: TEdit
        Left = 248
        Top = 32
        Width = 57
        Height = 17
        BorderStyle = bsNone
        TabOrder = 5
        OnChange = EditIPLPathItemPosZChange
      end
      object EditIPLPathItemU3: TEdit
        Left = 120
        Top = 8
        Width = 41
        Height = 17
        BorderStyle = bsNone
        TabOrder = 0
        OnChange = EditIPLPathItemU3Change
      end
      object EditIPLPathItemU7: TEdit
        Left = 160
        Top = 8
        Width = 41
        Height = 17
        BorderStyle = bsNone
        TabOrder = 1
        OnChange = EditIPLPathItemU7Change
      end
      object EditIPLPathItemConnect: TEdit
        Left = 32
        Top = 32
        Width = 33
        Height = 17
        BorderStyle = bsNone
        TabOrder = 2
        OnChange = EditIPLPathItemConnectChange
      end
      object EditIPLPathItemU10: TEdit
        Left = 208
        Top = 8
        Width = 33
        Height = 17
        BorderStyle = bsNone
        TabOrder = 6
        OnChange = EditIPLPathItemU10Change
      end
      object EditIPLPathItemU11: TEdit
        Left = 240
        Top = 8
        Width = 33
        Height = 17
        BorderStyle = bsNone
        TabOrder = 7
        OnChange = EditIPLPathItemU11Change
      end
      object EditIPLPathItemU12: TEdit
        Left = 272
        Top = 8
        Width = 33
        Height = 17
        BorderStyle = bsNone
        TabOrder = 8
        OnChange = EditIPLPathItemU12Change
      end
    end
  end
  object PanelIPLMultInst: TPanel
    Left = 0
    Top = 41
    Width = 492
    Height = 232
    Align = alClient
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 12
    Visible = False
    object Label58: TLabel
      Left = 288
      Top = 168
      Width = 41
      Height = 13
      AutoSize = False
      Caption = 'Rot X'
    end
    object Label59: TLabel
      Left = 288
      Top = 184
      Width = 41
      Height = 13
      AutoSize = False
      Caption = 'Rot Y'
    end
    object Label60: TLabel
      Left = 288
      Top = 200
      Width = 41
      Height = 13
      AutoSize = False
      Caption = 'Rot Z'
    end
    object Label61: TLabel
      Left = 8
      Top = 184
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Move By'
    end
    object EditIPLMultInstItemCount: TLabel
      Left = 8
      Top = 216
      Width = 73
      Height = 13
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object EditIPLMultInstItemCenter: TLabel
      Left = 104
      Top = 168
      Width = 177
      Height = 13
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label57: TLabel
      Left = 8
      Top = 168
      Width = 97
      Height = 13
      AutoSize = False
      Caption = 'Average Center'
    end
    object EditIPLMultInstPosXL1: TPanel
      Left = 96
      Top = 200
      Width = 17
      Height = 17
      BevelOuter = bvNone
      Caption = '<<'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 4
      OnMouseDown = EditIPLMultInstPosXL1MouseDown
      OnMouseUp = EditIPLMultInstPosXL1MouseUp
    end
    object EditIPLMultInstPosXL2: TPanel
      Left = 112
      Top = 200
      Width = 9
      Height = 17
      BevelOuter = bvNone
      Caption = '<'
      TabOrder = 5
      OnMouseDown = EditIPLMultInstPosXL2MouseDown
      OnMouseUp = EditIPLMultInstPosXL1MouseUp
    end
    object EditIPLMultInstPosXR2: TPanel
      Left = 120
      Top = 200
      Width = 9
      Height = 17
      BevelOuter = bvNone
      Caption = '>'
      TabOrder = 6
      OnMouseDown = EditIPLMultInstPosXR2MouseDown
      OnMouseUp = EditIPLMultInstPosXL1MouseUp
    end
    object EditIPLMultInstPosXR1: TPanel
      Left = 128
      Top = 200
      Width = 17
      Height = 17
      BevelOuter = bvNone
      Caption = '>>'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 7
      OnMouseDown = EditIPLMultInstPosXR1MouseDown
      OnMouseUp = EditIPLMultInstPosXL1MouseUp
    end
    object EditIPLMultInstPosYL1: TPanel
      Left = 160
      Top = 200
      Width = 17
      Height = 17
      BevelOuter = bvNone
      Caption = '<<'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 8
      OnMouseDown = EditIPLMultInstPosYL1MouseDown
      OnMouseUp = EditIPLMultInstPosXL1MouseUp
    end
    object EditIPLMultInstPosYL2: TPanel
      Left = 176
      Top = 200
      Width = 9
      Height = 17
      BevelOuter = bvNone
      Caption = '<'
      TabOrder = 9
      OnMouseDown = EditIPLMultInstPosYL2MouseDown
      OnMouseUp = EditIPLMultInstPosXL1MouseUp
    end
    object EditIPLMultInstPosYR2: TPanel
      Left = 184
      Top = 200
      Width = 9
      Height = 17
      BevelOuter = bvNone
      Caption = '>'
      TabOrder = 10
      OnMouseDown = EditIPLMultInstPosYR2MouseDown
      OnMouseUp = EditIPLMultInstPosXL1MouseUp
    end
    object EditIPLMultInstPosYR1: TPanel
      Left = 192
      Top = 200
      Width = 17
      Height = 17
      BevelOuter = bvNone
      Caption = '>>'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 11
      OnMouseDown = EditIPLMultInstPosYR1MouseDown
      OnMouseUp = EditIPLMultInstPosXL1MouseUp
    end
    object EditIPLMultInstPosCenter: TPanel
      Left = 88
      Top = 214
      Width = 193
      Height = 17
      BevelOuter = bvNone
      Caption = 'Center On Objects'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 16
      OnClick = EditIPLMultInstPosCenterClick
    end
    object EditIPLMultInstPosZL1: TPanel
      Left = 224
      Top = 200
      Width = 17
      Height = 17
      BevelOuter = bvNone
      Caption = '<<'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 12
      OnMouseDown = EditIPLMultInstPosZL1MouseDown
      OnMouseUp = EditIPLMultInstPosXL1MouseUp
    end
    object EditIPLMultInstPosZL2: TPanel
      Left = 240
      Top = 200
      Width = 9
      Height = 17
      BevelOuter = bvNone
      Caption = '<'
      TabOrder = 13
      OnMouseDown = EditIPLMultInstPosZL2MouseDown
      OnMouseUp = EditIPLMultInstPosXL1MouseUp
    end
    object EditIPLMultInstPosZR2: TPanel
      Left = 248
      Top = 200
      Width = 9
      Height = 17
      BevelOuter = bvNone
      Caption = '>'
      TabOrder = 14
      OnMouseDown = EditIPLMultInstPosZR2MouseDown
      OnMouseUp = EditIPLMultInstPosXL1MouseUp
    end
    object EditIPLMultInstPosZR1: TPanel
      Left = 256
      Top = 200
      Width = 17
      Height = 17
      BevelOuter = bvNone
      Caption = '>>'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 15
      OnMouseDown = EditIPLMultInstPosZR1MouseDown
      OnMouseUp = EditIPLMultInstPosXL1MouseUp
    end
    object Panel14: TPanel
      Left = 328
      Top = 166
      Width = 17
      Height = 17
      BevelOuter = bvNone
      Caption = '<<'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 17
    end
    object Panel15: TPanel
      Left = 328
      Top = 182
      Width = 17
      Height = 17
      BevelOuter = bvNone
      Caption = '<<'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 18
    end
    object Panel16: TPanel
      Left = 328
      Top = 198
      Width = 17
      Height = 17
      BevelOuter = bvNone
      Caption = '<<'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 19
    end
    object Panel17: TPanel
      Left = 344
      Top = 198
      Width = 9
      Height = 17
      BevelOuter = bvNone
      Caption = '<'
      TabOrder = 20
    end
    object Panel18: TPanel
      Left = 344
      Top = 182
      Width = 9
      Height = 17
      BevelOuter = bvNone
      Caption = '<'
      TabOrder = 21
    end
    object Panel19: TPanel
      Left = 344
      Top = 166
      Width = 9
      Height = 17
      BevelOuter = bvNone
      Caption = '<'
      TabOrder = 22
    end
    object Panel20: TPanel
      Left = 352
      Top = 166
      Width = 9
      Height = 17
      BevelOuter = bvNone
      Caption = '>'
      TabOrder = 23
    end
    object Panel21: TPanel
      Left = 352
      Top = 182
      Width = 9
      Height = 17
      BevelOuter = bvNone
      Caption = '>'
      TabOrder = 24
    end
    object Panel22: TPanel
      Left = 352
      Top = 198
      Width = 9
      Height = 17
      BevelOuter = bvNone
      Caption = '>'
      TabOrder = 25
    end
    object Panel23: TPanel
      Left = 360
      Top = 198
      Width = 17
      Height = 17
      BevelOuter = bvNone
      Caption = '>>'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 26
    end
    object Panel24: TPanel
      Left = 360
      Top = 182
      Width = 17
      Height = 17
      BevelOuter = bvNone
      Caption = '>>'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 27
    end
    object Panel25: TPanel
      Left = 360
      Top = 166
      Width = 17
      Height = 17
      BevelOuter = bvNone
      Caption = '>>'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 28
    end
    object Panel26: TPanel
      Left = 288
      Top = 214
      Width = 89
      Height = 17
      BevelOuter = bvNone
      Caption = 'Reset'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 29
    end
    object EditIPLMultInstPosX: TEdit
      Left = 88
      Top = 184
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 1
      OnKeyDown = EditIPLMultInstPosXKeyDown
    end
    object EditIPLMultInstPosY: TEdit
      Left = 152
      Top = 184
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 2
      OnKeyDown = EditIPLMultInstPosYKeyDown
    end
    object EditIPLMultInstPosZ: TEdit
      Left = 216
      Top = 184
      Width = 65
      Height = 17
      BorderStyle = bsNone
      TabOrder = 3
      OnKeyDown = EditIPLMultInstPosZKeyDown
    end
    object EditIPLMultInstList: TListView
      Left = 8
      Top = 8
      Width = 476
      Height = 153
      BorderStyle = bsNone
      Columns = <
        item
          Caption = 'ID'
          Width = 100
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsList
    end
  end
  object PanelIDEObjs: TPanel
    Left = 0
    Top = 41
    Width = 492
    Height = 232
    Align = alClient
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 6
    Visible = False
    object Label23: TLabel
      Left = 8
      Top = 8
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'ID Number'
    end
    object Label24: TLabel
      Left = 8
      Top = 32
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Model Name'
    end
    object Label25: TLabel
      Left = 8
      Top = 56
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Texture Lib'
    end
    object Label26: TLabel
      Left = 256
      Top = 8
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Unknown'
    end
    object Label27: TLabel
      Left = 256
      Top = 32
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'LOD Value'
    end
    object Label28: TLabel
      Left = 256
      Top = 56
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Flags'
    end
    object EditIDEObjsID: TEdit
      Left = 88
      Top = 8
      Width = 121
      Height = 17
      BorderStyle = bsNone
      TabOrder = 0
      OnChange = EditIDEObjsIDChange
    end
    object EditIDEObjsModel: TEdit
      Left = 88
      Top = 32
      Width = 121
      Height = 17
      BorderStyle = bsNone
      TabOrder = 1
      OnChange = EditIDEObjsModelChange
    end
    object EditIDEObjsTexture: TEdit
      Left = 88
      Top = 56
      Width = 121
      Height = 17
      BorderStyle = bsNone
      TabOrder = 2
      OnChange = EditIDEObjsTextureChange
    end
    object EditIDEObjsU4: TEdit
      Left = 336
      Top = 8
      Width = 121
      Height = 17
      BorderStyle = bsNone
      TabOrder = 3
      OnChange = EditIDEObjsU4Change
    end
    object EditIDEObjsLOD: TEdit
      Left = 336
      Top = 32
      Width = 121
      Height = 17
      BorderStyle = bsNone
      TabOrder = 4
      OnChange = EditIDEObjsLODChange
    end
    object EditIDEObjsFlags: TEdit
      Left = 336
      Top = 56
      Width = 121
      Height = 17
      BorderStyle = bsNone
      TabOrder = 5
      OnChange = EditIDEObjsFlagsChange
    end
    object EditIDEObjsBoxCheck: TGroupBox
      Left = 8
      Top = 128
      Width = 289
      Height = 97
      Caption = 'Validation'
      TabOrder = 6
      object EditIDEObjsValidation: TListView
        Left = 8
        Top = 16
        Width = 273
        Height = 73
        BevelEdges = []
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Color = clBtnFace
        Columns = <
          item
            Width = 190
          end
          item
            Width = 80
          end>
        ColumnClick = False
        ReadOnly = True
        RowSelect = True
        ShowColumnHeaders = False
        SmallImages = FormMain.ImageList
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object BtnIDEObjsModel: TButton
      Left = 208
      Top = 32
      Width = 25
      Height = 17
      Caption = '...'
      TabOrder = 7
      OnClick = BtnIDEObjsModelClick
    end
    object BtnIDEObjsTexture: TButton
      Left = 208
      Top = 56
      Width = 25
      Height = 17
      Caption = '...'
      TabOrder = 8
      OnClick = BtnIDEObjsTextureClick
    end
    object EditIDEObjsP256: TCheckBox
      Left = 208
      Top = 112
      Width = 97
      Height = 17
      Caption = 'Unknown'
      TabOrder = 9
      OnClick = EditIDEObjsP256Click
    end
    object EditIDEObjsP128: TCheckBox
      Left = 208
      Top = 96
      Width = 97
      Height = 17
      Caption = 'Unknown'
      TabOrder = 10
      OnClick = EditIDEObjsP128Click
    end
    object EditIDEObjsP64: TCheckBox
      Left = 208
      Top = 80
      Width = 97
      Height = 17
      Caption = 'Unknown'
      TabOrder = 11
      OnClick = EditIDEObjsP64Click
    end
    object EditIDEObjsP8: TCheckBox
      Left = 104
      Top = 80
      Width = 97
      Height = 17
      Caption = 'Unknown'
      TabOrder = 12
      OnClick = EditIDEObjsP8Click
    end
    object EditIDEObjsP16: TCheckBox
      Left = 104
      Top = 96
      Width = 97
      Height = 17
      Caption = 'Unknown'
      TabOrder = 13
      OnClick = EditIDEObjsP16Click
    end
    object EditIDEObjsP32: TCheckBox
      Left = 104
      Top = 112
      Width = 97
      Height = 17
      Caption = 'Unknown'
      TabOrder = 14
      OnClick = EditIDEObjsP32Click
    end
    object EditIDEObjsP4: TCheckBox
      Left = 8
      Top = 112
      Width = 97
      Height = 17
      Caption = 'Has Alpha'
      TabOrder = 15
      OnClick = EditIDEObjsP4Click
    end
    object EditIDEObjsP2: TCheckBox
      Left = 8
      Top = 96
      Width = 97
      Height = 17
      Caption = 'Unknown'
      TabOrder = 16
      OnClick = EditIDEObjsP2Click
    end
    object EditIDEObjsP1: TCheckBox
      Left = 8
      Top = 80
      Width = 97
      Height = 17
      Caption = 'Gets Wet'
      TabOrder = 17
      OnClick = EditIDEObjsP1Click
    end
  end
  object PanelIDETObj: TPanel
    Left = 0
    Top = 41
    Width = 492
    Height = 232
    Align = alClient
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 7
    Visible = False
    object Label29: TLabel
      Left = 8
      Top = 8
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'ID Number'
    end
    object Label30: TLabel
      Left = 8
      Top = 32
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Model Name'
    end
    object Label31: TLabel
      Left = 8
      Top = 56
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Texture Lib'
    end
    object Label32: TLabel
      Left = 256
      Top = 56
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Flags'
    end
    object Label33: TLabel
      Left = 256
      Top = 32
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'LOD Value'
    end
    object Label34: TLabel
      Left = 256
      Top = 8
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Unknown'
    end
    object Label35: TLabel
      Left = 312
      Top = 80
      Width = 65
      Height = 13
      AutoSize = False
      Caption = 'Time On'
    end
    object Label36: TLabel
      Left = 312
      Top = 104
      Width = 65
      Height = 13
      AutoSize = False
      Caption = 'Time Off'
    end
    object EditIDETObjID: TEdit
      Left = 88
      Top = 8
      Width = 121
      Height = 17
      BorderStyle = bsNone
      TabOrder = 0
      OnChange = EditIDETObjIDChange
    end
    object EditIDETObjModel: TEdit
      Left = 88
      Top = 32
      Width = 121
      Height = 17
      BorderStyle = bsNone
      TabOrder = 1
      OnChange = EditIDETObjModelChange
    end
    object EditIDETObjTexture: TEdit
      Left = 88
      Top = 56
      Width = 121
      Height = 17
      BorderStyle = bsNone
      TabOrder = 3
      OnChange = EditIDETObjTextureChange
    end
    object EditIDETObjFlags: TEdit
      Left = 336
      Top = 56
      Width = 121
      Height = 17
      BorderStyle = bsNone
      ReadOnly = True
      TabOrder = 5
      OnChange = EditIDETObjFlagsChange
    end
    object EditIDETObjLOD: TEdit
      Left = 336
      Top = 32
      Width = 121
      Height = 17
      BorderStyle = bsNone
      TabOrder = 6
      OnChange = EditIDETObjLODChange
    end
    object EditIDETObjU4: TEdit
      Left = 336
      Top = 8
      Width = 121
      Height = 17
      BorderStyle = bsNone
      TabOrder = 7
      OnChange = EditIDETObjU4Change
    end
    object EditIDETObjTimeOn: TComboBox
      Left = 376
      Top = 80
      Width = 81
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 8
      OnChange = EditIDETObjTimeOnChange
      Items.Strings = (
        '00:00'
        '01:00'
        '02:00'
        '03:00'
        '04:00'
        '05:00'
        '06:00'
        '07:00'
        '08:00'
        '09:00'
        '10:00'
        '11:00'
        '12:00'
        '13:00'
        '14:00'
        '15:00'
        '16:00'
        '17:00'
        '18:00'
        '19:00'
        '20:00'
        '21:00'
        '22:00'
        '23:00')
    end
    object EditIDETObjTimeOff: TComboBox
      Left = 376
      Top = 104
      Width = 81
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 9
      OnChange = EditIDETObjTimeOffChange
      Items.Strings = (
        '00:00'
        '01:00'
        '02:00'
        '03:00'
        '04:00'
        '05:00'
        '06:00'
        '07:00'
        '08:00'
        '09:00'
        '10:00'
        '11:00'
        '12:00'
        '13:00'
        '14:00'
        '15:00'
        '16:00'
        '17:00'
        '18:00'
        '19:00'
        '20:00'
        '21:00'
        '22:00'
        '23:00')
    end
    object EditIDETObjBoxCheck: TGroupBox
      Left = 8
      Top = 128
      Width = 289
      Height = 97
      Caption = 'Validation'
      TabOrder = 10
      object EditIDETObjValidation: TListView
        Left = 8
        Top = 16
        Width = 273
        Height = 73
        BevelEdges = []
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Color = clBtnFace
        Columns = <
          item
            Width = 190
          end
          item
            Width = 80
          end>
        ColumnClick = False
        ReadOnly = True
        RowSelect = True
        ShowColumnHeaders = False
        SmallImages = FormMain.ImageList
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object BtnIDETObjModel: TButton
      Left = 208
      Top = 32
      Width = 25
      Height = 17
      Caption = '...'
      TabOrder = 2
      OnClick = BtnIDETObjModelClick
    end
    object BtnIDETObjTexture: TButton
      Left = 208
      Top = 56
      Width = 25
      Height = 17
      Caption = '...'
      TabOrder = 4
      OnClick = BtnIDETObjTextureClick
    end
    object EditIDETObjP1: TCheckBox
      Left = 8
      Top = 80
      Width = 97
      Height = 17
      Caption = 'Gets Wet'
      TabOrder = 11
      OnClick = EditIDETObjP1Click
    end
    object EditIDETObjP2: TCheckBox
      Left = 8
      Top = 96
      Width = 97
      Height = 17
      Caption = 'Unknown'
      TabOrder = 12
      OnClick = EditIDETObjP2Click
    end
    object EditIDETObjP4: TCheckBox
      Left = 8
      Top = 112
      Width = 97
      Height = 17
      Caption = 'Has Alpha'
      TabOrder = 13
      OnClick = EditIDETObjP4Click
    end
    object EditIDETObjP8: TCheckBox
      Left = 104
      Top = 80
      Width = 97
      Height = 17
      Caption = 'Unknown'
      TabOrder = 14
      OnClick = EditIDETObjP8Click
    end
    object EditIDETObjP16: TCheckBox
      Left = 104
      Top = 96
      Width = 97
      Height = 17
      Caption = 'Unknown'
      TabOrder = 15
      OnClick = EditIDETObjP16Click
    end
    object EditIDETObjP32: TCheckBox
      Left = 104
      Top = 112
      Width = 97
      Height = 17
      Caption = 'Unknown'
      TabOrder = 16
      OnClick = EditIDETObjP32Click
    end
    object EditIDETObjP64: TCheckBox
      Left = 208
      Top = 80
      Width = 97
      Height = 17
      Caption = 'Unknown'
      TabOrder = 17
      OnClick = EditIDETObjP64Click
    end
    object EditIDETObjP128: TCheckBox
      Left = 208
      Top = 96
      Width = 97
      Height = 17
      Caption = 'Unknown'
      TabOrder = 18
      OnClick = EditIDETObjP128Click
    end
    object EditIDETObjP256: TCheckBox
      Left = 208
      Top = 112
      Width = 97
      Height = 17
      Caption = 'Unknown'
      TabOrder = 19
      OnClick = EditIDETObjP256Click
    end
  end
  object InfoPanel: TPanel
    Left = 0
    Top = 0
    Width = 492
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      492
      41)
    object ModeLabel: TLabel
      Left = 0
      Top = 0
      Width = 41
      Height = 13
      AutoSize = False
      Caption = 'Mode:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
    end
    object ModeValue: TLabel
      Left = 40
      Top = 0
      Width = 73
      Height = 13
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object FileIndexLabel: TLabel
      Left = 121
      Top = 0
      Width = 41
      Height = 13
      AutoSize = False
      Caption = 'In File:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
    end
    object FileIndexValue: TLabel
      Left = 160
      Top = 0
      Width = 329
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object ExpandedEdit: TEdit
      Left = 0
      Top = 16
      Width = 491
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      TabOrder = 0
    end
  end
  object BtnUndo: TBitBtn
    Left = 384
    Top = 212
    Width = 105
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Undo'
    Enabled = False
    TabOrder = 10
    OnClick = BtnUndoClick
    Kind = bkRetry
  end
  object BtnCancel: TBitBtn
    Left = 384
    Top = 244
    Width = 105
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 11
    OnClick = BtnCancelClick
    Kind = bkCancel
  end
  object MoveTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = MoveTimerTimer
    Left = 464
    Top = 9
  end
end
