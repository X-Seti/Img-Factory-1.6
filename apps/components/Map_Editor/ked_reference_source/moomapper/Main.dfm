object FormMain: TFormMain
  Left = 398
  Top = 280
  HelpContext = 11
  AutoScroll = False
  Caption = 'Moo Mapper Beta 0.94'
  ClientHeight = 566
  ClientWidth = 843
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  OnMouseWheel = FormMouseWheel
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object MainToolbar: TToolBar
    Left = 0
    Top = 0
    Width = 843
    Height = 27
    ButtonWidth = 102
    Images = ImageList
    List = True
    ParentShowHint = False
    ShowCaptions = True
    ShowHint = True
    TabOrder = 0
    Transparent = False
    object TBViewEditor: TToolButton
      Left = 0
      Top = 2
      Action = ActionViewEditor
      AutoSize = True
      Caption = 'Item Editor'
    end
    object TBSep3: TToolButton
      Left = 84
      Top = 2
      Width = 3
      Caption = 'TBSep3'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object TBViewExtra: TToolButton
      Left = 87
      Top = 2
      Action = ActionViewExtra
      AutoSize = True
      Caption = 'Second Display'
    end
    object TBViewTexture: TToolButton
      Left = 190
      Top = 2
      Action = ActionViewTexture
      AutoSize = True
      Caption = 'Texture Display'
    end
    object TBSep1: TToolButton
      Left = 296
      Top = 2
      Width = 3
      Caption = 'TBSep1'
      ImageIndex = 23
      Style = tbsSeparator
    end
    object TBViewDAT: TToolButton
      Left = 299
      Top = 2
      Action = ActionViewDAT
      AutoSize = True
      Caption = 'DAT Editor'
    end
    object TBSep2: TToolButton
      Left = 381
      Top = 2
      Width = 3
      Caption = 'TBSep2'
      ImageIndex = 24
      Style = tbsSeparator
    end
    object TBValidateAll: TToolButton
      Left = 384
      Top = 2
      Hint = 'Validate All Files'
      Action = ActionValidateAll
      AutoSize = True
      Caption = 'City Tools'
    end
    object ToolButton1: TToolButton
      Left = 462
      Top = 2
      AutoSize = True
      Caption = 'Settings'
      ImageIndex = 28
      OnClick = ToolButton1Click
    end
    object LblFPS: TLabel
      Left = 532
      Top = 2
      Width = 109
      Height = 22
      Alignment = taCenter
      AutoSize = False
      Caption = 'FPS'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
    end
  end
  object DrawPanel: TPanel
    Left = 325
    Top = 46
    Width = 289
    Height = 441
    Align = alClient
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clBlack
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 27
    Width = 843
    Height = 19
    Align = alTop
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 2
    DesignSize = (
      841
      17)
    object BtnHideShow3DView: TPanel
      Left = 325
      Top = 0
      Width = 289
      Height = 16
      Align = alCustom
      Anchors = [akLeft, akTop, akRight]
      BevelOuter = bvNone
      Caption = 'Attach / Detach 3D View'
      Color = clBtnShadow
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = BtnHideShow3DViewClick
    end
    object BtnHideShowEditors: TPanel
      Left = 0
      Top = 0
      Width = 324
      Height = 16
      Align = alCustom
      BevelOuter = bvNone
      Caption = 'Hide / Show Editing Panel'
      Color = clBtnShadow
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = BtnHideShowEditorsClick
    end
    object BtnHideShowPanel: TPanel
      Left = 615
      Top = 0
      Width = 226
      Height = 16
      Anchors = [akTop, akRight]
      BevelOuter = bvNone
      Caption = 'Hide / Show Control Panel'
      Color = clBtnShadow
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = BtnHideShowPanelClick
    end
  end
  object PageControl1: TPageControl
    Left = 614
    Top = 46
    Width = 229
    Height = 441
    ActivePage = TabSheet2
    Align = alRight
    TabOrder = 3
    object TabSheet1: TTabSheet
      Caption = 'Navigation'
      object LblCurrentPos: TLabel
        Left = 3
        Top = 3
        Width = 109
        Height = 21
        Alignment = taCenter
        AutoSize = False
        Caption = 'Position (X, Y, Z)'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label1: TLabel
        Left = 3
        Top = 48
        Width = 38
        Height = 18
        Alignment = taCenter
        AutoSize = False
        Caption = 'Zoom'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object EditCurrentPos: TEdit
        Left = 3
        Top = 27
        Width = 141
        Height = 19
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 0
        Text = '0, 0, 0'
        OnKeyDown = EditCurrentPosKeyDown
      end
      object BtnMoveToPos: TButton
        Left = 147
        Top = 27
        Width = 73
        Height = 19
        Caption = 'Move There'
        TabOrder = 1
        OnClick = BtnMoveToPosClick
      end
      object ResetButton: TButton
        Left = 3
        Top = 70
        Width = 107
        Height = 23
        Caption = 'Reset View'
        TabOrder = 2
        OnClick = ResetButtonClick
      end
      object PanelZoomIn: TPanel
        Left = 46
        Top = 48
        Width = 17
        Height = 18
        BevelOuter = bvNone
        Caption = '+'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMaroon
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
        OnMouseDown = PanelZoomInMouseDown
        OnMouseUp = PanelZoomInMouseUp
      end
      object PanelZoomOut: TPanel
        Left = 65
        Top = 48
        Width = 17
        Height = 18
        BevelOuter = bvNone
        Caption = '-'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMaroon
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 4
        OnMouseDown = PanelZoomOutMouseDown
        OnMouseUp = PanelZoomOutMouseUp
      end
      object ChkMovement: TCheckBox
        Left = 86
        Top = 48
        Width = 109
        Height = 17
        Caption = 'First Person'
        TabOrder = 5
        OnClick = ChkMovementClick
      end
      object EditKey: TEdit
        Left = 197
        Top = 48
        Width = 23
        Height = 21
        ReadOnly = True
        TabOrder = 6
        Text = 'X X'
        Visible = False
        OnKeyDown = EditKeyKeyDown
        OnKeyUp = EditKeyKeyUp
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Files'
      ImageIndex = 2
      DesignSize = (
        221
        413)
      object Label11: TLabel
        Left = 0
        Top = 0
        Width = 221
        Height = 15
        Align = alTop
        AutoSize = False
        Caption = ' Visible Files:'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object ChkListVisible: TCheckListBox
        Left = -4
        Top = 16
        Width = 221
        Height = 380
        Hint = 'Double click to center on objects in file'
        OnClickCheck = ChkListVisibleClickCheck
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnDblClick = ChkListVisibleDblClick
      end
      object BtnVisCheckAll: TButton
        Left = 0
        Top = 396
        Width = 111
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'Select All'
        TabOrder = 1
        OnClick = BtnVisCheckAllClick
      end
      object BtnVisUnCheckAll: TButton
        Left = 110
        Top = 396
        Width = 111
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'De-Select All'
        TabOrder = 2
        OnClick = BtnVisUnCheckAllClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'debug'
      ImageIndex = 2
      DesignSize = (
        221
        413)
      object Label5: TLabel
        Left = 3
        Top = 262
        Width = 57
        Height = 11
        AutoSize = False
        Caption = 'Diffuse'
        Visible = False
      end
      object Label7: TLabel
        Left = 3
        Top = 284
        Width = 57
        Height = 13
        AutoSize = False
        Caption = 'Ambient'
        Visible = False
      end
      object Label8: TLabel
        Left = 3
        Top = 308
        Width = 57
        Height = 13
        AutoSize = False
        Caption = 'Specular'
        Visible = False
      end
      object Label9: TLabel
        Left = 3
        Top = 332
        Width = 57
        Height = 13
        AutoSize = False
        Caption = 'Distance'
        Visible = False
      end
      object Label17: TLabel
        Left = 0
        Top = 0
        Width = 221
        Height = 15
        Align = alTop
        AutoSize = False
        Caption = ' Errors and warnings:'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object SpecRGB: TTrackBar
        Left = 53
        Top = 308
        Width = 162
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Max = 255
        Frequency = 8
        Position = 255
        TabOrder = 0
        ThumbLength = 15
        Visible = False
        OnChange = DiffRGBChange
      end
      object Dis: TTrackBar
        Left = 53
        Top = 332
        Width = 162
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Max = 1000
        Frequency = 25
        Position = 700
        TabOrder = 1
        ThumbLength = 15
        Visible = False
        OnChange = DiffRGBChange
      end
      object DistCheck: TCheckBox
        Left = 3
        Top = 353
        Width = 57
        Height = 17
        Caption = 'Omni'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        Visible = False
        OnClick = DiffRGBChange
      end
      object MemoDebug: TMemo
        Left = 0
        Top = 15
        Width = 221
        Height = 398
        Align = alClient
        Font.Charset = OEM_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Terminal'
        Font.Style = []
        Lines.Strings = (
          'Debug Output'
          '===========')
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 3
        WordWrap = False
      end
      object AmbRGB: TTrackBar
        Left = 53
        Top = 284
        Width = 162
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Max = 255
        Frequency = 8
        Position = 255
        TabOrder = 4
        ThumbLength = 15
        Visible = False
        OnChange = DiffRGBChange
      end
      object DiffRGB: TTrackBar
        Left = 53
        Top = 260
        Width = 162
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Max = 255
        Frequency = 8
        Position = 255
        TabOrder = 5
        ThumbLength = 15
        Visible = False
        OnChange = DiffRGBChange
      end
      object Button1: TButton
        Left = 140
        Top = 1
        Width = 75
        Height = 25
        Caption = 'dump cols'
        TabOrder = 6
        OnClick = Button1Click
      end
    end
  end
  object DockPage: TPageControl
    Left = 0
    Top = 46
    Width = 325
    Height = 441
    Align = alLeft
    TabOrder = 4
  end
  object toolsswitch: TTabSet
    Left = 0
    Top = 545
    Width = 843
    Height = 21
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Tabs.Strings = (
      'Editing'
      '3D View')
    TabIndex = 1
    OnClick = toolsswitchClick
  end
  object tools: TNotebook
    Left = 0
    Top = 487
    Width = 843
    Height = 58
    Align = alBottom
    PageIndex = 1
    TabOrder = 6
    object TPage
      Left = 0
      Top = 0
      Caption = 'Editing'
      object StatusLabel: TLabel
        Left = 4
        Top = 5
        Width = 97
        Height = 13
        AutoSize = False
        Caption = 'Editing Status:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object StatusValue: TLabel
        Left = 8
        Top = 21
        Width = 97
        Height = 13
        AutoSize = False
        Caption = '<< Nothing >>'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label2: TLabel
        Left = 4
        Top = 37
        Width = 105
        Height = 13
        AutoSize = False
        Caption = 'Dragging Controls:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label3: TLabel
        Left = 226
        Top = 5
        Width = 57
        Height = 13
        AutoSize = False
        Caption = 'Left Btn:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object LabelControlLeftBtn: TLabel
        Left = 303
        Top = 5
        Width = 153
        Height = 13
        AutoSize = False
        Caption = 'Move Camera'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LabelCtrlShift: TLabel
        Left = 234
        Top = 37
        Width = 57
        Height = 13
        AutoSize = False
        Caption = '+ Ctrl + Shift'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label13: TLabel
        Left = 234
        Top = 21
        Width = 33
        Height = 13
        AutoSize = False
        Caption = '+ Ctrl:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object LabelControlLeftBtnCtrl: TLabel
        Left = 303
        Top = 21
        Width = 153
        Height = 13
        AutoSize = False
        Caption = 'Move Object (XY Plane)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LabelControlLeftBtnCtrlShift: TLabel
        Left = 303
        Top = 37
        Width = 153
        Height = 13
        AutoSize = False
        Caption = 'Move Object (Single Axis)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label4: TLabel
        Left = 463
        Top = 5
        Width = 73
        Height = 13
        AutoSize = False
        Caption = 'Right Btn:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label6: TLabel
        Left = 536
        Top = 5
        Width = 153
        Height = 13
        AutoSize = False
        Caption = 'Rotate Camera'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label10: TLabel
        Left = 463
        Top = 21
        Width = 73
        Height = 13
        AutoSize = False
        Caption = 'Double Click:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object LabelControlDoubleBtn: TLabel
        Left = 536
        Top = 21
        Width = 153
        Height = 13
        AutoSize = False
        Caption = 'Select Item && Zoom In'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RadioMovement: TRadioButton
        Left = 108
        Top = 21
        Width = 113
        Height = 17
        Caption = 'Mouse Movement'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = RadioMovementClick
      end
      object RadioRotation: TRadioButton
        Left = 108
        Top = 5
        Width = 113
        Height = 17
        Caption = 'Mouse Rotation'
        Enabled = False
        TabOrder = 1
        OnClick = RadioRotationClick
      end
      object MovePanel: TPanel
        Left = 120
        Top = 37
        Width = 97
        Height = 17
        BevelOuter = bvNone
        TabOrder = 2
        object RadioX: TRadioButton
          Left = 0
          Top = 0
          Width = 33
          Height = 17
          Caption = 'X'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          OnClick = RadioXClick
        end
        object RadioY: TRadioButton
          Left = 32
          Top = 0
          Width = 33
          Height = 17
          Caption = 'Y'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
          OnClick = RadioYClick
        end
        object RadioZ: TRadioButton
          Left = 64
          Top = 0
          Width = 33
          Height = 17
          Caption = 'Z'
          Checked = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 2
          TabStop = True
          OnClick = RadioZClick
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = '3D View'
      object Label15: TLabel
        Left = 115
        Top = 7
        Width = 109
        Height = 17
        Alignment = taCenter
        AutoSize = False
        Caption = 'Background color'
        Color = clBtnShadow
        Font.Charset = ANSI_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label12: TLabel
        Left = 230
        Top = 7
        Width = 61
        Height = 17
        Alignment = taCenter
        AutoSize = False
        Caption = 'Time:'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object GameTime: TComboBox
        Left = 230
        Top = 31
        Width = 61
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 12
        TabOrder = 6
        Text = '12:00'
        OnChange = GameTimeChange
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
      object ChkWireframe: TCheckBox
        Left = 4
        Top = 19
        Width = 109
        Height = 17
        Caption = 'Wireframe Mode'
        TabOrder = 0
        OnClick = ChkWireframeClick
      end
      object ChkTexture: TCheckBox
        Left = 4
        Top = 3
        Width = 109
        Height = 17
        Caption = 'Enable Textures'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = ChkTextureClick
      end
      object ChkAlpha: TCheckBox
        Left = 4
        Top = 35
        Width = 109
        Height = 17
        Caption = 'Alpha Blending'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = ChkAlphaClick
      end
      object BoxColour: TColorBox
        Left = 115
        Top = 31
        Width = 109
        Height = 22
        DefaultColorColor = clSilver
        Selected = clSilver
        Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 1
        OnChange = BoxColourChange
      end
      object ComboLod: TRadioGroup
        Left = 328
        Top = 0
        Width = 100
        Height = 58
        Caption = ' Mode '
        ItemIndex = 0
        Items.Strings = (
          'Normal Mode'
          'LOD Mode'
          'normal+lod'
          'COLL')
        TabOrder = 4
        OnClick = ComboLodClick
      end
      object GroupBox1: TGroupBox
        Left = 433
        Top = 0
        Width = 117
        Height = 58
        TabOrder = 5
        DesignSize = (
          117
          58)
        object Label14: TLabel
          Left = 8
          Top = 19
          Width = 100
          Height = 12
          Alignment = taCenter
          AutoSize = False
          Caption = 'Radar Z-level'
          Color = clBtnShadow
          Font.Charset = ANSI_CHARSET
          Font.Color = clWhite
          Font.Height = -9
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          Layout = tlCenter
        end
        object ChkBackgroundMap: TCheckBox
          Left = 8
          Top = 0
          Width = 74
          Height = 17
          Caption = 'Radar Map'
          TabOrder = 0
          OnClick = ChkBackgroundMapClick
        end
        object TrackBar1: TTrackBar
          Left = 8
          Top = 34
          Width = 100
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Max = 1000
          Min = -300
          Frequency = 8
          TabOrder = 1
          ThumbLength = 15
          TickStyle = tsNone
          OnChange = TrackBar1Change
          OnEnter = ComboLodEnter
        end
      end
      object GroupBox2: TGroupBox
        Left = 555
        Top = 0
        Width = 174
        Height = 58
        Caption = ' Projection '
        TabOrder = 7
        DesignSize = (
          174
          58)
        object Label16: TLabel
          Left = 8
          Top = 19
          Width = 158
          Height = 12
          Alignment = taCenter
          AutoSize = False
          Caption = '<< orthogonal | Perspective >>'
          Color = clBtnShadow
          Font.Charset = ANSI_CHARSET
          Font.Color = clWhite
          Font.Height = -9
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          Layout = tlCenter
        end
        object TrackBar2: TTrackBar
          Left = 8
          Top = 34
          Width = 158
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Max = 100
          Min = 1
          Frequency = 8
          Position = 45
          TabOrder = 0
          ThumbLength = 15
          TickStyle = tsNone
          OnChange = TrackBar2Change
          OnEnter = ComboLodEnter
        end
        object BitBtn2: TBitBtn
          Left = 69
          Top = 0
          Width = 90
          Height = 15
          Caption = 'RESET'
          TabOrder = 1
          OnClick = BitBtn2Click
        end
      end
    end
  end
  object MainActionList: TActionList
    Images = ImageList
    Left = 8
    Top = 80
    object ActionViewExtra: TAction
      Category = 'View'
      Caption = 'Second &Display'
      Hint = 'View Secondary Display'
      ImageIndex = 12
      ShortCut = 32817
      OnExecute = ActionViewExtraExecute
    end
    object ActionViewIPL: TAction
      Category = 'View'
      Caption = 'View Item Instance Editor'
      Hint = 'View Item Instance Editor'
      OnExecute = ActionViewIPLExecute
    end
    object ActionViewArchive: TAction
      Category = 'View'
      Caption = 'View Archive Editor'
      Hint = 'View Archive Editor'
      OnExecute = ActionViewArchiveExecute
    end
    object ActionViewIDE: TAction
      Category = 'View'
      Caption = 'View Object Definition Editor'
      Hint = 'View Object Definition Editor'
      OnExecute = ActionViewIDEExecute
    end
    object ActionViewEditor: TAction
      Category = 'View'
      Caption = 'Item &Editor'
      Hint = 'View Item Editor'
      ImageIndex = 10
      ShortCut = 32816
      OnExecute = ActionViewEditorExecute
    end
    object ActionViewTexture: TAction
      Category = 'View'
      Caption = '&Texture Display'
      Hint = 'View Texture Display'
      ImageIndex = 23
      ShortCut = 32818
      OnExecute = ActionViewTextureExecute
    end
    object ActionSelectGame: TAction
      Category = 'File'
      Caption = 'Select Game...'
      OnExecute = ActionSelectGameExecute
    end
    object ActionSaveModified: TAction
      Category = 'File'
      Caption = '&Save Modified Files'
      Enabled = False
      Hint = 'Save Modified Files'
      ImageIndex = 8
      ShortCut = 16467
      OnExecute = ActionSaveModifiedExecute
    end
    object ActionHelpAbout: TAction
      Category = 'Help'
      Caption = '&About '
      Hint = 'About'
      ShortCut = 113
      OnExecute = ActionHelpAboutExecute
    end
    object ActionValidateAll: TAction
      Category = 'File'
      Caption = '&Validate All Files'
      Hint = 'Validate All'
      ImageIndex = 21
      ShortCut = 16454
      OnExecute = ActionValidateAllExecute
    end
    object ActionFileExit: TFileExit
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit Program'
      ImageIndex = 17
      ShortCut = 32883
    end
    object ActionViewDAT: TAction
      Category = 'View'
      Caption = '&DAT Editor'
      Hint = 'View DAT Editor'
      ImageIndex = 18
      ShortCut = 32819
      OnExecute = ActionViewDATExecute
    end
  end
  object MainMenu: TMainMenu
    Images = ImageList
    Left = 40
    Top = 80
    object FileMenu: TMenuItem
      Caption = '&File'
      object FileSelectGame: TMenuItem
        Action = ActionSelectGame
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object FileValidateAll: TMenuItem
        Action = ActionValidateAll
      end
      object FileSaveItem: TMenuItem
        Action = ActionSaveModified
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object FileExitItem: TMenuItem
        Action = ActionFileExit
      end
    end
    object ViewMenu: TMenuItem
      Caption = '&View'
      object ViewEditorItem: TMenuItem
        Action = ActionViewEditor
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object ViewExtraItem: TMenuItem
        Action = ActionViewExtra
      end
      object ViewTextureItem: TMenuItem
        Action = ActionViewTexture
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object DATEditor1: TMenuItem
        Action = ActionViewDAT
      end
    end
    object HelpMenu: TMenuItem
      Caption = '&Help'
      object HelpHelpItem: TMenuItem
        Caption = 'Mapper &Help'
        OnClick = HelpHelpItemClick
      end
      object HelpAboutItem: TMenuItem
        Action = ActionHelpAbout
      end
    end
  end
  object ImageList: TImageList
    Left = 72
    Top = 80
    Bitmap = {
      494C01011D002200040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000009000000001002000000000000090
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000004040400040404000404
      0400040404000404040004040400040404000404040004040400040404000404
      0400040404000404040004040400040404000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B2B2B20096969600969696009696
      96009696960094949400939393008F8F8F008B8B8B0087878700838383007F7F
      7F007B7B7B007777770077777700040404000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B2B2B200CFFFFF00CEFEFE00CAFC
      FC00C5F7F700BFF1F100B9ECEC00B2E4E400A9DADA009FCECE009AC6C70096BE
      BF0090B1B200829FA0007A7A7A00040404000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B2B2B200D5FFFF00D3FFFF00CEFF
      FF00CAFDFD00C7FAFA00C2F5F500BCEEEE00B4E5E600ACDCDC00A8D6D700A4CE
      CF009CBEBF008CACAD007E7E7E00040404000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B2B2B200E0FFFF00DBFFFF00D2FF
      FF00CCFEFE00C8FBFB00C3F6F600BDF0F000B8EAEA00B2E4E400AEDFE000A8D6
      D7009EC5C6008EB3B40082828200040404000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B2B2B200EBFFFF00E4FFFF00D9FF
      FF00D1FFFF00CDFEFE00C9FCFC00C5F8F800BFF2F200B7EAEA00B1E2E300AAD9
      DA00A1CBCC0090B8B90086868600040404000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B2B2B200F3FFFF00EDFFFF00E2FF
      FF00D8FFFF00D2FFFF00CDFFFF00CAFCFC00C4F6F600BCEEEF00B5E7E700B0E0
      E000A6D2D30091BBBC008A8A8A00040404000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B2B2B200F9FFFF00F5FFFF00ECFF
      FF00E1FFFF00D8FFFF00D1FFFF00CEFFFF00CBFDFD00C6F9F900C0F2F300B9EA
      EA00AEDCDD0092BEBF0090909000040404000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B2B2B200FDFFFF00FAFFFF00F3FF
      FF00EAFFFF00E0FFFF00D7FFFF00D1FFFF00CEFFFF00CBFDFD00C5F8F800BFF1
      F100B4E4E40095C2C30095959500040404000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B2B2B200FFFFFF00FDFFFF00F9FF
      FF00F2FFFF00E9FFFF00E1FFFF00D8FFFF00D2FFFF00CEFFFF00C9FCFC00C5F7
      F700BBEDED0097C6C70096969600040404000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B2B2B200FFFFFF00FFFFFF00FBFF
      FF00F5FFFF00EDFFFF00E5FFFF00DBFFFF00D3FFFF00CFFFFF00CDFFFF00CCFE
      FF00C2F5F5009ECECE0096969600040404000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CC990000B3800000A67300009F6C
      00009C6900009B6800009A670000996600009966000099660000996600009966
      0000996600009966000099660000040404000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFCC6600E6B33300D9A61A00D29F
      0D00CE9B0500CC990100CC990000CC990000CC990000CB980000CA970000C693
      0000BF8C0000B27F000099660000040404000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFCC6600E6B33300D9A61A00D29F
      0D00CE9B0600CC990100CC990000CC990000CC990000CC990000CB980000C693
      0000BF8C0000B27F000099660000040404000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF9900FFE68000FFD97300FFD2
      6C00FFCE6800FFCC6600FFCC6600FFCC6600FFCC6600FFCC6600FECB6400F9C6
      5900F2BF4C00E5B23300CC990000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000B9000000B9000000B9000000B9000000B90000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000000000000000000000000000000000000000000000000000000000B9
      000000B9000000B9000000B9000000B9000000B9000000B9000000B9000000B9
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF00FFFFFF000000FF000000FF000000FF00FFFFFF000000FF000000
      FF000000000000000000000000000000000000000000000000000000000000B9
      000000B9000000B90000FFFFFF00FFFFFF0000B9000000B9000000B9000000B9
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00000000000000000000000000FFFFFF00000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF00FFFFFF00FFFFFF00FFFFFF000000FF00FFFFFF00FFFFFF00FFFFFF000000
      FF000000FF00000000000000000000000000000000000000000000B9000000B9
      000000B90000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000B9000000B9000000B9
      000000B900000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000FF000000
      FF000000FF00000000000000000000000000000000000000000000B9000000B9
      0000FFFFFF00FFFFFF0000B90000FFFFFF00FFFFFF0000B9000000B9000000B9
      000000B900000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF00FFFFFF00FFFFFF00FFFFFF000000FF000000FF000000
      FF000000FF00000000000000000000000000000000000000000000B9000000B9
      000000B9000000B9000000B9000000B90000FFFFFF00FFFFFF0000B9000000B9
      000000B900000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000FF000000
      FF000000FF00000000000000000000000000000000000000000000B9000000B9
      000000B9000000B9000000B9000000B90000FFFFFF00FFFFFF0000B9000000B9
      000000B900000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00000000000000000000000000FFFFFF00000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF00FFFFFF00FFFFFF00FFFFFF000000FF00FFFFFF00FFFFFF00FFFFFF000000
      FF000000FF00000000000000000000000000000000000000000000B9000000B9
      000000B9000000B9000000B9000000B9000000B90000FFFFFF00FFFFFF0000B9
      000000B900000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF00FFFFFF000000FF000000FF000000FF00FFFFFF000000FF000000
      FF000000000000000000000000000000000000000000000000000000000000B9
      000000B9000000B9000000B9000000B9000000B90000FFFFFF00FFFFFF0000B9
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000000000000000000000000000000000000000000000000000000000B9
      000000B9000000B9000000B9000000B9000000B9000000B9000000B9000000B9
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000B9000000B9000000B9000000B9000000B90000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000525252005454540056565600575757005A5A5A005D5D5D005E5E5E006060
      6000626262006464640066666600000000000000000000000000000000000000
      0000525252005454540056565600575757005A5A5A005D5D5D005E5E5E006060
      6000626262006464640066666600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000808000008080000080
      8000008080000080800000808000008080000080800000808000008080000080
      8000008080000000000000000000000000000000000000000000000000000000
      00004E4E4E00E5E5E500E4E4E400E1E1E100DEDEDE00DDDDDD00DADADA00D8D8
      D800D6D6D600D4D4D40063636300000000000000000000000000000000000000
      00004E4E4E00E5E5E500E4E4E400E1E1E100DEDEDE00DDDDDD00DADADA00D8D8
      D800D6D6D600D4D4D400636363000000000000000000943C3C00943C3C000000
      000000000000FCFC0800FCFC0800000000000000000040948000409480000000
      0000000000000078FC000078FC00000000000000000000000000C0C0C0000080
      8000008080000080800000808000C0C0C0000080800000808000008080000080
      8000C0C0C0000000000000000000000000000000000000000000000000000000
      00004C4C4C00EAEAEA00E7E7E700E5E5E500E2E2E200E0E0E000DEDEDE00DBDB
      DB00DADADA00D7D7D70060606000000000000000000000000000000000000000
      00004C4C4C00EAEAEA00E7E7E700E5E5E500E2E2E200E0E0E000DEDEDE00DBDB
      DB00DADADA00D7D7D700606060000000000000000000943C3C00943C3C00FCE0
      B80000000000FCFC0800FCFC0800FCC87C00000000004094800040948000FCB0
      4000000000000078FC000078FC00FC9C00000000000000000000C0C0C0008000
      0000800000008000000080000000C0C0C0008000000080000000800000008000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      000048484800ECECEC0080808000E8E8E800808080008080800080808000E0E0
      E000DDDDDD00DADADA005D5D5D00000000000000000000000000000000000000
      000048484800ECECEC000000FF00E8E8E8000000FF000000FF000000FF00E0E0
      E000DDDDDD00DADADA005D5D5D00000000000000000000000000FCE0B800FCE0
      B8000000000000000000FCC87C00FCC87C000000000000000000FCB04000FCB0
      40000000000000000000FC9C0000FC9C00000000000000000000C0C0C0008000
      0000800000008000000080000000C0C0C0008000000080000000800000008000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      000046464600F0F0F000EEEEEE00EBEBEB00EAEAEA00E7E7E700E5E5E500E2E2
      E200E0E0E000DEDEDE005A5A5A00000000000000000000000000000000000000
      000046464600F0F0F000EEEEEE00EBEBEB00EAEAEA00E7E7E700E5E5E500E2E2
      E200E0E0E000DEDEDE005A5A5A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C0008000
      0000800000008000000080000000C0C0C0008000000080000000FFFFFF008000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      000042424200F3F3F30080808000EFEFEF008080800080808000808080008080
      800080808000E1E1E10056565600000000000000000000000000000000000000
      000042424200F3F3F3000000FF00EFEFEF000000FF000000FF000000FF000000
      FF000000FF00E1E1E100565656000000000000000000B45C5C00B45C5C000000
      000000000000FCFC4000FCFC4000000000000000000064B0A40064B0A4000000
      0000000000004098FC004098FC00000000000000000000000000C0C0C0000000
      0000000000000000000000000000C0C0C0000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      000040404000F6F6F600F4F4F400F1F1F100F0F0F000EEEEEE00ECECEC00EAEA
      EA00E7E7E700E5E5E50054545400000000000000000000000000000000000000
      000040404000F6F6F600F4F4F400F1F1F100F0F0F000EEEEEE00ECECEC00EAEA
      EA00E7E7E700E5E5E500545454000000000000000000B45C5C00B45C5C00B8FC
      BC0000000000FCFC4000FCFC40007CFC80000000000064B0A40064B0A40040FC
      4000000000004098FC004098FC0000FC08000000000000000000C0C0C0000080
      8000008080000080800000808000C0C0C0000080800000808000008080000080
      8000C0C0C0000000000000000000000000000000000000000000000000000000
      00003C3C3C00F9F9F90080808000F5F5F5008080800080808000808080008080
      8000EBEBEB00E8E8E80050505000000000000000000000000000000000000000
      00003C3C3C00F9F9F9000000FF00F5F5F5000000FF000000FF000000FF000000
      FF00EBEBEB00E8E8E80050505000000000000000000000000000B8FCBC00B8FC
      BC0000000000000000007CFC80007CFC8000000000000000000040FC400040FC
      4000000000000000000000FC080000FC08000000000000000000C0C0C0008000
      00008000000000FFFF0080808000C0C0C0008000000080808000800000008000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      00003A3A3A00FBFBFB00FAFAFA00F8F8F800F6F6F600F4F4F400F1F1F100F0F0
      F000EEEEEE00ECECEC004D4D4D00000000000000000000000000000000000000
      00003A3A3A00FBFBFB00FAFAFA00F8F8F800F6F6F600F4F4F400F1F1F100F0F0
      F000EEEEEE00ECECEC004D4D4D00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C0008000
      000000FFFF0000FFFF0080000000C0C0C000800000008000000000FFFF008000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      000037373700FFFFFF00FDFDFD00FBFBFB00F9F9F900F6F6F600F5F5F500F3F3
      F300F1F1F100EFEFEF0049494900000000000000000000000000000000000000
      000037373700FFFFFF00FDFDFD00FBFBFB00F9F9F900F6F6F600F5F5F500F3F3
      F300F1F1F100EFEFEF00494949000000000000000000D0808000D08080000000
      000000000000F8FC7C00F8FC7C0000000000000000008CD0C4008CD0C4000000
      000000000000DCE8FC00DCE8FC00000000000000000000000000C0C0C0008000
      000000FFFF008080800080000000C0C0C0008000000080000000800000008000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FDFDFD00000000000000000000000000F5F5
      F500F3F3F300F3F3F30047474700000000000000000000000000000000000000
      0000000000000000000000000000FDFDFD00000000000000000000000000F5F5
      F500F3F3F300F3F3F300474747000000000000000000D0808000D0808000B8FC
      FC0000000000F8FC7C00F8FC7C007CFCFC00000000008CD0C4008CD0C40040F4
      FC0000000000DCE8FC00DCE8FC0000F4FC000000000000000000C0C0C0000000
      0000800000008080800080000000C0C0C0008000000080000000800000000080
      8000C0C0C0000000000000000000000000008080800000000000000000000000
      0000ACACAC00A0A0A0007E7E7E0000000000ACACAC00A0A0A0007E7E7E000000
      0000F5F5F500F5F5F50044444400000000008080800000000000000000000000
      0000ACACAC00A0A0A0007E7E7E0000000000ACACAC00A0A0A0007E7E7E000000
      0000F5F5F500F5F5F50044444400000000000000000000000000B8FCFC00B8FC
      FC0000000000000000007CFCFC007CFCFC00000000000000000040F4FC0040F4
      FC00000000000000000000F4FC0000F4FC000000000000000000008080000000
      00008000000000FFFF0080000000C0C0C0008000000080000000800000000080
      8000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FBFBFB00A0A0A00000000000FFFFFF00FBFBFB00A0A0A0000000
      0000F8F8F800F8F8F80041414100000000000000000000000000000000000000
      0000FFFFFF00FBFBFB00A0A0A00000000000FFFFFF00FBFBFB00A0A0A0000000
      0000F8F8F800F8F8F80041414100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C0C0
      C000800000008000000080000000C0C0C000800000008000000080000000C0C0
      C000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00ACACAC0000000000FFFFFF00FFFFFF00ACACAC000000
      00003C3C3C003C3C3C003E3E3E00000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00ACACAC0000000000FFFFFF00FFFFFF00ACACAC000000
      00003C3C3C003C3C3C003E3E3E000000000000000000ECACAC00ECACAC000000
      000000000000FCFCB800FCFCB8000000000000000000BCECE400BCECE4000000
      000000000000A8BCD400A8BCD400000000000000000000000000000000000000
      0000C0C0C0000000000080000000C0C0C0008000000000808000C0C0C0000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000008080
      80000000000000000000000000000000000000000000ECACAC00ECACAC00C0C0
      FC0000000000FCFCB800FCFCB8007C7CFC0000000000BCECE400BCECE4004040
      FC0000000000A8BCD400A8BCD4000008FC000000000000000000000000000000
      00000000000000808000C0C0C000C0C0C000C0C0C00000000000000000000000
      0000000000000000000000000000000000008080800000000000808080000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000008080800000000000808080000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000C0C0FC00C0C0
      FC0000000000000000007C7CFC007C7CFC0000000000000000004040FC004040
      FC0000000000000000000008FC000008FC00FFFFFF0000000000000000000000
      0000000000000000000000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000006F6F6F0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000FFFFFF000000000000000000000000000000
      00000000000000000000FFFFFF00000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000088888800B6B6B600A2A2A2009B9B
      9B009B9B9B009B9B9B009B9B9B00949494007C7C7C0000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF000000FF000000FF000000800000008000000080000000800000008000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000096969600FFFFFF00D0F7F700CEF7
      F700CCF7F700C8F7F700C6F7F700C4F7F7008B8B8B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FF000000FF00000080000000800000000000000000000000800000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      000000000000FFFFFF0000000000FFFFFF009E9E9E00FFFFFF00D7F7F700D3F7
      F700D0F7F700CEF7F700CAF7F700C8F7F7009292920000000000949494007C7C
      7C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C0C0C000C0C0C00080808000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF0000000000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C0000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      000000000000FFFFFF0000000000FFFFFF009E9E9E00FFFFFF00DBF7F700D9F7
      F700D5F7F700D3F7F700CEF7F700CCF7F7009292920000000000C4F7F7008B8B
      8B00000000000000000000000000000000000000000000000000000000000000
      00000000000080808000FFFFFF00FFFFFF00FFFFFF0080808000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF0000000000CACACA00CACACA00CACACA00CACACA00CACACA00CACACA000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      000000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      000000000000FFFFFF0000000000FFFFFF009E9E9E00FFFFFF00DFF7F700DDF7
      F700DBF7F700D7F7F700D5F7F700D0F7F7009292920000000000C8F7F7009292
      920000000000949494007C7C7C00000000000000000000000000000000000000
      00000000000000000000FFFFFF00000000008080800000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF0000000000D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D5000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      000000000000FFFFFF0000000000FFFFFF009E9E9E00FFFFFF00E6F7F700E2F7
      F700DFF7F700DBF7F700D9F7F700D7F7F7009898980000000000CCF7F7009292
      920000000000C4F7F7008B8B8B00000000000000000000000000000000000000
      0000000000000000000000000000FFFF00000000000000000000FFFFFF000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF0000000000DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFFFF000000
      000000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      000000000000FFFFFF0000000000FFFFFF009E9E9E00FFFFFF00EAF7F700E8F7
      F700E4F7F700E2F7F700DDF7F700D9F5F5009E9E9E0000000000D0F7F7009292
      920000000000C8F7F70092929200000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      800000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000E7E7E700E7E7E700000000000000000000000000000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      000000000000FFFFFF0000000000FFFFFF009E9E9E00FFFFFF00EEF7F700ECF7
      F700E8F7F700E6F7F70000000000000000000000000000000000D7F7F7009898
      980000000000CCF7F70092929200000000000000000000000000000000000000
      0000000000000000000000000000FFFF00000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF0000000000EFEFEF00EFEFEF0000000000C0C0C000D4D4D400E4E4E4000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      000000000000FFFFFF0000000000FFFFFF00A1A1A100FFFFFF00F5F7F700F1F7
      F700F4FDFD00EAF7F70082828200E1E1E10000000000DDF7F700D9F5F5009E9E
      9E0000000000D0F7F70092929200000000000000000000000000000000000000
      0000000000000000000000000000FFFF0000000000000000000000000000C0C0
      C00000000000000000000000000000000000000000000000000000000000FFFF
      FF0000000000F7F7F700F7F7F70000000000D4D4D400E4E4E40000000000FFFF
      FF00FFFFFF000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      000000000000FFFFFF0000000000FFFFFF00B2B2B200FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF008484840000000000E6F7F70000000000000000000000
      000000000000D7F7F70098989800000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF0000000000FFFFFF00FFFFFF0000000000E4E4E40000000000FFFFFF00FFFF
      FF0000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      000000000000FFFFFF0000000000FFFFFF00BDBDBD00B2B2B200A8A8A800A1A1
      A1009B9B9B00939393007E7E7E00F4FDFD00EAF7F70082828200E1E1E1000000
      0000DDF7F700D9F5F5009E9E9E00000000000000000000000000000000000000
      00000000000000000000FFFF000000000000FFFF000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF000000000000000000000000000000000000000000FFFFFF00FFFFFF000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      000000000000FFFFFF0000000000FFFFFF00000000000000000000000000B2B2
      B200FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008484840000000000E6F7
      F700000000000000000000000000000000000000000000000000000000000000
      0000008080000000000000000000FFFF00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      000000000000FFFFFF0000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF00000000000000000000000000BDBD
      BD00B2B2B200A8A8A800A1A1A1009B9B9B00939393007E7E7E00F4FDFD00EAF7
      F70082828200E1E1E10000000000000000000000000000000000000000000080
      8000000000000080800000000000000000000000000000808000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      000000000000000000000000000000000000FFFFFF0000000000000000000000
      00000000000000000000FFFFFF00000000000000000000000000000000000000
      000000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF000000000000000000000000000000
      00000000000000000000B2B2B200FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000808000000000000000000000808000000000000000
      000000000000000000000000000000000000FFFFFF0000000000000000000000
      000000000000000000000000000000000000FFFFFF0000000000000000000000
      0000000000000000000000000000FFFFFF000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      00000000000000000000BDBDBD00B2B2B200A8A8A800A1A1A1009B9B9B009393
      93007E7E7E000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000023AEF1001FB9F40000000000000000000000000000000000000000000000
      00000000000000000000000000000000000024B5F3001EC1F600000000000000
      0000000000000000000000000000000000008282820099999900919191008989
      890081818100797979006F6F6F00666666005D5D5D00545454004B4B4B004242
      42003A3A3A00323232002A2A2A00222222000000000000000000000000000000
      0000000000000300030003000300030003000300030003000300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000023AE
      F1001FB9F4001CC2F60000000100000000000000000000000000000000000000
      000000000000000000000000000024B5F3001EC1F60019CEF900000000000000
      00000000000000000000000000000000000082828200E8E8E800FCFCFC00F3F3
      F300EAEAEA00E1E1E100D8D8D800CDCDCD00C3C3C300B9B9B900B0B0B000A6A6
      A6009C9C9C009393930089898900222222000000000000000000000000004A1C
      4A00431A43007C8C890021AC330004EC040004EC040007CA14003B1B4600441D
      4C00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000023AEF1001FB9
      F4001CC2F6000000510000000000000000000000000000000000000000000000
      0000000000000000000024B5F3001EC1F60019CEF90000000000000000000000
      00000000000000000000000000000000000082828200E8E8E800FCFCFC00F3F3
      F300EAEAEA00E1E1E100D8D8D800CDCDCD00C3C3C300B9B9B900B0B0B000A6A6
      A6009C9C9C0093939300898989002222220000000000000000004B1C4B009889
      9800B5BCB500B3BDB30064FE8D0004FF280004FF060003FE060004FF06000ABE
      2200441C4C000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000023AEF1001FB9F4001CC2
      F600000054000000000000000000000000000000000086868600DFBF6300E1C3
      6800E1C36800E0C264000000000019CEF9000000000000000000000000000000
      00001717470000000000000000000000000082828200E8E8E80000D2000000D2
      0000EAEAEA00E1E1E100D8D8D800CDCDCD00C3C3C300B9B9B900B0B0B000A6A6
      A6009C9C9C00939393008989890022222200000000004B1B4B00A49AA400DACA
      CE00E5DBDC00F4FEEB00CDFFE80006FF710004FF180003FF110003FF250003FF
      2C001AAB3200461C4C0000000000000000000000000000000000868686000000
      0000D2A62100D2A51E00D5A92900D6AC3000D7AF370000000C001CC2F6000000
      54000000000000000000000000000000000086868600DDBE5800E7C47100E4C9
      7B00E3C56E00E0C16100DFBF5800000000000000000000000000000000002A2A
      7E000000890000000000000000000000000082828200E8E8E800FCFCFC00F3F3
      F300EAEAEA00E1E1E100D8D8D800CDCDCD00C3C3C300B9B9B900B0B0B000A6A6
      A6009C9C9C0093939300898989002222220000000000471C47009E9B9E00DCC9
      D000CFCCCF00E9ECE100FAFFF10029EE6F0006DC240003FF180004FF390005FF
      500006F44400471C470000000000000000000000000086868600D0A11300D0A0
      1100D0A01200D0A01100D2A41C00D4A82600D6AD3200D8B13C0000000C000000
      00000000000000000000000000000000000086868600E3C57000EAD28E00EBD5
      9700E5C97700DFC06000DDBB53000000000000000000000000003838A8000202
      FF000000C00024246D001D1D5700000000008282820099999900919191008989
      890081818100797979006F6F6F00666666005D5D5D00545454004B4B4B004242
      42003A3A3A00323232002A2A2A00222222000300030064737C00638672008596
      85008C988C0077827A006959690049455600434E58004E9E6A0008FF53000CFF
      54000AF452004B585F0003000300000000000000000086868600DEBE5C00DEBD
      5A00DCBC5600D6B13E00D8A92D00D2A51C00D5AA2C00D7B03900000000000000
      00000000000000000000000000000000000086868600E6CB7D00EBD9A100F0E0
      B400EAD49500E2C56D00DCB94F00000000000000000000000000000000000707
      FF000D0DFF00000000000000000027277600000000000000000095959500E4FF
      FF00E2FFFF00DEFFFF00DCFFFF00DAFFFF00D6FFFF00D4FFFF00D0FFFF00CEFF
      FF00CCFFFF004D4D4D00000000000000000003000300577777004F996E004CA0
      69004C866A00513F51008C8C8C0000000000666666004B4E58001DFC68001FF3
      580027CA5E004A5A5C00030003000000000086868600E2C36900E8D08900E8D2
      8E00E7CE8400E0C26500DAB64900D6A62600D2A61F00D5AC3000D9B340000000
      00000000000000000000000000000000000086868600E4C97800EAD28F00ECDB
      A600E9D49200E1C36800D9B23E00000000000000000000000000000000000000
      00000000F1000000000000000000333399000000000000000000A1A1A100E8FF
      FF0000000000E4FFFF00000000000000000000000000D8FFFF00000000000000
      0000D0FFFF005A5A5A000000000000000000020102003C906B0031C25E002CCD
      5D002EBF4F00513F51000000000000000000000000005543550022DC5B0031CA
      6300409C6E0042916500020102000000000086868600E5C87900F0DBA900F1E3
      B900EFDDAC00E8D18D00E2C77300DAB64800D8AA2F00D5AA2B00D8B23D000000
      0000000000000000000000000000000000000000000086868600E5C97700E8CD
      8600E6C97900DEBB550086868600000000000000000000000000000000000000
      00000000000000000000000000003838A9000000000000000000A9A9A900EBFF
      FF00E9FFFF00E6FFFF00E5FFFF00E2FFFF00E0FFFF00DEFFFF00DAFFFF00D8FF
      FF00D4FFFF006D6D6D00000000000000000003000300485B540026CF590017FF
      6E0014FF7000575B6B005F5F5F0000000000505050005B4B5B0049A270004AA0
      6F00449A710052767000030003000000000086868600E0C06800EFD89F00F6EA
      CA00F5EAC800EFDFAF00E7D18B00DFC16300D8B44600D5A92800D7B139000000
      0000000000000000000000000000000000000000000000000000868686008686
      8600868686008686860000000000000000000000000000009600000000000000
      0000000000000000000000000000000000000000000000000000B5B5B500EEFF
      FF00000000000000000000000000E8FFFF0000000000E2FFFF00000000000000
      0000DAFFFF007575750000000000000000000300030036454B0007F1450006FF
      540006FF540026AD5500434D4F00454551007A708200B3B3B300B1B3B1007A9E
      7A00768F760060506000030003000000000086868600DDB64700E6CE8000F4E6
      C000F6EFD500F5E9C700EEDBA700E6CC7F00DDBC5800D3A72500D6AD32000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000CC00000000000000
      0000181848000000000000000000000000000000000000000000BFBFBF00F3FF
      FF00F0FFFF00EEFFFF00ECFFFF00E9FFFF00E8FFFF00E5FFFF00E4FFFF00E2FF
      FF00DEFFFF00818181000000000000000000000000003D1D400003F8140003FF
      2F0004FF2F0004FF1B0003FF25004DFF8700DBF6D200B4BCB400B7BBB700B0B7
      B0007E8C7E00431A430000000000000000000000000086868600DEBA5300EBD6
      9700F4EBCC00F6EBCC00F0E0B200E7D08900DEBD5B00D2A51E00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000303F100000000000000
      00000000910019194B0000000000000000000000000000000000CCCCCC00F6FF
      FF00000000000000000000000000EEFFFF00EBFFFF00E9FFFF00000000000000
      0000E4FFFF00898989000000000000000000000000003F1B4200266E290003FF
      1C0003FF1C0004FF140004FF1B0017FF7E00E7FFDD00BDCBBD00B4BCB400C1B8
      B7005B4B5B004A1C4A0000000000000000000000000086868600D6AD3400E1C3
      6400EBD49200ECD69A00EBD19100E4C87700DFBB5700D3A52000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000909FF002222
      FF000101FF000000BE001C1C5400000000000000000000000000C2C2C200FBFF
      FF00F7FFFF00F6FFFF00F4FFFF00F1FFFF00F0FFFF00ECFFFF00EBFFFF00E9FF
      FF00E6FFFF0095959500000000000000000000000000000000003C1C4100256E
      290004FF040004FF060004FF140006FF6000D4E4C800B7C1B700777878005B4B
      5B004B1C4B000000000000000000000000000000000000000000868686008686
      8600DFBE5E00E1BF6700E0C26300DDBB54008686860086868600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000404FF000000B00000000000000000000000000000000000B5B5B500BEBE
      BE00C2C2C200C7C7C700C8C8C800C3C3C300BABABA00B5B5B500ACACAC00A9A9
      A900A1A1A1009D9D9D0000000000000000000000000000000000000000003F1B
      42003A1D3F001D6D260009CD0F0006D92A001FCD5B0077767B00471C47004B1B
      4B00000000000000000000000000000000000000000000000000000000000000
      0000868686008686860086868600868686000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000202E8000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000300030003000300030003000300030003000300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000079797900686868000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000300030003000300030003000300030003000300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000071717100686868005D5D5D005D5D5D003C3C3C0000000000000000000000
      00000000000094949400ACACAC00ACACAC000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004A1C
      4A00431A43007C8C890021AC330004EC040004EC040007CA14003B1B4600441D
      4C000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000094949400717171009494
      9400ACACAC00B4B4B400B4B4B400ACACAC008888880068686800686868006868
      680094949400ACACAC00FFFFFF00B4B4B4000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004B1C4B009889
      9800B5BCB500B3BDB30064FE8D0004FF280004FF060003FE060004FF06000ABE
      2200441C4C0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      00000000000000000000000000000000000000000000CFCFCF00B4B4B400B4B4
      B400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B4B4B400B4B4B400B4B4
      B400B4B4B400FFFFFF00FFFFFF00B4B4B4000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000800000008000
      000080000000000000000000000000000000000000004B1B4B00A49AA400DACA
      CE00E5DBDC00F4FEEB00CDFFE80006FF710004FF180003FF110003FF250003FF
      2C001AAB3200461C4C00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000080808000C0C0C000C0C0C0008080
      80000000000000000000000000000000000000000000D5D5D500FFFFFF00FF00
      0000FF000000FFFFFF00FF000000FF000000FFFFFF00FFFFFF00FF000000FF00
      0000FF000000FFFFFF00FFFFFF00B4B4B4000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000800000008000
      00008000000000000000000000000000000000000000471C47009E9B9E00DCC9
      D000CFCCCF00E9ECE100FAFFF10029EE6F0006DC240003FF180004FF390005FF
      500006F44400471C4700000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000000080808000C0C0C000C0C0C000FFFF00008080
      80008080800000000000000000000000000000000000D5D5D500FFFFFF00FF00
      0000FF000000FFFFFF00FF000000FF000000FFFFFF00FF000000FF000000FFFF
      FF00FF000000FF000000FFFFFF00B4B4B4000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000300030064737C00638672008596
      85008C988C0077827A006959690049455600434E58004E9E6A0008FF53000CFF
      54000AF452004B585F00030003000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000C0C0C000C0C0C000C0C0C000C0C0C0008080
      8000C0C0C00000000000000000000000000000000000D5D5D500FFFFFF00FF00
      0000FF000000FFFFFF00FF000000FF000000FFFFFF00FF000000FF000000FFFF
      FF00FF000000FF000000FFFFFF00B4B4B4000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000800000008000
      00008000000000000000000000000000000003000300577777004F996E004CA0
      69004C866A00513F5100AA78AA00000000007C5385004B4E58001DFC68001FF3
      580027CA5E004A5A5C00030003000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000C0C0C000FFFF0000C0C0C000C0C0C0008080
      8000C0C0C00000000000000000000000000000000000D5D5D500FFFFFF00FF00
      0000FF000000FFFFFF00FF000000FF000000FFFFFF00FFFFFF00FF000000FF00
      0000FF000000FFFFFF00FFFFFF00B4B4B4000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000800000008000
      000080000000000000000000000000000000020102003C906B0031C25E002CCD
      5D002EBF4F00513F51000000000000000000000000005543550022DC5B0031CA
      6300409C6E0042916500020102000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000000080808000FFFF0000FFFF0000C0C0C0008080
      80008080800000000000000000000000000000000000D5D5D500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B4B4B4000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      00008000000080000000000000000000000003000300485B540026CF590017FF
      6E0014FF7000575B6B00715275000000000063406B005B4B5B0049A270004AA0
      6F00449A710052767000030003000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000080808000C0C0C000C0C0C0008080
      80000000000000000000000000000000000000000000D5D5D500FFFFFF00FFFF
      FF00FF000000FF000000FF000000FFFFFF00FFFFFF00FF000000FF000000FFFF
      FF00FF000000FF000000FFFFFF00B4B4B4000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800000008000000080000000000000000300030036454B0007F1450006FF
      540006FF540026AD5500434D4F00454551007A708200B3B3B300B1B3B1007A9E
      7A00768F760060506000030003000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      00000000000000000000000000000000000000000000D5D5D500FFFFFF00FF00
      0000FF000000FFFFFF00FF000000FF000000FFFFFF00FF000000FF000000FFFF
      FF00FF000000FF000000FFFFFF00B4B4B4000000000000000000000000000000
      0000000000000000000000000000800000008000000080000000000000000000
      000000000000800000008000000080000000000000003D1D400003F8140003FF
      2F0004FF2F0004FF1B0003FF25004DFF8700DBF6D200B4BCB400B7BBB700B0B7
      B0007E8C7E00431A4300000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      00000000000000000000000000000000000000000000D5D5D500FFFFFF00FF00
      0000FF000000FFFFFF00FF000000FF000000FFFFFF00FF000000FF000000FFFF
      FF00FF000000FF000000FFFFFF00B4B4B4000000000000000000000000000000
      0000000000000000000000000000800000008000000080000000000000000000
      000000000000800000008000000080000000000000003F1B4200266E290003FF
      1C0003FF1C0004FF140004FF1B0017FF7E00E7FFDD00BDCBBD00B4BCB400C1B8
      B7005B4B5B004A1C4A00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      00000000000000000000000000000000000000000000D5D5D500FFFFFF00FFFF
      FF00FF000000FF000000FF000000FFFFFF00FFFFFF00FF000000FF000000FFFF
      FF00FF0000003C3C3C005D5D5D00686868000000000000000000000000000000
      0000000000000000000000000000800000008000000080000000000000000000
      00000000000080000000800000008000000000000000000000003C1C4100256E
      290004FF040004FF060004FF140006FF6000D4E4C800B7C1B700777878005B4B
      5B004B1C4B0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000C0C0C000000000000000
      00000000000000000000000000000000000000000000D5D5D500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF005D5D5D0094949400949494000000000000000000000000000000
      0000000000000000000000000000000000008000000080000000800000008000
      0000800000008000000080000000000000000000000000000000000000003F1B
      42003A1D3F001D6D260009CD0F0006D92A001FCD5B0077767B00471C47004B1B
      4B000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      00000000000000000000000000000000000000000000D5D5D500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF005D5D5D0094949400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000800000008000
      0000800000008000000000000000000000000000000000000000000000000000
      0000000000000300030003000300030003000300030003000300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000ECECEC00D5D5D500D5D5
      D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5
      D500CFCFCF008888880000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF0000000000000000000000000000000000FFFFFF0000000000C5C5
      C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5
      C500C5C5C50000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000DCB02C00BD972500886D
      1B00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF0000000000FFFFFF00B8B8B800D7D7D7008D8D8D00A8A8A800606060000000
      0000FFFFFF0000000000000000000000000000000000FFFFFF0000000000CACA
      CA00CACACA00CACACA00CACACA00CACACA00CACACA00CACACA00CACACA00CACA
      CA00CACACA0000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000EBBC2F00CDA429009375
      1D0000000000000000000000000000000000FCC93200E0B32C00BE982600A382
      200096781E008C701C0083691A0000000000000000000000000000000000FFFF
      FF0000000000FFFFFF00B8B8B800D7D7D7008D8D8D00A8A8A800606060000000
      0000FFFFFF0000000000000000000000000000000000FFFFFF0000000000CFCF
      CF00CFCFCF00CFCFCF00CFCFCF00CFCFCF00CFCFCF00CFCFCF00CFCFCF00CFCF
      CF00CFCFCF0000000000FFFFFF000000000000000000000000000066660099FF
      FF000099CC000099CC000099CC000099CC000099CC000099CC000099CC000099
      CC000099CC000099CC000000000000000000F7C53100F9C73100D1A729009B7C
      1F000000000000000000000000000000000000000000FFDD3700FFCC3300E2B4
      2D00CCA32800B6912400A281200000000000000000000000000000000000FFFF
      FF0000000000FFFFFF00B8B8B800D7D7D7008D8D8D00A8A8A800606060000000
      0000FFFFFF0000000000000000000000000000000000FFFFFF0000000000D4D4
      D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D40000000000FFFFFF000000000000000000000000000066660095F4
      FC0054D5F5003CCEF30024C6F1000CBEEF0000B9EC0000B3E60000ADE00000A4
      D7000099CC000099CC000000000000000000FFD63500FECB3200C69E27000000
      0000000000000000000000000000000000000000000000000000FFE03A00FFD0
      3400F0C03000D2A82A00B792240000000000000000000000000000000000FFFF
      FF0000000000FFFFFF00B8B8B800D7D7D7008D8D8D00A8A8A800606060000000
      0000FFFFFF0000000000000000000000000000000000FFFFFF0000000000DADA
      DA00DADADA00DADADA00DADADA00DADADA00DADADA00DADADA00DADADA00DADA
      DA00DADADA0000000000FFFFFF0000000000000000000066660099FFFF0090E8
      F9006CDDF60054D5F5003CCEF30024C6F1000CBEEF0000B9EC0000B3E60000AD
      E000009ED100000000000099CC0000000000FFE63C00FBC83200BD9725000000
      0000000000000000000000000000000000000000000000000000EBBC2F00FFDA
      3900FFCD3400EBBC2F00CBA2280000000000000000000000000000000000FFFF
      FF0000000000FFFFFF00B8B8B800D7D7D7008D8D8D00A8A8A800606060000000
      0000FFFFFF0000000000000000000000000000000000FFFFFF0000000000DFDF
      DF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDF
      DF00DFDFDF0000000000FFFFFF00000000000000000000666600B8FFFF00A8F0
      FB0084E4F8006CDDF60054D5F5003CCEF30024C6F1000CBEEF0000B9EC0000B3
      E60000AADD00000000000099CC0000000000FFFE4D00FFDA3900CAA128000000
      00000000000000000000000000000000000000000000DFB22C00FBC83200FFD1
      3500FFCD3300FFCD3300E4B62D00000000000000000000000000FFFFFF00FFFF
      FF0000000000FFFFFF00B8B8B800D7D7D7008D8D8D00A8A8A800606060000000
      0000FFFFFF00FFFFFF00000000000000000000000000FFFFFF0000000000E3E3
      E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3
      E300E3E3E30000000000FFFFFF00000000000066660099FFFF00D8FFFF00C0F7
      FD009CECFA0084E4F8006CDDF60054D5F5003CCEF30024C6F1000CBEEF0000B9
      EC00000000000099CC000099CC0000000000FFFF4A00FFFF5C00F4C33000AB89
      2200000000000000000000000000BB952500DFB22C00FBC83200FFD13500DFB2
      2C0000000000FFDE3700FFCC3300000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00000000000000000000000000FFFFFF0000000000E8E8
      E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8E800E8E8
      E800E8E8E80000000000FFFFFF00000000000066660099FFFF0099FFFF0099FF
      FF0099FFFF0092FCFF008AF8FE0080F4FD0074EFFC0065E8FA0057E1F8004DDC
      F60000000000009ED1000099CC000000000000000000FFFF4700FFFF5B00F0C0
      3000C69E2700BD972500CFA52900E6B82E00FBC83200F9C73100EDBD2F000000
      00000000000000000000FFDC3700000000000000000000000000FFFFFF000000
      0000FFFFFF00F1F1F100E3E3E300D2D2D200C0C0C000B0B0B000A0A0A0009090
      900000000000FFFFFF00000000000000000000000000FFFFFF0000000000EDED
      ED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED0000000000000000000000
      00000000000000000000FFFFFF00000000000066660000666600006666000066
      6600006666000066660000666600006666000066660000666600006666000066
      660000AEE10000A8DB000099CC00000000000000000000000000FFFF4700FFFF
      5C00FFD63700FBC83200FFCE3400FFCD3300F6C43100E4B62D00000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      0000FFFFFF00F1F1F100E3E3E300D2D2D200C0C0C000B0B0B000A0A0A0009090
      900000000000FFFFFF00000000000000000000000000FFFFFF0000000000F1F1
      F100F1F1F100F1F1F100F1F1F100F1F1F100F1F1F10000000000C0C0C000D4D4
      D400E4E4E40000000000FFFFFF0000000000000000000066660099FFFF009DEC
      FA0089E6F8006CDDF60056D6F40041CFF3002BC8F10015C1EF0000BBEE0000B7
      EA0000B2E50000ACDF000099CC0000000000000000000000000000000000FFFF
      4A00FFF74700FFE23A00FFD73500FCC932000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00000000000000000000000000FFFFFF0000000000F6F6
      F600F6F6F600F6F6F600F6F6F600F6F6F600F6F6F60000000000D4D4D400E4E4
      E40000000000FFFFFF000000000000000000000000000066660099FFFF00B1F2
      FC009DECFA007FE3F7006CDDF60056D6F40041CFF3003CD2F40027CCF30027C9
      F00027C6EC0027C1E70027B3D900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF0000000000FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB0000000000E4E4E4000000
      0000FFFFFF00000000000000000000000000000000000066660099FFFF00C4F9
      FD00B1F2FC0093E9F9007FE3F7006CDDF60099CCCC0000666600006666000066
      6600006666000066660000666600006666000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF000000000000000000000000000000000000000000000000000066660098FE
      FF0093F9FD0090F5FC008CF1FB0099CCCC000066660000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000066
      6600006666000066660000666600006666000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF000000000000000000000000000000
      00000000000000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      00000000000000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF000000000000000000000000000000
      00000000000000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00000000000000000000000000000000000000
      00000000000000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      000000000000FFFFFF0000000000FFFFFF000000000000000000000000000000
      00000000000000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      000000000000FFFFFF0000000000FFFFFF000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C79F
      27009C7D1F00624E1300000000000000000000000000FFFFFF0000000000C4C4
      FF00C4C4FF00C4C4FF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF000000000000000000000000000000
      00000000000000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF00B08D23009B7C1F00896D1B00856A
      1A0084691A00816719007E65190000000000000000000000000000000000FFD0
      3400E0B32C0091741D00000000000000000000000000FFFFFF0000000000C4C4
      FF000000000000000000000000000000000000000000FFFFFF00000000000000
      0000FFFFFF0000000000FFFFFF000000000000000000FFFFFF00000000000000
      000000000000000000000000000000000000FFFFFF000000000000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF000000000000000000000000000000
      00000000000000000000FFFFFF0000000000FFFFFF000000000000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF00F0C03000DDB02C00CCA32800C39C
      2700B6912400A38220000000000000000000000000000000000000000000FFE7
      3900FFD93700C29B2600735C17000000000000000000FFFFFF0000000000C4C4
      FF00C4C4FF00C4C4FF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF000000000000000000FFFFFF0000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF000000000000000000000000000000
      00000000000000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF00FFDA3700FFD23500F6C43100E1B4
      2D00C59D27000000000000000000000000000000000000000000000000000000
      0000FFEA3F00E1B42D008F721C000000000000000000FFFFFF0000000000C4C4
      FF00000000000000000000000000000000000000000000000000FFFFFF000000
      0000FFFFFF0000000000FFFFFF000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF000000000000000000FFFFFF0000000000FFFFFF000000
      0000000000000000000000000000FFFFFF00000000000000000000000000B5B5
      B50042424200393939003939390000000000FFFFFF0000000000FFFFFF000000
      0000000000000000000000000000FFFFFF00FFED4200FFF34B00FFD93900F0C0
      3000C79F27000000000000000000000000000000000000000000000000000000
      0000FFDF3900EFBF2F009E7E1F000000000000000000FFFFFF0000000000C4C4
      FF00C4C4FF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF000000000000000000FFFFFF0000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF0000000000FFFFFF00FFFFFF00000000000000000000000000B5B5
      B5000000FF000000FF003939390000000000FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF0000000000FFFFFF00FFFFFF00FFFF5C00FFFF5300FFEF4500FFDB
      3900E7B82E00A886210000000000000000000000000000000000000000000000
      0000FFD53600EBBC2F00A88621000000000000000000FFFFFF0000000000C4C4
      FF0000000000C4C4FF0000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF000000000000000000FFFFFF0000000000FFFF
      FF000000000000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF000000
      000000000000FFFFFF00FFFFFF000000000000000000D6D6D600C6C6C600C6C6
      C6000000FF000000FF00393939003939390042424200FFFFFF00FFFFFF000000
      000000000000FFFFFF00FFFFFF0000000000FFFF5500FFE83B0000000000FFFF
      4E00FFF34B00E6B82E00AD8A2200876C1B00000000000000000000000000EDBD
      2F00FFCE3400E1B42D00AE8B22000000000000000000FFFFFF0000000000C4C4
      FF00C4C4FF0000000000000000000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF00FFFFFF000000000000000000FFFFFF0000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00000000000000000000000000D6D6D6000000FF000000
      FF000000FF000000FF000000FF000000FF004242420000000000000000000000
      0000FFFFFF00FFFFFF000000000000000000FFE83A0000000000000000000000
      0000FFFF4600FFFF5C00FBC83200D5AA2A00BD972500C19A2600DAAE2B00F2C1
      3000F2C13000CCA32800000000000000000000000000FFFFFF0037373700C4C4
      FF00C4C4FF00C4C4FF0000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00000000000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000000000D6D6D6000000FF000000
      FF000000FF000000FF000000FF000000FF0042424200FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      000000000000FFF93F00FFFF4900FFF84C00FFDA3900FFD43600FFD33600F7C5
      3100DCB02C0000000000000000000000000000000000FFFFFF00404040004040
      400000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF0000000000FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF0000000000FFFFFF00000000000000
      00000000000000000000000000000000000000000000D6D6D600D6D6D600D6D6
      D6000000FF000000FF0052525200525252004242420000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFEF3B00FFE83A00FFE03800FFD73500F1C0
      30000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000000000000FFFFFF0000000000FFFF
      FF00FFFFFF00FFFFFF000000000000000000FFFFFF00FFFFFF00000000000000
      000000000000000000000000000000000000000000000000000000000000D6D6
      D6000000FF000000FF005A5A5A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000FFFFFF00FFFFFF0000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D6D6
      D600D6D6D600D6D6D6005A5A5A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000900000000100010000000000800400000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0001000000000000FFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFF83FF83FFFFFFFFFE00FE00FFFFFFFFFC007C007F83FF83FC007C007
      F01FF01F80038003F01FF01F80038003F01FF01F80038003F01FF01F80038003
      F01FF01F80038003F83FF83FC007C007FFFFFFFFC007C007FFFFFFFFE00FE00F
      FFFFFFFFF83FF83FFFFFFFFFFFFFFFFFFFFFF001F001FFFF8003F001F0019999
      C007F001F0018888C007F001F001CCCCC007F001F001FFFFC007F001F0019999
      C007F001F0018888C007F001F001CCCCC007F001F001FFFFC007F001F0019999
      C007F001F0018888C00720012001CCCCC00760016001FFFFE00F600160019999
      F01F410F410F8888F83F1F9F1F9FCCCC7EFE9FFF003FFFFFBEFD87FF003FE007
      C00383FF0007E00FE00780000007F01FE00780000000F83FE00780000000F11F
      E00780000000E6CFE00080000000EFEF000780000000CEE7E00780000000CEEF
      E00780000000CF6FE00F80000000CD6FE017E000E000C6C7C03BF800E001E38F
      BF7DF800FC03F01F7F7EF800FC07FFFFFFF3FF3FFFFFFFFFFFE1FE1F0000F83F
      FFC1FC1F0000E00FFF83C03F0000C007F007807700008003C00F00E700008003
      801F00C100000001801F00E6C0030101000F00F6C0030381000F81FEC0030101
      000FC3BFC0030001000FFFB7C0038003801FFFB3C0038003801FFFC1C003C007
      C03FFFF3C003E00FF0FFFFF7FFFFF83FFFFFFFFFFFFCFFFFF83F000CF078F9FF
      E00F00088000F9FFC00700018000F3C780030003800073C780030003800027FF
      00010003800007C701010003800000C703810003800001E301010007800003F1
      0001000F800006388003000F80000E388003000F80001E38C007001F80003F01
      E00F003F80017F83F83F007F8003FFFFFFFFFFFF8001FFFFFFFFE0078001FFFF
      FFFFE0078001FFFF8FFFE0078001E0008F01E0078001C0000F81E0078001C000
      1FC1E007800180001FC1E007800180001F81C003800100000E09C00380010000
      801DC00380010000C03FC00380018000E0FFC00380038000FFFFC00380078000
      FFFFF81F800FC07FFFFFFFFF801FE0FFFFFFFC00FC00FFFFFFFFFC00FC00FFFF
      83C1FC00FC00FFFF8781FC00FC00FFE383C18000FC0001E387818000FC0003E1
      83C18000FC0007F187818000E00007F187818000E00003F183C18001800120E1
      878180038003700383C380078007F8078787803F807FFE0F878F803FE1FFFFFF
      FFFF807FE1FFFFFFFFFF80FFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object ZoomTimer: TTimer
    Enabled = False
    Interval = 50
    OnTimer = ZoomTimerTimer
    Left = 392
    Top = 247
  end
end
