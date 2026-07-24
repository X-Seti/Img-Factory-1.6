object FormEditorItem: TFormEditorItem
  Left = 477
  Top = 82
  Width = 342
  Height = 399
  HelpContext = 20
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Item Editor Dialog'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    334
    372)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 41
    Width = 334
    Height = 331
    ActivePage = paneliplinst
    Anchors = [akLeft, akTop, akRight, akBottom]
    MultiLine = True
    Style = tsButtons
    TabOrder = 1
    object panelideobjs: TTabSheet
      Caption = 'Ide Objects'
      DesignSize = (
        326
        276)
      object Label23: TLabel
        Left = 3
        Top = 3
        Width = 125
        Height = 17
        AutoSize = False
        Caption = ' ID Number'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label24: TLabel
        Left = 3
        Top = 22
        Width = 125
        Height = 17
        AutoSize = False
        Caption = ' Model Name'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label25: TLabel
        Left = 3
        Top = 41
        Width = 125
        Height = 17
        AutoSize = False
        Caption = ' Texture Lib'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label26: TLabel
        Left = 3
        Top = 60
        Width = 125
        Height = 17
        AutoSize = False
        Caption = ' Models (clumps)'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label27: TLabel
        Left = 3
        Top = 79
        Width = 125
        Height = 17
        AutoSize = False
        Caption = ' Draw Distance'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label28: TLabel
        Left = 3
        Top = 98
        Width = 125
        Height = 17
        AutoSize = False
        Caption = ' Flags'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object PaintBox1: TPaintBox
        Left = 280
        Top = 22
        Width = 15
        Height = 16
        OnPaint = PaintBox1Paint
      end
      object PaintBox2: TPaintBox
        Left = 280
        Top = 41
        Width = 15
        Height = 16
        OnPaint = PaintBox1Paint
      end
      object EditIDEObjsID: TEdit
        Left = 130
        Top = 3
        Width = 121
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 0
        OnChange = EditIDEObjsIDChange
      end
      object EditIDEObjsModel: TEdit
        Left = 130
        Top = 22
        Width = 121
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 1
        OnChange = EditIDEObjsModelChange
      end
      object EditIDEObjsTexture: TEdit
        Left = 130
        Top = 41
        Width = 121
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 2
        OnChange = EditIDEObjsTextureChange
      end
      object EditIDEObjsU4: TEdit
        Left = 130
        Top = 60
        Width = 121
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 3
        OnChange = EditIDEObjsU4Change
      end
      object EditIDEObjsLOD: TEdit
        Left = 130
        Top = 79
        Width = 121
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 4
        OnChange = EditIDEObjsLODChange
      end
      object EditIDEObjsFlags: TEdit
        Left = 130
        Top = 98
        Width = 121
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 5
        OnChange = EditIDEObjsFlagsChange
      end
      object BtnIDEObjsModel: TButton
        Left = 253
        Top = 22
        Width = 25
        Height = 17
        Caption = '...'
        TabOrder = 6
        OnClick = BtnIDEObjsModelClick
      end
      object BtnIDEObjsTexture: TButton
        Left = 253
        Top = 41
        Width = 25
        Height = 17
        Caption = '...'
        TabOrder = 7
        OnClick = BtnIDEObjsTextureClick
      end
      object ide_bits: TCheckListBox
        Left = 3
        Top = 121
        Width = 323
        Height = 154
        OnClickCheck = ide_bitsClickCheck
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        Items.Strings = (
          '1    Renders wet effect'
          '2    Tobj Night flag (used with flag 4)'
          '4    Alpha transparency (object doesn'#39't cull other objects)'
          '8    *Another alpha/culling flag*'
          '16   Tobj Day flag (used with flag 4)'
          '32   Interior object'
          '64   Disable shadow culling'
          '128  Excludes surface fom culling'
          '256  Disable Draw-distance (LOD across zones)'
          '512  Breakable window A'
          '1024 Breakable window B')
        TabOrder = 8
      end
    end
    object panelidetobj: TTabSheet
      Caption = 'Ide time objects'
      ImageIndex = 2
      DesignSize = (
        326
        276)
      object Label35: TLabel
        Left = 3
        Top = 117
        Width = 125
        Height = 17
        AutoSize = False
        Caption = ' Time On'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label36: TLabel
        Left = 3
        Top = 136
        Width = 125
        Height = 17
        AutoSize = False
        Caption = ' Time Off'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label66: TLabel
        Left = 3
        Top = 3
        Width = 125
        Height = 17
        AutoSize = False
        Caption = ' ID Number'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label68: TLabel
        Left = 3
        Top = 22
        Width = 125
        Height = 17
        AutoSize = False
        Caption = ' Model Name'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label69: TLabel
        Left = 3
        Top = 41
        Width = 125
        Height = 17
        AutoSize = False
        Caption = ' Texture Lib'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label71: TLabel
        Left = 3
        Top = 60
        Width = 125
        Height = 17
        AutoSize = False
        Caption = ' Models (clumps)'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label72: TLabel
        Left = 3
        Top = 79
        Width = 125
        Height = 17
        AutoSize = False
        Caption = ' Draw Distance'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label73: TLabel
        Left = 3
        Top = 98
        Width = 125
        Height = 17
        AutoSize = False
        Caption = ' Flags'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object PaintBox3: TPaintBox
        Left = 280
        Top = 22
        Width = 15
        Height = 16
        OnPaint = PaintBox1Paint
      end
      object PaintBox4: TPaintBox
        Left = 280
        Top = 41
        Width = 15
        Height = 16
        OnPaint = PaintBox1Paint
      end
      object EditIDETObjTimeOn: TComboBox
        Left = 130
        Top = 117
        Width = 81
        Height = 19
        Style = csDropDownList
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemHeight = 11
        ParentFont = False
        TabOrder = 0
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
        Left = 130
        Top = 136
        Width = 81
        Height = 19
        Style = csDropDownList
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemHeight = 11
        ParentFont = False
        TabOrder = 1
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
      object BtnIDETObjModel: TButton
        Left = 253
        Top = 22
        Width = 25
        Height = 17
        Caption = '...'
        TabOrder = 2
        OnClick = BtnIDETObjModelClick
      end
      object BtnIDETObjTexture: TButton
        Left = 253
        Top = 41
        Width = 25
        Height = 17
        Caption = '...'
        TabOrder = 3
        OnClick = BtnIDETObjTextureClick
      end
      object EditIDETObjID: TEdit
        Left = 130
        Top = 3
        Width = 121
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 4
        OnChange = EditIDETObjIDChange
      end
      object EditIDETObjModel: TEdit
        Left = 130
        Top = 22
        Width = 121
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 5
        OnChange = EditIDETObjModelChange
      end
      object EditIDETObjTexture: TEdit
        Left = 130
        Top = 41
        Width = 121
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 6
        OnChange = EditIDETObjTextureChange
      end
      object EditIDETObjU4: TEdit
        Left = 130
        Top = 60
        Width = 121
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 7
        OnChange = EditIDETObjU4Change
      end
      object EditIDETObjLOD: TEdit
        Left = 130
        Top = 79
        Width = 121
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 8
        OnChange = EditIDETObjLODChange
      end
      object EditIDETObjFlags: TEdit
        Left = 130
        Top = 98
        Width = 121
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 9
        OnChange = EditIDETObjFlagsChange
      end
      object CheckListBox1: TCheckListBox
        Left = 3
        Top = 158
        Width = 323
        Height = 117
        OnClickCheck = CheckListBox1ClickCheck
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        Items.Strings = (
          '1    Renders wet effect'
          '2    Tobj Night flag (used with flag 4)'
          '4    Alpha transparency (object doesn'#39't cull other objects)'
          '8    *Another alpha/culling flag*'
          '16   Tobj Day flag (used with flag 4)'
          '32   Interior object'
          '64   Disable shadow culling'
          '128  Excludes surface fom culling'
          '256  Disable Draw-distance (LOD across zones)'
          '512  Breakable window A'
          '1024 Breakable window B')
        TabOrder = 10
      end
    end
    object paneliplinst: TTabSheet
      Caption = 'Ipl instance'
      ImageIndex = 1
      object Label1: TLabel
        Left = 3
        Top = 3
        Width = 83
        Height = 17
        AutoSize = False
        Caption = ' ID Number'
        Color = clBtnShadow
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label2: TLabel
        Left = 3
        Top = 22
        Width = 83
        Height = 17
        AutoSize = False
        Caption = ' Model Name'
        Color = clBtnShadow
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label3: TLabel
        Left = 3
        Top = 41
        Width = 83
        Height = 17
        AutoSize = False
        Caption = ' Interior'
        Color = clBtnShadow
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label4: TLabel
        Left = 3
        Top = 81
        Width = 83
        Height = 17
        AutoSize = False
        Caption = ' Position'
        Color = clBtnShadow
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label5: TLabel
        Left = 3
        Top = 189
        Width = 83
        Height = 17
        AutoSize = False
        Caption = ' Scale'
        Color = clBtnShadow
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label6: TLabel
        Left = 3
        Top = 135
        Width = 83
        Height = 17
        AutoSize = False
        Caption = ' Rotation'
        Color = clBtnShadow
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object EditIPLInstPosZR1: TSpeedButton
        Left = 260
        Top = 97
        Width = 17
        Height = 17
        Caption = '>>'
        Font.Charset = ANSI_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnMouseDown = EditIPLInstPosZR1MouseDown
        OnMouseUp = EditIPLInstRotZR2MouseUp
      end
      object EditIPLInstPosZR2: TSpeedButton
        Left = 252
        Top = 97
        Width = 9
        Height = 17
        Caption = '>'
        Font.Charset = ANSI_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnMouseDown = EditIPLInstPosZR2MouseDown
        OnMouseUp = EditIPLInstRotZR2MouseUp
      end
      object EditIPLInstPosZL2: TSpeedButton
        Left = 241
        Top = 97
        Width = 9
        Height = 17
        Caption = '<'
        Font.Charset = ANSI_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnMouseDown = EditIPLInstPosZL2MouseDown
        OnMouseUp = EditIPLInstRotZR2MouseUp
      end
      object EditIPLInstPosZL1: TSpeedButton
        Left = 225
        Top = 97
        Width = 17
        Height = 17
        Caption = '<<'
        Font.Charset = ANSI_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnMouseDown = EditIPLInstPosZL1MouseDown
        OnMouseUp = EditIPLInstRotZR2MouseUp
      end
      object EditIPLInstPosYR1: TSpeedButton
        Left = 195
        Top = 97
        Width = 17
        Height = 17
        Caption = '>>'
        Font.Charset = ANSI_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnMouseDown = EditIPLInstPosYR1MouseDown
        OnMouseUp = EditIPLInstRotZR2MouseUp
      end
      object EditIPLInstPosYR2: TSpeedButton
        Left = 187
        Top = 97
        Width = 9
        Height = 17
        Caption = '>'
        Font.Charset = ANSI_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnMouseDown = EditIPLInstPosYR2MouseDown
        OnMouseUp = EditIPLInstRotZR2MouseUp
      end
      object EditIPLInstPosYL1: TSpeedButton
        Left = 160
        Top = 97
        Width = 17
        Height = 17
        Caption = '<<'
        Font.Charset = ANSI_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnMouseDown = EditIPLInstPosYL1MouseDown
        OnMouseUp = EditIPLInstRotZR2MouseUp
      end
      object EditIPLInstPosYL2: TSpeedButton
        Left = 176
        Top = 97
        Width = 9
        Height = 17
        Caption = '<'
        Font.Charset = ANSI_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnMouseDown = EditIPLInstPosYL2MouseDown
        OnMouseUp = EditIPLInstRotZR2MouseUp
      end
      object EditIPLInstPosXR1: TSpeedButton
        Left = 130
        Top = 97
        Width = 17
        Height = 17
        Caption = '>>'
        Font.Charset = ANSI_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnMouseDown = EditIPLInstPosXR1MouseDown
        OnMouseUp = EditIPLInstRotZR2MouseUp
      end
      object EditIPLInstPosXR2: TSpeedButton
        Left = 122
        Top = 97
        Width = 9
        Height = 17
        Caption = '>'
        Font.Charset = ANSI_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnMouseDown = EditIPLInstPosXR2MouseDown
        OnMouseUp = EditIPLInstRotZR2MouseUp
      end
      object EditIPLInstPosXL1: TSpeedButton
        Left = 95
        Top = 97
        Width = 17
        Height = 17
        Caption = '<<'
        Font.Charset = ANSI_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnMouseDown = EditIPLInstPosXL1MouseDown
        OnMouseUp = EditIPLInstRotZR2MouseUp
      end
      object EditIPLInstPosXL2: TSpeedButton
        Left = 111
        Top = 97
        Width = 9
        Height = 17
        Caption = '<'
        Font.Charset = ANSI_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnMouseDown = EditIPLInstPosXL2MouseDown
        OnMouseUp = EditIPLInstRotZR2MouseUp
      end
      object EditIPLInstRotXL1: TSpeedButton
        Left = 94
        Top = 152
        Width = 17
        Height = 17
        Caption = '<<'
        Font.Charset = ANSI_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnMouseDown = EditIPLInstRotXL1MouseDown
        OnMouseUp = EditIPLInstRotZR2MouseUp
      end
      object EditIPLInstRotXL2: TSpeedButton
        Left = 110
        Top = 152
        Width = 9
        Height = 17
        Caption = '<'
        Font.Charset = ANSI_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnMouseDown = EditIPLInstRotXL2MouseDown
        OnMouseUp = EditIPLInstRotZR2MouseUp
      end
      object EditIPLInstRotXR2: TSpeedButton
        Left = 121
        Top = 152
        Width = 9
        Height = 17
        Caption = '>'
        Font.Charset = ANSI_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnMouseDown = EditIPLInstRotXR2MouseDown
        OnMouseUp = EditIPLInstRotZR2MouseUp
      end
      object EditIPLInstRotXR1: TSpeedButton
        Left = 129
        Top = 152
        Width = 17
        Height = 17
        Caption = '>>'
        Font.Charset = ANSI_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnMouseDown = EditIPLInstRotXR1MouseDown
        OnMouseUp = EditIPLInstRotZR2MouseUp
      end
      object EditIPLInstRotYL1: TSpeedButton
        Left = 160
        Top = 152
        Width = 17
        Height = 17
        Caption = '<<'
        Font.Charset = ANSI_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnMouseDown = EditIPLInstRotYL1MouseDown
        OnMouseUp = EditIPLInstRotZR2MouseUp
      end
      object EditIPLInstRotYL2: TSpeedButton
        Left = 176
        Top = 152
        Width = 9
        Height = 17
        Caption = '<'
        Font.Charset = ANSI_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnMouseDown = EditIPLInstRotYL2MouseDown
        OnMouseUp = EditIPLInstRotZR2MouseUp
      end
      object EditIPLInstRotYR2: TSpeedButton
        Left = 187
        Top = 152
        Width = 9
        Height = 17
        Caption = '>'
        Font.Charset = ANSI_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnMouseDown = EditIPLInstRotYR2MouseDown
        OnMouseUp = EditIPLInstRotZR2MouseUp
      end
      object EditIPLInstRotYR1: TSpeedButton
        Left = 195
        Top = 152
        Width = 17
        Height = 17
        Caption = '>>'
        Font.Charset = ANSI_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnMouseDown = EditIPLInstRotYR1MouseDown
        OnMouseUp = EditIPLInstRotZR2MouseUp
      end
      object EditIPLInstRotZL1: TSpeedButton
        Left = 226
        Top = 152
        Width = 17
        Height = 17
        Caption = '<<'
        Font.Charset = ANSI_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnMouseDown = EditIPLInstRotZL1MouseDown
        OnMouseUp = EditIPLInstRotZR2MouseUp
      end
      object EditIPLInstRotZL2: TSpeedButton
        Left = 242
        Top = 152
        Width = 9
        Height = 17
        Caption = '<'
        Font.Charset = ANSI_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnMouseDown = EditIPLInstRotZL2MouseDown
        OnMouseUp = EditIPLInstRotZR2MouseUp
      end
      object EditIPLInstRotZR2: TSpeedButton
        Left = 253
        Top = 152
        Width = 9
        Height = 17
        Caption = '>'
        Font.Charset = ANSI_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnMouseDown = EditIPLInstRotZR2MouseDown
        OnMouseUp = EditIPLInstRotZR2MouseUp
      end
      object EditIPLInstRotZR1: TSpeedButton
        Left = 261
        Top = 152
        Width = 17
        Height = 17
        Caption = '>>'
        Font.Charset = ANSI_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnMouseDown = EditIPLInstRotZR1MouseDown
        OnMouseUp = EditIPLInstRotZR2MouseUp
      end
      object Label30: TLabel
        Left = 89
        Top = 61
        Width = 63
        Height = 17
        Alignment = taCenter
        AutoSize = False
        Caption = 'X'
        Color = clBtnShadow
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label31: TLabel
        Left = 154
        Top = 61
        Width = 63
        Height = 17
        Alignment = taCenter
        AutoSize = False
        Caption = 'Y'
        Color = clBtnShadow
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label32: TLabel
        Left = 219
        Top = 61
        Width = 63
        Height = 17
        Alignment = taCenter
        AutoSize = False
        Caption = 'Z'
        Color = clBtnShadow
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object EditIPLInstID: TEdit
        Left = 88
        Top = 3
        Width = 121
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 0
        OnChange = EditIPLInstIDChange
      end
      object EditIPLInstInterior: TEdit
        Left = 88
        Top = 41
        Width = 97
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 1
        OnChange = EditIPLInstInteriorChange
      end
      object EditIPLInstPosX: TEdit
        Left = 88
        Top = 81
        Width = 65
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 2
        OnChange = EditIPLInstPosXChange
      end
      object EditIPLInstScaleX: TEdit
        Left = 88
        Top = 189
        Width = 65
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 3
        OnChange = EditIPLInstScaleXChange
      end
      object EditIPLInstRotX: TEdit
        Left = 88
        Top = 135
        Width = 65
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 4
        OnChange = EditIPLInstRotXChange
      end
      object EditIPLInstPosY: TEdit
        Left = 153
        Top = 81
        Width = 65
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 5
        OnChange = EditIPLInstPosYChange
      end
      object EditIPLInstPosZ: TEdit
        Left = 218
        Top = 81
        Width = 65
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 6
        OnChange = EditIPLInstPosZChange
      end
      object EditIPLInstScaleY: TEdit
        Left = 153
        Top = 189
        Width = 65
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 7
        OnChange = EditIPLInstScaleYChange
      end
      object EditIPLInstScaleZ: TEdit
        Left = 218
        Top = 189
        Width = 65
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 8
        OnChange = EditIPLInstScaleZChange
      end
      object EditIPLInstRotY: TEdit
        Left = 153
        Top = 135
        Width = 65
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 9
        OnChange = EditIPLInstRotYChange
      end
      object EditIPLInstRotZ: TEdit
        Left = 218
        Top = 135
        Width = 65
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 10
        OnChange = EditIPLInstRotZChange
      end
      object EditIPLInstModel: TEdit
        Left = 88
        Top = 22
        Width = 121
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 11
        OnChange = EditIPLInstModelChange
      end
      object EditIPLInstHasInterior: TCheckBox
        Left = 192
        Top = 41
        Width = 103
        Height = 17
        Caption = 'has interior'
        TabOrder = 12
        OnClick = EditIPLInstHasInteriorClick
      end
      object BitBtn1: TBitBtn
        Left = 3
        Top = 102
        Width = 83
        Height = 17
        Caption = 'Center in view'
        TabOrder = 13
        OnClick = BitBtn1Click
      end
      object BitBtn2: TBitBtn
        Left = 4
        Top = 156
        Width = 83
        Height = 17
        Caption = 'Reset Rotation'
        TabOrder = 14
        OnClick = BitBtn2Click
      end
      object EditIPLInstValidation: TListView
        Left = 20
        Top = 219
        Width = 264
        Height = 73
        BorderStyle = bsNone
        Color = clBtnFace
        Columns = <
          item
            Caption = 'Item'
            Width = 180
          end
          item
            Caption = 'Status'
            Width = 80
          end>
        LargeImages = FormMain.ImageList
        ShowColumnHeaders = False
        SmallImages = FormMain.ImageList
        StateImages = FormMain.ImageList
        TabOrder = 15
        ViewStyle = vsReport
      end
    end
    object paneliplmultinst: TTabSheet
      Caption = 'ipl multiinst'
      ImageIndex = 3
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
        Font.Name = 'Tahoma'
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
        Font.Name = 'Tahoma'
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
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
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
        TabOrder = 1
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
        TabOrder = 2
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
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
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
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 4
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
        TabOrder = 5
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
        TabOrder = 6
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
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 7
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
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 8
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
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 9
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
        TabOrder = 10
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
        TabOrder = 11
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
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 12
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
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 13
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
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 14
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
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 15
      end
      object Panel17: TPanel
        Left = 344
        Top = 198
        Width = 9
        Height = 17
        BevelOuter = bvNone
        Caption = '<'
        TabOrder = 16
      end
      object Panel18: TPanel
        Left = 344
        Top = 182
        Width = 9
        Height = 17
        BevelOuter = bvNone
        Caption = '<'
        TabOrder = 17
      end
      object Panel19: TPanel
        Left = 344
        Top = 166
        Width = 9
        Height = 17
        BevelOuter = bvNone
        Caption = '<'
        TabOrder = 18
      end
      object Panel20: TPanel
        Left = 352
        Top = 166
        Width = 9
        Height = 17
        BevelOuter = bvNone
        Caption = '>'
        TabOrder = 19
      end
      object Panel21: TPanel
        Left = 352
        Top = 182
        Width = 9
        Height = 17
        BevelOuter = bvNone
        Caption = '>'
        TabOrder = 20
      end
      object Panel22: TPanel
        Left = 352
        Top = 198
        Width = 9
        Height = 17
        BevelOuter = bvNone
        Caption = '>'
        TabOrder = 21
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
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 22
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
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 23
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
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 24
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
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 25
      end
      object EditIPLMultInstPosX: TEdit
        Left = 88
        Top = 184
        Width = 65
        Height = 17
        BorderStyle = bsNone
        TabOrder = 26
        OnKeyDown = EditIPLMultInstPosXKeyDown
      end
      object EditIPLMultInstPosY: TEdit
        Left = 152
        Top = 184
        Width = 65
        Height = 17
        BorderStyle = bsNone
        TabOrder = 27
        OnKeyDown = EditIPLMultInstPosYKeyDown
      end
      object EditIPLMultInstPosZ: TEdit
        Left = 216
        Top = 184
        Width = 65
        Height = 17
        BorderStyle = bsNone
        TabOrder = 28
        OnKeyDown = EditIPLMultInstPosZKeyDown
      end
      object EditIPLMultInstList: TListView
        Left = 8
        Top = 8
        Width = 476
        Height = 153
        BorderStyle = bsNone
        Columns = <>
        ReadOnly = True
        RowSelect = True
        TabOrder = 29
        ViewStyle = vsList
      end
    end
    object paneliplpath: TTabSheet
      Caption = 'IPL Path'
      ImageIndex = 4
      DesignSize = (
        326
        276)
      object Label65: TLabel
        Left = 148
        Top = 6
        Width = 70
        Height = 17
        AutoSize = False
        Caption = ' Unknown'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object boxpathtype: TRadioGroup
        Left = 3
        Top = 3
        Width = 138
        Height = 41
        Caption = ' Path type '
        Columns = 3
        ItemIndex = 0
        Items.Strings = (
          'Ped'
          'Car'
          'Boat')
        TabOrder = 3
        OnClick = boxpathtypeClick
      end
      object EditIPLPathItems: TListView
        Left = 3
        Top = 50
        Width = 323
        Height = 145
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Index'
            Width = 43
          end
          item
            Caption = 'Type'
            Width = 43
          end
          item
            Caption = 'Link to'
            Width = 43
          end
          item
            Caption = 'Coords'
            Width = 170
          end>
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnSelectItem = EditIPLPathItemsSelectItem
      end
      object EditIPLPathOther: TEdit
        Left = 148
        Top = 26
        Width = 70
        Height = 19
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 1
        OnChange = EditIPLPathOtherChange
      end
      object PanelIPLPathItem: TPanel
        Left = 3
        Top = 202
        Width = 323
        Height = 74
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 2
        object Label67: TLabel
          Left = 80
          Top = 34
          Width = 57
          Height = 13
          AutoSize = False
          Caption = 'Position'
        end
        object EditIPLPathItemLL0: TSpeedButton
          Left = 202
          Top = 53
          Width = 17
          Height = 17
          GroupIndex = 1
          Down = True
          Caption = '0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBtnText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          OnClick = EditIPLPathItemLL0Click
        end
        object EditIPLPathItemLL1: TSpeedButton
          Tag = 1
          Left = 218
          Top = 53
          Width = 17
          Height = 17
          GroupIndex = 1
          Caption = '1'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBtnText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          OnClick = EditIPLPathItemLL0Click
        end
        object EditIPLPathItemLL2: TSpeedButton
          Tag = 2
          Left = 234
          Top = 53
          Width = 17
          Height = 17
          GroupIndex = 1
          Caption = '2'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBtnText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          OnClick = EditIPLPathItemLL0Click
        end
        object EditIPLPathItemLR0: TSpeedButton
          Left = 255
          Top = 53
          Width = 17
          Height = 17
          GroupIndex = 2
          Down = True
          Caption = '0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBtnText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          OnClick = EditIPLPathItemLR0Click
        end
        object EditIPLPathItemLR1: TSpeedButton
          Tag = 1
          Left = 271
          Top = 53
          Width = 17
          Height = 17
          GroupIndex = 2
          Caption = '1'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBtnText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          OnClick = EditIPLPathItemLR0Click
        end
        object EditIPLPathItemLR2: TSpeedButton
          Tag = 2
          Left = 287
          Top = 53
          Width = 17
          Height = 17
          GroupIndex = 2
          Caption = '2'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBtnText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          OnClick = EditIPLPathItemLR0Click
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
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          OnClick = EditIPLPathItemTypeNoneClick
        end
        object EditIPLPathItemTypeMid: TSpeedButton
          Tag = 2
          Left = 49
          Top = 8
          Width = 33
          Height = 17
          GroupIndex = 3
          Caption = 'Mid'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBtnText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          OnClick = EditIPLPathItemTypeNoneClick
        end
        object EditIPLPathItemTypeEnd: TSpeedButton
          Tag = 1
          Left = 82
          Top = 8
          Width = 34
          Height = 17
          GroupIndex = 3
          Caption = 'End'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBtnText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          OnClick = EditIPLPathItemTypeNoneClick
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
          AutoSize = False
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 3
          OnChange = EditIPLPathItemPosXChange
        end
        object EditIPLPathItemPosY: TEdit
          Left = 193
          Top = 32
          Width = 57
          Height = 17
          AutoSize = False
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 4
          OnChange = EditIPLPathItemPosXChange
        end
        object EditIPLPathItemPosZ: TEdit
          Left = 250
          Top = 32
          Width = 57
          Height = 17
          AutoSize = False
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 5
          OnChange = EditIPLPathItemPosXChange
        end
        object EditIPLPathItemU3: TEdit
          Left = 120
          Top = 7
          Width = 41
          Height = 17
          AutoSize = False
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 0
          OnChange = EditIPLPathItemU3Change
        end
        object EditIPLPathItemU7: TEdit
          Left = 161
          Top = 7
          Width = 41
          Height = 17
          AutoSize = False
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 1
          OnChange = EditIPLPathItemU7Change
        end
        object EditIPLPathItemConnect: TEdit
          Left = 32
          Top = 31
          Width = 33
          Height = 17
          AutoSize = False
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 2
          OnChange = EditIPLPathItemConnectChange
        end
        object EditIPLPathItemU10: TEdit
          Left = 208
          Top = 8
          Width = 33
          Height = 17
          AutoSize = False
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 6
          OnChange = EditIPLPathItemU10Change
        end
        object EditIPLPathItemU11: TEdit
          Left = 241
          Top = 8
          Width = 33
          Height = 17
          AutoSize = False
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 7
          OnChange = EditIPLPathItemU11Change
        end
        object EditIPLPathItemU12: TEdit
          Left = 274
          Top = 8
          Width = 33
          Height = 17
          AutoSize = False
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 8
          OnChange = EditIPLPathItemU12Change
        end
      end
      object BitBtn3: TBitBtn
        Left = 224
        Top = 26
        Width = 70
        Height = 19
        Caption = 'Show in View'
        TabOrder = 4
        OnClick = BitBtn3Click
      end
    end
    object paneliplzone: TTabSheet
      Caption = 'IPL Zone'
      ImageIndex = 5
      object Label37: TLabel
        Left = 3
        Top = 3
        Width = 125
        Height = 17
        AutoSize = False
        Caption = ' Zone Name'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label38: TLabel
        Left = 3
        Top = 22
        Width = 125
        Height = 17
        AutoSize = False
        Caption = ' Type'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label39: TLabel
        Left = 3
        Top = 41
        Width = 125
        Height = 17
        AutoSize = False
        Caption = ' Island (map.zon)'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label40: TLabel
        Left = 3
        Top = 60
        Width = 125
        Height = 17
        AutoSize = False
        Caption = ' Start'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label41: TLabel
        Left = 3
        Top = 98
        Width = 125
        Height = 17
        AutoSize = False
        Caption = ' End'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label42: TLabel
        Left = 131
        Top = 79
        Width = 193
        Height = 17
        Alignment = taCenter
        AutoSize = False
        Caption = 'to'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object EditIPLZoneName: TEdit
        Left = 130
        Top = 3
        Width = 121
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 0
        OnChange = EditIPLZoneNameChange
      end
      object EditIPLZoneU9: TEdit
        Left = 130
        Top = 41
        Width = 121
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 1
        OnChange = EditIPLZoneU9Change
      end
      object EditIPLZonePos1X: TEdit
        Left = 130
        Top = 60
        Width = 65
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 2
        OnChange = EditIPLZonePos1XChange
      end
      object EditIPLZonePos1Y: TEdit
        Left = 195
        Top = 60
        Width = 65
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 3
        OnChange = EditIPLZonePos1YChange
      end
      object EditIPLZonePos1Z: TEdit
        Left = 260
        Top = 60
        Width = 65
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 4
        OnChange = EditIPLZonePos1ZChange
      end
      object EditIPLZonePos2X: TEdit
        Left = 130
        Top = 98
        Width = 65
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 5
        OnChange = EditIPLZonePos2XChange
      end
      object EditIPLZonePos2Y: TEdit
        Left = 195
        Top = 98
        Width = 65
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 6
        OnChange = EditIPLZonePos2YChange
      end
      object EditIPLZonePos2Z: TEdit
        Left = 260
        Top = 98
        Width = 65
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 7
        OnChange = EditIPLZonePos2ZChange
      end
      object box_zonetypes: TComboBox
        Left = 130
        Top = 21
        Width = 121
        Height = 19
        Style = csDropDownList
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemHeight = 11
        ItemIndex = 0
        ParentFont = False
        TabOrder = 8
        Text = '0= Visual Navigation'
        OnChange = box_zonetypesChange
        Items.Strings = (
          '0= Visual Navigation'
          '1= Unknown'
          '2= Traffic Ped Types'
          '3= island_split')
      end
    end
    object paneliplcull: TTabSheet
      Caption = 'IPL Cull'
      ImageIndex = 6
      object Label43: TLabel
        Left = 3
        Top = 3
        Width = 125
        Height = 17
        AutoSize = False
        Caption = ' Vertex 1'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label44: TLabel
        Left = 3
        Top = 60
        Width = 125
        Height = 17
        AutoSize = False
        Caption = ' Unknown'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label46: TLabel
        Left = 3
        Top = 22
        Width = 125
        Height = 17
        AutoSize = False
        Caption = ' Vertex 2'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label47: TLabel
        Left = 3
        Top = 41
        Width = 125
        Height = 17
        AutoSize = False
        Caption = ' Vertex 3'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object EditIPLCullPos1X: TEdit
        Left = 130
        Top = 3
        Width = 65
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 0
      end
      object EditIPLCullPos1Y: TEdit
        Left = 195
        Top = 3
        Width = 65
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 1
      end
      object EditIPLCullPos1Z: TEdit
        Left = 260
        Top = 3
        Width = 65
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 2
      end
      object EditIPLCullU10: TEdit
        Left = 130
        Top = 60
        Width = 49
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 3
      end
      object EditIPLCullU11: TEdit
        Left = 181
        Top = 60
        Width = 49
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 4
      end
      object EditIPLCullPos2X: TEdit
        Left = 130
        Top = 22
        Width = 65
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 5
      end
      object EditIPLCullPos2Y: TEdit
        Left = 195
        Top = 22
        Width = 65
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 6
      end
      object EditIPLCullPos2Z: TEdit
        Left = 260
        Top = 22
        Width = 65
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 7
      end
      object EditIPLCullPos3X: TEdit
        Left = 130
        Top = 41
        Width = 65
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 8
      end
      object EditIPLCullPos3Y: TEdit
        Left = 195
        Top = 41
        Width = 65
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 9
      end
      object EditIPLCullPos3Z: TEdit
        Left = 260
        Top = 41
        Width = 65
        Height = 17
        AutoSize = False
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 10
      end
    end
    object panelide2dfx: TTabSheet
      Caption = 'Ipl 2DFX'
      ImageIndex = 7
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
        TabOrder = 0
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
        Top = 104
        Width = 369
        Height = 121
        TabOrder = 1
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
        Top = 104
        Width = 369
        Height = 121
        TabOrder = 2
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
      object PanelIDE2dfxReflection: TPanel
        Left = 8
        Top = 104
        Width = 369
        Height = 121
        Caption = 'Not Available'
        TabOrder = 3
        Visible = False
      end
      object EditIDE2dfxID: TEdit
        Left = 88
        Top = 16
        Width = 121
        Height = 17
        BorderStyle = bsNone
        TabOrder = 4
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
        TabOrder = 8
        OnChange = EditIDE2dfxColRChange
      end
      object EditIDE2dfxColG: TEdit
        Left = 136
        Top = 48
        Width = 49
        Height = 17
        BorderStyle = bsNone
        TabOrder = 9
        OnChange = EditIDE2dfxColGChange
      end
      object EditIDE2dfxColB: TEdit
        Left = 184
        Top = 48
        Width = 49
        Height = 17
        BorderStyle = bsNone
        TabOrder = 10
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
        TabOrder = 11
        OnChange = EditIDE2dfxColChooserChange
      end
      object EditIDE2dfxRadioLight: TRadioButton
        Left = 392
        Top = 80
        Width = 89
        Height = 17
        Caption = 'Lighting'
        Checked = True
        TabOrder = 12
        TabStop = True
        OnClick = EditIDE2dfxRadioLightClick
      end
      object EditIDE2dfxRadioParticle: TRadioButton
        Left = 392
        Top = 96
        Width = 89
        Height = 17
        Caption = 'Particle'
        TabOrder = 13
        OnClick = EditIDE2dfxRadioParticleClick
      end
      object EditIDE2dfxRadioUnknown: TRadioButton
        Left = 392
        Top = 112
        Width = 89
        Height = 17
        Caption = 'N/A'
        Enabled = False
        TabOrder = 14
      end
      object EditIDE2dfxRadioAnimation: TRadioButton
        Left = 392
        Top = 128
        Width = 89
        Height = 17
        Caption = 'Animation'
        TabOrder = 15
        OnClick = EditIDE2dfxRadioAnimationClick
      end
      object EditIDE2dfxRadioReflection: TRadioButton
        Left = 392
        Top = 144
        Width = 89
        Height = 17
        Caption = 'Reflection'
        TabOrder = 16
        OnClick = EditIDE2dfxRadioReflectionClick
      end
      object EditIDE2dfxViewDistance: TEdit
        Left = 392
        Top = 56
        Width = 97
        Height = 17
        BorderStyle = bsNone
        TabOrder = 17
        OnChange = EditIDE2dfxViewDistanceChange
      end
      object PanelIDE2dfxUnknown: TPanel
        Left = 8
        Top = 104
        Width = 369
        Height = 121
        Caption = 'Not Available'
        TabOrder = 18
        Visible = False
      end
    end
    object panelidepath: TTabSheet
      Caption = 'IDE path (gta3)'
      ImageIndex = 8
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
        TabOrder = 0
        OnChange = EditIDEPathIDChange
      end
      object EditIDEPathModel: TEdit
        Left = 88
        Top = 80
        Width = 121
        Height = 17
        BorderStyle = bsNone
        TabOrder = 1
        OnChange = EditIDEPathModelChange
      end
      object EditIDEPathRadioPed: TRadioButton
        Left = 88
        Top = 16
        Width = 113
        Height = 17
        Caption = 'Pedestrian (ped)'
        Checked = True
        TabOrder = 2
        TabStop = True
        OnClick = EditIDEPathRadioPedClick
      end
      object EditIDEPathRadioCar: TRadioButton
        Left = 88
        Top = 32
        Width = 113
        Height = 17
        Caption = 'Vehicle (car)'
        TabOrder = 3
        OnClick = EditIDEPathRadioCarClick
      end
      object EditIDEPathItems: TListView
        Left = 216
        Top = 8
        Width = 273
        Height = 153
        BorderStyle = bsNone
        Columns = <>
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
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
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
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
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
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
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
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
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
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
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
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
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
          Font.Name = 'Tahoma'
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
          Font.Name = 'Tahoma'
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
          Font.Name = 'Tahoma'
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
  end
  object InfoPanel: TPanel
    Left = 0
    Top = 0
    Width = 334
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      334
      41)
    object ModeLabel: TLabel
      Left = 3
      Top = 0
      Width = 41
      Height = 13
      AutoSize = False
      Caption = 'Mode:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
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
      Font.Name = 'Tahoma'
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
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
    end
    object FileIndexValue: TLabel
      Left = 160
      Top = 0
      Width = 98
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object ExpandedEdit: TEdit
      Left = 3
      Top = 20
      Width = 256
      Height = 19
      Anchors = [akLeft, akTop, akRight]
      Ctl3D = False
      ParentCtl3D = False
      ReadOnly = True
      TabOrder = 0
    end
    object BtnUndo: TBitBtn
      Left = 261
      Top = 20
      Width = 70
      Height = 19
      Anchors = [akRight, akBottom]
      Caption = '&Undo'
      Enabled = False
      ModalResult = 4
      TabOrder = 1
      OnClick = BtnUndoClick
      Glyph.Data = {
        D6040000424DD60400000000000036040000280000000F0000000A0000000100
        080000000000A0000000230B0000230B00000001000000010000624E1300735C
        17007E6519008167190084691A00856A1A00876C1B00896D1B008F721C009174
        1D009B7C1F009C7D1F009E7E1F00A3822000A8862100AD8A2200AE8B2200B08D
        2300B6912400BD972500C19A2600C29B2600C39C2700C59D2700C79F2700CCA3
        2800D5AA2A00DAAE2B00DCB02C00DDB02C00E0B32C00E1B42D00E6B82E00E7B8
        2E00EBBC2F00EDBD2F00EFBF2F00F0C03000F1C03000F2C13000F6C43100F7C5
        3100FBC83200FFCE3400FFD03400FFD23500FFD33600FFD73500FFD43600FFD5
        3600FFD93700FFDA3700FFD93900FFDA3900FFDB3900FFDF3900FFE03800FFE7
        3900FFE83A00FFE83B00FFEF3B00FFEA3F00FFF93F00FFED4200FFEF4500FFFF
        4600FFF34B00FFFF4900FFF84C00FFFF4E00FFFF5300FFFF5500FFFF5C00FF00
        FF00000000000000000000000000000000000000000000000000000000000000
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
        0000000000000000000000000000000000000000000000000000494949494949
        4949494949180B004900110A0705040302494949492C1E094900251D1916120D
        49494949493932150100332D281F17494949494949493D1F08003F4234251849
        49494949494937240C0048464036210E49494949494931220E00473B49454220
        0F06494949232B1F10003A49494941482A1A13141B272719490049494949493E
        434435302E291C494900494949494949493C3A382F2649494900}
    end
    object BtnCancel: TBitBtn
      Left = 261
      Top = 0
      Width = 70
      Height = 19
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Close'
      ModalResult = 2
      TabOrder = 2
      OnClick = BtnCancelClick
      Glyph.Data = {
        D6040000424DD60400000000000036040000280000000F0000000A0000000100
        080000000000A0000000330B0000330B000000010000000100003F3F3F004848
        48004B4B4B00575757006D6D6D006F6F6F00757575007B7B7B007E7E7E00FF00
        FF00818181008989890092929200939393009696960099999900A1A1A100AEAE
        AE00AFAFAF00B1B1B100B4B4B400B6B6B600BCBCBC00BDBDBD00C2C2C200C5C5
        C500C6C6C600CCCCCC00CFCFCF00D4D4D400D5D5D500D7D7D700D9D9D900DEDE
        DE00ECECEC000000000000000000000000000000000000000000000000000000
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
        0000000000000000000000000000000000000000000000000000090909090909
        090309090909090909FF0909090909090D0A00090909090909FF09090909090E
        131208000909090909FF090909090E13161711080009090909FF0909090E1316
        171717110800090909FF09090E131617171717171108000909FF090E13161717
        171717171711080009FF15171A1A1919191919191918150B01FF22211F1E1E1E
        1E1E1E1E1E1E1D1B0CFF090909090909090909090909090909FF}
    end
  end
  object MoveTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = MoveTimerTimer
    Left = 472
    Top = 6
  end
  object ImageList1: TImageList
    Left = 323
    Top = 39
    Bitmap = {
      494C010102000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001001000000000000008
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
      00000000000000000000000000000000000000000000000000000000007C007C
      007C007C007C00000000000000000000000000000000000000000000E002E002
      E002E002E0020000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000007C007C007C007C
      007C007C007C007C007C0000000000000000000000000000E002E002E002E002
      E002E002E002E002E00200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000007C007CFF7F007C
      007C007CFF7F007C007C0000000000000000000000000000E002E002E002FF7F
      FF7FE002E002E002E00200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000007C007CFF7FFF7FFF7F
      007CFF7FFF7FFF7F007C007C00000000000000000000E002E002E002FF7FFF7F
      FF7FFF7FE002E002E002E0020000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000007C007C007CFF7FFF7F
      FF7FFF7FFF7F007C007C007C00000000000000000000E002E002FF7FFF7FE002
      FF7FFF7FE002E002E002E0020000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000007C007C007C007CFF7F
      FF7FFF7F007C007C007C007C00000000000000000000E002E002E002E002E002
      E002FF7FFF7FE002E002E0020000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000007C007C007CFF7FFF7F
      FF7FFF7FFF7F007C007C007C00000000000000000000E002E002E002E002E002
      E002FF7FFF7FE002E002E0020000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000007C007CFF7FFF7FFF7F
      007CFF7FFF7FFF7F007C007C00000000000000000000E002E002E002E002E002
      E002E002FF7FFF7FE002E0020000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000007C007CFF7F007C
      007C007CFF7F007C007C0000000000000000000000000000E002E002E002E002
      E002E002FF7FFF7FE00200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000007C007C007C007C
      007C007C007C007C007C0000000000000000000000000000E002E002E002E002
      E002E002E002E002E00200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000007C007C
      007C007C007C00000000000000000000000000000000000000000000E002E002
      E002E002E0020000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF00000000FFFFFFFF00000000
      F83FF83F00000000E00FE00F00000000C007C00700000000C007C00700000000
      8003800300000000800380030000000080038003000000008003800300000000
      8003800300000000C007C00700000000C007C00700000000E00FE00F00000000
      F83FF83F00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
end
