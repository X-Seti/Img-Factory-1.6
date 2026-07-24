object FormEditorDAT: TFormEditorDAT
  Left = 192
  Top = 107
  Width = 500
  Height = 300
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'DAT Editor'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 248
    Width = 492
    Height = 2
    Align = alBottom
  end
  object Bevel2: TBevel
    Left = 442
    Top = 0
    Width = 2
    Height = 248
    Align = alRight
  end
  object ListDAT: TListBox
    Left = 0
    Top = 0
    Width = 442
    Height = 248
    Align = alClient
    BevelInner = bvNone
    BorderStyle = bsNone
    Ctl3D = False
    ItemHeight = 13
    ParentCtl3D = False
    TabOrder = 0
    OnClick = ListDATClick
  end
  object PanelUpdate: TPanel
    Left = 0
    Top = 250
    Width = 492
    Height = 23
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      492
      23)
    object BtnAdd: TButton
      Left = 384
      Top = 2
      Width = 51
      Height = 19
      Anchors = [akTop, akRight]
      Caption = '&Insert'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = BtnAddClick
    end
    object EditLine: TEdit
      Left = 3
      Top = 2
      Width = 324
      Height = 19
      Anchors = [akLeft, akTop, akRight]
      BevelInner = bvNone
      BevelKind = bkSoft
      BevelOuter = bvNone
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 0
    end
    object BtnUpdate: TButton
      Left = 330
      Top = 2
      Width = 51
      Height = 19
      Anchors = [akTop, akRight]
      Caption = '&Update'
      Default = True
      Enabled = False
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = BtnUpdateClick
    end
    object BtnSave: TButton
      Left = 440
      Top = 2
      Width = 51
      Height = 19
      Anchors = [akTop, akRight]
      Caption = '&Save File'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = BtnSaveClick
    end
  end
  object PanelMove: TPanel
    Left = 444
    Top = 0
    Width = 48
    Height = 248
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      Left = 4
      Top = 2
      Width = 41
      Height = 13
      AutoSize = False
      Caption = 'Move:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object BtnUp: TButton
      Left = 2
      Top = 18
      Width = 45
      Height = 17
      Caption = 'Up'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = BtnUpClick
    end
    object BtnDown: TButton
      Left = 2
      Top = 34
      Width = 45
      Height = 17
      Caption = 'Down'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = BtnDownClick
    end
    object BtnDel: TButton
      Left = 1
      Top = 58
      Width = 45
      Height = 17
      Caption = 'Delete'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = BtnDelClick
    end
  end
end
