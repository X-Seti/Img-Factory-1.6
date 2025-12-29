object FormEditorDAT: TFormEditorDAT
  Left = 192
  Top = 107
  Width = 500
  Height = 300
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'DAT Editor'
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
  object ListDAT: TListBox
    Left = 0
    Top = 0
    Width = 448
    Height = 256
    Align = alClient
    BorderStyle = bsNone
    ItemHeight = 13
    TabOrder = 0
    OnClick = ListDATClick
  end
  object PanelUpdate: TPanel
    Left = 0
    Top = 256
    Width = 492
    Height = 17
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object BtnAdd: TButton
      Left = 384
      Top = 0
      Width = 51
      Height = 17
      Caption = '&Insert'
      TabOrder = 2
      OnClick = BtnAddClick
    end
    object EditLine: TEdit
      Left = 0
      Top = 0
      Width = 329
      Height = 17
      BevelKind = bkSoft
      BevelOuter = bvNone
      BorderStyle = bsNone
      TabOrder = 0
    end
    object BtnUpdate: TButton
      Left = 328
      Top = 0
      Width = 51
      Height = 17
      Caption = '&Update'
      Default = True
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = BtnUpdateClick
    end
    object BtnSave: TButton
      Left = 440
      Top = 0
      Width = 51
      Height = 17
      Caption = '&Save File'
      TabOrder = 3
      OnClick = BtnSaveClick
    end
  end
  object PanelMove: TPanel
    Left = 448
    Top = 0
    Width = 44
    Height = 256
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      Left = 2
      Top = 0
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
      Left = 0
      Top = 16
      Width = 45
      Height = 17
      Caption = 'Up'
      TabOrder = 0
      OnClick = BtnUpClick
    end
    object BtnDown: TButton
      Left = 0
      Top = 32
      Width = 45
      Height = 17
      Caption = 'Down'
      TabOrder = 1
      OnClick = BtnDownClick
    end
    object BtnDel: TButton
      Left = -1
      Top = 56
      Width = 45
      Height = 17
      Caption = 'Del'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = BtnDelClick
    end
  end
end
