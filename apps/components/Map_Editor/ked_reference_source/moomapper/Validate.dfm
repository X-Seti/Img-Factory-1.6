object FormValidate: TFormValidate
  Left = 214
  Top = 142
  Width = 600
  Height = 299
  HelpContext = 23
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Validation Display'
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
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 592
    Height = 265
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Vaildation'
      DesignSize = (
        584
        237)
      object Label3: TLabel
        Left = 3
        Top = 27
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
      object Label2: TLabel
        Left = 0
        Top = 219
        Width = 41
        Height = 17
        Anchors = [akLeft, akBottom]
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
        Top = 227
        Width = 73
        Height = 17
        Anchors = [akLeft, akBottom]
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label1: TLabel
        Left = 120
        Top = 219
        Width = 49
        Height = 17
        Anchors = [akLeft, akBottom]
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
      object ResultValue: TLabel
        Left = 168
        Top = 219
        Width = 313
        Height = 17
        Anchors = [akLeft, akRight, akBottom]
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object ListResult: TMemo
        Left = 144
        Top = 0
        Width = 440
        Height = 217
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelInner = bvNone
        Ctl3D = False
        ParentCtl3D = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
        WordWrap = False
        OnDblClick = ListResultDblClick
      end
      object box_fix: TCheckBox
        Left = 3
        Top = 42
        Width = 137
        Height = 17
        Caption = 'Fix what can be fixed'
        TabOrder = 1
      end
      object BtnStart: TButton
        Left = 3
        Top = 2
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
        TabOrder = 2
        OnClick = BtnStartClick
      end
      object ListStatus: TListView
        Left = 3
        Top = 63
        Width = 137
        Height = 154
        Anchors = [akLeft, akTop, akBottom]
        BevelEdges = []
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Color = clBtnFace
        Columns = <>
        ColumnClick = False
        ReadOnly = True
        RowSelect = True
        ShowColumnHeaders = False
        SmallImages = FormMain.ImageList
        TabOrder = 3
        ViewStyle = vsReport
      end
      object BtnCopy: TButton
        Left = 485
        Top = 219
        Width = 99
        Height = 17
        Anchors = [akRight, akBottom]
        Caption = 'Copy To Clipboard'
        TabOrder = 4
        OnClick = BtnCopyClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Information'
      ImageIndex = 1
      object Label4: TLabel
        Left = 0
        Top = 0
        Width = 584
        Height = 237
        Align = alClient
        Alignment = taCenter
        Caption = 'work in progress :)'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold, fsItalic]
        ParentFont = False
        Layout = tlCenter
      end
    end
  end
end
