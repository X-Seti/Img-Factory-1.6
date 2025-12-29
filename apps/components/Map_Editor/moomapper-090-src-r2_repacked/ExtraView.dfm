object FormExtraView: TFormExtraView
  Left = 601
  Top = 185
  Width = 400
  Height = 400
  HelpContext = 21
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Secondary Display'
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
  OnDestroy = FormDestroy
  OnHide = FormHide
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object InfoPanel: TPanel
    Left = 0
    Top = 0
    Width = 392
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      392
      25)
    object BarDepth: TTrackBar
      Left = 0
      Top = 0
      Width = 249
      Height = 25
      Hint = 'Zoom Level'
      Anchors = [akLeft, akTop, akRight]
      LineSize = 5
      Max = 402
      Min = 2
      ParentShowHint = False
      PageSize = 5
      Frequency = 10
      Position = 52
      ShowHint = True
      TabOrder = 0
      ThumbLength = 15
      OnChange = BarDepthChange
    end
    object BoxColour: TColorBox
      Left = 276
      Top = 3
      Width = 109
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
      Anchors = [akTop, akRight]
      ItemHeight = 16
      TabOrder = 1
      OnChange = BoxColourChange
    end
    object BtnViewDFF: TButton
      Left = 248
      Top = 4
      Width = 25
      Height = 17
      Hint = 'Open External Model'
      Anchors = [akTop, akRight]
      Caption = '...'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = BtnViewDFFClick
    end
  end
  object RotatePanel: TPanel
    Left = 376
    Top = 25
    Width = 16
    Height = 348
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      16
      348)
    object BarRotation: TTrackBar
      Left = -8
      Top = 8
      Width = 25
      Height = 337
      Hint = 'Rotation Angle'
      Anchors = [akTop, akRight, akBottom]
      LineSize = 5
      Max = 0
      Min = -180
      Orientation = trVertical
      ParentShowHint = False
      PageSize = 5
      Frequency = 10
      Position = -135
      ShowHint = True
      TabOrder = 0
      ThumbLength = 15
      TickMarks = tmTopLeft
      OnChange = BarRotationChange
    end
  end
  object TimerRotate: TTimer
    Enabled = False
    Interval = 10
    OnTimer = TimerRotateTimer
    Top = 24
  end
end
