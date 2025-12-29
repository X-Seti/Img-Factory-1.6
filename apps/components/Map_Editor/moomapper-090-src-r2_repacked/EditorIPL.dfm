object FormEditorIPL: TFormEditorIPL
  Left = 735
  Top = 293
  Width = 325
  Height = 429
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Item Placement'
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
  PixelsPerInch = 96
  TextHeight = 13
  object ListFiles: TListView
    Left = 0
    Top = 29
    Width = 317
    Height = 150
    Align = alTop
    Columns = <
      item
        Caption = 'Index'
        Width = 40
      end
      item
        Caption = 'Filename'
        Width = 225
      end>
    HideSelection = False
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    PopupMenu = PopupFiles
    TabOrder = 0
    ViewStyle = vsReport
    OnColumnClick = ListFilesColumnClick
    OnData = ListFilesData
    OnSelectItem = ListFilesSelectItem
  end
  object ListItems: TListView
    Left = 0
    Top = 280
    Width = 317
    Height = 96
    Hint = 'Double click to select an object, hold shift to select multiple'
    Align = alClient
    Columns = <
      item
        Caption = 'Index'
        Width = 40
      end
      item
        Caption = 'Name'
        Width = 225
      end>
    DragKind = dkDock
    HideSelection = False
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    ParentShowHint = False
    PopupMenu = PopupItems
    ShowHint = True
    TabOrder = 1
    ViewStyle = vsReport
    OnColumnClick = ListItemsColumnClick
    OnData = ListItemsData
    OnKeyDown = ListItemsKeyDown
    OnMouseDown = ListItemsMouseDown
    OnSelectItem = ListItemsSelectItem
  end
  object RadioSection: TRadioGroup
    Left = 0
    Top = 179
    Width = 317
    Height = 72
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ItemIndex = 0
    Items.Strings = (
      'INST - Item Instances'
      'CULL - Object Culling'
      'ZONE - Map Zones'
      'PATH - Pedestrian / Car Paths (Vice)')
    ParentFont = False
    TabOrder = 2
    OnClick = RadioSectionClick
  end
  object MainToolbar: TToolBar
    Left = 0
    Top = 251
    Width = 317
    Height = 29
    ButtonWidth = 87
    Images = FormMain.ImageList
    List = True
    ParentShowHint = False
    ShowCaptions = True
    ShowHint = True
    TabOrder = 3
    Wrapable = False
    object TBEditDuplicate: TToolButton
      Left = 0
      Top = 2
      Action = ActionEditDuplicate
      Caption = '&Duplicate'
    end
    object ToolButton1: TToolButton
      Left = 87
      Top = 2
      Width = 8
      Caption = 'ToolButton1'
      ImageIndex = 6
      Style = tbsSeparator
    end
    object TBEditAdd: TToolButton
      Left = 95
      Top = 2
      Action = ActionEditAdd
    end
    object TBEditDel: TToolButton
      Left = 182
      Top = 2
      Action = ActionEditDelete
    end
  end
  object FileToolbar: TToolBar
    Left = 0
    Top = 0
    Width = 317
    Height = 29
    ButtonWidth = 64
    Images = FormMain.ImageList
    List = True
    ParentShowHint = False
    ShowCaptions = True
    ShowHint = True
    TabOrder = 4
    object TBFileOpen: TToolButton
      Left = 0
      Top = 2
      Action = ActionFileOpen
      Caption = '&Open'
    end
    object TBFileClose: TToolButton
      Left = 64
      Top = 2
      Action = ActionFileClose
      Caption = '&Close'
    end
    object TBSep: TToolButton
      Left = 128
      Top = 2
      Width = 8
      Caption = 'TBSep'
      ImageIndex = 6
      Style = tbsSeparator
    end
    object TBFileNew: TToolButton
      Left = 136
      Top = 2
      Action = ActionFileNew
      Caption = '&New'
    end
    object TBFileDelete: TToolButton
      Left = 200
      Top = 2
      Action = ActionFileDelete
      Caption = '&Delete'
    end
  end
  object PanelInstSelect: TPanel
    Left = 0
    Top = 376
    Width = 317
    Height = 26
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 5
    DesignSize = (
      317
      26)
    object BtnInstSelectAll: TButton
      Left = 0
      Top = 0
      Width = 314
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Select All'
      TabOrder = 0
      OnClick = BtnInstSelectAllClick
    end
  end
  object MainActionList: TActionList
    Images = FormMain.ImageList
    Left = 8
    Top = 56
    object ActionEditAdd: TAction
      Category = 'Edit'
      Caption = '&Add Item'
      Hint = 'Add Item'
      ImageIndex = 6
      OnExecute = ActionEditAddExecute
    end
    object ActionEditDelete: TAction
      Category = 'Edit'
      Caption = '&Delete Item'
      Hint = 'Delete Item'
      ImageIndex = 5
      ShortCut = 46
      OnExecute = ActionEditDeleteExecute
    end
    object ActionEditDuplicate: TAction
      Category = 'Edit'
      Caption = '&Duplicate Item'
      Enabled = False
      Hint = 'Duplicate Item'
      ImageIndex = 1
      OnExecute = ActionEditDuplicateExecute
    end
    object ActionFileOpen: TAction
      Category = 'File'
      Caption = '&Open IPL File'
      Hint = 'Open IPL File'
      ImageIndex = 7
      OnExecute = ActionFileOpenExecute
    end
    object ActionFileDelete: TAction
      Category = 'File'
      Caption = '&Delete File'
      Hint = 'Delete IPL File'
      ImageIndex = 5
      OnExecute = ActionFileDeleteExecute
    end
    object ActionFileNew: TAction
      Category = 'File'
      Caption = '&New IPL File'
      Hint = 'New IPL File'
      ImageIndex = 6
      OnExecute = ActionFileNewExecute
    end
    object ActionFileClose: TAction
      Category = 'File'
      Caption = '&Close IPL File'
      Hint = 'Close IPL File'
      ImageIndex = 3
      OnExecute = ActionFileCloseExecute
    end
  end
  object DlgNewFile: TSaveDialog
    DefaultExt = 'ipl'
    Filter = 'Object Instance Files (*.ipl)|*.ipl'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    OptionsEx = [ofExNoPlacesBar]
    Left = 40
    Top = 56
  end
  object DlgOpenFile: TOpenDialog
    DefaultExt = 'ipl'
    Filter = 'Object Instance Files (*.ipl)|*.ipl'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    OptionsEx = [ofExNoPlacesBar]
    Left = 72
    Top = 56
  end
  object PopupItems: TPopupMenu
    Left = 8
    Top = 288
    object PopupEditItem: TMenuItem
      Caption = '&Edit Item'
      Default = True
      OnClick = PopupEditItemClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object PopupDuplicateItem: TMenuItem
      Action = ActionEditDuplicate
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object PopupAddItem: TMenuItem
      Action = ActionEditAdd
    end
    object PopupDeleteItem: TMenuItem
      Action = ActionEditDelete
    end
  end
  object PopupFiles: TPopupMenu
    Left = 104
    Top = 56
    object PopupFileClose: TMenuItem
      Action = ActionFileClose
    end
    object PopupFileOpen: TMenuItem
      Action = ActionFileOpen
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object PopupFileNew: TMenuItem
      Action = ActionFileNew
    end
    object PopupFileDelete: TMenuItem
      Action = ActionFileDelete
    end
  end
end
