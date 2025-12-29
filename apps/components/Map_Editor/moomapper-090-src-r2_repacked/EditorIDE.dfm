object FormEditorIDE: TFormEditorIDE
  Left = 530
  Top = 306
  Width = 325
  Height = 429
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Object Definition'
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
      'OBJS - Object Definitions'
      'TOBJ - Changeable Object Definitions'
      'PATH - Pedestrian / Car Paths (GTA 3)'
      '2DFX - Lighting / Environment Effects')
    ParentFont = False
    TabOrder = 1
    OnClick = RadioSectionClick
  end
  object ListItems: TListView
    Left = 0
    Top = 280
    Width = 317
    Height = 122
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
    PopupMenu = PopupItems
    TabOrder = 2
    ViewStyle = vsReport
    OnColumnClick = ListItemsColumnClick
    OnData = ListItemsData
    OnDblClick = ListItemsDblClick
    OnKeyDown = ListItemsKeyDown
    OnSelectItem = ListItemsSelectItem
  end
  object MainToolbar: TToolBar
    Left = 0
    Top = 251
    Width = 317
    Height = 29
    ButtonWidth = 77
    Caption = 'MainToolbar'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Images = FormMain.ImageList
    List = True
    ParentFont = False
    ParentShowHint = False
    ShowCaptions = True
    ShowHint = True
    TabOrder = 3
    Wrapable = False
    object TBEditAddInstance: TToolButton
      Left = 0
      Top = 2
      Action = ActionEditAddInstance
      Caption = '&Instance'
    end
    object TBEditUseDefinition: TToolButton
      Left = 77
      Top = 2
      Action = ActionEditUseDefinition
      Caption = '&Definition'
    end
    object TBSep1: TToolButton
      Left = 154
      Top = 2
      Width = 8
      Caption = 'TBSep1'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object TBEditAdd: TToolButton
      Left = 162
      Top = 2
      Action = ActionEditAdd
      Caption = '&Add'
    end
    object TBEditDel: TToolButton
      Left = 239
      Top = 2
      Action = ActionEditDelete
      Caption = '&Delete'
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
  object MainActionList: TActionList
    Images = FormMain.ImageList
    Left = 8
    Top = 56
    object ActionFileClose: TAction
      Category = 'File'
      Caption = '&Close IDE File'
      Hint = 'Close IDE File'
      ImageIndex = 3
      OnExecute = ActionFileCloseExecute
    end
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
    object ActionEditUseDefinition: TAction
      Category = 'Edit'
      Caption = '&Use Object Definition'
      Enabled = False
      Hint = 'Use Model Definition'
      ImageIndex = 4
      OnExecute = ActionEditUseDefinitionExecute
    end
    object ActionFileNew: TAction
      Category = 'File'
      Caption = '&New IDE File'
      Hint = 'New IDE File'
      ImageIndex = 6
      OnExecute = ActionFileNewExecute
    end
    object ActionFileOpen: TAction
      Category = 'File'
      Caption = '&Open IDE File'
      Hint = 'Open IDE File'
      ImageIndex = 7
      OnExecute = ActionFileOpenExecute
    end
    object ActionFileDelete: TAction
      Category = 'File'
      Caption = '&Delete IDE File'
      Hint = 'Delete IDE File'
      ImageIndex = 5
      OnExecute = ActionFileDeleteExecute
    end
    object ActionEditAddInstance: TAction
      Category = 'Edit'
      Caption = '&Add Item Instance'
      Enabled = False
      Hint = 'Add Instance Of Definition'
      ImageIndex = 1
      OnExecute = ActionEditAddInstanceExecute
    end
  end
  object DlgNewFile: TSaveDialog
    DefaultExt = 'ide'
    Filter = 'Object Definition Files (*.ide)|*.ide'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    OptionsEx = [ofExNoPlacesBar]
    Left = 40
    Top = 56
  end
  object DlgOpenFile: TOpenDialog
    DefaultExt = 'ide'
    Filter = 'Object Instance Files (*.ide)|*.ide'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    OptionsEx = [ofExNoPlacesBar]
    Left = 72
    Top = 56
  end
  object PopupItems: TPopupMenu
    Left = 8
    Top = 304
    object PopupEditItem: TMenuItem
      Caption = '&Edit Item'
      Default = True
      OnClick = PopupEditItemClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object PopupAddInstance: TMenuItem
      Action = ActionEditAddInstance
    end
    object PopupUseDefinition: TMenuItem
      Action = ActionEditUseDefinition
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
