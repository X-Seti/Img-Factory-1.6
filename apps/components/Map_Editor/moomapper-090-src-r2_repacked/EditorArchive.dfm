object FormEditorArchive: TFormEditorArchive
  Left = 682
  Top = 173
  Width = 325
  Height = 600
  HorzScrollBar.Visible = False
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'IMG Archive'
  Color = clBtnFace
  DragKind = dkDock
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
  object ListArchive: TListView
    Left = 0
    Top = 29
    Width = 317
    Height = 544
    Align = alClient
    Columns = <
      item
        Caption = 'Index'
        Width = 40
      end
      item
        Caption = 'Filename'
        Width = 125
      end
      item
        Caption = 'Type'
        Width = 60
      end
      item
        Caption = 'Size'
        Width = 42
      end>
    HideSelection = False
    MultiSelect = True
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnColumnClick = ListArchiveColumnClick
    OnData = ListArchiveData
    OnSelectItem = ListArchiveSelectItem
  end
  object TBArchive: TToolBar
    Left = 0
    Top = 0
    Width = 317
    Height = 29
    ButtonWidth = 66
    Caption = 'TBArchive'
    Images = FormMain.ImageList
    List = True
    ParentShowHint = False
    ShowCaptions = True
    ShowHint = True
    TabOrder = 1
    object TBEditExtract: TToolButton
      Left = 0
      Top = 2
      Hint = 'Extract Item'
      Action = ActionEditExtract
      Caption = '&Extract'
    end
    object TBSep1: TToolButton
      Left = 66
      Top = 2
      Width = 8
      Caption = 'TBSep1'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object TBEditInsert: TToolButton
      Left = 74
      Top = 2
      Hint = 'Insert Item'
      Action = ActionEditInsert
      Caption = '&Insert'
    end
    object TBEditDelete: TToolButton
      Left = 140
      Top = 2
      Hint = 'Delete Item'
      Action = ActionEditDelete
      Caption = '&Delete'
    end
  end
  object MainActionList: TActionList
    Images = FormMain.ImageList
    Left = 8
    Top = 56
    object ActionEditDelete: TAction
      Category = 'Edit'
      Caption = '&Delete Selected Items'
      ImageIndex = 5
      OnExecute = ActionEditDeleteExecute
    end
    object ActionEditInsert: TAction
      Category = 'Edit'
      Caption = '&Insert Selected Items'
      ImageIndex = 7
      OnExecute = ActionEditInsertExecute
    end
    object ActionEditExtract: TAction
      Category = 'Edit'
      Caption = '&Extract Selected Items'
      ImageIndex = 1
      OnExecute = ActionEditExtractExecute
    end
    object ActionEditInsertDff: TAction
      Category = 'Edit'
      Caption = '&Insert Selected Items'
      ImageIndex = 7
      OnExecute = ActionEditInsertDffExecute
    end
    object ActionEditInsertTxd: TAction
      Category = 'Edit'
      Caption = '&Insert Selected Items'
      ImageIndex = 7
      OnExecute = ActionEditInsertTxdExecute
    end
  end
  object DlgAdd: TOpenDialog
    Filter = 'Supported Formats (*.txd, *.dff)|*.TXD;*.DFF|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select File(s) To Add To Archive'
    Left = 40
    Top = 56
  end
  object PopupMenu1: TPopupMenu
    Left = 72
    Top = 56
    object PopupExtract: TMenuItem
      Action = ActionEditExtract
      Default = True
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object PopupInsert: TMenuItem
      Action = ActionEditInsert
    end
    object PopupDelete: TMenuItem
      Action = ActionEditDelete
    end
  end
end
