unit EditorItem;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, StdCtrls, ExtCtrls, RequiredTypes, Buttons, ComCtrls,
  CheckLst, bitunit, ImgList, Math; // geometry

const
  DEC_NORMAL = 0.2;
  DEC_FAST = 2;

  VALIDATION_IMAGE_UNKNOWN = 24;
  VALIDATION_IMAGE_GOOD = 27;
  VALIDATION_IMAGE_BAD = 26;

  TIMERMODE_OFF = 0;
  TIMERMODE_INST_POSX = 1;
  TIMERMODE_INST_POSY = 2;
  TIMERMODE_INST_POSZ = 3;
  TIMERMODE_INST_ROTX = 4;
  TIMERMODE_INST_ROTY = 5;
  TIMERMODE_INST_ROTZ = 6;
  TIMERMODE_MULT_INST_POSX = 7;
  TIMERMODE_MULT_INST_POSY = 8;
  TIMERMODE_MULT_INST_POSZ = 9;

type
  TFormEditorItem = class(TForm)
    InfoPanel: TPanel;
    ExpandedEdit: TEdit;
    ModeLabel: TLabel;
    ModeValue: TLabel;
    FileIndexLabel: TLabel;
    FileIndexValue: TLabel;
    MoveTimer: TTimer;
    ImageList1: TImageList;
    PageControl1: TPageControl;
    panelideobjs: TTabSheet;
    paneliplinst: TTabSheet;
    panelidetobj: TTabSheet;
    paneliplmultinst: TTabSheet;
    paneliplpath: TTabSheet;
    paneliplzone: TTabSheet;
    paneliplcull: TTabSheet;
    panelide2dfx: TTabSheet;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    EditIDEObjsID: TEdit;
    EditIDEObjsModel: TEdit;
    EditIDEObjsTexture: TEdit;
    EditIDEObjsU4: TEdit;
    EditIDEObjsLOD: TEdit;
    EditIDEObjsFlags: TEdit;
    BtnIDEObjsModel: TButton;
    BtnIDEObjsTexture: TButton;
    ide_bits: TCheckListBox;
    PaintBox1: TPaintBox;
    PaintBox2: TPaintBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    EditIPLInstID: TEdit;
    EditIPLInstInterior: TEdit;
    EditIPLInstPosX: TEdit;
    EditIPLInstScaleX: TEdit;
    EditIPLInstRotX: TEdit;
    EditIPLInstPosY: TEdit;
    EditIPLInstPosZ: TEdit;
    EditIPLInstScaleY: TEdit;
    EditIPLInstScaleZ: TEdit;
    EditIPLInstRotY: TEdit;
    EditIPLInstRotZ: TEdit;
    EditIPLInstModel: TEdit;
    EditIPLInstHasInterior: TCheckBox;
    Label35: TLabel;
    Label36: TLabel;
    EditIDETObjTimeOn: TComboBox;
    EditIDETObjTimeOff: TComboBox;
    BtnIDETObjModel: TButton;
    BtnIDETObjTexture: TButton;
    Label58: TLabel;
    Label59: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    EditIPLMultInstItemCount: TLabel;
    EditIPLMultInstItemCenter: TLabel;
    Label57: TLabel;
    EditIPLMultInstPosXL1: TPanel;
    EditIPLMultInstPosXL2: TPanel;
    EditIPLMultInstPosXR2: TPanel;
    EditIPLMultInstPosXR1: TPanel;
    EditIPLMultInstPosYL1: TPanel;
    EditIPLMultInstPosYL2: TPanel;
    EditIPLMultInstPosYR2: TPanel;
    EditIPLMultInstPosYR1: TPanel;
    EditIPLMultInstPosCenter: TPanel;
    EditIPLMultInstPosZL1: TPanel;
    EditIPLMultInstPosZL2: TPanel;
    EditIPLMultInstPosZR2: TPanel;
    EditIPLMultInstPosZR1: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel17: TPanel;
    Panel18: TPanel;
    Panel19: TPanel;
    Panel20: TPanel;
    Panel21: TPanel;
    Panel22: TPanel;
    Panel23: TPanel;
    Panel24: TPanel;
    Panel25: TPanel;
    Panel26: TPanel;
    EditIPLMultInstPosX: TEdit;
    EditIPLMultInstPosY: TEdit;
    EditIPLMultInstPosZ: TEdit;
    EditIPLMultInstList: TListView;
    Label65: TLabel;
    EditIPLPathItems: TListView;
    EditIPLPathOther: TEdit;
    PanelIPLPathItem: TPanel;
    Label67: TLabel;
    EditIPLPathItemLL0: TSpeedButton;
    EditIPLPathItemLL1: TSpeedButton;
    EditIPLPathItemLL2: TSpeedButton;
    EditIPLPathItemLR0: TSpeedButton;
    EditIPLPathItemLR1: TSpeedButton;
    EditIPLPathItemLR2: TSpeedButton;
    EditIPLPathItemTypeNone: TSpeedButton;
    EditIPLPathItemTypeMid: TSpeedButton;
    EditIPLPathItemTypeEnd: TSpeedButton;
    Label70: TLabel;
    EditIPLPathItemPosX: TEdit;
    EditIPLPathItemPosY: TEdit;
    EditIPLPathItemPosZ: TEdit;
    EditIPLPathItemU3: TEdit;
    EditIPLPathItemU7: TEdit;
    EditIPLPathItemConnect: TEdit;
    EditIPLPathItemU10: TEdit;
    EditIPLPathItemU11: TEdit;
    EditIPLPathItemU12: TEdit;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    EditIPLZoneName: TEdit;
    EditIPLZoneU9: TEdit;
    EditIPLZonePos1X: TEdit;
    EditIPLZonePos1Y: TEdit;
    EditIPLZonePos1Z: TEdit;
    EditIPLZonePos2X: TEdit;
    EditIPLZonePos2Y: TEdit;
    EditIPLZonePos2Z: TEdit;
    Label43: TLabel;
    Label44: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    EditIPLCullPos1X: TEdit;
    EditIPLCullPos1Y: TEdit;
    EditIPLCullPos1Z: TEdit;
    EditIPLCullU10: TEdit;
    EditIPLCullU11: TEdit;
    EditIPLCullPos2X: TEdit;
    EditIPLCullPos2Y: TEdit;
    EditIPLCullPos2Z: TEdit;
    EditIPLCullPos3X: TEdit;
    EditIPLCullPos3Y: TEdit;
    EditIPLCullPos3Z: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label63: TLabel;
    PanelIDE2dfxLight: TPanel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label14: TLabel;
    Label62: TLabel;
    EditIDE2dfxAEffect1: TEdit;
    EditIDE2dfxAEffect2: TEdit;
    EditIDE2dfxADistance: TEdit;
    EditIDE2dfxASizeLamp: TEdit;
    EditIDE2dfxASizeCorona: TEdit;
    EditIDE2dfxAReflectionWet: TEdit;
    EditIDE2dfxALensFlare: TEdit;
    EditIDE2dfxADust: TEdit;
    EditIDE2dfxARangeOuter: TEdit;
    EditIDE2dfxARangeInner: TEdit;
    EditIDE2dfxAControl: TComboBox;
    PanelIDE2dfxParticle: TPanel;
    Label18: TLabel;
    Label19: TLabel;
    EditIDE2dfxBRotX: TEdit;
    EditIDE2dfxBRotZ: TEdit;
    EditIDE2dfxBRotY: TEdit;
    EditIDE2dfxBRotW: TEdit;
    EditIDE2dfxBType: TComboBox;
    PanelIDE2dfxAnimation: TPanel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    EditIDE2dfxDDir1X: TEdit;
    EditIDE2dfxDDir1Z: TEdit;
    EditIDE2dfxDDir1Y: TEdit;
    EditIDE2dfxDDir2X: TEdit;
    EditIDE2dfxDDir2Y: TEdit;
    EditIDE2dfxDDir2Z: TEdit;
    EditIDE2dfxDType: TComboBox;
    PanelIDE2dfxReflection: TPanel;
    EditIDE2dfxID: TEdit;
    EditIDE2dfxPosX: TEdit;
    EditIDE2dfxPosY: TEdit;
    EditIDE2dfxPosZ: TEdit;
    EditIDE2dfxColR: TEdit;
    EditIDE2dfxColG: TEdit;
    EditIDE2dfxColB: TEdit;
    EditIDE2dfxColChooser: TColorBox;
    EditIDE2dfxRadioLight: TRadioButton;
    EditIDE2dfxRadioParticle: TRadioButton;
    EditIDE2dfxRadioUnknown: TRadioButton;
    EditIDE2dfxRadioAnimation: TRadioButton;
    EditIDE2dfxRadioReflection: TRadioButton;
    EditIDE2dfxViewDistance: TEdit;
    PanelIDE2dfxUnknown: TPanel;
    panelidepath: TTabSheet;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    EditIDEPathID: TEdit;
    EditIDEPathModel: TEdit;
    EditIDEPathRadioPed: TRadioButton;
    EditIDEPathRadioCar: TRadioButton;
    EditIDEPathItems: TListView;
    PanelIDEPathItem: TPanel;
    Label53: TLabel;
    EditIDEPathItemLL0: TSpeedButton;
    EditIDEPathItemLL1: TSpeedButton;
    EditIDEPathItemLL2: TSpeedButton;
    EditIDEPathItemLR0: TSpeedButton;
    EditIDEPathItemLR1: TSpeedButton;
    EditIDEPathItemLR2: TSpeedButton;
    Label54: TLabel;
    EditIDEPathItemTypeNone: TSpeedButton;
    EditIDEPathItemTypeMid: TSpeedButton;
    EditIDEPathItemTypeEnd: TSpeedButton;
    Label55: TLabel;
    Label56: TLabel;
    EditIDEPathItemPosX: TEdit;
    EditIDEPathItemPosY: TEdit;
    EditIDEPathItemPosZ: TEdit;
    EditIDEPathItemU3: TEdit;
    EditIDEPathItemU7: TEdit;
    EditIDEPathItemConnect: TEdit;
    Label66: TLabel;
    Label68: TLabel;
    Label69: TLabel;
    Label71: TLabel;
    Label72: TLabel;
    Label73: TLabel;
    EditIDETObjID: TEdit;
    EditIDETObjModel: TEdit;
    EditIDETObjTexture: TEdit;
    EditIDETObjU4: TEdit;
    EditIDETObjLOD: TEdit;
    EditIDETObjFlags: TEdit;
    CheckListBox1: TCheckListBox;
    PaintBox3: TPaintBox;
    PaintBox4: TPaintBox;
    EditIPLInstPosZR1: TSpeedButton;
    EditIPLInstPosZR2: TSpeedButton;
    EditIPLInstPosZL2: TSpeedButton;
    EditIPLInstPosZL1: TSpeedButton;
    EditIPLInstPosYR1: TSpeedButton;
    EditIPLInstPosYR2: TSpeedButton;
    EditIPLInstPosYL1: TSpeedButton;
    EditIPLInstPosYL2: TSpeedButton;
    EditIPLInstPosXR1: TSpeedButton;
    EditIPLInstPosXR2: TSpeedButton;
    EditIPLInstPosXL1: TSpeedButton;
    EditIPLInstPosXL2: TSpeedButton;
    EditIPLInstRotXL1: TSpeedButton;
    EditIPLInstRotXL2: TSpeedButton;
    EditIPLInstRotXR2: TSpeedButton;
    EditIPLInstRotXR1: TSpeedButton;
    EditIPLInstRotYL1: TSpeedButton;
    EditIPLInstRotYL2: TSpeedButton;
    EditIPLInstRotYR2: TSpeedButton;
    EditIPLInstRotYR1: TSpeedButton;
    EditIPLInstRotZL1: TSpeedButton;
    EditIPLInstRotZL2: TSpeedButton;
    EditIPLInstRotZR2: TSpeedButton;
    EditIPLInstRotZR1: TSpeedButton;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    EditIPLInstValidation: TListView;
    box_zonetypes: TComboBox;
    boxpathtype: TRadioGroup;
    BitBtn3: TBitBtn;
    BtnUndo: TBitBtn;
    BtnCancel: TBitBtn;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnUndoClick(Sender: TObject);
    procedure EditIPLInstIDChange(Sender: TObject);
    procedure EditIPLInstModelChange(Sender: TObject);
    procedure EditIPLInstInteriorChange(Sender: TObject);
    procedure EditIPLInstHasInteriorClick(Sender: TObject);
    procedure EditIPLInstRotXChange(Sender: TObject);
    procedure EditIPLInstRotYChange(Sender: TObject);
    procedure EditIPLInstRotZChange(Sender: TObject);
    procedure EditIPLInstPosXChange(Sender: TObject);
    procedure EditIPLInstPosYChange(Sender: TObject);
    procedure EditIPLInstPosZChange(Sender: TObject);
    procedure EditIPLInstScaleXChange(Sender: TObject);
    procedure EditIPLInstScaleYChange(Sender: TObject);
    procedure EditIPLInstScaleZChange(Sender: TObject);
    procedure EditIDE2dfxIDChange(Sender: TObject);
    procedure EditIDE2dfxColRChange(Sender: TObject);
    procedure EditIDE2dfxColGChange(Sender: TObject);
    procedure EditIDE2dfxColBChange(Sender: TObject);
    procedure EditIDE2dfxPosXChange(Sender: TObject);
    procedure EditIDE2dfxPosYChange(Sender: TObject);
    procedure EditIDE2dfxPosZChange(Sender: TObject);
    procedure EditIDE2dfxColChooserChange(Sender: TObject);
    procedure EditIDE2dfxRadioLightClick(Sender: TObject);
    procedure EditIDE2dfxRadioParticleClick(Sender: TObject);
    procedure EditIDE2dfxBRotXChange(Sender: TObject);
    procedure EditIDE2dfxBRotYChange(Sender: TObject);
    procedure EditIDE2dfxBRotZChange(Sender: TObject);
    procedure EditIDE2dfxBRotWChange(Sender: TObject);
    procedure EditIDE2dfxAEffect1Change(Sender: TObject);
    procedure EditIDE2dfxAEffect2Change(Sender: TObject);
    procedure EditIDE2dfxADistanceChange(Sender: TObject);
    procedure EditIDE2dfxAReflectionWetChange(Sender: TObject);
    procedure EditIDE2dfxALensFlareChange(Sender: TObject);
    procedure EditIDE2dfxADustChange(Sender: TObject);
    procedure EditIDE2dfxASizeLampChange(Sender: TObject);
    procedure EditIDE2dfxASizeCoronaChange(Sender: TObject);
    procedure EditIDE2dfxARangeOuterChange(Sender: TObject);
    procedure EditIDE2dfxARangeInnerChange(Sender: TObject);
    procedure EditIDE2dfxAControlChange(Sender: TObject);
    procedure EditIDETObjIDChange(Sender: TObject);
    procedure EditIDETObjModelChange(Sender: TObject);
    procedure EditIDETObjTextureChange(Sender: TObject);
    procedure EditIDETObjU4Change(Sender: TObject);
    procedure EditIDETObjLODChange(Sender: TObject);
    procedure EditIDETObjFlagsChange(Sender: TObject);
    procedure EditIDETObjTimeOnChange(Sender: TObject);
    procedure EditIDETObjTimeOffChange(Sender: TObject);
    procedure EditIDEObjsIDChange(Sender: TObject);
    procedure EditIDEObjsModelChange(Sender: TObject);
    procedure EditIDEObjsTextureChange(Sender: TObject);
    procedure EditIDEObjsU4Change(Sender: TObject);
    procedure EditIDEObjsLODChange(Sender: TObject);
    procedure EditIDEObjsFlagsChange(Sender: TObject);
    procedure EditIPLZoneNameChange(Sender: TObject);
    procedure EditIPLZoneU9Change(Sender: TObject);
    procedure EditIPLZonePos1XChange(Sender: TObject);
    procedure EditIPLZonePos1YChange(Sender: TObject);
    procedure EditIPLZonePos1ZChange(Sender: TObject);
    procedure EditIPLZonePos2XChange(Sender: TObject);
    procedure EditIPLZonePos2YChange(Sender: TObject);
    procedure EditIPLZonePos2ZChange(Sender: TObject);
    procedure MoveTimerTimer(Sender: TObject);
    procedure EditIPLInstPosXL1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLInstPosXL2MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLInstPosXR2MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLInstPosXR1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLInstPosYL1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditIPLInstPosYL2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditIPLInstPosYR2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditIPLInstPosYR1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditIPLInstPosZL1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditIPLInstPosZL2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditIPLInstPosZR2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditIPLInstPosZR1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditIPLInstRotXL1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLInstRotYL1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLInstRotZL1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLInstRotXL2MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLInstRotYL2MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLInstRotZL2MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLInstRotXR2MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLInstRotYR2MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLInstRotZR2MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLInstRotXR1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLInstRotYR1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLInstRotZR1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLInstRotZR2MouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIDE2dfxDDir1XChange(Sender: TObject);
    procedure EditIDE2dfxDDir1YChange(Sender: TObject);
    procedure EditIDE2dfxDDir1ZChange(Sender: TObject);
    procedure EditIDE2dfxDDir2XChange(Sender: TObject);
    procedure EditIDE2dfxDDir2YChange(Sender: TObject);
    procedure EditIDE2dfxDDir2ZChange(Sender: TObject);
    procedure EditIDEPathRadioPedClick(Sender: TObject);
    procedure EditIDEPathRadioCarClick(Sender: TObject);
    procedure EditIDEPathIDChange(Sender: TObject);
    procedure EditIDEPathModelChange(Sender: TObject);
    procedure EditIDEPathItemsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure EditIDEPathItemTypeNoneClick(Sender: TObject);
    procedure EditIDEPathItemTypeMidClick(Sender: TObject);
    procedure EditIDEPathItemTypeEndClick(Sender: TObject);
    procedure EditIDEPathItemConnectChange(Sender: TObject);
    procedure EditIDEPathItemU3Change(Sender: TObject);
    procedure EditIDEPathItemU7Change(Sender: TObject);
    procedure EditIDEPathItemPosXChange(Sender: TObject);
    procedure EditIDEPathItemPosYChange(Sender: TObject);
    procedure EditIDEPathItemPosZChange(Sender: TObject);
    procedure EditIPLMultInstPosXChange(Sender: TObject);
    procedure EditIPLMultInstPosYChange(Sender: TObject);
    procedure EditIPLMultInstPosZChange(Sender: TObject);
    procedure EditIPLMultInstPosXL1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditIPLMultInstPosYL1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLMultInstPosZL1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLMultInstPosXL2MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLMultInstPosYL2MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLMultInstPosZL2MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLMultInstPosXR2MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLMultInstPosYR2MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLMultInstPosZR2MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLMultInstPosXR1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLMultInstPosYR1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLMultInstPosZR1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLMultInstPosXL1MouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditIPLMultInstPosXKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditIPLMultInstPosYKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditIPLMultInstPosZKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditIPLMultInstPosCenterClick(Sender: TObject);
    procedure BtnIDETObjModelClick(Sender: TObject);
    procedure BtnIDETObjTextureClick(Sender: TObject);
    procedure BtnIDEObjsTextureClick(Sender: TObject);
    procedure BtnIDEObjsModelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EditIDE2dfxRadioAnimationClick(Sender: TObject);
    procedure EditIDE2dfxRadioReflectionClick(Sender: TObject);
    procedure EditIDE2dfxViewDistanceChange(Sender: TObject);
    procedure EditIDE2dfxDTypeChange(Sender: TObject);
    procedure EditIDE2dfxBTypeChange(Sender: TObject);
    procedure EditIPLPathItemsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure EditIPLPathItemTypeNoneClick(Sender: TObject);
    procedure EditIPLPathItemConnectChange(Sender: TObject);
    procedure EditIPLPathItemU3Change(Sender: TObject);
    procedure EditIPLPathItemU10Change(Sender: TObject);
    procedure EditIPLPathItemU11Change(Sender: TObject);
    procedure EditIPLPathItemU12Change(Sender: TObject);
    procedure EditIPLPathItemU7Change(Sender: TObject);
    procedure EditIPLPathItemPosXChange(Sender: TObject);
    procedure EditIPLPathOtherChange(Sender: TObject);
    procedure EditIPLPathRadioCarClick(Sender: TObject);
    procedure ide_bitsClickCheck(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure CheckListBox1ClickCheck(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure box_zonetypesChange(Sender: TObject);
    procedure boxpathtypeClick(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure EditIPLPathItemLL0Click(Sender: TObject);
    procedure EditIPLPathItemLR0Click(Sender: TObject);
  private
    MainAction: TAction;
    TimerMode: Word;
    TimerAmount: single;

    AlreadyChanged, EditorValid, ManualChange: Boolean;
    CurrentFileMode: ShortInt;
    CurrentSection: ShortInt;

    CurrentFileIndex: LongWord;
    CurrentItemIndex: LongWord;

    FirstShow: Boolean;

    UndoStr: String;

    procedure UpdateData(ChangePanels, DoRepaint: Boolean);
    procedure FileChanged(inFile: LongWord);
    procedure PerformUndo;

    procedure EditIDEPathDrawlist;
    procedure EditIPLPathDrawlist;

    procedure PerformIPLInstValidation;
    procedure PerformIDEObjsValidation;
    procedure PerformIDETObjValidation;
  public
    constructor Create(AOwner: TComponent); override;

    procedure SetToNone;
    procedure SetToMode(inFileMode, inSection: integer; inFile, inIndex: LongWord);

    procedure JumpToFile(Fileindex, Section, subsection, node: LongWord);

    procedure ActivatePanel(inFileMode, inSection: ShortInt);

    procedure Update_Euler_angles;
    procedure Use_Euler_angles;

    function GetPos: TVector3f;
    function GetRot: TVector4f;
    procedure MoveByAmount(InAmount: TVector3f);
    procedure MoveToPos(InPos: TVector3f);
    procedure RotateToPos(InRot: TVector4f);

    procedure UpdateIPLDefinition(inID: LongInt; inModelName: String);

    procedure SetAction(inAction: TAction);
  end;

implementation

uses GTAText, GLView, Main, Validate, U_imgdlg;

{$R *.dfm}

constructor TFormEditorItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  EditorValid := False;
  CurrentFileMode := 0;
  CurrentSection := 0;
  CurrentFileIndex := 0;
  CurrentItemIndex := 0;
  UpdateData(True, True);

  BtnIDEObjsModel.Enabled := not GArchive.ReadOnly;
  BtnIDEObjsTexture.Enabled := not GArchive.ReadOnly;
  BtnIDETObjModel.Enabled := not GArchive.ReadOnly;
  BtnIDETObjTexture.Enabled := not GArchive.ReadOnly;
end;

procedure TFormEditorItem.UpdateData(ChangePanels, DoRepaint: Boolean);
var
  TempStr, FullStr: String;
begin
TempStr:= '';
FullStr:= '';

  if EditorValid then
  begin
    TempStr := 'N/A'; FullStr := 'N/A';

    case CurrentFileMode of
      FILE_IDE:
      begin
        case CurrentSection of
          SECTION_OBJS:
          begin
            TempStr := 'IDE - Objs';
            FullStr := TIDEFile(GFiles.Item[CurrentFileIndex]).Objs.SaveItem(CurrentItemIndex);
          end;
          SECTION_TOBJ:
          begin
            TempStr := 'IDE - TObj';
            FullStr := TIDEFile(GFiles.Item[CurrentFileIndex]).TObj.SaveItem(CurrentItemIndex);
          end;
          SECTION_PATH:
          begin
            TempStr := 'IDE - Path';
            FullStr := TIDEFile(GFiles.Item[CurrentFileIndex]).Path.SaveItem(CurrentItemIndex);
          end;
          SECTION_2DFX:
          begin
            TempStr := 'IDE - 2dfx';
            FullStr := TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.SaveItem(CurrentItemIndex);
          end;
        end;
      end;

      FILE_IPL:
      begin
        case CurrentSection of
          SECTION_MULT_INST:
          begin
            TempStr := 'IPL - Inst(s)';
            FullStr := 'N/A';
          end;
          SECTION_INST:
          begin
            TempStr := 'IPL - Inst';
            FullStr := TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.SaveItem(CurrentItemIndex);
          end;
          SECTION_CULL:
          begin
            TempStr := 'IPL - Cull';
            FullStr := TIPLFile(GFiles.Item[CurrentFileIndex]).Cull.SaveItem(CurrentItemIndex);
          end;
          SECTION_ZONE:
          begin
            TempStr := 'IPL - Zone';
            FullStr := TIPLFile(GFiles.Item[CurrentFileIndex]).Zone.SaveItem(CurrentItemIndex);
          end;
          SECTION_PATH:
          begin
            TempStr := 'IPL - Path';
            FullStr := TIPLFile(GFiles.Item[CurrentFileIndex]).Path.SaveItem(CurrentItemIndex);
          end;
        end;
      end;
    end;

    if ChangePanels then
    begin
      ManualChange := False;
      ActivatePanel(CurrentFileMode, CurrentSection);
      ManualChange := True;
      DoRepaint := True;
    end else
      FileChanged(CurrentFileIndex);

    if DoRepaint then
    begin
      case GFiles.Item[CurrentFileIndex].SubType of
        FILE_IDE:
        begin
          FormIDE.ListFiles.Repaint;
          FormIDE.ListItems.Repaint;
        end;

        FILE_IPL:
        begin
          FormIPL.ListFiles.Repaint;
          FormIPL.ListItems.Repaint;
        end;
      end;
    end;

    ModeValue.Caption := TempStr;
    FileIndexValue.Caption := GFiles.Item[CurrentFileIndex].Name;
    ExpandedEdit.Text := FullStr;
  end else
  begin
    ActivatePanel(-1, -1);

    ModeValue.Caption := 'N/A';
    FileIndexValue.Caption := 'N/A';
    ExpandedEdit.Text := '';
  end;
end;

procedure TFormEditorItem.PerformIPLInstValidation;
var
  ObjT, ObjectFound, CorrectModel, ModelExists, TextureExists: Boolean;
  ListItem: TListItem;
  J, K: LongWord;
  Index, ObjF, ObjI: LongInt;
begin
  if (EditorValid) and (CurrentFileMode = FILE_IPL) and (CurrentSection = SECTION_INST) then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex] do
    begin
      if not (RemFile = -1) and not (RemIndex = -1) and
             (LongWord(RemFile) < Main.GFiles.Count) and
             (((RemTimed = -1) and (LongWord(RemIndex) < TIDEFile(Main.GFiles.Item[RemFile]).Objs.Count) and
             (TIDEFile(Main.GFiles.Item[RemFile]).Objs.Item[RemIndex].ID = ID)) or
             (not (RemTimed = -1) and (LongWord(RemIndex) < TIDEFile(Main.GFiles.Item[RemFile]).TObj.Count) and
             (TIDEFile(Main.GFiles.Item[RemFile]).TObj.Item[RemIndex].ID = ID))) then
      begin
        ObjectFound := True;
        ObjF := RemFile; ObjI := RemIndex; ObjT := not (RemTimed = -1);
      end else
      begin
        ObjectFound := False;
        ObjF := -1; ObjI := -1; ObjT := False;
            if (Main.GFiles.Count > 0) then
            begin
              J := 0;
              while (J < Main.GFiles.Count) do
              begin
                K := 0;
                if (Main.GFiles.Item[J].SubType = FILE_IDE) and
                   (TIDEFile(Main.GFiles.Item[J]).Objs.Count > 0) then
                  if (CompareText(Main.GFiles.Item[J].Name, ChangeFileExt(Main.GFiles.Item[CurrentFileIndex].Name, 'IDE')) = 0) then
                    if not (TIDEFile(Main.GFiles.Item[J]).Objs.ObjectList = nil) then
                    begin
                      Index := Validate.BinarySearchObjs(ID, TIDEFile(Main.GFiles.Item[J]).Objs.ObjectList);
                      if not (Index = -1) then
                      begin
                        ObjF := J;
                        ObjI := TGTASObjsObj(TIDEFile(Main.GFiles.Item[J]).Objs.ObjectList.Items[Index]^).SortVal;
                        ObjT := False;
                        ObjectFound := True;
                      end;
                    end else
                    begin
                      while (K < TIDEFile(Main.GFiles.Item[J]).Objs.Count) do
                      begin
                        if (TIDEFile(Main.GFiles.Item[J]).Objs.Item[K].ID = ID) then
                        begin
                          ObjF := J;
                          ObjI := K;
                          K := TIDEFile(Main.GFiles.Item[J]).Objs.Count;
                          ObjectFound := True;
                        end;
                        Inc(K);
                      end;
                    end;
                K := 0;
                if not ObjectFound and (Main.GFiles.Item[J].SubType = FILE_IDE) and
                   (TIDEFile(Main.GFiles.Item[J]).TObj.Count > 0) then
                  if (CompareText(Main.GFiles.Item[J].Name, ChangeFileExt(Main.GFiles.Item[CurrentFileIndex].Name, 'IDE')) = 0) then
                    while (K < TIDEFile(Main.GFiles.Item[J]).TObj.Count) do
                    begin
                      if (TIDEFile(Main.GFiles.Item[J]).TObj.Item[K].ID = ID) then
                      begin
                        ObjF := J;
                        ObjI := K;
                        K := TIDEFile(Main.GFiles.Item[J]).TObj.Count;
                        ObjectFound := True;
                      end;
                      Inc(K);
                    end;
                if ObjectFound then
                  J := Main.GFiles.Count;
                Inc(J);
              end;

              J := 0;
              if (RemFile = -1) then while (J < Main.GFiles.Count) do
              begin
                K := 0;
                if (Main.GFiles.Item[J].SubType = FILE_IDE) and
                   (TIDEFile(Main.GFiles.Item[J]).Objs.Count > 0) then
                  if not (CompareText(Main.GFiles.Item[J].Name, ChangeFileExt(Main.GFiles.Item[CurrentFileIndex].Name, 'IDE')) = 0) then
                    if not (TIDEFile(Main.GFiles.Item[J]).Objs.ObjectList = nil) then
                    begin
                      Index := Validate.BinarySearchObjs(ID, TIDEFile(Main.GFiles.Item[J]).Objs.ObjectList);
                      if not (Index = -1) then
                      begin
                        ObjF := J;
                        ObjI := TGTASObjsObj(TIDEFile(Main.GFiles.Item[J]).Objs.ObjectList.Items[Index]^).SortVal;
                        ObjT := False;
                        ObjectFound := True;
                      end;
                    end else
                    begin
                      while (K < TIDEFile(Main.GFiles.Item[J]).Objs.Count) do
                      begin
                        if (TIDEFile(Main.GFiles.Item[J]).Objs.Item[K].ID = ID) then
                        begin
                          ObjF := J;
                          ObjI := K;
                          K := TIDEFile(Main.GFiles.Item[J]).Objs.Count;
                          ObjectFound := True;
                        end;
                        Inc(K);
                      end;
                    end;
                K := 0;
                if not ObjectFound and (Main.GFiles.Item[J].SubType = FILE_IDE) and
                   (TIDEFile(Main.GFiles.Item[J]).TObj.Count > 0) then
                  if not (CompareText(Main.GFiles.Item[J].Name, ChangeFileExt(Main.GFiles.Item[CurrentFileIndex].Name, 'IDE')) = 0) then
                    while (K < TIDEFile(Main.GFiles.Item[J]).TObj.Count) do
                    begin
                      if (TIDEFile(Main.GFiles.Item[J]).TObj.Item[K].ID = ID) then
                      begin
                        ObjF := J;
                        ObjI := K;
                        K := TIDEFile(Main.GFiles.Item[J]).TObj.Count;
                        ObjectFound := True;
                      end;
                      Inc(K);
                    end;
                if ObjectFound then
                  J := Main.GFiles.Count;
                Inc(J);
              end;
            end;
      end;

      if ObjectFound then
      begin
        if ObjT then
        begin
          CorrectModel := (CompareText(TIDEFile(Main.GFiles.Item[ObjF]).TObj.Item[ObjI].ModelName, ModelName) = 0);
          TextureExists := Main.GArchive.TxdExists(TIDEFile(Main.GFiles.Item[ObjF]).TObj.Item[ObjI].TextureName);
        end else
        begin
          CorrectModel := (CompareText(TIDEFile(Main.GFiles.Item[ObjF]).Objs.Item[ObjI].ModelName, ModelName) = 0);
          TextureExists := Main.GArchive.TxdExists(TIDEFile(Main.GFiles.Item[ObjF]).Objs.Item[ObjI].TextureName);
        end;
      end else
      begin
        CorrectModel := False;
        TextureExists := False;
      end;
      ModelExists := Main.GArchive.DffExists(ModelName);

    end;

    EditIPLInstValidation.Items.Clear;

    ListItem := EditIPLInstValidation.Items.Add;
    ListItem.Caption := 'Associated Object Definition';
    if ObjectFound then
    begin
      ListItem.ImageIndex := VALIDATION_IMAGE_GOOD;
      ListItem.SubItems.Add('Found');
    end else
    begin
      ListItem.ImageIndex := VALIDATION_IMAGE_BAD;
      ListItem.SubItems.Add('Not Found');
    end;

    ListItem := EditIPLInstValidation.Items.Add;
    ListItem.Caption := 'Correct Model Name';
    if CorrectModel then
    begin
      ListItem.ImageIndex := VALIDATION_IMAGE_GOOD;
      ListItem.SubItems.Add('Found');
    end else
    begin
      ListItem.ImageIndex := VALIDATION_IMAGE_BAD;
      ListItem.SubItems.Add('Not Found');
    end;

    ListItem := EditIPLInstValidation.Items.Add;
    ListItem.Caption := 'Model Exists in Archive';
    if ModelExists then
    begin
      ListItem.ImageIndex := VALIDATION_IMAGE_GOOD;
      ListItem.SubItems.Add('Found');
    end else
    begin
      ListItem.ImageIndex := VALIDATION_IMAGE_BAD;
      ListItem.SubItems.Add('Not Found');
    end;

    ListItem := EditIPLInstValidation.Items.Add;
    ListItem.Caption := 'Texture Exists in Archive';
    if TextureExists then
    begin
      ListItem.ImageIndex := VALIDATION_IMAGE_GOOD;
      ListItem.SubItems.Add('Found');
    end else
    begin
      ListItem.ImageIndex := VALIDATION_IMAGE_BAD;
      ListItem.SubItems.Add('Not Found');
    end;
  end;
end;

procedure TFormEditorItem.PerformIDEObjsValidation;
var
  ModelExists, TextureExists: Boolean;
  ListItem: TListItem;
begin
  if (EditorValid) and (CurrentFileMode = FILE_IDE) and (CurrentSection = SECTION_OBJS) then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Objs.Item[CurrentItemIndex] do
    begin
      ModelExists := Main.GArchive.DffExists(ModelName);
      TextureExists := Main.GArchive.TxdExists(TextureName);
    end;

    PaintBox1.tag:= integer(ModelExists);
    PaintBox2.tag:= integer(TextureExists);
    PanelIDEObjs.Repaint;

  end;
end;

procedure TFormEditorItem.PerformIDETObjValidation;
var
  ModelExists, TextureExists: Boolean;
  ListItem: TListItem;
begin
  if (EditorValid) and (CurrentFileMode = FILE_IDE) and (CurrentSection = SECTION_TOBJ) then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).TObj.Item[CurrentItemIndex] do
    begin
      ModelExists := Main.GArchive.DffExists(ModelName);
      TextureExists := Main.GArchive.TxdExists(TextureName);
    end;

    PaintBox3.tag:= integer(ModelExists);
    PaintBox4.tag:= integer(TextureExists);
    panelidetobj.Repaint;

  end;
end;

procedure TFormEditorItem.PerformUndo;
begin
  case CurrentFileMode of
    FILE_IDE:
    begin
      case CurrentSection of
        SECTION_OBJS: TIDEFile(GFiles.Item[CurrentFileIndex]).Objs.LoadItem(CurrentItemIndex, UndoStr);
        SECTION_TOBJ: TIDEFile(GFiles.Item[CurrentFileIndex]).TObj.LoadItem(CurrentItemIndex, UndoStr);
        SECTION_PATH: TIDEFile(GFiles.Item[CurrentFileIndex]).Path.LoadItem(CurrentItemIndex, UndoStr);
        SECTION_2DFX: TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.LoadItem(CurrentItemIndex, UndoStr);
      end;
    end;

    FILE_IPL:
    begin
      case CurrentSection of
        SECTION_INST: TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.LoadItem(CurrentItemIndex, UndoStr);
        SECTION_CULL: TIPLFile(GFiles.Item[CurrentFileIndex]).Cull.LoadItem(CurrentItemIndex, UndoStr);
        SECTION_ZONE: TIPLFile(GFiles.Item[CurrentFileIndex]).Zone.LoadItem(CurrentItemIndex, UndoStr);
        SECTION_PATH: TIPLFile(GFiles.Item[CurrentFileIndex]).Path.LoadItem(CurrentItemIndex, UndoStr);
      end;
    end;
  end;
  if not AlreadyChanged then
    GFiles.Item[CurrentFileIndex].Changed := False;
  UpdateData(True, True);
end;

procedure TFormEditorItem.FileChanged(inFile: LongWord);
begin
  BtnUndo.Enabled := True;
  GFiles.Item[CurrentFileIndex].Changed := True;
  Main.FormMain.ActionSaveModified.Enabled := True;
end;

procedure TFormEditorItem.ActivatePanel(inFileMode, inSection: ShortInt);
var
i: integer;
begin
  BtnUndo.Visible := not ((inFileMode = FILE_IPL) and (inSection = SECTION_MULT_INST));

  PanelIPLInst.TabVisible := (inFileMode = FILE_IPL) and (inSection = SECTION_INST);
  PanelIPLMultInst.TabVisible := (inFileMode = FILE_IPL) and (inSection = SECTION_MULT_INST);
  PanelIPLCull.TabVisible := (inFileMode = FILE_IPL) and (inSection = SECTION_CULL);
  PanelIPLPath.TabVisible := (inFileMode = FILE_IPL) and (inSection = SECTION_PATH);
  PanelIPLZone.TabVisible := (inFileMode = FILE_IPL) and (inSection = SECTION_ZONE);

  PanelIDEObjs.TabVisible := (inFileMode = FILE_IDE) and (inSection = SECTION_OBJS);
  PanelIDETObj.TabVisible := (inFileMode = FILE_IDE) and (inSection = SECTION_TOBJ);
  PanelIDEPath.TabVisible := (inFileMode = FILE_IDE) and (inSection = SECTION_PATH);
  PanelIDE2dfx.TabVisible := (inFileMode = FILE_IDE) and (inSection = SECTION_2DFX);

  for i:= 0 to PageControl1.PageCount-1 do
  if PageControl1.Pages[i].TabVisible = true then PageControl1.ActivePage:= PageControl1.Pages[i];

  case inFileMode of
    FILE_IDE:
    begin
      case inSection of
        SECTION_2DFX:
        begin
          with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
          begin
            EditIDE2dfxID.Text := IntToStr(ID);

            EditIDE2dfxPosX.Text := Format('%1.6g', [Pos[0]]);
            EditIDE2dfxPosY.Text := Format('%1.6g', [Pos[1]]);
            EditIDE2dfxPosZ.Text := Format('%1.6g', [Pos[2]]);

            EditIDE2dfxViewDistance.Text := floattostr(ViewDistance);

            EditIDE2dfxColR.Text := IntToStr(Color[0]);
            EditIDE2dfxColG.Text := IntToStr(Color[1]);
            EditIDE2dfxColB.Text := IntToStr(Color[2]);
            EditIDE2dfxColChooser.Selected := TColor((Color[2] shl 16) or (Color[1] shl 8) or (Color[0]));
            EditIDE2dfxColChooser.Repaint;

            case EffectType of
              EFFECT_LIGHT:
              begin
                EditIDE2dfxRadioLight.Checked := True;

                PanelIDE2dfxLight.Visible := True;
                PanelIDE2dfxParticle.Visible := False;
                PanelIDE2dfxUnknown.Visible := False;
                PanelIDE2dfxAnimation.Visible := False;
                PanelIDE2dfxReflection.Visible := False;

                EditIDE2dfxAEffect1.Text := AEffect1;
                EditIDE2dfxAEffect2.Text := AEffect2;
                EditIDE2dfxADistance.Text := IntToStr(ADistance);
                EditIDE2dfxASizeLamp.Text := Format('%1.6g', [ASizeLamp]);
                EditIDE2dfxASizeCorona.Text := Format('%1.6g', [ASizeCorona]);

                if (AControl < EditIDE2dfxAControl.Items.Count) then
                  EditIDE2dfxAControl.ItemIndex := AControl;

                EditIDE2dfxARangeOuter.Text := Format('%1.6g', [ARangeOuter]);
                EditIDE2dfxARangeInner.Text := Format('%1.6g', [ARangeInner]);

                EditIDE2dfxAReflectionWet.Text := IntToStr(AReflectionWet);
                EditIDE2dfxALensFlare.Text := IntToStr(ALensFlare);
                EditIDE2dfxADust.Text := IntToStr(ADust);
              end;

              EFFECT_PARTICLE:
              begin
                EditIDE2dfxRadioParticle.Checked := True;

                PanelIDE2dfxLight.Visible := False;
                PanelIDE2dfxParticle.Visible := True;
                PanelIDE2dfxUnknown.Visible := False;
                PanelIDE2dfxAnimation.Visible := False;
                PanelIDE2dfxReflection.Visible := False;

                if (BType < EditIDE2dfxBType.Items.Count) then
                  EditIDE2dfxBType.ItemIndex := BType;

                EditIDE2dfxBRotX.Text := Format('%1.2f', [ArcSin(BRotation[0]) * 180 / Pi]);
                EditIDE2dfxBRotY.Text := Format('%1.2f', [ArcSin(BRotation[1]) * 180 / Pi]);
                EditIDE2dfxBRotZ.Text := Format('%1.2f', [ArcSin(BRotation[2]) * 180 / Pi]);
                EditIDE2dfxBRotW.Text := Format('%1.2f', [ArcCos(BRotation[3]) * 180 / Pi]);
              end;

              EFFECT_UNKNOWN:
              begin
                EditIDE2dfxRadioUnknown.Checked := True;

                PanelIDE2dfxLight.Visible := False;
                PanelIDE2dfxParticle.Visible := False;
                PanelIDE2dfxUnknown.Visible := True;
                PanelIDE2dfxAnimation.Visible := False;
                PanelIDE2dfxReflection.Visible := False;
              end;

              EFFECT_ANIMATION:
              begin
                EditIDE2dfxRadioAnimation.Checked := True;

                PanelIDE2dfxLight.Visible := False;
                PanelIDE2dfxParticle.Visible := False;
                PanelIDE2dfxUnknown.Visible := False;
                PanelIDE2dfxAnimation.Visible := True;
                PanelIDE2dfxReflection.Visible := False;

                if (DType < EditIDE2dfxDType.Items.Count) then
                  EditIDE2dfxDType.ItemIndex := DType;

                EditIDE2dfxDDir1X.Text := Format('%1.6g', [DDirection1[0]]);
                EditIDE2dfxDDir1Y.Text := Format('%1.6g', [DDirection1[1]]);
                EditIDE2dfxDDir1Z.Text := Format('%1.6g', [DDirection1[2]]);

                EditIDE2dfxDDir2X.Text := Format('%1.6g', [DDirection2[0]]);
                EditIDE2dfxDDir2Y.Text := Format('%1.6g', [DDirection2[1]]);
                EditIDE2dfxDDir2Z.Text := Format('%1.6g', [DDirection2[2]]);
              end;

              EFFECT_REFLECTION:
              begin
                EditIDE2dfxRadioReflection.Checked := True;

                PanelIDE2dfxLight.Visible := False;
                PanelIDE2dfxParticle.Visible := False;
                PanelIDE2dfxUnknown.Visible := False;
                PanelIDE2dfxAnimation.Visible := False;
                PanelIDE2dfxReflection.Visible := True;
              end;
            end;
          end;
        end;

        SECTION_OBJS:
        begin
          with TIDEFile(GFiles.Item[CurrentFileIndex]).Objs.Item[CurrentItemIndex] do
          begin
            FormExtra.SetObject(ID);
            PerformIDEObjsValidation;

            EditIDEObjsID.Text := IntToStr(ID);
            EditIDEObjsModel.Text := ModelName;
            EditIDEObjsTexture.Text := TextureName;

            EditIDEObjsU4.Text := IntToStr(submodels);
            EditIDEObjsLOD.Text := Format('%1.6g', [LOD]);
            EditIDEObjsFlags.Text := IntToStr(Flags);

            for i:= 0 to ide_bits.items.count-1 do
            ide_bits.Checked[i]:= IsBitSet(Flags, i);

          end;
        end;

        SECTION_TOBJ:
        begin
          with TIDEFile(GFiles.Item[CurrentFileIndex]).TObj.Item[CurrentItemIndex] do
          begin
            FormExtra.SetObject(ID);
            PerformIDETObjValidation;

            EditIDETObjID.Text := IntToStr(ID);
            EditIDETObjModel.Text := ModelName;
            EditIDETObjTexture.Text := TextureName;

            EditIDETObjU4.Text := floattostr(submodels);
            EditIDETObjLOD.Text := floattostr(LOD);
            EditIDETObjFlags.Text := IntToStr(Flags);

            for i:= 0 to CheckListBox1.items.count-1 do
            CheckListBox1.Checked[i]:= IsBitSet(Flags, i);

            EditIDETObjTimeOn.ItemIndex := TimeOn;
            EditIDETObjTimeOff.ItemIndex := TimeOff;
          end;
        end;

        SECTION_PATH:
        begin
          with TIDEFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex] do
          begin
            if (PathType = 'ped') then
              EditIDEPathRadioPed.Checked := True
            else
              EditIDEPathRadioCar.Checked := True;

            EditIDEPathID.Text := IntToStr(ID);
            EditIDEPathModel.Text := ModelName;

            EditIDEPathItems.ItemIndex := -1;
            EditIDEPathDrawlist;

            PanelIDEPathItem.Visible := False;
          end;
        end;
      end;
    end;
    FILE_IPL:
    begin
      case inSection of
        SECTION_INST:
        begin
          with TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex] do
          begin
            FormExtra.SetObject(ID);
            PerformIPLInstValidation;
            Main.FormIDE.ActionEditUseDefinition.Enabled := True;
            if not GFiles.Item[CurrentFileIndex].Visible then
            begin
              GFiles.Item[CurrentFileIndex].Visible := True;
              Main.FormMain.UpdateVisible;
            end;

            EditIPLInstID.Text := IntToStr(ID);
            EditIPLInstModel.Text := ModelName;

            EditIPLInstHasInterior.Checked := HasInterior;
            EditIPLInstInterior.Enabled := HasInterior;
            EditIPLInstInterior.Text := IntToStr(Interior);

            EditIPLInstPosX.Text := Format('%1.6g', [Pos[0]]);
            EditIPLInstPosY.Text := Format('%1.6g', [Pos[1]]);
            EditIPLInstPosZ.Text := Format('%1.6g', [Pos[2]]);

            EditIPLInstScaleX.Text := Format('%1.6g', [Scale[0]]);
            EditIPLInstScaleY.Text := Format('%1.6g', [Scale[1]]);
            EditIPLInstScaleZ.Text := Format('%1.6g', [Scale[2]]);

            Update_Euler_angles;
          end;
        end;

        SECTION_MULT_INST:
        begin
          EditIPLMultInstPosX.Text := '0';
          EditIPLMultInstPosY.Text := '0';
          EditIPLMultInstPosZ.Text := '0';
        end;

        SECTION_ZONE:
        begin
          with TIPLFile(GFiles.Item[CurrentFileIndex]).Zone.Item[CurrentItemIndex] do
          begin
            EditIPLZoneName.Text := ZoneName;
            box_zonetypes.itemindex := Sort;

            EditIPLZonePos1X.Text := Format('%1.6g', [Pos1[0]]);
            EditIPLZonePos1Y.Text := Format('%1.6g', [Pos1[1]]);
            EditIPLZonePos1Z.Text := Format('%1.6g', [Pos1[2]]);

            EditIPLZonePos2X.Text := Format('%1.6g', [Pos2[0]]);
            EditIPLZonePos2Y.Text := Format('%1.6g', [Pos2[1]]);
            EditIPLZonePos2Z.Text := Format('%1.6g', [Pos2[2]]);

            EditIPLZoneU9.Text := IntToStr(U9);
          end;
        end;

        SECTION_PATH:
        begin
          with TIPLFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex] do
          begin
            boxpathtype.itemindex:= PathType;

            EditIPLPathOther.Text := IntToStr(PathOther);

            EditIPLPathDrawlist;
            EditIPLPathItems.ItemIndex := 0;

          end;
        end;

        SECTION_CULL:
        begin
          with TIPLFile(GFiles.Item[CurrentFileIndex]).Cull.Item[CurrentItemIndex] do
          begin
            EditIPLCullPos1X.Text := Format('%1.6g', [center[0]]);
            EditIPLCullPos1Y.Text := Format('%1.6g', [center[1]]);
            EditIPLCullPos1Z.Text := Format('%1.6g', [center[2]]);

            EditIPLCullPos2X.Text := Format('%1.6g', [start[0]]);
            EditIPLCullPos2Y.Text := Format('%1.6g', [start[1]]);
            EditIPLCullPos2Z.Text := Format('%1.6g', [start[2]]);

            EditIPLCullPos3X.Text := Format('%1.6g', [stop[0]]);
            EditIPLCullPos3Y.Text := Format('%1.6g', [stop[1]]);
            EditIPLCullPos3Z.Text := Format('%1.6g', [stop[2]]);

            EditIPLCullU10.Text := IntToStr(flags);
            EditIPLCullU11.Text := IntToStr(U11);
          end;
        end;
      end;
    end;
  end;

  PageControl1.Visible := not ((inFileMode = -1) or (inSection = -1));
end;

procedure TFormEditorItem.SetToNone;
begin
  EditorValid := False;
  CurrentFileMode := 0;
  CurrentSection := 0;
  CurrentFileIndex := 0;
  CurrentItemIndex := 0;
  MainGLView.SetMouseMode(MOUSEMODE_NORMAL);
  Main.FormIDE.ActionEditUseDefinition.Enabled := False;
  UpdateData(True, False);
end;

// delfi's extended objects picking

procedure TFormEditorItem.JumpToFile(Fileindex, Section, subsection, node: LongWord);
var
  LItem: TListItem;
  DoChangePanels: Boolean;
  I, J: LongWord;
  AlreadyThere: LongInt;
  CenPos: TVector3f;
begin
  DoChangePanels := True;

  // select ipl file
    J := 0;
    if (FormIPL.DisplayListFiles.Count > 0) then
      for I := 0 to FormIPL.DisplayListFiles.Count - 1 do
        if (LongWord(FormIPL.DisplayListFiles.Items[I]) = Fileindex) then
          J := I;

    FormIPL.ListFiles.ItemIndex := J;
    FormIPL.RadioSection.ItemIndex := Section;
    FormIPL.RadioSectionClick(self); // set proper in-section

    // select item

    J := 0;
    if (FormIPL.DisplayListItems.Count > 0) then
      for I := 0 to FormIPL.DisplayListItems.Count - 1 do
        if (LongWord(FormIPL.DisplayListItems.Items[I]) = subsection) then
          J := I;

    FormIPL.ListItems.ItemIndex := J;
    FormIPL.RadioSectionClick(self); // set proper in-section

  if not Visible then Visible := True;

  // bring the editor
  FormIPL.PopupEditItemClick(self);

  EditIPLPathItems.ItemIndex:= node;
  EditIPLPathItems.SetFocus;

  // enable mouse activity
  MainGLView.SetMouseMode(MOUSEMODE_ipl);

  MainGLView.JumpToLocation(GetPos);

end;

// kcow's selection for ipl inst

procedure TFormEditorItem.SetToMode(inFileMode, inSection: integer; inFile, inIndex: LongWord);
var
  LItem: TListItem;
  DoChangePanels: Boolean;
  I, J: LongWord;
  AlreadyThere: LongInt;
  CenPos: TVector3f;
begin
  DoChangePanels := True;

  if (InSection = SECTION_SEL_INST) then
  begin
    J := 0;
    if (FormIPL.DisplayListFiles.Count > 0) then for I := 0 to FormIPL.DisplayListFiles.Count - 1 do
      if (LongWord(FormIPL.DisplayListFiles.Items[I]) = InFile) then
        J := I;
    FormIPL.ListFiles.ItemIndex := J;
    FormIPL.RadioSection.ItemIndex := 0;

    J := 0;
    if (FormIPL.DisplayListItems.Count > 0) then for I := 0 to FormIPL.DisplayListItems.Count - 1 do
      if (LongWord(FormIPL.DisplayListItems.Items[I]) = InIndex) then
        J := I;
    FormIPL.ListItems.ItemIndex := J;
    InSection := SECTION_INST;
  end;

  if (inSection = SECTION_MULT_INST) then
  begin
    if (CurrentSection = SECTION_MULT_INST) then
      DoChangePanels := False
    else
    begin
      EditIPLMultInstList.Clear;
      if (EditorValid) and (CurrentSection = SECTION_INST) then
      begin
        LItem := EditIPLMultInstList.Items.Add;
        LItem.Data := Pointer(CurrentItemIndex);
        LItem.Caption := IntToStr(TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex].ID) + ' - ' + TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex].ModelName;
      end else
        inSection := SECTION_INST;
    end;
  end;
  CurrentFileMode := inFileMode;
  CurrentSection := inSection;
  CurrentFileIndex := inFile;
  CurrentItemIndex := inIndex;
  EditorValid := True;

  case inFileMode of
    FILE_IPL:
    begin
      case inSection of
        SECTION_INST: MainGLView.SetMouseMode(MOUSEMODE_ipl);
        SECTION_MULT_INST:
        begin
          AlreadyThere := -1;
          if (EditIPLMultInstList.Items.Count > 0) then for I := 0 to EditIPLMultInstList.Items.Count - 1 do
            if (LongWord(EditIPLMultInstList.Items.Item[I].Data) = CurrentItemIndex) then
              AlreadyThere := I;
          if (AlreadyThere = -1) then
          begin
            LItem := EditIPLMultInstList.Items.Add;
            LItem.Data := Pointer(CurrentItemIndex);
            LItem.Caption := IntToStr(TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex].ID) + ' - ' + TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex].ModelName;
          end else
            EditIPLMultInstList.Items.Delete(AlreadyThere);
          CenPos := GetPos;
          EditIPLMultInstItemCount.Caption := IntToStr(EditIPLMultInstList.Items.Count) + ' Items';
          EditIPLMultInstItemCenter.Caption := Format('%1.6g, %1.6g, %1.6g', [CenPos[0], CenPos[2], CenPos[2]]);
          MainGLView.SetMouseMode(MOUSEMODE_MULT_INST);
        end;
      else
        MainGLView.SetMouseMode(MOUSEMODE_NORMAL);
      end;
    end;
  else
    MainGLView.SetMouseMode(MOUSEMODE_NORMAL);
  end;

  Main.FormIDE.ActionEditUseDefinition.Enabled := False;
  UpdateData(DoChangePanels, False);

  UndoStr := ExpandedEdit.Text;
  AlreadyChanged := GFiles.Item[CurrentFileIndex].Changed;
  BtnUndo.Enabled := False;

  if not Visible then Visible := True;
end;

function TFormEditorItem.GetPos: TVector3f;
var
  I: LongWord;
begin
  Result[0] := 0;
  Result[1] := 0;
  Result[2] := 0;
  if (EditorValid) then
    case CurrentFileMode of
      FILE_IPL:
      begin
        case CurrentSection of
          SECTION_INST: Result := TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex].Pos;

          SECTION_PATH: begin
          Result := TIPLFile(GFiles.Item[CurrentFileIndex]).path.Item[CurrentItemIndex].Item[EditIPLPathItems.ItemIndex].Pos;
          result[0]:= result[0] * iplpathmp;
          result[1]:= result[1] * iplpathmp;
          result[2]:= result[2] * iplpathmp;
          end;

          SECTION_MULT_INST:
          begin
            if (EditIPLMultInstList.Items.Count > 0) then
            begin
              for I := 0 to EditIPLMultInstList.Items.Count - 1 do
              begin
                Result[0] := Result[0] + TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[LongWord(EditIPLMultInstList.Items.Item[I].Data)].Pos[0];
                Result[1] := Result[1] + TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[LongWord(EditIPLMultInstList.Items.Item[I].Data)].Pos[1];
                Result[2] := Result[2] + TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[LongWord(EditIPLMultInstList.Items.Item[I].Data)].Pos[2];
              end;
              Result[0] := Result[0] / EditIPLMultInstList.Items.Count;
              Result[1] := Result[1] / EditIPLMultInstList.Items.Count;
              Result[2] := Result[2] / EditIPLMultInstList.Items.Count;
            end;
          end;
        end;
      end;
  end;
end;

function TFormEditorItem.GetRot: TVector4f;
begin
  if (EditorValid) and (CurrentFileMode = FILE_IPL) and (CurrentSection = SECTION_INST) then
    Result := TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex].Rotation
  else
  begin
    Result[0] := 0;
    Result[1] := 0;
    Result[2] := 0;
    Result[3] := 1;
  end;
end;

// watch this
procedure TFormEditorItem.MoveByAmount(InAmount: TVector3f);
var
  Change: TVector3f;
  I: LongWord;
begin
  if (EditorValid) then
  begin
    case CurrentFileMode of
      FILE_IPL:
      begin
        case CurrentSection of
          SECTION_INST:
          begin

            with TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex] do
            begin
              Change := Pos;

              Change[0] := Change[0] + InAmount[0];
              Change[1] := Change[1] + InAmount[1];
              Change[2] := Change[2] + InAmount[2];

              Pos := Change;

              ManualChange := False;
              EditIPLInstPosX.Text := Format('%1.6g', [Pos[0]]);
              EditIPLInstPosY.Text := Format('%1.6g', [Pos[1]]);
              EditIPLInstPosZ.Text := Format('%1.6g', [Pos[2]]);
              ManualChange := True;
            end;

            UpdateData(False, False);
          end;

          SECTION_path:
            with TIPLFile(GFiles.Item[CurrentFileIndex]).path.Item[CurrentItemIndex].Item[EditIPLPathItems.ItemIndex] do begin

              pos[0]:= pos[0] + InAmount[0] * 2;
              pos[1]:= pos[1] + InAmount[1] * 2;
              pos[2]:= pos[2] + InAmount[2] * 2;

              ManualChange := False;
              EditIPLPathItemPosX.Text := Format('%1.6g', [Pos[0]]);
              EditIPLPathItemPosY.Text := Format('%1.6g', [Pos[1]]);
              EditIPLPathItemPosZ.Text := Format('%1.6g', [Pos[2]]);
              ManualChange := True;
            UpdateData(False, False);
          end;

          SECTION_MULT_INST:
          begin
            if (EditIPLMultInstList.Items.Count > 0) then for I := 0 to EditIPLMultInstList.Items.Count - 1 do
              with TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[LongWord(EditIPLMultInstList.Items.Item[I].Data)] do
              begin
                Change := Pos;

                Change[0] := Change[0] + InAmount[0];
                Change[1] := Change[1] + InAmount[1];
                Change[2] := Change[2] + InAmount[2];

                Pos := Change;
              end;
              
            UpdateData(False, False);
          end;
        end;
      end;
    end;
  end;
end;

procedure TFormEditorItem.MoveToPos(InPos: TVector3f);
begin
  if (EditorValid) and (CurrentFileMode = FILE_IPL) and (CurrentSection = SECTION_INST) then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex] do
    begin
      Pos := InPos;

      ManualChange := False;
      EditIPLInstPosX.Text := Format('%1.6g', [Pos[0]]);
      EditIPLInstPosY.Text := Format('%1.6g', [Pos[1]]);
      EditIPLInstPosZ.Text := Format('%1.6g', [Pos[2]]);
      ManualChange := True;
    end;

    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.RotateToPos(InRot: TVector4f);
begin
  if (EditorValid) and (CurrentFileMode = FILE_IPL) and (CurrentSection = SECTION_INST) then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex] do
    begin
      Rotation := InRot;

      Update_Euler_angles;
    end;

    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.UpdateIPLDefinition(inID: LongInt; inModelName: String);
begin
  if (EditorValid) and (CurrentFileMode = FILE_IPL) and (CurrentSection = SECTION_INST) then
  begin
    TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex].ID := inID;
    TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex].ModelName := inModelName;

    TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex].RemFile := -1;
    TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex].RemIndex := -1;

    UpdateData(False, True);
  end;
end;


procedure TFormEditorItem.SetAction(inAction: TAction);
begin
  MainAction := inAction;
end;

procedure TFormEditorItem.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if not (MainAction = nil) then
    MainAction.Checked := False;
end;

procedure TFormEditorItem.BtnCancelClick(Sender: TObject);
begin
  SetToNone;
  Hide;
end;

procedure TFormEditorItem.BtnUndoClick(Sender: TObject);
begin
  PerformUndo;
  BtnUndo.Enabled := False;
end;

procedure TFormEditorItem.EditIPLInstIDChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex] do
      ID := StrToIntDef(EditIPLInstID.Text, 0);
    UpdateData(False, True);
    PerformIPLInstValidation;
  end;
end;

procedure TFormEditorItem.EditIPLInstModelChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex] do
      ModelName := EditIPLInstModel.Text;
    UpdateData(False, True);
    PerformIPLInstValidation;
  end;
end;

procedure TFormEditorItem.EditIPLInstInteriorChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex] do
      Interior := StrToIntDef(EditIPLInstInterior.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIPLInstHasInteriorClick(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex] do
    begin
      HasInterior := EditIPLInstHasInterior.Checked;
      EditIPLInstInterior.Enabled := HasInterior;
    end;
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIPLInstRotXChange(Sender: TObject);
begin
  if ManualChange then
  begin
    Use_euler_angles;
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIPLInstRotYChange(Sender: TObject);
begin
  if ManualChange then
  begin
    Use_euler_angles;
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIPLInstRotZChange(Sender: TObject);
begin
  if ManualChange then
  begin
    Use_euler_angles;
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIPLInstPosXChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex] do
      Pos[0] := StrToFloatDef(EditIPLInstPosX.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIPLInstPosYChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex] do
      Pos[1] := StrToFloatDef(EditIPLInstPosY.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIPLInstPosZChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex] do
      Pos[2] := StrToFloatDef(EditIPLInstPosZ.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIPLInstScaleXChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex] do
      Scale[0] := StrToFloatDef(EditIPLInstScaleX.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIPLInstScaleYChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex] do
      Scale[1] := StrToFloatDef(EditIPLInstScaleY.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIPLInstScaleZChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex] do
      Scale[2] := StrToFloatDef(EditIPLInstScaleZ.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxIDChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      ID := StrToIntDef(EditIDE2dfxID.Text, 0);
    UpdateData(False, True);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxColRChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
    begin
      Color[0] := StrToIntDef(EditIDE2dfxColR.Text, 0);
      ManualChange := False;
      EditIDE2dfxColChooser.Selected := TColor((Color[2] shl 16) or (Color[1] shl 8) or (Color[0]));
      EditIDE2dfxColChooser.Repaint;
      ManualChange := True;
    end;
    UpdateData(False, True);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxColGChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
    begin
      Color[1] := StrToIntDef(EditIDE2dfxColG.Text, 0);
      ManualChange := False;
      EditIDE2dfxColChooser.Selected := TColor((Color[2] shl 16) or (Color[1] shl 8) or (Color[0]));
      EditIDE2dfxColChooser.Repaint;
      ManualChange := True;
    end;
    UpdateData(False, True);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxColBChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
    begin
      Color[2] := StrToIntDef(EditIDE2dfxColB.Text, 0);
      ManualChange := False;
      EditIDE2dfxColChooser.Selected := TColor((Color[2] shl 16) or (Color[1] shl 8) or (Color[0]));
      EditIDE2dfxColChooser.Repaint;
      ManualChange := True;
    end;
    UpdateData(False, True);
  end;
end;


procedure TFormEditorItem.EditIDE2dfxColChooserChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
    begin
      Color[0] := (EditIDE2dfxColChooser.Selected and $FF);
      Color[1] := (EditIDE2dfxColChooser.Selected shr 8 and $FF);
      Color[2] := (EditIDE2dfxColChooser.Selected shr 16 and $FF);
      EditIDE2dfxColR.Text := IntToStr(Color[0]);
      EditIDE2dfxColG.Text := IntToStr(Color[1]);
      EditIDE2dfxColB.Text := IntToStr(Color[2]);
    end;
    UpdateData(False, True);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxPosXChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      Pos[0] := StrToIntDef(EditIDE2dfxPosX.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxPosYChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      Pos[1] := StrToIntDef(EditIDE2dfxPosY.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxPosZChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      Pos[2] := StrToIntDef(EditIDE2dfxPosZ.Text, 0);
    UpdateData(False, False);
  end;
end;


procedure TFormEditorItem.EditIDE2dfxRadioLightClick(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      EffectType := EFFECT_LIGHT;
    UpdateData(True, True);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxRadioParticleClick(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      EffectType := EFFECT_PARTICLE;
    UpdateData(True, True);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxBRotXChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      BRotation[0] := Sin(StrToFloatDef(EditIDE2dfxBRotX.Text, 0) * PI / 180);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxBRotYChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      BRotation[1] := Sin(StrToFloatDef(EditIDE2dfxBRotY.Text, 0) * PI / 180);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxBRotZChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      BRotation[2] := Sin(StrToFloatDef(EditIDE2dfxBRotZ.Text, 0) * PI / 180);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxBRotWChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      BRotation[3] := Cos(StrToFloatDef(EditIDE2dfxBRotW.Text, 0) * PI / 180);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxAEffect1Change(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      AEffect1 := EditIDE2dfxAEffect1.Text;
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxAEffect2Change(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      AEffect2 := EditIDE2dfxAEffect2.Text;
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxADistanceChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      ADistance := StrToIntDef(EditIDE2dfxADistance.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxAReflectionWetChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      AReflectionWet := StrToIntDef(EditIDE2dfxAReflectionWet.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxALensFlareChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      ALensFlare := StrToIntDef(EditIDE2dfxALensFlare.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxADustChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      ADust := StrToIntDef(EditIDE2dfxADust.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxASizeLampChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      ASizeLamp := StrToFloatDef(EditIDE2dfxASizeLamp.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxASizeCoronaChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      ASizeCorona := StrToFloatDef(EditIDE2dfxASizeCorona.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxARangeOuterChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      ARangeOuter := StrToFloatDef(EditIDE2dfxARangeOuter.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxARangeInnerChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      ARangeInner := StrToFloatDef(EditIDE2dfxARangeInner.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxAControlChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      AControl := EditIDE2dfxAControl.ItemIndex;
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDETObjIDChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).TObj.Item[CurrentItemIndex] do
      ID := StrToIntDef(EditIDETObjID.Text, 0);
    UpdateData(False, True);
    PerformIDETObjValidation;
  end;
end;

procedure TFormEditorItem.EditIDETObjModelChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).TObj.Item[CurrentItemIndex] do
      ModelName := EditIDETObjModel.Text;
    UpdateData(False, True);
    PerformIDETObjValidation;
  end;
end;

procedure TFormEditorItem.EditIDETObjTextureChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).TObj.Item[CurrentItemIndex] do
      TextureName := EditIDETObjTexture.Text;
    UpdateData(False, False);
    PerformIDETObjValidation;
  end;
end;

procedure TFormEditorItem.EditIDETObju4Change(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).TObj.Item[CurrentItemIndex] do
      submodels := StrToIntDef(EditIDETObju4.Text, 1);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDETObjLODChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).TObj.Item[CurrentItemIndex] do
      LOD := StrToIntDef(EditIDETObjLOD.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDETObjFlagsChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).TObj.Item[CurrentItemIndex] do
      Flags := StrToIntDef(EditIDETObjFlags.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDETObjTimeOnChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).TObj.Item[CurrentItemIndex] do
      TimeOn := EditIDETObjTimeOn.ItemIndex;
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDETObjTimeOffChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).TObj.Item[CurrentItemIndex] do
      TimeOff := EditIDETObjTimeOff.ItemIndex;
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDEObjsIDChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Objs.Item[CurrentItemIndex] do
    begin
      ID := StrToIntDef(EditIDEObjsID.Text, 0);
      FormExtra.SetObject(ID);
    end;
    UpdateData(False, True);
    PerformIDEObjsValidation;
  end;
end;

procedure TFormEditorItem.EditIDEObjsModelChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Objs.Item[CurrentItemIndex] do
    begin
      ModelName := EditIDEObjsModel.Text;
      ListAvailable := False; DffNum := -1;
    end;
    UpdateData(False, True);
    PerformIDEObjsValidation;
  end;
end;

procedure TFormEditorItem.EditIDEObjsTextureChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Objs.Item[CurrentItemIndex] do
    begin
      TextureName := EditIDEObjsTexture.Text;
      ListAvailable := False; DffNum := -1;
    end;
    TIDEFile(GFiles.Item[CurrentFileIndex]).Objs.CalcArchiveNums(CurrentItemIndex);
    UpdateData(False, False);
    PerformIDETObjValidation;
  end;
end;

procedure TFormEditorItem.EditIDEObjsu4Change(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Objs.Item[CurrentItemIndex] do
      submodels := StrToIntDef(EditIDEObjsu4.Text, 1);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDEObjsLODChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Objs.Item[CurrentItemIndex] do
      LOD := StrToIntDef(EditIDEObjsLOD.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDEObjsFlagsChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Objs.Item[CurrentItemIndex] do
      Flags := StrToIntDef(EditIDEObjsFlags.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIPLZoneNameChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Zone.Item[CurrentItemIndex] do
      ZoneName := EditIplZoneName.Text;
    UpdateData(False, True);
  end;
end;

procedure TFormEditorItem.EditIPLZoneU9Change(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Zone.Item[CurrentItemIndex] do
      U9 := StrToIntDef(EditIPLZoneU9.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIPLZonePos1XChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Zone.Item[CurrentItemIndex] do
      Pos1[0] := StrToFloatDef(EditIPLZonePos1X.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIPLZonePos1YChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Zone.Item[CurrentItemIndex] do
      Pos1[1] := StrToFloatDef(EditIPLZonePos1Y.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIPLZonePos1ZChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Zone.Item[CurrentItemIndex] do
      Pos1[2] := StrToFloatDef(EditIPLZonePos1Z.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIPLZonePos2XChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Zone.Item[CurrentItemIndex] do
      Pos2[0] := StrToFloatDef(EditIPLZonePos2X.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIPLZonePos2YChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Zone.Item[CurrentItemIndex] do
      Pos2[1] := StrToFloatDef(EditIPLZonePos2Y.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIPLZonePos2ZChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Zone.Item[CurrentItemIndex] do
      Pos2[2] := StrToFloatDef(EditIPLZonePos2Z.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.MoveTimerTimer(Sender: TObject);
var
  MoveAmount: TVector3f;
begin
  if (EditorValid) then
  begin
    case TimerMode of
      TIMERMODE_OFF:  MoveTimer.Enabled := False;
      TIMERMODE_MULT_INST_POSX:
      begin
        if (CurrentFileMode = FILE_IPL) and (CurrentSection = SECTION_MULT_INST) then
        begin
          MoveAmount[0] := TimerAmount;
          MoveAmount[1] := 0;
          MoveAmount[2] := 0;
          MoveByAmount(MoveAmount);
          UpdateData(False, False);
        end else
          MoveTimer.Enabled := False;
      end;
      TIMERMODE_MULT_INST_POSY:
      begin
        if (CurrentFileMode = FILE_IPL) and (CurrentSection = SECTION_MULT_INST) then
        begin
          MoveAmount[0] := 0;
          MoveAmount[1] := TimerAmount;
          MoveAmount[2] := 0;
          MoveByAmount(MoveAmount);
          UpdateData(False, False);
        end else
          MoveTimer.Enabled := False;
      end;
      TIMERMODE_MULT_INST_POSZ:
      begin
        if (CurrentFileMode = FILE_IPL) and (CurrentSection = SECTION_MULT_INST) then
        begin
          MoveAmount[0] := 0;
          MoveAmount[1] := 0;
          MoveAmount[2] := TimerAmount;
          MoveByAmount(MoveAmount);
          UpdateData(False, False);
        end else
          MoveTimer.Enabled := False;
      end;
      TIMERMODE_INST_POSX:
      begin
        if (CurrentFileMode = FILE_IPL) and (CurrentSection = SECTION_INST) then
        begin
          with TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex] do
          begin
            Pos[0] := Pos[0] + TimerAmount;
            ManualChange := False;
            EditIPLInstPosX.Text := Format('%1.6g', [Pos[0]]);
            ManualChange := True;
          end;
          UpdateData(False, False);
        end else
          MoveTimer.Enabled := False;
      end;
      TIMERMODE_INST_POSY:
      begin
        if (CurrentFileMode = FILE_IPL) and (CurrentSection = SECTION_INST) then
        begin
          with TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex] do
          begin
            Pos[1] := Pos[1] + TimerAmount;
            ManualChange := False;
            EditIPLInstPosY.Text := Format('%1.6g', [Pos[1]]);
            ManualChange := True;
          end;
          UpdateData(False, False);
        end else
          MoveTimer.Enabled := False;
      end;
      TIMERMODE_INST_POSZ:
      begin
        if (CurrentFileMode = FILE_IPL) and (CurrentSection = SECTION_INST) then
        begin
          with TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex] do
          begin
            Pos[2] := Pos[2] + TimerAmount;
            ManualChange := False;
            EditIPLInstPosZ.Text := Format('%1.6g', [Pos[2]]);
            ManualChange := True;
          end;
          UpdateData(False, False);
        end else
          MoveTimer.Enabled := False;
      end;
      TIMERMODE_INST_ROTX:
      begin
        if (CurrentFileMode = FILE_IPL) and (CurrentSection = SECTION_INST) then
        begin
          with TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex] do
          begin
            Rotation := QuatMultiply(CreateXRotQuad(TimerAmount * PI / 180), Rotation);
            //Rotation := QuatMultiply(Rotation,CreateXRotQuad(TimerAmount * PI / 180));
            Update_Euler_angles;
          end;
          UpdateData(False, False);
        end else
          MoveTimer.Enabled := False;
      end;
      TIMERMODE_INST_ROTY:
      begin
        if (CurrentFileMode = FILE_IPL) and (CurrentSection = SECTION_INST) then
        begin
          with TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex] do
          begin
            Rotation := QuatMultiply(CreateYRotQuad(TimerAmount * PI / 180), Rotation);
            //Rotation := QuatMultiply(Rotation,CreateYRotQuad(TimerAmount * PI / 180));
            Update_Euler_angles;
          end;
          UpdateData(False, False);
        end else
          MoveTimer.Enabled := False;
      end;
      TIMERMODE_INST_ROTZ:
      begin
        if (CurrentFileMode = FILE_IPL) and (CurrentSection = SECTION_INST) then
        begin
          with TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex] do
          begin
            Rotation := QuatMultiply(CreateZRotQuad(TimerAmount * PI / 180), Rotation);
            //Rotation := QuatMultiply( Rotation,CreateZRotQuad(TimerAmount * PI / 180));
            Update_Euler_angles;
          end;
          UpdateData(False, False);
        end else
          MoveTimer.Enabled := False;
      end;
    end;
  end else
    MoveTimer.Enabled := False;
  if not MoveTimer.Enabled then
    TimerMode := TIMERMODE_OFF;
end;

procedure TFormEditorItem.EditIPLInstPosXL1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_INST_POSX;
  TimerAmount := -DEC_FAST;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLInstPosXL2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_INST_POSX;
  TimerAmount := -DEC_NORMAL;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLInstPosXR2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_INST_POSX;
  TimerAmount := DEC_NORMAL;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLInstPosXR1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_INST_POSX;
  TimerAmount := DEC_FAST;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLInstPosYL1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_INST_POSY;
  TimerAmount := -DEC_FAST;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLInstPosYL2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_INST_POSY;
  TimerAmount := -DEC_NORMAL;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLInstPosYR2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_INST_POSY;
  TimerAmount := DEC_NORMAL;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLInstPosYR1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_INST_POSY;
  TimerAmount := DEC_FAST;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLInstPosZL1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_INST_POSZ;
  TimerAmount := -DEC_FAST;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLInstPosZL2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_INST_POSZ;
  TimerAmount := -DEC_NORMAL;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLInstPosZR2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_INST_POSZ;
  TimerAmount := DEC_NORMAL;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLInstPosZR1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_INST_POSZ;
  TimerAmount := DEC_FAST;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLInstRotXL1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_INST_ROTX;
  TimerAmount := -DEC_FAST;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLInstRotYL1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_INST_ROTY;
  TimerAmount := -DEC_FAST;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLInstRotZL1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_INST_ROTZ;
  TimerAmount := -DEC_FAST;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLInstRotXL2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_INST_ROTX;
  TimerAmount := -DEC_NORMAL;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLInstRotYL2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_INST_ROTY;
  TimerAmount := -DEC_NORMAL;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLInstRotZL2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_INST_ROTZ;
  TimerAmount := -DEC_NORMAL;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLInstRotXR2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_INST_ROTX;
  TimerAmount := DEC_NORMAL;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLInstRotYR2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_INST_ROTY;
  TimerAmount := DEC_NORMAL;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLInstRotZR2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_INST_ROTZ;
  TimerAmount := DEC_NORMAL;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLInstRotXR1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_INST_ROTX;
  TimerAmount := DEC_FAST;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLInstRotYR1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_INST_ROTY;
  TimerAmount := DEC_FAST;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLInstRotZR1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_INST_ROTZ;
  TimerAmount := DEC_FAST;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLInstRotZR2MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MoveTimer.Enabled := False;
end;

procedure TFormEditorItem.EditIDE2dfxDDir1XChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      DDirection1[0] := StrToFloatDef(EditIDE2dfxDDir1X.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxDDir1YChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      DDirection1[1] := StrToFloatDef(EditIDE2dfxDDir1Y.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxDDir1ZChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      DDirection1[2] := StrToFloatDef(EditIDE2dfxDDir1Z.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxDDir2XChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      DDirection2[0] := StrToFloatDef(EditIDE2dfxDDir2X.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxDDir2YChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      DDirection2[1] := StrToFloatDef(EditIDE2dfxDDir2Y.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxDDir2ZChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      DDirection2[2] := StrToFloatDef(EditIDE2dfxDDir2Z.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDEPathRadioPedClick(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex] do
      PathType := 'ped';
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDEPathRadioCarClick(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex] do
      PathType := 'car';
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDEPathIDChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex] do
      ID := StrToIntDef(EditIDEPathID.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDEPathModelChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex] do
      ModelName := EditIDEPathModel.Text;
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDEPathItemsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if ManualChange then
  begin
    if (Item = nil) or (Selected = False) then
    begin
      PanelIDEPathItem.Visible := False;
    end else
    begin
      PanelIDEPathItem.Visible := True;
      ManualChange := False;
      with TIDEFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex].Item[Item.Index] do
      begin
        case NodeType of
          0: EditIDEPathItemTypeNone.Down := True;
          1: EditIDEPathItemTypeEnd.Down := True;
          2: EditIDEPathItemTypeMid.Down := True;
        end;
        EditIDEPathItemConnect.Text := IntToStr(NodeConnect);
        EditIDEPathItemU3.Text := IntToStr(U3);
        EditIDEPathItemU7.Text := IntToStr(U7);

        EditIDEPathItemPosX.Text := IntToStr(Pos[0]);
        EditIDEPathItemPosY.Text := IntToStr(Pos[1]);
        EditIDEPathItemPosZ.Text := IntToStr(Pos[2]);

        case LaneLeft of
          0: EditIDEPathItemLL0.Down := True;
          1: EditIDEPathItemLL1.Down := True;
          2: EditIDEPathItemLL2.Down := True;
        end;

        case LaneRight of
          0: EditIDEPathItemLR0.Down := True;
          1: EditIDEPathItemLR1.Down := True;
          2: EditIDEPathItemLR2.Down := True;
        end;
      end;
      ManualChange := True;
    end;
  end;
end;

procedure TFormEditorItem.EditIDEPathItemTypeNoneClick(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex].Item[EditIDEPathItems.ItemIndex] do
      NodeType := 0;
    UpdateData(False, False);
    EditIDEPathDrawlist;
  end;
end;

procedure TFormEditorItem.EditIDEPathItemTypeMidClick(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex].Item[EditIDEPathItems.ItemIndex] do
      NodeType := 2;
    UpdateData(False, False);
    EditIDEPathDrawlist;
  end;
end;

procedure TFormEditorItem.EditIDEPathItemTypeEndClick(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex].Item[EditIDEPathItems.ItemIndex] do
      NodeType := 1;
    UpdateData(False, False);
    EditIDEPathDrawlist;
  end;
end;

procedure TFormEditorItem.EditIDEPathItemConnectChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex].Item[EditIDEPathItems.ItemIndex] do
      NodeConnect := StrToIntDef(EditIDEPathItemConnect.Text, -1);
    UpdateData(False, False);
    EditIDEPathDrawlist;
  end;
end;

procedure TFormEditorItem.EditIDEPathItemU3Change(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex].Item[EditIDEPathItems.ItemIndex] do
      U3 := StrToIntDef(EditIDEPathItemU3.Text, 0);
    UpdateData(False, False);
    EditIDEPathDrawlist;
  end;
end;

procedure TFormEditorItem.EditIDEPathItemU7Change(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex].Item[EditIDEPathItems.ItemIndex] do
      U7 := StrToIntDef(EditIDEPathItemU7.Text, 0);
    UpdateData(False, False);
    EditIDEPathDrawlist;
  end;
end;

procedure TFormEditorItem.EditIDEPathItemPosXChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex].Item[EditIDEPathItems.ItemIndex] do
      Pos[0] := StrToIntDef(EditIDEPathItemPosX.Text, 0);
    UpdateData(False, False);
    EditIDEPathDrawlist;
  end;
end;

procedure TFormEditorItem.EditIDEPathItemPosYChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex].Item[EditIDEPathItems.ItemIndex] do
      Pos[1] := StrToIntDef(EditIDEPathItemPosY.Text, 0);
    UpdateData(False, False);
    EditIDEPathDrawlist;
  end;
end;

procedure TFormEditorItem.EditIDEPathItemPosZChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex].Item[EditIDEPathItems.ItemIndex] do
      Pos[2] := StrToIntDef(EditIDEPathItemPosZ.Text, 0);
    UpdateData(False, False);
    EditIDEPathDrawlist;
  end;
end;

procedure TFormEditorItem.EditIDEPathDrawlist;
var
  I: LongWord;
  PreManualChange: Boolean;
  PreItemIndex: LongInt;
  LItem: TListItem;
begin
  with TIDEFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex] do
  begin
    PreManualChange := ManualChange;
    ManualChange := False;
    PreItemIndex := EditIDEPathItems.ItemIndex;
    EditIDEPathItems.Clear;
    if (RCount > 0) then for I := 0 to RCount - 1 do
    begin
      LItem := EditIDEPathItems.Items.Add;
      case Item[I].NodeType of
        0: LItem.Caption := '-';
        1: LItem.Caption := 'End';
        2: LItem.Caption := 'Mid';
      end;
      LItem.SubItems.Add(IntToStr(Item[I].NodeConnect));
      LItem.SubItems.Add(Format('(%d, %d, %d)', [Item[I].Pos[0], Item[I].Pos[1], Item[I].Pos[2]]));
    end;
    EditIDEPathItems.ItemIndex := PreItemIndex;
    ManualChange := PreManualChange;
  end;
end;

procedure TFormEditorItem.EditIPLPathDrawlist;
var
  I: LongWord;
  PreManualChange: Boolean;
  PreItemIndex: LongInt;
  LItem: TListItem;
begin
  with TIPLFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex] do
  begin
    PreManualChange := ManualChange;
    ManualChange := False;
    PreItemIndex := EditIPLPathItems.ItemIndex;
    EditIPLPathItems.Clear;
    if (RCount > 0) then for I := 0 to RCount - 1 do
    begin
      LItem := EditIPLPathItems.Items.Add;
      LItem.Caption:= inttostr(i);
      case Item[I].NodeType of
        0: LItem.SubItems.Add('-');
        1: LItem.SubItems.Add('End');
        2: LItem.SubItems.Add('Mid');
      end;
      LItem.SubItems.Add(IntToStr(Item[I].NodeConnect));
      LItem.SubItems.Add(Format('(%1.6g, %1.6g, %1.6g)', [Item[I].Pos[0], Item[I].Pos[1], Item[I].Pos[2]]));
    end;
    EditIPLPathItems.ItemIndex := PreItemIndex;
    try
    EditIPLPathItems.setfocus;
    except end; // lazy trick, i know.. - Delfi
    ManualChange := PreManualChange;
  end;
end;

procedure TFormEditorItem.EditIPLMultInstPosXChange(Sender: TObject);
var
  MoveAmount: TVector3f;
begin
  if ManualChange then
  begin
    MoveAmount[0] := StrToFloatDef(EditIPLMultInstPosX.Text, 0);
    MoveAmount[1] := StrToFloatDef(EditIPLMultInstPosY.Text, 0);
    MoveAmount[2] := StrToFloatDef(EditIPLMultInstPosZ.Text, 0);
    MoveByAmount(MoveAmount);
    EditIPLMultInstPosX.Text := '0';
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIPLMultInstPosYChange(Sender: TObject);
var
  MoveAmount: TVector3f;
begin
  if ManualChange then
  begin
    MoveAmount[0] := StrToFloatDef(EditIPLMultInstPosX.Text, 0);
    MoveAmount[1] := StrToFloatDef(EditIPLMultInstPosY.Text, 0);
    MoveAmount[2] := StrToFloatDef(EditIPLMultInstPosZ.Text, 0);
    MoveByAmount(MoveAmount);
    EditIPLMultInstPosY.Text := '0';
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIPLMultInstPosZChange(Sender: TObject);
var
  MoveAmount: TVector3f;
begin
  if ManualChange then
  begin
    MoveAmount[0] := StrToFloatDef(EditIPLMultInstPosX.Text, 0);
    MoveAmount[1] := StrToFloatDef(EditIPLMultInstPosY.Text, 0);
    MoveAmount[2] := StrToFloatDef(EditIPLMultInstPosZ.Text, 0);
    MoveByAmount(MoveAmount);
    EditIPLMultInstPosZ.Text := '0';
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIPLMultInstPosXL1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_MULT_INST_POSX;
  TimerAmount := -DEC_FAST;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLMultInstPosYL1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_MULT_INST_POSY;
  TimerAmount := -DEC_FAST;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLMultInstPosZL1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_MULT_INST_POSZ;
  TimerAmount := -DEC_FAST;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLMultInstPosXL2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_MULT_INST_POSX;
  TimerAmount := -DEC_NORMAL;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLMultInstPosYL2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_MULT_INST_POSY;
  TimerAmount := -DEC_NORMAL;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLMultInstPosZL2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_MULT_INST_POSZ;
  TimerAmount := -DEC_NORMAL;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLMultInstPosXR2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_MULT_INST_POSX;
  TimerAmount := DEC_NORMAL;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLMultInstPosYR2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_MULT_INST_POSY;
  TimerAmount := DEC_NORMAL;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLMultInstPosZR2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_MULT_INST_POSZ;
  TimerAmount := DEC_NORMAL;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLMultInstPosXR1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_MULT_INST_POSX;
  TimerAmount := DEC_FAST;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLMultInstPosYR1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_MULT_INST_POSY;
  TimerAmount := DEC_FAST;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLMultInstPosZR1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerMode := TIMERMODE_MULT_INST_POSZ;
  TimerAmount := DEC_FAST;
  MoveTimer.Enabled := True;
  MoveTimerTimer(Self);
end;

procedure TFormEditorItem.EditIPLMultInstPosXL1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MoveTimer.Enabled := False;
end;

procedure TFormEditorItem.EditIPLMultInstPosXKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    EditIPLMultInstPosXChange(Self);
end;

procedure TFormEditorItem.EditIPLMultInstPosYKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    EditIPLMultInstPosYChange(Self);
end;

procedure TFormEditorItem.EditIPLMultInstPosZKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    EditIPLMultInstPosZChange(Self);
end;

procedure TFormEditorItem.EditIPLMultInstPosCenterClick(Sender: TObject);
begin
  MainGLView.JumpToLocation(GetPos);
end;

procedure TFormEditorItem.BtnIDETObjModelClick(Sender: TObject);
var
  TempStr: String;
begin
  if ManualChange then
  begin
      TempStr := ChangeFileExt(ExtractFileName(wnd_imgfilepicker.FileDialog(EditIDETObjModel.text, true)), '');

      with TIDEFile(GFiles.Item[CurrentFileIndex]).Tobj.Item[CurrentItemIndex] do
      begin
        ModelName := TempStr;
        ListAvailable := False; DffNum := -1;
      end;
      ManualChange := False;
      EditIDETObjModel.Text := TempStr;
      ManualChange := True;
      UpdateData(False, True);
      PerformIDETObjValidation;
    end;
end;

procedure TFormEditorItem.BtnIDETObjTextureClick(Sender: TObject);
var
  TempStr: String;
begin
  if ManualChange then
  begin
      TempStr := ChangeFileExt(ExtractFileName(wnd_imgfilepicker.FileDialog(EditIDETObjTexture.text, false)), '');
      with TIDEFile(GFiles.Item[CurrentFileIndex]).Tobj.Item[CurrentItemIndex] do
      begin
        TextureName := TempStr;
        ListAvailable := False; DffNum := -1;
      end;
      TIDEFile(GFiles.Item[CurrentFileIndex]).Tobj.CalcArchiveNums(CurrentItemIndex);
      ManualChange := False;
      EditIDETObjTexture.Text := TempStr;
      ManualChange := True;
      UpdateData(False, True);
      PerformIDETobjValidation;
    end;
end;

procedure TFormEditorItem.BtnIDEObjsModelClick(Sender: TObject);
var
  TempStr: String;
begin
  if ManualChange then
  begin
      TempStr := ChangeFileExt(ExtractFileName(wnd_imgfilepicker.FileDialog(EditIDEObjsModel.text, true)), '');

      with TIDEFile(GFiles.Item[CurrentFileIndex]).Objs.Item[CurrentItemIndex] do
      begin
        ModelName := TempStr;
        ListAvailable := False; DffNum := -1;
      end;
      ManualChange := False;
      EditIDEObjsModel.Text := TempStr;
      ManualChange := True;
      UpdateData(False, True);
      PerformIDEObjsValidation;
    end;
end;

procedure TFormEditorItem.BtnIDEObjsTextureClick(Sender: TObject);
var
  TempStr: String;
begin
  if ManualChange then
  begin
      TempStr := ChangeFileExt(ExtractFileName(wnd_imgfilepicker.FileDialog(EditIDEObjsTexture.text, false)), '');
      with TIDEFile(GFiles.Item[CurrentFileIndex]).Objs.Item[CurrentItemIndex] do
      begin
        TextureName := TempStr;
        ListAvailable := False; DffNum := -1;
      end;
      TIDEFile(GFiles.Item[CurrentFileIndex]).Objs.CalcArchiveNums(CurrentItemIndex);
      ManualChange := False;
      EditIDEObjsTexture.Text := TempStr;
      ManualChange := True;
      UpdateData(False, True);
      PerformIDEObjsValidation;
    end;
end;

procedure TFormEditorItem.FormShow(Sender: TObject);
var
  T, L: LongInt;
  Rct: TRect;
begin
  if FirstShow then
  begin
    GetWindowRect(MainGLView.Handle, Rct);
    L := Rct.Right - Width;
    T := Rct.Bottom - Height;
    if (T < 0) then
      T := 0;
    if (L < 0) then
      L := 0;
    Top := T;
    Left := L;
    FirstShow := False;
  end;
end;

procedure TFormEditorItem.FormCreate(Sender: TObject);
begin
  FirstShow := True;
  PageControl1.top:= PageControl1.top - 30;
  PageControl1.height:= PageControl1.height + 30;
end;

procedure TFormEditorItem.EditIDE2dfxRadioAnimationClick(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      EffectType := EFFECT_ANIMATION;
    UpdateData(True, True);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxRadioReflectionClick(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      EffectType := EFFECT_REFLECTION;
    UpdateData(True, True);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxViewDistanceChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      ViewDistance := StrToIntDef(EditIDE2dfxViewDistance.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxDTypeChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      DType := EditIDE2dfxDType.ItemIndex;
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIDE2dfxBTypeChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Wdfx.Item[CurrentItemIndex] do
      BType := EditIDE2dfxBType.ItemIndex;
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIPLPathItemsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if ManualChange then
  begin
    if (Item = nil) or (Selected = False) then
    begin
      PanelIPLPathItem.Visible := False;
    end else
    begin
      PanelIPLPathItem.Visible := True;
      ManualChange := False;
      with TIPLFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex].Item[Item.Index] do
      begin
        case NodeType of
          0: EditIPLPathItemTypeNone.Down := True;
          1: EditIPLPathItemTypeEnd.Down := True;
          2: EditIPLPathItemTypeMid.Down := True;
        end;
        EditIPLPathItemConnect.Text := IntToStr(NodeConnect);
        EditIPLPathItemU3.Text := IntToStr(U3);
        EditIPLPathItemU7.Text := Format('%1.6g', [U7]);

        EditIPLPathItemPosX.Text := Format('%1.6g', [Pos[0]]);
        EditIPLPathItemPosY.Text := Format('%1.6g', [Pos[1]]);
        EditIPLPathItemPosZ.Text := Format('%1.6g', [Pos[2]]);

        case LaneLeft of
          0: EditIPLPathItemLL0.Down := True;
          1: EditIPLPathItemLL1.Down := True;
          2: EditIPLPathItemLL2.Down := True;
        end;

        case LaneRight of
          0: EditIPLPathItemLR0.Down := True;
          1: EditIPLPathItemLR1.Down := True;
          2: EditIPLPathItemLR2.Down := True;
        end;

        EditIPLPathItemU10.Text := IntToStr(U10);
        EditIPLPathItemU11.Text := IntToStr(roadblock);
        EditIPLPathItemU12.Text := IntToStr(U12);
      end;
      ManualChange := True;
    end;
  end;
end;

procedure TFormEditorItem.EditIPLPathItemTypeNoneClick(Sender: TObject);
begin
  if ManualChange then
  begin
    TIPLFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex].Item[EditIPLPathItems.ItemIndex].NodeType := (sender as Tspeedbutton).tag;

    UpdateData(False, False);
    EditIPLPathDrawlist;
  end;
end;

procedure TFormEditorItem.EditIPLPathItemConnectChange(Sender: TObject);
begin
  if ManualChange then
  begin
    TIPLFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex].Item[EditIPLPathItems.ItemIndex].NodeConnect := StrToIntDef(EditIPLPathItemConnect.Text, 0);
    UpdateData(False, True);
    EditIPLPathDrawlist;
  end;
end;

procedure TFormEditorItem.EditIPLPathItemU3Change(Sender: TObject);
begin
  if ManualChange then
  begin
    TIPLFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex].Item[EditIPLPathItems.ItemIndex].U3 := StrToIntDef(EditIPLPathItemU3.Text, 0);
    UpdateData(False, False);
    EditIPLPathDrawlist;
  end;
end;

procedure TFormEditorItem.EditIPLPathItemU10Change(Sender: TObject);
begin
  if ManualChange then
  begin
    TIPLFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex].Item[EditIPLPathItems.ItemIndex].u10:= StrToIntDef(EditIPLPathItemU10.Text, 0);
    UpdateData(False, False);
    EditIPLPathDrawlist;
  end;
end;

procedure TFormEditorItem.EditIPLPathItemU11Change(Sender: TObject);
begin
  if ManualChange then
  begin
    TIPLFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex].Item[EditIPLPathItems.ItemIndex].roadblock := StrToIntDef(EditIPLPathItemU11.Text, 0);
    UpdateData(False, False);
    EditIPLPathDrawlist;
  end;
end;

procedure TFormEditorItem.EditIPLPathItemU12Change(Sender: TObject);
begin
  if ManualChange then
  begin
    TIPLFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex].Item[EditIPLPathItems.ItemIndex].U12 := StrToIntDef(EditIPLPathItemU12.Text, 0);
    UpdateData(False, False);
    EditIPLPathDrawlist;
  end;
end;

procedure TFormEditorItem.EditIPLPathItemU7Change(Sender: TObject);
begin
  if ManualChange then
  begin
    TIPLFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex].Item[EditIPLPathItems.ItemIndex].U7 := StrToFloatDef(EditIPLPathItemU7.Text, 0);
    UpdateData(False, False);
    EditIPLPathDrawlist;
  end;
end;

procedure TFormEditorItem.EditIPLPathItemPosXChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex].Item[EditIPLPathItems.ItemIndex] do begin
    Pos[0] := StrToFloatDef(EditIPLPathItemPosX.Text, 0);
    Pos[1] := StrToFloatDef(EditIPLPathItemPosY.Text, 0);
    Pos[2] := StrToFloatDef(EditIPLPathItemPosZ.Text, 0);
    end;

    UpdateData(False, False);
    EditIPLPathDrawlist;
  end;
end;

procedure TFormEditorItem.EditIPLPathOtherChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex] do
      PathOther := StrToIntDef(EditIPLPathOther.Text, 0);
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.EditIPLPathRadioCarClick(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex] do
      PathType := 1;
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.ide_bitsClickCheck(Sender: TObject);
var
i: integer;
b: longword;
begin

b:= 0;
for i:= 0 to ide_bits.Items.count-1 do
b:= setbit(b, i, ide_bits.Checked[i]);

  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).Objs.Item[CurrentItemIndex] do
    begin
      Flags:= b;

      ManualChange := False;
      EditIDEObjsFlags.Text := IntToStr(Flags);
      ManualChange := True;
    end;
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.PaintBox1Paint(Sender: TObject);
begin
ImageList1.Draw((sender as Tpaintbox).Canvas, 0, 0, (sender as Tpaintbox).tag);
end;

procedure TFormEditorItem.CheckListBox1ClickCheck(Sender: TObject);
var
i: integer;
b: longword;
begin

b:= 0;
for i:= 0 to ide_bits.Items.count-1 do
b:= setbit(b, i, ide_bits.Checked[i]);

  if ManualChange then
  begin
    with TIDEFile(GFiles.Item[CurrentFileIndex]).TObj.Item[CurrentItemIndex] do
    begin
      Flags:= b;

      ManualChange := False;
      EditIDEObjsFlags.Text := IntToStr(Flags);
      ManualChange := True;
    end;
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.BitBtn1Click(Sender: TObject);
begin
  with TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex] do
    MainGLView.JumpToLocation(Pos);
end;

procedure TFormEditorItem.BitBtn2Click(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex] do
    begin
      Rotation[0] := 0;
      Rotation[1] := 0;
      Rotation[2] := 0;
      Rotation[3] := 1;

      Update_Euler_angles;
    end;
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.box_zonetypesChange(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Zone.Item[CurrentItemIndex] do
      Sort := box_zonetypes.itemindex;
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.boxpathtypeClick(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex] do
      PathType := boxpathtype.itemindex;
    UpdateData(False, False);
  end;
end;

procedure TFormEditorItem.BitBtn3Click(Sender: TObject);
var
tmp: TVector3f;
begin
tmp:= TIPLFile(GFiles.Item[CurrentFileIndex]).path.Item[CurrentItemIndex].Item[EditIPLPathItems.itemindex].Pos;

tmp[0]:= tmp[0] * iplpathmp;
tmp[1]:= tmp[1] * iplpathmp;
tmp[2]:= tmp[2] * iplpathmp;

MainGLView.JumpToLocation(tmp);
end;

procedure TFormEditorItem.EditIPLPathItemLL0Click(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex].Item[EditIPLPathItems.ItemIndex] do
      LaneLeft := (sender as Tspeedbutton).Tag;
    UpdateData(False, False);
    EditIDEPathDrawlist;
  end;
end;

procedure TFormEditorItem.EditIPLPathItemLR0Click(Sender: TObject);
begin
  if ManualChange then
  begin
    with TIPLFile(GFiles.Item[CurrentFileIndex]).Path.Item[CurrentItemIndex].Item[EditIPLPathItems.ItemIndex] do
      LaneRight := (sender as Tspeedbutton).Tag;
    UpdateData(False, False);
    EditIDEPathDrawlist;
  end;
end;
procedure TFormEditorItem.Update_Euler_angles;
begin
  with TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex] do
  begin

  ManualChange := False;
  EditIPLInstRotX.Text := Format('%1.2f', [ArcTan2( (2*(Rotation[1]*Rotation[2]+Rotation[3]*Rotation[0]))  ,
                                      (Rotation[3]*Rotation[3]-Rotation[0]*Rotation[0]-Rotation[1]*Rotation[1]+Rotation[2]*Rotation[3])  ) * 180 / Pi]);
  EditIPLInstRotY.Text := Format('%1.2f', [ArcSin( -2*(Rotation[0]*Rotation[2]-Rotation[3]*Rotation[1])   ) * 180 / Pi]);
  EditIPLInstRotZ.Text := Format('%1.2f', [ArcTan2( (2*(Rotation[0]*Rotation[1]+Rotation[2]*Rotation[3]))  ,
                                      (Rotation[3]*Rotation[3]+Rotation[0]*Rotation[0]-Rotation[1]*Rotation[1]-Rotation[2]*Rotation[2])     ) * 180 / Pi]);
  ManualChange := True;
  end;
end;
procedure TFormEditorItem.Use_Euler_angles;
var
   sinx,siny,sinz,cosx,cosy,cosz:Single;
begin
  with TIPLFile(GFiles.Item[CurrentFileIndex]).Inst.Item[CurrentItemIndex] do
  begin
     sinx := Sin(StrToFloatDef(EditIPLInstRotX.Text,0)*PI/360); // sin(Rx/2)
     siny := Sin(StrToFloatDef(EditIPLInstRotY.Text,0)*PI/360); // sin(Ry/2)
     sinz := Sin(StrToFloatDef(EditIPLInstRotZ.Text,0)*PI/360); // sin(Rz/2)
     cosx := Cos(StrToFloatDef(EditIPLInstRotX.Text,0)*PI/360); // cos(Rx/2)
     cosy := Cos(StrToFloatDef(EditIPLInstRotY.Text,0)*PI/360); // cos(Ry/2)
     cosz := Cos(StrToFloatDef(EditIPLInstRotZ.Text,0)*PI/360); // cos(Rz/2)

     Rotation[0] := cosz*cosy*sinx - sinz*siny*cosx;
     Rotation[1] := sinz*cosy*sinx + cosz*siny*cosx;
     Rotation[2] := sinz*cosy*cosx - cosz*siny*sinx;
     Rotation[3] := cosz*cosy*cosx + sinz*siny*sinx;
  end;
end;
end.
