{
  Copyright (C) 2012 Tim Sinaeve tim.sinaeve@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit ts_Editor_Manager;

{$mode delphi}

{$region 'documentation' /fold}
{
  Datamodule holding common actions, menu's to manage one or more IEditorView
  instances.

  TODO:
   - apply consistent casing for word under cursor/all words ? => dangerous for strings

   - fix highlighter issues
   - fix DeleteView methods
   - fix SortText

   - show a hintwindow of the selection with the proposed operation!
   - list of all supported actions, category, shortcut, description (treeview)

   - make ts_Editor_Actions, ts_Editor_Menus, ts_Editor_Images ?
   - make ts_Editor_Highlighters, ts_Editor_Events and ts_Editor_Views?

   - xml treeview?
   - settings dialog (store settings in xml)

   - goto line
   - goto position

  BUGS:
   - createdesktoplink has problems with spaces in paths
   - Actions cannot be executed on toolforms!
   - fix codeshaper undo

   - copy to clipboard
      - as HTML object has a incomplete HTML closing tag
      - as RTF object does the same as copy as RTF text

  list of supported actions:

      actNew
      actOpen
      actSave
      actSaveAs
      actOpenSelectionInNewEditor

      actCut
      actCopy
      actPaste

      actUndo
      actDelete
      actSelectAll

      actFind
      actReplace
      actFindNext
      actFindPrevious
      actFindNextWordOccurence
      actFindPrevWordOccurence

      actIncFontSize
      actDecFontSize

      actAlignSelection

      actLowerCaseSelection
      actUpperCaseSelection

      actToggleComment
      actToggleHighlighter

      actToggleFoldLevel

      actSortSelection
      actSmartSelect
      actFormat

      actToggleHighlighter

      actShapeCode
      actFilterCode

      actInsertCharacterFromMap
      actInsertColorValue

      actShowControlCharacters

      actQuoteSelection
      actDeQuoteSelection
      actQuoteLines
      actQuoteLinesAndDelimit
      actPascalStringOfSelection

      actStoreFoldState
      actRestoreFoldState

      actShowPreview
      actInspect
      actAutoGuessHighlighter

      actClose
      actCloseOthers
}
{$endregion}

//*****************************************************************************

interface

uses
  Classes, SysUtils, Controls, ActnList, StdActns, Menus, Dialogs, Forms,
  Contnrs,

  // logging
  sharedloggerlcl,

  LCLType,

  SynEdit, SynEditHighlighter,

  SynHighlighterPas, SynHighlighterAny, SynHighlighterSQL, SynHighlighterLFM,
  SynHighlighterXML, SynMacroRecorder, SynExportHTML, SynHighlighterBat,
  SynHighlighterHTML, SynHighlighterCpp, SynHighlighterJava, SynHighlighterPerl,
  SynHighlighterPython, SynExportRTF, SynExportWiki, SynUniHighlighter,
  SynHighlighterPo,

  ts_Editor_Interfaces, ts_Editor_Resources, ts_Editor_SynHighlighterCollection,
  ts_Editor_View;

type
  TdmEditorManager = class(TDataModule, IEditorManager,
                                        IEditorActions,
                                        IEditorView, // active view
                                        IEditorViews,
                                        //IEditorToolView,  // needed?
                                        IEditorToolViews,
                                        IEditorEvents,
                                        IEditorCommands,
                                        IEditorMenus,
                                        IEditorSettings,
                                        IEditorSearchEngine)
    {$region 'designer controls' /fold}
    aclActions                    : TActionList;
    actAlignAndSortSelection      : TAction;
    actAlignSelection             : TAction;
    actAutoFormatXML              : TAction;
    actDequoteSelection           : TAction;
    actAutoGuessHighlighter       : TAction;
    actClose                      : TAction;
    actCloseOthers                : TAction;
    actAbout                      : TAction;
    actCopyFullPath               : TAction;
    actCopyFileName               : TAction;
    actCopyFilePath               : TAction;
    actEncodeBase64               : TAction;
    actDecodeBase64               : TAction;
    actClear                      : TAction;
    actCreateDesktopLink          : TAction;
    actExit                       : TAction;
    actCut                        : TAction;
    actDelete                     : TAction;
    actShowTest                   : TAction;
    actSelectAll                  : TAction;
    actUndo                       : TAction;
    actPaste                      : TAction;
    actStripMarkup                : TAction;
    actTestForm                   : TAction;
    actSyncEdit                   : TAction;
    actShowViews                  : TAction;
    actShowActions                : TAction;
    actMonitorChanges             : TAction;
    actRedo                       : TAction;
    actStripFirstChar             : TAction;
    actStripLastChar              : TAction;
    actSmartSelect                : TAction;
    actQuoteSelection             : TAction;
    actShowControlCharacters      : TAction;
    actCopy                       : TAction;
    actCopyHTMLTextToClipboard    : TAction;
    actCopyRTFTextToClipboard     : TAction;
    actCopyRTFToClipboard         : TAction;
    actCopytHTMLToClipboard       : TAction;
    actCopyToClipboard            : TAction;
    actCopyWikiTextToClipboard    : TAction;
    actCopyWikiToClipboard        : TAction;
    actDecFontSize                : TAction;
    actDequoteLines               : TAction;
    actExportToHTML               : TAction;
    actExportToRTF                : TAction;
    actExportToWiki               : TAction;
    actFilterCode                 : TAction;
    actFind                       : TAction;
    actFindNext                   : TAction;
    actFindNextWord               : TAction;
    actFindPrevious               : TAction;
    actFindPrevWord               : TAction;
    actFoldLevel0                 : TAction;
    actFoldLevel1                 : TAction;
    actFoldLevel10                : TAction;
    actFoldLevel2                 : TAction;
    actFoldLevel3                 : TAction;
    actFoldLevel4                 : TAction;
    actFoldLevel5                 : TAction;
    actFoldLevel6                 : TAction;
    actFoldLevel7                 : TAction;
    actFoldLevel8                 : TAction;
    actFoldLevel9                 : TAction;
    actFormat                     : TAction;
    actHelp                       : TAction;
    actIncFontSize                : TAction;
    actInsertCharacterFromMap     : TAction;
    actInsertColorValue           : TAction;
    actInspect                    : TAction;
    actLoadHighlighterFromFile    : TAction;
    actLowerCaseSelection         : TAction;
    actNew                        : TAction;
    actOpen                       : TAction;
    actOpenFileAtCursor           : TAction;
    actOpenSelectionInNewEditor   : TAction;
    actPageSetup                  : TAction;
    actPascalStringOfSelection    : TAction;
    actShowPreview                : TAction;
    actPrint                      : TAction;
    actPrintPreview               : TAction;
    actQuoteLines                 : TAction;
    actQuoteLinesAndDelimit       : TAction;
    actReload                     : TAction;
    actReplace                    : TAction;
    actSave                       : TAction;
    actSaveAs                     : TAction;
    actSettings                   : TAction;
    actShapeCode                  : TAction;
    actSortSelection              : TAction;
    actToggleComment              : TAction;
    actToggleFoldLevel            : TAction;
    actToggleHighlighter          : TAction;
    actUpperCaseSelection         : TAction;
    dlgColor                      : TColorDialog;
    dlgOpen                       : TOpenDialog;
    dlgSave                       : TSaveDialog;
    imlMain                       : TImageList;
    MenuItem1                     : TMenuItem;
    MenuItem10                    : TMenuItem;
    MenuItem102                   : TMenuItem;
    MenuItem103                   : TMenuItem;
    MenuItem11                    : TMenuItem;
    MenuItem12                    : TMenuItem;
    MenuItem13                    : TMenuItem;
    MenuItem14                    : TMenuItem;
    MenuItem15                    : TMenuItem;
    MenuItem16                    : TMenuItem;
    MenuItem17                    : TMenuItem;
    MenuItem18                    : TMenuItem;
    MenuItem19                    : TMenuItem;
    MenuItem2                     : TMenuItem;
    MenuItem20                    : TMenuItem;
    MenuItem21                    : TMenuItem;
    MenuItem22                    : TMenuItem;
    MenuItem23                    : TMenuItem;
    MenuItem24                    : TMenuItem;
    MenuItem25                    : TMenuItem;
    MenuItem26                    : TMenuItem;
    MenuItem27                    : TMenuItem;
    MenuItem28                    : TMenuItem;
    MenuItem29                    : TMenuItem;
    MenuItem3                     : TMenuItem;
    MenuItem30                    : TMenuItem;
    MenuItem31                    : TMenuItem;
    MenuItem32                    : TMenuItem;
    MenuItem33                    : TMenuItem;
    MenuItem34                    : TMenuItem;
    MenuItem35                    : TMenuItem;
    MenuItem36                    : TMenuItem;
    MenuItem37                    : TMenuItem;
    MenuItem38                    : TMenuItem;
    MenuItem39                    : TMenuItem;
    MenuItem4                     : TMenuItem;
    MenuItem40                    : TMenuItem;
    MenuItem41                    : TMenuItem;
    MenuItem42                    : TMenuItem;
    MenuItem43                    : TMenuItem;
    MenuItem44                    : TMenuItem;
    MenuItem45                    : TMenuItem;
    MenuItem46                    : TMenuItem;
    MenuItem47                    : TMenuItem;
    MenuItem48                    : TMenuItem;
    MenuItem49                    : TMenuItem;
    MenuItem5                     : TMenuItem;
    MenuItem50                    : TMenuItem;
    MenuItem51                    : TMenuItem;
    MenuItem52                    : TMenuItem;
    MenuItem53                    : TMenuItem;
    MenuItem54                    : TMenuItem;
    MenuItem55                    : TMenuItem;
    MenuItem56                    : TMenuItem;
    MenuItem57                    : TMenuItem;
    MenuItem58                    : TMenuItem;
    MenuItem59                    : TMenuItem;
    MenuItem6                     : TMenuItem;
    MenuItem60                    : TMenuItem;
    MenuItem61                    : TMenuItem;
    MenuItem62                    : TMenuItem;
    MenuItem63                    : TMenuItem;
    MenuItem64                    : TMenuItem;
    MenuItem65                    : TMenuItem;
    MenuItem66                    : TMenuItem;
    MenuItem7                     : TMenuItem;
    MenuItem70                    : TMenuItem;
    MenuItem71                    : TMenuItem;
    MenuItem72                    : TMenuItem;
    MenuItem73                    : TMenuItem;
    MenuItem74                    : TMenuItem;
    MenuItem75                    : TMenuItem;
    MenuItem76                    : TMenuItem;
    MenuItem77                    : TMenuItem;
    MenuItem78                    : TMenuItem;
    MenuItem79                    : TMenuItem;
    MenuItem8                     : TMenuItem;
    MenuItem80                    : TMenuItem;
    MenuItem81                    : TMenuItem;
    MenuItem82                    : TMenuItem;
    MenuItem83                    : TMenuItem;
    MenuItem84                    : TMenuItem;
    MenuItem85                    : TMenuItem;
    MenuItem86                    : TMenuItem;
    MenuItem87                    : TMenuItem;
    MenuItem88                    : TMenuItem;
    MenuItem89                    : TMenuItem;
    MenuItem9                     : TMenuItem;
    MenuItem90                    : TMenuItem;
    MenuItem91                    : TMenuItem;
    MenuItem92                    : TMenuItem;
    MenuItem93                    : TMenuItem;
    mniCopy                       : TMenuItem;
    mniCopyToClipboard            : TMenuItem;
    mniCopyToClipboardAsASCIIText : TMenuItem;
    mniCopyToClipboardAsHTMLObject: TMenuItem;
    mniCopyToClipboardAsHTMLText  : TMenuItem;
    mniCopyToClipboardAsRTFObject : TMenuItem;
    mniCopyToClipboardAsRTFText   : TMenuItem;
    mniCopyToClipboardAsTeXObject : TMenuItem;
    mniCopyToClipboardAsTeXText   : TMenuItem;
    mniCut                        : TMenuItem;
    mniExport                     : TMenuItem;
    mniExportToHTML               : TMenuItem;
    mniExportToRTF                : TMenuItem;
    mniExportToTeX                : TMenuItem;
    mniFindNext                   : TMenuItem;
    mniFindPrevious               : TMenuItem;
    mniPageSetup                  : TMenuItem;
    mniPaste                      : TMenuItem;
    mniPrint                      : TMenuItem;
    mniPrintPreview               : TMenuItem;
    mniReplace                    : TMenuItem;
    mniSelectAll                  : TMenuItem;
    mniSeperator1                 : TMenuItem;
    mniSeperator2                 : TMenuItem;
    mniSeperator3                 : TMenuItem;
    mniSeperator4                 : TMenuItem;
    mniSettings                   : TMenuItem;
    mnitSaveToFile                : TMenuItem;
    mniUndo                       : TMenuItem;
    ppmClipboard                  : TPopupMenu;
    ppmEditor                     : TPopupMenu;
    ppmEncoding                   : TPopupMenu;
    ppmExport                     : TPopupMenu;
    ppmFold                       : TPopupMenu;
    ppmHighLighters               : TPopupMenu;
    ppmLineBreakStyle             : TPopupMenu;
    SynAnySyn                     : TSynAnySyn;
    SynBatSyn                     : TSynBatSyn;
    SynCppSyn                     : TSynCppSyn;
    SynExporterHTML               : TSynExporterHTML;
    SynExporterRTF                : TSynExporterRTF;
    SynExporterWiki               : TSynExporterWiki;
    SynHTMLSyn                    : TSynHTMLSyn;
    SynJavaSyn                    : TSynJavaSyn;
    SynLFMSyn                     : TSynLFMSyn;
    SynMacroRecorder              : TSynMacroRecorder;
    SynPasSyn                     : TSynPasSyn;
    SynPerlSyn                    : TSynPerlSyn;
    SynPythonSyn                  : TSynPythonSyn;
    SynSQLSyn                     : TSynSQLSyn;
    SynUniSyn                     : TSynUniSyn;
    SynXMLSyn                     : TSynXMLSyn;
    {$endregion}

    {$region 'action handlers' /fold}
    procedure actAboutExecute(Sender: TObject);
    procedure actAlignAndSortSelectionExecute(Sender: TObject);
    procedure actAlignSelectionExecute(Sender: TObject);
    procedure actATOpenSelectionInNewEditorExecute(Sender: TObject);
    procedure actATSelectAllExecute(Sender: TObject);
    procedure actAutoFormatXMLExecute(Sender: TObject);
    procedure actAutoGuessHighlighterExecute(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actCloseOthersExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actCopyFileNameExecute(Sender: TObject);
    procedure actCopyFilePathExecute(Sender: TObject);
    procedure actCopyFullPathExecute(Sender: TObject);
    procedure actCopyHTMLTextToClipboardExecute(Sender: TObject);
    procedure actCopyRTFTextToClipboardExecute(Sender: TObject);
    procedure actCopyRTFToClipboardExecute(Sender: TObject);
    procedure actCopytHTMLToClipboardExecute(Sender: TObject);
    procedure actCopyToClipboardExecute(Sender: TObject);
    procedure actCopyWikiTextToClipboardExecute(Sender: TObject);
    procedure actCopyWikiToClipboardExecute(Sender: TObject);
    procedure actCreateDesktopLinkExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actDecFontSizeExecute(Sender: TObject);
    procedure actDecodeBase64Execute(Sender: TObject);
    procedure actDequoteLinesExecute(Sender: TObject);
    procedure actDequoteSelectionExecute(Sender: TObject);
    procedure actEncodeBase64Execute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actExportToHTMLExecute(Sender: TObject);
    procedure actExportToRTFExecute(Sender: TObject);
    procedure actExportToWikiExecute(Sender: TObject);
    procedure actFilterCodeExecute(Sender: TObject);
    procedure actFindNextExecute(Sender: TObject);
    procedure actFindNextWordExecute(Sender: TObject);
    procedure actFindPreviousExecute(Sender: TObject);
    procedure actFindPrevWordExecute(Sender: TObject);
    procedure actFoldLevel0Execute(Sender: TObject);
    procedure actFoldLevel10Execute(Sender: TObject);
    procedure actFoldLevel1Execute(Sender: TObject);
    procedure actFoldLevel2Execute(Sender: TObject);
    procedure actFoldLevel3Execute(Sender: TObject);
    procedure actFoldLevel4Execute(Sender: TObject);
    procedure actFoldLevel5Execute(Sender: TObject);
    procedure actFoldLevel6Execute(Sender: TObject);
    procedure actFoldLevel7Execute(Sender: TObject);
    procedure actFoldLevel8Execute(Sender: TObject);
    procedure actFoldLevel9Execute(Sender: TObject);
    procedure actFormatExecute(Sender: TObject);
    procedure actHelpExecute(Sender: TObject);
    procedure actHighlighterExecute(Sender: TObject);
    procedure actIncFontSizeExecute(Sender: TObject);
    procedure actInsertCharacterFromMapExecute(Sender: TObject);
    procedure actInsertColorValueExecute(Sender: TObject);
    procedure actInspectExecute(Sender: TObject);
    procedure actLoadHighlighterFromFileExecute(Sender: TObject);
    procedure actLowerCaseSelectionExecute(Sender: TObject);
    procedure actMonitorChangesExecute(Sender: TObject);
    procedure actNewExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actOpenFileAtCursorExecute(Sender: TObject);
    procedure actOpenSelectionInNewEditorExecute(Sender: TObject);
    procedure actPascalStringOfSelectionExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actQuoteLinesAndDelimitExecute(Sender: TObject);
    procedure actQuoteLinesExecute(Sender: TObject);
    procedure actQuoteSelectionExecute(Sender: TObject);
    procedure actRedoExecute(Sender: TObject);
    procedure actReloadExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSearchExecute(Sender: TObject);
    procedure actSearchReplaceExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure actShapeCodeExecute(Sender: TObject);
    procedure actShowActionsExecute(Sender: TObject);
    procedure actShowControlCharactersExecute(Sender: TObject);
    procedure actShowPreviewExecute(Sender: TObject);
    procedure actShowTestExecute(Sender: TObject);
    procedure actShowViewsExecute(Sender: TObject);
    procedure actSmartSelectExecute(Sender: TObject);
    procedure actSortSelectionExecute(Sender: TObject);
    procedure actStripFirstCharExecute(Sender: TObject);
    procedure actStripMarkupExecute(Sender: TObject);
    procedure actStripLastCharExecute(Sender: TObject);
    procedure actSyncEditExecute(Sender: TObject);
    procedure actTestFormExecute(Sender: TObject);
    procedure actToggleCommentExecute(Sender: TObject);
    procedure actToggleFoldLevelExecute(Sender: TObject);
    procedure actToggleHighlighterExecute(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure actUpperCaseSelectionExecute(Sender: TObject);
    procedure actEncodingExecute(Sender: TObject);
    procedure actLineBreakStyleExecute(Sender: TObject);
    {$endregion}

    {$region 'event handlers' /fold}
    procedure SynMacroRecorderStateChange(Sender: TObject);
    {$endregion}

  private
    FPersistSettings   : Boolean;
    FSynHighlighterPo  : TSynPoSyn;

    FOnChange              : TNotifyEvent;
    FOnMacroStateChange    : TMacroStateChangeEvent;
    FOnNewFile             : TNewFileEvent;
    FOnOpenFile            : TFileEvent;
    FOnSaveFile            : TFileEvent;
    FOnCaretPositionChange : TCaretPositionEvent;
    FOnStatusMessage       : TStatusMessageEvent;
    FOnStatusChange        : TStatusChangeEvent;
    FOnActiveViewChange    : TNotifyEvent;
    FSearchEngine          : IEditorSearchEngine;

    FChanged      : Boolean;
    FSettings     : IEditorSettings;
    FActiveView   : IEditorView;
    FViewList     : TEditorViewList;
    FToolViewList : TEditorToolViewList;
    FFormsCreated : Boolean;

    {$region 'property access methods' /fold}
    function GetActionList: TActionList;
    function GetActions: IEditorActions;
    function GetCommands: IEditorCommands;
    function GetEditor: TSynEdit;
    function GetEditorPopupMenu: TPopupMenu;
    function GetEditorViews: IEditorViews;
    function GetEncodingPopupMenu: TPopupMenu;
    function GetEvents: IEditorEvents;
    function GetExportPopupMenu: TPopupMenu;
    function GetFoldPopupMenu: TPopupMenu;
    function GetHighlighterPopupMenu: TPopupMenu;
    function GetItem(AName: string): TCustomAction;
    function GetLineBreakStylePopupMenu: TPopupMenu;
    function GetMenus: IEditorMenus;
    function GetOnActiveViewChange: TNotifyEvent;
    function GetOnCaretPositionChange: TCaretPositionEvent;
    function GetOnChange: TNotifyEvent;
    function GetOnMacroStateChange: TMacroStateChangeEvent;
    function GetOnNewFile: TNewFileEvent;
    function GetOnOpenFile: TFileEvent;
    function GetOnSaveFile: TFileEvent;
    function GetOnStatusChange: TStatusChangeEvent;
    function GetPersistSettings: Boolean;
    function GetSearchEngine: IEditorSearchEngine;
    function GetSettings: IEditorSettings;
    function GetActiveView: IEditorView;
    function GetHighlighters: THighlighters;
    function IEditorToolViews.GetCount = GetToolViewCount;
    function GetToolViewCount: Integer;
    function GetToolViewList: TEditorToolViewList;
    function GetToolViews: IEditorToolViews;
    function GetView(AIndex: Integer): IEditorView;
    function GetViewByName(AName: string): IEditorView;
    function IEditorViews.GetCount = GetViewCount;
    function GetViewCount: Integer;
    function GetViewList: TEditorViewList;
    function GetViews: IEditorViews;
    function IEditorToolViews.GetView = GetToolView;
    function GetToolView(AIndex: Integer): IEditorToolView;
    function IEditorToolViews.GetViewByName = GetToolViewByName;
    function GetToolViewByName(AName: string): IEditorToolView;
    procedure SetActiveView(AValue: IEditorView);
    procedure SetOnActiveViewChange(const AValue: TNotifyEvent);
    procedure SetOnCaretPositionChange(const AValue: TCaretPositionEvent);
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetOnMacroStateChange(const AValue: TMacroStateChangeEvent);
    procedure SetOnNewFile(const AValue: TNewFileEvent);
    procedure SetOnOpenFile(const AValue: TFileEvent);
    procedure SetOnSaveFile(const AValue: TFileEvent);
    procedure SetOnStatusChange(const AValue: TStatusChangeEvent);
    procedure SetPersistSettings(const AValue: Boolean);
    {$endregion}

    // event handlers
    procedure CodeFilterFilteredLineChange(
            Sender  : TObject;
            AIndex  : Integer;
      const ALine   : string;
      const AFilter : string
    );
    procedure EditorSettingsApplySettings(Sender: TObject);

    procedure InitializeHighlighters;
    procedure InitializePopupMenus;
    procedure InitializeActions;
    procedure RegisterHighlighters;
    procedure RegisterToolViews;

    procedure ApplyHighlighterAttributes;

  protected
    procedure ActiveViewChanged;

    procedure ExportLines(AFormat: string; AToClipBoard: Boolean = True;
      ANativeFormat: Boolean = True);
    procedure FormatCode;

    procedure InsertCharacterFromMap;
    procedure ApplySettings;

    { IEditorViews }
    function IEditorViews.Add = AddView;
    function IEditorViews.Delete = DeleteView;
    function IEditorViews.Clear = ClearViews;
    function AddView(
      const AName        : string = '';
      const AFileName    : string = '';
      const AHighlighter : string = ''
    ): IEditorView;
    function DeleteView(AIndex: Integer): Boolean; overload;
    function DeleteView(AView: IEditorView): Boolean; overload;
    function DeleteView(const AName: string): Boolean; overload;
    procedure ClearViews(AExceptActive: Boolean = False);

    { IEditorToolViews }
    procedure IEditorToolViews.Add = AddToolView;
    function IEditorToolViews.Delete = DeleteToolView;
    procedure AddToolView(AToolView: IEditorToolView);
    function DeleteToolView(AIndex: Integer): Boolean; overload;
    function DeleteToolView(AView: IEditorToolView): Boolean; overload;
    function DeleteToolView(const AName: string): Boolean; overload;

    { IEditorCommands }
    function SaveFile(const AFileName: string = ''): Boolean;
    procedure LoadFile;
    procedure OpenFileAtCursor;
    procedure ToggleHighlighter;
    procedure CreateDesktopLink;
    //procedure SortSelection;
    procedure InsertCharacter(const C: TUTF8Char);
    procedure AssignHighlighter(const AName: string);
    procedure CopyToClipboard;

    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

    procedure FindNext;
    procedure FindNextWordOccurrence(DirectionForward: Boolean);
    procedure FindPrevious;

    function ActivateView(const AName: string): Boolean;

    // event dispatch methods
    procedure DoActiveViewChange; virtual;
    procedure DoCaretPositionChange; virtual;
    procedure DoMacroStateChange(AState : TSynMacroState); virtual;
    procedure DoStatusMessage(AText: string); virtual;
    procedure DoStatusChange(AChanges: TSynStatusChanges); virtual;
    procedure DoChange; virtual;
    procedure DoModified; virtual;
    procedure DoSaveFile;
    procedure DoOpenFile;
    procedure DoNewFile(const AFileName: string = ''; const AText: string = '');
    function DoFindAndReplace: Integer;

    procedure UpdateActions;
    procedure UpdateEncodingActions;
    procedure UpdateLineBreakStyleActions;
    procedure UpdateHighLighterActions;
    procedure UpdateFileActions;
    procedure UpdateSearchMatches;

    procedure ClearHighlightSearch;

    // TSI temp
    function ActiveToolView: IEditorToolView;

    // properties
    property ActionList: TActionList
      read GetActionList;

    property Items[AName: string]: TCustomAction
      read GetItem; default;

    { Set/get the reference to the active view. }
    property ActiveView: IEditorView
      read GetActiveView write SetActiveView {implements IEditorView};

    property EditorPopupMenu: TPopupMenu
      read GetEditorPopupMenu;

    property EncodingPopupMenu: TPopupMenu
      read GetEncodingPopupMenu;

    property LineBreakStylePopupMenu: TPopupMenu
      read GetLineBreakStylePopupMenu;

    property ExportPopupMenu: TPopupMenu
      read GetExportPopupMenu;

    property FoldPopupMenu: TPopupMenu
      read GetFoldPopupMenu;

    property HighlighterPopupMenu: TPopupMenu
      read GetHighlighterPopupMenu;

    property Highlighters: THighlighters
      read GetHighlighters;

    property PersistSettings: Boolean
      read GetPersistSettings write SetPersistSettings;

    property Views[AIndex: Integer]: IEditorView
      read GetView;

    property ToolViews: IEditorToolViews
      read GetToolViews;

    property ViewByName[AName: string]: IEditorView
      read GetViewByName;

    property ViewList: TEditorViewList
      read GetViewList;

    property ViewCount: Integer
      read GetViewCount;

    property ToolViewList: TEditorToolViewList
      read GetToolViewList;

    property ToolViewCount: Integer
      read GetToolViewCount;

    property OnCaretPositionChange: TCaretPositionEvent
      read GetOnCaretPositionChange write SetOnCaretPositionChange;

    property OnStatusChange: TStatusChangeEvent
      read GetOnStatusChange write SetOnStatusChange;

    property OnChange: TNotifyEvent
      read GetOnChange write SetOnChange;

    property OnMacroStateChange: TMacroStateChangeEvent
      read GetOnMacroStateChange write SetOnMacroStateChange;

    property OnOpenFile: TFileEvent
      read GetOnOpenFile write SetOnOpenFile;

    property OnNewFile: TNewFileEvent
      read GetOnNewFile write SetOnNewFile;

    property OnSaveFile: TFileEvent
      read GetOnSaveFile write SetOnSaveFile;

    property OnActiveViewChange: TNotifyEvent
      read GetOnActiveViewChange write SetOnActiveViewChange;

    { IEditorManager }
    property Settings: IEditorSettings
      read GetSettings implements IEditorSettings;

    property Events: IEditorEvents
      read GetEvents;

    property Menus: IEditorMenus
      read GetMenus;

    property Commands: IEditorCommands
      read GetCommands;

    property View: IEditorView
      read GetActiveView implements IEditorView;

    property SearchEngine: IEditorSearchEngine
      read GetSearchEngine implements IEditorSearchEngine;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

function EditorManager : IEditorManager;

//*****************************************************************************

implementation

{$R *.lfm}

uses
  FileUtil, Clipbrd, StrUtils, Math, ShlObj, Windows, Graphics,

  LConvEncoding, Base64,

  SynEditTypes, SynPluginSyncroEdit,

  ts_Core_Utils, ts_Core_ComponentInspector,

  ts_Editor_Settings, ts_Editor_Utils,
  ts_Editor_ViewListForm, ts_Editor_CodeShaperForm , ts_Editor_PreviewForm,
  ts_Editor_Testform, ts_Editor_SearchForm, ts_Editor_ShortcutsDialog,
  ts_Editor_ActionListViewForm, ts_Editor_SettingsDialog,
  ts_Editor_CodeFilterDialog, ts_Editor_CharacterMapDialog,
  ts_Editor_AlignLinesForm, ts_Editor_AboutDialog,

  ts_Editor_CodeFormatters, ts_Editor_SearchEngine;

var
  dmEditorManager: TdmEditorManager;

{$region 'interfaced methods' /fold}
//*****************************************************************************
// interfaced methods                                                    BEGIN
//*****************************************************************************

function EditorManager : IEditorManager;
begin
  if not dmEditorManager.FFormsCreated then
    dmEditorManager.RegisterToolViews;
  Result := dmEditorManager;
end;

//*****************************************************************************
// interfaced methods                                                      END
//*****************************************************************************
{$endregion}

{$region 'construction and destruction' /fold}
//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TdmEditorManager.AfterConstruction;
begin
  inherited AfterConstruction;
  FPersistSettings := False;
  FSettings        := TEditorSettings.Create(Self);
  FViewList        := TEditorViewList.Create;
  FToolViewList    := TEditorToolViewList.Create;
  FSearchEngine    := TSearchEngine.Create(Self);
  FSynHighlighterPo := TSynPoSyn.Create(Self);

  InitializeHighlighters;
  RegisterHighlighters;

  InitializeActions;
  InitializePopupMenus;
end;

procedure TdmEditorManager.BeforeDestruction;
begin
  if PersistSettings then
    FSettings.Save;
  FSearchEngine := nil;
  FActiveView := nil; // !!!!!!!!! after a long search this was a long lasting bug
  FSettings := nil;
  FreeAndNil(FSynHighlighterPo);
  FreeAndNil(FViewList);
  FreeAndNil(FToolViewList);
  inherited BeforeDestruction;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************
{$endregion}

{$region 'property access methods' /fold}
//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

function TdmEditorManager.GetEditor: TSynEdit;
begin
  if Assigned(ActiveView) then
    Result := ActiveView.Editor
  else
    Result := nil;
end;

function TdmEditorManager.GetEditorPopupMenu: TPopupMenu;
begin
  Result := ppmEditor;
end;

function TdmEditorManager.GetEditorViews: IEditorViews;
begin
  Result := Self as IEditorViews;
end;

function TdmEditorManager.GetEncodingPopupMenu: TPopupMenu;
begin
  Result := ppmEncoding;
end;

function TdmEditorManager.GetEvents: IEditorEvents;
begin
  Result := Self as IEditorEvents;
end;

function TdmEditorManager.GetExportPopupMenu: TPopupMenu;
begin
  Result := ppmExport;
end;

function TdmEditorManager.GetFoldPopupMenu: TPopupMenu;
begin
  Result := ppmFold;
end;

function TdmEditorManager.GetHighlighterPopupMenu: TPopupMenu;
begin
  Result := ppmHighLighters;
end;

function TdmEditorManager.GetActionList: TActionList;
begin
  Result := aclActions;
end;

procedure TdmEditorManager.ApplyHighlighterAttributes;
var
  K: Integer;
  J: Integer;
  I: Integer;
  A: TSynHighlighterAttributes;
begin
  for I := 0 to Settings.Highlighters.Count - 1 do
  begin
    for J := 0 to Settings.HighlighterAttributes.Count - 1 do
    begin
      for K := 0 to Settings.Highlighters.Items[I].SynHighlighter.AttrCount - 1 do
      begin
        A := Settings.Highlighters.Items[I].SynHighlighter.Attribute[K];
        if A.Name = Settings.HighlighterAttributes.Items[J].Name then
          A.Assign(Settings.HighlighterAttributes.Items[J].Attributes);
      end;
    end;
  end;
end;

function TdmEditorManager.GetActions: IEditorActions;
begin
  Result := Self as IEditorActions;
end;

function TdmEditorManager.GetCommands: IEditorCommands;
begin
  Result := Self as IEditorCommands;
end;

function TdmEditorManager.GetItem(AName: string): TCustomAction;
begin
  Result := aclActions.ActionByName(AName) as TCustomAction;
end;

function TdmEditorManager.GetLineBreakStylePopupMenu: TPopupMenu;
begin
  Result := ppmLineBreakStyle;
end;

function TdmEditorManager.GetMenus: IEditorMenus;
begin
  Result := Self as IEditorMenus;
end;

function TdmEditorManager.GetOnActiveViewChange: TNotifyEvent;
begin
  Result := FOnActiveViewChange;
end;

function TdmEditorManager.GetOnCaretPositionChange: TCaretPositionEvent;
begin
  Result := FOnCaretPositionChange;
end;

function TdmEditorManager.GetOnChange: TNotifyEvent;
begin
  Result := FOnChange;
end;

function TdmEditorManager.GetOnMacroStateChange: TMacroStateChangeEvent;
begin
  Result := FOnMacroStateChange;
end;

function TdmEditorManager.GetOnNewFile: TNewFileEvent;
begin
  Result := FOnNewFile;
end;

function TdmEditorManager.GetOnOpenFile: TFileEvent;
begin
  Result := FOnOpenFile;
end;

function TdmEditorManager.GetOnSaveFile: TFileEvent;
begin
  Result := FOnSaveFile;
end;

function TdmEditorManager.GetOnStatusChange: TStatusChangeEvent;
begin
  Result := FOnStatusChange;
end;

function TdmEditorManager.GetPersistSettings: Boolean;
begin
  Result := FPersistSettings;
end;

function TdmEditorManager.GetSearchEngine: IEditorSearchEngine;
begin
  Result := FSearchEngine;
end;

procedure TdmEditorManager.SetPersistSettings(const AValue: Boolean);
begin
  if AValue <> PersistSettings then
  begin
    if AValue then
    begin
      FSettings.Load;
      // TSI voorlopig
      RegisterHighlighters;

    end;
    FPersistSettings := AValue;
  end;
end;

procedure TdmEditorManager.SetOnCaretPositionChange(const AValue: TCaretPositionEvent);
begin
  FOnCaretPositionChange := AValue;
end;

procedure TdmEditorManager.SetOnChange(const AValue: TNotifyEvent);
begin
  FOnChange := AValue;
end;

procedure TdmEditorManager.SetOnMacroStateChange(const AValue: TMacroStateChangeEvent);
begin
  FOnMacroStateChange := AValue;
end;

procedure TdmEditorManager.SetOnNewFile(const AValue: TNewFileEvent);
begin
  FOnNewFile := AValue;
end;

procedure TdmEditorManager.SetOnOpenFile(const AValue: TFileEvent);
begin
  FOnOpenFile := AValue;
end;

procedure TdmEditorManager.SetOnSaveFile(const AValue: TFileEvent);
begin
  FOnSaveFile := AValue;
end;

procedure TdmEditorManager.SetOnStatusChange(const AValue: TStatusChangeEvent);
begin
  FOnStatusChange := AValue;
end;

function TdmEditorManager.GetSettings: IEditorSettings;
begin
  Result := FSettings;
end;

function TdmEditorManager.GetActiveView: IEditorView;
begin
  Result := FActiveView;
end;

procedure TdmEditorManager.SetActiveView(AValue: IEditorView);
begin
  if AValue <> FActiveView then
  begin
    FActiveView := AValue;
    DoActiveViewChange;
    ActiveViewChanged;
  end;
end;

procedure TdmEditorManager.SetOnActiveViewChange(const AValue: TNotifyEvent);
begin
  FOnActiveViewChange := AValue;
end;

function TdmEditorManager.GetHighlighters: THighlighters;
begin
  Result := Settings.Highlighters;
end;

function TdmEditorManager.GetToolViewCount: Integer;
begin
  if Assigned(FToolViewList) then
    Result := FToolViewList.Count
  else
    Result := 0;
end;

function TdmEditorManager.GetToolViewList: TEditorToolViewList;
begin
  Result := FToolViewList;
end;

function TdmEditorManager.GetToolViews: IEditorToolViews;
begin
  Result := Self as IEditorToolViews;
end;

function TdmEditorManager.GetView(AIndex: Integer): IEditorView;
begin
  if (AIndex > -1) and (AIndex < FViewList.Count) then
  begin
    Result := FViewList[AIndex] as IEditorView;
  end
  else
    Result := nil;
end;

{ If the view is not found the active view is returned. }

function TdmEditorManager.GetViewByName(AName: string): IEditorView;
var
  I: Integer;
  B: Boolean;
begin
  I := 0;
  B := False;
  while (I < FViewList.Count) and not B do
  begin
    B := Views[I].Name = AName;
    if not B then
      Inc(I);
  end;
  if B then
    Result := FViewList[I] as IEditorView
  else
    Result := ActiveView;
end;

function TdmEditorManager.GetViewCount: Integer;
begin
  if Assigned(FViewList) then
    Result := FViewList.Count
  else
    Result := 0;
end;

function TdmEditorManager.GetViewList: TEditorViewList;
begin
  Result := FViewList;
end;

function TdmEditorManager.GetViews: IEditorViews;
begin
  Result := Self as IEditorViews;
end;

function TdmEditorManager.GetToolView(AIndex: Integer): IEditorToolView;
begin
  Result := FToolViewList[AIndex] as IEditorToolView;
end;

function TdmEditorManager.GetToolViewByName(AName: string): IEditorToolView;
var
  I : Integer;
begin
  Result := nil;
  for I := 0 to FToolViewList.Count - 1 do
  begin
    if (FToolViewList[I] as IEditorToolView).Name = AName then
      Result := FToolViewList[I] as IEditorToolView;
  end;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************
{$endregion}

{$region 'event dispatch methods' /fold}
//*****************************************************************************
// event dispatch methods                                                BEGIN
//*****************************************************************************

procedure TdmEditorManager.DoActiveViewChange;
begin
  if Assigned(FOnActiveViewChange) then
    FOnActiveViewChange(Self);
end;

procedure TdmEditorManager.DoCaretPositionChange;
begin
  if Assigned(FOnCaretPositionChange) then
    FOnCaretPositionChange(Self, ActiveView.CaretX, ActiveView.CaretY);

  // TODO TSI!!!!

  if actShowPreview.Checked then
    ToolViews['frmPreview'].UpdateView;
  if ToolViews['frmTest'].Visible then
    ToolViews['frmTest'].UpdateView;
end;

procedure TdmEditorManager.DoMacroStateChange(AState: TSynMacroState);
begin
  if Assigned(FOnMacroStateChange) then
    FOnMacroStateChange(Self, AState);
end;

procedure TdmEditorManager.DoStatusMessage(AText: string);
begin
  if Assigned(FOnStatusMessage) then
    FOnStatusMessage(Self, AText);
end;

procedure TdmEditorManager.DoStatusChange(AChanges: TSynStatusChanges);
begin
  if Assigned(FOnStatusChange) then
    FOnStatusChange(Self, AChanges);
end;

procedure TdmEditorManager.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
  FChanged := True;
end;

procedure TdmEditorManager.DoModified;
begin
  if not ActiveView.Modified then
    ActiveView.Modified := True;
end;

procedure TdmEditorManager.DoSaveFile;
var
  S: string;
begin
  if Assigned(FOnSaveFile) then
  begin
    S := ActiveView.FileName;
    FOnSaveFile(Self, S);
    ActiveView.FileName := S;
  end;
end;

procedure TdmEditorManager.DoOpenFile;
var
  S: string;
begin
  if Assigned(FOnOpenFile) then
  begin
    S := ActiveView.FileName;
    FOnOpenFile(Self, S);
    ActiveView.FileName := S;
  end;
end;

procedure TdmEditorManager.DoNewFile(const AFileName: string; const AText: string);
var
  S : string;
begin
  S  := AFileName;
  if Assigned(FOnNewFile) then
    FOnNewFile(Self, S, AText);
  if S <> '' then
    ActiveView.FileName := S;
end;

{ TSI TODO: integrate visualisation logic in searchengine }

function TdmEditorManager.DoFindAndReplace: Integer;
//var
//  OldCaretXY : TPoint;
//  NewTopLine : Integer;
begin
  Result := 0;
  //if (ssoReplace in FFindReplaceDialog.Options) and Settings.ReadOnly then
  //begin
  //  Exit;
  //end;
  //
  //OldCaretXY := ActiveView.CaretXY;
  //if ActiveView.SelAvail and not (ssoSelectedOnly in FFindReplaceDialog.Options) then
  //begin
  //  // Adjust the cursor. to exclude the selection from being searched
  //  // needed for find next / find previous
  //  if ssoBackwards in FFindReplaceDialog.Options then
  //    ActiveView.LogicalCaretXY := ActiveView.BlockBegin
  //  else
  //    ActiveView.LogicalCaretXY := ActiveView.BlockEnd;
  //end;
  //
  //// TSI : TODO
  //Result := ActiveView.Editor.SearchReplace(
  //  FFindReplaceDialog.FindText,
  //  FFindReplaceDialog.ReplaceText,
  //  FFindReplaceDialog.Options
  //);
  //
  //if (OldCaretXY.X = ActiveView.CaretX) and (OldCaretXY.Y = ActiveView.CaretY) and
  //  not (ssoReplaceAll in FFindReplaceDialog.Options) then
  //begin
  //  //    ACaption := lisUENotFound;
  //  //AText := Format(lisUESearchStringNotFound, [ValidUTF8String(FFindReplaceDialog.FindText)]);
  //  //MessageDlg(ACaption, AText, mtInformation, [mbOk], 0);
  //  //TSourceNotebook(Owner).DeleteLastJumpPointClicked(Self);
  //end
  //else if (ActiveView.CaretY <= ActiveView.TopLine + 1)
  //  or (ActiveView.CaretY >= ActiveView.TopLine + ActiveView.LinesInWindow - 1) then
  //begin
  //  NewTopLine := ActiveView.CaretY - (ActiveView.LinesInWindow div 2);
  //  if NewTopLine < 1 then
  //    NewTopLine := 1;
  //  ActiveView.TopLine := NewTopLine;
  //end;
end;

//*****************************************************************************
// event dispatch methods                                                  END
//*****************************************************************************
{$endregion}

{$region 'action handlers' /fold}
//*****************************************************************************
// action handlers                                                       BEGIN
//*****************************************************************************

procedure TdmEditorManager.actSortSelectionExecute(Sender: TObject);
begin
  ShowMessage('TODO');
  //SortSelection;
end;

procedure TdmEditorManager.actToggleCommentExecute(Sender: TObject);
begin
  ActiveView.UpdateCommentSelection(False, True);
end;

procedure TdmEditorManager.actToggleHighlighterExecute(Sender: TObject);
begin
  ToggleHighlighter;
end;

procedure TdmEditorManager.actUndoExecute(Sender: TObject);
begin
  ActiveView.Undo;
end;

procedure TdmEditorManager.actUpperCaseSelectionExecute(Sender: TObject);
begin
  ActiveView.UpperCaseSelection;
end;

procedure TdmEditorManager.actOpenExecute(Sender: TObject);
begin
  if Assigned(ActiveView) and Assigned(ActiveView.Editor.Highlighter) then
    dlgOpen.Filter := ActiveView.Editor.Highlighter.DefaultFilter;
  if dlgOpen.Execute then
  begin
    ActiveView.Lines.LoadFromFile(dlgOpen.FileName);
    ActiveView.FileName := dlgOpen.FileName;
    AssignHighlighter(GuessHighlighterType(ActiveView.Text));
  end;
end;

procedure TdmEditorManager.actPascalStringOfSelectionExecute(Sender: TObject);
begin
  ActiveView.SelText := PascalStringOf(ActiveView.SelText);
  DoModified;
end;

procedure TdmEditorManager.actPasteExecute(Sender: TObject);
begin
  if ActiveView.Focused then
    ActiveView.Paste
  else if Assigned(ActiveToolView) then
    (ActiveToolView as IClipboardCommands).Paste;
end;

procedure TdmEditorManager.actQuoteLinesAndDelimitExecute(Sender: TObject);
begin
  ActiveView.SelText := QuoteLinesAndDelimit(ActiveView.SelText);
  DoModified;
end;

procedure TdmEditorManager.actQuoteLinesExecute(Sender: TObject);
begin
  ActiveView.SelText := QuoteLines(ActiveView.SelText);
  DoModified;
end;

procedure TdmEditorManager.actSaveExecute(Sender: TObject);
begin
  SaveFile(ActiveView.FileName);
end;

procedure TdmEditorManager.actSaveAsExecute(Sender: TObject);
begin
  SaveFile;
end;

procedure TdmEditorManager.actSearchExecute(Sender: TObject);
begin
  // handled by owner
end;

procedure TdmEditorManager.actSearchReplaceExecute(Sender: TObject);
begin
  // handled by owner
end;

procedure TdmEditorManager.actShapeCodeExecute(Sender: TObject);
begin
  // handled by owner
end;

procedure TdmEditorManager.actSelectAllExecute(Sender: TObject);
begin
  ActiveView.SelectAll;
end;

procedure TdmEditorManager.actExportToHTMLExecute(Sender: TObject);
begin
  ExportLines('HTML', False, False);
end;

procedure TdmEditorManager.actExportToWikiExecute(Sender: TObject);
begin
  ExportLines('WIKI', False, False);
end;

procedure TdmEditorManager.actLowerCaseSelectionExecute(Sender: TObject);
begin
  ActiveView.LowerCaseSelection;
end;

procedure TdmEditorManager.actExportToRTFExecute(Sender: TObject);
begin
  ExportLines(HL_RTF, False, False);
end;

procedure TdmEditorManager.actCopytHTMLToClipboardExecute(Sender: TObject);
begin
  ExportLines('HTML');
end;

procedure TdmEditorManager.actCopyWikiToClipboardExecute(Sender: TObject);
begin
  ExportLines('WIKI');
end;

procedure TdmEditorManager.actCreateDesktopLinkExecute(Sender: TObject);
begin
  CreateDesktopLink;
end;

procedure TdmEditorManager.actCutExecute(Sender: TObject);
begin
  if ActiveView.Focused then
    ActiveView.Cut
  else if Assigned(ActiveToolView) then
    (ActiveToolView as IClipboardCommands).Cut;
end;

procedure TdmEditorManager.actCopyRTFToClipboardExecute(Sender: TObject);
begin
  ExportLines(HL_RTF);
end;

procedure TdmEditorManager.actCopyRTFTextToClipboardExecute(Sender: TObject);
begin
  ExportLines(HL_RTF, True, False);
end;

procedure TdmEditorManager.actCopyWikiTextToClipboardExecute(Sender: TObject);
begin
  ExportLines('WIKI', True, False);
end;

procedure TdmEditorManager.actCopyHTMLTextToClipboardExecute(Sender: TObject);
begin
  ExportLines('HTML', True, False);
end;

procedure TdmEditorManager.actCopyToClipboardExecute(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TdmEditorManager.actFilterCodeExecute(Sender: TObject);
begin
  if Assigned(ToolViews['frmCodeFilterDialog']) then
  begin
    ToolViews['frmCodeFilterDialog'].Visible := True;
    ToolViews['frmCodeFilterDialog'].UpdateView;
    FChanged := True;
  end;
end;

procedure TdmEditorManager.actHelpExecute(Sender: TObject);
begin
  ShortCuts.Show;
end;

procedure TdmEditorManager.actInsertColorValueExecute(Sender: TObject);
begin
  dlgColor.Execute;
  ActiveView.InsertTextAtCaret(IntToStr(Integer(dlgColor.Color)));
end;

procedure TdmEditorManager.actInspectExecute(Sender: TObject);
begin
  //(FEditorSettings as IInterfaceComponentReference).GetComponent;
  InspectComponent((ActiveView.Editor as IInterfaceComponentReference).GetComponent);
end;

procedure TdmEditorManager.actLoadHighlighterFromFileExecute(Sender: TObject);
begin
  //dlgOpen.Filter := '*.hgl';
  //if dlgOpen.Execute then
  //begin
  //  SynUniSyn.LoadFromFile(dlgOpen.FileName);
  //end;
  //HighlighterType := shlUNI;
end;

procedure TdmEditorManager.actNewExecute(Sender: TObject);
var
  S: string;
begin
  if Assigned(ActiveView) then
    S := ActiveView.SelText;
  DoNewFile('<new>', S);
end;

procedure TdmEditorManager.actOpenSelectionInNewEditorExecute(Sender: TObject);
var
  S : string;
  T : string;
begin
  S := '';
  if Assigned(FOnNewFile) then
  begin
    if ActiveView.Focused then
    begin
      T := ActiveView.SelText;
    end;
    DoNewFile(S, T);
  end;
end;

procedure TdmEditorManager.actReloadExecute(Sender: TObject);
begin
  ActiveView.LoadFromFile(ActiveView.FileName);
end;

procedure TdmEditorManager.actSmartSelectExecute(Sender: TObject);
begin
   ActiveView.SmartSelect;
end;

procedure TdmEditorManager.actStripFirstCharExecute(Sender: TObject);
begin
  ActiveView.SelText := StripChars(ActiveView.SelText, True, False);
end;

procedure TdmEditorManager.actStripMarkupExecute(Sender: TObject);
begin
  ActiveView.SelText := StripMarkup(ActiveView.SelText);
end;

procedure TdmEditorManager.actStripLastCharExecute(Sender: TObject);
begin
  ActiveView.SelText := StripChars(ActiveView.SelText, False, True);
end;

procedure TdmEditorManager.actSyncEditExecute(Sender: TObject);
begin
  ActiveView.Editor.CommandProcessor(ecSynPSyncroEdStart, '', nil);
end;

procedure TdmEditorManager.actTestFormExecute(Sender: TObject);
begin
// TODO
end;

procedure TdmEditorManager.actToggleFoldLevelExecute(Sender: TObject);
begin
  ActiveView.FoldLevel := (ActiveView.FoldLevel + 1) mod 11;
end;

procedure TdmEditorManager.actFindNextExecute(Sender: TObject);
begin
  FindNext;
end;

procedure TdmEditorManager.actFindPreviousExecute(Sender: TObject);
begin
  FindPrevious;
end;

procedure TdmEditorManager.actFoldLevel0Execute(Sender: TObject);
begin
  ActiveView.FoldLevel := 0;
end;

procedure TdmEditorManager.actFoldLevel1Execute(Sender: TObject);
begin
  ActiveView.FoldLevel := 1;
end;

procedure TdmEditorManager.actFoldLevel2Execute(Sender: TObject);
begin
  ActiveView.FoldLevel := 2;
end;

procedure TdmEditorManager.actFoldLevel3Execute(Sender: TObject);
begin
  ActiveView.FoldLevel := 3;
end;

procedure TdmEditorManager.actFoldLevel4Execute(Sender: TObject);
begin
  ActiveView.FoldLevel := 4;
end;

procedure TdmEditorManager.actFoldLevel5Execute(Sender: TObject);
begin
  ActiveView.FoldLevel := 5;
end;

procedure TdmEditorManager.actFoldLevel6Execute(Sender: TObject);
begin
  ActiveView.FoldLevel := 6;
end;

procedure TdmEditorManager.actFoldLevel7Execute(Sender: TObject);
begin
  ActiveView.FoldLevel := 7;
end;

procedure TdmEditorManager.actFoldLevel8Execute(Sender: TObject);
begin
  ActiveView.FoldLevel := 8;
end;

procedure TdmEditorManager.actFoldLevel9Execute(Sender: TObject);
begin
  ActiveView.FoldLevel := 9;
end;

procedure TdmEditorManager.actFoldLevel10Execute(Sender: TObject);
begin
  ActiveView.FoldLevel := 10;
end;

procedure TdmEditorManager.actFormatExecute(Sender: TObject);
begin
  try
   FormatCode;
  except
    // do nothing
  end;
end;

procedure TdmEditorManager.actIncFontSizeExecute(Sender: TObject);
begin
  ActiveView.AdjustFontSize(1);
end;

procedure TdmEditorManager.actInsertCharacterFromMapExecute(Sender: TObject);
begin
//  InsertCharacterFromMap;
  ShowMessage('TODO');
end;

procedure TdmEditorManager.actAlignSelectionExecute(Sender: TObject);
begin
  // TSI TODO: smart mode.
  //  - detect common tokens on multiple lines, show dialog with possibilities
  //    if multiple tokens are possible
  //ActiveView.SelText := AlignLines(ActiveView.SelText, ':', True, True, True);
end;

procedure TdmEditorManager.actATOpenSelectionInNewEditorExecute(Sender: TObject);
{ TODO : yet to implement }
//var
//  S : string;
//  T : string;
begin
  //S := '';
  //if Assigned(FOnNewFile) then
  //begin
  //  if mmoPreview.SelText = '' then
  //    T := Editor.LineText
  //  else
  //    T := mmoPreview.SelText;
  //  FOnNewFile(Self, S, T);
  //end;
end;

procedure TdmEditorManager.actATSelectAllExecute(Sender: TObject);
begin
  //mmoPreview.SelectAll;
end;

procedure TdmEditorManager.actAutoFormatXMLExecute(Sender: TObject);
begin
  (Sender as TAction).Checked := not (Sender as TAction).Checked;
  Settings.AutoFormatXML := (Sender as TAction).Checked;
end;

procedure TdmEditorManager.actAlignAndSortSelectionExecute(Sender: TObject);
var
  S: string;
begin
  S := ActiveView.SelText;
  //S := S Text(S, sdAscending, sdLines, False, True);
  S := AlignLines(S, ':', True, True, True);
  // TODO: TrimRight is needed because AlignLines adds an extra #13#10 to the string
  ActiveView.SelText := TrimRight(S);
end;

procedure TdmEditorManager.actShowPreviewExecute(Sender: TObject);
begin
  Settings.PreviewVisible := (Sender as TAction).Checked;
  ApplySettings;
end;

procedure TdmEditorManager.actShowTestExecute(Sender: TObject);
begin
  ToolViews['frmTest'].Visible := not ToolViews['frmTest'].Visible;
end;

procedure TdmEditorManager.actShowViewsExecute(Sender: TObject);
begin
  ToolViews['frmViewList'].Visible := True;
end;

procedure TdmEditorManager.actMonitorChangesExecute(Sender: TObject);
begin
  (Sender as TAction).Checked := not (Sender as TAction).Checked;
  ActiveView.MonitorChanges := (Sender as TAction).Checked;
end;

procedure TdmEditorManager.actOpenFileAtCursorExecute(Sender: TObject);
begin
  OpenFileAtCursor;
end;

procedure TdmEditorManager.actQuoteSelectionExecute(Sender: TObject);
begin
  ActiveView.SelText := QuotedStr(ActiveView.SelText);
  DoModified;
end;

procedure TdmEditorManager.actRedoExecute(Sender: TObject);
begin
  ActiveView.Redo;
end;

procedure TdmEditorManager.actSettingsExecute(Sender: TObject);
begin
  ExecuteSettingsDialog(FSettings, EditorSettingsApplySettings);
end;

procedure TdmEditorManager.actShowActionsExecute(Sender: TObject);
begin
  ToolViews['frmActionListView'].Visible := True;
end;

procedure TdmEditorManager.actHighlighterExecute(Sender: TObject);
var
  A: TAction;
begin
  A := Sender as TAction;
  AssignHighlighter(A.Caption);
end;

procedure TdmEditorManager.actDequoteLinesExecute(Sender: TObject);
begin
  ActiveView.SelText := DequoteLines(ActiveView.SelText);
  DoModified;
end;

procedure TdmEditorManager.actAutoGuessHighlighterExecute(Sender: TObject);
begin
  AssignHighlighter(GuessHighlighterType(ActiveView.Text));
end;

procedure TdmEditorManager.actClearExecute(Sender: TObject);
begin
  ActiveView.Clear;
end;

procedure TdmEditorManager.actAboutExecute(Sender: TObject);
begin
  ShowAboutDialog;
end;

procedure TdmEditorManager.actCloseExecute(Sender: TObject);
begin
  {TODO: make a new event handler }
  if ViewCount > 1 then
    DeleteView(ActiveView);
end;

procedure TdmEditorManager.actCloseOthersExecute(Sender: TObject);
begin
  ClearViews(True);
end;

procedure TdmEditorManager.actCopyFileNameExecute(Sender: TObject);
begin
  Clipboard.AsText := ExtractFileName(ActiveView.FileName);
end;

procedure TdmEditorManager.actCopyFilePathExecute(Sender: TObject);
begin
  Clipboard.AsText := ExtractFilePath(ActiveView.FileName);
end;

procedure TdmEditorManager.actCopyFullPathExecute(Sender: TObject);
begin
  Clipboard.AsText := ActiveView.FileName;
end;

procedure TdmEditorManager.actDequoteSelectionExecute(Sender: TObject);
begin
  ActiveView.SelText := AnsiDequotedStr(ActiveView.SelText, '''');
  DoModified;
end;

procedure TdmEditorManager.actEncodeBase64Execute(Sender: TObject);
begin
  ActiveView.SelText := EncodeStringBase64(ActiveView.SelText);
  DoModified;
end;

procedure TdmEditorManager.actExitExecute(Sender: TObject);
begin
   // TODO: FIX THIS!!!
//  ClearViews(True);
  Application.Terminate;
end;

procedure TdmEditorManager.actFindNextWordExecute(Sender: TObject);
begin
  FindNextWordOccurrence(True);
end;

procedure TdmEditorManager.actFindPrevWordExecute(Sender: TObject);
begin
  FindNextWordOccurrence(False);
end;

procedure TdmEditorManager.actCopyExecute(Sender: TObject);
begin
  if ActiveView.Focused then
    ActiveView.Copy
  else if Assigned(ActiveToolView) then
    (ActiveToolView as IClipboardCommands).Copy;
end;

procedure TdmEditorManager.actDecFontSizeExecute(Sender: TObject);
begin
  ActiveView.AdjustFontSize(-1);
end;

procedure TdmEditorManager.actDecodeBase64Execute(Sender: TObject);
begin
  ActiveView.SelText := DecodeStringBase64(ActiveView.SelText);
  DoModified;
end;

procedure TdmEditorManager.actShowControlCharactersExecute(Sender: TObject);
begin
  (Sender as TAction).Checked := not (Sender as TAction).Checked;
  Settings.ShowControlCharacters := (Sender as TAction).Checked;
  ApplySettings;
end;

procedure TdmEditorManager.actEncodingExecute(Sender: TObject);
begin
  ActiveView.Encoding := (Sender as TAction).Caption;
end;

procedure TdmEditorManager.actLineBreakStyleExecute(Sender: TObject);
begin
  ActiveView.LineBreakStyle := (Sender as TAction).Caption;
end;

//*****************************************************************************
// action handlers                                                         END
//*****************************************************************************
{$endregion}

{$region 'event handlers' /fold}
//*****************************************************************************
// event handlers                                                        BEGIN
//*****************************************************************************

procedure TdmEditorManager.SynMacroRecorderStateChange(Sender: TObject);
begin
  DoMacroStateChange(SynMacroRecorder.State);
end;

procedure TdmEditorManager.CodeFilterFilteredLineChange(Sender: TObject;
  AIndex: Integer; const ALine: string; const AFilter: string);
begin
  ActiveView.SearchAndSelectLine(AIndex, ALine);
end;

procedure TdmEditorManager.EditorSettingsApplySettings(Sender: TObject);
begin
  ApplySettings;
end;

//*****************************************************************************
// event handlers                                                          END
//*****************************************************************************
{$endregion}

{$region 'private methods' /fold}
//*****************************************************************************
// private methods                                                       BEGIN
//*****************************************************************************

{$region 'Initialization' /fold}
{ Initializes extra information related to the built-in highlighters like
  folding configuration and devider info. }

procedure TdmEditorManager.InitializeHighlighters;
var
  I: Integer;
  N: Integer;
begin
  SynPasSyn.AddSpecialAttribute('');
  for I := Low(EditorOptionsDividerInfoPas) to High(EditorOptionsDividerInfoPas) do
  begin
    SynPasSyn.DividerDrawConfig[I].MaxDrawDepth :=
      EditorOptionsDividerInfoPas[I].MaxLevel;
  end;

  for I := Low(EditorOptionsFoldInfoPas) to High(EditorOptionsFoldInfoPas) do
  begin
    N := EditorOptionsFoldInfoPas[I].Index;
    if N >= 0 then
      SynPasSyn.FoldConfig[N].Enabled := EditorOptionsFoldInfoPas[I].Enabled;
  end;

  for I := Low(EditorOptionsFoldInfoXML) to High(EditorOptionsFoldInfoXML) do
  begin
    N := EditorOptionsFoldInfoXML[I].Index;
    if N >= 0 then
      SynXMLSyn.FoldConfig[N].Enabled := EditorOptionsFoldInfoXML[I].Enabled;
  end;

  for I := Low(EditorOptionsFoldInfoLFM) to High(EditorOptionsFoldInfoLFM) do
  begin
    N := EditorOptionsFoldInfoLFM[I].Index;
    if N >= 0 then
      SynLFMSyn.FoldConfig[N].Enabled := EditorOptionsFoldInfoLFM[I].Enabled;
  end;

  for I := Low(EditorOptionsFoldInfoHTML) to High(EditorOptionsFoldInfoHTML) do
  begin
    N := EditorOptionsFoldInfoHTML[I].Index;
    if N >= 0 then
      SynHTMLSyn.FoldConfig[N].Enabled := EditorOptionsFoldInfoHTML[I].Enabled;
  end;

  FSynHighlighterPo.CommentAttri.Foreground := clNavy;
  FSynHighlighterPo.CommentAttri.Style := [fsItalic];
  FSynHighlighterPo.KeyAttri.Foreground := clBlue;
  FSynHighlighterPo.KeyAttri.Style := [fsBold];
  FSynHighlighterPo.StringAttribute.ForeGround := clGreen;
  FSynHighlighterPo.StringAttribute.Style := [fsBold, fsItalic];
end;

procedure TdmEditorManager.InitializePopupMenus;
var
  SL: TStringList;
  S : string;
  MI: TMenuItem;
  HI: THighlighterItem;
  A : TCustomAction;
  I : Integer;
begin
  ppmHighLighters.Items.Caption := 'Highlighters';
  ppmHighLighters.Items.Action := actToggleHighlighter;
  ppmFold.Items.Caption := 'Folding';
  ppmFold.Items.Action := actToggleFoldLevel;
  ppmEditor.Items.Insert(15, ppmHighLighters.Items);
  ppmEditor.Items.Insert(16, ppmFold.Items);

  SL := TStringList.Create;
  try
    ppmEncoding.Items.Clear;
    GetSupportedEncodings(SL);
    for S in SL do
    begin
      MI := TMenuItem.Create(ppmEncoding);
      MI.Caption := S;
      S := 'actEncoding' + DelChars(S, '-');
      A := Items[S];
      if Assigned(A) then
      begin
        MI.Action     := A;
        MI.AutoCheck  := A.AutoCheck;
        MI.RadioItem  := True;
        MI.GroupIndex := A.GroupIndex;
      end;
      ppmEncoding.Items.Add(MI);
    end;
  finally
    FreeAndNil(SL);
  end;

  ppmLineBreakStyle.Items.Clear;
  for S in ALineBreakStyles do
  begin
    MI := TMenuItem.Create(ppmLineBreakStyle);
    S := 'actLineBreakStyle' +  S;
    A := Items[S];
    if Assigned(A) then
    begin
      MI.Action     := A;
      MI.Caption    := A.Caption;
      MI.AutoCheck  := A.AutoCheck;
      MI.RadioItem  := True;
      MI.GroupIndex := A.GroupIndex;
    end;
    ppmLineBreakStyle.Items.Add(MI);
  end;

  ppmHighLighters.Items.Clear;
  for I := 0 to Highlighters.Count -1 do
  begin
    HI := Highlighters[I];
    MI := TMenuItem.Create(ppmHighLighters);
    S := 'actHighlighter' + HI.Name;
    A := Items[S];
    if Assigned(A) then
    begin
      MI.Action     := A;
      MI.Hint       := HI.Description;
      MI.Caption    := A.Caption;
      MI.AutoCheck  := A.AutoCheck;
      MI.RadioItem  := True;
      MI.GroupIndex := A.GroupIndex;
    end;
    ppmHighLighters.Items.Add(MI);
  end;

end;

procedure TdmEditorManager.InitializeActions;
var
  A : TAction;
  SL: TStringList;
  S : string;
  I : Integer;
  HI: THighlighterItem;
begin
  SL := TStringList.Create;
  try
    GetSupportedEncodings(SL);
    for S in SL do
    begin
      A := TAction.Create(ActionList);
      A.ActionList := ActionList;
      A.Caption := S;
      A.Name    := 'actEncoding' + DelChars(S, '-');
      A.AutoCheck := True;
      A.GroupIndex := 3;
      A.Category := 'Encoding';
      A.OnExecute  := actEncodingExecute;
    end;
    for S in ALineBreakStyles do
    begin
      A := TAction.Create(aclActions);
      A.ActionList := ActionList;
      A.Caption := S;
      A.Name    := 'actLineBreakStyle' + S;
      A.AutoCheck := True;
      A.GroupIndex := 4;
      A.Category := 'LineBreakStyle';
      A.OnExecute  := actLineBreakStyleExecute;
    end;
    for I := 0 to Highlighters.Count - 1 do
    begin
      HI := Highlighters[I];
      A.Tag := I;
      A := TAction.Create(ActionList);
      A.ActionList := ActionList;
      A.Caption := HI.Name;
      A.Name := 'actHighlighter' + HI.Name;
      A.AutoCheck := True;
      A.GroupIndex := 5;
      A.Category := 'Highlighter';
      A.OnExecute   := actHighlighterExecute;
    end;
  finally
    FreeAndNil(SL);
  end;
end;
{$endregion}

{$region 'Registration' /fold}
{ TODO: create the instances dynamically. }
procedure TdmEditorManager.RegisterHighlighters;

  procedure Reg(ASynHighlighterClass: TSynHighlighterClass;
    ASynHighlighter: TSynCustomHighlighter; const AName: string;
    const AFileExtensions: string = ''; const ADescription: string = '';
    const ACommentType: TCommentType = ctNone; ACodeFormatter: ICodeFormatter = nil;
    const ALayoutFileName: string = '');
  begin
    Highlighters.RegisterHighlighter(
      ASynHighlighterClass,
      ASynHighlighter,
      AName,
      AFileExtensions,
      ACommentType,
      ACodeFormatter,
      ADescription,
      ALayoutFileName
    );
  end;

begin
  Highlighters.Clear;
  Reg(TSynAnySyn, SynAnySyn, 'None');
  Reg(TSynAnySyn, SynAnySyn, HL_TXT, FILE_EXTENSIONS_TXT, STXTDescription);
  Reg(TSynPasSyn, SynPasSyn, HL_PAS, FILE_EXTENSIONS_PAS, SPASDescription, ctPascal, TPascalFormatter.Create);
  Reg(TSynSQLSyn, SynSQLSyn, HL_SQL, FILE_EXTENSIONS_SQL, SSQLDescription, ctCPP, TSQLFormatter.Create);
  Reg(TSynXMLSyn, SynXMLSyn, HL_XML, FILE_EXTENSIONS_XML, SXMLDescription, ctHtml, TXMLFormatter.Create);
  Reg(TSynLFMSyn, SynLFMSyn, HL_LFM, FILE_EXTENSIONS_LFM, SLFMDescription);
  Reg(TSynUniSyn, SynUniSyn, HL_BaltaLOG, 'log', SBaltaLOGDescription, ctNone, nil, LAYOUT_BALTALOG);
  Reg(TSynUniSyn, SynUniSyn, HL_INI, FILE_EXTENSIONS_INI, SINIDescription, ctNone, nil, LAYOUT_INI);
  Reg(TSynBatSyn, SynBatSyn, HL_BAT, FILE_EXTENSIONS_BAT, SBATDescription);
  Reg(TSynUniSyn, FSynHighlighterPo, HL_PO, FILE_EXTENSIONS_PO, SPODescription);
  Reg(TSynCppSyn, SynCppSyn, HL_CPP, FILE_EXTENSIONS_CPP, SCPPDescription, ctCPP, TCPPFormatter.Create);
  Reg(TSynJavaSyn, SynJavaSyn, HL_JAVA, FILE_EXTENSIONS_JAVA, SJavaDescription, ctCPP, TJavaFormatter.Create);
  Reg(TSynPerlSyn, SynPerlSyn, HL_PERL, FILE_EXTENSIONS_PERL, SPERLDescription);
  Reg(TSynPythonSyn, SynPythonSyn, HL_PY, FILE_EXTENSIONS_PY, SPYDescription);
  Reg(TSynHTMLSyn, SynHTMLSyn, HL_HTML, FILE_EXTENSIONS_HTML, SHTMLDescription,ctHtml, THTMLFormatter.Create);
  Reg(TSynUniSyn, SynUniSyn, HL_RTF, FILE_EXTENSIONS_RTF, SRTFDescription, ctNone, nil, LAYOUT_RTF);
  Reg(TSynUniSyn, SynUniSyn, HL_RES, FILE_EXTENSIONS_RES, SRESDescription, ctNone, nil, LAYOUT_RES);
  ApplyHighlighterAttributes;
end;

procedure TdmEditorManager.RegisterToolViews;
begin
  AddToolView(TfrmCodeShaper.Create(Self));
  AddToolView(TfrmSearchForm.Create(Self));
  AddToolView(TfrmViewList.Create(Self));
  AddToolView(TfrmPreview.Create(Self));
  AddToolView(TfrmActionListView.Create(Self));
  AddToolView(TfrmTest.Create(Self));
  AddToolView(TfrmAlignLines.Create(Self));
  AddToolView(TfrmCodeFilterDialog.Create(Self));

  (ToolViews['frmCodeFilterDialog'] as IEditorCodeFilter).OnFilteredLineChange :=
    CodeFilterFilteredLineChange;

  FFormsCreated := True;
end;
{$endregion}

//*****************************************************************************
// private methods                                                         END
//*****************************************************************************
{$endregion}

{$region 'protected methods' /fold}
//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

procedure TdmEditorManager.ActiveViewChanged;
begin
  UpdateHighLighterActions;
  UpdateEncodingActions;
  UpdateLineBreakStyleActions;
  UpdateFileActions;
  UpdateSearchMatches;
end;

{ Apply stored settings to the views and actions. }

procedure TdmEditorManager.ApplySettings;
begin
  ActiveView.ShowSpecialChars := Settings.ShowControlCharacters;
end;

procedure TdmEditorManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and Supports(AComponent, IEditorView) then
  begin
    DeleteView(AComponent as IEditorView);
  end;
  inherited Notification(AComponent, Operation);
end;

{$region 'IEditorActions' /fold}
function TdmEditorManager.AddView(const AName: string; const AFileName: string;
  const AHighlighter: string): IEditorView;
var
  V : IEditorView;
begin
  V := TEditorView.Create(Self);
  // if no name is provided, the view will get an automatically generated one.
  /// HUH!??? TSI
  if AName <> '' then
    V.Name := AName;
  V.FileName := AFileName;
  V.AssignHighlighter(AHighlighter);
  V.Form.Caption := '';
  ViewList.Add(V);
  Result := V;
end;

function TdmEditorManager.DeleteView(AIndex: Integer): Boolean;
var
  I : Integer;
begin
  if (AIndex > -1) and (AIndex < ViewCount) {and (ViewCount > 1)} then
  begin
    I := ViewList.IndexOf(ActiveView);
    if I = AIndex then // select a new active view
    begin
      I := IfThen(I > 0, I - 1, 1);
      Views[I].Activate;
    end;
    Views[AIndex].Close;
    ViewList.Delete(AIndex);
    Result := True;
  end
  else
    Result := False;
end;

{ 1. Removes the given instance from the list
  2. Closes the instance (which will free it)
  3. Set the active view to another view if we were closing the active view
}

function TdmEditorManager.DeleteView(AView: IEditorView): Boolean;
var
  I : Integer;
begin
  if Assigned(AView) then
  begin
    I := ViewList.IndexOf(AView);
    if I > -1 then
    begin
      if AView = ActiveView then
      begin
        AView.Form.Close;
        ViewList.Delete(I);
        Views[0].Activate;
        Result := True;
      end
      else
      begin
        AView.Form.Close;
        ViewList.Delete(I);
      end;
    end
    else
      Result := False;
  end
  else
    Result := False;
end;

function TdmEditorManager.DeleteView(const AName: string): Boolean;
begin
  Result := DeleteView(ViewByName[AName]);
end;

{ Closes and clears all views in the list (except for the active view when
  AExceptActive is True).


  TODO: Does not work when AExeptActive is False}

procedure TdmEditorManager.ClearViews(AExceptActive: Boolean);
var
  I: Integer;
begin
  if AExceptActive then
  begin
    I := ViewList.IndexOf(ActiveView);
    ViewList.Delete(I);
  end;
  while ViewCount > 0 do
    DeleteView(0);
  ViewList.Clear;
  if AExceptActive then
    ViewList.Add(ActiveView);
end;

procedure TdmEditorManager.AddToolView(AToolView: IEditorToolView);
begin
  AToolView.Visible := False;
  FToolViewList.Add(AToolView);
end;

function TdmEditorManager.DeleteToolView(AIndex: Integer): Boolean;
begin
  FToolViewList.Delete(AIndex);
end;

function TdmEditorManager.DeleteToolView(AView: IEditorToolView): Boolean;
var
  I : Integer;
begin
  I := FToolViewList.IndexOf(AView);
  if I <> -1 then
  begin
    FToolViewList.Delete(I);
  end
  else
    Result := False;
end;

function TdmEditorManager.DeleteToolView(const AName: string): Boolean;
var
  TV: IEditorToolView;
begin
  TV := GetToolViewByName(AName);
  Result := False;
  if Assigned(TV) then
    Result := DeleteToolView(TV);
end;

procedure TdmEditorManager.LoadFile;
begin
  DoOpenFile;
  // reload file from disk
end;
{$endregion}

{$region 'IEditorCommands' /fold}
procedure TdmEditorManager.AssignHighlighter(const AName: string);
begin
  ActiveView.HighlighterItem := Highlighters.ItemsByName[AName];
end;

procedure TdmEditorManager.InsertCharacter(const C: TUTF8Char);
begin
  ActiveView.InsertTextAtCaret(C);
end;

procedure TdmEditorManager.CopyToClipboard;
begin
  ActiveView.Editor.CopyToClipboard;
end;

procedure TdmEditorManager.ExportLines(AFormat: string; AToClipBoard: Boolean;
  ANativeFormat: Boolean);
var
  S  : string;
  SL : TStringList;
begin
  SL := TStringList.Create;
  try
    if AFormat = 'HTML' then
    begin
      SynExporterHTML.Highlighter := ActiveView.Editor.Highlighter;
      SynExporterHTML.ExportAsText := not ANativeFormat;
      SynExporterHTML.Font.Assign(ActiveView.Editor.Font);
      if ActiveView.SelAvail then
        SL.Text := ActiveView.SelText
      else
        SL.Text := ActiveView.Text;
      SynExporterHTML.ExportAll(SL);
      if AToClipboard then
        SynExporterHTML.CopyToClipboard
      else
      begin
        S := dlgSave.Filter;
        dlgSave.Filter := SynExporterHTML.DefaultFilter;
        dlgSave.FileName := ExtractFileNameWithoutExt(Settings.FileName) + '.html';
        if dlgSave.Execute then
          SynExporterHTML.SaveToFile(dlgSave.FileName);
        dlgSave.Filter := S;
      end;
    end
    else if AFormat = HL_RTF then
    begin
      SynExporterRTF.Highlighter := ActiveView.Editor.Highlighter;
      SynExporterRTF.ExportAsText := not ANativeFormat;
      SynExporterRTF.Font.Assign(ActiveView.Editor.Font);
      if ActiveView.SelAvail then
        SL.Text := ActiveView.SelText
      else
        SL.Text := ActiveView.Text;
      SynExporterRTF.ExportAll(SL);
      if AToClipboard then
        SynExporterRTF.CopyToClipboard
      else
      begin
        S := dlgSave.Filter;
        dlgSave.Filter := SynExporterRTF.DefaultFilter;
        dlgSave.FileName := ExtractFileNameWithoutExt(Settings.FileName) + '.rtf';
        if dlgSave.Execute then
          SynExporterRTF.SaveToFile(dlgSave.FileName);
        dlgSave.Filter := S;
      end;
    end
    else if AFormat = 'WIKI' then
    begin
      SynExporterWiki.Highlighter := ActiveView.Editor.Highlighter;
      SynExporterWiki.ExportAsText := not ANativeFormat;
      if ActiveView.SelAvail then
        SL.Text := ActiveView.SelText
      else
        SL.Text := ActiveView.Text;
      SynExporterWiki.ExportAll(SL);
      if AToClipboard then
        SynExporterWiki.CopyToClipboard
      else
      begin
        S := dlgSave.Filter;
        dlgSave.Filter := SynExporterWiki.DefaultFilter;
        dlgSave.FileName := ExtractFileNameWithoutExt(Settings.FileName) + '.txt';
        if dlgSave.Execute then
          SynExporterWiki.SaveToFile(dlgSave.FileName);
        dlgSave.Filter := S;
      end;
    end;
  finally
    SL.Free;
  end;
end;

{ Saves the content of the active editorview to the given filename. If the
  given filename does not exist or is empty, the user is prompted to enter a
  name with the save file dialog. }

function TdmEditorManager.SaveFile(const AFileName: string): Boolean;
begin
  DoSaveFile;
  if FileExists(AFileName) then
  begin
    ActiveView.FileName := AFileName;
    ActiveView.SaveToFile(AFileName);
    Result := True;
  end
  else
  begin
    if Assigned(ActiveView.Editor.Highlighter) then
      dlgSave.Filter := ActiveView.Editor.Highlighter.DefaultFilter;
    dlgSave.FileName := AFileName;
    if dlgSave.Execute then
    begin
      ActiveView.FileName := dlgSave.FileName;
      ActiveView.SaveToFile(dlgSave.FileName);
      Result := True;
    end
    else
      Result := False;
  end;
end;

function TdmEditorManager.ActivateView(const AName: string): Boolean;
var
  V: IEditorView;
begin
  V := ViewByName[AName];
  if  Assigned(V) then
  begin
    ViewByName[AName].Activate;
    Result := True;
  end
  else
    Result := False;
end;

{ Formats the (selected if applicable) code using the associated code formatter
  for the current highlighter. }

procedure TdmEditorManager.FormatCode;
var
  N: Integer;
  S: string;
begin
  if Assigned(ActiveView.HighlighterItem.CodeFormatter) then
  begin
    try
      ActiveView.BeginUpdate;
      actFormat.Enabled := False;
      try
        S := IfThen(ActiveView.SelAvail, ActiveView.SelText, ActiveView.Text);
        S := ActiveView.HighlighterItem.CodeFormatter.Format(S);
        if ActiveView.SelAvail then
        begin
          N := ActiveView.SelStart;
          ActiveView.Editor.ClearSelection;
          ActiveView.Editor.InsertTextAtCaret(Trim(S));
          ActiveView.SelStart := N;
          ActiveView.SelEnd := ActiveView.SelStart + Length(Trim(S));
        end
        else
        begin
          ActiveView.SelectAll;
          ActiveView.Editor.ClearSelection;
          ActiveView.Editor.InsertTextAtCaret(Trim(S));
        end;
      finally
        ActiveView.EndUpdate;
      end;
    finally
      actFormat.Enabled := True;
    end;
  end
  else
    raise Exception.Create('No codeformatter for current highlighter');
end;

{ TODO -oTSI : Charmap needs to be a tool window }

procedure TdmEditorManager.InsertCharacterFromMap;
begin
  ShowCharacterMap(InsertCharacter);
end;

procedure TdmEditorManager.OpenFileAtCursor;
var
  FN : string;
begin
  FN := ExtractFilePath(ActiveView.FileName)
    + ActiveView.CurrentWord + ExtractFileExt(ActiveView.FileName);
  if FileExists(FN) then
    DoNewFile(FN);
end;

procedure TdmEditorManager.CreateDesktopLink;
var
  PIDL     : LPItemIDList;
  InFolder : array[0..MAX_PATH] of Char;
  SL       : TShellLink;
begin
  SHGetSpecialFolderLocation(0, CSIDL_DESKTOPDIRECTORY, PIDL) ;
  SHGetPathFromIDList(PIDL, InFolder) ;
  SL.Filename := InFolder + '\' + ExtractFileName(ActiveView.FileName) + '.lnk';
  SL.WorkingDir := ExtractFilePath(SL.Filename);
  SL.ShortcutTo := Application.ExeName;
  SL.Parameters := ActiveView.FileName;
  CreateShellLink(SL);
end;

procedure TdmEditorManager.ToggleHighlighter;
var
  I: Integer;
  N: Integer;
begin
  if Assigned(ActiveView.HighlighterItem) then
  begin
    I := ActiveView.HighlighterItem.Index;
    N := Highlighters.Count;
    ActiveView.HighlighterItem := Highlighters[(I + 1) mod N];
    Settings.HighlighterType := ActiveView.HighlighterItem.Name;
  end;
end;
{$endregion}

{$region 'UpdateActions' /fold}
procedure TdmEditorManager.UpdateActions;
var
  B: Boolean;
  V: IEditorView;
begin
  V := ActiveView;
  if Assigned(V) and Assigned(Settings) {and V.Focused and FChanged} then
  begin
    B := V.SelAvail and not Settings.ReadOnly;
    actAlignAndSortSelection.Enabled    := B;
    actAlignSelection.Enabled           := B;
    actDequoteSelection.Enabled         := B;
    actLowerCaseSelection.Enabled       := B;
    actOpenSelectionInNewEditor.Enabled := B;
    actPascalStringOfSelection.Enabled  := B;
    actStripMarkup.Enabled              := B;
    actQuoteSelection.Enabled           := B;
    actQuoteLinesAndDelimit.Enabled     := B;
    actSortSelection.Enabled            := B;
//    actToggleComment.Enabled   := B;
    actUpperCaseSelection.Enabled       := B;
    actStripFirstChar.Enabled           := B;
    actStripLastChar.Enabled            := B;
    actQuoteLines.Enabled               := B;
    actDequoteLines.Enabled             := B;
    actEncodeBase64.Enabled             := B;
    actDecodeBase64.Enabled             := B;
    actSyncEdit.Enabled                 := B;

    B := not Settings.ReadOnly;
    actAlignSelection.Visible          := B;
    actAlignAndSortSelection.Visible   := B;
    actCut.Visible                     := B;
    actDelete.Visible                  := B;
    actDequoteLines.Visible            := B;
    actDequoteSelection.Visible        := B;
    actFormat.Visible                  := B;
    actInsertCharacterFromMap.Visible  := actInsertCharacterFromMap.Visible and B;
    actPascalStringOfSelection.Visible := B;
    actPaste.Visible                   := B;
    actQuoteSelection.Visible          := B;
    actQuoteLines.Visible              := B;
    actQuoteLinesAndDelimit.Visible    := B;
    actShapeCode.Visible               := actShapeCode.Visible and B;
    actToggleComment.Visible           := B;
    actStripLastChar.Visible           := B;
    actStripFirstChar.Visible          := B;
    actUpperCaseSelection.Visible      := B;
    actLowerCaseSelection.Visible      := B;
    actSyncEdit.Visible                := B;

    actFind.Checked           := ToolViews['frmSearchForm'].Visible;
    actShowPreview.Checked    := ToolViews['frmPreview'].Visible;
    actShapeCode.Checked      := ToolViews['frmCodeShaper'].Visible;
    actAlignSelection.Checked := ToolViews['frmAlignLines'].Visible;

    actRedo.Enabled := B and V.CanRedo;
    actUndo.Enabled := B and V.CanUndo;

    B := V.SupportsFolding;
    actToggleFoldLevel.Enabled := B;
    actFoldLevel0.Enabled      := B;
    actFoldLevel1.Enabled      := B;
    actFoldLevel2.Enabled      := B;
    actFoldLevel3.Enabled      := B;
    actFoldLevel4.Enabled      := B;
    actFoldLevel5.Enabled      := B;
    actFoldLevel6.Enabled      := B;
    actFoldLevel7.Enabled      := B;
    actFoldLevel8.Enabled      := B;
    actFoldLevel9.Enabled      := B;
    actFoldLevel10.Enabled     := B;

    actToggleFoldLevel.ImageIndex    := 59 + V.FoldLevel;
    actShowControlCharacters.Checked := V.ShowSpecialChars;

    actClose.Visible       := ViewCount > 1;
    actCloseOthers.Visible := ViewCount > 1;
    FChanged := False;
  end;
  actFilterCode.Checked := ToolViews['frmCodeFilterDialog'].Visible;
end;

procedure TdmEditorManager.UpdateEncodingActions;
var
  S: string;
  A: TCustomAction;
begin
  S := '';
  if Assigned(ActiveView) then
  begin
    S := 'actEncoding' + DelChars(ActiveView.Encoding, '-');
    A := Items[S];
    if Assigned(A) then
      A.Checked := True;
  end;
end;

procedure TdmEditorManager.UpdateLineBreakStyleActions;
var
  S: string;
  A: TCustomAction;
begin
  S := '';
  if Assigned(ActiveView) then
  begin
    S := 'actLineBreakStyle' + ActiveView.LineBreakStyle;
    A := Items[S];
    if Assigned(A) then
      A.Checked := True;
  end;
end;

procedure TdmEditorManager.UpdateFileActions;
var
  B: Boolean;
begin
  B := FileExists(ActiveView.FileName);
  actCreateDesktopLink.Enabled := B;
  actCopyFileName.Enabled   := B;
  actCopyFilePath.Enabled   := B;
  actCopyFullPath.Enabled   := B;
  actReload.Enabled         := B;
  actMonitorChanges.Enabled := B;
  actMonitorChanges.Checked := B and ActiveView.MonitorChanges;
end;

procedure TdmEditorManager.UpdateSearchMatches;
begin
  if ToolViews['frmSearchForm'].Visible then
  begin
    ActiveView.BeginUpdate;
    ActiveView.SetHighlightSearch(SearchEngine.SearchText, SearchEngine.Options);
    ActiveView.EndUpdate;
  end;
end;

procedure TdmEditorManager.ClearHighlightSearch;
var
  V: IInterface;
begin
  for V in ViewList do
  begin
    (V as IEditorView).ClearHighlightSearch;
  end;
end;

///////////////////////////////////TEMP////////////////////////////////////////

function TdmEditorManager.ActiveToolView: IEditorToolView;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FToolViewList.Count -1 do
  begin
    if (FToolViewList[I] as IEditorToolView).Visible and
      Supports(FToolViewList[I], IClipboardCommands)
    then
    begin
      Result := FToolViewList[I] as IEditorToolView;
      Logger.Send(Result.Name);
    end;
  end;
end;

procedure TdmEditorManager.UpdateHighLighterActions;
var
  S: string;
  A: TCustomAction;
begin
  S := '';
  if Assigned(ActiveView) and Assigned(ActiveView.HighlighterItem) then
  begin
    S := 'actHighlighter' + ActiveView.HighlighterItem.Name;
    A := Items[S];
    if Assigned(A) then
      A.Checked := True;
  end;
end;
{$endregion}

{$region 'Find' /fold}
procedure TdmEditorManager.FindNextWordOccurrence(DirectionForward: Boolean);
var
  StartX, EndX: Integer;
  Flags: TSynSearchOptions;
  LogCaret: TPoint;
begin
  StartX := 0;
  EndX   := ActiveView.Editor.MaxLeftChar;
  LogCaret := ActiveView.LogicalCaretXY;
  ActiveView.Editor.GetWordBoundsAtRowCol(LogCaret, StartX, EndX);
  if EndX <= StartX then
    Exit;
  Flags := [ssoWholeWord];
  if DirectionForward then
  begin
    LogCaret.X := EndX;
  end
  else
  begin
    LogCaret.X := StartX;
    Include(Flags, ssoBackwards);
  end;
  ActiveView.LogicalCaretXY := LogCaret;
  ActiveView.Editor.SearchReplace(ActiveView.Editor.GetWordAtRowCol(LogCaret), '', Flags);
end;

procedure TdmEditorManager.FindPrevious;
begin
  SearchEngine.FindPrevious;
end;

procedure TdmEditorManager.FindNext;
begin
  SearchEngine.FindNext;
end;
{$endregion}

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************
{$endregion}

initialization
  dmEditorManager := TdmEditorManager.Create(Application);

end.
