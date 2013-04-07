{
  Copyright (C) 2013 Tim Sinaeve tim.sinaeve@gmail.com

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

   - customizable shortcuts for actions.

   - show a hintwindow of the selection with the proposed operation!
   - list of all supported actions, category, shortcut, description (treeview)

   - make ts_Editor_Actions, ts_Editor_Menus, ts_Editor_Images ?
   - make ts_Editor_Events and ts_Editor_Views?

   - xml/HTML treeview?
   - binary editor view
   - viewports
   - settings dialog (store settings in xml)

   - goto line
   - goto position

  BUGS:
   - createdesktoplink has problems with spaces in paths
   - fix codeshaper undo

   - copy to clipboard
      - as HTML object has a incomplete HTML closing tag
      - as RTF object does the same as copy as RTF text

  Adding actions:
    - if checkable then AutoCheck should be set to True
    - the OnExecute should be handled in this unit
    - if the action alters application settings, the Settings instance should
      be adjusted in the action handler. When Settings change, a (multicast)
      notification will be dispatched which can be handled by any module that
      needs to be notified (observer pattern).

  Use a IEditorManager instance to create IEditorView instances. In most cases
  you only need a single manager so you can use the EditorManager singleton
  declared in this unit.
    - EditorManager.OpenFile(<<filename>>)
    - EditorManager.NewFile(<<filename>>)

    - When a new view is added to the list of editorviews, the OnAddEditorView
      event is dispatched. The application can handle this event for example to
      dock the view.
    - A view can also be added directly to the list using:
      EditorManager.Views.Add(..)



      EditorManager.Actions   : IEditorActions
                   .Commands  : IEditorCommands
                   .Events    : IEditorEvents
                   .Menus     : IEditorMenus
                   .Settings  : IEditorSettings
                   .ToolViews : IEditorToolViews
                   .Views     : IEditorViews
}
{$endregion}

//*****************************************************************************

interface

uses
  Classes, SysUtils, Controls, ActnList, Menus, Dialogs, Forms,

  // logging
  sharedloggerlcl,

  LCLType,

  SynEdit, SynEditHighlighter, SynExportHTML, SynMacroRecorder,

  ts_Editor_Interfaces, ts_Editor_Resources, ts_Editor_Highlighters,
  ts_Editor_View,

  ts_Components_UniHighlighter, ts_Components_ExportRTF,
  ts_Components_UniqueInstance;

type
  TdmEditorManager = class(TDataModule, IEditorManager,
                                        IEditorActions,
                                        IEditorView,   // active view
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
    actNewSharedView: TAction;
    actSingleInstance: TAction;
    actToggleMaximized: TAction;
    actStayOnTop: TAction;
    actSelectionInfo              : TAction;
    actXMLTree                    : TAction;
    actToggleBlockCommentSelection: TAction;
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
    actSearch                     : TAction;
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
    actPageSetup                  : TAction;
    actPascalStringOfSelection    : TAction;
    actShowPreview                : TAction;
    actPrint                      : TAction;
    actPrintPreview               : TAction;
    actQuoteLines                 : TAction;
    actQuoteLinesAndDelimit       : TAction;
    actReload                     : TAction;
    actSearchReplace              : TAction;
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
    MenuItem64                    : TMenuItem;
    MenuItem65                    : TMenuItem;
    MenuItem67                    : TMenuItem;
    MenuItem68                    : TMenuItem;
    MenuItem69                    : TMenuItem;
    MenuItem7                     : TMenuItem;
    MenuItem70                    : TMenuItem;
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
    ppmSelectionMode              : TPopupMenu;
    SynExporterHTML               : TSynExporterHTML;
    SynMacroRecorder              : TSynMacroRecorder;
    {$endregion}

    {$region 'action handlers' /fold}
    procedure actAboutExecute(Sender: TObject);
    procedure actAlignAndSortSelectionExecute(Sender: TObject);
    procedure actAlignSelectionExecute(Sender: TObject);
    procedure actAutoFormatXMLExecute(Sender: TObject);
    procedure actAutoGuessHighlighterExecute(Sender: TObject);
    procedure actNewSharedViewExecute(Sender: TObject);
    procedure actSelectionInfoExecute(Sender: TObject);
    procedure actSelectionModeExecute(Sender: TObject);
    procedure actSingleInstanceExecute(Sender: TObject);
    procedure actStayOnTopExecute(Sender: TObject);
    procedure actToggleBlockCommentSelectionExecute(Sender: TObject);
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
    procedure actToggleMaximizedExecute(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure actUpperCaseSelectionExecute(Sender: TObject);
    procedure actEncodingExecute(Sender: TObject);
    procedure actLineBreakStyleExecute(Sender: TObject);
    procedure actXMLTreeExecute(Sender: TObject);
    {$endregion}

    {$region 'event handlers' /fold}
    procedure SynMacroRecorderStateChange(Sender: TObject);
    procedure UniqueInstanceOtherInstance(Sender: TObject;
      ParamCount: Integer; Parameters: array of String);
    {$endregion}

  private
    FPersistSettings   : Boolean;
    FSynExporterRTF    : TSynExporterRTF;
    FSynUni            : TSynUniSyn;
    FUniqueInstance    : TUniqueInstance;

    FOnActiveViewChange    : TNotifyEvent;
    FOnAddEditorView       : TAddEditorViewEvent;
    FOnCaretPositionChange : TCaretPositionEvent;
    FOnChange              : TNotifyEvent;
    FOnMacroStateChange    : TMacroStateChangeEvent;
    FOnNewFile             : TNewFileEvent;
    FOnOpenFile            : TFileEvent;
    FOnOpenOtherInstance   : TOpenOtherInstanceEvent;
    FOnSaveFile            : TFileEvent;
    FOnStatusChange        : TStatusChangeEvent;
    FOnStatusMessage       : TStatusMessageEvent;
    FSearchEngine          : IEditorSearchEngine;

    FChanged      : Boolean;
    FSettings     : IEditorSettings;
    FActiveView   : IEditorView;
    FViewList     : TEditorViewList;
    FToolViewList : TEditorToolViewList;
    FFormsCreated : Boolean;

    FCurrentViewIndex: Integer;

    {$region 'property access methods' /fold}
    function GetActionList: TActionList;
    function GetActions: IEditorActions;
    function GetCommands: IEditorCommands;
    function GetCurrent: IEditorView;
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
    function GetOnAddEditorView: TAddEditorViewEvent;
    function GetOnCaretPositionChange: TCaretPositionEvent;
    function GetOnChange: TNotifyEvent;
    function GetOnMacroStateChange: TMacroStateChangeEvent;
    function GetOnNewFile: TNewFileEvent;
    function GetOnOpenFile: TFileEvent;
    function GetOnOpenOtherInstance: TOpenOtherInstanceEvent;
    function GetOnSaveFile: TFileEvent;
    function GetOnStatusChange: TStatusChangeEvent;
    function GetPersistSettings: Boolean;
    function GetSearchEngine: IEditorSearchEngine;
    function GetSelectionModePopupMenu: TPopupMenu;
    function GetSettings: IEditorSettings;
    function GetActiveView: IEditorView;
    function GetHighlighters: THighlighters;
    function IEditorToolViews.GetCount = GetToolViewCount;
    function GetToolViewCount: Integer;
    function GetToolViewList: TEditorToolViewList;
    function GetToolViews: IEditorToolViews;
    function GetView(AIndex: Integer): IEditorView;
    function GetViewByFileName(AFileName: string): IEditorView;
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
    procedure SetOnActiveViewChange(AValue: TNotifyEvent);
    procedure SetOnAddEditorView(AValue: TAddEditorViewEvent);
    procedure SetOnCaretPositionChange(const AValue: TCaretPositionEvent);
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetOnMacroStateChange(const AValue: TMacroStateChangeEvent);
    procedure SetOnNewFile(const AValue: TNewFileEvent);
    procedure SetOnOpenFile(const AValue: TFileEvent);
    procedure SetOnOpenOtherInstance(AValue: TOpenOtherInstanceEvent);
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
    procedure EditorSettingsChanged(ASender: TObject);

    procedure InitializeFoldHighlighters;
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

    { IEditorViews }
    function IEditorViews.Add = AddView;
    function IEditorViews.Delete = DeleteView;
    function IEditorViews.Clear = ClearViews;
    function IEditorViews.GetEnumerator = GetViewsEnumerator;

    function AddView(
      const AName        : string = '';
      const AFileName    : string = '';
      const AHighlighter : string = ''
    ): IEditorView;
    function AddSharedView(
            AEditorView : IEditorView;
      const AName       : string = ''
    ): IEditorView;

    function DeleteView(AIndex: Integer): Boolean; overload;
    function DeleteView(AView: IEditorView): Boolean; overload;
    function DeleteView(const AName: string): Boolean; overload;
    procedure ClearViews(AExceptActive: Boolean = False);
    function GetViewsEnumerator: TEditorViewListEnumerator;

    { IEditorToolViews }
    procedure IEditorToolViews.Add = AddToolView;
    function IEditorToolViews.Delete = DeleteToolView;
    procedure AddToolView(AToolView: IEditorToolView);
    function DeleteToolView(AIndex: Integer): Boolean; overload;
    function DeleteToolView(AView: IEditorToolView): Boolean; overload;
    function DeleteToolView(const AName: string): Boolean; overload;

    { IEditorCommands } { TODO -oTS : Move to dedicated class or TEditorView }
    function SaveFile(
      const AFileName   : string = '';
            AShowDialog : Boolean = False
    ): Boolean;
    procedure LoadFile;
    procedure OpenFileAtCursor;
    procedure ToggleHighlighter;
    procedure CreateDesktopLink;
    procedure InsertCharacter(const C: TUTF8Char);
    procedure AssignHighlighter(const AName: string);
    procedure CopyToClipboard;

    procedure FindNext;
    procedure FindNextWordOccurrence(DirectionForward: Boolean);
    procedure FindPrevious;

    function ActivateView(const AName: string): Boolean;

    // TComponent overrides
    procedure Notification(
      AComponent : TComponent;
      Operation  : TOperation
    ); override;

    // event dispatch methods
    procedure DoActiveViewChange; virtual;
    procedure DoAddEditorView(AEditorView: IEditorView); virtual;
    procedure DoCaretPositionChange; virtual;
    procedure DoMacroStateChange(AState : TSynMacroState); virtual;
    procedure DoOpenOtherInstance(const AParams: array of string); virtual;
    procedure DoStatusMessage(AText: string); virtual;
    procedure DoStatusChange(AChanges: TSynStatusChanges); virtual;
    procedure DoChange; virtual;
    procedure DoModified; virtual;
    procedure DoSaveFile;
    procedure DoOpenFile(const AFileName: string);
    procedure DoNewFile(
      const AFileName : string = '';
      const AText     : string = ''
    );

    procedure UpdateActions;
    procedure UpdateEncodingActions;
    procedure UpdateLineBreakStyleActions;
    procedure UpdateSelectionModeActions;
    procedure UpdateHighLighterActions;
    procedure UpdateFileActions;
    procedure UpdateSearchMatches;
    procedure UpdateCodeFilter;

    procedure ClearHighlightSearch;

    { IEditorManager }
    function OpenFile(const AFileName: string): IEditorView;
    function NewFile(
      const AFileName  : string;
      const AText      : string = ''
    ): IEditorView;

    // TS temp
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

    property SelectionModePopupMenu: TPopupMenu
      read GetSelectionModePopupMenu;

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

    property ViewByFileName[AFileName: string]: IEditorView
      read GetViewByFileName;

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

    property OnOpenOtherInstance: TOpenOtherInstanceEvent
      read GetOnOpenOtherInstance write SetOnOpenOtherInstance;

    property OnAddEditorView: TAddEditorViewEvent
      read GetOnAddEditorView write SetOnAddEditorView;

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
  FileUtil, Clipbrd, StrUtils, Math, ShlObj, Windows, Graphics, TypInfo,

  LConvEncoding,

  SynEditTypes, SynPluginSyncroEdit, SynEditHighlighterFoldBase,

  SynHighlighterPas, SynHighlighterSQL, SynHighlighterLFM, SynHighlighterXML,
  SynHighlighterBat, SynHighlighterHTML, SynHighlighterCpp, SynHighlighterJava,
  SynHighlighterPerl, SynHighlighterPython, SynHighlighterPo,
  SynHighlighterPHP, SynHighlighterCss, SynHighlighterJScript,
  synhighlighterunixshellscript,

  ts_Core_Utils, ts_Core_ComponentInspector,

  //ts_Components_UniHighlighter,

  ts_Editor_Settings, ts_Editor_HighlighterAttributes,
  ts_Editor_ViewListForm, ts_Editor_CodeShaperForm , ts_Editor_PreviewForm,
  ts_Editor_Testform, ts_Editor_SearchForm, ts_Editor_ShortcutsDialog,
  ts_Editor_ActionListViewForm, ts_Editor_SettingsDialog, ts_Editor_Utils,
  ts_Editor_CodeFilterDialog, ts_Editor_CharacterMapDialog,
  ts_Editor_XmlTreeForm, ts_Editor_AlignLinesForm, ts_Editor_AboutDialog,
  ts_Editor_CodeFormatters, ts_Editor_CodeFormatters_SQL,
  ts_Editor_SearchEngine, ts_Editor_SelectionInfoForm;

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
  FPersistSettings  := False;
  FSettings         := TEditorSettings.Create(Self);
  FSettings.AddEditorSettingsChangedHandler(EditorSettingsChanged);
  FViewList         := TEditorViewList.Create;
  FToolViewList     := TEditorToolViewList.Create;
  FSearchEngine     := TSearchEngine.Create(Self);
  FSynExporterRTF   := TSynExporterRTF.Create(Self);
  FSynUni           := TSynUniSyn.Create(Self);
  FUniqueInstance   := TUniqueInstance.Create(Self);
  FUniqueInstance.Identifier := 'Me';
  FUniqueInstance.OnOtherInstance := UniqueInstanceOtherInstance;
  RegisterHighlighters;
  InitializeFoldHighlighters;

  InitializeActions;
  InitializePopupMenus;
end;

procedure TdmEditorManager.BeforeDestruction;
begin
  Logger.EnterMethod(Self, 'BeforeDestruction');
  FActiveView := nil; // !!!!!!!!! after a long search this was a long lasting bug
  if PersistSettings then
    FSettings.Save;
  FSearchEngine := nil;
  FSettings := nil;
  FreeAndNil(FToolViewList);
  FreeAndNil(FViewList);
  inherited BeforeDestruction;
  Logger.ExitMethod(Self, 'BeforeDestruction');
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

function TdmEditorManager.GetActions: IEditorActions;
begin
  Result := Self as IEditorActions;
end;

function TdmEditorManager.GetCommands: IEditorCommands;
begin
  Result := Self as IEditorCommands;
end;

function TdmEditorManager.GetCurrent: IEditorView;
begin
  Result := FViewList[FCurrentViewIndex] as IEditorView;
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

procedure TdmEditorManager.SetOnActiveViewChange(AValue: TNotifyEvent);
begin
  FOnActiveViewChange := AValue;
end;

function TdmEditorManager.GetOnAddEditorView: TAddEditorViewEvent;
begin
  Result := FOnAddEditorView;
end;

procedure TdmEditorManager.SetOnAddEditorView(AValue: TAddEditorViewEvent);
begin
  FOnAddEditorView := AValue;
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

function TdmEditorManager.GetOnOpenOtherInstance: TOpenOtherInstanceEvent;
begin
  Result := FOnOpenOtherInstance;
end;

procedure TdmEditorManager.SetOnOpenOtherInstance(AValue: TOpenOtherInstanceEvent);
begin
  FOnOpenOtherInstance := AValue;
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

procedure TdmEditorManager.SetPersistSettings(const AValue: Boolean);
begin
  if AValue <> PersistSettings then
  begin
    if AValue then
    begin
      Settings.Load;
      FUniqueInstance.Enabled := Settings.SingleInstance;
      // TSI voorlopig
      RegisterHighlighters;
      InitializeFoldHighlighters;
    end;
    FPersistSettings := AValue;
  end;
end;

function TdmEditorManager.GetSearchEngine: IEditorSearchEngine;
begin
  Result := FSearchEngine;
end;

function TdmEditorManager.GetSelectionModePopupMenu: TPopupMenu;
begin
  Result := ppmSelectionMode;
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
  if Assigned(AValue) and (AValue <> FActiveView) then
  begin
    FActiveView := AValue;
    DoActiveViewChange;
    ActiveViewChanged;
  end;
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

function TdmEditorManager.GetViewByFileName(AFileName: string): IEditorView;
var
  I: Integer;
  B: Boolean;
begin
  I := 0;
  B := False;
  while (I < FViewList.Count) and not B do
  begin
    B := SameFileName(Views[I].FileName, AFileName);
    if not B then
      Inc(I);
  end;
  if B then
    Result := FViewList[I] as IEditorView
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
  if not Assigned(Result) then
    Exception.CreateFmt('Toolview (%s) does not exist!', [AName]);
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

procedure TdmEditorManager.DoAddEditorView(AEditorView: IEditorView);
begin
  if Assigned(FOnAddEditorView) then
    FOnAddEditorView(Self, AEditorView);
end;

procedure TdmEditorManager.DoCaretPositionChange;
begin
  if Assigned(FOnCaretPositionChange) then
    FOnCaretPositionChange(Self, ActiveView.CaretX, ActiveView.CaretY);

  UpdateSelectionModeActions;

  { TODO -oTS : Needs to be refactored }
  if ToolViews['frmPreview'].Visible then
    ToolViews['frmPreview'].UpdateView;
  if ToolViews['frmTest'].Visible then
    ToolViews['frmTest'].UpdateView;
  if ToolViews['frmAlignLines'].Visible then
    ToolViews['frmAlignLines'].UpdateView;
  if ToolViews['frmActionListView'].Visible then
    ToolViews['frmActionListView'].UpdateView;
  if ToolViews['frmViewList'].Visible then
    ToolViews['frmViewList'].UpdateView;
end;

procedure TdmEditorManager.DoMacroStateChange(AState: TSynMacroState);
begin
  if Assigned(FOnMacroStateChange) then
    FOnMacroStateChange(Self, AState);
end;

procedure TdmEditorManager.DoOpenOtherInstance(const AParams: array of string);
begin
  if Assigned(FOnOpenOtherInstance) then
    FOnOpenOtherInstance(Self, AParams);
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

procedure TdmEditorManager.DoOpenFile(const AFileName: string);
var
  S : string;
begin
  S  := AFileName;
  if Assigned(FOnOpenFile) then
    FOnOpenFile(Self, S);
end;

procedure TdmEditorManager.DoNewFile(const AFileName: string; const AText: string);
var
  S : string;
begin
  S  := AFileName;
  if Assigned(FOnNewFile) then
    FOnNewFile(Self, S, AText);
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
  { TODO -oTS : implement a new toolform for this }
  ShowMessage('TODO');
end;

procedure TdmEditorManager.actToggleCommentExecute(Sender: TObject);
begin
  ActiveView.UpdateCommentSelection(False, True);
end;

procedure TdmEditorManager.actToggleHighlighterExecute(Sender: TObject);
begin
  ToggleHighlighter;
end;

procedure TdmEditorManager.actToggleMaximizedExecute(Sender: TObject);
var
  A : TAction;
begin
  A := Sender as TAction;
  if A.Checked then
    Settings.FormSettings.WindowState := wsMaximized
  else
    Settings.FormSettings.WindowState := wsNormal;
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
  ActiveView.PascalStringFromSelection;
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
  ActiveView.QuoteLinesInSelection(True);
end;

procedure TdmEditorManager.actQuoteLinesExecute(Sender: TObject);
begin
  ActiveView.QuoteLinesInSelection;
end;

procedure TdmEditorManager.actSaveExecute(Sender: TObject);
begin
  SaveFile(ActiveView.FileName);
end;

procedure TdmEditorManager.actSaveAsExecute(Sender: TObject);
begin
  SaveFile(ActiveView.FileName, True);
end;

procedure TdmEditorManager.actSearchExecute(Sender: TObject);
begin
  // handled by owner
  if not actSearch.Checked then
    actSearchReplace.Checked := False;
end;

procedure TdmEditorManager.actSearchReplaceExecute(Sender: TObject);
begin
  // handled by owner
  if not actSearch.Checked then
    actSearch.Checked := True;
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

procedure TdmEditorManager.actCopyRTFToClipboardExecute(Sender: TObject);
begin
  ExportLines(HL_RTF);
end;

procedure TdmEditorManager.actCreateDesktopLinkExecute(Sender: TObject);
begin
  CreateDesktopLink;
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

procedure TdmEditorManager.actCutExecute(Sender: TObject);
begin
  if ActiveView.Focused then
    ActiveView.Cut
  else if Assigned(ActiveToolView) then
    (ActiveToolView as IClipboardCommands).Cut;
end;

procedure TdmEditorManager.actFilterCodeExecute(Sender: TObject);
var
  ETV: IEditorToolView;
begin
  ETV := ToolViews['frmCodeFilterDialog'];
  if Assigned(ETV) then
  begin
    if not ETV.Visible then
    begin
      ETV.Visible := True;
      ETV.UpdateView;
    end;
    ETV.SetFocus;
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
  InspectComponents([
    Settings as TComponent,
    ActiveView.Editor,
    ActiveView.Editor.Highlighter
  ]);
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
  NewFile(SNewEditorViewFileName, S);
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
  ActiveView.StripCharsFromSelection(True, False);
end;

procedure TdmEditorManager.actStripMarkupExecute(Sender: TObject);
begin
  ActiveView.StripMarkupFromSelection;
end;

procedure TdmEditorManager.actStripLastCharExecute(Sender: TObject);
begin
  ActiveView.StripCharsFromSelection(False, True);
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
  { TODO -oTS : Will be implemented as a toolview. }
end;

procedure TdmEditorManager.actAlignSelectionExecute(Sender: TObject);
begin
// TOOLVIEW
end;

procedure TdmEditorManager.actAutoFormatXMLExecute(Sender: TObject);
begin
  (Sender as TAction).Checked := not (Sender as TAction).Checked;
  Settings.AutoFormatXML := (Sender as TAction).Checked;
end;

procedure TdmEditorManager.actShowPreviewExecute(Sender: TObject);
begin
  Settings.PreviewVisible := (Sender as TAction).Checked;
end;

procedure TdmEditorManager.actShowTestExecute(Sender: TObject);
begin
  //ToolViews['frmTest'].Visible := not ToolViews['frmTest'].Visible;
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
  ExecuteSettingsDialog(Self);
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
  ActiveView.DequoteLinesInSelection;
end;

procedure TdmEditorManager.actAutoGuessHighlighterExecute(Sender: TObject);
begin
  AssignHighlighter(GuessHighlighterType(ActiveView.Text));
end;

procedure TdmEditorManager.actNewSharedViewExecute(Sender: TObject);
begin
  AddSharedView(ActiveView);
end;

procedure TdmEditorManager.actSelectionInfoExecute(Sender: TObject);
begin
  //
end;

procedure TdmEditorManager.actSelectionModeExecute(Sender: TObject);
begin
  ActiveView.SelectionMode := TSynSelectionMode((Sender as TCustomAction).Tag);
end;

procedure TdmEditorManager.actSingleInstanceExecute(Sender: TObject);
begin
  Settings.SingleInstance := not Settings.SingleInstance;
end;

procedure TdmEditorManager.actStayOnTopExecute(Sender: TObject);
var
  A : TAction;
begin
  A := Sender as TAction;
  if A.Checked then
    Settings.FormSettings.FormStyle := fsSystemStayOnTop
  else
    Settings.FormSettings.FormStyle := fsNormal;
end;

procedure TdmEditorManager.actToggleBlockCommentSelectionExecute(Sender: TObject);
begin
  ActiveView.ToggleBlockCommentSelection;
end;

procedure TdmEditorManager.actClearExecute(Sender: TObject);
begin
  ActiveView.Clear;
end;

procedure TdmEditorManager.actAboutExecute(Sender: TObject);
begin
  ShowAboutDialog;
end;

procedure TdmEditorManager.actAlignAndSortSelectionExecute(Sender: TObject);
begin
  { TODO -oTS : Implement as a shortcut which takes default settings from the
  dedicated toolview. }
end;

procedure TdmEditorManager.actCloseExecute(Sender: TObject);
begin
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
  ActiveView.Base64FromSelection;
end;

procedure TdmEditorManager.actExitExecute(Sender: TObject);
begin
  ClearViews;
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
  ActiveView.Base64FromSelection(True);
end;

procedure TdmEditorManager.actShowControlCharactersExecute(Sender: TObject);
begin
  Settings.ShowControlCharacters := (Sender as TAction).Checked;
end;

procedure TdmEditorManager.actEncodingExecute(Sender: TObject);
begin
  ActiveView.Encoding := (Sender as TAction).Caption;
end;

procedure TdmEditorManager.actLineBreakStyleExecute(Sender: TObject);
begin
  ActiveView.LineBreakStyle := (Sender as TAction).Caption;
end;

procedure TdmEditorManager.actXMLTreeExecute(Sender: TObject);
begin
  //ToolViews['frmXmlTree'].Visible := True;
  //ToolViews['frmXmlTree'].UpdateView;
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

procedure TdmEditorManager.UniqueInstanceOtherInstance(Sender: TObject;
   ParamCount: Integer; Parameters: array of String);
begin
  DoOpenOtherInstance(Parameters);
end;

procedure TdmEditorManager.CodeFilterFilteredLineChange(Sender: TObject;
  AIndex: Integer; const ALine: string; const AFilter: string);
begin
  ActiveView.SearchAndSelectLine(AIndex, ALine);
end;

procedure TdmEditorManager.EditorSettingsChanged(ASender: TObject);
begin
  if actShowPreview.Checked <> Settings.PreviewVisible then
  begin
    actShowPreview.Execute;
  end;
  FUniqueInstance.Enabled := Settings.SingleInstance;
  ApplyHighlighterAttributes;
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

procedure TdmEditorManager.InitializeFoldHighlighters;
var
  I  : Integer;
  N  : Integer;
  FH : TSynCustomFoldHighlighter;
begin
  FH := TSynCustomFoldHighlighter(Highlighters.ItemsByName['PAS'].SynHighlighter);
  FH.AddSpecialAttribute(''); // not sure why this is needed...
  for I := Low(EditorOptionsDividerInfoPas) to High(EditorOptionsDividerInfoPas) do
  begin
    FH.DividerDrawConfig[I].MaxDrawDepth :=
      EditorOptionsDividerInfoPas[I].MaxLevel;
  end;
  for I := Low(EditorOptionsFoldInfoPas) to High(EditorOptionsFoldInfoPas) do
  begin
    N := EditorOptionsFoldInfoPas[I].Index;
    if N >= 0 then
      FH.FoldConfig[N].Enabled := EditorOptionsFoldInfoPas[I].Enabled;
  end;
  FH := TSynCustomFoldHighlighter(Highlighters.ItemsByName['XML'].SynHighlighter);
  for I := Low(EditorOptionsFoldInfoXML) to High(EditorOptionsFoldInfoXML) do
  begin
    N := EditorOptionsFoldInfoXML[I].Index;
    if N >= 0 then
      FH.FoldConfig[N].Enabled := EditorOptionsFoldInfoXML[I].Enabled;
  end;
  FH := TSynCustomFoldHighlighter(Highlighters.ItemsByName['LFM'].SynHighlighter);
  for I := Low(EditorOptionsFoldInfoLFM) to High(EditorOptionsFoldInfoLFM) do
  begin
    N := EditorOptionsFoldInfoLFM[I].Index;
    if N >= 0 then
      FH.FoldConfig[N].Enabled := EditorOptionsFoldInfoLFM[I].Enabled;
  end;
  FH := TSynCustomFoldHighlighter(Highlighters.ItemsByName['HTML'].SynHighlighter);
  for I := Low(EditorOptionsFoldInfoHTML) to High(EditorOptionsFoldInfoHTML) do
  begin
    N := EditorOptionsFoldInfoHTML[I].Index;
    if N >= 0 then
      FH.FoldConfig[N].Enabled := EditorOptionsFoldInfoHTML[I].Enabled;
  end;
end;

procedure TdmEditorManager.InitializePopupMenus;
var
  SL : TStringList;
  S  : string;
  MI : TMenuItem;
  HI : THighlighterItem;
  A  : TCustomAction;
  SM : TSynSelectionMode;
begin
  HighlighterPopupMenu.Items.Caption := 'Highlighters';
  HighlighterPopupMenu.Items.Action := actToggleHighlighter;
  FoldPopupMenu.Items.Caption := 'Folding';
  FoldPopupMenu.Items.Action := actToggleFoldLevel;
  EditorPopupMenu.Items.Insert(15, HighlighterPopupMenu.Items);
  EditorPopupMenu.Items.Insert(16, FoldPopupMenu.Items);

  SL := TStringList.Create;
  try
    EncodingPopupMenu.Items.Clear;
    GetSupportedEncodings(SL);
    for S in SL do
    begin
      MI := TMenuItem.Create(EncodingPopupMenu);
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
      EncodingPopupMenu.Items.Add(MI);
    end;
  finally
    FreeAndNil(SL);
  end;

  LineBreakStylePopupMenu.Items.Clear;
  for S in ALineBreakStyles do
  begin
    MI := TMenuItem.Create(LineBreakStylePopupMenu);
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
    LineBreakStylePopupMenu.Items.Add(MI);
  end;

  HighlighterPopupMenu.Items.Clear;
  for HI in Highlighters do
  begin
    MI := TMenuItem.Create(HighlighterPopupMenu);
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
    HighlighterPopupMenu.Items.Add(MI);
  end;

  SelectionModePopupMenu.Items.Clear;
  for SM := Low(TSynSelectionMode) to High(TSynSelectionMode) do
  begin
    MI := TMenuItem.Create(SelectionModePopupMenu);
    S := GetEnumName(TypeInfo(TSynSelectionMode), Ord(SM));
    S := System.Copy(S, 3, Length(S));
    S := 'actSelectionMode' + S;
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
    SelectionModePopupMenu.Items.Add(MI);
  end;
end;

procedure TdmEditorManager.InitializeActions;
var
  A  : TAction;
  SL : TStringList;
  S  : string;
  HI : THighlighterItem;
  SM : TSynSelectionMode;
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
    for HI in Highlighters do
    begin
      A.Tag := HI.Index;
      A := TAction.Create(ActionList);
      A.ActionList := ActionList;
      A.Caption := HI.Name;
      A.Name := 'actHighlighter' + HI.Name;
      A.AutoCheck := True;
      A.GroupIndex := 5;
      A.Category := 'Highlighter';
      A.OnExecute := actHighlighterExecute;
    end;
    for SM := Low(TSynSelectionMode) to High(TSynSelectionMode) do
    begin
      A := TAction.Create(ActionList);
      A.ActionList := ActionList;
      A.Tag := Ord(SM);
      S := GetEnumName(TypeInfo(TSynSelectionMode), A.Tag);
      S := System.Copy(S, 3, Length(S));
      A.Caption := S;
      A.Name := 'actSelectionMode' + S;
      A.AutoCheck := True;
      A.GroupIndex := 6;
      A.Category := 'SelectionMode';
      A.OnExecute := actSelectionModeExecute;
    end;
  finally
    FreeAndNil(SL);
  end;
end;

procedure TdmEditorManager.ApplyHighlighterAttributes;
var
  I   : Integer;
  HL  : THighlighterItem;
  HAI : THighlighterAttributesItem;
  A   : TSynHighlighterAttributes;
begin
  for HL in Settings.Highlighters do
  begin
    for HAI in Settings.HighlighterAttributes do
    begin
      if Assigned(HL.SynHighlighter) then
      begin
        for I := 0 to HL.SynHighlighter.AttrCount - 1 do
        begin
          A := HL.SynHighlighter.Attribute[I];
          if A.Name = HAI.Name then
            A.Assign(HAI.Attributes);
        end;
      end;
    end;
  end;
end;

{$endregion}

{$region 'Registration' /fold}

procedure TdmEditorManager.RegisterHighlighters;
var
  S: string;
  F: string;

  procedure Reg(ASynHighlighterClass: TSynHighlighterClass;
    ASynHighlighter: TSynCustomHighlighter; const AName: string;
    const AFileExtensions: string = ''; const ADescription: string = '';
    const ALineCommentTag: string = ''; const ABlockCommentStartTag: string = '';
    const ABlockCommentEndTag: string = ''; ACodeFormatter: ICodeFormatter = nil;
    const ALayoutFileName: string = '');
  begin
    Highlighters.RegisterHighlighter(
      ASynHighlighterClass,
      ASynHighlighter,
      AName,
      AFileExtensions,
      ALineCommentTag,
      ABlockCommentStartTag,
      ABlockCommentEndTag,
      ACodeFormatter,
      ADescription,
      ALayoutFileName
    );
    Logger.Send('Registered:', AName);
  end;

begin
  Highlighters.Clear;
  Reg(nil, nil, 'None');
  Reg(nil, nil, HL_TXT, FILE_EXTENSIONS_TXT, STXTDescription);
  Reg(TSynPasSyn, nil, HL_PAS, FILE_EXTENSIONS_PAS, SPASDescription, '//', '{', '}', TPascalFormatter.Create);
  Reg(TSynSQLSyn, nil, HL_SQL, FILE_EXTENSIONS_SQL, SSQLDescription, '--', '/*', '*/', TSQLFormatter.Create);
  Reg(TSynXMLSyn, nil, HL_XML, FILE_EXTENSIONS_XML, SXMLDescription, '', '<!--', '-->', TXMLFormatter.Create);
  Reg(TSynLFMSyn, nil, HL_LFM, FILE_EXTENSIONS_LFM, SLFMDescription);
  Reg(TSynBatSyn, nil, HL_BAT, FILE_EXTENSIONS_BAT, SBATDescription, '::');
  Reg(TSynUniSyn, nil, HL_PO, FILE_EXTENSIONS_PO, SPODescription, '#');
  Reg(TSynCppSyn, nil, HL_CPP, FILE_EXTENSIONS_CPP, SCPPDescription, '//', '/*', '*/', TCPPFormatter.Create);
  Reg(TSynJavaSyn, nil, HL_JAVA, FILE_EXTENSIONS_JAVA, SJavaDescription, '//', '/*', '*/', TJavaFormatter.Create);
  Reg(TSynPerlSyn, nil, HL_PERL, FILE_EXTENSIONS_PERL, SPERLDescription, '#', '/*', '*/');
  Reg(TSynPythonSyn, nil, HL_PY, FILE_EXTENSIONS_PY, SPYDescription, '#', '/*', '*/');
  Reg(TSynHTMLSyn, nil, HL_HTML, FILE_EXTENSIONS_HTML, SHTMLDescription, '', '<!--', '-->', THTMLFormatter.Create);
  Reg(TSynJScriptSyn, nil, HL_JS, FILE_EXTENSIONS_JS, SJSDescription);
  Reg(TSynPHPSyn, nil, HL_PHP, FILE_EXTENSIONS_PHP, SPHPDescription, '');
  Reg(TSynCssSyn, nil, HL_CSS, FILE_EXTENSIONS_CSS, SCSSDescription);
  ApplyHighlighterAttributes;

  S := ExtractFilePath(Application.ExeName);

  F := S + LAYOUT_LOG;
  if FileExistsUTF8(F) then
    Reg(TSynUniSyn, FSynUni, HL_LOG, 'txt log', SLOGDescription, '', '', '', nil, F);
  F := S + LAYOUT_INI;
  if FileExistsUTF8(F) then
    Reg(TSynUniSyn, FSynUni, HL_INI, FILE_EXTENSIONS_INI, SINIDescription, ';', '', '', nil, F);
  F := S + LAYOUT_RTF;
  if FileExistsUTF8(F) then
    Reg(TSynUniSyn, FSynUni, HL_RTF, FILE_EXTENSIONS_RTF, SRTFDescription, '', '', '', nil, F);
  F := S + LAYOUT_RES;
  if FileExistsUTF8(F) then
    Reg(TSynUniSyn, FSynUni, HL_RES, FILE_EXTENSIONS_RES, SRESDescription, ';', '', '', nil, F);
  F := S + LAYOUT_CS;
  if FileExistsUTF8(F) then
    Reg(TSynUniSyn, FSynUni, HL_CS, FILE_EXTENSIONS_CS, SCSDescription, '//', '/*', '*/', nil, F);
end;

{ TODO -oTS : Make this more elegant }

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
  AddToolView(TfrmSelectionInfo.Create(Self));
//  AddToolView(TfrmXmlTree.Create(Self));

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
  UpdateCodeFilter;
end;

function TdmEditorManager.GetViewsEnumerator: TEditorViewListEnumerator;
begin
  Result := TEditorViewListEnumerator.Create(FViewList);
end;

procedure TdmEditorManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Supports(AComponent, IEditorView) and (Operation = opRemove) then
  begin
    Logger.EnterMethod(Self, 'Notification');
    DeleteView(AComponent as IEditorView);
    Logger.Watch('ViewCount', ViewCount);
    Logger.ExitMethod(Self, 'Notification');
  end;
  inherited Notification(AComponent, Operation);
end;

{$region 'IEditorActions' /fold}
function TdmEditorManager.AddView(const AName: string; const AFileName: string;
  const AHighlighter: string): IEditorView;
var
  V : IEditorView;
begin
  Logger.EnterMethod(Self, 'AddView');
  V := TEditorView.Create(Self);
  // if no name is provided, the view will get an automatically generated one.
  { TODO -oTS : Needs to be refactored. }
  if AName <> '' then
    V.Name := AName;
  V.FileName := AFileName;
  V.AssignHighlighter(AHighlighter);
  V.Form.Caption := '';
  ViewList.Add(V);
  DoAddEditorView(V);
  Result := V;
  Logger.Watch('ViewCount', ViewCount);
  Logger.ExitMethod(Self, 'AddView');
end;

function TdmEditorManager.AddSharedView(AEditorView: IEditorView;
  const AName: string): IEditorView;
var
  V : IEditorView;
begin
  if not Assigned(ActiveView.SlaveView) then
  begin
    V := TEditorView.Create(Self);
    V.MasterView := ActiveView;
    V.AssignHighlighter(AEditorView.HighlighterItem.Name);
    V.Form.Caption := AEditorView.Form.Caption;
    ViewList.Add(V);
    DoAddEditorView(V);
    Result := V;
  end
  else
    Result := ActiveView;
end;

function TdmEditorManager.DeleteView(AIndex: Integer): Boolean;
var
  I : Integer;
  V : IEditorView;
begin
  Logger.EnterMethod(Self, 'DeleteView(AIndex)');
  if (AIndex > -1) and (AIndex < ViewCount) {and (ViewCount > 1)} then
  begin
    Logger.Watch('AIndex', AIndex);
    I := ViewList.IndexOf(ActiveView);
    Logger.Send('ViewList.IndexOf(ActiveView)', I);
    if I = AIndex then // select a new active view
    begin
      V := Views[I];
      V.Activate
    end;
    Views[AIndex].Close;
    ViewList.Delete(AIndex);
    Result := True;
  end
  else
    Result := False;
  Logger.Watch('ViewCount', ViewCount);
  Logger.ExitMethod(Self, 'DeleteView(AIndex)');
end;

{ 1. Removes the given instance from the list
  2. Closes the instance (which will free it)
  3. Set the active view to another view if we were closing the active view
}

function TdmEditorManager.DeleteView(AView: IEditorView): Boolean;
var
  I : Integer;
begin
  Logger.EnterMethod(Self, 'DeleteView(AView)');
  if Assigned(AView) and Assigned(ViewList) then
  begin
    I := ViewList.IndexOf(AView);
    if I > -1 then
    begin
      if AView = ActiveView then
      begin
        AView.Close;
        ViewList.Delete(I);
        Views[0].Activate;
        Result := True;
      end
      else
      begin
        AView.Close;
        ViewList.Delete(I);
      end;
    end
    else
      Result := False;
  end
  else
    Result := False;
  Logger.Watch('ViewCount', ViewCount);
  Logger.ExitMethod(Self, 'DeleteView(AView)');
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
  Logger.EnterMethod(Self, 'AExceptActive');
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
  Logger.ExitMethod(Self, 'AExceptActive');
end;

procedure TdmEditorManager.AddToolView(AToolView: IEditorToolView);
begin
  AToolView.Visible := False;
  FToolViewList.Add(AToolView);
end;

function TdmEditorManager.DeleteToolView(AIndex: Integer): Boolean;
begin
  if AIndex <> -1 then
  begin
    FToolViewList.Delete(AIndex);
  end
  else
    Result := False;
end;

function TdmEditorManager.DeleteToolView(AView: IEditorToolView): Boolean;
var
  I : Integer;
begin
  I := FToolViewList.IndexOf(AView);
  Result := DeleteToolView(I);
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

{ TODO -oTS : Not correct! }

procedure TdmEditorManager.LoadFile;
begin
  DoOpenFile(ActiveView.FileName);
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
      FSynExporterRTF.Highlighter := ActiveView.Editor.Highlighter;
      FSynExporterRTF.ExportAsText := not ANativeFormat;
      FSynExporterRTF.Font.Assign(ActiveView.Editor.Font);
      if ActiveView.SelAvail then
        SL.Text := ActiveView.SelText
      else
        SL.Text := ActiveView.Text;
      FSynExporterRTF.ExportAll(SL);
      if AToClipboard then
        FSynExporterRTF.CopyToClipboard
      else
      begin
        S := dlgSave.Filter;
        dlgSave.Filter := FSynExporterRTF.DefaultFilter;
        dlgSave.FileName := ExtractFileNameWithoutExt(Settings.FileName) + '.rtf';
        if dlgSave.Execute then
          FSynExporterRTF.SaveToFile(dlgSave.FileName);
        dlgSave.Filter := S;
      end;
    end
    //else if AFormat = 'WIKI' then
    //begin
    //  SynExporterWiki.Highlighter := ActiveView.Editor.Highlighter;
    //  SynExporterWiki.ExportAsText := not ANativeFormat;
    //  if ActiveView.SelAvail then
    //    SL.Text := ActiveView.SelText
    //  else
    //    SL.Text := ActiveView.Text;
    //  SynExporterWiki.ExportAll(SL);
    //  if AToClipboard then
    //    SynExporterWiki.CopyToClipboard
    //  else
    //  begin
    //    S := dlgSave.Filter;
    //    dlgSave.Filter := SynExporterWiki.DefaultFilter;
    //    dlgSave.FileName := ExtractFileNameWithoutExt(Settings.FileName) + '.txt';
    //    if dlgSave.Execute then
    //      SynExporterWiki.SaveToFile(dlgSave.FileName);
    //    dlgSave.Filter := S;
    //  end;
    //end;
  finally
    SL.Free;
  end;
end;

{ Saves the content of the active editorview to the given filename. If the
  given filename does not exist or is empty, the user is prompted to enter a
  name with the save file dialog. }

function TdmEditorManager.SaveFile(const AFileName: string;
AShowDialog: Boolean): Boolean;
begin
  DoSaveFile;
  if AShowDialog or not FileExists(AFileName) then
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
  end
  else
  begin
    ActiveView.FileName := AFileName;
    ActiveView.SaveToFile(AFileName);
    Result := True;
  end
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
  PIDL := nil;
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
    actDequoteSelection.Enabled            := B;
    actLowerCaseSelection.Enabled          := B;
    actToggleBlockCommentSelection.Enabled := B;
    actPascalStringOfSelection.Enabled     := B;
    actStripMarkup.Enabled                 := B;
    actQuoteSelection.Enabled              := B;
    actQuoteLinesAndDelimit.Enabled        := B;
    actSortSelection.Enabled               := B;
    actUpperCaseSelection.Enabled          := B;
    actStripFirstChar.Enabled              := B;
    actStripLastChar.Enabled               := B;
    actQuoteLines.Enabled                  := B;
    actDequoteLines.Enabled                := B;
    actEncodeBase64.Enabled                := B;
    actDecodeBase64.Enabled                := B;
    actSyncEdit.Enabled                    := B;

    B := not Settings.ReadOnly;
    actAlignSelection.Visible          := B;
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

    actSearch.Checked         := ToolViews['frmSearchForm'].Visible;
    actShapeCode.Checked      := ToolViews['frmCodeShaper'].Visible;
    actAlignSelection.Checked := ToolViews['frmAlignLines'].Visible;
    actShowPreview.Checked    := ToolViews['frmPreview'].Visible;

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
    actShowControlCharacters.Checked := Settings.ShowControlCharacters;

    actClose.Visible       := ViewCount > 1;
    actCloseOthers.Visible := ViewCount > 1;

    actToggleMaximized.Checked :=
      Settings.FormSettings.WindowState = wsMaximized;
    actStayOnTop.Checked := Settings.FormSettings.FormStyle = fsSystemStayOnTop;
    actSingleInstance.Checked := Settings.SingleInstance;

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

procedure TdmEditorManager.UpdateSelectionModeActions;
var
  S: string;
  A: TCustomAction;
begin
  S := '';
  if Assigned(ActiveView) then
  begin
    S := GetEnumName(TypeInfo(TSynSelectionMode), Ord(ActiveView.SelectionMode));
    S := System.Copy(S, 3, Length(S));
    S := 'actSelectionMode' + S;
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
    ToolViews['frmSearchForm'].UpdateView;
  end;
end;

procedure TdmEditorManager.UpdateCodeFilter;
begin
  if ToolViews['frmCodeFilterDialog'].Visible then
  begin
    ToolViews['frmCodeFilterDialog'].UpdateView;
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

procedure TdmEditorManager.ClearHighlightSearch;
var
  V: IInterface;
begin
  for V in ViewList do
  begin
    (V as IEditorView).ClearHighlightSearch;
  end;
end;

function TdmEditorManager.OpenFile(const AFileName: string): IEditorView;
var
  V : IEditorView;
begin
  DoOpenFile(AFileName);
  { Check if the file is already opened in a view. }
  V := ViewByFileName[AFileName];
  if Assigned(V) then
    Result := V
  else
  begin
    if FileExists(AFileName) then
    begin
      V := AddView('', AFileName);
      V.LoadFromFile(AFileName);
    end;
    Result := V;
  end;
end;

function TdmEditorManager.NewFile(const AFileName: string; const AText: string): IEditorView;
var
  V : IEditorView;
begin
  DoNewFile(AFileName, AText);
  V := AddView('', AFileName);
  V.Text := AText;
  Result := V;
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
