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

{$MODE Delphi}

{$region 'documentation' /fold}
{
  Datamodule holding common actions, menu's to manage one or more IEditorView
  instances.

  CTRL-ALT-SHIFT keycombinations are used for experimental features

  TODO:
   - apply consistent casing for word under cursor/all words ? => dangerous for strings

   - fix highlighter issues
   - fix DeleteView methods
   - fix SortText

   - customizable shortcuts for actions.

   - show a hintwindow of the selection with the proposed operation!
   - list of all supported actions, category, shortcut, description (treeview)

   - make ts_Editor_Actions, ts_Editor_Menus, ts_Editor_Images ?
   - make ts_Editor_Events and ts.Editor.Views?

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

    TODO:
      - let toolviews be started in two modes:
         - modal
         - docked (or handled by owner form)

}
{$endregion}

interface

uses
  Classes, SysUtils, Controls, ActnList, Menus, Dialogs, Forms,

  // logging
  ts.Core.SharedLogger,

  LCLType,

  SynEdit, SynEditHighlighter, SynExportHTML, SynMacroRecorder,

  dwsComp, dwsVCLGUIFunctions, dwsGlobalVarsFunctions, dwsDebugger, dwsEngine,

  ts.Components.UniHighlighter, ts.Components.ExportRTF,
  ts.Components.UniqueInstance,

  ts.Editor.Types, ts.Editor.Interfaces, ts_Editor_Resources,
  ts.Editor.Highlighters, ts_Editor_View, ts.Editor.Events;

type
  { TdmEditorManager }

  TdmEditorManager = class(TDataModule, IEditorManager,
                                        IEditorActions,
                                        IEditorView,   // active view
                                        IEditorViews,
                                        IEditorEvents,
                                        IEditorCommands,
                                        IEditorMenus,
                                        IEditorSettings,
                                        IEditorSearchEngine)
    {$region 'designer controls' /fold}
    aclActions                        : TActionList;
    actAlignSelection                 : TAction;
    actAutoFormatXML                  : TAction;
    actDequoteSelection               : TAction;
    actAutoGuessHighlighter           : TAction;
    actClose                          : TAction;
    actCloseOthers                    : TAction;
    actAbout                          : TAction;
    actCopyFullPath                   : TAction;
    actCopyFileName                   : TAction;
    actCopyFilePath                   : TAction;
    actEncodeBase64                   : TAction;
    actDecodeBase64                   : TAction;
    actClear                          : TAction;
    actCreateDesktopLink              : TAction;
    actExit                           : TAction;
    actCut                            : TAction;
    actDelete                         : TAction;
    actHighlighter                    : TAction;
    actEncodingMenu                       : TAction;
    actExportMenu                         : TAction;
    actClipboardMenu                      : TAction;
    actInsertGUID                     : TAction;
    actInsertMenu                         : TAction;
    actFindAllOccurences              : TAction;
    actIndent                         : TAction;
    actFileMenu                           : TAction;
    actConvertTabsToSpacesInSelection : TAction;
    actExecuteScriptOnSelection       : TAction;
    actHighlighterMenu: TAction;
    actFoldMenu: TAction;
    actSettingsMenu: TAction;
    actShowFilterTest: TAction;
    actShowScriptEditor               : TAction;
    actShowMiniMap                    : TAction;
    actShowHexEditor                  : TAction;
    actShowHTMLViewer                 : TAction;
    actUnindent                       : TAction;
    actSelectMenu                         : TAction;
    actSearchMenu                     : TAction;
    actLineBreakStyleMenu                 : TAction;
    actSelectionModeMenu                  : TAction;
    actSelectionMenu                      : TAction;
    actNewSharedView                  : TAction;
    actSingleInstance                 : TAction;
    actToggleMaximized                : TAction;
    actStayOnTop                      : TAction;
    actSelectionInfo                  : TAction;
    actShowStructureViewer            : TAction;
    actToggleBlockCommentSelection    : TAction;
    actShowTest                       : TAction;
    actSelectAll                      : TAction;
    actUndo                           : TAction;
    actPaste                          : TAction;
    actStripMarkup                    : TAction;
    actTestForm                       : TAction;
    actSyncEdit                       : TAction;
    actShowViews                      : TAction;
    actShowActions                    : TAction;
    actMonitorChanges                 : TAction;
    actRedo                           : TAction;
    actStripFirstChar                 : TAction;
    actStripLastChar                  : TAction;
    actSmartSelect                    : TAction;
    actQuoteSelection                 : TAction;
    actShowSpecialCharacters          : TAction;
    actCopy                           : TAction;
    actCopyHTMLTextToClipboard        : TAction;
    actCopyRTFTextToClipboard         : TAction;
    actCopyRTFToClipboard             : TAction;
    actCopytHTMLToClipboard           : TAction;
    actCopyToClipboard                : TAction;
    actCopyWikiTextToClipboard        : TAction;
    actCopyWikiToClipboard            : TAction;
    actDecFontSize                    : TAction;
    actDequoteLines                   : TAction;
    actExportToHTML                   : TAction;
    actExportToRTF                    : TAction;
    actExportToWiki                   : TAction;
    actFilterCode                     : TAction;
    actSearch                         : TAction;
    actFindNext                       : TAction;
    actFindNextWord                   : TAction;
    actFindPrevious                   : TAction;
    actFindPrevWord                   : TAction;
    actFoldLevel0                     : TAction;
    actFoldLevel1                     : TAction;
    actFoldLevel10                    : TAction;
    actFoldLevel2                     : TAction;
    actFoldLevel3                     : TAction;
    actFoldLevel4                     : TAction;
    actFoldLevel5                     : TAction;
    actFoldLevel6                     : TAction;
    actFoldLevel7                     : TAction;
    actFoldLevel8                     : TAction;
    actFoldLevel9                     : TAction;
    actFormat                         : TAction;
    actHelp                           : TAction;
    actIncFontSize                    : TAction;
    actInsertCharacterFromMap         : TAction;
    actInsertColorValue               : TAction;
    actInspect                        : TAction;
    actLoadHighlighterFromFile        : TAction;
    actLowerCaseSelection             : TAction;
    actNew                            : TAction;
    actOpen                           : TAction;
    actOpenFileAtCursor               : TAction;
    actPageSetup                      : TAction;
    actPascalStringOfSelection        : TAction;
    actShowPreview                    : TAction;
    actPrint                          : TAction;
    actPrintPreview                   : TAction;
    actQuoteLines                     : TAction;
    actQuoteLinesAndDelimit           : TAction;
    actReload                         : TAction;
    actSearchReplace                  : TAction;
    actSave                           : TAction;
    actSaveAs                         : TAction;
    actSettings                       : TAction;
    actShapeCode                      : TAction;
    actSortSelection                  : TAction;
    actToggleComment                  : TAction;
    actToggleFoldLevel                : TAction;
    actToggleHighlighter              : TAction;
    actUpperCaseSelection             : TAction;
    DelphiWebScript                   : TDelphiWebScript;
    dlgColor                          : TColorDialog;
    dlgOpen                           : TOpenDialog;
    dlgSave                           : TSaveDialog;
    dwsGlobalVarsFunctions            : TdwsGlobalVarsFunctions;
    dwsGUIFunctions                   : TdwsGUIFunctions;
    dwsSimpleDebugger                 : TdwsSimpleDebugger;
    imlMain                           : TImageList;
    MenuItem1                         : TMenuItem;
    MenuItem10                        : TMenuItem;
    MenuItem11                        : TMenuItem;
    MenuItem12                        : TMenuItem;
    MenuItem13                        : TMenuItem;
    MenuItem14                        : TMenuItem;
    MenuItem15                        : TMenuItem;
    MenuItem16                        : TMenuItem;
    MenuItem17                        : TMenuItem;
    MenuItem18                        : TMenuItem;
    MenuItem19                        : TMenuItem;
    MenuItem2                         : TMenuItem;
    MenuItem20                        : TMenuItem;
    MenuItem3                         : TMenuItem;
    MenuItem4                         : TMenuItem;
    MenuItem43                        : TMenuItem;
    MenuItem44                        : TMenuItem;
    MenuItem45                        : TMenuItem;
    MenuItem46                        : TMenuItem;
    MenuItem47                        : TMenuItem;
    MenuItem48                        : TMenuItem;
    MenuItem49                        : TMenuItem;
    MenuItem5                         : TMenuItem;
    MenuItem50                        : TMenuItem;
    MenuItem51                        : TMenuItem;
    MenuItem52                        : TMenuItem;
    MenuItem6                         : TMenuItem;
    MenuItem7                         : TMenuItem;
    MenuItem8                         : TMenuItem;
    MenuItem9                         : TMenuItem;
    ppmExecuteScriptOnSelection       : TPopupMenu;
    ppmClipboard                      : TPopupMenu;
    ppmEditor                         : TPopupMenu;
    ppmEncoding                       : TPopupMenu;
    ppmExport                         : TPopupMenu;
    ppmFold                           : TPopupMenu;
    ppmHighLighters                   : TPopupMenu;
    ppmInsert                         : TPopupMenu;
    ppmFile                           : TPopupMenu;
    ppmSearch                         : TPopupMenu;
    ppmLineBreakStyle                 : TPopupMenu;
    ppmSelect                         : TPopupMenu;
    ppmSettings                       : TPopupMenu;
    ppmSelection                      : TPopupMenu;
    ppmSelectionMode                  : TPopupMenu;
    SynExporterHTML                   : TSynExporterHTML;
    SynMacroRecorder                  : TSynMacroRecorder;
    {$endregion}

    {$region 'action handlers' /fold}
    procedure actAboutExecute(Sender: TObject);
    procedure actAlignAndSortSelectionExecute(Sender: TObject);
    procedure actAlignSelectionExecute(Sender: TObject);
    procedure actAssociateFilesExecute(Sender: TObject);
    procedure actAutoFormatXMLExecute(Sender: TObject);
    procedure actAutoGuessHighlighterExecute(Sender: TObject);
    procedure actConvertTabsToSpacesInSelectionExecute(Sender: TObject);
    procedure actExecuteScriptOnSelectionExecute(Sender: TObject);
    procedure actPageSetupExecute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
    procedure actPrintPreviewExecute(Sender: TObject);
    procedure actShowFilterTestExecute(Sender: TObject);
    procedure actShowHexEditorExecute(Sender: TObject);
    procedure actShowHTMLViewerExecute(Sender: TObject);
    procedure actShowMiniMapExecute(Sender: TObject);
    procedure actShowScriptEditorExecute(Sender: TObject);
    procedure actUnindentExecute(Sender: TObject);
    procedure actFindAllOccurencesExecute(Sender: TObject);
    procedure actIndentExecute(Sender: TObject);
    procedure actInsertGUIDExecute(Sender: TObject);
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
    procedure actShowSpecialCharactersExecute(Sender: TObject);
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
    procedure actShowStructureViewerExecute(Sender: TObject);
    {$endregion}

    {$region 'event handlers' /fold}
    procedure SynMacroRecorderStateChange(Sender: TObject);
    procedure UniqueInstanceOtherInstance(Sender: TObject;
      ParamCount: Integer; Parameters: array of String);
    {$endregion}

  private
    FChanged          : Boolean;
    FCurrentViewIndex : Integer;
    FPersistSettings  : Boolean;
    FSynExporterRTF   : TSynExporterRTF;
    FSynUni           : TSynUniSyn;
    FUniqueInstance   : TUniqueInstance;

    FdwsProgramExecution : IdwsProgramExecution;
    FdwsProgram          : IdwsProgram;
    FScriptFunctions     : TStringList;

    FToolViews    : IEditorToolViews;
    FEvents       : IEditorEvents;
    FEditorTools  : TEditorTools;
    FSettings     : IEditorSettings;
    FActiveView   : IEditorView;
    FViewList     : TEditorViewList;
    FSearchEngine : IEditorSearchEngine;

    {$region 'property access methods' /fold}
    function GetActionList: TActionList;
    function GetActions: IEditorActions;
    function GetClipboardPopupMenu: TPopupMenu;
    function GetCommands: IEditorCommands;
    function GetCurrent: IEditorView;
    function GetEditor: TSynEdit;
    function GetEditorPopupMenu: TPopupMenu;
    function GetEditorTools: TEditorTools;
    function GetEditorViews: IEditorViews;
    function GetEncodingPopupMenu: TPopupMenu;
    function GetEvents: IEditorEvents;
    function GetExecuteScriptOnSelectionPopupMenu: TPopupMenu;
    function GetExportPopupMenu: TPopupMenu;
    function GetFilePopupMenu: TPopupMenu;
    function GetFoldPopupMenu: TPopupMenu;
    function GetHighlighterPopupMenu: TPopupMenu;
    function GetInsertPopupMenu: TPopupMenu;
    function GetItem(AName: string): TCustomAction;
    function GetLineBreakStylePopupMenu: TPopupMenu;
    function GetMenus: IEditorMenus;
    function GetPersistSettings: Boolean;
    function GetSearchEngine: IEditorSearchEngine;
    function GetSearchPopupMenu: TPopupMenu;
    function GetSelection: IEditorSelection;
    function GetSelectionModePopupMenu: TPopupMenu;
    function GetSelectionPopupMenu: TPopupMenu;
    function GetSelectPopupMenu: TPopupMenu;
    function GetSettings: IEditorSettings;
    function GetActiveView: IEditorView;
    function GetHighlighters: THighlighters;
    function GetSettingsPopupMenu: TPopupMenu;
    function GetToolViews: IEditorToolViews;
    function GetView(AIndex: Integer): IEditorView;
    function GetViewByFileName(AFileName: string): IEditorView;
    function GetViewByName(AName: string): IEditorView;
    function IEditorViews.GetCount = GetViewCount;
    function GetViewCount: Integer;
    function GetViewList: TEditorViewList;
    function GetViews: IEditorViews;
    procedure SetActiveView(AValue: IEditorView);
    procedure SetEditorTools(AValue: TEditorTools);
    procedure SetPersistSettings(const AValue: Boolean);
    {$endregion}

    function AddMenuItem(
      AParent : TMenuItem;
      AAction : TBasicAction = nil
    ): TMenuItem; overload;
    function AddMenuItem(
      AParent : TMenuItem;
      AMenu   : TMenu
    ): TMenuItem; overload;

    // event handlers
    procedure EditorSettingsChanged(ASender: TObject);

    procedure InitializeFoldHighlighters;
    procedure InitializePopupMenus;
    procedure InitializeActions;
    procedure RegisterHighlighters;
    procedure RegisterToolViews;

    procedure BuildClipboardPopupMenu;
    procedure BuildEncodingPopupMenu;
    procedure BuildLineBreakStylePopupMenu;
    procedure BuildFilePopupMenu;
    procedure BuildHighlighterPopupMenu;
    procedure BuildInsertPopupMenu;
    procedure BuildSearchPopupMenu;
    procedure BuildSelectPopupMenu;
    procedure BuildExecuteScriptOnSelectionPopupMenu;
    procedure BuildSelectionPopupMenu;
    procedure BuildSelectionModePopupMenu;
    procedure BuildSettingsPopupMenu;
    procedure BuildFoldPopupMenu;
    procedure BuildEditorPopupMenu;
    procedure BuildExportPopupMenu;

    procedure LoadScriptFiles;

    procedure ApplyHighlighterAttributes;

  protected
    procedure ActiveViewChanged;

    procedure ExportLines(
      AFormat       : string;
      AToClipBoard  : Boolean = True;
      ANativeFormat : Boolean = True
    );
    procedure FormatCode;

    {$region 'IEditorViews'}
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
    {$endregion}

    procedure ShowToolView(
       const AName      : string;
             AVisible   : Boolean;
             AShowModal : Boolean;
             ASetFocus  : Boolean
    );

    { IEditorCommands } { TODO -oTS : Move to dedicated class or TEditorView }
    function SaveFile(
      const AFileName   : string = '';
            AShowDialog : Boolean = False
    ): Boolean;
    procedure OpenFileAtCursor;
    procedure ToggleHighlighter;
    {$IFDEF Windows}
    procedure CreateDesktopLink;
    {$ENDIF}
    procedure InsertCharacter(const C: TUTF8Char);
    procedure AssignHighlighter(const AName: string);
    procedure CopyToClipboard;

    procedure FindNext;
    procedure FindPrevious;

    // TComponent overrides
    procedure Notification(
      AComponent : TComponent;
      Operation  : TOperation
    ); override;

    procedure UpdateActions;
    procedure UpdateEncodingActions;
    procedure UpdateLineBreakStyleActions;
    procedure UpdateSelectionModeActions;
    procedure UpdateHighLighterActions;
    procedure UpdateFileActions;

    {$region 'IEditorManager'}
    function ActivateView(const AName: string): Boolean;
    procedure ClearHighlightSearch;
    function OpenFile(const AFileName: string): IEditorView;
    function NewFile(
      const AFileName  : string;
      const AText      : string = ''
    ): IEditorView;
    {$endregion}

    // properties
    property ActionList: TActionList
      read GetActionList;

    property Items[AName: string]: TCustomAction
      read GetItem; default;

    property EditorTools: TEditorTools
      read GetEditorTools write SetEditorTools;

    {$region 'IEditorMenus'}
    property ClipboardPopupMenu: TPopupMenu
      read GetClipboardPopupMenu;

    property EditorPopupMenu: TPopupMenu
      read GetEditorPopupMenu;

    property EncodingPopupMenu: TPopupMenu
      read GetEncodingPopupMenu;

    property LineBreakStylePopupMenu: TPopupMenu
      read GetLineBreakStylePopupMenu;

    property ExportPopupMenu: TPopupMenu
      read GetExportPopupMenu;

    property FilePopupMenu: TPopupMenu
      read GetFilePopupMenu;

    property FoldPopupMenu: TPopupMenu
      read GetFoldPopupMenu;

    property HighlighterPopupMenu: TPopupMenu
      read GetHighlighterPopupMenu;

    property InsertPopupMenu: TPopupMenu
      read GetInsertPopupMenu;

    property SearchPopupMenu: TPopupMenu
      read GetSearchPopupMenu;

    property SelectPopupMenu: TPopupMenu
      read GetSelectPopupMenu;

    property SelectionPopupMenu: TPopupMenu
      read GetSelectionPopupMenu;

    property ExecuteScriptOnSelectionPopupMenu: TPopupMenu
      read GetExecuteScriptOnSelectionPopupMenu;

    property SelectionModePopupMenu: TPopupMenu
      read GetSelectionModePopupMenu;

    property SettingsPopupMenu: TPopupMenu
      read GetSettingsPopupMenu;
    {$endregion}

    {$region 'IEditorManager'}
    property PersistSettings: Boolean
      read GetPersistSettings write SetPersistSettings;

    property Highlighters: THighlighters
      read GetHighlighters;

    { Set/get the reference to the active view. }
    property ActiveView: IEditorView
      read GetActiveView write SetActiveView;

    property Events: IEditorEvents
      read GetEvents implements IEditorEvents;

    property ToolViews: IEditorToolViews
      read GetToolViews;

    property Menus: IEditorMenus
      read GetMenus;

    property Actions: IEditorActions
      read GetActions;

    property Commands: IEditorCommands
      read GetCommands;

    property Selection: IEditorSelection
      read GetSelection;

    property Settings: IEditorSettings
      read GetSettings implements IEditorSettings;

    property SearchEngine: IEditorSearchEngine
      read GetSearchEngine implements IEditorSearchEngine;
    {$endregion}

    property View: IEditorView
      read GetActiveView implements IEditorView;

    {$region 'IEditorViews'}
    property Views[AIndex: Integer]: IEditorView
      read GetView;

    property ViewByName[AName: string]: IEditorView
      read GetViewByName;

    property ViewByFileName[AFileName: string]: IEditorView
      read GetViewByFileName;

    property ViewList: TEditorViewList
      read GetViewList;

    property ViewCount: Integer
      read GetViewCount;
    {$endregion}

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

{$R *.lfm}

uses
{$IFDEF Windows}
  ShlObj, Windows,

  ts.Core.Logger.Channel.IPC,
{$ENDIF}
  FileUtil, Clipbrd, StrUtils, Math, TypInfo, Contnrs,

  LConvEncoding,

  SynEditKeyCmds, SynEditTypes, SynPluginSyncroEdit, SynEditHighlighterFoldBase,

  SynHighlighterPas, SynHighlighterSQL, SynHighlighterLFM, SynHighlighterXML,
  SynHighlighterBat, SynHighlighterHTML, SynHighlighterCpp, SynHighlighterJava,
  SynHighlighterPerl, SynHighlighterPython, SynHighlighterPHP, SynHighlighterCss,
  SynHighlighterJScript, SynHighlighterDiff, SynHighlighterTeX, SynHighlighterPo,
  SynhighlighterUnixShellScript, SynHighlighterIni,

  ts.Core.Utils, ts_Core_ComponentInspector,

  ts.Editor.HighlighterAttributes,

  ts.Editor.Settings, ts.Editor.Utils,

  ts_Editor_ViewList_ToolView,
  ts_Editor_CodeShaper_ToolView,
  ts_Editor_Preview_ToolView,
  ts_Editor_Test_ToolView,
  ts_Editor_Search_ToolView,
  ts_Editor_Shortcuts_ToolView,
  ts_Editor_ActionList_ToolView,
  ts_Editor_CodeFilter_ToolView,
  ts_Editor_CharacterMap_ToolView,
  ts_Editor_Structure_ToolView,
  ts_Editor_AlignLines_ToolView,
  ts_Editor_HTMLView_ToolView,
  ts_Editor_HexEditor_ToolView,
  ts_Editor_Minimap_Toolview,
  ts_Editor_ScriptEditor_ToolView,
  ts_Editor_SelectionInfo_ToolView,
  ts_Editor_Filter_ToolView,

  ts_Editor_AboutDialog, ts.Editor.CodeFormatters, ts.Editor.CodeFormatters.SQL,

  ts.Editor.Search.Engine,

  ts.Editor.Toolview.Manager,

  ts_Editor_SettingsDialog_Old,
  ts_Editor_SettingsDialog,
  ts_Editor_SettingsDialog_FileAssociations,

  ts.Editor.ActionList.Templates;

{$region 'construction and destruction' /fold}
procedure TdmEditorManager.AfterConstruction;
begin
  inherited AfterConstruction;
  FToolViews := TToolViews.Create(Self);
  FEvents    := TEditorEvents.Create(Self);
  FPersistSettings  := False;
  FSettings         := TEditorSettings.Create(Self);
  FSettings.AddEditorSettingsChangedHandler(EditorSettingsChanged);
  FViewList         := TEditorViewList.Create;
  FSearchEngine     := TSearchEngine.Create(Self);
  FSynExporterRTF   := TSynExporterRTF.Create(Self);
  FSynUni           := TSynUniSyn.Create(Self);
  FUniqueInstance   := TUniqueInstance.Create(Self);
  FUniqueInstance.Identifier := ApplicationName;
  FUniqueInstance.OnOtherInstance := UniqueInstanceOtherInstance;
  FScriptFunctions := TStringList.Create;
  FEditorTools := [
    etActionList,
    etAlignLines,
    etCodeFilter,
    etCodeShaper,
    etHTMLView,
    etPreview,
    etSearch,
    etHexEditor,
    etViewList
  ];

  // TODO: these steps should be configurable
  LoadScriptFiles;
  RegisterToolViews;
  RegisterHighlighters;

  InitializeFoldHighlighters;
  InitializeActions;
  InitializePopupMenus;
end;

procedure TdmEditorManager.BeforeDestruction;
begin
  FActiveView := nil;
  if PersistSettings then
    FSettings.Save;
  FSearchEngine := nil;
  FSettings := nil;
  FEvents   := nil;
  FToolViews := nil;
  FdwsProgramExecution := nil;
  FdwsProgram := nil;
  FreeAndNil(FScriptFunctions);
  FreeAndNil(FViewList);
  inherited BeforeDestruction;
end;
{$endregion}

{$region 'property access methods' /fold}
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

function TdmEditorManager.GetEditorTools: TEditorTools;
begin
  Result := FEditorTools;
end;

procedure TdmEditorManager.SetEditorTools(AValue: TEditorTools);
begin
  FEditorTools := AValue;
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
  Result := FEvents;
end;

function TdmEditorManager.GetExecuteScriptOnSelectionPopupMenu: TPopupMenu;
begin
  Result := ppmExecuteScriptOnSelection;
end;

function TdmEditorManager.GetExportPopupMenu: TPopupMenu;
begin
  Result := ppmExport;
end;

function TdmEditorManager.GetFilePopupMenu: TPopupMenu;
begin
  Result := ppmFile;
end;

function TdmEditorManager.GetFoldPopupMenu: TPopupMenu;
begin
  Result := ppmFold;
end;

function TdmEditorManager.GetHighlighterPopupMenu: TPopupMenu;
begin
  Result := ppmHighLighters;
end;

function TdmEditorManager.GetInsertPopupMenu: TPopupMenu;
begin
  Result := ppmInsert;
end;

function TdmEditorManager.GetActionList: TActionList;
begin
  Result := aclActions;
end;

function TdmEditorManager.GetActions: IEditorActions;
begin
  Result := Self as IEditorActions;
end;

function TdmEditorManager.GetClipboardPopupMenu: TPopupMenu;
begin
  Result := ppmClipboard;
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
var
  A: TCustomAction;
begin
  A := aclActions.ActionByName(AName) as TCustomAction;
  if Assigned(A) then
    Result := A
  else
    Logger.SendWarning(Format('Action with name (%s) not found!', [AName]));
end;

function TdmEditorManager.GetLineBreakStylePopupMenu: TPopupMenu;
begin
  Result := ppmLineBreakStyle;
end;

function TdmEditorManager.GetMenus: IEditorMenus;
begin
  Result := Self as IEditorMenus;
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
      // TSI temp
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

function TdmEditorManager.GetSearchPopupMenu: TPopupMenu;
begin
  Result := ppmSearch;
end;

function TdmEditorManager.GetSelection: IEditorSelection;
begin
  Result := ActiveView.Selection;
end;

function TdmEditorManager.GetSelectionModePopupMenu: TPopupMenu;
begin
  Result := ppmSelectionMode;
end;

function TdmEditorManager.GetSelectionPopupMenu: TPopupMenu;
begin
  Result := ppmSelection;
end;

function TdmEditorManager.GetSelectPopupMenu: TPopupMenu;
begin
  Result := ppmSelect;
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
    Events.DoActiveViewChange;
    ActiveViewChanged;
  end;
end;

function TdmEditorManager.GetHighlighters: THighlighters;
begin
  Result := Settings.Highlighters;
end;

function TdmEditorManager.GetSettingsPopupMenu: TPopupMenu;
begin
  Result := ppmSettings;
end;

function TdmEditorManager.GetToolViews: IEditorToolViews;
begin
  Result := FToolViews as IEditorToolViews;
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
{$endregion}

{$region 'action handlers' /fold}
procedure TdmEditorManager.actSortSelectionExecute(Sender: TObject);
begin
  { TODO -oTS : implement a new toolform for this }
  ShowMessage('This feature is not implemented yet.');
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
    OpenFile(dlgOpen.FileName);
  end;
end;

procedure TdmEditorManager.actPascalStringOfSelectionExecute(Sender: TObject);
begin
  ActiveView.PascalStringFromSelection;
end;

procedure TdmEditorManager.actPasteExecute(Sender: TObject);
begin
  if ActiveView.Focused then
    ActiveView.Paste;
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
  if ActiveView.IsFile then
    SaveFile(ActiveView.FileName)
  else
  begin
    Events.DoSave(ActiveView.FileName);
    ActiveView.Save;
  end;
end;

procedure TdmEditorManager.actSaveAsExecute(Sender: TObject);
begin
  SaveFile(ActiveView.FileName, True);
end;

procedure TdmEditorManager.actSearchExecute(Sender: TObject);
begin
  ShowToolView('Search', (Sender as TAction).Checked, False, True);
end;

procedure TdmEditorManager.actSearchReplaceExecute(Sender: TObject);
begin
  ShowToolView('Search', (Sender as TAction).Checked, False, True);
end;

procedure TdmEditorManager.actShapeCodeExecute(Sender: TObject);
begin
  ShowToolView('CodeShaper', (Sender as TAction).Checked, False, True);
end;

procedure TdmEditorManager.actInsertCharacterFromMapExecute(Sender: TObject);
begin
  ShowToolView('CharacterMap', (Sender as TAction).Checked, False, False);
end;

procedure TdmEditorManager.actAlignSelectionExecute(Sender: TObject);
begin
  ShowToolView('AlignLines', (Sender as TAction).Checked, False, True);
end;

procedure TdmEditorManager.actAssociateFilesExecute(Sender: TObject);
var
  F : TForm;
begin
  F := TfrmOptionsAssociate.Create(Self);
  try
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TdmEditorManager.actShowPreviewExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
  begin
    ShowToolView('Preview', (Sender as TAction).Checked, False, False);
    Settings.PreviewVisible := (Sender as TAction).Checked;
  end;
end;

procedure TdmEditorManager.actShowTestExecute(Sender: TObject);
begin
  ShowToolView('Test', (Sender as TAction).Checked, False, False);
end;

procedure TdmEditorManager.actShowViewsExecute(Sender: TObject);
begin
  ShowToolView('ViewList', (Sender as TAction).Checked, True, True);
end;

procedure TdmEditorManager.actShowActionsExecute(Sender: TObject);
begin
  ShowToolView('ActionListView', (Sender as TAction).Checked, True, True);
end;

procedure TdmEditorManager.actShowHexEditorExecute(Sender: TObject);
begin
  ShowToolView('HexEditor', (Sender as TAction).Checked, False, True);
end;

procedure TdmEditorManager.actShowHTMLViewerExecute(Sender: TObject);
begin
  ShowToolView('HTMLView', (Sender as TAction).Checked, False, False);
end;

procedure TdmEditorManager.actShowMiniMapExecute(Sender: TObject);
begin
  ShowToolView('MiniMap', (Sender as TAction).Checked, False, False);
end;

procedure TdmEditorManager.actShowScriptEditorExecute(Sender: TObject);
begin
  ShowToolView('ScriptEditor', (Sender as TAction).Checked, False, False);
end;

procedure TdmEditorManager.actShowStructureViewerExecute(Sender: TObject);
begin
  ShowToolView('Structure', (Sender as TAction).Checked, False, False);
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
  {$IFDEF Windows}
  CreateDesktopLink;
  {$ELSE}
  ShowMessage('This feature is not available yet for this platform');
  {$ENDIF}
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
    ActiveView.Cut;
end;

procedure TdmEditorManager.actFilterCodeExecute(Sender: TObject);
var
  ETV: IEditorToolView;
begin
  ETV := ToolViews.ViewByName['CodeFilter'];
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
    ActiveView.Editor,
    ActiveView.Editor.Gutter.ChangesPart,
    ActiveView.Editor.Gutter.LineNumberPart,
    ActiveView.Editor.Gutter.CodeFoldPart,
    ActiveView.Editor.Gutter.SeparatorPart
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
  ActiveView.Load;
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
  ActiveView.SyncEditSelection;
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

procedure TdmEditorManager.actAutoFormatXMLExecute(Sender: TObject);
begin
  (Sender as TAction).Checked := not (Sender as TAction).Checked;
  Settings.AutoFormatXML := (Sender as TAction).Checked;
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
  ActiveView.QuoteSelection;
end;

procedure TdmEditorManager.actRedoExecute(Sender: TObject);
begin
  ActiveView.Redo;
end;

procedure TdmEditorManager.actSettingsExecute(Sender: TObject);
begin
  //with TEditorSettingsDialog.Create(Self) do
  //begin
  //  ShowModal;
  //end;
  ExecuteSettingsDialog(Self);
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

procedure TdmEditorManager.actConvertTabsToSpacesInSelectionExecute(
  Sender: TObject);
begin
  ActiveView.ConvertTabsToSpacesInSelection;
end;

procedure TdmEditorManager.actExecuteScriptOnSelectionExecute(Sender: TObject);
var
  A  : TAction;
  FI : IInfo;
begin
  Selection.Store;
  try
    A := Sender as TAction;
    if Assigned(FdwsProgramExecution) then
    begin
      FdwsProgramExecution.BeginProgram;
      try
        FI := FdwsProgramExecution.Info.Func[A.Caption];
        if Assigned(FI) then
        begin
          Selection.Text := FI.Call(
            [Selection.Text]
          ).GetValueAsString;
        end
        else
          raise Exception.Create('Script function not found');
      finally
        FdwsProgramExecution.EndProgram;
      end;
    end
    else
      raise Exception.Create('No script file was found');
  finally
    Selection.Restore;
  end;
end;

procedure TdmEditorManager.actPageSetupExecute(Sender: TObject);
begin
  ShowMessage('Not implemented yet');
end;

procedure TdmEditorManager.actPrintExecute(Sender: TObject);
begin
  ShowMessage('Not implemented yet');
end;

procedure TdmEditorManager.actPrintPreviewExecute(Sender: TObject);
begin
  ShowMessage('Not implemented yet');
end;

procedure TdmEditorManager.actShowFilterTestExecute(Sender: TObject);
var
  F : TfrmFilter;
  OL : TObjectList;
  A: TContainedAction;
begin
  OL := TObjectList.create(False);
  try
    for A in ActionList do
    begin
      OL.Add(A);
      if OL.Count = 5  then
        Break;
    end;

    F := (ToolViews['Filter'].Form as TfrmFilter);
    F.ColumnDefinitions.AddColumn('Name', 'Name');
    F.ColumnDefinitions.AddColumn('Caption', 'Caption');
    //F.ItemTemplate  := TActionCategoryTemplate.Create(F.ColumnDefinitions);
    F.ItemsSource := OL;
    ShowToolView('Filter', True, True, True);
  finally
    OL.Free;
  end;

  // CTRL-ALT-SHIFT-F5
end;

procedure TdmEditorManager.actUnindentExecute(Sender: TObject);
begin
  ActiveView.Unindent;
end;

procedure TdmEditorManager.actFindAllOccurencesExecute(Sender: TObject);
begin
  // TODO
  SearchEngine.SearchText := ActiveView.CurrentWord;
  SearchEngine.Execute;
end;

procedure TdmEditorManager.actIndentExecute(Sender: TObject);
begin
  ActiveView.Indent;
end;

procedure TdmEditorManager.actInsertGUIDExecute(Sender: TObject);
var
  GUID: TGUID;
begin
  CreateGUID(GUID);
  ActiveView.InsertTextAtCaret(GUIDToString(GUID));
end;

procedure TdmEditorManager.actNewSharedViewExecute(Sender: TObject);
begin
  AddSharedView(ActiveView);
end;

procedure TdmEditorManager.actSelectionInfoExecute(Sender: TObject);
begin
  ShowToolView('SelectionInfo', (Sender as TAction).Checked, False, False);
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
  ShowMessage('Not implemented yet');
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
  ActiveView.DequoteSelection;
end;

procedure TdmEditorManager.actEncodeBase64Execute(Sender: TObject);
begin
  ActiveView.Base64FromSelection;
end;

procedure TdmEditorManager.actExitExecute(Sender: TObject);
begin
  ClearViews;
  //Application.Terminate;
end;

procedure TdmEditorManager.actFindNextWordExecute(Sender: TObject);
begin
  ActiveView.FindNextWordOccurrence(True);
end;

procedure TdmEditorManager.actFindPrevWordExecute(Sender: TObject);
begin
  ActiveView.FindNextWordOccurrence(False);
end;

procedure TdmEditorManager.actCopyExecute(Sender: TObject);
begin
  if ActiveView.Focused then
    ActiveView.Copy;
end;

procedure TdmEditorManager.actDecFontSizeExecute(Sender: TObject);
begin
  ActiveView.AdjustFontSize(-1);
end;

procedure TdmEditorManager.actDecodeBase64Execute(Sender: TObject);
begin
  ActiveView.Base64FromSelection(True);
end;

procedure TdmEditorManager.actShowSpecialCharactersExecute(Sender: TObject);
begin
  Settings.ShowSpecialCharacters := (Sender as TAction).Checked;
end;

procedure TdmEditorManager.actEncodingExecute(Sender: TObject);
begin
  ActiveView.Encoding := (Sender as TAction).Caption;
end;

procedure TdmEditorManager.actLineBreakStyleExecute(Sender: TObject);
begin
  ActiveView.LineBreakStyle := (Sender as TAction).Caption;
end;
{$endregion}

{$region 'event handlers' /fold}
procedure TdmEditorManager.SynMacroRecorderStateChange(Sender: TObject);
begin
  Events.DoMacroStateChange(SynMacroRecorder.State);
end;

procedure TdmEditorManager.UniqueInstanceOtherInstance(Sender: TObject;
   ParamCount: Integer; Parameters: array of String);
begin
  Events.DoOpenOtherInstance(Parameters);
end;

procedure TdmEditorManager.EditorSettingsChanged(ASender: TObject);
begin
  //if actShowPreview.Checked <> Settings.PreviewVisible then
  //begin
  //  actShowPreview.Execute;
  //end;
  FUniqueInstance.Enabled := Settings.SingleInstance;
  //ApplyHighlighterAttributes;
end;

{$endregion}

{$region 'private methods' /fold}
{$region 'Helpers' /fold}
function TdmEditorManager.AddMenuItem(AParent: TMenuItem; AAction: TBasicAction
  ): TMenuItem;
var
  MI: TMenuItem;
begin
  if not Assigned(AAction) then
  begin
    AParent.AddSeparator;
    Result := nil;
  end
  else
  begin
    MI := TMenuItem.Create(AParent.Owner);
    MI.Action := AAction;
    if (AAction is TAction) and (TAction(AAction).GroupIndex > 0) then
    begin
      MI.GlyphShowMode := gsmNever;
      MI.RadioItem := True;
    end;
    if (AAction is TAction) and (TAction(AAction).AutoCheck) then
    begin
      MI.GlyphShowMode := gsmNever;
      MI.ShowAlwaysCheckable := True;
    end;
    AParent.Add(MI);
    Result := MI;
  end;
end;

function TdmEditorManager.AddMenuItem(AParent: TMenuItem; AMenu: TMenu
  ): TMenuItem;
var
  MI  : TMenuItem;
  M   : TMenuItem;
  SM  : TMenuItem;
  SMI : TMenuItem;
  I   : Integer;
begin
  MI := TMenuItem.Create(AMenu);
  MI.Action := AMenu.Items.Action;
  AParent.Add(MI);
  for M in AMenu.Items do
  begin
    SMI := AddMenuItem(MI, M.Action);
    // add submenu(s)
    if M.Count > 0 then
    begin
      for I := 0 to M.Count - 1 do
      begin
        SM := M.Items[I];
        AddMenuItem(SMI, SM.Action);
      end;
    end;
  end;
  Result := MI;
end;
{$endregion}

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
begin
  BuildClipboardPopupMenu;
  BuildEncodingPopupMenu;
  BuildExportPopupMenu;
  BuildFoldPopupMenu;
  BuildHighlighterPopupMenu;
  BuildInsertPopupMenu;
  BuildLineBreakStylePopupMenu;
  BuildSearchPopupMenu;
  BuildSelectionModePopupMenu;
  BuildExecuteScriptOnSelectionPopupMenu;
  BuildSelectionPopupMenu;
  BuildSelectPopupMenu;
  BuildSettingsPopupMenu;
  BuildFilePopupMenu;
  BuildEditorPopupMenu;
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
      A.Name    := actEncodingMenu.Name + DelChars(S, '-');
      A.AutoCheck := True;
      A.GroupIndex := 3;
      A.Category := actEncodingMenu.Category;
      A.OnExecute  := actEncodingExecute;
    end;
    for S in ALineBreakStyles do
    begin
      A := TAction.Create(aclActions);
      A.ActionList := ActionList;
      A.Caption := S;
      A.Name    := actLineBreakStyleMenu.Name + S;
      A.AutoCheck := True;
      A.GroupIndex := 4;
      A.Category := actLineBreakStyleMenu.Category;
      A.OnExecute  := actLineBreakStyleExecute;
    end;
    for HI in Highlighters do
    begin
      A.Tag := HI.Index;
      A := TAction.Create(ActionList);
      A.ActionList := ActionList;
      A.Caption := HI.Name;
      A.Name := actHighlighter.Name + HI.Name;
      A.AutoCheck := True;
      A.GroupIndex := 5;
      A.Category := actHighlighter.Category;
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
      A.Name := actSelectionModeMenu.Name + S;
      A.AutoCheck := True;
      A.GroupIndex := 6;
      A.Category := actSelectionModeMenu.Category;
      A.OnExecute := actSelectionModeExecute;
    end;
  finally
    FreeAndNil(SL);
  end;
end;

{ Applies common highlighter attributes }
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
          if (A.Name = HAI.Name) or (HAI.AliasNames.IndexOf(A.Name) >=0) then
          begin
            A.Assign(HAI.Attributes);
          end;
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
  Reg(TSynPoSyn, nil, HL_PO, FILE_EXTENSIONS_PO, SPODescription, '#');
  Reg(TSynCppSyn, nil, HL_CPP, FILE_EXTENSIONS_CPP, SCPPDescription, '//', '/*', '*/', TCPPFormatter.Create);
  Reg(TSynJavaSyn, nil, HL_JAVA, FILE_EXTENSIONS_JAVA, SJavaDescription, '//', '/*', '*/', TJavaFormatter.Create);
  Reg(TSynPerlSyn, nil, HL_PERL, FILE_EXTENSIONS_PERL, SPERLDescription, '#', '/*', '*/');
  Reg(TSynPythonSyn, nil, HL_PY, FILE_EXTENSIONS_PY, SPYDescription, '#', '/*', '*/');
  Reg(TSynHTMLSyn, nil, HL_HTML, FILE_EXTENSIONS_HTML, SHTMLDescription, '', '<!--', '-->', THTMLFormatter.Create);
  Reg(TSynJScriptSyn, nil, HL_JS, FILE_EXTENSIONS_JS, SJSDescription);
  Reg(TSynPHPSyn, nil, HL_PHP, FILE_EXTENSIONS_PHP, SPHPDescription, '');
  Reg(TSynCssSyn, nil, HL_CSS, FILE_EXTENSIONS_CSS, SCSSDescription);
  Reg(TSynDiffSyn, nil, HL_DIFF, FILE_EXTENSIONS_DIFF, SDIFFDescription);
  Reg(TSynTeXSyn, nil, HL_TEX, FILE_EXTENSIONS_TEX, STEXDescription);
  Reg(TSynUNIXShellScriptSyn, nil, HL_SH, FILE_EXTENSIONS_SH, SSHDescription);
  //Reg(TSynIniSyn, nil, HL_INI, FILE_EXTENSIONS_INI, SINIDescription, '#');
    // apply common highlighter attributes


//  S := ExtractFilePath(Application.ExeName);
  S := '';

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
  F := S + LAYOUT_RUBY;
  if FileExistsUTF8(F) then
    Reg(TSynUniSyn, FSynUni, HL_RUBY, FILE_EXTENSIONS_RUBY, SRUBYDescription, '#', '/*', '*/', nil, F);
  F := S + LAYOUT_LUA;
  if FileExistsUTF8(F) then
    Reg(TSynUniSyn, FSynUni, HL_LUA, FILE_EXTENSIONS_LUA, SLUADescription, '--', '', '', nil, F);

  ApplyHighlighterAttributes;
end;

procedure TdmEditorManager.RegisterToolViews;
begin
  ToolViews.Register(TfrmCodeShaper, 'CodeShaper');
  ToolViews.Register(TfrmSearchForm, 'Search');
  ToolViews.Register(TfrmViewList, 'ViewList');
  ToolViews.Register(TfrmPreview, 'Preview');
  ToolViews.Register(TfrmActionListView, 'ActionListView');
  ToolViews.Register(TfrmTest, 'Test');
  ToolViews.Register(TfrmAlignLines, 'AlignLines');
  ToolViews.Register(TfrmCodeFilterDialog, 'CodeFilter');
  ToolViews.Register(TfrmSelectionInfo, 'SelectionInfo');
  ToolViews.Register(TfrmStructure, 'Structure');
  ToolViews.Register(TfrmCharacterMap, 'CharacterMap');
  ToolViews.Register(TfrmHTMLView, 'HTMLView');
  ToolViews.Register(TfrmHexEditor, 'HexEditor');
  ToolViews.Register(TfrmMiniMap, 'MiniMap');
  ToolViews.Register(TfrmScriptEditor, 'ScriptEditor');
  ToolViews.Register(TfrmFilter, 'Filter');
end;

procedure TdmEditorManager.BuildClipboardPopupMenu;
var
  MI: TMenuItem;
begin
  MI := ClipboardPopupMenu.Items;
  MI.Clear;
  MI.Action := actClipboardMenu;
  AddMenuItem(MI, actCopyFileName);
  AddMenuItem(MI, actCopyFilePath);
  AddMenuItem(MI, actCopyFullPath);
  AddMenuItem(MI);
  AddMenuItem(MI, actCopyHTMLTextToClipboard);
  AddMenuItem(MI, actCopyRTFTextToClipboard);
  AddMenuItem(MI, actCopyWikiTextToClipboard);
  AddMenuItem(MI);
  AddMenuItem(MI, actCopytHTMLToClipboard);
  AddMenuItem(MI, actCopyRTFToClipboard);
  AddMenuItem(MI, actCopyWikiToClipboard);
end;

procedure TdmEditorManager.BuildEncodingPopupMenu;
var
  SL : TStringList;
  S  : string;
  A  : TCustomAction;
  MI : TMenuItem;
begin
  SL := TStringList.Create;
  try
    MI := EncodingPopupMenu.Items;
    MI.Clear;
    MI.Action := actEncodingMenu;
    GetSupportedEncodings(SL);
    for S in SL do
    begin
      S := MI.Action.Name + DelChars(S, '-');
      A := Items[S];
      if Assigned(A) then
      begin
        AddMenuItem(MI, A);
      end;
    end;
  finally
    FreeAndNil(SL);
  end;
end;

procedure TdmEditorManager.BuildLineBreakStylePopupMenu;
var
  MI : TMenuItem;
  S  : string;
  A  : TCustomAction;
begin
  MI := LineBreakStylePopupMenu.Items;
  MI.Clear;
  MI.Action := actLineBreakStyleMenu;
  for S in ALineBreakStyles do
  begin
    MI := TMenuItem.Create(LineBreakStylePopupMenu);
    S := LineBreakStylePopupMenu.Items.Action.Name +  S;
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
end;

procedure TdmEditorManager.BuildFilePopupMenu;
var
  MI: TMenuItem;
begin
  MI := FilePopupMenu.Items;
  MI.Clear;
  MI.Action := actFileMenu;
  AddMenuItem(MI, actNew);
  AddMenuItem(MI, actOpen);
  AddMenuItem(MI, actSave);
  AddMenuItem(MI, actSaveAs);
  AddMenuItem(MI);
  AddMenuItem(MI, actMonitorChanges);
  AddMenuItem(MI, actCreateDesktopLink);
  AddMenuItem(MI);
  AddMenuItem(MI, actReload);
  AddMenuItem(MI, EncodingPopupMenu);
  AddMenuItem(MI, LineBreakStylePopupMenu);
  AddMenuItem(MI);
  AddMenuItem(MI, actPageSetup);
  AddMenuItem(MI, actPrintPreview);
  AddMenuItem(MI, actPrint);
end;

procedure TdmEditorManager.BuildHighlighterPopupMenu;
var
  MI : TMenuItem;
  S  : string;
  HI : THighlighterItem;
  A  : TCustomAction;
begin
  HighlighterPopupMenu.Items.Action := actHighlighterMenu;
  HighlighterPopupMenu.Items.Clear;
  for HI in Highlighters do
  begin
    MI := TMenuItem.Create(HighlighterPopupMenu);
    S := actHighlighter.Name + HI.Name;
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
end;

procedure TdmEditorManager.BuildSearchPopupMenu;
var
  MI : TMenuItem;
begin
  MI := SearchPopupMenu.Items;
  MI.Clear;
  MI.Action := actSearchMenu;
  AddMenuItem(MI, actSearch);
  AddMenuItem(MI, actSearchReplace);
  AddMenuItem(MI, actFindAllOccurences);
  AddMenuItem(MI);
  AddMenuItem(MI, actFindNext);
  AddMenuItem(MI, actFindPrevious);
  AddMenuItem(MI);
  AddMenuItem(MI, actFindNextWord);
  AddMenuItem(MI, actFindPrevWord);
end;

procedure TdmEditorManager.BuildSelectPopupMenu;
var
  MI : TMenuItem;
begin
  MI := SelectPopupMenu.Items;
  MI.Clear;
  MI.Action := actSelectMenu;
  AddMenuItem(MI, actSelectAll);
  AddMenuItem(MI);
  AddMenuItem(MI, actClear);
  AddMenuItem(MI);
  AddMenuItem(MI, actSmartSelect);
end;

procedure TdmEditorManager.BuildExecuteScriptOnSelectionPopupMenu;
var
  MI : TMenuItem;
  S  : string;
  A  : TCustomAction;
begin
  MI := ExecuteScriptOnSelectionPopupMenu.Items;
  MI.Clear;
  MI.Action := actExecuteScriptOnSelection;
  for S in FScriptFunctions do
  begin
    MI := TMenuItem.Create(ExecuteScriptOnSelectionPopupMenu);
    A := Items[actExecuteScriptOnSelection.Name + S];
    if Assigned(A) then
    begin
      MI.Action     := A;
      MI.Hint       := A.Hint;
      MI.Caption    := A.Caption;
      MI.AutoCheck  := A.AutoCheck;
      MI.RadioItem  := False;
      MI.GroupIndex := A.GroupIndex;
    end;
    ExecuteScriptOnSelectionPopupMenu.Items.Add(MI);
  end;
end;

procedure TdmEditorManager.BuildSelectionPopupMenu;
var
  MI : TMenuItem;
begin
  MI := SelectionPopupMenu.Items;
  MI.Clear;
  MI.Action := actSelectionMenu;
  AddMenuItem(MI, actSyncEdit);
  AddMenuItem(MI, actAlignSelection);
  AddMenuItem(MI, actSortSelection);
  AddMenuItem(MI);
  AddMenuItem(MI, actIndent);
  AddMenuItem(MI, actUnindent);
  AddMenuItem(MI);
  AddMenuItem(MI, actUpperCaseSelection);
  AddMenuItem(MI, actLowerCaseSelection);
  AddMenuItem(MI);
  AddMenuItem(MI, actStripFirstChar);
  AddMenuItem(MI, actStripLastChar);
  AddMenuItem(MI, actStripMarkup);
  AddMenuItem(MI);
  AddMenuItem(MI, actToggleComment);
  AddMenuItem(MI, actToggleBlockCommentSelection);
  AddMenuItem(MI);
  AddMenuItem(MI, actQuoteSelection);
  AddMenuItem(MI, actDequoteSelection);
  AddMenuItem(MI, actQuoteLinesAndDelimit);
  AddMenuItem(MI, actQuoteLines);
  AddMenuItem(MI, actDequoteLines);
  AddMenuItem(MI);
  AddMenuItem(MI, actEncodeBase64);
  AddMenuItem(MI, actDecodeBase64);
  AddMenuItem(MI);
  AddMenuItem(MI, actConvertTabsToSpacesInSelection);
  AddMenuItem(MI);
  AddMenuItem(MI, actPascalStringOfSelection);
  AddMenuItem(MI, ExecuteScriptOnSelectionPopupMenu);
end;

procedure TdmEditorManager.BuildInsertPopupMenu;
var
  MI : TMenuItem;
begin
  MI := InsertPopupMenu.Items;
  MI.Clear;
  MI.Action := actInsertMenu;
  AddMenuItem(MI, actInsertCharacterFromMap);
  AddMenuItem(MI, actInsertColorValue);
  AddMenuItem(MI, actInsertGUID);
end;

procedure TdmEditorManager.BuildSelectionModePopupMenu;
var
  SM : TSynSelectionMode;
  MI : TMenuItem;
  S  : string;
  A  : TCustomAction;
begin
  SelectionModePopupMenu.Items.Clear;
  SelectionModePopupMenu.Items.Action := actSelectionModeMenu;
  for SM := Low(TSynSelectionMode) to High(TSynSelectionMode) do
  begin
    MI := TMenuItem.Create(SelectionModePopupMenu);
    S := GetEnumName(TypeInfo(TSynSelectionMode), Ord(SM));
    S := System.Copy(S, 3, Length(S));
    S := SelectionModePopupMenu.Items.Action.Name + S;
    A := Items[S];
    if Assigned(A) then
    begin
      MI.Action     := A;
      MI.Hint       := A.Hint;
      MI.Caption    := A.Caption;
      MI.AutoCheck  := A.AutoCheck;
      MI.RadioItem  := True;
      MI.GroupIndex := A.GroupIndex;
    end;
    SelectionModePopupMenu.Items.Add(MI);
  end;
end;

procedure TdmEditorManager.BuildSettingsPopupMenu;
var
  MI : TMenuItem;
begin
  MI := SettingsPopupMenu.Items;
  MI.Clear;
  MI.Action := actSettingsMenu;
  AddMenuItem(MI, actSettings);
  AddMenuItem(MI);
  AddMenuItem(MI, actShowSpecialCharacters);
  AddMenuItem(MI);
  AddMenuItem(MI, actIncFontSize);
  AddMenuItem(MI, actDecFontSize);
  AddMenuItem(MI);
  AddMenuItem(MI, actStayOnTop);
  AddMenuItem(MI, actSingleInstance);
end;

procedure TdmEditorManager.BuildFoldPopupMenu;
begin
  FoldPopupMenu.Items.Action := actToggleFoldLevel;
end;

procedure TdmEditorManager.BuildExportPopupMenu;
var
  MI : TMenuItem;
begin
  MI := ExportPopupMenu.Items;
  MI.Clear;
  MI.Action := actExportMenu;
  AddMenuItem(MI, actExportToHTML);
  AddMenuItem(MI, actExportToRTF);
  AddMenuItem(MI, actExportToWiki);
end;

procedure TdmEditorManager.LoadScriptFiles;
var
  A : TAction;
  S : string;
  PS : TSymbol;
begin
  actExecuteScriptOnSelection.Enabled := False;
  if FileExistsUTF8('notepas.dws') then
  begin
    S := ReadFileToString('notepas.dws');
    FdwsProgram := DelphiWebScript.Compile(S);
    if FdwsProgram.Msgs.Count = 0 then
    begin
      FdwsProgramExecution := FdwsProgram.Execute;
      for PS in  FdwsProgram.Table do
      begin
        if PS is TFuncSymbol then
        begin
          actExecuteScriptOnSelection.Enabled := True;
          FScriptFunctions.Add(PS.Name);
          A := TAction.Create(ActionList);
          A.Hint       := (PS as TFuncSymbol).Description;
          A.ActionList := ActionList;
          A.Caption    := PS.Name;
          A.Name       := actExecuteScriptOnSelection.Name + PS.Name;
          A.Category   := actExecuteScriptOnSelection.Category;
          A.OnExecute  := actExecuteScriptOnSelectionExecute;
        end;
      end;
    end
    else
    begin
      raise Exception.CreateFmt(
        'Compilation failed with:'#13#10,
        [FdwsProgram.Msgs.AsInfo]
      );
    end;
  end;
end;

procedure TdmEditorManager.BuildEditorPopupMenu;
var
  MI : TMenuItem;
begin
  MI := EditorPopupMenu.Items;
  MI.Clear;
  AddMenuItem(MI, actCut);
  AddMenuItem(MI, actCopy);
  AddMenuItem(MI, actPaste);
  AddMenuItem(MI);
  AddMenuItem(MI, actUndo);
  AddMenuItem(MI, actRedo);
  AddMenuItem(MI);
  AddMenuItem(MI, FilePopupMenu);
  AddMenuItem(MI, SettingsPopupMenu);
  AddMenuItem(MI, SearchPopupMenu);
  AddMenuItem(MI, SelectPopupMenu);
  AddMenuItem(MI, SelectionPopupMenu);
  AddMenuItem(MI, InsertPopupMenu);
  AddMenuItem(MI, ClipboardPopupMenu);
  AddMenuItem(MI, ExportPopupMenu);
  AddMenuItem(MI, HighlighterPopupMenu);
  AddMenuItem(MI, FoldPopupMenu);
  //AddMenuItem(MI, EncodingPopupMenu);
  AddMenuItem(MI);
  AddMenuItem(MI, actFilterCode);
  AddMenuItem(MI, actShapeCode);
  AddMenuItem(MI, actFormat);
  AddMenuItem(MI);
  AddMenuItem(MI, actClose);
  AddMenuItem(MI, actCloseOthers);
  AddMenuItem(MI, actShowHTMLViewer);
  AddMenuItem(MI, actShowHexEditor);
end;
{$endregion}
{$endregion}

{$region 'protected methods' /fold}
{ Called when the active view is set to another view in the list. }
procedure TdmEditorManager.ActiveViewChanged;
begin
  UpdateHighLighterActions;
  UpdateEncodingActions;
  UpdateLineBreakStyleActions;
  UpdateFileActions;
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
//  V.AssignHighlighter(AHighlighter);
  V.Form.Caption := '';
  ViewList.Add(V);
  Events.DoAddEditorView(V);
  V.Activate;
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
    Events.DoAddEditorView(V);
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
    I := ViewList.IndexOf(ActiveView);
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
    if (ViewCount > 1) and (I > -1) then
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
    begin
      Result := False;
    end;
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
{$endregion}

procedure TdmEditorManager.ShowToolView(const AName: string; AVisible: Boolean;
  AShowModal: Boolean; ASetFocus: Boolean);
var
  ETV : IEditorToolView;
begin
  if not AShowModal then
    ToolViews.Hide;
  if AVisible then
  begin
    ETV := ToolViews[AName];
    if not AShowModal then
    begin
      { Allow owner to dock the toolview in the main application workspace. }
      Events.DoShowToolView(ETV);
      ETV.Visible := True;
    end
    else
    begin
      ETV.Form.ShowModal;
    end;
    ETV.UpdateView;
    if ASetFocus then
      ETV.SetFocus;
  end
  else
    Events.DoHideToolView(ETV);
end;

{$region 'IEditorCommands' /fold}
procedure TdmEditorManager.AssignHighlighter(const AName: string);
var
  HLI : THighlighterItem;
begin
  if Assigned(Highlighters) then
  begin
    HLI := Highlighters.ItemsByName[AName];
    if Assigned(HLI) then
      ActiveView.HighlighterItem := HLI;
  end;
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
  Events.DoSave(AFileName);
  if AShowDialog or not FileExistsUTF8(AFileName) then
  begin
    if Assigned(ActiveView.Editor.Highlighter) then
      dlgSave.Filter := ActiveView.Editor.Highlighter.DefaultFilter;
    dlgSave.FileName := AFileName;
    if dlgSave.Execute then
    begin
      ActiveView.FileName := dlgSave.FileName;
      ActiveView.Save(dlgSave.FileName);
      Result := True;
    end
    else
      Result := False;
  end
  else
  begin
    ActiveView.FileName := AFileName;
    ActiveView.Save(AFileName);
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

procedure TdmEditorManager.OpenFileAtCursor;
var
  FN : string;
begin
  FN := ExtractFilePath(ActiveView.FileName)
    + ActiveView.CurrentWord + ExtractFileExt(ActiveView.FileName);
  if FileExistsUTF8(FN) then
    Events.DoNew(FN);
end;

{$IFDEF Windows}
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
{$ENDIF}

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
  if Assigned(V) and Assigned(Settings) and V.Focused {and FChanged} then
  begin
    B := V.SelAvail and not Settings.ReadOnly;
    actDequoteSelection.Enabled               := B;
    actLowerCaseSelection.Enabled             := B;
    actToggleBlockCommentSelection.Enabled    := B;
    actPascalStringOfSelection.Enabled        := B;
    actStripMarkup.Enabled                    := B;
    actQuoteSelection.Enabled                 := B;
    actQuoteLinesAndDelimit.Enabled           := B;
    actSortSelection.Enabled                  := B;
    actUpperCaseSelection.Enabled             := B;
    actStripFirstChar.Enabled                 := B;
    actStripLastChar.Enabled                  := B;
    actQuoteLines.Enabled                     := B;
    actDequoteLines.Enabled                   := B;
    actEncodeBase64.Enabled                   := B;
    actDecodeBase64.Enabled                   := B;
    actConvertTabsToSpacesInSelection.Enabled := B;
    actSyncEdit.Enabled                       := B;

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

    actRedo.Enabled := B and V.CanRedo;
    actUndo.Enabled := B and V.CanUndo;

    B := ActiveView.Focused;
    actCut.Enabled   := actCut.Visible and B;
    actCopy.Enabled  := actCopy.Visible and B;
    actPaste.Enabled := actPaste.Visible and ActiveView.CanPaste and B;

    if ToolViews.Count > 0 then
    begin
      actInsertCharacterFromMap.Checked := ToolViews['CharacterMap'].Visible;
      actSearch.Checked                 := ToolViews['Search'].Visible;
      actShapeCode.Checked              := ToolViews['CodeShaper'].Visible;
      actAlignSelection.Checked         := ToolViews['AlignLines'].Visible;
      actShowPreview.Checked            := ToolViews['Preview'].Visible;
      actFilterCode.Checked             := ToolViews['CodeFilter'].Visible;
      actShowViews.Checked              := ToolViews['ViewList'].Visible;
      actShowActions.Checked            := ToolViews['ActionListView'].Visible;
      actShowHexEditor.Checked          := ToolViews['HexEditor'].Visible;
      actShowMiniMap.Checked            := ToolViews['MiniMap'].Visible;
      actShowHTMLViewer.Checked         := ToolViews['HTMLView'].Visible;
    end;

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
    actShowSpecialCharacters.Checked := Settings.ShowSpecialCharacters;

    actSave.Enabled := ActiveView.Modified;

    actClose.Visible       := ViewCount > 1;
    actCloseOthers.Visible := ViewCount > 1;

    actToggleMaximized.Checked :=
      Settings.FormSettings.WindowState = wsMaximized;
    actStayOnTop.Checked := Settings.FormSettings.FormStyle = fsSystemStayOnTop;
    actSingleInstance.Checked := Settings.SingleInstance;

    FChanged := False;
  end;
end;

procedure TdmEditorManager.UpdateEncodingActions;
var
  S: string;
  A: TCustomAction;
begin
  S := '';
  if Assigned(ActiveView) then
  begin
    S := actEncodingMenu.Name + DelChars(ActiveView.Encoding, '-');
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
    S := actLineBreakStyleMenu.Name + ActiveView.LineBreakStyle;
    A := Items[S];
    if Assigned(A) then
      A.Checked := True;
  end;
end;

procedure TdmEditorManager.UpdateSelectionModeActions;
var
  S : string;
  A : TCustomAction;
begin
  S := '';
  if Assigned(ActiveView) then
  begin
    S := GetEnumName(TypeInfo(TSynSelectionMode), Ord(ActiveView.SelectionMode));
    S := System.Copy(S, 3, Length(S));
    S := actSelectionModeMenu.Name + S;
    A := Items[S];
    if Assigned(A) then
      A.Checked := True;
  end;
end;

procedure TdmEditorManager.UpdateFileActions;
var
  B : Boolean;
begin
  B := FileExistsUTF8(ActiveView.FileName);
  actCreateDesktopLink.Enabled := B;
  actCopyFileName.Enabled   := B;
  actCopyFilePath.Enabled   := B;
  actCopyFullPath.Enabled   := B;
  actReload.Enabled         := B;
  actMonitorChanges.Enabled := B;
  actMonitorChanges.Checked := B and ActiveView.MonitorChanges;
end;

{ TODO: causes problems for an unknown reason when no highlighters are
  registered. }

procedure TdmEditorManager.UpdateHighLighterActions;
var
  S : string;
  A : TCustomAction;
begin
  S := '';
  if Assigned(ActiveView) and Assigned(ActiveView.HighlighterItem)
    and (ActiveView.HighlighterItem.Name <> 'None') then
  begin
    S := actHighlighter.Name + ActiveView.HighlighterItem.Name;
    A := Items[S];
    if Assigned(A) then
      A.Checked := True;
  end;
end;

procedure TdmEditorManager.ClearHighlightSearch;
var
  V : IInterface;
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
  Events.DoLoad(AFileName);
  { Check if the file is already opened in a view. }
  V := ViewByFileName[AFileName];
  if Assigned(V) then
    Result := V
  else
  begin
    if FileExistsUTF8(AFileName) then
    begin
      V := AddView('', AFileName);
      V.Load(AFileName);
    end;
    Result := V;
  end;
end;

function TdmEditorManager.NewFile(const AFileName: string;
  const AText: string): IEditorView;
var
  V : IEditorView;
begin
  Events.DoNew(AFileName, AText);
  V := AddView('', AFileName);
  V.Text := AText;
  Result := V;
end;
{$endregion}

{$region 'Find' /fold}
procedure TdmEditorManager.FindPrevious;
begin
  SearchEngine.FindPrevious;
end;

procedure TdmEditorManager.FindNext;
begin
  SearchEngine.FindNext;
end;
{$endregion}
{$endregion}

initialization
{$IFDEF Windows}
  Logger.Channels.Add(TIPCChannel.Create);
{$ENDIF}

end.