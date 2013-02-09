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

unit ts_Editor_Interfaces;

{$mode delphi}

//*****************************************************************************

interface

uses
  Classes, ActnList, Controls, Forms, Menus, Contnrs, Graphics,

  LCLType,

  SynEdit, SynEditTypes, SynMacroRecorder, SynEditHighlighter,

  ts_Core_FormSettings,

  ts_Editor_Settings_AlignLines, ts_Editor_Settings_SearchEngine,
  ts_Editor_Settings_CodeShaper,

  ts_Editor_Highlighters, ts_Editor_HighlighterAttributes;

{
  TODO:
    - Highlighter assignment (EditorView or EditorActions?)
    - non working shortcuts
         - F3 : find next
         - CTRL-'+' and CTRL-'-' to increase/decrease font size
    - Active highlighter visible in menus
}

//=============================================================================

type
  TCaretPositionEvent = procedure(
    Sender : TObject;
    X, Y   : Integer
  ) of object;

  TStatusMessageEvent = procedure(
    Sender : TObject;
    Text   : string
  ) of object;

  TMacroStateChangeEvent = procedure(
    Sender: TObject;
    AState: TSynMacroState
  ) of object;

  TFileEvent = procedure(
        Sender    : TObject;
    var AFileName : string
  ) of object;

  TNewFileEvent = procedure(
          Sender    : TObject;
    var   AFileName : string;
    const AText     : string
  ) of object;

  TOnFilteredLineChangeEvent = procedure(
          Sender  : TObject;
          AIndex  : Integer;
    const ALine   : string;
    const AFilter : string
  ) of object;

type
  TEditorViewList     = TInterfaceList;
  TEditorToolViewList = TInterfaceList;

type
  IEditorActions = interface;
  IEditorSettings = interface;
  TEditorViewListEnumerator = class;

  { Handles display view of the editor. }

  IEditorView = interface
  ['{94689213-B046-45F6-922B-FAE91C02A3FF}']
    {$region 'property access methods' /fold}
    function GetActions: IEditorActions;
    function GetBlockBegin: TPoint;
    function GetBlockEnd: TPoint;
    function GetCanPaste: Boolean;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetCaretX: Integer;
    function GetCaretXY: TPoint;
    function GetCaretY: Integer;
    function GetCurrentWord: string;
    function GetEditor: TSynEdit;
    function GetEditorFont: TFont;
    function GetEncoding: string;
    function GetFileName: string;
    function GetFindHistory: TStrings;
    function GetFoldLevel: Integer;
    function GetFoldState: string;
    function GetForm: TCustomForm;
    function GetHighlighterItem: THighlighterItem;
    function GetLineBreakStyle: string;
    function GetLines: TStrings;
    function GetLinesInWindow: Integer;
    function GetLineText: string;
    function GetLogicalCaretXY: TPoint;
    function GetModified: Boolean;
    function GetMonitorChanges: Boolean;
    function GetName: string;
    function GetOnChange: TNotifyEvent;
    function GetOnDropFiles: TDropFilesEvent;
    function GetOnStatusChange: TStatusChangeEvent;
    function GetParent: TWinControl;
    function GetPopupMenu: TPopupMenu;
    function GetPreviewText: string;
    function GetReplaceHistory: TStrings;
    function GetSearchOptions: TSynSearchOptions;
    function GetSearchText: string;
    function GetSelAvail: Boolean;
    function GetSelEnd: Integer;
    function GetSelStart: Integer;
    function GetSelText: string;
    function GetSettings: IEditorSettings;
    function GetShowSpecialChars: Boolean;
    function GetSupportsFolding: Boolean;
    function GetText: string;
    function GetTextSize: Integer;
    function GetTopLine: Integer;
    procedure SetBlockBegin(const AValue: TPoint);
    procedure SetBlockEnd(const AValue: TPoint);
    procedure SetCaretX(const AValue: Integer);
    procedure SetCaretXY(const AValue: TPoint);
    procedure SetCaretY(const AValue: Integer);
    procedure SetEditorFont(AValue: TFont);
    procedure SetEncoding(const AValue: string);
    procedure SetFileName(const AValue: string);
    procedure SetFoldLevel(const AValue: Integer);
    procedure SetFoldState(const AValue: string);
    procedure SetHighlighterItem(const AValue: THighlighterItem);
    procedure SetLineBreakStyle(const AValue: string);
    procedure SetLines(const AValue: TStrings);
    procedure SetLineText(const AValue: string);
    procedure SetLogicalCaretXY(const AValue: TPoint);
    procedure SetModified(const AValue: Boolean);
    procedure SetMonitorChanges(const AValue: Boolean);
    procedure SetName(const AValue: string);
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetOnDropFiles(const AValue: TDropFilesEvent);
    procedure SetOnStatusChange(const AValue: TStatusChangeEvent);
    procedure SetParent(const AValue: TWinControl);
    procedure SetPopupMenu(const AValue: TPopupMenu);
    procedure SetSearchOptions(AValue: TSynSearchOptions);
    procedure SetSearchText(const AValue: string);
    procedure SetSelEnd(const AValue: Integer);
    procedure SetSelStart(const AValue: Integer);
    procedure SetSelText(const AValue: string);
    procedure SetShowSpecialChars(const AValue: Boolean);
    procedure SetText(const AValue: string);
    procedure SetTopLine(const AValue: Integer);
    {$endregion}

    // information retrieval
    function Focused: Boolean;
    function GetWordAtPosition(APosition: TPoint): string;
    function GetWordFromCaret(const ACaretPos: TPoint): string;
    function GetHighlighterAttriAtRowCol(
          APosition : TPoint;
      out AToken    : string;
      out AAttri    : TSynHighlighterAttributes
    ): Boolean;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure StoreBlock;
    procedure RestoreBlock;

    procedure Activate;

    // configuration
    procedure AssignHighlighter(const AHighlighter: string = 'TXT');

    procedure SetHighlightSearch(
      const ASearch  : string;
            AOptions : TSynSearchOptions
    );

    // commands
    procedure Clear;
    procedure AdjustFontSize(AOffset: Integer);
    procedure SetFocus;
    procedure UpdateCommentSelection(ACommentOn, AToggle: Boolean);
    procedure ToggleBlockCommentSelection;
    procedure InsertTextAtCaret(const AText: string);
    procedure Close;
    procedure AlignSelection(
      const AToken                  : string;
            ACompressWS             : Boolean;
            AInsertSpaceBeforeToken : Boolean;
            AInsertSpaceAfterToken  : Boolean;
            AAlignInParagraphs      : Boolean
    );
    procedure StripMarkupFromSelection;
    procedure StripCharsFromSelection(
      AFirst : Boolean;
      ALast  : Boolean
    );

    // search
    procedure SearchAndSelectLine(ALineIndex: Integer; const ALine: string);
    procedure SearchAndSelectText(const AText: string);
    procedure ClearHighlightSearch;

    // load and save
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    procedure SaveToFile(const AFileName: string);

    // selection
    procedure SelectAll;
    procedure SelectWord;
    procedure UpperCaseSelection;
    procedure LowerCaseSelection;
    procedure SmartSelect;
    function SelectBlockAroundCursor(const AStartTag, AEndTag: string;
      AIncludeStartTag, AIncludeEndTag: Boolean): Boolean;

    // clipboard commands
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure Undo;
    procedure Redo;

    // properties
    property FindHistory: TStrings
      read GetFindHistory;

    property ReplaceHistory: TStrings
      read GetReplaceHistory;

    property Editor: TSynEdit
      read GetEditor;

    property Form: TCustomForm
      read GetForm;

    { Settings shared by all edit instances managed by IEditorActions instance. }
    property Settings: IEditorSettings
      read GetSettings;

    { Reference to the main 'manager' instance }
    property Actions: IEditorActions
      read GetActions;

    // Selection properties
    property BlockBegin: TPoint
      read GetBlockBegin write SetBlockBegin;

    property BlockEnd: TPoint
      read GetBlockEnd write SetBlockEnd;

    property SelText: string
      read GetSelText write SetSelText;

    property SelStart: Integer
      read GetSelStart write SetSelStart;

    property SelEnd: Integer
      read GetSelEnd write SetSelEnd;

    property SelAvail: Boolean
      read GetSelAvail;

    property SearchText: string
      read GetSearchText write SetSearchText;

    property SearchOptions: TSynSearchOptions
      read GetSearchOptions write SetSearchOptions;

    // Status information (readonly)
    property CanPaste: Boolean
      read GetCanPaste;

    property CanRedo: Boolean
      read GetCanRedo;

    property CanUndo: Boolean
      read GetCanUndo;

    property TextSize: Integer
      read GetTextSize;

    property LinesInWindow: Integer
      read GetLinesInWindow;

    property SupportsFolding: Boolean
      read GetSupportsFolding;

    property CurrentWord: string
      read GetCurrentWord;
    //------------------------------------------
    property Lines: TStrings
      read GetLines write SetLines;

    property Text: string
      read GetText write SetText;
    //------------------------------------------

    // TODO: Append? (for logging)


    //---| Cursor properties |-------------------------------------------------
    { current X-coordinate of the caret. }
    property CaretX: Integer
      read GetCaretX write SetCaretX;

    { current Y-coordinate of the caret. }
    property CaretY: Integer
      read GetCaretY write SetCaretY;

    { TPoint representation of current caret position }
    property CaretXY: TPoint
      read GetCaretXY write SetCaretXY;

    property LogicalCaretXY: TPoint
      read GetLogicalCaretXY write SetLogicalCaretXY;

    //---| Folding support |---------------------------------------------------
    { String identifying the foldstate of current instance }
    property FoldState: string
      read GetFoldState write SetFoldState;

    property FoldLevel: Integer
      read GetFoldLevel write SetFoldLevel;

    property FileName: string
      read GetFileName write SetFileName;

    property LineText: string
      read GetLineText write SetLineText;

    property Parent: TWinControl
      read GetParent write SetParent;

    property PreviewText: string
      read GetPreviewText;

    //---| Properties |--------------------------------------------------------
    property MonitorChanges: Boolean
      read GetMonitorChanges write SetMonitorChanges;

    property ShowSpecialChars: Boolean
      read GetShowSpecialChars write SetShowSpecialChars;

    property TopLine: Integer
      read GetTopLine write SetTopLine;

    property Modified: Boolean
      read GetModified write SetModified;

    property Encoding: string
      read GetEncoding write SetEncoding;

    property LineBreakStyle: string
      read GetLineBreakStyle write SetLineBreakStyle;

    property Name: string
      read GetName write SetName;

    property PopupMenu: TPopupMenu
      read GetPopupMenu write SetPopupMenu;

    property HighlighterItem: THighlighterItem
      read GetHighlighterItem write SetHighlighterItem;

    property EditorFont: TFont
      read GetEditorFont write SetEditorFont;

    // events
    property OnDropFiles: TDropFilesEvent
      read GetOnDropFiles write SetOnDropFiles;

    property OnStatusChange: TStatusChangeEvent
      read GetOnStatusChange write SetOnStatusChange;

    property OnChange: TNotifyEvent
      read GetOnChange write SetOnChange;
  end;

  IEditorSearchEngine = interface
  ['{5403336C-3E81-4A1B-B2BB-170CF0EF0B84}']
    function GetCurrentIndex: Integer;
    function GetItemList: TObjectList;
    function GetOptions: TSynSearchOptions;
    function GetReplaceText: string;
    function GetSearchAllViews: Boolean;
    function GetSearchText: string;
    procedure SetCurrentIndex(AValue: Integer);
    procedure SetOptions(AValue: TSynSearchOptions);
    procedure SetReplaceText(AValue: string);
    procedure SetSearchAllViews(AValue: Boolean);
    procedure SetSearchText(AValue: string);
    procedure Execute;
    procedure Replace;
    procedure ReplaceAll;
    procedure FindNext;
    procedure FindPrevious;

    property CurrentIndex: Integer
      read GetCurrentIndex write SetCurrentIndex;

    property Options: TSynSearchOptions
      read GetOptions write SetOptions;

    property SearchText : string
      read GetSearchText write SetSearchText;

    property ReplaceText: string
      read GetReplaceText write SetReplaceText;

    property SearchAllViews: Boolean
      read GetSearchAllViews write SetSearchAllViews;

    property ItemList: TObjectList
      read GetItemList;
  end;

  IEditorEvents = interface
  ['{D078C92D-16DF-4727-A18F-4C76E07D37A2}']
    // property access methods
    procedure SetOnMacroStateChange(const AValue: TMacroStateChangeEvent);
    function GetOnMacroStateChange: TMacroStateChangeEvent;
    function GetOnCaretPositionChange: TCaretPositionEvent;
    function GetOnChange: TNotifyEvent;
    function GetOnNewFile: TNewFileEvent;
    function GetOnOpenFile: TFileEvent;
    function GetOnSaveFile: TFileEvent;
    function GetOnStatusChange: TStatusChangeEvent;
    procedure SetOnCaretPositionChange(const AValue: TCaretPositionEvent);
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetOnNewFile(const AValue: TNewFileEvent);
    procedure SetOnOpenFile(const AValue: TFileEvent);
    procedure SetOnSaveFile(const AValue: TFileEvent);
    procedure SetOnStatusChange(const AValue: TStatusChangeEvent);

    // event dispatch methods
    procedure DoCaretPositionChange;
    procedure DoMacroStateChange(AState : TSynMacroState);
    procedure DoStatusMessage(AText: string);
    procedure DoStatusChange(AChanges: TSynStatusChanges);
    procedure DoChange;
    procedure DoModified;
    procedure DoSaveFile;
    procedure DoOpenFile;
    procedure DoNewFile(const AFileName: string = ''; const AText: string = '');

    // events
    { triggered when caret position changes }
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
  end;

  { Settings we should store if we would like to restore the editor to its
    current state }

  IEditorSettings = interface
  ['{CDB18A45-54AA-49F2-82C7-15D68C952197}']
    {$region 'property access methods' /fold}
    function GetAlignLinesSettings: TAlignLinesSettings;
    function GetAutoFormatXML: Boolean;
    function GetAutoGuessHighlighterType: Boolean;
    function GetCloseWithESC: Boolean;
    function GetCodeShaperSettings: TCodeShaperSettings;
    function GetDebugMode: Boolean;
    function GetDimInactiveView: Boolean;
    function GetEditorFont: TFont;
    function GetFileName: string;
    function GetFormSettings: TFormSettings;
    function GetHighlighterAttributes: THighlighterAttributes;
    function GetHighlighters: THighlighters;
    function GetHighlighterType: string;
    function GetPreviewVisible: Boolean;
    function GetReadOnly: Boolean;
    function GetSearchEngineSettings: TSearchEngineSettings;
    function GetShowControlCharacters: Boolean;
    function GetXML: string;
    procedure SetAlignLinesSettings(AValue: TAlignLinesSettings);
    procedure SetAutoFormatXML(const AValue: Boolean);
    procedure SetAutoGuessHighlighterType(const AValue: Boolean);
    procedure SetCloseWithESC(const AValue: Boolean);
    procedure SetCodeShaperSettings(AValue: TCodeShaperSettings);
    procedure SetDebugMode(AValue: Boolean);
    procedure SetDimInactiveView(const AValue: Boolean);
    procedure SetEditorFont(AValue: TFont);
    procedure SetFileName(const AValue: string);
    procedure SetFormSettings(const AValue: TFormSettings);
    procedure SetHighlighterAttributes(AValue: THighlighterAttributes);
    procedure SetHighlighterType(const AValue: string);
    procedure SetPreviewVisible(const AValue: Boolean);
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetSearchEngineSettings(AValue: TSearchEngineSettings);
    procedure SetShowControlCharacters(const AValue: Boolean);
    {$endregion}

    procedure Load;
    procedure Save;
    procedure Apply;

    procedure AddEditorSettingsChangedHandler(AEvent: TNotifyEvent);
    procedure RemoveEditorSettingsChangedHandler(AEvent: TNotifyEvent);

    property FileName: string
      read GetFileName write SetFileName;

    property EditorFont: TFont
      read GetEditorFont write SetEditorFont;

    property DimInactiveView: Boolean
      read GetDimInactiveView write SetDimInactiveView;

    property HighlighterType: string
      read GetHighlighterType write SetHighlighterType;

    property ReadOnly: Boolean
      read GetReadOnly write SetReadOnly;

    property PreviewVisible: Boolean
      read GetPreviewVisible write SetPreviewVisible;

    property ShowControlCharacters: Boolean
      read GetShowControlCharacters write SetShowControlCharacters;

    property AutoFormatXML: Boolean
      read GetAutoFormatXML write SetAutoFormatXML;

    property CloseWithESC: Boolean
      read GetCloseWithESC write SetCloseWithESC;

    property AutoGuessHighlighterType: Boolean
      read GetAutoGuessHighlighterType write SetAutoGuessHighlighterType;

    property Highlighters: THighlighters
      read GetHighlighters;

    property FormSettings: TFormSettings
      read GetFormSettings write SetFormSettings;

    property AlignLinesSettings: TAlignLinesSettings
      read GetAlignLinesSettings write SetAlignLinesSettings;

    property HighlighterAttributes: THighlighterAttributes
      read GetHighlighterAttributes write SetHighlighterAttributes;

    property SearchEngineSettings: TSearchEngineSettings
      read GetSearchEngineSettings write SetSearchEngineSettings;

    property CodeShaperSettings: TCodeShaperSettings
      read GetCodeShaperSettings write SetCodeShaperSettings;

    property DebugMode: Boolean
      read GetDebugMode write SetDebugMode;

    property XML: string
      read GetXML;
  end;

  // todo enumerator support
  IEditorViews = interface
  ['{FBFB8DC6-7663-4EA4-935D-5B9F3CD7C753}']
    function GetView(AIndex: Integer): IEditorView;
    function GetViewByName(AName: string): IEditorView;
    function GetCount: Integer;
    function GetViewList: TEditorViewList;

    function Add(
      const AName        : string = '';
      const AFileName    : string = '';
      const AHighlighter : string = ''
    ): IEditorView;
    function Delete(AIndex: Integer): Boolean; overload;
    function Delete(AView: IEditorView): Boolean; overload;
    function Delete(const AName: string): Boolean; overload;
    procedure Clear(AExceptActive: Boolean = False);

    function GetEnumerator: TEditorViewListEnumerator;

    property Views[AIndex: Integer]: IEditorView
      read GetView; default;

    property ViewByName[AName: string]: IEditorView
      read GetViewByName;

    property ViewList: TEditorViewList
      read GetViewList;

    property Count: Integer
      read GetCount;
  end;

  IEditorToolView = interface
  ['{F6BEE8F6-BA4D-4B38-8FB0-79088B615DF5}']
    function GetForm: TForm;
    function GetName:string;
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    { Lets the view respond to changes. }
    procedure UpdateView;
    procedure SetFocus;
    function Focused: Boolean;

    property Visible: Boolean
      read GetVisible write SetVisible;

    property Name: string
      read GetName;

    property Form: TForm
      read GetForm;
  end;

  IEditorToolViews = interface
  ['{A5575878-A189-4F3C-9008-61899B739DA1}']
    function GetView(AIndex: Integer): IEditorToolView;
    function GetViewByName(AName: string): IEditorToolView;
    function GetCount: Integer;
    function GetViewList: TEditorToolViewList;

    procedure Add(AToolView: IEditorToolView);
    function Delete(AIndex: Integer): Boolean; overload;
    function Delete(AView: IEditorToolView): Boolean; overload;
    function Delete(const AName: string): Boolean; overload;

    property Views[AIndex: Integer]: IEditorToolView
      read GetView;

    property ViewByName[AName: string]: IEditorToolView
      read GetViewByName; default;

    property ViewList: TEditorToolViewList
      read GetViewList;

    property Count: Integer
      read GetCount;
  end;

  IEditorCommands = interface
  ['{5CB77731-425D-44FD-93BA-2137875F76B5}']
    function SaveFile(const AFileName: string = ''): Boolean;
    procedure LoadFile;
    procedure OpenFileAtCursor;
    procedure ToggleHighlighter;
    procedure CreateDesktopLink;
    procedure InsertCharacter(const C: TUTF8Char);
    procedure AssignHighlighter(const AName: string);
    procedure CopyToClipboard;
    //procedure SortSelection;
  end;

  IClipboardCommands = interface
  ['{E8E71973-7048-4238-93DD-CB6307CD1BA8}']
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure Undo;
    procedure Redo;
  end;

  IEditorMenus = interface
  ['{4B6F6B6A-8A72-478B-B3AF-089E72E23CDF}']
    function GetEditorPopupMenu: TPopupMenu;
    function GetEncodingPopupMenu: TPopupMenu;
    function GetExportPopupMenu: TPopupMenu;
    function GetFoldPopupMenu: TPopupMenu;
    function GetHighlighterPopupMenu: TPopupMenu;
    function GetLineBreakStylePopupMenu: TPopupMenu;
    property EditorPopupMenu: TPopupMenu
      read GetEditorPopupMenu;

    property HighlighterPopupMenu: TPopupMenu
      read GetHighlighterPopupMenu;

    property FoldPopupMenu: TPopupMenu
      read GetFoldPopupMenu;

    property ExportPopupMenu: TPopupMenu
      read GetExportPopupMenu;

    property EncodingPopupMenu: TPopupMenu
      read GetEncodingPopupMenu;

    property LineBreakStylePopupMenu: TPopupMenu
      read GetLineBreakStylePopupMenu;
  end;

  { All supported actions by the editor views. }

  IEditorActions = interface
  ['{E42EF2E3-A7A0-4847-B299-3C35699DC708}']
    function GetActionList: TActionList;
    function GetItem(AName: string): TCustomAction;

    procedure UpdateActions;
    procedure UpdateHighLighterActions;

    property Items[AName: string]: TCustomAction
      read GetItem; default;

    property ActionList: TActionList
      read GetActionList;
  end;

  IEditorManager = interface
  ['{631A126F-1693-4E25-B691-CD2487BCB820}']
    function GetActions: IEditorActions;
    function GetCommands: IEditorCommands;
    function GetEvents: IEditorEvents;
    function GetMenus: IEditorMenus;
    function GetSearchEngine: IEditorSearchEngine;
    function GetSettings: IEditorSettings;
    function GetToolViews: IEditorToolViews;
    function GetViews: IEditorViews;
    function GetPersistSettings: Boolean;
    procedure SetPersistSettings(const AValue: Boolean);
    function GetActiveView: IEditorView;
    procedure SetActiveView(AValue: IEditorView);
    procedure SetOnActiveViewChange(const AValue: TNotifyEvent);
    function GetOnActiveViewChange: TNotifyEvent;
    function GetHighlighters: THighlighters;

    function ActivateView(const AName: string): Boolean;
    procedure ClearHighlightSearch;

    property PersistSettings: Boolean
      read GetPersistSettings write SetPersistSettings;

    property Highlighters: THighlighters
      read GetHighlighters;

    property ActiveView: IEditorView
      read GetActiveView write SetActiveView;

    property OnActiveViewChange: TNotifyEvent
      read GetOnActiveViewChange write SetOnActiveViewChange;

    property Events: IEditorEvents
      read GetEvents;

    property Views: IEditorViews
      read GetViews;

    property ToolViews: IEditorToolViews
      read GetToolViews;

    property Menus: IEditorMenus
      read GetMenus;

    property Actions: IEditorActions
      read GetActions;

    property Commands: IEditorCommands
      read GetCommands;

    property Settings: IEditorSettings
      read GetSettings;

    property SearchEngine: IEditorSearchEngine
      read GetSearchEngine;
  end;

//-----------------------------------------------------------------------------

  IEditorCodeFilter = interface
  ['{CF1E061E-89FD-47F0-91DF-C26CC457BC08}']
    function GetOnFilteredLineChange: TOnFilteredLineChangeEvent;
    procedure SetOnFilteredLineChange(AValue: TOnFilteredLineChangeEvent);
    property OnFilteredLineChange: TOnFilteredLineChangeEvent
      read GetOnFilteredLineChange write SetOnFilteredLineChange;
  end;

  TEditorViewListEnumerator = class
  strict private
    FIndex: Integer;
    FList : TEditorViewList;

  public
    constructor Create(AList: TEditorViewList);
    function GetCurrent: IEditorView;
    function MoveNext: Boolean;
    property Current: IEditorView
      read GetCurrent;
  end;



//*****************************************************************************

implementation

{$region 'TEditorViewListEnumerator' /fold}
constructor TEditorViewListEnumerator.Create(AList: TEditorViewList);
begin
  FList := AList;
  FIndex := -1;
end;

function TEditorViewListEnumerator.GetCurrent: IEditorView;
begin
  Result := FList[FIndex] as IEditorView;
end;

function TEditorViewListEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < (FList.Count - 1);
  if Result then
    Inc(FIndex);
end;
{$endregion}

end.

