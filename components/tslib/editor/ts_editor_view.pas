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

unit ts_Editor_View;

{$mode delphi}

{$region 'documentation' /fold}
{
Form holding a complete customizable text editor based on the open source
SynEdit components.
Technical details:
  - The owner is always a IEditorManager instance

Features:
  - accepts dropped files
  - auto detect file encoding
  - dynamic editor creation
  - dynamic highlighter creation and registration
  - search and replace capability with support for regular expressions
  - export capability to RTF, HTML, TEX or ASCII text format
  - synchronized edit
  - highlight selected text
  - code folding
  - file monitor function to watch for external file changes.
  - preview shows wrapped text of current line or selected text
  - code shaper
  - code filter

  26/09/2009
   - Support for Lazarus (initial version)

TODO:
  STRING MANIPULATION
  - Compress Whitespace
  - Strip first/last character + (non blank)
  - Strip trailing Blanks
  - Remove Blank Lines (preserve max one blank?)
  - Modify lines (prefix and suffix for each line)
  - Split lines/join lines / join paragraph

  USER INTERFACE ACTIONS
  - macrorecorder
  - regular expressions
  - template editor
  - configurable page setup and printing with preview
  - quickbuttons (like the Delphi version had)
  - URI opener, to open hyperlinks directly from the editor
  - configurable syntax colouring (Lettterpress alike or XML?)
  - customizable keystroke-function mappings
  - configurable code completion proposal
  - make the editor available as a component
  - documentation (in RTF)
  - convert to another encoding (partially implemented)
  - find a way to fold particular sections (now only levels are supported)

  CODE STRUCTURE
  - make TEditorView more suitable to inherit from
  - send to mail action

  KNOWN ISSUES:
  - When created at runtime, the cleanup of the TSynEdit instance is magnitudes
    faster than using a design-time instance. Therefor we rely on a manually
    created instance.
    This way it is easier to adapt to changes in the SynEdit component.

  DEPENDENCIES:
  - SynEdit
  - ts_Core_DirectoryWatch: do react on modifications.

  TODO
    - commands executed on selections should be factored out. They all have in
      common that undo information needs to be managed by the TSynEdit instance
}
{$endregion}

//*****************************************************************************

interface

uses
  Classes, Controls, Forms, Graphics, Menus, SysUtils, Windows, Dialogs,
  StdCtrls, Types,

  LMessages,

  SynEdit, SynEditHighlighter, SynPluginSyncroEdit, SynPluginTemplateEdit,
  SynEditMarkupHighAll, SynEditTypes, SynBeautifier,
  SynEditMarkupBracket, SynEditMarkupSelection, SynEditHighlighterFoldBase,

  ts_Core_DirectoryWatch,

  ts_Editor_Resources, ts_Editor_Highlighters, ts_Editor_Interfaces,

  // logging
  sharedloggerlcl;

type
  TEditorView = class(TForm, IEditorView)
  published
    imlBookmarkImages: TImageList;

    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormShortCut(var Msg: TLMKey; var Handled: Boolean);

  private
    // SynEdit event handlers
    procedure EditorChange(Sender: TObject);
    procedure EditorClickLink(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditorPaste(Sender: TObject; var AText: string;
      var AMode: TSynSelectionMode; ALogStartPos: TPoint;
      var AnAction: TSynCopyPasteAction);
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);

    procedure DirectoryWatchNotify(const Sender: TObject;
      const AAction: TWatchAction; const FileName: string);

    function IsActive: Boolean;

  private
    FUpdate               : Boolean;
    FDirectoryWatch       : TDirectoryWatch;
    FEncoding             : string;
    FLineBreakStyle       : string;
    FEditor               : TSynEdit;
    FFindHistory          : TStringList;
    FHighlighter          : TSynCustomHighlighter;
    FReplaceHistory       : TStringList;
    FSyncronizedEdit      : TSynPluginSyncroEdit;
    FTemplateEdit         : TSynPluginTemplateEdit;
    FHighlighterItem      : THighlighterItem;
    FCodeCompletionFormat : string;
    FCompletionInsertList : TStrings;
    FCompletionItemList   : TStrings;
    FMHAC                 : TSynEditMarkupHighlightAllCaret;
    //FMHA                  : TSynEditMarkupHighlightAll;
    //FMS                   : TSynEditMarkupSelection;
    FFileName             : string;
    FFoldLevel            : Integer;
    FBeautifier           : TSynBeautifier;
    FOnDropFiles          : TDropFilesEvent;
    FOnStatusChange       : TStatusChangeEvent;
    FOnChange             : TNotifyEvent;

    FStoredBlockBegin     : TPoint;
    FStoredBlockEnd       : TPoint;
    FStoredSelectionMode  : TSynSelectionMode;

    { search settings }
    FFileFilterList : TStringList;
    FSearchText     : string;
    FSearchOptions  : TSynSearchOptions;

    {$region 'property access methods' /fold}
    function GetActions: IEditorActions;
    function GetBlockBegin: TPoint;
    function GetBlockEnd: TPoint;
    function GetCanPaste: Boolean;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetCaretX: Integer; virtual;
    function GetCaretXY: TPoint;
    function GetCaretY: Integer; virtual;
    function GetCommands: IEditorCommands;
    function GetCurrentWord: string;
    function GetEditor: TSynEdit;
    function GetEditorFont: TFont;
    function GetManager: IEditorManager;
    function GetEncoding: string;
    function GetEvents: IEditorEvents;
    function GetFileName: string;
    function GetFindHistory: TStrings;
    function GetFoldLevel: Integer;
    function GetFoldState: string;
    function GetForm: TCustomForm;
    function GetHighlighterItem: THighlighterItem;
    function GetLineBreakStyle: string;
    function GetLines: TStrings; virtual;
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
    function GetPopupMenu: TPopupMenu; reintroduce;
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
    procedure SetCaretX(const Value: Integer); virtual;
    procedure SetCaretXY(const AValue: TPoint);
    procedure SetCaretY(const Value: Integer); virtual;
    procedure SetCodeCompletionFormat(const Value: string); virtual;
    procedure SetCompletionInsertList(const Value: TStrings); virtual;
    procedure SetCompletionItemList(const Value: TStrings); virtual;
    procedure SetEditorFont(AValue: TFont);
    procedure SetEncoding(const AValue: string);
    procedure SetFileName(const AValue: string);
    procedure SetFoldLevel(const AValue: Integer);
    procedure SetFoldState(const AValue: string);
    procedure SetHighlighter(const AValue: TSynCustomHighlighter);
    procedure SetHighlighterItem(const AValue: THighlighterItem);
    procedure SetLineBreakStyle(const AValue: string);
    procedure SetLines(const Value: TStrings); virtual;
    procedure SetLineText(const AValue: string);
    procedure SetLogicalCaretXY(const AValue: TPoint);
    procedure SetModified(const AValue: Boolean);
    procedure SetMonitorChanges(const AValue: Boolean);
    procedure SetName(AValue: string); reintroduce;
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetOnDropFiles(const AValue: TDropFilesEvent);
    procedure SetOnStatusChange(const AValue: TStatusChangeEvent);
    procedure SetParent(const AValue: TWinControl); reintroduce;
    procedure SetPopupMenu(const AValue: TPopupMenu);
    procedure SetSearchOptions(AValue: TSynSearchOptions);
    procedure SetSearchText(const Value: string); virtual;
    procedure SetSelEnd(const AValue: Integer);
    procedure SetSelStart(const AValue: Integer);
    procedure SetSelText(const AValue: string);
    procedure SetShowSpecialChars(const AValue: Boolean);
    procedure SetText(const AValue: string);
    procedure SetTopLine(const AValue: Integer);
    {$endregion}

    procedure InitializeEditor;
    procedure OnSettingsChanged(ASender: TObject);

  protected
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure StoreBlock;
    procedure RestoreBlock;

    procedure CopyToClipboard;
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure Undo;
    procedure Redo;

    procedure Activate; override;

    procedure AssignHighlighterForFileType(const AFileExt: string);
    procedure SmartSelect;

    procedure SetHighlightSearch(
      const ASearch  : string;
            AOptions : TSynSearchOptions
    );

    procedure SearchAndSelectLine(ALineIndex: Integer; const ALine: string);
    procedure SearchAndSelectText(const AText: string);
    procedure SelectWord;
    procedure ClearHighlightSearch;

    procedure Clear;
    procedure SelectAll;
    procedure UpdateActions; override;
    function SelectBlockAroundCursor(const AStartTag, AEndTag: string;
      AIncludeStartTag, AIncludeEndTag: Boolean): Boolean;
    procedure AdjustFontSize(AOffset: Integer);
    // operations on selections
    procedure UpdateCommentSelection(ACommentOn, AToggle: Boolean);
    procedure ToggleBlockCommentSelection;
    procedure StripMarkupFromSelection;
    procedure StripCharsFromSelection(
      AFirst : Boolean;
      ALast  : Boolean
    );
    procedure AlignSelection(
      const AToken                  : string;
            ACompressWS             : Boolean;
            AInsertSpaceBeforeToken : Boolean;
            AInsertSpaceAfterToken  : Boolean;
            AAlignInParagraphs      : Boolean
    );

    procedure UpperCaseSelection;
    procedure LowerCaseSelection;

    procedure DoChange; dynamic;
    procedure DoClose(var CloseAction: TCloseAction); override;

  public
    // constructors and destructors
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    // public overridden methods
    function CloseQuery: Boolean; override;

    // public methods
    function GetWordAtPosition(APosition: TPoint): string;
    function GetWordFromCaret(const ACaretPos: TPoint): string;
    procedure InsertTextAtCaret(const AText: string);
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    procedure SaveToFile(const AFileName: string);

    procedure AssignHighlighter(const AHighlighter: string = 'TXT');

    // public properties
    { Column and line of the start of the selected block. }
    property BlockBegin: TPoint
      read GetBlockBegin write SetBlockBegin;

    { Column and line of the end of the selected block. }
    property BlockEnd: TPoint
      read GetBlockEnd write SetBlockEnd;

    { Current coordinate of the caret. }
    property CaretXY: TPoint
      read GetCaretXY write SetCaretXY;

    property LogicalCaretXY: TPoint
      read GetLogicalCaretXY write SetLogicalCaretXY;

  published
    { current X-coordinate of the caret. }
    property CaretX: Integer
      read GetCaretX write SetCaretX;

    { current Y-coordinate of the caret. }
    property CaretY: Integer
      read GetCaretY write SetCaretY;

    property CurrentWord: string
      read GetCurrentWord;

    property TextSize: Integer
      read GetTextSize;

    property CanPaste: Boolean
      read GetCanPaste;

    property CanRedo: Boolean
      read GetCanRedo;

    property CanUndo: Boolean
      read GetCanUndo;

    property FoldLevel: Integer
      read GetFoldLevel write SetFoldLevel;

    property SelStart: Integer
      read GetSelStart write SetSelStart;

    property SelEnd: Integer
      read GetSelEnd write SetSelEnd;

    property ShowSpecialChars: Boolean
      read GetShowSpecialChars write SetShowSpecialChars;

    property Modified: Boolean
      read GetModified write SetModified;

    property Name: string
      read GetName write SetName;

    property LinesInWindow: Integer
      read GetLinesInWindow;

    property Editor: TSynEdit
      read GetEditor;

    property Form: TCustomForm
      read GetForm;

    { string identifying the foldstate of current instance }
    property FoldState: string
      read GetFoldState write SetFoldState;

    property LineText: string
      read GetLineText write SetLineText;

    property PreviewText: string
      read GetPreviewText;

    property SelAvail: Boolean
      read GetSelAvail;

    property Manager: IEditorManager
      read GetManager;

    { Reference to the IEditorActions singleton that manages one or more
      IEditView instances. }
    property Actions: IEditorActions
      read GetActions;

    property Commands: IEditorCommands
      read GetCommands;

    { A set of useful events to dispatch to the application. }
    property Events: IEditorEvents
      read GetEvents;

    { Global settings shared by all EditorView instances }
    property Settings: IEditorSettings
      read GetSettings;

    { TODO: assign this to active search engine }
    property FindHistory: TStrings
      read GetFindHistory;

    { TODO: assign this to active search engine }
    property ReplaceHistory: TStrings
      read GetReplaceHistory;

    property Highlighter: TSynCustomHighlighter
      read FHighlighter write SetHighlighter;

    property HighlighterItem: THighlighterItem
      read GetHighlighterItem write SetHighlighterItem;

    property SupportsFolding: Boolean
      read GetSupportsFolding;

    { Shortcut to the text contained in the editor. }
    property Lines: TStrings
      read GetLines write SetLines;

    property MonitorChanges: Boolean
      read GetMonitorChanges write SetMonitorChanges;

    property Text: string
      read GetText write SetText;

    property TopLine: Integer
      read GetTopLine write SetTopLine;

    { Stringlist containing (formatted) items to include in the displayed
      completion-item list of the editor. }
    property CompletionItemList: TStrings
      read FCompletionItemList write SetCompletionItemList;

    { Stringlist of strings referenced by the items in the CompletionItemList.
      These are the bare strings that will be inserted in the code editor when
      an item from the completionlist is selected. }
    property CompletionInsertList: TStrings
      read FCompletionInsertList write SetCompletionInsertList;

    { Readonly stringlist containing filters for all supported highlighters. }
    property FileFilterList: TStringList
      read FFileFilterList;

    property SearchText: string
      read GetSearchText write SetSearchText;

    property SearchOptions: TSynSearchOptions
      read GetSearchOptions write SetSearchOptions;

    { Currently selected text in the editor. }
    property SelText: string
      read GetSelText write SetSelText;

    property FileName: string
      read GetFileName write SetFileName;

    property Parent: TWinControl
      read GetParent write SetParent;

    { Current file encoding (Editor encoding is always UTF-8). }
    property Encoding: string
      read GetEncoding write SetEncoding;

    property EditorFont: TFont
      read GetEditorFont write SetEditorFont;

    property LineBreakStyle: string
      read GetLineBreakStyle write SetLineBreakStyle;

    property PopupMenu: TPopupMenu
      read GetPopupMenu write SetPopupMenu;

    property OnStatusChange: TStatusChangeEvent
      read GetOnStatusChange write SetOnStatusChange;

    property OnChange: TNotifyEvent
      read GetOnChange write SetOnChange;
  end;

//*****************************************************************************

implementation

{$R *.lfm}

uses
  GraphUtil,

  LConvEncoding, LCLProc,

  SynEditMouseCmds,

  ts_Editor_Utils;

{$region 'construction and destruction' /fold}
//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TEditorView.AfterConstruction;
begin
  inherited AfterConstruction;
  FEditor := TSynEdit.Create(Self);
  FFindHistory := TStringList.Create;
  FFindHistory.Sorted := True;
  FFindHistory.Duplicates := dupIgnore;
  FReplaceHistory := TStringList.Create;
  FReplaceHistory.Sorted := True;
  FReplaceHistory.Duplicates := dupIgnore;
  FFileFilterList := TStringList.Create;
  FFileFilterList.Duplicates := dupIgnore;
  FFileFilterList.Sorted := True;
  FEncoding := EncodingUTF8;
  FLineBreakStyle := ALineBreakStyles[Lines.TextLineBreakStyle];
  Doublebuffered := True;
  InitializeEditor;
  FDirectoryWatch          := TDirectoryWatch.Create;
  FDirectoryWatch.OnNotify := DirectoryWatchNotify;
  Settings.AddEditorSettingsChangedHandler(OnSettingsChanged);
end;

procedure TEditorView.BeforeDestruction;
begin
  if Assigned(Settings) then
    Settings.RemoveEditorSettingsChangedHandler(OnSettingsChanged);
  DisableAutoSizing;
  FreeAndNil(FDirectoryWatch);
  FreeAndNil(FReplaceHistory);
  FreeAndNil(FFindHistory);
  FreeAndNil(FBeautifier);
  FreeAndNil(FSyncronizedEdit);
  FreeAndNil(FTemplateEdit);
  FreeAndNil(FFileFilterList);
  inherited BeforeDestruction;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************
{$endregion}

{$region 'event handlers' /fold}
//*****************************************************************************
// event handlers                                                        BEGIN
//*****************************************************************************

procedure TEditorView.FormDropFiles(Sender: TObject;
  const FileNames: array of string);
begin
  if Assigned(FOnDropFiles) then
    FOnDropFiles(Self, FileNames);
  Events.DoChange;
end;

{ Makes actionlist shortcuts work on the form }

procedure TEditorView.FormShortCut(var Msg: TLMKey; var Handled: Boolean);
begin
  Handled := Actions.ActionList.IsShortCut(Msg);
end;

{ Event triggered when MonitorChanges is True }

procedure TEditorView.DirectoryWatchNotify(const Sender: TObject;
  const AAction: TWatchAction; const FileName: string);
begin
  if SameText(FileName, ExtractFileName(Self.FileName)) and (AAction = waModified) then
  begin
    LoadFromFile(Self.FileName);
    if CanFocus then
    begin
      Editor.CaretY := Editor.Lines.Count;
      Editor.EnsureCursorPosVisible;
    end;
  end;
end;

procedure TEditorView.EditorClickLink(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Commands.OpenFileAtCursor;
end;

procedure TEditorView.EditorPaste(Sender: TObject; var AText: string;
  var AMode: TSynSelectionMode; ALogStartPos: TPoint; var AnAction: TSynCopyPasteAction);
begin
  if (Lines.Count = 0) and Settings.AutoGuessHighlighterType then
    HighlighterItem := Manager.Highlighters.ItemsByName[GuessHighlighterType(AText)];
  if (HighlighterItem.Name = HL_XML) and Settings.AutoFormatXML then
  begin
    AText := FormatXML(AText);
  end;
end;

procedure TEditorView.EditorChange(Sender: TObject);
begin
  DoChange;
  Events.DoChange;
end;

procedure TEditorView.EditorStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if not (csDestroying in ComponentState) then
  begin
    // we use this event to ensure that the view is activated because the OnEnter
    // event is not triggered when the form is undocked!
    if not IsActive then
		  Activate;
    if Assigned(FOnStatusChange) then
      FOnStatusChange(Self, Changes);
    Events.DoStatusChange(Changes);
    if (scCaretX in Changes) or (scCaretY in Changes) then
    begin
      Events.DoCaretPositionChange;
    end;
  end;
end;

//*****************************************************************************
// event handlers                                                          END
//*****************************************************************************
{$endregion}

{$region 'event dispatch methods' /fold}
//*****************************************************************************
// event dispatch methods                                                BEGIN
//*****************************************************************************

procedure TEditorView.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TEditorView.DoClose(var CloseAction: TCloseAction);
begin
  CloseAction :=  caFree;
  inherited DoClose(CloseAction);
end;

//*****************************************************************************
// event dispatch methods                                                  END
//*****************************************************************************
{$endregion}

{$region 'property access methods' /fold}
//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

procedure TEditorView.SetSelEnd(const AValue: Integer);
begin
  Editor.SelEnd := AValue;
end;

procedure TEditorView.SetSelStart(const AValue: Integer);
begin
  Editor.SelStart := AValue;
end;

function TEditorView.GetSelText: string;
begin
  Result := Editor.SelText;
end;

procedure TEditorView.SetSelText(const AValue: string);
begin
  Editor.SelText := AValue;
end;

function TEditorView.GetFoldLevel: Integer;
begin
  Result := FFoldLevel;
end;

procedure TEditorView.SetFoldLevel(const AValue: Integer);
begin
  if AValue <> FoldLevel then
  begin
    FFoldLevel := AValue;
    Editor.FoldAll(FFoldLevel, True);
    Events.DoModified;
  end;
end;

function TEditorView.GetText: string;
begin
  Result := Lines.Text;
end;

procedure TEditorView.SetText(const AValue: string);
begin
  if (Lines.Count = 0) and not Assigned(HighlighterItem) and Settings.AutoGuessHighlighterType then
    HighlighterItem := Manager.Highlighters.ItemsByName[GuessHighlighterType(AValue)];
  if Assigned(HighlighterItem) and (HighlighterItem.Name = HL_XML)
    and Settings.AutoFormatXML then
  begin
    Lines.Text := FormatXML(AValue);
  end
  else
    Lines.Text := AValue;
end;

function TEditorView.GetCaretX: Integer;
begin
  Result := Editor.CaretX;
end;

procedure TEditorView.SetCaretX(const Value: Integer);
begin
  Editor.CaretX := Value;
end;

function TEditorView.GetCaretY: Integer;
begin
  Result := Editor.CaretY;
end;

function TEditorView.GetCommands: IEditorCommands;
begin
  Result := Owner as IEditorCommands;
end;

procedure TEditorView.SetCaretY(const Value: Integer);
begin
  Editor.CaretY := Value;
end;

procedure TEditorView.SetLogicalCaretXY(const AValue: TPoint);
begin
  Editor.LogicalCaretXY := AValue;
end;

procedure TEditorView.SetModified(const AValue: Boolean);
begin
  Editor.Modified := AValue;
end;

procedure TEditorView.SetMonitorChanges(const AValue: Boolean);
begin
  if AValue <> MonitorChanges then
  begin
    if AValue then
    begin
      if FileExists(FileName) then
      begin
        FDirectoryWatch.Directory := ExtractFileDir(FileName);
        FDirectoryWatch.Start;
      end;
    end
    else
      FDirectoryWatch.Stop;
  end;
end;

procedure TEditorView.SetName(AValue: string);
begin
  inherited Name := AValue;
end;

procedure TEditorView.SetOnChange(const AValue: TNotifyEvent);
begin
  FOnChange := AValue;
end;

procedure TEditorView.SetOnDropFiles(const AValue: TDropFilesEvent);
begin
  FOnDropFiles := AValue;
end;

procedure TEditorView.SetParent(const AValue: TWinControl);
begin
  inherited Parent := AValue;
  if Assigned(Parent) then
    Visible := True;
end;

procedure TEditorView.SetPopupMenu(const AValue: TPopupMenu);
begin
  Editor.PopupMenu := AValue;
end;

procedure TEditorView.SetSearchOptions(AValue: TSynSearchOptions);
begin
  FSearchOptions := AValue;
end;

function TEditorView.GetEditor: TSynEdit;
begin
  Result := FEditor;
end;

function TEditorView.GetEditorFont: TFont;
begin
  Result := Editor.Font;
end;

procedure TEditorView.SetEditorFont(AValue: TFont);
begin
  if not Editor.Font.IsEqual(AValue) then
  begin
    Editor.Font.Assign(AValue);
  end;
end;

function TEditorView.GetManager: IEditorManager;
begin
  Result := Owner as IEditorManager;
end;

function TEditorView.GetLines: TStrings;
begin
  Result := Editor.Lines;
end;

procedure TEditorView.SetLines(const Value: TStrings);
begin
  Editor.Lines := Value;
end;

procedure TEditorView.SetCodeCompletionFormat(const Value: string);
begin
  FCodeCompletionFormat := Value;
end;

procedure TEditorView.SetCompletionInsertList(const Value: TStrings);
begin
  //SynCompletionProposal.InsertList.Clear;
  //SynCompletionProposal.InsertList.AddStrings(Value);
end;

procedure TEditorView.SetCompletionItemList(const Value: TStrings);
begin
  //SynCompletionProposal.ItemList.Clear;
  //SynCompletionProposal.ItemList.AddStrings(Value);
end;

function TEditorView.GetSearchText: string;
begin
  Result := FSearchText;
end;

procedure TEditorView.SetSearchText(const Value: string);
begin
  if Value <> SearchText then
  begin
    FSearchText := Value;
    Editor.SetHighlightSearch(Value, SearchOptions);
  end;
end;

procedure TEditorView.SetLineText(const AValue: string);
begin
  Editor.LineText := AValue;
end;

function TEditorView.GetCurrentWord: string;
var
  P: TPoint;
begin
  P := Editor.LogicalCaretXY;
  Result := Editor.GetWordAtRowCol(P);
end;

procedure TEditorView.SetHighlighter(const AValue: TSynCustomHighlighter);
begin
  if AValue <> Highlighter then
  begin
    FHighlighter := AValue;
    Editor.Highlighter := AValue;
    if Editor.Focused and not Editor.Modified then
    begin
      Events.DoModified;
      Events.DoChange;
    end;
  end;
end;

function TEditorView.GetTopLine: Integer;
begin
  Result := Editor.TopLine;
end;

procedure TEditorView.SetTopLine(const AValue: Integer);
begin
  Editor.TopLine := AValue;
end;

function TEditorView.GetFileName: string;
begin
  Result := FFileName;
end;

procedure TEditorView.SetFileName(const AValue: string);
begin
  if AValue <> FileName then
  begin
    FFileName := AValue;
    Caption := ExtractFileName(AValue);
  end;
end;

function TEditorView.GetEncoding: string;
begin
  Result := FEncoding;
end;

function TEditorView.GetCanPaste: Boolean;
begin
  Result := Editor.CanPaste;
end;

function TEditorView.GetCanRedo: Boolean;
begin
  Result := Editor.CanRedo;
end;

function TEditorView.GetCanUndo: Boolean;
begin
  Result := Editor.CanUndo;
end;

function TEditorView.GetForm: TCustomForm;
begin
  Result := Self;
end;

procedure TEditorView.SetLineBreakStyle(const AValue: string);
begin
  if AValue <> LineBreakStyle then
  begin
    FLineBreakStyle := AValue;
    if FileExists(FFileName) then
      SaveToFile(FFileName);
  end;
end;

function TEditorView.GetLineBreakStyle: string;
begin
  Result := FLineBreakStyle;
end;

function TEditorView.GetLineText: string;
begin
  Result := Editor.LineText;
end;

function TEditorView.GetMonitorChanges: Boolean;
begin
  Result := FDirectoryWatch.Running;
end;

function TEditorView.GetName: string;
begin
  Result := inherited Name;
end;

function TEditorView.GetOnChange: TNotifyEvent;
begin
  Result := FOnChange;
end;

function TEditorView.GetOnDropFiles: TDropFilesEvent;
begin
  Result := FOnDropFiles;
end;

function TEditorView.GetOnStatusChange: TStatusChangeEvent;
begin
  Result := FOnStatusChange;
end;

procedure TEditorView.SetOnStatusChange(const AValue: TStatusChangeEvent);
begin
  FOnStatusChange := AValue;
end;

function TEditorView.GetParent: TWinControl;
begin
  Result := inherited Parent;
end;

function TEditorView.GetPopupMenu: TPopupMenu;
begin
  Result := Editor.PopupMenu;
end;

function TEditorView.GetPreviewText: string;
begin
  if SelAvail then
    Result := SelText
  else
    Result := LineText;
end;

function TEditorView.GetSelAvail: Boolean;
begin
  Result := Editor.SelAvail;
end;

procedure TEditorView.SetHighlighterItem(const AValue: THighlighterItem);
begin
  if HighlighterItem <> AValue then
  begin
    FHighlighterItem := AValue;
    if Assigned(AValue) then
    begin
      AValue.Reload;
      Settings.HighlighterType := FHighlighterItem.Name;
      Highlighter := AValue.SynHighlighter;
      // Update editor actions!!!
      Actions.UpdateHighLighterActions;
    end;
  end;
end;

function TEditorView.GetFoldState: string;
begin
  Result := Editor.FoldState;
end;

function TEditorView.GetLinesInWindow: Integer;
begin
  Result := Editor.LinesInWindow;
end;

function TEditorView.GetLogicalCaretXY: TPoint;
begin
  Result := Editor.LogicalCaretXY;
end;

function TEditorView.GetModified: Boolean;
begin
  Result := Editor.Modified;
end;

function TEditorView.GetReplaceHistory: TStrings;
begin
  Result := FReplaceHistory;
end;

function TEditorView.GetSearchOptions: TSynSearchOptions;
begin
  Result := FSearchOptions;
end;

function TEditorView.GetSelEnd: Integer;
begin
  Result := Editor.SelEnd;
end;

function TEditorView.GetSelStart: Integer;
begin
  Result := Editor.SelStart;
end;

function TEditorView.GetSettings: IEditorSettings;
begin
  Result := Owner as IEditorSettings;
end;

procedure TEditorView.SetFoldState(const AValue: string);
begin
  Editor.FoldState := AValue;
end;

function TEditorView.GetActions: IEditorActions;
begin
  Result := Owner as IEditorActions;
end;

function TEditorView.GetCaretXY: TPoint;
begin
  Result := Editor.CaretXY;
end;

procedure TEditorView.SetCaretXY(const AValue: TPoint);
begin
  Editor.CaretXY := AValue;
end;

function TEditorView.GetEvents: IEditorEvents;
begin
  Result := Owner as IEditorEvents;
end;

function TEditorView.GetFindHistory: TStrings;
begin
  Result := FFindHistory;
end;

function TEditorView.GetHighlighterItem: THighlighterItem;
begin
  Result := FHighlighterItem;
end;

function TEditorView.GetShowSpecialChars: Boolean;
begin
  Result := eoShowSpecialChars in Editor.Options;
end;

function TEditorView.GetSupportsFolding: Boolean;
begin
  Result := Assigned(HighlighterItem)
    and Assigned(HighlighterItem.SynHighlighter)
    and (HighlighterItem.SynHighlighter is TSynCustomFoldHighlighter);
end;

function TEditorView.GetTextSize: Integer;
begin
  Result := Length(Text);
end;

procedure TEditorView.SetEncoding(const AValue: string);
begin
  if AValue <> Encoding then
  begin
    FEncoding := AValue;
    if FileExists(FFileName) then
      SaveToFile(FFileName);
  end;
end;

procedure TEditorView.SetShowSpecialChars(const AValue: Boolean);
begin
  if AValue then
    Editor.Options := Editor.Options + [eoShowSpecialChars]
  else
    Editor.Options := Editor.Options - [eoShowSpecialChars];
end;

function TEditorView.GetBlockBegin: TPoint;
begin
  Result := Editor.BlockBegin;
end;

procedure TEditorView.SetBlockBegin(const AValue: TPoint);
begin
  Editor.BlockBegin := AValue;
end;

function TEditorView.GetBlockEnd: TPoint;
begin
  Result := Editor.BlockEnd;
end;

procedure TEditorView.SetBlockEnd(const AValue: TPoint);
begin
  Editor.BlockEnd := AValue;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************
{$endregion}

{$region 'private methods' /fold}
//*****************************************************************************
// private methods                                                       BEGIN
//*****************************************************************************

procedure TEditorView.AssignHighlighter(const AHighlighter: string);
begin
  HighlighterItem := Manager.Highlighters.ItemsByName[AHighlighter];
end;

procedure TEditorView.AssignHighlighterForFileType(const AFileExt: string);
begin
  HighlighterItem := Manager.Highlighters.FindHighlighterForFileType(AFileExt);
  if not Assigned(HighlighterItem) and Settings.AutoGuessHighlighterType then
    AssignHighlighter(GuessHighlighterType(Text));
end;

function TEditorView.IsActive: Boolean;
begin
  Result := Manager.ActiveView = (Self as IEditorView)
end;

procedure TEditorView.StoreBlock;
begin
  Logger.EnterMethod(Self, 'StoreBlock');
  FStoredBlockBegin    := BlockBegin;
  Logger.Send('FBlockBegin', FStoredBlockBegin);
  FStoredBlockEnd      := BlockEnd;
  Logger.Send('FBlockEnd', FStoredBlockEnd);
  FStoredSelectionMode := Editor.SelectionMode;
  Logger.ExitMethod(Self, 'StoreBlock');
end;

procedure TEditorView.RestoreBlock;
begin
  Logger.EnterMethod(Self, 'RestoreBlock');
  Logger.Send('FBlockBegin', FStoredBlockBegin);
  Logger.Send('FBlockEnd', FStoredBlockEnd);
  BlockBegin           := FStoredBlockBegin;
  BlockEnd             := FStoredBlockEnd;
  Editor.SelectionMode := FStoredSelectionMode;
  Logger.ExitMethod(Self, 'RestoreBlock');
end;

procedure TEditorView.InitializeEditor;
var
  N: Integer;
begin
  FEditor.Parent := Self;
  FEditor.Align := alClient;
  FEditor.Font.Assign(Settings.EditorFont);
  FEditor.BorderStyle := bsNone;
  FEditor.DoubleBuffered := True;
  FEditor.BookMarkOptions.BookmarkImages := imlBookmarkImages;
  FEditor.Gutter.Color := 15329769; // light gray
  FEditor.Gutter.Width := 29;
  FEditor.Gutter.SeparatorPart.Visible := False;
  with FEditor.Gutter.LineNumberPart do
  begin
    Width := 15;
    MarkupInfo.Background := clNone;
    MarkupInfo.Foreground := clGray;
    MarkupInfo.StyleMask := [fsBold];
    DigitCount := 2;
    ShowOnlyLineNumbersMultiplesOf := 10;
    ZeroStart := False;
    LeadingZeros := False;
  end;
  with FEditor.Gutter.ChangesPart do
  begin
    Width := 4;
    ModifiedColor := 59900;
    SavedColor := clGreen;
  end;
  with FEditor.Gutter.CodeFoldPart do
  begin
    MarkupInfo.Background := clNone;
    MarkupInfo.Foreground := clGray;
  end;
  // TODO: Bookmarks
  with FEditor.Gutter.MarksPart do
  begin
    Width := 1;
    Visible := False;
  end;

  FEditor.Options := [
    eoAltSetsColumnMode,
    eoAutoIndent,
    eoAutoIndentOnPaste,
    eoEnhanceHomeKey,
    eoGroupUndo,
    eoHalfPageScroll,
    eoSmartTabs,
    eoTabIndent,
    eoTabsToSpaces,
    eoTrimTrailingSpaces,
    eoBracketHighlight,
    eoShowCtrlMouseLinks,
    eoScrollPastEol,         // makes column selections easier
    eoDragDropEditing,
//    eoPersistentCaret,     // don't use! bug in TSynEdit
    eoShowScrollHint
  ];
  FEditor.Options2 := [
    eoEnhanceEndKey,
    eoFoldedCopyPaste,
    eoOverwriteBlock
  ];
  FEditor.MouseOptions := [
    emAltSetsColumnMode,
    emDragDropEditing,
    emCtrlWheelZoom,
    emShowCtrlMouseLinks
  ];
  FEditor.ScrollBars := ssAutoBoth;

  FEditor.BracketHighlightStyle := sbhsRightOfCursor;
  FEditor.TabWidth := 2;
  FEditor.WantTabs := True;

  FEditor.SelectedColor.Background := clGray;
  FEditor.SelectedColor.MergeFinalStyle := True;

  FEditor.BracketMatchColor.Background := clAqua;
  FEditor.BracketMatchColor.FrameColor := clGray;

  FEditor.HighlightAllColor.Background := $0064B1FF;  // light orange
  FEditor.HighlightAllColor.FrameColor := $004683FF;  // dark orange
  FEditor.HighlightAllColor.FrameStyle := slsSolid;
  FEditor.HighlightAllColor.FrameEdges := sfeAround;
  FEditor.HighlightAllColor.Foreground := clNone;
  FEditor.HighlightAllColor.MergeFinalStyle := True;

  FEditor.LineHighlightColor.Background := $009FFFFF; // yellow
  FEditor.LineHighlightColor.FrameEdges := sfeAround;
  FEditor.LineHighlightColor.FrameStyle := slsWaved;
  FEditor.LineHighlightColor.FrameColor := $0000C4C4; // darker shade of yellow
  FEditor.LineHighlightColor.MergeFinalStyle := True;

  FEditor.OnStatusChange := EditorStatusChange;
  FEditor.OnChange       := EditorChange;
  FEditor.OnClickLink    := EditorClickLink;
  FEditor.OnPaste        := EditorPaste;

  FEditor.Visible := True;

  FSyncronizedEdit := TSynPluginSyncroEdit.Create(nil);
  FSyncronizedEdit.Editor := Editor;
  FSyncronizedEdit.Active := False;

  FTemplateEdit := TSynPluginTemplateEdit.Create(nil);
  FTemplateEdit.Editor := Editor;
  FTemplateEdit.Active := False;

  FBeautifier := TSynBeautifier.Create(nil);
  FBeautifier.AutoIndent := True;
  FEditor.Beautifier := FBeautifier;

  // TEMP CODE TSI
  N := FEditor.Keystrokes.FindShortcut(TextToShortCut('Shift+Ctrl+N'));
  FEditor.Keystrokes.Delete(N);
  N := FEditor.Keystrokes.FindShortcut(TextToShortCut('Ctrl+N'));
  FEditor.Keystrokes.Delete(N);
  //N := FEditor.Keystrokes.FindShortcut(TextToShortCut('Ctrl+S'));
  //FEditor.Keystrokes.Delete(N);
  //N := FEditor.Keystrokes.FindShortcut(TextToShortCut('F1'));
  //FEditor.Keystrokes.Delete(N);

  //FCompletionProposal := TSynCompletionProposal.Create(Self);
  //FCompletionProposal.Editor := Editor;
  //FCompletionProposal.InsertList.Add('Insert');
  //FCompletionProposal.InsertList.Add('List');
  //FCompletionProposal.ItemList.Add('IOnsert');
  //FCompletionProposal.ItemList.Add('IOn');
  //FCompletionProposal.Columns.Add;
  //FCompletionProposal.Resizeable := True;
  //FEditor.MouseActions.FindCommand();
  // delete the quickpaste command with middle mouse button

  // highlights all words that are the same as the one surrounding the caret position
  FMHAC := Editor.MarkupByClass[TSynEditMarkupHighlightAllCaret]
    as TSynEditMarkupHighlightAllCaret;
  FMHAC.MarkupInfo.FrameColor := clSilver;
  FMHAC.MarkupInfo.FrameStyle := slsSolid;
  FMHAC.WaitTime := 200;
  FMHAC.MarkupInfo.Background := $00F0F0F0;
  FMHAC.IgnoreKeywords := True;
  FMHAC.SearchOptions := [ssoMatchCase, ssoWholeWord];
  FMHAC.FullWord := True;
  FMHAC.Enabled := True;

  // highlights all strings that are the same as the selected block
  //FMS :=  Editor.MarkupByClass[TSynEditMarkupSelection]
  //  as TSynEditMarkupSelection;
  //FMS.MarkupInfoSeletion.FrameStyle := slsWaved;
  //FMS.MarkupInfo.FrameStyle := slsWaved;
  //FMS.MarkupInfoIncr.FrameStyle := slsWaved;
  //FMS.Enabled := True;

  // highlights all strings that are the same as a particular word (SearchString)
  //FMHA := Editor.MarkupByClass[TSynEditMarkupHighlightAll]
  //  as TSynEditMarkupHighlightAll;
  //  FMHA.MarkupInfo.FrameColor := clBlue;
  //FMHA.MarkupInfo.FrameStyle := slsSolid;
  //FMHA.MarkupInfo.Background := clOlive;
  ////FMHA.SearchString := 'Style';
  //FMHA.SearchOptions := [ssoMatchCase, ssoWholeWord];
  //FMHA.Enabled := True;


  {
   FROM SYNEDIT SOURCES
    // needed before setting color
  fMarkupHighCaret := TSynEditMarkupHighlightAllCaret.Create(self);
  fMarkupHighCaret.Selection := FBlockSelection;
  fMarkupHighAll   := TSynEditMarkupHighlightAll.Create(self);
  fMarkupBracket   := TSynEditMarkupBracket.Create(self);
  fMarkupWordGroup := TSynEditMarkupWordGroup.Create(self);
  fMarkupCtrlMouse := TSynEditMarkupCtrlMouseLink.Create(self);
  fMarkupSpecialLine := TSynEditMarkupSpecialLine.Create(self);
  fMarkupSelection := TSynEditMarkupSelection.Create(self, FBlockSelection);
  fMarkupSpecialChar := TSynEditMarkupSpecialChar.Create(self);

  }
  ActiveControl := Editor;
end;

procedure TEditorView.OnSettingsChanged(ASender: TObject);
begin
  FUpdate := True;
end;

//*****************************************************************************
// private methods                                                         END
//*****************************************************************************
{$endregion}

{$region 'protected methods' /fold}
//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************
{ TODO: store caret pos? }

procedure TEditorView.BeginUpdate;
begin
  Logger.EnterMethod(Self, 'BeginUpdate');
//  Editor.BeginUpdate;
//  Editor.BeginUpdateBounds; // TODO investigate this
  Editor.BeginUndoBlock;
  Logger.ExitMethod(Self, 'BeginUpdate');
end;

procedure TEditorView.EndUpdate;
begin
  Logger.EnterMethod(Self, 'EndUpdate');
  Editor.EndUndoBlock;
//  Editor.EndUpdateBounds; // TODO investigate this
//  Editor.EndUpdate;
  Logger.ExitMethod(Self, 'EndUpdate');
end;

procedure TEditorView.CopyToClipboard;
begin
  Editor.CopyToClipboard;
end;

procedure TEditorView.Cut;
begin
  if Editor.Focused then
  begin
    if not Editor.SelAvail then
      Editor.SelectWord;
    Editor.CutToClipboard;
  end
end;

procedure TEditorView.Copy;
begin
  if Editor.Focused then
  begin
    if not Editor.SelAvail then
      Editor.SelectWord;
    Editor.CopyToClipboard;
  end
end;

procedure TEditorView.Paste;
begin
  if Editor.Focused and CanPaste then;
  begin
    Editor.PasteFromClipboard;
  end;
end;

procedure TEditorView.Undo;
begin
  Editor.Undo;
end;

procedure TEditorView.Redo;
begin
  Editor.Redo;
end;

{ Make current editor instance the active one in the editor manager. This does
  not automatically make it focused as the current focus can be set to eg. a
  tool window. }

procedure TEditorView.Activate;
begin
  inherited;
  Manager.ActiveView := Self as IEditorView;
end;

{ Selects block of code around cursor between AStartTag and AEndTag. Used by
  the SmartSelect procedure.

  TODO:
    - support for nested AStartTag and AEndTag (ignore sublevels)
}

function TEditorView.SelectBlockAroundCursor(const AStartTag, AEndTag: string;
  AIncludeStartTag, AIncludeEndTag: Boolean): Boolean;
var
  Pos : Integer;
  S   : string;
  B   : Boolean;
  I   : Integer;
  N   : Integer;
begin
  if (AStartTag = '') or (AEndTag = '') then
    Exit;

  S := Text;
  Pos := SelStart;
  B := False;
  while not B and (Pos > 1) do
  begin
    N := Length(AStartTag);
    I := N;
    B := S[Pos] = AStartTag[I];
    while B and (Pos > 1) and (I > 1) do
    begin
      Dec(I);
      Dec(Pos);
      B := S[Pos] = AStartTag[I];
    end;
    if not B and (Pos > 1) then
      Dec(Pos);
  end;
  if B then
  begin
    if AIncludeStartTag then
      SelStart := Pos
    else
      SelStart := Pos + N;
  end;

  if B then
  begin
    Pos := SelStart;
    B := False;
    while not B and (Pos <= Length(S)) do
    begin
      N := Length(AEndTag);
      I := 1;
      B := S[Pos] = AEndTag[I];
      while B and (Pos <= Length(S)) and (I < N) do
      begin
        Inc(I);
        Inc(Pos);
        B := S[Pos] = AEndTag[I];
      end;
      if not B and (Pos <= Length(S)) then
        Inc(Pos);
    end;
    if B then
    begin
      if AIncludeEndTag then
        SelEnd := Pos + 1
      else
        SelEnd := Pos - N + 1;
    end;
  end;
  Result := SelAvail;
end;

procedure TEditorView.AdjustFontSize(AOffset: Integer);
begin
  Editor.Font.Size := Editor.Font.Size + AOffset;
end;

{ Comments or uncomments selected code lines based on the line comment tag of
  the active highlighter. }

procedure TEditorView.UpdateCommentSelection(ACommentOn, AToggle: Boolean);
var
  OldCaretPos    : TPoint;
  WasSelAvail    : Boolean;
  BlockBeginLine : Integer;
  BlockEndLine   : Integer;
  CommonIndent   : Integer;
  Prefix         : string;
  PrefixLength   : Integer;

  function FirstNonBlankPos(const AText: string; AStart: Integer = 1): Integer;
  var
    I: Integer;
  begin
    for I := AStart to Length(AText) do
      if (AText[I] <> #32) and (AText[I] <> #9) then
        Exit(I);
    Result := -1;
  end;

  function MinCommonIndent: Integer;
  var
    I, J: Integer;
  begin
    if CommonIndent = 0 then
    begin
      CommonIndent := Max(FirstNonBlankPos(Lines[BlockBeginLine - 1]), 1);
      for I := BlockBeginLine + 1 to BlockEndLine do
      begin
        J := FirstNonBlankPos(Lines[I - 1]);
        if (J < CommonIndent) and (J > 0) then
          CommonIndent := J;
      end;
    end;
    Result := CommonIndent;
  end;

  function InsertPos(ALine: Integer): Integer;
  begin
    if not WasSelAvail then
      Result := MinCommonIndent
    else
      case FStoredSelectionMode of
        smColumn: // CommonIndent is not used otherwise
        begin
          if CommonIndent = 0 then
            CommonIndent := Min(Editor.LogicalToPhysicalPos(FStoredBlockBegin).X,
              Editor.LogicalToPhysicalPos(FStoredBlockEnd).X);
          Result := Editor.PhysicalToLogicalPos(Point(CommonIndent, ALine)).X;
        end;
        smNormal:
        begin
          if FStoredBlockBegin.Y = FStoredBlockEnd.Y then
            Result := FStoredBlockBegin.X
          else
            Result := MinCommonIndent;
        end;
        else
          Result := 1;
      end;
  end;

  function DeletePos(ALine: Integer): Integer;
  var
    S: string;
    T: string;
    N: Integer;
  begin
    S := Lines[ALine - 1];
    N := Length(S);
    Result := FirstNonBlankPos(S, InsertPos(ALine));
    if (FStoredSelectionMode = smColumn) and ((Result < 1) or (Result > N - 1)) then
      Result := N - 1;
    Result := Max(1, Result);
    T := System.Copy(S, Result, PrefixLength);
    if (N < Result + 1) or (T <> Prefix) then
      Result := -1;
  end;

var
  I             : Integer;
  NonBlankStart : Integer;
begin
  if Settings.ReadOnly then
    Exit;

  Prefix := HighlighterItem.LineCommentTag;
  PrefixLength := Length(Prefix);

  OldCaretPos := CaretXY;
  StoreBlock;
  WasSelAvail := SelAvail;
  CommonIndent := 0;

  BlockBeginLine := FStoredBlockBegin.Y;
  BlockEndLine   := FStoredBlockEnd.Y;
  if (FStoredBlockEnd.X = 1) and (BlockEndLine > BlockBeginLine)
    and (Editor.SelectionMode <> smLine) then
    Dec(BlockEndLine);

  if AToggle then
  begin
    ACommentOn := False;
    for I := BlockBeginLine to BlockEndLine do
      if DeletePos(I) < 0 then
      begin
        ACommentOn := True;
        Break;
      end;
  end;

  BeginUpdate;
  Editor.SelectionMode := smNormal;

  if ACommentOn then
  begin
    for I := BlockEndLine downto BlockBeginLine do
      Editor.TextBetweenPoints[Point(InsertPos(I), I), Point(InsertPos(I), I)] := Prefix;
    if OldCaretPos.X > InsertPos(OldCaretPos.Y) then
      OldCaretPos.X := OldCaretPos.X + PrefixLength;
    if FStoredBlockBegin.X > InsertPos(FStoredBlockBegin.Y) then
      FStoredBlockBegin.X := FStoredBlockBegin.X + PrefixLength;
    if FStoredBlockEnd.X > InsertPos(FStoredBlockEnd.Y) then
      FStoredBlockEnd.X := FStoredBlockEnd.X + PrefixLength;
  end
  else
  begin
    for I := BlockEndLine downto BlockBeginLine do
    begin
      NonBlankStart := DeletePos(I);
      if NonBlankStart < 1 then
        continue;
      Editor.TextBetweenPoints[Point(NonBlankStart, I),
        Point(NonBlankStart + PrefixLength, I)] := '';
      if (OldCaretPos.Y = I) and (OldCaretPos.X > NonBlankStart) then
        OldCaretPos.x := Max(OldCaretPos.X - PrefixLength, NonBlankStart);
      if (FStoredBlockBegin.Y = I) and (FStoredBlockBegin.X > NonBlankStart) then
        FStoredBlockBegin.X := Max(FStoredBlockBegin.X - PrefixLength, NonBlankStart);
      if (FStoredBlockEnd.Y = I) and (FStoredBlockEnd.X > NonBlankStart) then
        FStoredBlockEnd.X := Max(FStoredBlockEnd.X - PrefixLength, NonBlankStart);
    end;
  end;

  EndUpdate;
  CaretXY       := OldCaretPos;
  RestoreBlock;
end;

procedure TEditorView.ToggleBlockCommentSelection;
var
  S  : string;
  S1 : string;
  S2 : string;
  N1 : Integer;
  N2 : Integer;
begin
  if SelAvail and (HighlighterItem.BlockCommentStartTag <> '') then
  begin
    BeginUpdate;
    StoreBlock;
    N1 := Length(HighlighterItem.BlockCommentStartTag);
    N2 := Length(HighlighterItem.BlockCommentEndTag);
    S := Trim(SelText);
    S1 := System.Copy(S, 1, N1);
    S2 := System.Copy(S, Length(S) - N2 + 1, Length(S));
    Logger.Send('S1', S1);
    Logger.Send('S2', S2);
    if (S1 = HighlighterItem.BlockCommentStartTag) and
      (S2 = HighlighterItem.BlockCommentEndTag)
    then
    begin
      SelText := System.Copy(S, N1 + 1, Length(S) - N2 - N1);
      if FStoredBlockBegin.Y = FStoredBlockEnd.Y then
        Dec(FStoredBlockEnd.X, N1 + N2)
      else
        Dec(FStoredBlockEnd.X, N2)
    end
    else
    begin
      SelText := HighlighterItem.BlockCommentStartTag + SelText
        + HighlighterItem.BlockCommentEndTag;
      if FStoredBlockBegin.Y = FStoredBlockEnd.Y then
        Inc(FStoredBlockEnd.X, N1 + N2)
      else
        Inc(FStoredBlockEnd.X, N2);
    end;
    Logger.Send('BlockBegin', BlockBegin);
    Logger.Send('BlockEnd', BlockEnd);

    Logger.Send('BlockBegin', BlockBegin);
    Logger.Send('BlockEnd', BlockEnd);
    RestoreBlock;
    Logger.Send('BlockBegin', BlockBegin);
    Logger.Send('BlockEnd', BlockEnd);
    EndUpdate;
    Modified := True;
  end;
end;

procedure TEditorView.StripMarkupFromSelection;
begin
  BeginUpdate;
  SelText := StripMarkup(SelText);
  EndUpdate;
end;

procedure TEditorView.StripCharsFromSelection(AFirst: Boolean; ALast: Boolean);
begin
  BeginUpdate;
  SelText := StripChars(SelText, AFirst, ALast);
  EndUpdate;
end;

procedure TEditorView.AlignSelection(const AToken: string; ACompressWS: Boolean;
  AInsertSpaceBeforeToken: Boolean; AInsertSpaceAfterToken: Boolean;
  AAlignInParagraphs: Boolean);
var
  SL : TStringList;
  B  : Boolean;
  S : string;
begin
  SL := TStringList.Create;
  try
    SL.Text := SelText;

    BeginUpdate;
    StoreBlock;

  //  BlockBeginLine := FStoredBlockBegin.Y;
  //BlockEndLine   := FStoredBlockEnd.Y;

  if (FStoredBlockEnd.X = 1) and (FStoredBlockEnd.Y > FStoredBlockBegin.Y)
    and (Editor.SelectionMode <> smLine) then
  begin
    Dec(FStoredBlockEnd.Y);
    B := False;
  end
  else
    B := True;
  AlignLines(
    SL,
    AToken,
    ACompressWS,
    AInsertSpaceBeforeToken,
    AInsertSpaceAfterToken
    //,     AAlignInParagraphs
  );
  FStoredBlockEnd.X := Length(SL[SL.Count - 1]) + 1;
  if B then
  begin
    S := SL.Text;
    Logger.Send('SL.Text before', S);
    S := StripLastLineEnding(S);
    Logger.Send('SL.Text after', S);
    SelText := S;

  end
  else
    SelText := SL.Text;
  //  CaretXY       := OldCaretPos;
    RestoreBlock;
    EndUpdate;
  finally
    SL.Free;
  end;
end;

procedure TEditorView.UpperCaseSelection;
begin
  if SelAvail then
  begin
    StoreBlock;
    BeginUpdate;
    SelText := UpperCase(SelText);
    RestoreBlock;
    EndUpdate;
    Modified := True;
  end;
end;

procedure TEditorView.LowerCaseSelection;
begin
  if SelAvail then
  begin
    StoreBlock;
    BeginUpdate;
    SelText := LowerCase(SelText);
    RestoreBlock;
    EndUpdate;
    Modified := True;
  end;
end;

procedure TEditorView.SearchAndSelectLine(ALineIndex: Integer; const ALine: string);
begin
  Editor.SearchReplaceEx(ALine, '', [ssoWholeWord], Point(0, ALineIndex));
end;

procedure TEditorView.SearchAndSelectText(const AText: string);
begin
  Editor.SearchReplaceEx(AText, '', [], Point(0, 0));
end;

procedure TEditorView.SelectWord;
begin
  Editor.SelectWord;
end;

{ Clears all highlighted search matches of the last search operation. }

procedure TEditorView.ClearHighlightSearch;
begin
  Editor.SetHighlightSearch('', []);
end;

procedure TEditorView.Clear;
begin
  Editor.ClearAll;
end;

procedure TEditorView.SelectAll;
begin
  if Editor.Focused then
    Editor.SelectAll
end;

{ Makes a smart selection of a block around the cursor. }

{ TODO -oTS : Make this configurable per highlighter. }

procedure TEditorView.SmartSelect;
begin
  if Assigned(HighlighterItem) then
  begin
    if HighlighterItem.Name = 'XML' then
      SelectBlockAroundCursor('>', '<', False, False)
    else if HighlighterItem.Name = 'PAS' then
      SelectBlockAroundCursor('begin', 'end', True, True)
    else if HighlighterItem.Name = 'BaltaLOG' then
      SelectBlockAroundCursor('<XMLRoot>', '</XMLRoot>', True, True);
  end;
end;

procedure TEditorView.SetHighlightSearch(const ASearch: string; AOptions: TSynSearchOptions);
begin
  Editor.SetHighlightSearch(ASearch, AOptions);
end;

procedure TEditorView.UpdateActions;
var
  B: Boolean;
begin
  inherited UpdateActions;
  B := Focused;
  if not B and Assigned(Parent) then
  begin
    if Parent.Focused then
      B := True;
  end;

  if B then
  begin
    Activate;
  end;

  if Manager.ActiveView = (Self as IEditorView) then
  begin
    Editor.Color := clWhite;
  end
  else
  begin
    if Settings.DimInactiveView then
      Editor.Color := GetHighLightColor(15329769, 10);
  end;

  if Assigned(Actions) then
    Actions.UpdateActions;

  if FUpdate then
  begin
    ShowSpecialChars := Settings.ShowControlCharacters;
    EditorFont := Settings.EditorFont;
    FUpdate := False;
  end;
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************
{$endregion}

{$region 'public methods' /fold}
//*****************************************************************************
// public methods                                                        BEGIN
//*****************************************************************************

function TEditorView.CloseQuery: Boolean;
var
  MR: TModalResult;
begin
  Result := True;
  if Modified then
  begin
    Activate;
    MR := MessageDlg(SAskSaveChanges, mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    if MR = mrYes then
    begin
      Result := Commands.SaveFile;
    end
    else if MR = mrNo then
    begin
      Result := True;
    end
    else
      Result := False;
  end;
end;

procedure TEditorView.InsertTextAtCaret(const AText: string);
begin
  Editor.InsertTextAtCaret(AText); // has implicit undoblock
end;

procedure TEditorView.LoadFromFile(const AFileName: string);
var
  S  : string;
  FS : TFileStream;
begin
  if FileExists(AFileName) then
  begin
    FileName := AFileName;
    FS := TFileStream.Create(AFileName, fmOpenRead + fmShareDenyNone);
    try
      LoadFromStream(FS);
    finally
      FreeAndNil(FS);
    end;
    LineBreakStyle := ALineBreakStyles[GuessLineBreakStyle(Text)];
    S := ExtractFileExt(AFileName);
    S := System.Copy(S, 2, Length(S));
    Logger.Send('Extension', S);
    try
      AssignHighlighterForFileType(S);
    except
      { TODO -oTS : dirty: need to fix this }
      // for an unknown reason an EAbort is raised
    end;
    Modified := False;
  end;
end;

{ The loaded text always has to be converted to UTF8 because SynEdit only
  supports UTF8. When we save we convert back to the original file encoding. }

procedure TEditorView.LoadFromStream(AStream: TStream);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromStream(AStream);
    FEncoding := GuessEncoding(SL.Text);
    if FEncoding <> EncodingUTF8 then
      Text := ConvertEncoding(SL.Text, FEncoding, EncodingUTF8)
    else
      Text := SL.Text;
    FLineBreakStyle := ALineBreakStyles[GuessLineBreakStyle(Text)];
  finally
    FreeAndNil(SL);
  end;
end;

procedure TEditorView.SaveToStream(AStream: TStream);
var
  S  : string;
begin
  if LineBreakStyle <> ALineBreakStyles[GuessLineBreakStyle(Text)] then
  begin
    Text := ChangeLineBreakStyle(Text, StrToLineBreakStyle(LineBreakStyle));
  end;
  if Length(Text) > 0 then
  begin
    S := Text;
    S := ConvertEncoding(Text, EncodingUTF8, FEncoding);
    AStream.Write(S[1], Length(S));
  end;
  Modified := False;
  Editor.MarkTextAsSaved;
end;

procedure TEditorView.SaveToFile(const AFileName: string);
var
  FS : TFileStream;
  FN : string;
begin
  FN := Utf8ToAnsi(AFileName);
  FS := TFileStream.Create(FN, fmCreate);
  try
    SaveToStream(FS);
  finally
    FreeAndNil(FS);
  end;
end;

function TEditorView.GetWordAtPosition(APosition: TPoint): string;
var
  CaretPos: TPoint;
begin
  Result := '';
  Caretpos := Editor.PhysicalToLogicalPos(APosition);
  Result := GetWordFromCaret(CaretPos);
end;

function TEditorView.GetWordFromCaret(const ACaretPos: TPoint): string;
begin
  Result := Editor.GetWordAtRowCol(ACaretPos);
end;

//*****************************************************************************
// public methods                                                          END
//*****************************************************************************
{$endregion}

//*****************************************************************************

end.

 (*
procedure TFileDialogWrapper.AssignFileTypes;
var
  I, J: Integer;
  FilterStr: string;
begin
  FilterStr := FOpenDialog.Filter;
  J := 1;
  I := AnsiPos('|', FilterStr);
  while I <> 0 do
    with FFileDialog.Filetypes.Add do
    begin
      DisplayName := Copy(FilterStr, J, I - J);
      if not SysLocale.FarEast then
        J := PosEx('|', FilterStr, I + 1)
      else
      begin
        J := AnsiPos('|', Copy(FilterStr, I + 1, MAXINT));
        if J <> 0 then J := J + (I + 1) - 1;
      end;
      if J = 0 then J := Length(FilterStr) + 1;
      FileMask := Copy(FilterStr, I + 1, J - I - 1);
      Inc(J);

    end;
end;

*)

{$region 'Keyboard shortcuts' /fold}
(*//F1                      Topic Search
//Ctrl+F1                Topic Search
  ecNextEditor: SetResult(VK_F6,[]);
  ecPrevEditor: SetResult(VK_F6,[ssShift]);
  ecWordLeft:   SetResult(VK_A,[ssCtrl],VK_LEFT,[ssCtrl]);
  ecPageDown:   SetResult(VK_C,[ssCtrl],VK_NEXT,[]);
//Ctrl+D                 Moves the cursor right one column, accounting for the
//autoindent setting
//Ctrl+E                 Moves the cursor up one line
//Ctrl+F                 Moves one word right
//Ctrl+G                 Deletes the character to the right of the cursor
//Ctrl+H                 Deletes the character to the left of the cursor
//Ctrl+I                  Inserts a tab
//Ctrl+L                 Search|Search Again
//Ctrl+N                 Inserts a new line
//Ctrl+P                 Causes next character to be interpreted as an ASCII
//sequence
//Ctrl+R                 Moves up one screen
//Ctrl+S                 Moves the cursor left one column, accounting for the
//autoindent setting
//Ctrl+T                 Deletes a word
//Ctrl+V                 Turns insert mode on/off
//Ctrl+W                Moves down one screen
//Ctrl+X                 Moves the cursor down one line
//Ctrl+Y                 Deletes a line
//Ctrl+Z                 Moves the cursor up one line
//Ctrl+Shift+S          Performs an incremental search

//Block commands:
//---------------
//Ctrl+K+B      Marks the beginning of a block
//Ctrl+K+C      Copies a selected block
//Ctrl+K+H      Hides/shows a selected block
//Ctrl+K+I       Indents a block by the amount specified in the Block Indent
//combo box on the General page of the Editor Options dialog box.
//Ctrl+K+K      Marks the end of a block
//Ctrl+K+L       Marks the current line as a block
//Ctrl+K+N      Changes a block to uppercase
//Ctrl+K+O      Changes a block to lowercase
//Ctrl+K+P      Prints selected block
//Ctrl+K+R      Reads a block from a file
//Ctrl+K+T       Marks a word as a block
//Ctrl+K+U      Outdents a block by the amount specified in the Block Indent
//combo box on the General page of the Editor Options dialog box.
//Ctrl+K+V      Moves a selected block
//Ctrl+K+W      Writes a selected block to a file
//Ctrl+K+Y      Deletes a selected block
//Ctrl+O+C      Turns on column blocking
//Ctrl+O+I       Marks an inclusive block
//Ctrl+O+K      Turns off column blocking
//Ctrl+O+L      Marks a line as a block
//Shift+Alt+arrow Selects column-oriented blocks
//Click+Alt+mousemv Selects column-oriented blocks
//Ctrl+Q+B      Moves to the beginning of a block
//Ctrl+Q+K      Moves to the end of a block

//Miscellaneous commands:
//-----------------------
//Ctrl+K+D      Accesses the menu bar
//Ctrl+K+E       Changes a word to lowercase
//Ctrl+K+F       Changes a word to uppercase
//Ctrl+K+S      File|Save (Default and IDE Classic only)
//Ctrl+Q+A      Search|Replace
//Ctrl+Q+F      Search|Find
//Ctrl+Q+Y      Deletes to the end of a line
//Ctrl+Q+[       Finds the matching delimiter (forward)
//Ctrl+Q+Ctrl+[ Finds the matching delimiter (forward)
//Ctrl+Q+]       Finds the matching delimiter (backward)
//Ctrl+Q+Ctrl+] Finds the matching delimiter (backward)
//Ctrl+O+A      Open file at cursor
//Ctrl+O+B      Browse symbol at cursor (Delphi only)
//Alt+right arrow  For code browsing
//Alt +left arrow For code browsing
//Ctrl+O+G      Search|Go to line number
//Ctrl+O+O      Inserts compiler options and directives
//Ctrl+O+U      Toggles case
//Bookmark commands:
//------------------
//Shortcut       Action
//Ctrl+K+0       Sets bookmark 0
//Ctrl+K+1       Sets bookmark 1
//Ctrl+K+2       Sets bookmark 2
//Ctrl+K+3       Sets bookmark 3
//Ctrl+K+4       Sets bookmark 4
//Ctrl+K+5       Sets bookmark 5
//Ctrl+K+6       Sets bookmark 6
//Ctrl+K+7       Sets bookmark 7
//Ctrl+K+8       Sets bookmark 8
//Ctrl+K+9       Sets bookmark 9
//Ctrl+K+Ctrl+0 Sets bookmark 0
//Ctrl+K+Ctrl+1 Sets bookmark 1
//Ctrl+K+Ctrl+2 Sets bookmark 2
//Ctrl+K+Ctrl+3 Sets bookmark 3
//Ctrl+K+Ctrl+4 Sets bookmark 4
//Ctrl+K+Ctrl+5 Sets bookmark 5
//Ctrl+K+Ctrl+6 Sets bookmark 6
//Ctrl+K+Ctrl+7 Sets bookmark 7
//Ctrl+K+Ctrl+8 Sets bookmark 8
//Ctrl+K+Ctrl+9 Sets bookmark 9
//Ctrl+Q+0       Goes to bookmark 0
//Ctrl+Q+1       Goes to bookmark 1
//Ctrl+Q+2       Goes to bookmark 2
//Ctrl+Q+3       Goes to bookmark 3
//Ctrl+Q+4       Goes to bookmark 4
//Ctrl+Q+5       Goes to bookmark 5
//Ctrl+Q+6       Goes to bookmark 6
//Ctrl+Q+7       Goes to bookmark 7
//Ctrl+Q+8       Goes to bookmark 8
//Ctrl+Q+9       Goes to bookmark 9
//Ctrl+Q+Ctrl+0 Goes to bookmark 0
//Ctrl+Q+Ctrl+1 Goes to bookmark 1
//Ctrl+Q+Ctrl+2 Goes to bookmark 2
//Ctrl+Q+Ctrl+3 Goes to bookmark 3
//Ctrl+Q+Ctrl+4 Goes to bookmark 4
//Ctrl+Q+Ctrl+5 Goes to bookmark 5
//Ctrl+Q+Ctrl+6 Goes to bookmark 6
//Ctrl+Q+Ctrl+7 Goes to bookmark 7
//Ctrl+Q+Ctrl+8 Goes to bookmark 8
//Ctrl+Q+Ctrl+9 Goes to bookmark 9
//Cursor movement:
//----------------
//Ctrl+Q+B      Moves to the beginning of a block
//Ctrl+Q+C      Moves to end of a file
//Ctrl+Q+D      Moves to the end of a line
//Ctrl+Q+E      Moves the cursor to the top of the window
//Ctrl+Q+K      Moves to the end of a block
//Ctrl+Q+P      Moves to previous position
//Ctrl+Q+R      Moves to the beginning of a file
//Ctrl+Q+S      Moves to the beginning of a line
//Ctrl+Q+T      Moves the viewing editor so that the current line is placed at
//the top of the window
//Ctrl+Q+U      Moves the viewing editor so that the current line is placed at
//the bottom of the window, if possible
//Ctrl+Q+X      Moves the cursor to the bottom of the window
//System keys:
//------------

//F1              Displays context-sensitive Help
//F2              File|Save
//F3              File|Open
//F4              Run to Cursor
//F5              Zooms window
//F6              Displays the next page
//F7              Run|Trace Into
//F8              Run|Step Over
//F9              Run|Run
//F11             View|Object Inspector
//F12             View|Toggle Form/Unit
//Alt+0           View|Window List
//Alt+F2          View|CPU
//Alt+F3          File|Close
//Alt+F7          Displays previous error in Message view
//Alt+F8          Displays next error in Message view
//Alt+F11        File|Use Unit (Delphi)
//Alt+F11        File|Include Unit Hdr (C++)
//Alt+F12        Displays the Code editor
//Alt+X           File|Exit
//Alt+right arrow  For code browsing forward
//Alt +left arrow For code browsing backward
//Alt +up arrow  For code browsing Ctrl-click on identifier
//Alt+Page Down Goes to the next tab
//Alt+Page Up   Goes to the previous tab
//Ctrl+F1        Topic Search
//Ctrl+F2        Run|Program Reset
//Ctrl+F3        View|Call Stack
//Ctrl+F6        Open Source/Header file (C++)
//Ctrl+F7        Add Watch at Cursor
//Ctrl+F8        Toggle Breakpoint
//Ctrl+F9        Project|Compile project (Delphi)
//Ctrl+F9        Project|Make project (C++)
//Ctrl+F11       File|Open Project
//Ctrl+F12       View|Units
//Shift+F7       Run|Trace To Next Source Line
//Shift+F11      Project|Add To Project
//Shift+F12      View|Forms
//Ctrl+D         Descends item (replaces Inspector window)
//Ctrl+N         Opens a new Inspector window
//Ctrl+S          Incremental search
//Ctrl+T          Displays the Type Cast dialog
  else
    GetDefaultKeyForCommand(Command,TheKeyA,TheKeyB);
  end;
*)
{$endregion}
