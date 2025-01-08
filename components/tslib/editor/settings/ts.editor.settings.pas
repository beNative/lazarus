{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

unit ts.Editor.Settings;

{$MODE DELPHI}

{$REGION 'documentation'}
{
  TEditorSettings is a component that implements IEditorSettings and holds
  all settings that can be persisted.

  All published properties are streamed from/to a Json file in a similar way as
  components are stored in a DFM/LFM file so there is no special code needed
  that handles the storage and retrieval of properties.

  Reading qnd writing of the settings is handled completely by these methods,
  which utilize the fpjsonrtti object storage mechanism.
    procedure Load;
    procedure Save;
}
{$ENDREGION}

interface

uses
  Classes, SysUtils, Graphics, FileUtil, TypInfo,

  fpjson, fpjsonrtti,

  LazMethodList,

  SynEditMiscClasses,

  ts.Core.FormSettings,

  ts.Editor.Interfaces, ts.Editor.Highlighters, ts.Editor.HighlighterAttributes,
  ts.Editor.Colors.Settings,

  ts.Editor.AlignLines.Settings, ts.Editor.CodeFilter.Settings,
  ts.Editor.CodeShaper.Settings, ts.Editor.HexEditor.Settings,
  ts.Editor.MiniMap.Settings, ts.Editor.Options.Settings,
  ts.Editor.SortStrings.Settings, ts.Editor.Search.Engine.Settings,

  ts.Editor.CodeTags,

  ts.Core.Logger;

const
  DEFAULT_AUTO_GUESS_HIGHLIGHTER_TYPE = True;
  DEFAULT_AUTO_FORMAT_XML             = True;
  DEFAULT_RIGHT_EDGE                  = 80;
  DEFAULT_DIM_ACTIVE_VIEW             = True;
  DEFAULT_SINGLE_INSTANCE             = False;
  DEFAULT_LANGUAGE_CODE               = 'en';
  DEFAULT_FONT_NAME                   = 'Consolas';
  DEFAULT_SETTINGS_FILE               = 'settings.json';

type
  TEditorSettings = class(TComponent, IEditorSettings)
  private
    FAutoFormatXml            : Boolean;
    FChangedEventList         : TMethodList;
    FReadOnly                 : Boolean;
    FHighlighterType          : string;
    FAutoGuessHighlighterType : Boolean;
    FCloseWithEsc             : Boolean;
    FDebugMode                : Boolean;
    FSingleInstance           : Boolean;
    FFileName                 : string;
    FLanguageCode             : string;
    FHighlighters             : THighLighters;
    FDimInactiveView          : Boolean;
    FFormSettings             : TFormSettings;
    FEditorFont               : TFont;
    FHighlighterAttributes    : THighlighterAttributes;
    FColors                   : TEditorColorSettings;
    FEditorOptions            : TEditorOptionsSettings;
    FAlignLinesSettings       : TAlignLinesSettings;
    FCodeFilterSettings       : TCodeFilterSettings;
    FCodeShaperSettings       : TCodeShaperSettings;
    FHexEditorSettings        : THexEditorSettings;
    FMiniMapSettings          : TMiniMapSettings;
    FSortStringsSettings      : TSortStringsSettings;
    FSearchEngineSettings     : TSearchEngineSettings;

    FDeStreamer : TJSONDeStreamer;
    FStreamer   : TJSONStreamer;

    procedure FDeStreamerBeforeReadObject(
      Sender  : TObject;
      AObject : TObject;
      JSON    : TJSONObject
    );
    procedure FDeStreamerRestoreProperty(
      Sender      : TObject;
      AObject     : TObject;
      Info        : PPropInfo;
      AValue      : TJSONData;
      var Handled : Boolean
    );
    procedure FEditorOptionsChanged(Sender: TObject);
    procedure FFormSettingsChanged(Sender: TObject);
    procedure FColorsChanged(Sender: TObject);

    {$REGION 'property access methods'}
    function GetAlignLinesSettings: TAlignLinesSettings;
    function GetAutoFormatXml: Boolean;
    function GetAutoGuessHighlighterType: Boolean;
    function GetCloseWithEsc: Boolean;
    function GetCodeFilterSettings: TCodeFilterSettings;
    function GetCodeShaperSettings: TCodeShaperSettings;
    function GetColors: TEditorColorSettings;
    function GetDebugMode: Boolean;
    function GetDimInactiveView: Boolean;
    function GetEditorFont: TFont;
    function GetEditorOptions: TEditorOptionsSettings;
    function GetFileName: string;
    function GetFormSettings: TFormSettings;
    function GetHexEditorSettings: THexEditorSettings;
    function GetHighlighterAttributes: THighlighterAttributes;
    function GetHighlighters: THighlighters;
    function GetHighlighterType: string;
    function GetLanguageCode: string;
    function GetMiniMapSettings: TMiniMapSettings;
    function GetReadOnly: Boolean;
    function GetSearchEngineSettings: TSearchEngineSettings;
    function GetSingleInstance: Boolean;
    function GetSortStringsSettings: TSortStringsSettings;
    procedure SetAlignLinesSettings(AValue: TAlignLinesSettings);
    procedure SetAutoFormatXml(const AValue: Boolean);
    procedure SetAutoGuessHighlighterType(const AValue: Boolean);
    procedure SetCloseWithEsc(const AValue: Boolean);
    procedure SetCodeFilterSettings(AValue: TCodeFilterSettings);
    procedure SetCodeShaperSettings(AValue: TCodeShaperSettings);
    procedure SetColors(AValue: TEditorColorSettings);
    procedure SetDebugMode(AValue: Boolean);
    procedure SetDimInactiveView(const AValue: Boolean);
    procedure SetEditorFont(AValue: TFont);
    procedure SetEditorOptions(AValue: TEditorOptionsSettings);
    procedure SetFileName(const AValue: string);
    procedure SetFormSettings(const AValue: TFormSettings);
    procedure SetHexEditorSettings(AValue: THexEditorSettings);
    procedure SetHighlighterAttributes(AValue: THighlighterAttributes);
    procedure SetHighlighters(const AValue: THighlighters);
    procedure SetHighlighterType(const AValue: string);
    procedure SetLanguageCode(AValue: string);
    procedure SetMiniMapSettings(AValue: TMiniMapSettings);
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetSearchEngineSettings(AValue: TSearchEngineSettings);
    procedure SetSingleInstance(AValue: Boolean);
    procedure SetSortStringsSettings(AValue: TSortStringsSettings);
    {$ENDREGION}

  protected
    procedure RegisterClasses;
    procedure RegisterHighlighters(AHighlighters: THighlighters);
    procedure InitializeFoldHighlighters(AHighlighters: THighlighters);
    procedure InitializeHighlighterAttributes;
    procedure InitializeHighlighters;
    procedure AssignDefaultColors;
    procedure AssignDefaultHighlighterAttibutesValues;

    procedure Changed;

    procedure AddEditorSettingsChangedHandler(AEvent: TNotifyEvent);
    procedure RemoveEditorSettingsChangedHandler(AEvent: TNotifyEvent);

    procedure LoadJson;
    procedure SaveJson;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    procedure Apply; // to manually force a notification

    procedure Load; virtual;
    procedure Save; virtual;

    property FileName: string
      read GetFileName write SetFileName;

    property Highlighters: THighlighters
      read GetHighlighters write SetHighlighters;

    property Name;
    property Tag;

  published
    property AlignLinesSettings: TAlignLinesSettings
      read GetAlignLinesSettings write SetAlignLinesSettings;

    property CodeFilterSettings: TCodeFilterSettings
      read GetCodeFilterSettings write SetCodeFilterSettings;

    property CodeShaperSettings: TCodeShaperSettings
      read GetCodeShaperSettings write SetCodeShaperSettings;

    property HexEditorSettings: THexEditorSettings
      read GetHexEditorSettings write SetHexEditorSettings;

    property MiniMapSettings: TMiniMapSettings
      read GetMiniMapSettings write SetMiniMapSettings;

    property SortStringsSettings: TSortStringsSettings
      read GetSortStringsSettings write SetSortStringsSettings;

    property SearchEngineSettings: TSearchEngineSettings
      read GetSearchEngineSettings write SetSearchEngineSettings;

    { Common styling attributes to apply to all supported highlighters. }
    property HighlighterAttributes: THighlighterAttributes
      read GetHighlighterAttributes write SetHighlighterAttributes;

    property Colors: TEditorColorSettings
      read GetColors write SetColors;

    property EditorOptions: TEditorOptionsSettings
      read GetEditorOptions write SetEditorOptions;

    { Default highlighter type to use when a new editor view is created. }
    property HighlighterType: string
      read GetHighlighterType write SetHighlighterType;

    property LanguageCode: string
      read GetLanguageCode write SetLanguageCode;

    property ReadOnly: Boolean
      read GetReadOnly write SetReadOnly default False;

    property DimInactiveView: Boolean
      read GetDimInactiveView write SetDimInactiveView
      default DEFAULT_DIM_ACTIVE_VIEW;

    property AutoFormatXml: Boolean
      read GetAutoFormatXml write SetAutoFormatXml
      default DEFAULT_AUTO_FORMAT_XML;

    property AutoGuessHighlighterType: Boolean
      read GetAutoGuessHighlighterType write SetAutoGuessHighlighterType
      default DEFAULT_AUTO_GUESS_HIGHLIGHTER_TYPE;

    { Determines if the application can be closed with the ESCAPE key. }
    property CloseWithEsc: Boolean
      read GetCloseWithEsc write SetCloseWithEsc default False;

    property FormSettings: TFormSettings
      read GetFormSettings write SetFormSettings;

    property EditorFont: TFont
      read GetEditorFont write SetEditorFont;

    property DebugMode: Boolean
      read GetDebugMode write SetDebugMode default False;

    property SingleInstance: Boolean
      read GetSingleInstance write SetSingleInstance
      default DEFAULT_SINGLE_INSTANCE;
  end;

implementation

uses
  SynEditStrConst, SynEditTypes, SynEditHighlighter, SynEditHighlighterFoldBase,
  SynHighlighterPas, SynHighlighterSQL, SynHighlighterLFM, SynHighlighterXML,
  SynHighlighterBat, SynHighlighterHTML, SynHighlighterCpp, SynHighlighterJava,
  SynHighlighterPerl, SynHighlighterPython, SynHighlighterPHP, SynHighlighterCss,
  SynHighlighterJScript, SynHighlighterDiff, SynHighlighterTeX, SynHighlighterPo,
  SynhighlighterUnixShellScript, SynHighlighterIni, SynHighlighterLua,
  SynHighlighterPike, SynHighlighterVB, SynHighlighterCS, SynHighlighterRC,
  SynHighlighterRuby, SynHighlighterInno, SynHighlighterJSON,

  ts.Editor.CodeFormatters, ts.Editor.CodeFormatters.SQL,

  ts.Core.Utils,

  ts.Editor.Resources;

{$REGION 'construction and destruction'}
procedure TEditorSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  RegisterClasses;
  FComponentStyle          := [csSubComponent];
  FChangedEventList        := TMethodList.Create;
  FFormSettings            := TFormSettings.Create;
  FFormSettings.OnChanged  := FFormSettingsChanged;
  FEditorOptions           := TEditorOptionsSettings.Create;
  FEditorOptions.OnChanged := FEditorOptionsChanged;
  FColors                  := TEditorColorSettings.Create;
  FColors.OnChanged        := FColorsChanged;
  AssignDefaultColors;

  FCodeFilterSettings   := TCodeFilterSettings.Create;
  FSortStringsSettings  := TSortStringsSettings.Create;
  FAlignLinesSettings   := TAlignLinesSettings.Create;
  FCodeShaperSettings   := TCodeShaperSettings.Create;
  FHexEditorSettings    := THexEditorSettings.Create;
  FMiniMapSettings      := TMiniMapSettings.Create;
  FSearchEngineSettings := TSearchEngineSettings.Create;

  FStreamer         := TJSONStreamer.Create(Self);
  FStreamer.Options :=  FStreamer.Options + [jsoComponentsInline];  //jsoStreamChildren,
  FDeStreamer         := TJSONDeStreamer.Create(Self);
  FDeStreamer.Options := FDeStreamer.Options + [jdoIgnorePropertyErrors];
  FDeStreamer.BeforeReadObject  := FDeStreamerBeforeReadObject;
  FDeStreamer.OnRestoreProperty := FDeStreamerRestoreProperty;

  FHighlighters      := THighLighters.Create(Self);
  FHighlighters.Name := 'Highlighters';
  InitializeHighlighters;

  FHighlighterAttributes := THighlighterAttributes.Create(Self);
  AssignDefaultHighlighterAttibutesValues;

  FFileName                 := DEFAULT_SETTINGS_FILE;
  FHighlighterType          := HL_TXT;
  FAutoFormatXml            := DEFAULT_AUTO_FORMAT_XML;
  FAutoGuessHighlighterType := DEFAULT_AUTO_GUESS_HIGHLIGHTER_TYPE;
  FSingleInstance           := DEFAULT_SINGLE_INSTANCE;
  FEditorFont               := TFont.Create;
  FEditorFont.Name          := DEFAULT_FONT_NAME;
  FEditorFont.Size          := 10;

  FDimInactiveView := DEFAULT_DIM_ACTIVE_VIEW;
  FLanguageCode    := DEFAULT_LANGUAGE_CODE;
end;

destructor TEditorSettings.Destroy;
begin
  FAlignLinesSettings.Free;
  FCodeFilterSettings.Free;
  FCodeShaperSettings.Free;
  FHexEditorSettings.Free;
  FSortStringsSettings.Free;
  FMiniMapSettings.Free;
  FSearchEngineSettings.Free;
  FColors.Free;
  FEditorOptions.Free;
  FFormSettings.Free;
  FHighlighters.Free;
  FEditorFont.Free;
  FHighlighterAttributes.Free;
  FChangedEventList.Free;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TEditorSettings.FColorsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TEditorSettings.FEditorOptionsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TEditorSettings.FDeStreamerRestoreProperty(Sender: TObject;
  AObject: TObject; Info: PPropInfo; AValue: TJSONData; var Handled: Boolean);
begin
  Logger.Send('AValueType', GetEnumName(TypeInfo(AValue.JSONType), Integer(AValue.JSONType)));
end;

procedure TEditorSettings.FDeStreamerBeforeReadObject(Sender: TObject;
  AObject: TObject; JSON: TJSONObject);
begin
  Logger.SendText(AObject.ClassName, JSON.FormatJSON);
end;

procedure TEditorSettings.FFormSettingsChanged(Sender: TObject);
begin
  Changed;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TEditorSettings.GetAlignLinesSettings: TAlignLinesSettings;
begin
  Result := FAlignLinesSettings;
end;

function TEditorSettings.GetAutoFormatXml: Boolean;
begin
  Result := FAutoFormatXml;
end;

procedure TEditorSettings.SetAutoFormatXml(const AValue: Boolean);
begin
  if AValue <> AutoFormatXml then
  begin
    FAutoFormatXml := AValue;
  end;
end;

function TEditorSettings.GetAutoGuessHighlighterType: Boolean;
begin
  Result := FAutoGuessHighlighterType;
end;

procedure TEditorSettings.SetAutoGuessHighlighterType(const AValue: Boolean);
begin
  if AValue <> AutoGuessHighlighterType then
  begin
    FAutoGuessHighlighterType := AValue;
  end;
end;

function TEditorSettings.GetCloseWithEsc: Boolean;
begin
  Result := FCloseWithEsc;
end;

function TEditorSettings.GetCodeFilterSettings: TCodeFilterSettings;
begin
  Result := FCodeFilterSettings;
end;

function TEditorSettings.GetCodeShaperSettings: TCodeShaperSettings;
begin
  Result := FCodeShaperSettings;
end;

procedure TEditorSettings.SetCloseWithEsc(const AValue: Boolean);
begin
  if AValue <> CloseWithEsc then
  begin
    FCloseWithEsc := AValue;
  end;
end;

procedure TEditorSettings.SetCodeFilterSettings(AValue: TCodeFilterSettings);
begin
  FCodeFilterSettings.Assign(AValue);
  Changed;
end;

procedure TEditorSettings.SetCodeShaperSettings(AValue: TCodeShaperSettings);
begin
  FCodeShaperSettings.Assign(AValue);
  Changed;
end;

function TEditorSettings.GetColors: TEditorColorSettings;
begin
  Result := FColors;
end;

procedure TEditorSettings.SetColors(AValue: TEditorColorSettings);
begin
  FColors.Assign(AValue);
  Changed;
end;

function TEditorSettings.GetDebugMode: Boolean;
begin
  Result := FDebugMode;
end;

procedure TEditorSettings.SetDebugMode(AValue: Boolean);
begin
  if AValue <> DebugMode then
  begin
    FDebugMode := AValue;
    Changed;
  end;
end;

function TEditorSettings.GetDimInactiveView: Boolean;
begin
  Result := FDimInactiveView;
end;

procedure TEditorSettings.SetDimInactiveView(const AValue: Boolean);
begin
  if AValue <> DimInactiveView then
  begin
    FDimInactiveView := AValue;
    Changed;
  end;
end;

function TEditorSettings.GetEditorFont: TFont;
begin
  Result := FEditorFont;
end;

procedure TEditorSettings.SetEditorFont(AValue: TFont);
begin
  if not FEditorFont.IsEqual(AValue) then
  begin
    FEditorFont.Assign(AValue);
    Changed;
  end;
end;

function TEditorSettings.GetFileName: string;
begin
  Result := FFileName;
end;

procedure TEditorSettings.SetFileName(const AValue: string);
begin
  if AValue <> FileName then
  begin
    FFileName := AValue;
  end;
end;

function TEditorSettings.GetFormSettings: TFormSettings;
begin
  Result := FFormSettings;
end;

function TEditorSettings.GetHexEditorSettings: THexEditorSettings;
begin
  Result := FHexEditorSettings;
end;

procedure TEditorSettings.SetFormSettings(const AValue: TFormSettings);
begin
  FFormSettings.Assign(AValue);
  Changed;
end;

procedure TEditorSettings.SetHexEditorSettings(AValue: THexEditorSettings);
begin
  FHexEditorSettings.Assign(AValue);
  Changed;
end;

function TEditorSettings.GetHighlighterAttributes: THighlighterAttributes;
begin
  Result := FHighlighterAttributes;
end;

procedure TEditorSettings.SetHighlighterAttributes(AValue: THighlighterAttributes);
begin
  FHighlighterAttributes.Assign(AValue);
  Changed;
end;

function TEditorSettings.GetHighlighters: THighlighters;
begin
  Result := FHighlighters;
end;

procedure TEditorSettings.SetHighlighters(const AValue: THighlighters);
begin
  FHighlighters.Assign(AValue);
  Changed;
end;

function TEditorSettings.GetHighlighterType: string;
begin
  Result := FHighlighterType;
end;

procedure TEditorSettings.SetHighlighterType(const AValue: string);
begin
  if AValue <> HighlighterType then
  begin
    FHighlighterType := AValue;
    Changed;
  end;
end;

function TEditorSettings.GetLanguageCode: string;
begin
  Result := FLanguageCode;
end;

procedure TEditorSettings.SetLanguageCode(AValue: string);
begin
  if AValue <> LanguageCode then
  begin
    FLanguageCode := AValue;
    Changed;
  end;
end;

function TEditorSettings.GetEditorOptions: TEditorOptionsSettings;
begin
  Result := FEditorOptions;
end;

function TEditorSettings.GetMiniMapSettings: TMiniMapSettings;
begin
  Result := FMiniMapSettings;
end;

procedure TEditorSettings.SetEditorOptions(AValue: TEditorOptionsSettings);
begin
  FEditorOptions.Assign(AValue);
  Changed;
end;

procedure TEditorSettings.SetMiniMapSettings(AValue: TMiniMapSettings);
begin
  FMiniMapSettings.Assign(AValue);
  Changed;
end;

function TEditorSettings.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

function TEditorSettings.GetSearchEngineSettings: TSearchEngineSettings;
begin
  Result := FSearchEngineSettings;
end;

procedure TEditorSettings.SetReadOnly(const AValue: Boolean);
begin
  if AValue <> ReadOnly then
  begin
    FReadOnly := AValue;
    Changed;
  end;
end;

procedure TEditorSettings.SetSearchEngineSettings(AValue: TSearchEngineSettings
  );
begin
  FSearchEngineSettings.Assign(AValue);
  Changed;
end;

function TEditorSettings.GetSingleInstance: Boolean;
begin
  Result := FSingleInstance;
end;

function TEditorSettings.GetSortStringsSettings: TSortStringsSettings;
begin
  Result := FSortStringsSettings;
end;

procedure TEditorSettings.SetSingleInstance(AValue: Boolean);
begin
  if AValue <> SingleInstance then
  begin
    FSingleInstance := AValue;
    // we need to Save here to make sure that any other instance runs with the
    // same configuration.
    Save;
    Changed;
  end;
end;

procedure TEditorSettings.SetSortStringsSettings(AValue: TSortStringsSettings);
begin
  FSortStringsSettings.Assign(AValue);
  Changed;
end;

procedure TEditorSettings.SetAlignLinesSettings(AValue: TAlignLinesSettings);
begin
  FAlignLinesSettings.Assign(AValue);
  Changed;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TEditorSettings.AssignDefaultColors;
begin
  Colors.BracketMatchColor.Background := clAqua;
  Colors.BracketMatchColor.Foreground := clNone;
  Colors.BracketMatchColor.FrameColor := clBlue;

  Colors.SelectedColor.Background := clMedGray;
  Colors.SelectedColor.BackAlpha  := 128;
  Colors.SelectedColor.Foreground := clNone;

  Colors.IncrementColor.Background := clMedGray;
  Colors.IncrementColor.BackAlpha  := 128;
  Colors.IncrementColor.Foreground := clNone;

  Colors.HighlightAllColor.Background := $000080FF; // orange
  Colors.HighlightAllColor.BackAlpha  := 128;
  Colors.HighlightAllColor.Foreground := clNone;
  Colors.HighlightAllColor.FrameColor := $00006BD7; // dark orange

  Colors.LineHighlightColor.Background := clYellow;
  Colors.LineHighlightColor.BackAlpha  := 128;
  Colors.LineHighlightColor.Foreground := clNone;
  Colors.LineHighlightColor.FrameColor := clOlive;
  Colors.LineHighlightColor.FrameAlpha := 64;
  Colors.LineHighlightColor.FrameStyle := slsDashed;

  Colors.FoldedCodeColor.Background := clNone;
  Colors.FoldedCodeColor.BackAlpha  := 128;
  Colors.FoldedCodeColor.Foreground := clMedGray;
  Colors.FoldedCodeColor.FrameColor := clMedGray;
end;

procedure TEditorSettings.AssignDefaultHighlighterAttibutesValues;
var
  LAttributes : TSynHighlighterAttributes;
begin
  if FHighlighterAttributes.Count = 0 then
    InitializeHighlighterAttributes;
  with FHighlighterAttributes do
  begin
    LAttributes := ItemsByName[SYNS_XML_AttrComment].Attributes;
    LAttributes.Foreground := clGray;
    LAttributes := ItemsByName[SYNS_XML_AttrString].Attributes;
    LAttributes.Foreground := clGreen;
    LAttributes.Style      := [fsBold];
    LAttributes := ItemsByName[SYNS_XML_AttrNumber].Attributes;
    LAttributes.Foreground := clRed;
    LAttributes.Style      := [fsBold];
    LAttributes := ItemsByName[SYNS_XML_AttrKey].Attributes;
    LAttributes.Foreground := clBlue;
    LAttributes.Style      := [fsBold];

    LAttributes := ItemsByName[SYNS_XML_AttrSymbol].Attributes;
    LAttributes.Foreground := clRed;
    LAttributes.Style      := [fsBold];

    LAttributes := ItemsByName[SYNS_XML_AttrFloat].Attributes;
    LAttributes.Foreground := clFuchsia;
    LAttributes.Style      := [fsBold];

    LAttributes := ItemsByName[SYNS_XML_AttrReservedWord].Attributes;
    LAttributes.Foreground := clBlue;
    LAttributes.Style      := [fsBold];

    LAttributes := ItemsByName[SYNS_XML_AttrDirective].Attributes;
    LAttributes.Foreground := clOlive;
    LAttributes.Style      := [fsBold];

    LAttributes := ItemsByName[SYNS_XML_AttrOperator].Attributes;
    LAttributes.Foreground := clRed;
    LAttributes.Style      := [fsBold];

    LAttributes := ItemsByName[SYNS_XML_AttrVariable].Attributes;
    LAttributes.Foreground := clRed;
    LAttributes.Style      := [fsBold];

    LAttributes := ItemsByName[SYNS_XML_AttrNull].Attributes;
    LAttributes.Foreground := clRed;
    LAttributes.Style      := [fsBold];

    LAttributes := ItemsByName[SYNS_XML_AttrOperator].Attributes;
    LAttributes.Foreground := clRed;
    LAttributes.Style      := [fsBold];

    LAttributes := ItemsByName[SYNS_XML_AttrAttributeName].Attributes;
    LAttributes.Foreground := clMaroon;
    LAttributes.Style      := [fsBold];

    LAttributes := ItemsByName[SYNS_XML_AttrAttributeValue].Attributes;
    LAttributes.Foreground := clGreen;
    LAttributes.Style      := [fsBold];

    LAttributes := ItemsByName[SYNS_XML_AttrCharacter].Attributes;
    LAttributes.Foreground := clGreen;
    LAttributes.Style      := [fsBold];

    LAttributes := ItemsByName[SYNS_XML_AttrMacro].Attributes;
    LAttributes.Foreground := clTeal;
    LAttributes.Style      := [fsBold];

    LAttributes := ItemsByName[SYNS_XML_AttrText].Attributes;
    LAttributes.Foreground := clBlack;
    LAttributes.Style      := [];

    LAttributes := ItemsByName[SYNS_XML_AttrSection].Attributes;
    LAttributes.Foreground := clGray;
    LAttributes.Style      := [fsBold];

    LAttributes := ItemsByName[SYNS_XML_AttrDataType].Attributes;
    LAttributes.Foreground := clTeal;
    LAttributes.Style      := [fsBold];
  end;
  Apply;
end;

procedure TEditorSettings.Changed;
begin
  FChangedEventList.CallNotifyEvents(Self);
end;

procedure TEditorSettings.AddEditorSettingsChangedHandler(AEvent: TNotifyEvent);
begin
  FChangedEventList.Add(TMethod(AEvent));
end;

procedure TEditorSettings.RemoveEditorSettingsChangedHandler(AEvent: TNotifyEvent);
begin
  FChangedEventList.Remove(TMethod(AEvent));
end;

{ TODO -oTS : Refactor this }

{ Registers common highlighter attributes to share settings for multiple
  highlighters. }

procedure TEditorSettings.InitializeHighlighterAttributes;
begin
  with FHighlighterAttributes do
  begin
    // Comment
    RegisterItem(
      SYNS_XML_AttrComment, [
        SYNS_XML_AttrComment,
        SYNS_XML_AttrDocumentation,
        SYNS_XML_AttrRplComment,
        SYNS_XML_AttrSASMComment
      ]
    );

    {$REGION 'literals'}
    // String literal
    RegisterItem(
      SYNS_XML_AttrString, [
        SYNS_XML_AttrString,
        SYNS_XML_AttrEmbedSQL,
        SYNS_XML_AttrEmbedText
      ]
    );
    RegisterItem(
      SYNS_XML_AttrCharacter, [
        SYNS_XML_AttrCharacter
      ]
    );
    // Integer number literal
    RegisterItem(
      SYNS_XML_AttrNumber, [
        SYNS_XML_AttrNumber
      ]
    );
    // Float literal
    RegisterItem(
      SYNS_XML_AttrFloat, [
        SYNS_XML_AttrFloat
      ]
    );
    // Hex literal
    RegisterItem(
      SYNS_XML_AttrHexadecimal, [
        SYNS_XML_AttrHexadecimal
      ]
    );
    // Octal
    RegisterItem(
      SYNS_XML_AttrOctal, [
        SYNS_XML_AttrOctal
      ]
    );
    {$ENDREGION}

    // Symbol
    RegisterItem(
      SYNS_XML_AttrSymbol, [
        SYNS_XML_AttrSymbol,
        SYNS_XML_AttrBrackets,
        SYNS_XML_AttrSquareBracket,
        SYNS_XML_AttrRoundBracket,
        SYNS_XML_AttrEscapeAmpersand
      ]
    );
    // Keyword
    RegisterItem(
      SYNS_XML_AttrKey, [
        SYNS_XML_AttrKey,
        SYNS_XML_AttrRplKey,
        SYNS_XML_AttrSQLKey,
        SYNS_XML_AttrSQLPlus,
        SYNS_XML_AttrSASMKey,
        SYNS_XML_AttrSystem,
        SYNS_XML_AttrNonReservedKeyword
      ]
    );
    // Reserved word
    RegisterItem(
      SYNS_XML_AttrReservedWord, [
        SYNS_XML_AttrReservedWord,
        SYNS_XML_AttrPLSQL,
        SYNS_XML_AttrSecondReservedWord
      ]
    );
    // Functions
    RegisterItem(
      SYNS_XML_AttrFunction, [
        SYNS_XML_AttrFunction,
        SYNS_XML_AttrInternalFunction,
        SYNS_XML_AttrTeXCommand,
        SYNS_XML_AttrUserFunction,
        SYNS_XML_AttrUser
      ]
    );
    // Directives, preprocessor, macros
    RegisterItem(
      SYNS_XML_AttrDirective, [
        SYNS_XML_AttrDirective,
        SYNS_XML_AttrDir,
        SYNS_XML_AttrPragma,
        SYNS_XML_AttrIDEDirective,
        SYNS_XML_AttrInclude,
        SYNS_XML_AttrPreprocessor,
        SYNS_XML_AttrAnnotation,
        SYNS_XML_AttrProcessingInstr
      ]
    );
    RegisterItem(
      SYNS_XML_AttrVariable, [
        SYNS_XML_AttrVariable,
        SYNS_XML_AttrSpecialVariable,
        SYNS_XML_AttrRegister,
        SYNS_XML_AttrFlags
      ]
    );
    RegisterItem(
      SYNS_XML_AttrNull, [
        SYNS_XML_AttrNull
      ]
    );
    RegisterItem(
      SYNS_XML_AttrOperator, [
        SYNS_XML_AttrOperator
      ]
    );
    RegisterItem(
      SYNS_XML_AttrAttributeName, [
        SYNS_XML_AttrNamespaceAttrName,
        SYNS_XML_AttrElementName
      ]
    );
    RegisterItem(
      SYNS_XML_AttrAttributeValue, [
        SYNS_XML_AttrValue,
        SYNS_XML_AttrNamespaceAttrValue,
        SYNS_XML_AttrSystemValue,
        SYNS_XML_AttrCDATA,
        SYNS_XML_AttrDOCTYPE
      ]
    );
    RegisterItem(
      SYNS_XML_AttrMacro, [
        SYNS_XML_AttrMacro
      ]
    );
    RegisterItem(
      SYNS_XML_AttrLabel, [
        SYNS_XML_AttrLabel,
        SYNS_XML_AttrMarker,
        SYNS_XML_AttrQualifier,
        SYNS_XML_AttrCaseLabel,
        SYNS_XML_AttrTerminator,
        SYNS_XML_AttrEntityReference,
        SYNS_XML_AttrIcon,
        SYNS_XML_AttrForm
      ]
    );
    RegisterItem(
      SYNS_XML_AttrText, [
        SYNS_XML_AttrText
      ]
    );
    RegisterItem(
      SYNS_XML_AttrIdentifier, [
        SYNS_XML_AttrIdentifier
      ]
    );
    RegisterItem(
      SYNS_XML_AttrSection, [
        SYNS_XML_AttrSection,
        SYNS_XML_AttrASP,
        SYNS_XML_AttrDOCTYPESection,
        SYNS_XML_AttrCDATASection
      ]
    );
    RegisterItem(
      SYNS_XML_AttrDataType, [
        SYNS_XML_AttrDataType
      ]
    );
    RegisterItem(
      SYNS_XML_AttrException, [
        SYNS_XML_AttrException
      ]
    );
    RegisterItem(
      SYNS_XML_AttrSyntaxError, [
        SYNS_XML_AttrInvalidSymbol,
        SYNS_XML_AttrIllegalChar,
        SYNS_XML_AttrSyntaxError,
        SYNS_XML_AttrUnknownWord
      ]
    );
    RegisterItem(
      SYNS_XML_AttrAssembler, [
        SYNS_XML_AttrAssembler
      ]
    );
    RegisterItem(
      SYNS_XML_AttrMessage, [
        SYNS_XML_AttrMessage
      ]
    );

  end;
end;

procedure TEditorSettings.RegisterClasses;
begin
  Classes.RegisterClasses([
    TSynBatSyn,
    TSynCppSyn,
    TSynCssSyn,
    TSynCsSyn,
    TSynCustomHighlighter,
    TSynDiffSyn,
    TSynHTMLSyn,
    TSynINISyn,
    TSynInnoSyn,
    TSynJavaSyn,
    TSynJScriptSyn,
    TSynJSONSyn,
    TSynLFMSyn,
    TSynLuaSyn,
    TSynPasSyn,
    TSynPerlSyn,
    TSynPHPSyn,
    TSynPikeSyn,
    TSynPoSyn,
    TSynPythonSyn,
    TSynRCSyn,
    TSynRubySyn,
    TSynSQLSyn,
    TSynTeXSyn,
    TSynUNIXShellScriptSyn,
    TSynVBSyn,
    TSynXMLSyn,
    TSynSelectedColor
  ]);
end;

procedure TEditorSettings.RegisterHighlighters(AHighlighters: THighlighters);

  procedure Reg(ASynHighlighterClass: TSynHighlighterClass;
    ASynHighlighter: TSynCustomHighlighter; const AName: string;
    const AFileExtensions: string = ''; const ADescription: string = '';
    const ALineCommentTag: string = ''; const ABlockCommentStartTag: string = '';
    const ABlockCommentEndTag: string = ''; ACodeFormatter: ICodeFormatter = nil;
    const ALayoutFileName: string = '');
  begin
    AHighlighters.RegisterHighlighter(
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
  Reg(nil, nil, HL_TXT, FILE_EXTENSIONS_TXT, STXTDescription);
  Reg(TSynBatSyn, nil, HL_BAT, FILE_EXTENSIONS_BAT, SBATDescription, '::');
  Reg(TSynCppSyn, nil, HL_CPP, FILE_EXTENSIONS_CPP, SCPPDescription, '//', '/*', '*/', TCppFormatter.Create);
  Reg(TSynCssSyn, nil, HL_CSS, FILE_EXTENSIONS_CSS, SCSSDescription);
  Reg(TSynCsSyn, nil, HL_CS, FILE_EXTENSIONS_CS, SCSDescription, '//', '/*', '*/');
  Reg(TSynDiffSyn, nil, HL_DIFF, FILE_EXTENSIONS_DIFF, SDIFFDescription);
  Reg(TSynHTMLSyn, nil, HL_HTML, FILE_EXTENSIONS_HTML, SHTMLDescription, '', '<!--', '-->', THtmlFormatter.Create);
  Reg(TSynIniSyn, nil, HL_INI, FILE_EXTENSIONS_INI, SINIDescription, ';');
  Reg(TSynInnoSyn, nil, HL_ISS, FILE_EXTENSIONS_ISS, SISSDescription, ';');
  Reg(TSynJavaSyn, nil, HL_JAVA, FILE_EXTENSIONS_JAVA, SJavaDescription, '//', '/*', '*/', TJavaFormatter.Create);
  Reg(TSynJScriptSyn, nil, HL_JS, FILE_EXTENSIONS_JS, SJSDescription, '//', '/*', '*/', TJsonFormatter.Create);
  Reg(TSynJSONSyn, nil, HL_JSON, FILE_EXTENSIONS_JSON, SJSONDescription, '', '', '', TJsonFormatter.Create);
  Reg(TSynLFMSyn, nil, HL_LFM, FILE_EXTENSIONS_LFM, SLFMDescription);
  Reg(TSynLuaSyn, nil, HL_LUA, FILE_EXTENSIONS_LUA, SLUADescription, '--');
  Reg(TSynPasSyn, nil, HL_PAS, FILE_EXTENSIONS_PAS, SPASDescription, '//', '{', '}', TPascalFormatter.Create);
  Reg(TSynPerlSyn, nil, HL_PERL, FILE_EXTENSIONS_PERL, SPERLDescription, '#', '/*', '*/');
  Reg(TSynPHPSyn, nil, HL_PHP, FILE_EXTENSIONS_PHP, SPHPDescription, '');
  Reg(TSynPikeSyn, nil, HL_PIKE, FILE_EXTENSIONS_PIKE, SPikeDescription, '', '', '');
  Reg(TSynPoSyn, nil, HL_PO, FILE_EXTENSIONS_PO, SPODescription, '#');
  Reg(TSynPythonSyn, nil, HL_PY, FILE_EXTENSIONS_PY, SPYDescription, '#', '/*', '*/');
  Reg(TSynRCSyn, nil, HL_RC, FILE_EXTENSIONS_RC, SRCDescription, '//', '/*', '*/');
  Reg(TSynRubySyn, nil, HL_RUBY, FILE_EXTENSIONS_RUBY, SRUBYDescription, '#');
  Reg(TSynSQLSyn, nil, HL_SQL, FILE_EXTENSIONS_SQL, SSQLDescription, '--', '/*', '*/', TSqlFormatter.Create);
  Reg(TSynTeXSyn, nil, HL_TEX, FILE_EXTENSIONS_TEX, STEXDescription);
  Reg(TSynUNIXShellScriptSyn, nil, HL_SH, FILE_EXTENSIONS_SH, SSHDescription);
  Reg(TSynVBSyn, nil, HL_VB, FILE_EXTENSIONS_VB, SVBDescription, '', '', '');
  Reg(TSynXMLSyn, nil, HL_XML, FILE_EXTENSIONS_XML, SXMLDescription, '', '<!--', '-->', TXmlFormatter.Create);
end;

procedure TEditorSettings.InitializeFoldHighlighters
  (AHighlighters: THighlighters);
var
  I  : Integer;
  N  : Integer;
  FH : TSynCustomFoldHighlighter;
begin
  FH := TSynCustomFoldHighlighter(AHighlighters.ItemsByName[HL_PAS].SynHighlighter);
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
  FH := TSynCustomFoldHighlighter(AHighlighters.ItemsByName[HL_XML].SynHighlighter);
  for I := Low(EditorOptionsFoldInfoXML) to High(EditorOptionsFoldInfoXML) do
  begin
    N := EditorOptionsFoldInfoXML[I].Index;
    if N >= 0 then
      FH.FoldConfig[N].Enabled := EditorOptionsFoldInfoXML[I].Enabled;
  end;
  FH := TSynCustomFoldHighlighter(AHighlighters.ItemsByName[HL_LFM].SynHighlighter);
  for I := Low(EditorOptionsFoldInfoLFM) to High(EditorOptionsFoldInfoLFM) do
  begin
    N := EditorOptionsFoldInfoLFM[I].Index;
    if N >= 0 then
      FH.FoldConfig[N].Enabled := EditorOptionsFoldInfoLFM[I].Enabled;
  end;
  FH := TSynCustomFoldHighlighter(AHighlighters.ItemsByName[HL_HTML].SynHighlighter);
  for I := Low(EditorOptionsFoldInfoHTML) to High(EditorOptionsFoldInfoHTML) do
  begin
    N := EditorOptionsFoldInfoHTML[I].Index;
    if N >= 0 then
      FH.FoldConfig[N].Enabled := EditorOptionsFoldInfoHTML[I].Enabled;
  end;
  FH := TSynCustomFoldHighlighter(AHighlighters.ItemsByName[HL_DIFF].SynHighlighter);
  for I := Low(EditorOptionsFoldInfoDiff) to High(EditorOptionsFoldInfoDiff) do
  begin
    N := EditorOptionsFoldInfoDiff[I].Index;
    if N >= 0 then
      FH.FoldConfig[N].Enabled := EditorOptionsFoldInfoDiff[I].Enabled;
  end;
end;

procedure TEditorSettings.InitializeHighlighters;
var
  HI : THighlighterItem;
begin
  RegisterHighlighters(Highlighters);

  for HI in Highlighters do
  begin
    HI.InitSynHighlighter(HI.SynHighlighter);
  end;
  InitializeFoldHighlighters(Highlighters);
end;

procedure TEditorSettings.LoadJson;
var
  LFileName : string;
begin
  Logger.Enter(Self, 'LoadJson');
  LFileName := GetApplicationConfigPath + FFileName;
  Logger.Info('LoadJson %s', [LFileName]);
  if FileExists(LFileName) then
  begin
    Logger.SendText(ReadFileToString(LFileName));
    FDeStreamer.JSONToObject(ReadFileToString(LFileName), Self);
  end;
  Logger.Leave(Self, 'LoadJson');
end;

procedure TEditorSettings.SaveJson;
var
  S         : string;
  LFileName : string;
  SL        : TStringList;
  JO        : TJSONObject;
begin
  Logger.Enter(Self, 'SaveJson');
  LFileName := GetApplicationConfigPath + FFileName;;
  Logger.Info('SaveJson %s', [LFileName]);
  JO := FStreamer.ObjectToJSON(Self);
  S := JO.FormatJSON;
  Logger.SendText(S);
  SL := TStringList.Create;
  try
    SL.Text := S;
    SL.SaveToFile(LFileName);
  finally
    SL.Free;
  end;
  Logger.Leave(Self, 'SaveJson');
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TEditorSettings.Load;
begin
  LoadJson;
end;

procedure TEditorSettings.Save;
begin
  SaveJson;
end;

procedure TEditorSettings.Apply;
begin
  Changed;
end;
{$ENDREGION}

end.

