{
  Copyright (C) 2013-2023 Tim Sinaeve tim.sinaeve@gmail.com

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

  All published properties are streamed from/to a XML file in a similar way as
  components are stored in a DFM/LFM file so there is no special code needed
  that handles the storage and retrieval of properties.

  Reading qnd writing of the settings is handled completely by these methods,
  which utilize the NativeXml object storage mechanism.
    procedure Load;
    procedure Save;

  To support another persistence mechanism (like e.g. LFM or JSON) you only
  need to override these two methods.

  Some remarks:
  - Don't use TPersistent objects with published TComponent properties as these
    will not be persisted. If you call SetSubComponent(True) on the nested
    component they will be stored to XML, but they will not be read if you
    try to load it back from XML.
  - Collections can be stored and loaded from XML without problems, but keep in
    mind that you don't wrap TComponent properties in your collection items (as
    these descend from TPersistent).
  - If you need to store a list of components the best way is to rely on the
    ownership mechanism of TComponent to store sub components.
    Both TToolSettings and THighlighters are components that are used as a
    container to host other components. Every subcomponent that they own will
    be persisted automatically. The property that references the container
    component does not need to be published.
    All the component types you want to store this way need to be registered
    with the RegisterClass procedure found in the Classes unit.
}
{$ENDREGION}

interface

uses
  Classes, SysUtils, Graphics, FileUtil, TypInfo,

  fpjson,

  LazMethodList,

  SynEditMiscClasses,

  ts.Core.FormSettings,

  ts.Editor.Interfaces, ts.Editor.Highlighters, ts.Editor.HighlighterAttributes,
  ts.Editor.Colors.Settings, ts.Editor.Tools.Settings,
  ts.Editor.Options.Settings,

  ts.Editor.CodeTags,

  ts.Core.Logger;

const
  DEFAULT_AUTO_GUESS_HIGHLIGHTER_TYPE = True;
  DEFAULT_AUTO_FORMAT_XML             = True;
  DEFAULT_RIGHT_EDGE                  = 80;
  DEFAULT_DIM_ACTIVE_VIEW             = True;
  DEFAULT_SINGLE_INSTANCE             = False;
  DEFAULT_LANGUAGE_CODE               = 'en';
  DEFAULT_FONT_NAME                   = 'Courier New';

type
  TEditorSettings = class(TComponent, IEditorSettings)
  private
    FAutoFormatXML            : Boolean;
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
    FToolSettings             : TEditorToolSettings;
    FEditorOptions            : TEditorOptionsSettings;

    //FDeStreamer : TJSONDeStreamer;
    //FStreamer   : TJSONStreamer;

    //procedure FDeStreamerBeforeReadObject(Sender: TObject; AObject: TObject;
    //  JSON: TJSONObject);
    //procedure FDeStreamerRestoreProperty(Sender: TObject; AObject: TObject;
    //  Info: PPropInfo; AValue: TJSONData; Var Handled: Boolean);
    procedure FEditorOptionsChanged(Sender: TObject);
    procedure FFormSettingsChanged(Sender: TObject);
    procedure FColorsChanged(Sender: TObject);

    {$REGION 'property access methods'}
    function GetAutoFormatXML: Boolean;
    function GetAutoGuessHighlighterType: Boolean;
    function GetCloseWithESC: Boolean;
    function GetColors: TEditorColorSettings;
    function GetDebugMode: Boolean;
    function GetDimInactiveView: Boolean;
    function GetEditorFont: TFont;
    function GetFileName: string;
    function GetFormSettings: TFormSettings;
    function GetHighlighterAttributes: THighlighterAttributes;
    function GetHighlighters: THighlighters;
    function GetHighlighterType: string;
    function GetLanguageCode: string;
    function GetEditorOptions: TEditorOptionsSettings;
    function GetReadOnly: Boolean;
    function GetSingleInstance: Boolean;
    function GetToolSettings: TEditorToolSettings;
    function GetXML: string;
    procedure SetAutoFormatXML(const AValue: Boolean);
    procedure SetAutoGuessHighlighterType(const AValue: Boolean);
    procedure SetCloseWithESC(const AValue: Boolean);
    procedure SetColors(AValue: TEditorColorSettings);
    procedure SetDebugMode(AValue: Boolean);
    procedure SetDimInactiveView(const AValue: Boolean);
    procedure SetEditorFont(AValue: TFont);
    procedure SetFileName(const AValue: string);
    procedure SetFormSettings(const AValue: TFormSettings);
    procedure SetHighlighterAttributes(AValue: THighlighterAttributes);
    procedure SetHighlighters(const AValue: THighlighters);
    procedure SetHighlighterType(const AValue: string);
    procedure SetLanguageCode(AValue: string);
    procedure SetEditorOptions(AValue: TEditorOptionsSettings);
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetSingleInstance(AValue: Boolean);
    procedure SetToolSettings(AValue: TEditorToolSettings);
    {$ENDREGION}

  protected
    procedure AssignDefaultColors;
    procedure InitializeHighlighterAttributes;
    procedure InitializeHighlighters;
    procedure Changed;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Apply; // to manually force a notification
    procedure LoadXML; virtual;
    procedure SaveXML; virtual;

    procedure Load; virtual;
    procedure Save; virtual;

    procedure AddEditorSettingsChangedHandler(AEvent: TNotifyEvent);
    procedure RemoveEditorSettingsChangedHandler(AEvent: TNotifyEvent);

    property FileName: string
      read GetFileName write SetFileName;

    property XML: string
      read GetXML;

  published
    property Highlighters: THighlighters
      read GetHighlighters write SetHighlighters;

    property ToolSettings: TEditorToolSettings
      read GetToolSettings write SetToolSettings;

    property HighlighterAttributes: THighlighterAttributes
      read GetHighlighterAttributes write SetHighlighterAttributes;

    property Colors: TEditorColorSettings
      read GetColors write SetColors;

    property EditorOptions: TEditorOptionsSettings
      read GetEditorOptions write SetEditorOptions;

    { Default highlighter type to use. }
    property HighlighterType: string
      read GetHighlighterType write SetHighlighterType;

    property LanguageCode: string
      read GetLanguageCode write SetLanguageCode;

    property ReadOnly: Boolean
      read GetReadOnly write SetReadOnly default False;

    property DimInactiveView: Boolean
      read GetDimInactiveView write SetDimInactiveView
      default DEFAULT_DIM_ACTIVE_VIEW;

    property AutoFormatXML: Boolean
      read GetAutoFormatXML write SetAutoFormatXML
      default DEFAULT_AUTO_FORMAT_XML;

    property AutoGuessHighlighterType: Boolean
      read GetAutoGuessHighlighterType write SetAutoGuessHighlighterType
      default DEFAULT_AUTO_GUESS_HIGHLIGHTER_TYPE;

    { Determines if the application can be closed with the ESCAPE key. }
    property CloseWithESC: Boolean
      read GetCloseWithESC write SetCloseWithESC default False;

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
  SynEditStrConst, SynEditTypes,

  ts.Core.NativeXml, ts.Core.NativeXml.ObjectStorage, ts.Core.Utils,

  ts.Editor.Resources;

{$REGION 'construction and destruction'}

//procedure OngetObject(Sender: TOBject; AObject: TObject; Info: PPropInfo;
//  AData: TJSONObject; DataName: TJSONStringType; Var AValue: TObject);
//begin
//  Logger.Send(DataName);
//end;

procedure TEditorSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FChangedEventList        := TMethodList.Create;
  FFormSettings            := TFormSettings.Create;
  FFormSettings.OnChanged  := FFormSettingsChanged;
  FColors                  := TEditorColorSettings.Create;
  FColors.OnChanged        := FColorsChanged;
  FEditorOptions           := TEditorOptionsSettings.Create;
  FEditorOptions.OnChanged := FEditorOptionsChanged;

  //FStreamer   := TJSONStreamer.Create(Self);
  //FStreamer.Options :=  FStreamer.Options + [jsoComponentsInline, jsoStreamChildren];
  //FDeStreamer := TJSONDeStreamer.Create(Self);
  //FDeStreamer.Options := FDeStreamer.Options + [jdoIgnorePropertyErrors];
  //FDeStreamer.BeforeReadObject := FDeStreamerBeforeReadObject;
  //FDeStreamer.OnRestoreProperty := FDeStreamerRestoreProperty;
  //FDeStreamer.OngetObject :=OngetObject ;

  FToolSettings      := TEditorToolSettings.Create(Self);
  FToolSettings.Name := 'ToolSettings';

  FHighlighters          := THighLighters.Create(Self);
  FHighlighters.Name     := 'Highlighters';
  FHighlighterAttributes := THighlighterAttributes.Create(Self);

  //FFileName := DEFAULT_SETTINGS_FILE;
  FHighlighterType          := HL_TXT;
  FAutoFormatXML            := DEFAULT_AUTO_FORMAT_XML;
  FAutoGuessHighlighterType := DEFAULT_AUTO_GUESS_HIGHLIGHTER_TYPE;
  FSingleInstance           := DEFAULT_SINGLE_INSTANCE;
  FEditorFont               := TFont.Create;
  FEditorFont.Name          := DEFAULT_FONT_NAME;
  FEditorFont.Size          := 10;

  FDimInactiveView := DEFAULT_DIM_ACTIVE_VIEW;
  FLanguageCode    := DEFAULT_LANGUAGE_CODE;

  RegisterClass(TSynSelectedColor);
  AssignDefaultColors;
end;

procedure TEditorSettings.BeforeDestruction;
begin
  FToolSettings.Free;
  FColors.Free;
  FEditorOptions.Free;
  FFormSettings.Free;
  FHighlighters.Free;
  FEditorFont.Free;
  FHighlighterAttributes.Free;
  FChangedEventList.Free;
  inherited BeforeDestruction;
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

//procedure TEditorSettings.FDeStreamerRestoreProperty(Sender: TObject;
//  AObject: TObject; Info: PPropInfo; AValue: TJSONData; Var Handled: Boolean);
////var
////  C : TComponent;
////  I: Integer;
////  A : TJSONArray;
//begin
//  Logger.Send('AValueType', Integer(AValue.JSONType));
//  if Info.Name = 'Highlighters' then
//  begin
//    Handled := True;
//  end
//  else if Info.Name = 'ToolSettings' then
//  begin
//    Handled := True;
//  end;
//end;
//
//procedure TEditorSettings.FDeStreamerBeforeReadObject(Sender: TObject;
//  AObject: TObject; JSON: TJSONObject);
//begin
//  Logger.Info(JSON.AsJSON);
//end;

procedure TEditorSettings.FFormSettingsChanged(Sender: TObject);
begin
  Changed;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TEditorSettings.GetAutoFormatXML: Boolean;
begin
  Result := FAutoFormatXML;
end;

procedure TEditorSettings.SetAutoFormatXML(const AValue: Boolean);
begin
  if AValue <> AutoFormatXML then
  begin
    FAutoFormatXML := AValue;
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

function TEditorSettings.GetCloseWithESC: Boolean;
begin
  Result := FCloseWithESC;
end;

procedure TEditorSettings.SetCloseWithESC(const AValue: Boolean);
begin
  if AValue <> CloseWithESC then
  begin
    FCloseWithESC := AValue;
  end;
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

procedure TEditorSettings.SetFormSettings(const AValue: TFormSettings);
begin
  FFormSettings.Assign(AValue);
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

procedure TEditorSettings.SetEditorOptions(AValue: TEditorOptionsSettings);
begin
  FEditorOptions.Assign(AValue);
  Changed;
end;

function TEditorSettings.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TEditorSettings.SetReadOnly(const AValue: Boolean);
begin
  if AValue <> ReadOnly then
  begin
    FReadOnly := AValue;
    Changed;
  end;
end;

function TEditorSettings.GetSingleInstance: Boolean;
begin
  Result := FSingleInstance;
end;

procedure TEditorSettings.SetSingleInstance(AValue: Boolean);
begin
  if AValue <> SingleInstance then
  begin
    FSingleInstance := AValue;
    // we need to SaveXML here to make sure that any other instance runs with the
    // same configuration.
    SaveXML;
    Changed;
  end;
end;

function TEditorSettings.GetToolSettings: TEditorToolSettings;
begin
  Result := FToolSettings;
end;

procedure TEditorSettings.SetToolSettings(AValue: TEditorToolSettings);
begin
  FToolSettings.Assign(AValue);
  Changed;
end;

function TEditorSettings.GetXML: string;
begin
  Result := ReadFileToString(FileName);
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

  Colors.FoldedCodeColor.Background := clSilver;
  Colors.FoldedCodeColor.BackAlpha  := 50;
  Colors.FoldedCodeColor.Foreground := clMedGray;
  Colors.FoldedCodeColor.FrameColor := clMedGray;
end;

procedure TEditorSettings.Changed;
begin
  FChangedEventList.CallNotifyEvents(Self);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TEditorSettings.LoadXML;
var
  Reader : TXmlObjectReader;
  Doc    : TNativeXml;
  S      : string;
begin
  S := GetApplicationPath + FFileName;
  Logger.Send('LoadXML', S);
  if FileExists(S) then
  begin
    Doc := TNativeXml.Create(nil);
    try
      Doc.LoadFromFile(S);
      Reader := TXmlObjectReader.Create;
      try
        Reader.ReadComponent(Doc.Root, Self, nil);
      finally
        FreeAndNil(Reader);
      end;
    finally
      FreeAndNil(Doc);
    end;
  end;
  InitializeHighlighters; // create higlighters if they cannot be loaded.
  InitializeHighlighterAttributes;
end;

procedure TEditorSettings.SaveXML;
var
  Writer : TXmlObjectWriter;
  Doc    : TNativeXml;
  S      : string;
begin
  S :=   GetApplicationPath + FFileName;
  Logger.Send('SaveXML', S);
  Doc := TNativeXml.CreateName('Root', nil);
  try
    Writer := TXmlObjectWriter.Create;
    try
      Doc.XmlFormat := xfReadable;
      Writer.WriteComponent(Doc.Root, Self);
      Doc.SaveToFile(S);
    finally
      FreeAndNil(Writer);
    end;
  finally
    FreeAndNil(Doc);
  end;
end;

procedure TEditorSettings.Load;
//var
//  S  : string;
begin
  LoadXML;
  //if FileExists(FileName) then
  //begin
  //  Logger.Info(ReadFileToString(FileName));
  //  FDeStreamer.JSONToObject(ReadFileToString(FileName), Self);
  //end;
end;

procedure TEditorSettings.Save;
//var
//  S  : string;
//  SL : TStringList;
//  JO : TJSONObject;
//  JA : TJSONArray;
//  I  : Integer;
begin
  SaveXML;

  //JO := FStreamer.ObjectToJSON(Self);
  //
  //JA := TJSONArray.Create;
  //for I := 0 to Highlighters.Count -1 do
  //begin
  //  JA.Add(FStreamer.ObjectToJSON(Highlighters.Items[I]));
  //end;
  //JO.Add('Highlighters', JA);
  //S := JO.FormatJSON;
  //SL := TStringList.Create;
  //try
  //  SL.Text := S;
  //  SL.SaveToFile(FileName);
  //finally
  //  SL.Free;
  //end;
end;

procedure TEditorSettings.Apply;
begin
  Changed;
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
    RegisterItem(
      SYNS_XML_AttrComment, [
        SYNS_XML_AttrComment,
        SYNS_XML_AttrDocumentation,
        SYNS_XML_AttrRplComment,
        SYNS_XML_AttrSASMComment
      ]
    );
    RegisterItem(
      SYNS_XML_AttrString, [
        SYNS_XML_AttrString
      ]
    );
    RegisterItem(
      SYNS_XML_AttrSymbol, [
        SYNS_XML_AttrSymbol,
        SYNS_XML_AttrBrackets,
        SYNS_XML_AttrSquareBracket,
        SYNS_XML_AttrRoundBracket
      ]
    );
    RegisterItem(
      SYNS_XML_AttrNumber, [
        SYNS_XML_AttrNumber
      ]
    );
    RegisterItem(
      SYNS_XML_AttrKey, [
        SYNS_XML_AttrKey,
        SYNS_XML_AttrRplKey,
        SYNS_XML_AttrSQLKey,
        SYNS_XML_AttrSQLPlus,
        SYNS_XML_AttrTeXCommand,
        SYNS_XML_AttrSASMKey
      ]
    );
    RegisterItem(
      SYNS_XML_AttrFloat, [
        SYNS_XML_AttrFloat
      ]
    );
    RegisterItem(
      SYNS_XML_AttrHexadecimal, [
        SYNS_XML_AttrHexadecimal
      ]
    );
    RegisterItem(
      SYNS_XML_AttrReservedWord, [
        SYNS_XML_AttrReservedWord,
        SYNS_XML_AttrPLSQL,
        SYNS_XML_AttrSecondReservedWord
      ]
    );
    RegisterItem(
      SYNS_XML_AttrDirective, [
        SYNS_XML_AttrIDEDirective,
        SYNS_XML_AttrInclude,
        SYNS_XML_AttrPreprocessor,
        SYNS_XML_AttrProcessingInstr
      ]
    );  //
    RegisterItem(
      SYNS_XML_AttrCharacter, [
        SYNS_XML_AttrCharacter
      ]
    );
    RegisterItem(
      SYNS_XML_AttrVariable, [
        SYNS_XML_AttrSpecialVariable
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
        SYNS_XML_AttrCDATA,
        SYNS_XML_AttrDOCTYPE
      ]
    );
    RegisterItem(
      SYNS_XML_AttrMacro, [
        SYNS_XML_AttrPragma
      ]
    );
    RegisterItem(
      SYNS_XML_AttrText, [
        SYNS_XML_AttrText,
        SYNS_XML_AttrEmbedText
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
  end;
end;

procedure TEditorSettings.InitializeHighlighters;
var
  HI: THighlighterItem;
begin
  for HI in Highlighters do
  begin
    HI.InitSynHighlighter(HI.SynHighlighter);
  end;
end;
{$ENDREGION}

end.

