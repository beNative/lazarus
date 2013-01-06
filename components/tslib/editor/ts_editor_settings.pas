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

unit ts_Editor_Settings;

{$mode delphi}

//*****************************************************************************

interface

uses
  Classes, SysUtils, Graphics, FileUtil,

  LazMethodList,

  NativeXML, NativeXmlObjectStorage,

  ts_Core_FormSettings,

  ts_Editor_Settings_AlignLines, ts_Editor_Settings_SearchEngine,
  ts_Editor_Settings_CodeShaper,

  ts_Editor_Interfaces, ts_Editor_SynHighlighterCollection,
  ts_Editor_SynHighlighterAttributesCollection;

{ TODO: make this a subject with the EV's and EM as observers to notify them
  when settings change. }

type
  TEditorSettingsChangedEventList = class(TMethodList)
  public
    procedure CallEditorSettingsChangedHandlers(Sender: TObject);
  end;

  TEditorSettings = class(TComponent, IEditorSettings)
  private
    FChangedEventList         : TEditorSettingsChangedEventList;
    FAutoFormatXML            : Boolean;
    FReadOnly                 : Boolean;
    FPreviewVisible           : Boolean;
    FHighlighterType          : string;
    FAutoGuessHighlighterType : Boolean;
    FShowControlCharacters    : Boolean;
    FCloseWithEsc             : Boolean;
    FDebugMode                : Boolean;
    FFileName                 : string;
    FFoldLevel                : Integer;
    FHighlighters             : THighLighters;
    FDimInactiveView          : Boolean;
    FFormSettings             : TFormSettings;
    FEditorFont               : TFont;
    FHighlighterAttributes    : TSynHighlighterAttributesCollection;
    FAlignLinesSettings       : TAlignLinesSettings;
    FSearchEngineSettings     : TSearchEngineSettings;
    FCodeShaperSettings       : TCodeShaperSettings;

    // property access methods
    function GetAlignLinesSettings: TAlignLinesSettings;
    function GetAutoFormatXML: Boolean;
    function GetAutoGuessHighlighterType: Boolean;
    function GetCloseWithESC: Boolean;
    function GetCodeShaperSettings: TCodeShaperSettings;
    function GetDebugMode: Boolean;
    function GetDimInactiveView: Boolean;
    function GetEditorFont: TFont;
    function GetFileName: string;
    function GetFoldLevel: Integer;
    function GetFormSettings: TFormSettings;
    function GetHighlighterAttributes: TSynHighlighterAttributesCollection;
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
    procedure SetHighlighterAttributes(AValue: TSynHighlighterAttributesCollection);
    procedure SetHighlighters(const AValue: THighlighters);
    procedure SetHighlighterType(const AValue: string);
    procedure SetPreviewVisible(const AValue: Boolean);
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetSearchEngineSettings(AValue: TSearchEngineSettings);
    procedure SetShowControlCharacters(const AValue: Boolean);

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Load;
    procedure Save;
    procedure Apply; // to manually notify observers

    procedure AddEditorSettingsChangedHandler(AEvent: TNotifyEvent);
    procedure RemoveEditorSettingsChangedHandler(AEvent: TNotifyEvent);

    property FileName: string
      read GetFileName write SetFileName;

    property XML: string
      read GetXML;

  protected
    procedure InitializeHighlighterAttributes;
    procedure Changed;

  published
    property HighlighterAttributes: TSynHighlighterAttributesCollection
      read GetHighlighterAttributes write SetHighlighterAttributes;

    property Highlighters: THighlighters
      read GetHighlighters write SetHighlighters;

    { Default highlighter type to use. }
    property HighlighterType: string
      read GetHighlighterType write SetHighlighterType;

    property ReadOnly: Boolean
      read GetReadOnly write SetReadOnly;

    property PreviewVisible: Boolean
      read GetPreviewVisible write SetPreviewVisible;

    property DimInactiveView: Boolean
      read GetDimInactiveView write SetDimInactiveView;

    property AutoFormatXML: Boolean
      read GetAutoFormatXML write SetAutoFormatXML;

    property AutoGuessHighlighterType: Boolean
      read GetAutoGuessHighlighterType write SetAutoGuessHighlighterType;

    property ShowControlCharacters: Boolean
      read GetShowControlCharacters write SetShowControlCharacters;

    { Determines if the application can be closed with the ESCAPE key. }
    property CloseWithESC: Boolean
      read GetCloseWithESC write SetCloseWithESC;

    property FormSettings: TFormSettings
      read GetFormSettings write SetFormSettings;

    property AlignLinesSettings: TAlignLinesSettings
      read GetAlignLinesSettings write SetAlignLinesSettings;

    property SearchEngineSettings: TSearchEngineSettings
      read GetSearchEngineSettings write SetSearchEngineSettings;

    property CodeShaperSettings: TCodeShaperSettings
      read GetCodeShaperSettings write SetCodeShaperSettings;

    property EditorFont: TFont
      read GetEditorFont write SetEditorFont;

    property DebugMode: Boolean
      read GetDebugMode write SetDebugMode;
  end;

const
  SETTINGS_FILE = 'settings.xml';

//*****************************************************************************

implementation

uses
  Dialogs, Forms,

  SynEditHighlighter, SynEditStrConst,

  ts_Editor_Resources;

procedure TEditorSettingsChangedEventList.CallEditorSettingsChangedHandlers(Sender: TObject);
var
  I: Integer;
begin
  I := Count;
  while NextDownIndex(I) do
    TNotifyEvent(Items[i])(Sender);
end;

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TEditorSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  Name := 'Settings';
  FChangedEventList := TEditorSettingsChangedEventList.Create;
  FFormSettings := TFormSettings.Create;
  FAlignLinesSettings := TAlignLinesSettings.Create;
  FSearchEngineSettings := TSearchEngineSettings.Create;
  FCodeShaperSettings := TCodeShaperSettings.Create;
  FHighlighters := THighLighters.Create(Self);
  FHighlighterAttributes := TSynHighlighterAttributesCollection.Create(nil);

  FFileName := SETTINGS_FILE;
  HighlighterType := HL_TXT;
  AutoFormatXML := True;
  AutoGuessHighlighterType := True;
  PreviewVisible := False;
  FEditorFont := TFont.Create;
  FEditorFont.Name := 'Consolas';
  FEditorFont.Size := 10;
end;

procedure TEditorSettings.BeforeDestruction;
begin
  FreeAndNil(FFormSettings);
  FreeAndNil(FHighlighters);
  FreeAndNil(FEditorFont);
  FreeAndNil(FHighlighterAttributes);
  FreeAndNil(FAlignLinesSettings);
  FreeAndNil(FSearchEngineSettings);
  FreeAndNil(FCodeShaperSettings);
  FreeAndNil(FChangedEventList);
  inherited BeforeDestruction;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

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

function TEditorSettings.GetAlignLinesSettings: TAlignLinesSettings;
begin
  Result := FAlignLinesSettings;
end;

procedure TEditorSettings.SetAlignLinesSettings(AValue: TAlignLinesSettings);
begin
  FAlignLinesSettings := AValue;
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

function TEditorSettings.GetCodeShaperSettings: TCodeShaperSettings;
begin
  Result := FCodeShaperSettings;
end;

procedure TEditorSettings.SetCodeShaperSettings(AValue: TCodeShaperSettings);
begin
  FCodeShaperSettings := AValue;
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

function TEditorSettings.GetFoldLevel: Integer;
begin
  Result := FFoldLevel;
end;

function TEditorSettings.GetFormSettings: TFormSettings;
begin
  Result := FFormSettings;
end;

procedure TEditorSettings.SetFormSettings(const AValue: TFormSettings);
begin
  FFormSettings := AValue;
end;

function TEditorSettings.GetHighlighterAttributes: TSynHighlighterAttributesCollection;
begin
  Result := FHighlighterAttributes;
end;

procedure TEditorSettings.SetHighlighterAttributes(AValue: TSynHighlighterAttributesCollection);
begin
  FHighlighterAttributes.Assign(AValue);
end;

function TEditorSettings.GetHighlighters: THighlighters;
begin
  Result := FHighlighters;
end;

procedure TEditorSettings.SetHighlighters(const AValue: THighlighters);
begin
  FHighlighters := AValue;
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

function TEditorSettings.GetPreviewVisible: Boolean;
begin
  Result := FPreviewVisible;
end;

procedure TEditorSettings.SetPreviewVisible(const AValue: Boolean);
begin
  if AValue <> PreviewVisible then
  begin
    FPreviewVisible := AValue;
    Changed;
  end;
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

function TEditorSettings.GetSearchEngineSettings: TSearchEngineSettings;
begin
  Result := FSearchEngineSettings;
end;

procedure TEditorSettings.SetSearchEngineSettings(AValue: TSearchEngineSettings);
begin
  FSearchEngineSettings := AValue;
end;

function TEditorSettings.GetShowControlCharacters: Boolean;
begin
  Result := FShowControlCharacters;
end;

procedure TEditorSettings.SetShowControlCharacters(const AValue: Boolean);
begin
  if AValue <> ShowControlCharacters then
  begin
    FShowControlCharacters := AValue;
    Changed;
  end;
end;

function TEditorSettings.GetXML: string;
begin
  Result := ReadFileToString(FileName);
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

procedure TEditorSettings.Changed;
begin
  FChangedEventList.CallEditorSettingsChangedHandlers(Self);
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************

//*****************************************************************************
// public methods                                                        BEGIN
//*****************************************************************************

procedure TEditorSettings.Load;
var
  Reader : TsdXmlObjectReader;
  Doc    : TNativeXml;
  S      : string;
begin
  S := ExtractFilePath(Application.ExeName) + FFileName;
  if FileExists(S) then
  begin
    Doc := TNativeXml.Create(nil);
    try
      Doc.LoadFromFile(S);
      Reader := TsdXmlObjectReader.Create;
      try
        Reader.ReadComponent(Doc.Root, Self, nil);
      finally
        FreeAndNil(Reader);
      end;
    finally
      FreeAndNil(Doc);
    end;
  end;
  InitializeHighlighterAttributes;
end;

procedure TEditorSettings.Save;
var
  Writer : TsdXmlObjectWriter;
  Doc    : TNativeXml;
  S      : string;
begin
  S := ExtractFilePath(Application.ExeName) + FFileName;
  Doc := TNativeXml.CreateName('Root', nil);
  try
    Writer := TsdXmlObjectWriter.Create;
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

procedure TEditorSettings.InitializeHighlighterAttributes;
var
  HA : TSynHighlighterAttributes;
begin
  if FHighlighterAttributes.Count = 0 then
  begin
    with FHighlighterAttributes do
    begin
      HA := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_XML_AttrComment);
      HA.StoredName := SYNS_XML_AttrComment;
      with FHighlighterAttributes.Add do
      begin
        Name := SYNS_XML_AttrComment;
        Attributes.Assign(HA);
      end;
      FreeAndNil(HA);

      HA := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_XML_AttrIdentifier);
      HA.StoredName := SYNS_XML_AttrIdentifier;
      with FHighlighterAttributes.Add do
      begin
        Name := SYNS_XML_AttrIdentifier;
        Attributes.Assign(HA);
      end;
      FreeAndNil(HA);

      HA := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_XML_AttrString);
      HA.StoredName := SYNS_XML_AttrString;
      with FHighlighterAttributes.Add do
      begin
        Name := SYNS_XML_AttrString;
        Attributes.Assign(HA);
      end;
      FreeAndNil(HA);

      HA := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_XML_AttrSymbol);
      HA.StoredName := SYNS_XML_AttrSymbol;
      with FHighlighterAttributes.Add do
      begin
        Name := SYNS_XML_AttrSymbol;
        Attributes.Assign(HA);
      end;
      FreeAndNil(HA);

      HA := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_XML_AttrNumber);
      HA.StoredName := SYNS_XML_AttrNumber;
      with FHighlighterAttributes.Add do
      begin
        Name := SYNS_XML_AttrNumber;
        Attributes.Assign(HA);
      end;
      FreeAndNil(HA);

      HA := TSynHighlighterAttributes.Create(SYNS_AttrKey, SYNS_XML_AttrKey);
      HA.StoredName := SYNS_XML_AttrKey;
      with FHighlighterAttributes.Add do
      begin
        Name := SYNS_XML_AttrKey;
        Attributes.Assign(HA);
      end;
      FreeAndNil(HA);


      HA := TSynHighlighterAttributes.Create(SYNS_AttrFloat, SYNS_XML_AttrFloat);
      with FHighlighterAttributes.Add do
      begin
        Name := SYNS_XML_AttrFloat;
        Attributes.Assign(HA);
      end;
      FreeAndNil(HA);

      HA := TSynHighlighterAttributes.Create(SYNS_AttrHexadecimal, SYNS_XML_AttrHexadecimal);
      with FHighlighterAttributes.Add do
      begin
        Name := SYNS_XML_AttrHexadecimal;
        Attributes.Assign(HA);
      end;
      FreeAndNil(HA);

      HA := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_XML_AttrReservedWord);
      with FHighlighterAttributes.Add do
      begin
        Name := SYNS_XML_AttrReservedWord;
        Attributes.Assign(HA);
      end;
      FreeAndNil(HA);


    end;
  end;



//Add( TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_XML_AttrComment));
//    Add(TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_XML_AttrIdentifier));
//    Add(TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_XML_AttrString));
//    Add(TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_XML_AttrSymbol));
//    Add(TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_XML_AttrNumber));
end;

//*****************************************************************************
// public methods                                                          END
//*****************************************************************************

//*****************************************************************************

initialization
  RegisterClass(TEditorSettings);

end.

