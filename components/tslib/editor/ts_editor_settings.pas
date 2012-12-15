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

unit ts_Editor_Settings;

{$mode delphi}

//*****************************************************************************

interface

uses
  Classes, SysUtils, Graphics, FileUtil,

  NativeXML, NativeXmlObjectStorage,

  ts_Core_FormSettings,

  ts_Editor_Settings_AlignLines,

  ts_Editor_Interfaces, ts_Editor_SynHighlighterCollection,
  ts_Editor_SynHighlighterAttributesCollection;

type
  TEditorSettings = class(TComponent, IEditorSettings)
  private
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
    FDefaultFont              : TFont;
    FHighlighterAttributes    : TSynHighlighterAttributesCollection;
    FAlignLinesSettings       : TAlignLinesSettings;

    // property access methods
    function GetAlignLinesSettings: TAlignLinesSettings;
    function GetAutoFormatXML: Boolean;
    function GetAutoGuessHighlighterType: Boolean;
    function GetCloseWithESC: Boolean;
    function GetDebugMode: Boolean;
    function GetDefaultFont: TFont;
    function GetDimInactiveView: Boolean;
    function GetFileName: string;
    function GetFoldLevel: Integer;
    function GetFormSettings: TFormSettings;
    function GetHighlighterAttributes: TSynHighlighterAttributesCollection;
    function GetHighlighters: THighlighters;
    function GetHighlighterType: string;
    function GetPreviewVisible: Boolean;
    function GetReadOnly: Boolean;
    function GetShowControlCharacters: Boolean;
    function GetXML: string;
    procedure SetAlignLinesSettings(AValue: TAlignLinesSettings);
    procedure SetAutoFormatXML(const AValue: Boolean);
    procedure SetAutoGuessHighlighterType(const AValue: Boolean);
    procedure SetCloseWithESC(const AValue: Boolean);
    procedure SetDebugMode(AValue: Boolean);
    procedure SetDefaultFont(const AValue: TFont);
    procedure SetDimInactiveView(const AValue: Boolean);
    procedure SetFileName(const AValue: string);
    procedure SetFormSettings(const AValue: TFormSettings);
    procedure SetHighlighterAttributes(AValue: TSynHighlighterAttributesCollection);
    procedure SetHighlighters(const AValue: THighlighters);
    procedure SetHighlighterType(const AValue: string);
    procedure SetPreviewVisible(const AValue: Boolean);
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetShowControlCharacters(const AValue: Boolean);

  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Load;
    procedure Save;

    property FileName: string
      read GetFileName write SetFileName;

    property XML: string
      read GetXML;

  protected
    procedure InitializeHighlighterAttributes;

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

    property DefaultFont: TFont
      read GetDefaultFont write SetDefaultFont;

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

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

constructor TEditorSettings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'Settings';
  FFormSettings := TFormSettings.Create;
  FAlignLinesSettings := TAlignLinesSettings.Create;
  FHighlighters := THighLighters.Create(Self);
  FHighlighterAttributes := TSynHighlighterAttributesCollection.Create(nil);

  FFileName := SETTINGS_FILE;
  HighlighterType := HL_TXT;
  AutoFormatXML := True;
  AutoGuessHighlighterType := True;
  PreviewVisible := False;
  FDefaultFont := TFont.Create;
  FDefaultFont.Name := 'Consolas';
  FDefaultFont.Size := 10;
end;

procedure TEditorSettings.AfterConstruction;
begin
  inherited AfterConstruction;

end;

procedure TEditorSettings.BeforeDestruction;
begin
  FreeAndNil(FFormSettings);
  FreeAndNil(FHighlighters);
  FreeAndNil(FDefaultFont);
  FreeAndNil(FHighlighterAttributes);
  FreeAndNil(FAlignLinesSettings);
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

function TEditorSettings.GetAlignLinesSettings: TAlignLinesSettings;
begin
  Result := FAlignLinesSettings;
end;

function TEditorSettings.GetAutoGuessHighlighterType: Boolean;
begin
  Result := FAutoGuessHighlighterType;
end;

function TEditorSettings.GetCloseWithESC: Boolean;
begin
  Result := FCloseWithESC;
end;

function TEditorSettings.GetDebugMode: Boolean;
begin
  Result := FDebugMode;
end;

function TEditorSettings.GetDimInactiveView: Boolean;
begin
  Result := FDimInactiveView;
end;

function TEditorSettings.GetFileName: string;
begin
  Result := FFileName;
end;

function TEditorSettings.GetFoldLevel: Integer;
begin
  Result := FFoldLevel;
end;

function TEditorSettings.GetFormSettings: TFormSettings;
begin
  Result := FFormSettings;
end;

function TEditorSettings.GetHighlighterAttributes: TSynHighlighterAttributesCollection;
begin
  Result := FHighlighterAttributes;
end;

function TEditorSettings.GetHighlighters: THighlighters;
begin
  Result := FHighlighters;
end;

function TEditorSettings.GetHighlighterType: string;
begin
  Result := FHighlighterType;
end;

function TEditorSettings.GetPreviewVisible: Boolean;
begin
  Result := FPreviewVisible;
end;

function TEditorSettings.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

function TEditorSettings.GetShowControlCharacters: Boolean;
begin
  Result := FShowControlCharacters;
end;

function TEditorSettings.GetXML: string;
begin
  Result := ReadFileToString(FileName);
end;

procedure TEditorSettings.SetAlignLinesSettings(AValue: TAlignLinesSettings);
begin
  FAlignLinesSettings := AValue;
end;

procedure TEditorSettings.SetAutoFormatXML(const AValue: Boolean);
begin
  if AValue <> AutoFormatXML then
  begin
    FAutoFormatXML := AValue;
  end;
end;

procedure TEditorSettings.SetAutoGuessHighlighterType(const AValue: Boolean);
begin
  if AValue <> AutoGuessHighlighterType then
  begin
    FAutoGuessHighlighterType := AValue;
  end;
end;

procedure TEditorSettings.SetCloseWithESC(const AValue: Boolean);
begin
  if AValue <> CloseWithESC then
  begin
    FCloseWithESC := AValue;
  end;
end;

procedure TEditorSettings.SetDebugMode(AValue: Boolean);
begin
  if AValue <> DebugMode then
  begin
    FDebugMode := AValue;
  end;
end;

function TEditorSettings.GetDefaultFont: TFont;
begin
  Result := FDefaultFont;
end;

procedure TEditorSettings.SetDefaultFont(const AValue: TFont);
begin
  if AValue <> DefaultFont then
  begin
    FDefaultFont := AValue;
  end;
end;

procedure TEditorSettings.SetDimInactiveView(const AValue: Boolean);
begin
  if AValue <> DimInactiveView then
  begin
    FDimInactiveView := AValue;
  end;
end;

procedure TEditorSettings.SetFileName(const AValue: string);
begin
  if AValue <> FileName then
  begin
    FFileName := AValue;
  end;
end;

procedure TEditorSettings.SetFormSettings(const AValue: TFormSettings);
begin
  FFormSettings := AValue;
end;

procedure TEditorSettings.SetHighlighterAttributes(AValue: TSynHighlighterAttributesCollection);
begin
  FHighlighterAttributes.Assign(AValue);
end;

procedure TEditorSettings.SetHighlighters(const AValue: THighlighters);
begin
  FHighlighters := AValue;
end;

procedure TEditorSettings.SetHighlighterType(const AValue: string);
begin
  if AValue <> HighlighterType then
  begin
    FHighlighterType := AValue;
  end;
end;

procedure TEditorSettings.SetPreviewVisible(const AValue: Boolean);
begin
  if AValue <> PreviewVisible then
  begin
    FPreviewVisible := AValue;
  end;
end;

procedure TEditorSettings.SetReadOnly(const AValue: Boolean);
begin
  if AValue <> ReadOnly then
  begin
    FReadOnly := AValue;
  end;
end;

procedure TEditorSettings.SetShowControlCharacters(const AValue: Boolean);
begin
  if AValue <> ShowControlCharacters then
  begin
    FShowControlCharacters := AValue;
  end;
end;

//*****************************************************************************
// property access methods                                                 END
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

