{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Components.ExportRTF;

{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynExportRTF.pas, released 2000-04-16.

The Original Code is partly based on the mwRTFExport.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Michael Hieke.
Portions created by Michael Hieke are Copyright 2000 Michael Hieke.
Portions created by James D. Jacobson are Copyright 1999 Martin Waldenburg.
Unicode translation by Maël Hörz.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{ Ported to FPC/Lazarus by Tim Sinaeve. }

interface

uses
  Graphics, Clipbrd, Classes,

  LCLIntf, LCLType,

  SynEditExport, SynEditHighlighter;

type

  { TSynExporterRTF }

  TSynExporterRTF = class(TSynCustomExporter)
  private
    FAttributesChanged: Boolean;
    FListColors: TList;
    function ColorToRTF(AColor: TColor): UnicodeString;
    function GetColorIndex(AColor: TColor): Integer;
  protected
    procedure FormatAfterLastAttribute; override;
    procedure FormatAttributeInitImmediate(Attri: TSynHighlighterAttributes;
      IsSpace: Boolean); override;
    procedure FormatAttributeDoneImmediate(Attri: TSynHighlighterAttributes;
      IsSpace: Boolean); override;
    procedure FormatAttributeDone(BackgroundChanged, ForegroundChanged: Boolean;
      FontStylesChanged: TFontStyles); override;
    procedure FormatAttributeInit(BackgroundChanged, ForegroundChanged: Boolean;
      FontStylesChanged: TFontStyles); override;
    procedure FormatBeforeFirstAttribute(BackgroundChanged,
      ForegroundChanged: Boolean; FontStylesChanged: TFontStyles); override;
    procedure FormatBeforeFirstAttributeImmediate(BG, FG: TColor); override;
    procedure FormatAfterLastAttributeImmediate; override;
    procedure FormatNewLine; override;
    function GetFooter: string; override;
    function GetFormatName: string; override;
    function GetHeader: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
  published
    property Color;
    property DefaultFilter;
    property Font;
    property Highlighter;
    property Title;
    property UseBackground;
  end;

implementation

uses
  SysUtils,

  SynEditStrConst;

constructor TSynExporterRTF.Create(AOwner: TComponent);
const
  CF_RTF = 'Rich Text Format';
begin
  inherited Create(AOwner);
  FListColors := TList.Create;
  FClipboardFormat := RegisterClipboardFormat(CF_RTF);
  FDefaultFilter := SYNS_FilterRTF;
  FReplaceReserved['\'] := '\\';
  FReplaceReserved['{'] := '\{';
  FReplaceReserved['}'] := '\}';
end;

destructor TSynExporterRTF.Destroy;
begin
  FListColors.Free;
  FListColors := nil;
  inherited Destroy;
end;

procedure TSynExporterRTF.Clear;
begin
  inherited Clear;
  if Assigned(FListColors) then
    FListColors.Clear;
end;

function TSynExporterRTF.ColorToRTF(AColor: TColor): UnicodeString;
var
  Col: Integer;
begin
  Col := ColorToRGB(AColor);
  Result := UnicodeString(Format('\red%d\green%d\blue%d;', [GetRValue(Col), GetGValue(Col),
    GetBValue(Col)]));
end;

procedure TSynExporterRTF.FormatAfterLastAttribute;
begin
  // no need to reset the font style here...
end;

procedure TSynExporterRTF.FormatAttributeInitImmediate(
  Attri: TSynHighlighterAttributes; IsSpace: Boolean);
begin
  //
end;

procedure TSynExporterRTF.FormatAttributeDoneImmediate(
  Attri: TSynHighlighterAttributes; IsSpace: Boolean);
begin
  //
end;

procedure TSynExporterRTF.FormatAttributeDone(BackgroundChanged,
  ForegroundChanged: Boolean; FontStylesChanged: TFontStyles);
const
  FontTags: array[TFontStyle] of UnicodeString = ('\b0', '\i0', '\ul0', '\strike0');
var
  AStyle: TFontStyle;
begin
  // nothing to do about the color, but reset the font style
  for AStyle := Low(TFontStyle) to High(TFontStyle) do
  begin
    if AStyle in FontStylesChanged then
    begin
      FAttributesChanged := True;
      AddData(AnsiString(FontTags[AStyle]));
    end;
  end;
end;

procedure TSynExporterRTF.FormatAttributeInit(BackgroundChanged,
  ForegroundChanged: Boolean; FontStylesChanged: TFontStyles);
const
  FontTags: array[TFontStyle] of UnicodeString = ('\b', '\i', '\ul', '\strike');
var
  AStyle: TFontStyle;
begin
  // background color
  if BackgroundChanged then
  begin
    AddData(Format('\cb%d', [GetColorIndex(fLastBG)]));
    FAttributesChanged := True;
  end;
  // text color
  if ForegroundChanged then
  begin
    AddData(Format('\cf%d', [GetColorIndex(fLastFG)]));
    FAttributesChanged := True;
  end;
  // font styles
  for AStyle := Low(TFontStyle) to High(TFontStyle) do
    if AStyle in FontStylesChanged then
    begin
      AddData(AnsiString(FontTags[AStyle]));
      FAttributesChanged := True;
    end;
  if FAttributesChanged then
  begin
    AddData(' ');
    FAttributesChanged := False;
  end;
end;

procedure TSynExporterRTF.FormatBeforeFirstAttribute(BackgroundChanged,
  ForegroundChanged: Boolean; FontStylesChanged: TFontStyles);
begin
  FormatAttributeInit(BackgroundChanged, ForegroundChanged, FontStylesChanged);
end;

procedure TSynExporterRTF.FormatBeforeFirstAttributeImmediate(BG, FG: TColor);
begin
  //
end;

procedure TSynExporterRTF.FormatAfterLastAttributeImmediate;
begin
  //
end;

procedure TSynExporterRTF.FormatNewLine;
begin
  AddData(#13#10'\par ');
end;

function TSynExporterRTF.GetColorIndex(AColor: TColor): Integer;
begin
  Result := FListColors.IndexOf(Pointer(AColor));
  if Result = -1 then
    Result := FListColors.Add(Pointer(AColor));
end;

function TSynExporterRTF.GetFooter: string;
begin
  Result := '}';
end;

function TSynExporterRTF.GetFormatName: string;
begin
  Result := SYNS_ExporterFormatRTF;
end;

function TSynExporterRTF.GetHeader: string;
var
  i: Integer;

  function GetFontTable: UnicodeString;
  begin
    Result := '{\fonttbl{\f0\fmodern ' + UnicodeString(Font.Name);
    Result := Result + ';}}'#13#10;
  end;

begin
  Result := '{\rtf1\ansi\ansicpg1252\uc1\deff0\deftab720' +
    string(GetFontTable);
  // all the colors
  Result := Result + '{\colortbl';
  for i := 0 to FListColors.Count - 1 do
    Result := Result + string(ColorToRTF(TColor(FListColors[i])));
  Result := Result + '}'#13#10;
  Result := Result + '{\info{\comment}'#13#10;
  Result := Result + '{\title ' + fTitle + '}}'#13#10;
  if fUseBackground then
    Result := Result + { TODO: use background color } #13#10;
  Result := Result + Format('\deflang1033\pard\plain\f0\fs%d ',
    [2 * Font.Size]);
end;

end.


