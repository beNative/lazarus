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

unit ts.Editor.Factories.Settings;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils,

  ts.Editor.Highlighters, ts.Editor.Interfaces;

type
  TEditorSettingsFactory = class(TInterfacedObject, IEditorSettingsFactory)
  public
    class procedure InitializeFoldHighlighters(AHighlighters: THighlighters);

    procedure RegisterHighlighters(AHighlighters: THighlighters);
    procedure RegisterClasses;

    function CreateInstance(
      AOwner          : TComponent = nil;
      const AFileName : string = ''
    ): IEditorSettings;
  end;

implementation

uses
  SynEditHighlighter, SynEditHighlighterFoldBase,
  SynHighlighterPas, SynHighlighterSQL, SynHighlighterLFM, SynHighlighterXML,
  SynHighlighterBat, SynHighlighterHTML, SynHighlighterCpp, SynHighlighterJava,
  SynHighlighterPerl, SynHighlighterPython, SynHighlighterPHP, SynHighlighterCss,
  SynHighlighterJScript, SynHighlighterDiff, SynHighlighterTeX, SynHighlighterPo,
  SynhighlighterUnixShellScript, SynHighlighterIni, SynHighlighterLua,
  SynHighlighterPike, SynHighlighterVB, SynHighlighterCS, SynHighlighterRC,
  SynHighlighterRuby, SynHighlighterInno,

  ts.Editor.CodeFormatters, ts.Editor.CodeFormatters.SQL,

  ts.Editor.AlignLines.Settings,
  ts.Editor.CodeFilter.Settings,
  ts.Editor.CodeShaper.Settings,
  ts.Editor.HexEditor.Settings,
  ts.Editor.MiniMap.Settings,
  ts.Editor.SortStrings.Settings,
  ts.Editor.Search.Engine.Settings,

  ts.Editor.Resources, ts.Editor.Settings;

{$REGION 'private methods'}
procedure TEditorSettingsFactory.RegisterHighlighters(
  AHighlighters: THighlighters);

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
  Reg(TSynCppSyn, nil, HL_CPP, FILE_EXTENSIONS_CPP, SCPPDescription, '//', '/*', '*/', TCPPFormatter.Create);
  Reg(TSynCssSyn, nil, HL_CSS, FILE_EXTENSIONS_CSS, SCSSDescription);
  Reg(TSynCsSyn, nil, HL_CS, FILE_EXTENSIONS_CS, SCSDescription, '//', '/*', '*/');
  Reg(TSynDiffSyn, nil, HL_DIFF, FILE_EXTENSIONS_DIFF, SDIFFDescription);
  Reg(TSynHTMLSyn, nil, HL_HTML, FILE_EXTENSIONS_HTML, SHTMLDescription, '', '<!--', '-->', THTMLFormatter.Create);
  Reg(TSynIniSyn, nil, HL_INI, FILE_EXTENSIONS_INI, SINIDescription, ';');
  Reg(TSynInnoSyn, nil, HL_ISS, FILE_EXTENSIONS_ISS, SISSDescription, ';');
  Reg(TSynJavaSyn, nil, HL_JAVA, FILE_EXTENSIONS_JAVA, SJavaDescription, '//', '/*', '*/', TJavaFormatter.Create);
  Reg(TSynJScriptSyn, nil, HL_JS, FILE_EXTENSIONS_JS, SJSDescription);
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
  Reg(TSynSQLSyn, nil, HL_SQL, FILE_EXTENSIONS_SQL, SSQLDescription, '--', '/*', '*/', TSQLFormatter.Create);
  Reg(TSynTeXSyn, nil, HL_TEX, FILE_EXTENSIONS_TEX, STEXDescription);
  Reg(TSynUNIXShellScriptSyn, nil, HL_SH, FILE_EXTENSIONS_SH, SSHDescription);
  Reg(TSynVBSyn, nil, HL_VB, FILE_EXTENSIONS_VB, SVBDescription, '', '', '');
  Reg(TSynXMLSyn, nil, HL_XML, FILE_EXTENSIONS_XML, SXMLDescription, '', '<!--', '-->', TXMLFormatter.Create);
end;

{ Initializes extra information related to the built-in highlighters like
  folding configuration and devider info. }

class procedure TEditorSettingsFactory.InitializeFoldHighlighters(
  AHighlighters: THighlighters);
var
  I  : Integer;
  N  : Integer;
  FH : TSynCustomFoldHighlighter;
begin
  FH := TSynCustomFoldHighlighter(AHighlighters.ItemsByName[HL_PAS].SynHighlighter);
  for I := Low(EditorOptionsDividerInfoPas) to High(EditorOptionsDividerInfoPas) do
  begin
    //FH.DividerDrawConfig[I].MaxDrawDepth :=
    //  EditorOptionsDividerInfoPas[I].MaxLevel;
  end;
  for I := Low(EditorOptionsFoldInfoPas) to High(EditorOptionsFoldInfoPas) do
  begin
    N := EditorOptionsFoldInfoPas[I].Index;
    //if N >= 0 then
    //  FH.FoldConfig[N].Enabled := EditorOptionsFoldInfoPas[I].Enabled;
  end;
  FH := TSynCustomFoldHighlighter(AHighlighters.ItemsByName[HL_XML].SynHighlighter);
  for I := Low(EditorOptionsFoldInfoXML) to High(EditorOptionsFoldInfoXML) do
  begin
    N := EditorOptionsFoldInfoXML[I].Index;
    //if N >= 0 then
    //  FH.FoldConfig[N].Enabled := EditorOptionsFoldInfoXML[I].Enabled;
  end;
  FH := TSynCustomFoldHighlighter(AHighlighters.ItemsByName[HL_LFM].SynHighlighter);
  for I := Low(EditorOptionsFoldInfoLFM) to High(EditorOptionsFoldInfoLFM) do
  begin
    N := EditorOptionsFoldInfoLFM[I].Index;
    //if N >= 0 then
    //  FH.FoldConfig[N].Enabled := EditorOptionsFoldInfoLFM[I].Enabled;
  end;
  FH := TSynCustomFoldHighlighter(AHighlighters.ItemsByName[HL_HTML].SynHighlighter);
  for I := Low(EditorOptionsFoldInfoHTML) to High(EditorOptionsFoldInfoHTML) do
  begin
    N := EditorOptionsFoldInfoHTML[I].Index;
    //if N >= 0 then
    //  FH.FoldConfig[N].Enabled := EditorOptionsFoldInfoHTML[I].Enabled;
  end;
  FH := TSynCustomFoldHighlighter(AHighlighters.ItemsByName[HL_DIFF].SynHighlighter);
  for I := Low(EditorOptionsFoldInfoDiff) to High(EditorOptionsFoldInfoDiff) do
  begin
    N := EditorOptionsFoldInfoDiff[I].Index;
    //if N >= 0 then
    //  FH.FoldConfig[N].Enabled := EditorOptionsFoldInfoDiff[I].Enabled;
  end;
end;

procedure TEditorSettingsFactory.RegisterClasses;
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
    TSynXMLSyn
  ]);
end;
{$ENDREGION}

{$REGION 'public methods'}
function TEditorSettingsFactory.CreateInstance(AOwner: TComponent;
  const AFileName: string): IEditorSettings;
var
  ES : IEditorSettings;
begin
  RegisterClasses;
  ES := TEditorSettings.Create(AOwner);
  RegisterHighlighters(ES.Highlighters);
  if AFileName <> '' then
  begin
    ES.FileName := AFileName;
    ES.Load;
  end;
  Result := ES;
end;
{$ENDREGION}

end.

