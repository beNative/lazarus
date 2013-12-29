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

unit ts.Editor.Factories.Settings;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, Controls,

  ts.Editor.Tools.Settings, ts.Editor.Highlighters,

  ts.Editor.Interfaces;

type

  { TEditorSettingsFactory }

  TEditorSettingsFactory = class
  public
    class procedure RegisterToolSettings(ASettings: TEditorToolSettings);
    class procedure RegisterHighlighters(AHighlighters: THighlighters);
    class procedure InitializeFoldHighlighters(AHighlighters: THighlighters);
    class procedure RegisterClasses;

    class function CreateEditorSettings(
      AOwner : TComponent = nil
    ): IEditorSettings;
  end;

implementation

uses
  Forms,

  FileUtil,

  SynEditHighlighter, SynEditHighlighterFoldBase,
  SynHighlighterPas, SynHighlighterSQL, SynHighlighterLFM, SynHighlighterXML,
  SynHighlighterBat, SynHighlighterHTML, SynHighlighterCpp, SynHighlighterJava,
  SynHighlighterPerl, SynHighlighterPython, SynHighlighterPHP, SynHighlighterCss,
  SynHighlighterJScript, SynHighlighterDiff, SynHighlighterTeX, SynHighlighterPo,
  SynhighlighterUnixShellScript, SynHighlighterIni,

  ts.Components.UniHighlighter,

  ts.Editor.CodeFormatters, ts.Editor.CodeFormatters.SQL,

  ts.Editor.AlignLines.Settings,
  ts.Editor.CodeFilter.Settings,
  ts.Editor.CodeShaper.Settings,
  ts.Editor.HexEditor.Settings,
  ts.Editor.HTMLView.Settings,
  ts.Editor.MiniMap.Settings,
  ts.Editor.SortStrings.Settings,
  ts.Editor.Search.Engine.Settings,

  ts_Editor_Resources, ts.Editor.Settings;

{ TEditorSettingsFactory }

{$region 'private methods' /fold}
class procedure TEditorSettingsFactory.RegisterToolSettings(
  ASettings: TEditorToolSettings);
begin
  ASettings.RegisterSettings(TAlignLinesSettings, 'AlignLinesSettings');
  ASettings.RegisterSettings(TCodeFilterSettings, 'CodeFilterSettings');
  ASettings.RegisterSettings(THTMLViewSettings, 'HTMLViewSettings');
  ASettings.RegisterSettings(TSortStringsSettings, 'SortStringsSettings');
  ASettings.RegisterSettings(TMiniMapSettings, 'MiniMapSettings');
  ASettings.RegisterSettings(THexEditorSettings, 'HexEditorSettings');
  ASettings.RegisterSettings(TSearchEngineSettings, 'SearchSettings');
  ASettings.RegisterSettings(TCodeShaperSettings, 'CodeShaperSettings');
end;

class procedure TEditorSettingsFactory.RegisterHighlighters(
  AHighlighters: THighlighters);
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


  //S := ExtractFilePath(Application.ExeName);
  //
  //F := S + LAYOUT_LOG;
  //if FileExistsUTF8(F) then
  //  Reg(TSynUniSyn, FSynUni, HL_LOG, 'txt log', SLOGDescription, '', '', '', nil, F);
  //F := S + LAYOUT_INI;
  //if FileExistsUTF8(F) then
  //  Reg(TSynUniSyn, FSynUni, HL_INI, FILE_EXTENSIONS_INI, SINIDescription, ';', '', '', nil, F);
  //F := S + LAYOUT_RTF;
  //if FileExistsUTF8(F) then
  //  Reg(TSynUniSyn, FSynUni, HL_RTF, FILE_EXTENSIONS_RTF, SRTFDescription, '', '', '', nil, F);
  //F := S + LAYOUT_RES;
  //if FileExistsUTF8(F) then
  //  Reg(TSynUniSyn, FSynUni, HL_RES, FILE_EXTENSIONS_RES, SRESDescription, ';', '', '', nil, F);
  //F := S + LAYOUT_CS;
  //if FileExistsUTF8(F) then
  //  Reg(TSynUniSyn, FSynUni, HL_CS, FILE_EXTENSIONS_CS, SCSDescription, '//', '/*', '*/', nil, F);
  //F := S + LAYOUT_RUBY;
  //if FileExistsUTF8(F) then
  //  Reg(TSynUniSyn, FSynUni, HL_RUBY, FILE_EXTENSIONS_RUBY, SRUBYDescription, '#', '/*', '*/', nil, F);
  //F := S + LAYOUT_LUA;
  //if FileExistsUTF8(F) then
  //  Reg(TSynUniSyn, FSynUni, HL_LUA, FILE_EXTENSIONS_LUA, SLUADescription, '--', '', '', nil, F);
end;

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

class procedure TEditorSettingsFactory.RegisterClasses;
begin
  Classes.RegisterClasses([
    TSynPasSyn,
    TSynSQLSyn,
    TSynXMLSyn,
    TSynLFMSyn,
    TSynBatSyn,
    TSynPoSyn,
    TSynCppSyn,
    TSynJavaSyn,
    TSynPerlSyn,
    TSynPythonSyn,
    TSynHTMLSyn,
    TSynJScriptSyn,
    TSynPHPSyn,
    TSynCssSyn,
    TSynDiffSyn,
    TSynTeXSyn,
    TSynUNIXShellScriptSyn,
    TSynUniSyn,
    TSynCustomHighlighter
  ]);
end;

{$endregion}

{$region 'public methods' /fold}
class function TEditorSettingsFactory.CreateEditorSettings(
  AOwner: TComponent): IEditorSettings;
var
  ES : IEditorSettings;
begin
  RegisterClasses;
  ES := TEditorSettings.Create(AOwner);
  //RegisterToolSettings(ES.ToolSettings);
  //RegisterHighlighters(ES.Highlighters);
  Result := ES;
end;
{$endregion}

end.

