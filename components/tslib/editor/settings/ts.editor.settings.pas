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

unit ts.Editor.Settings;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, Graphics, FileUtil, ActnList,

  LazMethodList,

  SynEditMiscClasses, SynEditMarkupBracket,

  ts.Core.FormSettings,

  ts.Editor.AlignLines.Settings, ts.Editor.Search.Engine.Settings,
  ts.Editor.CodeShaper.Settings, ts.Editor.CodeFilter.Settings,
  ts.Editor.HTMLView.Settings, ts.Editor.MiniMap.Settings,
  ts.Editor.HexEditor.Settings,

  ts.Editor.Interfaces, ts.Editor.Highlighters, ts.Editor.HighlighterAttributes,

  ts.Core.SharedLogger;

type
  { TEditorSettings }

  TEditorSettings = class(TComponent, IEditorSettings)
  private
    FAutoFormatXML            : Boolean;
    FChangedEventList         : TMethodList;
    FReadOnly                 : Boolean;
    FPreviewVisible           : Boolean;
    FHighlighterType          : string;
    FAutoGuessHighlighterType : Boolean;
    FShowControlCharacters    : Boolean;
    FCloseWithEsc             : Boolean;
    FDebugMode                : Boolean;
    FSingleInstance           : Boolean;
    FFileName                 : string;
    FLanguageCode             : string;
    FFoldLevel                : Integer;
    FHighlighters             : THighLighters;
    FDimInactiveView          : Boolean;
    FFormSettings             : TFormSettings;
    FEditorFont               : TFont;
    FHighlighterAttributes    : THighlighterAttributes;
    FAlignLinesSettings       : TAlignLinesSettings;
    FSearchEngineSettings     : TSearchEngineSettings;
    FCodeShaperSettings       : TCodeShaperSettings;
    FCodeFilterSettings       : TCodeFilterSettings;
    FMiniMapSettings          : TMiniMapSettings;
    FHTMLViewSettings         : THTMLViewSettings;
    FHexEditorSettings        : THexEditorSettings;

    FRightEdge             : Integer;
    FRightEdgeColor        : TColor;
    FBracketHighlightStyle : TSynEditBracketHighlightStyle;
    FWantTabs              : Boolean;
    FTabWidth              : Integer;
    FBlockIndent           : Integer;
    FBlockTabIndent        : Integer;
    FExtraCharSpacing      : Integer;
    FExtraLineSpacing      : Integer;

    FIncrementColor     : TSynSelectedColor;
    FHighlightAllColor  : TSynSelectedColor;
    FBracketMatchColor  : TSynSelectedColor;
    FMouseLinkColor     : TSynSelectedColor;
    FLineHighlightColor : TSynSelectedColor;
    FFoldedCodeColor    : TSynSelectedColor;
    FSelectedColor      : TSynSelectedColor;

    procedure FFormSettingsChanged(Sender: TObject);

    {$region 'property access methods' /fold}
    function GetAlignLinesSettings: TAlignLinesSettings;
    function GetAutoFormatXML: Boolean;
    function GetAutoGuessHighlighterType: Boolean;
    function GetBlockIndent: Integer;
    function GetBlockTabIndent: Integer;
    function GetBracketHighlightStyle: TSynEditBracketHighlightStyle;
    function GetBracketMatchColor: TSynSelectedColor;
    function GetCloseWithESC: Boolean;
    function GetCodeFilterSettings: TCodeFilterSettings;
    function GetCodeShaperSettings: TCodeShaperSettings;
    function GetDebugMode: Boolean;
    function GetDimInactiveView: Boolean;
    function GetEditorFont: TFont;
    function GetExtraCharSpacing: Integer;
    function GetExtraLineSpacing: Integer;
    function GetFileName: string;
    function GetFoldedCodeColor: TSynSelectedColor;
    function GetFoldLevel: Integer;
    function GetFormSettings: TFormSettings;
    function GetHexEditorSettings: THexEditorSettings;
    function GetHighlightAllColor: TSynSelectedColor;
    function GetHighlighterAttributes: THighlighterAttributes;
    function GetHighlighters: THighlighters;
    function GetHighlighterType: string;
    function GetHTMLViewSettings: THTMLViewSettings;
    function GetIncrementColor: TSynSelectedColor;
    function GetLanguageCode: string;
    function GetLineHighlightColor: TSynSelectedColor;
    function GetMiniMapSettings: TMiniMapSettings;
    function GetMouseLinkColor: TSynSelectedColor;
    function GetPreviewVisible: Boolean;
    function GetReadOnly: Boolean;
    function GetRightEdge: Integer;
    function GetRightEdgeColor: TColor;
    function GetSearchEngineSettings: TSearchEngineSettings;
    function GetSelectedColor: TSynSelectedColor;
    function GetShowSpecialCharacters: Boolean;
    function GetSingleInstance: Boolean;
    function GetTabWidth: Integer;
    function GetWantTabs: Boolean;
    function GetXML: string;
    procedure SetAlignLinesSettings(AValue: TAlignLinesSettings);
    procedure SetAutoFormatXML(const AValue: Boolean);
    procedure SetAutoGuessHighlighterType(const AValue: Boolean);
    procedure SetBlockIndent(AValue: Integer);
    procedure SetBlockTabIndent(AValue: Integer);
    procedure SetBracketHighlightStyle(AValue: TSynEditBracketHighlightStyle);
    procedure SetBracketMatchColor(AValue: TSynSelectedColor);
    procedure SetCloseWithESC(const AValue: Boolean);
    procedure SetCodeFilterSettings(AValue: TCodeFilterSettings);
    procedure SetCodeShaperSettings(AValue: TCodeShaperSettings);
    procedure SetDebugMode(AValue: Boolean);
    procedure SetDimInactiveView(const AValue: Boolean);
    procedure SetEditorFont(AValue: TFont);
    procedure SetExtraCharSpacing(AValue: Integer);
    procedure SetExtraLineSpacing(AValue: Integer);
    procedure SetFileName(const AValue: string);
    procedure SetFoldedCodeColor(AValue: TSynSelectedColor);
    procedure SetFormSettings(const AValue: TFormSettings);
    procedure SetHexEditorSettings(AValue: THexEditorSettings);
    procedure SetHighlightAllColor(AValue: TSynSelectedColor);
    procedure SetHighlighterAttributes(AValue: THighlighterAttributes);
    procedure SetHighlighters(const AValue: THighlighters);
    procedure SetHighlighterType(const AValue: string);
    procedure SetHTMLViewSettings(AValue: THTMLViewSettings);
    procedure SetIncrementColor(AValue: TSynSelectedColor);
    procedure SetLanguageCode(AValue: string);
    procedure SetLineHighlightColor(AValue: TSynSelectedColor);
    procedure SetMiniMapSettings(AValue: TMiniMapSettings);
    procedure SetMouseLinkColor(AValue: TSynSelectedColor);
    procedure SetPreviewVisible(const AValue: Boolean);
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetRightEdge(AValue: Integer);
    procedure SetRightEdgeColor(AValue: TColor);
    procedure SetSearchEngineSettings(AValue: TSearchEngineSettings);
    procedure SetSelectedColor(AValue: TSynSelectedColor);
    procedure SetShowSpecialCharacters(const AValue: Boolean);
    procedure SetSingleInstance(AValue: Boolean);
    procedure SetTabWidth(AValue: Integer);
    procedure SetWantTabs(AValue: Boolean);
    {$endregion}

  protected
    procedure AssignDefaultColors;
    procedure InitializeHighlighterAttributes;
    procedure Changed;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Apply; // to manually force a notification
    procedure Load;
    procedure Save;

    procedure AddEditorSettingsChangedHandler(AEvent: TNotifyEvent);
    procedure RemoveEditorSettingsChangedHandler(AEvent: TNotifyEvent);

    property FileName: string
      read GetFileName write SetFileName;

    property XML: string
      read GetXML;

  published
    property HighlighterAttributes: THighlighterAttributes
      read GetHighlighterAttributes write SetHighlighterAttributes;

    { Default highlighter type to use. }
    property HighlighterType: string
      read GetHighlighterType write SetHighlighterType;

    property LanguageCode: string
      read GetLanguageCode write SetLanguageCode;

    property ReadOnly: Boolean
      read GetReadOnly write SetReadOnly default False;

    property PreviewVisible: Boolean
      read GetPreviewVisible write SetPreviewVisible default False;

    property DimInactiveView: Boolean
      read GetDimInactiveView write SetDimInactiveView;

    property AutoFormatXML: Boolean
      read GetAutoFormatXML write SetAutoFormatXML default True;

    property AutoGuessHighlighterType: Boolean
      read GetAutoGuessHighlighterType write SetAutoGuessHighlighterType default True;

    property ShowSpecialCharacters: Boolean
      read GetShowSpecialCharacters write SetShowSpecialCharacters default False;

    { Determines if the application can be closed with the ESCAPE key. }
    property CloseWithESC: Boolean
      read GetCloseWithESC write SetCloseWithESC default False;

    property FormSettings: TFormSettings
      read GetFormSettings write SetFormSettings;

    property AlignLinesSettings: TAlignLinesSettings
      read GetAlignLinesSettings write SetAlignLinesSettings;

    property SearchEngineSettings: TSearchEngineSettings
      read GetSearchEngineSettings write SetSearchEngineSettings;

    property CodeShaperSettings: TCodeShaperSettings
      read GetCodeShaperSettings write SetCodeShaperSettings;

    property CodeFilterSettings: TCodeFilterSettings
      read GetCodeFilterSettings write SetCodeFilterSettings;

    property HexEditorSettings: THexEditorSettings
      read GetHexEditorSettings write SetHexEditorSettings;

    property HTMLViewSettings: THTMLViewSettings
      read GetHTMLViewSettings write SetHTMLViewSettings;

    property MiniMapSettings: TMiniMapSettings
      read GetMiniMapSettings write SetMiniMapSettings;

    property EditorFont: TFont
      read GetEditorFont write SetEditorFont;

    property DebugMode: Boolean
      read GetDebugMode write SetDebugMode default False;

    property SingleInstance: Boolean
      read GetSingleInstance write SetSingleInstance default False;

    // Colors
    property IncrementColor: TSynSelectedColor
      read GetIncrementColor write SetIncrementColor;

    property HighlightAllColor: TSynSelectedColor
      read GetHighlightAllColor write SetHighlightAllColor;

    property BracketMatchColor: TSynSelectedColor
      read GetBracketMatchColor write SetBracketMatchColor;

    property MouseLinkColor: TSynSelectedColor
      read GetMouseLinkColor write SetMouseLinkColor;

    property LineHighlightColor: TSynSelectedColor
      read GetLineHighlightColor write SetLineHighlightColor;

    property FoldedCodeColor: TSynSelectedColor
      read GetFoldedCodeColor write SetFoldedCodeColor;

    property SelectedColor: TSynSelectedColor
      read GetSelectedColor write SetSelectedColor;

    property RightEdgeColor: TColor
      read GetRightEdgeColor write SetRightEdgeColor default clSilver;

    property RightEdge: Integer
      read GetRightEdge write SetRightEdge default 80;

    property BracketHighlightStyle: TSynEditBracketHighlightStyle
      read GetBracketHighlightStyle write SetBracketHighlightStyle;

    property TabWidth: Integer
      read GetTabWidth write SetTabWidth default 8;

    property WantTabs: Boolean
      read GetWantTabs write SetWantTabs default True;

    property BlockIndent: Integer
      read GetBlockIndent write SetBlockIndent default 2;

    property BlockTabIndent: Integer
      read GetBlockTabIndent write SetBlockTabIndent default 0;

    property ExtraCharSpacing: Integer
      read GetExtraCharSpacing write SetExtraCharSpacing default 0;

    property ExtraLineSpacing: Integer
      read GetExtraLineSpacing write SetExtraLineSpacing default 0;

    property Highlighters: THighlighters
      read GetHighlighters write SetHighlighters;
  end;

const
  DEFAULT_SETTINGS_FILE = 'settings.xml';

implementation

uses
  Dialogs, Forms,

  SynEditHighlighter, SynEditStrConst, SynEditTypes,

    ts.Core.NativeXml, ts.Core.NativeXml.ObjectStorage,

  ts_Editor_Resources;

{$region 'construction and destruction' /fold}
procedure TEditorSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  Name := 'Settings';
  FChangedEventList := TMethodList.Create;
  FFormSettings := TFormSettings.Create;
  FFormSettings.OnChanged := FFormSettingsChanged;
  FAlignLinesSettings := TAlignLinesSettings.Create;
  FSearchEngineSettings := TSearchEngineSettings.Create;
  FCodeShaperSettings := TCodeShaperSettings.Create;
  FCodeFilterSettings := TCodeFilterSettings.Create;
  FMiniMapSettings := TMiniMapSettings.Create;
  FHexEditorSettings := THexEditorSettings.Create;
  FHTMLViewSettings := THTMLViewSettings.Create;
  FHighlighters := THighLighters.Create(Self);
  FHighlighterAttributes := THighlighterAttributes.Create(nil);

  FFileName := DEFAULT_SETTINGS_FILE;
  HighlighterType := HL_TXT;
  AutoFormatXML := True;
  AutoGuessHighlighterType := True;
  PreviewVisible := False;
  FEditorFont := TFont.Create;
  FEditorFont.Name := 'Courier New';
  FEditorFont.Size := 10;

  FBlockIndent := 2;
  FTabWidth := 8;
  FWantTabs := True;
  FRightEdge := 80;
  FRightEdgeColor := clSilver;

  RegisterClass(TSynSelectedColor);
  FIncrementColor     := TSynSelectedColor.Create;
  FHighlightAllColor  := TSynSelectedColor.Create;
  FBracketMatchColor  := TSynSelectedColor.Create;
  FMouseLinkColor     := TSynSelectedColor.Create;
  FLineHighlightColor := TSynSelectedColor.Create;
  FFoldedCodeColor    := TSynSelectedColor.Create;
  FSelectedColor      := TSynSelectedColor.Create;
  AssignDefaultColors;
end;

procedure TEditorSettings.BeforeDestruction;
begin
  FSelectedColor.Free;
  FIncrementColor.Free;
  FHighlightAllColor.Free;
  FBracketMatchColor.Free;
  FMouseLinkColor.Free;
  FLineHighlightColor.Free;
  FFoldedCodeColor.Free;
  FFormSettings.Free;
  FHighlighters.Free;
  FEditorFont.Free;
  FHighlighterAttributes.Free;
  FAlignLinesSettings.Free;
  FSearchEngineSettings.Free;
  FCodeShaperSettings.Free;
  FCodeFilterSettings.Free;
  FHTMLViewSettings.Free;
  FMiniMapSettings.Free;
  FHexEditorSettings.Free;
  FChangedEventList.Free;
  inherited BeforeDestruction;
end;
{$endregion}

{$region 'event handlers' /fold}
procedure TEditorSettings.FFormSettingsChanged(Sender: TObject);
begin
  Changed;
end;
{$endregion}

{$region 'property access methods' /fold}
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

function TEditorSettings.GetBracketHighlightStyle: TSynEditBracketHighlightStyle;
begin
  Result := FBracketHighlightStyle;
end;

procedure TEditorSettings.SetBracketHighlightStyle(AValue: TSynEditBracketHighlightStyle);
begin
  if AValue <> BracketHighlightStyle then
  begin
    FBracketHighlightStyle := AValue;
    Changed;
  end;
end;

procedure TEditorSettings.SetAutoGuessHighlighterType(const AValue: Boolean);
begin
  if AValue <> AutoGuessHighlighterType then
  begin
    FAutoGuessHighlighterType := AValue;
  end;
end;

function TEditorSettings.GetBlockIndent: Integer;
begin
  Result := FBlockIndent;
end;

procedure TEditorSettings.SetBlockIndent(AValue: Integer);
begin
  if AValue <> BlockIndent then
  begin
    FBlockIndent := AValue;
    Changed;
  end;
end;

function TEditorSettings.GetBlockTabIndent: Integer;
begin
  Result := FBlockTabIndent;
end;

procedure TEditorSettings.SetBlockTabIndent(AValue: Integer);
begin
  if AValue <> BlockTabIndent then
  begin
    FBlockTabIndent := AValue;
    Changed;
  end;
end;

function TEditorSettings.GetBracketMatchColor: TSynSelectedColor;
begin
  Result := FBracketMatchColor;
end;

procedure TEditorSettings.SetBracketMatchColor(AValue: TSynSelectedColor);
begin
  FBracketMatchColor.Assign(AValue);
  Changed;
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

function TEditorSettings.GetCodeFilterSettings: TCodeFilterSettings;
begin
  Result := FCodeFilterSettings;
end;

procedure TEditorSettings.SetCodeFilterSettings(AValue: TCodeFilterSettings);
begin
  FCodeFilterSettings.Assign(AValue);
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

function TEditorSettings.GetExtraCharSpacing: Integer;
begin
  Result := FExtraCharSpacing;
end;

procedure TEditorSettings.SetExtraCharSpacing(AValue: Integer);
begin
  if AValue <> ExtraCharSpacing then
  begin
    FExtraCharSpacing := AValue;
    Changed;
  end;
end;

function TEditorSettings.GetExtraLineSpacing: Integer;
begin
  Result := FExtraLineSpacing;
end;

procedure TEditorSettings.SetExtraLineSpacing(AValue: Integer);
begin
  if AValue <> ExtraLineSpacing then
  begin
    FExtraLineSpacing := AValue;
    Changed;
  end;
end;

function TEditorSettings.GetFileName: string;
begin
  Result := FFileName;
end;

function TEditorSettings.GetFoldedCodeColor: TSynSelectedColor;
begin
  Result := FFoldedCodeColor;
end;

procedure TEditorSettings.SetFileName(const AValue: string);
begin
  if AValue <> FileName then
  begin
    FFileName := AValue;
  end;
end;

procedure TEditorSettings.SetFoldedCodeColor(AValue: TSynSelectedColor);
begin
  FFoldedCodeColor.Assign(AValue);
  Changed;
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
  FFormSettings.Assign(AValue);
end;

function TEditorSettings.GetHexEditorSettings: THexEditorSettings;
begin
  Result := FHexEditorSettings;
end;

procedure TEditorSettings.SetHexEditorSettings(AValue: THexEditorSettings);
begin
  FHexEditorSettings.Assign(AValue);
end;

function TEditorSettings.GetHighlightAllColor: TSynSelectedColor;
begin
  Result := FHighlightAllColor;
end;

procedure TEditorSettings.SetHighlightAllColor(AValue: TSynSelectedColor);
begin
  FHighlightAllColor.Assign(AValue);
  Changed;
end;

function TEditorSettings.GetHighlighterAttributes: THighlighterAttributes;
begin
  Result := FHighlighterAttributes;
end;

procedure TEditorSettings.SetHighlighterAttributes(AValue: THighlighterAttributes);
begin
  FHighlighterAttributes.Assign(AValue);
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

function TEditorSettings.GetHTMLViewSettings: THTMLViewSettings;
begin
  Result := FHTMLViewSettings;
end;

procedure TEditorSettings.SetHTMLViewSettings(AValue: THTMLViewSettings);
begin
  FHTMLViewSettings.Assign(AValue);
end;

function TEditorSettings.GetIncrementColor: TSynSelectedColor;
begin
  Result := FIncrementColor;
end;

function TEditorSettings.GetLanguageCode: string;
begin
  Result := FLanguageCode;
end;

procedure TEditorSettings.SetLanguageCode(AValue: string);
begin
  FLanguageCode := AValue;
end;

procedure TEditorSettings.SetIncrementColor(AValue: TSynSelectedColor);
begin
  FIncrementColor.Assign(AValue);
  Changed;
end;

function TEditorSettings.GetLineHighlightColor: TSynSelectedColor;
begin
  Result := FLineHighlightColor;
end;

procedure TEditorSettings.SetLineHighlightColor(AValue: TSynSelectedColor);
begin
  FLineHighlightColor.Assign(AValue);
  Changed;
end;

function TEditorSettings.GetMiniMapSettings: TMiniMapSettings;
begin
  Result := FMiniMapSettings;
end;

procedure TEditorSettings.SetMiniMapSettings(AValue: TMiniMapSettings);
begin
  FMiniMapSettings.Assign(AValue);
end;

function TEditorSettings.GetMouseLinkColor: TSynSelectedColor;
begin
  Result := FMouseLinkColor;
end;

procedure TEditorSettings.SetMouseLinkColor(AValue: TSynSelectedColor);
begin
  FMouseLinkColor.Assign(AValue);
  Changed;
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

function TEditorSettings.GetRightEdge: Integer;
begin
  Result := FRightEdge;
end;

procedure TEditorSettings.SetRightEdge(AValue: Integer);
begin
  if AValue <> RightEdge then
  begin
    FRightEdge := AValue;
    Changed;
  end;
end;

function TEditorSettings.GetRightEdgeColor: TColor;
begin
  Result := FRightEdgeColor;
end;

procedure TEditorSettings.SetRightEdgeColor(AValue: TColor);
begin
  if AValue <> RightEdgeColor then
  begin
    FRightEdgeColor := AValue;
    Changed;
  end;
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

function TEditorSettings.GetSelectedColor: TSynSelectedColor;
begin
  Result := FSelectedColor;
end;

procedure TEditorSettings.SetSelectedColor(AValue: TSynSelectedColor);
begin
  FSelectedColor.Assign(AValue);
  Changed;
end;

procedure TEditorSettings.SetSearchEngineSettings(AValue: TSearchEngineSettings);
begin
  FSearchEngineSettings := AValue;
end;

function TEditorSettings.GetShowSpecialCharacters: Boolean;
begin
  Result := FShowControlCharacters;
end;

procedure TEditorSettings.SetShowSpecialCharacters(const AValue: Boolean);
begin
  if AValue <> ShowSpecialCharacters then
  begin
    FShowControlCharacters := AValue;
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
    // we need to save here to make sure that any other instance runs with the
    // same configuration.
    Save;
    Changed;
  end;
end;

function TEditorSettings.GetTabWidth: Integer;
begin
  Result := FTabWidth;
end;

procedure TEditorSettings.SetTabWidth(AValue: Integer);
begin
  if AValue <> TabWidth then
  begin
    FTabWidth := AValue;
    Changed;
  end;
end;

function TEditorSettings.GetWantTabs: Boolean;
begin
  Result := FWantTabs;
end;

procedure TEditorSettings.SetWantTabs(AValue: Boolean);
begin
  if AValue <> WantTabs then
  begin
    FWantTabs := AValue;
    Changed;
  end;
end;

function TEditorSettings.GetXML: string;
begin
  Result := ReadFileToString(FileName);
end;
{$endregion}

{$region 'protected methods'}
procedure TEditorSettings.AssignDefaultColors;
begin
  BracketMatchColor.Background := clAqua;
  BracketMatchColor.Foreground := clNone;
  BracketMatchColor.FrameColor := clBlue;

  SelectedColor.Background := clMedGray;
  SelectedColor.BackAlpha  := 128;
  SelectedColor.Foreground := clNone;

  IncrementColor.Background := clMedGray;
  IncrementColor.BackAlpha  := 128;
  IncrementColor.Foreground := clNone;

  HighlightAllColor.Background := $000080FF; // orange
  HighlightAllColor.BackAlpha  := 128;
  HighlightAllColor.Foreground := clNone;
  HighlightAllColor.FrameColor := $00006BD7; // dark orange

  LineHighlightColor.Background := clYellow;
  LineHighlightColor.BackAlpha  := 128;
  LineHighlightColor.Foreground := clNone;
  LineHighlightColor.FrameColor := clOlive;
  LineHighlightColor.FrameAlpha := 64;
  LineHighlightColor.FrameStyle := slsDashed;

  FoldedCodeColor.Background := clSilver;
  FoldedCodeColor.BackAlpha  := 50;
  FoldedCodeColor.Foreground := clMedGray;
  FoldedCodeColor.FrameColor := clMedGray;
end;

procedure TEditorSettings.Changed;
begin
  FChangedEventList.CallNotifyEvents(Self);
end;
{$endregion}

{$region 'public methods' /fold}
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
      //Logger.Send('Settings SAVE', ObjectSaveToXmlString(Self));
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
        SYNS_XML_AttrIdentifier,
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
{$endregion}

end.

