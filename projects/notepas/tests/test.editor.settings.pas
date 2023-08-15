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

unit Test.Editor.Settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  fpcunit, testutils, testregistry,

  ts.Editor.Interfaces;

type

  { TTestEditorSettings }

  TTestEditorSettings = class(TTestCase)
  private
    FSettings : IEditorSettings;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

    procedure LoadSettings;

    procedure TestLoadSaveCommentTags;
    procedure TestLoadSaveHighlighters;

  published
    procedure TestLoadSettings;
    procedure TestSaveSettings;

    procedure TestLoadSaveSettings;
    procedure TestLoadSaveColors;
    procedure TestLoadSaveHighlighterAttributes;
    procedure TestLoadSaveToolSettings;

  end;

implementation

uses
  Graphics, Forms,

  SynEditHighlighter, SynHighlighterPas,

  ts.Core.Logger,

  ts.Editor.CodeShaper.Settings,

  ts.Editor.Resources, ts.Editor.Factories, ts.Editor.Factories.Settings,

  ts.Editor.Highlighters, ts.Editor.HighlighterAttributes;

{$REGION 'protected methods'}
procedure TTestEditorSettings.SetUp;
begin
  FSettings := TEditorFactories.CreateSettings(Application);
end;

procedure TTestEditorSettings.TearDown;
begin
  FSettings := nil;
end;

procedure TTestEditorSettings.LoadSettings;
begin
  FSettings := nil;
  FSettings := TEditorFactories.CreateSettings(Application);
  FSettings.Load;
end;
{$ENDREGION}

{$REGION 'published methods'}
procedure TTestEditorSettings.TestLoadSettings;
var
  C  : TComponent;
  HL : THighlighterItem;
begin
  //Logger.Enter(Self, 'TestLoadSettings');
  //Logger.SendComponent(FSettings.ToolSettings);
  //for C in FSettings.ToolSettings do
  //  Logger.SendComponent(C);
  //
  //for C in FSettings.Highlighters do
  //begin
  //  HL := C as THighlighterItem;
  //  Logger.SendComponent(HL);
  //  Logger.SendComponent(HL.SynHighlighter);
  //end;
  //
  //FSettings.Load;
  //
  //Logger.SendComponent(FSettings.ToolSettings);
  //for C in FSettings.ToolSettings do
  //  Logger.SendComponent(C);
  //
  //for C in FSettings.Highlighters do
  //  Logger.SendComponent(C);
  //Logger.Leave(Self, 'TestLoadSettings');
end;

procedure TTestEditorSettings.TestSaveSettings;
begin
  Logger.Enter(Self, 'TestSaveSettings');
  Logger.SendPersistent('Settings', FSettings as TPersistent);
  FSettings.Save;
  Logger.Leave(Self, 'TestSaveSettings');
end;

procedure TTestEditorSettings.TestLoadSaveSettings;
var
  SO : string;
  BO : Boolean;
  BN : Boolean;
  SN : string;
begin
  Logger.Enter(Self, 'TestLoadSaveSettings');
  LoadSettings;
  SO := FSettings.HighlighterType;
  BO := FSettings.DebugMode;
  Logger.Send('BO', BO);

  SN := 'CPP';
  BN := True;
  FSettings.HighlighterType := SN;
  FSettings.DebugMode := BN;
  FSettings.Save;

  LoadSettings;
  CheckEquals(SN, FSettings.HighlighterType);
  CheckEquals(BN, FSettings.DebugMode);

  // restore original values
  BO := True;
  FSettings.HighlighterType := SO;
  FSettings.DebugMode := BO;
  FSettings.Save;
  Logger.Leave(Self, 'TestLoadSaveSettings');
end;

procedure TTestEditorSettings.TestLoadSaveColors;
var
  CO : TColor;
  CN : TColor;
begin
  Logger.Enter(Self, 'TestLoadSaveColors');
  LoadSettings;
  CO := FSettings.Colors.HighlightAllColor.FrameColor;
  CN := clMaroon;
  FSettings.Colors.HighlightAllColor.FrameColor := CN;
  FSettings.Save;
  LoadSettings;
  CheckEquals(FSettings.Colors.HighlightAllColor.FrameColor, CN);

  // restore original color
  FSettings.Colors.HighlightAllColor.FrameColor := CO;
  FSettings.Save;

  LoadSettings;
  CheckEquals(FSettings.Colors.HighlightAllColor.FrameColor, CO);
  Logger.Leave(Self, 'TestLoadSaveColors');
end;

procedure TTestEditorSettings.TestLoadSaveHighlighterAttributes;
var
  CO  : TColor;
  CN  : TColor;
  HLA : THighlighterAttributesItem;
begin
  Logger.Enter(Self, 'TestLoadSaveHighlighterAttributes');
  LoadSettings;
  HLA := FSettings.HighlighterAttributes.ItemsByName['Section'];
  CO := HLA.Attributes.Foreground;

  CN := clRed;
  HLA.Attributes.Foreground := CN;
  FSettings.Save;

  LoadSettings;
  HLA := FSettings.HighlighterAttributes.ItemsByName['Section'];
  CheckEquals(HLA.Attributes.Foreground, CN);

  // restore original color
  HLA.Attributes.Foreground := CO;
  FSettings.Save;
  FSettings.Load;
  HLA := FSettings.HighlighterAttributes.ItemsByName['Section'];
  CheckEquals(HLA.Attributes.Foreground, CO);
  Logger.Leave(Self, 'TestLoadSaveHighlighterAttributes');
end;

procedure TTestEditorSettings.TestLoadSaveHighlighters;
var
  HI : THighlighterItem;
  SO : string;
  SN : string;
begin
  Logger.Enter(Self, 'TestLoadSaveHighlighters');
  LoadSettings;
  HI := FSettings.Highlighters.ItemsByName['PAS'];

  if Assigned(HI) then
  begin
    SO := HI.FileExtensions;
    SN := 'test';
    HI.FileExtensions := SN;
    if Assigned(HI.SynHighlighter) then
      (HI.SynHighlighter as TSynPasSyn).CompilerMode := pcmObjFPC;
    FSettings.Save;

    LoadSettings;
    HI := FSettings.Highlighters.ItemsByName['PAS'];
    CheckEquals(HI.FileExtensions, SN);
    CheckTrue((HI.SynHighlighter as TSynPasSyn).CompilerMode = pcmObjFPC);

    // restore original values
    HI.FileExtensions := SO;
    FSettings.Save;

    LoadSettings;
    CheckEquals(HI.FileExtensions, SO);

    FSettings.Save;
  end;
  Logger.Leave(Self, 'TestLoadSaveHighlighters');
end;

procedure TTestEditorSettings.TestLoadSaveCommentTags;
var
  HI   : THighlighterItem;
  LCTO : string;
  LCTN : string;
  LCT  : string;
  SO   : string;
  SN   : string;
begin
  Logger.Enter(Self, 'TestLoadSaveCommentTags');
  //LoadSettings;
  //HI := FSettings.Highlighters.ItemsByName['XML'];
  //
  //if Assigned(HI) then
  //begin
  //  LCT := HI.LineCommentTag;
  //  SO := HI.FileExtensions;
  //  SN := 'test';
  //  HI.FileExtensions := SN;
  //  FSettings.Save;
  //
  //  LoadSettings;
  //  HI := FSettings.Highlighters.ItemsByName['PAS'];
  //  CheckEquals(HI.FileExtensions, SN);
  //  CheckTrue((HI.SynHighlighter as TSynPasSyn).CompilerMode = pcmObjFPC);
  //
  //  // restore original values
  //  HI.FileExtensions := SO;
  //  FSettings.Save;
  //
  //  LoadSettings;
  //  CheckEquals(HI.FileExtensions, SO);
  //
  //  FSettings.Save;
  //end;
  Logger.Leave(Self, 'TestLoadSaveCommentTags');
end;

procedure TTestEditorSettings.TestLoadSaveToolSettings;
var
  CSS : TCodeShaperSettings;
  NO  : Integer;
  NN  : Integer;
begin
  Logger.Enter(Self, 'TestLoadSaveToolSettings');
  LoadSettings;
  CSS := FSettings.CodeShaperSettings;
  NO := CSS.Width;
  NN := 200;
  CSS.Width := NN;
  FSettings.Save;

  LoadSettings;
  CSS := FSettings.CodeShaperSettings;
  CheckEquals(NN, CSS.Width);

  NN := 400;
  CSS.Width := NN;
  FSettings.Save;

  LoadSettings;
  CSS := FSettings.CodeShaperSettings;
  CheckEquals(NN, CSS.Width);

  // restore original value
  CSS.Width := NO;
  FSettings.Save;
  Logger.Leave(Self, 'TestLoadSaveToolSettings');
end;
{$ENDREGION}

initialization
  RegisterTest(TTestEditorSettings);

end.

