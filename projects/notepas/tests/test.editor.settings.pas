{
  Copyright (C) 2013-2014 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Test.Editor.Settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,

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


  public
    procedure AfterConstruction; override;
  published
    procedure TestLoadSettings;
    procedure TestSaveSettings;

//    public
    procedure TestLoadSaveSettings;

    procedure TestLoadSaveColors;

    procedure TestLoadSaveHighlighters;
    procedure TestLoadSaveHighlighterAttributes;


    procedure TestLoadSaveToolSettings;

  end;

implementation

uses
  Graphics, Forms,

  SynEditHighlighter, SynHighlighterPas,

  ts.Core.SharedLogger,

  ts.Editor.CodeShaper.Settings,

  ts_Editor_Resources, ts.Editor.Factories.Settings,

  ts.Editor.Highlighters, ts.Editor.HighlighterAttributes;

procedure TTestEditorSettings.TestLoadSettings;
var
  C : TComponent;
begin
  Logger.EnterMethod('TTestEditorSettings.TestLoadSettings');
    Logger.SendComponent(FSettings.ToolSettings);
  for C in FSettings.ToolSettings do
    Logger.SendComponent(C);

  for C in FSettings.Highlighters do
    Logger.SendComponent(C);

  TEditorSettingsFactory.RegisterHighlighters(FSettings.Highlighters);

  FSettings.Load;


  Logger.SendComponent(FSettings.ToolSettings);
  for C in FSettings.ToolSettings do
    Logger.SendComponent(C);

  for C in FSettings.Highlighters do
    Logger.SendComponent(C);


  Logger.ExitMethod('TTestEditorSettings.TestLoadSettings');
  //FSettings.Save;
end;

procedure TTestEditorSettings.TestSaveSettings;
begin
  Logger.EnterMethod('TTestEditorSettings.TestSaveSettings');
  Logger.Send('Settings', FSettings);
  TEditorSettingsFactory.RegisterToolSettings(FSettings.ToolSettings);
  TEditorSettingsFactory.RegisterHighlighters(FSettings.Highlighters);
  FSettings.Save;
  Logger.ExitMethod('TTestEditorSettings.TestSaveSettings');
end;

procedure TTestEditorSettings.TestLoadSaveSettings;
var
  SO : string;
  BO : Boolean;
  BN : Boolean;
  SN : string;
begin
  Logger.EnterMethod('TTestEditorSettings.TestLoadSaveSettings');
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

  SN := 'PAS';
  BN := False;
  FSettings.HighlighterType := SN;
  FSettings.DebugMode := BN;
  FSettings.Save;

  //FSettings.Load;
  LoadSettings;
  CheckEquals(SN, FSettings.HighlighterType);
  CheckEquals(BN, FSettings.DebugMode);

  // restore original values
  BO := True;
  FSettings.HighlighterType := SO;
  FSettings.DebugMode := BO;
  FSettings.Save;
  Logger.ExitMethod('TTestEditorSettings.TestLoadSaveSettings');
end;

procedure TTestEditorSettings.TestLoadSaveColors;
var
  CO : TColor;
  CN : TColor;
begin
  Logger.EnterMethod('TTestEditorSettings.TestLoadSaveColors');

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
  Logger.ExitMethod('TTestEditorSettings.TestLoadSaveColors');
end;

procedure TTestEditorSettings.TestLoadSaveHighlighterAttributes;
var
  CO  : TColor;
  CN  : TColor;
  HLA : THighlighterAttributesItem;
begin
  Logger.EnterMethod('TTestEditorSettings.TestLoadSaveHighlighterAttributes');
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
  Logger.ExitMethod('TTestEditorSettings.TestLoadSaveHighlighterAttributes');
end;

procedure TTestEditorSettings.TestLoadSaveHighlighters;
var
  HI : THighlighterItem;
  SO : string;
  SN : string;
begin
  Logger.EnterMethod('TTestEditorSettings.TestLoadSaveHighlighters');
  TEditorSettingsFactory.RegisterClasses;
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

    //HI.InitSynHighlighter;
    FSettings.Save;
  end;
  Logger.ExitMethod('TTestEditorSettings.TestLoadSaveHighlighters');
end;

procedure TTestEditorSettings.TestLoadSaveToolSettings;
var
  CSS : TCodeShaperSettings;
  NO  : Integer;
  NN  : Integer;
begin
  LoadSettings;
  CSS := FSettings.ToolSettings.ItemsByClass[TCodeShaperSettings] as TCodeShaperSettings;
  NO := CSS.Width;
  NN := 200;
  CSS.Width := NN;
  FSettings.Save;

  LoadSettings;
  CSS := FSettings.ToolSettings.ItemsByClass[TCodeShaperSettings] as TCodeShaperSettings;
  CheckEquals(NN, CSS.Width);

  NN := 400;
  CSS.Width := NN;
  FSettings.Save;

  LoadSettings;
  CSS := FSettings.ToolSettings.ItemsByClass[TCodeShaperSettings] as TCodeShaperSettings;
  CheckEquals(NN, CSS.Width);

  // restore original value
  CSS.Width := NO;
  FSettings.Save;
end;

procedure TTestEditorSettings.SetUp;
begin
  FSettings := TEditorSettingsFactory.CreateEditorSettings(Application);
end;

procedure TTestEditorSettings.TearDown;
begin
  FSettings := nil;
end;

procedure TTestEditorSettings.LoadSettings;
begin
  FSettings := nil;
  FSettings := TEditorSettingsFactory.CreateEditorSettings(Application);
  FSettings.Load;
end;

procedure TTestEditorSettings.AfterConstruction;
begin
  inherited AfterConstruction;
end;

initialization
  RegisterTest(TTestEditorSettings);

end.

