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
    FSLBackup : TStringList;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestLoadSettings;
    procedure TestSaveSettings;

    procedure TestLoadSaveSettings;
    procedure TestLoadSaveColors;
    procedure TestLoadSaveHighlighters;
    procedure TestLoadSaveToolSettings;

  public
    procedure TestLoadSaveHighlighterAttributes;

  end;

implementation

uses
  Graphics, Forms,

  SynEditHighlighter,

  ts.Core.SharedLogger,

  ts.Editor.CodeShaper.Settings,

  ts_Editor_Resources, ts.Editor.Factories.Settings,

  ts.Editor.Highlighters, ts.Editor.HighlighterAttributes;

procedure TTestEditorSettings.TestLoadSettings;
begin
  Logger.EnterMethod('TTestEditorSettings.TestLoadSettings');
  FSettings.Load;
  Logger.ExitMethod('TTestEditorSettings.TestLoadSettings');
end;

procedure TTestEditorSettings.TestSaveSettings;
begin
  Logger.EnterMethod('TTestEditorSettings.TestSaveSettings');
  Logger.Send('Settings', FSettings);
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
  FSettings := nil;
  FSettings := TEditorSettingsFactory.CreateEditorSettings(Application);
  FSettings.Load;
  SO := FSettings.HighlighterType;
  BO := FSettings.DebugMode;
  Logger.Send('BO', BO);

  SN := 'CPP';
  BN := True;
  FSettings.HighlighterType := SN;
  FSettings.DebugMode := BN;
  FSettings.Save;

  FSettings := nil;
  FSettings := TEditorSettingsFactory.CreateEditorSettings(Application);
  FSettings.Load;
  CheckEquals(SN, FSettings.HighlighterType);
  CheckEquals(BN, FSettings.DebugMode);

  SN := 'PAS';
  BN := False;
  FSettings.HighlighterType := SN;
  FSettings.DebugMode := BN;
  FSettings.Save;
  FSettings.Load;
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

  FSettings := nil;
  FSettings := TEditorSettingsFactory.CreateEditorSettings(Application);
  FSettings.Load;

  CO := FSettings.Colors.HighlightAllColor.FrameColor;
  CN := clMaroon;
  FSettings.Colors.HighlightAllColor.FrameColor := CN;
  FSettings.Save;

  FSettings := nil;
  FSettings := TEditorSettingsFactory.CreateEditorSettings(Application);
  FSettings.Load;
  CheckEquals(FSettings.Colors.HighlightAllColor.FrameColor, CN);

  // restore original color
  FSettings.Colors.HighlightAllColor.FrameColor := CO;
  FSettings.Save;

  FSettings := nil;
  FSettings := TEditorSettingsFactory.CreateEditorSettings(Application);
  FSettings.Load;
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
  HLA := FSettings.HighlighterAttributes.ItemsByName['Section'];
  CO := HLA.Attributes.Foreground;

  CN := clRed;
  HLA.Attributes.Foreground := CN;
  FSettings.Save;
  FSettings.Load;
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
  HI := FSettings.Highlighters.ItemsByName['DIFF'];

  SO := HI.FileExtensions;
  SN := 'test';
  HI.FileExtensions := SN;
  FSettings.Save;
  FSettings.Load;
  CheckEquals(HI.FileExtensions, SN);

  // restore original values
  HI.FileExtensions := SO;
  FSettings.Save;
  FSettings.Load;
  CheckEquals(HI.FileExtensions, SO);

  //HI.InitSynHighlighter;
  FSettings.Save;
  Logger.ExitMethod('TTestEditorSettings.TestLoadSaveHighlighters');
end;

procedure TTestEditorSettings.TestLoadSaveToolSettings;
var
  CSS : TCodeShaperSettings;
  NO  : Integer;
  NN  : Integer;
begin
  CSS := FSettings.ToolSettings.ItemsByClass[TCodeShaperSettings] as TCodeShaperSettings;
  NO := CSS.Width;

  NN := 200;
  CSS.Width := NN;
  FSettings.Save;
  FSettings.Load;
  CSS := FSettings.ToolSettings.ItemsByClass[TCodeShaperSettings] as TCodeShaperSettings;
  CheckEquals(NN, CSS.Width);

  NN := 400;
  CSS.Width := NN;
  FSettings.Save;
  FSettings.Load;
  CSS := FSettings.ToolSettings.ItemsByClass[TCodeShaperSettings] as TCodeShaperSettings;
  CheckEquals(NN, CSS.Width);

  // restore original value
  CSS.Width := NO;
  FSettings.Save;
end;

procedure TTestEditorSettings.SetUp;
begin
  FSettings := TEditorSettingsFactory.CreateEditorSettings(Application);
  FSLBackup := TStringList.Create;
  FSLBackup.LoadFromFile(FSettings.FileName);
end;

procedure TTestEditorSettings.TearDown;
begin
  //FSLBackup.SaveToFile(FSettings.FileName);
  FSettings := nil;
  FreeAndNil(FSLBackup);
end;

initialization
  RegisterTest(TTestEditorSettings);

end.

