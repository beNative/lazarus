{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Test.ComponentStreaming;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, TypInfo,

  fpjson, fpjsonrtti,
  fpcunit, testutils, testregistry,

  Test.ComponentStreaming.TestComponents,

  ts.Editor.Factories.Settings;

type

  { TTestComponentStreaming }

  TTestComponentStreaming = class(TTestCase)
  private
    FParent     : TParentComponent;
    FLFMFile    : string;
    FXMLFile    : string;
    FJSONFile   : string;
    FComponent  : TComponent;
    FStreamer   : TJSONStreamer;
    FDeStreamer : TJSONDeStreamer;

    procedure FDeStreamerRestoreProperty(
      Sender      : TObject;
      AObject     : TObject;
      Info        : PPropInfo;
      AValue      : TJSONData;
      var Handled : Boolean
    );

  protected
    procedure SetUp; override;
    procedure TearDown; override;

    procedure LoadLFM(AComponent: TComponent);
    procedure SaveLFM(AComponent: TComponent);

    procedure LoadJSON(AComponent: TComponent);
    procedure SaveJSON(AComponent: TComponent);

    procedure AddChildrenToParentComponent;
    procedure AddChildrenToComponent;

  public
    procedure AfterConstruction; override;

  published
    procedure TestLoadLFM;
    procedure TestSaveLFM;

    procedure TestLoadJSON;
    procedure TestSaveJSON;

  end;

implementation

uses
  Forms,

  FileUtil,

  ts.Core.Logger;

{$REGION 'construction and destruction'}
procedure TTestComponentStreaming.AfterConstruction;
begin
  inherited AfterConstruction;
  Logger.Clear;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure DoOngetObject(Sender: TOBject; AObject: TObject; Info: PPropInfo;
  AData: TJSONObject; DataName: TJSONStringType; Var AValue: TObject);
begin
  Logger.Enter('DoOngetObject');
  Logger.Send('DataName', DataName);
  Logger.SendObject('AValue', AValue);
  Logger.Leave('DoOngetObject');
end;

procedure TTestComponentStreaming.FDeStreamerRestoreProperty(Sender: TObject;
  AObject: TObject; Info: PPropInfo; AValue: TJSONData; var Handled: Boolean);
var
  C : TComponent = nil;
  I : Integer;
  A : TJSONArray;
begin
  Logger.Enter(Self, 'FDeStreamerRestoreProperty');
  Logger.Send('AValueType', GetEnumName(
    TypeInfo(AValue.JSONType), Integer(AValue.JSONType))
  );
  Logger.SendText(AValue.FormatJSON, 'JSON');
  if AValue.JSONType = jtArray then
  begin
    A := AValue as TJSONArray;
    for I := 0 to A.Count - 1 do
    begin
      Logger.SendText(AValue.Items[I].FormatJSON, 'JSON');
      FDeStreamer.JSONToObject(AValue.Items[I].FormatJSON, C);
      if Assigned(C) then
        FParent.Children.Add(C);
    end;
  end;
  Logger.Leave(Self, 'FDeStreamerRestoreProperty');
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TTestComponentStreaming.SetUp;
begin
  inherited SetUp;
  FXMLFile   := ExtractFilePath(Application.ExeName) + 'test.xml';
  FLFMFile   := ExtractFilePath(Application.ExeName) + 'test.lfm';
  FJSONFile  := ExtractFilePath(Application.ExeName) + 'test.json';
  FParent    := TParentComponent.Create(nil);
  FComponent := FParent;

  FStreamer         := TJSONStreamer.Create(nil);
  FStreamer.Options := FStreamer.Options + [jsoComponentsInline];

  FDeStreamer                   := TJSONDeStreamer.Create(nil);
  FDeStreamer.OngetObject       := @DoOngetObject;
  FDeStreamer.OnRestoreProperty := FDeStreamerRestoreProperty;
end;

procedure TTestComponentStreaming.TearDown;
begin
  FComponent := nil;
  FreeAndNil(FParent);
  FreeAndNil(FDeStreamer);
  FreeAndNil(FStreamer);
  inherited TearDown;
end;

procedure TTestComponentStreaming.LoadLFM(AComponent: TComponent);
var
  MS : TMemoryStream;
  FS : TFileStream;
begin
  if FileExists(FLFMFile) then
  begin
    FS := TFileStream.Create(FLFMFile, fmOpenRead);
    try
      MS := TMemoryStream.Create;
      try
        ObjectTextToBinary(FS, MS);
        MS.Position := 0;
        MS.ReadComponent(AComponent);
      finally
        MS.Free;
      end;
    finally
      FS.Free;
    end;
    Logger.SendComponent(AComponent);
  end;
end;

procedure TTestComponentStreaming.SaveLFM(AComponent: TComponent);
var
  MS : TMemoryStream;
  FS : TFileStream;
begin
  MS := TMemoryStream.Create;
  try
    MS.WriteComponent(AComponent);
    FS := TFileStream.Create(FLFMFile, fmCreate);
    try
      MS.Position := 0;
      ObjectBinaryToText(MS, FS);
    finally
      FS.Free;
    end;
  finally
    MS.Free;
  end;
end;

procedure TTestComponentStreaming.LoadJSON(AComponent: TComponent);
var
  S : string;
begin
  if FileExists(FJSONFile) then
  begin
    S := ReadFileToString(FJSONFile);
    FDeStreamer.JSONToObject(S, AComponent);
  end;
end;

procedure TTestComponentStreaming.SaveJSON(AComponent: TComponent);
var
  SL : TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := FStreamer.ObjectToJSON(AComponent).FormatJSON;
    SL.SaveToFile(FJSONFile);
  finally
    SL.Free;
  end;
end;

procedure TTestComponentStreaming.AddChildrenToParentComponent;
var
  I      : Integer;
  LChild : TChildComponent;
begin
  for I := 0 to 10 do
  begin
    LChild := TChildComponent.Create(FParent);
    LChild.Name := 'AddChildrenToParentComponentChild' + IntToStr(I);
    LChild.TestString := IntToStr(I);
    FParent.Children.Add(LChild);
  end;
end;

procedure TTestComponentStreaming.AddChildrenToComponent;
var
  I      : Integer;
  LChild : TComponent;
begin
  for I := 0 to 10 do
  begin
    LChild := TComponent.Create(FComponent);
    LChild.Name := 'AddChildrenToComponentChild' + IntToStr(I);
  end;
end;
{$ENDREGION}

{$REGION 'published methods'}
procedure TTestComponentStreaming.TestLoadLFM;
var
  C : TComponent;
begin
  Logger.Enter(Self, 'TestLoadLFM');
  LoadLFM(FComponent);
  Logger.SendComponent(FComponent);

  for C in FParent do
  begin
    Logger.SendComponent(C);
    if Assigned(C.GetParentComponent) then
      Logger.SendComponent(C.GetParentComponent);
  end;
  Logger.Leave(Self, 'TestLoadLFM');
end;

procedure TTestComponentStreaming.TestSaveLFM;
begin
  Logger.Enter(Self, 'TestSaveLFM');
  if FComponent.ComponentCount = 0 then
  begin
    AddChildrenToParentComponent;
  end;
  SaveLFM(FComponent);
  Logger.Leave(Self, 'TestSaveLFM');
end;

procedure TTestComponentStreaming.TestLoadJSON;
var
  C : TComponent;
begin
  Logger.Enter(Self, 'TestLoadJSON');
  LoadJSON(FParent);
  Logger.SendComponent(FParent);

  for C in FParent do
  begin
    Logger.SendComponent(C);
    if Assigned(C.GetParentComponent) then
      Logger.SendComponent(C.GetParentComponent);
  end;
  Logger.Leave(Self, 'TestLoadJSON');
end;

procedure TTestComponentStreaming.TestSaveJSON;
begin
  Logger.Enter(Self, 'TestSaveJSON');
  AddChildrenToParentComponent;
  SaveJSON(FParent);
  Logger.Leave(Self, 'TestSaveJSON');
end;
{$ENDREGION}

initialization
  RegisterTest(TTestComponentStreaming);

end.

