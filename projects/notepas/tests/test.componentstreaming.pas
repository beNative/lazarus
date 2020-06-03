{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Test.ComponentStreaming;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, TypInfo,

  fpjson, fpjsonrtti,
  fpcunit, testutils, testregistry,

  Test.ComponentStreaming.TestComponents,

  ts.Editor.Tools.Settings, ts.Editor.Factories.Settings;

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

    procedure FDeStreamerRestoreProperty(Sender: TObject; AObject: TObject;
      Info: PPropInfo; AValue: TJSONData; Var Handled: Boolean);

  protected
    procedure SetUp; override;
    procedure TearDown; override;

    procedure LoadXML(AComponent: TComponent);
    procedure SaveXML(AComponent: TComponent);

    procedure LoadLFM(AComponent: TComponent);
    procedure SaveLFM(AComponent: TComponent);

    procedure LoadJSON(AComponent: TComponent);
    procedure SaveJSON(AComponent: TComponent);

    procedure AddChildrenToParentComponent;
    procedure AddChildrenToComponent;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  published
    procedure TestLoadXML;
    procedure TestSaveXML;

    procedure TestLoadLFM;
    procedure TestSaveLFM;

    procedure TestLoadJSON;
    procedure TestSaveJSON;

  end;

implementation

uses
  Forms,

  FileUtil,

  ts.Core.Logger, ts.Core.NativeXml, ts.Core.NativeXml.ObjectStorage;

{ TTestComponentStreaming }

procedure DoOngetObject(Sender: TOBject; AObject: TObject; Info: PPropInfo;
  AData: TJSONObject; DataName: TJSONStringType; Var AValue: TObject);
begin
  Logger.Info(DataName);
end;

procedure TTestComponentStreaming.FDeStreamerRestoreProperty(Sender: TObject;
  AObject: TObject; Info: PPropInfo; AValue: TJSONData; Var Handled: Boolean);
var
  C : TComponent;
  I: Integer;
  A : TJSONArray;
begin
  //Logger.Track('FDeStreamerRestoreProperty');
  Logger.Send('AValueType', Integer(AValue.JSONType));
  Logger.Info(AValue.AsJSON);
  if AValue.JSONType = jtArray then
  begin
    A := AValue as TJSONArray;
    for I := 0 to A.Count - 1 do
    begin
      ;
      Logger.Info(AValue.Items[I].AsJSON);
      FDeStreamer.JSONToObject(AValue.Items[I].AsJSON, C);
      if Assigned(C) then
      FParent.Children.Add(C);
    end;
  end;
end;

procedure TTestComponentStreaming.SetUp;
begin
  inherited SetUp;
  FStreamer := TJSONStreamer.Create(nil);
  FStreamer.Options := FStreamer.Options + [jsoComponentsInline];
  FDeStreamer := TJSONDeStreamer.Create(nil);
  FDeStreamer.OngetObject := @DoOngetObject;
  FDeStreamer.OnRestoreProperty := FDeStreamerRestoreProperty;
end;

procedure TTestComponentStreaming.TearDown;
begin
  FreeAndNil(FDeStreamer);
  FreeAndNil(FStreamer);
  inherited TearDown;
end;

procedure TTestComponentStreaming.LoadXML(AComponent: TComponent);
var
  Reader : TXmlObjectReader;
  Doc    : TNativeXml;
begin
  if FileExists(FXMLFile) then
  begin
    Doc := TNativeXml.Create(nil);
    try
      Doc.LoadFromFile(FXMLFile);
      Reader := TXmlObjectReader.Create;
      try
        Reader.ReadComponent(Doc.Root, AComponent, nil);
      finally
        FreeAndNil(Reader);
      end;
    finally
      FreeAndNil(Doc);
    end;
  end;
end;

procedure TTestComponentStreaming.SaveXML(AComponent: TComponent);
var
  Writer : TXmlObjectWriter;
  Doc    : TNativeXml;
begin
  Doc := TNativeXml.CreateName('Root', nil);
  try
    Writer := TXmlObjectWriter.Create;
    try
      Doc.XmlFormat := xfReadable;
      Writer.WriteComponent(Doc.Root, AComponent);
      Doc.SaveToFile(FXMLFile);
    finally
      FreeAndNil(Writer);
    end;
  finally
    FreeAndNil(Doc);
  end;
end;

procedure TTestComponentStreaming.LoadLFM(AComponent: TComponent);
var
  MS : TMemoryStream;
  FS : TFileStream;
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
  I : Integer;
  Child : TChildComponent;
begin
  for I := 0 to 10 do
  begin
    Child := TChildComponent.Create(FParent);
    Child.Name := 'Child' + IntToStr(I);
    Child.TestString := IntToStr(I);
    FParent.Children.Add(Child);
  end;
end;

procedure TTestComponentStreaming.AddChildrenToComponent;
var
  I : Integer;
  Child : TComponent;
begin
  for I := 0 to 10 do
  begin
    Child := TComponent.Create(FComponent);
    Child.Name := 'Child' + IntToStr(I);
  end;
end;

procedure TTestComponentStreaming.AfterConstruction;
begin
  inherited AfterConstruction;
  FXMLFile  := ExtractFilePath(Application.ExeName) + 'test.xml';
  FLFMFile  := ExtractFilePath(Application.ExeName) + 'test.lfm';
  FJSONFile := ExtractFilePath(Application.ExeName) + 'test.json';
  FParent := TParentComponent.Create(nil);
  FComponent := FParent;
end;

procedure TTestComponentStreaming.BeforeDestruction;
begin
  FParent.Free;
  inherited BeforeDestruction;
end;

procedure TTestComponentStreaming.TestLoadXML;
var
  C : TComponent;
begin
  LoadXML(FComponent);
  Logger.SendComponent(FComponent);
  for C in FComponent do
  begin
    Logger.Send('Component', C);
    if Assigned(C.GetParentComponent) then
      Logger.Send('ParentComponent', C.GetParentComponent);
    if Assigned(C.Owner) then
      Logger.Send('Owner', C.Owner);
  end;
  SaveXML(FComponent);
end;

procedure TTestComponentStreaming.TestSaveXML;
var
  C : TComponent;
begin
  AddChildrenToParentComponent;
  //SaveXML(FParent);
  //AddChildrenToComponent;
  //TEditorSettingsFactory.RegisterToolSettings(FToolSettings);

  // for C in FComponent do
  //begin
  //  Logger.Send('Component', C);
  //  if Assigned(C.GetParentComponent) then
  //    Logger.Send('ParentComponent', C.GetParentComponent);
  //  if Assigned(C.Owner) then
  //        Logger.Send('Owner', C.Owner);
  //end;

  SaveXML(FComponent);
end;

procedure TTestComponentStreaming.TestLoadLFM;
var
  C : TComponent;
begin
  LoadLFM(FComponent);
  Logger.SendComponent(FComponent);

  for C in FParent do
  begin
    Logger.SendComponent(C);
    if Assigned(C.GetParentComponent) then
      Logger.SendComponent(C.GetParentComponent);
  end;
end;

procedure TTestComponentStreaming.TestSaveLFM;
begin
  if FComponent.ComponentCount = 0 then
  begin
    AddChildrenToParentComponent;
  end;
  SaveLFM(FComponent);
end;

procedure TTestComponentStreaming.TestLoadJSON;
var
  C : TComponent;
begin
  LoadJSON(FParent);
  Logger.SendComponent(FParent);

  for C in FParent do
  begin
    Logger.SendComponent(C);
    if Assigned(C.GetParentComponent) then
      Logger.SendComponent(C.GetParentComponent);
  end;
end;

procedure TTestComponentStreaming.TestSaveJSON;
begin
  AddChildrenToParentComponent;
  SaveJSON(FParent);
end;

initialization
  RegisterTest(TTestComponentStreaming);

end.

