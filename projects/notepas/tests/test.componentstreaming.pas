unit Test.ComponentStreaming;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,

  Test.ComponentStreaming.TestComponents,

  ts.Editor.Tools.Settings, ts.Editor.Factories.Settings;

type

  { TTestComponentStreaming }

  TTestComponentStreaming = class(TTestCase)
  private
    FParent: TParentComponent;
    FLFMFile : string;
    FXMLFile : string;
    FComponent: TComponent;
    FToolSettings : TEditorToolSettingsList;

  protected
    procedure SetUp; override;
    procedure TearDown; override;


    procedure LoadXML(AComponent: TComponent);
    procedure SaveXML(AComponent: TComponent);

    procedure LoadLFM(AComponent: TComponent);
    procedure SaveLFM(AComponent: TComponent);

    procedure AddChildrenToParentComponent;


  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  published

    procedure AddChildrenToComponent;

    procedure TestLoadXML;
    procedure TestSaveXML;

    procedure TestLoadLFM;
    procedure TestSaveLFM;


  end;

implementation

uses
  Forms,

  ts.Core.sharedlogger,
  ts.Core.NativeXml, ts.Core.NativeXml.ObjectStorage;

{ TTestComponentStreaming }

procedure TTestComponentStreaming.SetUp;
begin
  inherited SetUp;
end;

procedure TTestComponentStreaming.TearDown;
begin
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

procedure TTestComponentStreaming.AddChildrenToParentComponent;
var
  I : Integer;
  Child : TChildComponent;
begin
  Logger.EnterMethod('TTestComponentStreaming.AddChildrenToParentComponent');
  for I := 0 to 10 do
  begin
    Child := TChildComponent.Create(FParent);
    Child.Name := 'Child' + IntToStr(I);
    Child.TestString := IntToStr(I);
    FParent.Children.Add(Child);
  end;
  Logger.SendComponent(FParent);
  Logger.ExitMethod('TTestComponentStreaming.AddChildrenToParentComponent');
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
  FXMLFile := ExtractFilePath(Application.ExeName) + 'test.xml';
  FLFMFile := ExtractFilePath(Application.ExeName) + 'test.lfm';
  FParent := TParentComponent.Create(nil);
  //FComponent := TComponent.Create(nil);
  FToolSettings := TEditorToolSettingsList.Create(nil);
  FComponent := FParent;
end;

procedure TTestComponentStreaming.BeforeDestruction;
begin
  FParent.Free;
  //FComponent.Free;
  FToolSettings.Free;
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


initialization
  RegisterTest(TTestComponentStreaming);

end.

