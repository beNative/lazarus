unit Test.ComponentStreaming;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,

  Test.ComponentStreaming.TestComponents;

type

  { TTestEditorSettings }

  { TTestComponentStreaming }

  TTestComponentStreaming = class(TTestCase)
  private
    FParent: TParentComponent;
    FLFMFile : string;
    FXMLFile : string;
    FComponent: TComponent;


  protected
    procedure SetUp; override;
    procedure TearDown; override;


    procedure LoadXML(AComponent: TComponent);
    procedure SaveXML(AComponent: TComponent);

    procedure LoadLFM(AComponent: TComponent);
    procedure SaveLFM(AComponent: TComponent);

    procedure AddChildrenToParentComponent;
    procedure AddChildrenToComponent;

  public
    procedure AfterConstruction; override;

  published
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
var
  Child : TComponent;
  I     : Integer;
begin
  inherited SetUp;
  FParent := TParentComponent.Create(nil);
  FComponent := TComponent.Create(nil);
end;

procedure TTestComponentStreaming.TearDown;
begin
  FParent.Free;
  FComponent.Free;
  inherited TearDown;
end;

procedure TTestComponentStreaming.LoadXML(AComponent: TComponent);
var
  Reader : TXmlObjectReader;
  Doc    : TNativeXml;
  S      : string;
begin
  Logger.EnterMethod('TEditorSettings.Load');
  S := ExtractFilePath(Application.ExeName) + 'test.xml';
  if FileExists(S) then
  begin
    Doc := TNativeXml.Create(nil);
    try
      Doc.LoadFromFile(S);
      Reader := TXmlObjectReader.Create;
      try
        Reader.ReadComponent(Doc.Root, AComponent, nil);
      finally
        FreeAndNil(Reader);
      end;
      Logger.Send('Settings loaded');
    finally
      FreeAndNil(Doc);
    end;
  end;
  Logger.ExitMethod('TEditorSettings.Load');
end;

procedure TTestComponentStreaming.SaveXML(AComponent: TComponent);
var
  Writer : TXmlObjectWriter;
  Doc    : TNativeXml;
  S      : string;
begin
  Logger.SendCallStack('Save');
  Logger.EnterMethod('TEditorSettings.Save');
  S := ExtractFilePath(Application.ExeName) + 'test.xml';
  Doc := TNativeXml.CreateName('Root', nil);
  try
    Writer := TXmlObjectWriter.Create;
    try
      //Logger.Send('Settings SAVE', ObjectSaveToXmlString(Self));
      Doc.XmlFormat := xfReadable;
      Writer.WriteComponent(Doc.Root, AComponent);
      Doc.SaveToFile(S);
    finally
      FreeAndNil(Writer);
    end;
  finally
    FreeAndNil(Doc);
  end;
  Logger.ExitMethod('TEditorSettings.Save');
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
  for I := 0 to 10 do
  begin
    Child := TChildComponent.Create(nil);
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
  FXMLFile := ExtractFilePath(Application.ExeName) + 'test.xml';
  FLFMFile := ExtractFilePath(Application.ExeName) + 'test.lfm';
end;

procedure TTestComponentStreaming.TestLoadXML;
var
  C : TComponent;
begin
  //LoadXML(FParent);
  //Logger.SendComponent(FParent);
  //for C in FParent do
  //begin
  //  Logger.SendComponent(C);
  //  if Assigned(C.GetParentComponent) then
  //    Logger.SendComponent(C.GetParentComponent);
  //end;
  LoadXML(FComponent);
  Logger.SendComponent(FComponent);
  for C in FComponent do
  begin
    Logger.SendComponent(C);
    if Assigned(C.GetParentComponent) then
      Logger.SendComponent(C.GetParentComponent);
  end;
end;

procedure TTestComponentStreaming.TestSaveXML;
begin
  //AddChildrenToParentComponent;
  //SaveXML(FParent);
  AddChildrenToComponent;
  SaveXML(FComponent);
end;

procedure TTestComponentStreaming.TestLoadLFM;
var
  C : TComponent;
begin
  LoadLFM(FParent);
  Logger.SendComponent(FParent);

  for C in FParent do
  begin
    Logger.SendComponent(C);
    if Assigned(C.GetParentComponent) then
      Logger.SendComponent(C.GetParentComponent);

  end;
end;

procedure TTestComponentStreaming.TestSaveLFM;
begin
  //AddChildrenToParentComponent;
  //SaveLFM(FParent);
  AddChildrenToComponent;
  SaveLFM(FComponent);
end;


initialization
  RegisterTest(TTestComponentStreaming);

end.

