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

{ unit NativeXmlObjectStorage

  This unit provides functionality to store any TObject descendant to an XML file
  or stream. Internally it makes full use of RTTI (RunTime Type Information) in
  order to store all published properties and events.

  It can even be used to copy forms, but form inheritance is not exploited, so
  child forms descending from parent forms store everything that the parent already
  stored.

  All published properties and events of objects are stored. This includes
  the "DefineProperties". These are stored in binary form in the XML, encoded
  as BASE64.

  Known limitations:
  - The method and event lookup will not work correctly across forms.

  Please see the "ObjectToXML" demo for example usage of this unit.

  New version made compatible with FPC by using conditional defines,
  so the unit NativeXmlStorageFPC.pas is no longer required.

  Original Author: Nils Haeck M.Sc.
  Copyright (c) 2003-2011 Simdesign B.V.

  Contributor(s):
  Adam Siwon:
  - fixes for stored properties
  - TCollection items
  Tim Sinaeve
  - FPC version of RTTI
  - Encode/decode string properties
  Rob Kits (RSK)
  - fixed attribute handling in some places (denoted by RSK)

  It is NOT allowed under ANY circumstances to publish or copy this code
  without accepting the license conditions in accompanying LICENSE.txt
  first!

  This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied.

  Please visit http://www.simdesign.nl/xml.html for more information.
}

{
  TS: Fixed pointer operations on Integer to NativeInt for 64bit support.
  - removed dependency from debug unit.
  - use dedicated methods in unit TypInfo to read object properties
  - applied consistent naming
}
unit ts.Core.NativeXml.ObjectStorage;

{$I ts.Core.NativeXml.inc}

// undefine 'UseForms' to avoid including the forms and controls units (visual lib).
// This will reduce the app by several megabytes.
{$DEFINE UseForms}
{.$DEFINE LOGGER}

interface

uses
  Classes, SysUtils,
{$IFDEF UseForms}
  Forms, Controls,
{$ENDIF}
  TypInfo, Variants,

  ts.Core.SharedLogger,

  ts.Core.NativeXml;

type
  // Use TXmlObjectWriter to write any TPersistent descendant's published properties
  // to an XML node.
  TXmlObjectWriter = class(TPersistent)
  protected
    procedure WriteProperty(ANode: TXmlNode; AObject: TObject; AParent: TComponent; APropInfo: PPropInfo);
  public
    // Call WriteObject to write the published properties of AObject to the TXmlNode
    // ANode. Specify AParent in order to store references to parent methods and
    // events correctly.
    procedure WriteObject(ANode: TXmlNode; AObject: TObject; AParent: TComponent = nil);
    // Call WriteComponent to write the published properties of AComponent to the TXmlNode
    // ANode. Specify AParent in order to store references to parent methods and
    // events correctly.
    procedure WriteComponent(ANode: TXmlNode; AComponent: TComponent; AParent: TComponent = nil);
  end;

  // Use TXmlObjectReader to read any TPersistent descendant's published properties
  // from an XML node.
  TXmlObjectReader = class(TPersistent)
  private
    FSetDefaultValues: Boolean;
  protected
    function ReadProperty(ANode: TXmlNode; AObject: TObject; AParent: TComponent; APropInfo: PPropInfo): Boolean;
  public
    // Call CreateComponent to first create AComponent and then read its published
    // properties from the TXmlNode ANode. Specify AParent in order to resolve
    // references to parent methods and events correctly. In order to successfully
    // create the component from scratch, the component's class must be registered
    // beforehand with a call to RegisterClass. Specify Owner to add the component
    // as a child to Owner's component list. This is usually a form. Specify Name
    // as the new component name for the created component.
    function CreateComponent(ANode: TXmlNode; AOwner, AParent: TComponent; AName: string = ''): TComponent;
    // Call ReadObject to read the published properties of AObject from the TXmlNode
    // ANode. Specify AParent in order to resolve references to parent methods and
    // events correctly.
    function ReadObject(ANode: TXmlNode; AObject: TObject; AParent: TComponent = nil): Boolean;
    // Call ReadComponent to read the published properties of AComponent from the TXmlNode
    // ANode. Specify AParent in order to resolve references to parent methods and
    // events correctly.
    procedure ReadComponent(ANode: TXmlNode; AComponent: TComponent; AParent: TComponent);
    // The flag that determines whether the property that are not saved in the
    // XML file.
    property SetDefaultValues: Boolean read FSetDefaultValues write FSetDefaultValues;
  end;

// High-level create methods

// Create and read a component from the XML file with FileName. In order to successfully
// create the component from scratch, the component's class must be registered
// beforehand with a call to RegisterClass. Specify Owner to add the component
// as a child to Owner's component list. This is usually a form. Specify Name
// as the new component name for the created component.
function ComponentCreateFromXmlFile(const FileName: string; Owner: TComponent;
  const Name: string): TComponent;

// Create and read a component from the TXmlNode ANode. In order to successfully
// create the component from scratch, the component's class must be registered
// beforehand with a call to RegisterClass. Specify Owner to add the component
// as a child to Owner's component list. This is usually a form. Specify Name
// as the new component name for the created component.
function ComponentCreateFromXmlNode(ANode: TXmlNode; Owner: TComponent;
  const Name: string): TComponent;

// Create and read a component from the XML stream S. In order to successfully
// create the component from scratch, the component's class must be registered
// beforehand with a call to RegisterClass. Specify Owner to add the component
// as a child to Owner's component list. This is usually a form. Specify Name
// as the new component name for the created component.
function ComponentCreateFromXmlStream(S: TStream; Owner: TComponent;
  const Name: string): TComponent;

// Create and read a component from the XML in string in Value. In order to successfully
// create the component from scratch, the component's class must be registered
// beforehand with a call to RegisterClass. Specify Owner to add the component
// as a child to Owner's component list. This is usually a form. Specify Name
// as the new component name for the created component.
function ComponentCreateFromXmlString(const Value: string; Owner: TComponent;
  const Name: string): TComponent;

{$IFDEF UseForms}
// Create and read a form from the XML file with FileName. In order to successfully
// create the form from scratch, the form's class must be registered
// beforehand with a call to RegisterClass. Specify Owner to add the form
// as a child to Owner's component list. For forms this is usually Application.
// Specify Name as the new form name for the created form.
function FormCreateFromXmlFile(const FileName: string; Owner: TComponent;
  const Name: string): TForm;

// Create and read a form from the XML stream in S. In order to successfully
// create the form from scratch, the form's class must be registered
// beforehand with a call to RegisterClass. Specify Owner to add the form
// as a child to Owner's component list. For forms this is usually Application.
// Specify Name as the new form name for the created form.
function FormCreateFromXmlStream(S: TStream; Owner: TComponent;
  const Name: string): TForm;

// Create and read a form from the XML string in Value. In order to successfully
// create the form from scratch, the form's class must be registered
// beforehand with a call to RegisterClass. Specify Owner to add the form
// as a child to Owner's component list. For forms this is usually Application.
// Specify Name as the new form name for the created form.
function FormCreateFromXmlString(const Value: string; Owner: TComponent;
  const Name: string): TForm;
{$ENDIF}

// High-level load methods

// Load all the published properties of AObject from the XML file in Filename.
// Specify AParent in order to resolve references to parent methods and
// events correctly.
procedure ObjectLoadFromXmlFile(AObject: TObject; const FileName: string;
  AParent: TComponent = nil);

// Load all the published properties of AObject from the TXmlNode ANode.
// Specify AParent in order to resolve references to parent methods and
// events correctly.
procedure ObjectLoadFromXmlNode(AObject: TObject; ANode: TXmlNode; AParent: TComponent = nil);

// Load all the published properties of AObject from the XML stream in S.
// Specify AParent in order to resolve references to parent methods and
// events correctly.
procedure ObjectLoadFromXmlStream(AObject: TObject; S: TStream; AParent: TComponent = nil);

// Load all the published properties of AObject from the XML string in Value.
// Specify AParent in order to resolve references to parent methods and
// events correctly.
procedure ObjectLoadFromXmlString(AObject: TObject; const Value: string; AParent: TComponent = nil);

// High-level save methods

// Save all the published properties of AObject as XML to the file in Filename.
// Specify AParent in order to store references to parent methods and
// events correctly.
procedure ObjectSaveToXmlFile(AObject: TObject; const FileName: string;
  AParent: TComponent = nil);

// Save all the published properties of AObject to the TXmlNode ANode.
// Specify AParent in order to store references to parent methods and
// events correctly.
procedure ObjectSaveToXmlNode(AObject: TObject; ANode: TXmlNode; AParent: TComponent = nil);

// Save all the published properties of AObject as XML in stream S.
// Specify AParent in order to store references to parent methods and
// events correctly.
procedure ObjectSaveToXmlStream(AObject: TObject; S: TStream; AParent: TComponent = nil);

// Save all the published properties of AObject as XML in string Value.
// Specify AParent in order to store references to parent methods and
// events correctly.
function ObjectSaveToXmlString(AObject: TObject; AParent: TComponent = nil): string;

// Save all the published properties of AComponent as XML in the file in Filename.
// Specify AParent in order to store references to parent methods and
// events correctly.
procedure ComponentSaveToXmlFile(AComponent: TComponent; const FileName: string;
  AParent: TComponent = nil);

// Save all the published properties of AComponent to the TXmlNode ANode.
// Specify AParent in order to store references to parent methods and
// events correctly.
procedure ComponentSaveToXmlNode(AComponent: TComponent; ANode: TXmlNode;
  AParent: TComponent = nil);

// Save all the published properties of AComponent as XML in the stream in S.
// Specify AParent in order to store references to parent methods and
// events correctly.
procedure ComponentSaveToXmlStream(AComponent: TComponent; S: TStream;
  AParent: TComponent = nil);

// Save all the published properties of AComponent as XML in the string Value.
// Specify AParent in order to store references to parent methods and
// events correctly.
function ComponentSaveToXmlString(AComponent: TComponent; AParent: TComponent = nil): string;

{$IFDEF UseForms}
// Save the form AForm as XML to the file in Filename. This method also stores
// properties of all child components on the form, and can therefore be used
// as a form-storage method.
procedure FormSaveToXmlFile(AForm: TForm; const FileName: string);

// Save the form AForm as XML to the stream in S. This method also stores
// properties of all child components on the form, and can therefore be used
// as a form-storage method.
procedure FormSaveToXmlStream(AForm: TForm; S: TStream);

// Save the form AForm as XML to a string. This method also stores
// properties of all child components on the form, and can therefore be used
// as a form-storage method.
function FormSaveToXmlString(AForm: TForm): string;
{$ENDIF}

resourcestring
  SIllegalVariantType     = 'Illegal variant type';
  SUnregisteredClassType  = 'Unregistered classtype (%s)';
  SInvalidPropertyValue   = 'Invalid property value (%s)';
  SInvalidMethodName      = 'Invalid method name';
  SObjectCouldNotBeLoaded = 'Object could not be loaded for PropInfo (%s)';

const
  NAME_ATTRIBUTE    = 'Name';
  VARTYPE_ATTRIBUTE = 'VarType';

implementation

type
  TPersistentAccess = class(TPersistent);
  TComponentAccess = class(TComponent)
  public
    procedure SetComponentState(const AState: TComponentState);
  published
    property ComponentState;
  end;

  TReaderAccess = class(TReader);

function ComponentCreateFromXmlFile(const FileName: string; Owner: TComponent;
  const Name: string): TComponent;
var
  FS: TStream;
begin
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result := ComponentCreateFromXmlStream(FS, Owner, Name);
  finally
    FS.Free;
  end;
end;

function ComponentCreateFromXmlNode(ANode: TXmlNode; Owner: TComponent;
  const Name: string): TComponent;
var
  LReader: TXmlObjectReader;
begin
  Result := nil;
  if not Assigned(ANode) then
    Exit;
  // Create reader
  LReader := TXmlObjectReader.Create;
  try
    // Read the component from the node
    Result := LReader.CreateComponent(ANode, Owner, nil, Name);
  finally
    LReader.Free;
  end;
end;

function ComponentCreateFromXmlStream(S: TStream; Owner: TComponent;
  const Name: string): TComponent;
var
  LDoc: TNativeXml;
begin
  Result := nil;
  if not Assigned(S) then
    Exit;
  // Create XML document
  LDoc := TNativeXml.Create(nil);
  try
    // Load XML
    LDoc.LoadFromStream(S);
    // Load from XML node
    Result := ComponentCreateFromXmlNode(LDoc.Root, Owner, Name);
  finally
    LDoc.Free;
  end;
end;

function ComponentCreateFromXmlString(const Value: string; Owner: TComponent;
  const Name: string): TComponent;
var
  SS: TStream;
begin
  SS := TStringStream.Create(Value);
  try
    Result := ComponentCreateFromXmlStream(SS, Owner, Name);
  finally
    SS.Free;
  end;
end;

{$IFDEF UseForms}
function FormCreateFromXmlFile(const FileName: string; Owner: TComponent;
  const Name: string): TForm;
var
  FS: TStream;
begin
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result := FormCreateFromXmlStream(FS, Owner, Name);
  finally
    FS.Free;
  end;
end;

function FormCreateFromXmlStream(S: TStream; Owner: TComponent;
  const Name: string): TForm;
var
  LDoc: TNativeXml;
begin
  Result := nil;
  if not Assigned(S) then
    Exit;
  // Create XML document
  LDoc := TNativeXml.Create(nil);
  try
    // Load XML
    LDoc.LoadFromStream(S);

    // Load from XML node
    Result := TForm(ComponentCreateFromXmlNode(LDoc.Root, Owner, Name));
  finally
    LDoc.Free;
  end;
end;

function FormCreateFromXmlString(const Value: string; Owner: TComponent;
  const Name: string): TForm;
var
  SS: TStream;
begin
  SS := TStringStream.Create(Value);
  try
    Result := FormCreateFromXmlStream(SS, Owner, Name);
  finally
    SS.Free;
  end;
end;
{$ENDIF}

procedure ObjectLoadFromXmlFile(AObject: TObject; const FileName: string;
  AParent: TComponent = nil);
var
  FS: TStream;
begin
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    ObjectLoadFromXmlStream(AObject, FS, AParent);
  finally
    FS.Free;
  end;
end;

procedure ObjectLoadFromXmlNode(AObject: TObject; ANode: TXmlNode; AParent: TComponent = nil);
var
  LReader: TXmlObjectReader;
begin
  if not Assigned(AObject) or not Assigned(ANode) then
    Exit;
  // Create writer
  LReader := TXmlObjectReader.Create;
  try
    // Write the object to the document
    if AObject is TComponent then
      LReader.ReadComponent(ANode, TComponent(AObject), AParent)
    else
      LReader.ReadObject(ANode, AObject, AParent);
  finally
    LReader.Free;
  end;
end;

procedure ObjectLoadFromXmlStream(AObject: TObject; S: TStream; AParent: TComponent = nil);
var
  LDoc: TNativeXml;
begin
  if not Assigned(S) then
    Exit;
  // Create XML document
  LDoc := TNativeXml.Create(nil);
  try
    // Load XML
    LDoc.LoadFromStream(S);
    // Load from XML node
    ObjectLoadFromXmlNode(AObject, LDoc.Root, AParent);
  finally
    LDoc.Free;
  end;
end;

procedure ObjectLoadFromXmlString(AObject: TObject; const Value: string; AParent: TComponent = nil);
var
  SS: TStringStream;
begin
  SS := TStringStream.Create(Value);
  try
    ObjectLoadFromXmlStream(AObject, SS, AParent);
  finally
    SS.Free;
  end;
end;

procedure ObjectSaveToXmlFile(AObject: TObject; const FileName: string;
  AParent: TComponent = nil);
var
  FS: TStream;
begin
  FS := TFileStream.Create(FileName, fmCreate);
  try
    ObjectSaveToXmlStream(AObject, FS, AParent);
  finally
    FS.Free;
  end;
end;

procedure ObjectSaveToXmlNode(AObject: TObject; ANode: TXmlNode; AParent: TComponent = nil);
var
  LWriter: TXmlObjectWriter;
begin
  if not Assigned(AObject) or not Assigned(ANode) then
    Exit;
  // Create writer
  LWriter := TXmlObjectWriter.Create;
  try
    // Write the object to the document
    if AObject is TComponent then
      LWriter.WriteComponent(ANode, TComponent(AObject), AParent)
    else
    begin
      ANode.Name := UTF8String(AObject.ClassName);
      LWriter.WriteObject(ANode, AObject, AParent);
    end;
  finally
    LWriter.Free;
  end;
end;

procedure ObjectSaveToXmlStream(AObject: TObject; S: TStream; AParent: TComponent = nil);
var
  LDoc: TNativeXml;
begin
  if not Assigned(S) then
    Exit;
  // Create XML document
  LDoc := TNativeXml.Create(nil);
  try
    LDoc.XmlFormat := xfReadable;
    // Save to XML node
    ObjectSaveToXmlNode(AObject, LDoc.Root, AParent);
    // Save to stream
    LDoc.SaveToStream(S);
  finally
    LDoc.Free;
  end;
end;

function ObjectSaveToXmlString(AObject: TObject; AParent: TComponent = nil): string;
var
  SS: TStringStream;
begin
  SS := TStringStream.Create('');
  try
    ObjectSaveToXmlStream(AObject, SS, AParent);
    Result := SS.DataString;
  finally
    SS.Free;
  end;
end;

procedure ComponentSaveToXmlFile(AComponent: TComponent; const FileName: string;
  AParent: TComponent = nil);
begin
  ObjectSaveToXmlFile(AComponent, FileName, AParent);
end;

procedure ComponentSaveToXmlNode(AComponent: TComponent; ANode: TXmlNode;
  AParent: TComponent = nil);
begin
  ObjectSaveToXmlNode(AComponent, ANode, AParent);
end;

procedure ComponentSaveToXmlStream(AComponent: TComponent; S: TStream;
  AParent: TComponent = nil);
begin
  ObjectSaveToXmlStream(AComponent, S, AParent);
end;

function ComponentSaveToXmlString(AComponent: TComponent; AParent: TComponent = nil): string;
begin
  Result := ObjectSaveToXmlString(AComponent, AParent);
end;

{$IFDEF UseForms}
procedure FormSaveToXmlFile(AForm: TForm; const FileName: string);
begin
  ComponentSaveToXmlFile(AForm, FileName, AForm);
end;

procedure FormSaveToXmlStream(AForm: TForm; S: TStream);
begin
  ComponentSaveToXmlStream(AForm, S, AForm);
end;

function FormSaveToXmlString(AForm: TForm): string;
begin
  Result := ComponentSaveToXmlString(AForm, AForm);
end;
{$ENDIF}

{ TXmlObjectWriter }

procedure TXmlObjectWriter.WriteComponent(ANode: TXmlNode; AComponent,
  AParent: TComponent);
begin
  if not Assigned(ANode) or not Assigned(AComponent) then
    Exit;
  ANode.Name := UTF8String(AComponent.ClassName);
  if Length(AComponent.Name) > 0 then
    ANode.AttributeAdd(NAME_ATTRIBUTE, UTF8String(AComponent.Name));
  WriteObject(ANode, AComponent, AParent);
end;

procedure TXmlObjectWriter.WriteObject(ANode: TXmlNode; AObject: TObject;
  AParent: TComponent);
var
  I              : Integer;
  LCount         : Integer;
  LPropInfo      : PPropInfo;
  LPropList      : PPropList;
  SS             : TStringStream;
  LWriter        : TWriter;
  LChildNode     : TXmlNode;
  LComponentNode : TXmlNode;
  C              : TComponent;
begin
  if not Assigned(ANode) or not Assigned(AObject) then
    Exit;

  // If this is a component, store child components
  if AObject is TComponent then
  begin
    C := TComponent(AObject);
    if C.ComponentCount > 0 then
    begin
      LChildNode := ANode.NodeNew('Components');
      for I := 0 to C.ComponentCount - 1 do
      begin
        LComponentNode := LChildNode.NodeNew(UTF8String(C.Components[I].ClassName));
        if Length(C.Components[I].Name) > 0 then
          LComponentNode.AttributeAdd(NAME_ATTRIBUTE, UTF8String(C.Components[I].Name));
        WriteObject(LComponentNode, C.Components[I], TComponent(AObject));
      end;
    end;
  end;

  // If this is a collection, store collections items
  if AObject is TCollection then
  begin
    for I := 0 to TCollection(AObject).Count - 1 do
    begin
      LChildNode := ANode.NodeNew(UTF8String(TCollection(AObject).Items[I].ClassName));
      WriteObject(LChildNode, TCollection(AObject).Items[I], AParent);
    end;
  end;

  // Save all regular properties that need storing
  LCount := GetTypeData(AObject.ClassInfo)^.PropCount;
  if LCount > 0 then
  begin
    GetMem(LPropList, LCount * SizeOf(Pointer));
    try
      GetPropInfos(AObject.ClassInfo, LPropList);
      for I := 0 to LCount - 1 do
      begin
        LPropInfo := LPropList^[I];
        if LPropInfo = nil then
          continue;
        if IsStoredProp(AObject, LPropInfo) then
          WriteProperty(ANode, AObject, AParent, LPropInfo);
      end;
    finally
      FreeMem(LPropList, LCount * SizeOf(Pointer));
    end;
  end;

  // Save defined properties
  if AObject is TPersistent then
  begin
    SS := TStringStream.Create('');
    try
      LWriter := TWriter.Create(SS, 4096);
      try
        TPersistentAccess(AObject).DefineProperties(LWriter);
      finally
        LWriter.Free;
      end;
      // Do we have data from DefineProperties?
      if SS.Size > 0 then
      begin
        // Yes, add a node with binary data
        ANode.NodeNew('DefinedProperties').BinaryString := RawByteString(SS.DataString);
      end;
    finally
      SS.Free;
    end;
  end;
end;

procedure TXmlObjectWriter.WriteProperty(ANode: TXmlNode; AObject: TObject;
  AParent: TComponent; APropInfo: PPropInfo);
var
  LPropType       : PTypeInfo;
  LChildNode      : TXmlNode;
  LCollectionNode : TXmlNode;

  procedure WritePropName;
  begin
    LChildNode := ANode.NodeNew(PPropInfo(APropInfo)^.Name);
  end;

  procedure WriteInteger(Value: Int64);
  begin
    LChildNode.Value := UTF8String(IntToStr(Value));
  end;

  procedure WriteString(Value: string);
  begin
    LChildNode.ValueUnicode := UnicodeString(Value);
  end;

  procedure WriteBool(Value: Boolean);
  begin
    LChildNode.ValueAsBool := Value;
  end;

  procedure WriteSet(Value: Longint);
  var
    I        : Integer;
    BaseType : PTypeInfo;
    S        : string;
    Enum     : string;
  begin
    S := '';
    {$IFDEF FPC}
    BaseType := GetTypeData(LPropType)^.CompType;
    {$ELSE}
    BaseType := GetTypeData(PropType)^.CompType^;
    {$ENDIF}
    for i := 0 to SizeOf(TIntegerSet) * 8 - 1 do
    begin
      if i in TIntegerSet(Value) then
      begin
        Enum := GetEnumName(BaseType, i);
        if i > 0 then
          S := S + ',' + Enum
        else
          S := Enum;
      end;
    end;
    LChildNode.Value := UTF8String(Format('[%s]', [S]));
  end;

  procedure WriteIntProp(IntType: PTypeInfo; Value: Longint);
  var
    Ident      : string;
    IntToIdent : TIntToIdent;
  begin
    Ident := '';
    IntToIdent := FindIntToIdent(IntType);
    if Assigned(IntToIdent) and IntToIdent(Value, Ident) then
      WriteString(Ident)
    else
      WriteInteger(Value);
  end;

  procedure WriteCollectionProp(Collection: TCollection);
  var
    I: Integer;
  begin
    if Assigned(Collection) then
    begin
      for I := 0 to Collection.Count - 1 do
      begin
        LCollectionNode := LChildNode.NodeNew(UTF8String(Collection.Items[I].ClassName));
        WriteObject(LCollectionNode, Collection.Items[I], AParent);
      end;
    end;
  end;

  procedure WriteOrdProp;
  var
    Value: Int64;
  begin
    Value := GetOrdProp(AObject, APropInfo);
    if not (Value = PPropInfo(APropInfo)^.Default) then
    begin
      WritePropName;
      case LPropType^.Kind of
      tkInteger:     WriteIntProp(PPropInfo(APropInfo)^.PropType, Value);
      tkBool:        WriteBool(Value <> 0);
      tkChar:        WriteString(Chr(Value));
      tkSet:         WriteSet(Value);
      tkEnumeration: WriteString(GetEnumName(LPropType, Value));
      end;
    end;
  end;

  procedure WriteFloatProp;
  var
    Value : Extended;
  begin
    Value := GetFloatProp(AObject, APropInfo);
    if not (Value = 0) then
      ANode.WriteFloat(PPropInfo(APropInfo)^.Name, Value);
  end;

  procedure WriteInt64Prop;
  var
    Value: Int64;
  begin
    Value := GetInt64Prop(AObject, APropInfo);
    if not (Value = 0) then
      ANode.WriteInt64(PPropInfo(APropInfo)^.Name, Value);
  end;

  procedure WriteStrProp;
  var
    Value: Utf8String;
  begin
    Value := Utf8String(GetStrProp(AObject, APropInfo));
    if not (Length(Value) = 0) then
      ANode.WriteString(PPropInfo(APropInfo)^.Name, Value);
  end;

  procedure WriteWideStrProp;
  var
    Value: Utf8String;
  begin
    Value := Utf8String(GetWideStrProp(AObject, APropInfo));
    if not (Length(Value) = 0) then
      ANode.WriteString(PPropInfo(APropInfo)^.Name, Value);
  end;
  {$IFDEF UNICODE}
  procedure WriteUnicodeStrProp;
  var
    Value: UnicodeString;
  begin
    Value := GetUnicodeStrProp(AObject, PropInfo);
    if not (Length(Value) = 0) then
      ANode.WriteString(PPropInfo(PropInfo)^.Name, Value);
  end;
  {$ENDIF UNICODE}

  procedure WriteObjectProp;
  var
    LObject        : TObject;
    LComponentName : string;

    function GetComponentName(Component: TComponent): string;
    begin
      if Component.Owner = AParent then
        Result := Component.Name
      else if Component = AParent then
        Result := 'owner'
      else if Assigned(Component.Owner) and (Length(Component.Owner.Name) > 0) and (Length(Component.Name) > 0) then
        Result := Component.Owner.Name + '.' + Component.Name
      else if Length(Component.Name) > 0 then
        Result := Component.Name + '.owner'
      else
        Result := '';
    end;

  begin
    LObject := GetObjectProp(AObject, APropInfo);
    if not Assigned(LObject) then
      Exit;
    WritePropName;
    if (LObject is TComponent) and not (csSubComponent in TComponent(LObject).ComponentStyle) then
    begin
      LComponentName := GetComponentName(TComponent(LObject));
      if Length(LComponentName) > 0 then
        WriteString(LComponentName);
    end
    else
    begin
      WriteString(Format('(%s)', [LObject.ClassName]));
      if LObject is TCollection then
        WriteCollectionProp(TCollection(LObject))
      else
      begin
        if AObject is TComponent then
          WriteObject(LChildNode, LObject, TComponent(AObject))
        else
          WriteObject(LChildNode, LObject, AParent)
      end;
      // No need to store an empty child.. so check and remove
      if LChildNode.NodeCount = 0 then
        ANode.NodeRemove(LChildNode);
    end;
  end;

  procedure WriteMethodProp;
  var
    LMethod: TMethod;

    function IsDefaultValue: Boolean;
    begin
      Result := (LMethod.Code = nil) or
        ((LMethod.Code <> nil) and Assigned(AParent) and (AParent.MethodName(LMethod.Code) = ''));
    end;

  begin
    LMethod := GetMethodProp(AObject, APropInfo);
    if not IsDefaultValue then
    begin
      if Assigned(LMethod.Code) then
      begin
        WritePropName;
        if Assigned(AParent) then
          WriteString(AParent.MethodName(LMethod.Code))
        else
          LChildNode.Value := '';
      end;
    end;
  end;

  function WriteVariantProp: Boolean;
  var
    LVariant  : Variant;
    LCurrency : Currency;
    LVarType  : Integer;
  begin
    Result := True;
    LVariant := GetVariantProp(AObject, APropInfo);
    if not VarIsEmpty(LVariant) or VarIsNull(LVariant) then
    begin
      if VarIsArray(LVariant) then
      begin
        {$IFDEF LOGGER}Logger.SendError(SIllegalVariantType);{$ENDIF}
        Result := False;
        Exit;
      end;
      WritePropName;
      LVarType := VarType(LVariant);
      LChildNode.AttributeAdd(VARTYPE_ATTRIBUTE, UTF8String(IntToHex(LVarType, 4)));
      case LVarType and varTypeMask of
      varNull:    LChildNode.Value := '';
      varOleStr:  LChildNode.Value := Utf8String(LVariant);
      varString:  LChildNode.Value := Utf8String(LVariant);
      varByte,
      varSmallInt,
      varInteger: LChildNode.SetValueAsInteger(LVariant);
      varSingle,
      varDouble:  LChildNode.SetValueAsFloat(LVariant);
      varCurrency:
        begin
          LCurrency := LVariant;
          LChildNode.BufferWrite(LCurrency, SizeOf(LCurrency));
        end;
      varDate:    LChildNode.SetValueAsDateTime(LVariant);
      varBoolean: LChildNode.SetValueAsBool(LVariant);
      else
        try
          ANode.Value := LVariant;
        except
          {$IFDEF LOGGER}Logger.SendError(SIllegalVariantType);{$ENDIF}
          Result := False;
          Exit;
        end;
      end;//case
    end;
  end;

begin
  {$IFDEF LOGGER}Logger.EnterMethod('TXmlObjectWriter.WriteProperty');{$ENDIF}
  {$IFDEF LOGGER}Logger.Send('ANode: %s = %s', [ANode.Name, ANode.Value]);{$ENDIF}
  if (PPropInfo(APropInfo)^.SetProc <> nil) and
    (PPropInfo(APropInfo)^.GetProc <> nil) then
  begin
    LPropType := PPropInfo(APropInfo)^.PropType;
    case LPropType^.Kind of
      tkBool, tkInteger, tkChar, tkEnumeration, tkSet:
        WriteOrdProp;
      tkFloat:
        WriteFloatProp;
      tkAString, tkString, tkLString:
        WriteStrProp;
      tkWString:
        WriteWideStrProp;
      {$IFDEF UNICODE}
      tkUString:
        WriteUnicodeStrProp;
      {$ENDIF UNICODE}
      tkClass:
        WriteObjectProp;
      tkMethod:
        WriteMethodProp;
      tkVariant:
        WriteVariantProp;
      tkInt64:
        WriteInt64Prop;
    end;
  end;
  {$IFDEF LOGGER}Logger.ExitMethod('TXmlObjectWriter.WriteProperty');{$ENDIF}
end;

{ TXmlObjectReader }

function TXmlObjectReader.CreateComponent(ANode: TXmlNode;
  AOwner, AParent: TComponent; AName: string): TComponent;
var
  LClass: TComponentClass;
begin
  {$IFDEF LOGGER}Logger.EnterMethod('TXmlObjectReader.CreateComponent');{$ENDIF}
  LClass := TComponentClass(GetClass(string(ANode.Name)));
  if not Assigned(LClass) then
  begin
    raise Exception.CreateFmt(SUnregisteredClassType, [ANode.Name]);
    //{$IFDEF LOGGER}Logger.SendError(Format(SUnregisteredClassType, [ANode.Name]));{$ENDIF}
    //Result := nil;
    //Exit;
  end;
  Result := LClass.Create(AOwner);
  if Length(AName) = 0 then
    Result.Name := ANode.AttributeValueByName[NAME_ATTRIBUTE]
  else
    Result.Name := AName;
  if not Assigned(AParent) then
    AParent := Result;
  ReadComponent(ANode, Result, AParent);
  {$IFDEF LOGGER}Logger.ExitMethod('TXmlObjectReader.CreateComponent');{$ENDIF}
end;

procedure TXmlObjectReader.ReadComponent(ANode: TXmlNode; AComponent,
  AParent: TComponent);
begin
  ReadObject(ANode, AComponent, AParent);
end;

function TXmlObjectReader.ReadObject(ANode: TXmlNode; AObject: TObject; AParent: TComponent): Boolean;
var
  I              : Integer;
  LCount         : Integer;
  LItem          : TCollectionItem;
  LPropInfo      : PPropInfo;
  LPropList      : PPropList;
  SS             : TStringStream;
  LReader        : TReader;
  LChildNode     : TXmlNode;
  LComponentNode : TXmlNode;
  LClass         : TComponentClass;
  LComponent     : TComponent;
  C              : TComponent;
  CA             : TComponentAccess;
  LCollection    : TCollection;
begin
  {$IFDEF LOGGER}Logger.EnterMethod('TXmlObjectReader.ReadObject');{$ENDIF}
  Result := True;
  if not Assigned(ANode) or not Assigned(AObject) then
    Exit;

  {$IFDEF LOGGER}Logger.Send(ANode.Name, ANode.Value);{$ENDIF}
  // Start loading
  if AObject is TComponent then
  begin
    CA := TComponentAccess(AObject);
    CA.Updating;
    CA.SetComponentState(CA.ComponentState + [csLoading, csReading]);
  end;
  try
    // If this is a component, load child components
    if AObject is TComponent then
    begin
      C := TComponent(AObject);
      LChildNode := ANode.NodeByName('Components');
      if Assigned(LChildNode) then
      begin
        for I := 0 to LChildNode.ContainerCount - 1 do
        begin
          LComponentNode := LChildNode.Containers[I];
          LComponent := C.FindComponent(LComponentNode.AttributeValueByName[NAME_ATTRIBUTE]);
          if not Assigned(LComponent) then
          begin
            LClass := TComponentClass(GetClass(string(LComponentNode.Name)));
            if not Assigned(LClass) then
            begin
              raise Exception.CreateFmt(SUnregisteredClassType, [LComponentNode.Name]);
              //{$IFDEF LOGGER}Logger.SendError(Format(SUnregisteredClassType, [LComponentNode.Name]));{$ENDIF}
              //Result := False;
              //Exit;
            end;
            LComponent := LClass.Create(TComponent(AObject));
            LComponent.Name := LComponentNode.AttributeValueByName[NAME_ATTRIBUTE];
{$IFDEF UseForms}
            // In case of new (visual) controls we set the parent
            if (LComponent is TControl) and (AObject is TWinControl) then
              TControl(LComponent).Parent := TWinControl(AObject);
{$ENDIF}
          end;
          ReadComponent(LComponentNode, LComponent, TComponent(AObject));
        end;
      end;
    end;

    // If this is a collection, load collections items
    if AObject is TCollection then
    begin
      LCollection := TCollection(AObject);
      LCollection.BeginUpdate;
      try
        LCollection.Clear;
        for I := 0 to ANode.ContainerCount - 1 do
        begin
          LItem := LCollection.Add;
          ReadObject(ANode.Containers[I], LItem, AParent);
        end;
      finally
        LCollection.EndUpdate;
      end;
    end;

    // Load all loadable regular properties
    LCount := GetTypeData(AObject.ClassInfo)^.PropCount;
    if LCount > 0 then
    begin
      GetMem(LPropList, LCount * SizeOf(Pointer));
      try
        GetPropInfos(AObject.ClassInfo, LPropList);
        for I := 0 to LCount - 1 do
        begin
          LPropInfo := LPropList^[I];
          if LPropInfo = nil then
            continue;
          ReadProperty(ANode, AObject, AParent, LPropInfo);
        end;
      finally
        FreeMem(LPropList, LCount * SizeOf(Pointer));
      end;
    end;

    // Load defined properties
    if AObject is TPersistent then
    begin
      LChildNode := ANode.NodeByName('DefinedProperties');
      if Assigned(LChildNode) then
      begin
        SS := TStringStream.Create(string(LChildNode.BinaryString));
        SS.Position := 0;
        try
          LReader := TReader.Create(SS, 4096);
          try
            while SS.Position < SS.Size do
              TReaderAccess(LReader).ReadProperty(TPersistent(AObject));
          finally
            LReader.Free;
          end;
        finally
          SS.Free;
        end;
      end;
    end;

  finally
    // End loading
    if AObject is TComponent then
    begin
      CA := TComponentAccess(AObject);
      CA.SetComponentState(CA.ComponentState - [csReading]);
      CA.Loaded;
      CA.Updated;
    end;
  end;
  {$IFDEF LOGGER}Logger.ExitMethod('TXmlObjectReader.ReadObject');{$ENDIF}
end;

function TXmlObjectReader.ReadProperty(ANode: TXmlNode;
  AObject: TObject; AParent: TComponent; APropInfo: PPropInfo): Boolean;
var
  LPropType   : PTypeInfo;
  LChildNode  : TXmlNode;
  LMethod     : TMethod;
  LPropObject : TObject;

  procedure ReadCollectionProp(ACollection: TCollection);
  var
    I: Integer;
    Item: TPersistent;
  begin
    ACollection.BeginUpdate;
    try
      ACollection.Clear;
      for I := 0 to LChildNode.ContainerCount - 1 do
      begin
        Item := ACollection.Add;
        ReadObject(LChildNode.Containers[I], Item, AParent);
      end;
    finally
      ACollection.EndUpdate;
    end;
  end;

  function SetSetProp(const AValue: string): Boolean;
  var
    S: string;
    P: Integer;
    LSet: Integer;
    EnumType: PTypeInfo;
    // local local
    function AddToEnum(const EnumName: string): Boolean;
    var
      V: Integer;
    begin
      Result := True;
      if Length(EnumName) = 0 then
        Exit;
      V := GetEnumValue(EnumType, EnumName);
      if V = -1 then
      begin
        raise Exception.CreateFmt(SInvalidPropertyValue, [EnumName]);
        //{$IFDEF LOGGER}Logger.SendError(SInvalidPropertyValue, [EnumName]);{$ENDIF}
        //Result := False;
        //Exit;
      end;
      Include(TIntegerSet(LSet), V);
    end;
  begin
    Result := True;
    LSet := 0;
    EnumType := GetTypeData(LPropType)^.CompType;
    S := copy(AValue, 2, Length(AValue) - 2);
    repeat
      P := Pos(',', S);
      if P > 0 then
      begin
        AddToEnum(copy(S, 1, P - 1));
        S := copy(S, P + 1, Length(S));
      end
      else
      begin
        Result := AddToEnum(S);
        Break;
      end;
    until False;
    SetOrdProp(AObject, APropInfo, LSet);
  end;

  procedure SetIntProp(const AValue: string);
  var
    V: Longint;
    IdentToInt: TIdentToInt;
  begin
    V := 0;
    IdentToInt := FindIdentToInt(LPropType);
    if Assigned(IdentToInt) and IdentToInt(AValue, V) then
      SetOrdProp(AObject, APropInfo, V)
    else
      SetOrdProp(AObject, APropInfo, StrToInt(AValue));
  end;

  function SetCharProp(const AValue: string): Boolean;
  begin
    Result := True;
    if Length(AValue) <> 1 then
    begin
      raise Exception.CreateFmt(SInvalidPropertyValue, [AValue]);
      //{$IFDEF LOGGER}Logger.SendError(SInvalidPropertyValue, [AValue]);{$ENDIF}
      //Result := False;
      //Exit;
    end;
    SetOrdProp(AObject, APropInfo, Ord(AValue[1]));
  end;

  function SetEnumProp(const AValue: string): Boolean;
  var
    V: Integer;
  begin
    Result := True;
    V := GetEnumValue(LPropType, AValue);
    if V = -1 then
    begin
      raise Exception.CreateFmt(SInvalidPropertyValue, [AValue]);
      //{$IFDEF LOGGER}Logger.SendError(SInvalidPropertyValue, [AValue]);{$ENDIF}
      //Result := False;
      //Exit;
    end;
    SetOrdProp(AObject, APropInfo, V)
  end;

  function SetBoolProp(const AValue: Boolean): Boolean;
  var
    V: Integer;
  begin
    Result := True;
    if AValue then
      V := 1
    else
      V := 0;
    SetOrdProp(AObject, APropInfo, V)
  end;

  function SetObjectProp(const AValue: string): Boolean;
  var
    LClassName  : string;
    LPropObject : TObject;
    LReference  : TComponent;
  begin
    Result := True;
    if Length(AValue) = 0 then
      Exit;
    if AValue[1] = '(' then
    begin
      // Persistent class
      LClassName := Copy(AValue, 2, Length(AValue) - 2);
      LPropObject := GetObjectProp(AObject, APropInfo);

      if Assigned(LPropObject) then
        {$IFDEF LOGGER}Logger.Send('PropObject.ClassName', LPropObject.ClassName);{$ENDIF}

      if Assigned(LPropObject) and (LPropObject.ClassName = LClassName) then
      begin
        if LPropObject is TCollection then
          ReadCollectionProp(TCollection(LPropObject))
        else
        begin
          if AObject is TComponent then
            ReadObject(LChildNode, LPropObject, TComponent(AObject))
          else
            ReadObject(LChildNode, LPropObject, AParent);
        end;
      end
      else
      begin
        raise Exception.CreateFmt(SObjectCouldNotBeLoaded, [APropInfo.Name]);
        //{$IFDEF LOGGER}Logger.SendError(SObjectCouldNotBeLoaded, [APropInfo.Name]);{$ENDIF}
        //Result := False;
        //Exit;
      end;
    end
    else
    begin
      // Component LReference
      if Assigned(AParent) then
      begin
        LReference := FindNestedComponent(AParent, AValue);
        SetOrdProp(AObject, APropInfo, Longint(LReference));
      end;
    end;
  end;

  function SetMethodProp(const AValue: string): Boolean;
  var
    Method: TMethod;
  begin
    Result := True;
    // to do: add OnFindMethod
    if not Assigned(AParent) then
      Exit;
    Method.Code := AParent.MethodAddress(AValue);
    if not Assigned(Method.Code) then
    begin
      raise Exception.Create(SInvalidMethodName);
      //{$IFDEF LOGGER}Logger.SendError(SInvalidMethodName);{$ENDIF}
      //Result := False;
      //Exit;
    end;
    Method.Data := AParent;
    TypInfo.SetMethodProp(AObject, APropInfo, Method);
  end;

  function SetVariantProp(const AValue: string): Boolean;
  var
    LVType    : Integer;
    LVariant  : Variant;
    LCurrency : Currency;
  begin
    LCurrency := 0.0;
    Result := True;
    LVType := StrToInt('$' + LChildNode.AttributeValueByName[VARTYPE_ATTRIBUTE]);

    case LVType and varTypeMask of
      varNull:
        LVariant := Null;
      varOleStr:
        LVariant := LChildNode.ValueUnicode;
      varString:
        LVariant := LChildNode.Value;
      varByte,
      varSmallInt,
      varInteger:
        LVariant := LChildNode.GetValueAsInteger;
      varSingle,
      varDouble:
        LVariant := LChildNode.GetValueAsFloat;
      varCurrency:
      begin
        LChildNode.BufferRead(LCurrency, SizeOf(LCurrency));
        LVariant := LCurrency;
      end;
      varDate:
        LVariant := LChildNode.GetValueAsDateTime;
      varBoolean:
        LVariant := LChildNode.GetValueAsBool;
    else
      try
        LVariant := ANode.Value;
      except
        {$IFDEF LOGGER}Logger.SendError(SIllegalVariantType);{$ENDIF}
        Result := False;
        Exit;
      end;
    end;
    TVarData(LVariant).VType := LVType;
    TypInfo.SetVariantProp(AObject, APropInfo, LVariant);
  end;

begin
  {$IFDEF LOGGER}Logger.EnterMethod('TXmlObjectReader.ReadProperty');{$ENDIF}
  Result := True;
  {$IFDEF LOGGER}Logger.Send('APropInfo.Name', APropInfo.Name);{$ENDIF}
  if (PPropInfo(APropInfo)^.SetProc <> nil)
    and (PPropInfo(APropInfo)^.GetProc <> nil) then
  begin
    LPropType  := PPropInfo(APropInfo)^.PropType;
    LChildNode := ANode.NodeByName(PPropInfo(APropInfo)^.Name);
    if Assigned(LChildNode) then
    begin
      {$IFDEF LOGGER}Logger.Send('LChildNode: %s = %s', [LChildNode.Name, LChildNode.Value]);{$ENDIF}
      // Non-default values from XML
      case LPropType^.Kind of
        tkBool:
          SetBoolProp(LChildNode.GetValueAsBool);
        tkInteger:
          SetIntProp(LChildNode.Value);
        tkChar:
          SetCharProp(LChildNode.Value);
        tkSet:
          SetSetProp(LChildNode.Value);
        tkEnumeration:
          SetEnumProp(LChildNode.Value);
        tkFloat:
          SetFloatProp(AObject, APropInfo, LChildNode.GetValueAsFloat);
        tkAString,
        tkString,
        tkLString:
          SetStrProp(AObject, APropInfo, LChildNode.Value);
        {$IFDEF UNICODE}
        tkWString:
          SetWideStrProp(AObject, PropInfo, UTF8ToWideString(LChildNode.Value));
        {$ELSE}
        tkWString:
          SetWideStrProp(AObject, APropInfo, UTF8Decode(LChildNode.Value));
        {$ENDIF}
        {$IFDEF UNICODE}
        tkUString:
          SetUnicodeStrProp(AObject, PropInfo, LChildNode.Value);
        {$ENDIF}
        tkClass:
          SetObjectProp(LChildNode.Value);
        tkMethod:
          SetMethodProp(LChildNode.Value);
        tkVariant:
          SetVariantProp(LChildNode.Value);
        tkInt64:
          SetInt64Prop(AObject, APropInfo, LChildNode.GetValueAsInt64);
      end;
    end
    else
    begin
      if SetDefaultValues then
      begin
        // Set Default value
        case LPropType^.Kind of
          tkBool:
            SetOrdProp(AObject, APropInfo, PPropInfo(APropInfo)^.Default);
          tkInteger:
            SetOrdProp(AObject, APropInfo, PPropInfo(APropInfo)^.Default);
          tkChar:
            SetOrdProp(AObject, APropInfo, PPropInfo(APropInfo)^.Default);
          tkSet:
            SetOrdProp(AObject, APropInfo, PPropInfo(APropInfo)^.Default);
          tkEnumeration:
            SetOrdProp(AObject, APropInfo, PPropInfo(APropInfo)^.Default);
          tkFloat:
            SetFloatProp(AObject, APropInfo, 0);
          tkAString,
          tkString,
          tkLString,
          tkWString:
            SetStrProp(AObject, APropInfo, '');
          {$IFDEF UNICODE}
          tkUString:
            SetStrProp(AObject, PropInfo, '');
          {$ENDIF UNICODE}
          tkClass:
          begin
            LPropObject := GetObjectProp(AObject, APropInfo);
            if LPropObject is TComponent then
              TypInfo.SetObjectProp(AObject, APropInfo, nil);
          end;
          tkMethod:
          begin
            LMethod := TypInfo.GetMethodProp(AObject, APropInfo);
            LMethod.Code := nil;
            TypInfo.SetMethodProp(AObject, APropInfo, LMethod);
          end;
          tkInt64:
            SetInt64Prop(AObject, APropInfo, 0);
        end;
      end;
    end;
  end;
  {$IFDEF LOGGER}Logger.ExitMethod('TXmlObjectReader.ReadProperty');{$ENDIF}
end;

{ TComponentAccess }

procedure TComponentAccess.SetComponentState(const AState: TComponentState);
type
  PInteger = ^Integer;
var
  PSet: PInteger;
  LPropInfo: PPropInfo;
begin
  // This is a "severe" hack in order to set a non-writable property value,
  // also using RTTI
  PSet := PInteger(@AState);
  LPropInfo := GetPropInfo(TComponentAccess, 'ComponentState');
  if Assigned(LPropInfo.GetProc) then
    PInteger(NativeUInt(Self) + NativeUInt(LPropInfo.GetProc) and $00FFFFFF)^ := PSet^;
end;

end.

