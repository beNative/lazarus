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

{
  Copyright (C) 2010 Luiz Américo Pereira Câmara

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit ts.Components.VirtualPages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Controls;

type
  TConstArray = array of TVarRec;

type
  TVirtualPage = class;

  TVirtualPageEvent = procedure(Sender: TObject; Page: TVirtualPage) of object;

  TControlDisplayOptions = class(TPersistent)
  private
    FBorderSpacing : TControlBorderSpacing;
    FParent        : TWinControl;
    FHeight        : Integer;
    FLeft          : Integer;
    FTop           : Integer;
    FWidth         : Integer;
    FAlign         : TAlign;

    procedure SetBorderSpacing(AValue: TControlBorderSpacing);

  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetControlBounds(Control: TControl);

  published
    property Align: TAlign read FAlign write FAlign default alClient;
    property BorderSpacing: TControlBorderSpacing read FBorderSpacing write SetBorderSpacing;
    property Height: Integer read FHeight write FHeight default -1;
    property Left: Integer read FLeft write FLeft default -1;
    property Parent: TWinControl read FParent write FParent;
    property Top: Integer read FTop write FTop default -1;
    property Width: Integer read FWidth write FWidth default -1;
  end;

  { TVirtualPage }

  TVirtualPage = class(TCollectionItem)
  private
    FCaption: String;
    FControl: TControl;
    FControlClass: TControlClass;
    FControlClassName: String;
    FName: String;
    FProperties: TConstArray;
    procedure SetControl(Value: TControl);
  protected
    procedure ControlChanged; virtual;
    function GetDisplayName: String; override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetProperties(const Elements: array of const);
    property ControlClass: TControlClass read FControlClass write FControlClass;
    property Properties: TConstArray read FProperties;
  published
    property Caption: String read FCaption write FCaption;
    property Control: TControl read FControl write SetControl;
    property ControlClassName: String read FControlClassName write FControlClassName;
    property Name: String read FName write FName;
  end;

  { TVirtualPages }

  TVirtualPages = class(TCollection)
  private
    FDisplayOptions: TControlDisplayOptions;
    function GetItems(Index: Integer): TVirtualPage;
    procedure SetDisplayOptions(AValue: TControlDisplayOptions);
  protected
    procedure DoPageHide(Page: TVirtualPage); virtual;
    procedure DoPageLoad(Page: TVirtualPage); virtual;
    procedure DoPageShow(Page: TVirtualPage); virtual;
    procedure UpdateActivePage(OldPageIndex, NewPageIndex: Integer);
  public
    constructor Create(AItemClass: TCollectionItemClass);
    destructor Destroy; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Add(const Name, Caption: String; Control: TControl);
    procedure Add(const Name, Caption: String; ControlClass: TControlClass; const Properties: array of const);
    procedure Add(const Name, Caption, ControlClassName: String; const Properties: array of const);
    function FindPage(const PageName: String): TVirtualPage;
    function PageByName(const PageName: String): TVirtualPage;
    property DisplayOptions: TControlDisplayOptions read FDisplayOptions write SetDisplayOptions;
    property Items[Index: Integer]: TVirtualPage read GetItems; default;
  end;

  { TVirtualPageList }

  TVirtualPageList = class(TVirtualPages)
  private
    FOnPageHide: TVirtualPageEvent;
    FOnPageLoad: TVirtualPageEvent;
    FOnPageShow: TVirtualPageEvent;
    FPageIndex: Integer;
    procedure SetPageIndex(AValue: Integer);
  protected
    procedure DoPageHide(Page: TVirtualPage); override;
    procedure DoPageLoad(Page: TVirtualPage); override;
    procedure DoPageShow(Page: TVirtualPage); override;
  public
    constructor Create;
    property OnPageHide: TVirtualPageEvent read FOnPageHide write FOnPageHide;
    property OnPageLoad: TVirtualPageEvent read FOnPageLoad write FOnPageLoad;
    property OnPageShow: TVirtualPageEvent read FOnPageShow write FOnPageShow;
    property PageIndex: Integer read FPageIndex write SetPageIndex;
  end;

  { TVirtualPageManager }

  TVirtualPageManager = class(TComponent)
  private
    FOnPageHide: TVirtualPageEvent;
    FOnPageLoad: TVirtualPageEvent;
    FOnPageShow: TVirtualPageEvent;
    FPageIndex: Integer;
    FPages: TVirtualPages;
    function GetDisplayOptions: TControlDisplayOptions;
    procedure SetDisplayOptions(AValue: TControlDisplayOptions);
    procedure SetPageIndex(AValue: Integer);
    procedure SetPages(AValue: TVirtualPages);
  protected
    procedure DoPageHide(Page: TVirtualPage);
    procedure DoPageLoad(Page: TVirtualPage);
    procedure DoPageShow(Page: TVirtualPage);
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CallMethod(const AMethodName: String; AllPages: Boolean);
    function PageByName(const PageName: String): TVirtualPage;
  published
    property DisplayOptions: TControlDisplayOptions read GetDisplayOptions write SetDisplayOptions;
    property OnPageHide: TVirtualPageEvent read FOnPageHide write FOnPageHide;
    property OnPageLoad: TVirtualPageEvent read FOnPageLoad write FOnPageLoad;
    property OnPageShow: TVirtualPageEvent read FOnPageShow write FOnPageShow;
    property PageIndex: Integer read FPageIndex write SetPageIndex default -1;
    property Pages: TVirtualPages read FPages write SetPages;
  end;

implementation

uses
  RtlConsts, TypInfo,

  Math, Forms;

type

  { TManagedPages }

  TManagedPages = class(TVirtualPages)
  private
    FManager: TVirtualPageManager;
  protected
    procedure DoPageHide(Page: TVirtualPage); override;
    procedure DoPageLoad(Page: TVirtualPage); override;
    procedure DoPageShow(Page: TVirtualPage); override;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AManager: TVirtualPageManager);
  end;

// from LuiMiscUtils
function InternalCallMethod(Instance: TObject; const MethodName: String): Boolean;
var
  Method: procedure of object;
begin
  TMethod(Method).Data := Instance;
  TMethod(Method).Code := Instance.MethodAddress(MethodName);
  Result := Assigned(Method);
  if Result then
    Method;
end;

  // from LuiRTTIUtils
function VarRecToString(const VarRec: TVarRec): String;
begin
  case VarRec.VType of
    vtAnsiString: Result := AnsiString(VarRec.VAnsiString);
    vtPChar: Result := String(VarRec.VPChar);
    vtString: Result := VarRec.VString^;
  else
    raise Exception.Create('Type mismatch: is not possible convert TVarRec to String');
  end;
end;

function VarRecToInteger(const VarRec: TVarRec): Integer;
begin
  case VarRec.VType of
    vtInteger: Result := VarRec.VInteger;
  else
    raise Exception.Create('Type mismatch: is not possible convert TVarRec to Integer');
  end;
end;

function VarRecToInt64(const VarRec: TVarRec): Int64;
begin
  case VarRec.VType of
    vtInteger: Result := VarRec.VInteger;
    vtInt64: Result := (VarRec.VInt64)^;
  else
    raise Exception.Create('Type mismatch: is not possible convert TVarRec to Int64');
  end;
end;

function VarRecToFloat(const VarRec: TVarRec): Double;
begin
  case VarRec.VType of
    vtInteger: Result := VarRec.VInteger;
    vtInt64: Result := (VarRec.VInt64)^;
    vtExtended: Result := (VarRec.VExtended)^;
  else
    raise Exception.Create('Type mismatch: is not possible convert TVarRec to Double');
  end;
end;


function VarRecToObject(const VarRec: TVarRec): TObject;
begin
  case VarRec.VType of
    vtObject: Result := VarRec.VObject;
  else
    raise Exception.Create('Type mismatch: is not possible convert TVarRec to TObject');
  end;
end;

function VarRecToInterface(const VarRec: TVarRec): Pointer;
begin
  case VarRec.VType of
    vtInterface: Result := VarRec.VInterface;
  else
    raise Exception.Create('Type mismatch: is not possible convert TVarRec to Interface');
  end;
end;

function VarRecToBoolean(const VarRec: TVarRec): Integer;
begin
  case VarRec.VType of
    vtBoolean: Result := Integer(VarRec.VBoolean);
  else
    raise Exception.Create('Type mismatch: is not possible convert TVarRec to Boolean');
  end;
end;

procedure SetObjectProperties(Instance: TObject; const Properties: array of const);
var
  i, PropCount: Integer;
  PropInfo: PPropInfo;
  ClassInfo: PTypeInfo;
  PropertyName, PropertyValue: TVarRec;
begin
  PropCount := Length(Properties);
  if PropCount = 0 then
    Exit;
  if odd(PropCount) then
    raise Exception.Create('SetObjectProperties - Properties must of even length');
  if Instance = nil then
    raise Exception.Create('SetObjectProperties - Instance is nil');
  ClassInfo := Instance.ClassInfo;
  for i := Low(Properties) to PropCount - 2 do
  begin
    //param names are at an even index and should of ansistring type
    PropertyName := Properties[i];
    if odd(i) or (PropertyName.VType <> vtAnsiString) then
      continue;
    //todo: optimize - use GetPropInfos to get all properties at once
    PropInfo := GetPropInfo(ClassInfo, AnsiString(PropertyName.VAnsiString));
    if PropInfo <> nil then
    begin
      PropertyValue := Properties[i + 1];
      case PropInfo^.PropType^.Kind of
        tkAString, tkSString:
          SetStrProp(Instance, PropInfo, VarRecToString(PropertyValue));
        tkInteger:
          SetOrdProp(Instance, PropInfo, VarRecToInteger(PropertyValue));
        tkInt64:
          SetInt64Prop(Instance, PropInfo, VarRecToInt64(PropertyValue));
        tkFloat:
          SetFloatProp(Instance, PropInfo, VarRecToFloat(PropertyValue));
        tkClass:
          SetObjectProp(Instance, PropInfo, VarRecToObject(PropertyValue));
        tkInterface:
          SetInterfaceProp(Instance, PropInfo, IInterface(VarRecToInterface(PropertyValue)));
        tkInterfaceRaw:
          SetRawInterfaceProp(Instance, PropInfo, VarRecToInterface(PropertyValue));
        tkBool:
          SetOrdProp(Instance, PropInfo, VarRecToBoolean(PropertyValue));
      else
        raise Exception.CreateFmt('SetObjectProperties - Error setting %s: kind %s is not supported',
          [PropInfo^.Name, GetEnumName(TypeInfo(TTypeKind), Integer(PropInfo^.PropType^.Kind))]);
      end;
    end;
  end;
end;

  // from VarRecUtils;

function CopyVarRec(const Item: TVarRec): TVarRec;
var
  W: WideString;
begin
  // Copy entire TVarRec first
  Result := Item;

  // Now handle special cases
  case Item.VType of
    vtExtended:
      begin
        New(Result.VExtended);
        Result.VExtended^ := Item.VExtended^;
      end;
    vtString:
      begin
        New(Result.VString);
        Result.VString^ := Item.VString^;
      end;
    vtPChar:
      Result.VPChar := StrNew(Item.VPChar);
    // there is no StrNew for PWideChar
    vtPWideChar:
      begin
        W := Item.VPWideChar;
        GetMem(Result.VPWideChar,
               (Length(W) + 1) * SizeOf(WideChar));
        Move(PWideChar(W)^, Result.VPWideChar^,
             (Length(W) + 1) * SizeOf(WideChar));
      end;
    // a little trickier: casting to AnsiString will ensure
    // reference counting is done properly
    vtAnsiString:
      begin
        // nil out first, so no attempt to decrement
        // reference count
        Result.VAnsiString := nil;
        AnsiString(Result.VAnsiString) := AnsiString(Item.VAnsiString);
      end;
    vtCurrency:
      begin
        New(Result.VCurrency);
        Result.VCurrency^ := Item.VCurrency^;
      end;
    vtVariant:
      begin
        New(Result.VVariant);
        Result.VVariant^ := Item.VVariant^;
      end;
    // casting ensures proper reference counting
    vtInterface:
      begin
        Result.VInterface := nil;
        IInterface(Result.VInterface) := IInterface(Item.VInterface);
      end;
    // casting ensures a proper copy is created
    vtWideString:
      begin
        Result.VWideString := nil;
        WideString(Result.VWideString) := WideString(Item.VWideString);
      end;
    vtInt64:
      begin
        New(Result.VInt64);
        Result.VInt64^ := Item.VInt64^;
      end;
    // VPointer and VObject don't have proper copy semantics so it
    // is impossible to write generic code that copies the contents
  end;
end;

function CreateConstArray(const Elements: array of const): TConstArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Elements));
  for I := Low(Elements) to High(Elements) do
    Result[I] := CopyVarRec(Elements[I]);
end;

// use this function on copied TVarRecs only!
procedure FinalizeVarRec(var Item: TVarRec);
begin
  case Item.VType of
    vtExtended: Dispose(Item.VExtended);
    vtString: Dispose(Item.VString);
    vtPChar: StrDispose(Item.VPChar);
    vtPWideChar: FreeMem(Item.VPWideChar);
    vtAnsiString: AnsiString(Item.VAnsiString) := '';
    vtCurrency: Dispose(Item.VCurrency);
    vtVariant: Dispose(Item.VVariant);
    vtInterface: IInterface(Item.VInterface) := nil;
    vtWideString: WideString(Item.VWideString) := '';
    vtInt64: Dispose(Item.VInt64);
  end;
  Item.VInteger := 0;
end;

procedure FinalizeConstArray(var Arr: TConstArray);
var
  I: Integer;
begin
  for I := Low(Arr) to High(Arr) do
    FinalizeVarRec(Arr[I]);
  Finalize(Arr);
  Arr := nil;
end;

{ TVirtualPageList }

procedure TVirtualPageList.SetPageIndex(AValue: Integer);
begin
  if FPageIndex = AValue then Exit;
  UpdateActivePage(FPageIndex, AValue);
  FPageIndex := AValue;
end;

procedure TVirtualPageList.DoPageHide(Page: TVirtualPage);
begin
  if Assigned(FOnPageHide) then
    FOnPageHide(Self, Page);
end;

procedure TVirtualPageList.DoPageLoad(Page: TVirtualPage);
begin
  if Assigned(FOnPageLoad) then
    FOnPageLoad(Self, Page);
end;

procedure TVirtualPageList.DoPageShow(Page: TVirtualPage);
begin
  if Assigned(FOnPageShow) then
    FOnPageShow(Self, Page);
end;

constructor TVirtualPageList.Create;
begin
  inherited Create(TVirtualPage);
  FPageIndex := -1;
end;

{ TManagedPages }

procedure TManagedPages.DoPageHide(Page: TVirtualPage);
begin
  FManager.DoPageHide(Page);
end;

procedure TManagedPages.DoPageLoad(Page: TVirtualPage);
begin
  FManager.DoPageLoad(Page);
end;

procedure TManagedPages.DoPageShow(Page: TVirtualPage);
begin
  FManager.DoPageShow(Page);
end;

function TManagedPages.GetOwner: TPersistent;
begin
  Result := FManager;
end;

constructor TManagedPages.Create(AManager: TVirtualPageManager);
begin
  inherited Create(TVirtualPage);
  FManager := AManager;
end;

{ TVirtualPageManager }

procedure TVirtualPageManager.SetPages(AValue: TVirtualPages);
begin
  FPages.Assign(AValue);
end;

procedure TVirtualPageManager.DoPageHide(Page: TVirtualPage);
begin
  if Assigned(FOnPageHide) then
    FOnPageHide(Self, Page);
end;

procedure TVirtualPageManager.DoPageLoad(Page: TVirtualPage);
begin
  if Assigned(FOnPageLoad) then
    FOnPageLoad(Self, Page);
end;

procedure TVirtualPageManager.DoPageShow(Page: TVirtualPage);
begin
  if Assigned(FOnPageShow) then
    FOnPageShow(Self, Page);
end;

procedure TVirtualPageManager.Loaded;
begin
  inherited Loaded;
  if (DisplayOptions.Parent = nil) and (Owner is TWinControl) then
    DisplayOptions.Parent := TWinControl(Owner);
  if FPageIndex > -1 then
    FPages.UpdateActivePage(-1, FPageIndex);
end;

procedure TVirtualPageManager.SetPageIndex(AValue: Integer);
begin
  if (FPageIndex = AValue) or (AValue >= FPages.Count)
    or (csLoading in ComponentState) then
    Exit;
  FPages.UpdateActivePage(FPageIndex, AValue);
  FPageIndex := AValue;
end;

procedure TVirtualPageManager.SetDisplayOptions(AValue: TControlDisplayOptions);
begin
  FPages.DisplayOptions := AValue;
end;

function TVirtualPageManager.GetDisplayOptions: TControlDisplayOptions;
begin
  Result := FPages.DisplayOptions;
end;

constructor TVirtualPageManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPages := TManagedPages.Create(Self);
  FPageIndex := -1;
end;

destructor TVirtualPageManager.Destroy;
begin
  FPages.Destroy;
  inherited Destroy;
end;

procedure TVirtualPageManager.CallMethod(const AMethodName: String; AllPages: Boolean);
var
  Page: TVirtualPage;
  i: Integer;
begin
  if AllPages then
  begin
    for i := 0 to FPages.Count -1 do
    begin
      Page := FPages[i];
      if Page.Control <> nil then
        InternalCallMethod(Page.Control, AMethodName);
    end;
  end
  else
  begin
    if (FPageIndex > -1) and (FPageIndex < FPages.Count) then
    begin
     Page := FPages[FPageIndex];
     if Page.Control <> nil then
       InternalCallMethod(Page.Control, AMethodName);
    end;
  end;
end;

function TVirtualPageManager.PageByName(const PageName: String): TVirtualPage;
begin
  Result := FPages.PageByName(PageName);
end;

{ TControlDisplayOptions }

procedure TControlDisplayOptions.SetBorderSpacing(AValue: TControlBorderSpacing);
begin
  FBorderSpacing.Assign(AValue);
end;

constructor TControlDisplayOptions.Create;
begin
  FBorderSpacing := TControlBorderSpacing.Create(nil);
  FAlign := alClient;
  FLeft := -1;
  FTop := -1;
  FWidth := -1;
  FHeight := -1;
end;

destructor TControlDisplayOptions.Destroy;
begin
  FBorderSpacing.Destroy;
  inherited Destroy;
end;

procedure TControlDisplayOptions.Assign(Source: TPersistent);
begin
  if Source is TControlDisplayOptions then
  begin
    FBorderSpacing.Assign(TControlDisplayOptions(Source).BorderSpacing);
    FAlign := TControlDisplayOptions(Source).FAlign;
    FHeight := TControlDisplayOptions(Source).FHeight;
    FLeft := TControlDisplayOptions(Source).FLeft;
    FTop := TControlDisplayOptions(Source).FTop;
    FWidth := TControlDisplayOptions(Source).FWidth;
  end
  else
    inherited Assign(Source);
end;

procedure TControlDisplayOptions.SetControlBounds(Control: TControl);
var
  NewLeft, NewTop, NewWidth, NewHeight: Integer;
begin
  NewLeft := IfThen(FLeft >= 0, FLeft, Control.Left);
  NewTop := IfThen(FTop >= 0, FTop, Control.Top);
  NewHeight := IfThen(FHeight >= 0, FHeight, Control.Height);
  NewWidth := IfThen(FWidth >= 0, FWidth, Control.Width);
  Control.SetBounds(NewLeft, NewTop, NewWidth, NewHeight);
end;

{ TVirtualPage }

procedure TVirtualPage.SetControl(Value: TControl);
begin
  if FControl = Value then
    Exit;
  FControl := Value;
  ControlChanged;
end;

procedure TVirtualPage.ControlChanged;
begin
  //
end;

function TVirtualPage.GetDisplayName: String;
begin
  if FCaption <> '' then
    Result := FCaption
  else
    Result := FName;
end;

destructor TVirtualPage.Destroy;
begin
  FinalizeConstArray(FProperties);
  inherited Destroy;
end;

procedure TVirtualPage.Assign(Source: TPersistent);
begin
  if Source is TVirtualPage then
  begin
    Control := TVirtualPage(Source).FControl;
    FCaption := TVirtualPage(Source).FCaption;
    FControlClass := TVirtualPage(Source).FControlClass;
    FControlClassName := TVirtualPage(Source).FControlClassName;
    FName := TVirtualPage(Source).FName;
    FProperties := TVirtualPage(Source).FProperties;
  end
  else
    inherited Assign(Source);
end;

procedure TVirtualPage.SetProperties(const Elements: array of const);
begin
  FinalizeConstArray(FProperties);
  FProperties := CreateConstArray(Elements);
end;

{ TVirtualPages }

function TVirtualPages.GetItems(Index: Integer): TVirtualPage;
begin
  Result := TVirtualPage(GetItem(Index));
end;

procedure TVirtualPages.SetDisplayOptions(AValue: TControlDisplayOptions);
begin
  FDisplayOptions.Assign(AValue);
end;

procedure TVirtualPages.DoPageHide(Page: TVirtualPage);
begin
  //
end;

procedure TVirtualPages.UpdateActivePage(OldPageIndex, NewPageIndex: Integer);
var
  Page: TVirtualPage;
  PageControl: TControl;
  PageControlClass: TControlClass;
  FoundClass: TPersistentClass;
begin
  if NewPageIndex >= Count then
    raise Exception.CreateFmt(SListIndexError, [NewPageIndex]);
  if OldPageIndex >= Count then
    raise Exception.CreateFmt(SListIndexError, [OldPageIndex]);
  if (NewPageIndex > -1) then
  begin
    Page := Items[NewPageIndex];
    PageControl := Page.Control;
    if (PageControl = nil) then
    begin
      PageControlClass := Page.ControlClass;
      if PageControlClass = nil then
      begin
        FoundClass := FindClass(Page.ControlClassName);
        if FoundClass.InheritsFrom(TControl) then
          PageControlClass := TControlClass(FoundClass);
      end;
      if (PageControlClass <> nil) and (FDisplayOptions.Parent <> nil) then
      begin
        //todo: use Fowner as Owner?
        PageControl := PageControlClass.Create(FDisplayOptions.Parent);
        PageControl.Name := 'VirtualPage' + IntToStr(NewPageIndex) + PageControlClass.ClassName;
        PageControl.Visible := False;
        //todo: see how avoid unecessary resizes
        FDisplayOptions.SetControlBounds(PageControl);
        PageControl.BorderSpacing := FDisplayOptions.BorderSpacing;
        PageControl.Align := FDisplayOptions.Align;
        PageControl.Parent := FDisplayOptions.Parent;
        Page.Control := PageControl;
        SetObjectProperties(PageControl, Page.Properties);
        DoPageLoad(Page);
        InternalCallMethod(PageControl, 'PageLoad');
      end;
    end;
    if PageControl <> nil then
    begin
      PageControl.Visible := True;
      DoPageShow(Page);
      InternalCallMethod(PageControl, 'PageShow');
    end;
  end;
  if (OldPageIndex > -1) then
  begin
   Page := Items[OldPageIndex];
   PageControl := Page.Control;
   if PageControl <> nil then
   begin
     DoPageHide(Page);
     InternalCallMethod(PageControl, 'PageHide');
     PageControl.Visible := False;
   end;
  end;
end;

procedure TVirtualPages.DoPageLoad(Page: TVirtualPage);
begin
  //
end;

procedure TVirtualPages.DoPageShow(Page: TVirtualPage);
begin
  //
end;

constructor TVirtualPages.Create(AItemClass: TCollectionItemClass);
begin
  inherited Create(AItemClass);
  FDisplayOptions := TControlDisplayOptions.Create;
end;

destructor TVirtualPages.Destroy;
begin
  FDisplayOptions.Destroy;
  inherited Destroy;
end;

procedure TVirtualPages.AssignTo(Dest: TPersistent);
var
  i: Integer;
begin
  if Dest is TStrings then
  begin
    TStrings(Dest).Clear;
    for i := 0 to Count - 1 do
      TStrings(Dest).Add(Items[i].Caption);
  end
  else
    inherited AssignTo(Dest);
end;

procedure TVirtualPages.Add(const Name, Caption: String; Control: TControl);
var
  Page: TVirtualPage;
begin
  Page := TVirtualPage(inherited Add);
  Page.Caption := Caption;
  Page.Control := Control;
  Page.Name := Name;
end;

procedure TVirtualPages.Add(const Name, Caption: String; ControlClass: TControlClass;
  const Properties: array of const);
var
  Page: TVirtualPage;
begin
  Page := TVirtualPage(inherited Add);
  Page.Caption := Caption;
  Page.ControlClass := ControlClass;
  Page.Name := Name;
  Page.SetProperties(Properties);
end;

procedure TVirtualPages.Add(const Name, Caption, ControlClassName: String;
  const Properties: array of const);
var
  Page: TVirtualPage;
begin
  Page := TVirtualPage(inherited Add);
  Page.Caption := Caption;
  Page.ControlClassName := ControlClassName;
  Page.Name := Name;
  Page.SetProperties(Properties);
end;

function TVirtualPages.FindPage(const PageName: String): TVirtualPage;
var
  i: Integer;
begin
  for i := 0 to Count -1 do
  begin
    Result := Items[i];
    if SameText(PageName, Result.Name) then
      Exit;
  end;
  Result := nil;
end;

function TVirtualPages.PageByName(const PageName: String): TVirtualPage;
begin
  Result := FindPage(PageName);
  if Result = nil then
    raise Exception.CreateFmt('Page "%s" not found', [PageName]);
end;

end.

