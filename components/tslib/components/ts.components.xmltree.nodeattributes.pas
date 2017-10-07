{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Components.XMLTree.NodeAttributes;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, Graphics,

  ts.Core.Collections;

type
  TNodeType = (
    ntUnknown,   // Error
    ntRoot,     // DocumentElement, NODE_DOCUMENT, NODE_PROCESSING_INSTRUCTION
    ntComment,  // NODE_COMMENT
    ntText,     // NODE_TEXT, NODE_CDATA_SECTION
    ntAttribute,// NODE_ATTRIBUTE
    ntElement,  // NODE_ELEMENT without ChildNodes
    ntNode      // NODE_ELEMENT with ChildNodes
  );

type
  TCollectionItem = Classes.TCollectionItem;

type
  TNodeAttributesItem = class(TCollectionItem)
  private
    FBackGroundColor      : TColor;
    FFont                 : TFont;
    FName                 : string;
    FNodeType             : TNodeType;
    FValueFont            : TFont;
    FValueBackGroundColor : TColor;

    procedure SetBackGroundColor(AValue: TColor);
    procedure SetFont(AValue: TFont);
    procedure SetNodeType(AValue: TNodeType);
    procedure SetValueFont(const Value: TFont);
    procedure SetValueBackGroundColor(const Value: TColor);

  protected
    function GetDisplayName: string; override;

  public
    constructor Create(ACollection: Classes.TCollection); override;
    procedure BeforeDestruction; override;

  published
    property Name: string
      read FName write FName;

    property NodeType: TNodeType
      read FNodeType write SetNodeType;

    property BackGroundColor: TColor
      read FBackGroundColor write SetBackGroundColor;

    property Font: TFont
      read FFont write SetFont;

    property ValueFont: TFont
      read FValueFont write SetValueFont;

    property ValueBackGroundColor: TColor
      read FValueBackGroundColor write SetValueBackGroundColor;
  end;

type

  { TNodeAttributes }

  TNodeAttributes = class(TOwnedCollection<TNodeAttributesItem>)
  private
    function GetItemByType(Index: TNodeType): TNodeAttributesItem;

  published
    property ItemByType[Index: TNodeType]: TNodeAttributesItem
      read GetItemByType;
  end;

implementation

{$REGION 'TNodeAttributesItem' /FOLD}
{$REGION 'construction and destruction' /FOLD}

constructor TNodeAttributesItem.Create(ACollection: Classes.TCollection);
begin
  inherited Create(ACollection);
  FFont := TFont.Create;
  FFont.Size := 8;
  FValueFont := TFont.Create;
  FValueFont.Size := 8;
  FBackGroundColor := clWhite;
  FValueBackGroundColor := clWhite;
end;

function TNodeAttributesItem.GetDisplayName: string;
begin
  Result := FName;
end;

procedure TNodeAttributesItem.BeforeDestruction;
begin
  FFont.Free;
  FValueFont.Free;
  inherited BeforeDestruction;
end;

{$ENDREGION}

{$REGION 'property access mehods' /FOLD}

procedure TNodeAttributesItem.SetBackGroundColor(AValue: TColor);
begin
  if AValue <> BackGroundColor then
  begin
    FBackGroundColor := AValue;
    Changed(True);
  end;
end;

procedure TNodeAttributesItem.SetFont(AValue: TFont);
begin
  if not Font.IsEqual(AValue) then
  begin
    FFont.Assign(AValue);
    Changed(False);
  end;
end;

procedure TNodeAttributesItem.SetNodeType(AValue: TNodeType);
begin
  if AValue <> NodeType then
  begin;
    FNodeType := AValue;
    Changed(False);
  end;
end;

procedure TNodeAttributesItem.SetValueBackGroundColor(const Value: TColor);
begin
  FValueBackGroundColor := Value;
  Changed(False);
end;

procedure TNodeAttributesItem.SetValueFont(const Value: TFont);
begin
  FValueFont.Assign(Value);
  Changed(False);
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'TNodeAttributes' /FOLD}
{$REGION 'property access mehods' /FOLD}

function TNodeAttributes.GetItemByType(Index: TNodeType): TNodeAttributesItem;
var
  I : Integer;
  B : Boolean;
begin
  I := 0;
  B := False;
  Result := nil;
  while (I < Count) and not B do
  begin
    if Items[I].NodeType = Index then
    begin
      Result := Items[I];
      B := True;
    end;
    Inc(I);
  end;
end;

{$ENDREGION}

{$REGION 'protected methods'}

//procedure TNodeAttributes.Update(Item: TCollectionItem);
//begin
//  (Owner as TXMLTree).Invalidate;
//end;

{$ENDREGION}

{$ENDREGION}

end.

