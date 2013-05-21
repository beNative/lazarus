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

unit ts_Components_XMLTree_NodeAttributes;

{$ifdef FPC}
{$mode delphi}
{$endif}

//*****************************************************************************

interface

uses
  Classes, SysUtils, Graphics,

  ts_Core_Collections;

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
  TNodeAttributes = class(TOwnedCollection<TNodeAttributesItem>)
  private
    function GetItemByType(Index: TNodeType): T;

  published
    property ItemByType[Index: TNodeType]: T
      read GetItemByType;
  end;

//*****************************************************************************

implementation

{$region 'TNodeAttributesItem' /fold}
{$region 'construction and destruction' /fold}
//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

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

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************
{$endregion}

{$region 'property access mehods' /fold}
//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

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
//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************
{$endregion}
{$endregion}

{$region 'TNodeAttributes' /fold}
{$region 'property access mehods' /fold}
//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

function TNodeAttributes.GetItemByType(Index: TNodeType): T;
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

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************
{$endregion}

{$REGION 'protected methods'}
//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

procedure TNodeAttributes.Update(Item: TCollectionItem);
begin
  (Owner as TXMLTree).Invalidate;
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************
{$ENDREGION}

{$endregion}



end.

