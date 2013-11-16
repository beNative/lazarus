{
lzRichEdit

Copyright (C) 2010 Elson Junio elsonjunio@yahoo.com.br

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

This library is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
for more details.

You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit RichBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, StdCtrls, Graphics, LCLType, LCLProc;

type

TNumberingStyle=( nsNone, nsBullets);

TCustomRichBox = class;

{ TTextAttributes }

TTextAttributes = class(TPersistent)
 private
   FRichBox: TCustomRichBox;
 private
   function GetColor: TColor;
   procedure SetColor(Value: TColor);
   function GetName: TFontName;
   procedure SetName(Value: TFontName);
   function GetSize: Integer;
   procedure SetSize(Value: Integer);
   function GetStyle: TFontStyles;
   procedure SetStyle(Value: TFontStyles);
   function GetCharset: TFontCharset;
   procedure SetCharset(Value: TFontCharset);
   function GetPitch: TFontPitch;
   procedure SetPitch(Value: TFontPitch);
   function GetProtected: Boolean;
   procedure SetProtected(Value: Boolean);
 public
   constructor Create(AOwner: TCustomRichBox);
   procedure Assign(Source: TPersistent); override;
   property Charset: TFontCharset read GetCharset write SetCharset;
   property Color: TColor read GetColor write SetColor;
   property Name: TFontName read GetName write SetName;
   property Pitch: TFontPitch read GetPitch write SetPitch;
   property Protect: Boolean read GetProtected write SetProtected;
   property Size: Integer read GetSize write SetSize;
   property Style: TFontStyles read GetStyle write SetStyle;
 end;

{ TParaAttributes }
TParaAttributes = class(TPersistent)
private
  FRichBox: TCustomRichBox;
private
  function GetAlignment: TAlignment;
  procedure SetAlignment(Value: TAlignment);
  function GetFirstIndent: LongInt;
  procedure SetFirstIndent(Value:LongInt);
  function GetLeftIndent:LongInt;
  procedure SetLeftIndent(Value:LongInt);
  function GetNumbering: TNumberingStyle;
  procedure SetNumbering(Value: TNumberingStyle);
  function GetRightIndent: LongInt;
  procedure SetRightIndent(Value:LongInt);
  function GetTab(Index: Byte): Longint;
  procedure SetTab(Index: Byte; Value: Longint);
  function GetTabCount: Integer;
  procedure SetTabCount(Value: Integer);
public
    constructor Create(AOwner: TCustomRichBox);
    procedure Assign(Source: TPersistent); override;
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property FirstIndent: Longint read GetFirstIndent write SetFirstIndent;
    property LeftIndent: Longint read GetLeftIndent write SetLeftIndent;
    property Numbering: TNumberingStyle read GetNumbering write SetNumbering;
    property RightIndent: Longint read GetRightIndent write SetRightIndent;
    property Tab[Index: Byte]: Longint read GetTab write SetTab;
    property TabCount: Integer read GetTabCount write SetTabCount;
end;

{ TCustomRichBox }
TCustomRichBox = class(TCustomMemo)
  private
    FSelAttributes: TTextAttributes;
    FParagraph: TParaAttributes;
    FPlainText: Boolean;
  protected
    class procedure WSRegisterClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetRealTextBuf: String;
    function GetRealtextSel: String;
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
  public
    property Paragraph: TParaAttributes read FParagraph;
    property SelAttributes: TTextAttributes read FSelAttributes;
    property PlainText:Boolean read FPlainText write FPlainText default False;
  end;

{ TCustomRichBox }
TRichBox = class(TCustomRichBox)
published
  property Align;
  property Alignment;
  property Anchors;
  property BidiMode;
  property BorderSpacing;
  property BorderStyle;
  property CharCase;
  property Color;
  property Constraints;
  property DragCursor;
  property DragKind;
  property DragMode;
  property Enabled;
  property Font;
  property HideSelection;
  property Lines;
  property MaxLength;
  property OnChange;
  property OnClick;
  property OnContextPopup;
  property OnDblClick;
  property OnDragDrop;
  property OnDragOver;
  property OnEditingDone;
  property OnEndDrag;
  property OnEnter;
  property OnExit;
  property OnKeyDown;
  property OnKeyPress;
  property OnKeyUp;
  property OnMouseDown;
  property OnMouseEnter;
  property OnMouseLeave;
  property OnMouseMove;
  property OnMouseUp;
  property OnMouseWheel;
  property OnMouseWheelDown;
  property OnMouseWheelUp;
  property OnStartDrag;
  property OnUTF8KeyPress;
  property ParentBidiMode;
  property ParentColor;
  property ParentFont;
  property PopupMenu;
  property ParentShowHint;
  property ReadOnly;
  property ScrollBars;
  property ShowHint;
  property TabOrder;
  property TabStop;
  property Visible;
  property WantReturns;
  property WantTabs;
  property WordWrap;
end;

TlzRichEdit = class(TRichBox)
end;
procedure Register;

implementation
uses
  WSRichBox;
procedure Register;
begin
  RegisterComponents('Common Controls', [TlzRichEdit]);
end;

{$I tparaattributes.inc}
{$I ttextattributes.inc}
{$I tcustomrichbox.inc}
initialization
{$I lzrichedit.lrs}
end.
