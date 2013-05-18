unit ts_Editor_CodeTags;

{$mode delphi}

//*****************************************************************************

interface

uses
  Classes, SysUtils,

  ts_Core_Collections;

type
  TCodeTagItem = class(TCollectionItem)
  strict private
    FEndTag   : string;
    FStartTag : string;

    procedure SetEndTag(AValue: string);
    procedure SetStartTag(AValue: string);

  published
    property StartTag: string
      read FStartTag write SetStartTag;

    property EndTag: string
      read FEndTag write SetEndTag;
  end;

  TCodeTags = class(TOwnedCollection<TCodeTagItem>)

  end;

//*****************************************************************************

implementation

{ TCodeTagItem }

procedure TCodeTagItem.SetStartTag(AValue: string);
begin
  if FStartTag=AValue then Exit;
  FStartTag:=AValue;
end;

procedure TCodeTagItem.SetEndTag(AValue: string);
begin
  if FEndTag=AValue then Exit;
  FEndTag:=AValue;
end;

end.

