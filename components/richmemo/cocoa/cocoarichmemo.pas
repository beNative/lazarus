unit CocoaRichMemo;

interface

{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch objectivec2}

uses
  CocoaAll, Types, Classes, SysUtils,
  LCLType, Controls, StdCtrls,
  CocoaPrivate, CocoaUtils,
  CocoaWSCommon, CocoaWSStdCtrls,
  WSRichMemo, RichMemo;

type

  { TCocoaWSCustomRichMemo }

  TCocoaWSCustomRichMemo = class(TWSCustomRichMemo)
  public
    // assumption is made that LCL creates NSTextView
    class procedure SetParaAlignment(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AAlign: TIntParaAlignment); override;
    class function GetParaAlignment(const AWinControl: TWinControl; TextStart: Integer;
      var AAlign: TIntParaAlignment): Boolean; override;
    class procedure InDelText(const AWinControl: TWinControl;
      const TextUTF8: String; DstStart, DstLen: Integer); override;
    class function LoadRichText(const AWinControl: TWinControl; Source: TStream): Boolean; override;
  end;

implementation


function MemoTextView(AWinControl: TWinControl): TCocoaTextView;
begin
  if not Assigned(AWinControl) or (AWinControl.Handle=0) then
    Result := nil
  else
    Result := TCocoaTextView(NSScrollView(AWinControl.Handle).documentView);
end;

{ TCocoaWSCustomRichMemo }

class procedure TCocoaWSCustomRichMemo.SetParaAlignment(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AAlign: TIntParaAlignment);
var
  txt : TCocoaTextView;
  rng : NSRange;
const
  TxtAlign : array [TIntParaAlignment] of integer = (
   NSLeftTextAlignment,  NSRightTextAlignment, NSCenterTextAlignment, NSJustifiedTextAlignment
  );
begin
  txt:=MemoTextView(AWinControl);
  if not Assigned(txt) then Exit;

  rng.location:=TextStart;
  rng.length:=TextLen;
  if TextStart+TextLen>txt.textStorage.string_.length then
    rng.length:=txt.textStorage.string_.length-TextStart;

  rng:=txt.textStorage.string_.paragraphRangeForRange(rng);
  txt.setAlignment_range(TxtAlign[AAlign], rng);
end;

class function TCocoaWSCustomRichMemo.GetParaAlignment(
  const AWinControl: TWinControl; TextStart: Integer;
  var AAlign: TIntParaAlignment): Boolean;
var
  txt : TCocoaTextView;
  rng : NSRange;
  cur : NSRange;
  al  : NSTextAlignment;
const
  TxtAlign : array [TIntParaAlignment] of integer = (
   NSLeftTextAlignment,  NSRightTextAlignment, NSCenterTextAlignment, NSJustifiedTextAlignment
  );
begin
  txt:=MemoTextView(AWinControl);
  if not Assigned(txt) then begin
    Result:=false;
    Exit;
  end;

  cur:=txt.selectedRange;
  rng.location:=TextStart;
  rng.length:=1;
  if TextStart+1>txt.textStorage.string_.length then
    rng.length:=txt.textStorage.string_.length-TextStart;


  rng:=txt.textStorage.string_.paragraphRangeForRange(rng);
  txt.setSelectedRange(rng);
  al:=txt.alignment;
  case al of
    NSRightTextAlignment: AAlign:=paRight;
    NSCenterTextAlignment: AAlign:=paCenter;
    NSJustifiedTextAlignment: AAlign:=paJustify;
  else
    AAlign:=paLeft;
  end;
  txt.setSelectedRange(cur);
  Result:=true;
end;

class procedure TCocoaWSCustomRichMemo.InDelText(
  const AWinControl: TWinControl; const TextUTF8: String; DstStart,
  DstLen: Integer);
var
  txt : TCocoaTextView;
  str : NSString;
begin
  txt:=MemoTextView(AWinControl);
  if not Assigned(txt) then Exit;

  str := NSStringUtf8(TextUtf8);
  txt.textStorage.replaceCharactersInRange_withString(NSMakeRange(DstStart, DstLen), str);
  str.release;
end;

class function TCocoaWSCustomRichMemo.LoadRichText(
  const AWinControl: TWinControl; Source: TStream): Boolean;
var
  data: NSMutableData;
  rng : NSRange;
  txt : TCocoaTextView;
begin
  //todo: avoid copying data.
  if not Assigned(Source) or not Assigned(AWinControl) or (AWinControl.Handle=0) then Exit;

  txt:=MemoTextView(AWinControl);
  if Source.size>0 then begin
    data:=NSMutableData(NSMutableData.alloc).initWithLength(Source.size);
    Source.Read(data.mutableBytes^, Source.Size);
    rng.length:=txt.textStorage.string_.length;
    rng.location:=0;
    txt.replaceCharactersInRange_withRTF(rng, data);
    data.release;
  end;
end;

end.
