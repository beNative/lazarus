unit CocoaRichMemo;

interface

{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch objectivec2}

uses
  CocoaAll, Types,
  LCLType, Controls, StdCtrls,
  CocoaPrivate, CocoaUtils,
  CocoaWSCommon, CocoaWSStdCtrls,
  WSRichMemo;

type

  { TCocoaWSCustomRichMemo }

  TCocoaWSCustomRichMemo = class(TWSCustomRichMemo)
  public
    // assumption is made that LCL creates NSTextView
    class procedure SetParaAlignment(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AAlign: TIntParaAlignment); override;
    class procedure InDelText(const AWinControl: TWinControl;
      const TextUTF8: String; DstStart, DstLen: Integer); override;
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
  rng:=txt.textStorage.string_.paragraphRangeForRange(rng);
  txt.setAlignment_range(TxtAlign[AAlign], rng);
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

end.
