unit QtRichMemo;

interface
//
// Following class methods are need for the implementation
//  QTextCharFormatH
//  QTextBlockFormatH
uses
  LCLType, Controls, StdCtrls,
  qt4, qtwidgets, qtprivate,
  WSProc,
  RichMemo, WSRichMemo;

type
  { TQtWSCustomRichMemo }

  TQtWSCustomRichMemo = class(TWSCustomRichMemo)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class function GetParaAlignment(const AWinControl: TWinControl; TextStart: Integer;
      var AAlign: TIntParaAlignment): Boolean; override;
    class procedure SetParaAlignment(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AAlign: TIntParaAlignment); override;
  end;

implementation

const
  WordWrapMap: array[Boolean] of QTextEditLineWrapMode =
  (
    QTextEditNoWrap,
    QTextEditWidgetWidth
  );

  AlignmentMap: array[TIntParaAlignment] of QtAlignment =
  (
    QtAlignLeft,
    QtAlignRight,
    QtAlignHCenter,
    QtAlignJustify
  );

{ TQtWSCustomRichMemo }

class function TQtWSCustomRichMemo.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  QtTextEdit: TQtTextEdit;
begin
  QtTextEdit := TQtTextEdit.Create(AWinControl, AParams);
  QtTextEdit.AcceptRichText := True;
  QtTextEdit.ClearText;
  QtTextEdit.setBorder(TCustomMemo(AWinControl).BorderStyle = bsSingle);
  QtTextEdit.setReadOnly(TCustomMemo(AWinControl).ReadOnly);
  QtTextEdit.setLineWrapMode(WordWrapMap[TCustomMemo(AWinControl).WordWrap]);
  // create our FList helper
  QtTextEdit.FList := TQtMemoStrings.Create(TCustomMemo(AWinControl));
  QtTextEdit.setScrollStyle(TCustomMemo(AWinControl).ScrollBars);
  QtTextEdit.setTabChangesFocus(not TCustomMemo(AWinControl).WantTabs);

  QtTextEdit.AttachEvents;

  Result := TLCLIntfHandle(QtTextEdit);
end;

class procedure TQtWSCustomRichMemo.SetParaAlignment(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AAlign: TIntParaAlignment);
var
  w : QTextEditH;
  te : TQtTextEdit;
  ss, sl :  Integer;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetParaAlignment') then
    Exit;
  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);

  ss:=te.getSelectionStart;
  sl:=te.getSelectionLength;

  te.setSelection(TextStart, TextLen);

  // alignment
  QTextEdit_setAlignment(w, AlignmentMap[AAlign]);

  te.setSelection(ss, sl);
end;

class function TQtWSCustomRichMemo.GetParaAlignment(
  const AWinControl: TWinControl; TextStart: Integer;
  var AAlign: TIntParaAlignment): Boolean;
var
  te : TQtTextEdit;
  al : QtAlignment;
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetParaAlignment') then begin
    Result:=false;
    Exit;
  end;
  te:=TQtTextEdit(AWinControl.Handle);
  al:=QTextEdit_alignment(QTextEditH(te.Widget));
  if QtAlignLeading and al > 0 then AAlign:=paLeft
  else if QtAlignTrailing and al > 0 then AAlign:=paRight
  else if QtAlignCenter and al > 0 then AAlign:=paCenter
  else if QtAlignJustify and al > 0 then AAlign:=paJustify
  else AAlign:=paLeft;
  Result:=true;
end;

end.
