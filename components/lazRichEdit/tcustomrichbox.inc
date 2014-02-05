{ TCustomRichBox }

class procedure TCustomRichBox.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomRichBox;
end;

constructor TCustomRichBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSelAttributes := TTextAttributes.Create(Self);
  FParagraph := TParaAttributes.Create(Self);
end;

destructor TCustomRichBox.Destroy;
begin
  FParagraph.Free;
  FSelAttributes.Free;
  inherited Destroy;
end;

function TCustomRichBox.GetRealTextBuf: String;
begin
  Result:= TWSCustomRichBoxClass(WidgetSetClass).GetTextBuf(Self);
end;

function TCustomRichBox.GetRealtextSel: String;
begin
  Result:= TWSCustomRichBoxClass(WidgetSetClass).GetTextSel(Self);
end;

procedure TCustomRichBox.SaveToStream(Stream: TStream);
begin
  TWSCustomRichBoxClass(WidgetSetClass).SaveToStream(Self, Stream);
end;

procedure TCustomRichBox.LoadFromStream(Stream: TStream);
begin
  TWSCustomRichBoxClass(WidgetSetClass).LoadFromStream(Self, Stream);
end;



