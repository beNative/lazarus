{ TTextAttributes }

function TTextAttributes.GetColor: TColor;
begin
  Result:= TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Font_GetColor(FRichBox);
end;

procedure TTextAttributes.SetColor(Value: TColor);
begin
  TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Font_SetColor(FRichBox, Value);
end;

function TTextAttributes.GetName: TFontName;
begin
  Result:= TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Font_GetName(FRichBox);
end;

procedure TTextAttributes.SetName(Value: TFontName);
begin
  TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Font_SetName(FRichBox, Value);
end;

function TTextAttributes.GetSize: Integer;
begin
  Result:= TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Font_GetSize(FRichBox);
end;

procedure TTextAttributes.SetSize(Value: Integer);
begin
  TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Font_SetSize(FRichBox, Value);
end;

function TTextAttributes.GetStyle: TFontStyles;
begin
  Result:= TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Font_GetStyle(FRichBox);
end;

procedure TTextAttributes.SetStyle(Value: TFontStyles);
begin
  TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Font_SetStyle(FRichBox, Value);
end;

function TTextAttributes.GetCharset: TFontCharset;
begin
  Result:= TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Font_GetCharset(FRichBox);
end;

procedure TTextAttributes.SetCharset(Value: TFontCharset);
begin
  TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Font_SetCharset(FRichBox, Value);
end;

function TTextAttributes.GetPitch: TFontPitch;
begin
  Result:= TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Font_GetPitch(FRichBox);
end;

procedure TTextAttributes.SetPitch(Value: TFontPitch);
begin
  TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Font_SetPitch(FRichBox, Value);
end;

function TTextAttributes.GetProtected: Boolean;
begin
  Result:= TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Font_GetProtected(FRichBox);
end;

procedure TTextAttributes.SetProtected(Value: Boolean);
begin
  TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Font_SetProtected(FRichBox, Value);
end;

constructor TTextAttributes.Create(AOwner: TCustomRichBox);
begin
  FRichBox:= AOwner;
end;

procedure TTextAttributes.Assign(Source: TPersistent);
begin
  if Source is TTextAttributes then
    begin
      Color:= TTextAttributes(Source).Color;
      Size:= TTextAttributes(Source).Size;
      Name:= TTextAttributes(Source).Name;
      Style:= TTextAttributes(Source).Style;
      Charset:= TTextAttributes(Source).Charset;
      Pitch:= TTextAttributes(Source).Pitch;
      Protect:= TTextAttributes(Source).Protect;
   end
  else inherited Assign(Source);
end;
