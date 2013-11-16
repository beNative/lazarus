{ TParaAttributes }

function TParaAttributes.GetAlignment: TAlignment;
begin
  Result:= TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Para_GetAlignment(FRichBox);
end;

procedure TParaAttributes.SetAlignment(Value: TAlignment);
begin
  TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Para_SetAlignment(FRichBox, Value);
end;

function TParaAttributes.GetFirstIndent: LongInt;
begin
  Result:= TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Para_GetFirstIndent(FRichBox);
end;

procedure TParaAttributes.SetFirstIndent(Value: LongInt);
begin
  TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Para_SetFirstIndent(FRichBox, Value);
end;

function TParaAttributes.GetLeftIndent: LongInt;
begin
  Result:= TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Para_GetLeftIndent(FRichBox);
end;

procedure TParaAttributes.SetLeftIndent(Value: LongInt);
begin
  TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Para_SetLeftIndent(FRichBox, Value);
end;

function TParaAttributes.GetNumbering: TNumberingStyle;
begin
  Result:= TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Para_GetNumbering(FRichBox);
end;

procedure TParaAttributes.SetNumbering(Value: TNumberingStyle);
begin
  TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Para_SetNumbering(FRichBox, Value);
end;

function TParaAttributes.GetRightIndent: LongInt;
begin
  Result:= TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Para_GetRightIndent(FRichBox);
end;

procedure TParaAttributes.SetRightIndent(Value: LongInt);
begin
  TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Para_SetRightIndent(FRichBox, Value);
end;

function TParaAttributes.GetTab(Index: Byte): Longint;
begin
  Result:= TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Para_GetTab(FRichBox, Index);
end;

procedure TParaAttributes.SetTab(Index: Byte; Value: Longint);
begin
  TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Para_SetTab(FRichBox, Index, Value);
end;

function TParaAttributes.GetTabCount: Integer;
begin
  Result:= TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Para_GetTabCount(FRichBox);
end;

procedure TParaAttributes.SetTabCount(Value: Integer);
begin
  TWSCustomRichBoxClass(FRichBox.WidgetSetClass).Para_SetTabCount(FRichBox, Value);
end;

constructor TParaAttributes.Create(AOwner: TCustomRichBox);
begin
  FRichBox:= AOwner;
end;

procedure TParaAttributes.Assign(Source: TPersistent);
begin
  if Source is TParaAttributes then
  begin
    Alignment := TParaAttributes(Source).Alignment;
    FirstIndent := TParaAttributes(Source).FirstIndent;
    LeftIndent := TParaAttributes(Source).LeftIndent;
    RightIndent := TParaAttributes(Source).RightIndent;
    Numbering := TParaAttributes(Source).Numbering;
  end
  else inherited Assign(Source);
end;
