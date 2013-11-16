unit UPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Menus, ExtCtrls, Buttons, LCLProc,
  LCLType, ColorBox, Process, ULocalizar, UParagrafo, USobre,
  RichBox{$IFDEF LINUX}, GTKTextImage, UGetFontLinux{$ENDIF}
  {$IFDEF WINDOWS}, RichOleBox, RichOle{$ENDIF}, RTF2HTML;

type

  { TForm1 }

  TForm1 = class(TForm)
    ColorButton1: TColorButton;
    CBFont: TComboBox;
    CBSize: TComboBox;
    FDlg: TFontDialog;
    GroupBox1: TGroupBox;
    ImageList1: TImageList;
    lzRichEdit1: TlzRichEdit;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    Odlg: TOpenDialog;
    SDlg: TSaveDialog;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton2: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton26: TToolButton;
    ToolButton27: TToolButton;
    ToolButton28: TToolButton;
    ToolButton29: TToolButton;
    ToolButton3: TToolButton;
    ToolButton30: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure HTMLClick(Sender: TObject);
    procedure CBFontSelect(Sender: TObject);
    procedure CBSizeChange(Sender: TObject);
    procedure ColorButton1ChangeBounds(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lzRichEdit1Change(Sender: TObject);
    procedure lzRichEdit1Click(Sender: TObject);
    procedure lzRichEdit1KeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem19Click(Sender: TObject);
    procedure MenuItem22Click(Sender: TObject);
    procedure MenuItem24Click(Sender: TObject);
    procedure MenuItem26Click(Sender: TObject);
    procedure MenuItem27Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure ToolButton11Click(Sender: TObject);
    procedure ToolButton12Click(Sender: TObject);
    procedure ToolButton13Click(Sender: TObject);
    procedure ToolButton14Click(Sender: TObject);
    procedure ToolButton17Click(Sender: TObject);
    procedure ToolButton18Click(Sender: TObject);
    procedure ToolButton19Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton21Click(Sender: TObject);
    procedure ToolButton22Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton30Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
    procedure ToolButton8Click(Sender: TObject);
    procedure ToolButton9Click(Sender: TObject);
  private
    { private declarations }
  private
  {$IFDEF Windows}
    //Suporte a Objeto Ole
    procedure CreateOLEObjectInterface;
    procedure CloseOLEObjects;
  {$ENDIF}
  protected
    { protected declarations }
    FFileName: string;
    FSetColor: boolean;
  protected
    { protected declarations }
    procedure SetFileName(S: string);
    procedure GetTextStatus;
  public
    { public declarations }
    property FileName: string read FFileName write SetFileName;
  public
    {$IFDEF Windows}
      RichEditOle: IRichEditOle;
      RichEditOleCallback: IRichEditOleCallback;
    {$ENDIF}

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ToolButton13Click(Sender: TObject);
var
  S: TMemoryStream;
begin
  //-- Abrir
  Odlg.Title := 'Abrir...';
  Odlg.Filter := 'Rich Text (*.rtf)|*.rtf|Texto (*.txt)|*.txt';
  Odlg.Options := [ofEnableSizing, ofViewDetail, ofHideReadOnly];
  if Odlg.Execute then
  begin
    if not (FileExists(Odlg.FileName)) then
    begin
      MessageDlg('Erro ao Abrir', 'O arquivo especificado não existe.',
        mtError, [mbOK], 0);
      Exit;
    end;

    if UTF8LowerCase(ExtractFileExt(Odlg.FileName)) = '.rtf' then
      lzRichEdit1.PlainText := False
    else
      lzRichEdit1.PlainText := True;
    //--
    if not (FileIsReadOnly(Odlg.FileName)) then
      FileName := Odlg.FileName
    else
      FileName := '';
    //--
    lzRichEdit1.Clear;
    S := TMemoryStream.Create;
    S.LoadFromFile(Odlg.FileName);
    S.Seek(0 ,soBeginning);
    lzRichEdit1.LoadFromStream(S);
    S.Free;
    //--
  end;
end;

procedure TForm1.ToolButton14Click(Sender: TObject);
var
  S: TFileStream;
begin
  if (FileName = '') then
  begin
    MenuItem7Click(Sender);
    Exit;
  end;
  //--
  S := TFileStream.Create(FileName, fmCreate);
  lzRichEdit1.SaveToStream(S);
  S.Free;
end;

procedure TForm1.ToolButton17Click(Sender: TObject);
begin
  frmLocalizar.Execute('', lzRichEdit1);
end;

procedure TForm1.ToolButton18Click(Sender: TObject);
begin
  lzRichEdit1.CutToClipboard;
end;

procedure TForm1.ToolButton19Click(Sender: TObject);
begin
  lzRichEdit1.CopyToClipboard;
end;

procedure TForm1.ToolButton1Click(Sender: TObject);

begin
  if (fsBold in lzRichEdit1.SelAttributes.Style) then
    lzRichEdit1.SelAttributes.Style := lzRichEdit1.SelAttributes.Style - [fsBold]
  else
    lzRichEdit1.SelAttributes.Style := lzRichEdit1.SelAttributes.Style + [fsBold];

  GetTextStatus;
end;

procedure TForm1.ToolButton21Click(Sender: TObject);
begin
  lzRichEdit1.PasteFromClipboard;
end;

procedure TForm1.ToolButton22Click(Sender: TObject);
begin
  lzRichEdit1.Undo;
end;

procedure TForm1.ToolButton2Click(Sender: TObject);

begin
  if (fsItalic in lzRichEdit1.SelAttributes.Style) then
    lzRichEdit1.SelAttributes.Style := lzRichEdit1.SelAttributes.Style - [fsItalic]
  else
    lzRichEdit1.SelAttributes.Style := lzRichEdit1.SelAttributes.Style + [fsItalic];
  GetTextStatus;
end;

procedure TForm1.ToolButton30Click(Sender: TObject);
var
  Rtf2HTML:TRtf2HTML;
  S : TMemoryStream;
begin
  //--Salvar Como HTML
  Sdlg.Title := 'Salvar HTML';
  Sdlg.Filter := 'HTML (*.html)|*.html';
  Sdlg.Options := [ofEnableSizing, ofViewDetail, ofHideReadOnly];

  if Sdlg.Execute then
  begin
    //--
    if ExtractFileExt(Sdlg.FileName) = '' then
    begin
        Sdlg.FileName := Sdlg.FileName + '.html'
    end;
    //--
    if FileExists(Sdlg.FileName) then
    begin
      if (MessageDlg('Salvar HTML', Sdlg.FileName + ' já existe. ' +
        #10 + 'deseja substituí-lo?', mtWarning, [mbYes, mbNo], 0) <> 6) then
        Exit;
      if (FileIsReadOnly(Sdlg.FileName)) then
      begin
        MessageDlg('Salvar HTML', 'O arquivo ' + Sdlg.FileName +
          ' é somente leitura.',
          mtWarning, [mbOK], 0);
        ToolButton30Click(Sender);
        Exit;
      end;
    end;
    //--
    S := TMemoryStream.Create;
    lzRichEdit1.SaveToStream(S);
    //--
    Rtf2HTML:= TRtf2HTML.Create;
    Rtf2HTML.SunFontSize:= 5;
    Rtf2HTML.Convert(S, ExtractFileDir(Sdlg.FileName), ExtractFileName(Sdlg.FileName));
    Rtf2HTML.Free;
  end;

end;

procedure TForm1.ToolButton3Click(Sender: TObject);
begin
   if (fsUnderline in lzRichEdit1.SelAttributes.Style) then
    lzRichEdit1.SelAttributes.Style := lzRichEdit1.SelAttributes.Style - [fsUnderline]
  else
    lzRichEdit1.SelAttributes.Style := lzRichEdit1.SelAttributes.Style + [fsUnderline];

  GetTextStatus;
end;

procedure TForm1.ToolButton6Click(Sender: TObject);
begin
  lzRichEdit1.Paragraph.Alignment:= taLeftJustify;
  GetTextStatus;
end;

procedure TForm1.ToolButton8Click(Sender: TObject);
begin
  lzRichEdit1.Paragraph.Alignment:= taCenter;
  GetTextStatus;
end;

procedure TForm1.ToolButton9Click(Sender: TObject);
begin
  lzRichEdit1.Paragraph.Alignment:= taRightJustify;
  GetTextStatus;
end;
{$IFDEF WINDOWS}
procedure TForm1.CreateOLEObjectInterface;
begin
  RichEditOleCallback := TRichEditOleCallback.Create(lzRichEdit1);

   if not RichEdit_GetOleInterface(lzRichEdit1.Handle, RichEditOle) then
     raise Exception.Create('Unable to get interface');
   if not RichEdit_SetOleCallback(lzRichEdit1.Handle, RichEditOlecallback) then
         raise Exception.Create('Unable to set callback');

end;

procedure TForm1.CloseOLEObjects;
var
I, ObjCount: Integer;
ReObject: TReObject;

begin

if not Assigned(RichEditOle) then Exit;
  FillChar(ReObject, SizeOf(ReObject), 0);
  ReObject.cbStruct := SizeOf(ReObject);
  ObjCount := RichEditOle.GetObjectCount;
  for I := 1 to RicheditOle.GetObjectCount do
    if RichEditOle.GetObject(I, ReObject, REO_GETOBJ_POLEOBJ) = S_OK then
      ReObject.poleobj.CLOSE(OLECLOSE_NOSAVE);

end;
{$ENDIF}
procedure TForm1.ToolButton12Click(Sender: TObject);
var
  NewProcess: TProcess;
begin
  //Chama um novo RichEdit
  NewProcess := TProcess.Create(nil);
  //--
  NewProcess.CommandLine := ParamStr(0);
  NewProcess.Options := [poNoConsole];
  NewProcess.Execute;
  NewProcess.Free;
  //--
end;

procedure TForm1.MenuItem7Click(Sender: TObject);
var
  S: TFileStream;
begin
  //--Salvar Como
  Sdlg.Title := 'Salvar Como';
  Sdlg.Filter := 'Rich Text (*.rtf)|*.rtf|Texto (*.txt)|*.txt';
  Sdlg.Options := [ofEnableSizing, ofViewDetail, ofHideReadOnly];

  if Sdlg.Execute then
  begin
    //--
    if ExtractFileExt(Sdlg.FileName) = '' then
    begin
      if (Sdlg.FilterIndex = 1) then
        Sdlg.FileName := Sdlg.FileName + '.rtf'
      else
        Sdlg.FileName := Sdlg.FileName + '.txt';
    end;
    //--
    if FileExists(Sdlg.FileName) then
    begin
      if (MessageDlg('Salvar Como', Sdlg.FileName + ' já existe. ' +
        #10 + 'deseja substituí-lo?', mtWarning, [mbYes, mbNo], 0) <> 6) then
        Exit;
      if (FileIsReadOnly(Sdlg.FileName)) then
      begin
        MessageDlg('Salvar Como', 'O arquivo ' + Sdlg.FileName +
          ' é somente leitura.',
          mtWarning, [mbOK], 0);
        MenuItem7Click(Sender);
        Exit;
      end;
    end;
    //--
    S := TFileStream.Create(Sdlg.FileName, fmCreate);
    if (UTF8LowerCase(ExtractFileExt(Sdlg.FileName)) = '.rtf') then
      lzRichEdit1.SaveToStream(S)
    else
      lzRichEdit1.Lines.SaveToStream(S);
    S.Free;
    //--
    FileName := Sdlg.FileName;
  end;
end;

procedure TForm1.ToolButton11Click(Sender: TObject);
begin

  if lzRichEdit1.Paragraph.Numbering = nsNone then
    begin
      lzRichEdit1.Paragraph.Numbering:= nsBullets;
    end
  else
    begin
      lzRichEdit1.Paragraph.Numbering:= nsNone;
    end;

  GetTextStatus;
end;

procedure TForm1.lzRichEdit1Click(Sender: TObject);
begin
  GetTextStatus;
end;

procedure TForm1.ColorButton1ChangeBounds(Sender: TObject);
begin
  lzRichEdit1.SelAttributes.Color:= ColorButton1.ButtonColor;
  GetTextStatus;
end;

procedure TForm1.CBFontSelect(Sender: TObject);
begin
  lzRichEdit1.SelAttributes.Name:= CBfont.Text;
  GetTextStatus;
end;

procedure TForm1.HTMLClick(Sender: TObject);
begin

end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin

end;

procedure TForm1.CBSizeChange(Sender: TObject);
var
  FontSize: integer;
begin
  if TryStrToInt(CBSize.Text, FontSize) then
  begin
    lzRichEdit1.SelAttributes.Size:= FontSize;
  end
  else
    MessageDlg('Formatar', 'Número inválido', mtInformation, [mbOK], 0);

  GetTextStatus;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if MessageDlg('Salvar', 'Deseja Salvar o documento?', mtConfirmation,
    [mbYes, mbNo], 0) = 6 then
    ToolButton14Click(Sender);
{$IFDEF Windows}
  //Limpa Objetos OLE
  CloseOLEObjects;
{$ENDIF}
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FSetColor := True;
  {$IFDEF Linux}
    ToolButton22.Enabled := False;
    MenuItem13.Enabled := False;
  {$ENDIF}
end;

procedure TForm1.FormShow(Sender: TObject);
var
  sFont: TFont;
  I, I2, I3: integer;
{$IFDEF Linux}
  FontList: TStringList;
{$ENDIF}
begin
{$IFDEF Windows}
  //Lista de Fontes no Windows
  CBFont.Items.Assign(Screen.Fonts);
  //Adiciona Suporte a Objetos OLE
  CreateOLEObjectInterface;
{$ENDIF}

{$IFDEF Linux}
  //Lista de fontes no Linux
  FontList := TStringList.Create;
  GetFontList(FontList);
  CBFont.Items.Assign(FontList);
  FontList.Free;
{$ENDIF}
CBFont.Items.Add('Sans');

I2 := 1;
  I3 := 7;
  for I := 0 to 15 do
  begin
    if (I = 5) then
      I2 := 2;
    if (I = 13) then
      I2 := 8;
    if (I = 14) then
      I2 := 12;
    if (I = 15) then
      I2 := 24;

    I3 := I3 + I2;
    CBSize.Items.Add(IntToStr(I3));
  end;

  lzRichEdit1.SetFocus;
  GetTextStatus;
end;

procedure TForm1.lzRichEdit1Change(Sender: TObject);
begin
  {$IFDEF Linux}
  GetTextStatus;
  {$ENDIF}
end;

procedure TForm1.lzRichEdit1KeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  GetTextStatus;
end;

procedure TForm1.MenuItem11Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.MenuItem19Click(Sender: TObject);
begin
  lzRichEdit1.SelectAll;
end;

procedure TForm1.MenuItem22Click(Sender: TObject);
var
  P: TPicture;
begin
  //-- Inserir Imagem
  Odlg.Title := 'Abrir...';
  {$IFDEF WINDOWS}
  Odlg.Filter :=
    'Imagens (*.bmp;*.xpm;*.png;*.ico;*.jpg;*.jpeg)|*.bmp;*.xpm;*.png;*.ico;*.jpg;*.jpeg';
  {$ENDIF}
  {$IFDEF Linux}
  Odlg.Filter :=
    'Imagens (*.bmp;*.xpm;*.png)|*.bmp;*.xpm;*.png';
  {$ENDIF}

  Odlg.Options := [ofEnableSizing, ofViewDetail, ofHideReadOnly];

  if Odlg.Execute then
  begin
    P := TPicture.Create;
    P.LoadFromFile(Odlg.FileName);
      {$IFDEF Windows}
      P.Bitmap.SaveToClipboardFormat(2);
      lzRichEdit1.PasteFromClipboard;
      {$ENDIF}
      {$IFDEF Linux}
        InsertImage(lzRichEdit1, P, lzRichEdit1.SelStart);
      {$ENDIF}
    P.Free;
  end;

end;

procedure TForm1.MenuItem24Click(Sender: TObject);
begin
  FDlg.Title := 'Fonte';
  //--
  FDlg.Font.Name := lzRichEdit1.SelAttributes.Name;
  FDlg.Font.Size := lzRichEdit1.SelAttributes.Size;
  FDlg.Font.Color := lzRichEdit1.SelAttributes.Color;
  FDlg.Font.Style := lzRichEdit1.SelAttributes.Style;

  if FDlg.Execute then
  begin
    lzRichEdit1.SelAttributes.Name:= FDlg.Font.Name;
    lzRichEdit1.SelAttributes.Size:= FDlg.Font.Size;
    lzRichEdit1.SelAttributes.Color:= FDlg.Font.Color;
    lzRichEdit1.SelAttributes.Style:= FDlg.Font.Style;
  end;
end;

procedure TForm1.MenuItem26Click(Sender: TObject);
begin
  frmParagrafo.Execute(lzRichEdit1);
end;

procedure TForm1.MenuItem27Click(Sender: TObject);
begin
  frmSobre.Show;
end;

procedure TForm1.SetFileName(S: string);
begin
  FFileName := S;
end;

procedure TForm1.GetTextStatus;
begin
  //--
  CBFont.Caption := lzRichEdit1.SelAttributes.Name;
  CBSize.Text := IntToStr(lzRichEdit1.SelAttributes.Size);
  FSetColor := False;
  ColorButton1.ButtonColor := TColor(lzRichEdit1.SelAttributes.Color);
  FSetColor := True;
  ToolButton1.Down := (fsBold in lzRichEdit1.SelAttributes.Style);
  ToolButton2.Down := (fsItalic in lzRichEdit1.SelAttributes.Style);
  ToolButton3.Down := (fsUnderline in lzRichEdit1.SelAttributes.Style);
  //--

  ToolButton6.Down := (taLeftJustify = lzRichEdit1.Paragraph.Alignment);
  ToolButton8.Down := (taCenter = lzRichEdit1.Paragraph.Alignment);
  ToolButton9.Down := (taRightJustify = lzRichEdit1.Paragraph.Alignment);
  //--
  ToolButton11.Down := (lzRichEdit1.Paragraph.Numbering=nsBullets);
  MenuItem25.Checked :=(lzRichEdit1.Paragraph.Numbering=nsBullets);

end;

end.

