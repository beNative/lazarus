unit UParagrafo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  RichBox;

type

  { TfrmParagrafo }

  TfrmParagrafo = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
    RichControl:TWinControl;
    SStart, SLen: Integer;
  public
    { public declarations }
    procedure Execute(aRichControl: TWinControl);
  end; 

var
  frmParagrafo: TfrmParagrafo;

implementation

{$R *.lfm}

{ TfrmParagrafo }

procedure TfrmParagrafo.Edit1KeyPress(Sender: TObject; var Key: char);
begin
  if not (key in ['0'..'9',#8]) then
  key := #0;
end;

procedure TfrmParagrafo.Button1Click(Sender: TObject);
begin
  if Edit1.Text = '' then Edit1.Text:='0';
  if Edit2.Text = '' then Edit2.Text:='0';
  if Edit3.Text = '' then Edit3.Text:='0';

  TRichBox(RichControl).SelStart:= SStart;
  TRichBox(RichControl).SelLength:= SLen;

  if ComboBox1.Text = '' then ComboBox1.ItemIndex:= 2;
  //--
  {$IFDEF Windows}
  TRichBox(RichControl).Paragraph.LeftIndent:= StrToInt(Edit1.Text) +
                                               StrToInt(Edit3.Text);
  TRichBox(RichControl).Paragraph.FirstIndent:= StrToInt(Edit3.Text) * -1;
  {$ENDIF}

  {$IFDEF Linux}
  TRichBox(RichControl).Paragraph.LeftIndent:= StrToInt(Edit1.Text);
  TRichBox(RichControl).Paragraph.FirstIndent:= StrToInt(Edit3.Text);
  {$ENDIF}

  TRichBox(RichControl).Paragraph.RightIndent:= StrToInt(Edit2.Text);

  //--
  case ComboBox1.ItemIndex of
    2: TRichBox(RichControl).Paragraph.Alignment := taLeftJustify;
    1: TRichBox(RichControl).Paragraph.Alignment:= taRightJustify;
    0: TRichBox(RichControl).Paragraph.Alignment:= taCenter;
  end;
  Close;
end;

procedure TfrmParagrafo.Button2Click(Sender: TObject);
begin
  Close;
end;


procedure TfrmParagrafo.Execute(aRichControl: TWinControl);
begin
  RichControl:= aRichControl;
  Show;
  SStart:= TRichBox(RichControl).SelStart;
  Slen:= TRichBox(RichControl).SelLength;
  {$IFDEF WINDOWS}
  Edit1.Text:= IntToStr(TRichBox(RichControl).Paragraph.LeftIndent -
               (TRichBox(RichControl).Paragraph.FirstIndent * -1));
  Edit3.Text:= IntToStr(TRichBox(RichControl).Paragraph.FirstIndent * -1);
  {$ENDIF}

  {$IFDEF Linux}
  Edit1.Text:= IntToStr(TRichBox(RichControl).Paragraph.LeftIndent);
  Edit3.Text:= IntToStr(TRichBox(RichControl).Paragraph.FirstIndent);
  {$ENDIF}

  Edit2.Text:= IntToStr(TRichBox(RichControl).Paragraph.RightIndent);



  case TRichBox(RichControl).Paragraph.Alignment of
    taLeftJustify: ComboBox1.ItemIndex:= 2;
    taCenter: ComboBox1.ItemIndex:= 0;
    taRightJustify: ComboBox1.ItemIndex:= 1;
  end;
  TRichBox(RichControl).Refresh;
  TRichBox(RichControl).SelStart:= SStart;
  TRichBox(RichControl).SelLength:= SLen;
end;

end.
