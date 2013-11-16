unit USobre;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  RichBox, LResources;

type

  { TfrmSobre }

  TfrmSobre = class(TForm)
    Button1: TButton;
    lzRichEdit1: TlzRichEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmSobre: TfrmSobre;

implementation

{$R *.lfm}

{ TfrmSobre }

procedure TfrmSobre.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmSobre.FormCreate(Sender: TObject);
var
  S:TMemoryStream;
  RTF:String='';
  P:PString;
begin
  S:= TMemoryStream.Create;
  S.Write(LazarusResources.Find('a').Value[1], Length(LazarusResources.Find('a').Value));
  lzRichEdit1.LoadFromStream(S);
  S.Free;
end;

procedure TfrmSobre.FormShow(Sender: TObject);
begin
end;
initialization
{$I a.res}
end.

