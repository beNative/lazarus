unit uFormTest;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Memo;

type
  TFormTest = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTest: TFormTest;

implementation

uses OXmlUnitTests;

{$R *.fmx}

procedure TFormTest.FormCreate(Sender: TObject);
var
  xTest: TOXmlUnitTest;
begin
  ReportMemoryLeaksOnShutdown := True;

  Memo1.Lines.BeginUpdate;
  xTest := TOXmlUnitTest.Create;
  try
    xTest.OXmlTestAll(Memo1.Lines);
  finally
    xTest.Free;
    Memo1.Lines.EndUpdate;
  end;
end;

end.
