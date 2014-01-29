program FrameDem;
{A program to demonstrate the TFrameViewer component}

uses
{$IFDEF LCL}
  Interfaces,
{$ENDIF}
  Forms,
  FDemUnit in 'FDEMUNIT.PAS' {Form1},
  Fontdlg in 'FONTDLG.PAS' {FontForm},
  Submit in 'SUBMIT.PAS' {SubmitForm},
  HTMLAbt in 'HTMLABT.PAS' {AboutBox},
{$IFNDEF LCL}
  PrintStatusForm in 'PRINTSTATUSFORM.PAS' {PrnStatusForm},
  Gopage in 'GOPAGE.PAS' {GoPageForm},
  PreviewForm in 'PREVIEWFORM.PAS' {PreviewForm},
{$ENDIF}
  ImgForm in 'IMGFORM.PAS' {ImageForm};

{$IFDEF MSWINDOWS}
{$R *.res}
{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TSubmitForm, SubmitForm);
  Application.Run;
end.
