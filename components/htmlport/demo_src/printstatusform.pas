unit PrintStatusForm;

interface

uses
  {$IFNDEF LCL} Windows, Messages, {$ELSE} LclIntf, LMessages, LclType, LResources, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, HTMLView {$IFNDEF LCL}, MetaFilePrinter {$ENDIF};

type
  TPrnStatusForm = class(TForm)
    StatusLabel: TLabel;
    CancelButton: TBitBtn;
    procedure CancelButtonClick(Sender: TObject);
  private
    { Private declarations }
    Viewer: ThtmlViewer;
    Canceled: boolean;
{$IFNDEF LCL}
    MFPrinter: TMetaFilePrinter;
{$ENDIF}
    FromPage, ToPage: integer;
    procedure PageEvent(Sender: TObject; PageNum: integer; var Stop: boolean);
  public
    { Public declarations }
{$IFNDEF LCL}
  procedure DoPreview(AViewer: ThtmlViewer; AMFPrinter: TMetaFilePrinter;
              var Abort: boolean);
{$ENDIF}
  procedure DoPrint(AViewer: ThtmlViewer; FromPg, ToPg: integer;
              var Abort: boolean);
  end;

var
  PrnStatusForm: TPrnStatusForm;

implementation

{$IFNDEF LCL}
{$R *.DFM}
{$ENDIF}


{$IFNDEF LCL}
procedure TPrnStatusForm.DoPreview(AViewer: ThtmlViewer; AMFPrinter: TMetaFilePrinter;
              var Abort: boolean);
begin
Viewer := AViewer;
MFPrinter := AMFPrinter;
Viewer.OnPageEvent := PageEvent;
try                                   
  Show;
  Viewer.PrintPreview(MFPrinter);
  Hide;
  Abort := Canceled;
finally
  Viewer.OnPageEvent := Nil;      
  end;
end;
{$ENDIF}

procedure TPrnStatusForm.DoPrint(AViewer: ThtmlViewer; FromPg, ToPg: integer;
              var Abort: boolean);
begin
Viewer := AViewer;
FromPage := FromPg;
ToPage := ToPg;
Viewer.OnPageEvent := PageEvent;
try                                   
  Show;
  Viewer.Print(FromPage, ToPage);          
  Hide;
  Abort := Canceled;
finally
  Viewer.OnPageEvent := Nil;      
  end;
end;

procedure TPrnStatusForm.PageEvent(Sender: TObject; PageNum: integer; var Stop: boolean);
begin   
if Canceled then
  Stop := True
else
  if PageNum = 0 then
    StatusLabel.Caption := 'Formating'
  else
    StatusLabel.Caption := 'Page Number '+ IntToStr(PageNum);
Update;
end;

procedure TPrnStatusForm.CancelButtonClick(Sender: TObject);
begin
Canceled := True;
end;

initialization
{$IFDEF LCL}
{$I PrintStatusForm.lrs}  {Include form's resource file}
{$ENDIF}

end.
