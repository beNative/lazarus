unit Gopage;

interface

uses 
  {$IFNDEF LCL} WinTypes, WinProcs, {$ELSE} LclIntf, LResources, {$ENDIF} 
  Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, ExtCtrls, Spin;

type
  TGoPageForm = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    Bevel1: TBevel;
    PageNum: TSpinEdit;
    procedure PageNumEnter(Sender: TObject);
    procedure PageNumKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  GoPageForm: TGoPageForm;

implementation

{$IFNDEF LCL}
{$R *.DFM}
{$ENDIF}

procedure TGoPageForm.PageNumEnter(Sender: TObject);
begin
PageNum.SelectAll;
end;

procedure TGoPageForm.PageNumKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
if Key = 13 then
  Begin
  Key := 0;
  OKBtn.Click;
  end;
end;

initialization
{$IFDEF LCL}
{$I Gopage.lrs}  {Include form's resource file}
{$ENDIF}

end.
