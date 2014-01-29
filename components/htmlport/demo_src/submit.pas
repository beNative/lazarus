unit Submit;

interface

uses
  {$IFNDEF LCL} Windows, Messages, {$ELSE} LclIntf, LMessages, LclType, LResources, {$ENDIF}
  SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls;

type
  TSubmitForm = class(TForm)
    ActionText: TEdit;
    MethodText: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    ResultBox: TListBox;
    Label3: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SubmitForm: TSubmitForm;

implementation

{$IFNDEF LCL}
{$R *.DFM}
{$ENDIF}

procedure TSubmitForm.Button1Click(Sender: TObject);
begin
Close;
end;

initialization
{$IFDEF LCL}
{$I SUBMIT.lrs}  {Include form's resource file}
{$ENDIF}

end.
