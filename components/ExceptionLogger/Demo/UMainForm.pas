unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UExceptionLogger;

type

  { TSampleThread }

  TSampleThread = class(TThread)
    procedure Execute; override;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ExceptionLogger1: TExceptionLogger;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { private declarations }
  public
    Thread: TSampleThread;
  end; 

var
  MainForm: TMainForm;

implementation

{ TSampleThread }

procedure TSampleThread.Execute;
begin
  try
    raise Exception.Create('Exception inside thread');

  except
    on E: Exception do
      MainForm.ExceptionLogger1.ExceptionHandler(Self, E);
  end;
end;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.Button1Click(Sender: TObject);
begin
  raise Exception.Create('Simple exception');
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  Thread := TSampleThread.Create(True);
  Thread.FreeOnTerminate := True;
  Thread.Start;
end;

end.

