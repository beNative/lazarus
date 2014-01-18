unit UExceptionLogger;

{$H+}

interface

uses
  {$ifdef windows}Windows,{$endif}
  Classes, SysUtils, UStackTrace, CustomLineInfo, Forms;

type
  TThreadSynchronizeEvent = procedure (AObject: TObject; Method: TThreadMethod) of object;

  { TExceptionLogger }

  TExceptionLogger = class(TComponent)
  private
    FMaxCallStackDepth: Integer;
    FLogFileName: string;
    FOnThreadSynchronize: TThreadSynchronizeEvent;
    procedure ThreadSynchronize(AObject: TObject; Method: TThreadMethod);
    function GetAppVersion: string;
    procedure SetMaxCallStackDepth(const AValue: Integer);
    procedure MakeReport;
    procedure ShowForm;
  public
    StackTrace: TStackTrace;
    LastException: Exception;
    ExceptionSender: TObject;
    IgnoreList: TStringList;
    procedure LoadDetails;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExceptionHandler(Sender: TObject; E: Exception);
    procedure CreateTextReport(Output: TStringList);
    procedure LogToFile(Report: TStringList);
    procedure LogStackTraceToFile(StackTrace: TStackTrace);
    procedure ShowReportForm;
  published
    property LogFileName: string read FLogFileName write FLogFileName;
    property MaxCallStackDepth: Integer read FMaxCallStackDepth write SetMaxCallStackDepth;
    property OnThreadSynchronize: TThreadSynchronizeEvent read FOnThreadSynchronize
      write FOnThreadSynchronize;
  end;

procedure Register;

resourcestring
  SExceptionClass = 'Class';
  SMessage = 'Message';
  SApplication = 'Application';
  STime = 'Time';
  SProcessID = 'Process ID';
  SThreadID = 'Thread ID';
  SVersion = 'Version';
  SCallStack = 'Call stack';
  SGeneral = 'General';
  SErrorOccured = 'Error occured during program execution:';
  STerminate = 'Exit program';
  SClose = 'Continue';
  SDetails = 'Details';
  SIgnoreNextTime = 'Next time ignore this exception';
  SExceptionInfo = 'Exception info';
  SIndex = 'Index';
  SAddress = 'Address';
  SLine = 'Line';
  SClass = 'Class';
  SProcedureMethod = 'Procedure/method';
  SUnit = 'Unit';
  SExceptionHandlerCannotBeSynchronized = 'Exception handler cannot be synchronized with main thread.';

implementation

uses
  UExceptionForm;

procedure Register;
begin
  RegisterComponents('Samples', [TExceptionLogger]);
end;

{ TExceptionLogger }

constructor TExceptionLogger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IgnoreList := TStringList.Create;
  StackTrace := TStackTrace.Create;
  MaxCallStackDepth := 20;
  Application.OnException := ExceptionHandler;
  Application.Flags := Application.Flags - [AppNoExceptionMessages];
  OnThreadSynchronize := ThreadSynchronize;
end;

destructor TExceptionLogger.Destroy;
begin
  StackTrace.Free;
  IgnoreList.Free;
  inherited Destroy;
end;

procedure TExceptionLogger.CreateTextReport(Output: TStringList);
begin
  with Output do begin
    Clear;
    Add(SExceptionClass + ': ' + LastException.ClassName);
    Add(SMessage + ': ' + LastException.Message);
    Add(SApplication + ': ' + Application.Title);
    Add(SVersion + ': ' + GetAppVersion);
    Add(STime + ': ' + DateTimeToStr(Now));
    Add(SProcessID + ': ' + IntToStr(GetProcessID));
    Add(SThreadID + ': ' + IntToStr(GetThreadID));
  end;
end;

procedure TExceptionLogger.LogToFile(Report: TStringList);
var
  LogFile: TFileStream;
  Buffer: string;
begin
  Buffer := Report.Text;
  if FileExists(FLogFileName) then
    LogFile := TFileStream.Create(UTF8Decode(FLogFileName), fmOpenReadWrite)
    else LogFile := TFileStream.Create(UTF8Decode(FLogFileName), fmCreate);
  with LogFile do try
    Seek(0, soFromEnd);
    if Length(Buffer) > 0 then
      Write(Buffer[1], Length(Buffer));
  finally
    LogFile.Free;
  end;
end;

procedure TExceptionLogger.LogStackTraceToFile(StackTrace: TStackTrace);
var
  I: Integer;
  LogFile: TFileStream;
  Line: string;
begin
  if FileExists(FLogFileName) then
    LogFile := TFileStream.Create(UTF8Decode(FLogFileName), fmOpenReadWrite)
    else LogFile := TFileStream.Create(UTF8Decode(FLogFileName), fmCreate);
  with LogFile do try
    Seek(0, soFromEnd);
    for I := 0 to StackTrace.Count - 1 do
    with TStackFrameInfo(StackTrace[I]) do begin
      Line := IntToStr(Index) + ': ' + IntToHex(Address, 8) + ' in ' + FunctionName + ' ' +
        Source + '(' + IntToStr(LineNumber) + ')' + LineEnding;
      if Length(Line) > 0 then
        Write(Line[1], Length(Line));
    end;
    Line := LineEnding;
    Write(Line[1], Length(Line));
  finally
    LogFile.Free;
  end;
end;

procedure TExceptionLogger.ShowReportForm;
begin
  ExceptionForm.LoadStackTraceToListView(StackTrace);
  if not ExceptionForm.Visible then ExceptionForm.ShowModal;
end;

procedure TExceptionLogger.ExceptionHandler(Sender: TObject; E: Exception);
begin
  BackTraceStrFunc := @StabBackTraceStr;
  StackTrace.GetExceptionBackTrace;
  LastException := E;
  ExceptionSender := Sender;
  if (MainThreadID <> ThreadID) then begin
    if Assigned(FOnThreadSynchronize) then
      FOnThreadSynchronize(Sender, ShowForm)
      else raise Exception.Create(SExceptionHandlerCannotBeSynchronized);
  end else ShowForm;
end;

procedure TExceptionLogger.MakeReport;
var
  Report: TStringList;
begin
  StackTrace.GetInfo;
  if IgnoreList.IndexOf(LastException.ClassName) = -1 then begin
    Report := TStringList.Create;
    try
      CreateTextReport(Report);
      if FLogFileName <> '' then begin
        LogToFile(Report);
        LogStackTraceToFile(StackTrace);
      end;
      ExceptionForm.MemoExceptionInfo.Lines.Assign(Report);
      ShowReportForm;
    finally
      Report.Free;
    end;
    if ExceptionForm.CheckBoxIgnore.Checked then
      IgnoreList.Add(LastException.ClassName);
  end;
end;

procedure TExceptionLogger.ShowForm;
begin
  ExceptionForm.Logger := Self;
  ExceptionForm.LabelMessage.Caption := LastException.Message;
  ExceptionForm.MemoExceptionInfo.Clear;
  if not ExceptionForm.Visible then ExceptionForm.ShowModal;
end;

procedure TExceptionLogger.LoadDetails;
begin
  if ExceptionSender is TThread then
    TThread.Synchronize(TThread(ExceptionSender), MakeReport)
    else MakeReport;
end;

procedure TExceptionLogger.SetMaxCallStackDepth(const AValue: Integer);
begin
  FMaxCallStackDepth := AValue;
  StackTrace.MaxDepth := AValue;
end;

procedure TExceptionLogger.ThreadSynchronize(AObject: TObject;
  Method: TThreadMethod);
begin
  if AObject is TThread then TThread.Synchronize(TThread(AObject), Method)
    else raise Exception.Create(SExceptionHandlerCannotBeSynchronized);
end;

function TExceptionLogger.GetAppVersion: string;
var
  Size, Size2: DWord;
  Pt, Pt2: Pointer;
begin
  {$ifdef windows}
  Size := GetFileVersionInfoSize(PChar(ParamStr(0)), Size2);
  if Size > 0 then
  begin
    GetMem(Pt, Size);
    try
       GetFileVersionInfo(PChar (ParamStr(0)), 0, Size, Pt);
       VerQueryValue(Pt, '\', Pt2, Size2);
       with TVSFixedFileInfo(Pt2^) do
       begin
         Result := IntToStr(HiWord(dwFileVersionMS)) + '.' +
           IntToStr(LoWord(dwFileVersionMS)) + '.' +
           IntToStr(HiWord(dwFileVersionLS)) + '.' +
           IntToStr(LoWord(dwFileVersionLS));
      end;
    finally
      FreeMem(Pt);
    end;
  end;
  {$else}
  Result := '';
  {$endif}
end;

end.

