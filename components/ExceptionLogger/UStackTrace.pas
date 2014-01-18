unit UStackTrace;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, CustomLineInfo;

type
  TStackFrameInfo = class
    Index: Integer;
    LineNumber: Integer;
    Address: Integer;
    FunctionClassName: string;
    FunctionName: string;
    Source: string;
    procedure GetFrameInfo(Addr: Pointer);
  end;

  { TStackTrace }

  TStackTrace = class(TObjectList)
    Frames: array of Pointer;
    MaxDepth: Integer;
    procedure GetExceptionBackTrace;
    procedure GetCallStack(BP: Pointer);
    procedure GetCurrentCallStack;
    procedure GetInfo;
    constructor Create;
  end;


implementation

procedure TStackFrameInfo.GetFrameInfo(Addr: Pointer);
var
  Func: shortstring;
  SourceStr: shortstring;
  Line: LongInt;
  Store: TBackTraceStrFunc;
  Success: Boolean;
begin
  // Reset to prevent infinite recursion if problems inside the code PM
  Store := BackTraceStrFunc;
  BackTraceStrFunc := @SysBackTraceStr;
  Success := GetLineInfo(ptruint(Addr), Func, SourceStr, Line);
  Address := Integer(Addr);
  FunctionName := Func;
  if Pos('__', FunctionName) > 0 then begin
    FunctionClassName := Copy(FunctionName, 1, Pos('__', FunctionName) - 1);
    Delete(FunctionName, 1, Length(FunctionClassName) + 2);
  end else FunctionClassName := '';
  LineNumber := Line;
  Source := SourceStr;
  BackTraceStrFunc := Store;
end;

procedure TStackTrace.GetCallStack(BP: Pointer);
var
  I: Longint;
  prevbp: Pointer;
  CallerFrame: Pointer;
  CallerAddress: Pointer;
  StackFrameInfo: TStackFrameInfo;
begin
  Clear;
  try
    I := 0;
    SetLength(Frames, 0);
    while (BP <> nil) and (I < MaxDepth) do begin
      SetLength(Frames, Length(Frames) + 1);
      Frames[I] := TStackFrameInfo(get_caller_addr(BP));
      Inc(I);
      BP := TStackFrameInfo(get_caller_frame(BP));
    end;
  except
    { prevent endless dump if an exception occured }
  end;
end;

constructor TStackTrace.Create;
begin
  inherited;
  MaxDepth := 20;
end;

procedure TStackTrace.GetExceptionBackTrace;
var
  FrameCount: Integer;
  FramesList: PPointer;
  FrameNumber: Integer;
begin
  SetLength(Frames, 1);
  Frames[0] := ExceptAddr;
  FrameCount := ExceptFrameCount;
  FramesList := ExceptFrames;
  if FrameCount > MaxDepth then FrameCount := MaxDepth;
  SetLength(Frames, FrameCount + 1);
  for FrameNumber := 0 to FrameCount - 1 do begin
    Frames[FrameNumber + 1] := FramesList[FrameNumber]
  end;
end;

procedure TStackTrace.GetCurrentCallStack;
begin
  GetCallStack(get_frame);
end;

procedure TStackTrace.GetInfo;
var
  I: Integer;
  StackFrameInfo: TStackFrameInfo;
begin
  Clear;
  for I := 0 to High(Frames) do begin
    StackFrameInfo := TStackFrameInfo.Create;
    StackFrameInfo.GetFrameInfo(Frames[I]);
    StackFrameInfo.Index := I + 1;
    Add(StackFrameInfo);
  end;
end;

end.

