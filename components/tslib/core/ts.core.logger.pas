{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit ts.Core.Logger;

{
  The Original Code is part of the LuiPack library for Freepascal/Lazarus.
  The Initial Developer of the Original Code (multilog.pas) is Luiz Américo
  Pereira Câmara (pascalive@bol.com.br). Portions created by the Initial
  Developer are Copyright (C) 2006. All Rights Reserved. You may obtain a copy
  of the original code at http://code.google.com/p/luipack/
}

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Graphics,

  ts.Core.Logger.Interfaces;

type
  TLogger = class;

  TDebugClass = 0..31;

  TDebugClasses = set of TDebugClass;

  TLogMessage = packed record
    MsgType   : Integer;
    TimeStamp : TDateTime;
    Text      : UTF8String;
    Data      : TStream;
  end;

  { TLogChannel }

  TLogChannel = class
  private
    FActive: Boolean;
  public
    procedure Clear; virtual; abstract;
    procedure Deliver(const AMsg: TLogMessage); virtual; abstract;
    procedure Init; virtual;
    property Active: Boolean read FActive write FActive;
  end;

  { TChannelList }

  TChannelList = class
  private
    FList: TFpList;
    function GetCount: Integer; inline;
    function GetItems(AIndex:Integer): TLogChannel; inline;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AChannel: TLogChannel):Integer;
    procedure Remove(AChannel:TLogChannel);
    property Count: Integer read GetCount;
    property Items[AIndex:Integer]: TLogChannel read GetItems; default;
  end;

  { TLogger }

  TLogger = class(TInterfacedObject, ILogger)
  type
    TTrack = class(TInterfacedObject)
    private
      FLogger : ILogger;
      FName   : string;
      FSender : TObject;
    public
      constructor Create(
        const ALogger: ILogger;
        ASender      : TObject;
        const AName  : string
        );
      destructor Destroy; override;
    end;
  private
    FMaxStackCount : Integer;
    FChannels      : TChannelList;
    FLogStack      : TStrings;
    FCheckList     : TStringList;
    FCounterList   : TStringList;

    procedure GetCallStack(AStream:TStream);
    procedure SetMaxStackCount(const AValue: Integer);

  protected
    procedure Send(Classes: TDebugClasses; const AText: string; AValue: Int64);overload;
    procedure Send(Classes: TDebugClasses; const AText: string; AValue: QWord);overload;
    procedure Send(Classes: TDebugClasses; const AText: string; AValue: Boolean);overload;
    procedure Send(Classes: TDebugClasses; const AText: string; const ARect: TRect);overload;
    procedure Send(Classes: TDebugClasses; const AText: string; const APoint: TPoint);overload;
    procedure Send(Classes: TDebugClasses; const AText: string; AStrList: TStrings);overload;
    procedure Send(Classes: TDebugClasses; const AText: string; AValue: Double);overload;
    procedure Send(Classes: TDebugClasses; const AText: string; AValue: Cardinal);overload;
    procedure Send(Classes: TDebugClasses; const AText: string; AValue: Integer);overload;
    procedure Send(Classes: TDebugClasses; const AText,AValue: string); overload;
    procedure Send(Classes: TDebugClasses; const AText: string; Args: array of const);overload;
    procedure Send(Classes: TDebugClasses; const AText: string);overload;
    procedure Send(Classes: TDebugClasses; const AText: string; AObject: TObject);overload;

    procedure SendColor(Classes: TDebugClasses; const AText: string; AColor: TColor);
    procedure SendBitmap(Classes: TDebugClasses; const AText: string; ABitmap: TBitmap);
    procedure SendPointer(Classes: TDebugClasses; const AText: string; APointer: Pointer);overload;
    procedure SendCallStack(Classes: TDebugClasses; const AText: string);overload;
    procedure SendException(Classes: TDebugClasses; const AText: string; AException: Exception);overload;
    procedure SendHeapInfo(Classes: TDebugClasses; const AText: string);overload;
    procedure SendIf(Classes: TDebugClasses; const AText: string; Expression, IsTrue: Boolean);overload;
    procedure SendMemory(Classes: TDebugClasses; const AText: string; Address: Pointer; Size: LongWord);overload;
    procedure SendIf(Classes: TDebugClasses; const AText: string; Expression: Boolean); overload;

    procedure IncCounter(Classes: TDebugClasses; const CounterName: string);overload;

    procedure AddCheckPoint(Classes: TDebugClasses);overload; inline;
    procedure AddCheckPoint(Classes: TDebugClasses; const CheckName: string);overload;
    procedure DecCounter(Classes: TDebugClasses; const CounterName: string);overload;
    procedure ResetCounter(Classes: TDebugClasses; const CounterName: string);overload;
    procedure ResetCheckPoint(Classes: TDebugClasses);overload;
    procedure ResetCheckPoint(Classes: TDebugClasses;const CheckName: string);overload;
    procedure EnterMethod(Classes: TDebugClasses; const AMethodName: string); overload;
    procedure EnterMethod(Classes: TDebugClasses; Sender: TObject; const AMethodName: string);overload;
    procedure ExitMethod(Classes: TDebugClasses; const AMethodName: string); overload; inline;
    procedure ExitMethod(Classes: TDebugClasses; Sender: TObject; const AMethodName: string);overload;
    procedure Watch(Classes: TDebugClasses; const AText,AValue: string);overload;
    procedure Watch(Classes: TDebugClasses; const AText: string; AValue: Integer);overload;
    procedure Watch(Classes: TDebugClasses; const AText: string; AValue: Double);overload;
    procedure Watch(Classes: TDebugClasses; const AText: string; AValue: Boolean);overload;
    procedure Watch(Classes: TDebugClasses; const AText: string; AValue: Cardinal);overload;

    procedure SendStream(AMsgType: TLogMessageType;const AText:string; AStream: TStream);
    procedure SendBuffer(AMsgType: TLogMessageType;const AText:string;
      var Buffer; Count: LongWord);
  public
    ActiveClasses: TDebugClasses;//Made a public field to allow use of include/exclude functions
    DefaultClasses: TDebugClasses;
    constructor Create;
    destructor Destroy; override;
    function CalledBy(const AMethodName: string): Boolean;
    procedure Clear;
    //Helper functions
    function RectToStr(const ARect: TRect): string; //inline
    function PointToStr(const APoint: TPoint): string; //inline

    //Send functions
    procedure Send(const AText: string); overload;
    procedure Send(const AText: string; AArgs: array of const); overload;
    procedure Send(const AText: string; AValue: Integer); overload;
    procedure Send(const AText: string; AValue: Cardinal); overload;
    procedure Send(const AText: string; AValue: Double); overload;
    procedure Send(const AText: string; AValue: Int64); overload;
    procedure Send(const AText: string; AValue: QWord); overload;
    procedure Send(const AText: string; AValue: Boolean); overload;
    procedure Send(const AText: string; AStrings: TStrings); overload;
    procedure Send(const AText: string; AObject: TObject); overload;
    procedure Send(const AText: string; const AValue: string); overload;
    procedure Send(const AText: string; const ARect: TRect); overload;
    procedure Send(const AText: string; const APoint: TPoint); overload;
    procedure Send(const AText: string; const AInterface: IInterface); overload;

    procedure SendBitmap(const AText: string; ABitmap: TBitmap); //inline;
    procedure SendColor(const AText: string; AColor: TColor); //inline;
    procedure SendComponent(AComponent: TComponent); inline;
    procedure SendPointer(const AText: string; APointer: Pointer); overload; inline;
    procedure SendCallStack(const AText: string); overload; inline;
    procedure SendException(const AText: string; AException: Exception);overload; inline;
    procedure SendHeapInfo(const AText: string); overload; inline;
    procedure SendMemory(const AText: string; AAddress: Pointer; ASize: LongWord); overload; inline;
    procedure SendIf(const AText: string; AExpression: Boolean); overload; inline;
    procedure SendIf(const AText: string; AExpression, AIsTrue: Boolean); overload; inline;

    procedure Info(const AText: string); overload;
    procedure Info(const AText: string; AArgs: array of const); overload;
    procedure Warn(const AText: string); overload;
    procedure Warn(const AText: string; AArgs: array of const); overload;
    procedure Error(const AText: string); overload;
    procedure Error(const AText: string; AArgs: array of const); overload;

    procedure IncCounter(const AName: string); overload; inline;
    procedure DecCounter(const AName: string); overload; inline;
    procedure ResetCounter(const AName: string);overload; inline;
    function GetCounter(const AName: string): Integer;

    procedure AddCheckPoint; overload; inline;
    procedure AddCheckPoint(const AName: string);overload; inline;
    procedure ResetCheckPoint; overload; inline;
    procedure ResetCheckPoint(const AName: string); overload; inline;

    procedure Enter(const AName: string); overload; inline;
    procedure Enter(ASender: TObject; const AName: string); overload; inline;
    procedure Leave(const AName: string); overload; inline;
    procedure Leave(ASender: TObject; const AName: string); overload; inline;
    function Track(const AName: string): IInterface; overload;
    function Track(ASender: TObject; const AName: string): IInterface; overload;

    procedure Watch(const AText, AValue: string); overload; inline;
    procedure Watch(const AText: string; AValue: Integer); overload; inline;
    procedure Watch(const AText: string; AValue: Cardinal); overload; inline;
    procedure Watch(const AText: string; AValue: Double); overload; inline;
    procedure Watch(const AText: string; AValue: Boolean); overload; inline;

    property Channels: TChannelList
      read FChannels;

    property LogStack: TStrings
      read FLogStack;

    property MaxStackCount: Integer
      read FMaxStackCount write SetMaxStackCount;
  end;

implementation

uses
  IntfGraphics, GraphType, FPimage, FPWriteBMP;

const
  STACKCOUNTLIMIT        = 256;
  DEFAULT_CHECK_NAME = 'CheckPoint';

function ColorToStr(Color: TColor): string;
begin
  case Color of
    clBlack                   : Result := 'clBlack';
    clMaroon                  : Result := 'clMaroon';
    clGreen                   : Result := 'clGreen';
    clOlive                   : Result := 'clOlive';
    clNavy                    : Result := 'clNavy';
    clPurple                  : Result := 'clPurple';
    clTeal                    : Result := 'clTeal';
    clGray{clDkGray}          : Result := 'clGray/clDkGray';
    clSilver{clLtGray}        : Result := 'clSilver/clLtGray';
    clRed                     : Result := 'clRed';
    clLime                    : Result := 'clLime';
    clYellow                  : Result := 'clYellow';
    clBlue                    : Result := 'clBlue';
    clFuchsia                 : Result := 'clFuchsia';
    clAqua                    : Result := 'clAqua';
    clWhite                   : Result := 'clWhite';
    clCream                   : Result := 'clCream';
    clNone                    : Result := 'clNone';
    clDefault                 : Result := 'clDefault';
    clMoneyGreen              : Result := 'clMoneyGreen';
    clSkyBlue                 : Result := 'clSkyBlue';
    clMedGray                 : Result := 'clMedGray';
    clScrollBar               : Result := 'clScrollBar';
    clBackground              : Result := 'clBackground';
    clActiveCaption           : Result := 'clActiveCaption';
    clInactiveCaption         : Result := 'clInactiveCaption';
    clMenu                    : Result := 'clMenu';
    clWindow                  : Result := 'clWindow';
    clWindowFrame             : Result := 'clWindowFrame';
    clMenuText                : Result := 'clMenuText';
    clWindowText              : Result := 'clWindowText';
    clCaptionText             : Result := 'clCaptionText';
    clActiveBorder            : Result := 'clActiveBorder';
    clInactiveBorder          : Result := 'clInactiveBorder';
    clAppWorkspace            : Result := 'clAppWorkspace';
    clHighlight               : Result := 'clHighlight';
    clHighlightText           : Result := 'clHighlightText';
    clBtnFace                 : Result := 'clBtnFace';
    clBtnShadow               : Result := 'clBtnShadow';
    clGrayText                : Result := 'clGrayText';
    clBtnText                 : Result := 'clBtnText';
    clInactiveCaptionText     : Result := 'clInactiveCaptionText';
    clBtnHighlight            : Result := 'clBtnHighlight';
    cl3DDkShadow              : Result := 'cl3DDkShadow';
    cl3DLight                 : Result := 'cl3DLight';
    clInfoText                : Result := 'clInfoText';
    clInfoBk                  : Result := 'clInfoBk';
    clHotLight                : Result := 'clHotLight';
    clGradientActiveCaption   : Result := 'clGradientActiveCaption';
    clGradientInactiveCaption : Result := 'clGradientInactiveCaption';
    clForm                    : Result := 'clForm';
    {
    //todo find the conflicts
    clColorDesktop            : Result:='clColorDesktop';
    cl3DFace                  : Result:='cl3DFace';
    cl3DShadow                : Result:='cl3DShadow';
    cl3DHiLight               : Result:='cl3DHiLight';
    clBtnHiLight              : Result:='clBtnHiLight';
    }
  else
    Result := 'Unknown Color';
  end;//case
  Result := Result + ' ($' + IntToHex(Color, 6) + ')';
end;

procedure SaveBitmapToStream(Bitmap: TBitmap; Stream: TStream);
var
  IntfImg   : TLazIntfImage;
  ImgWriter : TFPCustomImageWriter;
  RawImage  : TRawImage;
begin
  // adapted from LCL code
  IntfImg   := nil;
  ImgWriter := nil;
  try
    IntfImg := TLazIntfImage.Create(0,0);
    IntfImg.LoadFromBitmap(Bitmap.Handle, Bitmap.MaskHandle);

    IntfImg.GetRawImage(RawImage);
    if RawImage.IsMasked(True) then
      ImgWriter := TLazWriterXPM.Create
    else
    begin
      ImgWriter := TFPWriterBMP.Create;
      TFPWriterBMP(ImgWriter).BitsPerPixel := IntfImg.DataDescription.Depth;
    end;

    IntfImg.SaveToStream(Stream, ImgWriter);
    Stream.Position := 0;
  finally
    IntfImg.Free;
    ImgWriter.Free;
  end;
end;

function FormatNumber(Value: Integer): string;
var
  TempStr : string;
  I       : Integer;
  Digits  : Integer;
begin
  Digits:=0;
  Result:='';
  TempStr:=IntToStr(Value);
  for I := length(TempStr) downto 1 do
  begin
    //todo: implement using mod() -> get rids of digits
    if Digits = 3 then
    begin
      Digits:=0;
      Result := FormatSettings.ThousandSeparator + Result;
    end;
    Result:=TempStr[I]+Result;
    Inc(Digits);
  end;
end;

{ TLogger.TTrack }

constructor TLogger.TTrack.Create(const ALogger: ILogger; ASender: TObject; const AName: string);
begin
  inherited Create;
  FLogger     := ALogger;
  FSender     := ASender;
  FName := AName;
  if not Assigned(FSender) then
    FLogger.Enter(FName)
  else
    FLogger.Enter(FSender, FName);
end;

destructor TLogger.TTrack.Destroy;
begin
  if not Assigned(FSender) then
    FLogger.Leave(FName)
  else
    FLogger.Leave(FSender, FName);
  inherited Destroy;
end;

{ TLogger }

procedure TLogger.GetCallStack(AStream: TStream);
var
  I            : Integer;
  prevbp       : Pointer;
  caller_frame : Pointer;
  caller_addr  : Pointer;
  bp           : Pointer;
  S            : string;
begin
  //routine adapted from fpc source
  //This trick skip SendCallstack item
  //bp:=get_frame;
  bp:= get_caller_frame(get_frame);
  try
    prevbp:=bp-1;
    I:=0;
    //is_dev:=do_isdevice(textrec(f).Handle);
    while bp > prevbp Do
     begin
       caller_addr := get_caller_addr(bp);
       caller_frame := get_caller_frame(bp);
       if (caller_addr=nil) then
         break;
       //todo: see what is faster concatenate string and use writebuffer or current
       S:=BackTraceStrFunc(caller_addr)+LineEnding;
       AStream.WriteBuffer(S[1],Length(S));
       Inc(I);
       if (I>=FMaxStackCount) or (caller_frame=nil) then
         break;
       prevbp:=bp;
       bp:=caller_frame;
     end;
   except
     { prevent endless dump if an exception occured }
   end;
end;

procedure TLogger.SendStream(AMsgType: TLogMessageType; const AText: string; AStream: TStream);
var
  MsgRec: TLogMessage;
  i:Integer;
begin
  with MsgRec do
  begin
    MsgType   := Integer(AMsgType);
    TimeStamp := Now;
    Text      := AText;
    Data      := AStream;
  end;
  for i:= 0 to Channels.Count - 1 do
    if Channels[i].Active then
      Channels[i].Deliver(MsgRec);
  AStream.Free;
end;

procedure TLogger.SendBuffer(AMsgType: TLogMessageType; const AText: string; var Buffer; Count: LongWord);
var
  AStream: TStream;
begin
  if Count > 0 then
  begin
    AStream:=TMemoryStream.Create;
    AStream.Write(Buffer,Count);
  end
  else
    AStream:=nil;
  //SendStream free AStream
  SendStream(AMsgType,AText,AStream);
end;

procedure TLogger.SetMaxStackCount(const AValue: Integer);
begin
  if AValue < 256 then
    FMaxStackCount := AValue
  else
    FMaxStackCount := 256;
end;

constructor TLogger.Create;
begin
  FChannels := TChannelList.Create;
  FMaxStackCount := 20;
  FLogStack := TStringList.Create;
  FCheckList := TStringList.Create;
  with FCheckList do
  begin
    CaseSensitive := False;
    Sorted := True; //Faster IndexOf?
  end;
  FCounterList := TStringList.Create;
  with FCounterList do
  begin
    CaseSensitive := False;
    Sorted := True; //Faster IndexOf?
  end;
  ActiveClasses := [0];
  DefaultClasses := [0];
end;

destructor TLogger.Destroy;
begin
  FChannels.Destroy;
  FLogStack.Destroy;
  FCheckList.Destroy;
  FCounterList.Destroy;
end;

function TLogger.CalledBy(const AMethodName: string): Boolean;
begin
  Result:=FLogStack.IndexOf(UpperCase(AMethodName)) <> -1;
end;

procedure TLogger.Clear;
var
  i: Integer;
begin
  for i:= 0 to Channels.Count - 1 do
    if Channels[i].Active then
      Channels[i].Clear;
end;

function TLogger.RectToStr(const ARect: TRect): string;
begin
  with ARect do
    Result:=Format('(Left: %d; Top: %d; Right: %d; Bottom: %d)',[Left,Top,Right,Bottom]);
end;

function TLogger.PointToStr(const APoint: TPoint): string;
begin
  with APoint do
    Result:=Format('(X: %d; Y: %d)',[X,Y]);
end;

procedure TLogger.Send(const AText: string);
begin
  Send(DefaultClasses,AText);
end;

procedure TLogger.Send(Classes: TDebugClasses; const AText: string);
begin
  if Classes * ActiveClasses = [] then Exit;
  SendStream(lmtInfo,AText,nil);
end;

procedure TLogger.Send(const AText: string; AArgs: array of const);
begin
  Send(DefaultClasses,AText,AArgs);
end;

procedure TLogger.Send(Classes: TDebugClasses; const AText: string;
  Args: array of const);
begin
  if Classes * ActiveClasses = [] then Exit;
  SendStream(lmtInfo, Format(AText,Args),nil);
end;

procedure TLogger.Send(const AText: string; const AValue: string);
begin
  Send(DefaultClasses,AText,AValue);
end;

procedure TLogger.Send(Classes: TDebugClasses; const AText, AValue: string);
begin
  if Classes * ActiveClasses = [] then Exit;
  SendStream(lmtValue,AText+' = '+AValue,nil);
end;

procedure TLogger.Send(const AText: string; AValue: Integer);
begin
  Send(DefaultClasses,AText,AValue);
end;

procedure TLogger.Send(Classes: TDebugClasses; const AText: string; AValue: Integer);
begin
  if Classes * ActiveClasses = [] then Exit;
  SendStream(lmtValue,AText+' = '+IntToStr(AValue),nil);
end;

procedure TLogger.Send(const AText: string; AValue: Cardinal);
begin
  Send(DefaultClasses,AText,AValue);
end;

procedure TLogger.Send(Classes: TDebugClasses; const AText: string;
  AValue: Cardinal);
begin
  if Classes * ActiveClasses = [] then Exit;
  SendStream(lmtValue,AText+' = '+IntToStr(AValue),nil);
end;

procedure TLogger.Send(const AText: string; AValue: Double);
begin
  Send(DefaultClasses,AText,AValue);
end;

procedure TLogger.Send(Classes: TDebugClasses; const AText: string; AValue: Double
  );
begin
  if Classes * ActiveClasses = [] then Exit;
  SendStream(lmtValue,AText+' = '+FloatToStr(AValue),nil);
end;

procedure TLogger.Send(const AText: string; AValue: Int64);
begin
  Send(DefaultClasses,AText,AValue);
end;

procedure TLogger.Send(Classes: TDebugClasses; const AText: string; AValue: Int64
  );
begin
  if Classes * ActiveClasses = [] then Exit;
  SendStream(lmtValue,AText+' = '+IntToStr(AValue),nil);
end;

procedure TLogger.Send(const AText: string; AValue: QWord);
begin
  Send(DefaultClasses,AText,AValue);
end;

procedure TLogger.Send(Classes: TDebugClasses; const AText: string; AValue: QWord
  );
begin
  if Classes * ActiveClasses = [] then Exit;
  SendStream(lmtValue,AText+' = '+IntToStr(AValue),nil);
end;

procedure TLogger.Send(const AText: string; AValue: Boolean);
begin
  Send(DefaultClasses, AText, AValue);
end;

procedure TLogger.Send(Classes: TDebugClasses; const AText: string; AValue: Boolean);
begin
  if Classes * ActiveClasses = [] then Exit;
  SendStream(lmtValue, AText + ' = ' + BoolToStr(AValue, True), nil);
end;

procedure TLogger.Send(const AText: string; const ARect: TRect);
begin
  Send(DefaultClasses,AText,ARect);
end;

procedure TLogger.Send(Classes: TDebugClasses; const AText: string;const ARect: TRect);
begin
  if Classes * ActiveClasses = [] then Exit;
  with ARect do
    SendStream(lmtValue,AText+ ' = '+RectToStr(ARect),nil);
end;

procedure TLogger.Send(const AText: string; const APoint: TPoint);
begin
  Send(DefaultClasses,AText,APoint);
end;

procedure TLogger.Send(Classes: TDebugClasses; const AText: string; const APoint: TPoint
  );
begin
  if Classes * ActiveClasses = [] then Exit;
  SendStream(lmtValue,AText+' = '+PointToStr(APoint),nil);
end;

procedure TLogger.Send(const AText: string; AStrings: TStrings);
begin
  Send(DefaultClasses,AText,AStrings);
end;

procedure TLogger.Send(Classes: TDebugClasses; const AText: string;
  AStrList: TStrings);
var
  S:string;
begin
  if Classes * ActiveClasses = [] then Exit;
  if Assigned(AStrList) then
    S:= AStrList.Text
  else
    S:='';
  SendBuffer(lmtStrings,AText,S[1],Length(S));
end;

procedure TLogger.Send(const AText: string; AObject: TObject);
begin
  Send(DefaultClasses,AText,AObject);
end;

procedure TLogger.Send(const AText: string; const AInterface: IInterface);
begin
  if AInterface is IInterfaceComponentReference then
    Send(AText, (AInterface as IInterfaceComponentReference).GetComponent)
  else
    Send(AText, 'no data for interface');
end;

function GetObjectDescription(Sender: TObject): string;
begin
  Result := Sender.ClassName;
  if (Sender is TComponent) and (TComponent(Sender).Name <> '') then
    Result := Result + '(' + TComponent(Sender).Name + ')';
end;

procedure TLogger.Send(Classes: TDebugClasses; const AText: string;
  AObject: TObject);
var
  TempStr: string;
  AStream: TStream;
begin
  if Classes * ActiveClasses = [] then Exit;
  AStream := nil;
  TempStr := AText + ' [';
  if AObject <> nil then
  begin
    if AObject is TComponent then
    begin
      AStream := TMemoryStream.Create;
      AStream.WriteComponent(TComponent(AObject));
    end
    else
      TempStr := TempStr + GetObjectDescription(AObject) + ' / ';
  end;
  TempStr := TempStr + ('$' + HexStr(AObject) + ']');
  //SendStream free AStream
  SendStream(lmtObject, TempStr, AStream);
end;

procedure TLogger.SendPointer(const AText: string; APointer: Pointer);
begin
  SendPointer(DefaultClasses,AText,APointer);
end;

procedure TLogger.SendPointer(Classes: TDebugClasses; const AText: string;
  APointer: Pointer);
begin
  if Classes * ActiveClasses = [] then Exit;
  SendStream(lmtValue, AText + ' = $' + HexStr(APointer), nil);
end;

procedure TLogger.SendCallStack(const AText: string);
begin
  SendCallStack(DefaultClasses,AText);
end;

procedure TLogger.SendCallStack(Classes: TDebugClasses; const AText: string);
var
  AStream: TStream;
begin
  if Classes * ActiveClasses = [] then Exit;
  AStream:=TMemoryStream.Create;
  GetCallStack(AStream);
  //SendStream free AStream
  SendStream(lmtCallStack,AText,AStream);
end;

procedure TLogger.SendException(const AText: string; AException: Exception);
begin
  SendException(DefaultClasses,AText,AException);
end;

procedure TLogger.SendException(Classes: TDebugClasses; const AText: string;
  AException: Exception);
var
  i      : Integer;
  Frames : PPointer;
  S      : string;
begin
  if Classes * ActiveClasses = [] then Exit;
  if AException <> nil then
    S:=AException.ClassName+' - '+AException.Message+LineEnding;
  S:= S + BackTraceStrFunc(ExceptAddr);
  Frames:=ExceptFrames;
  for i:= 0 to ExceptFrameCount - 1 do
    S:= S + (LineEnding+BackTraceStrFunc(Frames[i]));
  SendBuffer(lmtException,AText,S[1],Length(S));
end;

procedure TLogger.SendHeapInfo(const AText: string);
begin
  SendHeapInfo(DefaultClasses,AText);
end;

procedure TLogger.SendHeapInfo(Classes: TDebugClasses; const AText: string);
var
  S: string;
begin
  if Classes * ActiveClasses = [] then Exit;
  with GetFPCHeapStatus do
  begin
    S:='MaxHeapSize: '+FormatNumber(MaxHeapSize)+LineEnding
      +'MaxHeapUsed: '+FormatNumber(MaxHeapUsed)+LineEnding
      +'CurrHeapSize: '+FormatNumber(CurrHeapSize)+LineEnding
      +'CurrHeapUsed: '+FormatNumber(CurrHeapUsed)+LineEnding
      +'CurrHeapFree: '+FormatNumber(CurrHeapFree);
  end;
  SendBuffer(lmtHeapInfo,AText,S[1],Length(S));
end;

procedure TLogger.SendMemory(const AText: string; AAddress: Pointer;
  ASize: LongWord);
begin
  SendMemory(DefaultClasses,AText,AAddress,ASize)
end;

procedure TLogger.SendMemory(Classes: TDebugClasses; const AText: string;
  Address: Pointer; Size: LongWord);
begin
  if Classes * ActiveClasses = [] then Exit;
  SendBuffer(lmtMemory,AText,Address^,Size);
end;

procedure TLogger.SendIf(const AText: string; AExpression: Boolean);
begin
  SendIf(DefaultClasses, AText, AExpression, True);
end;

procedure TLogger.SendIf(Classes: TDebugClasses; const AText: string; Expression: Boolean
  );
begin
  SendIf(Classes, AText, Expression, True);
end;

procedure TLogger.SendIf(const AText: string; AExpression, AIsTrue: Boolean);
begin
  SendIf(DefaultClasses,AText,AExpression,AIsTrue);
end;

procedure TLogger.Info(const AText: string);
begin
  if DefaultClasses * ActiveClasses = [] then
    Exit;
  SendStream(lmtInfo, AText, nil);
end;

procedure TLogger.Info(const AText: string; AArgs: array of const);
begin
  Warn(Format(AText, AArgs));
end;

procedure TLogger.SendIf(Classes: TDebugClasses; const AText: string; Expression,
  IsTrue: Boolean);
begin
  if (DefaultClasses * ActiveClasses = []) or (Expression <> IsTrue) then
    Exit;
  SendStream(lmtConditional, AText, nil);
end;

procedure TLogger.Warn(const AText: string);
begin
  if DefaultClasses * ActiveClasses = [] then
    Exit;
  SendStream(lmtWarning, AText, nil);
end;

procedure TLogger.Warn(const AText: string; AArgs: array of const);
begin
  Warn(Format(AText, AArgs));
end;

procedure TLogger.Error(const AText: string);
begin
  if DefaultClasses * ActiveClasses = [] then
    Exit;
  SendStream(lmtError, AText, nil);
end;

procedure TLogger.Error(const AText: string; AArgs: array of const);
begin
  Error(Format(AText, AArgs));
end;

procedure TLogger.SendBitmap(const AText: string; ABitmap: TBitmap);
begin
  SendBitmap(DefaultClasses,AText,ABitmap);
end;

procedure TLogger.SendBitmap(Classes: TDebugClasses; const AText: string;
  ABitmap: TBitmap);
var
  AStream: TStream;
begin
  if Classes * ActiveClasses = [] then
    Exit;
  if ABitmap <> nil then
  begin
    AStream := TMemoryStream.Create;
    //use custom function to avoid bug in TBitmap.SaveToStream
    SaveBitmapToStream(ABitmap, AStream);
  end
  else
    AStream := nil;
  //SendStream free AStream
  SendStream(lmtBitmap, AText, AStream);
end;

procedure TLogger.SendColor(const AText: string; AColor: TColor);
begin
  SendColor(DefaultClasses, AText, AColor);
end;

procedure TLogger.SendColor(Classes: TDebugClasses; const AText: string;
  AColor: TColor);
begin
  if Classes * ActiveClasses = [] then Exit;
  SendStream(lmtValue, AText + ' = ' + ColorToStr(AColor),nil);
end;

procedure TLogger.SendComponent(AComponent: TComponent);
begin
  Send(Format('%s: %s', [AComponent.Name, AComponent.ClassName]), AComponent);
end;

procedure TLogger.AddCheckPoint;
begin
  AddCheckPoint(DefaultClasses,DEFAULT_CHECK_NAME);
end;

procedure TLogger.AddCheckPoint(Classes: TDebugClasses);
begin
  AddCheckPoint(Classes,DEFAULT_CHECK_NAME);
end;

procedure TLogger.AddCheckPoint(const AName: string);
begin
  AddCheckPoint(DefaultClasses,AName);
end;

procedure TLogger.AddCheckPoint(Classes: TDebugClasses; const CheckName: string);
var
  I : Integer;
  J : PtrInt;
begin
  if Classes * ActiveClasses = [] then
    Exit;
  I := FCheckList.IndexOf(CheckName);
  if I <> -1 then
  begin
    //Add a custom CheckList
    J := PtrInt(FCheckList.Objects[I]) + 1;
    FCheckList.Objects[I] := TObject(J);
  end
  else
  begin
    FCheckList.AddObject(CheckName, TObject(0));
    J := 0;
  end;
  SendStream(lmtCheckpoint, CheckName + ' #' + IntToStr(J), nil);
end;

procedure TLogger.IncCounter(const AName: string);
begin
  IncCounter(DefaultClasses,AName);
end;

procedure TLogger.IncCounter(Classes: TDebugClasses; const CounterName: string
  );
var
  I : Integer;
  J : PtrInt;
begin
  if Classes * ActiveClasses = [] then
    Exit;
  I := FCounterList.IndexOf(CounterName);
  if I <> -1 then
  begin
    J := PtrInt(FCounterList.Objects[I]) + 1;
    FCounterList.Objects[I] := TObject(J);
  end
  else
  begin
    FCounterList.AddObject(CounterName, TObject(1));
    J := 1;
  end;
  SendStream(lmtCounter, CounterName + '=' + IntToStr(J), nil);
end;

procedure TLogger.DecCounter(const AName: string);
begin
  DecCounter(DefaultClasses,AName);
end;

procedure TLogger.DecCounter(Classes: TDebugClasses; const CounterName: string
  );
var
  I : Integer;
  J : PtrInt;
begin
  if Classes * ActiveClasses = [] then
    Exit;
  I := FCounterList.IndexOf(CounterName);
  if I <> -1 then
  begin
    J := PtrInt(FCounterList.Objects[I]) - 1;
    FCounterList.Objects[I] := TObject(J);
  end
  else
  begin
    FCounterList.AddObject(CounterName, TObject(-1));
    J := -1;
  end;
  SendStream(lmtCounter, CounterName + '=' + IntToStr(J), nil);
end;

procedure TLogger.ResetCounter(const AName: string);
begin
  ResetCounter(DefaultClasses,AName);
end;

procedure TLogger.ResetCounter(Classes: TDebugClasses; const CounterName: string
  );
var
  i: Integer;
begin
  if Classes * ActiveClasses = [] then Exit;
  i := FCounterList.IndexOf(CounterName);
  if i <> -1 then
  begin
    FCounterList.Objects[i] := TObject(0);
    SendStream(lmtCounter, FCounterList[i] + '=0', nil);
  end;
end;

function TLogger.GetCounter(const AName: string): Integer;
var
  i: Integer;
begin
  i := FCounterList.IndexOf(AName);
  if i <> -1 then
    Result := PtrInt(FCounterList.Objects[i])
  else
    Result := 0;
end;

procedure TLogger.ResetCheckPoint;
begin
  ResetCheckPoint(DefaultClasses,DEFAULT_CHECK_NAME);
end;

procedure TLogger.ResetCheckPoint(Classes: TDebugClasses);
begin
  ResetCheckPoint(Classes,DEFAULT_CHECK_NAME);
end;

procedure TLogger.ResetCheckPoint(const AName: string);
begin
  ResetCheckPoint(DefaultClasses,AName);
end;

procedure TLogger.ResetCheckPoint(Classes: TDebugClasses; const CheckName:string);
var
  i: Integer;
begin
  if Classes * ActiveClasses = [] then Exit;
  i:=FCheckList.IndexOf(CheckName);
  if i <> -1 then
  begin
    FCheckList.Objects[i] := TObject(0);
    SendStream(lmtCheckpoint, CheckName + ' #0', nil);
  end;
end;

procedure TLogger.Enter(const AName: string);
begin
  EnterMethod(DefaultClasses,nil,AName);
end;

procedure TLogger.EnterMethod(Classes: TDebugClasses; const AMethodName: string);
begin
  EnterMethod(Classes,nil,AMethodName);
end;

procedure TLogger.Enter(ASender: TObject; const AName: string);
begin
  EnterMethod(DefaultClasses,ASender,AName);
end;

procedure TLogger.EnterMethod(Classes: TDebugClasses; Sender: TObject;
  const AMethodName: string);
begin
  if Classes * ActiveClasses = [] then Exit;
  FLogStack.Insert(0, UpperCase(AMethodName));
  if Sender <> nil then
    SendStream(lmtEnterMethod, GetObjectDescription(Sender) + '.' + AMethodName, nil)
  else
    SendStream(lmtEnterMethod, AMethodName, nil);
end;

procedure TLogger.Leave(const AName: string);
begin
  ExitMethod(DefaultClasses,nil,AName);
end;

procedure TLogger.Leave(ASender: TObject; const AName: string);
begin
  ExitMethod(DefaultClasses,ASender,AName);
end;

function TLogger.Track(const AName: string): IInterface;
begin
  Result := TTrack.Create(Self, nil, AName);
end;

function TLogger.Track(ASender: TObject; const AName: string): IInterface;
begin
  Result := TTrack.Create(Self, ASender, AName);
end;

procedure TLogger.ExitMethod(Classes: TDebugClasses; const AMethodName: string);
begin
  ExitMethod(Classes,nil,AMethodName);
end;

procedure TLogger.ExitMethod(Classes: TDebugClasses; Sender: TObject;
  const AMethodName: string);
var
  I : Integer;
begin
  //ensure that Leave will be called always even if there's an unpaired Entermethod
  //and Classes is not in ActiveClasses
  if FLogStack.Count = 0 then Exit;
  //todo: see if is necessary to do Uppercase (set case sensitive to false?)
  I := FLogStack.IndexOf(UpperCase(AMethodName));
  if I <> -1 then
    FLogStack.Delete(I)
  else
    Exit;
  if Sender <> nil then
    SendStream(lmtLeaveMethod, GetObjectDescription(Sender) + '.' + AMethodName, nil)
  else
    SendStream(lmtLeaveMethod, AMethodName, nil);
end;

procedure TLogger.Watch(const AText, AValue: string);
begin
  Watch(DefaultClasses, AText, AValue);
end;

procedure TLogger.Watch(Classes: TDebugClasses; const AText, AValue: string);
begin
  if Classes * ActiveClasses = [] then
    Exit;
  SendStream(lmtWatch, AText + '=' + AValue, nil);
end;

procedure TLogger.Watch(const AText: string; AValue: Integer);
begin
  Watch(DefaultClasses, AText, AValue);
end;

procedure TLogger.Watch(Classes: TDebugClasses; const AText: string;
  AValue: Integer);
begin
  if Classes * ActiveClasses = [] then
    Exit;
  SendStream(lmtWatch, AText + '=' + IntToStr(AValue), nil);
end;

procedure TLogger.Watch(const AText: string; AValue: Cardinal);
begin
  Watch(DefaultClasses,AText,AValue);
end;

procedure TLogger.Watch(Classes: TDebugClasses; const AText: string;
  AValue: Cardinal);
begin
  if Classes * ActiveClasses = [] then
    Exit;
  SendStream(lmtWatch, AText + '=' + IntToStr(AValue), nil);
end;

procedure TLogger.Watch(const AText: string; AValue: Double);
begin
  Watch(DefaultClasses, AText, AValue);
end;

procedure TLogger.Watch(Classes: TDebugClasses; const AText: string;
  AValue: Double);
begin
  if Classes * ActiveClasses = [] then
    Exit;
  SendStream(lmtWatch, AText + '=' + FloatToStr(AValue), nil);
end;

procedure TLogger.Watch(const AText: string; AValue: Boolean);
begin
  Watch(DefaultClasses, AText, AValue);
end;

procedure TLogger.Watch(Classes: TDebugClasses; const AText: string;
  AValue: Boolean);
begin
  if Classes * ActiveClasses = [] then
    Exit;
  SendStream(lmtWatch, AText + '=' + BoolToStr(AValue), nil);
end;

{ TChannelList }

function TChannelList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TChannelList.GetItems(AIndex:Integer): TLogChannel;
begin
  Result := TLogChannel(FList[AIndex]);
end;

constructor TChannelList.Create;
begin
  FList := TFPList.Create;
end;

destructor TChannelList.Destroy;
var
  I : Integer;
begin
  //free the registered channels
  for I := 0 to FList.Count - 1 do
    Items[I].Free;
  FList.Destroy;
end;

function TChannelList.Add(AChannel: TLogChannel):Integer;
begin
  Result := FList.Add(AChannel);
  AChannel.Init;
end;

procedure TChannelList.Remove(AChannel: TLogChannel);
begin
  FList.Remove(AChannel);
end;

{ TLogChannel }

procedure TLogChannel.Init;
begin
end;

end.

