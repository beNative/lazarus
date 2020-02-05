{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
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

  ts.Core.Logger.Interfaces, ts.Core.Logger.Channel;

type
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
    FLogLevel      : Byte;
    FCheckList     : TStringList;
    FCounterList   : TStringList;

  protected
    {$REGION 'property access methods'}
    function GetChannels: IChannelList;
    procedure GetCallStack(AStream:TStream);
    function GetLogLevel: Byte;
    procedure SetLogLevel(const AValue: Byte);
    procedure SetMaxStackCount(const AValue: Integer);
    {$ENDREGION}

    function InternalSend(
      AMsgType    : TLogMessageType;
      const AText : string = ''
    ): ILogger;
    function InternalSendStream(
      AMsgType    : TLogMessageType;
      const AText : string;
      AStream     : TStream
    ): ILogger;
    function InternalSendBuffer(
      AMsgType    : TLogMessageType;
      const AText : string;
      var ABuffer;
      ACount      : LongWord
    ): ILogger;

    procedure SendStream(
      AMsgType: TLogMessageType;
      const AText:string;
      AStream: TStream
    );
    procedure SendBuffer(
      AMsgType: TLogMessageType;
      const AText:string;
      var Buffer; Count: LongWord
    );

  public
    constructor Create;
    destructor Destroy; override;
    function CalledBy(const AMethodName: string): Boolean;
    function Clear: ILogger;
    //Helper functions
    function RectToStr(const ARect: TRect): string; //inline
    function PointToStr(const APoint: TPoint): string; //inline

    //Send functions
    function Send(const AName: string; const AArgs: array of const): ILogger; overload;

    function Send(const AName: string; const AValue: string): ILogger; overload;
    //function Send(const AName: string; const AValue: AnsiString): ILogger; overload;
    function Send(const AName: string; const AValue: WideString): ILogger; overload;
    function Send(const AName: string; const AValue: ShortString): ILogger; overload;

    // Overloads for builtin integer types
    function Send(const AName: string; const AValue: Cardinal): ILogger; overload;
    function Send(const AName: string; const AValue: Word): ILogger; overload;
    function Send(const AName: string; const AValue: SmallInt): ILogger; overload;
    function Send(const AName: string; const AValue: Byte): ILogger; overload;
    function Send(const AName: string; const AValue: ShortInt): ILogger; overload;
    function Send(const AName: string; const AValue: UInt64): ILogger; overload;
    //function Send(const AName: string; const AValue: FixedInt): ILogger; overload;

    //procedure SendBitmap(const AText: string; ABitmap: TBitmap); //inline;
    //procedure SendColor(const AText: string; AColor: TColor); //inline;
    //procedure SendComponent(AComponent: TComponent); inline;
    //procedure SendPointer(const AText: string; APointer: Pointer); overload; inline;
    //procedure SendCallStack(const AText: string); overload; inline;
    //procedure SendException(const AText: string; AException: Exception);overload; inline;
    //procedure SendHeapInfo(const AText: string); overload; inline;
    //procedure SendMemory(const AText: string; AAddress: Pointer; ASize: LongWord); overload; inline;
    //procedure SendIf(const AText: string; AExpression: Boolean); overload; inline;
    //procedure SendIf(const AText: string; AExpression, AIsTrue: Boolean); overload; inline;
    //
    function Info(const AText: string): ILogger; overload;
    function Info(const AText: string; AArgs: array of const): ILogger; overload;
    function Warn(const AText: string): ILogger; overload;
    function Warn(const AText: string; AArgs: array of const): ILogger; overload;
    function Error(const AText: string): ILogger; overload;
    function Error(const AText: string; AArgs: array of const): ILogger; overload;
    //
    //procedure IncCounter(const AName: string); overload; inline;
    //procedure DecCounter(const AName: string); overload; inline;
    //procedure ResetCounter(const AName: string);overload; inline;
    //function GetCounter(const AName: string): Integer;
    //
    //procedure AddCheckPoint; overload; inline;
    //procedure AddCheckPoint(const AName: string);overload; inline;
    //procedure ResetCheckPoint; overload; inline;
    //procedure ResetCheckPoint(const AName: string); overload; inline;
    //
    function Enter(const AName: string): ILogger; overload; inline;
    function Enter(ASender: TObject; const AName: string): ILogger; overload; inline;
    function Leave(const AName: string): ILogger; overload; inline;
    function Leave(ASender: TObject; const AName: string): ILogger; overload; inline;
    //function Track(const AName: string): IInterface; overload;
    //function Track(ASender: TObject; const AName: string): IInterface; overload;
    //
    //procedure Watch(const AText, AValue: string); overload; inline;
    //procedure Watch(const AText: string; AValue: Integer); overload; inline;
    //procedure Watch(const AText: string; AValue: Cardinal); overload; inline;
    //procedure Watch(const AText: string; AValue: Double); overload; inline;
    //procedure Watch(const AText: string; AValue: Boolean); overload; inline;

    { List of channels where logmessages will be posted to }
    property Channels: IChannelList
      read GetChannels;

    property LogLevel: Byte
      read GetLogLevel write SetLogLevel;

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
  FLogger := ALogger;
  FSender := ASender;
  FName   := AName;
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
end;

destructor TLogger.Destroy;
begin
  FChannels.Destroy;
  FLogStack.Destroy;
  FCheckList.Destroy;
  FCounterList.Destroy;
end;

function TLogger.GetChannels: IChannelList;
begin
  Result := FChannels as IChannelList;
end;

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

function TLogger.GetLogLevel: Byte;
begin
  Result := FLogLevel;
end;

procedure TLogger.SetLogLevel(const AValue: Byte);
begin
  FLogLevel := AValue;
end;

procedure TLogger.SendStream(AMsgType: TLogMessageType; const AText: string; AStream: TStream);
var
  LMessage : TLogMessage;
  I        :Integer;
begin
  with LMessage do
  begin
    MsgType   := Integer(AMsgType);
    TimeStamp := Now;
    Text      := AText;
    Data      := AStream;
  end;
  for I := 0 to Channels.Count - 1 do
    if Channels[I].Enabled then
      Channels[I].Write(LMessage);
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

function TLogger.InternalSend(AMsgType: TLogMessageType; const AText: string
  ): ILogger;
begin
  Result := InternalSendStream(AMsgType, AText, nil);
end;

function TLogger.InternalSendStream(AMsgType: TLogMessageType;
  const AText: string; AStream: TStream): ILogger;
var
  LM : TLogMessage;
  LC : ILogChannel;
  I  : Integer;
begin
  Result := Self;
  LM := Default(TLogMessage);
  LM.MsgType   := Byte(AMsgType);
  LM.LogLevel  := LogLevel;
  LM.Reserved1 := 0;
  LM.Reserved2 := 0;
  LM.TimeStamp := Now;
  LM.Text      := UTF8String(AText);
  LM.Data      := AStream;
  for I := 0 to Channels.Count - 1 do
  begin
    LC := Channels[I] as ILogChannel;
    if LC.Enabled then
      LC.Write(LM);
  end;
end;

function TLogger.InternalSendBuffer(AMsgType: TLogMessageType;
  const AText: string; var ABuffer; ACount: LongWord): ILogger;
var
  LStream: TStream;
begin
  Result := Self;
  LStream := nil;
  if ACount > 0 then
  begin
    LStream := TMemoryStream.Create;
    try
      LStream.Write(ABuffer, ACount);
      InternalSendStream(AMsgType, AText, LStream);
    finally
      FreeAndNil(LStream);
    end;
  end
  else
    InternalSendStream(AMsgType, AText, nil);
end;

function TLogger.CalledBy(const AMethodName: string): Boolean;
begin
  Result:=FLogStack.IndexOf(UpperCase(AMethodName)) <> -1;
end;

function TLogger.Clear: ILogger;
var
  I : Integer;
begin
  Result := Self;
  // repeated to compensate for initial message loss in combination with
  // some channels (ZeroMQ)
  for I := 0 to 3 do
  begin
    InternalSend(lmtClear);
    Sleep(100);
  end;
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

function TLogger.Send(const AName: string;
  const AArgs: array of const): ILogger;
begin
  Result := Self;
end;

function TLogger.Send(const AName: string; const AValue: string): ILogger;
begin
//  SendStream(lmtValue,AValue+' = '+AValue,nil);
  Result := Self;
end;

function TLogger.Send(const AName: string; const AValue: WideString): ILogger;
begin
  Result := Self;
end;

function TLogger.Send(const AName: string; const AValue: ShortString): ILogger;
begin
  Result := Self;
end;

function TLogger.Send(const AName: string; const AValue: Cardinal): ILogger;
begin
  Result := Self;
end;

function TLogger.Send(const AName: string; const AValue: Word): ILogger;
begin
  Result := Self;
end;

function TLogger.Send(const AName: string; const AValue: SmallInt): ILogger;
begin
  Result := Self;
end;

function TLogger.Send(const AName: string; const AValue: Byte): ILogger;
begin
  Result := Self;
end;

function TLogger.Send(const AName: string; const AValue: ShortInt): ILogger;
begin
  Result := Self;
end;

function TLogger.Send(const AName: string; const AValue: UInt64): ILogger;
begin
  Result := Self;
end;

function TLogger.Info(const AText: string): ILogger;
begin
  Result := InternalSend(lmtInfo, AText);
end;

function TLogger.Info(const AText: string; AArgs: array of const): ILogger;
begin
   Result := InternalSend(lmtInfo, Format(AText, AArgs));
end;

function TLogger.Warn(const AText: string): ILogger;
begin
  Result := InternalSend(lmtWarning, AText);
end;

function TLogger.Warn(const AText: string; AArgs: array of const): ILogger;
begin
  Result := InternalSend(lmtWarning, Format(AText, AArgs));
end;

function TLogger.Error(const AText: string): ILogger;
begin
  Result := InternalSend(lmtError, AText);
end;

function TLogger.Error(const AText: string; AArgs: array of const): ILogger;
begin
  Result := InternalSend(lmtError, Format(AText, AArgs));
end;

function TLogger.Enter(const AName: string): ILogger;
begin
  Result := Enter(nil, AName);
end;

function TLogger.Enter(ASender: TObject; const AName: string): ILogger;
begin
  Result := Self;
  FLogStack.Insert(0, UpperCase(AName));
  if ASender <> nil then
  begin
    if ASender is TComponent then
      InternalSend(lmtEnterMethod, TComponent(ASender).Name + '.' + AName)
    else
      InternalSend(lmtEnterMethod, ASender.ClassName + '.' + AName);
  end
  else
    InternalSend(lmtEnterMethod, AName);
end;

function TLogger.Leave(const AName: string): ILogger;
begin
  Result := Leave(nil, AName);
end;

function TLogger.Leave(ASender: TObject; const AName: string): ILogger;
var
  I : Integer;
begin
  Result := Self;
  // ensure that Leave will be called allways if there's a unpaired Enter
  if FLogStack.Count = 0 then
    Exit;
  I := FLogStack.IndexOf(AName);
  if I <> -1 then
    FLogStack.Delete(I)
  else
    Exit;
  if ASender <> nil then
  begin
    if ASender is TComponent then
      InternalSend(lmtLeaveMethod, TComponent(ASender).Name + '.' + AName)
    else
      InternalSend(lmtLeaveMethod, ASender.ClassName + '.' + AName);
  end
  else
    InternalSend(lmtLeaveMethod, AName);
end;

//procedure TLogger.Send(const AText: string; const AInterface: IInterface);
//begin
//  if AInterface is IInterfaceComponentReference then
//    Send(AText, (AInterface as IInterfaceComponentReference).GetComponent)
//  else
//    Send(AText, 'no data for interface');
//end;

function GetObjectDescription(Sender: TObject): string;
begin
  Result := Sender.ClassName;
  if (Sender is TComponent) and (TComponent(Sender).Name <> '') then
    Result := Result + '(' + TComponent(Sender).Name + ')';
end;

//procedure TLogger.Send(Classes: TDebugClasses; const AText: string;
//  AObject: TObject);
//var
//  TempStr: string;
//  AStream: TStream;
//begin
//  if Classes * ActiveClasses = [] then Exit;
//  AStream := nil;
//  TempStr := AText + ' [';
//  if AObject <> nil then
//  begin
//    if AObject is TComponent then
//    begin
//      AStream := TMemoryStream.Create;
//      AStream.WriteComponent(TComponent(AObject));
//    end
//    else
//      TempStr := TempStr + GetObjectDescription(AObject) + ' / ';
//  end;
//  TempStr := TempStr + ('$' + HexStr(AObject) + ']');
//  //SendStream free AStream
//  SendStream(lmtObject, TempStr, AStream);
//end;

//procedure TLogger.SendCallStack(Classes: TDebugClasses; const AText: string);
//var
//  AStream: TStream;
//begin
//  if Classes * ActiveClasses = [] then Exit;
//  AStream:=TMemoryStream.Create;
//  GetCallStack(AStream);
//  //SendStream free AStream
//  SendStream(lmtCallStack,AText,AStream);
//end;

//procedure TLogger.SendException(Classes: TDebugClasses; const AText: string;
//  AException: Exception);
//var
//  i      : Integer;
//  Frames : PPointer;
//  S      : string;
//begin
//  if Classes * ActiveClasses = [] then Exit;
//  if AException <> nil then
//    S:=AException.ClassName+' - '+AException.Message+LineEnding;
//  S:= S + BackTraceStrFunc(ExceptAddr);
//  Frames:=ExceptFrames;
//  for i:= 0 to ExceptFrameCount - 1 do
//    S:= S + (LineEnding+BackTraceStrFunc(Frames[i]));
//  SendBuffer(lmtException,AText,S[1],Length(S));
//end;

//procedure TLogger.SendHeapInfo(const AText: string);
//begin
//  SendHeapInfo(DefaultClasses,AText);
//end;
//
//procedure TLogger.SendHeapInfo(Classes: TDebugClasses; const AText: string);
//var
//  S: string;
//begin
//  if Classes * ActiveClasses = [] then Exit;
//  with GetFPCHeapStatus do
//  begin
//    S:='MaxHeapSize: '+FormatNumber(MaxHeapSize)+LineEnding
//      +'MaxHeapUsed: '+FormatNumber(MaxHeapUsed)+LineEnding
//      +'CurrHeapSize: '+FormatNumber(CurrHeapSize)+LineEnding
//      +'CurrHeapUsed: '+FormatNumber(CurrHeapUsed)+LineEnding
//      +'CurrHeapFree: '+FormatNumber(CurrHeapFree);
//  end;
//  SendBuffer(lmtHeapInfo,AText,S[1],Length(S));
//end;

//procedure TLogger.SendBitmap(Classes: TDebugClasses; const AText: string;
//  ABitmap: TBitmap);
//var
//  AStream: TStream;
//begin
//  if Classes * ActiveClasses = [] then
//    Exit;
//  if ABitmap <> nil then
//  begin
//    AStream := TMemoryStream.Create;
//    //use custom function to avoid bug in TBitmap.SaveToStream
//    SaveBitmapToStream(ABitmap, AStream);
//  end
//  else
//    AStream := nil;
//  //SendStream free AStream
//  SendStream(lmtBitmap, AText, AStream);
//end;


end.

