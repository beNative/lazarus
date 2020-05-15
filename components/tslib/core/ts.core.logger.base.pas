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

unit ts.Core.Logger.Base;

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
  Classes, SysUtils, Graphics, Menus,
  fgl,

  ts.Core.Value, ts.Core.Logger.Interfaces, ts.Core.Logger.Channel;

const
  DEFAULT_MAXSTACKCOUNT  = 20;

type
  { TLogger }

  TLogger = class(TInterfacedObject, ILogger)
  type
    // https://stackoverflow.com/questions/9592654/what-are-the-differences-between-implementation-of-interfaces-in-delphi-and-laza
    { TODO: this trick does not work with FPC. In delphi the object cleanup
    of an interface variable happens at the end of the scope of the calling
    method. In FPC it can be sooner (when it is not referenced elsewhere in
    the given scope) }

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
    FChannels      : IChannelList;
    FLogStack      : TStrings;
    FLogLevel      : Byte;
    FCheckList     : TStringList;
    FCounterList   : TFPGMap<string, Int64>;
    FEnabled       : Boolean;

  protected
    {$REGION 'property access methods'}
    function GetChannels: IChannelList;
    procedure GetCallStack(AStream:TStream);
    function GetLogLevel: Byte;
    procedure SetLogLevel(const AValue: Byte);
    procedure SetMaxStackCount(const AValue: Integer);
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
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
      AMsgType    : TLogMessageType;
      const AText : string;
      AStream     : TStream
    );
    procedure SendBuffer(
      AMsgType    : TLogMessageType;
      const AText : string;
      var Buffer;
      Count       : LongWord
    );

    //Helper functions
    function RectToStr(const ARect: TRect): string;
    function PointToStr(const APoint: TPoint): string;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    function CalledBy(const AMethodName: string): Boolean;
    function Clear: ILogger;

    //Send functions
    function Send(const AName: string; const AArgs: array of const): ILogger; overload;

    function Send(const AName: string; const AValue: TValue): ILogger; overload;
    function Send(const AValue: TValue): ILogger; overload;

    function Send(const AName: string; const AValue: string): ILogger; overload;
    //function Send(const AName: string; const AValue: AnsiString): ILogger; overload;
    function Send(const AName: string; const AValue: WideString): ILogger; overload;
    function Send(const AName: string; const AValue: ShortString): ILogger; overload;

    // Overloads for builtin integer types
    //function Send(const AName: string; const AValue: Cardinal): ILogger; overload;
    function Send(const AName: string; const AValue: Word): ILogger; overload;
    function Send(const AName: string; const AValue: SmallInt): ILogger; overload;
    function Send(const AName: string; const AValue: Byte): ILogger; overload;
    function Send(const AName: string; const AValue: ShortInt): ILogger; overload;
    function Send(const AName: string; const AValue: UInt64): ILogger; overload;
    function Send(const AName: string; const AValue: Integer): ILogger; overload;

    function Send(const AValue: string): ILogger; overload;
    function Send(const AValue: WideString): ILogger; overload;
    function Send(const AValue: ShortString): ILogger; overload;

    //function Send(const AValue: Cardinal): ILogger; overload;
    function Send(const AValue: Word): ILogger; overload;
    function Send(const AValue: SmallInt): ILogger; overload;
    function Send(const AValue: Byte): ILogger; overload;
    function Send(const AValue: ShortInt): ILogger; overload;
    function Send(const AValue: UInt64): ILogger; overload;
    function Send(const AValue: Integer): ILogger; overload;

    function SendText(
      const AName        : string;
      const AText        : string;
      const AHighlighter : string = ''
    ): ILogger; overload;
    function SendText(const AText: string): ILogger; overload;

    //function Send(const AName: string; const AValue: FixedInt): ILogger; overload;

    //procedure SendBitmap(const AText: string; ABitmap: TBitmap); //inline;
    //procedure SendColor(const AText: string; AColor: TColor); //inline;
    //procedure SendCallStack(const AText: string); overload; inline;
    //procedure SendHeapInfo(const AText: string); overload; inline;
    //procedure SendMemory(const AText: string; AAddress: Pointer; ASize: LongWord); overload; inline;

    function SendIf(
      const AText : string;
      AExpression : Boolean;
      AIsTrue     : Boolean = True
    ): ILogger;

    function SendComponent(const AName: string; AValue: TComponent): ILogger; overload;
    function SendComponent(AValue: TComponent): ILogger; overload;
    function SendException(const AName: string; AValue: Exception): ILogger; overload;
    function SendException(AValue: Exception): ILogger; overload;
    function SendPoint(const AName: string; const AValue: TPoint): ILogger; overload;
    function SendPoint(const AValue: TPoint): ILogger; overload;
    function SendPointer(const AName: string; AValue: Pointer): ILogger; overload;
    function SendPointer(AValue: Pointer): ILogger; overload;
    function SendRect(const AName: string; const AValue: TRect): ILogger; overload;
    function SendRect(const AValue: TRect): ILogger; overload;

    function SendShortCut(const AName: string; AValue: TShortCut): ILogger; overload;
    function SendShortCut(AValue: TShortCut): ILogger; overload;
    function SendStrings(const AName: string; AValue: TStrings): ILogger; overload;
    function SendStrings(AValue: TStrings): ILogger; overload;
    function SendStrings(const AValue: string): ILogger; overload;
    function SendStrings(const AName: string; AValue: string): ILogger; overload;
    function SendVariant(const AName: string; const AValue: Variant): ILogger; overload;
    function SendVariant(const AValue: Variant): ILogger; overload;

    function Info(const AText: string): ILogger; overload;
    function Info(const AText: string; AArgs: array of const): ILogger; overload;
    function Warn(const AText: string): ILogger; overload;
    function Warn(const AText: string; AArgs: array of const): ILogger; overload;
    function Error(const AText: string): ILogger; overload;
    function Error(const AText: string; AArgs: array of const): ILogger; overload;

    { Checkpoints are used to count how many times the application enters a
      certain position in the code. }
    function AddCheckPoint(const AName: string = ''): ILogger;
    function ResetCheckPoint(const AName: string = ''): ILogger;

    function Action(AAction: TBasicAction): ILogger;

    { Counter support }
    function IncCounter(const AName: string): ILogger;
    function DecCounter(const AName: string): ILogger;
    function ResetCounter(const AName: string): ILogger;
    function GetCounter(const AName: string): Int64;

    function Enter(const AName: string): ILogger; overload; inline;
    function Enter(ASender: TObject; const AName: string): ILogger; overload; inline;
    function Leave(const AName: string): ILogger; overload; inline;
    function Leave(ASender: TObject; const AName: string): ILogger; overload; inline;
    function Track(const AName: string): IInterface; overload;
    function Track(
      const AName : string;
      const AArgs : array of const
    ): IInterface; overload;
    function Track(ASender: TObject; const AName: string): IInterface; overload;
    function Track(
      ASender     : TObject;
      const AName : string;
      const AArgs : array of const
    ): IInterface; overload;
    //
    function Watch(const AName: string; const AValue: TValue): ILogger; overload;
    function Watch(const AName, AValue: string): ILogger; overload;
    function Watch(const AName: string; AValue: Integer): ILogger; overload;
    function Watch(const AName: string; AValue: Cardinal): ILogger; overload;
    function Watch(const AName: string; AValue: Double): ILogger; overload;
    function Watch(const AName: string; AValue: Boolean): ILogger; overload;

    { List of channels where logmessages will be posted to }
    property Channels: IChannelList
      read GetChannels;

    property LogLevel: Byte
      read GetLogLevel write SetLogLevel;

    property LogStack: TStrings
      read FLogStack;

    property Enabled: Boolean
      read GetEnabled write SetEnabled;

    property MaxStackCount: Integer
      read FMaxStackCount write SetMaxStackCount default DEFAULT_MAXSTACKCOUNT;
  end;

implementation

uses
  IntfGraphics, GraphType, FPimage, FPWriteBMP, LCLProc,

  ts.Core.Logger.Channel.Ipc;

const
  STACKCOUNTLIMIT        = 256;
  DEFAULT_CHECKPOINTNAME = 'CheckPoint';

{$REGION 'non-interfaced routines'}
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

function GetObjectDescription(Sender: TObject): string;
begin
  Result := Sender.ClassName;
  if (Sender is TComponent) and (TComponent(Sender).Name <> '') then
    Result := Result + '(' + TComponent(Sender).Name + ')';
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
{$ENDREGION}

{$REGION 'TLogger'}
{$REGION 'construction and destruction'}
procedure TLogger.AfterConstruction;
begin
  inherited AfterConstruction;
  FChannels                  := TChannelList.Create;
  FEnabled                   := True;
  FMaxStackCount             := DEFAULT_MAXSTACKCOUNT;
  FLogStack                  := TStringList.Create;
  FCheckList                 := TStringList.Create;
  FCheckList.CaseSensitive   := False;
  FCheckList.Sorted          := True;
  FEnabled                   := True;
  FCounterList               := TFPGMap<string, Int64>.Create;
end;

destructor TLogger.Destroy;
begin
  FChannels := nil;
  FLogStack.Free;
  FCheckList.Free;
  FCounterList.Free;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TLogger.GetChannels: IChannelList;
begin
  Result := FChannels;
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
  bp := get_caller_frame(get_frame);
  try
    prevbp := bp - 1;
    I := 0;
    //is_dev:=do_isdevice(textrec(f).Handle);
    while bp > prevbp Do
     begin
       caller_addr  := get_caller_addr(bp);
       caller_frame := get_caller_frame(bp);
       if caller_addr = nil then
         Break;
       //todo: see what is faster concatenate string and use writebuffer or current
       S := BackTraceStrFunc(caller_addr) + LineEnding;
       AStream.WriteBuffer(S[1], Length(S));
       Inc(I);
       if (I >= FMaxStackCount) or (caller_frame = nil) then
         Break;
       prevbp := bp;
       bp := caller_frame;
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

procedure TLogger.SetMaxStackCount(const AValue: Integer);
begin
  if AValue < STACKCOUNTLIMIT then
    FMaxStackCount := AValue
  else
    FMaxStackCount := STACKCOUNTLIMIT;
end;

function TLogger.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TLogger.SetEnabled(const Value: Boolean);
begin
  if Value <> Enabled then
  begin
    FEnabled := Value;
  end;
end;
{$ENDREGION}

{$REGION 'private methods'}
function TLogger.RectToStr(const ARect: TRect): string;
begin
  with ARect do
    Result := Format(
      '(Left: %d; Top: %d; Right: %d; Bottom: %d)',
      [Left, Top, Right, Bottom]
    );
end;

function TLogger.PointToStr(const APoint: TPoint): string;
begin
  with APoint do
    Result := Format('(X: %d; Y: %d)',[X,Y]);
end;
{$ENDREGION}

{$REGION 'protected methods'}
{$REGION 'Internal methods'}
function TLogger.InternalSendStream(AMsgType: TLogMessageType;
  const AText: string; AStream: TStream): ILogger;
var
  LM : TLogMessage;
  LC : ILogChannel;
  I  : Integer;
begin
  Result := Self;
  if Enabled then
  begin
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
      LC := Channels[I];
      if LC.Enabled then
        LC.Write(LM);
    end;
  end;
end;

function TLogger.InternalSend(AMsgType: TLogMessageType; const AText: string)
  : ILogger;
begin
  Result := InternalSendStream(AMsgType, AText, nil);
end;

function TLogger.InternalSendBuffer(AMsgType: TLogMessageType;
  const AText: string; var ABuffer; ACount: LongWord): ILogger;
var
  LStream : TStream;
begin
  Result  := Self;
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
{$ENDREGION}

{$REGION 'Notification'}
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

function TLogger.AddCheckPoint(const AName: string): ILogger;
begin
  Result := Self;
end;

function TLogger.ResetCheckPoint(const AName: string): ILogger;
begin
  Result := Self;
end;

function TLogger.Action(AAction: TBasicAction): ILogger;
begin
  Result := InternalSend(lmtAction, AAction.Name);
end;

function TLogger.IncCounter(const AName: string): ILogger;
var
  LValue : Int64;
begin
  if not FCounterList.TryGetData(AName, LValue) then
  begin
    LValue := 1;
    FCounterList.AddOrSetData(AName, LValue);
  end
  else
  begin
    LValue := FCounterList[AName] + 1;
    FCounterList[AName] := LValue;
  end;
  Result := InternalSend(lmtCounter, AName + ' = ' + IntToStr(LValue));
  Result := Self;
end;

function TLogger.DecCounter(const AName: string): ILogger;
begin
  Result := Self;
end;

function TLogger.ResetCounter(const AName: string): ILogger;
begin
  Result := Self;
end;

function TLogger.GetCounter(const AName: string): Int64;
begin
  if not FCounterList.TryGetData(AName, Result) then
  begin
    Result := 0;
  end;
end;
{$ENDREGION}

{$REGION 'Send overloads'}
function TLogger.Send(const AName: string; const AValue: string): ILogger;
var
  S : string;
begin
  S := Format('%s = %s', [AName, AValue]);
  Result := InternalSend(lmtValue, S);
end;

function TLogger.Send(const AName: string; const AValue: ShortString): ILogger;
begin
  Result := Send(AName, string(AValue));
end;

function TLogger.Send(const AName: string; const AValue: WideString): ILogger;
begin
  Result := Send(AName, WideString(AValue));
end;

function TLogger.Send(const AName: string; const AArgs: array of const)
  : ILogger;
begin
  Result := Self;
end;

function TLogger.Send(const AName: string; const AValue: UInt64): ILogger;
begin
  Result := Send(AName, TValue(AValue));
end;

function TLogger.Send(const AName: string; const AValue: Integer): ILogger;
begin
  Result := Send(AName, TValue(AValue));
end;

function TLogger.Send(const AName: string; const AValue: TValue): ILogger;
var
  S : string;
begin
  S := Format('%s = %s', [
    AName,
    AValue.ToString
  ]);
  Result := InternalSend(lmtValue, S);
end;

function TLogger.Send(const AValue: TValue): ILogger;
begin
  Result := Send('', AValue);
end;

function TLogger.Send(const AName: string; const AValue: Byte): ILogger;
begin
  Result := Send(AName, TValue(AValue));
end;

function TLogger.Send(const AName: string; const AValue: Word): ILogger;
begin
  Result := Send(AName, TValue(AValue));
end;

function TLogger.Send(const AName: string; const AValue: ShortInt): ILogger;
begin
  Result := Send(AName, TValue(AValue));
end;

//function TLogger.Send(const AName: string; const AValue: Cardinal): ILogger;
//begin
//  Result := Send(AName, IntToStr(AValue));
//end;

function TLogger.Send(const AName: string; const AValue: SmallInt): ILogger;
begin
  Result := Send(AName, IntToStr(AValue));
end;

function TLogger.Send(const AValue: string): ILogger;
begin
  Result := Send('', AValue);
end;

function TLogger.Send(const AValue: WideString): ILogger;
begin
  Result := Send('', AValue);
end;

function TLogger.Send(const AValue: ShortString): ILogger;
begin
  Result := Send('', AValue);
end;

//function TLogger.Send(const AValue: Cardinal): ILogger;
//begin
//  Result := Send('', AValue);
//end;

function TLogger.Send(const AValue: Word): ILogger;
begin
  Result := Send('', AValue);
end;

function TLogger.Send(const AValue: SmallInt): ILogger;
begin
  Result := Send('', AValue);
end;

function TLogger.Send(const AValue: Byte): ILogger;
begin
  Result := Send('', AValue);
end;

function TLogger.Send(const AValue: ShortInt): ILogger;
begin
  Result := Send('', AValue);
end;

function TLogger.Send(const AValue: UInt64): ILogger;
begin
  Result := Send('', AValue);
end;

function TLogger.Send(const AValue: Integer): ILogger;
begin
  Result := Send('', AValue);
end;
{$ENDREGION}

{$REGION 'Specialized Send methods'}
function TLogger.SendText(const AName: string; const AText: string;
  const AHighlighter: string): ILogger;
var
  S : string;
begin
  if AHighlighter.IsEmpty then
    S := Format('%s'#13#10'%s', [AName, AText])
  else
    S := Format('%s (%s)'#13#10'%s', [AName, AHighlighter, AText]);
  Result := InternalSend(lmtText, S);
end;

function TLogger.SendText(const AText: string): ILogger;
var
  S : string;
begin
  S := #13#10 + AText;
  Result := InternalSend(lmtText, S);
end;

function TLogger.SendIf(const AText: string; AExpression: Boolean;
  AIsTrue: Boolean): ILogger;
begin
  Result := Self;
  if AExpression = AIsTrue then
    InternalSend(lmtConditional, AText);
end;

function TLogger.SendComponent(const AName: string; AValue: TComponent
  ): ILogger;
var
  LStream : TStream;
  S       : string;
begin
  Result := Self;
  S := Format('%s (%s) = %s', [
    AName,
    AValue.ClassName,
    AValue.Name
  ]);
  LStream := TMemoryStream.Create;
  try
    LStream.WriteComponent(TComponent(AValue));
    InternalSendStream(lmtComponent, S, LStream);
  finally
    LStream.Free;
  end;
end;

function TLogger.SendComponent(AValue: TComponent): ILogger;
begin
  Result := SendComponent('', AValue);
end;

function TLogger.SendPointer(const AName: string; AValue: Pointer): ILogger;
begin
  Result := InternalSend(lmtValue, AName + ' = $' + IntToHex(NativeInt(AValue), 8));
end;

function TLogger.SendPointer(AValue: Pointer): ILogger;
begin
  Result := SendPointer('', AValue);
end;

function TLogger.SendRect(const AName: string; const AValue: TRect): ILogger;
begin
  Result := Send(AName, RectToStr(AValue));
end;

function TLogger.SendRect(const AValue: TRect): ILogger;
begin
  Result := SendRect('', AValue);
end;

function TLogger.SendException(const AName: string; AValue: Exception): ILogger;
var
  S : string;
begin
  if AValue <> nil then
    S := AValue.ClassName + ' - ' + AValue.Message + sLineBreak;
  Result := InternalSendBuffer(lmtException, AName, S[1], Length(S));
end;

function TLogger.SendException(AValue: Exception): ILogger;
begin
  Result := SendException('', AValue);
end;

function TLogger.SendPoint(const AName: string; const AValue: TPoint): ILogger;
begin
  Result := Send(AName, PointToStr(AValue));
end;

function TLogger.SendPoint(const AValue: TPoint): ILogger;
begin
  Result := SendPoint('', AValue);
end;

function TLogger.SendShortCut(const AName: string; AValue: TShortCut): ILogger;
begin
  Result := Send(AName, ShortCutToText(AValue));
end;

function TLogger.SendShortCut(AValue: TShortCut): ILogger;
begin
  Result := SendShortCut('', AValue);
end;

function TLogger.SendStrings(const AName: string; AValue: TStrings): ILogger;
begin
  Result := InternalSend(
    lmtStrings,
    Format('%s (%s) = ' + sLineBreak + '%s', [AName, AValue.ClassName, AValue.Text])
  );
end;

function TLogger.SendStrings(AValue: TStrings): ILogger;
begin
  Result := SendStrings('', AValue);
end;

function TLogger.SendStrings(const AName: string; AValue: string): ILogger;
begin
  Result := InternalSend(
    lmtStrings,
    Format('%s = ' + sLineBreak + '%s', [AName, AValue])
  );
end;

function TLogger.SendVariant(const AName: string; const AValue: Variant
  ): ILogger;
begin
  Result := Self;
  //Result := Send(AName, TValue.FromVariant(AValue));
end;

function TLogger.SendVariant(const AValue: Variant): ILogger;
begin
  Result := SendVariant('', AValue);
end;

function TLogger.SendStrings(const AValue: string): ILogger;
begin
  Result := SendStrings('', AValue);
end;
{$ENDREGION}

{$REGION 'Tracing methods'}
function TLogger.Enter(const AName: string): ILogger;
begin
  Result := Enter(nil, AName);
end;

function TLogger.Enter(ASender: TObject; const AName: string): ILogger;
begin
  Result := Self;
  FLogStack.Insert(0, UpperCase(AName));
  if Assigned(ASender) then
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
  if Assigned(ASender) then
  begin
    if ASender is TComponent then
      InternalSend(lmtLeaveMethod, TComponent(ASender).Name + '.' + AName)
    else
      InternalSend(lmtLeaveMethod, ASender.ClassName + '.' + AName);
  end
  else
    InternalSend(lmtLeaveMethod, AName);
end;

function TLogger.Track(const AName: string): IInterface;
begin
  Result := TTrack.Create(Self, nil, AName);
end;

function TLogger.Track(const AName: string;
  const AArgs: array of const): IInterface;
begin
  Result := TTrack.Create(Self, nil, Format(AName, AArgs));
end;

function TLogger.Track(ASender: TObject; const AName: string): IInterface;
var
  LI : IInterface;
begin
  LI := TTrack.Create(Self, ASender, AName);
  Result := LI;
end;

function TLogger.Track(ASender: TObject; const AName: string;
  const AArgs: array of const): IInterface;
begin
  Result := TTrack.Create(Self, ASender, Format(AName, AArgs));
end;
{$ENDREGION}

{$REGION 'Watch'}
function TLogger.Watch(const AName: string; const AValue: TValue): ILogger;
var
  S : string;
begin
  S := Format('%s = %s', [
    AName,
    AValue.ToString
  ]);
  Result := InternalSend(lmtWatch, S);
end;

function TLogger.Watch(const AName, AValue: string): ILogger;
begin
  Result := Self;
end;

function TLogger.Watch(const AName: string; AValue: Integer): ILogger;
begin
  Result := Self;
end;

function TLogger.Watch(const AName: string; AValue: Cardinal): ILogger;
begin
  Result := Self;
end;

function TLogger.Watch(const AName: string; AValue: Double): ILogger;
begin
  Result := Self;
end;

function TLogger.Watch(const AName: string; AValue: Boolean): ILogger;
begin
  Result := Self;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'public methods'}
procedure TLogger.SendStream(AMsgType: TLogMessageType; const AText: string;
  AStream: TStream);
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

procedure TLogger.SendBuffer(AMsgType: TLogMessageType; const AText: string;
  var Buffer; Count: LongWord);
var
  LStream : TStream;
begin
  if Count > 0 then
  begin
    LStream := TMemoryStream.Create;
    LStream.Write(Buffer, Count);
  end
  else
    LStream := nil;
  SendStream(AMsgType, AText, LStream);
end;

function TLogger.CalledBy(const AMethodName: string): Boolean;
begin
  Result := FLogStack.IndexOf(UpperCase(AMethodName)) <> -1;
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
    InternalSend(lmtClear, '');
    Sleep(100);
  end;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'TLogger.TTrack'}
constructor TLogger.TTrack.Create(const ALogger: ILogger; ASender: TObject;
  const AName: string);
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
{$ENDREGION}

end.

