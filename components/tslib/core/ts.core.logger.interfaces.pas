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

unit ts.Core.Logger.Interfaces;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils;

type
  TLogMessageType = (
    lmtInfo        = 0,
    lmtError       = 1,
    lmtWarning     = 2,
    lmtValue       = 3,
    lmtEnterMethod = 4,
    lmtLeaveMethod = 5,
    lmtConditional = 6,
    lmtCheckpoint  = 7,
    lmtStrings     = 8,   // TStrings and descendants
    lmtCallStack   = 9,   // not supported yet
    lmtComponent   = 10,
    lmtException   = 11,
    lmtBitmap      = 12,
    lmtHeapInfo    = 13,  // not supported yet
    lmtMemory      = 14,
    lmtCustomData  = 15,  // not supported yet
    lmtObject      = 16,
    lmtInterface   = 17,
    lmtPersistent  = 18,
    lmtReserved    = 19,
    lmtWatch       = 20,
    lmtCounter     = 21,
    lmtColor       = 22,
    lmtAlphaColor  = 23,
    lmtScreenShot  = 24,
    lmtText        = 25,  // arbitrary text with optional highlighter info
    lmtDataSet     = 26,
    lmtAction      = 27,  // TAction execution
    lmtClear       = 99,
    lmtNone        = 100  // can be used as a default value
  );
  TLogMessageTypes  = set of TLogMessageType;

  TLogMessageLevel  = 0..31;
  TLogMessageLevels = set of TLogMessageLevel;

const
  TracingMessages      : TLogMessageTypes =
    [lmtEnterMethod, lmtLeaveMethod];
  NotificationMessages : TLogMessageTypes =
    [lmtInfo, lmtError, lmtWarning, lmtConditional, lmtCheckpoint];
  DataValueMessages    : TLogMessageTypes = [
    lmtValue, lmtStrings, lmtComponent, lmtException, lmtBitmap, lmtObject,
    lmtInterface, lmtPersistent, lmtColor, lmtAlphaColor, lmtScreenShot,
    lmtText, lmtDataSet, lmtAction, lmtMemory
  ];
  StateMessages        : TLogMessageTypes =
    [lmtCounter, lmtWatch];
  CommandMessages      : TLogMessageTypes =
     [lmtClear];
  DiagnosticMessages   : TLogMessageTypes =
    [lmtCallStack, lmtHeapInfo];
  AllMessages          : TLogMessageTypes = [
    lmtEnterMethod, lmtLeaveMethod,
    lmtInfo, lmtError, lmtWarning, lmtConditional,
    lmtValue, lmtStrings, lmtComponent, lmtException, lmtBitmap, lmtObject,
    lmtInterface, lmtPersistent, lmtColor, lmtAlphaColor, lmtScreenShot,
    lmtCustomData,
    lmtText, lmtDataSet, lmtAction, lmtMemory,
    lmtCounter, lmtCheckpoint,
    lmtWatch,
    lmtReserved,
    lmtClear,
    lmtCallStack, lmtHeapInfo,
    lmtNone
  ];
  AllLevels : TLogMessageLevels =
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
     21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31];

type
  TLogMessage = packed record
    MsgType   : Byte; // TLogMessageType
    LogLevel  : Byte; // TLogMessageLevel
    Reserved1 : Byte;
    Reserved2 : Byte;
    TimeStamp : TDateTime;
    Text      : UTF8String;
    Data      : TStream;
  end;

  ILogChannel = interface
  ['{AE6B5FE0-1E93-44BC-9EF0-B3D38AD7C1AF}']
    {$REGION 'property access methods'}
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    function GetConnected: Boolean;
    function GetAutoConnect: Boolean;
    procedure SetAutoConnect(const Value: Boolean);
    {$ENDREGION}

    function Write(const AMsg: TLogMessage): Boolean;
    function Connect: Boolean;
    function Disconnect: Boolean;

    property Enabled: Boolean
      read GetEnabled write SetEnabled;

    { True when the channel is connected with the server (or receiving)
      instance. }
    property Connected: Boolean
      read GetConnected;
  end;

  { IChannelList }

  IChannelList = interface
  ['{39FC1459-F7FA-4190-A83E-1B0905456792}']
  function GetCount: Integer;
  function GetItems(AIndex: Integer): ILogChannel;

  property Count: Integer
    read GetCount;

  property Items[AIndex:Integer]: ILogChannel
    read GetItems; default;

  end;

  ILogger = interface
  ['{4DC99719-54BF-49F6-8A5B-A57145DB53C2}']
    {$REGION 'property access methods'}
    function GetChannels: IChannelList;
    function GetLogLevel: Byte;
    procedure SetLogLevel(const Value: Byte);
    {$ENDREGION}

    { These three overloads are here because TValue would cast them implicitely
      to string (and we would lose type information of AValue) }
    //function Send(const AName: string; const AValue: AnsiString): ILogger; overload;
    function Send(const AName: string; const AValue: WideString): ILogger; overload;
    function Send(const AName: string; const AValue: ShortString): ILogger; overload;
    function Send(const AName: string; const AValue: string): ILogger; overload;

    { UInt8 = Byte }
    function Send(const AName: string; const AValue: Byte): ILogger; overload;
    { UInt16 = Word }
    function Send(const AName: string; const AValue: Word): ILogger; overload;
    { UInt32 = Cardinal = FixedUInt }
    function Send(const AName: string; const AValue: Cardinal): ILogger; overload;
    { UInt64 }
    function Send(const AName: string; const AValue: UInt64): ILogger; overload;
    { Int8 = ShortInt }
    function Send(const AName: string; const AValue: ShortInt): ILogger; overload;
    { Int16 = SmallInt }
    function Send(const AName: string; const AValue: SmallInt): ILogger; overload;
    { Int32 = Integer = FixedInt }
    //function Send(const AName: string; const AValue: FixedInt): ILogger; overload;

    { All primary types that can implicitely be casted to TValue will be
      handled through this call. }
    //procedure Send(const AName: string; const AValue: TValue); overload;

    { Send methods for types that do not have an implicit cast to TValue
      These are equivalent to Send(AName, TValue.From(AValue)); }
    //procedure SendDateTime(const AName: string; AValue: TDateTime);
    //procedure SendDate(const AName: string; AValue: TDate);
    //procedure SendTime(const AName: string; AValue: TTime);

    { Send methods for types that need a custom representation. }
    //procedure SendColor(const AName: string; AColor: TColor);
    //procedure SendAlphaColor(const AName: string; AAlphaColor: TAlphaColor);
    //procedure SendObject(const AName: string; AValue: TObject);
    //procedure SendRect(const AName: string; const AValue: TRect);
    //procedure SendPoint(const AName: string; const APoint: TPoint);
    //procedure SendStrings(const AName: string; AValue: TStrings);
    //TODO procedure SendPersistent(const AName: string; AValue: TPersistent); -> log published properties

    //procedure SendComponent(const AName: string; AValue: TComponent);
    //procedure SendPointer(const AName: string; APointer: Pointer);
    //procedure SendException(const AName: string; AException: Exception);
    //procedure SendMemory(
    //  const AName: string;
    //  AAddress   : Pointer;
    //  ASize      : LongWord
    //);
    //procedure SendShortCut(const AName: string; AShortCut: TShortCut);

    // SendBitmap

    { Send methods for text that can be displayed with a dedicated
      highlighter. }
    //procedure SendText(
    //  const AName        : string;
    //  const AText        : string;
    //  const AHighlighter : string = ''
    //);

  //  procedure IncCounter(const AName: string);
  //  procedure DecCounter(const AName: string);
  //  procedure ResetCounter(const AName: string);
  //  function GetCounter(const AName: string): Integer;
  //
    function Enter(const AName: string): ILogger; overload;
    function Enter(ASender: TObject; const AName: string): ILogger; overload;
    function Leave(const AName: string): ILogger; overload;
    function Leave(ASender: TObject; const AName: string): ILogger; overload;
  //  { Track uses an interface variable to replace Enter/Leave calls in the
  //    scope of the method where it is called. A call to Track will create an
  //    instance and trigger the Enter method. When the interface variable goes
  //    out of scope (end of the routine or method) a call to the logger's Leave
  //    method is triggered. }
  //  function Track(const AName: string): IInterface; overload;
  //  function Track(ASender: TObject; const AName: string): IInterface; overload;
  //
  //  procedure AddCheckPoint(const AName: string = '');
  //  procedure ResetCheckPoint(const AName: string = '');
  //
  //  { Monitors a named value in the LogViewer application }
  //  //procedure Watch(const AName: string; const AValue: TValue); overload;
  //  procedure Watch(const AName: string; const AValue: string = ''); overload;
  //
  function Info(const AText: string): ILogger; overload;
  function Info(const AText: string; AArgs: array of const): ILogger; overload;
  function Warn(const AText: string): ILogger; overload;
  function Warn(const AText: string; AArgs: array of const): ILogger; overload;
  function Error(const AText: string): ILogger; overload;
  function Error(const AText: string; AArgs: array of const): ILogger; overload;

    //procedure SendIf(
    //  const AText : string;
    //  AExpression : Boolean;
    //  AIsTrue     : Boolean = True
    //);
    { Sends out a dedicated message to clear the logviewer contents. }
    function  Clear: ILogger;

    property Channels: IChannelList
      read GetChannels;
  end;

implementation

end.

