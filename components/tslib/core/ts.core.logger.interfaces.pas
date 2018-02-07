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
    lmtObject      = 10,
    lmtException   = 11,
    lmtBitmap      = 12,
    lmtHeapInfo    = 13,  // not supported yet
    lmtMemory      = 14,
    lmtCustomData  = 15,  // not supported yet
    lmtWatch       = 20,
    lmtCounter     = 21,
    lmtColor       = 22,
    lmtAlphaColor  = 23,
    lmtScreenShot  = 24,  // not supported yet
    lmtText        = 25,  // arbitrary text with optional highlighter info
    lmtClear       = 99
  );

type
  ILogger = interface(IInterface)
  ['{4DC99719-54BF-49F6-8A5B-A57145DB53C2}']
    //function GetChannels: TChannelList;

//    procedure Send(const AName: string; const AArgs: array of const); overload;
    { This overload is used for Variant arguments. }
    procedure Send(const AName: string; const AValue: string = ''); overload;

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

    procedure IncCounter(const AName: string);
    procedure DecCounter(const AName: string);
    procedure ResetCounter(const AName: string);
    function GetCounter(const AName: string): Integer;

    procedure Enter(const AName: string); overload;
    procedure Enter(ASender: TObject; const AName: string); overload;
    procedure Leave(const AName: string); overload;
    procedure Leave(ASender: TObject; const AName: string); overload;
    { Track uses an interface variable to replace Enter/Leave calls in the
      scope of the method where it is called. A call to Track will create an
      instance and trigger the Enter method. When the interface variable goes
      out of scope (end of the routine or method) a call to the logger's Leave
      method is triggered. }
    function Track(const AName: string): IInterface; overload;
    function Track(ASender: TObject; const AName: string): IInterface; overload;

    procedure AddCheckPoint(const AName: string = '');
    procedure ResetCheckPoint(const AName: string = '');

    { Monitors a named value in the LogViewer application }
    //procedure Watch(const AName: string; const AValue: TValue); overload;
    procedure Watch(const AName: string; const AValue: string = ''); overload;

  procedure Info(const AText: string); overload;
  procedure Info(const AText: string; AArgs: array of const); overload;
  procedure Warn(const AText: string); overload;
  procedure Warn(const AText: string; AArgs: array of const); overload;
  procedure Error(const AText: string); overload;
  procedure Error(const AText: string; AArgs: array of const); overload;

    //procedure Warn(const AText: string); overload;
    //procedure Warn(
    //  const AText : string;
    //  const AArgs : array of const
    //); overload;
    //procedure Error(const AText: string); overload;
    //procedure Error(
    //  const AText : string;
    //  const AArgs : array of const
    //); overload;
    //procedure Info(const AText: string); overload;
    //procedure Info(
    //  const AText: string;
    //  const AArgs: array of const
    //); overload;

    procedure SendIf(
      const AText : string;
      AExpression : Boolean;
      AIsTrue     : Boolean = True
    );
    { Sends out a dedicated message to clear the logviewer contents. }
    procedure Clear;

    //property Channels: TChannelList
    //  read GetChannels;
  end;

implementation

end.

