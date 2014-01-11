{
  Copyright (C) 2013-2014 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Editor.Types;

{$MODE Delphi}

interface

uses
  Classes, SysUtils,

  SynMacroRecorder;

type
  TSortDirection = (
    sdAscending,
    sdDescending
  );

  TSortScope = (
    ssWords,
    ssLines,
    ssParagraphs
  );
  TAlignToToken = (
    atLeftMost,
    atRightMost
  );
  // event types
  TCaretPositionEvent = procedure(
    Sender : TObject;
    X, Y   : Integer
  ) of object;

  TStatusMessageEvent = procedure(
    Sender : TObject;
    Text   : string
  ) of object;

  TMacroStateChangeEvent = procedure(
    Sender: TObject;
    AState: TSynMacroState
  ) of object;

  TStorageEvent = procedure(
        Sender       : TObject;
    var AStorageName : string
  ) of object;

  TNewEvent = procedure(
          Sender       : TObject;
    var   AStorageName : string;
    const AText        : string
  ) of object;

  TOpenOtherInstanceEvent = procedure(
          Sender  : TObject;
    const AParams : array of string
  ) of object;

  // type aliases
  TEditorViewList = TInterfaceList;

implementation

end.

