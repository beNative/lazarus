{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This library is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit LogViewer.WatchList;

{ Copyright (C) 2006 Luiz Américo Pereira Câmara

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  ts.Core.SharedLogger;
  
type
  TWatchUpdate = procedure (
    const AVariable : string;
    const AValue    : string
  ) of object;
  TNewWatchVariable = procedure (
    const AVariable : string;
    AIndex          : PtrInt
  ) of object;

  TVariableValue = record
    Index : LongWord;
    Value : string;
  end;
  
  PVariableValue = ^TVariableValue;

  { TWatchVariable }

  TWatchVariable = class
  private
    FFirstIndex   : LongWord;
    FName         : string;
    FList         : TFpList;
    FCurrentIndex : Integer;

    function GetCount: Integer;
    function GetCurrentValue:string;
    function GetValues(AIndex: Integer): string;

  public
    constructor Create(const AName: string; AIndex: LongWord);
    destructor Destroy; override;
    procedure AddValue (const AValue: string; AIndex: LongWord);
    function Find (AIndex: LongWord): Boolean;

    property Name: string
      read FName;

    property CurrentValue: string
      read GetCurrentValue;

    property Values[AIndex: Integer]: string
      read GetValues; default;

    property Count: Integer
      read GetCount;
  end;
  
  { TWatchList }

  TWatchList = class
  private
    FList          : TFpList;
    FOnNewVariable : TNewWatchVariable;
    FOnUpdate      : TWatchUpdate;

    function GetCount: Integer;
    function GetItems(AValue: Integer): TWatchVariable;

  public
    constructor Create;
    destructor Destroy; override;

    function IndexOf(const AName: string): Integer;
    procedure Add(
      const ANameValue  : string;
      AIndex            : LongWord;
      SkipOnNewVariable : Boolean
    );
    procedure Clear;
    procedure Update(AIndex: LongWord);

    property OnUpdate: TWatchUpdate
      read FOnUpdate write FOnUpdate;

    property OnNewVariable: TNewWatchVariable
      read FOnNewVariable write FOnNewVariable;

    property Items[AValue: Integer]: TWatchVariable
      read GetItems; default;

    property Count: Integer
      read GetCount;
  end;

implementation

{ TWatchVariable }

function TWatchVariable.GetCurrentValue: string;
begin
  Result := PVariableValue(FList[FCurrentIndex])^.Value;
end;

function TWatchVariable.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TWatchVariable.GetValues(AIndex: Integer): string;
begin
  Result := PVariableValue(FList[AIndex])^.Value;
end;

constructor TWatchVariable.Create(const AName: string; AIndex: LongWord);
begin
  FList := TFPList.Create;
  FName := AName;
  FFirstIndex := AIndex;
end;

destructor TWatchVariable.Destroy;
var
  i:Integer;
begin
  for i := 0 to FList.Count - 1 do
    Dispose(PVariableValue(FList[i]));
  FList.Destroy;
end;

procedure TWatchVariable.AddValue(const AValue: string; AIndex: LongWord);
var
  TempValue: PVariableValue;
begin
  New(TempValue);
  TempValue^.Index:=AIndex;
  TempValue^.Value:=AValue;
  FList.Add(TempValue);
end;

function TWatchVariable.Find(AIndex: LongWord): Boolean;
var
  I :Integer;
begin
  Result:=False;
  if AIndex < FFirstIndex then
    Exit;
  for I:= FList.Count - 1 downto 0 do
  begin
    if AIndex >= PVariableValue(FList[I])^.Index then
    begin
      Result := True;
      FCurrentIndex := I;
      Exit;
    end;
  end;
end;

{ TWatchList }

function TWatchList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TWatchList.GetItems(AValue: Integer): TWatchVariable;
begin
  Result := TWatchVariable(FList[AValue]);
end;

constructor TWatchList.Create;
begin
  FList := TFPList.Create;
end;

destructor TWatchList.Destroy;
begin
  Clear;
  FList.Destroy;
end;

function TWatchList.IndexOf(const AName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FList.Count - 1 do
  begin
    if Uppercase(TWatchVariable(FList[I]).Name) = Uppercase(AName) then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

procedure TWatchList.Add(const ANameValue: string; AIndex: LongWord; SkipOnNewVariable: Boolean);
var
  PosEqual,i:Integer;
  TempStr: string;
begin
  PosEqual := Pos('=',ANameValue);
  TempStr := Copy(ANameValue,1,PosEqual-1);
  i := IndexOf(TempStr);
  if i = -1 then
  begin
    i := FList.Add(TWatchVariable.Create(TempStr,AIndex));
    if not SkipOnNewVariable then
      FOnNewVariable(TempStr, i);
  end;
  TempStr := Copy(ANameValue,PosEqual+1,Length(ANameValue)-PosEqual);
  TWatchVariable(FList[i]).AddValue(TempStr,AIndex);
end;

procedure TWatchList.Clear;
var
  I:Integer;
begin
  for I:= 0 to FList.Count - 1 do
    TWatchVariable(FList[I]).Destroy;
  FList.Clear;
end;

procedure TWatchList.Update(AIndex: LongWord);
var
  I: Integer;
begin
  for I:= 0 to FList.Count - 1 do
  begin
    with TWatchVariable(FList[I]) do
    begin
      if Find(AIndex) then
        FOnUpdate(Name,CurrentValue);
    end;
  end;
end;

end.

