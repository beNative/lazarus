{
  Copyright (C) 2013-2023 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Core.Collections;

{$MODE DELPHI}

interface

{ Note: Generic constraint syntax not supported in FPC in Delphi compatible
  mode.

  Anthony Walter explained in FPC issue 23158 how the implementation of generics
  in FPC differs from the one in Delphi:

    "Constraints would be nice, but the way FPC handles generics in some ways is
    much superior to the current Delphi implementation or even the C#
    implementation, in my opinion.

    I think the way FPC generics work now is like C++ templates, which are code
    generators. The generated code is checked against the types at compile time
    allowing us to write something like:

      function TMyClass<T>.Add(const A, B: T): T;
      begin
        Result := A + B;
      end;

    The above is not possible in Delphi or C#, but the above is HIGHLY desirable.
    It's exactly because the code above won't work in Delphi or C# why those
    languages have generic constraints. The constraints relax those compilers to
    allow call specific methods of T to be invoked. But currently FPC generics
    will figure it methods of T exist at compile time without the need for
    constraints and this is a good thing.

    As such, the only reason I see to add constraints is to make FPC code more
    compatible with Delphi."
}

uses
  Classes;

type
  TBaseCollection = TCollection;
  TBaseCollectionEnumerator = TCollectionEnumerator;

  TCollectionNotifyEvent<T> = procedure(
    Sender     : TObject;
    const Item : T;
    Action     : TCollectionNotification
  ) of object;

  TCollectionEnumerator<T> = class(TBaseCollectionEnumerator)
  public
    function GetCurrent: T; inline;

    property Current: T
      read GetCurrent;
  end;

  TCollection<T> = class(TBaseCollection)
  protected
    function GetCount: Integer;
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; Value: T);

  public
    constructor Create; reintroduce; virtual;

    function Add: T;
    function Insert(Index: Integer): T;

    property Items[Index: Integer]: T
      read GetItem write SetItem; default;

  end;

  TOwnedCollection<T> = class(TCollection<T>)
  private
    FOwner: TPersistent;

  protected
    function GetOwner: TPersistent; override;

  public
    constructor Create(AOwner: TPersistent); reintroduce; virtual;
  end;

implementation

{$REGION 'TCollectionEnumerator<T>'}
function TCollectionEnumerator<T>.GetCurrent: T;
begin
  Result := T(inherited GetCurrent);
end;
{$ENDREGION}

{$REGION 'TCollection<T>'}
function TCollection<T>.Add: T;
begin
  Result := T(inherited Add());
end;

constructor TCollection<T>.Create;
begin
  inherited Create(T);
end;

function TCollection<T>.GetCount: Integer;
begin
  Result := inherited Count;
end;

function TCollection<T>.GetItem(Index: Integer): T;
begin
  Result := T(inherited GetItem(Index));
end;

function TCollection<T>.Insert(Index: Integer): T;
begin
  Result := T(inherited Insert(Index));
end;

procedure TCollection<T>.SetItem(Index: Integer; Value: T);
begin
  inherited SetItem(Index, Value);
end;
{$ENDREGION}

{$REGION 'TOwnedCollection<T>'}
constructor TOwnedCollection<T>.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
  inherited Create;
end;

function TOwnedCollection<T>.GetOwner: TPersistent;
begin
  Result := FOwner;
end;
{$ENDREGION}

end.

