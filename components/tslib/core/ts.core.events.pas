unit ts.Core.Events;

{$MODE Delphi}

interface

uses
  Classes, SysUtils,

  ts.Core.Generics.Collections;

type
  TEvent<T> = class
  private

    FItems: TArray<TMethod>;
    FCount: integer;
    function GetItems(Index: integer): TMethod;
    procedure SetItems(Index: integer; const AValue: TMethod);
  public
    destructor Destroy; override;
    function Count: integer;
    function NextDownIndex(var Index: integer): boolean;
    function IndexOf(const AMethod: TMethod): integer;
    procedure Delete(Index: integer);
    procedure Remove(const AMethod: TMethod);
    procedure Add(const AMethod: TMethod); overload;
    procedure Add(const AMethod: TMethod; AsLast: boolean); overload;
    procedure Insert(Index: integer; const AMethod: TMethod);
    procedure Move(OldIndex, NewIndex: integer);
    procedure RemoveAllMethodsOfObject(const AnObject: TObject);
    procedure CallNotifyEvents(Sender: TObject);
  public
    property Items[Index: integer]: TMethod read GetItems write SetItems; default;
  end;

implementation

{ TEvent }

function TEvent<T>.GetItems(Index: integer): TMethod;
begin
  Result:=FItems[Index];
end;

procedure TEvent<T>.SetItems(Index: integer; const AValue: TMethod);
begin
  FItems[Index]:=AValue;
end;

destructor TEvent<T>.Destroy;
begin
  ReAllocMem(FItems,0);
  inherited Destroy;
end;

function TEvent<T>.Count: integer;
begin
  if Self<>nil then
    Result:=FCount
  else
    Result:=0;
end;

function TEvent<T>.NextDownIndex(var Index: integer): boolean;
begin
  if Self<>nil then begin
    dec(Index);
    if (Index>=FCount) then
      Index:=FCount-1;
  end else
    Index:=-1;
  Result:=(Index>=0);
end;

function TEvent<T>.IndexOf(const AMethod: TMethod): integer;
begin
  if Self<>nil then begin
    Result:=FCount-1;
    while Result>=0 do begin
      if (FItems[Result].Code=AMethod.Code)
      and (FItems[Result].Data=AMethod.Data) then exit;
      dec(Result);
    end;
  end else
    Result:=-1;
end;

procedure TEvent<T>.Delete(Index: integer);
begin
  dec(FCount);
  if FCount>Index then
    System.Move(FItems[Index+1],FItems[Index],(FCount-Index)*SizeOf(TMethod));
  ReAllocMem(FItems,FCount*SizeOf(TMethod));
end;

procedure TEvent<T>.Remove(const AMethod: TMethod);
var
  i: integer;
begin
  if Self<>nil then begin
    i:=IndexOf(AMethod);
    if i>=0 then Delete(i);
  end;
end;

procedure TEvent<T>.Add(const AMethod: TMethod);
begin
  inc(FCount);
  ReAllocMem(FItems,FCount*SizeOf(TMethod));
  FItems[FCount-1]:=AMethod;
end;

procedure TEvent<T>.Add(const AMethod: TMethod; AsLast: boolean);
begin
  if AsLast then
    Add(AMethod)
  else
    Insert(0,AMethod);
end;

procedure TEvent<T>.Insert(Index: integer; const AMethod: TMethod);
begin
  inc(FCount);
  ReAllocMem(FItems,FCount*SizeOf(TMethod));
  if Index<FCount then
    System.Move(FItems[Index],FItems[Index+1],(FCount-Index-1)*SizeOf(TMethod));
  FItems[Index]:=AMethod;
end;

procedure TEvent<T>.Move(OldIndex, NewIndex: integer);
var
  MovingMethod: TMethod;
begin
  if OldIndex=NewIndex then exit;
  MovingMethod:=FItems[OldIndex];
  if OldIndex>NewIndex then
    System.Move(FItems[NewIndex],FItems[NewIndex+1],
                SizeOf(TMethod)*(OldIndex-NewIndex))
  else
    System.Move(FItems[NewIndex+1],FItems[NewIndex],
                SizeOf(TMethod)*(NewIndex-OldIndex));
  FItems[NewIndex]:=MovingMethod;
end;

procedure TEvent<T>.RemoveAllMethodsOfObject(const AnObject: TObject);
var
  i: Integer;
begin
  if Self=nil then exit;
  i:=FCount-1;
  while i>=0 do begin
    if TObject(FItems[i].Data)=AnObject then Delete(i);
    dec(i);
  end;
end;

procedure TEvent<T>.CallNotifyEvents(Sender: TObject);
var
  i: LongInt;
begin
  i:=Count;
  while NextDownIndex(i) do
    TNotifyEvent(Items[i])(Sender);
end;

end.

