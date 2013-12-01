{
  Copyright (C) 2013 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Editor.Highlighters;

{ Holds settings that are specific to each supported highlighter. }

{$MODE Delphi}

interface

uses
  SysUtils, Classes,

  SynEditHighlighter,

  ts.Editor.Utils, ts.Editor.CodeFormatters, ts.Editor.CodeTags,

  ts.Core.SharedLogger;

type
  TSynHighlighterClass = class of TSynCustomHighlighter;
  THighlighters        = class;

  { THighlighterItem }

  THighlighterItem = class(TCollectionItem)
  private
    FBlockCommentEndTag   : string;
    FBlockCommentStartTag : string;
    FCodeFormatter        : ICodeFormatter;
    FDescription          : string;
    FFormatterSupport     : Boolean;
    FLayoutFileName       : string;
    FLineCommentTag       : string;
    FDefaultFilter        : string;
    FName                 : string;
    FSmartSelectionTags   : TCodeTags;
    FSynHighlighter       : TSynCustomHighlighter;
    FSynHighlighterClass  : TSynHighlighterClass;
    FFileExtensions       : TStringList;

    // private property access methods
    function GetDefaultFilter: string;
    function GetFileExtensions: string;
    procedure SetCollection(const Value: THighlighters); reintroduce;
    function GetCollection: THighlighters;
    procedure SetDefaultFilter(AValue: string);
    procedure SetFileExtensions(AValue: string);
    procedure SetFormatterSupport(const AValue: Boolean);
    procedure SetSmartSelectionTags(AValue: TCodeTags);
    procedure SetSynHighlighter(AValue: TSynCustomHighlighter);
    procedure SetSynHighlighterClass(AValue: TSynHighlighterClass);

  protected
    function GetDisplayName: string; override;

  public
    // constructors and destructors
    constructor Create(ACollection: TCollection); override;
    procedure BeforeDestruction; override;

    procedure InitSynHighlighter(
      { Optional instance to assign the settings to. }
      ASynHighlighter: TSynCustomHighlighter = nil
    );
    procedure Assign(Source: TPersistent); override;
    function AsString: string;
    procedure Reload;

    { Collection that owns the instance of current THighlighterItem item. }
    property Collection: THighlighters
      read GetCollection write SetCollection;

    property SynHighlighterClass: TSynHighlighterClass
      read FSynHighlighterClass write SetSynHighlighterClass;

    property CodeFormatter: ICodeFormatter
      read FCodeFormatter write FCodeFormatter;

  published
    { The name displayed in the collection editor at design time. }
    property Name: string
      read FName write FName;

    { Character sequence that designates the start of a block comment. }
    property BlockCommentStartTag: string
      read FBlockCommentStartTag write FBlockCommentStartTag;

    { Character sequence that designates the end of a block comment. }
    property BlockCommentEndTag: string
      read FBlockCommentEndTag write FBlockCommentEndTag;

    { Character sequence that designates a line comment. }
    property LineCommentTag: string
      read FLineCommentTag write FLineCommentTag;

    property FormatterSupport: Boolean
      read FFormatterSupport write SetFormatterSupport;

    { Comma seperated list of file extensions associated with the highlighter. }
    property FileExtensions: string
      read GetFileExtensions write SetFileExtensions;

    property DefaultFilter: string
      read GetDefaultFilter write SetDefaultFilter;

    property Description: string
      read FDescription write FDescription;

    property LayoutFileName: string
      read FLayoutFileName write FLayoutFileName;

    property SynHighlighter: TSynCustomHighlighter
      read FSynHighlighter write SetSynHighlighter;

    { Set of tags that are used for the SmartSelect feature of the editor. }
    property SmartSelectionTags: TCodeTags
      read FSmartSelectionTags write SetSmartSelectionTags;

  end;

  THighlighterItemClass = class of THighlighterItem;

  { THighlighters inherits from TOwnedCollection to show the items in
    the Object Treeview of the IDE at designtime. }

  THighlighters = class(TOwnedCollection)
  private
  type
    THighlighterEnumerator = class
    private
      FHighlighters : THighlighters;
      FPosition     : Integer;

      function GetCurrent: THighlighterItem;
    public
      constructor Create(AHighlighters: THighlighters);

      function MoveNext: Boolean;

      property Current: THighlighterItem
        read GetCurrent;
    end;

    // property access methods
    function GetFileFilter: string;
    function GetItem(Index: Integer): THighlighterItem;
    function GetItemByName(const AName: string): THighlighterItem;
    procedure SetItem(Index: Integer; const Value: THighlighterItem);
    procedure SetItemByName(const AName: string; const AValue: THighlighterItem);

  protected
    procedure SetItemName(Item: TCollectionItem); override;
    procedure Update(AItem: TCollectionItem); override;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification);
      override;

  public
    // constructors and destructors
    constructor Create(AOwner: TPersistent);

    function Add: THighlighterItem;
    function Insert(Index: Integer): THighlighterItem;
    function Owner: TComponent; reintroduce;
    function AsString: string;
    function Exists(const AName: string): Boolean;

    function GetEnumerator: THighlighterEnumerator;

    function IndexOf(const AName: string): Integer; virtual;
    function FindItemID(ID: Integer): THighlighterItem;
    function Find(const AName: string): THighlighterItem;
    function FindHighlighterForFileType(const AFileExt: string): THighlighterItem;

    procedure RegisterHighlighter(
            ASynHighlighterClass  : TSynHighlighterClass;
            ASynHighlighter       : TSynCustomHighlighter;  // To ASSIGN the settings!!!
      const AName                 : string;       // unique name
      const AFileExtensions       : string = '';  // comma separated list
      const ALineCommentTag       : string = '';
      const ABlockCommentStartTag : string = '';
      const ABlockCommentEndTag   : string = '';
      const ACodeFormatter        : ICodeFormatter = nil;
      const ADescription          : string = '';  // highlighter description
      const ALayoutFileName       : string = ''   // only for TSynUNIHighlighter
    ); virtual;

    // public properties
    { The TCollectionItem decendant class of the collection items. }
    property ItemClass;

    { Provides indexed access to the list of collection items. }
    property Items[Index: Integer]: THighlighterItem
      read GetItem write SetItem; default;

    property ItemsByName[const AName: string]: THighlighterItem
      read GetItemByName write SetItemByName;

    property FileFilter: string
      read GetFileFilter;
  end;

implementation

uses
  Forms, StrUtils, Dialogs, FileUtil,

  ts.Components.UniHighlighter;

{$region 'THighlighterEnumerator' /fold}
function THighlighters.THighlighterEnumerator.GetCurrent: THighlighterItem;
begin
  Result := FHighlighters[FPosition];
end;

constructor THighlighters.THighlighterEnumerator.Create(AHighlighters: THighlighters);
begin
  FHighlighters := AHighlighters;
  FPosition := -1;
end;

function THighlighters.THighlighterEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FHighlighters.Count;
end;
{$endregion}

{$region 'THighlighters' /fold}
{$region 'construction and destruction' /fold}

constructor THighlighters.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, THighlighterItem);
end;

{$endregion}

{$region 'property access mehods' /fold}
function THighlighters.GetItem(Index: Integer): THighlighterItem;
begin
  Result := inherited Items[Index] as THighlighterItem;
end;

function THighlighters.GetFileFilter: string;
var
  S: string;
  HI: THighlighterItem;
begin
  S := '';
  for HI in Self do
  begin
    if HI.DefaultFilter <> '' then
      S := S + HI.DefaultFilter + '|';
  end;
  Result := S;
end;

procedure THighlighters.SetItem(Index: Integer; const Value: THighlighterItem);
begin
  Items[Index].Assign(Value);
end;

function THighlighters.GetItemByName(const AName: string): THighlighterItem;
begin
  Result := Find(AName);
  if not Assigned(Result) then
    Result := Find('None');
end;

procedure THighlighters.SetItemByName(const AName: string; const AValue: THighlighterItem);
var
  Item: THighlighterItem;
begin
  Item := Find(AName);
  if Assigned(Item) then
    Item.Assign(AValue);
end;
{$endregion}

{$region 'protected methods' /fold}
{ Overridden method from TCollection to make any necessary changes when the
  items in the collection change. This method is called automatically when an
  update is issued.
  Item = Item that changed. If the Item parameter is nil, then the change
    affects more than one item in the collection }

procedure THighlighters.Update(AItem: TCollectionItem);
begin
  // Make necessary adjustments when items in the collection change
  // Update gets called from TCollection.Changed.
end;

{ Responds when items are added to or removed from the collection. }

procedure THighlighters.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  // The actions that can be used to respond to are:
  //   - cnAdded      : an item was just added to the collection
  //   - cnExtracting : an item is about to be removed from the collection (but
  //                    not freed)
  //   - cnDeleting   : an item is about to be removed from the collection and
  //                    then freed
end;
{$endregion}

{$region 'public methods' /fold}
{ Adds a new THighlighterItem instance to the THighlighters
  collection. }

function THighlighters.Add: THighlighterItem;
begin
  Result := inherited Add as THighlighterItem;
end;

{ Inserts a new THighlighterItem instance to the THighlighters
  collection before position specified with Index. }

function THighlighters.Insert(Index: Integer): THighlighterItem;
begin
  Result := inherited Insert(Index) as THighlighterItem;
end;

{ Constructs a unique itemname for a new collection item. }

procedure THighlighters.SetItemName(Item: TCollectionItem);
begin
  // The Insert method calls SetItemName to initialize the Name property of items
  // when it inserts them into the collection. This overridden version provides
  // collection items with default names.
  THighlighterItem(Item).Name :=
    Copy(Item.ClassName, 2, Length(Item.ClassName)) + IntToStr(Item.ID + 1);
end;

function THighlighters.Owner: TComponent;
var
  AOwner: TPersistent;
begin
  AOwner := inherited Owner;
  if AOwner is TComponent then
    Result := TComponent(AOwner)
  else
    Result := nil;
end;

function THighlighters.AsString: string;
var
  I: Integer;
  S: string;
begin
  S := '';
  for I :=  0 to Count - 1 do
  begin
    S := S + LineEnding + Items[I].AsString;
  end;
  Result := S;
end;

function THighlighters.Exists(const AName: string): Boolean;
begin
  Result := Assigned(Find(AName));
end;

function THighlighters.GetEnumerator: THighlighterEnumerator;
begin
  Result := THighlighterEnumerator.Create(Self);
end;

function THighlighters.IndexOf(const AName: string): Integer;
var
  I: Integer;
  B: Boolean;
begin
  I := 0;
  B := False;
  while not B and (I < Count) do
  begin
    B := SameText(Items[I].Name, AName);
    if not B then
      Inc(I);
  end;
  if B then
    Result := I
  else
    Result := -1;
end;

{ The FindItemID method returns the item in the collection whose ID property
    is passed to it as a parameter. If no item has the specified ID, FindItemID
    returns nil. }

function THighlighters.FindItemID(ID: Integer): THighlighterItem;
begin
  Result := inherited FindItemID(ID) as THighlighterItem;
end;

function THighlighters.Find(const AName: string): THighlighterItem;
var
  I: Integer;
begin
  I := IndexOf(AName);
  if I < 0 then
    Result := nil
  else
    Result := Items[I];
end;

{ Finds the corresponding highlighteritem for a given file extension. }

function THighlighters.FindHighlighterForFileType(const AFileExt: string): THighlighterItem;
var
  I  : Integer;
  HL : TSynCustomHighlighter;
  S  : string;
begin
  Result := nil;
  S := LowerCase(AFileExt);
  for I := 0 to Count - 1 do
  begin
    HL :=  Items[I].SynHighlighter;
    if Assigned(HL) then
    begin
      if IsWordPresent(S, Items[I].FileExtensions, [','])
        or IsWordPresent(S, HL.DefaultFilter, [',','.', ';']) then
      begin
        Result := Items[I];
      end;
    end;
  end;
end;

{ Registers a new highlighter or updates an exiting one if the corresponding
  properties are not assigned yet. }

procedure THighlighters.RegisterHighlighter(ASynHighlighterClass:
  TSynHighlighterClass; ASynHighlighter: TSynCustomHighlighter;
  const AName: string; const AFileExtensions: string;
  const ALineCommentTag: string; const ABlockCommentStartTag: string;
  const ABlockCommentEndTag: string; const ACodeFormatter: ICodeFormatter;
  const ADescription: string; const ALayoutFileName: string);
var
  HI : THighlighterItem;
begin
  //Logger.Send('RegisterHighlighter', AName);
  //Logger.Send('Layout', ALayoutFileName);
  HI := Find(AName);
  if not Assigned(HI) then
  begin
    HI := Add;
  end;
  HI.Name := AName;
  if ADescription <> '' then
    HI.Description := ADescription;
  HI.SynHighlighterClass  := ASynHighlighterClass;
  HI.CodeFormatter        := ACodeFormatter;
  if HI.LineCommentTag = '' then
    HI.LineCommentTag := ALineCommentTag;
  if HI.BlockCommentStartTag = '' then
    HI.BlockCommentStartTag := ABlockCommentStartTag;
  if HI.BlockCommentEndTag = '' then
    HI.BlockCommentEndTag   := ABlockCommentEndTag;
  if HI.LayoutFileName = '' then
    HI.LayoutFileName := ALayoutFileName;
  if HI.FileExtensions = '' then
    HI.FileExtensions := AFileExtensions;
  HI.InitSynHighlighter(ASynHighlighter);
  if FileExistsUTF8(ALayoutFileName) and (ASynHighlighterClass = TSynUniSyn) then
  begin
    if Assigned(HI.SynHighlighter) then
      TSynUniSyn(HI.SynHighlighter).LoadFromFile(ALayoutFileName);
  end;
end;
{$endregion}
{$endregion}

{$region 'THighlighterItem' /fold}

{$region 'construction and destruction' /fold}
constructor THighlighterItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FFileExtensions := TStringList.Create;
  FFileExtensions.Duplicates := dupIgnore;
  FFileExtensions.Sorted     := True;
  FSmartSelectionTags := TCodeTags.Create(Self);
end;

procedure THighlighterItem.BeforeDestruction;
begin
  FCodeFormatter := nil;
  if Assigned(FSynHighlighter) then
    FreeAndNil(FSynHighlighter);
  FreeAndNil(FFileExtensions);
  FreeAndNil(FSmartSelectionTags);
  inherited BeforeDestruction;
end;
{$endregion}

{$region 'property access mehods' /fold}
function THighlighterItem.GetCollection: THighlighters;
begin
  Result := inherited Collection as THighlighters;
end;

function THighlighterItem.GetDefaultFilter: string;
begin
  Result := FDefaultFilter;
end;

procedure THighlighterItem.SetDefaultFilter(AValue: string);
begin
  if AValue <> DefaultFilter then
  begin
    FDefaultFilter := AValue;
  end;
end;

function THighlighterItem.GetFileExtensions: string;
begin
  Result := FFileExtensions.CommaText;
end;

procedure THighlighterItem.SetCollection(const Value: THighlighters);
begin
  inherited Collection := Value;
end;

procedure THighlighterItem.SetFileExtensions(AValue: string);
begin
  if AValue <> FileExtensions then
  begin
    FFileExtensions.CommaText := AValue;
  end;
end;

procedure THighlighterItem.SetFormatterSupport(const AValue: Boolean);
begin
  if AValue <> FormatterSupport then
  begin
    FFormatterSupport := AValue;
  end;
end;

procedure THighlighterItem.SetSmartSelectionTags(AValue: TCodeTags);
begin
  FSmartSelectionTags.Assign(AValue);
end;

procedure THighlighterItem.SetSynHighlighter(AValue: TSynCustomHighlighter);
begin
  FSynHighlighter.Assign(AValue);
  FSynHighlighter.SetSubComponent(True);
end;

procedure THighlighterItem.SetSynHighlighterClass(AValue: TSynHighlighterClass);
begin
  if AValue <> SynHighlighterClass then
  begin
    FSynHighlighterClass := AValue;
  end;
end;

function THighlighterItem.GetDisplayName: string;
begin
  Result := Name;
end;
{$endregion}

{$region 'public methods' /fold}
procedure THighlighterItem.Assign(Source: TPersistent);
var
  HLI: THighlighterItem;
begin
  if (Source <> Self) and (Source is THighlighterItem) then
  begin
    if Assigned(Collection) then
      Collection.BeginUpdate;
    try
      HLI := THighlighterItem(Source);
      SynHighlighterClass  := HLI.SynHighlighterClass;
      SynHighlighter.Assign(HLI.SynHighlighter);
      Name                 := HLI.Name;
      Description          := HLI.Description;
      LayoutFileName       := HLI.LayoutFileName;
      BlockCommentEndTag   := HLI.BlockCommentEndTag;
      BlockCommentStartTag := HLI.BlockCommentStartTag;
      LineCommentTag       := HLI.LineCommentTag;
      FileExtensions       := HLI.FileExtensions;
      SmartSelectionTags   := HLI.SmartSelectionTags;
      DefaultFilter        := HLI.DefaultFilter;
      FormatterSupport     := HLI.FormatterSupport;
    finally
      if Assigned(Collection) then
        Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function THighlighterItem.AsString: string;
const
  DATA =
    'SynHighlighter = %s' + #13#10 +
    'Name           = %s' + #13#10 +
    'Description    = %s' + #13#10 +
    'LayoutFileName = %s';
begin
  Result := Format(DATA, [SynHighlighter.ClassName, Name, Description, LayoutFileName]);
end;

procedure THighlighterItem.InitSynHighlighter(ASynHighlighter: TSynCustomHighlighter);
begin
  if not Assigned(SynHighlighter) and Assigned(SynHighlighterClass) then
  begin
    FSynHighlighter := SynHighlighterClass.Create(nil);
    // We need to call SetSubComponent to indicate that this component is a
    // subcomponent.
    // A subcomponent is a component whose Owner is a component other than the
    // form or data module in which it resides. Unless such a component calls
    // SetSubComponent with IsSubComponent set to True, its published properties
    // will not be persisted.
    FSynHighlighter.SetSubComponent(True);
  end;
  if Assigned(ASynHighlighter) then
    FSynHighlighter.Assign(ASynHighlighter);
end;

procedure THighlighterItem.Reload;
var
  S: string;
begin
  S := ExtractFilePath(Application.ExeName);
  if FileExists(S + LayoutFileName) and (SynHighlighterClass = TSynUniSyn) then
  begin
    if Assigned(SynHighlighter) then
      TSynUniSyn(SynHighlighter).LoadFromFile(S + LayoutFileName);
  end;
end;
{$endregion}
{$endregion}

end.

