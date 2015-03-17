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

unit SnippetSource.Modules.Data;

{$mode objfpc}{$H+}

interface

uses
  Classes, DB, Graphics, ImgList, Controls,

 StrHolder,
  // zeos
  ZConnection, ZDataset, ZSqlUpdate, ZSequence, ZSqlMetadata,

  SnippetSource.Interfaces;

{
  REMARKS:
    Updates on multi-table queries don't work well both with ZEOS (with
    TZUpdateSQL and SQLDB (providerflags and update queries) components if used
    in combination with SQLite (Well, I couldn't manage to get it work properly
    at least).

    It is important to select only from one table in qryMain as long that we
    don't have a good solution to address this problem.
}

type
  TdmMain = class(TDataModule, IConnection,
                               ISnippet,
                               IDataSet,
                               ILookup,
                               IGlyphs,
                               IHighlighters,
                               ISQLiteSettings)
    conMain          : TZConnection;
    dscLookup        : TDatasource;
    imlMain          : TImageList;
    imlGlyphs        : TImageList;
    qryNodeType      : TZQuery;
    qryHighlighter   : TZQuery;
    qryLookup        : TZQuery;
    qryMain          : TZQuery;
    qryGlyph         : TZQuery;
    shDataBaseScript : TStrHolder;

    procedure conMainAfterConnect(Sender: TObject);
    procedure dscLookupDataChange(Sender: TObject; Field: TField);
    procedure qryHighlighterAfterPost(DataSet: TDataSet);
    procedure qryMainBeforePost(DataSet: TDataSet);

  private
    function GetGlyphList: TImageList;
    function GetHighlighterDataSet: TDataSet;
    function GetImageList: TImageList;

    function GetActive: Boolean;
    { ISnippet }
    function GetComment: string;
    function GetCommentRTF: string;
    function GetDataSet: TDataSet;
    function GetDateCreated: TDateTime;
    function GetDateModified: TDateTime;
    function GetFoldLevel: Integer;
    function GetFoldState: string;
    function GetGlyphDataSet: TDataSet;
    function GetHighlighter: string;
    function GetID: Integer;
    function GetImageIndex: Integer;
    function GetNodeName: string;
    function GetNodePath: string;
    function GetNodeTypeID: Integer;
    function GetParentID: Integer;
    function GetRecordCount: Integer;
    function GetText: string;
    procedure SetActive(AValue: Boolean);
    procedure SetComment(AValue: string);
    procedure SetCommentRTF(AValue: string);
    procedure SetDateCreated(AValue: TDateTime);
    procedure SetDateModified(AValue: TDateTime);
    procedure SetFoldLevel(AValue: Integer);
    procedure SetFoldState(AValue: string);
    procedure SetHighlighter(AValue: string);
    procedure SetImageIndex(AValue: Integer);
    procedure SetNodeName(AValue: string);
    procedure SetNodePath(AValue: string);
    procedure SetNodeTypeID(AValue: Integer);
    procedure SetParentID(AValue: Integer);
    procedure SetText(AValue: string);
    function GetFileName: string;
    procedure SetFileName(AValue: string);

    { ISQLiteSettings }
    function GetAutomaticIndex: Boolean;
    function GetAutoVacuum: Boolean;
    procedure SetAutomaticIndex(AValue: Boolean);
    procedure SetAutoVacuum(AValue: Boolean);

    procedure SetValue(const AFieldName: string; const AValue: Variant);
    function AsString(const AFieldName: string): string;
    function AsInteger(const AFieldName: string): Integer;
    procedure LoadGlyphs;
    procedure LoadBitmap(ADataSet: TDataSet; const AFieldName: string;
      ABitmap: TBitmap);

    { IGlyphs }
    property ImageList: TImageList
      read GetImageList;
    property GlyphList: TImageList
      read GetGlyphList;

    { IHighlighters }
    property HighlighterDataSet: TDataSet
      read GetHighlighterDataSet;
    function ILookup.GetDataSet = GetLookupDataSet;
   function GetLookupDataSet: TDataSet;

  protected
    procedure BuildImageList(ADataSet: TDataSet; AImageList: TCustomImageList);

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    { IConnection }
    function Execute(const ASQL: string): Boolean;
    procedure CreateNewDatabase;
    procedure BeginUpdate;
    procedure EndUpdate;

    { IConnection }
    property FileName: string
      read GetFileName write SetFileName;

    { ILookup }
    procedure Lookup(
      const ASearchString    : string;
            ASearchInText    : Boolean;
            ASearchInName    : Boolean;
            ASearchInComment : Boolean
    );

    function Post: Boolean;
    function Append: Boolean;
    function Edit: Boolean;

    procedure DisableControls;
    procedure EnableControls;

    property Active: Boolean
      read GetActive write SetActive;

    property RecordCount: Integer
      read GetRecordCount;

    property GlyphDataSet: TDataSet
      read GetGlyphDataSet;

    property DataSet: TDataSet
      read GetDataSet;

    property LookupDataSet: TDataSet
      read GetLookupDataSet;

    { ISnippet }
    property Comment: string
      read GetComment write SetComment;
    property CommentRTF: string
      read GetCommentRTF write SetCommentRTF;
    property DateCreated: TDateTime
      read GetDateCreated write SetDateCreated;
    property DateModified: TDateTime
      read GetDateModified write SetDateModified;
    property FoldLevel: Integer
      read GetFoldLevel write SetFoldLevel;
    property FoldState: string
      read GetFoldState write SetFoldState;
    property Highlighter: string
      read GetHighlighter write SetHighlighter;
    property ID: Integer
      read GetID;
    property NodeName: string
      read GetNodeName write SetNodeName;
    property NodePath: string
      read GetNodePath write SetNodePath;
    property NodeTypeID: Integer read
      GetNodeTypeID write SetNodeTypeID;
    property ParentID: Integer
      read GetParentID write SetParentID;
    property Text: string
      read GetText write SetText;
    property ImageIndex: Integer
      read GetImageIndex write SetImageIndex;

    { ISQLiteSettings }
    property AutoVacuum: Boolean
      read GetAutoVacuum write SetAutoVacuum;

    property AutomaticIndex: Boolean
      read GetAutomaticIndex write SetAutomaticIndex;
  end;

implementation

{$R *.lfm}

uses
  Variants, Forms, SysUtils, Dialogs, Math;

{$region 'construction and destruction' /fold}
procedure TdmMain.AfterConstruction;
begin
  inherited AfterConstruction;
  conMain.Connect;
  //conMain.ExecuteDirect('PRAGMA synchronous = 1;'); // speeds up inserts
  conMain.ExecuteDirect('PRAGMA synchronous = 0;');
  conMain.ExecuteDirect('PRAGMA journal_mode = WAL;');
  conMain.ExecuteDirect('PRAGMA journal_mode = OFF;');
  conMain.ExecuteDirect('PRAGMA locking_mode = EXCLUSIVE;');
  conMain.ExecuteDirect('PRAGMA temp_store = MEMORY;');
  conMain.ExecuteDirect('PRAGMA count_changes = OFF;');
  conMain.ExecuteDirect('PRAGMA PAGE_SIZE = 4096;');
  conMain.ExecuteDirect('PRAGMA default_cache_size = 700000;');
  conMain.ExecuteDirect('PRAGMA cache_size = 700000;');
  qryHighlighter.Active := True;
  qryNodeType.Active    := True;
  LoadGlyphs;
  BuildImageList(GlyphDataSet, imlGlyphs);
end;

procedure TdmMain.BeforeDestruction;
begin
  conMain.ExecuteDirect('Vacuum;');
  inherited BeforeDestruction;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************
{$endregion}

{$region 'property access mehods' /fold}
//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

function TdmMain.GetDataSet: TDataSet;
begin
  Result := qryMain;
end;

function TdmMain.GetDateCreated: TDateTime;
begin
  Result := DataSet.FieldByName('DateCreated').AsDateTime;
end;

function TdmMain.GetDateModified: TDateTime;
begin
  Result := DataSet.FieldByName('DateModified').AsDateTime;
end;

function TdmMain.GetFoldLevel: Integer;
begin
  Result := AsInteger('FoldLevel');
end;

function TdmMain.GetFoldState: string;
begin
  Result := AsString('FoldState');
end;

function TdmMain.GetGlyphDataSet: TDataSet;
begin
  Result := qryGlyph;
end;

function TdmMain.GetHighlighter: string;
begin
  Result := VarToStrDef(qryHighlighter.Lookup('ID', DataSet['HighlighterID'], 'Code'), '');
end;

function TdmMain.GetNodeName: string;
begin
  Result := AsString('NodeName');
end;

function TdmMain.GetNodePath: string;
begin
  Result := AsString('NodePath');
end;

function TdmMain.GetNodeTypeID: Integer;
begin
  Result := AsInteger('NodeTypeID');
end;

function TdmMain.GetParentID: Integer;
begin
  Result := AsInteger('ParentID');
end;

function TdmMain.GetRecordCount: Integer;
begin
  if DataSet.Active then
    Result := DataSet.RecordCount
  else
    Result := 0;
end;

function TdmMain.GetText: string;
begin
  Result := AsString('Text');
end;

procedure TdmMain.SetActive(AValue: Boolean);
begin
  DataSet.Active := AValue;
end;

function TdmMain.GetComment: string;
begin
  Result := AsString('Comment');
end;

procedure TdmMain.SetComment(AValue: string);
begin
  SetValue('Comment', AValue);
end;

function TdmMain.GetCommentRTF: string;
begin
  Result := AsString('CommentRTF');
end;

procedure TdmMain.SetCommentRTF(AValue: string);
begin
  SetValue('CommentRTF', AValue);
end;

procedure TdmMain.SetDateCreated(AValue: TDateTime);
begin
  SetValue('DateCreated', AValue);
end;

procedure TdmMain.SetDateModified(AValue: TDateTime);
begin
  SetValue('DateModified', AValue);
end;

procedure TdmMain.SetFoldLevel(AValue: Integer);
begin
  SetValue('FoldLevel', AValue);
end;

procedure TdmMain.SetFoldState(AValue: string);
begin
  SetValue('FoldState', AValue);
end;

procedure TdmMain.SetHighlighter(AValue: string);
begin
  SetValue('HighlighterID', qryHighlighter.Lookup('Code', AValue, 'ID'));
end;

procedure TdmMain.SetImageIndex(AValue: Integer);
begin
  SetValue('ImageIndex', AValue);
end;

function TdmMain.GetID: Integer;
begin
  Result := AsInteger('ID');
end;

function TdmMain.GetImageIndex: Integer;
begin
  Result := AsInteger('ImageIndex');
end;

function TdmMain.GetLookupDataSet: TDataSet;
begin
  Result := qryLookup;
end;

procedure TdmMain.SetNodeName(AValue: string);
begin
  SetValue('NodeName', AValue);
end;

procedure TdmMain.SetNodePath(AValue: string);
begin
  SetValue('NodePath', AValue);
end;

procedure TdmMain.SetNodeTypeID(AValue: Integer);
begin
  SetValue('NodeTypeID', AValue);
end;

procedure TdmMain.SetParentID(AValue: Integer);
begin
  SetValue('ParentID', AValue);
end;

procedure TdmMain.SetText(AValue: string);
begin
  SetValue('Text', AValue);
end;

function TdmMain.GetFileName: string;
begin
  Result := conMain.Database;
end;

procedure TdmMain.SetFileName(AValue: string);
begin
  if AValue <> FileName then
  begin
    conMain.Connected := False;
    conMain.Database := AValue;
    conMain.Connected := True;
  end;
end;

function TdmMain.GetAutoVacuum: Boolean;
begin

end;

procedure TdmMain.SetAutomaticIndex(AValue: Boolean);
begin
  conMain.ExecuteDirect(Format('PRAGMA automatic_index = %d;', [IfThen(AValue, 1, 0)]));
end;

function TdmMain.GetAutomaticIndex: Boolean;
begin
  //Result := QueryLookup(conMain, 'PRAGMA automatic_index;') = 1;
end;

procedure TdmMain.SetAutoVacuum(AValue: Boolean);
begin

end;

function TdmMain.GetGlyphList: TImageList;
begin
  Result := imlGlyphs;
end;

function TdmMain.GetHighlighterDataSet: TDataSet;
begin
  Result := qryHighlighter;
end;

function TdmMain.GetImageList: TImageList;
begin
  Result := imlGlyphs;
end;

function TdmMain.GetActive: Boolean;
begin
  Result := DataSet.Active;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************
{$endregion}

{$region 'event handlers' /fold}
//*****************************************************************************
// event handlers                                                        BEGIN
//*****************************************************************************

procedure TdmMain.dscLookupDataChange(Sender: TObject; Field: TField);
begin
  //if DataSet.Active then
  //begin
  //  DataSet.Locate(
  //    'ID',
  //    VarArrayOf([qryLookup.FieldByName('ID').Value]), [loCaseInsensitive]
  //  );
  //end;
end;

procedure TdmMain.qryHighlighterAfterPost(DataSet: TDataSet);
begin
  LoadGlyphs;
end;

procedure TdmMain.qryMainBeforePost(DataSet: TDataSet);
begin
  if NodeTypeID = 1 then
    SetValue('ImageIndex', qryNodeType.Lookup('ID', NodeTypeID, 'ImageIndex'))
  else
    SetValue('ImageIndex', qryHighlighter.Lookup('ID', DataSet['HighlighterID'], 'ImageIndex'))
end;

procedure TdmMain.conMainAfterConnect(Sender: TObject);
begin
  qryGlyph.Active := True;
end;

//*****************************************************************************
// event handlers                                                          END
//*****************************************************************************
{$endregion}

{$region 'private methods' /fold}
//*****************************************************************************
// private methods                                                       BEGIN
//*****************************************************************************

procedure TdmMain.SetValue(const AFieldName: string; const AValue: Variant);
begin
  if DataSet.Active then
  begin
    if not (DataSet.State in dsEditModes) then
      DataSet.Edit;
    DataSet[AFieldName] := AValue;
  end;
end;

function TdmMain.AsString(const AFieldName: string): string;
begin
  Result := VarToStrDef(DataSet[AFieldName], '');
end;

function TdmMain.AsInteger(const AFieldName: string): Integer;
begin
  if DataSet.Active and not VarIsNull(DataSet[AFieldName]) then
    Result := DataSet[AFieldName]
  else
    Result := 0;
end;

//*****************************************************************************
// private methods                                                         END
//*****************************************************************************
{$endregion}

{$region 'protected methods' /fold}
//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

procedure TdmMain.LoadGlyphs;
const
  SQL = 'select' + #13#10 +
        '  ID,' + #13#10 +
        '  Image,' + #13#10 +
        '  ImageIndex' + #13#10 +
        'from' + #13#10 +
        '  Glyph' + #13#10 +
        'where' + #13#10 +
        '  ImageIndex is not null' + #13#10 +
        'order by' + #13#10 +
        '  ImageIndex';
var
  Q  : TZQuery;
begin
  Q := TZQuery.Create(nil);
  try
    Q.Connection := conMain;
    Q.SQL.Text := SQL;
    Q.Active := True;
    BuildImageList(Q, imlMain);
  finally
    FreeAndNil(Q);
  end;
end;

procedure TdmMain.LoadBitmap(ADataSet: TDataSet; const AFieldName: string; ABitmap: TBitmap);
var
  MS   : TBytesStream;
  Image: TPicture;
  //Pic: TSynPicture;
begin
  MS := TBytesStream.Create;
  try
    Image := TPicture.Create;
    try

      TGraphicField(ADataSet.FieldByName(AFieldName)).BlobType := ftGraphic;
      TGraphicField(ADataSet.FieldByName(AFieldName)).SaveToStream(MS);
      if MS.Size > 0 then
      begin
        MS.Position := 0;
        try
         Image.LoadFromStream(MS);
      //LoadFromStream(MS);
//        if Pic.GetImageFormat = gptTIF then
        ABitmap.Assign(Image.Bitmap);

        except
        end;
      end;
    finally
      FreeAndNil(Image);
    end;
  finally
    FreeAndNil(MS);
  end;
//Pic.LoadFromStream(aStream); // will load bmp/gif/tiff/jpeg/png content
//AImage.Picture.Graphic := Pic;
end;

procedure TdmMain.BuildImageList(ADataSet: TDataSet; AImageList: TCustomImageList);
var
  BM: TBitmap;
  I  : Integer;
begin
  BM := TBitmap.Create;
  //AImageList.Clear;
  AImageList.BeginUpdate;
  I := 0;
  try
    //ADataSet.First;
    //while not ADataSet.Eof do
    //begin
    //  LoadBitmap(ADataSet, 'Image', BM);
    //  if not BM.Empty then
    //  begin
    //    AImageList.AddMasked(BM, clNone);
    //  end;
    //
    //  ADataSet.Next;
    //end;
  finally
    AImageList.EndUpdate;
    FreeAndNil(BM);
  end;
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************
{$endregion}

{$region 'public methods' /fold}
//*****************************************************************************
// public methods                                                        BEGIN
//*****************************************************************************

{ Execute SQL string }

function TdmMain.Execute(const ASQL: string): Boolean;
begin
  Result := conMain.ExecuteDirect(ASQL);
end;

procedure TdmMain.CreateNewDatabase;
begin
  Execute(shDataBaseScript.Strings.Text);
  qryHighlighter.Active := True;
  DataSet.Active := True;
end;

procedure TdmMain.BeginUpdate;
begin
  qryMain.CachedUpdates := True;
end;

procedure TdmMain.EndUpdate;
begin
  Post;
  qryMain.ApplyUpdates;
  qryMain.CachedUpdates := False;
end;

procedure TdmMain.Lookup(const ASearchString: string; ASearchInText: Boolean;
  ASearchInName: Boolean; ASearchInComment : Boolean);
const
  LOOKUP_SQL =
    'select' + #13#10 +
    '  s.ID,' + #13#10 +
    '  s.NodeName,' + #13#10 +
    '  h.Code as Highlighter' + #13#10 +
    'from' + #13#10 +
    '  Snippet s' + #13#10 +
    '  inner join NodeType nt' + #13#10 +
    '    on (s.NodeTypeId = nt.Id)' + #13#10 +
    '  left outer join Highlighter h' + #13#10 +
    '    on (s.HighlighterID = h.Id)' + #13#10 +
    'where' + #13#10 +
    '  nt.Id = 2' + #13#10 +
    '  and h.Code in (''PAS'')' + #13#10 +
    '  and (' + #13#10 +
    '    (1 = %d and Text like ''%%%s%%'')' + #13#10 +
    '    or (1 = %d and NodeName like ''%%%s%%'')' + #13#10 +
    '    or (1 = %d and Comment like ''%%%s%%'')' + #13#10 +
    '  )';
var
  S: string;
begin
  DataSet.DisableControls;
  qryLookup.DisableControls;
  qryLookup.Active := False;
  qryLookup.SQL.Text := Format(
    LOOKUP_SQL,
    [IfThen(ASearchInText, 1, 0), ASearchString,
     IfThen(ASearchInName, 1, 0), ASearchString,
     IfThen(ASearchInComment, 1, 0), ASearchString
    ]);
  qryLookup.Active := True;
  qryLookup.EnableControls;
  DataSet.EnableControls;
end;

function TdmMain.Post: Boolean;
begin
  if DataSet.State in dsEditModes then
  begin
    DataSet.Post;
    Result := True;
  end
  else
    Result := False;
end;

function TdmMain.Append: Boolean;
begin
  if not (DataSet.State in dsEditModes) then
  begin
    DataSet.Append;
    Result := True;
  end
  else
    Result := False;
end;

function TdmMain.Edit: Boolean;
begin
  if not (DataSet.State in dsEditModes) then
  begin
    DataSet.Edit;
    Result := True;
  end
  else
    Result := False;
end;

procedure TdmMain.DisableControls;
begin
  DataSet.DisableControls;
end;

procedure TdmMain.EnableControls;
begin
  DataSet.EnableControls;
end;

//*****************************************************************************
// public methods                                                          END
//*****************************************************************************
{$endregion}

end.

{
 consistency checks

 select
  *
from
  Snippet s
 where
   s.ParentID <> 0 and
   not exists (select ID from Snippet where ID = s.ParentID)
}

{
uses Consts, DB;

procedure LoadPictureFromBlobField(Field: TBlobField; Dest: TPicture);
var
  Graphic: TGraphic;
  GraphicClass: TGraphicClass;
  Stream: TMemoryStream;
begin
  Graphic := nil;
  Stream := TMemoryStream.Create;
  try
    Field.SaveToStream(Stream);
    if Stream.Size = 0 then
    begin
      Dest.Assign(nil);
      Exit;
    end;
    if not FindGraphicClass(Stream.Memory^, Stream.Size, GraphicClass) then
      raise EInvalidGraphic.Create(SInvalidImage);
    Graphic := GraphicClass.Create;
    Stream.Position := 0;
    Graphic.LoadFromStream(Stream);
    Dest.Assign(Graphic);
  finally
    Stream.Free;
    Graphic.Free;
  end;
end;

uses SysUtils, Classes, Graphics, GIFImg, JPEG, PngImage;

const
  MinGraphicSize = 44; //we may test up to & including the 11th longword

function FindGraphicClass(const Buffer; const BufferSize: Int64;
  out GraphicClass: TGraphicClass): Boolean; overload;
var
  LongWords: array[Byte] of LongWord absolute Buffer;
  Words: array[Byte] of Word absolute Buffer;
begin
  GraphicClass := nil;
  Result := False;
  if BufferSize < MinGraphicSize then Exit;
  case Words[0] of
    $4D42: GraphicClass := TBitmap;
    $D8FF: GraphicClass := TJPEGImage;
    $4949: if Words[1] = $002A then GraphicClass := TWicImage; //i.e., TIFF
    $4D4D: if Words[1] = $2A00 then GraphicClass := TWicImage; //i.e., TIFF
  else
    if Int64(Buffer) = $A1A0A0D474E5089 then
      GraphicClass := TPNGImage
    else if LongWords[0] = $9AC6CDD7 then
      GraphicClass := TMetafile
    else if (LongWords[0] = 1) and (LongWords[10] = $464D4520) then
      GraphicClass := TMetafile
    else if StrLComp(PAnsiChar(@Buffer), 'GIF', 3) = 0 then
      GraphicClass := TGIFImage
    else if Words[1] = 1 then
      GraphicClass := TIcon;
  end;
  Result := (GraphicClass <> nil);
end;

function FindGraphicClass(Stream: TStream;
  out GraphicClass: TGraphicClass): Boolean; overload;
var
  Buffer: PByte;
  CurPos: Int64;
  BytesRead: Integer;
begin
  if Stream is TCustomMemoryStream then
  begin
    Buffer := TCustomMemoryStream(Stream).Memory;
    CurPos := Stream.Position;
    Inc(Buffer, CurPos);
    Result := FindGraphicClass(Buffer^, Stream.Size - CurPos, GraphicClass);
    Exit;
  end;
  GetMem(Buffer, MinGraphicSize);
  try
    BytesRead := Stream.Read(Buffer^, MinGraphicSize);
    Stream.Seek(-BytesRead, soCurrent);
    Result := FindGraphicClass(Buffer^, BytesRead, GraphicClass);
  finally
    FreeMem(Buffer);
  end;
end;

}

