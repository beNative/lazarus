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

unit ts_Editor_CodeFilter;

{$mode delphi}

//*****************************************************************************

interface

uses
  Classes, SysUtils, FileUtil, db,

  memds, BufDataset;

type
  TdmEditorCodeFilter = class(TDataModule)
    dsCode: TBufDataset;
    dsCodeLines : TMemDataset;
    procedure dsCodeLinesFilterRecord(DataSet: TDataSet; var Accept: Boolean);

  private
    FFilter: string;
    function GetDataSet: TDataSet;
    procedure SetFilter(AValue: string);
  public
    procedure AfterConstruction; override;
    procedure Build(const AString: string);

    property Filter: string
      read FFilter write SetFilter;

    property DataSet: TDataSet
      read GetDataSet;
  end; 

//*****************************************************************************

implementation

{$R *.lfm}

uses
  StrUtils;

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

function TdmEditorCodeFilter.GetDataSet: TDataSet;
begin
  Result := dsCodeLines;
end;

procedure TdmEditorCodeFilter.SetFilter(AValue: string);
begin
  if FFilter = AValue then exit;
  FFilter := AValue;
end;

procedure TdmEditorCodeFilter.AfterConstruction;
begin
  inherited AfterConstruction;
  //dsCodeLines.Active := True;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

procedure TdmEditorCodeFilter.dsCodeLinesFilterRecord(DataSet: TDataSet; var Accept: Boolean);
begin
  if FFilter <> '' then
    Accept := AnsiMatchText(DataSet.FieldByName('fdLineText').AsString, FFilter);
end;

procedure TdmEditorCodeFilter.Build(const AString: string);
var
  SL : TStringList;
  I  : Integer;
begin
  SL := TStringList.Create;
  try

    //dsCodeLines.Clear;
    SL.Text := AString;
    for I := 0 to SL.Count - 1 do
    begin
      dsCodeLines.Append;
      dsCodeLines.FieldByName('fdLineIndex').AsInteger := I;
      dsCodeLines.FieldByName('fdID').AsInteger := I;
      dsCodeLines.FieldByName('fdLineText').AsString := SL[I];
      dsCodeLines.Post;
    end;
  finally
    FreeAndNil(SL);
  end;
  dsCodeLines.Filtered := True;
end;

end.

