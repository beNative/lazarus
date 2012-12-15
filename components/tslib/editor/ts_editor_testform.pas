{
  Copyright (C) 2012 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts_Editor_Testform;

{$mode delphi}

//*****************************************************************************

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,

  ts_Editor_Interfaces;

//=============================================================================

type
  TfrmTest = class(TForm, IEditorToolView)
    btnExecute: TButton;
    edtInput: TEdit;
  private
    function GetForm: TForm;
    function GetManager: IEditorManager;
    function GetName: string;

  protected
    procedure UpdateView;

    property Manager: IEditorManager
      read GetManager;

    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);

    property Visible: Boolean
      read GetVisible write SetVisible;

    property Name: string
      read GetName;

    property Form: TForm
      read GetForm;

  public

  end; 

//*****************************************************************************

implementation

{$R *.lfm}

function TfrmTest.GetForm: TForm;
begin
  Result := Self;
end;

function TfrmTest.GetManager: IEditorManager;
begin
  Result := Owner as IEditorManager;
end;

function TfrmTest.GetName: string;
begin
  Result := inherited Name;
end;

procedure TfrmTest.UpdateView;
begin
  //
end;

function TfrmTest.GetVisible: Boolean;
begin
  Result := inherited Visible;
end;

procedure TfrmTest.SetVisible(AValue: Boolean);
begin
  inherited SetVisible(AValue);
end;

end.

