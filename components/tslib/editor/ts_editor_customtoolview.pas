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

unit ts_Editor_CustomToolView;

{$mode delphi}

//*****************************************************************************

interface

uses
  Forms,

  ts_Editor_Interfaces;

type
  TCustomEditorToolView = class(TForm, IEditorToolView, IClipboardCommands)
  strict private
    // this flag is set when there are pending updates.
    FUpdate: Boolean;

    function GetManager: IEditorManager;
    function GetSettings: IEditorSettings;
    function GetUpdate: Boolean;
    function GetView: IEditorView;
    function GetViews: IEditorViews;
    procedure SetUpdate(AValue: Boolean);

    procedure EditorSettingsChanged(Sender: TObject);

  strict protected
    function GetForm: TForm;
    function GetName: string;
    function GetVisible: Boolean;

    procedure Modified; virtual;
    procedure UpdateView; virtual;
    procedure SettingsChanged; virtual;

    { IClipboardCommands }
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure Undo;
    procedure Redo;

    property Update: Boolean
      read GetUpdate write SetUpdate;

    property Manager: IEditorManager
      read GetManager;

    property Settings: IEditorSettings
      read GetSettings;

    property View: IEditorView
      read GetView;

    property Views: IEditorViews
      read GetViews;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

//*****************************************************************************

implementation

{$R *.lfm}

uses
{$ifdef windows}
  Windows,
{$endif}
  LCLIntf, LMessages;

{$region 'construction and destruction' /fold}
//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TCustomEditorToolView.AfterConstruction;
begin
  inherited AfterConstruction;
  Manager.Settings.AddEditorSettingsChangedHandler(EditorSettingsChanged);
end;

procedure TCustomEditorToolView.BeforeDestruction;
begin
  if Assigned(Settings) then
    Settings.RemoveEditorSettingsChangedHandler(EditorSettingsChanged);
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

function TCustomEditorToolView.GetUpdate: Boolean;
begin
  Result := FUpdate;
end;

procedure TCustomEditorToolView.SetUpdate(AValue: Boolean);
begin
  if AValue <> Update then
  begin
    FUpdate := AValue;
  end;
end;

function TCustomEditorToolView.GetView: IEditorView;
begin
  Result := Owner as IEditorView;
end;

function TCustomEditorToolView.GetViews: IEditorViews;
begin
  Result := Owner as IEditorViews;
end;

function TCustomEditorToolView.GetForm: TForm;
begin
  Result := Self;
end;

function TCustomEditorToolView.GetName: string;
begin
  Result := inherited Name;
end;

function TCustomEditorToolView.GetVisible: Boolean;
begin
  Result := inherited Visible;
end;

function TCustomEditorToolView.GetManager: IEditorManager;
begin
  Result := Owner as IEditorManager;
end;

function TCustomEditorToolView.GetSettings: IEditorSettings;
begin
  Result := Owner as IEditorSettings;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************
{$endregion}

{$region 'event handlers' /fold}
//*****************************************************************************
// event handlers                                                        BEGIN
//*****************************************************************************

procedure TCustomEditorToolView.EditorSettingsChanged(Sender: TObject);
begin
  SettingsChanged;
end;

//*****************************************************************************
// event handlers                                                          END
//*****************************************************************************
{$endregion}

{$region 'protected methods' /fold}
//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

procedure TCustomEditorToolView.UpdateView;
begin
  // to be overridden
end;

procedure TCustomEditorToolView.Modified;
begin
  FUpdate := True;
end;

{ Responds to changes in the global settings. }

procedure TCustomEditorToolView.SettingsChanged;
begin
  // to be overridden
end;

procedure TCustomEditorToolView.Cut;
begin
  PostMessage(GetFocus, LM_CUT, 0, 0);
end;

procedure TCustomEditorToolView.Copy;
begin
  PostMessage(GetFocus, LM_COPY, 0, 0);
end;

procedure TCustomEditorToolView.Paste;
begin
  PostMessage(GetFocus, LM_PASTE, 0, 0);
end;

procedure TCustomEditorToolView.Undo;
begin
{$ifdef windows}
  PostMessage(GetFocus, WM_UNDO, 0, 0);
{$endif}
end;

procedure TCustomEditorToolView.Redo;
begin
{$ifdef windows}
  PostMessage(GetFocus, WM_UNDO, 1, 0);
{$endif}
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************
{$endregion}

end.

