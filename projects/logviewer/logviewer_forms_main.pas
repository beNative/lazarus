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

unit logviewer_forms_main;

{ Viewer for Multilog messages

  Copyright (C) 2006 Luiz Américo Pereira Câmara
  pascalive@bol.com.br

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR actClearMessages PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

{ Modified by Tim Sinaeve }

{$MODE Delphi}

interface
//todo: - Use only one StringGrid for Watches (???)
//      - Optimize Watch update (Cache current values?)

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Grids,
  StdCtrls, ComCtrls, Buttons, Menus, ActnList, RTTIGrids,

  LResources,

  SimpleIPC,

  VirtualTrees,

  SynEdit,

  SearchEdit, ATBinHex,

  ts.Core.Logger,

  ts.Editor.Interfaces,

  LogViewer.WatchList;

type
  TMessageSet = 0..31;

  { TfrmMain }

  TfrmMain = class(TForm)
    {$REGION 'designer controls' /FOLD}
    aclMain              : TActionList;
    actClearMessages     : TAction;
    actException         : TAction;
    actBitmap            : TAction;
    actHeapInfo          : TAction;
    actCustomData        : TAction;
    actSearchFilter      : TAction;
    actStop              : TAction;
    actMethodTraces      : TAction;
    actMemory            : TAction;
    actObject            : TAction;
    actToggleInfo        : TAction;
    actToggleWarning     : TAction;
    actValue             : TAction;
    actError             : TAction;
    actConditional       : TAction;
    actCheckPoint        : TAction;
    actStrings           : TAction;
    actCallStack         : TAction;
    actSelectNone        : TAction;
    actSelectAll         : TAction;
    actSave              : TAction;
    actOpen              : TAction;
    actToggleAlwaysOnTop : TAction;
    btnSelectAlll        : TSpeedButton;
    btnSelectNonen       : TSpeedButton;
    btnStop              : TSpeedButton;
    ComboWatchHistory    : TComboBox;
    edtFilterMessages    : TSearchEdit;
    edtHex               : TATBinHex;
    grdCallStack         : TStringGrid;
    gridLastestWatch     : TStringGrid;
    gridSelectedWatch    : TStringGrid;
    GridWatchHistory     : TStringGrid;
    imgToolbar           : TImageList;
    imgMessages          : TImageList;
    ImgViewer            : TImage;
    imlMain              : TImageList;
    ispMain              : TTIPropertyGrid;
    nbWatches            : TPageControl;
    Panel1               : TPanel;
    Panel2               : TPanel;
    pnlCallStackWatch    : TPanel;
    pnlFilter            : TPanel;
    pnlLeft              : TPanel;
    pnlMessages          : TPanel;
    pnlRight             : TPanel;
    pgcViewer            : TPageControl;
    pgHexEditor          : TTabSheet;
    pgImageViewer        : TTabSheet;
    pgInspector          : TTabSheet;
    pgText               : TTabSheet;
    SpeedButton1         : TSpeedButton;
    SpeedButton2         : TSpeedButton;
    splLeftHorizontal    : TSplitter;
    splVertical          : TSplitter;
    sbrMain              : TStatusBar;
    splLeftVertical      : TSplitter;
    tlbMessages          : TToolBar;
    ToolButton1          : TToolButton;
    ToolButton10         : TToolButton;
    ToolButton11         : TToolButton;
    ToolButton12         : TToolButton;
    ToolButton13         : TToolButton;
    ToolButton14         : TToolButton;
    ToolButton15         : TToolButton;
    ToolButton16         : TToolButton;
    ToolButton17         : TToolButton;
    ToolButton18         : TToolButton;
    ToolButton19         : TToolButton;
    ToolButton2          : TToolButton;
    ToolButton20         : TToolButton;
    ToolButton21         : TToolButton;
    ToolButton22         : TToolButton;
    ToolButton3          : TToolButton;
    ToolButton4          : TToolButton;
    ToolButton5          : TToolButton;
    ToolButton6          : TToolButton;
    ToolButton7          : TToolButton;
    ToolButton8          : TToolButton;
    ToolButton9          : TToolButton;
    tsHistory            : TTabSheet;
    tsLatest             : TTabSheet;
    tsSelected           : TTabSheet;
    vtvMessages          : TVirtualStringTree;
    {$ENDREGION}

    procedure actBitmapExecute(Sender: TObject);
    procedure actCallStackExecute(Sender: TObject);
    procedure actCheckPointExecute(Sender: TObject);
    procedure actClearMessagesExecute(Sender: TObject);
    procedure actConditionalExecute(Sender: TObject);
    procedure actCustomDataExecute(Sender: TObject);
    procedure actErrorExecute(Sender: TObject);
    procedure actExceptionExecute(Sender: TObject);
    procedure actHeapInfoExecute(Sender: TObject);
    procedure actMemoryExecute(Sender: TObject);
    procedure actMethodTracesExecute(Sender: TObject);
    procedure actObjectExecute(Sender: TObject);
    procedure actSearchFilterExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actSelectNoneExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actStringsExecute(Sender: TObject);
    procedure actToggleAlwaysOnTopExecute(Sender: TObject);
    procedure actToggleInfoExecute(Sender: TObject);
    procedure actToggleWarningExecute(Sender: TObject);
    procedure actValueExecute(Sender: TObject);

    procedure edtFilterMessagesExecute(Sender: TObject);
    procedure ClearMessages(Sender: TObject);
    procedure ComboWatchHistorySelect(Sender: TObject);
    procedure ImgViewerDblClick(Sender: TObject);
    procedure nbWatchesPageChanged(Sender: TObject);
    procedure PanelImageViewerDblClick(Sender: TObject);
    procedure vtvMessagesFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure vtvMessagesFocusChanging(Sender: TBaseVirtualTree;
      OldNode: PVirtualNode; NewNode: PVirtualNode; OldColumn: TColumnIndex;
      NewColumn: TColumnIndex; var Allowed: Boolean);
    procedure vtvMessagesFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode
      );
    procedure vtvMessagesGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: LongInt);
    procedure vtvMessagesGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure vtvMessagesInitNode(Sender: TBaseVirtualTree;
      ParentNode: PVirtualNode; Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);

  private
    FTitleFilter    : string;
    FActiveMessages : set of TMessageSet;
    FMessageCount   : LongWord;
    FActiveWatch    : TStringGrid;
    FCurrentMsg     : TLogMessage;
    FLastParent     : PVirtualNode;
    FLastNode       : PVirtualNode;
    FIPCServer      : TSimpleIPCServer;
    FWatches        : TWatchList;
    FExpandParent   : Boolean;
    FManager        : IEditorManager;
    FSettings       : IEditorSettings;
    FEditorView     : IEditorView;

    procedure FilterCallback(
          Sender : TBaseVirtualTree;
          Node   : PVirtualNode;
          Data   : Pointer;
      var Abort  : Boolean
    );
    function GetEditor: IEditorView;
    procedure SetupSearchFilter;
    procedure UpdateActiveMessages(
      const AMessageType : Byte;
      const Sender       : TObject;
      const AToggle      : Boolean = True
    );
    procedure WatchUpdateCallback(const AVariable, AValue: string);
    procedure NewWatchVariable(const AVariable: string; AIndex: PtrInt);
    procedure ReceiveMessage(Sender: TObject);
    procedure UpdateCallStack(var ANode: PVirtualNode);
    procedure UpdateWatches;
    procedure UpdateWatchHistory;
    procedure ShowBitmapInfo(ABitmap: TBitmap);

  protected
    procedure UpdateActions; override;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Editor: IEditorView
      read GetEditor;
  end;

var
  frmMain: TfrmMain;

const
  ALL_MESSAGES = [
    ltInfo,
    ltError,
    ltWarning,
    ltValue,
    ltEnterMethod,
    ltExitMethod,
    ltConditional,
    ltCheckpoint,
    ltStrings,
    ltCallStack,
    ltObject,
    ltException,
    ltBitmap,
    ltHeapInfo,
    ltMemory,
    ltCustomData,
    ltWatch,
    ltCounter
  ];

implementation

{$R *.lfm}

uses
  StrUtils,

  ts_Editor_Manager, ts.Editor.Factories,

  LCLIntf, LCLType;

type
  TNodeData = record
    Title   : string;
    MsgType : Integer;
    MsgData : TStream;
    MsgTime : TDateTime;
    Index   : LongWord;
  end;
  PNodeData = ^TNodeData;

//const
//  PixelFormatNames: array [TPixelFormat] of string =
//    (
//    'pfDevice',
//    'pf1bit',
//    'pf4bit',
//    'pf8bit',
//    'pf15bit',
//    'pf16bit',
//    'pf24bit',
//    'pf32bit',
//    'pfCustom'
//    );
//  HandleTypeNames: array [TBitmapHandleType] of string =
//    ('bmDIB',
//    'bmDDB');

{ TfrmMain }

{$REGION 'construction and destruction' /FOLD}
procedure TfrmMain.AfterConstruction;
begin
  inherited AfterConstruction;
  FSettings                := TEditorFactories.CreateSettings(Self, 'malloc.xml');
  //, 'settings.xml');
  FManager                 := TEditorFactories.CreateManager(Self, FSettings);
  FManager.Settings.Load;
  FEditorView              := TEditorFactories.CreateView(pgText,FManager,'Tool');

  vtvMessages.NodeDataSize := SizeOf(TNodeData);
  FWatches                 := TWatchList.Create;
  FWatches.OnUpdate        := WatchUpdateCallback;
  FWatches.OnNewVariable   := NewWatchVariable;
  FIPCServer               := TSimpleIPCServer.Create(nil);
  with FIPCServer do
  begin
    ServerID  := 'ipc_log_server';
    Global    := True;
    OnMessage := ReceiveMessage;
    StartServer;
  end;
  SetupSearchFilter;
  FActiveMessages := ALL_MESSAGES;
end;

procedure TfrmMain.BeforeDestruction;
begin
  FSettings.Save;
  FEditorView := nil;
  FSettings   := nil;
  FManager    := nil;
  FIPCServer.Free;
  FWatches.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'action handlers' /FOLD}
procedure TfrmMain.actClearMessagesExecute(Sender: TObject);
begin
  vtvMessages.Clear;
  grdCallStack.RowCount      := 1;
  gridLastestWatch.RowCount  := 1;
  gridSelectedWatch.RowCount := 1;
  GridWatchHistory.RowCount  := 1;
  FWatches.Clear;
  ComboWatchHistory.Clear;
  Editor.Lines.Clear;
  pgcViewer.PageIndex        := 0;
  FMessageCount              := 0;
  FLastNode                  := nil;
  FLastParent                := nil;
end;

procedure TfrmMain.actConditionalExecute(Sender: TObject);
begin
  UpdateActiveMessages(ltConditional, Sender);
end;

procedure TfrmMain.actCustomDataExecute(Sender: TObject);
begin
  UpdateActiveMessages(ltCustomData, Sender);
end;

procedure TfrmMain.actErrorExecute(Sender: TObject);
begin
  UpdateActiveMessages(ltError, Sender);
end;

procedure TfrmMain.actExceptionExecute(Sender: TObject);
begin
  UpdateActiveMessages(ltException, Sender);
end;

procedure TfrmMain.actHeapInfoExecute(Sender: TObject);
begin
  UpdateActiveMessages(ltHeapInfo, Sender);
end;

procedure TfrmMain.actMemoryExecute(Sender: TObject);
begin
  UpdateActiveMessages(ltMemory, Sender);
end;

procedure TfrmMain.actMethodTracesExecute(Sender: TObject);
begin
  UpdateActiveMessages(ltEnterMethod, Sender);
  UpdateActiveMessages(ltExitMethod, Sender, False);
end;

procedure TfrmMain.actObjectExecute(Sender: TObject);
begin
  UpdateActiveMessages(ltObject, Sender);
end;

procedure TfrmMain.actSearchFilterExecute(Sender: TObject);
begin
  edtFilterMessages.SetFocus;
end;

procedure TfrmMain.actCallStackExecute(Sender: TObject);
begin
  UpdateActiveMessages(ltCallStack, Sender);
end;

procedure TfrmMain.actBitmapExecute(Sender: TObject);
begin
  UpdateActiveMessages(ltBitmap, Sender);
end;

procedure TfrmMain.actCheckPointExecute(Sender: TObject);
begin
  UpdateActiveMessages(ltCheckpoint, Sender);
end;

procedure TfrmMain.actSelectAllExecute(Sender: TObject);
begin
  FActiveMessages := ALL_MESSAGES;
  vtvMessages.IterateSubtree(nil, FilterCallback, nil);
end;

procedure TfrmMain.actSelectNoneExecute(Sender: TObject);
begin
  FActiveMessages := [];
  //Scans all tree nodes
  vtvMessages.IterateSubtree(nil, FilterCallback, nil);
end;

procedure TfrmMain.actStopExecute(Sender: TObject);
begin
  if actStop.Checked then
    FIPCServer.OnMessage := nil
  else
    FIPCServer.OnMessage := ReceiveMessage;
end;

procedure TfrmMain.actStringsExecute(Sender: TObject);
begin
  UpdateActiveMessages(ltStrings, Sender);
end;

procedure TfrmMain.actToggleAlwaysOnTopExecute(Sender: TObject);
begin
  if actToggleAlwaysOnTop.Checked then
    SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE)
  else
    SetWindowPos(Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE);
end;

procedure TfrmMain.actToggleInfoExecute(Sender: TObject);
begin
  UpdateActiveMessages(ltInfo, Sender);
end;

procedure TfrmMain.actToggleWarningExecute(Sender: TObject);
begin
  UpdateActiveMessages(ltWarning, Sender);
end;

procedure TfrmMain.actValueExecute(Sender: TObject);
begin
  UpdateActiveMessages(ltValue, Sender);
end;
{$ENDREGION}

{$REGION 'event handlers' /FOLD}
procedure TfrmMain.ImgViewerDblClick(Sender: TObject);
begin
  //with ImgViewer.Picture.Bitmap do
    //PanelImageViewer.Canvas.DrawFocusRect(Rect(0, 0, Width + 1, Height + 1));
end;

procedure TfrmMain.nbWatchesPageChanged(Sender: TObject);
begin
  UpdateWatches;
end;

procedure TfrmMain.PanelImageViewerDblClick(Sender: TObject);
begin
  //if PanelImageViewer.Color = clBtnFace then
  //  PanelImageViewer.Color := clWhite
  //else
  //  PanelImageViewer.Color := clBtnFace;
end;

procedure TfrmMain.vtvMessagesFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  Stream: TStringStream;
  S       : string;
  ND      : PNodeData;
begin
  UpdateWatches;
  Stream := TStringStream.Create('');
  ND := PNodeData(Sender.GetNodeData(Node));
  try
    if ND.MsgData = nil then
    begin
      pgcViewer.PageIndex := 0;
      Editor.Text := '';
    end
    else
      ND.MsgData.Position := 0;
    case ND.MsgType of
      ltStrings, ltCallStack, ltException, ltHeapInfo, ltCustomData:
      begin
        Stream.Position := 0;
        Stream.CopyFrom(ND.MsgData, ND.MsgData.Size);
        Stream.Position := 0;
        S := Stream.DataString;
        Editor.Text := S;
        Editor.HighlighterName := 'TXT';
        pgcViewer.PageIndex := 0;
      end;
      ltObject:
      begin
        ND.MsgData.Position := 0;
        ObjectBinaryToText(ND.MsgData, Stream);
        Stream.Position := 0;
        S := Stream.DataString;
        Editor.Text := S;
        Editor.HighlighterName := 'LFM';
        pgcViewer.PageIndex := 0;
      end;
      ltBitmap:
      begin
        ImgViewer.Picture.Bitmap.LoadFromStream(ND.MsgData);
        pgcViewer.PageIndex := 1;
        ShowBitmapInfo(ImgViewer.Picture.Bitmap);
      end;
      ltMemory:
      begin
        edtHex.OpenStream(ND.MsgData);
{


        }

        pgcViewer.PageIndex := 3;
      end;
      else
      begin
        S := ND.Title;
        //S := Copy(S, Pos('=', S) + 1, Length(S));
        Editor.Text := S;
        FManager.Commands.GuessHighlighterType;
        pgcViewer.PageIndex := 0;
      end;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TfrmMain.vtvMessagesFocusChanging(Sender: TBaseVirtualTree;
  OldNode: PVirtualNode; NewNode: PVirtualNode; OldColumn: TColumnIndex;
  NewColumn: TColumnIndex; var Allowed: Boolean);
begin
  //Todo: merge with Changed?
  //The CallStack is only updated if the parent changes
  Allowed := OldNode <> NewNode;
  if Allowed and ((OldNode = nil) or (NewNode = nil) or
    (OldNode^.Parent <> NewNode^.Parent)) then
    UpdateCallStack(NewNode);
  //warning NewNode value is not more valid after here
end;

procedure TfrmMain.vtvMessagesFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  with PNodeData(Sender.GetNodeData(Node))^ do
  begin
    Title := '';
    if MsgData <> nil then
      MsgData.Destroy;
  end;
end;

procedure TfrmMain.vtvMessagesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: LongInt);
begin
  if Column = 0 then
    ImageIndex := PNodeData(Sender.GetNodeData(Node))^.MsgType;
end;

procedure TfrmMain.vtvMessagesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  if Column = 0 then
  begin
    CellText := PNodeData(Sender.GetNodeData(Node))^.Title;
  end
  else
  begin
    CellText := DateTimeToStr(Now);
  end;
end;

procedure TfrmMain.vtvMessagesInitNode(Sender: TBaseVirtualTree;
  ParentNode: PVirtualNode; Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
begin
  with PNodeData(Sender.GetNodeData(Node))^ do
  begin
    Title   := FCurrentMsg.MsgText;
    MsgData := FCurrentMsg.Data;
    MsgTime := FCurrentMsg.MsgTime;
    MsgType := FCurrentMsg.MsgType;
    //In fast computers two or more messages can have the same TimeStamp
    //This leads to conflicts when determining the Watches values
    //Use an unique index instead
    Index := FMessageCount;
    //Show only what matches filter criterias
    Sender.IsVisible[Node] := (MsgType in [ltEnterMethod, ltExitMethod]) or
      ((MsgType in FActiveMessages) and IsWild(Title, FTitleFilter, True));
  end;
end;
{$ENDREGION}

procedure TfrmMain.ClearMessages(Sender: TObject);
begin
  vtvMessages.Clear;
  grdCallStack.RowCount      := 1;
  gridLastestWatch.RowCount  := 1;
  gridSelectedWatch.RowCount := 1;
  GridWatchHistory.RowCount  := 1;
  FWatches.Clear;
  ComboWatchHistory.Clear;
  Editor.Lines.Clear;
  pgcViewer.PageIndex        := 0;
  FMessageCount              := 0;
  FLastNode                  := nil;
  FLastParent                := nil;
end;

procedure TfrmMain.ComboWatchHistorySelect(Sender: TObject);
begin
  UpdateWatchHistory;
end;

procedure TfrmMain.edtFilterMessagesExecute(Sender: TObject);
begin
  SetupSearchFilter;
  //Scans all tree nodes
  vtvMessages.IterateSubtree(nil, FilterCallback, nil);
  vtvMessages.SetFocus;
end;

procedure TfrmMain.FilterCallback(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Data: Pointer; var Abort: Boolean);
begin
  with PNodeData(Sender.GetNodeData(Node))^ do
    Sender.IsVisible[Node] := (MsgType in FActiveMessages)
      and IsWild(Title, FTitleFilter, True);
end;

function TfrmMain.GetEditor: IEditorView;
begin
  Result := FEditorView;
end;

procedure TfrmMain.SetupSearchFilter;
begin
  //Set Title Filter
  FTitleFilter := Trim(edtFilterMessages.Text) + '*';
  if Length(FTitleFilter) > 1 then //editFilter is not empty
    FTitleFilter := '*' + FTitleFilter;
end;

procedure TfrmMain.UpdateActiveMessages(const AMessageType: Byte;
  const Sender: TObject; const AToggle: Boolean);
var
  A : TAction;
begin
  A := Sender as TAction;
  if AToggle then
    A.Checked := not A.Checked;
  if A.Checked then
    Include(FActiveMessages, AMessageType)
  else
    Exclude(FActiveMessages, AMessageType);
  vtvMessages.IterateSubtree(nil, FilterCallback, nil);
end;

procedure TfrmMain.WatchUpdateCallback(const AVariable, AValue: string);
begin
  with FActiveWatch do
  begin
    RowCount := RowCount + 1;
    Cells[0, RowCount - 1] := AVariable;
    Cells[1, RowCount - 1] := AValue;
  end;
end;

procedure TfrmMain.NewWatchVariable(const AVariable: string; AIndex: PtrInt);
begin
  ComboWatchHistory.Items.AddObject(AVariable, TObject(AIndex));
end;

procedure TfrmMain.ReceiveMessage(Sender: TObject);
var
  TextSize, DataSize: Integer;
begin
  TextSize := 0;
  DataSize := 0;
  vtvMessages.BeginUpdate;
  Inc(FMessageCount);
  with TSimpleIPCServer(Sender).MsgData do
  begin
    Seek(0, soFromBeginning);
    ReadBuffer(FCurrentMsg.MsgType, SizeOf(Integer));
    ReadBuffer(FCurrentMsg.MsgTime, SizeOf(TDateTime));
    ReadBuffer(TextSize, SizeOf(Integer));
    SetLength(FCurrentMsg.MsgText, TextSize);
    ReadBuffer(FCurrentMsg.MsgText[1], TextSize);
    ReadBuffer(DataSize, SizeOf(Integer));
    if DataSize > 0 then
    begin
      FCurrentMsg.Data := TMemoryStream.Create;
      FCurrentMsg.Data.Size := 0;
      FCurrentMsg.Data.Position := 0;
      FCurrentMsg.Data.CopyFrom(TSimpleIPCServer(Sender).MsgData, DataSize);
    end
    else
      FCurrentMsg.Data := nil;

    case FCurrentMsg.MsgType of
      ltEnterMethod:
      begin
        FLastNode := vtvMessages.AddChild(FLastParent, nil);
        if FExpandParent then
          vtvMessages.Expanded[FLastParent] := True
        else
          FExpandParent := True;
        FLastParent := FLastNode;
        vtvMessages.ValidateNode(FLastNode, False);
      end;
      ltExitMethod:
      begin
        if (FLastParent = nil) or (FLastParent^.Parent =
          vtvMessages.RootNode) then
        begin
          FLastNode := vtvMessages.AddChild(nil, nil);
          FLastParent := nil;
        end
        else
        begin
          FLastNode := vtvMessages.AddChild(FLastParent^.Parent, nil);
          FLastParent := FLastNode^.Parent;
        end;
        vtvMessages.ValidateNode(FLastNode, False);
      end;
      ltWatch, ltCounter:
      begin
        FWatches.Add(FCurrentMsg.MsgText, FMessageCount,
          FCurrentMsg.MsgType = ltCounter);
        UpdateWatches;
      end;
      ltClear:
      begin
        ClearMessages(nil);
        FLastNode := nil;
        FLastParent := nil;
      end
      else
        FLastNode := vtvMessages.AddChild(FLastParent, nil);
        vtvMessages.ValidateNode(FLastNode, False);
        if FExpandParent then
        begin
          vtvMessages.Expanded[FLastParent] := True;
          FExpandParent := False;
        end;
    end;
  end;
  vtvMessages.EndUpdate;
  vtvMessages.FocusedNode := vtvMessages.GetLast;
end;

procedure TfrmMain.UpdateCallStack(var ANode: PVirtualNode);
var
  I: Integer;
begin
  with vtvMessages, grdCallStack do
  begin
    I := GetNodeLevel(ANode);
    RowCount := Succ(I);
    while I > 0 do
    begin
      Cells[0, I] := PNodeData(GetNodeData(ANode^.Parent))^.Title;
      ANode := ANode^.Parent;
      Dec(I);
    end;
  end;
end;

procedure TfrmMain.UpdateWatches;
var
  TempIndex: LongWord;
begin
  case nbWatches.PageIndex of
    0{Last}, 1{Selected}:
    begin
      if nbWatches.PageIndex = 0 then
      begin
        FActiveWatch := gridLastestWatch;
        TempIndex := FMessageCount;
      end
      else
      begin
        FActiveWatch := gridSelectedWatch;
        if vtvMessages.FocusedNode <> nil then
          TempIndex := PNodeData(vtvMessages.GetNodeData(
            vtvMessages.FocusedNode))^.Index
        else
          TempIndex := 0;
      end;
      FActiveWatch.RowCount := 1;
      FWatches.Update(TempIndex);
    end;
    2{History}:
    begin
      UpdateWatchHistory;
    end;
  end;
end;

procedure TfrmMain.UpdateWatchHistory;
var
  I: Integer;
begin
  with ComboWatchHistory do
  begin
    if ItemIndex = -1 then
      Exit;
    with FWatches[PtrInt(Items.Objects[ItemIndex])] do
    begin
      GridWatchHistory.RowCount := Count + 1;
      for I := 1 to Count do
        GridWatchHistory.Cells[0, I] := Values[I - 1];
    end;
  end;
end;

procedure TfrmMain.ShowBitmapInfo(ABitmap: TBitmap);
begin
  with {StringGridBitmap, }ABitmap do
  begin
    //Cells[1, 0] := IntToStr(Height);
    //Cells[1, 1] := IntToStr(Width);
    //Cells[1, 2] := PixelFormatNames[PixelFormat];
    //Cells[1, 3] := HandleTypeNames[HandleType];
    //Cells[1, 4] := '$' + IntToHex(TransparentColor, 8);
  end;
end;

procedure TfrmMain.UpdateActions;
var
  B: Boolean;
begin
  actBitmap.Checked        := ltBitmap in FActiveMessages;
  actCallStack.Checked     := ltCallStack in FActiveMessages;
  actCheckPoint.Checked    := ltCheckpoint in FActiveMessages;
  actConditional.Checked   := ltConditional in FActiveMessages;
  actToggleInfo.Checked    := ltInfo in FActiveMessages;
  actToggleWarning.Checked := ltWarning in FActiveMessages;
  actValue.Checked         := ltValue in FActiveMessages;
  actError.Checked         := ltError in FActiveMessages;
  actMethodTraces.Checked  := ltEnterMethod in FActiveMessages;
  actException.Checked     := ltException in FActiveMessages;
  actObject.Checked        := ltObject in FActiveMessages;
  actHeapInfo.Checked      := ltHeapInfo in FActiveMessages;
  actCustomData.Checked    := ltCustomData in FActiveMessages;
  actStrings.Checked       := ltStrings in FActiveMessages;
  actMemory.Checked        := ltMemory in FActiveMessages;
  B := not actStop.Checked;
  actBitmap.Enabled        := B;
  actCallStack.Enabled     := B;
  actCheckPoint.Enabled    := B;
  actConditional.Enabled   := B;
  actToggleInfo.Enabled    := B;
  actToggleWarning.Enabled := B;
  actValue.Enabled         := B;
  actError.Enabled         := B;
  actMethodTraces.Enabled  := B;
  actException.Enabled     := B;
  actObject.Enabled        := B;
  actHeapInfo.Enabled      := B;
  actCustomData.Enabled    := B;
  actStrings.Enabled       := B;
  actMemory.Enabled        := B;

  actSelectAll.Enabled := not (FActiveMessages = ALL_MESSAGES);
  actSelectNone.Enabled := not (FActiveMessages = []);
  inherited UpdateActions;
end;

end.

