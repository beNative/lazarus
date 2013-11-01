unit ts.Editor.Types;

{$MODE Delphi}

interface

uses
  Classes, SysUtils,

  SynMacroRecorder;

type
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

  // enumerations
type
  TEditorTool = (
    etSearch,
    etCodeShaper,
    etCodeFilter,
    etPreview,
    etHexEditor,
    etHTMLView,
    etAlignLines,
    etActionList,
    etViewList
  );
  TEditorTools = set of TEditorTool;

implementation

end.

