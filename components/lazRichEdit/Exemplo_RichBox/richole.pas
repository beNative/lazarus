unit RichOle;

{$mode objfpc}{$H+}

interface

uses
  Windows, ActiveX, Richedit, ComCtrls, ComObj;


type
  _reobject = packed record
    cbStruct: DWORD;
    cp: LONG;
    clsid: TCLSID;
    poleobj: IOleObject;
    pstg: IStorage;
    polesite: IOleClientSite;
    sizel: TSize;
    dvaspect: DWORD;
    dwFlags: DWORD;
    dwUser: DWORD;
  end;
  REOBJECT = _reobject;
  TReObject = _reobject;

const

  REO_GETOBJ_NO_INTERFACES	= $00000000;
  REO_GETOBJ_POLEOBJ		= $00000001;
  REO_GETOBJ_PSTG		= $00000002;
  REO_GETOBJ_POLESITE		= $00000004;
  REO_GETOBJ_ALL_INTERFACES	= $00000007;

  REO_CP_SELECTION = $FFFFFFFF;

  REO_IOB_SELECTION             = $FFFFFFFF;
  REO_IOB_USE_CP                = $FFFFFFFE;

    REO_NULL			= $00000000;
    REO_READWRITEMASK	        = $0000003F;
    REO_DONTNEEDPALETTE	        = $00000020;
    REO_BLANK			= $00000010;
    REO_DYNAMICSIZE		= $00000008;
    REO_INVERTEDSELECT	        = $00000004;
    REO_BELOWBASELINE	        = $00000002;
    REO_RESIZABLE		= $00000001;
    REO_LINK			= $80000000;
    REO_STATIC			= $40000000;
    REO_SELECTED		= $08000000;
    REO_OPEN			= $04000000;
    REO_INPLACEACTIVE	        = $02000000;
    REO_HILITED			= $01000000;
    REO_LINKAVAILABLE	        = $00800000;
    REO_GETMETAFILE		= $00400000;

  RECO_PASTE			= $00000000;
  RECO_DROP			= $00000001;
  RECO_COPY			= $00000002;
  RECO_CUT			= $00000003;
  RECO_DRAG			= $00000004;

  OLECLOSE_SAVEIFDIRTY = 0;
  OLECLOSE_NOSAVE = 1;
  OLECLOSE_PROMPTSAVE = 2;

  IID_IRichEditOle: TGUID = (D1: $00020D00; D2: $0000; D3: $0000; D4: ($C0, $00, $00, $00, $00, $00, $00, $46));
  IID_IRichEditOleCallback: TGUID = (D1: $00020D03; D2: $0000; D3: $0000; D4: ($C0, $00, $00, $00, $00, $00, $00, $46));

{IRichEditOle}

type
  IRichEditOle = interface(IUnknown)
    ['{00020D00-0000-0000-C000-000000000046}']
    function GetClientSite(out lplpolesite: IOLECLIENTSITE): HResult; stdcall;
    function GetObjectCount: longint; stdcall;
    function GetLinkCount: longint; stdcall;
    function GetObject(iob: longint; out reobject: TREOBJECT; dwFlags: DWORD): HRESULT; stdcall;
    function InsertObject(const reobject: TREOBJECT): HResult; stdcall;
    function ConvertObject(iob: longint; const clsidNew: TCLSID;
       lpStrUserTypeNew: POleStr): HRESULT; stdcall;
    function ActivateAs(const clsid, clsidAs: TCLSID): HRESULT; stdcall;
    function SetHostNames(lpstrContainerApp, lpstrContainerObj: POleStr): HRESULT; stdcall;
    function SetLinkAvailable(iob: longint; fAvailable: BOOL): HRESULT; stdcall;
    function SetDvaspect(iob: longint; dvaspect: DWORD): HRESULT; stdcall;
    function HandsOffStorage(iob: longint): HRESULT; stdcall;
    function SaveCompleted(iob: longint; stg: IStorage): HRESULT; stdcall;
    function InPlaceDeactivate: HRESULT; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HRESULT; stdcall;
    function GetClipboardData(const chrg: TCharRange; reco: DWORD;
         out dataobj: IDataObject): HRESULT; stdcall;
    function ImportDataObject(dataobj: IDataObject; cf: TClipFormat;
         hMetaPict: HGLOBAL): HRESULT; stdcall;
  end;

{IRichEditOleCallback}
  IRichEditOleCallback = interface(IUnknown)
    ['{00020D03-0000-0000-C000-000000000046}']
    function GetNewStorage(out stg: IStorage): HRESULT; stdcall;
    function GetInPlaceContext(out Frame: IOleInPlaceFrame;
         out Doc: IOleInPlaceUIWindow; var FrameInfo: TOleInPlaceFrameInfo): HRESULT; stdcall;
    function ShowContainerUI(fShow: BOOL): HRESULT; stdcall;
    function QueryInsertObject(const clsid: TCLSID; stg: IStorage; cp: longint): HRESULT; stdcall;
    function DeleteObject(oleobj: IOLEObject): HRESULT; stdcall;
    function QueryAcceptData(dataobj: IDataObject; var cfFormat: TClipFormat;
         reco: DWORD; fReally: BOOL; hMetaPict: HGLOBAL): HRESULT; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HRESULT; stdcall;
    function GetClipboardData(const chrg: TCharRange; reco: DWORD;
         out dataobj: IDataObject): HRESULT; stdcall;
    function GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD;
         var dwEffect: DWORD): HRESULT; stdcall;
    function GetContextMenu(seltype: Word; oleobj: IOleObject;
         const chrg: TCharRange; var menu: HMENU): HRESULT; stdcall;
  end;


function RichEdit_SetOleCallback(RichEdit: HWnd; OleInterface: IRichEditOleCallback): BOOL;
function RichEdit_GetOleInterface(RichEdit: HWnd; out OleInterface: IRichEditOle): BOOL;

implementation

function RichEdit_SetOleCallback(RichEdit: HWnd; OleInterface: IRichEditOleCallback): BOOL;
begin
  Result:= BOOL(SendMessage(RichEdit, EM_SETOLECALLBACK, 0, longint(OleInterface)));
end;

function RichEdit_GetOleInterface(RichEdit: HWnd; out OleInterface: IRichEditOle): BOOL;
begin
  Result:= BOOL(SendMessage(RichEdit, EM_GETOLEINTERFACE, 0, longint(@OleInterface)));
end;


end.

