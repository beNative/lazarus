unit HTMLAbt;

interface

uses
  {$IFNDEF LCL} Windows, Messages, {$ELSE} LclIntf, LMessages, LclType, LResources, LCLVersion, {$ENDIF}
  SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, Htmlview, ExtCtrls;

const
  Version = '9.45';

type
  TAboutBox = class(TForm)
    BitBtn1: TBitBtn;
    Panel1: TPanel;
    Viewer: THTMLViewer;
  private
    { Private declarations }
  public
    { Public declarations }
    constructor CreateIt(Owner: TComponent; const ProgName, CompName: string);
  end;

var
  AboutBox: TAboutBox;

implementation

{$IFNDEF LCL}
{$R *.DFM}
{$ENDIF}

constructor TAboutBox.CreateIt(Owner: TComponent; const ProgName, CompName: string);
var
  S: string[210];
begin
inherited Create(Owner);
//Viewer.DefFontName := 'MS Sans Serif';  //Windows-only font
Viewer.DefFontName := 'Arial';
Viewer.DefFontSize := 9;
Viewer.DefFontColor := clNavy;
S :='<body bgcolor="ffffeb" text="000080">'+
    '<center>'+
    '<h1>'+ProgName+'</h1>'+
    '<font color="Maroon">A demo program for the '+CompName+' component</font>'+

{$IFNDEF LCL}
    '<h3>Version '+Version+' compiled with Delphi '+
{$ifdef Windows}
    '1</h3>'+
{$endif}
{$ifdef Ver90}
    '2</h3>'+
{$endif}
{$ifdef Ver100}
    '3</h3>'+
{$endif}
{$ifdef Ver120}
    '4</h3>'+
{$endif}
{$ifdef Ver130}
    '5</h3>'+
{$endif}
{$ifdef Ver140}
    '6</h3>'+
{$endif}
{$ifdef Ver150}
    '7</h3>'+
{$endif}
{$ifdef Ver170}
    '2005</h3>'+
{$endif}
{$ifdef Ver180}
    '2006</h3>'+
{$endif}

{$ELSE}
    '<h3>Version ' + Version + ' compiled with Lazarus ' + lcl_version + '</h3>' +
{$ENDIF}

    '</center>'+
    '</body>';
Viewer.LoadFromBuffer(@S[1], Length(S), '');
end;

initialization
{$IFDEF LCL}
{$I Htmlabt.lrs}  {Include form's resource file}
{$ENDIF}

end.
