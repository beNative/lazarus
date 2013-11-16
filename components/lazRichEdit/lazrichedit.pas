{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazRichEdit;

interface

uses
    RichBox, WSRichBoxFactory, WSRichBox,
    {$IFDEF LCLWIN32} Win32WSRichBox, Win32WSRichBoxFactory,{$ENDIF} 
    {$IFDEF LCLGTK2}Gtk2WSRichBox, Gtk2WSRichBoxFactory, Gtk2RTFTool,{$ENDIF} 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RichBox', @RichBox.Register); 
end; 

initialization
  RegisterPackage('LazRichEdit', @Register);
end.
