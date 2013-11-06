unit ts_Editor_Colors_ToolView;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,

  SColorPicker, HSVColorPicker, HexaColorPicker, HSLColorPicker, HColorPicker,
  HSLRingPicker, ColorBox,

  ts_Editor_ToolView_Base;

type

  { TfrmColors }

  TfrmColors = class(TForm)
    ColorListBox1    : TColorListBox;
    HexaColorPicker1 : THexaColorPicker;

  end;

implementation

{$R *.lfm}

end.

