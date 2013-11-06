unit ts_Editor_Colors_ToolView;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, mbColorTree, SColorPicker,
  HSVColorPicker, HexaColorPicker, mbColorPalette, HSLColorPicker, HColorPicker,
  HSLRingPicker, mbColorList, Forms, Controls, Graphics, Dialogs, ColorBox,

  ts_Editor_ToolView_Base;

type

  { TfrmColors }

  TfrmColors = class(TForm)
    ColorListBox1: TColorListBox;
    HexaColorPicker1: THexaColorPicker;
  //TCustomEditorToolView)
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

end.

