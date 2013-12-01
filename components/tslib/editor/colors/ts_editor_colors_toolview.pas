unit ts_Editor_Colors_ToolView;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,

   HexaColorPicker, ColorBox,

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

