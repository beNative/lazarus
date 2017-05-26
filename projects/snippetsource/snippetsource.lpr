program SnippetSource;

{$MODE Delphi}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  SysUtils, Forms, FrameViewer09, pascalscript, richmemopackage,
  virtualtreeview_package, virtualdbtreeex_laz,
  luicontrols, khexeditorlaz,

  { you can add units after this }

  ts.Components.VirtualDBTreeEx,
  ts.Core.ComponentInspector, ts.Editor.AboutDialog, ts.Editor.Manager,
  ts.Editor.Resources, ts.Editor.View, ts.Editor.ToolView.Base,
  ts.Editor.Interfaces, ts.Editor.ScriptEditor.ToolView,
  ts.Editor.ActionList.ToolView, ts.Editor.ViewList.ToolView,
  ts.Editor.Test.ToolView, ts.Editor.Structure.ToolView,
  ts.Editor.Shortcuts.ToolView, ts.Editor.SettingsDialog,
  ts.Editor.SettingsDialog.ApplicationSettings, ts.Editor.SettingsDialog.base,
  ts.Editor.SettingsDialog.EditorSettings, ts.Editor.SettingsDialog.Extensions,
  ts.Editor.SettingsDialog.FileAssociations, ts.Editor.SettingsDialog.FileTypes,
  ts.Editor.SettingsDialog.Highlighters, ts.Editor.SettingsDialog.KeyMappings,
  ts.Editor.SettingsDialog.Old, ts.Editor.SelectionInfo.ToolView,
  ts.Editor.Search.ToolView, ts.Editor.Preview.ToolView,
  ts.Editor.Minimap.ToolView, ts.Editor.HtmlView.ToolView,
  ts.Editor.HexEditor.ToolView, ts.Editor.Filter.ToolView,
  ts.Editor.CodeShaper.ToolView, ts.Editor.CodeFilter.ToolView,
  ts.Editor.CharacterMap.ToolView, ts.Editor.AlignLines.ToolView,
  ts.Editor.AlignLines.Settings, ts.Editor.Search.Engine.Settings,
  ts.Editor.CodeShaper.Settings, ts.Editor.CodeFilter.Settings,
  ts.Editor.HTMLView.Settings, ts.Editor.MiniMap.Settings,
  ts.Editor.HexEditor.Settings, ts.Editor.Types,

  SnippetSource.Forms.Main, SnippetSource.Virtualtree.Editors,
  SnippetSource.Modules.Data, SnippetSource.Forms.SQLLog;

{$R *.res}

begin
  if FileExists('trace.trc') then
    DeleteFile('trace.trc');
  //SetHeapTraceOutput('trace.trc');
  Application.Title := 'SnippetSource';
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.



