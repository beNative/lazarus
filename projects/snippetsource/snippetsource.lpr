program SnippetSource;

{$MODE Delphi}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  SysUtils, Forms, FrameViewer09, pascalscript, richmemopackage,
  virtualtreeview_package, virtualdbtreeex_laz, virtualdbgrid_package,
  luicontrols, khexeditorlaz,

  { you can add units after this }

  ts.Components.VirtualDBTreeEx,
  ts_Core_ComponentInspector, ts_Editor_AboutDialog, ts_Editor_Manager,
  ts_Editor_Resources, ts_Editor_View, ts_Editor_ToolView_Base,
  ts.Editor.Interfaces, ts_Editor_ScriptEditor_ToolView,
  ts_Editor_ActionList_ToolView, ts_Editor_ViewList_ToolView,
  ts_Editor_Test_ToolView, ts_Editor_Structure_ToolView,
  ts_Editor_Shortcuts_ToolView, ts_Editor_SettingsDialog,
  ts_Editor_SettingsDialog_ApplicationSettings, ts_editor_settingsdialog_base,
  ts_editor_settingsdialog_editorsettings, ts_editor_settingsdialog_extensions,
  ts_editor_settingsdialog_fileassociations, ts_editor_settingsdialog_filetypes,
  ts_editor_settingsdialog_highlighters, ts_editor_settingsdialog_keymappings,
  ts_editor_settingsdialog_old, ts_editor_selectioninfo_toolview,
  ts_editor_search_toolview, ts_editor_preview_toolview,
  ts_editor_minimap_toolview, ts_editor_htmlview_toolview,
  ts_editor_hexeditor_toolview, ts_editor_filter_toolview,
  ts_editor_codeshaper_toolview, ts_editor_codefilter_toolview,
  ts_editor_charactermap_toolview, ts_editor_alignlines_toolview,
  ts.Editor.AlignLines.Settings, ts.Editor.Search.Engine.Settings,
  ts.Editor.CodeShaper.Settings, ts.Editor.CodeFilter.Settings,
  ts.Editor.HTMLView.Settings, ts.Editor.MiniMap.Settings,
  ts.Editor.HexEditor.Settings, ts.Editor.Types,

  SnippetSource_Forms_Main, SnippetSource.Virtualtree.Editors,
  snippetsource_modules_data2, SnippetSource_Forms_SQLLog;

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



