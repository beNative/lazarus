program Notepas;

{$MODE objfpc}{$H+}

uses
  SysUtils,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  Notepas.Forms.Main,
  { you can add units after this }
  DefaultTranslator, pl_exsystem, pl_virtualtrees, FrameViewer09,
  lazrichedit, richmemopackage,

  sharedlogger, ipcchannel, pl_luicontrols, runtimetypeinfocontrols,
  pl_kcontrols, pl_zeosdbocomp, pl_zmsql, pl_richview,
ts.Components.MultiPanel, ts.Components.DBGridView, ts.Components.Docking,
  ts.Components.Docking.OptionsDialog, ts.Components.Docking.Resources,
  ts.Components.Docking.Storage, ts.Components.ExportRTF,
  ts.Components.FileAssociation, ts.Components.GridView,
  ts.Components.Inspector, ts.Components.SynMiniMap,
  ts.Components.UNIHighlighter, ts.Components.UniqueInstance,
  ts.Components.XMLTree, ts.Components.XMLTree.Editors,
  ts.Components.XMLTree.NodeAttributes, ts.Core.BRRE, ts.Core.BRREUnicode,
ts.Core.NativeXml.Debug, ts.Core.NativeXml.Streams,
  ts.Core.NativeXml.StringTable, ts.Core.CodecUtilsWin32, ts.Core.Collections,
  ts.Core.ColumnDefinitions, ts.Core.ColumnDefinitionsDataTemplate,
  ts.Core.ComponentInspector, ts.Core.DataTemplates, ts.Core.DBUtils,
  ts.Core.DirectoryWatch, ts.Core.EncodingUtils, ts.Core.FormSettings,
  ts.Core.HashStrings, ts.Core.Helpers, ts.Core.KeyValues, ts.Core.NativeXml,
  ts.Core.NativeXml.ObjectStorage, ts.core.nativexml.win32compat,
  ts.Core.SQLParser, ts.Core.SQLScanner, ts.Core.SQLTree, ts.Core.StringUtils,
  ts.Core.TreeViewPresenter, ts.Core.Utils, ts.Core.Value, ts.Core.VersionInfo,
  ts.Core.XMLUtils, ts.Core.FileAssociations, ts.Editor.AboutDialog,
  ts.Editor.ToolView.Actionlist, ts.Editor.ToolView.AlignLines,
  ts.Editor.ToolView.CharacterMap, ts.Editor.ToolView.CodeFilter,
  ts.Editor.CodeFormatters, ts.Editor.CodeFormatters.SQL,
  ts.Editor.ToolView.CodeShaper, ts.Editor.CodeTags, ts.Editor.Commands,
  ts.Editor.CommentStripper, ts.Editor.ToolView.Base, ts.Editor.Helpers,
  ts.Editor.ToolView.HexEditor, ts.Editor.HighlighterAttributes,
  ts.Editor.Highlighters, ts.Editor.ToolView.HTMLView, ts.editor.interfaces,
  ts.editor.manager, ts.Editor.ToolView.MiniMap, ts.Editor.ToolView.Preview,
  ts.Editor.Resources, ts.editor.searchengine, ts.Editor.ToolView.Search,
  ts.Editor.Selection, ts.Editor.ToolView.SelectionInfo, ts.editor.settings,
  ts.Editor.SettingsDialog.Old, ts.Editor.Settings.AlignLines,
  ts.Editor.Settings.CodeShaper, ts.editor.settings.searchengine,
  ts.Editor.ToolView.Shortcuts, ts.Editor.ToolView.Test, ts.Editor.Utils,
  ts.editor.view, ts.Editor.ToolView.ViewList, ts.Editor.ToolView.Structure,
  ts.Editor.SettingsDialog.FileAssociations, ts.Editor.SettingsDialog.FileTypes,
  SetupFiltersDialog, ts.RichEditor.Helpers, ts.RichEditor.Interfaces,
  ts.RichEditor.Manager, ts.RichEditor.TextAttributes, ts.RichEditor.View,
  ts.Editor.SettingsDialog, ts.Editor.SettingsDialog.KeyMappings,
  ts.Editor.SettingsDialog.Highlighters, ts.Editor.ToolView.ScriptEditor,
ts.Editor.SettingsDialog.Colors, ts.editor.toolview.manager;

{$R *.res}

begin
  Application.Title := 'Notepas';
  //if FileExists('Notepas.trc') then
  //  DeleteFile('Notepas.trc');
  //SetHeapTraceOutput('Notepas.trc');
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  SetDefaultLang('nl');
  Application.Run;
end.

