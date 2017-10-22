{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

program notepas;

{$MODE DELPHI}

uses
  SysUtils,
{$IFDEF UNIX} // Required for Linux and BSD
  cthreads,
{$ENDIF}
  DefaultTranslator,
  Interfaces, // this includes the LCL widgetset
  Forms,

  richmemopackage,
  runtimetypeinfocontrols,
  pascalscript,
  kcontrols,
  ExceptionLogger,
  lclextensions_package,

  Notepas.Main.Form;

  //ts.Components.MultiPanel,
  //ts.Components.Docking,
  //ts.Components.Docking.OptionsDialog,
  //ts.Components.Docking.Resources,
  //ts.Components.Docking.Storage,
  //ts.Components.ExportRTF,
  //ts.Components.FileAssociation,
  //ts.Components.GridView,
  //ts.Components.Inspector,
  //ts.Components.SynMiniMap,
  //ts.Components.UNIHighlighter,
  //ts.Components.UniqueInstance,
  //ts.Components.XMLTree,
  //ts.Components.XMLTree.Editors,
  //ts.Components.XMLTree.NodeAttributes,
  //ts.Core.Logger.Channel.LogFile,
  //ts.Core.Logger.Channel.IPC,
  //ts.Core.Logger,
  //ts.Core.SharedLogger,
  //ts.Core.ComponentInspector,
  //ts.Core.BRRE,
  //ts.Core.BRREUnicode,
  //ts.Core.NativeXml.Debug,
  //ts.Core.NativeXml.Streams,
  //ts.Core.NativeXml.StringTable,
  //ts.Core.CodecUtilsWin32,
  //ts.Core.Collections,
  //ts.Core.ColumnDefinitions,
  //ts.Core.ColumnDefinitionsDataTemplate,
  //ts.Core.DataTemplates,
  //ts.Core.DirectoryWatch,
  //ts.Core.EncodingUtils,
  //ts.Core.FormSettings,
  //ts.Core.Helpers,
  //ts.Core.KeyValues,
  //ts.Core.NativeXml,
  //ts.Core.NativeXml.ObjectStorage,
  //ts.Core.NativeXml.win32compat,
  //ts.Core.SQLParser,
  //ts.Core.SQLScanner,
  //ts.Core.SQLTree,
  //ts.Core.StringUtils,
  //ts.Core.TreeViewPresenter,
  //ts.Core.Utils,
  //ts.Core.Value,
  //ts.Core.VersionInfo,
  //ts.Core.XMLUtils,
  //ts.Core.FileAssociations,
  //ts.Core.SystemInfo,
  //ts.Editor.CodeFormatters,
  //ts.Editor.CodeFormatters.SQL,
  //ts.Editor.CodeTags,
  //ts.Editor.Commands,
  //ts.Editor.CommentStripper,
  //ts.Editor.HighlighterAttributes,
  //ts.Editor.Highlighters,
  //ts.Editor.Interfaces,
  //ts.Editor.Resources,
  //ts.Editor.Search.Engine,
  //ts.Editor.Selection,
  //ts.Editor.Settings,
  //ts.Editor.AlignLines.Settings,
  //ts.Editor.CodeShaper.Settings,
  //ts.Editor.Search.Engine.Settings,
  //ts.Editor.Utils,
  //ts.Editor.ToolView.Manager,
  //ts.Editor.manager,
  //ts.Editor.aboutdialog,
  //ts.Editor.View,
  //ts.Editor.SettingsDialog.Old,
  //ts.Editor.settingsdialog,
  //ts.Editor.SettingsDialog.FileAssociations,
  //ts.Editor.SettingsDialog.FileTypes,
  //ts.Editor.SettingsDialog.Highlighters,
  //ts.Editor.SettingsDialog.KeyMappings,
  //ts.Editor.SettingsDialog.Extensions,
  //ts.Editor.Events, ts.Editor.Types,
  //ts.Editor.colors.ToolView,
  //ts.Editor.SettingsDialog.ApplicationSettings,
  //ts.Editor.SettingsDialog.Base,
  //ts.Editor.SettingsDialog.EditorSettings,
  //ts.Editor.MiniMap.Settings,
  //ts.Editor.HexEditor.Settings,
  //ts.Editor.CodeFilter.Settings,
  //ts.Editor.SortStrings.Settings,
  //ts.Editor.Filter.ToolView,
  //ts.Editor.ViewList.ToolView,
  //ts.Editor.ActionList.Templates,
  //ts.Editor.Search.Data,
  //ts.Editor.Search.Templates,
  //ts.Editor.Search.ToolView,
  //ts.Editor.ActionList.ToolView,
  //ts.Editor.AlignLines.ToolView,
  //ts.Editor.ToolView.base,
  //ts.Editor.CharacterMap.ToolView,
  //ts.Editor.CodeFilter.ToolView,
  //ts.Editor.CodeShaper.ToolView,
  //ts.Editor.HexEditor.ToolView,
  //ts.Editor.MiniMap.ToolView,
  //ts.Editor.Preview.ToolView,
  //ts.Editor.ScriptEditor.ToolView,
  //ts.Editor.SelectionInfo.ToolView,
  //ts.Editor.Shortcuts.ToolView,
  //ts.Editor.Structure.ToolView,
  //ts.Editor.SortStrings.ToolView,
  //ts.Editor.Test.ToolView,
  //ts.Editor.CodeFilter.Data,
  //ts.Editor.ViewList.Data,
  //ts.Editor.Factories.Settings,
  //ts.Editor.Factories.Manager,
  //ts.Editor.Factories.Views,
  //ts.Editor.Factories.Menus,
  //ts.Editor.Factories.Toolbars,
  //ts.Editor.Factories,
  //ts.Editor.Options.Settings,
  //Notepas.Resources;

{$R *.res}

begin
  {$IF DECLARED(UseHeapTrace)}
   GlobalSkipIfNoLeaks := True; // supported as of debugger version 3.1.1
  {$ENDIF}
  Application.Title := 'Notepas';
  //if FileExists('Notepas.trc') then
  //  DeleteFile('Notepas.trc');
  SetHeapTraceOutput('Notepas.trc');
  {$IFDEF WINDOWS}
  {$WARNINGS OFF}
  Application.MainFormOnTaskbar := True;
  {$WARNINGS ON}
  {$ENDIF}
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

