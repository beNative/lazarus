{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for tslib 2.0

   This file was generated on 2/10/2013
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_tslib;

var
  P : TPackage;
  T : TTarget;

begin
  with Installer do
    begin
    P:=AddPAckage('tslib');
    P.Version:='2.0';

{$ifdef ALLPACKAGES}
    // when this is part of a meta package, set here the sub directory
    // P.Directory:='put here the relative path';
{$endif ALLPACKAGES}

    P.Dependencies.Add('pl_dwscript');
    P.Dependencies.Add('frameviewer09');
    P.Dependencies.Add('pl_exsystem');
    P.Dependencies.Add('pl_kcontrols');
    P.Dependencies.Add('turbopoweripro');
    P.Dependencies.Add('pl_exdesign');
    P.Dependencies.Add('pl_aggpasvs');
    P.Dependencies.Add('pl_fzcontrols');
    P.Dependencies.Add('pl_lazsolutions');
    P.Dependencies.Add('pl_luicontrols');
    P.Dependencies.Add('pl_virtualtrees');
    P.Dependencies.Add('pl_zeosdbocomp');
    P.Dependencies.Add('richmemopackage');
    P.Dependencies.Add('lazrichedit');
    P.Dependencies.Add('synedit');
    P.Dependencies.Add('runtimetypeinfocontrols');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-CX');
    P.Options.Add('-Cirot');
    P.Options.Add('-O2');
    P.Options.Add('-gs');
    P.Options.Add('-gl');
    P.Options.Add('-gh');
    P.Options.Add('-vewnhid');
    P.Options.Add('-l');
    P.Options.Add('-vm5024');
    P.Options.Add('-dLCL');
    P.Options.Add('-dLCL$(LCL_PLATFORM)');
    P.IncludePath.Add('editor');
    P.IncludePath.Add('core');
    P.IncludePath.Add('components');
    P.Options.Add('-Fueditor');
    P.Options.Add('-Fucore');
    P.Options.Add('-Furicheditor');
    P.Options.Add('-Fucomponents');
    P.Options.Add('-Fu../../../typhon/components/pl_LuiControls/source/multilog');
    P.Options.Add('-Fu../../../typhon/packager/units/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../../../typhon/components/lazutils/lib/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../../../typhon/components/pl_Synapse/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/lcl/units/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../../../typhon/lcl/units/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/cairocanvas/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../thtmlviewer/package/lib/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../../../typhon/components/lazcontrols/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../lazRichEdit/lib/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../../../typhon/components/pl_AGGPas/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/pl_lclextensions/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../richmemo/lib/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../../../typhon/components/synedit/units/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/ideintf/units/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/pl_VirtualTrees/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/printers/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/tdbf/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/pl_AGGPasVS/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/pl_DWScript/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/pl_ExDesign/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/pl_ExSystem/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/pl_FZControls/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/pl_LuiControls/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/pl_ZeosDBO/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/rtticontrols/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/synedit/design/units/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/turbopower_ipro/units/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/pl_KControls/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../../typhon/components/pl_LazSolutions/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu.');
    T:=P.Targets.AddUnit('tslib.pas');
    t.Dependencies.AddUnit('ipcchannel');
    t.Dependencies.AddUnit('ts.core.brre');
    t.Dependencies.AddUnit('ts.core.brreunicode');
    t.Dependencies.AddUnit('ts.core.codecutilswin32');
    t.Dependencies.AddUnit('ts.core.collections');
    t.Dependencies.AddUnit('ts.core.columndefinitions');
    t.Dependencies.AddUnit('ts.core.columndefinitionsdatatemplate');
    t.Dependencies.AddUnit('ts.core.componentinspector');
    t.Dependencies.AddUnit('ts.core.datatemplates');
    t.Dependencies.AddUnit('ts.core.dbutils');
    t.Dependencies.AddUnit('ts.core.directorywatch');
    t.Dependencies.AddUnit('ts.core.encodingutils');
    t.Dependencies.AddUnit('ts.core.fileassociations');
    t.Dependencies.AddUnit('ts.core.formsettings');
    t.Dependencies.AddUnit('ts.core.hashstrings');
    t.Dependencies.AddUnit('ts.core.helpers');
    t.Dependencies.AddUnit('ts.core.keyvalues');
    t.Dependencies.AddUnit('ts.core.nativexml.debug');
    t.Dependencies.AddUnit('ts.core.nativexml.objectstorage');
    t.Dependencies.AddUnit('ts.core.nativexml');
    t.Dependencies.AddUnit('ts.core.nativexml.streams');
    t.Dependencies.AddUnit('ts.core.nativexml.stringtable');
    t.Dependencies.AddUnit('ts.core.nativexml.win32compat');
    t.Dependencies.AddUnit('ts.core.sqlparser');
    t.Dependencies.AddUnit('ts.core.sqlscanner');
    t.Dependencies.AddUnit('ts.core.sqltree');
    t.Dependencies.AddUnit('ts.core.stringutils');
    t.Dependencies.AddUnit('ts.core.treeviewpresenter');
    t.Dependencies.AddUnit('ts.core.utils');
    t.Dependencies.AddUnit('ts.core.value');
    t.Dependencies.AddUnit('ts.core.versioninfo');
    t.Dependencies.AddUnit('ts.core.xmlutils');
    t.Dependencies.AddUnit('ts.components.dbgridview');
    t.Dependencies.AddUnit('ts.components.docking.optionsdialog');
    t.Dependencies.AddUnit('ts.components.docking');
    t.Dependencies.AddUnit('ts.components.docking.resources');
    t.Dependencies.AddUnit('ts.components.docking.storage');
    t.Dependencies.AddUnit('ts.components.exportrtf');
    t.Dependencies.AddUnit('ts.components.fileassociation');
    t.Dependencies.AddUnit('ts.components.gridview');
    t.Dependencies.AddUnit('ts.components.inspector');
    t.Dependencies.AddUnit('ts.components.multipanel');
    t.Dependencies.AddUnit('ts.components.synminimap');
    t.Dependencies.AddUnit('ts.components.unihighlighter');
    t.Dependencies.AddUnit('ts.components.uniqueinstance');
    t.Dependencies.AddUnit('ts.components.xmltree.editors');
    t.Dependencies.AddUnit('ts.components.xmltree.nodeattributes');
    t.Dependencies.AddUnit('ts.components.xmltree');
    t.Dependencies.AddUnit('ts.editor.aboutdialog');
    t.Dependencies.AddUnit('ts.editor.codeformatters');
    t.Dependencies.AddUnit('ts.editor.codeformatters.sql');
    t.Dependencies.AddUnit('ts.editor.codetags');
    t.Dependencies.AddUnit('ts.editor.commands');
    t.Dependencies.AddUnit('ts.editor.commentstripper');
    t.Dependencies.AddUnit('ts.editor.helpers');
    t.Dependencies.AddUnit('ts.editor.highlighterattributes');
    t.Dependencies.AddUnit('ts.editor.highlighters');
    t.Dependencies.AddUnit('ts.editor.interfaces');
    t.Dependencies.AddUnit('ts.editor.manager');
    t.Dependencies.AddUnit('ts.editor.resources');
    t.Dependencies.AddUnit('ts.editor.searchengine');
    t.Dependencies.AddUnit('ts.editor.selection');
    t.Dependencies.AddUnit('ts.editor.settings.alignlines');
    t.Dependencies.AddUnit('ts.editor.settings.codeshaper');
    t.Dependencies.AddUnit('ts.editor.settings');
    t.Dependencies.AddUnit('ts.editor.settings.searchengine');
    t.Dependencies.AddUnit('ts.editor.settingsdialog.colors');
    t.Dependencies.AddUnit('ts.editor.settingsdialog.fileassociations');
    t.Dependencies.AddUnit('ts.editor.settingsdialog.filetypes');
    t.Dependencies.AddUnit('ts.editor.settingsdialog.highlighters');
    t.Dependencies.AddUnit('ts.editor.settingsdialog.keymappings');
    t.Dependencies.AddUnit('ts.editor.settingsdialog.old');
    t.Dependencies.AddUnit('ts.editor.settingsdialog');
    t.Dependencies.AddUnit('ts.editor.toolview.actionlist');
    t.Dependencies.AddUnit('ts.editor.toolview.alignlines');
    t.Dependencies.AddUnit('ts.editor.toolview.base');
    t.Dependencies.AddUnit('ts.editor.toolview.charactermap');
    t.Dependencies.AddUnit('ts.editor.toolview.codefilter');
    t.Dependencies.AddUnit('ts.editor.toolview.codeshaper');
    t.Dependencies.AddUnit('ts.editor.toolview.hexeditor');
    t.Dependencies.AddUnit('ts.editor.toolview.htmlview');
    t.Dependencies.AddUnit('ts.editor.toolview.manager');
    t.Dependencies.AddUnit('ts.editor.toolview.minimap');
    t.Dependencies.AddUnit('ts.editor.toolview.preview');
    t.Dependencies.AddUnit('ts.editor.toolview.scripteditor');
    t.Dependencies.AddUnit('ts.editor.toolview.search');
    t.Dependencies.AddUnit('ts.editor.toolview.selectioninfo');
    t.Dependencies.AddUnit('ts.editor.toolview.shortcuts');
    t.Dependencies.AddUnit('ts.editor.toolview.structure');
    t.Dependencies.AddUnit('ts.editor.toolview.test');
    t.Dependencies.AddUnit('ts.editor.toolview.viewlist');
    t.Dependencies.AddUnit('ts.editor.utils');
    t.Dependencies.AddUnit('ts.editor.view');

    T:=P.Targets.AddUnit('..\..\..\typhon\components\pl_LuiControls\source\multilog\ipcchannel.pas');
    T:=P.Targets.AddUnit('core\ts.core.brre.pas');
    T:=P.Targets.AddUnit('core\ts.core.brreunicode.pas');
    T:=P.Targets.AddUnit('core\ts.core.codecutilswin32.pas');
    T:=P.Targets.AddUnit('core\ts.core.collections.pas');
    T:=P.Targets.AddUnit('core\ts.core.columndefinitions.pas');
    T:=P.Targets.AddUnit('core\ts.core.columndefinitionsdatatemplate.pas');
    T:=P.Targets.AddUnit('core\ts.core.componentinspector.pas');
    T:=P.Targets.AddUnit('core\ts.core.datatemplates.pas');
    T:=P.Targets.AddUnit('core\ts.core.dbutils.pas');
    T:=P.Targets.AddUnit('core\ts.core.directorywatch.pas');
    T:=P.Targets.AddUnit('core\ts.core.encodingutils.pas');
    T:=P.Targets.AddUnit('core\ts.core.fileassociations.pas');
    T:=P.Targets.AddUnit('core\ts.core.formsettings.pas');
    T:=P.Targets.AddUnit('core\ts.core.hashstrings.pas');
    T:=P.Targets.AddUnit('core\ts.core.helpers.pas');
    T:=P.Targets.AddUnit('core\ts.core.keyvalues.pas');
    T:=P.Targets.AddUnit('core\ts.core.nativexml.debug.pas');
    T:=P.Targets.AddUnit('core\ts.core.nativexml.objectstorage.pas');
    T:=P.Targets.AddUnit('core\ts.core.nativexml.pas');
    T:=P.Targets.AddUnit('core\ts.core.nativexml.streams.pas');
    T:=P.Targets.AddUnit('core\ts.core.nativexml.stringtable.pas');
    T:=P.Targets.AddUnit('core\ts.core.nativexml.win32compat.pas');
    T:=P.Targets.AddUnit('core\ts.core.sqlparser.pas');
    T:=P.Targets.AddUnit('core\ts.core.sqlscanner.pas');
    T:=P.Targets.AddUnit('core\ts.core.sqltree.pas');
    T:=P.Targets.AddUnit('core\ts.core.stringutils.pas');
    T:=P.Targets.AddUnit('core\ts.core.treeviewpresenter.pas');
    T:=P.Targets.AddUnit('core\ts.core.utils.pas');
    T:=P.Targets.AddUnit('core\ts.core.value.pas');
    T:=P.Targets.AddUnit('core\ts.core.versioninfo.pas');
    T:=P.Targets.AddUnit('core\ts.core.xmlutils.pas');
    T:=P.Targets.AddUnit('components\ts.components.dbgridview.pas');
    T:=P.Targets.AddUnit('components\ts.components.docking.optionsdialog.pas');
    T:=P.Targets.AddUnit('components\ts.components.docking.pas');
    T:=P.Targets.AddUnit('components\ts.components.docking.resources.pas');
    T:=P.Targets.AddUnit('components\ts.components.docking.storage.pas');
    T:=P.Targets.AddUnit('components\ts.components.exportrtf.pas');
    T:=P.Targets.AddUnit('components\ts.components.fileassociation.pas');
    T:=P.Targets.AddUnit('components\ts.components.gridview.pas');
    T:=P.Targets.AddUnit('components\ts.components.inspector.pas');
    T:=P.Targets.AddUnit('components\ts.components.multipanel.pas');
    T:=P.Targets.AddUnit('components\ts.components.synminimap.pas');
    T:=P.Targets.AddUnit('components\ts.components.unihighlighter.pas');
    T:=P.Targets.AddUnit('components\ts.components.uniqueinstance.pas');
    T:=P.Targets.AddUnit('components\ts.components.xmltree.editors.pas');
    T:=P.Targets.AddUnit('components\ts.components.xmltree.nodeattributes.pas');
    T:=P.Targets.AddUnit('components\ts.components.xmltree.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.aboutdialog.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.codeformatters.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.codeformatters.sql.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.codetags.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.commands.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.commentstripper.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.helpers.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.highlighterattributes.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.highlighters.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.interfaces.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.manager.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.resources.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.searchengine.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.selection.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.settings.alignlines.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.settings.codeshaper.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.settings.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.settings.searchengine.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.settingsdialog.colors.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.settingsdialog.fileassociations.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.settingsdialog.filetypes.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.settingsdialog.highlighters.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.settingsdialog.keymappings.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.settingsdialog.old.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.settingsdialog.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.toolview.actionlist.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.toolview.alignlines.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.toolview.base.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.toolview.charactermap.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.toolview.codefilter.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.toolview.codeshaper.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.toolview.hexeditor.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.toolview.htmlview.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.toolview.manager.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.toolview.minimap.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.toolview.preview.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.toolview.scripteditor.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.toolview.search.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.toolview.selectioninfo.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.toolview.shortcuts.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.toolview.structure.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.toolview.test.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.toolview.viewlist.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.utils.pas');
    T:=P.Targets.AddUnit('editor\ts.editor.view.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.InstallFiles.Add('tslib.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_tslib;
  Installer.Run;
end.
{$endif ALLPACKAGES}
