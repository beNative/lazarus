{
  Copyright (C) 2013-2024 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

unit SnippetSource.Resources;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils;

const
  SETTINGS_FILE         = 'settings.json';

  EXECUTE_SCRIPT_FILE   = 'snippetsource_%d.%s';
  EXECUTE_PYTHON_SCRIPT = '.\%s\Scripts\activate.bat & py "%s"';
  LOAD_PYTHON_VENV      = '.\%s\Scripts\activate.bat';
  EXECUTE_PYTHON_VENV   = '.\%s\Scripts\activate.bat & %s';
  LAUNCH_JUPYTER_LAB    = 'jupyter lab --no-browser --LabApp.token=''''';


  SQLITE3_DLL           = 'sqlite3.dll';
  WEBVIEW2LOADER_DLL    = 'WebView2Loader.dll';
  DEFAULT_DATABASE_NAME = 'snippets.db';
  SQL_SQLITE_VERSION    = 'select sqlite_version();';

  DEFAULT_LINUX_DISTRIBUTION = 'ubuntu';


  FIELDNAME_IMAGE = 'Image';

  SQL_LAST_ID   = 'select Id from Snippet order by Id desc limit 1';
  SQL_PARENT_ID =
    '--SQL_PARENT_ID' + sLineBreak +
    'select'          + sLineBreak +
    '  Id'            + sLineBreak +
    'from'            + sLineBreak +
    '  Snippet'       + sLineBreak +
    'where'           + sLineBreak +
    '  (Id >= %d)'    + sLineBreak +
    '  and NodePath = ''%s''';
  SQL_DEFAULT_QUERY = 'select * from Snippet';

  SQL_LOOKUP_QUERY =
    '--SQL_LOOKUP_QUERY'              + sLineBreak +
    'select'                          + sLineBreak +
    '  *'                             + sLineBreak +
    'from'                            + sLineBreak +
    '  Snippet'                       + sLineBreak +
    'where'                           + sLineBreak +
    '  Text like ''%%%0:s%%'''        + sLineBreak +
    '  or RtfText like ''%%%0:s%%'''  + sLineBreak +
    '  or HtmlText like ''%%%0:s%%''' + sLineBreak +
    '  or NodeName like ''%%%0:s%%''';

  SQL_CLEAR_IDS =
    '--SQL_CLEAR_IDS'    + sLineBreak +
    'update'             + sLineBreak +
    '  Snippet'          + sLineBreak +
    'set'                + sLineBreak +
    '  Text = null,'     + sLineBreak +
    '  RtfData = null,'  + sLineBreak +
    '  RtfText = null,'  + sLineBreak +
    '  HtmlData = null,' + sLineBreak +
    '  HtmlText = null,' + sLineBreak +
    '  Source = null'    + sLineBreak +
    'where'              + sLineBreak +
    '  Id in (%s)';

  SQL_DUPLICATE_IDS =
    '--SQL_DUPLICATE_IDS'    + sLineBreak +
    'insert into Snippet ('  + sLineBreak +
    '  HighlighterId,'       + sLineBreak +
    '  NodeTypeId,'          + sLineBreak +
    '  ParentId,'            + sLineBreak +
    '  ImageIndex,'          + sLineBreak +
    '  Image,'               + sLineBreak +
    '  NodeName,'            + sLineBreak +
    '  NodePath,'            + sLineBreak +
    '  Text,'                + sLineBreak +
    '  FoldLevel,'           + sLineBreak +
    '  RtfText,'             + sLineBreak +
    '  RtfData,'             + sLineBreak +
    '  HtmlText,'            + sLineBreak +
    '  HtmlData,'            + sLineBreak +
    '  Source,'              + sLineBreak +
    '  FoldState,'           + sLineBreak +
    '  DateCreated,'         + sLineBreak +
    '  DateModified'         + sLineBreak +
    ')'                      + sLineBreak +
    'select'                 + sLineBreak +
    '  HighlighterId,'       + sLineBreak +
    '  NodeTypeId,'          + sLineBreak +
    '  ParentId,'            + sLineBreak +
    '  ImageIndex,'          + sLineBreak +
    '  Image,'               + sLineBreak +
    '  NodeName,'            + sLineBreak +
    '  NodePath,'            + sLineBreak +
    '  Text,'                + sLineBreak +
    '  FoldLevel,'           + sLineBreak +
    '  RtfText,'             + sLineBreak +
    '  RtfData,'             + sLineBreak +
    '  HtmlText,'            + sLineBreak +
    '  HtmlData,'            + sLineBreak +
    '  Source,'              + sLineBreak +
    '  FoldState,'           + sLineBreak +
    '  CURRENT_TIMESTAMP,'   + sLineBreak +
    '  CURRENT_TIMESTAMP'    + sLineBreak +
    'from'                   + sLineBreak +
    '  Snippet s'            + sLineBreak +
    'where'                  + sLineBreak +
    '  Id in (%s)';

  SQL_MOVEDOWN_IDS =
    '--SQL_MOVEDOWN_IDS'         + sLineBreak +
    'update'                     + sLineBreak +
    '  Snippet'                  + sLineBreak +
    'set'                        + sLineBreak +
    '  Sequence = Sequence - %d' + sLineBreak +
    'where'                      + sLineBreak +
    '  ParentId = %s'            + sLineBreak +
    '  and Sequence = ('         + sLineBreak +
    '    select '                + sLineBreak +
    '      Sequence + 1'         + sLineBreak +
    '    from'                   + sLineBreak +
    '      Snippet'              + sLineBreak +
    '    where '                 + sLineBreak +
    '      Id = %s'              + sLineBreak +
    '  );'                       + sLineBreak +
    'update'                     + sLineBreak +
    '  Snippet'                  + sLineBreak +
    'set'                        + sLineBreak +
    '  Sequence = Sequence + 1'  + sLineBreak +
    'where'                      + sLineBreak +
    '  Id in (%s);';

  SQL_MOVEUP_IDS =
    '--SQL_MOVEUP_IDS'           + sLineBreak +
    'update'                     + sLineBreak +
    '  Snippet'                  + sLineBreak +
    'set'                        + sLineBreak +
    '  Sequence = Sequence + %d' + sLineBreak +
    'where'                      + sLineBreak +
    '  ParentId = %s'            + sLineBreak +
    '  and Sequence = ('         + sLineBreak +
    '    select '                + sLineBreak +
    '      Sequence - 1'         + sLineBreak +
    '    from'                   + sLineBreak +
    '      Snippet'              + sLineBreak +
    '    where '                 + sLineBreak +
    '      Id = %s'              + sLineBreak +
    '  );'                       + sLineBreak +
    'update'                     + sLineBreak +
    '  Snippet'                  + sLineBreak +
    'set'                        + sLineBreak +
    '  Sequence = Sequence - 1'  + sLineBreak +
    'where'                      + sLineBreak +
    '  Id in (%s);';

  SQL_UPDATE_SEQUENCE =
    '--SQL_UPDATE_SEQUENCE'                     + sLineBreak +
    'update'                                    + sLineBreak +
    '  Snippet'                                 + sLineBreak +
    'set'                                       + sLineBreak +
    '  Sequence = ('                            + sLineBreak +
    '    select'                                + sLineBreak +
    '      count(*) + 1'                        + sLineBreak +
    '    from'                                  + sLineBreak +
    '      Snippet as S'                        + sLineBreak +
    '    where'                                 + sLineBreak +
    '      S.ParentId = Snippet.ParentId'       + sLineBreak +
    '        and S.Sequence < Snippet.Sequence' + sLineBreak +
    '  )'                                       + sLineBreak +
    'where'                                     + sLineBreak +
    '  ParentId <> 0';

  SQL_UPDATE_NODEPATH =
    '--SQL_UPDATE_NODEPATH'                               + sLineBreak +
    'with recursive NodePaths as ('                       + sLineBreak +
    '  select'                                            + sLineBreak +
    '    Id,'                                             + sLineBreak +
    '    NodeName,'                                       + sLineBreak +
    '    ParentId,'                                       + sLineBreak +
    '    cast (NodeName as text) as NodePath'             + sLineBreak +
    '  from'                                              + sLineBreak +
    '    Snippet'                                         + sLineBreak +
    '  where'                                             + sLineBreak +
    '    ParentId = 0'                                    + sLineBreak +
    '  union all'                                         + sLineBreak +
    '  select'                                            + sLineBreak +
    '    S.Id,'                                           + sLineBreak +
    '    S.NodeName,'                                     + sLineBreak +
    '    S.ParentId,'                                     + sLineBreak +
    '    NP.NodePath || ''/'' || S.NodeName as NodePath'  + sLineBreak +
    '  from'                                              + sLineBreak +
    '    Snippet as S'                                    + sLineBreak +
    '    join NodePaths as NP'                            + sLineBreak +
    '      on S.ParentId = NP.Id'                         + sLineBreak +
    ')'                                                   + sLineBreak +
    'update'                                              + sLineBreak +
    '  Snippet'                                           + sLineBreak +
    'set'                                                 + sLineBreak +
    '  NodePath = NodePaths.NodePath'                     + sLineBreak +
    'from'                                                + sLineBreak +
    '  NodePaths'                                         + sLineBreak +
    'where'                                               + sLineBreak +
    '  Snippet.Id = NodePaths.Id';

  SQL_NEW_SEQUENCE =
    'select'                            + sLineBreak +
    '  coalesce(max(Sequence), 0) + 1'  + sLineBreak +
    'from'                              + sLineBreak +
    '  Snippet'                         + sLineBreak +
    'where'                             + sLineBreak +
    '  ParentId = %d';

  // Windows registry paths to Python installations
  PYTHON_CORE_LOCAL_MACHINE = 'SOFTWARE\Python\PythonCore';
  PYTHON_CORE_CURRENT_USER  = 'Software\Python\PythonCore';
  PYTHON_DISPLAYNAME        = 'DisplayName';
  PYTHON_EXECUTABLEPATH     = 'ExecutablePath';
  PYTHON_INSTALLPATH        = 'InstallPath';

  VIEW_TYPE_RTF  = 'rtf';
  VIEW_TYPE_TXT  = 'txt';
  VIEW_TYPE_HTML = 'html';

  EDIT_MODE_INS = 'INS';
  EDIT_MODE_OVR = 'OVR';

  KEYBOARD_NUMLOCK  = 'NUM ';
  KEYBOARD_CAPSLOCK = 'CAPS';

resourcestring
  SLastFocusedRecordError       = 'Last focused record could not be located.';
  SQueryLookupErrorRunningQuery = 'Error running query <%s>';
  SQueryLookupTooManyRecords    = 'The query <%s> returned too many records';
  SParameterNotAssigned         = 'Parameter <%s> parameter not assigned';
  SDateCreated                  = 'Created: %s';
  SDateModified                 = 'Modified: %s';
  SId                           = 'Id: %d';
  SAskRecreateTables            =
    'Warning: this will delete all data!' + sLineBreak +
    'Are you sure you want to continue?';

  SDatabaseBackupCreated  = 'A backup of the database has been written to %s.';
  SDatabaseIndexesRebuilt = 'All database indexes have been rebuilt.';
  SDatabaseSizeHasBeenReduced = 'Database size was reduced by %s.';
  SDatabaseIntegrityCheckSuccessful = 'Integrity check was successful!';
  SDatabaseIntegrityCheckFailed     = 'Database failed integrity check!';
  SVirtualEnvironmentAlreadyExists  = 'This virtual environment already exists.';

  SDeleteSelectedItems = 'Are you sure you want to delete te selected item(s)?';
  SNewFolder           = 'New folder';
  SNew                 = 'New';

implementation

end.

