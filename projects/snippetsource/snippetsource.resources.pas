{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

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
  DEFAULT_DATABASE_NAME = 'snippets.db';
  SQL_LAST_ID = 'select Id from Snippet order by Id desc limit 1';
  SQL_PARENT_ID =
    'select'        + sLineBreak +
    '  Id'          + sLineBreak +
    'from'          + sLineBreak +
    '  Snippet'     + sLineBreak +
    'where'         + sLineBreak +
    '  (Id > %d)'   + sLineBreak +
    '  and NodePath = ''%s''';

resourcestring
  SQueryLookupErrorRunningQuery = 'Error running query <%s>';
  SQueryLookupTooManyRecords    = 'The query <%s> returned too many records';
  SParameterNotAssigned         = 'Parameter <%s> parameter not assigned';
  SDateCreated                  = 'Created: %s';
  SDateModified                 = 'Modified: %s';
  SId                           = 'Id: %d';
  SAskRecreateTables            = 'Warning: this will delete all data!' + #13#10 +
                                  'Are you sure you want to continue?';

  SDatabaseBackupCreated  = 'A backup of the database has been written to %s.';
  SDatabaseIndexesRebuilt = 'All database indexes have been rebuilt.';
  SDatabaseSizeHasBeenReduced = 'Database size was reduced by %s.';
  SDatabaseIntegrityCheckSuccessful = 'Integrity check was successful!';
  SDatabaseIntegrityCheckFailed     = 'Database failed integrity check!';

implementation

end.

