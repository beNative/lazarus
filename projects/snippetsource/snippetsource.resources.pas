{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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

unit SnippetSource.Resources;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils;

const
  SETTINGS_FILE = 'settings.json';

resourcestring
  SQueryLookupErrorRunningQuery = 'Error running query <%s>';
  SQueryLookupTooManyRecords    = 'The query <%s> returned too many records';
  SParameterNotAssigned         = 'Parameter <%s> parameter not assigned';
  SDateCreated                  = 'Created: %s';
  SDateModified                 = 'Modified: %s';
  SId                           = 'Id: %d';

implementation

end.

