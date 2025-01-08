{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.HtmlEditor.Resources;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils;

const
  DEFAULT_EDITOR_FONT = 'Segoe UI';

  EMPTY_PAGE_SOURCE  = 'about:blank';
  EMPTY_PAGE_CONTENT = '<html><head></head><body></body></html>';

  JS_ADD_CONTENT_MODIFIED_EVENT =
    'document.addEventListener(''input'', function() {'           + sLineBreak +
    '    window.chrome.webview.postMessage(''contentModified'');' + sLineBreak +
    '});';
  JS_ENABLE_EDITMODE  = 'document.designMode = "on";';
  JS_DISABLE_EDITMODE = 'document.designMode = "off";';
  JS_SET_FONTNAME     = 'document.execCommand(''fontName'', false, ''%s'');';

resourcestring
  SNotImplementedYet = 'Not implemented yet.';

implementation

end.

