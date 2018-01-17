{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Editor.AboutDialog;

{$MODE DELPHI}

{
  Credits:

    Lazarus - FPC Free Pascal development team
    TVirtualTreeView Mike Lischke, LCL port by Luiz Américo Pereira Câmara with modifications by Tim Sinaeve
    TSynEdit
    TTreeViewPresenter, datatemplates Stefan Glienke
    Anchor Docking Mattias Gaertner
    RTTI controls Mattias Gaertner
    TDirectoryWatch Iztok Kacin, Cromis, FPC port by Tim Sinaeve
    TRichMemo Dmitry 'skalogryz' Boyarintsev
    NativeXML Nils Haeck M.Sc.
    luicontrols Luiz Américo Pereira Câmara
    luicomponents Luiz Américo Pereira Câmara
    MultiLog Luiz Américo Pereira Câmara
    SynUniHighlighter Kirill Burtsev and Vitaly Nevzorov, ported to FPC by Tom Lisjac and Tim Sinaeve
    Silk Icons
    other icons
    GetText translation powered
}

interface

uses
  SysUtils, Forms, StdCtrls, ExtCtrls, Classes, Contnrs, ComCtrls, ActnList,
  Buttons, Clipbrd,

  VirtualTrees,

  ts.Core.VersionInfo, ts.Core.TreeViewPresenter, ts.Core.ColumnDefinitions;

type
  TfrmAbout = class(TForm)
    {$REGION 'designer controls'}
    aclMain: TActionList;
    actClose: TAction;
    actDonate: TAction;
    actCopyToClipboard: TAction;
    actProjectPage: TAction;
    actReportDefect: TAction;
    actHomePage: TAction;
    btnClose: TButton;
    btnCopyToClipboard: TSpeedButton;
    Image1: TImage;
    imgMain: TImage;
    lbBuildDate: TLabel;
    lbLCLVersion: TLabel;
    lblFileVersion: TLabel;
    lblFPCVersion: TLabel;
    lblLegalCopyright: TLabel;
    lblTargetCPU: TLabel;
    lblTargetOS: TLabel;
    lblHomePage: TLabel;
    lblWidgetSet: TLabel;
    pgcMain: TPageControl;
    pnlBuildDate: TPanel;
    pnlFileVersion: TPanel;
    pnlFPCVersion: TPanel;
    pnlLCLVersion: TPanel;
    pnlName: TPanel;
    pnlTargetCPU: TPanel;
    pnlTargetOS: TPanel;
    pnlWidgetSet: TPanel;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    btnDonate: TSpeedButton;
    btnReportIssue: TSpeedButton;
    tsTranslations: TTabSheet;
    tsInfo: TTabSheet;
    tsAbout: TTabSheet;
    tsCredits: TTabSheet;
    {$ENDREGION}

    procedure actCloseExecute(Sender: TObject);
    procedure actCopyToClipboardExecute(Sender: TObject);
    procedure actDonateExecute(Sender: TObject);
    procedure actProjectPageExecute(Sender: TObject);
    procedure actReportDefectExecute(Sender: TObject);
    procedure actHomePageExecute(Sender: TObject);

    procedure FTVPCreditsDoubleClick(Sender: TObject);
    procedure lblHomePageClick(Sender: TObject);
    procedure lblHomePageDblClick(Sender: TObject);
    procedure lblProjectPageClick(Sender: TObject);
    procedure lblProjectPageDblClick(Sender: TObject);

  private
    FVersionInfo     : TVersionInfo;
    FVSTCredits      : TVirtualStringTree;
    FVSTTranslations : TVirtualStringTree;
    FVSTInfoList     : TVirtualStringTree;
    FTVPCredits      : TTreeViewPresenter;
    FTVPTranslations : TTreeViewPresenter;
    FTVPInfoList     : TTreeViewPresenter;
    FCredits         : TObjectList;
    FTranslations    : TObjectList;
    FInfoList        : TObjectList;

    procedure FillCredits;
    procedure FillTranslations;
    procedure FillInfoList;

    procedure InitializeCredits;
    procedure InitializeTranslations;
    procedure InitializeInfoList;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

procedure ShowAboutDialog;

implementation

{$R *.lfm}

uses
  FileUtil,

  ts.Core.Helpers, ts.Core.Utils,

  LCLIntf;

const
  URL_DONATE =
    'https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=tim%2esin' +
    'aeve%40gmail%2ecom&lc=BE&item_name=notepas&currency_code=USD&bn=PP%2dDo' +
    'nationsBF%3abtn_donateCC_LG%2egif%3aNonHosted';
  URL_REPORT_ISSUE = 'https://github.com/beNative/lazarus/issues/new';

resourcestring
  SName         = 'Name';
  SAuthor       = 'Author';
  SLanguageCode = 'Code';
  SLanguageName = 'Language';
  SValue        = 'Value';

procedure ShowAboutDialog;
var
  F : TfrmAbout;
begin
  F := TfrmAbout.Create(Application);
  try
    F.ShowModal;
  finally
    F.Free;
  end;
end;

{$REGION 'TCredit'}
type
  TCredit = class(TPersistent)
  strict private
    FAuthor : string;
    FName   : string;
    FURL    : string;

  published
    property Name: string
      read FName write FName;

    property Author: string
      read FAuthor write FAuthor;

    property URL: string
      read FURL write FURL;
  end;
{$ENDREGION}

{$REGION 'TTranslation'}
type
  TTranslation = class(TPersistent)
  strict private
    FAuthor       : string;
    FLanguageCode : string;
    FLanguageName : string;

  published
    property LanguageCode: string
      read FLanguageCode write FLanguageCode;

    property LanguageName: string
      read FLanguageName write FLanguageName;

    property Author: string
      read FAuthor write FAuthor;
  end;
{$ENDREGION}

{$REGION 'TInfo'}
type
  TInfo = class(TPersistent)
  strict private
    FName  : string;
    FValue : string;

  published
    property Name: string
      read FName write FName;

    property Value: string
      read FValue write FValue;
  end;
{$ENDREGION}

{$REGION 'construction and destruction'}
procedure TfrmAbout.AfterConstruction;
begin
  inherited AfterConstruction;
  FVersionInfo := TVersionInfo.Create(Self);
  pnlFPCVersion.Caption     := FVersionInfo.FPCVersion;
  pnlLCLVersion.Caption     := FVersionInfo.LCLVersion;
  pnlTargetCPU.Caption      := FVersionInfo.TargetCPU;
  pnlTargetOS.Caption       := FVersionInfo.TargetOS;
  pnlBuildDate.Caption      := FVersionInfo.BuildDate;
  pnlWidgetSet.Caption      := FVersionInfo.WidgetSet;
  pnlFileVersion.Caption    := FVersionInfo.FileVersion;
  pnlName.Caption           := FVersionInfo.ProductName + ' '
    + FVersionInfo.ProductVersion;
  lblHomePage.Caption       := FVersionInfo.HomePage;
  lblLegalCopyright.Caption := FVersionInfo.LegalCopyright;
  imgMain.Picture.Assign(FVersionInfo.Icon);
  pnlName.Color                 := Color;
  lblLegalCopyright.Transparent := True;
  lblHomePage.Transparent       := True;
  InitializeCredits;
  InitializeTranslations;
  InitializeInfoList;
  pgcMain.ActivePageIndex := 0;
end;

procedure TfrmAbout.BeforeDestruction;
begin
  FreeAndNil(FCredits);
  FreeAndNil(FTranslations);
  FreeAndNil(FInfoList);
  FreeAndNil(FTVPCredits);
  FreeAndNil(FTVPTranslations);
  FreeAndNil(FTVPInfoList);
  FreeAndNil(FVersionInfo);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmAbout.lblHomePageClick(Sender: TObject);
begin
  actHomePage.Execute;
end;

procedure TfrmAbout.lblHomePageDblClick(Sender: TObject);
begin
  actHomePage.Execute;
end;

procedure TfrmAbout.lblProjectPageClick(Sender: TObject);
begin
  actProjectPage.Execute;
end;

procedure TfrmAbout.lblProjectPageDblClick(Sender: TObject);
begin
  actProjectPage.Execute;
end;

procedure TfrmAbout.FTVPCreditsDoubleClick(Sender: TObject);
begin
  OpenURL((FTVPCredits.SelectedItem as TCredit).URL);
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmAbout.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmAbout.actCopyToClipboardExecute(Sender: TObject);
var
  StrInfo: String;
  i: integer;
begin
  StrInfo:='';
  if FInfoList.Count=0 then
     FillInfoList;
  for i:=0 to FInfoList.Count-1 do
      StrInfo := StrInfo+TInfo(FInfoList.Items[i]).Name+TInfo(FInfoList.Items[i]).Value+LineEnding;
  Clipboard.AsText := StrInfo;
end;

procedure TfrmAbout.actDonateExecute(Sender: TObject);
begin
  OpenURL(URL_DONATE);
end;

procedure TfrmAbout.actProjectPageExecute(Sender: TObject);
begin
  OpenURL(FVersionInfo.ProjectPage);
end;

procedure TfrmAbout.actReportDefectExecute(Sender: TObject);
begin
  OpenURL(URL_REPORT_ISSUE);
end;

procedure TfrmAbout.actHomePageExecute(Sender: TObject);
begin
  OpenURL(FVersionInfo.HomePage);
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmAbout.FillCredits;

  procedure AddCredit(const AName: string; const AAuthor: string; const AURL: string);
  var
    C: TCredit;
  begin
    C := TCredit.Create;
    C.Name := AName;
    C.Author := AAuthor;
    C.URL := AURL;
    FCredits.Add(C);
  end;

begin
  AddCredit(
    'FPC',
    'Free Pascal development team',
    'http://www.freepascal.org/'
  );
  AddCredit(
    'Lazarus',
    'Lazarus development team',
    'http://www.lazarus.freepascal.org/'
  );
  AddCredit(
    'Virtual TreeView',
    'Mike Lischke, FPC port by Luiz Américo Pereira Câmara with slight modifications by Tim Sinaeve',
    'http://www.soft-gems.net/'
  );
  AddCredit(
    'Anchor Docking',
    'Mattias Gaertner, with slight modifications by Tim Sinaeve',
    ''
  );
  AddCredit(
    'RTTI controls',
    'Mattias Gaertner',
    ''
  );
  AddCredit(
    'NativeXML',
    'Nils Haeck',
    'http://www.simdesign.nl/xml.html'
  );
  AddCredit(
    'SynEdit',
    'Lazarus development team. Original implementation by Martin Waldenburg.',
    'http://sourceforge.net/apps/mediawiki/synedit/index.php?title=SynEdit_Home_Page'
  );
  AddCredit(
    'SynFacilSyn',
    'Tito Hinostroza',
    'http://blog.pucp.edu.pe/blog/tito'
  );
  AddCredit(
    'SynUniHighlighter',
    'Original implementation by Kirill Burtsev and Vitaly Nevzorov,' + #13#10 +
    'heavily modified and ported to FPC by Tim Sinaeve.',
    ''
  );
  AddCredit(
    'Multilog',
    'Luiz Américo Pereira Câmara',
    ''
  );
  AddCredit(
    'TRichMemo',
    'Dmitry ''skalogryz'' Boyarintsev',
    ''
  );
  AddCredit(
    'OMultiPanel',
    'Ondřej Pokorný',
    'http://www.kluug.net/omultipanel.php'
  );
  AddCredit(
    'KControls',
    'Tomas Krysl',
    'http://www.tkweb.eu/en/delphicomp/kcontrols.html'
  );
  AddCredit(
    'Silk Icons',
    'Mark James',
    'http://www.famfamfam.com/lab/icons/silk/'
  );
  AddCredit(
    'Fugue Icons',
    'Yusuke Kamiyamane',
    'http://p.yusukekamiyamane.com/'
  );
end;

procedure TfrmAbout.FillTranslations;

  procedure AddTranslation(const ACode: string; const AName: string;
    const AAuthor: string);
  var
    T: TTranslation;
  begin
    T := TTranslation.Create;
    T.LanguageCode := ACode;
    T.LanguageName := AName;
    T.Author := AAuthor;
    FTranslations.Add(T);
  end;

begin
  AddTranslation(
    'en',
    'English',
    'Tim Sinaeve'
  );
  AddTranslation(
    'es',
    'Spanish',
    'Esteban Vignolo'
  );
  AddTranslation(
    'cn',
    'Chinese',
    'Anonymous contributor'
  );
  AddTranslation(
    'nl',
    'Dutch',
    'Tim Sinaeve'
  );
end;

procedure TfrmAbout.FillInfoList;
var
  S : string;

  procedure AddInfo(const AName: string; const AValue: string);
  var
    I: TInfo;
  begin
    I := TInfo.Create;
    I.Name := AName;
    I.Value := AValue;
    FInfoList.Add(I);
  end;
begin
  S := Application.ExeName;
  AddInfo('Application name:', ExtractFileName(S));
  AddInfo('Application path:', GetApplicationPath);
  AddInfo('Application size:', FormatByteText(FileSize(S)));
  AddInfo('Current path:', ExtractFilePath(ParamStr(0)));
  AddInfo('Current user:', FVersionInfo.UserName);
  //
  //AddInfo('Startup parameters', Application.ApplicationType);
  //
end;

procedure TfrmAbout.InitializeCredits;
begin
  FCredits := TObjectList.Create(True);
  FillCredits;
  FVSTCredits := VST.Create(Self, tsCredits);
  FTVPCredits := TTreeViewPresenter.Create(Self);
  FTVPCredits.ColumnDefinitions.AddColumn('Name', SName, dtString, 140);
  FTVPCredits.ColumnDefinitions.AddColumn('Author', SAuthor, dtString, 150,
    100, 220);
  FTVPCredits.OnDoubleClick := FTVPCreditsDoubleClick;
  FTVPCredits.ListMode := True;
  FTVPCredits.ItemsSource := FCredits;
  FTVPCredits.TreeView := FVSTCredits;
end;

procedure TfrmAbout.InitializeTranslations;
begin
  FTranslations := TObjectList.Create(True);
  FillTranslations;
  FVSTTranslations := VST.Create(Self, tsTranslations);
  FTVPTranslations := TTreeViewPresenter.Create(Self);
  FTVPTranslations.ColumnDefinitions.AddColumn('LanguageCode', SLanguageCode, dtString, 50);
  FTVPTranslations.ColumnDefinitions.AddColumn('LanguageName', SLanguageName, dtString, 100);
  FTVPTranslations.ColumnDefinitions.AddColumn('Author', SAuthor, dtString, 150,
    100, 220);
  FTVPTranslations.ListMode := True;
  FTVPTranslations.ItemsSource := FTranslations;
  FTVPTranslations.TreeView := FVSTTranslations;
end;

procedure TfrmAbout.InitializeInfoList;
begin
  FInfoList := TObjectList.Create(True);
  FillInfoList;
  FVSTInfoList := VST.Create(Self, tsInfo);
  FTVPInfoList := TTreeViewPresenter.Create(Self);
  FTVPInfoList.ShowHeader := False;
  FTVPInfoList.ColumnDefinitions.AddColumn('Name', SName, dtString, 110);
  FTVPInfoList.ColumnDefinitions.AddColumn('Value', SValue, dtString, 200, 200, 400);
  FTVPInfoList.ListMode := True;
  FTVPInfoList.ItemsSource := FInfoList;
  FTVPInfoList.TreeView := FVSTInfoList;
end;
{$ENDREGION}

end.

