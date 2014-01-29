object AboutBox: TAboutBox
  Left = 286
  Top = 214
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 184
  ClientWidth = 306
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 16
  object BitBtn1: TBitBtn
    Left = 114
    Top = 146
    Width = 77
    Height = 30
    TabOrder = 0
    Kind = bkOK
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 306
    Height = 137
    Align = alTop
    BevelInner = bvLowered
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 1
    object Viewer: THTMLViewer
      Left = 1
      Top = 1
      Width = 304
      Height = 135
      ViewImages = False
      Enabled = False
      TabOrder = 0
      Align = alClient
      BorderStyle = htSingle
      HistoryMaxCount = 0
      DefFontName = 'Times New Roman'
      DefPreFontName = 'Courier New'
      NoSelect = True
      ScrollBars = ssNone
      CharSet = DEFAULT_CHARSET
      PrintMarginLeft = 2.000000000000000000
      PrintMarginRight = 2.000000000000000000
      PrintMarginTop = 2.000000000000000000
      PrintMarginBottom = 2.000000000000000000
      PrintScale = 1.000000000000000000
      htOptions = []
    end
  end
end
