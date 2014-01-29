object FontForm: TFontForm
  Left = 200
  Top = 153
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Edit Default Font and Colors'
  ClientHeight = 339
  ClientWidth = 307
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 225
    Top = 7
    Width = 66
    Height = 16
    Caption = '&Font Color'
    FocusControl = FontColorGrid
  end
  object Label2: TLabel
    Left = 225
    Top = 99
    Width = 65
    Height = 16
    Caption = '&Link Color'
    FocusControl = HotSpotColorGrid
  end
  object Label3: TLabel
    Left = 225
    Top = 189
    Width = 61
    Height = 16
    Caption = 'Font &Size'
    FocusControl = FontSizeEdit
  end
  object Label4: TLabel
    Left = 9
    Top = 200
    Width = 70
    Height = 16
    Caption = 'Font &Name'
    FocusControl = FontListBox
  end
  object Label5: TLabel
    Left = 122
    Top = 200
    Width = 77
    Height = 16
    Caption = '&Background'
    FocusControl = BackListBox
  end
  object FontListBox: TListBox
    Left = 11
    Top = 217
    Width = 87
    Height = 115
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 13
    ParentFont = False
    Sorted = True
    TabOrder = 0
    OnClick = ListBoxClicks
  end
  object FontColorGrid: TColorGrid
    Left = 225
    Top = 23
    Width = 72
    Height = 72
    ClickEnablesColor = True
    ForegroundIndex = -1
    ForegroundEnabled = False
    BackgroundEnabled = False
    Selection = 3
    TabOrder = 2
    TabStop = True
    OnChange = FontColorGridChange
  end
  object HotSpotColorGrid: TColorGrid
    Left = 225
    Top = 114
    Width = 72
    Height = 72
    ClickEnablesColor = True
    ForegroundIndex = -1
    ForegroundEnabled = False
    BackgroundEnabled = False
    TabOrder = 3
    TabStop = True
    OnChange = HotSpotColorGridChange
  end
  object BackListBox: TListBox
    Left = 122
    Top = 217
    Width = 87
    Height = 115
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 13
    ParentFont = False
    TabOrder = 1
    OnClick = ListBoxClicks
  end
  object OKButton: TButton
    Left = 234
    Top = 274
    Width = 54
    Height = 25
    Caption = 'O&K'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object Cancel: TButton
    Left = 234
    Top = 301
    Width = 54
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object ResetButton: TButton
    Left = 234
    Top = 246
    Width = 54
    Height = 25
    Caption = '&Reset'
    TabOrder = 6
    OnClick = ResetButtonClick
  end
  object FontSizeEdit: TSpinEdit
    Left = 225
    Top = 206
    Width = 65
    Height = 26
    MaxValue = 24
    MinValue = 6
    TabOrder = 4
    Value = 12
    OnChange = ListBoxClicks
  end
  object FontViewer: THTMLViewer
    Left = 15
    Top = 16
    Width = 193
    Height = 177
    Cursor = 2
    ViewImages = False
    TabOrder = 8
    BorderStyle = htSingle
    HistoryMaxCount = 0
    DefFontName = 'Times New Roman'
    DefPreFontName = 'Courier New'
    NoSelect = True
    ScrollBars = ssVertical
    CharSet = DEFAULT_CHARSET
    PrintMarginLeft = 2.000000000000000000
    PrintMarginRight = 2.000000000000000000
    PrintMarginTop = 2.000000000000000000
    PrintMarginBottom = 2.000000000000000000
    PrintScale = 1.000000000000000000
    htOptions = []
  end
end
