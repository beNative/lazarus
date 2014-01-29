object GoPageForm: TGoPageForm
  Left = 305
  Top = 180
  ActiveControl = PageNum
  BorderStyle = bsDialog
  Caption = 'Go to Page Number'
  ClientHeight = 116
  ClientWidth = 231
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = [fsBold]
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 11
    Top = 8
    Width = 209
    Height = 57
    Shape = bsFrame
    IsControl = True
  end
  object OKBtn: TBitBtn
    Left = 35
    Top = 76
    Width = 77
    Height = 27
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    Kind = bkOK
    Spacing = 3
    IsControl = True
  end
  object CancelBtn: TBitBtn
    Left = 119
    Top = 76
    Width = 77
    Height = 27
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    Kind = bkCancel
    Spacing = 3
    IsControl = True
  end
  object PageNum: TSpinEdit
    Left = 87
    Top = 27
    Width = 57
    Height = 22
    MaxValue = 0
    MinValue = 1
    TabOrder = 0
    Value = 0
    OnEnter = PageNumEnter
    OnKeyDown = PageNumKeyDown
  end
end
