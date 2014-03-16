object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 663
  ClientWidth = 940
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object LblTimeInfo: TLabel
    Left = 16
    Top = 144
    Width = 83
    Height = 13
    Caption = 'Time values in [s]'
  end
  object BtnReadPerformanceTest: TButton
    Left = 16
    Top = 16
    Width = 193
    Height = 25
    Caption = 'Read Performance Test'
    TabOrder = 0
    OnClick = BtnReadPerformanceTestClick
  end
  object BtnWritePerformanceTest: TButton
    Left = 216
    Top = 16
    Width = 193
    Height = 25
    Caption = 'Write Performance Test'
    TabOrder = 1
    OnClick = BtnWritePerformanceTestClick
  end
  object BtnResaveTest: TButton
    Left = 416
    Top = 16
    Width = 193
    Height = 25
    Caption = 'Resave Test'
    TabOrder = 2
    OnClick = BtnResaveTestClick
  end
  object BtnAttributeTest: TButton
    Left = 616
    Top = 16
    Width = 193
    Height = 25
    Caption = 'Attribute Performance Test'
    TabOrder = 3
    OnClick = BtnAttributeTestClick
  end
  object BtnXmlDirectWrite: TButton
    Left = 16
    Top = 48
    Width = 193
    Height = 25
    Caption = 'Xml Direct Write - simple.xml'
    TabOrder = 4
    OnClick = BtnXmlDirectWriteClick
  end
  object BtnTestSAX: TButton
    Left = 216
    Top = 48
    Width = 193
    Height = 25
    Caption = 'Test SAX Read'
    TabOrder = 5
    OnClick = BtnTestSAXClick
  end
  object BtnDOMTest: TButton
    Left = 415
    Top = 48
    Width = 194
    Height = 25
    Caption = 'DOM Level 1.0 Test'
    TabOrder = 6
    OnClick = BtnDOMTestClick
  end
  object BtnIterateTest: TButton
    Left = 16
    Top = 80
    Width = 193
    Height = 25
    Caption = 'Iterate Test'
    TabOrder = 7
    OnClick = BtnIterateTestClick
  end
  object BtnSequentialTest: TButton
    Left = 216
    Top = 80
    Width = 193
    Height = 25
    Caption = 'Sequential Test'
    TabOrder = 8
    OnClick = BtnSequentialTestClick
  end
  object BtnTestXPath: TButton
    Left = 416
    Top = 80
    Width = 193
    Height = 25
    Caption = 'XPath Test'
    TabOrder = 9
    OnClick = BtnTestXPathClick
  end
  object BtnTestReadInvalid: TButton
    Left = 16
    Top = 112
    Width = 193
    Height = 25
    Caption = 'Test read invalid document'
    TabOrder = 10
    OnClick = BtnTestReadInvalidClick
  end
  object BtnTestWriteInvalid: TButton
    Left = 216
    Top = 112
    Width = 193
    Height = 25
    Caption = 'Test write invalid document'
    TabOrder = 11
    OnClick = BtnTestWriteInvalidClick
  end
  object BtnEncodingTest: TButton
    Left = 416
    Top = 112
    Width = 193
    Height = 25
    Caption = 'Custom Encoding Test'
    TabOrder = 12
    OnClick = BtnEncodingTestClick
  end
  object Memo1: TMemo
    Left = 16
    Top = 160
    Width = 289
    Height = 409
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssBoth
    TabOrder = 13
  end
  object Memo2: TMemo
    Left = 312
    Top = 160
    Width = 289
    Height = 409
    Lines.Strings = (
      'Memo2')
    ScrollBars = ssBoth
    TabOrder = 14
  end
end
