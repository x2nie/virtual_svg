object Form1: TForm1
  Left = 192
  Top = 80
  Width = 928
  Height = 480
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btn1: TButton
    Left = 128
    Top = 48
    Width = 75
    Height = 25
    Caption = 'btn1'
    TabOrder = 0
    OnClick = btn1Click
  end
  object mmo1: TMemo
    Left = 128
    Top = 104
    Width = 185
    Height = 89
    Lines.Strings = (
      'mmo1')
    TabOrder = 1
  end
end
