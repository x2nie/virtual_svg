object Form1: TForm1
  Left = 169
  Top = 136
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
    Left = 144
    Top = 64
    Width = 75
    Height = 25
    Caption = 'btn1'
    TabOrder = 0
    OnClick = btn1Click
  end
  object mmo1: TMemo
    Left = 40
    Top = 104
    Width = 185
    Height = 249
    Lines.Strings = (
      'mmo1')
    TabOrder = 1
  end
  object mmo2: TMemo
    Left = 280
    Top = 104
    Width = 209
    Height = 249
    Lines.Strings = (
      'mmo2')
    TabOrder = 2
  end
  object btn2: TButton
    Left = 416
    Top = 64
    Width = 75
    Height = 25
    Caption = 'btn2'
    TabOrder = 3
    OnClick = btn2Click
  end
  object mmo3: TMemo
    Left = 520
    Top = 104
    Width = 193
    Height = 249
    Lines.Strings = (
      'mmo3')
    TabOrder = 4
  end
  object btn3: TButton
    Left = 640
    Top = 64
    Width = 75
    Height = 25
    Caption = 'btn3'
    TabOrder = 5
    OnClick = btn3Click
  end
end
