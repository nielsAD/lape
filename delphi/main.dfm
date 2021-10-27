object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Lape'
  ClientHeight = 410
  ClientWidth = 457
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ScriptMemo: TMemo
    Left = 8
    Top = 8
    Width = 441
    Height = 217
    Lines.Strings = (
      'begin'
      '  WriteLn('#39'Hello World'#39');'
      'end;')
    TabOrder = 0
  end
  object ButtonRun: TButton
    Left = 8
    Top = 231
    Width = 75
    Height = 25
    Caption = 'Run'
    TabOrder = 1
    OnClick = ButtonRunClick
  end
  object OutputMemo: TMemo
    Left = 8
    Top = 262
    Width = 441
    Height = 139
    Lines.Strings = (
      'OutputMemo')
    TabOrder = 2
  end
end
