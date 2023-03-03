object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Lape'
  ClientHeight = 688
  ClientWidth = 789
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 13
  object ScriptMemo: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 783
    Height = 506
    Align = alClient
    Lines.Strings = (
      '// Issue 87'
      ''
      'type'
      '  SetOfChar = set of Char;'
      '  SetOfByte = set of Byte;'
      '  SubSetOfChar = set of '#39'a'#39'..'#39'z'#39';'
      '  SubSetOfByte = set of 100..200;'
      ''
      'var'
      '  c: Char = '#39'c'#39';'
      '  b: Byte = 102;'
      '  soc: SetOfChar     := ['#39'b'#39'..'#39'y'#39'];'
      '  sob: SetOfByte     := [101..199];'
      '  ssoc: SubSetOfChar := ['#39'b'#39'..'#39'y'#39'];'
      '  ssob: SubSetOfByte := [101..199];'
      'begin'
      '  soc  := [c..'#39'y'#39'];'
      '  sob  := [b..199];'
      '  ssoc := [c..'#39'y'#39'];'
      '  ssob := [b..199];'
      'end;')
    ScrollBars = ssBoth
    TabOrder = 0
    ExplicitWidth = 563
    ExplicitHeight = 321
  end
  object ButtonRun: TButton
    AlignWithMargins = True
    Left = 3
    Top = 515
    Width = 783
    Height = 25
    Align = alBottom
    Caption = 'Run'
    TabOrder = 1
    OnClick = ButtonRunClick
    ExplicitLeft = 8
    ExplicitTop = 231
    ExplicitWidth = 75
  end
  object OutputMemo: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 546
    Width = 783
    Height = 139
    Align = alBottom
    Lines.Strings = (
      'OutputMemo')
    TabOrder = 2
    ExplicitLeft = 8
    ExplicitTop = 262
    ExplicitWidth = 441
  end
end
