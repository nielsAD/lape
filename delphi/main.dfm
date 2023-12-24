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
      'begin'
      '  WriteLn('#39'Hola'#39');'
      'end.')
    ScrollBars = ssBoth
    TabOrder = 0
    ExplicitWidth = 777
    ExplicitHeight = 497
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
    ExplicitTop = 506
    ExplicitWidth = 777
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
    ExplicitTop = 537
    ExplicitWidth = 777
  end
end
