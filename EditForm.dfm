object EditDetailForm: TEditDetailForm
  Left = 199
  Top = 277
  Width = 357
  Height = 183
  Caption = 'Edit detail'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 24
    Width = 53
    Height = 13
    Caption = 'Description'
  end
  object Label2: TLabel
    Left = 16
    Top = 64
    Width = 24
    Height = 13
    Caption = 'Price'
  end
  object AcceptBtn: TButton
    Left = 80
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Accept'
    TabOrder = 0
    OnClick = AcceptBtnClick
  end
  object CancelBtn: TButton
    Left = 184
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = CancelBtnClick
  end
  object EditDescription: TEdit
    Left = 80
    Top = 16
    Width = 233
    Height = 21
    TabOrder = 2
  end
  object EditPrice: TEdit
    Left = 80
    Top = 56
    Width = 121
    Height = 21
    TabOrder = 3
  end
  object CheckBox: TCheckBox
    Left = 232
    Top = 64
    Width = 81
    Height = 17
    Caption = 'Check'
    TabOrder = 4
  end
end
