object fmEditConnect: TfmEditConnect
  Left = 0
  Top = 0
  Caption = 'Edit Connect'
  ClientHeight = 286
  ClientWidth = 507
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object gbConnection: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 501
    Height = 250
    Align = alClient
    Caption = 'Connection'
    TabOrder = 0
    object lbName: TLabel
      Left = 5
      Top = 19
      Width = 27
      Height = 13
      Caption = 'Name'
    end
    object lbType: TLabel
      Left = 5
      Top = 46
      Width = 24
      Height = 13
      Caption = 'Type'
    end
    object lbHost: TLabel
      Left = 5
      Top = 101
      Width = 22
      Height = 13
      Caption = 'Host'
    end
    object lbDataBase: TLabel
      Left = 5
      Top = 73
      Width = 46
      Height = 13
      Caption = 'DataBase'
    end
    object lbUser: TLabel
      Left = 5
      Top = 128
      Width = 22
      Height = 13
      Caption = 'User'
    end
    object lbPassword: TLabel
      Left = 248
      Top = 128
      Width = 46
      Height = 13
      Caption = 'Password'
    end
    object lbPort: TLabel
      Left = 346
      Top = 101
      Width = 20
      Height = 13
      Caption = 'Port'
    end
    object lbParams: TLabel
      Left = 5
      Top = 152
      Width = 35
      Height = 13
      Caption = 'Params'
    end
    object btView: TSpeedButton
      Left = 451
      Top = 70
      Width = 21
      Height = 21
      Caption = 'V'
      OnClick = btViewClick
    end
    object btOption: TSpeedButton
      Left = 472
      Top = 70
      Width = 21
      Height = 21
      Caption = '...'
      Enabled = False
      OnClick = btOptionClick
    end
    object lbLibLocation: TLabel
      Left = 5
      Top = 222
      Width = 33
      Height = 13
      Caption = 'Library'
    end
    object btLibLocation: TSpeedButton
      Left = 474
      Top = 219
      Width = 21
      Height = 21
      Caption = '...'
      OnClick = btLibLocationClick
    end
    object edName: TEdit
      AlignWithMargins = True
      Left = 60
      Top = 18
      Width = 433
      Height = 21
      TabOrder = 0
    end
    object cbType: TComboBox
      Left = 60
      Top = 43
      Width = 433
      Height = 21
      TabOrder = 1
      OnChange = cbTypeChange
    end
    object edHost: TEdit
      Left = 60
      Top = 97
      Width = 280
      Height = 21
      TabOrder = 3
    end
    object edUser: TEdit
      Left = 60
      Top = 124
      Width = 182
      Height = 21
      TabOrder = 5
    end
    object edPassword: TEdit
      Left = 302
      Top = 125
      Width = 191
      Height = 21
      PasswordChar = '*'
      TabOrder = 6
    end
    object cbDataBase: TComboBox
      Left = 60
      Top = 70
      Width = 390
      Height = 21
      TabOrder = 2
      OnDropDown = cbDataBaseDropDown
    end
    object mmParams: TMemo
      Left = 60
      Top = 152
      Width = 433
      Height = 61
      Lines.Strings = (
        '')
      ScrollBars = ssVertical
      TabOrder = 7
    end
    object sePort: TSpinEdit
      Left = 372
      Top = 97
      Width = 121
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 4
      Value = 0
    end
    object edLibLocation: TEdit
      Left = 60
      Top = 219
      Width = 411
      Height = 21
      TabOrder = 8
    end
  end
  object pnButtom: TPanel
    Left = 0
    Top = 256
    Width = 507
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btOk: TButton
      AlignWithMargins = True
      Left = 348
      Top = 3
      Width = 75
      Height = 24
      Align = alRight
      Caption = 'Ok'
      ModalResult = 1
      TabOrder = 1
    end
    object btCancel: TButton
      AlignWithMargins = True
      Left = 429
      Top = 3
      Width = 75
      Height = 24
      Align = alRight
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object btTest: TButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 75
      Height = 24
      Align = alLeft
      Caption = 'Test'
      TabOrder = 0
      OnClick = btTestClick
    end
  end
  object OpenDialogDB: TOpenDialog
    Title = 'Select File'
    Left = 156
    Top = 170
  end
  object Connection: TZConnection
    ControlsCodePage = cCP_UTF16
    AutoEncodeStrings = True
    Catalog = ''
    SQLHourGlass = True
    HostName = ''
    Port = 0
    Database = ''
    User = ''
    Password = ''
    Protocol = ''
    Left = 84
    Top = 170
  end
end
