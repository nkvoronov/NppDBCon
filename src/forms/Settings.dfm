object fmSettings: TfmSettings
  Left = 0
  Top = 0
  ActiveControl = btCancel
  BorderStyle = bsDialog
  Caption = 'Settings'
  ClientHeight = 255
  ClientWidth = 512
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
  object pnButtons: TPanel
    Left = 0
    Top = 225
    Width = 512
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btCancel: TButton
      AlignWithMargins = True
      Left = 434
      Top = 3
      Width = 75
      Height = 24
      Align = alRight
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btOk: TButton
      AlignWithMargins = True
      Left = 353
      Top = 3
      Width = 75
      Height = 24
      Align = alRight
      Caption = 'Ok'
      ModalResult = 1
      TabOrder = 0
    end
  end
  object gbConnections: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 506
    Height = 156
    Align = alTop
    Caption = 'Connections'
    TabOrder = 0
    object lbConnections: TCheckListBox
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 496
      Height = 97
      OnClickCheck = lbConnectionsClickCheck
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      ItemHeight = 13
      TabOrder = 0
      OnDblClick = lbConnectionsDblClick
    end
    object pnEdtBtn: TPanel
      AlignWithMargins = True
      Left = 5
      Top = 121
      Width = 496
      Height = 30
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object btAdd: TButton
        AlignWithMargins = True
        Left = 256
        Top = 3
        Width = 75
        Height = 24
        Align = alRight
        Caption = 'Add'
        TabOrder = 1
        OnClick = btAddClick
      end
      object btDel: TButton
        AlignWithMargins = True
        Left = 418
        Top = 3
        Width = 75
        Height = 24
        Align = alRight
        Caption = 'Delete'
        TabOrder = 3
        OnClick = btDelClick
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
      object btEdit: TButton
        AlignWithMargins = True
        Left = 337
        Top = 3
        Width = 75
        Height = 24
        Align = alRight
        Caption = 'Edit'
        TabOrder = 2
        OnClick = btEditClick
      end
    end
  end
  object ckbShowListConnections: TCheckBox
    Left = 8
    Top = 165
    Width = 137
    Height = 17
    Caption = 'Show List Connections'
    TabOrder = 1
  end
  object ckbStartDefaultConnection: TCheckBox
    Left = 8
    Top = 188
    Width = 145
    Height = 17
    Caption = 'Start Default Connection'
    TabOrder = 3
  end
  object Connection: TZConnection
    ControlsCodePage = cCP_UTF16
    AutoEncodeStrings = True
    Catalog = ''
    Properties.Strings = (
      'AutoEncodeStrings=ON')
    SQLHourGlass = True
    HostName = ''
    Port = 0
    Database = ''
    User = ''
    Password = ''
    Protocol = ''
    Left = 28
    Top = 58
  end
end
