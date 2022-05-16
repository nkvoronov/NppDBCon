unit Settings;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  Vcl.Samples.Spin,
  Vcl.CheckLst,
  ZAbstractConnection,
  ZConnection,
  NppForms;

type
  TfmSettings = class(TNppForm)
    pnButtons: TPanel;
    btCancel: TButton;
    btOk: TButton;
    gbConnections: TGroupBox;
    lbConnections: TCheckListBox;
    pnEdtBtn: TPanel;
    btAdd: TButton;
    btDel: TButton;
    btTest: TButton;
    ckbShowListConnections: TCheckBox;
    btEdit: TButton;
    Connection: TZConnection;
    ckbStartDefaultConnection: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btTestClick(Sender: TObject);
    procedure btAddClick(Sender: TObject);
    procedure btDelClick(Sender: TObject);
    procedure btEditClick(Sender: TObject);
    procedure lbConnectionsDblClick(Sender: TObject);
    procedure lbConnectionsClickCheck(Sender: TObject);
  private
    procedure LoadSettings;
    procedure SaveSettings;
    procedure UpdateInterface;
    procedure SetDefault;
    procedure AddConnection;
    procedure EditConnection;
    procedure DeleteConnection;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  common,
  EditConnect;

procedure TfmSettings.LoadSettings;
begin
  ckbShowListConnections.Checked := INI.ReadBool('Main', 'ShowListConnections', false);
  ckbStartDefaultConnection.Checked := INI.ReadBool('Main', 'StartDefaultConnection', false);
  lbConnections.Clear;
  lbConnections.Items.AddStrings(LoadConnection);
  ConnectionSetDefault(lbConnections);
end;

procedure TfmSettings.SaveSettings;
begin
  INI.WriteBool('Main', 'ShowListConnections', ckbShowListConnections.Checked);
  INI.WriteBool('Main', 'StartDefaultConnection', ckbStartDefaultConnection.Checked);
  SaveConnection(lbConnections.Items);
end;

procedure TfmSettings.UpdateInterface;
begin
  btTest.Enabled := lbConnections.Items.Count > 0;
  btEdit.Enabled := lbConnections.Items.Count > 0;
  btDel.Enabled := lbConnections.Items.Count > 0;
end;

procedure TfmSettings.SetDefault;
var
  index : integer;
  oi : TConItem;
  check : boolean;
begin
  index := lbConnections.ItemIndex;
  check := lbConnections.Checked[index];
  oi := TConItem(lbConnections.Items.Objects[index]);
  if (oi <> nil) and (oi.Default <> check) then
  begin
    if check then ConnectionClearDefault(lbConnections);
    oi.Default := lbConnections.Checked[index];
  end;
end;

procedure TfmSettings.AddConnection;
var
  index : integer;
  oi : TConItem;
begin
  oi := TConItem.Create;
  oi.ConName := 'New Connection';
  index := lbConnections.Items.AddObject('New Connection', oi);
  lbConnections.ItemIndex := index;
end;

procedure TfmSettings.EditConnection;
var
  index : integer;
  oi : TConItem;
  EditConnect : TfmEditConnect;
begin
  if lbConnections.Items.Count = 0 then Exit;
  index := lbConnections.ItemIndex;
  oi := TConItem(lbConnections.Items.Objects[index]);
  if oi <> nil then
  begin
    EditConnect := TfmEditConnect.Create(self);
    try
      EditConnect.LoadToEdit(oi);
      if EditConnect.ShowModal = mrOK then
      begin
        EditConnect.SaveFromEdit(oi);
        lbConnections.Items.Strings[index] := oi.ConName;
      end;
    finally
      EditConnect.Free;
    end;
  end;
end;

procedure TfmSettings.DeleteConnection;
var
  index : integer;
  oi : TConItem;
begin
  if lbConnections.Items.Count = 0 then Exit;
  index := lbConnections.ItemIndex;
  oi := TConItem(lbConnections.Items.Objects[index]);
  if oi <> nil then oi.Free;
  lbConnections.Items.Delete(index);
  if index > 0 then
    lbConnections.ItemIndex := index - 1;
end;

procedure TfmSettings.FormCreate(Sender: TObject);
begin
  Caption := Plugin_Name + ' Settings';
  LoadSettings;
  if lbConnections.Items.Count > 0 then
    lbConnections.ItemIndex := 0;
  UpdateInterface;
end;

procedure TfmSettings.FormDestroy(Sender: TObject);
begin
  SaveSettings;
  ClearListConnection(lbConnections.Items);
  DoDisconnect(Connection);
end;

procedure TfmSettings.lbConnectionsClickCheck(Sender: TObject);
begin
  SetDefault;
end;

procedure TfmSettings.lbConnectionsDblClick(Sender: TObject);
begin
  EditConnection;
end;

procedure TfmSettings.btTestClick(Sender: TObject);
var
  index : integer;
  oi : TConItem;
begin
  if lbConnections.Items.Count = 0 then Exit;
  index := lbConnections.ItemIndex;
  oi := TConItem(lbConnections.Items.Objects[index]);
  if oi <> nil then
    DoConnect(Connection, oi, true);
end;

procedure TfmSettings.btAddClick(Sender: TObject);
begin
  AddConnection;
  UpdateInterface;
end;

procedure TfmSettings.btEditClick(Sender: TObject);
begin
  EditConnection;
end;

procedure TfmSettings.btDelClick(Sender: TObject);
begin
  DeleteConnection;
  UpdateInterface;
end;

end.
