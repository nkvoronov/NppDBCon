unit EditConnect;

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
  Vcl.ExtCtrls,
  Vcl.Samples.Spin,
  Vcl.Buttons,
  ZAbstractConnection,
  ZConnection,
  ADODB,
  common,
  NppForms;

type
  TfmEditConnect = class(TNppForm)
    gbConnection: TGroupBox;
    lbName: TLabel;
    lbType: TLabel;
    lbHost: TLabel;
    lbDataBase: TLabel;
    lbUser: TLabel;
    lbPassword: TLabel;
    lbPort: TLabel;
    lbParams: TLabel;
    btView: TSpeedButton;
    btOption: TSpeedButton;
    lbLibLocation: TLabel;
    btLibLocation: TSpeedButton;
    edName: TEdit;
    cbType: TComboBox;
    edHost: TEdit;
    edUser: TEdit;
    edPassword: TEdit;
    cbDataBase: TComboBox;
    mmParams: TMemo;
    sePort: TSpinEdit;
    edLibLocation: TEdit;
    pnButtom: TPanel;
    btOk: TButton;
    btCancel: TButton;
    btTest: TButton;
    OpenDialogDB: TOpenDialog;
    Connection: TZConnection;
    procedure FormCreate(Sender: TObject);
    procedure btViewClick(Sender: TObject);
    procedure btOptionClick(Sender: TObject);
    procedure btLibLocationClick(Sender: TObject);
    procedure cbTypeChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btTestClick(Sender: TObject);
    procedure cbDataBaseDropDown(Sender: TObject);
  private
    ConData : TConItem;
    procedure SetProtocols(AIsUser:Boolean);
    function isADO: boolean;
    procedure EnableFields(IsEnable:Boolean);
    procedure UpdateInterface(isClearDatabase : Boolean = false);
    procedure ClearEdit;
    procedure SetCatalogs;
  public
    procedure LoadToEdit(data : TConItem);
    procedure SaveFromEdit(data : TConItem);
  end;

implementation

{$R *.dfm}

uses
  View;

procedure TfmEditConnect.SetProtocols(AIsUser: Boolean);
begin
  if not AIsUser then  Connection.GetProtocolNames(cbType.Items)
  else
  begin
    With cbType.Items Do
    begin
      Clear;
      Add('ado');
      Add('postgresql-8');
      Add('postgresql-9');
      Add('firebird-2.0');
      Add('firebird-2.1');
      Add('firebird-2.5');
      Add('firebird-3.0');
    end;
  end;
end;

function TfmEditConnect.isADO: boolean;
begin
  Result := cbType.Items.Strings[cbType.ItemIndex] = 'ado';
end;

procedure TfmEditConnect.EnableFields(IsEnable: Boolean);
begin
  edHost.Enabled := IsEnable;
  sePort.Enabled := IsEnable;
  edUser.Enabled := IsEnable;
  edPassword.Enabled := IsEnable;
  mmParams.Enabled := IsEnable;
  edLibLocation.Enabled := IsEnable;
  btLibLocation.Enabled := IsEnable;
end;

procedure TfmEditConnect.UpdateInterface(isClearDatabase : Boolean = false);
begin
  EnableFields(not isADO);
  if (Pos('firebird', cbType.Items.Strings[cbType.ItemIndex]) > 0)
  or(Pos('interbase', cbType.Items.Strings[cbType.ItemIndex]) > 0)
  or(Pos('sqlite', cbType.Items.Strings[cbType.ItemIndex]) > 0)
  or isADO
  then btOption.Enabled := true
  else btOption.Enabled := false;
  if True then
  if isClearDatabase then
    cbDataBase.Clear;
end;

procedure TfmEditConnect.ClearEdit;
begin
  edName.Clear;
  cbType.ItemIndex := -1;
  cbDataBase.Clear;
  edHost.Clear;
  sePort.Value := 0;
  edUser.Clear;
  edPassword.Clear;
  mmParams.Clear;
  edLibLocation.Clear;
end;

procedure TfmEditConnect.FormCreate(Sender: TObject);
begin
  SetProtocols(false);
  ConData := TConItem.Create;
  mmParams.Clear;
  mmParams.Lines.Delimiter := DefDelimiter;
end;

procedure TfmEditConnect.FormDestroy(Sender: TObject);
begin
  ConData.Free;
  DoDisconnect(Connection);
end;

procedure TfmEditConnect.SetCatalogs;
var
 Catalogs : TStringList;
begin
  SaveFromEdit(ConData);
  DoConnect(Connection, ConData);
  Catalogs := TStringList.Create;
  try
    Connection.GetCatalogNames(Catalogs);
    if Catalogs.Count > 0 then
    begin
      cbDataBase.Items.Clear;
      cbDataBase.Items.AddStrings(Catalogs);
    end;
  finally
    Catalogs.Free;
  end;
end;

procedure TfmEditConnect.cbDataBaseDropDown(Sender: TObject);
begin
  if (cbDataBase.Items.Count = 0) and not isADO then
    SetCatalogs;
  if isADO then
    cbDataBase.Items.Clear;
end;

procedure TfmEditConnect.cbTypeChange(Sender: TObject);
begin
  UpdateInterface(true);
end;

procedure TfmEditConnect.btViewClick(Sender: TObject);
var
  EView: TfmView;
begin
  try
    EView := TfmView.Create(Self);
    EView.mmView.Lines.Text := cbDataBase.Text;
    if EView.ShowModal = mrOK then
      cbDataBase.Text := EView.mmView.Lines.Text;
  finally
    EView.Free;
  end;
end;

procedure TfmEditConnect.btOptionClick(Sender: TObject);
var
  ConectionStr:string;
begin
  if isADO then
  begin
    ConectionStr := PromptDataSource(Handle, cbDataBase.Text);
    cbDataBase.Text := ConectionStr;
  end
  else
  begin
    OpenDialogDB.Filter := '*.*';
    if cbDataBase.Text <> '' then
      OpenDialogDB.InitialDir := ExtractFilePath(cbDataBase.Text);
    if OpenDialogDB.Execute then
      cbDataBase.Text := OpenDialogDB.FileName;
  end;
end;

procedure TfmEditConnect.btLibLocationClick(Sender: TObject);
var
  Dir: string;
begin
  OpenDialogDB.Filter := '*.dll';
  if edLibLocation.Text <> '' then
    OpenDialogDB.InitialDir := ExtractFilePath(edLibLocation.Text);
  if OpenDialogDB.Execute then
    edLibLocation.Text := OpenDialogDB.FileName;
end;

procedure TfmEditConnect.btTestClick(Sender: TObject);
begin
  SaveFromEdit(ConData);
  DoConnect(Connection, ConData, true);
end;

procedure TfmEditConnect.LoadToEdit(data : TConItem);
begin
  if data <> nil then
  with data do
  begin
    edName.Text := ConName;
    cbType.ItemIndex := ConType;
    cbDataBase.Text := DataBase;
    edHost.Text := Host;
    sePort.Value := Port;
    edUser.Text := User;
    edPassword.Text := Password;
    mmParams.Lines.DelimitedText := Params;
    edLibLocation.Text := LibLocation;
  end;
  UpdateInterface;
end;

procedure TfmEditConnect.SaveFromEdit(data: TConItem);
begin
  if data <> nil then
  with data do
  begin
    ConName := edName.Text;
    ConType := cbType.ItemIndex;
    DataBase := cbDataBase.Text;
    Host := edHost.Text;
    Port := sePort.Value;
    User := edUser.Text;
    Password := edPassword.Text;
    Params := mmParams.Lines.DelimitedText;
    LibLocation := edLibLocation.Text;
  end;
end;

end.
