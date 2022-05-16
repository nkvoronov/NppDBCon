unit common;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.CheckLst,
  System.IOUtils,
  Vcl.Dialogs,
  IniFiles,
  ZAbstractConnection,
  ZConnection,
  ZSqlMetadata;

type
  TConItem = class
    ConName : String;
    Default : Boolean;
    ConType : Integer;
    DataBase : String;
    Host : String;
    Port : Integer;
    User : String;
    Password : String;
    Params : String;
    LibLocation : String;
    constructor Create;
    procedure CopyTo(Item : TConItem);
  end;

const
  DefDelimiter = ';';
  cnstMainDlgId = 0;
  Plugin_Name = 'NppDBCon';

var
  INI : TIniFile;

function GetIniFile: TIniFile;
function LoadConnection: TStringList;
function GetConnection(List: TStrings; Default: Boolean; Index: Integer):TConItem;

procedure SetConnect(AConnection : TZConnection; AI : TConItem);
procedure SaveConnection(List: TStrings);
procedure ClearListConnection(List: TStrings);

procedure ConnectionClearDefault(List: TCheckListBox);
procedure ConnectionSetDefault(List: TCheckListBox);

procedure DoConnect(AConnection : TZConnection;  AI : TConItem; isMessage : Boolean = false);
procedure DoDisconnect(AConnection : TZConnection);

implementation

function GetIniFile:TIniFile;
begin
  Result := TiniFile.Create(TPath.GetHomePath + '\Notepad++\plugins\Config\' + Plugin_Name + '.ini');
end;

procedure SetConnect(AConnection : TZConnection; AI : TConItem);
var
  List : TStringList;
begin
  List := TStringList.Create;
  try
    with AConnection Do
    begin
      GetProtocolNames(List);
      Protocol := List.Strings[AI.ConType];
      HostName := AI.Host;
      if AI.Port > 0 then Port := AI.Port;
      User := AI.User;
      Password := AI.Password;
      Database := AI.DataBase;
      List.Clear;
      Properties.Clear;
      List.Delimiter := DefDelimiter;
      List.DelimitedText := AI.Params;
      Properties.AddStrings(List);
      LibraryLocation := AI.LibLocation;
    end;
  finally
    List.Free;
  end;
end;

function LoadConnection: TStringList;
var
  oi : TConItem;
  Count, i, index : Integer;
  SectionName : string;
begin
  Result := nil;
  if INI = nil then Exit;
  Result := TStringList.Create;
  Count := INI.ReadInteger('Connections','Count',0);
  if Count = 0 then Exit;
  for i := 0 to Count - 1 do
  begin
    SectionName := 'Connection' + IntToStr(i + 1);
    oi := TConItem.Create;
    index := Result.AddObject('New Connection', oi);
    if oi <> nil then
    begin
      oi.ConName := INI.ReadString(SectionName, 'ConName', '');
      Result.Strings[i] := oi.ConName;
      oi.ConType := INI.ReadInteger(SectionName, 'ConType', -1);
      oi.Default := INI.ReadBool(SectionName, 'Default', false);
      oi.DataBase := INI.ReadString(SectionName, 'DataBase', '');
      oi.Host := INI.ReadString(SectionName, 'Host', '');
      oi.Port := INI.ReadInteger(SectionName, 'Port', 0);
      oi.User := INI.ReadString(SectionName, 'User', '');
      oi.Password := INI.ReadString(SectionName, 'Password', '');
      oi.Params := INI.ReadString(SectionName, 'Params', '');
      oi.LibLocation := INI.ReadString(SectionName, 'LibLocation', '');
    end;
  end;
end;

procedure SaveConnection(List: TStrings);
var
  oi : TConItem;
  Count, i : Integer;
  SectionName : string;
begin
  Count := List.Count;
  Ini.WriteInteger('Connections', 'Count', Count);
  for i := 0 to Count - 1 do
  begin
    SectionName := 'Connection' + IntToStr(i + 1);
    oi := TConItem(List.Objects[i]);
    if oi <> nil then
    begin
      Ini.WriteString(SectionName, 'ConName', oi.ConName);
      Ini.WriteInteger(SectionName, 'ConType', oi.ConType);
      Ini.WriteBool(SectionName, 'Default', oi.Default);
      Ini.WriteString(SectionName, 'DataBase', oi.DataBase);
      Ini.WriteString(SectionName, 'Host', oi.Host);
      Ini.WriteInteger(SectionName, 'Port', oi.Port);
      Ini.WriteString(SectionName, 'User', oi.User);
      Ini.WriteString(SectionName, 'Password', oi.Password);
      Ini.WriteString(SectionName, 'Params', oi.Params);
      Ini.WriteString(SectionName, 'LibLocation', oi.LibLocation);
    end;
  end;
end;

procedure ClearListConnection(List: TStrings);
var
  i : Integer;
  oi : TConItem;
begin
  for i := 0 to List.Count - 1 do
  begin
    oi := TConItem(List.Objects[i]);
    if oi <> nil then oi.Free;
  end;
  List.Clear;
end;

procedure ConnectionClearDefault(List: TCheckListBox);
var
  i : Integer;
  oi : TConItem;
begin
  for i := 0 to List.items.Count - 1 do
  begin
    oi := TConItem(List.items.Objects[i]);
    if (oi <> nil) and oi.Default then
    begin
      oi.Default := false;
      List.Checked[i] := false;
    end;
  end;
end;

procedure ConnectionSetDefault(List: TCheckListBox);
var
  i : Integer;
  oi : TConItem;
begin
  for i := 0 to List.items.Count - 1 do
  begin
    oi := TConItem(List.items.Objects[i]);
    if (oi <> nil) then
      List.Checked[i] := oi.Default;
  end;
end;

function GetConnection(List: TStrings; Default: Boolean; Index: Integer): TConItem;
var
  i : Integer;
  oi : TConItem;
begin
  if Default then
  begin
    Result := TConItem(List.Objects[0]);
    for i := 0 to List.Count - 1 do
    begin
      oi := TConItem(List.Objects[i]);
      if (oi <> nil) and oi.Default then
      begin
        Result := oi;
        Break;
      end;
    end
  end
  else Result := TConItem(List.Objects[Index]);
end;

procedure DoConnect(AConnection : TZConnection;  AI : TConItem; isMessage : Boolean = false);
begin
  DoDisconnect(AConnection);
  SetConnect(AConnection, AI);
  try
    AConnection.Connect;
    if Not AConnection.Connected then
    begin
      MessageDlg('No Connection' ,mtError,[mbOk], 0, mbOk);
      Exit;
    end;
    if isMessage then
      MessageDlg('Connection OK' ,mtInformation,[mbOk], 0, mbOk);
  except
    on E: Exception do
    MessageDlg(E.Message, mtError,[mbOk], 0, mbOk);
  end;
end;

procedure DoDisconnect(AConnection : TZConnection);
begin
  try
    if AConnection.Connected then
    begin
      AConnection.Disconnect;
    end;
  except
    on E: Exception do
    MessageDlg(E.Message, mtError, [mbOk], 0, mbOk);
  end;
end;

constructor TConItem.Create;
begin
  ConName := 'New Connection';
  Default := false;
  ConType := -1;
  DataBase := '';
  Host := '';
  Port := 0;
  User := '';
  Password := '';
  Params := '';
  LibLocation := '';
end;

procedure TConItem.CopyTo(Item: TConItem);
begin
  Item.ConName := ConName;
  Item.Default := Default;
  Item.ConType := ConType;
  Item.DataBase := DataBase;
  Item.Host := Host;
  Item.Port := Port;
  Item.User := User;
  Item.Password := Password;
  Item.Params := Params;
  Item.LibLocation := LibLocation;
end;

initialization
  INI := GetIniFile;

finalization
  if INI <> nil then INI.Free;

end.
