unit SqlResult;

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
  Data.DB,
  Vcl.Grids,
  Vcl.DBGrids,
  ZAbstractRODataset,
  ZAbstractDataset,
  ZDataset,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  ZAbstractConnection,
  ZConnection,
  ZSqlMonitor,
  System.ImageList,
  Vcl.ImgList,
  JvExDBGrids,
  JvDBGrid,
  JvDBUltimGrid,
  ZSqlProcessor,
  NppDockingForms,
  NppPlugin;

type
  TfmSQLResult = class(TNppDockingForm)
    pcSQLResult: TPageControl;
    tsMessages: TTabSheet;
    tsResult: TTabSheet;
    mmMessages: TMemo;
    dsResult: TDataSource;
    qResult: TZQuery;
    Connection: TZConnection;
    Monitor: TZSQLMonitor;
    ilResult: TImageList;
    ResultGrid: TJvDBUltimGrid;
    SQLProcessor: TZSQLProcessor;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MonitorLogTrace(Sender: TObject; Event: TZLoggingEvent);
  private
    FSQLText: string;
    FShowSelect: Boolean;
    FStartDefCon: Boolean;
    FCon: TStringList;
    FMenuItemCheck: TMenuItemCheck;
    procedure SetMenuItemCheck(const Value: TMenuItemCheck);
    procedure LoadSettings;
  public
    procedure DoSql(const SqlText: string);
    procedure SelItem;
    property MenuItemCheck: TMenuItemCheck read FMenuItemCheck write SetMenuItemCheck;
  end;

implementation

{$R *.dfm}

uses
  common,
  Select;

{ TfmSQLResult }

procedure TfmSQLResult.DoSql(const SqlText: string);
var
  index : integer;
  Select : TfmSelect;
begin
  LoadSettings;
  FSQLText := SqlText;
  if FShowSelect then
  begin
    Select := TfmSelect.Create(self);
    try
      if Select.ShowModal = mrOK then
      begin
        index := Select.lbConnections.ItemIndex;
      end;
    finally
      Select.Free;
    end;
  end;
  DoConnect(Connection, GetConnection(FCon, not FShowSelect, index));
  mmMessages.Lines.Clear;
  qResult.SQL.Text := FSQLText;
  qResult.Open;
  pcSQLResult.ActivePageIndex := 0;
end;

procedure TfmSQLResult.FormCreate(Sender: TObject);
begin
  FSQLText := '';
  FShowSelect := false;
  FStartDefCon := false;
  pcSQLResult.ActivePageIndex := 0;
  FCon := TStringList.Create;
  LoadSettings;
  if FStartDefCon then
    DoConnect(Connection, GetConnection(FCon, true, 0));
end;

procedure TfmSQLResult.FormDestroy(Sender: TObject);
begin
  ClearListConnection(FCon);
  FCon.Free;
  DoDisconnect(Connection);
end;

procedure TfmSQLResult.LoadSettings;
begin
  FShowSelect := INI.ReadBool('Main', 'ShowListConnections', false);
  FStartDefCon := INI.ReadBool('Main', 'StartDefaultConnection', false);
  FCon.Clear;
  FCon.AddStrings(LoadConnection);
end;

procedure TfmSQLResult.MonitorLogTrace(Sender: TObject; Event: TZLoggingEvent);
begin
  mmMessages.Lines.Add(DateTimeToStr(Event.Timestamp) + ' : ' + Event.Message);
end;

procedure TfmSQLResult.SelItem;
begin
  if MenuItemCheck  = miShown then
  begin
    Hide;
    MenuItemCheck := miHidden;
  end
  else
  begin
    Show;
    MenuItemCheck := miShown;
  end;
end;

procedure TfmSQLResult.SetMenuItemCheck(const Value: TMenuItemCheck);
begin
  if Value <> FMenuItemCheck then
  begin
    SendMessage(Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, CmdId, LPARAM(Value));
    FMenuItemCheck := Value;
  end;
end;

end.
