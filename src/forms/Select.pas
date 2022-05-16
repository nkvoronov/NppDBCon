unit Select;

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
  Vcl.CheckLst,
  NppForms;

type
  TfmSelect = class(TNppForm)
    lbConnections: TCheckListBox;
    pnBottom: TPanel;
    btOk: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbConnectionsClickCheck(Sender: TObject);
    procedure lbConnectionsDblClick(Sender: TObject);
  end;

implementation

{$R *.dfm}

uses
  common;

procedure TfmSelect.FormCreate(Sender: TObject);
begin
  lbConnections.Clear;
  lbConnections.Items.AddStrings(LoadConnection);
  ConnectionSetDefault(lbConnections);
  if lbConnections.Items.Count > 0 then
    lbConnections.ItemIndex := 0;
end;

procedure TfmSelect.FormDestroy(Sender: TObject);
begin
  ClearListConnection(lbConnections.Items);
end;

procedure TfmSelect.lbConnectionsClickCheck(Sender: TObject);
var
  index : integer;
  oi : TConItem;
begin
  index := lbConnections.ItemIndex;
  oi := TConItem(lbConnections.Items.Objects[index]);
  if (oi <> nil) then
    lbConnections.Checked[index] := oi.Default;
end;

procedure TfmSelect.lbConnectionsDblClick(Sender: TObject);
begin
  btOk.Click;
end;

end.
