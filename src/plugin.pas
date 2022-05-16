unit Plugin;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Controls,
  common,
  NppPlugin,
  SciSupport,
  SqlResult,
  DBBrowser,
  Settings,
  About;

type
  TDBconPlugin = class(TNppPlugin)
  private
    FFormBrowser: TForm;
    FFormResult: TForm;
  public
    constructor Create;
    procedure FuncExecSQL;
    procedure FuncDBBrowser;
    procedure FuncSqlResult;
    procedure FuncSettings;
    procedure FuncAbout;
    procedure DoNppnToolbarModification; override;
  end;

procedure _FuncExecSQL; cdecl;
procedure _FuncDBBrowser; cdecl;
procedure _FuncSqlResult; cdecl;
procedure _FuncSettings; cdecl;
procedure _FuncAbout; cdecl;

var
  NPlugin: TDBconPlugin;

implementation

procedure _FuncExecSQL; cdecl;
begin
  NPlugin.FuncExecSQL;
end;

procedure _FuncDBBrowser; cdecl;
begin
  NPlugin.FuncDBBrowser;
end;

procedure _FuncSqlResult; cdecl;
begin
  NPlugin.FuncSqlResult;
end;

procedure _FuncSettings; cdecl;
begin
  NPlugin.FuncSettings;
end;

procedure _FuncAbout; cdecl;
begin
  NPlugin.FuncAbout;
end;

{ THelloWorldPlugin }

constructor TDBconPlugin.Create;
var
  sk: TShortcutKey;
begin
  inherited;

  self.PluginName := Plugin_Name;

  sk.IsCtrl := true; sk.IsAlt := false; sk.IsShift := false;
  sk.Key := 69; // 'E'
  AddFuncItem('Execute SQL', _FuncExecSQL, sk);
  AddFuncItem('Show BD Browser', _FuncDBBrowser);
  AddFuncItem('Show Result', _FuncSqlResult);
  AddFuncItem('-', nil);
  AddFuncItem('Settings', _FuncSettings);
  AddFuncItem('-', nil);
  AddFuncItem('About', _FuncAbout);
end;

procedure TDBconPlugin.FuncExecSQL;
var S: string;
    N: integer;
begin
  S := SelectedText;
  N := Length(S);

  if N < 1 then
  begin
    S := GetText;
    N := Length(S);
  end;

  if N > 1 then
  begin
    if not Assigned(FFormResult) then
    begin
      FuncSqlResult;
    end;

    if TfmSQLResult(FFormResult).MenuItemCheck = miHidden then
    begin
      TfmSQLResult(FFormResult).SelItem;
    end;

    TfmSQLResult(FFormResult).DoSql(S);
  end;
end;

procedure TDBconPlugin.FuncDBBrowser;
begin
  if not Assigned(FFormBrowser) then FFormBrowser := TfmDBBrowser.Create(self, cnstMainDlgId + 1);
  (FFormBrowser as TfmDBBrowser).SelItem;
end;

procedure TDBconPlugin.FuncSqlResult;
begin
  if (not Assigned(FFormResult)) then FFormResult := TfmSQLResult.Create(self, cnstMainDlgId + 2);
  (FFormResult as TfmSQLResult).SelItem;
end;

procedure TDBconPlugin.FuncSettings;
var
  Settings: TfmSettings;
begin
  Settings := TfmSettings.Create(self);
  try
    if Settings.ShowModal = mrOK then
    begin
      //
    end;
  finally
    Settings.Free;
  end;
end;

procedure TDBconPlugin.FuncAbout;
var
  About: TfmAbout;
begin
  About := TfmAbout.Create(self);
  try
    About.ShowModal;
  finally
    About.Free;
  end;
end;

procedure TDBconPlugin.DoNppnToolbarModification;
var
  tb: TToolbarIcons;
begin
  tb.ToolbarIcon := 0;

  tb.ToolbarBmp := LoadImage(Hinstance, 'SQL', IMAGE_BITMAP, 0, 0, (LR_DEFAULTSIZE));
  Npp_Send(NPPM_ADDTOOLBARICON, WPARAM(self.CmdIdFromDlgId(cnstMainDlgId)), LPARAM(@tb));

  tb.ToolbarBmp := LoadImage(Hinstance, 'TREE', IMAGE_BITMAP, 0, 0, (LR_DEFAULTSIZE));
  Npp_Send(NPPM_ADDTOOLBARICON, WPARAM(self.CmdIdFromDlgId(cnstMainDlgId + 1)), LPARAM(@tb));

  tb.ToolbarBmp := LoadImage(Hinstance, 'RESULT', IMAGE_BITMAP, 0, 0, (LR_DEFAULTSIZE));
  Npp_Send(NPPM_ADDTOOLBARICON, WPARAM(self.CmdIdFromDlgId(cnstMainDlgId + 2)), LPARAM(@tb));
end;

initialization
  NPlugin := TDBconPlugin.Create;
end.
