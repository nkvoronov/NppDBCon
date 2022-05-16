unit DBBrowser;

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
  NppDockingForms,
  ZAbstractConnection,
  ZConnection,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Data.DB,
  ZAbstractRODataset,
  ZSqlMetadata,
  System.ImageList,
  Vcl.ImgList,
  Vcl.Grids,
  Vcl.DBGrids,
  JvExDBGrids,
  JvDBGrid,
  JvDBUltimGrid,
  NppPlugin;

type
  TTypeItem = (tiNone, tiSchema, tiSchemaItem, tiTables, tiTablesItem,
    tiTablesColumn, tiTablesPrimaryKeys, tiTablesForeginKeys, tiTablesChecks,
    tiTablesIndex, tiTablesTriggers, tiTablesColumnItem, tiTablesPrimaryKeysItem,
    tiTablesForeginKeysItem, tiTablesChecksItem, tiTablesIndexItem,
    tiTablesTriggersItem, tiProcedures, tiProceduresItem, tiProceduresColumn,
    tiProceduresColumnItem, tiSequences, tiSequencesItem);

  TTreeItem = class
    TypeItem: TTypeItem;
    TypeMD: TZMetadataType;
    Catalog: string;
    Schema: string;
    DBName: string;
    Filter: string;
    Sort: string;
    constructor Create;
  end;

  TfmDBBrowser = class(TNppDockingForm)
    Connection: TZConnection;
    btConnect: TButton;
    cbConnection: TComboBox;
    MetaData: TZSQLMetadata;
    ilDBBrowser: TImageList;
    dsMetaData: TDataSource;
    tvDBBrowser: TTreeView;
    spBottom: TSplitter;
    dbgInfo: TJvDBUltimGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btConnectClick(Sender: TObject);
    procedure tvDBBrowserChange(Sender: TObject; Node: TTreeNode);
    procedure tvDBBrowserExpanding(Sender: TObject; Node: TTreeNode; var
      AllowExpansion: Boolean);
    procedure FormResize(Sender: TObject);
  private
    FShowSelect: Boolean;
    FStartDefCon: Boolean;
    FCatalog: string;
    FMenuItemCheck: TMenuItemCheck;
    procedure SetMenuItemCheck(const Value: TMenuItemCheck);
    procedure LoadSettings;
    function GetMetaData(AType: TZMetadataType; ACatalog: string = ''; ASchema:
      string = ''; ADBName: string = ''; AFilter: string = ''; AUnique: Boolean
      = false; ASort: string = ''): TZSQLMetadata;
    function GetCountMetaData(AType: TZMetadataType; ACatalog: string = '';
      ASchema: string = ''; ADBName: string = ''; AFilter: string = ''; AUnique:
      Boolean = false): Integer;
    procedure CreateTMPNode(AParent: TTreeNode);
    procedure ClearTreeNodeData;
    procedure CreateTree;
    procedure SetCatalogs;
    procedure SetSchemas;
    procedure InitTableInfo;
    procedure SetTypeTables(AParent: TTreeNode; ATi: TTreeItem);
    procedure SetTablesItem(AParent: TTreeNode; ATi: TTreeItem);
    procedure SetProceduresItem(AParent: TTreeNode; ATi: TTreeItem);
    procedure SetTablesContent(AParent: TTreeNode; ATi: TTreeItem);
    procedure SetTablesColumn(AParent: TTreeNode; ATi: TTreeItem);
    procedure SetTablesColumnInfo(ATi: TTreeItem; ACol: Boolean);
    procedure CreateColumnsTable;
    procedure SetTablesPrimaryKey(AParent: TTreeNode; ATi: TTreeItem);
    procedure SetTablesPrimaryKeyInfo(ATi: TTreeItem; AKey: Boolean);
    procedure CreatePrimaryKeyTable;
    procedure SetTablesForeginKey(AParent: TTreeNode; ATi: TTreeItem);
    procedure SetTablesForeginKeyInfo(ATi: TTreeItem; AKey: Boolean);
    procedure CreateForeginKeyTable;
    procedure SetTablesCheck(AParent: TTreeNode; ATi: TTreeItem);
    procedure SetTablesIndex(AParent: TTreeNode; ATi: TTreeItem);
    procedure SetTablesIndexInfo(ATi: TTreeItem; AIdx: Boolean);
    procedure CreateIndexsTable;
    procedure SetTablesTrigger(AParent: TTreeNode; ATi: TTreeItem);
    procedure SetProcedures(AParent: TTreeNode; ATi: TTreeItem);
    procedure SetProceduresContent(AParent: TTreeNode; ATi: TTreeItem);
    procedure SetProceduresColumn(AParent: TTreeNode; ATi: TTreeItem);
    procedure SetProceduresColumnInfo(ATi: TTreeItem; ACol: Boolean);
    procedure CreateColumnProcedures;
    procedure SetSequences(AParent: TTreeNode; ATi: TTreeItem);
    procedure SetSequencesItem(AParent: TTreeNode; ATi: TTreeItem);
    function SetTreeList(Node: TTreeNode): Boolean;
    procedure SetTableInfo(Node: TTreeNode);
  public
    procedure SelItem;
    property MenuItemCheck: TMenuItemCheck read FMenuItemCheck write SetMenuItemCheck;
  end;

implementation

{$R *.dfm}

uses
  common;

{ TfmDBBrowser }

procedure TfmDBBrowser.btConnectClick(Sender: TObject);
begin
  DoConnect(Connection, GetConnection(cbConnection.Items, false, cbConnection.ItemIndex));
  CreateTree;
end;

procedure TfmDBBrowser.SetCatalogs;
var
  md: TZSQLMetadata;
  count: Integer;
  Catalogs: TStringList;
begin
  Catalogs := TStringList.Create;
  try
    Connection.GetCatalogNames(Catalogs);
    if Catalogs.Count > 0 then
      FCatalog := GetConnection(cbConnection.Items, false, cbConnection.ItemIndex).DataBase
    else
      FCatalog := '';
  finally
    Catalogs.Free;
  end;
end;

procedure TfmDBBrowser.SetSchemas;
var
  md: TZSQLMetadata;
  ASchema, Filter: string;
  Node, Parent: TTreeNode;
  count: Integer;
  ti: TTreeItem;
begin
  md := GetMetaData(mdSchemas, '', '', '', '', true, 'TABLE_SCHEM');
  try
    md.Open;
    count := md.RecordCount;
    if count > 0 then
    begin
      ti := TTreeItem.Create;
      ti.TypeItem := tiSchema;
      ti.TypeMD := mdSchemas;
      ti.Catalog := FCatalog;
      Parent := tvDBBrowser.Items.AddChildObject(nil, 'schema (' + IntToStr(count)
        + ')', Pointer(ti));
      md.First;
      while not md.Eof do
      begin
        ASchema := md.FieldByName('TABLE_SCHEM').AsString;
        ti := TTreeItem.Create;
        ti.TypeItem := tiSchemaItem;
        ti.TypeMD := mdSchemas;
        ti.Schema := ASchema;
        ti.Catalog := FCatalog;
        Node := tvDBBrowser.Items.AddChildObject(Parent, LowerCase(ASchema), Pointer(ti));
        SetTypeTables(Node, ti);
        SetProcedures(Node, ti);
        SetSequences(Node, ti);
        Node.Expand(false);
        md.Next;
      end;
      Parent.Expand(false);
    end
    else
    begin
      SetTypeTables(nil, nil);
      SetProcedures(nil, nil);
      SetSequences(nil, nil);
    end;
  finally
    md.Free;
  end;
end;

procedure TfmDBBrowser.SetTypeTables(AParent: TTreeNode; ATi: TTreeItem);
var
  md: TZSQLMetadata;
  ATableType, AFilter, ASchema: string;
  Node: TTreeNode;
  count: Integer;
  ti: TTreeItem;
begin
  if ATi <> nil then
    ASchema := ATi.Schema
  else
    ASchema := '';

  md := GetMetaData(mdTableTypes, '', '', '', '', true, 'TABLE_TYPE');
  try
    md.Open;
    md.First;
    while not md.Eof do
    begin
      ATableType := md.FieldByName('TABLE_TYPE').AsString;
      AFilter := 'TABLE_TYPE = ''' + ATableType + '''';
      count := GetCountMetaData(mdTables, FCatalog, ASchema, ATableType, AFilter, true);
      if count > 0 then
      begin
        ti := TTreeItem.Create;
        ti.TypeItem := tiTables;
        ti.TypeMD := mdTables;
        ti.Catalog := FCatalog;
        ti.Schema := ASchema;
        ti.DBName := ATableType;
        ti.Filter := AFilter;
        ti.Sort := 'TABLE_NAME';
        Node := tvDBBrowser.Items.AddChildObject(AParent, LowerCase(ATableType)
          + ' (' + IntToStr(count) + ')', Pointer(ti));
        CreateTMPNode(Node);
      end;
      md.Next;
    end;
  finally
    md.Free;
  end;
end;

procedure TfmDBBrowser.SetTablesItem(AParent: TTreeNode; ATi: TTreeItem);
var
  md: TZSQLMetadata;
  ATableName, AFilter: string;
  Node: TTreeNode;
  count: Integer;
  ti: TTreeItem;
begin
  md := GetMetaData(ATi.TypeMD, ATi.Catalog, ATi.Schema, ATi.DBName, ATi.Filter,
    true, ATi.Sort);
  try
    md.Open;
    md.First;
    while not md.Eof do
    begin
      ATableName := md.FieldByName('TABLE_NAME').AsString;
      AFilter := '';
      ti := TTreeItem.Create;
      ti.TypeItem := tiTablesItem;
      ti.TypeMD := ATi.TypeMD;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.DBName := ATableName;
      ti.Filter := AFilter;
      Node := tvDBBrowser.Items.AddChildObject(AParent, LowerCase(ATableName),
        Pointer(ti));
      CreateTMPNode(Node);
      md.Next;
    end;
  finally
    md.Free;
  end;
end;

procedure TfmDBBrowser.SetTablesContent(AParent: TTreeNode; ATi: TTreeItem);
var
  Node: TTreeNode;
  count: Integer;
  ti: TTreeItem;
  Filter: string;
begin
  // Columns
  try
    count := GetCountMetaData(mdColumns, ATi.Catalog, ATi.Schema, ATi.DBName,
      ATi.Filter, false);
    if count > 0 then
    begin
      ti := TTreeItem.Create;
      ti.TypeItem := tiTablesColumn;
      ti.TypeMD := mdColumns;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.Filter := ATi.Filter;
      ti.DBName := ATi.DBName;
      ti.Sort := 'ORDINAL_POSITION';
      Node := tvDBBrowser.Items.AddChildObject(AParent, 'column (' + IntToStr(count)
        + ')', Pointer(ti));
      CreateTMPNode(Node);
    end;
  except
    //
  end;
  // Primary Keys
  try
    count := GetCountMetaData(mdPrimaryKeys, ATi.Catalog, ATi.Schema, ATi.DBName,
      ATi.Filter, false);
    if count > 0 then
    begin
      ti := TTreeItem.Create;
      ti.TypeItem := tiTablesPrimaryKeys;
      ti.TypeMD := mdPrimaryKeys;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.Filter := ATi.Filter;
      ti.DBName := ATi.DBName;
      ti.Sort := 'PK_NAME';
      Node := tvDBBrowser.Items.AddChildObject(AParent, 'primary key (' +
        IntToStr(count) + ')', Pointer(ti));
      CreateTMPNode(Node);
    end;
  except
    //
  end;
  // Foregin Keys
  try
    Filter := '';
    count := GetCountMetaData(mdImportedKeys, ATi.Catalog, ATi.Schema, ATi.DBName,
      ATi.Filter, false);
    if count > 0 then
    begin
      ti := TTreeItem.Create;
      ti.TypeItem := tiTablesForeginKeys;
      ti.TypeMD := mdImportedKeys;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.Filter := Filter;
      ti.DBName := ATi.DBName;
      ti.Sort := '';
      Node := tvDBBrowser.Items.AddChildObject(AParent, 'foreign key (' +
        IntToStr(count) + ')', Pointer(ti));
      CreateTMPNode(Node);
    end;
  except
    //
  end;
  // Checks
  try
    count := GetCountMetaData(mdExportedKeys, ATi.Catalog, ATi.Schema, ATi.DBName,
      ATi.Filter, false);
    if count > 0 then
    begin
      ti := TTreeItem.Create;
      ti.TypeItem := tiTablesChecks;
      ti.TypeMD := mdExportedKeys;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.Filter := ATi.Filter;
      ti.DBName := ATi.DBName;
      ti.Sort := '';
      Node := tvDBBrowser.Items.AddChildObject(AParent, 'check (' + IntToStr(count)
        + ')', Pointer(ti));
      CreateTMPNode(Node);
    end;
  except
    //
  end;
  //  Index
  try
    count := GetCountMetaData(mdIndexInfo, ATi.Catalog, ATi.Schema, ATi.DBName,
      ATi.Filter, false);
    if count > 0 then
    begin
      ti := TTreeItem.Create;
      ti.TypeItem := tiTablesIndex;
      ti.TypeMD := mdIndexInfo;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.Filter := ATi.Filter;
      ti.DBName := ATi.DBName;
      ti.Sort := 'INDEX_NAME';
      Node := tvDBBrowser.Items.AddChildObject(AParent, 'index (' + IntToStr(count)
        + ')', Pointer(ti));
      CreateTMPNode(Node);
    end;
  except
    //
  end;
  // Triggers
  try
    Filter := '';
    count := GetCountMetaData(mdTriggers, ATi.Catalog, ATi.Schema, ATi.DBName,
      ATi.Filter, false);
    if count > 0 then
    begin
      ti := TTreeItem.Create;
      ti.TypeItem := tiTablesTriggers;
      ti.TypeMD := mdTriggers;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.Filter := Filter;
      ti.DBName := ATi.DBName;
      ti.Sort := 'TRIGGER_NAME';
      Node := tvDBBrowser.Items.AddChildObject(AParent, 'trigger (' + IntToStr(count)
        + ')', Pointer(ti));
      CreateTMPNode(Node);
    end;
  except
    //
  end;
end;

procedure TfmDBBrowser.SetTablesColumn(AParent: TTreeNode; ATi: TTreeItem);
var
  md: TZSQLMetadata;
  AColumnName, AFilter: string;
  Node: TTreeNode;
  count: Integer;
  ti: TTreeItem;
begin
  md := GetMetaData(ATi.TypeMD, ATi.Catalog, ATi.Schema, ATi.DBName, ATi.Filter,
    false, ATi.Sort);
  try
    md.Open;
    md.First;
    while not md.Eof do
    begin
      AColumnName := md.FieldByName('COLUMN_NAME').AsString;
      AFilter := 'COLUMN_NAME = ''' + AColumnName + '''';
      ti := TTreeItem.Create;
      ti.TypeItem := tiTablesColumnItem;
      ti.TypeMD := ATi.TypeMD;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.DBName := ATi.DBName;
      ti.Filter := AFilter;
      Node := tvDBBrowser.Items.AddChildObject(AParent, LowerCase(AColumnName),
        Pointer(ti));
      md.Next;
    end;
  finally
    md.Free;
  end;
end;

procedure TfmDBBrowser.SetTablesColumnInfo(ATi: TTreeItem; ACol: Boolean);
begin
  MetaData.MetadataType := ATi.TypeMD;
  MetaData.Catalog := ATi.Catalog;
  MetaData.Schema := ATi.Schema;
  MetaData.TableName := ATi.DBName;
  MetaData.SortedFields := ATi.Sort;
  MetaData.Filtered := ACol;
  if ACol then
    MetaData.Filter := ATi.Filter
  else
    MetaData.Filter := '';
  CreateColumnsTable;
  MetaData.Open;
end;

procedure TfmDBBrowser.SetTablesPrimaryKey(AParent: TTreeNode; ATi: TTreeItem);
var
  md: TZSQLMetadata;
  APKName, AFilter: string;
  Node: TTreeNode;
  count: Integer;
  ti: TTreeItem;
  pkNames : TStringList;
begin
  md := GetMetaData(ATi.TypeMD, ATi.Catalog, ATi.Schema, ATi.DBName, ATi.Filter,
    false, ATi.Sort);
  pkNames := TStringList.Create;
  try
    md.Open;
    md.First;
    while not md.Eof do
    begin
      APKName := md.FieldByName('PK_NAME').AsString;
      AFilter := 'PK_NAME = ''' + APKName + '''';
      if Trim(APKName) = '' Then APKName := 'NO NAME';
      ti := TTreeItem.Create;
      ti.TypeItem := tiTablesPrimaryKeysItem;
      ti.TypeMD := ATi.TypeMD;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.DBName := ATi.DBName;
      ti.Filter := AFilter;
      if pkNames.IndexOf(APKName) = -1 then
      begin
        Node := tvDBBrowser.Items.AddChildObject(AParent, LowerCase(APKName),
          Pointer(ti));
        pkNames.Add(APKName)
      end;
      md.Next;
    end;
  finally
    pkNames.Free;
    md.Free;
  end;
end;

procedure TfmDBBrowser.SetTablesPrimaryKeyInfo(ATi: TTreeItem; AKey: Boolean);
begin
  MetaData.MetadataType := ATi.TypeMD;
  MetaData.Catalog := ATi.Catalog;
  MetaData.Schema := ATi.Schema;
  MetaData.TableName := ATi.DBName;
  MetaData.SortedFields := ATi.Sort;
  MetaData.Filtered := AKey;
  if AKey then
    MetaData.Filter := ATi.Filter
  else
    MetaData.Filter := '';
  CreatePrimaryKeyTable;
  MetaData.Open;
end;

procedure TfmDBBrowser.SetTablesForeginKey(AParent: TTreeNode; ATi: TTreeItem);
var
  md: TZSQLMetadata;
  APKName, AFilter: string;
  Node: TTreeNode;
  count: Integer;
  ti: TTreeItem;
  pkNames : TStringList;
begin
  md := GetMetaData(ATi.TypeMD, ATi.Catalog, ATi.Schema, ATi.DBName, ATi.Filter,
    false, ATi.Sort);
  pkNames := TStringList.Create;
  try
    md.Open;
    md.First;
    while not md.Eof do
    begin
      APKName := md.FieldByName('FK_NAME').AsString;
      AFilter := 'FK_NAME = ''' + APKName + '''';
      if Trim(APKName) = '' Then APKName := 'NO NAME';
      ti := TTreeItem.Create;
      ti.TypeItem := tiTablesForeginKeysItem;
      ti.TypeMD := ATi.TypeMD;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.DBName := ATi.DBName;
      ti.Filter := AFilter;
      if pkNames.IndexOf(APKName) = -1 then
      begin
        Node := tvDBBrowser.Items.AddChildObject(AParent, LowerCase(APKName),
          Pointer(ti));
        pkNames.Add(APKName)
      end;
      md.Next;
    end;
  finally
    pkNames.Free;
    md.Free;
  end;
end;

procedure TfmDBBrowser.SetTablesForeginKeyInfo(ATi: TTreeItem; AKey: Boolean);
begin
  MetaData.MetadataType := ATi.TypeMD;
  MetaData.Catalog := ATi.Catalog;
  MetaData.Schema := ATi.Schema;
  MetaData.TableName := ATi.DBName;
  MetaData.SortedFields := ATi.Sort;
  MetaData.Filtered := AKey;
  if AKey then
    MetaData.Filter := ATi.Filter
  else
    MetaData.Filter := '';
  CreateForeginKeyTable;
  MetaData.Open;
end;

procedure TfmDBBrowser.SetTablesCheck(AParent: TTreeNode; ATi: TTreeItem);
begin
  //
end;

procedure TfmDBBrowser.SetTablesIndex(AParent: TTreeNode; ATi: TTreeItem);
var
  md: TZSQLMetadata;
  AIndexName, AFilter: string;
  Node: TTreeNode;
  count: Integer;
  ti: TTreeItem;
  IndexNames : TStringList;
begin
  md := GetMetaData(ATi.TypeMD, ATi.Catalog, ATi.Schema, ATi.DBName, ATi.Filter,
    false, ATi.Sort);
  IndexNames := TStringList.Create;
  try
    md.Open;
    md.First;
    while not md.Eof do
    begin
      AIndexName := md.FieldByName('INDEX_NAME').AsString;
      if Trim(AIndexName) = '' Then AIndexName := 'NO NAME';
      AFilter := 'INDEX_NAME = ''' + AIndexName + '''';
      ti := TTreeItem.Create;
      ti.TypeItem := tiTablesIndexItem;
      ti.TypeMD := ATi.TypeMD;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.DBName := ATi.DBName;
      ti.Filter := AFilter;
      if IndexNames.IndexOf(AIndexName) = -1 then
      begin
        Node := tvDBBrowser.Items.AddChildObject(AParent, LowerCase(AIndexName),
          Pointer(ti));
        IndexNames.Add(AIndexName)
      end;
      md.Next;
    end;
  finally
    IndexNames.Free;
    md.Free;
  end;
end;

procedure TfmDBBrowser.SetTablesIndexInfo(ATi: TTreeItem; AIdx: Boolean);
begin
  MetaData.MetadataType := ATi.TypeMD;
  MetaData.Catalog := ATi.Catalog;
  MetaData.Schema := ATi.Schema;
  MetaData.TableName := ATi.DBName;
  MetaData.SortedFields := ATi.Sort;
  MetaData.Filtered := AIdx;
  if AIdx then
    MetaData.Filter := ATi.Filter
  else
    MetaData.Filter := '';
  CreateIndexsTable;
  MetaData.Open;
end;

procedure TfmDBBrowser.SetTablesTrigger(AParent: TTreeNode; ATi: TTreeItem);
var
  md: TZSQLMetadata;
  ATriggerName, AFilter: string;
  Node: TTreeNode;
  count: Integer;
  ti: TTreeItem;
begin
  md := GetMetaData(ATi.TypeMD, ATi.Catalog, ATi.Schema, ATi.DBName, ATi.Filter,
    false, ATi.Sort);
  try
    md.Open;
    md.First;
    while not md.Eof do
    begin
      ATriggerName := md.FieldByName('TRIGGER_NAME').AsString;
      AFilter := 'TRIGGER_NAME = ''' + ATriggerName + '''';
      ti := TTreeItem.Create;
      ti.TypeItem := tiTablesTriggersItem;
      ti.TypeMD := ATi.TypeMD;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.DBName := ATi.DBName;
      ti.Filter := AFilter;
      Node := tvDBBrowser.Items.AddChildObject(AParent, LowerCase(ATriggerName),
        Pointer(ti));
      md.Next;
    end;
  finally
    md.Free;
  end;
end;

procedure TfmDBBrowser.SetProcedures(AParent: TTreeNode; ATi: TTreeItem);
var
  Node: TTreeNode;
  count: Integer;
  ti: TTreeItem;
  ASchema: string;
begin
  if ATi <> nil then
    ASchema := ATi.Schema
  else
    ASchema := '';

  count := GetCountMetaData(mdProcedures, FCatalog, ASchema, '', '', false);
  if count > 0 then
  begin
    ti := TTreeItem.Create;
    ti.TypeItem := tiProcedures;
    ti.TypeMD := mdProcedures;
    ti.Catalog := FCatalog;
    ti.Schema := ASchema;
    ti.Sort := 'PROCEDURE_NAME';
    Node := tvDBBrowser.Items.AddChildObject(AParent, 'procedure (' + IntToStr(count)
      + ')', Pointer(ti));
    CreateTMPNode(Node);
  end;
end;

procedure TfmDBBrowser.SetProceduresItem(AParent: TTreeNode; ATi: TTreeItem);
var
  md: TZSQLMetadata;
  AProcedureName, AFilter: string;
  Node: TTreeNode;
  count: Integer;
  ti: TTreeItem;
begin
  md := GetMetaData(ATi.TypeMD, ATi.Catalog, ATi.Schema, ATi.DBName, ATi.Filter,
    false, ATi.Sort);
  try
    md.Open;
    md.First;
    while not md.Eof do
    begin
      AProcedureName := md.FieldByName('PROCEDURE_NAME').AsString;
      AFilter := 'PROCEDURE_NAME = ''' + AProcedureName + '''';
      ti := TTreeItem.Create;
      ti.TypeItem := tiProceduresItem;
      ti.TypeMD := ATi.TypeMD;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.DBName := AProcedureName;
      ti.Filter := AFilter;
      Node := tvDBBrowser.Items.AddChildObject(AParent, LowerCase(AProcedureName),
        Pointer(ti));
      CreateTMPNode(Node);
      md.Next;
    end;
  finally
    md.Free;
  end;
end;

procedure TfmDBBrowser.SetProceduresContent(AParent: TTreeNode; ATi: TTreeItem);
var
  Node: TTreeNode;
  count: Integer;
  ti: TTreeItem;
begin
  // Columns
  try
    count := GetCountMetaData(mdProcedureColumns, ATi.Catalog, ATi.Schema, ATi.DBName,
      ATi.Filter, false);
    if count > 0 then
    begin
      ti := TTreeItem.Create;
      ti.TypeItem := tiProceduresColumn;
      ti.TypeMD := mdProcedureColumns;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.DBName := ATi.DBName;
      ti.Filter := ATi.Filter;
      ti.Sort := 'COLUMN_TYPE';
      Node := tvDBBrowser.Items.AddChildObject(AParent, 'column (' + IntToStr(count)
        + ')', Pointer(ti));
      CreateTMPNode(Node);
    end;
  except
    //
  end;
end;

procedure TfmDBBrowser.SetProceduresColumn(AParent: TTreeNode; ATi: TTreeItem);
var
  md: TZSQLMetadata;
  AColumnName, AFilter: string;
  Node: TTreeNode;
  count: Integer;
  ti: TTreeItem;
begin
  md := GetMetaData(ATi.TypeMD, ATi.Catalog, ATi.Schema, ATi.DBName, ATi.Filter,
    false, ATi.Sort);
  try
    md.Open;
    md.First;
    while not md.Eof do
    begin
      AColumnName := md.FieldByName('COLUMN_NAME').AsString;
      AFilter := 'COLUMN_NAME = ''' + AColumnName + '''';
      ti := TTreeItem.Create;
      ti.TypeItem := tiProceduresColumnItem;
      ti.TypeMD := ATi.TypeMD;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.DBName := ATi.DBName;
      ti.Filter := AFilter;
      Node := tvDBBrowser.Items.AddChildObject(AParent, LowerCase(AColumnName),
        Pointer(ti));
      md.Next;
    end;
  finally
    md.Free;
  end;
end;

procedure TfmDBBrowser.SetProceduresColumnInfo(ATi: TTreeItem; ACol: Boolean);
begin
  MetaData.MetadataType := ATi.TypeMD;
  MetaData.Catalog := ATi.Catalog;
  MetaData.Schema := ATi.Schema;
  MetaData.ProcedureName := ATi.DBName;
  MetaData.SortedFields := ATi.Sort;
  MetaData.Filtered := ACol;
  if ACol then
    MetaData.Filter := ATi.Filter
  else
    MetaData.Filter := '';
  CreateColumnProcedures;
  MetaData.Open;
end;

procedure TfmDBBrowser.SetSequences(AParent: TTreeNode; ATi: TTreeItem);
var
  Node: TTreeNode;
  count: Integer;
  ti: TTreeItem;
  ASchema: string;
begin
  if ATi <> nil then
    ASchema := ATi.Schema
  else
    ASchema := '';

  count := GetCountMetaData(mdSequences, FCatalog, ASchema, '', '', false);
  if count > 0 then
  begin
    ti := TTreeItem.Create;
    ti.TypeItem := tiSequences;
    ti.TypeMD := mdSequences;
    ti.Catalog := FCatalog;
    ti.Schema := ASchema;
    ti.Sort := 'SEQUENCE_NAME';
    Node := tvDBBrowser.Items.AddChildObject(AParent, 'sequence (' + IntToStr(count)
      + ')', Pointer(ti));
    CreateTMPNode(Node);
  end;
end;

procedure TfmDBBrowser.SetSequencesItem(AParent: TTreeNode; ATi: TTreeItem);
var
  md: TZSQLMetadata;
  ASequenceName, AFilter: string;
  Node: TTreeNode;
  count: Integer;
  ti: TTreeItem;
begin
  md := GetMetaData(ATi.TypeMD, ATi.Catalog, ATi.Schema, ATi.DBName, ATi.Filter,
    false, ATi.Sort);
  try
    md.Open;
    md.First;
    while not md.Eof do
    begin
      ASequenceName := md.FieldByName('SEQUENCE_NAME').AsString;
      AFilter := 'SEQUENCE_NAME = ''' + ASequenceName + '''';
      ti := TTreeItem.Create;
      ti.TypeItem := tiSequencesItem;
      ti.TypeMD := ATi.TypeMD;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.DBName := ASequenceName;
      ti.Filter := AFilter;
      Node := tvDBBrowser.Items.AddChildObject(AParent, LowerCase(ASequenceName),
        Pointer(ti));
      md.Next;
    end;
  finally
    md.Free;
  end;
end;

procedure TfmDBBrowser.tvDBBrowserExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  AllowExpansion := SetTreeList(Node);
end;

function TfmDBBrowser.SetTreeList(Node: TTreeNode): Boolean;
var
  FirstNode: TTreeNode;
  ti: TTreeItem;
begin
  Result := True;
  FirstNode := Node.GetFirstChild;
  if (FirstNode <> nil) and (FirstNode.Text = 'TMP') then
  begin
    ti := TTreeItem(Node.Data);
    tvDBBrowser.Items.BeginUpdate;
    try
      if ti <> nil then
      begin
        if ti.TypeItem in [tiTables, tiTablesItem, tiTablesColumn, tiTablesPrimaryKeys,
        tiTablesIndex, tiTablesTriggers, tiProcedures, tiProceduresItem, tiProceduresColumn,
        tiSequences] then
          tvDBBrowser.Items.Delete(FirstNode);
        case ti.TypeItem of
          tiTables:
            SetTablesItem(Node, ti);
          tiTablesItem:
            SetTablesContent(Node, ti);
          tiTablesColumn:
            SetTablesColumn(Node, ti);
          tiTablesPrimaryKeys:
            SetTablesPrimaryKey(Node, ti);
          tiTablesForeginKeys:
            Result := False;
          tiTablesChecks:
            Result := False;
          tiTablesIndex:
            SetTablesIndex(Node, ti);
          tiTablesTriggers:
            SetTablesTrigger(Node, ti);
          tiTablesColumnItem:
            Result := False;
          tiTablesPrimaryKeysItem:
            Result := False;
          tiTablesForeginKeysItem:
            Result := False;
          tiTablesChecksItem:
            Result := False;
          tiTablesIndexItem:
            Result := False;
          tiTablesTriggersItem:
            Result := False;
          tiProcedures:
            SetProceduresItem(Node, ti);
          tiProceduresItem:
            SetProceduresContent(Node, ti);
          tiProceduresColumn:
            SetProceduresColumn(Node, ti);
          tiProceduresColumnItem:
            Result := False;
          tiSequences:
            SetSequencesItem(Node, ti);
          tiSequencesItem:
            Result := False;
        end;
      end;
    finally
      tvDBBrowser.Items.EndUpdate;
    end;
  end;
end;

procedure TfmDBBrowser.tvDBBrowserChange(Sender: TObject; Node: TTreeNode);
begin
  SetTableInfo(Node);
end;

procedure TfmDBBrowser.SetTableInfo(Node: TTreeNode);
var
  ti: TTreeItem;
begin
  InitTableInfo;
  ti := TTreeItem(Node.Data);
  if ti <> nil then
    case ti.TypeItem of
      tiSchema:
        ;
      tiSchemaItem:
        ;
      tiTables:
        ;
      tiTablesItem:
        ;
      tiTablesColumn:
        SetTablesColumnInfo(ti,false);
      tiTablesPrimaryKeys:
        SetTablesPrimaryKeyInfo(ti,false);
      tiTablesForeginKeys:
        SetTablesForeginKeyInfo(ti,false);
      tiTablesChecks:
        ;
      tiTablesIndex:
        SetTablesIndexInfo(ti,false);
      tiTablesTriggers:
        ;
      tiTablesColumnItem:
        SetTablesColumnInfo(ti,true);
      tiTablesPrimaryKeysItem:
        SetTablesPrimaryKeyInfo(ti,true);
      tiTablesForeginKeysItem:
        SetTablesForeginKeyInfo(ti,true);
      tiTablesChecksItem:
        ;
      tiTablesIndexItem:
        SetTablesIndexInfo(ti,true);
      tiTablesTriggersItem:
        ;
      tiProcedures:
        ;
      tiProceduresItem:
        ;
      tiProceduresColumn:
        SetProceduresColumnInfo(ti,false);
      tiProceduresColumnItem:
        SetProceduresColumnInfo(ti,true);
      tiSequences:
        ;
      tiSequencesItem:
        ;
    end;
end;

procedure TfmDBBrowser.ClearTreeNodeData;
var
  i: Integer;
  ti: TTreeItem;
begin
  for i := 0 to tvDBBrowser.Items.Count - 1 do
  begin
    ti := TTreeItem(tvDBBrowser.Items.Item[i].Data);
    if ti <> nil then
      ti.Free;
  end;
  tvDBBrowser.Items.Clear;
end;

procedure TfmDBBrowser.CreateColumnProcedures;
begin
  dbgInfo.Columns.Add;
  dbgInfo.Columns[0].FieldName := 'COLUMN_NAME';
  dbgInfo.Columns[0].Title.Caption := 'Name';
  dbgInfo.Columns[0].Title.Alignment := taCenter;
  dbgInfo.Columns[0].Width := 150;

  dbgInfo.Columns.Add;
  dbgInfo.Columns[1].FieldName := 'TYPE_NAME';
  dbgInfo.Columns[1].Title.Caption := 'Type';
  dbgInfo.Columns[1].Title.Alignment := taCenter;
  dbgInfo.Columns[1].Width := 75;

  dbgInfo.Columns.Add;
  dbgInfo.Columns[2].FieldName := 'PRECISION';
  dbgInfo.Columns[2].Title.Caption := 'Size';
  dbgInfo.Columns[2].Title.Alignment := taCenter;
  dbgInfo.Columns[2].Width := 50;

  dbgInfo.Columns.Add;
  dbgInfo.Columns[3].FieldName := 'NULLABLE';
  dbgInfo.Columns[3].Title.Caption := 'Null';
  dbgInfo.Columns[3].Title.Alignment := taCenter;
  dbgInfo.Columns[3].Width := 50;

  dbgInfo.Columns.Add;
  dbgInfo.Columns[4].FieldName := 'REMARKS';
  dbgInfo.Columns[4].Title.Caption := 'Remarks';
  dbgInfo.Columns[4].Title.Alignment := taCenter;
  dbgInfo.Columns[4].Width := 150;
end;

procedure TfmDBBrowser.CreateColumnsTable;
begin
  dbgInfo.Columns.Add;
  dbgInfo.Columns[0].FieldName := 'ORDINAL_POSITION';
  dbgInfo.Columns[0].Title.Caption := '#';
  dbgInfo.Columns[0].Title.Alignment := taCenter;
  dbgInfo.Columns[0].Width := 30;

  dbgInfo.Columns.Add;
  dbgInfo.Columns[1].FieldName := 'COLUMN_NAME';
  dbgInfo.Columns[1].Title.Caption := 'Name';
  dbgInfo.Columns[1].Title.Alignment := taCenter;
  dbgInfo.Columns[1].Width := 150;

  dbgInfo.Columns.Add;
  dbgInfo.Columns[2].FieldName := 'TYPE_NAME';
  dbgInfo.Columns[2].Title.Caption := 'Type';
  dbgInfo.Columns[2].Title.Alignment := taCenter;
  dbgInfo.Columns[2].Width := 75;

  dbgInfo.Columns.Add;
  dbgInfo.Columns[3].FieldName := 'COLUMN_SIZE';
  dbgInfo.Columns[3].Title.Caption := 'Size';
  dbgInfo.Columns[3].Title.Alignment := taCenter;
  dbgInfo.Columns[3].Width := 50;

  dbgInfo.Columns.Add;
  dbgInfo.Columns[4].FieldName := 'COLUMN_DEF';
  dbgInfo.Columns[4].Title.Caption := 'Def';
  dbgInfo.Columns[4].Title.Alignment := taCenter;
  dbgInfo.Columns[4].Width := 50;

  dbgInfo.Columns.Add;
  dbgInfo.Columns[5].FieldName := 'AUTO_INCREMENT';
  dbgInfo.Columns[5].Title.Caption := 'AutoInc';
  dbgInfo.Columns[5].Title.Alignment := taCenter;
  dbgInfo.Columns[5].Width := 50;

  dbgInfo.Columns.Add;
  dbgInfo.Columns[6].FieldName := 'IS_NULLABLE';
  dbgInfo.Columns[6].Title.Caption := 'Null';
  dbgInfo.Columns[6].Title.Alignment := taCenter;
  dbgInfo.Columns[6].Width := 50;

  dbgInfo.Columns.Add;
  dbgInfo.Columns[7].FieldName := 'REMARKS';
  dbgInfo.Columns[7].Title.Caption := 'Remarks';
  dbgInfo.Columns[7].Title.Alignment := taCenter;
  dbgInfo.Columns[7].Width := 150;
end;

procedure TfmDBBrowser.CreateForeginKeyTable;
begin
  dbgInfo.Columns.Add;
  dbgInfo.Columns[0].FieldName := 'FK_NAME';
  dbgInfo.Columns[0].Title.Caption := 'Name';
  dbgInfo.Columns[0].Title.Alignment := taCenter;
  dbgInfo.Columns[0].Width := 150;

  dbgInfo.Columns.Add;
  dbgInfo.Columns[1].FieldName := 'COLUMN_NAME';
  dbgInfo.Columns[1].Title.Caption := 'Column';
  dbgInfo.Columns[1].Title.Alignment := taCenter;
  dbgInfo.Columns[1].Width := 150;

  dbgInfo.Columns.Add;
  dbgInfo.Columns[2].FieldName := 'KEY_SEQ';
  dbgInfo.Columns[2].Title.Caption := 'Seq';
  dbgInfo.Columns[2].Title.Alignment := taCenter;
  dbgInfo.Columns[2].Width := 30;
end;

procedure TfmDBBrowser.CreateIndexsTable;
begin
  dbgInfo.Columns.Add;
  dbgInfo.Columns[0].FieldName := 'INDEX_NAME';
  dbgInfo.Columns[0].Title.Caption := 'Name';
  dbgInfo.Columns[0].Title.Alignment := taCenter;
  dbgInfo.Columns[0].Width := 150;

  dbgInfo.Columns.Add;
  dbgInfo.Columns[1].FieldName := 'ORDINAL_POSITION';
  dbgInfo.Columns[1].Title.Caption := '#';
  dbgInfo.Columns[1].Title.Alignment := taCenter;
  dbgInfo.Columns[1].Width := 30;

  dbgInfo.Columns.Add;
  dbgInfo.Columns[2].FieldName := 'COLUMN_NAME';
  dbgInfo.Columns[2].Title.Caption := 'Column';
  dbgInfo.Columns[2].Title.Alignment := taCenter;
  dbgInfo.Columns[2].Width := 150;

  dbgInfo.Columns.Add;
  dbgInfo.Columns[3].FieldName := 'ASC_OR_DESC';
  dbgInfo.Columns[3].Title.Caption := 'Asc/Desc';
  dbgInfo.Columns[3].Title.Alignment := taCenter;
  dbgInfo.Columns[3].Width := 75;

  dbgInfo.Columns.Add;
  dbgInfo.Columns[4].FieldName := 'NON_UNIQUE';
  dbgInfo.Columns[4].Title.Caption := 'Unique';
  dbgInfo.Columns[4].Title.Alignment := taCenter;
  dbgInfo.Columns[4].Width := 50;

  dbgInfo.Columns.Add;
  dbgInfo.Columns[5].FieldName := 'FILTER_CONDITION';
  dbgInfo.Columns[5].Title.Caption := 'Filter';
  dbgInfo.Columns[5].Title.Alignment := taCenter;
  dbgInfo.Columns[5].Width := 150;
end;

procedure TfmDBBrowser.CreatePrimaryKeyTable;
begin
  dbgInfo.Columns.Add;
  dbgInfo.Columns[0].FieldName := 'PK_NAME';
  dbgInfo.Columns[0].Title.Caption := 'Name';
  dbgInfo.Columns[0].Title.Alignment := taCenter;
  dbgInfo.Columns[0].Width := 150;

  dbgInfo.Columns.Add;
  dbgInfo.Columns[1].FieldName := 'COLUMN_NAME';
  dbgInfo.Columns[1].Title.Caption := 'Column';
  dbgInfo.Columns[1].Title.Alignment := taCenter;
  dbgInfo.Columns[1].Width := 150;

  dbgInfo.Columns.Add;
  dbgInfo.Columns[2].FieldName := 'KEY_SEQ';
  dbgInfo.Columns[2].Title.Caption := 'Seq';
  dbgInfo.Columns[2].Title.Alignment := taCenter;
  dbgInfo.Columns[2].Width := 30;
end;

procedure TfmDBBrowser.CreateTMPNode(AParent: TTreeNode);
begin
  tvDBBrowser.Items.AddChildObject(AParent, 'TMP', nil);
end;

procedure TfmDBBrowser.CreateTree;
var
  OldCursor: TCursor;
begin
  tvDBBrowser.Items.BeginUpdate;
  Connection.SQLHourGlass := false;
  OldCursor := Screen.Cursor;
  Screen.Cursor := crSQLWait;
  ClearTreeNodeData;
  try
    SetCatalogs;
    SetSchemas;
  finally
    Screen.Cursor := OldCursor;
    Connection.SQLHourGlass := true;
    tvDBBrowser.Items.EndUpdate;
  end;
end;

procedure TfmDBBrowser.FormCreate(Sender: TObject);
begin
  FCatalog := '';
  FShowSelect := false;
  FStartDefCon := false;
  LoadSettings;
  if cbConnection.Items.Count > 0 then
    cbConnection.ItemIndex := 0;
  if FStartDefCon then
    DoConnect(Connection, GetConnection(cbConnection.Items, true, 0));
end;

procedure TfmDBBrowser.FormDestroy(Sender: TObject);
begin
  ClearListConnection(cbConnection.Items);
  ClearTreeNodeData;
  DoDisconnect(Connection);
end;

procedure TfmDBBrowser.FormResize(Sender: TObject);
begin
  Repaint;
  Refresh;
end;

function TfmDBBrowser.GetCountMetaData(AType: TZMetadataType; ACatalog: string;
  ASchema: string; ADBName: string; AFilter: string; AUnique: Boolean): Integer;
var
  md: TZSQLMetadata;
  AName, FieldName : String;
  ListNames : TStringList;
begin
  Result := 0;
  md := GetMetaData(AType, ACatalog, ASchema, ADBName, AFilter, AUnique);
  ListNames := TStringList.Create;
  try
    md.Open;
    if AType in [mdPrimaryKeys, mdIndexInfo] then
    begin
      md.First;
      case AType of
        mdPrimaryKeys:FieldName := 'PK_NAME';
        mdIndexInfo:FieldName := 'INDEX_NAME';
      end;
      while not md.Eof do
      begin
        AName := md.FieldByName(FieldName).AsString;
        if Trim(AName) = '' Then AName := 'NO NAME';
        if ListNames.IndexOf(AName) = -1 then ListNames.Add(AName);
        md.Next;
      end;
      Result := ListNames.Count;
    end
    else Result := md.RecordCount;
  finally
    ListNames.Free;
    md.Free
  end;
end;

function TfmDBBrowser.GetMetaData(AType: TZMetadataType; ACatalog: string;
  ASchema: string; ADBName: string; AFilter: string; AUnique: Boolean; ASort:
  string): TZSQLMetadata;
begin
  Result := TZSQLMetadata.Create(self);
  try
    Result.Connection := Connection;
    Result.MetadataType := AType;
    if ADBName <> '' then
      case AType of
        mdProcedures:
          Result.ProcedureName := ADBName;
        mdProcedureColumns:
          Result.ProcedureName := ADBName;
        mdTables:
          ;
        mdSchemas:
          ;
        mdCatalogs:
          ;
        mdTableTypes:
          ;
        mdColumns:
          Result.TableName := ADBName;
        mdColumnPrivileges:
          Result.TableName := ADBName;
        mdTablePrivileges:
          Result.TableName := ADBName;
        mdBestRowIdentifier:
          ;
        mdVersionColumns:
          ;
        mdPrimaryKeys:
          Result.TableName := ADBName;
        mdImportedKeys:
          Result.TableName := ADBName;
        mdExportedKeys:
          Result.TableName := ADBName;
        mdCrossReference:
          Result.TableName := ADBName;
        mdTypeInfo:
          ;
        mdTriggers:
          Result.TableName := ADBName;
        mdIndexInfo:
          Result.TableName := ADBName;
        mdSequences:
          Result.SequenceName := ADBName;
        mdUserDefinedTypes:
          ;
      end;
    if ASchema <> '' then
    begin
      Result.Schema := ASchema;
      Result.Catalog := '';
    end
    else
      Result.Catalog := ACatalog;
    if ASort <> '' then
      Result.SortedFields := ASort;
    if AFilter <> '' then
    begin
      Result.Filter := AFilter;
      Result.Filtered := true;
    end;
    Result.Unique := AUnique;
  except
    Result := nil;
  end;
end;

procedure TfmDBBrowser.InitTableInfo;
var
  i : integer;
begin
  MetaData.Close;
  for i:=dbgInfo.Columns.Count - 1 downto  0 do
    dbgInfo.Columns[i].Destroy;
end;

procedure TfmDBBrowser.LoadSettings;
begin
  FShowSelect := INI.ReadBool('Main', 'ShowListConnections', false);
  FStartDefCon := INI.ReadBool('Main', 'StartDefaultConnection', false);
  cbConnection.Clear;
  cbConnection.Items.AddStrings(LoadConnection);
end;

procedure TfmDBBrowser.SelItem;
begin
  if MenuItemCheck = miShown then
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

procedure TfmDBBrowser.SetMenuItemCheck(const Value: TMenuItemCheck);
begin
  if Value <> FMenuItemCheck then
  begin
    SendMessage(Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, CmdId, LPARAM(Value));
    FMenuItemCheck := Value;
  end;
end;

constructor TTreeItem.Create;
begin
  TypeItem := tiNone;
  TypeMD := mdVersionColumns;
  Catalog := '';
  Schema := '';
  DBName := '';
  Filter := '';
  Sort := '';
end;

end.



