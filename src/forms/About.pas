unit About;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.Variants,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Grids,
  common,
  NppForms;

type
  TfmAbout = class(TNppForm)
    btOk: TButton;
    sgInfo: TStringGrid;
    procedure FormCreate(Sender: TObject);
  end;

implementation

{$R *.dfm}

Const
  NoSelection : TGridRect = (Left:-1; Top:-1; Right:-1; Bottom:-1 );

procedure TfmAbout.FormCreate(Sender: TObject);
begin
  inherited;
  Caption := Plugin_Name + ' Plugin';
  sgInfo.Cells[0,0] := 'Version';
  sgInfo.Cells[1,0] := '1.1';
  sgInfo.Cells[0,1] := 'Author';
  sgInfo.Cells[1,1] := 'Nikolay Voronov';
  sgInfo.Cells[0,2] := 'Mail';
  sgInfo.Cells[1,2] := 'nkvoronov@gmail.com';
  sgInfo.Selection  := NoSelection;
end;

end.
