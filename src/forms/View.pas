unit View;

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
  NppForms;

type
  TfmView = class(TNppForm)
    pnBottom: TPanel;
    mmView: TMemo;
    btOk: TButton;
    btCancel: TButton;
  end;

implementation

{$R *.dfm}

end.
