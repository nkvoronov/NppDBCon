unit NppForms;

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
  System.UITypes,
  NppPlugin;

type
  TNppForm = class(TForm)
  protected
    procedure DoClose(var Action: TCloseAction); override;
  public
    Npp: TNppPlugin;
    DefaultCloseAction: TCloseAction;
    constructor Create(NppParent: TNppPlugin); overload;
    constructor Create(AOwner: TNppForm); overload;

    function Confirmed(const ACaption,ATitle: nppString): boolean;
    procedure MessageWarning(const ATitle, ACaption: nppString);
    procedure MessageError(const ATitle,ACaption: nppString);
    procedure MessageSimple(const ATitle,ACaption: nppString);
    destructor Destroy; override;
  end;

var
  NppForm: TNppForm;

implementation

{$R *.dfm}

{ TNppForm }

constructor TNppForm.Create(NppParent: TNppPlugin);
begin
  Npp := NppParent;
  DefaultCloseAction := caNone;
  inherited Create(nil);
  // We figure right now this does more damage than good.
  // So let the main transalte and dispatch do it's thing instead of isdialogmessage
  Npp.RegisterAsDialog(Handle);
end;

function TNppForm.Confirmed(const ACaption, ATitle: nppString): boolean;
begin
  Result := False;
  if Assigned(Application) then
    Result := (Application.MessageBox(nppPChar(ACaption),nppPChar(ATitle),
        MB_ICONQUESTION + MB_YESNO) = IDYES);
end;

constructor TNppForm.Create(AOwner: TNppForm);
begin
  Npp := AOwner.Npp;
  DefaultCloseAction := caNone;
  inherited Create(AOwner);
  Npp.RegisterAsDialog(Handle);
end;

destructor TNppForm.Destroy;
begin
  if (self.HandleAllocated) then
    Npp.UnRegisterAsDialog(Handle);
  inherited;
end;

procedure TNppForm.DoClose(var Action: TCloseAction);
begin
  if (self.DefaultCloseAction <> caNone) then Action := self.DefaultCloseAction;
  inherited;
end;

procedure TNppForm.MessageError(const ATitle, ACaption: nppString);
begin
  if Assigned(Application) then
    Application.MessageBox(nppPChar(ATitle),nppPChar(ACaption),MB_ICONSTOP + MB_OK);
end;

procedure TNppForm.MessageSimple(const ATitle, ACaption: nppString);
begin
  if Assigned(Application) then
    Application.MessageBox(nppPChar(ATitle),nppPChar(ACaption),MB_OK);
end;

procedure TNppForm.MessageWarning(const ATitle, ACaption: nppString);
begin
  if Assigned(Application) then
    Application.MessageBox(nppPChar(ATitle),nppPChar(ACaption),MB_ICONWARNING + MB_OK);
end;

end.
