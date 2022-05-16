library NppDBCon;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

{$R 'NppDBCon.res' 'NppDBCon.rc'}

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.Types,
  NppDockingForms in 'npp\NppDockingForms.pas' {NppDockingForm},
  NppForms in 'npp\NppForms.pas' {NppForm},
  SciSupport in 'npp\SciSupport.pas',
  NppPlugin in 'npp\NppPlugin.pas',
  plugin in 'src\plugin.pas',
  About in 'src\forms\About.pas' {fmAbout},
  Settings in 'src\forms\Settings.pas' {fmSettings},
  SqlResult in 'src\dockingforms\SqlResult.pas' {fmSQLResult},
  DBBrowser in 'src\dockingforms\DBBrowser.pas' {fmDBBrowser},
  common in 'src\common.pas',
  View in 'src\forms\View.pas' {fmView},
  Select in 'src\forms\Select.pas' {fmSelect},
  EditConnect in 'src\forms\EditConnect.pas' {fmEditConnect};

{$R *.res}

{$Include 'npp\NppPluginInclude.pas'}

begin
  { First, assign the procedure to the DLLProc variable }
  DllProc := @DLLEntryPoint;
  { Now invoke the procedure to reflect that the DLL is attaching to the process }
  DLLEntryPoint(DLL_PROCESS_ATTACH);
end.
