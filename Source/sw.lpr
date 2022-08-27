program sw;

{$mode objfpc}{$H+}

{$IFOPT D+}
  {$IFDEF WINDOWS}
    {$APPTYPE CONSOLE}
  {$ENDIF}
  {$DEFINE DEBUG}
{$ENDIF}

{$IFDEF UNIX}
  {$DEFINE UseCThreads}
{$ENDIF}
uses
  {$IFDEF UNIX}
    {$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}
    {$IF DEFINED(LCLGTK2) or DEFINED(LCLGTK3)}
    unix_xlib,
    {$ENDIF}
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, virtualtreeview_package, UMain, UTypes, UCommon,
  UUndo, UErrors, pascalscript, UFindAndReplace, USpellCheck,
  UAbout, UWelcome, UTexts, UTimings, UInfoAndErrors, UVideo,
  UInfoAndErrorsTypes, UAudioExtraction, UStylesAndActors, UGlossary, UTM, 
  UProject, USettings
  {$IFDEF DEBUG}, UWDebug{$ENDIF};

{$R *.res}

begin
  Application.Title:='Subtitle Workshop';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

