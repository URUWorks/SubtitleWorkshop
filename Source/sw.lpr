program sw;

{$mode objfpc}{$H+}
{$IFDEF UNIX}{$DEFINE UseCThreads}{$ENDIF}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, virtualtreeview_package, UMain, utypes, ucommon,
  UUndo, UErrors, pascalscript, ufindandreplace, uspellcheck,
  uabout, uwelcome, UTexts, UTimings, UInfoAndErrors, UVideo,
  UInfoAndErrorsTypes, UAudioExtraction, UStylesAndActors, UGlossary, UTM, 
UProject
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Subtitle Workshop';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

