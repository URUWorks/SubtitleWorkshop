{*
 *  URUWorks Subtitle Workshop
 *
 *  Author  : URUWorks
 *  Website : uruworks.net
 *
 *  The contents of this file are used with permission, subject to
 *  the Mozilla Public License Version 1.1 (the "License"); you may
 *  not use this file except in compliance with the License. You may
 *  obtain a copy of the License at
 *  http://www.mozilla.org/MPL/MPL-1.1.html
 *
 *  Software distributed under the License is distributed on an
 *  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing
 *  rights and limitations under the License.
 *
 *  Copyright (C) 2001-2016 Aldo Lacavalla, URUWorks.
 *}

unit UErrors;

// -----------------------------------------------------------------------------

{$mode objfpc}{$H+}

interface

uses
  UWSubtitleAPI, UWSubtitles.Utils, UWSubtitles.OCR;

type

  TErrCfg = packed record
    RepeatableChars   : String;   // -¡!¿?";\/_[]=
    ProhibitedChars   : String;   // @,http,www,#,*
    MaxLineLength     : Cardinal; // 45
    MaxDuration       : Cardinal; // 7000
    MinDuration       : Cardinal; // 1000
    MinPause          : Cardinal; // 200
    MaxCPS            : Byte;     // 20
    RepeatedTolerance : Cardinal; // 100
  end;

  function CheckErrors(const Subtitles: TUWSubtitles; const Index: Integer; const ErrorsToCheck: TSubtitleErrorTypeSet; const Options: TErrCfg; const OCR: TUWOCRScript = NIL): TSubtitleErrorTypeSet;
  function FixErrors(const Subtitles: TUWSubtitles; const Index: Integer; const ErrorsToFix: TSubtitleErrorTypeSet; const Options: TErrCfg; const OCR: TUWOCRScript = NIL): TUWSubtitleItem;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

function CompareSubtitle(const Item1, Item2: TUWSubtitleItem): Boolean;
begin
  if (Item1.InitialTime = Item2.InitialTime) and
     (Item1.FinalTime = Item2.FinalTime) and
     (Item1.Text = Item2.Text) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function CheckErrors(const Subtitles: TUWSubtitles; const Index: Integer; const ErrorsToCheck: TSubtitleErrorTypeSet; const Options: TErrCfg; const OCR: TUWOCRScript = NIL): TSubtitleErrorTypeSet;
var
  s: String;
begin
  Result := [];
  if not Assigned(Subtitles) or (Subtitles.Count < 1) or (Index > Subtitles.Count) then Exit;

  // Repeated subtitle
  if etRepeatedSubtitle in ErrorsToCheck then
  begin
    if (Index > 0) and CompareSubtitle(Subtitles[Index], Subtitles[Index-1]) and (Subtitles[Index].Text <> '') and (Subtitles.Pause[Index] < Options.RepeatedTolerance) then
    begin
      Result := Result + [etRepeatedSubtitle];
    end;
  end;

  // Unnecessary Spaces
  if etUnnecessarySpaces in ErrorsToCheck then
  begin
    s := RemoveUnnecessarySpaces(Subtitles[Index].Text);
    if Subtitles[Index].Text <> s then
    begin
      Result := Result + [etUnnecessarySpaces];
    end;
  end;

  // Unnecessary Dots
  if etUnnecessaryDots in ErrorsToCheck then
  begin
    s := RemoveUnnecessaryDots(Subtitles[Index].Text);
    if Subtitles[Index].Text <> s then
    begin
      Result := Result + [etUnnecessaryDots];
    end;
  end;

  // Empty subtitle
  if etEmpty in ErrorsToCheck then
  begin
    if Subtitles[Index].Text = '' then
    begin
      Result := Result + [etEmpty];
    end;
  end;

  // Fix Tags
  if etFixTags in ErrorsToCheck then
  begin
    s := FixTags(Subtitles[Index].Text, '<', '>');
    if Subtitles[Index].Text <> s then
    begin
      Result := Result + [etFixTags];
    end;
  end;

  // Prohibited chars
  if etProhibitedChars in ErrorsToCheck then
  begin
    if HasProhibitedChars(Subtitles[Index].Text, Options.ProhibitedChars) then
    begin
      Result := Result + [etProhibitedChars];
    end;
  end;

  // Break long lines
  if etBreakLongLines in ErrorsToCheck then
  begin
    if HasTooLongLine(Subtitles[Index].Text, Options.MaxLineLength) then
    begin
      Result := Result + [etBreakLongLines];
    end;
  end;

  // Hearing impaired
  if etHearingImpaired in ErrorsToCheck then
  begin
    if IsHearingImpaired(Subtitles[Index].Text) then
    begin
      s := FixHearingImpaired(Subtitles[Index].Text, sLineBreak);
      if Subtitles[Index].Text <> s then
      begin
        Result := Result + [etHearingImpaired];
      end;
    end;
  end;

  // Repeated chars
  if etRepeatedChars in ErrorsToCheck then
  begin
    if HasRepeatedChar(Subtitles[Index].Text, Options.RepeatableChars) then
    begin
      s := FixRepeatedChar(Subtitles[Index].Text, Options.RepeatableChars);
      if Subtitles[Index].Text <> s then
      begin
        Result := Result + [etRepeatedChars];
      end;
    end;
  end;

  // OCR
  if (etOCR in ErrorsToCheck) and (OCR <> NIL) then
  begin
    if OCR.HasErrors(Subtitles[Index].Text) then
    begin
      s := OCR.Fix(Subtitles[Index].Text);
      if Subtitles[Index].Text <> s then
      begin
        Result := Result + [etOCR];
      end;
    end;
  end;

  // Time too short
  if etTimeTooShort in ErrorsToCheck then
  begin
    if Subtitles.Duration[Index] < Options.MinDuration then
    begin
      Result := Result + [etTimeTooShort];
    end;
  end;

  // Time too long
  if etTimeTooLong in ErrorsToCheck then
  begin
    if Subtitles.Duration[Index] > Options.MaxDuration then
    begin
      Result := Result + [etTimeTooLong];
    end;
  end;

  // Bad Values
  if etBadValues in ErrorsToCheck then
  begin
    if Subtitles.InitialTime[Index] > Subtitles.FinalTime[Index] then
    begin
      Result := Result + [etBadValues];
    end;
  end;

  // Pause too short
  if etPauseTooShort in ErrorsToCheck then
  begin
    if (Index > 0) and (Subtitles.Pause[Index] < Options.MinPause) then
    begin
      Result := Result + [etPauseTooShort];
    end;
  end;

  // too much CPS
  if etMaxCPS in ErrorsToCheck then
  begin
    if (Index > 0) and (Subtitles.TextCPS[Index] > Options.MaxCPS) then
    begin
      Result := Result + [etMaxCPS];
    end;
  end;

  // Overlapping
  if etOverlapping in ErrorsToCheck then
  begin
    if (Index > 0) and (Subtitles.InitialTime[Index] <= Subtitles.FinalTime[Index-1]) then
    begin
      Result := Result + [etOverlapping];
    end;
  end;
end;

// -----------------------------------------------------------------------------

function FixErrors(const Subtitles: TUWSubtitles; const Index: Integer; const ErrorsToFix: TSubtitleErrorTypeSet; const Options: TErrCfg; const OCR: TUWOCRScript = NIL): TUWSubtitleItem;
var
  s: String;
  x: Integer;
begin
  ClearSubtitleItem(Result);
  if not Assigned(Subtitles) or (Subtitles.Count < 1) or (Index > Subtitles.Count) then Exit;

  Result := Subtitles[Index];
  Result.ErrorType := [];
  s := Result.Text;

  // Repeated subtitle
  if etRepeatedSubtitle in ErrorsToFix then
  begin
    if (Index > 0) and CompareSubtitle(Subtitles[Index], Subtitles[Index-1]) and (Subtitles[Index].Text <> '') and (Subtitles.Pause[Index] < Options.RepeatedTolerance) then
    begin
      Result.ErrorType := Result.ErrorType + [etRepeatedSubtitle];
    end;
  end;

  // Unnecessary Spaces
  if etUnnecessarySpaces in ErrorsToFix then
  begin
    s := RemoveUnnecessarySpaces(Result.Text);
    if Result.Text <> s then
    begin
      Result.Text := s;
      Result.ErrorType := Result.ErrorType + [etUnnecessarySpaces];
    end;
  end;

  // Unnecessary Dots
  if etUnnecessaryDots in ErrorsToFix then
  begin
    s := RemoveUnnecessaryDots(Result.Text);
    if Result.Text <> s then
    begin
      Result.Text := s;
      Result.ErrorType := Result.ErrorType + [etUnnecessaryDots];
    end;
  end;

  // Empty subtitle
  if etEmpty in ErrorsToFix then
  begin
    if s = '' then
    begin
      Result.ErrorType := Result.ErrorType + [etEmpty];
    end;
  end;

  // Fix Tags
  if etFixTags in ErrorsToFix then
  begin
    s := FixTags(Subtitles[Index].Text, '<', '>');
    if Subtitles[Index].Text <> s then
    begin
      Result.Text := s;
      Result.ErrorType := Result.ErrorType + [etFixTags];
    end;
  end;

  // Prohibited chars
  if etProhibitedChars in ErrorsToFix then
  begin
    if HasProhibitedChars(s, Options.ProhibitedChars) then
    begin
      Result.ErrorType := Result.ErrorType + [etProhibitedChars];
    end;
  end;

  // Break long lines
  if etBreakLongLines in ErrorsToFix then
  begin
    if HasTooLongLine(s, Options.MaxLineLength) then
    begin
      s := AutoBreakSubtitle(s, Options.MaxLineLength, sLineBreak, False);
      Result.Text := s;
      Result.ErrorType := Result.ErrorType + [etBreakLongLines];
    end;
  end;

  // Hearing impaired
  if etHearingImpaired in ErrorsToFix then
  begin
    if IsHearingImpaired(Subtitles[Index].Text) then
    begin
      s := FixHearingImpaired(Subtitles[Index].Text, sLineBreak);
      if Result.Text <> s then
      begin
        Result.Text := s;
        Result.ErrorType := Result.ErrorType + [etHearingImpaired];
      end;
    end;
  end;

  // Repeated chars
  if etRepeatedChars in ErrorsToFix then
  begin
    if HasRepeatedChar(s, Options.RepeatableChars) then
    begin
      s := FixRepeatedChar(s, Options.RepeatableChars);
      if Result.Text <> s then
      begin
        Result.Text := s;
        Result.ErrorType := Result.ErrorType + [etRepeatedChars];
      end;
    end;
  end;

  // OCR
  if (etOCR in ErrorsToFix) and (OCR <> NIL) then
  begin
    if OCR.HasErrors(s) then
    begin
      s := OCR.Fix(s);
      if Result.Text <> s then
      begin
        Result.Text := s;
        Result.ErrorType := Result.ErrorType + [etOCR];
      end;
    end;
  end;

  // Time too short
  if etTimeTooShort in ErrorsToFix then
  begin
    if Subtitles.Duration[Index] < Options.MinDuration then
    begin
      Result.FinalTime := Result.InitialTime + Options.MinDuration;
      Result.ErrorType := Result.ErrorType + [etTimeTooShort];
    end;
  end;

  // Time too long
  if etTimeTooLong in ErrorsToFix then
  begin
    if Subtitles.Duration[Index] > Options.MaxDuration then
    begin
      Result.FinalTime := Result.InitialTime + Options.MaxDuration;
      Result.ErrorType := Result.ErrorType + [etTimeTooLong];
    end;
  end;

  // Bad Values
  if etBadValues in ErrorsToFix then
  begin
    if Result.InitialTime > Result.FinalTime then
    begin
      x := Result.InitialTime;
      Result.InitialTime := Result.FinalTime;
      Result.FinalTime   := x;
      Result.ErrorType := Result.ErrorType + [etBadValues];
    end;
  end;

  // Pause too short
  if etPauseTooShort in ErrorsToFix then
  begin
    if (Index > 0) and (Subtitles.Pause[Index] < Options.MinPause) then
    begin
      Result.FinalTime := Result.InitialTime + Options.MinDuration;
      Result.ErrorType := Result.ErrorType + [etPauseTooShort];
    end;
  end;

  // too much CPS
  if etMaxCPS in ErrorsToFix then
  begin
    if (Index > 0) and (Subtitles.TextCPS[Index] > Options.MaxCPS) then
    begin
      Result.ErrorType := Result.ErrorType + [etMaxCPS];
    end;
  end;

  // Overlapping
  if etOverlapping in ErrorsToFix then
  begin
    if (Index > 0) and (Subtitles.InitialTime[Index] <= Subtitles.FinalTime[Index-1]) then
    begin
      Result.ErrorType := Result.ErrorType + [etOverlapping];
    end;
  end;
end;

// -----------------------------------------------------------------------------

end.

