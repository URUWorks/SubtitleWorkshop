// Insert copyright. Made by Bedazzle.

program Copyright;

// -----------------------------------------------------------------------------

var
  n: Integer;
  final: Integer;
  txt: String;

begin
  if IsOriginalLoaded = False then EnableWorkArea;

  n := GetSubtitleCount;
  final := GetSubtitleFinalTime(n-1);

  txt := 'Traducido con Subtitle Workshop, ' + FormatDateTime('dd.mm.yyyy', now);

  InsertSubtitle(n, final+5000, final+15000, txt, txt);
  GoToLineNumber(n);
end.
