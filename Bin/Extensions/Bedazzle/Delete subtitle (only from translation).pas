// Delete subtitle (only from translation). Made by Bedazzle.

program DeleteTransSubs;

// -----------------------------------------------------------------------------

var
  n: Integer;
  s: Boolean;
  i: Integer;
  it: Integer;
  ft: Integer;

  curr: Integer;
  txt: String;

begin
  n := GetSubtitleCount;

  s := false;

  for i := 1 to GetSubtitleCount do
  begin
     if (s = false) then
     begin
        if (IsSubtitleSelected(i) = true) then
        begin
           curr := i;
           break;
        end;
     end;
  end;

  for i := curr to GetSubtitleCount-1 do
  begin
     txt := GetSubtitleTrans(i+1);
     SetSubtitleTrans(i, txt);
  end;
end.
