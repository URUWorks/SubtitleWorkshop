// Insert subtitle (only in translation). Made by Bedazzle.

program InsertTransSubs;

// -----------------------------------------------------------------------------

var
  n: Integer;
  i: Integer;
  j: Integer;

  txt: String;

begin
  n := GetSubtitleCount-1;

  for i := 0 to n do
  begin
     if (IsSubtitleSelected(i) = true) then
     begin
        for j := n downto i+1 do
        begin
           txt := GetSubtitleTrans(j-1);
           SetSubtitleTrans(j, txt);
        end;

        SetSubtitleTrans(i+1, '');

        break;
     end;
  end;
end.
