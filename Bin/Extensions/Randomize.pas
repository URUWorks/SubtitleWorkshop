// -------------------------------------------------------------------------- //
//          Subtitle Workshop - Randomize subtitles extension script          //
//                       Copyright © 2001-2022 URUWorks                       //
//                            https://uruworks.net                            //
//                                                                            //
// This script will randomize the positions of all the subtitles in a file.   //
// The purpose of it is to be able to go through a subtitle file, correcting  //
// grammar/spelling mistakes without knowing really what is happening in the  //
// movie.                                                                     //
//                                                                            //
// -------------------------------------------------------------------------- //

program RandomizeSubs;

// -----------------------------------------------------------------------------

var
  i              : Integer;
  r              : Integer;
  Count          : Integer;
  tmpInitialTime : Integer;
  tmpFinalTime   : Integer;
  tmpText        : String;
  tmpTrans       : String;
begin
  Count := GetSubtitleCount;

  for i := 0 to Count-1 do
  begin
    Randomize;
    r := Random(Count-1);

    // Exchange subtitles
    tmpInitialTime := GetSubtitleInitialTime(i);
    tmpFinalTime   := GetSubtitleFinalTime(i);
    tmpText        := GetSubtitleText(i);
    tmpTrans       := GetSubtitleTrans(i);

    SetSubtitleInitialTime(i, GetSubtitleInitialTime(r));
    SetSubtitleFinalTime(i, GetSubtitleFinalTime(r));
    SetSubtitleText(i, GetSubtitleText(r));
    SetSubtitleTrans(i, GetSubtitleTrans(r));

    SetSubtitleInitialTime(r, tmpInitialTime);
    SetSubtitleFinalTime(r, tmpFinalTime);
    SetSubtitleText(r, tmpText);
    SetSubtitleTrans(r, tmpTrans);
  end;
end.
