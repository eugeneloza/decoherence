unit decogamemode;

{$mode objfpc}{$H+}
{$INCLUDE compilerconfig.inc}

interface

type TGameMode = (gmNone,gmLoadScreen,gmCharacterGeneration,gmTravel);

var CurrentGameMode : TGameMode = gmNone;
    LastGameMode : TGameMode = gmNone;

procedure SetGameMode(GM : TGameMode);

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses decogui;

procedure SetGameMode(GM : TGameMode);
begin
  if GM=LastGameMode then exit;

  {only gmLoadScreen and gmCharacterGeneration use wind so we can release it (true)
  after it was used}
  if LastGameMode=gmLoadScreen then GUI.FreeLoadScreen(gm<>gmCharacterGeneration);
  if LastGameMode=gmCharacterGeneration then GUI.FreeLoadScreen(gm<>gmLoadScreen);
  {no need to make Load screens because they are made automatically on demand}
  //if GM=gmLoadScreen then GUI.MakeLoadScreen;

  LastGameMode := CurrentGameMode;
  CurrentGameMode := GM;
end;


end.

