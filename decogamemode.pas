{Copyright (C) 2012-2016 Yevhen Loza

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <http://www.gnu.org/licenses/>.}

{---------------------------------------------------------------------------}

{ Hanldes changes of game mode and corresponding interface changes }
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

