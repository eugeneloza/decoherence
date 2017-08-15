{Copyright (C) 2012-2017 Yevhen Loza

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
unit DecoGameMode;

{$INCLUDE compilerconfig.inc}

interface

type TGameMode = (gmNone,
  { displays load screen }
  gmLoadScreen,
  gmCharacterGeneration,
  { any travel mode displaying 3D World }
  gmTravel,
  { any battle mode displaying 3D World }
  gmBattle);

var CurrentGameMode: TGameMode = gmNone;
    LastGameMode: TGameMode = gmNone;

    PlayerInBattle: boolean = false;

{ Correctly sets the current game mode and initializes all the corresponding
  routines }
procedure SetGameMode(GM: TGameMode);
{ Does this game mode require a 3D world render }
function is3DGameMode: boolean;
function GameModeNeedsClearingScreen: boolean;

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses DecoGui, DecoAbstractWorld, DecoAbstractWorld3d,
  DecoGlobal;

procedure SetGameMode(GM: TGameMode);
begin
  if GM = LastGameMode then Exit;

  {release interface elements used by previous game mode}

  {$hint make through switching of game interfaces}

  case LastGameMode of
    {only gmLoadScreen and gmCharacterGeneration use wind so we can release it (true) after it was used}
    gmLoadScreen: GUI.FreeLoadScreen(gm<>gmCharacterGeneration);
    gmCharacterGeneration: GUI.FreeLoadScreen(gm<>gmLoadScreen);
  end;
  {no need to make Load screens because they are made automatically on demand}
  //if GM=gmLoadScreen then GUI.MakeLoadScreen;

  {build new interface elements for next game mode}

  case GM of
    {gmLoadScreen}//built automatically when game mode is enabled (?)
    gmTravel: GUI.MakeCharacterGenerationInterface;
  end;

  LastGameMode := CurrentGameMode;
  CurrentGameMode := GM;

  { set 3D world rendering }
  if (CurrentWorld<>nil) and (CurrentWorld is DAbstractWorld3d) then
    (CurrentWorld as DAbstractWorld3d).ToggleSceneManager(is3DGameMode);
{  if Window.SceneManager <> nil then
    if is3DGameMode then Window.SceneManager.exists := true
                    else Window.SceneManager.exists := false;}

end;

{------------------------------------------------------------}

function is3DGameMode: boolean;
begin
  if (CurrentGameMode = gmTravel) or (CurrentGameMode = gmBattle) then
    Result := true
  else
    Result := false;
end;

{------------------------------------------------------------}

function GameModeNeedsClearingScreen: boolean;
begin
  {$HINT something more efficient, or world-attached might be here}
  if (Window.SceneManager = nil) or (Window.SceneManager.Exists = false) then
    Result := true
  else
    Result := false;
end;


end.

