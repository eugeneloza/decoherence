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

unit decoglobal;

{$mode objfpc}{$H+}

interface

uses
  Classes,CastleWindowTouch;

const Interface_Foler = 'interface/';

type float = single;

type TGameMode = (gmNone,gmLoadScreen,gmTravel);
  //gmCombat,gmInventory,gmLevelup,gmMenu,gmMainMenu,...
  //gm render mode??? gmInventory gmLevelUp gmLoadScreen gm3d gmMenu

var Window : TCastleWindowTouch;
    LogStream : TFileStream;
    CurrentGameMode : TGameMode = gmNone;
    LastGameMode : TGameMode = gmNone;

procedure SetGameMode(GM : TGameMode);

implementation

procedure SetGameMode(GM : TGameMode);
begin
  LastGameMode := CurrentGameMode;
  CurrentGameMode := gm;
end;


end.

