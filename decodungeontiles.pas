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

{ Contains definition for a dungeon tile and loading/parsing procedures }
unit decodungeontiles;

{$INCLUDE compilerconfig.inc}
interface

uses classes,
  decoglobal;

var MaxTileTypes: integer;
    TilesList: TStringList;

procedure LoadTiles;
procedure destroyTiles;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses sysUtils,
  CastleLog;

{ Reads tile list for the current map.
  At this moment it scans the predefined directory in desktop-way, so it's not
  portable to Android.
  TODO: need to create a special XML file with list of tiles in Map Manager
  It'll also provide for different tiles reusing in different maps, e.g.
  can use cave tiles in both normal caves, and caves section of other maps }
{ TODO: All textures DDS }
procedure MakeTileList; deprecated;   //TODO: depending on current map parameters
var Rec: TSearchRec;
begin
  DestroyTiles;
  TilesList := TStringList.Create;
  MaxTileTypes := 0;
  //TODO: Read XML list for the map!
  //TODO: Android incompatible!!!
  if FindFirst (Tiles_folder + '*.x3d', faAnyFile - faDirectory, Rec) = 0 then
   try
     repeat
       inc(MaxTileTypes);
       TilesList.Add(Rec.Name);
     until FindNext(Rec) <> 0;
   finally
     FindClose(Rec);
   end;
 if MaxTileTypes = 0 then begin
   writeLnLog('decodungeontiles>MakeTileList','FATAL: No tiles to load: '+ Tiles_folder);
   halt;
 end;
 WriteLnLog('decodungeontiles>MakeTileList','Max Tile Types = '+inttostr(MaxTileTypes)+' success');
end;


{----------------------------------------------------------------------------}

procedure LoadTiles;
begin
  WriteLnLog('decodungeontiles','Load tiles started...');
  MakeTileList;

end;

{----------------------------------------------------------------------------}

procedure DestroyTiles;
begin
  FreeAndNil(TilesList);
end;

end.

