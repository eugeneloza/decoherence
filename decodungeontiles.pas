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

type
  { defines "what face is this"
   tfNone is compatible to any tile
   tfWall is compatible to any wall
   >=tfFree is compatible only to exactly the same face id >=tfFree }
  TTileFace = 0..5;//byte;//(tfNone, tfWall, tfFree);
const tfNone = 0;
      tfWall = 1;
      tfFree = 2; //(used as >=)

type
  { This is a rectagonal grid with 4 angles
   try to merge it with floor}
  TAngle = (aTop,aRight,aBottom,aLeft,aUp,aDown);
{  { Each tile has top and bottom floor }
  TFloorType = (ftUp,ftDown);}

type
  {Kind of the tile base,
   tkNone is not assigned tile (can be assigned by generator),
   tkFree is a passable tile,
   tkWall is unused yet,
   tkUp/tkDown are stairs up-down tiles,
   tkInacceptible is an internal type (acceptible error returned by some routines)}
  TTileKind = (tkNone, tkFree, tkWall, tkUp, tkDown, tkInacceptible);

  {well... 2-abundance is obsolete, really.
   I was making it in order to protect the code from errors
   when writing a lot of code without being able to visually test
   if it produces a correct (intended) result, so I had to introduce
   a lot of internal checks including 2-abundance...
   Maybe, some day this should be simplified and rewritten to remove abundance.
   But for now it works and that is all I want :)
   WARNING, if someone who is not "me" will try to rewrite the 2-abundance,
   be careful, because it is used not only to validate the tiles docking,
   but for a lot of other purposes in the generation algorithm
   (maybe in pathfinding and tile management too).
   It can't be done as easy as I'd wanted :) Otherwise, I'd have done it long ago...
   So change it only in case you're absolutely sure what you are doing. }
type
  { 1x1x1 tile with some base and free/blocked faces }
  Basic_Tile_Type = {packed} record
    base: TTileKind;
    faces: array [TAngle] of TTileFace; //this is 2x redundant!
  end;


type
  {A large tile used in dungeon generation. With a set of Basic_Tile_type
   and additional parameters and procedures
   todo: split DTile and DGenerationTile}
  DTile = class
    {size of this tile}
    tilesizex,tilesizey,tilesizez: byte;
    {internal map of this tile}
    TileMap: array of array of array of Basic_Tile_Type;

    //Tile3d: TX3DRootNode;

    {if this tile is a blocker?}
    blocker: boolean;
    {how many free faces are there}
    FreeFaces: integer;
    {does this tile has stairs down?}
    Has_stairs_down: boolean;
    {create the tile with given parameters}
    constructor create(tx: integer = 1; ty: integer = 1; tz: integer = 1);
    procedure setsize(tx: integer = 1; ty: integer = 1; tz: integer = 1);
    procedure CalculateFaces;
end;

{type Generator_tile = class(DTile)
end;}

var MaxTileTypes: integer;
    TilesList: TStringList;

{ convert TTileKind to a string for saving }
function TileKindToStr(value: TTileKind): string;
{ convert a string to TTileKind for loading }
function StrToTileKind(value: string): TTileKind;

function isPassable(value: TTileFace): boolean; inline;
function isPassable(value: TTileKind): boolean; inline;

procedure LoadTiles;
procedure destroyTiles;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses sysUtils,
  CastleLog;

function TileKindToStr(value: TTileKind): string;
begin
  case Value of
    tkNone: result := 'NA';
    tkFree: result := 'FREE';
    tkUP: result := 'UP';
    tkDown: result := 'DOWN';
    tkWall: result := 'WALL';
    tkInacceptible: raise exception.Create('tkInacceptible in TileKindToStr');  //this one shouldn't appear in string
    else raise exception.Create('Unknown TileKind in TileKindToStr');
  end;
end;

{-------------------------------------------------------------------------}

function StrToTileKind(value: string): TTileKind;
begin
  case value of
    'NA': result := tkNone;
    'FREE': result := tkFree;
    'UP': result := tkUp;
    'DOWN': result := tkDown;
    'WALL': result := tkWall;
    'ERROR':  raise exception.Create('tkInacceptible in StrToTileKind');  //this one shouldn't appear in string
    else raise exception.Create('Unknown TileKind in StrToTileKind');
  end;
end;

function isPassable(value: TTileFace): boolean; inline;
begin
  if value >= tfFree then result := true else result := false;
end;
function isPassable(value: TTileKind): boolean; inline;
begin
  if (value = tkFree) or (value=tkUp) or (value=tkDown) then
    result := true
  else
    result := false;
end;


{======================== DTILE TYPE ==========================================}

constructor DTile.create(tx: integer = 1; ty: integer = 1; tz: integer = 1);
begin
  //inherited;
  setsize(tx,ty,tz);
end;

{----------------------------------------------------------------------------}

procedure DTile.setsize(tx: integer = 1; ty: integer = 1; tz: integer = 1);
var ix,iy: integer;
begin
  tilesizex := tx;
  tilesizey := ty;
  tilesizez := tz;
  //initialize the dynamic arrays
  if (tilesizex<>0) and (tilesizey<>0) and (tilesizez<>0) then begin
  //in case this is a normal tile...
  setlength(TileMap,tilesizex);
  for ix := 0 to tilesizex do begin
    setlength(TileMap[ix],tilesizey);
    for iy := 0 to tilesizey do
      setlength(TileMap[ix,iy],tilesizez);
  end;
  end
  else begin
  //in case this is a "blocker" tile we still need a complete 1x1x1 base
  raise exception.Create('Tilesize is zero!');
  {  blocker := true;         { TODO :  }
  setlength(tilemap,1);
  setlength(tilemap[0],1);
  setlength(tliemap[0,0],1);  }
  end;
end;

{----------------------------------------------------------------------------}

procedure DTile.CalculateFaces;
var ix,iy,iz: integer;
begin
  FreeFaces:=0;
  for iz:=0 to tilesizez-1 do begin
    {$Warning A critical issue must be fixed in DTile.CalculateFaces}
    {todo: this is not correct! Exits may be not only on border tiles
     correct way is to search for borders and face_na
     however, the issue is not as critical, just it'll make the
     algorithm work in an incorrect way and might even lead to tile
     not being used}
    ix:=0;
    for iy:=0 to tilesizey-1 do if isPassable(TileMap[ix,iy,iz].faces[aLeft]) then inc(FreeFaces);
    ix:=tilesizex-1;
    for iy:=0 to tilesizey-1 do if isPassable(TileMap[ix,iy,iz].faces[aRight]) then inc(FreeFaces);
    iy:=0;
    for ix:=0 to tilesizex-1 do if isPassable(TileMap[ix,iy,iz].faces[aTop]) then inc(FreeFaces);
    iy:=tilesizey;
    for ix:=0 to tilesizex-1 do if isPassable(TileMap[ix,iy,iz].faces[aBottom]) then inc(FreeFaces);
  end;
  //check if it's a blocker tile
  if (FreeFaces = 1) and (tilesizex+tilesizey+tilesizez = 3) and (TileMap[1,1,1].base = tkNone) then
    blocker:=true
  else
    blocker:=false;
  //check if it has stairs down for later generation
  has_stairs_down := false;
  for ix:=0 to tilesizex-1 do
   for iy:=0 to tilesizey-1 do
    for iz:=0 to tilesizez-1 do if TileMap[ix,iy,iz].base=tkDown then has_stairs_down := true;
end;

{=============================================================================}

{ Reads tile list for the current map.
  At this moment it scans the predefined directory in desktop-way, so it's not
  portable to Android.
  TODO: need to create a special XML file with list of tiles in Map Manager
  It'll also provide for different tiles reusing in different maps, e.g.
  can use cave tiles in both normal caves, and caves section of other maps }
{ TODO: All textures DDS }
procedure MakeTileList; deprecated;
var Rec: TSearchRec;
begin
  DestroyTiles;
  TilesList := TStringList.Create;
  MaxTileTypes := 0;
  //TODO: Read XML list for the map!
  //TODO: Android incompatible!!!
  {$Warning Fix folders!}
  if FindFirst ('data'+pathdelim+'models'+pathdelim+ 'tiles'+pathdelim + '*.x3d', faAnyFile - faDirectory, Rec) = 0 then
   try
     repeat
       inc(MaxTileTypes);
       TilesList.Add(Rec.Name);
     until FindNext(Rec) <> 0;
   finally
     FindClose(Rec);
   end;
 if MaxTileTypes = 0 then begin
   writeLnLog('decodungeontiles>MakeTileList','FATAL: No tiles to load ');
   halt;
 end;
 WriteLnLog('decodungeontiles>MakeTileList','Max Tile Types = '+inttostr(MaxTileTypes)+' success');
end;


{----------------------------------------------------------------------------}

procedure LoadTiles; deprecated;
begin
  WriteLnLog('decodungeontiles','Load tiles started...');
  MakeTileList;

end;

{----------------------------------------------------------------------------}

procedure DestroyTiles; deprecated;
begin
  FreeAndNil(TilesList);
end;

end.

