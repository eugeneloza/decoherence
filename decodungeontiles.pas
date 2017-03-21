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

uses classes, fgl,
  decoglobal;

const TileScale = 2;

type
  { defines "what face is this"
   tfNone is compatible to any tile
   tfWall is compatible to any wall
   >=tfFree is compatible only to exactly the same face id >=tfFree
   At the moment, floors follow the same logic, but they are used only
   for internal purposes (there is no actual floor docking).
   Theoretically there can be tiles with open/compatible ceiling and/or floor elements
   and it should work as expected without any additional modifications}
  TTileFace = -1..4;//byte;//(tfNone, tfWall, tfFree1, tfFree2...);
const tfNone = 0;
      tfWall = 1;
      tfFree = 2;//(use isPassable to detect tfFree, howerer all tfFree are "tfFree + 0..n")
      tfInacceptible = -1;
{should be only 3/4 varians + kind (TFGPlist)}


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

type
  { This is a rectagonal grid with 4 angles + floor + ceiling
   Yes it's theoretically possible to make a hexagonal/other map}
  TAngle = (aTop,aRight,aBottom,aLeft,aUp,aDown);

  {well... 2-abundance is obsolete, really.
   I was making it in order to protect the code from errors
   when writing a lot of code without being able to visually test
   if it produces a correct (intended) result, so I had to introduce
   a lot of internal checks including 2-abundance...
   It both consumes additional memory and requires more CPU time.
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
  BasicTile = {packed} record
    base: TTileKind;
    faces: array [TAngle] of TTileFace; //this is 2x redundant!
  end;

//unused yet;
{const
  InacceptibleTile: BasicTile =
    (base: tkInacceptible;
     faces: (tfNone,tfNone,tfNone,tfNone,tfNone,tfNone));}

type
  {A large tile used in dungeon generation. With a set of Basic_Tile_type
   and additional parameters and procedures
   todo: split DTile and DGenerationTile}
  DTileMap = class
    private
      fReady: boolean;
    public
      {size of this tile}
      tilesizex,tilesizey,tilesizez: byte;
      {internal map of this tile}
      TileMap: array of array of array of BasicTile;
      {name of this tile for debugging}
      TileName: string;

      {if this tile is a blocker?}
      blocker: boolean;
      {how many free faces are there}
      FreeFaces: integer;
      {does this tile has stairs down?}
      Has_stairs_down: boolean;
      {is the tile ready to work (loaded and parsed correctly)?}
      property Ready: boolean read fReady write fReady;
      constructor Load(URL: string);
      {unused}
      procedure setsize(tx: integer = 1; ty: integer = 1; tz: integer = 1);
      {distribute memory according to tilesizex,tilesizey,tilesizez}
      procedure GetMapMemory;
      {calculates faces of the tile and prepares it for work}
      procedure CalculateFaces;
      {checks if tx,ty,tz are correct for this tile}
      function IsSafe(tx,ty,tz: integer): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
      function IsSafe(tx,ty: integer): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
      {safe way to get BasicTile
       (checks if tx,ty,tz are within the Tile size and returns tkInacceptible otherwise) }
      function TileMapSafe(tx,ty,tz: integer): BasicTile; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
      { safe way to get Tile base
       (checks if tx,ty,tz are within the Tile size and returns tkInacceptible otherwise) }
      function TileMapSafeBase(tx,ty,tz: integer): TTileKind; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
      { safe way to get Tile Face
        (checks if tx,ty,tz are within the Tile size and returns tfInacceptible otherwise) }
      function TileMapSafeFace(tx,ty,tz: integer; face: TAngle): TTileFace; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
end;

{type Generator_tile = class(DTile)
end;}

type TTileMapList = specialize TFPGObjectList<DTileMap>;

//var MaxTileTypes: integer;
    //TilesList: TStringList;

{ convert TTileKind to a string for saving }
function TileKindToStr(value: TTileKind): string;
{ convert a string to TTileKind for loading }
function StrToTileKind(value: string): TTileKind;

function TileFaceToStr(value: TTileFace): string;
function StrToTileFace(value: string): TTileFace;
function AngleToStr(value: TAngle): string;
function StrToAngle(value: string): TAngle;

{check if this tile is Passable - in a safe way}
function isPassable(value: TTileFace): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function isPassable(value: TTileKind): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{determine x/y shifts introduced by current Angle}
function a_dx(Angle: TAngle): integer; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function a_dy(Angle: Tangle): integer; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{inverse function - calculate angle based on dx,dy}
function getAngle(ddx,ddy: integer): TAngle; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{inverts Angle to the opposite of the pair}
function InvertAngle(Angle: TAngle): TAngle; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

procedure LoadTiles;
procedure destroyTiles;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses sysUtils, CastleURIUtils, CastleLog,
  DOM, CastleXMLUtils;

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

{---------------------------------------------------------------------------}

function TileFaceToStr(value: TTileFace): string;
begin
  case Value of
    tfNone: Result := 'none';
    tfWall: Result := 'wall';
    tfFree..high(TTileFace): Result := IntToStr(value);
    else raise exception.create('Unknown TTileFace value in TileFaceToStr');
  end;
end;
function StrToTileFace(value: string): TTileFace;
begin
  case value of
    'none': Result := tfNone;
    'wall': result := tfWall;
    else Result := StrToInt(value);
  end;
end;

{---------------------------------------------------------------------------}

function AngleToStr(value: TAngle): string;
begin
  case value of
    aTop:    Result := 'top';
    aBottom: Result := 'bottom';
    aLeft:   Result := 'left';
    aRight:  Result := 'right';
    aUp:     Result := 'up';
    aDown:   Result := 'down';
  end;
end;
function StrToAngle(value: string): TAngle;
begin
  case value of
    'top':    Result := aTop;
    'bottom': Result := aBottom;
    'left':   Result := aLeft;
    'right':  Result := aRight;
    'up':     Result := aUp;
    'down':   Result := aDown;
  end;
end;

{---------------------------------------------------------------------------}

function isPassable(value: TTileFace): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  if value >= tfFree then result := true else result := false;
end;
function isPassable(value: TTileKind): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  if (value = tkFree) or (value = tkUp) or (value = tkDown) then
    result := true
  else
    result := false;
end;

{----------------------------------------------------------------------}

function a_dx(Angle: TAngle): integer; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  case Angle of
    aLeft:  Result := -1;
    aRight: Result := +1;
    else Result := 0;
  end;
end;
function a_dy(Angle: Tangle): integer; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  case Angle of
    aTop:    Result := -1;
    aBottom: Result := +1;
    else Result := 0;
  end;
end;

{----------------------------------------------------------------------}

function getAngle(ddx,ddy: integer): TAngle; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  if ddy<0 then Result := aTop else
  if ddy>0 then Result := aBottom else
  if ddx<0 then Result := aLeft else
  if ddx>0 then Result := aRight;
end;

{----------------------------------------------------------------------}

function InvertAngle(Angle: TAngle): TAngle; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  case Angle of
    aTop:    Result := aBottom;
    aBottom: Result := aTop;
    aLeft:   Result := aRight;
    aRight:  Result := aLeft;
    aUp:     Result := aDown;
    aDown:   REsult := aUp;
    else raise Exception.create('Unknown Angle in DecoDungeonTiles.InvertAngle');
  end;
end;

{======================== DTILE TYPE ==========================================}

constructor DTileMap.Load(URL: string);
var TileDOC: TXMLDocument;
    RootNode,WorkNode,ValueNode: TDOMElement;
    Iterator: TXMLElementIterator;
    jx,jy,jz: integer;
    j: TAngle;
begin
  //inherited;
  TileName := DeleteURIExt(ExtractURIName(URL));
  WriteLnLog('DTileMap.Load',URL);

  {todo: clear the tile with base = tkInacceptible
   and check that all tile elements are loaded and give an error otherwise
   Unexpected errors might occur on damaged game data }

  TileDoc := nil;
  try
    TileDOC := URLReadXML(URL);
    RootNode := TileDOC.DocumentElement;
    WorkNode := RootNode.ChildElement('Size');
    TileSizeX := WorkNode.AttributeInteger('size_x');
    TileSizeY := WorkNode.AttributeInteger('size_y');
    TileSizeZ := WorkNode.AttributeInteger('size_z');
    SetSize(TileSizex,TileSizeY,TileSizeZ);
    blocker := WorkNode.AttributeBoolean('blocker');

    Iterator := RootNode.ChildrenIterator;
    try
      while Iterator.GetNext do if Iterator.current.NodeName = UTF8decode('Tile') then
      begin
        ValueNode := Iterator.current;
        jx := ValueNode.AttributeInteger('x');
        jy := ValueNode.AttributeInteger('y');
        jz := ValueNode.AttributeInteger('z');
        WorkNode := ValueNode.ChildElement('base', true);
        TileMap[jx,jy,jz].base := StrToTileKind(WorkNode.AttributeString('tile_kind'));
        WorkNode := ValueNode.ChildElement('faces', true);
        for j in TAngle do
           TileMap[jx,jy,jz].faces[j] := StrToTileFace(WorkNode.AttributeString(AngleToStr(j)));
      end;
    finally
      FreeAndNil(Iterator);
    end;
    fReady := true;
  except
    fReady := false;
  end;
  FreeAndNil(TileDOC);

  CalculateFaces;

  if not Ready then
    raise Exception.create('Fatal Error in DTileMap.Load! Unable to open file '+TileName);
end;

{----------------------------------------------------------------------------}

procedure DTileMap.setsize(tx: integer = 1; ty: integer = 1; tz: integer = 1);
begin
  tilesizex := tx;
  tilesizey := ty;
  tilesizez := tz;
  //initialize the dynamic arrays
  if (tilesizex<>0) and (tilesizey<>0) and (tilesizez<>0) then
    //in case this is a normal tile...
    GetMapMemory
  else
    //in case this is a "blocker" tile we still need a complete 1x1x1 base
    raise exception.Create('DTileMap.setsize: Tilesize is zero!');
end;

{----------------------------------------------------------------------------}

procedure DTileMap.GetMapMemory;
var ix,iy: integer;
begin
  {set length of 3d dynamic array}
  setlength(TileMap,tilesizex);
  for ix := 0 to tilesizex-1 do begin
    setlength(TileMap[ix],tilesizey);
    for iy := 0 to tilesizey-1 do
      setlength(TileMap[ix,iy],tilesizez);
  end;
end;

{----------------------------------------------------------------------------}

procedure DTileMap.CalculateFaces;
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
    iy:=tilesizey-1;
    for ix:=0 to tilesizex-1 do if isPassable(TileMap[ix,iy,iz].faces[aBottom]) then inc(FreeFaces);
  end;
  //check if it's a blocker tile
  if (FreeFaces = 1) and (tilesizex+tilesizey+tilesizez = 3) and (TileMap[0,0,0].base = tkNone) then
    blocker:=true
  else
    blocker:=false;
  //check if it has stairs down for later generation
  has_stairs_down := false;
  for ix:=0 to tilesizex-1 do
   for iy:=0 to tilesizey-1 do
    for iz:=0 to tilesizez-1 do if TileMap[ix,iy,iz].base=tkDown then has_stairs_down := true;
end;

{-------------------------------------------------------------------------}

function DTileMap.IsSafe(tx,ty,tz: integer): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  if (tx>=0)        and (ty>=0)        and (tz>=0)        and
     (tx<TileSizex) and (ty<TileSizeY) and (tz<TileSizeZ)
  then
    result := true
  else
    result := false;
end;
function DTileMap.IsSafe(tx,ty: integer): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  if (tx>=0)        and (ty>=0)        and
     (tx<TileSizex) and (ty<TileSizeY)
  then
    result := true
  else
    result := false;
end;

{--------------------------------------------------------------------}

Function DTileMap.TileMapSafe(tx,ty,tz: integer): BasicTile; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  if IsSafe(tx,ty,tz) then
    result := TileMap[tx,ty,tz]
  else
    result.base := tkInacceptible;
  //maybe Result := InacceptibleTile?
end;
Function DTileMap.TileMapSafeBase(tx,ty,tz: integer): TTileKind; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  if IsSafe(tx,ty,tz) then
    result := TileMap[tx,ty,tz].base
  else
    result := tkInacceptible;
end;
Function DTileMap.TileMapSafeFace(tx,ty,tz: integer; face: TAngle): TTileFace; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  if IsSafe(tx,ty,tz) then
    result := TileMap[tx,ty,tz].faces[face]
  else
    result := tfInacceptible;
end;

{=============================================================================}

procedure LoadTiles; deprecated;
begin
  WriteLnLog('decodungeontiles','Load tiles started...');
  //dummy
end;

{----------------------------------------------------------------------------}

procedure DestroyTiles; deprecated;
begin
  //dummy
end;

end.

