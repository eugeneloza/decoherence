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
unit DecoDungeonTiles;

{$INCLUDE compilerconfig.inc}
interface

uses Classes, CastleImages,
  DecoGlobal;

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
const
  THorizontalAngle: set of TAngle = [aTop,aRight,aBottom,aLeft];
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
    Base: TTileKind;
    Faces: array [TAngle] of TTileFace; //this is 2x redundant!
  end;

//inacceptible tile, used as "return"
const
  InacceptibleTile: BasicTile =
    (Base:   tkInacceptible;
     Faces: (tfInacceptible, tfInacceptible, tfInacceptible,
             tfInacceptible, tfInacceptible, tfInacceptible));

type
  {common routines shared by TileMap and DungeonMap}
  DMap = class (DObject)
    public
      {size of this map}
      SizeX, SizeY, SizeZ: byte;
      {internal map of this tile}
      Map: array of array of array of BasicTile;

      {images of this tile/map 0..sizez-1}
      Img: array of TRGBAlphaImage;

      {sets the size of the tile and adjusts memory}
      procedure SetSize(const tx: integer = 1; const ty: integer = 1; const tz: integer = 1);
      {distribute memory according to tilesizex,tilesizey,tilesizez}
      procedure GetMapMemory;
      {empties the map with walls at borders}
      procedure EmptyMap(const InitToFree: boolean);
      {checks if tx,ty,tz are correct for this tile}
      function IsSafe(const tx,ty,tz: integer): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
      function IsSafe(const tx,ty: integer): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
      {safe way to get BasicTile
       (checks if tx,ty,tz are within the Tile size and returns tkInacceptible otherwise) }
      function MapSafe(const tx,ty,tz: integer): BasicTile; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
      { safe way to get Tile base
       (checks if tx,ty,tz are within the Tile size and returns tkInacceptible otherwise) }
      function MapSafeBase(const tx,ty,tz: integer): TTileKind; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
      { safe way to get Tile Face
        (checks if tx,ty,tz are within the Tile size and returns tfInacceptible otherwise) }
      function MapSafeFace(const tx,ty,tz: integer; const Face: TAngle): TTileFace; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

      destructor Destroy; override;
      {frees the minimap memory (careful)}
      procedure FreeMinimap;
end;

type
  {A large tile used in dungeon generation. With a set of Basic_Tile_type
   and additional parameters and procedures
   todo: split DTile and DGenerationTile}
  DTileMap = class (DMap)
    private
      fReady: boolean;
    public
      {name of this tile for debugging}
      TileName: string;
      {if this tile is a blocker?}
      Blocker: boolean;

      {is the tile ready to work (loaded and parsed correctly)?}
      property Ready: boolean read fReady write fReady;
      constructor Load(const URL: string; const GZipped: boolean);// override;
end;

//var MaxTileTypes: integer;
    //TilesList: TStringList;

{ convert TTileKind to a string for saving }
function TileKindToStr(const Value: TTileKind): string;
{ convert a string to TTileKind for loading }
function StrToTileKind(const Value: string): TTileKind;

function TileFaceToStr(const Value: TTileFace): string;
function StrToTileFace(const Value: string): TTileFace;
function AngleToStr(const Value: TAngle): string;
function StrToAngle(const Value: string): TAngle;

{check if this tile is Passable - in a safe way}
function isPassable(const Value: TTileFace): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function isLookable(const Value: TTileFace): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function isPassable(const Value: TTileKind): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{determine x/y shifts introduced by current Angle}
function a_dx(const Angle: TAngle): integer; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function a_dy(const Angle: Tangle): integer; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function a_dz(const Angle: Tangle): integer; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{inverse function - calculate angle based on dx,dy}
function GetAngle(const ddx, ddy: integer): TAngle; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{inverts Angle to the opposite of the pair}
function InvertAngle(const Angle: TAngle): TAngle; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses SysUtils, CastleURIUtils,
  DOM, CastleXMLUtils,
  DecoLog, Profiler;

function TileKindToStr(const Value: TTileKind): string;
begin
  StartProfiler;

  case Value of
    tkNone: Result := 'NA';
    tkFree: Result := 'FREE';
    tkUP:   Result := 'UP';
    tkDown: Result := 'DOWN';
    tkWall: Result := 'WALL';
    tkInacceptible: raise Exception.Create('tkInacceptible in TileKindToStr');  //this one shouldn't appear in string
    else raise Exception.Create('Unknown TileKind in TileKindToStr');
  end;

  StopProfiler;
end;
function StrToTileKind(const Value: string): TTileKind;
begin
  StartProfiler;

  case Value of
    'NA':   Result := tkNone;
    'FREE': Result := tkFree;
    'UP':   Result := tkUp;
    'DOWN': Result := tkDown;
    'WALL': Result := tkWall;
    'ERROR':  raise Exception.Create('tkInacceptible in StrToTileKind');  //this one shouldn't appear in string
    else raise Exception.Create('Unknown TileKind in StrToTileKind');
  end;

  StopProfiler;
end;

{---------------------------------------------------------------------------}

function TileFaceToStr(const Value: TTileFace): string;
begin
  StartProfiler;

  case Value of
    tfNone: Result := 'none';
    tfWall: Result := 'wall';
    tfFree..high(TTileFace): Result := IntToStr(Value);
    else raise Exception.Create('Unknown TTileFace Value in TileFaceToStr');
  end;

  StopProfiler;
end;
function StrToTileFace(const Value: string): TTileFace;
begin
  StartProfiler;

  case Value of
    'none': Result := tfNone;
    'wall': Result := tfWall;
    else Result := StrToInt(Value);
  end;

  StopProfiler;
end;

{---------------------------------------------------------------------------}

function AngleToStr(const Value: TAngle): string;
begin
  StartProfiler;

  case Value of
    aTop:    Result := 'top';
    aBottom: Result := 'bottom';
    aLeft:   Result := 'left';
    aRight:  Result := 'right';
    aUp:     Result := 'up';
    aDown:   Result := 'down';
  end;

  StopProfiler;
end;
function StrToAngle(const Value: string): TAngle;
begin
  StartProfiler;

  case Value of
    'top':    Result := aTop;
    'bottom': Result := aBottom;
    'left':   Result := aLeft;
    'right':  Result := aRight;
    'up':     Result := aUp;
    'down':   Result := aDown;
  end;

  StopProfiler;
end;

{---------------------------------------------------------------------------}

function isPassable(const Value: TTileFace): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  if Value >= tfFree then Result := true else Result := false;
end;
function isLookable(const Value: TTileFace): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  if Value >= tfFree then Result := true else Result := false;
end;
function isPassable(const Value: TTileKind): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  if (Value = tkFree) or (Value = tkUp) or (Value = tkDown) then
    Result := true
  else
    result := false;
end;

{----------------------------------------------------------------------}

function a_dx(const Angle: TAngle): integer; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  case Angle of
    aLeft:  Result := -1;
    aRight: Result := +1;
    else    Result := 0;
  end;
end;
function a_dy(const Angle: Tangle): integer; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  case Angle of
    aTop:    Result := -1;
    aBottom: Result := +1;
    else     Result := 0;
  end;
end;
function a_dz(const Angle: Tangle): integer; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  case Angle of
    aUp:   Result := -1;
    aDown: Result := +1;
    else   Result := 0;
  end;
end;

{----------------------------------------------------------------------}

function GetAngle(const ddx, ddy: integer): TAngle; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  if ddy<0 then Result := aTop else
  if ddy>0 then Result := aBottom else
  if ddx<0 then Result := aLeft else
  if ddx>0 then Result := aRight;
end;

{----------------------------------------------------------------------}

function InvertAngle(const Angle: TAngle): TAngle; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  case Angle of
    aTop:    Result := aBottom;
    aBottom: Result := aTop;
    aLeft:   Result := aRight;
    aRight:  Result := aLeft;
    aUp:     Result := aDown;
    aDown:   Result := aUp;
    else raise Exception.Create('Unknown Angle in DecoDungeonTiles.InvertAngle');
  end;
end;

{======================== DTILE TYPE ==========================================}

constructor DTileMap.Load(const URL: string; const GZipped: boolean);
var TileDOC: TXMLDocument;
    RootNode,WorkNode,ValueNode: TDOMElement;
    Iterator: TXMLElementIterator;
    jx,jy,jz: integer;
    j: TAngle;
    FullURL: string;
begin
  StartProfiler;

  //inherited;
  TileName := DeleteURIExt(ExtractURIName(URL));
  Log(LogInitData,_CurrentRoutine,URL);

  {todo: clear the tile with base = tkInacceptible
   and check that all tile elements are loaded and give an error otherwise
   Unexpected errors might occur on damaged game data }

  //TileDoc := nil;
  try
    FullURL := URL+'.map';
    if gzipped then FullURL += GZ_ext;
    TileDOC := URLReadXML(FullURL);
    RootNode := TileDOC.DocumentElement;
    WorkNode := RootNode.ChildElement('Size');
    SizeX := WorkNode.AttributeInteger('size_x');
    SizeY := WorkNode.AttributeInteger('size_y');
    SizeZ := WorkNode.AttributeInteger('size_z');
    SetSize(Sizex,SizeY,SizeZ);
    Blocker := WorkNode.AttributeBoolean('blocker');

    Iterator := RootNode.ChildrenIterator;
    try
      while Iterator.GetNext do if Iterator.Current.NodeName = UTF8decode('Tile') then
      begin
        ValueNode := Iterator.Current;
        jx := ValueNode.AttributeInteger('x');
        jy := ValueNode.AttributeInteger('y');
        jz := ValueNode.AttributeInteger('z');
        WorkNode := ValueNode.ChildElement('base', true);
        Map[jx,jy,jz].Base := StrToTileKind(WorkNode.AttributeString('tile_kind'));
        WorkNode := ValueNode.ChildElement('faces', true);
        for j in TAngle do
           Map[jx,jy,jz].Faces[j] := StrToTileFace(WorkNode.AttributeString(AngleToStr(j)));
      end;
    finally
      FreeAndNil(Iterator);
    end;
    fReady := true;
  except
    fReady := false;
  end;
  FreeAndNil(TileDOC);

  if not Ready then
    raise Exception.Create('Fatal Error in DTileMap.Load! Unable to open file '+TileName);

  SetLength(Img,SizeZ);
  for jz := 0 to SizeZ-1 do
    Img[jz] := LoadImage(ChangeURIExt(URL,'_'+IntToStr(jz)+'.png'),[TRGBAlphaImage]) as TRGBAlphaImage;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DMap.FreeMinimap;
var jz: integer;
begin
  StartProfiler;

  if Length(Img)>0 then
  for jz := 0 to Length(Img)-1 do
    FreeAndNil(Img[jz]);

  StopProfiler;
end;

destructor DMap.Destroy;
begin
  StartProfiler;

  FreeMinimap;
  inherited Destroy;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DMap.SetSize(const tx: integer = 1; const ty: integer = 1; const tz: integer = 1);
begin
  StartProfiler;

  SizeX := tx;
  SizeY := ty;
  SizeZ := tz;
  //initialize the dynamic arrays
  if (SizeX<>0) and (SizeY<>0) and (SizeZ<>0) then
    //in case this is a normal tile...
    GetMapMemory
  else
    //in case this is a "blocker" tile we still need a complete 1x1x1 base
    raise Exception.Create('DMap.setsize: Tilesize is zero!');

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DMap.GetMapMemory;
var ix,iy: integer;
begin
  StartProfiler;

  {set length of 3d dynamic array}
  SetLength(Map,SizeX);
  for ix := 0 to SizeX-1 do begin
    SetLength(Map[ix],SizeY);
    for iy := 0 to SizeY-1 do
      SetLength(Map[ix,iy],SizeZ);
  end;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DMap.EmptyMap(const InitToFree: boolean);
var jx,jy,jz: integer;
    a: TAngle;
begin
  StartProfiler;

  for jx := 0 to SizeX-1 do
    for jy := 0 to SizeY-1 do
      for jz := 0 to SizeZ-1 do begin
        if InitToFree then begin
          Map[jx,jy,jz].Base := tkFree;
          for a in TAngle do Map[jx,jy,jz].Faces[a] := tfFree;
        end else begin
          Map[jx,jy,jz].Base := tkNone;
          for a in TAngle do Map[jx,jy,jz].Faces[a] := tfNone;
        end;
        {if this tile is at border then make corresponding walls around it}
        if jx = 0       then Map[jx,jy,jz].Faces[aLeft]   := tfWall;
        if jy = 0       then Map[jx,jy,jz].Faces[aTop]    := tfWall;
        if jx = SizeX-1 then Map[jx,jy,jz].Faces[aRight]  := tfWall;
        if jy = SizeY-1 then Map[jx,jy,jz].Faces[aBottom] := tfWall;
        if jz = 0       then Map[jx,jy,jz].Faces[aUp]     := tfWall;
        if jz = SizeZ-1 then Map[jx,jy,jz].Faces[aDown]   := tfWall;
      end;

  StopProfiler;
end;

{-------------------------------------------------------------------------}

function DMap.IsSafe(const tx,ty,tz: integer): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  if (tx >= 0)    and (ty >= 0)    and (tz >= 0)        and
     (tx < Sizex) and (ty < SizeY) and (tz < SizeZ)
  then
    Result := true
  else
    Result := false;
end;
function DMap.IsSafe(const tx,ty: integer): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  if (tx >= 0)    and (ty >= 0)        and
     (tx < SizeX) and (ty < SizeY)
  then
    Result := true
  else
    Result := false;
end;

{--------------------------------------------------------------------}

Function DMap.MapSafe(const tx,ty,tz: integer): BasicTile; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  if IsSafe(tx,ty,tz) then
    Result := Map[tx,ty,tz]
  else
    Result := InacceptibleTile; //this is a bit slower, but more bug-proof
end;
Function DMap.MapSafeBase(const tx,ty,tz: integer): TTileKind; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  if IsSafe(tx,ty,tz) then
    Result := Map[tx,ty,tz].Base
  else
    Result := tkInacceptible;
end;
Function DMap.MapSafeFace(const tx,ty,tz: integer; const Face: TAngle): TTileFace; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  if IsSafe(tx,ty,tz) then
    Result := Map[tx,ty,tz].Faces[face]
  else
    Result := tfInacceptible;
end;

end.

