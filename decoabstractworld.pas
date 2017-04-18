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

{ contains definitions for most abstract World entity }

unit decoabstractworld;

{$INCLUDE compilerconfig.inc}
interface

uses CastleRandom,
  decoabstractgenerator;

{Type PathElement = record
  {Absolute Coordinates of pathPoint}
  X,y,z: TCoordinate;
  {world tile it belongs to}
  Tile: TTileType;
  {Link of tiles adjacent to this tile}
  Links: TLinkList;
end;}

Type
  {The most abstract world implementation to merge independent world routines
   Like manage and pathfind}
  DAbstractWorld = class
  private
    fSeed: LongWord;
    { xorshift random generator, fast and thread-safe }
    RNDM: TCastleRandom;
  public
    {Seed used to "build" the world if it requires random}
    property Seed: LongWord read fSeed write fSeed;
    {World management routine. Called every frame;
     Most important thing it does is managing LODs of tiles/landscape
     And hiding/LODding world chunks}
    Procedure manage; virtual; abstract;
    {Builds a PathTree for the world}
    //Function pathfind: DPathTree;
    {load the World from a file}
    procedure Load(URL: string); virtual; abstract;
    {load the World from a running Generator}
    procedure Load(Generator: DAbstractGenerator); virtual; abstract;
    {builds a world from the obtained data }
    procedure Build; virtual;
    {Splits the World into chunks}
    //Procedure chunk_n_slice; virtual; abstract;
    constructor create;
    destructor destroy; override;
  End;

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, classes;

constructor DAbstractWorld.create;
begin
  inherited;
  {we create an non-initialized random (i.e. initialized by a stupid constant integer)
  Just to make sure we don't waste any time (<1ms) on initialization now}
  RNDM := TCastleRandom.Create(1);
end;

{------------------------------------------------------------------------------}

destructor DAbstractWorld.destroy;
begin
  FreeAndNil(RNDM);
  inherited;
end;

{------------------------------------------------------------------------------}

procedure DAbstractWorld.Build;
begin
  if fSeed = 0 then raise exception.create('DAbstractWorld.Build: World random must be predefined!');
  RNDM.Initialize(fSeed);
end;

end.

