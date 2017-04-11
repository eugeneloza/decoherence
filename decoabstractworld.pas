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

uses
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
  Public
    //Seed:
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
  End;

implementation

end.

