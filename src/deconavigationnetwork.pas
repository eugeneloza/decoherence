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

{ Routines linked to Navigation&pathfinding
  Actually it is a very simplified map graph
  Pay attention that NavMesh is built by the World, and processed here }
unit DecoNavigationNetwork;

{$INCLUDE compilerconfig.inc}
interface

uses CastleGenericLists, CastleVectors,
  DecoGlobal;

type TNavID = integer;
const UnitinializedNav: TNavID = -1;

{$hint not sure how to work correctly here, specialized lists must be managed (from a class constructor/destructor)}
{looks like 8 nav points links is enough}
type TLinksList = array [0..7] of TNavID; //specialize TGenericStructList<TNavID>;

Type DNavPt = record
  { Absolute Coordinates of pathPoint }
  Pos: TVector3;
  {{ world tile it belongs to }
  Tile: TTileType;}
  {Link of tiles adjacent to this tile}
  LinksCount: shortint;
  isSafe: boolean;
  Links: TLinksList;
  {is somebody standing over this Nav?}
  Blocked: boolean;
end;

type TWeenieType = (wtEntrance);
type
  { this is an "interesting point" in the map,
    required to spawn enemies properly, set up triggers,
    etc,etc,etc.
    I'm not sure yet how to organize them flexible and easily. }
  DWeenie = record
    Kind: TWeenieType;
    NavId: TNavID;
end;

type TNavList = specialize TGenericStructList<DNavPt>;
type TWeeniesList = specialize TGenericStructList<DWeenie>;

implementation

end.

