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

{ routines linked to Navigation&pathfinding
  Actually it is a very simplified map graph
  Pay attention that NavMesh is built by the World, and processed here }
unit DecoNav;

{$INCLUDE compilerconfig.inc}
interface

uses CastleGenericLists,
  DecoGlobal;

Type DNavPt = record
  { Absolute Coordinates of pathPoint }
  x,y,z: float;
  {{ world tile it belongs to }
  Tile: TTileType;
  {Link of tiles adjacent to this tile}
  Links: TLinkList;}
end;

type TNavList = specialize TGenericStructList<DNavPt>;



implementation

end.

