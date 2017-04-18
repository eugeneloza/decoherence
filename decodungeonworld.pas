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

unit decodungeonworld;

{$INCLUDE compilerconfig.inc}
interface

uses
  decoabstractworld;

type
  {Dungeon world manages any indoor tiled location}
  DDungeonWorld = class(DAbstractWorld)
  private
    {Manages tiles (show/hide/trigger events)}
    Procedure manage_tiles; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
  public
    {Detects if the current tile has been changed and launches manage_tiles}
    Procedure manage; override;
    {Sorts tiles into chunks}
    //Procedure chunk_n_slice; override;

  end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

procedure DDungeonWorld.manage_tiles; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  {prepare visibe lists,
   we can't assign the values directly, because groups can contain multiple tiles
   and the effect might be overlapping
   there should be no performance/memory issues with two additional local arrays, I hope
   however, some optimization here might come in handy some day}

  {
  repeat
  {  if old[i].tile = new[i].tile then ;
    else}
    if old[i].tile > new[j].tile then begin
      {there's a new tile to turn on}
      tile[new[j]] := true
      inc(j);
    end else
    if new[i].tile>old[j].tile then begin
    {there's an old tile to turn off}
      tile[old[i].tile] := false;
      inc(i);
    end else begin
      {the tile exists in both new and old neighbours lists, change nothing and advance to the next tile}
      inc(i);
      inc(j);
    end;
  until i>= old.count-1 and j>= new.count-1; {$warning check here}
  {now we actually put the calculated arrays into current 3d world}
  for i := 0 to group.count-1 do group[i] := false;
  for i := 0 to new.count-1 do group...
  }

end;

procedure DDungeonWorld.manage;
begin
  //if tilechanged then
  manage_tiles;
end;

end.

