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

{ Creatures are animated and interactive entities in the World.
  They are spawned and managed by the World. }
unit DecoCreatures;

{$INCLUDE compilerconfig.inc}

interface

uses CastleCreatures;

type
  { Generic animated creature either monster, NPC
    or player's characters (if I'll manage make them viewable) }
  {$WARNING it conflicts with DActor}
  DCreature = class (TCreature)

  end;

implementation



end.

