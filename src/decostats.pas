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

{ Contains description of basic characters and monsters stats }

unit DecoStats;

{$INCLUDE compilerconfig.inc}

interface

uses
  DecoGlobal;

const
  {stats for actors}
  St_str = 0;
  St_dx = 1;
  St_ag = 2;
  St_acc = 3;
  MaxBaseStats = 3; {+1}
  {stats for player characters}
  St_end = 1;
  St_spd = 5;
  St_chem = 6;
  St_phys = 7;
  St_bio = 8;
  St_mph = 9;
  MaxStats = 9; {+1}

type
  {A record representing the base stats array}
  DStats = class(DObject)
  public
    Value: array of integer;
    Count: integer;
    constructor Create(SetFullStats: boolean);
    destructor Destroy; override;
  end;

implementation

uses DecoLog, Profiler;

constructor DStats.Create(SetFullStats: boolean);
begin
  {StartProfiler}
  //inherited Create;

  if SetFullStats then
    Count := MaxStats + 1
  else
    Count := MaxBaseStats + 1;
  SetLength(Value, Count);

  {StopProfiler}
end;

destructor DStats.Destroy;
begin
  {StartProfiler}

  SetLength(Value, 0); //redundant
  inherited Destroy;

  {StopProfiler}
end;

end.
