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
{uses
  decoglobal; }

Const
  {stats for actors}
  St_str = 0;
  St_dx = 1;
  St_ag = 2;
  St_acc = 3;
  Maxbasestats = 3; {+1}
  {stats for player characters}
  St_end = 1;
  St_spd = 5;
  St_chem = 6;
  St_phys = 7;
  St_bio = 8;
  St_mph = 9;
  Maxstats = 9; {+1}

Type
  {A record representing the base stats array}
  DStats = class(TObject)
  public
    Value: array of integer;
    Count: integer;
    Constructor Create(SetFullStats: boolean);
    Destructor Destroy; override;
  End;

implementation

Constructor DStats.Create(SetFullStats: boolean);
Begin
  If SetFullStats then
    Count := MaxStats+1
  Else
    Count := MaxBaseStats+1;
  SetLength(Value,Count);
End;

Destructor DStats.Destroy;
begin
  SetLength(Value,0); //redundant
end;

end.

