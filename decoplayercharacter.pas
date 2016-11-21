{Copyright (C) 2012-2016 Yevhen Loza

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

{ Definitions and routines for player character and recruitable characters }
unit decoplayercharacter;

{$INCLUDE compilerconfig.inc}

interface

uses classes,
  decoactor,
  decoglobal;

const maxparty = 6; {7 characters}

Type
  {player character - the most complex actor available :)}
  DPlayerCharacter = class(DActor)
  public
    Procedure die; override;
    constructor create(AOwner: Tcomponent); override;
end;

var Party: array[0..maxparty] of DPlayerCharacter;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

implementation

uses castleLog;

constructor DPlayerCharacter.create(AOwner: TComponent);
begin
  inherited create(AOwner);
end;

{----------------------------------------------------------------------------}

Procedure DPlayerCharacter.die;
begin
  WriteLnLog('DPlayerCharacter.die','Character has died');
end;


end.

