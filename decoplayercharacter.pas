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

{ Definitions and routines for player character and recruitable characters }
unit decoplayercharacter;

{$INCLUDE compilerconfig.inc}

interface

uses Classes,
  DecoActor, DecoRaceProfession, DecoPerks,
  DecoGlobal;

const MaxParty = 6; {0..6 = 7 characters}

Type
  {player character - the most complex actor available :)}
  DPlayerCharacter = class(DActor)
  public
    Actions: DPerksList;
    Procedure Die; override;
    constructor Create; override;
    destructor Destroy; override;
end;

  {$WARNING party must be an object with a proper create/destroy}
var Party: array[0..MaxParty] of DPlayerCharacter;

{creates a test party. Temporary}
procedure CreateTestParty;
procedure FreeTestParty;
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses SysUtils, CastleLog;

procedure CreateTestParty;
var i: integer;
begin
  for i := 0 to MaxParty do begin
    Party[i] := DPlayerCharacter.Create;
    {if i<4 then }Party[i].MaxMaxMPH := 0;
    Party[i].Hit(DRND.Random(80),1);
    Party[i].DrainCNC(DRND.Random(80),1);
    Party[i].DrainMPH(DRND.Random(80),1);
    Party[i].DrainSTA(DRND.Random(80),1);
    if (Perks=nil) or (Perks.Count=0) then WriteLnLog('CreateTestParty','FATAL ERROR: Perks is empty!');
    Party[i].Actions := DPerksList.Create(false);
    Party[i].Actions.Add(Perks[0]);
  end;

end;

{----------------------------------------------------------------------------}

procedure FreeTestParty;
var i: integer;
begin
  for i:= 0 to MaxParty do FreeAndNil(Party[i]);
end;

{======================== DPlayerCharacter ==================================}

constructor DPlayerCharacter.Create;
begin
  inherited;
end;

{----------------------------------------------------------------------------}

Procedure DPlayerCharacter.die;
begin
  WriteLnLog('DPlayerCharacter.die','Character has died');
end;

{----------------------------------------------------------------------------}

destructor DPlayerCharacter.destroy;
begin
  FreeAndNil(Actions);
  Inherited;
end;

end.

