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

uses classes,
  decoactor, decoraceprofession, decoperks,
  decoglobal;

const maxparty = 6; {0..6 = 7 characters}

Type
  {player character - the most complex actor available :)}
  DPlayerCharacter = class(DActor)
  public
    Actions: DPerksList;
    Procedure die; override;
    constructor create(AOwner: Tcomponent); override;
    destructor destroy; override;
end;

var Party: array[0..maxparty] of DPlayerCharacter;

{creates a test party. Temporary}
procedure CreateTestParty;
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses SysUtils, castleLog;

procedure CreateTestParty;
var i: integer;
begin
  for i := 0 to maxparty do begin
    Party[i] := DPlayerCharacter.create(Window);
    {if i<4 then }party[i].maxmaxMPH := 0;
    party[i].hit(rnd.Random(80),1);
    party[i].drainCNC(rnd.Random(80),1);
    party[i].drainMPH(rnd.Random(80),1);
    party[i].drainSTA(rnd.Random(80),1);
    if (Perks=nil) or (Perks.count=0) then WriteLnLog('CreateTestParty','FATAL ERROR: Perks is empty!');
    party[i].Actions := DPerksList.create(false);
    Party[i].actions.Add(perks[0]);
  end;

end;

{---------------------------------------------------------------------------}

constructor DPlayerCharacter.create(AOwner: TComponent);
begin
  inherited create(AOwner);
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

