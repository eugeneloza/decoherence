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

{contains basic descriptions for each race and profession}
unit decoraceprofession;

{$INCLUDE compilerconfig.inc}

interface
Uses classes, decoperks, decostats,
  decoglobal;

//Type TRaceCompatibility = (rcNo, rcFull, rcLimited);

Type
  {At this moment there is no difference between race and profession in data they store, only processing is different}
  DRaceProfession = class(TComponent)
  Private
  Public
    {Stats and bonuses}
    Stats, bonus: DStats;
    {Sum of bonuses and stats, only needed for profession to quickly discern between 1-bonus and 2-bonus professions}
    Bonussum, statssum: integer;
    {Name and shortname of the race/prof.}
    FullName: string;
    Shortname: string[3];
    {Extended description of the race/profession}
    Description: string;
    Constructor create(AOwner: TComponent); override;
    Destructor destroy;
  End;

Var races, professions: array of DRaceProfession;

{Loads data for all in-game races and professions}
Procedure LoadRaceProfessionData;
{Checks compatibility of the race and profession in 0..1 range; returns "-1" if and  race combination is impossible}
Function CheckCompatibiliyGeneration(const race, profession: DRaceProfession): Float;
Implementation
Uses SysUtils;

Procedure LoadRaceProfessionData;
var i: integer;
Begin
  {Todo: load from file}
  SetLength(races,1);
  Races[0] := DRaceProfession.create(Window);
  With races[0] do begin
    Name := '';
    Shortname := '';
    Description := '';
    Stats.value[st_str] := 12; bonus.value[st_str] := 0;
    //...
  End;
  SetLength(professions,1);
  Professions[0] := DRaceProfession.create(Window);
  With professions[0] do begin
    Name := '';
    Shortname := '';
    Description := '';
    Stats.value[st_str] := 0; bonus.value[st_str] := 0;
    //...
    {Calculate bonus sum for profession}
    BonusSum := 0;
    For i := 0 to maxstats do inc(BonusSum,bonus.value[i]);
    StatsSum := 0;
    For i := 0 to maxstats do inc(StatsSum,stats.value[i]);
  End;
End;

Constructor DRaceProfession.create(AOwner: TComponent);
Begin
  Inherited create(AOwner);
  Stats := DStats.create(true);
  Bonus := DStats.create(true);
End;

Destructor DRaceProfession.destroy;
Begin
  freeandnil(stats);
  freeandnil(bonus);
  Inherited;
End;

Function CheckCompatibiliyGeneration(const race, profession: DRaceProfession): Float;
Var flg: boolean;
    i, diffbonus, diffstats: integer;
Begin
  Diffbonus := 0;
  For i := 0 to maxstats do if race.bonus.value[i]<profession.bonus.value[i] then inc(diffbonus);
  If diffbonus > profession.bonusSum then result := -1 else begin
    Diffstats := 0;
    For i := 0 to maxstats do if race.stats.value[i] < profession.stats.value[i] then inc(diffstats, profession.stats.value[i] - race.stats.value[i]);
    If diffbonus > 0 then result := 0.5 else result := 1;
    Result := result*(1-diffstats/profession.statsSum);
  End
{  ....

  If profession.bonussum = 1 then begin
    //Check stats requirements
    Flg := true;
    For i := 0 to maxstats do begin
      If race.stats[i] < profession.stats[i] then flg := false;
      break;
    End;
    If flg then Result := rcFull else result := rcLimited;
  End else begin
    //Check bonus requirements
    Diff := 0;
    For i := 0 to maxstats do
      If (race.bonus[i] = 0) and  (profession. bonus[i] > 0) then inc(diff);

    If diff > 1 then Result := rcNo else begin
      //Check stats requirements --- make this a procedure.
      Flg := true;
      For i := 0 to maxstats do begin
        If race.stats[i] < profession.stats[i] then flg := false;
      break;
      End;
      If flg then Result := rcFull else result := rcLimited;
    End;
  End;}
End;
// Float! <0 - impossible 0..1 - compatible

end.

