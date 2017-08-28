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
unit DecoRaceProfession;

{$INCLUDE compilerconfig.inc}

interface
uses Classes, DecoPerks, DecoStats,
  DecoGlobal;

//Type TRaceCompatibility = (rcNo, rcFull, rcLimited);

type
  {At this moment there is no difference between race and profession in data they store, only processing is different}
  DRaceProfession = class(TComponent)
  public
    {Stats and bonuses}
    Stats, bonus: DStats;
    {Sum of bonuses and stats, only needed for profession to quickly discern between 1-bonus and 2-bonus professions}
    Bonussum, statssum: integer;
    {Name and shortname of the race/prof.}
    FullName: string;
    Shortname: string[3];
    {Extended description of the race/profession}
    Description: string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { make them generic lists }
var Races, Professions: array of DRaceProfession;

{Loads data for all in-game races and professions}
procedure LoadRaceProfessionData;
{Checks compatibility of the race and profession in 0..1 range; returns "-1" if and  race combination is impossible}
function CheckCompatibiliyGeneration(const race, profession: DRaceProfession): Float;
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses SysUtils;

procedure LoadRaceProfessionData;
var i: integer;
begin
  {Todo: load from file}
  SetLength(races,1);
  Races[0] := DRaceProfession.create(Window);
  with races[0] do begin
    Name := '';
    Shortname := '';
    Description := '';
    Stats.value[st_str] := 12; bonus.value[st_str] := 0;
    //...
  end;
  SetLength(professions,1);
  Professions[0] := DRaceProfession.create(Window);
  with professions[0] do begin
    Name := '';
    Shortname := '';
    Description := '';
    Stats.value[st_str] := 0; bonus.value[st_str] := 0;
    //...
    {Calculate bonus sum for profession}
    BonusSum := 0;
    for i := 0 to maxstats do inc(BonusSum,bonus.value[i]);
    StatsSum := 0;
    for i := 0 to maxstats do inc(StatsSum,stats.value[i]);
  end;
end;

constructor DRaceProfession.create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Stats := DStats.create(true);
  Bonus := DStats.create(true);
end;

Destructor DRaceProfession.destroy;
begin
  FreeAndNil(Stats);
  FreeAndNil(Bonus);
  inherited Destroy;
end;

function CheckCompatibiliyGeneration(const race, profession: DRaceProfession): Float;
var //flg: boolean;
    i, diffbonus, diffstats: integer;
begin
  Diffbonus := 0;
  for i := 0 to MaxStats do if Race.Bonus.Value[i]<Profession.Bonus.Value[i] then inc(DiffBonus);
  if diffbonus > Profession.BonusSum then Result := -1 else begin
    DiffStats := 0;
    for i := 0 to MaxStats do if Race.Stats.Value[i] < Profession.Stats.value[i] then inc(DiffStats, Profession.Stats.Value[i] - Race.Stats.Value[i]);
    if DiffBonus > 0 then Result := 0.5 else Result := 1;
    Result := result*(1-diffstats/profession.statsSum);
  end
{  ....

  If profession.bonussum = 1 then begin
    //Check stats requirements
    Flg := true;
    for i := 0 to maxstats do begin
      If race.stats[i] < profession.stats[i] then flg := false;
      break;
    end;
    if flg then Result := rcFull else result := rcLimited;
  end else begin
    //Check bonus requirements
    Diff := 0;
    for i := 0 to maxstats do
      if (race.bonus[i] = 0) and  (profession. bonus[i] > 0) then inc(diff);

    If diff > 1 then Result := rcNo else begin
      //Check stats requirements --- make this a procedure.
      Flg := true;
      for i := 0 to maxstats do begin
        If race.stats[i] < profession.stats[i] then flg := false;
      break;
      end;
      if flg then Result := rcFull else Result := rcLimited;
    end;
  end;}
end;
// Float! <0 - impossible 0..1 - compatible

end.

