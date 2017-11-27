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

{contains basic descriptions for each Race and Profession}
unit DecoRaceProfession;

{$INCLUDE compilerconfig.inc}

interface

uses Classes, DecoPerks, DecoStats,
  DecoGlobal;

//Type TRaceCompatibility = (rcNo, rcFull, rcLimited);

type
  {At this moment there is no Difference between race and Profession in data they store, only processing is Different}
  DRaceProfession = class(TComponent) //DObject;
  public
    {Stats and bonuses}
    Stats, Bonus: DStats;
    {Sum of bonuses and stats, only needed for Profession to quickly discern between 1-bonus and 2-bonus Professions}
    BonusSum, StatsSum: integer;
    {Name and shortname of the race/prof.}
    FullName: string;
    Shortname: string[3];
    {Extended description of the race/Profession}
    Description: string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ make them generic lists }
var
  Races, Professions: array of DRaceProfession;

{Loads data for all in-game races and Professions}
procedure LoadRaceProfessionData;
{Checks compatibility of the race and Profession in 0..1 range; returns "-1" if and  race combination is impossible}
function CheckCompatibiliyGeneration(const Race, Profession: DRaceProfession): Float;
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, DecoLog, Profiler;

procedure LoadRaceProfessionData;
var
  i: integer;
begin
  StartProfiler;

  {Todo: load from file}
  SetLength(Races, 1);
  Races[0] := DRaceProfession.Create(Window);
  with races[0] do
  begin
    Name := '';
    Shortname := '';
    Description := '';
    Stats.Value[st_str] := 12;
    Bonus.Value[st_str] := 0;
    //...
  end;
  SetLength(Professions, 1);
  Professions[0] := DRaceProfession.Create(Window);
  with Professions[0] do
  begin
    Name := '';
    Shortname := '';
    Description := '';
    Stats.Value[st_str] := 0;
    Bonus.Value[st_str] := 0;
    //...
    {Calculate bonus sum for Profession}
    BonusSum := 0;
    for i := 0 to MaxStats do
      Inc(BonusSum, Bonus.Value[i]);
    StatsSum := 0;
    for i := 0 to MaxStats do
      Inc(StatsSum, Stats.Value[i]);
  end;

  StopProfiler;
end;

constructor DRaceProfession.Create(AOwner: TComponent);
begin
  StartProfiler;

  inherited Create(AOwner);
  Stats := DStats.Create(True);
  Bonus := DStats.Create(True);

  StopProfiler;
end;

destructor DRaceProfession.Destroy;
begin
  StartProfiler;

  FreeAndNil(Stats);
  FreeAndNil(Bonus);
  inherited Destroy;

  StopProfiler;
end;

function CheckCompatibiliyGeneration(const race, Profession: DRaceProfession): Float;
var //flg: boolean;
  i, DiffBonus, DiffStats: integer;
begin
  StartProfiler;

  DiffBonus := 0;
  for i := 0 to MaxStats do
    if Race.Bonus.Value[i] < Profession.Bonus.Value[i] then
      Inc(DiffBonus);
  if DiffBonus > Profession.BonusSum then
    Result := -1
  else
  begin
    DiffStats := 0;
    for i := 0 to MaxStats do
      if Race.Stats.Value[i] < Profession.Stats.Value[i] then
        Inc(DiffStats, Profession.Stats.Value[i] - Race.Stats.Value[i]);
    if DiffBonus > 0 then
      Result := 0.5
    else
      Result := 1;
    Result := Result * (1 - DiffStats / Profession.StatsSum);
  end;
{  ....

  If Profession.Bonussum = 1 then begin
    //Check stats requirements
    Flg := true;
    for i := 0 to MaxStats do begin
      If Race.Stats[i] < Profession.Stats[i] then flg := false;
      break;
    end;
    if flg then Result := rcFull else Result := rcLimited;
  end else begin
    //Check bonus requirements
    Diff := 0;
    for i := 0 to MaxStats do
      if (Race.Bonus[i] = 0) and  (Profession.Bonus[i] > 0) then inc(Diff);

    If Diff > 1 then Result := rcNo else begin
      //Check stats requirements --- make this a procedure.
      Flg := true;
      for i := 0 to MaxStats do begin
        If Race.Stats[i] < Profession.Stats[i] then flg := false;
      break;
      end;
      if flg then Result := rcFull else Result := rcLimited;
    end;
  end;}

  StopProfiler;
end;
// Float! <0 - impossible 0..1 - compatible

end.
