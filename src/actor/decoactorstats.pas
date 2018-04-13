{ Copyright (C) 2012-2018 Yevhen Loza

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>. }

{---------------------------------------------------------------------------}

(* An abstract stat container enabling changing values and preforming automatic checks *)

{$INCLUDE compilerconfig.inc}

unit DecoActorStats;

interface

uses
  CastleVectors,
  DecoGlobal;

type
  { Result of changing the stat:
    srOk - it worked.
    srBelowZero - the actual stat fell below zero
    srAboveMax - it worked and the stat got clamped by Max (not sure if it's needed)
    srNoNeed - the stat is already at max and the action may be cancelled }
  TStatResult = (srOk, srAboveMax, srBelowZero, srNoNeed);


type
  { A stat, such as health, stamina or strength which are relatively static for Actor
    but can temporarily change in game situations

    Value[0] = Current value, represents current state of the stat
    Value[1] = Max value, represents current maximum value of the stat
               Current value cannot be greater than Max
               But the Max can be changed by buffs or debuffs
    Value[2] = MaxMax value, or absolute current maximum value
               determined by the Actor's current level/type
               Neither current, nor max values can be greater than this
               (warning: some buffs should be able to increase Max above MaxMax) }
  DStat = object
  public
    { [0]Current - [1]Max - [2]Max-Max value }
    Value: TVector3;
  public
    { Change values of this stat
      aSkill must be in 0..1 range and represents how much the aValue change
      affects Max value. }
    function ChangeCurrent(const aValue: DFloat; const aSkill: DFloat): TStatResult;
    function ChangeMax(const aValue: DFloat): TStatResult;
    function SetValue(const aValue: DFloat): TStatResult;
    function SetMaxMax(const aValue: DFloat): TStatResult;
    { Resets Current to Max }
    procedure ResetToMax;
    { Resets Current and Max to MaxMax }
    procedure ResetToMaxMax;
  end;

{............................................................................}
implementation
uses
  SysUtils,
  DecoMath, DecoLog;

function DStat.ChangeCurrent(const aValue: DFloat; const aSkill: DFloat): TStatResult;
begin
  if (aSkill < 0) or (aSkill > 1) then
    Log(LogActorWarning, CurrentRoutine, 'Warning aSkill value is wrong: ' + FloatToStr(aSkill));

  Value[0] := Value[0] + aValue;

  ChangeMax(aValue * aSkill); //we drop the function result here, not sure if its ok

  if Value[0] <= 0 then
    Result := srBelowZero
  else
  if dZero(Value[0] - Value[1]) then
    Result := srNoNeed
  else
  if Value[0] > Value[1] then
  begin
    Value[0] := Value[1];
    Result := srAboveMax;
  end
  else
    Result := srOk;
end;

{-----------------------------------------------------------------------------}

function DStat.ChangeMax(const aValue: DFloat): TStatResult;
begin
  Value[1] := Value[1] + aValue;

  if Value[1] <= 0 then
    Result := srBelowZero
  else
  if dZero(Value[1] - Value[2]) then
    Result := srNoNeed
  else
  if Value[1] > Value[2] then
  begin
    Value[1] := Value[2];
    Result := srAboveMax;
  end
  else
    Result := srOk;
end;

{-----------------------------------------------------------------------------}

function DStat.SetValue(const aValue: DFloat): TStatResult;
begin
  if aValue <= 0 then
  begin
    Value[0] := aValue;
    Result := srBelowZero;
  end
  else
  if aValue > Value[1] then
  begin
    Value[0] := Value[1];
    Result := srAboveMax;
  end
  else
  begin
    Value[0] := aValue;
    Result := srOk;
  end;
end;

{-----------------------------------------------------------------------------}

function DStat.SetMaxMax(const aValue: DFloat): TStatResult;
begin
  Value[2] := aValue;
  if aValue > 0 then
    Result := srOk
  else
    Result := srBelowZero;
end;

{-----------------------------------------------------------------------------}

procedure DStat.ResetToMax;
begin
  Value[0] := Value[1];
end;

{-----------------------------------------------------------------------------}

procedure DStat.ResetToMaxMax;
begin
  Value[1] := Value[2];
  Value[0] := Value[1];
end;

end.

