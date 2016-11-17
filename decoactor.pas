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

{ Describes characters and creatures basic behaviour }
unit decoactor;

{$mode objfpc}{$H+}
{$INCLUDE compilerconfig.inc}

interface

uses classes, castleLog,
  decoglobal;

{Maybe, add some basic actor (visible only, no collisions)}
Type
  {basic actor. With hit-points and coordinates}
  DActor = class(TComponent)
  private
    fHP,fMAXHP,fMAXMAXHP: float;
    { maybe, move all non-HP to deco player character? }
    fSTA,fMAXSTA,fMAXMAXSTA: float;
    fCONC,fMAXCONC,fMAXMAXCONC: float;
    fMPH,fMAXMPH,fMAXMAXMPH: float;
    Procedure setHP(value: float);
    Procedure setmaxHP(value: float);
    Procedure setmaxmaxHP(value: float);
    Procedure setSTA(value: float);
    Procedure setmaxSTA(value: float);
    Procedure setmaxmaxSTA(value: float);
    Procedure setCONC(value: float);
    Procedure setmaxCONC(value: float);
    Procedure setmaxmaxCONC(value: float);
    Procedure setMPH(value: float);
    Procedure setmaxMPH(value: float);
    Procedure setmaxmaxMPH(value: float);
  public
    { getters and setters }
    Property HP: float read fHP write sethp;
    Property maxHP: float read fMAXHP write setmaxhp;
    Property maxmaxHP: float read fMAXMAXHP write setmaxmaxhp;
    procedure resetHP;
    Property STA: float read fSTA write setSTA;
    Property maxSTA: float read fMAXSTA write setmaxSTA;
    Property maxmaxSTA: float read fMAXMAXSTA write setmaxmaxSTA;
    procedure resetSTA;
    Property CONC: float read fCONC write setCONC;
    Property maxCONC: float read fMAXCONC write setmaxCONC;
    Property maxmaxCONC: float read fMAXMAXCONC write setmaxmaxCONC;
    procedure resetCONC;
    Property MPH: float read fMPH write setMPH;
    Property maxMPH: float read fMAXMPH write setmaxMPH;
    Property maxmaxMPH: float read fMAXMAXMPH write setmaxmaxMPH;
    procedure resetMPH;

    {hit equals to consume+drain}
    procedure hit(damage: float; skill: float); //=consumeHP
    {returns true if healed or false if nothing to heal}
    function heal(value: float; skill: float): boolean; //=restoreHP
    Procedure die; virtual; abstract;

    {"consumption" procedures return true if success and false if failed,
     "restoration" procedures return true if something has been restored,
     "drain" procedures can drain values below zero}
    function consumeSTA(consumption: float; skill: float): boolean;
    function restoreSTA(restoration: float; skill: float): boolean;
    procedure drainSTA(drain: float; skill: float);
    function consumeCONC(consumption: float; skill: float): boolean;
    function restoreCONC(restoration: float; skill: float): boolean;
    procedure drainCONC(drain: float; skill: float);
    function consumeMPH(consumption: float; skill: float): boolean;
    function restoreMPH(restoration: float; skill: float): boolean;
    procedure drainMPH(drain: float; skill: float);
end;

Type
  {player character - the most complex actor available :)}
  DPlayerCharacter = class(DActor)
  public
    Procedure die; override;
end;

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

Procedure DActor.setHP(value: float);
begin
  If value < fmaxHP then fHP := value else fHP := fmaxHP;
  If value < 0 then die;
end;
Procedure DActor.setmaxHP(value: float);
begin
  If value < fmaxmaxHP then fmaxHP := value else fmaxHP := fmaxmaxHP;
  If value < 0 then die;
end;
Procedure DActor.setmaxmaxHP(value: float);
begin
  fmaxmaxHP := value;
  If value < 0 then die;
end;
procedure DActor.resetHP;
begin
  fmaxHP := fmaxmaxHP;
  fHP := fmaxHP;
end;

{---------------------------------------------------------------------------}

Procedure DActor.setSTA(value: float);
begin
  If value < fmaxSTA then fSTA := value else fSTA := fmaxSTA;
  If value < 0 then {EXAUSTED STATE};
end;
Procedure DActor.setmaxSTA(value: float);
begin
  If value < fmaxmaxSTA then fmaxSTA := value else fmaxSTA := fmaxmaxSTA;
  If value < 0 then {EXAUSTED STATE};
end;
Procedure DActor.setmaxmaxSTA(value: float);
begin
  fmaxmaxSTA := value;
  If value < 0 then {EXAUSTED STATE};
end;
procedure DActor.resetSTA;
begin
  fmaxSTA := fmaxmaxSTA;
  fSTA := fmaxSTA;
end;

{-----------------------------------------------------------------------------}

Procedure DActor.setCONC(value: float);
begin
  If value < fmaxCONC then fCONC := value else fCONC := fmaxCONC;
  If value < 0 then {BURN-OUT STATE};
end;
Procedure DActor.setmaxCONC(value: float);
begin
  If value < fmaxmaxCONC then fmaxCONC := value else fmaxCONC := fmaxmaxCONC;
  If value < 0 then {BURN-OUT STATE};
end;
Procedure DActor.setmaxmaxCONC(value: float);
begin
  fmaxmaxCONC := value;
  If value < 0 then {BURN-OUT STATE};
end;
procedure DActor.resetCONC;
begin
  fmaxCONC := fmaxmaxCONC;
  fCONC := fmaxCONC;
end;

{---------------------------------------------------------------------------}

Procedure DActor.setMPH(value: float);
begin
  If value < fmaxMPH then fMPH := value else fMPH := fmaxMPH;
  If value < 0 then {* STATE};
end;
Procedure DActor.setmaxMPH(value: float);
begin
  If value < fmaxmaxMPH then fmaxMPH := value else fmaxMPH := fmaxmaxMPH;
  If value < 0 then {* STATE};
end;
Procedure DActor.setmaxmaxMPH(value: float);
begin
  fmaxmaxMPH := value;
  If value < 0 then {* STATE};
end;
procedure DActor.resetMPH;
begin
  fmaxMPH := fmaxmaxMPH;
  fMPH := fmaxMPH;
end;

{---------------------------------------------------------------------------}

Procedure DActor.hit(damage: float; skill: float);
begin
  setHP(HP-damage);
  setmaxHP(maxHP-damage*skill); // todo
  {SETTERS WILL DO EVERYTHING}
{  If fHP < 0 then die; //K.D. state for player character
  if fmaxHP < 0 then {clinical death}; //C.D. state}
end;

function DActor.heal(value: float; skill: float): boolean;
begin
  if (HP<maxHP) or ((maxHP<maxmaxHP) and (skill>0)) then begin
    setHP(HP+value);
    setMaxHP(MaxHP+value*skill); // todo
    result := true;
  end else
    result := false;
end;

{-----------------------------------------------------------------------------}

function DActor.consumeSTA(consumption: float; skill: float): boolean;
begin
  if (STA>consumption) then begin
    setSTA(STA-consumption);
    setmaxSTA(maxSTA-consumption*skill); // todo
    result := true;
  end else result := false;
end;
function DActor.restoreSTA(restoration: float; skill: float): boolean;
begin
  if (STA<maxSTA) or ((maxSTA<maxmaxSTA) and (skill>0)) then begin
    setSTA(STA+restoration);
    setMaxSTA(MaxSTA+restoration*skill); // todo
    result := true;
  end else
    result := false;
end;
procedure DActor.drainSTA(drain: float; skill: float);
begin
 setSTA(STA-drain);
 setmaxSTA(maxSTA-drain*skill); // todo
end;

{-----------------------------------------------------------------------------}

function DActor.consumeCONC(consumption: float; skill: float): boolean;
begin
  if (CONC>consumption) then begin
    setCONC(CONC-consumption);
    setmaxCONC(maxCONC-consumption*skill); // todo
    result := true;
  end else result := false;
end;
function DActor.restoreCONC(restoration: float; skill: float): boolean;
begin
  if (CONC<maxCONC) or ((maxCONC<maxmaxCONC) and (skill>0)) then begin
    setCONC(CONC+restoration);
    setMaxCONC(MaxCONC+restoration*skill); // todo
    result := true;
  end else
    result := false;
end;
procedure DActor.drainCONC(drain: float; skill: float);
begin
  setCONC(CONC-drain);
  setmaxCONC(maxCONC-drain*skill); // todo
end;

{-----------------------------------------------------------------------------}

function DActor.consumeMPH(consumption: float; skill: float): boolean;
begin
  if (MPH>consumption) then begin
    setMPH(MPH-consumption);
    setmaxMPH(maxMPH-consumption*skill); // todo
    result := true;
  end else result := false;
end;
function DActor.restoreMPH(restoration: float; skill: float): boolean;
begin
  if (MPH<maxMPH) or ((maxMPH<maxmaxMPH) and (skill>0)) then begin
    setMPH(MPH+restoration);
    setMaxMPH(MaxMPH+restoration*skill); // todo
    result := true;
  end else
    result := false;
end;
procedure DActor.drainMPH(drain: float; skill: float);
begin
  setMPH(MPH-drain);
  setmaxMPH(maxMPH-drain*skill); // todo
end;

{==========================================================================}
{================ Player Character ========================================}
{==========================================================================}

Procedure DPlayerCharacter.die;
begin
  WriteLnLog('DPlayerCharacter.die','Character has died');
end;

end.

