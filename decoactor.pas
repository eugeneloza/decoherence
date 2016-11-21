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

{$INCLUDE compilerconfig.inc}

interface

uses classes,
  decoglobal;

{Maybe, add some basic actor (visible only, no collisions)}
Type
  {basic actor. With hit-points and coordinates}
  DActor = class(TComponent)
  private
    fHP,fMAXHP,fMAXMAXHP: float;
    { maybe, move all non-HP to deco player character? }
    fSTA,fMAXSTA,fMAXMAXSTA: float;
    fCNC,fMAXCNC,fMAXMAXCNC: float;
    fMPH,fMAXMPH,fMAXMAXMPH: float;
    Procedure setHP(value: float);
    Procedure setmaxHP(value: float);
    Procedure setmaxmaxHP(value: float);
    Procedure setSTA(value: float);
    Procedure setmaxSTA(value: float);
    Procedure setmaxmaxSTA(value: float);
    Procedure setCNC(value: float);
    Procedure setmaxCNC(value: float);
    Procedure setmaxmaxCNC(value: float);
    Procedure setMPH(value: float);
    Procedure setmaxMPH(value: float);
    Procedure setmaxmaxMPH(value: float);
  public
    constructor create(AOwner: TComponent); override;
    { getters and setters }
    Property HP: float read fHP write sethp;
    Property maxHP: float read fMAXHP write setmaxhp;
    Property maxmaxHP: float read fMAXMAXHP write setmaxmaxhp;
    procedure resetHP;
    Property STA: float read fSTA write setSTA;
    Property maxSTA: float read fMAXSTA write setmaxSTA;
    Property maxmaxSTA: float read fMAXMAXSTA write setmaxmaxSTA;
    procedure resetSTA;
    Property CNC: float read fCNC write setCNC;
    Property maxCNC: float read fMAXCNC write setmaxCNC;
    Property maxmaxCNC: float read fMAXMAXCNC write setmaxmaxCNC;
    procedure resetCNC;
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
    function consumeCNC(consumption: float; skill: float): boolean;
    function restoreCNC(restoration: float; skill: float): boolean;
    procedure drainCNC(drain: float; skill: float);
    function consumeMPH(consumption: float; skill: float): boolean;
    function restoreMPH(restoration: float; skill: float): boolean;
    procedure drainMPH(drain: float; skill: float);
end;



{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses castleLog;

constructor DActor.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  setmaxmaxHP(100);
  setmaxmaxSTA(100);
  setmaxmaxCNC(100);
  setmaxmaxMPH(100);
  resetHP;
  resetSTA;
  resetCNC;
  resetMPH;
end;

{----------------------------------------------------------------------------}

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
  if maxmaxHP < value then heal(value-fmaxmaxHP,1);
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
  if maxmaxSTA < value then restoreSTA(value-fmaxmaxSTA,1);
  fmaxmaxSTA := value;
  If value < 0 then {EXAUSTED STATE};
end;
procedure DActor.resetSTA;
begin
  fmaxSTA := fmaxmaxSTA;
  fSTA := fmaxSTA;
end;

{-----------------------------------------------------------------------------}

Procedure DActor.setCNC(value: float);
begin
  If value < fmaxCNC then fCNC := value else fCNC := fmaxCNC;
  If value < 0 then {BURN-OUT STATE};
end;
Procedure DActor.setmaxCNC(value: float);
begin
  If value < fmaxmaxCNC then fmaxCNC := value else fmaxCNC := fmaxmaxCNC;
  If value < 0 then {BURN-OUT STATE};
end;
Procedure DActor.setmaxmaxCNC(value: float);
begin
  if maxmaxCNC < value then restoreCNC(value-fmaxmaxCNC,1);
  fmaxmaxCNC := value;
  If value < 0 then {BURN-OUT STATE};
end;
procedure DActor.resetCNC;
begin
  fmaxCNC := fmaxmaxCNC;
  fCNC := fmaxCNC;
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
  if maxmaxMPH < value then restoreMPH(value-fmaxmaxMPH,1);
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
  if (HP < maxHP) or ((maxHP < maxmaxHP) and (skill > 0)) then begin
    setHP(HP+value);
    setMaxHP(MaxHP+value*skill); // todo
    result := true;
  end else
    result := false;
end;

{-----------------------------------------------------------------------------}

function DActor.consumeSTA(consumption: float; skill: float): boolean;
begin
  if (STA > consumption) then begin
    setSTA(STA-consumption);
    setmaxSTA(maxSTA-consumption*skill); // todo
    result := true;
  end else result := false;
end;
function DActor.restoreSTA(restoration: float; skill: float): boolean;
begin
  if (STA < maxSTA) or ((maxSTA < maxmaxSTA) and (skill > 0)) then begin
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

function DActor.consumeCNC(consumption: float; skill: float): boolean;
begin
  if (CNC > consumption) then begin
    setCNC(CNC-consumption);
    setmaxCNC(maxCNC-consumption*skill); // todo
    result := true;
  end else result := false;
end;
function DActor.restoreCNC(restoration: float; skill: float): boolean;
begin
  if (CNC < maxCNC) or ((maxCNC < maxmaxCNC) and (skill > 0)) then begin
    setCNC(CNC+restoration);
    setMaxCNC(MaxCNC+restoration*skill); // todo
    result := true;
  end else
    result := false;
end;
procedure DActor.drainCNC(drain: float; skill: float);
begin
  setCNC(CNC-drain);
  setmaxCNC(maxCNC-drain*skill); // todo
end;

{-----------------------------------------------------------------------------}

function DActor.consumeMPH(consumption: float; skill: float): boolean;
begin
  if (MPH > consumption) then begin
    setMPH(MPH-consumption);
    setmaxMPH(maxMPH-consumption*skill); // todo
    result := true;
  end else result := false;
end;
function DActor.restoreMPH(restoration: float; skill: float): boolean;
begin
  if (MPH < maxMPH) or ((maxMPH < maxmaxMPH) and (skill > 0)) then begin
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


end.

