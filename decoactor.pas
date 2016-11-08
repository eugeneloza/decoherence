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
    Procedure sethp(value: float);
    Procedure setmaxhp(value: float);
    Procedure setmaxmaxhp(value: float);
  public
    Property hp: float read fHP write sethp;
    Property maxhp: float read fMAXHP write setmaxhp;
    Property maxmaxhp: float read fMAXMAXHP write setmaxmaxhp;
    procedure resetHP;
    procedure hit(damage: float);
    procedure heal(value: float);
    Procedure die; virtual; abstract;
end;

Type
  {player character - the most complex actor available :)}
  DPlayerCharacter = class(DActor)
  public
    Procedure die; override;
end;

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

Procedure DActor.sethp(value: float);
begin
  If value < fmaxhp then fhp := value else fhp := fmaxhp;
  If value < 0 then die;
end;

{-----------------------------------------------------------------------------}

Procedure DActor.setmaxhp(value: float);
begin
  If value < fmaxmaxhp then fmaxhp := value else fmaxhp := fmaxmaxhp;
  If value < 0 then die;
end;

{-----------------------------------------------------------------------------}

Procedure DActor.setmaxmaxhp(value: float);
begin
  fmaxmaxhp := value;
  If value < 0 then die;
end;

{-----------------------------------------------------------------------------}

procedure DActor.resetHP;
begin
  fmaxHP := fmaxmaxHP;
  fHP := fmaxHp;
end;

{-----------------------------------------------------------------------------}

Procedure DActor.hit(damage: float);
begin
  fHp -= damage;
  fmaxhp -= damage*0.5; // todo
  If fhp < 0 then die;
end;

{-----------------------------------------------------------------------------}

Procedure DActor.heal(value: float);
begin
  fHp += value;
  If fhp > fmaxhp then fhp := fmaxhp;
  fMaxhp += value*0.1; // todo
  If fmaxhp > fmaxmaxhp then fmaxhp := fmaxmaxhp;
end;

{==========================================================================}
{================ Player Character ========================================}
{==========================================================================}

Procedure DPlayerCharacter.die;
begin
  WriteLnLog('DPlayerCharacter.die','Character has died');
end;

end.

