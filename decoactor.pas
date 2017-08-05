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

{ Describes characters and creatures basic behaviour }
unit DecoActor;

{$INCLUDE compilerconfig.inc}

interface

uses Classes, CastleRandom, fgl, CastleVectors,
  CastleResources, CastleCreatures,
  DecoStats,
  DecoGlobal;

type TDamageType = (dtHealth);
type TDamageProcedure = procedure (Dam: float; Damtype: TDamageType) of Object;

type DBody = TCreatureResource;

type
  { Actor with a rendered body and corresponding management routines }
  DActorBody = class (TObject)
  protected
    procedure doAI; virtual;
  public
    { 3D body of this Actor, it may be nil (in case the body is unloaded from
      RAM or belongs to an body-less entity, like Player's character at the moment }
    Body: TCreature;

    {Spawns a body for the Actor, overriden in children to spawn attributes}
    procedure Spawn(Pos: TVector3single; SpawnBody: DBody); virtual;

    { manages this actor, e.g. preforms AI }
    procedure Manage; virtual;

    destructor Destroy; override;
    constructor Create; virtual;
  end;

type TActorList = specialize TFPGObjectList<DActorBody>;

Type
  {basic actor. With stats.}
  DActor = class(DActorBody)
  private
    fHP,fMaxHP,fMaxMaxHP: float;
    { maybe, move all non-HP to deco player character? }
    fSTA,fMaxSTA,fMaxMaxSTA: float;
    fCNC,fMaxCNC,fMaxMaxCNC: float;
    fMPH,fMaxMPH,fMaxMaxMPH: float;
    Procedure SetHP(Value: float);
    Procedure SetMaxHP(Value: float);
    Procedure SetMaxMaxHP(Value: float);
    Procedure SetSTA(Value: float);
    Procedure SetMaxSTA(Value: float);
    Procedure SetMaxMaxSTA(Value: float);
    Procedure SetCNC(Value: float);
    Procedure SetMaxCNC(Value: float);
    Procedure SetMaxMaxCNC(Value: float);
    Procedure SetMPH(Value: float);
    Procedure SetMaxMPH(Value: float);
    Procedure SetMaxMaxMPH(Value: float);
  public
    { getters and setters }
    Property HP: float read fHP write SetHP;
    Property MaxHP: float read fMaxHP write SetMaxHP;
    Property MaxMaxHP: float read fMaxMaxHP write SetMaxMaxHP;
    procedure ResetHP;
    Property STA: float read fSTA write SetSTA;
    Property MaxSTA: float read fMaxSTA write SetMaxSTA;
    Property MaxMaxSTA: float read fMaxMaxSTA write SetMaxMaxSTA;
    procedure ResetSTA;
    Property CNC: float read fCNC write SetCNC;
    Property MaxCNC: float read fMaxCNC write SetMaxCNC;
    Property MaxMaxCNC: float read fMaxMaxCNC write SetMaxMaxCNC;
    procedure ResetCNC;
    Property MPH: float read fMPH write SetMPH;
    Property MaxMPH: float read fMaxMPH write SetMaxMPH;
    Property MaxMaxMPH: float read fMaxMaxMPH write SetMaxMaxMPH;
    procedure ResetMPH;

    {hit equals to consume+drain}
    procedure Hit(Damage: float; Skill: float); //=consumeHP
    {returns true if healed or false if nothing to heal}
    function Heal(Value: float; Skill: float): boolean; //=restoreHP
    Procedure Die; virtual; abstract;

    {"consumption" procedures return true if success and false if failed,
     "restoration" procedures return true if something has been restored,
     "drain" procedures can drain values below zero}
    function ConsumeSTA(Consumption: float; Skill: float): boolean;
    function RestoreSTA(Restoration: float; Skill: float): boolean;
    procedure DrainSTA(Drain: float;        Skill: float);
    function ConsumeCNC(Consumption: float; Skill: float): boolean;
    function RestoreCNC(Restoration: float; Skill: float): boolean;
    procedure DrainCNC(Drain: float;        Skill: float);
    function ConsumeMPH(Consumption: float; Skill: float): boolean;
    function RestoreMPH(Restoration: float; Skill: float): boolean;
    procedure DrainMPH(Drain: float;        Skill: float);
  public
    { events }
    onHit: TDamageProcedure;

  public
    { Stats of this Actor, they must be initialized afterwards, when it
      is clear, whether SetFullStats is true (PC and RPC) or false (NPC and monsters) }
    Stats, MaxStats: DStats;

  public
    {three-letter nickname for short display}
    Nickname: string;
    {these are randoms for the actor: defense gives his defense rolls,
     Attack provides for attack rolls, and JustRandom determines actor's
     behaviour and any other not too important random rolls}
    DefenseRandom,AttackRandom,JustRandom: TCastleRandom;
  public
    destructor Destroy; override;
    constructor Create; override;
end;

type
  { this is a monster Actor, featuring AI depending on its type,
    NPCs will have a different AI (?) because they can "switch" to moster/agressive AI }
  DMonster = class(DActor)
  protected
    procedure doAI; override;
  public
    //procedure Manage; override;
  end;



var tmpKnightCreature: DBody;

procedure tmpLoadKnightCreature;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses SysUtils, CastleLog,
  CastleFilesUtils, DecoInputOutput,

  DecoNavigation{?}, CastleScene, CastleSceneCore;

constructor DActor.Create;
begin
  inherited;
  Nickname := 'abc';
  SetMaxMaxHP(100);
  SetMaxMaxSTA(100);
  SetMaxMaxCNC(100);
  SetMaxMaxMPH(100);
  DefenseRandom := TCastleRandom.Create; {$HINT read seed from the savegame}
  AttackRandom := TCastleRAndom.Create;
  JustRandom := TCastleRandom.Create;
  ResetHP;
  ResetSTA;
  ResetCNC;
  ResetMPH;
end;

destructor DActor.Destroy;
begin
  FreeAndNil(DefenseRandom);
  FreeAndNil(AttackRandom);
  FreeAndNil(JustRandom);
  inherited;
end;

{----------------------------------------------------------------------------}

Procedure DActor.SetHP(Value: float);
begin
  If Value < fMaxHP then fHP := Value else fHP := fMaxHP;
  If Value < 0 then Die;     {$HINT clinical death state}
end;
Procedure DActor.SetMaxHP(Value: float);
begin
  If Value < fMaxMaxHP then fMaxHP := Value else fMaxHP := fMaxMaxHP;
  If Value < 0 then Die;
end;
Procedure DActor.SetMaxMaxHP(Value: float);
begin
  if MaxMaxHP < Value then Heal(Value-fMaxMaxHP,1);
  fMaxMaxHP := Value;
  If Value < 0 then Die;
end;
procedure DActor.ResetHP;
begin
  fMaxHP := fMaxMaxHP;
  fHP := fMaxHP;
end;

{---------------------------------------------------------------------------}

Procedure DActor.SetSTA(Value: float);
begin
  If Value < fMaxSTA then fSTA := Value else fSTA := fMaxSTA;
  If Value < 0 then {EXAUSTED STATE};
end;
Procedure DActor.SetMaxSTA(Value: float);
begin
  If Value < fMaxMaxSTA then fMaxSTA := Value else fMaxSTA := fMaxMaxSTA;
  If Value < 0 then {EXAUSTED STATE};
end;
Procedure DActor.SetMaxMaxSTA(Value: float);
begin
  if MaxMaxSTA < Value then RestoreSTA(Value-fMaxMaxSTA,1);
  fMaxMaxSTA := Value;
  If Value < 0 then {EXAUSTED STATE};
end;
procedure DActor.ResetSTA;
begin
  fMaxSTA := fMaxMaxSTA;
  fSTA := fMaxSTA;
end;

{-----------------------------------------------------------------------------}

Procedure DActor.SetCNC(Value: float);
begin
  If Value < fMaxCNC then fCNC := Value else fCNC := fMaxCNC;
  If Value < 0 then {BURN-OUT STATE};
end;
Procedure DActor.SetMaxCNC(Value: float);
begin
  If Value < fMaxMaxCNC then fMaxCNC := Value else fMaxCNC := fMaxMaxCNC;
  If Value < 0 then {BURN-OUT STATE};
end;
Procedure DActor.SetMaxMaxCNC(Value: float);
begin
  if MaxMaxCNC < Value then RestoreCNC(Value-fMaxMaxCNC,1);
  fMaxMaxCNC := Value;
  If Value < 0 then {BURN-OUT STATE};
end;
procedure DActor.ResetCNC;
begin
  fMaxCNC := fMaxMaxCNC;
  fCNC := fMaxCNC;
end;

{---------------------------------------------------------------------------}

Procedure DActor.SetMPH(Value: float);
begin
  If Value < fMaxMPH then fMPH := Value else fMPH := fmaxMPH;
  If Value < 0 then {* STATE};
end;
Procedure DActor.SetMaxMPH(Value: float);
begin
  If Value < fMaxmaxMPH then fMaxMPH := Value else fMaxMPH := fMaxmaxMPH;
  If Value < 0 then {* STATE};
end;
Procedure DActor.SetMaxMaxMPH(Value: float);
begin
  if MaxMaxMPH < Value then RestoreMPH(Value-fMaxMaxMPH,1);
  fMaxMaxMPH := Value;
  If Value < 0 then {* STATE};
end;
procedure DActor.ResetMPH;
begin
  fMaxMPH := fMaxMaxMPH;
  fMPH := fMaxMPH;
end;

{---------------------------------------------------------------------------}

Procedure DActor.Hit(Damage: float; Skill: float);
begin
  SetHP(HP-Damage);
  SetmaxHP(MaxHP-Damage*Skill); // todo
  if Assigned(Self.onHit) then Self.onHit(Damage, dtHealth);
end;

function DActor.Heal(Value: float; Skill: float): boolean;
begin
  if (HP < MaxHP) or ((MaxHP < MaxMaxHP) and (Skill > 0)) then begin
    SetHP(HP+Value);
    SetMaxHP(MaxHP+Value*Skill); // todo
    Result := true;
  end else
    Result := false;
end;

{-----------------------------------------------------------------------------}

function DActor.ConsumeSTA(Consumption: float; Skill: float): boolean;
begin
  if (STA > Consumption) then begin
    SetSTA(STA-Consumption);
    SetMaxSTA(MaxSTA-Consumption*Skill); // todo
    Result := true;
  end else Result := false;
end;
function DActor.RestoreSTA(Restoration: float; Skill: float): boolean;
begin
  if (STA < MaxSTA) or ((MaxSTA < MaxMaxSTA) and (Skill > 0)) then begin
    SetSTA(STA+Restoration);
    SetMaxSTA(MaxSTA+Restoration*Skill); // todo
    Result := true;
  end else
    Result := false;
end;
procedure DActor.DrainSTA(Drain: float; Skill: float);
begin
 SetSTA(STA-Drain);
 SetMaxSTA(MaxSTA-Drain*Skill); // todo
end;

{-----------------------------------------------------------------------------}

function DActor.ConsumeCNC(Consumption: float; Skill: float): boolean;
begin
  if (CNC > Consumption) then begin
    SetCNC(CNC-Consumption);
    SetMaxCNC(MaxCNC-Consumption*Skill); // todo
    Result := true;
  end else Result := false;
end;
function DActor.RestoreCNC(Restoration: float; Skill: float): boolean;
begin
  if (CNC < MaxCNC) or ((MaxCNC < MaxMaxCNC) and (Skill > 0)) then begin
    SetCNC(CNC+Restoration);
    SetMaxCNC(MaxCNC+Restoration*Skill); // todo
    Result := true;
  end else
    Result := false;
end;
procedure DActor.DrainCNC(Drain: float; Skill: float);
begin
  SetCNC(CNC-Drain);
  SetMaxCNC(MaxCNC-Drain*Skill); // todo
end;

{-----------------------------------------------------------------------------}

function DActor.ConsumeMPH(Consumption: float; Skill: float): boolean;
begin
  if (MPH > Consumption) then begin
    SetMPH(MPH-Consumption);
    SetMaxMPH(MaxMPH-Consumption*Skill); // todo
    Result := true;
  end else Result := false;
end;
function DActor.RestoreMPH(Restoration: float; Skill: float): boolean;
begin
  if (MPH < MaxMPH) or ((MaxMPH < MaxMaxMPH) and (Skill > 0)) then begin
    SetMPH(MPH+Restoration);
    SetMaxMPH(MaxMPH+Restoration*Skill); // todo
    Result := true;
  end else
    Result := false;
end;
procedure DActor.DrainMPH(Drain: float; Skill: float);
begin
  SetMPH(MPH-Drain);
  SetMaxMPH(MaxMPH-Drain*Skill); // todo
end;

{========================== ACTOR BODY =====================================}

constructor DActorBody.Create;
begin

end;

{-----------------------------------------------------------------------------}

destructor DActorBody.Destroy;
begin
  inherited;
end;

{-----------------------------------------------------------------------------}

procedure DActorBody.Spawn(Pos: TVector3; SpawnBody: DBody);
begin
  body := SpawnBody.CreateCreature(Window.SceneManager.Items, Pos, Vector3(1,0,0));
  body.Up := Vector3(0,0,1);
  body.Exists := true;
end;

{-----------------------------------------------------------------------------}

procedure DActorBody.doAI;
begin
  //does nothing yet, maybe should be abstract?
end;

{-----------------------------------------------------------------------------}

procedure DActorBody.Manage;
begin
  //cute and simple, maybe merge them?
  doAI;
end;

{-----------------------------------------------------------------------------}

procedure DMonster.doAI;
begin
  if (Camera.Position - body.Position).Length < 10 then
  body.Direction := Camera.Position - body.Position;
  body.Up := Vector3(0,0,1); {$Warning this is a bug!}
  //(body.Items[0] as TCastleScene).PlayAnimation('attack', paForceNotLooping);
  //Scene.AnimationTimeSensor('my_animation').EventIsActive.OnReceive.Add(@AnimationIsActiveChanged)
end;

{-----------------------------------------------------------------------------}

procedure tmpLoadKnightCreature;
begin
  Resources.LoadSafe(ApplicationData('models/creatures/knight_creature/'));
  tmpKnightCreature := Resources.FindName('Knight') as TCreatureResource;
end;


end.


