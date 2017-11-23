{ Copyright (C) 2012-2017 Yevhen Loza

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

{ --------------------------------------------------------------------------- }

{ Describes characters and creatures basic behaviour }
unit DecoActor;

{$INCLUDE compilerconfig.inc}

interface

uses Classes, CastleRandom, Generics.Collections, CastleVectors, CastleCameras,
  CastleResources, CastleCreatures,
  DecoNavigationNetwork,
  DecoStats, DecoPerks, DecoActorBody,
  DecoGlobal, DecoTime;

type
  TDamageType = (dtHealth);

type
  TDamageProcedure = procedure(const Dam: Float; const Damtype: TDamageType)
    of Object;

type
  TFaction = (fPlayer, fHostile);

type
  { This Actor has only the most basic features like his "tile" position
    Will be used in some remote future for Actors behaviour on global map }
  DSimpleActor = class abstract(DObject)
  private
    { Last Nav, where the Actor was standing }
    LastNav: TNavID;
  public
    { if this entity is a player character? }
    isPlayer: Boolean;
    { Faction this Actor belongs to }
    Faction: TFaction;
    { Teleport this Actor to aNav }
    procedure TeleportTo(const aNav: TNavID); virtual;
    { Procedures preformed on this actor every frame }
    procedure Manage; virtual; // it'll do something useful some day...
    constructor Create; virtual; // override;
  end;

  { List of DSimpleActors }
type
  TActorList = specialize TObjectList<DSimpleActor>;

type
  { A group of actors, that can manage it's Members }
  DActorGroup = class(DObject) // maybe class(DSimpleActor);
  public
    { a list of actors in this ActorGroup }
    Members: TActorList;
    constructor Create; // override;
    destructor Destroy; override;
  end;

type
  { Actor with full World coordinates,
    Mostly needed as a Target for AI
    and Base for Player camera mangement }
  DCoordActor = class(DSimpleActor)
  private
    procedure FixZeroDirection; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}
  public
    { Position, direction and rotate_to_direction of the Body }
    Position, Direction: TVector3;
    { Instantly move Actor to aPosition or aNav }
    procedure TeleportTo(const aPosition: TVector3);
    procedure TeleportTo(const aPosition, aDirection: TVector3);
    procedure TeleportTo(const aNav: TNavID); override;
  protected
    { What position/direction this Actor moves to }
    toPos, toDir: TVector3;
    { Initializes direction with a random value }
    procedure GetRandomDirection;
    { rotates the body }
    procedure doRotate;
    { moves the body }
    procedure doMove;
  public
    constructor Create; override;
  end;

type
  { Actor with basic physics, mostly gravity and acceleration.
    maybe, merge these with DCoordActor? but it'll come later. }
  DActorPhysics = class abstract(DCoordActor)
  private
    { premultiplied by time gravity facing *down* }
    CurrentGravityDown: TVector3;
  protected
    { DActorPhysics does not set the camera, but uses it! It's defined in descendants }
    InternalCamera: TWalkCamera;
    { Is movement to this direction or position is allowed? }
    function CanMovePos(const aPos: TVector3): Boolean;
{$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}
    function CanMoveDir(const aDir: TVector3): Boolean;
{$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}
  public
    { Height of this actor, usually should be zero,
      Mostly needed for player's camera height }
    Height: Float;
    { Determine gravity influence on this Actor }
    procedure doGravity;
    procedure Manage; override;
    constructor Create; override;
  end;

type
  { Actor with a rendered body and corresponding management routines }
  DActorBody = class abstract(DActorPhysics)
  protected
    { AI of this actor, determines orders for actors and choice of targets }
    procedure doAI; virtual; abstract;
  private
    { shows or hides body of this actor }
    procedure SetVisible(const Value: Boolean);
    { reads visibility of this Actor's body }
    function GetVisible: Boolean;
    { creates a body }
    procedure SpawnBodyHere(const SpawnBody: DBodyResource);
  public
    { 3D body of this Actor, it may be nil (in case the body is unloaded from
      RAM or belongs to an body-less entity, like Player's character at the moment }
    Body: DBody;
    { This is a type of the Actor's body
      and the resource, containing 3D model animations and etc. }
    BodyResource: DBodyResource;
    { Shows or hides the actor's body }
    property Visible: Boolean read GetVisible write SetVisible;
    { Spawns a body for the Actor, overriden in children to spawn attributes }
    procedure Spawn(const aNav: TNavID; const SpawnBody: DBodyResource);
    procedure Spawn(const aPosition: TVector3;
      const SpawnBody: DBodyResource); virtual;
    { Manages this actor, e.g. preforms AI,
      called each frame }
    procedure Manage; override;
    { Forces immediate change of the animation }
    procedure ForceAnimation(const at: TAnimationType);
    procedure ForceAnimation(const at: string);
    { Softly changes the animation (when the current animation cycle ends) }
    procedure Animation(const at: TAnimationType);
    procedure Animation(const at: string);

    destructor Destroy; override;
    constructor Create; override;
  end;

type
  DStatValue = record
    Current, Max, MaxMax: Float;
  end;

  PStatValue = ^DStatValue;

Type
  { basic actor. With stats }
  DBasicActor = class abstract(DActorBody)
  private
    fHP: DStatValue;
    { maybe, move all non-HP to deco player character? }
    fSTA: DStatValue;
    fCNC: DStatValue;
    fMPH: DStatValue;
    procedure SetHP(const Value: Float);
    procedure SetMaxHP(const Value: Float);
    procedure SetMaxMaxHP(const Value: Float);
    procedure SetSTA(const Value: Float);
    procedure SetMaxSTA(const Value: Float);
    procedure SetMaxMaxSTA(const Value: Float);
    procedure SetCNC(const Value: Float);
    procedure SetMaxCNC(const Value: Float);
    procedure SetMaxMaxCNC(const Value: Float);
    procedure SetMPH(const Value: Float);
    procedure SetMaxMPH(const Value: Float);
    procedure SetMaxMaxMPH(const Value: Float);
  public
    { getters and setters }
    property HP: Float read fHP.Current write SetHP;
    property MaxHP: Float read fHP.Max write SetMaxHP;
    property MaxMaxHP: Float read fHP.MaxMax write SetMaxMaxHP;
    procedure ResetHP;
    property STA: Float read fSTA.Current write SetSTA;
    property MaxSTA: Float read fSTA.Max write SetMaxSTA;
    property MaxMaxSTA: Float read fSTA.MaxMax write SetMaxMaxSTA;
    procedure ResetSTA;
    property CNC: Float read fCNC.Current write SetCNC;
    property MaxCNC: Float read fCNC.Max write SetMaxCNC;
    property MaxMaxCNC: Float read fCNC.MaxMax write SetMaxMaxCNC;
    procedure ResetCNC;
    property MPH: Float read fMPH.Current write SetMPH;
    property MaxMPH: Float read fMPH.Max write SetMaxMPH;
    property MaxMaxMPH: Float read fMPH.MaxMax write SetMaxMaxMPH;
    procedure ResetMPH;
    procedure ResetAll;

    function HPRef: PStatValue;
    function STARef: PStatValue;
    function CNCRef: PStatValue;
    function MPHRef: PStatValue;

    { Hit equals to consume+drain }
    procedure Hit(const Damage: Float; const Skill: Float); // =consumeHP
    { Returns true if healed or false if nothing to heal }
    function Heal(const Value: Float; const Skill: Float): Boolean;
    // =restoreHP

    { Abstract action preformed on Actor's death }
    procedure Die; virtual; abstract;

    { "consumption" procedures return true if success and false if failed,
      "restoration" procedures return true if something has been restored,
      "drain" procedures can drain Values below zero }
    function ConsumeSTA(const Consumption: Float; const Skill: Float): Boolean;
    function RestoreSTA(const Restoration: Float; const Skill: Float): Boolean;
    procedure DrainSTA(const Drain: Float; const Skill: Float);
    function ConsumeCNC(const Consumption: Float; const Skill: Float): Boolean;
    function RestoreCNC(const Restoration: Float; const Skill: Float): Boolean;
    procedure DrainCNC(const Drain: Float; const Skill: Float);
    function ConsumeMPH(const Consumption: Float; const Skill: Float): Boolean;
    function RestoreMPH(const Restoration: Float; const Skill: Float): Boolean;
    procedure DrainMPH(const Drain: Float; const Skill: Float);
  public
    { events }
    onHit: TDamageProcedure;

  public
    { Stats of this Actor, they must be initialized afterwards, when it
      is clear, whether SetFullStats is true (PC and RPC) or false (NPC and monsters) }
    Stats, MaxStats: DStats;

  public
    { Three-letter nickname for short display }
    Nickname: string;
    { These are randoms for the actor: defense gives his defense rolls,
      Attack provides for attack rolls
      All non-important random rolls are taken by global DRND }
    DefenseRandom, AttackRandom: TCastleRandom;
  public
    destructor Destroy; override;
    constructor Create; override;
  end;

type
  { Actor with actions and target }
  DActor = class(DBasicActor)
  const
    { temporary }
    CombatRange = 10;
  private
    fTarget, fTargetGroup: DCoordActor;
    function GetTarget: DCoordActor;
    procedure GetEnemyTarget;
    procedure RequestSoftPause;
  protected
    { Look at fTarget }
    procedure LookAt;
    { Look at aPosition }
    procedure LookAt(const aPosition: TVector3);
    { Can this Actor see another Actor? }
    function CanSee(const a1: DCoordActor): Boolean;
    { temp }
    procedure PerformAction(const doAction: DMultiPerk);
  public
    { List of actions for this Actor }
    Actions: DPerksList;
    { Used for AI and preforming actions
      if target is nil any valid target is selected }
    property Target: DCoordActor read GetTarget write fTarget;
    { Rests all stats and removes all effects }
    procedure RecoverAll;
    constructor Create; override;
    destructor Destroy; override;
  end;

type
  { this is a monster Actor, featuring AI depending on its type,
    NPCs will have a different AI (?) because they can "switch" to moster/agressive AI }
  DMonster = class(DActor)
  protected
    procedure doAI; override;
    { Call-back to react to external damage }
    procedure doHit(const Dam: Float; const Damtype: TDamageType);
  public
    procedure Die; override;
    // procedure Manage; override;
    constructor Create; override;
  end;

  { ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
implementation

uses SysUtils,
  CastleFilesUtils,

  DecoAbstractWorld, DecoAbstractWorld3D,
  CastleScene,
  DecoGameMode, DecoLog;

{ =========================================================================== }
{ ====================== SIMPLE ACTOR ======================================= }
{ =========================================================================== }

procedure DSimpleActor.TeleportTo(const aNav: TNavID);
begin
  if LastNav <> UnitinializedNav then
    CurrentWorld.ReleaseNav(LastNav);
  CurrentWorld.BlockNav(aNav);
  LastNav := aNav;
end;

{ ----------------------------------------------------------------------------- }

procedure DSimpleActor.Manage;
begin
{$HINT todo}
end;

{ ----------------------------------------------------------------------------- }

constructor DSimpleActor.Create;
begin
  // inherited; <------ nothing to inherit
  isPlayer := False;
  // nothing to create yet
end;

{ =========================================================================== }
{ ====================== COORD ACTOR ======================================== }
{ =========================================================================== }

procedure DCoordActor.GetRandomDirection;
var
  rDir: Float;
begin
  rDir := drnd.Random * 2 * Pi;
  Direction := Vector3(sin(rDir), cos(rDir), 0);
  toDir := Direction;
end;

{ ----------------------------------------------------------------------------- }

procedure DCoordActor.doRotate;
begin
{$WARNING dummy}
  if not TVector3.Equals(Direction, toDir) then
  begin
    Direction := toDir;
  end;
end;

{ ----------------------------------------------------------------------------- }

procedure DCoordActor.doMove;
begin
{$WARNING dummy}
  if TVector3.Equals(Direction, toDir) then
  begin
    Position := toPos;
  end;
end;

{ ----------------------------------------------------------------------------- }

procedure DCoordActor.TeleportTo(const aPosition: TVector3);
begin
  GetRandomDirection;
  Position := aPosition;
  toPos := Position;
end;

procedure DCoordActor.TeleportTo(const aPosition, aDirection: TVector3);
begin
  Direction := aDirection;
  toDir := Direction;
  Position := aPosition;
  toPos := Position;
end;

procedure DCoordActor.TeleportTo(const aNav: TNavID);
begin
  inherited TeleportTo(aNav);
  TeleportTo(CurrentWorld.NavToVector3(aNav));
end;

{ ----------------------------------------------------------------------------- }

procedure DCoordActor.FixZeroDirection; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}
begin
  { this is a critical, very rare and nearly unfixable error, so making just
    some precautions to forget about it forever. }
  if toDir.IsZero then
  begin
    if not Direction.IsZero then
      toDir := Direction
    else
      toDir := Vector3(0, 1, 0);
    Log(LogActorError, {$I %CURRENTROUTINE%}, 'ERROR: Direction is zero!');
  end;
end;

{ ----------------------------------------------------------------------------- }

constructor DCoordActor.Create;
begin
  inherited Create;
  LastNav := UnitinializedNav;
end;

{ =========================================================================== }
{ ====================== COORD ACTOR ======================================== }
{ =========================================================================== }

procedure DActorPhysics.doGravity;
begin
  if InternalCamera = nil then
    Exit;
  if CanMoveDir(CurrentGravityDown - Vector3(0, 0, Height)) then
    Position := Position + CurrentGravityDown;
end;

{ ---------------------------------------------------------------------------- }

constructor DActorPhysics.Create;
begin
  inherited Create;
  Height := 0;
  CurrentGravityDown := Vector3(0, 0, 1);
end;

{ ---------------------------------------------------------------------------- }

procedure DActorPhysics.Manage;
begin
  CurrentGravityDown := -DeltaTLocal * CurrentWorld.GetGravity(Position) *
    CurrentWorld.GravityAcceleration;
  inherited Manage;
  doGravity;
end;

{ ---------------------------------------------------------------------------- }

function DActorPhysics.CanMovePos(const aPos: TVector3): Boolean;
{$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}
var
  tmp: TVector3;
begin
  // InternalCamera may be nil, but we skip the check for speed. Be careful.
  Result := InternalCamera.DoMoveAllowed(aPos, tmp, False)
end;

function DActorPhysics.CanMoveDir(const aDir: TVector3): Boolean;
{$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}
var
  tmp: TVector3;
begin
  // InternalCamera may be nil, but we skip the check for speed. Be careful.
  Result := InternalCamera.DoMoveAllowed(InternalCamera.Position + aDir,
    tmp, False)
end;

{ =========================================================================== }
{ ========================== BASIC ACTOR ==================================== }
{ =========================================================================== }

constructor DBasicActor.Create;
begin
  inherited Create;
  Nickname := 'abc';
  // setting some Values to avoid uncertainity
  SetMaxMaxHP(100);
  SetMaxMaxSTA(100);
  SetMaxMaxCNC(100);
  SetMaxMaxMPH(100);
  DefenseRandom := TCastleRandom.Create; {$HINT read seed from the savegame}
  AttackRandom := TCastleRandom.Create;
  ResetAll;
end;

{ ---------------------------------------------------------------------------- }

destructor DBasicActor.Destroy;
begin
  FreeAndNil(DefenseRandom);
  FreeAndNil(AttackRandom);
  inherited Destroy;
end;

{ ---------------------------------------------------------------------------- }

Procedure DBasicActor.SetHP(const Value: Float);
begin
  If Value < fHP.Max then
    fHP.Current := Value
  else
    fHP.Current := fHP.Max;
  If fHP.Current < 0 then
    Die;
end;

Procedure DBasicActor.SetMaxHP(const Value: Float);
begin
  If Value < fHP.MaxMax then
    fHP.Max := Value
  else
    fHP.Max := fHP.MaxMax;
  If fHP.Max < 0 then
    Die;
end;

Procedure DBasicActor.SetMaxMaxHP(const Value: Float);
begin
  if fHP.MaxMax < Value then
    Heal(Value - fHP.MaxMax, 1);
  fHP.MaxMax := Value;
  If fHP.MaxMax < 0 then
    Die;
end;

procedure DBasicActor.ResetHP;
begin
  fHP.Max := fHP.MaxMax;
  fHP.Current := fHP.Max;
end;

{ --------------------------------------------------------------------------- }

Procedure DBasicActor.SetSTA(const Value: Float);
begin
  If Value < fSTA.Max then
    fSTA.Current := Value
  else
    fSTA.Current := fSTA.Max;
  If fSTA.Current < 0 then { EXAUSTED STATE };
end;

Procedure DBasicActor.SetMaxSTA(const Value: Float);
begin
  If Value < fSTA.MaxMax then
    fSTA.Max := Value
  else
    fSTA.Max := fSTA.MaxMax;
  If fSTA.Max < 0 then { EXAUSTED STATE };
end;

Procedure DBasicActor.SetMaxMaxSTA(const Value: Float);
begin
  if fSTA.MaxMax < Value then
    RestoreSTA(Value - fSTA.MaxMax, 1);
  fSTA.MaxMax := Value;
  If fSTA.MaxMax < 0 then { EXAUSTED STATE };
end;

procedure DBasicActor.ResetSTA;
begin
  fSTA.Max := fSTA.MaxMax;
  fSTA.Current := fSTA.Max;
end;

{ ----------------------------------------------------------------------------- }

Procedure DBasicActor.SetCNC(const Value: Float);
begin
  If Value < fCNC.Max then
    fCNC.Current := Value
  else
    fCNC.Current := fCNC.Max;
  If fCNC.Current < 0 then { BURN-OUT STATE };
end;

Procedure DBasicActor.SetMaxCNC(const Value: Float);
begin
  If Value < fCNC.MaxMax then
    fCNC.Max := Value
  else
    fCNC.Max := fCNC.MaxMax;
  If fCNC.Max < 0 then { BURN-OUT STATE };
end;

Procedure DBasicActor.SetMaxMaxCNC(const Value: Float);
begin
  if fCNC.MaxMax < Value then
    RestoreCNC(Value - fCNC.MaxMax, 1);
  fCNC.MaxMax := Value;
  If fCNC.MaxMax < 0 then { BURN-OUT STATE };
end;

procedure DBasicActor.ResetCNC;
begin
  fCNC.Max := fCNC.MaxMax;
  fCNC.Current := fCNC.Max;
end;

{ --------------------------------------------------------------------------- }

Procedure DBasicActor.SetMPH(const Value: Float);
begin
  If Value < fMPH.Max then
    fMPH.Current := Value
  else
    fMPH.Current := fMPH.Max;
  If fMPH.Current < 0 then { * STATE };
end;

Procedure DBasicActor.SetMaxMPH(const Value: Float);
begin
  If Value < fMPH.MaxMax then
    fMPH.Max := Value
  else
    fMPH.Max := fMPH.MaxMax;
  If fMPH.Max < 0 then { * STATE };
end;

Procedure DBasicActor.SetMaxMaxMPH(const Value: Float);
begin
  if fMPH.MaxMax < Value then
    RestoreMPH(Value - fMPH.MaxMax, 1);
  fMPH.MaxMax := Value;
  If fMPH.MaxMax < 0 then { * STATE };
end;

procedure DBasicActor.ResetMPH;
begin
  fMPH.Max := fMPH.MaxMax;
  fMPH.Current := fMPH.Max;
end;

{ --------------------------------------------------------------------------- }

procedure DBasicActor.ResetAll;
begin
  ResetHP;
  ResetSTA;
  ResetCNC;
  ResetMPH;
end;

{ --------------------------------------------------------------------------- }

function DBasicActor.HPRef: PStatValue;
begin
  Result := @fHP;
end;

function DBasicActor.STARef: PStatValue;
begin
  Result := @fSTA;
end;

function DBasicActor.CNCRef: PStatValue;
begin
  Result := @fCNC;
end;

function DBasicActor.MPHRef: PStatValue;
begin
  Result := @fMPH;
end;

{ --------------------------------------------------------------------------- }

Procedure DBasicActor.Hit(const Damage: Float; const Skill: Float);
begin
  SetHP(HP - Damage);
  SetMaxHP(MaxHP - Damage * Skill); // todo
  if Assigned(Self.onHit) then
    Self.onHit(Damage, dtHealth);
end;

function DBasicActor.Heal(const Value: Float; const Skill: Float): Boolean;
begin
  if (HP < MaxHP) or ((MaxHP < MaxMaxHP) and (Skill > 0)) then
  begin
    SetHP(HP + Value);
    SetMaxHP(MaxHP + Value * Skill); // todo
    Result := true;
  end
  else
    Result := False;
end;

{ ----------------------------------------------------------------------------- }

function DBasicActor.ConsumeSTA(const Consumption: Float;
  const Skill: Float): Boolean;
begin
  if (STA > Consumption) then
  begin
    SetSTA(STA - Consumption);
    SetMaxSTA(MaxSTA - Consumption * Skill); // todo
    Result := true;
  end
  else
    Result := False;
end;

function DBasicActor.RestoreSTA(const Restoration: Float;
  const Skill: Float): Boolean;
begin
  if (STA < MaxSTA) or ((MaxSTA < MaxMaxSTA) and (Skill > 0)) then
  begin
    SetSTA(STA + Restoration);
    SetMaxSTA(MaxSTA + Restoration * Skill); // todo
    Result := true;
  end
  else
    Result := False;
end;

procedure DBasicActor.DrainSTA(const Drain: Float; const Skill: Float);
begin
  SetSTA(STA - Drain);
  SetMaxSTA(MaxSTA - Drain * Skill); // todo
end;

{ ----------------------------------------------------------------------------- }

function DBasicActor.ConsumeCNC(const Consumption: Float;
  const Skill: Float): Boolean;
begin
  if (CNC > Consumption) then
  begin
    SetCNC(CNC - Consumption);
    SetMaxCNC(MaxCNC - Consumption * Skill); // todo
    Result := true;
  end
  else
    Result := False;
end;

function DBasicActor.RestoreCNC(const Restoration: Float;
  const Skill: Float): Boolean;
begin
  if (CNC < MaxCNC) or ((MaxCNC < MaxMaxCNC) and (Skill > 0)) then
  begin
    SetCNC(CNC + Restoration);
    SetMaxCNC(MaxCNC + Restoration * Skill); // todo
    Result := true;
  end
  else
    Result := False;
end;

procedure DBasicActor.DrainCNC(const Drain: Float; const Skill: Float);
begin
  SetCNC(CNC - Drain);
  SetMaxCNC(MaxCNC - Drain * Skill); // todo
end;

{ ----------------------------------------------------------------------------- }

function DBasicActor.ConsumeMPH(const Consumption: Float;
  const Skill: Float): Boolean;
begin
  if (MPH > Consumption) then
  begin
    SetMPH(MPH - Consumption);
    SetMaxMPH(MaxMPH - Consumption * Skill); // todo
    Result := true;
  end
  else
    Result := False;
end;

function DBasicActor.RestoreMPH(const Restoration: Float;
  const Skill: Float): Boolean;
begin
  if (MPH < MaxMPH) or ((MaxMPH < MaxMaxMPH) and (Skill > 0)) then
  begin
    SetMPH(MPH + Restoration);
    SetMaxMPH(MaxMPH + Restoration * Skill); // todo
    Result := true;
  end
  else
    Result := False;
end;

procedure DBasicActor.DrainMPH(const Drain: Float; const Skill: Float);
begin
  SetMPH(MPH - Drain);
  SetMaxMPH(MaxMPH - Drain * Skill); // todo
end;

{ =========================================================================== }
{ ========================== ACTOR BODY ===================================== }
{ =========================================================================== }

constructor DActorBody.Create;
begin
  inherited Create;
{$WARNING dummy, should set LastTime here}
end;

{ ----------------------------------------------------------------------------- }

destructor DActorBody.Destroy;
begin
  Window.SceneManager.Items.Remove(Body); // looks afwul
  FreeAndNil(Body);
  inherited Destroy;
end;

{ ----------------------------------------------------------------------------- }

procedure DActorBody.SpawnBodyHere(const SpawnBody: DBodyResource);
begin
  Body := DBody.Create(nil);
  // No one owns Body, so we'll free it manually in DActor.destroy
  // SpawnBody.CreateCreature(Window.SceneManager.Items, Position, Direction);
  Body.Resource := SpawnBody;
  Window.SceneManager.Items.Add(Body);
  Body.Up := CurrentWorld.GetGravity(Body.Position);
{$WARNING sometimes it's not enough, why???}
  Visible := true;
  Body.Collides := true;
  Body.CollidesWithMoving := true;
  InternalCamera := Body.Camera;
  ForceAnimation(atIdle);
end;

{ ----------------------------------------------------------------------------- }

procedure DActorBody.Spawn(const aNav: TNavID; const SpawnBody: DBodyResource);
begin
  TeleportTo(aNav);
  SpawnBodyHere(SpawnBody);
end;

procedure DActorBody.Spawn(const aPosition: TVector3;
  const SpawnBody: DBodyResource);
begin
  TeleportTo(aPosition);
  SpawnBodyHere(SpawnBody);
end;

{ ----------------------------------------------------------------------------- }

procedure DActorBody.SetVisible(const Value: Boolean);
begin
  Body.Exists := Value;
end;

function DActorBody.GetVisible: Boolean;
begin
  Result := Body.Exists;
end;

{ ----------------------------------------------------------------------------- }

procedure DActorBody.ForceAnimation(const at: TAnimationType);
begin
  Body.CurrentAnimationName := AnimationToString(at);
  Body.ResetAnimation;
end;

procedure DActorBody.ForceAnimation(const at: string);
begin
  Body.CurrentAnimationName := at;
  Body.ResetAnimation;
end;

procedure DActorBody.Animation(const at: TAnimationType);
begin
  Body.NextAnimationName := AnimationToString(at);
end;

procedure DActorBody.Animation(const at: string);
begin
  Body.NextAnimationName := at;
end;

{ ----------------------------------------------------------------------------- }

procedure DActorBody.Manage;
begin
  { dt := DecoNow - LastT;
    dtl := DecoNowLocal - LastTLocal;
    LastT := DecoNow;
    LastTLocal := DecoNowLocal; }

  // cute and simple, maybe merge them?
  doAI;

  doRotate;
  doMove;

  if Body <> nil then
  begin
    Body.Position := Position;
    Body.Direction := Direction;
  end;
end;

{ =========================================================================== }
{ ====================== D ACTOR ============================================ }
{ =========================================================================== }

function DActor.GetTarget: DCoordActor;
begin
  if fTarget = nil then
    GetEnemyTarget; // todo
  // dLog(LogActorError,Self,{$I %CURRENTROUTINE%},'Warning: Autoselecting target not implemented yet...');
  Result := fTarget;
end;

{ ----------------------------------------------------------------------------- }

function isEnemyFaction(const f1, f2: TFaction): Boolean;
begin
  // todo
  if f1 <> f2 then
    Result := true
  else
    Result := False;
end;

function isEnemy(const a1, a2: DSimpleActor): Boolean;
begin
  if isEnemyFaction(a1.Faction, a2.Faction) then
    Result := true
  else
    Result := False;
end;

procedure DActor.GetEnemyTarget;
var
  a, e: DSimpleActor;
  d, d_min: Float;
begin
  fTarget := nil;
  fTargetGroup := nil;
  // may be optimized by caching.
  if CurrentWorld is DAbstractWorld3d then

    d_min := -1;
  e := nil;
  for a in DAbstractWorld3d(CurrentWorld).Actors do
{$HINT dummy, actually this is wrong, as "target" may be a friendly unit, e.g. to heal, or in case of control loss}
    if (a <> Self) and (a is DBasicActor) and isEnemy(Self, a) and
      CanSee(DCoordActor(a)) and (DBasicActor(a).HP > 0) then
    begin
      d := (DCoordActor(a).Position - Self.Position).Length;
      if (d < Self.CombatRange) and ((d_min < d) or (d_min < 0)) then
      begin
        d_min := d;
        e := a;
      end;
    end;
  fTarget := DCoordActor(e);
end;

{ ----------------------------------------------------------------------------- }

procedure DActor.RequestSoftPause;
begin
{$WARNING todo}
{$HINT and player in THIS battle}
  if PlayerInBattle then
    RequestSoftPauseByAction(1);
end;

{ ----------------------------------------------------------------------------- }

function DActor.CanSee(const a1: DCoordActor): Boolean;
begin
  if (TVector3.DotProduct(Self.Direction, (a1.Position - Self.Position)) > 0)
  then
    Result := true
  else
    Result := False;
end;

{ ----------------------------------------------------------------------------- }

procedure DActor.PerformAction(const doAction: DMultiPerk);
begin
  if fTarget = nil then
  begin
    Log(LogActorError,
{$I %CURRENTROUTINE%},
      'ERROR: Action was requested but no target specified...');
    Exit;
  end;

  if (fTarget is DBasicActor) and
    ((Position - fTarget.Position).Length < Self.CombatRange) then
  begin
    Self.Animation(atAttack);
    Self.RequestSoftPause;
    DBasicActor(fTarget).Hit(10, 1);
  end
  else
    Log(LogActorError,
{$I %CURRENTROUTINE%}, 'ERROR: Trying to preform action on invalid actor...');
end;

{ ----------------------------------------------------------------------------- }

procedure DActor.LookAt;
begin
  if fTarget <> nil then
    LookAt(Target.Position)
  else
    Log(LogActorError,
{$I %CURRENTROUTINE%}, 'Warning: trying to look at a nil target...');
end;

procedure DActor.LookAt(const aPosition: TVector3);
begin
  toDir := aPosition - Position;
  toDir[2] := 0; // cut-off z component
  FixZeroDirection;
  toDir.NormalizeMe;
end;

{ ----------------------------------------------------------------------------- }

procedure DActor.RecoverAll;
begin
  // dummy, should also reset all active statuses
  ResetAll;
end;

{ ----------------------------------------------------------------------------- }

constructor DActor.Create;
begin
  inherited Create;
  Actions := DPerksList.Create(False);
end;

{ ----------------------------------------------------------------------------- }

destructor DActor.Destroy;
begin
  FreeAndNil(Actions);
  inherited Destroy;
end;

{ =========================================================================== }
{ =========================== D MONSTER ===================================== }
{ =========================================================================== }

procedure DMonster.doAI;
begin
  { if the target is close enough look at it }
  if fTarget = nil then
    GetEnemyTarget;

  if fTarget <> nil then
    if ((fTarget.Position - Position).Length < Self.CombatRange) and
      (CanSee(fTarget)) then
      LookAt;

  if SoftPause > 0 then
    Exit;

  if fTarget <> nil then
    // tmp
    if (drnd.Random < 0.005) and CanSee(fTarget) then
      Self.PerformAction(nil);

  // if drnd.Random<0.006 then self.Animation(atAttack);
  // if drnd.Random<0.002 then self.ForceAnimation(atDie);

  // body.Resource.Animations.FindName('Attack');
  // body.Sound3d();
  // body.ExecuteAction();

  // (body.Items[0] as TCastleScene).PlayAnimation('attack', paForceNotLooping);
  // Scene.AnimationTimeSensor('my_animation').EventIsActive.OnReceive.Add(@AnimationIsActiveChanged)
end;

{ ----------------------------------------------------------------------------- }

constructor DMonster.Create;
begin
  inherited Create;
  Self.onHit := @Self.doHit;
  Faction := fHostile;
end;

{ ----------------------------------------------------------------------------- }

procedure DMonster.doHit(const Dam: Float; const Damtype: TDamageType);
begin
  if Self.HP > 0 then
    Self.ForceAnimation(atHurt)

    // and show numeric representation of Dam
    // ? and negative status applied ?
end;

{ ----------------------------------------------------------------------------- }

procedure DMonster.Die;
begin
  Self.ForceAnimation(atDie);
end;

{ =========================================================================== }
{ ========================== ACTOR GROUP ==================================== }
{ =========================================================================== }

constructor DActorGroup.Create;
begin
  inherited Create; // <------- actually may be unneeded if parent is DObject
  Members := TActorList.Create(False);
end;

destructor DActorGroup.Destroy;
begin
  FreeAndNil(Members)
end;

end.
