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
  along with this program. if not, see <http://www.gnu.org/licenses/>. }

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
  TDamageProcedure = procedure(const Dam: Float;
    const Damtype: TDamageType) of object;

type
  TFaction = (ffPlayer, ffHostile);

type
  { This Actor has only the most basic features like his "tile" position
    Will be used in some remote future for Actors behaviour on global map }
  DSimpleActor = class abstract(DObject)
  private
    { Last Nav, where the Actor was standing }
    LastNav: TNavID;
  public
    { if this entity is a player character? }
    isPlayer: boolean;
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
    function CanMovePos(const aPos: TVector3): boolean;
{$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}
    function CanMoveDir(const aDir: TVector3): boolean;
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
    procedure SetVisible(const Value: boolean);
    { reads visibility of this Actor's body }
    function GetVisible: boolean;
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
    property Visible: boolean read GetVisible write SetVisible;
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

type
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
    function Heal(const Value: Float; const Skill: Float): boolean;
    // =restoreHP

    { Abstract action preformed on Actor's death }
    procedure Die; virtual; abstract;

    { "consumption" procedures return true if success and false if failed,
      "restoration" procedures return true if something has been restored,
      "drain" procedures can drain Values below zero }
    function ConsumeSTA(const Consumption: Float; const Skill: Float): boolean;
    function RestoreSTA(const Restoration: Float; const Skill: Float): boolean;
    procedure DrainSTA(const Drain: Float; const Skill: Float);
    function ConsumeCNC(const Consumption: Float; const Skill: Float): boolean;
    function RestoreCNC(const Restoration: Float; const Skill: Float): boolean;
    procedure DrainCNC(const Drain: Float; const Skill: Float);
    function ConsumeMPH(const Consumption: Float; const Skill: Float): boolean;
    function RestoreMPH(const Restoration: Float; const Skill: Float): boolean;
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
    function CanSee(const a1: DCoordActor): boolean;
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
  DecoGameMode, DecoLog, Profiler;

{ =========================================================================== }
{ ====================== SIMPLE ACTOR ======================================= }
{ =========================================================================== }

procedure DSimpleActor.TeleportTo(const aNav: TNavID);
begin
  {StartProfiler}

  if LastNav <> UnitinializedNav then
    CurrentWorld.ReleaseNav(LastNav);
  CurrentWorld.BlockNav(aNav);
  LastNav := aNav;

  {StopProfiler}
end;

{ ----------------------------------------------------------------------------- }

procedure DSimpleActor.Manage;
begin
  {StartProfiler}
  {$HINT todo}
  {StopProfiler}
end;

{ ----------------------------------------------------------------------------- }

constructor DSimpleActor.Create;
begin
  {StartProfiler}

  // inherited; <------ nothing to inherit
  isPlayer := False;
  // nothing to create yet

  {StopProfiler}
end;

{ =========================================================================== }
{ ====================== COORD ACTOR ======================================== }
{ =========================================================================== }

procedure DCoordActor.GetRandomDirection;
var
  rDir: Float;
begin
  {StartProfiler}

  rDir := DRND.Random * 2 * Pi;
  Direction := Vector3(sin(rDir), cos(rDir), 0);
  toDir := Direction;

  {StopProfiler}
end;

{ ----------------------------------------------------------------------------- }

procedure DCoordActor.doRotate;
begin
  {StartProfiler}

  {$WARNING dummy}
  if not TVector3.Equals(Direction, toDir) then
  begin
    Direction := toDir;
  end;

  {StopProfiler}
end;

{ ----------------------------------------------------------------------------- }

procedure DCoordActor.doMove;
begin
  {StartProfiler}

  {$WARNING dummy}
  if TVector3.Equals(Direction, toDir) then
  begin
    Position := toPos;
  end;

  {StopProfiler}
end;

{ ----------------------------------------------------------------------------- }

procedure DCoordActor.TeleportTo(const aPosition: TVector3);
begin
  {StartProfiler}

  GetRandomDirection;
  Position := aPosition;
  toPos := Position;

  {StopProfiler}
end;

procedure DCoordActor.TeleportTo(const aPosition, aDirection: TVector3);
begin
  {StartProfiler}

  Direction := aDirection;
  toDir := Direction;
  Position := aPosition;
  toPos := Position;

  {StopProfiler}
end;

procedure DCoordActor.TeleportTo(const aNav: TNavID);
begin
  {StartProfiler}

  inherited TeleportTo(aNav);
  TeleportTo(CurrentWorld.NavToVector3(aNav));

  {StopProfiler}
end;

{ ----------------------------------------------------------------------------- }

procedure DCoordActor.FixZeroDirection; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}
begin
  {StartProfiler}

  { this is a critical, very rare and nearly unfixable error, so making just
    some precautions to forget about it forever. }
  if toDir.IsZero then
  begin
    if not Direction.IsZero then
      toDir := Direction
    else
      toDir := Vector3(0, 1, 0);
    Log(LogActorError, _CurrentRoutine, 'ERROR: Direction is zero!');
  end;

  {StopProfiler}
end;

{ ----------------------------------------------------------------------------- }

constructor DCoordActor.Create;
begin
  {StartProfiler}

  inherited Create;
  LastNav := UnitinializedNav;

  {StopProfiler}
end;

{ =========================================================================== }
{ ====================== COORD ACTOR ======================================== }
{ =========================================================================== }

procedure DActorPhysics.doGravity;
begin
  {StartProfiler}

  if InternalCamera = nil then
    Exit;
  if CanMoveDir(CurrentGravityDown - Vector3(0, 0, Height)) then
    Position := Position + CurrentGravityDown;

  {StopProfiler}
end;

{ ---------------------------------------------------------------------------- }

constructor DActorPhysics.Create;
begin
  {StartProfiler}

  inherited Create;
  Height := 0;
  CurrentGravityDown := Vector3(0, 0, 1);

  {StopProfiler}
end;

{ ---------------------------------------------------------------------------- }

procedure DActorPhysics.Manage;
begin
  {StartProfiler}

  CurrentGravityDown := -DeltaTLocal * CurrentWorld.GetGravity(Position) *
    CurrentWorld.GravityAcceleration;
  inherited Manage;
  doGravity;

  {StopProfiler}
end;

{ ---------------------------------------------------------------------------- }

function DActorPhysics.CanMovePos(const aPos: TVector3): boolean;
{$IFDEF SUPPORTS_INLINE}inline;
{$ENDIF}
var
  tmp: TVector3;
begin
  {StartProfiler}

  // InternalCamera may be nil, but we skip the check for speed. Be careful.
  Result := InternalCamera.DoMoveAllowed(aPos, tmp, False);

  {StopProfiler}
end;

function DActorPhysics.CanMoveDir(const aDir: TVector3): boolean;
{$IFDEF SUPPORTS_INLINE}inline;
{$ENDIF}
var
  tmp: TVector3;
begin
  {StartProfiler}

  // InternalCamera may be nil, but we skip the check for speed. Be careful.
  Result := InternalCamera.DoMoveAllowed(InternalCamera.Position + aDir, tmp, False);

  {StopProfiler}
end;

{ =========================================================================== }
{ ========================== BASIC ACTOR ==================================== }
{ =========================================================================== }

constructor DBasicActor.Create;
begin
  {StartProfiler}

  inherited Create;
  Nickname := 'abc';
  // setting some Values to avoid uncertainity
  SetMaxMaxHP(100);
  SetMaxMaxSTA(100);
  SetMaxMaxCNC(100);
  SetMaxMaxMPH(100);
  DefenseRandom := TCastleRandom.Create;
{$HINT read seed from the savegame}
  AttackRandom := TCastleRandom.Create;
  ResetAll;

  {StopProfiler}
end;

{ ---------------------------------------------------------------------------- }

destructor DBasicActor.Destroy;
begin
  {StartProfiler}

  FreeAndNil(DefenseRandom);
  FreeAndNil(AttackRandom);
  inherited Destroy;

  {StopProfiler}
end;

{ ---------------------------------------------------------------------------- }

procedure DBasicActor.SetHP(const Value: Float);
begin
  if Value < fHP.Max then
    fHP.Current := Value
  else
    fHP.Current := fHP.Max;
  if fHP.Current < 0 then
    Die;
end;

procedure DBasicActor.SetMaxHP(const Value: Float);
begin
  if Value < fHP.MaxMax then
    fHP.Max := Value
  else
    fHP.Max := fHP.MaxMax;
  if fHP.Max < 0 then
    Die;
end;

procedure DBasicActor.SetMaxMaxHP(const Value: Float);
begin
  if fHP.MaxMax < Value then
    Heal(Value - fHP.MaxMax, 1);
  fHP.MaxMax := Value;
  if fHP.MaxMax < 0 then
    Die;
end;

procedure DBasicActor.ResetHP;
begin
  fHP.Max := fHP.MaxMax;
  fHP.Current := fHP.Max;
end;

{ --------------------------------------------------------------------------- }

procedure DBasicActor.SetSTA(const Value: Float);
begin
  if Value < fSTA.Max then
    fSTA.Current := Value
  else
    fSTA.Current := fSTA.Max;
  if fSTA.Current < 0 then
    { EXAUSTED STATE };
end;

procedure DBasicActor.SetMaxSTA(const Value: Float);
begin
  if Value < fSTA.MaxMax then
    fSTA.Max := Value
  else
    fSTA.Max := fSTA.MaxMax;
  if fSTA.Max < 0 then
    { EXAUSTED STATE };
end;

procedure DBasicActor.SetMaxMaxSTA(const Value: Float);
begin
  if fSTA.MaxMax < Value then
    RestoreSTA(Value - fSTA.MaxMax, 1);
  fSTA.MaxMax := Value;
  if fSTA.MaxMax < 0 then
    { EXAUSTED STATE };
end;

procedure DBasicActor.ResetSTA;
begin
  fSTA.Max := fSTA.MaxMax;
  fSTA.Current := fSTA.Max;
end;

{ ----------------------------------------------------------------------------- }

procedure DBasicActor.SetCNC(const Value: Float);
begin
  if Value < fCNC.Max then
    fCNC.Current := Value
  else
    fCNC.Current := fCNC.Max;
  if fCNC.Current < 0 then
    { BURN-OUT STATE };
end;

procedure DBasicActor.SetMaxCNC(const Value: Float);
begin
  if Value < fCNC.MaxMax then
    fCNC.Max := Value
  else
    fCNC.Max := fCNC.MaxMax;
  if fCNC.Max < 0 then
    { BURN-OUT STATE };
end;

procedure DBasicActor.SetMaxMaxCNC(const Value: Float);
begin
  if fCNC.MaxMax < Value then
    RestoreCNC(Value - fCNC.MaxMax, 1);
  fCNC.MaxMax := Value;
  if fCNC.MaxMax < 0 then
    { BURN-OUT STATE };
end;

procedure DBasicActor.ResetCNC;
begin
  fCNC.Max := fCNC.MaxMax;
  fCNC.Current := fCNC.Max;
end;

{ --------------------------------------------------------------------------- }

procedure DBasicActor.SetMPH(const Value: Float);
begin
  if Value < fMPH.Max then
    fMPH.Current := Value
  else
    fMPH.Current := fMPH.Max;
  if fMPH.Current < 0 then
    { * STATE };
end;

procedure DBasicActor.SetMaxMPH(const Value: Float);
begin
  if Value < fMPH.MaxMax then
    fMPH.Max := Value
  else
    fMPH.Max := fMPH.MaxMax;
  if fMPH.Max < 0 then
    { * STATE };
end;

procedure DBasicActor.SetMaxMaxMPH(const Value: Float);
begin
  if fMPH.MaxMax < Value then
    RestoreMPH(Value - fMPH.MaxMax, 1);
  fMPH.MaxMax := Value;
  if fMPH.MaxMax < 0 then
    { * STATE };
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

procedure DBasicActor.Hit(const Damage: Float; const Skill: Float);
begin
  SetHP(HP - Damage);
  SetMaxHP(MaxHP - Damage * Skill); // todo
  if Assigned(Self.onHit) then
    Self.onHit(Damage, dtHealth);
end;

function DBasicActor.Heal(const Value: Float; const Skill: Float): boolean;
begin
  if (HP < MaxHP) or ((MaxHP < MaxMaxHP) and (Skill > 0)) then
  begin
    SetHP(HP + Value);
    SetMaxHP(MaxHP + Value * Skill); // todo
    Result := True;
  end
  else
    Result := False;
end;

{ ----------------------------------------------------------------------------- }

function DBasicActor.ConsumeSTA(const Consumption: Float; const Skill: Float): boolean;
begin
  if (STA > Consumption) then
  begin
    SetSTA(STA - Consumption);
    SetMaxSTA(MaxSTA - Consumption * Skill); // todo
    Result := True;
  end
  else
    Result := False;
end;

function DBasicActor.RestoreSTA(const Restoration: Float; const Skill: Float): boolean;
begin
  if (STA < MaxSTA) or ((MaxSTA < MaxMaxSTA) and (Skill > 0)) then
  begin
    SetSTA(STA + Restoration);
    SetMaxSTA(MaxSTA + Restoration * Skill); // todo
    Result := True;
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

function DBasicActor.ConsumeCNC(const Consumption: Float; const Skill: Float): boolean;
begin
  if (CNC > Consumption) then
  begin
    SetCNC(CNC - Consumption);
    SetMaxCNC(MaxCNC - Consumption * Skill); // todo
    Result := True;
  end
  else
    Result := False;
end;

function DBasicActor.RestoreCNC(const Restoration: Float; const Skill: Float): boolean;
begin
  if (CNC < MaxCNC) or ((MaxCNC < MaxMaxCNC) and (Skill > 0)) then
  begin
    SetCNC(CNC + Restoration);
    SetMaxCNC(MaxCNC + Restoration * Skill); // todo
    Result := True;
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

function DBasicActor.ConsumeMPH(const Consumption: Float; const Skill: Float): boolean;
begin
  if (MPH > Consumption) then
  begin
    SetMPH(MPH - Consumption);
    SetMaxMPH(MaxMPH - Consumption * Skill); // todo
    Result := True;
  end
  else
    Result := False;
end;

function DBasicActor.RestoreMPH(const Restoration: Float; const Skill: Float): boolean;
begin
  if (MPH < MaxMPH) or ((MaxMPH < MaxMaxMPH) and (Skill > 0)) then
  begin
    SetMPH(MPH + Restoration);
    SetMaxMPH(MaxMPH + Restoration * Skill); // todo
    Result := True;
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
  {StartProfiler}

  inherited Create;
  {$WARNING dummy, should set LastTime here}

  {StopProfiler}
end;

{ ----------------------------------------------------------------------------- }

destructor DActorBody.Destroy;
begin
  {StartProfiler}

  Window.SceneManager.Items.Remove(Body); // looks afwul
  FreeAndNil(Body);
  inherited Destroy;

  {StopProfiler}
end;

{ ----------------------------------------------------------------------------- }

procedure DActorBody.SpawnBodyHere(const SpawnBody: DBodyResource);
begin
  {StartProfiler}

  Body := DBody.Create(nil);
  // No one owns Body, so we'll free it manually in DActor.destroy
  // SpawnBody.CreateCreature(Window.SceneManager.Items, Position, Direction);
  Body.Resource := SpawnBody;
  Window.SceneManager.Items.Add(Body);
  Body.Up := CurrentWorld.GetGravity(Body.Position);
{$WARNING sometimes it's not enough, why???}
  Visible := True;
  Body.Collides := True;
  Body.CollidesWithMoving := True;
  InternalCamera := Body.Camera;
  ForceAnimation(atIdle);

  {StopProfiler}
end;

{ ----------------------------------------------------------------------------- }

procedure DActorBody.Spawn(const aNav: TNavID; const SpawnBody: DBodyResource);
begin
  {StartProfiler}

  TeleportTo(aNav);
  SpawnBodyHere(SpawnBody);

  {StopProfiler}
end;

procedure DActorBody.Spawn(const aPosition: TVector3; const SpawnBody: DBodyResource);
begin
  {StartProfiler}

  TeleportTo(aPosition);
  SpawnBodyHere(SpawnBody);

  {StopProfiler}
end;

{ ----------------------------------------------------------------------------- }

procedure DActorBody.SetVisible(const Value: boolean);
begin
  {StartProfiler}

  Body.Exists := Value;

  {StopProfiler}
end;

function DActorBody.GetVisible: boolean;
begin
  {StartProfiler}

  Result := Body.Exists;

  {StopProfiler}
end;

{ ----------------------------------------------------------------------------- }

procedure DActorBody.ForceAnimation(const at: TAnimationType);
begin
  {StartProfiler}

  Body.CurrentAnimationName := AnimationToString(at);
  Body.ResetAnimation;

  {StopProfiler}
end;

procedure DActorBody.ForceAnimation(const at: string);
begin
  {StartProfiler}

  Body.CurrentAnimationName := at;
  Body.ResetAnimation;

  {StopProfiler}
end;

procedure DActorBody.Animation(const at: TAnimationType);
begin
  {StartProfiler}

  Body.NextAnimationName := AnimationToString(at);

  {StopProfiler}
end;

procedure DActorBody.Animation(const at: string);
begin
  {StartProfiler}

  Body.NextAnimationName := at;

  {StopProfiler}
end;

{ ----------------------------------------------------------------------------- }

procedure DActorBody.Manage;
begin
  {StartProfiler}

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

  {StopProfiler}
end;

{ =========================================================================== }
{ ====================== D ACTOR ============================================ }
{ =========================================================================== }

function DActor.GetTarget: DCoordActor;
begin
  {StartProfiler}

  if fTarget = nil then
    GetEnemyTarget; // todo
  // dLog(LogActorError,Self,_CurrentRoutine,'Warning: Autoselecting target not implemented yet...');
  Result := fTarget;

  {StopProfiler}
end;

{ ----------------------------------------------------------------------------- }

function isEnemyFaction(const f1, f2: TFaction): boolean;
begin
  {StartProfiler}

  // todo
  if f1 <> f2 then
    Result := True
  else
    Result := False;

  {StopProfiler}
end;

function isEnemy(const a1, a2: DSimpleActor): boolean;
begin
  {StartProfiler}

  if isEnemyFaction(a1.Faction, a2.Faction) then
    Result := True
  else
    Result := False;

  {StopProfiler}
end;

procedure DActor.GetEnemyTarget;
var
  a, e: DSimpleActor;
  d, d_min: Float;
begin
  {StartProfiler}

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

  {StopProfiler}
end;

{ ----------------------------------------------------------------------------- }

procedure DActor.RequestSoftPause;
begin
  {StartProfiler}

  {$WARNING todo}
  {$HINT and player in THIS battle}
  if PlayerInBattle then
    RequestSoftPauseByAction(1);

  {StopProfiler}
end;

{ ----------------------------------------------------------------------------- }

function DActor.CanSee(const a1: DCoordActor): boolean;
begin
  {StartProfiler}

  if (TVector3.DotProduct(Self.Direction, (a1.Position - Self.Position)) > 0) then
    Result := True
  else
    Result := False;

  {StopProfiler}
end;

{ ----------------------------------------------------------------------------- }

procedure DActor.PerformAction(const doAction: DMultiPerk);
begin
  {StartProfiler}

  if fTarget = nil then
  begin
    Log(LogActorError, _CurrentRoutine,
      'ERROR: Action was requested but no target specified...');
    Exit;
  end;

  if (fTarget is DBasicActor) and ((Position - fTarget.Position).Length <
    Self.CombatRange) then
  begin
    Self.Animation(atAttack);
    Self.RequestSoftPause;
    DBasicActor(fTarget).Hit(10, 1);
  end
  else
    Log(LogActorError, _CurrentRoutine,
      'ERROR: Trying to preform action on invalid actor...');

  {StopProfiler}
end;

{ ----------------------------------------------------------------------------- }

procedure DActor.LookAt;
begin
  {StartProfiler}

  if fTarget <> nil then
    LookAt(Target.Position)
  else
    Log(LogActorError, _CurrentRoutine,
      'Warning: trying to look at a nil target...');

  {StopProfiler}
end;

procedure DActor.LookAt(const aPosition: TVector3);
begin
  {StartProfiler}

  toDir := aPosition - Position;
  toDir[2] := 0; // cut-off z component
  FixZeroDirection;
  toDir.NormalizeMe;

  {StopProfiler}
end;

{ ----------------------------------------------------------------------------- }

procedure DActor.RecoverAll;
begin
  {StartProfiler}

  // dummy, should also reset all active statuses
  ResetAll;

  {StopProfiler}
end;

{ ----------------------------------------------------------------------------- }

constructor DActor.Create;
begin
  {StartProfiler}

  inherited Create;
  Actions := DPerksList.Create(False);

  {StopProfiler}
end;

{ ----------------------------------------------------------------------------- }

destructor DActor.Destroy;
begin
  {StartProfiler}

  FreeAndNil(Actions);
  inherited Destroy;

  {StopProfiler}
end;

{ =========================================================================== }
{ =========================== D MONSTER ===================================== }
{ =========================================================================== }

procedure DMonster.doAI;
begin
  {StartProfiler}

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
    if (DRND.Random < 0.005) and CanSee(fTarget) then
      Self.PerformAction(nil);

  // if DRND.Random<0.006 then Self.Animation(atAttack);
  // if DRND.Random<0.002 then Self.ForceAnimation(atDie);

  // body.Resource.Animations.FindName('Attack');
  // body.Sound3d();
  // body.ExecuteAction();

  // (body.Items[0] as TCastleScene).PlayAnimation('attack', paForceNotLooping);
  // Scene.AnimationTimeSensor('my_animation').EventIsActive.OnReceive.Add(@AnimationIsActiveChanged)

  {StopProfiler}
end;

{ ----------------------------------------------------------------------------- }

constructor DMonster.Create;
begin
  {StartProfiler}

  inherited Create;
  Self.onHit := @Self.doHit;
  Faction := ffHostile;

  {StopProfiler}
end;

{ ----------------------------------------------------------------------------- }

procedure DMonster.doHit(const Dam: Float; const Damtype: TDamageType);
begin
  {StartProfiler}

  if Self.HP > 0 then
    Self.ForceAnimation(atHurt);

  // and show numeric representation of Dam
  // ? and negative status applied ?

  {StopProfiler}
end;

{ ----------------------------------------------------------------------------- }

procedure DMonster.Die;
begin
  {StartProfiler}

  Self.ForceAnimation(atDie);

  {StopProfiler}
end;

{ =========================================================================== }
{ ========================== ACTOR GROUP ==================================== }
{ =========================================================================== }

constructor DActorGroup.Create;
begin
  {StartProfiler}

  inherited Create; // <------- actually may be unneeded if parent is DObject
  Members := TActorList.Create(False);

  {StopProfiler}
end;

destructor DActorGroup.Destroy;
begin
  {StartProfiler}

  FreeAndNil(Members);

  {StopProfiler}
end;

end.
