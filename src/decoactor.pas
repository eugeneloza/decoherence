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

uses Classes, CastleRandom, fgl, CastleVectors, CastleCameras,
  CastleResources, CastleCreatures,
  DecoNavigationNetwork,
  DecoStats, DecoPerks, decoactorbody,
  DecoGlobal, DecoTime;

type TDamageType = (dtHealth);
type TDamageProcedure = procedure (const Dam: float; const Damtype: TDamageType) of Object;

type TFaction = (fPlayer, fHostile);

type
  { This Actor has only the most basic features like his "tile" position
    Will be used in some remote future for Actors behaviour on global map }
  DSimpleActor = class abstract(TObject)
  private
    { Last Nav, where the Actor was standing }
    LastNav: TNavID;
  public
    {}
    isPlayer: boolean;
    { Faction this Actor belongs to }
    Faction: TFaction;
    { Teleport this Actor to aNav }
    procedure TeleportTo(const aNav: TNavID); virtual;
    { Procedures preformed on this actor every frame }
    procedure Manage; virtual; //it'll do something useful some day...
    constructor Create; virtual; // override;
  end;

{ List of DSimpleActors }
type TActorList = specialize TFPGObjectList<DSimpleActor>;

type
  { Actor with full World coordinates,
    Mostly needed as a Target for AI
    and Base for Player camera mangement }
  DCoordActor = class (DSimpleActor)
  private
    procedure FixZeroDirection; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
  public
    { Position, direction and rotate_to_direction of the Body }
    Position,Direction: TVector3;
    { Instantly move Actor to aPosition or aNav }
    procedure TeleportTo(const aPosition: TVector3);
    procedure TeleportTo(const aPosition, aDirection: TVector3);
    procedure TeleportTo(const aNav: TNavID); override;
  protected
    { What position/direction this Actor moves to }
    toPos,toDir: TVector3;
    { Initializes direction with a random value}
    procedure GetRandomDirection;
    {rotates the body}
    procedure doRotate;
    {moves the body}
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
    { DActorPhysics does not set the camera, but uses it! It's defined in descendants}
    InternalCamera: TWalkCamera;
    { Is movement to this direction or position is allowed? }
    function CanMovePos(const aPos: TVector3): boolean;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    function CanMoveDir(const aDir: TVector3): boolean;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
  public
    { Height of this actor, usually should be zero,
          Mostly needed for player's camera height }
    Height: float;
    { Determine gravity influence on this Actor }
    procedure doGravity;
    procedure Manage; override;
    constructor Create; override;
  end;

type
  { Actor with a rendered body and corresponding management routines }
  DActorBody = class abstract (DActorPhysics)
  protected
    { AI of this actor, determines orders for actors and choice of targets }
    procedure doAI; virtual; abstract;
  private
    {shows or hides body of this actor}
    procedure SetVisible(const value: boolean);
    {reads visibility of this Actor's body}
    function GetVisible: boolean;
    {creates a body}
    procedure SpawnBodyHere(const SpawnBody: DBodyResource);
  public
    { 3D body of this Actor, it may be nil (in case the body is unloaded from
      RAM or belongs to an body-less entity, like Player's character at the moment }
    Body: DBody;
    { This is a type of the Actor's body
      and the resource, containing 3D model animations and etc.}
    BodyResource: DBodyResource;
    { Shows or hides the actor's body}
    property Visible: boolean read GetVisible write SetVisible;
    { Spawns a body for the Actor, overriden in children to spawn attributes}
    procedure Spawn(const aNav: TNavID; const SpawnBody: DBodyResource);
    procedure Spawn(const aPosition: TVector3; const SpawnBody: DBodyResource); virtual;
    { Manages this actor, e.g. preforms AI,
      called each frame}
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
    Current, Max, MaxMax: float;
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
    Procedure SetHP(const Value: float);
    Procedure SetMaxHP(const Value: float);
    Procedure SetMaxMaxHP(const Value: float);
    Procedure SetSTA(const Value: float);
    Procedure SetMaxSTA(const Value: float);
    Procedure SetMaxMaxSTA(const Value: float);
    Procedure SetCNC(const Value: float);
    Procedure SetMaxCNC(const Value: float);
    Procedure SetMaxMaxCNC(const Value: float);
    Procedure SetMPH(const Value: float);
    Procedure SetMaxMPH(const Value: float);
    Procedure SetMaxMaxMPH(const Value: float);
  public
    { getters and setters }
    Property HP: float read fHP.Current write SetHP;
    Property MaxHP: float read fHP.Max write SetMaxHP;
    Property MaxMaxHP: float read fHP.MaxMax write SetMaxMaxHP;
    procedure ResetHP;
    Property STA: float read fSTA.Current write SetSTA;
    Property MaxSTA: float read fSTA.Max write SetMaxSTA;
    Property MaxMaxSTA: float read fSTA.MaxMax write SetMaxMaxSTA;
    procedure ResetSTA;
    Property CNC: float read fCNC.Current write SetCNC;
    Property MaxCNC: float read fCNC.Max write SetMaxCNC;
    Property MaxMaxCNC: float read fCNC.MaxMax write SetMaxMaxCNC;
    procedure ResetCNC;
    Property MPH: float read fMPH.Current write SetMPH;
    Property MaxMPH: float read fMPH.Max write SetMaxMPH;
    Property MaxMaxMPH: float read fMPH.MaxMax write SetMaxMaxMPH;
    procedure ResetMPH;
    procedure ResetAll;

    function HPRef : PStatValue;
    function STARef: PStatValue;
    function CNCRef: PStatValue;
    function MPHRef: PStatValue;

    { Hit equals to consume+drain }
    procedure Hit(const Damage: float; const Skill: float); //=consumeHP
    { Returns true if healed or false if nothing to heal }
    function Heal(const Value: float; const Skill: float): boolean; //=restoreHP

    { Abstract action preformed on Actor's death }
    Procedure Die; virtual; abstract;

    { "consumption" procedures return true if success and false if failed,
      "restoration" procedures return true if something has been restored,
      "drain" procedures can drain values below zero }
    function ConsumeSTA(const Consumption: float; const Skill: float): boolean;
    function RestoreSTA(const Restoration: float; const Skill: float): boolean;
    procedure DrainSTA(const Drain: float;        const Skill: float);
    function ConsumeCNC(const Consumption: float; const Skill: float): boolean;
    function RestoreCNC(const Restoration: float; const Skill: float): boolean;
    procedure DrainCNC(const Drain: float;        const Skill: float);
    function ConsumeMPH(const Consumption: float; const Skill: float): boolean;
    function RestoreMPH(const Restoration: float; const Skill: float): boolean;
    procedure DrainMPH(const Drain: float;        const Skill: float);
  public
    { events }
    onHit: TDamageProcedure;

  public
    { Stats of this Actor, they must be initialized afterwards, when it
      is clear, whether SetFullStats is true (PC and RPC) or false (NPC and monsters) }
    Stats, MaxStats: DStats;

  public
    { Three-letter nickname for short display}
    Nickname: string;
    { These are randoms for the actor: defense gives his defense rolls,
      Attack provides for attack rolls
      All non-important random rolls are taken by global DRND }
    DefenseRandom,AttackRandom: TCastleRandom;
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
    procedure doHit(const Dam: float; const Damtype: TDamageType);
    procedure Die; override;
  public
    //procedure Manage; override;
    constructor Create; override;
  end;

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses SysUtils, CastleLog,
  CastleFilesUtils, DecoInputOutput,

  DecoAbstractWorld, DecoAbstractWorld3D,
  CastleScene,
  DecoGameMode;


{===========================================================================}
{====================== SIMPLE ACTOR =======================================}
{===========================================================================}

procedure DSimpleActor.TeleportTo(const aNav: TNavID);
begin
  if LastNav<>UnitinializedNav then CurrentWorld.ReleaseNav(LastNav);
  CurrentWorld.BlockNav(aNav);
  LastNav := aNav;
end;

{-----------------------------------------------------------------------------}

procedure DSimpleActor.Manage;
begin
  {$hint todo}
end;

{-----------------------------------------------------------------------------}

constructor DSimpleActor.Create;
begin
  //inherited; <------ nothing to inherit
  isPlayer := false;
  //nothing to create yet
end;

{===========================================================================}
{====================== COORD ACTOR ========================================}
{===========================================================================}

procedure DCoordActor.GetRandomDirection;
var rDir: float;
begin
  rDir := drnd.Random*2*Pi;
  Direction := Vector3(sin(rDir),cos(rDir),0);
  toDir := Direction;
end;

{-----------------------------------------------------------------------------}

procedure DCoordActor.doRotate;
begin
  {$Warning dummy}
  if not TVector3.Equals(Direction, toDir) then begin
    Direction := toDir;
  end;
end;

{-----------------------------------------------------------------------------}

procedure DCoordActor.doMove;
begin
  {$Warning dummy}
  if TVector3.Equals(Direction, toDir) then begin
    Position := toPos;
  end;
end;

{-----------------------------------------------------------------------------}

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

{-----------------------------------------------------------------------------}

procedure DCoordActor.FixZeroDirection; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  {this is a critical, very rare and nearly unfixable error, so making just
   some precautions to forget about it forever.}
  if ToDir.IsZero then begin
    if not Direction.IsZero then
      toDir := Direction
    else
      toDir := Vector3(0,1,0);
    WriteLnLog('DActor.LookAt','ERROR: Direction is zero!');
  end;
end;

{-----------------------------------------------------------------------------}

constructor DCoordActor.Create;
begin
  inherited Create;
  LastNav := UnitinializedNav;
end;

{===========================================================================}
{====================== COORD ACTOR ========================================}
{===========================================================================}

procedure DActorPhysics.doGravity;
begin
  if InternalCamera=nil then Exit;
  if CanMoveDir(CurrentGravityDown-Vector3(0,0,Height)) then Position := Position+CurrentGravityDown;
end;

{----------------------------------------------------------------------------}

constructor DActorPhysics.Create;
begin
  inherited Create;
  Height := 0;
  CurrentGravityDown := Vector3(0,0,1);
end;

{----------------------------------------------------------------------------}

procedure DActorPhysics.Manage;
begin
  CurrentGravityDown := -DeltaTLocal*CurrentWorld.GetGravity(Position)*CurrentWorld.GravityAcceleration;
  inherited Manage;
  doGravity;
end;

{----------------------------------------------------------------------------}

function DActorPhysics.CanMovePos(const aPos: TVector3): boolean;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
var tmp: TVector3;
begin
  //InternalCamera may be nil, but we skip the check for speed. Be careful.
  Result := InternalCamera.DoMoveAllowed(aPos,tmp,false)
end;
function DActorPhysics.CanMoveDir(const aDir: TVector3): boolean;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
var tmp: TVector3;
begin
  //InternalCamera may be nil, but we skip the check for speed. Be careful.
  Result := InternalCamera.DoMoveAllowed(InternalCamera.Position+aDir,tmp,false)
end;

{===========================================================================}
{========================== BASIC ACTOR ====================================}
{===========================================================================}

constructor DBasicActor.Create;
begin
  inherited Create;
  Nickname := 'abc';
  //setting some values to avoid uncertainity
  SetMaxMaxHP(100);
  SetMaxMaxSTA(100);
  SetMaxMaxCNC(100);
  SetMaxMaxMPH(100);
  DefenseRandom := TCastleRandom.Create; {$HINT read seed from the savegame}
  AttackRandom := TCastleRAndom.Create;
  ResetAll;
end;

{----------------------------------------------------------------------------}

destructor DBasicActor.Destroy;
begin
  FreeAndNil(DefenseRandom);
  FreeAndNil(AttackRandom);
  inherited Destroy;
end;

{----------------------------------------------------------------------------}

Procedure DBasicActor.SetHP(const Value: float);
begin
  If Value < fHP.Max then fHP.Current := Value else fHP.Current := fHP.Max;
  If fHP.Current < 0 then Die;
end;
Procedure DBasicActor.SetMaxHP(const Value: float);
begin
  If Value < fHP.MaxMax then fHP.Max := Value else fHP.Max := fHP.MaxMax;
  If fHP.Max < 0 then Die;
end;
Procedure DBasicActor.SetMaxMaxHP(const Value: float);
begin
  if fHP.MaxMax < Value then Heal(Value-fHP.MaxMax,1);
  fHP.MaxMax := Value;
  If fHP.MaxMax < 0 then Die;
end;
procedure DBasicActor.ResetHP;
begin
  fHP.Max := fHP.MaxMax;
  fHP.Current := fHP.Max;
end;

{---------------------------------------------------------------------------}

Procedure DBasicActor.SetSTA(const Value: float);
begin
  If Value < fSTA.Max then fSTA.Current := Value else fSTA.Current := fSTA.Max;
  If fSTA.Current < 0 then {EXAUSTED STATE};
end;
Procedure DBasicActor.SetMaxSTA(const Value: float);
begin
  If Value < fSTA.MaxMax then fSTA.Max := Value else fSTA.Max := fSTA.MaxMax;
  If fSTA.Max < 0 then {EXAUSTED STATE};
end;
Procedure DBasicActor.SetMaxMaxSTA(const Value: float);
begin
  if fSTA.MaxMax < Value then RestoreSTA(Value-fSTA.MaxMax,1);
  fSTA.MaxMax := Value;
  If fSTA.MaxMax < 0 then {EXAUSTED STATE};
end;
procedure DBasicActor.ResetSTA;
begin
  fSTA.Max := fSTA.MaxMax;
  fSTA.Current := fSTA.Max;
end;

{-----------------------------------------------------------------------------}

Procedure DBasicActor.SetCNC(const Value: float);
begin
  If Value < fCNC.Max then fCNC.Current := Value else fCNC.Current := fCNC.Max;
  If fCNC.Current < 0 then {BURN-OUT STATE};
end;
Procedure DBasicActor.SetMaxCNC(const Value: float);
begin
  If Value < fCNC.MaxMax then fCNC.Max := Value else fCNC.Max := fCNC.MaxMax;
  If fCNC.Max < 0 then {BURN-OUT STATE};
end;
Procedure DBasicActor.SetMaxMaxCNC(const Value: float);
begin
  if fCNC.MaxMax < Value then RestoreCNC(Value-fCNC.MaxMax,1);
  fCNC.MaxMax := Value;
  If fCNC.MaxMax < 0 then {BURN-OUT STATE};
end;
procedure DBasicActor.ResetCNC;
begin
  fCNC.Max := fCNC.MaxMax;
  fCNC.Current := fCNC.Max;
end;

{---------------------------------------------------------------------------}

Procedure DBasicActor.SetMPH(const Value: float);
begin
  If Value < fMPH.Max then fMPH.Current := Value else fMPH.Current := fMPH.Max;
  If fMPH.Current < 0 then {* STATE};
end;
Procedure DBasicActor.SetMaxMPH(const Value: float);
begin
  If Value < fMPH.MaxMax then fMPH.Max := Value else fMPH.Max := fMPH.MaxMax;
  If fMPH.Max < 0 then {* STATE};
end;
Procedure DBasicActor.SetMaxMaxMPH(const Value: float);
begin
  if fMPH.MaxMax < Value then RestoreMPH(Value-fMPH.MaxMax,1);
  fMPH.MaxMax := Value;
  If fMPH.MaxMax < 0 then {* STATE};
end;
procedure DBasicActor.ResetMPH;
begin
  fMPH.Max := fMPH.MaxMax;
  fMPH.Current := fMPH.Max;
end;

{---------------------------------------------------------------------------}

procedure DBasicActor.ResetAll;
begin
  ResetHP;
  ResetSTA;
  ResetCNC;
  ResetMPH;
end;

{---------------------------------------------------------------------------}

function DBasicActor.HPRef : PStatValue;
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

{---------------------------------------------------------------------------}

Procedure DBasicActor.Hit(const Damage: float; const Skill: float);
begin
  SetHP(HP-Damage);
  SetmaxHP(MaxHP-Damage*Skill); // todo
  if Assigned(Self.onHit) then Self.onHit(Damage, dtHealth);
end;

function DBasicActor.Heal(const Value: float; const Skill: float): boolean;
begin
  if (HP < MaxHP) or ((MaxHP < MaxMaxHP) and (Skill > 0)) then begin
    SetHP(HP+Value);
    SetMaxHP(MaxHP+Value*Skill); // todo
    Result := true;
  end else
    Result := false;
end;

{-----------------------------------------------------------------------------}

function DBasicActor.ConsumeSTA(const Consumption: float; const Skill: float): boolean;
begin
  if (STA > Consumption) then begin
    SetSTA(STA-Consumption);
    SetMaxSTA(MaxSTA-Consumption*Skill); // todo
    Result := true;
  end else Result := false;
end;
function DBasicActor.RestoreSTA(const Restoration: float; const Skill: float): boolean;
begin
  if (STA < MaxSTA) or ((MaxSTA < MaxMaxSTA) and (Skill > 0)) then begin
    SetSTA(STA+Restoration);
    SetMaxSTA(MaxSTA+Restoration*Skill); // todo
    Result := true;
  end else
    Result := false;
end;
procedure DBasicActor.DrainSTA(const Drain: float; const Skill: float);
begin
 SetSTA(STA-Drain);
 SetMaxSTA(MaxSTA-Drain*Skill); // todo
end;

{-----------------------------------------------------------------------------}

function DBasicActor.ConsumeCNC(const Consumption: float; const Skill: float): boolean;
begin
  if (CNC > Consumption) then begin
    SetCNC(CNC-Consumption);
    SetMaxCNC(MaxCNC-Consumption*Skill); // todo
    Result := true;
  end else Result := false;
end;
function DBasicActor.RestoreCNC(const Restoration: float; const Skill: float): boolean;
begin
  if (CNC < MaxCNC) or ((MaxCNC < MaxMaxCNC) and (Skill > 0)) then begin
    SetCNC(CNC+Restoration);
    SetMaxCNC(MaxCNC+Restoration*Skill); // todo
    Result := true;
  end else
    Result := false;
end;
procedure DBasicActor.DrainCNC(const Drain: float; const Skill: float);
begin
  SetCNC(CNC-Drain);
  SetMaxCNC(MaxCNC-Drain*Skill); // todo
end;

{-----------------------------------------------------------------------------}

function DBasicActor.ConsumeMPH(const Consumption: float; const Skill: float): boolean;
begin
  if (MPH > Consumption) then begin
    SetMPH(MPH-Consumption);
    SetMaxMPH(MaxMPH-Consumption*Skill); // todo
    Result := true;
  end else Result := false;
end;
function DBasicActor.RestoreMPH(const Restoration: float; const Skill: float): boolean;
begin
  if (MPH < MaxMPH) or ((MaxMPH < MaxMaxMPH) and (Skill > 0)) then begin
    SetMPH(MPH+Restoration);
    SetMaxMPH(MaxMPH+Restoration*Skill); // todo
    Result := true;
  end else
    Result := false;
end;
procedure DBasicActor.DrainMPH(const Drain: float; const Skill: float);
begin
  SetMPH(MPH-Drain);
  SetMaxMPH(MaxMPH-Drain*Skill); // todo
end;

{===========================================================================}
{========================== ACTOR BODY =====================================}
{===========================================================================}

constructor DActorBody.Create;
begin
  inherited Create;
  {$warning dummy, should set LastTime here}
end;

{-----------------------------------------------------------------------------}

destructor DActorBody.Destroy;
begin
  Window.SceneManager.Items.Remove(Body); //looks afwul
  FreeAndNil(Body);
  inherited Destroy;
end;

{-----------------------------------------------------------------------------}

procedure DActorBody.SpawnBodyHere(const SpawnBody: DBodyResource);
begin
  Body := DBody.Create(nil);   //No one owns Body, so we'll free it manually in DActor.destroy
  //SpawnBody.CreateCreature(Window.SceneManager.Items, Position, Direction);
  Body.Resource := SpawnBody;
  Window.SceneManager.Items.Add(Body);
  Body.Up := CurrentWorld.GetGravity(Body.Position); {$Warning sometimes it's not enough, why???}
  Visible := true;
  Body.Collides := true;
  Body.CollidesWithMoving := true;
  InternalCamera := Body.Camera;
  ForceAnimation(atIdle);
end;

{-----------------------------------------------------------------------------}

procedure DActorBody.Spawn(const aNav: TNavID; const SpawnBody: DBodyResource);
begin
  TeleportTo(aNav);
  SpawnBodyHere(SpawnBody);
end;
procedure DActorBody.Spawn(const aPosition: TVector3; const SpawnBody: DBodyResource);
begin
  TeleportTo(aPosition);
  SpawnBodyHere(SpawnBody);
end;

{-----------------------------------------------------------------------------}

procedure DActorBody.SetVisible(const Value: boolean);
begin
  Body.Exists := Value;
end;
function DActorBody.GetVisible: boolean;
begin
  Result := Body.Exists;
end;

{-----------------------------------------------------------------------------}

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

{-----------------------------------------------------------------------------}

procedure DActorBody.Manage;
begin
{  dt := DecoNow - LastT;
  dtl := DecoNowLocal - LastTLocal;
  LastT := DecoNow;
  LastTLocal := DecoNowLocal;}

  //cute and simple, maybe merge them?
  doAI;

  doRotate;
  doMove;

  if Body<>nil then begin
    Body.Position := Position;
    Body.Direction := Direction;
  end;
end;

{===========================================================================}
{====================== D ACTOR ============================================}
{===========================================================================}

function DActor.GetTarget: DCoordActor;
begin
  if fTarget = nil then
    GetEnemyTarget; //todo
    //WriteLnLog('DActor.GetTarget','Warning: Autoselecting target not implemented yet...');
  Result := fTarget;
end;

{-----------------------------------------------------------------------------}

function isEnemyFaction(const f1,f2: TFaction): boolean;
begin
  //todo
  if f1<>f2 then Result := true else Result := false;
end;
function isEnemy(const a1,a2: DSimpleActor): boolean;
begin
  if isEnemyFaction(a1.Faction,a2.Faction) then Result := true else Result := false;
end;
procedure DActor.GetEnemyTarget;
var a,e: DSimpleActor;
    d,d_min: float;
begin
  fTarget := nil;
  fTargetGroup := nil;
  //may be optimized by caching.
  if CurrentWorld is DAbstractWorld3d then

  d_min := -1;
  e := nil;
  for a in DAbstractWorld3d(CurrentWorld).Actors do
  {$hint dummy, actually this is wrong, as "target" may be a friendly unit, e.g. to heal, or in case of control loss}
  if (a<>Self) and (a is DBasicACtor) and isEnemy(Self,a) and CanSee(DCoordActor(a)) and (DBasicActor(a).HP>0) then begin
    d := (DCoordActor(a).Position - Self.Position).Length;
    if (d<Self.CombatRange) and ((d_min<d) or (d_min<0)) then begin
      d_min := d;
      e := a;
    end;
  end;
  fTarget := DCoordActor(e);
end;

{-----------------------------------------------------------------------------}

procedure DActor.RequestSoftPause;
begin
  {$warning todo}
  {$hint and player in THIS battle}
  if PlayerInBattle then
    SoftPause := 2*SoftPauseCoefficient; {request 0.5 seconds of pause for animation}
end;

{-----------------------------------------------------------------------------}

function DActor.CanSee(const a1: DCoordActor): boolean;
begin
  if (TVector3.DotProduct(Self.Direction,(a1.Position-Self.Position))>0)
     then Result := true else Result := false;
end;

{-----------------------------------------------------------------------------}

procedure DActor.PerformAction(const doAction: DMultiPerk);
begin
  if fTarget = nil then begin
    WriteLnLog('DActor.PreformAction','ERROR: Action was requested but no target specified...');
    Exit;
  end;

  if (fTarget is DBasicActor) and ((Position-fTarget.Position).Length<self.CombatRange) then begin
    Self.Animation(atAttack);
    Self.RequestSoftPause;
    DBasicActor(fTarget).Hit(10,1);
  end else WriteLnLog('DActor.PreformAction','ERROR: Trying to preform action on invalid actor...');
end;

{-----------------------------------------------------------------------------}

procedure DActor.LookAt;
begin
  if fTarget<>nil then
    LookAt(Target.Position)
  else
    WriteLnLog('DActor.LookAt','Warning: trying to look at a nil target...');
end;
procedure DActor.LookAt(const aPosition: TVector3);
begin
  toDir := aPosition - Position;
  toDir[2] := 0;  //cut-off z component
  FixZeroDirection;
  toDir.NormalizeMe;
end;

{-----------------------------------------------------------------------------}

procedure DActor.RecoverAll;
begin
  //dummy, should also reset all active statuses
  ResetAll;
end;

{-----------------------------------------------------------------------------}

constructor DActor.Create;
begin
  inherited Create;
  Actions := DPerksList.Create(false);
end;

{-----------------------------------------------------------------------------}

destructor DActor.Destroy;
begin
  FreeAndNil(Actions);
  inherited Destroy;
end;

{===========================================================================}
{=========================== D MONSTER =====================================}
{===========================================================================}

procedure DMonster.doAI;
begin
  {if the target is close enough look at it}
  if fTarget = nil then GetEnemyTarget;

  if fTarget<>nil then
    if ((fTarget.Position - Position).Length < Self.CombatRange) and (CanSee(fTarget)) then LookAt;

  if SoftPause>0 then exit;

  if fTarget<>nil then
    //tmp
    if drnd.Random<0.005 then Self.PerformAction(nil);

  //if drnd.Random<0.006 then self.Animation(atAttack);
  //if drnd.Random<0.002 then self.ForceAnimation(atDie);

  //body.Resource.Animations.FindName('Attack');
  //body.Sound3d();
  //body.ExecuteAction();

  //(body.Items[0] as TCastleScene).PlayAnimation('attack', paForceNotLooping);
  //Scene.AnimationTimeSensor('my_animation').EventIsActive.OnReceive.Add(@AnimationIsActiveChanged)
end;

{-----------------------------------------------------------------------------}

constructor DMonster.Create;
begin
  inherited Create;
  Self.onHit := @Self.doHit;
  Faction := fHostile;
end;

{-----------------------------------------------------------------------------}

procedure DMonster.doHit(const Dam: float; const Damtype: TDamageType);
begin
  if Self.HP > 0 then
    Self.ForceAnimation(atHurt)

  //and show numeric representation of Dam
  //? and negative status applied ?
end;

{-----------------------------------------------------------------------------}

procedure DMonster.Die;
begin
  Self.ForceAnimation(atDie);
end;




end.


