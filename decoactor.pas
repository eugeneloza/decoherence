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
  DecoNavigationNetwork,
  DecoStats, DecoPerks,
  DecoGlobal;

type TDamageType = (dtHealth);
type TDamageProcedure = procedure (Dam: float; Damtype: TDamageType) of Object;

type DBody = TCreatureResource;

type
  { This Actor has only the most basic features like his "tile" position
    Will be used in some remote future for Actors behaviour on global map }
  DSimpeActor = class (TObject)
  public
  end;

type
  { Actor with full World coordinates,
    Mostly needed as a Target for AI
    and Base for Player camera mangement }
  DCoordActor = class (DSimpeActor)
  public
    {position, direction and rotate_to_direction of the Body}
    Position,Direction: TVector3;
    procedure TeleportTo(aPosition: TVector3);
    procedure TeleportTo(aPosition, aDirection: TVector3);
  protected
    toPos,toDir: TVector3;
    {initializes direction with a random value}
    procedure GetRandomDirection;
    {rotates the body}
    procedure doRotate;
    {moves the body}
    procedure doMove;
  end;

type
  { Actor with a rendered body and corresponding management routines }
  DActorBody = class (DCoordActor)
  protected
    {last time, last local time, and corresponding (temporary) deltas}
    LastT, dt, LastTLocal, dtl: DTime;
    {speed of actor's rotation}
    RotSpeed: float;
  protected
    procedure doAI; virtual;
  private
    procedure SetVisible(value: boolean);
    function GetVisible: boolean;
  public
    { 3D body of this Actor, it may be nil (in case the body is unloaded from
      RAM or belongs to an body-less entity, like Player's character at the moment }
    Body: TCreature;
    { Shows or hides the actor's body}
    property Visible: boolean read GetVisible write SetVisible;
    { Spawns a body for the Actor, overriden in children to spawn attributes}
    procedure Spawn(aNav: TNavID; SpawnBody: DBody);
    procedure Spawn(aPosition: TVector3single; SpawnBody: DBody); virtual;
    { Manages this actor, e.g. preforms AI }
    procedure Manage; virtual;

    destructor Destroy; override;
    constructor Create; virtual;
  end;

type TActorList = specialize TFPGObjectList<DActorBody>;

Type
  { basic actor. With stats }
  DBasicActor = class(DActorBody)
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
    procedure ResetAll;

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
    { these are randoms for the actor: defense gives his defense rolls,
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
  private
    fTarget: DCoordActor;
    function GetTarget: DCoordActor;
  protected
    procedure LookAt;
    procedure LookAt(aPosition: TVector3);
  public
    Actions: DPerksList;
    {used for AI and preforming actions
     if target is nil any valid target is selected}
    property Target: DCoordActor read GetTarget write fTarget;
    {rests all stats and removes all effects}
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
  public
    //procedure Manage; override;
  end;



var tmpKnightCreature: DBody;

procedure tmpLoadKnightCreature;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses SysUtils, CastleLog,
  CastleFilesUtils, DecoInputOutput,

  DecoAbstractWorld,
  DecoNavigation{?}, CastleScene, CastleSceneCore;

constructor DBasicActor.Create;
begin
  inherited;
  Nickname := 'abc';
  SetMaxMaxHP(100);
  SetMaxMaxSTA(100);
  SetMaxMaxCNC(100);
  SetMaxMaxMPH(100);
  DefenseRandom := TCastleRandom.Create; {$HINT read seed from the savegame}
  AttackRandom := TCastleRAndom.Create;
  ResetHP;
  ResetSTA;
  ResetCNC;
  ResetMPH;
end;

destructor DBasicActor.Destroy;
begin
  FreeAndNil(DefenseRandom);
  FreeAndNil(AttackRandom);
  inherited;
end;

{----------------------------------------------------------------------------}

Procedure DBasicActor.SetHP(Value: float);
begin
  If Value < fMaxHP then fHP := Value else fHP := fMaxHP;
  If Value < 0 then Die;     {$HINT clinical death state}
end;
Procedure DBasicActor.SetMaxHP(Value: float);
begin
  If Value < fMaxMaxHP then fMaxHP := Value else fMaxHP := fMaxMaxHP;
  If Value < 0 then Die;
end;
Procedure DBasicActor.SetMaxMaxHP(Value: float);
begin
  if MaxMaxHP < Value then Heal(Value-fMaxMaxHP,1);
  fMaxMaxHP := Value;
  If Value < 0 then Die;
end;
procedure DBasicActor.ResetHP;
begin
  fMaxHP := fMaxMaxHP;
  fHP := fMaxHP;
end;

{---------------------------------------------------------------------------}

Procedure DBasicActor.SetSTA(Value: float);
begin
  If Value < fMaxSTA then fSTA := Value else fSTA := fMaxSTA;
  If Value < 0 then {EXAUSTED STATE};
end;
Procedure DBasicActor.SetMaxSTA(Value: float);
begin
  If Value < fMaxMaxSTA then fMaxSTA := Value else fMaxSTA := fMaxMaxSTA;
  If Value < 0 then {EXAUSTED STATE};
end;
Procedure DBasicActor.SetMaxMaxSTA(Value: float);
begin
  if MaxMaxSTA < Value then RestoreSTA(Value-fMaxMaxSTA,1);
  fMaxMaxSTA := Value;
  If Value < 0 then {EXAUSTED STATE};
end;
procedure DBasicActor.ResetSTA;
begin
  fMaxSTA := fMaxMaxSTA;
  fSTA := fMaxSTA;
end;

{-----------------------------------------------------------------------------}

Procedure DBasicActor.SetCNC(Value: float);
begin
  If Value < fMaxCNC then fCNC := Value else fCNC := fMaxCNC;
  If Value < 0 then {BURN-OUT STATE};
end;
Procedure DBasicActor.SetMaxCNC(Value: float);
begin
  If Value < fMaxMaxCNC then fMaxCNC := Value else fMaxCNC := fMaxMaxCNC;
  If Value < 0 then {BURN-OUT STATE};
end;
Procedure DBasicActor.SetMaxMaxCNC(Value: float);
begin
  if MaxMaxCNC < Value then RestoreCNC(Value-fMaxMaxCNC,1);
  fMaxMaxCNC := Value;
  If Value < 0 then {BURN-OUT STATE};
end;
procedure DBasicActor.ResetCNC;
begin
  fMaxCNC := fMaxMaxCNC;
  fCNC := fMaxCNC;
end;

{---------------------------------------------------------------------------}

Procedure DBasicActor.SetMPH(Value: float);
begin
  If Value < fMaxMPH then fMPH := Value else fMPH := fmaxMPH;
  If Value < 0 then {* STATE};
end;
Procedure DBasicActor.SetMaxMPH(Value: float);
begin
  If Value < fMaxmaxMPH then fMaxMPH := Value else fMaxMPH := fMaxmaxMPH;
  If Value < 0 then {* STATE};
end;
Procedure DBasicActor.SetMaxMaxMPH(Value: float);
begin
  if MaxMaxMPH < Value then RestoreMPH(Value-fMaxMaxMPH,1);
  fMaxMaxMPH := Value;
  If Value < 0 then {* STATE};
end;
procedure DBasicActor.ResetMPH;
begin
  fMaxMPH := fMaxMaxMPH;
  fMPH := fMaxMPH;
end;

procedure DBasicActor.ResetAll;
begin
  ResetHP;
  ResetSTA;
  ResetCNC;
  ResetMPH;
end;

{---------------------------------------------------------------------------}

Procedure DBasicActor.Hit(Damage: float; Skill: float);
begin
  SetHP(HP-Damage);
  SetmaxHP(MaxHP-Damage*Skill); // todo
  if Assigned(Self.onHit) then Self.onHit(Damage, dtHealth);
end;

function DBasicActor.Heal(Value: float; Skill: float): boolean;
begin
  if (HP < MaxHP) or ((MaxHP < MaxMaxHP) and (Skill > 0)) then begin
    SetHP(HP+Value);
    SetMaxHP(MaxHP+Value*Skill); // todo
    Result := true;
  end else
    Result := false;
end;

{-----------------------------------------------------------------------------}

function DBasicActor.ConsumeSTA(Consumption: float; Skill: float): boolean;
begin
  if (STA > Consumption) then begin
    SetSTA(STA-Consumption);
    SetMaxSTA(MaxSTA-Consumption*Skill); // todo
    Result := true;
  end else Result := false;
end;
function DBasicActor.RestoreSTA(Restoration: float; Skill: float): boolean;
begin
  if (STA < MaxSTA) or ((MaxSTA < MaxMaxSTA) and (Skill > 0)) then begin
    SetSTA(STA+Restoration);
    SetMaxSTA(MaxSTA+Restoration*Skill); // todo
    Result := true;
  end else
    Result := false;
end;
procedure DBasicActor.DrainSTA(Drain: float; Skill: float);
begin
 SetSTA(STA-Drain);
 SetMaxSTA(MaxSTA-Drain*Skill); // todo
end;

{-----------------------------------------------------------------------------}

function DBasicActor.ConsumeCNC(Consumption: float; Skill: float): boolean;
begin
  if (CNC > Consumption) then begin
    SetCNC(CNC-Consumption);
    SetMaxCNC(MaxCNC-Consumption*Skill); // todo
    Result := true;
  end else Result := false;
end;
function DBasicActor.RestoreCNC(Restoration: float; Skill: float): boolean;
begin
  if (CNC < MaxCNC) or ((MaxCNC < MaxMaxCNC) and (Skill > 0)) then begin
    SetCNC(CNC+Restoration);
    SetMaxCNC(MaxCNC+Restoration*Skill); // todo
    Result := true;
  end else
    Result := false;
end;
procedure DBasicActor.DrainCNC(Drain: float; Skill: float);
begin
  SetCNC(CNC-Drain);
  SetMaxCNC(MaxCNC-Drain*Skill); // todo
end;

{-----------------------------------------------------------------------------}

function DBasicActor.ConsumeMPH(Consumption: float; Skill: float): boolean;
begin
  if (MPH > Consumption) then begin
    SetMPH(MPH-Consumption);
    SetMaxMPH(MaxMPH-Consumption*Skill); // todo
    Result := true;
  end else Result := false;
end;
function DBasicActor.RestoreMPH(Restoration: float; Skill: float): boolean;
begin
  if (MPH < MaxMPH) or ((MaxMPH < MaxMaxMPH) and (Skill > 0)) then begin
    SetMPH(MPH+Restoration);
    SetMaxMPH(MaxMPH+Restoration*Skill); // todo
    Result := true;
  end else
    Result := false;
end;
procedure DBasicActor.DrainMPH(Drain: float; Skill: float);
begin
  SetMPH(MPH-Drain);
  SetMaxMPH(MaxMPH-Drain*Skill); // todo
end;

{========================== ACTOR BODY =====================================}

constructor DActorBody.Create;
begin
  inherited;
  {$warning dummy, should set LastTime here}
end;

{-----------------------------------------------------------------------------}

destructor DActorBody.Destroy;
begin
  //body is freed automatically (IF IT HAS BEEN ADDED TO SceneManager)
  inherited;
end;

{-----------------------------------------------------------------------------}

procedure DActorBody.Spawn(aNav: TNavID; SpawnBody: DBody);
begin
  Spawn(CurrentWorld.NavToVector3(aNav),SpawnBody);
  CurrentWorld.BlockNav(aNav);
end;
procedure DActorBody.Spawn(aPosition: TVector3; SpawnBody: DBody);
begin
  TeleportTo(aPosition);
  Body := SpawnBody.CreateCreature(Window.SceneManager.Items, Position, Direction);
  Body.Up := Vector3(0,0,1); {$Warning sometimes it's not enough, why???}
  Visible := true;
end;

procedure DActorBody.SetVisible(Value: boolean);
begin
  Body.Exists := Value;
end;

function DActorBody.GetVisible: boolean;
begin
  Result := Body.Exists;
end;


{-----------------------------------------------------------------------------}

procedure DActorBody.doAI;
begin
  //does nothing yet, maybe should be abstract?
end;

{-----------------------------------------------------------------------------}

constructor DActor.create;
begin
  inherited;
  Actions := DPerksList.Create(false);
end;

{-----------------------------------------------------------------------------}

destructor DActor.destroy;
begin
  FreeAndNil(Actions);
  inherited;
end;

{-----------------------------------------------------------------------------}

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

procedure DCoordActor.TeleportTo(aPosition: TVector3);
begin
  GetRandomDirection;
  Position := aPosition;
  toPos := Position;
end;
procedure DCoordActor.TeleportTo(aPosition, aDirection: TVector3);
begin
  Direction := aDirection;
  toDir := Direction;
  Position := aPosition;
  toPos := Position;
end;

{-----------------------------------------------------------------------------}

procedure DActorBody.Manage;
begin
  dt := DecoNow - LastT;
  dtl := DecoNowLocal - LastTLocal;
  LastT := DecoNow;
  LastTLocal := DecoNowLocal;

  //cute and simple, maybe merge them?
  doAI;

  doRotate;
  doMove;

  if Body<>nil then begin
    Body.Position := Position;
    Body.Direction := Direction;
  end;
end;

{-----------------------------------------------------------------------------}

procedure DMonster.doAI;
begin
  {if the target is close enough look at it}
  if fTarget<>nil then
    if (fTarget.Position - Position).Length < 10 then LookAt;

  //(body.Items[0] as TCastleScene).PlayAnimation('attack', paForceNotLooping);
  //Scene.AnimationTimeSensor('my_animation').EventIsActive.OnReceive.Add(@AnimationIsActiveChanged)
end;

{-----------------------------------------------------------------------------}

function DActor.GetTarget: DCoordActor;
begin
  if fTarget = nil then begin
    Result := nil;
    WriteLnLog('DActor.GetTarget','Warning: Autoselecting target not implemented yet...');
  end
  else
    Result := fTarget;
end;

{-----------------------------------------------------------------------------}

procedure DActor.LookAt;
begin
  if fTarget<>nil then
    LookAt(Target.Position)
  else
    WriteLnLog('DActor.LookAt','Warning: trying to look at a nil target...');
end;
procedure DActor.LookAt(aPosition: TVector3);
begin
  toDir := aPosition - Position;
  toDir[2] := 0;  //cut-off z component
  toDir.NormalizeMe;
end;

{-----------------------------------------------------------------------------}

procedure DActor.RecoverAll;
begin
  //dummy, should also reset all active statuses
  ResetAll;
end;

{-----------------------------------------------------------------------------}

procedure tmpLoadKnightCreature;
begin
  Resources.LoadSafe(ApplicationData('models/creatures/knight_creature/'));
  tmpKnightCreature := Resources.FindName('Knight') as TCreatureResource;
end;




end.


