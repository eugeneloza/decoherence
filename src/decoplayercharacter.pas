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

{ Definitions and routines for player character and recruitable characters }
unit DecoPlayerCharacter;

{$INCLUDE compilerconfig.inc}

interface

uses Classes, Generics.Collections, CastleVectors,
  DecoActor, DecoRaceProfession, DecoPerks,
  DecoNavigationNetwork,
  DecoImages,
  DecoGlobal;

const MaxParty = 6; {0..6 = 7 characters}

type
  { Extension of DCoordActor with Up vector required for Camera to work properly }
  DCameraMan = class(DActorPhysics)
  public
    { CameraMan reqruies different handling of "up" vector }
    Up: TVector3;
    { Rotations of the CameraMan, required for smoother camera rotations }
    Theta, Phi: float;
    { Sets Up to GravityUp }
    procedure ResetUp;
    { Resets Theta and Phi to match CameraMan.Direction
          Also zeroes mouse coordinate shift }
    procedure ResetAngles;
    constructor Create; override;
  end;

{ at this point portraits are static }
type DPlayerPortrait = DStaticImage;

type
  {player character - the most complex actor available :)}
  DPlayerCharacter = class(DActor)
  public
    procedure doAI; override;//not sure, maybe, move AI higher? Or player will use AI?
    procedure Die; override;
    constructor Create; override;
    destructor Destroy; override;
end;

{ List of the party characters, a bit better than DActorList }
type DCharList = specialize TObjectList<DPlayerCharacter>;

{ todo: remake it }
type TMoveDirection = (mdForward,mdBack,mdLeft,mdRight);

type
  {Player party containing and managing a group of a few characters (no less than one)
     in a more convenient and extended way DActorGroup does}
  DParty = class(DActorGroup)
  private
    { Some day these will become variables / todo }
    { Speed in meters per second }
    const Speed = 10;
    { Speed fade ratio, in ~meters per second^2, 0 never stops }
    const Friction = 40;
    { Rotation fade ratio. ~radians per second, rather hard to explain :) adds some inertion to camera, the higher this value the faster is rotation}
    const AngularFriction = 40;
  private
    { Updates game camera with CameraMan coordinates }
    procedure UpdateCamera;
    { Teleports characters of this Group to rally point }
    procedure CollectCharacters;
  public
    { An imaginary Actor "holding" the camera }
    CameraMan: DCameraMan;
    { Is camera initialized? }
    CameraInitialized: boolean;
    { Characters in this Party }
    Character: DCharList;
    { Should be called each frame to process Camera stuff }
    procedure Manage;
    { Teleports party to some Position or Nav }
    procedure TeleportTo(const aPosition, aDirection: TVector3);
    procedure TeleportTo(const aNav: TNavId; const aDirection: TVector3);
    procedure TeleportTo(const aNav: TNavId);
    { Puts the party to sleep // all the parties available? }
    procedure Rest;

    constructor Create; //override;
    destructor Destroy; override;
  public
    { generates a temporary party / todo }
    procedure tmpParty;
  private
    { move to DActor}
    Acceleration, MoveSpeed: TVector3;
    procedure doMove1;
    procedure doMove2;
  end;

{ A list of player's parties }
type DPartyList = specialize TObjectList<DParty>;


type
  {Container for all player parties
     together with basic mouse/touch/keyboard input handle}
  DPlayerControl = class(DObject)
  public
    { Mouse sensivity }
    const MouseSensivity = 1/1800;
  public
    {A list of player's parties (should contain at least 1 record)}
    Parties: DPartyList;
    {Reference to current player party (the one controlled by the player at the moment)}
    CurrentParty: DParty;
  private
    isAccelerating: boolean;
    { movepress may be not discrete for gamepad! remake to 0..1 or -1..1 }
    MovePress: array [TMoveDirection] of boolean;
    { Resets all move input controllers }
    procedure ResetMoveInput;
  public
    { is MouseLook active (Desktop only) or DragLook is used }
    MouseLook: boolean;
    { Move in Direction pressed }
    procedure InputMove(const MoveDir: TMoveDirection);
    { Move in Direction released }
    procedure InputRelease(const MoveDir: TMoveDirection);
    { Mouse moved }
    procedure InputMouse(const Delta: TVector2);
    { Stop all movement }
    procedure Stop;
  public
    { Should be called each frame to process Camera stuff }
    procedure Manage;
    constructor Create;
    destructor Destroy; override;
  end;

var Player: DPlayerControl;

{ Temporary to init/free the PlayerControl }
procedure InitPlayer;
procedure FreePlayer;
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses SysUtils, Math,
  DecoNavigation, DecoAbstractWorld, DecoAbstractWorld3d,
  DecoInput,
  DecoGameMode, DecoTime, DecoLog, Profiler;

constructor DParty.Create;
begin
  StartProfiler;

  //inherited Create;
  Character := DCharList.Create(true);
  CameraMan := DCameraMan.Create;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

Procedure DParty.tmpParty;
var i: integer;
    NewMember: DPlayerCharacter;
begin
  StartProfiler;

  for i := 0 to MaxParty do begin
    NewMember := DPlayerCharacter.Create;
    NewMember.MaxMaxMPH := 0;
    NewMember.Hit(DRND.Random(80),1);
    NewMember.DrainCNC(DRND.Random(80),1);
    NewMember.DrainMPH(DRND.Random(80),1);
    NewMember.DrainSTA(DRND.Random(80),1);

    if (Perks = nil) or (Perks.Count = 0) then Log(logInit,_CurrentRoutine,'FATAL ERROR: Perks is empty!');
    NewMember.Actions.Add(Perks[0]);

    Character.Add(NewMember);
  end;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DParty.Rest;
var c: DPlayerCharacter;
begin
  StartProfiler;

  {for now it just resets the Actor to its initial state}
  for c in Character do c.RecoverAll;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

destructor DParty.Destroy;
begin
  StartProfiler;

  FreeAndNil(Character); //it will auto free all its components
  FreeAndNil(CameraMan);
  inherited Destroy;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DParty.UpdateCamera;
var aFriction: float;
begin
  StartProfiler;

  doMove1; doMove2;
  if Camera = nil then begin
    // InitNavigation;
    Log(LogNavigationError,_CurrentRoutine,'Camera is Nil!');
    Exit;
  end;

  Camera.Position := CameraMan.Position;
  Camera.Position[2] := Camera.Position[2] + CameraMan.Height;
  {soften climb/fall here?}

  if CameraInitialized then begin
    aFriction := AngularFriction*DeltaT;
    if aFriction>1 then aFriction := 1;
    Camera.Direction := (1-aFriction) * Camera.Direction + aFriction*CameraMan.Direction;
    Camera.Up := (1-aFriction) * Camera.Up + aFriction*CameraMan.Up;
  end else begin
    Camera.Direction := CameraMan.Direction;
    Camera.Up := CameraMan.Up;
    CameraInitialized := true;
  end;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DParty.Manage;
begin
  StartProfiler;

  UpdateCamera;
  //teleport all characters to CameraMan position;
  CollectCharacters;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DParty.CollectCharacters;
var c: DPlayerCharacter;
begin
  StartProfiler;

  for c in Character do begin
    c.Position := CameraMan.Position;
    c.Direction := CameraMan.Direction;
  end;
  {$hint collect battle state from char}
  PlayerInBattle := true;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DParty.TeleportTo(const aPosition, aDirection: TVector3);
begin
  StartProfiler;

  CameraMan.TeleportTo(aPosition, aDirection);
  CameraMan.ResetUp;
  CameraMan.Height := PlayerHeight*(CurrentWorld as DAbstractWorld3d).MyScale;
  CameraMan.InternalCamera := Camera;  {maybe, store camera in Controlled Party}

  CameraMan.ResetAngles;

  CameraInitialized := false;

  UpdateCamera;

  StopProfiler;
end;
procedure DParty.TeleportTo(const aNav: TNavId);
begin
  StartProfiler;

  CameraMan.TeleportTo(aNav); //this is redundant, but blocking navNet would be more stable
  TeleportTo(CurrentWorld.NavToVector3(aNav),Vector3(0,1,0));

  StopProfiler;
end;
procedure DParty.TeleportTo(const aNav: TNavId; const aDirection: TVector3);
begin
  StartProfiler;

  CameraMan.TeleportTo(aNav);
  TeleportTo(CurrentWorld.NavToVector3(aNav),aDirection);

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DParty.doMove1;
var MoveVector: TVector3;
  function Right90(var v: TVector3): TVector3; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
  begin
    Result[0] :=  v[1];
    Result[1] := -v[0];
    Result[2] :=  v[2];
  end;
  function Left90(var v: TVector3): TVector3; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
  begin
    Result[0] := -v[1];
    Result[1] :=  v[0];
    Result[2] :=  v[2];
  end;
begin
  StartProfiler;

  MoveVector := CameraMan.Direction;
  MoveVector[2] := 0;
  Acceleration := TVector3.Zero;

  if Player.MovePress[mdForward] then Acceleration += MoveVector;
  if Player.MovePress[mdBack]    then Acceleration += -MoveVector;
  if Player.MovePress[mdLeft]    then Acceleration += Left90(MoveVector);
  if Player.MovePress[mdRight]   then Acceleration += Right90(MoveVector);
  Acceleration.Normalize;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DParty.doMove2;
var FixedFriction: float;
  function TryDirection(climb,slide: float): boolean;
  var NewPosAdjusted, NewPosHeightAdjusted, ProposedDir: TVector3;
  begin
    NewPosAdjusted := CameraMan.Position + (Speed*DeltaTLocal)*RotatePointAroundAxisRad(slide, MoveSpeed, vector3(0,0,1));
    NewPosAdjusted[2] := NewPosAdjusted[2] + climb;
    NewPosHeightAdjusted := NewPosAdjusted;
    NewPosHeightAdjusted[2] := NewPosHeightAdjusted[2]+CameraMan.Height; {ugly fix for difference in CameraMan height}
    if Camera.DoMoveAllowed(NewPosHeightAdjusted,ProposedDir,false) then begin
      CameraMan.Position := NewPosAdjusted;
      Result := true;
    end else
      Result := false;

  end;
begin
  StartProfiler;

  CameraMan.Manage;

  {this is not a correct way to account for friction/innertia, but actually
   it doesn't really matter. We're not driving a car, it just adds some
   softness for movement and, maybe, provides for slippery ice surface}
  FixedFriction := Friction*DeltaTLocal;
  if FixedFriction > 1 then FixedFriction := 1;
  MoveSpeed := (1-FixedFriction)*MoveSpeed+FixedFriction*Acceleration;
  if not Player.isAccelerating then Acceleration := TVector3.Zero;

  {todo: extremely ugly wall/stairs sliding algorithm }
  if not TryDirection(0,0) then
  if not TryDirection(1,0) then
  if not TryDirection(0,Pi/2) then
  if not TryDirection(0,-Pi/2) then ;

  StopProfiler;
end;

{======================== DPlayerCharacter ==================================}

constructor DPlayerCharacter.Create;
begin
  StartProfiler;

  inherited Create;
  isPlayer := true;
  Faction := ffPlayer;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

Procedure DPlayerCharacter.Die;
begin
  StartProfiler;

  Log(LogTemp,_CurrentRoutine,'Character has entered clinical death state');

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DPlayerCharacter.doAI;
begin
  StartProfiler;

  //inherited doAI; <---------- player characters have unique AI
  //just do nothing for now;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

destructor DPlayerCharacter.Destroy;
begin
  StartProfiler;

  inherited Destroy;

  StopProfiler;
end;

{============================================================================}

constructor DCameraMan.Create;
begin
  StartProfiler;

  inherited Create;
  ResetUp;
  Theta := 0;
  phi := 0;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DCameraMan.ResetUp;
begin
  StartProfiler;

  Up := CurrentWorld.GetGravity(Position);

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DCameraMan.ResetAngles;
begin
  StartProfiler;

  Theta := ArcSin(Direction[2]/Direction.Length);
  Phi := Sign(Direction[1])*ArcCos(Direction[0]/(sqr(Direction[0])+sqr(Direction[1])));
  CenterMouseCursor;

  StopProfiler;
end;

{============================================================================}

constructor DPlayerControl.Create;
begin
  StartProfiler;

  //inherited Create;
  Parties := DPartyList.Create(true);
  isAccelerating := false;
  CurrentParty := DParty.Create;
  CurrentParty.tmpParty;
  Parties.Add(CurrentParty);
  ResetMoveInput;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

destructor DPlayerControl.Destroy;
begin
  StartProfiler;

  FreeAndNil(Parties);
  inherited Destroy;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DPlayerControl.InputMouse(const Delta: TVector2);
var TraverseAxis: TVector3;
    UpVector,ForwardVector: TVector3;
begin
  StartProfiler;

  begin
    {based on CastleCameras implementation}
    UpVector := Vector3(0,0,1);
    ForwardVector := Vector3(1,0,0);
    {rotate horizontal}
    CurrentParty.CameraMan.Phi += -Delta[0]*MouseSensivity;
    if CurrentParty.CameraMan.Phi> Pi then CurrentParty.CameraMan.Phi -= 2*Pi else
    if CurrentParty.CameraMan.Phi<-Pi then CurrentParty.CameraMan.Phi += 2*Pi;
    CurrentParty.CameraMan.Theta += Delta[1]*MouseSensivity;
    if CurrentParty.CameraMan.Theta> Pi/3 then CurrentParty.CameraMan.Theta :=  Pi/3 else
    if CurrentParty.CameraMan.Theta<-Pi/3 then CurrentParty.CameraMan.Theta := -Pi/3;

    CurrentParty.CameraMan.Up := UpVector;
    CurrentParty.CameraMan.Direction := RotatePointAroundAxisRad(CurrentParty.CameraMan.Phi, ForwardVector, UpVector);
    TraverseAxis := TVector3.CrossProduct(CurrentParty.CameraMan.Direction, UpVector);
    CurrentParty.CameraMan.Direction := RotatePointAroundAxisRad(CurrentParty.CameraMan.Theta, CurrentParty.CameraMan.Direction, TraverseAxis);
    CurrentParty.CameraMan.Up := RotatePointAroundAxisRad(CurrentParty.CameraMan.Theta, CurrentParty.CameraMan.Up, TraverseAxis);
  end;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DPlayerControl.Manage;
begin
  StartProfiler;

  {todo}
  CurrentParty.Manage;

  StopProfiler;
end;


{----------------------------------------------------------------------------}

procedure DPlayerControl.InputMove(const MoveDir: TMoveDirection);
begin
  StartProfiler;

  MovePress[MoveDir] := true;
  isAccelerating := MovePress[mdForward] or
                    MovePress[mdBack] or
                    MovePress[mdLeft] or
                    MovePress[mdRight];
  StopProfiler;
end;

procedure DPlayerControl.InputRelease(const MoveDir: TMoveDirection);
begin
  StartProfiler;
  MovePress[MoveDir] := false;
  isAccelerating := MovePress[mdForward] or
                    MovePress[mdBack] or
                    MovePress[mdLeft] or
                    MovePress[mdRight];

  StopProfiler;
end;

procedure DPlayerControl.ResetMoveInput;
var
  I: TMoveDirection;
begin
  StartProfiler;

  for I := Low(TMoveDirection) to High(TMoveDirection) do
    MovePress[I] := false;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DPlayerControl.Stop;
begin
  StartProfiler;
  isAccelerating := false;
  StopProfiler;
end;

{============================================================================}

procedure InitPlayer;
begin
  StartProfiler;
  Player := DPlayerControl.Create;
  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure FreePlayer;
begin
  StartProfiler;
  FreeAndNil(Player);
  StopProfiler;
end;

end.

