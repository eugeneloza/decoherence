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

uses Classes, fgl, CastleVectors,
  DecoActor, DecoRaceProfession, DecoPerks,
  DecoNavigationNetwork,
  DecoGlobal;

const MaxParty = 6; {0..6 = 7 characters}

{
  {Stores the perk's image. Generally we don't use perks directly as static images,
   but static images provide a convenient routine to load the image in a thread
   so let it be this way for now. Theoretically, it's better to make a separate
   object that handles perks and items images and allows just rescaling them
   correctly - to save memory. But it is not the issue for now.}
  Image: DStaticImage;
}

type
  {extension of DCoordActor with Up vector required for camera to work properly}
  DCameraMan = class(DActorPhysics)
  public
    Up: TVector3;
    theta,phi: float;
    procedure ResetUp;
    procedure ResetAngles;
    constructor Create; override;
  end;

type
  {player character - the most complex actor available :)}
  DPlayerCharacter = class(DActor)
  public
    Procedure Die; override;
    constructor Create; override;
    destructor Destroy; override;
end;

{list of the party characters}
type DCharList = specialize TFPGObjectList<DPlayerCharacter>;

type TMoveDirection = (mdForward,mdBack,mdLeft,mdRight);

type
  { Physical manifestation of Player in the world
    including camera
    party characters }
  DParty = class (TComponent)
  private
    const Speed = 10; {meters per second}
    const Friction = 40; {~meters per second, 0 never stops}
    const AngularFriction = 40; {~radians per second, rather hard to explain :) adds some inertion to camera, the higher this value the faster is rotation}
    const MouseSensivity = 1/1800;
  private
    {updates game camera with CameraMan coordinates}
    procedure UpdateCamera;
    procedure CollectCharacters;
  public
    CameraMan: DCameraMan;
    CameraInitialized: boolean;
    Character: DCharList;
    {generates a temporary party}
    procedure tmpParty;
    procedure Manage;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure TeleportTo(aPosition, aDirection: TVector3);
    procedure TeleportTo(aNav: TNavId; aDirection: TVector3);
    procedure TeleportTo(aNav: TNavId);
    procedure Rest;
  private
    {movepress may be not discrete for gamepad! remake to 0..1 or -1..1}
    MovePress: array [TMoveDirection] of boolean;
    Acceleration,MoveSpeed: TVector3;
    isAccelerating: boolean;
    procedure doMove1;
    procedure doMove2;
    procedure ResetMoveInput;
  public
    MouseLook: boolean;
    procedure InputMove(MoveDir: TMoveDirection);
    procedure InputRelease(MoveDir: TMoveDirection);
    procedure InputMouse(Delta: TVector2);
    procedure Stop;
  end;

{a list of player's parties}
type DPartyList = specialize TFPGObjectList<DParty>;

var Parties: DPartyList;
    CurrentParty: DParty;


{temporary to free the partyList}
procedure FreeParty;
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses SysUtils, CastleLog, Math,
  DecoNavigation, DecoAbstractWorld, DecoAbstractWorld3d,
  DecoMouse,
  DecoGameMode, DecoTime;

constructor DParty.create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  Character := DCharList.Create(true);
  CameraMan := DCameraMan.Create;
  isAccelerating := false;
  ResetMoveInput;
end;

{----------------------------------------------------------------------------}

Procedure DParty.tmpParty;
var i: integer;
    NewMember: DPlayerCharacter;
begin
  for i := 0 to MaxParty do begin
    NewMember := DPlayerCharacter.Create;
    NewMember.MaxMaxMPH := 0;
    NewMember.Hit(DRND.Random(80),1);
    NewMember.DrainCNC(DRND.Random(80),1);
    NewMember.DrainMPH(DRND.Random(80),1);
    NewMember.DrainSTA(DRND.Random(80),1);

    if (Perks=nil) or (Perks.Count=0) then WriteLnLog('CreateTestParty','FATAL ERROR: Perks is empty!');
    NewMember.Actions.Add(Perks[0]);

    Character.Add(NewMember);
  end;
end;

{----------------------------------------------------------------------------}

procedure DParty.Rest;
var c: DPlayerCharacter;
begin
  {for now it just resets the Actor to its initial state}
  for c in Character do c.RecoverAll;
end;

{----------------------------------------------------------------------------}

destructor DParty.Destroy;
begin
  FreeAndNil(Character); //it will auto free all its components
  FreeAndNil(CameraMan);
end;

{----------------------------------------------------------------------------}

procedure DParty.UpdateCamera;
var aFriction: float;
begin
  doMove1; doMove2;
  if Camera = nil then begin
    Exit;// InitNavigation;
    WriteLnLog('DParty.UpdateCamera','Camera is Nil!');
  end;

  Camera.Position := CameraMan.Position;
  Camera.Position[2] := Camera.Position[2] + CameraMan.Height;
  {soften climb/fall here?}

  if CameraInitialized then begin
    aFriction := AngularFriction*DeltaT;
    if aFriction>1 then aFriction := 1;
    Camera.Direction := (1-aFriction)*Camera.Direction+aFriction*CameraMan.Direction;
    Camera.Up := (1-aFriction)*Camera.Up + aFriction*CameraMan.Up;
  end else begin
    Camera.Direction := CameraMan.Direction;
    Camera.Up := CameraMan.Up;
    CameraInitialized := true;
  end;
end;

{----------------------------------------------------------------------------}

procedure DParty.Manage;
begin
  UpdateCamera;
  //teleport all characters to CameraMan position;
  CollectCharacters;
end;

{----------------------------------------------------------------------------}

procedure DParty.CollectCharacters;
var c: DPlayerCharacter;
begin
  for c in Character do begin
    c.Position := CameraMan.Position;
    c.Direction := CameraMan.Direction;
  end;
  {$hint collect battle state from char}
  PlayerInBattle := true;
end;

{----------------------------------------------------------------------------}

procedure DParty.TeleportTo(aPosition, aDirection: TVector3);
begin
  CameraMan.TeleportTo(aPosition, aDirection);
  CameraMan.ResetUp;
  CameraMan.Height := PlayerHeight*(CurrentWorld as DAbstractWorld3d).MyScale;
  CameraMan.InternalCamera := Camera;  {maybe, store camera in Controlled Party}

  CameraMan.ResetAngles;

  CameraInitialized := false;

  UpdateCamera;
end;
procedure DParty.TeleportTo(aNav: TNavId);
begin
  CameraMan.TeleportTo(aNav); //this is redundant, but blocking navNet would be more stable
  TeleportTo(CurrentWorld.NavToVector3(aNav),Vector3(0,1,0));
end;
procedure DParty.TeleportTo(aNav: TNavId; aDirection: TVector3);
begin
  CameraMan.TeleportTo(aNav);
  TeleportTo(CurrentWorld.NavToVector3(aNav),aDirection);
end;

{----------------------------------------------------------------------------}

procedure DParty.InputMove(MoveDir: TMoveDirection);
begin
  MovePress[MoveDir] := true;
  isAccelerating := MovePress[mdForward] or
                    MovePress[mdBack] or
                    MovePress[mdLeft] or
                    MovePress[mdRight];
end;

procedure DParty.InputRelease(MoveDir: TMoveDirection);
begin
  MovePress[MoveDir] := false;
  isAccelerating := MovePress[mdForward] or
                    MovePress[mdBack] or
                    MovePress[mdLeft] or
                    MovePress[mdRight];
end;

procedure DParty.ResetMoveInput;
begin
  MovePress[mdForward] := false;
  MovePress[mdBack] := false;
  MovePress[mdLeft] := false;
  MovePress[mdRight] := false;
end;

{----------------------------------------------------------------------------}

procedure DParty.InputMouse(Delta: TVector2);
var TraverseAxis: TVector3;
    UpVector,ForwardVector: TVector3;
begin
  {based on CastleCameras implementation}
  UpVector := Vector3(0,0,1);
  ForwardVector := Vector3(1,0,0);
  {rotate horizontal}
  CameraMan.Phi += -Delta[0]*MouseSensivity;
  if CameraMan.Phi> Pi then CameraMan.Phi -= 2*Pi else
  if CameraMan.Phi<-Pi then CameraMan.Phi += 2*Pi;
  CameraMan.Theta += Delta[1]*MouseSensivity;
  if CameraMan.Theta> Pi/3 then CameraMan.Theta :=  Pi/3 else
  if CameraMan.Theta<-Pi/3 then CameraMan.Theta := -Pi/3;

  CameraMan.Up := UpVector;
  CameraMan.Direction := RotatePointAroundAxisRad(CameraMan.Phi, ForwardVector, UpVector);
  TraverseAxis := TVector3.CrossProduct(CameraMan.Direction, UpVector);
  CameraMan.Direction := RotatePointAroundAxisRad(CameraMan.Theta, CameraMan.Direction, TraverseAxis);
  CameraMan.Up := RotatePointAroundAxisRad(CameraMan.Theta, CameraMan.Up, TraverseAxis);
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
  MoveVector := CameraMan.Direction;
  MoveVector[2] := 0;
  Acceleration := TVector3.Zero;
  if MovePress[mdForward] then Acceleration += MoveVector;
  if MovePress[mdBack]    then Acceleration += -MoveVector;
  if MovePress[mdLeft]    then Acceleration += Left90(MoveVector);
  if MovePress[mdRight]   then Acceleration += Right90(MoveVector);
  Acceleration.Normalize;
end;

{----------------------------------------------------------------------------}

procedure DParty.doMove2;
var NewPosGravity,tmp: TVector3;
    FixedFriction: float;
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
  CameraMan.Manage;

  {this is not a correct way to account for friction/innertia, but actually
   it doesn't really matter. We're not driving a car, it just adds some
   softness for movement and, maybe, provides for slippery ice surface}
  FixedFriction := Friction*DeltaTLocal;
  if FixedFriction > 1 then FixedFriction := 1;
  MoveSpeed := (1-FixedFriction)*MoveSpeed+FixedFriction*Acceleration;
  if not isAccelerating then Acceleration := TVector3.Zero;

  if not TryDirection(0,0) then
  if not TryDirection(1,0) then
  if not TryDirection(0,Pi/2) then
  if not TryDirection(0,-Pi/2) then ;

  {gravity...}
  {NewPos := CameraMan.Position;
  NewPos[2] := NewPos[2]-0.1;
  if Camera.DoMoveAllowed(NewPosGravity,tmp,false) then
    CameraMan.Position := NewPosGravity; }


  {use body here, including body.gravity}
end;

{----------------------------------------------------------------------------}

procedure DParty.Stop;
begin
  isAccelerating := false;
end;

{======================== DPlayerCharacter ==================================}

constructor DPlayerCharacter.Create;
begin
  inherited Create;
  Faction := fPlayer;
end;

{----------------------------------------------------------------------------}

Procedure DPlayerCharacter.Die;
begin
  WriteLnLog('DPlayerCharacter.die','Character has entered clinical death state');
end;

{----------------------------------------------------------------------------}

destructor DPlayerCharacter.Destroy;
begin
  Inherited Destroy;
end;

{============================================================================}

constructor DCameraMan.Create;
begin
  inherited Create;
  ResetUp;
  theta := 0;
  phi := 0;
end;

{----------------------------------------------------------------------------}

procedure DCameraMan.ResetUp;
begin
  Up := CurrentWorld.GetGravity(Position);
end;

{----------------------------------------------------------------------------}

procedure DCameraMan.ResetAngles;
begin
  Theta := ArcSin(Direction[2]/Direction.Length);
  Phi := Sign(Direction[1])*ArcCos(Direction[0]/(sqr(Direction[0])+sqr(Direction[1])));
  CenterMouseCursor;
end;

{============================================================================}

procedure FreeParty;
begin
  FreeAndNil(Parties);
end;

end.

