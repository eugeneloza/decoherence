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
unit decoplayercharacter;

{$INCLUDE compilerconfig.inc}

interface

uses Classes, fgl, CastleVectors,
  DecoActor, DecoRaceProfession, decoperks,
  DecoNavigationNetwork,
  decoglobal;

const MaxParty = 6; {0..6 = 7 characters}

{
  {Stores the perk's image. Generally we don't use perks directly as static images,
   but static images provide a convenient routine to load the image in a thread
   so let it be this way for now. Theoretically, it's better to make a separate
   object that handles perks and items images and allows just rescaling them
   correctly - to save memory. But it is not the issue for now.}
  Image: DStaticImage;
}

Type
  {player character - the most complex actor available :)}
  DPlayerCharacter = class(DActor)
  public
    Procedure Die; override;
    constructor Create; override;
    destructor Destroy; override;
end;

{list of the party characters}
type DCharList = specialize TFPGObjectList<DPlayerCharacter>;

type
  { Physical manifestation of Player in the world
    including camera
    party characters }
  DParty = class (TComponent)
  private
    {updates game camera with CameraMan coordinates}
    procedure UpdateCamera;
    procedure CollectCharacters;
  public
    CameraMan: DCoordActor;
    Char: DCharList;
    {generates a temporary party}
    procedure tmpParty;
    procedure Manage;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure TeleportTo(aPosition, aDirection: TVector3);
    procedure TeleportTo(aNav: TNavId; aDirection: TVector3);
    procedure TeleportTo(aNav: TNavId);
    procedure Rest;
  end;

var Party: DParty;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses SysUtils, CastleLog,
  DecoNavigation, DecoAbstractWorld, DecoAbstractWorld3d;

constructor DParty.create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Char := DCharList.Create(true);
  CameraMan := DCoordActor.Create;
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

    Char.Add(NewMember);
  end;
end;

{----------------------------------------------------------------------------}

procedure DParty.Rest;
var c: DPlayerCharacter;
begin
  {for now it just resets the Actor to its initial state}
  for c in char do c.RecoverAll;
end;

{----------------------------------------------------------------------------}

destructor DParty.Destroy;
begin
  FreeAndNil(Char); //it will auto free all its components
  FreeAndNil(CameraMan);
end;

{----------------------------------------------------------------------------}

procedure DParty.UpdateCamera;
begin
  if Camera = nil then Exit;// InitNavigation;
  Camera.Position := CameraMan.Position;
  Camera.Direction := CameraMan.Direction;
end;

{----------------------------------------------------------------------------}

procedure DParty.Manage;
begin
  //UpdateCamera;
  {actually we're doing the very opposite now:}
  CameraMan.Position := Camera.Position;
  CameraMan.Direction := Camera.Direction;
  //teleport all characters to CameraMan position;
  CollectCharacters;
end;

{----------------------------------------------------------------------------}

procedure DParty.CollectCharacters;
var p: DPlayerCharacter;
begin
  for p in Char do begin
    p.Position := CameraMan.Position;
    p.Direction := CameraMan.Direction;
  end;
end;

{----------------------------------------------------------------------------}

procedure DParty.TeleportTo(aPosition, aDirection: TVector3);
begin
  CameraMan.TeleportTo(aPosition, aDirection);
  if Camera = nil then Raise Exception.Create('Camrea is nil!');//InitNavigation;

  CameraMan.Position[2] := CameraMan.Position[2]+PlayerHeight*(CurrentWorld as DAbstractWorld3d).MyScale;

  {$Hint Do it only once per World!}
  Camera.MoveSpeed := 1*(CurrentWorld as DAbstractWorld3d).WorldScale;
  Camera.PreferredHeight := PlayerHeight*(CurrentWorld as DAbstractWorld3d).MyScale;

  //make it a "reset gravity"? and call at every nav change?
  Camera.GravityUp := CurrentWorld.GetGravity(CameraMan.Position); //can't disable Camera.Gravity yet, some day it will be overtaken by Actor.Gravity
  Camera.Up := CurrentWorld.GetGravity(CameraMan.Position);

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

{======================== DPlayerCharacter ==================================}

constructor DPlayerCharacter.Create;
begin
  inherited;
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
  Inherited;
end;

end.

