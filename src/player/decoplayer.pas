{ Copyright (C) 2012-2018 Yevhen Loza

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

{---------------------------------------------------------------------------}

(* Handling camera and movement of player-controlled party *)

unit DecoPlayer;

{$INCLUDE compilerconfig.inc}

interface

uses
  CastleVectors,
  DecoCameraMan,
  DecoGlobal;

type
  { Handling of player movement }
  DPlayerControl = class(DObject)
  strict private
    {}
    CameraMan: DCameraMan;
    {}
    AccelerationForward, AccelerationStrafe: DFloat;
  strict private
    {}
    procedure doMove;
  public
    //CurrentParty
    { Change CameraMan look direction }
    procedure doLook(const Delta: TVector2);
    { Change CameraMan acceleration }
    procedure doAccelerateForward(const Value: DFloat);
    procedure doAccelerateStrafe(const Value: DFloat);
  public
    procedure Manage;
    constructor Create; //override;
    destructor Destroy; override;
  end;

var
  { Player input and camera }
  Player: DPlayerControl;

{ Creates and initializes Player instance }
procedure InitPlayer;
{ Frees Player }
procedure FreePlayer;
{.......................................................................}
implementation
uses
  CastleKeysMouse,
  //DecoInput,
  DecoMath;

procedure DPlayerControl.doAccelerateForward(const Value: DFloat);
begin
  AccelerationForward := Value;
end;

{----------------------------------------------------------------------------}

procedure DPlayerControl.doAccelerateStrafe(const Value: DFloat);
begin
  AccelerationStrafe := Value;
end;

{----------------------------------------------------------------------------}

procedure DPlayerControl.doMove;
var
  InputAccelerationForward, InputAccelerationStrafe: DFloat;
begin

  if Abs(AccelerationForward) > 1 then
    InputAccelerationForward := Sign(AccelerationForward)
  else
    InputAccelerationForward := AccelerationForward;

  if Abs(AccelerationStrafe) > 1 then
    InputAccelerationStrafe := Sign(AccelerationStrafe)
  else
    InputAccelerationStrafe := AccelerationStrafe;

end;

{----------------------------------------------------------------------------}

procedure DPlayerControl.doLook(const Delta: TVector2);
begin
  //todo
end;


{----------------------------------------------------------------------------}

procedure DPlayerControl.Manage;
begin
  doMove;
end;

{----------------------------------------------------------------------------}

constructor DPlayerControl.Create;
begin
  //inherited Create
  CameraMan := DCameraMan.Create;
end;

{----------------------------------------------------------------------------}

destructor DPlayerControl.Destroy;
begin
  CameraMan.Free;
  inherited Destroy;
end;

{............................................................................}

procedure InitPlayer;
begin
  Player := DPlayerControl.Create;
end;

{----------------------------------------------------------------------------}

procedure FreePlayer;
begin
  Player.Free; //not sure, maybe FreeAndNil to be safe?
end;

end.

