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
  DecoGlobal;

type
  DMoveKey = (KeyboardForward, KeyboardBackward, KeyboardStrafeLeft, KeyboardStrafeRight);

type
  { Handling of player movement }
  DPlayerControl = class(DObject)
  private
    MoveKeys: array[DMoveKey] of boolean;
    procedure doMove;
  public
    //CurrentParty
    //CameraMman
    MouseLook: boolean;
    { Reset all input controls to initial values }
    procedure ReleaseControls;
    { Keyboard movement }
    procedure MoveKeyPress(const aKey: DMoveKey);
    procedure MoveKeyRelease(const aKey: DMoveKey);
    { Turn MouseLook on/off }
    procedure ToggleMouseLook;
  public
    procedure Manage;
    constructor Create; //override;
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
  DecoInput,
  DecoMath;

procedure DPlayerControl.ReleaseControls;
var
  k: DMoveKey;
begin
  for k in DMoveKey do
    MoveKeys[k] := false;
end;

{----------------------------------------------------------------------------}

procedure DPlayerControl.MoveKeyPress(const aKey: DMoveKey);
begin
  MoveKeys[aKey] := true;
end;

{----------------------------------------------------------------------------}

procedure DPlayerControl.MoveKeyRelease(const aKey: DMoveKey);
begin
  MoveKeys[aKey] := false;
end;

{----------------------------------------------------------------------------}

procedure DPlayerControl.doMove;
var
  InputAccelerationForward, InputAccelerationStrafe: DFloat;
begin
  //collect all sources of possible movement and adjust CameraMan position and location
  InputAccelerationForward := 0;
  InputAccelerationStrafe := 0;
  //get keyboard input
  if MoveKeys[KeyboardForward] then
    InputAccelerationForward += 1;
  if MoveKeys[KeyboardBackward] then
    InputAccelerationForward += -1;
  if MoveKeys[KeyboardStrafeLeft] then
    InputAccelerationStrafe += 1;
  if MoveKeys[KeyboardStrafeRight] then
    InputAccelerationStrafe += -1;
  //get mousedrag input
  //get gamepad input
  if Abs(InputAccelerationForward) > 1 then
    InputAccelerationForward := Sign(InputAccelerationForward);
  if Abs(InputAccelerationStrafe) > 1 then
    InputAccelerationStrafe := Sign(InputAccelerationStrafe);

end;

{----------------------------------------------------------------------------}

procedure DPlayerControl.Manage;
begin
  doMove;
end;

{----------------------------------------------------------------------------}

procedure DPlayerControl.ToggleMouseLook;
begin
  TouchInput.CenterMouseCursor;
  MouseLook := not MouseLook;
end;

{----------------------------------------------------------------------------}

constructor DPlayerControl.Create;
begin
  ReleaseControls;
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

