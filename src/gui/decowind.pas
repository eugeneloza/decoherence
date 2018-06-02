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

(* Wind image that floats from right to left *)

{$INCLUDE compilerconfig.inc}

unit DecoWind;

interface

uses
  DecoInterfaceCore, DecoInterfaceImages,
  DecoGlobal, DecoTime;

type
  { Image that changes its location with phase }
  DWindImage = class(DSimpleImage)
  strict private
    Phase, OpacityPhase: DFloat;
    procedure ResetPhase;
  public
    { 1/seconds to scroll the full screen }
    PhaseSpeed: DFloat;
    procedure Update; override;
    procedure Draw; override;
    { Wind is always "white" }
    procedure SetTint; override;
  public
    constructor Create; override;
  end;

type
  { Interface element, containing 2 DWindImage elements }
  DWind = class(DInterfaceElement)
  strict private
    Wind1, Wind2: DWindImage;

  public
    constructor Create; override;
  end;

procedure InitWind;
{............................................................................}
implementation
uses
  CastleVectors,
  DecoImageLoader, DecoGUIScale, DecoImages,
  DecoLog, Profiler;

constructor DWindImage.Create;
begin
  inherited Create;
  ResetPhase;
end;

{----------------------------------------------------------------------------}

procedure DWindImage.ResetPhase;
begin
  Phase := DRND.Random;
  OpacityPhase := DRND.Random;
end;

{----------------------------------------------------------------------------}

procedure DWindImage.SetTint;
begin
  //inherited SetTint; <---------- replacing parent completely
  if Image <> nil then
    Image.Color := Vector4(1,1,1,1);
end;

{----------------------------------------------------------------------------}

procedure DWindImage.Update;
var
  PhaseShift: DFloat;
begin
  {StartProfiler}

  inherited Update;
  PhaseShift := DeltaT * PhaseSpeed;
  if PhaseShift < 0.5 then
  begin
    Phase += PhaseShift * (1 + 0.1 * DRND.Random);
    if Phase > 1 then
      Phase -= 1;
    OpacityPhase += PhaseShift / 2 * (1 + 0.2 * DRND.Random);
    if OpacityPhase > 1 then
      OpacityPhase -= 1;
  end else
    { if it was too long since last frame, reset the wind phase }
    ResetPhase;

  {StopProfiler}
end;

{----------------------------------------------------------------------------}

procedure DWindImage.Draw;
var
  PhaseScaled: integer;
begin
  {StartProfiler}

  //inherited Draw <------------ replacing parent completely
  Update;

  Image.SetAlpha(Current.a + Current.a / 4 * Sin(2 * Pi * OpacityPhase));
  PhaseScaled := Round((1 - Phase) * GUIWidth);

  //draw first part of the image
  Image.Draw(PhaseScaled, 0,
    GUIWidth - PhaseScaled, GUIHeight,
    0, 0,
    GUIWidth - PhaseScaled, GUIHeight);
  //draw second part of the image
  Image.Draw(0, 0,
    PhaseScaled, GUIHeight,
    GUIWidth - PhaseScaled, 0,
    PhaseScaled, GUIHeight);

  {StopProfiler}
end;

{===========================================================================}

var
  WindImage1, WindImage2: DImage;

constructor DWind.Create;
begin
  {StartProfiler}

  inherited Create;
  Log(LogInterfaceImageLoading, CurrentRoutine, 'Loading wind...');
  Self.FullScreen;
  Wind1 := DWindImage.Create;
  Wind2 := DWindImage.Create;
  Wind1.FullScreen(0.1);
  Wind2.FullScreen(0.1);
  Wind1.PhaseSpeed := 1 / (15 + DRND.Random);
  Wind2.PhaseSpeed := 1 / (10 + DRND.Random);
  Wind1.Load(WindImage1);
  Wind2.Load(WindImage2);

  Grab(Wind1);
  Grab(Wind2);

  {StopProfiler}
end;

{..........................................................................}

procedure InitWind;
begin
  {StartProfiler}

  WindImage1 := LoadFullScreenImage('GUI/Wind/WindClouds1_GIMP.jpg');
  WindImage2 := LoadFullScreenImage('GUI/Wind/WindClouds2_GIMP.jpg');
  { will be freed automatically at game end }

  {StopProfiler}
end;


end.

