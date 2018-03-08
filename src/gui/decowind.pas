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

unit DecoWind;

{$INCLUDE compilerconfig.inc}

interface

uses
  DecoInterfaceCore, DecoInterfaceImages,
  DecoGlobal, DecoTime;

type
  { Image that changes its location with phase }
  DWindImage = class(DSimpleImage)
  strict protected
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
{............................................................................}
implementation
uses
  CastleVectors;

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
  inherited Update;
  PhaseShift := DeltaT * PhaseSpeed;
  Phase += PhaseShift * (1 + 0.1 * DRND.Random);

end;

{----------------------------------------------------------------------------}

procedure DWindImage.Draw;
begin
  //inherited Draw <------------ replacing parent completely

end;

{===========================================================================}

constructor DWind.Create;
begin
  inherited Create;
end;

end.

