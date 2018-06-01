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

(* Bar images, that are displayed partially based on progress *)

{$INCLUDE compilerconfig.inc}

unit DecoInterfaceBars;

interface

uses
  DecoInterfaceImages, DecoFramedElement,
  DecoBaseActor,
  DecoGlobal;

type
  TBarStyle = (bsVertical, bsHorizontal);

type
  { Generic bar used for progress bars and health bars }
  DAbstractBarImage = class(DSimpleImage)
  public
    { minimmum, maximum, current maximum and current position
      minimum usually is zero and automatically set to 0 in constructor }
    Min, Max, CurrentMax, Position: DFloat;
    { vertical or horizontal style of the bar }
    Kind: TBarStyle;
    procedure Draw; override;
  public
    constructor Create; override;
  end;

type
  {}
  DFramedBar = class(DRectagonalFramedElement)
  strict protected
    FBar: DabstractBarImage;
  public
    constructor Create; override;
  end;

type
  {}
  DStatBar = class(DFramedBar)
  strict protected
    FTarget: DBaseActor;
    procedure SetTarget(const aTarget: DBaseActor);
  public
    {}
    property Target: DBaseActor read FTarget write SetTarget;
  end;

type
  {}
  DHealthBar = class(DStatBar)
  public
    procedure Update; override;
    constructor Create; override;
  end;

type
  {}
  DStaminaBar = class(DStatBar)
  public
    procedure Update; override;
    constructor Create; override;
  end;

type
  {}
  DConcentrationBar = class(DStatBar)
  public
    procedure Update; override;
    constructor Create; override;
  end;

type
  {}
  DMetaphysicsBar = class(DStatBar)
  public
    procedure Update; override;
    constructor Create; override;
  end;

type
  {}
  DPlayerBars = class(DRectagonalFramedElement)
  strict private
    HealthBar: DHealthBar;
    StaminaBar: DStaminaBar;
    ConcentrationBar: DConcentrationBar;
    MetaphysicsBar: DMetaphysicsBar;
    FTarget: DBaseActor;
    procedure SetTarget(const aTarget: DBaseActor);
  public
    {}
    property Target: DBaseActor read FTarget write SetTarget;
  end;

implementation

uses
  DecoLog, DecoImages, DecoFrames, DecoMath;

procedure DAbstractBarImage.Draw;
var
  Phase: DFloat;
  PositionSource, PositionScaled: integer;
begin
  //inherited <---------- this render is different
  if Max = Min then
  begin
    Log(LogInterfaceError, CurrentRoutine, 'ERROR: Division by zero!');
    Exit;
  end;

  Update;

  Phase := Position / (Max - Min);
  if Kind = bsVertical then
  begin
    PositionScaled := Round(Current.h * Phase);
    PositionSource := Round(Image.Height * Phase);
    Image.Draw(Current.x, Current.y, Current.w, PositionScaled,
      0, 0, Image.Width, PositionSource);
  end else
  begin
    PositionScaled := Round(Current.w * Phase);
    PositionSource := Round(Image.Width * Phase);
    Image.Draw(Current.x, Current.y, PositionScaled, Current.h,
      0, 0, PositionSource, Image.Height);
  end;
end;

{---------------------------------------------------------------------------}

constructor DAbstractBarImage.Create;
begin
  inherited Create;
  Min := 0;
  Max := 1;
  Position := Max;
  Kind := bsHorizontal;
end;

{===========================================================================}

constructor DFramedBar.Create;
begin
  inherited Create;
  FBar := DAbstractBarImage.Create;
  Grab(FBar);
end;

{===========================================================================}

procedure DStatBar.SetTarget(const aTarget: DBaseActor);
begin
  if FTarget <> aTarget then
  begin
    FTarget := aTarget;
    //and update something here?
  end;
end;

{===========================================================================}

constructor DHealthBar.Create;
begin
  inherited Create;
  LoadFrame(GetFrameByName('PlayerStatBarFrame'));
  FBar.Load(GetImageByName('PlayerHealthBarImage'));
  FBar.Kind := bsVertical;
end;

{---------------------------------------------------------------------------}

procedure DHealthBar.Update;
begin
  inherited Update;
  if FTarget <> nil then
  begin
    FBar.Min := 0;
    FBar.Max := FTarget.Hp.Value[2];
    FBar.Position := AboveZero(FTarget.Hp.Value[0]);
  end;
end;

{--------------------------------------------------------------------------}

constructor DStaminaBar.Create;
begin
  inherited Create;
  LoadFrame(GetFrameByName('PlayerStatBarFrame'));
  FBar.Load(GetImageByName('PlayerStaminaBarImage'));
  FBar.Kind := bsVertical;
end;

{---------------------------------------------------------------------------}

procedure DStaminaBar.Update;
begin
  inherited Update;
  if FTarget <> nil then
  begin
    FBar.Min := 0;
    FBar.Max := FTarget.Sta.Value[2];
    FBar.Position := AboveZero(FTarget.Sta.Value[0]);
  end;
end;

{--------------------------------------------------------------------------}

constructor DConcentrationBar.Create;
begin
  inherited Create;
  LoadFrame(GetFrameByName('PlayerStatBarFrame'));
  FBar.Load(GetImageByName('PlayerConcentrationBarImage'));
  FBar.Kind := bsVertical;
end;

{---------------------------------------------------------------------------}

procedure DConcentrationBar.Update;
begin
  inherited Update;
  if FTarget <> nil then
  begin
    FBar.Min := 0;
    FBar.Max := FTarget.Cnc.Value[2];
    FBar.Position := AboveZero(FTarget.Cnc.Value[0]);
  end;
end;

{--------------------------------------------------------------------------}

constructor DMetaphysicsBar.Create;
begin
  inherited Create;
  LoadFrame(GetFrameByName('PlayerStatBarFrame'));
  FBar.Load(GetImageByName('PlayerMetaphysicsBarImage'));
  FBar.Kind := bsVertical;
end;

{---------------------------------------------------------------------------}

procedure DMetaphysicsBar.Update;
begin
  inherited Update;
  if FTarget <> nil then
  begin
    FBar.Min := 0;
    FBar.Max := FTarget.Mph.Value[2];
    FBar.Position := AboveZero(FTarget.Mph.Value[0]);
  end;
end;

{==========================================================================}

procedure DPlayerBars.SetTarget(const aTarget: DBaseActor);
begin
  if FTarget <> aTarget then
  begin
    FTarget := aTarget;
    HealthBar.Target := FTarget;
    StaminaBar.Target := FTarget;
    ConcentrationBar.Target := FTarget;
    MetaphysicsBar.Target := FTarget;
  end;
end;

{--------------------------------------------------------------------------}

end.

