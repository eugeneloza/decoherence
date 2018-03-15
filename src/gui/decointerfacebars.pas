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

unit DecoInterfaceBars;

{$INCLUDE compilerconfig.inc}

interface

uses
  DecoInterfaceImages,
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
    constructor Create; override;
  end;

implementation

uses
  DecoLog;

procedure DAbstractBarImage.Draw;
var
  xx: integer;
begin
  //inherited <---------- this render is different
  if Max = Min then
  begin
    Log(LogInterfaceError, CurrentRoutine, 'ERROR: Division by zero!');
    Exit;
  end;

  Update;
  if Kind = bsVertical then
  begin
    xx := Round(Current.h * Position / (Max - Min));
    Image.Draw(Current.x, Current.y, Current.w, xx);
  end
  else
  begin
    xx := Round(Current.w * Position / (Max - Min));
    Image.Draw(Current.x, Current.y, xx, Current.h);
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

end.

