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

(* Container of interface elements
   Responsible for scale and animation of the interface elements *)

unit DecoInterfaceContainer;

{$INCLUDE compilerconfig.inc}

interface

uses
  DecoGlobal;

type
  { Determines coordinates and size of a interface element }
  DInterfaceContainer = class(DObject)
  public
    isInitialized: boolean;
    x, y, w, h: integer;
    a: DFloat;
    { cached to accelerate things, never assign them }
    x2, y2: integer;
    { Copy current container's xywha to aDest }
    procedure AssignTo(const aDest: DInterfaceContainer); TryInline
    { Copy current container's xywha from aSource }
    procedure AssignFrom(const aSource: DInterfaceContainer); TryInline
    { Mix this container's xywha from aLast and aNext with aPhase as a weight }
    procedure AssignMix(const aLast, aNext: DInterfaceContainer; const aPhase: DFloat); TryInline

    { Initialize current container with float coordinates,
      providing negative ax will count it from RIGHT side. }
    procedure SetIntSize(const ax, ay, aw, ah: integer; const aAlpha: DFloat = 1);
    procedure SetIntCoord(const ax1, ay1, ax2, ay2: integer; const aAlpha: DFloat = 1);
    procedure SetIntWidthHeight(const aw, ah: integer; const aAlpha: DFloat = 1);
    { Initialize current container with float coordinates,
      providing negative ax will count it from RIGHT side.
      Both coordinates are scaled against GUIHeight = 1.0 }
    procedure SetFloatSize(const ax, ay, aw, ah: DFloat; const aAlpha: DFloat = 1);
    procedure SetFloatCoord(const ax1, ay1, ax2, ay2: DFloat; const aAlpha: DFloat = 1);
  public
    constructor Create;
  end;

{............................................................................}
implementation
uses
  DecoGUIScale;

procedure DInterfaceContainer.AssignTo(const aDest: DInterfaceContainer); TryInline
begin
  aDest.x := Self.x;
  aDest.y := Self.y;
  aDest.w := Self.w;
  aDest.h := Self.h;
  aDest.x2 := Self.x2;
  aDest.y2 := Self.y2;
  aDest.a := Self.a;
  Self.isInitialized := aDest.isInitialized;
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceContainer.AssignFrom(const aSource: DInterfaceContainer); TryInline
begin
  Self.x := aSource.x;
  Self.y := aSource.y;
  Self.w := aSource.w;
  Self.h := aSource.h;
  Self.x2 := aSource.x2;
  Self.y2 := aSource.y2;
  Self.a := aSource.a;
  Self.isInitialized := aSource.isInitialized;
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceContainer.AssignMix(const aLast, aNext: DInterfaceContainer; const aPhase: DFloat); TryInline
begin
  Self.x := Round(aLast.x + (aNext.x - aLast.x) * aPhase);
  Self.y := Round(aLast.y + (aNext.y - aLast.y) * aPhase);
  Self.w := Round(aLast.w + (aNext.w - aLast.w) * aPhase);
  Self.h := Round(aLast.h + (aNext.h - aLast.h) * aPhase);
  Self.x2 := x + w;
  Self.y2 := y + h;
  Self.a :=      (aLast.a + (aNext.a - aLast.a) * aPhase);
  Self.isInitialized := aNext.isInitialized;
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceContainer.SetIntSize(const ax, ay, aw, ah: integer; const aAlpha: DFloat = 1);
begin
  x := ax;
  y := ay;
  w := aw;
  h := ah;
  if ax < 0 then
    x := x + GUIWidth - w;
  x2 := x + w; //cache x2,y2
  y2 := y + h;
  a := aAlpha;
  isInitialized := true;
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceContainer.SetIntWidthHeight(const aw, ah: integer; const aAlpha: DFloat = 1);
begin
  x := x - (w - aw) div 2;
  y := y - (h - ah) div 2;
  w := aw;
  h := ah;
  x2 := x + w; //cache x2,y2
  y2 := y + h;
  a := aAlpha;
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceContainer.SetIntCoord(const ax1, ay1, ax2, ay2: integer; const aAlpha: DFloat = 1);
begin
  SetFloatSize(ax1, ay1, ax2 - ax1, ay2 - ay1, aAlpha);
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceContainer.SetFloatSize(const ax, ay, aw, ah: DFloat; const aAlpha: DFloat = 1);
begin
  x := Round(ax * GUIHeight);
  y := Round(ay * GUIHeight);
  w := Round(aw * GUIHeight);
  h := Round(ah * GUIHeight);
  if ax < 0 then
    x := x + GUIWidth - w;
  a := aAlpha;
  x2 := x + w; //cache x2,y2
  y2 := y + h;
  isInitialized := true;
end;

{-----------------------------------------------------------------------------}

procedure DInterfaceContainer.SetFloatCoord(const ax1, ay1, ax2, ay2: DFloat; const aAlpha: DFloat = 1);
begin
  SetFloatSize(ax1, ay1, ax2 - ax1, ay2 - ay1, aAlpha);
end;

{-----------------------------------------------------------------------------}

constructor DInterfaceContainer.Create;
begin
  //inherited <------ Nothing to inherit
  isInitialized := false;
end;

end.

