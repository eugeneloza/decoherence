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

(* Meta-elements arranging children *)

unit DecoInterfaceArrangers;

{$INCLUDE compilerconfig.inc}

interface

uses
  DecoInterfaceCore,
  DecoTime, DecoGlobal;

type
  { calls ManageChildren in Update and resets their animation state }
  DAbstractArranger = class(DInterfaceElement)
  strict protected
    procedure ResetChildren;
    procedure ArrangeChildren(const Animate: TAnimationStyle; const Duration: DTime); virtual;
  public
    procedure SetSize(const ax, ay, aw, ah: integer; const aAlpha: DFloat = 1.0;
      const Animate: TAnimationStyle = asDefault; const Duration: DTime = DefaultAnimationDuration); override;
  end;

type
  { arranges children relative to its center without scaling them }
  DCenterArranger = class(DAbstractArranger)
  strict protected
    procedure ArrangeChildren(const Animate: TAnimationStyle; const Duration: DTime); override;
  end;

{............................................................................}
implementation
uses
  DecoLog;

procedure DAbstractArranger.SetSize(const ax, ay, aw, ah: integer; const aAlpha: DFloat = 1.0;
  const Animate: TAnimationStyle = asDefault; const Duration: DTime = DefaultAnimationDuration);
begin
  inherited SetSize(ax, ay, aw, ah, aAlpha, Animate, Duration);
  ArrangeChildren(Animate, Duration);
end;

{-----------------------------------------------------------------------------}

procedure DAbstractArranger.ArrangeChildren(const Animate: TAnimationStyle; const Duration: DTime);
var
  c: DSingleInterfaceElement;
begin
  //no need to, as SetSize will fire those automatically
{  for c in Children do
    if c is DAbstractArranger then
      DAbstractArranger(c).ArrangeChildren(Animate, Duration); }
end;

{-----------------------------------------------------------------------------}

procedure DAbstractArranger.ResetChildren;
var
  c: DSingleInterfaceElement;
begin
  for c in Children do
    c.ResetAnimation;
end;

{======================  DCenterArranger =====================================}

procedure DCenterArranger.ArrangeChildren(const Animate: TAnimationStyle; const Duration: DTime);
var
  c: DSingleInterfaceElement;
begin
  for c in Children do
  begin
    c.SetSize(Self.Next.x + (Self.Next.w - c.Next.w) div 2,
      Self.Next.y + (Self.Next.h - c.Next.h) div 2,
      c.Next.w,
      c.Next.h,
      c.Next.a, Animate, Duration);
  end;

  inherited ArrangeChildren(Animate, Duration);
end;

end.

