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

(* A Framed element - basis for most complex interface elements *)

{$INCLUDE compilerconfig.inc}

unit DecoFramedElement;

{$mode objfpc}{$H+}

interface

uses
  DecoFrames, DecoInterfaceArrangers;

type
  {
    May be used "as is", but expected to contain only one CenterArranger child }
  DFramedElement = class(DAbstractArranger)
  private
    FFrame: DAbstractFrame;
    procedure SetFrame(const Value: DAbstractFrame);
  strict protected
    procedure ArrangeChildren(const Animate: TAnimationStyle; const Duration: DTime); override;
    {}
    property Frame: DAbstractFrame read FFrame write SetFrame;
  end;

{............................................................................}
implementation

procedure DFramedElement.SetFrame(const Value: DAbstractFrame);
begin

end;

procedure DCenterArranger.ArrangeChildren(const Animate: TAnimationStyle; const Duration: DTime);
var
  c: DSingleInterfaceElement;
begin
  //inherited ArrangeChildren(Animate, Duration); <------- parent is abstract
  Self.GetAnimationState;

{  for c in Children do
  begin
    c.GetAnimationState;
    c.Next.x := Self.Current.x + (Self.Current.w - c.Next.w) div 2;
    c.Next.y := Self.Current.y + (Self.Current.h - c.Next.h) div 2;
    c.ResetAnimation;

    c.SetSize(Self.Next.x + (Self.Next.w - c.Next.w) div 2,
      Self.Next.y + (Self.Next.h - c.Next.h) div 2,
      c.Next.w,
      c.Next.h,
      c.Next.a, Animate, Duration);
  end;}
end;

end.

