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
  DecoGlobal;

type
  { calls ManageChildren in Update and resets their animation state }
  DAbstractArranger = class(DInterfaceElement)
  strict protected
    procedure ResetChildren;
    procedure ArrangeChildren; virtual; abstract;
  public
    procedure Update; override;
  end;

type
  { arranges children relative to its center without scaling them }
  DCenterArranger = class(DAbstractArranger)
  strict protected
    procedure ArrangeChildren; override;
  end;

{............................................................................}
implementation
uses
  DecoLog;

procedure DAbstractArranger.Update;
begin
  inherited Update; //gets animation state and kills children
  ArrangeChildren;  //sets NEXT for children
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

procedure DCenterArranger.ArrangeChildren;
var
  c: DSingleInterfaceElement;
begin
  //inherited <------- parent is abstract
  for c in Children do
    c.SetSize(Self.Current.x + (Self.Current.w - c.Next.w) div 2,
      Self.Current.y + (Self.Current.h - c.Next.h) div 2,
      Round(c.Next.w {* Self.Current.w / Self.Next.w}),
      Round(c.Next.h {* Self.Current.h / Self.Next.h}),
      c.Next.a, asNone, -1);

  ResetChildren;    //Resets children animation state to NEXT
end;

end.

