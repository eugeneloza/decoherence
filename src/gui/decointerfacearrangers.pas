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
  {}
  DAbstractArranger = class(DInterfaceElement)
  strict private
    procedure ResetChildren;
  strict protected
    procedure ManageChildren; virtual; abstract;
  public
    procedure Update; override;
  end;

{............................................................................}
implementation
uses
  DecoLog;

procedure DAbstractArranger.Update;
begin
  inherited Update; //gets animation state and kills children
  ManageChildren;   //sets NEXT for children
  ResetChildren;    //Resets children animation state to NEXT
end;

{-----------------------------------------------------------------------------}

procedure DAbstractArranger.ResetChildren;
var
  c: DSingleInterfaceElement;
begin
  for c in Children do
    c.ResetAnimation;
end;

end.

