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

(* Base actor - posessing stats and location, but no body or AI *)

{$INCLUDE compilerconfig.inc}

unit DecoBaseActor;

interface

uses
  DecoActorStats, DecoNavigation,
  DecoGlobal;

type
  {}
  DSimpleActor = class(DObject)
  protected
    Nav: TNav;
  public
    { Teleport this Actor to aNav }
    procedure TeleportTo(const aNav: TNav); virtual;
    { Procedures preformed on this actor every frame }
    procedure Manage; virtual; abstract;
  public
    constructor Create; virtual; //override;
  end;

type
  {}
  DBaseActor = class(DSimpleActor)
  public
    Hp, Sta, Mph: DStat;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;
{............................................................................}
implementation
uses
  DecoLog;

procedure DSimpleActor.TeleportTo(const aNav: TNav);
begin
  //release Nav block
  Nav := aNav;
end;

{-----------------------------------------------------------------------------}

constructor DSimpleActor.Create;
begin
  //nothing?
end;

{=============================================================================}

constructor DBaseActor.Create;
begin
  inherited Create;
end;

{-----------------------------------------------------------------------------}

destructor DBaseActor.Destroy;
begin
  inherited Destroy;
end;




end.

