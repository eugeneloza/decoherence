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

(* Contains most of interface basics and grouping *)

unit DecoInterfaceCore;

{$INCLUDE compilerconfig.inc}

interface

uses
  Generics.Collections,
  DecoInterfaceTimer,
  DecoGlobal, DecoTime;

type
  { most abstract container for interface elements
    Defines size, scaling and animation state }
  DAbstractElement = class abstract(DObject)
  strict private
    //...
  public
    { draw the element / as abstract as it might be :) }
    procedure Draw; virtual; abstract;
    procedure SetTint; virtual; abstract;
  public
    //...
  public
    constructor Create; virtual; //override;
    destructor Destroy; override;
  end;

type
  { Fully-featured Interface Element with Mouse/Touch support
    It lacks only "Children" or specific "Draw" to be used }
  DSingleInterfaceElement = class abstract(DAbstractElement)
  strict protected
    //...
    { A simple timer to fire some event on time-out }
    Timer: DTimer;
  public
    { Activate and initialize timer }
    procedure SetTimeOut(const Seconds: DTime);
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

type
  { List of DSingleInterfaceElement instances }
  DInterfaceElementsList = specialize TObjectList<DSingleInterfaceElement>;
type
  { An interface element, that can contain "Children" }
  DInterfaceElement = class(DSingleInterfaceElement)
  strict protected
    //...
  public
    { List of the children of this interface element }
    Children: DInterfaceElementsList;
    procedure SetTint; override;
    procedure Draw; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

{............................................................................}
implementation
uses
  SysUtils,
  DecoLog;

{============================================================================}
{======================== D ABSTRACT ELEMENT ================================}
{============================================================================}

constructor DAbstractElement.Create;
begin
  //inherited <------- nothing to inherit

end;

{-----------------------------------------------------------------------------}

destructor DAbstractElement.Destroy;
begin

  inherited Destroy;
end;

{============================================================================}
{===================== D SINGLE INTERFACE ELEMENT ===========================}
{============================================================================}

constructor DSingleInterfaceElement.Create;
begin
  inherited Create;
  //...
end;

{-----------------------------------------------------------------------------}

destructor DSingleInterfaceElement.Destroy;
begin
  FreeAndNil(Timer);
  inherited Destroy;
end;

{-----------------------------------------------------------------------------}

procedure DSingleInterfaceElement.SetTimeOut(const Seconds: DTime);
begin
  if Timer = nil then
    Timer := DTimer.Create;
  Timer.SetTimeOut(Seconds);
end;

{============================================================================}
{======================= D INTERFACE ELEMENT ================================}
{============================================================================}

constructor DInterfaceElement.Create;
begin
  inherited Create;
  Children := DInterfaceElementsList.Create(True);
end;

{----------------------------------------------------------------------------}

destructor DInterfaceElement.Destroy;
begin
  //this should fire as recoursive because children owns elements, which in turn will fire their destructors onfree
  Children.Free;
  inherited Destroy;
end;

{----------------------------------------------------------------------------}

procedure DInterfaceElement.SetTint;
var
  i: integer;
begin
  //inherited SetTint; <---------- parent is abstract
  for i := 0 to Children.Count - 1 do
    Children[i].SetTint;
end;

{----------------------------------------------------------------------------}

procedure DInterfaceElement.Draw;
var
  i: integer;
begin
  //inherited Draw; <---------- parent is abstract
  {if isVisible then
  begin
    Update; }
    for i := 0 to Children.Count - 1 do
      Children[i].Draw;
  {end;}
end;

end.

