{Copyright (C) 2012-2018 Yevhen Loza

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <http://www.gnu.org/licenses/>.}

{---------------------------------------------------------------------------}

(* A highets-level container for the interface and its routines *)

unit DecoGUI;

{$INCLUDE compilerconfig.inc}

interface

uses
  DecoGlobal, DecoGUIScale, DecoInterfaceCore;

type
  { GUI container, manages all other GUI elements }
  DGUI = class(DInterfaceElement)
  public
    procedure ShowMessage(const aMessage: string);
  public
    procedure Draw; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

var
  GUI: DGUI;

{ Initialize GUI instance }
procedure InitGUI;
{ Free GUI instance }
procedure FreeGUI;
{............................................................................}
implementation
uses
  SysUtils,
  DecoTime, DecoLog;

procedure DGUI.ShowMessage(const aMessage: string);
begin
  Log(true, CurrentRoutine, aMessage); //temporary
  { will create a message window with text }
end;

{-----------------------------------------------------------------------------}

procedure DGUI.Draw;
begin
  { clear the screen depending on the game mode
    in case SceneManager doesn't clear it }
//  if GameModeNeedsClearingScreen then
//    RenderContext.Clear([cbColor], Black);

  { draw children elements }
  //inherited Draw;

  { draw special elements }
  //FPSLabel.CountFPS;
end;

{-----------------------------------------------------------------------------}

constructor DGUI.Create;
begin
  inherited Create;
  //...
end;

{-----------------------------------------------------------------------------}

destructor DGUI.Destroy;
begin
  //...
  inherited Destroy;
end;

{............................................................................}

procedure InitGUI;
begin
  GUI := DGUI.Create;
end;

{-----------------------------------------------------------------------------}

procedure FreeGUI;
begin
  GUI.Free;
end;

end.

