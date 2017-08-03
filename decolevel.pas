{Copyright (C) 2012-2017 Yevhen Loza

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

{ Temporary unit "loads" a game level and sets up the camera }
unit decolevel;

{$INCLUDE compilerconfig.inc}

interface

uses
  CastleLog,
  CastleWindow, CastleWindowTouch, CastleScene, X3DNodes,
  DecoLoad3d,
  SysUtils,

  DecoAbstractWorld, DecoDungeonWorld, DecoDungeonGenerator,
  DecoActor,
  DecoNavigation, DecoGlobal;

procedure load_test_level;
Procedure InitTestLevel;
procedure FreeTestLevel;

var LoadCompleted: boolean = false;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses CastleFilesUtils,
  DecoGameMode;

procedure Generate3DWorld;
var GENERATOR: D3dDungeonGenerator;
begin
  GENERATOR := D3dDungeonGenerator.Create;
  GENERATOR.parameters.load(ApplicationData(GetScenarioFolder+MapsFolder+'1'+'.xml'+GZ_ext));
  GENERATOR.InitParameters;
  GENERATOR.Generate;
  CurrentWorld := DDungeonWorld.create;
  CurrentWorld.Load(Generator);
  FreeAndNil(GENERATOR);
  CurrentWorld.Build;

  tmpLoadKnightCreature;
end;

var LoadedLevel: boolean = false;
procedure load_test_level;
begin
  Generate3DWorld;

  WritelnLog('load_test_level','Scene');


  Window.ShadowVolumes := Shadow_volumes_enabled;
  Window.ShadowVolumesRender := Shadow_volumes_enabled;
  Window.AntiAliasing := aa8SamplesNicer;


  InitNavigation;

  WritelnLog('load_test_level','Finished');
end;

Procedure InitTestLevel;
begin
  if not LoadCompleted then exit;
  if not loadedlevel then begin
     WritelnLog('InitTestLevel','Init');
     SetGameMode(gmTravel);
     LoadedLevel := true;
     CurrentWorld.activate;

     {$WARNING this is wrong}
     {why does it gets a wrong GRAVITY_UP if called from Self.Activate?????}
     CurrentWorld.SpawnActors; //must go after BuildNav;

     Window.TouchInterface := {$IFDEF Android}tiCtlWalkDragRotate{$ELSE}tiNone{$ENDIF};
     SetGameMode(gmTravel);
  end;
end;

procedure FreeTestLevel;
begin

end;

end.

