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
  CastleWindow, CastleWindowTouch, CastleSceneCore, CastleScene,
  castleVectors,  X3DNodes,
  deco3dload,
  sysutils,

  decoabstractworld, decodungeonworld, decodungeongenerator,
  deconavigation, decoglobal;

procedure load_test_level;
Procedure InitTestLevel;

var scene: TcastleScene;
    LoadCompleted: boolean = false;

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
end;

var loadedlevel:boolean=false;
procedure load_test_level;
begin
  Generate3DWorld;

  WritelnLog('load_test_level','Scene');


  Window.ShadowVolumes := Shadow_volumes_enabled;
  window.ShadowVolumesRender := Shadow_volumes_enabled;
  window.AntiAliasing := aa8SamplesNicer;

  InitNavigation;

  WritelnLog('load_test_level','Finished');
end;

Procedure InitTestLevel;
begin
  if not LoadCompleted then exit;
  if not loadedlevel then begin
     WritelnLog('InitTestLevel','Init');
     loadedlevel := true;
     CurrentWorld.activate;

     Window.TouchInterface := {$IFDEF Android}tiCtlWalkDragRotate{$ELSE}tiNone{$ENDIF};
     SetGameMode(gmTravel);
  end;
end;

end.

