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
  CastleWindow, CastleWindowTouch, CastleSceneCore, CastleScene, CastleFilesUtils,
  castlePlayer, castleVectors, castleCameras,
  deco3dload, decodungeontiles,
  decoglobal;

procedure load_test_level;
Procedure InitTestLevel;

var scene: TcastleScene;
  monster: TCastleScene;
  //player:TPlayer;
  Camera: TWalkCamera;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses DecoGameMode,
  X3DNodes;

var loadedlevel:boolean=false;
procedure load_test_level;
{var Nav:TKambiNavigationInfoNode; /// !!!
    NavLight:TPointLightNode;   }
begin
  LoadTiles;

  WritelnLog('load_test_level','Scene');
  Scene := TCastleScene.Create(Application);
  Scene.Load(LoadBlenderX3D(ApplicationData('level/test-level.x3d')),true);
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;
  scene.ShadowMaps := Shadow_maps_enabled;

  monster := TCastleScene.create(Application);
  monster.Spatial := [ssRendering, ssDynamicCollisions];
  monster.ProcessEvents := true;
  monster.ShadowMaps := Shadow_maps_enabled;
  //monster.Load(ApplicationData('creatures/forest-monster-final.castle-anim-frames'));
  monster.Load(ApplicationData('creatures/walk.castle-anim-frames'));
  //(monster.RootNode.FdChildren[4] as TTRansformNode).Rotation := vector4single(1,0,0,Pi/2);
 { //create light that follows the player
  NavLight:= TPointLightNode.Create('', '');
  NavLight.FdColor.Value := vector3single(1,0.1,0.1);
  NavLight.FdAttenuation.value := Vector3Single(0,0,6);
  NavLight.FdRadius.value := 1;
  NavLight.FdIntensity.value := 30;
  NavLight.FdOn.value := true;
  NavLight.FdShadows.value := false;
  //and create a respective navigation node
  nav:=TKambiNavigationInfoNode.Create('', '');
  nav.FdHeadLightNode.Value := NavLight;
  nav.FdHeadlight.Value := true;

  scene.RootNode.FdChildren.Add(nav);  }

  Window.ShadowVolumes := Shadow_volumes_enabled;
  window.ShadowVolumesRender := Shadow_volumes_enabled;
  window.AntiAliasing := aa8SamplesNicer;
  //Scene.Attributes.EnableTextures := false;
  WritelnLog('load_test_level','Player');

  camera := TWalkCamera.create(Window);
  {z-up orientation}
  camera.SetView(Vector3Single(0,0,1),Vector3Single(0,1,0),Vector3Single(0,0,1),Vector3Single(0,0,1),true);
  camera.MoveSpeed := 5;
  camera.MouseDragMode := mdRotate;
  WritelnLog('load_test_level','Finished');

end;

Procedure InitTestLevel;
begin
  if not loadedlevel then begin
     WritelnLog('InitTestLevel','Init');
     loadedlevel := true;
     Window.SceneManager.Items.Add(Scene);

     monster.PlayAnimation('animation',paForceLooping);
     Window.SceneManager.Items.Add(monster);

     Window.SceneManager.MainScene := Scene;
     Window.SceneManager.Camera := camera;
     Window.TouchInterface := {$IFDEF Android}tiCtlWalkDragRotate{$ELSE}tiNone{$ENDIF};
     SetGameMode(gmTravel);
  end;
end;

end.

