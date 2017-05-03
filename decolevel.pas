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
  castlePlayer, castleVectors, castleCameras, X3DNodes,
  deco3dload, decodungeontiles,
  x3dload, sysutils,
  castle3d,
  decoabstractworld, decodungeonworld, decodungeongenerator,
  decoglobal;

procedure load_test_level;
Procedure InitTestLevel;

var scene: TcastleScene;
  Camera: TWalkCamera;
  monsters: array[0..10] of T3DOrient;
  //shaders: TSwitchNode;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses DecoGameMode  ;

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

var monster: TX3DRootNode;

var loadedlevel:boolean=false;
procedure load_test_level;
var Nav:TKambiNavigationInfoNode; /// !!!
    NavLight:TPointLightNode;
var ScreenEffect: TX3DRootNode;
    mRoot: TX3DRootNode;
    //i: integer;
begin
  Generate3DWorld;

  WritelnLog('load_test_level','Scene');

  mRoot := LoadBlenderX3D(ApplicationData('level/test-level.x3d'));

  //create light that follows the player
   NavLight:= TPointLightNode.Create('', '');
   NavLight.FdColor.Value := vector3single(1,0.3,0.1);
   NavLight.FdAttenuation.value := Vector3Single(1,0,1);
   NavLight.FdRadius.value := 10;
   NavLight.FdIntensity.value := 20;
   NavLight.FdOn.value := true;
   NavLight.FdShadows.value := false;
   //and create a respective navigation node
   nav:=TKambiNavigationInfoNode.Create('', '');
   nav.FdHeadLightNode.Value := NavLight;
   nav.FdHeadlight.Value := true;

  mRoot.FdChildren.Add(nav);

  {this is a temporary "addition" of a screen shader,
   should be replaced for something more useful some time later}
  {Shaders := TSwitchNode.create;}
  ScreenEffect := load3D(ApplicationData('shaders/empty.x3dv'));
  {Shaders.fdChildren.add(screenEffect.FdChildren[0]);
  ScreenEffect := load3D(ApplicationData('shaders/edgedetect.x3dv'));
  Shaders.fdChildren.add(screenEffect.FdChildren[0]);
  ScreenEffect := load3D(ApplicationData('shaders/blur.x3dv'));
  Shaders.fdChildren.add(screenEffect.FdChildren[0]);}

  mRoot.FdChildren.add(ScreenEffect);

  Scene := TCastleScene.Create(Application);
  Scene.Load(mRoot,true);
  //Scene.Attributes.EnableTextures := false;

  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;
  scene.ShadowMaps := Shadow_maps_enabled;

  monster := LoadBlenderX3D(ApplicationData('models/creatures/idle.castle-anim-frames'));

  Window.ShadowVolumes := Shadow_volumes_enabled;
  window.ShadowVolumesRender := Shadow_volumes_enabled;
  window.AntiAliasing := aa8SamplesNicer;
  WritelnLog('load_test_level','Player');

  camera := TWalkCamera.create(Window);
  {z-up orientation}
  camera.SetView(Vector3Single(0,0,1),Vector3Single(0,1,0),Vector3Single(0,0,1),Vector3Single(0,0,1),true);
  camera.MoveSpeed := 5; //set to zero to stop
  camera.MouseDragMode := mdRotate;
  //camera.Input := [];  //-----  completely disable camera
  WritelnLog('load_test_level','Finished');

end;

Procedure InitTestLevel;
var i: integer;
  monsterscene: array[0..10] of TCastleScene;
  m: TCastleScene;
begin
  if not loadedlevel then begin
     WritelnLog('InitTestLevel','Init');
     loadedlevel := true;
     Window.SceneManager.Items.Add(Scene);

     m := TCastleScene.create(window);
     m.load(monster,true,true);
     for i := 0 to 10 do begin
{       monsterscene[i] := TCastleScene.create(window);
       MonsterScene[i].load(monster,false,false);} //not working??? Switch node preserves its state, root must be cloned too... bad.
       monsterscene[i] := m.clone(window);
       monsterscene[i].ProcessEvents := true;
       monsterscene[i].PlayAnimation('animation',paForceLooping);
       monsterscene[i].increasetime(drnd.random);
       //monsterscene[i].Attributes.EnableTextures := false;

       monsters[i] := T3DOrient.Create(window);
       monsters[i].Orientation := otUpZDirectionMinusY;
       monsters[i].Up := Vector3Single(0,0,1);
       //monsters[i].UpPrefer(Vector3Single(0,0,1)); //not working?
       monsters[i].add(monsterscene[i]);
       monsters[i].translate(Vector3Single(i,0,1));

       Window.SceneManager.Items.Add(monsters[i]);
     end;

     Window.SceneManager.MainScene := Scene;
     Window.SceneManager.Camera := camera;
     Window.TouchInterface := {$IFDEF Android}tiCtlWalkDragRotate{$ELSE}tiNone{$ENDIF};
     SetGameMode(gmTravel);
  end;
end;

end.

