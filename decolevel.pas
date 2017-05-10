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
  castleVectors,  X3DNodes,
  deco3dload, {decodungeontiles,}
  {x3dload,} sysutils,
  {castle3d,}
  decoabstractworld, decodungeonworld, decodungeongenerator,
  deconavigation, decoglobal;

procedure load_test_level;
Procedure InitTestLevel;

var scene: TcastleScene;
  //monsters: array[0..10] of T3DOrient;
  //shaders: TSwitchNode;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses DecoGameMode
  ;

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

//var monster: TX3DRootNode;

var loadedlevel:boolean=false;
procedure load_test_level;
{var Nav:; /// !!!

var ScreenEffect: TX3DRootNode;
    mRoot: TX3DRootNode;
    //i: integer; }
begin
  Generate3DWorld;

  WritelnLog('load_test_level','Scene');


  //monster := LoadBlenderX3D(ApplicationData('models/creatures/idle.castle-anim-frames'));

  Window.ShadowVolumes := Shadow_volumes_enabled;
  window.ShadowVolumesRender := Shadow_volumes_enabled;
  window.AntiAliasing := aa8SamplesNicer;

  InitNavigation;

  WritelnLog('load_test_level','Finished');
end;

Procedure InitTestLevel;
//var i: integer;
 { monsterscene: array[0..10] of TCastleScene;
  m: TCastleScene;}
begin
  if not loadedlevel then begin
     WritelnLog('InitTestLevel','Init');
     loadedlevel := true;
     CurrentWorld.activate;
    // Window.SceneManager.Items.Add(Scene);

    { m := TCastleScene.create(window);
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

     //Window.SceneManager.MainScene := Scene; }
     Window.TouchInterface := {$IFDEF Android}tiCtlWalkDragRotate{$ELSE}tiNone{$ENDIF};
     SetGameMode(gmTravel);
  end;
end;

end.

