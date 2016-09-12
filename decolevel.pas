{Copyright (C) 2012-2016 Yevhen Loza

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
unit decolevel;

{$mode objfpc}{$H+}

interface

uses
  CastleLog,
  CastleWindow, CastleSceneCore, CastleScene, CastleFilesUtils,
  castlePlayer, castleVectors, castleCameras,
  decoloadscreen,decoglobal;

procedure load_test_level;
Procedure InitTestLevel;

var scene:TcastleScene;
  player:TPlayer;

implementation

var loadedlevel:boolean=false;
procedure load_test_level;
begin
  WritelnLog('load_test_level','Scene');
  Scene := TCastleScene.Create(Application);
  Scene.Load(ApplicationData('level/test-level.x3d'));
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;
  //Scene.Attributes.EnableTextures:=false;
  WritelnLog('load_test_level','Player');

  Player := TPlayer.Create(Window.SceneManager);
  player.Camera.MouseLook:=false;
  Player.Camera.GravityUp:=Vector3Single(0,0,1);
  Player.Up:=Vector3Single(0,0,1);
  player.position:=Vector3Single(0,0,1);
  player.FallingEffect:=false;
  player.DefaultPreferredHeight:=1;
  WritelnLog('load_test_level','Finished');

end;

Procedure InitTestLevel;
begin
  if not loadedlevel then begin
     WritelnLog('InitTestLevel','Init');
     loadedlevel:=true;
     Window.SceneManager.Items.Add(Player);
     Window.SceneManager.Player := Player;
     Window.scenemanager.camera:=player.camera;
     Window.SceneManager.Items.Add(Scene);
     Window.SceneManager.MainScene := Scene;
     DestroyLoadScreen;
  end;
end;

end.

