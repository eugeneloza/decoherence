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

