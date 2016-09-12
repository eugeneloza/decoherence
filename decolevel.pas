unit decolevel;

{$mode objfpc}{$H+}

interface

uses
  CastleWindow, CastleSceneCore, CastleScene, CastleFilesUtils,
  castlePlayer, castleVectors, castleCameras,
  decoloadscreen,decoglobal;

procedure load_test_level;

var scene:TcastleScene;
  player:TPlayer;

implementation

var loadedlevel:boolean=false;
procedure load_test_level;
begin
 if not loadedlevel then begin
  loadedlevel:=true;
  Scene := TCastleScene.Create(Application);
  Scene.Load(ApplicationData('level/test-level.x3d'));
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;
  //Scene.Attributes.EnableTextures:=false;

  DestroyLoadScreen;

  Player := TPlayer.Create(Window.SceneManager);
  Window.SceneManager.Items.Add(Player);
  Window.SceneManager.Player := Player;
  player.Camera.MouseLook:=false;
  Player.Camera.GravityUp:=Vector3Single(0,0,1);
  Player.Up:=Vector3Single(0,0,1);
  Window.scenemanager.camera:=player.camera;
  player.position:=Vector3Single(0,0,2);
  player.FallingEffect:=false;
  player.DefaultPreferredHeight:=1;

  Window.SceneManager.Items.Add(Scene);
  Window.SceneManager.MainScene := Scene;
 end;
end;

end.

