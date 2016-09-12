unit decolevel;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,CastleWindow, CastleSceneCore, CastleScene, CastleFilesUtils,
  castlePlayer, castleVectors,
  decoloadscreen,global_var;

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
  Scene.Load(ApplicationData('level'+pathdelim+'test-level.x3d'));
  writeln(ApplicationData('level'+pathdelim+'test-level.x3d'));
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;

  DestroyLoadScreen;

  Player := TPlayer.Create(Window.SceneManager);
  Window.SceneManager.Items.Add(Player);
  Window.SceneManager.Player := Player;
  player.Camera.MouseLook:=true;
  Player.Camera.GravityUp:=Vector3Single(0,0,1);
  Player.Up:=Vector3Single(0,0,1);
  Window.scenemanager.camera:=player.camera;
  player.position:=Vector3Single(0,0,1);
  player.FallingEffect:=false;
  player.DefaultPreferredHeight:=1;


  Window.SceneManager.Items.Add(Scene);
  Window.SceneManager.MainScene := Scene;
 end;
end;

end.

