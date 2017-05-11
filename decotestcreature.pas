unit decotestcreature;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  CastleFilesUtils, CastleVectors,

  CastleResources, CastleCreatures,

  decoglobal;


var CreatureResource: TCreatureResource;

procedure InitCreatures;
procedure FreeCreatures;
procedure SpawnCreatures;
implementation

procedure InitCreatures;
begin
  Resources.LoadFromFiles(ApplicationData('models/creatures/knight_creature/'), true);
  CreatureResource := Resources.FindName('Knight') as TCreatureResource;
  //CreatureResource.Prepare(nil,vector3single(0,0,1));
end;

procedure SpawnCreatures;
const scale = 2*3;
begin
  CreatureResource.CreateCreature(window.SceneManager.Items, vector3single((4-1)*scale,-(4)*scale,0), Vector3Single(1,0,0));
  CreatureResource.CreateCreature(window.SceneManager.Items, vector3single((4)*scale,-(4-1)*scale,0), Vector3Single(1,0,0));
  CreatureResource.CreateCreature(window.SceneManager.Items, vector3single((4+1)*scale,-(4)*scale,0), Vector3Single(1,0,0));
  CreatureResource.CreateCreature(window.SceneManager.Items, vector3single((4)*scale,-(4+1)*scale,0), Vector3Single(1,0,0));
end;

procedure FreeCreatures;
begin
  //freeandnil(CreatureResource);
end;

end.

