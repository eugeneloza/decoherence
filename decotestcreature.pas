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
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses decoinputoutput;

procedure InitCreatures;
begin
  Resources.LoadSafe(ApplicationData('models/creatures/knight_creature/'));
  Resources.FindName('Knight').ConfigAlwaysPrepared := true;  {$hint doesn't work}
  CreatureResource := Resources.FindName('Knight') as TCreatureResource;
  creatureResource.ConfigAlwaysPrepared := true;  {$hint doesn't work}
  //CreatureResource.Prepare(nil,vector3single(0,0,1));
end;

{---------------------------------------------------------------------------}

procedure SpawnCreatures;
const scale = 2*3;
begin
  CreatureResource.CreateCreature(window.SceneManager.Items, vector3single((4-1)*scale,-(4)*scale,0), Vector3Single(1,0,0));
  CreatureResource.CreateCreature(window.SceneManager.Items, vector3single((4)*scale,-(4-1)*scale,0), Vector3Single(1,0,0));
  CreatureResource.CreateCreature(window.SceneManager.Items, vector3single((4+1)*scale,-(4)*scale,0), Vector3Single(1,0,0));
  CreatureResource.CreateCreature(window.SceneManager.Items, vector3single((4)*scale,-(4+1)*scale,0), Vector3Single(1,0,0));
end;

{---------------------------------------------------------------------------}

procedure FreeCreatures;
begin
  //freeandnil(CreatureResource);   //automatically done
end;

end.

