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

{ Temporary unit to test animated creatures loading }   
unit decotestcreature;

{$INCLUDE compilerconfig.inc}

interface

uses
  Classes, SysUtils,
  CastleFilesUtils, CastleVectors,

  CastleResources, CastleCreatures,

  DecoInputOutput,
  DecoGlobal;


var CreatureResource: TCreatureResource;

procedure InitCreatures;
procedure FreeCreatures;
procedure SpawnCreatures;
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
//uses decoinputoutput;
uses CastleLog,
  DecoDungeonWorld, DecoNav, DecoAbstractWorld;

procedure InitCreatures;
begin
  Resources.LoadSafe(ApplicationData('models/creatures/knight_creature/'));
  CreatureResource := Resources.FindName('Knight') as TCreatureResource;
  //CreatureResource.PrepareSafe;
end;

{---------------------------------------------------------------------------}

procedure SpawnCreatures;
var i: integer;
    n: DNavPt;
    DW: DDungeonWorld;
begin
  //temporary - unsafe
  DW := CurrentWorld as DDungeonWorld;
  for i := 0 to DW.Nav.Count div 10 do begin
    n := DW.Nav[DRND.Random(DW.Nav.Count)];
    WriteLnLog('Adding creature');
    CreatureResource.CreateCreature(window.SceneManager.Items, Vector3(n.x*DW.WorldScale,-n.y*DW.WorldScale,-n.z*DW.WorldScale+1*DW.MyScale), Vector3(1,0,0));
  end;
//  WriteLnLog('Adding creature');
end;

{---------------------------------------------------------------------------}

procedure FreeCreatures;
begin
  //freeandnil(CreatureResource);   //automatically done
end;

end.

