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

{ contains definitions for most abstract World entity }

unit DecoDungeonWorld;

{$INCLUDE compilerconfig.inc}
interface

uses Classes, Generics.Collections, CastleVectors,
  X3DNodes, CastleScene,
  DecoDungeonGenerator, DecoAbstractGenerator, DecoAbstractWorld3d,
  DecoDungeonTiles,
  DecoNavigationNetwork,
  DecoGlobal;


{list of "normal" tiles}
type TTilesList = specialize TObjectList<DTileMap>;

//type TContainer = TTransformNode;//{$IFDEF UseSwitches}TSwitchNode{$ELSE}TTransformNode{$ENDIF}

type
  {Dungeon world builds and manages any indoor tiled location}
  DDungeonWorld = class(DAbstractWorldManaged)
  private
    {some ugly fix for coordinate uninitialized at the beginning of the world}
    const UninitializedCoordinate = -1000000;
  private
    px,py,pz,px0,py0,pz0: TIntCoordinate;
    {converts x,y,z to px,py,pz and returns true if changed
     *time-critical procedure}
    function UpdatePlayerCoordinates(const x,y,z: float): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    {Manages tiles (show/hide/trigger events) *time-critical procedure}
    procedure ManageTiles; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
  protected
    {neighbours array}
    Neighbours: TNeighboursMapArray;
    {sequential set of tiles, to be added to the world during "build"}
    Steps: TGeneratorStepsArray;
    {assembles MapTiles from Tiles 3d, adding transforms nodes according to generator steps}
    procedure BuildTransforms; override;
  public
    {all-purpose map of the current world, also contains minimap image}
    Map: DMap;

    {Detects if the current tile has been changed and launches manage_tiles
     position is CAMERA.position}
    Procedure Manage(const Position: TVector3); override;
    {Sorts tiles into chunks}
    //Procedure chunk_n_slice; override;
    {loads the world from a running generator}
    procedure Load(const Generator: DAbstractGenerator); override;
    {loads the world from a saved file}
    procedure Load(const URL: string); override;

    {loads word scenes into scene manager}
    procedure Activate; override;

    { Dungeong world is flat, so it always returns (0,0,1)
      See also https://github.com/eugeneloza/decoherence/issues/76#issuecomment-321906640 }
    function GetGravity(const aPosition: TVector3): TVector3; override;
    function GetGravity(const aNav: TNavID): TVector3; override;

    constructor Create; override;
    destructor Destroy; override;
  end;


{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses SysUtils, CastleFilesUtils, CastleLog,
  DecoPlayerCharacter, DecoTime, DecoLog, Profiler;

procedure DDungeonWorld.ManageTiles; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  StartProfiler;
  {$WARNING dummy}
  StopProfiler;
end;

{----------------------------------------------------------------------------}

function DDungeonWorld.UpdatePlayerCoordinates(const x,y,z: float): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
//var nx,ny,nz: TIntCoordinate;
begin
  StartProfiler;

  {todo: may be optimized, at the moment we have here: 6 assignments,
   using nx,ny,nz we will have *MORE* work *IF* the tile has changed,
   but only 3 assignments otherwise}
  px0 := px;
  py0 := py;
  pz0 := pz;
  px := Round( x/WorldScale);
  py := Round(-y/WorldScale);     //pay attention: y-coordinate is inversed!
  pz := Round(-(z-1)/WorldScale); //pay attention: z-coordinate is inversed and shifted!
  if px<0 then px :=0 else if px>Map.SizeX-1 then px := map.SizeX-1;
  if py<0 then py :=0 else if py>Map.SizeY-1 then py := map.SizeY-1;
  if pz<0 then pz :=0 else if pz>Map.SizeZ-1 then pz := map.SizeZ-1;
  if (px<>px0) or (py<>py0) or (pz<>pz0) then {begin}
    {current tile has changed}
    Result := true
  {end}
  else
    {current tile hasn't changed}
    Result := false;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DDungeonWorld.Manage(const Position: TVector3);
begin
  StartProfiler;

  inherited Manage(Position);
  if FirstRender then begin
    {$warning reset all tiles visibility here}
    LastRender := decoNow; //GetNow or ResetNowThread
    FirstRender := false;
  end;

  if UpdatePlayerCoordinates(Position[0],Position[1],Position[2]) then
    ManageTiles;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

function DDungeonWorld.GetGravity(const aPosition: TVector3): TVector3;
begin
  StartProfiler;
  Result := Vector3(0,0,1);
  StopProfiler;
end;
function DDungeonWorld.GetGravity(const aNav: TNavID): TVector3;
begin
  StartProfiler;
  Result := Vector3(0,0,1);
  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DDungeonWorld.Load(const Generator: DAbstractGenerator);
var DG: D3dDungeonGenerator;
begin
  StartProfiler;

  inherited Load(Generator);
  fGravityAcceleration := 10;
  DG := Generator as D3dDungeonGenerator;
  Map := DG.ExportMap;
  Neighbours := DG.ExportNeighbours;
  Steps := DG.ExportSteps;
  {$HINT this is ugly. WorldScale must support different scales}
  WorldScale := TileScale*MyScale;
  RescaleNavigationNetwork;

  {yes, we load tiles twice in this case (once in the generator and now here),
   however, loading from generator is *not* a normal case
   (used mostly for testing in pre-alpha), so such redundancy will be ok
   normally World should load from a file and loading tiles will happen only once}
  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DDungeonWorld.Load(const URL: string);
begin
  StartProfiler;
  {$Warning dummy}
  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DDungeonWorld.Activate;
begin
  StartProfiler;

  inherited Activate;
  {$WARNING todo}
  Player.CurrentParty.TeleportTo(Weenies[0].NavId);

  {$WARNING this is wrong}
  {why does it get a wrong GRAVITY_UP if called from Self.Activate?????}
  SpawnActors; //must go after BuildNav;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DDungeonWorld.BuildTransforms;
var i: integer;
  Transform: TTransformNode;
begin
  StartProfiler;

  WorldObjects := TTransformList.Create(false); //scene will take care of freeing
  for i := 0 to High(Steps) do begin
    Transform := TTransformNode.Create;
    //put current tile into the world. Pay attention to y and z coordinate inversion.
    Transform.Translation := Vector3(WorldScale*(Steps[i].x),-WorldScale*(Steps[i].y),-WorldScale*(steps[i].z));
    {$Warning this is ugly}
    Transform.Scale := Vector3(MyScale,MyScale,MyScale);
    AddRecoursive(Transform,WorldElements3d[Steps[i].Tile]);
    WorldObjects.Add(Transform);
  end;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

constructor DDungeonWorld.Create;
begin
  StartProfiler;

  inherited Create;
  px  := UninitializedCoordinate;
  py  := UninitializedCoordinate;
  pz  := UninitializedCoordinate;
  px0 := UninitializedCoordinate;        //we use px0 := px at the moment, so these are not needed, but it might change in future
  py0 := UninitializedCoordinate;
  pz0 := UninitializedCoordinate;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

destructor DDungeonWorld.Destroy;
begin
  StartProfiler;

  {$hint freeandnil(window.scenemanager)?}
  
  //free basic map parameters
  FreeAndNil(Map);
  FreeAndNil(Groups);
  FreeNeighboursMap(Neighbours);

  inherited Destroy;

  StopProfiler;
end;

end.

