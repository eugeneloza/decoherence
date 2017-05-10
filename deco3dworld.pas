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

{ contains definitions for 3d World entity }

unit deco3dworld;

{$INCLUDE compilerconfig.inc}
{$DEFINE UseSwitches}
interface
uses fgl, castleVectors,
  X3DNodes, CastleScene,
  decoabstractworld, decoabstractgenerator,
  deconavigation, decoglobal;

type TRootList = specialize TFPGObjectList<TX3DRootNode>;
type TSceneList = specialize TFPGObjectList<TCastleScene>;
{list of ttransform nodes, reperesenting each tile in the dungeon}
type TTransformList = specialize TFPGObjectList<TTransformNode>;
{list of switch nodes wrapping each element of TTransformList}
{$IFDEF UseSwitches}type TSwitchList = specialize TFPGObjectList<TSwitchNode>;{$ENDIF}


type
  {World using 3D management and definitions,
   shared by interior and exterior worlds
   WARNING: it is abstract. Use DungeonWorld or Overworld.}
  D3dWorld = class(DAbstractWorld)
  protected
    {a list of transforms representing each generator step
     todo: unneeded after build and can be freed? (list only)
     todo: how to LODs?!!!}
    WorldObjects: TTransformList;
    {wrapper around transform node used for optimization
     and LOD management}
    {$IFDEF UseSwitches}WorldSwitches: TSwitchList;{$ENDIF}
    {list of tiles in neighbours groups}
    Groups: TIndexGroups;
    {list of root nodes, representing each neighbour group}
    WorldRoots: TRootList;
    {list of scenes, representing each neighbour group}
    WorldScenes: TSceneList;
    {loads world objects from HDD}
    procedure LoadWorldObjects; virtual; abstract;
    {assembles MapTiles from Tiles 3d, adding transforms nodes according to generator steps}
    procedure BuildTransforms; virtual; abstract;
    {wraps transforms into switches if enabled}
    {$IFDEF UseSwitches}procedure BuildSwitches;{$ENDIF}
    {assembles transforms/switches into a list of root nodes according to neighbours groups}
    procedure BuildRoots;
    {loads root nodes into scenes (according to neighbours groups)}
    procedure BuildScenes;
    {add a Node to a AbstractGrouping Node detecting
     and replacing placeholders as necessary (detected by IsPlaceholder function,
     processed by ParsePlaceholder function)
     At this moment dest/source can be only Grouping Nodes, maybe forever
     WARNING DEST must be a "fresh-created" node
             Otherwise nodes will be added to the already-existing tree
     WARNING only Children of the source node are added
             The root (container) node is not added
     WARNING placeholders cannot be children of children!
             Otherwise we'll have to recreate the whole nodes tree}
    procedure AddRecoursive(dest,source: TAbstractX3DGroupingNode);
  public
    procedure Activate; override;
    {builds current 3d world}
    procedure build; override;

    destructor destroy; override;
  end;

{temporary?
 if this node is a placeholder}
function IsPlaceholder(node: TX3DNode): boolean;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses sysutils,
  castlescenecore;

procedure D3dWorld.Activate;
var  i: integer;
begin
  inherited;
  Window.SceneManager.Items.Clear;
  Window.SceneManager.Items.Add(Navigation);
  Window.SceneManager.MainScene := Navigation;
  Window.SceneManager.Camera := nil; {$HINT check here for a correct way to free camera}
  Window.SceneManager.Camera := camera;

  for i := 0 to WorldScenes.count-1 do
    Window.sceneManager.Items.add(WorldScenes[i]);
end;

{------------------------------------------------------------------------------}

procedure D3dWorld.AddRecoursive(dest,source: TAbstractX3DGroupingNode);
var i: integer;
begin
  for i := 0 to source.FdChildren.Count-1 do
    if not isPlaceholder(source.FdChildren[i]) then
      dest.FdChildren.add(source.FdChildren[i])
    else
      {addRecoursive};
end;



{----------------------------------------------------------------------------}
{$IFDEF UseSwitches}
procedure D3dWorld.buildSwitches;
var i: integer;
  Switch: TSwitchNode;
begin
  WorldSwitches := TSwitchList.create(false); //scene will take care of freeing
  for i := 0 to WorldObjects.Count-1 do begin
    Switch := TSwitchNode.create;
    Switch.FdChildren.add(WorldObjects[i]);
    Switch.WhichChoice := 0;
    WorldSwitches.add(Switch);
  end;
end;
{$ENDIF}
{----------------------------------------------------------------------------}

procedure D3dWorld.buildRoots;
var i,j: integer;
  Root: TX3DRootNode;
begin
  WorldRoots := TRootList.create(false); //scene will take care of freeing, owns root
  for i := 0 to groups.count-1 do begin
    Root := TX3DRootNode.create;
    for j := 0 to groups[i].Count-1 do
      Root.FdChildren.Add({$IFDEF UseSwitches}WorldSwitches[groups[i].Items[j]]{$ELSE}WorldObjects[groups[i].Items[j]]{$ENDIF});
    WorldRoots.Add(Root);
  end;
end;

{----------------------------------------------------------------------------}

procedure D3dWorld.buildScenes;
var i: integer;
  Scene: TCastleScene;
begin
  WorldScenes := TSceneList.create(true); //list owns the scenes and will free them accordingly
  for i := 0 to WorldRoots.count-1 do begin
    Scene := TCastleScene.create(nil); //List will free the scenes, not freeing them automatically
    Scene.ShadowMaps := Shadow_maps_enabled;  {?????}
    Scene.Spatial := [ssRendering, ssDynamicCollisions];
    Scene.ProcessEvents := true;
    Scene.Load(WorldRoots[i],true);
    WorldScenes.add(scene);
  end;
end;

{----------------------------------------------------------------------------}

procedure D3dWorld.build;
begin
  inherited build;
  LoadWorldObjects;
  BuildTransforms;
  {$IFDEF UseSwitches}BuildSwitches;{$ENDIF}
  BuildRoots;
  BuildScenes;
end;

{----------------------------------------------------------------------------}

destructor D3dWorld.destroy;
begin
  //free 3d-related lists
  FreeAndNil(WorldScenes);
  FreeAndNil(WorldRoots);
  {$IFDEF UseSwitches}FreeAndNil(WorldSwitches);{$ENDIF}
  FreeAndNil(WorldObjects);

  inherited;
end;

{=============================== OTHER ROUTINES ===============================}

function IsPlaceholder(node: TX3DNode): boolean;
begin
  {$warning this is obsolete, and will interfere with collisions node}
  if copy(node.X3DName,1,1) = '(' then
    result := true
  else
    result := false;
end;


{------------------------------------------------------------------------------}

{procedure FreeRootList(List: TRootList);
var i: integer;
begin
  for i := 0 to List.count do FreeAndNil(List[i]);
end;}


end.

