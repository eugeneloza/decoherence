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

{ contains definitions for 3d World entity
  WARNING: it is abstract. Use DungeonWorld or Overworld.}

unit decoabstractworld3d;

{$INCLUDE compilerconfig.inc}
interface
uses classes, fgl, castleVectors,
  X3DNodes, CastleScene,
  decoabstractworld, decoabstractgenerator, deconodeparser,
  deconavigation, decoglobal;

{generic list of TX3DRootNodes, representing render groups}
type TRootList = specialize TFPGObjectList<TX3DRootNode>;
{generic list of TCastleScene, sync with TRootList}
type TSceneList = specialize TFPGObjectList<TCastleScene>;
{list of ttransform nodes, reperesenting each basic object in the world}
type TTransformList = specialize TFPGObjectList<TTransformNode>;
{$IFDEF UseSwitches}
{list of switch nodes wrapping each element of TTransformList}
type TSwitchList = specialize TFPGObjectList<TSwitchNode>;
{$ENDIF}

{Type PathElement = record
  {Absolute Coordinates of pathPoint}
  X,y,z: TCoordinate;
  {world tile it belongs to}
  Tile: TTileType;
  {Link of tiles adjacent to this tile}
  Links: TLinkList;
end;}

type
  {World using 3D management and definitions,
   shared by interior and exterior worlds}
  DAbstractWorld3d = class(DAbstractWorld)
  (*build*)
  protected
    {root nodes of the each tile
     MUST go synchronous with groups/neighbours!}
    WorldElements3d: TRootList;
    {List of world objects filenames (Absolute URLs!)}
    {$HINT MAY be freed after loading?}
    WorldElementsURL: TStringList;
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

    //!!!WorldCHUNKs here

    {loads world objects from HDD}
    procedure LoadWorldObjects;
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
    {loads the World into Window.SceneManager}
    procedure Activate; override;
    {builds current 3d world}
    procedure build; override;

    destructor destroy; override;
  end;

type
  {Abstract 3d world with rendering support,
   namely, rendering WorldObjects, groups and chunks into sprites
   most needed for overworld, maybe redundant for dungeon world}
  DAbstractWorldRendered = class(DAbstractWorld3d)
  (*render something into sprite*)
  //protected

  end;


type
  {manages appear and vanish lists
  by comparing previous and new neighbours lists
  will have access to protected fields as it is in the same unit}
  DAppearVanishManagerThread = class(TThread) // no need to report progress
  public
    {link to the world that requested management}
    //parent: DAbstractWorldManaged;
    {
     read-only, pass only links, doesn't free them even with destructor}

    {output
     warning! Internally managed, don't free manually}
    ObjectsAppear, ObjectsVanish,
    GroupsAppear, GroupsVanish: TIndexList;

    constructor create; {override;}
    destructor destroy; override;
  protected
    {actually preforms the requested task}
    procedure execute; override;
  end;

  {abstract world with worldObjects visibility management
      a good bonus for a dungeon and critical for overworld}
  DAbstractWorldManaged = class(DAbstractWorldRendered)
  (*manage*)
  protected
    {appea/vanish manager}
    AppearVanishManager: DAppearVanishManagerThread;  //note, recoursive link
    {initialize the appea/vanish manager in a thread, results are usually ready next render}
    procedure StartAppearVanishManagerThread;
  public
    constructor create; override;
    destructor destroy; override;
  end;

{creates a fresh empty copy of TTransformNode,
 WARNING: the result must be freed manually
 maybe should be moved somewhere to more abstract level?
 No need to publish it yet}
//function CopyTransform(const Source: TTransformNode): TTransformNode;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses sysutils, decoload3d, CastleLog,
  castlescenecore;

{============================ DAbstractWorld3D =============================}
{================================ MANAGE ===================================}

constructor DAppearVanishManagerThread.create;
begin
  inherited;
  ObjectsAppear := TIndexList.create;
  ObjectsVanish := TIndexList.create;
  GroupsAppear := TIndexList.create;
  GroupsVanish := TIndexList.create;
end;

{--------------------------------------------------------------------------}

destructor DAppearVanishManagerThread.destroy;
begin
  freeandnil(ObjectsAppear);
  freeandnil(ObjectsVanish);
  freeandnil(GroupsAppear);
  freeandnil(GroupsVanish);
  inherited;
end;

{--------------------------------------------------------------------------}

procedure DAppearVanishManagerThread.execute;
begin
  ObjectsAppear.clear;
  ObjectsVanish.clear;
  GroupsAppear.clear;
  GroupsVanish.clear;

  {prepare visibe lists,
   we can't assign the values directly, because groups can contain multiple tiles
   and the effect might be overlapping
   there should be no performance/memory issues with two additional local arrays, I hope
   however, some optimization here might come in handy some day}

  //*** IF PX0<0 then...

  //appear list
  //vanish list

  {
  repeat
  {  if old[i].tile = new[i].tile then ;
    else}
    if old[i].tile > new[j].tile then begin
      {there's a new tile to turn on}
      tile[new[j]] := true
      inc(j);
    end else
    if new[i].tile>old[j].tile then begin
    {there's an old tile to turn off}
      tile[old[i].tile] := false;
      inc(i);
    end else begin
      {the tile exists in both new and old neighbours lists, change nothing and advance to the next tile}
      inc(i);
      inc(j);
    end;
  until i>= old.count-1 and j>= new.count-1; {$warning check here}
  {now we actually put the calculated arrays into current 3d world}
  for i := 0 to group.count-1 do group[i] := false;
  for i := 0 to new.count-1 do group...
  }
end;

{--------------------------------------------------------------------------}

procedure DAbstractWorldManaged.StartAppearVanishManagerThread;
begin

end;

{--------------------------------------------------------------------------}

constructor DAbstractWorldManaged.create;
begin
  inherited;
  AppearVanishManager := DAppearVanishManagerThread.create;
  AppearVanishManager.Priority := tpNormal;  {$HINT maybe use tpLower}
end;

{--------------------------------------------------------------------------}

destructor DAbstractWorldManaged.destroy;
begin
  freeandnil(AppearVanishManager);
  inherited;
end;


{============================ DAbstractWorld3D =============================}
{================================ BUILD ====================================}

function CopyTransform(const Source: TTransformNode): TTransformNode;
begin
  Result := TTransformNode.create;
  Result.Translation := Source.Translation;
  Result.Scale := Source.Scale;
  Result.Rotation := Source.Rotation;
end;

{---------------------------------------------------------------------------}

procedure DAbstractWorld3d.Activate;
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

procedure DAbstractWorld3d.AddRecoursive(dest,source: TAbstractX3DGroupingNode);
var i: integer;
    Slot,Replacement: TTransformNode;
    Parsed: DNodeInfo;
begin
  for i := 0 to source.FdChildren.Count-1 do
    if not isPlaceholder(source.FdChildren[i]) then
      //add the node normally
      dest.FdChildren.add(source.FdChildren[i])
    else begin
      //replace the node with the actual placeholder
      Slot := source.FdChildren[i] as TTransformNode; //should fire an exception if this is wrong, we should have checked it in "isPlaceholder"
      Parsed := ParseNode(Slot);
      if RNDM.random<Parsed.rand then begin
        {$WARNING Memory Leak Here}
        Replacement := CopyTransform(Slot);
        //rotate
        //AddRecoursive(Replacement, GetPlaceholder(Parsed)); //plus symmetry groups
        //WriteLnLog(Parsed.placeholder);
        dest.FdChildren.add(Replacement);
      end;
    end;
end;


{----------------------------------------------------------------------------}

procedure DAbstractWorld3d.LoadWorldObjects;
var s: string;
  tmpRoot: TX3DRootNode;
begin
  WorldElements3d := TRootList.create(true);
  For s in WorldElementsURL do begin
    tmpRoot := LoadBlenderX3D(s);
    tmpRoot.KeepExisting := 1;   //List owns the nodes, so don't free them manually/automatically
    WorldElements3d.add(tmpRoot);
  end;
end;

{----------------------------------------------------------------------------}
{$IFDEF UseSwitches}
procedure DAbstractWorld3d.buildSwitches;
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

procedure DAbstractWorld3d.buildRoots;
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

procedure DAbstractWorld3d.buildScenes;
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

procedure DAbstractWorld3d.build;
begin
  inherited build;
  LoadWorldObjects;
  BuildTransforms;
  {$IFDEF UseSwitches}BuildSwitches;{$ENDIF}
  BuildRoots;
  BuildScenes;
end;

{----------------------------------------------------------------------------}

destructor DAbstractWorld3d.destroy;
begin
  //free 3d-related lists
  FreeAndNil(WorldScenes);
  FreeAndNil(WorldRoots);
  {$IFDEF UseSwitches}FreeAndNil(WorldSwitches);{$ENDIF}
  FreeAndNil(WorldObjects);
  FreeAndNil(WorldElements3d); //owns children, so will free them automatically
  FreeAndNil(WorldElementsURL);
  inherited;
end;

{=============================== OTHER ROUTINES ===============================}


{------------------------------------------------------------------------------}

{procedure FreeRootList(List: TRootList);
var i: integer;
begin
  for i := 0 to List.count do FreeAndNil(List[i]);
end;}


end.

