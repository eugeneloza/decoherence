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

unit DecoAbstractWorld3d;

{$INCLUDE compilerconfig.inc}
interface
uses Classes, Generics.Collections, CastleVectors, X3DNodes, CastleScene,
  DecoAbstractWorld, DecoAbstractGenerator, DecoNodeParser,
  DecoActor,
  DecoNavigation, DecoGlobal;

{generic list of TX3DRootNodes, representing render groups}
type TRootList = specialize TObjectList<TX3DRootNode>;
{generic list of TCastleScene, sync with TRootList}
type TSceneList = specialize TObjectList<TCastleScene>;
{list of ttransform nodes, reperesenting each basic object in the world}
type TTransformList = specialize TObjectList<TTransformNode>;
{$IFDEF UseSwitches}
{list of switch nodes wrapping each element of TTransformList}
type TSwitchList = specialize TObjectList<TSwitchNode>;
{$ENDIF}


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
    procedure AddRecoursive(const Dest,Source: TAbstractX3DGroupingNode);
  public
    {list of actors in the world}
    Actors: TActorList;
  protected
    procedure SpawnActors; override;
  public
    {loads the World into Window.SceneManager}
    procedure Activate; override;
    {builds current 3d world}
    procedure Build; override;
    { Load the World from a running Generator }
    procedure Load(const Generator: DAbstractGenerator); override;
    procedure Load(const URL: string); override;
    procedure RescaleNavigationNetwork;

    procedure Manage(const Position: TVector3); override;

    { turns on or off SceneManager.Exists
      This should not be called in Rendered World types }
    procedure ToggleSceneManager(const Value: boolean);

    destructor Destroy; override;
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
  DAppearVanishManagerThread = class(TThread)
  public
    {link to the world that requested management}
    //parent: DAbstractWorldManaged;
    {
     read-only, pass only links, doesn't free them even with destructor}

    {output
     warning! Internally managed, don't free manually}
    ObjectsAppear, ObjectsVanish,
    GroupsAppear, GroupsVanish: TIndexList;

    constructor Create; //virtual;{override;}
    destructor Destroy; override;
  protected
    {actually preforms the requested task}
    procedure Execute; override;
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
    constructor Create; override;
    destructor Destroy; override;
  end;

{creates a fresh empty copy of TTransformNode,
 WARNING: the result must be freed manually
 maybe should be moved somewhere to more abstract level?
 No need to publish it yet}
//function CopyTransform(const Source: TTransformNode): TTransformNode;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, DecoLoad3d,
  DecoNavigationNetwork, DecoPlayerCharacter,
  DecoActorBody,
  CastleSceneCore,
  DecoLog, Profiler;

{============================ DAbstractWorld3D =============================}
{================================ MANAGE ===================================}

{Warning! No profiler for threaded objects}

constructor DAppearVanishManagerThread.Create;
begin
  //inherited Create;
  ObjectsAppear := TIndexList.Create;
  ObjectsVanish := TIndexList.Create;
  GroupsAppear  := TIndexList.Create;
  GroupsVanish  := TIndexList.Create;
end;

{--------------------------------------------------------------------------}

destructor DAppearVanishManagerThread.Destroy;
begin
  FreeAndNil(ObjectsAppear);
  FreeAndNil(ObjectsVanish);
  FreeAndNil(GroupsAppear);
  FreeAndNil(GroupsVanish);
  inherited Destroy;
end;

{--------------------------------------------------------------------------}

procedure DAppearVanishManagerThread.Execute;
begin
  ObjectsAppear.Clear;
  ObjectsVanish.Clear;
  GroupsAppear.Clear;
  GroupsVanish.Clear;

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
  {  if old[i].Tile = New[i].Tile then ;
    else}
    if old[i].Tile > New[j].Tile then begin
      {there's a new tile to turn on}
      Tile[New[j]] := true
      inc(j);
    end else
    if New[i].Tile>Old[j].Tile then begin
    {there's an old tile to turn off}
      Tile[Old[i].Tile] := false;
      inc(i);
    end else begin
      {the tile exists in both new and old neighbours lists, change nothing and advance to the next tile}
      inc(i);
      inc(j);
    end;
  until i>= Old.Count-1 and j>= New.Count-1; {$warning check here}
  {now we actually put the calculated arrays into current 3d world}
  for i := 0 to Group.Count-1 do Group[i] := false;
  for i := 0 to New.Count-1 do Group...
  }
end;

{--------------------------------------------------------------------------}

procedure DAbstractWorldManaged.StartAppearVanishManagerThread;
begin
  StartProfiler;
  {$warning todo}
  StopProfiler;
end;

{--------------------------------------------------------------------------}

constructor DAbstractWorldManaged.Create;
begin
  StartProfiler;

  inherited Create;
  AppearVanishManager := DAppearVanishManagerThread.Create;
  AppearVanishManager.Priority := tpNormal;  {$HINT maybe use tpLower}

  StopProfiler;
end;

{--------------------------------------------------------------------------}

destructor DAbstractWorldManaged.Destroy;
begin
  StartProfiler;

  FreeAndNil(AppearVanishManager);
  inherited Destroy;

  StopProfiler;
end;


{============================ DAbstractWorld3D =============================}
{================================ BUILD ====================================}

function CopyTransform(const Source: TTransformNode): TTransformNode;
begin
  StartProfiler;

  Result := TTransformNode.Create;
  Result.Translation := Source.Translation;
  Result.Scale := Source.Scale;
  Result.Rotation := Source.Rotation;

  StopProfiler;
end;

{---------------------------------------------------------------------------}

procedure DAbstractWorld3d.Load(const Generator: DAbstractGenerator);
begin
  StartProfiler;

  inherited Load(Generator);
  Groups := Generator.ExportGroups;
  WorldElementsURL := Generator.ExportTiles;

  StopProfiler;
end;

{---------------------------------------------------------------------------}

procedure DAbstractWorld3d.RescaleNavigationNetwork;
var i: integer;
begin
  StartProfiler;

  {$hint I don't like that!}
  for i := 0 to Nav.Count-1 do begin
{
    Nav.L[i].Pos[0] *=  WorldScale;
    Nav.L[i].Pos[1] *= -WorldScale;
    Nav.L[i].Pos[2] *= -WorldScale;
}
    Nav.L[i].Pos[0] :=  WorldScale * Nav.L[i].Pos[0];
    Nav.L[i].Pos[1] := -WorldScale * Nav.L[i].Pos[1];
    Nav.L[i].Pos[2] := -WorldScale * Nav.L[i].Pos[2];
  end;

  StopProfiler;
end;

{---------------------------------------------------------------------------}

procedure DAbstractWorld3d.Load(const URL: string);
begin
  StartProfiler;

  inherited Load(URL);
  //load groups and WorldElementsList

  StopProfiler;
end;

{---------------------------------------------------------------------------}

procedure DAbstractWorld3d.Activate;
var  i: integer;
begin
  StartProfiler;

  inherited Activate;
  Window.SceneManager.Items.Clear;
  Window.SceneManager.Items.Add(Navigation);
  Window.SceneManager.MainScene := Navigation;
  Window.SceneManager.Camera := nil; {$HINT check here for a correct way to free camera}
  Window.SceneManager.Camera := Camera;

  for i := 0 to WorldScenes.Count-1 do
    Window.SceneManager.Items.Add(WorldScenes[i]);

  StopProfiler;
end;

{------------------------------------------------------------------------------}

procedure DAbstractWorld3d.AddRecoursive(const Dest, Source: TAbstractX3DGroupingNode);
var i: integer;
    Slot,Replacement: TTransformNode;
    Parsed: DNodeInfo;
begin
  StartProfiler;

  for i := 0 to Source.FdChildren.Count-1 do
    if not isPlaceholder(Source.FdChildren[i]) then
      //add the node normally
      Dest.FdChildren.Add(Source.FdChildren[i])
    else begin
      //replace the node with the actual placeholder
      Slot := Source.FdChildren[i] as TTransformNode; //should fire an exception if this is wrong, we should have checked it in "isPlaceholder"
      Parsed := ParseNode(Slot);
      if RNDM.random<Parsed.rand then begin
        {$WARNING Memory Leak Here}
        Replacement := CopyTransform(Slot);
        //rotate
        //AddRecoursive(Replacement, GetPlaceholder(Parsed)); //plus symmetry groups
        //dLog(Parsed.placeholder);
        Dest.FdChildren.Add(Replacement);
      end;
    end;

  StopProfiler;
end;


{----------------------------------------------------------------------------}

procedure DAbstractWorld3d.LoadWorldObjects;
var s: string;
  tmpRoot: TX3DRootNode;
begin
  StartProfiler;

  WorldElements3d := TRootList.Create(true);
  For s in WorldElementsURL do begin
    tmpRoot := LoadBlenderX3D(s);
    tmpRoot.KeepExisting := 1;   //List owns the nodes, so don't free them manually/automatically
    WorldElements3d.Add(tmpRoot);
  end;

  StopProfiler;
end;

{----------------------------------------------------------------------------}
{$IFDEF UseSwitches}
procedure DAbstractWorld3d.BuildSwitches;
var i: integer;
  Switch: TSwitchNode;
begin
  StartProfiler;

  WorldSwitches := TSwitchList.Create(false); //scene will take care of freeing
  for i := 0 to WorldObjects.Count-1 do begin
    Switch := TSwitchNode.Create;
    Switch.FdChildren.add(WorldObjects[i]);
    Switch.WhichChoice := 0;
    WorldSwitches.Add(Switch);
  end;

  StopProfiler;
end;
{$ENDIF}
{----------------------------------------------------------------------------}

procedure DAbstractWorld3d.BuildRoots;
var i,j: integer;
  Root: TX3DRootNode;
begin
  StartProfiler;

  WorldRoots := TRootList.Create(false); //scene will take care of freeing, owns root
  for i := 0 to Groups.Count-1 do begin
    Root := TX3DRootNode.Create;
    for j := 0 to Groups[i].Count-1 do
      Root.FdChildren.Add({$IFDEF UseSwitches}WorldSwitches[Groups[i].Items[j]]{$ELSE}WorldObjects[Groups[i].Items[j]]{$ENDIF});
    WorldRoots.Add(Root);
  end;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DAbstractWorld3d.BuildScenes;
var i: integer;
    Scene: TCastleScene;
begin
  StartProfiler;

  WorldScenes := TSceneList.Create(true); //list owns the scenes and will free them accordingly
  for i := 0 to WorldRoots.Count-1 do begin
    Scene := TCastleScene.Create(nil); //List will free the scenes, not freeing them automatically
    Scene.ShadowMaps := ShadowMapsEnabled;  {?????}
    Scene.Spatial := [ssRendering, ssDynamicCollisions];
    Scene.ProcessEvents := true;
    Scene.Load(WorldRoots[i],true);
    WorldScenes.Add(Scene);
  end;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DAbstractWorld3d.SpawnActors;
var i: integer;
    n: TNavID;
    a: DMonster;
begin
  StartProfiler;

  if Actors <> nil then begin
    Log(LogWorldInitSoftError,_CurrentRoutine,'WARNING: Actors is not nil, freeing...');
    FreeAndNil(Actors);
  end;
  Actors := TActorList.Create(true);

  {$hint some rules on actors spawning should go here}
  for i := 0 to Round(Nav.Count*NEnemies) do begin
    repeat
      n := DRND.Random(Nav.Count);
    until (Nav[n].Blocked = false) and (Nav[n].isSafe = false);
    a := DMonster.Create;
    {$Warning todo}
    a.Target := Player.CurrentParty.Character[0];
    {monsters are centered by "underfeet" point}
    a.Spawn(n, tmpKnightCreature);
    Actors.Add(a);
  end;

  StopProfiler;
end;

{----------------------------------------------------------------------------}

procedure DAbstractWorld3d.Build;
begin
  StartProfiler;

  inherited Build;
  LoadWorldObjects;
  BuildTransforms;
  {$IFDEF UseSwitches}BuildSwitches;{$ENDIF}
  BuildRoots;
  BuildScenes;

  StopProfiler;
end;

{------------------------------------------------------------------------------}

Procedure DAbstractWorld3d.Manage(const Position: TVector3);
var a: DSimpleActor;
begin
  StartProfiler;

  //inherited; --- nothing to inherit yet

  //manage all actors in the World
  if Actors<>nil then
    for a in Actors do a.Manage;

  StopProfiler;
end;

{------------------------------------------------------------------------------}


Procedure DAbstractWorld3d.ToggleSceneManager(const Value: boolean);
begin
  StartProfiler;
  Window.SceneManager.Exists := Value;
  StopProfiler;
end;


{----------------------------------------------------------------------------}

destructor DAbstractWorld3d.Destroy;
begin
  StartProfiler;

  //free 3d-related lists
  FreeAndNil(WorldScenes);
  FreeAndNil(WorldRoots);
  {$IFDEF UseSwitches}FreeAndNil(WorldSwitches);{$ENDIF}
  FreeAndNil(WorldObjects);
  FreeAndNil(WorldElements3d); //owns children, so will free them automatically
  FreeAndNil(WorldElementsURL);
  FreeAndNil(Actors);           //owns children, so will free them automatically (! might conflict);
  inherited Destroy;

  StopProfiler;
end;

{=============================== OTHER ROUTINES ===============================}


{------------------------------------------------------------------------------}

{procedure FreeRootList(List: TRootList);
var i: integer;
begin
  StartProfiler;
  for i := 0 to List.count do FreeAndNil(List[i]);
  StopProfiler;
end;}


end.

