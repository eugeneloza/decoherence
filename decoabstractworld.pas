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

unit decoabstractworld;

{$INCLUDE compilerconfig.inc}
interface

uses CastleRandom, fgl, castleVectors,
  X3DNodes, CastleScene,
  decoabstractgenerator,
  deconavigation, decoglobal;

type TRootList = specialize TFPGObjectList<TX3DRootNode>;
type TSceneList = specialize TFPGObjectList<TCastleScene>;

{Type PathElement = record
  {Absolute Coordinates of pathPoint}
  X,y,z: TCoordinate;
  {world tile it belongs to}
  Tile: TTileType;
  {Link of tiles adjacent to this tile}
  Links: TLinkList;
end;}

Type
  {The most abstract world implementation to merge independent world routines
   Like manage and pathfind}
  DAbstractWorld = class
  protected
    fSeed: LongWord;
    { xorshift random generator, fast and thread-safe }
    RNDM: TCastleRandom;
    LastRender: TDateTime;
    FirstRender: boolean;
  protected
    {add a Node to a AbstractGrouping Node detecting
     and replacing placeholders as necessary (detected by IsPlaceholder function)
     At this moment dest/source can be only Grouping Nodes, maybe forever
     WARNING DEST must be a "fresh-created" node
     WARNING only Children of the source node are added
     WARNING placeholders cannot be children of children!
             Otherwise we'll have to recreate the whole nodes tree}
    procedure AddRecoursive(dest,source: TAbstractX3DGroupingNode);
  {*** "interface" section ***}
  public
    {Seed used to "build" the world if it requires random}
    property Seed: LongWord read fSeed write fSeed;
    {World management routine. Called every frame;
     Most important thing it does is managing LODs of tiles/landscape
     And hiding/LODding world chunks
     x,y,z are current world coordinates of render camera}
    Procedure manage(position: TVector3Single); virtual; abstract;
    {Builds a PathTree for the world}
    //Function pathfind: DPathTree;
    {load the World from a file}
    procedure Load(URL: string); virtual; abstract;
    {load the World from a running Generator}
    procedure Load(Generator: DAbstractGenerator); virtual; abstract;
    {builds a world from the obtained data }
    procedure Build; virtual;
    {activates the current world. Caution it will modify Window.SceneManager!}
    procedure Activate; virtual;
    {Splits the World into chunks}
    //Procedure chunk_n_slice; virtual; abstract;
    constructor create; virtual;
    destructor destroy; override;
  End;

var CurrentWorld: DAbstractWorld;


procedure FreeWorld;

{temporary?
 if this node is a placeholder}
function IsPlaceholder(node: TX3DNode): boolean;

{procedure FreeRootList(List: TRootList);} //unneeded as list "owns" children
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, classes, castlelog
  ;

constructor DAbstractWorld.create;
begin
  inherited;
  {we create an non-initialized random (i.e. initialized by a stupid constant integer)
  Just to make sure we don't waste any time (<1ms) on initialization now}
  RNDM := TCastleRandom.Create(1);
  LastRender := -1;
  FirstRender := true;
end;

{------------------------------------------------------------------------------}

destructor DAbstractWorld.destroy;
begin
  FreeAndNil(RNDM);
  inherited;
end;

{------------------------------------------------------------------------------}

procedure DAbstractWorld.Build;
begin
  if fSeed = 0 then raise exception.create('DAbstractWorld.Build: World random must be predefined!');
  RNDM.Initialize(fSeed);
end;

{------------------------------------------------------------------------------}

Procedure DAbstractWorld.Activate;
begin
  WriteLnLog('DAbstractWorld.Activate','Activating the world.');
  firstRender := true;
  Window.SceneManager.Items.Clear;
  Window.SceneManager.Items.Add(Navigation);
  Window.SceneManager.MainScene := Navigation;
  Window.SceneManager.Camera := nil; {$HINT check here for a correct way to free camera}
  Window.SceneManager.Camera := camera;

end;

{------------------------------------------------------------------------------}


procedure DAbstractWorld.AddRecoursive(dest,source: TAbstractX3DGroupingNode);
var i: integer;
begin
  for i := 0 to source.FdChildren.Count-1 do
    if not isPlaceholder(source.FdChildren[i]) then
      dest.FdChildren.add(source.FdChildren[i])
    else
      {addRecoursive};
end;


{------------------------------------------------------------------------------}

{procedure FreeRootList(List: TRootList);
var i: integer;
begin
  for i := 0 to List.count do FreeAndNil(List[i]);
end;}

{------------------------------------------------------------------------------}

function IsPlaceholder(node: TX3DNode): boolean;
begin
  {$warning this is obsolete, and will interfere with collisions node}
  if copy(node.X3DName,1,1) = '(' then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}

procedure FreeWorld;
begin
  FreeAndNil(CurrentWorld);
end;

end.

