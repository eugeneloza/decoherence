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

unit DecoAbstractWorld;

{$INCLUDE compilerconfig.inc}
interface

uses CastleRandom, CastleVectors,
  DecoAbstractGenerator, DecoNavigationNetwork,
  DecoGlobal;

Type
  {The most abstract World implementation.
   Used as external interface for the World}
  DAbstractWorld = class(DObject)
  private
    { Average and Min distance between nav points
      Warning, this might go very,very wrong if 2 nav poinst are too close }
    fNavMinStep, fNavAvgStep: float;
    procedure CacheNavDistance;
  protected
    fSeed: LongWord;
    { xorshift random generator, fast and thread-safe
      used by BUILD (maybe freed afterwards?) }
    RNDM: TCastleRandom;
    {time of last render. Used for FPS management}
    LastRender: TDateTime;
    { Is this render the first one?
      Additional initialization procedures may be required }
    {$HINT todo: LastRender = -1 does the same job, optimize it}
    {$HINT allow first initialization flow during load without caring for FPS!}
    FirstRender: boolean;
    fGravityAcceleration: float;
  protected
    { Spawns Actors in the World }
    procedure SpawnActors; virtual; abstract;
  public
    { Acceleration of gravity in this World
      Pay attention, nothing special happens (yet) if it is zero
      in m/s^2 }
    property GravityAcceleration: float read fGravityAcceleration;
    { Seed used to "build" the World if it requires random}
    property Seed: LongWord read fSeed write fSeed;
    { World management routine. Called every frame;
      Most important thing it does is managing LODs of tiles/landscape
      And hiding/LODding World chunks
      x,y,z are current World coordinates of render camera }
    Procedure Manage(Position: TVector3); virtual; abstract;
    {Builds a PathTree for the World}
    //Function pathfind: DPathTree;
    { Load the World from a file}
    procedure Load(URL: string); virtual;
    { Load the World from a running Generator }
    procedure Load(Generator: DAbstractGenerator); virtual;
    { Builds a World from the obtained data }
    procedure Build; virtual;
    { activates the current World.
      Caution, it might and will modify Window.SceneManager! }
    procedure Activate; virtual;
    { Splits the World into chunks }
    //Procedure chunk_n_slice; virtual; abstract;

    { Returns Gravity Up for the given location in the World
      Maybe, move this one to AbstractWorld3D, as it's not needed in non-3D World
      it's bad, that there is no way to make this procedure inline,
      because it might be important to keep it efficiently
      However, for now it's used only for spawning process, so don't bother yet}
    function GetGravity(aPosition: TVector3): TVector3; virtual; abstract;
    function GetGravity(aNav: TNavID): TVector3; virtual; abstract;

    { A dummy procedure to be overriden in rendered World types
     (such as text or 2D)}
    procedure Render; virtual;

    constructor Create; virtual;
    destructor Destroy; override;

  public
    { scaling factor, required to set up FOV properly / or a bug? }
    const myScale = 3;
    {scale used to define a tile size. Usually 1 is man-height.
      CAUTION this scale must correspond to tiles model scale, otherwise it'll mess everything up}
    var WorldScale: float;
  public
    Nav: TNavList;
    Weenies: TWeeniesList;
    function NavToVector3(aNav: TNavID): TVector3;
    function PositionToNav(aPosition: TVector3): TNavID;
    procedure BlockNav(aNav: TNavID);
    procedure ReleaseNav(aNav: TNavID);
    procedure ClearNavBlocks;
  end;

var CurrentWorld: DAbstractWorld;

procedure FreeWorld;


{procedure FreeRootList(List: TRootList);} //unneeded as list "owns" children
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, Classes,
  DecoLog;

constructor DAbstractWorld.Create;
begin
  inherited Create;
  Log(LogWorldInit,{$I %CURRENTROUTINE%},'Creating the World');
  {we create an non-initialized random (i.e. initialized by a stupid constant integer)
  Just to make sure we don't waste any time (<1ms) on initialization now}
  RNDM := TCastleRandom.Create(1);

  fNavMinStep := -1;
end;

{------------------------------------------------------------------------------}

destructor DAbstractWorld.Destroy;
begin
  Log(LogWorldInit,{$I %CURRENTROUTINE%},'Freeing the World');
  FreeAndNil(RNDM);
  FreeAndNil(Nav);
  FreeAndNil(Weenies);
  inherited Destroy;
end;

{------------------------------------------------------------------------------}

procedure DAbstractWorld.Build;
begin
  if fSeed = 0 then raise Exception.Create('DAbstractWorld.Build: World random must be predefined!');
  RNDM.Initialize(fSeed);
end;

{------------------------------------------------------------------------------}

Procedure DAbstractWorld.Activate;
begin
  Log(LogWorldInit,{$I %CURRENTROUTINE%},'Activating the World.');
  if (CurrentWorld<>nil) and (CurrentWorld<>Self) then
    raise Exception.Create('ERROR: Free and nil the previous World before activating this one!');
  LastRender := -1;
  FirstRender := true;
end;

{------------------------------------------------------------------------------}

Procedure DAbstractWorld.Render;
begin
  {this is an abstract routine,
  it must be overridden by DRenderedWorld}
  Log(LogWorldError,{$I %CURRENTROUTINE%},'Warning: This shouldn''t happen in normal situation, it''s abstract');
end;

{------------------------------------------------------------------------------}

function DAbstractWorld.NavToVector3(aNav: TNavID): TVector3;
begin
  Result := Nav[aNav].Pos;
end;

{------------------------------------------------------------------------------}

procedure DAbstractWorld.CacheNavDistance;
var i,j: TNavID;
    Count: integer;
    d, min, sum: float;
begin
  //warning, this will significantly changed after "LINKS" between navs will be implemented
  min := (Nav[0].Pos-Nav[1].Pos).Length;
  sum := min;
  Count := 0;
  for i := 0 to Nav.Count-1 do
    for j := 0 to Nav[i].LinksCount do begin
      d := (Nav[i].Pos-Nav[Nav[i].Links[j]].Pos).Length;
      if d < min then min := d;
      sum += d;
      inc(Count);
    end;
  fNavAvgStep := sum/Count;
  fNavMinStep := min;
end;
function DAbstractWorld.PositionToNav(aPosition: TVector3): TNavID;
var i, m: TNavID;
    d, min_d: float;
begin
  if fNavMinStep<0 then CacheNavDistance; //maybe throw this into BuildNav?
  min_d := (aPosition-Nav[0].Pos).Length;
  m := 0;
  { we've already included "0" in the min, starting from "1" }
  if min_d > fNavMinStep/2 then
    for i := 1 to Nav.Count-1 do begin
      d := (aPosition-Nav[i].Pos).Length;
      if d <= fNavMinStep/2 then begin
        m := i;
        Break;
      end else
      if d < min_d then begin
        m := i;
        min_d := d;
      end;
    end;
  Result := m;
end;

{------------------------------------------------------------------------------}

procedure DAbstractWorld.BlockNav(aNav: TNavId);
begin
  Nav.L[aNav].Blocked := true;
end;

{------------------------------------------------------------------------------}

procedure DAbstractWorld.ReleaseNav(aNav: TNavId);
begin
  Nav.L[aNav].Blocked := false;
end;

{------------------------------------------------------------------------------}

procedure DAbstractWorld.ClearNavBlocks;
var i: integer;
begin
  //actually, it seems redundant for now
  for i := 0 to Nav.Count-1 do Nav.L[i].Blocked := false;
end;

{------------------------------------------------------------------------------}

procedure DAbstractWorld.Load(Generator: DAbstractGenerator);
begin
  if Generator = nil then raise Exception.Create('DAbstractWorld.Load: Generator is nil!');
  fSeed := drnd.Random32bit; //maybe other algorithm?
  if Nav<>nil then begin
    Log(LogWorldInitSoftError,{$I %CURRENTROUTINE%},'WARNING: Nav is not nil! Freeing...');
    FreeAndNil(Nav);
  end;
  Nav := Generator.ExportNav;
  ClearNavBlocks;
  Weenies := Generator.ExportWeenies;
end;

{------------------------------------------------------------------------------}

procedure DAbstractWorld.Load(URL: string);
begin
  if not URLValid(URL) then fLog(LogWorldInit,{$I %CURRENTROUTINE%},'World is not nil, freeing');
  {$hint dummy}
  //load seed and Nav
end;

{------------------------------------------------------------------------------}

procedure FreeWorld;
begin
  if CurrentWorld<>nil then fLog(LogWorldInit,{$I %CURRENTROUTINE%},'World is not nil, freeing');
  FreeAndNil(CurrentWorld);
end;

end.

