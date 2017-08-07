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

{ contains definitions for most abstract Generator entity }
unit DecoAbstractGenerator;

{$INCLUDE compilerconfig.inc}
interface

uses {Classes,} CastleRandom, fgl, CastleGenericLists,
  DecoThread;

type
  {maybe I'll change it later}
  TTileType = word;
  TIntCoordinate = integer;

type TIndexList = specialize TFPGList<TTileType>;
{$warning change it to a generic list and export}
type TIndexGroups = specialize TFPGObjectList<TIndexList>;

type
  {a two-value description of a "neighbour" tile}
  DNeighbour = record
    {index of current tile}
    Tile: TTileType;
    {visiblity of the tile ~spatial angle}
    Visible: integer;
  end;

type TNeighboursList = specialize TGenericStructList<DNeighbour>;


type
  DAbstractGeneratorParameters = class(TObject)
  private
    fisReady: boolean;
  public
    {random generation seed}
    Seed: LongWord;
    {are links in TilesList absolute URL or just a tile name}
    AbsoluteURL: boolean;
    {is the generator ready to wrok?
     Generatie will raise an exception if it isn't}
    property isReady: boolean read fisReady write fisReady default false;
  end;


type
  {Most abstract generation routine to parent all the generation algorithms
   Required for abstract calls to different implementation of generators}
  DAbstractGenerator = class(TAbstractThread)
  protected
    fisWorking: boolean;
    fisInitialized: boolean;
    fisFinished: boolean;
  protected
    { xorshift random generator, fast and thread-safe }
    RNDM: TCastleRandom;
    {Specific SEED of the random number for this algorithm }
    procedure InitSeed(newseed: longword = 0);
  public
    {are the parameters initialized? If no, they'll be init
     automatically, but its best to do it manually outside the thread
     due to possible HDD reading issues}
    property isInitialized: boolean read fisInitialized default false;
    {is the generator currently working?}
    property isWorking: boolean read fisWorking default false;
    {if the generation finished successfully?}
    property isFinished: boolean read fisFinished default false;

    {MUST BE MANUALLY RUN BEFORE GENERATION (best if outside the thread)
     initialize parameters and load pre-generated tiles
     Will attempt automatic initialization if possible
     Use ForceReady to define parameters manually}
    procedure InitParameters; virtual; abstract;
  public

    //Property Seed: LongWord;
    {Abstract call to generation procedure
     It has different implementation in different generators
     Will launch a 3D routines also in specific children classes}
    procedure Generate; virtual; abstract;

    constructor Create; virtual;//override;
    destructor Destroy; override;
  protected
    { here we simply launch "Generate" in a Thread }
    procedure Execute; override;
  End;


{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, CastleLog;

{-----------------------------------------------------------------------------}

constructor DAbstractGenerator.Create;
begin
  inherited;
  {we create an non-initialized random (i.e. initialized by a stupid constant integer)
  Just to make sure we don't waste any time (<1ms) on initialization now}
  RNDM := TCastleRandom.Create(1);
end;

{-----------------------------------------------------------------------------}

destructor DAbstractGenerator.Destroy;
begin
  FreeAndNil(RNDM);
  inherited;
end;

{-----------------------------------------------------------------------------}

procedure DAbstractGenerator.InitSeed(NewSeed: longword = 0);
begin
  RNDM.Initialize(NewSeed);
end;

{-----------------------------------------------------------------------------}

procedure DAbstractGenerator.Execute;
begin
  Generate;
end;



end.

