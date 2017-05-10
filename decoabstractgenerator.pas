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
unit decoabstractgenerator;

{$INCLUDE compilerconfig.inc}
interface

uses {Classes,} CastleRandom,
  decothread;

type
  {maybe I'll change it later}
  TTileType = word;
  TIntCoordinate = integer;

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

    constructor create; virtual;//override;
    destructor destroy; override;
  protected
    { here we simply launch "Generate" in a Thread }
    procedure Execute; override;
  End;


{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, CastleLog;

{-----------------------------------------------------------------------------}

constructor DAbstractGenerator.create;
begin
  inherited;
  {we create an non-initialized random (i.e. initialized by a stupid constant integer)
  Just to make sure we don't waste any time (<1ms) on initialization now}
  RNDM := TCastleRandom.Create(1);
end;

{-----------------------------------------------------------------------------}

destructor DAbstractGenerator.destroy;
begin
  FreeAndNil(RNDM);
  inherited;
end;

{-----------------------------------------------------------------------------}

procedure DAbstractGenerator.InitSeed(newseed: longword = 0);
begin
  RNDM.initialize(newseed);
end;

{-----------------------------------------------------------------------------}

procedure DAbstractGenerator.execute;
begin
  Generate;
end;



end.

