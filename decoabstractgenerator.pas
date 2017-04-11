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

uses Classes, CastleRandom;

type
  {maybe I'll change it later}
  TTileType = word;
  TIntCoordinate = integer;

Type
  {Most abstract generation routine to parent all the generation algorithms
   Required for abstract calls to different implementation of generators}
  DAbstractGenerator = class(TThread)
  protected
    fisWorking: boolean;
    fisReady: boolean;
    fisInitialized: boolean;
  protected
    { xorshift random generator, fast and thread-safe }
    RNDM: TCastleRandom;
  public
    {is the generator ready to wrok?
     Generatie will raise an exception if it isn't}
    property isReady: boolean read fisReady default false;
    {are the parameters initialized? If no, they'll be init
     automatically, but its best to do it manually outside the thread}
    property isInitialized: boolean read fisInitialized default false;
    {is the generator currently working?}
    property isWorking: boolean read fisWorking default false;
    {this forces isReady to true. Must be used only in constructor which skips
     loading of the map}
    procedure ForceReady;

    {MUST BE MANUALLY RUN BEFORE GENERATION (best if outside the thread)
     initialize parameters and load pre-generated tiles
     Will attempt automatic initialization if possible
     Use ForceReady to define parameters manually}
    procedure InitParameters; virtual; abstract;
  public

    //Property Seed: LongWord;
    {Abstract call to generation procedure
     It has different implementation in different generators}
    procedure Generate; virtual; abstract;
    {Abstract call to 3d world generation
     Will remain abstract in simple generators
     But will get unique implementation in 3d world generators}
    procedure Generate3D; virtual; abstract;

    constructor create; virtual;//override;
    destructor destroy; override;
  End;


{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses SysUtils, CastleLog;

{-----------------------------------------------------------------------------}

procedure DAbstractGenerator.ForceReady;
begin
  fisReady := true;
  WriteLnLog('DAbstractGenerator.ForceReady','Be careful, parameters might not be initialized correctly.');
end;

constructor DAbstractGenerator.create;
begin
  inherited;
  {we create an non-initialized random (i.e. initialized by a stupid constant integer)
  Just to make sure we don't waste any time (<1ms) on initialization now}
  RNDM := TCastleRandom.Create(1);
end;

destructor DAbstractGenerator.destroy;
begin
  FreeAndNil(RNDM);
  inherited;
end;

end.

