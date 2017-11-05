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

{ Defines some generic variables,
  also handles random initialzation and random for GUI and other minor purposes }

unit DecoGlobal;

{$INCLUDE compilerconfig.inc}

interface

uses {todo: temporary}//SysUtils,
  Classes, CastleWindow,
  CastleRandom;

const NEnemies = 0.05; {must be <1 / temporary quantity of enemies in % of NavPoints}

{ for easy changing into double in case needed }
type Float = single;
     pFloat = ^Float;

type
  {}
  DObject = class(TObject)
    {
      we aren't using DecoLog directly, but relying on it to initialize the log }
    procedure Log(const LogLevel: boolean; const aProcedure, aMessage: string);
  end;

{ folders constants relative to ApplicationData path
  should be "/", not pathdelim, because those are URLs }
const InterfaceFolder     = 'interface/';
      FramesFolder        = InterfaceFolder+'frames/';
      LoadScreenFolder    = InterfaceFolder+'loadscreen/';
      ProgressBarFolder   = InterfaceFolder+'progressbar/';
      BackgroundsFolder   = InterfaceFolder+'background/';
      WindFolder          = InterfaceFolder+'wind/';
      DecorationsFolder   = InterfaceFolder+'decorations/';
      PortraitFolder      = InterfaceFolder+'portrait/';
      PerksFolder         = InterfaceFolder+'perks/';
      DamageFolder        = InterfaceFolder+'damage/';
      FontFolder          = InterfaceFolder+'fonts/';

      ScenarioBaseFolder  = 'scenario/';
      {because there can be several scenario subfolders, these are only for "addition"}
      TextFolder          = 'text/';
      MapsFolder          = 'maps/';

      ModelsFolder        = 'models/';
      TilesFolder         = ModelsFolder+'tiles/';
      PlaceholdersFolder  = ModelsFolder+'placeholders/';
      CreaturesFolder     = ModelsFolder+'creatures/';

      //TODO: Android incompatible!!!
      //Models_folder       = +'models'+pathdelim;
      //Tiles_folder        = Models_folder + ;
      //Placeholders_folder = Models_folder + 'placeholders'+pathdelim;


const GZ_ext = {$IFDEF gzipdata}'.gz'{$ELSE}''{$ENDIF};

const AnisotropicSmoothing = 4;
      ShadowMapsEnabled = false;
      ShadowVolumesEnabled = false;

var {global window of the game}
    Window : TCastleWindow;
    {todo: current scenario}
    CurrentScenarioFolder: string = 'test/';
    { random generator used for all interface random events }
    drnd: TCastleRandom;

    {$IFNDEF Android}
    LogStream : TFileStream;
    {$ENDIF}

{$IFDEF LINUX}
{$DEFINE USE_DEV_URANDOM}
{$ENDIF}
{a little modification of CastleRandom RandomSeed initialization algorithm
 to use /dev/urandom on Linux. Actually /dev/urandom exists on all *NIX OS,
 but I'm not exactly sure if it'll work as expected (test needed)
 We're not pursing cryptographic purposes, so /dev/urandom is perfectly enough}
function GetRandomSeed: LongWord;

function GetScenarioFolder: string;

{inlined equivalent of FloatEquals / Math.equals}
function dEqual(const a,b: float): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{ check if URL is valid }
function URLValid(const aURL: string): boolean;
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses SysUtils, DecoLog, CastleLog;

procedure DObject.Log(const LogLevel: boolean; const aProcedure, aMessage: string);
begin
  if not doLog then Exit;
  if LogLevel then WriteLnLog(Self.ClassName+'.'+aProcedure,aMessage)
end;
{---------------------------------------------------------------------------}

function GetRandomSeed: LongWord;
{$IFDEF USE_DEV_URANDOM}
var dev_rnd: file of integer;
begin
  { algorithm according to http://wiki.freepascal.org/Dev_random
   /dev/urandom is a native *nix very high-quality random number generator.
   it's 1000 times slower than CastleRandom,
   but provides a perfect seed initialization. }
  AssignFile(dev_rnd, '/dev/urandom');
  Reset(dev_rnd);
  repeat
    Read(dev_rnd,Result);
  until Result <> 0; // xorshift can't accept 0 as a random seed so we just read /dev/urandom until its not zero
  CloseFile(dev_rnd);
end;
{$ELSE}
begin
  {otherwise just let an internal algorithm's random initialization do the job}
  GetRandomSeed := 0;
end;
{$ENDIF}

{---------------------------------------------------------------------------}

procedure InitGlobal;
begin
  drnd := TCastleRandom.Create(GetRandomSeed);
end;

{----------------------------------------------------------------------------}

procedure DestroyGlobal;
begin
  FreeAndNil(drnd);
end;

{----------------------------------------------------------------------------}

function GetScenarioFolder: string;
begin
  Result := ScenarioBaseFolder + CurrentScenarioFolder;
end;

{----------------------------------------------------------------------------}

function dEqual(const a,b: float): boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
Const Epsilon = 1E-4;
{  EZeroResolution = 1E-16;
  DZeroResolution = 1E-12;
  SZeroResolution = 1E-4;}
begin
  if (a>b) then
    Result:=((A-B)<=Epsilon)
  else
    Result:=((B-A)<=Epsilon);
end;

{----------------------------------------------------------------------------}

function URLValid(const aURL: string): boolean;
begin
  //todo
  if aURL<>'' then Result := true else Result := false;
end;

initialization
InitGlobal;

finalization
DestroyGlobal;

end.


