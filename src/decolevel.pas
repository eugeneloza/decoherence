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

{ Temporary unit "loads" a game level and sets up the camera }
unit DecoLevel;

{$INCLUDE compilerconfig.inc}

interface

uses CastleWindow, {CastleWindowTouch,} CastleScene, X3DNodes,
  DecoLoad3d,
  SysUtils,
  DecoAbstractWorld, decodungeonworld, DecoDungeonGenerator,
  DecoNavigation, decoglobal;

procedure LoadTestLevel;
procedure InitTestLevel;

var
  LoadCompleted: boolean = False;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses CastleFilesUtils,
  DecoPlayerCharacter,
  DecoActorBody,
  DecoGameMode, DecoLog, Profiler;

procedure Generate3DWorld;
var
  GENERATOR: D3dDungeonGenerator;
begin
  {StartProfiler}

  GENERATOR := D3dDungeonGenerator.Create;
  GENERATOR.parameters.load(ApplicationData(GetScenarioFolder + MapsFolder +
    '1' + '.xml' + GZ_ext));
  GENERATOR.InitParameters;
  GENERATOR.Generate;
  CurrentWorld := DDungeonWorld.Create;
  CurrentWorld.Load(Generator);
  FreeAndNil(GENERATOR);
  CurrentWorld.Build;

  tmpLoadKnightCreature;

  {StopProfiler}
end;

{---------------------------------------------------------------------------}

var
  LoadedLevel: boolean = False;

procedure LoadTestLevel;
begin
  {StartProfiler}

  Generate3DWorld;

  Log(LogInitData, _CurrentRoutine, 'Scene');

  Window.ShadowVolumes := ShadowVolumesEnabled;
  Window.ShadowVolumesRender := ShadowVolumesEnabled;
  Window.AntiAliasing := aa8SamplesNicer;

  //make a temporary party {wrong place}
  InitPlayer;
  InitNavigation;

  Log(LogInitData, _CurrentRoutine, 'Finished');

  {StopProfiler}
end;

{---------------------------------------------------------------------------}

procedure InitTestLevel;
begin
  {StartProfiler}

  if not LoadCompleted then
    Exit;
  if not LoadedLevel then
  begin
    Log(LogInitData, _CurrentRoutine, 'Init');
    LoadedLevel := True;
    if CurrentWorld = nil then
      Exit;
    CurrentWorld.Activate;
    //Window.TouchInterface := {$IFDEF Android}tiCtlWalkDragRotate{$ELSE}tiNone{$ENDIF};
    SetGameMode(gmTravel);
  end;

  {StopProfiler}
end;


end.
