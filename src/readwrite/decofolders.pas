{ Copyright (C) 2012-2018 Yevhen Loza

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>. }

{ --------------------------------------------------------------------------- }

(* Constants and routines to define and manage game folders *)

{$INCLUDE compilerconfig.inc}

unit DecoFolders;

interface

{uses
  DecoGlobal;}

{ Wrapper for CastleFilesUtils.ApplicationData
  to be able to switch between Game and Architect folder
  Warning: it doesn't create any folders, the structure must exist }
function GameFolder(const FileURL: string): string;
{ Game configuration folder, created if inexistend }
function GameConfigFolder(const FileURL: string): string;
{ Saved games folder, created if inexistent, including subfolders
  SubFolder should go in WORLD_001 ... WORLD_999, SAVE_001...SAVE_999
  and should have no sub-folders to avoid multiple URL-dir-URL conversions
  SavedGames root folder should contain only index.xml of saved games }
function SavedGamesFolder(const SubFolder: string; const FileURL: string): string;
{............................................................................}
implementation
uses
  SysUtils,
  CastleURIUtils, CastleUtils, CastleFilesUtils,
  DecoLog;

function GameFolder(const FileURL: string): string;
begin
  Result := ApplicationData(FileURL);
end;

{-----------------------------------------------------------------------------}

var
  ConfigurationDirURL: string = '';


function GameConfigFolder(const FileURL: string): string;
begin
  {$IFDEF Desktop}
  if ConfigurationDirURL = '' then
  begin
    ConfigurationDirURL := InclPathDelim(GetCurrentDir) + 'Configuration' + PathDelim ;
    if not ForceDirectories(ConfigurationDirURL) then
      raise Exception.Create('ERROR: Unable to create Save Game folder!');
    ConfigurationDirURL := FilenameToURISafe(ConfigurationDirURL);
    Log(LogInit, CurrentRoutine, 'Configuration folder: ' + ConfigurationDirURL);
  end;
  Result := ConfigurationDirURL + FileURL;
  {$ELSE}
  Result := ApplicationConfig(FileURL);
  {$ENDIF}
end;

{-----------------------------------------------------------------------------}

var
  SavedGamesDir: string = '';
  SavedGamesDirURL: string = '';

function SavedGamesFolder(const SubFolder: string; const FileURL: string): string;
begin
  {$IFDEF Desktop}
  if SavedGamesDir = '' then
  begin
    SavedGamesDir := InclPathDelim(GetCurrentDir) + 'SavedGames' + PathDelim ;
    if not ForceDirectories(SavedGamesDir) then
      raise Exception.Create('ERROR: Unable to create ' + SavedGamesDir + ' folder!');
    SavedGamesDirURL := FilenameToURISafe(SavedGamesDirURL);
    Log(LogInit, CurrentRoutine, 'Saved Games folder: ' + SavedGamesDirURL);
  end;

  if not ForceDirectories(SavedGamesDir + SubFolder + PathDelim) then
    raise Exception.Create('ERROR: Unable to create ' + SavedGamesDir
      + PathDelim + SubFolder + ' folder!');

  Result := SavedGamesDirURL + SubFolder + '/' + FileURL;
  {$ELSE}
  Result := ApplicationConfig(FileURL);
  {$ENDIF}
end;

end.

