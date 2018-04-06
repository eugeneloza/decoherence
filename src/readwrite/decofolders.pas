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

(* Defines some generic types and variables,
   also handles random initialzation and other minor but global tasks *)

unit DecoFolders;

{$INCLUDE compilerconfig.inc}

interface

uses
  Classes, SysUtils;

{ Wrapper for CastleFilesUtils.ApplicationData
  to be able to switch between Game and Architect folder }
function GameFolder(const FileURL: string): string;
function GameConfigFolder(const FileURL: string): string;
function SavedGamesFolder(const FileURL: string): string;
{............................................................................}
implementation
uses
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
  SavedGamesDirURL: string = '';

function SavedGamesFolder(const FileURL: string): string;
begin
  {$IFDEF Desktop}
  if SavedGamesDirURL = '' then
  begin
    SavedGamesDirURL := InclPathDelim(GetCurrentDir) + 'SavedGames' + PathDelim ;
    if not ForceDirectories(SavedGamesDirURL) then
      raise Exception.Create('ERROR: Unable to create Save Game folder!');
    SavedGamesDirURL := FilenameToURISafe(SavedGamesDirURL);
    Log(LogInit, CurrentRoutine, 'Saved Games folder: ' + SavedGamesDirURL);
  end;
  Result := SavedGamesDirURL + FileURL;
  {$ELSE}
  Result := ApplicationConfig(FileURL);
  {$ENDIF}
end;

end.

