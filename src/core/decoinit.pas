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

(* Initializes and frees the application *)

unit DecoInit;

{$INCLUDE compilerconfig.inc}

interface

{ Initializes all generic stuff }
procedure InitDecoherence;
{ Frees everything initialized before }
procedure FreeDecoherence;
{............................................................................}
implementation

uses
  SysUtils, CastleWindow, CastleControls, CastleGLImages,
  CastleApplicationProperties,
  DecoLoadEmbedded,
  DecoTranslation,
  DecoInput, DecoPlayer, DecoGUI,
  DecoMain,
  DecoTime, DecoLog, DecoWindow, DecoGlobal;

{ Displays a "Loading..." image for the language
  thanks to Michalis, it's simple :) see https://github.com/eugeneloza/decoherence/issues/22 }
procedure SetLoadingImage;
begin
  case CurrentLanguage of
    Language_English: Theme.Images[tiLoading] := Loading_eng;
    Language_Russian: Theme.Images[tiLoading] := Loading_rus;
    else begin
      Theme.Images[tiLoading] := Loading_eng;
      Log(LogLanguageError, CurrentRoutine, 'WARNING: Unknown Language in DecoInit! Falling back to English.');
    end;
  end;

  Theme.OwnsImages[tiLoading] := False;
end;

{-----------------------------------------------------------------------------}

function GetApplicationName: string;
begin
  Result := 'Decoherence 1';
end;

{-----------------------------------------------------------------------------}

procedure ApplicationInitialize;
begin
  { Be careful with init sequence and do not change the init order
    unless you know what you are doing
    as some units require others being already initialized }
  Log(LogInit, CurrentRoutine, 'Init sequence started.');
  InitTime;
  InitGUI;
  InitPlayer;
  InitInput;
  InitGlobal;
  InitManagement;
  Log(LogInit, CurrentRoutine, TextureMemoryProfiler.Summary);
  Log(LogInit, CurrentRoutine, 'Init sequence finished.');
end;

{-----------------------------------------------------------------------------}

procedure InitDecoherence;
begin
  ApplicationProperties(true).ApplicationName := GetApplicationName;
  InitLog;
  Log(LogInit, CurrentRoutine, 'Initializing Application and Window.');
  OnGetApplicationName := @GetApplicationName;
  TextureMemoryProfiler.Enabled := true;
  InitTranslation;
  SetLoadingImage;
  InitWindow;
  Application.MainWindow := Window;
  Application.OnInitialize := @ApplicationInitialize;
  Window.Caption := GetApplicationName;
end;

{-----------------------------------------------------------------------------}

procedure FreeDecoherence;
begin
  { Be careful with free sequence and do not change the init order
    unless you know what you are doing
    as some units might accidentally (thou unlikely) send a call to already-freed instance }
  Log(LogInit, CurrentRoutine, 'Game over. Freeing all data.');
  FreeManagement;
  FreeGlobal;
  FreeInput;
  FreePlayer;
  FreeGUI;
  FreeTime;
  Log(LogInit, CurrentRoutine, 'Finished.');
  FreeLog;
end;

end.

