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

{ initializes all generic stuff }
procedure InitDecoherence;
{ frees everything initialized before }
procedure FreeDecoherence;
{............................................................................}
implementation

uses
  SysUtils, CastleWindow, CastleControls,
  DecoLoadEmbedded,
  DecoTranslation,
  DecoInput, DecoPlayer,
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

function MyGetApplicationName: string;
begin
  Result := 'Decoherence 1';
end;

{-----------------------------------------------------------------------------}

procedure ApplicationInitialize;
begin
  Log(LogInit, CurrentRoutine, 'Init sequence started.');
  InitPlayer;
  InitInput;
  InitGlobal;
  InitTime;
  InitManagement;
  Log(LogInit, CurrentRoutine, 'Init sequence finished.');
end;

{-----------------------------------------------------------------------------}

procedure InitDecoherence;
begin
  InitLog;
  Log(LogInit, CurrentRoutine, 'Initializing Application and Window.');
  OnGetApplicationName := @MyGetApplicationName;
  InitTranslation;
  SetLoadingImage;
  InitWindow;
  Application.MainWindow := Window;
  Application.OnInitialize := @ApplicationInitialize;
  Window.Caption := MyGetApplicationName;
end;

{-----------------------------------------------------------------------------}

procedure FreeDecoherence;
begin
  Log(LogInit, CurrentRoutine, 'Game over. Freeing all data.');
  FreeManagement;
  FreeTime;
  FreeGlobal;
  FreeInput;
  FreePlayer;
  Log(LogInit, CurrentRoutine, 'Finished.');
  FreeLog;
end;

end.

