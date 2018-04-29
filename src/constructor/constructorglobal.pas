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

{---------------------------------------------------------------------------}

(* Some global constructor definitions *)

{$INCLUDE compilerconfig.inc}

unit ConstructorGlobal;

interface

uses
  Classes, SysUtils, Forms,
  DecoGlobal, DecoFolders;

type
  { Form, capable of tracking it's "changed" state and calling
    Write/Read standard routines
    Will also handle language stuff.
    WARNING: There is a decent Lazarus *bug*
    We can't use second child of TForm, therefore we can't make a
    LanguageForm separate from ConstructorFomr (at least for now)
    see https://github.com/eugeneloza/decoherence/issues/56 }

  { it doesn't really like class abstract formulation here too! :D }
  TConstructorForm = class(TForm)
  end;

type
  { abstract data module
    children will contain the data and implement read/write functions }
  TDataModule = class abstract(TObject)
  strict private
    fisChanged: boolean;
    fisDirectWrite: boolean;
  strict protected
    WritingToGameFolder: boolean;
    URL: string;
  public
    { Name of the current file (including sub-folder) }
    FileName: string;
    { if this data module has been changed? }
    property isChanged: boolean read fisChanged write fisChanged default false;
    { does this data module works directly on game data
      or through intermediate Constructor folder? }
    property isDirectWrite: boolean read fisDirectWrite write fisDirectWrite default false;

    { Write this data module (core routine - do the actual writing) }
    procedure WriteMe; virtual; abstract;
    { Write this data module to game folder }
    procedure WriteToGameFolder;
    { Write this data module to constructor folder }
    procedure WriteToConstructorFolder;

    { Read this data module }
    procedure ReadMe; virtual; abstract;
    { If this data module is valid? }
    function isValid: boolean; virtual;
    { Call-back event in case this data module has been changed }
    procedure SetChanged(Sender: TObject); //TNotifyEvent
  end;

{............................................................................}
implementation
uses
  CastleFilesUtils,
  DecoLog;

function ConstructorFolder(const FileURL: string): string;
begin
  Result := ApplicationData(FileURL);
end;

{-----------------------------------------------------------------------------}

procedure TDataModule.SetChanged(Sender: TObject);
begin
  fisChanged := true;
end;

{-----------------------------------------------------------------------------}

function TDataModule.isValid: boolean;
begin
  Result := true;
end;

{-----------------------------------------------------------------------------}

procedure TDataModule.WriteToGameFolder;
begin
  WritingToGameFolder := true;
  URL := GameFolder(Self.FileName);
  WriteMe;
end;

{-----------------------------------------------------------------------------}

procedure TDataModule.WriteToConstructorFolder;
begin
  if isDirectWrite then
    WriteToGameFolder
  else
  begin
    WritingToGameFolder := false;
    URL := ConstructorFolder(Self.FileName);
    WriteMe;
  end;
end;

end.

