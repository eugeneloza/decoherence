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

{ Editor for all game data }
program Constructor_Tool;

{$IFDEF Windows}{$APPTYPE Console}{$ENDIF}//it's always console
{$INCLUDE src/compilerconfig.inc}

uses
 {$IFDEF UNIX}cThreads, {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  Castle_Base,
  Castle_Window,
  Castle_Components,
  Constructor_MainUnit,
  Constructor_Facts,
  Constructor_Global,
  DecoTranslation,
  Constructor_Tiles,
  Constructor_Map,
  Constructor_Placeholders;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFactsEditor, FactsEditor);
  Application.CreateForm(TDungeonTilesEditor, DungeonTilesEditor);
  Application.CreateForm(TMapEditor, MapEditor);
  Application.CreateForm(TPlaceholdersEditor, PlaceholdersEditor);
  Application.Run;
end.
