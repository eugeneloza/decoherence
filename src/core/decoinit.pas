{ Copyright (C) 2012-2017 Yevhen Loza

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

{ Initializes and frees the application }
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
  SysUtils, CastleWindow, DecoWindow, DecoGlobal;

function MyGetApplicationName: string;
begin
  Result := 'Decoherence 1';
end;


procedure ApplicationInitialize;
begin
  InitGlobal;
end;

procedure InitDecoherence;
begin
  OnGetApplicationName := @MyGetApplicationName;
  InitWindow;
  Application.MainWindow := Window;
  Application.OnInitialize := @ApplicationInitialize;
  Window.Caption := MyGetApplicationName;
end;

procedure FreeDecoherence;
begin
  FreeGlobal;
end;

end.

