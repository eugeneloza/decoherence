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

(* Main Form of the Constructor
   It spawns and manages all other Constructor Forms *)

unit ConstructorMain;

{$INCLUDE compilerconfig.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs;

type
  TMainForm = class(TForm)
  private

  public

  end;

var
  MainForm: TMainForm;

{............................................................................}
implementation

{$R *.lfm}

end.

