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
  Classes, SysUtils, Forms;

type
  { Form, capable of tracking it's "changed" state and calling
    Write/Read standard routines
    Will also handle language stuff.
    WARNING: There is a decent Lazarus *bug*
    We can't use second child of TForm, therefore we can't make a
    LanguageForm separate from ConstructorFomr (at least for now)
    see https://github.com/eugeneloza/decoherence/issues/56 }
  TConstructorForm = class abstract(TForm)
  end;

type
  { Data modules }
  TDataModule = class abstract(TObject)
  strict private
    fisChanged: boolean;
  public
    property isChanged: boolean read fisChanged write fisChanged default false;
    procedure WriteMe; virtual; abstract;
    procedure ReadMe; virtual; abstract;
    function isValid: boolean; virtual;
    procedure SetChanged(Sender: TObject); //TNotifyEvent
  end;

{............................................................................}
implementation

procedure TDataModule.SetChanged(Sender: TObject);
begin
  fisChanged := true;
end;

function TDataModule.isValid: boolean;
begin
  Result := true;
end;

end.

