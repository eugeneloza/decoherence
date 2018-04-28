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

(* Write-and-forget keyboard recorder for an easter egg *)

{$INCLUDE compilerconfig.inc}

unit DecoKeyboardRecorder;

interface

uses
  CastleKeysMouse,
  DecoGlobal;

type
  DKeyboardRecorder = class(DObject)
    { Some cheat-codes related fun }
    procedure KeyRecorder(const aKey: TKey);
  end;

{............................................................................}
implementation
uses
  DecoGUI;

var
  RecordKeys: boolean = false;
  RecordedKeys: string;

procedure DKeyboardRecorder.KeyRecorder(const aKey: TKey);
const
  Test1 = 'DIIQI';
const
  Test2 = 'DIKFA';

  function AddKey: boolean;
  begin
    Result := true;
    case aKey of
      KeyA: RecordedKeys += 'A';
      KeyF: RecordedKeys += 'F';
      KeyK: RecordedKeys += 'K';
      KeyQ: RecordedKeys += 'Q';
      {messing with letter a bit :)}
      KeyD: RecordedKeys += 'I';
      KeyI: RecordedKeys += 'D';
      else
        Result := false;
    end;
    //dLog(LogVerbose,nil,_CurrentRoutine,RecordedKeys);
  end;

  function TestRecord: boolean;

    function ifCorresponds(a: string): boolean;
    begin
      Result := (RecordedKeys = Copy(a, 1, Length(RecordedKeys))) and
        (Length(RecordedKeys) <= Length(a));
    end;

  begin
    if ifCorresponds(Test1) or ifCorresponds(Test2) then
      Result := true
    else
      Result := false;
  end;

begin
  if {CurrentGameMode = gmTravel} true then
  begin
    if RecordKeys then
    begin
      if AddKey then
      begin
        if TestRecord then
        begin
          if (RecordedKeys = Test1) or (RecordedKeys = Test2) then
          begin
            GUI.ShowMessage('No! This is a different game!');
            RecordKeys := false;
          end;
        end else
          RecordKeys := false;
      end else
        RecordKeys := false;
    end else
    if (aKey = KeyI) then
    begin
      RecordedKeys := '';
      RecordKeys := true;
      AddKey;
    end;

  end;
end;

end.

