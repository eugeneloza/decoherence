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

(* Contains most of interface basics and grouping *)

{$INCLUDE compilerconfig.inc}

unit DecoInterfaceTimer;

interface

uses
  DecoTime, DecoGlobal;

type
  { A simple time-out mechanisms to preform some timed events on interface
    elements }
  DTimer = class(DObject)
  private
    { Set automatically, date of the timer count start }
    StartTime: DTime;
  public
    { If the timer is running }
    Enabled: boolean;
    { How long (in seconds) will it take the timer to fire }
    Interval: DTime;
    { Action to preform }
    onTimer: TSimpleProcedure;
    { A simple way to set and run timer }
    procedure SetTimeOut(const Seconds: DTime);
    { Check if the timer finished and run onTimer if true }
    procedure Update;
  public
    constructor Create;
  end;

{............................................................................}
implementation

constructor DTimer.Create;
begin
  inherited Create;
  Enabled := false;
  StartTime := -1;
end;

{-----------------------------------------------------------------------------}

procedure DTimer.Update;
begin
  if StartTime < 0 then
    StartTime := DecoNow
  else
    if (DecoNow - StartTime) >= Interval then
    begin
      Enabled := false;
      if Assigned(onTimer) then
        onTimer;
    end;
end;

{-----------------------------------------------------------------------------}

procedure DTimer.SetTimeout(const Seconds: DTime);
begin
  StartTime := -1;
  Enabled := true;
  Interval := Seconds;
end;

end.

