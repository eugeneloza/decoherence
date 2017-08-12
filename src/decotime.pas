{Copyright (C) 2012-2017 Michalis Kamburelis, Yevhen Loza

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

{ Copy of Castle Game Engine Time utilities. }
unit DecoTime;

{$INCLUDE compilerconfig.inc}

interface

uses
  {$ifdef MSWINDOWS} Windows{,} {$endif}
  {$ifdef UNIX} BaseUnix, Unix{, Dl, }{$endif},
  CastleTimeUtils{SysUtils, Math};

Type DTime = TFloatTime;
     {see note for CastleTimeUtils.TTimerResult}
     DIntTime = int64;

var
    { analogue to Now function, but a fast-access variable, representing
      current global time (accessed once per frame) }
    DecoNow: DTime;
    { analogue to Now function, but a fast-access variable, representing
      current in-game time }
    DecoNowLocal: DTime;

{ Gets CastleTimeUtils.Timer value from some "starting point"
  Starting point is thread-safe (Read only). }
function GetNow: DTime; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

{ This is a less accurate but accelerated (~130 times) version
  of the timer by using threads. Should be used after ForceGetNowThread.
  Should be used only in time-critical cases, such as World.Manage }
function GetNowThread: DTime; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{ Forces initialization of the threaded timer value. }
function ForceGetNowThread: DTime; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses SysUtils, Classes;

{$ifdef MSWINDOWS}
{************************* WINDOWS TIME **************************************}
type
  TTimerFrequency = Int64;
  TTimerState = (tsNotInitialized, tsQueryPerformance, tsGetTickCount64);

var
  FTimerState: TTimerState = tsNotInitialized;
  FTimerFrequency: TTimerFrequency;

{ Set FTimerState to something <> tsNotInitialized.
  Also set FTimerFrequency. }
procedure InitTimer;
begin
  if QueryPerformanceFrequency(FTimerFrequency) then
    FTimerState := tsQueryPerformance else
  begin
    FTimerState := tsGetTickCount64;
    FTimerFrequency := 1000;
  end;
end;

function TimerFrequency: TTimerFrequency;
begin
  if FTimerState = tsNotInitialized then InitTimer;

  Result := FTimerFrequency;
end;

function Timer: TTimerResult;
begin
  if FTimerState = tsNotInitialized then InitTimer;

  if FTimerState = tsQueryPerformance then
    QueryPerformanceCounter(Value)
  else
  begin
    { Deliberately using deprecated GetTickCount64 and friends.
      It should be internal in this unit. }
    {$warnings off}
    { Unfortunately, below will cast GetTickCount64 back to 32-bit.
      Hopefully QueryPerformanceCounter is usually available. }
    Value := GetTickCount64;
    {$warnings on}
  end;
end;
{$endif MSWINDOWS}

{$ifdef UNIX}
{************************* UNIX TIME **************************************}

type
  TTimerFrequency = LongWord;
const
  TimerFrequency: TTimerFrequency = 1000000;

function Timer: DIntTime; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
var
  tv: TTimeval;
begin
  FpGettimeofday(@tv, nil);
  Result := Int64(tv.tv_sec) * 1000000 + Int64(tv.tv_usec);
end;
{$endif UNIX}

{============================= GET TIME DIRECTLY =============================}

function GetNow: DTime; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  Result := Timer / TimerFrequency;
end;

{========================== GET TIME IN A THREAD =============================}

type TTimerThread = class(TThread)
  protected
    procedure Execute; override;
  public
    Time: DIntTime;
  end;

var ThreadedTimer: TTimerThread;

procedure TTimerThread.Execute;
begin
  Time := Timer;
end;

{----------------------------------------------------------------------------}

var LastTime: DTime;
function GetNowThread: DTime; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  if ThreadedTimer.Finished then begin
    LastTime := ThreadedTimer.time / TimerFrequency;
    Result := LastTime;
    ThreadedTimer.Start;
  end else
    Result := LastTime;
end;

{----------------------------------------------------------------------------}

function ForceGetNowThread: DTime; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  LastTime := GetNow;
  Result := LastTime;
  if ThreadedTimer.Finished then ThreadedTimer.Start;
end;

initialization
  ThreadedTimer := TTimerThread.create(false);
  ThreadedTimer.Priority := tpLower;
  ThreadedTimer.FreeOnTerminate := false;

finalization
  FreeAndNil(ThreadedTimer);

end.
