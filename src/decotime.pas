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

{ Crop of Castle Game Engine Time utilities plus a threaded timer.
  Provides for accurate and fast time access, hopefully thread-safe :)
  GetNow should be used to access immediate time value.
  Everywhere possible, DecoNow and DecoNowLocal should be used (updated once per frame).
  GetNowThread and ForceGetNowThread are very specific and should be used
  only in extremely time-critical routines often calculating time like World.Manage}
unit DecoTime;

{$INCLUDE compilerconfig.inc}

interface

uses
  {$ifdef MSWINDOWS} Windows, {$endif}
  {$ifdef UNIX} Unix, {$endif}
  CastleTimeUtils,
  DecoGlobal;

Type DTime = TFloatTime;
     {see note for CastleTimeUtils.TTimerResult}
     DIntTime = int64;

var { analogue to Now function, but a fast-access variable, representing
      current global time (time where animations take place)
      works ~200 times faster than SysUtils.Now so should be used anywhere possible
      Updated once per frame}
    DecoNow: DTime;
    deltaT: DTime;
    { analogue to Now function, but a fast-access variable, representing
      current in-game time (time where actions take place)
      Updated once per frame }
    DecoNowLocal: DTime;
    DeltaTLocal: DTime;

    SoftPause: float = 0.0;
    SoftPauseCoefficient: float = 1.0;

{ Gets CastleTimeUtils.Timer value from some "starting point" in a thread-safe way }
function GetNow: DTime; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

{$HINT maybe use raw integer for these values? That'll give approx +0.5% speed, but will require converting FPS_goal to integer}
{ This is a less accurate but accelerated (~130 times) version
  of the timer by using threads. Should be used after ForceGetNowThread.
  Should be used only in time-critical cases, such as World.Manage }
function GetNowThread: DTime; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{ Forces initialization of the threaded timer value. }
function ForceGetNowThread: DTime; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses SysUtils, Classes{$IFDEF Windows}, SyncObjs{$ENDIF};

{$IFDEF Windows}
{************************* WINDOWS TIME **************************************}
type TTimerFrequency = Int64;
     TTimerState = (tsQueryPerformance, tsGetTickCount64);

var
  FTimerState: TTimerState;
  TimerFrequency: TTimerFrequency;
  TimerLock: TCriticalSection;  //we'll need a critical section as we access FTimerState.

function Timer: DIntTime; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
var QueryPerformance: boolean;
begin
  TimerLock.Acquire;   //maybe, this is redundant, but let it be here for now...
  QueryPerformance := FTimerState = tsQueryPerformance;
  TimerLock.Release;

  if QueryPerformance then
    QueryPerformanceCounter({$warnings off}Result{$warnings on})
  else
    {in case of ancient Windows version fall back to GetTickCount :)}
    Result := {$warnings off} GetTickCount64 {$warnings on};
end;
{$ENDIF}

{$IFDEF Unix}
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
{$ENDIF}

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

{ This procedure forces correct initialization of ThreadedTimer
  and must always be used once before starting accessing the GetNowThread sequentially
  so that the first value will be correct (otherwise it might be extermely wrong,
  which is bad for World.Manage) }
function ForceGetNowThread: DTime; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  LastTime := GetNow;
  {//DecoNow; --- actually we should be perfectly fine with this value?
   It will give "a bit incorrect" (thou it can't go badly wrong) value for first
   several cycles of World.Manage, but anyway it should preform
   several additional routines.
   On the other hand, we access ForceGetNowThread only once per frame,
   so, maybe, better not to bother}
  Result := LastTime;
  if ThreadedTimer.Finished then ThreadedTimer.Start;
end;

initialization
  //create threaded timer and run it immediately to make sure everything is initialized properly
  ThreadedTimer := TTimerThread.create(false);
  ThreadedTimer.Priority := tpLower;
  ThreadedTimer.FreeOnTerminate := false;

  {$IFDEF Windows}
  //initialize the timer in Windows and determine TimerFrequency
  if QueryPerformanceFrequency(TimerFrequency) then
    FTimerState := tsQueryPerformance else
  begin
    FTimerState := tsGetTickCount64;
    TimerFrequency := 1000;
  end;
  TimerLock := TCriticalSection.Create;
  {$ENDIF}

finalization
  FreeAndNil(ThreadedTimer);
  {$IFDEF Windows}
  FreeAndNil(TimerLock);
  {$ENDIF}

end.
