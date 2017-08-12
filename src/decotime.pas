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


type
  { Current time from @link(ProcessTimer).
    If possible, this measures only the CPU usage local to this process. }
  TProcessTimerResult = object
  private
    Value:
      {$ifdef UNIX} clock_t {$endif}
      {$ifdef MSWINDOWS} DWord {$endif};
  end;

type
  { Current time from @link(Timer). }
  TTimerResult = object
  private
    { The type of this could be platform-dependent. But for now, all platforms
      are happy with Int64. }
    Value: Int64;
  end;

var
    { analogue to Now function, but a fast-access variable, representing
      current global time (accessed once per frame) }
    DecoNow: DTime;
    { analogue to Now function, but a fast-access variable, representing
      current in-game time }
    DecoNowLocal: DTime;


{ Current time, to measure real time passed.
  This may be a time local to this process. It is a "real" time,
  which means that subtracting two values measures the actual time
  that passed between two events. Contrast this with @link(ProcessTimer)
  and friends that try to measure only CPU time used by the current process.

  Call Timer twice, and calculate the difference (in seconds)
  using the TimerSeconds. }
function Timer: TTimerResult;

{ Gets CastleTimeUtils.Timer value from some "starting point"
  Starting point is thread-safe (Read only). }
function GetNow: DTime; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{ This is a less accurate but accelerated (~130 times) version
  of the timer, using threads }
function GetNowThread: DTime; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}


{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
//uses CastleLog;

{ timer ---------------------------------------------------------- }

{$ifdef MSWINDOWS}
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
    QueryPerformanceCounter(Result.Value)
  else
  begin
    { Deliberately using deprecated GetTickCount64 and friends.
      It should be internal in this unit. }
    {$warnings off}
    { Unfortunately, below will cast GetTickCount64 back to 32-bit.
      Hopefully QueryPerformanceCounter is usually available. }
    Result.Value := GetTickCount64;
    {$warnings on}
  end;
end;
{$endif MSWINDOWS}

{$ifdef UNIX}
type
  TTimerFrequency = LongWord;
const
  TimerFrequency: TTimerFrequency = 1000000;
var
  LastTimer: TTimerResult;

function Timer: TTimerResult;
var
  tv: TTimeval;
begin
  FpGettimeofday(@tv, nil);

  { We can fit whole TTimeval inside Int64, no problem. }
  Result.Value := Int64(tv.tv_sec) * 1000000 + Int64(tv.tv_usec);
end;
{$endif UNIX}

function TimerSeconds(const A, B: TTimerResult): TFloatTime;
begin
  Result := (A.Value - B.Value) / TimerFrequency;
end;
function TimerSecondsInt(const A, B: TTimerResult): integer; inline;
begin
  Result := A.Value - B.Value;
end;

{ maybe initTime shift should be saved with the game and correspond to
  global playtime}
var InitTime: TTimerResult;

function GetNow: DTime; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  Result := TimerSeconds(Timer, InitTime);
end;
function GetNowThread: DTime; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  Result := TimerSeconds(Timer, InitTime);
end;

initialization
InitTime := Timer;



end.
