{Copyright (C) 2012-2017 Michalis Kamburelis

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
  {$ifdef UNIX} BaseUnix, Unix{, Dl, }{$endif}
  {SysUtils, Math};

type
  { Current time from @link(ProcessTimer).
    If possible, this measures only the CPU usage local to this process. }
  TProcessTimerResult = object
  private
    Value:
      {$ifdef UNIX} clock_t {$endif}
      {$ifdef MSWINDOWS} DWord {$endif};
  end;

const
  { Resolution of the timer used by @link(ProcessTimer). }
  ProcessTimersPerSec
    {$ifdef UNIX}
      = { What is the frequency of FpTimes ?
          sysconf (_SC_CLK_TCK) ?
          Or does sysconf exist only in Libc ? }
        { Values below were choosen experimentally for Linux and FreeBSD
          (and I know that on most UNIXes it should be 128, that's
          a traditional value) }
        {$ifdef LINUX} 100 {$else}
          {$ifdef DARWIN}
            { In /usr/include/ppc/_limits.h and
                 /usr/include/i386/_limits.h
              __DARWIN_CLK_TCK is defined to 100. }
            100 {$else}
              128 {$endif} {$endif}
    {$endif}
    {$ifdef MSWINDOWS} = 1000 { Using GetLastError } {$endif}

type
  { Current time from @link(Timer). }
  TTimerResult = object
  private
    { The type of this could be platform-dependent. But for now, all platforms
      are happy with Int64. }
    Value: Int64;
  end;

{ Current time, to measure real time passed.
  This may be a time local to this process. It is a "real" time,
  which means that subtracting two values measures the actual time
  that passed between two events. Contrast this with @link(ProcessTimer)
  and friends that try to measure only CPU time used by the current process.

  Call Timer twice, and calculate the difference (in seconds)
  using the TimerSeconds. }
function Timer: TTimerResult;

{ Subtract two times obtained from @link(Timer),
  A-B, return a difference in seconds. }
function TimerSeconds(const A, B: TTimerResult): TFloatTime;
function TimerSecondsInt(const A, B: TTimerResult): integer; inline;

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

  { We cannot trust some Android systems to return increasing values here
    (Android device "Moto X Play", "XT1562", OS version 5.1.1).
    Maybe they synchronize the time from the Internet, and do not take care
    to keep it monotonic (unlike https://lwn.net/Articles/23313/ says?) }

  if Result.Value < LastTimer.Value then
  begin
    WritelnLog('Time', 'Detected gettimeofday() going backwards on Unix, workarounding. This is known to happen on some Android devices');
    Result.Value := LastTimer.Value;
  end else
    LastTimer.Value := Result.Value;
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

end.
