{Copyright (C) 2017 Yevhen Loza

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

(* Simple macro-based profiler unit

  usage:

  {$INCLUDE profiler.inc} and add Profiler unit to "USES" section
  of every unit of the project

  You may disable/enable profiler by editing profiler.inc and
  commenting/uncommenting {$DEFINE UseProfiler} line
  When disabled, the profiler doesn't affect anything at all.
  Defining SortProfilerResults would sort the routines in decreasing order
  based on how much total time they consumed (separately at every hierarchy level)

  Just place StartProfiler just after "begin" of the procedure
  and StopProfiler just before "end" of the procedure that needs profiling.
  (see example)

  You may also use direct calls to doStartProfiler...doStopProfiler with a
  custom string given to doStartProfiler to profile different parts of the code.

  Caution: StartProfiler must always have a matching StopProfiler otherwise
  the profiler may build a wild profiler tree :)

  Works safely for recoursive, inlined, nested(local) and inherited procedures.
  Can handle unexpected "Exit" from the procedure correctly.
  Enjoy!

  Warning: it's not thread-safe.

  Requires FPC 3.1.1 and above
  Requires Castle Game Engine (Generics.Collections, CastleLog and CastleTimeUtils)

  I highly recommend disabling code folding in
  Lazarus IDE > Options > Editor > Code Folding
  Lazarus IDE > Options > Completion and Hints > Add Close Statements for Pascal Blocks
  as macros significantly slow down the IDE in this case
*)

unit Profiler;

interface

{$INCLUDE compilerconfig.inc}

//{$DEFINE SortProfilerResults}

{$IFDEF UseProfiler}
{ Tries to find a profiler entry for aFunction or creates it otherwise
  assigns CurrentLevel to this function
  and starts counting time for the current function }
procedure doStartProfiler(const aFunction: string); TryInline
{ Stops counting time for the current function
  and records results }
procedure doStopProfiler(const aFunction: string); TryInline
{$ENDIF}

implementation

{$IFDEF UseProfiler}
uses SysUtils, CastleLog,
  Generics.Defaults, Generics.Collections,
  DecoTime;

type
  { A profiler record }
  TProfilerChild = class(TObject)
    { Profiled procedure name }
    EntryName: string;
    { Total procedure time }
    EntryTime: DTime;
    { Number of procedure calls }
    EntryHits: integer;
  end;

  { List of profiler records }
  TProfilerList = specialize TObjectDictionary<string, TProfilerChild>;
  //for some stupid reason it won't allow recoursive type definition
  { A profiler tree }
  TProfiler = class(TProfilerChild)
    { Higher level element }
    Parent: TProfiler;
    { Last access time (assigned by StartProfiler) }
    TimerStart: DTime;
    { Tree of children }
    Children: TProfilerList;
    constructor Create; //override;
    destructor Destroy; override;
  end;

var
  { Top-level element, hosts all other profiler results as Children }
  TopProfiler: TProfiler;
  { Current profiler level }
  CurrentLevel: TProfiler;

  ProgramStartTime: DTime;

  SelfUsedTime: DTime;


constructor TProfiler.Create;
begin
  //inherited Create; <-- nothing to inherit
  Children := TProfilerList.Create([doOwnsValues]);
  EntryTime := 0; //redundant
  EntryHits := 0;
end;

destructor TProfiler.Destroy;
begin
  Children.Free;
  inherited Destroy;
end;

procedure doStartProfiler(const aFunction: string); TryInline

  function FindEntry: TProfiler; TryInline
  var
    v: TProfilerChild;
    NewEntry: TProfiler;
  begin
    Result := nil;
    //try to find if the requested function is already in the Children
    if CurrentLevel.Children.TryGetValue(aFunction, v) then
      Result := v as TProfiler
    else begin
      //else - function name is not found, create a new entry for it
      NewEntry := TProfiler.Create;
      NewEntry.EntryName := aFunction;
      NewEntry.Parent := CurrentLevel;
      CurrentLevel.Children.Add(aFunction, NewEntry);
      Result := NewEntry;
    end;
  end;

var
  CurrentElement: TProfiler;
  CurrentTimer: DTime;
begin
  CurrentTimer := GetNow;

  //find entry for aFunction
  CurrentElement := FindEntry;
  //start counting time for it
  CurrentElement.TimerStart := GetNow;
  //and switch down a level
  CurrentLevel := CurrentElement;

  SelfUsedTime += GetNow - CurrentTimer;
end;

procedure doStopProfiler(const aFunction: string); TryInline
var
  CurrentTimer: DTime;
begin
  CurrentTimer := GetNow;

  repeat
    //stop counting time and record the result
    CurrentLevel.EntryTime += GetNow - CurrentLevel.TimerStart;
    //increase number of accesses to the function
    Inc(CurrentLevel.EntryHits);
    //and return to upper level profiler
    if CurrentLevel.EntryName <> aFunction then
      CurrentLevel := CurrentLevel.Parent;

    if CurrentLevel = nil then
      raise Exception.Create('FATAL. No matching StartProfiler...StopProfiler found in ' +
        aFunction);
  until CurrentLevel.EntryName = aFunction;
  //and return to upper level profiler
  CurrentLevel := CurrentLevel.Parent;

  SelfUsedTime += GetNow - CurrentTimer;
end;

{$IFDEF SortProfilerResults}
//used to sort profiler results if requested
function CompareProfiles(constref p1, p2: TProfilerChild): integer;
begin
  if p1.EntryTime > p2.EntryTime then
    Result := -1
  else
  if p1.EntryTime < p2.EntryTime then
    Result := 1
  else
    Result := 0;
end;

type
  TProfilerComparer = specialize TComparer<TProfilerChild>;

{$ENDIF}

procedure DisplayProfilerResult;

  function DisplayTime(t: DTime): string;
  begin
    Result := FloatToStr(Round(t * 1000) / 1000) + 's';
  end;

  procedure DisplayRecoursive(const aProfiler: TProfiler; const aPrefix: string);
  var
    v: TProfilerChild;
  begin
    {$IFDEF SortProfilerResults}
    aProfiler.Children.Sort(TProfilerComparer.Construct(@CompareProfiles));
    {$ENDIF}
    for v in aProfiler.Children.Values do
    begin
      WriteLnLog(aPrefix + v.EntryName +
        '(x' + IntToStr(v.EntryHits) + ')' + ' : ' +
        DisplayTime(v.EntryTime));

      DisplayRecoursive(v as TProfiler, aPrefix + '...');
    end;
  end;

begin
  WriteLnLog('--------- Profiler analysis --------');
  WriteLnLog('Total Execution Time = ' + DisplayTime(GetNow - ProgramStartTime));
  WriteLnLog('Profiler itself consumed ' + DisplayTime(SelfUsedTime));
  //the top element is not displayed, only its children
  DisplayRecoursive(TopProfiler, '');
  WriteLnLog('------------------------------------');
end;

initialization
  SelfUsedTime := 0;
  ProgramStartTime := GetNow;
  TopProfiler := TProfiler.Create;
  CurrentLevel := TopProfiler;

finalization
  DisplayProfilerResult;
  TopProfiler.Free; //will recoursively free all its Children
{$ENDIF}
end.
