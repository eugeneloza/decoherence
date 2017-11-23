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

  Eventually just replace begin...end; of every routine you want to profile into:
  pbegin...pend; in case of a method (procedure/function of object)
  fbegin...fend; in case of a plain procedure/function;

  Defining SortProfilerResults would sort the routines in decreasing order
  based on how much total time they consumed (separately at every hierarchy level)

  Warning: it's not thread-safe

  Warning: it won't work for nested(local) procedures

  Requires FPC 3.1.1 and above
  Requires Castle Game Engine (Generics.Collections and CastleTimeUtils)

  I highly recommend disabling code folding in
  Lazarus IDE > Options > Editor > Code Folding
  Lazarus IDE > Options > Completion and Hints > Add Close Statements for Pascal Blocks
  as macros significantly slow down the IDE in this case
*)

unit Profiler;
interface

{$INCLUDE profiler.inc}

//{$DEFINE SortProfilerResults}

{$IFDEF UseProfiler}
{ Tries to find a profiler entry for aFunction or creates it otherwise
  assigns CurrentLevel to this function
  and starts counting time for the current function }
procedure StartProfiler(const aFunction: string); inline;
{ Stops counting time for the current function
  and records results}
procedure StopProfiler; inline;
{$ENDIF}

implementation

{$IFDEF UseProfiler}
uses SysUtils, Generics.Defaults, Generics.Collections, CastleTimeUtils;

type
  { A profiler record }
  TProfilerChild = class(TObject)
    { Profiled procedure name }
    EntryName: string;
    { Total procedure time }
    EntryTime: TFloatTime;
    { Number of procedure calls }
    EntryHits: integer;
  end;
  { List of profiler records }
  TProfilerList = specialize TObjectList<TProfilerChild>; //for some stupid reason it won't allow recoursive type definition
  { A profiler tree }
  TProfiler = class(TProfilerChild)
    { Higher level element }
    Parent: TProfiler;
    { Last access time (assigned by StartProfiler)}
    TimerStart: TTimerResult;
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

constructor TProfiler.Create;
begin
  //inherited Create; <-- nothing to inherit
  Children := TProfilerList.Create(true);
  EntryTime := 0; //redundant
  EntryHits := 0;
end;

destructor TProfiler.Destroy;
begin
  FreeAndNil(Children);
  inherited Destroy;
end;

procedure StartProfiler(const aFunction: string); inline;
  function FindEntry: TProfiler; inline;
  var
    i: integer;
    NewEntry: TProfiler;
  begin
    Result := nil;
    //try to find if the requested function is already in the Children
    for i := 0 to CurrentLevel.Children.Count-1 do
      if CurrentLevel.Children[i].EntryName = aFunction then begin
        Result := CurrentLevel.Children[i] as TProfiler;
        Exit;
      end;
    //else - function name is not found, create a new entry for it
    NewEntry := TProfiler.Create;
    NewEntry.EntryName := aFunction;
    NewEntry.Parent := CurrentLevel;
    CurrentLevel.Children.Add(NewEntry);
    Result := NewEntry;
  end;
var
  CurrentElement: TProfiler;
begin
  //find entry for aFunction
  CurrentElement := FindEntry;
  //start counting time for it
  CurrentElement.TimerStart := Timer;
  //and switch down a level
  CurrentLevel := CurrentElement;
end;

procedure StopProfiler; inline;
begin
  //stop counting time and record the result
  CurrentLevel.EntryTime += TimerSeconds(Timer, CurrentLevel.TimerStart);
  //increase number of accesses to the function
  inc(CurrentLevel.EntryHits);
  //and return to upper level profiler
  CurrentLevel := CurrentLevel.Parent;
end;

{$IFDEF SortProfilerResults}
//used to sort profiler results if requested
function CompareProfiles(constref p1, p2: TProfilerChild): integer;
begin
  if p1.EntryTime > p2.EntryTime then Result := -1 else
  if p1.EntryTime < p2.EntryTime then Result := 1 else Result := 0;
end;
type TProfilerComparer = specialize TComparer<TProfilerChild>;
{$ENDIF}

procedure DisplayProfilerResult;
  procedure DisplayRecoursive(const aProfiler: TProfiler; const aPrefix: string);
  var
    i: integer;
  begin
    {$IFDEF SortProfilerResults}
    aProfiler.Children.Sort(TProfilerComparer.Construct(@CompareProfiles));
    {$ENDIF}
    for i := 0 to aProfiler.Children.Count-1 do begin
      WriteLn(aPrefix + aProfiler.Children[i].EntryName+'(x'+IntToStr(aProfiler.Children[i].EntryHits)+')'+' : '+IntToStr(Round(aProfiler.Children[i].EntryTime*1000))+'ms');
      DisplayRecoursive(aProfiler.Children[i] as TProfiler,aPrefix+'...');
    end;
  end;
begin
  WriteLn('--------- Profiler analysis --------');
  DisplayRecoursive(TopProfiler,'');   //the top element is not displayed, only its children
  WriteLn('------------------------------------');
end;

initialization
  TopProfiler := TProfiler.Create;
  CurrentLevel := TopProfiler;

finalization
  DisplayProfilerResult;
  FreeAndNil(TopProfiler);
{$ENDIF}
end.

