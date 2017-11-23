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

{$WARN 2005 off : Comment level $1 found}
{ Simple macro-based profiler unit

  usage:

  {$INCLUDE profiler.inc} in the project and add Profiler unit to "USES" section

  You may disable/enable profiler by editing profiler.inc and
  commenting/uncommenting {$DEFINE UseProfiler} line
  When disabled, the profiler doesn't affect anything at all.

  Eventually just replace begin...end; of every routine you want to profile into:
  pbegin...pend; in case of a method (procedure/function of object)
  fbegin...fend; in case of a plain procedure/function;

  Warning: it's not thread-safe

  Requires FPC 3.1.1 and above
  Requires CastleUtils (Castle Game Engine)

  I highly recommend disabling code folding in
  Lazarus IDE > Options > Editor > Code Folding
  Lazarus IDE > Options > Completion and Hints > Add Close Statements for Pascal Blocks
  as macros significantly slow down the IDE in this case
}
{$WARN 2005 on : Comment level $1 found}

unit Profiler;
interface

{$INCLUDE profiler.inc}

{$IFDEF UseProfiler}
procedure StartProfiler(const aFunction: string); inline;
procedure StopProfiler; inline;
{$ENDIF}

implementation

{$IFDEF UseProfiler}
uses SysUtils, Generics.Defaults, Generics.Collections, CastleTimeUtils;

type
  TProfilerChild = class(TObject)
    EntryName: string;
    EntryTime: TFloatTime;
  end;
  TProfilerList = specialize TObjectList<TProfilerChild>; //for some stupid reason it won't allow recoursive type definition
  TProfiler = class(TProfilerChild)
    Parent: TProfiler;
    TimerStart: TTimerResult;
    Children: TProfilerList;
    constructor Create; //override;
    destructor Destroy; override;
  end;

var
  TopProfiler: TProfiler;
  CurrentLevel: TProfiler;

constructor TProfiler.Create;
begin
  //inherited Create; <-- nothing to inherit
  Children := TProfilerList.Create(true);
  EntryTime := 0; //redundant
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
    for i := 0 to CurrentLevel.Children.Count-1 do
      if CurrentLevel.Children[i].EntryName = aFunction then begin
        Result := CurrentLevel.Children[i] as TProfiler;
        Exit;
      end;
    //else - function name is not found
    NewEntry := TProfiler.Create;
    NewEntry.EntryName := aFunction;
    NewEntry.Parent := CurrentLevel;
    CurrentLevel.Children.Add(NewEntry);
    Result := NewEntry;
  end;
var
  CurrentElement: TProfiler;
begin
  CurrentElement := FindEntry;
  CurrentElement.TimerStart := Timer;
  CurrentLevel := CurrentElement;
end;

procedure StopProfiler; inline;
begin
  CurrentLevel.EntryTime := TimerSeconds(Timer, CurrentLevel.TimerStart);
  CurrentLevel := CurrentLevel.Parent;
end;

{$IFDEF SortProfilerResults}
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
    for i := 0 to aProfiler.Children.Count-1 do begin
      WriteLn(aPrefix + aProfiler.Children[i].EntryName+' : '+IntToStr(Round(aProfiler.Children[i].EntryTime*1000))+'ms');
      DisplayRecoursive(aProfiler.Children[i] as TProfiler,aPrefix+'> ');
    end;
  end;
begin
  WriteLn('--------- Profiler analysis --------');
  DisplayRecoursive(TopProfiler,'');
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

