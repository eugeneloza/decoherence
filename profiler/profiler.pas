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

  {$INCLUDE profiler.inc} in the project and add ProfilerUnit to "USES" section
  (warning: no comma before ProfilerUnit)

  You may disable/enable profiler by editing profiler.inc and
  commenting/uncommenting {$DEFINE UseProfiler} line
  When disabled, the profiler doesn't affect anything at all.

  Eventually just replace begin...end; of every routine you want to profile into:
  pbegin...pend; in case of a method (procedure/function of object)
  fbegin...fend; in case of a plain procedure/function;

  Requires FPC 3.1.1 and above
  Requires CastleUtils (Castle Game Engine)
}
{$WARN 2005 on : Comment level $1 found}

unit Profiler;
interface
uses SysUtils, Generics.Defaults, CastleUtils;

type
  TProfilerTime = TDateTime;

type
  TProfilerRecord = record
    EntryName: string;
    EntryValue: TProfilerTime;
  end;

type
  TProfilerData = specialize TStructList<TProfilerRecord>;

var
  ProfilerList: TProfilerData;

procedure SendProfilerData(const aFunction: string; const aTime: TProfilerTime); inline;
implementation

procedure SendProfilerData(const aFunction: string; const aTime: TProfilerTime); inline;
var
  _i: integer;
  _NewEntry: TProfilerRecord;
begin
  for _i := 0 to ProfilerList.Count-1 do
    if ProfilerList.L[_i].EntryName = aFunction then begin
      ProfilerList.L[_i].EntryValue += aTime;
      Exit;
    end;
  //else - function name is not found
  _NewEntry.EntryName := aFunction;
  _NewEntry.EntryValue := aTime;
  ProfilerList.Add(_NewEntry);
end;

function CompareProfiles(constref p1, p2: TProfilerRecord): integer;
begin
  if p1.EntryValue > p2.EntryValue then Result := -1 else
  if p1.EntryValue < p2.EntryValue then Result := 1 else Result := 0;
end;
type TProfilerComparer = specialize TComparer<TProfilerRecord>;

procedure DisplayProfilerResult;
var _i: integer;
begin
  ProfilerList.Sort(TProfilerComparer.Construct(@CompareProfiles));
  for _i := 0 to ProfilerList.Count-1 do
    WriteLn(ProfilerList.L[_i].EntryName+' : '+IntToStr(Round((ProfilerList.L[_i].EntryValue)*24*60*60*1000))+'ms');
end;

initialization
  ProfilerList := TProfilerData.Create;

finalization
  DisplayProfilerResult;
  FreeAndNil(ProfilerList);

end.

