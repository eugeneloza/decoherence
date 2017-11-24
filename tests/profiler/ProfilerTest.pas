program ProfilerTest;

{$INCLUDE ../../src/profiler.inc}

uses SysUtils, CastleClassUtils, CastleLog, Profiler;

type
  TProfiledObject = class(TObject)
    procedure MyProc1;
    procedure MyProc2;
    procedure MyProc3;
  end;

procedure TProfiledObject.MyProc1;
var i: integer;
begin
StartProfiler;

  for i := 0 to MaxInt div 10 do ;

StopProfiler;
end;

procedure TProfiledObject.MyProc2;
var i: integer;
begin
StartProfiler;

  MyProc1;
  for i := 0 to MaxInt div 7 do ;

StopProfiler;
end;

procedure TProfiledObject.MyProc3;
var i: integer;
begin
StartProfiler;

  MyProc2;
  MyProc1;
  for i := 0 to MaxInt div 14 do ;

StopProfiler;
end;

procedure RawProcedure1;
var i: integer;
begin
StartProfiler;

  for i := 0 to MaxInt div 8 do ;

StopProfiler;
end;

procedure RawProcedure2;
{
  procedure NestedProcedure;
  var k: integer;
  begin
  StartProfiler;

    for k := 0 to MaxInt div 119 do ;

  StopProfiler;
  end; --------- NOT WORKING FOR NESTED
}
var i: integer;
begin
StartProfiler;

  RawProcedure1;
  for i := 0 to MaxInt div 11 do ;

StopProfiler;
end;

var MyObj: TProfiledObject;

procedure CoreTest;
begin
StartProfiler;

  RawProcedure2;
  MyObj.MyProc3;
  RawProcedure1;
  MyObj.MyProc1;
  MyObj.MyProc2;
  RawProcedure2;
  MyObj.MyProc3;
  MyObj.MyProc2;
  RawProcedure1;
  MyObj.MyProc3;
  RawProcedure2;

StopProfiler;
end;

procedure InitTest;
begin
StartProfiler;

  MyObj.MyProc1;
  CoreTest;
  RawProcedure1;

StopProfiler;
end;

begin
  InitializeLog;

  MyObj := TProfiledObject.Create;

  InitTest;

  MyObj.Free;
end.

