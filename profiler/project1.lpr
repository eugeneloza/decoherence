program project1;

{$INCLUDE profiler.inc}

uses SysUtils
  ProfilerUnit;

type
  TProfiledObject = class(TObject)
    procedure MyProc1;
    procedure MyProc2;
    procedure MyProc3;
  end;

procedure TProfiledObject.MyProc1;
var i: integer;
pbegin
  for i := 0 to maxint div 10 do ;
pend;

procedure TProfiledObject.MyProc2;
var i: integer;
pbegin
  for i := 0 to maxint div 7 do ;
pend;

procedure TProfiledObject.MyProc3;
var i: integer;
pbegin
  for i := 0 to maxint div 14 do ;
pend;

procedure RawProcedure1;
var i: integer;
fbegin
  for i := 0 to maxint div 8 do ;
fend;

procedure RawProcedure2;
var i: integer;
fbegin
  for i := 0 to maxint div 11 do ;
fend;

var MyObj: TProfiledObject;

begin
  MyObj := TProfiledObject.Create;

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

  MyObj.Free;
end.

