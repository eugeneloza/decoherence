program Project1;

{$INCLUDE profiler.inc}

uses SysUtils, CastleClassUtils, Profiler;

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
  MyProc1;
  for i := 0 to maxint div 7 do ;
pend;

procedure TProfiledObject.MyProc3;
var i: integer;
pbegin
  MyProc2;
  MyProc1;
  for i := 0 to maxint div 14 do ;
pend;

procedure RawProcedure1;
var i: integer;
fbegin
  for i := 0 to maxint div 8 do ;
fend;

procedure RawProcedure2;
  {procedure NestedProcedure;
  var k: integer;
  fbegin
    for k := 0 to maxint div 119 do ;
  fend; --------- NOT WORKING FOR NESTED}
var i: integer;
fbegin
  RawProcedure1;
  for i := 0 to maxint div 11 do ;
fend;

var MyObj: TProfiledObject;

procedure CoreTest;
fbegin
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
fend;

procedure InitTest;
fbegin
  MyObj.MyProc1;
  CoreTest;
  RawProcedure1;
fend;

begin
  MyObj := TProfiledObject.Create;

  InitTest;

  MyObj.Free;
end.

