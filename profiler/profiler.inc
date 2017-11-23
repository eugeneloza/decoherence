{$MACRO ON}

{$DEFINE UseProfiler}

{$IFDEF UseProfiler}
{$WARN 2005 off : Comment level $1 found}
{$DEFINE pbegin:=
  var _t: TProfilerTime;
  begin
  _t := Now;}
{$DEFINE fbegin:=pbegin}
{$DEFINE pend:=
  SendProfilerData(Self.ClassName+'.'+{$I %CURRENTROUTINE%},Now-_t);
  end}//no semicolon in the end!
{$DEFINE fend:=
  SendProfilerData({$I %CURRENTROUTINE%},Now-_t);
  end}//no semicolon in the end!
{$DEFINE ProfilerUnit:=, Profiler} //warning: no comma should be before this macro
{$WARN 2005 on : Comment level $1 found}
{$ELSE}
{$DEFINE pbegin:=begin}
{$DEFINE fbegin:=begin}
{$DEFINE pend:=end}//no semicolon in the end!
{$DEFINE fend:=end}
{$DEFINE ProfilerUnit:=}
{$ENDIF}
