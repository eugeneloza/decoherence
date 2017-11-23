{$MACRO ON}

{$DEFINE UseProfiler}

{$IFDEF UseProfiler} //if we use profiler then create macros

{$WARN 2005 off : Comment level $1 found}
//this will start a method profiler
{$DEFINE pbegin:=begin
  StartProfiler(Self.ClassName+'.'+{$I %CURRENTROUTINE%});}
//this will start a routine profiler
{$DEFINE fbegin:=begin
  StartProfiler({$I %CURRENTROUTINE%});}
//these are actually equal they stop profiler and record the result
{$DEFINE pend:=StopProfiler;
  end}//no semicolon in the end!
{$DEFINE fend:=StopProfiler;
  end}
{$WARN 2005 on : Comment level $1 found}

{$ELSE} //otherwise just replace fbegin/fend with begin/end

{$DEFINE pbegin:=begin}
{$DEFINE fbegin:=begin}
{$DEFINE pend:=end}//no semicolon in the end!
{$DEFINE fend:=end}

{$ENDIF}
