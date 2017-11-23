{$MACRO ON}

{$DEFINE UseProfiler}

{$IFDEF UseProfiler}
{$WARN 2005 off : Comment level $1 found}
{$DEFINE pbegin:=begin
  StartProfiler(Self.ClassName+'.'+{$I %CURRENTROUTINE%});}
{$DEFINE fbegin:=begin
  StartProfiler({$I %CURRENTROUTINE%});}
{$DEFINE pend:=StopProfiler;
  end}//no semicolon in the end!
{$DEFINE fend:=StopProfiler;
  end}
{$WARN 2005 on : Comment level $1 found}
{$ELSE}
{$DEFINE pbegin:=begin}
{$DEFINE fbegin:=begin}
{$DEFINE pend:=end}//no semicolon in the end!
{$DEFINE fend:=end}
{$ENDIF}
