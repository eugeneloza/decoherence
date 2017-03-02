{$mode objfpc}{$H+}

{defines fullscreen mode}
//{$DEFINE Fullscreen}

{automatically no rescale in fullscreen and Android modes}
{$IFNDEF Android}{$IFNDEF Fullscreen}{$DEFINE AllowRescale}{$ENDIF}{$ENDIF}

{undefine rescale to save memory}
{$UNDEF AllowRescale}

{generic variable to inline code. Just in case of bugs...}
{$define SUPPORTS_INLINE}

{convenient global range-checking on/off}
{$IFNDEF RELEASE}
{$R+}{$Q+}
{$ENDIF}

{$interfaces corba}
