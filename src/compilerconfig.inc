{Copyright (C) 2012-2017 Yevhen Loza

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

{ Compiler directives/options to include in every unit }

{====================== LANGUAGE-SPECIFIC DIRECTIVES =========================}

{$mode objfpc}
{$H+} //AnsiStrings
{$J-} //non-writeable constants

{ Write heap trace to file or to log? }
//{$DEFINE HEAP_FILE}

{ Use simple corba interfaces }
{$interfaces corba}

{ I should try using cmem some day. They say in some cases it may +30% speed and -50% RAM usage. However, I didn't test it yet. }
//{$DEFINE useCMEM}

{ Convenient global range-checking on/off. Redundand, as compiler mode already does that? }
{$IFNDEF RELEASE}
{$R+}{$Q+}
{$ELSE}
{$R-}{$Q-}
{$ENDIF}

{======================== ENGINE-SPECIFIC DIRECTIVES =========================}

{ Inline code? Just in case of bugs...
  e.g. freepascal debugger doesn't show line numbers in inlined code }
{$DEFINE SUPPORTS_INLINE}

{ Use thread for loading stuff in DecoHDD? }
{$DEFINE ThreadLoad}

{ Gzip or not gzip, that is the answer. }
{$DEFINE gzipdata}

{ If we attach additional Switch Nodes to 3d world management }
{$DEFINE UseSwitches}

{ Fix a windows encoding bug }
{$IFDEF Windows}{$DEFINE UTF8Encode}{$ENDIF}

{======================== GAME-SPECIFIC DIRECTIVES =========================}

{ Defines fullscreen mode }
//{$DEFINE Fullscreen}

{ Automatically no rescale in fullscreen and Android modes }
{$IFNDEF Android}{$IFNDEF Fullscreen}{$DEFINE AllowRescale}{$ENDIF}{$ENDIF}

{ Undefine rescale to save memory. Temporary. }
//{$UNDEF AllowRescale}

{======================= EXPERIMENTAL ======================================}

{ A burner image is applied to frames and some other interface elements }
//{$DEFINE BurnerImage}

{======================= SOME DEBUG STUFF ==================================}

{$WARN 2005 off : Comment level $1 found}