{Copyright (C) 2012-2018 Yevhen Loza

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

(* Compiler directives/options to include in every unit *)

{====================== PASCAL-SPECIFIC DIRECTIVES =========================}

//yes, I use this horrible stuff
{$WARN 2005 off : Comment level $1 found}

{$SMARTLINK ON} // Enable smart-linking

{$MODE objfpc}
{$H+} // AnsiStrings
{$J-} // non-writeable constants
{$COPERATORS ON} // allow += style operators, I like them
{$GOTO OFF} // Disallow goto and label

{ Write heap trace to file or to log? }
//{$DEFINE HEAP_FILE}

{ Use simple corba interfaces }
{$INTERFACES corba}

{ I should try using cmem some day. They say in some cases it may +30% speed and -50% RAM usage. However, I didn't test it yet. }
//{$DEFINE useCMEM}

{======================== ENGINE-SPECIFIC DIRECTIVES =========================}

{$IFDEF RELEASE}
{$DEFINE WriteLog}
{$ENDIF}

{$IFDEF DEBUG}
{$DEFINE TextureProfiler}
{$ENDIF}

{ Inline code? Just in case of bugs...
  e.g. freepascal debugger doesn't show line numbers in inlined code }
{$DEFINE SUPPORTS_INLINE}

{ Gzip or not gzip, that is the answer. }
{$DEFINE gzipdata}

{ Fix a windows encoding bug }
{$IFDEF Windows}{$DEFINE UTF8Encode}{$ENDIF}

{================================ MOBILE-OS =================================}

{ Determines whether we're running a Desktop or Mobile OS
  used to optimize code concerning input and memory usage }
{$IFDEF ANDROID}{$DEFINE MOBILE}{$ENDIF}
{$IFDEF IOS}{$DEFINE MOBILE}{$ENDIF}
{$IFNDEF MOBILE}{$DEFINE DESKTOP}{$ENDIF}

{============================== ENABLE MACRO ================================}

{ Activate useful macros }
{$INCLUDE macro.inc}

{======================== GAME-SPECIFIC DIRECTIVES =========================}

{$DEFINE BurnerImage}

