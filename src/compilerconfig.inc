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

//yes, I use this horrible stuff
{$WARN 2005 off : Comment level $1 found}

{====================== LANGUAGE-SPECIFIC DIRECTIVES =========================}

{$MODE objfpc}
{$H+} // AnsiStrings
{$J-} // non-writeable constants
{$COPERATORS ON} // allow += style operators, I like them
{$GOTO OFF} // disallow goto and label

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

{ Inline code? Just in case of bugs...
  e.g. freepascal debugger doesn't show line numbers in inlined code }
{$DEFINE SUPPORTS_INLINE}

{ Gzip or not gzip, that is the answer. }
{$DEFINE gzipdata}

{ Fix a windows encoding bug }
{$IFDEF Windows}{$DEFINE UTF8Encode}{$ENDIF}

{============================== ENABLE MACRO ================================}

{ Activate useful macros }
{$INCLUDE macro.inc}

