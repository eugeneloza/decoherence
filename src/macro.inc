{ Copyright (C) 2012-2018 Yevhen Loza, Michalis Kamburelis

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>. }

{---------------------------------------------------------------------------}

(* Some useful macros for Decoherence *)

{$MACRO ON}

//a universal macro to represent current routine name for logging and/or profiling
//works only in FPC3.1.1 and later
{$DEFINE CurrentRoutine:={$IF DECLARED(ClassName)}ClassName+'.'+{$ENDIF}{$I %CURRENTROUTINE%}}

{ define a inline replacer macro
  pay attention that TryInline should not have a semicolon after it }
{$IFDEF SUPPORTS_INLINE}
{$DEFINE TryInline:=inline;}
{$ELSE}
{$DEFINE TryInline:=}
{$ENDIF}
