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

{write heap trace to file or to log?}
//{$DEFINE HEAP_FILE}

{Not using interfaces yet (and maybe will never use them). But let it be here for now}
{$interfaces corba}

{convenient global range-checking on/off. Redundand, as compiler mode already does that?}
{$IFNDEF RELEASE}
{$R+}{$Q+}
{$ELSE}
{$R-}{$Q-}
{$ENDIF}

{======================== ENGINE-SPECIFIC DIRECTIVES =========================}

{inline code? Just in case of bugs...
 e.g. freepascal debugger doesn't show line numbers in inlined code}
{$define SUPPORTS_INLINE}

{gzip or not gzip, that is the answer. This one deprecated as it seems I've fixed gzip-input problem}
{$DEFINE gzipdata}

{If we attach additional Switch Nodes to 3d world management}
{$DEFINE UseSwitches}

{$IFDEF Windows}{$DEFINE UTF8Encode}{$ENDIF}

{======================== GAME-SPECIFIC DIRECTIVES =========================}

{defines fullscreen mode}
//{$DEFINE Fullscreen}

{automatically no rescale in fullscreen and Android modes}
{$IFNDEF Android}{$IFNDEF Fullscreen}{$DEFINE AllowRescale}{$ENDIF}{$ENDIF}

{undefine rescale to save memory}
{$UNDEF AllowRescale}

