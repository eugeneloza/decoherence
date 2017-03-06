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

{gzip or not gzip}
{$UNDEF gzipdata}

{$interfaces corba}
