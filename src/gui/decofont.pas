{ Copyright (C) 2012-2018 Yevhen Loza

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

(* Game fonts and text to image conversion *)

unit DecoFont;

{$INCLUDE compilerconfig.inc}

interface

uses
  DecoFontEncoding,
  DecoGlobal;

{}
procedure InitFonts;
{}
procedure FreeFonts;
{............................................................................}
implementation
uses
  DecoLog;


{............................................................................}

procedure InitFonts;
begin
  Log(LogInit, CurrentRoutine, 'Loading fonts.');
  InitEncoding;


  FreeEncoding; //as soon as everything is loaded, we don't need it anymore
end;

{-----------------------------------------------------------------------------}

procedure FreeFonts;
begin
  //actually everything should be freed automatically, but let it remain here for now.
end;

end.

