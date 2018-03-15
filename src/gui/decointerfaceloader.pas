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

(* Load some content for the Interface *)

unit DecoInterfaceLoader;

{$INCLUDE compilerconfig.inc}

interface

uses
  DecoGlobal;


{ Read all interface images, icons, cursor pointers, and so on }
procedure LoadInterface;
procedure FreeInterface;
{............................................................................}
implementation
uses
  Generics.Collections,
  {$IFDEF BurnerImage}DecoBurner,{$ENDIF}
  DecoWind, DecoFrames,
  DecoImageLoader;

procedure LoadFrames;
begin
  FramesDictionary := TFramesDictionary.Create([doOwnsKeys]);
  FramesDictionary.Add('RegularFrame',
    LoadFrameImage('GUI/Frames/GradientFrame.png',3,3,3,3)) //AddOrSetValue;
end;


{............................................................................}

procedure LoadInterface;
begin
  {$IFDEF BurnerImage}
  InitBurnerImage;
  {$ENDIF}
  InitWind;
  LoadFrames;
end;

procedure FreeInterface;
begin
  FramesDictionary.Free;
end;

end.

