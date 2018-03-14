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

(* Loading of TCastleImage -> DImage, scaling, ownership and thrash *)

unit DecoImageLoader;

{$INCLUDE compilerconfig.inc}

interface

uses
  CastleImages,
  DecoImages,
  DecoGlobal;

{ Loads a TCastleImage and scales into DImage
  the resulting DImage is freed automatically as the game ends
  KeepImageCopy determines whether the "original" image will be kept
  in memory (means anything only if the image is scaled)
  KeepProportions determines whether the image proportions will be kept during scaling }
function LoadDecoImage(const aImage: TEncodedImage; const aWidth: integer = 0;
  const aHeight: integer = 0; const KeepImageCopy: boolean = true; const KeepProportions: boolean = false): DImage;
{ Loads a DImage from an URL
  the resulting DImage is freed automatically as the game ends }
function LoadDecoImage(const FileURL: string; const aWidth: integer = 0;
  const aHeight: integer = 0; const KeepProportions: boolean = false): DImage;
{ Loads the image and scales it to full screen (GUI size) }
function LoadFullScreenImage(const FileURL: string): DImage;
{ A wrapper for TCastleImage loading
  automatically frees the image as the game ends}
function LoadCastleImage(const FileURL: string): TCastleImage;

{ Load a single cursor image with parameters }
function LoadCursorImage(const FileURL: string;
  const ShiftX, ShiftY: integer): DCursorImage;
{ Load a single frame image with parameters }
function LoadFrameImage(const FileURL: string;
  const CornerTop, CornerRight, CornerBottom, CornerLeft: integer): DFrameImage;
{............................................................................}
implementation

uses
  SysUtils,
  DecoTrash, DecoGUIScale,
  DecoLog;

function LoadDecoImage(const aImage: TEncodedImage; const aWidth: integer = 0;
  const aHeight: integer = 0; const KeepImageCopy: boolean = true; const KeepProportions: boolean = false): DImage;
var
  ScaledImage: TCastleImage;
  ScaledWidth, ScaledHeight: integer;
begin
  if (aImage = nil) or (aImage.IsEmpty) then
  begin
    Log(LogImageScaleError, CurrentRoutine, 'ERROR: No image to load.');
    Exit;
  end;

  if (aWidth <= 0) or (aHeight <= 0) then
    Result := DImage.Create(aImage, true, false) // no ownership
  else
  begin
    if not (aImage is TCastleImage) then
    begin
      Log(LogImageScaleError, CurrentRoutine, 'ERROR: Cannot Scale image ' + aImage.ClassName);
      Exit;
    end;

    ScaledWidth := aWidth;
    ScaledHeight := aHeight;
    if KeepProportions then
    begin
      if aHeight / aWidth > aImage.Height / aImage.Width then
        ScaledHeight := Round(aHeight * aImage.Height / aImage.Width)
      else
        ScaledWidth := Round(aWidth * aImage.Width / aImage.Height);
    end;

    Log(LogInterfaceImageLoading, CurrentRoutine, 'Scaling image to ' +
      IntToStr(ScaledWidth) + 'x' + IntToStr(ScaledHeight));

    if KeepImageCopy then
      ScaledImage := aImage.CreateCopy as TCastleImage
    else
    begin
      ScaledImage := aImage as TCastleImage;
      AutoFree.Extract(aImage); //<----- "extract" doesn't free the aImage unlike "remove"
    end;

    ScaledImage.Resize(ScaledWidth, ScaledHeight, InterfaceScalingMethod);

    Result := DImage.Create(ScaledImage, true, true);
  end;
  AutoFree.Add(Result);
end;

{-----------------------------------------------------------------------------}

function LoadDecoImage(const FileURL: string; const aWidth: integer = 0;
  const aHeight: integer = 0; const KeepProportions: boolean = false): DImage;
begin
  Result := LoadDecoImage(LoadCastleImage(FileURL), aWidth, aHeight, false, KeepProportions);
end;

{-----------------------------------------------------------------------------}

function LoadFullScreenImage(const FileURL: string): DImage;
begin
  Result := LoadDecoImage(LoadCastleImage(FileURL), GUIWidth, GUIHeight, false, false);
end;

{-----------------------------------------------------------------------------}

function LoadCastleImage(const FileURL: string): TCastleImage;
begin
  Log(LogInterfaceImageLoading, CurrentRoutine, 'Loading image: ' + FileURL);
  Result := LoadImage(GameFolder(FileURL));
  AutoFree.Add(Result);
end;

{-----------------------------------------------------------------------------}

function LoadCursorImage(const FileURL: string;
  const ShiftX, ShiftY: integer): DCursorImage;
begin
  Result.Image := LoadDecoImage(FileURL);
  Result.CursorShift.Data[0] := ShiftX;
  Result.CursorShift.Data[1] := ShiftY;
end;

{-----------------------------------------------------------------------------}

function LoadFrameImage(const FileURL: string;
  const CornerTop, CornerRight, CornerBottom, CornerLeft: integer): DFrameImage;
begin
  Result := LoadCastleImage(FileURL) as DFrameImage;
  Result.Corners[0] := CornerTop;
  Result.Corners[1] := CornerRight;
  Result.Corners[2] := CornerBottom;
  Result.Corners[3] := CornerLeft;
end;

end.

