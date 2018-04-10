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

(* Handles Frames of the interface elements:
   Image frames - drawn as simple underlying image
   Rectagonal frames - drawn as Draw3x3 *)

{$INCLUDE compilerconfig.inc}

unit DecoFrames;

interface

uses
  Generics.Collections,
  CastleVectors,
  DecoImages, DecoInterfaceImages,
  DecoGlobal;

type
  { Just an alias to be able to assign a DSimpleImage as a frame
    However, in some time, we might want to add "burner" to
    non-rectagonal frames. Thou I'm unsure if it worth the trouble. }
  DAbstractFrame = DAbstractImage;

type
  { Despite the differences, gaps must be calculated for both frames types }
  IFrame = interface
  ['{E8BBFE03-2AFD-4042-891E-565654657415}']
    function GapLeft: integer;
    function GapBottom: integer;
    function GapRight: integer;
    function GapTop: integer;
  end;

type
  DImageFrame = class(DSimpleImage, IFrame)
  strict private
    Corners: TVector4Integer;
  public
    function GapLeft: integer;
    function GapBottom: integer;
    function GapRight: integer;
    function GapTop: integer;
    { Determine frame gaps to take space between the frame and the content
      Specifying value < 0 means this corner will be equal to gLeft
      i.e. to SetGap(10,10,10,10) just SetGap(10) may be used;
      as image frames are usually expected to be symmetric }
    procedure SetGap(const gLeft: integer = -1; const gBottom: integer = -1;
  const gRight: integer = -1; const gTop: integer = -1);
  end;

type
  { Rectagonal frame is scaled 3x3 and accepts DFrameImage}
  DRectagonalFrame = class(DAbstractFrame, IFrame)
  strict private
    { frame must be a DImage to scale properly during animations
      this is just a source image link, stored only until next render
      the delayed rescale is made as a safe-guard to avoid accident
      frame sclaing before the NEXT was initialized properly }
    FrameImage: DFrameImage;
  strict private
    Corners: TVector4Integer;
    InitPending: boolean;
    procedure ResizeFrame;
  public
    function GapLeft: integer;
    function GapBottom: integer;
    function GapRight: integer;
    function GapTop: integer;
    { Determine additional gaps for the frame (including frame internal gaps)
      Specifying value < 0 means this corner will be equal to gLeft
      i.e. to SetGap(10,10,10,10) just SetGap(10) may be used }
    procedure SetGap(const gLeft: integer = 0; const gBottom: integer = -1;
      const gRight: integer = -1; const gTop: integer = -1);
  public
    procedure Draw; override;
    { Load a frame image here }
    procedure Load(const aImage: DFrameImage);
  public
    constructor Create; override;
  end;

type
  TFramesDictionary = specialize TObjectDictionary<string, DFrameImage>;

var
  FramesDictionary: TFramesDictionary;

function GetFrameByName(const ItemName: string): DFrameImage; TryInline
{............................................................................}
implementation
uses
  SysUtils,
  CastleImages, CastleRectangles,
  {$IFDEF BurnerImage}DecoBurner,{$ENDIF}
  DecoLog;

function GetFrameByName(const ItemName: string): DFrameImage; TryInline
begin
  Result := nil; //to avoid uninitialized variable hint
  if not FramesDictionary.TryGetValue(ItemName, Result) then
  begin
    Log(LogInterfaceError, CurrentRoutine, 'Unknown Frame Name: ' + ItemName);
    Result := nil;
  end;
end;

{=============================================================================}

function DImageFrame.GapLeft: integer;
begin
  Result := Corners[0];
end;
function DImageFrame.GapBottom: integer;
begin
  Result := Corners[1];
end;
function DImageFrame.GapRight: integer;
begin
  Result := Corners[2];
end;
function DImageFrame.GapTop: integer;
begin
  Result := Corners[3];
end;

{-----------------------------------------------------------------------------}

procedure DImageFrame.SetGap(const gLeft: integer = -1; const gBottom: integer = -1;
  const gRight: integer = -1; const gTop: integer = -1);
begin
  if gLeft > 0 then
    Corners[0] := gLeft
  else
    Corners[0] := 0;

  if gBottom >= 0 then
    Corners[1] := gBottom
  else
    Corners[1] := Corners[0];

  if gRight >= 0 then
    Corners[2] := gRight
  else
    Corners[2] := Corners[0];

  if gTop >= 0 then
    Corners[3] := gTop
  else
    Corners[3] := Corners[0];
end;

{=============================================================================}

function DRectagonalFrame.GapLeft: integer;
begin
  Result := FrameImage.Corners[0] + Corners[0];
end;
function DRectagonalFrame.GapBottom: integer;
begin
  Result := FrameImage.Corners[1] + Corners[1];
end;
function DRectagonalFrame.GapRight: integer;
begin
  Result := FrameImage.Corners[2] + Corners[2];
end;
function DRectagonalFrame.GapTop: integer;
begin
  Result := FrameImage.Corners[3] + Corners[3];
end;

{-----------------------------------------------------------------------------}

procedure DRectagonalFrame.SetGap(const gLeft: integer = 0; const gBottom: integer = -1;
  const gRight: integer = -1; const gTop: integer = -1);
begin
  if gLeft > 0 then
    Corners[0] := gLeft
  else
    Corners[0] := 0;

  if gBottom >= 0 then
    Corners[1] := gBottom
  else
    Corners[1] := Corners[0];

  if gRight >= 0 then
    Corners[2] := gRight
  else
    Corners[2] := Corners[0];

  if gTop >= 0 then
    Corners[3] := gTop
  else
    Corners[3] := Corners[0];
end;

{-----------------------------------------------------------------------------}

procedure DRectagonalFrame.ResizeFrame;
var
  TempImage: TCastleImage;
begin
  FreeAndNil(Image);
  TempImage := TRGBAlphaImage.Create(Next.w, Next.h);
  TempImage.Clear(Vector4(0,0,0,0));
  TempImage.DrawFrom3x3( Rectangle(0, 0, Next.w, Next.h),
    FrameImage.Image, FrameImage.Corners, dmOverwrite, InterfaceScalingMethod);
  {$IFDEF BurnerImage}Burn(TempImage, Next);{$ENDIF}
  Image := DImage.Create(TempImage, true, true);
  SetTint;
end;

{-----------------------------------------------------------------------------}

procedure DRectagonalFrame.Draw;
begin
  if InitPending then ResizeFrame;
  inherited Draw;
end;

{-----------------------------------------------------------------------------}

procedure DRectagonalFrame.Load(const aImage: DFrameImage);
begin
  FrameImage := aImage;
  InitPending := true;
end;

{-----------------------------------------------------------------------------}

constructor DRectagonalFrame.Create;
begin
  inherited Create;
  InitPending := false;
  OwnsImage := true;
  Corners := TVector4Integer.Zero;
end;

end.

