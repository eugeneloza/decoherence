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

(* A Framed element - basis for most complex interface elements *)

{$INCLUDE compilerconfig.inc}

unit DecoFramedElement;

interface

uses
  DecoInterfaceCore, DecoInterfaceContainer,
  DecoFrames, DecoImages, DecoInterfaceArrangers,
  DecoGlobal, DecoTime;

type
  {}
  DAbstractFramedElement = class(DAbstractArranger)
  strict protected
    FFrame: DAbstractFrame;
    FromState, ToState: DInterfaceContainer;
    {}
    function SubstractFrame(const aContainer: DInterfaceContainer): DInterfaceContainer;
    {}
    procedure UpdateFrame;
    procedure ArrangeChildren(const Animate: TAnimationStyle; const Duration: DTime); override;
  end;

type
  { An element with a frame
    Automatically manages frame gaps based on IFrame interface
    Supports only rectagonal frames (!)
    May be used "as is", but expected to contain only one CenterArranger child }
  DRectagonalFramedElement = class(DAbstractFramedElement)
  public
    {}
    procedure LoadFrame(const aImage: DFrameImage);
  public
    constructor Create; override;
  end;

type
  {}
  DImageFramedElement = class(DAbstractFramedElement)
  public
    {}
    procedure LoadFrame(const aImage: DImage);
  public
    constructor Create; override;
  end;

{............................................................................}
implementation
uses
  DecoMath, DecoLog, Profiler;

function DAbstractFramedElement.SubstractFrame(const aContainer: DInterfaceContainer): DInterfaceContainer;
begin
  {StartProfiler}

  Result.AssignFrom(aContainer);
  if FFrame is IFrame then
  begin
    Result.w := AboveZeroInt(aContainer.w - ((FFrame as IFrame).GapLeft + (FFrame as IFrame).GapRight));
    Result.h := AboveZeroInt(aContainer.h - ((FFrame as IFrame).GapTop + (FFrame as IFrame).GapBottom));
    if Result.w > 0 then
      Result.x := aContainer.x + (FFrame as IFrame).GapLeft
    else
      Result.x := aContainer.x; //to fix zoom-in animation
    if Result.h > 0 then
      Result.y := aContainer.y + (FFrame as IFrame).GapBottom
    else
      Result.y := aContainer.y;
  end;

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

procedure DAbstractFramedElement.UpdateFrame;
begin
  {StartProfiler}

  if not (FFrame is IFrame) then
  begin
    Log(LogInterfaceError, CurrentRoutine, 'ERROR: Frame is nil!');
    //Exit; //the procedure is robust and will handle Frame=nil just as gap=0.
  end
  else
    FFrame.SetSize(Self.Next, asNone, -1);

  FromState := SubstractFrame(Self.Current);
  ToState := SubstractFrame(Self.Next);

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

procedure DAbstractFramedElement.ArrangeChildren(const Animate: TAnimationStyle; const Duration: DTime);
var
  c: DSingleInterfaceElement;
begin
  {StartProfiler}

  //inherited ArrangeChildren(Animate, Duration); <------- parent is abstract

  UpdateFrame;

  for c in Children do
    if c <> FFRame then
    begin
      c.FromToAnimate(FromState, ToState, Animate, Duration);
    end;

  {StopProfiler}
end;

{=============================================================================}

procedure DRectagonalFramedElement.LoadFrame(const aImage: DFrameImage);
begin
  {StartProfiler}

  if aImage <> nil then
  begin
    //FFrame is guaranteed to be DRectagonalFrame by Create
    (FFrame as DRectagonalFrame).Load(aImage);
    ArrangeChildren(asNone, -1); //just reset the animation
  end else
    Log(LogInterfaceError, CurrentRoutine, 'Frame image provided is nil!');

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

constructor DRectagonalFramedElement.Create;
begin
  {StartProfiler}

  inherited Create;
  FFrame := DRectagonalFrame.Create;
  Children.Add(FFrame);

  {StopProfiler}
end;

{=============================================================================}

procedure DImageFramedElement.LoadFrame(const aImage: DImage);
begin
  {StartProfiler}

  if aImage <> nil then
  begin
    //FFrame is guaranteed to be DRectagonalFrame by Create
    (FFrame as DImageFrame).Load(aImage);
    ArrangeChildren(asNone, -1); //just reset the animation
  end else
    Log(LogInterfaceError, CurrentRoutine, 'Frame image provided is nil!');

  {StopProfiler}
end;

{-----------------------------------------------------------------------------}

constructor DImageFramedElement.Create;
begin
  {StartProfiler}

  inherited Create;
  FFrame := DImageFrame.Create;
  Children.Add(FFrame);

  {StopProfiler}
end;

end.

