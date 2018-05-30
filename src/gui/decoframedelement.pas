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
  { An element with a frame
    Automatically manages frame gaps based on IFrame interface
    Supports only rectagonal frames (!)
    May be used "as is", but expected to contain only one CenterArranger child }
  DFramedElement = class(DAbstractArranger)
  strict private
    FFrame: DRectagonalFrame;
//    procedure SetFrame(const Value: DAbstractFrame);
    function SubstractFrame(const aContainer: DInterfaceContainer): DInterfaceContainer;
  strict protected
    procedure ArrangeChildren(const Animate: TAnimationStyle; const Duration: DTime); override;
  public
    procedure LoadFrame(const aImage: DFrameImage);
  public
    constructor Create; override;
  end;

{............................................................................}
implementation
uses
  DecoMath, DecoLog;

{procedure DFramedElement.SetFrame(const Value: DAbstractFrame);
begin
  if (FFrame <> Value) and (Value is IFrame) then
  begin
    if FFrame <> nil then
      Children.Remove(FFrame);
    FFrame := Value;
    ArrangeChildren(asNone, -1); //just reset the animation
  end;
end;}

procedure DFramedElement.LoadFrame(const aImage: DFrameImage);
begin
  if aImage <> nil then
  begin
    FFrame.Load(aImage);
    ArrangeChildren(asNone, -1); //just reset the animation
  end else
    Log(LogInterfaceError, CurrentRoutine, 'Frame image provided is nil!');
end;

{-----------------------------------------------------------------------------}

function DFramedElement.SubstractFrame(const aContainer: DInterfaceContainer): DInterfaceContainer;
begin
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
end;

{-----------------------------------------------------------------------------}

procedure DFramedElement.ArrangeChildren(const Animate: TAnimationStyle; const Duration: DTime);
var
  c: DSingleInterfaceElement;
  FromState, ToState: DInterfaceContainer;
begin
  //inherited ArrangeChildren(Animate, Duration); <------- parent is abstract

  if FFrame is IFrame then
  begin
    Log(LogInterfaceError, CurrentRoutine, 'ERROR: Frame is nil!');
    //Exit; //the procedure is robust and will handle Frame=nil just as gap=0.
  end;

  FromState := SubstractFrame(Self.Current);
  ToState := SubstractFrame(Self.Next);

  for c in Children do
  begin
    c.ForceSize(FromState);
    c.SetSize(ToState, Animate, Duration);
  end;
end;

constructor DFramedElement.Create;
begin
  inherited Create;
  FFrame := DRectagonalFrame.Create;
  Children.Add(FFrame);
end;

end.

