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

{ a thread descendant used for most game purposes }
unit DecoThread;

{$INCLUDE compilerconfig.inc}
interface

uses Classes,
  DecoGlobal;

type

  DThreadedObject = class(TObject)
    {$IFDEF ThreadLoad}
  strict private
    fThread: TThread;
    fThreadWorking: boolean;
  public
    { is Thread running? }
    function ThreadLocked: boolean;
    { report that thread has started }
    procedure LockThread;
    { report that thread has stopped }
    procedure UnlockThread;
    { source object grabs a thread to terminate it in destructor }
    procedure GrabThread(aThread: TThread);
    destructor Destroy; override;
    {$ENDIF}
    constructor Create; virtual; // override;
  end;

type
  {a thread which reports its current state and progress
   should be hold in actual state by calling UpdateProgress from the Execute}
  TAbstractThread = class(TThread)
  protected
    {current progress in 0..1}
    fProgress: float;
    {name of the current job}
    fCurrentJob: string;
    {multiplier for scaling the current progress, e.g. in case each part
     of the procedure is scaled in 0..1, then mult will equal to number of the procedures}
    fMult: float;
    {should be called to update (and scale) the progress conveniently
     pay attention: next progress is always larger than last progress
     i.e. passing values 0.1, 0.2 and 0.1 will result in progress = 0.2 (last known largest value)
     this is caused by progress most often used in procedures, making several tries to solve the taks
     to reset progress, use fprogress}
    procedure UpdateProgress(const currentJobValue: string; const ProgressValue: float);
  public
    {current progress of the thread}
    property Progress: float read fProgress;
    {name of the current job}
    property CurrentJob: string read fCurrentJob;
  end;

{redundant function to get max value of two values,
 I know there's such procedure somewhere already,
 but I was too lazy to search for it}
function Minimum(v1,v2: float): float; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses SysUtils;

procedure TAbstractThread.UpdateProgress(const CurrentJobValue: string; const ProgressValue: float);
begin
  fCurrentJob := CurrentJobValue;
  if fProgress < ProgressValue/fMult then
    fProgress := ProgressValue/fMult;
  if fProgress>1 then fProgress := 1;
end;

{------------------------------------------------------------------------------}

function Minimum(v1,v2: float): float; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  if v1>v2 then Result := v2 else Result := v1;
end;

{-----------------------------------------------------------------------------}

{$IFDEF ThreadLoad}
function DThreadedObject.ThreadLocked: boolean;
begin
 Result := fThreadWorking;
end;
procedure DThreadedObject.LockThread;
begin
  fThreadWorking := true;
end;
procedure DThreadedObject.UnlockThread;
begin
  fThreadWorking := true;
end;
procedure DThreadedObject.GrabThread(aThread: TThread);
begin
  fThread := aThread;
end;
constructor DThreadedObject.Create;
begin
  //Inherited Create;
  fThreadWorking := false;
end;
destructor DThreadedObject.Destroy;
begin
  if fThreadWorking then begin
    Try
      fThread.Terminate;
    finally
      FreeAndNil(fThread); //redundant, as freeonterminate=true?
    end;
  end;
  inherited Destroy;
end;
{$ELSE}
constructor DThreadedObject.Create;
begin
  //inherited Create;
  //just empty constructor, no threads used
end;
{$ENDIF}

end.

