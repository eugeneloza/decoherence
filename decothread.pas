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
unit decothread;

{$INCLUDE compilerconfig.inc}
interface

uses Classes,
  decoglobal;

type
  TAbstractThread = class(TThread)
  protected
    fprogress: float;
    fcurrentjob: string;
    fmult: float;
    procedure UpdateProgress(currentJobValue: string; progressValue: float);
  public
    property progress: float read fprogress;
    property currentjob: string read fcurrentjob;
  end;

function minimum(v1,v2: float): float;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

procedure TAbstractThread.UpdateProgress(currentJobValue: string; progressValue: float);
begin
  fcurrentJob := currentJobValue;
  if fprogress < progressValue*fmult then
    fprogress := progressValue*fmult;
  if fprogress>1 then fprogress := 1;
end;

function minimum(v1,v2: float): float;
begin
  if v1>v2 then result := v2 else result := v1;
end;

end.

