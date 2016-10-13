{Copyright (C) 2012-2016 Yevhen Loza

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
unit decointerface;

{$mode objfpc}{$H+}

interface

uses
  Classes, fgl, sysutils,
  CastleLog, castleFilesUtils, CastleRandom,
  castleVectors, CastleGLImages, CastleImages,
  decoglobal;

const Frames_Folder='interface/frames/';

{1/17 of window.height is a "unit" in GUI scale.
Basically calculated as 4 characters vertical space allocation
3 lines of buttons for each
and add one line at the bottom for menu button and other stuff
i.e. 3*4+1 units in window.height
Most often equal scale is used for width - in fractions of height to maintain squares etc.}

const GUI_scale_unit_float = 1/(4*3+1);

Type
  DAbstractElement = class(TComponent)
  end;

Type
  DAbstractInterfaceElement = class(DAbstractElement)
  end;

type DInterfaceChildrenList = specialize TFPGObjectList<DAbstractInterfaceElement>;

Type
  DInterfaceElement = class(DAbstractInterfaceElement)
  public
    children: DInterfaceChildrenList;
  end;

Type
  DInterfaceContainer = class(DInterfaceElement)
  public
    { used for all interface random events }
    rnd: TCastleRandom;
    constructor create(AOwner:TComponent); override;
    destructor destroy; override;
end;

implementation

{=============================================================================}
{========================== interface container ==============================}
{=============================================================================}

constructor DInterfaceContainer.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  rnd := TCastleRandom.Create;
end;

destructor DInterfaceContainer.destroy;
begin
  freeandnil(rnd);
  inherited;
end;

end.

