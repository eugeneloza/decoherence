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

{ Ensure thread-safety of input/output functions (hopefully) }
unit decoinputoutput;

{$INCLUDE compilerconfig.inc}
interface

uses SyncObjs,
  CastleImages, CastleXMLUtils, DOM, X3DNodes;

{$WARNING: Maybe, I'm using CriticalSection in a wrong way?}
var
  {a lock to ensure no simultaneous HDD access}
  Lock: TCriticalSection;

{safe wrapper for CastleImages.LoadImage, overloaded}
function LoadImageSafe(const URL: String): TCastleImage;
function LoadImageSafe(const URL: string;
  const AllowedImageClasses: array of TEncodedImageClass): TCastleImage;
{safe wrapper for CastleXMLUtils.URLReadXMLSafe}
function URLReadXMLSafe(const URL: String): TXMLDocument;
{safe wrapper for x3dload.Load3D}
function Load3DSafe(const URL: string): TX3DRootNode;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses SysUtils, x3dload;

function LoadImageSafe(const URL: String): TCastleImage;
begin
  Result := nil;
  Lock.Acquire;
  try
    Result := LoadImage(URL);
  finally
    Lock.Release;
  end;
end;
function LoadImageSafe(const URL: string;
  const AllowedImageClasses: array of TEncodedImageClass): TCastleImage;
begin
  Result := nil;
  Lock.Acquire;
  try
    Result := LoadImage(URL, AllowedImageClasses);
  finally
    Lock.Release;
  end;
end;

{----------------------------------------------------------------------------}

function URLReadXMLSafe(const URL: String): TXMLDocument;
begin
  Result := nil;
  Lock.Acquire;
  try
    Result := URLReadXML(URL);
  finally
    Lock.Release;
  end;
end;

{----------------------------------------------------------------------------}

function Load3DSafe(const URL: string): TX3DRootNode;
begin
  Result := nil;
  Lock.Acquire;
  try
    Result := Load3D(URL);
  finally
    Lock.Release;
  end;
end;

initialization
Lock := TCriticalSection.create;

finalization
freeAndNil(Lock);

end.

