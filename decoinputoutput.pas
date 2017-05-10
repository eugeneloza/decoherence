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

var Lock: TCriticalSection;

{}
function LoadImageSafe(const URL: String): TCastleImage;
function LoadImageSafe(const URL: string;
  const AllowedImageClasses: array of TEncodedImageClass): TCastleImage;
{}
function URLReadXMLSafe(const URL: String): TXMLDocument;
{}
function LoadBlenderX3DSafe(const URL: string): TX3DRootNode;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses SysUtils,
  decoload3d;

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

function LoadBlenderX3DSafe(const URL: string): TX3DRootNode;
begin
  Result := nil;
  Lock.Acquire;
  try
    Result := LoadBlenderX3D(URL);
  finally
    Lock.Release;
  end;
end;

initialization
Lock := TCriticalSection.create;

finalization
freeAndNil(Lock);

end.

