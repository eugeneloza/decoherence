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

(* Ensure thread-safety of input/output functions (hopefully) *)

{$INCLUDE compilerconfig.inc}

unit DecoHDD;

interface

uses
  CastleImages,
  CastleXMLUtils, DOM,
  X3DNodes,
  CastleSoundEngine;

{safe wrapper for CastleImages.LoadImage, overloaded}
function LoadImageSafe(const URL: String): TCastleImage;
function LoadImageSafe(const URL: string;
  const AllowedImageClasses: array of TEncodedImageClass): TCastleImage;
{safe wrapper for CastleXMLUtils.URLReadXML and URLWriteXML}
function URLReadXMLSafe(const URL: String): TXMLDocument;
procedure URLWriteXMLSafe(Doc: TXMLDocument; const URL: String);
{safe wrapper for soundengine.loadbuffer}
function LoadBufferSafe(const URL: string): TSoundBuffer;
{safe wrapper for x3dload.Load3D}
function Load3DSafe(const URL: string): TX3DRootNode;

{ Initialize HDD lock which prevents simultaneous access to HDD from threads }
procedure InitHDDLock;
{ Free HDD lock }
procedure FreeHDDLock;
{............................................................................}
implementation
uses
  SyncObjs,
  X3DLoad;

{============================================================================}
{====================== SAFE LOADING (WITH LOCKS) ==========================}
{============================================================================}

var
  {a lock to ensure no simultaneous HDD access}
  HDD_Lock: TCriticalSection;

function LoadImageSafe(const URL: String): TCastleImage;
begin
  Result := nil;
  HDD_Lock.Acquire;
  try
    Result := LoadImage(URL);
  finally
    HDD_Lock.Release;
  end;
end;
function LoadImageSafe(const URL: string;
  const AllowedImageClasses: array of TEncodedImageClass): TCastleImage;
begin
  Result := nil;
  HDD_Lock.Acquire;
  try
    Result := LoadImage(URL, AllowedImageClasses);
  finally
    HDD_Lock.Release;
  end;
end;

{----------------------------------------------------------------------------}

function URLReadXMLSafe(const URL: String): TXMLDocument;
begin
  Result := nil;
  HDD_Lock.Acquire;
  try
    Result := URLReadXML(URL);
  finally
    HDD_Lock.Release;
  end;
end;

{----------------------------------------------------------------------------}

procedure URLWriteXMLSafe(Doc: TXMLDocument; const URL: String);
begin
  HDD_Lock.Acquire;
  try
    URLWriteXML(Doc, URL);
  finally
    HDD_Lock.Release;
  end;
end;

{----------------------------------------------------------------------------}

function Load3DSafe(const URL: string): TX3DRootNode;
begin
  Result := nil;
  HDD_Lock.Acquire;
  try
    Result := Load3D(URL);
  finally
    HDD_Lock.Release;
  end;
end;

{----------------------------------------------------------------------------}

function LoadBufferSafe(const URL: string): TSoundBuffer;
begin
  Result := nil;
  HDD_Lock.Acquire;
  try
    Result := SoundEngine.LoadBuffer(URL);
  finally
    HDD_Lock.Release;
  end;
end;

{............................................................................}

procedure InitHDDLock;
begin
  HDD_Lock := TCriticalSection.create;
end;

{----------------------------------------------------------------------------}

procedure FreeHDDLock;
begin
  HDD_Lock.Free;
end;


end.

