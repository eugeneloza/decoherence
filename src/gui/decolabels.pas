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

(* Different types of labels
   All labels are rendered as images to (significantly) boost performance. *)

unit DecoLabels;

{$INCLUDE compilerconfig.inc}

interface

uses
  DecoInterfaceImages, DecoFont,
  DecoTime, DecoGlobal;

type
  { a powerful text label, converted to GLImage to be extremely fast }
  DLabelImage = class(DAbstractImage)
  private
    fText: string;
    { a set of strings each no longer than label width }
    BrokenString: DBrokenString;
    procedure SetText(const Value: string);
  public
    { text at the label }
    property Text: string read fText write SetText;
  public
    destructor Destroy; override;
    constructor Create; override;
  end;

{............................................................................}
implementation
uses
  SysUtils,
  DecoLog;

constructor DLabelImage.Create;
begin
  inherited Create;
  OwnsImage := true;
end;

{-----------------------------------------------------------------------------}

destructor DLabelImage.Destroy;
begin
  FreeAndNil(BrokenString);
  inherited Destroy;
end;

{-----------------------------------------------------------------------------}

procedure DLabelImage.SetText(const Value: string);
begin
  if fText <> Value then
  begin
    fText := Value;
    //PrepareTextImage;
  end;
end;


end.

