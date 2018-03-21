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
  strict private
    fText: string;
    { Change the current fText and call Prepare Text Image if needed }
    procedure SetText(const Value: string);
    { converts string (Text) into an image }
    procedure PrepareTextImage;
  public
    { Font to print the label }
    Font: DFont;
    { Shadow intensity. Shadow=0 is no shadow (strictly) }
    ShadowIntensity: DFloat;
    { Shadow length in pixels }
    ShadowLength: integer;
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
  CastleImages,
  DecoImages,
  DecoLog;

constructor DLabelImage.Create;
begin
  inherited Create;
  ShadowIntensity := 0;
  ShadowLength := 3;
  Font := DefaultFont;
  OwnsImage := true;
end;

{-----------------------------------------------------------------------------}

destructor DLabelImage.Destroy;
begin
  inherited Destroy;
end;

{-----------------------------------------------------------------------------}

procedure DLabelImage.SetText(const Value: string);
begin
  if fText <> Value then
  begin
    fText := Value;
    PrepareTextImage;
  end;
end;

{-----------------------------------------------------------------------------}

procedure DLabelImage.PrepareTextImage;
var
  BrokenString: DBrokenString;
  TextImage: TGrayscaleAlphaImage;
begin
  FreeAndNil(Image);

  BrokenString := Font.BreakStings(fText, Next.w);
  if ShadowIntensity = 0 then
    TextImage := Font.BrokenStringToImage(BrokenString)
  else
    TextImage := Font.BrokenStringToImageWithShadow(
      BrokenString, ShadowIntensity, ShadowLength);

  FreeAndNil(BrokenString);

  Image := DImage.Create(TextImage, true, true);
end;



end.

