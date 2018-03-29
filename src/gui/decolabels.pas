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
  DecoInterfaceCore,
  DecoInterfaceImages, DecoFont,
  DecoTime, DecoGlobal;

type
  { Type of the label.
    ltNormal is a most nomral label, breaking in lines along the width (default).
    ltJustify is a large-text label, aligned to width.
    ltOneLine is a simple label, occupying only one line. }
  TLabelType = (ltSimple, ltJustify, ltOneLine);

type
  { A powerful text label, converted to GLImage to be extremely fast }
  DLabelImage = class(DAbstractImage)
  strict private
    fText: string;
    { Width at which the text image was rendered }
    RecentLabelWidth: integer;
    { Change the current fText and call Prepare Text Image if needed }
    procedure SetText(const Value: string);
    { Get the label width to prepare text image }
    function GetLabelWidth: integer;
    { Converts string (Text) into an image }
    procedure PrepareTextImage;
  public
    { Font to print the label }
    Font: DFont;
    { Shadow intensity. Shadow=0 is no shadow (strictly) }
    ShadowIntensity: DFloat;
    { Shadow length in pixels }
    ShadowLength: integer;
    { Label type }
    LabelType: TLabelType;
    { Text at the label }
    property Text: string read fText write SetText;
    property LabelWidth: integer read GetLabelWidth;
    procedure SizeChanged(const Animate: TAnimationStyle; const Duration: DTime); override;
  public
    destructor Destroy; override;
    constructor Create; override;
  end;

type
  { A simple FPS-counting label
    It's a separate GUI element and is used/managed directly by DGUI
    Practically it just increases FPScount by 1 each CountFPS call
    and changes displayed value approx once per second }
  DFPSLabel = class(DLabelImage)
  strict private
    FPScount: integer;
    LastRenderTime: DTime;
  public
    { Call this each frame instead of Draw,
         Draw is automatically called here }
    procedure CountFPS;
  public
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
  RecentLabelWidth := -1;
  LabelType := ltSimple;
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

function DLabelImage.GetLabelWidth: integer;
begin
  if LabelType = ltOneLine then
    Result := MaxInt div 2
  else
    Result := Next.w;
end;

{-----------------------------------------------------------------------------}

procedure DLabelImage.PrepareTextImage;
var
  TextImage: TGrayscaleAlphaImage;
begin
  FreeAndNil(Image);

  RecentLabelWidth := GetLabelWidth;

  if ShadowIntensity = 0 then
    TextImage := Font.StringToImage(fText, LabelWidth, LabelType = ltJustify)
  else
    TextImage := Font.StringToImageWithShadow(fText, LabelWidth,
      ShadowIntensity, ShadowLength, LabelType = ltJustify);

  Image := DImage.Create(TextImage, true, true);
  SetTint;
end;

{-----------------------------------------------------------------------------}

procedure DLabelImage.SizeChanged(const Animate: TAnimationStyle; const Duration: DTime);
begin
  if (FText <> '') and (LabelWidth <> RecentLabelWidth) then
  begin
    Log(LogInterfaceWarning, CurrentRoutine, 'Warning: changing size of a non-empty label; content = ' + FText);
    PrepareTextImage;
  end;
end;

{=============================================================================}
{============================ FPS label ======================================}
{=============================================================================}

constructor DFPSLabel.Create;
begin
  inherited Create;
  LabelType := ltOneLine;
  FPSCount := 0;
  LastRenderTime := -1;

  SetSize(0, 0, 1, 1);
  Font := DebugFont;
  Text := ' '; //initialize the label, so that it always has an image
end;

{---------------------------------------------------------------------------}

procedure DFPSLabel.CountFPS;
begin
  if LastRenderTime < 0 then
    LastRenderTime := DecoNow;

  if (DecoNow - LastRenderTime >= 1) then
  begin
    Text := IntToStr(FPSCount){+' '+IntToStr(Round(Window.Fps.RealTime))};
    Self.Next.SetIntSize(0, 0, Self.RealWidth, Self.RealHeight, 1);
    Self.ResetAnimation;
    FPSCount := 0;
    LastRenderTime := DecoNow;
  end
  else
    inc(FPSCount);

  Draw;
end;


end.

