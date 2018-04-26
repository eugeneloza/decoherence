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

(* Main Form of the Constructor
   It spawns and manages all other Constructor Forms *)

{$INCLUDE compilerconfig.inc}

unit ConstructorFont;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  DecoGenerics;

type
  TFontEditor = class(TForm)
    PageControl1: TPageControl;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

  public

  end;

var
  FontEditor: TFontEditor;

implementation
uses
  DecoFont, DecoFontEncoding, DecoFontFile,
  DecoLog;

{$R *.lfm}

{ TFontEditor }

{to move outside?}
procedure MakeInfoTab(constref aTab: TTabSheet; constref aFontInfo: DFontInfoDictionary);
begin
  aTab.Caption := 'Fonts';

end;

{this one certanily should be moved outisde}
procedure MakeAliasTab(constref aTab: TTabSheet; constref aAliasDictionary: DStringDictionary);
begin
  aTab.Caption := 'Aliases';
end;

procedure TFontEditor.FormCreate(Sender: TObject);
begin
  {FontInfo and FontAlias are read here}
  if not ReadFontsInfo then
    DefaultFontInfo;

  //load alias dictionary
  MakeInfoTab(PageControl1.AddTabSheet, FontInfo);
  MakeAliasTab(PageControl1.AddTabSheet, FontAlias);
end;

procedure TFontEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FontInfo);
  FreeAndNil(FontAlias);
end;

end.

