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

(* Management of font reading and writing *)

{$INCLUDE compilerconfig.inc}

unit DecoFontFile;

interface

uses
  Generics.Collections,
  DecoFontEncoding;

type
  { For reading-writing a Font file }
  DFontRecord = record
    {}
    URL: string;
    {}
    Size: integer;
    {}
    AdditionalLineSpacing: integer;
    {}
    Charset: TCharSet;
  end;

type
  DFontRecordDictionary = specialize TDictionary<string, DFontRecord>;

{............................................................................}
implementation

end.

