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

{ Global variables and definitions for Constructor }
unit Constructor_Global;

{$INCLUDE compilerconfig.inc}
interface

uses
  Classes, Forms, SysUtils,
  //Controls,
  StdCtrls,
  DecoFile,
  DecoTranslation,

  DOM, CastleXMLUtils;

const
  {all data that will remain in Architect folder and used only in Constructor}
  LocalFolder = 'local/';

type
  {}
  DHeaderModule = class(DDataModule)
  strict private const
    s_Module = 'Header';
    s_FriendlyName = 'FriendlyName';
    s_Comment = 'Comment';
  public
    FriendlyName: string;
    Comment: string;
    function WriteModule: TDOMNode; override;
    procedure ReadModule(const aParent: TDOMElement); override;
  {$IFDEF Constructor}
  public
    procedure ConstructInterface; override;
    procedure ReadInterface; override;
  {$ENDIF}
  end;

type
  {a generic Form with abstract write/load and several other abstract
   routines to simplify and automate constructor's data management}
  TWriterForm = class(TForm)
  private
    fisLoaded: boolean;
    fisChanged: boolean;
  strict protected
    { an exact copy of DObject.Log }
    procedure Log(const LogLevel: boolean; const aProcedure, aMessage: string);
  public
    {if the TWriterForm instance is loaded from the Architect?}
    property isLoaded: boolean read fisLoaded write fisLoaded default false;
    {Was the TWriterForm changed since last save/load?}
    property isChanged: boolean read fisChanged write fisChanged default false;
    {TWriterForm abstract load procedure}
    procedure LoadMe; virtual; abstract;
    {TWriterForm abstract save procedure}
    procedure WriteMe(const ToGameFolder: boolean); virtual;
    {TWriterForm abstract destructor}
    procedure FreeMe; virtual; abstract;
  end;

type LanguageChangeCallback = procedure of object;

type
  TLanguageForm = class (TWriterForm)
  private
    fMyLanguage: TLanguage;
  public
    LanguageSwitch: TComboBox;
    OnLanguageChange: LanguageChangeCallback;

    {TWriterForm current displayed language}
    property MyLanguage: TLanguage read fMyLanguage write fMyLanguage;

    Procedure MakeLanguageSwitch;
    {sets MyLanguage to ConstructorLanguage and returns "true" if they were different}
    Function ResetLanguageSwitch: boolean;
    Procedure DetectLanguageSelect; virtual;
    procedure LanguageSelectChange(Sender: TObject);
  end;

var ConstructorLanguage: TLanguage;

{analogue to castleFilesUtils.ApplicationData (and made based on it)
 but points to ARCHITECT directory (true) or ApplicationData (false)
 WARNING! This is a buggy procedure. No folder can be named "data" in the game files!!!}
function ConstructorData(const URL: string; const ToGameFolder: boolean): string;
{Desktop-only ApplicationData analogue. Replacement for ApplicationData for
 native FPC functions that don't work with URLs
 The result is relative to application folder!}
function FakeConstructorData(const URL: string; const ToGameFolder: boolean): string;

{searches a StringList for the specific element
 This is an inoptimal algorithm
 and MAYBE there is already a ready algorithm in strutils! I didn't look too well}
function StringListContains(const SL: TStringList; const Search: string): boolean;

{reads a specific file extensions from a specific path
 Creates a TStringList, don't forget to free manually
 !Android incompatible}
function GetFilesList(const Path, Ext: string): TStringList;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation

uses CastleFilesUtils, StrUtils,
  DecoLog, CastleLog;

{case-sensitive replace the last occurence of searchstring to replacestring}
procedure ReplaceStringReverse(var s: string; const SearchString, ReplaceString: string);
var i: integer;
begin
  for i := Length(s)-Length(SearchString) downto 0 do
    if Copy(s,i,Length(SearchString)) = SearchString then begin
      s := Copy(s,0,i-1) + ReplaceString + Copy(s,i+Length(SearchString),Length(s));
      Break;
    end;
end;

{-----------------------------------------------------------------------------}

function ConstructorData(const URL: string; const ToGameFolder: boolean): string;
begin
  Result := ApplicationData(URL);
  if not ToGameFolder then begin
    ReplaceStringReverse(Result,'/data/','/architect/');
    //{$Warning folder names cannot contain /data/ folder!}
    //result := AnsiReplaceText(Result,'/data/','/architect/');
    //invoke data compression
    {$IFDEF gzipdata}result := AnsiReplaceText(Result,'.gz','.xml');{$ENDIF}
  end;
end;

{-----------------------------------------------------------------------------}

function FakeConstructorData(const URL: string; const ToGameFolder: boolean): string;
begin
  if ToGameFolder then
    Result := 'data/'+URL
  else
    Result := 'architect/'+URL;
  Result := AnsiReplaceText(Result,'/',PathDelim); //we're using native OS file access
end;

{-----------------------------------------------------------------------------}

function StringListContains(const SL: TStringList; const Search: string): boolean;
var s: string;
begin
  Result := false;
  if SL = nil then begin
    fLog(LogConstructorError,{$I %CURRENTROUTINE%},'ERROR: String List is nil!');
    Exit;
  end;
  for s in SL do if s = Search then begin
    Result := true;
    Break;
  end;
end;

{-----------------------------------------------------------------------------}

function GetFilesList(const Path, Ext: string): TStringList;
var Rec: TSearchRec;
begin
  Result := TStringList.Create;
  // Android incompatible
  if FindFirst (FakeConstructorData(Path + '*.'+Ext,false), faAnyFile - faDirectory, Rec) = 0 then
   try
     repeat
       Result.Add(AnsiReplaceText(Rec.Name,'.'+Ext,''));
     until FindNext(Rec) <> 0;
   finally
     FindClose(Rec);
   end;
end;

{============================================================================}

procedure TWriterForm.WriteMe(const ToGameFolder: boolean);
begin
  if not ToGameFolder then isChanged := false;
end;

{---------------------------------------------------------------------------}

procedure TWriterForm.Log(const LogLevel: boolean; const aProcedure, aMessage: string);
begin
  if not doLog then Exit;
  if LogLevel then WriteLnLog(Self.ClassName+'.'+aProcedure,aMessage)
end;

{============================================================================}

Procedure TLanguageForm.MakeLanguageSwitch;
var L: TLanguage;
begin
  if LanguageSwitch = nil then begin
    LanguageSwitch := TComboBox.Create(Self);
    LanguageSwitch.Parent := Self;  //required to be displayed on the form
    LanguageSwitch.Style := csDropDownList;
    LanguageSwitch.Hint := 'Select current displayed language.';
    LanguageSwitch.ShowHint := true;
    LanguageSwitch.Left := 208;
    LanguageSwitch.Top := 8;
    LanguageSwitch.Width := 100;
    LanguageSwitch.Height := 21;
    LanguageSwitch.Visible := true;
    LanguageSwitch.Enabled := true;
    for L in TLanguage do
      LanguageSwitch.Items.Add(SayLanguage(L));
    LanguageSwitch.ItemIndex := 0;
    LanguageSwitch.OnChange := @LanguageSelectChange;
  end else Log(LogConstructorError,{$I %CURRENTROUTINE%},'ERROR: LanguageSelect already exists.');
  ResetLanguageSwitch;
end;

{---------------------------------------------------------------------------}

function TLanguageForm.ResetLanguageSwitch: boolean;
var i: integer;
begin
  Result := MyLanguage <> ConstructorLanguage;
  if Result then begin
    MyLanguage := ConstructorLanguage;
    if Assigned(Self.OnLanguageChange) then Self.OnLanguageChange;
    For i := 0 to LanguageSwitch.Items.Count-1 do
      if Trim(LanguageSwitch.Items[i]) = SayLanguage(MyLanguage) then
              LanguageSwitch.ItemIndex := i;
  end;
end;

{---------------------------------------------------------------------------}

Procedure TLanguageForm.DetectLanguageSelect;
var L: TLanguage;
    NL: TLanguage;
begin
  //not optimal
  NL := MyLanguage;
  For L in TLanguage do
    if Trim(LanguageSwitch.Items[LanguageSwitch.ItemIndex]) = SayLanguage(L) then
    NL := L;
  if NL <> MyLanguage then begin
    MyLanguage := NL;
    if Assigned(Self.OnLanguageChange) then Self.OnLanguageChange;
  end;
  //what should happen in case of no language found?
end;
Procedure TLanguageForm.LanguageSelectChange(Sender: TObject);
begin
  DetectLanguageSelect;
end;

{============================================================================}

function DHeaderModule.WriteModule: TDOMNode;
var ValueNode, TextNode: TDOMNode;
begin
  inherited WriteModule;
  Result := Parent.CreateElement(s_Module);

  ValueNode := Parent.CreateElement(s_FriendlyName);
  TextNode := Parent.CreateTextNode(UTF8decode(FriendlyName));
  ValueNode.AppendChild(TextNode);
  Result.AppendChild(ValueNode);

  ValueNode := Parent.CreateElement(s_Comment);
  TextNode := Parent.CreateTextNode(UTF8decode(Comment));
  ValueNode.AppendChild(TextNode);
  Result.AppendChild(ValueNode);
end;

{---------------------------------------------------------------------------}

procedure DHeaderModule.ReadModule(const aParent: TDOMElement);
var ContainerNode, ValueNode: TDOMElement;
begin
  inherited ReadModule(aParent);
  ContainerNode := aParent.ChildElement(s_Module);
  ValueNode := ContainerNode.ChildElement(s_FriendlyName);
  FriendlyName := ValueNode.TextData;
  ValueNode := ContainerNode.ChildElement(s_Comment);
  Comment := ValueNode.TextData;
end;

{---------------------------------------------------------------------------}

{$IFDEF Constructor}

procedure DHeaderModule.ConstructInterface;
begin

end;

{---------------------------------------------------------------------------}

procedure DHeaderModule.ReadInterface;
begin

end;

{$ENDIF}

end.

