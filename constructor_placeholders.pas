unit constructor_placeholders;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  constructor_global;

type

  { TPlaceholdersEditor }

  TPlaceholdersEditor = class(TWriterForm)
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    procedure LoadMe; override;
    procedure FreeMe; override;
    procedure WriteMe(ToGameFolder: boolean); override;
  end;

var
  PlaceholdersEditor: TPlaceholdersEditor;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
{$R *.lfm}

{uses CastleVectors, CastleCameras, castleLog,
     DOM, CastleXMLUtils,
     decoLoad3d, x3dLoad, CastleURIUtils, blendercleaner,
     castleFilesUtils,
     decoglobal;}

procedure TPlaceholdersEditor.LoadMe;
begin
  {$Warning dummy}
  isLoaded := true;
  isChanged := false;
end;

{------------------------------------------------------------------------------}

procedure TPlaceholdersEditor.FreeMe;
begin
  {$Warning dummy}
end;
procedure TPlaceholdersEditor.FormDestroy(Sender: TObject);
begin
  FreeMe;
end;

{------------------------------------------------------------------------------}

procedure TPlaceholdersEditor.WriteMe(ToGameFolder: boolean);
begin
  {$Warning dummy}
  inherited WriteMe(ToGameFolder);
end;

{------------------------------------------------------------------------------}

procedure TPlaceholdersEditor.FormShow(Sender: TObject);
begin
  {$warning dummy}
  LoadMe;
end;

end.

