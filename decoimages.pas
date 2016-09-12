unit decoimages;

{$mode objfpc}{$H+}

interface

uses classes, fgl,
  CastleLog,
  CastleControls, CastleImages, castleFilesUtils,
  global_var;

Type DImageFrame=class(TCastleImage)
  timedelay:TDateTime;
end;

type DListOfImages=specialize TFPGObjectList<DImageFrame>;

type DImage=class(TCastleImageControl)
  private

  public
    animationstart:TDateTime;
    animate:boolean;
    Images:DListOfImages;
    {t:time in miliseconds}
    procedure LoadFrame(const file_name:string; tt:float);
end;

implementation

procedure DImage.LoadFrame(const file_name:string; tt:float);
var newImage:DImageFrame;
begin
  newImage:=LoadImage(ApplicationData(file_name)) as DImageFrame;
  newImage.timedelay:=tt/1000/24/60/60; {convert to TDateTime}
  images.Add(newImage);
end;

end.

