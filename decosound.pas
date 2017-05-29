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

{ Sound and music routines
  Adaptive music and playlists }

unit decosound;

{$INCLUDE compilerconfig.inc}
interface
uses classes, fgl,
  CastleSoundEngine, CastleTimeUtils;

type
  {thread that actually loads the sound file}
  DSoundLoadThread = class(TThread)
  public
    parent: TObject;
  protected
    procedure Execute; override;
  end;

type
  {store and manage music track}
  DSoundFile = class
  private
    LoadThread: DSoundLoadThread;
    fURL: string;
    fisLoaded: boolean;
  public
    {}
    buffer: TSoundBuffer;
    {}
    duration: TFloatTime;

    property isLoaded: boolean read fisLoaded default false;
    {creates and starts LoadThread. When finished runs LoadFinished}
    procedure Load;
    procedure Load(URL: string);
    procedure LoadFinished;
    constructor create;
    constructor create(URL: string);
    destructor destroy; override;
  end;

type
  {}
  DMusicTrack = class(DSoundFile)
  private
    fCurrent: TSound;
    fLoop: boolean;
    FadeStart: TDateTime;
    fisPlaying: boolean;
    procedure doFade;
  private
    const fgain = 1; {maybe, global music volume?}
    const FadeTime = 3/24/60/60; {3 seconds}
  public
    property isPlaying: boolean read fisPlaying default false;
    procedure Start;
    procedure FadeOut;
    procedure manage;
    procedure setGain(value: single);
    constructor create;
  end;

type
  {}
  DLoopMusicTrack = class(DMusicTrack)
    //a,b
  end;

type
  {}
  TTrackList = specialize TFPGObjectList<DMusicTrack>;

type
  {}
  DPlaylist = class
  public
    tracks: TTrackList;
    URLs: TStringList;
    constructor create;
    destructor destroy; override;
  end;

type
  {}
  DSequentialPlaylist = class(DPlayList)

  end;

type
  {}
  DVerticalSyncPlaylist = class(DPlayList)

  end;

type
  {}
  DMusicManager = class
  private
    Ambient: DLoopMusicTrack;
    Music,OldMusic: DPlaylist; //sequential or vertical
  public
    procedure manage;
    constructor create;
    destructor destroy; override;
  end;

var Music: DMusicManager;

procedure initMusicManager;
procedure freeMusicManager;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses SyncObjs, SysUtils, CastleLog, castleFilesUtils,
  CastleVectors,
  decoinputoutput;
 {CastleOpenAL,}

var
  {a lock to ensure that the music won't load twice accidetnally}
  MusicLock: TCriticalSection;

{========================== INTERNAL TYPES =================================}


{========================== TMusicLoadThread ===============================}

procedure DSoundLoadThread.Execute;
begin
  if not MusicLock.TryEnter then begin
    WriteLnLog('TMusicLoadThread.Execute','Another thread is already loading music. Exiting.');
    {$HINT Something more usefull should happen here... The game situation has already changed, so, maybe, we just need to wait for this thread to finish and load the next one at once?}
    exit;
  end;
  //MusicLock.Acquire;

  //parent := DSoundFile.create;

  (parent as DSoundFile).buffer := LoadBufferSafe((parent as DSoundFile).fURL,(parent as DSoundFile).duration);

  MusicLock.Release;

  {$warning dummy}
  //FreeAndNil(parent);
  (parent as DSoundFile).LoadFinished;
end;

{============================= DSoundFile ==================================}

constructor DSoundFile.create;
begin
  inherited;
  buffer := 0;
  duration := -1;
end;
constructor DSoundFile.create(URL: string);
begin
  self.create;
  fURL := URL;
  self.Load;
end;

{---------------------------------------------------------------------------}

procedure DSoundFile.Load(URL :string);
begin
  fURL := URL;
  self.Load;
end;
procedure DSoundFile.Load;
begin
  if fURL='' then begin
    writeLnLog('DSoundFile.Load','ERROR: No valid URL provided. Exiting...');
    exit;
  end;
  LoadThread := DSoundLoadThread.Create(true);
  LoadThread.Priority := tpLower;
  LoadThread.parent := self;
  LoadThread.FreeOnTerminate := true;
  LoadThread.Start;
end;
procedure DSoundFile.LoadFinished;
begin
  fisLoaded := true;
end;

{---------------------------------------------------------------------------}

destructor DSoundFile.destroy;
begin
  soundengine.FreeBuffer(buffer);
  if Assigned(LoadThread) then begin
    WriteLnLog('DSoundFile.destroy','Warning, Assigned(LoadThread)=true. Terminating and freeing...');
    LoadThread.Terminate;
    FreeAndNil(LoadThread); //if thread is working, terminate it;
  end;
  inherited;
end;

{========================== DMusicTrack ==================================}

procedure DMusicTrack.Start;
begin
  if not isLoaded then begin
    WriteLnLog('DMusicTrack.Start','ERROR: Music is not loaded!');
    exit;
  end;
  fCurrent := SoundEngine.PlaySound(self.buffer, false, fLoop, 10, fgain, 0, 1, ZeroVector3Single);
  if fCurrent = nil then WriteLnLog('DMusicTrack.Start','ERROR: Unable to allocate music!');
  fisPlaying := true;
end;

procedure DMusicTrack.FadeOut;
begin
  FadeStart := now;
end;

procedure DMusicTrack.manage;
begin
  if isPlaying then
    if FadeStart>0 then doFade;
end;

constructor DMusicTrack.create;
begin
  FadeStart := -1;
end;

procedure DMusicTrack.doFade;
begin
  if Now-FadeStart<FadeTime then begin
    setGain(1-(Now-FadeStart)/FadeTime);
  end else begin
    SetGain(0);       //fade to zero
    fCurrent.Release; //stop the music
    fisPlaying := false;
  end;
end;

procedure DMusicTrack.setGain(value: single);
begin
  if Assigned(fCurrent) and fCurrent.PlayingOrPaused then
    fCurrent.Gain := value
  else begin
    //fGain := value;
    WriteLnLog('DMusicTrack.setGain','Warning: Setting gain of a non-playing music track...');
  end;
end;

{============================ DPlaylist ================================}

constructor DPlaylist.create;
begin
  Inherited;
  Tracks := TTrackList.Create(true);
  URLs := TStringList.create;
end;

{---------------------------------------------------------------------------}

destructor DPlayList.destroy;
begin
  freeAndNil(Tracks);
  FreeAndNil(URLs);
  Inherited;
end;

{============================ DMusicManager ================================}

procedure DMusicManager.manage;
begin

end;

{---------------------------------------------------------------------------}

constructor DMusicManager.create;
begin

end;

{---------------------------------------------------------------------------}

destructor DMusicManager.destroy;
begin

end;

{============================ other routines ===============================}

procedure initMusicManager;
begin
  Music := DMusicManager.create;
end;

{---------------------------------------------------------------------------}

procedure freeMusicManager;
begin
  FreeAndNil(Music);
end;



initialization
MusicLock := TCriticalSection.create;

finalization
freeAndNil(MusicLock);

end.

