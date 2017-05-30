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
    {refrence to DSoundFile / Why can't I make a cyclic refernce?}
    parent: TObject;
  protected
    {loads the file}
    procedure Execute; override;
  end;

type
  {store and manage music track}
  DSoundFile = class
  private
    {thread to load the sound file. Auto freed on terminate. Sets isLoaded flag}
    LoadThread: DSoundLoadThread;
    {full URL of the sound file}
    fURL: string;
    fisLoaded: boolean;
  public
    {reference to the sound buffer of this file}
    buffer: TSoundBuffer;
    {duration of the loaded file}
    duration: TFloatTime;

    {if the file is loaded and ready to play}
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
  {A sound file which is a Music track with additional features}
  DMusicTrack = class(DSoundFile)
  private
    {reference to "now playing" sound file to adjust its properties realtime}
    fCurrent: TSound;
    {is looping?}
    fLoop: boolean;
    {actual beginning of the fade out process}
    FadeStart: TDateTime;
    {if the file is playing at the moment}
    fisPlaying: boolean;
    {preforms fade out by adjusting the track's gain}
    procedure doFade;
  private
    {gain volume 0..1}
    const fgain = 1; {maybe, global music volume?}
    {fade out time}
    const FadeTime = 3/24/60/60; {3 seconds}
  public
    {if the current music is playing at the moment?}
    property isPlaying: boolean read fisPlaying default false;
    {prepare and start the music}
    procedure Start;
    {softly stop the music with a fade-out}
    procedure FadeOut;
    {manage the music / should be called each frame}
    procedure manage;
    {sets current music "gain", move to private?}
    procedure setGain(value: single);
    constructor create;
  end;

type
  {describes and manages music intended to have a loop-part [a..b]
   with optional intro [0..a] and ?ending [b..end]}
  DLoopMusicTrack = class(DMusicTrack)
    //a,b
  end;

type
  {a list of music tracks}
  TTrackList = specialize TFPGObjectList<DMusicTrack>;

type
  {abstract music tracks manager}
  DPlaylist = class
  public
    tracks: TTrackList;
    URLs: TStringList;
    procedure manage; virtual; abstract;
    constructor create;
    destructor destroy; override;
  end;

type
  {Tracks sequentially change each other with pre-loading and no fading
   also supports occasional silence (silence will never be the first track playing)}
  DSequentialPlaylist = class(DPlayList)

  end;

type
  {Vertical synchronized playlist for combat.
   Softly changes the synchronized tracks according to current situation.
   BeatList is optional, but recommended that fades will happen on beats change}
  DVerticalSyncPlaylist = class(DPlayList)

  end;

type
  {Manages all the playlists, currently played music and ambience
   also tries to merge playlists (not implemented yet)}
  DMusicManager = class
  private
    {current ambient track, just plays continuously}
    Ambient: DLoopMusicTrack;
    {current music playlist}
    Music,OldMusic: DPlaylist; //sequential or vertical
  public
    {manage the music, should be called each frame}
    procedure manage;

    constructor create;
    destructor destroy; override;
  end;

var
  {global music manager}
  Music: DMusicManager;

procedure initMusicManager;
procedure freeMusicManager;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses SyncObjs, SysUtils, CastleLog, castleFilesUtils,
  CastleVectors,
  decoinputoutput;
 {CastleOpenAL,}

var
  {a lock to ensure that the music won't load twice accidetnally
   ??? Maybe it's wrong to do so, we already have a critical section in LoadBufferSafe?}
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

