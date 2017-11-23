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
  Adaptive music and playlists.

  Sequential playlist. Just plays a random track from a provided list.

  Adaptive (or inetractive) music is the term used for the music tracks
  that adapt to current game situation, providing much better music blending
  with the gameplay. There are two most used ways to use Adaptive Music.

  Horizontal synchronization. The tracks match each other's beat, so that
  they can be safely crossfaded one into another at the same playing position.

  Vertical synchronization. The tracks match each other's harmony, so that
  during crossfading the tracks will seamlessly blend into one another.

  Of course only specially composed tracks that have the same rhythm
  and harmony can be smoothly synchronized.}
unit DecoSound;

{$INCLUDE compilerconfig.inc}
interface
uses Classes, Generics.Collections,
  CastleSoundEngine, CastleTimeUtils,
  DecoGlobal;

type
  {thread that actually loads the sound file}
  DSoundLoadThread = class(TThread)
  public
    {refrence to DSoundFile / Why can't I make a cyclic refernce?}
    Parent: DObject; //DSoundFile
  protected
    {loads the file}
    procedure Execute; override;
  end;

type
  {store and manage music track}
  DSoundFile = class (DObject)
  private
    {thread to load the sound file. Auto freed on terminate. Sets isLoaded flag}
    //LoadThread: DSoundLoadThread;
    //ThreadWorking: boolean;
    {full URL of the sound file}
    fURL: string;
    fisLoaded: boolean;
  public
    {reference to the sound buffer of this file}
    Buffer: TSoundBuffer;
    {duration of the loaded file}
    Duration: TFloatTime;

    {if the file is loaded and ready to play}
    property isLoaded: boolean read fisLoaded default false;
    {creates and starts LoadThread. When finished runs LoadFinished}
    procedure Load;
    procedure Load(const URL: string);
    procedure LoadFinished;

    constructor Create; virtual;//override;
    constructor Create(const URL: string); virtual;
    destructor Destroy; override;
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
    const FadeTime = 3; {in seconds}
  public
    {if the current music is playing at the moment?}
    property isPlaying: boolean read fisPlaying default false;
    {prepare and start the music}
    procedure Start;
    {softly stop the music with a fade-out}
    procedure FadeOut;
    {manage the music / should be called each frame}
    procedure Manage;
    {sets current music "gain", move to private?}
    procedure SetGain(const Value: single);
    constructor Create; override;
    constructor Create(const URL: string); override;
  end;

{situation differentiator. Best if assigned to const mysituation = 0.
 situations must start from zero and be sequential. // todo?}
type TSituation = byte;

type
  {describes and manages music intended to have a loop-part [a..b]
   with optional intro [0..a] and ?ending [b..end]}
  DLoopMusicTrack = class(DMusicTrack)
    //a,b
    Tension: single;
    //Situation: TSituation;
  end;

type
  {a list of music tracks}
  TTrackList = specialize TObjectList<DMusicTrack>;
  TLoopTrackList = specialize TObjectList<DLoopMusicTrack>;

type
  {the most abstract features of the playlist}
  DAbstractPlaylist = class(DObject)
    public
      {manage the playlist (loading, start, stop, fade)}
      procedure Manage; virtual; abstract;
    end;

type
  {abstract music tracks manager}
  DPlaylist = class(DAbstractPlaylist)
  public
    {URLs of the music files in this playlist}
    URLs: TStringList;
    {load all tracks in the URLs list}
    //procedure LoadAll; // this is wrong in every way!

    constructor Create; virtual;
    destructor Destroy; override;
  end;

type
  {Tracks sequentially change each other with pre-loading and no fading
   also supports occasional silence (silence will never be the first track playing)}
  DSequentialPlaylist = class(DPlayList)
    protected
      {tracks of this playlist}
      Tracks: TTrackList;  {$hint it looks wrong! Only 2 (now playing & fading + there might be >1 fading) tracks are needed at every moment of time}
      {number of the previous track, stored not to repeat that track again if possible}
      PreviousTrack: integer;
    public
      {(prepare to) load the next random track from the playlist}
      procedure LoadNext;
      constructor Create; override;
      destructor Destroy; override;
    end;

{"tension" of the current track which determines which of the synchronized
 tracks should be used at this moment.}
type TTension = single;

type
  {Synchronized playlist for combat.
   Softly changes the synchronized tracks according to current situation.
   BeatList is optional, but recommended that fades will happen on beats change}
  DSyncPlaylist = class(DPlayList)
    private
      TensionChanged: boolean;
      fTension: TTension;
      procedure SetTension(const Value: TTension);
      function GetTrack(const NewTension: TTension): integer;
    protected
      {list of tracks in the playlist. All the tracks are horizontally synchronized
       and MUST have the same rhythm,
       the melody line should also be blendable.
       On the other hand, playing together with MultiSyncPlaylist in case the
       tension changes only on situation change, then the tracks must only have
       the same beat, and can be unblendable between one another}
      Tracks: TLoopTrackList;
    public
      {current music tension. It's a good idea to keep this value in 0..1 range,
       however it's not mandatory}
      property Tension: TTension read fTension write SetTension;

      procedure Manage; override;

      constructor Create; override;
      destructor Destroy; override;
    end;

type
  {This is a set of several "vertically synchronized" synchronized playlists
   The set of playlists sholud blend smoothly with one another depending on the situation,
   i.e. the blending takes place in "2D space" of tension/situation,
   e.g. situations may be player's/enemie's turn
   with different tension depending on amount/strengths of enemy units visible
   If the tension may change only on situation change, then there is no need to
   blend harmony of all the tracks in the playlists, only different possible
   situation changes should blend flawlessly.}
  DMultiSyncPlaylist = class(DAbstractPlaylist)
    private
      fSituation: TSituation;
      SituationChanged: boolean;
      procedure SetSituation(const Value: TSituation);
    public
      {vertically synchronized synchronized playlists :)
       You have to manage their blendability based on possible situation change,
       e.g. an incoming nuke might happen only on enemy's turn, there's no need
       to blend it with player's turn.}
      PlayLists: array of DSyncPlaylist;
      {}
      property Situation: TSituation read fsituation write setsituation;
      procedure Manage; override;
    end;


type
  {Manages all the playlists, currently played music and ambience
   also tries to merge playlists (not implemented yet)}
  DMusicManager = class(DObject)
  private
    {current ambient track, just plays continuously}
    Ambient: DLoopMusicTrack;
    {current music playlist}
    Music,OldMusic: DPlaylist; //sequential or synchronized
  public
    {manage the music, should be called each frame
     or otherwise relatively often
     Note, that loading a new track/playlist takes some time depending on HDD speed
     so while MusicManager tries its best to make it as quick as possible,
     the requested change usually won't be immediate,
     especially in case of an unexpected change}
    procedure Manage;

    constructor Create; virtual;//override
    destructor Destroy; override;
  end;

var
  {global music manager}
  Music: DMusicManager;

procedure InitMusicManager;
procedure FreeMusicManager;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
implementation
uses SysUtils, castleFilesUtils,
  CastleVectors,
  //DecoHDD, //used for safe threaded loading of sound buffer
  DecoTime, DecoLog;

{========================== TMusicLoadThread ===============================}

procedure DSoundLoadThread.Execute;
begin
  //issue HDD lock
 (Parent as DSoundFile).Buffer := SoundEngine.LoadBuffer((parent as DSoundFile).fURL,(parent as DSoundFile).duration);
 (Parent as DSoundFile).LoadFinished;
end;

{============================= DSoundFile ==================================}

constructor DSoundFile.Create;
begin
  inherited Create;
  buffer := 0;
  duration := -1;
end;
constructor DSoundFile.Create(const URL: string);
begin
  Self.Create;
  fURL := URL;
  Self.Load;
end;

{---------------------------------------------------------------------------}

procedure DSoundFile.Load(const URL :string);
begin
  fURL := URL;
  Self.Load;
end;
procedure DSoundFile.Load;
begin
  if fURL='' then begin
    Log(LogSoundError,{$I %CURRENTROUTINE%},'ERROR: No valid URL provided. Exiting...');
    Exit;
  end;
  {if not ThreadLocked then begin
    {LoadThread := DSoundLoadThread.Create(true);
    LoadThread.Priority := tpLower;
    LoadThread.parent := self;
    LoadThread.FreeOnTerminate := true;
    LoadThread.Start;
    ThreadWorking := true; }
  end
  else
     Log(LogSoundError,{$I %CURRENTROUTINE%},'Thread already working...');}
end;
procedure DSoundFile.LoadFinished;
begin
  fisLoaded := true;
end;

{---------------------------------------------------------------------------}

destructor DSoundFile.Destroy;
begin
  SoundEngine.FreeBuffer(Buffer);
  inherited Destroy;
end;

{========================== DMusicTrack ==================================}

procedure DMusicTrack.Start;
begin
  if not isLoaded then begin
    Log(LogSoundError,{$I %CURRENTROUTINE%},'ERROR: Music is not loaded!');
    Exit;
  end;
  fCurrent := SoundEngine.PlaySound(self.buffer, false, fLoop, 10, fgain, 0, 1, TVector3.Zero);
  if fCurrent = nil then Log(LogSoundError,{$I %CURRENTROUTINE%},'ERROR: Unable to allocate music!');
  fisPlaying := true;
end;

{---------------------------------------------------------------------------}

procedure DMusicTrack.FadeOut;
begin
  FadeStart := DecoNow;
end;

{---------------------------------------------------------------------------}

procedure DMusicTrack.manage;
begin
  if isPlaying then
    if FadeStart>0 then doFade;
end;

{---------------------------------------------------------------------------}

constructor DMusicTrack.Create;
begin
  inherited Create;
  FadeStart := -1;
end;
constructor DMusicTrack.create(const URL: string);
begin
  inherited Create(URL);
  FadeStart := -1;
end;

{---------------------------------------------------------------------------}

procedure DMusicTrack.doFade;
begin
  if decoNow-FadeStart<FadeTime then begin
    setGain(1-(decoNow-FadeStart)/FadeTime);
  end else begin
    SetGain(0);       //fade to zero
    fCurrent.Release; //stop the music
    fisPlaying := false;
  end;
end;

{---------------------------------------------------------------------------}

procedure DMusicTrack.setGain(const Value: single);
begin
  if Assigned(fCurrent) and fCurrent.PlayingOrPaused then
    fCurrent.Gain := Value
  else begin
    //fGain := value;
    Log(LogSoundError,{$I %CURRENTROUTINE%},'Warning: Setting gain of a non-playing music track...');
  end;
end;

{============================ DPlaylist ================================}

constructor DPlaylist.Create;
begin
  //inherited Create;
  URLs := TStringList.Create;
end;

{---------------------------------------------------------------------------}

destructor DPlayList.Destroy;
begin
  FreeAndNil(URLs);
  inherited Destroy;
end;

{---------------------------------------------------------------------------}

{procedure DPlayList.LoadAll;
var s: string;
    musicTrack: DMusicTrack;
begin
  {$warning this is wrong! we need to load only one url at once in sequential playlist!}
  for s in URLs do
    musicTrack := DMusicTrack.create(s);
  {$hint different implementation for loop tracks}
  {$hint delayed load of the music files!}
end;}

{---------------------------------------------------------------------------}

procedure DSequentialPlaylist.LoadNext;
var NewTrack: integer;
begin
  if URLs.Count=1 then PreviousTrack := -1; //if only one track is available, then forget about shuffling
  //shuffle tracks, but don't repeat the previous one
  repeat
    NewTrack := DRND.Random(URLs.Count);
  until NewTrack<>PreviousTrack;
  {$hint process silence here}
  //load here
end;

{---------------------------------------------------------------------------}

constructor DSequentialPlaylist.Create;
begin
  inherited Create;
  PreviousTrack := -1;
  Tracks := TTrackList.Create;
end;

destructor DSequentialPlaylist.Destroy;
begin
  FreeAndNil(Tracks);
  inherited Destroy;
end;

{---------------------------------------------------------------------------}

procedure DSyncPlaylist.settension(const Value: TTension);
begin
  if gettrack(ftension){current track playing}<>gettrack(value) then tensionchanged := true;
  ftension := value;
end;

{---------------------------------------------------------------------------}

function DSyncPlaylist.gettrack(const NewTension: TTension): integer;
var i: integer;
    tension_dist: single;
begin
  tension_dist := 9999; //some arbitrary large value
  Result := -1;
  {$warning the track may be not loaded yet!}
  //select a track that fits the new tension best //todo: optmize?
  for i := 0 to Tracks.count do
    if abs(tracks[i].Tension - NewTension)<Tension_Dist then begin
      tension_dist := abs(Tracks[i].Tension - newtension);
      Result := i;
    end;
end;

{---------------------------------------------------------------------------}

procedure DSyncPlaylist.manage;
begin
  tensionchanged := false;
end;

{---------------------------------------------------------------------------}

constructor DSyncPlaylist.Create;
begin
  inherited Create;
  Tracks := TLoopTrackList.Create;
end;

{---------------------------------------------------------------------------}

destructor DSyncPlaylist.Destroy;
begin
  FreeAndNil(Tracks);
  inherited Destroy;
end;

{---------------------------------------------------------------------------}

procedure DMultiSyncPlaylist.SetSituation(const Value: TSituation);
begin
  if fsituation<>value then begin
    situationChanged := true;
    fsituation := value;
  end;
end;

{---------------------------------------------------------------------------}

procedure DMultiSyncPlaylist.manage;
begin
  situationchanged := false;
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

procedure InitMusicManager;
begin
  fLog(LogInitSound,{$I %CURRENTROUTINE%},'Creating music manager...');
  Music := DMusicManager.Create;
end;

{---------------------------------------------------------------------------}

procedure FreeMusicManager;
begin
  fLog(LogInitSound,{$I %CURRENTROUTINE%},'Freeing music manager...');
  FreeAndNil(Music);
end;

{
initialization

finalization
}
end.

