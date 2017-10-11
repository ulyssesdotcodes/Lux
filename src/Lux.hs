{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}


module Lux where

import Debug.Trace

import Prelude hiding (Right, Left, lookup, delete)

import LambdaDesigner.Op as LD
import LambdaDesigner.Lib as LD

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception (finally, catch)
import Control.Lens
import Control.Lens.Reified
import Control.Monad (forM_, forever)
import Control.Monad.State.Lazy hiding (fix)
import Data.Aeson as A
import qualified Data.Bool as B
import Data.IORef
import Data.List hiding (lookup)
import Data.Map.Strict as M (Map, toList, fromList, (!), lookup, member, delete, map, keys)
import Data.Matrix (fromList)
import Data.Maybe
import Data.Text (Text, unpack)
import qualified Network.WebSockets as WS
import Numeric (showIntAtBase)
import System.Random
import Text.Printf


import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T

type Client = (Int, WS.Connection)

type TOPRunner = ((Tree TOP, Tree CHOP) -> IO ())
data ServerState =
  ServerState { _clients :: [Client]
              , _runner :: TOPRunner
              , _password :: Text
              , _tdState :: TDState
              }

data Message = Connecting Text
  | RegisterVote Int
  | DoShowVote Text [Int]
  | DoFilmVote Text [Int]
  | ForceFilmVote Int
  | Reset
  | KitchenScene
  | Reel1
  | Reel2
  | Reboot
  | OffsetTime Float
-- Cues
  | PreshowLoop
  | ROS2
  | Calibration
  | Countdown
  | PlaneTracker1
  | Glitch1
  | FilmBreak
  | HackingScene1
  | HackingScene2
  | PlaneTracker2
  | Memoriam
  | Glitch2
  | FilmBreak2
  | OperaLoading
  | Opera
  | EndingB
  | EndingC
  | HauntedHouseEnd
  | EndCredits
  | SV2Driving
  | SV2Moustache
  | SV5One
  | SV5Two
  | SV5Three
  | SV6Language
  | SV6FleshChassis
-- Audio
  | QA
  | ROS1
  | ROS3
  | PaperPlane1
  | HauntedHouse1
  | PaperPlane2
  | HauntedHouse2
  | ROS4
  | PaperPlane3
  | HauntedHouse3
  | ROS5
  | PaperPlane4
  | HauntedHouse4
  | ROS6
  | PaperPlane5
  | HauntedHouse5
  | SV2Championship
  | SV3MTA
  | SV3Western
  | SV3Medieval
  | SV4Laser
  | SV4DanceParty
  | SV4TShirt
  | SV6TimeCapsule
  -- Triger(movie,lux,etc), GotoTime,

data OutputState = Tree TOP

instance FromJSON Message where
  parseJSON = withObject "message" $ \o -> do
    ty <- o .: "type"
    case ty of
      "connecting" -> Connecting <$> o.: "password"
      "vote" -> RegisterVote <$> o .: "index"
      "doShowVote" -> DoShowVote <$> o .: "question" <*> o .: "votes"
      "doFilmVote" -> DoFilmVote <$> o .: "question" <*> o .: "votes"
      "forceFilmVote" -> ForceFilmVote <$> o .: "vote"
      "offsetTime" -> OffsetTime <$> o .: "timeOffset"
      "kitchenScene" -> return KitchenScene
      "reel1" -> return Reel1
      "reel2" -> return Reel2
      "reboot" -> return Reboot
      "reset" -> return Reset
      a -> maybe (fail ("Unknown type " ++ ty)) return (msum [lookup a audioCues, lookup a videoCues])
-- Cues
videoCues :: Map String Message
videoCues = M.fromList
      [ ("preshowloop", PreshowLoop)
      , ("ROS2", ROS2)
      , ("calibration", Calibration)
      , ("countdown", Countdown)
      , ("planetracker1", PlaneTracker1)
      , ("glitch1", Glitch1)
      , ("filmbreak", FilmBreak)
      , ("hackingscene1", HackingScene1)
      , ("hackingscene2", HackingScene2)
      , ("planetracker2", PlaneTracker2)
      , ("memoriam", Memoriam)
      , ("glitch2", Glitch2)
      , ("filmbreak2", FilmBreak2)
      , ("operaloading", OperaLoading)
      , ("opera", Opera)
      , ("endingb", EndingB)
      , ("endingc", EndingC)
      , ("haunted", HauntedHouseEnd)
      , ("end", EndCredits)
      , ("qa", QA)
      , ("driving.mov", SV2Driving)
      , ("moustache.mov", SV2Moustache)
      , ("show5one.mov", SV5One)
      , ("show5two.mov", SV5Two)
      , ("show5three.mov", SV5Three)
      , ("language.mov", SV6Language)
      , ("fleshchassis.mov", SV6FleshChassis)
      ]

audioCues :: Map String Message
audioCues = M.fromList
      [ ("ROS1", ROS1)
      , ("ROS3", ROS3)
      , ("paperplane1", PaperPlane1)
      , ("hauntedhou", HauntedHouse1)
      , ("paperplane2", PaperPlane2)
      , ("ROS", ROS4)
      , ("paperplane3", PaperPlane3)
      , ("ROS5", ROS5)
      , ("paperplane4", PaperPlane4)
      , ("paperplane5", PaperPlane5)
      , ("SV2Championship", SV2Championship)
      , ("SV3MTA", SV3MTA)
      , ("SV3Western", SV3Western)
      , ("SV3Medieval", SV3Medieval)
      , ("SV4Laser", SV4Laser)
      , ("SV4DanceParty", SV4DanceParty)
      , ("SV4TShirt", SV4TShirt)
      , ("SV6TimeCapsule", SV6TimeCapsule)
      ]

data OutMsg = VotesMsg [Text] | PasswordResult Bool [(Int, Text)] [Text] [Text]

instance ToJSON OutMsg where
  toJSON (VotesMsg vs) = object ["type" A..= "vote", "votes" A..= vs]
  toJSON (PasswordResult b vs vcues acues) = object [ "type" A..= "password"
                                        , "success" A..= b
                                        , "votes" A..= ((\(i, t) -> object ["index" A..= i, "text" A..= t]) <$> vs)
                                        , "vcues" A..= vcues
                                        , "acues" A..= acues
                                        ]

type Effect = Tree TOP -> Tree TOP

data CueAudioTrack = ATOne | ATTwo

data TDState = TDState { _activeVotes :: ActiveVotes
                       , _filmVotePool :: Map Int FilmVote
                       , _lastVoteWinner :: Maybe VoteText
                       , _voteQuestion :: Maybe Text
                       , _voteTimer :: Maybe Int
                       , _movie :: MovieData
                       , _secondaryMovie :: Maybe MovieData
                       , _overlayVoteScreen :: Maybe MovieData
                       , _overlays :: [ MovieData ]
                       , _altmovie :: Maybe MovieData
                       , _effects :: [ Effect ]
                       , _soundtracks :: [BS.ByteString]
                       , _cueAudioSwitch :: CueAudioTrack
                       , _cueAudioOne :: Maybe BS.ByteString
                       , _cueAudioTwo :: Maybe BS.ByteString
                       , _inCamera :: Int
                       , _resetMovie :: Bool
                       , _resetSecondary :: Bool
                       , _resetVoteTimer :: Bool
                       , _rlist :: [Float]
                       }

data MovieData = MovieData { _movieId :: Int
                           , _movieFile :: TDState -> String
                           , _movieLength :: Float
                           , _movieCycle :: Bool
                           , _movieEffects :: Bool
                           , _movieTimeOffset :: Float
                           }

data DeckIndex = Left | Right deriving (Show, Eq)

type CrypticName = Text
type DecrypticName = Text
newtype VoteText = VoteText { voteNames :: (CrypticName, DecrypticName) } deriving Show

class Vote a where
  run :: a -> TDState -> TDState
  voteText :: a -> VoteText

data ShowVote = ShowVote { showVoteText :: VoteText }

instance Vote ShowVote where
  run = id . flip const
  voteText (ShowVote vt) = vt

data FilmVote = FilmVote VoteText FilmData

data FilmData = InCamera Int
              | Effect (Tree TOP -> Tree TOP)
              | Audio Int
              | AltCamera Int
              | Overlay Int

data ActiveVotes = ShowVotes [ (ShowVote, Int) ]
                 | FilmVotes [ (Int, Int) ]
                 | NoVotes


makeLenses ''MovieData
makeLenses ''TDState
makeLenses ''ServerState

instance Show TDState where
  show td@(TDState {_activeVotes, _filmVotePool, _lastVoteWinner, _movie, _effects, _soundtracks}) =
    "activeVotes=" ++ (show $ activeVoteTexts _activeVotes) ++ " lastVoteWinner=" ++ show _lastVoteWinner ++
    " movie" ++ show (_movieFile _movie td) ++ " audioTracks" ++ show _soundtracks

instance Vote FilmVote where
  run (FilmVote _ (InCamera e)) td = td & inCamera +~ e
  run (FilmVote _ (Audio file)) td = td & soundtracks %~ ((audios ! file):)
  run (FilmVote _ (Effect eff)) td = td & effects %~ (eff:)
  run (FilmVote _ (AltCamera id)) td = td & altmovie ?~ (films ! id)
  run (FilmVote _ (Overlay id)) td = td & overlays %~ ((films ! id):)
  voteText (FilmVote vt _) = vt

-- Vote is a

-- Every vote: announcement that vote is up, voting update w/ countdown, 10 sec announcement, vote locked - loading vote audio, vote
-- data VoteType =
--   ShowVote -- [nothing, extra audio file, screen c video] votes are always the same 3 in the same order
--   | FilmVote -- switch video files, adding effect, swap out of video audio (doesn't have to be sunken, but check if it can do it easily (timeline to timeline of show?)), 3 channels of audio (primary (can be switched to xyz), soundtrack, director comentary)

-- Will be pulling from different pool for each vote and removing from master if it's selected
-- Votes each have: question, cryptic selection, decrypted text
-- Second movie file on screen A w/ alpha
-- See which vote for show control
-- Kitchen scene (and in memorium) overrides everything - just make TD show kitchen scene then go back to regular
-- Paper plane cue
-- Show control UI shows Vote 1, Vote 2, Vote 3, Vote 4
-- Color votes randomly after show vote 4 (decide on prearranged or random) but film vote right after show vote 4 is blue, green, red
-- Ending is [BS.ByteString] of films and the runner will click "next clip" when appropriate

-- newType Vote (a :: VoteType) b = Vote Int

data TimerState = Start | Stop
type Timer = (TVar TimerState, TMVar ())

activeVoteTexts :: ActiveVotes -> [ VoteText ]
activeVoteTexts (ShowVotes vs) = voteText . fst <$> vs
activeVoteTexts (FilmVotes vs) = catMaybes $ fmap voteText . flip lookup filmVotes .  fst <$> vs
activeVoteTexts NoVotes = []

newServerState :: TOPRunner -> IO ServerState
newServerState tr = newTDState >>= pure . ServerState [] tr "password"

newTDState :: IO TDState
newTDState = newStdGen >>= pure . TDState NoVotes filmVotes Nothing Nothing Nothing (films ! 0) Nothing Nothing [] Nothing [] [] ATOne Nothing Nothing 0 True True False . randoms

filmVotes :: Map Int FilmVote
filmVotes = M.fromList [ (1, FilmVote (VoteText ("Six foot Orange", "SFO")) (InCamera 2))
                      , (2, FilmVote (VoteText ("Director Redux", "DR")) (InCamera 4))
                      , (3, FilmVote (VoteText ("Black & White", "B&W")) (Effect $ glslTP' id "scripts/bandw.glsl" [] . (:[])))
                      , (4, FilmVote (VoteText ("VHS", "V")) (Effect $ glslTP' id "scripts/vhs.glsl" [("i_time", emptyV4 & _1 ?~ seconds)] . (:[])))
                      , (5, FilmVote (VoteText ("Annoyance", "A")) (Audio 0))
                      , (6, FilmVote (VoteText ("Square", "S")) (Effect $ glslTP' id "scripts/crop.glsl" [("uAspectRatio", emptyV4 & _1 ?~ float 1)] . (:[])))
                      , (7, FilmVote (VoteText ("Cinescope", "C")) (Effect $ glslTP' id "scripts/crop.glsl" [("uAspectRatio", emptyV4 & _1 ?~ float 2.35)] . (:[])))
                      , (8, FilmVote (VoteText ("Imax", "I")) (Effect $ glslTP' id "scripts/crop.glsl" [("uAspectRatio", emptyV4 & _1 ?~ float 1.43)] . (:[])))
                      , (9, FilmVote (VoteText ("Artistic Significance", "AS")) (InCamera 8))
                      , (10, FilmVote (VoteText ("Webcam", "W")) (Effect $ compT 0 . (vidIn:) . (:[])))
                      , (11, FilmVote (VoteText ("Roller skates", "RS")) (InCamera 1))
                      , (12, FilmVote (VoteText ("Space Opera", "SO")) (InCamera 16))
                      -- , (13, FilmVote (VoteText ("3rd Grader", "3G")) (AltCamera 5))
                      , (14, FilmVote (VoteText ("Fish cam", "FC")) (Overlay 6))
                      -- , (15, FilmVote (VoteText ("Chicken cam", "CC")) (AltCamera 7))
                      , (16, FilmVote (VoteText ("Bottle Vision", "BV")) (AltCamera 8))
                      , (17, FilmVote (VoteText ("Subtitles 1", "S1")) (Overlay 9))
                      , (18, FilmVote (VoteText ("Subtitles 2", "S2")) (Overlay 10))
                      , (19, FilmVote (VoteText ("Soundtrack 1", "S1")) (Audio 1))
                      , (20, FilmVote (VoteText ("Soundtrack 2", "S2")) (Audio 2))
                      , (21, FilmVote (VoteText ("Soundtrack 3", "S3")) (Audio 3))
                      ]

films :: Map Int MovieData
films = M.fromList [ (0, MovieData 0 (printf "Holme/0%05b.mov" . _inCamera) 1357 False True 0)
                   , (1, MovieData 1 (const "Holme/100000.mov") 600 False False 0)
                   , (2, MovieData 2 (printf "Holme/3%05b.mov" . _inCamera) 1357 False True 0)
                   , (5, MovieData 5 (const "Holme/third_grader.mov") 1357 False False 0)
                   , (6, MovieData 6 (const "Holme/fish_cam.mov") 1357 True False 0)
                   , (7, MovieData 7 (const "Holme/chicken_cam.mov") 1357 False False 0)
                   , (8, MovieData 8 (const "Holme/bottle_vision.mov") 1357 False False 0)
                   , (9, MovieData 9 (const "Holme/sub1.mov") 1357 False False 0)
                   , (10, MovieData 10 (const "Holme/sub2.mov") 1357 False False 0)
                   , (11, MovieData 11 (const "Holme/sub_3.mov") 1357 False False 0)
                   , (12, MovieData 12 (const "Holme/mocap_man.mov") 1357 False False 0)
                   , (13, MovieData 13 (const "Holme/subliminals.mov") 1357 False False 0)
                   , (14, MovieData 14 (const "Holme/preshowloop.mov") 900 True False 0)
                   , (15, MovieData 15 (const "Holme/mrdna.mov") 900 False False 0)
                   , (16, MovieData 16 (const "Holme/calibration.mov") 180 False False 0)
                   , (17, MovieData 17 (const "Holme/countdown.mov") 30 False False 0)
                   , (18, MovieData 18 (const "Holme/tracker1.mov") 240 False False 0)
                   , (19, MovieData 19 (const "Holme/glitch1.mov") 240 False False 0)
                   , (20, MovieData 20 (const "Holme/filmbreak.mov") 60 False False 0)
                   , (21, MovieData 21 (const "Holme/hack1.mov") 120 False False 0)
                   , (22, MovieData 22 (const "Holme/hack2.mov") 240 False False 0)
                   , (23, MovieData 23 (const "Holme/tracker2.mov") 720 False False 0)
                   , (24, MovieData 24 (const "Holme/memoriam.mov") 300 False False 0)
                   , (25, MovieData 25 (const "Holme/glitch2.mov") 240 False False 0)
                   , (26, MovieData 26 (const "Holme/filmbreak2.mov") 120 False False 0)
                   , (27, MovieData 27 (const "Holme/operaload.mov") 360 False False 0)
                   , (28, MovieData 28 (const "Holme/opera.mov") 600 False False 0)
                   , (29, MovieData 29 (const "Holme/ending B.mov") 600 False False 0)
                   , (30, MovieData 30 (const "Holme/ending C.mov") 600 False False 0)
                   , (31, MovieData 31 (const "Holme/hauntedend.mov") 120 False False 0)
                   , (32, MovieData 32 (const "Holme/endcredits.mov") 900 False False 0)
                   , (33, MovieData 33 (const "Holme/qa.mov") 900 False False 0)
                   , (34, MovieData 34 (const "Holme/supertitles.mov") 600 False False 0)
                   , (34, MovieData 34 (const "Holme/supertitles.mov") 600 False False 0)
                   , (35, MovieData 35 (const "Holme/driving.mov") 300 False False 0)
                   , (36, MovieData 36 (const "Holme/moustache.mov") 300 False False 0)
                   , (37, MovieData 37 (const "Holme/show5one.mov") 300 False False 0)
                   , (38, MovieData 38 (const "Holme/show5two.mov") 300 False False 0)
                   , (39, MovieData 39 (const "Holme/show5three.mov") 300 False False 0)
                   , (40, MovieData 40 (const "Holme/language.mov") 300 False False 0)
                   , (41, MovieData 41 (const "Holme/fleshchassis.mov") 300 False False 0)
                   ]

audios :: Map Int BS.ByteString
audios = M.fromList [ (0, "Holme/TAKEONMEUCC.mp3")
                    , (1, "Holme/soundtrack_1.mp3")
                    , (2, "Holme/soundtrack_2.mp3")
                    , (3, "Holme/soundtrack_3.mp3")
                    , (4, "Holme/soundtrack_3.mp3")
                    , (5, "Holme/luxos1.mp3")
                    , (6, "Holme/buddymusical.mp3")
                    , (7, "Holme/plane1.mp3")
                    , (8, "Holme/haunted1.mp3")
                    , (9, "Holme/championship.mp3")
                    , (10, "Holme/plane2.mp3")
                    , (11, "Holme/haunted2.mp3")
                    , (12, "Holme/mta.mp3")
                    , (13, "Holme/western.mp3")
                    , (14, "Holme/Medieval.mp3")
                    , (15, "Holme/luxos2.mp3")
                    , (16, "Holme/luxoscomedy.mp3")
                    , (17, "Holme/plane3.mp3")
                    , (18, "Holme/haunted3.mp3")
                    , (19, "Holme/luxos3.mp3")
                    , (20, "Holme/plane4.mp3")
                    , (21, "Holme/haunted4.mp3")
                    , (22, "Holme/laser.mp3")
                    , (23, "Holme/danceparty.mp3")
                    , (24, "Holme/tshirt.mp3")
                    , (25, "Holme/luxos4.mp3")
                    , (26, "Holme/timecapsule.mp3")
                    , (27, "Holme/plane5.mp3")
                    , (28, "Holme/haunted5.mp3")
                    ]


showVotes :: Map Int ShowVote
showVotes = M.fromList [ (0, ShowVote (VoteText ("Running (for your life or for exercise)", "A HAUNTED HOUSE")))
                       , (1, ShowVote (VoteText ("Making things into other things", "PAPER PLANE FUN BREAKS")))
                       , (2, ShowVote (VoteText ("Strength", "ARM WRESTING TOURNAMENT")))
                       , (3, ShowVote (VoteText ("Dexterity", "DRIVING GAME")))
                       , (4, ShowVote (VoteText ("Facial Hair", "MOUSTACHE GAME")))
                       , (5, ShowVote (VoteText ("Chivalry", "THE UVX MEDIEVAL EXPERIENCE")))
                       , (6, ShowVote (VoteText ("Grit", "THE UVX WESTERN EXPERIENCE")))
                       , (7, ShowVote (VoteText ("Transportation", "THE MTA EXPERIENCE")))
                       , (8, ShowVote (VoteText ("Cats that display aberrant behavior", "MY CAT IS SECRETLY SATAN IN DISGUISE")))
                       , (9, ShowVote (VoteText ("Modern Telecommunications", "PHONES ARE WEIRD, RIGHT?")))
                       , (10, ShowVote (VoteText ("Social interaction with other humans", "THE NUANCES OF HUMAN SMALL TALK ESCAPE ME")))
                       , (11, ShowVote (VoteText ("RES*)89__...#&*[[[*(*&]]]", "")))
                       , (12, ShowVote (VoteText ("CA(*^^...[TODD]%$&^#", "")))
                       , (13, ShowVote (VoteText ("GAZO___&*&(*&^&*", "")))
                       , (14, ShowVote (VoteText ("Update my lexicon!", "LEARN ITALIAN")))
                       , (15, ShowVote (VoteText ("Improve my flesh chassis!", "PHYSICAL FITNESS CHALLENGE")))
                       , (16, ShowVote (VoteText ("Make a backup copy!", "TIME CAPSULE")))
                       ]

-- Run

go = do
  state <- newIORef mempty
  let
    runner = \(t, c) -> run2 state [t] [c]
  newState <- newServerState runner
  state <- newMVar newState
  runner $ renderTDState $ newState ^. tdState
  serve state

loop :: TVar Int -> IO ()
loop count = do
  timer <- newTimer (1000)
  waitTimer timer
  r <- topRunner
  cv <- readTVarIO count
  r $ textT $ str $ show cv
  atomically $ modifyTVar count (+1)
  loop count


renderTDState td@(TDState {_activeVotes, _lastVoteWinner, _voteTimer, _movie, _effects, _soundtracks, _resetMovie, _overlays, _secondaryMovie, _resetSecondary, _overlayVoteScreen, _cueAudioOne, _cueAudioTwo, _resetVoteTimer, _voteQuestion}) =
  (outT $  sidebyside $ (maybeToList (mv _resetSecondary <$> _secondaryMovie)) ++ [maybe (
    compT 31 $ zipWith renderVote [0..] votes
    ++ maybeToList (translate (0, 0.375) . resText <$> msum [(++) "Last vote: " . unpack . snd . voteNames <$> _lastVoteWinner, unpack <$> _voteQuestion])
    ++ maybeToList
      (fmap (translate (0.4, 0.3) . resTexts . caststr . LD.floor)
          $ (!*) . msToF
          <*> ((!+) (float 1) . (!*) (float (-1))) . chopChanName "timer_fraction" . (timerS' ((timerStart .~ _resetVoteTimer))) . msToF
          <$> _voteTimer)) (mv _resetMovie) _overlayVoteScreen
    , compT 31 $
    -- ++
      (mv _resetMovie <$> (if isMainReel then _overlays else []))
        ++ [ (if isMainReel then maybe (mv _resetMovie _movie) (mv _resetMovie) (_altmovie td) else mv _resetMovie _movie) & (foldl (.) id (if isMainReel then _effects else [])) ]
    ]
  , audioDevOut' (audioDevOutVolume ?~ float 1) $
    math' opsadd $
                  [ audioMovie (mv _resetMovie _movie)
                  ] ++
                  (fmap (audioFileIn . str. BS.unpack) $ _soundtracks ++ catMaybes [_cueAudioOne, _cueAudioTwo])
  )
  where
    renderVote idx (voteName, tally) =
      (resText .  (flip (++) $ show tally) . unpack $ voteName)
      & translate (0, (1 - 0.25 * (fromIntegral $ idx) - 0.875))
    msToF = float . (flip (/) 1000.0) . fromIntegral
    mv rmov mf = movieFileIn' ((moviePlayMode ?~ int 0) . ((?~) movieIndex $ casti . mvtimer rmov $ mf)) . str $ (mf ^. movieFile) td
    mvtimer rmov (MovieData{_movieTimeOffset, _movieFile, _movieCycle, _movieLength, _movieId}) = (float $ 60 * _movieTimeOffset) !+ (chopChan0 $ fix (BS.pack $ "custardtimer_" ++ show _movieId ++ "_cust") $ timerS' ((timerCount ?~ int 2) . (timerShowFraction ?~ bool False) . (timerStart .~ rmov) . (timerCycle ?~ bool (_movieCycle)) . (timerCycleLimit ?~ bool (_movieCycle)) . (timerCue .~ rmov)) (float $ _movieLength - _movieTimeOffset))
    isMainReel = _movie ^. movieEffects
    sidebyside = glslTP' (topResolution .~ (Just . casti $ bstr "sum(map(lambda x: x.width, me.inputs))", Just . casti $ bstr "max(map(lambda x: x.height, me.inputs))")) "scripts/sidebyside.glsl" []
    votes =
      case _activeVotes of
        (ShowVotes vs) -> vs & traverse . _1 %~ (fst . voteNames . voteText)
        (FilmVotes vs) -> vs & traverse . _1 %~ (fst . voteNames . voteText . (!) filmVotes)
        NoVotes -> []

resText = resTexts . str
resTexts = textT' (topResolution .~ iv2 (1920, 1080))

translate :: (Float, Float) -> Tree TOP -> Tree TOP
translate xy = transformT' (transformTranslate .~ (Just . float $ fst xy, Just . float $ snd xy))

modifyTDState :: (TDState -> TDState) -> MVar ServerState -> IO ()
modifyTDState f state = do
  s <- takeMVar state
  let s' = s & tdState %~ ((\tdp -> tdp & resetMovie .~ ((s ^. tdState . movie . movieId) /= (tdp ^. movie . movieId))
                  & resetSecondary .~ ((s ^? tdState . secondaryMovie . _Just . movieId) /= (tdp ^? secondaryMovie . _Just . movieId))
                  & resetVoteTimer .~ ((s ^? tdState . voteTimer) /= (tdp ^? voteTimer))) . f)
  putMVar state s'
  s ^. runner $ renderTDState $ s' ^. tdState
  let tdVal g = s' ^. tdState . g
  broadcast (VotesMsg . fmap (fst . voteNames) . activeVoteTexts $ tdVal activeVotes) $ s ^. clients

-- Server

serve :: MVar ServerState -> IO ()
serve state = do
  _ <- async $ WS.runServer "0.0.0.0" 9160 . application $ state
  threadDelay 1000000000

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  jmsg <- WS.receiveData conn
  mstate <- readMVar state
  let mclients = mstate ^. clients
      id = length mclients
      filmVotesList = toList $ M.map (fst . voteNames . voteText) filmVotes
      vcuesList = T.pack <$> M.keys videoCues
      acuesList = T.pack <$> M.keys audioCues
  case decode jmsg of
    (Just (Connecting ps)) ->
      if (traceShowId ps) == (traceShowId $ mstate ^. password) then
        flip finally disconnect $ do
          WS.sendTextData conn (encode . PasswordResult True filmVotesList vcuesList $ acuesList)
          liftIO $ modifyMVar_ state $ pure . (clients %~ ((:) (id, conn)))
          receive conn state (id, conn)
      else
        WS.sendTextData conn (encode $ PasswordResult False [] [] [])
      where
        disconnect = modifyMVar_ state (return . (clients %~ filter ((/= id) . fst)))
    _ -> WS.sendTextData conn ("Nope" :: Text)

receive :: WS.Connection -> MVar ServerState -> Client -> IO ()
receive conn state (id, _) = do
  thr <- async $ forever $ do
    msg <- WS.receiveData conn
    traceShowM msg
    case decode msg of
      (Just (RegisterVote i)) -> modifyTDState (updateVote i) state
      (Just Reset) -> newTDState >>= flip modifyTDState state . const
      (Just (ForceFilmVote v)) -> modifyTDState (forceFilmVote v) state
      (Just (DoFilmVote q vs)) -> startTimer (nextFilmVote q vs)
      (Just (DoShowVote q vs)) -> startTimer (nextShowVote q vs)
      (Just (Connecting ps)) -> putStrLn "Connecting twice?"
      (Just Reel1) -> modifyTDState (changeReel 0) state
      (Just Reboot) -> modifyTDState (changeReel 1) state
      (Just Reel2) -> modifyTDState (changeReel 2) state
      (Just KitchenScene) -> modifyTDState (changeReel 1) state
      (Just (OffsetTime t)) -> modifyTDState (changeTime t) state
  -- Video cues
      (Just PreshowLoop) -> modifyTDState (changeReel 14) state
      (Just ROS2) -> modifyTDState (changeSecondaryReel 15) state
      (Just Calibration) -> modifyTDState (changeReel 16) state
      (Just Countdown) -> modifyTDState (overrideVoteScreen 17 . changeReel 17 . changeSecondaryReel 17) state
      (Just PlaneTracker1) -> modifyTDState (changeSecondaryReel 18) state
      (Just Glitch1) -> modifyTDState (changeReel 19) state
      (Just FilmBreak) -> modifyTDState (changeReel 20 . changeSecondaryReel 20) state
      (Just HackingScene1) -> modifyTDState (changeSecondaryReel 21) state
      (Just HackingScene2) -> modifyTDState (changeSecondaryReel 22) state
      (Just PlaneTracker2) -> modifyTDState (changeSecondaryReel 23) state
      (Just Memoriam) -> modifyTDState (changeSecondaryReel 24) state
      (Just Glitch2) -> modifyTDState (changeSecondaryReel 25) state
      (Just FilmBreak2) -> modifyTDState (changeReel 26 . overrideVoteScreen 26 . changeSecondaryReel 26) state
      (Just OperaLoading) -> modifyTDState (changeReel 27 . changeSecondaryReel 27) state
      (Just Opera) -> modifyTDState (changeReel 28 . overrideVoteScreen 34 . changeSecondaryReel 34) state
      (Just EndingB) -> modifyTDState (changeReel 29 . overrideVoteScreen 34 . changeSecondaryReel 34) state
      (Just EndingC) -> modifyTDState (changeReel 30 . overrideVoteScreen 30 . changeSecondaryReel 30) state
      (Just HauntedHouseEnd) -> modifyTDState (overrideVoteScreen 31) state
      (Just EndCredits) -> modifyTDState (changeReel 32) state
      (Just QA) -> modifyTDState (changeSecondaryReel 33) state
  -- Audio
      (Just ROS1) -> modifyTDState (cueAudio 5) state
      (Just ROS3) -> modifyTDState (cueAudio 6) state
      (Just PaperPlane1) -> modifyTDState (cueAudio 7) state
      (Just HauntedHouse1) -> modifyTDState (cueAudio 8) state
      (Just PaperPlane2) -> modifyTDState (cueAudio 10) state
      (Just HauntedHouse2) -> modifyTDState (cueAudio 11) state
      (Just ROS4) -> modifyTDState (cueAudio 15) state
      (Just PaperPlane3) -> modifyTDState (cueAudio 17) state
      (Just HauntedHouse3) -> modifyTDState (cueAudio 18) state
      (Just ROS5) -> modifyTDState (cueAudio 19) state
      (Just PaperPlane4) -> modifyTDState (cueAudio 20) state
      (Just HauntedHouse4) -> modifyTDState (cueAudio 21) state
      (Just ROS6) -> modifyTDState (cueAudio 25) state
      (Just PaperPlane5) -> modifyTDState (cueAudio 27) state
      (Just HauntedHouse5) -> modifyTDState (cueAudio 28) state
      Nothing -> putStrLn "Unrecognized message"
  wait thr
  where
    startTimer nextVote = do
        let voteLength = 16000
        timer <- newTimer voteLength
        __ <- forkIO $ do
          waitTimer timer
          modifyTDState endVote state
        modifyTDState ((voteTimer ?~ voteLength) . nextVote) state

broadcast :: OutMsg -> [Client] -> IO ()
broadcast msg cs = do
  forM_ cs $ \(_, conn) -> WS.sendTextData conn (encode msg)

-- Votes

updateVote :: Int -> TDState -> TDState
updateVote i td@(TDState { _activeVotes = ShowVotes vs })= td & activeVotes .~ ShowVotes (vs & ix i . _2 %~ (+ 1))
updateVote i td@(TDState { _activeVotes = FilmVotes vs })= td & activeVotes .~ FilmVotes (vs & ix i . _2 %~ (+ 1))
updateVote i td@(TDState { _activeVotes = NoVotes })= td

nextFilmVote :: Text -> [ Int ] -> TDState -> TDState
nextFilmVote q ids td =
  let
    (newVotes, newrs) =
      case rvotes rlist' ((filter (flip member fvp)) ids) of
        (_, [])  -> (NoVotes, td ^. rlist)
        (rlist'', vs) -> (FilmVotes $ (,0) <$> vs, rlist'')
    fvp = td ^. filmVotePool
    rlist' = td ^. rlist
    rvotes rs vs = (drop (length vs) rlist',) . take 3 . nub . catMaybes . snd $ mapAccumL (flip popAt) vs (zipWith rids (backsaw $ length vs) rs)
    rids l r = Prelude.floor $ r * (fromIntegral l + 1)
  in
    td & (activeVotes .~ newVotes) . (rlist .~ newrs) . (overlayVoteScreen .~ Nothing) . (voteQuestion ?~ q) . (lastVoteWinner .~ Nothing)

changeReel :: Int -> TDState -> TDState
changeReel id = movie .~ films ! id

changeSecondaryReel :: Int -> TDState -> TDState
changeSecondaryReel id = secondaryMovie ?~ films ! id

overrideVoteScreen :: Int -> TDState -> TDState
overrideVoteScreen id = overlayVoteScreen ?~ films ! id

changeTime :: Float -> TDState -> TDState
changeTime dt = movie %~ (\(MovieData i f l c e t) -> MovieData i f l c e (t + dt))

cueAudio :: Int -> TDState -> TDState
cueAudio id td@(TDState { _cueAudioSwitch }) =
  case _cueAudioSwitch of
    ATOne -> td & cueAudioOne ?~ audios ! id & cueAudioSwitch .~ ATTwo
    ATTwo -> td & cueAudioTwo ?~ audios ! id & cueAudioSwitch .~ ATOne

backsaw :: Int -> [Int]
backsaw n = [n - 1, n - 2 .. 0]

popAt :: Show a => Int -> [a] -> ([a], Maybe a)
popAt i as = foldr (\(i', a) (as, ma)  -> (if i == i' then (as, Just a) else (a:as, ma))) ([], Nothing) $ zip [0..] as

nextShowVote :: Text -> [ Int ] -> TDState -> TDState
nextShowVote q ids = (activeVotes .~ (ShowVotes . catMaybes $ fmap (, 0)  . flip lookup showVotes <$> ids)) . (overlayVoteScreen .~ Nothing) . (voteQuestion ?~ q) . (lastVoteWinner .~ Nothing)

lookups :: Ord k => Map k v -> [k] -> [v]
lookups m = catMaybes . fmap (flip lookup m)

endVote :: TDState -> TDState
endVote td@(TDState { _activeVotes = ShowVotes vs }) =
  td & (activeVotes .~ NoVotes) .
       (lastVoteWinner ?~ (voteText . maxVote $ vs)) .
       (voteTimer .~ Nothing) .
       (voteQuestion .~ Nothing)


endVote td@(TDState { _activeVotes = FilmVotes vs }) =
  let
    maxVote' = maxVote vs
    maxVote'' = filmVotes ! maxVote'
  in
    td &
      (activeVotes .~ NoVotes) .
      (lastVoteWinner ?~ (voteText maxVote'')) .
      (Lux.run maxVote'') .
      (filmVotePool %~ M.delete maxVote')
endVote td@(TDState { _activeVotes = NoVotes }) = td

forceFilmVote :: Int -> TDState -> TDState
forceFilmVote i td =
  let
    vote = filmVotes ! i
    containsVote = member i (td ^. filmVotePool)
  in
    if containsVote then td & (Lux.run vote) . (filmVotePool %~ M.delete i) else td

maxVote :: [(a, Int)] -> a
maxVote = fst . maximumBy (flip (flip compare . snd) . snd)


-- Timer

waitTimer :: Timer -> IO ()
waitTimer (_, timer) = atomically $ readTMVar timer

stopTimer :: Timer -> IO ()
stopTimer (state, _) = atomically $ writeTVar state Stop

newTimer :: Int -> IO Timer
newTimer n = do
    state <- atomically $ newTVar Start
    timer <- atomically $ newEmptyTMVar
    forkIO $ do
        threadDelay $ 1000 * n
        atomically $ do
            runState <- readTVar state
            case runState of
                Start -> putTMVar timer ()
                Stop  -> return ()
    return (state, timer)
