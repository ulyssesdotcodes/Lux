{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Data.Map.Strict as M (Map, toList, fromList, (!), lookup, member, delete, map, keys, insert)
import Data.Matrix (fromList)
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text, unpack)
import GHC.Generics
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

data Color = Color { r :: Float, g :: Float, b :: Float } deriving (Generic, Show, Eq)

instance FromJSON Color

data Message = Connecting Text
  | RegisterVote Int
  | DoShowVote Text [Int] Bool
  | DoFilmVote Text [Int] Bool
  | ForceFilmVote Int
  | Reset
  | KitchenScene
  | Reel1
  | Reel2
  | Reboot
  | OffsetTime Float
-- Cues
  | Blank
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
  | Todd0
  | ToddCats
  | ToddNuts
  | LuxDie
  | Todd Int
  | SV3Western
  | SV3Shootout
-- Audio
  | QA
  | ROS1
  | ROS3
  | PaperPlane1
  | HauntedHouse1
  | PaperPlane2
  | ROS4
  | PaperPlane3
  | ROS5
  | PaperPlane4
  | ROS6
  | PaperPlane5
  | SV2Championship
  | SV3MTA
  | SV3Gunshot
  | SV3Medieval
  | SV4Laser
  | SV4DanceParty
  | SV4TShirt
  | SV6TimeCapsule
  | Car1
  | Car2
  | LuxBoot1
  | LuxBoot2
  | LuxosReset
  | BlankAudio
  -- Triger(movie,lux,etc), GotoTime,

data OutputState = Tree TOP

instance FromJSON Message where
  parseJSON = withObject "message" $ \o -> do
    ty <- o .: "type"
    case ty of
      "connecting" -> Connecting <$> o.: "password"
      "vote" -> RegisterVote <$> o .: "index"
      "doShowVote" -> DoShowVote <$> o .: "question" <*> o .: "votes" <*> o .: "colored"
      "doFilmVote" -> DoFilmVote <$> o .: "question" <*> o .: "votes" <*> o .: "colored"
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
videoCues = M.fromList $
      [ ("preshowloop", PreshowLoop)
      , ("countdown", Countdown)
      -- , ("planetracker1", PlaneTracker1)
      -- , ("glitch1", Glitch1)
      , ("filmbreak", FilmBreak)
      -- , ("hackingscene1", HackingScene1)
      -- , ("hackingscene2", HackingScene2)
      -- , ("planetracker2", PlaneTracker2)
      -- , ("memoriam", Memoriam)
      -- , ("glitch2", Glitch2)
      , ("filmbreak2", FilmBreak2)
      , ("operaloading", OperaLoading)
      -- , ("opera", Opera)
      , ("endingb", EndingB)
      , ("endingc", EndingC)
      -- , ("haunted", HauntedHouseEnd)
      , ("end credits", EndCredits)
      -- , ("qa", QA)
      -- , ("driving.mov", SV2Driving)
      -- , ("show5one.mov", SV5One)
      -- , ("show5two.mov", SV5Two)
      -- , ("show5three.mov", SV5Three)
      -- , ("language.mov", SV6Language)
      -- , ("fleshchassis.mov", SV6FleshChassis)
      -- , ("SV2Championship", SV2Championship)
      -- , ("hauntedhou", HauntedHouse1)
      -- , ("paperplane1", PaperPlane1)
     -- , ("paperplane2", PaperPlane2)
      -- , ("paperplane3", PaperPlane3)
      -- , ("paperplane4", PaperPlane4)
      -- , ("paperplane5", PaperPlane5)
      , ("toddcats", ToddCats)
      , ("toddnuts", ToddNuts)
      , ("luxdie", LuxDie)
      , ("todd0", Todd0)
      , ("sv3 western", SV3Western)
      , ("sv3 shootout", SV3Shootout)
      , ("mrdna", ROS2)
      ] ++ ((\i -> ("todd" ++ show i, Todd i)) <$> [1..32])

audioCues :: Map String Message
audioCues = M.fromList
      -- [ ("ROS1", ROS1)
      -- , ("ROS2", ROS3)
      -- , ("ROS4", ROS4)
      [ ("ROS5", ROS5)
      , ("moustache.mov", SV2Moustache)
      -- , ("SV3MTA", SV3MTA)
      , ("SV3Western", SV3Western)
      -- , ("SV3Gunshot", SV3Gunshot)
      -- , ("SV3Medieval", SV3Medieval)
      -- , ("SV4Laser", SV4Laser)
      -- , ("SV4DanceParty", SV4DanceParty)
      -- , ("SV4TShirt", SV4TShirt)
      -- , ("SV6TimeCapsule", SV6TimeCapsule)
      -- , ("Car1", Car1)
      -- , ("Car2", Car2)
      , ("Luxos Reset", LuxosReset)
      , ("Blank", BlankAudio)
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
                       , _voteAudio :: Maybe BS.ByteString
                       , _movie :: MovieData
                       , _secondaryMovie :: MovieData
                       , _overlayVoteScreen :: Maybe MovieData
                       , _overlays :: [ MovieData ]
                       , _altmovie :: Maybe MovieData
                       , _effects :: [ Effect ]
                       , _soundtrack :: Maybe BS.ByteString
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

data ShowVote = ShowVote { showVoteText :: VoteText, showVoteColor :: Int } 

instance Vote ShowVote where
  run = id . flip const
  voteText (ShowVote vt _) = vt

data FilmVote = FilmVote VoteText FilmData

data FilmData = InCamera Int
              | Effect (Tree TOP -> Tree TOP)
              | Audio Int Int
              | AltCamera Int
              | Overlay Int

data ActiveVotes = ShowVotes [ (ShowVote, (Color, Int))]
                 | FilmVotes [ (Int, (Color, Int))]
                 | NoVotes

red = Color 1 0 0
green = Color 0 1 0
blue = Color 0 0 1
grey x = Color x x x
rev (Color r g b) = Color (1 - r) (1 - g) (1 - b)
fromColor (Color r g b) = (Just $ float r, Just $ float g, Just $ float b)


makeLenses ''MovieData
makeLenses ''TDState
makeLenses ''ServerState

instance Show TDState where
  show td@(TDState {_activeVotes, _filmVotePool, _lastVoteWinner, _movie, _effects, _soundtrack}) =
    " lastVoteWinner=" ++ show _lastVoteWinner ++
    " movie" ++ show (_movieFile _movie td) ++ " audioTracks" ++ show _soundtrack

instance Vote FilmVote where
  run (FilmVote _ (InCamera e)) td = td & inCamera +~ e
  run (FilmVote _ (Audio r file)) td = td & soundtrack ?~ (audios ! file) & filmVotePool %~ if M.member r filmVotes then M.insert r (filmVotes ! r) else id
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
activeVoteTexts (FilmVotes vs) = voteText . (!) filmVotes . fst <$> filter (flip member filmVotes . fst) vs
activeVoteTexts NoVotes = []

newServerState :: Text -> TOPRunner -> IO ServerState
newServerState pass tr = newTDState >>= pure . ServerState [] tr pass

newTDState :: IO TDState
newTDState = newStdGen >>= pure . TDState NoVotes initialVotePool Nothing Nothing Nothing Nothing blankMovieData blankMovieData (Just blankMovieData) [] Nothing [] Nothing ATOne Nothing Nothing 0 True True False . randoms

filmVotes :: Map Int FilmVote
filmVotes = M.fromList [ (1, FilmVote (VoteText ("Six Foot Orange", "Six Foot Orange")) (InCamera 2))
                      , (2, FilmVote (VoteText ("Director Redux", "Director Redux")) (InCamera 4))
                      , (3, FilmVote (VoteText ("Black & White", "Black & White")) (Effect $ glslTP' id "scripts/bandw.glsl" [] . (:[])))
                      , (4, FilmVote (VoteText ("VHS", "VHS")) (Effect $ glslTP' id "scripts/vhs.glsl" [("i_time", emptyV4 & _1 ?~ seconds)] . (:[])))
                      -- , (5, FilmVote (VoteText ("Annoyance", "Annoyance")) (Audio 0))
                      , (6, FilmVote (VoteText ("Square", "Square")) (Effect $ glslTP' id "scripts/crop.glsl" [("uAspectRatio", emptyV4 & _1 ?~ float 1)] . (:[])))
                      , (7, FilmVote (VoteText ("Cinescope", "Cinescope")) (Effect $ glslTP' id "scripts/crop.glsl" [("uAspectRatio", emptyV4 & _1 ?~ float 2.35)] . (:[])))
                      , (8, FilmVote (VoteText ("IMAX Aspect", "IMAX Aspect")) (Effect $ glslTP' id "scripts/crop.glsl" [("uAspectRatio", emptyV4 & _1 ?~ float 1.43)] . (:[])))
                      , (9, FilmVote (VoteText ("Artistic Significance", "Artistic Significance")) (InCamera 8))
                      -- , (10, FilmVote (VoteText ("Webcam", "Webcam")) (Effect $ compT 0 . (vidIn:) . (:[])))
                      , (11, FilmVote (VoteText ("Roller skates", "Roller skates")) (InCamera 1))
                      , (12, FilmVote (VoteText ("Space Opera", "Space Opera")) (InCamera 16))
                      -- , (13, FilmVote (VoteText ("3rd Grader", "3rd Grader")) (AltCamera 5))
                      , (14, FilmVote (VoteText ("Fish Cam", "Fish Cam")) (Overlay 6))
                      -- , (15, FilmVote (VoteText ("Chicken cam", "CC")) (AltCamera 7))
                      -- , (16, FilmVote (VoteText ("Bottle Vision", "Bottle Vision")) (AltCamera 8))
                      , (17, FilmVote (VoteText ("English Subtitles", "English Subtitles")) (Overlay 9))
                      , (18, FilmVote (VoteText ("Russian Subtitles", "Russian Subtitles")) (Overlay 10))
                      , (19, FilmVote (VoteText ("Soundtrack: Smooth Jazz", "Soundtrack: Smooth Jazz")) (Audio 25 1))
                      , (20, FilmVote (VoteText ("Soundtrack: Ragtime", "Soundtrack: Ragtime")) (Audio 26 2))
                      , (21, FilmVote (VoteText ("Soundtrack: Audrey Theme", "Soundtrack: Audrey Theme")) (Audio 27 3))
                      , (22, FilmVote (VoteText ("Pelicans", "Pelicans")) (Overlay 49))
                      , (23, FilmVote (VoteText ("Mocap Man", "Mocap Man")) (Overlay 12))
                      , (24, FilmVote (VoteText ("Colorama", "Colorama")) (Effect $ hsvT' ((hsvAdjSatMult ?~ float 2) . (hsvAdjValMult ?~ float 2))))
                      , (25, FilmVote (VoteText ("Soundtrack: Smooth Jazz Remix", "Soundtrack: Smooth Jazz Remix")) (Audio (-1) 39))
                      , (26, FilmVote (VoteText ("Soundtrack: Ragtime Remix", "Soundtrack: Ragtime Remix")) (Audio (-1) 40))
                      , (27, FilmVote (VoteText ("Soundtrack: Audrey Theme Remix", "Soundtrack: Audrey Theme Remix")) (Audio (-1) 41))
                      , (28, FilmVote (VoteText ("Chinese Subtitles", "Chinese Subtitles")) (Overlay 11))
                      , (29, FilmVote (VoteText ("Director Commentary", "Director Commentary")) (Effect id))
                      ]

initialVotePool :: Map Int FilmVote
initialVotePool = foldr M.delete filmVotes [25, 26, 27]

blankMovieData = MovieData (100) (const "Holme/blank.jpg") 0 False False 0

films :: Map Int MovieData
films = M.fromList $ [ (0, MovieData 0 (printf "Holme/0%05b.mov" . _inCamera) 1357 False True 0)
                   , (1, MovieData 1 (const "Holme/kitchen.mov") 2100 False False 0)
                   , (2, MovieData 2 (printf "Holme/3%05b.mov" . _inCamera) 600 False True 0)
                   , (5, MovieData 5 (const "Holme/third_grader.mov") 1357 False False 0)
                   , (6, MovieData 6 (const "Holme/fish_cam.mov") 1357 True False 0)
                   , (7, MovieData 7 (const "Holme/chicken_cam.mov") 1357 False False 0)
                   , (8, MovieData 8 (const "Holme/bottle_vision.mov") 1357 False False 0)
                   , (9, MovieData 9 (\td -> if td ^. movie . movieId == 2 then "Holme/sub1_2.mov" else "Holme/sub1_1.mov") 1357 False False 0)
                   , (10, MovieData 10 (\td -> if td ^. movie . movieId == 2 then "Holme/sub2_2.mov" else "Holme/sub2_1.mov") 1357 False False 0)
                   , (11, MovieData 11 (\td -> if td ^. movie . movieId == 2 then "Holme/sub3_2.mov" else "Holme/sub3_1.mov") 1357 False False 0)
                   , (12, MovieData 12 (const "Holme/mocap_man.mov") 1357 False False 0)
                   , (13, MovieData 13 (const "Holme/subliminals.mov") 1357 False False 0)
                   , (14, MovieData 14 (const "Holme/preshowloop.mov") 970 True False 0)
                   , (15, MovieData 15 (const "Holme/mrdna.mov") 900 False False 0)
                   , (16, MovieData 16 (const "Holme/calibration.mov") 180 False False 0)
                   , (17, MovieData 17 (const "Holme/countdown.mov") 30 False False 0)
                   , (18, MovieData 18 (const "Holme/tracker1.mov") 240 False False 0)
                   , (19, MovieData 19 (const "Holme/glitch1.mov") 240 False False 0)
                   , (20, MovieData 20 (const "Holme/filmbreak.mov") 300 False False 0)
                   , (21, MovieData 21 (const "Holme/hack1.mov") 120 False False 0)
                   , (22, MovieData 22 (const "Holme/hack2.mov") 240 False False 0)
                   , (23, MovieData 23 (const "Holme/tracker2.mov") 720 False False 0)
                   , (24, MovieData 24 (const "Holme/memoriam.mov") 300 False False 0)
                   , (25, MovieData 25 (const "Holme/glitch2.mov") 240 False False 0)
                   , (26, MovieData 26 (const "Holme/filmbreak2.mov") 120 False False 0)
                   , (27, MovieData 27 (const "Holme/operaload.mov") 360 False False 0)
                   , (28, MovieData 28 (const "Holme/opera.mov") 600 False False 0)
                   , (29, MovieData 29 (const "Holme/resistance.mov") 600 False False 0)
                   , (30, MovieData 30 (const "Holme/todd0.mov") 60 True False 0)
                   , (31, MovieData 31 (const "Holme/hauntedend.mov") 120 False False 0)
                   , (32, MovieData 32 (const "Holme/endcredits.mov") 900 False False 0)
                   , (33, MovieData 33 (const "Holme/qa.mov") 900 False False 0)
                   , (34, MovieData 34 (const "Holme/supertitles.mov") 600 False False 0)
                   , (35, MovieData 35 (const "Holme/driving.mov") 300 False False 0)
                   , (37, MovieData 37 (const "Holme/show5one.mov") 300 False False 0)
                   , (38, MovieData 38 (const "Holme/show5two.mov") 300 False False 0)
                   , (39, MovieData 39 (const "Holme/show5three.mov") 300 False False 0)
                   , (40, MovieData 40 (const "Holme/language.mov") 300 False False 0)
                   , (42, MovieData 42 (const "Holme/championship.mov") 300 False False 0)
                   , (43, MovieData 43 (const "Holme/haunted1.mov") 300 False False 0)
                   , (44, MovieData 44 (const "Holme/plane1.mov") 300 False False 0)
                   , (45, MovieData 45 (const "Holme/plane2.mov") 300 False False 0)
                   , (46, MovieData 46 (const "Holme/plane3.mov") 300 False False 0)
                   , (47, MovieData 47 (const "Holme/plane4.mov") 300 False False 0)
                   , (48, MovieData 48 (const "Holme/plane5.mov") 300 False False 0)
                   , (49, MovieData 49 (const "Holme/pelicans.mov") 600 False False 0)
                   , (50, MovieData 50 (const "Holme/toddcats.mov") 120 True False 0)
                   , (51, MovieData 51 (const "Holme/toddnuts.mov") 120 False False 0)
                   , (52, MovieData 52 (const "Holme/luxdie.mov") 120 False False 0)
                   , (84, MovieData 84 (const "Holme/SV3Western.mov") 120 True False 0)
                   , (85, MovieData 85 (const "Holme/SV3Shootout.mov") 120 False False 0)
                   ] ++ ((\i -> (52 + i, MovieData (52 + i) (const $ "Holme/todd" ++ (show i) ++ ".mov") 30 False False 0)) <$> [1..32])

audios :: Map Int BS.ByteString
audios = M.fromList [ (0, "Holme/TAKEONMEUCC.mp3")
                    , (1, "Holme/soundtrack_1.mp3")
                    , (2, "Holme/soundtrack_2.mp3")
                    , (3, "Holme/soundtrack_3.mp3")
                    , (5, "Holme/luxos1.mp3")
                    , (6, "Holme/buddymusical.mp3")
                    , (12, "Holme/mta.mp3")
                    , (14, "Holme/Medieval.mp3")
                    , (15, "Holme/luxos2.mp3")
                    , (16, "Holme/luxoscomedy.mp3")
                    , (19, "Holme/luxos3.mp3")
                    , (22, "Holme/laser.mp3")
                    , (23, "Holme/danceparty.mp3")
                    , (24, "Holme/tshirt.mp3")
                    , (25, "Holme/luxos4.mp3")
                    , (26, "Holme/timecapsule.mp3")
                    , (29, "Holme/voteaudio.mp3")
                    , (30, "Holme/moustache.mp3")
                    , (31, "Holme/car1.mp3")
                    , (32, "Holme/car2.mp3")
                    , (33, "Holme/luxboot1.mp3")
                    , (34, "Holme/luxboot2.mp3")
                    , (35, "Holme/luxosreset.mp3")
                    , (36, "Holme/fleshchassis.mp3")
                    , (37, "Holme/gunshot.mp3")
                    , (38, "")
                    , (39, "Holme/soundtrack_1_remix.mp3")
                    , (40, "Holme/soundtrack_2_remix.mp3")
                    , (41, "Holme/soundtrack_3_remix.mp3")
                    ]


showVotes :: Map Int ShowVote
showVotes = M.fromList [ (0, ShowVote (VoteText ("Running (for your life or for exercise)", "A HAUNTED HOUSE")) 0)
                       , (1, ShowVote (VoteText ("Making things into other things", "PAPER PLANE FUN BREAKS")) 0)
                       , (2, ShowVote (VoteText ("Strength", "ARM WRESTLING TOURNAMENT")) 0)
                       , (3, ShowVote (VoteText ("Dexterity", "DRIVING GAME")) 0)
                       , (4, ShowVote (VoteText ("Facial Hair", "MOUSTACHE GAME")) 0)
                       , (5, ShowVote (VoteText ("Chivalry", "THE UVX MEDIEVAL EXPERIENCE")) 0)
                       , (6, ShowVote (VoteText ("Grit", "THE UVX WESTERN EXPERIENCE")) 0)
                       , (7, ShowVote (VoteText ("Transportation", "THE MTA EXPERIENCE")) 0)
                       , (8, ShowVote (VoteText ("Cats that display aberrant behavior", "MY CAT IS SECRETLY SATAN IN DISGUISE")) 0)
                       , (9, ShowVote (VoteText ("Modern Telecommunications", "PHONES ARE WEIRD, RIGHT?")) 0)
                       , (10, ShowVote (VoteText ("Social interaction with other humans", "THE NUANCES OF HUMAN SMALL TALK ESCAPE ME")) 0)
                       , (11, ShowVote (VoteText ("RES*)89__...#&*[[[*(*&]]]", "")) 1)
                       , (12, ShowVote (VoteText ("CA(*^^...[TODD]%$&^#", "")) 2)
                       , (13, ShowVote (VoteText ("GAZO___&*&(*&^&*", "")) 3)
                       , (14, ShowVote (VoteText ("Update my lexicon!", "LEARN ITALIAN")) 1)
                       , (15, ShowVote (VoteText ("Improve my flesh chassis!", "PHYSICAL FITNESS CHALLENGE")) 2)
                       , (16, ShowVote (VoteText ("Make a backup copy!", "TIME CAPSULE")) 3)
                       , (17, ShowVote (VoteText ("Lasershow", "Lasershow")) 0)
                       , (18, ShowVote (VoteText ("T-Shirt", "T-Shirt")) 0)
                       , (19, ShowVote (VoteText ("Dance party", "Dance party")) 0)
                       ]

-- Run

go = do
  state <- newIORef mempty
  let
    runner = \(t, c) -> run2 state [t] [c]
  -- putStrLn "Enter password: "
  -- pass <- T.pack <$> getLine
  newState <- newServerState "password" runner
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


renderTDState td@(TDState {_activeVotes, _lastVoteWinner, _voteTimer, _movie, _effects, _soundtrack, _resetMovie, _overlays, _secondaryMovie, _resetSecondary, _overlayVoteScreen, _cueAudioOne, _cueAudioTwo, _resetVoteTimer, _voteQuestion, _voteAudio, _altmovie}) =
  (outT $  sidebyside $
   --([mv _resetSecondary _secondaryMovie & transformT' (transformScale .~ (Just $ float 0.667, Just $ float 0.667))]) ++
    [maybe (
    compT 31 $ zipWith renderVote [0..] votes
    ++ (if length votes == 0 && isNothing _lastVoteWinner then [mv False blankMovieData] else [])
    ++ maybeToList (translate (0, 0.375) . borderText (grey 0) 0.95 <$> unpack <$> _voteQuestion)
    ++ maybeToList (borderText (grey 1) 0.95 . (++) "Your vote is: " . unpack . snd . voteNames <$> _lastVoteWinner)
    ++ maybeToList
      (fmap (translate (0, (-0.45)) . resTexts (grey 1) . caststr . LD.floor)
          $ (!*) . msToF
          <*> ((!+) (float 1) . (!*) (float (-1))) . chopChanName "timer_fraction" . (timerS' ((timerStart .~ _resetVoteTimer))) . msToF
          <$> _voteTimer)) (mv _resetMovie) _overlayVoteScreen
    , ( compT 31 $
        (mv' _resetMovie _movie <$> if _movie ^. movieEffects then maybe (_overlays ++ [_movie]) (:[]) _altmovie else [_movie]) ) & (foldl (.) id (if _movie ^. movieEffects then _effects else []))
    -- ++
    ]
  , audioDevOut' (audioDevOutVolume ?~ float 1) $
    math' opsadd $
                  ([audioMovie $ mv _resetMovie _movie]) ++
                  (maybeToList $ audnorep . str . BS.unpack <$> _voteAudio) ++
                  (if isMainReel _movie then maybeToList $ audioFileIn . str. BS.unpack <$> _soundtrack else [])++
                  (fmap (audnorep . str. BS.unpack) $ catMaybes [_cueAudioOne, _cueAudioTwo]) -- use movie for audio timing
  )
  where
    audnorep = audioFileIn' (audioFileRepeat ?~ int 0)
    renderVote idx ((col, tally), voteName) =
      borderText col 0.8 ((flip (++) $ ": " ++ show tally) . unpack $ voteName)
      & translate (0, (1 - 0.2 * (fromIntegral $ idx) - 0.875))
    borderText c w t =
      compT 31 [ (resText (rev c) t)
               , rectangle' ((rectangleBorderColor .~ fromColor (rev c)) . (rectangleColor .~ fromColor c) . (rectangleBorderWidth ?~ float 0.01) . (topResolution .~ iv2 (1920, 1080))) (Just $ float w, Just $ float 0.075)
               ]
    msToF = float . (flip (/) 1000.0) . fromIntegral
    mv rmov mf = mv' rmov mf mf
    mv' rmov mft mf = movieFileIn' ((moviePlayMode ?~ int 0) . ((?~) movieIndex $ casti . mvtimer rmov $ mft)) . str $ (mf ^. movieFile) td
    mvtimer rmov (MovieData{_movieTimeOffset, _movieFile, _movieCycle, _movieLength, _movieId}) = (float $ 60 * _movieTimeOffset) !+ (chopChan0 $ fix (BS.pack $ "custardtimer_" ++ show _movieId ++ "_cust") $ timerS' ((timerCount ?~ int 2) . (timerShowFraction ?~ bool False) . (timerStart .~ rmov) . (timerCycle ?~ bool (_movieCycle)) . (timerCycleLimit ?~ bool (_movieCycle)) . (timerCue .~ rmov)) (float $ _movieLength - _movieTimeOffset))
    isMainReel mov = mov ^. movieEffects
    sidebyside = glslTP' (topResolution .~ (Just . casti $ bstr "sum(map(lambda x: x.width, me.inputs))", Just . casti $ bstr "max(map(lambda x: x.height, me.inputs))")) "scripts/sidebyside.glsl" []
    votes =
      case _activeVotes of
        (ShowVotes vs) -> vs & traverse %~ (\v -> (snd $ v, fst . voteNames . voteText . fst $ v))
        (FilmVotes vs) -> vs & traverse %~ (\v -> (snd $ v, fst . voteNames . voteText . (!) filmVotes . fst $ v))
        NoVotes -> []

resText c = resTexts c . str
resTexts c = textT' ((textColor .~ fromColor c) . (topResolution .~ iv2 (1920, 1080)))

translate :: (Float, Float) -> Tree TOP -> Tree TOP
translate xy = transformT' (transformTranslate .~ (Just . float $ fst xy, Just . float $ snd xy))

modifyTDState :: (TDState -> TDState) -> MVar ServerState -> IO ()
modifyTDState f state = do
  s <- takeMVar state
  let s' = s & tdState %~ ((\tdp -> tdp & resetMovie .~ ((s ^. tdState . movie . movieId) /= (tdp ^. movie . movieId))
             & resetSecondary .~ ((s ^. tdState . secondaryMovie .  movieId) /= (tdp ^. secondaryMovie . movieId))
             & resetVoteTimer .~ ((s ^? tdState . voteTimer) /= (tdp ^? voteTimer))) . f)
  putMVar state s'
  s ^. runner $ renderTDState $ s' ^. tdState
  let tdVal g = s' ^. tdState . g
  broadcast (VotesMsg . fmap (fst . voteNames) . activeVoteTexts $ tdVal activeVotes) $ s ^. clients

-- Server

serve :: MVar ServerState -> IO ()
serve state = do
  _ <- async $ WS.runServer "0.0.0.0" 9160 . application $ state
  threadDelay 86400000000

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
          s <- readMVar state
          let tdVal g = s ^. tdState . g
          WS.sendTextData conn (encode $ VotesMsg . fmap (fst . voteNames) . activeVoteTexts $ tdVal activeVotes)
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
      (Just (DoFilmVote q vs c)) -> startTimer (nextFilmVote q vs c)
      (Just (DoShowVote q vs c)) -> startTimer (nextShowVote q vs c)
      (Just (Connecting ps)) -> putStrLn "Connecting twice?"
      (Just Reel1) -> modifyTDState (changeReel 0) state
      (Just Reboot) -> modifyTDState (changeReel 1) state
      (Just Reel2) -> modifyTDState (changeReel 2) state
      (Just KitchenScene) -> modifyTDState (changeReel 1) state
      (Just (OffsetTime t)) -> modifyTDState (changeTime t) state
      (Just Blank) -> modifyTDState ((secondaryMovie .~ blankMovieData) . (movie .~ blankMovieData)) state
  -- Video cues
      (Just PreshowLoop) -> modifyTDState (changeReel 14) state
      (Just ROS2) -> modifyTDState (changeReel 15) state
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
      (Just OperaLoading) -> modifyTDState (changeReel 27 . overrideVoteScreen 27 . changeSecondaryReel 27) state
      (Just Opera) -> modifyTDState (changeReel 28 . overrideVoteScreen 28 . changeSecondaryReel 28) state
      (Just EndingB) -> modifyTDState (changeReel 29 . overrideVoteScreen 29 . changeSecondaryReel 29) state
      (Just EndingC) -> modifyTDState (changeReel 30 . overrideVoteScreen 30 . changeSecondaryReel 30) state
      (Just HauntedHouseEnd) -> modifyTDState (overrideVoteScreen 31) state
      (Just EndCredits) -> modifyTDState (changeReel 32) state
      (Just QA) -> modifyTDState (changeSecondaryReel 33) state
      (Just SV2Driving) -> modifyTDState (changeSecondaryReel 35) state
      (Just SV5One) -> modifyTDState (changeSecondaryReel 37) state
      (Just SV5Two) -> modifyTDState (changeSecondaryReel 38) state
      (Just SV5Three) -> modifyTDState (changeSecondaryReel 39) state
      (Just SV6Language) -> modifyTDState (changeSecondaryReel 40) state
      (Just SV2Championship) -> modifyTDState (changeSecondaryReel 42) state
      (Just HauntedHouse1) -> modifyTDState (changeReel 43) state
      (Just PaperPlane1) -> modifyTDState (changeReel 44) state
      (Just PaperPlane2) -> modifyTDState (changeReel 45) state
      (Just PaperPlane3) -> modifyTDState (changeReel 46) state
      (Just PaperPlane4) -> modifyTDState (changeReel 47) state
      (Just PaperPlane5) -> modifyTDState (changeReel 48) state
      (Just Todd0) -> modifyTDState (changeReel 30) state
      (Just (Todd i)) -> modifyTDState (changeReel $ 52 + i) state
      (Just ToddCats) -> modifyTDState (changeSecondaryReel 50) state
      (Just ToddNuts) -> modifyTDState (changeSecondaryReel 51) state
      (Just LuxDie) -> modifyTDState (changeSecondaryReel 52) state
      (Just SV3Western) -> modifyTDState (changeSecondaryReel 84) state
      (Just SV3Shootout) -> modifyTDState (changeSecondaryReel 85) state
  -- Audio
      (Just ROS1) -> modifyTDState (cueAudio 5) state
      (Just ROS3) -> modifyTDState (cueAudio 6) state
      (Just ROS4) -> modifyTDState (cueAudio 15) state
      (Just ROS5) -> modifyTDState (cueAudio 19) state
      (Just ROS6) -> modifyTDState (cueAudio 25) state
      (Just SV3MTA) -> modifyTDState (cueAudio 12) state
      (Just SV3Gunshot) -> modifyTDState (cueAudio 37) state
      (Just SV3Medieval) -> modifyTDState (cueAudio 14) state
      (Just SV4Laser) -> modifyTDState (cueAudio 22) state
      (Just SV4DanceParty) -> modifyTDState (cueAudio 23) state
      (Just SV4TShirt) -> modifyTDState (cueAudio 24) state
      (Just SV6TimeCapsule) -> modifyTDState (cueAudio 26) state
      (Just SV6FleshChassis) -> modifyTDState (cueAudio 36) state
      (Just SV2Moustache) -> modifyTDState (cueAudio 30) state
      (Just Car1) -> modifyTDState (cueAudio 31) state
      (Just Car2) -> modifyTDState (cueAudio 32) state
      (Just LuxBoot1) -> modifyTDState (cueAudio 33) state
      (Just LuxBoot2) -> modifyTDState (cueAudio 34) state
      (Just LuxosReset) -> modifyTDState (cueAudio 35) state
      (Just BlankAudio) -> modifyTDState (cueAudio 38) state
      Nothing -> putStrLn "Unrecognized message"
  wait thr
  where
    startTimer nextVote = do
        let voteLength = 30000
        timer <- newTimer voteLength
        __ <- forkIO $ do
          waitTimer timer
          modifyTDState endVote state
        modifyTDState ((voteTimer ?~ voteLength) . nextVote . (voteAudio ?~ audios ! 29)) state

broadcast :: OutMsg -> [Client] -> IO ()
broadcast msg cs = do
  forM_ cs $ \(_, conn) -> WS.sendTextData conn (encode msg)

-- Votes

updateVote :: Int -> TDState -> TDState
updateVote i td@(TDState { _activeVotes = ShowVotes vs })= td & activeVotes .~ ShowVotes (vs & ix i . _2 . _2 %~ (+ 1))
updateVote i td@(TDState { _activeVotes = FilmVotes vs })= td & activeVotes .~ FilmVotes (vs & ix i . _2 . _2 %~ (+ 1))
updateVote i td@(TDState { _activeVotes = NoVotes })= td

colFromIdx 0 = red
colFromIdx 1 = green
colFromIdx 2 = blue
colFromIdx _ = grey 1


nextFilmVote :: Text -> [ Int ] -> Bool -> TDState -> TDState
nextFilmVote _ vs b td =
  let
    (newVotes, newrs) =
      case rvotes rlist' (filter (flip member fvp) vs) of
        (_, [])  -> (NoVotes, td ^. rlist)
        (rlist'', vs) -> (FilmVotes $ zipWith (\i v -> (v,(if b then colFromIdx i else grey 0, 0))) [0..] vs, rlist'')
    fvp = td ^. filmVotePool
    rlist' = td ^. rlist
    rvotes rs vs = (drop (length vs) rlist',) . take 3 . nub . catMaybes . snd $ mapAccumL (flip popAt) vs (zipWith rids (backsaw $ length vs) rs)
    rids l r = Prelude.floor $ r * (fromIntegral l + 1)
  in
    td & (activeVotes .~ newVotes) . (rlist .~ newrs) . (overlayVoteScreen .~ Nothing) . (voteQuestion .~ Nothing) . (lastVoteWinner .~ Nothing)

changeReel :: Int -> TDState -> TDState
changeReel id = movie .~ films ! id

changeSecondaryReel :: Int -> TDState -> TDState
changeSecondaryReel id = secondaryMovie .~ films ! id

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

nextShowVote :: Text -> [ Int ] -> Bool -> TDState -> TDState
nextShowVote q vs col =
  (activeVotes .~ (ShowVotes $ zipWith (\i v -> (, (if col then colFromIdx i else grey 0, 0)) $ showVotes ! v) [0..] $ filter (flip member showVotes) vs)) .
  (overlayVoteScreen .~ Nothing) .
  (voteQuestion ?~ q) .
  (lastVoteWinner .~ Nothing)

lookups :: Ord k => Map k v -> [k] -> [v]
lookups m = catMaybes . fmap (flip lookup m)

endVote :: TDState -> TDState
endVote td@(TDState { _activeVotes = ShowVotes vs }) =
  td & (activeVotes .~ NoVotes) .
       (lastVoteWinner ?~ (voteText . maxVote $ (\(i, (c, t)) -> (i, t)) <$> vs)) .
       (voteTimer .~ Nothing) .
       (voteQuestion .~ Nothing) .
       (voteAudio .~ Nothing)



endVote td@(TDState { _activeVotes = FilmVotes vs }) =
  let
    maxVote' = maxVote $ (\(i, (c, t)) -> (i, t))<$> vs
    maxVote'' = filmVotes ! maxVote'
  in
    td &
      (activeVotes .~ NoVotes) .
      (voteTimer .~ Nothing) .
      (lastVoteWinner ?~ (voteText maxVote'')) .
      (Lux.run maxVote'') .
      (filmVotePool %~ M.delete maxVote') .
      (voteAudio .~ Nothing)
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
