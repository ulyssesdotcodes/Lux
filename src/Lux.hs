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

import OSCServer

import LambdaDesigner.Op as LD
import LambdaDesigner.JSONOutput as LD
import LambdaDesigner.ParsedOps as LD
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
import Data.Trie hiding (lookup, member)
import Data.List as L hiding (lookup) 
import Data.List.Split as LS
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
              , _tdState :: TDState
              }

data Color = Color { r :: Float, g :: Float, b :: Float } deriving (Generic, Show, Eq)

instance FromJSON Color

data Message = Connecting Text
  | RegisterVote Text Int
  | DoFilmVote
  | StartRun

data OutputState = Tree TOP

instance FromJSON Message where
  parseJSON = withObject "message" $ \o -> do
    ty <- o .: "type"
    case ty of
      "connecting" -> Connecting <$> o .: "localId"
      "vote" -> RegisterVote <$> o .: "localId" <*> o .: "index"
      "doFilmVote" -> return DoFilmVote
      "start" -> return StartRun

data OutMsg = VotesMsg [(Text, Int)]  (Maybe Int)

instance ToJSON OutMsg where
  toJSON (VotesMsg vs vn) = object ["type" A..= "vote", "votes" A..= vs, "votedNum" A..= vn]

type Effect = Tree TOP -> Tree TOP

data CueAudioTrack = ATOne | ATTwo

data TDState = TDState { _activeVotes :: ActiveVotes
                       , _filmVotesList :: [[((Color, Int), Int)]]
                       , _lastVoteWinner :: Maybe Text
                       , _voteTimer :: Maybe Int
                       , _voteAudio :: Maybe BS.ByteString 
                       , _movie :: MovieData
                       , _overlayVoteScreen :: Maybe MovieData
                       , _overlays :: [ MovieData ]
                       , _effects :: [ Effect ]
                       , _soundtrack :: Maybe BS.ByteString
                       , _cueAudioSwitch :: CueAudioTrack
                       , _cueAudioOne :: Maybe BS.ByteString
                       , _cueAudioTwo :: Maybe BS.ByteString
                       , _inCamera :: Int
                       , _resetMovie :: Bool
                       , _resetVoteTimer :: Bool
                       }

data MovieData = MovieData { _movieId :: Int
                           , _movieFile :: TDState -> String
                           , _movieLength :: Float
                           , _movieCycle :: Bool
                           , _movieEffects :: Bool
                           , _movieTimeOffset :: Float
                           }

class Vote a where
  run :: a -> TDState -> TDState
  voteText :: a -> Text

data FilmVote = FilmVote Text FilmData

data FilmData = InCamera Int
              | Effect (Tree TOP -> Tree TOP)
              | Audio Int
              | Overlay Int
 
data ActiveVotes = FilmVotes [ ((Color, Int), Int) ] (Map Text Int)
                 | NoVotes

red = Color 1 0 0
green = Color 0 1 0
blue = Color 0 0 1
orange = Color 1 0.64 0
grey x = Color x x x
rev (Color r g b) = Color (1 - r) (1 - g) (1 - b)
fromColor (Color r g b) = (Just $ float r, Just $ float g, Just $ float b)

makeLenses ''MovieData
makeLenses ''TDState
makeLenses ''ServerState

instance Show TDState where
  show td@(TDState {_activeVotes, _lastVoteWinner, _movie, _effects, _soundtrack}) =
    " lastVoteWinner=" ++ show _lastVoteWinner ++
    " movie" ++ show (_movieFile _movie td) ++ " audioTracks" ++ show _soundtrack

instance Vote FilmVote where
  run (FilmVote _ (InCamera e)) td = td & inCamera +~ e
  run (FilmVote _ (Audio file)) td = td & soundtrack ?~ (audios ! file) 
  run (FilmVote _ (Effect eff)) td = td & effects %~ (eff:)
  run (FilmVote _ (Overlay id)) td = td & overlays %~ ((films ! id):)
  voteText (FilmVote vt _) = vt

data TimerState = Start | Stop
type Timer = (TVar TimerState, TMVar ())

activeVoteTexts :: ActiveVotes -> [ (Text, Int) ]
activeVoteTexts (FilmVotes vs _) = fmap (over _1 (voteText . (!) filmVoteChoices . snd)) vs
activeVoteTexts NoVotes = []

votedIndex :: Text -> ActiveVotes -> Maybe Int
votedIndex lid (FilmVotes _ maplocalid) = lookup lid maplocalid
votedIndex _ NoVotes = Nothing

newServerState :: TOPRunner -> IO ServerState
newServerState tr = newTDState >>= pure . ServerState [] tr

newTDState :: IO TDState
newTDState = pure $
  TDState 
    NoVotes 
    filmVotes
    Nothing 
    Nothing 
    Nothing
    (films ! 0) 
    Nothing
    []
    []
    Nothing
    ATOne
    Nothing
    Nothing
    0
    False
    False

audios :: Map Int BS.ByteString
audios = M.fromList [ (0, "Holme/Audrey.mp3")
                    , (1, "Holme/Western.mp3")
                    , (2, "Holme/Vote.mp3")
                    ]

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

                    

    
glslTP' f name params = glslmultiTOP (f . 
                          (glslmultiTOPpixeldat ?~ (textDAT (textDATfile ?~ str name) [])) .
                          (foldl (.) id $ zipWith3 (\(pn, pv) n v -> (n ?~ str pn) . (v .~ pv)) params 
                                [glslmultiTOPuniname0, glslmultiTOPuniname1, glslmultiTOPuniname2, glslmultiTOPuniname3] 
                                [glslmultiTOPvalue0, glslmultiTOPvalue1, glslmultiTOPvalue2, glslmultiTOPvalue3])
                          )

filmVoteChoices:: Map Int FilmVote
filmVoteChoices= M.fromList $
  [ (0, FilmVote "Square" (Effect $ glslTP' id "scripts/crop.glsl" [("uAspectRatio", emptyV4 & _1 ?~ float 1)] . (:[])))
  , (1, FilmVote "IMAX Aspect" (Effect $ glslTP' id "scripts/crop.glsl" [("uAspectRatio", emptyV4 & _1 ?~ float 1.43)] . (:[])))
  , (2, FilmVote "Bustello" (InCamera 8))
  , (3, FilmVote "Six Foot Orange" (InCamera 2))
  , (4, FilmVote "Russian Subtitles" (Overlay 10))
  , (5, FilmVote "English Subtitles" (Overlay 9))
  , (6, FilmVote "Soundtrack: Audrey Theme" (Audio 0))
  , (7, FilmVote "Western" (Audio 1))
  , (8, FilmVote "Roller skates" (InCamera 1))
  , (9, FilmVote "Director Redux" (InCamera 4))
  , (10, FilmVote "Fish Cam" (Overlay 6))
  , (11, FilmVote "Mocap Man" (Overlay 12))
  , (12, FilmVote "Pelicans" (Overlay 49))
  , (13, FilmVote "Mocap Man" (Overlay 12))
  ]

filmVotes :: [[ ((Color, Int), Int) ]]
filmVotes =  zipWith (\color (id, count) -> ((color, id), count)) [blue, orange] <$> 
  (LS.chunksOf 2 $ (,0) <$> take (length filmVoteChoices) [0..])

-- Run

go = do
  state <- newIORef (mempty :: Messages)
  runner <- OSCServer.topRunner
  newState <- newServerState runner
  state <- newMVar newState
  runner $ renderTDState $ newState ^. tdState
  serve state

renderTDState :: TDState -> (Tree TOP, Tree CHOP)
renderTDState td@(TDState { _activeVotes
                          , _lastVoteWinner
                          , _voteTimer
                          , _voteAudio
                          , _movie
                          , _effects
                          , _soundtrack
                          , _overlays
                          , _overlayVoteScreen
                          , _cueAudioOne
                          , _cueAudioTwo
                          , _resetMovie
                          , _resetVoteTimer}) =
  (outTOP id $
    compositeTOP (compositeTOPoperand ?~ int 31) $ zipWith renderVote [0..] votes
    ++ maybeToList (borderText (grey 1) 0.95 . (++) "Your vote is: " . unpack <$> _lastVoteWinner)
    ++ maybeToList
      (fmap (translate (0, (-0.45)) . resTexts (grey 1) . caststr . LD.floor)
          $ (!*) . msToF
          <*> ((!+) (float 1) . (!*) (float (-1))) . chanNamef "timer_fraction" . 
            (\f -> timerS' ((chopCommands .~ if _resetVoteTimer then [Pulse "start" "1" 2] else []) . 
                            (timerCHOPlength ?~ msToF f)))
          <$> _voteTimer)
    ++ [(compositeTOP (compositeTOPoperand ?~ int 31) $
        (mv' _resetMovie _movie <$> if _movie ^. movieEffects then _overlays ++ [_movie] else [_movie]) ) 
        & (foldl (.) id (if _movie ^. movieEffects then _effects else []))
    -- ++
       ]
  , audiodeviceoutCHOP (audiodeviceoutCHOPvolume ?~ float 1) . (:[]) $
      mathCHOP (mathCHOPchopop ?~ int 1) $
                  ([audiomovieCHOP (audiomovieCHOPmoviefileintop ?~ mv _resetMovie _movie)]) ++
                  (maybeToList $ audnorep . str . BS.unpack <$> _voteAudio) ++
                  (maybeToList $ (\s -> 
                    audiofileinCHOP (audiofileinCHOPfile ?~ (str . BS.unpack $ s))) <$> _soundtrack) ++
                  (fmap (audnorep . str. BS.unpack) $ catMaybes [_cueAudioOne, _cueAudioTwo]) -- use movie for audio timing
  )
  where
    audnorep s = audiofileinCHOP ((audiofileinCHOPrepeat ?~ int 0) . (audiofileinCHOPfile ?~ s))
    renderVote idx (col, tally, voteName) =
      borderText col 0.8 ((flip (++) $ ": " ++ show tally) . unpack $ voteName)
      & translate (0, (1 - 0.2 * (fromIntegral $ idx) - 0.875))
    borderText c w t =
      compositeTOP (compositeTOPoperand ?~ int 31) 
               [ (resText c t)
               ]
    msToF = float . (flip (/) 1000.0) . fromIntegral
    mv rmov mf = mv' rmov mf mf
    mv' rmov mft mf = moviefileinTOP ((moviefileinTOPplaymode ?~ int 0) . 
                                      ((?~) moviefileinTOPindex $ mvtimer rmov $ mft) . 
                                      (moviefileinTOPfile ?~ str ( (^.) mf movieFile td)))
    mvtimer rmov (MovieData{_movieFile, _movieCycle, _movieLength, _movieId}) = 
      (chan0f $ fix (BS.pack $ "custardtimer_" ++ show _movieId ++ "_cust") $ 
      timerS' ((timerCHOPouttimercount ?~ int 2) . 
      (timerCHOPoutfraction ?~ bool False) . 
      (timerCHOPcycle ?~ bool (_movieCycle)) . 
      (timerCHOPcyclelimit ?~ bool (_movieCycle)) . 
      (chopCommands .~ if rmov then [ Pulse "start" "1" 2, Pulse "cuepulse" "1" 1 ] else []) .
      (timerCHOPlength ?~ float (_movieLength))))
    timerS' f = timerCHOP (f . (timerCHOPlengthunits ?~ int 2)) []
    votes =
      case _activeVotes of
        (FilmVotes vs _) -> vs & traverse %~ (\v -> (fst . fst $ v, snd v, voteText . (!) filmVoteChoices . snd . fst $ v))
        NoVotes -> []

resText c = resTexts c . str
resTexts c str = textTOP ((textTOPfontcolor .~ fromColor c) . 
                      (textTOPresolutionw ?~ int 1920) . 
                      (textTOPresolutionh ?~ int 1080) .
                      (textTOPtext ?~ str)) []

translate :: (Float, Float) -> Tree TOP -> Tree TOP
translate xy = transformTOP (transformTOPt .~ (Just . float $ fst xy, Just . float $ snd xy))

modifyTDState :: (TDState -> TDState) -> MVar ServerState -> IO ()
modifyTDState f state = do
  s <- takeMVar state
  let s' = s & tdState %~ ((\tdp -> tdp & resetMovie .~ ((s ^. tdState . movie . movieId) /= (tdp ^. movie . movieId))
             & resetVoteTimer .~ ((s ^? tdState . voteTimer) /= (tdp ^? voteTimer))) . f)
  putMVar state s'
  s ^. runner $ renderTDState $ s' ^. tdState
  let tdVal g = s' ^. tdState . g
  broadcast (VotesMsg (activeVoteTexts $ tdVal activeVotes) Nothing) $ s ^. clients

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
  case decode jmsg of
    (Just (Connecting localId)) ->
      flip finally disconnect $ do
        s <- readMVar state
        let tdVal g = s ^. tdState . g
        WS.sendTextData conn (encode $ VotesMsg (activeVoteTexts $ tdVal activeVotes) (votedIndex localId $ tdVal activeVotes) )
        liftIO $ modifyMVar_ state $ pure . (clients %~ ((:) (id, conn)))
        receive conn state (id, conn)
      where
        disconnect = modifyMVar_ state (return . (clients %~ filter ((/= id) . fst)))
    _ -> WS.sendTextData conn ("Nope" :: Text)

receive :: WS.Connection -> MVar ServerState -> Client -> IO ()
receive conn state (id, _) = do
  thr <- async $ forever $ do
    msg <- WS.receiveData conn
    traceShowM msg
    case decode msg of
      (Just (RegisterVote localId i)) -> modifyTDState (updateVote localId i) state
      (Just (DoFilmVote)) -> startTimer nextFilmVote
      (Just (StartRun)) -> return ()
      (Just (Connecting _)) -> putStrLn "Connecting twice?"
      Nothing -> putStrLn "Unrecognized message"
  wait thr
  where
    startTimer nextVote = do
        let voteLength = 30000
        timer <- newTimer voteLength
        __ <- forkIO $ do
          waitTimer timer
          modifyTDState endVote state
        modifyTDState ((voteTimer ?~ voteLength) . nextVote . (voteAudio ?~ audios ! 2)) state

broadcast :: OutMsg -> [Client] -> IO ()
broadcast msg cs = do
  forM_ cs $ \(_, conn) -> WS.sendTextData conn (encode msg)

-- Votes

updateVote :: Text -> Int -> TDState -> TDState
updateVote lid i td@(TDState { _activeVotes = FilmVotes vs mapv })= 
  if member lid mapv then td else td & activeVotes .~ FilmVotes (vs & ix i . _2 %~ (+ 1)) (M.insert lid i mapv)
updateVote _ _ td@(TDState { _activeVotes = NoVotes })= td

nextFilmVote :: TDState -> TDState
nextFilmVote td =
  td & 
    (activeVotes .~ FilmVotes (head (td ^. filmVotesList)) mempty) . 
    (filmVotesList .~ tail (td ^. filmVotesList)) . 
    (overlayVoteScreen .~ Nothing) . 
    (lastVoteWinner .~ Nothing)

changeReel :: Int -> TDState -> TDState
changeReel id = movie .~ films ! id

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

vec2 :: Tree Float -> Tree Float -> Vec2
vec2 a b = (Just a, Just b)


lookups :: Ord k => Map k v -> [k] -> [v]
lookups m = catMaybes . fmap (flip lookup m)

endVote :: TDState -> TDState
endVote td@(TDState { _activeVotes = NoVotes }) = td
endVote td@(TDState { _activeVotes = FilmVotes vs _}) =
  let
    maxVote' = maxVote $ vs & traverse . _1 %~ (!) filmVoteChoices . snd
    maxVote'' = maxVote'
  in
    td &
      (activeVotes .~ NoVotes) .
      (voteTimer .~ Nothing) .
      (lastVoteWinner ?~ (voteText maxVote'')) .
      (Lux.run maxVote'')

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
