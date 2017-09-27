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
import Data.Map.Strict as M (Map, fromList, (!), lookup, member, delete)
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
  | DoShowVote [Int]
  | DoFilmVote [Int]
  | Reset
  | KitchenScene
  | MainReel
  | Opera Int
  | OffsetTime Float
  -- Triger(movie,lux,etc), GotoTime,

data OutputState = Tree TOP

instance FromJSON Message where
  parseJSON = withObject "message" $ \o -> do
    ty <- o .: "type"
    case ty of
      "connecting" -> Connecting <$> o.: "password"
      "vote" -> RegisterVote <$> o .: "index"
      "doShowVote" -> DoShowVote <$> o .: "votes"
      "doFilmVote" -> DoFilmVote <$> o .: "votes"
      "offsetTime" -> OffsetTime <$> o .: "timeOffset"
      "kitchenScene" -> return KitchenScene
      "mainReel" -> return MainReel
      "opera" -> Opera <$> o .: "version"
      "reset" -> return Reset
      _ -> fail ("Unknown type " ++ ty)

data OutMsg = VotesMsg [Text] | PasswordResult Bool

instance ToJSON OutMsg where
  toJSON (VotesMsg vs) = object ["type" A..= "vote", "votes" A..= vs]
  toJSON (PasswordResult b) = object ["type" A..= "password", "success" A..= b]

type Effect = Tree TOP -> Tree TOP

data TDState = TDState { _activeVotes :: ActiveVotes
                       , _filmVotePool :: Map Int FilmVote
                       , _lastVoteWinner :: Maybe VoteText
                       , _voteTimer :: Maybe Int
                       , _movie :: MovieData
                       , _altmovie :: Maybe MovieData
                       , _effects :: [ Effect ]
                       , _audioTrack :: Maybe BS.ByteString
                       , _inCamera :: Int
                       , _resetMovie :: Bool
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

data ShowVote = ShowVote VoteText

instance Vote ShowVote where
  run = id . flip const
  voteText (ShowVote vt) = vt

data FilmVote = FilmVote VoteText FilmData

data FilmData = InCamera Int
              | Effect (Tree TOP -> Tree TOP)
              | Audio Int
              | AltCamera Int

data ActiveVotes = ShowVotes [ (ShowVote, Int) ]
                 | FilmVotes [ (Int, Int) ]
                 | NoVotes


makeLenses ''MovieData
makeLenses ''TDState
makeLenses ''ServerState

instance Show TDState where
  show td@(TDState {_activeVotes, _filmVotePool, _lastVoteWinner, _movie, _effects, _audioTrack}) =
    "activeVotes=" ++ (show $ activeVoteTexts _activeVotes) ++ " lastVoteWinner=" ++ show _lastVoteWinner ++
    " movie" ++ show (_movieFile _movie td) ++ " audioTrack" ++ show _audioTrack

instance Vote FilmVote where
  run (FilmVote _ (InCamera e)) td = td & inCamera +~ e
  run (FilmVote _ (Audio file)) td = td & audioTrack ?~ audios ! file
  run (FilmVote _ (Effect eff)) td = td & effects %~ (eff:)
  run (FilmVote _ (AltCamera id)) td = td & altmovie ?~ (films ! id)
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
newTDState = newStdGen >>= pure . TDState NoVotes filmVotes Nothing Nothing (films ! 0) Nothing [] Nothing 0 False . randoms

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
                      , (13, FilmVote (VoteText ("3rd Grader", "3G")) (AltCamera 6))
                      , (14, FilmVote (VoteText ("Fish cam", "FC")) (AltCamera 7))
                      , (15, FilmVote (VoteText ("Chicken cam", "CC")) (AltCamera 8))
                      , (16, FilmVote (VoteText ("Bottle Vision", "BV")) (AltCamera 5))
                      ]

films :: Map Int MovieData
films = M.fromList [ (0, MovieData 0 (printf "Holme/%05b.mov" . _inCamera) 1357 False True 0)
                   , (1, MovieData 1 (const "Holme/kitchen_scene.mov") 600 False False 0)
                   , (2, MovieData 2 (const "Holme/opera_1.mov") 600 False False 0)
                   , (3, MovieData 3 (const "Holme/opera_2.mov") 600 False False 0)
                   , (4, MovieData 4 (const "Holme/opera_3.mov") 600 False False 0)
                   , (5, MovieData 5 (const "Holme/kitchen_scene.mov") 1357 False False 0)
                   , (6, MovieData 6 (const "Holme/kitchen_scene.mov") 1357 False False 0)
                   , (7, MovieData 7 (const "Holme/kitchen_scene.mov") 1357 False False 0)
                   , (8, MovieData 8 (const "Holme/kitchen_scene.mov") 1357 False False 0)
                   ]

audios :: Map Int BS.ByteString
audios = M.fromList [ (0, "Holme/TAKEONMEUCC.mp3")
                    ]

showVotes :: Map Int ShowVote
showVotes = M.fromList [ (0, ShowVote $ VoteText ("Basic", "B"))
                       , (1, ShowVote $ VoteText ("Test", "T"))
                       , (2, ShowVote $ VoteText ("Hi", "H"))
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


renderTDState td@(TDState {_activeVotes, _lastVoteWinner, _voteTimer, _movie, _effects, _audioTrack, _resetMovie}) =
  (outT $ compT 0
  $ zipWith renderVote [0..] votes
  ++ maybeToList (resText . (++) "Last vote: " . unpack . snd . voteNames <$> _lastVoteWinner)
  ++ maybeToList (fmap (resTexts . caststr . LD.floor)
                  $ (!*) . msToF
                  <*> ((!+) (float 1) . (!*) (float (-1))) . chopChanName "timer_fraction" . (timerS' (timerStart .~ _resetMovie)) . msToF
                  <$> _voteTimer)
  ++ [ maybe (mv _movie) mv (_altmovie td) & (foldl (.) id _effects) ]
  , audioDevOut' (audioDevOutVolume ?~ float 0.3) $
    math' opsadd $
                  [ audioMovie (mv _movie)
                  ] ++ maybeToList (audioFileIn . str . BS.unpack <$> _audioTrack)
  )
  where
    renderVote idx (voteName, tally) =
      (resText .  (flip (++) $ show tally) . unpack $ voteName)
      & transformT' (transformTranslate .~ (Nothing, Just . float $ (1 - 0.33 * (fromIntegral $ idx) - 0.66)))
    msToF = float . (flip (/) 1000.0) . fromIntegral
    mv mf = movieFileIn' ((moviePlayMode ?~ int 0) . ((?~) movieIndex $ casti . mvtimer $ mf)) . str $ (mf ^. movieFile) td
    mvtimer (MovieData{_movieTimeOffset, _movieFile, _movieCycle, _movieLength}) = (float $ 60 * _movieTimeOffset) !+ (chopChan0 $ timerS' ((timerCount ?~ int 2) . (timerShowFraction ?~ bool False) . (timerStart .~ _resetMovie) . (timerCycle ?~ bool (_movieCycle)) . (timerCycleLimit ?~ bool (_movieCycle)) . (timerCue .~ (traceShowId $ _resetMovie))) (float $ _movieLength - _movieTimeOffset))
    votes =
      case _activeVotes of
        (ShowVotes vs) -> vs & traverse . _1 %~ (fst . voteNames . voteText)
        (FilmVotes vs) -> vs & traverse . _1 %~ (fst . voteNames . voteText . (!) filmVotes)
        NoVotes -> []

resText = resTexts . str
resTexts = textT' (topResolution .~ iv2 (1920, 1080))

modifyTDState :: (TDState -> TDState) -> MVar ServerState -> IO ()
modifyTDState f state = do
  s <- takeMVar state
  let s' = s & tdState %~ ((\tdp -> tdp & resetMovie .~ ((s ^. tdState . movie . movieId) /= (tdp ^. movie . movieId))) . f)
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
  case decode jmsg of
    (Just (Connecting ps)) ->
      if (traceShowId ps) == (traceShowId $ mstate ^. password) then
        flip finally disconnect $ do
          WS.sendTextData conn (encode $ PasswordResult True)
          liftIO $ modifyMVar_ state $ pure . (clients %~ ((:) (id, conn)))
          receive conn state (id, conn)
      else
        WS.sendTextData conn (encode $ PasswordResult False)
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
      (Just (DoFilmVote vs)) -> startTimer (nextFilmVote vs)
      (Just (DoShowVote vs)) -> startTimer (nextShowVote vs)
      (Just (Connecting ps)) -> putStrLn "Connecting twice?"
      (Just MainReel) -> modifyTDState (changeReel 0) state
      (Just KitchenScene) -> modifyTDState (changeReel 1) state
      (Just (Opera i)) -> modifyTDState (changeReel (i + 2)) state
      (Just (OffsetTime t)) -> modifyTDState (changeTime t) state
      Nothing -> putStrLn "Unrecognized message"
  wait thr
  where
    startTimer nextVote = do
        let voteLength = 4000
        timer <- newTimer voteLength
        __ <- forkIO $ do
          waitTimer timer
          modifyTDState endVote state
        modifyTDState nextVote state

broadcast :: OutMsg -> [Client] -> IO ()
broadcast msg cs = do
  forM_ cs $ \(_, conn) -> WS.sendTextData conn (encode msg)

-- Votes

updateVote :: Int -> TDState -> TDState
updateVote i td@(TDState { _activeVotes = ShowVotes vs })= td & activeVotes .~ ShowVotes (vs & ix i . _2 %~ (+ 1))
updateVote i td@(TDState { _activeVotes = FilmVotes vs })= td & activeVotes .~ FilmVotes (vs & ix i . _2 %~ (+ 1))
updateVote i td@(TDState { _activeVotes = NoVotes })= td

nextFilmVote :: [ Int ] -> TDState -> TDState
nextFilmVote ids td =
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
    td & (activeVotes .~ newVotes) . (rlist .~ newrs)

changeReel :: Int -> TDState -> TDState
changeReel id = movie .~ films ! id

changeTime :: Float -> TDState -> TDState
changeTime dt = movie %~ (\(MovieData i f l c e t) -> MovieData i f l c e (t + dt))

backsaw :: Int -> [Int]
backsaw n = [n - 1, n - 2 .. 0]

popAt :: Show a => Int -> [a] -> ([a], Maybe a)
popAt i as = foldr (\(i', a) (as, ma)  -> (if i == i' then (as, Just a) else (a:as, ma))) ([], Nothing) $ zip [0..] as

nextShowVote :: [ Int ] -> TDState -> TDState
nextShowVote ids = set activeVotes (ShowVotes . catMaybes $ fmap (, 0)  . flip lookup showVotes <$> ids)

lookups :: Ord k => Map k v -> [k] -> [v]
lookups m = catMaybes . fmap (flip lookup m)

endVote :: TDState -> TDState
endVote td@(TDState { _activeVotes = ShowVotes vs }) = td & (activeVotes .~ NoVotes) . (lastVoteWinner ?~ (voteText . maxVote $ vs))
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
