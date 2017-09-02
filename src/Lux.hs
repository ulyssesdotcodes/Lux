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


import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T

type Client = (Int, WS.Connection)

type TOPRunner = ((Tree TOP, Tree CHOP) -> IO ())
data ServerState =
  ServerState { _clients :: [Client]
              , _tdState :: TDState
              , _runner :: TOPRunner
              }

data Message = Connecting | RegisterVote Int | DoShowVote [Int] | DoFilmVote [Int] | Reset
  -- Triger(movie,lux,etc), GotoTime,

data OutputState = Tree TOP

instance FromJSON Message where
  parseJSON = withObject "message" $ \o -> do
    ty <- o .: "type"
    case ty of
      "connecting" -> return Connecting
      "vote" -> RegisterVote <$> o .: "index"
      "doShowVote" -> DoShowVote <$> o .: "votes"
      "doFilmVote" -> DoFilmVote <$> o .: "votes"
      "reset" -> return Reset
      _ -> fail ("Unknown type " ++ ty)

data OutMsg = VotesMsg [Text]

instance ToJSON OutMsg where
  toJSON (VotesMsg vs) = object ["type" A..= "vote", "votes" A..= vs]

type Effect = Tree TOP -> Tree TOP

data TDState = TDState { _activeVotes :: ActiveVotes
                       , _filmVotePool :: Map Int FilmVote
                       , _lastVoteWinner :: Maybe VoteText
                       , _voteTimer :: Maybe Int
                       , _movieDecks :: (BS.ByteString, BS.ByteString)
                       , _movieDeckIndex :: DeckIndex
                       , _effects :: [ Effect ]
                       }

data DeckIndex = Left | Right deriving (Show, Eq)

type CrypticName = Text
type DecrypticName = Text
newtype VoteText = VoteText { voteNames :: (CrypticName, DecrypticName) }

class Vote a where
  run :: a -> TDState -> TDState
  voteText :: a -> VoteText

data ShowVote = ShowVote VoteText

instance Vote ShowVote where
  run = id . flip const
  voteText (ShowVote vt) = vt

data FilmVote = FilmVote VoteText FilmData

data FilmData = Movie BS.ByteString
              | Effect (Tree TOP -> Tree TOP)

data ActiveVotes = ShowVotes [ (ShowVote, Int) ]
                 | FilmVotes [ (Int, Int) ]
                 | NoVotes


makeLenses ''TDState
makeLenses ''ServerState

instance Vote FilmVote where
  run (FilmVote _ (Movie file)) td =
    let
      accessor = if (td ^. movieDeckIndex) == Right then _1 else _2
    in
      td &
        (movieDecks . accessor .~ file) .
        (movieDeckIndex %~ B.bool Left Right . ((==) Left))
  run (FilmVote _ (Effect eff)) td = td & effects %~ (eff:)
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

newServerState :: TOPRunner -> ServerState
newServerState = ServerState [] newTDState

newTDState :: TDState
newTDState = TDState NoVotes filmVotes Nothing Nothing ("", "") Right []

filmVotes :: Map Int FilmVote
filmVotes = M.fromList [ (0, FilmVote (VoteText ("Basic", "B")) (Movie "Holme/hlme000a_hap.mov"))
                      , (1, FilmVote (VoteText ("Artistic Significance, Airdancer", "ASA")) (Movie "Holme/hlme000ac_hap.mov"))
                      , (2, FilmVote (VoteText ("Airdancer", "A")) (Movie "Holme/hlme000d_hap.mov"))
                      , (3, FilmVote (VoteText ("Black & White", "B&W")) (Effect $ glslTP' id "scripts/bandw.glsl" [] . (:[])))
                      , (4, FilmVote (VoteText ("VHS", "V")) (Effect $ glslTP' id "scripts/vhs.glsl" [("i_time", emptyV4 & _1 ?~ seconds)] . (:[])))
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
    newState = newServerState runner
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

renderTDState :: TDState -> (Tree TOP, Tree CHOP)
renderTDState (TDState {_activeVotes, _lastVoteWinner, _voteTimer, _movieDecks, _movieDeckIndex, _effects}) =
  (outT $ compT 0
  $ zipWith renderVote [0..] votes
  ++ maybeToList (resText . (++) "Last vote: " . unpack . snd . voteNames <$> _lastVoteWinner)
  ++ maybeToList (fmap (resTexts . caststr . LD.floor)
                  $ (!*) . msToF
                  <*> ((!+) (float 1) . (!*) (float (-1))) . chopChanName "timer_fraction" . (timerS' (timerStart .~ True)) . msToF
                  <$> _voteTimer)
  ++ [(switchT' (switchTBlend ?~ bool True)
      (chopChan0 . lag (float 0.3) (float 0.3) . constC . (:[]) . B.bool (float 0) (float 1) . ((==) Right) $ _movieDeckIndex)
      [ (mv $ fst _movieDecks)
      , (mv $ snd _movieDecks)
      ]) & (foldl (.) id _effects)
     ], (switchC
      (casti . chopChan0 . constC . (:[]) . B.bool (float 0) (float 1) . ((==) Right) $ _movieDeckIndex)
      [ audioMovie (mv $ fst _movieDecks)
      , audioMovie (mv $ snd _movieDecks)
      ]) & audioDevOut' (audioDevOutVolume ?~ float 0))
  where
    renderVote idx (voteName, tally) =
      (resText .  (flip (++) $ show tally) . unpack $ voteName)
      & transformT' (transformTranslate .~ (Nothing, Just . float $ (1 - 0.33 * (fromIntegral $ idx) - 0.66)))
    msToF = float . (flip (/) 1000.0) . fromIntegral
    mv = movieFileIn' (moviePlayMode ?~ int 0) . str. BS.unpack
    votes =
      case _activeVotes of
        (ShowVotes vs) -> vs & traverse . _1 %~ (fst . voteNames . voteText)
        (FilmVotes vs) -> vs & traverse . _1 %~ (fst . voteNames . voteText . (!) filmVotes)
        NoVotes -> []

resText = resTexts . str
resTexts = textT' (topResolution .~ iv2 (1920, 1080))

modifyTDState :: (TDState -> TDState) -> MVar ServerState -> IO ()
modifyTDState f state = do
  s <- modifyMVar state $ (\s -> return (s, s)) . (tdState %~ f)
  s ^. runner $ renderTDState $ s ^. tdState
  let tdVal g = s ^. tdState . g
  broadcast (VotesMsg . fmap (fst . voteNames) . activeVoteTexts $ tdVal activeVotes) $ s ^. clients

-- Server

serve :: MVar ServerState -> IO ()
serve state = do
  _ <- async $ WS.runServer "127.0.0.1" 9160 . application $ state
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
    (Just Connecting) ->
        flip finally disconnect $ do
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
      (Just (RegisterVote i)) -> modifyTDState (updateVote i) state
      (Just Reset) -> modifyTDState (const newTDState) state
      (Just (DoFilmVote vs)) -> startTimer (nextFilmVote vs)
      (Just (DoShowVote vs)) -> startTimer (nextShowVote vs)
      (Just Connecting) -> putStrLn "Connecting twice?"
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
nextFilmVote [] td = set activeVotes NoVotes td
nextFilmVote ids td = set activeVotes (FilmVotes $ (, 0) <$> filter (flip member (td ^. filmVotePool)) ids) td

nextShowVote :: [ Int ] -> TDState -> TDState
nextShowVote [] = set activeVotes NoVotes
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
      (runFilmVote maxVote'') .
      (filmVotePool %~ M.delete maxVote')
endVote td@(TDState { _activeVotes = NoVotes }) = td

maxVote :: [(a, Int)] -> a
maxVote = fst . maximumBy (flip (flip compare . snd) . snd)

  -- let
  --   maxIdx = fromJust . listToMaybe . map fst . reverse . sortOn snd . zip [0..] $ _tallies
  --   currentVotes = voteList !! _currentVote
  -- in
  --   td & ((currentVote %~ flip mod (length voteList) . (+ 1)) .
  --         (tallies .~ []) .
  --         (voteTimer .~ Nothing) .
  --         (lastVoteWinner ?~ maxIdx) .
  --         (applyVote $ currentVotes !! maxIdx)
  --        )

runFilmVote :: FilmVote -> TDState -> TDState
runFilmVote (FilmVote _ (Movie m)) td =
  let
    accessor = if (td ^. movieDeckIndex) == Right then _1 else _2
  in
    td &
      (movieDecks . accessor .~ m) .
      (movieDeckIndex %~ B.bool Left Right . ((==) Left))

runFilmVote (FilmVote _ (Effect e)) td = td & effects %~ (e:)


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

-- TD Handling


-- -- TDData

-- votesTable = voteToBS <$> [ VoteMovie "a" "A"
--                           , VoteMovie "b" "B"
--                           , VoteMovie "c" "C"
--                           , VoteMovie "d" "D"
--                           ]

-- votes = table $ fromLists votesList
-- votesList = [ ["a", "b", "c"]
--             , ["c", "b", "a"]
--             , ["b", "a", "c"]
--             ]

-- voteResultCache = fix "voteResults" $ table $ mempty
-- voteValueCache = fix "voteValues" $ table $ mempty


-- voteTimer = timerSeg' ((timerShowSeg ?~ bool True) . (timerCallbacks ?~
--                                                       fileD' (datVars .~ [ ("resultCache", Resolve voteResultCache)
--                                                                          , ("valueCache", Resolve voteValueCache)
--                                                                          , ("maxvote", Resolve maxVote)
--                                                                          , ("votesList", Resolve votes)
--                                                                          ]) "scripts/timer_callbacks.py")) . ((TimerSegment 0 0.1):)$ (\_ -> TimerSegment 0 8) <$> votesList

-- voteTick = casti (chopChanName "segment" voteTimer) !+ int (-1)
-- currentVotes = selectD' ((selectDRStartI ?~ casti voteTick) . (selectDREndI ?~ casti voteTick)) votes
-- voteNums = zipWith (\i c -> fix (BS.pack $ "voteNum" ++ show i) c) [0..] [constC [(float 0)], constC [(float 0)], constC [(float 0)]]
-- voteCount r v = count' ((countThresh ?~ (float 0.5)) . (countReset ?~ r) . (countResetCondition ?~ int 0)) v
-- voteEnabled = ceil $ chopChanName "timer_fraction" voteTimer

-- maxVote = fix "maxVote" $ cookC $ fan' ((fanOp .~ (Just $ int 1)) . (fanOffNeg ?~ bool False)) $
--             math' ((mathCombChops ?~ (int 4)) . (mathInt ?~ (int 2)))
--               [ mergeC $ voteCount (fix "resetVotes" $ constC [float 0]) <$> voteNums
--               , math' (mathCombChops ?~ (int 7)) $ voteCount (fix "resetVotes" $ constC [float 0]) <$> voteNums
--               ]

-- -- Screens

-- voteScreenRes = iv2 (1920, 1080)

-- voteScreen = compT 0 $ (lastVoteT & transformT' (transformTranslate._2 ?~ float 0.4)):((\i -> (transformT' ((transformTranslate .~ (emptyV2 & _2 ?~ float (0.33 - (fromIntegral i) * 0.33))))) . textT' (topResolution .~ voteScreenRes) $ (cell (int 0, int i) currentVotes)) <$> [0..2])

-- lastVote = cell (numRows voteValueCache !+ int (-1), int 0) voteValueCache

-- lastVoteT = textT' (topResolution .~ voteScreenRes) $ ternary (lastVote !== bstr "None") (str "") $ str "Last Vote: " !+ lastVote



-- --Server

-- server = fix "server"
--   (fileD' (datVars .~ [ ("website", Resolve website)
--                      , ("control", Resolve control)
--                      , ("timer", Resolve voteTimer)
--                      , ("resultCache", Resolve voteResultCache)
--                      , ("valueCache", Resolve voteValueCache)
--                      -- , ("movieTimer", Resolve movieTimer)
--                      -- , ("base", Resolve movieout)
--                      -- , ("outf", Resolve finalout)
--                      ] ++ zipWith (\i v -> (BS.pack $ "vote" ++ show i, Resolve v)) [0..] voteNums) "scripts/server.py")
--         & tcpipD' ((tcpipMode ?~ (int 1)) . (tcpipCallbackFormat ?~ (int 2)))

-- peers = fix "myPeers" $ textD ""
-- closepeer = fix "closePeer" $ textD "args[0].close()"
-- website = fileD "scripts/website.html"
-- control = fileD "scripts/control.html"

-- sendServer = datExec' (deTableChange ?~ "  if dat[0, 0]: mod.server.updateVotes(dat[0, 0].val, dat[0,1].val, dat[0,2].val)") currentVotes






----------------------------------------------------

-- data VoteType = Movie | Effect deriving Eq
-- data VoteEffect = VoteEffect VoteType BS.ByteString BS.ByteString BS.ByteString deriving Eq

-- vtToBS Movie = "movie"
-- vtToBS Effect = "effect"
-- veToBS (VoteEffect (vtToBS -> ty) i1 i2 i3) = ty:[i1, i2, i3]

-- sidebyside = glslT' (topResolution .~ iv2 (1920 * 2, 1080)) "scripts/sidebyside.frag"
-- movieout = nullT $ switchT (chopChan0 $ invert [lastMovieInd]) [deckA, deckB]
-- finalout = outT $ sidebyside [movieout, voteview]

-- movieTimer = timer' id (int (60 * 60 * 3))

-- deck ind = movieFileIn' ((moviePlayMode ?~ int 1) .
--                       (movieIndex ?~ casti (chopChanName "timer_frames" movieTimer)) .
--                       (topResolution .~ iv2 (1920, 1080))) $
--   (cell ((casti $ floor $ chopChan0 ind) !% int (length moviesList), int 0) movies)
-- deckA = hold movieInd lastMovieInd & deck
-- deckB = hold movieInd (invert [lastMovieInd]) & deck

-- movieVote = selectD' (selectDRExpr ?~ PyExpr "re.match('movie',me.inputCell.val) != None") prevVote
-- movieInd' = constC . (:[]) $ castf $ cell ((int 0), casti (chopChan0 maxVote) !+ int 1) movieVote
-- movieInd = feedbackC (constC [float 0]) (\m -> hold (mergeC' (mergeCDupes ?~ int 1) [movieInd', m]) (invert $ [constC [voteEnabled]])) id

-- voteEnabled = ceil $ chopChanName "timer_fraction" voteTimer


-- lastMovieInd = logic' ((logicPreop ?~ (int 2)) . (logicConvert ?~ (int 3))) [movieInd]

-- moviesList = map (\i -> BS.concat ["C:\\Users\\ulyssesp\\Development\\Lux-TD\\3 min\\Anna - Copy (", BS.pack $ show i, ").mp4"]) [1..34]
--   ++
--   [ "C:\\Users\\ulyssesp\\Development\\Lux-TD\\3 min\\David.mp4"
--   , "C:\\Users\\ulyssesp\\Development\\Lux-TD\\3 min\\Helen.mp4"
--   ]
-- movies = table $ transpose $ fromLists [moviesList]


-- -- Votes
-- votesList = veToBS <$> [ VoteEffect Movie "0" "0" "0"
--                        , VoteEffect Effect "bandw" "vhs" "bandw"
--                        , VoteEffect Movie "19" "34" "35"
--                        , VoteEffect Movie "0" "34" "35"
--                        , VoteEffect Effect "vhs" "vhs" "bandw"
--                        , VoteEffect Movie "20" "34" "35"
--                        , VoteEffect Movie "10" "34" "35"
--                        , VoteEffect Effect "vanish" "fade" "dim"
--                        , VoteEffect Movie "15" "34" "35"
--                        ]
-- votes = table $ fromLists votesList
-- currentVote = selectD' (selectDRI ?~ (casti $ (chopChanName "segment" voteTimer) !+ (chopChanName "running" voteTimer))) votes
-- prevVote = selectD' (selectDRI ?~ (casti $ (chopChanName "segment" voteTimer))) votes

-- voteCount r v = count' ((countThresh ?~ (float 0.5)) . (countReset ?~ r) . (countResetCondition ?~ int 0)) v

-- maxVote = fan' ((fanOp .~ (Just $ int 1)) . (fanOffNeg ?~ bool False)) $
--             math' ((mathCombChops ?~ (int 4)) . (mathInt ?~ (int 2)))
--               [ mergeC $ voteCount (constC [voteEnabled]) <$> voteNums
--               , math' (mathCombChops ?~ (int 7)) $ voteCount (constC [voteEnabled]) <$> voteNums
--               ]

-- voteview = let c x = (int 0, int x)
--                mcell n =
--                  transformT' (transformTranslate._2 ?~ float (fromIntegral (n - 2) * 0.33))
--                  $ textT' (topResolution .~ iv2 (1920, 1080)) $ cell (c n) currentVote
--   in compT 0 $ mcell <$> [1..3]

-- -- Effects

-- effects = [ fix "bandw" $ N $ GLSLTOP (fix "bandwfrag" $ fileD "scripts/Lux/bandw.glsl") [] Nothing (iv2 (1920, 1080)) [] Nothing
--           , fix "vhs" $ N $ GLSLTOP (fix "vhsfrag" $ fileD "scripts/Lux/vhs.glsl") [("i_time", emptyV4 & _1 ?~ seconds)] Nothing (iv2 (1920, 1080)) [] Nothing
--           ]

-- effectVote = selectD' (selectDRExpr ?~ PyExpr "re.match('effect',me.inputCell.val) != None") prevVote
-- effectRunner t = datExec' ((datVars .~ [("base", Resolve movieout), ("voteResult", Resolve maxVote)]) . (deTableChange ?~ t)) effectVote

-- --Server

-- server = fix "server"
--   (fileD' (datVars .~ [ ("website", Resolve website)
--                      , ("control", Resolve control)
--                      , ("timer", Resolve voteTimer)
--                      , ("movieTimer", Resolve movieTimer)
--                      , ("base", Resolve movieout)
--                      , ("outf", Resolve finalout)
--                      ] ++ zipWith (\i v -> (BS.pack $ "vote" ++ show i, Resolve v)) [0..] voteNums) "scripts/Lux/server.py")
--         & tcpipD' ((tcpipMode ?~ (int 1)) . (tcpipCallbackFormat ?~ (int 2)))

-- peers = fix "myPeers" $ textD ""
-- closepeer = fix "closePeer" $ textD "args[0].close()"
-- website = fileD "scripts/Lux/website.html"
-- control = fileD "scripts/Lux/control.html"

-- sendServer = datExec' (deTableChange ?~ "  mod.server.updateVotes(dat[0, 1].val, dat[0,2].val, dat[0,3].val)") currentVote

-- voteNums = zipWith (\i c -> fix (BS.pack $ "voteNum" ++ show i) c) [0..] [constC [(float 0)], constC [(float 0)], constC [(float 0)]]

-- voteTimer = timerSeg' ((timerShowSeg ?~ bool True) . (timerCallbacks ?~ fileD "scripts/Lux/Lux/timer_callbacks.py"))
--   [ TimerSegment 0 8
--   , TimerSegment 0 8
--   , TimerSegment 0 8
--   , TimerSegment 0 8
--   , TimerSegment 0 8
--   , TimerSegment 0 8
--   ]

-- -- Helpers

-- invert l = logic' (logicPreop ?~ int 1) l
-- secChop = constC [floor seconds]
