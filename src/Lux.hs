{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lux where

import Debug.Trace

import LambdaDesigner.Op as LD
import LambdaDesigner.Lib as LD

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception (finally, catch)
import Control.Lens
import Control.Lens.Reified
import Control.Monad (forM_, forever)
import Control.Monad.State.Lazy
import Data.Aeson as A
import Data.IORef
import Data.List
import Data.Matrix
import Data.Maybe
import Data.Text (Text)
import qualified Network.WebSockets as WS


import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T

type Client = (Int, WS.Connection)

type TOPRunner = (Tree TOP -> IO ())
data ServerState =
  ServerState { _clients :: [Client]
              , _tdState :: TDState
              , _runner :: TOPRunner
              }

data Message = Connecting | RegisterVote Int | NextVote

data OutputState = Tree TOP

instance FromJSON Message where
  parseJSON = withObject "message" $ \o -> do
    ty <- o .: "type"
    case ty of
      "connecting" -> return Connecting
      "vote" -> RegisterVote <$> o .: "index"
      "nextVote" -> return NextVote
      _ -> fail ("Unknown type " ++ ty)

data OutMsg = Votes [String]

instance ToJSON OutMsg where
  toJSON (Votes ns) = object ["type" A..= "vote", "votes" A..= ns]

data TDState = TDState { _tallies :: [Int]
                       , _currentVote :: Int
                       , _lastVoteWinner :: Maybe Int
                       , _voteTimer :: Maybe Int
                       } deriving Show

data Vote = Vote { _name :: String }

data TimerState = Start | Stop
type Timer = (TVar TimerState, TMVar ())

makeLenses ''TDState
makeLenses ''Vote
makeLenses ''ServerState

voteList = [ [Vote "a", Vote "b", Vote "c"]
           , [Vote "b", Vote "c", Vote "a"]
           , [Vote "c", Vote "a", Vote "b"]
           ]

-- Run

go = do
  r <- topRunner
  let newState = newServerState r
  state <- newMVar newState
  r $ renderTDState $ newState ^. tdState
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

renderTDState :: TDState -> Tree TOP
renderTDState (TDState {_currentVote, _tallies, _lastVoteWinner, _voteTimer}) =
  compT 0 $ zipWith renderVote [0..] _tallies ++ maybeToList (resText . (++) "Last vote: " . _name <$> (_lastVoteWinner >>= \i -> voteList ^? ((ix ((_currentVote - 1) `mod` length voteList))) . (ix i))) ++ maybeToList ((\t -> resTexts . caststr . LD.floor . ((!*) (float (fromIntegral t / 1000.0))) . ((!+) (float 1) . (!*) (float (-1))) . chopChanName "timer_fraction" . (timerS' (timerStart .~ True)) . float . (flip (/) 1000.0) . fromIntegral $ t) <$> _voteTimer)
  where
    renderVote optionidx tally =
      (resText .  (flip (++) $ show tally) . _name $ currentVotes !! optionidx)
      & transformT' (transformTranslate .~ (Nothing, Just . float $ (1 - 0.33 * (fromIntegral $ optionidx) - 0.66)))
    currentVotes = voteList !! _currentVote

resText = resTexts . str
resTexts = textT' (topResolution .~ iv2 (1920, 1080))

modifyTDState :: (TDState -> TDState) -> MVar ServerState -> IO ()
modifyTDState f state = do
  s <- modifyMVar state $ (\s -> return (s, s)) . (tdState %~ f)
  s ^. runner $ renderTDState $ s ^. tdState
  traceShowM (s ^. tdState)
  let tdVal g = s ^. tdState . g
  broadcast (Votes $ fmap _name $ take (length $ tdVal tallies) . ((!!) voteList) $ (tdVal currentVote)) $ s ^. clients

-- Server

newServerState :: TOPRunner -> ServerState
newServerState = ServerState [] (TDState [0, 0, 0] 0 Nothing Nothing)

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
      (Just (RegisterVote i)) -> do
        modifyTDState (updateVote i) state
      (Just NextVote) -> do
        let voteLength = 10000
        timer <- newTimer voteLength
        __ <- forkIO $ do
          waitTimer timer
          modifyTDState endVote state
        modifyTDState (nextVote voteLength) state

      _ -> putStrLn "Unrecognized message"
  wait thr

broadcast :: OutMsg -> [Client] -> IO ()
broadcast msg cs = do
  forM_ cs $ \(_, conn) -> WS.sendTextData conn (encode msg)

-- Votes

updateVote :: Int -> TDState -> TDState
updateVote id = tallies . ix id %~ (+ 1)

nextVote :: Int -> TDState -> TDState
nextVote timer td@(TDState { _currentVote, _tallies, _lastVoteWinner }) =
  let
    votes = (!!) voteList
  in
    td & (tallies .~ take (length $ votes _currentVote) (repeat 0)) . (voteTimer ?~ timer)

endVote :: TDState -> TDState
endVote td@(TDState { _tallies })=
  let
    maxIdx = listToMaybe . map fst . reverse . sortOn snd . zip [0..]
  in
    td & ((currentVote %~ flip mod (length voteList) . (+ 1)) . (tallies .~ []) . (voteTimer .~ Nothing) . (lastVoteWinner .~ maxIdx _tallies))


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
