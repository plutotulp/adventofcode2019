{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language PatternSynonyms #-}
{-# language RankNTypes #-}
{-# language ViewPatterns #-}

import Control.Monad.Except
  ( ExceptT
  , MonadError
  , liftEither
  , runExcept
  , runExceptT
  , throwError
  )
import Control.Monad.State (StateT, gets, modify', runStateT)
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Foldable (toList)
import qualified  Data.IntMap.Strict as IntMap
import Data.Ix (Ix)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import System.IO (hFlush, stdout)

import Intcode
  ( IMEnv(imMemory)
  , IMInterpreter
  , TraceFlag(NotTracing)
  , initIMEnv
  , readIntcodeProg
  , runIntcodeInterpreter
  , stepIntcode
  )

type TileId = Word

-- pattern EmptyTileId :: TileId
-- pattern EmptyTileId = 0

-- pattern WallTileId :: TileId
-- pattern WallTileId = 1

pattern BlockTileId :: TileId
pattern BlockTileId = 2

pattern PaddleTileId :: TileId
pattern PaddleTileId = 3

pattern BallTileId :: TileId
pattern BallTileId = 4

-- | Position is V2 down right, so V2 10 (-12) is 10 units down and
-- and 12 units to the left.
data V2 =
  V2 { v2Y :: !Int, v2X :: !Int }
  deriving (Eq, Ix, Ord, Show)

data DrawScoreCmd =
  DrawScore !Int
  deriving Show

data DrawTileCmd =
  DrawTile
  { dtcPos :: !V2
  , dtcTid :: !TileId
  }
  deriving Show

type Cmd =
  Either DrawScoreCmd DrawTileCmd

chunkBy :: Int -> [a] -> [[a]]
chunkBy n =
  go
  where
    go lst =
      let
        (chk, rest) =
          splitAt n lst
      in
        chk : if null rest then [] else go rest

parseCmdStream :: MonadError String m => [Int] -> m [Cmd]
parseCmdStream =
  traverse go . chunkBy 3
  where
    go [(-1),0,score'] =
      pure (Left (DrawScore score'))
    go [x,y,tid] =
      pure (Right (DrawTile (V2 y x) (fromIntegral tid)))
    go other =
      throwError $
      "Cmd stream ended too soon (final elements are "
      ++ show other ++ ")"

-- | Run intcode interpreter until it either halts or produces a new
-- command.
getNextCmd' :: [Int] -> IMInterpreter 'NotTracing Int (Maybe Cmd)
getNextCmd' input = do
  mv1 <- stepIntcode input
  mv2 <- stepIntcode []
  mv3 <- stepIntcode []
  case mv1 of
    Nothing ->
      pure Nothing
    Just v1 -> do
      let
        mvs =
          sequence [mv2,mv3]
        err =
          throwError $
          "Intcode program erroneously ended mid-output with "
          ++ show [mv1,mv2,mv3]
        parse vs =
          listToMaybe <$> parseCmdStream (v1:vs)
      maybe err parse mvs






data GameState =
  GameState
  {
    -- | All tiles on the screen. Since we don't know how large the
    -- screen is, we're going with map, which will be much slower than
    -- mutable arrays.
    tiles :: !(Map V2 TileId)

    -- | Current game score.
  , score :: !Int

    -- | Intcode interpreter's execution environment, for the game
    -- logic interpreter.
  , intcodeEnv :: !(IMEnv Int)
  }
  deriving Show

initGameState :: [Int] -> GameState
initGameState prog =
  GameState mempty 0 (initIMEnv prog)

type GameT m a =
  Monad m => StateT GameState (ExceptT String m) a

runGameT ::
  Monad m =>
  GameT m a ->
  GameState ->
  m (Either String (a, GameState))
runGameT action st =
  runExceptT (runStateT action st)

executeCmd :: Cmd -> GameT m ()
executeCmd = \case
  Left (DrawScore score') -> do
    modify' $ \st ->
      st { score = score' }
  Right dtc -> do
    modify' $ \st ->
      st { tiles =
             Map.insert (dtcPos dtc) (dtcTid dtc) (tiles st)
      }

getSingleTilePos :: TileId -> String -> GameT m (Maybe V2)
getSingleTilePos targetTid name =
  extract =<< getPositions
  where
    extract = \case
      [pos] ->
        pure (Just pos)
      [] ->
        pure Nothing
      ps ->
        throwError $
        "There are multiple " ++ name ++ " tiles: " ++ show ps
    getPositions =
      fmap fst . filter isTarget . Map.assocs <$> gets tiles
    isTarget (snd -> tid) =
      tid == targetTid

getBallPos :: GameT m (Maybe V2)
getBallPos =
  getSingleTilePos BallTileId "ball"

getPaddlePos :: GameT m (Maybe V2)
getPaddlePos =
  getSingleTilePos PaddleTileId "paddle"

countBlockTiles :: GameT m Int
countBlockTiles =
  length . filter isBlock . toList <$> gets tiles
  where
    isBlock tid =
      tid == BlockTileId

-- | Run game logic until it either halts or produces a new command.
getNextCmd :: [Int] -> GameT m (Maybe Cmd)
getNextCmd input =
  gets intcodeEnv >>= execute >>= updateStAndYield
  where
    execute env =
      liftEither (runExcept (runStateT interpret env))

    interpret =
      runIntcodeInterpreter (getNextCmd' input)

    updateStAndYield (mCmd, env) =
      mCmd <$ updateSt env

    updateSt env =
      modify' $ \st -> st { intcodeEnv = env }

insertQuarters :: Int -> GameT m ()
insertQuarters n =
  updateSt . modifyEnv =<< gets intcodeEnv
  where
    modifyEnv env =
      env { imMemory = IntMap.insert 0 n (imMemory env) }
    updateSt env =
      modify' $ \st -> st { intcodeEnv = env}






-- | Initial game with no inputs. Performs all commands dictated by
-- the game logic until it halts, then yield the number of block tiles
-- currently drawn.
initialRunBlockTiles ::
  (MonadError String m, MonadIO m) => [Int] -> m (Int)
initialRunBlockTiles prog =
  liftEither =<< go
  where
    go = do
      fmap (fmap fst) $ flip runGameT (initGameState prog) $ do
        runUntilHalt

    runUntilHalt =
      maybe countBlockTiles updateGame =<< getNextCmd []

    updateGame cmd = do
      executeCmd cmd
      runUntilHalt

type JoystickPos = Int

pattern NeutralJoystickPos :: JoystickPos
pattern NeutralJoystickPos = 0

pattern LeftJoystickPos :: JoystickPos
pattern LeftJoystickPos = -1

pattern RightJoystickPos :: JoystickPos
pattern RightJoystickPos = 1

-- | Play game until finished with very simple paddle movement logic:
-- Just always move the paddle towards the ball.
play :: (MonadError String m, MonadIO m) => [Int] -> m (GameState)
play prog =
  liftEither =<< play'
  where
    play' = do
      fmap (fmap snd) $ flip runGameT (initGameState prog) $ do
        insertQuarters 2
        stepLogic [NeutralJoystickPos]

    stepLogic input =
      maybe (pure ()) (updateGame input) =<< getNextCmd input

    updateGame prevInput cmd = do
      executeCmd cmd
      input <-
        maybe prevInput pure <$> considerCmd cmd
      stepLogic input

    considerCmd cmd = do
      if cmdUpdatesBallOrPaddle cmd
        then considerPositionsIfTheyExist
        else pure Nothing

    cmdUpdatesBallOrPaddle = \case
      Right (dtcTid -> tid) ->
        tid == BallTileId || tid == PaddleTileId
      _ ->
        False

    considerPositionsIfTheyExist =
      fmap considerPositions <$> getPositions

    getPositions =
      runMaybeT
      ((,)
       <$> (fmap v2X <$> lift getPaddlePos)
       <*> (fmap v2X <$> lift getBallPos))

    considerPositions (px, bx) =
      case compare px bx of
        LT ->
          RightJoystickPos
        EQ ->
          NeutralJoystickPos
        GT ->
          LeftJoystickPos

main :: IO ()
main = do
  either error pure =<< runExceptT run

run :: (MonadError String m, MonadIO m) => m ()
run = do
  prog <-
    liftIO (readIntcodeProg "prog.txt")

  liftIO $ do
    putStr "Drawing screen ... "
    hFlush stdout
  nblk <-
    initialRunBlockTiles prog
  liftIO $ do
    putStrLn ("found " ++ show nblk ++ " block tiles")

  liftIO $ do
    putStr "Playing ... "
    hFlush stdout
  res <-
    play prog
  liftIO $ do
    putStrLn ("final score is " ++ show (score res))
