{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language ViewPatterns #-}

module Robot
  ( RobotState(..)
  , Dir
  , V2(..)
  , initRobotState
  , runRobotProg
  ) where

-- black = 0, white = 1
-- all hull panels start out black, so robot start location doesn't matter
-- robot starts oriented up
-- do not restart program while painting
-- input: color of panel at robot's location
-- output: 1. color to pain panel at robot's location, 2. direction to turn (0 left 90 deg, 1 right 90 deg)..
-- after robot turns, move it forward one panel

import Control.Monad.State

import Data.Ix (Ix)
import Data.Set (Set)
import qualified Data.Set as Set

import Intcode (IMEnv(imInput), mkIMEnv, runIMInterpreterUntilOutputOrHalted)

-- | Position is V2 right down, so V2 12 (-10) is 12 units to the
-- right and 10 units to the left.
data V2 =
  V2 !Int !Int
  deriving (Eq, Ix, Ord, Show)

data Dir = U | R | D | L
  deriving (Eq, Show)

data RelDir = RelRight | RelLeft
  -- We rely on Enum pinning RelLeft to 0 and RelRight to 1.
  deriving (Eq, Enum, Show)

turn' :: Dir -> RelDir -> Dir
turn' orientation = \case
  RelRight ->
    turnRight' orientation
  RelLeft ->
    turnLeft' orientation

turnRight' :: Dir -> Dir
turnRight' = \case
  U -> R
  R -> D
  D -> L
  L -> U

turnLeft' :: Dir -> Dir
turnLeft' = \case
  U -> L
  L -> D
  D -> R
  R -> U

move' :: V2 -> Dir -> V2
move' (V2 x y) = \case
  U -> V2 x (y-1)
  R -> V2 (x+1) y
  D -> V2 x (y+1)
  L -> V2 (x-1) y

data RobotState =
  RobotState
  { location :: V2
  , direction :: Dir
  , painted :: Set V2
  , whitePanels :: Set V2
  , brain :: IMEnv Int
  , nextIp :: Int
  }
  deriving (Eq, Show)

newtype Robot a =
  Robot { unwrap :: State RobotState a }
  deriving (Functor, Applicative, Monad, MonadState RobotState)

move :: Robot ()
move =
  modify' $ \st ->
  st { location = move' (location st) (direction st) }

turn :: RelDir -> Robot ()
turn relDir =
  modify' $ \st ->
  st { direction = turn' (direction st) relDir }

data Color = Black | White
  -- We rely on Enum pinning Black to 0 and White to 1.
  deriving (Eq, Enum, Ord, Show)

type IntProg = [Int]

inspect :: Robot Color
inspect = do
  loc <-
    gets location
  isWhite <-
    gets (Set.member loc . whitePanels)
  if isWhite then pure White else pure Black

input :: Color -> Robot ()
input color =
  modify' $ \st ->
  let
    brain' =
      brain st
  in
    st
    { brain =
        (brain'
         { imInput =
             (imInput brain') ++ (pure (fromEnum color))
         }
        )
    }

awaitOutput :: Robot (Maybe (Color, RelDir))
awaitOutput =
  go
  where
    go = do
      v1 <-
        await
      case v1 of
        Nothing ->
          pure Nothing
        Just (toEnum -> color) -> do
          v2 <-
            await
          case v2 of
            Nothing ->
              error $
              "Got next color (" ++ show color
              ++ ", but not direction!"
            Just (toEnum -> relDir) -> do
              pure (Just (color, relDir))

    await = do
      env <-
        gets brain
      ip <-
        gets nextIp
      case runIMInterpreterUntilOutputOrHalted env ip of
        Left err ->
          error err
        Right (mRes, newEnv) -> do
          modify' (\st -> st { brain = newEnv })
          case mRes of
            Nothing ->
              -- halted
              pure Nothing
            Just (newIp, val) -> do
              modify' (\st -> st { nextIp = newIp })
              pure (Just val)

paint :: Color -> Robot ()
paint color =
  gets location >>= paintAt
  where
    paintAt loc =
      modify' $ \st ->
      st
      { whitePanels =
          if color == White
          then Set.insert loc (whitePanels st)
          else Set.delete loc (whitePanels st)
      , painted =
          Set.insert loc (painted st)
      }

robotProg :: Robot ()
robotProg =
  iter
  where
    iter = do
      oldColor <-
        inspect
      input oldColor
      res <-
        awaitOutput
      case res of
        Nothing ->
          pure ()
        Just (newColor, relDir) -> do
          paint newColor
          turn relDir
          move
          iter

initRobotState :: IntProg -> Set V2 -> RobotState
initRobotState prog whitePanels' =
  RobotState
  { location = V2 0 0
  , direction = U
  , painted = Set.empty
  , whitePanels = whitePanels'
  , brain = mkIMEnv prog 0
  , nextIp = 0
  }

runRobotProg :: RobotState -> RobotState
runRobotProg st =
  execState (unwrap robotProg) st
