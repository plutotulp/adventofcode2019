#!/usr/bin/env runghc

{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}

import Data.Foldable
import System.IO

import qualified Control.Monad.State as State
import qualified Text.Read as Read
import qualified Data.Set as Set
import qualified Data.Map as Map

data Dir = U | D | L | R
  deriving (Eq, Read, Show)

-- wire segment
data Seg =
  Seg !Dir !Word

segDir :: Seg -> Dir
segDir (Seg d _) = d

segNum :: Seg -> Word
segNum (Seg _ n) = n

-- Show and Read instances compatible with the file format used in
-- wires.txt.
instance Show Seg where
  show (Seg d v) =
    show d ++ show v
instance Read Seg where
  readPrec = do
    Read.parens $ Read.prec 10 $
      Seg <$> (read . pure <$> Read.get) <*> Read.readPrec

type Wire =
  [Seg]

getWires :: IO [Wire]
getWires =
  -- File contains one wire description per line. A wire is just a
  -- sequence of segments. Munge input by adding brackets to each
  -- line, making them match the Haskell list syntax, then parse the
  -- lists of segments.
  fmap (read . ('[':) . (++ "]")) . lines
  <$> readFile "wires.txt"

-- The coordinates are (V2 up right), so V2 10 (-12) is at 10 in the
-- "up" direction and 12 in the "left" direction.
data V2 =
  V2 !Int !Int
  deriving (Eq, Ord, Read, Show)

addV2 :: V2 -> V2 -> V2
V2 u1 r1 `addV2` V2 u2 r2 =
  V2 (u1+u2) (r1+r2)

diffV2 :: V2 -> V2 -> V2
V2 u1 r1 `diffV2` V2 u2 r2 =
  V2 (u1-u2) (r1-r2)

manhattan :: V2 -> V2 -> Int
manhattan a b =
  abs du + abs dr
  where
    V2 du dr =
      a `diffV2` b

newtype WireId =
  WireId Int
  deriving (Enum, Eq, Ord, Num)

newtype MinSteps =
  MinSteps Int
  deriving (Eq, Ord, Num)

type Canvas =
  Map.Map V2 (Map.Map WireId MinSteps)

centralPortLocation :: V2
centralPortLocation =
  V2 0 0

unitRight, unitLeft, unitUp, unitDown :: V2
unitRight = V2   0    1
unitLeft  = V2   0  (-1)
unitUp    = V2   1    0
unitDown  = V2 (-1)   0

dirUnit :: Dir -> V2
dirUnit = \case
  U -> unitUp
  D -> unitDown
  R -> unitRight
  L -> unitLeft

-- Render wire by recording all locations it runs across. At each
-- locatin, we register the wire ID as well as the total run length of
-- the wire (no. steps so far). In cases where the wire crosses
-- itself, we only keep the first (i.e. shortest) length.
--
-- The final output is the ending location (V2), the total wire
-- length, and the final canvas, updated with the wire rendering.
renderWire :: WireId -> Wire -> Canvas -> ((V2, MinSteps), Canvas)
renderWire wireId wire canvas =
  State.execState (mapM_ go wire) ((centralPortLocation, 0), canvas)
  where
    segUnit =
      dirUnit . segDir
    segmentUnits seg =
      take (fromIntegral (segNum seg)) (repeat (segUnit seg)) :: [V2]
    combine new old =
      -- Combine 'Map.Map WireId MinSteps' at canvas location. Use the
      -- old values when duplicate keys in new and old map. This way
      -- we only keep record of the minimum number of steps (i.e. wire
      -- length).
      Map.union old new
    imprint unit =
      State.modify $ \((loc0, steps0), cvs0) ->
      let
        steps =
          steps0 + 1
        loc =
          loc0 `addV2` unit
        cvs =
          Map.insertWith combine loc (Map.singleton wireId steps) cvs0
      in
        ((loc, steps), cvs)

    go seg =
      mapM_ imprint (segmentUnits seg)

main :: IO ()
main = do
  wires <-
    getWires

  putStr "Render size … "
  hFlush stdout
  let
    render = do
      flip State.execState mempty $ do
        for_ (zip [1..] wires) $ \(wireId, wire) -> do
          State.modify (snd . renderWire wireId wire)
  putStrLn (show (Map.size render))

  putStr "No. intersections … "
  hFlush stdout
  let
    intersections =
      flip Map.foldMapWithKey render $ \loc wireRenders ->
      case Map.size wireRenders of
        2 ->
          Set.singleton (loc, sum (Map.elems wireRenders))
        _ ->
          mempty
  putStrLn (show (Set.size intersections))

  let
    nearest =
      minimum (Set.map (manhattan centralPortLocation . fst) intersections)
  putStrLn ("Nearest intersection is at distance " ++ show nearest)

  let
    MinSteps shortest =
      minimum (Set.map snd intersections)
  putStrLn ("Shortest wire length is " ++ show shortest ++ " steps")
