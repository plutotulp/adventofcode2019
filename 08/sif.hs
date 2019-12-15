#!/usr/bin/env runghc

{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

import Control.Monad (when)
import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.ST (ST)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Array (Array)
import Data.Array.IArray (IArray, (!), bounds)
import Data.Array.IArray (assocs, elems, listArray)
import Data.Array.MArray (MArray, newArray, getBounds, readArray, writeArray)
import Data.Array.ST (STUArray, runSTUArray)
import Data.Array.Unboxed (UArray)
import Data.Function (on)
import Data.Ix (Ix, range)
import Data.List (minimumBy)
import Data.Monoid (Sum(..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

-- The coordinates are (V2 down right), so V2 10 (-12) is at 10 in the
-- "down" direction and 12 in the "left" direction.
data V2 =
  V2 !Int !Int
  deriving (Eq, Ix, Ord, Read, Show)

subtractV2 :: V2 -> V2 -> V2
V2 u1 r1 `subtractV2` V2 u2 r2 =
  V2 (u1-u2) (r1-r2)

readSif :: V2 -> FilePath -> IO (Array Int (UArray V2 Int))
readSif dims@(V2 height width) name =
  toImg . toLayers . toInts <$> readFile name
  where
    toInts =
      fmap (read . pure)

    layerSize =
      width * height

    layerBounds =
      (V2 0 0, dims `subtractV2` V2 1 1)

    toLayers = \case
      [] ->
        []
      ints ->
        let
          oneLayer =
            listArray layerBounds ints
          restLayers =
            toLayers (drop layerSize ints)
        in
          oneLayer : restLayers

    toImg ls =
      listArray (0, length ls - 1) ls

printUsageAndDie :: IO ()
printUsageAndDie = do
  hPutStrLn stderr "USAGE: sif WIDTH HEIGHT FILE"
  exitFailure

newtype Args =
  Args (Int, Int, FilePath)

parseArgs :: IO (Maybe Args)
parseArgs =
  parse <$> getArgs
  where
    parse = \case
      [width,height,name] -> do
        width' <-
          readMaybe width
        height' <-
          readMaybe height
        pure (Args (width', height', name))
      _ ->
        Nothing

sumEq :: (Eq a, Num b) => a -> a -> Sum b
sumEq a b
  | a == b    = Sum 1
  | otherwise = Sum 0

iarrayCount ::
  forall cnt arr i e.
  (Eq e, IArray arr e, Ix i, Num cnt) => e -> arr i e -> cnt
iarrayCount val layer =
  getSum (foldMap (sumEq val) (elems layer))

uarrFlatten ::
  forall i t.
  (Foldable t, Ix i) => (i, i) -> t (UArray i Int) -> UArray i Int
uarrFlatten arrayBounds layers = do
  runSTUArray $ do
    canvas <-
      newArray arrayBounds Transparent :: ST s (STUArray s i Int)
    mapM_ (mergeLayer canvas) layers
    pure canvas

pattern Black :: (Eq a, Num a) => a
pattern Black = 0

pattern White :: (Eq a, Num a) => a
pattern White = 1

pattern Transparent :: (Eq a, Num a) => a
pattern Transparent = 2

mergeLayer ::
  (MArray canvas e m, Ix i, Num e, IArray layer e, Eq e) =>
  canvas i e -> layer i e -> m ()
mergeLayer canvas layer =
  mapM_ mergePixel =<< (range <$> getBounds canvas)
  where
    mergePixel loc = do
      p0 <-
        readArray canvas loc
      when
        (p0 == Transparent)
        (writeArray canvas loc (layer ! loc))

showLayer ::
  (IArray arr e, Eq e, Num e) => arr V2 e -> String
showLayer layer =
  unlines (showLine <$> [h0..hn])
  where
    showLine h =
      showPixelAt h <$> [w0..wn]
    showPixelAt h w =
      showPixel (layer ! (V2 h w))
    (V2 h0 w0, V2 hn wn) =
      bounds layer

showPixel :: (Eq e, Num e) => e -> Char
showPixel = \case
  Black -> ' '
  White -> '#'
  Transparent -> '.'
  _ -> '~'

run :: (MonadIO m, MonadError String m) => Args -> m ()
run (Args (width, height, name)) = do
  layers <-
    liftIO (readSif (V2 height width) name)
  let
    zeroesAtAllLayers =
      iarrayCount @Int 0 <$> layers
    (targetLayer, zeroes) =
      minimumBy (compare `on` snd) (assocs zeroesAtAllLayers)
    ones =
      iarrayCount @Int 1 (layers ! targetLayer)
    twos =
      iarrayCount @Int 2 (layers ! targetLayer)

  liftIO $ do
    putStrLn ("target layer is " ++ show (targetLayer+1))
    putStrLn ("  " ++ show zeroes ++ " zeroes, " ++ show ones ++ " ones, " ++ show twos ++ " twos")
    putStrLn ("  product is " ++ show (ones*twos))

  let
    bnds =
      -- All layers have the same bounds. We just get them from a
      -- layer known to exist.
      bounds (layers ! targetLayer)
    img =
      uarrFlatten bnds layers
  liftIO $ do
    putStrLn ""
    putStrLn (showLayer img)

main :: IO ()
main =
  maybe printUsageAndDie run' =<< parseArgs
  where
    run' args =
      either error pure =<< runExceptT (run args)
