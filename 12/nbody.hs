{-# language FlexibleContexts #-}

module Main (main) where

import Control.Monad (foldM, replicateM_)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans (lift)
import Data.Array.MArray
  ( MArray, freeze, getBounds, newArray, newListArray, readArray
  , writeArray)
import Data.Array.ST (STUArray)
import Data.Array.Unboxed (UArray, (!), bounds)
import Data.Foldable (for_, toList)
import Data.Ix (Ix, range)
import Data.List (tails)
import System.IO (hFlush, stdout)

mkArr :: Foldable f => f Int -> ST s (STUArray s Int Int)
mkArr es =
  newListArray (0,length es - 1) (toList es)

-- xyz coordinate or velocity vector arrays, the i'th entry of all
-- arrays belonging to the i'th moon.
data Vec3 s =
  Vec3 !(STUArray s Int Int) !(STUArray s Int Int) !(STUArray s Int Int)

getVec3Bounds :: Vec3 s -> ST s (Int, Int)
getVec3Bounds (Vec3 x _ _) =
  getBounds x

newVec3 :: (Int,Int) -> Int -> ST s (Vec3 s)
newVec3 bnds v =
  Vec3 <$> mk <*> mk <*> mk
  where
    mk =
      newArray bnds v

eqArray :: STUArray s Int Int -> UArray Int Int -> ST s Bool
eqArray a b =
  go
  where
    bnds =
      bounds b
    go =
      foldM and' True (range bnds)
    and' acc i =
      (acc &&) <$> ((==) <$> readArray a i <*> pure (b ! i))

modifyArray :: (MArray arr e m, Ix pos) => arr pos e -> pos -> (e -> e) -> m ()
modifyArray arr pos f = do
  v <-
    readArray arr pos
  writeArray arr pos (f v)

updateVelocityArray ::
  ( Enum ve
  , Ix i
  , MArray parr pe m
  , MArray varr ve m
  , Ord pe
  ) => parr i pe -> varr i ve -> m ()
updateVelocityArray ps vs = do
  bnds <-
    getBounds ps
  for_ (zip (range bnds) (tail (tails (range bnds)))) $ \(i1, i2s) -> do
    for_ i2s $ \i2 -> do
      p1 <-
        readArray ps i1
      p2 <-
        readArray ps i2
      case compare p1 p2 of
        LT ->
          modifyArray vs i1 succ *> modifyArray vs i2 pred
        EQ ->
          pure ()
        GT ->
          modifyArray vs i1 pred *> modifyArray vs i2 succ

updatePositionArray ::
 ( Ix i
 , MArray parr e m
 , MArray varr e m
 , Num e
 ) => parr i e -> varr i e -> m ()
updatePositionArray ps vs = do
  bnds <-
    getBounds ps
  for_ (range bnds) $ \i -> do
    v <-
      readArray vs i
    modifyArray ps i (+v)

-- xyz coordinate or velocity vector
data V3 =
  V3 !Int !Int !Int

readV3 :: Vec3 s -> Int -> ST s V3
readV3 (Vec3 xs ys zs) i =
  V3 <$> readArray xs i <*> readArray ys i <*> readArray zs i

sumAbsV3At :: Vec3 s -> Int -> ST s Int
sumAbsV3At vec i =
  addAbs <$> readV3 vec i
  where
    addAbs (V3 x y z) =
      abs x + abs y + abs z





data BodiesEnv s =
  BodiesEnv
  { positions :: Vec3 s
  , velocities :: Vec3 s
  }

mkBodiesEnv :: Vec3 s -> ST s (BodiesEnv s)
mkBodiesEnv pos = do
  bnds <-
    getVec3Bounds pos
  BodiesEnv <$> pure pos <*> newVec3 bnds 0

type Bodies s =
  ReaderT (BodiesEnv s) (ST s)

updateVelocities :: Bodies s ()
updateVelocities = do
  Vec3 xps yps zps <- asks positions
  Vec3 xvs yvs zvs <- asks velocities
  lift $ do
    updateVelocityArray xps xvs
    updateVelocityArray yps yvs
    updateVelocityArray zps zvs

updatePositions :: Bodies s ()
updatePositions = do
  Vec3 xps yps zps <- asks positions
  Vec3 xvs yvs zvs <- asks velocities
  lift $ do
    updatePositionArray xps xvs
    updatePositionArray yps yvs
    updatePositionArray zps zvs

step :: Bodies s ()
step = do
  updateVelocities
  updatePositions

getPotentialEnergyAt :: Int -> Bodies s Int
getPotentialEnergyAt i =
  lift . flip sumAbsV3At i =<< asks positions

getKineticEnergyAt :: Int -> Bodies s Int
getKineticEnergyAt i =
  lift . flip sumAbsV3At i =<< asks velocities

getTotalEnergyAt :: Int -> Bodies s Int
getTotalEnergyAt i = do
  (*) <$> getPotentialEnergyAt i <*> getKineticEnergyAt i

getTotalEnergy :: Bodies s Int
getTotalEnergy = do
  ps <-
    asks positions
  bnds <-
    lift (getVec3Bounds ps)

  let
    add acc i =
      (acc +) <$> getTotalEnergyAt i
  foldM add 0 (range bnds)






findCycleLength1D :: STUArray s Int Int -> STUArray s Int Int -> ST s Int
findCycleLength1D ps vs =
  findCycleLength1D' ps vs =<< ((,) <$> freeze ps <*> freeze vs)

findCycleLength1D' :: STUArray s Int Int -> STUArray s Int Int -> (UArray Int Int, UArray Int Int) -> ST s Int
findCycleLength1D' ps vs (ps0, vs0) =
  go 1
  where
    go n = do
      updateVelocityArray ps vs
      updatePositionArray ps vs
      eq <-
        (&&) <$> eqArray ps ps0 <*> eqArray vs vs0
      if eq then pure n else go (succ n)

findCycleLength :: Bodies s Int
findCycleLength = do
  Vec3 xps yps zps <- asks positions
  Vec3 xvs yvs zvs <- asks velocities
  (nx, ny, nz) <- do
    lift
      ((,,)
       <$> findCycleLength1D xps xvs
       <*> findCycleLength1D yps yvs
       <*> findCycleLength1D zps zvs)
  let
    d =
      minimum
      [ gcd nx ny
      , gcd nx nz
      , gcd ny nz
      ]
  pure (div nx d * div ny d * div nz d)

-- puzzle input
mkBodies :: ST s (BodiesEnv s)
mkBodies =
  mkBodiesEnv =<< mkPos
  where
    mkPos =
      Vec3
      <$> mkArr [5,-11,0,-13]
      <*> mkArr [4,-11,7,2]
      <*> mkArr [4,-3,0,10]

main :: IO ()
main = do
  putStr "Total energy after 1000 steps is "
  hFlush stdout
  let
    totalEnergy = do
      runST $ do
        bodies <- mkBodies
        flip runReaderT bodies $ do
          replicateM_ 1000 step
          getTotalEnergy
  putStrLn (show totalEnergy)

  putStr "Cycle length ... "
  hFlush stdout
  let
    ncycle =
      runST (runReaderT findCycleLength =<< mkBodies)
  putStrLn (show ncycle)
