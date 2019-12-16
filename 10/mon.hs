#!/usr/bin/env runghc

{-# language FlexibleContexts #-}
{-# language PatternSynonyms #-}
{-# language TemplateHaskell #-}

module Main (main) where

import Data.Foldable (for_)
import Data.Complex (Complex((:+)), phase)
import Data.Function (on)
import Data.Ix (Ix, inRange)
import Data.List (maximumBy)
import Data.Word (Word8)
import Language.Haskell.TH (runQ, Pat(LitP), Lit(IntegerL))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Control.Monad.State.Strict as State
import qualified Data.Map.Strict as Map

pattern Asteroid :: Word8
pattern Asteroid =
  $(runQ (pure (LitP (IntegerL (fromIntegral (fromEnum '#'))))))

-- | Position is V2 down right, so V2 10 12 is 10 units from the top
-- edge and 12 units from the left edge.
data V2 =
  V2 !Int !Int
  deriving (Eq, Ix, Ord, Show, Read)

addV2 :: V2 -> V2 -> V2
V2 u1 r1 `addV2` V2 u2 r2 =
  V2 (u1+u2) (r1+r2)

subtractV2 :: V2 -> V2 -> V2
V2 u1 r1 `subtractV2` V2 u2 r2 =
  V2 (u1-u2) (r1-r2)

-- | Generate all locations at delta distances. The expression @spread
-- bounds centrum delta@ generates all locations within @bounds@ you
-- get by repeatedly adding @delta@, starting at @centrum + delta@.
spread :: (V2, V2) -> V2 -> V2 -> [V2]
spread bnd centrum delta =
  go centrum delta
  where
    go c d =
      let
        loc =
          c `addV2` d
      in
        case inRange bnd loc of
          True ->
            loc : go loc d
          False ->
            []

-- | Asteroid map in sparse representation.
data AstMap =
  AstMap
  { -- | All asteroids locations within map bounds.
    asteroids :: !(Set V2)
    -- | Map bounds.
  , bounds :: (V2, V2)
  }
  deriving (Eq, Show)

readAstMap :: FilePath -> IO AstMap
readAstMap name =
  extract <$> readRows
  where
    extract (bnds, rows) =
      AstMap (State.execState (insert rows) Set.empty) bnds

    insert rows = do
      for_ (zip [0..] rows) $ \(h, r) -> do
        for_ (zip [0..] r) $ \(w, e) -> do
          case e of
            Asteroid ->
              State.modify' (Set.insert (V2 h w))
            _ ->
              pure ()

    readRows = do
      rows <-
        fmap (fmap (fromIntegral . fromEnum)) . lines
        <$> readFile name
      case rows of
        [] ->
          error "Empty map file."
        (r:_) ->
          let
            width =
              length r
            height =
              length rows
          in
            pure ((V2 0 0, V2 (height-1) (width-1)),  rows)

detect :: AstMap -> V2 -> Set V2
detect amp loc =
  State.execState (mapM_ remove (concatMap blocked asts)) asts
  where
    remove ast =
      State.modify' (Set.delete ast)

    -- All asteroids in image, except the one housing the monitoring
    -- station.
    asts =
      Set.delete loc (asteroids amp)

    -- All locations on the map that are blocked by a specific
    -- asteroid.
    blocked ast =
      let
        -- Distance between asteroid and monitoring station.
        V2 dh dw =
          ast `subtractV2` loc
        gcd' =
          gcd dh dw
        -- Delta scaled by greatest common divisor in both directions.
        delta =
          V2 (dh `div` gcd') (dw `div` gcd')
      in
        spread (bounds amp) ast delta

bestLocation :: AstMap -> (V2, Int)
bestLocation amp =
  maximumBy (compare `on` snd) assocs'
  where
    mkAssoc loc =
      (loc, Set.size (detect amp loc))
    assocs' =
      mkAssoc <$> Set.toList (asteroids amp)


-- | Calculate bearing clockwise from up, from @V2 0 0@ to the given
-- location.
bearing :: V2 -> Float
bearing (V2 h w) =
  pi + phase (rh :+ negate rw)
  where
    rh =
      fromIntegral h
    rw =
      fromIntegral w

-- | Vaporize all asteroids from the monitoring station, going in
-- circles clockwise.
shoot ::
  -- | Asteroid map.
  AstMap ->
  -- | Monitoring station location.
  V2 ->
  -- | Locations of all destroyed asteroids, ordered by when they were
  -- shot.
  [V2]
shoot amp loc =
  check amp
  where
    check remaining =
      let
        visible =
          detect remaining loc
      in
        if Set.null visible then [] else remove visible remaining

    remove visible remaining =
      let
        annotateWithBearing ast =
          (bearing (ast `subtractV2` loc), ast)
        orderedShots =
          Map.fromList (annotateWithBearing <$> Set.toList visible)
        shotAsts =
          -- Yields the values (asteroid locations) in ascending order
          -- of keys (bearing).
          Map.elems orderedShots

        -- Updated astroid map where we have removed all visible
        -- asteroids.
        remaining' =
          remaining
          { asteroids =
              Set.difference (asteroids remaining) visible
          }
      in
        shotAsts ++ check remaining'

showAsteroidLoc :: V2 -> String
showAsteroidLoc (V2 y x) =
  show x ++ ", " ++ show y

main :: IO ()
main = do
  amp <-
    readAstMap "map.txt"
  let
    (loc, count) =
      bestLocation amp
  putStrLn $
    "Best location at " ++ showAsteroidLoc loc ++ ", showing "
    ++ show count ++ " asteroids."

  let
    no200Loc@(V2 y x) =
      head (drop 199 (shoot amp loc))
  putStrLn $ "Asteroid no 200 shot was at " ++ showAsteroidLoc no200Loc
  putStrLn $ "The coordinate code is " ++ show ((100*x)+y)
