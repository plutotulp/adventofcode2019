#!/usr/bin/env runghc

{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}

import Control.Arrow ((>>>), second)
import Control.Monad (foldM)
import Data.Char (isAlphaNum)
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Sequence (Seq, Seq((:<|)))
import Data.String (IsString)
import Data.Text (Text)

import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State.Lazy as State
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

newtype Object =
  Object Text
  deriving (Eq, Ord, Show, IsString)

-- | The second object orbits the first.
type Orbit = (Object, Object)

-- | Parse orbit text file format.
parseOrbit :: Text -> Either Text Orbit
parseOrbit txt =
  check (State.runState parse txt)
  where
    parse = do
      a <-
        State.state (Text.span isAlphaNum)
      sep <-
        State.state (Text.splitAt 1)
      b <-
        State.state (Text.span isAlphaNum)
      pure (a, sep, b)

    -- Must parse the whole input text. Identifiers must not be empty.
    check ((a, ")", b), "")
      | not (Text.null a), not (Text.null b) =
          Right (Object a, Object b)
    check _ =
      Left ("Could not parse orbit " <> txt <> ".")

readOrbits :: FilePath -> IO (Either Text (Seq Orbit))
readOrbits name =
  sequence . Seq.fromList . fmap parseOrbit . Text.lines
  <$> Text.IO.readFile name

-- | Calculate the orbit count checksum (sum of no. direct and
-- indirect orbits), assuming that the Center Of Mass (central object)
-- is know.
checksum ::
  Foldable f =>
  -- | Center Of Mass object.
  Object ->
  -- | Orbit descriptions. The second object orbits the first one.
  f Orbit ->
  Int
checksum com =
  -- Munge input into a map from object to the objects that orbit it,
  -- then recursively count how many objects there are at each depth,
  -- starting at the known Center of Mass (COM) object.
  toList
  >>> fmap (second Set.singleton)
  >>> Map.fromListWith (<>)
  >>> Reader.runReader (walk 0 com)
  where
    walk depth obj =
      Reader.asks (Map.lookup obj) >>= maybe (pure depth) (accChildren depth)
    accChildren depth =
      foldM (\acc obj -> (+ acc) <$> walk (depth+1) obj) depth

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

parentMap ::
  Foldable f =>
  -- | Orbit descriptions. The second object orbits the first one.
  f Orbit ->
  -- | Map from object to the object it orbits.
  Map Object Object
parentMap =
  Map.fromList . fmap swap . toList

orbitalTransfers :: Object -> Object -> Map Object Object -> Int
orbitalTransfers san you mp =
  lengths (dropSharedPrefix (ancestors you) (ancestors san))
  where

    ancestors obj =
      State.evalState (Reader.runReaderT (inspect obj) mp) mempty

    inspect obj =
      maybe State.get update =<< Reader.asks (Map.lookup obj)

    update obj =
      State.modify (obj :<|) *> inspect obj

    dropSharedPrefix (a:<|as) (b:<|bs)
      | a == b =
          dropSharedPrefix as bs
    dropSharedPrefix as bs =
      (as, bs)

    lengths (as, bs) =
      Seq.length as + Seq.length bs

main :: IO ()
main = do

  Right os <-
    readOrbits "orbits.txt"

  let
    chks =
      checksum "COM" os
  putStrLn ("Orbit checksum: " ++ show chks)

  let
    xfers =
      orbitalTransfers "SAN" "YOU" (parentMap os)
  putStrLn ("Orbital transfers required: " ++ show xfers)
