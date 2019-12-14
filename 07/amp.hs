{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language LambdaCase #-}
{-# language RecursiveDo #-}
{-# language TupleSections #-}

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, dupChan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (BlockedIndefinitelyOnMVar, catch)
import Control.Monad.Except (MonadError, liftEither, runExceptT)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Array.MArray (thaw)
import Data.Array.Unboxed (UArray, listArray)
import Data.Either (partitionEithers)
import Data.Foldable (for_, maximumBy)
import Data.Function (on)
import Data.List (permutations)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Traversable (for)
import System.IO (hFlush, stdout)

import Intcode (Addr, runIntcodeST, runIntcodeIO)

runAmp ::
  MonadError String m =>
  -- | Amplifier program
  UArray Addr Int ->
  -- | Input
  [Int] ->
  -- | Phase setting
  Int ->
  m [Int]
runAmp prog input phase =
  liftEither (fmap fst (runIntcodeST (phase:input) prog))

-- | Connect together a sequence of amplifiers, using the given phase
-- settings. The first amplifier gets the input provided, the second
-- amplifier gets the output of the first, and so on. Yield the output
-- from the final amplifier.
runAmpSequence ::
  MonadError String m =>
  -- | Amplifier program
  UArray Addr Int ->
  -- | Input
  [Int] ->
  -- | Phase settings
  NonEmpty Int ->
  -- | Outputs
  m [Int]
runAmpSequence prog input phases =
  go (NonEmpty.toList phases) input
  where
    go (p:ps) i =
      go ps =<< runAmp prog i p
    go [] i =
      pure i

-- | Convert list to program memory for ST intcode interpreter.
mkArr ::
  [Int] -> UArray Addr Int
mkArr lst =
  listArray (0,fromIntegral (length lst - 1)) lst

-- | All legal phase settings combinations for the five amplifiers.
allPhaseSettings :: [NonEmpty Int]
allPhaseSettings =
  -- we know the inner lists are non-empty, because we have hard coded
  -- them to contain exactly five elements.
  NonEmpty.fromList <$> permutations [0..4]

-- | Find the phase setting that maximizes the amplifier sequence
-- output by brute forcing the phase parameters.
findMax :: UArray Addr Int -> [Int] -> [NonEmpty Int] -> Either String (NonEmpty Int, Int)
findMax prog input phases =
  fmap (maximumBy (compare `on` snd))
  $ fmap (fmap (\(phases', outputs) -> (phases', head outputs)))
  $ sequence
  $ map (\phase -> (fmap (phase,) (runAmpSequence prog input phase))) phases

lopsidedZip :: NonEmpty a -> [b] -> [(a,b)]
lopsidedZip as bs =
  go as bs
  where
    go (x:|xs) (y:ys) = (x,y) : zip xs ys
    go _ [] = []

-- | Connect amplifier to input and output channel. Write result to mvar.
hookupAmp :: UArray Addr Int -> (MVar (Either String ()), (Chan Int, Chan Int)) -> IO ()
hookupAmp prog (resRef, (inChan, outChan)) = do
  thaw prog
  >>= runIntcodeIO (readChan inChan) (writeChan outChan)
  >>= putMVar resRef

-- | Connect together a sequence of amplifiers, using the given phase
-- settings. The first amplifier gets the input provided, the second
-- amplifier gets the output of the first, and so on. Additionally,
-- the output of the last amplifier gets looped back to the front as
-- more input for the first amplifier.
--
-- Yield the final output from the last amplifier when the amplifier
-- programs have halted.
runAmpSequenceWithFeedback :: UArray Addr Int -> [Int] -> NonEmpty Int -> IO (Either String Int)
runAmpSequenceWithFeedback prog initialInput phases = do
  -- Each amp gets an input channel. The input for the last amp
  -- connects to the output of the amp before it, all the way up to
  -- the first amp, which gets its input from the output of the last
  -- amp.
  chans <-
    mapM (const newChan) phases
  ampResults <-
    mapM (const newEmptyMVar) phases
  let
    firstInputChan =
      NonEmpty.head chans

    inputChans =
      chans <> pure firstInputChan

    -- The input and output channels for each amp.
    ampChans =
      lopsidedZip inputChans (NonEmpty.tail inputChans)

    ampConfigs =
      lopsidedZip ampResults ampChans

  -- Hook up amps to inputs and outputs and run each in a separate
  -- thread.
  mapM_ (forkIO . hookupAmp prog) ampConfigs

  -- Supply the phase settings to each amp.
  for_ (NonEmpty.zip phases chans) $ \(ph, inChan) -> do
    writeChan inChan ph

  -- The first amp gets the initial input as well.
  mapM_ (writeChan firstInputChan) initialInput

  -- Getting the output is a bit icky, because the only way we have of
  -- knowing that the amp programs have stopped running is to se if
  -- reading from the output channel of the final amplifier fails. We
  -- catch the exception and yield the last value we were able to get
  -- out of there.
  --
  -- Remember, the input to the first amp is also the output of the
  -- final amp, so that's where we'll find the final output. We must
  -- also make sure to use a duplicate of the channel, otherwise we
  -- would be in a race with the first amplifier and take each others
  -- values.
  resultChan <-
    dupChan firstInputChan
  let
    drainResults v = do
      res <- do
        catch
          (Just <$> readChan resultChan)
          (\(_e :: BlockedIndefinitelyOnMVar) -> pure Nothing)
      maybe (pure v) (drainResults . Right) res
  signal <-
    drainResults (Left "No signal") :: IO (Either String Int)

  res <-
    sequence <$> mapM takeMVar ampResults
    :: IO (Either String (NonEmpty ()))

  -- The final result is either a runtime error found in 'ampResults'
  -- or the signal we hopefully recovered from 'resultChan'.
  pure (res >>= const signal)

-- | All legal phase settings combinations for the five amplifiers
-- when used in feedback configuration.
allFeedbackPhaseSettings :: [NonEmpty Int]
allFeedbackPhaseSettings =
  -- we know the inner lists are non-empty, because we have hard coded
  -- them to contain exactly five elements.
  NonEmpty.fromList <$> permutations [5..9]

-- | Find the phase setting that maximizes the amplifier sequence
-- output in feedback mode by brute forcing the phase parameters.
findMaxWithFeedback :: UArray Addr Int -> [Int] -> [NonEmpty Int] -> IO (Either String (NonEmpty Int, Int))
findMaxWithFeedback prog input phases =
  select <$> generate
  where
    generate = do
      fmap partitionEithers $ do
        for phases $ \pha -> do
          fmap (pha,) <$> runAmpSequenceWithFeedback prog input pha
    select ([], ls) =
      Right (maximumBy (compare `on` snd) ls)
    select (errs, _) =
      Left (unlines errs)

main :: IO ()
main = do
  either error pure =<< runExceptT run

run :: (MonadError String m, MonadIO m) => m ()
run = do

  (phases, signal) <-
    liftEither (findMax thrusterProg [0] allPhaseSettings)
  liftIO $ putStrLn $
    "Maximum thruster signal is " ++ show signal
    ++ ", using phases " ++ show (NonEmpty.toList phases)

  (phases', signal') <-
    liftIO (findMaxWithFeedback thrusterProg [0] allFeedbackPhaseSettings)
    >>= liftEither
  liftIO $ putStrLn $
    "Maximum thruster signal with feedback is "
    ++ show signal' ++ ", using phases "
    ++ show (NonEmpty.toList phases')

thrusterProg :: UArray Addr Int
thrusterProg =
  mkArr [3,8,1001,8,10,8,105,1,0,0,21,38,59,84,97,110,191,272,353,434,99999,3,9,1002,9,2,9,101,4,9,9,1002,9,2,9,4,9,99,3,9,102,5,9,9,1001,9,3,9,1002,9,5,9,101,5,9,9,4,9,99,3,9,102,5,9,9,101,5,9,9,1002,9,3,9,101,2,9,9,1002,9,4,9,4,9,99,3,9,101,3,9,9,1002,9,3,9,4,9,99,3,9,102,5,9,9,1001,9,3,9,4,9,99,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,99]
