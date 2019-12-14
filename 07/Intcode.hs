{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}

module Intcode
  (
    -- * Actions language for implementing intcode interpreters.
    Store(..)
  , getIndir
  , putIndir
  , Input(input)
  , Output(output)

    -- * Abstract intcode machine interpreter.
  , Mode(..)
  , Op
  , decodeOpAt
  , interpretOp
  , execute

    -- * Concrete implementations.
  , Addr(..)
  , STEnv(..)
  , STInterpreter(..)
  , runIntcodeST
  , IOEnv(..)
  , IOInterpreter(..)
  , runIntcodeIO

    -- * Very basic testing.
  , testsPassed
  ) where

import Control.Monad.ST
import Data.Coerce
import Data.Functor
import Data.Ix
import Data.STRef

import qualified Control.Monad.Except as Except
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans as Trans
import qualified Data.Array.IArray as IArray
import qualified Data.Array.IO as IOArray
import qualified Data.Array.MArray as MArray
import qualified Data.Array.ST as STArray
import qualified Data.Array.Unboxed as UArray

--
-- Actions language for implementing Intcode interpreters.
--

-- | Actions on mutable memory. Not allowing for errors in adressing,
-- so any access that is out-of-bounds should probably raise an
-- 'IndexOutOfBounds' exception.
class Store addr val m | m -> addr, m -> val where

  -- | Get value at given address.
  get :: addr -> m val

  -- | Store value at address.
  put :: addr -> val -> m ()

-- | Indirect get. Read value from address held at the given
-- location. Requires that addresses and values are represented the
-- same way.
getIndir ::
  ( Monad m
  , Store loc val m
  , Coercible loc val
  ) => loc -> m val
getIndir loc =
  get . coerce =<< get loc

-- | Indirect put. Store value at address held at the given location.
-- Requires that addresses and values are represented the same way.
putIndir ::
  ( Monad m
  , Store loc val m
  , Coercible loc val
  ) => loc -> val -> m ()
putIndir loc val =
  flip put val . coerce =<< get loc

class Input m val | m -> val where

  -- | Read from an input stream.
  input :: m val

class Output m val | m -> val where

  -- | Write to an output stream.
  output :: val -> m ()




--
-- Abstract intcode machine interpreter.
--

-- | Addressing mode: position or immediate.
data Mode =
  Pos | Imm
  -- Careful! We depend on the stock Enum instance mapping Pos to zero
  -- and Imm to one.
  deriving (Enum, Show)

-- | Version of 'get' that respects the addressing mode.
getModal ::
  ( Monad m
  , Store loc val m
  , Coercible loc val
  ) => (Mode, loc) -> m val
getModal (mode, addr) =
  case mode of
   Pos ->
     getIndir addr
   Imm ->
     get addr

-- | Helper for implementing binary operators that read their values
-- from the addresses held in the first to locations, then store to
-- the address in the third location.  Requires that addresses and
-- values are represented the same way.
binOp ::
  ( Monad m
  , Store loc val m
  , Coercible loc val
  ) => (val -> val -> val) -> (Mode, loc) -> (Mode, loc) -> loc -> m ()
binOp op r1 r2 w = do
  putIndir w =<< (op <$> getModal r1 <*> getModal r2)

-- | Machine operations.
data Op addr

  -- | Args r1 r2 w.  Sum vals at r1 and r2, store at w.
  = Add (Mode, addr) (Mode, addr) addr

  -- | Args r1 r2 w: multiply vals at r1 and r2, store at w.
  | Mul (Mode, addr) (Mode, addr) addr

  -- | Store input at addr.
  | Input addr

  -- | Write output found at addr.
  | Output (Mode, addr)

  -- | Set instruction pointer to second parameter if first parameter
  -- is non-zero. Otherwise do nothing.
  | JumpIfTrue (Mode, addr) (Mode, addr)

  -- | Set instruction pointer to second parameter if first parameter
  -- is zero. Otherwise do nothing.
  | JumpIfFalse (Mode, addr) (Mode, addr)

  -- | Compare first and second parameters. Write results to third
  -- parameter. If p1 < p2 then write 1 else write 0.
  | LessThan (Mode, addr) (Mode, addr) addr

  -- | Compare first and second parameters. Write results to third
  -- parameter. If p1 = p2 then write 1 else write 0.
  | Equals (Mode, addr) (Mode, addr) addr

  -- | Stop execution.
  | Halt

  deriving Show

-- | List the digits of an integer, in reverse order order (from right
-- to left).
--
-- >>> reverseDigits 1002
-- [2,0,0,1]
--
reverseDigits :: Integral n => n -> [n]
reverseDigits =
  go
  where
    go =
      check . chop
    chop n =
      n `divMod` 10
    check (n, d)
      | n == 0    = pure d
      | otherwise = d : go n

-- | Decode adressing mode digits into list of adressing mode for all
-- opcode parameters, starting at param 1 and going to infinity.
decodeAddressingModeCode :: Integral n => n -> [Mode]
decodeAddressingModeCode mcode =
  (toEnum . fromIntegral <$> reverseDigits mcode)
  ++ repeat Pos

-- | Decode opcode at given address. Yield the operation and the
-- updated instruction pointer.
decodeOpAt ::
  forall m addr val.
  ( Eq val
  , Except.MonadError String m
  , Integral val
  , Monad m
  , Num addr
  , Num val
  , Show addr
  , Show val
  , Store addr val m
  ) => addr -> m (Op addr, Maybe addr)
decodeOpAt ip = do
      val <-
        get ip
      let
        (mcode, opcode) =
          fromIntegral val `divMod` 100
        modes =
          decodeAddressingModeCode mcode
        getMode n =
          modes !! (n-1)
        par n =
          (getMode n, ip + fromIntegral n)
      case opcode of
        (1 :: val) ->
          pure (Add (par 1) (par 2) (ip+3), Just (ip+4))
        2 ->
          pure (Mul (par 1) (par 2) (ip+3), Just (ip+4))
        3 ->
          pure (Input (ip+1), Just (ip+2))
        4 ->
          pure (Output (par 1), Just (ip+2))
        5 ->
          pure (JumpIfTrue (par 1) (par 2), Just (ip+3))
        6 ->
          pure (JumpIfFalse (par 1) (par 2), Just (ip+3))
        7 ->
          pure (LessThan (par 1) (par 2) (ip+3), Just (ip+4))
        8 ->
          pure (Equals (par 1) (par 2) (ip+3), Just (ip+4))
        99 ->
          pure (Halt, Nothing)
        _ ->
          Except.throwError $
          "Invalid opcode " ++ show opcode ++ "@"
          ++ show ip

data AndThen addr = Continue | Stop | Goto addr

interpretOp ::
  ( Coercible addr val
  , Eq val
  , Input m val
  , Monad m
  , Num val
  , Output m val
  , Ord val
  , Store addr val m
  ) => Op addr -> m (AndThen addr)
interpretOp = \case
  Add r1 r2 w ->
    binOp (+) r1 r2 w $> Continue
  Mul r1 r2 w ->
    binOp (*) r1 r2 w $> Continue
  Input addr ->
    (putIndir addr =<< input) $> Continue
  Output addr ->
    (output =<< getModal addr) $> Continue
  JumpIfTrue test ip -> do
    val <-
      getModal test
    if val == 0
      then pure Continue
      else Goto . coerce <$> getModal ip
  JumpIfFalse test ip -> do
    val <-
      getModal test
    if val == 0
      then Goto . coerce <$> getModal ip
      else pure Continue
  LessThan p1 p2 w -> do
    v1 <-
      getModal p1
    v2 <-
      getModal p2
    putIndir w (if v1 < v2 then 1 else 0) $> Continue
  Equals p1 p2 w -> do
    v1 <-
      getModal p1
    v2 <-
      getModal p2
    putIndir w (if v1 == v2 then 1 else 0) $> Continue
  Halt ->
    pure Stop

-- | Execute program in storage, starting from the given address.
execute ::
  ( Coercible addr val
  , Except.MonadError String m
  , Input m val
  , Integral val
  , Num addr
  , Output m val
  , Show addr
  , Show val
  , Store addr val m
  ) =>
  addr -> m ()
execute ip0 =
  go =<< decodeOpAt ip0
  where
    go (op, mIp) = do
      c <-
        interpretOp op
      case (c, mIp) of

        (Continue, Just ip) ->
          decodeOpAt ip >>= go

        (Stop, Nothing) ->
          pure ()

        (Goto ip, _) ->
          decodeOpAt ip >>= go

        -- These two error modes should be theoretical; they only
        -- surface if 'decodeOpAt' and 'interpretOp' are mismatched.
        (Stop, Just ip) ->
          Except.throwError $
          "Internal decoder or interpreter error: halted, but still "
          ++ "has ip for next op (@" ++ show ip ++ ")."
        (Continue, Nothing) ->
          Except.throwError $
          "Internal decoder or interpreter error: ip for next op "
          ++ "doesn't exist."




--
-- Two needlessly complex interpreter implementations: IO and ST.
--

data IOEnv addr val =
  IOEnv
  { ioEnvMem :: !(IOArray.IOArray addr val)
  , ioEnvStdin :: IO val
  , ioEnvStdout :: val -> IO ()
  }

newtype IOInterpreter addr val a =
  IOInterpreter
  { runIOInterpreter
    :: Reader.ReaderT
       (IOEnv addr val)
       (Except.ExceptT String IO)
       a
  }
  deriving
    ( Applicative
    , Functor
    , Monad
    , Reader.MonadReader (IOEnv addr val)
    , Except.MonadError String
    , Trans.MonadIO
    )

runIOInterpreter'
  :: IOEnv addr val
  -> IOInterpreter addr val ()
  -> IO (Either String ())
runIOInterpreter' env =
  Except.runExceptT
  . flip Reader.runReaderT env
  . runIOInterpreter

instance Ix addr => Store addr val (IOInterpreter addr val) where
  get addr = do
    IOInterpreter $ do
      arr <-
        Reader.asks ioEnvMem
      (Trans.lift . Trans.lift) (MArray.readArray arr addr)
  put addr val = do
    IOInterpreter $ do
      arr <-
        Reader.asks ioEnvMem
      (Trans.lift . Trans.lift) (MArray.writeArray arr addr val)

instance Input (IOInterpreter addr val) val where
  input =
    IOInterpreter (Trans.lift . Trans.lift =<< Reader.asks ioEnvStdin)

instance Output (IOInterpreter addr val) val where
  output val = do
    IOInterpreter $ do
      Trans.lift . Trans.lift =<< Reader.asks ioEnvStdout <*> pure val






data STEnv s addr val =
  STEnv
  { stEnvMem :: STArray.STUArray s addr val
  , stEnvStdin :: ST s val
  , stEnvStdout :: val -> ST s ()
  , stEnvStdoutDrain :: ST s [val]
  }

newtype STInterpreter s addr val a =
  STInterpreter
  { runSTInterpreter
    :: Reader.ReaderT
       (STEnv s addr val)
       (Except.ExceptT String (ST s))
       a
  }
  deriving
    ( Applicative
    , Functor
    , Monad
    , Reader.MonadReader (STEnv s addr val)
    , Except.MonadError String
    )

runSTInterpreter' ::
  ( Ix addr
  , MArray.MArray (STArray.STUArray s) val (ST s)
  , IArray.IArray arr val
  ) =>
  STEnv s addr val ->
  STInterpreter s addr val () ->
  ST s (Either String ([val], arr addr val))
runSTInterpreter' env prog = do
  res <-
    Except.runExceptT
    (Reader.runReaderT
      (runSTInterpreter prog)
      env)
  case res of
    Left err ->
      pure (Left err)
    Right () -> do
      outputs <-
        stEnvStdoutDrain env
      mem <-
        MArray.freeze (stEnvMem env)
      pure (Right (outputs, mem))

mkSTEnv ::
  ( Ix addr
  , IArray.IArray arr val
  , MArray.MArray (STArray.STUArray s) val (ST s))
  =>
  arr addr val ->
  [val] ->
  ST s (STEnv s addr val)
mkSTEnv mem0 inputs =
  newSTRef [] >>= \outRef ->
  (STEnv
   <$> mkMem
   <*> mkInputStream
   <*> pure (nextOutput outRef)
   <*> pure (outDrain outRef))

  where
    mkMem =
      MArray.thaw mem0

    mkInputStream =
      nextInput <$> newSTRef inputs

    nextInput ref = do
      inVal <-
        readSTRef ref
      case inVal of
        (v:vs) ->
          writeSTRef ref vs *> pure v
        [] ->
          error "Input list is empty!"

    nextOutput ref val =
      modifySTRef' ref (val:)

    outDrain ref =
      readSTRef ref <* writeSTRef ref []

genGetSTI ::
  ( MArray.MArray (STArray.STUArray s) val (ST s)
  , Ix addr
  ) =>
  addr -> STInterpreter s addr val val
genGetSTI addr = do
  STInterpreter $ do
    arr <-
      Reader.asks stEnvMem
    (Trans.lift . Trans.lift) (MArray.readArray arr addr)

genPutSTI ::
  ( MArray.MArray (STArray.STUArray s) val (ST s)
  , Ix addr
  ) =>
  addr -> val -> STInterpreter s addr val ()
genPutSTI addr val = do
  STInterpreter $ do
    arr <-
      Reader.asks stEnvMem
    (Trans.lift . Trans.lift) (MArray.writeArray arr addr val)

instance Ix addr => Store addr Int (STInterpreter s addr Int) where
  get =
    genGetSTI
  put =
    genPutSTI

instance Input (STInterpreter s addr val) val where
  input =
    STInterpreter (Trans.lift . Trans.lift =<< Reader.asks stEnvStdin)

instance Output (STInterpreter s addr val) val where
  output val = do
    STInterpreter $ do
      Trans.lift . Trans.lift =<< Reader.asks stEnvStdout <*> pure val




--
-- Finally: Actually useful interpreters
--

-- | The type of addresses in Incode programs.
newtype Addr =
  Addr Int
  deriving (Eq, Ix, Ord, Num, Read, Show)

-- | Run IO version of Intcode interpreter.
runIntcodeIO ::
  -- | Infinite input stream, or at least enough for the program to
  -- execute.
  IO Int ->
  -- | Output stream.
  (Int -> IO ()) ->
  -- | Mutable memory containing program.
  IOArray.IOArray Addr Int ->
  -- | Results is either an error string or just unit, having
  -- performed the effects and modified the memory during execution.
  IO (Either String ())
runIntcodeIO inputStr outputStr mem =
  runIOInterpreter' (IOEnv mem inputStr outputStr) (execute 0)

-- | Run ST version of Intcode interpreter.
runIntcodeST ::
  -- | Infinite input stream, or at least enough for the program to
  -- execute.
  [Int] ->
  -- | Initial memory containing program.
  UArray.UArray Addr Int ->
  -- | Results is either an error string or the output stream as well
  -- as the final memory state.
  Either String ([Int], UArray.UArray Addr Int)
runIntcodeST inputs mem0 = do
  runST $ do
    env <-
      mkSTEnv mem0 inputs
    runSTInterpreter' env (execute 0)




--
-- Example runs
--

prog1 :: Either String ([Int], UArray.UArray Addr Int)
prog1 =
  runIntcodeST [] (UArray.listArray (0,4) [1002,4,3,4,33])

prog1ExpectedResults :: Either String ([Int], UArray.UArray Addr Int)
prog1ExpectedResults =
  Right ([], (UArray.listArray (0,4) [1002,4,3,4,99]))

testProg1 :: Bool
testProg1 =
  prog1 == prog1ExpectedResults

testsPassed :: Bool
testsPassed =
  and [testProg1]
