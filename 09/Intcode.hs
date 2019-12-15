{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}

module Intcode
  (
    -- * Actions language for implementing intcode interpreters.
    Store(..)
  , RelativeBase(..)
  , Input(input)
  , Output(output)

    -- * Abstract intcode machine interpreter.
  , adjustRelativeBase
  , Mode(..)
  , getIndir
  , putIndir
  , getModal
  , putModal
  , Op
  , decodeOpAt
  , interpretOp
  , execute

  -- * Concrete implementation.
  , IMInterpreter(..)
  , IMEnv(..)
  , runIMInterpreter
  , runTracingIMInterpreter

  ) where

import Data.Functor

import qualified Data.Sequence as Seq
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State as State
import qualified Data.IntMap as IntMap

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

class RelativeBase addr m | m -> addr where

  -- | Set the relative base address.
  getRelativeBase :: m addr

  -- | Get the current relative base address.
  putRelativeBase :: addr -> m ()

class Input val m | m -> val where

  -- | Read from an input stream.
  input :: m val

class Output val m | m -> val where

  -- | Write to an output stream.
  output :: val -> m ()

class Trace m where

  -- | Write program execution trace message.
  traceMsg :: String -> m ()




--
-- Abstract intcode machine interpreter.
--

-- | Adjust relative base address by adding offset.
adjustRelativeBase ::
  (Num addr, Monad m, RelativeBase addr m) => addr -> m ()
adjustRelativeBase offset =
  putRelativeBase . (offset +) =<< getRelativeBase

-- | Addressing mode: position, immediate or relative.
data Mode =
  Pos | Imm | Rel
  -- Careful! We depend on the stock Enum instance mapping Pos to zero
  -- and Imm to one.
  deriving (Enum, Show)

-- | Indirect get. Read value from address held at the given
-- location. Requires that addresses and values are represented the
-- same way.
getIndir ::
  ( Integral val
  , Num loc
  , Monad m
  , Store loc val m
  ) => loc -> m val
getIndir loc =
  get . fromIntegral =<< get loc

-- | Indirect put. Store value at address held at the given location.
-- Requires that addresses and values are represented the same way.
putIndir ::
  ( Integral val
  , Num loc
  , Monad m
  , Store loc val m
  ) => loc -> val -> m ()
putIndir loc val =
  flip put val . fromIntegral =<< get loc

-- | Version of 'get' that respects the addressing mode.
getModal ::
  ( Integral val
  , Monad m
  , Num loc
  , RelativeBase loc m
  , Store loc val m
  ) => (Mode, loc) -> m val
getModal (mode, addr) =
  case mode of
   Pos ->
     getIndir addr
   Imm ->
     get addr
   Rel -> do
     rb <-
       getRelativeBase
     addr' <-
       fromIntegral <$> get addr
     get (addr' + rb)

-- | Version of 'put' that respects the addressing mode. Will fail if
-- the storage address uses immediate mode.
putModal ::
  ( Integral val
  , Monad m
  , Num loc
  , Except.MonadError String m
  , RelativeBase loc m
  , Show loc
  , Show val
  , Store loc val m
  ) => (Mode, loc) -> val -> m ()
putModal (mode, addr) val =
  case mode of
   Pos ->
     putIndir addr val
   Imm ->
     Except.throwError $
     "Trying to write with immediate mode " ++ show val ++ "@imm "
     ++ show addr
   Rel -> do
     rb <-
       getRelativeBase
     addr' <-
       fromIntegral <$> get addr
     put (addr' + rb) val

-- | Helper for implementing binary operators that read their values
-- from the addresses held in the first to locations, then store to
-- the address in the third location.  Requires that addresses and
-- values are represented the same way.
binOp ::
  ( Integral val
  , Monad m
  , Num loc
  , Except.MonadError String m
  , RelativeBase loc m
  , Show loc
  , Show val
  , Store loc val m
  ) => (val -> val -> val) -> (Mode, loc) -> (Mode, loc) -> (Mode, loc) -> m ()
binOp op r1 r2 w = do
  putModal w =<< (op <$> getModal r1 <*> getModal r2)

-- | Machine operations.
data Op addr

  -- | Args r1 r2 w.  Sum vals at r1 and r2, store at w.
  = Add (Mode, addr) (Mode, addr) (Mode, addr)

  -- | Args r1 r2 w: multiply vals at r1 and r2, store at w.
  | Mul (Mode, addr) (Mode, addr) (Mode, addr)

  -- | Store input at addr.
  | Input (Mode, addr)

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
  | LessThan (Mode, addr) (Mode, addr) (Mode, addr)

  -- | Compare first and second parameters. Write results to third
  -- parameter. If p1 = p2 then write 1 else write 0.
  | Equals (Mode, addr) (Mode, addr) (Mode, addr)

  -- | Adjust relative base by offset.
  | AdjRelBase (Mode, addr)

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
          fromIntegral val `divMod` 100 :: (val, val)
        modes =
          decodeAddressingModeCode mcode
        getMode n =
          modes !! (n-1)
        par n =
          (getMode n, ip + fromIntegral n)
      case opcode of
        1 ->
          pure (Add (par 1) (par 2) (par 3), Just (ip+4))
        2 ->
          pure (Mul (par 1) (par 2) (par 3), Just (ip+4))
        3 ->
          pure (Input (par 1), Just (ip+2))
        4 ->
          pure (Output (par 1), Just (ip+2))
        5 ->
          pure (JumpIfTrue (par 1) (par 2), Just (ip+3))
        6 ->
          pure (JumpIfFalse (par 1) (par 2), Just (ip+3))
        7 ->
          pure (LessThan (par 1) (par 2) (par 3), Just (ip+4))
        8 ->
          pure (Equals (par 1) (par 2) (par 3), Just (ip+4))
        9 ->
          pure (AdjRelBase (par 1), Just (ip+2))
        99 ->
          pure (Halt, Nothing)
        _ ->
          Except.throwError $
          "Invalid opcode " ++ show opcode ++ "@"
          ++ show ip

data AndThen addr = Continue | Stop | Goto addr

interpretOp ::
  ( Eq val
  , Input val m
  , Integral val
  , Monad m
  , Except.MonadError String m
  , Num addr
  , Num val
  , Output val m
  , Ord val
  , RelativeBase addr m
  , Show addr
  , Show val
  , Store addr val m
  ) => Op addr -> m (AndThen addr)
interpretOp = \case
  Add r1 r2 w ->
    binOp (+) r1 r2 w $> Continue
  Mul r1 r2 w ->
    binOp (*) r1 r2 w $> Continue
  Input addr ->
    (putModal addr =<< input) $> Continue
  Output addr ->
    (output =<< getModal addr) $> Continue
  JumpIfTrue test ip -> do
    val <-
      getModal test
    if val == 0
      then pure Continue
      else Goto . fromIntegral <$> getModal ip
  JumpIfFalse test ip -> do
    val <-
      getModal test
    if val == 0
      then Goto . fromIntegral <$> getModal ip
      else pure Continue
  LessThan p1 p2 w -> do
    v1 <-
      getModal p1
    v2 <-
      getModal p2
    putModal w (if v1 < v2 then 1 else 0) $> Continue
  Equals p1 p2 w -> do
    v1 <-
      getModal p1
    v2 <-
      getModal p2
    putModal w (if v1 == v2 then 1 else 0) $> Continue
  AdjRelBase p1 -> do
    offset <-
      fromIntegral <$> getModal p1
    adjustRelativeBase offset $> Continue
  Halt ->
    pure Stop

-- | Execute program in storage, starting from the given address.
execute ::
  ( Except.MonadError String m
  , Input val m
  , Integral val
  , Num addr
  , Output val m
  , RelativeBase addr m
  , Show addr
  , Show val
  , Store addr val m
  , Trace m
  ) =>
  addr -> m ()
execute ip0 =
  go =<< (traceMsg ("ip0: " ++ show ip0) *> decodeOpAt ip0)
  where
    go (op, mIp) = do
      traceMsg ("op: " ++ show op ++ ", nextIp: " ++ show mIp)
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
-- A pure implementation using IntMap-backed memory (supporting very
-- fragmented memory operations) and lists for input and output.
--

data IMEnv val =
  IMEnv
  { imMemory :: !(IntMap.IntMap val)
  , imRelativeBase :: !Int
  , imInput :: [val]
  , imOutput :: [val]
  , imTrace :: !(Seq.Seq String)
  }
  deriving (Eq, Read, Show)

runTracingIMInterpreter ::
  forall val.
  ( Integral val
  , Show val
  ) =>
  -- | Program code, in sequence, starting from address zero.
  [val] ->
  -- | Initial relative base. Usually zero.
  Int ->
  -- | Program start address. Usually zero.
  Int ->
  -- | Inputs.
  [val] ->
  -- | Resulting interpreter state, including program memory and
  -- outputs.
  Either String (IMEnv val)
runTracingIMInterpreter prog relativeBase startAddr input' =
  Except.runExcept
  $ flip State.execStateT env
  $ runIMInterpreterInner
  $ (execute startAddr :: IMInterpreter 'Tracing val ())
  where
    env =
      IMEnv mem relativeBase input' mempty mempty
    mem =
      IntMap.fromList (zip [0..] prog)

runIMInterpreter ::
  forall val.
  ( Integral val
  , Show val
  ) =>
  -- | Program code, in sequence, starting from address zero.
  [val] ->
  -- | Initial relative base. Usually zero.
  Int ->
  -- | Program start address. Usually zero.
  Int ->
  -- | Inputs.
  [val] ->
  -- | Resulting interpreter state, including program memory and
  -- outputs.
  Either String (IMEnv val)
runIMInterpreter prog relativeBase startAddr input' =
  Except.runExcept
  $ flip State.execStateT env
  $ runIMInterpreterInner
  $ (execute startAddr :: IMInterpreter 'NotTracing val ())
  where
    env =
      IMEnv mem relativeBase input' mempty mempty
    mem =
      IntMap.fromList (zip [0..] prog)

data TraceFlag = Tracing | NotTracing

newtype IMInterpreter (trc :: TraceFlag) val a =
  IMInterpreter
  { runIMInterpreterInner ::
      State.StateT (IMEnv val) (Except.Except String) a
  }
  deriving
    ( Applicative
    , Except.MonadError String
    , Functor
    , Monad
    , State.MonadState (IMEnv val)
    )

instance
  ( Integral val
  , Show val
  , Trace (IMInterpreter trc val)
  ) =>
  Store Int val (IMInterpreter trc val) where
  get addr = do
    val <-
      maybe 0 id . IntMap.lookup addr <$> State.gets imMemory
    traceMsg ("get val " ++ show val ++ "@" ++ show addr)
    pure val

  put addr val = do
    traceMsg ("put val " ++ show val ++ "@" ++ show addr)
    State.modify $ \env ->
      env { imMemory = IntMap.insert addr val (imMemory env) }

instance
  ( Trace (IMInterpreter trc val)
  ) =>
  RelativeBase Int (IMInterpreter trc val) where
  getRelativeBase = do
    rb <-
      State.gets imRelativeBase
    traceMsg ("get rb " ++ show rb)
    pure rb

  putRelativeBase rb = do
    traceMsg ("put rb " ++ show rb)
    State.modify $ \env -> env { imRelativeBase = rb }

instance
  ( Show val
  , Trace (IMInterpreter trc val)
  ) =>
  Input val (IMInterpreter trc val) where
  input = do
    vals <-
      State.gets imInput
    case vals of
      [] ->
        Except.throwError "No input."
      (v:vs) -> do
        traceMsg ("input " ++ show v)
        State.modify $ \env -> env { imInput = vs }
        pure v

instance
  ( Show val
  , Trace (IMInterpreter trc val)
  ) =>
  Output val (IMInterpreter trc val) where
  output val = do
    traceMsg ("output " ++ show val)
    State.modify $ \env -> env { imOutput = (imOutput env) ++ pure val }

instance Trace (IMInterpreter 'Tracing val) where
  traceMsg msg =
    State.modify $ \env -> env { imTrace = (imTrace env) Seq.|> msg }

instance Trace (IMInterpreter 'NotTracing val) where
  traceMsg =
    const (pure ())
