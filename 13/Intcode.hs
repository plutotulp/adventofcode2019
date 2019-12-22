{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language OverloadedLists #-}
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
  , decodeOp
  , interpretOp
  , execute

  -- * Concrete implementation.
  , TraceFlag(..)
  , IMInterpreter(..)
  , IMEnv(..)
  , initIMEnv

  , runIntcodeInterpreter
  , stepIntcode

  , runIntcodeInterpreterTracing
  , stepIntcodeTracing

  -- * Utilities
  , readIntcodeProg

  ) where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Control.Monad.Except (MonadError)
import qualified Control.Monad.Except as Except
import Control.Monad.State (MonadState)
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

-- | Manipulate instruction pointer.
class Ip addr m | m -> addr where
  getIp :: m addr
  setIp :: addr -> m ()



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
  -- Careful! We depend on the stock Enum instance starting at zero
  -- and mapping to constructors in the order given (Pos <-> 0, Imm
  -- <-> 1, Rel <-> 2).
  deriving (Enum, Eq, Show)

-- | Modal read-only address. Modes are absolute position, immediate
-- value or relative to Relative Base Address).
data ModalReadAddr addrOrVal =
  PosReadAddr addrOrVal | ImmVal addrOrVal | RelReadAddr addrOrVal
  deriving (Eq, Show)

toModalReadAddr ::
  Show addrOrVal => Mode -> addrOrVal -> ModalReadAddr addrOrVal
toModalReadAddr mode =
  case mode of
    Pos ->
      PosReadAddr
    Imm ->
      ImmVal
    Rel ->
      RelReadAddr

-- | Modal write-only address. Two modes: absolute position or
-- relative to Relative Base Address.
data ModalWriteAddr addr =
  PosWriteAddr addr | RelWriteAddr addr
  deriving (Eq, Show)

toModalWriteAddr ::
  MonadError String m
  =>
  Show addr => Mode -> addr -> m (ModalWriteAddr addr)
toModalWriteAddr mode addr =
  case mode of
    Pos ->
      pure (PosWriteAddr addr)
    Rel ->
      pure (RelWriteAddr addr)
    _ ->
      Except.throwError $
      "Invalid write mode " ++ show mode ++ " for @" ++ show addr

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
  ) => ModalReadAddr loc -> m val
getModal mloc =
  case mloc of
    PosReadAddr addr ->
      getIndir addr
    ImmVal val ->
      get val
    RelReadAddr addr -> do
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
  , MonadError String m
  , RelativeBase loc m
  , Show loc
  , Show val
  , Store loc val m
  ) => ModalWriteAddr loc -> val -> m ()
putModal modalAddr val =
  case modalAddr of
    PosWriteAddr addr ->
      putIndir addr val
    RelWriteAddr addr -> do
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
  , MonadError String m
  , RelativeBase loc m
  , Show loc
  , Show val
  , Store loc val m
  ) =>
  (val -> val -> val) ->
  ModalReadAddr loc ->
  ModalReadAddr loc ->
  ModalWriteAddr loc ->
  m ()
binOp op r1 r2 w = do
  putModal w =<< (op <$> getModal r1 <*> getModal r2)

-- | Machine operations.
data Op addr

  -- | Args r1 r2 w.  Sum vals at r1 and r2, store at w.
  = Add
    (ModalReadAddr addr)
    (ModalReadAddr addr)
    (ModalWriteAddr addr)
    addr

  -- | Args r1 r2 w: multiply vals at r1 and r2, store at w.
  | Mul
    (ModalReadAddr addr)
    (ModalReadAddr addr)
    (ModalWriteAddr addr)
    addr

  -- | Store input at addr.
  | Input (ModalWriteAddr addr) addr

  -- | Write output found at addr.
  | Output (ModalReadAddr addr) addr

  -- | Set instruction pointer to second parameter if first parameter
  -- is non-zero. Otherwise do nothing.
  | JumpIfTrue (ModalReadAddr addr) (ModalReadAddr addr) addr

  -- | Set instruction pointer to second parameter if first parameter
  -- is zero. Otherwise do nothing.
  | JumpIfFalse (ModalReadAddr addr) (ModalReadAddr addr) addr

  -- | Compare first and second parameters. Write results to third
  -- parameter. If p1 < p2 then write 1 else write 0.
  | LessThan
    (ModalReadAddr addr)
    (ModalReadAddr addr)
    (ModalWriteAddr addr)
    addr

  -- | Compare first and second parameters. Write results to third
  -- parameter. If p1 = p2 then write 1 else write 0.
  | Equals
    (ModalReadAddr addr)
    (ModalReadAddr addr)
    (ModalWriteAddr addr)
    addr

  -- | Adjust relative base by offset.
  | AdjRelBase (ModalReadAddr addr) addr

  -- | Stop execution.
  | Halt

  deriving (Eq, Show)

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

decodeOp ::
  forall m addr val.
  ( Eq val
  , MonadError String m
  , Integral val
  , Ip addr m
  , Monad m
  , Num addr
  , Num val
  , Show addr
  , Show val
  , Store addr val m
  ) => m (Op addr)
decodeOp = do
      ip <-
        getIp
      val <-
        get ip
      let
        (mcode, opcode) =
          fromIntegral val `divMod` 100 :: (val, val)
        modes =
          decodeAddressingModeCode mcode
        getMode n =
          modes !! (n-1)
        rpar n =
          pure (toModalReadAddr (getMode n) (ip + fromIntegral n))
        wpar n =
          toModalWriteAddr (getMode n) (ip + fromIntegral n)

      case opcode of
        1 -> do
          Add <$> rpar 1 <*> rpar 2 <*> wpar 3 <*> pure (ip+4)
        2 ->
          Mul <$> rpar 1 <*> rpar 2 <*> wpar 3 <*> pure (ip+4)
        3 ->
          Input <$> wpar 1 <*> pure (ip+2)
        4 ->
          Output <$> rpar 1 <*> pure (ip+2)
        5 ->
          JumpIfTrue <$> rpar 1 <*> rpar 2 <*> pure (ip+3)
        6 ->
          JumpIfFalse <$> rpar 1 <*> rpar 2 <*> pure (ip+3)
        7 ->
          LessThan <$> rpar 1 <*> rpar 2 <*> wpar 3 <*> pure (ip+4)
        8 ->
          Equals <$> rpar 1 <*> rpar 2 <*> wpar 3 <*> pure (ip+4)
        9 ->
          AdjRelBase <$> rpar 1 <*> pure (ip+2)
        99 ->
          pure Halt
        _ ->
          Except.throwError $
          "Invalid opcode " ++ show opcode ++ "@"
          ++ show ip

interpretOp ::
  ( Eq val
  , Input val m
  , Integral val
  , Ip addr m
  , Monad m
  , MonadError String m
  , Num addr
  , Num val
  , Output val m
  , Ord val
  , RelativeBase addr m
  , Show addr
  , Show val
  , Store addr val m
  ) => Op addr -> m ()
interpretOp = \case
  Add r1 r2 w ip ->
    binOp (+) r1 r2 w *> setIp ip
  Mul r1 r2 w ip ->
    binOp (*) r1 r2 w *> setIp ip
  Input addr ip ->
    (putModal addr =<< input) *> setIp ip
  Output addr ip ->
    (output =<< getModal addr) *> setIp ip
  JumpIfTrue test ipIfTrue ip -> do
    val <-
      getModal test
    case val == 0 of
      True ->
        setIp ip
      False ->
        setIp . fromIntegral =<< getModal ipIfTrue

  JumpIfFalse test ipIfFalse ip -> do
    val <-
      getModal test
    case val == 0 of
      True ->
        setIp . fromIntegral =<< getModal ipIfFalse
      False ->
        setIp ip
  LessThan p1 p2 w ip -> do
    v1 <-
      getModal p1
    v2 <-
      getModal p2
    putModal w (if v1 < v2 then 1 else 0) *> setIp ip
  Equals p1 p2 w ip -> do
    v1 <-
      getModal p1
    v2 <-
      getModal p2
    putModal w (if v1 == v2 then 1 else 0) *> setIp ip
  AdjRelBase p1 ip -> do
    offset <-
      fromIntegral <$> getModal p1
    adjustRelativeBase offset *> setIp ip
  Halt ->
    pure ()

execute ::
  ( MonadError String m
  , Eq addr
  , Input val m
  , Integral val
  , Ip addr m
  , Num addr
  , Output val m
  , RelativeBase addr m
  , Show addr
  , Show val
  , Store addr val m
  , Trace m
  ) =>
  m ()
execute =
  go
  where
    go = do
      op <- decodeOp
      traceMsg ("op: " ++ show op)
      interpretOp op
      if op == Halt then pure () else go

data StillRunning = Halted | NotDone
  deriving (Eq, Show)

executeUntilOutputOrHalted ::
  ( MonadError String m
  , Input val m
  , Integral val
  , Ip addr m
  , Num addr
  , Output val m
  , RelativeBase addr m
  , Show addr
  , Show val
  , Store addr val m
  , Trace m
  ) =>
  m StillRunning
executeUntilOutputOrHalted =
  go
  where
    go = do
      op <- decodeOp
      traceMsg ("op: " ++ show op)
      interpretOp op
      case op of
        Output _ _ ->
          pure NotDone
        Halt ->
          pure Halted
        _ ->
          go

--
-- A pure implementation using IntMap-backed memory (supporting very
-- fragmented memory).
--

data IMEnv val =
  IMEnv
  { imMemory :: !(IntMap.IntMap val)
  , imRelativeBase :: !Int
  , imInput :: [val]
  , imOutput :: !(Seq val)
  , imTrace :: !(Seq String)
  , imIp :: !Int
  }
  deriving (Eq, Read, Show)

data TraceFlag = Tracing | NotTracing

newtype IMInterpreter (trc :: TraceFlag) val a =
  IMInterpreter
  { runIMInterpreter ::
      State.StateT (IMEnv val) (Except.Except String) a
  }
  deriving
    ( Applicative
    , MonadError String
    , Functor
    , Monad
    , MonadState (IMEnv val)
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
    State.modify $ \env ->
      env { imOutput = (imOutput env) Seq.|> val }

instance Trace (IMInterpreter 'Tracing val) where
  traceMsg msg =
    State.modify $ \env ->
    env { imTrace = (imTrace env) Seq.|> msg }

instance Trace (IMInterpreter 'NotTracing val) where
  traceMsg =
    const (pure ())

instance
  Trace (IMInterpreter trc val) =>
  Ip Int (IMInterpreter trc val) where
  getIp =
    State.gets imIp

  setIp ip = do
    traceMsg ("set ip " ++ show ip)
    State.modify $ \env -> env { imIp = ip }

initIMEnv :: Foldable f => f val -> IMEnv val
initIMEnv prog =
  env
  where
    env =
      IMEnv mem 0 mempty mempty mempty 0
    mem =
      IntMap.fromList (zip [0..] (toList prog))

setIMInterpreterInput ::
  ( Show val
  , Trace (IMInterpreter trc val)
  ) =>
  [val] -> IMInterpreter trc val ()
setIMInterpreterInput input' = do
  traceMsg ("set input " ++ show input')
  State.modify $ \st -> st { imInput = input' }

takeIMInterpreterOutput ::
  ( Show val
  , Trace (IMInterpreter trc val)
  ) =>
  IMInterpreter trc val (Seq val)
takeIMInterpreterOutput = do
  output' <-
    State.gets imOutput
  traceMsg ("take output " ++ show output')
  State.modify $ \st -> st { imOutput = mempty }
  pure output'

genericStepIMInterpreter ::
  ( Integral val
  , Show val
  , Trace (IMInterpreter trc val)
  ) =>
  [val] -> IMInterpreter trc val (Maybe val)
genericStepIMInterpreter input' = do
  setIMInterpreterInput input'
  done <-
    executeUntilOutputOrHalted
  res <-
    takeIMInterpreterOutput
  case done of
    Halted ->
      pure Nothing
    NotDone ->
      case res of
        [val] ->
          pure (Just val)
        [] ->
          Except.throwError $
          "Internal decoder or interpreter error: should have "
          ++ "produced single output, but none exists."
        vals ->
          Except.throwError $
          "Internal decoder or interpreter error: should have "
          ++ "produced single output, but more exists: "
          ++ show vals


--
-- finally, a very concrete intcode interpreter.
--


runIntcodeInterpreter ::
  IMInterpreter 'NotTracing Int a ->
  State.StateT (IMEnv Int) (Except.Except String) a
runIntcodeInterpreter =
  runIMInterpreter

stepIntcode :: [Int] -> IMInterpreter 'NotTracing Int (Maybe Int)
stepIntcode =
  genericStepIMInterpreter

runIntcodeInterpreterTracing ::
  IMInterpreter 'Tracing Int a ->
  State.StateT (IMEnv Int) (Except.Except String) a
runIntcodeInterpreterTracing =
  runIMInterpreter

stepIntcodeTracing ::
  [Int] -> IMInterpreter 'Tracing Int (Maybe Int)
stepIntcodeTracing =
  genericStepIMInterpreter



--
-- Utilities
--

readIntcodeProg :: FilePath -> IO [Int]
readIntcodeProg name =
  read . ('[':) . (++"]")
  <$> readFile name
