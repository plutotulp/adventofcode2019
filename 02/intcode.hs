#!/usr/bin/env runghc

{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}

import System.IO

import Control.Monad.ST as ST
import Control.Monad.Except as Except
import Control.Monad.Reader as Reader
import Control.Monad.Trans as Trans
import Data.Array.Unboxed as UArray
import Data.Array.IArray as IArray
import Data.Array.MArray as MArray
import Data.Array.ST as STArray

newtype Addr =
  Addr Int
  deriving (Eq, Ix, Ord, Num, Show)

type Mem s =
  STArray.STUArray s Addr Int

type Prog s a =
  Reader.ReaderT (Mem s) (Except.ExceptT String (ST s)) a

get :: Addr -> Prog s Int
get a =
  read' =<< Reader.ask
  where
    read' mem =
      Trans.lift (Trans.lift (MArray.readArray mem a))

put :: Addr -> Int -> Prog s ()
put a v =
  write' =<< Reader.ask
  where
    write' mem =
      Trans.lift (Trans.lift (MArray.writeArray mem a v))

getIndir :: Addr -> Prog s Int
getIndir a =
  get =<< (Addr <$> get a)

putIndir :: Addr -> Int -> Prog s ()
putIndir a v =
  flip put v =<< (Addr <$> get a)

binOp :: (Int -> Int -> Int) -> Addr -> Addr -> Addr -> Prog s ()
binOp op r1 r2 w =
  putIndir w =<< (op <$> getIndir r1 <*> getIndir r2)

add :: Addr -> Addr -> Addr -> Prog s ()
add r1 r2 w =
  binOp (+) r1 r2 w

mul :: Addr -> Addr -> Addr -> Prog s ()
mul r1 r2 w =
  binOp (*) r1 r2 w

runMem :: Mem s -> Prog s a -> ST s (Either String a)
runMem mem prog =
  Except.runExceptT (Reader.runReaderT prog mem)

-- opcode names
pattern OpAdd :: Int
pattern OpAdd = 1

pattern OpMul :: Int
pattern OpMul = 2

pattern OpHalt :: Int
pattern OpHalt = 99

executeFrom ::
  Addr -> Prog s ()
executeFrom ip0 =
  go ip0
  where
    go ip = do
      op <-
        get ip
      case op of
        OpAdd ->
          add (ip+1) (ip+2) (ip+3) *> go (ip+4)
        OpMul ->
          mul (ip+1) (ip+2) (ip+3) *> go (ip+4)
        OpHalt ->
          pure ()
        _ ->
          lift (Except.throwError ("Unknown opcode " ++ show op ++ "@" ++ show ip))

mkMem :: [Int] -> ST s (Mem s)
mkMem ops =
  MArray.newListArray (Addr 0, Addr (length ops - 1)) ops

runProg :: [Int] -> [Int]
runProg ops = do
  IArray.elems $ STArray.runSTUArray $ do
    mem <-
      mkMem ops
    res <- runMem mem (executeFrom 0)
    either fail (const (pure mem)) res

fixup :: Int -> Int -> Prog s ()
fixup noun verb = do
  put 1 noun
  put 2 verb

gravityAssistOps :: UArray.UArray Addr Int
gravityAssistOps =
  IArray.listArray (Addr 0, Addr (length ops - 1)) ops
  where
    ops =
      [2,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,1,19,6,23,2,23,13,27,1,27,5,31,2,31,10,35,1,9,35,39,1,39,9,43,2,9,43,47,1,5,47,51,2,13,51,55,1,55,9,59,2,6,59,63,1,63,5,67,1,10,67,71,1,71,10,75,2,75,13,79,2,79,13,83,1,5,83,87,1,87,6,91,2,91,13,95,1,5,95,99,1,99,2,103,1,103,6,0,99,2,14,0,0]

search :: [Int] -> [Int] -> Int -> Either String (Int, Int)
search nouns verbs target =
  ST.runST (go [(noun, verb) | noun <- nouns, verb <- verbs])
  where
    go [] = pure (Left "not found")
    go (cfg@(noun, verb):restCfgs) = do
      mem <-
        MArray.thaw gravityAssistOps
        :: ST s (STArray.STUArray s Addr Int)
      res <- runMem mem $ do
        fixup noun verb
        executeFrom 0
        get 0
      either (pure . Left) (check cfg restCfgs) res

    check cfg restCfgs v
      | v == target  = pure (Right cfg)
      | otherwise    = go restCfgs

main :: IO ()
main = do
  putStrLn "smallprog1"
  print (runProg [1,0,0,0,99])
  putStrLn "smallprog2"
  print (runProg [2,3,0,3,99])
  putStrLn "smallprog3"
  print (runProg [2,4,4,5,99,0])
  putStrLn "smallprog4"
  print (runProg [1,1,1,4,99,5,6,0,99])

  putStr "fixed gravity assist prog result … "
  hFlush stdout
  putStrLn $ either id show $ do
    ST.runST $ do
      mem <-
        MArray.thaw gravityAssistOps
        :: ST s (STArray.STUArray s Addr Int)
      runMem mem $ do
        fixup 12 2
        executeFrom 0
        get 0

  putStr "noun+verb search … "
  hFlush stdout
  putStrLn
    (either
      id
      (\(n,v) -> "noun: " ++ show n ++ ", verb: " ++ show v)
      (search [0..99] [0..99] 19690720))
