#!/usr/bin/env runghc

import Intcode (runIMInterpreter, runTracingIMInterpreter, IMEnv(..))

diagnose :: [Integer] -> IO ()
diagnose prog = do
  let
    relativeBase =
      0
    startAddr =
      0
    input =
      [1]
    env =
      either error id (runTracingIMInterpreter prog relativeBase startAddr input)

  case imOutput env of
    [keyCode] ->
      putStrLn ("BOOST keycode: " ++ show keyCode)
    failures -> do
      putStrLn "Program trace"
      mapM_ putStrLn (imTrace env)
      putStrLn ("Failing opcodes: " ++ show failures)

findCoordinates :: [Integer] -> IO ()
findCoordinates prog = do
  let
    relativeBase =
      0
    startAddr =
      0
    input =
      [2]
    env =
      either error id (runIMInterpreter prog relativeBase startAddr input)

  case imOutput env of
    [coordinates] ->
      putStrLn ("Distress signal coordinates: " ++ show coordinates)
    res -> do
      putStrLn ("Unexpected coordinate search results: " ++ show res)

main :: IO ()
main = do
  prog <-
    read . ('[':) . (++"]")
    <$> readFile "boostprog.txt"
    :: IO [Integer]

  diagnose prog
  findCoordinates prog
