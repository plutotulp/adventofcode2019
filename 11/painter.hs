#!/usr/bin/env runghc

{-# language BangPatterns #-}
{-# language LambdaCase #-}

import Data.Array.Unboxed
import Data.Set (Set)
import qualified Data.Set as Set

import Intcode (readIntcodeProg)
import Robot (V2(V2), RobotState(painted, whitePanels), initRobotState, runRobotProg)

showUArray :: UArray V2 Char -> String
showUArray arr =
  unlines (map showRow [y0..yn])
  where
    showRow y =
      -- reversing x direction because otherwise the resulting text is
      -- mirrored.
      [ arr ! (V2 x y) | x <- [xn,xn-1..x0] ]
    (V2 x0 y0, V2 xn yn) =
      bounds arr

renderPanels :: Set V2 -> UArray V2 Char
renderPanels wps =
  accumArray step ' ' bnds (mkAssoc <$> Set.toList wps)
  where
    step _ v =
      v
    mkAssoc loc =
      (loc, '#') -- white paint
    bnds =
      (V2 xmin ymin, V2 xmax ymax)
    xmin =
      minimum ((\(V2 x _) -> x) <$> Set.toList wps)
    xmax =
      maximum ((\(V2 x _) -> x) <$> Set.toList wps)
    ymin =
      minimum ((\(V2 _ y) -> y) <$> Set.toList wps)
    ymax =
      maximum ((\(V2 _ y) -> y) <$> Set.toList wps)


main :: IO ()
main = do
  intProg <-
    readIntcodeProg "prog.txt"

  putStrLn "First run ... "
  let
    !noPanels =
      Set.size (painted (runRobotProg (initRobotState intProg mempty)))
  putStrLn ("Painted " ++ show noPanels ++ " panels")

  putStrLn ""
  putStrLn "Second run ..."
  let
    !finalState =
      runRobotProg (initRobotState intProg (Set.singleton (V2 0 0)))
  putStrLn (showUArray (renderPanels (whitePanels finalState)))
