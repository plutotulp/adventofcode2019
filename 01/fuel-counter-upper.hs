#!/usr/bin/env runghc

{-# language GeneralizedNewtypeDeriving #-}

import Data.Coerce (coerce)

newtype Mass = Mass Double
newtype Fuel = Fuel Double deriving Num

fuel :: Mass -> Fuel
fuel (Mass m) =
  Fuel (if f < 0 then 0 else f)
  where
    f =
      fromIntegral (floor (m / 3.0)) - 2

-- Fuel required for module as well as fuel required for that fuel,
-- and so on.
integratedFuel :: Mass -> Fuel
integratedFuel (Mass m) =
  -- Calculates all residual fuels until they drop to zero, then sums
  -- them. The 'drop 1' is for leaving the original mass out of the
  -- sum.
  Fuel (sum (fst (span (0 <) (drop 1 (iterate (coerce fuel) m)))))

getModuleMasses :: IO [Mass]
getModuleMasses =
  fmap (Mass . read) . lines
  <$> readFile "module_masses.txt"

main :: IO ()
main = do
  ms <-
    getModuleMasses
  let
    Fuel f1 =
      sum (fmap fuel ms)
  putStrLn ("Total fuel for modules: " ++ show f1)

  let
    Fuel f2 =
      sum (fmap integratedFuel ms)
  putStrLn ("Total fuel for modules and additional fuel: " ++ show f2)
