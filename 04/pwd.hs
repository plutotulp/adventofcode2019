#!/usr/bin/env runghc

import Data.Foldable
import Data.Monoid
import System.IO

import qualified Data.List as List

newtype Digits =
  Digits { unDigits :: [Int] }

valid :: Int -> Bool
valid num =
  getAll (mconcat (All <$> checks))
  where
    checks =
      [sixDigits, neverDecrease, twoAdjacent]
      <*> pure digits
    digits =
      Digits (read . pure <$> show num)

sixDigits :: Digits -> Bool
sixDigits =
  (6 ==) . length . unDigits

-- twoAdjacent :: Digits -> Bool
-- twoAdjacent (Digits ds) =
--   not (null (filter (\(d1, d2) -> d1 == d2) (zip ds (tail ds))))

twoAdjacent :: Digits -> Bool
twoAdjacent (Digits ds) =
  0 < length groups && elem 2 (length <$> groups)
  where
  groups =
    List.group ds

neverDecrease :: Digits -> Bool
neverDecrease (Digits ds) =
  null (filter (\(d1, d2) -> d1 > d2) (zip ds (tail ds)))

showPwd :: Int -> String
showPwd n =
  show n ++ " is "
  ++ (if valid n then "" else "not ")
  ++ "valid."

main :: IO ()
main = do
  -- test pwds
  for_
    [111111, 223450, 123789, 112233, 123444, 111122]
    (putStrLn . showPwd)

  -- brute force search
  putStr "No. valid passwords … "
  hFlush stdout
  let
    validPasswords =
      filter valid [256310..732736]
  putStrLn (show (length validPasswords))
