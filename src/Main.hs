module Main where

import System.Environment
import Text.Read

import Day1 qualified
import Util

main :: IO ()
main =
  getArgs >>= \case
    (mapM (readMaybe @Int) -> Just [day, part]) -> do
      input <- readFile $ "inputs/" ++ show day ++ "-" ++ show part
      case toEnum (day-1) of
        Day1 -> Day1.dayMain (toEnum (part-1)) input
    _ -> error "usage: aoc2021 <day> <part>"
