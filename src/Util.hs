module Util where

import Data.List
import Data.Proxy
import GHC.TypeLits

data Part = Part1 | Part2
  deriving (Enum, Show)

data Day = Day1
  deriving (Enum, Show)

takeNat :: forall n a. KnownNat n => [a] -> [a]
takeNat = genericTake (natVal @n Proxy)

dropNat :: forall n a. KnownNat n => [a] -> [a]
dropNat = genericDrop (natVal @n Proxy)
