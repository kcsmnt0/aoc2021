module Day1 where

import Control.Arrow
import Data.Foldable
import Data.List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import GHC.TypeLits

import Util

dayMain :: Part -> String -> IO ()
dayMain Part1 = partMain1
dayMain Part2 = partMain2


-- part 1:
--   count how many times a list element is larger than the previous one

consecutivePairs :: [a] -> [(a,a)]
consecutivePairs xs = zip xs $ tail xs

increasingElems :: Ord a => [a] -> Int
increasingElems = sum . map (fromEnum . uncurry (<)) . consecutivePairs

partMain1 :: String -> IO ()
partMain1 = print . increasingElems . map (read @Int) . lines


-- part 2:
--   for three-element sliding windows of the input list, count when the sum of
--   a window is larger than the previous one

data AtLeast n a = AtLeast 
  { window :: Vector n a
  , wall :: [a]
  }

atLeast :: forall n a. KnownNat n => [a] -> Maybe (AtLeast n a)
atLeast xs = do
  heads <- Vector.fromList @n $ takeNat @n xs
  pure $ AtLeast heads $ dropNat @n xs

slide :: forall n a. (1 <= n) => AtLeast n a -> Maybe (AtLeast n a)
slide (AtLeast xs ys) = do
  let xs' = Vector.tail xs
  (y, ys') <- uncons ys
  pure $ AtLeast (Vector.snoc @(n-1) xs' y) ys'

windows :: forall n a. (1 <= n) => AtLeast n a -> NonEmpty (Vector n a)
windows = NonEmpty.unfoldr (window &&& slide)

listWindows :: forall n a. (KnownNat n, 1 <= n) => [a] -> [Vector n a]
listWindows = maybe [] (toList . windows) . atLeast

increasingWindows :: forall n a. (Num a, Ord a, KnownNat n, 1 <= n) => [a] -> Int
increasingWindows = increasingElems . map sum . listWindows @n

partMain2 :: String -> IO ()
partMain2 = print . increasingWindows @3 . map (read @Int) . lines
