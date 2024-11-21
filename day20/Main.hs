module Main where

import Control.Monad.State

import qualified Data.IntMap.Strict as IM
import           Data.List          (findIndex, group)
import           Data.Maybe         (fromJust)

main :: IO ()
main = do
  putStr "part 1: " >> print part1
  putStr "part 2: " >> print part2

target :: Int
target = 34000000

part2 :: Int
part2 = flip evalState mempty $ fromJust . findIndex (>=div target 11) <$> traverse (\n -> sum . filter (\d -> n <= 50 * d) . divisors <$> primeFactors n) [0..]

divisors :: [Int] -> [Int]
divisors factors = foldr combineDivisors [1] (group factors)
  where
    combineDivisors :: [Int] -> [Int] -> [Int]
    combineDivisors primeGroup acc = do
      let p = head primeGroup
      d <- acc
      e <- [0..length primeGroup]
      return $ d * p ^ e

part1 :: Int
part1 = flip evalState mempty $ fromJust . findIndex (>=div target 10) <$> traverse divisorsSum [0..]

type PrimeFactorsMemo = State (IM.IntMap [Int])

divisorsSum :: Int -> PrimeFactorsMemo Int
divisorsSum n = product . map (sum . scanl (*) 1) . group <$> primeFactors n

primes :: [Int]
primes = 2 : 3 : [n | n <- [5, 7..], isPrime n]

isPrime :: Int -> Bool
isPrime n = all ((/=0) . rem n) $ takeWhile (\p -> p * p <= n) primes

primeFactors :: Int -> PrimeFactorsMemo [Int]
primeFactors 0 = return []
primeFactors n = primeFactors' primes n
  where
    primeFactors' :: [Int] -> Int -> PrimeFactorsMemo [Int]
    primeFactors' (p:_) n | p * p > n = return [n]
    primeFactors' (p:ps) n = do
      if mod n p /= 0
      then primeFactors' ps n
      else gets (IM.lookup n) >>= maybe calcNext return
      where
        calcNext = do
          res <- (p:) <$> primeFactors' (p:ps) (div n p)
          modify (IM.insert n res)
          return res
