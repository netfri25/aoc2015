module Main (main) where

import Data.List (sort, group, sortOn)

main :: IO ()
main = do
  items <- sort . map read . lines <$> readFile "input"
  let total = sum items
  putStrLn $ unwords ["total:", show total]
  putStrLn $ unwords ["part 1:", show $ part1 items]
  putStrLn $ unwords ["part 2:", show $ part2 items]

part1 :: [Int] -> Integer
part1 items = product $ map fromIntegral $ head $ head $ groupSums (sum items `div` 3) items

part2 :: [Int] -> Integer
part2 items = product $ map fromIntegral $ head $ head $ groupSums (sum items `div` 4) items

groupSums :: Int -> [Int] -> [[[Int]]]
groupSums _ [] = return [[]]
groupSums target items = do
  xs <- sortOn length $ sumCombs target items
  (xs:) <$> groupSums target (items `diff` xs)

sumCombs :: Int -> [Int] -> [[Int]]
sumCombs 0 _ = return []
sumCombs target [] = mempty
sumCombs target (x:_) | x > target = mempty
sumCombs target (x:xs) = map (x:) (sumCombs (target - x) xs) ++ sumCombs target xs

-- diff = sort xs - sort ys
diff :: Ord a => [a] -> [a] -> [a]
diff xs [] = xs
diff [] _ = []
diff (x:xs) (y:ys) =
  case compare x y of
    GT -> diff (x:xs) ys
    EQ -> diff xs ys
    LT -> x : diff xs (y:ys)
