module Main (main) where

import Data.List (group)

main = do
  putStrLn $ unwords ["part 1:", show $ length $ iterate step input !! 40]
  putStrLn $ unwords ["part 2:", show $ length $ iterate step input !! 50]

input = [3, 1, 1, 3, 3, 2, 2, 1, 1, 3]

step xs = group xs >>= \xs -> [length xs, head xs]
