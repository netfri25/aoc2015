{-# OPTIONS_GHC -Wall -Wextra #-}
module Main where

import Data.List (tails)

main :: IO ()
main = do
  putStr "part 1: " >> print part1
  putStr "part 2: " >> print part2

boss :: Player
boss = Player 103 9 2

part1 :: Int
part1 = minimum $ map fst $ filter (winner boss . snd) allPlayers

part2 :: Int
part2 = maximum $ map fst $ filter (not . winner boss . snd) allPlayers

data Player = Player
  { playerHitpoints :: Int
  , playerDamage    :: Int
  , playerArmor     :: Int
  } deriving Show

winner :: Player -> Player -> Bool
winner opp player
  | playerHitpoints player <= 0 = False
  | otherwise = not $ winner player (opp { playerHitpoints = playerHitpoints opp - damage })
  where
    damage = max 1 (playerDamage player - playerArmor opp)

data Item = Item
  { itemCost   :: Int
  , itemDamage :: Int
  , itemArmor  :: Int
  } deriving Show

weapons :: [Item]
weapons =
  [ Item   8 4 0
  , Item  10 5 0
  , Item  25 6 0
  , Item  40 7 0
  , Item  74 8 0
  ]

armors :: [Item]
armors =
  [ Item  13 0 1
  , Item  31 0 2
  , Item  53 0 3
  , Item  75 0 4
  , Item 102 0 5
  ]

rings :: [Item]
rings =
  [ Item  25 1 0
  , Item  50 2 0
  , Item 100 3 0
  , Item  20 0 1
  , Item  40 0 2
  , Item  80 0 3
  ]

startHitpoints :: Int
startHitpoints = 100

allPlayers :: [(Int, Player)]
allPlayers = do
  let no_item = Item 0 0 0
  weapon <- weapons
  armor <- no_item : armors
  ring1 : rings' <- init $ tails $ no_item : rings
  ring2 <- no_item : rings'
  let selected_items = [weapon, armor, ring1, ring2]
  let total_cost = sum $ map itemCost selected_items
  let total_damage = sum $ map itemDamage selected_items
  let total_armor = sum $ map itemArmor selected_items
  return (total_cost, Player startHitpoints total_damage total_armor)
