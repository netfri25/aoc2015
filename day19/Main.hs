module Main where

import Data.List (sort, group, tails, isPrefixOf)
import Data.Char (isUpper)

main :: IO ()
main = do
  input <- parseInput <$> readFile "./input.txt"
  print $ part1 input
  print $ part2 input

part1 :: Input -> Int
part1 (Input comb mappings) = length $ stepPath mappings comb

part2 :: Input -> Int
part2 (Input comb _) =
  let total_elements = length $ filter isUpper comb
      ar_and_rn = length $ filter (\c -> any (`isPrefixOf` c) ["Ar", "Rn"]) $ tails comb
      ys = length $ filter (=='Y') comb
   in total_elements - ar_and_rn - 2 * ys - 1

findPath :: Comb -> [Mapping] -> [Comb] -> Int
findPath target mappings combs
  | target `elem` combs = 0
  | otherwise = findPath target mappings nexts + 1
  where nexts = concatMap (stepPath mappings) combs

stepPath :: [Mapping] -> Comb -> [Comb]
stepPath mappings comb = map head $ group $ sort $ concatMap (applyMapping comb) mappings

type Mapping = (String, String)
type Comb = String

data Input = Input
  { inputComb     :: Comb
  , inputMappings :: [Mapping]
  } deriving Show

applyMapping :: Comb -> Mapping -> [Comb]
applyMapping [] _ = mempty
applyMapping text (from, to) =
  case removePrefix text from of
    Just res -> (to ++ res) : map (head text :) (applyMapping (tail text) (from, to))
    Nothing  -> map (head text :) (applyMapping (tail text) (from, to))

removePrefix :: Eq a => [a] -> [a] -> Maybe [a]
removePrefix src [] = Just src
removePrefix [] _ = Nothing
removePrefix (x:xs) (y:ys)
  | x == y = removePrefix xs ys
  | otherwise = Nothing

parseInput :: String -> Input
parseInput input_text =
  let input_lines = lines input_text
      (mappings_lines, ["", comb]) = break null input_lines
      mappings = map parseMapping mappings_lines
   in Input comb mappings

parseMapping :: String -> Mapping
parseMapping input =
  let [from, _, to] = words input
   in (from, to)
