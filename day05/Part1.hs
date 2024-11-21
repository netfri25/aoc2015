import Data.Char (toLower)
import Data.List (isInfixOf, tails)

main :: IO ()
main = readFile "./input.txt" >>= print . length . filter isNice . words

isNice :: String -> Bool
isNice s =
  let vowels_count = length $ filter (`elem` "aeiou") s
      has_duplicate = or $ zipWith (==) s (tail s)
      is_banned = any (`isInfixOf` s) ["ab", "cd", "pq", "xy"]
   in vowels_count >= 3 && has_duplicate && not is_banned
