{-# LANGUAGE DerivingStrategies #-}

import Data.Bits (xor)
import Data.Bool (bool)

main :: IO ()
main = do
  insts <- map parseInst . lines <$> readFile "./input.txt"
  putStrLn $ unwords ["part 1:", show (part1 insts)]
  putStrLn $ unwords ["part 2:", show (part2 insts)]

part1 :: [Inst] -> Int
part1 insts = length [() | x <- [0..999], y <- [0..999], findState (Pos x y) (reverse insts)]

part2 :: [Inst] -> Int
part2 insts = getBrightness $ sum $ map (`findBrightness` insts) $ Pos <$> [0..999] <*> [0..999]

findState :: Pos -> [Inst] -> Bool
findState pos insts =
  case span (==Toggle) actions of
    (toggles, state:_) -> xor (state == On) (odd $ length toggles)
    (toggles, _)       -> odd $ length toggles
  where
    actions = map instAction $ filter (inRange pos . instRange) insts

findBrightness :: Pos -> [Inst] -> Brightness
findBrightness pos = sum . map (convertBright . instAction) . filter (inRange pos . instRange)

newtype Brightness = Brightness { getBrightness :: Int }
  deriving newtype (Show, Eq, Ord)

instance Num Brightness where
  Brightness a + Brightness b = Brightness (max (a + b) 0)
  (-) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  fromInteger = Brightness . fromInteger

convertBright :: Action -> Brightness
convertBright Toggle = Brightness 2
convertBright On = Brightness 1
convertBright Off = Brightness (-1)

data Inst = Inst
  { instAction :: Action
  , instRange  :: Range
  } deriving Show

parseInst :: String -> Inst
parseInst s =
  case words s of
    ["toggle", from, "through", to]      -> Inst Toggle $ Range (parsePos from) (parsePos to)
    ["turn", "on", from, "through", to]  -> Inst On $ Range (parsePos from) (parsePos to)
    ["turn", "off", from, "through", to] -> Inst Off $ Range (parsePos from) (parsePos to)

data Action = On | Off | Toggle
  deriving (Show, Eq)

data Range = Range
  { rangeFrom :: Pos
  , rangeTo   :: Pos
  } deriving Show

-- inclusive
inRange :: Pos -> Range -> Bool
inRange (Pos x y) (Range (Pos x1 y1) (Pos x2 y2)) = lx <= x && x <= hx && ly <= y && y <= hy
  where
    lx = min x1 x2
    ly = min y1 y2
    hx = max x1 x2
    hy = max y1 y2

data Pos = Pos
  { posX :: Int
  , posY :: Int
  } deriving Show

parsePos :: String -> Pos
parsePos s = Pos (read x_str) (read y_str)
  where
    (x_str, ',':y_str) = span (/=',') s
