-- TODO: rewrite this with haskell laziness nature
import Text.Read (readMaybe)
import Data.Maybe (maybe)
import Data.Word (Word16)
import Data.Bits (shiftL, shiftR, (.&.), (.|.), complement)
import Control.Monad.State (State, gets, modify, evalState)
import qualified Data.Map as M

main :: IO ()
main = do
  circuit <- M.fromList . map parseWire . lines <$> readFile "input.txt"
  let part1 = evalState (eval (Var "a")) circuit  -- part 1
  let part2 = evalState (setVar "b" part1 >> eval (Var "a")) circuit  -- part 2
  putStrLn $ unwords ["part1:", show part1]
  putStrLn $ unwords ["part2:", show part2]

type Wire = (String, Expr)
type Circuit = M.Map String Expr

parseWire :: String -> Wire
parseWire s =
  case span (/="->") (words s) of
    ([lit], ["->", dest]) -> (dest, parseLit lit)
    (["NOT", lit], ["->", dest]) -> (dest, Not (parseLit lit))
    ([lhs, op, rhs], ["->", dest]) -> (dest, parseBin op lhs rhs)

eval :: Expr -> State Circuit Word16
eval expr =
  case expr of
    Var var -> do
      value <- getVar var >>= eval
      setVar var value
      return value
    Num n -> return n
    Or lhs rhs -> (.|.) <$> eval lhs <*> eval rhs
    And lhs rhs -> (.&.) <$> eval lhs <*> eval rhs
    LShift lhs rhs -> shiftL <$> eval lhs <*> fmap fromIntegral (eval rhs)
    RShift lhs rhs -> shiftR <$> eval lhs <*> fmap fromIntegral (eval rhs)
    Not expr -> complement <$> eval expr

getVar :: String -> State Circuit Expr
getVar var = gets (M.! var)

setVar :: String -> Word16 -> State Circuit ()
setVar var value = modify $ M.insert var (Num value)

data Expr
  = Var String
  | Num Word16
  | Or Expr Expr
  | And Expr Expr
  | LShift Expr Expr
  | RShift Expr Expr
  | Not Expr
  deriving Show

parseLit :: String -> Expr
parseLit lit = maybe (Var lit) Num (readMaybe lit)

parseBin :: String -> String -> String -> Expr
parseBin "OR" lhs rhs = Or (parseLit lhs) (parseLit rhs)
parseBin "AND" lhs rhs = And (parseLit lhs) (parseLit rhs)
parseBin "LSHIFT" lhs rhs = LShift (parseLit lhs) (parseLit rhs)
parseBin "RSHIFT" lhs rhs = RShift (parseLit lhs) (parseLit rhs)
